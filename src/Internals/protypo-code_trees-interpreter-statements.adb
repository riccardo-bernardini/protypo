pragma Ada_2012;

with Protypo.Code_Trees.Interpreter.Compiled_Functions;
with Protypo.Code_Trees.Interpreter.Names;
with Protypo.Code_Trees.Interpreter.Expressions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter.Statements is

   ---------
   -- Run --
   ---------

   procedure Run (Status  : Interpreter_Access;
                  Program : Node_Vectors.Vector)
   is

   begin
      for Statement of Program loop
         Run (Status, Statement);

         if Status.Break.Breaking_Reason /= None then
            return;
         end if;
      end loop;
   end Run;

   function Is_True (X : Engine_Value) return Boolean
   is (if X.Class in Numeric_Classes then
         (case Numeric_Classes (X.Class) is
             when Int  => Get_Integer (X) /= 0,
             when Real => Get_Float (X) /= 0.0)
       else
          raise Constraint_Error);

   ---------
   -- Run --
   ---------

   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
   is
      use Compiled_Functions;
      use type Names.Name_Reference;

      package LHS_Vectors is
        new Ada.Containers.Vectors (Index_Type   => Positive,
                                    Element_Type => Names.Name_Reference);

      subtype Lhs_Array is LHS_Vectors.Vector;

      type Continue_Or_Stop is (Continue, Stop);

      function Run_Loop_Body  (Status  : Interpreter_Access;
                               Program : not null Node_Access)
                               return Continue_Or_Stop
        with Pre => Program.Class in Loop_Block .. While_Block;

      function Run_Loop_Body  (Status  : Interpreter_Access;
                               Program : not null Node_Access)
                               return Continue_Or_Stop
      is
      begin
         Run (Status, Program.Loop_Body);

         case Status.Break.Breaking_Reason is
            when None =>
               return Continue;

            when Return_Statement =>
               return Stop;

            when Exit_Statement =>
               if Status.Break.Loop_Label = Program.Labl
                 or Status.Break.Loop_Label = Null_Unbounded_String
               then
                  Status.Break := No_Break;
               end if;

               return Stop;
         end case;
      end Run_Loop_Body;
   begin
      if Program.Class not in Statement_Classes then
         raise Program_Error;
      end if;


      case Statement_Classes (Program.Class) is
         when Statement_Sequence =>
            Run (Status, Program.Statements);

         when Defun       =>
            Status.Symbol_Table.Create
              (Name          =>
                  To_String (Program.Definition_Name),
               Initial_Value =>
                  Create (new Compiled_Function'(Function_Body => Program.Function_Body,
                                                 Parameters    => Program.Parameters,
                                                 Status        => Status)));

         when Assignment  =>
            declare
               use type Ada.Containers.Count_Type;
               use type Names.Value_Name_Class;

               --                 LHS : array (Program.Lhs.First_Index .. Program.Lhs.Last_Index) of Names.Name_Reference;

               Values : Engine_Value_Vectors.Vector;
               LHS : Lhs_Array;
            begin
               Values  := Expressions.Eval_Vector (Status, Program.Rvalues);

               if Program.Lhs.Length /= Values.Length then
                  raise Constraint_Error;
               end if;

--                 Put_Line ("[" & Program.Lhs.First_Index'Image & Program.Lhs.Last_Index'Image);


               for Name of Program.Lhs loop
                  --
                  -- We first evaluate all the names and only after we do all
                  -- the assignment because we could have something like
                  --
                  --  n := 3;
                  --  n, x(n) := 4, 7;
                  --
                  -- In this case, with the separation of name evaluation and
                  -- assignment, 7 is assigned to x(3), while if we did not
                  -- do this, 7 would be assigned to x(4).
                  --
                  -- To be honest, there is not anything deeply wrong with
                  -- second option (that assigns to x(4)), as long as
                  -- we know it.  However, I think that the first one is
                  -- more intuitive: first all the left hand names are
                  -- evaluated, then the RHS expressions are computed
                  -- (with the variable values still unchanged) and finally
                  -- the assigment is done-
                  --
                  LHS.Append (Names.Eval_Name (Status, Name));

                  if LHS.Last_Element.Class /= Names.Variable_Reference then
                     --
                     -- Only reference handlers (that allow for both reading
                     -- and writing) can be on the LHS
                     --
                     raise Constraint_Error;
                  end if;
               end loop;

               declare
                  Shift : constant Integer := Values.First_Index - LHS.First_Index;
               begin

                  for K in LHS.First_Index .. Lhs.Last_Index loop
                     LHS (K).Variable_Handler.Write (Values (K + Shift));
                  end loop;
               end;
            end;
         when Return_Statement =>
            Status.Break :=
              Break_Status'(Breaking_Reason => Return_Statement,
                            Result          => Expressions.Eval_Vector (Status, Program.Return_Values));
            return;

         when Procedure_Call =>
            declare
               use Api.Symbols.Protypo_Tables;

               Position : constant Cursor :=
                            Status.Symbol_Table.Find (To_String (Program.Name));

               Proc_Handler : Engine_Value;

            begin
--                 Put_Line ("@pc " & To_String (Program.Name));
               if Position = No_Element then
--                    Put_Line ("@pc 1");
                  raise Constraint_Error with
                    "Unknown function '" & To_String (Program.Name) & "'";
               end if;

               Proc_Handler :=  Value (Position);
               if Proc_Handler.Class /= Function_Handler then
                  raise Constraint_Error;
               end if;

               declare
                  Funct : constant Function_Interface_Access :=
                            Get_Function (Proc_Handler);

                  Params : constant Engine_Value_Vectors.Vector :=
                             Expressions.Eval_Vector (Status, Program.Params);

                  Call_Ref : constant Names.Name_Reference :=
                               (Class            => Names.Function_Call,
                                Function_Handler => Funct,
                                Parameters       => Params);

                  Result   : constant Engine_Value_Array :=
                               Expressions.Call_Function (Call_Ref);
               begin
                  if Result'Length /= 0 then
                     raise Run_Time_Error with "Procedure call returns a value";
                  end if;
               end;

            end;

         when Exit_Statement =>
            Status.Break :=
              Break_Status'(Breaking_Reason => Exit_Statement,
                            Loop_Label      => Program.Loop_Label);
            return;

         when If_Block    =>
            for Branch of Program.Branches loop
               if Is_True (Expressions.Eval_Scalar (Status, Branch.Condition)) then
                  Run (Status, Branch.Code);
                  return;
               end if;
            end loop;

            if Program.Else_Branch /= null then
               Run (Status, Program.Else_Branch);
            end if;

         when Loop_Block  =>
            loop
               exit when Run_Loop_Body (Status, Program) = Stop;
            end loop;

         when For_Block   =>
            declare
               use Api.Symbols;

               Iterator_Ref : constant Iterator_Interface_Access :=
                                Expressions.Eval_Iterator (Status, Program.Iterator);

               Variable : constant String := To_String (Program.Variable);

               Position : Protypo_Tables.Cursor;
            begin
               Iterator_Ref.Reset;

               Status.Symbol_Table.Open_Internal_Block;

               Status.Symbol_Table.Create
                 (Name          => Variable,
                  Initial_Value => Void_Value);

               loop
                  exit when Iterator_Ref.End_Of_Iteration;

                  Protypo_Tables.Update (Pos       => Position,
                                         New_Value => Iterator_Ref.Element);

                  exit when Run_Loop_Body (Status, Program) = Stop;

                  Iterator_Ref.Next;
               end loop;

               Status.Symbol_Table.Close_Block;
            end;

         when While_Block =>
            loop
               exit when not Is_True (Expressions.Eval_Scalar (Status, Program.Condition));
               exit when Run_Loop_Body (Status, Program) = Stop;
            end loop;


      end case;

   end Run;

end Protypo.Code_Trees.Interpreter.Statements;
