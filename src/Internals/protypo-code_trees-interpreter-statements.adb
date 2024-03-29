pragma Ada_2012;

with Protypo.Code_Trees.Interpreter.Compiled_Functions;
with Protypo.Code_Trees.Interpreter.Names;
with Protypo.Code_Trees.Interpreter.Expressions;

with Protypo.Api.Engine_Values.Handlers;

with Ada.Exceptions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;

package body Protypo.Code_Trees.Interpreter.Statements is
   use type Names.Name_Reference;

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
          raise Run_Time_Error
            with "Trying to convert non-numeric value ("
       & X.Class'Image & ") to Boolean");

   package Lhs_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Names.Name_Reference);

   procedure Do_Procedure_Call (Status : Interpreter_Access;
                                Name   : Unbounded_Id;
                                Params : Node_Vectors.Vector)
   is
      use Api.Symbols.Protypo_Tables;
      use type Ada.Containers.Count_Type;

      Position : constant Cursor :=
                   Status.Symbol_Table.Find (Id (To_String (Name)));


   begin
      if Position = No_Element then
         raise Constraint_Error with
           "Unknown function '" & To_String (Name) & "'";
      end if;

      declare
         Proc_Handler : constant Engine_Value :=  Value (Position);
      begin
         if Proc_Handler.Class /= Function_Handler then
            raise Constraint_Error;
         end if;

         declare
            Funct : constant Protypo.Api.Engine_Values.Handlers.Function_Interface_Access :=
                      Handlers.Get_Function (Proc_Handler);

            Parameters : constant Engine_Value_Vectors.Vector :=
                           Expressions.Eval_Vector (Status, Params);

            Call_Ref : constant Names.Name_Reference :=
                         (Class            => Names.Function_Call,
                          Function_Handler => Funct,
                          Parameters       => Parameters);

            Result   : constant Engine_Value_Vectors.Vector :=
                         Expressions.Call_Function (Call_Ref);
         begin
            if Result.Length /= 0 then
               raise Run_Time_Error with "Procedure call returns a value";
            end if;
         end;
      end;
   end Do_Procedure_Call;

   ---------
   -- Run --
   ---------

   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
   is
      use Compiled_Functions;


      subtype Lhs_Array is Lhs_Vectors.Vector;

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
         raise Program_Error
           with "Trying to execute a non-statment node ("
           & Program.Class'Image & ")";
      end if;


      case Statement_Classes (Program.Class) is
         when Statement_Sequence =>
            Run (Status, Program.Statements);

         when Defun       =>
            Status.Symbol_Table.Create
              (Name          =>
                  Id (To_String (Program.Definition_Name)),
               Initial_Value =>
                  Handlers.Create (new Compiled_Function'(Function_Body => Program.Function_Body,
                                                          Parameters    => Program.Parameters,
                                                          Status        => Status)));

         when Assignment  =>
            declare
               use type Ada.Containers.Count_Type;
               use type Names.Value_Name_Class;

               Values : Engine_Value_Vectors.Vector;
               Lhs : Lhs_Array;
            begin
               Values  := Expressions.Eval_Vector (Status, Program.Rvalues);

               if Program.Lhs.Length /= Values.Length then
                  raise Run_Time_Error
                    with "Assignment with "
                    & Program.Lhs.Length'Image & " LHS terms and "
                    & Values.Length'Image & " RHS terms";
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
                  --                    Put_Line ("@@@");
                  Lhs.Append (Names.Eval_Name (Status, Name));
                  --                    Put_Line ("@@@ 1");

                  if Lhs.Last_Element.Class /= Names.Variable_Reference then
                     --
                     -- Only reference handlers (that allow for both reading
                     -- and writing) can be on the LHS
                     --
                     raise Run_Time_Error
                       with
                         "Found " & Lhs.Last_Element.Class'Image & " on LHS";
                  end if;
               end loop;
               --                 Put_Line ("@@@ xx");

               declare
                  Shift : constant Integer := Values.First_Index - Lhs.First_Index;
               begin

                  for K in Lhs.First_Index .. Lhs.Last_Index loop
                     Lhs (K).Variable_Handler.Write (Values (K + Shift));
                  end loop;

                  --                    Put_Line ("@@@ uu");

               end;
            end;
         when Return_Statement =>
            Status.Break :=
              Break_Status'(Breaking_Reason => Return_Statement,
                            Result          => Expressions.Eval_Vector (Status, Program.Return_Values));
            return;

         when Procedure_Call =>
            Do_Procedure_Call (Status, Program.Name, Program.Params);


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

               Iterator_Ref : constant handlers.Iterator_Interface_Access :=
                                Expressions.Eval_Iterator (Status, Program.Iterator);

               Variable : constant Id := Id (To_String (Program.Variable));

               Position : Protypo_Tables.Cursor;
            begin
               Iterator_Ref.Reset;

               Status.Symbol_Table.Open_Internal_Namespace;

               Status.Symbol_Table.Create
                 (Name          => Variable,
                  Initial_Value => Void_Value,
                  Position      => Position);

               loop
                  exit when Iterator_Ref.End_Of_Iteration;

                  Protypo_Tables.Update (Pos       => Position,
                                         New_Value => Iterator_Ref.Element);

                  exit when Run_Loop_Body (Status, Program) = Stop;

                  Iterator_Ref.Next;
               end loop;

               Status.Symbol_Table.Close_Namespace;
            end;

         when While_Block =>
            loop
               exit when not Is_True (Expressions.Eval_Scalar (Status, Program.Condition));
               exit when Run_Loop_Body (Status, Program) = Stop;
            end loop;


      end case;

   exception
      when E : Bad_Iterator | Bad_Field =>
         raise Run_Time_Error with Ada.Exceptions.Exception_Message (E)
           & " at "
           & Tokens.Image (Program.Source_Position, False);

      when E : Run_Time_Error =>
         raise Run_Time_Error with Ada.Exceptions.Exception_Message (E)
           & ", "
           & Tokens.Image (Program.Source_Position, False);

   end Run;

end Protypo.Code_Trees.Interpreter.Statements;
