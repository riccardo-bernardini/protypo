pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;
with Protypo.Code_Trees.Interpreter.Compiled_Functions;
with Protypo.Code_Trees.Interpreter.Symbol_Table_References;

package body Protypo.Code_Trees.Interpreter is
   use type Ada.Containers.Count_Type;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (X : Engine_Value_Array) return Engine_Value_Vectors.Vector
   is
      Result : Engine_Value_Vectors.Vector;
   begin
      for Element of X loop
         Result.Append (Element);
      end loop;

      return Result;
   end To_Vector;

   --------------
   -- To_Array --
   --------------

   function To_Array (X : Engine_Value_Vectors.Vector) return Engine_Value_Array
   is
      Result : Engine_Value_Array (X.First_Index .. X.Last_Index);
   begin
      for K in Result'Range loop
         Result (K) := X (K);
      end loop;

      return Result;
   end To_Array;

   --     function "+" (X : Engine_Value_Vectors.Vector)
   --                   return Engine_Value
   --           with
   --                 Pre => X.Length = 1;

   --     function "+" (X : Engine_Value_Vectors.Vector)  return Engine_Value
   --     is (X.First_Element);

   --     function "+" (X : Engine_Value)  return Engine_Value_Vectors.Vector
   --     is
   --        Result : Engine_Value_Vectors.Vector;
   --     begin
   --        Result.Append (X);
   --        return Result;
   --     end "+";

   --     function "+" (X : Engine_Value)  return Engine_Value_Array
   --     is (To_Array (+X));
   --
   --     function "+" (X : Engine_Value_Array)  return Engine_Value
   --     is (if X'Length = 1 then
   --            X (X'First)
   --         else
   --            raise Constraint_Error);


   function Call_Function (Reference : Function_Call_Reference)
                           return Engine_Value_Array
   is
      function Apply_Default (Specs      : Engine_Value_Array;
                              Parameters : Engine_Value_Vectors.Vector)
                              return Engine_Value_Array
      is
         Result : Engine_Value_Array (Specs'Range);
      begin
         if Natural (Parameters.Length) > Specs'Length then
            raise Constraint_Error;
         end if;

         declare
            Shift : constant Natural := Parameters.First_Index - Result'First;
         begin
            for Idx in Result'Range loop
               if Idx + Shift <= Parameters.Last_Index then
                  Result (Idx) := Parameters (Idx + Shift);

               elsif Specs (Idx) /= Void_Value then
                  Result (Idx) := Specs (Idx);

               else
                  raise Constraint_Error;
               end if;
            end loop;
         end;

         return Result;
      end Apply_Default;

      Funct      : constant Function_Interface_Access := Reference.Function_Handler;
      Parameters : constant Engine_Value_Array :=
                     Apply_Default (Funct.Default_Parameters, Reference.Parameters);
   begin
      return Funct.Process (Parameters);
   end Call_Function;


   --------------
   -- To_Value --
   --------------

   function To_Value (Ref : Name_Reference) return Engine_Value_Array
   is
   begin
      if not (Ref.Class in Evaluable_Classes) then
         raise Program_Error;
      end if;

      case Evaluable_Classes (Ref.Class) is
         when Constant_Reference =>
            return Engine_Value_Array'(1 => Ref.Costant_Handler.Read);

         when Variable_Reference =>
            return Engine_Value_Array'(1 => Ref.Variable_Handler.Read);

         when Function_Call =>
            return Call_Function (Ref);
      end case;
   end To_Value;


   ---------------
   -- Eval_Name --
   ---------------

   function Eval_Name (Status : Interpreter_Access;
                       Expr   : not null Node_Access)
                       return Name_Reference
   is

      ---------
      -- "+" --
      ---------

      function "+" (X : Engine_Value) return Name_Reference
            with
                  Pre => X.Class in Handler_Classes;

      function "+" (X : Engine_Value) return Name_Reference
      is
      begin
         if not (X.Class in Handler_Classes) then
            raise Program_Error;
         end if;

         case Handler_Classes (X.Class) is
            when Array_Handler =>
               return Name_Reference'(Class            => Array_Reference,
                                      Array_Handler    => Get_Array (X));

            when Record_Handler =>
               return Name_Reference'(Class             => Record_Reference,
                                      Record_Handler    => Get_Record (X));

            when Function_Handler =>
               return Name_Reference'(Class            => Function_Call,
                                      Function_Handler => Get_Function (X),
                                      Parameters       => <>);

            when Reference_Handler =>
               return Name_Reference'(Class            => Variable_Reference,
                                      Variable_Handler => Get_Reference (X));

            when Constant_Handler =>
               return Name_Reference'(Class             => Constant_Reference,
                                      Costant_Handler   => Get_Constant (X));
         end case;
      end "+";


   begin
      if not (Expr.Class in Name) then
         raise Program_Error;
      end if;

      case Name (Expr.Class) is
         when Selected    =>
            declare
               Head : constant Name_Reference := Eval_Name (Status, Expr.Record_Var);
            begin
               if Head.Class /= Record_Reference then
                  raise Constraint_Error;
               end if;

               return + Head.Record_Handler.Get (To_String (Expr.Field_Name));
            end;
         when Indexed     =>
            declare
               subtype Indexed_References is Value_Name_Class
                     with Static_Predicate => Indexed_References in Array_Reference | Function_Reference;

               Head    : constant Name_Reference := Eval_Name (Status, Expr.Indexed_Var);
               Indexes : constant Engine_Value_Vectors.Vector := Eval_Vector (Status, Expr.Indexes);
            begin
               if not (Head.Class in Indexed_References) then
                  raise Program_Error;
               end if;

               case Indexed_References (Head.Class) is
                  when Array_Reference =>

                     return + Head.Array_Handler.Get (To_Array (Indexes));

                  when Function_Reference =>

                     return Name_Reference'(Class            => Function_Call,
                                            Function_Handler => Head.Function_Handler,
                                            Parameters       => Indexes);
               end case;
            end;

         when Identifier  =>
            declare
               use Api.Symbols.Protypo_Tables;
               use Protypo.Code_Trees.Interpreter.Symbol_Table_References;

               ID       : constant String := To_String (Expr.ID_Value);
               Position : constant Cursor := Status.Symbol_Table.Find (ID);
               Val      : Engine_Value;
            begin
               if Position = No_Element then
                  raise Constraint_Error;
               end if;

               Val := Value (Position);

               if Val.Class in Handler_Classes then
                  return + Val;

               else
                  return Name_Reference'
                        (Class            => Variable_Reference,
                         Variable_Handler => Symbol_Table_Reference (Position));

               end if;
            end;
      end case;
   end Eval_Name;

   ----------
   -- Eval --
   ----------

   function Eval_Vector (Status : Interpreter_Access;
                         Expr   : Node_Vectors.Vector)
                         return Engine_Value_Vectors.Vector
   is
      Result : Engine_Value_Vectors.Vector;
   begin
      for Ex of Expr loop
         Result.Append (Eval_Expression (Status, Ex));
      end loop;

      return Result;
   end Eval_Vector;

   -----------------
   -- Eval_Scalar --
   -----------------

   function Eval_Scalar (Status : Interpreter_Access;
                         Expr   : not null Node_Access)
                         return Engine_Value
   is
      Tmp : constant Engine_Value_Vectors.Vector := Eval_Expression (Status, Expr);
      Result : Engine_Value;
   begin
      if Tmp.Length /= 1 then
         raise Constraint_Error;
      end if;

      Result := Tmp.First_Element;

      if not (Result.Class in Scalar_Classes) then
         raise Constraint_Error;
      end if;

      return Result;
   end Eval_Scalar;

   -----------------
   -- Eval_Scalar --
   -----------------

   -------------------
   -- Eval_Iterator --
   -------------------

   function Eval_Iterator (Status : Interpreter_Access;
                           Expr   : not null Node_Access)
                           return Iterator_Interface_Access
   is
      Tmp : constant Engine_Value_Vectors.Vector := Eval_Expression (Status, Expr);
      Result : Engine_Value;
   begin
      if Tmp.Length /= 1 then
         raise Constraint_Error;
      end if;

      Result := Tmp.First_Element;

      if Result.Class /= Iterator then
         raise Constraint_Error;
      end if;

      return Get_Iterator (Result);
   end Eval_Iterator;


   function Eval_Expression (Status : Interpreter_Access;
                             Expr   : not null Node_Access)
                             return Engine_Value_Vectors.Vector
   is

      function Embed (X : Engine_Value) return Engine_Value_Vectors.Vector
      is
         Result : Engine_Value_Vectors.Vector;
      begin
         Result.Append (X);
         return Result;
      end Embed;

      -----------
      -- Apply --
      -----------

      function Apply (Op : Tokens.Unary_Operator;
                      X  : Engine_Value)
                      return Engine_Value_Vectors.Vector
      is
         use Tokens;
      begin
         case Op is
            when Plus        =>
               return Embed (X);

            when Minus       =>
               return Embed (-X);

            when Kw_Not      =>
               return Embed (not X);
         end case;
      end Apply;

      -----------
      -- Apply --
      -----------

      function Apply (Op    : Tokens.Binary_Operator;
                      Left  : Engine_Value;
                      Right : Engine_Value)
                      return Engine_Value_Vectors.Vector
      is
         use Tokens;

      begin
         case Op is
            when Plus        =>
               return Embed (Left + Right);

            when Minus       =>
               return Embed (Left - Right);

            when Mult        =>
               return Embed (Left * Right);

            when Div         =>
               return Embed (Left / Right);

            when Equal       =>
               return Embed (Left = Right);

            when Different   =>
               return Embed (Left /= Right);

            when Less_Than   =>
               return Embed (Left < Right);

            when Greater_Than =>
               return Embed (Left > Right);

            when Less_Or_Equal =>
               return Embed (Left <= Right);

            when Greater_Or_Equal =>
               return Embed (Left >= Right);

            when Kw_And      =>
               return Embed (Left and Right);

            when Kw_Or       =>
               return Embed (Left or Right);

            when Kw_Xor      =>
               return Embed (Left xor Right);
         end case;
      end Apply;

      Left, Right : Engine_Value;
   begin
      if not (Expr.Class in Code_Trees.Expression) then
         raise Program_Error;
      end if;

      case Code_Trees.Expression (Expr.Class) is
         when Binary_Op   =>
            Left := Eval_Scalar (Status, Expr.Left);
            Right := Eval_Scalar (Status, Expr.Right);

            return  Apply (Expr.Operator, Left, Right);

         when Unary_Op    =>
            Right := Eval_Scalar (Status, Expr.Operand);

            return  Apply (Expr.Uni_Op, Right);

         when Int_Constant =>
            return Embed (Create (Expr.N));

         when Real_Constant =>
            return Embed (Create (Expr.X));

         when Text_Constant =>
            return Embed (Create (To_String (Expr.S)));

         when Selected | Indexed | Identifier  =>
            declare
               Ref : constant Name_Reference := Eval_Name (Status, Expr);
            begin
               if not (Ref.Class in Evaluable_Classes) then
                  raise Constraint_Error;
               end if;

               return To_Vector (To_Value (Ref));
            end;

      end case;
   end Eval_Expression;


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

   ---------
   -- Run --
   ---------

   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
   is
      use Compiled_Functions;

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
               Names : array (Program.Lhs.First_Index .. Program.Lhs.Last_Index) of Name_Reference;

               Values : constant Engine_Value_Vectors.Vector := Eval_Vector (Status, Program.Rvalues);
            begin
               if Program.Lhs.Length /= Values.Length then
                  raise Constraint_Error;
               end if;

               for K in Names'Range loop
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
                  Names (K) := Eval_Name (Status, Program.Lhs (K));

                  if Names (K).Class /= Variable_Reference then
                     --
                     -- Only reference handlers (that allow for both reading
                     -- and writing) can be on the LHS
                     --
                     raise Constraint_Error;
                  end if;
               end loop;

               declare
                  Shift : constant Integer := Values.First_Index - Names'First;
               begin
                  for K in Names'Range loop
                     Names (K).Variable_Handler.Write (Values (K + Shift));
                  end loop;
               end;
            end;
         when Return_Statement =>
            Status.Break :=
                  Break_Status'(Breaking_Reason => Return_Statement,
                                Result          => Eval_Vector (Status, Program.Return_Values));
            return;

         when Procedure_Call =>
            declare
               use Api.Symbols.Protypo_Tables;

               Position : constant Cursor :=
                            Status.Symbol_Table.Find (To_String (Program.Name));

               Proc_Handler : Engine_Value;
               Result : Engine_Value;
            begin
               if Position = No_Element then
                  raise Constraint_Error;
               end if;

               Proc_Handler :=  Value (Position);
               if Proc_Handler.Class /= Function_Handler then
                  raise Constraint_Error;
               end if;

               declare
                  Call_Ref : constant Name_Reference :=
                               (Name_Reference'(Class            => Function_Call,
                                                Function_Handler => Get_Function (Proc_Handler),
                                                Parameters       => Program.Parameters));

                  Result   : Constant Engine_Value_Array := Call_Function (Call_Ref);
               begin
                  if Result'Length /= 0 then
                     raise Constraint_Error;
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
               if Is_True (Eval_Scalar (Status, Branch.Condition)) then
                  Run (Status, Branch.Code);
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
                                Eval_Iterator (Status, Program.Iterator);

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
               exit when not Is_True (Eval_Scalar (Status, Program.Condition));
               exit when Run_Loop_Body (Status, Program) = Stop;
            end loop;


      end case;

   end Run;

   ---------
   -- Run --
   ---------

   procedure Run
         (Program      : Parsed_Code;
          Symbol_Table : Api.Symbols.Table;
          Consumer     : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols;

      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table)
      is
      begin
         Table.Create (Name          => "consume",
                       Initial_Value => Create (Consumer_Handlers.Create (Consumer)));
      end Add_Builtin_Values;

      Interpreter : constant Interpreter_Access :=
                      new Interpreter_Type'(Break        => No_Break,
                                            Symbol_Table => Copy_Globals (Symbol_Table));
   begin
      Add_Builtin_Values (Interpreter.Symbol_Table);

      Run (Interpreter, Program.Pt);

      if Interpreter.Break /= No_Break  then
         raise Program_Error;
      end if;
   end Run;

end Protypo.Code_Trees.Interpreter;
