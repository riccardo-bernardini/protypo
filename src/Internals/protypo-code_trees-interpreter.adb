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

   function "+" (X : Engine_Value_Vectors.Vector)
                 return Engine_Value
     with
       Pre => X.Length = 1;

   function "+" (X : Engine_Value_Vectors.Vector)  return Engine_Value
   is (X.First_Element);

   function "+" (X : Engine_Value)  return Engine_Value_Vectors.Vector
   is
      Result : Engine_Value_Vectors.Vector;
   begin
      Result.Append (X);
      return Result;
   end "+";

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

   function Eval_Name (Expr   : not null Node_Access;
                       Status : Interpreter_Access)
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
               Head : constant Name_Reference := Eval_Name (Expr.Record_Var, Status);
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

               Head    : constant Name_Reference := Eval_Name (Expr.Indexed_Var, Status);
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

               ID : constant String := To_String (Expr.ID_Value);
               Position : constant Cursor := Status.Symbol_Table.Find (ID);
               Val : Engine_Value;
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
         Result.Append (Eval (Status, Ex));
      end loop;

      return Result;
   end Eval_Vector;

   function Eval (Status : Interpreter_Access;
                  Expr   : not null Node_Access)
                  return Engine_Value_Vectors.Vector
   is

      -----------
      -- Apply --
      -----------

      function Apply (Op : Tokens.Unary_Operator;
                      X  : Engine_Value)
                      return Engine_Value
      is
         use Tokens;
      begin
         case Op is
            when Plus        =>
               return X;

            when Minus       =>
               return -X;

            when Kw_Not      =>
               return not X;
         end case;
      end Apply;

      function Apply (Op    : Tokens.Binary_Operator;
                      Left  : Engine_Value;
                      Right : Engine_Value)
                      return Engine_Value
      is
         use Tokens;
      begin
         case Op is
            when Plus        =>
               return Left + Right;

            when Minus       =>
               return Left - Right;

            when Mult        =>
               return Left * Right;

            when Div         =>
               return Left / Right;

            when Equal       =>
               return Left = Right;

            when Different   =>
               return Left /= Right;

            when Less_Than   =>
               return Left < Right;

            when Greater_Than =>
               return Left > Right;

            when Less_Or_Equal =>
               return Left <= Right;

            when Greater_Or_Equal =>
               return Left >= Right;

            when Kw_And      =>
               return Left and Right;

            when Kw_Or       =>
               return Left or Right;

            when Kw_Xor      =>
               return Left xor Right;
         end case;
      end Apply;

      Left, Right : Engine_Value;
   begin
      if not (Expr.Class in Code_Trees.Expression) then
         raise Program_Error;
      end if;

      case Code_Trees.Expression (Expr.Class) is
         when Binary_Op   =>
            Left := + Eval (Status, Expr.Left);
            Right := + Eval (Status, Expr.Right);

            return + Apply (Expr.Operator, Left, Right);

         when Unary_Op    =>
            Right := + Eval (Status, Expr.Operand);

            return + Apply (Expr.Uni_Op, Right);

         when Int_Constant =>
            return + Create (Expr.N);

         when Real_Constant =>
            return + Create (Expr.X);

         when Text_Constant =>
            return + Create (To_String (Expr.S));

         when Selected    =>
            raise Program_Error;

         when Indexed     =>
            raise Program_Error;

         when Identifier  =>
            raise Program_Error;

      end case;
   end Eval;



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


   ---------
   -- Run --
   ---------

   procedure Run (Status  : Interpreter_Access;
                  Program : not null Node_Access)
   is
      use Compiled_Functions;
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
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when Return_Statement =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when Procedure_Call =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when Exit_Statement =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when If_Block    =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when Loop_Block  =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when For_Block   =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;

         when While_Block =>
            pragma Compile_Time_Warning(True, "unimplemented");
            raise Program_Error;


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
