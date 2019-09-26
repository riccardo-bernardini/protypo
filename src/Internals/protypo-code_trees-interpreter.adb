pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;
with Protypo.Code_Trees.Interpreter.Compiled_Functions;

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
   pragma Unreferenced (To_Vector);

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

   function eval (expr   : Node_Vectors.Vector;
                  status : Status_Access)
               return Engine_Value_Vectors.Vector;

   function Eval_Name (Expr   : not null Node_Access;
                       status : Status_Access)
                       return Engine_Value
         with
               Pre => Expr.Class in Name;

   function Eval_Name (Expr   : not null Node_Access;
                       status : Status_Access)
                       return Engine_Value
   is
   begin
      if not (Expr.Class in Name) then
         raise Program_Error;
      end if;

      case Name (Expr.Class) is
         when Selected =>
            declare
               head : constant Engine_Value := Eval_Name (expr.Record_Var, status);
            begin
               if head.Class /= Record_Handler then
                  raise Constraint_Error;
               end if;

               return Get_Record (head).get (To_String (expr.Field_Name));
            end;
         when Indexed =>
            declare
               head    : constant Engine_Value := Eval_Name (expr.Indexed_Var, status);
               indexes : Engine_Value_Vectors.Vector := Eval (expr.Indexes, status);
            begin
               if head.Class = Array_Handler then
                  return get_array (Get_Array (head), indexes);

               elsif head.Class = Function_Handler then
                  return call_function (Get_Function (head), indexes);

               else
                  raise Constraint_Error;
               end if;
            end;
         when Identifier =>
            pragma Compile_Time_Warning (True, "unimplemented");
            raise Program_Error;

      end case;
   end Eval_Name;

   function eval (expr   : Node_Vectors.Vector;
                  status : Status_Access)
                  return Engine_Value_Vectors.Vector
   is
      result : Engine_Value_Vectors.Vector;
   begin
      for ex of expr loop
         result.append (eval (ex, status));
      end loop;

      return result;
   end eval;

   function Eval (Expr   : not null Node_Access;
                  Status : Status_Access)
                  return Engine_Value_Vectors.Vector
   is
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
            when Plus =>
               return X;

            when Minus =>
               return -X;

            when Kw_Not =>
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
            when Plus =>
               return Left + Right;

            when Minus =>
               return Left - Right;

            when Mult =>
               return Left * Right;

            when Div =>
               return Left / Right;

            when Equal =>
               return Left = Right;

            when Different =>
               return Left /= Right;

            when Less_Than =>
               return Left < Right;

            when Greater_Than =>
               return Left > Right;

            when Less_Or_Equal =>
               return Left <= Right;

            when Greater_Or_Equal =>
               return Left >= Right;

            when Kw_And =>
               return Left and Right;

            when Kw_Or =>
               return Left or Right;

            when Kw_Xor =>
               return Left xor Right;
         end case;
      end Apply;

      Left, Right : Engine_Value;
   begin
      if not (Expr.Class in Code_Trees.Expression) then
         raise Program_Error;
      end if;

      case Code_Trees.Expression (Expr.Class) is
         when Binary_Op =>
            Left := + Eval (Expr.Left, Status);
            Right := + Eval (Expr.Right, Status);

            return + Apply (Expr.Operator, Left, Right);

         when Unary_Op =>
            Right := + Eval (Expr.Operand, Status);

            return + Apply (Expr.Uni_Op, Right);

         when Int_Constant =>
            return + Create (Expr.N);

         when Real_Constant =>
            return + Create (Expr.X);

         when Text_Constant =>
            return + Create (To_String (Expr.S));

         when Selected =>
            raise Program_Error;

         when Indexed =>
            raise Program_Error;

         when Identifier =>
            raise Program_Error;

      end case;
   end Eval;



   ---------
   -- Run --
   ---------

   procedure Run (Program : Node_Vectors.Vector;
                  Status  : Status_Access)
   is

   begin
      for Statement of Program loop
         Run (Statement, Status);

         if Status.Break.Breaking_Reason /= None then
            return;
         end if;
      end loop;
   end Run;


   ---------
   -- Run --
   ---------

   procedure Run (Program : not null Node_Access;
                  Status  : Status_Access)
   is
      use Compiled_Functions;
   begin
      if Program.Class not in Statement_Classes then
         raise Program_Error;
      end if;


      case Statement_Classes (Program.Class) is
         when Statement_Sequence =>
            Run (Program.Statements, Status);

         when Defun =>
            Status.Symbol_Table.Create
                  (Name          =>
                      To_String (Program.Definition_Name),
                   Initial_Value =>
                      Create (new Compiled_Function'(Function_Body => Program.Function_Body,
                                                     Parameters    => Program.Parameters,
                                                     Status        => Status)));

         when Assignment =>
            raise Program_Error;

         when Return_Statement =>
            raise Program_Error;

         when Procedure_Call =>
            raise Program_Error;

         when Exit_Statement =>
            raise Program_Error;

         when If_Block =>
            raise Program_Error;

         when Loop_Block =>
            raise Program_Error;

         when For_Block =>
            raise Program_Error;

         when While_Block =>
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

      Interpreter : constant Status_Access :=
                      new Interpreter_Status'(Break        => No_Break,
                                              Symbol_Table => Copy_Globals (Symbol_Table));
   begin
      Add_Builtin_Values (Interpreter.Symbol_Table);

      Run (Program.Pt, Interpreter);

      if Interpreter.Break /= No_Break  then
         raise Program_Error;
      end if;
   end Run;

end Protypo.Code_Trees.Interpreter;
