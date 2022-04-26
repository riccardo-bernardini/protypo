pragma Ada_2012;

with Ada.Exceptions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
--  with Protypo.Api.Engine_Values.Parameter_Lists;

with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Api.Consumers.Buffers;

with Protypo.Code_Trees.Interpreter.Names;

package body Protypo.Code_Trees.Interpreter.Expressions is
   Unvaluable_Expression : exception;

   --  ---------------
   --  -- To_Vector --
   --  ---------------
   --
   --  function To_Vector (X : Engine_Value_Array) return Engine_Value_Array
   --  is
   --     Result : Engine_Value_Array;
   --  begin
   --     for Element of X loop
   --        Result.Append (Element);
   --     end loop;
   --
   --     return Result;
   --  end To_Vector;
   --
   --  --------------
   --  -- To_Array --
   --  --------------
   --
   --  function To_Array (X : Engine_Value_Array) return Engine_Value_Array
   --  is
   --     Result : Engine_Value_Array  := Engine_Value_Vectors.To_Vector (X.First_Index .. X.Last_Index);
   --  begin
   --     for K in Result'Range loop
   --        Result (K) := X (K);
   --     end loop;
   --
   --     return Result;
   --  end To_Array;

   -------------------
   -- Eval_Iterator --
   -------------------

   function Eval_Iterator (Status : Interpreter_Access;
                           Expr   : not null Node_Access)
                           return Handlers.Iterator_Interface_Access
   is
      use Ada.Containers;

      Tmp    : Engine_Value_Array;
   begin
      Tmp  := Eval_Expression (Status, Expr);

      if Tmp.Length /= 1 then
         raise Bad_Iterator with "Vector expression when iterator expected";
      end if;

      declare
         Result : constant Engine_Value := Tmp.First_Element;
      begin
         if Result.Class /= Iterator then
            raise Bad_Iterator with "Found " & Result.Class'Image & " when iterator expected";
         end if;

         return Handlers.Get_Iterator (Result);
      end;
   exception
      when E : Unvaluable_Expression =>
         raise Bad_Iterator
           with "Found unvaluable name "
           & Ada.Exceptions.Exception_Message (E)
           & " when iterator expected";
   end Eval_Iterator;


   function Eval_Expression (Status : Interpreter_Access;
                             Expr   : not null Node_Access)
                             return Engine_Value_Array
   is

      function Embed (X : Engine_Value) return Engine_Value_Array
      is
         Result : Engine_Value_Array;
      begin
         Result.Append (X);
         return Result;
      end Embed;

      -----------
      -- Apply --
      -----------

      function Apply (Op : Tokens.Unary_Operator;
                      X  : Engine_Value)
                      return Engine_Value_Array
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
                      return Engine_Value_Array
      is
         use Tokens;

      begin
         case Op is
            when Kw_Mod      =>
               if not (Left.Class = Int and Right.Class = Int) then
                  raise Run_Time_Error with """mod"" defined only for integer values";
               end if;

               return Embed (Left mod Right);

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

      function Do_Capture (Status : Interpreter_Access;
                           Name   : Unbounded_Id;
                           Params : Node_Vectors.Vector)
                           return Engine_Value_Array
      is
         Buffer : Api.Consumers.Buffers.Buffer_Access := Api.Consumers.Buffers.New_Buffer;
      begin
         Push_Consumer (Status, Api.Consumers.Consumer_Access (Buffer));

         Statements.Do_Procedure_Call (Status => Status,
                                       Name   => Name,
                                       Params => Params);

         Pop_Consumer (Status);

         return Result : constant Engine_Value_Array :=
           Embed (Api.Engine_Values.Create (Buffer.Get_Data))
         do
            Api.Consumers.Buffers.Destroy (Buffer);
         end return;

      end Do_Capture;

   begin

      if not (Expr.Class in Code_Trees.Expression) then
         raise Program_Error
           with "Trying evaluating code that is not an expression, class="
           & Expr.Class'Image;
      end if;

      case Code_Trees.Expression (Expr.Class) is
         when Binary_Op   =>
            declare
               Left  : constant Engine_Value := Eval_Scalar (Status, Expr.Left);
               Right : constant Engine_Value  := Eval_Scalar (Status, Expr.Right);
            begin
               return  Apply (Expr.Operator, Left, Right);
            end;

         when Unary_Op    =>
            declare
               Operand : constant Engine_Value := Eval_Scalar (Status, Expr.Operand);
            begin
               return  Apply (Expr.Uni_Op, Operand);
            end;

         when Int_Constant =>
            return Embed (Create (Expr.N));

         when Real_Constant =>
            return Embed (Create (Expr.X));

         when Text_Constant =>
            return Embed (Create (To_String (Expr.S)));


         when Capture_Call =>
            return Do_Capture (Status, Expr.Name, Expr.Params);


         when Selected =>
            declare
               Ref : constant Engine_Reference'Class :=
                       Names.Eval_Name (Status, Expr.Record_Var);

               Val : constant Engine_Value := Ref.Read;
            begin
               if not (Val.Class in Record_Like_Handler) then
                  raise Run_Time_Error
                    with
                      "Trying to access as a record a value of type "
                      & Val.Class'Image
                    & " at " & Tokens.Image (Expr.Source_Position);
               end if;

               declare
                  Result : constant Engine_Value :=
                             Get_Field (Val, To_Id (Expr.Field_Name)).Read;
               begin
                  return Singleton (Result);
               end;
            end;

         when Identifier  =>
            declare
               Ref : constant Engine_Reference'Class := Names.Eval_Name (Status, Expr);
            begin
               return Singleton (Ref.Read);
            end;

         when Indexed =>
            --
            --  This case is a bit special since it can represent different
            --  things, namely: an array access or a function call.
            --  The only possibility for a function call is when the field
            --  Indexed_Var is an identifier (we cannot "return functions").
            --
            --  Note that Indexed_Var can be an identifier and still have
            --  an array access.
            --
            if Expr.Indexed_Var.Class /= Identifier then
               declare
                  use type Ada.Containers.Count_Type;

                  Basis : constant Engine_Value_Array :=
                            Eval_Expression (Status, Expr.Indexed_Var);


                  Indexes : constant Engine_Value_Array :=
                              Eval_Vector (Status, Expr.Indexes);

                  Ref : constant Engine_Value :=
                          (if Basis.Length = 1
                           then
                              Basis.First_Element
                           else
                              Void_Value);
               begin
                  if Basis.Length /= 1 then
                     raise Run_Time_Error
                       with
                         "Array name evaluation returns "
                         & Basis.Length'Image & " values, one was expected";
                  end if;

                  if not (Ref.Class in Indexed_Handler) then
                     raise Run_Time_Error
                       with
                         "Indexed access to value of type "
                         & Ref.Class'Image;
                  end if;

                  return Singleton (Get_Indexed (Ref, Indexes).Read);
               end;

            end if;

            pragma Assert (Expr.Indexed_Var.Class = Identifier);

            declare
               use Protypo.Api.Symbols;
               use type Protypo_Tables.Cursor;

               Var_Name : constant Id := To_Id (Expr.Indexed_Var.Id_Value);

               Pos : constant Protypo_Tables.Cursor :=
                       Status.Symbol_Table.Find (Var_Name);

               Value : constant Engine_Value :=
                         (if Pos = Protypo_Tables.No_Element
                          then
                             Void_Value
                          else
                             Protypo_Tables.Value (Pos));

               Parameters : constant Engine_Value_Array :=
                              Eval_Vector (Status, Expr.Indexes);
            begin
               if Pos = Protypo_Tables.No_Element then
                  raise Run_Time_Error
                    with "Unknown identifier " & String (Var_Name);
               end if;

               case Value.Class is
                  when Array_Handler | Ambivalent_Handler =>
                     declare
                        Ref : constant Engine_Reference'Class :=
                                Get_Indexed (Value, Parameters);
                     begin
                        return Singleton (Ref.Read);
                     end;

                  when Function_Handler =>
                     declare
                        Result : constant Engine_Value_Array :=
                                   Call_Function (Value, Parameters);
                     begin
                        if Result.Is_Empty then
                           raise Run_Time_Error
                             with "Procedure called in an expression context";
                        else
                           return Result;
                        end if;
                     end;
                  when others =>
                     raise Run_Time_Error
                       with "Array access to wrong value type " & Value.Class'Image;
               end case;
            end;
      end case;
   end Eval_Expression;

   -----------------
   -- Eval_Vector --
   -----------------

   function Eval_Vector (Status : Interpreter_Access;
                         Expr   : Node_Vectors.Vector)
                         return Engine_Value_Array
   is
      Result : Engine_Value_Array;
   begin
      for Ex of Expr loop
         Result.Append (Eval_Expression (Status, Ex));
      end loop;

      return Result;
   end Eval_Vector;


   function Eval_Single_Expression (Status : Interpreter_Access;
                                    Expr   : not null Node_Access)
                                    return Engine_Value
   is
      use type Ada.Containers.Count_Type;
      Tmp    : constant Engine_Value_Array := Eval_Expression (Status, Expr);
   begin
      if Tmp.Length /= 1 then
         raise Run_Time_Error
           with "Single value expected";
      end if;

      return Tmp.First_Element;
   end Eval_Single_Expression;

   -----------------
   -- Eval_Scalar --
   -----------------

   function Eval_Scalar (Status : Interpreter_Access;
                         Expr   : not null Node_Access)
                         return Engine_Value
   is
      Result : constant Engine_Value := Eval_Single_Expression (Status, Expr);
   begin
      if not (Result.Class in Scalar_Classes) then
         raise Run_Time_Error
           with
         Result.Class'Image & " at " & Tokens.Image (Expr.Source_Position);
      end if;

      return Result;
   end Eval_Scalar;



   --------------
   -- To_Value --
   --------------

   --  function To_Value (Ref : Names.Name_Reference) return Engine_Value_Array
   --  is
   --  begin
   --     --        if not (Ref.Class in Evaluable_Classes) then
   --     --           raise Program_Error;
   --     --        end if;
   --
   --     case Ref.Class is
   --        when Names.Constant_Reference =>
   --           return Engine_Value_Vectors.To_Vector (Ref.Costant_Handler.Read, 1);
   --
   --        when Names.Variable_Reference =>
   --           return Engine_Value_Vectors.To_Vector (Ref.Variable_Handler.Read, 1);
   --
   --        when Names.Function_Call =>
   --           return Call_Function (Ref);
   --
   --        when Names.Function_Reference =>
   --           return Call_Function
   --             (Names.Name_Reference'
   --                (Class            => Names.Function_Call,
   --                 Function_Handler => Ref.Function_Handler,
   --                 Parameters       => Engine_Value_Vectors.Empty_Vector));
   --
   --        when Names.Array_Reference =>
   --           return Engine_Value_Vectors.To_Vector
   --             (Handlers.Create (Ref.Array_Handler), 1);
   --
   --        when Names.Record_Reference =>
   --           return Engine_Value_Vectors.To_Vector
   --             (Handlers.Create (Ref.Record_Handler), 1);
   --
   --        when Names.Ambivalent_Reference =>
   --           return Engine_Value_Vectors.To_Vector
   --             (Handlers.Create (Ref.Ambivalent_Handler), 1);
   --
   --     end case;
   --  end To_Value;


end Protypo.Code_Trees.Interpreter.Expressions;
