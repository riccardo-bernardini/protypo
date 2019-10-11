pragma Ada_2012;

with Ada.Exceptions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter.Expressions is
   Unvaluable_Expression : exception;

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

   -------------------
   -- Eval_Iterator --
   -------------------

   function Eval_Iterator (Status : Interpreter_Access;
                           Expr   : not null Node_Access)
                           return Iterator_Interface_Access
   is
      use Ada.Containers;

      Tmp    : Engine_Value_Vectors.Vector;
      Result : Engine_Value;
   begin
      Tmp  := Eval_Expression (Status, Expr);

      if Tmp.Length /= 1 then
         raise Bad_Iterator with "Vector expression when iterator expected";
      end if;

      Result := Tmp.First_Element;

      if Result.Class /= Iterator then
         raise Bad_Iterator with "Found " & Result.Class'Image & " when iterator expected";
      end if;

      return Get_Iterator (Result);
   exception
      when E : Unvaluable_Expression =>
         raise Bad_Iterator
           with "Found unvaluable name "
           & Ada.Exceptions.Exception_Message (E)
           & " when iterator expected";
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
            --              Code_Trees.Dump (Expr, 0);
            declare
               Ref : constant Names.Name_Reference := Names.Eval_Name (Status, Expr);
            begin
--  --                 Put_Line ("@@@" & Ref.Class'Image);
--                 if not (Ref.Class in Evaluable_Classes) then
--                    raise Unvaluable_Expression with Ref.Class'image;
--                 end if;

               return To_Vector (To_Value (Ref));
            end;

      end case;
   end Eval_Expression;

   -----------------
   -- Eval_Vector --
   -----------------

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
      use Ada.Containers;

      Tmp    : constant Engine_Value_Vectors.Vector := Eval_Expression (Status, Expr);
      Result : Engine_Value;
   begin
      if Tmp.Length /= 1 then
         raise Constraint_Error;
      end if;

      Result := Tmp.First_Element;

      if not (Result.Class in Scalar_Classes) then
         raise Constraint_Error with Result.Class'Image;
      end if;

      return Result;
   end Eval_Scalar;

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function (Reference : Function_Call_Reference)
                           return Engine_Value_Array
   is
      procedure Apply_Default_And_Varargin
        (Specs      : Api.Engine_Values.Parameter_Signature;
         Parameters : Engine_Value_Vectors.Vector;
         Result     : in out Engine_Value_Array)
        with Pre =>
          Api.Engine_Values.Is_Valid_Parameter_Signature (Specs)
        and Specs'First = Result'First
        and Specs'Last = Result'Last;

      procedure Apply_Default (Specs      : Api.Engine_Values.Parameter_Signature;
                               Parameters : Engine_Value_Vectors.Vector;
                               Result     : in out Engine_Value_Array)
        with Pre =>
          Api.Engine_Values.Is_Valid_Parameter_Signature (Specs)
        and Specs(Specs'Last).Class /= Varargin
        and Specs'First = Result'First
        and Specs'Last = Result'Last;

      procedure Apply_Default (Specs      : Api.Engine_Values.Parameter_Signature;
                               Parameters : Engine_Value_Vectors.Vector;
                               Result     : in out Engine_Value_Array)
      is
      begin
         if not Api.Engine_Values.Is_Valid_Parameter_Signature (Specs) then
            raise Program_Error with "Bad parameter signature";
         end if;

         if Natural (Parameters.Length) > Specs'Length then
            raise Constraint_Error;
         end if;



         declare
            Shift : constant Integer := Parameters.First_Index - Result'First;
         begin
            for Idx in Result'Range loop
               if Idx + Shift <= Parameters.Last_Index then
                  Result (Idx) := Parameters (Idx + Shift);

               elsif Specs (Idx).Class = Optional then
                  Result (Idx) := Specs (Idx).Default;

               else
                  raise Constraint_Error;
               end if;
            end loop;
         end;
      end Apply_Default;

      procedure Apply_Default_And_Varargin
        (Specs      : Api.Engine_Values.Parameter_Signature;
         Parameters : Engine_Value_Vectors.Vector;
         Result     : in out Engine_Value_Array)
      is

      begin
         if not Api.Engine_Values.Is_Valid_Parameter_Signature (Specs) then
            raise Program_Error with "Bad parameter signature";
         end if;

         if Specs (Specs'Last).Class /= Varargin then
            Apply_Default (Specs, Parameters, Result);

         else
            pragma Compile_Time_Warning (True, "Varargin not implemented");
            raise Program_Error with "Varargin not implemented";
         end if;
      end Apply_Default_And_Varargin;


      Funct      : constant Function_Interface_Access := Reference.Function_Handler;
      Parameters : Engine_Value_Array (Funct.Signature'Range);
      --                       Apply_Default (Funct.Signature, Reference.Parameters);
   begin
      Apply_Default_And_Varargin (Specs      => Funct.Signature,
                                  Parameters => Reference.Parameters,
                                  Result     => Parameters);

      return Funct.Process (Parameters);
   end Call_Function;


   --------------
   -- To_Value --
   --------------

   function To_Value (Ref : Names.Name_Reference) return Engine_Value_Array
   is
   begin
--        if not (Ref.Class in Evaluable_Classes) then
--           raise Program_Error;
--        end if;

      case Ref.Class is
         when Names.Constant_Reference =>
            return Engine_Value_Array'(1 => Ref.Costant_Handler.Read);

         when Names.Variable_Reference =>
            return Engine_Value_Array'(1 => Ref.Variable_Handler.Read);

         when Names.Function_Call =>
            return Call_Function (Ref);

         when Names.Function_Reference =>
            return Call_Function
              (Names.Name_Reference'
                 (Class            => Names.Function_Call,
                  Function_Handler => Ref.Function_Handler,
                  Parameters       => Engine_Value_Vectors.Empty_Vector));

         when Names.Array_Reference =>
            return Engine_Value_Array'(1 => Create (Ref.Array_Handler));

         when Names.Record_Reference =>
            return Engine_Value_Array'(1 => Create (Ref.Record_Handler));

         when Names.Ambivalent_Reference =>
            return Engine_Value_Array'(1 => Create (Ref.Ambivalent_Handler));

      end case;
   end To_Value;


end Protypo.Code_Trees.Interpreter.Expressions;
