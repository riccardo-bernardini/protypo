pragma Ada_2012;

with Ada.Exceptions;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
with Protypo.Api.Engine_Values.Parameter_Lists;

with Protypo.Code_Trees.Interpreter.Statements;
with Protypo.Api.Consumers.Buffers;

package body Protypo.Code_Trees.Interpreter.Expressions is
   Unvaluable_Expression : exception;

   --  ---------------
   --  -- To_Vector --
   --  ---------------
   --
   --  function To_Vector (X : Engine_Value_Vectors.Vector) return Engine_Value_Vectors.Vector
   --  is
   --     Result : Engine_Value_Vectors.Vector;
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
   --  function To_Array (X : Engine_Value_Vectors.Vector) return Engine_Value_Vectors.Vector
   --  is
   --     Result : Engine_Value_Vectors.Vector  := Engine_Value_Vectors.To_Vector (X.First_Index .. X.Last_Index);
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

      Tmp    : Engine_Value_Vectors.Vector;
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
                           return Engine_Value_Vectors.Vector
      is
         Buffer : Api.Consumers.Buffers.Buffer_Access := Api.Consumers.Buffers.New_Buffer;
      begin
         Push_Consumer (Status, Api.Consumers.Consumer_Access (Buffer));

         Statements.Do_Procedure_Call (Status => Status,
                                       Name   => Name,
                                       Params => Params);

         Pop_Consumer (Status);

         return Result : constant Engine_Value_Vectors.Vector :=
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

         when Selected | Indexed | Identifier  =>
            --              Code_Trees.Dump (Expr, 0);
            declare
               Ref : constant Names.Name_Reference := Names.Eval_Name (Status, Expr);
            begin
               --  --                 Put_Line ("@@@" & Ref.Class'Image);
               --                 if not (Ref.Class in Evaluable_Classes) then
               --                    raise Unvaluable_Expression with Ref.Class'image;
               --                 end if;

               return To_Value (Ref);
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
   begin
      if Tmp.Length /= 1 then
         raise Constraint_Error;
      end if;

      declare
         Result : constant Engine_Value := Tmp.First_Element;
      begin
         if not (Result.Class in Scalar_Classes) then
            raise Constraint_Error
              with
            Result.Class'Image & " at " & Tokens.Image (Expr.Source_Position);
         end if;

         return Result;
      end;
   end Eval_Scalar;

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function (Reference : Function_Call_Reference)
                           return Engine_Value_Vectors.Vector
   is
      use type Parameter_Lists.Parameter_Spec;

      procedure Apply_Default_And_Varargin
        (Specs      : Parameter_Lists.Parameter_Signature;
         Parameters : Engine_Value_Vectors.Vector;
         Result     : in out Engine_Value_Vectors.Vector)
        with Pre =>
          Parameter_Lists.Is_Valid_Parameter_Signature (Specs);

      procedure Apply_Default (Specs      : Parameter_Lists.Parameter_Signature;
                               Parameters : Engine_Value_Vectors.Vector;
                               Result     : in out Engine_Value_Vectors.Vector)
        with Pre =>
          Parameter_Lists.Is_Valid_Parameter_Signature (Specs)
          and (Specs'Length = 0 or else Specs (Specs'Last) /= Parameter_Lists.Varargin);

      procedure Apply_Default (Specs      : Parameter_Lists.Parameter_Signature;
                               Parameters : Engine_Value_Vectors.Vector;
                               Result     : in out Engine_Value_Vectors.Vector)
      is
      begin
         if not Parameter_Lists.Is_Valid_Parameter_Signature (Specs) then
            raise Program_Error with "Bad parameter signature";
         end if;

         if Natural (Parameters.Length) > Specs'Length then
            raise Constraint_Error;
         end if;

         declare
            use type Engine_Value_Vectors.Cursor;

            Pos : Engine_Value_Vectors.Cursor := Parameters.First;
         begin
            for Spec of Specs loop
               if Pos /= Engine_Value_Vectors.No_Element then
                  Result.Append (Engine_Value_Vectors.Element (Pos));

               elsif Parameter_Lists.Is_Optional (Spec) then
                  Result.Append (Parameter_Lists.Default_Value (Spec));

               else
                  raise Constraint_Error;
               end if;

               Engine_Value_Vectors.Next (Pos);
            end loop;
         end;

      end Apply_Default;

      procedure Apply_Default_And_Varargin
        (Specs      : Parameter_Lists.Parameter_Signature;
         Parameters : Engine_Value_Vectors.Vector;
         Result     : in out Engine_Value_Vectors.Vector)
      is

      begin
         if not Parameter_Lists.Is_Valid_Parameter_Signature (Specs) then
            raise Program_Error with "Bad parameter signature";
         end if;

         if Specs'Length = 0 or else Specs (Specs'Last) /= Parameter_Lists.Varargin then
            Apply_Default (Specs, Parameters, Result);

         else
            pragma Compile_Time_Warning (False, "Varargin not implemented");
            raise Program_Error with "Varargin not implemented";
         end if;
      end Apply_Default_And_Varargin;


      Funct      : constant Handlers.Function_Interface_Access := Reference.Function_Handler;
      Parameters : Engine_Value_Vectors.Vector;
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

   function To_Value (Ref : Names.Name_Reference) return Engine_Value_Vectors.Vector
   is
   begin
      --        if not (Ref.Class in Evaluable_Classes) then
      --           raise Program_Error;
      --        end if;

      case Ref.Class is
         when Names.Constant_Reference =>
            return Engine_Value_Vectors.To_Vector (Ref.Costant_Handler.Read, 1);

         when Names.Variable_Reference =>
            return Engine_Value_Vectors.To_Vector (Ref.Variable_Handler.Read, 1);

         when Names.Function_Call =>
            return Call_Function (Ref);

         when Names.Function_Reference =>
            return Call_Function
              (Names.Name_Reference'
                 (Class            => Names.Function_Call,
                  Function_Handler => Ref.Function_Handler,
                  Parameters       => Engine_Value_Vectors.Empty_Vector));

         when Names.Array_Reference =>
            return Engine_Value_Vectors.To_Vector
              (Handlers.Create (Ref.Array_Handler), 1);

         when Names.Record_Reference =>
            return Engine_Value_Vectors.To_Vector
              (Handlers.Create (Ref.Record_Handler), 1);

         when Names.Ambivalent_Reference =>
            return Engine_Value_Vectors.To_Vector
              (Handlers.Create (Ref.Ambivalent_Handler), 1);

      end case;
   end To_Value;


end Protypo.Code_Trees.Interpreter.Expressions;
