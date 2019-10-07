pragma Ada_2012;
package body Protypo.API.Engine_Values is

   function Is_Valid_Parameter_Signature (Signature : Parameter_Signature)
                                          return Boolean
   is
      --
      -- We check this with the following finite automata
      --                non-void
      --   Mandatory -------------->  Optional
      --    ||  ^                       ||  ^
      --    |+--| Void                  |+--| Non-void
      --    |                           |
      --    |  Varargin                 | Varargin
      --    +-----------> Varargin <----+
      --
      -- Note that the next status is determined by the current
      -- value class and that Varargin cannot go anywhere
      --
      Old : Parameter_Class := Mandatory;
   begin
      --
      -- A signature is valid if and only if
      --
      -- * Any Mandatory is preceded by another Mandatory
      -- * Nothing follows Varargin
      --
      for Param  of Signature loop
         if Old = Varargin then
            return False;
         end if;

         if Param.Class = Mandatory and Old /= Mandatory then
            return False;
         end if;

         Old := Param.Class;
      end loop;

      return True;
   end Is_Valid_Parameter_Signature;

   ------------------------
   -- Default_Parameters --
   ------------------------

   function Signature (Fun : Callback_Based_Handler)
                       return Parameter_Signature
   is
      Result : constant Parameter_Signature (2 .. Fun.N_Parameters + 1) :=
                 (others => Parameter_Spec'(Class => Mandatory));
   begin
      return Result;
   end Signature;

   function Bool (X : Integer) return Integer
   is (if X /= 0 then 1 else 0);

   function Bool (X : Float) return Integer
   is (if X /= 0.0 then 1 else 0);

   function Create (X : Boolean) return Engine_Value
   is (Create (Integer'(if X then 1 else 0)));

   function Bool (X : Engine_Value) return Integer
   is (case X.Class is
          when Int    => Bool (Get_Integer (X)),
          when Real   => Bool (Get_Float (X)),
          when others => raise Constraint_Error);

   function Real (X : Engine_Value) return Float
   is (case X.Class is
          when Int    => Float (Get_Integer (X)),
          when Real   => Get_Float (X),
          when others => raise Constraint_Error);


   ---------
   -- "-" --
   ---------

   function "-" (X : Engine_Value) return Engine_Value is
   begin
      if not Is_Numeric (X) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      case Numeric_Classes (X.Class) is
         when Int =>
            return Create (-Get_Integer (X));
         when Real =>
            return Create (-Get_Float (X));
      end case;
   end "-";

   -----------
   -- "not" --
   -----------

   function "not" (X : Engine_Value) return Engine_Value is
   begin
      if not Is_Numeric (X) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      return Create (1 - Bool (X));
   end "not";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (Left) and Is_Scalar (Right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (Left) xor Is_Numeric (Right) then
         raise Constraint_Error;
      end if;

      if Left.Class = Text and Right.Class = Text then
         return Create (Get_String (Left) & Get_String (Right));

      elsif Left.Class = Int and Right.Class = Int then
         return Create (Get_Integer (Left) + Get_Integer (Right));

      else
         return Create (Real (Left) + Real (Right));
      end if;
   end "+";


   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Numeric (Left) and Is_Numeric (Right)) then
         raise Constraint_Error;
      end if;

      if Left.Class = Int and Right.Class = Int then
         return Create (Get_Integer (Left) * Get_Integer (Right));

      else
         return Create (Real (Left) * Real (Right));
      end if;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Numeric (Left) and Is_Numeric (Right)) then
         raise Constraint_Error;
      end if;

      if Left.Class = Int and Right.Class = Int then
         return Create (Get_Integer (Left) / Get_Integer (Right));

      else
         return Create (Real (Left) / Real (Right));
      end if;
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (Left) and Is_Scalar (Right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (Left) xor Is_Numeric (Right) then
         raise Constraint_Error;
      end if;

      if Left.Class = Text and Right.Class = Text then
         return Create (Get_String (Left) = Get_String (Right));

      elsif Left.Class = Int and Right.Class = Int then
         return Create (Get_Integer (Left) = Get_Integer (Right));

      else
         return Create (Real (Left) = Real (Right));
      end if;
   end "=";


   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (Left) and Is_Scalar (Right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (Left) xor Is_Numeric (Right) then
         raise Constraint_Error;
      end if;

      if Left.Class = Text and Right.Class = Text then
         return Create (Get_String (Left) < Get_String (Right));

      elsif Left.Class = Int and Right.Class = Int then
         return Create (Get_Integer (Left) < Get_Integer (Right));

      else
         return Create (Real (Left) < Real (Right));
      end if;
   end "<";


   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create (Bool (Left) * Bool (Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create (Bool (Left) + Bool (Right));
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create ((Bool (Left) + Bool (Right)) mod 2);
   end "xor";

end Protypo.API.Engine_Values;
