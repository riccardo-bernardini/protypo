pragma Ada_2012;
package body Protypo.API.Engine_Values is
   function bool (x : integer) return Integer
   is (if x /= 0 then 1 else 0);

   function bool (x : float) return Integer
   is (if x /= 0.0 then 1 else 0);

   function create (x : Boolean) return Engine_Value
   is (create (integer'(if x then 1 else 0)));

   function bool (x : Engine_Value) return Integer
   is (case x.Class is
          when Int    => bool (Get_Integer (x)),
          when real   => bool (Get_Float (x)),
          when others => raise Constraint_Error);

   function real (x : Engine_Value) return float
   is (case x.Class is
          when Int    => float (Get_Integer (x)),
          when real   => Get_Float (x),
          when others => raise Constraint_Error);


   ---------
   -- "-" --
   ---------

   function "-" (X : Engine_Value) return Engine_Value is
   begin
      if not Is_Numeric (x) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      case Numeric_Classes (x.Class) is
         when Int =>
            return create (-Get_Integer (x));
         when Real =>
            return create (-Get_Float (x));
      end case;
   end "-";

   -----------
   -- "not" --
   -----------

   function "not" (X : Engine_Value) return Engine_Value is
   begin
      if not Is_Numeric (x) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      return create (1 - bool (x));
   end "not";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (left) and Is_Scalar (right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (left) xor Is_Numeric (right) then
         raise Constraint_Error;
      end if;

      if left.Class = text and Right.Class = text then
         return create (Get_String (left) & Get_String (Right));

      elsif left.Class = int and Right.Class = int then
         return create (Get_Integer (Left) + Get_Integer (right));

      else
         return create (real (left) + real (Right));
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

      if left.Class = int and Right.Class = int then
         return create (Get_Integer (Left) * Get_Integer (right));

      else
         return create (real (left) * real (Right));
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

      if left.Class = int and Right.Class = int then
         return create (Get_Integer (Left) / Get_Integer (right));

      else
         return create (real (left) / real (Right));
      end if;
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (left) and Is_Scalar (right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (left) xor Is_Numeric (right) then
         raise Constraint_Error;
      end if;

      if left.Class = text and Right.Class = text then
         return create (Get_String (left) = Get_String (Right));

      elsif left.Class = int and Right.Class = int then
         return create (Get_Integer (Left) = Get_Integer (right));

      else
         return create (real (left) = real (Right));
      end if;
   end "=";


   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Engine_Value) return Engine_Value is
   begin
      if not (Is_Scalar (left) and Is_Scalar (right)) then
         raise Constraint_Error;
      end if;

      if Is_Numeric (left) xor Is_Numeric (right) then
         raise Constraint_Error;
      end if;

      if left.Class = text and Right.Class = text then
         return create (Get_String (left) < Get_String (Right));

      elsif left.Class = int and Right.Class = int then
         return create (Get_Integer (Left) < Get_Integer (right));

      else
         return create (real (left) < real (Right));
      end if;
   end "<";


   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create (bool (left) * bool (right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create (bool (left) + bool (right));
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Engine_Value) return Engine_Value is
   begin
      return Create ((bool (left) + bool (right)) mod 2);
   end "xor";

end Protypo.API.Engine_Values;
