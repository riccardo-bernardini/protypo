pragma Ada_2012;
package body Protypo.API.Engine_Values is
   function bool (x : integer) return Integer
   is (if x /= 0 then 1 else 0);

   function bool (x : float) return Integer
   is (if x /= 0.0 then 1 else 0);

   function bool (x : Engine_Value) return Integer
   is (case x.Class is
          when Int => bool (Get_Integer (x)),
          when real => bool (Get_Float (x)),
          when others => raise Constraint_Error);

   ---------
   -- "-" --
   ---------

   function "-" (X : Engine_Value) return Engine_Value is
   begin
      if not Is_Numeric(x) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      case Numeric_Classes(x.Class) is
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
      if not Is_Numeric(x) then
         raise Constraint_Error with "Non-numeric value";
      end if;

      return create (1 - bool (x));
   end "not";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """+"" unimplemented");
      return raise Program_Error with "Unimplemented function ""+""";
   end "+";


   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """*"" unimplemented");
      return raise Program_Error with "Unimplemented function ""*""";
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """/"" unimplemented");
      return raise Program_Error with "Unimplemented function ""/""";
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """="" unimplemented");
      return raise Program_Error with "Unimplemented function ""=""";
   end "=";


   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """<"" unimplemented");
      return raise Program_Error with "Unimplemented function ""<""";
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
