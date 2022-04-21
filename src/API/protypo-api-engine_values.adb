pragma Ada_2012;
-------------------------------
-- Protypo.API.Engine_Values --
-------------------------------
with Protypo.Api.Engine_Values.Handlers;
package body Protypo.API.Engine_Values is

   function "mod" (X, Y : Integer_Value) return Integer_Value
   is
   begin
      return Engine_Value'(Class   => Int,
                           Int_Val => X.Int_Val mod Y.Int_Val);
   end "mod";

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

   function "not" (X : Engine_Value) return Integer_Value is
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

   function "=" (Left, Right : Engine_Value) return Integer_Value is
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

   function "<" (Left, Right : Engine_Value) return Integer_Value is
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

   function "and" (Left, Right : Integer_Value) return Integer_Value is
   begin
      return Create (Bool (Left) * Bool (Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Integer_Value) return Integer_Value is
   begin
      return Create (Bool (Left) + Bool (Right));
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Integer_Value) return Integer_Value is
   begin
      return Create ((Bool (Left) + Bool (Right)) mod 2);
   end "xor";

   function Element  (V     : Engine_Value_Array;
                      Index : Engine_Index)
                      return Engine_Value
   is (V.V (Index));

   function First_Index (V : Engine_Value_Array) return Engine_Index
   is (V.V.First_Index);

   function Last_Index (V : Engine_Value_Array) return Extended_Index
   is (V.V.Last_Index);

   procedure Append (V    : in out Engine_Value_Array;
                     Item : Engine_Value)
   is
   begin
      V.V.Append (Item);
   end Append;


   function Singleton (Item : Engine_Value)
                       return Engine_Value_Array
   is (V =>  Engine_Value_Arrays.To_Vector (Item, 1));

   function Get_Field (Val   : Engine_Value;
                       Field : Id)
                       return Reference'Class
   is
   begin
      case Val.Class is
         when Record_Handler =>
            return Record_Value (Val).Record_Object.all.Get (Field);

         when Ambivalent_Handler =>
            return Ambivalent_Value (Val).Ambivalent_Object.all.Get (Field);

         when others =>
            raise Constraint_Error;
      end case;
   end Get_Field;

   function Get_Indexed (Val   : Engine_Value;
                         Index : Engine_Value_Array)
                         return Reference'Class
   is
   begin
      case Val.Class is
         when Array_Handler =>
            return Array_Value (Val).Array_Object.Get (Index);

         when Ambivalent_Handler =>
            return Ambivalent_Value (Val).Ambivalent_Object.Get (Index);

         when others =>
            raise Constraint_Error;
      end case;

   end Get_Indexed;


end Protypo.API.Engine_Values;
