pragma Ada_2012;
package body Protypo.API.Engine_Values is

   ---------
   -- "-" --
   ---------

   function "-" (X : Engine_Value) return Engine_Value is
   begin
   end "-";

   -----------
   -- "not" --
   -----------

   function "not" (X : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """not"" unimplemented");
      return raise Program_Error with "Unimplemented function ""not""";
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
   -- "-" --
   ---------

   function "-" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """-"" unimplemented");
      return raise Program_Error with "Unimplemented function ""-""";
   end "-";

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

   ----------
   -- "/=" --
   ----------

   function "/=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """/="" unimplemented");
      return raise Program_Error with "Unimplemented function ""/=""";
   end "/=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """<"" unimplemented");
      return raise Program_Error with "Unimplemented function ""<""";
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """<="" unimplemented");
      return raise Program_Error with "Unimplemented function ""<=""";
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """>"" unimplemented");
      return raise Program_Error with "Unimplemented function "">""";
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """>="" unimplemented");
      return raise Program_Error with "Unimplemented function "">=""";
   end ">=";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """and"" unimplemented");
      return raise Program_Error with "Unimplemented function ""and""";
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """or"" unimplemented");
      return raise Program_Error with "Unimplemented function ""or""";
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Engine_Value) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, """xor"" unimplemented");
      return raise Program_Error with "Unimplemented function ""xor""";
   end "xor";

   ------------
   -- Create --
   ------------

   function Create (Val : Integer) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Float) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : String) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Array_Interface_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Record_Interface_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Iterator_Interface_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Function_Interface_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Callback_Function_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Reference_Interface_Access) return Engine_Value is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Val : Integer_Value) return Integer is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Integer unimplemented");
      return raise Program_Error with "Unimplemented function Get_Integer";
   end Get_Integer;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Val : Real_Value) return Float is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Float unimplemented");
      return raise Program_Error with "Unimplemented function Get_Float";
   end Get_Float;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Val : String_Value) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_String unimplemented");
      return raise Program_Error with "Unimplemented function Get_String";
   end Get_String;

   ---------------
   -- Get_Array --
   ---------------

   function Get_Array (Val : Array_Value) return Array_Interface_Access is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Array unimplemented");
      return raise Program_Error with "Unimplemented function Get_Array";
   end Get_Array;

   ----------------
   -- Get_Record --
   ----------------

   function Get_Record (Val : Record_Value) return Record_Interface_Access is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Record unimplemented");
      return raise Program_Error with "Unimplemented function Get_Record";
   end Get_Record;

   ------------------
   -- Get_Iterator --
   ------------------

   function Get_Iterator
     (Val : Iterator_Value) return Iterator_Interface_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Iterator unimplemented");
      return raise Program_Error with "Unimplemented function Get_Iterator";
   end Get_Iterator;

   ------------------
   -- Get_Function --
   ------------------

   function Get_Function
     (Val : Function_Value) return Function_Interface_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Function unimplemented");
      return raise Program_Error with "Unimplemented function Get_Function";
   end Get_Function;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Val : Reference_Value) return Reference_Interface_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Reference unimplemented");
      return raise Program_Error with "Unimplemented function Get_Reference";
   end Get_Reference;

end Protypo.API.Engine_Values;
