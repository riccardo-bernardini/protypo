pragma Ada_2012;
package body Protypo.Api.Engine_Values.Record_Wrappers is

   type Record_Access_Handler is
     new Constant_Interface
   with
      record
         Pos : Record_Maps.Cursor;
      end record;

   function Read (X : Record_Access_Handler) return Engine_Value
   is
   begin
      return Record_Maps.Element (X.Pos);
   end Read;

   function To_Handler (Pos : Record_Maps.Cursor) return Engine_Value
     with Post => To_Handler'Result.Class = Constant_Handler;

   ----------------
   -- To_Handler --
   ----------------

   function To_Handler (Pos : Record_Maps.Cursor) return Engine_Value
   is
   begin
      return Create (Constant_Interface_Access'(new Record_Access_Handler'(Pos => Pos)));
   end To_Handler;

   ---------
   -- Get --
   ---------

   function Get
     (X     : Record_Wrapper;
      Field : ID)
      return Handler_Value
   is
      Result : Engine_Value;
   begin
      if not X.Map.Contains (Field) then
         raise Unknown_Field with Field;
      end if;

      Result := X.Map (Field);

      if Result.Class in Handler_Classes then
         return Result;
      else
         return To_Handler (X.Map.Find (Field));
      end if;
   end Get;

end Protypo.Api.Engine_Values.Record_Wrappers;
