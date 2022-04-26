pragma Ada_2012;
with Protypo.Api.Constant_References;

package body Protypo.Api.Engine_Values.Record_Wrappers is

   type Record_Access_Handler is
     new Handlers.Constant_Interface
   with
      record
         Pos : Record_Maps.Cursor;
      end record;

   function Read (X : Record_Access_Handler) return Engine_Value
   is
   begin
      return Record_Maps.Element (X.Pos);
   end Read;

   --  function To_Handler (Pos : Record_Maps.Cursor) return Engine_Value
   --    with Post => To_Handler'Result.Class = Constant_Handler;
   --
   --  ----------------
   --  -- To_Handler --
   --  ----------------
   --
   --  function To_Handler (Pos : Record_Maps.Cursor) return Engine_Value
   --  is
   --     use Handlers;
   --  begin
   --     return Create (Constant_Interface_Access'(new Record_Access_Handler'(Pos => Pos)));
   --  end To_Handler;

   ---------
   -- Get --
   ---------

   function Get
     (X     : Record_Wrapper;
      Field : ID)
      return Engine_Reference'Class
   is
   begin
      if not X.Map.Contains (Field) then
         raise Handlers.Unknown_Field with String (Field);
      end if;

      declare
         Result : constant Engine_Value := X.Map (Field);
      begin
         if Result.Class in Handler_Classes then
            return To_Reference (Result);
         else
            return Constant_References.To_Reference (Result);
         end if;
      end;
   end Get;

end Protypo.Api.Engine_Values.Record_Wrappers;
