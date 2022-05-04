pragma Ada_2012;
--  with Protypo.Api.Constant_References;

package body Protypo.Api.Engine_Values.Record_Wrappers is

   type Record_Access_Handler is
     new Engine_Reference
   with
      record
         Pos : Record_Maps.Cursor;
      end record;
   overriding
   function Read
     (X : Record_Access_Handler) return Engine_Value;

   overriding
   function Is_Writable
     (Ref : Record_Access_Handler) return Boolean;

   overriding
   procedure Write
     (Ref       : Record_Access_Handler;
      New_Value : Engine_Value);

   function Read (X : Record_Access_Handler) return Engine_Value
   is
   begin
      return Record_Maps.Element (X.Pos);
   end Read;

   function Is_Writable (Ref : Record_Access_Handler) return Boolean
   is (False);


   procedure Write (Ref       : Record_Access_Handler;
                    New_Value : Engine_Value)
   is
   begin
      raise Program_Error;
   end Write;


   ---------
   -- Get --
   ---------

   function Get
     (X     : Record_Wrapper;
      Field : Id)
      return Engine_Reference'Class
   is
      use type Record_Maps.Cursor;

      Pos : constant Record_Maps.Cursor := X.Map.Find (Field);
   begin
      if Pos = Record_Maps.No_Element then
         raise Handlers.Unknown_Field with String (Field);
      else
         return Record_Access_Handler'(Pos => Pos);
      end if;
   end Get;


   function Type_Name (Item : Record_Wrapper) return String
   is ("record");

   function Image (Item   : Record_Wrapper;
                   Format : String := "")
                   return String
   is ("[record]");

end Protypo.Api.Engine_Values.Record_Wrappers;
