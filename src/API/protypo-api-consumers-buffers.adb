pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Protypo.Api.Consumers.Buffers is

   -------------
   -- Process --
   -------------

   overriding procedure Process (Consumer : in out Buffer; Parameter : String)
   is
   begin
      if Consumer.Data /= Null_Unbounded_String then
         Consumer.Data := Consumer.Data & Ascii.Lf;
      end if;

      Consumer.Data := Consumer.Data & Parameter;
   end Process;

   function New_Buffer return Buffer_Access
   is
   begin
      return new Buffer'(Data => Null_Unbounded_String);

   end New_Buffer;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Consumer : Buffer) return String is
   begin
      return To_String (Consumer.Data);
   end Get_Data;

   procedure Destroy (Item : in out Buffer_Access)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Buffer,
                                        Name   => Buffer_Access);
   begin
      Free (Item);
   end Destroy;


end Protypo.Api.Consumers.Buffers;
