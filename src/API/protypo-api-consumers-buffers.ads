with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Protypo.Api.Consumers.Buffers is
   type Buffer (<>) is
     new Consumer_Interface
   with private;

   type Buffer_Access is access Api.Consumers.Buffers.Buffer;

   function New_Buffer return Buffer_Access;

   procedure Destroy (Item  : in out Buffer_Access);

   overriding procedure Process (Consumer  : in out Buffer;
                                 Parameter : String);


   function Get_Data (Consumer : Buffer) return String;
private
   type Buffer is new Consumer_Interface
     with
       record
         Data : Unbounded_String;
      end record;
end Protypo.Api.Consumers.Buffers;
