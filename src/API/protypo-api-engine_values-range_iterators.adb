pragma Ada_2012;
package body Protypo.Api.Engine_Values.Range_Iterators is

   -----------
   -- Reset --
   -----------

   procedure Reset (Iter : in out Range_Iterator) is
   begin
      Iter.Current := Iter.Start;
   end Reset;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Range_Iterator) is
   begin
      Iter.Current := Iter.Current + 1;
   end Next;


end Protypo.Api.Engine_Values.Range_Iterators;
