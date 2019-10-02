pragma Ada_2012;
package body Protypo.Code_Trees.Interpreter.Range_Iterators is

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


end Protypo.Code_Trees.Interpreter.Range_Iterators;
