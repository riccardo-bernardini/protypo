pragma Ada_2012;

package body Protypo.Api.Engine_Values.Iterator_Wrappers is

--     package Gg is new Ada.Containers.Doubly_Linked_Lists (Integer);
--
--     type Z is access Gg.List_Iterator_Interfaces.Reversible_Iterator'Class;
--
--     W : GG.List;
--     X : Z := new Gg.List_Iterator_Interfaces.Reversible_Iterator'Class'(W.Iterate);
--
   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper
     (Iter : Basic_Iterator_Access)
      return Iterator_Wrapper_Access
   is
      Result : Iterator_Wrapper_Access;
   begin
      Result := new Iterator_Wrapper'
        (Iterator => Iter,
         Position => <>);

      Result.Position := Result.Iterator.First;

      return Result;
   end Make_Wrapper;

   -----------
   -- Reset --
   -----------

   procedure Reset (Iter : in out Iterator_Wrapper) is
   begin
      Iter.Position := Iter.Iterator.First;
   end Reset;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Iterator_Wrapper) is
   begin
      Iter.Position := Iter.Iterator.Next (Iter.Position);
   end Next;

   ----------------------
   -- End_Of_Iteration --
   ----------------------

   function End_Of_Iteration (Iter : Iterator_Wrapper) return Boolean
   is (not Has_Element (Iter.Position));

   -------------
   -- Element --
   -------------

   function Element (Iter : Iterator_Wrapper) return Engine_Value
   is (Element(Iter.Position));

end Protypo.Api.Engine_Values.Iterator_Wrappers;
