with Protypo.Api.Engine_Values.Constant_Wrappers;

package Protypo.Code_Trees.Interpreter.Range_Iterators is
   type Range_Iterator is new Iterator_Interface with private;

   function Create (Start, Stop : Integer) return Iterator_Interface_Access;
   procedure Reset (Iter : in out Range_Iterator);
   procedure Next (Iter : in out Range_Iterator);

   function End_Of_Iteration (Iter : Range_Iterator) return Boolean;

   function Element (Iter : Range_Iterator) return Handler_Value;
private
   use Protypo.Api.Engine_Values.Constant_Wrappers;

   type Range_Iterator is new Iterator_Interface with
      record
         Start   : Integer;
         Stop    : Integer;
         Current : Integer;
      end record;


   function Create (Start, Stop : Integer) return Iterator_Interface_Access
   is (new Range_Iterator'(Start   => Start,
                           Stop    => Stop,
                           Current => Start));

   function End_Of_Iteration (Iter : Range_Iterator) return Boolean
   is (Iter.Current > Iter.Stop);

   function Element (Iter : Range_Iterator) return Handler_Value
   is (Create (Constant_Interface_Access (Make_Wrapper (Iter.Current))));
end Protypo.Code_Trees.Interpreter.Range_Iterators;
