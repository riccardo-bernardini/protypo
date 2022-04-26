with Protypo.Api.Engine_Values.Constant_Wrappers;
with Protypo.Api.Engine_Values.Handlers;

--
-- ## What is this?
--
-- A _range iterator_ implements the `Iterator_Interface` and it
-- allows to iterate over an interval between two integers
--
package Protypo.Api.Engine_Values.Range_Iterators is
   type Range_Iterator is new Handlers.Iterator_Interface with private;

   function Create (Start, Stop : Integer) return Handlers.Iterator_Interface_Access;
   function Create (Start, Stop : Integer) return Handler_Value
   is (Handlers.Create (Create (Start, Stop)));

   overriding procedure Reset (Iter : in out Range_Iterator);
   overriding procedure Next (Iter : in out Range_Iterator);
   overriding function End_Of_Iteration (Iter : Range_Iterator) return Boolean;
   overriding function Element (Iter : Range_Iterator) return Handler_Value;
private
   use Protypo.Api.Engine_Values.Constant_Wrappers;

   type Range_Iterator is new Handlers.Iterator_Interface with
      record
         Start   : Integer;
         Stop    : Integer;
         Current : Integer;
      end record;


   function Create (Start, Stop : Integer) return Handlers.Iterator_Interface_Access
   is (new Range_Iterator'(Start   => Start,
                           Stop    => Stop,
                           Current => Start));

   function End_Of_Iteration (Iter : Range_Iterator) return Boolean
   is (Iter.Current > Iter.Stop);

   function Element (Iter : Range_Iterator) return Handler_Value
   is (Handlers.Create (Handlers.Constant_Interface_Access (Make_Wrapper (Iter.Current))));
end Protypo.Api.Engine_Values.Range_Iterators;
