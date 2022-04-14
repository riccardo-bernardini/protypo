with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;
--
-- ## What is this?
--
-- This package provides  a wrapper for a vector of Engine_Value.
-- The wrapper implements the `Ambivalent_Interface` that allows for both
-- indexed and record-like access.  More precisely, it exports the following
-- access methods
--
-- * _indexed_ access to access a specific element of the array
-- * _first_, _last_ and _length_  analoguos to the corresponding Ada
--   attributes for arrays.
-- * _range_ and _iterate_ iterators to run over the array content.
--   _range_ iterates over the set of indexes, _iterate_ over the array
--   elements
--
--

package Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers is
   subtype Array_Wrapper_Index is Positive;

   type Array_Wrapper is new Handlers.Ambivalent_Interface with private;
   type Array_Wrapper_Access is access Array_Wrapper;

   function Make_Wrapper
     (Init : Engine_Value_Vectors.Vector := Engine_Value_Vectors.Empty_Vector)
      return Handlers.Ambivalent_Interface_Access;

   function Make_Wrapper
     (Init : Engine_Value_Vectors.Vector := Engine_Value_Vectors.Empty_Vector)
      return Array_Wrapper_Access;

   procedure Set (Container : in out Array_Wrapper;
                  Index     : Array_Wrapper_Index;
                  Value     : Engine_Value);

   procedure Append (Container : in out Array_Wrapper;
                     Item      : Engine_Value);

   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Vectors.Vector)
                 return Handler_Value;

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean;
private
   --  type Vector_Access is access Engine_Value_Vectors.Vector;
   --  Why do we use an access to the vector, rather then the vector itself?
   --  Because we need it for the iterators

   type Array_Wrapper is
     new Handlers.Ambivalent_Interface
   with
      record
         Vector : Engine_Value_Vectors.Vector;
      end record;
end Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers;
