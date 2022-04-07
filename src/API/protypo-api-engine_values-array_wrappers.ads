with Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;
with Protypo.Api.Engine_Values.Handlers;

--
-- ## What is this?
--
-- This is a generic package that allows you to export to the template
-- an "array of something".
--
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
-- The programmer must provide
-- * the type of the element
-- * the type of the array
-- * a function to convert an element to an `Engine_Value`
--
generic
   type Element_Type is private;

   type Array_Type is
     array (Engine_Value_Array_Wrappers.Array_Wrapper_Index range <>) of Element_Type;

   with function Create (X : Element_Type) return Engine_Value is <>;
package Protypo.Api.Engine_Values.Array_Wrappers is
   subtype Index_Type is Engine_Value_Array_Wrappers.Array_Wrapper_Index;

   type Array_Wrapper is new Handlers.Ambivalent_Interface with private;
   type Array_Wrapper_Access is access Array_Wrapper;

   function Make_Wrapper (Init : Array_Type)
                          return Handlers.Ambivalent_Interface_Access;

   function Create (Value : Array_Type) return Ambivalent_Value;
   -- Syntactic sugar equivalent to Create(Make_Wrapper(Value))

   procedure Set (Container : in out Array_Wrapper;
                  Index     : Index_Type;
                  Value     : Element_Type);

   procedure Append (Container : in out Array_Wrapper;
                     Value     : Element_Type);


   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Vectors.Vector)
                 return Handler_Value;

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean;
private
   type Array_Wrapper is
     new Handlers.Ambivalent_Interface
   with
      record
         A : Engine_Value_Array_Wrappers.Array_Wrapper_Access;
      end record;

   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Vectors.Vector)
                 return Handler_Value
   is (X.A.Get (Index));

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value
   is (X.A.Get (Field));

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
   is (X.A.Is_Field (Field));

   function Create (Value : Array_Type) return Ambivalent_Value
   is (Handlers.Create (Make_Wrapper (Value)));

end Protypo.Api.Engine_Values.Array_Wrappers;
