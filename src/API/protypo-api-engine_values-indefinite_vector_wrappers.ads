with Ada.Containers.Indefinite_Vectors;

with Protypo.Api.Engine_Values.Engine_Vector_Handlers;
with Protypo.Api.Engine_Values.Handlers;

--
-- ## What is this?
--
-- This is a generic package that allows you to export to the template an "array
-- of something," where the "something" can be an indefinite type (e.g.,
-- String)
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
   type Element_Type (<>) is private;

   type Index_Type is range <>;

   with function "=" (X, Y : Element_Type) return Boolean is <>;

   with package Element_Vectors
     is new Ada.Containers.Indefinite_Vectors (Index_Type   => Index_Type,
                                               Element_Type => Element_Type);

   with function Create (X : Element_Type) return Engine_Value is <>;

   Name : String;
package Protypo.Api.Engine_Values.Indefinite_Vector_Wrappers is

   type Array_Wrapper is new Handlers.Ambivalent_Interface with private;
   type Array_Wrapper_Access is access Array_Wrapper;

   function Make_Wrapper (Init : Element_Vectors.Vector)
                          return Handlers.Ambivalent_Interface_Access;

   function Create (Value : Element_Vectors.Vector) return Engine_Value;
   -- Syntactic sugar equivalent to Create(Make_Wrapper(Value))

   procedure Set (Container : in out Array_Wrapper;
                  Index     : Index_Type;
                  Value     : Element_Type);

   procedure Append (Container : in out Array_Wrapper;
                     Value     : Element_Type);


   overriding function Get (X     : Array_Wrapper;
                            Index : Engine_Value_Array)
                            return Engine_Reference'Class;

   overriding function Get (X     : Array_Wrapper;
                            Field : Id)
                            return Engine_Reference'Class;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean;

   function Type_Name (Item : Array_Wrapper) return String;

   function Image (Item   : Array_Wrapper;
                   Format : String := "")
                   return String;

private
   type Array_Wrapper is
     new Handlers.Ambivalent_Interface
   with
      record
         A : Engine_Vector_Handlers.Vector_Handler_Access;
      end record;

   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Array)
                 return Engine_Reference'Class
   is (X.A.Get (Index));

   function Get (X     : Array_Wrapper;
                 Field : Id)
                 return Engine_Reference'Class
   is (X.A.Get (Field));

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
   is (X.A.Is_Field (Field));

   function Create (Value : Element_Vectors.Vector) return Engine_Value
   is (Handlers.Create (Make_Wrapper (Value)));

end Protypo.Api.Engine_Values.Indefinite_Vector_Wrappers;
