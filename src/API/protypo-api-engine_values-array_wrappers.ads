with Protypo.Api.Engine_Values.Basic_Array_Wrappers;

generic
   type Element_Type is private;

   type Array_Type is
     array (Basic_Array_Wrappers.Array_Wrapper_Index range <>) of Element_Type;

   with function Create (X : Element_Type) return Engine_Value is <>;
package Protypo.Api.Engine_Values.Array_Wrappers is
   subtype Index_Type is Basic_Array_Wrappers.Array_Wrapper_Index;

   type Array_Wrapper is new Ambivalent_Interface with private;
   type Array_Wrapper_Access is access Array_Wrapper;

   function Make_Wrapper (Init : Array_Type)
                          return Ambivalent_Interface_Access;

   function Create (Value : Array_Type) return Engine_Value;
   -- Syntactic sugar equivalent to Create(Make_Wrapper(Value))

   procedure Set (Container : in out Array_Wrapper;
                  Index     : Index_Type;
                  Value     : Element_Type);

   procedure Append (Container : in out Array_Wrapper;
                     Value     : Element_Type);


   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Array)
                 return Handler_Value;

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean;
private
   type Array_Wrapper is
     new Ambivalent_Interface
   with
      record
         A : Basic_Array_Wrappers.Array_Wrapper_Access;
      end record;

   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Array)
                 return Handler_Value
   is (X.A.Get (Index));

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value
   is (X.A.Get (Field));

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
   is (X.A.Is_Field (Field));

   function Create (Value : Array_Type) return Engine_Value
   is (Create (Make_Wrapper (Value)));

end Protypo.Api.Engine_Values.Array_Wrappers;
