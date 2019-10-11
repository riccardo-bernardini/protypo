with Ada.Containers.Vectors;

package Protypo.Api.Engine_Values.Basic_Array_Wrappers is
   subtype Array_Wrapper_Index is Positive;

   type Array_Wrapper is new Ambivalent_Interface with private;
   type Array_Wrapper_Access is access Array_Wrapper;

   function Make_Wrapper (Init : Engine_Value_Array := No_Value)
                          return Array_Wrapper_Access;

   procedure Set (Container : in out Array_Wrapper;
                  Index     : Array_Wrapper_Index;
                  Value     : Engine_Value);

   function Get (X     : Array_Wrapper;
                 Index : Engine_Value_Array)
                 return Handler_Value;

   function Get (X     : Array_Wrapper;
                 Field : ID)
                 return Handler_Value;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean;
private
   package Engine_Value_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Array_Wrapper_Index,
                                 Element_Type => Engine_Value);

   type Vector_Access is access Engine_Value_Vectors.Vector;

   type Array_Wrapper is
     new Ambivalent_Interface
   with
      record
         Vector : Vector_Access;
      end record;
end Protypo.Api.Engine_Values.Basic_Array_Wrappers;
