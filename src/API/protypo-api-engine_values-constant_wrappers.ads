package Protypo.Api.Engine_Values.Constant_Wrappers is
   type Constant_Wrapper is new Constant_Interface with private;
   type Constant_Wrapper_Access is access Constant_Wrapper;

   function Make_Wrapper (Value : Engine_Value) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : Integer) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : String) return Constant_Wrapper_Access;
   function Read (X : Constant_Wrapper) return Engine_Value;

   function To_Handler_Value (Value : Engine_Value) return Handler_Value;
   function To_Handler_Value (Value : Integer) return Handler_Value;
   function To_Handler_Value (Value : String) return Handler_Value;
private
   type Constant_Wrapper is new Constant_Interface
   with
      record
         Value : Engine_Value;
      end record;


   function Read (X : Constant_Wrapper) return Engine_Value
   is (X.Value);

   function Make_Wrapper (Value : Engine_Value) return Constant_Wrapper_Access
   is (new Constant_Wrapper'(Value => Value));

   function Make_Wrapper (Value : Integer) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function Make_Wrapper (Value : String) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function To_Handler_Value (Value : Integer) return Handler_Value
   is (To_Handler_Value (Create (Value)));

   function To_Handler_Value (Value : String) return Handler_Value
   is (To_Handler_Value (Create (Value)));

end Protypo.Api.Engine_Values.Constant_Wrappers;
