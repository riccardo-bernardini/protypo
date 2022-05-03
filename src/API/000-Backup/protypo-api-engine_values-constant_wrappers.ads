with Protypo.Api.Engine_Values.Handlers;
with protypo.Api.Engine_Values.engine_value_holders;
--
-- ## What is this?
--
-- A constant wrapper is just a wrapper around a constant value
--
package Protypo.Api.Engine_Values.Constant_Wrappers is
   type Constant_Wrapper is new handlers.Constant_Interface with private;
   type Constant_Wrapper_Access is access Constant_Wrapper;

   function Read (X : Constant_Wrapper) return Engine_Value;

   function Make_Wrapper (Value : Engine_Value) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : Integer) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : Boolean) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : Float) return Constant_Wrapper_Access;
   function Make_Wrapper (Value : String) return Constant_Wrapper_Access;

   function To_Handler_Value (Value : Engine_Value) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

   function To_Handler_Value (Value : Integer) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

   function To_Handler_Value (Value : Boolean) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

   function To_Handler_Value (Value : Float) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

   function To_Handler_Value (Value : String) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

   function To_Handler_Value (Value : Unbounded_String) return Handler_Value;
   -- Equivalent to Create(Make_Wrapper(Value))

private
   type Constant_Wrapper is new handlers.Constant_Interface
   with
      record
         Value : engine_value_holders.Holder;
      end record;


   function Read (X : Constant_Wrapper) return Engine_Value
   is (X.Value.Element);

   function Make_Wrapper (Value : Engine_Value) return Constant_Wrapper_Access
   is (new Constant_Wrapper'(Value => Engine_Value_Holders.To_Holder (Value)));

   function Make_Wrapper (Value : Integer) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function Make_Wrapper (Value : Boolean) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function Make_Wrapper (Value : Float) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function Make_Wrapper (Value : String) return Constant_Wrapper_Access
   is (Make_Wrapper (Create (Value)));

   function To_Handler_Value (Value : Integer) return Handler_Value
   is (To_Handler_Value (Create (Value)));

   function To_Handler_Value (Value : Float) return Handler_Value
   is (To_Handler_Value (Create (Value)));

   function To_Handler_Value (Value : String) return Handler_Value
   is (To_Handler_Value (Create (Value)));

   function To_Handler_Value (Value : Boolean) return Handler_Value
   is (To_Handler_Value (Create (Value)));


   function To_Handler_Value (Value : Unbounded_String) return Handler_Value
   is (To_Handler_Value (To_String (Value)));

end Protypo.Api.Engine_Values.Constant_Wrappers;
