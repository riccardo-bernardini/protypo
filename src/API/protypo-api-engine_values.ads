with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Protypo.API.Engine_Values is
   type Engine_Value_Class is
     (
      Void,
      Int,
      Real,
      Text,
      Array_Like,
      Record_Like,
      Iterator,
      Function_Handler
     );

   type Engine_Value (Class : Engine_Value_Class := Void) is private;
   type Engine_Value_Array is array (Positive range <>) of Engine_Value;

   Void_Value : constant Engine_Value;
   No_Value : constant Engine_Value_Array;

   subtype Integer_Value  is Engine_Value (Int);
   subtype Real_Value     is Engine_Value (Real);
   subtype String_Value   is Engine_Value (Text);
   subtype Array_Value    is Engine_Value (Array_Like);
   subtype Record_Value   is Engine_Value (Record_Like);
   subtype Iterator_Value is Engine_Value (Iterator);
   subtype Function_Value is Engine_Value (Function_Handler);

   type Array_Interface is interface;
   type Array_Interface_Access is not null access all Array_Interface'Class;

   function Get (X     : Array_Interface;
                 Index : Engine_Value_Array)
                 return Engine_Value
                 is abstract;

   procedure Set (X     : in out Array_Interface;
                  Index : Engine_Value_Array;
                  Value : Engine_Value)
   is abstract;

   type Record_Interface is interface;
   type Record_Interface_Access is not null access all Record_Interface'Class;

   function Get (X     : Array_Interface;
                 Field : String)
                 return Engine_Value
                 is abstract;

   procedure Set (X     : in out Array_Interface;
                  Field : String;
                  Value : Engine_Value)
   is abstract;

   type Iterator_Interface is interface;
   type Iterator_Interface_Access is not null access all Iterator_Interface'Class;


   procedure Reset (Iter : in out Iterator_Interface) is abstract;
   procedure Next (Iter : in out Iterator_Interface) is abstract;

   function End_Of_Iteration (Iter : Iterator_Interface)
                              return Boolean is abstract;

   function Element (Iter : Iterator_Interface)
                     return Engine_Value is abstract;

   type Function_Interface is interface;
   type Function_Interface_Access is not null access all Function_Interface'Class;

   function Process (Fun       : Function_Interface;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array is abstract;

   type Callback_Function_Access is
   not null access function (Parameters : Engine_Value_Array) return Engine_Value_Array;


   function Create (Val : Integer) return Engine_Value
     with Post => Create'Result.Class = Int;

   function Create (Val : Float) return Engine_Value
     with Post => Create'Result.Class = Real;

   function Create (Val : String) return Engine_Value
     with Post => Create'Result.Class = Text;

   function Create (Val : Array_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Array_Like;

   function Create (Val : Record_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Record_Like;

   function Create (Val : Iterator_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Iterator;

   function Create (Val : Function_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Function_Handler;

   function Create (Val : Callback_Function_Access) return Engine_Value
     with Post => Create'Result.Class = Function_Handler;



   function Get_Integer (Val : Integer_Value) return Integer;
   function Get_Float (Val : Real_Value) return Float;
   function Get_String (Val : String_Value) return String;
   function Get_Array (Val : Array_Value) return Array_Interface_Access;
   function Get_Record (Val : Record_Value) return Record_Interface_Access;
   function Get_Iterator (Val : Iterator_Value) return Iterator_Interface_Access;
   function Get_Function (Val : Function_Value) return Function_Interface_Access;

private
   type Engine_Value (Class : Engine_Value_Class := Void) is
      record
         case Class is
         when Void =>
            null;

         when Int =>
            Int_Val : Integer;

         when Real =>
            Real_Val : Float;

         when Text =>
            Text_Val : Unbounded_String;

         when Array_Like =>
            Array_Object : Array_Interface_Access;

         when Record_Like =>
            Record_Object : Record_Interface_Access;

         when Iterator =>
            Iteration_Object : Iterator_Interface_Access;

         when Function_Handler =>
            Function_Object : Function_Interface_Access;

         end case;
      end record;

   Void_Value : constant Engine_Value := (Class => Void);
   No_Value   : constant Engine_Value_Array (2 .. 1) := (others => Void_Value);

   type Callback_Based_Handler is
     new Function_Interface
   with
      record
         Callback : Callback_Function_Access;
      end record;

   function Process (Fun       : Callback_Based_Handler;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array
   is (Fun.Callback (Parameter));


   function Create (Val : Integer) return Engine_Value
   is (Engine_Value'(Class            => Int,
                     Int_Val          => Val));


   function Create (Val : Float) return Engine_Value
   is (Engine_Value'(Class            => Real,
                     Real_Val         => Val));

   function Create (Val : String) return Engine_Value
   is (Engine_Value'(Class            => Text,
                     Text_Val         => To_Unbounded_String (Val)));

   function Create (Val : Array_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Array_Like,
                     Array_Object     => Val));

   function Create (Val : Record_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Record_Like,
                     Record_Object    => Val));

   function Create (Val : Iterator_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Iterator,
                     Iteration_Object => Val));

   function Create (Val : Function_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Function_Handler,
                     Function_Object  => Val));

   function Create (Val : Callback_Function_Access) return Engine_Value
   is (Create (new Callback_Based_Handler'(Callback => Val)));

   function Get_Integer (Val : Integer_Value) return Integer
   is (Val.Int_Val);

   function Get_Float (Val : Real_Value) return Float
   is (Val.Real_Val);

   function Get_String (Val : String_Value) return String
   is (To_String (Val.Text_Val));

   function Get_Array (Val : Array_Value) return Array_Interface_Access
   is (Val.Array_Object);

   function Get_Record (Val : Record_Value) return Record_Interface_Access
   is (Val.Record_Object);

   function Get_Iterator (Val : Iterator_Value) return Iterator_Interface_Access
   is (Val.Iteration_Object);

   function Get_Function (Val : Function_Value) return Function_Interface_Access
   is (Val.Function_Object);
end Protypo.API.Engine_Values;
