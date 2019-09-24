package Protypo.API.Engine_Values is
   type Engine_Value_Class is
     (
      Void,
      Int,
      Real,
      Text,
      Array_Like,
      Record_Like,
      Iterator
     );

   type Engine_Value (Class : Engine_Value_Class := Void) is private;
   type Engine_Value_Array is array (Positive range <>) of Engine_Value;

   subtype Void_Value     is Engine_Value (Void);
   subtype Integer_Value  is Engine_Value (Int);
   subtype Real_Value     is Engine_Value (Real);
   subtype String_Value   is Engine_Value (Text);
   subtype Array_Value    is Engine_Value (Array_Like);
   subtype Record_Value   is Engine_Value (Record_Like);
   subtype Iterator_Value is Engine_Value (Iterator);

   type Array_Interface is interface;
   type Array_Interface_Access is access all Array_Interface;

   function Get (X     : Array_Interface;
                 Index : Engine_Value_Array)
                 return Engine_Value
                 is abstract;

   procedure Set (X     : in out Array_Interface;
                  Index : Engine_Value_Array;
                  Value : Engine_Value)
   is abstract;

   type Record_Interface is interface;
   type Record_Interface_Access is access all Record_Interface;

   function Get (X     : Array_Interface;
                 Field : String)
                 return Engine_Value
                 is abstract;

   procedure Set (X     : in out Array_Interface;
                  Field : String;
                  Value : Engine_Value)
   is abstract;

   type Iterator_Interface is interface;
   type Iterator_Interface_Access is access all Iterator_Interface;


   procedure Reset (Iter : in out Iterator_Interface) is abstract;
   procedure Next (Iter : in out Iterator_Interface) is abstract;

   function End_Of_Iteration (Iter : Iterator_Interface)
                              return Boolean is abstract;

   function Element (Iter : Iterator_Interface)
                     return Engine_Value is abstract;
private
   type Engine_Value (Class : Engine_Value_Class := Void) is null record;
end Protypo.API.Engine_Values;
