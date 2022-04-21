with Protypo.Api.Engine_Values;

--
--  This package defines a "constant reference," that is a reference that
--  wraps around a constant engine value
--
package Protypo.Api.Constant_References is
   type Constant_Reference (<>) is
     new Engine_Values.Reference
       with
     private;

   function To_Reference (Value : Engine_Values.Engine_Value)
                          return Constant_Reference;

   function Read (Ref : Constant_Reference)
                  return Engine_Values.Engine_Value;

private
   type Constant_Reference (Class : Engine_Values.Engine_Value_Class) is
     new Engine_Values.Reference
   with
      record
         Value : Engine_Values.Engine_Value (Class);
      end record;
end Protypo.Api.Constant_References;
