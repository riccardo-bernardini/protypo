
with Protypo.Api.Engine_Values.Parameter_Lists;
--  use Protypo.Api.Engine_Values.Parameter_Lists;


package Protypo.Api.Engine_Values.Handlers is
   type Array_Interface is interface;
   type Array_Interface_Access is access all Array_Interface'Class;

   function Get (X     : Array_Interface;
                 Index : Engine_Value_Array)
                 return Engine_Reference'Class
                 is abstract;

   function Create (Val : Array_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Array_Handler;


   Out_Of_Range : exception;

   type Record_Interface is interface;
   type Record_Interface_Access is access all Record_Interface'Class;

   function Is_Field (X : Record_Interface; Field : Id) return Boolean
                      is abstract;

   function Get (X     : Record_Interface;
                 Field : Id)
                 return Engine_Reference'Class
                 is abstract;

   Unknown_Field : exception;

   function Create (Val : Record_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Record_Handler;


   type Ambivalent_Interface is interface
     and Record_Interface
     and Array_Interface;

   type Ambivalent_Interface_Access is  access all Ambivalent_Interface'Class;

   function Create (Val : Ambivalent_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Ambivalent_Handler;


   type Constant_Interface is interface;
   type Constant_Interface_Access  is  access all Constant_Interface'Class;

   function Read (X : Constant_Interface) return Engine_Value is abstract;


   function Create (Val : Constant_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Constant_Handler;

   --  type Reference_Interface is interface and Constant_Interface;
   --  type Reference_Interface_Access is access all Reference_Interface'Class;
   --
   --  procedure Write (What  : Reference_Interface;
   --                   Value : Engine_Value)
   --  is abstract;
   --
   --  function Create (Val : Reference_Interface_Access) return Engine_Value
   --    with Post => Create'Result.Class = Reference_Handler;


   type Iterator_Interface is limited interface;
   type Iterator_Interface_Access is access all Iterator_Interface'Class;


   procedure Reset (Iter : in out Iterator_Interface) is abstract;
   procedure Next (Iter : in out Iterator_Interface) is abstract
     with Pre'Class => not Iter.End_Of_Iteration;

   function End_Of_Iteration (Iter : Iterator_Interface)
                              return Boolean is abstract;

   function Element (Iter : Iterator_Interface)
                     return Handler_Value is abstract
     with Pre'Class => not Iter.End_Of_Iteration;

   function Create (Val : Iterator_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Iterator;


   type Function_Interface is interface;
   type Function_Interface_Access is access all Function_Interface'Class;

   function Process (Fun       : Function_Interface;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array is abstract;

   function Signature (Fun : Function_Interface)
                       return Parameter_Signature is abstract
     with Post'Class => Parameter_Lists.Is_Valid_Parameter_Signature (Signature'Result);


   function Create (Val : Function_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Function_Handler;


   type Callback_Function_Access is
   not null access function (Parameters : Engine_Value_Array)
   return Engine_Value_Array;

   function Create (Val            : Callback_Function_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False) return Engine_Value
     with
       Pre => Max_Parameters >= Min_Parameters,
       Post => Create'Result.Class = Function_Handler;

   function Create (Val          : Callback_Function_Access;
                    N_Parameters : Natural := 1) return Engine_Value
     with Post => Create'Result.Class = Function_Handler;

   type Procedure_Interface is interface;
   type Procedure_Interface_Access is access all Procedure_Interface'Class;

   procedure Process (Fun       : Procedure_Interface;
                      Parameter : Engine_Value_Array) is abstract;

   function Signature (Fun : Procedure_Interface)
                       return Parameter_Signature is abstract
     with Post'Class =>
       Parameter_Lists.Is_Valid_Parameter_Signature (Signature'Result);


   type Callback_Procedure_Access is
   not null access procedure (Parameters : Engine_Value_Array);

   function Create (Val            : Callback_Procedure_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False) return Engine_Value
     with
       Pre => Max_Parameters >= Min_Parameters,
       Post => Create'Result.Class = Procedure_Handler;

   function Create (Val          : Callback_Procedure_Access;
                    N_Parameters : Natural := 1) return Engine_Value
     with Post => Create'Result.Class = Procedure_Handler;


   function Create (Val : Procedure_Interface_Access) return Engine_Value
     with Post => Create'Result.Class = Procedure_Handler;


   function Get_Array (Val : Array_Value) return Array_Interface_Access;
   function Get_Record (Val : Record_Value) return Record_Interface_Access;
   function Get_Ambivalent (Val : Ambivalent_Value) return Ambivalent_Interface_Access;
   function Get_Iterator (Val : Iterator_Value) return Iterator_Interface_Access;
   function Get_Function (Val : Function_Value) return Function_Interface_Access;
   --  function Get_Reference (Val : Reference_Value) return Reference_Interface_Access;
   function Get_Constant (Val : Constant_Value) return Constant_Interface_Access;

   function Force_Handler (Item : Engine_Value) return Handler_Value
     with Pre => Item.Class /= Void and Item.Class /= Iterator;
   --  Take any Engine_Value (but Void and Iterator) and embed it
   --  (if necessary) into a Constant handler value.  If Item is
   --  already a handler, just return Item.

private
   type Callback_Function_Handler is
     new Function_Interface
   with
      record
         Callback       : Callback_Function_Access;
         Min_Parameters : Natural;
         Max_Parameters : Natural;
         With_Varargin  : Boolean;
      end record;


   function Process (Fun       : Callback_Function_Handler;
                     Parameter : Engine_Value_Array)
                     return Engine_Value_Array
   is (Fun.Callback (Parameter));

   function Signature (Fun : Callback_Function_Handler)
                       return Parameter_Signature;
   --     is (Engine_Value_Array(2..Fun.N_Parameters+1)'(others => Void_Value));

   type Callback_Procedure_Handler is
     new Procedure_Interface
   with
      record
         Callback       : Callback_Procedure_Access;
         Min_Parameters : Natural;
         Max_Parameters : Natural;
         With_Varargin  : Boolean;
      end record;


   procedure Process (Fun       : Callback_Procedure_Handler;
                     Parameter : Engine_Value_Array);

   function Signature (Fun : Callback_Procedure_Handler)
                       return Parameter_Signature;


end Protypo.Api.Engine_Values.Handlers;
