pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers;

package body Protypo.Api.Engine_Values.Handlers is

   function Create (Val : Array_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Array_Handler,
                     Array_Object     => Val));

   function Create (Val : Record_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Record_Handler,
                     Record_Object    => Val));

   function Create (Val : Ambivalent_Interface_Access) return Engine_Value
   is (Engine_Value'(Class             => Ambivalent_Handler,
                     Ambivalent_Object => Val));


   function Create (Val : Iterator_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Iterator,
                     Iteration_Object => Val));

   function Create (Val : Function_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Function_Handler,
                     Function_Object  => Val));

   function Create (Val          : Callback_Function_Access;
                    N_Parameters : Natural := 1)
                    return Engine_Value
   is (Create (new Callback_Based_Handler'(Callback => Val,
                                           Min_Parameters => N_Parameters,
                                           Max_Parameters => N_Parameters,
                                           With_Varargin  => False)));

   function Create (Val            : Callback_Function_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False) return Engine_Value
   is (Create (new Callback_Based_Handler'(Callback => Val,
                                           Min_Parameters => Min_Parameters,
                                           Max_Parameters => Max_Parameters,
                                           With_Varargin  => With_Varargin)));

   --  function Create (Val : Reference_Interface_Access) return Engine_Value
   --  is (Engine_Value'(Class            => Reference_Handler,
   --                    Reference_Object => Val));

   function Create (Val : Constant_Interface_Access) return Engine_Value
   is (Engine_Value'(Class            => Constant_Handler,
                     Constant_Object  => Val));

   function Get_Array (Val : Array_Value) return Array_Interface_Access
   is (Val.Array_Object);

   function Get_Record (Val : Record_Value) return Record_Interface_Access
   is (Val.Record_Object);

   function Get_Ambivalent (Val : Ambivalent_Value) return Ambivalent_Interface_Access
   is (Val.Ambivalent_Object);

   function Get_Iterator (Val : Iterator_Value) return Iterator_Interface_Access
   is (Val.Iteration_Object);

   function Get_Function (Val : Function_Value) return Function_Interface_Access
   is (Val.Function_Object);

   --  function Get_Reference (Val : Reference_Value) return Reference_Interface_Access
   --  is (Val.Reference_Object);

   function Get_Constant (Val : Constant_Value) return Constant_Interface_Access
   is (Val.Constant_Object);

   function Force_Handler (Item : Engine_Value) return Handler_Value
   is (case Item.Class is
          when Handler_Classes      =>
             Item,

          when Int                  =>
             Constant_Wrappers.To_Handler_Value (Get_Integer (Item)),

          when Real                 =>
             Constant_Wrappers.To_Handler_Value (Get_Float (Item)),

          when Text                 =>
             Constant_Wrappers.To_Handler_Value (Get_String (Item)),

          when Void | Iterator      =>
             raise Constraint_Error);

   ---------------
   function Signature (Fun : Callback_Based_Handler)
                       return Parameter_Signature
   is
      Tot_Parameters : constant Natural :=
                         Fun.Max_Parameters
                           + (if Fun.With_Varargin then 1 else 0);

      Result         : Parameter_Signature (2 .. Tot_Parameters + 1) :=
                         (others => No_Spec);
      -- Result is initialized to an invalid Parameter_Signature, so that
      -- if there is some bug, it will be (with large probability)
      -- caught by the contract of Signature

      Last_Mandatory : constant Natural := Result'First + Fun.Min_Parameters - 1;
      Last_Optional  : constant Natural := Result'First + Fun.Max_Parameters - 1;
   begin
      if Fun.Min_Parameters > 0 then
         Result (Result'First .. Last_Mandatory) :=
           (others => Mandatory);
      end if;

      if Fun.Max_Parameters > Fun.Min_Parameters then
         Result (Last_Mandatory + 1 .. Last_Optional) :=
           (others => Optional (Void_Value));
      end if;

      if Fun.With_Varargin then
         Result (Result'Last) := Varargin;
      end if;

      return Result;
   end Signature;


end Protypo.Api.Engine_Values.Handlers;
