pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers;

package body Protypo.Api.Engine_Values.Handlers is
   function Apply_Default_And_Varargin
     (Specs      : Parameter_Lists.Parameter_Signature;
      Parameters : Engine_Value_Array)
      return Engine_Value_Array
     with Pre =>
       Parameter_Lists.Is_Valid_Parameter_Signature (Specs);

   function Apply_Default_And_Varargin
     (Specs      : Parameter_Lists.Parameter_Signature;
      Parameters : Engine_Value_Array)
      return Engine_Value_Array
   is
      use type Parameter_Lists.Parameter_Spec;

      function Apply_Default (Specs      : Parameter_Lists.Parameter_Signature;
                              Parameters : Engine_Value_Array)
                              return Engine_Value_Array
        with Pre =>
          Parameter_Lists.Is_Valid_Parameter_Signature (Specs)
          and (Specs'Length = 0
               or else Specs (Specs'Last) /= Parameter_Lists.Varargin);

      function Apply_Default (Specs      : Parameter_Lists.Parameter_Signature;
                              Parameters : Engine_Value_Array)
                              return Engine_Value_Array
      is
         Result : Engine_Value_Array;
      begin
         if not Parameter_Lists.Is_Valid_Parameter_Signature (Specs) then
            raise Program_Error with "Bad parameter signature";
         end if;

         if Natural (Parameters.Length) > Specs'Length then
            raise Constraint_Error;
         end if;

         declare
            Pos : Cursor := Parameters.First;
         begin
            for Spec of Specs loop
               if Has_Element (Pos) then
                  Result.Append (Element (Pos));

               elsif Parameter_Lists.Is_Optional (Spec) then
                  Result.Append (Parameter_Lists.Default_Value (Spec));

               else
                  raise Constraint_Error;
               end if;

               Pos := Next (Pos);
            end loop;
         end;

         return Result;
      end Apply_Default;

   begin
      if not Parameter_Lists.Is_Valid_Parameter_Signature (Specs) then
         raise Program_Error with "Bad parameter signature";
      end if;

      if Specs'Length = 0 or else Specs (Specs'Last) /= Parameter_Lists.Varargin then
         return Apply_Default (Specs, Parameters);

      else
         pragma Compile_Time_Warning (False, "Varargin not implemented");
         raise Program_Error with "Varargin not implemented";
      end if;
   end Apply_Default_And_Varargin;

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function (Funct      : Handlers.Function_Interface'Class;
                           Parameters : Engine_Value_Array)
                           return Engine_Value_Array
   is
   begin
      return Funct.Process
        (Apply_Default_And_Varargin (Funct.Signature, Parameters));
   end Call_Function;


   function Create (Val : Array_Interface_Access) return Array_Value
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

   --  function Create (Val : Function_Interface_Access) return Engine_Value
   --  is (Engine_Value'(Class            => Function_Handler,
   --                    Function_Object  => Val));

   function Create (Val          : Callback_Function_Access;
                    N_Parameters : Natural := 1)
                    return Callback_Function_Handler
   is (Create (Val            => Val,
               Min_Parameters => N_Parameters,
               Max_Parameters => N_Parameters,
               With_Varargin  => False));

   function Create (Val            : Callback_Function_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False)
                    return Callback_Function_Handler
   is (Callback_Function_Handler'(Callback => Val,
                                  Min_Parameters => Min_Parameters,
                                  Max_Parameters => Max_Parameters,
                                  With_Varargin  => With_Varargin));

   --  function Create (Val : Procedure_Interface_Access) return Engine_Value
   --  is (Engine_Value'(Class            => Procedure_Handler,
   --                    Procedure_Object  => Val));

   function Create (Val          : Callback_Procedure_Access;
                    N_Parameters : Natural := 1)
                    return Callback_Procedure_Handler
   is (Create (Val            => Val,
               Min_Parameters => N_Parameters,
               Max_Parameters => N_Parameters,
               With_Varargin  => False));

   function Create (Val            : Callback_Procedure_Access;
                    Min_Parameters : Natural;
                    Max_Parameters : Natural;
                    With_Varargin  : Boolean := False)
                    return Callback_Procedure_Handler
   is (Callback_Procedure_Handler'(Callback => Val,
                                   Min_Parameters => Min_Parameters,
                                   Max_Parameters => Max_Parameters,
                                   With_Varargin  => With_Varargin));

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

   --  function Get_Function (Val : Function_Value) return Function_Interface_Access
   --  is (Val.Function_Object);

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
   function Signature (Min_Parameters : Natural;
                       Max_Parameters : Natural;
                       With_Varargin  : Boolean)
                       return Parameter_Signature
   is
      Tot_Parameters : constant Natural :=
                         Max_Parameters
                           + (if With_Varargin then 1 else 0);

      Result         : Parameter_Signature (2 .. Tot_Parameters + 1) :=
                         (others => No_Spec);
      -- Result is initialized to an invalid Parameter_Signature, so that
      -- if there is some bug, it will be (with large probability)
      -- caught by the contract of Signature

      Last_Mandatory : constant Natural := Result'First + Min_Parameters - 1;
      Last_Optional  : constant Natural := Result'First + Max_Parameters - 1;
   begin
      if Min_Parameters > 0 then
         Result (Result'First .. Last_Mandatory) :=
           (others => Mandatory);
      end if;

      if Max_Parameters > Min_Parameters then
         Result (Last_Mandatory + 1 .. Last_Optional) :=
           (others => Optional (Void_Value));
      end if;

      if With_Varargin then
         Result (Result'Last) := Varargin;
      end if;

      return Result;
   end Signature;

   procedure Process (Fun       : Callback_Procedure_Handler;
                      Parameter : Engine_Value_Array)
   is
   begin
      Fun.Callback (Parameter);
   end Process;

   function Signature (Fun : Callback_Procedure_Handler)
                       return Parameter_Signature
   is (Signature (Min_Parameters => Fun.Min_Parameters,
                  Max_Parameters => Fun.Max_Parameters,
                  With_Varargin  => Fun.With_Varargin));

   function Signature (Fun : Callback_Function_Handler)
                       return Parameter_Signature
   is (Signature (Min_Parameters => Fun.Min_Parameters,
                  Max_Parameters => Fun.Max_Parameters,
                  With_Varargin  => Fun.With_Varargin));


end Protypo.Api.Engine_Values.Handlers;
