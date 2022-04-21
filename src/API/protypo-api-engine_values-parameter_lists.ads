with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Containers.Indefinite_Holders;

package Protypo.Api.Engine_Values.Parameter_Lists is
   use type Ada.Containers.Count_Type;


   type Parameter_Spec is private;

   No_Spec : constant Parameter_Spec;

   Mandatory : constant Parameter_Spec;

   Varargin : constant Parameter_Spec;

   function Optional (Default : Engine_Value) return Parameter_Spec;

   function Is_Optional (Item : Parameter_Spec) return Boolean;

   function Default_Value (Item : Parameter_Spec) return Engine_Value
     with
       Pre => Is_Optional (Item);

   type Parameter_Signature is array (Positive range <>) of Parameter_Spec;

   function Image (Spec : Parameter_Spec) return String;

   function Image (Signature : Parameter_Signature) return String;

   function Is_Valid_Parameter_Signature (Signature : Parameter_Signature)
                                          return Boolean;
   --
   -- Return True if Signature is a valid parameter signature that can be returned
   -- by Signature method.  A valid signature satisfies the following "regexp"
   --
   --   Void_Value* Non_Void_Value* Varargin_Value?
   --
   -- that is,
   -- * there is a "head" (potentially empty) of void values that
   -- mark the parameters that are mandatory and have no default;
   --
   -- * a (maybe empty) sequence of non void values follows, these are
   -- default values of optional parameters
   --
   -- * the last entry can be Varargin_Value, showing that the
   -- last parameter is an array (maybe empty) that collects all the
   -- remaining parameters
   --

   type Parameter_List is private;
   -- This is type that can help writing callbacks.  A parameter list
   -- is created using the Engine_Value_Array given to the callback
   -- and it is possible to read its element one at time, using Shift
   -- (that removes the first element too, kind of shift in Ruby, bash, ...)
   -- or Peek (that does not change the list)

   function Create (Params : Engine_Value_Array) return Parameter_List
     with Post => Length (Create'Result) = Params.Length;

   function Length (List : Parameter_List) return Ada.Containers.Count_Type;

   function Is_Empty (List : Parameter_List) return Boolean
   is (Length (List) = 0);

   function Shift (List : in out Parameter_List) return Engine_Value
     with Post => (if Is_Empty (List'Old)
                     then Shift'Result = Void_Value
                       else Length (List) = Length (List'Old)-1);
   -- Return the first parameter and remove it from the list.  If the
   -- list is empty, return Void_Value

   function Peek (List : Parameter_List) return Engine_Value
     with Post => (if Is_Empty (List) then Peek'Result = Void_Value);
   -- Return the first parameter
private
   package Engine_Value_Holders is
     new Ada.Containers.Indefinite_Holders (Engine_Value);

   type Parameter_Class is (Mandatory_Class, Optional_Class, Varargin_Class, Void);

   type Parameter_Spec (Class : Parameter_Class := Mandatory_Class) is
      record
         case Class is
            when Mandatory_Class | Varargin_Class | Void =>
               null;

            when Optional_Class =>
               Default : Engine_Value_Holders.Holder;
         end case;
      end record;

   No_Spec : constant Parameter_Spec := (Class => Void);

   Mandatory : constant Parameter_Spec := (Class => Mandatory_Class);

   function Optional (Default : Engine_Value) return Parameter_Spec
   is (Class => Optional_Class, Default => Engine_Value_Holders.To_Holder (Default));

   Varargin : constant Parameter_Spec := (Class => Varargin_Class);

   package Parameter_Linked_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Engine_Value);

   type Parameter_List is
      record
         L : Parameter_Linked_Lists.List;
      end record;

   function Length (List : Parameter_List) return Ada.Containers.Count_Type
   is (List.L.Length);

   ----------
   -- Peek --
   ----------

   function Peek (List : Parameter_List) return Engine_Value
   is (if Is_Empty (List)
       then
          Void_Value
       else
          List.L.First_Element);


end Protypo.Api.Engine_Values.Parameter_Lists;
