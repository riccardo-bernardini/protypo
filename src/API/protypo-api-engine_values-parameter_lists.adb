pragma Ada_2012;
package body Protypo.Api.Engine_Values.Parameter_Lists is

   function Is_Optional (Item : Parameter_Spec) return Boolean
   is (Item.Class = Optional_Class);

   function Default_Value (Item : Parameter_Spec) return Engine_Value
   is (if Is_Optional (Item) then
          Item.Default.Element
       else
          raise Constraint_Error);

   ------------
   -- Create --
   ------------

   function Create (Params : Engine_Value_Vectors.Vector) return Parameter_List
   is
      Result : Parameter_List;
   begin
      for Element of Params loop
         Result.L.Append (Element);
      end loop;

      return Result;
   end Create;


   -----------
   -- Shift --
   -----------

   function Shift (List : in out Parameter_List) return Engine_Value
   is
      Result : constant Engine_Value := Peek (List);
   begin
      List.L.Delete_First;
      return Result;
   end Shift;

   function Image (Spec : Parameter_Spec) return String
   is (case Spec.Class is
          when Mandatory_Class =>
             "Mandatory",

          when Optional_Class  =>
             "Optional",

          when Varargin_Class  =>
             "Varargin",

          when Void            =>
             "Void");

   function Image (Signature : Parameter_Signature) return String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("(");

      for Idx in Signature'Range loop
         Result := Result & Image (Signature (Idx));

         if Idx < Signature'Last then
            Result := Result & ", ";
         end if;
      end loop;

      return To_String (Result & ")");
   end Image;


   ----------------------------------
   -- Is_Valid_Parameter_Signature --
   ----------------------------------

   function Is_Valid_Parameter_Signature (Signature : Parameter_Signature)
                                          return Boolean
   is
      --
      -- We check this with the following finite automata
      --                non-void
      --   Mandatory -------------->  Optional
      --    ||  ^                       ||  ^
      --    |+--| Void                  |+--| Non-void
      --    |                           |
      --    |  Varargin                 | Varargin
      --    +-----------> Varargin <----+
      --
      -- Note that the next status is determined by the current
      -- value class and that Varargin cannot go anywhere
      --
      Old : Parameter_Class := Mandatory_Class;
   begin
      --
      -- A signature is valid if and only if
      --
      -- * Any Mandatory is preceded by another Mandatory
      -- * Nothing follows Varargin
      --
      for Param  of Signature loop
         if Old = Varargin_Class then
            return False;
         end if;

         if Param.Class = Mandatory_Class and Old /= Mandatory_Class then
            return False;
         end if;

         Old := Param.Class;
      end loop;

      return True;
   end Is_Valid_Parameter_Signature;

end Protypo.Api.Engine_Values.Parameter_Lists;
