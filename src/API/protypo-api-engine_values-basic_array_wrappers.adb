pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers;

with Protypo.Api.Engine_Values.Range_Iterators;
with Protypo.Api.Field_Names;

pragma Warnings(Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Api.Engine_Values.Basic_Array_Wrappers is
   -- Type that represents the name of the fields that we export
   -- every name is preceded by "Field_" in order to avoid clashes
   -- with keywords (e.g., 'range')
   type Field_Name is
     (
      Field_First,
      Field_Last,
      Field_Range,
      Field_Iterate,
      Field_Length
     );

   package Field_Names_Package is
     new Field_Names (Field_Enumerator => Field_Name,
                      Prefix           => "Field_");

   function To_Field (X : ID) return Field_Name
   is (Field_Names_Package.To_Field (X));

--     function To_Field (X : String) return Field_Name
--     is (Field_Name'Value ("Field_" & X));


   type Array_Iterator is
     new Iterator_Interface
   with
      record
         Cursor    : Array_Wrapper_Index;
         Container : Vector_Access;
      end record;


   overriding procedure Reset (Iter : in out Array_Iterator);
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean;
   overriding function Element (Iter : Array_Iterator) return Handler_Value;

   overriding procedure Reset (Iter : in out Array_Iterator)
   is
   begin
      Iter.Cursor := Iter.Container.First_Index;
   end Reset;

   overriding procedure Next (Iter : in out Array_Iterator)
   is
   begin
      if not Iter.End_Of_Iteration then
         Iter.Cursor := Iter.Cursor + 1;
      end if;
   end Next;

   function Force_Handler (Item : Engine_Value) return Handler_Value
   is (case Item.Class is
          when Handler_Classes =>
             Item,

          when Int             =>
             Constant_Wrappers.To_Handler_Value(Get_Integer(Item)),

          when Real             =>
             Constant_Wrappers.To_Handler_Value (Get_Float (Item)),

          when Text             =>
             Constant_Wrappers.To_Handler_Value (Get_String (Item)),

          when Void | Iterator   =>
             raise Constraint_Error);


   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean
   is (Iter.Cursor > Iter.Container.Last_Index);

   overriding function Element (Iter : Array_Iterator) return Handler_Value
   is (Force_Handler (Iter.Container.all (Iter.Cursor)));

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper
     (Init : Engine_Value_Array := No_Value) return Array_Wrapper_Access
   is
      V : constant Vector_Access :=
            new Engine_Value_Vectors.Vector'(Engine_Value_Vectors.Empty_Vector);

      Result : constant Array_Wrapper_Access :=
                 new Array_Wrapper'(Vector => V);
   begin
      for El of Init loop
         Result.Vector.Append (El);
      end loop;

      return Result;
   end Make_Wrapper;

   ---------
   -- Set --
   ---------

   procedure Set
     (Container : in out Array_Wrapper;
      Index     : Array_Wrapper_Index;
      Value     :        Engine_Value)
   is
   begin
      Container.Vector.Insert (Index, Force_Handler (Value));
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Array_Wrapper;
                     Item      : Engine_Value)
   is
   begin
      Container.Vector.Append (Item);
   end Append;

   ---------
   -- Get --
   ---------

   function Get
     (X : Array_Wrapper; Index : Engine_Value_Array) return Handler_Value
   is
   begin
      if Index'Length /= 1 then
         raise Out_Of_Range with "Array access with /= 1 index";
      end if;

      if Index (Index'First).Class /= Int then
         raise Out_Of_Range with "Array access with non-integer index";
      end if;

      declare
         Idx : constant Integer := Get_Integer (Index (Index'First));
      begin
         if Idx < X.Vector.First_Index or Idx > X.Vector.Last_Index then
            raise Out_Of_Range with "Out of bounds index";
         end if;

         return Constant_Wrappers.To_Handler_Value (X.Vector.all (Idx));
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X : Array_Wrapper; Field : ID) return Handler_Value is
      use Constant_Wrappers;
   begin
      case To_Field (Field) is
         when Field_First =>
            return To_Handler_Value (X.Vector.First_Index);

         when Field_Last =>
            return To_Handler_Value (X.Vector.Last_Index);

         when Field_Length =>
            return To_Handler_Value (Integer (X.Vector.Length));

         when Field_Iterate =>
            return To_Handler_Value
              (Create
                 (Iterator_Interface_Access'
                      (new Array_Iterator'(Cursor    => X.Vector.First_Index,
                                           Container => X.Vector))));

         when Field_Range =>
            return To_Handler_Value
              (Create
                 (Range_Iterators.Create (Start     => X.Vector.First_Index,
                                          Stop      => X.Vector.Last_Index)));
      end case;
   end Get;

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
   is (Field_Names_Package.Is_Field (Field));


--     --------------
--     -- Is_Field --
--     --------------
--
--     function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
--     is
--        pragma Unreferenced (X);
--        Ignored : Field_Name;
--     begin
--        Ignored := To_Field (Field);
--        return True;
--        -- Yes, I know, it is not the best practice to use exceptions
--        -- to do flow control, but this is the easiest way
--     exception
--        when Constraint_Error =>
--           return False;
--     end Is_Field;
--  --        return Equal_Case_Insensitive (Field, "first")
--          or Equal_Case_Insensitive (Field, "last")
--          or Equal_Case_Insensitive (Field, "length")
--          or Equal_Case_Insensitive (Field, "iterate")
--          or Equal_Case_Insensitive (Field, "range");


end Protypo.Api.Engine_Values.Basic_Array_Wrappers;
