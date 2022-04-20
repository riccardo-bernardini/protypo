pragma Ada_2012;
with Ada.Containers;

with Protypo.Api.Engine_Values.Range_Iterators;
with Protypo.Api.Field_Names;

with Protypo.Api.References.Constant_References;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;

package body Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers is
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

   function To_Field (X : Id) return Field_Name
   is (Field_Names_Package.To_Field (X));

   --     function To_Field (X : String) return Field_Name
   --     is (Field_Name'Value ("Field_" & X));


   type Array_Iterator is
     new Handlers.Iterator_Interface
   with
      record
         Cursor : Engine_Value_Vectors.Cursor;
         First  : Engine_Value_Vectors.Cursor;
         --  Container : Vector_Access;
      end record;

   --
   --  function Force_Handler (Item : Engine_Value) return Handler_Value
   --  is (case Item.Class is
   --         when Handler_Classes   =>
   --            Item,
   --
   --         when Int               =>
   --            Constant_Wrappers.To_Handler_Value (Get_Integer (Item)),
   --
   --         when Real              =>
   --            Constant_Wrappers.To_Handler_Value (Get_Float (Item)),
   --
   --         when Text              =>
   --            Constant_Wrappers.To_Handler_Value (Get_String (Item)),
   --
   --         when logical              =>
   --            Constant_Wrappers.To_Handler_Value (Get_Logical (Item)),
   --
   --         when Void | Iterator   =>
   --            raise Constraint_Error);

   overriding procedure Reset (Iter : in out Array_Iterator);
   overriding procedure Next (Iter : in out Array_Iterator);
   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean;
   overriding function Element (Iter : Array_Iterator) return Handler_Value;

   overriding procedure Reset (Iter : in out Array_Iterator)
   is
   begin
      Iter.Cursor := Iter.First;
   end Reset;

   overriding procedure Next (Iter : in out Array_Iterator)
   is
   begin
      Engine_Value_Vectors.Next (Iter.Cursor);
   end Next;


   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean
   is (not Engine_Value_Vectors.Has_Element (Iter.Cursor));

   overriding function Element (Iter : Array_Iterator) return Handler_Value
   is (Handlers.Force_Handler (Engine_Value_Vectors.Element (Iter.Cursor)));

   ------------------
   -- Make_Wrapper --
   ------------------

   function Make_Wrapper
     (Init : Engine_Value_Vectors.Vector := Engine_Value_Vectors.Empty_Vector)
      return Array_Wrapper_Access
   is
      --  V : constant Vector_Access :=
      --        new Engine_Value_Vectors.Vector'(Engine_Value_Vectors.Empty_Vector);

      Result : constant Array_Wrapper_Access :=
                 new Array_Wrapper'(Vector => Engine_Value_Vectors.Empty_Vector);
   begin
      for El of Init loop
         Result.Vector.Append (El);
      end loop;

      return  Result;
   end Make_Wrapper;

   function Make_Wrapper
     (Init : Engine_Value_Vectors.Vector := Engine_Value_Vectors.Empty_Vector)
      return Handlers.Ambivalent_Interface_Access
   is
      Tmp : constant Array_Wrapper_Access := Make_Wrapper (Init);
   begin
      return Handlers.Ambivalent_Interface_Access (Tmp);
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
      Container.Vector.Insert (Index, Value);
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
     (X     : Array_Wrapper;
      Index : Engine_Value_Vectors.Vector)
      return References.Reference'Class
   is
      use References;
      use type Ada.Containers.Count_Type;
   begin
      if Index.Length /= 1 then
         raise Handlers.Out_Of_Range with "Array access with /= 1 index";
      end if;

      if Index.First_Element.Class /= Int then
         raise Handlers.Out_Of_Range with "Array access with non-integer index";
      end if;

      declare
         Idx : constant Integer := Get_Integer (Index.First_Element);
      begin
         if Idx < X.Vector.First_Index or Idx > X.Vector.Last_Index then
            raise Handlers.Out_Of_Range with "Out of bounds index";
         end if;

         return Constant_References.To_Reference (X.Vector (Idx));
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (X     : Array_Wrapper;
                 Field : Id)
                 return References.Reference'Class is
      use Protypo.Api.References.Constant_References;
   begin
      case To_Field (Field) is
         when Field_First =>
            return To_Reference (Create (X.Vector.First_Index));

         when Field_Last =>
            return To_Reference (Create (X.Vector.Last_Index));

         when Field_Length =>
            return To_Reference (Create (Integer (X.Vector.Length)));

         when Field_Iterate =>
            return To_Reference
              (Handlers.Create
                 (Handlers.Iterator_Interface_Access'
                      (new Array_Iterator'(Cursor    => X.Vector.First,
                                           First     => X.Vector.First))));

         when Field_Range =>
            return To_Reference
              (Handlers.Create
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


end Protypo.Api.Engine_Values.Engine_Value_Array_Wrappers;
