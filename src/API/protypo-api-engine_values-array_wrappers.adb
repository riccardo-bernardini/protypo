pragma Ada_2012;
with Protypo.Api.Engine_Values.Constant_Wrappers;
with Ada.Strings.Equal_Case_Insensitive;

with Protypo.Api.Engine_Values.Range_Iterators;

package body Protypo.Api.Engine_Values.Array_Wrappers is

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

   overriding function End_Of_Iteration (Iter : Array_Iterator) return Boolean
   is (Iter.Cursor > Iter.Container.Last_Index);

   overriding function Element (Iter : Array_Iterator) return Handler_Value
   is (Iter.Container.all (Iter.Cursor));

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
      Container.Vector.all (Index) := Value;
   end Set;

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
      use Ada.Strings;
      use Constant_Wrappers;
   begin
      if Equal_Case_Insensitive (Field, "first") then
         return To_Handler_Value (X.Vector.First_Index);

      elsif Equal_Case_Insensitive (Field, "last") then
         return To_Handler_Value (X.Vector.Last_Index);

      elsif Equal_Case_Insensitive (Field, "iterate") then
         return To_Handler_Value
           (Create
              (Iterator_Interface_Access'
                   (new Array_Iterator'(Cursor    => X.Vector.First_Index,
                                        Container => X.Vector))));

      elsif Equal_Case_Insensitive (Field, "range") then
         return To_Handler_Value
           (Create
              (Range_Iterators.Create (Start     => X.Vector.First_Index,
                                       Stop      => X.Vector.Last_Index)));
      else
         raise Unknown_Field with Field;
      end if;
   end Get;

   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Array_Wrapper; Field : Id) return Boolean
   is
      pragma Unreferenced (X);
      use Ada.Strings;
   begin
      return Equal_Case_Insensitive (Field, "first")
        or Equal_Case_Insensitive (Field, "last")
        or Equal_Case_Insensitive (Field, "iterate")
        or Equal_Case_Insensitive (Field, "range");
   end Is_Field;


end Protypo.Api.Engine_Values.Array_Wrappers;
