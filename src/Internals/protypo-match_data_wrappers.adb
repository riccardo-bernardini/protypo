pragma Ada_2012;
with Ada.Containers;
with Protypo.Api.Engine_Values.Constant_Wrappers;

package body Protypo.Match_Data_Wrappers is

   Matched_Field_Name : constant Id := "matched?";

   function Wrap (Match_Data : Gnat.Regpat.Match_Array;
                  Source     : String)
                  return Handlers.Ambivalent_Interface_Access
   is
      use Gnat.Regpat;

      Wrapper : constant Match_Data_Wrapper_Access :=
                  new Match_Data_Wrapper (Match_Data'Last);
   begin
      if Match_Data (0) = No_Match then
         Wrapper.Matched := False;
         Wrapper.Submatches := (others => Null_Unbounded_String);

      else
         Wrapper.Matched := True;

         for Idx in Match_Data'Range loop
            Wrapper.Submatches (Idx) :=
              To_Unbounded_String
                (Source (Match_Data (Idx).First .. Match_Data (Idx).Last));
         end loop;
      end if;

      return Handlers.Ambivalent_Interface_Access (Wrapper);
   end Wrap;

   function Wrap (Match_Data : Gnat.Regpat.Match_Array;
                  Source     : String)
                  return Engine_Value
   is (Handlers.Create (Wrap (Match_Data, Source)));


   --------------
   -- Is_Field --
   --------------

   function Is_Field (X : Match_Data_Wrapper; Field : Id) return Boolean
   is (Field = Matched_Field_Name);

   ---------
   -- Get --
   ---------

   function Get (X : Match_Data_Wrapper; Field : Id) return Handler_Value is
   begin
      if Field = Matched_Field_Name then
         return Constant_Wrappers.To_Handler_Value (X.Matched);

      else
         raise Run_Time_Error
           with "Unknown field for match data: '" & String (Field) & "'";
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (X : Match_Data_Wrapper; Index : Engine_Value_Vectors.Vector)
      return Handler_Value
   is
      use type Ada.Containers.Count_Type;
   begin
      if Index.Length /= 1 then
         raise Run_Time_Error with "Match data requires 1 index only";
      end if;

      if  Index.First_Element.Class /= Int then
         raise Run_Time_Error with "Match data requires one integer index";
      end if;

      declare
         use Constant_Wrappers;

         Idx : constant Integer := Get_Integer (Index.First_Element);
      begin
         if not (Idx in X.Submatches'Range) then
            raise Run_Time_Error
              with "Match data index "
              & Idx'Image
              & " outside of valid range "
              & X.Submatches'First'Image & ".." & X.Submatches'Last'Image;
         end if;

         return To_Handler_Value (X.Submatches (Idx));
      end;
   end Get;

end Protypo.Match_Data_Wrappers;
