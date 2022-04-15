with Ada.Strings.Unbounded;

with Gnat.Regpat;

with Protypo.Api.Engine_Values.Handlers;
with Protypo.Api.Engine_Values.Engine_Value_Vectors;

use Protypo.Api.Engine_Values;

package Protypo.Match_Data_Wrappers is
   type Match_Data_Wrapper (<>) is
     new Handlers.Ambivalent_Interface
   with
     private;

   type Match_Data_Wrapper_Access is access Match_Data_Wrapper;

   function Wrap (Match_Data : Gnat.Regpat.Match_Array;
                  Source     : String)
                  return Handlers.Ambivalent_Interface_Access;

   function Wrap (Match_Data : Gnat.Regpat.Match_Array;
                  Source     : String)
                  return Engine_Value;

   function Is_Field (X : Match_Data_Wrapper; Field : Id) return Boolean;

   function Get (X     : Match_Data_Wrapper;
                 Field : Id)
                 return Handler_Value;

   function Get (X     : Match_Data_Wrapper;
                 Index : Engine_Value_Vectors.Vector)
                 return Handler_Value;

private
   use Ada.Strings.Unbounded;

   type Submatches_Array is
     array (Natural range <>) of Unbounded_String;

   type Match_Data_Wrapper (N : Natural)is
     new Api.Engine_Values.Handlers.Ambivalent_Interface
   with
      record
         Matched    : Boolean;
         Submatches : Submatches_Array (0 .. N);
      end record;
end Protypo.Match_Data_Wrappers;
