pragma Ada_2012;
package body Protypo.Api.Callback_Utilities is

   ---------------------
   -- Match_Signature --
   ---------------------

   function Match_Signature (Parameters : Engine_Value_Vectors.Vector;
                             Signature  : Class_Array)
                             return Boolean
   is
      use Engine_Value_Vectors;

   begin
      if Natural (Parameters.Length) /= Signature'Length then
         return False;
      end if;

      declare
         Pos : Cursor := Parameters.First;
      begin
         for Class of Signature loop
            if Class /= Element (Pos).Class then
               return False;
            end if;

            Next (Pos);
         end loop;
      end;

      return True;
   end Match_Signature;


   function Get (Parameters : Engine_Value_Vectors.Vector;
                 Index      : Positive)
                 return Engine_Value
   is (Parameters (Parameters.First_Index + Index - 1));

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Parameters : Engine_Value_Vectors.Vector; Index : Positive;
      Class      : Engine_Value_Class) return Boolean
   is (Get (Parameters, Index).Class = Class);

   -------------------
   -- Get_Parameter --
   -------------------

   function Get_Parameter (Parameters : Engine_Value_Vectors.Vector;
                           Index      : Positive)
                          return String
   is (Get_String (Get (Parameters, Index)));

end Protypo.Api.Callback_Utilities;
