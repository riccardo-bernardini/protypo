package body Protypo is
   function Is_Valid_Id (Id : String) return Boolean
   is
      use Strings.Maps;
   begin
      if Id = "" then
         return False;
      end if;

      if not Is_In (Id (Id'First), Begin_Id_Set) then
         return False;
      end if;

      for I in Id'First + 1 .. Id'Last - 1 loop
         if not Is_In (Id (I), Id_Charset) then
            return False;
         end if;
      end loop;

      if not Is_In (Id (Id'Last), End_Id_Set) then
         return False;
      end if;

      return True;
   end Is_Valid_Id;
end Protypo;
