with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package User_Lists.User_Db is
   type Db_Entry is
      record
         First_Name  : Unbounded_String;
         Last_Name   : Unbounded_String;
         Institution : Unbounded_String;
      end record;
end User_Lists.User_Db;
