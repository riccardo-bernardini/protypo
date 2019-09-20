with Symbol_Tables.Generic_Symbol_Table;

procedure Symbol_Tables.Test is
   package St is
     new Generic_Symbol_Table (Symbol_Name      => ID_Type,
                               Symbol_Value     => Integer,
                               Hash             => Hash_Id,
                               Equivalent_Names => Equivalent_Id);
begin
   null;
end Symbol_Tables.Test;
