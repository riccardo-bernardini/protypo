with Protypo.Symbols;
with Protypo.Consumers;

package Protypo.Code_Trees.Interpreter is
   procedure Run (Program      : Parsed_Code;
                  Symbol_Table : Symbols.Table;
                  Consumer     : in out Consumers.Consumer_Interface'Class);
end Protypo.Code_Trees.Interpreter;
