with Protypo.Api.Symbols;
with Protypo.Api.Consumers;

package Protypo.Code_Trees.Interpreter is
   procedure Run (Program      : Parsed_Code;
                  Symbol_Table : Api.Symbols.Table;
                  Consumer     : in out Api.Consumers.Consumer_Interface'Class);
end Protypo.Code_Trees.Interpreter;
