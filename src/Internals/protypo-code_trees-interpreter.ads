with Protypo.Api.Symbols;
with Protypo.Api.Consumers;

package Protypo.Code_Trees.Interpreter is
   procedure Run (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
                  Consumer     : Api.Consumers.Consumer_Access);
end Protypo.Code_Trees.Interpreter;
