with Protypo.Api.Interpreters;

procedure Protypo.Scanning.Test is
begin
   --     Dump (Tokenize ("\wpitem{#WP.index}{#WP.start}{#WP.end}"));
   Dump (Tokenize (Api.Interpreters.Slurp ("test-data/scanner.txt"), ""));
end Protypo.Scanning.Test;
