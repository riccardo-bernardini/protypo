with Utilities;

procedure Protypo.Scanning.Test is
begin
   --     Dump (Tokenize ("\wpitem{#WP.index}{#WP.start}{#WP.end}"));
   Dump (Tokenize (Utilities.Slurp ("test-data/scanner.txt"), ""));
end Protypo.Scanning.Test;
