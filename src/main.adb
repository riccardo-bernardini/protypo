with Protypo.Scanning;
with Utilities;

procedure Main is
   use Protypo.Scanning;
begin
   --     Dump (Tokenize ("\wpitem{#WP.index}{#WP.start}{#WP.end}"));
   Dump (Tokenize (Utilities.Slurp ("test-data/scanner.txt")));
end Main;
