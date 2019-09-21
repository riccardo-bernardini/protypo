with Protypo.Scanning;
procedure Main is
   use Protypo.Scanning;
begin
   Dump (Tokenize ("\wpitem{#WP.index}{#WP.start}{#WP.end}"));
end Main;
