pragma Ada_2012;
with Ada.Characters.Latin_1;
package body Multi_Test.Tty_Colors is

   ----------------
   -- Color_Text --
   ----------------

   function Color_Text (Text : String; Color : Tty_Color) return String is
      Csi : constant String := (1 => Ada.Characters.Latin_1.Esc,
                                2 => '[');

      subtype Color_String is String (1 .. 2);

      Code : constant array (Tty_Color) of Color_String :=
               (
                Black   => "30",
                Red     => "31",
                Green   => "32",
                Yellow  => "33",
                Blue    => "34",
                Magenta => "35",
                Cyan    => "36",
                White   => "37"
               );

      Default_Color : constant Color_String := "39";
   begin
      return
        Csi & Code (Color) & "m"
        & Text
        & Csi & Default_Color & "m";
   end Color_Text;

end Multi_Test.Tty_Colors;
