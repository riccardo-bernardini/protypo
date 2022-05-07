package Multi_Test.Tty_Colors is
   type Tty_Color is
     (
      Black,
      Red,
      Green,
      Yellow,
      Blue,
      Magenta,
      Cyan,
      White
     );

   function Color_Text (Text : String; Color : Tty_Color) return String;


end Multi_Test.Tty_Colors;
