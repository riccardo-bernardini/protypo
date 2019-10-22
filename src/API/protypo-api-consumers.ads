--
-- ## What is a consumer?
-- A consumer is an object that implement the `Consumer_Interface`.
-- It is expected to accept a single string as argument and it can do
-- whatever it wants with it.  A consumer receives its parameter from
-- the builtin function `@` after the `#...#` processing has taken
-- place.
--
-- In most cases it is expected that the consumer will be an
-- instance of `File_Writer`
--
package Protypo.API.Consumers is
   type Consumer_Interface is limited interface;
   type Consumer_Access is not null access all Consumer_Interface'Class;

   procedure Process (Consumer  : in out Consumer_Interface;
                      Parameter : String)
   is abstract;
end Protypo.API.Consumers;
