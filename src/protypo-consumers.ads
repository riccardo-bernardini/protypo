package Protypo.Consumers is
   type Consumer_Interface is limited interface;

   procedure Process (Consumer  : in out Consumer_Interface;
                      Parameter : String)
   is abstract;
end Protypo.Consumers;
