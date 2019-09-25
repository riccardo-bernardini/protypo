package Protypo.API.Consumers is
   type Consumer_Interface is limited interface;
   type Consumer_Access is not null access all Consumer_Interface'Class;

   procedure Process (Consumer  : in out Consumer_Interface;
                      Parameter : String)
   is abstract;
end Protypo.API.Consumers;
