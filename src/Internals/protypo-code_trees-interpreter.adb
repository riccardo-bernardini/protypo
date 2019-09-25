pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;

package body Protypo.Code_Trees.Interpreter is
   type Consumer_Callback is
     new Api.Engine_Values.Function_Interface
   with
      record
         Consumer : Api.Consumers.Consumer_Access;
      end record;

   overriding function Process (Fun       : Consumer_Callback;
                                Parameter : Engine_Value_Array)
                                return Engine_Value_Array
   is
      function To_String (X : Engine_Value) return String
      is (case X.Class is
             when Int    => Get_Integer (X)'Image,
             when Real   => Get_Float (X)'Image,
             when Text   => Get_String (X),
             when others => raise Constraint_Error);

   begin
      for P of Parameter loop
         Fun.Consumer.Process (To_String (P));
      end loop;

      return No_Value;
   end Process;


   function Run
     (Program      : not null Node_Access;
      Symbol_Table : in out Api.Symbols.Table)
      return Engine_Value
   is
      subtype Executable_Nodes is Non_Terminal
        with
          Static_Predicate => Executable_Nodes in Statement_Classes | Expression;

      Result : Engine_Value;
   begin
      if Program.Class not in Executable_Nodes then
         raise Program_Error;
      end if;

      case Executable_Nodes (Program.Class) is
         when Statement_Sequence =>
            for Nd of Program.Statements loop
               Result := Run (Nd, Symbol_Table);

               if Result.Class /= Void then
                  raise Program_Error;
               end if;
            end loop;

            return Void_Value;

         when Defun =>
            raise Program_Error;

         when Assignment =>
            raise Program_Error;

         when Return_Statement =>
            raise Program_Error;

         when Procedure_Call =>
            raise Program_Error;

         when Exit_Statement =>
            raise Program_Error;

         when If_Block =>
            raise Program_Error;

         when Loop_Block =>
            raise Program_Error;

         when For_Block =>
            raise Program_Error;

         when While_Block =>
            raise Program_Error;

         when Binary_Op =>
            raise Program_Error;

         when Unary_Op =>
            raise Program_Error;

         when Int_Constant =>
            raise Program_Error;

         when Real_Constant =>
            raise Program_Error;

         when Text_Constant =>
            raise Program_Error;

         when Selected =>
            raise Program_Error;

         when Indexed =>
            raise Program_Error;

         when Identifier =>
            raise Program_Error;

      end case;

      return Result;
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
      Consumer     : Api.Consumers.Consumer_Access)
   is
      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table)
      is
      begin
         Table.Create (Name          => "consume",
                       Initial_Value => Create (new Consumer_Callback'(Consumer => Consumer)));
      end Add_Builtin_Values;

      Ignored       : Engine_Value;
      Private_Table : Api.Symbols.Table := Api.Symbols.Copy_Globals (Symbol_Table);
   begin
      Add_Builtin_Values (Private_Table);

      Ignored := Run (Program.Pt, Private_Table);

      if Ignored.Class /= Void  then
         raise Program_Error;
      end if;
   end Run;

end Protypo.Code_Trees.Interpreter;
