pragma Ada_2012;
with Protypo.Api.Engine_Values;  use Protypo.Api.Engine_Values;
with Protypo.Code_Trees.Interpreter.Consumer_Handlers;
with Protypo.Code_Trees.Interpreter.Compiled_Functions;

package body Protypo.Code_Trees.Interpreter is


   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (X : Engine_Value_Array) return Engine_Value_Vectors.Vector
   is
      Result : Engine_Value_Vectors.Vector;
   begin
      for Element of X loop
         Result.Append (Element);
      end loop;

      return Result;
   end To_Vector;
   pragma Unreferenced (To_Vector);

   --------------
   -- To_Array --
   --------------

   function To_Array (X : Engine_Value_Vectors.Vector) return Engine_Value_Array
   is
      Result : Engine_Value_Array (X.First_Index .. X.Last_Index);
   begin
      for K in Result'Range loop
         Result (K) := X (K);
      end loop;

      return Result;
   end To_Array;




   function Run
     (Program      : Node_Vectors.Vector;
      Symbol_Table : Symbol_Table_Access)
      return Interpreter_Result
   is

   begin
      for Statement of Program loop
         declare
            Result : constant Interpreter_Result := Run (Statement, Symbol_Table);
         begin
            if Result.Reason /= End_Of_Code then
               return Result;
            end if;
         end;
      end loop;

      return No_Result;
   end Run;


--     type Compiled_Function is
--       new Api.Engine_Values.Function_Interface
--     with
--        record
--           Function_Body : Node_Vectors.Vector;
--           Parameters    : Parameter_Specs;
--           Symbol_Table  : Symbol_Table_Access;
--        end record;




   function Run
     (Program      : not null Node_Access;
      Symbol_Table : Symbol_Table_Access)
      return Interpreter_Result
   is
      subtype Executable_Nodes is Non_Terminal
        with
          Static_Predicate => Executable_Nodes in Statement_Classes | Code_Trees.Expression;

      use Compiled_Functions;
   begin
      if Program.Class not in Executable_Nodes then
         raise Program_Error;
      end if;


      case Executable_Nodes (Program.Class) is
         when Statement_Sequence =>
            return Run (Program.Statements, Symbol_Table);

         when Defun =>
            Symbol_Table.Create
              (Name          =>
                  To_String (Program.Definition_Name),
               Initial_Value =>
                  Create (new Compiled_Function'(Function_Body => Program.Function_Body,
                                                 Parameters    => Program.Parameters,
                                                 Symbol_Table  => Symbol_Table)));

               return No_Result;

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

   end Run;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program      : Parsed_Code;
      Symbol_Table : Api.Symbols.Table;
      Consumer     : Api.Consumers.Consumer_Access)
   is
      use Api.Symbols;

      procedure Add_Builtin_Values (Table    : in out Api.Symbols.Table)
      is
      begin
         Table.Create (Name          => "consume",
                       Initial_Value => Create (Consumer_Handlers.Create (Consumer)));
      end Add_Builtin_Values;

      Ignored       : Interpreter_Result;
      Private_Table : constant Symbol_Table_Access :=
                        new Api.Symbols.Table'(Copy_Globals (Symbol_Table));
   begin
      Add_Builtin_Values (Private_Table.all);

      Ignored := Run (Program.Pt, Private_Table);

      if Ignored.Reason /= End_Of_Code  then
         raise Program_Error;
      end if;
   end Run;

end Protypo.Code_Trees.Interpreter;
