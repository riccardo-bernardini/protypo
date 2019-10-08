pragma Ada_2012;

with Protypo.Code_Trees.Interpreter.Expressions;
with Protypo.Code_Trees.Interpreter.Symbol_Table_References;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO; use Ada.Text_IO;

package body Protypo.Code_Trees.Interpreter.Names is

   ---------------
   -- Eval_Name --
   ---------------

   function Eval_Name (Status : Interpreter_Access;
                       Expr   : not null Node_Access)
                       return Name_Reference
   is

      ---------
      -- "+" --
      ---------

      function "+" (X : Handler_Value) return Name_Reference
      is
      begin
         if not (X.Class in Handler_Classes) then
            raise Program_Error with X.Class'Image & "is not handler class";
         end if;

         case Handler_Classes (X.Class) is
            when Array_Handler =>
               return Name_Reference'(Class            => Array_Reference,
                                      Array_Handler    => Get_Array (X));

            when Record_Handler =>
               return Name_Reference'(Class             => Record_Reference,
                                      Record_Handler    => Get_Record (X));

            when Ambivalent_Handler =>
               return Name_Reference'(Class              => Ambivalent_Reference,
                                      Ambivalent_Handler => Get_Ambivalent (X));

            when Function_Handler =>
               return Name_Reference'(Class            => Function_Reference,
                                      Function_Handler => Get_Function (X),
                                      Parameters       => <>);

            when Reference_Handler =>
               return Name_Reference'(Class            => Variable_Reference,
                                      Variable_Handler => Get_Reference (X));

            when Constant_Handler =>
               return Name_Reference'(Class             => Constant_Reference,
                                      Costant_Handler   => Get_Constant (X));
         end case;
      end "+";


   begin
      --              Put_Line ("#1" & Expr.Class'Image);
      if not (Expr.Class in Name) then
         raise Program_Error;
      end if;

      case Name (Expr.Class) is
         when Selected    =>
            declare
               Head  : constant Name_Reference := Eval_Name (Status, Expr.Record_Var);
               Field : constant ID := To_String (Expr.Field_Name);
            begin
               case Head.Class is
                  when Record_Reference =>
                     if not Head.Record_Handler.Is_Field (Field) then
                        raise Bad_Field  with "Unknown field '" & Field & "'";
                     end if;

                     return + Head.Record_Handler.Get (Field);

                  when Ambivalent_Reference =>
                     if not Head.Ambivalent_Handler.Is_Field (Field) then
                        raise Bad_Field  with "Unknown field '" & Field & "'";
                     end if;

                     return + Head.Ambivalent_Handler.Get (Field);

                  when others =>
                     raise Run_Time_Error with "Record access to non-record value";
               end case;
            end;
         when Indexed     =>
            declare
               subtype Indexed_References is Value_Name_Class
                 with Static_Predicate => Indexed_References
                   in Array_Reference | Function_Reference | Ambivalent_Reference;

               Head    : constant Name_Reference := Eval_Name (Status, Expr.Indexed_Var);
               Indexes : constant Engine_Value_Vectors.Vector := Expressions.Eval_Vector (Status, Expr.Indexes);
            begin
               if not (Head.Class in Indexed_References) then
                  raise Program_Error with Head.Class'Image;
               end if;

               case Indexed_References (Head.Class) is
                  when Array_Reference =>

                     return + Head.Array_Handler.Get (Expressions.To_Array (Indexes));

                  when Function_Reference =>

                     return Name_Reference'(Class            => Function_Call,
                                            Function_Handler => Head.Function_Handler,
                                            Parameters       => Indexes);

                  when Ambivalent_Reference =>
                     --                       Put_Line ("@@@ index");
                     return + Head.Ambivalent_Handler.Get (Expressions.To_Array (Indexes));

               end case;
            end;

         when Identifier  =>

            declare
               use Api.Symbols.Protypo_Tables;
               use Protypo.Code_Trees.Interpreter.Symbol_Table_References;

               ID       : constant String := To_String (Expr.ID_Value);
               Position : Cursor := Status.Symbol_Table.Find (ID);
               Val      : Engine_Value := Void_Value;
            begin

--                 Put_Line ("@@@ searching '" & ID & "'");
               if Position = No_Element then

--                    Put_Line ("@@@ not found '" & ID & "'");
                  --
                  -- The name is not in the symbol table: create it
                  -- but leave it not initialized, it can be used only
                  -- as a LHS.
                  --
                  Status.Symbol_Table.Create (Name          => ID,
                                              Position      => Position,
                                              Initial_Value => Void_Value);

--                    Put_Line ("@@@ inserted '" & ID & "' @" & Image (Position));
                  if Position = No_Element then
                     raise Program_Error with "something bad";
                  end if;

                  declare
                     X : constant Reference_Interface_Access :=
                           Symbol_Table_Reference (Position);

                     Result : Name_Reference :=
                                (Class            => Variable_Reference,
                                 Variable_Handler => X);
                  begin
--                       Put_Line ("@@@ hh");
                     Result.Variable_Handler := X;
--                       Put_Line ("@@@ hhh");
                     return Result;
                  end;
               else
                  --
                  -- The name is in the symbol table.  If its value is an
                  -- handler, returnt the handler; otherwise return the
                  -- reference to the symbol table.  Remember that the
                  -- result of the evaluation of a name is always a reference.
                  --
                  Val := Value (Position);


                  if Val.Class in Handler_Classes then
                     return + Val;

                  else
                     return Name_Reference'
                       (Class            => Variable_Reference,
                        Variable_Handler => Symbol_Table_Reference (Position));

                  end if;
               end if;
            end;
      end case;
   exception
      when Unknown_Field =>
         raise Run_Time_Error with "Unknown field at " & Tokens.Image (Expr.Source_Position);
   end Eval_Name;

end Protypo.Code_Trees.Interpreter.Names;
