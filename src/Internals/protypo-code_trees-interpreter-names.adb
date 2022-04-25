pragma Ada_2012;
with Protypo.Api.Engine_Values.Handlers;

with Protypo.Code_Trees.Interpreter.Expressions;
with Protypo.Code_Trees.Interpreter.Symbol_Table_References;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_Io; use Ada.Text_Io;
with Protypo.Api.Engine_Values; use Protypo.Api.Engine_Values;

package body Protypo.Code_Trees.Interpreter.Names is

   ---------------
   -- Eval_Name --
   ---------------

   function Eval_Name (Status : Interpreter_Access;
                       Expr   : not null Node_Access)
                       return Api.Engine_Values.Engine_Reference'Class
   is

      --  ---------
      --  -- "+" --
      --  ---------
      --
      --  function "+" (X : Handler_Value) return Name_Reference
      --  is
      --  begin
      --     if not (X.Class in Handler_Classes) then
      --        raise Program_Error with X.Class'Image & "is not handler class";
      --     end if;
      --
      --     case Handler_Classes (X.Class) is
      --        when Array_Handler =>
      --           return Name_Reference'(Class            => Array_Reference,
      --                                  Array_Handler    => Handlers.Get_Array (X));
      --
      --        when Record_Handler =>
      --           return Name_Reference'(Class             => Record_Reference,
      --                                  Record_Handler    => Handlers.Get_Record (X));
      --
      --        when Ambivalent_Handler =>
      --           return Name_Reference'(Class              => Ambivalent_Reference,
      --                                  Ambivalent_Handler => Handlers.Get_Ambivalent (X));
      --
      --        when Function_Handler =>
      --           return Name_Reference'(Class            => Function_Reference,
      --                                  Function_Handler => Handlers.Get_Function (X),
      --                                  Parameters       => <>);
      --
      --        when Reference_Handler =>
      --           return Name_Reference'(Class            => Variable_Reference,
      --                                  Variable_Handler => Handlers.Get_Reference (X));
      --
      --        when Constant_Handler =>
      --           return Name_Reference'(Class             => Constant_Reference,
      --                                  Costant_Handler   => Handlers.Get_Constant (X));
      --     end case;
      --  end "+";


   begin
      --              Put_Line ("#1" & Expr.Class'Image);
      if not (Expr.Class in Name) then
         raise Program_Error;
      end if;

      case Name (Expr.Class) is
         when Selected    =>
            declare
               Head  : constant Engine_Value :=
                         Expressions.Eval_Single_Expression (Status, Expr.Record_Var);

               Field : constant Id := Id (To_String (Expr.Field_Name));
            begin
               case Head.Class is
                  when Record_Handler | Ambivalent_Handler =>
                     if not Is_Field (Head, Field) then
                        raise Bad_Field
                          with "Unknown field '" & String (Field) & "'";
                     end if;

                     return Get_Field (Head, Field);

                     --  when Ambivalent_Reference =>
                     --     if not Head.Ambivalent_Handler.Is_Field (Field) then
                     --        raise Bad_Field  with "Unknown field '" & String (Field) & "'";
                     --     end if;
                     --
                     --     return + Head.Ambivalent_Handler.Get (Field);

                  when others =>
                     raise Run_Time_Error
                       with
                         "Record access to non-record value, class="
                         & Head.Class'Image;
               end case;
            end;
         when Indexed     =>
            declare
               --  subtype Indexed_References is Value_Name_Class
               --    with Static_Predicate =>
               --      Indexed_References
               --        in Array_Reference      |
               --          Function_Reference   |
               --            Ambivalent_Reference;

               Head    : constant Engine_Value :=
                           Expressions.Eval_Single_Expression (Status, Expr.Indexed_Var);

               Indexes : constant Engine_Value_Array :=
                           Expressions.Eval_Vector (Status, Expr.Indexes);
            begin
               if not (Head.Class in Indexed_Handler) then
                  raise Run_Time_Error
                    with
                      "Indexed access to a value of class="
                      & Head.Class'Image
                    & " at " & Tokens.Image (Expr.Source_Position)
                    & " in an expression of class="
                    & Expr.Class'Image;
               end if;

               case Indexed_Handler (Head.Class) is
                  when Array_Handler | Ambivalent_Handler =>

                     return Get_Indexed (Head, Indexes);

                  when Function_Handler =>

                     return Name_Reference'(Class            => Function_Call,
                                            Function_Handler => Head.Function_Handler,
                                            Parameters       => Indexes);
               end case;
            end;

         when Identifier  =>

            declare
               use Api.Symbols;
               use Protypo.Code_Trees.Interpreter.Symbol_Table_References;

               use type Protypo_Tables.Cursor;

               Ident    : constant Id := To_Id (Expr.Id_Value);
               Position : Protypo_Tables.Cursor := Status.Symbol_Table.Find (Ident);
            begin

               --                 Put_Line ("@@@ searching '" & String(IDent) & "'");
               if Position = Protypo_Tables.No_Element then

                  --                    Put_Line ("@@@ not found '" & ID & "'");
                  --
                  -- The name is not in the symbol table: create it
                  -- but leave it not initialized, it can be used only
                  -- as a LHS.
                  --
                  Status.Symbol_Table.Create (Name          => Ident,
                                              Position      => Position,
                                              Initial_Value => Void_Value);

                  --                    Put_Line ("@@@ inserted '" & ID & "' @" & Image (Position));
                  if Position = Protypo_Tables.No_Element then
                     raise Program_Error with "something bad";
                  end if;

                  declare
                     X : constant Api.Engine_Values.Handlers.Reference_Interface_Access :=
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
                  declare
                     Val : constant Engine_Value := Value (Position);
                  begin
                     if Val.Class in Handler_Classes then
                        return + Val;

                     else
                        return Name_Reference'
                          (Class            => Variable_Reference,
                           Variable_Handler => Symbol_Table_Reference (Position));

                     end if;
                  end;
               end if;
            end;
      end case;
   exception
      when Handlers.Unknown_Field =>
         raise Run_Time_Error with "Unknown field at " & Tokens.Image (Expr.Source_Position);
   end Eval_Name;

end Protypo.Code_Trees.Interpreter.Names;
