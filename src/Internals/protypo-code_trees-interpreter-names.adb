pragma Ada_2012;
with Protypo.Api.Engine_Values.Handlers;

with Protypo.Code_Trees.Interpreter.Expressions;
with Protypo.Code_Trees.Interpreter.Symbol_Table_References;
with Protypo.Symbols;

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
               if Head.Class in Indexed_Handler then
                  return Get_Indexed (Head, Indexes);

               else
                  raise Run_Time_Error
                    with
                      "Indexed access to a value of class="
                      & Head.Class'Image
                    & " at " & Tokens.Image (Expr.Source_Position)
                    & " in an expression of class="
                    & Expr.Class'Image;
               end if;
            end;

         when Identifier  =>

            declare
               use Symbol_Tables;
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
                  Status.Symbol_Table.Create
                    (Name          => Ident,
                     Position      => Position,
                     Initial_Value => symbols.To_Symbol_Value (Void_Value));

                  --                    Put_Line ("@@@ inserted '" & ID & "' @" & Image (Position));
                  if Position = Protypo_Tables.No_Element then
                     raise Program_Error
                       with "I added the variable '"
                       & String (Ident) & "' to the symbol table, "
                       & "but it seems it is not there. Why?";
                  end if;
               end if;

               --
               --  Here Position points to an actual entry of the
               --  symbol table
               --

               pragma Assert (Position /= Protypo_Tables.No_Element);

               return Symbol_Table_Reference (Position);
            end;
      end case;
   exception
      when Handlers.Unknown_Field =>
         raise Run_Time_Error with "Unknown field at " & Tokens.Image (Expr.Source_Position);
   end Eval_Name;

end Protypo.Code_Trees.Interpreter.Names;
