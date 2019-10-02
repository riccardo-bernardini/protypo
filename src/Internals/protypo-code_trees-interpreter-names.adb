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

      function "+" (X : Engine_Value) return Name_Reference
        with
          Pre => X.Class in Handler_Classes;

      function "+" (X : Engine_Value) return Name_Reference
      is
      begin
         if not (X.Class in Handler_Classes) then
            raise Program_Error;
         end if;

         case Handler_Classes (X.Class) is
            when Array_Handler =>
               return Name_Reference'(Class            => Array_Reference,
                                      Array_Handler    => Get_Array (X));

            when Record_Handler =>
               return Name_Reference'(Class             => Record_Reference,
                                      Record_Handler    => Get_Record (X));

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
      --        Put_Line ("#1" & Expr.Class'Image);
      if not (Expr.Class in Name) then
         raise Program_Error;
      end if;

      case Name (Expr.Class) is
         when Selected    =>
            declare
               Head : constant Name_Reference := Eval_Name (Status, Expr.Record_Var);
            begin
               if Head.Class /= Record_Reference then
                  raise Constraint_Error;
               end if;

               return + Head.Record_Handler.Get (To_String (Expr.Field_Name));
            end;
         when Indexed     =>
            declare
               subtype Indexed_References is Value_Name_Class
                 with Static_Predicate => Indexed_References in Array_Reference | Function_Reference;

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
               end case;
            end;

         when Identifier  =>

            declare
               use Api.Symbols.Protypo_Tables;
               use Protypo.Code_Trees.Interpreter.Symbol_Table_References;

               ID       : constant String := To_String (Expr.ID_Value);
               Position : Cursor := Status.Symbol_Table.Find (ID);
               Val      : Engine_Value;
            begin

               if Position = No_Element then
                  --
                  -- The name is not in the symbol table: create it
                  -- but leave it not initialized, it can be used only
                  -- as a LHS.
                  --
                  Status.Symbol_Table.Create (Name          => ID,
                                              Position      => Position);

                  return Name_Reference'
                    (Class            => Variable_Reference,
                     Variable_Handler => Symbol_Table_Reference (Position));
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
   end Eval_Name;

end Protypo.Code_Trees.Interpreter.Names;
