package body Protypo.Tokens is

   ----------------
   -- Make_Token --
   ----------------

   function Make_Token (Builder : in out Token_Builder;
                        Class   : Valued_Token;
                        Value   : String)
                        return Token
   is
      Result : constant Token := Token'(Class    => Class,
                                        Value    => To_Unbounded_String (Value),
                                        Position => Builder.Position);
   begin
      Builder.Clear_Position;
      return Result;
   end Make_Token;

   ----------------
   -- Make_Token --
   ----------------

   function Make_Token (Builder : in out Token_Builder;
                        Class   : Unvalued_Token)
                        return Token
   is
      Result : constant Token := Token'(Class    => Class,
                                        Value    => Null_Unbounded_String,
                                        Position => Builder.Position);
   begin
      Builder.Clear_Position;
      return Result;
   end Make_Token;

   function Is_Position_Set (Builder : Token_Builder) return Boolean
   is (Builder.Has_Position);

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Builder  : in out Token_Builder;
                           Position : Token_Position)
   is
   begin
      if Builder.Has_Position then
         raise Constraint_Error;
      else
         Builder.Has_Position := True;
         Builder.Position := Position;
      end if;
   end Set_Position;

   --------------------
   -- Clear_Position --
   --------------------

   procedure Clear_Position (Builder  : in out Token_Builder)
   is
   begin
      if not Builder.Has_Position then
         raise Constraint_Error;
      else
         Builder.Has_Position := False;
      end if;
   end Clear_Position;

end Protypo.Tokens;
