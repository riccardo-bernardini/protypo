with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Protypo.Tokens;

private
package Protypo.Code_Trees is
   package ID_Lists is 
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => String);
   
   subtype Id_List is ID_Lists.Vector;
   
   type Non_Terminal is
     (
      Statement_Sequence,
      Defun,
      Assignment,
      Return_Statement,
      Procedure_Call,
      Exit_Statement,
      If_Block,
      Loop_Block,
      For_Block,
      While_Block,
      Binary_Op,
      Unary_Op,
      Int_Constant,
      Real_Constant,
      Text_Constant,
      Selected,
      Indexed,
      Identifier,
      List_Of_Names,
      List_Of_Expressions,
      Parameter_Signature
     );

   subtype Expression        is Non_Terminal range Binary_Op .. Identifier;
   subtype Name              is Non_Terminal range Selected .. Identifier;
   subtype Statement_Classes is Non_Terminal range Statement_Sequence .. While_Block;
   
   type Parsed_Code is private;

   Empty_Tree : constant Parsed_Code;

   function Is_Empty (X : Parsed_Code) return Boolean;
   
   type Tree_Array is array (Positive range <>) of Parsed_Code;


   Empty_Tree_Array : constant Tree_Array;

   function Class (X : Parsed_Code) return Non_Terminal;

   function If_Then_Else (Conditions    : Tree_Array;
                          Then_Branches : Tree_Array;
                          Else_Branch   : Parsed_Code)
                          return Parsed_Code
     with
       Pre =>
         Conditions'Length = Then_Branches'Length
         and (for all Cond of Conditions => Class (Cond) in Expression)
         and (for all B of Then_Branches => Class (B) in Statement_Sequence),
         Post =>
           Class (If_Then_Else'Result) = If_Block;

   function Assignment (LHS   : Tree_Array;
                        Value : Tree_Array)
                        return Parsed_Code
     with
       Pre =>
         (for all Item of Lhs => Class (Item) in Name) and
         (for all Item of Value => Class (Item) in Expression),
         Post =>
           Class (Assignment'Result) = Assignment;
   
   function Parameter_List (Names   : Id_List;
                            Default : Tree_Array)
                            return Parsed_Code
     with 
       Pre => (for all Val of Default => Is_Empty (Val) or else (Class (Val) in Expression)),
     Post => Class (Parameter_List'Result) = Parameter_Signature;

   function Definition (Name           : String;
                        Parameter_List : Parsed_Code;
                        Function_Body  : Parsed_Code;
                        Is_Function    : Boolean)
                        return Parsed_Code
     with 
       Pre => 
         Class (Function_Body) = Statement_Sequence
     and Class (Parameter_List) = Parameter_Signature,
     Post => 
       Class (Definition'Result) = Defun;
   
   function Statement_Sequence (Statements : Tree_Array)
                                return Parsed_Code
     with
       Post => Class (Statement_Sequence'Result) = Statement_Sequence;

   --     function Naked_Expression (Statements : Tree_Array)
   --                                return Parsed_Code
   --       with
   --         Post => Class (Naked_Expression'Result) = Naked;

   function Binary_Operation (Left      : Parsed_Code;
                              Right     : Parsed_Code;
                              Operation : Tokens.Binary_Operator)
                              return Parsed_Code
     with
       Post => Class (Binary_Operation'Result) = Binary_Op;

   function Unary_Operation (X         : Parsed_Code;
                             Operation : Tokens.Unary_Operator)
                             return Parsed_Code
     with
       Post => Class (Unary_Operation'Result) = Unary_Op;


   function String_Constant (Val : String) return Parsed_Code
     with
       Post => Class (String_Constant'Result) = Text_Constant;

   function Integer_Constant (Val : String) return Parsed_Code
     with
       Post => Class (Integer_Constant'Result) = Int_Constant;

   function Float_Constant (Val : String) return Parsed_Code
     with
       Post => Class (Float_Constant'Result) = Real_Constant;


   function Identifier (Id : String) return Parsed_Code
     with
       Post => Class (Identifier'Result) = Identifier;

   function Indexed_Name (Function_Ref : Parsed_Code;
                          Parameters   : Tree_Array)
                          return Parsed_Code
     with
       Post => Class (Indexed_Name'Result) = Indexed;

   function Procedure_Call (Procedure_Name : String;
                            Parameters     : Tree_Array)
                            return Parsed_Code
     with
       Post => Class (Procedure_Call'Result) = Procedure_Call;

   function Procedure_Call (Procedure_Name : String)
                            return Parsed_Code
     with
       Post => Class (Procedure_Call'Result) = Procedure_Call;


   function Selector (Ref   : Parsed_Code;
                      Field : String)
                      return Parsed_Code
     with
       Post => Class (Selector'Result) = Selected;


   function Loop_Exit (Label     : String)
                       return Parsed_Code;


   function Basic_Loop (Loop_Body : Parsed_Code;
                        Label     : String)
                        return Parsed_Code
     with
       Post => Class (Basic_Loop'Result) = Loop_Block;


   function For_Loop (Variable  : Parsed_Code;
                      Iterator  : Parsed_Code;
                      Loop_Body : Parsed_Code)
                      return Parsed_Code
     with
       Post => Class (For_Loop'Result) = For_Block,
     Pre =>
       Class (Variable) = Identifier
     and
       Class (Loop_Body) = Loop_Block;


   function While_Loop (Condition : Parsed_Code;
                        Loop_Body : Parsed_Code)
                        return Parsed_Code
     with
       Pre => Class (Loop_Body) = Loop_Block  and (Class (Condition) in Expression),
     Post => Class (While_Loop'Result) = While_Block;



   function Return_To_Caller (Values : Tree_Array)
                              return Parsed_Code
     with
       Pre => (for all V of Values => Class (V) in Expression),
       Post => Class (Return_To_Caller'Result) = Return_Statement;
   
   procedure Delete (Code : in out Parsed_Code);

   procedure Dump (Code : Parsed_Code);
private
   subtype Label_Type is Unbounded_String;
   subtype Loops is Non_Terminal range Loop_Block .. While_Block;
   
   type Node;
   type Node_Access is access Node;

   package Node_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Node_Access);
   
   type Parameter_Specs is
      record
         Names   : Id_List;
         Default : Node_Vectors.Vector;
      end record;
   
   type Conditional_Branch is 
      record
         Condition : Node_Access;
         Code      : Node_Access;
      end record;
   
   package Conditional_Branch_Vectors is 
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Conditional_Branch);
   
   type Node (Class : Non_Terminal) is
      record
         case Class is
            when Parameter_Signature =>
               Signature : Parameter_Specs;
               
            when Defun => 
               Is_Function     : Boolean;
               Definition_Name : Unbounded_String;
               Function_Body   : Node_Vectors.Vector;
               Parameters      : Parameter_Specs;
               
            when Statement_Sequence =>
               Statements      : Node_Vectors.Vector;
            
               --              when Naked =>
               --                 Naked_Values    : Node_Vectors.Vector;
            
            when Assignment =>
               Lhs             : Node_Vectors.Vector;
               Rvalues         : Node_Vectors.Vector;
            
            when Return_Statement =>
               Return_Values   : Node_Vectors.Vector;
            
            when Procedure_Call =>
               Name            : Unbounded_String;
               Params          : Node_Vectors.Vector;
            
            when Exit_Statement =>
               Loop_Label      : Label_Type;
            
            when If_Block =>
               Branches        : Conditional_Branch_Vectors.Vector;
               Else_Branch     : Node_Access;
            
            when List_Of_Names =>
               Names           : Node_Vectors.Vector;
            
            when List_Of_Expressions =>
               Exprs           : Node_Vectors.Vector;
            
            when Binary_Op =>
               Operator        : Tokens.Binary_Operator;
               Left            : Node_Access;
               Right           : Node_Access;
         
            when Unary_Op =>
               Uni_Op          : Tokens.Unary_Operator;
               Operand         : Node_Access;
            
            when Int_Constant =>
               N               : Integer;
            
            when Real_Constant =>
               X               : Float;
            
            when Text_Constant =>
               S               : Unbounded_String;
            
            when Selected =>
               Record_Var      : Node_Access;
               Field_Name      : Unbounded_String;
               
            when Indexed =>
               Indexed_Var     : Node_Access;
               Indexes         : Node_Vectors.Vector;
               
            when Identifier => 
               ID_Value        : Unbounded_String;
            
            when Loop_Block | For_Block |  While_Block =>
               Loop_Body       : Node_Vectors.Vector;
               Labl            : Label_Type;
            
               case Class is 
               when Loop_Block =>
                  null;
                  
               when For_Block =>
                  Variable : Unbounded_String;
                  Iterator : Node_Access;
                  
               when While_Block =>
                  Condition : Node_Access;
                  
               when others =>
                  null;
               end case;
         end case;
      end record;

   procedure Free  is 
     new Ada.Unchecked_Deallocation (Object => Node,
                                     Name   => Node_Access);
   
   procedure Delete (Item : in out Node_Access);
   procedure Delete (Item : in out Node_Vectors.Vector);
   
   type Parsed_Code is
      record
         Pt : Node_Access;
      end record;

   Empty_Tree : constant Parsed_Code := (Pt => null);

   Empty_Tree_Array : constant Tree_Array (2 .. 1) := (others => <>);

   function Is_Empty (X : Parsed_Code) return Boolean
   is (X.Pt = null);
   
end Protypo.Code_Trees;
