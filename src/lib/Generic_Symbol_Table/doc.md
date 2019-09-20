# What is this?

This package provides a generic implementation of a symbol table that can be handy in interpreters and parsers.
The pacakge is parameterized with the type of the symbol name and the symbol value.

# The conceptual model

A symbol table is a structure mapping names to value, but differently from a normal map it has a hierarchial structure that reflects the block structure in many programming languages.  The table can be see conceptually as a tree, where each node is a map.  
* At every time there is an *active node* that is always a leaf. 
* When a symbol is searched for, it is first searched in the active node and if not found it is searched in its ancestors until the root is reached.  
* When a new symbol it is added, it is always added in the current node
* If the value of a symbol is modified, it can be in any node
* When a new block in the code is opened, the corresponding map is added as a leaf and made the active node.  
* When the block is exited,the corresponding node is deleted and the node who opened the block becomes the active one.
* The root node can never be deleted and it contains the *global* symbols

### Cursors

There two type of *cursors* (like "internal pointer")
1. Cursors to entries (used to read/write an entry value)
1. Cursors to nodes (used to create new blocks, see below)

## Opening new blocks

There are two cases when it is necessary to open a new block.  The first case is an *internal block* like, for example, an Ada `declare`
```
declare
  x: Integer;
begin 
  -- something
end;
```
In this case inside the block we still have the visibility of all the symbols above, so we create the new table block as a **child of the current one**.

Another case is when a procedure or function is entered, like in
```
procedure Foo(X: Integer) is
  C : Float;
 begin
   -- something
 end Foo;
 ```
 In this case we loose the connection with the local symbols of the caller, therefore the block is created as child of the root.
 
 In general, we can create a new block as a child of any node.  This would allow to have procedures defined inside other procedures.  Therefore, we can imagine having the following procedures
 
 ```
 procedure Open_Block (Table  : Symbol_Table; 
                       Parent : Table_Block := Table.Current_Block)
 with Post => Parent_Of(Table.Current_Block) = Parent;
                      
 function Root (Table : Symbol) return Table_Block;
 
 function Current_Block (Table : Symbol) return Table_Block;
 
 procedure Close_Current_Block (Table : Symbol_Table)
 with Pre => Table.Current_Block /= Table.Root;
 
 ```
