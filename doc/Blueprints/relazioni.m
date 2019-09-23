names = {'program',
	 'EOT',
	 'seq_stat',
	 'statement',
	 'naked',
	 'assign',
	 'stat_return',
	 'stat_if',
	 'stat_loop',
	 'stat_while',
	 'stat_for',
	 'expr_list',
	 'name_list',
	 'NAKED_OPEN',
	 'NAKED_CLOSE',
	 'ASSIGN',
	 'name',	 
	 'ID',
	 'DOT',
	 'OPEN',
	 'CLOSED',
	 'RETURN',
	 'term',
	 'ADDOP',
	 'factor',
	 'MULOP',
	 'expression',
	 'NUMBER',
	 'TEXT',
	 'IF',
	 'THEN',
	 'ELSIF',
	 'ELSE',
	 'END',
	 'LOOP'
	 'IN',
	 'FOR',
	 'WHILE',
	 'AND',
	 'OR',
	 'XOR',
	 'relation',
	 'simple_exp',
	 'SEMICOLON',
	 'COMMA',
	 'COMP',
	 'basic'
	}

alphabet_size = length(names);

for k=1:alphabet_size
  eval([names{k} '=k'])
end

vicini = [seq_stat EOT
	  NAKED_OPEN expr_list
	  expr_list NAKED_CLOSE
	  name_list ASSIGN
	  ASSIGN expr_list
	  expr_list SEMICOLON
	  name COMMA
	  name DOT
	  name OPEN
	  DOT ID 
	  expr_list CLOSED
	  OPEN expr_list
	  RETURN expr_list 
	  expr_list SEMICOLON
	  relation AND
	  relation OR
	  relation XOR
	  simple_exp COMP
	  term ADDOP
	  factor MULOP
	  ADDOP basic
	  OPEN expression
	  expression CLOSED
	  expression COMMA
	  IF expression
expression THEN
THEN seq_stat
ELSIF expression
ELSE seq_stat
  seq_stat END
  END IF
  IF SEMICOLON
  LOOP seq_stat
  seq_stat END
  END LOOP
  LOOP SEMICOLON
  FOR ID
  ID IN
  IN expression
  expression stat_loop
  WHILE expression];
	  
R_vicini = zeros(alphabet_size);
for k=1:size(vicini,1)
  R_vicini(vicini(k,1), vicini(k,2))=1;
end

first = [program seq_stat
	 seq_stat statement
	 statement naked
	 statement assign
	 statement stat_return
	 statement stat_if
	 statement stat_loop
	 statement stat_while	 
	 statement stat_for
	 naked NAKED_OPEN
	 assign name_list
	 name_list name
	 name ID
	 name name
	 stat_return RETURN
	 expression relation
	 relation simple_exp
	 simple_exp term
	 term factor
	 factor ADDOP
	 factor basic
	 basic OPEN
	 basic name
	 basic NUMBER
	 basic TEXT
	 expr_list expression
	 stat_if IF
	 stat_loop LOOP
	 stat_for FOR
	 stat_while WHILE ];

R_first = zeros(alphabet_size);
for k=1:size(first,1)
  R_first(first(k,1), first(k,2))=1;
end

last = [program EOT
	seq_stat statement
	statement naked
	statement assign
	statement stat_return
	statement stat_if
	statement stat_loop
	statement stat_while	 
	statement stat_for
	naked NAKED_CLOSE
	assign SEMICOLON
	name_list name
	name ID
	name CLOSED
	stat_return SEMICOLON
	expression relation
	relation simple_exp
	simple_exp simple_exp
	term term
	factor basic
	basic CLOSED
	basic name
	basic NUMBER
	basic TEXT
	expr_list expr_list
	stat_if SEMICOLON
	stat_loop SEMICOLON
	stat_for stat_loop
	stat_while stat_loop];
	
R_last = zeros(alphabet_size);
for k=1:size(last,1)
  R_last(last(k,1), last(k,2))=1;
end

first_plus = chiusura(R_first');
last_plus = chiusura(R_last');
I = eye(alphabet_size);

R = last_plus' * R_vicini  * (I + first_plus);