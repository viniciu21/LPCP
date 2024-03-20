<P> -> <DECLS> <SUBPs> <M>


<DECLS> -> | <DECL_BEGIN> <DECS> <DECL_END>

<DECS> -> <DECL> 
	  | <DECL> <DECS>

<DECL> -> <var_decl> | <type_decl> |<const_decl> | <func_decl>

<var_decl> ->  <ids> ':' <type> ';'

<ids> -> <id> | <id> ',' <ids>

<const_decl> -> "const" <id> '=' <value> ';'

<func_decl> -> <id> ':' <args> "->" <type> ';'

<args> -> <types>

<types> -> <type>
	| <type> ',' <types> 

<type_decl> -> "struct" '{' '}' <id> ';'

<value> -> <number> | <caracter> | <cadeia> 

<number> -> Conjunto dos números reais 
<caracter> -> elemento da tabela ascii
<cadeia> -> <caracter> <cadeia> 
<id> -> <cadeia>
<type> -> <int> | <char> | <string> | <float> | <bool>

<SUBPs> -> "func" <id> ':' <programa> <END_FUNC>

<M> -> <BEGIN_M> <programa> <END_M>

<programa> -> <DECS> <programa> 
	      | <FOR> <programa>
	      | <WHILE> <programa>
	      | <IF> <programa>
	      | <atr> <programa>
<atr> -> <id> '=' <value> 
	 | <id> '=' <opera>
<opera> -> <id> <op> <id> 
	   |<id> <op> <opera>
	   |<id> <op> <value>
	   |<value> <op> <value>
	   |<value> <op> <opera>

<op> -> '+' | '-' | '/' | '*' | '**' 

<FOR> -> "for" '(' <atr> ';' <bool> ';' <opera> ') ':' <programa> <END_FOR>
<IF> -> "if" '(' <bool> ')' ':' <programa> <END_IF>
	| "if" '(' <bool> ')' ':' <programa> "elif" '(' <bool> ')' ':' <programa> <END_ELIF> <END_IF>
	|"if" '(' <bool> ')' ':' <programa> "elif" '(' <bool> ')' ':' <programa> <END_ELIF> "else" <programa> <END_ELSE> <END_IF> 
	|"if" '(' <bool> ')' ':' <programa> "else" <programa> <END_ELSE> <END_IF>  

<WHILE> -> "while" '(' <bool> ')' <programa> <END_WHILE>

  


	