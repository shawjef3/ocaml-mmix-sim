%{

  open Parsertypes

%}

%token HexPrefix
%token OctPrefix
%token BinPrefix
%token <string> Symbol
%token <string> Number
%token Dollar
%token Mul Div FracDiv Rem SLeft SRight
%token And Plus Minus Or XOr
%token Negate
%token OpenParen
%token CloseParen
%token Comma
%token At
%token EOF
%token <int> LocalBack
%token <int> LocalForward
%token <int> LocalLabel
%token <char> SQuote
%token <string> DQuote

%nonassoc Symbol At EOF EOI
%nonassoc HexPrefix OctPrefix BinPrefix Number
%nonassoc Negate
%nonassoc Dollar
%nonassoc OpenParen CloseParen
%nonassoc Comma

%left Mul Div Rem FracDiv SLeft SRight
%left And Plus Minus Xor Or

%type <Parsertypes.instruction list> program
%start program

%%

pure : Number { Number $1 }
     | HexPrefix Number { Number16 $2 }
     | OctPrefix Number { Number8 $2 }
     | BinPrefix Number { Number2 $2 }

primary : Symbol { Symbol $1 }
        | pure { Constant $1 }
	| At { At }
	| OpenParen primary CloseParen { $2 }
	| Negate primary { Negate $2 }
	| Minus primary { Negative $2 }
	| Plus primary { $2 }
	| Dollar pure { Register $2 }
	| LocalForward { Forward $1 }
	| LocalBack { Backward $1 }
	| SQuote { Char $1 }
	| DQuote { Str $1 }

term : primary { Term $1 }
      | primary Mul primary { Mul ($1,$3) }
      | primary Div primary { Div ($1,$3) }
      | primary FracDiv primary { FracDiv ($1,$3) }
      | primary Rem primary { Rem ($1,$3) }
      | primary SLeft primary { SLeft ($1,$3) }
      | primary SRight primary { SRight ($1,$3) }

expression : term { Expression $1 }
           | term And term { And ($1,$3) }
	   | term XOr term { XOr ($1,$3) }
	   | term Plus term { Plus ($1,$3) }
	   | term Minus term { Minus ($1,$3) }

instr_aux : EOI { [] }
          | expression { [$1] }
	  | expression Comma instr_aux { $1::($3) }

instruction : Symbol instr_aux { Instruction ($1,$2) }
	    | Symbol Symbol instr_aux { LInstruction ($1,$2,$3) }
	    | LocalLabel Symbol instr_aux { L2Instruction ($1,$2,$3) }
	    | EOI { Empty }

program : instruction { [$1] }
	| instruction program { $1::($2) }
	| EOF { [] }

%%
