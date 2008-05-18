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
%token EOI
%token <int> LocalBack
%token <int> LocalForward
%token <int> LocalLabel
%token <char> SQuote
%token <string> DQuote


%left Xor Or
%left And
%left SLeft SRight
%left Plus Minus
%left Mul Div Rem FracDiv
%nonassoc Negate Positive Negative
%nonassoc Dollar

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
	| Dollar pure { Register $2 }
	| LocalForward { Forward $1 }
	| LocalBack { Backward $1 }
	| SQuote { Char $1 }
	| DQuote { Str $1 }

term : primary { Term $1 }
      | term Mul term { Mul ($1,$3) }
      | term Div term { Div ($1,$3) }
      | term FracDiv term { FracDiv ($1,$3) }
      | term Rem term { Rem ($1,$3) }
      | term SLeft term { SLeft ($1,$3) }
      | term SRight term { SRight ($1,$3) }
      | term And term { And ($1,$3) }
      | term XOr term { XOr ($1,$3) }
      | term Plus term { Plus ($1,$3) }
      | term Minus term { Minus ($1,$3) }
      | Minus term %prec Negative { Negative $2 }
      | Plus term %prec Positive { $2 }
      | Negate term { Negate $2 }
      | OpenParen term CloseParen { $2 }

instr_aux : EOI { [] }
          | term { [$1] }
	  | term Comma instr_aux { $1::($3) }

instruction : Symbol instr_aux { Instruction ($1,$2) }
	    | Symbol Symbol instr_aux { LInstruction ($1,$2,$3) }
	    | LocalLabel Symbol instr_aux { L2Instruction ($1,$2,$3) }
	    | EOI { Empty }

program : instruction { [$1] }
	| instruction program { $1::($2) }
	| EOF { [] }

%%
