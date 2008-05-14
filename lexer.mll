{

  include Parser

}

rule lex = parse
    | ([ '0' - '9' ] as n)['B'] { LocalBack (int_of_string n) }
    | ([ '0' - '9' ] as n)['F'] { LocalForward (int_of_string n) }
    | ([ '0' - '9' ] as n)['H'] { LocalLabel (int_of_string n) }
    | '\''([^ '\n' '\''] as n)'\'' { SQuote n }
    | '\"'([^ '\n' '\"']* as n)'\"' { DQuote n }
    | '#' [' ' '\t']+ '\"' ([^ '\"' '\n']* as name) '\"' eof { lex lexbuf }
    | '#' { HexPrefix }
    | ['0' - '9' 'a' - 'f' 'A' - 'F']+ as n { Number n }
    | '$' { Dollar }
    | '*' { Mul }
    | '/' { Div }
    | "//" { FracDiv }
    | '%' { Rem }
    | "<<" { SLeft }
    | ">>" { SRight }
    | '&' { And }
    | '+' { Plus }
    | '-' { Minus }
    | '^' { XOr }
    | '|' { Or }
    | '~' { Negate }
    | '(' { OpenParen }
    | ')' { CloseParen }
    | ',' { Comma }
    | ['a' - 'z' '_' ':']['a'-'z' 'A'-'Z' '0'-'9' '_' ':']* as l { Symbol l }
    | '@' { At }
    | ' ' | '\t' { lex lexbuf }
    | ';' | '\n' { EOI }
    | eof { EOF }
