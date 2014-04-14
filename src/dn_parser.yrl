Nonterminals dn rdn attr v vs.
Terminals type value ',' '+' '='.
Rootsymbol dn.

dn   -> dn ',' rdn:       '$1' ++ ['$3'].
dn   -> rdn:              ['$1'].
rdn  -> rdn '+' attr:     '$1' ++ ['$3'].
rdn  -> attr:             ['$1'].
attr -> type '=' vs:      {element(3, '$1'), '$3'}.
attr -> type '=':         {element(3, '$1'), ""}.
vs   -> vs v:             '$1' ++ '$2'.
vs   -> v:                '$1'.
v    -> type:             element(3, '$1').
v    -> '=':              "=".
v    -> value:            element(3, '$1').

Erlang code.