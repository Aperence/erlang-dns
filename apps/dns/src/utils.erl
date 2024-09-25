-module(utils).

-export([name_to_atoms/1]).

name_to_atoms(Name) ->
    L=string:tokens(Name, "."),
    L2 = lists:map(fun (Elem) -> list_to_atom(Elem) end, L),
    lists:reverse(L2).
