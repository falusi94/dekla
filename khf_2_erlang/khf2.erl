-module(khf2).
-author('falusi.david94@gmail.com').
-vsn('2016-09-28').
-export([ertekek/2]).

%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @spec khf2:ertekek(SSpec::sspec(), R_C::coords()) -> Vals::[integer()]
%%   Egy érték pontosan akkor szerepel a Vals listában, ha teljesíti a
%%   fenti Prolog specifikációban felsorolt (a), (b) és (c) feltételeket, ahol
%%   Vals az SSpec specifikációval megadott Sudoku-feladvány R_C
%%   koordinátájú mezőjében megengedett értékek listája.

ertekek({K, L}, {R,C}) ->
    RET = getItem(L,R,C),
    io:format("RET: ~p \n", [RET]),
    [].

getItem([H|T], RI, CI, R, C) ->
    io:format("R: ~p  C: ~p  H: ~p\n", [RI, CI,H]),
    ELEMENT_LIST = isElementList(H),
    if
        is_list(H) andalso ELEMENT_LIST ->
            io:format("is_list(H) andalso ELEMENT_LIST\n", []),
            if
                RI<R ->
                    getItem(T, RI+1, CI, R, C);
                RI=:=R ->
                    getItem(H, RI, CI, R, C)
            end;
        is_list(H) andalso not ELEMENT_LIST ->
            io:format("is_list(H) andalso not ELEMENT_LIST\n", []),
            if
                CI<C ->
                    getItem(T, RI, CI+1, R, C);
                CI=:=C ->
                    getItem(H, RI, CI, R, C)
            end;
        true ->
            io:format("RET\n", []),
            [H|T]
    end;
getItem([], _, _, _, _) ->
    [].
getItem([H|T], R, C) ->
    getItem([H|T], 1, 1, R, C).

isElementList([H|_]) ->
    is_list(H);
isElementList(_) ->
    false.
