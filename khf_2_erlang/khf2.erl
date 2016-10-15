-module(khf2).
-author('falusi.david94@gmail.com').
-vsn('$LastChangedDate: 2016-10-15 14:30:51 +0200 (Sat, 15 Oct 2016) $$').
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
    io:format("List: ~p \n", [createRetList(5)]),
    [].

% Create list with every possible values
createRetList(K) ->
    createRetList(K, [], 1).
createRetList(K, L, I) ->
    if
        I<K orelse I=:=K ->
            createRetList(K, lists:append(L, [I]), I+1);
        true ->
            L
    end.

% Return an item from given matrix structure
getItem([H|T], RI, CI, R, C) ->
    ELEMENT_LIST = isElementList(H),
    if
        is_list(H) andalso ELEMENT_LIST ->
            if
                RI<R ->
                    getItem(T, RI+1, CI, R, C);
                RI=:=R ->
                    getItem(H, RI, CI, R, C)
            end;
        is_list(H) andalso not ELEMENT_LIST ->
            if
                CI<C ->
                    getItem(T, RI, CI+1, R, C);
                CI=:=C ->
                    getItem(H, RI, CI, R, C)
            end;
        true ->
            [H|T]
    end;
getItem([], _, _, _, _) ->
    [].
getItem([H|T], R, C) ->
    getItem([H|T], 1, 1, R, C).

% Returns true if the first element is a list of the input list,
% every other case returns false
isElementList([H|_]) ->
    is_list(H);
isElementList(_) ->
    false.
