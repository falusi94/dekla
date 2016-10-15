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

ertekek({K, LIST}, {ROW,COL}) ->
    RET = getItem(LIST,ROW,COL),
    io:format("RET: ~p \n", [RET]),
    io:format("List: ~p \n", [createFullList(5)]),
    io:format("diff: ~p \n", [removeElements(createFullList(5), [2])]),
    [].

% Remove not valid elements from list
removeElements(LIST, [HEAD]) ->
    lists:delete(HEAD, LIST);
removeElements(LIST, [HEAD|TAIL]) ->
    removeElements(lists:delete(HEAD, LIST), TAIL).

% Create list with every possible values
createFullList(K) ->
    createFullList(K, [], 1).
createFullList(K, LIST, INDEX) ->
    if
        INDEX<K orelse INDEX=:=K ->
            createFullList(K, lists:append(LIST, [INDEX]), INDEX+1);
        true ->
            LIST
    end.

% Return an item from given matrix structure
getItem([HEAD|TAIL], ROWI, COLI, ROW, COL) ->
    ELEMENT_LIST = isElementList(HEAD),
    if
        is_list(HEAD) andalso ELEMENT_LIST ->
            if
                ROWI<ROW ->
                    getItem(TAIL, ROWI+1, COLI, ROW, COL);
                ROWI=:=ROW ->
                    getItem(HEAD, ROWI, COLI, ROW, COL)
            end;
        is_list(HEAD) andalso not ELEMENT_LIST ->
            if
                COLI<COL ->
                    getItem(TAIL, ROWI, COLI+1, ROW, COL);
                COLI=:=COL ->
                    getItem(HEAD, ROWI, COLI, ROW, COL)
            end;
        true ->
            [HEAD|TAIL]
    end;
getItem([], _, _, _, _) ->
    [].
getItem([HEAD|TAIL], ROW, COL) ->
    getItem([HEAD|TAIL], 1, 1, ROW, COL).

% Returns true if the first element is a list of the input list,
% every other case returns false
isElementList([HEAD|_]) ->
    is_list(HEAD);
isElementList(_) ->
    false.
