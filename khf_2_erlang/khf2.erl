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
    RET = getItem(LIST, ROW, COL),
    io:format("RET: ~p \n", [RET]),
    io:format("List: ~p \n", [createFullList(5)]),
    io:format("diff: ~p \n", [removeElements(createFullList(5), [2,3])]),
    io:format("tuple: ~p \n", [createTuple([o])]),
    io:format("original: ~p \n", [LIST]),
    io:format("normalized: ~p \n", [normalizeInputList({K, LIST})]),
    [].

% Change items to tuples {num, even/odd, south/west}
normalizeInputList({K, LIST}) ->
    normalizeInputList({K, LIST}, 1, 1, [], []).
normalizeInputList({K, LIST}, ROWI, COLI, RET, TEMP) ->
    if
        ROWI>K*K ->
            RET;
        COLI>K*K ->
            normalizeInputList({K, LIST}, ROWI+1, 1, lists:append(RET, [TEMP]), []);
        true ->
            ITEM = createTuple(getItem(LIST, ROWI, COLI)),
            normalizeInputList({K, LIST}, ROWI, COLI+1, RET, lists:append(TEMP, [ITEM]))
    end.

% Create tuple from an 'item' (field)
createTuple(ITEM) ->
    PARITY = evenOrOdd(ITEM),
    DIRECTION = southOrWest(ITEM),
    NUMBER = number(ITEM),
    {NUMBER, PARITY, DIRECTION}.

% Gives back the number if exists in item, else -1
number([HEAD]) ->
    if
        is_number(HEAD) ->
            HEAD;
        true ->
            -1
    end;
number([HEAD|TAIL]) ->
    if
        is_number(HEAD) ->
            HEAD;
        true ->
            number(TAIL)
    end;
number([]) ->
    -1.

% Gives back proper direction, if given, else return u (unknown)
southOrWest(ITEM) ->
    MEMBER_S = lists:member(s, ITEM),
    MEMBER_W = lists:member(w, ITEM),
    if
        MEMBER_S ->
            s;
        MEMBER_W ->
            w;
        true ->
            u
    end.

% Gives back proper parity, if given, else return u (unknown)
evenOrOdd(ITEM) ->
    MEMBER_O = lists:member(o, ITEM),
    MEMBER_E = lists:member(e, ITEM),
    if
        MEMBER_O ->
            o;
        MEMBER_E ->
            e;
        true ->
            u
    end.

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
