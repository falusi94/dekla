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

ertekek({1, LIST}, {ROW,COL}) ->
    NORMALIZED = normalizeInputList({1, LIST}),
    [NUM,PAR,_] = getItem(NORMALIZED, ROW, COL),
    if
        NUM=:=1 orelse NUM=:=-1 andalso PAR=:=u orelse PAR=:=o ->
            [1];
        true ->
            []
    end;
ertekek({K, LIST}, {ROW,COL}) ->
    NORMALIZED = normalizeInputList({K, LIST}),
    ROWRESTRICTION = examineRow({K, NORMALIZED}, {ROW,COL}),
    COLRESTRICTION = examineCol({K, NORMALIZED}, {ROW,COL}),
    SQUARERESTRICTION = examineSquare({K, NORMALIZED}, {ROW,COL}),
    RESTRICTED = lists:append(lists:append(ROWRESTRICTION, COLRESTRICTION), SQUARERESTRICTION),
    CANDIDATES = removeElements( createFullList(K*K), RESTRICTED),
    [NUM,_,_] = getItem(NORMALIZED, ROW, COL),
    ISMEMBER = lists:member(NUM, CANDIDATES),
    if
        NUM>0 ->
            if
                ISMEMBER ->
                    [NUM];
                true ->
                    []
            end;
        true ->
            CANDIDATES
    end.

examineSquare({K, NORMALIZED}, {ROW,COL}) ->
    SQUARES = cutIntoArrays(NORMALIZED, K),
    INDEX = ((ROW-1) div K)*K + (COL-1) div K + 1,
    SQUARE = getSquare(SQUARES, INDEX),
    RET = examineSquare(SQUARE, ROW, COL, ((ROW-1) div K)*K+1, ((COL-1) div K)*K+1, [], K),
    io:format("SQUARES: ~p \n INDEX: ~p SQUARE: ~p\n RET: ~p\n", [SQUARES, INDEX, SQUARE, RET]),
    RET.
examineSquare([[NUM,_,_]], ROW, COL, ROWI, COLI, RET, K) ->
    io:format("NUM: ~p ROWI: ~p COLI: ~p RET: ~p ROW: ~p COL: ~p\n", [NUM,  ROWI, COLI, RET, ROW, COL]),
    if
        ROW=:=ROWI andalso COL=:=COLI ->
            RET;
        true ->
            lists:append(RET, [NUM])
    end;
examineSquare([[NUM,_,_]|TAIL], ROW, COL, ROWI, COLI, RET, K) ->
    io:format("NUM: ~p ROWI: ~p COLI: ~p RET: ~p ROW: ~p COL: ~p\n", [NUM,  ROWI, COLI, RET, ROW, COL]),
    if
        (COLI+2) div K =:= (COL+1) div K->
            COLI1 = COLI+1,
            ROWI1 = ROWI;
        true ->
            COLI1 = COLI-K+1,
            ROWI1 = ROWI+1
    end,
    if
        ROW=:=ROWI andalso COL=:=COLI ->
            examineSquare(TAIL, ROW, COL, ROWI1, COLI1, RET, K);
        true ->
            examineSquare(TAIL, ROW, COL, ROWI1, COLI1, lists:append(RET, [NUM]), K)
    end.

getSquare([HEAD|_], 1) ->
    HEAD;
getSquare([_|TAIL], INDEX) ->
    getSquare(TAIL, INDEX-1).

% Return the values that cant be in the given field because of the row
examineRow({K, NORMALIZED}, {ROW,COL}) ->
    examineRow(K*K, NORMALIZED, ROW, COL, 1, []).
examineRow(MAX, NORMALIZED, ROW, COL, INDEX, RET) ->
    if
        INDEX=:=COL ->
            [_,PAR,_] = getItem(NORMALIZED, ROW, INDEX),
            if
                PAR=:=o ->
                    RET1 = lists:append(RET, createEvenList(MAX));
                PAR=:=e ->
                    RET1 = lists:append(RET, createOddList(MAX));
                true ->
                    RET1 = RET
            end,
            examineRow(MAX, NORMALIZED, ROW, COL, INDEX+1, RET1);
        (INDEX<MAX orelse INDEX=:=MAX) ->
            [NUM,_,_] = getItem(NORMALIZED, ROW, INDEX),
            if
                NUM>0 ->
                    RET1 = lists:append(RET, [NUM]);
                true ->
                    RET1 = RET
            end,
            examineRow(MAX, NORMALIZED, ROW, COL, INDEX+1, RET1);
        true ->
            RET
    end.

% Return the values that cant be in the given field because of the col
examineCol({K, NORMALIZED}, {ROW,COL}) ->
    examineCol(K*K, NORMALIZED, ROW, COL, 1, []).
examineCol(MAX, NORMALIZED, ROW, COL, INDEX, RET) ->
    if
        INDEX=:=ROW ->
            [_,PAR,_] = getItem(NORMALIZED, INDEX, COL),
            if
                PAR=:=o ->
                    RET1 = lists:append(RET, createEvenList(MAX));
                PAR=:=e ->
                    RET1 = lists:append(RET, createOddList(MAX));
                true ->
                    RET1 = RET
            end,
            examineCol(MAX, NORMALIZED, ROW, COL, INDEX+1, RET1);
        (INDEX<MAX orelse INDEX=:=MAX) ->
            [NUM,_,_] = getItem(NORMALIZED, INDEX, COL),
            if
                NUM>0 ->
                    RET1 = lists:append(RET, [NUM]);
                true ->
                    RET1 = RET
            end,
            examineCol(MAX, NORMALIZED, ROW, COL, INDEX+1, RET1);
        true ->
            RET
    end.

% Create a list from 1 to K with even or odd numbers
createEvenList(MAX) ->
    createList(MAX, 2, []).
createOddList(MAX) ->
    createList(MAX, 1, []).
createList(MAX, INDEX, LIST) ->
    if
        INDEX<MAX orelse INDEX=:=MAX ->
            createList(MAX, INDEX+2, lists:append(LIST, [INDEX]));
        true ->
            LIST
    end.

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
    [NUMBER, PARITY, DIRECTION].

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
removeElements(LIST, [] ) ->
    LIST;
removeElements(LIST, [HEAD]) ->
    lists:delete(HEAD, LIST);
removeElements(LIST, [HEAD|TAIL]) ->
    removeElements(lists:delete(HEAD, LIST), TAIL).

% Create list with every possible values
createFullList(MAX) ->
    createFullList(MAX, [], 1).
createFullList(MAX, LIST, INDEX) ->
    if
        INDEX<MAX orelse INDEX=:=MAX ->
            createFullList(MAX, lists:append(LIST, [INDEX]), INDEX+1);
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

% Create
cutIntoArrays(LIST, K) ->
    cutIntoArrays(LIST, K, K, 1, 1, 1, 1, K*K, K*K, 1, [], []).
cutIntoArrays(L, R, C, RI, CI, J, K, CMAX, RMAX, ASD, LIST, RET) ->
    NEW = (mod(ASD,R*C) =:= 0) or
            ( ((CI =:= CMAX) and (mod(RI,R) =:= 0)) or ((RI =:= RMAX) and (mod(CI,C) =:= 0)))
            or (RI*CI=:=RMAX*CMAX),
    if
        NEW ->
            ELEMENT = getItem( L, RI, CI),
            LIST1 = [],
            ASD1=0,
            RET1 = lists:append( RET, [lists:append(LIST, [ELEMENT])]);
        true ->
            RET1 = RET,
            ASD1=ASD,
            ELEMENT = getItem( L, RI, CI),
            LIST1 = lists:append(LIST, [ELEMENT])
    end,
    if
        CI=:=CMAX ->
            if
                RI=:=RMAX ->
                    RET1;
                RI=:=(K*R) ->
                    cutIntoArrays(L, R, C, RI+1, 1, 1, K+1, CMAX, RMAX, ASD1+1, LIST1, RET1);
                true ->
                     cutIntoArrays(L, R, C, RI+1, (J-1)*C+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
             end;
        CI=:=(J*C) ->
            if
                RI=:=RMAX ->
                    cutIntoArrays(L, R, C, (K-1)*R+1, CI+1, J+1, K, CMAX, RMAX, ASD1+1, LIST1, RET1);
                RI=:=(K*R) ->
                    cutIntoArrays(L, R, C, (K-1)*R+1, CI+1, J+1, K, CMAX, RMAX, ASD1+1, LIST1, RET1);
                true ->
                    cutIntoArrays(L, R, C, RI+1, (J-1)*C+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
            end;
         true ->
             cutIntoArrays(L, R, C, RI, CI+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
    end.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.
