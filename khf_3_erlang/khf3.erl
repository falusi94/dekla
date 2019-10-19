-module(khf3).
-author('falusi.david94@gmail.com').
-vsn('year-mm-dd').
-export([megoldase/2]).

%% @spec khf3:megoldase(SSpec::sspec(), SSol::ssol()) -> B::bool().
%% B igaz, ha SSol megoldĂĄsa az SSpec feladvĂĄnynak.
megoldase({PARAM, MATRIX}, SOLUTION) ->
    NORMALIZED = normalizeInputList({PARAM, MATRIX}),
    NORMALIZEDSOLUTIN = normalizeInputList({PARAM, SOLUTION}),
    %io:format(" ~p", [SOLUTION]),
    %io:format(" ~p", [getCols(SOLUTION, PARAM)]),
    checkRows(SOLUTION, PARAM) andalso
    checkCols(SOLUTION, PARAM) andalso
    checkIntegrity(NORMALIZED, SOLUTION).

checkIntegrity([ROW1| TAIL1], [ROW2|TAIL2]) ->
    RET = checkRowIntegrity(ROW1, ROW2),
    if
        RET ->
            checkIntegrity(TAIL1, TAIL2);
        true ->
            false
    end;
checkIntegrity([], []) -> true .

checkRowIntegrity([[NUMBER, PARITY, WEST, SOUTH]|TAIL1], [VALUE|TAIL2]) ->
    RET =
        (NUMBER=:= -1 orelse NUMBER=:=VALUE) andalso
        (PARITY=:=u
            orelse (PARITY=:=e andalso mod(VALUE, 2)=:=0)
            orelse (PARITY=:=o andalso mod(VALUE, 2)=:=1)),
    if
        RET ->
            checkRowIntegrity(TAIL1, TAIL2);
        true ->
            false
    end;
checkRowIntegrity([], []) -> true.

checkRows([SOLROW|TAIL], PARAM) ->
    RET = checkFullList(SOLROW, PARAM),
    if
        RET ->
            checkRows(TAIL, PARAM);
        true ->
            false
    end;
checkRows([], _) -> true.

checkCols(MATRIX, PARAM) ->
    COLS = getCols(MATRIX, PARAM),
%    io:format(" ~p", [COLS]),
    checkRows(COLS, PARAM).

% Get columns from SOLUTION matrix
getCols(MATRIX, PARAM) ->
    getCols(MATRIX, 1, PARAM*PARAM, []).
getCols(MATRIX, COL, PARAM2, TEMP) ->
    if
        PARAM2<COL ->
            TEMP;
        true ->
            COLUMN = getCol(MATRIX, 1, COL, PARAM2, []),
            getCols(MATRIX, COL+1, PARAM2, lists:append(TEMP, [COLUMN]))
    end.
getCol(MATRIX, ROW, COL, PARAM2, TEMP) ->
    if
        PARAM2<ROW ->
            TEMP;
        true ->
            ITEM = getItem(MATRIX, ROW, COL),
            getCol(MATRIX, ROW+1, COL, PARAM2, lists:append(TEMP, [ITEM]))
    end.

% Check if every number from 1..PARAM*PARAM is in the list
checkFullList(LIST, PARAM) ->
    checkFullListHelper(LIST, PARAM*PARAM).
checkFullListHelper(LIST, INDEX) ->
    RET = lists:member(INDEX, LIST),
    if
        RET ->
            checkFullListHelper(LIST, INDEX-1);
        INDEX<1 ->
            true;
        true ->
            false
    end.

% Select the needed square from squares
getSquare([HEAD|_], 1) ->
    HEAD;
getSquare([_|TAIL], INDEX) ->
    getSquare(TAIL, INDEX-1).

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
    NUMBER = number(ITEM),
    [NUMBER, PARITY, lists:member(w, ITEM), lists:member(s, ITEM)].

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
