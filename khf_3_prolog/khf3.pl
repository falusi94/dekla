% :- pred megoldase(sspec::in, ssol::in).
% megoldase(+SSpec,+SSol) : sikeres, ha az SSol érték-mátrix megoldása az SSpec Sudoku-feladványnak.
megoldase(s(PARAM, MATRIX), SOLUTION) :-
    check(s(PARAM, MATRIX), SOLUTION), !.
megoldase(_,_) :-
    fail,!.

check(s(PARAM, MATRIX), SOLUTION) :-
    normalizeInput(MATRIX, NORMALIZED).

% Returns Rth row
getRow(MATRIX, R, ROW) :-
    getNth(MATRIX, R, ROW).

% Returns Cth column
getCol(MATRIX, C, COL) :-
    getCol(MATRIX, C, [], COL).
getCol([ROW|TAIL], C, TEMP, COL) :-
    getNth(ROW, C, ELEMENT),
    append(TEMP, [ELEMENT], TEMP1),
    getCol(TAIL, C, TEMP1, COL).
getCol([], _, COL, COL).

% Returns the NTH element from list
getNth([HEAD|_], 1, HEAD).
getNth([_|TAIL], NTH, RET) :-
    NEXT is NTH-1,
    getNth(TAIL, NEXT, RET).

% Examine list
examine(NORMALIZED, NTH, RESTRICTED) :-
    examine(NORMALIZED, NTH, [], RESTRICTED).
examine([[NUMBER,_,_,_]|TAIL], NTH, TEMP, RESTRICTED) :-
    NTH=:=1 ->
        NEXT is NTH-1,
        examine(TAIL, NEXT, TEMP, RESTRICTED);
    %else
        NUMBER> -1 ->
            NEXT is NTH-1,
            append(TEMP, [NUMBER], TEMP1),
            examine(TAIL, NEXT, TEMP1, RESTRICTED);
    %else
        NEXT is NTH-1,
        examine(TAIL, NEXT, TEMP, RESTRICTED).
examine([], _, RESTRICTED, RESTRICTED).

% Return an item from given matrix structure
getItem([HEAD|TAIL], ROW, COL, RET) :-
    ROW=:=1 ->
        getItem(HEAD, COL-1, RET);
    ROW>0 ->
        getItem(TAIL, ROW-1, COL, RET).
getItem([HEAD|TAIL], COL, RET) :-
    COL=:=0 ->
        RET = HEAD;
    COL>0 ->
        getItem(TAIL, COL-1, RET).

% Create Indexes
getIndex(ROW, COL, PARAM, RET) :-
    TEMP=[],
    getIndex(ROW, COL, PARAM, 0, TEMP, RET).
getIndex(ROW, COL, PARAM, INDEX, TEMP, RET) :-
    PARAM2 is PARAM*PARAM,
    INDEX=:=PARAM2 ->
        RET = TEMP,
        true;
    mod(COL, PARAM, X),
    X=:=0 ->
        ROW1 is ROW+1,
        COLPARAM is COL-PARAM+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW1, COLPARAM, PARAM, INDEX1, TEMP1, RET);
    %else
        COL1 is COL+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW, COL1, PARAM, INDEX1, TEMP1, RET).

% Swap indexes to elements from MATRIX
swapIndexesForElements(INDEXMATRIX, MATRIX, RET) :-
    TEMP=[],
    swapIndexesForElements(INDEXMATRIX, MATRIX, TEMP, RET).
swapIndexesForElements([[ROW,COL]|TAIL], MATRIX, TEMP, RET) :-
    getItem(MATRIX, ROW, COL, ELEMENT),
    append(TEMP, [ELEMENT], TEMP1),
    swapIndexesForElements(TAIL, MATRIX, TEMP1, RET).
swapIndexesForElements([], _, TEMP, RET) :-
    RET = TEMP,
    true.

% Create PARAM*PARAM size squares from MATRIX
makeArrays(MATRIX, PARAM, RET) :-
    TEMP=[],
    makeArrays(MATRIX, PARAM, TEMP, 1, 1, RET).
makeArrays(MATRIX, PARAM, TEMP, ROWK, COLK, RET):-
    ROWK>PARAM ->
        RET=TEMP,
        true;
    COLK=:=PARAM ->
        ROWK1 is ROWK+1,
        makeArraysHelper(ROWK, COLK, PARAM, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, PARAM, TEMP1, ROWK1, 1, RET);
    %else
        COLK1 is COLK+1,
        makeArraysHelper(ROWK, COLK, PARAM, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, PARAM, TEMP1, ROWK, COLK1, RET).
makeArraysHelper(ROWK, COLK, PARAM, MATRIX, IN, RET) :-
    ROWB is 1+(ROWK-1)*PARAM,
    COLB is 1+(COLK-1)*PARAM,
    getIndex(ROWB, COLB, PARAM, INDEXMATRIX),
    swapIndexesForElements(INDEXMATRIX, MATRIX, ELEMENTMATRIX),
    append(IN, [ELEMENTMATRIX], RET).

% Change items to tuples {num, even/odd, south/west}
normalizeInput(MATRIX, RET) :-
    TEMP=[],
    normalizeRows(MATRIX, TEMP, RET).
normalizeRows([ROW|TAIL], TEMP, RET) :-
    normalizeRow(ROW, NORMALIZED),
    append(TEMP, [NORMALIZED], TEMP1),
    normalizeRows(TAIL, TEMP1, RET).
normalizeRows([], TEMP, TEMP).
normalizeRow(ROW, RET) :-
    TEMP=[],
    normalizeRow(ROW, ROW, TEMP, RET).
normalizeRow([HEAD|TAIL], ROW, TEMP, RET) :-
    createTuple(HEAD, ELEMENT),
    append(TEMP, [ELEMENT], TEMP1),
    normalizeRow(TAIL, ROW, TEMP1, RET).
normalizeRow([], _, TEMP, RET) :-
    RET = TEMP,
    true.

% Create tuple from an 'item' (field)
createTuple(ITEM, [NUMBER, PARITY, SOUTH, WEST]) :-
    getEvenOrOdd(ITEM, PARITY),
    isSouth(ITEM, SOUTH),
    isWest(ITEM, WEST),
    getNumber(ITEM, NUMBER), true.

% True if value satisfy the input form v(value)
isValue(v(_)).

% Gives back the number if exists in item, else -1
getNumber([HEAD|TAIL], NUMBER) :-
    isValue(HEAD) ->
        getNumberHelper(HEAD, NUMBER),
        true;
    %else
    getNumber(TAIL, NUMBER).
getNumber([HEAD], NUMBER) :-
    isValue(HEAD) ->
        getNumberHelper(HEAD, NUMBER);
    %else
        NUMBER = -1.
getNumber([], -1).
getNumberHelper(v(NUM), NUM).

% Gives back proper direction, if given, else return u (unknown)
isSouth(ITEM, SOUTH) :-
    member(s, ITEM) ->
        SOUTH=true;
    SOUTH=false.
isWest(ITEM, WEST) :-
    member(w, ITEM) ->
        WEST=true;
    WEST=false.

% Gives back proper parity, if given, else return u (unknown)
getEvenOrOdd(ITEM, PARITY) :-
    member(o, ITEM) ->
        PARITY=o;
    member(e, ITEM) ->
        PARITY=e;
    %else
        PARITY=u.

% Modulo function
mod(X, Y, RET) :-
    X=:=0 ->
        RET is 0;
    X>0 ->
        RET is X rem Y;
    %else
        RET is Y + X rem Y.
