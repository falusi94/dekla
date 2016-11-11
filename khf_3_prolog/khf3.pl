% :- pred megoldase(sspec::in, ssol::in).
% megoldase(+SSpec,+SSol) : sikeres, ha az SSol érték-mátrix megoldása az SSpec Sudoku-feladványnak.
megoldase(s(PARAM2, MATRIX), SOLUTION) :-
    normalizeInput(MATRIX, NORMALIZED),
    checkIntegrity(NORMALIZED, SOLUTION),!.

% Get rows from SOLUTION matrix
getRows(LIST, PARAM, RET) :-
    PARAM2 is PARAM*PARAM,
    getRows(LIST, PARAM2, 1, [], [], RET).
getRows([HEAD|TAIL], PARAM2, INDEX, TEMP, ASD, RET) :-
    PARAM2 =:= INDEX ->
        append(TEMP, [HEAD], LIST),
        append(ASD, [LIST], RET1),
        getRows(TAIL, PARAM2, 1, [], RET1, RET);
    %else
        append(TEMP, [HEAD], LIST),
        INDEX1 is INDEX + 1,
        getRows(TAIL, PARAM2, INDEX1, LIST, ASD, RET).
getRows([HEAD], _, _, TEMP, ASD, RET) :-
    append(TEMP, [HEAD], TEMP1),
    append(ASD, TEMP1, RET).

% Get columns from SOLUTION matrix
getCols(MATRIX, PARAM, RET) :-
    PARAM2 is PARAM*PARAM,
    getCols(MATRIX, 1, PARAM2, [], RET).
getCols(MATRIX, COL, PARAM2, TEMP, RET) :-
    PARAM2<COL ->
        RET = TEMP;
    %else
        getCol(MATRIX, 1, COL, PARAM2, [], ASD),
        append(TEMP, [ASD], TEMP1),
        COLNEXT is COL+1,
        getCols(MATRIX, COLNEXT, PARAM2, TEMP1, RET).
getCol(MATRIX, ROW, COL, PARAM2, TEMP, RET) :-
    PARAM2<ROW ->
        RET = TEMP;
    %else
        getItem(MATRIX, ROW, COL, ITEM),
        append(TEMP, [ITEM], TEMP1),
        ROWNEXT is ROW+1,
        getCol(MATRIX, ROWNEXT, COL, PARAM2, TEMP1, RET).

% Check if every value in solution is meet the number and parity requirement from matrix
checkIntegrity([ [ [NUMBER, PARITY, _, _] | TAIL1 ] |NORMTAIL], [[VALUE|TAIL2]|SOLTAIL]) :-
    checkRowIntegrity([ [NUMBER, PARITY, _, _]|TAIL1], [VALUE|TAIL2]) ->
        checkIntegrity(NORMTAIL, SOLTAIL);
    %else
        fail,!.
checkIntegrity([],[]).

% Check a row's integrity
checkRowIntegrity([[NUMBER, PARITY, _, _]|TAIL1], [VALUE|TAIL2]) :-
    checkParity(PARITY, VALUE)->
        (checkNumber(NUMBER, VALUE) ->
            checkRowIntegrity(TAIL1, TAIL2));
    %else
        false.
checkRowIntegrity([],[]).

% Returns true if value meet the number requirement
checkNumber(NUMBER, VALUE) :-
    or( (NUMBER=:= -1), NUMBER=:=VALUE).

% Check VALUE meet the parity info
checkParity(o, VALUE) :-
    mod(VALUE, 2, RET),
    RET=:=1.
checkParity(e, VALUE) :-
    mod(VALUE, 2, RET),
    RET=:=0.
checkParity(u, _).

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
getIndex(ROW, COL, PARAM2, RET) :-
    TEMP=[],
    getIndex(ROW, COL, PARAM2, 0, TEMP, RET).
getIndex(ROW, COL, PARAM2, INDEX, TEMP, RET) :-
    PARAM22 is PARAM2*PARAM2,
    INDEX=:=PARAM22 ->
        RET = TEMP,
        true;
    mod(COL, PARAM2, X),
    X=:=0 ->
        ROW1 is ROW+1,
        COLPARAM2 is COL-PARAM2+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW1, COLPARAM2, PARAM2, INDEX1, TEMP1, RET);
    %else
        COL1 is COL+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW, COL1, PARAM2, INDEX1, TEMP1, RET).

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

% Create PARAM2*PARAM2 size squares from MATRIX
getArrays(MATRIX, PARAM2, RET) :-
    TEMP=[],
    getArrays(MATRIX, PARAM2, TEMP, 1, 1, RET).
getArrays(MATRIX, PARAM2, TEMP, ROWK, COLK, RET):-
    ROWK>PARAM2 ->
        RET=TEMP,
        true;
    COLK=:=PARAM2 ->
        ROWK1 is ROWK+1,
        getArraysHelper(ROWK, COLK, PARAM2, MATRIX, TEMP, TEMP1),
        getArrays(MATRIX, PARAM2, TEMP1, ROWK1, 1, RET);
    %else
        COLK1 is COLK+1,
        getArraysHelper(ROWK, COLK, PARAM2, MATRIX, TEMP, TEMP1),
        getArrays(MATRIX, PARAM2, TEMP1, ROWK, COLK1, RET).
getArraysHelper(ROWK, COLK, PARAM2, MATRIX, IN, RET) :-
    ROWB is 1+(ROWK-1)*PARAM2,
    COLB is 1+(COLK-1)*PARAM2,
    getIndex(ROWB, COLB, PARAM2, INDEXMATRIX),
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

% OR function
or(A, B) :-
    A ->
        true;
        B ->
            true;
        false.
