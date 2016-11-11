% :- pred megoldase(sspec::in, ssol::in).
% megoldase(+SSpec,+SSol) : sikeres, ha az SSol érték-mátrix megoldása az SSpec Sudoku-feladványnak.
megoldase(s(PARAM, MATRIX), SOLUTION) :-
    checkRows(SOLUTION, PARAM),
    checkCols(SOLUTION, PARAM),
    checkSquares(SOLUTION, PARAM),
    normalizeInput(MATRIX, NORMALIZED),
    checkIntegrity(NORMALIZED, SOLUTION),
    !.

% Check every row has all number from 1 to PARAM*PARAM
checkSquares(MATRIX, PARAM) :-
    makeArrays(MATRIX, PARAM, SQUARES),
    checkHelper(SQUARES, PARAM).

% Check every row has all number from 1 to PARAM*PARAM
checkRows(MATRIX, PARAM) :-
    checkHelper(MATRIX, PARAM).

% Check every column has all number from 1 to PARAM*PARAM
checkCols(MATRIX, PARAM) :-
    getCols(MATRIX, PARAM, COLS),
    checkHelper(COLS, PARAM).

% Check helper, checkFullList on every list
checkHelper([HEAD|TAIL], PARAM) :-
    checkFullList(HEAD, PARAM) ->
        checkHelper(TAIL, PARAM).
checkHelper([], _).

% Check if every number from 1..PARAM*PARAM is in the list
checkFullList(LIST, PARAM) :-
    PARAM2 is PARAM*PARAM,
    checkFullListHelper(LIST, PARAM2).
checkFullListHelper(LIST, INDEX) :-
    member(INDEX, LIST) ->
        INDEX1 is INDEX-1,
        checkFullListHelper(LIST, INDEX1);
    INDEX<1 ->
        true;
    %else
        fail, !.

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
checkIntegrity(NORMALIZED, SOLUTION) :-
    checkIntegrity(NORMALIZED, SOLUTION, SOLUTION, 1).
checkIntegrity([ [ [NUMBER, PARITY, SOUTH, WEST] | TAIL1 ] |NORMTAIL], [[VALUE|TAIL2]|SOLTAIL], SOLUTION, ROW) :-
    checkRowIntegrity([ [NUMBER, PARITY, SOUTH, WEST]|TAIL1], [VALUE|TAIL2], SOLUTION, ROW) ->
        ROW1 is ROW+1,
        checkIntegrity(NORMTAIL, SOLTAIL, SOLUTION, ROW1);
    %else
        fail,!.
checkIntegrity([],[], _, _).

% Check a row's integrity
checkRowIntegrity(NORMROW, SOLROW, SOLUTION, ROW) :-
    checkRowIntegrity(NORMROW, SOLROW, SOLUTION, ROW, 1).
checkRowIntegrity([[NUMBER, PARITY, SOUTH, WEST]|TAIL1], [VALUE|TAIL2], SOLUTION, ROW, COL) :-
    checkParity(PARITY, VALUE)->
        (checkNumber(NUMBER, VALUE) ->
            checkWest(NUMBER, WEST, ROW, COL, SOLUTION) ->
                checkSouth(NUMBER, SOUTH, ROW, COL, SOLUTION) ->
                    COL1 is COL+1,
                    checkRowIntegrity(TAIL1, TAIL2, SOLUTION, ROW, COL1));
    %else
        false, !.
checkRowIntegrity([],[], _, _, _).

% Check if 'west' is true, the sum of number and the one on the left is odd
checkWest(_, false, _, _, _).
checkWest(NUMBER, true, ROW, COL, SOLUTION) :-
    COL1 is COL-1,
    getItem(SOLUTION, ROW, COL1, VALUE),
    SUM is NUMBER+VALUE,
    mod(SUM, 2, RET),
    RET=:=1.

% Check if 'south' is true, the sum of number and the one under it is odd
checkSouth(_, false, _, _, _).
checkSouth(NUMBER, true, ROW, COL, SOLUTION) :-
    ROW1 is ROW+1,
    getItem(SOLUTION, ROW1, COL, VALUE),
    SUM is NUMBER+VALUE,
    mod(SUM, 2, RET),
    RET=:=1.

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
getArrays(MATRIX, PARAM, RET) :-
    TEMP=[],
    getArrays(MATRIX, PARAM, TEMP, 1, 1, RET).
getArrays(MATRIX, PARAM, TEMP, ROWK, COLK, RET):-
    ROWK>PARAM ->
        RET=TEMP,
        true;
    COLK=:=PARAM ->
        ROWK1 is ROWK+1,
        getArraysHelper(ROWK, COLK, PARAM, MATRIX, TEMP, TEMP1),
        getArrays(MATRIX, PARAM, TEMP1, ROWK1, 1, RET);
    %else
        COLK1 is COLK+1,
        getArraysHelper(ROWK, COLK, PARAM, MATRIX, TEMP, TEMP1),
        getArrays(MATRIX, PARAM, TEMP1, ROWK, COLK1, RET).
getArraysHelper(ROWK, COLK, PARAM, MATRIX, IN, RET) :-
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

% OR function
or(A, B) :-
    A ->
        true;
        B ->
            true;
        false.
