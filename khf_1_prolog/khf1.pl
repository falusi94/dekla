% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).
% feldarabolasa(Mx, P, LL): Az LL lista az Mx mátrix P paraméterű feldarabolása.

%feldarabolasa(MATRIX, PARAM, LIST) :-
%    .

makeRows(LIST, PARAM, RET) :-
    makeRows(LIST, PARAM, 1, [], [], RET).
makeRows([HEAD|TAIL], PARAM, INDEX, TEMP, ASD, RET) :-
    PARAM =:= INDEX ->
        append(TEMP, [HEAD], LIST),
        append(ASD, [LIST], RET1),
        makeRows(TAIL, PARAM, 1, [], RET1, RET);

        append(TEMP, [HEAD], LIST),
        INDEX1 is INDEX + 1,
        makeRows(TAIL, PARAM, INDEX1, LIST, ASD, RET).
makeRows([HEAD], _, _, TEMP, ASD, RET) :-
    append(TEMP, [HEAD], TEMP1),
    append(ASD, [TEMP1], RET).

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

% Returns the column and row number of matrix
sizeOfMatrix([HEAD|TAIL], ROWS, COLS) :-
    countElements(HEAD, COLS, 0),
    countElements(TAIL, ROWT, 0),
    ROWS is ROWT+1.
countElements([], INDEX, INDEX).
countElements([_|TAIL], COUNT, INDEX) :-
    NEXT is INDEX+1,
    countElements(TAIL, COUNT, NEXT).

% Create R*C size index matrix
getIndex(ROW, COL, R, C, RET) :-
    TEMP=[],
    getIndex(ROW, COL, R, C, 0, TEMP, RET).
getIndex(ROW, COL, R, C, INDEX, TEMP, RET) :-
    RC is R*C,
    INDEX=:=RC ->
        RET = TEMP,
        true;
    mod(COL, C, X),
    X=:=0 ->
        ROW1 is ROW+1,
        COLPARAM is COL-C+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW1, COLPARAM, R, C, INDEX1, TEMP1, RET);
    %else
        COL1 is COL+1,
        INDEX1 is INDEX+1,
        append(TEMP, [[ROW, COL]], TEMP1),
        getIndex(ROW, COL1, R, C, INDEX1, TEMP1, RET).

% Create R*C size squares from MATRIX
makeArrays(MATRIX, R, C, RET) :-
    TEMP=[],
    sizeOfMatrix(MATRIX, RMAX, CMAX),
    makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP, 1, 1, RET).
makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP, ROWK, COLK, RET):-
    ROWKMAX is RMAX div R,
    ROWK>ROWKMAX ->
        RET=TEMP,
        true;
    COLK=:=C ->
        ROWK1 is ROWK+1,
        makeArraysHelper(ROWK, COLK, R, C, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP1, ROWK1, 1, RET);
    %else
        COLK1 is COLK+1,
        makeArraysHelper(ROWK, COLK, R, C, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP1, ROWK, COLK1, RET).
makeArraysHelper(ROWK, COLK, R, C, MATRIX, IN, RET) :-
    ROWB is 1+(ROWK-1)*R,
    COLB is 1+(COLK-1)*C,
    getIndex(ROWB, COLB, R, C, INDEXMATRIX),
    swapIndexesForElements(INDEXMATRIX, MATRIX, ELEMENTMATRIX),
    append(IN, [ELEMENTMATRIX], RET).

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

% Modulo function
mod(X, Y, RET) :-
    X=:=0 ->
        RET is 0;
    X>0 ->
        RET is X rem Y;
    %else
        RET is Y + X rem Y.
