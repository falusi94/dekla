% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).
% feldarabolasa(Mx, P, LL): Az LL lista az Mx mátrix P paraméterű feldarabolása.

feldarabolasa(MATRIX, R-C, RET) :-
    makeArrays(MATRIX, R, C, RET).

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

% Create Indexes
getIndex(ROW, COL, R, C, MATRIX, RET) :-
    TEMP=[],
    sizeOfMatrix(MATRIX, RMAX, CMAX),
    getIndex(ROW, COL, R, C, RMAX, CMAX, 0, TEMP, RET).
getIndex(ROW, COL, R, C, RMAX, CMAX, INDEX, TEMP, RET) :-
    RC is R*C,
    INDEX=:=RC ->
        RET = TEMP,
        true;
    mod(COL, C, X),
    X=:=0 ->
        ROW1 is ROW+1,
        COLPARAM is COL-C+1,
        INDEX1 is INDEX+1,
        getIndexHelper(ROW, COL, RMAX, CMAX, TEMP, TEMP1),
        getIndex(ROW1, COLPARAM, R, C, RMAX, CMAX, INDEX1, TEMP1, RET);
    %else
        COL1 is COL+1,
        INDEX1 is INDEX+1,
        getIndexHelper(ROW, COL, RMAX, CMAX, TEMP, TEMP1),
        getIndex(ROW, COL1, R, C, RMAX, CMAX, INDEX1, TEMP1, RET).
getIndexHelper(ROW, COL, RMAX, CMAX, IN, OUT) :-
    RMAX<ROW ->
        OUT = IN;
    CMAX<COL ->
        OUT = IN;
    append(IN, [[ROW, COL]], OUT).

% Create R*C size squares from MATRIX
%makeArrays(MATRIX, R, C, RET) :-
%    TEMP=[],
%    sizeOfMatrix(MATRIX, RMAX, CMAX),
%    %ROWKMAX is RMAX / R + 1,
%    makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP, 1, 1, RET).
%makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP, ROWK, COLK, RET):-
%    ROWKMAX is RMAX div R + 1,
%    print(a),
%    ROWK>ROWKMAX ->
%        print(a),
%        RET=TEMP,
%        true;
%    (COLK=:=C ->
%        print(b),
%        ROWK1 is ROWK+1,
%        makeArraysHelper(ROWK, COLK, R, C, RMAX, CMAX, MATRIX, TEMP, TEMP1),
%        makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP1, ROWK1, 1, RET));
%    %else
%        COLK1 is COLK+1,
%        print(c),
%        makeArraysHelper(ROWK, COLK, R, C, RMAX, CMAX, MATRIX, TEMP, TEMP1),
%        makeArrays(MATRIX, R, C, RMAX, CMAX, TEMP1, ROWK, COLK1, RET).
%makeArraysHelper(ROWK, COLK, R, C, RMAX, CMAX, MATRIX, IN, RET) :-
%    ROWB is 1+(ROWK-1)*R,
%    COLB is 1+(COLK-1)*C,
%    getIndex(ROWB, COLB, R, C, MATRIX, INDEXMATRIX),
%    swapIndexesForElements(INDEXMATRIX, MATRIX, RMAX, CMAX, ELEMENTMATRIX),
%    append(IN, [ELEMENTMATRIX], RET).

% Create PARAM*PARAM size squares from MATRIX
makeArrays(MATRIX, R, C, RET) :-
    TEMP=[],
    sizeOfMatrix(MATRIX, ROW, COL),
    COLKMAX is COL div C + 1,
    ROWKMAX is ROW div R + 1,
    makeArrays(MATRIX, R, C, TEMP, 1, 1, ROWKMAX, COLKMAX, RET).
makeArrays(MATRIX, R, C, TEMP, ROWK, COLK, ROWKMAX, COLKMAX, RET):-
    ROWK>ROWKMAX ->
        RET=TEMP,
        true;
    (COLK=:=COLKMAX ->
        ROWK1 is ROWK+1,
        makeArraysHelper(ROWK, COLK, R, C, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, R, C, TEMP1, ROWK1, 1, ROWKMAX, COLKMAX, RET));
    %else
        COLK1 is COLK+1,
        makeArraysHelper(ROWK, COLK, R, C, MATRIX, TEMP, TEMP1),
        makeArrays(MATRIX, R, C, TEMP1, ROWK, COLK1, ROWKMAX, COLKMAX, RET).
makeArraysHelper(ROWK, COLK, R, C, MATRIX, IN, RET) :-
    ROWB is 1+(ROWK-1)*R,
    COLB is 1+(COLK-1)*C,
    getIndex(ROWB, COLB, R, C, MATRIX, INDEXMATRIX),
    swapIndexesForElements(INDEXMATRIX, MATRIX, ELEMENTMATRIX),
    notNullAppend(IN, [ELEMENTMATRIX], RET).

% Swap indexes to elements from MATRIX
swapIndexesForElements(INDEXMATRIX, MATRIX, RET) :-
    TEMP=[],
    sizeOfMatrix(MATRIX, RMAX, CMAX),
    swapIndexesForElements(INDEXMATRIX, MATRIX, RMAX, CMAX, TEMP, RET).
swapIndexesForElements([[ROW,COL]|TAIL], MATRIX, RMAX, CMAX, TEMP, RET) :-
    RMAX>=ROW ->
        (CMAX>=COL ->
            getItem(MATRIX, ROW, COL, ELEMENT),
            notNullAppend(TEMP, [ELEMENT], TEMP1),
            swapIndexesForElements(TAIL, MATRIX, RMAX, CMAX, TEMP1, RET));
    %else
        swapIndexesForElements(TAIL, MATRIX, RMAX, CMAX, TEMP, RET).
swapIndexesForElements([], _, _, _, TEMP, RET) :-
    RET = TEMP,
    true.

% Append [ELEMENT] to IN if it is not [[]]
notNullAppend(IN, [ELEMENT], OUT) :-
    isEmpty(ELEMENT) ->
        OUT = IN;
    append(IN, [ELEMENT], OUT).

isEmpty([]).

% Modulo function
mod(X, Y, RET) :-
    X=:=0 ->
        RET is 0;
    X>0 ->
        RET is X rem Y;
    %else
        RET is Y + X rem Y.
