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
