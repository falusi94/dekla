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
    print('asd3'),
    append(TEMP, [HEAD], TEMP1),
    append(ASD, [TEMP1], RET).
