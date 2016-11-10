% :- type col  == int.
% :- type row  == int.
% :- type coords -->row-col.
% :- pred ertekek(sspec::in, coords::in, list(int)::out).
% ertekek(SSpec, R_C, Vals):
% Egy érték pontosan akkor szerepel a Vals listában, ha:
%    (a) 1..k*k közötti egész, ahol k az SSpec feladvány cellamérete,
%    (b) teljesíti az adott mezőre vonatkozó szám- és paritási infók
%        által előírt megszorításokat, továbbá
%    (c) különbözik az adott mezőt tartalmazó sor, oszlop és cella többi
%        mezőjében szereplő száminfóktól,
% ahol
%    SSpec az sspec típusspecifikációnak megfelelő Sudoku-feladvány,
%    R_C az adott feladvány egy mezőjének (sor-oszlop formában megadott) koordinátája,
%    Vals list(int) típusú mezőértéklista, az SSpec feladvány R_C koordinátájú
%         mezőjében megengedett értékek listája.

%ertekek(MATRIX, R-C, RET) :-
%    .


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
