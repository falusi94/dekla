# Prolog
## A feladat

A feladat azon megengedett mezőértékek a meghatározása, amelyek a félévi nagy házi feladatban specifikált Sudoku-feladvány egy adott mezőjében előfordulhatnak.

Legyen a Sudoku-feladványban k a cellaméret. Egy adott mező értékeként megengedett egy szám, ha 1..k*k közötti egész, teljesíti az adott mezőben szereplő szám- és paritási infók által előírt megszorításokat, továbbá különbözik az adott mezőt tartalmazó sor, oszlop és cella többi mezőjében szereplő száminfók által előírt értékektől.

A feladat bemenete tehát egy Sudoku-feladvány és azon belül egy mező. A Sudoku-feladványt a nagy házi feladatban specifikált Prolog-struktúrával, illetve Erlang-párral adjuk meg. Az adott mezőt egy sor-oszlop koordinátapárral határozzuk meg, ahol a Sudoku-feladvány bal felső mezőjének koordinátája: (1,1). A feladat kimenete a megengedett mezőértékeket tartalmazó, esetleg üres lista. A lista elemeinek sorrendje tetszőleges, de ismétlődő elem nem lehet benne.

Egy Sudoku-feladvány megoldásában egy adott mezőben nyilván nem szerepelhet egy olyan érték, amely – a fenti definíció szerint – nem megengedett szám, hiszen egy ilyen érték nem tesz eleget a Sudoku megoldásokra előírt valamelyik megszorításnak. Bonyolultabb következtetésekkel más értékekről is kimutatható, hogy nem szerepelhetnek a megoldás egy adott mezőjében. Ha például egy k=2 cellaméretű Sudoku feladvány egy sorának 1. és 2. mezőjében rendre a 2 és e infók szerepelnek, akkor ennek a sornak a 3. és 4. eleméről könnyen belátható, hogy ezek csak páratlanok lehetnek, ugyanis a sorban már van két páros elem (az 1. és a 2.). Hasonló következtetésre juthatunk abban az esetben, ha a sor 2. és 3. mezőjében rendre a w és e infók szerepelnek: ekkor a sor 4. eleméről látható be, hogy az biztosan páratlan.

Fontos, hogy a kis házi feladat megoldásában a szomszédsági megszorításokat megadó s és w jeleket figyelmen kívül kell hagyni, és összetett következtetéseket nem szabad végezni: pontosan a fent leírt definíció szerint kell a megengedett értékek listáját előállítani. Ilyen eseteket mutatunk be alább, a Prolog és Erlang példafutások között: az első két példában a megengedett értékek listájában szerepelnie kell a 4 számnak, annak ellenére, hogy belátható, hogy ez az érték az adott helyen nem szerepelhet az adott feladvány megoldásában.

A házi feladat megoldása során a paraméterekre vonatkozó formai előírások meglétét nem kell ellenőriznie, azaz feltételezheti, hogy

a feladványt leíró adatstruktúra megfelel a nagy házi feladat kiírásában megadott formai követelményeknek;
az adott mezőt meghatározó sor-oszlop koordinátapár mindkét komponense az 1..k*k tartományba esik, ahol k a feladvány cellamérete.

## Prolog-specifikációk

Írjon Prolog-eljárást ertekek/3 néven, amely egy adott Sudoku-feladvány adott mezőjére előállítja a megengedett mezőértékek listáját. A szóbanforgó mezőt egy R-C struktúrával adjuk meg, ahol R a mező sorának, míg C a mező oszlopának a sorszáma, 1-től számozva.

```
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
```

## Példák
```
| ?- ertekek(
             s(2, [[[v(2)],          [],         [e],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],      [v(1)],          []],
                   [    [],         [w],          [],          []]]), 1-4, Vals).
Vals = [1,3,4] ? ;
no
| ?- ertekek(
             s(2, [[[v(2)],         [w],          [],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],      [v(1)],          []],
                   [    [],         [w],          [],          []]]), 1-2, Vals).
Vals = [1,3,4] ? ;
no
| ?- ertekek(
             s(2, [[[v(2)],          [],         [e],          []],
                   [    [],         [s],          [],         [o]],
                   [    [],          [],      [v(1)],          []],
                   [    [],         [w],          [],          []]]), 1-3, Vals).
Vals = [4] ? ;
no
| ?- ertekek(
             s(2, [[[v(2)],          [],         [e],          []],
                   [    [],         [e],          [],         [o]],
                   [    [],          [],      [v(1)],          []],
                   [    [],         [w],          [],          []]]), 2-2, Vals).
Vals = [4] ? ;
no
| ?- ertekek(
             s(2, [[[v(2)],         [w],         [e],          []],
                   [    [],          [],          [],         [o]],
                   [    [],          [],      [v(4)],          []],
                   [    [],         [w],          [],          []]]), 1-3, Vals).
Vals = [] ? ;
no
| ?- ertekek(
             s(2, [[[v(2)],         [w],         [e],          []],
                   [    [],  [e,s,v(2)],          [],         [o]],
                   [    [],          [],      [v(4)],          []],
                   [    [],         [w],          [],          []]]), 1-1, Vals).
Vals = [] ? ;
no
| ?- ertekek(
             s(3, [[[v(2)],   [],[v(9)],   [s],    [],   [s],   [s],  [e],    []],
                   [    [],  [s],[v(4)],   [w],[v(9)],[v(8)],    [],  [o],   [e]],
                   [    [],[e,w],[v(1)],   [w],    [],   [o],    [],  [w], [s,w]],
                   [    [],   [],    [],   [w],    [],   [w],    [],  [o],    []],
                   [    [],   [],    [],    [],   [w],    [],   [w],  [w],[v(8)]],
                   [    [],   [],    [],    [],    [],    [],   [o],  [o],    []],
                   [    [],   [],    [],[v(3)],    [],    [],[v(1)],  [e],   [s]],
                   [    [],   [],    [],[v(1)],    [],    [],[v(2)],   [],   [e]],
                   [    [],   [],    [],   [w],   [w],[v(2)],    [],  [w],    []]]), 3-6, Vals).
Vals = [3,5,7] ? ;
no
```
