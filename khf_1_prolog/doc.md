# A feladat

A feladat egy mátrix kisebb mátrixokra való feldarabolása, és a kis mátrixok elemeit tartalmazó listák előállítása.

A feladat paramétere egy (R,C) számpár, ahol R, ill. C a kis mátrixok sorainak, ill. oszlopainak a számát adja meg.

Egy M mátrix (R,C) paraméterű feldarabolását a következőképpen végezzük el:

Az első R sor után húzunk egy vízszintes elválasztó vonalat, majd minden további R sor után is.
Az első C oszlop után húzunk egy függőleges elválasztó vonalat, majd minden további C oszlop után is.
Az elválasztó vonalak által határolt minden egyes, nem üres kis mátrix elemeiből sorfolytonosan egy-egy listát képezünk.
A kis mátrixok elemeiből az előző pont szerint képzett listákat egyetlen listába gyűjtjük össze, tetszőleges sorrendben.
Az így előállított listák listáját nevezzük az M mátrix (R,C) paraméterű feldarabolásának.

Írjon olyan Prolog-eljárást, illetve Erlang-függvényt, amely előállítja egy M mátrix (R,C) paraméterű feldarabolását!

A mátrixot – mindkét nyelven – sorok listájaként adjuk meg; az első listaelem felel meg a mátrix első sorának s.í.t. A mátrix minden egyes sorát az adott sorban levő mátrixelemek listájaként ábrázoljuk; a lista első eleme tartalmazza az adott sor első elemét s.í.t.

Feltételezheti (tehát nem kell ellenőriznie), hogy a mátrix minden sora azonos számú elemből áll; ezt a számot nevezzük a mátrix oszlopszámának. Feltételezheti, hogy a mátrix sorainak és oszlopainak a száma legalább 1. Végül feltételezheti, hogy a feldarabolás paraméterében megadott R és C mennyiségek pozitív egész számok (azaz R,C≥1).

A feldarabolás eredménye egy olyan lista, amelynek elemei a bemenetként megadott mátrix elemeiből képzett, nem üres listák. Az utóbbi listák hossza nem feltétlenül egyezik meg.

# Prolog-specifikációk

Írjon Prolog-eljárást feldarabolasa/3 néven egy tetszőleges mátrix adott paraméterű feldarabolására! A mátrix elemei tetszőleges Prolog-kifejezések lehetnek.

A feldarabolás paraméterét a \`-´ operátorral képzett R-C Prolog-kifejezéssel adjuk meg.

A feldarabolasa/3 első, bemenő argumentuma a mátrix, második, bemenő argumentuma a feldarabolás paramétere, míg harmadik, kimenő argumentuma a feldarabolás eredménye.

A feldarabolasa/3 eljárás argumentumaival kapcsolatos elvárásokat az alábbi típuskommentekben formálisan is leírjuk (a Mercury típusleíró nyelvének segítségével), majd megadjuk az eljárás fejkommentjét.

```
% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).
% feldarabolasa(Mx, P, LL): Az LL lista az Mx mátrix P paraméterű feldarabolása.
```
# Prolog-példák

```
| ?- feldarabolasa([[a,b,  c,d],
                    [e,f,  g,h]], 2-2, LL).
LL = [[a,b,e,f],
      [c,d,g,h]] ? ;
no
| ?- feldarabolasa([[a,b,  c,d],
                    [e,f,  g,h],

		    [i,j,  k,l],
                    [m,n,  o,p]], 2-2, LL).
LL = [[a,b,e,f],
      [c,d,g,h],
      [i,j,m,n],
      [k,l,o,p]] ? ;
no
| ?- feldarabolasa([[a,b,c,  d],
                    [e,f,g,  h],
                    [i,j,k,  l],

                    [m,n,o,  p]], 3-3, LL).
LL = [[a,b,c,e,f,g,i,j,k],
      [d,h,l],
      [m,n,o],
      [p]] ? ;
no
| ?- feldarabolasa([[a,b,  c,d],
                    [e,f,  g,h],
		    [i,j,  k,l],

		    [m,n,  o,p]], 3-2, LL).
LL = [[a,b,e,f,i,j],
      [c,d,g,h,k,l],
      [m,n],
      [o,p]] ? ;
no
| ?- feldarabolasa([[a,b,c,  d],
		    [e,f,g,  h],

		    [i,j,k,  l],
		    [m,n,o,  p]], 2-3, LL).
LL = [[a,b,c,e,f,g],
      [d,h],
      [i,j,k,m,n,o],
      [l,p]] ? ;
no
| ?- feldarabolasa([[a,b,  c,d],

                    [e,f,  g,h],

		    [i,j,  k,l],

		    [m,n,  o,p]], 1-2, LL).
LL = [[a,b],
      [c,d],
      [e,f],
      [g,h],
      [i,j],
      [k,l],
      [m,n],
      [o,p]] ? ;
no
| ?-```
