# dp_hf_1_erlang

A feladat

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

Erlang-specifikációk

Írjon Erlang-függvényt khf1:feldarabolasa/2 néven egy tetszőleges mátrix adott paraméterű feldarabolására!
%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterű feldarabolása.
A programot tartalmazó modul attribútumai ezek legyenek:
-module(khf1).
-author('email@unit.org.hu').
-vsn('year-mm-dd').
-export([feldarabolasa/2]).
%-compile(export_all).
