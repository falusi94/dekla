-module(khf1).
-author('falusi.david94@gmail.com').
-vsn('2016-09-28').
-export([feldarabolasa/2]).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterű feldarabolása.

feldarabolasa([H|T],{R,C}) ->
    cutIntoArrays(lists:append([H],T), R, C, 1, 1, 1, 1, length(H), length(T)+1, 1, [], []).


getItem([H|T], RI, CI, R, C) ->
    if
        is_list(H) ->
            if
                RI<R ->
                    getItem(T, RI+1, CI, R, C);
                RI=:=R ->
                    getItem(H, RI, CI, R, C)
            end;
        true ->
            if
                CI<C ->
                    getItem(T, RI, CI+1, R, C);
                CI=:=C ->
                    getItem(H, RI, CI, R, C)
            end
    end;
getItem(H, RI, CI, R, C) ->
    H.

getItem([H|T], R, C) ->
    getItem([H|T], 1, 1, R, C).

cutIntoArrays(L, R, C, RI, CI, J, K, CMAX, RMAX, ASD, LIST, RET) ->
    NEW = (mod(ASD,R*C) =:= 0) or
            ( ((CI =:= CMAX) and (mod(RI,R) =:= 0)) or ((RI =:= RMAX) and (mod(CI,C) =:= 0)))
            or (RI*CI=:=RMAX*CMAX),
    if
        NEW ->
            ELEMENT = getItem( L, RI, CI),
            LIST1 = [],
            ASD1=0,
            RET1 = lists:append( RET, [lists:append(LIST, [ELEMENT])]);
        true ->
            RET1 = RET,
            ASD1=ASD,
            ELEMENT = getItem( L, RI, CI),
            LIST1 = lists:append(LIST, [ELEMENT])
    end,

    if
        CI=:=CMAX ->
            if
                RI=:=RMAX ->
                    RET1;
                RI=:=(K*R) ->
                    cutIntoArrays(L, R, C, RI+1, 1, 1, K+1, CMAX, RMAX, ASD1+1, LIST1, RET1);
                true ->
                     cutIntoArrays(L, R, C, RI+1, (J-1)*C+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
             end;
        CI=:=(J*C) ->
            if
                RI=:=RMAX ->
                    cutIntoArrays(L, R, C, (K-1)*R+1, CI+1, J+1, K, CMAX, RMAX, ASD1+1, LIST1, RET1);
                RI=:=(K*R) ->
                    cutIntoArrays(L, R, C, (K-1)*R+1, CI+1, J+1, K, CMAX, RMAX, ASD1+1, LIST1, RET1);
                true ->
                    cutIntoArrays(L, R, C, RI+1, (J-1)*C+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
            end;
         true ->
             cutIntoArrays(L, R, C, RI, CI+1, J, K, CMAX, RMAX, ASD1+1, LIST1, RET1)
    end.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,Y) -> 0.
