#include "cekla.h"

/* osszekevert(S, A) == SK, ha SK az S szám A alapú összekevert
   változata (S>0, A>1 egész számok).
   Egy S szám A alapú összekevertjét úgy kapjuk meg, hogy az S számot felírjuk
   az A alapú számrendszerben, a számjegyeit egy listával ábrázoljuk, a
   listát a fentiek szerint átrendezzük, majd a kapott lista elemeit
   egy A alapú szám jegyeinek tekintve előállítjuk a keresett értéket.
*/
int osszekevert(const int S, const int A) {
    const int retval = shuffle(changeBase(S, A));
    writeln(retval);
    writeln(append(retval));
    return 0;
}

list changeBase(const int number, const int base) {
    return changeBase(number, base, nil);
}
list changeBase(const int number, const int base, const list l) {
    const int remainder = number % base;
    const int left = number / base;
    if(left == 0){
        return cons(remainder, l);
    }
    else
        return changeBase(left, base, cons(remainder, l));
}

list shuffle(const list l) {
    if(length(l)%2 == 0)
        return shuffleEven(l, nil, 1);
    else
        return shuffleOdd(l, nil, 1);
}
list shuffleEven(const list l, const list shuffled, const int index) {
    if(index > length(l))
        return shuffled;
    if(index%2 == 1)
        return shuffleEven(l, cons(getNth_(l,index), shuffled), index+1);
    else
        return shuffleEven(l, cons(getNth_(l,length(l)-index), shuffled), index+1);
}
list shuffleOdd(const list l, const list shuffled, const int index) {
    if(index > length(l))
        return shuffled;
    if(index%2 == 1)
        return shuffleOdd(l, cons(getNth_(l,index), shuffled), index+1);
    else
        return shuffleOdd(l, cons(getNth_(l,length(l)-index), shuffled), index+1);
}

int getNth_(const list l, const int n) {
    if(1 == n)
        return hd(l);
    else
        return getNth_( tl(l), n-1);
}

int length(const list l) {
    return length(l, 0);
}
int length(const list l, const int index) {
    if(l == nil)
        return index;
    return length(tl(l), index+1);
}

int append(const list l) {
    return append(l, 0);
}
int append(const list l, const int ret) {
    if(l==nil)
        return ret;
    return append(tl(l), ret*pow(10,(hd(l)/10+1))+hd(l));
}

int pow(const int number, const int x){
    return pow(number, x, number);
}
int pow(const int number, const int x, const int ret){
    if(x == 1)
        return ret;
    else
        return pow(number, x-1, ret*number);
}
