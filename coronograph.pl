/*https://gist.github.com/Leonidas-from-XIV/4585246*/

:- use_module(library(clpfd)).
:- use_module(library(ugraphs)).
:- use_module(library(lists)).

total([N,M],G,Answers) :-
   N = M,
   functor(V,a,N),
   of2(1,V,N),
   cycle(1,G,V,Path),
   length(Path, Length2),
   of2(1,V,N),
   of1(Path,V),
   trees(Path,V,G,[],S),
   fuckingsort(S,X),
   sum_list(X,N),
   append([Length2],[X],Answers),
   !.

total(_,_,Answers) :-
   Answers = "'NO CORONA'".

splitlist(L, [], L, 0) :- !.
splitlist([H|T], [H|A], B, N) :- Nminus1 is N-1, splitlist(T, A, B, Nminus1).

halfhalf(L, A, B) :- length(L, Len), Half is Len//2, splitlist(L, A, B, Half).

merger(A, [], A) :- !.
merger([], B, B) :- !.
merger([Ha|Ta], [Hb|Tb], R) :- Ha =< Hb, merger(Ta, [Hb|Tb], M), R = [Ha|M].
merger([Ha|Ta], [Hb|Tb], R) :- Ha > Hb, merger(Tb, [Ha|Ta], M), R = [Hb|M].

fuckingsort([], []) :-
   !.
fuckingsort([E], [E]) :-
   !.
fuckingsort([H1, H2], [H1, H2]) :- H1 =< H2,!.
fuckingsort([H1, H2], [H2, H1]) :- H1 > H2,!.
fuckingsort(L, R) :- halfhalf(L, A, B), fuckingsort(A, Asort), fuckingsort(B, Bsort), merger(Asort, Bsort, R).

of2(A,_,N) :-
   A>N,!.
of2(A,V,N) :-
   setarg(A,V,0),
   B is A+1,
   of2(B,V,N).

of1(L,_) :-
   L=[],!.
of1([H|T],P) :-
   setarg(H,P,1),
   of1(T,P).

trees(L,_,_,Ter,Ter) :-
   L = [],!.
trees([H|T],P,G,Sect,Ter) :-
   arg(H,G,L),
   connected(H,L,G,P,[],[],[],FF),
   PreSect = [H|FF],
   length(PreSect,DD),
   trees(T,P,G,[DD|Sect],Ter),!.

/*
merge_list([],L,L ) :- !.
merge_list([H|T],L,[H|M]):-
    merge_list(T,L,M). */

connected(_,L,_,_,Parents,_,FF,FF) :-
   Parents = [],
   L = [],!.
connected(_,L,G,P,[H|T],[N|M],Nodes,FF) :-
   L = [],
   connected(H,N,G,P,T,M,Nodes,FF).
connected(_,[H|T],G,P,Parents,Rest,Nodes,FF) :-
   arg(H,P,0),
   setarg(H,P,1),
   arg(H,G,Lnew),
   connected(H,Lnew,G,P,[H|Parents],[T|Rest],[H|Nodes],FF).
connected(Z,[_|T],G,P,Parents,Rest,Nodes,FF) :-
   connected(Z,T,G,P,Parents,Rest,Nodes,FF).

cycle(A,G,V,Path) :-
    setarg(A,V,1),
    arg(A,G,L),
    travel(A,G,L,V,0,[A],[H|T]),
    clear(H,T,[],Path),
    !.
travel(_,_,[H|_],V,P,K,[H|K]) :-
   arg(H,V,1),
   H\=P,!.
travel(A,G,[H|_],V,_,Visited,Path) :-
   arg(H,V,0),
   setarg(H,V,1),
   arg(H,G,L),
   travel(H,G,L,V,A,[H|Visited],Path).
travel(A,G,[_|T],V,Parent,Visited,Path) :-
   travel(A,G,T,V,Parent,Visited,Path).

clear(A,[H|_],Acc,[H|Acc]) :-
   A=H,!.
clear(A,[H|T],Acc,Path) :-
    clear(A,T,[H|Acc],Path).

edgecreator(_,X,M,_) :-
    X>M,!.
edgecreator(Stream,X,M,G) :-
    read_line(Stream,[A,B]),
    arg(A,G,L1),
    setarg(A,G,[B|L1]),
    arg(B,G,L2),
    setarg(B,G,[A|L2]),
    Y is X+1,
    edgecreator(Stream,Y,M,G).

initial(A,N,_) :-
   A>N,!.
initial(A,N,G) :-
   arg(A,G,[]),
   B is A+1,
   initial(B,N,G).

step2(Stream, [N,M],G) :-
    functor(G,a,N),
    initial(1,N,G),
    edgecreator(Stream,1,M,G).

step1(Stream, X, Answers) :-
    read_line(Stream, L),
    step2(Stream,L,G),
    total(L,G,A),
    nth1(X, Answers, A).

coronograph(File, Answers) :-
    open(File, read, Stream),
    read_line(Stream, [T]),
    foreach(between(1, T, X), step1(Stream, X, Answers)),
    length(Answers, T),
    close(Stream),
        !.

read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).