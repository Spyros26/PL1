%https://stackoverflow.com/questions/27788739/binary-to-decimal-prolog (used for binary_list predicate)
%https://www.swi-prolog.org/pldoc/man?section=lists (used for learning general list manipulation predicates)
%https://stackoverflow.com/questions/44368571/swi-prolog-read-file-examples (used for reading from a file)

:- use_module(library(clpfd)).

binary_list([], N, N).
binary_list([Bit|Bits], Acc, N) :-
	Bit in 0..1,
	Acc1 #= Acc*2 + Bit,
	binary_list(Bits, Acc1, N).

rev_binary_list(Bin, N) :-
	binary_list(B, 0, N),
	reverse(B, Bin),
	!.

reshape([], []).
reshape([H|T], L) :-
	nth0(0, T, 0, _), 
	reshape(T, L2), 
	append([H], L2, L),
	!.
reshape([H|T], L2) :-
	nth0(0, T, A, R), 
	B is A-1,
	C is H+2,
	append([B], R, END),
	append([C], END, L2).

compact(L, G) :- last(L, A), A \= 0, delete(L, '', G), !.
compact(L, G) :-
	length(L, N),
	nth1(N, L, 0, R),
	compact(R, G).

find(L, K, G) :-
	sum_list(L, K),
	compact(L, G),
	!.
find(M, K, G) :-
	reshape(M, L),
	find(L, K, G).


powers([N, K], L) :- K > N, length(L, 0), !.
powers([N, K], L) :-	
	rev_binary_list(B, N),
	sum_list(B, S),
	S > K,
	length(L, 0),
	!.
powers([N, K], L) :-
	rev_binary_list(B, N),
	find(B, K, L).

do(Stream, X, Answers) :-
	read_line(Stream, L),
	powers(L, A),
	nth1(X, Answers, A).
	

powers2(File, Answers) :-
	open(File, read, Stream),
	read_line(Stream, [T]),
	foreach(between(1, T, X), do(Stream, X, Answers)), 
	length(Answers, T),
	close(Stream).

read_line(Stream, List) :-
	read_line_to_codes(Stream, Line),
	atom_codes(A, Line),
	atomic_list_concat(As, ' ', A),
	maplist(atom_number, As, List).


%  powers2('C:/Users/spval/Documents/Prolog/f.txt', Answers)
	
	
	
	
	
	