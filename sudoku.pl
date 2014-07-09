:- use_module(library(readutil)).
:- use_module(library(clpfd)).

solvername('Giacomo Girolamo Casasolver').  %% charmant, intelligent, und ruft immer "erster", wenn er fertig ist.

%% Reader Section ###############################
open_and_read(File,Lines) :-
    open(File, read, Str),
    read_file(Str,Lines),
    close(Str).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,X),
    read_file(Stream,L).

%% Parser Section ###############################

% parse_lines(+Input, -Size, -Output)
% Parses Lines and returns relevant information for solver
% Input is raw Codes-Output from open_and_read/2, Size gives the width (and height) of the Sudoku,
% Output is a List of Sudoku rows, formatted for the solver.

parse_lines([H|T], Size, Output) :-
	get_size(H, Size),
	create_rows([H|T], Output).

get_size(Line, Size) :-
	get_size(Line, 0, TSize),
	Size is (TSize+1)*(TSize+1).

get_size([], Size, Size).

get_size([124|T], Current, Size) :-  %%124 == |
	NCurrent is Current + 1, 
	get_size(T, NCurrent, Size).

get_size([_|T], Current, Size) :- 
	get_size(T, Current, Size).

create_rows([], []).

create_rows([H|T], [H2|T2]) :-
	\+ invalid(H),
	atom_codes(Line, H),
	atomic_list_concat(Temp, ' ', Line),  %%Split at those nasty Whitespaces
	create_row(Temp, H2),
	create_rows(T, T2).

create_rows([H|T], T2) :-
	invalid(H),
	create_rows(T, T2).

invalid([45|_]).  %% 45 == -

create_row([],[]).

create_row([H|T], [H2|T2]) :-
	parse_val(H, H2),
	create_row(T, T2).
create_row([_|T], T2) :-
	create_row(T, T2).

parse_val('X', _).
parse_val('.', _).
parse_val(Val, Res) :-
	atom_number(Val, Res).

%% Solver Section ###############################

solve_sudoku(InFile, OutFile) :-
	open_and_read(InFile, RawLines), !,
	parse_lines(RawLines, Size, Lines), !,
	solve_intern(Lines, Size), !,
	sudoku_to_file(Lines, Size, OutFile), !.

%% Special cases for da speedz
solve_intern(Lines, 9) :-
	length(Lines, 9), maplist(length_(9), Lines),
	append(Lines, Vars), 
	Vars ins 1..9,
	maplist(all_distinct, Lines),
	transpose(Lines, Cols),
	maplist(all_distinct, Cols),
	Lines = [A,B,C,D,E,F,G,H,I],
	blocks(A,B,C), blocks(D,E,F), blocks(G,H,I),
	maplist(label, Lines).

blocks([], [], []).
blocks([A,B,C|T1], [D,E,F|T2], [G,H,I|T3]) :-
	all_distinct([A,B,C,D,E,F,G,H,I]),
	blocks(T1, T2, T3).

solve_intern(Lines, 4) :-
	length(Lines, 4), maplist(length_(4), Lines),
	append(Lines, Vars), 
	Vars ins 1..4,
	maplist(all_distinct, Lines),
	transpose(Lines, Cols),
	maplist(all_distinct, Cols),
	Lines = [A,B,C,D],
	blocks(A,B), blocks(C,D),
	maplist(label, Lines).

blocks([], []).
blocks([A,B|T1], [C,D|T2]) :-
	all_distinct([A,B,C,D]),
	blocks(T1, T2).


solve_intern(Lines, 16) :-
	length(Lines, 16), maplist(length_(4), Lines),
	append(Lines, Vars), 
	Vars ins 1..16,
	maplist(all_distinct, Lines),
	transpose(Lines, Cols),
	maplist(all_distinct, Cols),
	Lines = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
	blocks(A,B,C,D), blocks(E,F,G,H), blocks(I,J,K,L), blocks(M,N,O,P),
	maplist(label, Lines).

blocks([], [], [], []).
blocks([A,B,C,D|T1], [E,F,G,H|T2], [I,J,K,L|T3], [M,N,O,P|T4]) :-
	all_distinct([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]),
	blocks(T1, T2, T3, T4).

%% general case, applies from 25x25 upwards

solve_intern(Lines, Size) :-
	length(Lines, Size), maplist(length_(Size), Lines),
 	append(Lines, Vars), 
 	Vars ins 1..Size,
 	maplist(all_distinct, Lines),
 	transpose(Lines, Cols),
 	maplist(all_distinct, Cols),
 	Width is integer(sqrt(Size)),
	get_blocks(Lines, Width, [], Blocks),
	maplist(all_distinct, Blocks),
	maplist(label, Lines).

get_blocks([], _, Res, Res).
get_blocks(Matrix, K, Acc, Res) :-
	get_K(Matrix, K, Out, Rest),
	transpose(Out, TOut),
	chop_K(TOut, K, H),
	append(Acc, H, NAcc),
	get_blocks(Rest, K, NAcc, Res).

chop_K([], _, []).
chop_K(In, K, [H|Out]) :-
	get_K(In, K, TOut, Rest),
	flatten(TOut, H),
	chop_K(Rest, K, Out).

get_K(Rest, 0, [], Rest).
get_K([H|T], Count, [H|Res], Rest) :-
	NCount is Count - 1,
	get_K(T, NCount, Res, Rest).	

%% Output Section ###############################

%% Make Changes, to accomodate 2-digit numbers.
sudoku_to_file(Lines, Size, File) :-
	open(File, write, Str),
	Width is integer(sqrt(Size)),
	write_lines(Lines, Width, Str, 0),
	close(Str).

write_lines([], _, _, _).
write_lines(Lines, Size, Str, Size) :-  %% Time to paint some separators
	write(Str, '-'),
	write_separator(Size, Str, 0, 0),
	nl(Str),
	write_lines(Lines, Size, Str, 0).
write_lines([H|T], Size, Str, Count) :-
	write(Str, ' '),
	write_line(H, Size, Str, 0),
	nl(Str),
	NCount is Count + 1,
	write_lines(T, Size, Str, NCount).

write_line([],_,_,_).
write_line(Line, Size, Str, Size) :-
	write(Str, '| '),
	write_line(Line, Size, Str, 0).
write_line([H|T], Size, Str, Count) :-
	write(Str, H),
	write(Str, ' '),
	NCount is Count + 1,
	write_line(T, Size, Str, NCount).

write_separator(Size, Str, Size, BCount) :-
	BCount is Size - 1.
write_separator(Size, Str, Size, BCount) :- 
	write(Str, '+-'),
	NBCount is BCount + 1,
	write_separator(Size, Str, 0, NBCount).
write_separator(Size, Str, Count, BCount) :-
	write(Str, '--'),
	NCount is Count + 1,
	write_separator(Size, Str, NCount, BCount).

%% Helper Functions #############################
length_(L, Ls) :- length(Ls, L).


%% N-QUEENS ##################################################~~~~~
queens(N, Sol) :-
	numlist(1, N, Xpos),
	numlist(1, N, Ypos),
	queens(Xpos, Ypos, [], Sol).

num_sols(N, Num) :-
	numlist(1, N, Xpos),
	numlist(1, N, Ypos),
	findall(Ls, queens(Xpos, Ypos, [], Ls), Solutions),
	length(Solutions, Num).

queens([], [], Result, Result) :- !.  							%% All positions were set
queens([X|T], Ypos, Set, Result) :-	!,							%% Take a remaining X
	select(Y, Ypos, YRest),										%% Select random, still valid 
	valid((X,Y), Set),											%% check if combination is valid
	queens(T, YRest, [(X,Y)|Set], Result).						%% If yes, set remaining queens

valid(_, []) :- !.
valid((X,Y), Set) :-
	select((X2, Y2), Set, Rest), !,								%% Don't backtrack here. If the picked (X,Y) colided, just fail.
	abs(X2-X) =\= abs(Y2-Y),									%% Clever Internet Gnomes and their diagonal-check ideas
	valid((X,Y), Rest). 				
