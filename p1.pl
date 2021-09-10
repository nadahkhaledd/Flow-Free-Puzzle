
getlength(Rows,Columns):-
    grid(Rows,Columns).
getCoordinates(List,List2):-
    findall([In1,In2],dot(Char,[In1,In2]), List),
    findall(Char,dot(Char,[In1,In2]), List2).

allocateL(L,Indexis,Chars,Columns,List,AllStart, AllGoal):-
	allocateList(L,Indexis,Chars,Columns,[],List, [],[], AllStart, AllGoal).
allocateList(L,[],[],Columns,List,Liist,TempS,TempG,AllStart,AllGoal):-
    reverse(List,Liist),
	reverse(TempS,AllStart),
	reverse(TempG, AllGoal),!.
allocateList(L,[[F,S]|T2],[Ch1|T3],Columns,List,Liist,TempS,TempG, AllStart,AllGoal):-
    Index is  F*Columns + S,
	allocate(L,Index,Ch1,Array),
	nth0(I,[[F,S]|T2],[F,S]),
	(
		I mod 2 =:= 0 -> (allocate(L,I,Ch1,St), allocateList(L,T2,T3,Columns,[Array|List],Liist,[St|TempS], TempG, AllStart,AllGoal))
		;
		I mod 2 =:= 1 -> (allocate(L,I,Ch1,G), allocateList(L,T2,T3,Columns,[Array|List],Liist,TempS, [G|TempG],AllStart,AllGoal))
	).

allocate([H|T],Index1,Char,NStart):-
    allocate2([H|T],Index1,Char,[],NStart).
allocate2([],_,Char,Start,NStart):-
    reverse(Start,NStart),!.
allocate2([H|T],Index1,Char,List,Start):-
    (
    H =:= Index1 -> (H1 = Char, !)
    ;
    H1 = '-'
    ),
    allocate2(T,Index1,Char,[H1|List],Start).


findBound2(Start,Rows,Columns,Boundries):-
    NewStart is Start * Columns,
    findBoundries2(NewStart,Rows,Columns,[NewStart],Boundries).
findBoundries2(_,_,0,NBoundries,Boundries):-
    reverse(NBoundries,Boundries),!.
findBoundries2(Start,Rows,Columns,List,Boundries):-
    NewCol is Columns - 1,
    NewCol > -1,
    Start2 is Start + 1,
    findBoundries2(Start2,Rows,NewCol,[Start|List],Boundries).
	
move(S,Snew,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char):-
        findBound2(RowLimit,Rows,Columns,Boundries),
	down(S,Snew,Char,Rows,Boundries).
down(S,Snew,Char,Rows,[]):-
       blank_down(S,Snew,Char,Rows),!.
down(S,Snew,Char,Rows,[H|T]):-
	nth0(H,S,N),
        N = '-',
        down(S,Snew,Char,Rows,T).
blank_down(List,S,Char,Rows):-
	nth0(N,List,Char),
	Z is N+Rows,
    substitute(Z,List,10,Q,0),
	substitute(N,Q,'-',V,0),
    nth0(Y,V,10),
	substitute(Y,V,Char,S,0).



move(S,Snew,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char):-
    findBound2(RowStart,Rows,Columns,Boundries),
    up(S,Snew,Char,Rows,Boundries).
up(S,Snew,Char,Rows,[]):-
    blank_up(S,Char,Rows,Snew),!.
up(S,Snew,Char,Rows,[H|T]):-
    nth0(H,S,N),
    N = '-',
	up(S,Snew,Char,Rows,T).
blank_up(List,Char,Rows,S):-
    nth0(N,List,Char),
	Z is N-Rows,
    substitute(Z,List,10,Q,0),
	substitute(N,Q,'-',V,0),
    nth0(Y,V,10),
	substitute(Y,V,Char,S,0).


findBound(Start,Rows,Columns,Boundries):-
    findBoundries(Start,Rows,Columns,[],Boundries).
findBoundries(_,0,_,NBoundries,Boundries):-
    reverse(NBoundries,Boundries),!.
findBoundries(Start,Rows,Columns,List,Boundries):-
    NewRow is Rows - 1,
    NewRow > -1,
    NewStart is Start + Columns,
    findBoundries(NewStart,NewRow,Columns,[Start|List],Boundries).

move(S,Snew,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char):-
    findBound(ColLimit,Rows,Columns,Boundries),
    right(S,Snew,Char,Boundries).

right(S , Snew ,Char,[]):-
	blank_right(S,Snew,Char),!.
right(S , Snew ,Char,[H|T]):-
	nth0(H,S,N),
    N = '-',
    right(S,Snew,Char,T).

blank_right(List,S,Char):-
	nth0(N,List,Char),
	Z is N+1,
    substitute(Z,List,10,Q,0),
	substitute(N,Q,'-',V,0),
    nth0(Y,V,10),
	substitute(Y,V,Char,S,0).


move(S,Snew,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char):-
        findBound(ColStart,Rows,Columns,Boundries),
	left(S,Snew,Char,Boundries).
left(S,Snew,Char,[]):-
	blank_left(S,Snew,Char),!.
left(S,Snew,Char,[H|T]):-
	nth0(H,S,N),
    N = '-',
	left(S,Snew,Char,T).
blank_left(List,S,Char):-
	nth0(N,List,Char),
	Z is N-1,
    substitute(Z,List,10,Q,0),
	substitute(N,Q,'-',V,0),
    nth0(Y,V,10),
	substitute(Y,V,Char,S,0).
	
substitute(_,[],_,[],_):-!.
substitute(I,[H|T],Ele,A,Char):-
    I is Char,
    A = [Ele|Hold],
    Char2 is Char+1,
    substitute(I,T,Ele,Hold,Char2).
substitute(I,[H|T],Ele,A,Char):-
    Char2 is Char+1,
    A=[H|Hold],
    substitute(I,T,Ele,Hold,Char2).


go(Start,Goal):-
		path([[Start,null]],[],Goal).


path([],_,_):-
		write('No solution'),nl,!.
path([[Goal,Parent] | _], Closed, Goal):-
		write('A solution is found'), nl ,
		printsolution([_,Parent],Closed),!.
path(Open, Closed, Goal):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent] | Closed], Goal).


getchildren(State, Open ,Closed , Children):-
		bagof(X, moves( State, Open, Closed, X), Children), ! .
getchildren(_,_,_, []).


addListToOpen(Children,[],Children).
addListToOpen(L, [H|Open], [H|NewOpen]):-
		addListToOpen(L, Open, NewOpen).


removeFromOpen([State|RestOpen], State, RestOpen).


moves( State, Open, Closed,[Next,State]):-
		move(State,Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

printsolution([State, null],_):-
		write(State),nl.
printsolution([State, Parent], Closed):-
	member([Parent, GrandParent], Closed),
	printsolution([Parent, GrandParent], Closed),
	write(State), nl.

printGrid([],_,_):-!.
printGrid([H|T],Columns, Counter):-	
	(
		Counter < Columns-1 -> (write(H), write('  '), NewC is Counter+1,!)
		;
		write(H),nl, NewC is 0
	),
	printGrid(T, Columns, NewC).


solve():-
    getlength(Rows,Columns),
    N is Rows*Columns-1,
    numlist(0,N,L),
    getCoordinates(ListIndex,ListChar),
    allocateL(L,ListIndex,ListChar,Columns,NewL,AllStart, AllGoal),
	sort(ListChar, Colors),
	write('Start and goal for each color:'),nl,printGrid(NewL,1,0), nl,
	write('Existing Colors:  '), write(Colors),nl,!.

    %go(Start,Goal,Rows,Columns,Char1),!.

