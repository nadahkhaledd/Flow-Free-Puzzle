getlength(Rows,Columns):-
    grid(Rows,Columns).
getCoordinates(List,Chars):-
    findall([In1,In2],dot(Char,[In1,In2]), List),
    findall(Char,dot(Char,[In1,In2]), Chars).


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
	down(S,Snew,Char,Columns,Boundries).
down(S,Snew,Char,Columns,[]):-
       blank_down(S,Snew,Char,Columns),!.
down(S,Snew,Char,Columns,[H|T]):-
	nth0(H,S,N),
        N = '-',
        down(S,Snew,Char,Columns,T).
blank_down(List,S,Char,Columns):-
	nth0(N,List,Char),
	Z is N+Columns,
    substitute(Z,List,10,Q,0),
	substitute(N,Q,'-',V,0),
    nth0(Y,V,10),
	substitute(Y,V,Char,S,0).



move(S,Snew,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char):-
    findBound2(RowStart,Rows,Columns,Boundries),
    up(S,Snew,Char,Columns,Boundries).
up(S,Snew,Char,Columns,[]):-
    blank_up(S,Char,Columns,Snew),!.
up(S,Snew,Char,Columns,[H|T]):-
    nth0(H,S,N),
    N = '-',
	up(S,Snew,Char,Columns,T).
blank_up(List,Char,Columns,S):-
    nth0(N,List,Char),
	Z is N-Columns,
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


go(Start,Goal,Rows,Columns,Char):-
		getHeuristic(Start, H, Goal),
		path([[Start,null, 0, H, H]],[],Goal,Rows,Columns,Char).


path([], _, _,_,_,_):-
		write('No solution'),nl,!.
path(Open, Closed, Goal,_,Columns,Char):-
		getBestChild(Open, [Goal, Parent, G, H, F], RestOfOpen),
		write('A solution is found'),  nl ,
		printsolution([Goal,Parent, G, H, F],Columns, Closed),!.
path(Open, Closed, Goal,Rows,Columns,Char):-
		getBestChild(Open, [State, Parent, G, H, F], RestOfOpen),
		getChildren(State, Open, Closed, Children, G, Goal,Rows,Columns,Char),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent, G, H, F] | Closed], Goal,Rows,Columns,Char).


getChildren(State, Open ,Closed , Children, G, Goal,Rows,Columns,Char):-
		bagof(X, moves( State, Open, Closed, X, G, Goal,Rows,Columns,Char), Children) .
getChildren(_,_,_, [],_,_,_,_,_).


addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, G, H, F], [_, _, _, _, F1], [State, Parent, G, H, F]):-
	F < F1, !.
getBest([_, _, _, _, _], [State1, Parent1, G1, H1, F1], [State1, Parent1, G1, H1, F1]).


removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

moves( State, Open, Closed,[Next,State, NG, H, F], G, Goal,Rows,Columns,Char):-
    ColLimit is  Columns - 1,
    ColStart is 0,
    RowLimit is Rows-1,
    RowStart is 0,
    move(State,Next,ColStart,ColLimit,RowStart,RowLimit,Rows,Columns,Char),
	\+ member([Next, _, _, _, _],Open),
	\+ member([Next, _, _, _, _],Closed),
	NG is G + 1,
	getHeuristic(Next, H, Goal),
	F is NG + H.

getHeuristic([], 0, []):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).

getHeuristic([_|T1],H,[_|T2]):-
	getHeuristic(T1,TH, T2),
	H is TH + 1.


printsolution([State, null, G, H, F],Columns,_):-!,
	write('Steps:'),nl,printGrid(State, Columns, 0),nl.
printsolution([State, Parent, G, H, F],Columns, Closed):-
	member([Parent, GrandParent, G1, H1, F1], Closed),
	printsolution([Parent, GrandParent, G1, H1, F1],Columns, Closed),
	printGrid(State, Columns, 0),nl,nl.

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
    getCoordinates([[H,T],[S,R]],[Char,Char2]),
    Index1 is H*Columns + T,
    Index2 is S*Columns + R,
    allocate(L,Index1,Char,Start),
    allocate(L,Index2,Char,Goal),
    go(Start,Goal,Rows,Columns,Char),!.