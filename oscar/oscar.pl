/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(57195).


solve_task(Task,Cost):-
	agent_current_position(oscar,P),
	heuristic_cost_estimate(P, Task, H),
	solve_task_bt(Task,[c(H,0,none,P)],[],0,R,Cost,_NewPos),!,	% prune choice point for efficiency
	R = [c(_F,_G,_Came_From,Exit)|_Children],
	reconstruct_path(Exit,R,Path),
	reverse(Path,[_Init|Path1]),
	agent_do_moves(oscar,Path1).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,ClosedSet,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :- 
	achieved(Task,Current,ClosedSet,RPath,Cost,NewPos).
solve_task_bt(Task,Current,ClosedSet,D,RR,Cost,NewPos) :-
	Current = [c(F,G,Came_From,P)|Children],
	ClosedSet1 = [c(F,G,Came_From,P)|ClosedSet], %Adding first node in current to ClosedSet
	findall(R,search(P,R,R,1),AdjacentPos),
	elementsNotInSecondList(AdjacentPos,ClosedSet,AdjacentPosExcClosedSet),
	adjPosFormatted(AdjacentPosExcClosedSet, G, P, Task, AdjPosFormatted),
	updateOpenSet(AdjPosFormatted, Children, UpdatedOpenSet),
	ifNotInListAddToList(Children, UpdatedOpenSet, UpdatedOpenSet1),
	insert_sort(UpdatedOpenSet1, SortedOpenSet),
	D1 is D+1,

	solve_task_bt(Task,SortedOpenSet,ClosedSet1,D1,RR,Cost,NewPos). % backtracking search

reconstruct_path(none, _R, Path) :-
	Path = [].
reconstruct_path(Position, R, Path) :-
	findNextPosition(Position, R, Position1),
	reconstruct_path(Position1, R, Path1),
	Path = [Position|Path1].

findNextPosition(Came_From,[c(_F,_H,Position,Came_From)|_Remainder],Position).
findNextPosition(Came_From,[c(_F,_H,_Position,_Came_From)|Remainder],Position) :-
	findNextPosition(Came_From, Remainder, Position).

updateOpenSet([], _OpenSet, UpdatedOpenSet) :-
	UpdatedOpenSet = [].
updateOpenSet(List, OpenSet, UpdatedOpenSet) :-
	List = [c(F,G,Came_From,P)|Remainder],
	updateOpenSet(Remainder,OpenSet,UpdatedOpenSet1),
	(checkPositionInOpenSet(P, OpenSet, c(F1,G1,Came_From1,P1)) -> 
		(G<G1 -> UpdatedOpenSet=[c(F,G,Came_From,P)|UpdatedOpenSet1]
			; UpdatedOpenSet=[c(F1,G1,Came_From1,P1)|UpdatedOpenSet1]
		)
		; UpdatedOpenSet=[c(F,G,Came_From,P)|UpdatedOpenSet1]
	).

ifNotInListAddToList([],List,RemainingOpenListElements) :-
	RemainingOpenListElements = List.

ifNotInListAddToList(CurrentOpenSet,List,RemainingOpenListElements) :-
	CurrentOpenSet = [c(F,G,Came_From,P)|Remainder],
	ifNotInListAddToList(Remainder,List,RemainingOpenListElements1),
	(checkPositionInOpenSet(P, List, _Element) -> RemainingOpenListElements=RemainingOpenListElements1
		; RemainingOpenListElements=[c(F,G,Came_From,P)|RemainingOpenListElements1]
	).

checkPositionInOpenSet(P,[c(F,G,Came_From,P)|_T],Element) :-
	Element = c(F,G,Came_From,P).
checkPositionInOpenSet(P,[_H|T],Element) :-
	checkPositionInOpenSet(P,T,Element).

elementsNotInSecondList([], _SecondList, DisjointList) :-
	DisjointList=[].
elementsNotInSecondList(List, SecondList, DisjointList) :-
	List = [p(X,Y)|Remainder],
	(member(c(_F,_G,_Came_From,p(X,Y)),SecondList) -> 
		elementsNotInSecondList(Remainder,SecondList, DisjointList1),
		DisjointList=DisjointList1
	; otherwise -> 
		elementsNotInSecondList(Remainder,SecondList, DisjointList1),
		DisjointList=[p(X,Y)|DisjointList1]).

achieved(go(Exit),Current,ClosedSet,RPath,Cost,NewPos) :-
	Current = [c(F,Cost,Came_From,NewPos)|_Children],
	[c(F,Cost,Came_From,NewPos)|ClosedSet] = RPath,
	( Exit=none -> true
	; otherwise -> RPath = [c(_F1,_G1,_Came_From1,Exit)|_]
	).
achieved(find(O),Current,ClosedSet,RPath,Cost,NewPos) :-
	Current = [c(F,Cost,Came_From,NewPos)|_Children],
	[c(F,Cost,Came_From,NewPos)|ClosedSet] = RPath,
	( O=none    -> true
	; otherwise -> RPath = [c(_F,_Cost,_Came_From,Last)|_],map_adjacent(Last,_,O)
	).

adjPosFormatted([],_G,_Came_From,_Task,OpenSet) :-
	OpenSet = [].
adjPosFormatted(NextPositions,G,Came_From,Task,OpenSet) :-
	NextPositions = [p(X,Y)|Children],
	heuristic_cost_estimate(p(X,Y),Task,H),
	G1 is G + 1,
	F is G1 + H,
	adjPosFormatted(Children,G,Came_From,Task,OpenSet1),
	OpenSet = [c(F,G1,Came_From,p(X,Y))|OpenSet1].

insert_sort(List,Sorted):-
	i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-
	insert(H,Acc,NAcc),
	i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]):-
	X = c(XF,_,_,_),
	Y = c(YF,_,_,_),
	XF>=YF,
	insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-
	X = c(XF,_,_,_),
	Y = c(YF,_,_,_),
	XF<YF.
insert(X,[],[X]).

heuristic_cost_estimate(Start, Task, Dist) :-
	( Task=go(Goal) ->
		Start = p(X0, Y0),
		Goal = p(X1, Y1),
		Dist is abs(X0 - X1) + abs(Y0-Y1)
	; otherwise -> Dist is 0).
	

search(F,N,N,1):-
	map_adjacent(F,N,empty).


%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = reset -> ailp_reset,shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([oscar,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([oscar,say,A])
	; R=[H|T]             -> show_response(H),show_response(T)
	; R=[]                -> true
	; otherwise           -> writes(['! ',R])
	).

writes(A):-
	( A=[]      -> nl
	; A=nl      -> nl
	; A=[H|T]   -> writes(H),writes(T)
	; A=term(T) -> write(T)
	; otherwise -> write(A)
	).

% callable(+Command, +Goal, ?Response)
callable(call(G),call(G),G).
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)).
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))).
callable(position,agent_current_position(oscar,P),both(current_position(P))).
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
