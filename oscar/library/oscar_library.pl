/*
 *      oscar_library.pl
 *
 *		Only use predicates exported in module heading in your code!
 */


:- module(oscar_library,
	  [ 
	    %%% map predicates %%%
        map_adjacent/3,           % ?-map_adjacent(p(1,5),A,O).
        map_distance/3,           % ?-map_distance(p(1,5),p(2,3),D).
	    %%% agent predicates %%%
        agent_do_move/2,          % ?-agent_do_move(oscar,p(2,1)).      % must be adjacent
        agent_do_moves/2,         % ?-agent_do_moves(oscar,[p(2,1),p(3,1),p(4,1),p(4,2)]).
        agent_current_energy/2,   % ?-agent_current_energy(oscar,E).
        agent_current_position/2, % ?-agent_current_position(oscar,P).
        agent_topup_energy/2,     % ?-agent_topup_energy(oscar, c(1)).  % must be adjacent
        agent_ask_oracle/4,       % ?-agent_ask_oracle(oscar,o(1)).     % must be adjacent
        agent_check_oracle/2,     % ?-agent_check_oracle(oscar,o(1)).
	    %%% global predicates %%%
        ailp_reset/0,             % ?-ailp_reset.
        ailp_start_position/1     % ?-ailp_start_position(Pos).
	  ]).



:- dynamic
	 ailp_internal/1.

:- set_homepage('oscar.html').


%%% map predicates %%%

% map_adjacent(+Pos, ?AdjPos, ?Occ)
% Occ = empty / c(42) / o(37) - charging station / oracle and ids
map_adjacent(Pos, AdjPos, OID) :-
	nonvar(Pos),
	internal_poss_step(Pos, _M, AdjPos, 1),
	( internal_off_board(AdjPos) -> fail
	; internal_object1(O,AdjPos,_) -> OID = O
	; otherwise -> OID = empty
	).

% map_distance(+Pos1, +Pos2, ?Distance)
% Manhattan distance between two grid squares
map_distance(p(X,Y),p(X1,Y1), D) :-
	D is abs(X - X1) + abs(Y - Y1).


%%% agent predicates %%%

% agent_do_move(+Agent, +To)
% Has constraint that To is map_adjacent to Agent's current position
% Reduces energy by 1 if step is valid
agent_do_move(Agent,To) :-
	nonvar(Agent),
	nonvar(To),
	ailp_internal(agent_energy(Agent, F)), 
	F>0, 
	%% check p(X,Y) if To is map_adjacent to current position and free
	agent_current_position(Agent,Pos),
	map_adjacent(Pos, To, Obj),
	Obj = empty,!,
	%% send move to server
	p(X,Y) = To,
	do_command([Agent, move, X, Y], _R),
	do_command([Agent, colour, X, Y, yellow]),
	%% move was successful so decrease agent energy
	internal_use_energy(Agent,1),
	retract(ailp_internal(agent_position(Agent, Pos))),
	assert(ailp_internal(agent_position(Agent, To))).

% agent_do_moves(+Agent, +ListOfMoves)
agent_do_moves(_, []).
agent_do_moves(Agent, [H|T]) :-
	agent_do_move(Agent, H),
	agent_do_moves(Agent,T).

% agent_current_energy(+Agent, -Energy)
agent_current_energy(Agent, Energy) :-
	nonvar(Agent),
	var(Energy),
	ailp_internal(agent_energy(Agent,Energy)),
	atomic_list_concat(['Current energy:',Energy],' ',A),
	do_command([Agent,console,A]). 


% agent_current_position(+Agent, -Pos)
agent_current_position(Agent, Pos) :-
	nonvar(Agent),
	var(Pos),
	ailp_internal(agent_position(Agent,Pos)).

% agent_topup_energy(+Agent, +OID)
% Agent's position needs to be map_adjacent to charging station identified by OID
agent_topup_energy(Agent, OID) :-
	nonvar(Agent),
	nonvar(OID),
	agent_current_position(Agent,Pos),
	map_adjacent(Pos, _AdjPos, OID),
	OID = c(_),
	retract(ailp_internal(agent_energy(Agent, _E))),
	internal_topup(Emax),
	assert(ailp_internal(agent_energy(Agent,Emax))).

% agent_ask_oracle(+Agent, +OID, +Question, -Answer)
% Agent's position needs to be map_adjacent to oracle identified by OID
% fails if oracle already visited by Agent
agent_ask_oracle(Agent, OID, Question, Answer) :-
	nonvar(Agent),
	nonvar(OID),
	\+ ailp_internal(agent_visited_oracle(oscar, OID)),
	nonvar(Question),
	var(Answer),
	internal_topup(Emax),
	Cost is ceiling(Emax/10),
	ailp_internal(agent_energy(Agent,Energy)),
	( Energy>Cost ->
		agent_current_position(Agent,Pos),
		map_adjacent(Pos, AdjPos, OID),
		OID = o(_),
		internal_object(OID, AdjPos, Options),
		member(question(Q)/answer(A),Options),
		( Question=Q -> Answer=A ; Answer='I do not know' ),
		atomic_list_concat([Question,Answer],': ',AA),
		internal_use_energy(Agent,Cost),
		assert(ailp_internal(agent_visited_oracle(oscar, OID)))
	; otherwise -> Answer='Sorry, not enough energy',AA=Answer
	),
	do_command([Agent,console,AA]).

% agent_check_oracle(+Agent, +OID)
% checks whether oracle already visited by Agent
agent_check_oracle(Agent, OID) :-
	nonvar(Agent),
	nonvar(OID),
	ailp_internal(agent_visited_oracle(oscar, OID)).


%%% global predicates %%%

ailp_start_position(p(1,1)).

ailp_reset :- 
	internal_grid_size(N),
	ailp_start_position(p(X0,Y0)),
	% set initial agent state
	retractall(ailp_internal(_)),
	assert(ailp_internal(agent_position(oscar, p(X0,Y0)))),
	internal_topup(Emax),
	assert(ailp_internal(agent_energy(oscar, Emax))),
	init_things(oracle,N/2),
	init_things(charging_station,N/10),
	init_things(thing,N*N/4),
	init_identity,	% defined in wp.pl
	reset([
		grid_size=N,
		cells=[
			[green, 1,1, N,N]
		],
		agents=[[oscar, 6, blue, X0,Y0]]
	]),
	internal_colour_map,	% make objects visible at the start
	do_command([oscar, colour, X0, Y0, yellow]).


%%% Do not query any of the predicates below! %%%

internal_grid_size(20).	% may be changed in testing

internal_topup(Emax):-
	internal_grid_size(N),
	Emax is ceiling(N*N/4).

internal_poss_step(p(X,Y), M, p(X1,Y1), I) :-
	member(M, [s,e,n,w]), % try moves in this order
	( M = s -> X1 =  X,    Y1 is Y+I
	; M = e -> X1 is X+I,  Y1 =  Y
	; M = n -> X1 =  X,    Y1 is Y-I
	; M = w -> X1 is X-I,  Y1 =  Y
	).

internal_off_board(p(X,Y)) :-
	internal_grid_size(N),
	( X < 1
	; X > N
	; Y < 1
	; Y > N
	).

internal_use_energy(Agent,Cost) :-
	nonvar(Agent),
	retract(ailp_internal(agent_energy(Agent, E))),
	E>0, E1 is E - Cost,
	assert(ailp_internal(agent_energy(Agent,E1))),
	( E1 < 20 -> atomic_list_concat(['WARNING -- Low energy:',E1],' ',A),
				 do_command([Agent,console,A])
	; true
	).

%% The position and number of these objects changes every time ailp_reset/0 is called
internal_object(c(I),Pos,[]):-
	ailp_internal(charging_station(I,Pos)).
%% Oracles that have information
internal_object(o(I),Pos,[question(link)/answer(Link)]):-
	ailp_internal(oracle(I,Pos)),
	ailp_identity(A),
	random_link(A,Link).
%% Obstacles (things)
internal_object(t(I),Pos,[]):-
	ailp_internal(thing(I,Pos)).

% version that makes the object visible on the map
internal_object1(O,Loc,L):-
	internal_object(O,Loc,L).
	%internal_colour_loc(O,Loc).	% disabled as may cause web client overload

internal_colour_loc(O,p(X,Y)):-
	( O=t(_) -> Colour=black	% obstacle
	; O=c(_) -> Colour=orange	% charging station
	; O=o(_) -> Colour=red	% oracle
	),	
	do_command([oscar,colour,X,Y,Colour]).

internal_colour_map:-
	internal_object(O,Loc,_),
	internal_colour_loc(O,Loc),
	fail.
internal_colour_map.

init_things(Label,Exp) :-
	K is ceiling(Exp), 	% round up if Exp evaluates to a fraction
	KK = 99999,
	randset(K,KK,S),
	internal_grid_size(N),
	internal_things(S,N,Label,1).

internal_things([],_N,_L,_M).
internal_things([Z|Zs],N,Label,M):-
	internal_number2pos(Z,N,Pos),
	Fact =.. [Label,M,Pos],
	( internal_object(_O,Pos,_L) -> true
	; ailp_start_position(Pos) -> true
	; otherwise -> assert(ailp_internal(Fact))
	),
	M1 is M+1,
	internal_things(Zs,N,Label,M1).

internal_number2pos(Z,N,p(X,Y)):-
	K=N*N/5,	% roughly one in five cells is an obstacle
	Z1 is mod(Z,K),
	Z2 is (Z-Z1)/K,
	X is mod(Z1,N) + 1,
	Y is mod(Z2,N) + 1.

