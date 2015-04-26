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
	    %%% global predicates %%%
        ailp_reset/0,             % ?-ailp_reset.
        ailp_start_position/1     % ?-ailp_start_position(Pos).
	  ]).



:- dynamic
	 ailp_internal/1,
	 ailp_internal_thing/2.

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
	internal_use_energy(Agent),
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
	ailp_internal(agent_energy(Agent,Energy)).

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
agent_ask_oracle(Agent, OID, Question, Answer) :-
	nonvar(Agent),
	nonvar(OID),
	nonvar(Question),
	var(Answer),
	agent_current_position(Agent,Pos),
	map_adjacent(Pos, AdjPos, OID),
	OID = o(_),
	internal_object(OID, AdjPos, Options),
	member(question(Q)/answer(A),Options),
	( Question=Q -> Answer=A ; Answer='I do not know' ).


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
	init_things,
	reset([
		grid_size=N,
		cells=[
			[green, 1,1, N,N]
		],
		agents=[[oscar, 6, blue, X0,Y0]]
	]),
	do_command([oscar, colour, X0, Y0, yellow]).


%%% Do not query any of the predicates below! %%%

internal_grid_size(20).	% may be changed in testing

internal_topup(Emax):-
	internal_grid_size(N),
	Emax is N*N/5.

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

internal_use_energy(Agent) :-
	nonvar(Agent),
	retract(ailp_internal(agent_energy(Agent, E))),
	E>0, E1 is E - 1,
	assert(ailp_internal(agent_energy(Agent,E1))),
	( E1 < 20 -> atomic_list_concat(['Low energy:',E1],' ',A),
				 do_command([Agent,console,A])
	; true
	).

%% The position and number of these objects may change when testing your code
%% Charging stations
internal_object(c(1),p(1,10),[]).
internal_object(c(2),p(10,20),[]).
internal_object(c(3),p(20,9),[]).
internal_object(c(4),p(9,1),[]).
%% Oracles that have information
internal_object(o(1),p(5,3),[question(_)/answer(42)]).
%% Obstacles (things)
internal_object(t(I),Pos,[]):-
	ailp_internal_thing(I,Pos).

% version that makes the object visible on the map
internal_object1(O,Loc,L):-
	internal_object(O,Loc,L),
	Loc=p(X,Y),
	( O=t(_) -> Colour=black	% obstacle
	; O=c(_) -> Colour=orange	% charging station
	; O=o(_) -> Colour=red	% oracle
	),	
	do_command([oscar,colour,X,Y,Colour]).

init_things :- 	
	retractall(ailp_internal_thing(_,_)),
	assert_internal_things.
	% the following code generates a random set of internal things instead
	%randset(100,9999,S),
	%internal_grid_size(N),
	%internal_things(S,N,1).

internal_things([],_N,_M).
internal_things([Z|Zs],N,M):-
	internal_number2pos(Z,N,Pos),
	( internal_object(_O,Pos,_L) -> true
	; ailp_start_position(Pos) -> true
	; otherwise -> assert(ailp_internal_thing(M,Pos))
	),
	M1 is M+1,
	internal_things(Zs,N,M1).

internal_number2pos(Z,N,p(X,Y)):-
	K=N*N/5,	% roughly one in five cells is an obstacle
	Z1 is mod(Z,K),
	Z2 is (Z-Z1)/K,
	X is mod(Z1,N) + 1,
	Y is mod(Z2,N) + 1.

% fixed set of obstacles for development, your code will be tested with a different set
assert_internal_things:-
	assert(ailp_internal_thing(1,p(7,1))),
	assert(ailp_internal_thing(2,p(11,1))),
	assert(ailp_internal_thing(3,p(12,1))),
	assert(ailp_internal_thing(4,p(8,4))),
	assert(ailp_internal_thing(5,p(5,5))),
	assert(ailp_internal_thing(6,p(7,6))),
	assert(ailp_internal_thing(7,p(3,6))),
	assert(ailp_internal_thing(8,p(2,6))),
	assert(ailp_internal_thing(9,p(8,10))),
	assert(ailp_internal_thing(10,p(8,11))),
	assert(ailp_internal_thing(11,p(19,12))),
	assert(ailp_internal_thing(12,p(11,12))),
	assert(ailp_internal_thing(13,p(2,13))),
	assert(ailp_internal_thing(14,p(13,13))),
	assert(ailp_internal_thing(15,p(7,16))),
	assert(ailp_internal_thing(16,p(6,19))),
	assert(ailp_internal_thing(17,p(13,20))),
	assert(ailp_internal_thing(18,p(18,20))),
	assert(ailp_internal_thing(19,p(13,3))),
	assert(ailp_internal_thing(20,p(1,5))),
	assert(ailp_internal_thing(21,p(17,6))),
	assert(ailp_internal_thing(22,p(19,7))),
	assert(ailp_internal_thing(23,p(7,12))),
	assert(ailp_internal_thing(25,p(2,14))),
	assert(ailp_internal_thing(26,p(20,14))),
	assert(ailp_internal_thing(27,p(5,14))),
	assert(ailp_internal_thing(28,p(9,15))),
	assert(ailp_internal_thing(29,p(16,15))),
	assert(ailp_internal_thing(30,p(5,16))),
	assert(ailp_internal_thing(31,p(19,18))),
	assert(ailp_internal_thing(32,p(7,19))),
	assert(ailp_internal_thing(33,p(6,20))),
	assert(ailp_internal_thing(34,p(11,20))),
	assert(ailp_internal_thing(35,p(13,1))),
	assert(ailp_internal_thing(36,p(17,1))),
	assert(ailp_internal_thing(37,p(6,3))),
	assert(ailp_internal_thing(38,p(12,3))),
	assert(ailp_internal_thing(39,p(3,4))),
	assert(ailp_internal_thing(40,p(5,4))),
	assert(ailp_internal_thing(41,p(10,5))),
	assert(ailp_internal_thing(42,p(1,8))),
	assert(ailp_internal_thing(44,p(19,11))),
	assert(ailp_internal_thing(45,p(1,12))),
	assert(ailp_internal_thing(46,p(19,13))),
	assert(ailp_internal_thing(47,p(8,14))),
	assert(ailp_internal_thing(48,p(19,17))),
	assert(ailp_internal_thing(49,p(5,18))),
	assert(ailp_internal_thing(50,p(4,19))),
	assert(ailp_internal_thing(52,p(7,2))),
	assert(ailp_internal_thing(53,p(2,4))),
	assert(ailp_internal_thing(55,p(6,6))),
	assert(ailp_internal_thing(56,p(3,7))),
	assert(ailp_internal_thing(57,p(14,8))),
	assert(ailp_internal_thing(58,p(16,8))),
	assert(ailp_internal_thing(59,p(18,9))),
	assert(ailp_internal_thing(60,p(4,14))),
	assert(ailp_internal_thing(61,p(20,15))),
	assert(ailp_internal_thing(62,p(18,16))),
	assert(ailp_internal_thing(63,p(8,16))),
	assert(ailp_internal_thing(64,p(13,17))),
	assert(ailp_internal_thing(65,p(15,18))),
	assert(ailp_internal_thing(67,p(15,20))),
	assert(ailp_internal_thing(68,p(2,20))),
	assert(ailp_internal_thing(69,p(6,2))),
	assert(ailp_internal_thing(70,p(14,2))),
	assert(ailp_internal_thing(72,p(9,5))),
	assert(ailp_internal_thing(73,p(10,6))),
	assert(ailp_internal_thing(74,p(14,6))),
	assert(ailp_internal_thing(75,p(5,9))),
	assert(ailp_internal_thing(76,p(6,9))),
	assert(ailp_internal_thing(77,p(14,11))),
	assert(ailp_internal_thing(79,p(8,17))),
	assert(ailp_internal_thing(80,p(10,19))),
	assert(ailp_internal_thing(81,p(18,1))),
	assert(ailp_internal_thing(82,p(3,1))),
	assert(ailp_internal_thing(83,p(16,1))),
	assert(ailp_internal_thing(84,p(14,3))),
	assert(ailp_internal_thing(85,p(18,4))),
	assert(ailp_internal_thing(86,p(19,4))),
	assert(ailp_internal_thing(88,p(15,6))),
	assert(ailp_internal_thing(89,p(7,8))),
	assert(ailp_internal_thing(90,p(4,8))),
	assert(ailp_internal_thing(92,p(15,16))),
	assert(ailp_internal_thing(95,p(2,2))),
	assert(ailp_internal_thing(96,p(15,2))),
	assert(ailp_internal_thing(97,p(9,2))),
	assert(ailp_internal_thing(98,p(12,5))),
	assert(ailp_internal_thing(99,p(3,5))).
