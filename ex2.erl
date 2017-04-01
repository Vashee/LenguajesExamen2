%% @author Equipo1
%% @doc @todo Add description to 'ex2'.

-module('ex2').
-compile(export_all).
-type expr() :: {'num',integer()}
			 |  {'var',atom()}
			 |  {'add',expr(),expr()}
			 |  {'mul',expr(),expr()}.

%%Así corre la función:
%%'MCex2':print({add,{num,2},{mul,{num,3},{num,4}}}).
%%Resultado="(2+(3*4))"

%%'MCex2':print({add,{num,2},{mul,{num,3},{var,a}}}).
%%Resultado="(2+(3*a))"

-spec print(expr()) -> string().

print({num,N}) ->
	integer_to_list(N);
print({var,A}) ->
	atom_to_list(A);
print({add,E1,E2}) ->
	"(" ++ print(E1) ++ "+" ++ print(E2) ++ ")";
print({mul,E1,E2}) ->
	"(" ++ print(E1) ++ "*" ++ print(E2) ++ ")".

-spec eval(expr()) -> integer().

%%Así corre la función:
%%'MCex2':eval({add,{num,2},{mul,{num,3},{num,4}}}).
%%Resultado=14

eval({num,N}) ->
	N;
eval({add,E1,E2}) ->
	eval(E1) + eval(E2);
eval({mul,E1,E2}) ->
	eval(E1) * eval(E2).

-type env() :: [{atom(),integer()}].
-spec eval2(env(),expr()) -> integer().

%%Así corre la función:
%%'MCex2':eval2([{a,4}],{add,{num,2},{mul,{num,3},{var,a}}}).
%%Resultado=14

eval2(_Env,{num,N}) ->
	N;
eval2(Env,{var,A}) ->
	lookup(A,Env);
eval2(Env,{add,E1,E2}) ->
	eval2(Env,E1) + eval2(Env,E2);
eval2(Env,{mul,E1,E2}) ->
	eval2(Env,E1) * eval2(Env,E2).

-spec lookup(atom(),env()) -> integer().

lookup(A,[{A,V}|_]) ->
	V;
lookup(A,[_|Rest]) ->
	lookup(A,Rest).

-type instr() :: {'push', integer()}
			   | {'fetch', atom()}  %%variable
			   | {'add2'}
			   | {'mul2'}.

-type program() :: [instr()]. %% list

-type stack() :: [integer()].

-spec compile(expr()) -> program().

-spec run(program(), env(), stack()) -> integer().

run([{push, N} | Continue], Env, Stack) -> 
	run(Continue, Env, [N | Stack]);

run([{fetch, A} | Continue], Env, Stack) -> 
	run(Continue, Env, [lookup(A, Env) | Stack]);

run([{add2} | Continue], Env, [N1, N2 | Stack]) -> 
	run(Continue, Env, [(N1+N2) | Stack]);

run([{mul2} | Continue], Env, [N1, N2 | Stack]) ->
	run(Continue, Env, [(N1*N2) | Stack]);

run([], _Env, [N]) -> 
	N.

%%COMPILATION
compile({num, N}) -> 
	[{push, N}];

compile({var, A}) -> 
	[{fetch, A}];

compile({add, E1, E2}) -> 
	compile(E1) ++ compile(E2) ++ [{add2}];

compile({mul, E1, E2}) -> 
	compile(E1) ++ compile(E2) ++ [{mul2}].
	
%%Así se corren:
%%'MCex2':compile({add, {num, 2}, {mul, {num, 3}, {num, 4}}}).
%%'MCex2':run([{push, 2}, {push, 3}, {push, 4}, {mul2}, {add2}], [], []).

%%Así se corren:
%%'MCex2':compile({add, {num, 2}, {mul, {num, 3}, {var, a}}}).
%%'MCex2':run(A, [{a,4}], []).

%% PARSING
%%Así lo corren:
%% 'MCex2':parse("(2+(3*4))").

-spec parse(string()) -> {expr(), string()}.

parse([$( | Rest]) ->						%%starts with a '(
    {E1, Rest1}   		= parse(Rest),		%%then an expression	
    [Op | Rest2]  		= Rest1,			%% then an operator * +
    {E2, Rest3}   		= parse(Rest2),		%% then another expression
    [$) | RestFinal]    = Rest3,			%% starts with a ')'
    {case Op of
        $+ -> {add, E1, E2};
        $* -> {mul, E1, E2}
    end,
    RestFinal};

%%literals
parse([Ch|Rest]) when ($0 =< Ch andalso Ch =< $9) orelse Ch==$- ->
    {Succeeds,Remainder} = get_while(fun is_digit/1,Rest),
    {{num, list_to_integer([Ch|Succeeds])}, Remainder};

parse([Ch|Rest])  when $a =< Ch andalso Ch =< $z ->
    {Succeeds,Remainder} = get_while(fun is_alpha/1,Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

-spec is_digit(integer()) -> boolean().
is_digit(Ch) ->
    $0 =< Ch andalso Ch =< $9.

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.

-spec get_while(fun((T) -> boolean()), [T]) -> {[T], [T]}.

%%Así se corre:
%%'MCex2':get_while(fun(X) -> X>0 end, [1,2,3]).
%%'MCex2':get_while(fun(X) -> X>5 end, [7,6,2,3]).
%%'MCex2':get_while([],[]).

get_while(P, [Ch | Rest]) ->
	case P(Ch) of
		true ->
			{Succeeds, Remainder} = get_while(P, Rest),
			{[Ch | Succeeds], Remainder};
		false ->
			{[], [Ch | Rest]}
	end;

get_while(_P, []) ->
	{[], []}.


%%SIMPLIFICATION
zeroA({add, E, {num, 0}}) ->
	E;
zeroA({add, {num, 0}, E}) ->
	E;
zeroA(E) ->
	E.

%%Multiplying by one
mul0({mul, E, {num,1}}) ->
	E;
mul0({mul,{num,1}, E}) ->
	E;
mul0(E) ->
	E.
%%Multiplying by zero
mulZ({mul, _, {num,0}}) ->
	{num,0};
mulZ({mul,{num,0}, _}) ->
	{num,0};
mulZ(E) ->
	E.

compose([]) ->
	fun (E) -> E end;  %%caso base

%%Example of high-order function
compose([Rule|Rules]) ->
	fun (E) -> (compose(Rules))(Rule(E)) end.

rules() ->
	[fun zeroA/1, fun mul0/1, fun mulZ/1].

simp(F, {add, E1, E2}) ->
	F({add, simp(F, E1), simp(F, E2)});
simp(F, {mul, E1, E2}) ->
	F({mul, simp(F, E1), simp(F, E2)});
	
%%High-order function
simp(_F, E) -> E.

simplify(E) ->
	simp(compose(rules()), E).