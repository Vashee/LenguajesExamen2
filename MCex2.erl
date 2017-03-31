%% @author Equipo1
%% @doc @todo Add description to 'MCex2'.

-module('MCex2').

%% ====================================================================
%% API functions
%% ====================================================================
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

%%Turn an expression into a number, by working out its value.
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

%% ====================================================================
%% Internal functions
%% ====================================================================

%%Four kind of data that represent instructions. Diversion machine, compilation
%% and its running
-type instr() :: {'push', integer()}
			   | {'fetch', atom()}  %%variable
			   | {'add2'}
			   | {'mul2'}.

-type program() :: [instr()]. %% list

-type stack() :: [integer()].

%%Do the work of our implementation

%%Compile and expression into a program. Program: List of instructions
-spec compile(expr()) -> program().

%%Function that will run a program. 
%%Takes a program and an environment to look at values of variablles
%% and will return the result of running that program, so will be an integer.
-spec run(program(), env(), stack()) -> integer().

%%The stack machine run function is defined using pattern matching
%% and tail recursion.

%%matching a push instruction, we have the continuation. The environment isn't change but The stack is different.
run([{push, N} | Continue], Env, Stack) -> 
	run(Continue, Env, [N | Stack]);

run([{fetch, A} | Continue], Env, Stack) -> 
	run(Continue, Env, [lookup(A, Env) | Stack]);

%%we have and add instruction at the head and 
%%in the continue we match on the environment by variable but there must be at least two values on the stack.
run([{add2} | Continue], Env, [N1, N2 | Stack]) -> 
	run(Continue, Env, [(N1+N2) | Stack]);

run([{mul2} | Continue], Env, [N1, N2 | Stack]) ->
	run(Continue, Env, [(N1*N2) | Stack]);

%%No tenemos ninguna instrucción más, y tenemos sólo un valor en el stack--> regresamos ese valor.
run([], _Env, [N]) -> 
	N.

%%COMPILATION
compile({num, N}) -> 
	[{push, N}];

compile({var, A}) -> 
	[{fetch, A}];

%%To perform and add, evaluate two sub-expressions, putting each of the results on the stack
%% and then add the values on top of the stack.
compile({add2, E1, E2}) -> 
	compile(E1) ++ compile(E2) ++ [{add2}];

compile({mul2, E1, E2}) -> 
	compile(E1) ++ compile(E2) ++ [{mul2}].

%% PARSING

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
parse([Ch|Rest])  when $a =< Ch andalso Ch =< $z ->
    {Succeeds,Remainder} = get_while(fun is_alpha/1,Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

%%Testing for a small alphabetic character..
is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.

%%Recognising numbers and literals
%%Get the longest initial segment of a list with a given property
-spec get_while(fun((T) -> boolean()), [T]) -> {[T], [T]}.

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

mul0({mul, E, {num,1}}) ->
	E;
mul0({mul,{num,1}, E}) ->
	E;
mul0(E) ->
	E.


