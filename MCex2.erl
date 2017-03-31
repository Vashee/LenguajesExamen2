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


