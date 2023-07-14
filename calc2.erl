-module(quiz_6).
-compile(export_all).

% calc({const, N}, env) ->
%   N;
% calc({EXP, N, M}, env) ->
%   case EXP of
%     add-> calc(N, env) + calc(M, env);
%     sub-> calc(N, env) - calc(M, env);
%     mult-> calc(N, env) * calc(M, env);
%     divi-> calc(N, env) / calc(M, env)
%   end.

aDict() ->
  dict:store("x",3,dict:store("y", 7, dict:new())).

calcrun({const, N}) ->
  calc{}

calc(_Env, {const, N}) ->
  {const, N};
calc(Env, {var, X}) ->
  {const, dict:fetch(X,Env)};
calc(Env, {add, E1, E2}) ->
  {const,V1} = calc(Env,E1),
  {const,V2} = calc(Env,E2),
  {const, V1+V2};
calc(Env, {sub, E1, E2}) ->
  {const,V1} = calc(Env,E1),
  {const,V2} = calc(Env,E2),
  {const, V1-V2};
calc(Env, {mult, E1, E2}) ->
  {const,V1} = calc(Env,E1),
  {const,V2} = calc(Env,E2),
  {const, V1*V2};
calc(Env, {divi, E1, E2}) ->
  {const,V1} = calc(Env,E1),
  {const,V2} = calc(Env,E2),
  {const, V1 div V2}.