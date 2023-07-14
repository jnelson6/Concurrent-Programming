%%% Stub for Quiz 6
%%% NOT MINE

-module(calc).
-compile(export_all).

env() -> #{"x"=>3, "y"=>7}.

e1() ->
    {add, 
     {const,3},
     {divi,
      {var,"x"},
      {const,4}}}.

e2() ->
    {add, 
     {const,3},
     {divi,
      {var,"x"},
      {const,0}}}.

e3() ->
    {add, 
     {const,3},
     {divi,
      {var,"r"},
      {const,4}}}.

calc({const,N},_E) ->
    {val,N};

calc({var,Id},E) ->
    case maps:find(Id,E) of
        {ok,N} -> 
            {val,N};
        error ->
            throw(unbound_identifier_error)
    end;

calc({add,E1,E2},E) ->
    {val, N1} = calc(E1,E),
    {val, N2} = calc(E2,E),
    {val, N1+N2};

calc({sub,E1,E2},E) ->
    {val, N1} = calc(E1,E),
    {val, N2} = calc(E2,E),
    {val, N1-N2};

calc({mult,E1,E2},E) ->
    {val, N1} = calc(E1,E),
    {val, N2} = calc(E2,E),
    {val, N1*N2};

calc({divi,E1,E2},E) ->
    {val, N1} = calc(E1,E),
    {val, N2} = calc(E2,E),
    case N2 == 0 of
        true ->
            throw(division_by_zero_error);
        false ->
            {val, N1 div N2}
    end.

