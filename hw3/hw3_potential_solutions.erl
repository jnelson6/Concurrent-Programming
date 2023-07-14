
%% 4. get_occupied_docks: method returns a list of all the occupied docks for a given port.
%%                        It returns the empty list if the port id does not exist.
%%              example: shipping:get_occupied_docks(shipping:shipco(),3).
%%                     >>  [’C’]
%%              example: shipping:get_occupied_docks(shipping:shipco(),23).
% %%                     >>  []
get_occupied_docks(Shipping_State, Port_ID) ->
  OccupiedDocks = lists:filter(fun(Docks) when element(1,Docks) == Port_ID ->  true; (_) -> false end, Shipping_State#shipping_state.ship_locations),
  lists:map(fun(Docks) -> element(2,Docks) end, OccupiedDocks).




%% 6. get_container_weight:  method returns the total weight of ALL of the container ids in the list Container IDs. 
%%                          It returns the atom error if any of the container Ids does not exist.
%%                example: shipping:get_container_weight(shipping:shipco(),[3,5]). 
%%                       >> 243
%%                example: shipping:get_container_weight(shipping:shipco(),[89,3,5]). 
%%                       >> error
get_container_weight(Shipping_State, Container_IDs) ->
IDs = (fun(C) -> get_container(Container_IDs, #shipping_state.containers) end),%lists:member(C#container.id, Container_IDs) end),
  case IDs of error -> error; 
    _ ->
    Results = lists:filter( IDs, Shipping_State#shipping_state.containers),
    Weights = lists:map(fun(C) -> element(3,C) end, Results), 
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, Weights) end.

%%%

%%%


% case get_container(Shipping_State, Container_IDs) of error -> error;
%   true ->  IDs = (fun(C) -> lists:member(C#container.id, Container_IDs) end),
%   Results = lists:filter( IDs, Shipping_State#shipping_state.containers),
%   Weights = lists:map(fun(C) -> element(3,C) end, Results), 
%   lists:foldl(fun(K, Sum) -> K + Sum end, 0, Weights) end.

