get_ship(Shipping_State, Ship_ID) ->
  lists:keyfind(Ship_ID, 2, Shipping_State#shipping_state.ships).

get_container(Shipping_State, Container_ID) ->
  lists:keyfind(Container_ID, 2, Shipping_State#shipping_state.containers).

get_port(Shipping_State, Port_ID) ->
  lists:keyfind(Port_ID, 2, Shipping_State#shipping_state.ports).

get_occupied_docks(Shipping_State, Port_ID) ->
  lists:map(fun(Y) -> element(2, Y) end, lists:filter(fun(X) -> element(1, X) == Port_ID end, Shipping_State#shipping_state.ship_locations)).

get_ship_location(Shipping_State, Ship_ID) ->
  X = lists:keyfind(Ship_ID, 3, Shipping_State#shipping_state.ship_locations),
  {element(1, X), element(2, X)}.




get_container_weight(Shipping_State, Container_IDs) ->
  lists:foldl(fun(K, Sum) -> K + Sum end, 0, lists:map(fun(I) -> I#container.weight end, lists:flatten(lists:map(fun(X) -> lists:filter(fun(Y) -> Y#container.id == X end, Shipping_State#shipping_state.containers) end, Container_IDs)))).










get_ship_weight(Shipping_State, Ship_ID) ->
  get_container_weight(Shipping_State, maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)).

get_container_port(Shipping_State, Container_ID) -> % Container has to be in a port
  lists:nth(1, maps:keys(maps:filter(fun(_K, V) -> lists:member(Container_ID, V) end, Shipping_State#shipping_state.port_inventory))).

get_container_ship(Shipping_State, Container_ID) ->
  Result = maps:keys(maps:filter(fun(_K, V) -> lists:member(Container_ID, V) end, Shipping_State#shipping_state.ship_inventory)),
  if
    Result == [] ->
      55;
    true ->
      lists:nth(1, Result)
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
  Old_Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
  Ship = get_ship(Shipping_State, Ship_ID),
  Port = get_container_port(Shipping_State, lists:nth(1, Container_IDs)),
  Test = lists:filter(fun(Container) -> get_container_port(Shipping_State, Container) /= Port end, Container_IDs),
  if
    length(Old_Ship_Inventory) + length(Container_IDs) > Ship#ship.container_cap ->
      error;
    Test /= [] ->
      error;
    true ->
      New_Ship_Inventory = lists:append(Old_Ship_Inventory, Container_IDs),
      New_Port_Inventory = lists:filter(fun (Elem) -> not lists:member(Elem, Container_IDs) end, maps:get(Port, Shipping_State#shipping_state.port_inventory)),

      Result = Shipping_State#shipping_state{ship_inventory = maps:update(Ship_ID, New_Ship_Inventory, Shipping_State#shipping_state.ship_inventory),
      port_inventory = maps:update(Port, New_Port_Inventory, Shipping_State#shipping_state.port_inventory)},
      Result
  end.

unload_ship_all(Shipping_State, Ship_ID) ->
  Port = get_port(Shipping_State, element(1, get_ship_location(Shipping_State, Ship_ID))),
  Old_Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
  Old_Port_Inventory = maps:get(Port#port.id, Shipping_State#shipping_state.port_inventory),
  if
    length(Old_Port_Inventory) + length(Old_Ship_Inventory) > Port#port.container_cap ->
      error;
    true ->
      New_Ship_Inventory = [],
      New_Port_Inventory = lists:append(Old_Port_Inventory, Old_Ship_Inventory),
      Result = Shipping_State#shipping_state{ship_inventory = maps:update(Ship_ID, New_Ship_Inventory, Shipping_State#shipping_state.ship_inventory),
      port_inventory = maps:update(Port#port.id, New_Port_Inventory, Shipping_State#shipping_state.port_inventory)},
      Result
  end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
  Port = get_port(Shipping_State, element(1, get_ship_location(Shipping_State, Ship_ID))),
  Old_Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
  Old_Port_Inventory = maps:get(Port#port.id, Shipping_State#shipping_state.port_inventory),
  Test = lists:filter(fun(Container) -> get_container_ship(Shipping_State, Container) /= Ship_ID end, Container_IDs),
  if
    length(Old_Port_Inventory) + length(Container_IDs) > Port#port.container_cap ->
      error;
    Test /= [] ->
      error;
    true ->
      New_Ship_Inventory = lists:filter(fun (Elem) -> not lists:member(Elem, Container_IDs) end, Old_Ship_Inventory),
      New_Port_Inventory = lists:append(Old_Port_Inventory, Container_IDs),
      Result = Shipping_State#shipping_state{ship_inventory = maps:update(Ship_ID, New_Ship_Inventory, Shipping_State#shipping_state.ship_inventory),
      port_inventory = maps:update(Port#port.id, New_Port_Inventory, Shipping_State#shipping_state.port_inventory)},
      Result
    end.


set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
  Old_Ship_Locations = Shipping_State#shipping_state.ship_locations,
  New_Ship_Locations = lists:keyreplace(Ship_ID, 3, Old_Ship_Locations, {Port_ID, Dock, Ship_ID}),
  Occupied_Locations = lists:map(fun(X) -> get_ship_location(Shipping_State, X) end, [1, 2, 3, 4, 5]),
  case lists:member({Port_ID, Dock}, Occupied_Locations) of
    true ->
      error;
    false ->
      Result = Shipping_State#shipping_state{ship_locations = New_Ship_Locations},
      Result
    end.
