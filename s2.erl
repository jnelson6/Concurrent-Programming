-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
  lists:keyfind(ShipID, 1, Shipping_State#shipping_state.ships).

get_container(Shipping_State, Container_ID) ->
  lists:keyfind(Container_ID, 1, Shipping_State#shipping_state.container).
    

get_port(Shipping_State, Port_ID) ->
   lists:keyfind(Port_ID, 1,Shipping_State#shipping_state.ports).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_occupied_docks_helper([], Port_ID) ->
  ok;
get_occupied_docks_helper([H|T], Port_ID) ->
  {P, D, S} = H,
  if P == Port_ID -> D.
  ; get_occupied_docks_helper(T)
  end.
get_occupied_docks(Shipping_State, Port_ID) ->
    get_occupied_docks_helper(Shipping_State#shipping_state.ship_locations, Port_ID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ship_location_helper([], Ship_ID) ->
  ok;
get_ship_location_helper([H|T], Ship_ID) ->
  {P, D, S} = H
  if S == Ship_ID  -> {P, D}.
  ; get_ship_location_helper(T, Ship_ID)
  end.
get_ship_location(Shipping_State, Ship_ID) ->
    get_ship_location_helper(Shipping_State#shipping_state.ship_locations, Ship_ID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_container_weight_sum_helper([]) ->
  0;

get_container_weight_sum_helper([H|T]) ->
  H + get_container_weight_helper(T).

get_container_weight_helper(Weights, [], Shipping_State)->
  ok;
get_container_weight_helper(Weights, [H|T], Shipping_State) ->
  {I, W} = get_container(Shipping_State, H),
  get_container_weight_helper(Weights++[W], T, Shipping_State).

get_container_weight(Shipping_State, Container_IDs) ->
   WeightContainers = get_container_weight_helper([], Container_IDs, Shipping_State),
   get_container_weight_sum_helper(WeightContainers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ship_weight(Shipping_State, Ship_ID) ->
    get_container_weight(Shipping_State, maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_ship_helper(Shipping_State, []) ->
  Shipping_State.

load_ship_helper(Shipping_State, [H|T]) ->
  {V, M} = maps:take(H, Shipping_State#shipping_state.port_inventory),
  {V2, M2} = maps:put(V, Shipping_State#shipping_state.ship_inventory),

  ShipsN = Shipping_State#shipping_state.ships,
  ContainersN = Shipping_State#shipping_state.containers,
  PortsN = Shipping_State#shipping_state.ports,
  ShipLocsN = Shipping_State#shipping_state.ship_locations,
  ShipInvN = M2,
  PortInvN = M,

  Shipping_StateN = #shipping_state{ships = ShipsN, containers = ContainersN, ports = PortsN, ship_locations = ShipLocsN, ship_inventory = ShipInvN, port_inventory = PortInvN},
  load_ship_helper(Shipping_StateN, T).

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {I, N, C} = get_ship(Shipping_State, Ship_ID),
    Container_Max = C,
    if (length(Container_IDs) + maps:size(Shipping_State#shipping_state.ship_inventory)) > C -> error.
   
    Shipping_State2 = load_ship_helper(Shipping_State, Container_IDs),
    Shipping_State2
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unload_ship_all(Shipping_State, Ship_ID) ->
   {P,D} = get_ship_location(Shipping_State, Ship_ID),
   Port = get_port(Shipping_State, P),
   if(maps:size(Shipping_State#shipping_state.ship_inventory + maps:size(Shipping_State#shipping_state.port_inventory) > Port#port.container_cap)) -> error.
   orelse
    Container_IDs = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    Shipping_State2 = unload_ship_helper(Shipping_State, Container_IDs),
    Shipping_State2
 end.

 unload_ship_helper(Shipping_State, []) -> 
 Shipping_State.

 unload_ship_helper(Shipping_State, [H|T]) ->
   {V, M} = maps:take(H, Shipping_State#shipping_state.ship_inventory),
   {V2, M2} = maps:put(V, Shipping_State#shipping_state.port_inventory),

   ShipsN = Shipping_State#shipping_state.ships,
   ContainersN = Shipping_State#shipping_state.containers,
   PortsN = Shipping_State#shipping_state.ports,
   ShipLocsN = Shipping_State#shipping_state.ship_locations,
   ShipInvN = M2,
   PortInvN = M,
   Shipping_StateN = #shipping_state{ships = ShipsN, containers = ContainersN, ports = PortsN, ship_locations = ShipLocsN, ship_inventory = ShipInvN, port_inventory = PortInvN},
   unload_ship_helper(Shipping_StateN, T).

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
  {P,D} = get_ship_location(Shipping_State, Ship_ID),
  Port = get_port(Shipping_State, P),
  if(length(Container_IDs) + maps:size(Shipping_State#shipping_state.port_inventory) > Port#port.container_cap) -> 
    error.
  orelse
    Shipping_State2 = unload_ship_helper(Shipping_State, Container_IDs),
    Shipping_State2
  end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    OccDock = get_occupied_docks(Shipping_State, Port_ID),
    if(lists:member(Dock , OccDock) == true) ->
     error.
    orelse
    CurLoc = get_ship_location(Shipping_State, Ship_ID),
    NewShipLoc = lists:delete(CurLoc, Shipping_State#shipping_state.ship_locations),
    NewShipLoc2 = lists:append(NewShipLoc, [{Port_ID, Dock}]),

   ShipsN = Shipping_State#shipping_state.ships,
   ContainersN = Shipping_State#shipping_state.containers,
   PortsN = Shipping_State#shipping_state.ports,
   ShipLocsN = NewShipLoc2,
   ShipInvN = Shipping_State#shipping_state.ship_inventory,
   PortInvN = Shipping_State#shipping_state.port_inventory,

   Shipping_State2 = #shipping_state{ships = ShipsN, containers = ContainersN, ports = PortsN, ship_locations = ShipLocsN, ship_inventory = ShipInvN, port_inventory = PortInvN},
   Shipping_State2
 end.
