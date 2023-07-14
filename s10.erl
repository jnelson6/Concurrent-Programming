
get_ship(Shipping_State, Ship_ID) ->
    case lists:filter(fun (R) -> R#ship.id==Ship_ID end, Shipping_State#shipping_state.ships) of
        [] -> error; % no ship with given id
        [R] -> R
    end.

get_container(Shipping_State, Container_ID) ->
    case lists:filter(fun (R) -> R#container.id==Container_ID end, Shipping_State#shipping_state.containers) of
        [] -> error; % no container with given id
        [R] -> R
    end.

get_port(Shipping_State, Port_ID) ->
    case lists:filter(fun (R) -> R#port.id==Port_ID end, Shipping_State#shipping_state.ports) of
        [] -> error; % no port with given id"
        [R] -> R
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    case lists:filter(fun ({Port, _, _}) -> Port==Port_ID end, Shipping_State#shipping_state.ship_locations) of
        [] -> error; % no occupied docks
        Docks -> lists:map(fun ({_, Dock, _}) -> Dock end, Docks)
    end.

get_ship_location(Shipping_State, Ship_ID) ->
    case lists:filter(fun ({_, _, Ship}) -> Ship==Ship_ID end, Shipping_State#shipping_state.ship_locations) of
        [] -> error; % ship not found
        [{Port, Dock, _}] -> {Port, Dock}
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    lists:foldl(fun (C, Sum) -> (case get_container(Shipping_State, C) of
        R -> R#container.weight;
        error -> error % some containers not found
    end) + Sum end, 0, Container_IDs).

get_ship_weight(Shipping_State, Ship_ID) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        {ok, Cs} -> lists:foldl(fun (C, Sum) -> get_container_weight(Shipping_State, [C]) + Sum end, 0, Cs);
        error -> error % ship not found
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    ShipConts = case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end,
    ShipCap = case get_ship(Shipping_State, Ship_ID) of R -> R#ship.container_cap end,
    ShipOk = lists:flatlength(Container_IDs) + lists:flatlength(ShipConts) < ShipCap,
    case Container_IDs of
        [] -> Shipping_State;
        [C|Cs] -> case lists:member(C, case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, X} -> X end) of
            true when ShipOk -> % current container (C) is at ship's port; call load_ship using updated record values
                NewPort = lists:delete(C, case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end),
                NewShip = lists:append(case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end, [C]),
                NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
                load_ship(NewSS, Ship_ID, Cs);
            _ -> error % container is not at the same port as the ship OR requested containers exceed container cap
        end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    PortConts = case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end,
    ShipConts = case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end,
    PortCap = case get_port(Shipping_State, P) of R -> R#port.container_cap end,
    PortOk = lists:flatlength(ShipConts) + lists:flatlength(PortConts) < PortCap,
    case PortOk of
        true -> % all containers on the ship can be unloaded
            NewPort = lists:append(case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end, ShipConts),
            NewShip = [],
            NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
            NewSS;
        _ -> error % ship containers exceed port cap
    end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    PortConts = case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end,
    PortCap = case get_port(Shipping_State, P) of R -> R#port.container_cap end,
    PortOk = lists:flatlength(Container_IDs) + lists:flatlength(PortConts) < PortCap,
    case Container_IDs of
        [] -> Shipping_State;
        [C|Cs] -> case lists:member(C, case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, X} -> X end) of
            true when PortOk -> % current container (C) is loaded on the ship; call unload_ship using updated record values
                NewPort = lists:append(case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end, [C]),
                NewShip = lists:delete(C, case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end),
                NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
                unload_ship(NewSS, Ship_ID, Cs);
            _ -> error % container is not loaded on the ship OR requested containers exceed port cap
        end
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    PortsDocks = lists:map(fun ({P, D, _}) -> {P, D} end, Shipping_State#shipping_state.ship_locations),
    case lists:member({Port_ID, Dock}, PortsDocks) of
        true -> error; % port & dock combination is occupied
        _ -> case lists:filter(fun ({_, _, S}) -> S==Ship_ID end, Shipping_State#shipping_state.ship_locations) of % find location with Ship_ID
            [] -> NewShipLoc = lists:append(Shipping_State#shipping_state.ship_locations, [{Port_ID, Dock, Ship_ID}]);
            [T] -> NewShipLoc = lists:append(lists:delete(T, Shipping_State#shipping_state.ship_locations), [{Port_ID, Dock, Ship_ID}])
        end,
        NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
        ship_locations=NewShipLoc, ship_inventory=Shipping_State#shipping_state.ship_inventory, port_inventory=Shipping_State#shipping_state.port_inventory},
        NewSS
    end.

