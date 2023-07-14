get_ship(Shipping_State, Ship_ID) ->
    ShipList = lists:filter(fun (Ship) -> Ship#ship.id == Ship_ID end, Shipping_State#shipping_state.ships),
    if
        length(ShipList) > 0 ->
            hd(ShipList);
        true ->
            error
    end.

    

get_container(Shipping_State, Container_ID) ->
    ContainerList = lists:filter(fun (Container) -> Container#container.id == Container_ID end, Shipping_State#shipping_state.containers),
    if
        length(ContainerList) > 0 ->
            hd(ContainerList);
        true ->
            error
    end.
    

get_port(Shipping_State, Port_ID) ->
        PortList = lists:filter(fun (Port) -> Port#port.id == Port_ID end, Shipping_State#shipping_state.ports),
        if
            length(PortList) > 0 ->
                hd(PortList);
            true ->
                error
        end.

get_occupied_docks(Shipping_State, Port_ID) ->
    OccupiedDocksAtPort = lists:filter(fun({P, _D, _S}) -> P == Port_ID end, Shipping_State#shipping_state.ship_locations),
    [D || {_P, D, _S} <- OccupiedDocksAtPort].


get_ship_location(Shipping_State, Ship_ID) ->
    Location = lists:filter(fun({_P, _D, S}) -> S == Ship_ID end, Shipping_State#shipping_state.ship_locations),
    if
        length(Location) > 0 ->
            LocTup = hd(Location),
            {element(1, LocTup), element(2, LocTup)};
        true ->
            error
    end.  

get_container_weight(Shipping_State, Container_IDs) ->
    lists:foldr(fun(X, Sum) -> X#container.weight + Sum end, 0, lists:filter(fun(Container) -> lists:member(Container#container.id, Container_IDs) end, Shipping_State#shipping_state.containers)).

get_ship_weight(Shipping_State, Ship_ID) ->
    case get_ship(Shipping_State, Ship_ID) of
        error -> error;
        _Good -> get_container_weight(Shipping_State, maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory))
    end.


load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortInventory = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),
            ContainersAtPort = lists:filter(fun(ID) -> lists:member(ID, PortInventory) end, Container_IDs),
            ShipContCap = Ship#ship.container_cap,
            Curr_Ship_Inventory = maps:get(Ship#ship.id, Shipping_State#shipping_state.ship_inventory),
            ShipCurrCont = length(Curr_Ship_Inventory),
            if
                ShipCurrCont + length(ContainersAtPort) > ShipContCap ->
                    error;
                true ->
                    if
                        length(ContainersAtPort) =/= length(Container_IDs) ->
                            io:format("Not all containers at port...\n"),
                            error;
                        true ->
                            load_ship(Shipping_State, Ship_ID, Port_ID, ContainersAtPort)
                    end
            end
    end.

load_ship(Shipping_State, Ship_ID, Port_ID, ContainersAtPort) ->
    NewShipInv = maps:map(fun(K, V) -> if
                K == Ship_ID -> V ++ ContainersAtPort;
                true -> V
                end
            end, Shipping_State#shipping_state.ship_inventory),

    NewPortInv = maps:map(fun(K, V) -> if
                K == Port_ID -> V -- ContainersAtPort;
                true -> V
                end
            end, Shipping_State#shipping_state.port_inventory),
    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                    containers = Shipping_State#shipping_state.containers,
                    ports = Shipping_State#shipping_state.ports,
                    ship_locations = Shipping_State#shipping_state.ship_locations,
                    ship_inventory = NewShipInv,
                    port_inventory = NewPortInv}}.







unload_ship_all(Shipping_State, Ship_ID) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortCap = (get_port(Shipping_State, Port_ID))#port.container_cap,
            PortInventory = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),
            ShipInventories = Shipping_State#shipping_state.ship_inventory,
            ShipInventory = maps:get(Ship_ID, ShipInventories),
            Curr_Ship_Inventory = maps:get(Ship#ship.id, Shipping_State#shipping_state.ship_inventory),
            ShipCurrCont = length(Curr_Ship_Inventory),
            if
                ShipCurrCont + length(PortInventory) > PortCap ->
                    error;
                true ->
                    NewShipInv = maps:map(fun(K, V) -> if
                                K == Ship_ID -> [];
                                true -> V
                                end
                            end, ShipInventories),
                    NewPortInventory = maps:map(fun(K, V) -> if
                                K == Port_ID -> V ++ ShipInventory;
                                true -> V
                                end
                            end, Shipping_State#shipping_state.port_inventory),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                                    containers = Shipping_State#shipping_state.containers,
                                    ports = Shipping_State#shipping_state.ports,
                                    ship_locations = Shipping_State#shipping_state.ship_locations,
                                    ship_inventory = NewShipInv,
                                    port_inventory = NewPortInventory}}
            end
        end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    case Ship of
        error -> error;
        _Good ->
            {Port_ID, _Dock} = (get_ship_location(Shipping_State, Ship_ID)),
            PortCap = (get_port(Shipping_State, Port_ID))#port.container_cap,
            CurrPortInventory = length(maps:get(Port_ID, Shipping_State#shipping_state.port_inventory)),
            ShipInventories = Shipping_State#shipping_state.ship_inventory,
            ShipInventory = maps:get(Ship_ID, ShipInventories),
            ContainersOnShip = lists:filter(fun(C) -> lists:member(C, ShipInventory) end, Container_IDs),
            if
                length(ContainersOnShip) + CurrPortInventory > PortCap ->
                    error;
                length(ContainersOnShip) /= length(Container_IDs) ->
                    io:format("The given conatiners are not all on the same ship...\n"),
                    error;
                true ->
                    NewShipInv = maps:map(fun(K, V) -> if
                                K == Ship_ID -> V -- ContainersOnShip;
                                true -> V
                                end,
                            end, ShipInventories),
                    NewPortInventory = maps:map(fun(K, V) -> if
                                K == Port_ID -> V ++ ContainersOnShip;
                                true -> V
                                end
                            end, Shipping_State#shipping_state.port_inventory),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                                    containers = Shipping_State#shipping_state.containers,
                                    ports = Shipping_State#shipping_state.ports,
                                    ship_locations = Shipping_State#shipping_state.ship_locations,
                                    ship_inventory = NewShipInv,
                                    port_inventory = NewPortInventory}}
            end
        end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Ship = get_ship(Shipping_State, Ship_ID),
    IsRealPort = get_port(Shipping_State, Port_ID),
    case {IsRealPort, Ship} of
        {error, _Else} -> error;
        {_Else, error} -> error;
        {_, _} ->
            IsRealDock = lists:member(Dock, (get_port(Shipping_State, Port_ID))#port.docks),
            IsOccupiedDock = lists:member(Dock, get_occupied_docks(Shipping_State, Port_ID)),
            case {IsRealDock, IsOccupiedDock} of
                {true, false} ->
                    NewLocations = lists:map(
                        fun({P, D, S}) -> if
                                            S == Ship_ID -> {Port_ID, Dock, Ship_ID};
                                            true -> {P, D, S}
                                          end
                                        end, Shipping_State#shipping_state.ship_locations),
                    {ok, #shipping_state{ships = Shipping_State#shipping_state.ships,
                            containers = Shipping_State#shipping_state.containers,
                            ports = Shipping_State#shipping_state.ports,
                            ship_locations = NewLocations,
                            ship_inventory = Shipping_State#shipping_state.ship_inventory,
                            port_inventory = Shipping_State#shipping_state.port_inventory}};
                {_, _} -> error
            end
    end.
                                