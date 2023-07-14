get_ship(Shipping_State, Ship_ID) ->
    case lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships) of
        false -> error;
        Record -> Record
    end.

get_container(Shipping_State, Container_ID) ->
    case lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers) of
        false -> error;
        Record -> Record
    end.

get_port(Shipping_State, Port_ID) ->
    case lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports) of
        false -> error;
        Record -> Record
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    lists:filtermap(fun(Ship_Location) -> 
        {ID, Dock, Ship_ID}=Ship_Location,
            case ID of
                Port_ID -> {true, Dock};
                _ -> false
            end
        end,
    (Shipping_State)#shipping_state.ship_locations).

% helper function to get_ship_location to check if the ship actually exists
print_ship_location([]) -> error;
print_ship_location([H]) -> H.

get_ship_location(Shipping_State, Ship_ID) ->
    print_ship_location(lists:filtermap(fun(Ship_Location) ->
        {Port_ID, Dock, ID}=Ship_Location,
            case ID of
                Ship_ID -> {true, {Port_ID,Dock}};
                _ -> false
            end
        end,
    (Shipping_State)#shipping_state.ship_locations)).

% helper function to get_container_weight to check whether all the containers actually exist
weight_helper(Shipping_State, []) -> ok;
weight_helper(Shipping_State, [H|T]) ->
    case get_container(Shipping_State, H) of
        error -> error;
        _ -> weight_helper(Shipping_State, T)
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    L=weight_helper(Shipping_State, Container_IDs),
    if L==error ->
        error;
    true ->
    case Container_IDs of
        [] -> 0;
        _ ->
            lists:foldl(fun(ID,Sum) ->
                Sum+(get_container(Shipping_State,ID))#container.weight 
                    end,
                0, Container_IDs)
            end
    end.

get_ship_weight(Shipping_State, Ship_ID) ->
    case get_ship(Shipping_State, Ship_ID) of
        error -> error;
        _ -> get_container_weight(Shipping_State, 
            maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory))
        end.

% helper function to load_ship to check all the containers and the ship are all at the same port
all_same_port(Shipping_State, Ship_ID, Container_IDs) ->
    {Port,Dock}=get_ship_location(Shipping_State,Ship_ID),
    Port_Inventory=maps:get(Port, Shipping_State#shipping_state.port_inventory),
    lists:foldl(fun(Container, Sublist) -> lists:member(Container,Port_Inventory) and Sublist end,
    true, Container_IDs).

% helper function to load_ship to see if the containers will fit in the ship
ship_over_capacity(Shipping_State, Ship_ID, Container_IDs) ->
    ((get_ship(Shipping_State,Ship_ID))#ship.container_cap 
        - length(maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)) 
        - length(Container_IDs)) < 0.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    case all_same_port(Shipping_State, Ship_ID, Container_IDs) of
        false -> error;
        true -> case ship_over_capacity(Shipping_State,Ship_ID, Container_IDs) of
            true -> error;
            false -> {Port,Dock}=get_ship_location(Shipping_State, Ship_ID),
            {ok, #shipping_state{
                                ships=Shipping_State#shipping_state.ships,
                                containers=Shipping_State#shipping_state.containers,
                                ports=Shipping_State#shipping_state.ports,
                                ship_locations=Shipping_State#shipping_state.ship_locations,
                                ship_inventory=maps:put(Ship_ID,
                                    lists:append(maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory), Container_IDs),
                                    Shipping_State#shipping_state.ship_inventory
                                ),
                                port_inventory=maps:put(Port,
                                    lists:filter(fun (List) -> not lists:member(List, Container_IDs) end, 
                                    maps:get(Port, Shipping_State#shipping_state.port_inventory)),
                                    Shipping_State#shipping_state.port_inventory)
                                }
                            }
                end
    end.

% helper function to unload_ship to check all the given containers are actually on the ship
containers_are_on_the_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_Inventory=maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    lists:foldl(fun(Container, Sublist) -> lists:member(Container, Ship_Inventory) and Sublist end,
    true, Container_IDs).

% helper function to unload_ship to check if all the containers can fit at the port
port_over_capacity(Shipping_State, Ship_ID, Container_IDs) ->
    {Port, Dock}=get_ship_location(Shipping_State, Ship_ID),
    ((get_port(Shipping_State, Port))#port.container_cap
        - length(maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)) 
        - length(Container_IDs)) < 0.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    case containers_are_on_the_ship(Shipping_State, Ship_ID, Container_IDs) of
        false -> io:format("The given containers are not all on the same ship...\n"),
            error;
        true -> case port_over_capacity(Shipping_State, Ship_ID, Container_IDs) of
            true -> error;
            false -> {Port,Dock}=get_ship_location(Shipping_State, Ship_ID),
            {ok, #shipping_state{
                                ships=Shipping_State#shipping_state.ships,
                                containers=Shipping_State#shipping_state.containers,
                                ports=Shipping_State#shipping_state.ports,
                                ship_locations=Shipping_State#shipping_state.ship_locations,
                                ship_inventory=maps:put(Ship_ID,
                                    lists:filter(fun (List) -> not lists:member(List, Container_IDs) end,
                                    maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)),
                                    Shipping_State#shipping_state.ship_inventory
                                ),
                                port_inventory=maps:put(Port,
                                    lists:append(maps:get(Port, Shipping_State#shipping_state.port_inventory), Container_IDs), 
                                    Shipping_State#shipping_state.port_inventory)
                                }
                            }
                end
    end.

% after unload_ship because it is easier and less code to write 
% to just have it do the same call for every container on the ship 
% than to do unload_ship_all first and then have to edit it for unload_ship, 
% without actually being able to use all of it
unload_ship_all(Shipping_State, Ship_ID) ->
    unload_ship(Shipping_State, Ship_ID, maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)).

% helper function to set_sail to change the ship_location attribute of the map.
new_location(Shipping_State, Ship_ID, {Port_ID, Dock}, Ship_Location) ->
    lists:foldr(fun({Current_Port,Current_Dock,Current_Ship}, Location) ->
                    case Current_Ship of
                        Ship_ID -> [{Port_ID, Dock, Ship_ID}|Location];
                        _ -> [{Current_Port,Current_Dock,Current_Ship}|Location]
                    end
                end,
    [], Ship_Location).

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Occupied_Docks=get_occupied_docks(Shipping_State, Port_ID),
    case lists:member(Dock, Occupied_Docks) of
        true -> error;
        false -> {ok, #shipping_state{
                                    ships=Shipping_State#shipping_state.ships,
                                    containers=Shipping_State#shipping_state.containers,
                                    ports=Shipping_State#shipping_state.ports,
                                    ship_locations=new_location(Shipping_State, Ship_ID, {Port_ID, Dock}, Shipping_State#shipping_state.ship_locations),
                                    ship_inventory=Shipping_State#shipping_state.ship_inventory,
                                    port_inventory=Shipping_State#shipping_state.port_inventory
                                }
                            }
    end.