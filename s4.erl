
keysearch(Search_term, Search_value, Lst)->
    case lists:keysearch(Search_term, Search_value, Lst) of
        {value,Result} -> Result;
        false -> error
    end.
get_ship(Shipping_State, Ship_ID) ->
    keysearch(Ship_ID,#ship.id,(Shipping_State)#shipping_state.ships).

get_container(Shipping_State, Container_ID) ->
    keysearch(
        Container_ID,
        #container.id,
        (Shipping_State)#shipping_state.containers
    ).

get_port(Shipping_State, Port_ID) ->
    keysearch(
        Port_ID,
        #port.id, 
        (Shipping_State)#shipping_state.ports
    ).
    
get_occupied_docks(Shipping_State, Port_ID) ->
    lists:filtermap(fun(Shipping_Location) -> 
                {ID,Dock,_Ship_ID} = Shipping_Location, 
                    case ID of 
                        Port_ID -> {true, Dock};
                        _ -> false
                    end
                end,
    (Shipping_State)#shipping_state.ship_locations).

get_ship_location(Shipping_State, Ship_ID) ->
     [H] = lists:filtermap(fun(Shipping_Location) -> 
                {Port_ID,Dock,Docked_Ship_ID} = Shipping_Location, 
                    case Docked_Ship_ID of 
                        Ship_ID -> {true, {Port_ID,Dock}};
                        _ -> false
                    end
                end,
    (Shipping_State)#shipping_state.ship_locations),
    H.
   

get_container_weight(Shipping_State, Container_IDs) ->
    lists:foldr(
        fun(ID,Acc) -> 
            Acc + (get_container((Shipping_State),ID))#container.weight end, 
        0, 
        Container_IDs
    ).

get_ship_weight(Shipping_State, Ship_ID) ->
    get_container_weight(
        (Shipping_State),
        maps:get(
            Ship_ID,
            (Shipping_State)#shipping_state.ship_inventory
        )
    ).
    
load_containers(Shipping_State, Ship_ID, Containers) ->
    {Location,_Dock} = get_ship_location(Shipping_State,Ship_ID),
    {ok, 
        #shipping_state{
            ships = Shipping_State#shipping_state.ships, 
            containers = Shipping_State#shipping_state.containers, 
            ports = Shipping_State#shipping_state.ports, 
            ship_locations = Shipping_State#shipping_state.ship_locations,
            ship_inventory = maps:put(
                Ship_ID,
                maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory)++Containers,
                Shipping_State#shipping_state.ship_inventory
            ),
            port_inventory = maps:put(
                Location,
                maps:get(Location,Shipping_State#shipping_state.port_inventory)--Containers,
                Shipping_State#shipping_state.port_inventory
            )
        }
    }.

is_valid_container_amount(Shipping_State, Ship_ID, Containers) ->
    (get_ship(Shipping_State,Ship_ID))#ship.container_cap - length(maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory)) - length(Containers) >= 0.

containers_have_valid_dock(Shipping_State,Ship_ID,Containers) ->
    {Location,_Dock} = get_ship_location(Shipping_State,Ship_ID),
    Port_Inventory = maps:get(
                Location,
                Shipping_State#shipping_state.port_inventory
            ),
    lists:foldr(fun(Container, Is_Sublist) ->
            lists:member(Container, Port_Inventory) and Is_Sublist
        end,
        true,
        Containers
    ).
    
check_if_able_to_add_containers(Shipping_State, Ship_ID, Containers) ->
    case is_valid_container_amount(Shipping_State, Ship_ID, Containers) of
        true -> case containers_have_valid_dock(Shipping_State, Ship_ID, Containers) of
                    true -> true;
                    false -> false
                end;
        false -> false
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    case check_if_able_to_add_containers(Shipping_State, Ship_ID, Container_IDs) of  
        true -> load_containers(Shipping_State, Ship_ID,Container_IDs);
        false -> error
    end.

remove_containers(Shipping_State, Ship_ID, Containers) ->
    {Location,_Dock} = get_ship_location(Shipping_State,Ship_ID),
    {ok, 
        #shipping_state{
            ships = Shipping_State#shipping_state.ships, 
            containers = Shipping_State#shipping_state.containers, 
            ports = Shipping_State#shipping_state.ports, 
            ship_locations = Shipping_State#shipping_state.ship_locations,
            ship_inventory = maps:put(
                Ship_ID,
                maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory)--Containers,
                Shipping_State#shipping_state.ship_inventory
            ),
            port_inventory = maps:put(
                Location,
                maps:get(Location,Shipping_State#shipping_state.port_inventory)++Containers,
                Shipping_State#shipping_state.port_inventory
            )
        }
    }.
   
check_if_able_to_remove_containers(Shipping_State, Ship_ID, Containers) ->
    Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),
    lists:foldr(
        fun(Container, Is_Sublist) ->
            lists:member(Container, Ship_Inventory) and Is_Sublist
        end,
        true,
        Containers
    ).
    
unload_ship(Shipping_State, Ship_ID, Containers) ->
    case check_if_able_to_remove_containers(Shipping_State, Ship_ID, Containers) of
        true -> remove_containers(Shipping_State, Ship_ID, Containers);
        false -> error
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    unload_ship(
        Shipping_State, 
        Ship_ID,
        maps:get(
            Ship_ID,
            Shipping_State#shipping_state.ship_inventory
        )
    ).

new_ship_locations(Locations, Ship_ID, {Port_ID, Dock}) ->
    lists:foldr(
        fun({Current_Port_ID,Current_Dock,Current_Ship_ID},Ship_Locations) ->
            case Current_Ship_ID of
                Ship_ID -> [{Port_ID,Dock,Ship_ID}|Ship_Locations];
                _ ->[{Current_Port_ID,Current_Dock,Current_Ship_ID}|Ship_Locations]
            end
        end,
        [],
        Locations
    ).

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
     {ok, 
        #shipping_state{
            ships = Shipping_State#shipping_state.ships, 
            containers = Shipping_State#shipping_state.containers, 
            ports = Shipping_State#shipping_state.ports, 
            ship_locations = new_ship_locations(
                Shipping_State#shipping_state.ship_locations, 
                Ship_ID, 
                {Port_ID, Dock}
            ),
            ship_inventory = Shipping_State#shipping_state.ship_inventory,
            port_inventory = Shipping_State#shipping_state.port_inventory
        }
    }.
     

