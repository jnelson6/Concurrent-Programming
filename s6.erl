error_if_false(false)->
    error;
error_if_false(Other)->
    Other.

% Question 1 %
get_ship(Shipping_State, Ship_ID) ->
    error_if_false(get_ship_from_ships(Shipping_State#shipping_state.ships, Ship_ID)).

get_ship_from_ships(Ships, Ship_ID) ->
    lists:keyfind(Ship_ID, #ship.id, Ships).

% Question 2 %
get_container(Shipping_State, Container_ID) ->
    error_if_false(get_container_from_containers(Shipping_State#shipping_state.containers, Container_ID)).

get_container_from_containers(Containers, Container_ID)->
    lists:keyfind(Container_ID, #container.id, Containers).

% Question 3 %
get_port(Shipping_State, Port_ID) ->
    error_if_false(get_port_from_ports(Shipping_State#shipping_state.ports, Port_ID)).

get_port_from_ports(Ports, Port_ID) ->
    lists:keyfind(Port_ID, #port.id, Ports).

% Question 4 %
is_same_port_then_dock({Port, Dock, _Ship}, Port_ID)->
    if Port == Port_ID ->
        {true, Dock};
    true ->
        false
    end.
get_locations(Shipping_State) ->
    Shipping_State#shipping_state.ship_locations.

get_occupied_docks(Shipping_State, Port_ID) ->
    lists:filtermap(fun(Elem)->is_same_port_then_dock(Elem, Port_ID) end, get_locations(Shipping_State)).

% Question 5 %
is_same_ship_then_port_and_dock({Port, Dock, Ship}, Ship_ID) ->
    if Ship =:= Ship_ID ->
        {Port, Dock};
    true ->
        false
    end.

find_ship([], _Ship_ID)->
    false;
find_ship([H|T], Ship_ID) ->
    Res = is_same_ship_then_port_and_dock(H, Ship_ID),
    if Res == false ->
        find_ship(T, Ship_ID);
    true ->
        Res
    end.

get_ship_location(Shipping_State, Ship_ID) ->
    error_if_false(find_ship(get_locations(Shipping_State), Ship_ID)).

% Question 6 %
get_one_container_weight(Shipping_State, Container_ID) ->
    Res = get_container(Shipping_State, Container_ID),
    if Res == error ->
        error;
    true ->
        Res#container.weight
    end.

sum_all_containers_weight(_Shipping_State, []) ->
    0;
sum_all_containers_weight(Shipping_State, [H|T]) ->
    Cur = get_one_container_weight(Shipping_State, H), 
    Acc = sum_all_containers_weight(Shipping_State, T),
    if (Cur == error) or (Acc == error) ->
        error;
    true ->
        Cur+Acc
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    sum_all_containers_weight(Shipping_State, Container_IDs).

% Question 7 %
get_ship_weight(Shipping_State, Ship_ID) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        {ok, Containers} ->
            get_container_weight(Shipping_State, Containers);
        error ->
            error
        end.

% Question 8 %
load_success(Shipping_State, Ship_ID, Container_IDs) ->
    case is_not_over_capacity(Shipping_State, Ship_ID, Container_IDs) of
        false ->
            false;
        true ->
            case get_ship_location(Shipping_State, Ship_ID) of 
                {Port, _Dock} ->
                    Containers_At_Ports = element(2,maps:find(Port, Shipping_State#shipping_state.port_inventory)),
                    case is_sublist(Containers_At_Ports, Container_IDs) of
                        false -> 
                            false;
                        true ->
                            NewP = remove_containers_from_port(Containers_At_Ports, Container_IDs),
                            Ship_id_inventory = element(2,maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory)),
                            NewS = load_containers_to_ship(Ship_id_inventory, Container_IDs),
                            New_Shipping_State = Shipping_State#shipping_state{
                                ships = Shipping_State#shipping_state.ships,
                                containers = Shipping_State#shipping_state.containers,
                                ports = Shipping_State#shipping_state.ports,
                                ship_locations = Shipping_State#shipping_state.ship_locations,
                                ship_inventory = maps:put(Ship_ID ,NewS,Shipping_State#shipping_state.ship_inventory),
                                port_inventory = maps:put(Port, NewP, Shipping_State#shipping_state.port_inventory)
                                },
                            {ok,New_Shipping_State}
                    end;
                error ->
                    false
            end
    end.


load_containers_to_ship(Ship_id_inventory, Container_IDs)->
    lists:append(Ship_id_inventory, Container_IDs).
    
remove_containers_from_port(Containers_At_Ports, Container_IDs)->
    lists:subtract(Containers_At_Ports, Container_IDs).

    
is_not_over_capacity(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_Cap = (get_ship(Shipping_State, Ship_ID))#ship.container_cap,
    Cur_Ship_Weight = length(element(2,maps:find(Ship_ID,Shipping_State#shipping_state.ship_inventory))),
    
    if Cur_Ship_Weight + length(Container_IDs) > Ship_Cap ->
        false;
    true ->
        true
    end.


load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    case load_success(Shipping_State, Ship_ID, Container_IDs) of
        false -> 
            error;
        New_Shipping_State->
            {ok,New_Shipping_State}
    end.


% Question 9 %
if_ship_is_vaild_then_unload(Shipping_State, Ship_ID, Containers_on_ship, New_containers_on_ship) ->
    case get_ship_location(Shipping_State, Ship_ID) of
        {Port, _Dock} ->
            Containers_on_port = element(2,maps:find(Port, Shipping_State#shipping_state.port_inventory)),
            Port_capacity = (get_port(Shipping_State, Port))#port.container_cap,
            if Port_capacity < length(Containers_on_ship) + length(Containers_on_port) ->
                error;
            true ->
                New_containers_on_port = lists:append(Containers_on_port, Containers_on_ship),
                New_Shipping_State = Shipping_State#shipping_state{
                    ships = Shipping_State#shipping_state.ships,
                    containers = Shipping_State#shipping_state.containers,
                    ports = Shipping_State#shipping_state.ports,
                    ship_locations = Shipping_State#shipping_state.ship_locations,
                    ship_inventory = maps:put(Ship_ID ,New_containers_on_ship,Shipping_State#shipping_state.ship_inventory),
                    port_inventory = maps:put(Port, New_containers_on_port, Shipping_State#shipping_state.port_inventory)
                    },
                {ok,New_Shipping_State}
            end;
        error ->
            error
    end.

unload_ship_all(Shipping_State, Ship_ID) -> 
    
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        error->
            error;
        {ok,Containers_on_ship}->
            if_ship_is_vaild_then_unload(Shipping_State, Ship_ID, Containers_on_ship,[])
    end.    
   

% Question 10 %
unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        error->
            error;
        {ok,Containers_on_ship}->
            case is_sublist(Containers_on_ship, Container_IDs) of
                false -> 
                    error;
                true -> 
                    if_ship_is_vaild_then_unload(Shipping_State, Ship_ID, Container_IDs, lists:subtract(Containers_on_ship, Container_IDs))
            end
            
    end.    

% Question 11 
is_same({Port, Dock, _Ship}, {Port_ID, Dock_ID}) ->
    if (Port == Port_ID) and (Dock == Dock_ID) ->
        true;
    true ->
        false
    end.

my_filter([],{_Port_ID, _Dock})->
    [];
my_filter([H|T], {Port_ID, Dock})->
    case is_same(H,{Port_ID, Dock}) of
        true->
            my_filter(T,{Port_ID, Dock});
        false->
            
            lists:append([H], my_filter(T, {Port_ID, Dock}))
    end.

check_is_no_occupied([], {_Port_ID, _Dock_ID})->
    true;
check_is_no_occupied([H|T], {Port_ID, Dock_ID})->
    case is_same(H, {Port_ID, Dock_ID}) of
        true->
            error;
        false->
            check_is_no_occupied(T, {Port_ID, Dock_ID})
    end.
    
set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    
    Ship_locations = Shipping_State#shipping_state.ship_locations,
    case check_is_no_occupied(Ship_locations,{Port_ID, Dock}) of
        error->
            error;
        true->
            {Ship_at_port, Ship_at_dock} = get_ship_location(Shipping_State, Ship_ID),
            Removed_old_dock = my_filter(Ship_locations, {Ship_at_port, Ship_at_dock}),
            
            New_ship_locations = lists:append(Removed_old_dock, {Port_ID, Dock, Ship_ID}),
            
                New_Shipping_State = Shipping_State#shipping_state{
                    ships = Shipping_State#shipping_state.ships,
                    containers = Shipping_State#shipping_state.containers,
                    ports = Shipping_State#shipping_state.ports,
                    ship_locations = New_ship_locations,
                    ship_inventory = Shipping_State#shipping_state.ship_inventory,
                    port_inventory = Shipping_State#shipping_state.port_inventory
                    },
                {ok,New_Shipping_State}
    end.
            

