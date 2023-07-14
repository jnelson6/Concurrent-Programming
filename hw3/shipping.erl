% Julia Nelson
% 11/10/2022
% "I pledge my honor that I have abided by the Stevens Honor System."

-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

%% 1. get_ship: returns a ship record for the given id. 
%%              If the ship does not exist, it returns the atom error.
%%
%%           example:    shipping:get_ship(shipping:shipco(),1)  
%%                    >> returns the ship whose id is 1. 
get_ship(Shipping_State, Ship_ID) ->
    Results = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if Results == false -> error;
      true -> Results end.

%% 2. get_container: method returns a container record for the given id. 
%%                   It returns the atom error if the container id does not exist.
%%
%%           exmaple: shipping:get_container(shipping:shipco(),4).
%%                  >>  #container{id = 4,weight = 62}
%%           example: shipping:get_container(shipping:shipco(),47).
%%                  >>  error
get_container(Shipping_State, Container_ID) ->
    Results = lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers),
    if Results == false -> error;
      true -> Results end.

%% 3. get port: method returns a port records for the given id. 
%%              It returns the atom error if the port id does not exist.
%%          example:  shipping:get_port(shipping:shipco(),3). 
%%                 >>  #port{id = 3,name = "Miami",
%%                           docks = [’A’,’B’,’C’,’D’],
%%                           container_cap = 200}
%%          example: shipping:get_port(shipping:shipco(),12). 
%%                >>  error
get_port(Shipping_State, Port_ID) ->
    Results = lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports),
    if Results == false -> error;
      true -> Results end.


%% 4. get_occupied_docks: method returns a list of all the occupied docks for a given port.
%%                        It returns the empty list if the port id does not exist.
%%              example: shipping:get_occupied_docks(shipping:shipco(),3).
%%                     >>  [’C’]
%%              example: shipping:get_occupied_docks(shipping:shipco(),23).
% %%                     >>  []
get_occupied_docks(Shipping_State, Port_ID) ->
  lists:map(fun(OccuppiedDocks) -> element(2,OccuppiedDocks) end, 
           ( lists:filtermap( fun(Ships_at_docks) when element(1,Ships_at_docks) =:= Port_ID ->  true; 
                                    (_) -> false end, Shipping_State#shipping_state.ship_locations))).




%% 5. get_ship_location: method returns the location, {Port ID, Dock ID}, of a given ship. 
%%                      It returns the atom error if the ship id does not exist. 
%%              example: shipping:get_ship_location(shipping:shipco(),3). 
%%                    >> {1,’A’}
%%              example: shipping:get_ship_location(shipping:shipco(),23). 
%%                    >> error
get_ship_location(Shipping_State, Ship_ID) ->
  Results = lists:keyfind(Ship_ID, 3, Shipping_State#shipping_state.ship_locations),
  if Results == false -> error;
    true -> {element(1,Results),element(2,Results)} end.






%% 6. get_container_weight:  method returns the total weight of ALL of the container ids in the list Container IDs. 
%%                          It returns the atom error if any of the container Ids does not exist.
%%                example: shipping:get_container_weight(shipping:shipco(),[3,5]). 
%%                       >> 243
%%                example: shipping:get_container_weight(shipping:shipco(),[89,3,5]). 
%%                       >> error
get_container_weight(Shipping_State, Container_IDs) ->
IDs = (fun(C) -> lists:member(C#container.id, Container_IDs) end),
  case IDs of error -> error; 
    _ ->
    Results = lists:filter( IDs, Shipping_State#shipping_state.containers),
    Weights = lists:map(fun(C) -> element(3,C) end, Results), 
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, Weights) end.




%% 7. get_ship_weight:  method returns the total weight of a ship, measured by the total weight of the ship’s containers. 
%%                      It returns the atom error if ship Id does not exist.
%%              example: shipping:get_ship_weight(shipping:shipco(),2).
%%                      >> 676
%%              example: shipping:get_ship_weight(shipping:shipco(),22).
%%                      >> error
get_ship_weight(Shipping_State, Ship_ID) ->
case get_ship(Shipping_State, Ship_ID) of error -> error;
  _ ->  Ship_Inv_List = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
  get_container_weight(Shipping_State, Ship_Inv_List) end.






%% 8. load_ship:  method returns a shipping state in which the containers in the list of Container IDs are moved 
%%                from a port to the ship with the given Ship ID. 
%%%               - Make sure that all the containers are at the same port as the ship they are loading onto. 

%%                - In the case that loading the ship would put the ship over capacity, return an atom error 
%%                                                                and do not add any containers to the ship. 

%%                Also return error if there are container IDs that aren’t in the same port as the ship. 
%%%%        Here is an example. 
%%                Notice how containers 16, 18 and 20 have been removed from dock 1 and loaded onto ship 1.
%%%         shipping:load_ship(shipping:shipco(), 1, [16,18,20]).
%%%%          >>      
load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_to_load = get_ship(Shipping_State, Ship_ID),
    case Ship_to_load of error -> error;
        _ ->
            P_ID = element(1, get_ship_location(Shipping_State, Ship_ID)), 
            % P_ID = Shipping_State#port.id,
            Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
            Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),  %% Ship_ID may be Ship_ID#ship.id,
            Current_Ship_inventory = length(Ship_Inventory),
            Amount_to_load = length(Container_IDs),

            case is_sublist(Port_Inventory, Container_IDs) of false -> error; 
              true -> 
                %% check that length to load is okay
                if Ship_to_load#ship.container_cap < (Current_Ship_inventory + Amount_to_load) ->  error; 
                  true -> 
                    Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),  %% Ship_ID may be Ship_ID#ship.id,
                    Updated_Ship = maps:map(fun(X, Y) -> if (Ship_ID == X) -> Y ++ Container_IDs; true -> Y end end, Shipping_State#shipping_state.ship_inventory),
                    Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
                    Updated_Port = maps:map(fun(X, Y) -> if (P_ID == X) -> Y -- Container_IDs; true -> Y end end, Shipping_State#shipping_state.port_inventory),


                    {ok, #shipping_state{
                            ships = Shipping_State#shipping_state.ships, 
                            containers = Shipping_State#shipping_state.containers, 
                            ports = Shipping_State#shipping_state.ports, 
                            ship_locations = Shipping_State#shipping_state.ship_locations,
                            ship_inventory = Updated_Ship,
                            port_inventory = Updated_Port}}

                  end
            end
    end.







%% 9. unload_ship_all: This method returns a shipping state in which all of the containers that are on a given ship are offloaded to the port in which the ship is docked. 
%%                     In the case that offloading to a port would put the port over capacity, return an error and do not unload any containers to the port.
unload_ship_all(Shipping_State, Ship_ID) ->
    Ship_to_unload = get_ship(Shipping_State, Ship_ID),
    case Ship_to_unload of error -> error;
        _ ->
            P_ID = element(1, get_ship_location(Shipping_State, Ship_ID)), 
            Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
            Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),  
            Current_Port_inventory = length(Port_Inventory),
            Amount_to_unload = length(Ship_Inventory),

                %% check that length to load is okay
                if P_ID#ship.container_cap < (Current_Port_inventory + Amount_to_unload) ->  error; 
                  true -> 
                    Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory), 
                    Updated_Ship = maps:map(fun(X, Y) -> if (Ship_ID == X) -> Y -- Ship_Inventory; true -> Y end end, Shipping_State#shipping_state.ship_inventory),
                    Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
                    Updated_Port = maps:map(fun(X, Y) -> if (P_ID == X) -> Y ++ Ship_Inventory; true -> Y end end, Shipping_State#shipping_state.port_inventory),


                    {ok, #shipping_state{
                            ships = Shipping_State#shipping_state.ships, 
                            containers = Shipping_State#shipping_state.containers, 
                            ports = Shipping_State#shipping_state.ports, 
                            ship_locations = Shipping_State#shipping_state.ship_locations,
                            ship_inventory = Updated_Ship,
                            port_inventory = Updated_Port}}
            end
    end.








%% 10. unload_ship: This method returns a shipping state in which the given containers on a ship are offloaded to the port in which the ship is docked. 
%%                  Make sure that all the containers are located on the ship. 
%%                  In the case that offloading to a port would put the port over capacity, return an error and do not offload any containers to the port.
unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_to_unload = get_ship(Shipping_State, Ship_ID),
    case Ship_to_unload of error -> error;
        _ ->
            P_ID = element(1, get_ship_location(Shipping_State, Ship_ID)), 
            Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
            Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),  
            Current_Port_inventory = length(Port_Inventory),
            Amount_to_unload = length(Ship_Inventory),

                %% check that length to load is okay
                if P_ID#ship.container_cap < (Current_Port_inventory + Amount_to_unload) ->  error; 
                  true -> 
                    Ship_Inventory = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory), 
                    Updated_Ship = maps:map(fun(X, Y) -> if (Ship_ID == X) -> Y -- Container_IDs; true -> Y end end, Shipping_State#shipping_state.ship_inventory),
                    Port_Inventory = maps:get(P_ID, Shipping_State#shipping_state.port_inventory),
                    Updated_Port = maps:map(fun(X, Y) -> if (P_ID == X) -> Y ++ Container_IDs; true -> Y end end, Shipping_State#shipping_state.port_inventory),

                    case is_sublist(Ship_Inventory, Container_IDs) of false -> error; 
                    true -> 
                      {ok, #shipping_state{
                            ships = Shipping_State#shipping_state.ships, 
                            containers = Shipping_State#shipping_state.containers, 
                            ports = Shipping_State#shipping_state.ports, 
                            ship_locations = Shipping_State#shipping_state.ship_locations,
                            ship_inventory = Updated_Ship,
                            port_inventory = Updated_Port}}
                    end

            end
    end.











%% set_sail: This method changes the given ship’s port and dock location to the new port and dock location. 
%%           Be sure to check whether or not the new port and dock is occupied. If it is, then return the atom error.
set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
  Docks = get_occupied_docks(Shipping_State, Port_ID),




  case Docks of error -> error; 
    true -> 
      Updated_location = lists:append( (lists:delete( get_ship_location(Shipping_State, Ship_ID), Shipping_State#shipping_state.ship_locations)), {Port_ID, Dock}),
 
      {ok, #shipping_state{
            ships = Shipping_State#shipping_state.ships, 
            containers = Shipping_State#shipping_state.containers, 
            ports = Shipping_State#shipping_state.ports, 
            ship_locations = Updated_location,
            ship_inventory = Shipping_State#shipping_state.ship_inventory,
            port_inventory = Shipping_State#shipping_state.port_inventory
           }
      } end.
     














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
