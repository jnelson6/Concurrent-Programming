-module(client).

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
	    {{dummy_target, dummy_response}, State};

	    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IS THIS WHERE I DO WHOAMI%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.























%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
	GUI = whereis(list_to_atom(State#cl_st.gui)),
	% 2. The client checks in its cl_st record to see if it is already in the chatroom. 
	% If the client is already in the chatroom identified by ChatName, then the message 
	% {result, self(), Ref, err} [A] should be sent back to the GUI, and the following steps should be skipped. 
	case maps:is_key(ChatName, State#cl_st.con_ch) of  
		true -> 
			%io:format("User already in chat: ~s ~n", [ChatName]),
			GUI!{result, self(), Ref, err}, {ok, State};		%% instead of err do {err, State};
			%New_Chats = State#cl_st.con_ch;			% update connected chats..
		% 3. Otherwise, if the client is not in the chatroom identified by ChatName, 
		% the client should ask the server to join said chatroom. 
		% The client will send the message {self(), Ref, join, ChatName} [B] to the server.
		false -> 
			whereis(server)!{self(), Ref, join, ChatName},	
			%io:format("User added to chat: ~s ~n", [ChatName]),
			% 9. The client will receive the message, update its record of connected chatrooms, and send the message 
			% {result, self(), Ref, History} [A] back to the GUI, where History is the chatroom history received from 
			% the chatroom process.
			receive 
				{From, Ref, connect, History} ->
					GUI!{result, self(), Ref, History},
					%  updated map of connected chats 
					New_Chats = maps:put(ChatName, From, State#cl_st.con_ch),	
					% update state 
					New_State = State#cl_st{ gui = State#cl_st.gui,  
    									nick = State#cl_st.nick,  
    									con_ch = New_Chats },
    				{ok, New_State} 
    		end
	end.

	% GuiPid = whereis(list_to_atom(State#cl_st.gui),
	% case maps:find(ChatName, State#cl_st.con_ch) of
	% 	{ok, _} -> GuiPid ! {result, self(), Ref, err}, {ok, State}; % client is already in chat
	% 	_ -> whereis(server) ! {self(), Ref, join, ChatName}, % ask server to join specified chat
	% 		receive
	% 			{From, Ref, connect, ChatHist} -> % chatroom sent chat history, send to gui
	% 				GuiPid ! {result, self(), Ref, ChatHist},
	% 				{ok, State#cl_st{gui=State#cl_st.gui, nick=State#cl_st.nick, con_ch=maps:put(ChatName, From, State#cl_st.con_ch)}}
	% 		end
	% end.













%% executes `/leave` protocol from client perspective
	ServerPid = whereis(server),
do_leave(State, Ref, ChatName) ->
	GuiPid = whereis(list_to_atom(State#cl_st.gui)),
	%% check if client is in the chatroom
	case maps:find(ChatName, State#cl_st.con_ch) of
		{ok, _} -> ServerPid ! {self(), Ref, leave, ChatName}, % request to leave from server
			receive
				{ServerPid, Ref, ack_leave} -> % client removes chatroom from its connected channels, sends message to gui
					GuiPid ! {result, self(), Ref, ok},
					{ok, State#cl_st{gui=State#cl_st.gui, nick=State#cl_st.nick, con_ch=maps:remove(ChatName, State#cl_st.con_ch)}}
			end;
		_ -> GuiPid ! {result, self(), Ref, err}, {ok, State} % send error result to gui
	end.

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
	ServerPid = whereis(server),
	GuiPid = whereis(list_to_atom(State#cl_st.gui)),
	CurNick = State#cl_st.nick,
	case NewNick of
		CurNick -> GuiPid ! {result, self(), Ref, err_same}, {ok, State};
		_ -> ServerPid ! {self(), Ref, nick, NewNick},
			receive
				{ServerPid, Ref, err_nick_used} -> % server says nickname is already in use
					GuiPid ! {result, self(), Ref, err_nick_used}, {ok, State};
				{ServerPid, Ref, ok_nick} -> % server changed nickname, update gui
					GuiPid ! {result, self(), Ref, ok_nick}, {ok, State#cl_st{gui=State#cl_st.gui, nick=NewNick, con_ch=State#cl_st.con_ch}}
			end
	end.

%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
	GuiPid = whereis(list_to_atom(State#cl_st.gui)),
	RoomPid = case maps:find(ChatName, State#cl_st.con_ch) of {ok, R} -> R end,
	RoomPid ! {self(), Ref, message, Message}, % send outgoing message to chatroom
	receive
		{RoomPid, Ref, ack_msg} -> GuiPid ! {result, self(), Ref, {msg_sent, State#cl_st.nick}},
		{ok, State}
	end.

%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
	ServerPid = whereis(server),
	GuiPid = whereis(list_to_atom(State#cl_st.gui)),
	ServerPid ! {self(), Ref, quit},
	receive
		{ServerPid, Ref, ack_quit} -> GuiPid ! {self(), Ref, ack_quit}
	end,
    {ok, State}.







