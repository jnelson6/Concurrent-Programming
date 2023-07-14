%% eb8 exercise 1 

%% Exercise 1. Implement the turnstile example given at the beginning of the course. You should have a
%% counter process and two turnstile processes. The turnstile thread should send a {bump} message to the counter
%% thread so that the counter can increment its local counter. The counter should also support a {read,From}
%% that sends back the value of the counter to the process with pid From.
%% ex1.erl name of file if we call the function called ex1 
%% going to run through shell with start 


-module(ex1).
-compile(export_all).

start(N) -> %% Spawns a counter and N turnstile clients
	C = spawn(?MODULE,counter_server,[0]),
	[ spawn(?MODULE,turnstile,[ C,50] ) || _ <- lists:seq(1,N)],
	C.

counter_server(State) -> %% State is the current value of the counter
	recieve 
		{bump} ->
			counter_server(State+1);
			{read,From} -> 
				{FROM! State}
				%% NEED TO FIN
				}


turnstile(C,0) -> %% C is the PID of the counter , and N the number of times the turnstile turns
	done;

turnstile(C,N) when N>0  -> 
	C!{bump},
	turnstile(C,N-1).




	%% c(ex1)		 {ok,ex1}