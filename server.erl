-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
	% chatrooms on the form list of tuples with key "roomname" and list of 
	% Pids of users in the chatroom.
	% An element looks like this: {#hobbits, [pid1, pid2, pid4]}, dunno what a pid looks like.
	% clients are just the pids on the server, regardless of chatroom
    #server_st{name = ServerName, chatrooms = [], clients = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Pid}) ->
    NewState = St#server_st {clients = [Pid|St#server_st.clients]},
    {reply, "Connected" , NewState};

handle(St, {disconnect, Pid}) -> 
	{reply, Pid, St}.
