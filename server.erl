-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
	% Chatrooms is lists of Channel names
	% clients are just the pids on the server, regardless of chatroom
    #server_st{name = ServerName, chatrooms = [], clients = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick}) ->
	B = lists:member(Nick, St#server_st.clients),
	if not B -> 
	    NewState = St#server_st {clients = [Nick|St#server_st.clients]},
	    {reply, "Connected" , NewState};
	true ->
		{reply, "Failed", St}
	end;

handle(St, {disconnect, Nick}) ->
	ClientsConnected = lists:delete(Nick, St#server_st.clients),
	NewState = St#server_st {clients = ClientsConnected},
	{reply, Nick, NewState};

handle(St, {join, Pid, Channel}) ->
	Chatroom = lists:member(Channel, St#server_st.chatrooms),
	if (not Chatroom) ->
    	genserver:start(list_to_atom(Channel), channel:initial_state(Channel, Pid), fun channel:handle/2),
		NewState = St#server_st {chatrooms = [Channel|St#server_st.chatrooms]},
		{reply, ok, NewState};
	true -> 
		C = list_to_atom(Channel),
		genserver:request(C, {add, Pid}),
		{reply, ok, St}
	end.