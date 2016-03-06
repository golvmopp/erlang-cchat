-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
	% Chatrooms is lists of Channel names
	% clients are just the nicks on the server, regardless of chatroom
    #server_st{name = ServerName, chatrooms = [], clientnicks = [], clientpids = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick, Pid}) ->
	OnServer = lists:member(Nick, St#server_st.clientnicks),
	if not OnServer -> 
	    NewState = St#server_st {clientnicks = [Nick|St#server_st.clientnicks], 
	    						 clientpids  = [Pid|St#server_st.clientpids]},
	    {reply, "Connected" , NewState};
	true ->
		{reply, "Failed", St}
	end;

handle(St, {disconnect, Nick}) ->
	% Client can only send disconnect request if it's connected
	ClientsConnected = lists:delete(Nick, St#server_st.clientnicks),
	NewState = St#server_st {clientnicks = ClientsConnected},
	{reply, ok, NewState};

handle(St, {join, Pid, Channel}) ->
	ChannelExists = lists:member(Channel, St#server_st.chatrooms),
	if (not ChannelExists) ->
		% Start a process with genserver, running the channel.
    	genserver:start(list_to_atom(Channel), channel:initial_state(Channel, Pid), fun channel:handle/2),
		NewState = St#server_st {chatrooms = [Channel|St#server_st.chatrooms]},
		{reply, ok, NewState};
	true -> 
		% If it exists, just add the client to the running channel
		C = list_to_atom(Channel),
		genserver:request(C, {add, Pid}),
		{reply, ok, St}
	end;

handle(St, {work, Fun, Inputs}) -> 
	Assignments = assign_tasks(St#server_st.clientpids, Inputs),
	Result = [genserver:request(C, {work, Fun, I},infinity) || {C,I} <- Assignments],
	{reply, Result, St}.


assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].