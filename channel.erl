-module(channel).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(Name, Client) ->
	% Channel must be started with an initial client
    #channel_st{name = Name, clients = [Client]}.

handle(St, {add, Pid}) ->
	% Add the Pid to the list of clients
	NewList = [Pid | St#channel_st.clients],
	NewSt = St#channel_st {clients = NewList},
	{reply, ok, NewSt};

handle(St, {remove, Pid}) -> 
	% Remove the Pid from the list of clients
	NewSt = St#channel_st {clients = lists:delete(Pid, St#channel_st.clients)},
	{reply, ok, NewSt};

handle(St, {message, Nick, Msg, Pid}) ->
	ListOfPids = lists:delete(Pid, St#channel_st.clients), % Can't send to yourself
	% Spawn process for broadcasted message
	[spawn(fun() -> genserver:request(P, {incoming_msg, St#channel_st.name, Nick, Msg}) end) || P <- ListOfPids],
	{reply, ok, St}. 