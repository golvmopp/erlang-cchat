-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, server = "" , chatrooms = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    if 
        St#client_st.server == Server -> 
            {reply, {error, user_already_connected, "Already connected."}, St};
        true ->
            ServerAtom = list_to_atom(Server),
            try genserver:request(ServerAtom, St#client_st.gui) of
                Response -> 
                    io:fwrite("Client received: ~p~n", [Response]),
                    NewState = St#client_st {server = Server},
                    {reply, ok, NewState}
            catch
                _:_ -> {reply, {error, server_not_reached, "Server unavailible."}, St}
            end
    end;
    

%% Disconnect from server
handle(St, disconnect) ->
    if
        %kolla så att man faktiskt är ansluten till en server
        St#client_st.server == "" -> 
            {reply, {error, user_not_connected, "Not connected to a server"}, St};
        %kolla så att chatrooms är tom, annars error.
        St#client_st.chatrooms == [] ->
            {reply, {error, leave_channels_first, "Must leave all chatrooms before disconnect"}, St};
        true ->
            ServerAtom = list_to_atom(St#client_st.server),
            try genserver:request(ServerAtom, St#client_st.gui) of
                Response ->
                    io:fwrite("Client disconnected: ~p~n", [Response]),
                    NewState = St#client_st {server = ""},
                    {reply, ok, NewState}
            %Try-catch fel från servern.
            catch
                _:_ -> {reply, {error, server_not_reached, "Server unavailible."}, St}
            end
    end;

% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
