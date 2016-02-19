-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, server = "" , chatrooms = [], nick = Nick}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% reqeumulsting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    if 
        St#client_st.server == Server -> 
            {reply, {error, user_already_connected, "Already connected."}, St};
        true ->
            ServerAtom = list_to_atom(Server),
            try genserver:request(ServerAtom, {connect, St#client_st.nick}) of
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
        St#client_st.chatrooms /= [] ->
            {reply, {error, leave_channels_first, "Must leave all chatrooms before disconnect"}, St};
        true ->
            ServerAtom = list_to_atom(St#client_st.server),
            try genserver:request(ServerAtom, {disconnect, St#client_st.gui}) of
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
    B = lists:member(Channel, St#client_st.chatrooms),
    if
        %kolla så man är ansluten till en server.
        St#client_st.server == "" ->
            {reply, {error, user_not_connected, "Not connected to a server"}, St};
        %kolla om man redan är med i chattrummet, då error.
        B ->
            {reply, {error, user_already_joined, "Already in chatroom."}, St};
        true ->
             %join chatroom
             ServerAtom = list_to_atom(St#client_st.server),
             try genserver:request(ServerAtom, {join, St#client_st.gui, Channel}) of
                Response ->
                    io:fwrite("Client joined channel: ~p~n", [Response]),
                    OldChatrooms = St#client_st.chatrooms,
                    NewState = St#client_st {chatrooms = [Channel|OldChatrooms]},
                    {reply, ok, NewState}
            catch 
                %try catch fel från servern.
                _:_ -> {reply, {error, server_not_reached, "Server unavailible."}, St}
            end
    end;
    %FÖR SERVERN SENARE:
    %kolla om chattrummet finns på servern, om nej, skapa chattrum
    %not list:member(Channel, St#server_st.chatrooms) ->
    %   OldChatrooms = St#server_st.chatrooms,
    %  NewState = St#server_st {chatrooms = Channel:OldChatrooms},

%% Leave channel
handle(St, {leave, Channel}) ->
    B = lists:member(Channel, St#client_st.chatrooms),
    if 
        not B ->
            {reply, {error, user_not_joined, "You can't leave a channel you're not in."}, St};
        true -> 
            ServerAtom = list_to_atom(St#client_st.server),
            try genserver:request(ServerAtom, {leave, St#client_st.gui, Channel}) of
                _ ->
                    NewRooms = lists:delete(Channel, St#client_st.chatrooms),
                    NewState = St#client_st {chatrooms = NewRooms},
                    {reply, ok, NewState}
            catch
                _:_ -> {reply, {error, server_not_reached, "Server unavailible."}, St}
            end
    end;



% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    if
        St#client_st.server == "" ->
            NewState = St#client_st {nick = Nick},
            {reply, ok, NewState};
        true -> 
            {reply, {error, user_already_connected, "Can't change nick while connected."}, St}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.