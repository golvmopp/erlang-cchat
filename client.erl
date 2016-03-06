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
            Pid = self(),
            try genserver:request(ServerAtom, {connect, St#client_st.nick, Pid}) of
                "Connected" -> 
                    NewState = St#client_st {server = Server},
                    {reply, ok, NewState};
                "Failed" -> 
                    {reply, {error, nick_taken, "Please change username"}, St}
            catch
                _:_ -> {reply, {error, server_not_reached, "Server unavailible on connect."}, St}
            end
    end;
    

%% Disconnect from server
handle(St, disconnect) ->
    if
        St#client_st.server == "" -> 
            {reply, {error, user_not_connected, "Not connected to a server"}, St};
        St#client_st.chatrooms /= [] ->
            {reply, {error, leave_channels_first, "Must leave all chatrooms before disconnect"}, St};
        true ->
            ServerAtom = list_to_atom(St#client_st.server),
            try genserver:request(ServerAtom, {disconnect, St#client_st.nick}) of
                _ ->
                    {reply, ok, St#client_st {server = ""}}
            catch
                _:_ -> {reply, {error, server_not_reached, "Server unavailible on disconnect."}, St}
            end
    end;

% Join channel
handle(St, {join, Channel}) ->
    InChannel = lists:member(Channel, St#client_st.chatrooms),
    if
        St#client_st.server == "" ->
            {reply, {error, user_not_connected, "Not connected to a server"}, St};
        InChannel ->
            {reply, {error, user_already_joined, "Already in the chatroom."}, St};
        true ->
             %join chatroom
             ServerAtom = list_to_atom(St#client_st.server),
             Pid = self(),
             try genserver:request(ServerAtom, {join, Pid, Channel}) of
                _ ->
                    % Add Channel to list of channels joined
                    NewState = St#client_st {chatrooms = [Channel|St#client_st.chatrooms]},
                    {reply, ok, NewState}
            catch 
                _ -> {reply, {error, server_not_reached, "Server unavailible on channel join."}, St}
            end
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    InChannel = lists:member(Channel, St#client_st.chatrooms),
    if 
        not InChannel ->
            {reply, {error, user_not_joined, "You can't leave a channel you're not in."}, St};
        true -> 
            ChannelAtom = list_to_atom(Channel),
            Pid = self(),
            % Requesting directly to channel, to avoid server bottleneck
            try genserver:request(ChannelAtom, {remove, Pid}) of
                _ ->
                    % Remove the channel from channels joined
                    NewRooms = lists:delete(Channel, St#client_st.chatrooms),
                    NewState = St#client_st {chatrooms = NewRooms},
                    {reply, ok, NewState}
            catch
                _ -> {reply, {error, server_not_reached, "Server unavailible on channel leave."}, St}
            end
    end;



% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    InChannel = lists:member(Channel, St#client_st.chatrooms),
    if 
        not InChannel ->
            {reply, {error, user_not_joined, "You can't write to a channel you're not in."}, St};
        true -> 
            ChannelAtom = list_to_atom(Channel),
            Pid = self(),
            % Requesting directly to channel, to avoid server bottleneck
            % Spawning a process for each message sent
            try spawn(fun() -> genserver:request(ChannelAtom, {message, St#client_st.nick, Msg, Pid}) end) of
                _ ->
                    {reply, ok, St}
            catch
                _ -> {reply, {error, server_not_reached, "Channel/server unavailible on send msg."}, St}
            end
    end;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    if
        St#client_st.server == "" ->
            NewState = St#client_st {nick = Nick},
            {reply, ok, NewState};
        true -> 
            {reply, {error, user_already_connected, "Can't change nick while connected."}, St}
    end;

%% Incoming message (not changed)
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St};

handle(St, {work, Fun, Input}) ->
    Result = Fun(Input),
    {reply,Result,St}.

    %% below doesn't seem to be faster than the above..
    %Pid = self(),
    %Proc = spawn_link(fun() -> Pid ! {self(), Fun(Input)} end),
    %receive 
    %    {Proc, Result} -> 
    %        {reply, Result, St}
    %end.

    % Self = self(),
%    Pids = [ spawn_link(fun() -> Self ! {self(), {X, fact(X)}} end)
%            || X <- lists:seq(1, N) ],
%    [ receive {Pid, R} -> R end || Pid <- Pids ].