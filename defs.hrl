% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   server: name or pid of server you are connected to.
% chatrooms = list of names of channels the client is in
% nick = name of the client
-record(client_st, {gui, server, chatrooms, nick}).

% This record defines the structure of the server process.
% name = name of the server, chatrooms = list of names of started channels
% clients = list of names of clients connected
-record(server_st, {name, chatrooms, clientnicks, clientpids}).

% Defines the structure of a channel process
% name = name of channel, clients = list of Pids of clients in channel
-record(channel_st, {name, clients}).