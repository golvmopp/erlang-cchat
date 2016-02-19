% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   server: name or pid of server you are connected to.
-record(client_st, {gui, server, chatrooms, nick}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {name, chatrooms, clients}).
