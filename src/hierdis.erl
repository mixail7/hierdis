% (The MIT License)

% Copyright (c) 2013 Nathan Aschbacher

% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the
% 'Software'), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish,
% distribute, sublicense, and/or sell copies of the Software, and to
% permit persons to whom the Software is furnished to do so, subject to
% the following conditions:

% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(hierdis).
-author('Nathan Aschbacher <nathan@basho.com>').

-export([connect/2,
         connect/3,
         connect_unix/1,
         connect_unix/2,
         command/2,
         command/3,
         pipeline/2,
         pipeline/3,
         transaction/2,
         transaction/3,
         append_command/2,
         append_command/3,
         get_reply/1,
         get_reply/2,
         set_timeout/2]).

-include("hierdis.hrl").

-type pipe() :: [ok | {error, term()}].

-on_load(init/0).

-spec init() -> ok | error().
init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", "hierdis"]);
                         _ ->
                             filename:join("../priv", "hierdis")
                     end;
                 Dir ->
                     filename:join(Dir, "hierdis")
             end,
    erlang:load_nif(SoName, 0).


%% @doc: Connects to Redis on ip:port (timeout in milliseconds can be passed as
%% a 3rd parameter). Default timeout: 0 (unlimited).
-spec connect(string(), integer()) -> {'ok', binary()} | error().
connect(_Ip, _Port) ->
    erlang:nif_error({error, not_loaded}).

-spec connect(string(), integer(), non_neg_integer()) -> {'ok', binary()} | error().
connect(_Ip, _Port, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Connects to Redis via unix domain socket (timeout in milliseconds can
%% be passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec connect_unix(string()) -> {'ok', binary()} | error().
connect_unix(_SocketPath) ->
    erlang:nif_error({error, not_loaded}).

-spec connect_unix(string(), non_neg_integer()) -> {'ok', binary()} | error().
connect_unix(_SocketPath, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Executes given command ("GET", "SET", etc.; timeout in milliseconds
%% can be passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec command(context(), iolist()) -> {'ok', term()} | error().
command(_Context, _CommandArgs) ->
	erlang:nif_error({error, not_loaded}).

-spec command(context(), iolist(), non_neg_integer()) -> {'ok', term()} | error().
command(_Context, _CommandArgs, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Sends given commands in a pipeline (timeout in milliseconds can be
%% passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec pipeline(context(), iolist()) -> [{'ok', term()} | error()].
pipeline(Context, CommandList) ->
    pipeline(Context, CommandList, 0).

-spec pipeline(context(), iolist(), non_neg_integer()) -> [{'ok', term()} | error()].
pipeline(Context, CommandList, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    TimeoutPerCommand = case Timeout =:= 0 of
        false -> Timeout div length(CommandList);
        true -> Timeout
    end,
    Pipe = build_pipe(Context, CommandList, TimeoutPerCommand, []),
    clean_pipe(Context, Pipe, TimeoutPerCommand, []).

%% @doc: Wraps given commands in a transaction statement (timeout in
%% milliseconds can be passed as a 3rd parameter). Default timeout: 0
%% (unlimited).
-spec transaction(context(), iolist()) -> {'ok', [term()]} | error().
transaction(Context, CommandList) ->
    transaction(Context, CommandList, 0).

-spec transaction(context(), iolist(), non_neg_integer()) -> {'ok', [term()]} | error().
transaction(Context, CommandList, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    TimeoutPerCommand = case Timeout =:= 0 of
        false -> Timeout div length(CommandList);
        true -> Timeout
    end,
    try
        build_transaction_pipe(Context, CommandList, TimeoutPerCommand, [])
    of
        Pipe -> clean_transaction_pipe(Context, Pipe, TimeoutPerCommand, [])
    catch
        throw:{append_command_error, Error} -> Error
    end.

-spec append_command(context(), iolist()) -> {'ok', integer()} | error().
append_command(_Context, _CommandArgs) ->
    erlang:nif_error({error, not_loaded}).

-spec append_command(context(), iolist(), non_neg_integer()) -> {'ok', integer()} | error().
append_command(_Context, _CommandArgs, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

-spec get_reply(context()) -> {'ok', term()} | error().
get_reply(_Context) ->
	erlang:nif_error({error, not_loaded}).

-spec get_reply(context(), non_neg_integer()) -> {'ok', term()} | error().
get_reply(_Context, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Sets read/write timeout for all subsequent operations (0 means unlimited).
-spec set_timeout(context(), non_neg_integer()) -> ok | error.
set_timeout(_Context, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec build_transaction_pipe(binary(), iolist(), non_neg_integer(), pipe()) -> pipe().
build_transaction_pipe(Context, [Command|Rest], TimeoutPerCommand, []) ->
    ok = unsafe_append_command(Context, [?TRANSACTION_BEGIN], TimeoutPerCommand),
    ok = unsafe_append_command(Context, Command, TimeoutPerCommand),
    build_transaction_pipe(Context, Rest, TimeoutPerCommand, [ok, ok]);
build_transaction_pipe(Context, [], TimeoutPerCommand, Pipe) ->
    ok = unsafe_append_command(Context, [?TRANSACTION_END], TimeoutPerCommand),
    [ok | Pipe];
build_transaction_pipe(Context, [Command|Rest], TimeoutPerCommand, Pipe) ->
    ok = unsafe_append_command(Context, Command, TimeoutPerCommand),
    build_transaction_pipe(Context, Rest, TimeoutPerCommand, [ok | Pipe]).

-spec clean_transaction_pipe(context(), pipe(), non_neg_integer(), list()) -> list().
clean_transaction_pipe(Context, Pipe, TimeoutPerCommand, ReplyAcc) ->
    Replies = clean_pipe(Context, Pipe, TimeoutPerCommand, ReplyAcc),
    lists:last(Replies).

-spec build_pipe(binary(), iolist(), non_neg_integer(), pipe()) -> pipe().
build_pipe(_, [], _, Pipe) ->
    lists:reverse(Pipe);
build_pipe(Context, [Command|Rest], TimeoutPerCommand, Pipe) ->
    Result = case append_command(Context, Command, TimeoutPerCommand) of
        {ok, _} -> ok;
        Error = {error, _} -> Error
    end,
    build_pipe(Context, Rest, TimeoutPerCommand, [Result | Pipe]).

-spec clean_pipe(context(), pipe(), non_neg_integer(), list()) -> list().
clean_pipe(_, [], _, ReplyAcc) ->
    lists:reverse(ReplyAcc);
clean_pipe(Context, [ok | RestOfPipe], TimeoutPerCommand, ReplyAcc) ->
    clean_pipe(Context, RestOfPipe, TimeoutPerCommand, [get_reply(Context, TimeoutPerCommand) | ReplyAcc]);
clean_pipe(Context, [Error = {error, _} | RestOfPipe], TimeoutPerCommand, ReplyAcc) ->
    clean_pipe(Context, RestOfPipe, TimeoutPerCommand, [Error | ReplyAcc]).

%% @throws {append_command_error, Error :: error()}
-spec unsafe_append_command(binary(), iolist(), non_neg_integer()) -> ok.
unsafe_append_command(Context, CommandArgs, Timeout) ->
    case append_command(Context, CommandArgs, Timeout) of
        {ok, _} -> ok;
        Error = {error, _} -> throw({append_command_error, Error})
    end.
