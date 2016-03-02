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
         get_reply/1,
         set_timeout/2]).

-include("hierdis.hrl").

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
-spec connect(Ip::string(), Port::integer()) -> {'ok', binary()} | error().
connect(_Ip, _Port) ->
    erlang:nif_error({error, not_loaded}).
-spec connect(Ip::string(), Port::integer(), Timeout::integer()) -> {'ok', binary()} | error().
connect(_Ip, _Port, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Connects to Redis via unix domain socket (timeout in milliseconds can
%% be passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec connect_unix(SocketPath::string()) -> {'ok', binary()} | error().
connect_unix(_SocketPath) ->
    erlang:nif_error({error, not_loaded}).
-spec connect_unix(SocketPath::string(), Timeout::integer()) -> {'ok', binary()} | error().
connect_unix(_SocketPath, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Executes given command ("GET", "SET", etc.; timeout in milliseconds
%% can be passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec command(Context::binary(), CommandArgs::iolist()) -> {'ok', term()} | error().
command(_Context, _CommandArgs) ->
	erlang:nif_error({error, not_loaded}).
-spec command(Context::binary(), CommandArgs::iolist(), Timeout::integer()) -> {'ok', term()} | error().
command(_Context, _CommandArgs, _Timeout) ->
    erlang:nif_error({error, not_loaded}).

%% @doc: Sends given commands in a pipeline (timeout in milliseconds can be
%% passed as a 3rd parameter). Default timeout: 0 (unlimited).
-spec pipeline(Context::binary(), CommandList::iolist()) -> [{'ok', term()} | error()].
pipeline(Context, CommandList) ->
    PipelineLength = pipe_builder(pipeline, Context, CommandList, 0),
    pipe_cleaner(pipeline, Context, [], PipelineLength).
-spec pipeline(Context::binary(), CommandList::iolist(), Timeout::integer()) -> [{'ok', term()} | error()].
pipeline(Context, CommandList, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    set_timeout(Context, Timeout div length(CommandList)),
    try
        pipeline(Context, CommandList)
    after
        set_timeout(Context, 0)
    end.

%% @doc: Wraps given commands in a transaction statement (timeout in
%% milliseconds can be passed as a 3rd parameter). Default timeout: 0
%% (unlimited).
-spec transaction(Context::binary(), CommandArgs::iolist()) -> [{'ok', term()} | error()].
transaction(Context, CommandList) ->
    TransactionLength = pipe_builder(transaction, Context, CommandList, 0),
    pipe_cleaner(transaction, Context, [], TransactionLength).
-spec transaction(Context::binary(), CommandArgs::iolist(), Timeout::integer()) -> [{'ok', term()} | error()].
transaction(Context, CommandList, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    set_timeout(Context, Timeout div length(CommandList)),
    try
        transaction(Context, CommandList)
    after
        set_timeout(Context, 0)
    end.

%% @private
pipe_builder(pipeline, _Context, [], Counter) ->
    Counter;
pipe_builder(transaction, Context, [Command|List], 0) ->
    append_command(Context, [?TRANSACTION_BEGIN]),
    append_command(Context, Command),
    pipe_builder(transaction, Context, List, 2);
pipe_builder(transaction, Context, [], Counter) ->
    append_command(Context, [?TRANSACTION_END]),
    Counter+1;
pipe_builder(Scheme, Context, [Command|List], Counter) ->
    append_command(Context, Command),
    pipe_builder(Scheme, Context, List, Counter+1).

%% @private
pipe_cleaner(pipeline, _Context, Acc, 0) ->
    lists:reverse(Acc);
pipe_cleaner(transaction, Context, _Acc, 1) ->
    get_reply(Context);
pipe_cleaner(Scheme, Context, Acc, Counter) ->
    pipe_cleaner(Scheme, Context, [get_reply(Context)|Acc], Counter-1).


-spec append_command(Context::binary(), CommandArgs::iolist()) -> {'ok', integer()} | error().
append_command(_Context, _CommandArgs) ->
    erlang:nif_error({error, not_loaded}).

-spec get_reply(Context::binary()) -> {'ok', term()} | error().
get_reply(_Context) ->
	erlang:nif_error({error, not_loaded}).

%% @doc: Sets read/write timeout for all subsequent operations (0 means unlimited).
-spec set_timeout(Context::binary(), Timeout::integer()) -> ok | error.
set_timeout(_Context, _Timeout) ->
    erlang:nif_error({error, not_loaded}).
