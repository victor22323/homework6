-module(lesson6_task01).
-behaviour(application).


-export([start/2, stop/1, create/1, insert/3, insert/4, lookup/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start_link/0]).


start(_Type, _Args) ->
    homework6_sup:start_link().

stop(_State) ->
    ok.


-module(homework6_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    ChildSpec = #{id => homework6_server,
                  start => {homework6_server, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker},
    {ok, {one_for_one, [ChildSpec]}}.


-module(homework6_server).
-behaviour(gen_server).

-export([start_link/0, create/1, insert/3, insert/4, lookup/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create(TableName) ->
    gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value, infinity}).

insert(TableName, Key, Value, TimeToLive) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value, TimeToLive}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).


init(_) ->

    {ok, []}.

handle_call({create, TableName}, _From, State) ->
    ets:new(TableName, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    {reply, ok, [TableName | State]};

handle_call({insert, TableName, Key, Value, TimeToLive}, _From, State) ->
    ExpirationTime = case TimeToLive of
        infinity -> infinity;
        _ -> calendar:universal_time_to_local_time(calendar:now_to_universal_time(erlang:now())) + TimeToLive
    end,
    ets:insert(TableName, {Key, Value, ExpirationTime}),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    case ets:lookup(TableName, Key) of
        [{Key, Value, ExpirationTime}] ->
            Result = case ExpirationTime of
                infinity -> Value;
                _ ->
                    CurrentTime = calendar:universal_time_to_local_time(calendar:now_to_universal_time(erlang:now())),
                    if
                        CurrentTime =< ExpirationTime -> Value;
                        true -> undefined
                    end
            end,
            {reply, Result, State};
        [] -> {reply, undefined, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    lists:foreach(fun(TableName) -> delete_obsolete(TableName) end, State),
    erlang:send_after(60000, self(), timeout),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


delete_obsolete(TableName) ->
    CurrentTime = calendar:universal_time_to_local_time(calendar:now_to_universal_time(erlang:now())),
    Fun = fun({Key, _Value, ExpirationTime}) ->
        case ExpirationTime of
            infinity -> false;
            _ -> CurrentTime > ExpirationTime
        end
    end,
    ets:select_delete(TableName, [{{'$1', '$2', '$3'}, [{const, {Fun, []}}], [true]}]),
    ok.
