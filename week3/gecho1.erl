-module(gecho1).
-behaviour(gen_server).
-export([count/0, start_link/0, echo/1, reset/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link(
      {local, ?MODULE},
      ?MODULE, [], []).

count() ->
    gen_server:call(?MODULE, count).

echo(X) ->
    gen_server:call(?MODULE, X).

reset(N) ->
    gen_server:cast(?MODULE, {reset, N}).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    {ok, 0}.

handle_call(count, _From, State) ->
    {reply, State, State};
handle_call(Msg, _From, State) ->
    {reply, Msg, State + 1}.

handle_cast({reset, N}, _State) ->
    {noreply, N}.
