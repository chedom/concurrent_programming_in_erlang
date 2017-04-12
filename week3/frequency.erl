%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init_frequency_server/1]).
-export([router_init/0]).

%% These are the start functions used to create and
%% initialize the server.

%%start() ->
%%    register(frequency,
%%	     spawn(frequency, init, [])).

start() -> 
    register(router, 
            spawn(?MODULE,router_init, [])).
%%
%%round robin server START
%%
router_init() ->
    process_flag(trap_exit, true),
    S1 = spawn_link(?MODULE, init_frequency_server, [1]),
    S2 = spawn_link(?MODULE, init_frequency_server, [2]),
    router_loop([S1, S2], 1).

router_loop(Servers, ServerNum) ->
    receive
        {request, Pid, allocate} ->
            SPid = lists:nth(ServerNum, Servers),
            SPid ! {request, self(), allocate, Pid},
            router_loop(Servers, get_next_num(Servers, ServerNum));
        {request, Pid, {deallocate, Freq}} ->
            SPid = find_server_by_frequency(Servers, Freq),
            io:format("SPid ~p~n", [SPid]),
            SPid ! {request, self(), {deallocate, Freq}, Pid},
            router_loop(Servers, ServerNum);
        {reply, _From, Pid, Message} ->
            Pid ! {reply, Message},
            router_loop(Servers, ServerNum);
        {request, stop} ->
            ok
    end.

find_server_by_frequency(Servers, Freq) ->
    [{Server, _Num}]  = lists:filter(
                  fun({_X, Num}) -> lists:member(Freq, get_frequencies(Num)) end, 
                  lists:zip(Servers, lists:seq(1, length(Servers)))
                 ),
    Server.
    

get_next_num(List, Curr) ->
    L = length(List),
    Next = (Curr + 1) rem L,
    if
        Next =:= 0 ->
            L;
        true ->
            Next
    end.
%%
%%round robin server END
%%

init_frequency_server(Num) ->
  Frequencies = {get_frequencies(Num), []},
  loop(Frequencies).




% Hard Coded
get_frequencies(1) -> 
    [10,11,12,13,14,15];
get_frequencies(2) -> 
    [20,21,22,23,24,25].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, From, allocate, Pid} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      From ! {reply, self(),Pid, Reply},
      loop(NewFrequencies);
    {request, From, {deallocate, Freq}, Pid} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      From ! {reply, self(), Pid, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
    router ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    router ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    router ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
