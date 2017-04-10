%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
%%-compile(export_all).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([start_client/0, client/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {'EXIT', Pid, _Why} ->
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value, {Freq, Pid}} = lists:keysearch(Freq, 1 , Allocated),  
  unlink(Pid),
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid, 2, Allocated) of
        {value, {Freq, Pid}} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {[Freq | Free], NewAllocated};
        false ->
            {Free, Allocated}
    end.

client_allocate() ->
    frequency ! {request, self(), allocate},
    client_receive().
client_deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    client_receive().

client_receive() ->
    receive
        {stop} ->
            io:format("Stop client~n"),
            ok;
        {'EXIT', ?MODULE, Why} ->
            io:format("Server die ~p~n", [Why]),
            timer:sleep(infinity);
        {reply, Reply} -> 
            io:format("Reply: ~p~n", [Reply]),
            Reply
    end.

start_client() ->
    Pid1 = spawn(frequency, client, []),
    Pid2 = spawn(frequency, client, []),
    {Pid1, Pid2}.

client() ->
   process_flag(trap_exit, true),
   start_client_action().

start_client_action() ->
    case whereis(?MODULE) of
        undefined ->
            io:format("Server process doesnt exist"),
            timer:sleep(infinity);
        _ ->
        io:format("allocate/deallocate client~n"),
        {ok, Freq} = client_allocate(),
        ok = client_deallocate(Freq),
        timer:sleep(3000),
        start_client_action()
    end.

  
