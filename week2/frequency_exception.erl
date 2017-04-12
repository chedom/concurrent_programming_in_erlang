%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_exception).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency_exception, init, [])).

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    io:format("Hello !~n"),
    try 
        receive
            {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq, Pid),
            Pid ! {reply, ok},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
        end
    catch
        throw: Error ->
            io:format("Error, ~p~n", [Error]),
            loop(Frequencies)
    end.

%% Functional interface

allocate() -> 
    io:format("Alocate!~n"),
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    after 3000 ->
              "Some error"
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    after 3000 ->
              "Some error"
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    after 3000 ->
              "Some error"
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    io:format("Allocated: ~p~n", [Allocated]),
    case lists:keyfind(Pid, 2, Allocated) of
        false ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        _ -> 
            throw(pid_already_exist)
    end.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keysearch(Freq,1,Allocated) of
        {value, {Freq, Pid}} -> 
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {[Freq|Free],  NewAllocated};
        _ -> 
            throw(incorrect_freq_or_pid)
    end.

