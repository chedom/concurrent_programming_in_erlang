%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0]).
%% These are the start functions used to create and
%% initialize the server.
start() ->
    Pid = spawn(fun() -> init() end),
    register(frequency, Pid),
    Pid.

init() ->
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
          {NewFrequencies, Reply} = deallocate(Frequencies, {Freq, Pid}),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated} = Init, Pid) ->
    case keyexist(Pid, 1, Allocated) of
        true ->
            {Init, {error, already_exist}};
        false ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
    end.

deallocate({Free, Allocated}, {Freq, Pid} = Pair) ->
    case lists:any(fun(El) -> El =:= Pair end, Allocated) of
        true ->
            NewAllocated=lists:keydelete(Pid, 1, Allocated),
            {{[Freq|Free],  NewAllocated}, ok};
        false ->
            {{Free, Allocated}, {error, incorrect_params}}
    end.

nth([], _N) ->
    out_of_range;
nth([X | _Xs], 0) ->
    X;
nth([_X | Xs], N) ->
    nth(Xs, N-1).

keyexist(_K, _N, []) ->
    false;
keyexist(Key, Num, [X | Xs]) ->
    case nth(erlang:tuple_to_list(X), Num) of
        Key -> 
            true;
        _ -> 
            keyexist(Key, Num, Xs)
    end.

