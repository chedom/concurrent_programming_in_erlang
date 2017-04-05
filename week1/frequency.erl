%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0, allocate/0, deallocate/1, clear/0]).
-define(CLIENT_TIME_OUT, 5000).
-define(SERVER_TIME_OUT, 6000).
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
          timer:sleep(?SERVER_TIME_OUT),
          {NewFrequencies, Reply} = allocate(Frequencies, Pid),
          Pid ! {reply, Reply},
          loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
          timer:sleep(?SERVER_TIME_OUT),
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
    case lists:keyfind(Pid, 2, Allocated) of
        false ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
         _ ->
            {Init, {error, already_exist}}

    end.


deallocate({Free, Allocated}, {Freq, _Pid} = Pair) ->
    case lists:any(fun(El) -> El =:= Pair end, Allocated) of
        true ->
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {{[Freq|Free],  NewAllocated}, ok};
        false ->
            {{Free, Allocated}, {error, incorrect_params}}
    end.


%% functional interface
allocate() ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
        {reply, Reply} ->
            Reply
    after ?CLIENT_TIME_OUT ->
              ok
    end.

deallocate(Freq) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} ->
                Reply
    after ?CLIENT_TIME_OUT ->
              ok
    end.
%%

clear() ->
    receive
        Msg ->
            io:format("Clear message:~p~n", [Msg]),
            clear()
    after 0 ->
        ok
    end.


