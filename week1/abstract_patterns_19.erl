-module(abstract_patterns_19).
-compile(export_all).

rpc(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    receive
        {Tag, Response} ->
            Response
    end.

promise(Pid, Request) ->
  Tag = erlang:make_ref(),
  Pid ! {self(), Tag, Request},
  Tag.

yield(Tag) ->
  receive
    {Tag, Response} ->
      Response
  end.

pmap(L) ->
  S = self(),
  Pids = [do(S, F) || F <- L ],
  [receive {Pid, Val} -> Val end || Pid <- Pids].

do(Parent, F) ->
  spawn(fun() ->
    Parent ! {self(), F()} end
  ).

