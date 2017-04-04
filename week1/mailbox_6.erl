-module(mailbox_6).
-compile(export_all).

start_receiver(N) -> 
    spawn(fun() -> receiver(N) end).

receiver(N) ->
    timer:sleep(N),
    receive_loop().

receive_loop() ->
    receive
        stop ->
            io:format("exit~n");
        Message ->
            io:format("message:~w~n", [Message]),
            receive_loop()
    end.


orderProcessMess() ->
    spawn(fun() -> order_loop(first) end).

getNextTag(Tag) ->
    if 
        Tag =:= first ->
            second;
        true ->
            first
    end.

order_loop(Tag) ->
    receive
        {Tag, Mes} ->
            io:format("message: ~p~n", [Mes]),
            order_loop(getNextTag(Tag))
    end.

