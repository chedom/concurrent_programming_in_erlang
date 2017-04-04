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




