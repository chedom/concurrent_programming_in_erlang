-module(palin_server_5).
-export([server/0]).

-import(palin, [palindrome/1]).

server() -> spawn(fun() -> loop() end).

getResultMessage(Text) ->
    case palindrome(Text) of
        true ->
            " is a palindrome";
        false ->
            " is not a palindrome"
    end.

loop() ->
    receive
        {Pid, check, Text} = Msg ->
            io:format("Message ~p~n", [Msg]), 
            Pid ! {self(), result, Text ++ getResultMessage(Text)},
            loop();
        _ ->
            exit("Incorrect message")
    end.


