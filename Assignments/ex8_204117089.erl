-module(ex8_204117089).
%c(ex8_204117089). Pid=ex8_204117089:startChat(node()).
%c(ex8_204117089). ex8_204117089:steadyMon(fun() -> 1/0 end).
-export([startChat/1, call/1,createRemote/2,send/1, steadyLink/1,steadyMon/1,try_F/1]).


startChat(Remote@Host)->
    register(local,LocalPid = spawn(fun() -> local_chat_loop(Remote@Host,0,0) end)),
    rpc:call(Remote@Host,ex8_204117089,createRemote,[node(),whereis(remote)]),
    put(remote@Host, Remote@Host),
    LocalPid.

createRemote(Local@Host,undefined) ->
       register(remote,spawn(fun() -> remote_chat_loop(Local@Host,0,0) end));
createRemote(_,_)->okay.
local_chat_loop(Remote@Host,Sent,Received)->
    receive
        quit ->
        {remote,Remote@Host}!{quit},
        io:format("~p - Successfully closed.~n",[self()]),
        unregister(local),
        exit(normal);

        stats -> io:format("local stats: sent: ~p received: ~p~n",[Sent,Received]),
                 local_chat_loop(Remote@Host,Sent,Received);

        {Message,toLocal} -> Message,
                local_chat_loop(Remote@Host,Sent,Received+1);

        Message ->
%            io:format("local got ~p, now sending to remote~n",[Message]),
            {remote,Remote@Host}!{Message,toRemote},
            local_chat_loop(Remote@Host,Sent+1,Received)

    end.

remote_chat_loop(Local@Host,Sent,Received)->
    receive
        {quit} ->
        io:format("~p - Successfully closed.~n",[self()]),
        unregister(remote),
        exit(normal);

        quit ->   _={local,Local@Host}!quit,
                 remote_chat_loop(Local@Host,Sent,Received);

        stats -> io:format("remote stats: sent: ~p received: ~p~n",[Sent,Received]),
                 remote_chat_loop(Local@Host,Sent,Received);

        {Message,toRemote} -> %io:format("remote got ~p~n",[Message]),
         remote_chat_loop(Local@Host,Sent,Received+1);
        Message ->
%            io:format("remote got ~p, sending to local",[Message]),
            _={local,Local@Host}!{Message,toLocal},
            remote_chat_loop(Local@Host,Sent+1,Received)
    end.

call(Message) -> rpc:call(get(remote@Host), ex8_204117089,send,[Message]).
send(Message) -> _={remote,node()}!Message.


steadyLink(F) -> Pid = spawn_link(F),
                    wait(Pid).
wait(Pid)->
  receive
        _-> wait(Pid)
      after 5000 -> Pid
  end.

steadyMon(F) -> {Pid,_} =spawn_monitor(ex8_204117089,try_F,[F]),
                receive
                    {_,_,_,Pid,normal} -> io:format("Normal termination of process ~p was detected~n",[Pid]);
                    {_,_,_,Pid,Why} -> io:format("An exception in process ~p was detected: ~p~n",[Pid,Why])


                    after 5000 -> Pid
                end.
try_F(F) ->
    try F() of
        _-> normal
    catch
        %%catch the right type of error by pattern maching.
        error:Error->  exit(Error);
        throw:Throw->  exit(Throw);
        exit:Exit->  exit(Exit)
    end.