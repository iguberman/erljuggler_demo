# erljuggler_demo
A demo of a uniquely resilient simulated "server" made possible by unique features of Erlang VM, the BEAM

cd src
erl

DEMO 1 JUGGLER:
>c(juggler).
>juggler:accept(10000). %% 10,000 = NumRequests we'll handle
> %% (of them every Nth request will result in inf loop) can be modified by changing define of BUG_MOD
**** 9620 [INF]***** %%% INDICATES INFINITE LOOP WAS SPAWNED by request # 9620
...9392 [3090]......9393 [2083]    %%% INDICATES a "request_handler" process was finished in [millis], in this case request #9392 finished in 3030 millis and request # 9393 finished in 2083 millis. 
. 
.
.
>9000 finished, 1000 running forever %%% 9000 requests successfully completed, while 1000 are inf loops that will be running until you kill the shell 
ok
>%%% Feel free to try again, but now we'll be running 2000 inf loops, the rest of the requests will still be completing


DEMO 2 KILLER JUGGLER:   %% Same as JUGGLER, but much more useful!
>c(killer_juggler).
>killer_juggler:accept(10000). 
