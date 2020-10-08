Travian Collector
=====

This is an application for scraping the map.sql info published by every Travian server at the morning.

It aims to be fast and robust as possible, so it use the benefits which the Erlang Virtual machine is made for, concurrency and fault tolerance.

Objectives in mind
---------------
1º Pick more than map.sql info, e.g, server start day,...

2º Start it one time, and let it works forever(Probably at the morning)

3º Collect a lot of data and publish it on Kaggle

Build
-----

    $ rebar3 compile

Explanation
-----------
![Supervision Tree](figures/tc_supervision_tree.png)
![Dispatcher Flow Diagram](figures/dispatcher_state_diagram.png)
