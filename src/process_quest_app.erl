%%%-------------------------------------------------------------------
%% @doc process_quest public API
%% @end
%%%-------------------------------------------------------------------
-module(process_quest_app).
%%%-------------------------------------------------------------------
-behaviour(application).
%%%-------------------------------------------------------------------
-export([start/2]).
-export([stop/1]).

-export([start_player/2]).
-export([stop_player/1]).
-export([subscribe/3]).
%%%-------------------------------------------------------------------
%%%--------------------- Application Callbacks --------------------%%%
%%%-------------------------------------------------------------------
-spec start(normal, list()) ->{ok, pid()}.
start(_StartType, _StartArgs) ->
    process_quest_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%%------------------------ User Interface ------------------------%%%
%%%-------------------------------------------------------------------
start_player(Name, Info) ->
    process_quest_supersup:start_player(Name, Info).

stop_player(Name) ->
    process_quest_supersup:stop_player(Name).

subscribe(Name, Handler, Args) ->
    process_quest_event:add_handler(Name, Handler, Args).
