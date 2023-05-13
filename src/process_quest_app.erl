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
%%%-------------------------------------------------------------------
-spec start(normal, list()) ->{ok, pid()}.
start(_StartType, _StartArgs) ->
    process_quest_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.