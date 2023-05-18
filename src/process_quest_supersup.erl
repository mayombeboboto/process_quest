%%%-------------------------------------------------------------------
%% @doc process_quest_market API
%% @end
%%%-------------------------------------------------------------------
-module(process_quest_supersup).
%%%-------------------------------------------------------------------
-behaviour(supervisor).
%%%-------------------------------------------------------------------
-export([start_link/0]).
-export([start_player/2]).
-export([stop_player/1]).

-export([init/1]).
%%%-------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Starts an individual player
start_player(Name, Info) ->
    supervisor:start_child(?MODULE, [Name, Info]).

%% Stops a player.
stop_player(Name) ->
    supervisor:terminate_child(?MODULE, regis:whereis(Name)).

%%%-------------------------------------------------------------------
%%%----------------------- Callback Function ----------------------%%%
%%%-------------------------------------------------------------------
init([]) ->
    SupFlags = #{ strategy => simple_one_for_one,
                  intensity => 1,
                  period => 60000 },
    ChildSpecs = [#{ id => sup,
                     start => {process_quest_sup, start_link, []},
                     restart => permanent,
                     shutdown => infinity,
                     type => worker,
                     modules => [process_quest_sup] }],
    {ok, {SupFlags, ChildSpecs}}.
