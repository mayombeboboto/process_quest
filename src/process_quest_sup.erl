%%%-------------------------------------------------------------------
%% @doc process_quest top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(process_quest_sup).
%%%-------------------------------------------------------------------
-behaviour(supervisor).
%%%-------------------------------------------------------------------
-export([start_link/2]).

-export([init/1]).
%%%-------------------------------------------------------------------
start_link(Name, Info) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Name, Info}).

init({Name, Info}) ->
    SupFlags = #{ strategy => one_for_all,
                  intensity => 2,
                  period => 36000 },
    ChildSpecs = [#{ id => events,
                     start => {process_quest_event, start_link, [Name]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [dynamic] },
                  #{ id => player,
                     start => {process_quest_player, start_link, [Name, Info]},
                     restart => permanent,
                     shutdown => 2000,
                     type => worker,
                     modules => [process_quest_player ]}],
    {ok, {SupFlags, ChildSpecs}}.
