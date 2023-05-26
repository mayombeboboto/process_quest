-module(process_quest_player).
% -behaviour(gen_fsm).
-export([start_link/2]).
-export([init/1, market/3, killing/3, terminate/3]).

-record(state, { name,
                 stats,
                 exp=0,
                 lvlexp=1000,
                 lvl=1,
                 equip=[],
                 money=0,
                 loot=[],
                 bought=[],
                 time=0, quest }).


%%% Possible states & events
%%
%      sell  buy
%     /   | |  \
%     \   ^ ^  /
%      [market]<--,
%      |          |
% done buying     |
%      |     bag full   
%      v         /
%  [killing fields]
%   /   V   V   |
%   \   /   |   |
%   kill    lvl up

start_link(Name, Opts) ->
    gen_statem:start_link(?MODULE, {Name, Opts}, []).

init({Name, Opts}) ->
    %% Properly seeding stuff. If not doing this, the random module will
    %% seed it based on a slightly unique time value. However, when starting
    %% many processes at about the same time, the seeds can be very close
    %% and give barely random results. The crypto:strong_rand_bytes/1 function
    %% allows for much better seeding.
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsss, {A,B,C}),
    %% The first event, to start the FSM
    gen_statem:cast(self(), kill),
    case regis:register(Name, self()) of
        {error, _Reason} ->
            {stop, name_taken};
        ok ->
            %% Use proplists with default values to let the user configure
            %% all parts of the FSM's state by using the Opts proplist.
            State = #state{ name=Name,
                            stats=proplists:get_value(stats, Opts, process_quest_stats:initial_roll()),
                            exp=proplists:get_value(exp, Opts, 0),
                            lvlexp=proplists:get_value(lvlexp, Opts, 1000),
                            lvl=proplists:get_value(lvl, Opts, 1),
                            equip=proplists:get_value(equip, Opts, []),
                            money=proplists:get_value(money, Opts, 0),
                            loot=proplists:get_value(loot, Opts, []),
                            bought=proplists:get_value(bought, Opts, []),
                            time=proplists:get_value(time, Opts, 0),
                            quest=proplists:get_value(quest, Opts, process_quest:fetch()) },
            {ok, market, State}
    end.

%% Done selling. Switch to the event where we head to the killing fields
market(cast, sell, #state{ loot=[] }) ->
    gen_statem:cast(self(), buy),
    keep_state_and_data;
%% Selling an Item we have looted to the market, for whatever value it has
market(cast, sell, State=#state{ name=Name, loot=[{Drop,Val}|Loots], money=M, lvl=Lvl, time=Time }) ->
    process_quest_events:sell(Name, {Drop, Val*Lvl}, Time),
    gen_statem:cast(self(), sell),
    {keep_state, State#state{ loot=Loots, money=M+Val*Lvl }};
%% When done selling, buy items with your money
market(cast, buy, State=#state{ equip=Equip, money=Money, bought=Bought }) ->
    %% we have slots of equipment. It's useless to buy the same
    %% kind of item time after time, so we must select one we haven't observed yet
    case next_slot(Equip, Bought) of
        undefined ->
            %% when no slot is found, go to the killing field
            gen_statem:cast(self(), kill),
            {next_state, market, State#state{bought=[]}};
        OldItem = {Slot, {_Name, Modifier, Lvl, _Price}} ->
            %% Replace the item by a slightly better one if possible.
            case process_quest_market:Slot(Modifier+Lvl, Money) of
                undefined ->
                    market(cast, buy, State#state{bought=[Slot|Bought]});
                NewItem = {_, _, _, Price} ->
                    process_quest_events:buy(State#state.name, Slot, NewItem, State#state.time),
                    gen_statem:cast(self(), buy),
                    NewEquip = [{Slot, NewItem} | Equip -- [OldItem]],
                    {next_state, market, State#state{ equip=NewEquip,
                                                      money=Money-Price,
                                                      bought=[Slot|Bought]}}
            end
    end;
%% Heading to the killing field. State only useful as a state transition.
market(cast, kill, State=#state{ name=Name, time=Time }) ->
    process_quest_events:location(Name, killing, Time),
    gen_statem:cast(self(), kill),
    {next_state, killing, State}.

%% Killing an enemy on the killing field. Taking its drop and keeping it
%% in our loot.
killing(cast, kill, State=#state{ loot=Loot, stats=Stats, exp=Exp, lvlexp=LvlExp,
                                  quest=Quest }) ->
    MaxSize = proplists:get_value(strength, Stats)*2,
    {EnemyName, Props} = process_quest_enemy:fetch(),
    process_quest_events:killed(State#state.name, {EnemyName, Props}, State#state.time),
    Drop = {_N, _V} = proplists:get_value(drop, Props),
    KillExp = proplists:get_value(experience, Props),
    NewLoot = [Drop|Loot],
    {QuestExp, NewQuest} = case check_quest(Quest) of
        UpdatedQuest = {0, _} -> UpdatedQuest;
        QuestBeaten = {_, NewQuest0} ->
            process_quest_events:quest(State#state.name, Quest, NewQuest0, State#state.time),
            QuestBeaten
    end,
    if length(NewLoot) =:= MaxSize ->
        gen_statem:cast(self(), market);
       Exp+KillExp+QuestExp >= LvlExp ->
        gen_statem:cast(self(), lvl_up);
       true ->
        gen_statem:cast(self(), kill)
    end,
    {keep_state, State#state{ loot=NewLoot, exp=Exp+KillExp+QuestExp, quest=NewQuest }};
%% If we just leveled up, the stats get updated before we keep killing.
killing(cast, lvl_up, State=#state{ stats=Stats, lvl=Lvl, lvlexp=LvlExp }) ->
    NewStats = [{charisma, proplists:get_value(charisma, Stats)+process_quest_stats:roll()},
     {constitution, proplists:get_value(constitution, Stats)+process_quest_stats:roll()},
     {dexterity, proplists:get_value(dexterity, Stats)+process_quest_stats:roll()},
     {intelligence, proplists:get_value(intelligence, Stats)+process_quest_stats:roll()},
     {strength, proplists:get_value(strength, Stats)+process_quest_stats:roll()},
     {wisdom, proplists:get_value(wisdom, Stats)+process_quest_stats:roll()}],
    gen_statem:cast(self(), kill),
    process_quest_events:lvl_up(State#state.name, NewStats, Lvl+1, LvlExp*2, State#state.time),
    {keep_state, State#state{ stats=NewStats, lvl=Lvl+1, lvlexp=LvlExp*2 }};
%% Heading to the market state transition
killing(cast, market, State=#state{ name=Name, time=Time }) ->
    process_quest_events:location(Name, market, Time),
    gen_statem:cast(self(), sell),
    {next_state, market, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% Picks a slot based on what has been seen so far, combined with the
%% current weakest item.
next_slot(Equip, Bought) ->
    L = expand(Equip),
    case lists:sort([{Mod+Lvl, Entry} || Entry = {Slot, {_, Mod, Lvl, _}} <- L,
                                         not lists:member(Slot, Bought)]) of
        [] -> undefined;
        [{_, Entry}|_] -> Entry
    end.

expand(L) ->
    [expand_field(armor, L),
     expand_field(helmet, L),
     expand_field(shield, L),
     expand_field(weapon, L)].

expand_field(F, L) ->
    {F, proplists:get_value(F, L, {undefined,0,0,0})}.

%% Checks quests, if they are ready for the next level or not
check_quest({Name, Props}) ->
    case proplists:get_value(kills, Props) of
        1 ->
            case process_quest:fetch() of
                %% Same name, we want new stuff!
                {Name, _} -> check_quest({Name, Props});
                NewQuest ->
                    Exp = proplists:get_value(experience, Props),
                    {Exp, NewQuest}
            end;
        Q ->
            {0, {Name, [{kills,Q-1} | Props--[{kills,Q}]]}}
    end.
