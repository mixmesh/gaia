-module(gaia_commands).
-export([all_patterns/0, all/0, leave_command_mode/0]).
%% Internal exports
-export([hi/1,
         list_active_calls/1,
         call/1, call_yes/1, call_no/1,
         hang_up/1, hang_up_yes/1, hang_up_no/1,
         list_joined_groups/1,
         join/1, join_yes/1, join_no/1,
         leave/1, leave_yes/1, leave_no/1,
         am_i_busy/1, busy/1, not_busy/1,
         am_i_muted/1,
         mute/1, mute_yes/1, mute_no/1,
         unmute/1, unmute_yes/1, unmute_no/1,
         am_i_deaf/1,
         deafen/1, deafen_yes/1, deafen_no/1,
         undeafen/1, undeafen_yes/1, undeafen_no/1,
         am_i_ignoring/1, ignore/1, do_not_ignore/1,
         has_direct_access/1, give_direct_access/1, do_not_give_direct_access/1,
         has_high_priority/1, give_high_priority/1, do_not_give_high_priority/1,
         online_contacts/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/gaia_serv.hrl").
-include("gaia_commands.hrl").

%%
%% all_patterns
%%

all_patterns() ->
    all_patterns(all(), #{}).

all_patterns([], PatternDb) ->
    PatternDb;
all_patterns([#command{patterns = Patterns,
                       children = Children}|Rest], PatternDb) ->
    all_patterns(Rest,
                 all_patterns(Children,
                              lists:foldl(fun(Pattern, Acc) ->
                                                  maps:put(Pattern, Pattern, Acc)
                                          end, PatternDb, Patterns))).

-define(ask(Name, Patterns, Onsuccess),
	#command{
	   name = (Name),
	   patterns = (Patterns),
	   onsuccess = (Onsuccess)}).

-define(ask_yes_no(Name, Patterns, Onsuccess, Yes, No),
	#command{
	   name = (Name),
	   patterns = (Patterns),
	   onsuccess = (Onsuccess),
	   children =
	       [#command{
		   name = yes,
		   patterns = [["yes"], ["yes", "please"], ["yeah"]],
		   onsuccess = Yes},
		#command{
		   name = no,
		   patterns = [["no"],
			       ["no","thanks"],
			       ["no","thank","you"],
			       ["nah"]],
		   onsuccess = No}]}).


%%
%% Export: all
%%

all() ->
    [#command{
        name = hi,
        patterns = [["hi", "gaia"], ["command"]],
        onsuccess = fun ?MODULE:hi/1,
        children =
            [
             %%
             %% List [all] active calls?
             %%
             ?ask(list_active_calls,
                 [[["list", "least"], "active", ["calls", "cause"]],
                  [["list", "least"], "all", "active", ["calls", "cause"]]],
                 fun ?MODULE:list_active_calls/1),
             %%
             %% Call [contact] X
             %%
             ?ask_yes_no(call,
                        [["call", name],
                         ["call", "contact", name]],
                        fun ?MODULE:call/1,
                        fun ?MODULE:call_yes/1,
                        fun ?MODULE:call_no/1),
             %%
             %% Hang-up [contact] X
             %%
             ?ask_yes_no(
               hang_up,
               [["hang", "up", name],
                ["hang", "up", "contact", name]],
               fun ?MODULE:hang_up/1,
               fun ?MODULE:hang_up_yes/1,
               fun ?MODULE:hang_up_no/1),
             %%
             %% List [all] joined groups
             %%
             ?ask(list_joined_groups,
                 [["list", "joined", "groups"],
                  ["list", "all", "joined", "groups"]],
                 fun ?MODULE:list_joined_groups/1),
             %%
             %% Join [group] X
             %%
             ?ask_yes_no(
               join,
               [["join", name],
                ["join", "group", name]],
               fun ?MODULE:join/1,
               fun ?MODULE:join_yes/1,
               fun ?MODULE:join_no/1),
             %%
             %% Leave [group] X
             %%
             ?ask_yes_no(
               leave,
               [["leave", name],
                ["leave", "group", name]],
               fun ?MODULE:leave/1,
               fun ?MODULE:leave_yes/1,
               fun ?MODULE:leave_no/1),
             %%
             %% Am I busy?
             %%
             ?ask(am_i_busy,
                 [["am", "i", ["busy", "bc"]]],
                 fun ?MODULE:am_i_busy/1),
             %%
             %% I'm busy
             %%
             ?ask(busy,
                 [["i'm", ["busy", "bc"]],
                  ["i", "am", ["busy", "bc"]]],
                 fun ?MODULE:busy/1),
             %%
             %% I'm not busy
             %%
             ?ask(not_busy,
                 [["i'm", "not", ["busy", "bc"]],
                  ["i", "am", "not", ["busy", "bc"]]],
                 fun ?MODULE:not_busy/1),
             %%
             %% Am I muted for X?
             %%
             ?ask(am_i_muted,
                 [["am", "i", "muted", "for", name],
                  ["am", "i", "muted", "for", "contact", name]],
                 fun ?MODULE:am_i_muted/1),
             %%
             %% Mute me for [contact] X
             %%
             ?ask_yes_no(
               mute,
               [["mute", "me", "for", name],
                ["mute", "me", "for", "contact", name]],
               fun ?MODULE:mute/1,
               fun ?MODULE:mute_yes/1,
               fun ?MODULE:mute_no/1),
             %%
             %% Do not mute me for [contact] X
             %%
             ?ask_yes_no(
               unmute,
               [["unmute", "me", "for", name],
                ["unmute", "me", "for", "contact", name]],
               fun ?MODULE:unmute/1,
               fun ?MODULE:unmute_yes/1,
               fun ?MODULE:unmute_no/1),
             %%
             %% Am I deaf to [contact] X?
             %%
             ?ask(am_i_deaf,
                 [["am", "I", "deaf", "to", name],
                  ["am", "I", "deaf", "to", "contact", name]],
                 fun ?MODULE:am_i_deaf/1),
             %%
             %% Deafen me to [contact] X
             %%
             ?ask_yes_no(
               deafen,
               [["deafen", "me", "to", name],
                ["deafen", "me", "to", "contact", name]],
               fun ?MODULE:deafen/1,
               fun ?MODULE:deafen_yes/1,
               fun ?MODULE:deafen_no/1),
             %%
             %% Undeafen me to [contact] X
             %%
             ?ask_yes_no(
               undeafen,
               [["undeafen", "me", "to", name],
                ["undeafen", "me", "to", "contact", name]],
               fun ?MODULE:undeafen/1,
               fun ?MODULE:undeafen_yes/1,
               fun ?MODULE:undeafen_no/1),
             %%
             %% Am I ignoring [contact] X?
             %%
             ?ask(am_i_ignoring,
                 [["am", "i", "ignoring", name],
                  ["am", "i", "ignoring", "contact", name]],
                 fun ?MODULE:am_i_ignoring/1),
             %%
             %% Ignore [contact] X
             %%
             ?ask(ignore,
                 [["ignore", name],
                  ["ignore", "contact", name]],
                 fun ?MODULE:ignore/1),
             %%
             %% Do not ignore [contact] X
             %%
             ?ask(do_not_ignore,
                 [["do", "not", "ignore", name],
                  ["do", "not", "ignore", "contact", name]],
                 fun ?MODULE:do_not_ignore/1),
             %%
             %% Has [contact] X direct access?
             %%
             ?ask(has_direct_access,
                 [["has", name, "direct", "access"],
                  ["has", "contact", name, "direct", "access"]],
                 fun ?MODULE:has_direct_access/1),
             %%
             %% Give direct access to [contact] X
             %%
             ?ask(give_direct_access,
                 [["give", "direct", "access", "to", name],
                  ["give", "direct", "access", "to", "contact", name]],
                 fun ?MODULE:give_direct_access/1),
             %%
             %% Do not give direct access to [contact] X
             %%
             ?ask(do_not_give_direct_access,
                 [["do", "not", "give", "direct", "access", "to", name],
                  ["do", "not", "give", "direct", "access", "to", "contact",
                   name]],
                 fun ?MODULE:do_not_give_direct_access/1),
             %%
             %% Has [contact] X high priority?
             %%
             ?ask(has_high_priority,
                 [["has", name, "high", "priority"],
                  ["has", "contact", name, "high", "priority"]],
                 fun ?MODULE:has_high_priority/1),
             %%
             %% Give high priority to [contact] X
             %%
             ?ask(give_high_priority,
                 [["give", "high", "priority", "to", name],
                  ["give", "high", "priority", "to", "contact", name]],
                 fun ?MODULE:give_high_priority/1),
             %%
             %% Do not give high priority to [contact] X
             %%
             ?ask(do_not_give_high_priority,
                 [["do", "not", "give", "high", "priority", "to", name],
                  ["do", "not", "give", "high", "priority", "to", "contact",
                   name]],
                 fun ?MODULE:do_not_give_high_priority/1),
             %%
             %% Which [contacts] are online?
             %%
             ?ask(online_contacts,
                 [["which", "are", "online"],
                  ["which", "contacts", "are", "online"]],
                 fun ?MODULE:online_contacts/1)]}].

hi(_Dict) ->
    ?LOG_INFO(#{onsuccess => hi}),
    enter_command_mode().

list_active_calls(_Dict) ->
    ?LOG_INFO(#{onsuccess => list_active_calls}),
    PeerNames =
        gaia_serv:fold(
          fun(#gaia_peer{name = PeerName, conversation = {true, _}}, Acc) ->
                  [PeerName|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    Text =
        case PeerNames of
            [] ->
                <<"You are not in any active calls">>;
            [PeerName] ->
                [<<"You are in an active call with ">>, PeerName];
            _ ->
                [<<"You are in active calls with ">>,
                 gaia_tts_serv:format_items(PeerNames)]
        end,
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

call(Dict) ->
    ?LOG_INFO(#{onsuccess => call}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text = [<<"Do you want to call ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}]
    end.

call_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:start_peer_conversation(
           PeerId, #{read => true, write => true}) of
        ok ->
            Text = [<<"You are now connecting to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, not_online} ->
            Text = [PeerName, <<" is not online">>],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, no_such_peer} ->
            Text = [PeerName, <<" is unknown">>],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_started} ->
            Text = [<<"You are already in a call with ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

call_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

%%
%% Export: leave_command_mode
%%

leave_command_mode() ->
    ?LOG_DEBUG(#{leave_command_mode => now}),
    %%ok = gaia_asr_serv:serve_all().
    play(leave_command_mode).

hang_up(Dict) ->
    ?LOG_INFO(#{onsuccess => hang_up}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text = [<<"Do you want to hang-up ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}]
    end.

hang_up_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:stop_peer_conversation(PeerId) of
        ok ->
            Text = [<<"You are ending the call with ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, no_such_peer} ->
            ?LOG_ERROR(#{unexpected_return_value => no_such_peer}),
            leaving_command_mode();
        {error, already_stopped} ->
            Text = [<<"You are not in a call with ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

hang_up_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

list_joined_groups(_Dict) ->
    ?LOG_INFO(#{onsuccess => list_joined_groups}),
    GroupNames =
        gaia_serv:fold(
          fun(#gaia_group{name = GroupName, conversation = true}, Acc) ->
                  [GroupName|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    Text =
        case GroupNames of
            [] ->
                <<"You have not joined any groups">>;
            [GroupName] ->
                [<<"You have joined group ">>, GroupName];
            _ ->
                [<<"You have joined groups ">>,
                 gaia_tts_serv:format_items(GroupNames)]
        end,
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

join(Dict) ->
    ?LOG_INFO(#{onsuccess => join}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_group{name = GroupName} = Group] ->
            Text = [<<"Do you want to join group ">>, GroupName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{group => Group}}, remove_timeout, {last_say, Text}];
        _ ->
            Text = [<<"Group ">>, Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}]
    end.

join_yes(#{group := #gaia_group{id = GroupId, name = GroupName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:start_group_conversation(GroupId) of
        ok ->
            Text = [<<"You are now an active member of group ">>, GroupName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_started} ->
            Text = [<<"You are already an active member of group ">>, GroupName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

join_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

leave(Dict) ->
    ?LOG_INFO(#{onsuccess => leave}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_group{name = GroupName} = Group] ->
            Text = [<<"Do you want to leave group ">>, GroupName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{group => Group}}, remove_timeout, {last_say, Text}];
        [] ->
            Text = [<<"Group ">>, Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}]
    end.

leave_yes(#{group := #gaia_group{id = GroupId, name = GroupName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:stop_group_conversation(GroupId) of
        ok ->
            Text =
                [<<"You are no longer an active member of group ">>, GroupName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, no_such_group} ->
            ?LOG_ERROR(#{unexpected_return_value => no_such_group}),
            leaving_command_mode();
        {error, already_stopped} ->
            Text = [<<"You are not an active member of group ">>, GroupName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

leave_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

am_i_busy(_Dict) ->
    ?LOG_INFO(#{onsuccess => am_i_busy}),
    case gaia_serv:busy() of
        true ->
            Text = <<"Yes, you are busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        false ->
            Text = <<"No, you are not busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

busy(_Dict) ->
    ?LOG_INFO(#{onsuccess => busy}),
    case gaia_serv:busy() of
        true ->
            Text = <<"You are already busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        false ->
            ok = gaia_serv:busy(true),
            Text = <<"You are now busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

not_busy(_Dict) ->
    ?LOG_INFO(#{onsuccess => busy}),
    case gaia_serv:busy() of
        false ->
            Text = <<"You are not busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        true ->
            ok = gaia_serv:busy(false),
            Text = <<"You are no longer busy">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

am_i_muted(Dict) ->
    ?LOG_INFO(#{onsuccess => am_i_muted}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName,
                    conversation = {true, #{write := false}}}] ->
            Text = [<<"Yes, you are muted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [#gaia_peer{name = PeerName, conversation = {true, #{write := true}}}] ->
            Text = [<<"No, you are not muted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [#gaia_peer{name = PeerName, conversation = false}] ->
            Text = [<<"You are neither in a call nor in an active group with ">>,
                    PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    MutedForAll =
                        gaia_serv:fold(
                          fun(#gaia_peer{
                                 conversation = {true, #{write := true}}},
                              _Acc) ->
                                  false;
                             (_, Acc) ->
                                  Acc
                          end, true),
                    case MutedForAll of
                        true ->
                            Text = <<"Yes, you are muted for all">>,
                            ok = gaia_tts_serv:say(Text),
                            [{last_say, Text}|leaving_command_mode()];
                        false ->
                            Text = <<"No, you are not muted for all">>,
                            ok = gaia_tts_serv:say(Text),
                            [{last_say, Text}|leaving_command_mode()]
                    end;
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

mute(Dict) ->
    ?LOG_INFO(#{onsuccess => mute}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text = [<<"Do you want to mute yourself for ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    Text = [<<"Do you want to mute yourself for all?">>],
                    ok = gaia_tts_serv:say(Text),
                    [{dict, Dict#{peer => all}},
                     remove_timeout,
                     {last_say, Text}];
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

mute_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:set_peer_conversation_status(PeerId, #{write => false}) of
        ok ->
            Text = [<<"You are now muted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, conversation_not_started} ->
            Text = [<<"You do not talk to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_set} ->
            Text = [<<"You are already muted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end;
mute_yes(#{peer := all}) ->
    NonMutedPeerIds =
        gaia_serv:fold(
          fun(#gaia_peer{
                 id = PeerId,
                 conversation = {true, #{write := true}}},
              Acc) ->
                  [PeerId|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    lists:foreach(
      fun(PeerId) ->
              _ = gaia_serv:set_peer_conversation_status(
                    PeerId, #{write => false})
      end, NonMutedPeerIds),
    Text = [<<"You are now muted for all">>],
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

mute_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

unmute(Dict) ->
    ?LOG_INFO(#{onsuccess => unmute}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text =
                [<<"Do you want to unmute yourself for ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    Text = [<<"Do you want to unmute yourself for all?">>],
                    ok = gaia_tts_serv:say(Text),
                    [{dict, Dict#{peer => all}},
                     remove_timeout,
                     {last_say, Text}];
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

unmute_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:set_peer_conversation_status(PeerId, #{write => true}) of
        ok ->
            Text = [<<"You are now unmuted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_set} ->
            Text = [<<"You are not muted for ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end;
unmute_yes(#{peer := all}) ->
    MutedPeerIds =
        gaia_serv:fold(
          fun(#gaia_peer{id = PeerId,
                         conversation = {true, #{write := false}}},
              Acc) ->
                  [PeerId|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    lists:foreach(
      fun(PeerId) ->
              _ = gaia_serv:set_peer_conversation_status(
                    PeerId, #{write => true})
      end, MutedPeerIds),
    Text = [<<"You are now unmuted for all">>],
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

unmute_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

am_i_deaf(Dict) ->
    ?LOG_INFO(#{onsuccess => am_i_deaf}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName,
                    conversation = {true, #{read := false}}}] ->
            Text = [<<"Yes, you are deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [#gaia_peer{name = PeerName,
                    conversation = {true, #{read := true}}}] ->
            Text = [<<"No, you are not deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [#gaia_peer{name = PeerName, conversation = false}] ->
            Text = [<<"You are neither in a call nor in an active group with ">>,
                    PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    DeafToAll =
                        gaia_serv:fold(
                          fun(#gaia_peer{
                                 conversation = {true, #{read := true}}},
                              _Acc) ->
                                  false;
                             (_, Acc) ->
                                  Acc
                          end, true),
                    case DeafToAll of
                        true ->
                            Text = <<"Yes, you are deaf to all">>,
                            ok = gaia_tts_serv:say(Text),
                            [{last_say, Text}|leaving_command_mode()];
                        false ->
                            Text = <<"No, you are not deaf to all">>,
                            ok = gaia_tts_serv:say(Text),
                            [{last_say, Text}|leaving_command_mode()]
                    end;
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

deafen(Dict) ->
    ?LOG_INFO(#{onsuccess => deafen}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text = [<<"Do you want to be deaf to ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    Text = [<<"Do you want to be deaf to all?">>],
                    ok = gaia_tts_serv:say(Text),
                    [{dict, Dict#{peer => all}},
                     remove_timeout,
                     {last_say, Text}];
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

deafen_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:set_peer_conversation_status(PeerId, #{read => false}) of
        ok ->
            Text = [<<"You are now deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_set} ->
            Text = [<<"You are already deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end;
deafen_yes(#{peer := all}) ->
    NonMutedPeerIds =
        gaia_serv:fold(
          fun(#gaia_peer{id = PeerId,
                         conversation = {true, #{read := true}}},
              Acc) ->
                  [PeerId|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    lists:foreach(
      fun(PeerId) ->
              _ = gaia_serv:set_peer_conversation_status(
                    PeerId, #{read => false})
      end, NonMutedPeerIds),
    Text = [<<"You are now deaf to all">>],
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

deafen_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

undeafen(Dict) ->
    ?LOG_INFO(#{onsuccess => undeafen}),
    Name = maps:get(name, Dict),
    case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
        [#gaia_peer{name = PeerName} = Peer] ->
            Text =
                [<<"Do you no longer want to be deaf to ">>, PeerName, <<"?">>],
            ok = gaia_tts_serv:say(Text),
            [{dict, Dict#{peer => Peer}}, remove_timeout, {last_say, Text}];
        [] ->
            case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                {ok, _} ->
                    Text = [<<"Do you no longer want to be deaf to all?">>],
                    ok = gaia_tts_serv:say(Text),
                    [{dict, Dict#{peer => all}},
                     remove_timeout,
                     {last_say, Text}];
                nomatch ->
                    Text = [Name, <<" is not known. Please try again!">>],
                    ok = gaia_tts_serv:say(Text),
                    [{cd, '..'}, {last_say, Text}]
            end
    end.

undeafen_yes(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
    ?LOG_INFO(#{onsuccess => yes}),
    case gaia_serv:set_peer_conversation_status(PeerId, #{read => true}) of
        ok ->
            Text = [<<"You are no longer deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        {error, already_set} ->
            Text = [<<"You are not deaf to ">>, PeerName],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end;
undeafen_yes(#{peer := all}) ->
    MutedPeerIds =
        gaia_serv:fold(
          fun(#gaia_peer{id = PeerId, conversation = {true, #{read := false}}},
              Acc) ->
                  [PeerId|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    lists:foreach(
      fun(PeerId) ->
              _ = gaia_serv:set_peer_conversation_status(PeerId, #{read => true})
      end, MutedPeerIds),
    Text = [<<"You are no longer deaf to all">>],
    ok = gaia_tts_serv:say(Text),
    [{last_say, Text}|leaving_command_mode()].

undeafen_no(_Dict) ->
    ?LOG_INFO(#{onsuccess => no}),
    ok = gaia_tts_serv:say(<<"OK">>),
    leaving_command_mode().

am_i_ignoring(Dict) ->
    ?LOG_INFO(#{onsuccess => am_i_ignoring}),
    Name = maps:get(name, Dict),
    case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [ignore] ->
                    Text = [<<"Yes, ">>, Name, <<" is ignored">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    Text = [<<"No, ">>, Name, <<" is not ignored">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

ignore(Dict) ->
    ?LOG_INFO(#{onsuccess => ignore}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [ignore] ->
                    Text = [<<"You already ignore ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {mode, <<"ignore">>}]]}]}]),
                    Text = [<<"You now ignore ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

do_not_ignore(Dict) ->
    ?LOG_INFO(#{onsuccess => do_not_ignore}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [ignore] ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {mode, <<"call">>}]]}]}]),
                    Text = [<<"You no longer ignore ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    Text = [<<"You do not ignore ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

has_direct_access(Dict) ->
    ?LOG_INFO(#{onsuccess => hasd_direct_access}),
    Name = maps:get(name, Dict),
    case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [direct] ->
                    Text = [<<"Yes, ">>, Name, <<" has direct access">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    Text = [<<"No, ">>, Name, <<" has not direct access">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

give_direct_access(Dict) ->
    ?LOG_INFO(#{onsuccess => give_direct_access}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [direct] ->
                    Text = [<<"You already give direct access to ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {mode, <<"direct">>}]]}]}]),
                    Text = [<<"You gave direct access to ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

do_not_give_direct_access(Dict) ->
    ?LOG_INFO(#{onsuccess => do_not_give_direct_access}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [direct] ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {mode, <<"call">>}]]}]}]),
                    Text = [<<"You no longer give direct access to ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    Text = [<<"You do not give direct access to ">>, Name],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

has_high_priority(Dict) ->
    ?LOG_INFO(#{onsuccess => has_high_priority}),
    Name = maps:get(name, Dict),
    case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            case config:lookup_children([mode], ConfigPeer) of
                [direct] ->
                    Text = [<<"Yes, ">>, Name, <<" has high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                _ ->
                    Text =
                        [<<"No, ">>, Name, <<" does not have high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

give_high_priority(Dict) ->
    ?LOG_INFO(#{onsuccess => give_high_priority}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            [Options] = config:lookup_children([options], ConfigPeer),
            case lists:member(override_busy, Options) of
                true ->
                    Text = [<<"You already give ">>, Name, <<" high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                false ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {options, [<<"override_busy">>|
                                            [?a2b(Option) ||
                                                Option <- Options]]}]]}]}]),
                    Text = [<<"You gave ">>, Name, <<" high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

do_not_give_high_priority(Dict) ->
    ?LOG_INFO(#{onsuccess => do_not_give_high_priority}),
    Name = ?l2b(maps:get(name, Dict)),
    case config:lookup([gaia, peers, {name, Name}]) of
        not_found ->
            Text = [Name, <<" is not known. Please try again!">>],
            ok = gaia_tts_serv:say(Text),
            [{cd, '..'}, {last_say, Text}];
        ConfigPeer ->
            [Options] = config:lookup_children([options], ConfigPeer),
            case lists:member(override_busy, Options) of
                false ->
                    ok = config:edit_config(
                           [{gaia,
                             [{peers,
                               [[{name, Name},
                                 {options,
                                  lists:delete(override_busy, Options)}]]}]}]),
                    Text =
                        [<<"You no longer give ">>, Name, <<" high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()];
                true ->
                    Text = [<<"You do not give ">>, Name,
                            <<" high priority">>],
                    ok = gaia_tts_serv:say(Text),
                    [{last_say, Text}|leaving_command_mode()]
            end
    end.

online_contacts(_Dict) ->
    ?LOG_INFO(#{onsuccess => online_contacts}),
    OnlinePeerNames =
        gaia_serv:fold(
          fun(#gaia_peer{name = PeerName,
                         nodis_address = NodisAddress}, Acc)
                when NodisAddress /= undefined ->
                  [PeerName|Acc];
             (_, Acc) ->
                  Acc
          end, []),
    case OnlinePeerNames of
        [] ->
            Text = <<"No one is online">>,
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        [PeerName] ->
            Text = [PeerName, <<" is online">>],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()];
        _ ->
            Text =
                [gaia_tts_serv:format_items(OnlinePeerNames), <<" are online">>],
            ok = gaia_tts_serv:say(Text),
            [{last_say, Text}|leaving_command_mode()]
    end.

%%
%% Command utilities
%%



enter_command_mode() ->
    ?LOG_DEBUG(#{enter_command_mode => now}),
    %%ok = gaia_asr_serv:serve_only_me(),
    ok = play(enter_command_mode),
    %%[{set_timeout, 4000, fun leave_command_mode/1}].
    [].

leaving_command_mode() ->
    ?LOG_DEBUG(#{leaving_command_mode => now}),
    ok = gaia_pa_serv:playcd(gaia_serv),
    [].

play(enter_command_mode) ->
    ok = alsa_wave:enter(),
    %% FIXME: lower volume and implement a alsa_wave:{start,stop}_loop/1
    %%spawn(fun() -> alsa_wave:mute() end),
    ok;
play(leave_command_mode) ->
    alsa_wave:leave().
