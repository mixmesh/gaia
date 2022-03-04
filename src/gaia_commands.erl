%% -*- erlang-indent-level: 2 -*-
-module(gaia_commands).
-export([all/0, leave_command_mode/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/gaia_serv.hrl").
-include("gaia_commands.hrl").

%%
%% Export: all
%%

all() ->
  [#command{
      name = hi,
      patterns = [["hi", "gaia"], ["command"]],
      onsuccess =
        fun(_Dict) ->
            ?LOG_INFO(#{onsuccess => hi}),
            enter_command_mode()
        end,
      children =
        [
         %%
         %% List [all] active calls?
         %%
         ask(list_active_calls,
             [["list", "active", "calls"],
              ["list", "all", "active", "calls"]],
             fun(_Dict) ->
                 ?LOG_INFO(#{onsuccess => list_active_calls}),
                 PeerNames =
                   gaia_serv:fold(
                     fun(#gaia_peer{name = PeerName,
                                    conversation = {true, _}}, Acc) ->
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
                        gaia_command_serv:format_items(PeerNames)]
                   end,
                 ok = gaia_command_serv:say(Text),
                 [{last_say, Text}|leave_command_mode()]
             end),
         %%
         %% Call X
         %%
         ask_yes_no(
           call,
           [["call", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => call}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to call ">>, PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! ">>, Name,
                           <<" is not known. Please try again!">>],
                   ok = gaia_command_serv:say(Text),
                   [{cd, '..'}, {last_say, Text}]
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:start_peer_conversation(
                      PeerId, #{read => true, write => true}) of
                 ok ->
                   Text = [<<"You are now in a call with ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_started} ->
                   Text = [<<"Hey! You are already in a call with ">>,
                           PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Hang up X
         %%
         ask_yes_no(
           hang_up,
           [["hang", "up", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => hang_up}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to hangup ">>, PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! ">>, Name,
                           <<" is not known. Please try again!">>],
                   ok = gaia_command_serv:say(Text),
                   [{cd, '..'}, {last_say, Text}]
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:stop_peer_conversation(PeerId) of
                 ok ->
                   Text = [<<"You are no longer in a call with ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, no_such_peer} ->
                   ?LOG_ERROR(#{unexpected_return_value => no_such_peer}),
                   leave_command_mode();
                 {error, already_stopped} ->
                   Text = [<<"Hey! You are not in a call with ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% List [all] joined groups
         %%
         ask(list_joined_groups,
             [["list", "joined", "groups"],
              ["list", "all", "joined", "groups"]],
             fun(_Dict) ->
                 ?LOG_INFO(#{onsuccess => list_joined_groups}),
                 GroupNames =
                   gaia_serv:fold(
                     fun(#gaia_group{name = GroupName,
                                     conversation = true}, Acc) ->
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
                        gaia_command_serv:format_items(GroupNames)]
                   end,
                 ok = gaia_command_serv:say(Text),
                 [{last_say, Text}|leave_command_mode()]
             end),
         %%
         %% Join X
         %%
         ask_yes_no(
           join,
           [["join", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => join}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_group{name = GroupName} = Group] ->
                   Text = [<<"Do you want to join group ">>, GroupName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{group => Group}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! Group ">>, Name,
                           <<" is not known. Please try again!">>],
                   ok = gaia_command_serv:say(Text),
                   [{cd, '..'}, {last_say, Text}]
               end
           end,
           fun(#{group := #gaia_group{id = GroupId, name = GroupName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:start_group_conversation(GroupId) of
                 ok ->
                   Text = [<<"You are now an active member of group ">>,
                           GroupName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_started} ->
                   Text = [<<"Hey! You are already in active member of ">>,
                           GroupName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Leave X
         %%
         ask_yes_no(
           leave,
           [["leave", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => leave}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_group{name = GroupName} = Group] ->
                   Text = [<<"Do you want to leave group ">>, GroupName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{group => Group}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! Group ">>, Name,
                           <<" is not known. Please try again!">>],
                   ok = gaia_command_serv:say(Text),
                   [{cd, '..'}, {last_say, Text}]
               end
           end,
           fun(#{group := #gaia_group{id = GroupId, name = GroupName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:stop_group_conversation(GroupId) of
                 ok ->
                   Text = [<<"You are no longer an active member of group ">>,
                           GroupName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, no_such_group} ->
                   ?LOG_ERROR(#{unexpected_return_value => no_such_group}),
                   leave_command_mode();
                 {error, already_stopped} ->
                   Text = [<<"Hey! You are not an active member of group ">>,
                           GroupName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Am I busy?
         %%
         ask(am_i_busy,
             [["am", "i", "busy"]],
             fun(_Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_busy}),
                 case gaia_serv:busy() of
                   true ->
                     Text = <<"Yes, you are busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   false ->
                     Text = <<"No, you are not busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()]
                 end
             end),
         %%
         %% I'm busy
         %%
         ask(busy,
             [["i'm", "busy"]],
             fun(_Dict) ->
                 ?LOG_INFO(#{onsuccess => busy}),
                 case gaia_serv:busy() of
                   true ->
                     Text = <<"Hey! You are already busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   false ->
                     ok = gaia_serv:busy(true),
                     Text = <<"You are now busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()]
                 end
             end),
         %%
         %% I'm not busy
         %%
         ask(not_busy,
             [["i'm", "not", "busy"]],
             fun(_Dict) ->
                 ?LOG_INFO(#{onsuccess => busy}),
                 case gaia_serv:busy() of
                   false ->
                     Text = <<"Hey! You are not busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   true ->
                     ok = gaia_serv:busy(false),
                     Text = <<"You are no longer busy">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()]
                 end
             end),
         %%
         %% Am I muted for X?
         %%
         ask(am_i_muted,
             [["am", "i", "muted", "for", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_muted}),
                 Name = maps:get(name, Dict),
                 case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{write := false}}}] ->
                     Text = [<<"Yes, you are muted for ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{write := true}}}] ->
                     Text = [<<"No, you are not muted for ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = false}] ->
                     Text =
                       [<<"Hey! You are neither in a call nor in an active \
group with ">>,
                        PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [] ->
                     case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                       {ok, _} ->
                         MutedForAll =
                           gaia_serv:fold(
                             fun(#gaia_peer{
                                    conversation =
                                      {true, #{write := true}}}, _Acc) ->
                                 false;
                                (_, Acc) ->
                                 Acc
                             end, true),
                         case MutedForAll of
                           true ->
                             Text = <<"Yes, you are muted for all">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()];
                           false ->
                             Text = <<"No, you are not muted for all">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()]
                         end;
                       nomatch ->
                         Text = [<<"Hey! ">>, Name,
                                 <<" is not known. Please try again!">>],
                         ok = gaia_command_serv:say(Text),
                         [{cd, '..'}, {last_say, Text}]
                     end
                 end
             end),
         %%
         %% Mute me for X
         %%
         ask_yes_no(
           mute,
           [["mute", "me", "for", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => mute}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to mute yourself for ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you want to mute yourself for all?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! ">>, Name,
                               <<" is not known. Please try again!">>],
                       ok = gaia_command_serv:say(Text),
                       [{cd, '..'}, {last_say, Text}]
                   end
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:set_peer_conversation_status(
                      PeerId, #{write => false}) of
                 ok ->
                   Text = [<<"You are now muted for ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are already muted for ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end;
              (#{peer := all}) ->
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
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Unmute me for X
         %%
         ask_yes_no(
           unmute,
           [["unmute", "me", "for", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => unmute}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to unmute yourself for ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you want to unmute yourself for all?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! ">>, Name,
                               <<" is not known. Please try again!">>],
                       ok = gaia_command_serv:say(Text),
                       [{cd, '..'}, {last_say, Text}]
                   end
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:set_peer_conversation_status(
                      PeerId, #{write => true}) of
                 ok ->
                   Text = [<<"You are now unmuted for ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are not muted for ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end;
              (#{peer := all}) ->
               MutedPeerIds =
                 gaia_serv:fold(
                   fun(#gaia_peer{
                          id = PeerId,
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
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Am I deaf to X?
         %%
         ask(am_i_deaf,
             [["am", "I", "deaf", "to", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_deaf}),
                 Name = maps:get(name, Dict),
                 case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{read := false}}}] ->
                     Text = [<<"Yes, you are deaf to ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{read := true}}}] ->
                     Text = [<<"No, you are not deaf to ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = false}] ->
                     Text =
                       [<<"Hey! You are neither in a call nor in an active \
group with ">>,
                        PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [] ->
                     case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                       {ok, _} ->
                         DeafToAll =
                           gaia_serv:fold(
                             fun(#gaia_peer{
                                    conversation =
                                      {true, #{read := true}}}, _Acc) ->
                                 false;
                                (_, Acc) ->
                                 Acc
                             end, true),
                         case DeafToAll of
                           true ->
                             Text = <<"Yes, you are deaf to all">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()];
                           false ->
                             Text = <<"No, you are not deaf to all">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()]
                         end;
                       nomatch ->
                         Text = [<<"Hey! ">>, Name,
                                 <<" is not known. Please try again!">>],
                         ok = gaia_command_serv:say(Text),
                         [{cd, '..'}, {last_say, Text}]
                     end
                 end
             end),
         %%
         %% Deafen me to X
         %%
         ask_yes_no(
           deafen,
           [["deafen", "me", "to", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => deafen}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to be deaf to ">>, PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you want to be deaf to all?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! ">>, Name,
                               <<" is not known. Please try again!">>],
                       ok = gaia_command_serv:say(Text),
                       [{cd, '..'}, {last_say, Text}]
                   end
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:set_peer_conversation_status(
                      PeerId, #{read => false}) of
                 ok ->
                   Text = [<<"You are now deaf to ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are already deaf to ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end;
              (#{peer := all}) ->
               NonMutedPeerIds =
                 gaia_serv:fold(
                   fun(#gaia_peer{
                          id = PeerId,
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
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Undeafen me to X
         %%
         ask_yes_no(
           undeafen,
           [["undeafen", "me", "to", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => undeafen}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you no longer want to be deaf to ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you no longer want to be deaf to all?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! ">>, Name,
                               <<" is not known. Please try again!">>],
                       ok = gaia_command_serv:say(Text),
                       [{cd, '..'}, {last_say, Text}]
                   end
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId,
                                    name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:set_peer_conversation_status(
                      PeerId, #{read => true}) of
                 ok ->
                   Text = [<<"You are no longer deaf to ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are not deaf to ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()]
               end;
              (#{peer := all}) ->
               MutedPeerIds =
                 gaia_serv:fold(
                   fun(#gaia_peer{id = PeerId,
                                  conversation = {true, #{read := false}}},
                       Acc) ->
                       [PeerId|Acc];
                      (_, Acc) ->
                       Acc
                   end, []),
               lists:foreach(
                 fun(PeerId) ->
                     _ = gaia_serv:set_peer_conversation_status(
                           PeerId, #{read => true})
                 end, MutedPeerIds),
               Text = [<<"You are no longer deaf to all">>],
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Am I ignoring X?
         %%
         ask(am_i_ignoring,
             [["am", "i", "ignoring", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_ignoring}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"Yes, ">>, Name, <<" is ignored">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"No, ">>, Name, <<" is not ignored">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Ignore X
         %%
         ask(ignore,
             [["ignore", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => ignore}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"Hey! You already ignore ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"ignore">>}]]}]}]),
                         Text = [<<"You now ignore ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not ignore X
         %%
         ask(ignore,
             [["do", "not", "ignore", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_ignore}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
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
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"You do not ignore ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Has X direct access?
         %%
         ask(has_direct_access,
             [["has", name, "direct", "access"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => hasd_direct_access}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text = [<<"Yes, ">>, Name, <<" has direct access">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"No, ">>, Name, <<" has not direct access">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Give X direct access
         %%
         ask(give_direct_access,
             [["give", "direct", "access", "to", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => give_direct_access}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text = [<<"Hey! You already give direct access to ">>,
                                 Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"direct">>}]]}]}]),
                         Text = [<<"You gave direct access to ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not give X direct access
         %%
         ask(do_not_give_direct_access,
             [["do", "not", "give", "direct", "access", "to", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_give_direct_access}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"call">>}]]}]}]),
                         Text = [<<"You no longer give direct access to ">>,
                                 Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"Hey! You do not give direct access to ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Has X high priority?
         %%
         ask(has_high_priority,
             [["has", name, "high", "priority"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => has_high_priority}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text = [<<"Yes, ">>, Name, <<" has high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"No, ">>, Name,
                            <<" does not have high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Give X high priority
         %%
         ask(give_high_priority,
             [["give", name, "high", "priority"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => give_high_priority}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     [Options] = config:lookup_children([options], ConfigPeer),
                     case lists:member(override_busy, Options) of
                       true ->
                         Text = [<<"Hey! You already give ">>, Name,
                                 <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       false ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {options, [<<"override_busy">>|
                                                 [?a2b(Option) ||
                                                   Option <- Options]]}]]}]}]),
                         Text = [<<"You gave ">>, Name, <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not give X high priority
         %%
         ask(do_not_give_high_priority,
             [["do", "not", "give", name, "high", "priority"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_git_high_priority}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
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
                                       lists:delete(override_busy,
                                                    Options)}]]}]}]),
                         Text = [<<"You no longer give ">>, Name,
                                 <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       true ->
                         Text =
                           [<<"Hey! You do not give ">>, Name,
                            <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end)]}].

%%
%% Export: leave_command_mode
%%

leave_command_mode(CommandState) ->
  ok = gaia_command_serv:beep(leave_command_mode),
  ok = gaia_command_serv:serve_all(),
  CommandState#{path => [], dict => #{}}.

%%
%% Command utilities
%%

ask(Name, AskPatterns, Onsuccess) ->
  #command{
     name = Name,
     patterns = AskPatterns,
     onsuccess = Onsuccess}.

ask_yes_no(Name, AskPatterns, AskOnsuccess, YesOnsuccess, NoOnsuccess) ->
  #command{
     name = Name,
     patterns = AskPatterns,
     onsuccess = AskOnsuccess,
     children =
       [#command{
           name = yes,
           patterns = [["yes"], ["yeah"]],
           onsuccess = YesOnsuccess},
        #command{
           name = no,
           patterns = [["no"], ["nah"]],
           onsuccess = NoOnsuccess}]}.

enter_command_mode() ->
  ?LOG_DEBUG(#{enter_command_mode => now}),
  ok = gaia_command_serv:beep(enter_command_mode),
  ok = gaia_command_serv:serve_only_me(),
  [].
%[{set_timeout, 4000, fun leave_command_mode/1}].

leave_command_mode() ->
  ok = gaia_command_serv:beep(leave_command_mode),
  ok = gaia_command_serv:serve_all(),
  [{cd, []}, {dict, #{}}].
