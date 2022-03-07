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
             [[["list", "least"], "active", ["calls", "cause"]],
              [["list", "least"], "all", "active", ["calls", "cause"]]],
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
         %% Call [contact] X
         %%
         ask_yes_no(
           call,
           [["call", name],
            ["call", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => call}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->

                   io:format("PEER: ~p\n", [Peer]),


                   Text = [<<"Do you want to call contact ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! Contact ">>, Name,
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
                   Text = [<<"You are now in a call with contact ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_started} ->
                   Text = [<<"Hey! You are already in a call with contact ">>,
                           PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, not_online} ->
                   Text = [<<"Hey! Contact ">>, PeerName, <<" is not online">>],
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
         %% Hang-up [contact] X
         %%
         ask_yes_no(
           hang_up,
           [["hang", "up", name],
            ["hang", "up", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => hang_up}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to hang-up contact ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   Text = [<<"Hey! Contact ">>, Name,
                           <<" is not known. Please try again!">>],
                   ok = gaia_command_serv:say(Text),
                   [{cd, '..'}, {last_say, Text}]
               end
           end,
           fun(#{peer := #gaia_peer{id = PeerId, name = PeerName}}) ->
               ?LOG_INFO(#{onsuccess => yes}),
               case gaia_serv:stop_peer_conversation(PeerId) of
                 ok ->
                   Text = [<<"You are no longer in a call with contact ">>,
                           PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, no_such_peer} ->
                   ?LOG_ERROR(#{unexpected_return_value => no_such_peer}),
                   leave_command_mode();
                 {error, already_stopped} ->
                   Text = [<<"Hey! You are not in a call with contact ">>,
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
         %% Join [group] X
         %%
         ask_yes_no(
           join,
           [["join", name],
            ["join", "group", name]],
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
                   Text =
                     [<<"Hey! You are already an active member of group ">>,
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
         %% Leave [group] X
         %%
         ask_yes_no(
           leave,
           [["leave", name],
            ["leave", "group", name]],
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
             [["am", "i", ["busy", "bc"]]],
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
             [["i'm", ["busy", "bc"]],
              ["i", "am", ["busy", "bc"]]],
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
             [["i'm", "not", ["busy", "bc"]],
              ["i", "am", "not", ["busy", "bc"]]],
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
             [["am", "i", "muted", "for", name],
              ["am", "i", "muted", "for", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_muted}),
                 Name = maps:get(name, Dict),
                 case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{write := false}}}] ->
                     Text = [<<"Yes, you are muted for contact ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{write := true}}}] ->
                     Text = [<<"No, you are not muted for contact ">>,
                             PeerName],
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
                             Text = <<"Yes, you are muted for all contacts">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()];
                           false ->
                             Text =
                               <<"No, you are not muted for all contacts">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()]
                         end;
                       nomatch ->
                         Text = [<<"Hey! Contact ">>, Name,
                                 <<" is not known. Please try again!">>],
                         ok = gaia_command_serv:say(Text),
                         [{cd, '..'}, {last_say, Text}]
                     end
                 end
             end),
         %%
         %% Mute me for [contact] X
         %%
         ask_yes_no(
           mute,
           [["mute", "me", "for", name],
            ["mute", "me", "for", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => mute}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to mute yourself for contact ">>,
                           PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text =
                         [<<"Do you want to mute yourself for all contacts?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! Contact ">>, Name,
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
                   Text = [<<"You are now muted for contact ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are already muted for contact ">>,
                           PeerName],
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
               Text = [<<"You are now muted for all contacts">>],
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Do not mute me for [contact] X
         %%
         ask_yes_no(
           unmute,
           [["unmute", "me", "for", name],
            ["unmute", "me", "for", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => unmute}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to unmute yourself for contact ">>,
                           PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text =
                         [<<"Do you want to unmute yourself for all contacts?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! Contact ">>, Name,
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
                   Text = [<<"You are now unmuted for contact ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are not muted for contact ">>, PeerName],
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
               Text = [<<"You are now unmuted for all contacts">>],
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Am I deaf to [contact] X?
         %%
         ask(am_i_deaf,
             [["am", "I", "deaf", "to", name],
              ["am", "I", "deaf", "to", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_deaf}),
                 Name = maps:get(name, Dict),
                 case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{read := false}}}] ->
                     Text = [<<"Yes, you are deaf to contact ">>, PeerName],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [#gaia_peer{name = PeerName,
                               conversation = {true, #{read := true}}}] ->
                     Text = [<<"No, you are not deaf to contact ">>, PeerName],
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
                             Text = <<"Yes, you are deaf to all contacts">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()];
                           false ->
                             Text = <<"No, you are not deaf to all contacts">>,
                             ok = gaia_command_serv:say(Text),
                             [{last_say, Text}|leave_command_mode()]
                         end;
                       nomatch ->
                         Text = [<<"Hey! Contact ">>, Name,
                                 <<" is not known. Please try again!">>],
                         ok = gaia_command_serv:say(Text),
                         [{cd, '..'}, {last_say, Text}]
                     end
                 end
             end),
         %%
         %% Deafen me to [contact] X
         %%
         ask_yes_no(
           deafen,
           [["deafen", "me", "to", name],
            ["deafen", "me", "to", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => deafen}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you want to be deaf to contact ">>, PeerName,
                           <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you want to be deaf to all contacts?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! Contact ">>, Name,
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
                   Text = [<<"You are now deaf to contact ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are already deaf to contact ">>,
                           PeerName],
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
               Text = [<<"You are now deaf to all contacts">>],
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Undeafen me to [contact] X
         %%
         ask_yes_no(
           undeafen,
           [["undeafen", "me", "to", name],
            ["undeafen", "me", "to", "contact", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => undeafen}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you no longer want to be deaf to contact ">>,
                           PeerName, <<"?">>],
                   ok = gaia_command_serv:say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text =
                         [<<"Do you no longer want to be deaf to all contacts?">>],
                       ok = gaia_command_serv:say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [<<"Hey! Contact ">>, Name,
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
                   Text = [<<"You are no longer deaf to contact ">>, PeerName],
                   ok = gaia_command_serv:say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"Hey! You are not deaf to contact ">>, PeerName],
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
               Text = [<<"You are no longer deaf to all contacts">>],
               ok = gaia_command_serv:say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = gaia_command_serv:say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Am I ignoring [contact] X?
         %%
         ask(am_i_ignoring,
             [["am", "i", "ignoring", name],
              ["am", "i", "ignoring", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => am_i_ignoring}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"Yes, contact ">>, Name, <<" is ignored">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"No, contact ">>, Name,
                                 <<" is not ignored">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Ignore [contact] X
         %%
         ask(ignore,
             [["ignore", name],
              ["ignore", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => ignore}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"Hey! You already ignore contact ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"ignore">>}]]}]}]),
                         Text = [<<"You now ignore contact ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not ignore [contact] X
         %%
         ask(ignore,
             [["do", "not", "ignore", name],
              ["do", "not", "ignore", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_ignore}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
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
                         Text = [<<"You no longer ignore contact ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"You do not ignore contact ">>, Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Has [contact] X direct access?
         %%
         ask(has_direct_access,
             [["has", name, "direct", "access"],
              ["has", "contact", name, "direct", "access"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => hasd_direct_access}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text = [<<"Yes, contact ">>, Name,
                                 <<" has direct access">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"No, contact ">>, Name,
                            <<" has not direct access">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Give direct access to [contact] X
         %%
         ask(give_direct_access,
             [["give", "direct", "access", "to", name],
              ["give", "direct", "access", "to", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => give_direct_access}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text =
                           [<<"Hey! You already give direct access to contact ">>,
                            Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"direct">>}]]}]}]),
                         Text = [<<"You gave direct access to contact ">>,
                                 Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not give direct access to [contact] X
         %%
         ask(do_not_give_direct_access,
             [["do", "not", "give", "direct", "access", "to", name],
              ["do", "not", "give", "direct", "access", "to", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_give_direct_access}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
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
                         Text =
                           [<<"You no longer give direct access to contact ">>,
                            Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"Hey! You do not give direct access to contact ">>,
                            Name],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Has [contact] X high priority?
         %%
         ask(has_high_priority,
             [["has", name, "high", "priority"],
              ["has", "contact", name, "high", "priority"]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => has_high_priority}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [direct] ->
                         Text = [<<"Yes, contact ">>, Name,
                                 <<" has high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text =
                           [<<"No, contact ">>, Name,
                            <<" does not have high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Give high priority to [contact] X
         %%
         ask(give_high_priority,
             [["give", "high", "priority", "to", name],
              ["give", "high", "priority", "to", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => give_high_priority}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
                             <<" is not known. Please try again!">>],
                     ok = gaia_command_serv:say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     [Options] = config:lookup_children([options], ConfigPeer),
                     case lists:member(override_busy, Options) of
                       true ->
                         Text = [<<"Hey! You already give contact ">>, Name,
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
                         Text = [<<"You gave contact ">>, Name,
                                 <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Do not give high priority to [contact] X
         %%
         ask(do_not_give_high_priority,
             [["do", "not", "give", "high", "priority", "to", name],
              ["do", "not", "give", "high", "priority", "to", "contact", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => do_not_git_high_priority}),
                 Name = ?l2b(maps:get(name, Dict)),
                 case config:lookup([gaia, peers, {name, Name}]) of
                   not_found ->
                     Text = [<<"Hey! Contact ">>, Name,
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
                         Text = [<<"You no longer give contact ">>, Name,
                                 <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       true ->
                         Text =
                           [<<"Hey! You do not give contact ">>, Name,
                            <<" high priority">>],
                         ok = gaia_command_serv:say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end),
         %%
         %% Which [contacts] are online?
         %%
         ask(online_contacts,
             [["which", "are", "online"],
              ["which", "contacts", "are", "online"]],
             fun(_Dict) ->
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
                     Text = <<"No contacts are online">>,
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   [PeerName] ->
                     Text = [<<"Contact ">>, PeerName, <<" is online">>],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()];
                   _ ->
                     Text = [<<"The contacts ">>,
                             gaia_command_serv:format_items(OnlinePeerNames),
                             <<" are online">>],
                     ok = gaia_command_serv:say(Text),
                     [{last_say, Text}|leave_command_mode()]
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
