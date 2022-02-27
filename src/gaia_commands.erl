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
         %% Call X
         %%
         #command{
            name = call,
            patterns = [["call", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => call}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_peer{name = PeerName} = Peer] ->
                      Text = [<<"Do you want to call ">>, PeerName, <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{peer => Peer}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      Text = [Name, <<" is not known. Please try again!">>],
                      ok = say(Text),
                      [{cd, '..'}, {last_say, Text}]
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{peer := #gaia_peer{id = PeerId,
                                             name = PeerName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:start_peer_conversation(
                               PeerId, #{read => true, write => true}) of
                          ok ->
                            Text =
                              [<<"You are now in a call with ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, already_started} ->
                            Text =
                              [<<"You are already in a call with ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()]
                        end
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Hang up X
         %%
         #command{
            name = hang_up,
            patterns = [["hang", "up", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => hangup}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_peer{name = PeerName} = Peer] ->
                      Text = [<<"Do you want to hangup ">>, PeerName, <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{peer => Peer}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      Text = [Name, <<" is not known. Please try again!">>],
                      ok = say(Text),
                      [{cd, '..'}, {last_say, Text}]
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{peer := #gaia_peer{id = PeerId,
                                             name = PeerName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:stop_peer_conversation(
                               PeerId) of
                          ok ->
                            Text = [<<"You are no longer in a call with ">>,
                                    PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, no_such_peer} ->
                            ?LOG_ERROR(
                               #{unexpected_return_value => no_such_peer}),
                            leave_command_mode();
                          {error, already_stopped} ->
                            Text =
                              [<<"You are not in a call with ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()]
                        end
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Join X
         %%
         #command{
            name = join,
            patterns = [["join", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => join}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_group{name = GroupName} = Group] ->
                      Text = [<<"Do you want to join ">>, GroupName, <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{group => Group}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      Text = [Name, <<" is not known. Please try again!">>],
                      ok = say(Text),
                      [{cd, '..'}, {last_say, Text}]
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{group := #gaia_group{id = GroupId,
                                               name = GroupName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:start_group_conversation(GroupId) of
                          ok ->
                            Text =
                              [<<"You are now an active member of ">>,
                               GroupName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, already_started} ->
                            Text =
                              [<<"You are already in active member of ">>,
                               GroupName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()]
                        end
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Leave X
         %%
         #command{
            name = leave,
            patterns = [["leave", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => leave}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_group{name = GroupName} = Group] ->
                      Text = [<<"Do you want to leave ">>, GroupName, <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{group => Group}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      Text = [Name, <<" is not known. Please try again!">>],
                      ok = say(Text),
                      [{cd, '..'}, {last_say, Text}]
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{group := #gaia_group{id = GroupId,
                                               name = GroupName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:stop_group_conversation(GroupId) of
                          ok ->
                            Text =
                              [<<"You are no longer active in ">>, GroupName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, no_such_group} ->
                            ?LOG_ERROR(#{unexpected_return_value =>
                                           no_such_group}),
                            leave_command_mode();
                          {error, already_stopped} ->
                            Text =
                              [<<"You are not an active member of ">>,
                               GroupName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()]
                        end
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Am I busy?
         %%
         #command{
            name = is_busy,
            patterns = [["is", "busy"], ["am", "i", "busy"]],
            onsuccess =
              fun(_Dict) ->
                  ?LOG_INFO(#{onsuccess => is_busy}),
                  case gaia_serv:busy() of
                    true ->
                      Text = <<"Yes, you are busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()];
                    false ->
                      Text = <<"No, you are not busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()]
                  end
              end},
         %%
         %% I'm busy
         %%
         #command{
            name = busy,
            patterns = [["busy"], ["i'm", "busy"]],
            onsuccess =
              fun(_Dict) ->
                  ?LOG_INFO(#{onsuccess => busy}),
                  case gaia_serv:busy() of
                    true ->
                      Text = <<"You are already busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()];
                    false ->
                      ok = gaia_serv:busy(true),
                      Text = <<"You are now busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()]
                  end
              end},
         %%
         %% I'm not busy
         %%
         #command{
            name = not_busy,
            patterns = [["not", "busy"], ["i'm", "not", "busy"]],
            onsuccess =
              fun(_Dict) ->
                  ?LOG_INFO(#{onsuccess => busy}),
                  case gaia_serv:busy() of
                    false ->
                      Text = <<"You are not busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()];
                    true ->
                      ok = gaia_serv:busy(false),
                      Text = <<"You are no longer busy">>,
                      ok = say(Text),
                      [{last_say, Text}|leave_command_mode()]
                  end
              end},
         %%
         %% Mute X
         %%
         #command{
            name = mute,
            patterns = [["mute", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => mute}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_peer{name = PeerName} = Peer] ->
                      Text =
                        [<<"Do you want to mute yourself for ">>, PeerName,
                         <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{peer => Peer}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                        {ok, _} ->
                          Text = [<<"Do you want to mute yourself for all?">>],
                          ok = say(Text),
                          [{dict, Dict#{peer => all}},
                           remove_timeout,
                           {last_say, Text}];
                        nomatch ->
                          Text = [Name, <<" is not known. Please try again!">>],
                          ok = say(Text),
                          [{cd, '..'}, {last_say, Text}]
                      end
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{peer := #gaia_peer{id = PeerId,
                                             name = PeerName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:set_peer_conversation_status(
                               PeerId, #{write => false}) of
                          ok ->
                            Text = [<<"You are now muted for ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, already_set} ->
                            Text = [<<"You are already muted for ">>, PeerName],
                            ok = say(Text),
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
                        ok = say(Text),
                        [{last_say, Text}|leave_command_mode()]
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Unmute X
         %%
         #command{
            name = unmute,
            patterns = [["unmute", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => unmute}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_peer{name = PeerName} = Peer] ->
                      Text =
                        [<<"Do you want to unmute yourself for ">>, PeerName,
                         <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{peer => Peer}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                        {ok, _} ->
                          Text =
                            [<<"Do you want to unmute yourself for all?">>],
                          ok = say(Text),
                          [{dict, Dict#{peer => all}},
                           remove_timeout,
                           {last_say, Text}];
                        nomatch ->
                          Text = [Name, <<" is not known. Please try again!">>],
                          ok = say(Text),
                          [{cd, '..'}, {last_say, Text}]
                      end
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{peer := #gaia_peer{id = PeerId,
                                             name = PeerName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:set_peer_conversation_status(
                               PeerId, #{write => true}) of
                          ok ->
                            Text = [<<"You are now unmuted for ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, already_set} ->
                            Text =
                              [<<"You are already unmuted for ">>, PeerName],
                            ok = say(Text),
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
                        ok = say(Text),
                        [{last_say, Text}|leave_command_mode()]
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Deafen X
         %%
         #command{
            name = deafen,
            patterns = [["deafen", name]],
            onsuccess =
              fun(Dict) ->
                  ?LOG_INFO(#{onsuccess => deafen}),
                  Name = maps:get(name, Dict),
                  case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                    [#gaia_peer{name = PeerName} = Peer] ->
                      Text =
                        [<<"Do you want to be deaf to ">>, PeerName, <<"?">>],
                      ok = say(Text),
                      [{dict, Dict#{peer => Peer}},
                       remove_timeout,
                       {last_say, Text}];
                    [] ->
                      case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                        {ok, _} ->
                          Text = [<<"Do you want to be deaf to all?">>],
                          ok = say(Text),
                          [{dict, Dict#{peer => all}},
                           remove_timeout,
                           {last_say, Text}];
                        nomatch ->
                          Text = [Name, <<" is not known. Please try again!">>],
                          ok = say(Text),
                          [{cd, '..'}, {last_say, Text}]
                      end
                  end
              end,
            children =
              [#command{
                  name = yes,
                  patterns = [["yes"], ["yeah"]],
                  onsuccess =
                    fun(#{peer := #gaia_peer{id = PeerId,
                                             name = PeerName}}) ->
                        ?LOG_INFO(#{onsuccess => yes}),
                        case gaia_serv:set_peer_conversation_status(
                               PeerId, #{read => false}) of
                          ok ->
                            Text = [<<"You are now deaf to ">>, PeerName],
                            ok = say(Text),
                            [{last_say, Text}|leave_command_mode()];
                          {error, already_set} ->
                            Text = [<<"You are already deaf to ">>, PeerName],
                            ok = say(Text),
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
                        ok = say(Text),
                        [{last_say, Text}|leave_command_mode()]
                    end},
               #command{
                  name = no,
                  patterns = [["no"], ["nah"]],
                  onsuccess =
                    fun(_Dict) ->
                        ?LOG_INFO(#{onsuccess => no}),
                        ok = say(<<"OK">>),
                        leave_command_mode()
                    end}]},
         %%
         %% Undeafen X
         %%
         ask_yes_no(
           undeafen,
           [["undeafen", name]],
           fun(Dict) ->
               ?LOG_INFO(#{onsuccess => undeafen}),
               Name = maps:get(name, Dict),
               case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                 [#gaia_peer{name = PeerName} = Peer] ->
                   Text = [<<"Do you no longer want to be deaf to ">>, PeerName,
                           <<"?">>],
                   ok = say(Text),
                   [{dict, Dict#{peer => Peer}},
                    remove_timeout,
                    {last_say, Text}];
                 [] ->
                   case gaia_fuzzy:match(?l2b(Name), [<<"all">>]) of
                     {ok, _} ->
                       Text = [<<"Do you no longer want to be deaf to all?">>],
                       ok = say(Text),
                       [{dict, Dict#{peer => all}},
                        remove_timeout,
                        {last_say, Text}];
                     nomatch ->
                       Text = [Name, <<" is not known. Please try again!">>],
                       ok = say(Text),
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
                   ok = say(Text),
                   [{last_say, Text}|leave_command_mode()];
                 {error, already_set} ->
                   Text = [<<"You are already hearing ">>, PeerName],
                   ok = say(Text),
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
               ok = say(Text),
               [{last_say, Text}|leave_command_mode()]
           end,
           fun(_Dict) ->
               ?LOG_INFO(#{onsuccess => no}),
               ok = say(<<"OK">>),
               leave_command_mode()
           end),
         %%
         %% Is X ignored?
         %%
         ask(is_ignored,
             [["is", name, "ignored"],
              ["do", "i", "ignore", name]],
             fun(Dict) ->
                 ?LOG_INFO(#{onsuccess => is_ignored}),
                 Name = maps:get(name, Dict),
                 case config:lookup([gaia, peers, {name, ?l2b(Name)}]) of
                   not_found ->
                     Text = [Name, <<" is not known. Please try again!">>],
                     ok = say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"Yes, ">>, Name, <<" is ignored">>],
                         ok = say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"No, ">>, Name, <<" is not ignored">>],
                         ok = say(Text),
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
                     Text = [Name, <<" is not known. Please try again!">>],
                     ok = say(Text),
                     [{cd, '..'}, {last_say, Text}];
                   ConfigPeer ->
                     case config:lookup_children([mode], ConfigPeer) of
                       [ignore] ->
                         Text = [<<"You already ignore ">>, Name],
                         ok = say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         ok = config:edit_config(
                                [{gaia,
                                  [{peers,
                                    [[{name, Name},
                                      {mode, <<"ignore">>}]]}]}]),
                         Text = [<<"You now ignore ">>, Name],
                         ok = say(Text),
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
                     Text = [Name, <<" is not known. Please try again!">>],
                     ok = say(Text),
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
                         ok = say(Text),
                         [{last_say, Text}|leave_command_mode()];
                       _ ->
                         Text = [<<"You do not ignore ">>, Name],
                         ok = say(Text),
                         [{last_say, Text}|leave_command_mode()]
                     end
                 end
             end)]}].

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
  ok = mute(),
  ok = beep(enter_command_mode),
  [].
%[{set_timeout, 4000, fun leave_command_mode/1}].

say(Text) ->
  _ = flite:say(Text, [{latency, 60}]),
  ok.

leave_command_mode() ->
  ?LOG_DEBUG(#{leave_command_mode => now}),
  ok = beep(leave_command_mode),
  unmute(),
  [{cd, []}, {dict, #{}}].

leave_command_mode(CommandState) ->
  ?LOG_DEBUG(#{leave_command_mode => now2}),
  ok = beep(leave_command_mode),
  ok = unmute(),
  CommandState#{path => [], dict => #{}}.

beep(enter_command_mode) ->
  ok = alsa_wave:enter(),
  %% FIXME: lower volume!!!
  %%spawn(fun() -> alsa_wave:mute() end),
  ok;
beep(leave_command_mode) ->
  alsa_wave:leave().

mute() ->
  ?LOG_DEBUG(#{mute => now}),
  ok.

unmute() ->
  ?LOG_DEBUG(#{unmute => now}),
  ok.
