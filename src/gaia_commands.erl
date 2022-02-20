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
             %% Call contact X
             %%
             #command{
                name = call,
                patterns = [["call", name], ["call", "contact", name]],
                onsuccess =
                    fun(Dict) ->
                            ?LOG_INFO(#{onsuccess => call}),
                            Name = maps:get(name, Dict),
                            case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                                [#gaia_peer{name = PeerName} = Peer] ->
                                    Text = [<<"Do you want to call ">>,
                                            PeerName, <<"?">>],
                                    ok = say(Text),
                                    [{dict, Dict#{peer => Peer}},
                                     remove_timeout,
                                     {last_say, Text}];
                                [] ->
                                    Text =
                                        [Name,
                                         <<" is not known. Please try again!">>],
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
                                           PeerId, read_write) of
                                        ok ->
                                            Text =
                                                [<<"You are now in a call with ">>,
                                                 PeerName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()];
                                        {error, already_started} ->
                                            Text =
                                                [<<"You are already in a call with ">>,
                                                 PeerName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()]
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
             %% Hangup contact X
             %%
             #command{
                name = hangup,
                patterns = [["hangup", name], ["hangup", "contact", name]],
                onsuccess =
                    fun(Dict) ->
                            ?LOG_INFO(#{onsuccess => hangup}),
                            Name = maps:get(name, Dict),
                            case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                                [#gaia_peer{name = PeerName} = Peer] ->
                                    Text = [<<"Do you want to hangup ">>,
                                            PeerName, <<"?">>],
                                    ok = say(Text),
                                    [{dict, Dict#{peer => Peer}},
                                     remove_timeout,
                                     {last_say, Text}];
                                [] ->
                                    Text =
                                        [Name,
                                         <<" is not known. Please try again!">>],
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
                                            Text =
                                                [<<"You are no longer in a call with ">>,
                                                 PeerName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()];
                                        {error, no_such_peer} ->
                                            ?LOG_ERROR(
                                               #{unexpected_return_value =>
                                                     no_such_peer}),
                                            leave_command_mode();
                                        {error, already_stopped} ->
                                            Text =
                                                [<<"You are not in a call with ">>, PeerName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()]
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
             %% Join group X
             %%
             #command{
                name = join,
                patterns = [["join", name], ["join", "group", name]],
                onsuccess =
                    fun(Dict) ->
                            ?LOG_INFO(#{onsuccess => join}),
                            Name = maps:get(name, Dict),
                            case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                                [#gaia_group{name = GroupName} = Group] ->
                                    Text = [<<"Do you want to join ">>,
                                            GroupName, <<"?">>],
                                    ok = say(Text),
                                    [{dict, Dict#{group => Group}},
                                     remove_timeout,
                                     {last_say, Text}];
                                [] ->
                                    Text =
                                        [Name,
                                         <<" is not known. Please try again!">>],
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
                                            [{last_say, Text}|
                                             leave_command_mode()];
                                        {error, already_started} ->
                                            Text =
                                                [<<"You are already in active member of ">>,
                                                 GroupName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()]
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
             %% Leave group X
             %%
             #command{
                name = leave,
                patterns = [["leave", name], ["leave", "group", name]],
                onsuccess =
                    fun(Dict) ->
                            ?LOG_INFO(#{onsuccess => leave}),
                            Name = maps:get(name, Dict),
                            case gaia_serv:lookup({fuzzy_name, ?l2b(Name)}) of
                                [#gaia_group{name = GroupName} = Group] ->
                                    Text = [<<"Do you want to leave ">>,
                                            GroupName, <<"?">>],
                                    ok = say(Text),
                                    [{dict, Dict#{group => Group}},
                                     remove_timeout,
                                     {last_say, Text}];
                                [] ->
                                    Text = [Name,
                                            <<" is not known. Please try again!">>],
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
                                    case gaia_serv:stop_group_conversation(
                                           GroupId) of
                                        ok ->
                                            Text =
                                                [<<"You are no longer active in ">>,
                                                 GroupName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()];
                                        {error, no_such_group} ->
                                            ?LOG_ERROR(
                                               #{unexpected_return_value =>
                                                     no_such_group}),
                                            leave_command_mode();
                                        {error, already_stopped} ->
                                            Text =
                                                [<<"You are not an active member of ">>,
                                                 GroupName],
                                            ok = say(Text),
                                            [{last_say, Text}|
                                             leave_command_mode()]
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
                                    Text = <<"Yes">>,
                                    ok = say(Text),
                                    [{last_say, Text}|leave_command_mode()];
                                false ->
                                    Text = <<"No">>,
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
                                    Text =
                                        <<"You are already flagged as busy">>,
                                    ok = say(Text),
                                    [{last_say, Text}|leave_command_mode()];
                                false ->
                                    ok = gaia_serv:busy(true),
                                    Text = <<"You are now flagged as busy">>,
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
                                true ->
                                    Text = <<"You aren't flagged as busy">>,
                                    ok = say(Text),
                                    [{last_say, Text}|leave_command_mode()];
                                false ->
                                    ok = gaia_serv:busy(true),
                                    Text =
                                        <<"You are no longer flagged as busy">>,
                                    ok = say(Text),
                                    [{last_say, Text}|leave_command_mode()]
                            end
                    end}]}].

enter_command_mode() ->
    ?LOG_DEBUG(#{enter_command_mode => now}),
    ok = mute(),
    ok = beep(enter_command_mode),
    [].
%    [{set_timeout, 4000, fun leave_command_mode/1}].

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

beep(Sound) ->
    ?LOG_DEBUG(#{beep => Sound}),
    alsa_wave:play(#{rate => 16000,
                     envelope => #{sustain => 0.05,
                                   release => 0.05,
                                   peek_level => 0.9,
                                   sustain_level => 0.7},
                     waves => [[{sine, ["C4"]}],
                               [{sine, ["E4"]}],
                               [{sine, ["G4"]}]]}),
    ok.

mute() ->
    ?LOG_DEBUG(#{mute => now}),
    ok.

unmute() ->
    ?LOG_DEBUG(#{unmute => now}),
    ok.
