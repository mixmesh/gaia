-module(gaia_command_serv).
-export([start_link/1, stop/0]).
-export([local_public_groups/1, remote_public_groups/2,
         groups_of_interest_updated/2,
         peer_up/1, peer_down/1,
         conversation_accepted/1, conversation_rejected/2,
         call/1,
         negotiation_failed/2]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").
-include("gaia_commands.hrl").
-include("globals.hrl").

-define(MODEL, "vosk-model-small-en-us-0.15").

%%
%% Exported: start_link
%%

-spec start_link(boolean()) ->
          serv:spawn_server_result().

start_link(AudioSourceCallback) ->
    ?spawn_server(fun(Parent) -> init(Parent, AudioSourceCallback) end,
                  fun initial_message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: local_public_groups
%%

-spec local_public_groups([gaia_serv:group_id()]) -> ok.

local_public_groups([]) ->
    ok;
local_public_groups(PublicGroupIds) ->
    serv:cast(?MODULE, {local_public_groups, PublicGroupIds}).

%%
%% Exported: remote_public_groups
%%

-spec remote_public_groups(gaia_serv:peer_name(), [gaia_serv:group_id()]) -> ok.

remote_public_groups(_PeerName, []) ->
    ok;
remote_public_groups(PeerName, PublicGroupIds) ->
    serv:cast(?MODULE, {remote_public_groups, PeerName, PublicGroupIds}).

%%
%% Exported: groups_of_interest_updated
%%

-spec groups_of_interest_updated(gaia_serv:peer_name(),
                                 [gaia_serv:group_name()]) -> ok.

groups_of_interest_updated(_PeerName, []) ->
    ok;
groups_of_interest_updated(PeerName, GroupNamesOfInterest) ->
    serv:cast(?MODULE, {groups_of_interest_updated, PeerName,
                        GroupNamesOfInterest}).
%%
%% Exported: peer_up
%%

-spec peer_up(#gaia_peer{}) -> ok.

peer_up(Peer) ->
    serv:cast(?MODULE, {peer_up, Peer}).

%%
%% Exported: peer_down
%%

-spec peer_down(#gaia_peer{}) -> ok.

peer_down(Peer) ->
    serv:cast(?MODULE, {peer_down, Peer}).

%%
%% Exported: conversation_accepted
%%

-spec conversation_accepted(#gaia_peer{}) -> ok.

conversation_accepted(Peer) ->
    serv:cast(?MODULE, {conversation_accepted, Peer}).

%%
%% Exported: conversation_rejected
%%

-spec conversation_rejected(#gaia_peer{}, busy | ignore | not_available) -> ok.

conversation_rejected(Peer, Reason) ->
    serv:cast(?MODULE, {conversation_rejected, Peer, Reason}).

%%
%% Exported: call
%%

-spec call(#gaia_peer{}) -> ok.

call(Peer) ->
    serv:cast(?MODULE, {call, Peer}).

%%
%% Exported: negotiation_failed
%%

-spec negotiation_failed(gaia_serv:peer_name(),
                         calling | busy | not_available) ->
          ok.

negotiation_failed(PeerName, Reason) ->
    serv:cast(?MODULE, {negotiation_failed, PeerName, Reason}).

%%
%% Server
%%

init(Parent, AudioSourceCallback) ->
    VoskModel = vosk:model_new(filename:join(code:priv_dir(vosk), ?MODEL)),
    VoskRecognizer = vosk:recognizer_new(VoskModel, ?RATE_IN_HZ),
    VoskTransform =
        if ?CHANNELS =:= 2 ->
                fun(Data) -> alsa_util:stereo_to_mono(?FORMAT, Data) end;
           true ->
                fun(Data) -> Data end
        end,
    ?LOG_INFO("Gaia command server has been started"),
    {ok, #{parent => Parent,
           audio_source_callback => AudioSourceCallback,
           vosk_recognizer => VoskRecognizer,
           vosk_transform => VoskTransform}}.

initial_message_handler(#{audio_source_callback := AudioSourceCallback,
                          vosk_recognizer := VoskRecognizer,
                          vosk_transform := VoskTransform} = State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            CommandState =
                #{parent => self(),
                  path => [],
                  all_commands => gaia_commands:all(),
                  dict => #{},
                  timeout_timer => undefined,
                  last_say => <<"I didn't say anything!">>},
            Callback =
                create_callback(VoskRecognizer, VoskTransform, CommandState),
            UpdatedState =
                case AudioSourceCallback of
                    true ->
                        ok = gaia_audio_source_serv:subscribe(Callback),
                        State#{local_callback => undefined};
                    false ->
                        ok = gaia_audio_source_serv:subscribe(),
                        State#{local_callback => Callback}
                    end,
            {swap_message_handler, fun ?MODULE:message_handler/1, UpdatedState}
    end.

message_handler(#{parent := Parent, local_callback := LocalCallback} = State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {cast, {local_public_groups, PublicGroupIds} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case PublicGroupIds of
                [GroupId] ->
                    [#gaia_group{name = GroupName}] = gaia_serv:lookup(GroupId),
                    Text = [<<"You broadcast the group ">>, GroupName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                GroupIds ->
                    GroupNames = get_group_names(GroupIds),
                    Text = [<<"You broadcast the groups ">>,
                            format_items(GroupNames)],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}}
            end;
        {cast, {remote_public_groups, PeerName, PublicGroupIds} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case PublicGroupIds of
                [GroupId] ->
                    [#gaia_group{name = GroupName}] = gaia_serv:lookup(GroupId),
                    Text = [PeerName, <<" broadcasts the group ">>, GroupName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                GroupIds ->
                    case get_group_names(GroupIds) of
                        [] ->
                            noreply;
                        GroupNames ->
                            Text = [PeerName, <<" broadcasts the groups ">>,
                                    format_items(GroupNames)],
                            NewLocalCallback = say(LocalCallback, Text),
                            {noreply,
                             State#{local_callback => NewLocalCallback}}
                        end
            end;
        {cast, {groups_of_interest_updated, PeerName, GroupNamesOfInterest}} ->
            case GroupNamesOfInterest of
                [GroupName] ->
                    Text = [PeerName, <<" updated ">>, GroupName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                GroupNames ->
                    Text = [PeerName, <<" updated: ">>,
                            format_items(GroupNames)],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}}
            end;
        {cast, {peer_up, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [PeerName, <<" appeared">>],
            NewLocalCallback = say(LocalCallback, Text),
            {noreply, State#{local_callback => NewLocalCallback}};
        {cast, {peer_down, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [PeerName, <<" disappeared">>],
            NewLocalCallback = say(LocalCallback, Text),
            {noreply, State#{local_callback => NewLocalCallback}};
        {cast, {conversation_accepted,
                #gaia_peer{name = PeerName,
                           conversation = Conversation}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case Conversation of
                {true, #{read := true, write := true}} ->
                    Text = [<<"You now listen *and* talk to ">>, PeerName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                {true, #{read := true}} ->
                    Text = [<<"You now listen to ">>, PeerName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                {true, #{write := true}} ->
                    Text = [<<"You now talk to ">>, PeerName],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                false ->
                    noreply
            end;
        {cast, {conversation_rejected, #gaia_peer{name = PeerName},
                _Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Conversation with ">>, PeerName, <<" rejected">>],
            NewLocalCallback = say(LocalCallback, Text),
            {noreply, State#{local_callback => NewLocalCallback}};
        {cast, {call, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [PeerName, <<" is calling. Do you want to answer?">>],
            NewLocalCallback = say(LocalCallback, Text),
            {noreply, State#{local_callback => NewLocalCallback}};
        {cast, {negotiation_failed, PeerName, Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case Reason of
                calling ->
                    Text = [<<"Calling ">>, PeerName, <<"now">>],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                busy ->
                    Text = [PeerName, <<" is busy">>],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}};
                not_available ->
                    Text = [PeerName, <<" is not available">>],
                    NewLocalCallback = say(LocalCallback, Text),
                    {noreply, State#{local_callback => NewLocalCallback}}
            end;
        {subscription_packet, Packet} when LocalCallback /= undefined ->
            NewLocalCallback = LocalCallback(Packet),
            {noreply, State#{local_callback => NewLocalCallback}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {command_timeout, TimeoutCallback} = Message ->
            ?LOG_DEBUG(#{message => Message}),
            case LocalCallback of
                undefined ->
                    ok = gaia_audio_source_serv:trigger_callback(
                           {remove_timeout, TimeoutCallback}),
                    noreply;
                _ ->
                    NewLocalCallback =
                        LocalCallback({remove_timeout, TimeoutCallback}),
                    {noreply, State#{local_callback => NewLocalCallback}}
            end;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

say(_LocalCallback = undefined, Text) ->
    ok = gaia_audio_source_serv:trigger_callback({last_say, Text}),
    _ = say(Text),
    undefined;
say(LocalCallback, Text) ->
    NewLocalCallback = LocalCallback({last_say, Text}),
    _ = say(Text),
    NewLocalCallback.

say(Text) ->
    _ = flite:say(Text, [{latency, 60}]),
    ok.

get_group_names([]) ->
    [];
get_group_names([GroupId|Rest]) ->
    case gaia_serv:lookup(GroupId) of
        [#gaia_group{name = GroupName}] ->
            [GroupName|get_group_names(Rest)];
        _ ->
            get_group_names(Rest)
    end.

format_items([Item]) ->
    Item;
format_items([Item|Rest]) ->
    [Item|format_remaining_items(Rest)].

format_remaining_items([Item]) ->
    [<<" and ">>, Item];
format_remaining_items([Item|Rest]) ->
    [<<", ">>, Item|format_remaining_items(Rest)].

create_callback(VoskRecognizer, VoskTransform, CommandState) ->
    fun({last_say, Text}) ->
            create_callback(VoskRecognizer, VoskTransform,
                            CommandState#{last_say => Text});
       ({remove_timeout, Callback}) ->
            NewCommandState = Callback(CommandState),
            create_callback(VoskRecognizer, VoskTransform,
                            NewCommandState#{timeout_timer => undefined});
       (Packet) when is_binary(Packet) ->
            case vosk:recognizer_accept_waveform(
                   VoskRecognizer, VoskTransform(Packet)) of
		0 ->
                    create_callback(VoskRecognizer, VoskTransform,
                                    CommandState);
                1 ->
                    #{"text" := Text} = vosk:recognizer_result(VoskRecognizer),
                    ?LOG_INFO(#{vosk_text => Text}),
                    NewCommandState = handle_command(Text, CommandState),
                    _ = vosk:recognizer_reset(VoskRecognizer),
                    create_callback(VoskRecognizer, VoskTransform,
                                    NewCommandState);
		-1 ->
                    ?LOG_ERROR("Vosk failed!"),
		    _ = vosk:recognizer_reset(VoskRecognizer),
                    create_callback(VoskRecognizer, VoskTransform, CommandState)
	    end;
       (OldCallback) when is_function(OldCallback) ->
            create_callback(VoskRecognizer, VoskTransform, CommandState)
    end.

handle_command(Text, #{parent := Parent,
                       path := Path,
                       all_commands := AllCommands,
                       dict := Dict,
                       timeout_timer := TimeoutTimer,
                       last_say := LastSay} = CommandState) ->
    Commands = get_commands(Path, AllCommands),
    ?LOG_INFO(#{match_commands => {string:lexemes(Text, " "), Dict, Commands}}),
    Tokens = string:lexemes(Text, " "),
    case match_command(Tokens, Dict, Commands) of
        {ok, UpdatedDict, #command{name = Name, onsuccess = OnSuccess}} ->


            Result = OnSuccess(UpdatedDict),
            CommandState#
                {path => update_path(Path, Name, Result),
                 dict => update_dict(UpdatedDict, Result),
                 timeout_timer =>
                     update_timeout_timer(Parent, TimeoutTimer, Result),
                 last_say => update_last_say(LastSay, Result)};
        nomatch ->
            case Path of
                [] ->
                    CommandState;
                _ ->
                    case match_patterns(Tokens, #{}, [["goodbye"]]) of
                        {ok, _} ->
                            gaia_commands:leave_command_mode(CommandState);
                        nomatch ->
                            case match_patterns(Tokens, #{}, [["what?"]]) of
                                {ok, _} ->
                                    ok = say(LastSay),
                                    CommandState;
                                nomatch ->
                                    ok = say("I don't understand. Try again!"),
                                    CommandState
                            end
                    end
            end
    end.

get_commands([], Commands) ->
    Commands;
get_commands([Name|Rest], [#command{name = Name, children = Children}|_]) ->
    get_commands(Rest, Children);
get_commands(Path, [_|Rest]) ->
    get_commands(Path, Rest).

match_command(_Tokens, _Dict, []) ->
    nomatch;
match_command(Tokens, Dict, [#command{patterns = Patterns} = Command|Rest]) ->
    case match_patterns(Tokens, Dict, Patterns) of
        {ok, UpdatedDict} ->
            {ok, UpdatedDict, Command};
        nomatch ->
            match_command(Tokens, Dict, Rest)
    end.

match_patterns(_Tokens, _Dict, []) ->
    nomatch;
match_patterns(Tokens, Dict, [Pattern|Rest])
  when length(Tokens) == length(Pattern) ->
    case match_pattern(Tokens, Dict, Pattern) of
        {ok, UpdatedDict} ->
            {ok, UpdatedDict};
        nomatch ->
            match_patterns(Tokens, Dict, Rest)
    end;
match_patterns(Tokens, Dict, [_|Rest]) ->
    match_patterns(Tokens, Dict, Rest).

match_pattern([], Dict, []) ->
    {ok, Dict};
match_pattern([Token|RemainingTokens], Dict, [PatternVariable|Rest])
  when is_atom(PatternVariable) ->
    match_pattern(RemainingTokens, Dict#{PatternVariable => Token}, Rest);
match_pattern([Token|RemainingTokens], Dict, [PatternToken|Rest]) ->
    Matchers =
        if
            length(Token) < 4 ->
                [exact];
            true ->
                all
        end,
    case gaia_fuzzy:match(?l2b(Token), [?l2b(PatternToken)], Matchers) of
        {ok, _} ->
            match_pattern(RemainingTokens, Dict, Rest);
        nomatch ->
            nomatch
    end.

update_path(Path, Name, SuccessResult) ->
    case lists:keysearch(cd, 1, SuccessResult) of
        {value, {_, '.'}} ->
            Path ++ [Name];
        {value, {_, '..'}} ->
            Path;
        {value, {_, NewPath}} ->
            NewPath;
        false ->
            Path ++ [Name]
    end.

update_dict(Dict, SuccessResult) ->
    case lists:keysearch(dict, 1, SuccessResult) of
        {value, {_, NewDict}} ->
            NewDict;
        false ->
            Dict
    end.

update_timeout_timer(_Parent, TimeoutTimer, []) ->
    TimeoutTimer;
update_timeout_timer(Parent, TimeoutTimer, [{set_timeout, Time, Callback}|_]) ->
    ok = cancel_timer(TimeoutTimer),
    erlang:send_after(Time, Parent, {command_timeout, Callback});
update_timeout_timer(_Parent, TimeoutTimer, [remove_timeout|_]) ->
    ok = cancel_timer(TimeoutTimer),
    undefined;
update_timeout_timer(Parent, TimeoutTimer, [_|Rest]) ->
    update_timeout_timer(Parent, TimeoutTimer, Rest).

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

update_last_say(LastSay, SuccessResult) ->
    case lists:keysearch(last_say, 1, SuccessResult) of
        {value, {_, NewLastSay}} ->
            NewLastSay;
        false ->
            LastSay
    end.
