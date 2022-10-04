-module(gaia_asr_serv).
-export([start_link/1, stop/0,
         serve_only_me/0, serve_all/0,
         listen/0, unlisten/0,
         trigger_callback/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia_commands.hrl").
-include("globals.hrl").

-define(MODEL, "vosk-model-small-en-us-0.15").

%%
%% Exported: start_link
%%

-spec start_link(boolean()) -> serv:spawn_server_result().

start_link(ListenAlways) ->
    ?spawn_server(
       fun(Parent) -> init(Parent, ListenAlways) end,
       fun initial_message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: serve_only_me
%%

-spec serve_only_me() -> ok.

serve_only_me() ->
    serv:cast(?MODULE, serve_only_me).

%%
%% Exported: serve_all
%%

-spec serve_all() -> ok.

serve_all() ->
    serv:cast(?MODULE, serve_all).

%%
%% Exported: listen
%%

-spec listen() -> ok.

listen() ->
    serv:cast(?MODULE, listen).

%%
%% Exported: unlisten
%%

-spec unlisten() -> ok.

unlisten() ->
    serv:cast(?MODULE, unlisten).

%%
%% Exported: trigger_callback
%%

trigger_callback(Term) ->
    serv:cast(?MODULE, {trigger_callback, Term}).

%%
%% Server
%%

init(Parent, ListenAlways) ->
    VoskModel = vosk:model_new(filename:join(code:priv_dir(vosk), ?MODEL)),
    VoskRecognizer = vosk:recognizer_new(VoskModel, ?RATE_IN_HZ),
    VoskTransform =
        if ?CHANNELS =:= 2 ->
                fun(Data) -> alsa_util:stereo_to_mono(?FORMAT, Data) end;
           true ->
                fun(Data) -> Data end
        end,
    ?LOG_INFO("Gaia ASR server has been started"),
    {ok, #{parent => Parent,
           listen_always => ListenAlways,
           vosk_recognizer => VoskRecognizer,
           vosk_transform => VoskTransform}}.

initial_message_handler(#{listen_always := ListenAlways,
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
            case ListenAlways of
                true ->
                    ok = gaia_audio_source_serv:subscribe(),
                    {swap_message_handler, fun ?MODULE:message_handler/1,
                     State#{callback => Callback, listen => true}};
                false ->
                    {swap_message_handler, fun ?MODULE:message_handler/1,
                     State#{callback => Callback, listen => false}}
            end
    end.

message_handler(#{parent := Parent,
                  listen_always := ListenAlways,
                  callback := Callback,
                  listen := _Listen} = State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {cast, serve_only_me = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            %% FIXME: enable serve only!!!
            %%ok = gaia_audio_source_serv:serve_only_me()
            noreply;
        {cast, serve_all = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            %% FIXME: enable serve only!!!
            %%ok = gaia_audio_source_serv:serve_all(),
            noreply;
        {cast, listen = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            UpdatedCallback = Callback({handle_command, "command"}),
            ok = gaia_audio_source_serv:subscribe(),
            {noreply, State#{callback => UpdatedCallback, listen => true}};
        {cast, unlisten = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case ListenAlways of
                true ->
                    noreply;
                false ->
                    ok = gaia_audio_source_serv:unsubscribe(),
                    UpdatedCallback = Callback(leave_command_mode),
                    {noreply, State#{callback => UpdatedCallback,
                                     listen => false}}
            end;
        {cast, {trigger_callback, Term}} ->
            {noreply, State#{callback => Callback(Term)}};
        {subscription_packet, Packet} ->
            {noreply, State#{callback => Callback(Packet)}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {command_timeout, TimeoutCallback} = Message ->
            ?LOG_DEBUG(#{message => Message}),
            UpdatedCallback = Callback({remove_timeout, TimeoutCallback}),
            {noreply, State#{callback => UpdatedCallback}};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

create_callback(VoskRecognizer, VoskTransform, CommandState) ->
    fun(leave_command_mode) ->
            ok = gaia_commands:leave_command_mode(),
            create_callback(VoskRecognizer, VoskTransform,
                            CommandState#{path => [], dict => #{}});
       ({last_say, Text}) ->
            create_callback(VoskRecognizer, VoskTransform,
                            CommandState#{last_say => Text});
       ({remove_timeout, Callback}) ->
            NewCommandState = Callback(CommandState),
            create_callback(VoskRecognizer, VoskTransform,
                            NewCommandState#{timeout_timer => undefined});
       ({cd, Path, Dict}) ->
            create_callback(VoskRecognizer, VoskTransform,
                            CommandState#{path => Path, dict => Dict});
       ({handle_command, Text}) ->
            NewCommandState = handle_command(Text, CommandState),
            create_callback(VoskRecognizer, VoskTransform, NewCommandState);
       (Packet) when is_binary(Packet) ->
            case vosk:recognizer_accept_waveform(
                   VoskRecognizer, VoskTransform(Packet)) of
		0 ->
                    create_callback(VoskRecognizer, VoskTransform,
                                    CommandState);
                1 ->
                    case vosk:recognizer_result(VoskRecognizer) of
                        #{"text" := ""} ->
                            _ = vosk:recognizer_reset(VoskRecognizer),
                            create_callback(VoskRecognizer, VoskTransform,
                                            CommandState);
                        #{"text" := "huh"} ->
                            _ = vosk:recognizer_reset(VoskRecognizer),
                            create_callback(VoskRecognizer, VoskTransform,
                                            CommandState);
                        #{"text" := Text} ->
                            ?LOG_INFO(#{vosk_text => Text}),
                            ok = purge_subscription_packets(0),
                            NewCommandState =
                                handle_command(Text, CommandState),
                            _ = vosk:recognizer_reset(VoskRecognizer),
                            create_callback(VoskRecognizer, VoskTransform,
                                            NewCommandState)
                    end;
		-1 ->
                    ?LOG_ERROR("Vosk failed!"),
		    _ = vosk:recognizer_reset(VoskRecognizer),
                    create_callback(VoskRecognizer, VoskTransform, CommandState)
	    end;
       (OldCallback) when is_function(OldCallback) ->
            create_callback(VoskRecognizer, VoskTransform, CommandState)
    end.

purge_subscription_packets(Size) ->
    receive
        {subscription_packet, Packet} ->
            purge_subscription_packets(Size + size(Packet))
    after
        0 ->
            if
                Size == 0 ->
                    ok;
                true ->
                    ?LOG_INFO(#{purging_packets => Size}),
                    ok
            end
    end.

handle_command(Text, #{parent := Parent,
                       path := Path,
                       all_commands := AllCommands,
                       dict := Dict,
                       timeout_timer := TimeoutTimer,
                       last_say := LastSay} = CommandState) ->
    Commands = get_commands(Path, AllCommands),
    Tokens = string:lexemes(Text, " "),
    %%?LOG_INFO(#{match_commands => {Tokens, Dict, Commands}}),
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
                            ok = gaia_pa_serv:playcd(gaia_serv),
                            CommandState#{path => [], dict => #{}};
                        nomatch ->
                            case match_patterns(Tokens, #{}, [["what?"]]) of
                                {ok, _} ->
                                    ok = gaia_tts_serv:say(LastSay),
                                    CommandState;
                                nomatch ->
                                    ok = gaia_tts_serv:say(
                                           "I don't understand. Try again!"),
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
match_pattern([Token|RemainingTokens], Dict,
              [PatternVariable|RemainingPatterns])
  when is_atom(PatternVariable) ->
    match_pattern(RemainingTokens, Dict#{PatternVariable => Token},
                  RemainingPatterns);
match_pattern([Token|RemainingTokens], Dict,
              [[Pattern|_] = AlternativePatterns|RemainingPatterns])
  when is_list(Pattern) orelse is_atom(Pattern) ->
    case check_alternative_patterns(Token, Dict, AlternativePatterns) of
        {ok, NewDict} ->
            match_pattern(RemainingTokens, NewDict, RemainingPatterns);
        nomatch ->
            nomatch
    end;
match_pattern([Token|RemainingTokens], Dict,
              [PatternToken|RemainingPatterns]) ->
    Matchers =
        if
            length(Token) < 4 ->
                [exact];
            true ->
                all
        end,
    case gaia_fuzzy:match(?l2b(Token), [?l2b(PatternToken)], Matchers) of
        {ok, _} ->
            match_pattern(RemainingTokens, Dict, RemainingPatterns);
        nomatch ->
            nomatch
    end.

check_alternative_patterns(_Token, _Dict, []) ->
    nomatch;
check_alternative_patterns(Token, Dict, [Pattern|Rest]) ->
    case match_pattern([Token], Dict, [Pattern]) of
        {ok, NewDict} ->
            {ok, NewDict};
        nomatch ->
            check_alternative_patterns(Token, Dict, Rest)
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
