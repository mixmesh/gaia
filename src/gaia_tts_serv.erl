-module(gaia_tts_serv).
-export([start_link/0, stop/0,
         groups_of_interest_updated/2, peer_up/1, peer_down/1,
         say/1, format_items/1]).
-export([message_handler/1]).
%% Initiated from REST service
-export([conversation_started/1, conversation_stopped/1,
         conversation_rejected/2,
         call/1]).
%% Initiated from REST client
-export([start_of_conversation_succeeded/1, start_of_conversation_failed/2,
         stop_of_conversation_succeeded/1, stop_of_conversation_failed/2]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").

%%
%% Exported: start_link
%%

-spec start_link() -> serv:spawn_server_result().

start_link() ->
    ?spawn_server(
       fun init/1,
       fun initial_message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

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
%% Exported: conversation_started
%%

-spec conversation_started(#gaia_peer{}) -> ok.

conversation_started(Peer) ->
    serv:cast(?MODULE, {conversation_started, Peer}).

%%
%% Exported: conversation_stopped
%%

-spec conversation_stopped(#gaia_peer{}) -> ok.

conversation_stopped(Peer) ->
    serv:cast(?MODULE, {conversation_stopped, Peer}).

%%
%% Exported: conversation_rejected
%%

-spec conversation_rejected(#gaia_peer{}, busy) -> ok.

conversation_rejected(Peer, Reason) ->
    serv:cast(?MODULE, {conversation_rejected, Peer, Reason}).

%%
%% Exported: call
%%

-spec call(#gaia_peer{}) -> ok.

call(Peer) ->
    serv:cast(?MODULE, {call, Peer}).

%%
%% Exported: start_of_conversation_succeeded
%%

-spec start_of_conversation_succeeded(gaia_serv:peer_name()) ->
          ok.

start_of_conversation_succeeded(PeerName) ->
    serv:cast(?MODULE, {start_of_conversation_succeeded, PeerName}).

%%
%% Exported: start_of_conversation_failed
%%

-spec start_of_conversation_failed(
        gaia_serv:peer_name(), calling | busy | not_available | error) ->
          ok.

start_of_conversation_failed(PeerName, Reason) ->
    serv:cast(?MODULE, {start_of_conversation_failed, PeerName, Reason}).

%%
%% Exported: stop_of_conversation_succeeded
%%

-spec stop_of_conversation_succeeded(gaia_serv:peer_name()) ->
          ok.

stop_of_conversation_succeeded(PeerName) ->
    serv:cast(?MODULE, {stop_of_conversation_succeeded, PeerName}).

%%
%% Exported: stop_of_conversation_failed
%%

-spec stop_of_conversation_failed(gaia_serv:peer_name(), error) ->
          ok.

stop_of_conversation_failed(PeerName, Reason) ->
    serv:cast(?MODULE, {stop_of_conversation_failed, PeerName, Reason}).

%%
%% Exported: say
%%

-spec say(binary() | iolist()) -> ok.

say(Text) ->
    ?LOG_INFO(#{say => Text}),
    _ = flite:say(Text, [{latency, 60}]),
    ok.

%%
%% Exported: format_items
%%

-spec format_items([binary()]) -> iolist().

format_items([Item]) ->
    Item;
format_items([Item|Rest]) ->
    [Item|format_remaining_items(Rest)].

format_remaining_items([Item]) ->
    [<<" and ">>, Item];
format_remaining_items([Item|Rest]) ->
    [<<", ">>, Item|format_remaining_items(Rest)].

%%
%% Server
%%

init(Parent) ->
    ?LOG_INFO("Gaia TTS server has been started"),
    {ok, #{parent => Parent}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent}) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {cast, {groups_of_interest_updated, PeerName, GroupNamesOfInterest}} ->
            case GroupNamesOfInterest of
                [GroupName] ->
                    Text = [<<"Hey! ">>, PeerName, <<" updated group ">>,
                            GroupName],
                    ok = async_say(Text),
                    noreply;
                GroupNames ->
                    Text = [<<"Hey! ">>, PeerName, <<" updated the groups ">>,
                            format_items(GroupNames)],
                    ok = async_say(Text),
                    noreply
            end;
        {cast, {peer_up, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! ">>, PeerName, <<" is now online">>],
            ok = async_say(Text),
            noreply;
        {cast, {peer_down, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! ">>, PeerName, <<" is no longer online">>],
            ok = async_say(Text),
            noreply;
        {cast, {conversation_started,
                #gaia_peer{name = PeerName,
                           conversation = Conversation}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case Conversation of
                {true, #{read := true, write := true}} ->
                    Text = [<<"Hey! You now listen and talk to ">>, PeerName],
                    ok = async_say(Text),
                    noreply;
                {true, #{read := true}} ->
                    Text = [<<"Hey! You now only listen to ">>, PeerName],
                    ok = async_say(Text),
                    noreply;
                {true, #{write := true}} ->
                    Text = [<<"Hey! You now only talk to ">>, PeerName],
                    ok = async_say(Text),
                    noreply;
                false ->
                    noreply
            end;
        {cast, {conversation_stopped,
                #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! You no longer neither listen nor talk to ">>,
                    PeerName],
            ok = async_say(Text),
            noreply;
        {cast, {conversation_rejected, #gaia_peer{name = PeerName},
                busy} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! A call from ">>, PeerName, <<" was rejected">>],
            ok = async_say(Text),
            noreply;
        {cast, {call, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! ">>, PeerName,
                    <<" is pinging you. Do you want to answer?">>],
            ok = async_say(Text),
            ok = gaia_asr_serv:trigger_callback(
                   {cd, [hi, call], #{name => PeerName}}),
            noreply;
        {cast, {start_of_conversation_succeeded, PeerName} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! You are now in a call with ">>, PeerName],
            ok = async_say(Text),
            noreply;
        {cast, {start_of_conversation_failed, PeerName, Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case Reason of
                calling ->
                    Text = [<<"Hey! You are now pinging ">>, PeerName],
                    ok = async_say(Text),
                    noreply;
                busy ->
                    Text = [<<"Hey! ">>, PeerName, <<" is busy">>],
                    ok = async_say(Text),
                    noreply;
                not_available ->
                    Text = [<<"Hey! ">>, PeerName, <<" is not available">>],
                    ok = async_say(Text),
                    noreply;
                error ->
                    Text = [<<"Hey! ">>, PeerName, <<" did not respond">>],
                    ok = async_say(Text),
                    noreply
            end;
        {cast, {stop_of_conversation_succeeded, PeerName} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! You are no longer in a call with ">>, PeerName],
            ok = async_say(Text),
            noreply;
        {cast, {stop_of_conversation_failed, PeerName, error} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            Text = [<<"Hey! ">>, PeerName, <<" did not respond">>],
            ok = async_say(Text),
            noreply;
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

async_say(Text) ->
    %% FIXME:Make a unique async sound
    ok = alsa_wave:enter(),
    ok = say(Text),
    gaia_asr_serv:trigger_callback({last_say, Text}).
