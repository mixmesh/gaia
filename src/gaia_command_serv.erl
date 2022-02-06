-module(gaia_command_serv).
-export([start_link/1, stop/0]).
-export([local_public_groups/1, remote_public_groups/2,
         groups_of_interest_updated/2,
         peer_up/1, peer_down/1,
         conversation_accepted/1, conversation_rejected/2,
         ask_for_conversation/1,
         negotiation_failed/2]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").

%%
%% Exported: start_link
%%

-spec start_link(boolean()) ->
          serv:spawn_server_result().

start_link(UseCallback) ->
    ?spawn_server(fun(Parent) -> init(Parent, UseCallback) end,
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
%% Exported: ask_for_conversation
%%

-spec ask_for_conversation(#gaia_peer{}) -> ok.

ask_for_conversation(Peer) ->
    serv:cast(?MODULE, {ask_for_conversation, Peer}).

%%
%% Exported: negotiation_failed
%%

-spec negotiation_failed(
        #gaia_peer{}, gaia_rest_client:start_peer_negotiation_error_reason()) ->
          ok.

negotiation_failed(Peer, Reason) ->
    serv:cast(?MODULE, {negotiation_failed, Peer, Reason}).

%%
%% Server
%%

init(Parent, UseCallback) ->
    ?LOG_INFO("Gaia command server has been started"),
    {ok, #{parent => Parent, use_callback => UseCallback}}.

initial_message_handler(#{use_callback := UseCallback} = State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            case UseCallback of
                true ->
                    Callback = create_callback(),
                    ok = gaia_audio_source_serv:subscribe(Callback);
                false ->
                    ok = gaia_audio_source_serv:subscribe()
            end,
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent}) ->
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
                    flite:say([<<"You broadcasted ">>, GroupName]),
                    noreply;
                GroupIds ->
                    GroupNames = get_group_names(GroupIds),
                    flite:say(
                      [<<"You broadcasted: ">>, format_items(GroupNames)]),
                    noreply
            end;
        {cast, {remote_public_groups, PeerName, PublicGroupIds} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case PublicGroupIds of
                [GroupId] ->
                    [#gaia_group{name = GroupName}] = gaia_serv:lookup(GroupId),
                    flite:say([PeerName, <<" broadcasted ">>, GroupName]),
                    noreply;
                GroupIds ->
                    GroupNames = get_group_names(GroupIds),
                    flite:say([PeerName, <<" broadcasted: ">>,
                               format_items(GroupNames)]),
                    noreply
            end;
        {cast, {groups_of_interest_updated, PeerName, GroupNamesOfInterest}} ->
            case GroupNamesOfInterest of
                [GroupName] ->
                    flite:say([PeerName, <<" updated ">>, GroupName]),
                    noreply;
                GroupNames ->
                    flite:say(
                      [PeerName, <<" updated: ">>, format_items(GroupNames)]),
                    noreply
            end;
        {cast, {peer_up, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            flite:say([PeerName, <<" appeared">>]),
            noreply;
        {cast, {peer_down, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            flite:say([PeerName, <<" disappeared">>]),
            noreply;
        {cast, {conversation_accepted,
                #gaia_peer{name = PeerName,
                           conversation = Conversation}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case Conversation of
                {true, read} ->
                    flite:say([<<"You now listen to ">>, PeerName]),
                    noreply;
                {true, write} ->
                    flite:say([<<"You now talk to ">>, PeerName]),
                    noreply;
                {true, read_write} ->
                    flite:say([<<"You now listen *and* talk to ">>, PeerName]),
                    noreply;
                false ->
                    noreply
            end;
        {cast, {conversation_rejected, #gaia_peer{name = PeerName},
                _Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            flite:say([<<"Conversation with ">>, PeerName, <<" rejected">>]),
            noreply;
        {cast, {ask_for_conversation, #gaia_peer{name = PeerName}} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            flite:say([PeerName, <<" is calling. Do you accept?">>]),
            noreply;
        {cast, {negotiation_failed,
                #gaia_peer{name = PeerName}, _Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            flite:say([<<"Negotiation with ">>, PeerName, <<" failed!">>]),
            noreply;
        {subscription_packet, _Packet} ->
            %% Do something with the the audio packet
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

get_group_names([]) ->
    [];
get_group_names([GroupId|Rest]) ->
    case gaia_serv:lookup(GroupId) of
        [#gaia_group{name = GroupName}] ->
            [GroupName|get_group_names(Rest)];
        _ ->
            get_group_names(Rest)
    end.

format_items([Item|Rest]) ->
    [Item|format_remaining_items(Rest)].

format_remaining_items([Item]) ->
    [<<" and ">>, Item];
format_remaining_items([Item|Rest]) ->
    [<<", ">>, Item|format_remaining_items(Rest)].

create_callback() ->
    fun(_Packet) ->
            %% Do something with the the audio packet
            create_callback()
    end.
