-module(gaia_command_serv).
-export([start_link/1, stop/0]).
-export([my_public_group_ids/1, peer_has_public_groups/1,
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

-spec start_link(string()) ->
          serv:spawn_server_result().

start_link(CapturePcmName) ->
    ?spawn_server(fun(Parent) -> init(Parent, CapturePcmName) end,
                  fun initial_message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: my_public_group_ids
%%

-spec my_public_group_ids([gaia_serv:group_id()]) -> ok.

my_public_group_ids(PublicGroupIds) ->
    serv:cast(?MODULE, {my_public_group_ids, PublicGroupIds}).

%%
%% Exported: peer_has_public_groups
%%

-spec peer_has_public_groups(#gaia_group{}) -> ok.

peer_has_public_groups(PublicGroups) ->
    serv:cast(?MODULE, {peer_has_public_groups, PublicGroups}).

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

init(Parent, _CapturePcmName) ->
    ?LOG_INFO("Gaia command server has been started"),
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
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {cast, {my_public_group_ids, PublicGroupIds} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case PublicGroupIds of
                [GroupId] ->
                    [#gaia_group{name = GroupName}] = gaia_serv:lookup(GroupId),
                    flite:say([<<"You have announced the public group ">>,
                               GroupName, <<".">>]),
                    noreply;
                GroupIds ->
                    GroupNames = get_group_names(GroupIds),
                    flite:say([<<"You have announced the following public groups: ">>,
                               format_items(GroupNames), <<".">>]),
                    noreply
            end;
        {cast, {peer_has_public_groups, PublicGroups} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case PublicGroups of
                [#gaia_group{name = GroupName, admin = Admin}] ->
                    case gaia_serv:lookup(Admin) of
                        [#gaia_peer{name = PeerName}] ->
                            flite:say([<<"Peer ">>, PeerName,
                                       <<" has announced the public group ">>,
                                       GroupName, <<".">>]),
                            noreply;
                        _ ->
                            noreply
                    end;
                [#gaia_group{admin = Admin}|Rest] ->
                    case gaia_serv:lookup(Admin) of
                        [#gaia_peer{name = PeerName}] ->
                            GroupNames =
                                [GroupName ||
                                    #gaia_group{name = GroupName} <-
                                        PublicGroups],
                            flite:say([<<"Peer ">>, PeerName,
                                       <<" has announced the following public groups: ">>,
                                       GroupNames, <<".">>]),
                            noreply;
                        _ ->
                            noreply
                    end
            end;
        {cast, {peer_up, _Peer} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            noreply;
        {cast, {peer_down, _Peer} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            noreply;
        {cast, {conversation_accepted, _Peer} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            noreply;
        {cast, {conversation_rejected, _Peer, _Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            noreply;
        {cast, {ask_for_conversation, _Peer} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            noreply;
        {cast, {negotiation_failed, _Peer, _Reason} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
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
