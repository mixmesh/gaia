-module(gaia_serv).
-export([start_link/5, stop/0]).
-export([busy/0, busy/1,
         start_peer_conversation/2, stop_peer_conversation/1,
         set_peer_conversation_status/2,
         start_group_conversation/1, stop_group_conversation/1,
         lookup/1, fold/2,
         generate_artificial_id/1,
         handle_peer_negotiation/2]).
-export([message_handler/1]).
-export_type([name/0, peer_name/0, group_name/0,
              id/0, peer_id/0, group_id/0,
              mode/0, options/0,
              conversation_status/0,
              group_type/0,
              session_key/0,
              conversations/0]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").
-include("globals.hrl").

-type name() :: binary().
-type peer_name() :: name().
-type group_name() :: name().
-type id() :: non_neg_integer().
-type peer_id() :: id().
-type group_id() :: id().
-type mode() :: direct | ask | ignore.
-type options() :: [override_busy | known_peers_only].
-type conversation_status() :: read | write | read_write.
-type group_type() :: open | closed.
-type session_key() :: binary().
-type conversations() ::
        [{peer, gaia_serv:peer_id()}|
         {group, gaia_serv:group_id(), inet:ip_address() | undefined,
          inet:port_number()}].

%%
%% Exported: start_link
%%

-spec start_link(binary(), peer_name(), peer_id(), inet:port_number(),
                 string()) ->
          serv:spawn_server_result().

start_link(GaiaDir, PeerName, PeerId, RestPort, PlaybackPcmName) ->
    ?spawn_server(
       fun(Parent) ->
               init(Parent, GaiaDir, PeerName, PeerId, RestPort,
                    PlaybackPcmName)
       end,
       fun initial_message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: busy
%%

-spec busy() -> boolean().

busy() ->
    serv:call(?MODULE, busy).

-spec busy(boolean()) -> ok.

busy(Busy) ->
    serv:call(?MODULE, {busy, Busy}).

%%
%% Exported: start_peer_conversation
%%

-spec start_peer_conversation(
        peer_id() | {name, peer_name()}, conversation_status()) ->
          ok | {error, no_such_peer | already_started}.

start_peer_conversation(PeerIdOrName, ConversationStatus) ->
    serv:call(?MODULE, {start_peer_conversation, PeerIdOrName,
                        ConversationStatus}).

%%
%% Exported: stop_peer_conversation
%%

-spec stop_peer_conversation(peer_id() | {name, peer_name} | all) ->
          ok | {error, no_such_peer | already_stopped}.

stop_peer_conversation(PeerIdOrName) ->
    serv:call(?MODULE, {stop_peer_conversation, PeerIdOrName}).

%%
%% Exported: set_peer_conversation_status
%%

-spec set_peer_conversation_status(
        peer_id() | {name, peer_name()}, conversation_status()) ->
          ok | {error, no_such_peer | conversation_not_started}.

set_peer_conversation_status(PeerIdOrName, ConversationStatus) ->
    serv:call(?MODULE, {set_peer_conversation_status, PeerIdOrName,
                        ConversationStatus}).

%%
%% Exported: start_group_conversation
%%

-spec start_group_conversation(group_id() | {name, group_name()}) ->
          ok | {error, no_such_group | already_started}.

start_group_conversation(GroupIdOrName) ->
    serv:call(?MODULE, {start_group_conversation, GroupIdOrName}).

%%
%% Exported: stop_group_conversation
%%

-spec stop_group_conversation(group_id() | {name, group_name()} | all) ->
          ok | {error, no_such_group | already_stopped}.

stop_group_conversation(GroupIdOrName) ->
    serv:call(?MODULE, {stop_group_conversation, GroupIdOrName}).

%%
%% Exported: lookup
%%

-spec lookup(id() | {name, name()}) -> [#gaia_peer{}|#gaia_group{}].

lookup(IdOrName) ->
    serv:call(?MODULE, {lookup, IdOrName}).

%%
%% Exported: fold
%%

-spec fold(fun((#gaia_peer{} | #gaia_group{}, AccIn :: term()) ->
                      AccOut :: term()), Acc0 :: term()) -> Acc1 :: term().

fold(Fun, Acc0) ->
    serv:call(?MODULE, {fold, Fun, Acc0}).

%%
%% generate_artificial_id
%%

-spec generate_artificial_id(peer_name()) -> peer_id().

generate_artificial_id(PeerName) when size(PeerName) < 5 ->
    binary:decode_unsigned(PeerName);
generate_artificial_id(PeerName) ->
    binary:decode_unsigned(binary:part(PeerName, {byte_size(PeerName), -4})).

%%
%% Exported: handle_peer_negotiation
%%

-spec handle_peer_negotiation(peer_id(), inet:port_number()) ->
          {ok, inet:port_number()} |
          {error, ignore | no_nodis_address | ask | busy | not_available |
           no_such_peer}.

handle_peer_negotiation(PeerId, RemotePort) ->
    serv:call(?MODULE, {handle_peer_negotiation, PeerId, RemotePort}).

%%
%% Server
%%

init(Parent, GaiaDir, PeerName, PeerId, RestPort, PlaybackPcmName) ->
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start(#{pcm_name => PlaybackPcmName,
                          playback_audio => ?PLAYBACK_AUDIO}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    Db = new_db(GaiaDir),
    NodeInfo = prepare_node_info(PeerId, RestPort, Db),




    ?LOG_INFO(#{prepare_node_info => NodeInfo}),




    ok = nodis:set_node_info(NodeInfo),
    ok = config_serv:subscribe(),
    {ok, NodisSubscription} = nodis_serv:subscribe(),
    ?LOG_INFO("Gaia server has been started"),
    {ok, #{parent => Parent,
           peer_id => PeerId,
           peer_name => PeerName,
           busy => false,
           db => Db,
           nodis_subscription => NodisSubscription}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [NetworkSenderPid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_network_sender_serv], NeighbourWorkers),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{network_sender_pid => NetworkSenderPid}}
    end.

message_handler(#{parent := Parent,
                  peer_id := MyPeerId,
                  peer_name := MyPeerName,
                  db := Db,
                  busy := Busy,
                  nodis_subscription := NodisSubscription,
                  network_sender_pid := NetworkSenderPid} = State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {call, From, busy = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            {reply, From, Busy};
        {call, From, {busy, Busy} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            {reply, From, ok, State#{busy => Busy}};
        {call, From, {start_peer_conversation, PeerIdOrName,
                      ConversationStatus} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_peer(Db, PeerIdOrName) of
                [#gaia_peer{conversation = {true, _ConversationStatus}}] ->
                    {reply, From, {error, already_started}};
                [Peer] ->
                    UpdatedPeer =
                        Peer#gaia_peer{conversation =
                                           {true, ConversationStatus}},
                    true = db_insert(Db, UpdatedPeer),
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, no_such_peer}}
            end;
        {call, From, {stop_peer_conversation, all} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            ok = db_fold(
                   fun(#gaia_peer{
                          conversation = {true, _ConversationStatus}} = Peer,
                       Acc) ->
                           true = db_insert(Db, Peer#gaia_peer{
                                                  conversation = false}),
                           Acc;
                      (_, Acc) ->
                           Acc
                   end, ok, Db),
            ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
            {reply, From, ok};
        {call, From, {stop_peer_conversation, PeerIdOrName} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_peer(Db, PeerIdOrName) of
                [#gaia_peer{conversation = false}] ->
                    {reply, From, {error, already_stopped}};
                [Peer] ->
                    UpdatedPeer = Peer#gaia_peer{conversation = false},
                    true = db_insert(Db, UpdatedPeer),
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, no_such_peer}}
            end;
        {call, From, {set_peer_conversation_status, PeerIdOrName,
                      ConversationStatus} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_peer(Db, PeerIdOrName) of
                [#gaia_peer{conversation =
                                {true, _ConversationStatus}} = Peer] ->
                    UpdatedPeer =
                        Peer#gaia_peer{conversation =
                                           {true, ConversationStatus}},
                    true = db_insert(Db, UpdatedPeer),
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    {reply, From, ok};
                [_] ->
                    {reply, From, {error, conversation_not_started}};
                [] ->
                    {reply, From, {error, no_such_peer}}
            end;
        {call, From, {start_group_conversation, GroupIdOrName} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_group(Db, GroupIdOrName) of
                [#gaia_group{conversation = true}] ->
                    {reply, From, already_started};
                [Group] ->
                    UpdatedGroup = Group#gaia_group{conversation = true},
                    true = db_insert(Db, UpdatedGroup),
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, no_such_group}}
            end;
        {call, From, {stop_group_conversation, all} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            ok = db_fold(
                   fun(#gaia_group{conversation = true} = Group, Acc) ->
                           true = db_insert(
                                    Db, Group#gaia_group{conversation = false}),
                           Acc;
                      (_, Acc) ->
                           Acc
                   end, ok, Db),
            ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
            {reply, From, ok};
        {call, From, {stop_group_conversation, GroupIdOrName}= Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_group(Db, GroupIdOrName) of
                [#gaia_group{conversation = false}] ->
                    {reply, From, {error, already_stopped}};
                [Group] ->
                    UpdatedGroup = Group#gaia_group{conversation = false},
                    true = db_insert(Db, UpdatedGroup),
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, no_such_group}}
            end;
        {call, From, {lookup, IdOrName} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            {reply, From, db_lookup(Db, IdOrName)};
        {call, From, {fold, Fun, Acc0} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            {reply, From, db_fold(Fun, Acc0, Db)};
        {call, From, {handle_peer_negotiation, PeerId, RemotePort} = Call} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => Call}),
            case db_lookup_peer_by_id(Db, PeerId) of
                [#gaia_peer{name = PeerName} = Peer] ->
                    case accept_peer(Busy, Peer) of
                        {no, Reason} ->
                            {reply, From, {error, Reason}};

                        {yes, UpdatedPeer} ->
                            true = db_insert(Db, UpdatedPeer#gaia_peer{
                                                   remote_port = RemotePort}),
                            ok = update_network(
                                   MyPeerId, Db, Busy, NetworkSenderPid, false),
                            case db_lookup_peer_by_id(Db, PeerId) of
                                [#gaia_peer{local_port = undefined}] ->
                                    ?LOG_INFO(
                                       #{peer_negotiation_failed =>
                                             {MyPeerName, PeerName}}),
                                    {reply, From, {error, not_available}};
                                [#gaia_peer{local_port = LocalPort}] ->
                                    ?LOG_INFO(
                                       #{peer_negotiation_succeeded =>
                                             {MyPeerName, PeerName},
                                         local_port => LocalPort}),
                                    {reply, From, {ok, LocalPort}}
                            end
                    end;
                [] ->
                    {reply, From, {error, no_such_peer}}
            end;
        config_update = Message ->
            ?LOG_DEBUG(#{module => ?MODULE, message => Message}),
            true = sync_db(Db),
            ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
            noreply;
        {nodis, NodisSubscription, {pending, _NodisAddress} = NodisEvent} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis_event => NodisEvent}),
            noreply;
        {nodis, NodisSubscription, {up, _NodisAddress} = NodisEvent} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis_event => NodisEvent}),
            noreply;
        {nodis, NodisSubscription, {change, NodisAddress, Info} = NodisEvent} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis_event => NodisEvent}),
            case change_peer(MyPeerId, Db, NodisAddress, Info) of
                ok ->
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    noreply;
                {error, Reason} ->
                    ?LOG_ERROR(#{module => ?MODULE, change_peer => Reason}),
                    noreply
            end;
        {nodis, NodisSubscription, {wait, _NodisAddress} = NodisEvent} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis_event => NodisEvent}),
            noreply;
        {nodis, NodisSubscription, {down, NodisAddress} = NodisEvent} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis_event => NodisEvent}),
            case down_peer(Db, NodisAddress) of
                ok ->
                    ok = update_network(MyPeerId, Db, Busy, NetworkSenderPid),
                    noreply;
                {error, Reason} ->
                    ?LOG_ERROR(#{module => ?MODULE, down_peer => Reason}),
                    noreply
            end;
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

%%
%% Network management
%%

update_network(MyPeerId, Db, Busy, NetworkSenderPid) ->
    update_network(MyPeerId, Db, Busy, NetworkSenderPid, _Negotiate = true).

update_network(MyPeerId, Db, Busy, NetworkSenderPid, Negotiate) ->
    Conversations = extract_conversations(Db, Busy),
    ?LOG_INFO(#{module => ?MODULE, conversations => Conversations}),
    ok = update_network_receiver(Db, Conversations),
    if
        Negotiate ->
            ok = negotiate_with_peers(MyPeerId, Db, Conversations);
        true ->
            skip_negotation
    end,
    update_network_sender(Db, NetworkSenderPid, Conversations).

extract_conversations(_Db, _Busy = true) ->
    [];
extract_conversations(Db, _Busy = false) ->
    db_fold(
      fun(#gaia_peer{name = <<"*">>}, Acc) ->
              Acc;
         (#gaia_peer{nodis_address = undefined}, Acc) ->
              Acc;
         %% NOTE: This is very important for the peer port negotiation!
         (#gaia_peer{id = PeerId,
                     local_port = undefined,
                     remote_port = RemotePort}, Acc)
            when RemotePort /= undefined ->
              [{peer, PeerId}|Acc];
         (#gaia_peer{id = PeerId,
                     conversation = {true, read}}, Acc) ->
              [{peer, PeerId}|Acc];
         (#gaia_peer{id = PeerId,
                     conversation = {true, read_write}}, Acc) ->
              [{peer, PeerId}|Acc];
         (#gaia_group{id = GroupId,
                      conversation = true,
                      multicast_ip_address = MulticastIpAddress,
                      port = GroupPort}, Acc) ->
              [{group, GroupId, MulticastIpAddress, GroupPort}|Acc];
         (_, Acc) ->
              Acc
      end, [], Db).

update_network_receiver(Db, Conversations) ->
    %% Update network receiver
    LocalPorts =
        gaia_nif:update_conversations(
          lists:map(
            fun({group, GroupId, MulticastIpAddress, GroupPort})
                  when MulticastIpAddress /= undefined ->
                    {group, GroupId, inet:ntoa(MulticastIpAddress), GroupPort};
               (Conversation) ->
                    Conversation
            end, Conversations)),
    ?LOG_INFO(#{module => ?MODULE, local_ports => LocalPorts}),
    %% Update peer with new local port
    lists:foreach(
      fun({{peer, PeerId}, NewLocalPort}) ->
              case db_lookup_peer_by_id(Db, PeerId) of
                  [#gaia_peer{local_port = NewLocalPort}] ->
                      ok;
                  [Peer] ->
                      db_insert(Db, Peer#gaia_peer{local_port = NewLocalPort})
              end;
         (_) ->
              ok
      end, LocalPorts).

negotiate_with_peers(_MyPeerId, _Db, []) ->
    ok;
negotiate_with_peers(
  MyPeerId, Db, [{group, _GroupId, _MulticastIpAddress, _GroupPort}|Rest]) ->
    negotiate_with_peers(MyPeerId, Db, Rest);
negotiate_with_peers(MyPeerId, Db, [{peer, PeerId}|Rest]) ->
    [#gaia_peer{nodis_address = {IpAddress, _SyncPort},
                rest_port = RestPort,
                local_port = LocalPort} = Peer] =
        db_lookup_peer_by_id(Db, PeerId),
    case gaia_rest_client:start_peer_negotiation(
           MyPeerId, {IpAddress, RestPort}, LocalPort) of
        {ok, NewRemotePort} ->
            true = db_insert(Db, Peer#gaia_peer{remote_port = NewRemotePort}),
            negotiate_with_peers(MyPeerId, Db, Rest);
        {error, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE,
                         {rest_service_client, negotiate} => Reason}),
            %% FIXME: Implement gaia_command_serv:not_available/1
            %%ok = gaia_command_serv:not_available(PeerId),
            true = db_insert(Db, Peer#gaia_peer{conversation = false}),
            negotiate_with_peers(MyPeerId, Db, Rest)
    end.

update_network_sender(Db, NetworkSenderPid, Conversations) ->
    ConversationAddresses =
        lists:foldl(
          fun({peer, PeerId}, Acc) ->
                  [#gaia_peer{nodis_address = {IpAddress, _SyncPort},
                              remote_port = RemotePort}] =
                      db_lookup_peer_by_id(Db, PeerId),
                  if
                      RemotePort /= undefined ->
                          [{IpAddress, RemotePort}|Acc];
                      true ->
                          Acc
                  end;
             ({group, GroupId, undefined, GroupPort}, Acc) ->
                  case db_lookup_group_by_id(Db, GroupId) of
                      [#gaia_group{members = '*'}] ->
                          [#gaia_peer{options = GroupOptions}] =
                              db_lookup_peer_by_name(Db, <<"*">>),
                          db_fold(
                            fun(#gaia_peer{
                                   ephemeral = Ephemeral,
                                   nodis_address = {IpAddress, _SyncPort}},
                                MemberAddresses) ->
                                    if
                                        Ephemeral ->
                                            case lists:member(
                                                   known_peer_only,
                                                   GroupOptions) of
                                                true ->
                                                    MemberAddresses;
                                                false ->
                                                    [{IpAddress, GroupPort}|
                                                     MemberAddresses]
                                            end;
                                        true ->
                                            [{IpAddress, GroupPort}|
                                             MemberAddresses]
                                    end;
                               (_, MemberAddresses) ->
                                    MemberAddresses
                            end, [], Db) ++ Acc;
                      [#gaia_group{members = Members}] ->
                          lists:foldl(
                            fun(PeerId, MemberAddresses) ->
                                    [#gaia_peer{
                                        nodis_address =
                                            {IpAddress, _SyncPort}}] =
                                        db_lookup_peer_by_id(Db, PeerId),
                                    [{IpAddress, GroupPort}|MemberAddresses]
                            end, [], Members) ++ Acc
                  end;
             ({group, _GroupId, MulticastIpAddress, GroupPort}, Acc) ->
                  [{MulticastIpAddress, GroupPort}|Acc]
          end, [], Conversations),
    ?LOG_INFO(#{module => ?MODULE,
                conversation_addresses => ConversationAddresses}),
    gaia_network_sender_serv:set_conversation_addresses(
      NetworkSenderPid, lists:usort(ConversationAddresses)).

%%
%% Peer negotiation
%%

accept_peer(_Busy,
            #gaia_peer{conversation = {true, _ConversationStatus}} = Peer) ->
    {yes, Peer};
accept_peer(_Busy, #gaia_peer{mode = ignore}) ->
    {no, ignore};
accept_peer(_Busy, #gaia_peer{nodis_address = undefined}) ->
    {no, no_nodis_address};
accept_peer(_Busy = true, #gaia_peer{mode = ask,
                                     options = Options,
                                     conversation = false} = _Peer) ->
    case lists:member(override_busy, Options) of
        true ->
            %% FIXME: Implement gaia_command_serv:ask/1
            %%ok = gaia_command_serv:ask(Peer),
            {no, ask};
        false ->
            {no, busy}
    end;
accept_peer(_Busy = true, #gaia_peer{
                             mode = direct,
                             options = Options,
                             conversation = false} = Peer) ->
    case lists:member(override_busy, Options) of
        true ->
            {yes, Peer#gaia_peer{conversation = {true, read_write}}};
        false ->
            {no, busy}
    end;
accept_peer(_Busy = false, #gaia_peer{mode = ask}) ->
    %% FIXME: Implement gaia_command_serv:ask/1
    %%ok = gaia_command_serv:ask(Peer),
    {no, ask};
accept_peer(_Busy = false, #gaia_peer{mode = direct} = Peer) ->
    {yes, Peer#gaia_peer{conversation = {true, read_write}}};
accept_peer(_Busy, _Peer) ->
    {no, not_available}.

%%
%% Nodis handling
%%

prepare_node_info(MyPeerId, RestPort, Db) ->
    PublicGroups =
        db_fold(
          fun(#gaia_group{id = GroupId,
                          public = true,
                          admin = PeerId}, Acc)
                when PeerId == MyPeerId ->
                  [GroupId|Acc];
             (_, Acc) ->
                  Acc
          end, [], Db),
    #{gaia => #{peer_id => MyPeerId,
                rest_port => RestPort,
                public_groups => PublicGroups}}.

change_peer(MyPeerId, Db, {IpAddress, _SyncPort} = NewNodisAddress, Info) ->
    MaxPeerId = math:pow(2, 32) - 1,
    case lists:keysearch(gaia, 1, Info) of
        {value, {gaia, #{peer_id := NewPeerId,
                         rest_port := NewRestPort,
                         public_groups := PublicGroups},
                 _PreviousGaiaInfo}}
          when is_integer(NewPeerId) andalso
               NewPeerId > 0 andalso
               NewPeerId =< MaxPeerId andalso
               is_integer(NewRestPort) andalso
               NewRestPort >= 1024 andalso
               NewRestPort =< 65535 andalso
               is_list(PublicGroups) ->
            %% Get public groups of interest
            lists:foreach(
              fun(GroupId) when is_integer(GroupId) ->








                      %% FIXME: Check if we are interested in the group
                      case gaia_rest_client:get_group(
                             MyPeerId, {IpAddress, NewRestPort}, GroupId) of
                          {ok, Group} ->
                              ?LOG_INFO(#{module => ?MODULE,
                                          insert_group => Group}),
                              true = db_insert(Db, Group);
                          {error, Reason} ->
                              ?LOG_ERROR(
                                 #{module => ?MODULE,
                                   {gaia_rest_client, get_group} => Reason})
                      end;
                 (InvalidGroupId) ->
                      ?LOG_ERROR(#{module => ?MODULE,
                                   invalid_group_id => InvalidGroupId})
              end, PublicGroups),
            %% Update peer if needed or create ephemeral peer
            case db_lookup_peer_by_id(Db, NewPeerId) of
                [#gaia_peer{nodis_address = NodisAddress,
                            rest_port = RestPort} = Peer]
                  when NodisAddress /= NewNodisAddress andalso
                       RestPort /= NewRestPort ->
                    UpdatedPeer =
                        Peer#gaia_peer{nodis_address = NewNodisAddress,
                                       remote_port = undefined,
                                       rest_port = NewRestPort},
                    true = db_insert(Db, UpdatedPeer),
                    ok;
                [_] ->
                    ok;
                [] ->
                    EphemeralPeer =
                        #gaia_peer{
                           id = NewPeerId,
                           ephemeral = true,
                           nodis_address = NewNodisAddress,
                           rest_port = NewRestPort},
                    true = db_insert(Db, EphemeralPeer),
                    ok
                end;
        {value, {gaia, GaiaInfo, _PreviousGaiaInfo}} ->
            {error, {bad_gaia_info, GaiaInfo}};
        false ->
            {error, no_gaia_info}
    end.

down_peer(Db, NodisAddress) ->
    case db_lookup_peer_by_nodis_address(Db, NodisAddress) of
        [#gaia_peer{id = PeerId, ephemeral = true}] ->
            true = db_delete(Db, PeerId),
            ok;
        [Peer] ->
            true = db_insert(Db, Peer#gaia_peer{
                                   nodis_address = undefined,
                                   rest_port = undefined}),
            ok;
        [] ->
            {error, no_such_nodis_address}
    end.

%%
%% Peer and group database management
%%

new_db(GaiaDir) ->
    FilePath = filename:join([GaiaDir, ?MODULE]),
    #gaia_peer.id = #gaia_group.id, %% This must me true!
    {ok, DetsTab} =
        dets:open_file(?MODULE, [{file, ?b2l(FilePath)},
                                 {keypos, #gaia_peer.id}]),
    Tab = ets:new(?MODULE, [public, {keypos, #gaia_peer.id}]),
    Tab = dets:to_ets(DetsTab, Tab),
    Db = {Tab, DetsTab},
    ok = sync_with_config(Db, true),
    ?LOG_DEBUG(#{module => ?MODULE, db_created => ets:tab2list(Tab)}),
    Db.

sync_db({Tab, DetsTab} = Db) ->
    ok = sync_with_config(Db, false),
    DetsTab = ets:to_dets(Tab, DetsTab),
    ?LOG_DEBUG(#{module => ?MODULE,
                 db_synced_with_config => ets:tab2list(Tab)}),
    true.

sync_with_config(Db, _DeleteEphemeralPeers = true) ->
    db_delete_ephemeral_peers(Db),
    sync_with_config(Db, false);
sync_with_config(Db, _DeleteEphemeralPeers = false) ->
    %% Update existing peers and groups
    {NewConfigPeers, NewConfigGroups} =
        db_fold(
          fun(#gaia_peer{id = PeerId} = Peer, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigPeers) of
                      {value, ConfigPeer, RemainingConfigPeers} ->
                          [Mode, Options] =
                              config:lookup_children([mode, options],
                                                     ConfigPeer),
                          UpdatedPeer =
                              Peer#gaia_peer{mode = Mode,
                                             options = Options},
                          true = db_insert(Db, UpdatedPeer),
                          {RemainingConfigPeers, ConfigGroups};
                      false ->
                          true = db_delete(Db, PeerId),
                          {ConfigPeers, ConfigGroups}
                  end;
             (#gaia_group{id = GroupId} = Group, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigGroups) of
                      {value, ConfigGroup, RemainingConfigGroups} ->
                          [Public, MulticastIpAddress, GroupPort, Type,
                           Members, Admin] =
                              config:lookup_children(
                                [public, 'multicast-ip-address', port, type,
                                 members, admin], ConfigGroup),
                          UpdatedGroup =
                              Group#gaia_group{
                                public = Public,
                                multicast_ip_address = MulticastIpAddress,
                                port = GroupPort,
                                type = Type,
                                members = Members,
                                admin = Admin},
                          true = db_insert(Db, UpdatedGroup),
                          {ConfigPeers, RemainingConfigGroups};
                      false ->
                          true = db_delete(Db, GroupId),
                          {ConfigPeers, ConfigGroups}
                  end
          end, {config:lookup([gaia, peers]),
                config:lookup([gaia, groups])}, Db),
    %% Create new peers
    lists:foreach(
      fun(ConfigPeer) ->
              [PeerId, PeerName, Mode, Options] =
                  config:lookup_children([id, name, mode, options], ConfigPeer),
              Peer = #gaia_peer{
                        id = generate_id_if_needed(PeerId, PeerName),
                        name = PeerName,
                        mode = Mode,
                        options = Options},
              true = db_insert(Db, Peer)
      end, NewConfigPeers),
    %% Create new groups
    lists:foreach(
      fun(ConfigGroup) ->
              [GroupId, GroupName, Public, MulticastIpAddress, GroupPort, Type,
               Members, Admin] =
                  config:lookup_children(
                    [id, name, public, 'multicast-ip-address', port, type,
                     members, admin], ConfigGroup),
              Peer = #gaia_group{
                        id = generate_id_if_needed(GroupId, GroupName),
                        name = GroupName,
                        public = Public,
                        multicast_ip_address = MulticastIpAddress,
                        port = GroupPort,
                        type = Type,
                        members = Members,
                        admin = Admin},
              true = db_insert(Db, Peer)
      end, NewConfigGroups),
    %% Resolve peer ids
    db_fold(
      fun(#gaia_group{members = Members, admin = Admin} = Group, Acc) ->
              UpdatedGroup =
                  Group#gaia_group{
                    members = replace_with_peer_id(Db, Members),
                    admin = get_peer_id(Db, Admin)},
              true = db_insert(Db, UpdatedGroup),
              Acc;
         (_, Acc) ->
              Acc
      end, ok, Db).

generate_id_if_needed(0, Name) ->
    generate_artificial_id(Name);
generate_id_if_needed(Id, _Name) ->
    Id.

get_peer_id(Db, PeerName) ->
    case db_lookup_peer_by_name(Db, PeerName) of
        [#gaia_peer{id = PeerId}] ->
            PeerId;
        [] ->
            generate_artificial_id(config:lookup([gaia, 'peer-name']))
    end.


replace_with_peer_id(_Db, [<<"*">>]) ->
    '*';
replace_with_peer_id(_Db, []) ->
    [];
replace_with_peer_id(Db, [PeerName|Rest]) ->
    case db_lookup_peer_by_name(Db, PeerName) of
        [#gaia_peer{id = PeerId}] ->
            [PeerId|replace_with_peer_id(Db, Rest)];
        [] ->
            %% In this case it must be [gaia, 'peer-name'].
            %% This is verfied by mixmesh_config_serv:post_process/3
            PeerName = config:lookup([gaia, 'peer-name']),
            [generate_artificial_id(PeerName)|replace_with_peer_id(Db, Rest)]
    end.

db_insert({Tab, DetsTab}, PeerOrGroup) ->
    ok = dets:insert(DetsTab, PeerOrGroup),
    ets:insert(Tab, PeerOrGroup).

db_delete({Tab, DetsTab}, Id) ->
    ok = dets:delete(DetsTab, Id),
    ets:delete(Tab, Id).

db_lookup(Db, {name, Name}) ->
    db_lookup_by_name(Db, Name);
db_lookup(Db, Id) ->
    db_lookup_by_id(Db, Id).

db_lookup_by_id({Tab, _DetsTab}, Id) ->
    ets:lookup(Tab, Id).

db_lookup_by_name(Db, Name) ->
    case db_lookup_peer_by_name(Db, Name) of
        [] ->
            db_lookup_group_by_name(Db, Name);
        Peers ->
            Peers
    end.

db_lookup_peer(Db, {name, Name}) ->
    db_lookup_peer_by_name(Db, Name);
db_lookup_peer(Db, PeerId) ->
    db_lookup_peer_by_id(Db, PeerId).

db_lookup_group(Db, {name, Name}) ->
    db_lookup_group_by_name(Db, Name);
db_lookup_group(Db, GroupId) ->
    db_lookup_group_by_id(Db, GroupId).

db_lookup_peer_by_id({Tab, _DetsTab}, PeerId) ->
    ets:match_object(Tab, #gaia_peer{id = PeerId, _ = '_'}).

db_lookup_peer_by_name({Tab, _DetsTab}, Name) ->
    ets:match_object(Tab, #gaia_peer{name = Name, _ = '_'}).

db_lookup_group_by_id({Tab, _DetsTab}, PeerId) ->
    ets:match_object(Tab, #gaia_group{id = PeerId, _ = '_'}).

db_lookup_group_by_name({Tab, _DetsTab}, Name) ->
    ets:match_object(Tab, #gaia_group{name = Name, _ = '_'}).

db_lookup_peer_by_nodis_address({Tab, _DetsTab}, NodisAddress) ->
    ets:match_object(Tab, #gaia_peer{nodis_address = NodisAddress, _ = '_'}).

db_fold(Fun, Acc, {Tab, _DetsTab}) ->
    ets:foldl(Fun, Acc, Tab).

db_delete_ephemeral_peers({Tab, DetsTab}) ->
    ok = dets:match_delete(DetsTab, #gaia_peer{ephemeral = true, _ = '_'}),
    ets:match_delete(Tab, #gaia_peer{ephemeral = true, _ = '_'}).
