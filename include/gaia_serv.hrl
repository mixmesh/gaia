-ifndef(GAIA_SERV_HRL).
-define(GAIA_SERV_HRL, true).

-record(gaia_peer,
        {
         id :: gaia_serv:peer_id() | '_',
         name = undefined :: gaia_serv:peer_name() | undefined | '_',
         mode = undefined :: gaia_serv:mode() | undefined | '_',
         options = [] :: gaia_serv:options() | '_',
         ephemeral = false :: boolean() | '_',
         conversation = false ::
           false | {true, gaia_serv:conversation_status()} | '_',
         nodis_address = undefined ::
           {inet:ip_address(), inet:port_number()} | undefined | '_',
         rest_port = undefined :: inet:port_number() | undefined | '_',
         local_port = undefined :: inet:port_number() | undefined | '_',
         remote_port = undefined :: inet:port_number() | undefined | '_',
         session_key = undefined :: gaia_serv:session_key() | undefined | '_'
        }).

-record(gaia_group,
        {
         id :: gaia_serv:group_id() | '_',
         name :: gaia_serv:group_name() | '_',
         public :: boolean() | '_',
         multicast_ip_address :: inet:ip_address() | undefined | '_',
         port :: inet:port_number() | '_',
         type :: gaia_serv:group_type() | '_',
         members :: [{gaia_serv:peer_id(), gaia_serv:peer_name()}] |
                    wildcard |
                    '_',
         admin :: gaia_serv:peer_id() | '_',
         conversation = false :: boolean() | '_',
         session_key = undefined :: gaia_serv:session_key() | undefined | '_'
        }).

-endif.
