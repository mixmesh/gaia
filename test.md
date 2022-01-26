* m1.conf

gaia_serv:start_talking_to({name, <<"johanna">>}).
gaia_serv:stop_talking_to({name, <<"johanna">>}).
gaia_serv:start_talking_to({name, <<"johanna">>}).

gaia_serv:mute().
gaia_serv:unmute().

gaia_serv:set_status(busy).
gaia_serv:set_status(available).
.
gaia_serv:stop_talking_to({name, <<"johanna">>}).
mode ask
gaia_serv:start_talking_to({name, <<"johanna">>}).

gaia_serv:stop_talking_to({name, <<"johanna">>}).
mode ignore
gaia_serv:start_talking_to({name, <<"johanna">>})

* m.conf (dvs med wildcard)

gaia_serv:start_talking_to({name, <<"*">>}).
test samma som ovan
test med johanna1 också (ephemeral)

* m.conf

testa med tre

* m.conf

testa en grupp
