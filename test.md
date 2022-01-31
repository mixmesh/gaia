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




gaia_command_serv UI:



Talk to <name>
  Do you want to talk to <name>?
    Yes
      You now talk to <name>
      <name> is not available
      <name> has been asked to talk...
    No

Stop talking to <name> | all
  Do you want to stop talking to <name>?
    Yes
      You no longer talk to <name>
      You do not talk to <name>
    No


Start listen to <name> | all
Stop listen to <name> | all


I am busy

I am available

Mute
  You are now muted

Unmute
  You are no longer muted

<name> can talk directly [with me]
  <name> now can talk directly with you

<name> must ask [before calling]
  <name> now must ask before talking to you

Ignore <name>
  You now ignore <name>

Let <name> interrupt me
   <name> can now interrupt you

not intterupt


