%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 25 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(gaia_command).
-compile(export_all).

-define(MODEL, "vosk-model-small-en-us-0.15").

start() ->
    VoskPid = start_vosk(),
    command_query(VoskPid).

command_query(VoskPid) ->
    receive
	{vosk, VoskPid, VoskData} ->
	    io:format("VoskData: ~p\n", [VoskData]),
	    case maps:get("text", VoskData, "") of
		"" ->
		    command_query(VoskPid);
		Text ->
		    case action(string:tokens(Text, " \t\n\r")) of
			{'?', [], _} ->
			    command_query(VoskPid);
			{'?', [Word|_Cs], _} ->
			    Response = "I do not understand " ++ join([Word]),
			    flite:aplay(Response),
			    command_query(VoskPid);
			{'say', Cs, _} ->
			    Response = join(Cs),
			    flite:aplay(Response),
			    command_query(VoskPid);
			Cmd = {Command, Args, _More} ->
			    io:format("~p\n", [Cmd]),
			    Query = "Do you really want to " ++ 
				join([Command | Args]) ++ "?",
			    flite:aplay(Query),
			    command_ack(VoskPid, Cmd)
		    end
	    end
    end.

command_ack(VoskPid, Cmd={Command,Args,_}) ->
    receive
	{vosk, VoskPid, Response} ->
	    io:format("Response: ~p\n", [Response]),
	    case string:tokens(maps:get("text", Response, ""), "\s\t") of
		"" -> %% wait
		    command_ack(VoskPid, Cmd);
		["yes"|_] ->
		    Ack = join(["execute",Command | Args]),
		    flite:aplay(Ack),
		    if Command =:= exit ->
			    VoskPid ! stop,
			    ok;
		       true ->
			    command_query(VoskPid)
		    end;
		["no"|_] ->
		    Ack = join(["aborting",Command | Args]),
		    flite:aplay(Ack),
		    command_query(VoskPid);
		_ ->
		    Ack = "please respond with yes or no",
		    flite:aplay(Ack),
		    command_ack(VoskPid, Cmd)
	    end
    end.

join(Cs) ->
    string:join(
      [ if is_atom(C) -> atom_to_list(C);
	   is_integer(C) -> integer_to_list(C);
	   true -> C
	end || C <- Cs], " ").
	    
%%
%% <action> <object> [and <object>]
%% <action> <object> and <action> <object>
%%
action(["please"|Cs]) -> %% polite is ok
    action_(Cs);  %% polite is ok
action(Cs) ->
    action_(Cs).
action_(Cs0) ->
    case Cs0 of
	["open"|Cs]       -> obj(Cs,open);
	["close"|Cs]      -> obj(Cs,close);
	["turn","on"|Cs]  -> obj(Cs,'turn-on');
	["turn","off"|Cs] -> obj(Cs,'turn-off');
	["enable"|Cs]     -> obj(Cs,enable);
	["disable"|Cs]    -> obj(Cs,disable);
	["send"|Cs]       -> obj(Cs,send);
	["format"|Cs]     -> obj(Cs,format);
	["quit"|Cs]       -> {exit, [], Cs};
	["bye"|Cs]        -> {exit, [], Cs};
	["exit"|Cs]       -> {exit, [], Cs};
	["say"|Cs]        -> {say, Cs, []};
	_ -> {'?', Cs0, []}
    end.

obj(["my","mothers"|Cs], Command) -> obj_(Cs, Command, [my,mothers]);
obj(["my","fathers"|Cs], Command) -> obj_(Cs, Command, [my,fathers]);
obj(["my","brothers"|Cs], Command) -> obj_(Cs, Command, [my,brothers]);
obj(["my","sisters"|Cs], Command) -> obj_(Cs, Command, [my,sisters]);
obj(["the"|Cs], Command) ->   obj_(Cs, Command, [the]);
obj(["my"|Cs], Command) ->    obj_(Cs, Command, [my]);
obj(["his"|Cs], Command) ->   obj_(Cs, Command, [his]);
obj(["her"|Cs], Command) ->   obj_(Cs, Command, [her]);
obj(["dads"|Cs], Command) ->   obj_(Cs, Command, [dads]);
obj(["daddies"|Cs], Command) ->   obj_(Cs, Command, [daddies]);
obj(["moms"|Cs], Command) ->   obj_(Cs, Command, [moms]);
obj(["mommy"|Cs], Command) ->   obj_(Cs, Command, [mommy]);
obj(Cs, Command) ->
    obj_(Cs, Command, []).
obj_(Cs0, Command, Form) ->
    case Cs0 of
	["email"|Cs] -> jobj(Command,Form,['email'],Cs);
	["front","door"|Cs] -> jobj(Command,Form,['front-door'],Cs);
	["back","door"|Cs] -> jobj(Command,Form,['back-door'],Cs);
	["window"|Cs] -> jobj(Command,Form,['window'],Cs);
	["door"|Cs] -> jobj(Command,Form,["door"],Cs);
	["car","door"|Cs] -> jobj(Command,Form,["car-door"],Cs);
	["gate"|Cs] -> jobj(Command,Form,["gate"],Cs);
	["lamp"|Cs] -> jobj(Command,Form,["lamp"],Cs);
	["light"|Cs] -> jobj(Command,Form,["light"],Cs);
	["heat"|Cs] -> jobj(Command,Form,["heat"],Cs);
	["air", "conditioner"|Cs] -> 
	    jobj(Command,Form,['air-conditioner'],Cs);
	["air", "conditioning"|Cs] ->
	    jobj(Command,Form,['air-conditioning'],Cs);
	["radiator"|Cs] -> jobj(Command,Form,["radiator"],Cs);
	["internet"|Cs] -> jobj(Command,Form,["internet"],Cs);
	["hard", "drive" | Cs] -> jobj(Command,Form,["hard-drive"],Cs);
	["floppy", "drive" | Cs] -> jobj(Command,Form,["floppy-drive"], Cs);
	["C","colon" | Cs] -> jobj(Command,Form,["C:"],Cs);
	["D","colon" | Cs] -> jobj(Command,Form,["D:"],Cs);
	Cs -> {'?',[Command|Cs]}
    end.

jobj(Command,Form,Obj,Cs) ->
    {Command,Form++Obj,Cs}.

start_vosk() ->
    Parent = self(),
    spawn(
      fun() ->
	      SampleRate = 16000,
	      Model = vosk:model_new(filename:join(code:priv_dir(vosk),?MODEL)),
	      Vosk = vosk:recognizer_new(Model, SampleRate),
	      AlsaPort = alsa_open("hw", SampleRate, "S16_LE", 1),
	      alsa_loop(Parent, AlsaPort, Vosk, Model)
      end).

alsa_open(Device, SampleRate, Format, NumChannels) ->
    Command = 
	lists:flatten(
	  ["arecord ",
	   "--quiet", $\s,
	   %% "--device=", Device, $\s,
	   "--format=", Format, $\s,
	   "--rate=",integer_to_list(SampleRate),$\s,
	   "-c ",integer_to_list(NumChannels),$\s
	   %% "--channels=",integer_to_list(NumChannels),$\s
	  ]),
    io:format("run command: ~s\n", [Command]),
    erlang:open_port({spawn, Command}, [eof, binary]).

alsa_loop(Parent,AlsaPort, Vosk, Model) ->
    receive
	{AlsaPort, {data, Data}} ->
	    %% io:format("data: size=~w\n", [byte_size(Data)]),
	    case vosk:recognizer_accept_waveform(Vosk, Data) of
		0 ->
		    alsa_loop(Parent, AlsaPort, Vosk, Model);
		1 ->
		    Result = vosk:recognizer_result(Vosk),
		    Parent ! {vosk,self(),Result},
		    alsa_loop(Parent, AlsaPort, Vosk, Model);
		-1 ->
		    vosk:recognizer_reset(Vosk),
		    alsa_loop(Parent, AlsaPort, Vosk, Model)
	    end;
	stop ->
	    erlang:port_close(AlsaPort),
	    ok;
	Got ->
	    io:format("alsa_loop: got ~p\n", [Got]),
	    alsa_loop(Parent, AlsaPort, Vosk, Model)
    end.