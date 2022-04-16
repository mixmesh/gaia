%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 25 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(gaia_command).
-export([start/0, start/1]).
-compile(export_all).

-define(MODEL, "vosk-model-small-en-us-0.15").

start() ->
    start(#{}).
start(Params) ->
    VoskPid = start_vosk(Params),
    receive 
	{vosk_start, VoskPid, VoskParams} ->
	    command_query(VoskPid, VoskParams)
    end.


command_query(VoskPid, Params) ->
    receive
	{vosk, VoskPid, _VoskData} ->	
	    command_query(VoskPid, Params)
    after 0 ->
	    command_query_(VoskPid, Params)
    end.

command_query_(VoskPid, Params) ->
    receive
	{vosk, VoskPid, VoskData} ->
	    io:format("VoskData: ~p\n", [VoskData]),
	    case maps:get("text", VoskData, "") of
		"" ->
		    command_query(VoskPid, Params);
		Text ->
		    case action(string:tokens(Text, " \t\n\r")) of
			{'?', [], _} ->
			    command_query(VoskPid, Params);
			{'?', [Word|_Cs], _} ->
			    Response = "I do not understand " ++ join([Word]),
			    flite:say(Response, Params),
			    command_query(VoskPid, Params);
			{'say', Cs, _} ->
			    Response = join(Cs),
			    flite:say(Response, Params),
			    command_query(VoskPid, Params);
			Cmd = {Command, Args, _More} ->
			    io:format("~p\n", [Cmd]),
			    Query = "Do you really want to " ++ 
				join([Command | Args]) ++ "?",
			    flite:say(Query, Params),
			    command_ack(VoskPid, Cmd, Params)
		    end
	    end
    end.

%% flush vosk data and the start process
command_ack(VoskPid, Cmd, Params) ->
    receive
	{vosk, VoskPid, _VoskData} ->	
	    command_ack(VoskPid, Cmd, Params)
    after 0 ->
	    command_ack_(VoskPid, Cmd, Params)
    end.


command_ack_(VoskPid, Cmd={Command,Args,_}, Params) ->
    receive
	{vosk, VoskPid, Response} ->
	    io:format("Response: ~p\n", [Response]),
	    case string:tokens(maps:get("text", Response, ""), "\s\t") of
		"" -> %% wait
		    command_ack(VoskPid, Cmd, Params);
		["yes"|_] ->
		    Ack = join(["execute",Command | Args]),
		    flite:say(Ack, Params),
		    if Command =:= exit ->
			    VoskPid ! stop,
			    ok;
		       true ->
			    command_query(VoskPid, Params)
		    end;
		["no"|_] ->
		    Ack = join(["aborting",Command | Args]),
		    flite:say(Ack, Params),
		    command_query(VoskPid, Params);
		_ ->
		    Ack = "please respond with yes or no",
		    flite:say(Ack, Params),
		    command_ack(VoskPid, Cmd, Params)
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
	["huh"|Cs]       -> action(Cs);
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
	["stop"|Cs]       -> {exit, [], Cs};
	["say"|Cs]        -> {say, Cs, []};
	["hello"]         -> {say, ["hi"], []};
	["hi"]            -> {say, ["hello"], []};
	["how","are","you"] -> {say,["Fine", "Thank", "You"],[]};
	["call"|Cs]       -> {call, Cs, []};
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
	["phone"|Cs] -> jobj(Command,Form,["phone"],Cs);
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
	Cs -> {'?',[Command], Cs}
    end.

jobj(Command,Form,Obj,Cs) ->
    {Command,Form++Obj,Cs}.

start_vosk(Options) ->
    Parent = self(),
    spawn(
      fun() ->
	      {ok, AlsaHandle, Params} = alsa_capture:open(Options),
	      #{ device := Device,
		 channels := Channels,
 		 format := Format,
		 rate := Rate, 
		 buffer_size := BufferSize
	       } = Params,
	      Model = vosk:model_new(filename:join(code:priv_dir(vosk),?MODEL)),
	      Vosk = vosk:recognizer_new(Model, Rate),
	      Transform =
		  if Channels =:= 2 ->
			  fun (X) -> alsa_util:stereo_to_mono(Format, X) end;
		     true ->
			  fun (X) -> X end
		  end,
	      Parent ! {vosk_start, self(), [{device, Device }]},
	      alsa_loop(Parent, AlsaHandle, BufferSize, Transform,
			Vosk, Model)
      end).


alsa_loop(Parent, AlsaHandle, NumFrames,  Transform, Vosk, Model) ->
    case alsa:read_(AlsaHandle, NumFrames) of
	{ok, {_ReadFrames, Data}} ->
	    Data1 =  Transform(Data),
	    case vosk:recognizer_accept_waveform(Vosk, Data1) of
		0 ->
		    alsa_util:async_wait_ready(AlsaHandle),
		    alsa_loop(Parent,AlsaHandle,NumFrames,Transform,
			      Vosk, Model);
		1 ->
		    Result = vosk:recognizer_result(Vosk),
		    Parent ! {vosk,self(),Result},
		    vosk:recognizer_reset(Vosk),
		    alsa_loop(Parent,AlsaHandle,NumFrames,Transform,
			      Vosk, Model);
		-1 ->
		    vosk:recognizer_reset(Vosk),
		    alsa_loop(Parent,AlsaHandle,NumFrames,Transform,Vosk,Model)
	    end;
	{error, eagain} -> %% reading too fast
	    alsa_util:async_wait_ready(AlsaHandle),
	    alsa_loop(Parent,AlsaHandle,NumFrames,Transform,Vosk,Model);
	stop ->
	    alsa:close(AlsaHandle),
	    ok;
	Got ->
	    io:format("alsa_loop: got ~p\n", [Got]),
	    alsa_loop(Parent,AlsaHandle,NumFrames,Transform,Vosk,Model)
    end.
