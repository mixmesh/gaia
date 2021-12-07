%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 25 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(gaia_command).
-export([start/0, start/1]).
-compile(export_all).

-define(DEFAULT_FORMAT, s16_le).
-define(DEFAULT_SAMPLE_RATE, 48000).
-define(DEFAULT_CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 4800).  %% 100 ms
-define(BUFFER_PERIODS, 2).
-define(DEFAULT_DEVICE, "plughw:1,0").

-define(MODEL, "vosk-model-small-en-us-0.15").

start() ->
    start([]).
start(Params) ->
    VoskPid = start_vosk(Params),
    receive 
	{vosk_start, VoskPid, Params1} ->
	    command_query(VoskPid, Params1)
    end.


command_query(VoskPid, Params) ->
    receive
	{vosk, VoskPid, _VoskData} ->	
	    command_query(VoskPid, Params)
    after 0 ->
	    command_query_(VoskPid, Params)
    end.

command_query_(VoskPid, Params1) ->
    receive
	{vosk, VoskPid, VoskData} ->
	    io:format("VoskData: ~p\n", [VoskData]),
	    case maps:get("text", VoskData, "") of
		"" ->
		    command_query(VoskPid, Params1);
		Text ->
		    case action(string:tokens(Text, " \t\n\r")) of
			{'?', [], _} ->
			    command_query(VoskPid, Params1);
			{'?', [Word|_Cs], _} ->
			    Response = "I do not understand " ++ join([Word]),
			    flite:say(Response, Params1),
			    command_query(VoskPid, Params1);
			{'say', Cs, _} ->
			    Response = join(Cs),
			    flite:say(Response, Params1),
			    command_query(VoskPid, Params1);
			Cmd = {Command, Args, _More} ->
			    io:format("~p\n", [Cmd]),
			    Query = "Do you really want to " ++ 
				join([Command | Args]) ++ "?",
			    flite:say(Query, Params1),
			    command_ack(VoskPid, Cmd, Params1)
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


command_ack_(VoskPid, Cmd={Command,Args,_}, Params1) ->
    receive
	{vosk, VoskPid, Response} ->
	    io:format("Response: ~p\n", [Response]),
	    case string:tokens(maps:get("text", Response, ""), "\s\t") of
		"" -> %% wait
		    command_ack(VoskPid, Cmd, Params1);
		["yes"|_] ->
		    Ack = join(["execute",Command | Args]),
		    flite:say(Ack, Params1),
		    if Command =:= exit ->
			    VoskPid ! stop,
			    ok;
		       true ->
			    command_query(VoskPid, Params1)
		    end;
		["no"|_] ->
		    Ack = join(["aborting",Command | Args]),
		    flite:say(Ack, Params1),
		    command_query(VoskPid, Params1);
		_ ->
		    Ack = "please respond with yes or no",
		    flite:say(Ack, Params1),
		    command_ack(VoskPid, Cmd, Params1)
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

start_vosk(Params0) ->
    Parent = self(),
    spawn(
      fun() ->
	      Rate0 = 16000,
	      Params1 = [{device,"default"},
			 {rate,Rate0},
			 {format,s16_le},
			 {channels,1}],
	      Params = Params0 ++ Params1,
	      {ok, {AlsaHandle,PeriodFrames,Header}} = alsa_open(Params),
	      Channels = proplists:get_value(channels, Header),
	      Format   = proplists:get_value(format, Header),
	      SampleRate = proplists:get_value(rate, Header),

	      Model = vosk:model_new(filename:join(code:priv_dir(vosk),?MODEL)),
	      Vosk = vosk:recognizer_new(Model, SampleRate),

	      Transform =
		  if Channels =:= 2 ->
			  fun (X) -> stereo_to_mono(Format, X) end;
		     true ->
			  fun (X) -> X end
		  end,
	      Parent ! {vosk_start, self(), Params},
	      alsa_loop(Parent, AlsaHandle, PeriodFrames, Transform,
			Vosk, Model)
      end).

alsa_open(Params) ->
    PeriodSizeInFrames = 
	proplists:get_value(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    NumBufferPeriods =
	proplists:get_value(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * NumBufferPeriods,
    Format = proplists:get_value(format, Params, ?DEFAULT_FORMAT),
    Channels = proplists:get_value(channels, Params, ?DEFAULT_CHANNELS),
    SampleRate = proplists:get_value(rate, Params, ?DEFAULT_SAMPLE_RATE),
    Device = proplists:get_value(device, Params, ?DEFAULT_DEVICE),
    WantedHwParams =
	[{format,Format},
	 {channels, Channels},
	 {rate,SampleRate},
	 {period_size,PeriodSizeInFrames},
	 {buffer_size,BufferSizeInFrames}],
    io:format("gaia_command: alsa_open device=~s, wanted_hw_params = ~p\n",
	      [Device, WantedHwParams]),
    WantedSwParams = [],
    %% WantedSwParams = [{avail_min, 1}],
    %% [{start_threshold,PeriodSizeInFrames}],
    case alsa:open(Device, capture, WantedHwParams, WantedSwParams) of
	{ok, Handle, ActualHwParams, ActualSwParams} ->
	    io:format("gaia_command: alsa_open actual_hw_params = ~p\n",
		      [ActualHwParams]),
	    io:format("gaia_command: alsa_open actual_sw_params = ~p\n",
		      [ActualSwParams]),
	    Format1 = proplists:get_value(format, ActualHwParams),
	    Channels1 = proplists:get_value(channels, ActualHwParams),
	    SampleRate1 = proplists:get_value(rate, ActualHwParams),
	    PeriodSizeInFrames1 = proplists:get_value(period_size, 
						      ActualHwParams),
	    Header = [{format,Format1},
		      {channels,Channels1},
		      {rate,SampleRate1}],
	    {ok,{Handle,PeriodSizeInFrames1, Header}};
	Error ->
	    Error
    end.


alsa_loop(Parent, AlsaHandle, PeriodFrames,  Transform, Vosk, Model) ->
    case alsa:read(AlsaHandle, PeriodFrames) of
	{ok, Data} ->
	    Data1 =  Transform(Data),
	    case vosk:recognizer_accept_waveform(Vosk, Data1) of
		0 ->
		    alsa_loop(Parent, AlsaHandle, PeriodFrames, Transform,
			      Vosk, Model);
		1 ->
		    Result = vosk:recognizer_result(Vosk),
		    Parent ! {vosk,self(),Result},
		    vosk:recognizer_reset(Vosk),
		    alsa_loop(Parent, AlsaHandle, PeriodFrames,  Transform,
			      Vosk, Model);
		-1 ->
		    vosk:recognizer_reset(Vosk),
		    alsa_loop(Parent, AlsaHandle, PeriodFrames,  Transform,
			      Vosk, Model)
	    end;
	stop ->
	    alsa:close(AlsaHandle),
	    ok;
	Got ->
	    io:format("alsa_loop: got ~p\n", [Got]),
	    alsa_loop(Parent, AlsaHandle, PeriodFrames,  Transform,
		      Vosk, Model)
    end.

%% Add more stuff? or use mixer device?
mono_to_stereo(s16_le, Bin,Pan) ->
    << <<(trunc(X*(1.0-Pan))):16/signed-little, 
	 (trunc(X*Pan)):16/signed-little>> ||
	<<X:16/signed-little>> <= Bin >>.

stereo_to_mono(s16_le, Bin) ->
    << <<(max(X1,X2)):16/little-signed>> || 
	<<X1:16/little-signed,X2:16/little-signed>> <= Bin >>.
