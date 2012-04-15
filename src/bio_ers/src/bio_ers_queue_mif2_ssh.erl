-module(bio_ers_queue_mif2_ssh).
-behaviour(ssh_channel).
-export([start/0, start_link/0, check/1, stop/1, store_config/3]). % API
-export([init/1, terminate/2, handle_ssh_msg/2,handle_msg/2]). % Server side ssh_channel?
-export([handle_call/3, handle_cast/2, code_change/3]).        % Client side ssh_tunnel

-define(TIMEOUT, 10000).
-record(state, {cref, chan, cmd, wd, req}).

%%
%%  Some initial ideas were:
%%      ssh uosis.mif.vu.lt /users3/karolis/PST/bin/cluster-shell
%%      ????ssh_connection:exec(CR, CH, "/users3/karolis/PST/bin/cluster-login", 5000).
%%      ????ssh_connection_manager:request(CR, self(), CH, "/users3/karolis/PST/bin/cluster-login", false, <<>>, 0).
%%
%% karolis@uosis: .ssh/authorized_keys:
%%     command="/users3/karolis/PST/bin/cluster-shell",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ssh-rsa ... bio_ers_queue_mif2_ssh@karolis-home
%% karolis@home:
%%     ssh -T -i /home/karolis/GITWORK/kape1395.biosensor.solver-2D/src/bio_ers/etc/ssh/id_rsa uosis.mif.vu.lt
%%
%% See:
%%     http://binaries.erlang-solutions.com/R15A/lib/ssh-2.0.8./src/ssh_shell.erl
%%
%% Tests:
%%      rr(bio_ers).
%%      application:start(crypto), application:start(ssh).
%%      {ok, PID} = bio_ers_queue_mif2_ssh:start_link().
%%      ok = bio_ers_queue_mif2_ssh:check(PID).
%%
%%      Model = bio_ers_model:read_model("test/bio_ers_model_tests-CNT-2D.xml", kp1_xml).
%%      #model{definition = ModelDef} = Model.
%%      ModelId = bio_ers:get_id(Model).
%%      bio_ers_queue_mif2_ssh:store_config(PID, ModelId, ModelDef).
%%
%%      ok = bio_ers_queue_mif2_ssh:stop(PID).
%%

start() ->
    start_internal(fun ssh_channel:start/4).

start_link() ->
    start_internal(fun ssh_channel:start_link/4).

start_internal(StartFun) ->
    {ok, CRef} = ssh:connect("uosis.mif.vu.lt", 22, [
        {user_dir, "/home/karolis/GITWORK/kape1395.biosensor.solver-2D/src/bio_ers/etc/ssh"},
        {user, "karolis"},
        {silently_accept_hosts, true}
    ]),
    {ok, Chan} = ssh_connection:session_channel(CRef, ?TIMEOUT),
    StartFun(CRef, Chan, ?MODULE, #state{
        cref = CRef, chan = Chan,
        cmd = "/users3/karolis/PST/bin/cluster",
        wd = "/scratch/lustre/karolis/PST",
        req = []
    }).


check(Ref) ->
    ssh_channel:call(Ref, check).

stop(Ref) ->
    ssh_channel:cast(Ref, stop).

store_config(Ref, ConfigName, ConfigData) ->
    ssh_channel:call(Ref, {store_config, ConfigName, ConfigData}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%


%%
%%  Initialization.
%%
init(Args = #state{cref = CRef, chan = Chan}) ->
    ok = ssh_connection:shell(CRef, Chan),
    {ok, Args}.

%%
%%  Termination.
%%
terminate(Reason, #state{cref = CRef, chan = Chan}) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: destroy(reason=~p)~n", [Reason]),
    ssh_connection:close(CRef, Chan),
    ssh:close(CRef),
    ok.


%%
%%  Sync commands.
%%
handle_call(check = Cmd, From, State) ->
    #state{cref = CRef, chan = Chan} = State,
    CallRef = make_uid(),
    CmdLine = make_cmd(State, CallRef, "check", []),
    ssh_connection:send(CRef, Chan, CmdLine),
    {noreply, add_req(State, Cmd, CallRef, From), ?TIMEOUT};

handle_call({store_config = Cmd, ConfigName, ConfigData}, From, State) ->
    #state{cref = CRef, chan = Chan} = State,
    CallRef = make_uid(),
    CmdLine = make_cmd(State, CallRef, "store_config", [ConfigName]),
    ssh_connection:send(CRef, Chan, CmdLine),
    ssh_connection:send(CRef, Chan, bin_to_base64(ConfigData)),
    ssh_connection:send(CRef, Chan, ["#END_OF_FILE__store_config__", CallRef, "\n"]),
    {noreply, add_req(State, Cmd, CallRef, From), ?TIMEOUT};

handle_call(Msg, From, State) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_call(msg=~p, from=~p)~n", [Msg, From]),
    {reply, ok, State}.


%%
%%  Async commands.
%%
handle_cast(stop, State = #state{cref = CRef, chan = Chan}) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_cast(stop)~n"),
    ssh_connection:send_eof(CRef, Chan),
    {stop, normal, State};

handle_cast(Msg, State) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_cast(msg=~p)~n", [Msg]),
    {noreply, State}.

%%
%%  Messages comming from the SSH server.
%%
handle_ssh_msg({ssh_cm, _Ref, {data, _Chan, _Type, BinaryData}}, State) ->
    case BinaryData of
        <<"#CLUSTER:LGN(0000000000000000000000000000000000000000)==>", Msg/binary>> ->
            error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_ssh_msg(LGN): msg=~p~n", [Msg]),
            {ok, State};
        <<"#CLUSTER:OUT(", CallRefBin:40/binary, ")==>", Msg/binary>> ->
            CallRef = binary:bin_to_list(CallRefBin),
            {Cmd, From} = get_req(State, CallRef),
            handle_ssh_msg_call(Cmd, From, Msg),
            {ok, rem_req(State, CallRef)};
        <<"#CLUSTER:ERR(", CallRefBin:40/binary, ")==>", ErrCode:3/binary, ":", ErrMsg/binary>> ->
            error_logger:error_msg("bio_ers_queue_mif2_ssh: handle_ssh_msg(ERR): ref=~p, code=~p, msg=~p~n", [CallRefBin, ErrCode, ErrMsg]),
            CallRef = binary:bin_to_list(CallRefBin),
            {_, From} = get_req(State, CallRef),
            ssh_channel:reply(From, error),
            {ok, rem_req(State, CallRef)};
        <<"#", Msg/binary>> ->
            error_logger:error_msg("bio_ers_queue_mif2_ssh: handle_ssh_msg(#??): ~p~n", [Msg]),
            {ok, State};
        _ ->
            error_logger:error_msg("bio_ers_queue_mif2_ssh: handle_ssh_msg(???): ~p~n", [BinaryData]),
            {ok, State}
    end;

handle_ssh_msg(Msg, State) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_ssh_msg(msg=~p)~n", [Msg]),
    {ok, State}.


handle_ssh_msg_call(check, From, <<"OK\n">>) ->
    ssh_channel:reply(From, ok);

handle_ssh_msg_call(store_config, From, Message) ->
    case Message of
        <<"STORED\n">>   -> ssh_channel:reply(From, {ok, stored});
        <<"EXISTING\n">> -> ssh_channel:reply(From, {ok, existing})
    end.


%%
%%  Other messages.
%%
handle_msg({ssh_channel_up, _Chan, _CRef}, State) ->
    {ok, State};
handle_msg(timeout, State = #state{chan = Chan}) ->
    {stop, Chan, State};
handle_msg(Msg, State) ->
    error_logger:info_msg("bio_ers_queue_mif2_ssh: handle_msg(msg=~p)~n", [Msg]),
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_uid() ->
    bio_ers:get_id(unique).

make_cmd(#state{cmd = Cmd, wd = WD}, Ref, Command, Args) ->
    [Cmd, " ", Ref, " ", WD, " ", Command, " ", [ [" \"", A, "\"" ] || A <- Args ], "\n"].

%%
%%  Functions for manipulating with the request queue.
%%
add_req(State = #state{req = Req}, Cmd, Ref, From) ->
    State#state{req = [{Cmd, Ref, From} | Req]}.

get_req(#state{req = Req}, CallRef) ->
    [{Cmd, _, From}] = lists:filter(fun ({_, CR, _}) when CR == CallRef -> true; (_) -> false end, Req),
    {Cmd, From}.

rem_req(State = #state{req = Req}, CallRef) ->
    State#state{req = lists:filter(fun ({_, CR, _}) when CR == CallRef -> false; (_) -> true end, Req)}.

%%
%%  Function for converting binary to the wrapped base64.
%%
bin_to_base64(Binary) when is_binary(Binary) ->
    Base64 = base64:encode(Binary),
    lists:reverse(bin_to_base64_wrap(Base64, [])).

bin_to_base64_wrap(<<Line:76/binary, Tail/binary>>, Lines) ->
    bin_to_base64_wrap(Tail, [ <<Line/binary, "\n">> | Lines ]);

bin_to_base64_wrap(<<LastLine/binary>>, Lines) ->
    [ <<LastLine/binary, "\n">> | Lines ].

