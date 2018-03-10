-module(miner_gpu)
.
-export([start/0, unpack_mining_data/1]).
%-define(Peer, "http://localhost:8081/").%for a full node on same computer.
%-define(Peer, "http://localhost:8085/").%for a mining pool on the same computer.
-define(Peer, "http://amoveopool.com/work/").%for a mining pool on an external server
-define(Pubkey, <<"BIGGeST9w6M//7Bo8iLnqFSrLLnkDXHj9WFFc+kwxeWm2FBBi0NDS0ERROgBiNQqv47wkh0iABPN1/2ECooCTOM=">>).
-define(timeout, 600).%how long to wait in seconds before checking if new mining data is available.
-define(pool_sleep_period, 1000).%How long to wait in miliseconds if we cannot connect to the mining pool.
%This should probably be around 1/20th of the blocktime.

unpack_mining_data(R) ->
    case string:equal(?Peer,"http://amoveopool.com/work/") of
	true->
	    <<_:(8*11), R2/binary>> = list_to_binary(R),
	    {First,R3} = slice(R2,hd("\"")),
	    <<_:8, R4/binary>> = R3,
	    {BlockDiff,R5} = slice(R4,hd(",")),
	    {ShareDiff,_} = slice(R5,hd("]")),
	    BlockHash = base64:decode(First);
	false->
	    <<_:(8*11), R2/binary>> = list_to_binary(R),
	    {First, R3} = slice(R2, hd("\"")),
	    <<_:(8*2), R4/binary>> = R3,
	    {_, R5} = slice(R4, hd("\"")),
	    <<_:8, R6/binary>> = R5,
	    {BlockDiff, _} = slice(R6, hd("]")),
	    BlockHash = base64:decode(First),
	    ShareDiff = BlockDiff
    end,
    {BlockHash,BlockDiff,ShareDiff}.

connectionInfo() ->
    flush(),
    case string:equal(?Peer,"http://amoveopool.com/work/") of
	true -> 
	    Data = "[\"mining_data\",\""++binary_to_list(?Pubkey)++"\"]",
	    Server = "http://AmoveoPool.com/work"; 
	false -> 
	    Data = <<"[\"mining_data\"]">>,
	    Server = ?Peer
    end,
    {Data,Server}.

start() ->
    io:fwrite("Started mining, "),
    io:fwrite("see debug"++os:getenv("CUDA_VISIBLE_DEVICES")++".txt for more info.\n"),
    io:fwrite("Your Pubkey is "++binary_to_list(?Pubkey)++"\n"),
    io:fwrite("You are connecting to "++?Peer++"\n"),
    miner().

miner() ->
    {Data,Server} = connectionInfo(),
    R = talk_helper(Data,Server,1000),
    if
	is_list(R) ->
	    {BlockHash,BlockDiff,WorkDiff} = unpack_mining_data(R),
	    start_gpu_miner(BlockHash,BlockDiff,WorkDiff);
	is_atom(R) ->
	    timer:sleep(1000),
	    start()
    end.
    
read_nonce(0) -> 0;
read_nonce(N) ->
    case file:read_file("./mining_data/"++"nonce"++os:getenv("CUDA_VISIBLE_DEVICES")) of
	{ok, <<Nonce:256>>} -> Nonce;
	{ok, <<>>} -> 
	    io:fwrite("nonce failed "),
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n"),
	    timer:sleep(100),
	    read_nonce(N-1)
    end.

start_gpu_miner(BlockHash,BlockDiff,WorkDiff) ->
    NonceRand = crypto:strong_rand_bytes(32),
    GPUID = os:getenv("CUDA_VISIBLE_DEVICES"),
    ok = file:write_file("./mining_data/"++"nonce"++GPUID, <<"">>),
    DiffsDelimiter = <<"|">>,
    BlockDiffInt = list_to_integer(binary_to_list(BlockDiff)),
    WorkDiffInt = list_to_integer(binary_to_list(WorkDiff)),
    file:write_file("./mining_data/mining_input"++GPUID, <<BlockHash/binary, NonceRand/binary, BlockDiff/binary,DiffsDelimiter/binary,WorkDiff/binary>>),
    Port = open_port({spawn, "./amoveo_gpu_miner "++GPUID},[exit_status]),
    receive 
	{Port, {exit_status,1}}->
	    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
	    Nonce = read_nonce(1),
	    io:fwrite(StrTime++" - Found block "),
	    io:fwrite(base64:encode(<<BlockHash/binary,BlockDiffInt:16,Nonce:256>>)),
	    io:fwrite(" for difficulty "++integer_to_list(WorkDiffInt)++"."),
            BinNonce = base64:encode(<<Nonce:256>>),
            Data = << <<"[\"work\",\"">>/binary, BinNonce/binary, <<"\",\"">>/binary, ?Pubkey/binary, <<"\"]">>/binary>>,
            Response = talk_helper(Data, ?Peer, 5),
	    io:fwrite(" Server reply:"),
	    io:fwrite(Response),
	    io:fwrite("\n"),
            timer:sleep(100);
	{Port, {exit_status,0}}->
            ok		
    end,
    miner().

talk_helper2(Data, Peer) ->
    httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(Data)}, [{timeout, 3000}], []).
talk_helper(_Data, _Peer, 0) -> talk_helper(_Data,_Peer,1);
talk_helper(Data, Peer, N) ->
    case talk_helper2(Data, Peer) of
        {ok, {_Status, _Headers, []}} ->
            io:fwrite("server gave confusing response\n"),
            timer:sleep(?pool_sleep_period),
            talk_helper(Data, Peer, N-1);
        {ok, {_, _, R}} -> R;
        %{error, _} ->
        E -> 
            io:fwrite("No Server Reply.\n"),
	    timer:sleep(?pool_sleep_period),
	    talk_helper(Data, Peer, N-1)
    end.
slice(Bin, Char) ->
    slice(Bin, Char, 0).
slice(Bin, Char, N) ->
    NN = N*8,
    <<First:NN, Char2:8, Second/binary>> = Bin,
    if
        N > size(Bin) -> 1=2;
        (Char == Char2) ->
            {<<First:NN>>, Second};
        true ->
            slice(Bin, Char, N+1)
    end.
flush() ->
    receive
        _ ->
            flush()
    after
        0 ->
            ok
    end.
