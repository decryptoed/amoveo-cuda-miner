-module(miner_gpu).

-export([start/0, unpack_mining_data/2]).
-define(timeout, 600).%how long to wait in seconds before checking if new mining data is available.
-define(pool_sleep_period, 10000).%How long to wait in miliseconds if we cannot connect to the mining pool.
%This should probably be around 1/20th of the blocktime.

unpack_mining_data(R,Peer) ->
    case string:equal(Peer,"http://amoveopool.com/work/") of
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

connectionInfo(Pubkey,Peer) ->
    flush(),
    case string:equal(Peer,"http://amoveopool.com/work/") of
	true ->
	    Data = "[\"mining_data\",\""++Pubkey++"\"]",
	    Server = "http://AmoveoPool.com/work"; 
	false -> 
	    Data = <<"[\"mining_data\"]">>,
	    Server = Peer
    end,
    {Data,Server}.

start() ->
    {_,[[Pubkey]]} = init:get_argument(pubkey),
    {_,[[Peer]]} = init:get_argument(pool),
    io:fwrite("Started mining, "),
    io:fwrite("see debug"++os:getenv("CUDA_VISIBLE_DEVICES")++".txt for more info.\n"),
    io:fwrite("Your Pubkey is "++Pubkey++"\n"),
    io:fwrite("You are connecting to "++Peer++"\n"),
    miner(Pubkey,Peer).

getTime()->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).
    

miner(Pubkey, Peer) ->
    {Data,Server} = connectionInfo(Pubkey,Peer),
    io:fwrite(getTime()++" - Ask server for work. "),
    R = talk_helper(Data,Server,1000),
    if
	is_list(R) ->
	    {BlockHash,BlockDiff,WorkDiff} = unpack_mining_data(R,Peer),
	    start_gpu_miner(BlockHash,BlockDiff,WorkDiff,Pubkey,Peer);
	is_atom(R) ->
	    timer:sleep(1000),
	    start()
    end.
    
read_nonce(0) -> 0;
read_nonce(N) ->
    case file:read_file("./mining_data/"++"nonce"++os:getenv("CUDA_VISIBLE_DEVICES")) of
	{ok, <<Nonce:184>>} -> Nonce;
	{ok, <<>>} -> 
	    io:fwrite("nonce failed "),
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n"),
	    timer:sleep(100),
	    read_nonce(N-1)
    end.

start_gpu_miner(BlockHash,BlockDiff,WorkDiff,Pubkey,Peer) ->
    NonceRand = crypto:strong_rand_bytes(23),
    GPUID = os:getenv("CUDA_VISIBLE_DEVICES"),
    ok = file:write_file("./mining_data/"++"nonce"++GPUID, <<"">>),
    DiffsDelimiter = <<"|">>,
    BlockDiffInt = list_to_integer(binary_to_list(BlockDiff)),
    WorkDiffInt = list_to_integer(binary_to_list(WorkDiff)),
    file:write_file("./mining_data/mining_input"++GPUID, <<BlockHash/binary, NonceRand/binary, BlockDiff/binary,DiffsDelimiter/binary,WorkDiff/binary>>),
    Port = open_port({spawn, "./amoveo_gpu_miner "++GPUID},[exit_status]),
    receive 
	{Port, {exit_status,1}}->
	    Nonce = read_nonce(1),
	    io:fwrite(getTime()++" - Found block "),
	    io:fwrite(base64:encode(<<BlockHash/binary,Nonce:184>>)),
	    io:fwrite(" for difficulty "++integer_to_list(WorkDiffInt)++". "),
            BinNonce = base64:encode(<<Nonce:184>>),
	    BinPubkey = list_to_binary(Pubkey),
            Data = << <<"[\"work\",\"">>/binary, BinNonce/binary, <<"\",\"">>/binary, BinPubkey/binary, <<"\"]">>/binary>>,
            talk_helper(Data, Peer, 5),
            timer:sleep(100);
	{Port, {exit_status,0}}->
            ok		
    end,
    miner(Pubkey,Peer).

talk_helper2(Data, Peer) ->
    httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(Data)}, [{timeout, 3000}], []).
talk_helper(_Data, _Peer, 0) -> ok;
talk_helper(Data, Peer, N) ->
    case talk_helper2(Data, Peer) of
        {ok, {_Status, _Headers, []}} ->
            io:fwrite("Server gave empty response\n"),
            timer:sleep(?pool_sleep_period),
            talk_helper(Data, Peer, N-1);
        {ok, {_, _, R}} ->
	    io:fwrite("Server reply: "),
	    io:fwrite(R),
	    io:fwrite("\n"),
	    StrLen = string:len(R),
	    FirstChar = string:sub_string(R,1,1),
	    LastChar = string:sub_string(R,StrLen,StrLen),
	    case string:equal(FirstChar,"[") andalso string:equal(LastChar,"]") of
		true -> R;
		false ->
		    io:fwrite("Server gave incorrect response\n"),
		    timer:sleep(?pool_sleep_period),
	 	    talk_helper(Data,Peer,N-1)
	    end;
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
