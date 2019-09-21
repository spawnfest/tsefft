%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the tsefft_data module.
%%% @end
%%% Created :  21 Sep 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsefft_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

% non-existent data file
nofile_open_test() ->
    ?assertMatch({error, enoent}, tsefft_data:siggen_from_file("nofile.txt")).
