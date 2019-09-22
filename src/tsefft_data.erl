%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Various functions for generating test data.
%%%
%%% The data will be generated sequentially rather than making use of
%%% `espace' for speed up.
%%%
%%% Please note that this module was created at the start of the
%%% project for experimentations. It is not of significant use.
%%%
%%% @end
%%% Created : 21 Sep 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsefft_data).

-export([siggen_from_file/1, siggen_1/4, siggen/3]).


%%--------------------------------------------------------------------
%% @doc Read the signal generation parameters from file
%%
%% The file should be an Erlang terms file that can be read with
%% `file:consult/1'.
%%
%% @end
%%--------------------------------------------------------------------
-spec siggen_from_file(file:filename()) -> ok | {error, term()}.
siggen_from_file(File) ->
    case  file:consult(File) of
	{ok, Signals} ->
	    gen_signals(Signals);
	Error ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @doc Generate sets of signals based on the given spec, and save to
%% file.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_signals([{number(),integer(), sin|cos, file:filename()}]) -> ok.
gen_signals([]) ->
    ok;
gen_signals([Sig | Sigs]) ->
    {Sample_freq, Length, Comps, Ofile} = Sig,
    Signal = siggen(Sample_freq, Length, Comps),
    ok = file:write_file(Ofile, io_lib:format("~p~n", [Signal])),
    gen_signals(Sigs).


%%--------------------------------------------------------------------
%% @doc generate a signal of give length and sampling frequency for a
%% given list of harmonics.
%%
%% `Samp_freq' is the sampling frequency, `Length' is the number of
%% samples to generate, and `Components' is a list of `{Amp, Freq,
%% Func}' triples for the harmonics. The `Func' should be either `sin'
%% or `cos'.
%%
%% @end
%%--------------------------------------------------------------------
-spec siggen(number(), integer(), list()) -> [number()].
siggen(Samp_freq, Length, Component) ->
    Period = 1/Samp_freq,
    Ts = [L * Period || L <- lists:seq(0, Length-1)],

    Harmonics = lists:map(
		  fun ({Func, Amp, Freq}) -> siggen_1(Func, Amp, Freq, Ts) end,
		  Component),

    Pair_sum = fun (Xs, Ys) -> lists:zipwith(fun (X,Y) -> X+Y end, Xs, Ys) end,
    Zeros = lists:duplicate(Length, 0),

    Signal = lists:foldl(fun (L, A) -> Pair_sum(L, A) end,
			 Zeros, Harmonics),
    Signal.


%%--------------------------------------------------------------------
%% @doc generate a single sinusoidal series for a given amplitude and
%% frequency.
%%
%% `Func' is the base function from the `math' module to use, it
%% should be either `sin' or `cos'. `Freq' is the frequency in
%% Hz. `Amp' is the signal amplitude. `Ts' is a list of time values
%% for the period.
%%
%% @end
%%--------------------------------------------------------------------
-spec siggen_1(sin|cos, number(), number(), [number()]) -> [number()].
siggen_1(Func, Amp, Freq, Ts) ->
    Omega = 2*math:pi()*Freq,
    Signal = [ Amp*math:Func(Omega*T) || T <- Ts ],
    Signal.
