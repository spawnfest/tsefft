% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% This module handles the functions for the Discrete Fourier
%%% Transforms (DFT).
%%%
%%% All the computation here is based on a freely available DSP ebook
%%% <a href="http://www.dspguide.com/pdfbook.htm">here</a>.
%%%
%%% The main equations used are from
%%% <a href="http://www.dspguide.com/ch8.htm">chapter 8</a>.
%%%
%%% For equations 8-2 and 8-3 see
%%% <a href="http://www.dspguide.com/ch8/5.htm">this section</a>
%%%
%%% For equations 8-4 see
%%% <a href="http://www.dspguide.com/ch8/6.htm">this section</a>
%%%
%%% @end
%%% Created : 22 Sep 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsefft_dft).

-export([sum/1, do_sum/3, dft_by_corr/1, inverse_dft/1]).


%%--------------------------------------------------------------------
%% @doc Initiate the worker process to perform the sum.
%%
%% Compute the sum of a set of data points. The data points are
%% expected to be in the TS tagged with `Tag'.
%%
%% The `Tag' can be any term, although, the best choices are either an
%% atom unique to this data set, or perhaps `{tag,''_'`}', where the
%% second element may identify the index of the data element within
%% the set. But we should use the '_' pattern as wild card to capture
%% all the data.
%%
%% There should be two types of tuples: the count of data,
%% `{Tag,count,Count}', and the `Count' data values: `{Tag, value,
%% Value}'.
%%
%% The sum will be performed within a single process, during which,
%% the `count' and `value' tuples will be consumed.
%%
%% Once complete, the sum will be added to the TS as `{Tag, sum, Sum}'
%%
%% @end
%%--------------------------------------------------------------------
-spec sum(term()) -> ok.
sum(Tag) ->
    {[Count], _} = espace:in({Tag, count, '$1'}),
    espace:worker({?MODULE, do_sum, [Tag, Count, 0]}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%%
%% This is the worker process that will consume the data points and
%% produce their sum.
%%
%% 
%%
%% @end
%%--------------------------------------------------------------------
-spec do_sum(term(), integer(), number()) -> ok.
do_sum(Tag, 0, Sum) ->
    espace:out({Tag, sum, Sum}),
    ok;

do_sum(Tag, Count, Sum) ->
    {[Value], _} = espace:in({Tag, value, '$1'}),
    do_sum(Tag, Count-1, Sum+Value).


%%--------------------------------------------------------------------
%% @doc Compute the DFT by Correlation.
%%
%% Compute the `Re' and `Im' frequency components of `Signal' using
%% the equations defined in equation 8-4 of chapter 8 of the the DSP
%% Guide, see http://www.dspguide.com/ch8/6.htm.
%%
%% For each of the N input data points we generate 2 x N/2 tuples,
%% which are then summed up to form the N/2 {Re_x,K] and {Im_x,K} data sets.
%%
%% The function returns N/2 complex pairs.
%%
%% @end
%%--------------------------------------------------------------------
-spec dft_by_corr([float()]) -> [{float(), float()}].
dft_by_corr([]) ->
    [];

dft_by_corr(Signal) ->
    NN = length(Signal),
    Pi = math:pi(),
    S = fun (K, I, N) -> math:sin(2*Pi*K*I/N) end,
    C = fun (K, I, N) -> math:cos(2*Pi*K*I/N) end,

    % load the data points into the TS
    dft_by_corr_data(Signal, 0, S, NN, im_x),
    dft_by_corr_data(Signal, 0, C, NN, re_x),

    % sum up the data points, the Im sums will need to be negated afterwards
    lists:foreach(fun (K) -> tsefft_dft:sum({re_x, K}) end, lists:seq(0, NN div 2)),
    lists:foreach(fun (K) -> tsefft_dft:sum({im_x, K}) end, lists:seq(0, NN div 2)),

    % extrat the frequence coordinate pairs
    Ext_sum = fun (Tag) -> {[Sum], _} = espace:in({Tag, sum, '$1'}), Sum end,
    [ {Ext_sum({re_x, K}), -Ext_sum({im_x, K})} || K <- lists:seq(0, NN div 2) ].


%%--------------------------------------------------------------------
%% @doc Generate the data points for the Re_X_k and Im_X_k values.
%%
%% The data points will be evaluated concurrently via espace
%% eval. When done, we will end up with 2 x N x (N/2+1) tuples.
%%
%% The tuples will have tags `{re_x, K}' and `{im_x, K}'. `K' will
%% range from 0 to `N/2'. There will be one tuple corresponding to
%% each data point in the input Signal.
%%
%% @end
%%--------------------------------------------------------------------
-spec dft_by_corr_data([float()], integer(), function(), integer(), term()) -> ok.
dft_by_corr_data([], _I, _C_S, N, Tag) ->
    lists:foreach(
      fun (K) -> espace:out({ {Tag, K}, count, N}) end,
      lists:seq(0, N div 2)),
    ok;

dft_by_corr_data([H_sig|T_sig], I, C_S, N, Tag) ->
    lists:foreach(
      fun (K) -> espace:eval({ {Tag, K}, value, fun ()-> H_sig*C_S(K, I, N) end}) end,
      lists:seq(0, N div 2)),
    dft_by_corr_data(T_sig, I+1, C_S, N, Tag),
    ok.

%%--------------------------------------------------------------------
%% @doc Calculate the inverse DFT from the cosine and sine amplitudes.
%%
%% The input is a list of N/2 complex pairs.
%%
%% We are applying equations 8-2 and 8-3 from chapter 8 of the the DSP
%% Guide, see http://www.dspguide.com/ch8/5.htm.
%%
%% From the N/2 complex pairs, we generate the 2 x `N/2' scaled, eqn
%% 8-3, `{re_x_s,K}' and `{im_x_s,K}'.
%%
%% From the `{re_x_s,K}' and `{im_x_s,K}' we generate the 2 x N x N/2
%% products, `{x_re,I}' and `{x_im,I}', which when summed up, eqn
%% 8-2, produce the N x `{x,I}' values for the result.
%%
%% @end
%%--------------------------------------------------------------------
-spec inverse_dft([ {float(), float()} ]) -> [float()].
inverse_dft([]) ->
    [];

inverse_dft(Re_Im_x) ->
    N2 = length(Re_Im_x)-1, %% this is N/2 in the eqns, note index is 0..(N/2)
    NN = 2*N2, %% this is N in the eqns, index is 0..(N-1)

    % split and add the individual complex pairs into the TS
    split_out(Re_Im_x, 0),

    % apply the scale factors to the tuples (concurrently)
    scale_coeff(N2),

    Pi = math:pi(),
    C = fun (K, I) -> math:cos(2*Pi*K*I/NN) end,
    S = fun (K, I) -> math:sin(2*Pi*K*I/NN) end,

    % generate the 2 x N x N/2 elements that will be summed up
    lists:foreach(fun (I) -> calc_x_I_K(I, N2, C, re) end, lists:seq(0, NN-1)),
    lists:foreach(fun (I) -> calc_x_I_K(I, N2, S, im) end, lists:seq(0, NN-1)),
    
    % sum up the results
    lists:foreach(fun (I) -> tsefft_dft:sum({x_re, I}) end, lists:seq(0, NN-1)),
    lists:foreach(fun (I) -> tsefft_dft:sum({x_im, I}) end, lists:seq(0, NN-1)),

    % generate the x_i set
    lists:foreach(fun (I) -> espace:eval({{x,I}, {fun calc_x_I/1, [I]}}) end, lists:seq(0, NN-1)),

    % flush the input data, as they're no longer needed
    lists:foreach(fun (K) ->
                          espace:in({{re_x_s,K}, '_'}),
                          espace:in({{im_x_s,K}, '_'})
                  end,
                  lists:seq(0,N2)),

    % extract the calculated x_I into an ordered sequence
    Ext_x_i = fun (I) -> {[X_i], _} = espace:in({{x,I}, '$1'}), X_i end,
    Signal = [ Ext_x_i(I) || I <- lists:seq(0,NN-1)],
    Signal.


%%--------------------------------------------------------------------
%% @doc Apply scaling all Re/Im coefficients.
%%
%% All {{re_x,K},Value} tuples will be replaced with
%% {{re_x_s,K},Scaled_value}, and similarly for the im_x ones
%%
%% The scaling will be performed concurrently for all the tuples.
%%
%% @end
%%--------------------------------------------------------------------
-spec scale_coeff(integer()) -> ok.
scale_coeff(N2) ->
    lists:foreach(fun (K) ->
                          espace:eval({{re_x_s,K}, {fun scale_re/2, [K, N2]}})
                  end,
                  lists:seq(0, N2)),
    lists:foreach(fun (K) ->
                          espace:eval({{im_x_s,K}, {fun scale_im/2, [K, N2]}})
                  end,
                  lists:seq(0, N2)),
    ok.


%%--------------------------------------------------------------------
%% @doc Given a list of {Re, Im} pairs, dump them as indexed tuples.
%%
%% This is the first stage of the inverse DFT calculation.
%%
%% @end
%%--------------------------------------------------------------------
-spec split_out(list(), integer()) -> ok.
split_out([], _K) ->
    ok;
split_out([{Re, Im}|Rest], K) ->
    espace:out({{re_x,K}, Re}),
    espace:out({{im_x,K}, Im}),
    split_out(Rest, K+1).


%%--------------------------------------------------------------------
%% @doc apply scaling to the input Im coefficients, and return the
%% scaled value.
%%
%% This is only need to be done once for each coefficient, so the
%% coefficient tuple is removed from the Tuple Space.
%%
%% @end
%%--------------------------------------------------------------------
-spec scale_im(integer(), integer()) -> float().
scale_im(K, N2) ->
    {[Im], _} = espace:in({{im_x, K}, '$1'}),
    Im / N2.


%%--------------------------------------------------------------------
%% @doc apply scaling to the input Re coefficients, and return the
%% scaled value.
%%
%% This is only need to be done once for each coefficient, so the
%% coefficient tuple is removed from the Tuple Space.
%%
%% @end
%%--------------------------------------------------------------------
-spec scale_re(integer(), integer()) -> float().
scale_re(K, N2) ->
    {[Re], _} = espace:in({{re_x, K}, '$1'}),
    case K of
        0  -> Re / (2*N2);
        N2 -> Re / (2*N2);
        _  -> Re / N2
    end.


%%--------------------------------------------------------------------
%% @doc initiate calculation of individual x_i products.
%%
%% For a given x_i we generate a set of N/2+1 calculations.
%%
%% The ultimate collective output of this call will be N/2+1 tuples
%% each for the cosine and sine products, as in eqn 8-2.
%%
%% We get espace to perform the calculations independently, producing
%% N/2+1 products. These weill later be used to compute the larger Re
%% and Im sums.
%%
%% `C_S' is either the cosine or sine function, `Tag' is either `re_x'
%% or `im_x'
%%
%% @end
%%--------------------------------------------------------------------
-spec calc_x_I_K(integer(), integer(), function(), term()) -> ok.
calc_x_I_K(I, N2, C_S, re) ->
    lists:foreach(
      fun (K) ->
              espace:eval({ {x_re, I}, value, {fun calc_product/4, [re, C_S, I, K]} })
      end,
      lists:seq(0, N2)),
    espace:out({{x_re, I}, count, N2+1}),
    ok;

calc_x_I_K(I, N2, C_S, im) ->
    lists:foreach(
      fun (K) ->
              espace:eval({ {x_im, I}, value, {fun calc_product/4, [im, C_S, I, K]} })
      end,
      lists:seq(0, N2)),
    espace:out({{x_im, I}, count, N2+1}),
    ok.


%%--------------------------------------------------------------------
%% @doc calculate the product of a given Re/Im coefficient and its
%% corresponding cosine/sine function.
%%
%% These are the individual products in the sums in eqn 8-2.
%%
%% Note that unlike the rest of this module, here we need to use
%% `espace:rd/1' instead of `espace:in/1', since each Re/Im
%% coefficient is needed by all `x_i'. These will be removed, once the
%% entire computation is complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec calc_product(re|im, function(), integer(), integer()) -> float().
calc_product(re, C_S, I, K) ->
    {[X_s_k], _} = espace:rd({{re_x_s,K}, '$1'}),
    X_s_k * C_S(I,K);

calc_product(im, C_S, I, K) ->
    {[X_s_k], _} = espace:rd({{im_x_s,K}, '$1'}),
    X_s_k * C_S(I,K).
    

%%--------------------------------------------------------------------
%% @doc calculate the x_i as the sum of the two Re and Im sums
%%
%% This is basically the final stage of calculation of an x_i.
%%
%% @end
%%--------------------------------------------------------------------
-spec calc_x_I(integer()) -> float().
calc_x_I(I) ->
    {[Re_sum],_} = espace:in({{x_re,I}, sum, '$1'}),
    {[Im_sum],_} = espace:in({{x_im,I}, sum, '$1'}),
    Re_sum+Im_sum.
