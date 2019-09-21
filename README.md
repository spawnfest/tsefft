# tsefft - Tuple Space erlang FFT

`tsefft` is a Tuple Space based erlang application for computing the
Fast Fourier Transforms.

## About the project

The main objective is to demonstrate/discover how performant `espace`
can be in concurrent numerical applications, and in particular those
with well defined algorithms, such as FFT.

### About `espace` and Tuple spaces

`espace` is an Erlang based implementation of tuple spaces. The first
version of espace was created during the 2017 edition of Spawnfest,
and development since then. Details can be found on
[Hex](https://hex.pm/packages/espace) and
[Github](https://github.com/fredyouhanaie/espace).

[Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space) (aka Linda)
is a programming paradigm for communication and coordination among
concurrent processes.  _Commuincation_ is achieved by worker
processes, there can be as many as needed, posting data tuples to the
tuple space for other cooperating workers to pick them and process
them.  _Coordination_ is achived by worker processes blocking on
certain tuple patterns.

### About Fast Fourier Transforms (FFT)

[FFT](https://en.wikipedia.org/wiki/Fast_Fourier_transform) enables
engineers and scientists to convert a signal, or any data of interest,
from the time domain to frequency domain. For example, given an audio
signal that varies over time, one can produce the frequency spectrum
for that segment, and vice versa with inverse FFT. A list of
applications of FFT can be found on
[Wikipedia](https://en.wikipedia.org/wiki/Discrete_Fourier_transform#Applications).

## Overall plan

We will start with a baseline working implementation, perhaps
`tsefft_0`, and work our way to more efficient implementations.

## Build and tests

As with most Erlang projects, `rebar3` is used for building and
testing the code:

    $ rebar3 compile
	$ rebar3 dialyzer
	$ rebar3 eunit
	$ rebar3 edoc
