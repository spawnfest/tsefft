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

## Some explanations for the curious

### The Mechanics of Tuple spaces (and `espace`)

The tuple space (TS) paradigm uses tuples of various shapes and sizes
as the central tool for performing computations by concurrent
actors/workers. The tuples are kept in an associative store where the
worker processes can input and output tuples as the need
arises. Worker processes can appear and disappear as the need arises.

To use a human analogy, one can imagine the tuples as post-it notes
and the assiciative store as large wall. Workers can stick notes on
wall for others to pick and stick other notes as needed. A worker can
appear out of nowhere with a note that contains one or more formulae,
the worker will then evaluate the formulae, replacing them with the
result, and then post the result on the wall. There may be some
workers that just monitor the wall for specific patterned
tuples/notes, once they see one, they will take the note and carry out
whatever work they should perform with it.

One last example, imagine a busy restaurant, an order is arrives in
the kitchen. The person who picks the order, shouts out its
contents. The chef in charge of the soup will be listening for the
soup order and reac accordingly, the one in charge of salads will be
listening for salad orders, etc. Once all the food is ready, the whole
order is delivered to the customer by the waiter/waitress, who would
be watching out for it.

With `espace`, there are operations for outputting tuples to the TS,
operations for inputting (blocking and non-blocking) from the TS, and
operations for evaluating and outputtin a tuple with
functions/expression.

Some small examples:

* add a tuple to the TS

	> espace:out({addme, 1, 2}).
  
* wait for a tuple, and process it, and output the result to the TS
  (yes, the syntax is all plumbing and no porcelain!)
  
	  > {[X,Y], _} = espace:in({addme, '$1', '$2'}).
	  > espace:out({sum, X, Y, X+Y}).

* evaluate and output a tuple

```
	> X = 1.
	> Y = 2.
	> espace:eval({sum, X, Y, fun () -> X+Y end}).
```


### The Mechanics of DFT/FFT

Discrete Fourier Transforms
([DFT](https://en.wikipedia.org/wiki/Discrete_Fourier_transform)) and
Fast Fourier Transforms
([FFT](https://en.wikipedia.org/wiki/Fast_Fourier_transform)) are
algorithms for computing the frequency distribution of a time based
sequence. An example that many may have encountered is the graphical
display on some Hi-Fi equipment that show vertical bars moving up and
down as music is played. The vertical bars are the frequencies of the
audio being played. However, they cover many application
[areas](https://en.wikipedia.org/wiki/Discrete_Fourier_transform#Applications).
than just audio signals.

The calculations involve many operations (sums and products), that can
be performed concurrently. The algorithm is classed as
[Embarrassingly_parallel](https://en.wikipedia.org/wiki/Embarrassingly_parallel).
This makes it an ideal candidate for computation with Tuple Spaces.

There are numerous sources of information and books on the
subject. The author of this project has used the
[book](http://www.dspguide.com/pdfbook.htm) which has been made
available for free for personal use by its author.

### The Mechanics of TSEFFT

The DFT computations involves sums (&Sigma;), for example for an input
signal with 32 point, there will be 2 x 16 sums that will add 32
numbers together. The sum is implemented in the `tsefft_dft:sum/1` and
`tsefft_dft:do_sum/3` functions. They expect a sequence of numbers to
be in the TS, identified by a unique tag, e.g. there will be multiple
tuples like `{{re_x, K}, value, V}`, each identifying one data point,
together with `{{re_x, K}, count, 32}`, which is the count of the data
points for this tag. A worker process initiated by `sum/1` will
extract the data points one at a time, and when done will output their
sum as `{{re_x, K}, sum, Sum}`. Of course, there will be 32 such
workers running concurrently.

There are many areas where we are using `lists:foreach/2` and
`espace:eval/1` to inittiate multiple concurrent calculations.
