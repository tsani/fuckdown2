fuckdown2
=========

Fuckdown2 is a JIT compiler for Brainfuck to x86-64 native linux code. It
should work on any modern Linux computer.

It's called fuckdown2 because I had originally started to work on a Brainfuck
compiler about a year or so ago but didn't make it anywhere; that compiler was
simply called "fuckdown".

In case you were wondering, the goal of this project is to work as many
expletives as possible into the source code.

Features
========

#### Can parse Brainfuck programs from files into a high-level AST

This is Brainfuck, so take the term "high-level" with a grain of salt.

#### Can compile a Brainfuck AST into a high-level x86 AST

This happens in particular in `src/Compiler.hs`. I've developed an EDSL for
creating these x86 ASTs, so it essentially just looks like you're writing
assembly straight into Haskell. Under the hood, this is all just syntactic
sugar for the `AsmF` free monad that is built in `src/Asm.hs`.

Another cool thing about this representation is that the label type and address
type are kept abstract as much as possible. This allows us to reuse the exact
same data structure for both pretty-printing and compilation.

#### Can pretty-print Brainfuck and x86 ASTs

The pretty-printed Brainfuck AST should look just like the input program. It is
a known bug that it currently doesn't. The pretty-printed x86 code is properly
labelled, but is nonetheless pretty hard to read.

#### Can assemble an x86-64 AST into a ByteString

This was definitely the most tedious part of the project, since it involved
frequently looking up the opcode specifications for different instructions, and
figuring exactly which bytes to emit to the bytestring. This all takes place in
`src/Assembler.hs`.

The labelling mechanism is again probably the most interesting part. In our x86
DSL, we would like to be able to jump to labels that are declared further in
the code, since we want to be able to jump over a section of code to a later
one. Since we're using the do notation, we can't exactly do this; labels aren't
associated with string names, they're just magical opaque values. The
workaround that I came up with was to give some kind of syntax for "forward
declaring" a label, and at a later time filling in its value.

The implementation of this label forward declaration mechanism is tricky. The
assembler is a state monad, and it keeps track of all the labels that have been
declared so far in a map. We represent the fact that labels might be _empty_
with `Maybe` in this map. When a label is given an explicit value, we fill it
into the map with `Just`. We have to keep track of how many bytes we've emitted
to the output string in order to be able to correctly assign values to the
labels.

The output of this state monad is then a reader monad which takes as its
configuration a map from labels to values. This is precisely the map that has
been built up by the state monad. If when the state monad completes, the map
has some labels undefined, we simply raise an error. If all the labels are
defined, then we can sequence the map of Maybe values into a map of plain
values and feed it to the reader monad computation that has been built up. This
reader monad computation then will then use the label addresses in the map to
compute the offsets for jump instructions.

The final output of that is the assembled byte array that is essentially the
body of a compiled C function. In order to make it into a bona fide function,
we wrap it with the function intro and outro code for setting up a stack frame.

#### Can invoke a ByteString as a function

This is where the real black magic happens.

We allocate a chunk of memory at least as bit as our assembled ByteString and
ensure that this memory region has the executable permission set. We do this
with an FFI call to `mmap`. Then we copy the assembled ByteString into that
region and using a call to `unsafeCoerce`, convert a pointer to the start of
that region into a pointer to a function having the type `Ptr a -> IO ()` which
essentially corresponds to a C function `void bf(void *mem)`. The argument to
this function is a pointer to a region in memory where our brainfuck program
can play around.

Next we use a _dynamic_ FFI call to convert the function pointer that we
created with `unsafeCoerce` into a bona fide Haskell function. We allocate a
chunk of read-write memory (4KiB should be enough, right?) and pass it to this
function that we've synthesized with the FFI to obtain an IO action. We run the
IO action in main, and that executes our compiled program. Hooray!

Bugs
=====

The major known bug is that the pretty-printer for the Brainfuck AST produces
bogus text. It's not clear exactly why that happens yet.
