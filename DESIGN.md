# Design of Clinklang
This document contains ideas about what the Clinklang synthesizer is going to
be like.

1. [Overview](#overview)
2. [The nodegraph](#the-nodegraph)
   1. [Signals](#signals)
   2. [Nodes](#nodes)
      1. [Numerical operations](#numerical-operations)
      2. [Logical operations](#logical-operations)
      3. [Width operations](#width-operations)
      4. [Implicit values](#implicit-values)
      5. [Random numbers](#random-numbers)
      6. [State and delay](#state-and-delay)
      7. [Buffer access](#buffer-access)
   3. [Composite nodes](#composite-nodes)

## Overview
The primary components of the Clinklang toolchain are:
- A VST plugin with a nodegraph-based user interface, somewhat similar to
  [64klang](https://github.com/hzdgopher/64klang). Instrument/effect patches
  (and subcomponents of these) can be loaded from and saved to files, or
  stored in preset banks in DAW-specifc projects.
- An exporter, which converts the nodegraph into a bytecode-based
  representation and writes it, along with the midi events of the music,
  to a data file. The exporter could be accessed from within the VST through
  a DAW-independent recording mechanism, or from an external tool reading
  a DAW project file.
- A player runtime containing a tiny bytecode compiler and midi event
  sequencer, capable of reproducing the music as part of a small executable.

## The nodegraph
Clinklang employs fine-grained modularity: all available constructs are
ultimately built from very primitive components, many of which correspond to
individual machine instructions. These primitive operations can be combined
into larger components in arbitrary levels of nesting, eventually giving rise
to familiar audio building blocks such as oscillators, filters, mixers,
distortion, delay, reverb, etc.

The whole nodegraph is traversed once per sample. The result is a single
stereo sample value.

MIDI note handling is inherently polyphonic. Each **Note On** results in the
instantiation of a part of the graph with its own state, which persists until
the note is done playing (which may be some time after the **Note Off** due to
release). Monophonic behavior can easily be built on top of this mechanism if
desired.

### Signals
The signals flowing between nodes can be characterized according to several
properties:

Property    | Values                  | Description
:---        | :---                    | :---
Type        | number, bitmask, buffer | Numbers are 64-bit floats.
Width       | mono, stereo            | Mono signals are automatically converted to stereo as needed, but not vice versa.
Context     | note-local, global      | A note-local signal has one instance for every currently playing note.
Scope(?)    | static, dynamic         | A global signal is static if it is constant or only depends on constants. A note-local signal is static if it is available when the note is triggered. All other signals are dynamic, meaning their value (may) vary from one sample to the next.

### Nodes

Input and output connections to nodes are characterized according to the same
properties as signals, indicating what is expected on the inputs and produced
at the outputs. Furthermore, inputs and outputs can be *generic*, meaning that
(for that particular property) they accept any kind of value in, and the kind
of output depends on what inputs are given.

#### Numerical operations

Nodes                   | Type                         | Width   | Context | Scope
:---                    | :---                         | :---    | :---    | :---
add, mul, min, max      | number* &rarr; number        | generic | generic | generic
sub, div                | number, number &rarr; number | generic | generic | generic
addsub                  | number, number &rarr; number | stereo  | generic | generic
sqrt                    | number &rarr; number         | generic | generic | generic
acos, asin, atan, cos, exp, log, log10, sin, tan | number &rarr; number | mono | generic | generic
atan2, pow              | number, number &rarr; number | mono | generic | generic

#### Logical operations

Nodes                   | Type                            | Width   | Context | Scope
:---                    | :---                            | :---    | :---    | :---
compare (various)       | number, number &rarr; bitmask   | generic | generic | generic
and, andnot, or, xor    | number, bitmask &rarr; number   | generic | generic | generic
and, andnot, or, xor    | bitmask, bitmask &rarr; bitmask | generic | generic | generic

#### Width operations

Nodes                   | Type    | Width                    | Context | Scope
:---                    | :---    | :---                     | :---    | :---
expand                  | generic | mono &rarr; stereo       | generic | generic
split                   | generic | stereo &rarr; mono, mono | generic | generic
merge                   | generic | mono, mono &rarr; stereo | generic | generic

#### Implicit values

Nodes                   | Type    | Width  | Context    | Scope
:---                    | :---    | :---   | :---       | :---
Sample rate             | number  | mono   | global     | static
Global sample index     | number  | mono   | global     | dynamic
Note sample index       | number  | mono   | note-local | dynamic
Note tone               | number  | mono   | note-local | static
Note velocity           | number  | mono   | note-local | static
Note gate               | bitmask | mono   | note-local | dynamic

#### Random numbers

Random number generation is provided by a stateless hash function on an
arbitrary number of inputs. The result is a number between -1 and +1 that
depends in a chaotic way on all of its inputs. A sequence of changing values
(such as the current sample index) as input to the random function results
in white noise, which can be used as a basis for noise-based synthesis.

Nodes                   | Type                  | Width  | Context    | Scope
:---                    | :---                  | :---   | :---       | :---
random                  | number* &rarr; number | mono   | generic    | generic

#### State and delay

A state node remembers a value from the previous sample. It has a static
initialization input, defining its value for the first sample.

Delay nodes delay their input some number of samples. A fixed delay takes a
single, static delay value. A variable delay takes an arbitrary number of
dynamic delay values and produces an appropriately delayed output for each.

Nodes          | Type                             | Width   | Context | Scope
:---           | :---                             | :---    | :---    | :---
state          | generic                          | generic | generic | static, dynamic &rarr; dynamic
fixed delay    | number, generic &rarr; generic   | mono, generic &rarr; generic   | generic | static, dynamic &rarr; dynamic
variable delay | generic, number* &rarr; generic* | generic, mono* &rarr; generic* | generic | dynamic, dynamic* &rarr; dynamic*

#### Buffer access

Buffers are arrays of values, which are initialized statically and can be
indexed into dynamically. Buffer initialization is described in the
[Composite nodes](#composite-nodes) section, below.

Nodes          | Type                          | Width   | Context | Scope
:---           | :---                          | :---    | :---    | :---
index          | buffer, number &rarr; generic | mono, mono &rarr; generic | generic | static, dynamic &rarr; dynamic
length         | buffer &rarr; number          | mono &rarr; mono | generic | static &rarr; static

### Composite nodes

To be continued...
