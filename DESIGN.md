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
instantiation of a part of the graph with its own voice state, which persists
until the note is done playing (which may be some time after the **Note Off**
due to release). Monophonic behavior can easily be built on top of this
mechanism if desired.

### Signals
The signals flowing between nodes can be characterized according to several
properties:

Property    | Values                  | Description
:---        | :---                    | :---
Type        | number, bitmask, buffer | Numbers are 64-bit floats.
Width       | mono, stereo            | Mono signals are automatically converted to stereo as needed, but not vice versa.
Context     | voice, global           | A voice signal has one instance for every currently playing note of the voice.
Scope(?)    | static, dynamic         | A global signal is static if it is constant or only depends on constants. A voice signal is static if it is available when the note is triggered. All other signals are dynamic, meaning their value (may) vary from one sample to the next.

### Nodes

Input and output connectors to nodes are characterized according to the same
properties as signals, indicating what is expected on the inputs and produced
at the outputs. Furthermore, inputs and outputs can be *generic*, meaning that
(for that particular property) they accept any kind of value in, and the kind
of output depends on what inputs are given.

#### Numerical operations

Nodes                   | Type                         | Width   | Context | Scope
:---                    | :---                         | :---    | :---    | :---
add, max, min, mul      | number* &rarr; number        | generic | generic | generic
div, sub                | number, number &rarr; number | generic | generic | generic
addsub                  | number, number &rarr; number | stereo  | generic | generic
ceil, floor, round, sqrt, trunc | number &rarr; number | generic | generic | generic
acos, asin, atan, cos, exp2, log2, sin, tan | number &rarr; number | mono | generic | generic
atan2, pow              | number, number &rarr; number | mono | generic | generic

#### Logical operations

Nodes                   | Type                            | Width   | Context | Scope
:---                    | :---                            | :---    | :---    | :---
compare (eq, ge, gt, le, lt, ne) | number, number &rarr; bitmask | generic | generic | generic
and, andnot, or, xor    | generic, bitmask &rarr; generic | generic | generic | generic

#### Width operations

Nodes                   | Type    | Width                    | Context | Scope
:---                    | :---    | :---                     | :---    | :---
expand                  | generic | mono &rarr; stereo       | generic | generic
split                   | generic | stereo &rarr; mono, mono | generic | generic
merge                   | generic | mono, mono &rarr; stereo | generic | generic

#### Implicit values

Nodes                   | Type    | Width  | Context  | Scope
:---                    | :---    | :---   | :---     | :---
Sample rate             | number  | mono   | global   | static
Global sample index     | number  | mono   | global   | dynamic
Note sample index       | number  | mono   | voice    | dynamic
Note tone               | number  | mono   | voice    | static
Note velocity           | number  | mono   | voice    | static
Note gate               | bitmask | mono   | voice    | dynamic

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
index          | buffer, number &rarr; generic | mono, mono &rarr; generic | generic | generic
length         | buffer &rarr; number          | mono &rarr; mono | generic | generic

### Composite nodes

A composite node is a node which itself contains a graph of nodes inside. These are essential for the modularity of the synth.

Composite nodes have input and output connectors like other nodes, where signals from the outside go in and resulting signals to the outside go out. These are called the *exterior* inputs and outputs. In addition, they have *interior* inputs and outputs, which are the connectors to which the contained graph is connected. Graphically, composite nodes could be shown as frames with the exterior connectors on the outside and the interior connectors on the inside.

#### Block

A **block** node simply groups together a subsection of the graph into a conceptual whole which can be used in multiple places in the graph as if it was an built-in node. It the basic mechanism of abstraction in the graph.

A block can have any number of inputs and outputs. There will be one interior input/output directly corresponding to each exterior input/output. The block definition specifies the type, width, context and scope of each of its inputs and outputs. A property can be specified as *automatic*, in which case it will be inferred from the nodes within the block.

If a property is specified (or inferred) as generic, the actual value is determined for each use, where input properties are inherited from the connected signal, and output properties are propagated through the block from the inputs.

#### Voice

A **voice** node is dynamically instantiated for every played note. Thus, the contents of the voice node are evaluated for every currently playing note.

Static computations inside a voice node are performed once when the node is triggered. These can be used to initialize state nodes containing the state used to generate the sound. Dynamic computations are performed for every sample.

A voice node has two interior outputs: the **sound** signal, a stereo number transmitting the generated sound, and the **kill** signal, a mono bitmask controlling when the note should stop playing and be removed from the list of active notes. The sound signal for all active notes are summed and output on the external sound output.

It is possible to connect an output from a node outside the voice node directly to the input of a node inside the voice node. In this situation, it should be somehow specified whether this signal is to be regarded as static or dynamic inside the voice node, i.e. whether it is read once when the note is triggered or for every sample.

#### Repeat

A **repeat** node repeats its contents a number of times specified by a static input. An internal input contains the current iteration index. All state and delay nodes inside a repeat node are duplicated such that each iteration has its own state.

Internal outputs from the repeat nodes are combined into the external outputs. Usually, the outputs from each iteration would be summed to compute the final output. But any of the associative operations (add, max, min, mul) could be selected. It could also have a more general mode where the output from the previous iteration is available as input to the next.

Repeat nodes are useful for effects like chorus, where almost the same sound is added together many times with small variations e.g. in detuning. It could also be used to iterate through partials in an additive synth.

#### Fill

A **fill** node repeats its contents a number of times specified by a static input, just like a repeat node, but instead of summing the results, they are written to the entries of a buffer.

Internal state is not duplicated, but instead carry over from one iteration to the next.

A fill node has one external output which is a handle to the filled buffer.

Fill nodes can be used to pre-calculate pieces of sound and other data.

#### Stereo

Often it is easiest to describe a computation using a mono signal, but the computation is to be applied to both sides of a stereo signal. A **stereo** node runs its content graph twice, for the left and right sides of the inputs. An extra interior input contains a number identifying the side (e.g. 0 for left, 1 for right).
