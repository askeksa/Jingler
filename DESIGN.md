# Design of Clinklang
This document contains ideas about what the Clinklang synthesizer is going to
be like.

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

MIDI note handling is inherently polyphonic. Each **Note On** results in the
instantiation of a part of the graph with its own state, which persists until
the note is done playing (which may be some time after the **Note Off** due to
release). Monophonic behavior can easily be built on top of this mechanism if
desired.

### Signals
The signals flowing between nodes can be characterized according to several
properties:

Property    | Values              | Description
:---        | :---                | :---
Type        | float, bool, buffer |
Width       | mono, stereo        |
Context     | note, global        |
Presence(?) | static, dynamic     |

To be continued...
