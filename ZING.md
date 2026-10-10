# The Zing language

Zing is the language you write Jingler instruments in. You describe how a sound is made, as a
chain of oscillators, filters, envelopes and effects, and Jingler runs it once for every sample of
audio, both in the plugin while you compose and in the finished intro.

This guide goes from the surface to the depths:

- [Part 1: Playing with the standard library](#part-1-playing-with-the-standard-library) is all
  you need to build instruments from ready-made parts.
- [Part 2: Your own functions and modules](#part-2-your-own-functions-and-modules) shows how to
  package your own parts.
- [Part 3: Under the hood](#part-3-under-the-hood) explains state, timing and types precisely.
- [Part 4: Advanced](#part-4-advanced) covers layering and buffers.

After that come the [standard library](#standard-library) and a
[language reference](#language-reference). To hear what you write, see [ZING-CMD.md](ZING-CMD.md).

## Part 1: Playing with the standard library

### A first instrument

```zing
include "base.zing"

instrument bass() -> out: mono
	out = key().key2freq.saw.release(0.1.s)

global module main() -> out: stereo
	out = 1::bass()
```

- `include "base.zing"` brings in the **standard library**: oscillators, envelopes, filters,
  effects and unit conversions. It lives in the [`zing`](zing) folder. The path is relative to the
  file that contains the `include`.
- `instrument bass() -> out: mono` defines an instrument called `bass` with no inputs and one
  output, `out`, which is a mono signal. The indented lines below are its body.
- `key().key2freq.saw.release(0.1.s)` reads from left to right: take the key of the note,
  convert it into a frequency, play a sawtooth at that frequency, and fade it out over 0.1 seconds
  after note-off.
- `main` is the output of the whole program. There is exactly one, and it always looks like this
  one: no inputs and a stereo output.
- `1::bass()` plays `bass` for every note on MIDI channel 1 and adds them all up. Each note gets
  its own copy of the instrument, called a **note**, so chords just work. A mono signal used where
  stereo is expected plays on both channels.

Comments start with `#` and run to the end of the line.

### Chaining

Writing `x.f(a, b)` is the same as writing `f(x, a, b)`, and when there are no other arguments,
`x.f` is the same as `f(x)`. So `key().key2freq.saw` is `saw(key2freq(key()))`, written in the
order the signal flows. A chain can continue on the next lines:

```zing
instrument bass() -> out: mono
	out = key().key2freq.saw
		.filter(800.hz.phase2angle, 0.5).lowp
		.adsr(5.ms, 0.3.s, 0.4, 0.2.s)
```

`filter` is a resonant filter with three outputs; `.lowp` picks the low-pass one (`.bandp` and
`.highp` pick the others). Its second argument, `dampen`, sets the resonance: the filter
amplifies its cutoff frequency by `1 / dampen`. `adsr` is an attack-decay-sustain-release
envelope: 5 ms attack, 0.3 s decay, sustain at 0.4 and 0.2 s release.

The [standard library](#standard-library) chapter lists everything base.zing offers, with the
inputs each part takes.

### Units

Inside Zing, time is counted in samples, frequency in cycles per sample and volume as a gain
factor. The standard library converts from the units you know:

| Write | To get |
|---|---|
| `0.5.s`, `20.ms` | a time in samples |
| `440.hz` | a frequency |
| `key().key2freq` | the frequency of the note's key, where key 69 is A at 440 Hz |
| `key().key2period` | the length of one period of the note in samples |
| `-6.db` | a gain factor (0.5) |
| `800.hz.phase2angle` | a filter cutoff |

In `0.5.s` the first dot is the decimal point and the second one calls `s`. In `-6.db` the
minus belongs to the 6, so this is `db(-6)`.

### Variables

Name intermediate results to keep long chains readable. A name is assigned once, and must be
assigned before it's used:

```zing
instrument bass() -> out: mono
	freq = key().key2freq
	tone = freq.saw + 0.5 * (freq / 2).saw
	cutoff = (300 + 2000 * 1.falloff(0.1.s)).hz.phase2angle
	out = tone.filter(cutoff, 0.5).lowp.adsr(5.ms, 0.3.s, 0.4, 0.2.s)
```

`(freq / 2).saw` adds a sawtooth one octave down. `1.falloff(0.1.s)` is a decay from 1 that
halves every 0.1 seconds, so the cutoff starts at 2300 Hz and settles at 300 Hz: an envelope used
to control something other than volume.

### Envelopes and the life of a note

A note starts at note-on. While the key is held, `gate()` is true; at note-off it becomes false.
The envelopes in the standard library use this:

- `adsr(a, d, s, r)` rises to 1 over `a`, falls towards the sustain level `s` at a rate of 1 per
  `d` (so it reaches `s` after `d * (1 - s)`), holds while the key is held, and fades to 0 over
  `r` after note-off.
- `release(r)` only fades out after note-off.
- `falloff(halftime)` decays by itself and ignores the gate. Use it for plucks and drums.

All times are in samples, so write them as `5.ms` or `0.3.s`.

**When does a note end?** A note stops when its output has stayed below about −84 dB (1/16384)
for 8192 samples in a row, about 0.19 seconds. This happens whether or not the key is still
held. So:

- Give every instrument something that brings it to silence: an envelope with a release, or a
  decay. Without one, a note plays forever.
- An instrument that is silent for its first 0.19 seconds, waiting to come in late, is stopped
  before it makes a sound.

### Stereo

An instrument can have a stereo output. `[left, right]` builds a stereo signal from two mono
ones, and arithmetic with a stereo signal works on both channels:

```zing
instrument bass() -> out: stereo
	freq = key().key2freq
	tone = (freq * [0.998, 1.002]).saw + 0.5 * (freq / 2).saw
	cutoff = (300 + 2000 * 1.falloff(0.1.s)).hz.phase2angle
	out = tone.filter(cutoff, 0.5).lowp.adsr(5.ms, 0.3.s, 0.4, 0.2.s)
```

`freq * [0.998, 1.002]` is a stereo frequency, slightly detuned on each side, so `saw` plays two
sawtooths. Parts marked `generic` in the standard library, like `saw` and `filter`, work in
mono or stereo. Parts marked `mono`, like `sine`, only take mono.

To place a mono sound, use `pan(p)` with `p` from −1 (left) to 1 (right). To go from stereo to
mono, use `.left`, `.right` or `.center` (the average).

### Knobs: parameters

A `parameter` is a knob on the plugin. Declare it after the includes, with a range and
optionally a default, and use its name like a variable:

```zing
parameter brightness 100 to 4000 = 800

instrument bass() -> out: stereo
	freq = key().key2freq
	tone = (freq * [0.998, 1.002]).saw + 0.5 * (freq / 2).saw
	cutoff = (brightness + 2000 * 1.falloff(0.1.s)).hz.phase2angle
	out = tone.filter(cutoff, 0.5).lowp.adsr(5.ms, 0.3.s, 0.4, 0.2.s)
```

The plugin has 15 knobs, *Param 01* to *Param 15*, given to the parameters in the order they
are declared, with those of included files first. A parameter follows the knob while notes play,
so you can automate it.

> **Plugin and intro.** In the intro, parameters follow the automation of the song, with values
> rounded to steps of 1/16 of their range (by default). Sounds read from gm.dls (`gmdls`,
> `gmdls_linear`) use the file Windows provides; on other systems the plugin looks for
> `~/.jingler/gm.dls` and is silent without it.

### Instrument inputs

An instrument can take inputs, given where it's called. A `static` input is read once, at
note-on. Any other input is a signal that keeps flowing into the notes while they play. Static
inputs come first:

```zing
parameter brightness 100 to 4000 = 800

instrument bass(transpose: static mono, wobble: mono) -> out: stereo
	freq = (key() + transpose).key2freq
	tone = (freq * [0.998, 1.002]).saw + 0.5 * (freq / 2).saw
	cutoff = (brightness * (1 + 0.5 * wobble) + 2000 * 1.falloff(0.1.s)).hz.phase2angle
	out = tone
		.filter(cutoff, 0.5).lowp
		.adsr(5.ms, 0.3.s, 0.4, 0.2.s)
		.gain(-6.db)

global module main() -> out: stereo
	wobble = 0.2.hz.sine
	out = 1::bass(0, wobble) + 2::bass(-12, wobble)
```

The same instrument now plays on two channels with different settings, one an octave below the
other, and one slow sine wobbles the filters of all their notes. This is the finished bass; it's
also in [`zing/tutorial.zing`](zing/tutorial.zing). The examples below call it as `bass`.

### Channels, key ranges and transposition

`1::` selects MIDI channel 1 (channels go from 1 to 16). You can call the same instrument on
several channels, and use one channel for several instruments, so that each note plays all of
them.

A key range in braces limits a call to some keys, and a key after `/` transposes them: the first
key of the range plays as that key.

```zing main
low = 1{..B-3}::bass(0, 0)
high = 1{C-4..}::bass(0, 0).pan(0.5)
octave_down = 2{C-4..C-6/C-3}::bass(0, 0)
out = low + high + octave_down
```

| Range | Keys |
|---|---|
| `{C-4}` | only C-4 |
| `{C-4..G-4}` | C-4 to G-4 |
| `{..B-3}` | up to and including B-3 |
| `{C-4..}` | C-4 and up |
| `{C-4..C-6/C-3}` | C-4 to C-6, played an octave lower |

A key name is a letter, then `-` or `#` (sharp), then the octave. C-4 is key 48, C-5 is key 60
and A-5 is key 69. The `#` in a key name doesn't start a comment.

### Effects on the mix

Effects inside an instrument run separately for every note. Effects that should apply to the
sum of the notes, like reverb, belong in `main`:

```zing main
lfo = 0.2.hz.sine
basses = 1::bass(0, lfo) + 2::bass(-12, lfo)
out = (basses + basses.reverb(512, 40.ms, 10.ms, 1.s).gain(-40.db)).dc
```

`reverb` returns only the reverberation, and it's loud, so mix it in at a low gain. `dc` removes
any constant offset from the mix.

## Part 2: Your own functions and modules

A song can define its own parts, next to its instruments. There are two kinds: **functions**,
which compute their outputs from their inputs alone, and **modules**, which have state, like an
oscillator or a filter does.

### Functions

```zing
function bpm() -> out: mono
	out = 125

# Length of a number of tracker rows in samples, at 4 rows per beat
function rows(n: mono) -> samples: mono
	samples = n * 60 / (bpm() * 4) * samplerate()

instrument chord() -> out: stereo
	out = key().key2freq.sine.echo(3.rows, 0.4).release(0.5.s)
```

Every input and output has a width: `mono`, `stereo` or `generic` (either one). Functions can call
other functions, but no modules. `samplerate()` is the sample rate in Hz.

### Modules

Turn a chain into a module, and each call gets its own oscillators and filters:

```zing
module acid(freq: mono, cutoff: mono) -> out: mono
	out = freq.saw.filter(cutoff.hz.phase2angle, 0.2).lowp.softclip(3)

instrument squelch() -> out: mono
	freq = key().key2freq
	sweep = 1.falloff(0.2.s)
	out = (acid(freq, 300 + 2000 * sweep) + acid(freq * 1.5, 900)).adsr(5.ms, 0.3.s, 0.5, 0.1.s)
```

### Static inputs

A `static` input of a module is read once, when the module starts: at note-on inside an
instrument, at startup inside `main`. The standard library uses static inputs for things that
can't change while a sound plays, like delay lengths and envelope times. Giving them a signal
that changes is an error:

```zing error
instrument ping() -> out: mono
	# Error: Can't pass a dynamic value into a static input.
	out = key().key2freq.sine.release(0.1.s).echo(0.2.s * (1 + 0.5.hz.sine), 0.5)
```

Mark your own inputs `static` when they must be fixed, for example because you pass them on to a
static input:

```zing
module slapback(in: generic, time: static mono) -> out: generic
	out = in + in.delay(time).gain(-6.db)
```

### Generic inputs

A `generic` input takes mono or stereo, and a `generic` output has the width of the generic
inputs. The same function then works on both:

```zing
function crush(in: generic, levels: mono) -> out: generic
	out = floor(in * levels) / levels
```

An output can only be `generic` if one of the inputs is.

### Several outputs

```zing
module split(in: mono, freq: mono) -> (low: mono, high: mono)
	low = in.filter(freq.hz.phase2angle, 1).lowp
	high = in - low

instrument grit() -> out: mono
	low, high = key().key2freq.saw.split(300)
	out = (low + high.softclip(8).gain(-12.db)).adsr(5.ms, 0.2.s, 0.5, 0.1.s)
```

Assign the outputs to several names at once, using `_` for outputs you don't need, or pick one
with `.0`, `.1` and so on. A call with several outputs can also be the start of a chain: its
outputs become the first arguments. That's how `.lowp` works: it's a function with three inputs
and no body, whose output is its first input.

### Note and global members

Functions and modules marked `note` can read the note with `key()`, `velocity()` (0 to 127) and
`gate()`, and can only be used by instruments and other `note` functions and modules. `adsr`
and `release` are note modules.

```zing
note function vel() -> out: mono
	out = velocity() / 127

instrument tap() -> out: mono
	out = noise(1).filter(4000.hz.phase2angle, 0.5).highp.falloff(30.ms).gain(vel())
```

A `global` module can only be used by `main` and other global modules, and can play
instruments. Give it MIDI inputs, named before its name, to pass channels on to the instruments
it plays. This keeps an instrument and its effects together:

```zing
include "tutorial.zing"

global module ch::wet_bass(transpose: mono) -> out: stereo
	dry = ch::bass(transpose, 0)
	out = dry + dry.echo(0.3.s, 0.4).pan(-0.5)

global module main() -> out: stereo
	out = 1::wet_bass(0) + 2::wet_bass(-12)
```

Functions, modules and instruments are together called **members**.

## Part 3: Under the hood

### Every sample, and once per note

Jingler runs `main` once for every sample. Each value in a program is either:

- **static**: computed once, when the call it's in starts. Inside an instrument that's at
  note-on; inside `main`, at startup.
- **dynamic**: computed again for every sample.

The compiler works out which. A value is static when everything it's computed from is static:
numbers, static inputs, `key()`, `velocity()`, `samplerate()` and other static values. Module
and instrument inputs, parameters and anything coming out of a module are dynamic.
You can state it yourself, and the compiler checks it:

```zing
freq: static = key().key2freq
wobble: dynamic = 5.hz.sine
out = (freq * (1 + 0.01 * wobble)).saw.release(0.1.s)
```

```zing error
# Error: can't be assigned to 'wobble'
wobble: static = 5.hz.sine
out = wobble
```

Functions have no static or dynamic marks: a function call is static when its arguments are.

### State: cell

All memory in Zing comes from two built-in modules, `cell` and `delay`. `cell(next, init)` gives
`init` in the first sample, and in every later sample the value `next` had in the sample before.
`next` can refer to the result of the `cell` itself, which makes a feedback loop:

```zing
module counter() -> out: mono
	out = cell(out + 1, 0)

module smooth(in: mono, speed: static mono) -> out: mono
	out = cell(out + (in - out) * speed, 0)
```

`counter` gives 0, 1, 2, … and `smooth` follows its input a little slower. This is also how
`up` and `phase` in base.zing are made.

Normally a name must be assigned before it's used. Inside the first argument of `cell` or
`delay`, it may also refer to names assigned later, as `filter` in base.zing does. Anywhere else,
that's an error:

```zing error
module broken() -> out: mono
	# Error: Reference to a later variable is only allowed in a cell or delay.
	a = b + 1
	b = 1.up
	out = a
```

Each call of a module has its own cells: every place the module is called, every note of an
instrument, and every round of a `for` loop.

### Delay lines

`delay(next, length)` gives `next` as it was `length` samples ago, and 0 before that. `length`
is static. `dyndelay(next, time, maxtime)` takes a `time` that can change, up to the static
`maxtime`. Both round their delay to a whole number of samples. `comb` in base.zing is a delay
that feeds back into itself; here is a chorus:

```zing
module chorus(in: mono, rate: mono, depth: static mono) -> out: mono
	time = 20.ms + depth * (1 + rate.sine)
	out = in + dyndelay(in, time, 20.ms + 2 * depth)
```

### Mono, stereo and generic

A mono value is automatically copied to both channels where stereo is expected. The opposite
needs `left`, `right` or `center`. A generic member takes the width of its arguments at each
call; mixing stereo and generic arguments in one call is an error. The operator `a -+ b` gives
`[left(a) - left(b), right(a) + right(b)]`; `pan` is built on it.

### Operators

From loosest to tightest:

| Operators | Meaning |
|---|---|
| `c ? a : b` | `a` where `c` is true, else `b`; with a stereo `c`, per channel |
| `\|` `^` `&` | or, exclusive or, and (on bools) |
| `==` `!=` `<` `<=` `>` `>=` | comparisons, giving bools |
| `+` `-` `-+` | addition, subtraction, subtract-left-add-right |
| `*` `/` | multiplication, division |
| `x.f` `x.f(…)` `t.0` | calls, picking one of several values |
| `-x` `!x` | negation, not |

Negation binds tighter than calls, so `-x.abs` is `abs(-x)`, which is what makes `-6.db` work.
Write `-(x.abs)` for the other meaning, and `(a + b).f` to call `f` on a sum.

Comparisons give **bools**, which `?`, `&`, `|`, `^` and `!` work with. Bools are not numbers:
write `c ? 1 : 0` to turn one into a number.

```zing
square = key().key2freq.saw > 0 ? 1 : -1
out = square.gain(-12.db).release(0.1.s)
```

## Part 4: Advanced

### Layering with for

`for i to N add body` computes `body` N times, with `i` running from 0 to N − 1, and adds up the
results. `mul`, `min` and `max` combine them in the other ways. `N` must be static. Every round
has its own state, so this plays 7 independent sawtooths:

```zing
instrument strings() -> out: stereo
	freq = key().key2freq
	layers = for i to 7 add (freq * (1 + 0.003 * random(i, 1))).saw.pan(random(i, 2))
	out = layers.gain(-12.db).adsr(0.3.s, 1.s, 0.7, 0.5.s)
```

`random(x, seed)` gives a number between −1 and 1 that only depends on `x` (rounded to a whole
number) and `seed`, so every note gets the same detuning and panning.

### Buffers

A buffer is a static list of numbers. Write one with braces, read an element with `b[i]` and get
its size with `length(b)`. An index is rounded to the nearest whole number, and reading outside
the buffer gives 0.

```zing
instrument arp() -> out: mono
	steps = {0, 3, 7, 12}
	step = floor(8.hz.up)
	k = key() + steps[step - 4 * floor(step / 4)]
	out = k.key2freq.saw.adsr(5.ms, 0.2.s, 0.6, 0.1.s)
```

`for N buffer m()` fills a buffer with the first N outputs of a module call `m()`, whose inputs
must all be static. Use it to compute something expensive once, when the note starts, instead of
in every sample:

```zing
module organ_wave() -> out: mono
	angle = (1.up / 256).phase2angle
	out = sin(angle) + 0.5 * sin(2 * angle) + 0.25 * sin(3 * angle)

instrument organ() -> out: mono
	table = for 256 buffer organ_wave()
	out = table[floor(key().key2freq.phase(0) * 256)].gain(-6.db).release(0.2.s)
```

Write `for N stereo buffer m()` to make a stereo buffer from a mono module.

### External members

A function or module can be marked `external` and given no body. Its implementation then comes
from the program running Zing instead. See [EXTERNALS.md](EXTERNALS.md).

## Standard library

The standard library is the file [`zing/base.zing`](zing/base.zing). Include it with
`include "base.zing"`, using the path from your file to it. Times are in samples, frequencies in
cycles per sample (see [Units](#units)). Inputs marked `static` must be fixed when the call
starts, and `note` modules can only be used in instruments (see Part 2).

### Basic math functions

| Member | Description |
|---|---|
| `function abs(a: generic) -> b: generic` | Absolute value. |
| `function clamp(x: generic, min_: generic, max_: generic) -> out: generic` | Limit `x` to the range `min_` to `max_`. |
| `function clamp1(x: generic) -> out: generic` | Limit `x` to the range -1 to 1. |
| `function clamp01(x: generic) -> out: generic` | Limit `x` to the range 0 to 1. |
| `function lerp(a: generic, b: generic, t: generic) -> out: generic` | Linear interpolation: `a` when `t` is 0, `b` when `t` is 1. `t` is clamped to 0 to 1. |
| `function mod1(a: generic) -> b: generic` | Fractional part: wraps any value, also negative ones, into 0 to 1. |
| `function range(in: generic, low: generic, high: generic) -> out: generic` | Map `in` from the range -1 to 1 onto `low` to `high`, e.g. to turn an LFO into a sweep. Not clamped. |
| `function fixnan(in: generic) -> out: generic` | Replace NaN (not-a-number) values by 0. |
| `function ssin(in: stereo) -> out: stereo` | Sine of a stereo phase in turns (1 turn = 2 pi), per channel. The built-in `sin` is mono only. |
| `function scos(in: stereo) -> out: stereo` | Cosine of a stereo phase in turns (1 turn = 2 pi), per channel. The built-in `cos` is mono only. |

### Unit conversion functions

| Member | Description |
|---|---|
| `function s(seconds: mono) -> samples: mono` | Seconds to samples, the unit of all times: `0.5.s` |
| `function ms(milliseconds: mono) -> samples: mono` | Milliseconds to samples, the unit of all times: `20.ms` |
| `function hz(hertz: mono) -> persample: mono` | Hertz to cycles per sample, the unit of all frequencies: `440.hz` |
| `function db(level: mono) -> out: mono` | Decibels to a gain factor, counting 6 dB per doubling: `-6.db` is 0.5, `0.db` is 1. |
| `function key2freq(key: mono) -> freq: mono` | MIDI key to frequency in cycles per sample. Key 69 is A at 440 Hz; fractional keys detune. |
| `function key2period(key: mono) -> d: mono` | MIDI key to the length of one period in samples, e.g. to tune a `comb` to a note. |
| `function freq2key(freq: mono) -> key: mono` | Frequency in cycles per sample to MIDI key. The inverse of `key2freq`. |
| `function period2key(period: mono) -> key: mono` | Period length in samples to MIDI key. The inverse of `key2period`. |
| `function phase2angle(phase: generic) -> angle: generic` | Phase in turns to angle in radians (multiply by 2 pi). |
| `function angle2phase(angle: generic) -> phase: generic` | Angle in radians to phase in turns (divide by 2 pi). |

### Basic audio functions

| Member | Description |
|---|---|
| `function gain(in: generic, g: generic) -> out: generic` | Multiply by a gain factor, usually given in decibels: `x.gain(-6.db)` |
| `function pan(in: stereo, p: mono) -> out: stereo` | Constant-power panning, `p` from -1 (left) to 1 (right), clamped. 0 leaves the signal unchanged; fully to one side makes that side 3 dB louder. |
| `function shape(in: generic, s: generic) -> out: generic` | Waveshaper (from 4klang). `s` = 0 leaves `in` unchanged, `s` towards 1 adds drive. Must be below 1. Keeps -1 and 1 in place, so a signal within -1 to 1 stays within it. |
| `function softclip(in: generic, gain: mono) -> out: generic` | Soft saturation: multiply by `gain`, then squash smoothly into -1 to 1. |
| `function gmdls_linear(id: mono, t: mono) -> out: mono` | Sound number `id` from gm.dls at position `t` in samples, linearly interpolated between samples. Silent if gm.dls is not available. |
| `function polyblep(t: generic, dt: generic) -> out: generic` | PolyBLEP smoothing term for a waveform that jumps at phase 0: `t` is the phase (0 to 1), `dt` the frequency in cycles per sample. Zero except within `dt` of the jump. |

### Basic modules

| Member | Description |
|---|---|
| `module up(per_sample: generic) -> out: generic` | Ramp starting at 0 and growing by `per_sample` every sample: `1.up` counts samples. |
| `module phase(freq: generic, offset: static generic) -> out: generic` | Phase that grows by `freq` (cycles per sample) every sample and wraps from 1 to 0. `offset` is the starting phase. The core of the oscillators. |

### Envelopes

| Member | Description |
|---|---|
| `module falloff(in: generic, halftime: static mono) -> out: generic` | Multiply by an exponential decay that starts at 1 and halves every `halftime` samples. |
| `note module adsr(in: generic, a_time: static mono, d_time: static mono, s_level: static mono, r_time: static mono) -> out: generic` | Multiply by an ADSR envelope: rise to 1 over `a_time`, fall by 1 per `d_time` down to `s_level` (reached after `d_time * (1 - s_level)`), and after note-off, fade to 0 over `r_time`. Times in samples. |
| `note module release(in: generic, r_time: static mono) -> out: generic` | Fade out linearly to 0 over `r_time` samples after note-off. No effect while the key is held. |

### Filters

| Member | Description |
|---|---|
| `module filter(input: generic, cutoff: generic, dampen: generic) -> (lpf_out: generic, bpf_out: generic, hpf_out: generic)` | Resonant filter with low-, band- and high-pass outputs; pick one with `lowp`, `bandp` or `highp`. `cutoff` in radians per sample (`1000.hz.phase2angle`), below about 1. Gain at cutoff is `1 / dampen`. |
| `function lowp(lpf_out: generic, bpf_out: generic, hpf_out: generic) -> lpf_out: generic` | Pick the low-pass output of filter: `x.filter(cutoff, dampen).lowp` |
| `function bandp(lpf_out: generic, bpf_out: generic, hpf_out: generic) -> bpf_out: generic` | Pick the band-pass output of filter: `x.filter(cutoff, dampen).bandp` |
| `function highp(lpf_out: generic, bpf_out: generic, hpf_out: generic) -> hpf_out: generic` | Pick the high-pass output of filter: `x.filter(cutoff, dampen).highp` |
| `module comb(in: generic, del: static mono, mul: mono) -> out:generic` | Feedback delay: `in` delayed by `del` samples and multiplied by `mul`, repeating with `mul` applied each time round. The dry signal is not included. With `del = key2period(key())` it rings at the note. |
| `module echo(in: generic, del: static mono, mul: mono) -> out:generic` | Echo: `in` plus its `comb` repeats. |
| `module dc(in: generic) -> out: generic` | Remove DC offset (a constant shift away from 0), e.g. from a mix after distortion or reverb. |

### Oscillator modules

| Member | Description |
|---|---|
| `module saw(freq: generic) -> out: generic` | Sawtooth falling from 1 to -1, `freq` in cycles per sample. |
| `module sine(freq: mono) -> out: mono` | Sine wave, `freq` in cycles per sample. |
| `module noise(seed: mono) -> out: mono` | White noise between -1 and 1. Different integer seeds give independent noise. |
| `module pulse(freq: mono, phase_offset: static mono, phase_width: mono) -> out: mono` | Pulse wave: 1 for the first `phase_width` (0 to 1) of each cycle, -1 for the rest. `freq` in cycles per sample, `phase_offset` is the starting phase. |
| `module trisaw(freq: mono, phase_offset: static mono, phase_width: mono) -> out: mono` | Wave rising from -1 to 1 during the first `phase_width` (0 to 1) of each cycle, falling for the rest: 0.5 is a triangle, near 0 or 1 a sawtooth. `freq` in cycles per sample, `phase_offset` the starting phase. |
| `module fastsine(freq: static stereo, phase_offset: static stereo) -> out: stereo` | Cheap stereo sine for a fixed frequency (cycles per sample) and starting phase, per channel. |

### Effects

| Member | Description |
|---|---|
| `module reverb(in: stereo, offset: static mono, delay_avg: static mono, delay_var: static mono, half_time: static mono) -> out: stereo` | Reverb on the mono sum of `in`: 32 randomly panned combs of `delay_avg` +- `delay_var` samples, decaying to half every `half_time` samples, after a delay of `offset` samples. Returns only the reverb, which is loud. |

## Language reference

### Files

A file consists of, in this order: `include "file.zing"` lines, `parameter` declarations and
members. Included paths are relative to the including file, and each file is included once.
Comments run from `#` to the end of the line, except in key names like `C#4`. Line breaks and
indentation don't matter.

`parameter name min to max = default` declares a knob; the default is optional and is `min` if
left out. In the program, a parameter is a dynamic mono number.

### Members

```
[global | note] function name(inputs) -> outputs
[global | note] module name(inputs) -> outputs
global module ch1::ch2::name(inputs) -> outputs
instrument name(inputs) -> output
external function | module name(inputs) -> outputs
```

followed by statements `pattern = expression`, where a pattern is one or more names, separated
by commas, each optionally with a type. One `global module main() -> out: stereo` is required.

| Member | Can be used by |
|---|---|
| `function`, `module` | any member, except that functions can't use modules |
| `note function`, `note module` | instruments and other note members |
| `global module` | global modules, including `main` |
| `global function` | global modules and other global functions |
| `instrument` | global modules, with a MIDI prefix: `ch::name(…)` |

### Types

A type is `[static | dynamic] [mono | stereo | generic] [number | bool | buffer]`; anything left
out is inferred, and `number` is the default.

- Inputs and outputs must give a width.
- Inputs of modules and instruments are dynamic unless marked `static`. Outputs can't be static.
- Functions, and variables inside functions, take no `static` or `dynamic`.
- An instrument has exactly one output, mono or stereo, and its static inputs come first.
- An output can only be `generic` if an input is.
- A variable is static if its expression is, unless marked `dynamic`.

### Expressions

| Form | Meaning |
|---|---|
| `42`, `0.5`, `0x1F`, `0x0.8`, `inf`, `true`, `false` | numbers (hexadecimal may have a fraction: `0x0.8` is 0.5) and bools |
| `name` | a variable, input or parameter |
| `f(a, b)`, `a.f(b)`, `a.f` | call; with several values on the left, they all become arguments |
| `ch::f(…)`, `ch{range}::f(…)` | instrument or global module call with MIDI input |
| `[l, r]` | stereo from two monos |
| `(a, b)`, `t.0` | several values, one of several values |
| `{a, b, c}`, `b[i]` | static buffer, buffer element |
| `c ? a : b` | choice |
| `for i to N add\|mul\|min\|max body` | repetition |
| `for N [mono \| stereo] buffer m(…)` | buffer filled from a module |

A MIDI input is a channel from 1 to 16 or a MIDI input name of the enclosing global module. A
range is `{k}`, `{k..k}`, `{..k}` or `{k..}`, optionally followed by `/k` to transpose its first
key to `k`.

### Built-in members

| Member | Description |
|---|---|
| `sin(x)`, `cos(x)`, `tan(x)` | trigonometry in radians (mono) |
| `sincos(x)` | two outputs: `sin(x)`, `cos(x)` |
| `atan2(y, x)` | angle of the point (x, y) |
| `exp2(x)`, `log2(x)`, `pow(x, y)` | 2 to the power x, base-2 logarithm, x to the power y (mono) |
| `sqrt(x)`, `floor(x)`, `ceil(x)`, `round(x)`, `trunc(x)` | square root and rounding (generic) |
| `min(a, b)`, `max(a, b)` | smaller and larger (generic) |
| `left(x)`, `right(x)`, `center(x)`, `swap(x)` | channels of a stereo value; `swap` exchanges them |
| `random(x, seed)` | number between −1 and 1 depending only on `x` (rounded) and `seed` |
| `samplerate()` | sample rate in Hz |
| `gmdls(id, i)` | sample `i` of gm.dls sound `id`; looped sounds repeat their loop, others end in silence |
| `length(b)` | number of elements of a buffer |
| `key()`, `velocity()`, `gate()` | the note's key (0 to 127), velocity (0 to 127) and whether its key is held (note members only) |
| `cell(next, init)` | module: `init`, then each sample the previous `next` |
| `delay(next, length)` | module: `next` from `length` (static) samples ago |
| `dyndelay(next, time, maxtime)` | module: `next` from `time` samples ago, at most `maxtime` (static) |
