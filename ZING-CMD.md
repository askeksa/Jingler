# Using zing-cmd and the Jingler plugin

`zing-cmd` compiles Zing files ([ZING.md](ZING.md)) and sends them to the Jingler plugin, plays
them, or exports them. The [README](README.md) explains how to build both and install the plugin.

## Hearing your instruments

Instruments only make sound when notes are played, so you hear them through the plugin:

1. Load *Jingler* as an instrument plugin in your DAW or tracker. Send it notes on the MIDI
   channels your `main` uses: `1::bass()` plays notes on channel 1.
2. Run

   ```bash
   zing-cmd mysong.zing -r -c
   ```

   This compiles `mysong.zing` and sends it to the plugin (`-c`). With `-r` it keeps running,
   and compiles and sends again every time you save the file or a file it includes.
3. Play. Compile errors are printed by `zing-cmd`; the plugin keeps playing the last program
   that compiled.

When an edit only changes numbers in the code, the plugin swaps them in without interrupting the
sound. Any other edit restarts the program: playing notes stop and delays are cleared.

The plugin has 15 knobs, *Param 01* to *Param 15*, one for each `parameter` in the order they
are declared. It listens for programs on port 26127 on all network interfaces, so it can also be
fed from another machine: `zing-cmd mysong.zing -c -a 192.168.1.20:26127`.

Sounds from gm.dls (`gmdls`, `gmdls_linear`) need the gm.dls file: Windows provides it, and on
other systems you copy it to `~/.jingler/gm.dls`. Without it they are silent.

## Rendering without a DAW

`-y` plays the program and `-w out.wav` writes it to a WAV file. Without notes, only what `main`
makes by itself is heard, for `-d` seconds (default 1). With `-x song.xrns`, the notes and
automation of a Renoise song are played, for the length of the song:

```bash
zing-cmd mysong.zing -x mysong.xrns -w mysong.wav
```

## Options

| Option | Meaning |
|---|---|
| `-r`, `--resident` | Keep running, and redo everything when the file or its includes change |
| `-c`, `--connect` | Send the program to the plugin |
| `-a`, `--address <ADDRESS>` | Where the plugin listens (default `127.0.0.1:26127`) |
| `-x`, `--xrns <RENOISE_FILE>` | Take notes and automation from a Renoise song |
| `-y`, `--play` | Play the sound |
| `-w`, `--write-wav <WAV_FILE>` | Write the sound to a WAV file |
| `-d`, `--duration <SECONDS>` | Length to play or write without a song (default 1) |
| `-s`, `--sample-rate <HZ>` | Sample rate (default 44100) |
| `-p`, `--pretty-print` | Print the program as parsed |
| `-i`, `--print-ir` | Print the compiled bytecode |
| `-g`, `--write-wasm <WASM_FILE>` | Write the generated WebAssembly module |
| `-o`, `--write-source <OUTPUT_FILE>` | Write player source for an intro |
| `-j`, `--jingler-asm-path <JINGLER_ASM>` | Path to `jingler.asm`, for `-o` (default `jingler.asm`) |
| `-q`, `--quantization-levels <N>` | Steps per parameter range in the intro, for `-o` (default 16) |
| `-b`, `--byte-index` | Use a separate index byte for constants, for `-o` |
