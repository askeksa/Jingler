# Jingler
[Jingler](https://github.com/askeksa/Jingler) is a flexible,
modular synthesizer engine with a primary focus on making music for small
[demoscene](https://en.wikipedia.org/wiki/Demoscene) productions - similar in
spirit to popular modular demoscene synthesizers such as
[4klang](https://github.com/hzdgopher/4klang) and
[64klang](https://github.com/hzdgopher/64klang).

Instrument patches for Jingler are written in the [Zing](ZING.md) programming language. A node graph based GUI is in development.

## Building and installing

1. [Install Rust](https://rust-lang.org/tools/install/).
2. Run the build script for your platform ([`build_mac.sh`](build_mac.sh) or [`build_windows.ps1`](build_windows.ps1)). This will place `Jingler.vst3` and `zing-cmd` inside the `out` directory.
3. Copy `Jingler.vst3` to your VST3 directory (`/Library/Audio/Plug-Ins/VST3` or `~/Library/Audio/Plug-Ins/VST3` on Mac, `C:\Program Files\Common Files\VST3` on Windows).

## Usage

TBW

## License

The [player code](player/jingler.asm) for inclusion in intros is distributed under the terms of the [Zlib license](https://en.wikipedia.org/wiki/Zlib_License). The rest of the project is covered by the [Mozilla Public License 2.0](https://en.wikipedia.org/wiki/Mozilla_Public_License).
