# Build zing-cmd and VST3 plugin
cargo build -r

# Target directory for cargo build
$TARGET_DIR = (cargo metadata --format-version 1 | ConvertFrom-Json).target_directory

# Output directory
$OUTPUT_DIR = "out/Windows"
if (Test-Path $OUTPUT_DIR) { Remove-Item -Recurse -Force $OUTPUT_DIR }
New-Item -ItemType Directory -Path $OUTPUT_DIR | Out-Null

# Copy zing-cmd
Copy-Item "$TARGET_DIR\release\zing-cmd.exe" "$OUTPUT_DIR\"

# VST3 plugin - on Windows, the .vst3 file is just the DLL directly
Copy-Item "$TARGET_DIR\release\vst3_plugin.dll" "$OUTPUT_DIR\Jingler.vst3"
