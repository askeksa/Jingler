#!/bin/bash

# Build zing-cmd and VST3 plugin
cargo build -r

# Target directory for cargo build
TARGET_DIR=$(cargo metadata --format-version 1 | jq -r '.target_directory')

# Output directory
OUTPUT_DIR="out"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy zing-cmd
cp "$TARGET_DIR/release/zing-cmd" "$OUTPUT_DIR/"

# VST3 Bundle
BUNDLE_NAME="Jingler.vst3"
mkdir -p "$OUTPUT_DIR/$BUNDLE_NAME/Contents/MacOS"

# PkgInfo
echo "BNDL????" > "$OUTPUT_DIR/$BUNDLE_NAME/Contents/PkgInfo"

# Info.plist
VERSION=$(sed -n '/\[workspace.package\]/,/^\[/ { /version *=/ s/.*"\(.*\)".*/\1/p; }' Cargo.toml)
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>English</string>
    <key>CFBundleExecutable</key>
    <string>Jingler</string>
    <key>CFBundleGetInfoString</key>
    <string>vst3</string>
    <key>CFBundleIdentifier</key>
    <string>dk.loonies.jingler</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>Jingler</string>
    <key>CFBundlePackageType</key>
    <string>BNDL</string>
    <key>CFBundleVersion</key>
    <string>$VERSION</string>
    <key>CFBundleSignature</key>
    <string>????</string>
    <key>CSResourcesFileMapped</key>
    <string></string>
</dict>
</plist>" > "$OUTPUT_DIR/$BUNDLE_NAME/Contents/Info.plist"

# Copy dynamic library for VST3 plugin
cp "$TARGET_DIR/release/libvst3_plugin.dylib" "$OUTPUT_DIR/$BUNDLE_NAME/Contents/MacOS/Jingler"
