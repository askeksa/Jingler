//! Fails when the serialized format of `ir::Program` changes while the protocol
//! version stays the same. After raising `protocol::PROTOCOL_VERSION`, running
//! this test rewrites the snapshot `format.json` for the new version.

use std::fs;

use protocol::PROTOCOL_VERSION;
use serde::{Deserialize, Serialize};
use serde_reflection::{Registry, Tracer, TracerConfig};

const SNAPSHOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/format.json");

const NOTE: &str = "The serialized format of ir::Program at this protocol version, traced by \
	`cargo test -p protocol`. The test rewrites this file when the protocol version changes, \
	and fails when the format changes without it.";

#[derive(Deserialize, Serialize)]
struct Snapshot {
	#[serde(default)]
	note: String,
	protocol_version: u32,
	format: Registry,
}

fn traced_format() -> Registry {
	let mut tracer = Tracer::new(TracerConfig::default());
	// The tracer only finds all variants of an enum that is traced by itself,
	// so every enum in ir::Program is listed here, inner ones first.
	tracer.trace_simple_type::<ir::Scope>().unwrap();
	tracer.trace_simple_type::<ir::Width>().unwrap();
	tracer.trace_simple_type::<ir::ValueType>().unwrap();
	tracer.trace_simple_type::<ir::ProcedureKind>().unwrap();
	tracer.trace_simple_type::<ir::NoteProperty>().unwrap();
	tracer.trace_simple_type::<ir::Instruction>().unwrap();
	tracer.trace_simple_type::<ir::Program>().unwrap();
	tracer.registry().unwrap_or_else(|e| panic!("{e}: add the enums to traced_format in {}", file!()))
}

#[test]
fn the_format_changes_only_with_the_protocol_version() {
	let format = traced_format();
	let old = fs::read_to_string(SNAPSHOT).ok().map(|text| {
		serde_json::from_str::<Snapshot>(&text).unwrap_or_else(|e| panic!("{SNAPSHOT} is unreadable: {e}"))
	});
	match old {
		Some(old) if old.protocol_version > PROTOCOL_VERSION => panic!(
			"{SNAPSHOT} is for protocol version {}, newer than protocol::PROTOCOL_VERSION ({PROTOCOL_VERSION})",
			old.protocol_version),
		Some(old) if old.protocol_version == PROTOCOL_VERSION => assert!(old.format == format,
			"The serialized format of ir::Program changed. Raise protocol::PROTOCOL_VERSION \
			(now {PROTOCOL_VERSION}) and run `cargo test -p protocol` again to regenerate {SNAPSHOT}."),
		_ => {
			let snapshot = Snapshot { note: NOTE.to_string(), protocol_version: PROTOCOL_VERSION, format };
			fs::write(SNAPSHOT, serde_json::to_string_pretty(&snapshot).unwrap() + "\n").unwrap();
		}
	}
}
