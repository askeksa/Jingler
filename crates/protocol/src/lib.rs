//! The link that carries compiled programs from zing-cmd and the GUI to the
//! plugin. A sender connects, sends one program and closes the connection.
//!
//! The bytes are, in order:
//!
//! 1. the magic `JNGL`;
//! 2. the protocol version, a little-endian `u32`;
//! 3. the sender's Jingler version, a `u8` length followed by that many bytes
//!    of UTF-8;
//! 4. the body, whose format belongs to the protocol version.
//!
//! The first three never change (docs/adr/0007), so any two builds can tell
//! whether they use the same protocol version. In protocol version 10 the body
//! is a little-endian `u32` length followed by that many bytes of the bincode
//! of an [`ir::Program`].

use std::error::Error;
use std::fmt;
use std::io::{self, Read, Write};

/// Raise by one with every change after which one side would read or play a
/// program differently from the other: any change to the serialized format of
/// `ir::Program` (renames included), a change in what an existing IR
/// instruction means, or a change to the body that follows the header. Then
/// run `cargo test -p protocol`, which regenerates the format snapshot
/// `tests/format.json`.
pub const PROTOCOL_VERSION: u32 = 10;

/// The Jingler version of this build (the workspace version, raised by hand).
pub const JINGLER_VERSION: &str = env!("CARGO_PKG_VERSION");

/// The first bytes of every connection.
pub const MAGIC: [u8; 4] = *b"JNGL";

/// The TCP port the plugin listens on.
pub const PORT: u16 = 26127;

/// The largest program the plugin accepts, against senders that aren't
/// Jingler.
pub const MAX_PROGRAM_BYTES: usize = 64 * 1024 * 1024;

/// The protocol and Jingler versions of one side of the link.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Versions {
	pub protocol: u32,
	pub jingler: String,
}

impl Versions {
	/// The versions of this build.
	pub fn ours() -> Versions {
		Versions { protocol: PROTOCOL_VERSION, jingler: JINGLER_VERSION.to_string() }
	}
}

impl fmt::Display for Versions {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Jingler {} (protocol {})", self.jingler, self.protocol)
	}
}

/// Send a program with the versions of this build. Returns the size of the
/// program's bincode.
pub fn write_program(writer: &mut impl Write, program: &ir::Program) -> io::Result<usize> {
	write_program_as(writer, &Versions::ours(), program)
}

fn write_program_as(writer: &mut impl Write, versions: &Versions, program: &ir::Program) -> io::Result<usize> {
	let data = bincode::serialize(program).map_err(io::Error::other)?;
	let length = u32::try_from(data.len()).map_err(|_| io::Error::other("program too large"))?;
	let mut frame = header(versions)?;
	frame.extend_from_slice(&length.to_le_bytes());
	frame.extend_from_slice(&data);
	writer.write_all(&frame)?;
	writer.flush()?;
	Ok(data.len())
}

fn header(versions: &Versions) -> io::Result<Vec<u8>> {
	let jingler = versions.jingler.as_bytes();
	let length = u8::try_from(jingler.len()).map_err(|_| io::Error::other("Jingler version too long"))?;
	let mut header = MAGIC.to_vec();
	header.extend_from_slice(&versions.protocol.to_le_bytes());
	header.push(length);
	header.extend_from_slice(jingler);
	Ok(header)
}

/// A program as the plugin received it.
#[derive(Debug)]
pub struct Received {
	pub program: ir::Program,
	/// The sender's versions.
	pub versions: Versions,
	/// The size of the program's bincode.
	pub bytes: usize,
}

/// Why a connection didn't deliver a program. The messages are written for the
/// plugin's status bar.
#[derive(Debug)]
pub enum ReadError {
	/// The connection closed before sending anything.
	Closed,
	/// The data doesn't start like anything Jingler sends.
	NotJingler,
	/// The sender is from before the protocol version was sent.
	Unversioned,
	/// The sender uses another protocol version. Holds the sender's versions.
	VersionMismatch(Versions),
	/// The program is larger than [`MAX_PROGRAM_BYTES`].
	TooLarge(usize),
	/// The connection failed or ended early.
	Io(io::Error),
	/// The program couldn't be decoded, although the protocol versions match.
	Decode(bincode::Error),
}

impl fmt::Display for ReadError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let ours = Versions::ours();
		match self {
			ReadError::Closed => write!(f, "A connection closed without sending a program"),
			ReadError::NotJingler => write!(f, "Received data that isn't a program from Jingler"),
			ReadError::Unversioned => write!(f,
				"Program rejected: the sender is older than protocol versioning, but this plugin (Jingler {}) \
				uses protocol version {}. Update the sender (zing-cmd or the GUI).",
				ours.jingler, ours.protocol),
			ReadError::VersionMismatch(theirs) => write!(f,
				"Program rejected: protocol version {} from Jingler {}, but this plugin (Jingler {}) \
				uses protocol version {}. Update the {} so both use the same protocol version.",
				theirs.protocol, theirs.jingler, ours.jingler, ours.protocol,
				if theirs.protocol < ours.protocol { "sender (zing-cmd or the GUI)" } else { "plugin" }),
			ReadError::TooLarge(bytes) => write!(f, "Program too large ({} bytes, max {})", bytes, MAX_PROGRAM_BYTES),
			ReadError::Io(e) => write!(f, "Failed to read program: {}", e),
			ReadError::Decode(e) => write!(f, "Failed to decode program: {}", e),
		}
	}
}

impl Error for ReadError {}

impl From<io::Error> for ReadError {
	fn from(e: io::Error) -> ReadError {
		ReadError::Io(e)
	}
}

/// Receive a program. Reads the header first and stops there unless the
/// sender's protocol version is [`PROTOCOL_VERSION`].
pub fn read_program(reader: &mut impl Read) -> Result<Received, ReadError> {
	let mut magic = [0u8; 4];
	let first = loop {
		match reader.read(&mut magic) {
			Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
			result => break result?,
		}
	};
	if first == 0 {
		return Err(ReadError::Closed);
	}
	reader.read_exact(&mut magic[first..])?;
	if magic != MAGIC {
		// Senders from before the header started with the program's length.
		let length = u32::from_le_bytes(magic) as usize;
		return Err(if length > 0 && length <= MAX_PROGRAM_BYTES { ReadError::Unversioned } else { ReadError::NotJingler });
	}

	let protocol = read_u32(reader)?;
	let mut length = [0u8; 1];
	reader.read_exact(&mut length)?;
	let mut jingler = vec![0u8; length[0] as usize];
	reader.read_exact(&mut jingler)?;
	let versions = Versions { protocol, jingler: String::from_utf8_lossy(&jingler).into_owned() };
	if protocol != PROTOCOL_VERSION {
		return Err(ReadError::VersionMismatch(versions));
	}

	let bytes = read_u32(reader)? as usize;
	if bytes > MAX_PROGRAM_BYTES {
		return Err(ReadError::TooLarge(bytes));
	}
	let mut data = vec![0u8; bytes];
	reader.read_exact(&mut data)?;
	let program = bincode::deserialize(&data).map_err(ReadError::Decode)?;
	Ok(Received { program, versions, bytes })
}

fn read_u32(reader: &mut impl Read) -> io::Result<u32> {
	let mut bytes = [0u8; 4];
	reader.read_exact(&mut bytes)?;
	Ok(u32::from_le_bytes(bytes))
}

#[cfg(test)]
mod tests {
	use super::*;

	fn program() -> ir::Program {
		ir::Program {
			parameters: vec![ir::Parameter { name: "cutoff".to_string(), min: 20.0, max: 2000.0, default: 800.0 }],
			procedures: vec![ir::Procedure {
				name: "main".to_string(),
				kind: ir::ProcedureKind::Module { scope: ir::Scope::Dynamic },
				inputs: vec![],
				outputs: vec![],
				code: vec![ir::Instruction::Constant(0x3f80_0000)],
			}],
			externals: vec![],
			main_static_proc_id: 0,
			main_dynamic_proc_id: 0,
			track_order: vec![ir::MidiMapping { channel: 0, start: 0, end: 127, transpose_to: 0 }],
		}
	}

	fn sent_as(versions: &Versions) -> Vec<u8> {
		let mut data = vec![];
		write_program_as(&mut data, versions, &program()).unwrap();
		data
	}

	fn versions(protocol: u32, jingler: &str) -> Versions {
		Versions { protocol, jingler: jingler.to_string() }
	}

	#[test]
	fn a_program_arrives_with_the_senders_versions() {
		let mut data = vec![];
		let bytes = write_program(&mut data, &program()).unwrap();
		let received = read_program(&mut data.as_slice()).unwrap();
		assert_eq!(received.program, program());
		assert_eq!(received.versions, Versions::ours());
		assert_eq!(received.bytes, bytes);
	}

	#[test]
	fn the_header_is_magic_protocol_version_and_jingler_version() {
		let data = sent_as(&versions(10, "0.8.1"));
		assert_eq!(&data[..14], b"JNGL\x0a\0\0\0\x050.8.1");
	}

	#[test]
	fn another_protocol_version_is_rejected_after_the_header() {
		let mut data = sent_as(&versions(PROTOCOL_VERSION + 1, "9.9.9"));
		data.truncate(4 + 4 + 1 + 5);
		match read_program(&mut data.as_slice()) {
			Err(ReadError::VersionMismatch(theirs)) => assert_eq!(theirs, versions(PROTOCOL_VERSION + 1, "9.9.9")),
			result => panic!("expected a version mismatch, got {:?}", result),
		}
	}

	#[test]
	fn the_mismatch_message_has_both_sides_versions_and_names_the_older_side() {
		let newer = ReadError::VersionMismatch(versions(PROTOCOL_VERSION + 1, "9.9.9")).to_string();
		assert_eq!(newer, format!(
			"Program rejected: protocol version {} from Jingler 9.9.9, but this plugin (Jingler {}) \
			uses protocol version {}. Update the plugin so both use the same protocol version.",
			PROTOCOL_VERSION + 1, JINGLER_VERSION, PROTOCOL_VERSION));
		let older = ReadError::VersionMismatch(versions(PROTOCOL_VERSION - 1, "0.0.1")).to_string();
		assert!(older.contains("Update the sender (zing-cmd or the GUI) so"), "{older}");
	}

	#[test]
	fn a_sender_from_before_the_header_is_recognized() {
		let data = bincode::serialize(&program()).unwrap();
		let mut old = (data.len() as u32).to_le_bytes().to_vec();
		old.extend_from_slice(&data);
		assert!(matches!(read_program(&mut old.as_slice()), Err(ReadError::Unversioned)));
	}

	#[test]
	fn other_data_is_not_mistaken_for_an_old_sender() {
		assert!(matches!(read_program(&mut &b"GET / HTTP/1.1\r\n"[..]), Err(ReadError::NotJingler)));
	}

	#[test]
	fn an_empty_connection_is_closed() {
		assert!(matches!(read_program(&mut &b""[..]), Err(ReadError::Closed)));
	}

	#[test]
	fn a_truncated_program_is_an_io_error() {
		let data = sent_as(&Versions::ours());
		assert!(matches!(read_program(&mut &data[..data.len() - 1]), Err(ReadError::Io(_))));
	}

	#[test]
	fn a_too_large_program_is_rejected_before_reading_it() {
		let mut data = header(&Versions::ours()).unwrap();
		data.extend_from_slice(&(MAX_PROGRAM_BYTES as u32 + 1).to_le_bytes());
		assert!(matches!(read_program(&mut data.as_slice()), Err(ReadError::TooLarge(n)) if n == MAX_PROGRAM_BYTES + 1));
	}
}
