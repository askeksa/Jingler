//! What the plugin has received and what went wrong, shown by the editors and
//! by the DAW's value texts. Shared by all plugin instances, like the runtime.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, Mutex, MutexGuard};

use nice_plug_egui::RepaintNotifier;

#[derive(Default)]
pub struct Status {
	/// The last program the runtime accepted.
	pub program: Option<Arc<ir::Program>>,
	/// When and from whom the last program arrived.
	pub received: Option<Arrival>,
	/// The last error since then.
	pub error: Option<String>,
}

pub struct Arrival {
	/// Local time.
	pub time: String,
	/// The sender's versions.
	pub sender: protocol::Versions,
}

#[derive(Default)]
pub struct Shared {
	status: Mutex<Status>,
	/// The repaint notifiers of the editors, with an id for unregistering.
	notifiers: Mutex<Vec<(u64, RepaintNotifier)>>,
	next_id: AtomicU64,
}

static SHARED: LazyLock<Shared> = LazyLock::new(Shared::default);

pub fn shared() -> &'static Shared {
	&SHARED
}

impl Shared {
	pub fn status(&self) -> MutexGuard<'_, Status> {
		self.status.lock().unwrap_or_else(|e| e.into_inner())
	}

	pub fn parameter(&self, index: usize) -> Option<ir::Parameter> {
		self.status().program.as_ref()?.parameters.get(index).cloned()
	}

	/// Record a program the runtime accepted, with its sender's versions.
	pub fn accept_program(&self, program: ir::Program, sender: protocol::Versions) {
		{
			let mut status = self.status();
			status.program = Some(Arc::new(program));
			status.received = Some(Arrival { time: chrono::Local::now().format("%H:%M:%S").to_string(), sender });
			status.error = None;
		}
		self.repaint();
	}

	pub fn report_error(&self, error: String) {
		self.status().error = Some(error);
		self.repaint();
	}

	/// Report an error from the audio thread, which must not wait for a lock.
	/// Lost if the status is busy.
	pub fn try_report_error(&self, error: String) {
		if let Ok(mut status) = self.status.try_lock() {
			status.error = Some(error);
			drop(status);
			if let Ok(notifiers) = self.notifiers.try_lock() {
				notifiers.iter().for_each(|(_, notifier)| notifier.request_repaint());
			}
		}
	}

	pub fn register(&self, notifier: RepaintNotifier) -> u64 {
		let id = self.next_id.fetch_add(1, Ordering::Relaxed);
		self.notifiers.lock().unwrap().push((id, notifier));
		id
	}

	pub fn unregister(&self, id: u64) {
		self.notifiers.lock().unwrap().retain(|(i, _)| *i != id);
	}

	fn repaint(&self) {
		self.notifiers.lock().unwrap().iter().for_each(|(_, notifier)| notifier.request_repaint());
	}
}
