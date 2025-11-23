#!/usr/bin/env python3

import sys
import zipfile
import XML
import struct
import datetime

TOTAL_SEMITONES = 120
DEFAULT_SAMPLE_RATE = 44100

PARAMETER_QUANTIZATION_LEVELS = 16

class InputException(Exception):
	def __init__(self, message):
		Exception.__init__(self)
		self.message = message


class Instrument:
	def __init__(self, number, name, channel):
		self.number = number
		self.name = name
		self.channel = channel
		self.title = "%02X|%s" % (self.number, self.name)


class Note:
	NOTEBASES = {
		"C": 0, "D": 2, "E": 4, "F": 5, "G": 7, "A": 9, "B": 11
	}

	NOTENAMES = {
		0: "C-", 1: "C#", 2: "D-", 3: "D#", 4: "E-", 5: "F-",
		6: "F#", 7: "G-", 8: "G#", 9: "A-", 10: "A#", 11: "B-"
	}

	def __init__(self, tname, column, line, songpos, pat, patline, note, instr, velocity):
		note = str(note)
		self.column = column
		self.line = int(line)
		self.length = None
		self.songpos = int(songpos)
		self.pat = int(pat)
		self.patline = patline
		if note == "OFF":
			self.off = True
			self.tone = None
			self.instr = 0
			self.velocity = 0
		else:
			self.off = False
			octave = int(note[2])
			notebase = Note.NOTEBASES[note[0]]
			sharp = int(note[1] == "#")
			self.tone = octave * 12 + notebase + sharp
			self.instr = int(str(instr), 16)
			try:
				self.velocity = 127 if str(velocity) == "" or str(velocity) == ".." else int(str(velocity), 16)
			except ValueError:
				print("Track '%s' column %d pattern %d line %d: Illegal velocity value '%s'" % (tname, column, pat, patline, str(velocity)))
				self.velocity = 127

class AutomationPoint:
	def __init__(self, line, value):
		self.line = line
		self.value = value

	def __repr__(self):
		return "(%d, %d)" % (self.line, self.value)

def notename(tone):
	return Note.NOTENAMES[tone%12] + str(tone/12)


def isactive(xdevice):
	if not xdevice:
		return False
	if xdevice.IsActive.Value:
		return float(xdevice.IsActive.Value) != 0.0
	else:
		return str(xdevice.IsActive) == "true"


def f2i(f):
	return struct.unpack('I', struct.pack('f', f))[0]

def i2f(i):
	return struct.unpack('f', struct.pack('I', i))[0]


class Track:
	def __init__(self, number, name, instr, notes):
		self.number = number
		self.name = name
		self.instr = instr
		self.notes = notes
		self.note_lengths = dict()

		self.title = self.name

		self.labelname = ""
		for c in self.name:
			if c.isalnum() or c == '_':
				self.labelname += c

		self.latest_note = 0
		self.max_length = 0

		for note in notes:
			if not note.off:
				if note.instr is None:
					raise InputException("Track '%s' column %d pattern %d line %d: Undefined instrument" % (name, note.column, note.pat, note.patline));
				length = note.length
				if length is None:
					raise InputException("Track '%s' column %d pattern %d line %d: Note has no length" % (name, note.column, note.pat, note.patline))
				if note.tone is None:
					raise InputException("Track '%s' column %d pattern %d line %d: Toneless note" % (name, note.column, note.pat, note.patline))
				self.latest_note = max(self.latest_note, note.line)
				self.max_length = max(self.max_length, length)

				if length not in self.note_lengths:
					self.note_lengths[length] = 0
				self.note_lengths[length] += 1

		self.latest_note = 0

		if len(self.note_lengths) == 1:
			for l in self.note_lengths:
				self.singular_length = l if l <= 255 else None
		else:
			self.singular_length = None


class Music:
	def __init__(self, sample_rate, tracks, instruments, length, ticklength, autos, track_order):
		self.sample_rate = sample_rate
		self.tracks = tracks
		self.instruments = instruments
		self.length = length
		self.ticklength = ticklength
		self.autos = autos

		channel_map = [None for c in range(16)]
		for track in self.tracks:
			channel = instruments[track.instr].channel
			if channel_map[channel] is not None:
				raise InputException("Track '%s' uses same channel as track '%s'" % (track.name, channel_map[channel].name))
			channel_map[channel] = track

		self.track_order = [channel_map[i] for i in track_order]

		self.datainit = None
		self.out = None

	def dataline(self, data):
		if len(data) > 0:
			line = self.datainit
			first = True
			for d in data:
				if not first:
					line += ","
				line += str(d)
				first = False
			line += "\n"
			self.out += line

	def comment(self, c):
		self.out += "\t; %s\n" % c

	def label(self, l):
		self.out += "%s:\n" % l

	def dword(self, value):
		self.out += "\tdd\t%s\n" % str(value)

	def notelist(self, datafunc, trackterm, prefix):
		for i,track in enumerate(self.track_order):
			if track is not None:
				self.comment(track.title)
				self.label("%s%d_%s_%d" % (prefix, i, track.labelname, track.instr))
				prev_n = None
				pat_data = []
				track.latest_note = 0
				for n in track.notes:
					if not n.off if prev_n is None else n.songpos != prev_n.songpos:
						self.dataline(pat_data)
						pat_data = []
						self.comment("Position %d, pattern %d" % (n.songpos, n.pat))
					pat_data += datafunc(track, prev_n, n)
					prev_n = n
				self.dataline(pat_data)
			self.dataline(trackterm)
			self.out += "\n"

	def autolist(self, datafunc, trackterm):
		for i,points in enumerate(self.autos):
			self.comment("Parameter %d" % i)
			self.label(".p_%d" % i)
			data = []
			prev_p = None
			for p in points:
				data += datafunc(prev_p, p)
				prev_p = p
			self.dataline(data)
			self.dataline(trackterm)
			self.out += "\n"

	def export(self):
		self.datainit = "\tdb\t"
		self.out = ""

		spt = int(self.ticklength * self.sample_rate)
		total_samples = (self.length * self.ticklength) * self.sample_rate

		def roundup(v):
			return (int(v) & -0x10000) + 0x10000

		global infile
		self.out += "; Music converted from %s %s\n" % (infile, str(datetime.datetime.now())[:-7])
		self.out += "\n"
		self.out += "%%define SAMPLE_RATE %d\n" % self.sample_rate
		self.out += "\n"
		self.out += "%%define NUM_TRACKS %d\n" % len(self.track_order)
		self.out += "\n"
		self.out += "%%define MUSIC_LENGTH %d\n" % self.length
		self.out += "%%define TOTAL_SAMPLES %d\n" % roundup(total_samples)
		self.out += "\n"
		self.out += "%%define SAMPLES_PER_TICK %d\n" % spt
		self.out += "%%define TICKS_PER_SECOND %.9f\n" % (1.0 / self.ticklength)
		self.out += "\n"

		def encode_distance(dist):
			if dist < 128:
				return [dist]
			else:
				return [255 - (dist >> 8), dist & 255]

		def emit_velocity(track, prev_n, n):
			return [n.velocity] if not n.off else []

		def emit_key(track, prev_n, n):
			return [n.tone] if not n.off else []

		def emit_length(track, prev_n, n):
			return encode_distance(n.length) if not n.off else []

		def emit_distance(track, prev_n, n):
			if not n.off:
				dist = n.line - track.latest_note
				track.latest_note = n.line
				return encode_distance(dist)
			return []

		def auto_empty(prev_p, p):
			return []

		def auto_value(prev_p, p):
			return [p.value]

		def auto_distance(prev_p, p):
			prev_line = 0 if prev_p is None else prev_p.line
			return encode_distance(p.line - prev_line)

		# Velocities
		self.out += "Velocities:\n"
		self.dword(1)
		self.notelist(emit_velocity, [0x80], ".v_")
		self.autolist(auto_empty, [0x80])

		# Keys
		self.out += "Keys:\n"
		self.dword(1)
		self.notelist(emit_key, [0x80], ".k_")
		self.autolist(auto_value, [0x80])

		# Lengths
		self.out += "Lengths:\n"
		self.dword("SAMPLES_PER_TICK")
		self.notelist(emit_length, [0x80], ".l_")
		self.autolist(auto_empty, [0x80])

		# Distances
		self.out += "Distances:\n"
		self.dword("SAMPLES_PER_TICK")
		self.notelist(emit_distance, [0x80], ".d_")
		self.autolist(auto_distance, [0x80])

		return self.out

	def makeDeltas(self, init_delta, lines_per_beat):
		beats_per_line = 1.0/lines_per_beat
		deltas = []
		for t in self.tracks:
			tdeltas = []
			delta = init_delta
			note_i = 0
			for p in range(0, self.length):
				while t.notes[note_i].line <= p:
					if not t.notes[note_i].off:
						delta = p * beats_per_line
					note_i += 1
				tdeltas.append(delta)
			deltas.append(tdeltas)
		return deltas


def extractTrackNotes(xsong, tr, column):
	xsequence = xsong.PatternSequence.PatternSequence
	if not xsequence:
		xsequence = xsong.PatternSequence.SequenceEntries.SequenceEntry
	xpatterns = xsong.PatternPool.Patterns.Pattern
	tname = str(xsong.Tracks.SequencerTrack[tr].Name)

	notes = []

	pattern_top = 0
	prev_instr = None
	for posn,xseq in enumerate(xsequence):
		patn = int(xseq.Pattern)
		xpat = xpatterns[patn]
		nlines = int(xpat.NumberOfLines)
		if tr in [int(xmt) for xmt in xseq.MutedTracks.MutedTrack]:
			off = Note(tname, column, pattern_top, posn, patn, 0, "OFF", None, 127)
			notes.append(off)
		else:
			xtrack = xpat.Tracks.PatternTrack[tr]
			for xline in xtrack.Lines.Line:
				index = int(xline("index"))
				if index < nlines:
					line = pattern_top + index
					xcol = xline.NoteColumns.NoteColumn[column - 1]
					if xcol.Note and str(xcol.Note) != "---":
						instr = str(xcol.Instrument)
						if instr == ".." and str(xcol.Note) != "OFF":
							if prev_instr is None:
								raise InputException("Track '%s' column %d pattern %d line %d: Unspecified instrument" % (tname, column, patn, index))
							instr = prev_instr
						prev_instr = instr

						note = Note(tname, column, line, posn, patn, index, xcol.Note, instr, xcol.Volume)
						notes.append(note)

						if note.velocity > 128 and not note.off:
							raise InputException("Track '%s' column %d pattern %d line %d: Illegal velocity value" % (tname, column, patn, index))
						if note.velocity == 0:
							note.off = True

					# Check for illegal uses of panning, delay and effect columns
					def checkColumn(x, allow_zero, msg):
						if x and not (str(x) == "" or str(x) == ".." or (allow_zero and str(x) == "00")):
							raise InputException("Track '%s' column %d pattern %d line %d: %s" % (tname, column, patn, index, msg))
					checkColumn(xcol.Panning, False, "Panning column used")
					checkColumn(xcol.Delay, True, "Delay column used")
					for xeff in xline.EffectColumns.EffectColumn.Number:
						checkColumn(xeff, True, "Effect column used")
		pattern_top += nlines

	# Add inital OFF and remove redundant OFFs
	if len(notes) > 0 and notes[0].line == 0:
		notes2 = []
		off = False
	else:
		notes2 = [Note(tname, column, 0, 0, int(xsequence[0].Pattern), 0, "OFF", 0, 127)]
		off = True
	for n in notes:
		if n.off:
			if not off:
				notes2.append(n)
				off = True
		else:
			notes2.append(n)
			off = False

	last = notes2[-1]
	if not last.off:
		raise InputException("Track '%s' column %d pattern %d line %d: Note not terminated (insert OFF)" % (tname, column, last.pat, last.patline))

	return notes2

def extractAutomation(xsong, id):
	xsequence = xsong.PatternSequence.PatternSequence
	if not xsequence:
		xsequence = xsong.PatternSequence.SequenceEntries.SequenceEntry
	xpatterns = xsong.PatternPool.Patterns.Pattern

	points = []
	line = 0

	def add(pline, pvalue):
		if len(points) == 0 or pline != points[-1].line or pvalue != points[-1].value:
			if len(points) >= 2 and points[-2].value == pvalue and points[-1].value == pvalue:
				points[-1].line = pline
			else:
				points.append(AutomationPoint(pline, pvalue))

	def flush():
		if len(points) >= 1 and points[-1].line < line:
			add(line, points[-1].value)

	for posn,xseq in enumerate(xsequence):
		patn = int(xseq.Pattern)
		xpat = xpatterns[patn]
		nlines = int(xpat.NumberOfLines)

		for xtrack in xpat.Tracks.PatternTrack:
			for xenvelope in xtrack.Automations.Envelopes.Envelope:
				if int(xenvelope.ParameterIndex) == id:
					length = int(xenvelope.Envelope.Length)
					first = True
					for xpoint in xenvelope.Envelope.Points.Point:
						x,y = tuple(float(v) for v in str(xpoint).split(",")[:2])
						pline = line + int(round(x * nlines) / length)
						pvalue = int(round(y * PARAMETER_QUANTIZATION_LEVELS))
						if first and pline > line:
							add(line, pvalue)
						add(pline, pvalue)
						first = False
		line += nlines
		flush()

	return points

def filterTrackNotes(tname, notes, instr):
	notes2 = []
	off = False
	for n in notes:
		if n.off or n.instr != instr:
			if not off:
				notes2.append(Note(tname, n.column, n.line, n.songpos, n.pat, n.patline, "OFF", 0, 127))
				off = True
		else:
			notes2.append(n)
			off = False

	return notes2

def makeTracks(xsong, ticklength):
	instruments = []
	tracks = []

	for ii,xinst in enumerate(xsong.Instruments.Instrument):
		channel = int(xinst.PluginGenerator.Channel)
		instrument = Instrument(ii, str(xinst.Name), channel)
		instruments.append(instrument)

	for xgrouptrack in xsong.Tracks.SequencerGroupTrack:
		tname = xgrouptrack.Name
		xdevices = xgrouptrack.FilterDevices.Devices
		for xmixer in xdevices.GroupTrackMixerDevice:
			if isactive(xmixer):
				if float(xmixer.Volume.Value) != 1.0 or float(xmixer.PostVolume.Value) != 1.0:
					raise InputException("Group track '%s' has non-zero volume" % tname);
				if float(xmixer.Panning.Value) != 0.5 or float(xmixer.PostPanning.Value) != 0.5:
					raise InputException("Group track '%s' has non-center panning" % tname);
		if isactive(xdevices.SendDevice):
			raise InputException("Group track '%s' uses Send" % tname);

	for tr,xtrack in enumerate(xsong.Tracks.SequencerTrack):
		if str(xtrack.State) != "Active":
			continue

		tname = str(xtrack.Name)
		ncols = int(xtrack.NumberOfVisibleNoteColumns)
		xdevices = xtrack.FilterDevices.Devices
		xdevice = xdevices.SequencerTrackDevice
		if not xdevice:
			xdevice = xdevices.TrackMixerDevice
		while isactive(xdevices.SendDevice):
			raise InputException("Track '%s' uses Send" % tname);

		notes = []
		track_instrs = []
		for column in range(1, ncols + 1):
			if str(xtrack.NoteColumnStates.NoteColumnState[column - 1]) != "Active":
				continue

			notes += extractTrackNotes(xsong, tr, column)

			for note in notes:
				if not note.off:
					instr = instruments[note.instr]
					if instr is None:
						raise InputException("Track '%s' column %d pattern %d line %d: Undefined instrument (%d)" % (tname, column, note.pat, note.patline, note.instr));
					if note.instr not in track_instrs:
						track_instrs.append(note.instr)

		for instr in track_instrs:
			instr_notes = filterTrackNotes(tname, notes, instr)
			computeNoteLengths(instr_notes)
			instr_notes = sorted(instr_notes, key=lambda n: n.line)
			track = Track(tr, tname, instr, instr_notes)
			tracks.append(track)

	return tracks, instruments

def computeNoteLengths(notes):
	prev = None
	for note in notes:
		if prev is not None and not prev.off:
			if note.line == prev.line:
				raise InputException("Track '%s' column %d pattern %d line %d: Zero length" % (tname, column, note.pat, note.patline));
			prev.length = note.line - prev.line
		prev = note

def makeMusic(xsong, sample_rate, track_order, num_parameters):
	xgsd = xsong.GlobalSongData
	if xgsd.PlaybackEngineVersion and int(xgsd.PlaybackEngineVersion) >= 4:
		lines_per_minute = float(xgsd.BeatsPerMin) * float(xgsd.LinesPerBeat)
		print("New timing format: %d ticks per minute" % lines_per_minute)
	else:
		lines_per_minute = float(xgsd.BeatsPerMin) * 24.0 / float(xgsd.TicksPerLine)
		print("Old timing format: %d ticks per minute" % lines_per_minute)
	ticklength = 60.0 / lines_per_minute
	print()

	tracks,instruments = makeTracks(xsong, ticklength)

	autos = []
	for i in range(num_parameters):
		points = extractAutomation(xsong, i+1)
		autos.append(points)

	xpositions = xsong.PatternSequence.PatternSequence.Pattern
	if not xpositions:
		xpositions = xsong.PatternSequence.SequenceEntries.SequenceEntry.Pattern
	xpatterns = xsong.PatternPool.Patterns.Pattern
	length = 0
	for xpos in xpositions:
		patn = int(xpos)
		xpat = xpatterns[patn]
		nlines = int(xpat.NumberOfLines)
		length += nlines

	return Music(sample_rate, tracks, instruments, length, ticklength, autos, track_order)


def printMusicStats(music, ansi):
	def form(s):
		f = ""
		for c in s:
			if c == 'T':
				f += "\033[33m%s\033[0m" if ansi else "%s"
			elif c == 'H':
				f += "\033[32m%s\033[0m" if ansi else "%s"
			elif c == 'L':
				f += "\033[36m%d\033[0m" if ansi else "%d"
			elif c == 'N':
				f += ":%d" if ansi else ":%d"
			else:
				f += c
		return f

	print("Music length: %d ticks at %0.2f ticks per minute" % (music.length, 60.0 / music.ticklength))

	ii = None
	for ti,track in enumerate(music.tracks):
		if track.instr != ii:
			print()
			ii = track.instr
			instr = music.instruments[ii]
			print(form("T") % instr.title)

		print(form(" H") % track.title)

		lengths = ""
		for l in sorted(track.note_lengths.keys()):
			lengths += form(" LN") % (l, track.note_lengths[l])
		print("  Lengths:  " + lengths)


def writefile(filename, s):
	f = open(filename, "wb")
	f.write(s.encode("utf-8"))
	f.close()
	print("Wrote file %s" % filename)


if __name__ == "__main__": 
	ansi = False
	args = []
	sample_rate = DEFAULT_SAMPLE_RATE
	sample_rate_option = False
	for a in sys.argv[1:]:
		if a == "-ansi":
			ansi = True
		elif a == "--samplerate" or a == "-s":
			sample_rate_option = True
		elif sample_rate_option:
			sample_rate = int(a)
			sample_rate_option = False
		else:
			args.append(a)

	if len(args) < 4:
		print("Usage: %s [-ansi] [--samplerate | -s <sample rate>] <input xrns file> <output asm file> <num parameters> <midi channel for each track>" % sys.argv[0])
		sys.exit(1)

	infile = args[0]
	outfile = args[1]
	num_parameters = int(args[2])
	track_order = [int(a) for a in args[3:]]

	x = XML.makeXML(zipfile.ZipFile(infile).read("Song.xml"))
	try:
		music = makeMusic(x.RenoiseSong, sample_rate, track_order, num_parameters)
		print()
		printMusicStats(music, ansi)
		print()

		writefile(outfile, music.export())

	except InputException as e:
		print("Error in input song: %s" % e.message)
