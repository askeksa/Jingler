
import xml.dom
import xml.dom.minidom

class XML(object):
	def __init__(self, domlist):
		self.domlist = list(domlist)

	def __getattr__(self, name):
		l = []
		for d in self.domlist:
			for c in d.childNodes:
				if c.nodeName == name:
					l.append(c)
		return XML(l)

	def __len__(self):
		return len(self.domlist)

	def __getitem__(self, i):
		if i >= len(self.domlist):
			return XML([])
		return XML([self.domlist[i]])

	def __iter__(self):
		for d in self.domlist:
			yield XML([d])

	def __call__(self, attrname):
		s = ""
		for d in self.domlist:
			if d.nodeType == xml.dom.Node.ELEMENT_NODE and d.hasAttribute(attrname):
				s += d.getAttribute(attrname)
		return s

	def __str__(self):
		def collect(dl):
			s = ""
			for d in dl:
				if d.nodeType == xml.dom.Node.TEXT_NODE:
					s += d.data
				else:
					s += collect(d.childNodes)
			return s
		return collect(self.domlist)

	def __int__(self):
		return int(str(self))

	def __float__(self):
		return float(str(self))

	def __nonzero__(self):
		return len(self.domlist) != 0


def readXML(filename):
	return XML([xml.dom.minidom.parse(filename)])

def makeXML(xstring):
	return XML([xml.dom.minidom.parseString(xstring)])
