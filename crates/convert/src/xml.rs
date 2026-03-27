use roxmltree::Node;

pub struct XmlDocument<'input> {
	doc: roxmltree::Document<'input>,
}

pub struct XmlNode<'a, 'input: 'a> {
	nodes: Vec<Node<'a, 'input>>,
}

impl<'input> XmlDocument<'input> {
	pub fn parse(input: &'input str) -> Result<Self, impl std::error::Error> {
		let doc = roxmltree::Document::parse(input)?;
		Ok::<XmlDocument<'_>, roxmltree::Error>(Self { doc })
	}

	pub fn root(&self) -> XmlNode<'_, 'input> {
		XmlNode::wrap(self.doc.root())
	}
}

impl<'a, 'input: 'a> XmlNode<'a, 'input> {
	fn new(nodes: Vec<Node<'a, 'input>>) -> Self {
		Self { nodes }
	}

	fn wrap(node: Node<'a, 'input>) -> Self {
		Self::new(vec![node])
	}

	pub fn at(&self, index: usize) -> Self {
		Self::wrap(self.nodes[index])
	}

	pub fn child(&self, name: &str) -> Self {
		let mut children = Vec::new();
		for node in &self.nodes {
			for child in node.children() {
				if child.has_tag_name(name) {
					children.push(child);
				}
			}
		}
		Self::new(children)
	}

	pub fn attr(&self, name: &str) -> String {
		let mut s = String::new();
		for node in &self.nodes {
			if let Some(val) = node.attribute(name) {
				s.push_str(val);
			}
		}
		s
	}

	pub fn text(&self) -> String {
		let mut s = String::new();
		for node in &self.nodes {
			if let Some(text) = node.text() {
				s.push_str(text);
			}
		}
		s
	}

	pub fn is_empty(&self) -> bool {
		self.nodes.is_empty()
	}

	pub fn len(&self) -> usize {
		self.nodes.len()
	}

	pub fn iter(&self) -> impl Iterator<Item = Self> + '_ {
		self.nodes.iter().map(|n| Self::wrap(*n))
	}
}
