use super::*;

impl<Meta: PartialEq + std::fmt::Debug> Node<Meta> {
    pub fn verify(&self) {
        match &self.kind {
            NodeKind::Simple(simple_node_kind) => match simple_node_kind {
                SimpleNodeKind::Const(_) => {
                    expect_len(0, "regions", self.regions.len(), "const");
                    expect_len(0, "inputs", self.sig.inputs.len(), "const");
                }
                SimpleNodeKind::Unit => {
                    expect_len(0, "regions", self.regions.len(), "unit");
                    expect_len(0, "inputs", self.sig.inputs.len(), "unit");
                }
                SimpleNodeKind::Apply => {
                    expect_len(0, "regions", self.regions.len(), "apply");
                    assert!(
                        self.sig.inputs.len() > 0,
                        "apply must have at least lambda input"
                    );
                }
                SimpleNodeKind::Add => {
                    expect_len(0, "regions", self.regions.len(), "add");
                    expect_len(2, "inputs", self.sig.inputs.len(), "add");
                }
                SimpleNodeKind::Sub => {
                    expect_len(0, "regions", self.regions.len(), "sub");
                    expect_len(2, "inputs", self.sig.inputs.len(), "sub");
                }
            },
            NodeKind::Structural(structural_node_kind) => match structural_node_kind {
                StructuralNodeKind::Gamma => {}
                StructuralNodeKind::Theta => {}
                StructuralNodeKind::Lambda { .. } => {}
                StructuralNodeKind::Delta => expect_len(1, "regions", self.regions.len(), "delta"),
                StructuralNodeKind::Phi => {
                    self.regions.values().for_each(|region| {
                        assert!(
                            region.sig.arguments.len()
                                == self.sig.inputs.len() + self.regions.len()
                        );
                    });

                    self.sig
                        .outputs
                        .values()
                        .for_each(|v| assert!(matches!(v, Value::Lambda(..))));
                }
                StructuralNodeKind::Omega => {}
            },
        }

        self.regions.values().for_each(|region| {
            assert!(
                self.sig.inputs.len() >= region.sig.arguments.len(),
                "node input not propegated to region arguments"
            );

            region.verify()
        });
    }

    fn as_lambda(&self) {
        match self.kind {
            NodeKind::Structural(StructuralNodeKind::Lambda { dependencies }) => {
                // &self.regions[RegionKey(0)].sig
                // todo!();
            }
            _ => panic!("not a lambda"),
        }
    }
}

fn expect_len(exp: usize, kind_: &str, got: usize, for_: &str) {
    if got != exp {
        panic!("expected {exp} {kind_} (not {got}) for {for_}");
    }
}
