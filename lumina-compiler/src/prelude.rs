#![allow(unused)]

pub use crate::{ast, hir, lir, mir};
pub use derive_more::Constructor;
pub use derive_new::new;
pub use itertools::Itertools;
pub use lumina_key as key;
pub use lumina_key::{EntityRef, IterMapCollect, LinearFind, Map, MapExt, ModMap, M};
pub use lumina_util::{Span, Spanned, Tr};
pub use std::collections::HashMap;
use std::sync::Once;
pub use tracing::{error, info, trace, warn};
use tracing_subscriber::{
    filter::LevelFilter, layer::SubscriberExt, registry::Registry, EnvFilter,
};

#[cfg(test)]
static TRACING_INIT: Once = Once::new();

#[cfg(test)]
pub fn test_logger() {
    TRACING_INIT.call_once(|| {
        let filter = EnvFilter::from_default_env();

        let layer = tracing_tree::HierarchicalLayer::default()
            .with_writer(std::io::stdout)
            .with_indent_lines(true)
            .with_indent_amount(2)
            .with_verbose_entry(false)
            .with_verbose_exit(false)
            .with_targets(true);

        let subscriber = Registry::default().with(layer).with(filter);

        tracing::subscriber::set_global_default(subscriber).unwrap();
    });
}
