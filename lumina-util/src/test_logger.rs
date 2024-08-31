use std::sync::Once;
use tracing_subscriber::{layer::SubscriberExt, registry::Registry, EnvFilter};

static TRACING_INIT: Once = Once::new();

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
