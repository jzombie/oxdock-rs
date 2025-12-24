use oxdock_parser::ast::{Guard, Step, StepKind};
use oxdock_parser::parse_script;

#[test]
fn env_equals_display_prefers_not_equals() {
    let guard = Guard::EnvEquals {
        key: "A".into(),
        value: "1".into(),
        invert: true,
    };

    assert_eq!(guard.to_string(), "env:A!=1");

    let step = Step {
        guards: vec![vec![guard.clone()]],
        kind: StepKind::Workdir("a".into()),
        scope_enter: 0,
        scope_exit: 0,
    };

    let rendered = step.to_string();
    assert_eq!(rendered, "[env:A!=1] WORKDIR a");

    let parsed = parse_script(&rendered).expect("parse");
    assert_eq!(parsed, vec![step]);
}
