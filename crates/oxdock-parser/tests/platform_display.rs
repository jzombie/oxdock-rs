use oxdock_parser::ast::{Guard, PlatformGuard, Step, StepKind};
use oxdock_parser::parse_script;

#[test]
fn platform_guard_display_uses_equals() {
    let guard = Guard::Platform {
        target: PlatformGuard::Unix,
        invert: false,
    };

    assert_eq!(guard.to_string(), "platform:unix");

    let step = Step {
        guards: vec![vec![guard]],
        kind: StepKind::Workdir("a".into()),
        scope_enter: 0,
        scope_exit: 0,
    };

    let rendered = step.to_string();
    assert_eq!(rendered, "[platform:unix] WORKDIR a");

    let parsed = parse_script(&rendered).expect("parse");
    assert_eq!(parsed, vec![step]);
}
