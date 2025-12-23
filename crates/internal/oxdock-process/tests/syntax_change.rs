#[cfg(test)]
mod tests {
    use oxdock_process::expand_command_env;
    use oxdock_process::CommandContext;
    use oxdock_fs::GuardedPath;
    use oxdock_fs::PolicyPath;
    use std::collections::HashMap;
    use std::path::PathBuf;

    #[test]
    fn test_double_brace_syntax() {
        let root = PathBuf::from("/tmp");
        let cwd = GuardedPath::new(&root, &root).unwrap();
        let mut envs = HashMap::new();
        envs.insert("FOO".into(), "bar".into());
        
        let policy_cwd: PolicyPath = cwd.clone().into();
        let ctx = CommandContext::new(
            &policy_cwd,
            &envs,
            &cwd,
            &cwd,
            &cwd,
        );

        // Test new syntax
        let input = "Value: {{ env.FOO }}";
        let expanded = expand_command_env(input, &ctx);
        assert_eq!(expanded, "Value: bar", "Should expand {{ env.FOO }}");

        // Test whitespace handling
        let input_tight = "Value: {{env.FOO}}";
        let expanded_tight = expand_command_env(input_tight, &ctx);
        assert_eq!(expanded_tight, "Value: bar", "Should expand {{env.FOO}}");

        // Test legacy syntax (should NOT work)
        let input_legacy = "Value: ${env:FOO}";
        let expanded_legacy = expand_command_env(input_legacy, &ctx);
        assert_eq!(expanded_legacy, "Value: ${env:FOO}", "Should NOT expand ${{env:FOO}}");
        
        // Test shell conflict
        let input_shell = "Value: ${FOO}";
        let expanded_shell = expand_command_env(input_shell, &ctx);
        assert_eq!(expanded_shell, "Value: ${FOO}", "Should NOT expand ${{FOO}}");
    }
}
