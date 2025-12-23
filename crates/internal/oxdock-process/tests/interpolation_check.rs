#[cfg(test)]
mod tests {
    use oxdock_process::expand_command_env;
    use oxdock_process::CommandContext;
    use oxdock_fs::GuardedPath;
    use std::collections::HashMap;
    use std::path::PathBuf;

    #[test]
    fn test_aggressive_brace_interpolation() {
        let root = PathBuf::from("/tmp");
        let cwd = GuardedPath::new(&root, &root).unwrap();
        let envs = HashMap::new();
        let policy_cwd: oxdock_fs::PolicyPath = cwd.clone().into();
        let ctx = CommandContext::new(
            &policy_cwd,
            &envs,
            &cwd,
            &cwd,
            &cwd,
        );

        // If I have a JSON-like string
        let input = r#"{ "foo": "bar" }"#;
        // And no env vars match " \"foo\": \"bar\" " (which is unlikely to be a valid var name anyway, but let's see)
        
        // Let's try a simpler one that looks like a var
        let input_simple = "Values: {PATH}";
        
        // PATH is likely in the host env
        let expanded = expand_command_env(input_simple, &ctx);
        println!("Input: {}", input_simple);
        println!("Expanded: {}", expanded);
        
        // {VAR} syntax is removed, so it should NOT expand
        assert_eq!(input_simple, expanded, "{{VAR}} syntax should no longer be active");

        // Verify ${env:PATH} does NOT work
        let input_namespaced = "Values: ${env:PATH}";
        let expanded_namespaced = expand_command_env(input_namespaced, &ctx);
        assert_eq!(input_namespaced, expanded_namespaced, "${{env:VAR}} syntax should NOT be active");

        // Verify {{ env.PATH }} works
        let input_new = "Values: {{ env.PATH }}";
        let expanded_new = expand_command_env(input_new, &ctx);
        assert_ne!(input_new, expanded_new, "{{ env.VAR }} syntax should be active");
    }
}
