fn main() {
    oxdock_buildtime_helpers::emit_feature_and_cfg_envs()
        .expect("emit feature/cfg envs");
}
