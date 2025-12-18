#!/usr/bin/env bash
# This helper is invoked from GitHub Actions only; it inspects Miri test coverage
# and writes badge artifacts for the README. Do not run manually in production workflows.
set -euo pipefail

: "${BASE_LINE_COVERAGE:=0}"
MIRI_TEST_CMD=${MIRI_TEST_CMD:-"cargo miri test --workspace --all-features --lib --tests"}

run_listing() {
  bash -c "$MIRI_TEST_CMD $1" | grep -c ': test' || true
}

total=$(run_listing "-- --list --format terse")
ignored=$(run_listing "-- --ignored --list --format terse")
run_cnt=$((total - ignored))

runnable_ratio_value="0.0"
ratio_label="n/a"
if [ "$total" -gt 0 ]; then
  runnable_ratio_value=$(awk -v r="$run_cnt" -v t="$total" 'BEGIN { printf "%.1f", (r / t) * 100 }')
  ratio_label="${runnable_ratio_value}%"
fi

percent_int=$(printf "%.0f" "$runnable_ratio_value")
if [ "$percent_int" -ge 90 ]; then
  color="brightgreen"
elif [ "$percent_int" -ge 75 ]; then
  color="green"
elif [ "$percent_int" -ge 50 ]; then
  color="yellowgreen"
elif [ "$percent_int" -gt 0 ]; then
  color="orange"
else
  color="lightgrey"
fi

base_cov="${BASE_LINE_COVERAGE:-0}"
if [ -z "$base_cov" ]; then
  base_cov="0"
fi

effective_cov=$(awk -v base="$base_cov" -v ratio="$runnable_ratio_value" 'BEGIN { printf "%.1f", (base * ratio) / 100 }')
effective_label="${effective_cov}%"

mkdir -p badges
{
  printf "Miri runnable tests: %s/%s\n" "$run_cnt" "$total"
  printf "Runnable ratio: %s\n" "$ratio_label"
  printf "LLVM line coverage baseline: %s%%\n" "$base_cov"
  printf "Effective Miri coverage: %s\n" "$effective_label"
} | tee miri-summary.txt
printf "effective_coverage=%s\n" "${effective_label}" | tee miri-output.env
printf '{"schemaVersion":1,"label":"miri coverage","message":"%s","color":"%s"}\n' "${effective_label}" "${color}" | tee badges/miri-coverage.json

if [ -n "${GITHUB_OUTPUT:-}" ]; then
  cat miri-output.env >> "$GITHUB_OUTPUT"
fi

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Miri coverage"
    echo
    cat miri-summary.txt
  } >> "$GITHUB_STEP_SUMMARY"
else
  cat miri-summary.txt
fi
