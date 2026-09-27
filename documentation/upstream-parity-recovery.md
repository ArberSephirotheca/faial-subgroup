# Upstream Parity Recovery

Verified on 2026-09-11 in `integration/subgroup-upstream-20260911`, based on
`b9b50da8`. This supersedes the incomplete replay in
`upstream-port-verification.md`. Changes are in the isolated port and translator
worktrees; the original Faial-subgroup checkout and the paper are unchanged.

## Recovered Component Verdicts

The replay compares each manifest entry, not just aggregate totals, against
`llama.cpp-eval/verification/memory-ordering-fix-2026-09-10/after`.

| Subset | Entries | Conclusive | Matching memory and participation verdicts |
| --- | ---: | ---: | ---: |
| Previously warp-analyzed entries | 98 | 98 | 98 |
| Paper subset | 95 | 95 | 95 |

The 98 entries have 68 passing both components, 29 reporting only memory
alarms, and one reporting both kinds of alarm. The paper subset has 65, 29,
and one, respectively. Neither subset has a timeout or rejection. The extra
three entries are RMS-normalization launches, all passing both components.

These are component-verdict matches, **not identical alarm inventories**.
The paper subset now has 62 memory alarms and three participation alarms,
versus 67 and three in the archive. The site audit retains 55 memory access
pairs, removes 12, and adds seven:

- The 12 removed pairs are four self-write pairs per non-scatter Q8 launch
  (`quantize_588`, `quantize_592`, and `quantize_596`), at lines 542, 544,
  549, and 551 of `quantize.cu`. The updated lowering retains the enclosing
  array index of anonymous-union fields instead of an anonymous temporary.
- Seven added pairs are in Q8 and MXFP4 quantization. Four are MXFP4 writes
  at lines 434, 435, 446, and 447; three pair a Q8 access at line 537 with
  a metadata write. The supplementary complete-memory check can report
  additional potential races; these are not newly validated defects.
- All archived memory alarm pairs outside those Q8 entries are retained,
  including the MMVF, group-normalization, ID-compaction, and MoE pairs.
  All three participation alarm sites are retained.

Do not replace the paper's alarm/category table with this run without
revalidating the changed quantization pairs.

## Repairs

- Compile only the selected entry and its reachable helper definitions.
  This removes repeated work over unrelated functions without dropping callees.
- Build the current translator with project-specific dumper class names.
  Its old private names collided with Clang's classes under LLVM 20.
  Instantiate referenced, concrete CUDA device-template definitions whose
  bodies Clang otherwise defers.
- Parse `CXXDefaultInitExpr`; retain record offsets, cast views, single-field
  scalar records, and anonymous-union members. Overlapping members share a
  backing allocation rather than becoming unrelated arrays.
- Resolve helpers by declaration identity even when qualified template
  arguments have different textual spellings. Preserve guarded pointer aliases
  using semantic expression equality rather than source-location equality.
- Lower exact-size `memcpy` between private scalars as an unknown scalar value.
  Shared, pointer, and oversized copies are not silently ignored.
- Supplement source-site memory checks when the complete protocol contains
  missing helper accesses or overlapping views. Existing source-site alarms
  remain in the report. This conservative check may lose warp-only ordering;
  it does not establish precision parity for every pointer/helper pattern.
- Normalize sequences before lowering local assertions. A fast-division
  rewrite can prepend a divisor bound to an early-return guard; previously
  the resulting nested sequence dropped that guard. A unit regression and a
  CUDA regression reproduce the false race before the fix. With the guard
  retained, `quantize_q8_1@quantize_571` again passes both components.

No matrix-memory feature or blanket exemption for unknown calls was added.

## Replay Compatibility

`verification/upstream-replay-adaptations.json` records the four MMVF selector
renames, checked against their source launches and template arguments. The
replay expands legacy source-family assumptions into upstream's exact
`kernel=NAME:` scopes. It removes the now-eliminated `warp_size==16` term for
one gated-delta launch only because the retained `S_v==16` and CUDA width 32
imply that local constant. No new caller preconditions were invented.

The full 479-entry manifest still needs migration to upstream's changed launch
identities and deduplication. Missing selectors, zero-access results, vacuous
results, errors, rejections, and timeouts are not DRF passes. Recovery of the
98-entry subset must not be described as whole-corpus parity.

The completed 479-entry replay reports 85 ordinary DRF results, 37 ordinary
race results, 31 zero-access results, one vacuous result, 198 missing selectors,
seven unnamed-region rejections, ten errors, and 11 timeouts. Its 99 subgroup
results comprise 68 passing both components, 30 with memory alarms only, and
one with both kinds of alarm. The additional subgroup-routed entry is
`reduce_rows_f32`; it is not added to the archived 98-entry comparison or the
95-entry paper subset. The full replay also reproduces every component verdict
in those two fixed subsets.

## Tests

| Check | Result |
| --- | --- |
| `dune build @all` | Pass |
| Unit tests | 1,040 pass; 23 optional historical-manifest cases skipped |
| Existing DRF CLI cases | 440/443 pass |
| Five added CLI cases | 5/5 pass |
| Translator pytest suite | 156/161 pass |

The three remaining CLI failures have the same exit codes on unmodified
upstream: `drf-memcpy-extent.cu`, `racy-funcion-call-unknowns.cu`, and the
`undefined-call.cu --opaque-calls=skip-all` sibling case. The five remaining
translator failures concern record/typedef and dependent-type metadata
expectations. They remain failures, not waived passes. See the saved test logs.

The added regressions cover helper-memory completeness, overlapping union
members, record casts, private scalar copies, and the fast-division guard.
A separate exploratory direct-shift subgroup case remains unsupported; it is
not counted as a passing registered test.

## Reproduce

Use the patched translator in `artifact/c-to-json-upstream-port` and the replay
wrapper/headers in `artifact/toolchain-parity`. The original installed
translator and its headers were not overwritten.

From the port worktree:

```sh
opam exec --switch=../faial-subgroup -- dune build @all
```

From `artifact/llama.cpp-eval`, choosing a new output directory:

```sh
.venv/bin/python verification/check_upstream_port.py \
  --binary ../faial-subgroup-upstream-port/_build/default/drf/bin/main.exe \
  --baseline verification/memory-ordering-fix-2026-09-10/after \
  --translator ../toolchain-parity/bin/cu-to-json \
  --adaptations verification/upstream-replay-adaptations.json \
  --only-warp --workers 4 --out verification/parity-replay
```

Authoritative recovered results are in `verification/parity-recovered-warp`
and `verification/parity-recovered-full`, with executable snapshots, raw JSON,
provenance hashes, and component comparisons. The former also contains
`site-audit.json`, generated by `verification/audit_upstream_parity.py`.
`verification/parity-recovered-current` repeats all 98 entries against the
current build after test-registration cleanup and confirms the same result.
Only that test-registration change, not analyzer code, separates its source
tree from the full-sweep build.
`verification/parity-final-tests` contains test logs. Earlier directories named
`parity-final-*` are intermediate runs, before the early-return-guard repair.
