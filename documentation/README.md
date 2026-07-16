# Faial Documentation

The directory holds conceptual notes for Faial implementation work. The notes
explain analysis intent and formal boundaries rather than command-line usage.

Current notes:

- `ggml-cuda-alarm-investigation.md` classifies the remaining subgroup
  campaign alarms, gives concrete source-level counterexamples, proves the
  false alarms, and distinguishes the subgroup ordering assumption from the
  strict CUDA portability boundary exposed by `mm_ids_helper`.
- `subgroup-map-formalization.md` formalizes the local subgroup/matrix DRF
  extension in MAP-style notation and compares it with the supplied FaialAA
  paper.
- `symbolic-metric-analysis.md` records the symbolic metric-analysis work log.
- `menhir-parsing-setup.md` documents parser setup.
- `august-2025-work-log.md` records historical implementation notes.
- `guidelines.md` defines documentation-writing conventions for this project.
