# Faial: finds bugs in CUDA kernels

Faial is a static analysis tool for finding bugs in CUDA kernels, featuring **data-race freedom (DRF) analysis** and **performance analysis**.

# Quick start with Docker

Run Faial instantly using Docker, no installation required:

```bash
$ docker run --rm -v $(pwd):/workspace registry.gitlab.com/umb-svl/faial/faial-tools:latest faial-drf ./tutorial/saxpy.cu
Kernel 'saxpy' is DRF!
```

# Binary distribution

### [Download `faial` for Linux x86-64bits](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle-lin&inline=false)
### [Download `faial` for macOS arm64 (M1/M2/M3)](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle-mac&inline=false)

Instructions:
1. Create a directory to hold the binary distribution, say `/opt/faial`
2. Extract the binary distribution archive `faial.tar.bz2`
3. (Optional) Add `/opt/faial` to the `PATH`, or run directly `/opt/faial/faial-drf`

# [Usage and tutorial](tutorial/README.md)

Ensure that your CUDA file has all of its includes available and then run:

```bash
$ faial-drf example.cu
```

Next, feel free to access the [`tutorial/`](tutorial/) directory!

# Focused subgroup/matrix DRF extension

The local W509+ residual-closure campaign continues the OCaml-owned
subgroup/matrix path for the focused `flash_attn_wmma_mirror_kernel` fixture.
This is not arbitrary CUDA or arbitrary Flash Attention support, and it is not
a wrapper around the Rust oracle.

The subgroup path is enabled explicitly:

```bash
opam exec -- dune exec drf/bin/main.exe -- \
  --cu-to-json=./bin/cu-to-json \
  -DFAIAL_CAPTURE \
  '-D__float2half_rn(x)=((half)(x))' \
  --ignore-asserts \
  -t 1000 \
  --kernel flash_attn_wmma_mirror_kernel \
  --block-dim 64 \
  --subgroup-size 32 \
  --find-true-dr \
  ../flash_attn_wgsl_matrix_mirror.cu
```

Current OCaml outcome for that focused command at the historical 1000ms
per-obligation solver budget is DRF:

```text
mem_drf: drf
memory_checks: 28 total, 0 racy, 0 unknown, 0 timeout, 0 unsupported, 8 pre_solver_unsat
subgroup_uniformity: drf
drf_full: drf
```

The eight structural discharges are the focused solver-budget residuals:
`o_shmem` obligations `#16` and `#17` discharge by guarded WMMA tile
row/lane ownership, while `dst` cross-component obligations `#19`, `#20`,
`#21`, `#23`, `#24`, and `#26` discharge by guarded hierarchical subgroup
row/lane-vector ownership. Same-component `dst` pairs remain solver-visible
and classify as `solver=unsat(drf)`.

The previous focused OCaml boundary was solver-budget sensitivity, not a SAT
race model: before the guarded pre-solver rules, `o_shmem` obligations `#16`
and `#17` and `dst` obligation `#26` needed a larger Z3 budget. The current
1000ms result closes that focused residual without claiming arbitrary CUDA,
arbitrary Flash Attention support, or focused Rust/OCaml structured parity.

```text
solver_config: logic=default timeout_ms=1000
```

The live Rust oracle for the same source and configuration still reports
`mem_drf: drf`, `subgroup_uniformity: drf`, `drf_full: drf`, and
`memory_checks: 774 total, 0 racy`. This is still not a structured Rust/OCaml
source/site/effect/obligation memory-parity claim, because Rust does not yet
expose an equivalent subgroup-lowered structured artifact for this focused
command.

Ordinary shared/global source memory effects in subgroup/matrix kernels are
now modeled as subgroup-aware memory obligations. Existing non-WMMA examples
remain on the legacy `Imp` path, preserve legacy DRF/racy verdicts, and do not
require `--subgroup-size`. The legacy gate for this boundary is
`opam exec -- make`, `PATH="$(pwd)/bin:$PATH" opam exec -- dune runtest`, and
no-`--subgroup-size` smokes for both DRF and racy examples under
`examples/drf/`.

See [`inference/README.md`](inference/README.md) for source-to-subgroup
dispatch and [`drf/README.md`](drf/README.md) for the DRF verdict boundary.

# Build from source

## Dependencies

* [opam `>= 2.0`](https://opam.ocaml.org/)
* [ocamlc `>= 5.1`](https://ocaml.org/)

### 1. Setup

**Run this once.** The following command will install all dependencies needed to build the project.

```bash
./configure.sh
```

> [!NOTE]
> For an isolated environment, use `./configure.sh --create-switch` to create a local opam switch.

### 2. Build

**Run this to build the binary.**

```bash
$ make
```

# Citing our research

If you use Faial in your research, please cite our paper:

> Tiago Cogumbreiro, Julien Lange, Dennis Liew, and Hannah Zicarelli. "Memory access protocols: certified data-race freedom for GPU kernels." *Formal Methods in System Design* 63, no. 1 (2024): 134-171. DOI: [10.1007/s10703-023-00415-0](https://doi.org/10.1007/s10703-023-00415-0)

**BibTeX:**
```bibtex
@article{faial:fmsd23,
  title={Memory access protocols: certified data-race freedom for GPU kernels},
  author={Cogumbreiro, Tiago and Lange, Julien and Liew, Dennis and Zicarelli, Hannah},
  journal={Formal Methods in System Design},
  volume={63},
  number={1},
  pages={134--171},
  year={2024},
  publisher={Springer},
  doi={10.1007/s10703-023-00415-0},
}
```
# Publications

- [A Modular Static Cost Analysis for GPU Warp-Level Parallelism](https://dx.doi.org/10.1145/3776693). Gregory Blike, Hannah Zicarelli, Udaya Sathiyamoorthy, Julien Lange, Tiago Cogumbreiro. PACMPL, 10(POPL), 2026.

- [Hidden assumptions in static verification of data-race free GPU programs](https://dx.doi.org/10.1007/978-3-031-97492-2_6). In Principles and Practices of Building Parallel Software. LNCS, vol 14564. Tiago Cogumbreiro, Julien Lange. 2025

- [Sound and partially-complete static analysis of data-races in GPU programs](https://dx.doi.org/10.1145/3689797). Dennis Liew, Tiago Cogumbreiro, Julien Lange. PACMPL, 8(OOPSLA2), 2024.

- [Memory Access Protocols: Certified Data-Race Freedom for GPU Kernels](https://dx.doi.org/10.1007/s10703-023-00415-0). Tiago Cogumbreiro, Julien Lange, Dennis Liew, Hannah Zicarelli. FMSD, 2023.

- [Provable GPU Data-Races in Static Race Detection](https://dx.doi.org/10.4204/EPTCS.356.4). Dennis Liew, Tiago Cogumbreiro, Julien Lange. In PLACES, volume 356 of EPTCS, page 36–45. 2022.

- [Checking Data-Race Freedom of GPU Kernels, Compositionally](https://dx.doi.org/10.1007/978-3-030-81685-8_19). Tiago Cogumbreiro, Julien Lange, Dennis Lew, Hannah Zicarelli. In CAV, volume 12759, page 403–426. Springer, 2021.

# Contributors

Thanks to all contributors who have helped improve Faial:

- [Tiago Cogumbreiro](https://gitlab.com/cogumbreiro)
- [Dennis Liew](https://gitlab.com/dennisliew11)
- [Hannah Zicarelli](https://gitlab.com/hzicarelli)
- [Gregory Blike](https://gitlab.com/gblike)
- [Miguel Cardenas](https://gitlab.com/miguelecsx)
- [Samyak Gangwal](https://gitlab.com/sam-gangwal)
- [Paul Maynard](https://gitlab.com/pmaynard001)
- [Ramsey Harrison](https://gitlab.com/rharrison)
- Nandinii Yeleswarapu
- Udaya Sathiyamoorthy
