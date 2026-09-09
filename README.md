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

The subgroup route keeps original Faial's loop alignment for repeated block
barriers while retaining source locations for the accesses in each memory
check. Subgroup participation is checked separately. A detector-only
`--find-true-dr` request never filters the loop-aligned memory protocol used
for the final subgroup DRF judgment.

The subgroup path is enabled explicitly:

```bash
opam exec -- dune exec drf/bin/main.exe -- \
  --cu-to-json=./bin/cu-to-json \
  --ignore-asserts \
  -t 10000 \
  --kernel flash_attn_ext_f16_ggml_wmma_d64_ncols16 \
  --block-dim 128 \
  --subgroup-size 32 \
  ../fattn.cu
```

The extracted kernel currently reports one ordinary-memory race while passing
the participation check:

```text
mem_drf: not_drf
memory_checks: 10 total, 1 racy, 0 unknown, 0 timeout, 0 unsupported
subgroup_uniformity: drf
drf_full: not_drf
```

The witness is the read of `KQ_max[j]` at line 191 and the lane-zero write at
line 215. The intervening warp reductions exchange register values but do not
order shared-memory accesses.

Ordinary shared/global source memory effects in subgroup/matrix kernels are
now modeled as subgroup-aware memory obligations owned by
`drf/lib/memory_event.ml`. Existing non-WMMA examples remain on the ordinary
`Imp` path, preserve original DRF/racy verdicts, and do not require
`--subgroup-size`. CUDA source that contains subgroup or WMMA operations must
provide `--subgroup-size`; otherwise it is rejected before ordinary `Imp`
lowering rather than analyzed with guessed workgroup-only semantics. The
regression gate for this boundary is
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
