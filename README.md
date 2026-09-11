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

# WarpDRF subgroup extension

This fork adds explicit subgroup memory and participation checks. The Docker
images and downloads above are upstream Faial; build this checkout to use the
extension.

With `cu-to-json` on `PATH`, a small checked-in example can be run as follows:

```bash
opam exec --switch=. -- dune exec drf/bin/main.exe -- \
  --subgroup-size 32 --block-dim 32 \
  examples/drf/drf-subgroup-repeated-barrier.cu
```

Use `--cu-to-json=/path/to/cu-to-json` to select the CUDA translator explicitly.
The example contains a repeated warp barrier and exercises the loop-protocol
fallback.

Without `--subgroup-size`, the CLI uses upstream lowering. With the option,
kernels that contain supported subgroup operations use the extension; ordinary
kernels still use the original full-program pipeline, including device helpers.
A result without the option is not a subgroup-participation proof.

The subgroup report separates `mem_drf`, `subgroup_uniformity`, and
`drf_full`. The full verdict passes only when both checks pass. Source launch
facts, unsupported forms, and solver failures remain explicit in the report.

See [the DRF maintainer guide](drf/README.md) for the implementation map, tests,
and current limitations, and [the inference guide](inference/README.md) for
source extraction.

# Build from source

## Dependencies

* [opam `>= 2.0`](https://opam.ocaml.org/)
* [ocamlc `>= 5.3`](https://ocaml.org/)

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
