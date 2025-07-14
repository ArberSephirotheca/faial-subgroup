# Faial: finds bugs in CUDA kernels

Faial is a static analysis tool for finding bugs in CUDA kernels, featuring **data-race freedom (DRF) analysis** and **performance analysis**.


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

# Build from source

## Dependencies

* [opam `>= 2.0`](https://opam.ocaml.org/)
* [ocamlc `>= 5.1`](https://ocaml.org/)

### 1. Setup

**Run this once.** The following command will install all dependencies needed to build the project.

```bash
./configure.sh
```

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

- [Hidden assumptions in static verification of data-race free GPU programs](https://dx.doi.org/10.1007/978-3-031-97492-2_6). In Principles and Practices of Building Parallel Software. LNCS, vol 14564. Tiago Cogumbreiro, Julien Lange. 2025

- [Sound and partially-complete static analysis of data-races in GPU programs](https://dx.doi.org/10.1145/3689797). Dennis Liew, Tiago Cogumbreiro, Julien Lange. PACMPL, 8(OOPSLA2), 2024. 

- [Memory Access Protocols: Certified Data-Race Freedom for GPU Kernels](https://dx.doi.org/10.1007/s10703-023-00415-0). Tiago Cogumbreiro, Julien Lange, Dennis Liew, Hannah Zicarelli. FMSD, 2023.

- [Provable GPU Data-Races in Static Race Detection](https://dx.doi.org/10.4204/EPTCS.356.4). Dennis Liew, Tiago Cogumbreiro, Julien Lange. In PLACES, volume 356 of EPTCS, page 36–45. 2022. 

- [Checking Data-Race Freedom of GPU Kernels, Compositionally](https://dx.doi.org/10.1007/978-3-030-81685-8_19). Tiago Cogumbreiro, Julien Lange, Dennis Lew, Hannah Zicarelli. In CAV, volume 12759, page 403–426. Springer, 2021.
