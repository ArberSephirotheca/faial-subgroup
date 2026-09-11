#ifndef FAIAL_INFERENCE_CUDA_INCLUDE_MMA_H
#define FAIAL_INFERENCE_CUDA_INCLUDE_MMA_H

/*
 * Parser-only declarations for CUDA WMMA capture.
 *
 * Real CUDA builds use NVIDIA's <mma.h>. Faial's c-to-json path needs only
 * enough declaration surface for Clang to type-check focused WMMA calls before
 * OCaml inference either rejects them explicitly or routes them to a future
 * subgroup/matrix representation. These declarations do not provide analysis
 * semantics.
 */
namespace nvcuda {
namespace wmma {

struct matrix_a {};
struct matrix_b {};
struct accumulator {};
struct row_major {};
struct col_major {};

enum mem_layout {
  mem_row_major,
  mem_col_major,
};

template <class Use, int M, int N, int K, class T, class Layout = void>
struct fragment {};

template <class Fragment, class Value>
__host__ __device__ inline void fill_fragment(Fragment &, Value) {}

template <class Fragment, class Pointer>
__host__ __device__ inline void load_matrix_sync(Fragment &, Pointer, unsigned) {}

template <class Pointer, class Fragment>
__host__ __device__ inline void store_matrix_sync(Pointer, const Fragment &, unsigned,
                                                  mem_layout) {}

template <class Accumulator, class A, class B>
__host__ __device__ inline void mma_sync(Accumulator &, const A &, const B &,
                                         const Accumulator &) {}

} // namespace wmma
} // namespace nvcuda

#endif
