// Templated kernel that references a dependent scope expression
// [Traits<T>::value] in its body. Without an explicit launch the
// primary template body is parsed and the qualified dependent
// reference reaches faial as a [DependentScopeRef] carrying the
// unqualified name and the nested-name-specifier, rather than
// collapsing to an opaque RecoveryExpr. The kernel writes a constant
// to a single shared address from every thread, so it is racy.
template <typename T>
struct Traits { static const int value = 1; };

template <typename T>
__global__ void k(int *p) {
    p[0] = Traits<T>::value;
}
