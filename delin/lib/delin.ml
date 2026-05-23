module Atom = Atom
module Term_inner = Term_inner
module Term = Term
module Expr = Expr
module Index = Index
module Greedy = Greedy
module ICS15 = Ics15

module type DelinAlgorithm = Algorithm.S

let size_params = Polynomial.size_params
let size_params_all = Polynomial.size_params_all
let dims = Polynomial.dims
let accesses = Polynomial.accesses
let parameter_atoms = Polynomial.parameter_atoms
let group_by_parameters = Polynomial.group_by_parameters
let try_scalar_quotient = Polynomial.try_scalar_quotient
let permutations = Polynomial.permutations
