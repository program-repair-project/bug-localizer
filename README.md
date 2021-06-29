# SmaLLVM Dataflow Analysis
The goal of this homework is to implement a static analyzer based on dataflow analysis that detects potential division-by-zero errors
in SmaLLVM programs.
Students will write the following modules based on the definitions in [this document](doc/homework.pdf):
- abstract domains for memories (module `Memory` in [src/domain.ml](src/domain.ml))
- abstract domains for integer values (module `Sign` in [src/domain.ml](src/domain.ml))
- abstract semantics for selected LLVM instructions (`transfer`, `transfer_cond`, `transfer_phi`, `eval`, and `filter` in [src/semantics.ml](src/semantics.ml))
- a generic fixed point computation engine (`run` in [src/analysis.ml](src/analysis.ml))

In short, replace all `failwith "Not implemented"` with your own implementation.
You can ignore the other LLVM instructions marked as `raise Unsupported`,
and assume that input programs are always **syntactically** valid.

Notice that students are not permitted to change directory structures and types of the functions.
All the functions you implement must have the same types as described in the module signatures. 
However, students are allowed to change `let` to `let rec` if needed.

## Setup
This homework assumes that students set up the OCaml environment following the same instructions in the previous homework.

After running make under the root directory of this repository, you can run the interpreter with the generated LLVM IR:
```
# run the analyzer
./analyzer test/example1.ll 
```
The commandline argument specifies the input file path.
Once you complete the implementation, the following output will be printed:
```
./analyzer interval example1.ll
Potential Division-by-zero @ example1.c:main:7:12, %call = Top
Potential Division-by-zero @ example1.c:main:14:10, %call = Top
```
Each line reports a potential division-by-zero bug at a certain source location
denoted by [file]:[function]:[line]:[column]. 
You can use helper function `string_of_location` in `src/utils.ml` to get the string representation of a source location.
The analyzer also prints out the
variable name of the divisor and its abstract value.

## Instruction

### 1. Control-flow Graph
A program is represented as a control-flow graph (module `Graph` in [src/domain.ml](src/domain.ml)). 
There are three types of nodes:
- Atomic: each atomic node associated with a single LLVM command which is either `assignment`, `input`, `print` or `goto`.
- Conditional: one conditional branch in LLVM is transformed to two conditional nodes, each of which is the first node of the true or false branch.
- Phi: one phi node may contain multiple contiguous phi commands in a basic block.
For detailed definitions, see the [document](src/homework.pdf).

The module provides an utility function `pred` to collect all predecessors of a given node.

### 2. Implementing Numerical Domains and Memory Domain
In this homework, you will implement the sign domain in the corresponding module.
Students are required to implement sound abstract semantic functions declared in module type `NUMERICAL_DOMAIN`.
Assume that all integers are signed and ignore integer-overflow effects.

To achieve precise static analysis result, use of the `filter` funtion at conditional branches is important.
Function call `filter pred v1 v2` will return a sound refinement of abstract numerical value `v1`
with respect to predicate `pred` and abstract numerical value `v2`.
If no refinment is possible, it will soundly return `v1` as it is.
In case of the sign domain, `filter < Top Zero` will soundly refine the left interval to `Neg`, but
`filter < Top Top` will return `Top` because no refinement is possible.

You will define the filter function for memories (`Semantics.Make.filter`) using the ones for values.
The first argument of the function represents whether the condition (i.e., the second argument) is true or not.
For example, `filter (x < 10) true mem` will return an abstract memory that *satisfies* condition `x < 10`.
On the other hand, `filter (x < 10) false mem` will return an abstract memory that *does not satisfy* condition `x < 10`.

The domains for memories will be defined with the choice of numerical domain in [src/main.ml](src/main.ml).
See modules `SignMemory`.

### 3. Implementing the Abstract Semantics for LLVM Instructions
You will implement a set of abstract semantic functions ([transfer*](src/semantics.ml)) for LLVM instructions.
You have to implement the semantics using the provided interface such as the `MEMORY_DOMAIN` and `NUMERICAL_DOMAIN` module signature.

Semantic function `filter neg cond memory` will return an abstract memory that soundly
subsumes all possible concrete memories satisfiying condition `cond` (if `neg` is true, then the negation of `cond`).
The implementation will use the filter functions of the underlying numerical abstract domain.

The semantic function for PHI nodes propagate two abstract memories.
First, the semantic function computes the join of all incoming abstract values
and store the result to the target variable. This output memory is propagated to the first non-PHI node in the block.
You can use utility function `get_first_nonphi` in [src/utils.ml](src/utils.ml) to get the label for the first non-PHI command.
Second, the semantic function propagate the input memory to the next PHI node. According to the LLVM semantics,  
all of the PHI nodes for a block must be executed atomically and they use the old local values from the previous block.
You can use utility function `get_next_phi` in [src/utils.ml](src/utils.ml) to get the label for the first non-PHI command.
Notice that, in LLVM, it is guaranteed that all PHI nodes precedes non-phi nodes in a block.

Consider function calls to LLVM intrinsic functions (e.g., `llvm.dbg.*`) and `print` as nops.

### 4. Implementing a Generic Worklist Algorithm
You will implement a generic worklist algorithm in function `run` in [src/analysis.ml](src/analysis.ml).
Function call `run llctx phase worklist table` will store the analysis result in `table` that is a map from labels
to memories. Initially, only the entry point of `main` has the empty memory and all the other labels have
the bottom memories (i.e., unreachable so far).
For unreachable code, the analyzer does not report alarms (see the `check_instr` function).

The algorithm will work with either widening operator or narrowing operator based on the argument `phase`.

After finishing the analysis, function `check` will scan the input program and print out the analysis results at
command `print` and `div`. For each function call to `print`, it will print the abstract value of the argument.
For each division instruction, it will print an alarm if it cannot prove the safety.

The first parameter `llctx` will be used if you want to invoke the helper function `string_of_location`
for debugging (described in the next section).

### 5. Pretty Printing
All the modules for domains have a pretty printer function named `pp`.
You can print a domain element to `stdout`(resp, `stderr`) using `Format.std_formatter` (resp, `Format.err_formatter`).
For example, `Value.pp Format.std_formatter v` will print out the abstract value `v` to `stdout`.
You can also print out in your own format using `Format.fprintf`: `Format.fprintf "=== %a ===" Value.pp v`.
See the [library document](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html) and the use in the skeleton code.

To print out the abstract value of an arbitrariy variable, use the `print` function.
The abstract value of the argument will be printed after the analysis.
See function `check_instr` in [src/analysis.ml](src/analysis.ml).

Make sure to remove all debugging code in the submition.

### 6. Format of Input Programs
Input programs in this homework are assumed to have only sub-features of the C language and LLVM IR as follows:

- Programs only have a single function definition of `main` and do not have global variables.
- All values are `i32` (interpreted as Int) or `i1` (interpreted as Bool) type of integers in LLVM IR (i.e., no floating points, pointers, structures, enums, arrays, etc). You can ignore other types of values.
- You should handle assignments, arithmetic operations (+, -, *, /), comparison operations (<, <=, >, >=, ==, !=), branches, and loops.
- There is no function call except for the ones to `input` and `print` that are declared in [this file](test/homework.h).
- All the other instructions are considered to be nop.

## References
- [Prosys LLVM Primer](https://github.com/prosyslab-classroom/llvm-primer)
- [OCaml Standard Library](http://caml.inria.fr/pub/docs/manual-ocaml/libref)
- [LLVM OCaml Binding](https://llvm.moe/ocaml/Llvm.html)
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html)
