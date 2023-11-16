# SCaD 
(Systems Control and Development Language)

SCAD is a work-in-progress programming language designed for high-performance computing, enabling low-level control of computer systems. It is engineered to provide maximum control to programmers, offering zero-cost abstractions and eliminating hidden allocations.

## Usage
To run an example program:

```
cargo run <INPUT_NAME>.scad <OUTPUT_EXE_NAME>
```
Soon it will run as an executable but not yet. 


Because SCaD is a cool language, you can also use this as a file extension: 🖥.

```
cargo run <INPUT_NAME>.🖥️ <OUTPUT_EXE_NAME>
```

This is just another way that SCaD improves developer experience.

## Architecture 
1. Conversion from concrete syntax to high level IR
2. HIR to MIR lowering (involves static single assignment transformation)
3. MIR to LLVM IR lowering
4. LLVM optimisation


## Features

- **High Performance:** SCaD is designed to deliver high-performance computing capabilities by allowing direct low-level control of system resources.
- **Programmer Control:** The language prioritises maximum control for programmers, enabling them to manage and manipulate system resources explicitly.
- **Zero-Cost Abstractions:** SCaD ensures that abstractions do not incur additional costs, meaning developers can create efficient code without sacrificing performance.
- **No Hidden Allocations:** The language eliminates hidden memory allocations, providing complete transparency and control over memory usage.

## Key Goals

- **Performance-Centric:** SCAD focuses on performance optimization and efficiency in system-level programming.
- **Low-Level Control:** The language aims to give programmers direct control over hardware resources.
- **Transparency:** All processes and resource management are specified by the programmer, avoiding hidden side-effects that could impact performance.


## Roadmap

The planned development roadmap for SCaD includes:

- **Type Checking:** Improve safety by implementing a type checker.
- **Language Specification:** Finalize the language syntax, features, and behavior.
- **Compiler Development:** Build a robust and efficient compiler for the SCAD language (hopefully faster than rustc).
- **Performance Eval:** Compare the peformance to other well peformaing programming languages. 

