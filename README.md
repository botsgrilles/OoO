# Chisel Template Project

This is a minimal standalone Chisel template for rapid module development.

## Requirements

- Java 11 or newer
- Mill (or use the wrapper from your environment)

## Quick Start

```bash
cd chisel-template
mill -i chiselTemplate.compile
mill -i chiselTemplate.runMain chiseltemplate.GenerateVerilog
```

## XMake Workflow

```bash
# Initialize submodules
xmake init

# Compile Scala/Chisel
xmake comp

# Format Scala sources
xmake fmt

# Generate RTL to build/rtl
xmake rtl
```

Generated RTL will be emitted to `build/rtl`.

## Files

- `build.sc`: Mill build definition
- `src/main/scala/chiseltemplate/Adder.scala`: Example Chisel module
- `src/main/scala/chiseltemplate/GenerateVerilog.scala`: RTL generation entry

## Next Steps

- Replace `Adder` with your own module
- Add parameters and custom generators
- Add tests with chiseltest when needed
