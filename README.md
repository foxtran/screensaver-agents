# screensaver-agents

Simple screensaver written in pure Fortran.

# Compilation

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
make
```

# Run

```bash
./screensaver
```

# Notes

It works for framebuffers in ARGB format with 8 bits per channel.

Framebuffer must have resolution 1920x1080 and be located at `/dev/fb0`.

Tested on JH7110 RISC-V CPU.

# Demo

![Demo](/demo/demo.mov)
