# Regex FSM Tool

This is a web application that visualizes the minimum FSM representation of a
regular language accepted by a regular expression.

## Demo

View a demo at [the GitHub page](https://snootiermoon.github.io/re-fsm-tool).

## Build & Run Locally

```sh
git clone https://github.com/SnootierMoon/re-fsm-tools.git
cd re-fsm-tools
zig build run
```

The [Zig](https://ziglang.org/download/) compiler is a dependency for building
this project.

The build script will start a Python HTTP server by invoking `python3 -m
http.server` in the installation directory. Therefore, Python3 is a dependency
for running this project. Alternatively, you could run an HTTP server in the
installation directory yourself with an application of your choosing
(potentially written in Funge98 if you so desire).
