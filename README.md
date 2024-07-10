# Regex FSM Tool

This is a web application that visualizes the minimum-state DFA equivalent to a
regular expression.

This tool supports only the printable class of ASCII characters.

\[WIP\]

## Demo

View a demo at [the GitHub page](https://snootiermoon.github.io/re-fsm-tool).

## Build & Run Locally

```sh
git clone https://github.com/SnootierMoon/re-fsm-tool.git
cd re-fsm-tool
zig build run
```

The [Zig](https://ziglang.org/download/) compiler is a dependency for building
this project.

The build script will start a Python HTTP server by invoking `python3 -m
http.server` in the installation directory. Therefore, Python3 is a dependency
for running this project. You can run `zig build install` to generate the
webpage files without running the server.

## On FSMs, DFAs, and Regular Expressions

*Regular expressions (regexes? regices??)* are said to be discovered in 1951 by
mathematician Stephen Cole Kleene. The kind of regular expressions we see today
in text processing software can be attributed to Ken Thompson, who added
regex-based capabilities to early programs such as `ed`. Ken Thompson's initial
implementation of regex used *finite state machines (FSMs)* based on the
mathematical theory. However most modern regex engines nowadays use
backtracking instead.

A regex is a pattern which can be used to match certain strings. A regex and a
string can either match or fail to match. Thus, for any regex, there is a set
of all strings that match it.

An regex engine implemented with FSMs converts the regex to an *equivalent*
FSM, an FSM that accepts precisely the strings that the input regex matches.
There are two common variants of FSMs: *nondeterministic finite automata
(NFAs)* and *deterministic finite automata (DFAs)*. It is straightforward to
convert a regex to an NFA with a number of states roughly proportional to the
length of the regex, while a DFA may require an exponential number of states.
Therefore, NFAs are the more practical option for regex engines. However, DFAs
have the cool property that for any regular language, there is a unique DFA
with the smallest number of states that precisely accepts the language. In
other words, when asked to find the minimum DFA equivalent to a given regex,
*there is only one right answer*. The goal of this project is to find that
answer, for any regex in any common flavor.

A pitfall of FSMs as opposed to backtracking is that the set of strings that an
FSM accepts must be a *regular language*. Many features supported by modern
regex libraries, like backreferences, allow regular expressions to match
non-regular languages (making "regular expression" somewhat of a mathematical
misnomer for these patterns). Other features, such as capture groups, can be
implemented with special modifications to FSMs. Since this project is focused
on generating pure FSMs, backreferences, capture groups, and other
regular-language-unfriendly features will not be supported. Surprisingly, FSMs
can still support many complicated regex features including [lookahead and
lookbehind](https://www.regular-expressions.info/lookaround.html).

## Spec

Currently, this program is designed to support ASCII printable characters, the
95 characters from ' ' (0x20) to '~' (0x7E), inclusive. Future versions may
support the full set of ASCII characters, NUL (0x00) to DEL (0x7F).

Implemented Re -> AST. TODO AST -> NFA, NFA -> DFA, DFA minimization.

### Flavors

Currently supports a weird mixed version of different flavors. Eventually, to
support:

 - Posix Basic/Extended
 - Vim (extension of Posix Basic)
 - PCRE/PCRE2
 - Python, Java, Golang, Rust, C\#

### Renderers

 - Viz.js looks nice, but has a low state limit
 - Dagre/D3 looks a little worse, but has an incredibly high state limit
