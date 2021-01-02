Readme
======
This is a mostly useless exercise in parsing and simulating Verilog-1995 written in Ocaml.
The tool implements a textbook top-down recursive descent parser for a subset of Verilog-95 and does a better job than [the C version](https://github.com/ineganov/pnetlist).
Simulation is done via bytecode intermediary, somewhat similar to [Icarus](https://github.com/steveicarus/iverilog).

Current status is beyond useless; a work in progress that might never converge.

To make:
`make`

To run:
`mlverilog something.v`

