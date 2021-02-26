# Regen

A Regex Generator. Takes a regex pattern and produces strings matching that pattern.

This project is mostly intended as a learning experiment.

## Usage

This repo comes with a dev container that includes all the dependencies. Currently, this doesn't compile to a single executable, so you must compile generator.ml to a cmo file and load it in a toplevel to use it.

## Status

This project is currently **on hold**. While it is already fully functional and supports the subset of regex features I was initially aiming for, the code is slightly messy and I built it entirely on intuition. I'm currently taking the time to study computation via a book.

Long term plans include:

[] Supporting more regex features, like subexpressions and negation.
[] A single executable to use in a CLI.

