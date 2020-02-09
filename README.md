# FAT in Haskell

[![Build Status](https://travis-ci.com/kompendium-llc/fat-haskell-client.svg?branch=master)](https://travis-ci.com/kompendium-llc/fat-haskell-client)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)


A JSON-RPC Haskell client for the FAT protocol. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

## Installation

To run and test from repository

1. Build with stack
```bash
$ stack build
```
2. Load REPL with stack for evaluation
```
$ stack repl
```

## Token Issuance


## Token Transactions