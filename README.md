# FAT client in Haskll

[![Build Status](https://travis-ci.com/kompendium-llc/fat-haskell.svg?branch=master)](https://travis-ci.com/kompendium-llc/fat-haskell)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)


A JSON-RPC Haskell client for the [FAT](https://github.com/Factom-Asset-Tokens/fatd)(Factom Asset Token) protocol on Factom blockchain. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

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

### FAT-0


### FAT-1


## Token Transactions


## Contributions

Please, feel free to contribute, fork the repo and submit PR.


Say thanks, send a tip:

- `btc`: 39oVXpsgsyW8ZgzsnX3sV7HLdtXWfT96qN
- `fct`: FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss
- `eth`: 0x9cDBA6bb44772259B3A3fb89cf233A147a720f34
- `xlm`: GD2FROTYXYSNOQQREYXHCXVOYWV556EM6PAHMVUHJNJEUYTUXUSNUZQ3