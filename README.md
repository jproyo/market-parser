# market-parser

[![Build Status](https://travis-ci.org/jproyo/market-parser.svg?branch=master)](https://travis-ci.org/jproyo/market-parser.svg?branch=master)

## Spec Packet

### General

    LE [ 24 bytes HEADER ]|[PACKET]

### PACKET

    [16 bytes IFACE]|[42 bytes PROTO]|[DATA]

### IFACE

    [4 bytes timestamp]|[4 bytes microseconds]|[4 bytes size packet saved]|[4 bytes size packet capture]

### DATA

215 = Valid Packet Quote

    [5 bytes mark quote]|[210 rest Quote]

Mark Quote = B6034 ASCII

## Run

### Prerequisites

In order to run this solution you are going to need the following distributions installed.

- Stack 1.6

### Run tests

```shell
bash.$ stack test
```

### Run tests

```shell
bash.$ stack test
```

### Startup Server

```shell
bash.$ stack build
bash.$ stack exec mkt-ps < FILE_PATH_WITH_DATA
```

#### Run without Nix integration

```shell
bash.$ stack build --no-nix
bash.$ stack --no-nix exec mkt-ps < FILE_PATH_WITH_DATA
```
