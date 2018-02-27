# market-parser

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
