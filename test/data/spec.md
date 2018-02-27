Little Endian
- 24 Header Global
for each data block
  - 16 Interface Block
    1. 4 bytes unix timestamp
    2. 4 bytes microseconds
    3. 4 bytes size packet saved
    4. 4 bytes size packet captured (This - 42 == length Data)
  - 42 until Data
  - DATA
