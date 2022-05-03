__intcap_t foo() { return 0x12345678abcdefLL; }

/* { dg-final { scan-assembler-not {\.data\.rel} } } */
