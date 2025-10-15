FUNCTION-POOL ZBI_REPORTS                MESSAGE-ID SV.

  DATA: WA_ZBIT0003 TYPE ZBIT0003,
        WA_ZBIT0002 TYPE ZBIT0002,
        WA_ZBIT0001 TYPE ZBIT0001,
        LC_URI      TYPE AGR_URL.

* INCLUDE LZBI_REPORTSD...                   " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZBI_REPORTST00                         . "view rel. data dcl.
