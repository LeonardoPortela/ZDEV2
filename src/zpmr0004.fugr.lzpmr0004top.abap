*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL ZPMR0004                   MESSAGE-ID SV.

  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZPMR0004T00                            . "view rel. data dcl.

  DATA: BEGIN OF TL_LOG OCCURS 0,
          EQTYP TYPE EQUI-EQTYP,
        END OF TL_LOG.
