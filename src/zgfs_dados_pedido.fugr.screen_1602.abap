PROCESS BEFORE OUTPUT.

  MODULE status_1602.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zib_nfe_dist_lot-charg.
    MODULE alterou_lote ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zib_nfe_dist_lot-vfdat.
    FIELD zib_nfe_dist_lot-licha.
    FIELD zib_nfe_dist_lot-herkl.
    FIELD zib_nfe_dist_lot-hsdat.
    FIELD zib_nfe_dist_lot-class.
    FIELD zib_nfe_dist_lot-klart.
    FIELD zib_nfe_dist_lot-menge.
    MODULE alterou_lote_info ON CHAIN-REQUEST.
  ENDCHAIN.

* MODULE USER_COMMAND_1602.
