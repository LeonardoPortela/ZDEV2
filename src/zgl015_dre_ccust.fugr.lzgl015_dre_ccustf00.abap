*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*

* base table related FORM-routines.............
INCLUDE LSVIMFTX .

*&---------------------------------------------------------------------*
*&      Module  Z_TRATA_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_trata_field INPUT.

  ZGL015_DRE_CCUST-USNAM  = sy-uname.
  ZGL015_DRE_CCUST-ZDT_ATUAL = sy-datum.
  ZGL015_DRE_CCUST-ZHR_ATUAL = sy-uzeit.

ENDMODULE.
