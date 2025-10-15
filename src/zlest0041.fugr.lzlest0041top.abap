*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL zlest0041                  MESSAGE-ID sv.

INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzlest0041t00                           . "view rel. data dcl.

DATA: nm_cliente    TYPE kna1-name1,
      nm_material   TYPE makt-matnr,
      ok_fornecedor TYPE c LENGTH 1,
      ok_nota       TYPE c LENGTH 1,
      vl_alterou    TYPE c LENGTH 1.
