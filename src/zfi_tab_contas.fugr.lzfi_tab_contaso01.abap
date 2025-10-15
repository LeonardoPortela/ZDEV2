*----------------------------------------------------------------------*
***INCLUDE LZFI_TAB_CONTASO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  DESC_LIFNR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc_lifnr OUTPUT.

  DATA:
    v_desc_lifnr TYPE lfa1-name1.


  SELECT SINGLE name1
    INTO v_desc_lifnr
    FROM lfa1
    WHERE lifnr = zimp_contas_cons-lifnr.

ENDMODULE.                 " DESC_LIFNR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DATA_VENCTO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE data_vencto OUTPUT.

  DATA:
    v_dt_vencto TYPE zimp_contas_cons-zfbdt.


  v_dt_vencto = zimp_contas_cons-zfbdt +
                zimp_contas_cons-zbd1t.

ENDMODULE.                 " DATA_VENCTO  OUTPUT
