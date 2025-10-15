FUNCTION z_memo_cfop_check.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CFOP) TYPE  CHAR10
*"     REFERENCE(DIRECT) TYPE  J_1BDIRECT
*"     REFERENCE(EXP_PROPRIA) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(VALIDO) TYPE  CHAR1
*"----------------------------------------------------------------------

  DATA: gs_cfop   TYPE zmemo_cfop,
        gt_cfop   LIKE TABLE OF gs_cfop.

  CLEAR: valido.

  CASE direct.
    WHEN 1.
      CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
        TABLES
          cfops = gt_cfop.
    WHEN 2.
      CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
        EXPORTING
          exp_propria = exp_propria
        TABLES
          cfops       = gt_cfop.
  ENDCASE.

  SORT gt_cfop BY low.

  READ TABLE gt_cfop INTO gs_cfop WITH KEY low = cfop.
  IF sy-subrc EQ 0.
    valido = c_x.
  ENDIF.

ENDFUNCTION.
