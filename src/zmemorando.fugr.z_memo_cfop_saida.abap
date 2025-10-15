FUNCTION z_memo_cfop_saida.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(EXP_PROPRIA) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      CFOPS STRUCTURE  LXHME_RANGE_C10
*"----------------------------------------------------------------------

  DATA: wa_setleaf  TYPE setleaf,
        wa_cfops    TYPE lxhme_range_c10,
        it_setleaf  LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: cfops[], it_setleaf[].

  IF exp_propria IS INITIAL.
    "Opter CFOPS de Venda com finalidade exportação
    SELECT * INTO TABLE it_setleaf
      FROM setleaf
     WHERE setname EQ 'ZMEMORANDO_CFOPS'.
  ELSE.
    "Opter CFOPS de Exportação Própria
    SELECT * INTO TABLE it_setleaf
      FROM setleaf
     WHERE setname EQ 'ZMEMORANDO_CFOPP'.
  ENDIF.

  LOOP AT it_setleaf INTO wa_setleaf.
    wa_cfops-sign   = 'I'.
    wa_cfops-option = 'EQ'.
    wa_cfops-low    = wa_setleaf-valfrom.
    wa_cfops-high   = wa_setleaf-valfrom.
    APPEND wa_cfops TO cfops.
  ENDLOOP.

ENDFUNCTION.
