FUNCTION z_memo_cfop_comercializacao.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CFOPS STRUCTURE  LXHME_RANGE_C10
*"--------------------------------------------------------------------
*{   INSERT         DEVK9A1XAW                                        1

  DATA: wa_setleaf TYPE setleaf,
        wa_cfops   TYPE lxhme_range_c10,
        it_setleaf LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: cfops[], it_setleaf[].

*** Inicio - Rubenilson - 09.10.24 #154153
  SELECT *
    FROM zsdt0352
    INTO TABLE @DATA(lt_zsdt0352)
    WHERE excluido EQ @space.
  LOOP AT lt_zsdt0352 ASSIGNING FIELD-SYMBOL(<fs_zsdt0352>).
    wa_cfops-sign   = 'I'.
    wa_cfops-option = 'EQ'.
    wa_cfops-low    = <fs_zsdt0352>-cfop.
    wa_cfops-high   = <fs_zsdt0352>-cfop.
    APPEND wa_cfops TO cfops.
  ENDLOOP.

*}   INSERT
ENDFUNCTION.
