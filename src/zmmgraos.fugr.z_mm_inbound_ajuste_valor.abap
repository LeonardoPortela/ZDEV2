FUNCTION z_mm_inbound_ajuste_valor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ZMMT0006 STRUCTURE  ZMMT0006
*"----------------------------------------------------------------------

  DATA: vch_referencia LIKE zmmt0006-ch_referencia.

  DATA: BEGIN OF wa_zmmt0006.
          INCLUDE STRUCTURE zmmt0006.
  DATA: END OF wa_zmmt0006.

  LOOP AT it_zmmt0006 INTO wa_zmmt0006.

    wa_zmmt0006-mandt         = sy-mandt.
    wa_zmmt0006-rg_atualizado = 'N'.
    wa_zmmt0006-cpudt         = sy-datum.
    wa_zmmt0006-cputm         = sy-uzeit.


    SELECT SINGLE ch_referencia
           FROM zmmt0006
       INTO vch_referencia
     WHERE ch_referencia EQ wa_zmmt0006-ch_referencia.



    IF sy-subrc EQ 0.
      UPDATE zmmt0006 FROM wa_zmmt0006.
    ELSE.
      INSERT into zmmt0006 values wa_zmmt0006.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.

ENDFUNCTION.
