
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT lo_report.
  lo_report->get_data( ).
  lo_report->generate_output( ).
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  "SET TITLEBAR 'xxx'.
ENDMODULE.

MODULE user_command_0200 INPUT.
  PERFORM action_process.
ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCELAR'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'SALVAR'.

*      DATA: dynfields TYPE TABLE OF dynpread WITH HEADER LINE.
*      CLEAR: dynfields, dynfields[].
*
*      dynfields-fieldname = 'WA_SAIDA-BUKRS'.
*      APPEND dynfields.
*
*      dynfields-fieldname = 'WA_SAIDA-WERKS'.
*      APPEND dynfields.
*
*      dynfields-fieldname = 'WA_SAIDA-GJHAR'.
*      APPEND dynfields.
*
*      dynfields-fieldname = 'WA_SAIDA-MONAT'.
*      APPEND dynfields.
*
**      dynfields-fieldname = 'WA_SAIDA-SALDO_FIXO'.
**      APPEND dynfields.
*
*      sort dynfields[] ASCENDING.
*
*      CALL FUNCTION 'DYNP_VALUES_READ'
*        EXPORTING
*          dyname             = sy-cprog
*          dynumb             = sy-dynnr
*          translate_to_upper = 'X'
*        TABLES
*          dynpfields         = dynfields.
*
*      IF sy-subrc = 0.
*
*        LOOP AT dynfields ASSIGNING FIELD-SYMBOL(<_get>).
*          CASE <_get>-fieldname.
*            WHEN 'WA_SAIDA-BUKRS'.
*              wa_saida-bukrs = <_get>-fieldvalue.
*            WHEN 'WA_SAIDA-WERKS'.
*              wa_saida-werks = <_get>-fieldvalue.
*            WHEN 'WA_SAIDA-MONAT'.
*              wa_saida-monat = <_get>-fieldvalue.
*            WHEN 'WA_SAIDA-GJHAR'.
*              wa_saida-gjhar = <_get>-fieldvalue.
*            WHEN 'WA_SAIDA-SALDO_FIXO'.
*              wa_saida-saldo_fixo = <_get>-fieldvalue.
*          ENDCASE.
*        ENDLOOP.
*
*      ENDIF.

      IF wa_saida-bukrs IS NOT INITIAL AND wa_saida-werks IS NOT INITIAL AND wa_saida-monat IS NOT INITIAL AND wa_saida-GJAHR IS NOT INITIAL.

        DATA: wa_zfit0216 TYPE zfit0216.
        CLEAR:wa_zfit0216.
        MOVE-CORRESPONDING wa_saida TO wa_zfit0216.
        MODIFY zfit0216 FROM wa_zfit0216.
        lo_report->set_refresh( ).
        SET SCREEN 0.
        LEAVE SCREEN.
        CLEAR: wa_saida,wa_zfit0216.
      ELSE.
        MESSAGE 'Favor preencher todos os Campos!' TYPE 'I'.
      ENDIF.
  ENDCASE.
ENDFORM.
