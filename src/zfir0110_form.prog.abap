
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
      IF wa_saida-saknr IS NOT INITIAL AND wa_saida-d_c IS NOT INITIAL.
        wa_saida-usnam = sy-uname.
        wa_saida-dt_entrada = sy-datum.
        wa_saida-hr_entrada = sy-uzeit.

        DATA: wa_zfit0219 TYPE zfit0219.
        CLEAR:wa_zfit0219.
        MOVE-CORRESPONDING wa_saida TO wa_zfit0219.
        MODIFY zfit0219 FROM wa_zfit0219.
        lo_report->set_refresh( ).
        SET SCREEN 0.
        LEAVE SCREEN.
        CLEAR: wa_saida,wa_zfit0219.
            ELSE.
        MESSAGE 'Favor preencher todos os Campos!' TYPE 'I'.
      ENDIF.
  ENDCASE.
ENDFORM.
