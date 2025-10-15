
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
    WHEN 'LOG'.
      CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
        EXPORTING
          cusobj   = 'ZPMR0002'
          tabfirst = 'X'.
    WHEN 'LOG2'.
      CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
        EXPORTING
          cusobj   = 'ZPMR0011'
          tabfirst = 'X'.
  ENDCASE.
ENDFORM.
