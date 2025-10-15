
MODULE user_command_0100 INPUT.
PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.

  "ZFIR0115
  IMPORT LR_memory FROM MEMORY ID 'ZFIR0115'.
  p_werks[] = lr_memory-lr_werks[].
  p_monat[] = lr_memory-lr_monat[].
  p_gjahr[] = lr_memory-lr_gjahr[].

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
  ENDCASE.
ENDFORM.
