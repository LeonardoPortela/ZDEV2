
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
  ENDCASE.
ENDFORM.
FORM f4_file.
    DATA: lt_file   TYPE filetable,
          lv_action TYPE i,
          lv_rc     TYPE i,
          nm_file   TYPE string.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             = |xlsx (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_excel }|
      CHANGING
        file_table              = lt_file
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF lv_action = cl_gui_frontend_services=>action_ok.

      IF lv_rc = 1.
        READ TABLE lt_file INDEX 1 INTO nm_file.
        IF sy-subrc = 0.
          iv_pfile = nm_file.
*          model( i_file =  nm_file ).
        ENDIF.
      ENDIF.
    ENDIF.
ENDFORM.
