*----------------------------------------------------------------------*
***INCLUDE ZCOR025_USER_COMMAND_0100I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

*  FREE: t_rows[].
*
*  CALL METHOD g_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = t_rows.

*  IF t_rows[] IS INITIAL.
*    MESSAGE s068(poc_main) DISPLAY LIKE 'E'.
*  ENDIF.

  CASE ok_code.
    WHEN '&F03' OR '&F15' OR '&F12'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.
