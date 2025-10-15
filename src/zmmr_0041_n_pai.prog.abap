*&---------------------------------------------------------------------*
*&  Include           ZMMR_0041_N_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  PERFORM f_get_selected_line.

  CHECK gv_erro IS INITIAL.

  CASE sy-ucomm.
    WHEN '&DATA_SAVE'.

    WHEN '&F03'.
      LEAVE TO SCREEN 0.

    WHEN '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&F12'.
      LEAVE PROGRAM.

    WHEN 'EVENT'.
      PERFORM f_exibe_log.

    WHEN 'REFRESH'.
      PERFORM f_refresh_alv.
    WHEN 'STATUS_DOC'.
      PERFORM f_edit_doc.
    WHEN 'EDIT_DOC'.
      PERFORM f_edit_doc.
    WHEN 'VIEW_PDF'.
      PERFORM f_view_pdf.
    WHEN 'VIEW_XML'.
      PERFORM f_view_xml.
    WHEN 'DOWN_PDF'.
      PERFORM f_download_pdf.
    WHEN 'DOWN_XML'.
      PERFORM f_download_xml.
    WHEN 'STATUS_DOC'.
    WHEN 'ESTORNO'.
      PERFORM f_estornar.
    WHEN 'CANCEL'.
      PERFORM f_cancelar.
  ENDCASE.

  "PERFORM f_refresh_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
CASE sy-ucomm.
    WHEN 'BACK' OR 'UP' OR 'EXIT' or 'SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
