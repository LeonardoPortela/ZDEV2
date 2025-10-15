*----------------------------------------------------------------------*
***INCLUDE LZ_TRACECONTTONI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'CARGA'.
      IF g_file_name IS INITIAL.
        MESSAGE s024(sd) WITH 'Informar nome do Arquivo!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      PERFORM f_carrega_file   CHANGING l_erro.
      PERFORM f_valida_arquivo CHANGING l_erro.

      IF l_erro = abap_true.
        PERFORM f_exibe_erros.
      ELSE.
        PERFORM f_grava_dados.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILE_DROPDOWN_BOX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE file_dropdown_box INPUT.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable,
        w_filetable TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO w_filetable INDEX 1.
  g_file_name = w_filetable-filename.

ENDMODULE.
