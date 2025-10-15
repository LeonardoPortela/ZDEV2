*----------------------------------------------------------------------*
***INCLUDE LZCO_BALANCETEI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  CASE sy-ucomm.
    WHEN 'GERAR'.
      CASE abap_true.
        WHEN moeda_interna.
          PERFORM f_gerar_excel TABLES gt_saldo_contas_interna
                                USING 'ZSCO_SALDO_CONTAS_INTERNA'.
        WHEN moeda_forte.
          PERFORM f_gerar_excel TABLES gt_saldo_contas_forte
                                USING 'ZSCO_SALDO_CONTAS_FORTE'.
        WHEN moeda_indice.
          PERFORM f_gerar_excel TABLES gt_saldo_contas_indice
                                USING 'ZSCO_SALDO_CONTAS_INDICE'.

        WHEN tp_moeda.
          PERFORM f_gerar_excel TABLES gt_tp_moeda
                                USING 'ZSCO_TP_MOEDA'.

      ENDCASE.
    WHEN 'EXIT'.
      FREE: diretorio, moeda_interna, moeda_forte, moeda_indice, gt_columns, gt_temp, gt_saldo_contas, gt_saldo_contas_interna, gt_saldo_contas_forte, gt_saldo_contas_indice.
      LEAVE TO SCREEN 0.
      EXIT.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_DIRECTORY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_help_directory INPUT.

  DATA: lt_file_table  TYPE filetable,
        ls_return_code TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = CONV #( text-s01 ) "Text-s01: Diretório
    CHANGING
      file_table              = lt_file_table
      rc                      = ls_return_code
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_file_table INTO DATA(ls_file_table) INDEX 1.
  IF sy-subrc IS INITIAL.
    diretorio = ls_file_table-filename.
  ENDIF.

ENDMODULE.
