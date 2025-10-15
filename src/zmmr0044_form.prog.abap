*&---------------------------------------------------------------------*
*&  Include           ZMMR0044_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CAMINHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ARQ  text
*----------------------------------------------------------------------*
FORM f_caminho  CHANGING p_arq.

  DATA: lt_filetable         TYPE         filetable,
        ls_filetable         TYPE LINE OF filetable,
        lv_rc                TYPE i,
        lv_window_title      TYPE string,
        lv_default_extension TYPE string,
        lv_file_filter       TYPE string.

  CLEAR: lt_filetable.
  lv_window_title = TEXT-t05. "Selecione o arquivo para Upload.
  lv_default_extension = TEXT-t06. "CSV
  lv_file_filter = TEXT-t07. "Arquivos do csv (*.CSV)|*.CSV|

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title "Selecione o arquivo para Upload
      default_extension       = lv_default_extension "CSV
      file_filter             = lv_file_filter
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    IF sy-subrc EQ 0.
      p_arq = ls_filetable-filename.            "Identifica o arquivo selecionado e joga para o parametro de seleção
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_layout .
  comm0 = 'Utiliza o arquivo excel (.xls) disponibilizado como '.
  com00 = 'modelo, seguindo a sequência abaixo:'.
  comm1 = 'COLUNA 1: TP_ESTRAT - Tipo de Estratégia'.
  comm2 = 'COLUNA 2: KOSTL - Centro de Custo'.
  comm3 = 'COLUNA 3: WERKS - Centro'.
  comm4 = 'COLUNA 4: NIVEL - Nível de Aprovador'.
  comm5 = 'COLUNA 5: LTEXT - Texto Descritivo'.
  comm6 = 'COLUNA 6: NAME1 - Nome'.
  comm7 = 'COLUNA 7: TP_OPER - Tipo de Operação Suprimentos'.
  comm8 = 'COLUNA 8: APROVADOR - Nome Aprovador'.
ENDFORM.
