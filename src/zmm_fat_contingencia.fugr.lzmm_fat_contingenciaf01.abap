*----------------------------------------------------------------------*
***INCLUDE LZMM_FAT_CONTINGENCIAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_verifica_duplicidade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_duplicidade .

  FREE: t_alv.

  LOOP AT t_zmmt0008_ecc INTO w_zmmt0008_ecc.
    READ TABLE  t_zmmt0008_hana INTO w_zmmt0008_hana WITH KEY werks = w_zmmt0008_ecc-werks
                                                              lgort = w_zmmt0008_ecc-lgort
                                                              charg = w_zmmt0008_ecc-charg.

    IF sy-subrc = 0.
      APPEND w_zmmt0008_hana TO t_alv.
    ENDIF.
  ENDLOOP.

  IF t_alv[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há duplicidade de registros!'.
    EXIT.
  ENDIF.

  l_program                  = sy-repid.
  l_grid_title               = 'Registros que ja Existem no HANA'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_program
      i_structure_name       = 'ZMMT0008'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = l_program
      is_layout             = w_layout
      it_fieldcat           = t_fieldcat
      i_grid_title          = l_grid_title
      i_screen_start_column = 10
      i_screen_start_line   = 02
      i_screen_end_column   = 182
      i_screen_end_line     = 20
    TABLES
      t_outtab              = t_alv
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.

FORM f_carrega_tabela.

  DATA: l_totecc  TYPE i,
        l_tothana TYPE i,
        l_mesg1   TYPE char200,
        l_mesg2   TYPE char200.

  l_importado = abap_off.

  LOOP AT t_zmmt0008_ecc INTO w_zmmt0008_ecc.
    CLEAR: w_zmmt0008_ecc-mblnr, w_zmmt0008_ecc-mjahr, w_zmmt0008_ecc-vbeln_vf.

    READ TABLE  t_zmmt0008_hana INTO w_zmmt0008_hana WITH KEY werks = w_zmmt0008_ecc-werks
                                                              lgort = w_zmmt0008_ecc-lgort
                                                              charg = w_zmmt0008_ecc-charg.
    CHECK sy-subrc <> 0.

    MODIFY zmmt0008 FROM w_zmmt0008_ecc.

    l_importado = abap_true.
  ENDLOOP.

  IF l_importado = abap_true.
    MESSAGE s024(sd) WITH 'Tabela ZMMT0008 importada com sucesso'.
  ELSE.
    MESSAGE s024(sd) WITH 'Tabela ZMMT0008 ja foi importada'.
  ENDIF.

  COMMIT WORK AND WAIT.

  DESCRIBE TABLE t_zmmt0008_ecc LINES l_totecc.

  SELECT COUNT(*)
    INTO l_tothana
    FROM zmmt0008
   WHERE dados_contingencia = abap_true.

  l_mesg1 = |{ 'Total Regs Carregados ECC:' }  { l_totecc } |.
  l_mesg2 = |{ 'Total Regs Importados HANA:' } { l_tothana } |.

  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
    EXPORTING
      titel        = 'Importação ZMMT0008'
      textline1    = l_mesg1
      textline2    = l_mesg2
      start_column = 25
      start_row    = 6.

ENDFORM.

FORM f_carrega_sobregrava.

  DATA: l_totecc  TYPE i,
        l_tothana TYPE i,
        l_mesg1   TYPE char200,
        l_mesg2   TYPE char200.

  l_importado = abap_off.

  LOOP AT t_zmmt0008_ecc INTO w_zmmt0008_ecc.
    CLEAR: w_zmmt0008_ecc-mblnr, w_zmmt0008_ecc-mjahr, w_zmmt0008_ecc-vbeln_vf.

    MODIFY zmmt0008 FROM w_zmmt0008_ecc.

    l_importado = abap_true.
  ENDLOOP.

  IF l_importado = abap_true.
    MESSAGE s024(sd) WITH 'Tabela ZMMT0008 importada com sucesso'.
  ELSE.
    MESSAGE s024(sd) WITH 'Tabela ZMMT0008 ja foi importada'.
  ENDIF.

  COMMIT WORK AND WAIT.

  DESCRIBE TABLE t_zmmt0008_ecc LINES l_totecc.

  SELECT COUNT(*)
    INTO l_tothana
    FROM zmmt0008
   WHERE dados_contingencia = abap_true.

  l_mesg1 = |{ 'Total Regs Carregados ECC:' }  { l_totecc } |.
  l_mesg2 = |{ 'Total Regs Importados HANA:' } { l_tothana } |.

  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
    EXPORTING
      titel        = 'Importação ZMMT0008'
      textline1    = l_mesg1
      textline2    = l_mesg2
      start_column = 25
      start_row    = 6.

ENDFORM.
