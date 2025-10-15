*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0150_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0292_exit.

FORM f_exit_zsdt0292_0001 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zsdt0292_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.
  DATA: wl_zsdt0292 TYPE zsdt0292,
        vl_message  TYPE c LENGTH 200,
        lv_cpf      TYPE zde_stcd2,
        lv_answer.

  CLEAR: wl_zsdt0292.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0292.
  CASE sy-ucomm .
    WHEN 'NOVO' OR 'CHANGE'.

      IF ( wl_zsdt0292-empresa IS INITIAL ).
        MESSAGE 'Informar Empresa' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.

*------------------------------
*---- valida empresa
*------------------------------
      SELECT bukrs
        INTO @DATA(l_bukrs)
        FROM t001
          UP TO 1 ROWS
       WHERE bukrs = @wl_zsdt0292-empresa.
      ENDSELECT.

      IF sy-subrc <> 0.
        MESSAGE 'Empresa Informada está Incorreta.' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.

*------------------------------
*---- valida cpf
*------------------------------
      IF wl_zsdt0292-cpf IS NOT INITIAL AND
        wl_zsdt0292-cpf  <> '00000000000'.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
          EXPORTING
            input     = wl_zsdt0292-cpf
          IMPORTING
            output    = lv_cpf
          EXCEPTIONS
            not_valid = 1
            OTHERS    = 2.

        IF sy-subrc <> 0.
          MESSAGE 'CPF Informado está Incorreto.' TYPE 'S' DISPLAY LIKE 'E'.
          p_error = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

      IF wl_zsdt0292-cpf = '00000000000'.
        CLEAR wl_zsdt0292-cpf.
      ENDIF.

*      IF ( wl_zsdt0292-data IS INITIAL ).
*        MESSAGE 'Informar Data' TYPE 'S' DISPLAY LIKE 'E'.
*        p_error = abap_true.
*        EXIT.
*      ENDIF.
*
*      IF ( wl_zsdt0292-hora IS INITIAL ).
*        MESSAGE 'Informar Hora' TYPE 'S' DISPLAY LIKE 'E'.
*        p_error = abap_true.
*        EXIT.
*      ENDIF.

      IF sy-ucomm = 'NOVO'.
        SELECT SINGLE COUNT( * )
          FROM zsdt0292
          WHERE empresa  EQ wl_zsdt0292-empresa
            AND status   EQ 'A'.

        IF ( sy-subrc IS INITIAL ).
          p_error = abap_true.
          CONCATENATE 'Já tem um Cadastro ativo para a Empresa' wl_zsdt0292-empresa INTO vl_message SEPARATED BY space.
          MESSAGE vl_message TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

    WHEN 'DEL'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = 'Eliminação'
          text_question  = 'Deseja realmente eliminar o registro?'
          text_button_1  = 'Sim'
          text_button_2  = 'Não'
        IMPORTING
          answer         = lv_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF ( lv_answer NE '1' ).
        p_error = abap_true.
*        CONCATENATE  'Item cancelado' INTO vl_message SEPARATED BY space.
        MESSAGE 'Item cancelado' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_exit_zsdt0292_0003 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0292 TYPE zsdt0292,
        vl_message  TYPE c LENGTH 200.

  CLEAR: wl_zsdt0292.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0292.
  CASE sy-ucomm .
    WHEN 'NOVO'.
      IF ( wl_zsdt0292-status IS INITIAL ).
        wl_zsdt0292-status = 'A'.
      ENDIF.
      IF ( wl_zsdt0292-texto_geral IS INITIAL
      AND  wl_zsdt0292-empresa     IS NOT INITIAL ).
        CONCATENATE 'ZSDT0141_CADEXPORTADO_' wl_zsdt0292-empresa INTO wl_zsdt0292-texto_geral.
      ENDIF.
      wl_zsdt0292-usname   = sy-uname.
      wl_zsdt0292-data     = sy-datum.
      wl_zsdt0292-hora     = sy-uzeit.
    WHEN 'CHANGE'.
      wl_zsdt0292-usname_c = sy-uname.
      wl_zsdt0292-data_c   = sy-datum.
      wl_zsdt0292-hora_c   = sy-uzeit.
    WHEN 'DEL'.
      wl_zsdt0292-status   = 'I'.
      wl_zsdt0292-usname_c = sy-uname.
      wl_zsdt0292-data_c   = sy-datum.
      wl_zsdt0292-hora_c   = sy-uzeit.
    WHEN OTHERS.
  ENDCASE.

  MOVE-CORRESPONDING wl_zsdt0292 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0292_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0292 TYPE zsdt0292.

  CLEAR: wl_zsdt0292.
  MOVE-CORRESPONDING p_saida TO wl_zsdt0292.

  IF wl_zsdt0292-cpf = '00000000000'.
    CLEAR wl_zsdt0292-cpf.
    MOVE-CORRESPONDING wl_zsdt0292 TO p_saida.
  ENDIF.

  IF ( wl_zsdt0292-status EQ 'I' ).
    FREE: p_saida.
  ENDIF.

ENDFORM.
FORM f_exit_zsdt0292_0005 CHANGING p_saida TYPE any.

  LOOP AT SCREEN.

    IF ( sy-ucomm EQ 'CHANGE' AND ( screen-group1 EQ 'GNE' OR screen-group1 EQ 'GNS' ) ) OR
       ( sy-ucomm EQ 'NOVO'   AND ( screen-group1 EQ 'GNS' ) ).
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDFORM.
FORM f_exit_zsdt0292_0014 USING p_saida TYPE any CHANGING p_break.

  DATA: wl_zsdt0292 TYPE zsdt0292.
  CLEAR: wl_zsdt0292.
  MOVE-CORRESPONDING p_saida TO wl_zsdt0292.

  wl_zsdt0292-status   = 'I'.
  wl_zsdt0292-usname_c = sy-uname.
  wl_zsdt0292-data_c   = sy-datum.
  wl_zsdt0292-hora_c   = sy-uzeit.

  MODIFY zsdt0292 FROM wl_zsdt0292.

  IF sy-subrc = 0.
    MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
  ENDIF.

  p_break = abap_true.

ENDFORM.
FORM f_exit_zsdt0292_0016 USING p_ucomm
                          CHANGING p_registro_manter p_saida.
  DATA: wl_thead    TYPE thead.
  DATA: wl_zsdt0292 TYPE zsdt0292.

  CLEAR: wl_zsdt0292.
  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0292.

  IF ( p_ucomm EQ 'TEXT' ).
    IF ( wl_zsdt0292-texto_geral IS INITIAL
    AND  wl_zsdt0292-empresa     IS INITIAL ).
      MESSAGE 'Informar a empresa primeiro' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF ( wl_zsdt0292-texto_geral IS INITIAL
    AND  wl_zsdt0292-empresa     IS NOT INITIAL ).
      CONCATENATE 'ZSDT0141_CADEXPORTADO_' wl_zsdt0292-empresa INTO wl_zsdt0292-texto_geral.
    ENDIF.

    wl_thead-tdname   = wl_zsdt0292-texto_geral.
    wl_thead-tdobject = 'TEXT'.
    wl_thead-tdid     = 'ST'.
    wl_thead-tdspras  = sy-langu.

    SELECT SINGLE COUNT( * ) FROM stxh
        WHERE tdobject = wl_thead-tdobject
          AND tdname   = wl_thead-tdname
          AND tdid     = wl_thead-tdid
          AND tdspras  = wl_thead-tdspras.

    IF ( sy-subrc IS NOT INITIAL ).
      PERFORM f_create_text USING wl_thead.
    ENDIF.
    wl_thead-tdlinesize  = 100.
    PERFORM f_edit_text USING wl_thead.
  ENDIF.
  MOVE-CORRESPONDING wl_zsdt0292 TO  p_registro_manter.


ENDFORM.
FORM f_edit_text USING wl_thead TYPE thead.
  DATA: lt_tline TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = wl_thead-tdid
      language                = sy-langu
      name                    = wl_thead-tdname
      object                  = wl_thead-tdobject
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CALL FUNCTION 'EDIT_TEXT'
    EXPORTING
*     DISPLAY       = ' '
*     EDITOR_TITLE  = ' '
      header        = wl_thead
*     PAGE          = ' '
*     WINDOW        = ' '
      save          = 'X'
*     LINE_EDITOR   = ' '
*     CONTROL       = ' '
*     PROGRAM       = ' '
*     LOCAL_CAT     = ' '
*   IMPORTING
*     FUNCTION      =
*     NEWHEADER     =
*     RESULT        =
    TABLES
      lines         = lt_tline
    EXCEPTIONS
      id            = 1
      language      = 2
      linesize      = 3
      name          = 4
      object        = 5
      textformat    = 6
      communication = 7
      OTHERS        = 8.

  IF sy-subrc IS INITIAL .
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

ENDFORM.
FORM f_create_text USING wl_thead.
  DATA: lt_tline TYPE TABLE OF tline.

  APPEND VALUE tline( tdformat = '*' ) TO lt_tline.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
*     CLIENT          = SY-MANDT
      header          = wl_thead
*     INSERT          = ' '
      savemode_direct = abap_true
*     OWNER_SPECIFIED = ' '
*     LOCAL_CAT       = ' '
*     KEEP_LAST_CHANGED       = ' '
*  IMPORTING
*     FUNCTION        =
*     NEWHEADER       =
    TABLES
      lines           = lt_tline
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0292_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'EMPRESA'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'DATA'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'HORA'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'VENDEDOR'.
    p_outputlen    = 40.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'CPF'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'RG'.
    p_outputlen    = 30.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0292' AND
     p_field       = 'TEXTO_GERAL'.
    p_outputlen    = 30.
  ENDIF.

ENDFORM.
