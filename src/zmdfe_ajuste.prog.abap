*&---------------------------------------------------------------------*
*& Report ZMDFE_AJUSTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdfe_ajuste.

TABLES: zsdt0102.

TYPES: BEGIN OF ty_saida,
         docnum     TYPE zsdt0102-docnum,
         nmdfe      TYPE zsdt0102-nmdfe,
         cancel     TYPE zsdt0102-cancel,
         encerrado  TYPE zsdt0102-encerrado,
         estornado  TYPE zsdt0102-estornado,
         autorizado TYPE zsdt0102-autorizado.
TYPES: END OF ty_saida.

CONSTANTS: c_yf_user_command   TYPE slis_formname   VALUE 'F_USER_COMMAND'.

DATA: t_0102     TYPE TABLE OF zsdt0102,
      w_0102     TYPE zsdt0102,
      t_0102_new TYPE TABLE OF zsdt0102_log,
      w_0102_new TYPE zsdt0102_log,
      t_0102_old TYPE TABLE OF zsdt0102_log,
      w_0102_old TYPE zsdt0102_log,
      t_saida    TYPE TABLE OF zsdt0102, "ty_saida,
      w_saida    TYPE zsdt0102, "ty_saida,
      t_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      events     TYPE slis_t_event,
      t_print    TYPE slis_print_alv,
      w_layout   TYPE slis_layout_alv,
      t_fcat     TYPE slis_t_fieldcat_alv,
      v_report   LIKE sy-repid,
      s_variant  TYPE disvariant,
      v_variant  TYPE disvariant,
      lv_answer  TYPE c,
      lv_apagar_evento TYPE abap_bool VALUE abap_false,
      encerrado TYPE abap_bool VALUE abap_false.

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_docnum   FOR zsdt0102-docnum.
SELECTION-SCREEN   END OF BLOCK 02.

START-OF-SELECTION.

  PERFORM f_selecao.
  IF t_0102[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002.
    EXIT.
  ENDIF.

  PERFORM f_saida.

FORM f_selecao.

  SELECT *
    FROM zsdt0102
    INTO TABLE t_0102
   WHERE docnum      IN s_docnum.
   "  AND contingencia = abap_true.

ENDFORM.

FORM f_saida.

  FREE: t_saida.

  t_saida[] = t_0102[].

* LOOP AT t_0102            INTO w_0102.
*   MOVE-CORRESPONDING w_0102 TO w_saida.
*   APPEND w_saida            TO t_saida.
* ENDLOOP.

  PERFORM alv_preenche_cat USING:
        'DOCNUM'          'Nº documento'    '15'  ' ' ' '  ' ',
        'NMDFE'           'Número MDF-e'    '15'  ' ' ' '  ' ',
        'CANCEL'          'Cancelado'       '15'  'X' 'X'  'X',
        'ENCERRADO'       'Encerrado'       '15'  'X' 'X'  'X',
        'ESTORNADO'       'Estornado'       '15'  'X' 'X'  'X',
        'AUTORIZADO'      'Autorizado'      '15'  'X' 'X'  'X'.

  v_report                   = sy-repid.
  w_layout-cell_merge        = abap_true.
  w_layout-expand_all        = abap_true.
  w_layout-zebra             = abap_true.
  w_layout-window_titlebar   = 'Alterar Status ZSDT0102' .
* w_layout-detail_titlebar   = text-004.
  w_layout-box_tabname       = 'T_SAIDA'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = c_yf_user_command
      it_fieldcat             = t_fcat[]
      is_layout               = w_layout
*     it_sort                 = t_sort[]
      i_save                  = 'X'
*     it_events               = events
*     is_print                = t_print
      is_variant              = v_variant
    TABLES
      t_outtab                = t_saida.

ENDFORM.

FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_edit  TYPE c
                               p_hot   TYPE c
                               p_checkbox  TYPE c           .

  DATA: wl_fcat TYPE slis_fieldcat_alv.

  wl_fcat-tabname   = 'T_SAIDA'.
  wl_fcat-fieldname = p_campo   .
  wl_fcat-seltext_l = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-edit      = p_edit.
  wl_fcat-hotspot   = p_hot     .
  wl_fcat-checkbox  = p_checkbox    .
  wl_fcat-outputlen = p_tam     .

  APPEND wl_fcat TO t_fcat.

ENDFORM.

FORM f_salvar.

  DATA: l_timestamp TYPE timestampl.

  LOOP AT t_saida INTO w_saida.

    READ TABLE t_0102 INTO w_0102 WITH KEY docnum = w_saida-docnum.

    CHECK sy-subrc = 0.

    IF w_saida-cancel     = w_0102-cancel    AND
       w_saida-encerrado  = w_0102-encerrado AND
       w_saida-estornado  = w_0102-estornado AND
       w_saida-autorizado = w_0102-autorizado.
      CONTINUE.
    ENDIF.

    GET TIME STAMP FIELD l_timestamp.

    MOVE-CORRESPONDING w_0102  TO w_0102_old.
    w_0102_old-status_reg       = 'ANTES'.
    w_0102_old-sequencia        = l_timestamp.
    w_0102_old-user_reg         = sy-uname.
    w_0102_old-data_reg         = sy-datum.
    w_0102_old-hora_reg         = sy-uzeit.
    MODIFY zsdt0102_log      FROM w_0102_old.

    MOVE-CORRESPONDING w_saida TO w_0102_new.
    w_0102_new-status_reg       = 'DEPOIS'.
    w_0102_new-sequencia        = l_timestamp.
    w_0102_new-user_reg         = sy-uname.
    w_0102_new-data_reg         = sy-datum.
    w_0102_new-hora_reg         = sy-uzeit.
    MODIFY zsdt0102_log      FROM w_0102_new.

    MODIFY zsdt0102          FROM w_saida.
    IF encerrado = abap_true.
      DELETE FROM J_1BNFE_EVENT
        WHERE DOCNUM = W_SAIDA-DOCNUM
          AND int_event = 'EV_ENC'.
    ENDIF.
CLEAR lv_apagar_evento.
    COMMIT WORK AND WAIT.
  ENDLOOP.

ENDFORM.

FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  READ TABLE t_saida INTO w_saida INDEX l_selfield-tabindex.


  CASE l_ucomm.
    WHEN '&IC1'.
      CASE l_selfield-fieldname.
        WHEN 'CANCEL'.
          w_saida-cancel     = abap_true.
          w_saida-encerrado  = abap_false.
          w_saida-estornado  = abap_false.
          w_saida-autorizado = abap_false.
          MODIFY t_saida  FROM w_saida INDEX l_selfield-tabindex.

        WHEN 'ENCERRADO'.
          w_saida-cancel     = abap_false.
          w_saida-encerrado  = abap_true.
          w_saida-estornado  = abap_false.
          w_saida-autorizado = abap_false.
          MODIFY t_saida  FROM w_saida INDEX l_selfield-tabindex.

        WHEN 'ESTORNADO'.
          w_saida-cancel     = abap_false.
          w_saida-encerrado  = abap_false.
          w_saida-estornado  = abap_true.
          w_saida-autorizado = abap_false.
          MODIFY t_saida  FROM w_saida INDEX l_selfield-tabindex.

        WHEN 'AUTORIZADO'.
*------US 190846 - ROBPDIAS
          READ TABLE t_0102 INTO w_0102 WITH KEY docnum = w_saida-docnum.
          IF sy-subrc = 0 AND w_0102-encerrado = abap_true.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar       = 'Confirmação'
              text_question  = 'Deseja reenviar Encerramento?'
              text_button_1  = 'Sim'
              text_button_2  = 'Não'
              default_button = '2'
              display_cancel_button = ''
            IMPORTING
                answer         = lv_answer.
          ENDIF.
          IF lv_answer = '1'.
             lv_apagar_evento = abap_true.
             encerrado = lv_apagar_evento.
          ENDIF.
*------US 190846 - ROBPDIAS
          w_saida-cancel     = abap_false.
          w_saida-encerrado  = abap_false.
          w_saida-estornado  = abap_false.
          w_saida-autorizado = abap_true.
          MODIFY t_saida  FROM w_saida INDEX l_selfield-tabindex.
      ENDCASE.

    WHEN '&DATA_SAVE'.
      PERFORM f_salvar.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.

  l_selfield-refresh = abap_true.
  l_ucomm = '&REFRESH'.

ENDFORM.                    "f_user_command
