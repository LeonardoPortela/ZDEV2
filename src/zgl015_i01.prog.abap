*&---------------------------------------------------------------------*
*&  Include           ZGL015_I01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'TS_100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE ts_100_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_ts_100-tab1.
      g_ts_100-pressed_tab = c_ts_100-tab1.
    WHEN c_ts_100-tab2.
      g_ts_100-pressed_tab = c_ts_100-tab2.
    WHEN c_ts_100-tab3.
      g_ts_100-pressed_tab = c_ts_100-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TS_100_ACTIVE_TAB_GET INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA  w_answer.
  DATA: cursorfield(30) TYPE c,
        cursorline(30)  TYPE c,
        cursorvalue(30) TYPE c.

  CASE ok-code.
    WHEN 'COPIA'. "modelo de lançamento
      wg_acao = 'COPY'.
      PERFORM f_limpa_campos.
      REFRESH: tg_fields.
      PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                      'GR3'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
    WHEN 'LIBER'.
      SET PARAMETER ID 'LOT' FIELD  wg_zglt034-lote.
      CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.
    WHEN 'LOTE'.
      CLEAR v_lote15.
      CALL TRANSACTION 'ZGL015' AND SKIP FIRST SCREEN.
      GET PARAMETER ID 'LOT' FIELD  v_lote15.
      wg_zglt034-lote = v_lote15.
    WHEN 'PICK'.
      GET CURSOR FIELD cursorfield LINE cursorline VALUE cursorvalue.
      IF NOT wa_zib_contabil_chv IS INITIAL AND  NOT cursorvalue IS INITIAL AND cursorfield = 'WG_ZGLT035-BELNR'.
        SET PARAMETER ID 'BLN' FIELD cursorvalue.
        SET PARAMETER ID 'BUK' FIELD wg_zglt035-bukrs.
        SET PARAMETER ID 'GJR' FIELD wg_zglt035-budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT wg_zglt035-belnr_e IS INITIAL AND  NOT cursorvalue IS INITIAL AND cursorfield = 'WG_ZGLT035-BELNR_E'.
        SET PARAMETER ID 'BLN' FIELD cursorvalue.
        SET PARAMETER ID 'BUK' FIELD wg_zglt035-bukrs.
        SET PARAMETER ID 'GJR' FIELD wg_zglt035-gjahr_e.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF wg_zglt035-reversao_status = icon_message_error. "130130 - CS2023000969 Gisele Follmann PSA

        DATA: reversao_obj_key TYPE zib_contabil_chv-obj_key,
              lv_message       TYPE string,
              dt               TYPE string,
              hr               TYPE string.
        CLEAR: reversao_obj_key,lv_message,dt,hr.
        reversao_obj_key = |{ wa_zib_contabil_chv-obj_key }R|.

        SELECT message FROM zib_contabil_err WHERE obj_key = @reversao_obj_key INTO TABLE @DATA(it_message).

        LOOP AT it_message ASSIGNING FIELD-SYMBOL(<msg_erro>).
          CONCATENATE lv_message <msg_erro> INTO lv_message SEPARATED BY space.
        ENDLOOP.

        SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @reversao_obj_key INTO @DATA(aux_message_erro).

        dt =  aux_message_erro-dt_atualizacao.
        hr =  aux_message_erro-hr_ATUALIZACAO.

        MESSAGE lv_message TYPE 'I'.

      ENDIF.
      IF NOT wa_zib_contabil_chv IS INITIAL AND  NOT cursorvalue IS INITIAL AND cursorfield = 'WG_ZGLT035-REVERSAO_DOC'. "130130 - CS2023000969 Gisele Follmann PSA
        SET PARAMETER ID 'BLN' FIELD cursorvalue.
        SET PARAMETER ID 'BUK' FIELD wg_zglt035-bukrs.
        SET PARAMETER ID 'GJR' FIELD wg_zglt035-budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VISAO'.
      IF x_visao = 'X'.
        CLEAR x_visao.
*        BTN_VISAO = '@KU@ Visão de Razão'.
        btn_visao = TEXT-b01.
      ELSE.
        x_visao = 'X'.
*        BTN_VISAO = '@KU@ Visão de Entrada'.
        btn_visao = TEXT-b02.
      ENDIF.
    WHEN c_col_exp.
      IF wg_colaps EQ '@K1@'.
        wg_colaps = '@K2@'.
      ELSE.
        wg_colaps = '@K1@'.
      ENDIF.
    WHEN c_deldoc.
      SELECT SINGLE * FROM zglt034 INTO wg_zglt034 WHERE lote EQ wg_zglt034-lote.
      IF wg_zglt034-status_lote = 'L'.
        MESSAGE s836(sd) DISPLAY LIKE 'I' WITH TEXT-e51 .
        EXIT.
      ENDIF.

      IF wg_zglt034-status_lote = 'A'.
        MESSAGE s836(sd) DISPLAY LIKE 'I' WITH TEXT-e52.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
          text_question         = TEXT-p01
          text_button_1         = TEXT-p02 "'Sim'(001)
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = TEXT-p03 "'Não'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF w_answer = '1'.
        PERFORM f_eliminar_lancamento.
      ENDIF.
    WHEN c_search.

      IF wg_acao EQ 'COPY'.

        PERFORM f_busca_dados.

        IF tg_zglt036[] IS INITIAL.
          PERFORM f_copiar_lancamento.
        ELSE."modifica

          PERFORM f_valida_contas.

          REFRESH: tg_fields.
          PERFORM f_trata_campos USING  space
                                        'GR2'
                                        c_1       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0

          PERFORM f_trata_campos USING  space
                                        'GR1'
                                        c_0       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0
          PERFORM f_trata_campos USING  space
                                        'GR3'
                                        c_0       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0

          CALL METHOD obg_descbox->set_readonly_mode
            EXPORTING
              readonly_mode = 1.
        ENDIF.
      ELSE.
        IF vg_carrega IS INITIAL.
          PERFORM f_busca_dados.
        ENDIF.
      ENDIF.
    WHEN c_displa.
      wg_acao = c_displa.
      PERFORM f_limpa_campos.
      REFRESH: tg_fields.
      PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                      'GR3'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
    WHEN c_save.
      IF wg_zglt035-belnr IS NOT INITIAL.
        MESSAGE TEXT-i01  TYPE 'I'.
        EXIT.
      ENDIF.
      CALL METHOD grid1->check_changed_data.
      PERFORM f_verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
        IF wg_zglt035-doc_lcto IS INITIAL.
          PERFORM  f_obtem_proximo.
        ENDIF.
        PERFORM f_grava_dados.

        CLEAR vg_chamada.

        REFRESH tg_fields.
        PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR3'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH TEXT-e59.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = space   "c_x
            i_repid       = sy-repid
            i_pressed_tab = 'TS_100_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.
    WHEN c_add.
      CHECK wg_acao <> c_add.
      wg_acao = c_add.  "c_modif.
      PERFORM:  f_limpa_campos.

      IF  v_lote15 IS NOT INITIAL.
        wg_zglt034-lote = v_lote15.
        CLEAR v_lote15.
      ENDIF.

      REFRESH: tg_fields.
      PERFORM f_trata_campos USING  space
                                    'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                    'GR1'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM f_trata_campos USING  space
                                    'GR3'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.

      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 0.

    WHEN c_atuali.

    WHEN c_modif.
      IF wg_zglt035-belnr IS NOT INITIAL.
        MESSAGE TEXT-i01 TYPE 'I'.
        EXIT.
      ENDIF.
      IF wg_acao = c_modif.
        CLEAR wg_acao.
        REFRESH: tg_fields.
        PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM f_trata_campos USING  space
                                      'GR3'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.

      ELSE.
        wg_acao = c_modif.
        PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM f_trata_campos USING  space
                                      'GR3'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 0.

      ENDIF.

      CLEAR: ok-code.
    WHEN c_show_msgre.
      PERFORM f_verifica_erros.
      " IF tg_msg_ret[] IS NOT INITIAL.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = c_x
          i_repid       = sy-repid
          i_popup       = 0
          i_pressed_tab = 'TS_100-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
          i_set_cell    = 'WG_CELL'
          i_set_obj     = 'WG_OBJ'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.
      "ENDIF.
    WHEN c_cancel.
      CLEAR wg_acao.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN 'SAIR'.
      IF grid4 IS NOT INITIAL.
        CALL METHOD grid4->free.

        IF obg_conteiner_vat IS NOT INITIAL.
          CALL METHOD obg_conteiner_vat->free.
        ENDIF.
        FREE: obg_conteiner_vat, grid4.
      ENDIF.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE ok-code.
    WHEN 'SAIR'.
      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->free.

        IF obg_conteiner_obj IS NOT INITIAL.
          CALL METHOD obg_conteiner_obj->free.
        ENDIF.
        FREE: obg_conteiner_obj, grid5.
      ENDIF.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
