*----------------------------------------------------------------------*
***INCLUDE ZMMR121_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok-code.
    WHEN 'AGRUPA'.
      IF  wg_zmmt0083-code IS INITIAL..
        MESSAGE 'Informe a Transação!' TYPE 'I'.
      ELSE.
        wg_acao = c_add.
        PERFORM f_busca_dados2.
        CALL SCREEN 0200.
      ENDIF.
    WHEN c_displa.
      wg_acao = c_displa.
      CLEAR: wg_zmmt0083.
      REFRESH: tg_saida.
      REFRESH: tg_fields.

      CASE sy-tcode.
        WHEN 'ZMM0120'.
          wg_zmmt0083-code = 'ZMM0124'.
        WHEN 'ZMM0177'.
          wg_zmmt0083-code = 'ZMM0177'.
        WHEN OTHERS.
          wg_zmmt0083-code = 'ZMM0134'.
      ENDCASE.

*      IF sy-tcode NE 'ZMM0120'.
*        wg_zmmt0083-code = 'ZMM0134'.
*      ELSE.
*        wg_zmmt0083-code = 'ZMM0124'.
*      ENDIF.
      PERFORM f_trata_campos USING  space
                                     'GR1'
                                     c_1       "INPUT 1     NO INPUT 0
                                     c_0.      "INVISIBLE 1 VISIBLE 0
    WHEN c_add.
      CHECK wg_acao <> c_add.

      wg_acao = c_add.  "c_modif.


      CLEAR: wg_zmmt0083-ds_gru_val, wg_zmmt0083-ds_gru_qtd.
      CASE sy-tcode.
        WHEN 'ZMM0120'.
          wg_zmmt0083-code = 'ZMM0124'.
        WHEN 'ZMM0177'.
          wg_zmmt0083-code = sy-tcode.
        WHEN OTHERS.
          wg_zmmt0083-code = 'ZMM0134'.
      ENDCASE.


*      IF sy-tcode NE 'ZMM0120'.
*        wg_zmmt0083-code = 'ZMM0134'.
*      ELSE.
*        wg_zmmt0083-code = 'ZMM0124'.
*      ENDIF.
      REFRESH: tg_saida.

      REFRESH: tg_fields.

      PERFORM f_trata_campos USING  space
                                    'GR1'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

    WHEN c_search.
      PERFORM f_busca_dados.
    WHEN c_save.

      CALL METHOD grid1->check_changed_data.
      PERFORM f_verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.

        PERFORM: f_grava_dados.

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e35.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = ''
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_show_msgre.
      PERFORM f_verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = c_x
          i_repid       = sy-repid
          i_pressed_tab = ''
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN c_displa.
      wg_acao = c_displa.
      REFRESH: tg_saida2.
      PERFORM f_busca_dados2.
      REFRESH: tg_fields.

    WHEN c_add.
      CHECK wg_acao <> c_add.

      wg_acao = c_add.  "c_modif.

*      REFRESH: TG_SAIDA2.
      REFRESH: tg_fields.

    WHEN c_search.
      PERFORM f_busca_dados2.
    WHEN c_save.

      CALL METHOD grid2->check_changed_data.
      PERFORM f_verifica_erros2.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.

        PERFORM: f_grava_dados2.

      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e35.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '200'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = ''
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_show_msgre.
      PERFORM f_verifica_erros2.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '200'
          i_show        = c_x
          i_repid       = sy-repid
          i_pressed_tab = ''
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    WHEN c_cancel.
      SET SCREEN 0.
    WHEN c_exit.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
