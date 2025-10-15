*----------------------------------------------------------------------*
***INCLUDE MZIMP01_USER_COMMAND_0001I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  IF '|BACK|EXIT|CANCEL' CS v_okcode AND
     NOT sy-fdpos IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.


  SELECT SINGLE *
    INTO w_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs      = w_zimp_cabecalho-bukrs      AND
          nro_doc_tr = w_zimp_cabecalho-nro_doc_tr AND
          gjahr      = w_zimp_cabecalho-gjahr
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 INICIO.
*    AND   belnr      = space
    AND   ( estorno    = 'X' OR estorno = space ).
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 FIM.
  IF sy-subrc <> 0.
    MESSAGE text-e01 TYPE c_e.
  ELSEIF NOT w_zimp_cabecalho-belnr IS INITIAL
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 INICIO.
         AND w_zimp_cabecalho-estorno IS INITIAL.
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 FIM.
*    MESSAGE text-e02 TYPE c_e.
  ENDIF.

  IF sy-subrc = 0.

    SELECT *
      INTO TABLE t_tc_detalhes
      FROM zimp_detalhe
    WHERE bukrs      = w_zimp_cabecalho-bukrs      AND
          nro_doc_tr = w_zimp_cabecalho-nro_doc_tr AND
          gjahr      = w_zimp_cabecalho-gjahr.

  ENDIF.

  LOOP AT t_tc_detalhes INTO w_zimp_detalhe.

    MODIFY t_tc_detalhes FROM w_zimp_detalhe TRANSPORTING lifnr.

  ENDLOOP.


* Descrição Fornecedor
  SELECT SINGLE name1
    INTO v_cab_txt_cta_forn
    FROM lfa1
    WHERE lifnr = w_zimp_cabecalho-lifnr.

  SELECT SINGLE ktopl
    INTO v_ktopl
    FROM t001
    WHERE bukrs = w_zimp_cabecalho-bukrs.

* Descrição Cta. Imposto
  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_imposto
                              v_cab_txt_cta_imp.

* Descrição Cta. Juros
  IF NOT w_zimp_cabecalho-cta_juros IS INITIAL.

    PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_juros
                                v_cab_txt_cta_jur.

  ENDIF.

* Descrição Cta. Multa
  IF NOT w_zimp_cabecalho-cta_multa IS INITIAL.

    PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_multa
                                v_cab_txt_cta_mul.

  ENDIF.

* Início Alteração Ricardo Furst 18.07.2009
  IF NOT w_zimp_cabecalho-cta_at_mon IS INITIAL.

    PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_at_mon
                                v_cab_txt_cta_mon.

  ENDIF.

  IF NOT w_zimp_cabecalho-cta_tse IS INITIAL.

    PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_tse
                                v_cab_txt_cta_tse.

  ENDIF.
* Fim Alteração Ricardo Furst 18.07.2009

  SELECT SINGLE arrecadacao cod_barras
                dt_apuracao mes_apuracao
    INTO (v_cab_txt_tip_imp, v_imp_cod_barras,
          v_imp_dt_apuracao, v_imp_mes_apuracao)
    FROM zimp_tipos_impos
    WHERE tp_arrec = w_zimp_cabecalho-tp_arrec.

  LEAVE TO SCREEN 0100.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_TC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_tc INPUT.

  w_zimp_detalhe-modif = c_x.

  MODIFY t_tc_detalhes INDEX tc_detalhes-current_line
    FROM w_zimp_detalhe.

  IF sy-subrc <> 0.
    APPEND w_zimp_detalhe TO t_tc_detalhes.
  ENDIF.

  v_total_multa = v_total_multa + w_zimp_detalhe-vlr_multa.
  v_total_juros = v_total_juros + w_zimp_detalhe-vlr_juros.

ENDMODULE.                 " ATUALIZA_TC  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_user_command INPUT.

  DATA:
    v_answer TYPE c.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Abandonar transação'
      text_question         = 'Deseja abandonar o lançamento ?'
      display_cancel_button = space
    IMPORTING
      answer                = v_answer
    EXCEPTIONS
      OTHERS                = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF v_answer = '1'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " EXIT_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF '|BACK|EXIT' CS v_okcode AND
      NOT sy-fdpos IS INITIAL.

    v_answer = '1'.

    READ TABLE t_tc_detalhes TRANSPORTING NO FIELDS
      WITH KEY modif = c_x.

    IF sy-subrc = 0.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Gravar dados'
          text_question         = 'Deseja abandonar as modificações ?'
          display_cancel_button = space
        IMPORTING
          answer                = v_answer
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

    IF v_answer = '1'.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDIF.

  IF v_okcode = 'MORE'.
*    CLEAR tc_detalhes-lines.
    IF tc_detalhes-lines IS INITIAL.
      tc_detalhes-lines = 12.
    ELSE.
      tc_detalhes-lines = tc_detalhes-lines + 2.
    ENDIF.
  ENDIF.

  IF v_okcode = 'DEL'.
    LOOP AT t_tc_detalhes INTO w_zimp_detalhe.
      IF NOT w_zimp_detalhe-mark IS INITIAL.

*        DELETE FROM zimp_detalhe WHERE bukrs      = w_zimp_detalhe-bukrs      AND
*                                       nro_doc_tr = w_zimp_detalhe-nro_doc_tr AND
*                                       gjahr      = w_zimp_detalhe-gjahr      AND
*                                       buzei      = w_zimp_detalhe-buzei.

        DELETE t_tc_detalhes.
        APPEND w_zimp_detalhe TO t_det_aux.
      ENDIF.
    ENDLOOP.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF v_okcode = 'SAVE'.

    IF sy-tcode = c_tcode_criar.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = 'ZT'
          object      = 'RF_BELEG'
          subobject   = w_zimp_cabecalho-bukrs
        IMPORTING
          number      = w_zimp_cabecalho-nro_doc_tr
        EXCEPTIONS
          OTHERS      = 1.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      w_zimp_cabecalho-gjahr = sy-datum(4).
      w_zimp_cabecalho-blart = 'TB'.


      IF v_imp_dt_apuracao IS INITIAL.

        " Conforme solicitação chamado
        DATA:   day(2)     TYPE c,
                month(2)   TYPE c,
                year(4)    TYPE c.

        month = w_zimp_cabecalho-mes_ano_comp+4(2) .

        IF month EQ 13.

          year = w_zimp_cabecalho-mes_ano_comp(4).

          CONCATENATE year '12' '01' INTO w_zimp_cabecalho-bldat..
          " Fim Alteração
        ELSE.

          CONCATENATE w_zimp_cabecalho-mes_ano_comp '01'
            INTO w_zimp_cabecalho-bldat.

        ENDIF.

        CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
          EXPORTING
            i_date = w_zimp_cabecalho-bldat
          IMPORTING
            e_date = w_zimp_cabecalho-bldat.

        CLEAR:  day,
                month,
                year.

      ELSE.

        w_zimp_cabecalho-bldat = w_zimp_cabecalho-dt_per_apur.

      ENDIF.


      w_zimp_cabecalho-dt_entrada = sy-datum.
      w_zimp_cabecalho-hr_entrada = sy-uzeit.

    ENDIF.

    break abap.
    LOOP AT t_tc_detalhes INTO w_zimp_detalhe.

      w_zimp_detalhe-bukrs      = w_zimp_cabecalho-bukrs.
      w_zimp_detalhe-nro_doc_tr = w_zimp_cabecalho-nro_doc_tr.
      w_zimp_detalhe-gjahr      = w_zimp_cabecalho-gjahr.
      IF sy-tcode = c_tcode_criar.
        w_zimp_detalhe-buzei      = sy-tabix.
      ENDIF.
* Início Alteração Ricardo Furst 20.07.2009
      w_zimp_detalhe-usuario_apr = sy-uname.
* Fim Alteração Ricardo Furst 20.07.2009

      MODIFY t_tc_detalhes FROM w_zimp_detalhe TRANSPORTING bukrs nro_doc_tr gjahr buzei usuario_apr.

    ENDLOOP.
* Início Alteração Ricardo Furst 20.07.2009
    w_zimp_cabecalho-usnam = sy-uname.
* Fim Alteração Ricardo Furst 20.07.2009

    LOOP AT t_det_aux INTO w_zimp_detalhe .
      DELETE FROM zimp_detalhe WHERE bukrs      = w_zimp_detalhe-bukrs      AND
                                     nro_doc_tr = w_zimp_detalhe-nro_doc_tr AND
                                     gjahr      = w_zimp_detalhe-gjahr      AND
                                     buzei      = w_zimp_detalhe-buzei.
    ENDLOOP.
    COMMIT WORK AND WAIT.
    MODIFY: zimp_cabecalho FROM w_zimp_cabecalho,
            zimp_detalhe   FROM TABLE t_tc_detalhes.

    COMMIT WORK AND WAIT.

    MESSAGE s398(00) WITH 'Documento' w_zimp_cabecalho-nro_doc_tr
                          'registrado na empresa' w_zimp_cabecalho-bukrs.

    LEAVE TO TRANSACTION sy-tcode.

  ENDIF.

  CLEAR v_okcode.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CTA_MULTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cta_multa INPUT.

  CHECK v_okcode = 'SAVE'.

  IF v_total_multa > 0 AND w_zimp_cabecalho-cta_multa IS INITIAL.
    MESSAGE 'Conta Multa obrigatório pois existe valor de multa!' TYPE c_e.
  ENDIF.

ENDMODULE.                 " VERIF_CTA_MULTA  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CTA_JUROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cta_juros INPUT.

  CHECK v_okcode = 'SAVE'.

  IF v_total_juros > 0 AND w_zimp_cabecalho-cta_juros IS INITIAL.
    MESSAGE 'Conta Juros obrigatório pois existe valor de juros!' TYPE c_e.
  ENDIF.

ENDMODULE.                 " VERIF_CTA_JUROS  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_mark INPUT.

  MODIFY t_tc_detalhes INDEX tc_detalhes-current_line
    FROM w_zimp_detalhe TRANSPORTING mark.

ENDMODULE.                 " SET_MARK  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_lifnr INPUT.

  SELECT SINGLE name1
    INTO v_cab_txt_cta_forn
    FROM lfa1
    WHERE lifnr = w_zimp_cabecalho-lifnr.

  CHECK sy-subrc <> 0.

  MESSAGE 'Fornecedor inconsistente!' TYPE c_e.

ENDMODULE.                 " VERIF_CONS_LIFNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_CTA_IMPOSTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_cta_imposto INPUT.

  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_imposto
                              v_cab_txt_cta_imp.

ENDMODULE.                 " VERIF_CONS_CTA_IMPOSTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_CTA_JUROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_cta_juros INPUT.

  CLEAR v_cab_txt_cta_jur.

  CHECK NOT w_zimp_cabecalho-cta_juros IS INITIAL.

  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_juros
                              v_cab_txt_cta_jur.

ENDMODULE.                 " VERIF_CONS_CTA_JUROS  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_TP_ARREC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_tp_arrec INPUT.

  SELECT SINGLE arrecadacao cod_barras
                dt_apuracao mes_apuracao
    INTO (v_cab_txt_tip_imp, v_imp_cod_barras,
          v_imp_dt_apuracao, v_imp_mes_apuracao)
    FROM zimp_tipos_impos
    WHERE tp_arrec = w_zimp_cabecalho-tp_arrec.

  CHECK sy-subrc <> 0.

  MESSAGE 'Tipo de Imposto inexistente!' TYPE c_e.

ENDMODULE.                 " VERIF_CONS_TP_ARREC  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_BUKRS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_bukrs INPUT.

  SELECT SINGLE ktopl
    INTO v_ktopl
    FROM t001
    WHERE bukrs = w_zimp_cabecalho-bukrs.

  CHECK sy-subrc <> 0.

  MESSAGE 'Empresa inexistente!' TYPE c_e.

ENDMODULE.                 " VERIF_CONS_BUKRS  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_CTA_MULTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_cta_multa INPUT.

  CLEAR v_cab_txt_cta_mul.

  CHECK NOT w_zimp_cabecalho-cta_multa IS INITIAL.

  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_multa
                              v_cab_txt_cta_mul.

ENDMODULE.                 " VERIF_CONS_CTA_MULTA  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_LIFNR_DET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_lifnr_det INPUT.

  CHECK NOT w_zimp_detalhe-lifnr IS INITIAL.

  SELECT SINGLE name1
    INTO v_desc_lifnr
    FROM lfa1
    WHERE lifnr = w_zimp_cabecalho-lifnr.

  CHECK sy-subrc <> 0.

  MESSAGE 'Fornecedor inconsistente!' TYPE c_e.

ENDMODULE.                 " VERIF_CONS_LIFNR_DET  INPUT
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DESC_LIFNR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_desc_lifnr OUTPUT.

  CLEAR v_desc_lifnr.

  CHECK NOT w_zimp_detalhe-lifnr IS INITIAL.

  SELECT SINGLE name1
    INTO v_desc_lifnr
    FROM lfa1
    WHERE lifnr = w_zimp_cabecalho-lifnr.

  IF w_zimp_detalhe-cod_ident IS INITIAL.

    SELECT SINGLE stcd1
      INTO w_zimp_detalhe-cod_ident
      FROM lfa1
      WHERE lifnr = w_zimp_cabecalho-lifnr.

  ENDIF.

ENDMODULE.                 " ATUALIZA_DESC_LIFNR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_CTA_MON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_cta_mon INPUT.

  CLEAR v_cab_txt_cta_mon.

  CHECK NOT w_zimp_cabecalho-cta_at_mon IS INITIAL.

  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_at_mon
                              v_cab_txt_cta_mon.

ENDMODULE.                 " VERIF_CONS_CTA_MON  INPUT
*&---------------------------------------------------------------------*
*&      Module  VERIF_CONS_CTA_TSE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verif_cons_cta_tse INPUT.

  CLEAR v_cab_txt_cta_tse.

  CHECK NOT w_zimp_cabecalho-cta_tse IS INITIAL.

  PERFORM zf_verif_skat USING w_zimp_cabecalho-cta_tse
                              v_cab_txt_cta_tse.

ENDMODULE.                 " VERIF_CONS_CTA_TSE  INPUT
