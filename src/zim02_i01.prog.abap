*&---------------------------------------------------------------------*
*&  Include           ZIM02_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  SELECT SINGLE ktext FROM cskt INTO cskt-ltext
  WHERE spras EQ sy-langu AND
        kostl EQ zim01_sol_ap_inv-kostl AND
        datbi GE sy-datum.

  IF sy-ucomm = 'CRIA'.
    w_up = w_cria = 'X'.
  ELSE.
    CLEAR: w_up, w_cria.
  ENDIF.

  w_hora = sy-uzeit.

  CASE sy-ucomm.

    WHEN 'CRIA' OR 'VIEW'.
      PERFORM f_busca_dados.
    WHEN 'DELE'.
      PERFORM f_apaga_registros.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  GET CURSOR FIELD w_field.
  GET CURSOR LINE w_line.

  CASE sy-ucomm.
    WHEN 'BACK'.
*      REFRESH t_inv.
*      LEAVE TO SCREEN 100.
      PERFORM f_volta.
    WHEN 'SAVE'.
      PERFORM f_salva_dados.

    WHEN 'TEXTO'.

      DATA v_line_est    TYPE i.
      CLEAR v_line_est.
      GET CURSOR LINE v_line_est.
      v_line_est = tc_inv-top_line + v_line_est - 1.

      PERFORM monta_texto USING w_up
                                v_line_est.

    WHEN 'MODI'.
      IF w_up NE 'V'.
        IF w_up IS INITIAL.
          CLEAR t_inv.

          DELETE t_inv WHERE        izwek        IS INITIAL AND
                                    objetivo     IS INITIAL AND
                                    descr_item   IS INITIAL AND
                                    menge        IS INITIAL AND
                                    vlr_unitario IS INITIAL AND
                                    dt_inicio    IS INITIAL AND
                                    dt_fim       IS INITIAL.

          DO 10 TIMES.
            APPEND t_inv.
          ENDDO.
          w_up = 'X'.
        ELSE.
          CLEAR w_up.
        ENDIF.
      ELSE.
        MESSAGE i000(z01) WITH 'Período Fechado para Planejamento'.
        LEAVE SCREEN.
      ENDIF.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0110  INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_INV'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_inv_modify INPUT.

  IF t_inv-knttp IS INITIAL.
    CLEAR t_inv-knttx.
  ENDIF.

  IF t_inv-saknr IS INITIAL.
    CLEAR  t_inv-txt20.
  ENDIF.

  IF t_inv-cod_gpo IS INITIAL.
    CLEAR t_inv-cod_item.
  ENDIF.

  t_inv-vlr_total = t_inv-vlr_unitario * t_inv-menge.

  IF t_inv-solicitacao_invest IS INITIAL. " BUG 63289 - CSB
    IF t_inv-tx_usd IS INITIAL.
      t_inv-tx_usd = zim02_sol_ap_ctl-tx_usd.
    ENDIF.

    IF t_inv-tx_eur IS INITIAL.
      t_inv-tx_eur = zim02_sol_ap_ctl-tx_eur.
    ENDIF.
  ENDIF.

  IF NOT t_inv-izwek IS INITIAL.
    SELECT SINGLE txt50 FROM t087j INTO t_inv-txt50
      WHERE spras = sy-langu AND
            izwek = t_inv-izwek.
  ENDIF.

  IF NOT t_inv-knttp IS INITIAL.
    SELECT SINGLE knttx FROM t163i INTO t_inv-knttx
    WHERE spras = sy-langu AND
          knttp = t_inv-knttp.
  ENDIF.
  IF t_inv-solicitacao_invest IS INITIAL. " BUG 63289 - CSB
    IF NOT t_inv-tx_usd IS INITIAL.
      t_inv-vl_usd = t_inv-vlr_total / t_inv-tx_usd.
    ENDIF.

    IF NOT t_inv-tx_eur IS INITIAL.
      t_inv-vl_eur = t_inv-vlr_total / t_inv-tx_eur.
    ENDIF.
  ENDIF.
  IF NOT t_inv-saknr IS INITIAL.

    SELECT SINGLE txt20 FROM skat INTO t_inv-txt20
        WHERE spras = sy-langu AND
              saknr = t_inv-saknr.
  ENDIF.


  IF t_inv-aprovador    IS INITIAL AND
     t_inv-dt_aprovacao IS INITIAL.
*    T_INV-APROVADOR = ZIM02_SOL_AP_CTL-APROVADOR.
  ENDIF.

  IF t_inv-dt_inicio < zim02_sol_ap_ctl-dt_inicio AND
     NOT t_inv-dt_inicio IS INITIAL.
    SET CURSOR FIELD 'T_INV-DT_INICIO' LINE sy-tabix.
    MESSAGE e000(z01) WITH 'Data inicio inválida para planejamento'.
*    LEAVE SCREEN.
  ENDIF.

  IF t_inv-dt_fim < zim02_sol_ap_ctl-dt_inicio AND
     NOT t_inv-dt_fim IS INITIAL .
    SET CURSOR FIELD 'T_INV-DT_FIM' LINE sy-tabix.
    MESSAGE e000(z01) WITH 'Data fim inválida para planejamento'.
*    LEAVE SCREEN.

  ENDIF.



  IF t_inv-moeda IS INITIAL.
    t_inv-moeda = 'BRL'.
  ENDIF.

  IF sy-tcode = 'ZIM05'.
    t_inv-data_entr_im = sy-datum.
    t_inv-usuario_im   = sy-uname.

    IF t_inv-status_aprov EQ '2'.
      CLEAR t_inv-status_aprov.
      CLEAR t_inv-dt_aprovacao.
    ENDIF.
  ENDIF.

  IF sy-tcode = 'ZIM02'.
* Início Alteração Ricardo Furst.
    IF t_inv-hora_entr IS INITIAL.
      t_inv-data_entr = sy-datum.
      t_inv-hora_entr = w_hora.
      t_inv-usuario  = sy-uname.
    ELSE.
      t_inv-data_mod = sy-datum.
      t_inv-hora_mod = w_hora.
      t_inv-usuario  = sy-uname.
    ENDIF.
* Fim Alteração Ricardo Furst.
    IF t_inv-status_cta = '3'.
      CLEAR  t_inv-status_cta.
    ENDIF.
  ENDIF.

  IF t_inv-fase IS INITIAL.
    t_inv-fase = zim02_sol_ap_ctl-fase.
  ENDIF.
  MODIFY t_inv
    INDEX tc_inv-current_line.
  IF sy-subrc <> 0.
    t_inv-fase = zim02_sol_ap_ctl-fase.
    APPEND t_inv.
  ENDIF.

*Início Alteração Ricardo Furst.
  IF NOT t_inv-izwek IS INITIAL.
    SELECT SINGLE izwek
      FROM t087i
      INTO v_izwek
      WHERE izwek EQ t_inv-izwek.
    IF sy-subrc NE 0.

      MESSAGE s000(z01) WITH 'Valor incorreto para Categoria do Investimento.'.
      CALL SCREEN 0110.
    ENDIF.
  ENDIF.
*Fim Alteração Ricardo Furst.

  w_save = 'X'.
ENDMODULE.                    "TC_INV_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_INV'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_inv_mark INPUT.
  DATA: g_tc_inv_wa2 LIKE LINE OF t_inv.
  IF tc_inv-line_sel_mode = 1
  AND t_inv-flag = 'X'.
    LOOP AT t_inv INTO g_tc_inv_wa2
      WHERE flag = 'X'.
      g_tc_inv_wa2-flag = ''.
      MODIFY t_inv
        FROM g_tc_inv_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY t_inv
    INDEX tc_inv-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC_INV_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_INV'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_inv_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_INV'
                              'T_INV'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC_INV_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  M_SAIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_sair INPUT.

  IF sy-ucomm = 'BACK'.
    LEAVE PROGRAM.
  ENDIF.

  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_INV'
                              'T_INV'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                 " M_SAIR  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_CONS_CAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_cons_cat INPUT.

*Início Alteração Ricardo Furst.
  IF NOT t_inv-izwek IS INITIAL.
    SELECT SINGLE izwek
      FROM t087i
      INTO v_izwek
      WHERE izwek EQ t_inv-izwek.
    IF sy-subrc NE 0.

      MESSAGE s000(z01) WITH 'Valor incorreto para Categoria do Investimento.'.
      CALL SCREEN 0110.
    ENDIF.
  ENDIF.
*Fim Alteração Ricardo Furst.

ENDMODULE.                 " TC_CONS_CAT  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_COD_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_cod_item INPUT.
*break-point.

  IF NOT t_inv-cod_item IS INITIAL.
    READ TABLE tg_11 WITH KEY cod_item = t_inv-cod_item
                              cod_gpo  = t_inv-cod_gpo.
    IF sy-subrc <> 0.
      MESSAGE e000(z01) WITH 'Código do item inválido para o grupo:' t_inv-cod_gpo.
    ELSE.
      IF sy-tcode NE 'ZIM05'.
        IF sy-tcode NE 'ZIM02'.
          t_inv-descr_item   = tg_11-descr_item.
        ENDIF.
        t_inv-status_cta   = tg_11-status_cta.
        t_inv-knttp        = tg_11-knttp.
        t_inv-knttx        = tg_11-knttx.
        t_inv-observacoes  = tg_11-observacoes.
        t_inv-saknr        = tg_11-saknr.
        t_inv-txt20        = tg_11-txt20.


        READ TABLE tg_10 WITH KEY cod_gpo = t_inv-cod_gpo BINARY SEARCH.
        IF tg_10-cod_lib EQ 'X'. "Se for liberado, não faz nada

        ELSE.
          t_inv-vlr_unitario = tg_11-preco_item.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    READ TABLE tg_10 WITH KEY cod_gpo = t_inv-cod_gpo BINARY SEARCH.
    IF tg_10-cod_lib IS INITIAL. "Se for liberado, não faz nada
      CLEAR: t_inv-descr_item, t_inv-status_cta, t_inv-knttp, t_inv-knttx,
             t_inv-observacoes, t_inv-saknr, t_inv-txt20, t_inv-vlr_unitario.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDA_COD_ITEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_COD_GPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_cod_gpo INPUT.

  IF NOT t_inv-cod_gpo IS INITIAL.
    READ TABLE tg_10 WITH KEY cod_gpo = t_inv-cod_gpo.
    IF sy-subrc <> 0.
      MESSAGE e000(z01) WITH 'Código do grupo:' t_inv-cod_item.
    ENDIF.
  ENDIF.
ENDMODULE.                 " VALIDA_COD_GPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_GRUPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_grupo INPUT.
  IF sy-ucomm EQ 'ENTE'.
    IF sy-tcode EQ 'ZIM02'.
      IF t_inv-cod_gpo IS INITIAL.
        MESSAGE e836(sd) WITH 'Preencher o grupo corretamente.'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " VALIDA_GRUPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_VALIDA_CAMPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_valida_campo INPUT.
  DATA: wl_field(20).

  CLEAR: wl_field.

  IF t_inv-dt_inicio  IS INITIAL
  OR t_inv-dt_fim     IS INITIAL
  OR t_inv-objetivo   IS INITIAL
  OR t_inv-descr_item IS INITIAL
  OR t_inv-menge      IS INITIAL
  OR t_inv-vlr_unitario IS INITIAL
  OR t_inv-izwek IS INITIAL
  OR t_inv-finalidade IS INITIAL
  AND t_inv-cod_gpo IS NOT INITIAL
  AND sy-tcode EQ 'ZIM02'.

    IF sy-ucomm EQ 'ENTE'.
*      READ TABLE tg_10 WITH KEY cod_gpo = t_inv-cod_gpo.
*      IF tg_10-cod_lib IS NOT INITIAL.
      GET CURSOR FIELD wl_field.
      IF wl_field NE 'T_INV-COD_GPO'.
        MESSAGE e000(z01) WITH 'É obrigatorio o preenchimento dos campos'.
      ENDIF.
*        LEAVE SCREEN.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " TC_VALIDA_CAMPO  INPUT
