*&---------------------------------------------------------------------*
*&  Include           MZLES001F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  LE_LOG_CONTAINER_700
*&---------------------------------------------------------------------*

FORM le_log_container_700 .

  CALL METHOD vg_log_edit_700->get_text_as_r3table
    IMPORTING
      table       = ti_700_log
      is_modified = vg_log_status_700.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                     " LE_LOG_CONTAINER_700

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GERA_LOGLACTO_COCKPIT
*&---------------------------------------------------------------------*
FORM lancto_gera_loglacto_cockpit .

  DATA: lt_log_zib  TYPE STANDARD TABLE OF zib_contabil_err
                         WITH HEADER LINE INITIAL SIZE 0.

  DATA: lc_chave         TYPE epsfilnam,
        ln_idctrl        TYPE zidctrl,
        ln_vcont         TYPE numc10,
        ld_data_contabil TYPE d,
        lc_data_char     TYPE char10.

* Gera data para pesquisa no log (Um dia antes da data atual)
  PERFORM yf_calc_date USING sy-datum '01' '00' '00' '-'
                    CHANGING ld_data_contabil.

* Obtem registros de log contábil
  SELECT *
    INTO TABLE lt_log_zib
    FROM zib_contabil_err
   WHERE obj_key = ti_obj_contabil-obj_key.

* Gera controle para última chave de erro no log
  lc_chave = sy-repid.

  SELECT MAX( idctrl ) MAX( cont )
    INTO (ln_idctrl, ln_vcont)
    FROM zlest0008
   WHERE filename = lc_chave
    GROUP BY idctrl.
  ENDSELECT.

* Gera sequencia de controle de erro
  IF sy-subrc IS INITIAL.
    IF ln_vcont >= '9999999000'.
      ADD 1 TO ln_idctrl.
      CLEAR ln_vcont.
    ENDIF.
  ELSE.
    ADD 1 TO ln_idctrl.
  ENDIF.

* Transfere o erro para area de LOG
  CLEAR zlest0008.

  zlest0008-filename = lc_chave.
  zlest0008-idctrl   = ln_idctrl.
  zlest0008-tcode    = sy-tcode.
  zlest0008-cont     = ln_vcont.
  zlest0008-msgtyp   = cc_e.
  zlest0008-msgspra  = sy-langu.
  zlest0008-msgid    = 'FR'.
  zlest0008-msgnr    = '999'.
  zlest0008-data     = sy-datum.
  zlest0008-hora     = sy-uzeit.
  zlest0008-usuario  = sy-uname.
  zlest0008-lote     = ti_obj_contabil-lote.

  ADD 1 TO ln_vcont.
  zlest0008-cont  = ln_vcont.
  WRITE sy-datum TO lc_data_char.

  IF lt_log_zib[] IS INITIAL.
    CONCATENATE lc_data_char ': ' TEXT-m09
           INTO zlest0008-msgv1.
    INSERT zlest0008.
  ELSE.
    CONCATENATE lc_data_char ': ' TEXT-m11
           INTO zlest0008-msgv1.
    LOOP AT lt_log_zib.
      ADD 1 TO ln_vcont.
      zlest0008-cont  = ln_vcont.
      zlest0008-msgv1 = lt_log_zib-message.
      INSERT zlest0008.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " LANCTO_GERA_LOGLACTO_COCKPIT

*&---------------------------------------------------------------------*
*&      Form  NOVOS_CONHECIMENTOS_200
*&---------------------------------------------------------------------*
FORM novos_conhecimentos_200 .

  DATA: lr_status         TYPE lxhme_range_c1_t,
        lw_status         TYPE LINE OF lxhme_range_c1_t,
        wa_cockpit_lancto TYPE zles_cockpit_lancto.

* Campo status de preenchimento obrigatório
  lw_status-sign   = cc_i.
  lw_status-option = cc_eq.
  lw_status-low    = p_stats.
  APPEND lw_status TO lr_status.

* Reseta valores de abas
  REFRESH: ti_203_lotes,
           ti_300_lacto,
           ti_400_confer,
           ti_700_log.

  CLEAR: ti_400_confer,
         ti_700_log,
         vg_ctrl_inout_1200,
         sheader_300,
         stotais_lancto_203,
         sdetlh_confer_400,
         snlancto_confer_400,
         sctrl_saldo_400,
         slog_700.

* Busca conhecimentos, use [] para manter compatibilidade parâmetros
  CALL FUNCTION 'Z_LES_COCKPIT_AUTOMACAO_POSTO'
    EXPORTING
      rt_trasnportador = s_trans[]
      rt_posto         = s_posto[]
      rt_lote          = s_lote[]
      rt_conhecimento  = s_conhe[]
*     rt_carta_frete   =
      rt_periodo       = s_perio[]
      rt_fechamento    = s_fecham[]
      rt_vencimento    = s_venci[]
      rt_status        = lr_status
    IMPORTING
      e_msgerr         = vg_msgerro
    TABLES
      t_lotes          = ti_cockpit_lote
      t_lanctos        = ti_cockpit_lancto
      t_deltas         = ti_cockpit_deltas
      t_confer         = ti_cockpit_confer
      t_acrdecr        = ti_cockpit_acrdecr.

  IF NOT vg_msgerro IS INITIAL.
    MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_e.
    EXIT.
  ENDIF.

  vg_ctrl_inout_1200 = cc_x.

* Reseta valores de subtotais
  CLEAR: stotais_lote_203.

* Alimenta tabela table control - tela 200
  LOOP AT ti_cockpit_lote INTO vg_wa_lote.
    MOVE-CORRESPONDING vg_wa_lote TO ti_203_lotes.
    CLEAR: ti_203_lotes-acrdecr,
           ti_203_lotes-mark.
    IF vg_wa_lote-docsapadto IS INITIAL.
      READ TABLE ti_cockpit_acrdecr
            WITH KEY codtrp   = vg_wa_lote-codtrp
                     codposto = vg_wa_lote-codposto
            TRANSPORTING NO FIELDS
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ti_203_lotes-acrdecr = cc_x.
      ENDIF.
    ELSE.
      SELECT SINGLE gjahr bukrs
        INTO (ti_203_lotes-gjahr, ti_203_lotes-bukrs)
        FROM zlest0015
       WHERE transportador = vg_wa_lote-codtrp
         AND posto         = vg_wa_lote-codposto
         AND lote          = vg_wa_lote-lote.
    ENDIF.
    APPEND ti_203_lotes.
*   Totaliza subtotais
    ADD ti_203_lotes-vlrimportado   TO stotais_lote_203-tot_vimportado.
    ADD ti_203_lotes-vlrconfirmado  TO stotais_lote_203-tot_vconfirmado.
    ADD ti_203_lotes-vlrrecusado    TO stotais_lote_203-tot_vrecusado.
    ADD ti_203_lotes-vlrrealizado   TO stotais_lote_203-tot_vrealizado.
    ADD ti_203_lotes-vlr_acrec_desc TO stotais_lote_203-tot_vlr_acrec_desc.
    ADD ti_203_lotes-vlr_a_pagar    TO stotais_lote_203-tot_vlr_a_pagar.
  ENDLOOP.

  SORT ti_203_lotes BY dscodposto.
  DESCRIBE TABLE ti_203_lotes LINES vg_sbs203_tabcontrol-lines.

ENDFORM.                    " NOVOS_CONHECIMENTOS_200

*&---------------------------------------------------------------------*
*&      Form  SALVA_VALORES_SELECAO_1200
*&---------------------------------------------------------------------*
FORM salva_valores_selecao_1200 .

* stela_1200-range_codtrp[]   = s_trans[].
* stela_1200-range_codposto[] = s_posto[].
* stela_1200-range_lote[]     = s_lote[].

  stela_1200-status              = p_stats.
  stela_1200-range_conhec[]      = s_conhe[].
  stela_1200-range_periodo[]     = s_perio[].
  stela_1200-range_fechamento[]  = s_fecham[].
  stela_1200-range_vencimento[]  = s_venci[].

ENDFORM.                    " SALVA_VALORES_SELECAO_1200

*&---------------------------------------------------------------------*
*&      Form  RESETA_VLRSALVO_SELECAO_1200
*&---------------------------------------------------------------------*
FORM reseta_vlrsalvo_selecao_1200 .

* REFRESH: stela_1200-range_codtrp,
*          stela_1200-range_codposto,
*          stela_1200-range_lote.

  REFRESH: stela_1200-range_conhec,
           stela_1200-range_periodo,
           stela_1200-range_fechamento.

  CLEAR stela_1200-status.

ENDFORM.                    " RESETA_VLRSALVO_SELECAO_1200

*&---------------------------------------------------------------------*
*&      Form  RENOVA_TELA_PARA_PESQUISA
*&---------------------------------------------------------------------*
FORM renova_tela_para_pesquisa .

* Controles de tela
  CLEAR: vg_ctrl_inout_1200,
         vg_indx_selec_200,
         vg_existe_log_200.

* Dados de exibição header/detalhe
  REFRESH: ti_203_lotes,
           ti_300_lacto.

* Aba default
  vg_dynnr_tabstrip = '0200'.
  vg_main100_tabstrip-activetab = cc_100_tabstrip-tab1.

  PERFORM libera_lotes_usuario.

ENDFORM.                    " RENOVA_TELA_PARA_PESQUISA

*&---------------------------------------------------------------------*
*&      Form  CHECK_COMMAND_TABSTRIP
*&---------------------------------------------------------------------*
FORM check_command_tabstrip .

  CHECK:  vg_save_ok(4) = 'TAB_'.

  CASE vg_save_ok.
    WHEN cc_100_tabstrip-tab1.
      vg_dynnr_tabstrip = '0200'.
    WHEN cc_100_tabstrip-tab2.
      vg_dynnr_tabstrip = '0300'.
    WHEN cc_100_tabstrip-tab3.
      vg_dynnr_tabstrip = '0400'.
    WHEN cc_100_tabstrip-tab4.
      vg_dynnr_tabstrip = '0700'.
  ENDCASE.

* Controle de obrigatoriedade para seleção antes da navegação
  IF vg_save_ok <> cc_100_tabstrip-tab1.

    CLEAR: vg_indx_selec_200,   "Salva a Linha selecionada
           vg_existe_log_200,   "Salva flag de identificação de Log
           vg_msgerro.          "Controle de mensagem de erro

*   Extrai a linha selecionada no lote (table-control de seleção única)
    CLEAR vg_index.
    LOOP AT ti_203_lotes WHERE mark = cc_x.
      vg_indx_selec_200 = sy-tabix.
      ADD 1 TO vg_index.
    ENDLOOP.

    IF vg_index = 1.
*     READ TABLE ti_203_lotes INDEX vg_indx_selec_200.
      IF NOT ti_203_lotes-icon_log IS INITIAL.
        vg_existe_log_200 = cc_x.
      ENDIF.
    ELSE.
      CLEAR vg_indx_selec_200.
    ENDIF.

*   Verifica a possibilidade de navegação para detalhamentos do lote
    IF vg_dynnr_tabstrip = '0300' AND vg_indx_selec_200 <= 0.
      vg_msgerro = TEXT-m01.
    ELSEIF vg_dynnr_tabstrip = '0400' AND vg_indx_selec_200 <= 0.
      vg_msgerro = TEXT-m02.
    ELSEIF vg_dynnr_tabstrip = '0700'.
      IF vg_indx_selec_200 <= 0 OR vg_existe_log_200 IS INITIAL.
        vg_msgerro = TEXT-m03.
      ENDIF.
    ENDIF.

*   Nenhuma ocorrência selecionada e/ou válida para detalhemento
    IF NOT vg_msgerro IS INITIAL.
      vg_dynnr_tabstrip = '0200'.
      vg_save_ok = cc_100_tabstrip-tab1.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
    ENDIF.

  ENDIF.

* Identifica a ABA default
  vg_main100_tabstrip-activetab = vg_save_ok.
  CLEAR vg_save_ok.

ENDFORM.                    " CHECK_COMMAND_TABSTRIP

*&---------------------------------------------------------------------*
*&      Form  ALTERA_STATBL_LIBERADO_203
*&---------------------------------------------------------------------*
FORM altera_statbl_liberado_203 .

  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.
    CLEAR vg_msgerro.

    IF ti_203_lotes-status <> cc_importado.
      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m05
             INTO vg_msgerro SEPARATED BY space.
      REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
               WITH ti_203_lotes-status.
    ENDIF.

*   Verifica inconsistências
    IF NOT vg_msgerro IS INITIAL.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
      EXIT.
    ENDIF.

*   Atualiza status BL - Lotes
*   O status é atualizado somente em memória - esta informação
*   não econtra-se na tabela transparente ZLEST0013 (BL)
*   Se for acrescentado usar rotina CONVERSION_EXIT_ZCNPJ_INPUT/OUTPUT
*   para acesso da chave CNPJ

*   Atualiza table control
    ti_203_lotes-bl          = cc_liberado.     "Modifica status
    ti_203_lotes-mark        = space.           "Desmarca seleção OK

    MODIFY ti_203_lotes INDEX vg_index TRANSPORTING bl mark.

  ENDLOOP.

  PERFORM libera_lotes_usuario.

ENDFORM.                    " ALTERA_STATBL_LIBERADO_203

*&---------------------------------------------------------------------*
*&      Form  ALTERA_STATBL_BLOQUEADO_203
*&---------------------------------------------------------------------*
FORM altera_statbl_bloqueado_203 .

  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.
    CLEAR vg_msgerro.

    IF ti_203_lotes-status <> cc_importado.
      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m05
             INTO vg_msgerro SEPARATED BY space.
      REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
               WITH ti_203_lotes-status.
    ENDIF.

*   Verifica inconsistências
    IF NOT vg_msgerro IS INITIAL.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
      EXIT.
    ENDIF.

*   Atualiza status BL - Lotes
*   O status é atualizado somente em memória - esta informação
*   não econtra-se na tabela transparente ZLEST0013 (BL)
*   Se for acrescentado usar rotina CONVERSION_EXIT_ZCNPJ_INPUT/OUTPUT
*   para acesso da chave CNPJ

*   Atualiza table control
    ti_203_lotes-bl          = cc_bloqueado.   "Modifica status
    ti_203_lotes-mark        = space.           "Desmarca seleção OK

    MODIFY ti_203_lotes INDEX vg_index TRANSPORTING bl mark.
  ENDLOOP.

  PERFORM libera_lotes_usuario.

ENDFORM.                    " ALTERA_STATBL_BLOQUEADO_203

*&---------------------------------------------------------------------*
*&      Form  GERA_LANCTO_CONTABIL_203
*&---------------------------------------------------------------------*
FORM gera_lancto_contabil_203 .

  DATA: lp_vlr_adiantamento TYPE kwert,
        lf_ha_doc_ok.

* Grava registros na tabela de lançamento contábil
  PERFORM lancto_grava_dados_contabil.
  CHECK: NOT ti_obj_contabil[] IS INITIAL.

* Gera documento contábil
* - Executa report de criacao de documento ( nao necessario mais )
  "PERFORM lancto_gera_docto_contabil USING cc_importado
  "                                CHANGING lf_ha_doc_ok.

  "CHECK: lf_ha_doc_ok = cc_x.
* Atualiza tabelas de interfacede de posto de gasolina
  PERFORM lancto_grava_interface_posto.

* Atualiza tela
  IF ti_203_lotes[] IS INITIAL.
*   Retorna para nova consulta
    PERFORM renova_tela_para_pesquisa.
    PERFORM reseta_vlrsalvo_selecao_1200.
  ELSE.
*   Renova Grid
    PERFORM novos_conhecimentos_200.
    DESCRIBE TABLE ti_203_lotes LINES vg_sbs203_tabcontrol-lines.
  ENDIF.

ENDFORM.                    " GERA_LANCTO_CONTABIL_203

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GRAVA_DADOS_CONTABIL
*&---------------------------------------------------------------------*
FORM lancto_grava_dados_contabil.

  DATA: it_zlest0015 TYPE TABLE OF zlest0015 INITIAL SIZE 0 WITH HEADER LINE,
        wa_zlest0015 TYPE zlest0015,
        p_valor      TYPE  netwr_fp,
        lc_branch    TYPE j_1bbranc_,
        lc_bukrs     TYPE bukrs,
        vg_zuonr_d   TYPE dzuonr,
        vg_zuonr_c   TYPE dzuonr.


  REFRESH: ti_obj_contabil,
             ti_idxacrdecr.

  CLEAR: vg_msgerro.

* Analisa se todos status marcado do lote permite lançamento
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.

*   Analisa o status do lote
    IF ti_203_lotes-bl = cc_bloqueado.

      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONDENSE vg_campo_char05.

      CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m08
             INTO vg_msgerro SEPARATED BY space.

    ELSEIF ti_203_lotes-status <> cc_importado.

      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONDENSE vg_campo_char05.

      CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m05
             INTO vg_msgerro SEPARATED BY space.
      REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
               WITH ti_203_lotes-status.
    ELSE.

*     Obtem o valor programado para o Lote
      PERFORM lancto_vlrlote_programado CHANGING vg_vlr_programado.
      IF vg_vlr_programado <= 0.

        WRITE vg_index TO vg_campo_char05 NO-ZERO.
        CONDENSE vg_campo_char05.

        CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m22
               INTO vg_msgerro SEPARATED BY space.
        REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
                 WITH ti_203_lotes-lote.

      ENDIF.

      PERFORM lancto_vlrlote_programado CHANGING p_valor.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ti_203_lotes-codtrp+6
        IMPORTING
          output = lc_branch.

      SELECT SINGLE bukrs INTO lc_bukrs
        FROM j_1bbranch
        WHERE branch = lc_branch.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs          = lc_bukrs
          p_lifnr          = ti_203_lotes-codposto
          p_valor          = p_valor
        EXCEPTIONS
          nao_fornecedor   = 1
          fornecedor_conta = 2
          fornecedor_banco = 3
          faixa_valor      = 4
          banco_empresa    = 5
          OTHERS           = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO vg_msgerro.
      ENDIF.

    ENDIF.

*   Verifica inconsistências
    IF NOT vg_msgerro IS INITIAL.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
      EXIT.
    ENDIF.

*   Verifica necessidade de validar acréscimo/decréscimo
    IF ti_203_lotes-acrdecr = cc_x.
      ti_idxacrdecr-index = vg_index.
      ti_idxacrdecr-valor = vg_vlr_programado.
      APPEND ti_idxacrdecr.
    ENDIF.

  ENDLOOP.

* Tudo Ok?
  CHECK vg_msgerro IS INITIAL.

* Verifica Acréscimo/Decréscimo
  IF NOT ti_idxacrdecr[] IS INITIAL.
    REFRESH: ti_204h_acrdecr,
             ti_204d_acrdecr.
    PERFORM check_acresimo_decrescimo CHANGING vg_resposta.
    IF vg_resposta IS INITIAL.
      vg_msgerro = TEXT-m34.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
    ENDIF.
    CHECK vg_resposta = cc_x.
  ENDIF.

* Verificação de Status de Lote em banco de dados
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.

    SELECT SINGLE * INTO wa_zlest0015
      FROM zlest0015
     WHERE transportador EQ ti_203_lotes-codtrp
       AND posto         EQ ti_203_lotes-codposto
       AND lote          EQ ti_203_lotes-lote
       AND status        EQ cc_a.

    IF sy-subrc IS INITIAL.
      CLEAR: ti_203_lotes-mark.
      CONCATENATE 'Lote' ti_203_lotes-lote 'do posto' ti_203_lotes-codposto 'e transp.' ti_203_lotes-codtrp 'já foi gerado!'
             INTO vg_msgerro SEPARATED BY space.
      MESSAGE vg_msgerro TYPE cc_w.
    ENDIF.

    MODIFY ti_203_lotes INDEX vg_index TRANSPORTING mark.

  ENDLOOP.

* Seleciona registros marcados para lançamento contábil
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.
    CLEAR ti_obj_contabil.
    vg_campo_curr = 0.

*   Verifica acréscimo/decrésimo
    READ TABLE ti_204h_acrdecr WITH KEY codtrp = ti_203_lotes-codtrp
                                      codposto = ti_203_lotes-codposto
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL AND
       ti_204h_acrdecr-acao = cc_1_adacdc AND
       ti_204h_acrdecr-vlrlancto > 0.
      ti_obj_contabil-valor = ti_204h_acrdecr-vlrlancto.
      ti_obj_contabil-acdcusado = cc_x.
      LOOP AT ti_204d_acrdecr WHERE codtrp       = ti_203_lotes-codtrp
                                AND codposto     = ti_203_lotes-codposto
                                AND acdctipo     = cc_1_adacdc
                                AND lote_aplicar = ti_203_lotes-lote.
        CLEAR: vg_wa_lancto.
        vg_wa_lancto-codtrp         = ti_203_lotes-codtrp.
        vg_wa_lancto-codposto       = ti_203_lotes-codposto.
        vg_wa_lancto-lote           = ti_203_lotes-lote.
        vg_wa_lancto-conhec         = ti_204d_acrdecr-conhecimento.
        vg_wa_lancto-ctafrete       = ti_204d_acrdecr-ctafrete.

        CASE ti_204d_acrdecr-chvid.
          WHEN '17'.
            vg_wa_lancto-chvid      = '19'.
          WHEN '18'.
            vg_wa_lancto-chvid      =   '20'.
          WHEN '26'.
            vg_wa_lancto-chvid      = '26'.
          WHEN '27'.
            vg_wa_lancto-chvid      = '27'.
          WHEN '28'.
            vg_wa_lancto-chvid      = '28'.
          WHEN '29'.
            vg_wa_lancto-chvid      = '29'.
        ENDCASE.

*---> 07/06/2023 - Migração S4 - JS
*            VG_WA_LANCTO-VLRCONFIRMADO  = TI_204D_ACRDECR-WRBTR.
*            VG_WA_LANCTO-VLRPROGRAMADO  = TI_204D_ACRDECR-WRBTR.
        ti_204d_acrdecr-wrbtr = CONV #( vg_wa_lancto-vlrconfirmado ).
        ti_204d_acrdecr-wrbtr = CONV #( vg_wa_lancto-vlrprogramado ).
*<--- 07/06/2023 - Migração S4 - JS
        vg_wa_lancto-datalote       = ti_203_lotes-datalote.
        vg_wa_lancto-lctochvid      = cc_m.

        IF ti_204d_acrdecr-chvid EQ '18'.
          MULTIPLY ti_204d_acrdecr-wrbtr BY -1.
        ENDIF.
        " CSB - Para o caso do lancamento 26 e 28 - Conforme chamado 69048
        IF ( ti_204d_acrdecr-chvid EQ '26' ) OR ( ti_204d_acrdecr-chvid EQ '28') .
          MULTIPLY ti_204d_acrdecr-wrbtr BY -1.
        ENDIF.
        " Fim CSB

        ADD ti_204d_acrdecr-wrbtr TO vg_campo_curr.

        PERFORM inclui_lancto_lote USING vg_wa_lancto.
      ENDLOOP.
    ELSE.
      PERFORM lancto_vlrlote_programado CHANGING ti_obj_contabil-valor.
    ENDIF.

    CONCATENATE 'ADTO - ' ti_203_lotes-dscodposto
           INTO vg_campo_sgtxt SEPARATED BY space.

    CONCATENATE 'FR-' ti_203_lotes-lote INTO vg_zuonr_d.
    CONCATENATE 'FR-' ti_203_lotes-lote INTO vg_zuonr_c.

*   Atualiza tabela contábil
    PERFORM lancto_atlz_zib_contabil USING ti_203_lotes-codtrp
                                           ti_203_lotes-codposto
                                           ti_203_lotes-lote
                                           space
                                           ti_203_lotes-vencimento
                                           ti_obj_contabil-valor
                                           ti_203_lotes-codposto
                                           cc_j
                                           cc_bschl_29
                                           ti_203_lotes-codposto
                                           cc_j
                                           cc_bschl_39
                                           vg_zuonr_d
                                           vg_zuonr_c
                                           vg_campo_sgtxt
                                           cc_tdoc_adto
                                           space
                                           vg_wa_lancto-chvid  " CSB
                                  CHANGING ti_obj_contabil-obj_key
                                           ti_obj_contabil-bukrs.

*   Salva documentos para geração lançamento
    ti_obj_contabil-codtrp   = ti_203_lotes-codtrp.
    ti_obj_contabil-codposto = ti_203_lotes-codposto.
    ti_obj_contabil-lote     = ti_203_lotes-lote.
    ti_obj_contabil-chvid    = ti_203_lotes-chvid.
    APPEND ti_obj_contabil.

    IF vg_campo_curr NE 0.
      ti_203_lotes-vlr_acrec_desc = vg_campo_curr.
      ti_203_lotes-vlr_a_pagar    = ti_203_lotes-vlr_a_pagar + vg_campo_curr.
      MODIFY ti_203_lotes INDEX vg_index TRANSPORTING vlr_acrec_desc vlr_a_pagar.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " LANCTO_GRAVA_DADOS_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  LANCTO_VLRLOTE_ADIANTAMENTO
*&---------------------------------------------------------------------*
FORM lancto_vlrlote_adiantamento CHANGING p_vlr_adiant.

  DATA: l_index           TYPE i.
  CLEAR: p_vlr_adiant.

* Cabeçalho- Posto - Lançamentos - Detalhes
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_203_lotes-codtrp
                                    codposto = ti_203_lotes-codposto
                                    lote     = ti_203_lotes-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

  l_index = sy-tabix.
  DO.
    READ TABLE ti_cockpit_deltas INTO vg_wa_deltas
                             WITH KEY codtrp = vg_wa_lancto-codtrp
                                    codposto = vg_wa_lancto-codposto
                                        lote = vg_wa_lancto-lote
                                    ctafrete = vg_wa_lancto-ctafrete
                                      conhec = vg_wa_lancto-conhec
                                       chvid = vg_wa_lancto-chvid
                                    datalote = vg_wa_lancto-datalote
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      p_vlr_adiant = p_vlr_adiant + vg_wa_deltas-vlradiantamento.
    ENDIF.

*   Lê o próximo registro
    ADD 1 TO l_index.
    READ TABLE ti_cockpit_lancto INTO vg_wa_lancto INDEX l_index.
    IF sy-subrc              <> 0                     OR
       ti_203_lotes-codtrp   <> vg_wa_lancto-codtrp   OR
       ti_203_lotes-codposto <> vg_wa_lancto-codposto OR
       ti_203_lotes-lote     <> vg_wa_lancto-lote.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " LANCTO_VLRLOTE_ADIANTAMENTO

*&---------------------------------------------------------------------*
*&      Form  LANCTO_VLRLOTE_PROGRAMADO
*&---------------------------------------------------------------------*
FORM lancto_vlrlote_programado CHANGING p_vlr_prog.

  DATA: l_index           TYPE i.
  CLEAR: p_vlr_prog.

* Cabeçalho- Posto - Lançamentos - Detalhes
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_203_lotes-codtrp
                                    codposto = ti_203_lotes-codposto
                                    lote     = ti_203_lotes-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

  l_index = sy-tabix.
  DO.

    p_vlr_prog = p_vlr_prog + vg_wa_lancto-vlrprogramado.

*   Lê o próximo registro
    ADD 1 TO l_index.
    READ TABLE ti_cockpit_lancto INTO vg_wa_lancto INDEX l_index.
    IF sy-subrc              <> 0                     OR
       ti_203_lotes-codtrp   <> vg_wa_lancto-codtrp   OR
       ti_203_lotes-codposto <> vg_wa_lancto-codposto OR
       ti_203_lotes-lote     <> vg_wa_lancto-lote.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " LANCTO_VLRLOTE_PROGRAMADO

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_ZIB_CONTABIL
*&---------------------------------------------------------------------*
FORM lancto_atlz_zib_contabil  USING p_codtrp
                                     p_codposto
                                     p_lote
                                     p_chave
                                     p_vencimento
                                     p_valor
                                     p_ctdebito
                                     p_razesp_d
                                     p_chvlcto_d
                                     p_ctcredito
                                     p_razesp_c
                                     p_chvlcto_c
                                     p_zuonr_d
                                     p_zuonr_c
                                     p_descricao
                                     p_tdoc
                                     p_data
                                     p_chvid " CSB
                            CHANGING p_obj_key
                                     p_bukrs.

  STATICS: lc_branch TYPE j_1bbranc_,
           lc_bukrs  TYPE bukrs,
           lc_centro TYPE werks_d.

  DATA: l_number           TYPE char10,
        l_data_char        TYPE char10,
        l_monat            TYPE monat,
        l_gjahr            TYPE gjahr,
        l_bankl            TYPE bankl,
        l_werks            TYPE werks,
*BBKO/Vagner Santos - Início da alteração - 19.10.2010
        l_bvtyp            TYPE lfbk-bvtyp,
        vg_valor           TYPE netwr_fp,
        p_forma_pagamento  TYPE dzlsch,
        p_princ_bnc_emp    TYPE hbkid,
        p_banco_fornecedor TYPE lfbk,

*BBKO/Vagner Santos - Fim da alteração - 19.10.2010
        l_exige_ccusto.

* Controle sequencial para retorno de numeração
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZFORM01'
    IMPORTING
      number      = l_number.

  CLEAR: zib_contabil.

  CONCATENATE 'LES' l_number sy-datum(4) INTO zib_contabil-obj_key.
  p_obj_key = zib_contabil-obj_key.

  zib_contabil-seqitem = 1.
  zib_contabil-bschl   = p_chvlcto_d.
* Caso seja uma conta razao e tiver conhecimento entao contabiliza no centro tomador do servico..
  IF ti_vlrctlglcto-tipoconta_d EQ 'RZ' AND ti_400_confer-conhec IS NOT INITIAL.
    CLEAR l_werks.
* Seleciona o centro da remessa como centro tomador do serviço
    SELECT t3~werks
        INTO l_werks
        FROM vttk AS t1
       INNER JOIN vttp AS t2
          ON t2~tknum = t1~tknum
       INNER JOIN lips AS t3
          ON t3~vbeln   = t2~vbeln
       WHERE t1~tdlnr = ti_400_confer-codtrp
         AND t1~exti1 = ti_400_confer-conhec
         AND t1~exti2 = ti_400_confer-ctafrete.
    ENDSELECT.

    IF l_werks IS NOT INITIAL.

      lc_centro = l_werks.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = lc_centro
        IMPORTING
          centro_out           = lc_centro
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.

      l_werks = lc_centro.

      zib_contabil-gsber   = l_werks.
      zib_contabil-bupla   = l_werks.
    ELSE.
      zib_contabil-gsber   = p_codtrp+6.
      zib_contabil-bupla   = p_codtrp+6.
    ENDIF.
  ELSE.
    zib_contabil-gsber   = p_codtrp+6.
    zib_contabil-bupla   = p_codtrp+6.
  ENDIF.

  IF lc_branch <> zib_contabil-gsber.
    lc_branch = zib_contabil-gsber.
    CLEAR lc_bukrs.
    SELECT bukrs
      INTO lc_bukrs
      FROM j_1bbranch
        UP TO 1 ROWS
      WHERE branch = lc_branch.
    ENDSELECT.
  ENDIF.

  zib_contabil-bukrs = lc_bukrs.
  p_bukrs = zib_contabil-bukrs.
  IF p_data IS INITIAL OR p_data EQ space.
    CONCATENATE sy-datum+6(2) '. ' sy-datum+4(2) '.' sy-datum(4)
         INTO l_data_char.
    l_gjahr = sy-datum(4).
    l_monat = sy-datum+4(2).
  ELSE.
    CONCATENATE p_data+6(2) '. ' p_data+4(2) '.' p_data(4)
         INTO l_data_char.
    l_gjahr = p_data(4).
    l_monat = p_data+4(2).
  ENDIF.

  zib_contabil-interface = '03'.
  zib_contabil-bktxt     = 'POSTOS'.
  zib_contabil-bldat     = l_data_char.
  zib_contabil-budat     = l_data_char.
  zib_contabil-gjahr     = l_gjahr.
  zib_contabil-monat     = l_monat.
  zib_contabil-blart     = p_tdoc.
  zib_contabil-xblnr     = p_lote.
  zib_contabil-hkont     = p_ctdebito.
  zib_contabil-wrbtr     = abs( p_valor ).
  zib_contabil-waers     = 'BRL'.

  CONCATENATE p_vencimento+6(2) '. '
              p_vencimento+4(2) '.'
              p_vencimento(4)
         INTO zib_contabil-zfbdt.

* Apenas possui dados bancarios para documento de adtos..
  " Inicio CSB
  IF p_tdoc = 'SF'.
    IF ( p_chvlcto_c EQ cc_bschl_39 ) AND ( p_chvid = '26' ).
      zib_contabil-zuonr   = p_zuonr_c.
    ENDIF.

    IF zib_contabil-bschl = '29' AND p_chvid = '26'.
      zib_contabil-zuonr  = ''.
    ENDIF.
  ENDIF.
  " Fim CSB


  IF p_tdoc = 'ME'.

    vg_valor = abs( p_valor ).

    CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
      EXPORTING
        p_bukrs            = lc_bukrs
        p_lifnr            = p_codposto
        p_valor            = vg_valor
      IMPORTING
        p_forma_pagamento  = p_forma_pagamento
        p_princ_bnc_emp    = p_princ_bnc_emp
        p_banco_fornecedor = p_banco_fornecedor
      EXCEPTIONS
        nao_fornecedor     = 1
        fornecedor_conta   = 2
        fornecedor_banco   = 3
        faixa_valor        = 4
        banco_empresa      = 5
        OTHERS             = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      zib_contabil-zlsch = p_forma_pagamento.
      zib_contabil-hbkid = p_princ_bnc_emp.
      zib_contabil-bvtyp = p_banco_fornecedor-bvtyp.
    ENDIF.

  ENDIF.

  zib_contabil-sgtxt     = p_descricao.
  zib_contabil-xref3     = '2.08.03'.


  zib_contabil-zuonr     = p_zuonr_d.
  zib_contabil-umskz     = p_razesp_d.
  zib_contabil-waers_i   = 'BRL'.
  zib_contabil-dmbtr     = abs( p_valor ).
*BBKO/Vagner Santos - Fim da alteração - 19.10.2010

  zib_contabil-rg_atualizado = cc_n.

* Verifica conta débito há necessidade de centro de custo
  PERFORM check_contarazao_exige_ccusto USING zib_contabil-hkont
                                              cc_d
                                              zib_contabil-bschl
                                     CHANGING l_exige_ccusto.
  IF l_exige_ccusto = cc_x.
    PERFORM obtem_centro_custo_contarazao USING p_codtrp
                                                cc_d
                                                zib_contabil-hkont
                                       CHANGING zib_contabil-kostl.
  ENDIF.
  MODIFY zib_contabil.

  zib_contabil-seqitem = 2.
  zib_contabil-bschl   = p_chvlcto_c.
  zib_contabil-zuonr   = p_zuonr_c.

  IF ( p_chvlcto_c EQ cc_bschl_39 ) AND ( p_tdoc = 'ME' ) .
    CLEAR zib_contabil-zuonr.
  ENDIF.
  zib_contabil-umskz   = p_razesp_c.
  zib_contabil-hkont   = p_ctcredito.
* Caso seja uma conta razao e tiver conhecimento entao contabiliza no centro tomador do servico..
  IF ti_vlrctlglcto-tipoconta_c EQ 'RZ' AND ti_400_confer-conhec IS NOT INITIAL.
    CLEAR l_werks.
* Seleciona o centro da remessa como centro tomador do serviço
    SELECT t3~werks
        INTO l_werks
        FROM vttk AS t1
       INNER JOIN vttp AS t2
          ON t2~tknum = t1~tknum
       INNER JOIN lips AS t3
          ON t3~vbeln   = t2~vbeln
       WHERE t1~tdlnr = ti_400_confer-codtrp
         AND t1~exti1 = ti_400_confer-conhec
         AND t1~exti2 = ti_400_confer-ctafrete.
    ENDSELECT.
    IF l_werks IS NOT INITIAL.

      lc_centro = l_werks.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = lc_centro
        IMPORTING
          centro_out           = lc_centro
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.

      l_werks = lc_centro.

      zib_contabil-gsber   = l_werks.
      zib_contabil-bupla   = l_werks.
    ELSE.
      zib_contabil-gsber   = p_codtrp+6.
      zib_contabil-bupla   = p_codtrp+6.
    ENDIF.
  ELSE.
    zib_contabil-gsber   = p_codtrp+6.
    zib_contabil-bupla   = p_codtrp+6.

  ENDIF.

* Verifica conta débito há necessidade de centro de custo
  PERFORM check_contarazao_exige_ccusto USING zib_contabil-hkont
                                              cc_c
                                              zib_contabil-bschl
                                     CHANGING l_exige_ccusto.
  IF l_exige_ccusto = cc_x.
    PERFORM obtem_centro_custo_contarazao USING p_codtrp
                                                cc_c
                                                zib_contabil-hkont
                                       CHANGING zib_contabil-kostl.
  ENDIF.
  MODIFY zib_contabil.

ENDFORM.                    " LANCTO_ATLZ_ZIB_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GERA_DOCTO_CONTABIL
*&---------------------------------------------------------------------*
FORM lancto_gera_docto_contabil USING p_status
                             CHANGING p_ha_dcto_contabil.

  DATA: p_ha_dcto_log.

  CLEAR: p_ha_dcto_contabil,
         zib_contabil.

* Gera documento contábil - Chama o programa em execução on-line
  "SUBMIT zfi_xi_document AND RETURN.

* Obtem resultado documento gerado
  LOOP AT ti_obj_contabil.

    vg_index = sy-tabix.

    IF vg_index = 1.
*     Se o processamento foi executado em backgroud
*     Aguarda a primeira atualização da geração do documento contábil
      DO 5 TIMES.

        SELECT *
          FROM zib_contabil
            UP TO 1 ROWS
         WHERE obj_key = ti_obj_contabil-obj_key
           AND rg_atualizado = cc_s.
        ENDSELECT.

        IF sy-subrc IS INITIAL.
          EXIT.
        ENDIF.
        WAIT UP TO 2 SECONDS.

      ENDDO.
    ENDIF.

*    IF zib_contabil-rg_atualizado IS INITIAL OR
*       zib_contabil-rg_atualizado = cc_n.
**     Se não foi possível recuperar do documento neste momento
**     Schedula um JOB para atualização em background
*      PERFORM lancto_atlz_background.
*      MESSAGE s000 WITH text-m17 DISPLAY LIKE cc_w.
*      EXIT.
*    ENDIF.

*   Lê o documento contábil gerado
    CLEAR zib_contabil_chv.

    SELECT SINGLE *
      FROM zib_contabil_chv
     WHERE obj_key = ti_obj_contabil-obj_key.

*   Analisa LOG de Erro na geração do documento contábil
    IF zib_contabil_chv-belnr IS INITIAL.

*     Gera Log cockpit
      PERFORM lancto_gera_loglacto_cockpit.

*     Elimina documento contábil
      "DELETE FROM zib_contabil
      "      WHERE obj_key = ti_obj_contabil-obj_key.

*     Atualiza tabela de Lote para refletir o LOG
      READ TABLE ti_203_lotes
                 WITH KEY codtrp   = ti_obj_contabil-codtrp
                          codposto = ti_obj_contabil-codposto
                          lote     = ti_obj_contabil-lote
                          status   = p_status
                          bl       = cc_liberado.
      IF sy-subrc IS INITIAL.
        ti_203_lotes-icon_log = icon_message_warning_small.
        MODIFY ti_203_lotes INDEX sy-tabix TRANSPORTING icon_log.
        p_ha_dcto_log = cc_x.
      ENDIF.

      CONTINUE.
    ENDIF.

    p_ha_dcto_contabil = cc_x.

*   Atualiza tabela interna com os documentos gerados
    ti_obj_contabil-docsap = zib_contabil_chv-belnr.
    ti_obj_contabil-gjahr  = zib_contabil_chv-gjahr.

    MODIFY ti_obj_contabil INDEX vg_index TRANSPORTING docsap
                                                       gjahr.
  ENDLOOP.

  IF p_ha_dcto_log = cc_x.
    vg_msgerro = TEXT-m21.
    MESSAGE TEXT-m21 TYPE cc_s DISPLAY LIKE cc_w.
    CLEAR vg_msgerro.
  ENDIF.

ENDFORM.                    " LANCTO_GERA_DOCTO_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  LANCTO_GRAVA_INTERFACE_POSTO
*&---------------------------------------------------------------------*
FORM lancto_grava_interface_posto .

* Classifica tabela de processamento contábil
  SORT ti_obj_contabil BY codtrp codposto lote.

* Seleciona registros para lançamento contábil
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.

    READ TABLE ti_obj_contabil
      WITH KEY codtrp   = ti_203_lotes-codtrp
               codposto = ti_203_lotes-codposto
               lote     = ti_203_lotes-lote
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      "CHECK: NOT ti_obj_contabil-docsap IS INITIAL.

*   Atualiza dados importados do Posto de Combustivel
      PERFORM lancto_atlz_zlest00013 USING vg_index
                                  CHANGING vg_msgerro.

*   Atualiza Posto - Lançamentos - Detalhes
      PERFORM lancto_atlz_zlest0016.

*   Atualiza cabeçalho- Posto - Lotes
      PERFORM lancto_atlz_zlest0015 USING ti_obj_contabil-docsap
                                          ti_obj_contabil-gjahr
                                          ti_obj_contabil-bukrs
                                          ti_obj_contabil-acdcusado
                                          ti_obj_contabil-obj_key.
*   Elimina registro do Grid
      DELETE ti_203_lotes INDEX vg_index.

      CALL FUNCTION 'Z_LES_EMAIL_COCKPIT'
        EXPORTING
          cod_transp     = ti_203_lotes-codtrp
          cod_posto      = ti_203_lotes-codposto
          cod_lote       = ti_203_lotes-lote
        EXCEPTIONS
          nao_localizada = 1
          nao_email      = 2
          OTHERS         = 3.

    ENDIF.

  ENDLOOP.

* Atualiza valores usado em Acréscimo/Decrésimo.
  PERFORM atlz_acdc_confer_zlest0022.

ENDFORM.                    " LANCTO_GRAVA_INTERFACE_POSTO

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_ZLEST00013
*&---------------------------------------------------------------------*
FORM lancto_atlz_zlest00013  USING p_index
                          CHANGING p_msgerro.

  CLEAR p_msgerro.

  IF NOT ti_203_lotes-cnpj_trp   IS INITIAL AND
     NOT ti_203_lotes-cnpj_posto IS INITIAL.

*   Atualiza pela chave primária
    UPDATE zlest0013
       SET status  = cc_a_confirmar
           data    = sy-datum
           hora    = sy-uzeit
           usuario = sy-uname
     WHERE cnpj_trp   = ti_203_lotes-cnpj_trp
       AND cnpj_posto = ti_203_lotes-cnpj_posto
       AND lote       = ti_203_lotes-lote
       AND status     = stela_1200-status
       AND conhec    IN stela_1200-range_conhec
*      AND datalote  IN stela_1200-range_periodo
       AND data      IN stela_1200-range_periodo.

  ELSE.

*   Atualiza pela informações principais do lote
    UPDATE zlest0013
       SET status  = cc_confirmado
           data    = sy-datum
           hora    = sy-uzeit
           usuario = sy-uname
     WHERE codtrp     = ti_203_lotes-codtrp
       AND codposto   = ti_203_lotes-codposto
       AND lote       = ti_203_lotes-lote
       AND status     = stela_1200-status
       AND conhec    IN stela_1200-range_conhec
*      AND datalote  IN stela_1200-range_periodo
       AND data      IN stela_1200-range_periodo.

  ENDIF.

* Verifica inconsistências
  IF sy-subrc <> 0.

    WRITE p_index TO vg_campo_char05 NO-ZERO.
    CONDENSE vg_campo_char05.

    CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m07
           INTO p_msgerro SEPARATED BY space.
  ENDIF.

ENDFORM.                    " LANCTO_ATLZ_ZLEST00013

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_ZLEST0016
*&---------------------------------------------------------------------*
FORM lancto_atlz_zlest0016.

  DATA: l_index          TYPE i,
        l_vlr_recusado   TYPE kwert,
        l_vlr_confirmado TYPE kwert,
        l_vlr_importado  TYPE kwert.

* Posto - Lançamentos - Detalhes
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_203_lotes-codtrp
                                    codposto = ti_203_lotes-codposto
                                    lote     = ti_203_lotes-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

  l_index = sy-tabix.
  DO.
    zlest0016-transportador    = vg_wa_lancto-codtrp.
    zlest0016-posto            = vg_wa_lancto-codposto.
    zlest0016-lote             = vg_wa_lancto-lote.
    zlest0016-chvid            = vg_wa_lancto-chvid.
    zlest0016-ctafrete         = vg_wa_lancto-ctafrete.
    zlest0016-conhecimento     = vg_wa_lancto-conhec.
    zlest0016-peso_origem      = vg_wa_lancto-peso_origem.
    zlest0016-peso_importado   = vg_wa_lancto-peso_importado.
    zlest0016-peso_confirmado  = vg_wa_lancto-peso_confirmado.
    zlest0016-unidade_peso     = vg_wa_lancto-unid_peso.
    zlest0016-vlr_origem       = vg_wa_lancto-vlrorigem.
    zlest0016-vlr_importado    = vg_wa_lancto-vlrimportado.
    zlest0016-vlr_confirmado   = vg_wa_lancto-vlrconfirmado.
    zlest0016-peso_confiralter = 0.
    zlest0016-vlr_confiralter  = 0.
    zlest0016-diferenca        = vg_wa_lancto-vlrdiferenca.
    zlest0016-vlr_programado   = vg_wa_lancto-vlrprogramado.
    zlest0016-dta_chegada      = vg_wa_lancto-dtacheg.
    zlest0016-observacoes      = vg_wa_lancto-observacoes.
    zlest0016-erdat            = sy-datum.
    zlest0016-uzeit            = sy-uzeit.
    zlest0016-uname            = sy-uname.

    MODIFY zlest0016 FROM zlest0016.

    COMMIT WORK. " CSB

    CLEAR zlest0016.

    READ TABLE ti_cockpit_deltas INTO vg_wa_deltas
                             WITH KEY codtrp = vg_wa_lancto-codtrp
                                    codposto = vg_wa_lancto-codposto
                                        lote = vg_wa_lancto-lote
                                    ctafrete = vg_wa_lancto-ctafrete
                                      conhec = vg_wa_lancto-conhec
                                       chvid = vg_wa_lancto-chvid
                                    datalote = vg_wa_lancto-datalote
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.
*     Dados de calculo - Deltas
      MOVE-CORRESPONDING vg_wa_deltas TO zlest0020.
      MOVE: vg_wa_lancto-codtrp       TO zlest0020-transportador,
            vg_wa_lancto-codposto     TO zlest0020-posto,
            vg_wa_lancto-conhec       TO zlest0020-conhecimento,
            sy-datum                  TO zlest0020-erdat,
            sy-uname                  TO zlest0020-uname.

      MODIFY zlest0020 FROM zlest0020.
      COMMIT WORK. " CSB

      CLEAR zlest0020.

    ENDIF.

*   Sumariza valores para cálculo de valor realizado
    l_vlr_confirmado = l_vlr_confirmado
                       + vg_wa_lancto-vlrconfirmado.
    l_vlr_importado  = l_vlr_importado
                       + vg_wa_lancto-vlrimportado.

*   Obtem o total do valor recusado
    l_vlr_recusado   =  vg_wa_lancto-vlrconfirmado
                      - vg_wa_lancto-vlrimportado.

*   Atualiza subtotais - 1
    SUBTRACT vg_wa_lancto-vlrimportado
        FROM stotais_lote_203-tot_vimportado.
    SUBTRACT vg_wa_lancto-vlrconfirmado
        FROM stotais_lote_203-tot_vconfirmado.
    SUBTRACT l_vlr_recusado
        FROM stotais_lote_203-tot_vrecusado.

*   Lê o próximo registro
    ADD 1 TO l_index.
    READ TABLE ti_cockpit_lancto INTO vg_wa_lancto INDEX l_index.
    IF sy-subrc              <> 0                     OR
       ti_203_lotes-codtrp   <> vg_wa_lancto-codtrp   OR
       ti_203_lotes-codposto <> vg_wa_lancto-codposto OR
       ti_203_lotes-lote     <> vg_wa_lancto-lote.
      EXIT.
    ENDIF.
  ENDDO.

* Atualiza subtotais - 2 com o menor valor entre...
  IF l_vlr_confirmado <= l_vlr_importado.
    SUBTRACT l_vlr_confirmado  FROM stotais_lote_203-tot_vrealizado.
  ELSE.
    SUBTRACT l_vlr_importado   FROM stotais_lote_203-tot_vrealizado.
  ENDIF.

ENDFORM.                    " LANCTO_ATLZ_ZLEST0016

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_ZLEST0015
*&---------------------------------------------------------------------*
FORM lancto_atlz_zlest0015 USING  p_docsap
                                  p_gjahr
                                  p_bukrs
                                  p_acdcusado
                                  p_obj_key.
* Cabeçalho- Posto - Lotes
  zlest0015-transportador     = ti_203_lotes-codtrp.
  zlest0015-posto             = ti_203_lotes-codposto.
  zlest0015-lote              = ti_203_lotes-lote.
  zlest0015-bl                = ti_203_lotes-bl.
  zlest0015-data              = ti_203_lotes-datalote.
  zlest0015-vencimento        = ti_203_lotes-vencimento.
  zlest0015-vlr_origem        = ti_203_lotes-vlrorigem.
  zlest0015-vlr_importado     = ti_203_lotes-vlrimportado.
  zlest0015-vlr_confirmado    = ti_203_lotes-vlrconfirmado.
  zlest0015-vlr_recusado      = ti_203_lotes-vlrrecusado.
  zlest0015-vlr_realizado     = ti_203_lotes-vlrrealizado.
  zlest0015-diferenca         = ti_203_lotes-vlrdiferenca.

  zlest0015-vlr_acrec_desc    = ti_203_lotes-vlr_acrec_desc.
  zlest0015-vlr_a_pagar       = ti_203_lotes-vlr_a_pagar.

  zlest0015-status            = cc_a_confirmar.
  zlest0015-acdcusado         = p_acdcusado.
  zlest0015-erdat             = sy-datum.
  zlest0015-uzeit             = sy-uzeit.
  zlest0015-uname             = sy-uname.

  zlest0015-docsap            = p_docsap.
  zlest0015-gjahr             = p_gjahr.
  zlest0015-bukrs             = p_bukrs.
  zlest0015-obj_key           = p_obj_key.

  MODIFY zlest0015 FROM zlest0015.

  COMMIT WORK. " CSB
  WAIT UP TO 7 SECONDS. "CSB


ENDFORM.                    " LANCTO_ATLZ_ZLEST0015

*&---------------------------------------------------------------------*
*&      Form  YF_CALC_DATE
*&---------------------------------------------------------------------*
FORM yf_calc_date     USING p_data_inicio
                            p_dia
                            p_mes
                            p_ano
                            p_sinal
                   CHANGING p_data_calculada.

  DATA: ln_dia   TYPE dlydy,
        ln_mes   TYPE dlymo,
        ln_ano   TYPE dlyyr,
        lc_sinal TYPE spli1.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = p_data_inicio
      days      = ln_dia
      months    = ln_mes
      signum    = lc_sinal
      years     = ln_ano
    IMPORTING
      calc_date = p_data_calculada.

ENDFORM.                    " YF_CALC_DATE

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_BACKGROUND
*&---------------------------------------------------------------------*
FORM lancto_atlz_background .

  CHECK: NOT ti_obj_contabil[] IS INITIAL.

  DATA: jobname      LIKE tbtcjob-jobname VALUE 'SAPMZLES001-ZIB_CONTABIL',
        jobcount     LIKE tbtcjob-jobcount,
        l_indxkey    LIKE indx-srtfd      VALUE 'ZLESJOB',
        l_waindx     TYPE indx,
        l_tstamp     LIKE tzonref-tstamps,
        l_auxvlr(14).

  DATA: l_horajob TYPE t,
        l_addtemp TYPE t     VALUE '001000',  "10 minutos exec. Job
        l_diajob  TYPE d.

* Gera chave única de gravação no data-base p/parâmetros de importação
  CONVERT DATE sy-datlo TIME sy-timlo
     INTO TIME STAMP l_tstamp TIME ZONE sy-zonlo.

  WRITE l_tstamp TO l_auxvlr NO-ZERO NO-GROUPING.
  CONCATENATE l_indxkey l_auxvlr INTO l_indxkey.

* Determina hora de execução do Job
  CALL FUNCTION 'DIMP_ADD_TIME'
    EXPORTING
      iv_starttime = sy-uzeit
      iv_startdate = sy-datum
      iv_addtime   = l_addtemp
    IMPORTING
      ev_endtime   = l_horajob
      ev_enddate   = l_diajob.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = jobname
      sdlstrtdt        = l_diajob
      sdlstrttm        = l_horajob
    IMPORTING
      jobcount         = jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  CHECK sy-subrc IS INITIAL.

  l_waindx-aedat = sy-datum.
  l_waindx-usera = sy-uname.

  EXPORT ti_obj_contabil FROM ti_obj_contabil
      TO DATABASE indx(st)
    FROM l_waindx ID l_indxkey.

  SUBMIT zlesr0009 AND RETURN
    WITH p_keyid = l_indxkey
    USER sy-uname
     VIA JOB jobname
  NUMBER jobcount.

  CHECK sy-subrc IS INITIAL.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = jobcount
      jobname              = jobname
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      OTHERS               = 9.

ENDFORM.                    " LANCTO_ATLZ_BACKGROUND


*&---------------------------------------------------------------------*
*&      Form  EXIBE_DETALHE_DOCTO_401
*&---------------------------------------------------------------------*
FORM exibe_detalhe_docto_401 .

  DATA: it_zlest0022     TYPE TABLE OF zlest0022 INITIAL SIZE 0 WITH HEADER LINE,
        wa_zlest0022     TYPE zlest0022,
        wa_zlest0016     TYPE zlest0016,
        vg_chvid         TYPE zchvid,
        xvlrconfirmado   TYPE c LENGTH 16,
        observacoes(100).

  SORT ti_cockpit_confer BY codtrp codposto lote conhecimento chvid ASCENDING obj_key DESCENDING.

* Lê o primeiro documento marcado para detalhamento
  READ TABLE ti_400_confer WITH KEY mark = cc_x.
  CHECK: sy-subrc IS INITIAL.

  vg_index = sy-tabix.

* Garante que o registro do Header é o mesmo da conferência
  CHECK: ti_400_confer-codtrp   = ti_203_lotes-codtrp
    AND  ti_400_confer-codposto = ti_203_lotes-codposto
    AND  ti_400_confer-lote     = ti_203_lotes-lote.

* Limpa área de detalhamento
  CLEAR: sdetlh_confer_400,
         vg_wa_lancto,
         vg_wa_deltas.

  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

* Alimenta dados do novo detalhe
  IF ti_400_confer-acrescentado = space.

    READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                             WITH KEY codtrp   = ti_400_confer-codtrp
                                      codposto = ti_400_confer-codposto
                                      lote     = ti_400_confer-lote
                                    ctafrete   = ti_400_confer-ctafrete
                                      conhec   = ti_400_confer-conhec
                                      chvid    = ti_400_confer-chvid
                                      datalote = ti_203_lotes-datalote
                             BINARY SEARCH.

    READ TABLE ti_cockpit_deltas INTO vg_wa_deltas
                             WITH KEY codtrp = vg_wa_lancto-codtrp
                                    codposto = vg_wa_lancto-codposto
                                        lote = vg_wa_lancto-lote
                                    ctafrete = vg_wa_lancto-ctafrete
                                      conhec = vg_wa_lancto-conhec
                                       chvid = vg_wa_lancto-chvid
                                    datalote = vg_wa_lancto-datalote
                             BINARY SEARCH.

*   Atribui detalhamentos para o documento - Quadro de controle
    IF vg_wa_lancto-peso_confiralter IS INITIAL.
      sdetlh_confer_400-peso_conferido = vg_wa_lancto-peso_confirmado.
    ELSE.
      sdetlh_confer_400-peso_conferido = vg_wa_lancto-peso_confiralter.
    ENDIF.
    MOVE:
    vg_wa_lancto-peso_origem      TO sdetlh_confer_400-peso_origem,
    vg_wa_deltas-diferenca_peso   TO sdetlh_confer_400-peso_diferenca,
    vg_wa_deltas-quebra_peso      TO sdetlh_confer_400-peso_perda,
    vg_wa_deltas-quebra_real      TO sdetlh_confer_400-peso_qbr_tolerav,
    vg_wa_lancto-vlrorigem        TO sdetlh_confer_400-vlr_origem,
    vg_wa_lancto-vlrprogramado    TO sdetlh_confer_400-vlr_progr.
    IF sdetlh_confer_400-peso_conferido >= sdetlh_confer_400-peso_origem.
      CLEAR sdetlh_confer_400-peso_perda.
    ENDIF.

*   Atribui detalhamentos para o documento - Quadro de Impostos Retidos
    IF vg_wa_lancto-vlrconfiralter IS INITIAL.
      sdetlh_confer_400-vlr_conferido = vg_wa_lancto-vlrconfirmado.
    ELSE.
      sdetlh_confer_400-vlr_conferido = vg_wa_lancto-vlrconfiralter.
    ENDIF.
    MOVE:
    vg_wa_deltas-vlrimp_retidos   TO sdetlh_confer_400-vlr_impretido,
    vg_wa_deltas-vlrseguro        TO sdetlh_confer_400-seguro,
    vg_wa_deltas-vlrperda         TO sdetlh_confer_400-vlr_perda,
    vg_wa_lancto-dtacheg          TO sdetlh_confer_400-dtacheg,
*    vg_wa_deltas-vlrsobra_quebra  TO sdetlh_confer_400-vlr_sobra_quebra.
    vg_wa_deltas-difer_transp     TO sdetlh_confer_400-vlr_sobra_quebra.

*   Atribui controle de lançamento
    MOVE: vg_wa_lancto-lctochvid   TO sdetlh_confer_400-lctochvid,
          vg_wa_lancto-chvid       TO sdetlh_confer_400-chvid.

*   Atribui acréscimo e/ou decréscimo
    IF vg_wa_lancto-ctlgchavid = cc_chvid_adiant.
      sdetlh_confer_400-vlr_acre_decre = ti_400_confer-valor_acdc_ad.
    ENDIF.

  ELSE.
    sdetlh_confer_400-vlr_conferido = ti_400_confer-valor_acresc.
    sdetlh_confer_400-dtacheg = ti_400_confer-data_acresc.
  ENDIF.

* Exibe Documentos SAP
  IF ti_400_confer-status = cc_confirmado.
    READ TABLE ti_cockpit_confer INTO vg_wa_confer
                             WITH KEY codtrp       = ti_400_confer-codtrp
                                      codposto     = ti_400_confer-codposto
                                      lote         = ti_400_confer-lote
                                      conhecimento = ti_400_confer-conhec
                                      chvid        = ti_400_confer-chvid
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      vg_index = sy-tabix.
      DO 4 TIMES.
        CASE vg_wa_confer-ctlglancto.
          WHEN 'VC'.
            sdetlh_confer_400-docsap1 = vg_wa_confer-docsap.
            sdetlh_confer_400-doc1msg = vg_wa_confer-err_msg.
          WHEN 'P'.
            sdetlh_confer_400-docsap3 = vg_wa_confer-docsap.
            sdetlh_confer_400-doc3msg = vg_wa_confer-err_msg.
          WHEN 'S' OR 'Q'.
            sdetlh_confer_400-docsap2 = vg_wa_confer-docsap.
            sdetlh_confer_400-doc2msg = vg_wa_confer-err_msg.
          WHEN OTHERS.
            sdetlh_confer_400-docsap4 = vg_wa_confer-docsap.
            sdetlh_confer_400-doc4msg = vg_wa_confer-err_msg.
        ENDCASE.
*        CASE sy-index.
*          WHEN 1.
*            sdetlh_confer_400-docsap1 = vg_wa_confer-docsap.
*            sdetlh_confer_400-doc1msg = vg_wa_confer-err_msg.
*          WHEN 2.
*            sdetlh_confer_400-docsap2 = vg_wa_confer-docsap.
*            sdetlh_confer_400-doc2msg = vg_wa_confer-err_msg.
*          WHEN 3.
*            sdetlh_confer_400-docsap3 = vg_wa_confer-docsap.
*            sdetlh_confer_400-doc3msg = vg_wa_confer-err_msg.
*          WHEN 4.
*            sdetlh_confer_400-docsap4 = vg_wa_confer-docsap.
*            sdetlh_confer_400-doc4msg = vg_wa_confer-err_msg.
*        ENDCASE.
        ADD 1 TO vg_index.
        READ TABLE ti_cockpit_confer INTO vg_wa_confer INDEX vg_index.
        IF sy-subrc <> 0 OR
           vg_wa_confer-codtrp       <> ti_400_confer-codtrp   OR
           vg_wa_confer-codposto     <> ti_400_confer-codposto OR
           vg_wa_confer-lote         <> ti_400_confer-lote     OR
           vg_wa_confer-chvid        <> ti_400_confer-chvid    OR
           vg_wa_confer-conhecimento <> ti_400_confer-conhec.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.

* Identifica o documento detalhado
* Não alterar o início do texto contendo o número da linha detalhada
  WRITE vg_index TO vg_campo_char05 NO-ZERO.
  CONDENSE vg_campo_char05.
  CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m12
         INTO sdetlh_confer_400-docdetalhe SEPARATED BY space.
  REPLACE FIRST OCCURRENCE OF '&1' IN sdetlh_confer_400-docdetalhe
         WITH ti_400_confer-chvid.
  REPLACE FIRST OCCURRENCE OF '&2' IN sdetlh_confer_400-docdetalhe
         WITH ti_400_confer-deschvid.
  REPLACE FIRST OCCURRENCE OF '&3' IN sdetlh_confer_400-docdetalhe
         WITH ti_400_confer-ctafrete.
  REPLACE FIRST OCCURRENCE OF '&4' IN sdetlh_confer_400-docdetalhe
         WITH ti_400_confer-conhec.

  IF vg_obs_edit_400 IS BOUND.

    REFRESH ti_400_obs.

    IF NOT ti_400_confer-observacoes IS INITIAL.
      APPEND ti_400_confer-observacoes(85)     TO ti_400_obs.
      APPEND ti_400_confer-observacoes+85(85)  TO ti_400_obs.
      APPEND ti_400_confer-observacoes+170     TO ti_400_obs.
    ENDIF.

    IF ti_400_confer-chvid EQ '19' OR ti_400_confer-chvid EQ '20'.

      CASE ti_400_confer-chvid.
        WHEN '19'.
          vg_chvid = '17'.
        WHEN '20'.
          vg_chvid = '18'.
      ENDCASE.

      SELECT * INTO TABLE it_zlest0022
        FROM zlest0022
       WHERE transportador EQ ti_400_confer-codtrp
         AND posto         EQ ti_400_confer-codposto
         AND lote_aplicado EQ ti_400_confer-lote
         AND conhecimento  EQ ti_400_confer-conhec
         AND chvid         EQ vg_chvid.

      LOOP AT it_zlest0022 INTO wa_zlest0022.

        SELECT SINGLE * INTO wa_zlest0016
          FROM zlest0016
         WHERE transportador EQ wa_zlest0022-transportador
           AND posto         EQ wa_zlest0022-posto
           AND lote          EQ wa_zlest0022-lote
           AND conhecimento  EQ wa_zlest0022-conhecimento
           AND chvid         EQ vg_chvid.

        IF sy-subrc IS INITIAL.
          MOVE wa_zlest0016-vlr_confirmado TO xvlrconfirmado.
          SHIFT:  xvlrconfirmado LEFT DELETING LEADING space.
          CONCATENATE 'Lote contabilizado' wa_zlest0022-lote 'Valor compensado:' xvlrconfirmado INTO observacoes SEPARATED BY space.
          APPEND observacoes TO ti_400_obs.
        ELSE.
          CONCATENATE 'Lote contabilizado' wa_zlest0022-lote INTO ti_400_confer-observacoes SEPARATED BY space.
          APPEND observacoes TO ti_400_obs.
        ENDIF.
      ENDLOOP.

    ENDIF.

    CALL METHOD vg_obs_edit_400->set_text_as_r3table
      EXPORTING
        table = ti_400_obs.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.

  ENDIF.

* Obtem Valor da Nota Fiscal
  CHECK: ti_400_confer-acrescentado IS INITIAL
    AND  NOT ti_400_confer-conhec   IS INITIAL
    AND  NOT ti_400_confer-ctafrete IS INITIAL.

  PERFORM obtem_valor_nf_401 USING ti_400_confer-codtrp
                                   ti_400_confer-conhec
                                   ti_400_confer-ctafrete
                          CHANGING sdetlh_confer_400-vlr_nf.

ENDFORM.                    " EXIBE_DETALHE_DOCTO_401

*&---------------------------------------------------------------------*
*&      Form  NOVAS_ENTRADAS_DOCTO_401
*&---------------------------------------------------------------------*
FORM novas_entradas_docto_401 .

* Verifica se há dados no header da tabela de conferência
*  IF ti_400_confer-codtrp   IS INITIAL OR
*     ti_400_confer-codposto IS INITIAL OR
*     ti_400_confer-lote     IS INITIAL.

*   Extrai um Header Line para exibir dados no lançamento
  READ TABLE ti_400_confer WITH KEY mark = cc_x
                          acrescentado = space.
*  ENDIF.

  IF sctrl_saldo_400-saldo_atual IS INITIAL.
    MESSAGE s000 WITH TEXT-m18 DISPLAY LIKE cc_w.
  ELSE.
    CLEAR: snlancto_confer_400.
*   Exibe tela de entradas manuais
    CALL SCREEN 0402
         STARTING AT 07 05
           ENDING AT 104 21.
  ENDIF.

ENDFORM.                    " NOVAS_ENTRADAS_DOCTO_401

*&---------------------------------------------------------------------*
*&      Form  EXIBE_MATCH_CODE_CHVID
*&---------------------------------------------------------------------*
FORM exibe_match_code_chvid USING VALUE(pn_tela)
                                  VALUE(pn_campo_i)
                                  VALUE(pn_campo_o).

  DATA: lc_tabname   TYPE dd03v-tabname,
        lc_fieldname TYPE dd03v-fieldname,
        lc_searchelp TYPE shlpname,
        ln_dynnr     TYPE sy-dynnr,
        lc_dynprofld TYPE dynfnam,
        lc_chvid     TYPE zchvid,
        lc_deschvid  TYPE zdeschvid,
        t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                              WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval,
        st_ret       TYPE ddshretval.

* Chama o matchCode
  MOVE: pn_tela              TO ln_dynnr,
        pn_campo_i           TO lc_dynprofld,
        'ZLEST0025'          TO lc_tabname,
        'CHVID'              TO lc_fieldname,
        'ZLESH0003'          TO lc_searchelp.

* Limita a pesquisa somente para categoria Manual
  SET PARAMETER ID: 'ZTH' FIELD 'M'.

* Exibe Ajuda de Pesquisa
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = lc_tabname
      fieldname         = lc_fieldname
      searchhelp        = lc_searchelp
      dynpprog          = sy-repid
      dynpnr            = ln_dynnr
      dynprofield       = lc_dynprofld
    TABLES
      return_tab        = t_ret
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  lc_chvid = st_ret-fieldval.

  SELECT SINGLE deschvid
    INTO lc_deschvid
    FROM zlest0025
   WHERE chvid EQ lc_chvid.

  MOVE: pn_campo_i     TO  t_dynpfields-fieldname,
        lc_chvid       TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: pn_campo_o     TO  t_dynpfields-fieldname,
        lc_deschvid    TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = ln_dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDFORM.                    " EXIBE_MATCH_CODE_CHVID

*&---------------------------------------------------------------------*
*&      Form  ELIMINA_LINHAS_TC_401
*&---------------------------------------------------------------------*
FORM elimina_linhas_tc_401 .

  DATA: li_offset    TYPE i,
        li_leng      TYPE i,
        ln_index(5)  TYPE n,
        lf_clear_dtl,
        lf_erro.

* Obtem o indice selecionado no detalhamento
  IF NOT sdetlh_confer_400-docdetalhe IS INITIAL.

*   Nota: hoje o número da linha está fixa no texto do docto detalhado
    FIND FIRST OCCURRENCE OF REGEX '\d+'
            IN sdetlh_confer_400-docdetalhe
         MATCH OFFSET li_offset
         MATCH LENGTH li_leng.

    IF li_leng > 0.
      ln_index = sdetlh_confer_400-docdetalhe+li_offset(li_leng).
      READ TABLE ti_400_confer INDEX ln_index INTO vg_wa_400_confer.
    ENDIF.

  ENDIF.

* Permite deleção somente para entradas manuais
  LOOP AT ti_400_confer WHERE mark = cc_x.

    vg_index = sy-tabix.

    IF ti_400_confer-acrescentado IS INITIAL.
      lf_erro = cc_x.
    ELSE.

*     Verifica se a eliminação tem detalhamento
      IF vg_wa_400_confer-acrescentado = ti_400_confer-acrescentado AND
         vg_wa_400_confer-chvid        = ti_400_confer-chvid        AND
         vg_wa_400_confer-conhec       = ti_400_confer-conhec       AND
         vg_wa_400_confer-ctafrete     = ti_400_confer-ctafrete.
        lf_clear_dtl = cc_x.
      ENDIF.

*     Atualiza saldo do histórico
      IF sctrl_saldo_400-hist_negat IS INITIAL.
        sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                               + ti_400_confer-valor_acresc.
      ELSE.
        sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                               - ti_400_confer-valor_acresc.
      ENDIF.

*     Elimina linhas do Grid
      DELETE ti_400_confer INDEX vg_index.

*     Elimina linha da base de dados
      DELETE
        FROM zlest0016
       WHERE transportador = ti_400_confer-codtrp
         AND posto         = ti_400_confer-codposto
         AND lote          = ti_400_confer-lote
         AND chvid         = ti_400_confer-chvid
         AND ctafrete      = ti_400_confer-ctafrete
         AND conhecimento  = ti_400_confer-conhec.

    ENDIF.

  ENDLOOP.

* Limpa área de detalhamento
  IF lf_clear_dtl = cc_x.
    CLEAR: sdetlh_confer_400.
    IF vg_obs_edit_400 IS BOUND.
      REFRESH ti_400_obs.
      CALL METHOD vg_obs_edit_400->set_text_as_r3table
        EXPORTING
          table = ti_400_obs.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.
  ENDIF.

* Atualiza valor do saldo de histórico(s) na tela
  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

* Renova Grid
  DESCRIBE TABLE ti_400_confer LINES vg_sbs400_tabcontrol-lines.

* Exibe mensagem se não foi possível eliminar a linha selecionada
  CHECK: lf_erro = cc_x.
  MESSAGE TEXT-m13 TYPE cc_s DISPLAY LIKE cc_e.

ENDFORM.                    " ELIMINA_LINHAS_TC_401

*&---------------------------------------------------------------------*
*&      Form  SALVA_LANCTO_MANUAL_402
*&---------------------------------------------------------------------*
FORM salva_lancto_manual_402 .

  CLEAR vg_index.

* Lê o registro de conferência alterado
  READ TABLE ti_400_confer INTO vg_wa_400_confer
                           WITH KEY codtrp = ti_203_lotes-codtrp
                                  codposto = ti_203_lotes-codposto
                                      lote = ti_203_lotes-lote
                                     chvid = snlancto_confer_400-chvid
  BINARY SEARCH.

* Se não existir registro anexar nova entrada
  IF sy-subrc IS INITIAL.
    vg_index = sy-tabix.
  ELSE.
*   Lê um registro para modelo
    READ TABLE ti_400_confer INTO vg_wa_400_confer
                            WITH KEY
*                            codtrp = ti_203_lotes-codtrp
*                                   codposto = ti_203_lotes-codposto
*                                       lote = ti_203_lotes-lote
                                       mark = cc_x.
*    BINARY SEARCH.
    CHECK: sy-subrc IS INITIAL.
    vg_wa_400_confer-chvid = snlancto_confer_400-chvid.
    vg_wa_400_confer-acrescentado  = cc_x.
  ENDIF.

* Salva valores informado na tela de lançamento manual
  IF snlancto_confer_400-deschvid IS INITIAL.
    SELECT SINGLE deschvid
      INTO snlancto_confer_400-deschvid
      FROM zlest0025
     WHERE chvid = snlancto_confer_400-chvid.
  ENDIF.

  vg_wa_400_confer-deschvid      = snlancto_confer_400-deschvid.
  vg_wa_400_confer-data_acresc   = snlancto_confer_400-data.
  vg_wa_400_confer-valor_acresc  = snlancto_confer_400-valor.
* Verificando se eh necessario ter um conhecimento para o lancamento manual..
  IF zlest0025-vdarcohec = 'N'.
    vg_wa_400_confer-conhec   = space.
    vg_wa_400_confer-ctafrete = space.
  ENDIF.
  CONCATENATE snlancto_confer_400-observ1
              snlancto_confer_400-observ2
              snlancto_confer_400-observ3
         INTO vg_wa_400_confer-observacoes SEPARATED BY space.

  IF vg_index > 0.
    MODIFY ti_400_confer INDEX vg_index FROM vg_wa_400_confer.
  ELSE.
    APPEND vg_wa_400_confer TO ti_400_confer.
  ENDIF.

  DESCRIBE TABLE ti_400_confer LINES vg_sbs400_tabcontrol-lines.

* Inclui novo lançamento na base de dados
  PERFORM inclui_lancto_manual_base.

* Atualiza valores no saldo de histórico(s)
  IF sctrl_saldo_400-saldo_atual < 0.
    sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                           + snlancto_confer_400-valor.
  ELSE.
    sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                           - snlancto_confer_400-valor.
  ENDIF.

  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

* Desmarca linhas da table control
  PERFORM desmarcar_linhas_tc_401.

ENDFORM.                    " SALVA_LANCTO_MANUAL_402

*&---------------------------------------------------------------------*
*&      Form  GERAR_LANCTO_CONTABIL_401
*&---------------------------------------------------------------------*
FORM gerar_lancto_contabil_401 .

  DATA: lc_shtyp          TYPE shtyp,
        p_data_val        TYPE datum,
        lc_bukrs          TYPE bukrs,
        lc_branch	        TYPE j_1bbranc_,
        lc_ctlg_tiptrp(7) TYPE c,
        ld_data_lancto    TYPE d,
        lf_ha_doc_ok.

* Analisa o valor do saldo de histórico para lançamento
  IF sctrl_saldo_400-saldo_atual <> 0.
    WRITE: sctrl_saldo_400-saldo_atual TO vg_campo_char20
        NO-SIGN NO-ZERO.
    CONDENSE: vg_campo_char20.
    MESSAGE s039 WITH vg_campo_char20 DISPLAY LIKE cc_e.
    EXIT.
  ENDIF.
* Analisa a data de fechamento
  IF sheader_300-data_fechamento IS INITIAL.
    MESSAGE s048 DISPLAY LIKE cc_e.
    EXIT.
  ENDIF.

* Verifica se todas ChvId tem conta razão para lançamento
  PERFORM check_chvid_lancto_contab_401 CHANGING vg_msgerro.
  IF NOT vg_msgerro IS INITIAL.
    MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_e.
    EXIT.
  ENDIF.

* Verifica se todas os itens foram marcados para contabilizaçao
  READ TABLE ti_400_confer WITH KEY check = space.
  IF sy-subrc IS INITIAL.
    MESSAGE s049 DISPLAY LIKE cc_e.
    EXIT.
  ENDIF.

* Tabela para gerar entradas na tabela ZIB_CONTABIL
  REFRESH ti_obj_contabil.

* Gera lançamento contábil para histórico (ChvId)
  LOOP AT ti_400_confer.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ti_400_confer-codtrp
      IMPORTING
        output = lc_branch.

    SELECT SINGLE bukrs INTO lc_bukrs
      FROM j_1bbranch
      WHERE branch = lc_branch.

*   Busca o tipo do documento de transporte
    SELECT shtyp
        INTO lc_shtyp
        FROM vttk
          UP TO 1 ROWS
       WHERE tdlnr = ti_400_confer-codtrp
         AND exti1 = ti_400_confer-conhec
         AND exti2 = ti_400_confer-ctafrete.
    ENDSELECT.

    IF ti_400_confer-data_acresc IS INITIAL.
      ld_data_lancto = sy-datum.
    ELSE.
      ld_data_lancto = ti_400_confer-data_acresc.
    ENDIF.

*   Obtem categorias e valores para lançamentos
    PERFORM obtem_ctlglcto_contabil_401 USING lc_shtyp.

    LOOP AT ti_vlrctlglcto.

      CLEAR ti_obj_contabil.

*     Gera descrição para o lançamento
      CONDENSE ti_vlrctlglcto-ctlglancto.
      CONCATENATE ti_vlrctlglcto-ctlglancto '/' lc_shtyp
             INTO lc_ctlg_tiptrp.
      IF ti_vlrctlglcto-historico IS INITIAL.
        CONCATENATE ti_400_confer-dscodposto
                    'CF:'    ti_400_confer-ctafrete
                    'DACTE:' ti_400_confer-conhec
               INTO vg_campo_sgtxt SEPARATED BY space.
      ELSE.
        CONCATENATE ti_vlrctlglcto-historico
                    'CF:'    ti_400_confer-ctafrete
                    'DACTE:' ti_400_confer-conhec
               INTO vg_campo_sgtxt SEPARATED BY space.
      ENDIF.

      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
        EXPORTING
          p_data_ent  = sheader_300-data_fechamento
          p_bukrs     = lc_bukrs
        IMPORTING
          p_data_val  = p_data_val
        EXCEPTIONS
          sem_periodo = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*     Atualiza tabela contábil
      PERFORM lancto_atlz_zib_contabil USING ti_400_confer-codtrp
                                             ti_400_confer-codposto
                                             ti_400_confer-lote
                                             ti_400_confer-chvid
                                             ld_data_lancto
                                             ti_vlrctlglcto-valor
                                             ti_vlrctlglcto-ctdebito
                                             ti_vlrctlglcto-razesp_d
                                             ti_vlrctlglcto-chvlcto_d
                                             ti_vlrctlglcto-ctcredito
                                             ti_vlrctlglcto-razesp_c
                                             ti_vlrctlglcto-chvlcto_c
                                             ti_vlrctlglcto-zuonr_d
                                             ti_vlrctlglcto-zuonr_c
                                             vg_campo_sgtxt
                                             cc_tdoc_conf
                                             p_data_val
                                             space "CSB
                                    CHANGING ti_obj_contabil-obj_key
                                             ti_obj_contabil-bukrs.

*     Salva documentos para geração lançamento
      ti_obj_contabil-codtrp      = ti_400_confer-codtrp.
      ti_obj_contabil-codposto    = ti_400_confer-codposto.
      ti_obj_contabil-lote        = ti_400_confer-lote.
      ti_obj_contabil-chvid       = ti_400_confer-chvid.
      ti_obj_contabil-valor       = ti_vlrctlglcto-valor.
      ti_obj_contabil-tiptransp   = ti_vlrctlglcto-tiptransp.
      ti_obj_contabil-ctlglancto  = ti_vlrctlglcto-ctlglancto.
      ti_obj_contabil-data_confer = ld_data_lancto.
      ti_obj_contabil-observ      = ti_400_confer-observacoes.
      ti_obj_contabil-conhec      = ti_400_confer-conhec.
      APPEND ti_obj_contabil.

    ENDLOOP.

  ENDLOOP.

  CHECK: NOT ti_obj_contabil[] IS INITIAL.

* Gera documento contábil
*  PERFORM lancto_gera_docto_contabil USING cc_a_confirmar
*                                  CHANGING lf_ha_doc_ok.
*  CHECK: lf_ha_doc_ok = cc_x.

  PERFORM atlz_status_docto_confer_401.

* Atualiza tela de Lote
  IF ti_203_lotes[] IS INITIAL.
*   Retorna para nova consulta
    PERFORM renova_tela_para_pesquisa.
    PERFORM reseta_vlrsalvo_selecao_1200.
  ELSE.
*   Renova Grid
    DESCRIBE TABLE ti_203_lotes LINES vg_sbs203_tabcontrol-lines.
*   Avança para aba de Lote
    vg_dynnr_tabstrip = '0200'.
    vg_main100_tabstrip-activetab = cc_100_tabstrip-tab1.
  ENDIF.

ENDFORM.                    " GERAR_LANCTO_CONTABIL_401

*&---------------------------------------------------------------------*
*&      Form  CHECK_CHVID_LANCTO_CONTAB_401
*&---------------------------------------------------------------------*
FORM check_chvid_lancto_contab_401 CHANGING p_msgerro.

  DATA: lf_erro_ctraz    TYPE zctlglancto,
        lc_shtyp         TYPE shtyp,
        vl_valida_ctr    TYPE c,
        lc_lifnr         TYPE lifnr,
        lc_branch	       TYPE j_1bbranc_,
        lc_bukrs         TYPE bukrs,
        l_exige_ccusto,
        lf_erro_ccusto,
        lf_chk_conferido.

  CLEAR:  p_msgerro.

* Verifica se todos lançamentos estão corretos
  LOOP AT ti_400_confer.

    vg_index = sy-tabix.
    CLEAR: lf_erro_ctraz,
           vl_valida_ctr.

*   Verifica se para a chave do historico é necessario validar o documento de transportte
    SELECT SINGLE vdarcohec
        INTO vl_valida_ctr
       FROM zlest0025
      WHERE chvid = ti_400_confer-chvid.
    IF sy-subrc <> 0 OR vl_valida_ctr EQ 'S'.
*   Busca o tipo do documento de transporte
      SELECT shtyp
          INTO lc_shtyp
          FROM vttk
            UP TO 1 ROWS
         WHERE tdlnr = ti_400_confer-codtrp
           AND exti1 = ti_400_confer-conhec
           AND exti2 = ti_400_confer-ctafrete.
      ENDSELECT.

      IF sy-subrc <> 0.
        WRITE vg_index TO vg_campo_char05 NO-ZERO.
        CONDENSE vg_campo_char05.
        MESSAGE s040 WITH ti_400_confer-ctafrete ti_400_confer-conhec
                     INTO p_msgerro.
        CONCATENATE 'Linha nº' vg_campo_char05 '-' p_msgerro
               INTO p_msgerro SEPARATED BY space.
*       Encerra o Loop
        EXIT.
      ENDIF.
    ENDIF.

    SELECT t2~lifnr
      INTO lc_lifnr
      FROM vttk AS t1
     INNER JOIN vtpa AS t2
        ON t2~vbeln = t1~tknum
       AND t2~parvw = cc_parvw_pv
        UP TO 1 ROWS
     WHERE t1~tdlnr = ti_400_confer-codtrp
       AND t1~exti1 = ti_400_confer-conhec
       AND t1~exti2 = ti_400_confer-ctafrete.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ti_400_confer-codtrp
        IMPORTING
          output = lc_branch.

      SELECT SINGLE bukrs INTO lc_bukrs
        FROM j_1bbranch
        WHERE branch = lc_branch.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          p_koart      = 'K'
          p_empresa    = lc_bukrs
          p_fornecedor = lc_lifnr
        EXCEPTIONS
          error        = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO p_msgerro.
        EXIT.
      ENDIF.

    ENDSELECT.

    IF ti_400_confer-acrescentado IS INITIAL.

      PERFORM obtem_vlrs_p_q_s_vc_vp_401 USING lc_shtyp
                                      CHANGING vg_vlr_perda
                                               vg_vlr_quebra
                                               vg_vlr_sobra
                                               vg_vlr_confer
                                               vg_vlr_programado.
*     Verifica conta razão para ChvId de perda
      IF vg_vlr_perda <> 0.
        PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                   ti_400_confer-codposto
                                                   ti_400_confer-conhec
                                                   ti_400_confer-ctafrete
                                                   ti_400_confer-chvid
                                                   lc_shtyp
                                                   cc_ctlg_perda
                                                   vl_valida_ctr
                                          CHANGING zlest0018.
        IF zlest0018-contadebito IS INITIAL OR
          zlest0018-contacredito IS INITIAL.
          lf_erro_ctraz = cc_ctlg_perda.
        ENDIF.
      ENDIF.

*     Verifica conta razão para ChvId de quebra
      IF vg_vlr_quebra <> 0 AND lf_erro_ctraz IS INITIAL.
        PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                   ti_400_confer-codposto
                                                   ti_400_confer-conhec
                                                   ti_400_confer-ctafrete
                                                   ti_400_confer-chvid
                                                   lc_shtyp
                                                   cc_ctlg_quebra
                                                   vl_valida_ctr
                                          CHANGING zlest0018.
        IF zlest0018-contadebito IS INITIAL OR
          zlest0018-contacredito IS INITIAL.
          lf_erro_ctraz = cc_ctlg_quebra.
        ENDIF.
      ENDIF.

*     Verifica conta razão para ChvId de sobra
      IF vg_vlr_sobra <> 0 AND lf_erro_ctraz IS INITIAL.
        PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                   ti_400_confer-codposto
                                                   ti_400_confer-conhec
                                                   ti_400_confer-ctafrete
                                                   ti_400_confer-chvid
                                                   lc_shtyp
                                                   cc_ctlg_sobra
                                                   vl_valida_ctr
                                          CHANGING zlest0018.
        IF zlest0018-contadebito IS INITIAL OR
          zlest0018-contacredito IS INITIAL.
          lf_erro_ctraz = cc_ctlg_sobra.
        ENDIF.
      ENDIF.

*     NOTA: Não usar valor programado para lançamento !!!

      lf_chk_conferido = cc_x.

*     Verifica conta razão para ChvId de valor de conferência
*     <> OU Verifica conta razão para ChvId de valor programado

***      IF ti_400_confer-codtrp = cc_chvid_adiant OR
***         ti_400_confer-codtrp = cc_chvid_autpag.
***
****       Verifica conta razão para ChvId Valor Programado
***        IF vg_vlr_programado <> 0 AND lf_erro_ctraz IS INITIAL.
***          PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
***                                                     ti_400_confer-codposto
***                                                     ti_400_confer-conhec
***                                                     ti_400_confer-ctafrete
***                                                     ti_400_confer-chvid
***                                                     lc_shtyp
***                                                     cc_ctlg_vlrprogdo
***                                            CHANGING zlest0018.
***          IF zlest0018-contadebito  IS INITIAL OR
***             zlest0018-contacredito IS INITIAL.
***            lf_erro_ctraz = cc_ctlg_vlrprogdo.
***          ELSE.
****           Sucesso no programado despreza o check para conferido
***            CLEAR lf_chk_conferido.
***          ENDIF.
***        ENDIF.
***      ENDIF.

      IF lf_chk_conferido = cc_x.
        IF vg_vlr_confer <> 0 AND lf_erro_ctraz IS INITIAL.
          PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                     ti_400_confer-codposto
                                                     ti_400_confer-conhec
                                                     ti_400_confer-ctafrete
                                                     ti_400_confer-chvid
                                                     lc_shtyp
                                                     cc_ctlg_vlrconfer
                                                     vl_valida_ctr
                                            CHANGING zlest0018.
          IF zlest0018-contadebito IS INITIAL OR
            zlest0018-contacredito IS INITIAL.
            lf_erro_ctraz = cc_ctlg_vlrconfer.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

*     Verifica conta razão para ChvId de conferência - Manual
      IF ti_400_confer-valor_acresc <> 0.
        PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                   ti_400_confer-codposto
                                                   ti_400_confer-conhec
                                                   ti_400_confer-ctafrete
                                                   ti_400_confer-chvid
                                                   lc_shtyp
                                                   cc_ctlg_vlrconfer
                                                   vl_valida_ctr
                                          CHANGING zlest0018.
        IF zlest0018-contadebito IS INITIAL OR
          zlest0018-contacredito IS INITIAL.
          lf_erro_ctraz = cc_ctlg_vlrconfer.
        ENDIF.
      ENDIF.
    ENDIF.

*   Verifica sucesso
    IF NOT lf_erro_ctraz IS INITIAL.

      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONDENSE vg_campo_char05.

      MESSAGE w041 WITH ti_400_confer-chvid lc_shtyp
         INTO p_msgerro.
      CONCATENATE 'Linha nº' vg_campo_char05 '-' p_msgerro
             INTO p_msgerro SEPARATED BY space.

      vg_dd_name  = 'ZCTLGLANCTO'.
      vg_dd_value = lf_erro_ctraz.
      CLEAR: vg_wa_dd07v.

      CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
        EXPORTING
          name          = vg_dd_name
          value         = vg_dd_value
        IMPORTING
          dd07v_wa      = vg_wa_dd07v
        EXCEPTIONS
          not_found     = 1
          illegal_input = 2
          OTHERS        = 3.

      IF NOT vg_wa_dd07v-ddtext IS INITIAL.
        CONCATENATE p_msgerro '( categoria:' vg_wa_dd07v-ddtext ')'
               INTO p_msgerro SEPARATED BY space.
      ENDIF.
*     Encerra loop
      EXIT.
    ENDIF.

*   Verifica se a conta contábil existe centro de custo
    CHECK: lf_erro_ctraz IS INITIAL
      AND  NOT zlest0018-contadebito  IS INITIAL
      AND  NOT zlest0018-contacredito IS INITIAL.

    CLEAR: lf_erro_ccusto.

*   Verifica conta de débito para centro de custo
    PERFORM check_contarazao_exige_ccusto USING zlest0018-contadebito
                                                cc_d
                                                zlest0018-chvlancto_d
                                       CHANGING l_exige_ccusto.
    IF l_exige_ccusto = cc_x.
      PERFORM obtem_centro_custo_contarazao USING ti_400_confer-codtrp
                                                  cc_d
                                                  zlest0018-contadebito
                                         CHANGING vg_kostl.
      IF vg_kostl IS INITIAL.
        lf_erro_ccusto = cc_x.
      ENDIF.
    ENDIF.

*   Verifica conta de crédito para centro de custo
    IF lf_erro_ccusto IS INITIAL.
      PERFORM check_contarazao_exige_ccusto USING zlest0018-contacredito
                                                  cc_c
                                                  zlest0018-chvlancto_c
                                         CHANGING l_exige_ccusto.
      IF l_exige_ccusto = cc_x.
        PERFORM obtem_centro_custo_contarazao USING ti_400_confer-codtrp
                                                    cc_c
                                                    zlest0018-contacredito
                                           CHANGING vg_kostl.
        IF vg_kostl IS INITIAL.
          lf_erro_ccusto = cc_x.
        ENDIF.
      ENDIF.
    ENDIF.

*   Verifica erro de conta razão x centro de custo
    IF lf_erro_ccusto = cc_x.

      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONDENSE vg_campo_char05.

      MESSAGE w046 WITH ti_400_confer-chvid INTO p_msgerro.
      CONCATENATE 'Linha nº' vg_campo_char05 '-' p_msgerro
             INTO p_msgerro SEPARATED BY space.

*     Encerra loop
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_CHVID_LANCTO_CONTAB_401

*&---------------------------------------------------------------------*
*&      Form  OBTEM_VLRS_P_Q_S_VC_VP_401
*&---------------------------------------------------------------------*
FORM obtem_vlrs_p_q_s_vc_vp_401  USING p_tiptrp
                              CHANGING p_perda
                                       p_quebra
                                       p_sobra
                                       p_confer
                                       p_progrdo.
  CLEAR: p_perda,
         p_quebra,
         p_sobra,
         p_confer,
         p_progrdo.

  CHECK: ti_400_confer-acrescentado = space.

* Dados de posto - Detalhes
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_400_confer-codtrp
                                    codposto = ti_400_confer-codposto
                                    lote     = ti_400_confer-lote
                                  ctafrete   = ti_400_confer-ctafrete
                                    conhec   = ti_400_confer-conhec
                                    chvid    = ti_400_confer-chvid
                                    datalote = ti_203_lotes-datalote
                           BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

* Dados de calculo - Deltas
  CLEAR: vg_wa_deltas.
  READ TABLE ti_cockpit_deltas INTO vg_wa_deltas
                           WITH KEY codtrp = vg_wa_lancto-codtrp
                                  codposto = vg_wa_lancto-codposto
                                      lote = vg_wa_lancto-lote
                                  ctafrete = vg_wa_lancto-ctafrete
                                    conhec = vg_wa_lancto-conhec
                                     chvid = vg_wa_lancto-chvid
                                  datalote = vg_wa_lancto-datalote
                           BINARY SEARCH.

* Valor Conferido
  IF vg_wa_lancto-vlrconfiralter IS INITIAL.
    p_confer = vg_wa_lancto-vlrconfirmado.
  ELSE.
    p_confer = vg_wa_lancto-vlrconfiralter.
  ENDIF.

* Valor Programado
  p_progrdo = vg_wa_lancto-vlrprogramado.

* Valor de Quebra ou Sobra
  IF vg_wa_deltas-vlrperda <= 0.
    p_quebra = vg_wa_deltas-vlrperda.
  ELSE.
    p_sobra = vg_wa_deltas-vlrperda.
  ENDIF.

* Valor de Perda
  p_perda = vg_wa_deltas-difer_transp.

ENDFORM.                    " OBTEM_VLRS_P_Q_S_VC_VP_401

*&---------------------------------------------------------------------*
*&      Form  OBTEM_CTLGLCTO_CONTABIL_401
*&---------------------------------------------------------------------*
FORM obtem_ctlglcto_contabil_401 USING p_tiptrp.

  DATA: lf_chk_conferido,
        vg_chave TYPE zchvid.

  DATA: wa_aux_zlest0022 TYPE zlest0022.

  CASE ti_400_confer-chvid.
    WHEN '1'.

      SELECT SINGLE * INTO wa_aux_zlest0022
        FROM zlest0022
       WHERE chvid         EQ '28'
         AND transportador EQ ti_400_confer-codtrp
         AND posto         EQ ti_400_confer-codposto
         AND lote          EQ ti_400_confer-lote
         AND conhecimento  EQ ti_400_confer-conhec
         AND ctafrete      EQ ti_400_confer-ctafrete.

      IF sy-subrc IS INITIAL.
        vg_chave = '28'.
      ELSE.
        vg_chave = ti_400_confer-chvid.
      ENDIF.

    WHEN '2'.

      SELECT SINGLE * INTO wa_aux_zlest0022
        FROM zlest0022
       WHERE chvid         EQ '26'
         AND transportador EQ ti_400_confer-codtrp
         AND posto         EQ ti_400_confer-codposto
         AND lote          EQ ti_400_confer-lote
         AND conhecimento  EQ ti_400_confer-conhec
         AND ctafrete      EQ ti_400_confer-ctafrete.

      IF sy-subrc IS INITIAL.
        vg_chave = '26'.
      ELSE.
        vg_chave = ti_400_confer-chvid.
      ENDIF.

    WHEN OTHERS.
      vg_chave = ti_400_confer-chvid.
  ENDCASE.

  REFRESH: ti_vlrctlglcto.

*Histórico - Importação
  IF ti_400_confer-acrescentado = space.

    PERFORM obtem_vlrs_p_q_s_vc_vp_401 USING p_tiptrp
                                    CHANGING vg_vlr_perda
                                             vg_vlr_quebra
                                             vg_vlr_sobra
                                             vg_vlr_confer
                                             vg_vlr_programado.
*Obtem conta razão para perda
    IF vg_vlr_perda <> 0.
      PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                 ti_400_confer-codposto
                                                 ti_400_confer-conhec
                                                 ti_400_confer-ctafrete
                                                 vg_chave
                                                 p_tiptrp
                                                 cc_ctlg_perda
                                                 space
                                        CHANGING zlest0018.
      IF NOT zlest0018-contadebito  IS INITIAL AND
         NOT zlest0018-contacredito IS INITIAL.
        ti_vlrctlglcto-ctlglancto  = cc_ctlg_perda.
        ti_vlrctlglcto-tiptransp   = p_tiptrp.
        ti_vlrctlglcto-ctdebito    = zlest0018-contadebito.
        ti_vlrctlglcto-razesp_d    = zlest0018-razaoesp_d.
        ti_vlrctlglcto-chvlcto_d   = zlest0018-chvlancto_d.
        ti_vlrctlglcto-ctcredito   = zlest0018-contacredito.
        ti_vlrctlglcto-razesp_c    = zlest0018-razaoesp_c.
        ti_vlrctlglcto-chvlcto_c   = zlest0018-chvlancto_c.
        ti_vlrctlglcto-tipoconta_c = zlest0018-tipoconta_c.
        ti_vlrctlglcto-tipoconta_d = zlest0018-tipoconta_d.
        ti_vlrctlglcto-valor       = vg_vlr_perda.
        ti_vlrctlglcto-historico   = zlest0018-especificacao.

        IF ( ( vg_chave EQ '26' ) OR ( vg_chave EQ '28' ) ).
          CLEAR: ti_vlrctlglcto-zuonr_d,
                 ti_vlrctlglcto-zuonr_c.
          CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
        ELSE.
          IF ti_vlrctlglcto-razesp_d IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
          ELSE.
            CASE ti_400_confer-chvid.
              WHEN '18' OR '20'.
                CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
              WHEN '17'.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
              WHEN OTHERS.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
            ENDCASE.
          ENDIF.

          IF ti_vlrctlglcto-razesp_c IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_c.
          ELSE.
            CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_c.
          ENDIF.
        ENDIF.

        APPEND ti_vlrctlglcto.
      ENDIF.
    ENDIF.

*   Obtem conta razão para valor sobra
    IF vg_vlr_sobra <> 0.
      PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                 ti_400_confer-codposto
                                                 ti_400_confer-conhec
                                                 ti_400_confer-ctafrete
                                                 vg_chave
                                                 p_tiptrp
                                                 cc_ctlg_sobra
                                                 space
                                        CHANGING zlest0018.
      IF NOT zlest0018-contadebito  IS INITIAL AND
         NOT zlest0018-contacredito IS INITIAL.
        ti_vlrctlglcto-ctlglancto = cc_ctlg_sobra.
        ti_vlrctlglcto-tiptransp  = p_tiptrp.
        ti_vlrctlglcto-ctdebito   = zlest0018-contadebito.
        ti_vlrctlglcto-razesp_d   = zlest0018-razaoesp_d.
        ti_vlrctlglcto-chvlcto_d  = zlest0018-chvlancto_d.
        ti_vlrctlglcto-ctcredito  = zlest0018-contacredito.
        ti_vlrctlglcto-razesp_c   = zlest0018-razaoesp_c.
        ti_vlrctlglcto-chvlcto_c  = zlest0018-chvlancto_c.
        ti_vlrctlglcto-tipoconta_c = zlest0018-tipoconta_c.
        ti_vlrctlglcto-tipoconta_d = zlest0018-tipoconta_d.
        ti_vlrctlglcto-valor      = vg_vlr_sobra.
        ti_vlrctlglcto-historico  = zlest0018-especificacao.

        IF ( ( vg_chave EQ '26' ) OR ( vg_chave EQ '28' ) ).
          CLEAR: ti_vlrctlglcto-zuonr_d,
                 ti_vlrctlglcto-zuonr_c.
          CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
        ELSE.
          IF ti_vlrctlglcto-razesp_d IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
          ELSE.
            CASE ti_400_confer-chvid.
              WHEN '18' OR '20'.
                CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
              WHEN '17'.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
              WHEN OTHERS.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
            ENDCASE.
          ENDIF.

          IF ti_vlrctlglcto-razesp_c IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_c.
          ELSE.
            CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_c.
          ENDIF.
        ENDIF.

        APPEND ti_vlrctlglcto.
      ENDIF.
    ENDIF.

*   Obtem conta razão para valor quebra
    IF vg_vlr_quebra <> 0.
      PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                 ti_400_confer-codposto
                                                 ti_400_confer-conhec
                                                 ti_400_confer-ctafrete
                                                 vg_chave
                                                 p_tiptrp
                                                 cc_ctlg_quebra
                                                 space
                                        CHANGING zlest0018.
      IF NOT zlest0018-contadebito  IS INITIAL AND
         NOT zlest0018-contacredito IS INITIAL.
        ti_vlrctlglcto-ctlglancto = cc_ctlg_quebra.
        ti_vlrctlglcto-tiptransp  = p_tiptrp.
        ti_vlrctlglcto-ctdebito   = zlest0018-contadebito.
        ti_vlrctlglcto-razesp_d   = zlest0018-razaoesp_d.
        ti_vlrctlglcto-chvlcto_d  = zlest0018-chvlancto_d.
        ti_vlrctlglcto-ctcredito  = zlest0018-contacredito.
        ti_vlrctlglcto-razesp_c   = zlest0018-razaoesp_c.
        ti_vlrctlglcto-chvlcto_c  = zlest0018-chvlancto_c.
        ti_vlrctlglcto-tipoconta_c = zlest0018-tipoconta_c.
        ti_vlrctlglcto-tipoconta_d = zlest0018-tipoconta_d.
        ti_vlrctlglcto-valor      = vg_vlr_quebra.
        ti_vlrctlglcto-historico  = zlest0018-especificacao.

        IF ( ( vg_chave EQ '26' ) OR ( vg_chave EQ '28' ) ).
          CLEAR: ti_vlrctlglcto-zuonr_d,
                 ti_vlrctlglcto-zuonr_c.
          CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
        ELSE.
          IF ti_vlrctlglcto-razesp_d IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
          ELSE.
            CASE ti_400_confer-chvid.
              WHEN '18' OR '20'.
                CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
              WHEN '17'.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
              WHEN OTHERS.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
            ENDCASE.
          ENDIF.

          IF ti_vlrctlglcto-razesp_c IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_c.
          ELSE.
            CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_c.
          ENDIF.
        ENDIF.

        APPEND ti_vlrctlglcto.
      ENDIF.
    ENDIF.

*   NOTA: Não usar valor programado para lançamento !!!

    lf_chk_conferido = cc_x.

*   Verifica conta razão para ChvId de valor de conferência
*   < OU > Verifica conta razão para ChvId de valor programado
***    IF ti_400_confer-codtrp = cc_chvid_adiant OR
***       ti_400_confer-codtrp = cc_chvid_autpag.
***
****     Obtem conta razão para valor programado
***      IF vg_vlr_programado <> 0.
***        PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
***                                                   ti_400_confer-codposto
***                                                   ti_400_confer-conhec
***                                                   ti_400_confer-ctafrete
***                                                   ti_400_confer-chvid
***                                                   p_tiptrp
***                                                   cc_ctlg_vlrprogdo
***                                          CHANGING zlest0018.
***        IF NOT zlest0018-contadebito  IS INITIAL AND
***           NOT zlest0018-contacredito IS INITIAL.
***          ti_vlrctlglcto-ctlglancto = cc_ctlg_vlrconfer.
***          ti_vlrctlglcto-tiptransp  = p_tiptrp.
***          ti_vlrctlglcto-ctdebito   = zlest0018-contadebito.
***          ti_vlrctlglcto-razesp_d   = zlest0018-razaoesp_d.
***          ti_vlrctlglcto-chvlcto_d  = zlest0018-chvlancto_d.
***          ti_vlrctlglcto-ctcredito  = zlest0018-contacredito.
***          ti_vlrctlglcto-razesp_c   = zlest0018-razaoesp_c.
***          ti_vlrctlglcto-chvlcto_c  = zlest0018-chvlancto_c.
***          ti_vlrctlglcto-valor      = vg_vlr_programado.
***          ti_vlrctlglcto-historico  = zlest0018-especificacao.
***          APPEND ti_vlrctlglcto.
***          CLEAR lf_chk_conferido.
***        ENDIF.
***      ENDIF.
***    ENDIF.

*   Obtem conta razão para valor conferido
    IF vg_vlr_confer <> 0 AND lf_chk_conferido = cc_x.
      PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                 ti_400_confer-codposto
                                                 ti_400_confer-conhec
                                                 ti_400_confer-ctafrete
                                                 vg_chave
                                                 p_tiptrp
                                                 cc_ctlg_vlrconfer
                                                 space
                                        CHANGING zlest0018.
      IF NOT zlest0018-contadebito  IS INITIAL AND
         NOT zlest0018-contacredito IS INITIAL.
        ti_vlrctlglcto-ctlglancto = cc_ctlg_vlrconfer.
        ti_vlrctlglcto-tiptransp  = p_tiptrp.
        ti_vlrctlglcto-ctdebito   = zlest0018-contadebito.
        ti_vlrctlglcto-razesp_d   = zlest0018-razaoesp_d.
        ti_vlrctlglcto-chvlcto_d  = zlest0018-chvlancto_d.
        ti_vlrctlglcto-ctcredito  = zlest0018-contacredito.
        ti_vlrctlglcto-razesp_c   = zlest0018-razaoesp_c.
        ti_vlrctlglcto-chvlcto_c  = zlest0018-chvlancto_c.
        ti_vlrctlglcto-tipoconta_c = zlest0018-tipoconta_c.
        ti_vlrctlglcto-tipoconta_d = zlest0018-tipoconta_d.
        ti_vlrctlglcto-valor      = vg_vlr_confer.
        ti_vlrctlglcto-historico  = zlest0018-especificacao.

        IF ( ( vg_chave EQ '26' ) OR ( vg_chave EQ '28' ) ).
          CLEAR: ti_vlrctlglcto-zuonr_d,
                 ti_vlrctlglcto-zuonr_c.
          CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
        ELSE.
          IF ti_vlrctlglcto-razesp_d IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
          ELSE.
            CASE ti_400_confer-chvid.
              WHEN '18' OR '20'.
                CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
              WHEN '17'.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
              WHEN OTHERS.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
            ENDCASE.
          ENDIF.

          IF ti_vlrctlglcto-razesp_c IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_c.
          ELSE.
            CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_c.
          ENDIF.
        ENDIF.

        APPEND ti_vlrctlglcto.
      ENDIF.
    ENDIF.

* Histórico acrescentado
  ELSE.

*   Obtem conta razão para valor conferido
    IF ti_400_confer-valor_acresc <> 0.
      PERFORM obtem_ctrazao_catlglcto_401  USING ti_400_confer-codtrp
                                                 ti_400_confer-codposto
                                                 ti_400_confer-conhec
                                                 ti_400_confer-ctafrete
                                                 vg_chave
                                                 p_tiptrp
                                                 cc_ctlg_vlrconfer
                                                 space
                                        CHANGING zlest0018.
      IF NOT zlest0018-contadebito  IS INITIAL AND
         NOT zlest0018-contacredito IS INITIAL.
        ti_vlrctlglcto-ctlglancto = cc_ctlg_vlrconfer.
        ti_vlrctlglcto-tiptransp  = p_tiptrp.
        ti_vlrctlglcto-ctdebito   = zlest0018-contadebito.
        ti_vlrctlglcto-razesp_d   = zlest0018-razaoesp_d.
        ti_vlrctlglcto-chvlcto_d  = zlest0018-chvlancto_d.
        ti_vlrctlglcto-ctcredito  = zlest0018-contacredito.
        ti_vlrctlglcto-razesp_c   = zlest0018-razaoesp_c.
        ti_vlrctlglcto-chvlcto_c  = zlest0018-chvlancto_c.
        ti_vlrctlglcto-tipoconta_c = zlest0018-tipoconta_c.
        ti_vlrctlglcto-tipoconta_d = zlest0018-tipoconta_d.
        ti_vlrctlglcto-valor      = ti_400_confer-valor_acresc.
        ti_vlrctlglcto-historico  = zlest0018-especificacao.

        IF ( ( vg_chave EQ '26' ) OR ( vg_chave EQ '28' ) ).
          CLEAR: ti_vlrctlglcto-zuonr_d,
                 ti_vlrctlglcto-zuonr_c.
          CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
        ELSE.
          IF ti_vlrctlglcto-razesp_d IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
          ELSE.
            CASE ti_400_confer-chvid.
              WHEN '18' OR '20'.
                CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_d.
              WHEN '17'.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
              WHEN OTHERS.
                CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_d.
            ENDCASE.
          ENDIF.

          IF ti_vlrctlglcto-razesp_c IS INITIAL.
            CONCATENATE 'FR-' ti_400_confer-ctafrete INTO ti_vlrctlglcto-zuonr_c.
          ELSE.
            CONCATENATE 'FR-' ti_400_confer-lote INTO ti_vlrctlglcto-zuonr_c.
          ENDIF.
        ENDIF.

        APPEND ti_vlrctlglcto.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " OBTEM_CTLGLCTO_CONTABIL_401

*&---------------------------------------------------------------------*
*&      Form  OBTEM_CTRAZAO_CATLGLCTO_401
*&---------------------------------------------------------------------*
FORM obtem_ctrazao_catlglcto_401  USING p_codtrp
                                        p_codposto
                                        p_conhec
                                        p_ctafrete
                                        p_chvid
                                        p_shtyp
                                        p_ctlgl
                                        p_vldctr
                               CHANGING p_zlest0018 TYPE zlest0018.

  DATA: lc_lifnr  TYPE lifnr.
  CLEAR: p_zlest0018.
  IF p_vldctr EQ 'S'.
    SELECT SINGLE *
      INTO p_zlest0018
      FROM zlest0018
     WHERE chvid       = p_chvid
       AND tiptransp   = p_shtyp
       AND ctlglancto  = p_ctlgl
       AND tipcontabil = 'FC'.
  ELSE.
    SELECT SINGLE *
      INTO p_zlest0018
      FROM zlest0018
     WHERE chvid       = p_chvid
*       AND tiptransp  = p_shtyp
       AND ctlglancto  = p_ctlgl
       AND tipcontabil = 'FC'.
  ENDIF.
  CHECK: sy-subrc IS INITIAL.

* Fornecedor SubContrato
  IF p_zlest0018-tipoconta_d = cc_tipcta_fs OR
     p_zlest0018-tipoconta_c = cc_tipcta_fs.

    SELECT t2~lifnr
      INTO lc_lifnr
      FROM vttk AS t1
     INNER JOIN vtpa AS t2
        ON t2~vbeln = t1~tknum
       AND t2~parvw = cc_parvw_pv
        UP TO 1 ROWS
     WHERE t1~tdlnr = p_codtrp
       AND t1~exti1 = p_conhec
       AND t1~exti2 = p_ctafrete.
    ENDSELECT.

  ENDIF.

* Verifica o tipo de conta
* FS = Fornecedor SubContrato
* FP = Fornecedor Posto
* RZ = Conta Razão
  IF p_zlest0018-contadebito IS INITIAL.
    IF p_zlest0018-tipoconta_d = cc_tipcta_fs.
      p_zlest0018-contadebito = lc_lifnr.
    ELSEIF p_zlest0018-tipoconta_d = cc_tipcta_fp.
      p_zlest0018-contadebito = p_codposto.
    ENDIF.
  ENDIF.

  IF p_zlest0018-contacredito IS INITIAL.
    IF p_zlest0018-tipoconta_c = cc_tipcta_fs.
      p_zlest0018-contacredito = lc_lifnr.
    ELSEIF p_zlest0018-tipoconta_c = cc_tipcta_fp.
      p_zlest0018-contacredito = p_codposto.
    ENDIF.
  ENDIF.

ENDFORM.                    " OBTEM_CTRAZAO_CATLGLCTO_401

*&---------------------------------------------------------------------*
*&      Form  OBTEM_VALOR_NF_401
*&---------------------------------------------------------------------*
FORM obtem_valor_nf_401  USING  p_codtrp
                                p_conhec
                                p_ctafrete
                      CHANGING  p_vlr_nf.

  TYPES BEGIN OF tp_rem.
  TYPES: vbeln TYPE vbeln,
         tknum TYPE tknum,
         tpnum TYPE tpnum,
         posnn TYPE posnr_nach.
  TYPES END OF tp_rem.

  RANGES: rg_reftyp  FOR j_1bnflin-reftyp OCCURS 0,
          rg_vbtyp_n FOR vbfa-vbtyp_n     OCCURS 0.

  STATICS: lc_codtrp   TYPE lifnr,
           lc_conhec   TYPE exti1,
           lc_ctafrete TYPE exti2,
           lp_vlr_nf   TYPE j_1bnftot,
           lw_range_2  TYPE lxhme_range_c2,
           lw_range_1  TYPE lxhme_range_c1,
           vl_refkey   TYPE j_1bnflin-refkey.

  DATA: BEGIN OF lt_nf OCCURS 100,
          docnum TYPE j_1bdocnum,
        END   OF lt_nf,
        lt_rem             TYPE TABLE OF tp_rem WITH HEADER LINE,
        wa_rem             LIKE lt_rem,
        mt_vbfa_remessa    TYPE TABLE OF vbfa WITH HEADER LINE,
        nf_aviso_receb     TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE,
        vg_vstel           TYPE char01,
        wa_zcte_identifica TYPE zcte_identifica,
        it_zcte_info_nota  TYPE TABLE OF zcte_info_nota WITH HEADER LINE,

        BEGIN OF lt_vbfa OCCURS 100,
          vbeln   TYPE vbeln_nach,
          posnn   TYPE posnr_nach,
          vbtyp_n TYPE vbtyp_n,               "#EC CI_USAGE_OK[2198647]
          refkey  TYPE j_1brefkey,
        END   OF lt_vbfa.

* Verifica documento em memória
  IF p_codtrp   = lc_codtrp AND
     p_conhec   = lc_conhec AND
     p_ctafrete = lc_ctafrete.
    p_vlr_nf = lp_vlr_nf.
    EXIT.
  ELSE.
    lc_codtrp   = p_codtrp.
    lc_conhec   = p_conhec.
    lc_ctafrete = p_ctafrete.
    CLEAR: p_vlr_nf,
           lp_vlr_nf.
  ENDIF.

* Obtendo as remessas referente
  SELECT t2~vbeln t2~tknum t2~tpnum
      INTO TABLE lt_rem
      FROM vttk AS t1
     INNER JOIN vttp AS t2
        ON t2~tknum = t1~tknum
     WHERE t1~tdlnr = lc_codtrp
       AND t1~exti1 = lc_conhec
       AND t1~exti2 = lc_ctafrete.

  LOOP AT lt_rem INTO wa_rem.
    wa_rem-posnn = wa_rem-tpnum.
    MODIFY lt_rem INDEX sy-tabix FROM wa_rem TRANSPORTING posnn.
  ENDLOOP.

  SELECT * INTO TABLE mt_vbfa_remessa
    FROM vbfa
    FOR ALL ENTRIES IN lt_rem
   WHERE vbtyp_n EQ '8'
     AND vbeln   EQ lt_rem-tknum
     AND posnn   EQ lt_rem-posnn
     AND vbelv   EQ lt_rem-vbeln.

  READ TABLE mt_vbfa_remessa INDEX 1.

  IF mt_vbfa_remessa-vbtyp_v EQ 'J'.

    LOOP AT lt_rem.

      CLEAR: vg_vstel.

      CALL FUNCTION 'Z_LES_TIPO_REMESSA'
        EXPORTING
          p_vbeln  = lt_rem-vbeln
        CHANGING
          vg_vstel = vg_vstel.

      IF NOT vg_vstel IS INITIAL.

        SELECT SINGLE * INTO wa_zcte_identifica
          FROM zcte_identifica
         WHERE tknum EQ lt_rem-tknum.

        IF sy-subrc IS INITIAL.

          SELECT * INTO TABLE it_zcte_info_nota
            FROM zcte_info_nota
           WHERE docnum EQ wa_zcte_identifica-docnum.

          LOOP AT it_zcte_info_nota.
            lp_vlr_nf = lp_vlr_nf + it_zcte_info_nota-vl_nota_fiscal.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    lw_range_1-sign   = cc_i.
    lw_range_1-option = cc_eq.
    lw_range_1-low    = cc_vbtypn_r.
    APPEND lw_range_1 TO rg_vbtyp_n.
    lw_range_1-low    = cc_vbtypn_m.
    APPEND lw_range_1 TO rg_vbtyp_n.

* Seleciona documento NF no fluxo de documentos
    SELECT vbeln posnn vbtyp_n mjahr
      INTO TABLE lt_vbfa
      FROM vbfa
       FOR ALL ENTRIES IN lt_rem
     WHERE vbelv   = lt_rem-vbeln
       AND vbtyp_v = cc_vbtypv_j
       AND vbtyp_n IN rg_vbtyp_n.

* Documentos de NF de itens
    LOOP AT lt_vbfa.
      IF lt_vbfa-vbtyp_n = 'R'.
        CONCATENATE lt_vbfa-vbeln lt_vbfa-refkey INTO vl_refkey.
        lt_vbfa-refkey = vl_refkey.
      ELSE.
        lt_vbfa-refkey = lt_vbfa-vbeln.
      ENDIF.
      MODIFY lt_vbfa INDEX sy-tabix TRANSPORTING refkey.
    ENDLOOP.

    lw_range_2-sign   = cc_i.
    lw_range_2-option = cc_eq.
    lw_range_2-low    = cc_reftyp_md.
    APPEND lw_range_2 TO rg_reftyp.
    lw_range_2-low    = cc_reftyp_bi.
    APPEND lw_range_2 TO rg_reftyp.

    SELECT DISTINCT docnum
      INTO TABLE lt_nf
      FROM j_1bnflin
       FOR ALL ENTRIES IN lt_vbfa
     WHERE reftyp IN rg_reftyp
       AND refkey = lt_vbfa-refkey
       AND refitm = lt_vbfa-posnn.

  ELSEIF mt_vbfa_remessa-vbtyp_v EQ '7'.

    SELECT * INTO TABLE nf_aviso_receb
      FROM zmmt_ee_zgr_docs
       FOR ALL ENTRIES IN lt_rem
     WHERE av_vbeln EQ lt_rem-vbeln.

    IF sy-subrc IS INITIAL.
      DELETE nf_aviso_receb WHERE docnum EQ space.
      IF NOT nf_aviso_receb[] IS INITIAL.
        SELECT DISTINCT docnum INTO TABLE lt_nf
          FROM j_1bnflin
          FOR ALL ENTRIES IN nf_aviso_receb
         WHERE docnum EQ nf_aviso_receb-docnum.
      ENDIF.
    ENDIF.

  ENDIF.

  CHECK: NOT lt_nf[] IS INITIAL.

  SORT lt_nf.
  DELETE ADJACENT DUPLICATES FROM lt_nf.

  CLEAR vg_wa_docheader.

* Extrai valores de NF's
  LOOP AT lt_nf.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
      EXPORTING
        doc_number         = lt_nf-docnum
      IMPORTING
        doc_header         = vg_wa_docheader
      TABLES
        doc_partner        = ti_docpartner
        doc_item           = ti_docitem
        doc_item_tax       = ti_docitemtax
        doc_header_msg     = ti_docheadermsg
        doc_refer_msg      = ti_docrefermsg
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CHECK: NOT ti_docitem[] IS INITIAL
      AND  NOT ti_docitemtax[] IS INITIAL.

    CLEAR: vg_wa_ext_header.

    IF vg_wa_docheader-cancel IS INITIAL.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
        EXPORTING
          nf_header   = vg_wa_docheader
        IMPORTING
          ext_header  = vg_wa_ext_header
        TABLES
          nf_item     = ti_docitem
          nf_item_tax = ti_docitemtax
          ext_item    = ti_ext_item.

      LOOP AT ti_ext_item.
        lp_vlr_nf = lp_vlr_nf + ti_ext_item-nftot.
      ENDLOOP.

    ENDIF.
  ENDLOOP.
* Retorna
  p_vlr_nf = lp_vlr_nf.

ENDFORM.                    " OBTEM_VALOR_NF_401

*&---------------------------------------------------------------------*
*&      Form  DESMARCAR_LINHAS_TC_401
*&---------------------------------------------------------------------*
FORM desmarcar_linhas_tc_401 .

  ti_400_confer-mark = space.
  MODIFY ti_400_confer TRANSPORTING mark
                       WHERE mark GT space.

* Limpa o detalhamento
  CLEAR: sdetlh_confer_400.
  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

  IF vg_obs_edit_400 IS BOUND.

    REFRESH ti_400_obs.
    CALL METHOD vg_obs_edit_400->set_text_as_r3table
      EXPORTING
        table = ti_400_obs.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.

  ENDIF.

ENDFORM.                    " DESMARCAR_LINHAS_TC_401

*&---------------------------------------------------------------------*
*&      Form  ATLZ_STATUS_DOCTO_CONFER_401
*&---------------------------------------------------------------------*
FORM atlz_status_docto_confer_401 .

  DATA: wa_zlest0025 TYPE zlest0025.

  SORT ti_obj_contabil BY codtrp codposto lote chvid
                          conhec tiptransp ctlglancto.

  LOOP AT ti_400_confer.

    IF NOT ti_400_confer-acrescentado IS INITIAL.

*     Gera nova entrada para lançamento manual
      CLEAR zlest0016.
      zlest0016-transportador   = ti_400_confer-codtrp.
      zlest0016-posto           = ti_400_confer-codposto.
      zlest0016-lote            = ti_400_confer-lote.
      zlest0016-chvid           = ti_400_confer-chvid.
      zlest0016-ctafrete        = ti_400_confer-ctafrete.
      zlest0016-conhecimento    = ti_400_confer-conhec.
      zlest0016-vlr_confirmado  = ti_400_confer-valor_acresc.
      zlest0016-dta_chegada     = ti_400_confer-data_acresc.
      zlest0016-erdat           = sy-datum.
      zlest0016-uzeit           = sy-uzeit.
      zlest0016-uname           = sy-uname.
      MODIFY zlest0016 FROM zlest0016.

      COMMIT WORK. " CSB

    ENDIF.

*   Obtem documento contabil para ChvId
    READ TABLE ti_obj_contabil WITH KEY codtrp   = ti_400_confer-codtrp
                                        codposto = ti_400_confer-codposto
                                        lote     = ti_400_confer-lote
                                        chvid    = ti_400_confer-chvid
                                        conhec   = ti_400_confer-conhec
                               BINARY SEARCH.
    CHECK: sy-subrc IS INITIAL.
    vg_index = sy-tabix.

*   Grava documentos contábil gerado no lançamento
    DO.
      CLEAR zlest0022.
      MOVE: ti_400_confer-codtrp       TO zlest0022-transportador,
            ti_400_confer-codposto     TO zlest0022-posto,
            ti_400_confer-lote         TO zlest0022-lote,
            ti_400_confer-chvid        TO zlest0022-chvid,
            ti_400_confer-observacoes  TO zlest0022-obs_confer,
            ti_obj_contabil-tiptransp  TO zlest0022-tiptransp,
            ti_obj_contabil-ctlglancto TO zlest0022-ctlglancto,
            ti_obj_contabil-docsap     TO zlest0022-docsap,
            ti_obj_contabil-gjahr      TO zlest0022-gjahr,
            ti_obj_contabil-bukrs      TO zlest0022-bukrs,
            ti_obj_contabil-obj_key    TO zlest0022-obj_key,
            ti_400_confer-conhec       TO zlest0022-conhecimento,
            ti_400_confer-ctafrete     TO zlest0022-ctafrete,
            sy-datum                   TO zlest0022-erdat,
            sy-uname                   TO zlest0022-uname.

      IF ti_400_confer-acrescentado = cc_x.

        SELECT SINGLE * INTO wa_zlest0025
          FROM zlest0025
         WHERE chvid EQ ti_400_confer-chvid.

        ti_400_confer-ctlgchavid = wa_zlest0025-ctlgchavid.

*       Adiantamento para Acréscimo e/ou Decréscimo
        IF ti_400_confer-ctlgchavid = cc_chvid_admaior OR
           ti_400_confer-ctlgchavid = cc_chvid_admenor.
          zlest0022-acdctipo = cc_1_adacdc.
          zlest0022-acdcinfo = space.
        ENDIF.

      ENDIF.

      MODIFY zlest0022 FROM zlest0022.

      COMMIT WORK. " CSB

*     Lê o próximo lançamento para mesmo histórico
      ADD 1 TO vg_index.
      READ TABLE ti_obj_contabil INDEX vg_index.
      IF sy-subrc <> 0
         OR ti_obj_contabil-codtrp   <> ti_400_confer-codtrp
         OR ti_obj_contabil-codposto <> ti_400_confer-codposto
         OR ti_obj_contabil-lote     <> ti_400_confer-lote
         OR ti_obj_contabil-chvid    <> ti_400_confer-chvid
         OR ti_obj_contabil-conhec   <> ti_400_confer-conhec.
        EXIT.
      ENDIF.

    ENDDO.

  ENDLOOP.

* Atualiza status do documento
  UPDATE zlest0015
      SET status = cc_confirmado
          erdat  = sy-datum
          uzeit  = sy-uzeit
          uname  = sy-uname
          datafechamento = sheader_300-data_fechamento
    WHERE transportador = ti_400_confer-codtrp
      AND posto         = ti_400_confer-codposto
      AND lote          = ti_400_confer-lote.

  COMMIT WORK.
  WAIT UP TO 7 SECONDS.


* Elimina o registro feito a conferência
  DELETE ti_203_lotes WHERE codtrp   = ti_400_confer-codtrp
                        AND codposto = ti_400_confer-codposto
                        AND lote     = ti_400_confer-lote.

* Reseta valores de conferência
  CLEAR: sdetlh_confer_400,
         sctrl_saldo_400.

ENDFORM.                    " ATLZ_STATUS_DOCTO_CONFER_401

*&---------------------------------------------------------------------*
*&      Form  EDITAR_ENTRADAS_DOCTO_401
*&---------------------------------------------------------------------*
FORM editar_entradas_docto_401 .

  READ TABLE ti_400_confer WITH KEY mark = cc_x.
  CHECK: sy-subrc IS INITIAL.

  IF ti_400_confer-acrescentado IS INITIAL.
    vg_msgerro = TEXT-m20.
    MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
    EXIT.
  ENDIF.

  CLEAR: snlancto_confer_400.

* Move dados da linha selecionada para área de lançamento manual
  MOVE: ti_400_confer-chvid        TO snlancto_confer_400-chvid,
        ti_400_confer-deschvid     TO snlancto_confer_400-deschvid,
        ti_400_confer-data_acresc  TO snlancto_confer_400-data,
        ti_400_confer-valor_acresc TO snlancto_confer_400-valor.

  IF ti_400_confer-observacoes IS INITIAL.
    CLEAR: snlancto_confer_400-observ1,
           snlancto_confer_400-observ2,
           snlancto_confer_400-observ3.
  ELSE.
    MOVE: ti_400_confer-observacoes(85)
                                  TO snlancto_confer_400-observ1,
          ti_400_confer-observacoes+85(85)
                                  TO snlancto_confer_400-observ2,
          ti_400_confer-observacoes+170
                                  TO snlancto_confer_400-observ3.
  ENDIF.

* Atualiza o novo valor para saldo
  sctrl_saldo_400-saldo_hist = sctrl_saldo_400-saldo_atual.
  sctrl_saldo_400-edit_lanc  = cc_x.
  IF sctrl_saldo_400-hist_negat IS INITIAL.
    sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                           + ti_400_confer-valor_acresc.
  ELSE.
    sctrl_saldo_400-saldo_atual = sctrl_saldo_400-saldo_atual
                           - ti_400_confer-valor_acresc.
  ENDIF.
  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

* Exibe tela de entradas manuais
  CALL SCREEN 0402
       STARTING AT 07 05
         ENDING AT 104 21.

* Limpa valores após o retorno da tela de lançamento
  CLEAR: sctrl_saldo_400-saldo_hist,
         sctrl_saldo_400-edit_lanc.

* Exibe detalhe da entrada editada
  PERFORM exibe_detalhe_docto_401.

ENDFORM.                    " EDITAR_ENTRADAS_DOCTO_401

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TRANSACAO_VT03N
*&---------------------------------------------------------------------*
FORM chama_transacao_vt03n  USING  p_codtrp
                                   p_conhec
                                   p_ctafrete.
* Cabeçalho transporte
  SELECT tknum
    INTO vttk-tknum
    FROM vttk
      UP TO 1 ROWS
   WHERE tdlnr = p_codtrp
     AND exti1 = p_conhec
     AND exti2 = p_ctafrete.
  ENDSELECT.

  CHECK: sy-subrc IS INITIAL.

  SET PARAMETER ID: 'TNR' FIELD vttk-tknum.

* Visualiza dados de transporte
  CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

ENDFORM.                    " CHAMA_TRANSACAO_VT03N

*&---------------------------------------------------------------------*
*&      Form  EXIBE_MATCH_CODE_DOMINIO
*&---------------------------------------------------------------------*
FORM exibe_match_code_dominio  USING VALUE(p_dominio)
                            CHANGING p_selecao.

  TYPES: BEGIN OF yh_dominio,
           valor TYPE val_single,
           descr TYPE val_text,
         END  OF yh_dominio.

  DATA: lt_h_dominio TYPE STANDARD TABLE OF yh_dominio INITIAL SIZE 0,
        lt_h_return  TYPE STANDARD TABLE OF ddshretval INITIAL SIZE 0,
        lt_h_dd07v   TYPE STANDARD TABLE OF dd07v      INITIAL SIZE 0,
        lw_dominio   TYPE yh_dominio,
        lwa_return   TYPE ddshretval.

  CLEAR: p_selecao.
  vg_dd_name = p_dominio.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = vg_dd_name
      text           = cc_x
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_h_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  CHECK: NOT lt_h_dd07v[] IS INITIAL.

  LOOP AT lt_h_dd07v INTO vg_wa_dd07v.
    lw_dominio-valor = vg_wa_dd07v-domvalue_l.
    lw_dominio-descr = vg_wa_dd07v-ddtext.
    APPEND lw_dominio TO lt_h_dominio.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VALOR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = cc_s
    TABLES
      value_tab       = lt_h_dominio
      return_tab      = lt_h_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK: sy-subrc IS INITIAL.

  READ TABLE lt_h_return INTO lwa_return INDEX 1.
  p_selecao = lwa_return-fieldval.

ENDFORM.                    " EXIBE_MATCH_CODE_DOMINIO

*&---------------------------------------------------------------------*
*&      Form  NOVO_CALCULO_PESO_CONFIRMADO
*&---------------------------------------------------------------------*
FORM novo_calculo_peso_confirmado .

  CLEAR ti_400_confer.
  READ TABLE ti_400_confer WITH KEY mark = cc_x.

* Garante que o registro do Header é o mesmo da conferência
  CHECK: ti_400_confer-codtrp   = ti_203_lotes-codtrp
    AND  ti_400_confer-codposto = ti_203_lotes-codposto
    AND  ti_400_confer-lote     = ti_203_lotes-lote.

* Garante registro na área de detalhe
  CHECK: sdetlh_confer_400-peso_conferido > 0
    AND  sdetlh_confer_400-lctochvid = cc_a
    AND  ti_400_confer-mark = cc_x
    AND  ti_400_confer-status = cc_a_confirmar
    AND  ti_400_confer-acrescentado = space.

* Limpa área de detalhamento
  CLEAR: vg_wa_lancto,
         vg_wa_deltas,
         stela_403.

* Busca dados de lançamento
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_400_confer-codtrp
                                    codposto = ti_400_confer-codposto
                                    lote     = ti_400_confer-lote
                                  ctafrete   = ti_400_confer-ctafrete
                                    conhec   = ti_400_confer-conhec
                                    chvid    = ti_400_confer-chvid
                                    datalote = ti_203_lotes-datalote
                           BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.
  stela_403-idx_lancto = sy-tabix.

* Busca dados valores delta
  READ TABLE ti_cockpit_deltas INTO vg_wa_deltas
                           WITH KEY codtrp = vg_wa_lancto-codtrp
                                  codposto = vg_wa_lancto-codposto
                                      lote = vg_wa_lancto-lote
                                  ctafrete = vg_wa_lancto-ctafrete
                                    conhec = vg_wa_lancto-conhec
                                     chvid = vg_wa_lancto-chvid
                                  datalote = vg_wa_lancto-datalote
                           BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.
  stela_403-idx_delta = sy-tabix.

  SELECT SINGLE msehl
    INTO stela_403-desc_unid
    FROM t006a
   WHERE spras = sy-langu
     AND msehi = vg_wa_lancto-unid_peso.

  IF vg_wa_lancto-peso_confiralter IS INITIAL.
    stela_403-peso_conferido = vg_wa_lancto-peso_confirmado.
  ELSE.
    stela_403-peso_conferido = vg_wa_lancto-peso_confiralter.
  ENDIF.
  stela_403-unid_peso      = vg_wa_lancto-unid_peso.
  stela_403-unid_alter     = stela_403-unid_peso.
  stela_403-desc_ualter    = stela_403-desc_unid.

* Exibe tela para alteração de peso conferido
  CALL SCREEN 403 STARTING AT 12  6
                    ENDING AT 80 16.

ENDFORM.                    " NOVO_CALCULO_PESO_CONFIRMADO

*&---------------------------------------------------------------------*
*&      Form  ALTERA_PESO_CONFIRMADO_COCKPIT
*&---------------------------------------------------------------------*
FORM altera_peso_confirmado_cockpit .

  DATA: lp_vlr_programado TYPE kwert,
        lp_vlr_confirmado TYPE kwert,
        lp_vlr_diferenca  TYPE kwert,
        lp_adiantamento   TYPE kwert.

  IF stela_403-peso_alter IS INITIAL.
    MESSAGE TEXT-m51 TYPE cc_s DISPLAY LIKE cc_w.
    EXIT.
  ENDIF.

* Refaz calculo do cockpit para novo peso confirmado
  PERFORM yf_cockpit_calculo_deltas IN PROGRAM saplzles0002 IF FOUND
                                 USING stela_403-peso_alter
                                       vg_wa_lancto-peso_origem
                                       vg_wa_lancto-unid_peso
                                       vg_wa_deltas-grpconta
                                       vg_wa_deltas-tolerancia
                                       sdetlh_confer_400-vlr_nf
                                       vg_wa_deltas-vlradiantamento
                                       vg_wa_deltas-vlrimp_retidos
                                       vg_wa_deltas-vlrfrete
                                       vg_wa_deltas-vlrseguro
                                       vg_wa_lancto-vlrimportado
                              CHANGING lp_vlr_confirmado
                                       lp_vlr_diferenca
                                       lp_vlr_programado
                                       vg_wa_deltas-vlconfirmado_ref
                                       vg_wa_deltas-vlrperda
                                       vg_wa_deltas-vlrsobra_quebra
                                       vg_wa_deltas-difer_transp
                                       vg_wa_deltas-diferenca_peso
                                       vg_wa_deltas-quebra_peso
                                       vg_wa_deltas-quebra_real
                                       vg_wa_deltas-fator_conversao.

* Salva novos valores alterados no lançamento
  vg_wa_lancto-peso_confiralter = stela_403-peso_alter.
  vg_wa_lancto-vlrconfiralter   = lp_vlr_confirmado.

* Atualiza base de lançamentos
  UPDATE zlest0016
     SET peso_confiralter = vg_wa_lancto-peso_confiralter
         vlr_confiralter  = vg_wa_lancto-vlrconfiralter
   WHERE transportador    = ti_400_confer-codtrp
     AND posto            = ti_400_confer-codposto
     AND lote             = ti_400_confer-lote
     AND chvid            = ti_400_confer-chvid
     AND conhecimento     = ti_400_confer-conhec.

* Atualiza base de calculo cokpit - Valores Deltas
  UPDATE zlest0020
     SET quebra_real      = vg_wa_deltas-quebra_real
         diferenca_peso   = vg_wa_deltas-diferenca_peso
         quebra_peso      = vg_wa_deltas-quebra_peso
         fator_conversao  = vg_wa_deltas-fator_conversao
         difer_transp     = vg_wa_deltas-difer_transp
         vlconfirmado_ref = vg_wa_deltas-vlconfirmado_ref
         vlrperda         = vg_wa_deltas-vlrperda
         vlrsobra_quebra  = vg_wa_deltas-vlrsobra_quebra
         erdat            = sy-datum
         uname            = sy-uname
   WHERE transportador    = ti_400_confer-codtrp
     AND posto            = ti_400_confer-codposto
     AND lote             = ti_400_confer-lote
     AND chvid            = ti_400_confer-chvid
     AND conhecimento     = ti_400_confer-conhec.

* Atualiza valores lançamento em memória
  MODIFY ti_cockpit_lancto INDEX stela_403-idx_lancto
                            FROM vg_wa_lancto
                    TRANSPORTING peso_confiralter
                                 vlrconfiralter.

* Atualiza valores deltas em memória
  MODIFY ti_cockpit_deltas INDEX stela_403-idx_delta
                            FROM vg_wa_deltas
                    TRANSPORTING vlconfirmado_ref
                                 vlrperda
                                 vlrsobra_quebra
                                 difer_transp
                                 diferenca_peso
                                 quebra_peso
                                 quebra_real
                                 fator_conversao.

* Atualiza área da tela de detalhamento
  MOVE:
    vg_wa_lancto-peso_confiralter TO sdetlh_confer_400-peso_conferido,
    vg_wa_lancto-vlrconfiralter   TO sdetlh_confer_400-vlr_conferido,
*   lp_vlr_programado             TO sdetlh_confer_400-vlr_progr,
    vg_wa_deltas-quebra_real      TO sdetlh_confer_400-peso_qbr_tolerav,
    vg_wa_deltas-diferenca_peso   TO sdetlh_confer_400-peso_diferenca,
    vg_wa_deltas-quebra_peso      TO sdetlh_confer_400-peso_perda,
    vg_wa_deltas-vlrperda         TO sdetlh_confer_400-vlr_perda,
*   vg_wa_deltas-vlrsobra_quebra  TO sdetlh_confer_400-vlr_sobra_quebra,
    vg_wa_deltas-difer_transp     TO sdetlh_confer_400-vlr_sobra_quebra.

* Atualiza valores de histórico
  READ TABLE ti_cockpit_lancto INTO vg_wa_lancto
                           WITH KEY codtrp   = ti_400_confer-codtrp
                                    codposto = ti_400_confer-codposto
                                    lote     = ti_400_confer-lote
  BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.
  vg_index = sy-tabix.

  CLEAR: lp_vlr_confirmado.
  DO.
*   Calcula o valor do saldo (Valor conferido / confirmado)
    IF vg_wa_lancto-vlrconfiralter IS INITIAL.
      IF  vg_wa_lancto-vlrconfirmado IS INITIAL.
        ADD  vg_wa_lancto-vlrimportado  TO lp_vlr_confirmado.
      ELSE.
        ADD  vg_wa_lancto-vlrconfirmado TO lp_vlr_confirmado.
      ENDIF.
    ELSE.
      ADD  vg_wa_lancto-vlrconfiralter  TO lp_vlr_confirmado.
    ENDIF.
*   Lê o próximo registro
    ADD 1 TO vg_index.
    READ TABLE ti_cockpit_lancto INTO vg_wa_lancto INDEX vg_index.
    IF sy-subrc               <> 0                     OR
       ti_400_confer-codtrp   <> vg_wa_lancto-codtrp   OR
       ti_400_confer-codposto <> vg_wa_lancto-codposto OR
       ti_400_confer-lote     <> vg_wa_lancto-lote.
      EXIT.
    ENDIF.
  ENDDO.

* Obtem o valor de adiantamento

* ---> S4 Migration - 14/06/2023 - MA
*  select WRBTR
*    into LP_ADIANTAMENTO
*    from BSEG
*      up to 1 rows
*   where BUKRS = TI_203_LOTES-BUKRS
*     and BELNR = TI_203_LOTES-DOCSAPADTO
*     and GJAHR = TI_203_LOTES-GJAHR.
*  endselect.

  CALL FUNCTION 'FAGL_GET_BSEG'
    EXPORTING
      i_bukrs   = ti_203_lotes-bukrs
      i_belnr   = ti_203_lotes-docsapadto
      i_gjahr   = ti_203_lotes-gjahr
    IMPORTING
      et_bseg   = lt_bseg
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  READ TABLE lt_bseg INTO ls_bseg INDEX 1.

  IF sy-subrc = 0.
    lp_adiantamento = CONV #( ls_bseg-wrbtr )  .
  ENDIF.
*<--- S4 Migration - 14/06/2023 - MA

* Valor de Saldo
  sctrl_saldo_400-saldo_atual = lp_adiantamento - lp_vlr_confirmado.
  sdetlh_confer_400-saldo_historico = sctrl_saldo_400-saldo_atual.

  IF sctrl_saldo_400-saldo_atual < 0.
    sctrl_saldo_400-hist_negat = cc_x.
  ENDIF.

ENDFORM.                    " ALTERA_PESO_CONFIRMADO_COCKPIT

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DECIMAL
*&---------------------------------------------------------------------*
FORM converte_char_decimal  USING    p_valor
                            CHANGING p_valor_dec.

  DATA: lc_valor TYPE char30.

  CLEAR p_valor_dec.
  lc_valor = p_valor.

  CHECK: NOT lc_valor IS INITIAL.
  REPLACE ALL OCCURRENCES OF REGEX '\.' IN lc_valor WITH ''.

  FIND FIRST OCCURRENCE OF REGEX '[^0-9,\s]' IN lc_valor.
  CHECK: NOT sy-subrc IS INITIAL
     AND NOT lc_valor IS INITIAL.

  CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
    EXPORTING
      i_char         = lc_valor
    IMPORTING
*     E_FLOAT        =
      e_packed       = p_valor_dec
    EXCEPTIONS
      invalid_number = 1
      OTHERS         = 2.

ENDFORM.                    " CONVERTE_CHAR_DECIMAL

*&---------------------------------------------------------------------*
*&      Form  INCLUI_LANCTO_MANUAL_BASE
*&---------------------------------------------------------------------*
FORM inclui_lancto_manual_base .

  CLEAR: vg_wa_lancto,        "Área de inclusão
          sheader_300.        "Força refazer a tela no lançamento

  vg_wa_lancto-codtrp         = vg_wa_400_confer-codtrp.
  vg_wa_lancto-codposto       = vg_wa_400_confer-codposto.
  vg_wa_lancto-lote           = vg_wa_400_confer-lote.
  vg_wa_lancto-datalote       = ti_203_lotes-datalote.
  vg_wa_lancto-chvid          = vg_wa_400_confer-chvid.
  vg_wa_lancto-conhec         = vg_wa_400_confer-conhec.
  vg_wa_lancto-ctafrete       = vg_wa_400_confer-ctafrete.
  vg_wa_lancto-deschvid       = vg_wa_400_confer-deschvid.
  vg_wa_lancto-lctochvid      = cc_m.
  vg_wa_lancto-observacoes    = vg_wa_400_confer-observacoes.
  vg_wa_lancto-vlrconfirmado  = vg_wa_400_confer-valor_acresc.
  vg_wa_lancto-dtacheg        = vg_wa_400_confer-data_acresc.

* Inclui ou atualiza registro na tabela interna de consulta
  READ TABLE ti_cockpit_lancto WITH KEY codtrp = vg_wa_lancto-codtrp
                                      codposto = vg_wa_lancto-codposto
                                          lote = vg_wa_lancto-lote
                                      ctafrete = vg_wa_lancto-ctafrete
                                        conhec = vg_wa_lancto-conhec
                                         chvid = vg_wa_lancto-chvid
                                      datalote = vg_wa_lancto-datalote
                           TRANSPORTING NO FIELDS
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MODIFY ti_cockpit_lancto FROM vg_wa_lancto INDEX sy-tabix.
  ELSE.
    APPEND vg_wa_lancto TO ti_cockpit_lancto.
    SORT   ti_cockpit_lancto BY codtrp codposto lote ctafrete conhec
                                chvid datalote.
  ENDIF.

* Inclui ou atualiza base de dados
  CLEAR zlest0016.
  zlest0016-transportador    = vg_wa_lancto-codtrp.
  zlest0016-posto            = vg_wa_lancto-codposto.
  zlest0016-lote             = vg_wa_lancto-lote.
  zlest0016-chvid            = vg_wa_lancto-chvid.
  zlest0016-ctafrete         = vg_wa_lancto-ctafrete.
  zlest0016-conhecimento     = vg_wa_lancto-conhec.
  zlest0016-peso_origem      = vg_wa_lancto-peso_origem.
  zlest0016-peso_importado   = vg_wa_lancto-peso_importado.
  zlest0016-peso_confirmado  = vg_wa_lancto-peso_confirmado.
  zlest0016-unidade_peso     = vg_wa_lancto-unid_peso.
  zlest0016-vlr_origem       = vg_wa_lancto-vlrorigem.
  zlest0016-vlr_importado    = vg_wa_lancto-vlrimportado.
  zlest0016-vlr_confirmado   = vg_wa_lancto-vlrconfirmado.
  zlest0016-diferenca        = vg_wa_lancto-vlrdiferenca.
  zlest0016-peso_confiralter = 0.
  zlest0016-vlr_confiralter  = 0.
  zlest0016-vlr_programado   = vg_wa_lancto-vlrprogramado.
  zlest0016-dta_chegada      = vg_wa_lancto-dtacheg.
  zlest0016-observacoes      = vg_wa_lancto-observacoes.
  zlest0016-erdat            = sy-datum.
  zlest0016-uzeit            = sy-uzeit.
  zlest0016-uname            = sy-uname.
  MODIFY zlest0016 FROM zlest0016.

ENDFORM.                    " INCLUI_LANCTO_MANUAL_BASE

*&---------------------------------------------------------------------*
*&      Form  CHECK_CONTARAZAO_EXIGE_CCUSTO
*&---------------------------------------------------------------------*
FORM check_contarazao_exige_ccusto USING  p_conta_razao
                                          p_tipo_conta
                                          p_chave_lanc
                                CHANGING  p_exige_ccusto.

* ---> S4 Migration - 17/07/2023 - CA
  DATA: lv_keydate         TYPE bapi1030_gen-some_date,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist.
* <--- S4 Migration - 17/07/2023 - CA

  STATICS: lqbr_contadebito  TYPE saknr,
           lqbr_contacredito TYPE saknr,
           lf_ccusto_debito,
           lf_ccusto_credito.

  FIELD-SYMBOLS: <conta-razao>  TYPE c,
                 <exige_ccusto> TYPE c.

* Limpa retorno
  CLEAR: p_exige_ccusto.

  CHECK p_chave_lanc NE '21' AND p_chave_lanc NE '31' AND p_chave_lanc NE '29' AND p_chave_lanc NE '39'.

* Verifica quebra de conta de débito ou credito
  IF p_tipo_conta = cc_d.
    IF lqbr_contadebito <> p_conta_razao.
      lqbr_contadebito = p_conta_razao.
      CLEAR lf_ccusto_debito.
      ASSIGN: lqbr_contadebito TO <conta-razao>,
              lf_ccusto_debito TO <exige_ccusto>.
    ENDIF.
  ELSEIF p_tipo_conta = cc_c.

    IF lqbr_contacredito <> p_conta_razao.
      lqbr_contacredito = p_conta_razao.
      CLEAR lf_ccusto_credito.
      ASSIGN: lqbr_contacredito TO <conta-razao>,
              lf_ccusto_credito TO <exige_ccusto>.
    ENDIF.
  ENDIF.

* Verifica acesso a base de dados
  IF <conta-razao> IS ASSIGNED AND <exige_ccusto> IS ASSIGNED.
* ---> S4 Migration - 17/07/2023 - CA
*    SELECT COUNT( * )
*      FROM cskb
*     WHERE kokrs = cc_kokrs_magi
*       AND kstar = <conta-razao>
*       AND datbi >= sy-datum.

    lv_keydate         = sy-datum.
    lv_controllingarea = cc_kokrs_magi.
    lv_costelement     = <conta-razao>.

    CLEAR: lt_returns[], ls_coeldes.

    CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
      EXPORTING
        controllingarea   = lv_controllingarea
        costelement       = lv_costelement
        keydate           = lv_keydate
      IMPORTING
        costelementdetail = ls_coeldes
      TABLES
        return            = lt_returns.

    READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc IS NOT INITIAL.
      <exige_ccusto> = cc_x.
    ELSE.
      CLEAR <exige_ccusto>.
    ENDIF.
*    IF sy-subrc IS INITIAL.
*      <exige_ccusto> = cc_x.
*    ELSE.
*      CLEAR <exige_ccusto>.
*    ENDIF.
* <--- S4 Migration - 17/07/2023 - CA
  ENDIF.

* Retorna condição se a conta razão exige centro de custo
  IF p_tipo_conta = cc_d.
    p_exige_ccusto = lf_ccusto_debito.
  ELSEIF p_tipo_conta = cc_c.
    p_exige_ccusto = lf_ccusto_credito.
  ENDIF.

ENDFORM.                    " CHECK_CONTARAZAO_EXIGE_CCUSTO

*&---------------------------------------------------------------------*
*&      Form  OBTEM_CENTRO_CUSTO_CONTARAZAO
*&---------------------------------------------------------------------*
FORM obtem_centro_custo_contarazao USING  p_codtrp
                                          p_tipo_conta
                                          p_conta_razao
                                CHANGING  p_kostl.

  STATICS: lqbr_codtrp       TYPE zcodtrp,
           lqbr_contadebito  TYPE saknr,
           lqbr_contacredito TYPE saknr,
           lc_bukrs          TYPE bukrs,
           lc_gsber          TYPE gsber,
           lc_kostl_debito   TYPE kostl,
           lc_kostl_credito  TYPE kostl.

  DATA:    lc_saknr             TYPE saknr.
  FIELD-SYMBOLS: <conta-razao>  TYPE c,
                 <centro-custo> TYPE c.
* Limpa retorno
  CLEAR p_kostl.

* Verifica quebra da transportadora
  IF  p_codtrp <> lqbr_codtrp.
    CLEAR: lqbr_contadebito,
           lqbr_contacredito,
           lc_bukrs,
           lc_gsber,
           lc_kostl_debito,
           lc_kostl_credito.
    lqbr_codtrp = p_codtrp.
    lc_gsber = p_codtrp+6.
    SELECT bukrs
      INTO lc_bukrs
      FROM j_1bbranch
        UP TO 1 ROWS
     WHERE branch = lc_gsber.
    ENDSELECT.
  ENDIF.

* Verifica quebra de conta de débito / crédito
  IF p_tipo_conta = cc_d.
    IF lqbr_contadebito <> p_conta_razao.
      lqbr_contadebito = p_conta_razao.
      ASSIGN: lqbr_contadebito TO <conta-razao>,
              lc_kostl_debito  TO <centro-custo>.
    ENDIF.
  ELSEIF p_tipo_conta = cc_c.
    IF lqbr_contacredito <> p_conta_razao.
      lqbr_contacredito = p_conta_razao.
      ASSIGN: lqbr_contacredito TO <conta-razao>,
              lc_kostl_credito  TO <centro-custo>.
    ENDIF.
  ENDIF.

* Verifica acesso a base de dados
  IF <conta-razao> IS ASSIGNED AND <centro-custo> IS ASSIGNED.
    SELECT MAX( saknr ) kostl
      INTO (lc_saknr, <centro-custo>)
      FROM zlest0033
     WHERE bukrs = lc_bukrs
       AND gsber = lc_gsber
       AND (  saknr = <conta-razao> OR saknr = space )
     GROUP BY saknr kostl.
    ENDSELECT.
  ENDIF.

* Retorna o centro de custo se localizado
  IF p_tipo_conta = cc_d.
    p_kostl = lc_kostl_debito.
  ELSEIF p_tipo_conta = cc_c.
    p_kostl = lc_kostl_credito.
  ENDIF.
  IF <conta-razao> IS ASSIGNED.
    UNASSIGN <conta-razao>.
  ENDIF.
  IF <centro-custo> IS ASSIGNED.
    UNASSIGN <centro-custo>.
  ENDIF.
ENDFORM.                    " OBTEM_CENTRO_CUSTO_CONTARAZAO
*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJAUT_AUTHORIZACAO
*&---------------------------------------------------------------------*
FORM check_objaut_authorizacao USING p_valor_objeto
                            CHANGING p_tem_autoriz.

  CLEAR p_tem_autoriz.

  AUTHORITY-CHECK OBJECT 'ZLESOBJAUT'
                      ID 'Z_AUTH_LES' FIELD  p_valor_objeto.

  IF sy-subrc IS INITIAL.
    p_tem_autoriz = cc_x.
  ENDIF.

ENDFORM.                    " CHECK_OBJAUT_AUTHORIZACAO

*&---------------------------------------------------------------------*
*&      Form  EXCLUI_LOTE_STATUS_I_203
*&---------------------------------------------------------------------*
FORM exclui_lote_status_i_203 .

  CLEAR: vg_msgerro,
         vg_resposta.

* Verifica condição para eliminação do lote
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    IF ti_203_lotes-status <> cc_importado.
      WRITE vg_index TO vg_campo_char05 NO-ZERO.
      CONCATENATE 'Linha nº' vg_campo_char05 '-' TEXT-m23
             INTO vg_msgerro SEPARATED BY space.
      REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
               WITH ti_203_lotes-status.
    ENDIF.

*   Verifica inconsistências
    IF NOT vg_msgerro IS INITIAL.
      MESSAGE vg_msgerro TYPE cc_s DISPLAY LIKE cc_w.
      EXIT.
    ENDIF.

  ENDLOOP.

  CHECK: vg_msgerro IS INITIAL.

* Exibe mensagem de confirmação...
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-m30
      text_question         = TEXT-m31
      text_button_1         = TEXT-m32
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-m33
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
    IMPORTING
      answer                = vg_resposta
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK vg_resposta = '1'.

* Elimina registros selecionados
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    vg_index = sy-tabix.

    DELETE FROM zlest0013
      WHERE codtrp   = ti_203_lotes-codtrp
        AND codposto = ti_203_lotes-codposto
        AND lote     = ti_203_lotes-lote.

    DELETE ti_203_lotes INDEX vg_index.

  ENDLOOP.

  COMMIT WORK.

* Atualiza tela
  IF ti_203_lotes[] IS INITIAL.
*   Retorna para nova consulta
    PERFORM renova_tela_para_pesquisa.
    PERFORM reseta_vlrsalvo_selecao_1200.
  ELSE.
*   Renova Grid
    DESCRIBE TABLE ti_203_lotes LINES vg_sbs203_tabcontrol-lines.
  ENDIF.

ENDFORM.                    " EXCLUI_LOTE_STATUS_I_203

*&---------------------------------------------------------------------*
*&      Form  CHECK_ACRESIMO_DECRESCIMO
*&---------------------------------------------------------------------*
FORM check_acresimo_decrescimo  CHANGING  p_resposta.

  CLEAR p_resposta.

* Exibe tela para acréscimo e/ou decréscimo
  CALL SCREEN 204 STARTING AT 12  2
                    ENDING AT 155 27.

ENDFORM.                    " CHECK_ACRESIMO_DECRESCIMO

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_VALORES_ACAO_ACRDESC
*&---------------------------------------------------------------------*
FORM preenche_valores_acao_acrdesc .

  DATA: lc_name_id TYPE   vrm_id,
        lt_list    TYPE   vrm_values,
        lw_value   LIKE   LINE OF lt_list.

  REFRESH lt_list.
  CLEAR lw_value.

  lw_value-key = '1'.
  lw_value-text = 'Aplicar'.
  APPEND lw_value TO lt_list.

  lw_value-key = '2'.
  lw_value-text = 'Rejeitar'.
  APPEND lw_value TO lt_list.

  lw_value-key = ' '.
  lw_value-text = space.
  APPEND lw_value TO lt_list.

  lc_name_id = 'TI_204H_ACRDECR-ACAO'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lc_name_id
      values = lt_list.

ENDFORM.                    " PREENCHE_VALORES_ACAO_ACRDESC

*&---------------------------------------------------------------------*
*&      Form  ATLZ_ACDC_CONFER_ZLEST0022
*&---------------------------------------------------------------------*
FORM atlz_acdc_confer_zlest0022 .

  DATA: lc_acdctipo TYPE zacdctipo,
        lc_acdcinfo TYPE char10.

  CHECK: NOT ti_204h_acrdecr[] IS INITIAL.

  LOOP AT ti_204h_acrdecr.

    IF ti_204h_acrdecr-acao = cc_1_adacdc.
      lc_acdctipo = cc_3_adacdc. "ACDC - Aplicado
      lc_acdcinfo = ti_204h_acrdecr-lote.
    ELSEIF ti_204h_acrdecr-acao = cc_2_adacdc.
      lc_acdctipo = cc_2_adacdc. "ACDC - Rejeitado
      lc_acdcinfo = sy-uname.
    ELSE.
      CONTINUE.                  "ACDC - Não alterado
    ENDIF.

    LOOP AT ti_204d_acrdecr WHERE codtrp       EQ ti_204h_acrdecr-codtrp
                              AND codposto     EQ ti_204h_acrdecr-codposto
                              AND lote_aplicar EQ ti_204h_acrdecr-lote.
      UPDATE zlest0022
         SET acdctipo      = lc_acdctipo
             acdcinfo      = lc_acdcinfo
             lote_aplicado = ti_204d_acrdecr-lote_aplicar
       WHERE transportador = ti_204d_acrdecr-codtrp
         AND posto         = ti_204d_acrdecr-codposto
         AND lote          = ti_204d_acrdecr-lote
         AND acdctipo      = cc_1_adacdc
         AND chvid         = ti_204d_acrdecr-chvid " CSB conforme foi solicitado
         AND conhecimento  = ti_204d_acrdecr-conhecimento " CSB conforme foi solicitado
         AND ctlglancto   IN (cc_ctlg_vlrconfer, cc_ctlg_vlrprogdo).
    ENDLOOP.

  ENDLOOP.

  REFRESH ti_204h_acrdecr.

ENDFORM.                    " ATLZ_ACDC_CONFER_ZLEST0022
*&---------------------------------------------------------------------*
*&      Form  REENVIO_DOC_SAP
*&---------------------------------------------------------------------*
FORM reenvio_doc_sap USING p_obj_key
                           p_index.

  DATA: vl_confer   TYPE zles_cockpit_confer,
        it_contabil TYPE TABLE OF zib_contabil INITIAL SIZE 0 WITH HEADER LINE,
        wa_contabil TYPE zib_contabil.

  DELETE FROM zib_contabil_err
   WHERE obj_key = p_obj_key.

  SELECT * INTO TABLE it_contabil
    FROM zib_contabil
   WHERE obj_key EQ p_obj_key.

  LOOP AT it_contabil INTO wa_contabil.

    CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO wa_contabil-bldat.
    wa_contabil-budat = wa_contabil-bldat.
    wa_contabil-gjahr = wa_contabil-bldat+6(4).
    wa_contabil-monat = wa_contabil-bldat+3(2).
    wa_contabil-zfbdt = wa_contabil-bldat.

    wa_contabil-rg_atualizado = 'N'.
    MODIFY zib_contabil FROM wa_contabil.
  ENDLOOP.

  READ TABLE ti_cockpit_confer INDEX p_index INTO vl_confer.
  IF sy-subrc = 0.
    vl_confer-err_msg = space.
    MODIFY ti_cockpit_confer INDEX p_index FROM vl_confer
      TRANSPORTING err_msg.
  ENDIF.
  COMMIT WORK.

ENDFORM.                    " REENVIO_DOC_SAP

INCLUDE ole2incl.
* handles for OLE objects

DATA: h_excel TYPE ole2_object,        " Excel object
      h_mapl  TYPE ole2_object,         " list of workbooks
      h_map   TYPE ole2_object,          " workbook
      h_zl    TYPE ole2_object,           " cell
      h_f     TYPE ole2_object.            " font
*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_excel .

  IF ti_203_lotes IS INITIAL.
    MESSAGE w000(z01) WITH 'Não ha dados para Gerar EXCEL !' .
    CHECK NOT ti_203_lotes IS INITIAL.
    "    stop.
  ENDIF.

  DATA  h TYPE i.

* start Excel
  CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
*  PERFORM ERR_HDL.

  SET PROPERTY OF h_excel  'Visible' = 1.

* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = TEXT-008
    EXCEPTIONS
      OTHERS = 1.

* get list of workbooks, initially empty
  CALL METHOD OF h_excel 'Workbooks' = h_mapl.
  PERFORM err_hdl.
* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map.
  PERFORM err_hdl.
* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE = 0
      text   = TEXT-009
    EXCEPTIONS
      OTHERS = 1.

** output column headings to active Excel sheet
*  PERFORM fill_cell USING 1 1  1 'Transportador'(001).
*  PERFORM fill_cell USING 1 2  1 'Descrição Transp.'(002).
*  PERFORM fill_cell USING 1 3  1 'Posto'(003).
*  PERFORM fill_cell USING 1 4  1 'Descrição Posto'(004).
*  PERFORM fill_cell USING 1 5  1 'Lote'(005).
*  PERFORM fill_cell USING 1 6  1 'Docto SAP'(006).
*  PERFORM fill_cell USING 1 7  1 'BL'(007).
*  PERFORM fill_cell USING 1 8  1 'Data'(008).
*  PERFORM fill_cell USING 1 9  1 'Vencimento'(009).
*  PERFORM fill_cell USING 1 10 1 'Vlr Importado'(010).
*  PERFORM fill_cell USING 1 11 1 'Vlr Confirmado'(011).
*  PERFORM fill_cell USING 1 12 1 'VLR RECUSADO'(012).
*  PERFORM fill_cell USING 1 13 1 'Vlr Realizado'(013).
*  PERFORM fill_cell USING 1 14 1 'A/D'(014).
*  PERFORM fill_cell USING 1 15 1 'Dt Fechamento'(015).
*
*  LOOP AT ti_203_lotes.
*    h = sy-tabix + 1.
*    PERFORM fill_cell USING h 1  0 ti_203_lotes-codtrp         .
*    PERFORM fill_cell USING h 2  0 ti_203_lotes-dscodtrp       .
*    PERFORM fill_cell USING h 3  0 ti_203_lotes-codposto       .
*    PERFORM fill_cell USING h 4  0 ti_203_lotes-dscodposto     .
*    PERFORM fill_cell USING h 5  0 ti_203_lotes-lote           .
*    PERFORM fill_cell USING h 6  0 ti_203_lotes-docsapadto     .
*    PERFORM fill_cell USING h 7  0 ti_203_lotes-bl             .
*    PERFORM fill_cell USING h 8  0 ti_203_lotes-datalote       .
*    PERFORM fill_cell USING h 9  0 ti_203_lotes-vencimento     .
*    PERFORM fill_cell USING h 10 0 ti_203_lotes-vlrimportado   .
*    PERFORM fill_cell USING h 11 0 ti_203_lotes-vlrconfirmado  .
*    PERFORM fill_cell USING h 12 0 ti_203_lotes-vlrrecusado    .
*    PERFORM fill_cell USING h 13 0 ti_203_lotes-vlrrealizado   .
*    PERFORM fill_cell USING h 14 0 ti_203_lotes-acrdecr        .
*    PERFORM fill_cell USING h 15 0 ti_203_lotes-datafechamento .
*  ENDLOOP.

  CALL METHOD OF h_excel 'Worksheets' = h_mapl." EXPORTIN    G #1 = 2.

* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map
    EXPORTING
    #1 = 2.

* tell user what is going on
  SET PROPERTY OF h_map 'NAME' = 'COPY'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = TEXT-009
    EXCEPTIONS
      OTHERS = 1.

* output column headings to active Excel sheet
  PERFORM fill_cell USING 1 1  1 'Transportador'(001).
  PERFORM fill_cell USING 1 2  1 'Descrição Transp.'(002).
  PERFORM fill_cell USING 1 3  1 'Posto'(003).
  PERFORM fill_cell USING 1 4  1 'Descrição Posto'(004).
  PERFORM fill_cell USING 1 5  1 'Lote'(005).
  PERFORM fill_cell USING 1 6  1 'Docto SAP'(006).
  PERFORM fill_cell USING 1 7  1 'BL'(007).
  PERFORM fill_cell USING 1 8  1 'Data'(008).
  PERFORM fill_cell USING 1 9  1 'Vencimento'(009).
  PERFORM fill_cell USING 1 10 1 'Vlr Importado'(010).
  PERFORM fill_cell USING 1 11 1 'Vlr Confirmado'(011).
  PERFORM fill_cell USING 1 12 1 'VLR RECUSADO'(012).
  PERFORM fill_cell USING 1 13 1 'Vlr Realizado'(013).
  PERFORM fill_cell USING 1 14 1 'A/D'(014).
  PERFORM fill_cell USING 1 15 1 'Dt Fechamento'(015).

  LOOP AT ti_203_lotes.
    h = sy-tabix + 1.
    PERFORM fill_cell USING h 1  0 ti_203_lotes-codtrp         .
    PERFORM fill_cell USING h 2  0 ti_203_lotes-dscodtrp       .
    PERFORM fill_cell USING h 3  0 ti_203_lotes-codposto       .
    PERFORM fill_cell USING h 4  0 ti_203_lotes-dscodposto     .
    PERFORM fill_cell USING h 5  0 ti_203_lotes-lote           .
    PERFORM fill_cell USING h 6  0 ti_203_lotes-docsapadto     .
    PERFORM fill_cell USING h 7  0 ti_203_lotes-bl             .
    PERFORM fill_cell USING h 8  0 ti_203_lotes-datalote       .
    PERFORM fill_cell USING h 9  0 ti_203_lotes-vencimento     .
    PERFORM fill_cell USING h 10 0 ti_203_lotes-vlrimportado   .
    PERFORM fill_cell USING h 11 0 ti_203_lotes-vlrconfirmado  .
    PERFORM fill_cell USING h 12 0 ti_203_lotes-vlrrecusado    .
    PERFORM fill_cell USING h 13 0 ti_203_lotes-vlrrealizado   .
    PERFORM fill_cell USING h 14 0 ti_203_lotes-acrdecr        .
    PERFORM fill_cell USING h 15 0 ti_203_lotes-datafechamento .
  ENDLOOP.

  FREE OBJECT h_excel.

  MESSAGE i000(z01) WITH 'Gerado para o EXCEL com sucesso !' .

ENDFORM.                    " GERA_EXCEL


*---------------------------------------------------------------------*
*       FORM FILL_CELL                                                *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell USING i j bold val.
  CALL METHOD OF h_excel 'Cells' = h_zl
    EXPORTING
    #1 = i
    #2 = j.
  PERFORM err_hdl.
  SET PROPERTY OF h_zl 'Value' = val .
  PERFORM err_hdl.
  GET PROPERTY OF h_zl 'Font' = h_f.
  PERFORM err_hdl.
  SET PROPERTY OF h_f 'Bold' = bold .
  PERFORM err_hdl.
ENDFORM.                    "FILL_CELL

*&---------------------------------------------------------------------*
*&      Form  ERR_HDL
*&---------------------------------------------------------------------*
*       outputs OLE error if any                                       *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM err_hdl.
  IF sy-subrc <> 0.
    WRITE: / 'Fehler bei OLE-Automation:'(010), sy-subrc.
    STOP.
  ENDIF.
ENDFORM.                    "ERR_HDL

*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL_401
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_excel_401 .


  IF ti_400_confer IS INITIAL.
    MESSAGE w000(z01) WITH 'Não ha dados para Gerar EXCEL !' .
    CHECK NOT ti_400_confer IS INITIAL.
    "    stop.
  ENDIF.

  DATA  h TYPE i.

* start Excel
  CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
*  PERFORM ERR_HDL.

  SET PROPERTY OF h_excel  'Visible' = 1.

* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = TEXT-008
    EXCEPTIONS
      OTHERS = 1.

* get list of workbooks, initially empty
  CALL METHOD OF h_excel 'Workbooks' = h_mapl.
  PERFORM err_hdl.
* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map.
  PERFORM err_hdl.
* tell user what is going on
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE = 0
      text   = TEXT-009
    EXCEPTIONS
      OTHERS = 1.

** output column headings to active Excel sheet
*  PERFORM fill_cell USING 1 1  1 'ChvId'(001).
*  PERFORM fill_cell USING 1 2  1 'Descrição ChvId'(002).
*  PERFORM fill_cell USING 1 3  1 'Carta Frete'(003).
*  PERFORM fill_cell USING 1 4  1 'Conhecimento'(004).
*  PERFORM fill_cell USING 1 5  1 'Peso Origem'(005).
*  PERFORM fill_cell USING 1 6  1 'Peso Importado'(006).
*  PERFORM fill_cell USING 1 7  1 'Peso Conferido'(007).
*  PERFORM fill_cell USING 1 8  1 'Peso Diferença'(008).
*  PERFORM fill_cell USING 1 9  1 'Vlr Sobra/Quebra'(009).
*  PERFORM fill_cell USING 1 10 1 'Vlr Perda'(010).
*  PERFORM fill_cell USING 1 11 1 'Vle Importado'(011).
*  PERFORM fill_cell USING 1 12 1 'Vlr Programado'(012).
*  PERFORM fill_cell USING 1 13 1 'Vlr Conferido'(013).
*  PERFORM fill_cell USING 1 14 1 'Vlr Diferença'(014).
*  PERFORM fill_cell USING 1 15 1 'Vlr Seguro'(015).
*  PERFORM fill_cell USING 1 16 1 'Vlr Imp'(016).
*  PERFORM fill_cell USING 1 17 1 'Vlr Adto'(017).
*  PERFORM fill_cell USING 1 18 1 'Vlr Tarifa'(018).
*
*  LOOP AT TI_400_CONFER.
*    h = sy-tabix + 1.
*    PERFORM fill_cell USING h 1  0 TI_400_CONFER-CHVID            .
*    PERFORM fill_cell USING h 2  0 TI_400_CONFER-DESCHVID         .
*    PERFORM fill_cell USING h 3  0 TI_400_CONFER-CTAFRETE         .
*    PERFORM fill_cell USING h 4  0 TI_400_CONFER-CONHEC           .
*    PERFORM fill_cell USING h 5  0 TI_400_CONFER-PESO_ORIGEM      .
*    PERFORM fill_cell USING h 6  0 TI_400_CONFER-PESO_IMPORTADO   .
*    PERFORM fill_cell USING h 7  0 TI_400_CONFER-PESO_CONFERIDO   .
*    PERFORM fill_cell USING h 8  0 TI_400_CONFER-PESO_DIFERENCA   .
*    PERFORM fill_cell USING h 9  0 TI_400_CONFER-VLR_PERDA        .
*    PERFORM fill_cell USING h 10 0 TI_400_CONFER-VLR_SOBRA_QUEBRA .
*    PERFORM fill_cell USING h 11 0 TI_400_CONFER-VLR_IMPORTADO    .
*    PERFORM fill_cell USING h 12 0 TI_400_CONFER-VLR_PROGRAMADO   .
*    PERFORM fill_cell USING h 13 0 TI_400_CONFER-VLR_CONFERIDO    .
*    PERFORM fill_cell USING h 14 0 TI_400_CONFER-VLR_DIFERENCA    .
*    PERFORM fill_cell USING h 15 0 TI_400_CONFER-VLR_SEGURO       .
*    PERFORM fill_cell USING h 16 0 TI_400_CONFER-VLR_IMP_RETIDO   .
*    PERFORM fill_cell USING h 17 0 TI_400_CONFER-VLR_ADIANTAMENTO .
*    PERFORM fill_cell USING h 18 0 TI_400_CONFER-VLR_TARIFA       .
*
*  ENDLOOP.

  CALL METHOD OF h_excel 'Worksheets' = h_mapl." EXPORTIN    G #1 = 2.

* add a new workbook
  CALL METHOD OF h_mapl 'Add' = h_map
    EXPORTING
    #1 = 2.

* tell user what is going on
  SET PROPERTY OF h_map 'NAME' = 'COPY'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = TEXT-009
    EXCEPTIONS
      OTHERS = 1.

* output column headings to active Excel sheet


  PERFORM fill_cell USING 1 1  1 'ChvId'(001).
  PERFORM fill_cell USING 1 2  1 'Descrição ChvId'(002).
  PERFORM fill_cell USING 1 3  1 'Carta Frete'(003).
  PERFORM fill_cell USING 1 4  1 'Conhecimento'(004).
  PERFORM fill_cell USING 1 5  1 'Peso Origem'(005).
  PERFORM fill_cell USING 1 6  1 'Peso Importado'(006).
  PERFORM fill_cell USING 1 7  1 'Peso Conferido'(007).
  PERFORM fill_cell USING 1 8  1 'Peso Diferença'(008).
  PERFORM fill_cell USING 1 9  1 'Vlr Sobra/Quebra'(009).
  PERFORM fill_cell USING 1 10 1 'Vlr Perda'(010).
  PERFORM fill_cell USING 1 11 1 'Vle Importado'(011).
  PERFORM fill_cell USING 1 12 1 'Vlr Programado'(012).
  PERFORM fill_cell USING 1 13 1 'Vlr Conferido'(013).
  PERFORM fill_cell USING 1 14 1 'Vlr Diferença'(014).
  PERFORM fill_cell USING 1 15 1 'Vlr Seguro'(015).
  PERFORM fill_cell USING 1 16 1 'Vlr Imp'(016).
  PERFORM fill_cell USING 1 17 1 'Vlr Adto'(017).
  PERFORM fill_cell USING 1 18 1 'Vlr Tarifa'(018).

  LOOP AT ti_400_confer.
    h = sy-tabix + 1.
    PERFORM fill_cell USING h 1  0 ti_400_confer-chvid            .
    PERFORM fill_cell USING h 2  0 ti_400_confer-deschvid         .
    PERFORM fill_cell USING h 3  0 ti_400_confer-ctafrete         .
    PERFORM fill_cell USING h 4  0 ti_400_confer-conhec           .
    PERFORM fill_cell USING h 5  0 ti_400_confer-peso_origem      .
    PERFORM fill_cell USING h 6  0 ti_400_confer-peso_importado   .
    PERFORM fill_cell USING h 7  0 ti_400_confer-peso_conferido   .
    PERFORM fill_cell USING h 8  0 ti_400_confer-peso_diferenca   .
    PERFORM fill_cell USING h 9  0 ti_400_confer-vlr_perda        .
    PERFORM fill_cell USING h 10 0 ti_400_confer-vlr_sobra_quebra .
    PERFORM fill_cell USING h 11 0 ti_400_confer-vlr_importado    .
    PERFORM fill_cell USING h 12 0 ti_400_confer-vlr_programado   .
    PERFORM fill_cell USING h 13 0 ti_400_confer-vlr_conferido    .
    PERFORM fill_cell USING h 14 0 ti_400_confer-vlr_diferenca    .
    PERFORM fill_cell USING h 15 0 ti_400_confer-vlr_seguro       .
    PERFORM fill_cell USING h 16 0 ti_400_confer-vlr_imp_retido   .
    PERFORM fill_cell USING h 17 0 ti_400_confer-vlr_adiantamento .
    PERFORM fill_cell USING h 18 0 ti_400_confer-vlr_tarifa       .
  ENDLOOP.

  FREE OBJECT h_excel.

  MESSAGE i000(z01) WITH 'Gerado para o EXCEL com sucesso !' .

ENDFORM.                    " GERA_EXCEL_401

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       Enviar e-mail de lotes Selecionados
*----------------------------------------------------------------------*
FORM enviar_email .

* Verifica condição para eliminação do lote
  LOOP AT ti_203_lotes WHERE mark = cc_x.

    CALL FUNCTION 'Z_LES_EMAIL_COCKPIT'
      EXPORTING
        cod_transp     = ti_203_lotes-codtrp
        cod_posto      = ti_203_lotes-codposto
        cod_lote       = ti_203_lotes-lote
      EXCEPTIONS
        nao_localizada = 1
        nao_email      = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s056.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ENVIAR_EMAIL


*&---------------------------------------------------------------------*
*&      Form  INCLUI_LANCTO_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inclui_lancto_lote USING vg_lancto_acre_desc TYPE zles_cockpit_lancto.
  CLEAR: sheader_300. "Força refazer a tela no lançamento

* Inclui ou atualiza registro na tabela interna de consulta
  READ TABLE ti_cockpit_lancto WITH KEY codtrp = vg_lancto_acre_desc-codtrp
                                      codposto = vg_lancto_acre_desc-codposto
                                          lote = vg_lancto_acre_desc-lote
                                         chvid = vg_lancto_acre_desc-chvid
                                      datalote = vg_lancto_acre_desc-datalote
                           TRANSPORTING NO FIELDS
                           BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MODIFY ti_cockpit_lancto FROM vg_lancto_acre_desc INDEX sy-tabix.
  ELSE.
    APPEND vg_lancto_acre_desc TO ti_cockpit_lancto.
    SORT   ti_cockpit_lancto BY codtrp codposto lote ctafrete conhec chvid datalote.
  ENDIF.

* Inclui ou atualiza base de dados
  CLEAR zlest0016.
  zlest0016-transportador    = vg_lancto_acre_desc-codtrp.
  zlest0016-posto            = vg_lancto_acre_desc-codposto.
  zlest0016-lote             = vg_lancto_acre_desc-lote.
  zlest0016-chvid            = vg_lancto_acre_desc-chvid.
  zlest0016-ctafrete         = vg_lancto_acre_desc-ctafrete.
  zlest0016-conhecimento     = vg_lancto_acre_desc-conhec.
  zlest0016-peso_origem      = vg_lancto_acre_desc-peso_origem.
  zlest0016-peso_importado   = vg_lancto_acre_desc-peso_importado.
  zlest0016-peso_confirmado  = vg_lancto_acre_desc-peso_confirmado.
  zlest0016-unidade_peso     = vg_lancto_acre_desc-unid_peso.
  zlest0016-vlr_origem       = vg_lancto_acre_desc-vlrorigem.
  zlest0016-vlr_importado    = vg_lancto_acre_desc-vlrimportado.
  zlest0016-vlr_confirmado   = vg_lancto_acre_desc-vlrconfirmado.
  zlest0016-diferenca        = vg_lancto_acre_desc-vlrdiferenca.
  zlest0016-peso_confiralter = 0.
  zlest0016-vlr_confiralter  = 0.
  zlest0016-vlr_programado   = vg_lancto_acre_desc-vlrprogramado.
  zlest0016-dta_chegada      = vg_lancto_acre_desc-dtacheg.
  zlest0016-observacoes      = vg_lancto_acre_desc-observacoes.
  zlest0016-erdat            = sy-datum.
  zlest0016-uzeit            = sy-uzeit.
  zlest0016-uname            = sy-uname.
  MODIFY zlest0016 FROM zlest0016.

  COMMIT WORK. " CSB

ENDFORM.                    " INCLUI_LANCTO_LOTE

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_TODOS_CONFERENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selecionar_todos_conferencia .

  LOOP AT ti_400_confer.
    ti_400_confer-check = 'X'.
    MODIFY ti_400_confer INDEX sy-tabix TRANSPORTING check.
  ENDLOOP.

ENDFORM.                    " SELECIONAR_TODOS_CONFERENCIA
