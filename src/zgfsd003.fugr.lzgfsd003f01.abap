*----------------------------------------------------------------------*
***INCLUDE LZGFSD003F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_9000
*&---------------------------------------------------------------------*
FORM f_refresh_9000 .

  CLEAR: gv_ucomm_9000,
         gv_erro,
         gt_alv_9000[],
         gv_edit_9000,
         gv_fatu_9000,
         gv_exibe_travas,
         gt_desab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_EVENT
*&---------------------------------------------------------------------*
FORM f_check_event .

  DATA lv_erro.

  PERFORM f_check_selec CHANGING lv_erro.

  IF lv_erro IS INITIAL.

    READ TABLE gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WITH KEY selec = 'X'.

    IF sy-subrc EQ 0.

      zsds078-edit_index = sy-tabix.
      zsds078-editing = 'X'.

      zsds078-doc_simulacao = <fs_alv>-doc_simulacao.
      zsds078-sequencia = <fs_alv>-sequencia.
      zsds078-dtprevpag_a = <fs_alv>-dtprevpag_a.
      zsds078-taxa_a = <fs_alv>-taxa_a.
      zsds078-vlr_prev = <fs_alv>-vlr_prev.
      zsds078-vlr_prev_brl = <fs_alv>-vlr_prev_brl.
      zsds078-multa_prev = <fs_alv>-multa_prev.
      zsds078-juros_prev = <fs_alv>-juros_prev.
      zsds078-vlr_liq_prev = <fs_alv>-vlr_liq_prev.
      zsds078-vlr_liq_prev_brl = zsds078-vlr_liq_prev * zsds078-taxa_a. " 26.01.2024


    ELSE.

      CLEAR: zsds078-editing,
             zsds078-sequencia,
             zsds078-dtprevpag_a,
             zsds078-taxa_a,
             zsds078-vlr_prev,
             zsds078-vlr_prev_brl,
             zsds078-multa_prev,
             zsds078-juros_prev,
             zsds078-vlr_liq_prev,
             zsds078-vlr_liq_prev_brl.

      PERFORM f_global_to_screen
     CHANGING zsds078-dtvenc
              zsds078-taxa_a
              zsds078-dtprevpag_a.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_TAX_FROM_OV
*&---------------------------------------------------------------------*
FORM f_get_tax_from_ov USING p_vbeln TYPE vbeln
                    CHANGING p_vlr_ov TYPE netwr
                             p_vlr_imposto TYPE netwr
                             p_mont_moeda TYPE netwr.

  SELECT vbap~vbeln,vbap~posnr,netwr,mwsbp,kwmeng FROM vbap
        INNER JOIN vbep ON vbep~vbeln = vbap~vbeln
                   AND vbep~posnr = vbap~posnr
    INTO TABLE @DATA(lt_vbap)
      WHERE vbap~vbeln = @p_vbeln
        AND vbep~lifsp <> '12'.

  SELECT SINGLE knumv,netwr FROM vbak
    INTO @DATA(ls_vbak)
      WHERE vbeln = @p_vbeln.

  IF ls_vbak-knumv IS NOT INITIAL.

    SELECT SINGLE kawrt FROM konv
        INTO @DATA(lw_kawrt)
          WHERE knumv = @ls_vbak-knumv
            AND kschl IN ('ICMO').

    p_vlr_imposto = lw_kawrt.

    "p_vlr_ov = ls_vbak-netwr.

  ENDIF.

  LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).
    ADD <fs_vbap>-netwr TO p_vlr_ov.
  ENDLOOP.

  SELECT SUM( mont_moeda ) FROM zfit0026
    INTO p_mont_moeda
      WHERE vbeln = p_vbeln.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZSDT0090
*&---------------------------------------------------------------------*
FORM f_get_zsdt0090 USING p_vbeln TYPE vbeln
                 CHANGING cv_soma TYPE netwr
                          ct_travas TYPE zsdt0090_t.
  CLEAR cv_soma.

  SELECT * FROM zsdt0090
     INTO TABLE ct_travas
       WHERE vbelv = p_vbeln
         AND categoria = 'C'
         AND estorno <> 'X'
         AND trav_camb_utilizada = space
         AND vbelv_agp = space.

  " 19.07.2023 - RAMON - AGRUPAMENTO DE OV -->
  SELECT * FROM zsdt0090
     APPENDING TABLE ct_travas
       WHERE categoria = 'C'
         AND estorno <> 'X'
         AND trav_camb_utilizada = space
         AND vbelv_agp = p_vbeln.
  " 19.07.2023 - RAMON - AGRUPAMENTO DE OV --><

  LOOP AT ct_travas ASSIGNING FIELD-SYMBOL(<fs_travas>).
    ADD <fs_travas>-prev_vl_liq_usd TO cv_soma.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_STATUS_FAT
*&---------------------------------------------------------------------*
FORM f_check_status_fat USING p_vbeln TYPE vbeln
                       CHANGING p_check TYPE zsde_status_faturamento.

  CLEAR p_check.

  " Verifica se OV está faturada 100%
  SELECT SUM( rfmng ) FROM vbfa
    INTO @DATA(lv_rfmng)
      WHERE vbelv = @p_vbeln
        AND vbtyp_v = 'C'
        AND vbtyp_n = 'J'
        AND bwart NE @space.

  SELECT SUM( kwmeng ) FROM vbap
    INNER JOIN vbep ON vbep~vbeln = vbap~vbeln
                   AND vbep~posnr = vbap~posnr
     INTO @DATA(lv_kwmeng)
      WHERE vbap~vbeln = @p_vbeln
        AND vbep~lifsp <> '12'.

*  " Se for igual abre os popup e as aplica as regras do cenário 02 e 03
*  IF lv_rfmng <> lv_kwmeng.
*    p_check = 'X'.
*  ENDIF.

  " 100%
  IF lv_rfmng = lv_kwmeng.
    p_check = 'C'. " 100% Faturada
    EXIT.
  ENDIF.

  IF lv_rfmng IS INITIAL.
    p_check = ''. " zero % faturada
  ENDIF.

  IF lv_rfmng > 0.
    p_check = 'P'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_FIELDS
*&---------------------------------------------------------------------*
FORM f_check_fields CHANGING p_erro TYPE c.

  DATA lv_vlr TYPE netwr_ap.

  CLEAR p_erro.

  CHECK gv_edit_9000 = 'X'.

  IF zsds078-dtprevpag_n IS INITIAL.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Preencher' 'Nova Prev. Pgto' DISPLAY LIKE 'E'.

  ENDIF.

  IF check_dt IS INITIAL.
    MESSAGE 'Data deve ser dia util' TYPE 'S' DISPLAY LIKE 'E'..
    p_erro = 'X'.
    EXIT.
  ENDIF.

  CHECK p_erro IS INITIAL.

  "Data não pode ser menor que Data de criação do registro (SYSDATE) -
  " Para Matriz Poderá ser igual ou maior (se for filial
  " somente permitir 72h Uteis - 3 dias Uteis)

  IF zsds078-dtprevpag_n < sy-datum.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Nova Prev. Pgto' 'não pode ser menor que a data atual' DISPLAY LIKE 'E'.

  ENDIF.

  CHECK p_erro IS INITIAL.

  IF zsds078-taxa_neg IS INITIAL.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Preencher' 'Taxa do Négocio' DISPLAY LIKE 'E'.

  ENDIF.

  CHECK p_erro IS INITIAL.

  IF zsds078-vlr_prev IS INITIAL.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Preencher Valor Previsto' DISPLAY LIKE 'E'.

    EXIT.

  ENDIF.

  IF zsds078-vlr_liq_prev > zsds078-vlr_ov.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Valor Liq. Previsto' 'é maior que o valor' 'da OV' DISPLAY LIKE 'E'.

  ENDIF.

  LOOP AT gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>).

    IF <fs_alv>-selec = 'X'.
      "SUBTRACT <fs_alv>-vlr_liq_prev from lv_vlr.
    ELSE.
      ADD <fs_alv>-vlr_liq_prev TO lv_vlr.
    ENDIF.

  ENDLOOP.

  "DATA(lv_vlr) = REDUCE netwr_ap( INIT x TYPE netwr_ap FOR wa IN gt_alv_9000 NEXT x = x + wa-vlr_liq_prev ).

  lv_vlr = lv_vlr + zsds078-vlr_liq_prev.

  IF lv_vlr > zsds078-vlr_ov.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Total s/Trava Cambio' DISPLAY LIKE 'E'.

  ENDIF.

  " ----->
  IF zsds078-vlr_prev > zsds078-tt_aliq.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Total s/Trava Cambio' DISPLAY LIKE 'E'.

  ENDIF.

  IF lv_vlr > zsds078-tt_aliq.

    p_erro = 'X'.

    MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Total s/Trava Cambio' DISPLAY LIKE 'E'.

  ENDIF.
  " -----<

  CASE zsds078-cenario.
      " --------------------------------------- 100% FATURADA
    WHEN '01'.
      " --------------------------------------- PARCIALMENTE FATURADA
    WHEN '02'.

      " 15.06.2023 - RAMON - Foi descomentado pq só pode alterar o cambio do valor faturado
      " Story: BUG IMPEDITIVO 115347 - Cenario 04. -->

      IF zsds078-fat_flg = 'X'.

        IF zsds078-vlr_prev > zsds078-tt_fat.

          p_erro = 'X'.

          MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Valor Faturado' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.

        IF zsds078-tt_trava_new > zsds078-tt_fat.

          p_erro = 'X'.

          MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Valor Faturado' 'com Valores de Trava de Cambio' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.

      ENDIF.

      " 15.06.2023 - <<<

      IF zsds078-afat_flg = 'X'.

        IF zsds078-vlr_prev > zsds078-tt_afat.

          p_erro = 'X'.

          MESSAGE s001(zsd) WITH 'Valor Liq. Previsto maior' 'que o Valor a Faturar' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.


      ENDIF.

      " --------------------------------------- 0% FATURADA
    WHEN '03'.
  ENDCASE.


  CHECK p_erro IS INITIAL.

*  IF zsds078-kunnr IS INITIAL.
*
*    p_erro = 'X'.
*
*    MESSAGE s001(zsd) WITH 'Preencher cliente' DISPLAY LIKE 'E'.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_TAXA_CURV_PROJ
*&---------------------------------------------------------------------*
FORM f_get_taxa_curv_proj USING p_dt_venc TYPE sydatum
                       CHANGING p_taxa TYPE kurrf.

  DATA: gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva,
        gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.

  CREATE OBJECT gobj_zcl_taxa_curva.

  CREATE OBJECT gobj_zcl_webservice_tx_curva.

  CLEAR p_taxa.

*  IF p_dt_venc LE sy-datum.
*    sy-cprog = 'ZCARGA'.
*  ENDIF.

  p_taxa = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = p_dt_venc   " Dt. Vencimento
                                                      i_data_lib = sy-datum    " Dt. Liberação
                                                      i_tipo     = 'C' ).

  IF p_taxa IS INITIAL.

    MESSAGE i001(zsd) WITH 'A consulta da taxa curva está indisponivel'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPD_FIELS_9000
*&---------------------------------------------------------------------*
FORM f_upd_fiels_9000 CHANGING p_erro.

  DATA lv_sim.
  DATA lv_erro.

  PERFORM f_check_calc_curva CHANGING lv_sim.

  CLEAR p_erro.

  CLEAR: zsds078-tt_editing,
        zsds078-tt_trava_rest,
        zsds078-qtd_restante,
        zsds078-tt_em_travac,
        zsds078-vlr_max_restante,
        zsds078-tt_trava_new.

  IF lv_sim = 'X'.

    PERFORM f_get_taxa_curv_proj
      USING zsds078-dtprevpag_n
   CHANGING zsds078-taxa_curv_proj.

    zsds078-taxa_neg = zsds078-taxa_curv_proj.

  ENDIF.

  IF sy-dynnr = '7000'.

    PERFORM f_calc_qtd_prev_7000 CHANGING p_erro.
  ENDIF.

  IF ( zsds078-juros_prev IS INITIAL AND zsds078-multa_prev IS INITIAL )
    AND zsds078-vlr_prev IS NOT INITIAL
    AND zsds078-exec_01 = 'X'.

    CLEAR zsds078-exec_01. " não é mais primeira execução

    PERFORM f_calcular_juros
     USING zsds078-dtinijuros
           "zsds078-dtvenc
           zsds078-dtprevpag_n
           zsds078-juros_ano
           zsds078-tx_multa
           zsds078-vlr_ov
           zsds078-taxa_a
           zsds078-taxa_neg
           zsds078-vlr_prev
  CHANGING zsds078-juros_prev
           zsds078-multa_prev.

  ENDIF.

  "PERFORM f_valida_vlr_prev.

  zsds078-vlr_liq_prev = zsds078-vlr_prev - ( zsds078-juros_prev +
                         zsds078-multa_prev ).

  zsds078-vlr_prev_brl = zsds078-vlr_prev * zsds078-taxa_neg.

  zsds078-dtprevpag_aux = zsds078-dtprevpag_n.

  " 25.01.2024 - Inclusão campo valor liq prev BRL -->
  zsds078-vlr_liq_prev_brl = zsds078-vlr_liq_prev * zsds078-taxa_neg.
  " 25.01.2024 - Inclusão campo valor liq prev BRL --<

  LOOP AT gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>).

    IF <fs_alv>-selec = 'X'.

      ADD <fs_alv>-vlr_liq_prev TO zsds078-tt_editing.

      ADD zsds078-vlr_liq_prev TO zsds078-tt_trava_new.

    ELSE.

      ADD <fs_alv>-vlr_liq_prev TO zsds078-tt_trava_rest.
      ADD <fs_alv>-vlr_liq_prev TO zsds078-tt_trava_new.

      ADD 1 TO zsds078-qtd_restante.

    ENDIF.

    ADD <fs_alv>-vlr_liq_prev TO zsds078-tt_em_travac.

  ENDLOOP.

  " se nao selecionou nenhuma trava, então a trava nova
  " vai ser o total + o que foi digitado
  IF zsds078-editing IS INITIAL.
    ADD zsds078-vlr_liq_prev TO zsds078-tt_trava_new.
  ENDIF.

  IF zsds078-qtd_restante > 0.
    zsds078-vlr_max_restante = zsds078-tt_fat / zsds078-qtd_restante.
  ELSE.
    zsds078-vlr_max_restante = zsds078-tt_fat.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALCULAR_JUROS
*&---------------------------------------------------------------------*
FORM f_calcular_juros USING p_dtvenc TYPE sydatum
                            p_dtprevpag_n TYPE sydatum
                            p_juros_ano TYPE vvwbekpzp
                            p_tx_multa TYPE zde003
                            p_vlr_ov TYPE netwr_ap
                            p_taxa_a TYPE kurrf
                            p_taxa_n TYPE kurrf
                            p_vlr TYPE netwr_ap
                       CHANGING p_juros TYPE netwr_ap
                                p_multa TYPE netwr_ap.

  DATA lv_atraso TYPE i.
  DATA lv_prop TYPE kurrf.

  DATA vlr_jros TYPE p DECIMALS 2.
  DATA vlr_mult TYPE p DECIMALS 2.
  DATA prop_juros TYPE p DECIMALS 9.
  DATA prop_multa TYPE p DECIMALS 9.

  lv_atraso  = ( p_dtprevpag_n - p_dtvenc ).

  IF lv_atraso < 1.
    EXIT."lv_atraso = 1.
  ENDIF.

  lv_prop = ( p_juros_ano / 360 ) * lv_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

  vlr_jros = ( p_vlr_ov * lv_prop ) / 100. " Porcentagem proporcional ao valor do juros

  vlr_mult = ( p_vlr_ov * p_tx_multa ) / 100.

  DATA(tot_ov) = ( vlr_jros + p_vlr_ov ). "Total da OV + juros.

  prop_juros = ( vlr_jros / tot_ov )  * 100. " Porcentagem proporcional ao valor do juros
  prop_multa = ( vlr_mult / tot_ov )  * 100. " Porcentagem proporcional da multa.

  p_juros = ( prop_juros * p_vlr ) / 100.
  p_multa =  ( prop_multa * p_vlr ) / 100.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_VALDT
*&---------------------------------------------------------------------*
FORM f_check_valdt .

  CLEAR check_dt.

  CHECK NOT zsds078-dtprevpag_n IS INITIAL.

  zcl_solicitacao_ov=>dia_util( EXPORTING p_vencimento = zsds078-dtprevpag_n
                                IMPORTING e_subrc      = check_dt ).
  IF check_dt IS INITIAL.
    MESSAGE 'Data deve ser dia util' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SELEC
*&---------------------------------------------------------------------*
FORM f_check_selec CHANGING p_erro TYPE c.

  DATA lv_count TYPE i.
  DATA lv_edit.

  LOOP AT gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE selec = 'X'.
    ADD 1 TO lv_count.
    lv_edit = 'X'.
  ENDLOOP.

  IF lv_count > 1.

    LOOP AT gt_alv_9000 ASSIGNING <fs_alv>.
      <fs_alv>-selec = space.
    ENDLOOP.

    p_erro = 'X'.

    MESSAGE 'Selecionar apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

  IF lv_edit = 'X'.

    CALL FUNCTION 'ZSDMF_AUTORIZACAO_ZSDT0087'
      EXPORTING
        iv_bukrs        = zsds078-bukrs
        iv_vkbur        = zsds078-vkbur
        iv_field        = 'ZFLG_ESTO'
        iv_value        = 'X'
      EXCEPTIONS
        sem_autorizacao = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.

      LOOP AT gt_alv_9000 ASSIGNING <fs_alv>.
        <fs_alv>-selec = space.
      ENDLOOP.

      p_erro = 'X'.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.


      EXIT.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GLOBAL_TO_SCREEN
*&---------------------------------------------------------------------*
FORM f_global_to_screen CHANGING p_dt_venc TYPE sydatum
                                 p_taxa_a TYPE kurrf
                                 p_prevpag TYPE sydatum.

  p_dt_venc = gv_dtvenc.
  p_taxa_a = gv_taxa_a.
  p_prevpag = gv_dtprev_pag_a.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_COM_POPUP
*&---------------------------------------------------------------------*
FORM f_check_com_popup CHANGING p_erro.

  DATA lv_ret.

  CLEAR p_erro.

  IF zsds078-sequencia IS NOT INITIAL
    AND zsds078-vbeln IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Ao confirmar estará cancelando a trava de cambio selecionada e criando uma nova, Deseja prosseguir?'
        text_button_1         = 'Continuar'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Cancelar'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_ret
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_ret <> '1'.

      p_erro = 'X'.

      EXIT.

    ENDIF.

  ENDIF.

  IF zsds078-taxa_neg <> zsds078-taxa_curv_proj.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Taxa do negócio diferente da taxa curva projetada'
        text_button_1         = 'Continuar'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Cancelar'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_ret
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_ret <> '1'.

      p_erro = 'X'.

      EXIT.

    ENDIF.



  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_VLR_FATURADO
*&---------------------------------------------------------------------*
FORM f_get_vlr_faturado USING uv_vbeln TYPE vbeln
                     CHANGING cv_faturado TYPE netwr_ap
                              cv_afaturar TYPE netwr_ap
                              cv_qtde_fat TYPE menge_d
                              cv_qtde_afat TYPE menge_d.

  CLEAR: cv_faturado, cv_afaturar, cv_qtde_fat, cv_qtde_afat.

  SELECT vbeln FROM vbfa
    INTO TABLE @DATA(lt_vbeln)
      WHERE vbelv = @uv_vbeln
        AND vbtyp_n = 'M'
        AND vbtyp_v = 'C'.

  CHECK sy-subrc EQ 0.

  SELECT vbeln,posnr,fkimg,netwr,mwsbp FROM vbrp
    INTO TABLE @DATA(lt_vbrp)
      FOR ALL ENTRIES IN @lt_vbeln
      WHERE vbeln = @lt_vbeln-vbeln.

  LOOP AT lt_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).

    ADD <fs_vbrp>-fkimg TO cv_qtde_fat.
    ADD <fs_vbrp>-netwr TO cv_faturado.
    ADD <fs_vbrp>-mwsbp TO cv_faturado.

  ENDLOOP.

  SELECT vbap~vbeln,vbap~posnr,netwr,mwsbp,kwmeng FROM vbap
        INNER JOIN vbep ON vbep~vbeln = vbap~vbeln
                   AND vbep~posnr = vbap~posnr
    INTO TABLE @DATA(lt_vbap)
      WHERE vbap~vbeln = @uv_vbeln
        AND vbep~lifsp <> '12'.

  LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).

    ADD <fs_vbap>-kwmeng TO cv_qtde_afat.
    ADD <fs_vbap>-netwr TO cv_afaturar.
    ADD <fs_vbap>-mwsbp TO cv_afaturar.

  ENDLOOP.

  cv_qtde_afat = cv_qtde_afat - cv_qtde_fat.
  cv_afaturar = cv_afaturar - cv_faturado.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_TAXA_CURVA
*&---------------------------------------------------------------------*
FORM f_atualiza_taxa_curva.

  IF zsds078-dtprevpag_n IS NOT INITIAL.

    PERFORM f_get_taxa_curv_proj
      USING zsds078-dtprevpag_n
   CHANGING zsds078-taxa_curv_proj.

    zsds078-taxa_neg = zsds078-taxa_curv_proj.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CALC_CURVA
*&---------------------------------------------------------------------*
FORM f_check_calc_curva CHANGING cv_sim.

  IF zsds078-dtprevpag_n IS NOT INITIAL AND zsds078-taxa_neg IS INITIAL.

    cv_sim = 'X'.
    EXIT.

  ENDIF.

  IF zsds078-dtprevpag_n <> zsds078-dtprevpag_aux.

    cv_sim = 'X'.
    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQUENCIAL
*&---------------------------------------------------------------------*
FORM f_get_sequencial USING p_vbeln TYPE vbeln
                   CHANGING p_seqnr TYPE bdl_seq.

  SELECT MAX( seqnr ) FROM zsdt0315
    INTO p_seqnr
      WHERE vbeln = p_vbeln.

  ADD 1 TO p_seqnr.

ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F_CALCULA_VLR_7000
**&---------------------------------------------------------------------*
*FORM f_calcula_vlr_7000 USING p_menge_total TYPE rfmng
*                              p_netwr_total TYPE netwr_ap
*                              p_menge_desej TYPE rfmng
*                     CHANGING p_netwr_desej TYPE netwr_ap.
*
*
*  DATA lv_netwr_uni TYPE kbetr.
*
*  lv_netwr_uni = p_netwr_total / p_menge_total.
*
*  p_netwr_desej = lv_netwr_uni * p_menge_desej.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALCULA_QTD
*&---------------------------------------------------------------------*
FORM f_calcula_qtd USING uv_qtde TYPE rfmng
                  CHANGING ct_desme TYPE zsdc081
                           c_erro TYPE flag.

  DATA lv_lines TYPE int4.
  DATA lv_qtde TYPE rfmng.

  IF uv_qtde IS INITIAL OR ct_desme IS INITIAL.
    c_erro = 'X'.
    EXIT.
  ENDIF.

  lv_lines = lines( ct_desme ).

  lv_qtde = uv_qtde / lv_lines.

  LOOP AT ct_desme ASSIGNING FIELD-SYMBOL(<fs_line>).

    <fs_line>-vlr_calc =  <fs_line>-vlr_unit * lv_qtde.

    <fs_line>-qt_tran = lv_qtde.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CALCULA_VLR
*&---------------------------------------------------------------------*
FORM f_calcula_vlr USING uv_vlr TYPE netwr_ap
                  CHANGING ct_desme TYPE zsdc081
                           c_erro TYPE flag.

  DATA lv_lines TYPE int4.
  DATA lv_valor TYPE netwr_ap.

  IF uv_vlr IS INITIAL OR ct_desme IS INITIAL.
    c_erro = 'X'.
    EXIT.
  ENDIF.

  lv_lines = lines( ct_desme ).

  lv_valor = uv_vlr / lv_lines.

  LOOP AT ct_desme ASSIGNING FIELD-SYMBOL(<fs_line>).

    <fs_line>-unit_calc = <fs_line>-vlr_unit / lv_valor.
    <fs_line>-vlr_unit = lv_valor.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_VLR_PREV
*&---------------------------------------------------------------------*
FORM f_valida_vlr_prev .

  CHECK zsds078-vlr_prev IS INITIAL.

  MESSAGE 'Preencher valor previsto' TYPE 'E'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALC_VLR_QTD_7000
*&---------------------------------------------------------------------*
* V - calcula valor
* Q - calcula quantidade
FORM f_calc_vlr_qtd_7000 USING uv_tp_calc TYPE char1 CHANGING p_erro TYPE c.

  DATA lv_msg TYPE string.
  DATA lv_vlr TYPE netwr_ap.
  DATA lv_multiplo.
  DATA lv_lines TYPE i.
  DATA lv_vlr_rat TYPE netwr_ap.

  CHECK gt_alv_7000[] IS NOT INITIAL.

  IF uv_tp_calc = 'V'.

    CLEAR zsds078-vlr_prev.

    LOOP AT gt_alv_7000 ASSIGNING FIELD-SYMBOL(<fs_alv>).

      CHECK <fs_alv>-qt_tran > 0.

      IF <fs_alv>-spart = '03'.

        PERFORM f_check_multiplo
          USING <fs_alv>-qt_tran
                <fs_alv>-groes
       CHANGING lv_multiplo.

        IF lv_multiplo IS INITIAL.

          lv_msg = `Valor não é múltiplo do valor parametrizado no cadastro do Material: ` && <fs_alv>-groes.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          p_erro = 'X'.
          EXIT.

        ENDIF.

      ENDIF.

      <fs_alv>-vl_tran = <fs_alv>-qt_tran * ( <fs_alv>-vlr_calc / 1000 ).

      ADD <fs_alv>-vl_tran TO lv_vlr.

    ENDLOOP.

    CHECK p_erro IS INITIAL.

    ADD lv_vlr TO zsds078-vlr_prev.

  ELSEIF uv_tp_calc = 'Q'.

    lv_lines = lines( gt_alv_7000 ).

    IF zsds078-vlr_prev IS NOT INITIAL.

      lv_vlr_rat = zsds078-vlr_prev / lv_lines.

      LOOP AT gt_alv_7000 ASSIGNING <fs_alv>.

        <fs_alv>-qt_tran = ( <fs_alv>-kwmeng * lv_vlr_rat ) / <fs_alv>-vlr_ttl_item.

        PERFORM f_check_multiplo
          USING <fs_alv>-qt_tran
                <fs_alv>-groes
        CHANGING lv_multiplo.

        IF lv_multiplo IS INITIAL.

          lv_msg = `Valor não é múltiplo do valor parametrizado no cadastro do Material: ` && <fs_alv>-groes.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          p_erro = 'X'.
          EXIT.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALC_QTD_PREV_7000
*&---------------------------------------------------------------------*
FORM f_calc_qtd_prev_7000 CHANGING p_erro TYPE c.

  DATA lv_field TYPE c LENGTH 30.
  DATA lv_tp_calc. " V = Por valor, Q = por qtde

  GET CURSOR FIELD lv_field.

  "break rblima.

  IF lv_field = 'ZSDS078-VLR_PREV'.
    lv_tp_calc = 'Q'.
  ELSEIF lv_field = 'ZSDS081-QT_TRAN'.
    lv_tp_calc = 'V'.
  ELSE.
    EXIT.
  ENDIF.

  PERFORM f_calc_vlr_qtd_7000 USING lv_tp_calc CHANGING p_erro.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HEDGE_DEDUZIR
*&---------------------------------------------------------------------*
FORM f_hedge_deduzir USING p_values TYPE zsds078.

  DATA lv_vlr_brl TYPE netwr_ap.
  DATA ls_new_0094 TYPE zsdt0094.

  " 1 - ACHAR REGISTRO
  SELECT SINGLE * FROM zsdt0094
    INTO @DATA(ls_0094)
      WHERE nro_sol_ov = @p_values-doc_simulacao
        AND tipo = 'VDI'
        AND tipo_taxa = 'C'
        AND estorno = @space
        AND vbeln = @p_values-vbelv.

  CHECK sy-subrc EQ 0.

  ls_new_0094 = ls_0094.

  lv_vlr_brl = ( p_values-vlr_liq_prev * ls_new_0094-taxa_curva ).
  ls_new_0094-total_proporc =  ls_0094-total_proporc - lv_vlr_brl.
  ls_new_0094-tipo_taxa = 'V'.

  SUBTRACT p_values-qtde_fat FROM ls_new_0094-cadencia_qte.

  CHECK ls_new_0094-total_proporc > 0.

  ls_new_0094-data_registro = sy-datum.
  ls_new_0094-hora_registro = sy-uzeit.

  ls_new_0094-total_proporc =  ls_new_0094-total_proporc * -1.

  INSERT zsdt0094 FROM ls_new_0094.

  COMMIT WORK AND WAIT.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUTH_FILIAL
*&---------------------------------------------------------------------*
FORM f_get_auth_filial USING p_werks TYPE werks_d
                    CHANGING p_liberado TYPE c.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
    ID 'WERKS' FIELD   '*'
    ID 'ACTVT' FIELD '03'.    "Alteração

  IF sy-subrc EQ 0.
    p_liberado = 'X'.
  ELSE.

    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD   p_werks
      ID 'ACTVT' FIELD '03'.    "Alteração

    IF sy-subrc EQ 0.
      p_liberado = 'X'.
    ELSE.
      p_liberado = ''.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_MULTIPLO
*&---------------------------------------------------------------------*
FORM f_check_multiplo USING p_qtde TYPE rfmng
                            p_groes TYPE groes
                  CHANGING p_multiplo TYPE c.

  DATA lv_calc TYPE p DECIMALS 2.

  CHECK p_qtde IS NOT INITIAL.

  IF p_qtde < p_groes.
    lv_calc = p_groes MOD p_qtde.
  ELSE.
    lv_calc = p_qtde MOD p_groes.
  ENDIF.

  IF lv_calc = 0.
    p_multiplo = 'X'.
  ELSE.
    p_multiplo = ''.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOMAIN_TEXT
*&---------------------------------------------------------------------*
FORM f_domain_text USING uv_domname TYPE domname
                CHANGING ct_domain TYPE vlc_dd07v_t.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = uv_domname
    TABLES
      values_tab = ct_domain.

ENDFORM.
