FUNCTION zsdmf_define_cenario_insumos .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_DOC_SIMULACAO) TYPE  ZSDED003 OPTIONAL
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_POSNR) TYPE  POSNR OPTIONAL
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(IV_VKBUK) TYPE  VKBUK OPTIONAL
*"     REFERENCE(IV_DTINIJUROS) TYPE  SYDATUM OPTIONAL
*"     REFERENCE(IV_JUROS_ANO) TYPE  VVWBEKPZP DEFAULT '18.00'
*"     REFERENCE(IV_TAXA_MULTA) TYPE  ZDE003 DEFAULT '0.00'
*"     REFERENCE(IV_DT_PREVPGTO) TYPE  SYDATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(EV_CENAR_FAT) TYPE  NUMC2
*"     REFERENCE(EV_CENAR_LIQ) TYPE  NUMC2
*"     REFERENCE(ES_POPUP_VALUES) TYPE  ZSDS078
*"  TABLES
*"      ET_TRAVAS STRUCTURE  ZSDT0090 OPTIONAL
*"  EXCEPTIONS
*"      FAT_INCOMPLETO
*"----------------------------------------------------------------------

  DATA lv_vlr_ov TYPE netwr.
  DATA lv_vlr_impo TYPE netwr.
  DATA lv_mont_moeda TYPE netwr.
  DATA lv_tp_fat TYPE zsde_status_faturamento.

  CHECK iv_vbeln IS NOT INITIAL.

  CLEAR es_popup_values.

  CLEAR: gv_dtprev_pag_a, gv_taxa_a, gv_dtvenc.

  gv_dtprev_pag_a = iv_dt_prevpgto.
  "gv_dtvenc = iv_dt_prevpgto.

  PERFORM f_get_tax_from_ov
    USING iv_vbeln
 CHANGING lv_vlr_ov
          lv_vlr_impo
          lv_mont_moeda.

  PERFORM f_get_zsdt0090
    USING iv_vbeln
 CHANGING es_popup_values-c_trav_cam
          et_travas[].

  es_popup_values-doc_simulacao = iv_doc_simulacao.
  es_popup_values-vbelv = iv_vbeln.
  es_popup_values-posnr = iv_posnr.
  es_popup_values-matnr = iv_matnr.
  es_popup_values-werks = iv_werks.
  es_popup_values-dtinijuros = iv_dtinijuros.
  es_popup_values-juros_ano = iv_juros_ano.
  es_popup_values-tx_multa = iv_taxa_multa.

  es_popup_values-bukrs = iv_bukrs.
  es_popup_values-vkbur = iv_vkbuk.

  PERFORM f_check_status_fat
    USING iv_vbeln
 CHANGING lv_tp_fat.

  " se tiver parcial, preenche colunas de faturamento
  IF lv_tp_fat = 'P'.

    PERFORM f_get_vlr_faturado
      USING iv_vbeln
   CHANGING es_popup_values-tt_fat
            es_popup_values-tt_afat
            es_popup_values-qtde_fat
            es_popup_values-qtde_afat.

    IF es_popup_values-tt_fat IS INITIAL
       OR es_popup_values-tt_afat IS INITIAL.

      MESSAGE e105(zsd) RAISING fat_incompleto.
      EXIT.

    ENDIF.

  ENDIF.

  SELECT SINGLE kurrf valdt FROM vbkd
    INTO (gv_taxa_a, gv_dtvenc)
      WHERE vbeln EQ iv_vbeln.

  " só mostra a taxa quando selecionar
  CLEAR gv_taxa_a.

  es_popup_values-vbeln = iv_vbeln.
  es_popup_values-vlr_ov = lv_vlr_ov + lv_vlr_impo.
  es_popup_values-tt_liq = lv_mont_moeda.

  es_popup_values-tt_aliq = es_popup_values-vlr_ov - es_popup_values-tt_liq.

  es_popup_values-s_trav_cam = es_popup_values-vlr_ov - es_popup_values-tt_liq - es_popup_values-c_trav_cam.

  PERFORM f_global_to_screen
    CHANGING es_popup_values-dtvenc
             es_popup_values-taxa_a
             es_popup_values-dtprevpag_a.

*  PERFORM f_get_taxa_curv_proj
*    USING es_popup_values-dtvenc
* CHANGING es_popup_values-taxa_curv_proj.
*
*  es_popup_values-taxa_neg = es_popup_values-taxa_curv_proj.

  CASE lv_tp_fat.

      "---------------------------------Cenario 01 - OV 0% Faturada
    WHEN space.
      es_popup_values-cenario = ev_cenar_fat = '01'.

      "---------------------------------Cenario 02 - OV Parcialmente Faturada
    WHEN 'P'.
      es_popup_values-cenario = ev_cenar_fat = '02'.

      "---------------------------------Cenario 03 - OV 100% Faturada
    WHEN 'C'.
      es_popup_values-cenario = ev_cenar_fat = '03'.
  ENDCASE.

  " 100% liquidada

  " 24.10.2024 - RAMON -->
  IF es_popup_values-tt_liq >= es_popup_values-vlr_ov.
    "IF es_popup_values-vlr_ov = es_popup_values-tt_liq. " comentamos pq o tt_liq esta maior que o vlr_ov - OV: 0012811093
    " 24.10.2024 - RAMON --<
    ev_cenar_liq = '03'.

    " parcialmente liquidada
  ELSEIF es_popup_values-tt_liq IS NOT INITIAL.
    ev_cenar_liq = '02'.

    " 0% liquidada
  ELSEIF es_popup_values-tt_liq IS INITIAL.
    ev_cenar_liq = '01'.
  ENDIF.

  PERFORM f_get_auth_filial
    USING es_popup_values-werks
 CHANGING es_popup_values-acesso_filial.

*  CALL FUNCTION 'ZSDMF_SELEC_REG_ZSDT0315'
*    EXPORTING
*      iv_vbeln     = iv_vbeln
*    IMPORTING
*      ev_total_fat = es_popup_values-tt_liq_0.

ENDFUNCTION.
