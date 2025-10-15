FUNCTION zsdmf_popup_trava_cambio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_EDIT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_OPT_FAT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IT_FIELDS_DESAB) TYPE  ZSDC082 OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  FLAG
*"  TABLES
*"      CT_TRAVAS STRUCTURE  ZSDT0090
*"  CHANGING
*"     REFERENCE(CS_POPUP_VALUES) TYPE  ZSDS078
*"----------------------------------------------------------------------

  DATA lv_numc TYPE c LENGTH 4.

  PERFORM f_refresh_9000.

  gv_edit_9000 = iv_edit.
  gv_fatu_9000 = iv_opt_fat.

  gt_desab = it_fields_desab.

  zsds078 = cs_popup_values.

  LOOP AT ct_travas ASSIGNING FIELD-SYMBOL(<fs_travas>).

    gv_exibe_travas = 'X'.

    APPEND INITIAL LINE TO gt_alv_9000 ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-vbeln = <fs_travas>-vbelv.

    UNPACK  <fs_travas>-sequencia TO lv_numc.
    <fs_alv>-sequencia = lv_numc.

*    UNPACK <fs_travas>-seq_trava TO lv_numc.
*    <fs_alv>-seq_trava = lv_numc.

    <fs_alv>-doc_simulacao = <fs_travas>-doc_simulacao.
    <fs_alv>-dtvenc = <fs_travas>-data_prevpgto.
    <fs_alv>-dtprevpag_a = <fs_travas>-data_prevpgto.
    <fs_alv>-dtprevpag_n = <fs_travas>-data_prevpgto.

    <fs_alv>-taxa_curv_proj = <fs_travas>-kurrf.
    <fs_alv>-taxa_a = <fs_travas>-kurrf.
    <fs_alv>-taxa_neg = <fs_travas>-kurrf.

    <fs_alv>-vlr_prev = <fs_travas>-prev_pgto_usd.
    <fs_alv>-juros_prev = <fs_travas>-prev_juros_usd.
    <fs_alv>-multa_prev = <fs_travas>-prev_multa_usd.
    <fs_alv>-vlr_liq_prev = <fs_travas>-prev_vl_liq_usd.
    " alterado 11.07- pq deve ser exibido o prev pgto usd convertido em brl
    "<fs_alv>-vlr_prev_brl = <fs_travas>-prev_vl_liq_usd * <fs_alv>-taxa_neg.
    <fs_alv>-vlr_prev_brl = <fs_travas>-prev_pgto_usd * <fs_alv>-taxa_neg.
    <fs_alv>-vlr_liq_brl = <fs_travas>-prev_vl_liq_usd * <fs_alv>-taxa_neg.

  ENDLOOP.

  SORT gt_alv_9000 BY dtprevpag_a ASCENDING.

  zsds078-exec_01 = 'X'. "<- primeira execução

  CALL SCREEN 9000 STARTING AT 20 1.

  IF gv_ucomm_9000 = 'CANC'.

    ev_canc = 'X'.

    CLEAR cs_popup_values.

  ELSE.

    cs_popup_values = zsds078.

  ENDIF.

ENDFUNCTION.
