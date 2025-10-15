FUNCTION zmm_nfse_exibe_nf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EDIT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(I_TOLER) TYPE  /TCSR/E_TOLERANCE_TAX
*"     REFERENCE(IT_COND_ITEMS) TYPE  /TCSR/Y_COND_LIST
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"  CHANGING
*"     REFERENCE(C_HEADER) TYPE  ZSHEADER_DATA_NFSE_INBOUND
*"     REFERENCE(C_PAYMENT) TYPE  ZSPAYMENT_DATA_NFSE_INBOUND
*"     REFERENCE(C_NFSE) TYPE  ZIBS_NFSE_001
*"  EXCEPTIONS
*"      NOTA_DUPLICADA
*"      NOTA_DUPLIC_RECUSADA
*"----------------------------------------------------------------------

  DATA lt_po_tab TYPE zibc_nfse_pedidos.

  "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
  IF i_bukrs IS NOT INITIAL.
    wa_bukrs_calendary = i_bukrs.
  ENDIF.
  "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim

  gv_edit_2000 = i_edit.

  PERFORM f_refresh_all.

  go_znfse = NEW zcl_nfse_inbound( c_nfse-guid_header ).

*  PERFORM f_data_vencimento CHANGING c_payment.

  zsheader_data_nfse_inbound = c_header.
  zibs_nfse_001 = c_nfse.
  zspayment_data_nfse_inbound = c_payment.
  gt_scr_condition_list = it_cond_items.

  gv_header_old = c_header.
  gv_paymen_old = c_payment.

  PERFORM f_valores_sap.

  gv_total_tolerance = i_toler.

  "IF go_znfse->check_authorization( ) = 'X'.

  "ENDIF.

  "gt_scr_popup_list,
  "gt_scr_popup_search.

  CHECK go_znfse IS BOUND.

  "PERFORM f_po_associados CHANGING lt_po_tab.

  PERFORM f_atualiza_campos
    USING c_nfse
 CHANGING zsheader_data_nfse_inbound
          zspayment_data_nfse_inbound.

  PERFORM f_preenche_nfse_002 USING zibs_nfse_001.

  PERFORM f_total_znfse CHANGING zspayment_data_nfse_inbound-ktwrt.

  PERFORM f_get_obs_xml USING c_nfse-guid_header.

  PERFORM f_dados_banco_tela
    USING zsheader_data_nfse_inbound
 CHANGING zspayment_data_nfse_inbound.

  " 31.03.2023 - 107971 - RBL -->
  PERFORM f_processa_retem USING space.
  " 31.03.2023 - 107971 - RBL --<

  CALL SCREEN 2000.

  IF zibt_nfse_001 IS NOT INITIAL.
    MOVE-CORRESPONDING zibt_nfse_001 TO c_nfse.
  ENDIF.

  PERFORM f_global_nfse_check USING zibt_nfse_001.

  " DEVOLVE COM AS ALTERAÇÕES FEITAS
  "c_nfse = zibs_nfse_001.
  c_header = zsheader_data_nfse_inbound.
  c_payment = zspayment_data_nfse_inbound.

ENDFUNCTION.
