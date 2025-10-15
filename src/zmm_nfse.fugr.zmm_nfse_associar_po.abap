FUNCTION zmm_nfse_associar_po.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_HEADER) TYPE  ZSHEADER_DATA_NFSE_INBOUND
*"     REFERENCE(I_NFSE) TYPE  ZIBS_NFSE_001
*"  CHANGING
*"     REFERENCE(CT_NAO_ASSOCIADOS) TYPE  ZIBC_NFSE_PEDIDOS
*"     REFERENCE(CT_ASSOCIADOS) TYPE  ZIBC_NFSE_PEDIDOS
*"----------------------------------------------------------------------
  CLEAR: gt_nfse_002, gt_scr_popup_search, gt_scr_popup_list, gt_associated_po.

  gt_scr_popup_search = ct_nao_associados .
  gt_scr_popup_list = ct_associados .
  gt_associated_po = ct_associados .

  zibs_nfse_001 = i_nfse.
  zsheader_data_nfse_inbound = i_header.

  PERFORM f_preenche_nfse_002 USING i_nfse.

  READ TABLE gt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>)
    WITH KEY pstyp = '9'.

  IF sy-subrc EQ 0.

    PERFORM f_get_po_serv
      USING i_nfse-guid_header
            <fs_nfse_002>-ebeln
            <fs_nfse_002>-ebelp
   CHANGING gt_service.

  ENDIF.

  PERFORM f_atualiza_totais_3000.

  CALL SCREEN 3000 STARTING AT 20 1.

*  IF gv_save_3010 IS INITIAL.
*
*    gv_save_3010 = 'X'.
*
*    PERFORM f_save_serv.
*
*  ENDIF.

  ct_nao_associados = gt_scr_popup_search.
  ct_associados = gt_associated_po.

ENDFUNCTION.
