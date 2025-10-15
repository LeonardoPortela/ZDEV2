FUNCTION zsd_popup_lotes_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ORDEM_CARGA) TYPE  ZSDT0304
*"----------------------------------------------------------------------

  FREE: g_custom_container,
        g_custom_container2,
        g_grid,
        g_grid2,
        g_grid3,
        t_lotes,
        t_hist,
        l_erro.

  w_ordem_carga = i_ordem_carga.

  IF w_ordem_carga-lock   = abap_true OR
     w_ordem_carga-status = 2.
    l_edit = abap_false.
  ELSE.
    l_edit = abap_true.
  ENDIF.

*-----------------------------
*-parametrizacao material
*-----------------------------
  PERFORM f_recupera_parametros USING w_ordem_carga-vbeln
                                      w_ordem_carga-matnr
                             CHANGING l_qtd_dias_vencimento
                                      l_bloq_lotes_vencidos
                                      l_lote_vencmto_proximo
                                      l_regra_vencto.

*-----------------------------
*-carrega lotes
*-----------------------------
  PERFORM f_selecao_dados       USING w_ordem_carga.

*-----------------------------
*-exibir lotes
*-----------------------------
  CALL SCREEN 0100 STARTING AT 03  01
                     ENDING AT 186 22.

ENDFUNCTION.
