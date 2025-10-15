FUNCTION zsd_ov_ztro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_TKNUM) TYPE  VTTK-TKNUM OPTIONAL
*"     VALUE(P_TDLNR) TYPE  VTTK-TDLNR OPTIONAL
*"     VALUE(P_ADD03) TYPE  VTTK-ADD03 OPTIONAL
*"     REFERENCE(P_SEM_MENSAGEM) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(P_FATURAMENTO_AUTOM) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     VALUE(P_SALES) TYPE  VBAK-VBELN
*"     VALUE(P_FAT) TYPE  VBAK-VBELN
*"     VALUE(T_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

*-#133089-21.02.2024-JT-inicio
  vg_faturamento_autom  = p_faturamento_autom.
*-#133089-12.02.2024-JT-fim

* Seleciona Dados
  PERFORM: z_seleciona_dados USING p_tknum
                                   p_tdlnr
                                   p_add03,

* Cria OV
           z_cria_ov  USING p_tknum,
* Retorna Menssagens
           z_ret_msn  USING p_sem_mensagem.

  t_return[] = t_bapiret2[].
  p_sales    = v_vbeln.
  p_fat      = v_fat.

ENDFUNCTION.
