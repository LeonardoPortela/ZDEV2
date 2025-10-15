FUNCTION z_compra_cfop_entradas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CFOPS STRUCTURE  LXHME_RANGE_C10
*"----------------------------------------------------------------------

*"// CFOP Comercialização
  zcl_im_cl_fluxo_exportacao=>get_cfop(
    EXPORTING
      i_comercializacao = abap_true
    RECEIVING
      r_cfop            = cfops[]
  ).

*"// wbarbosa 18112024 US-158263 PROJETO 1X1 comentado para aplicar nova tabela de parametro.
*  DATA: wa_setleaf TYPE setleaf,
*        wa_cfops   TYPE lxhme_range_c10,
*        it_setleaf LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.
*
*  CLEAR: cfops[], it_setleaf[].
*
*  "Opter Área de contabilidade de custos
*  SELECT * INTO TABLE it_setleaf
*    FROM setleaf
*   WHERE setname EQ 'ZCOMPRA_COM_CFOP'.
*
*  LOOP AT it_setleaf INTO wa_setleaf.
*    wa_cfops-sign   = 'I'.
*    wa_cfops-option = 'EQ'.
*    wa_cfops-low    = wa_setleaf-valfrom.
*    wa_cfops-high   = wa_setleaf-valfrom.
*    APPEND wa_cfops TO cfops.
*  ENDLOOP.
*"// wbarbosa 18112024 US-158263 PROJETO 1X1 comentado para aplicar nova tabela de parametro.

ENDFUNCTION.
