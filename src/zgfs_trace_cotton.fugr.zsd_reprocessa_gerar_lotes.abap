FUNCTION zsd_reprocessa_gerar_lotes.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ID_CARGA) TYPE  ZID_CARGA
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_LGORT) TYPE  LGORT_D
*"     VALUE(I_ACHARG) TYPE  CHARG_D
*"     VALUE(I_SAFRA) TYPE  CHAR4
*"     VALUE(I_UCOMM) TYPE  SY-UCOMM
*"----------------------------------------------------------------------

  DATA: l_email TYPE char1,
        l_integ TYPE char1,
        l_repro TYPE char1.

  l_repro = COND #( WHEN i_ucomm = '&GERALOTE' THEN abap_true
                                               ELSE abap_false ).
  l_email = COND #( WHEN i_ucomm = '&EMAIL'    THEN abap_true
                                               ELSE abap_false ).
  l_integ = COND #( WHEN i_ucomm = '&INTEGRAR' THEN abap_true
                                               ELSE abap_false ).

*-----------------------------------
* geracao lote
*-----------------------------------
  SUBMIT zsdr0153_job WITH p_unico  = abap_true
                      WITH p_repro  = l_repro
                      WITH p_email  = l_email
                      WITH p_integ  = l_integ
                      WITH p_idcar  = i_id_carga
                      WITH p_matnr  = i_matnr
                      WITH p_werks  = i_werks
                      WITH p_lgort  = i_lgort
                      WITH p_acharg = i_acharg
                      WITH p_safra  = i_safra
                       AND RETURN.

ENDFUNCTION.
