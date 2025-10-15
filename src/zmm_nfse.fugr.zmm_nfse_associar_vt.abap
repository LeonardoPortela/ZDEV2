FUNCTION zmm_nfse_associar_vt.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_NFSE_001) TYPE  ZIBS_NFSE_001 OPTIONAL
*"     REFERENCE(IV_TKNUM) TYPE  TKNUM OPTIONAL
*"----------------------------------------------------------------------

  PERFORM f_refresh_4000.

  gs_nfse_4000 = is_nfse_001.

  zibs_nfps_001-tknum = iv_tknum.

  PERFORM f_preenche_data CHANGING so_datr[].

  CALL SCREEN 4000 STARTING AT 20 1.

ENDFUNCTION.
