FUNCTION z_inves_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS
*"     VALUE(I_EMPRESA) TYPE  BUKRS
*"     VALUE(I_DT_INICIAL) TYPE  DATUM
*"     VALUE(I_DT_FINAL) TYPE  DATUM
*"  TABLES
*"      POSNR STRUCTURE  ZSDS_POSNR_RANGE OPTIONAL
*"      RESULTADO_TOT STRUCTURE  ZSYS_INVES2 OPTIONAL
*"----------------------------------------------------------------------

  FREE: resultado_tot.

  CALL FUNCTION 'Z_INVES_SYSPHERA_REPORT'
    EXPORTING
      i_area        = i_area
      i_empresa     = i_empresa
      i_dt_inicial  = i_dt_inicial
      i_dt_final    = i_dt_final
    TABLES
      posnr         = posnr
      resultado_tot = resultado_tot.

ENDFUNCTION.
