FUNCTION zsd_reprocessa_transferencia.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ID_CARGA) TYPE  ZID_CARGA
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_LGORT) TYPE  LGORT_D
*"     VALUE(I_ACHARG) TYPE  CHARG_D
*"     VALUE(I_SAFRA) TYPE  CHAR4
*"----------------------------------------------------------------------

*-----------------------------------
* status fardo
*-----------------------------------
  UPDATE zsdt0330 SET status_fardo = '1'
                WHERE id_carga     = i_id_carga
                  AND matnr        = i_matnr
                  AND werks        = i_werks
                  AND lgort        = i_lgort
                  AND acharg       = i_acharg
                  AND safra        = i_safra
                  AND cancelado    = abap_off.

  COMMIT WORK AND WAIT.

*-----------------------------------
* executa transferencia
*-----------------------------------
  SUBMIT zsdr0152_job WITH p_unico  = abap_true
                      WITH p_idcar  = i_id_carga
                      WITH p_matnr  = i_matnr
                      WITH p_werks  = i_werks
                      WITH p_lgort  = i_lgort
                      WITH p_acharg = i_acharg
                      WITH p_safra  = i_safra
                       AND RETURN.

ENDFUNCTION.
