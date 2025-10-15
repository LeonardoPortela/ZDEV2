FUNCTION zsd_int_ob_integra_tracking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NRO_CG) TYPE  ZNRO_CG
*"     VALUE(I_ENDPOINT) TYPE  STRING
*"  EXCEPTIONS
*"      ERRO_INTEGRACAO
*"----------------------------------------------------------------------

  ls_tracking-nro_cg   = i_nro_cg.
  ls_tracking-metodo   = 'POST'.
  ls_tracking-endpoint = i_endpoint.

*----------------------------------------------------
* integracao TRANSPORT / ANEXOS
*----------------------------------------------------
  TRY.
      zcl_int_ob_safra_crt_crg_track=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_tracking
                                                                                                IMPORTING e_integracao   = ls_retorno ).
    CATCH zcx_integracao INTO DATA(ex_integra).
      RAISE erro_integracao.
    CATCH zcx_error      INTO DATA(ex_error).
      RAISE erro_integracao.
  ENDTRY.

ENDFUNCTION.
