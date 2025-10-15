FUNCTION zsdmf001_reverte_boleta_api.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  CHANGING
*"     REFERENCE(CS_REQUEST) TYPE  ZSDT0308
*"  EXCEPTIONS
*"      FALHA_COMUNICACAO
*"----------------------------------------------------------------------

  DATA lo_sigam TYPE REF TO zcl_int_sigam_hedge_reverter.

  DATA lw_dados TYPE zsde0024.

  CLEAR ev_erro.  "*-Equalização RISE x PRD - 19.07.2023 - JT

  TRY.

      lw_dados-identificacao = cs_request-ident.
      lw_dados-nrdocexterno = cs_request-doc_simu.
      lw_dados-valor = cs_request-vlr_brl_enviado.

      PERFORM f_data_sigam USING cs_request-data_venc CHANGING  lw_dados-dtvencimento.

      lw_dados-comentario = cs_request-coment.

      DATA(lt_dados) = zcl_int_sigam_hedge_reverter=>zif_int_sigam_reverter_hedge~get_instance( )->enviar_sigam( lw_dados ).

      cs_request-ident_rev_sigam = VALUE #( lt_dados[ 1 ]-identificaoreversa DEFAULT '' ).

      IF cs_request-ident_rev_sigam IS INITIAL.

        ev_erro = abap_true.  "*-Equalização RISE x PRD - 19.07.2023 - JT

        MESSAGE e103(zsd) RAISING falha_comunicacao.

      ENDIF.

    CATCH zcx_integracao .
    CATCH zcx_error .
  ENDTRY.

ENDFUNCTION.
