FUNCTION zsd_envio_ordem_venda_trace.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NRO_SOL_OV) TYPE  ZSDED013
*"     VALUE(I_POSNR) TYPE  POSNR_VA
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"     VALUE(I_ACAO) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

  FREE: l_error, l_mesg.

  CASE i_acao.

    WHEN 'C'.
*-----------------------------
*---- cadastra Ordem VEnda no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_cadastra_ordem_venda( EXPORTING i_nro_sol_ov  = i_nro_sol_ov
                                                    i_posnr       = i_posnr ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
          l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

        CATCH zcx_error INTO DATA(ex_error).
          l_error = abap_true.
          l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
      ENDTRY.

    WHEN 'E'.
*-----------------------------
*---- exclui Ordem VEnda no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_exclui_ordem_venda( EXPORTING i_nro_sol_ov  = i_nro_sol_ov
                                                  i_posnr       = i_posnr
                                                  i_vbeln       = i_vbeln ).

        CATCH zcx_integracao INTO ex_integra.
          l_error = abap_true.
          l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

        CATCH zcx_error INTO ex_error.
          l_error = abap_true.
          l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
      ENDTRY.

  ENDCASE.

* IF l_error = abap_true.
*   zcl_trace_cotton=>zif_trace_cotton~get_instance(
*      )->set_gravar_log( EXPORTING i_zseq_inst   = i_zseq_inst
*                                   i_objek       = i_objek
*                                   i_objecttable = i_objecttable
*                                   i_tipo_msg    = 'E'
*                                   i_mensagem    = l_mesg ).
*   COMMIT WORK AND WAIT.
* ENDIF.

ENDFUNCTION.
