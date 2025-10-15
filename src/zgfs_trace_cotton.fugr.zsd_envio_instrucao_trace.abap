FUNCTION zsd_envio_instrucao_trace.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ZSEQ_INST) TYPE  ZSEQ_INST
*"     VALUE(I_OBJEK) TYPE  OBJNUM
*"     VALUE(I_OBJECTTABLE) TYPE  TABELLE
*"     VALUE(I_ACAO) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

  FREE: l_error, l_mesg.

  CASE i_acao.

    WHEN 'C'.
*-----------------------------
*---- cadastra instrucao no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_cadastra_instrucao( EXPORTING i_zseq_inst   = i_zseq_inst
                                                  i_objek       = i_objek
                                                  i_objecttable = i_objecttable ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
          l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

        CATCH zcx_error INTO DATA(ex_error).
          l_error = abap_true.
          l_mesg  = ex_error->msgv1   && '-' && ex_error->msgv2   && '-' && ex_error->msgv3   && '-' && ex_error->msgv4.
      ENDTRY.

    WHEN 'E'.
*-----------------------------
*---- exclui instrucao no Trace
*-----------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_exclui_instrucao( EXPORTING i_zseq_inst   = i_zseq_inst
                                                i_objek       = i_objek
                                                i_objecttable = i_objecttable ).

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
