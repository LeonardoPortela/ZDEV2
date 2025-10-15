FUNCTION zsd_insumos_gerar_doc_sigam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOC_SIMULACAO) TYPE  ZSDED003
*"     VALUE(I_TIPO_DOC) TYPE  ZTIPO_DOC
*"     VALUE(I_ID_DOCUMENTO) TYPE  ZID_DOCUMENTO
*"----------------------------------------------------------------------

  FREE: l_erro.

*-----------------------------
* Gerar documrnto SIGAM
*-----------------------------
  TRY .
      zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
         )->set_criar_documento( EXPORTING i_nr_venda     = i_doc_simulacao
                                           i_tipo_doc     = i_tipo_doc
                                           i_id_documento = i_id_documento ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
    CATCH zcx_error INTO DATA(ex_error).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    UPDATE zsdt0310  SET status           = '11'
                         tipo_doc_digital = abap_off
                         data_doc_gerado  = sy-datum
                         hora_doc_gerado  = sy-uzeit
                   WHERE nr_venda         = i_doc_simulacao
                     AND tipo_doc         = i_tipo_doc
                     AND id_documento     = i_id_documento.

    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
