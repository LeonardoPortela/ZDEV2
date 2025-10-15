FUNCTION zsd_insumos_assinatura_bry.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOC_SIMULACAO) TYPE  ZSDED003
*"     VALUE(I_TIPO_DOC) TYPE  ZTIPO_DOC
*"     VALUE(I_ID_DOCUMENTO) TYPE  ZID_DOCUMENTO
*"----------------------------------------------------------------------

  DATA: w_zsdt0310 TYPE zsdt0310,
        l_erro     TYPE char01,
        l_mesg     TYPE string.

  FREE: l_erro.

*-----------------------------
* veifica se ha pendecias de assinantes
*-----------------------------
  SELECT SINGLE *
           INTO w_zsdt0310
           FROM zsdt0310
          WHERE nr_venda     = i_doc_simulacao
            AND tipo_doc     = i_tipo_doc
            AND id_documento = i_id_documento
            AND status      <> '10'.

  CHECK sy-subrc = 0.

  SELECT *
    FROM zsdt0316
    INTO TABLE @DATA(t_0316)
   WHERE nr_doc_gerado    = @w_zsdt0310-nr_doc_gerado
     AND id_doc_agrupador = @w_zsdt0310-id_documento.

  IF sy-subrc <> 0.
    l_erro = abap_true.
    l_mesg = 'Não há assinantes para este Documento!'.
  ELSE.
    l_mesg = 'Não foi possivel enviar a Bry. Participantes :'.
    LOOP AT t_0316 INTO DATA(w_0316).
      IF w_0316-id_grupo IS INITIAL  AND w_0316-email IS INITIAL.
        l_erro = abap_true.
        l_mesg = l_mesg && w_0316-nome && ': sem e-mail |'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF l_erro = abap_true.
    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = i_doc_simulacao
                                    i_tipo_doc     = i_tipo_doc
                                    i_id_documento = i_id_documento
                                    i_tipo_msg     = 'E'
                                    i_mensagem     = l_mesg ).

    UPDATE zsdt0310  SET status           = '12'
                         data_doc_gerado  = sy-datum
                         hora_doc_gerado  = sy-uzeit
                   WHERE nr_venda         = i_doc_simulacao
                     AND tipo_doc         = i_tipo_doc
                     AND id_documento     = i_id_documento.
    COMMIT WORK AND WAIT.
    EXIT.
  ENDIF.

*-----------------------------
* Solicitar assinatura BRY
*-----------------------------
  TRY .
      zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
         )->set_solicitar_assinatura( EXPORTING i_nr_venda     = i_doc_simulacao
                                                i_tipo_doc     = i_tipo_doc
                                                i_id_documento = i_id_documento ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
    CATCH zcx_error INTO DATA(ex_error).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    l_mesg  = 'Ocorreu erro ao Solicitar Assinatura na Bry.'.

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = i_doc_simulacao
                                    i_tipo_doc     = i_tipo_doc
                                    i_id_documento = i_id_documento
                                    i_tipo_msg     = 'E'
                                    i_mensagem     = l_mesg ).

    UPDATE zsdt0310  SET status           = '12'
                         data_doc_gerado  = sy-datum
                         hora_doc_gerado  = sy-uzeit
                   WHERE nr_venda         = i_doc_simulacao
                     AND tipo_doc         = i_tipo_doc
                     AND id_documento     = i_id_documento.
  ELSE.
    l_mesg  = 'Assinatura Solicitada com Sucesso.'.

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = i_doc_simulacao
                                    i_tipo_doc     = i_tipo_doc
                                    i_id_documento = i_id_documento
                                    i_tipo_msg     = 'S'
                                    i_mensagem     = l_mesg ).

    UPDATE zsdt0310  SET status           = '03'
                         data_doc_gerado  = sy-datum
                         hora_doc_gerado  = sy-uzeit
                   WHERE nr_venda         = i_doc_simulacao
                     AND tipo_doc         = i_tipo_doc
                     AND id_documento     = i_id_documento.
  ENDIF.

*-----------------------------
* verifica se chave workflow foi recebida
*-----------------------------
  SELECT SINGLE chave_workflow
           INTO @DATA(_chave_workflow)
           FROM zsdt0314
          WHERE nr_doc_gerado    = @w_zsdt0310-nr_doc_gerado
            AND id_doc_agrupador = @w_zsdt0310-id_documento.

  IF sy-subrc = 0 AND _chave_workflow IS INITIAL.
    l_mesg  = 'Resposta da BRY sem sucesso para coleta de Assinaturas!'.

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = i_doc_simulacao
                                    i_tipo_doc     = i_tipo_doc
                                    i_id_documento = i_id_documento
                                    i_tipo_msg     = 'E'
                                    i_mensagem     = l_mesg ).

    UPDATE zsdt0310  SET status           = '12'
                         data_doc_gerado  = sy-datum
                         hora_doc_gerado  = sy-uzeit
                   WHERE nr_venda         = i_doc_simulacao
                     AND tipo_doc         = i_tipo_doc
                     AND id_documento     = i_id_documento.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
