*----------------------------------------------------------------------*
***INCLUDE LZMMG001F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  YF_MOV_ESTOQUE_ESTORNO
*&---------------------------------------------------------------------*
*   Verifica movimentações de estoque / estorno
*----------------------------------------------------------------------*
*      -->P_TABLE  Tabela de entrada
*      -->P_ESTRUC Estrutura
*----------------------------------------------------------------------*
form yf_mov_estoque_estorno  tables p_table  type standard table
                             using  p_estruc type any.

  loop at p_table into p_estruc.

    clear vg_index.
    vg_index = sy-tabix.

*   Preenche a estrutura com os valores de entrada
    if not it_zmmt_ee_zgr[] is initial.
      vg_obj_key = wa_mov_estq-obj_key.
    else.
      vg_obj_key = wa_estorno-obj_key.
    endif.

*   Objeto de Bloqueio para o campo OBJ_KEY
    call function 'ENQUEUE_EZMMT001'
      exporting
        mode_zmmt_ee_zgr = c_s
        mandt            = sy-mandt
        obj_key          = vg_obj_key
      exceptions
        foreign_lock     = 1
        system_failure   = 2
        others           = 3.

*   Se o objeto estiver sendo utilizado, lê o próximo registro
    if sy-subrc is initial.
      if wa_mov_estq-st_atualizacao eq c_i.
*       Inclusão
        perform yf_bapi_goodsmvt_create.
      else.
*       Estorno
        perform yf_bapi_goodsmvt_cancel.
      endif.
    endif.

    if it_return[] is initial.
*     Execução da BAPI realizada com sucesso
      perform yf_grava_dados using 1.
    else.
*     Verifica se a BAPI foi realizada com sucesso
      read table it_return into wa_return
                           with key type = c_s
                           binary search.

      if sy-subrc is initial.
*       Execução da BAPI realizada com sucesso
        perform yf_grava_dados using 1.
      else.
*       Execução da BAPI que não foi realizada com sucesso
        perform yf_grava_dados using 0.
      endif.
    endif.

    loop at it_return into wa_return.
      perform yf_z_fi_outbound_return using vg_obj_key
                                            wa_return-type
                                            wa_return-message.
    endloop.
  endloop.

endform.                    " YF_MOV_ESTOQUE_ESTORNO

*&---------------------------------------------------------------------*
*&      Form  YF_BAPI_GOODSMVT_CREATE
*&---------------------------------------------------------------------*
*   Chamada da função BAPI_GOODSMVT_CREATE
*----------------------------------------------------------------------*
form yf_bapi_goodsmvt_create .

  clear wa_header.
  wa_header-doc_date   = wa_mov_estq-doc_date.
  wa_header-pstng_date = wa_mov_estq-pstng_date.
  wa_header-ref_doc_no = wa_mov_estq-nt_remessa.

  clear vg_gmcode.
  vg_gmcode = c_01.

  wa_item-move_type    = wa_mov_estq-move_type.
  wa_item-po_item      = wa_mov_estq-nu_item.
  wa_item-po_pr_qnt    = wa_mov_estq-entry_qnt.
  wa_item-po_number    = wa_mov_estq-po_number.
  wa_item-amount_lc    = wa_mov_estq-amount_lc.
  append wa_item to it_item.
  clear wa_item.

  append wa_serialnumber to it_serialnumber.

  call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    exporting
      goodsmvt_header       = wa_header
      goodsmvt_code         = vg_gmcode
*    IMPORTING
*      goodsmvt_headret = wa_head_ret
*      materialdocument = wa_mat_doc
*      matdocumentyear  = wa_doc_year
    tables
      goodsmvt_item         = it_item
      goodsmvt_serialnumber = it_serialnumber
      return                = it_return.

endform.                    " YF_BAPI_GOODSMVT_CREATE

*&---------------------------------------------------------------------*
*&      Form  YF_BAPI_GOODSMVT_CANCEL
*&---------------------------------------------------------------------*
*   Chamada da função BAPI_GOODSMVT_CANCEL
*----------------------------------------------------------------------*
form yf_bapi_goodsmvt_cancel .

  clear wa_mat_doc2.
  wa_mat_doc2-mat_doc    = wa_estorno-mblnr.
  wa_mat_doc2-doc_year   = wa_estorno-mjahr.
  wa_mat_doc2-pstng_date = wa_estorno-budat.

  call function 'BAPI_GOODSMVT_CANCEL'
    exporting
      materialdocument    = wa_mat_doc2-mat_doc
      matdocumentyear     = wa_mat_doc2-doc_year
      goodsmvt_pstng_date = wa_mat_doc2-pstng_date
    importing
      goodsmvt_headret    = wa_head_ret
    tables
      return              = it_return.

endform.                    " YF_BAPI_GOODSMVT_CANCEL

*&---------------------------------------------------------------------*
*&      Form  YF_GRAVA_DADOS
*&---------------------------------------------------------------------*
*   Grava dados na tabela ZMMT_PO_EEZGR e movimenta as informações
*----------------------------------------------------------------------*
*      -->P_ZRG_ATLZ      RG_ATUALIZADO
*----------------------------------------------------------------------*
form yf_grava_dados  using  p_zrg_atlz type any.

  data: wa_zmmt_eee_zgr type zmmt_eee_zgr,
        wa_zmmt_ee_zgr  type zmmt_ee_zgr.

  if not it_zmmt_ee_zgr[] is initial.
    wa_mov_estq-zrg_atlz = p_zrg_atlz.
    move-corresponding wa_mov_estq to wa_zmmt_ee_zgr.
    modify it_zmmt_ee_zgr from wa_zmmt_ee_zgr index vg_index.
  endif.

  if not it_zmmt_eee_zgr[] is initial.
    wa_estorno-rg_atualizado = p_zrg_atlz.
    move-corresponding wa_estorno to wa_zmmt_eee_zgr.
    modify it_zmmt_eee_zgr from wa_zmmt_eee_zgr index vg_index.
  endif.

endform.                    " YF_GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Form  YF_Z_FI_OUTBOUND_RETURN
*&---------------------------------------------------------------------*
*   Chamada da RFC Z_FI_OUTBOUND_RETURN
*----------------------------------------------------------------------*
*      -->P_TYPE      Tipo da Mensagem
*      -->P_MESSAGE   Mensagem
*-------------------------------------------------------------
form yf_z_fi_outbound_return using p_obj_key type any
                                   p_type    type any
                                   p_message type any.


  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = c_11.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = p_type.
  wa_outreturn-id             = c_mm.
  wa_outreturn-num            = c_899.
  wa_outreturn-message        = p_message.

  append wa_outreturn to it_outreturn.
  clear wa_outreturn.

endform.                    " YF_Z_FI_OUTBOUND_RETURN

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_DBC
*&---------------------------------------------------------------------*
*       Preenche tabela BDCData
*----------------------------------------------------------------------*
form z_preenche_dbc  tables it_bdcdata structure bdcdata
                     using  p_dynbegin type any
                            p_name     type any
                            p_value    type any.

  data: wa_bdcdata type bdcdata.

  if p_dynbegin = 'X'.
    move: p_name      to wa_bdcdata-program,
          p_value     to wa_bdcdata-dynpro,
          p_dynbegin  to wa_bdcdata-dynbegin.
    append wa_bdcdata to it_bdcdata.
  else.
    move: p_name      to wa_bdcdata-fnam,
          p_value     to wa_bdcdata-fval.
    append wa_bdcdata to it_bdcdata.
  endif.
  clear: wa_bdcdata.
endform.                    " Z_PREENCHE_DBC


*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM2
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
form z_prepara_mensagem  tables it_out structure zfie_ret_document
                          using p_obj_key    type any
                                p_type       type any
                                p_message    type any
                                p_message_v1 type any
                                p_message_v2 type any
                                p_interface  type any.

  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = p_interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = p_type.
  wa_outreturn-id             = c_mm.
  wa_outreturn-num            = c_899.
  wa_outreturn-message        = p_message.
  wa_outreturn-message_v1     = p_message_v1.
  wa_outreturn-message_v2     = p_message_v2.

  append wa_outreturn to it_out.
  clear wa_outreturn.

endform.                    " Z_PREPARA_MENSAGEM2

*&---------------------------------------------------------------------*
*&      Form  ADD_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MSG  text
*      -->P_C_10  text
*----------------------------------------------------------------------*
form add_mensagem  tables it_out structure zfie_ret_document
                    using wa_mov_estq type  zmmt_ee_zgr
                          it_msg type bdcmsgcoll
                          p_interface
                          p_desc_nf.

  data: vg_return type string.

  call function 'MESSAGE_PREPARE'
    exporting
      msg_id                 = it_msg-msgid
      msg_no                 = it_msg-msgnr
      msg_var1               = it_msg-msgv1(50)
      msg_var2               = it_msg-msgv2(50)
      msg_var3               = it_msg-msgv3(50)
      msg_var4               = it_msg-msgv4(50)
    importing
      msg_text               = vg_return
    exceptions
      function_not_completed = 1
      message_not_found      = 2
      others                 = 3.

  perform z_prepara_mensagem tables it_out
              using wa_mov_estq-obj_key it_msg-msgtyp vg_return it_msg-msgv2 p_desc_nf p_interface.

endform.                    " ADD_MENSAGEM
