FUNCTION zsd_troca_nota_upload.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZSDT0001) TYPE  ZDE_LES_SAIDA_ZSDT0001_TN OPTIONAL
*"  EXPORTING
*"     VALUE(E_ST_PROC) TYPE  ZST_PROC
*"     VALUE(E_DOCS_ENVIADO_CARGUERO) TYPE  CHAR1
*"  EXCEPTIONS
*"      ERRO_UPLOAD
*"      ERRO_UPLOAD_NAO_AUTORIZADO
*"      ERRO_ROMANEIO_NAO_TROCANOTA
*"      ERRO_APROVACAO_CARREGA
*"----------------------------------------------------------------------

  FREE: it_file_table,
        it_file_table_tmp,
        it_pdffiles,
        t_pdf_files,
        it_file_local,
        pdf_merger,    l_url_upload, merged_document,
        docindex,      errordoc,     rc,           l_tem_mdfe,
        wa_zsdt0001,   l_id_viagem,  l_vbeln_vttp, l_vbeln_likp,
        l_file01,      l_file02,     l_file03,
        l_file04,      l_file05,     l_file06,
        l_file07,      l_file08,     l_file09,
        l_file10,      lva_declaracao,
        l_tipo01,      l_tipo02,     l_tipo03,
        l_tipo04,      l_tipo05,     l_tipo06,
        l_tipo07,      l_tipo08,     l_tipo09,
        l_tipo10,
        l_docs_gerados, t_doctos_faltantes, l_mensagem,      l_lifnr,
        l_danfe,        l_dacte,            l_contrato,      l_mdfe,
        e_st_proc,      l_naotem_doc,       l_abandona_tela, l_nome_usuario,
        w_address,      t_return,           w_return,        l_msg_texto,
        e_docs_enviado_carguero.

*-----------------------------------------
* nome usuario - mensagem confirmacao
*-----------------------------------------
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = sy-uname
    IMPORTING
      address  = w_address
    TABLES
      return   = t_return.

  READ TABLE t_return INTO w_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    l_nome_usuario = sy-uname.
  ELSE.
    l_nome_usuario = w_address-fullname.
  ENDIF.

  l_msg_texto = l_nome_usuario && text-100.

*-----------------------------------------
* referencia
*-----------------------------------------
  SELECT *
    INTO wa_zsdt0001
    FROM zsdt0001
      UP TO 1 ROWS
   WHERE ch_referencia = i_zsdt0001-ch_referencia.
  ENDSELECT.

  CHECK sy-subrc = 0.

*-----------------------------------------
* saida
*-----------------------------------------
  l_st_proc               = wa_zsdt0001-st_proc.
  l_docs_enviado_carguero = wa_zsdt0001-docs_enviado_carguero.
  e_st_proc               = wa_zsdt0001-st_proc.
  e_docs_enviado_carguero = wa_zsdt0001-docs_enviado_carguero.

*-----------------------------------------
* valida transporte
*-----------------------------------------
  SELECT id_viagem
    INTO l_id_viagem
    FROM vttk
      UP TO 1 ROWS
   WHERE tknum = wa_zsdt0001-doc_transp.
  ENDSELECT.

  CHECK sy-subrc = 0.

  CLEAR: gwa_zlest0185.

  SELECT SINGLE *
    FROM zlest0185 INTO gwa_zlest0185
   WHERE viagem_id EQ l_id_viagem.

  CHECK l_id_viagem IS NOT INITIAL AND sy-subrc EQ 0.

*------------------------------------------
*-inicio transporte Troca Nota
*------------------------------------------
  IF zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
       EXPORTING i_ch_referencia = wa_zsdt0001-ch_referencia ) = abap_false.
    RAISE erro_romaneio_nao_trocanota.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguardando aprovação do Carguero...'.

*-----------------------------------------
* autorizacao upload
*-----------------------------------------


  TRY .

      DATA(lva_url) = zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_url_documento( EXPORTING i_viagem_id  = l_id_viagem ).

      zcl_integracao_upload_auth=>zif_integracao_upload_auth~get_instance(
        )->set_empresa( EXPORTING i_bukrs = gwa_zlest0185-bukrs
        )->set_id_referencia( EXPORTING i_id_referencia = gwa_zlest0185-viagem_id
        )->set_blob_path( EXPORTING i_blob_path = CONV #( lva_url )
        )->get_json(      IMPORTING e_json          = DATA(lc_json_tn)
        )->set_ds_data(   EXPORTING i_json          = lc_json_tn
        )->set_ds_url(
        )->set_send_msg(  IMPORTING e_id_integracao = l_id_integracao
                                    e_url_upload    = l_url_upload
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao_tn).
      IF NOT ( ex_integracao_tn->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
               ex_integracao_tn->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
        RAISE erro_upload_nao_autorizado.
      ENDIF.

    CATCH zcx_error INTO DATA(ex_error_tn).
      RAISE erro_upload_nao_autorizado.
  ENDTRY.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 70
      text       = 'Aguardando aprovação do Carguero...'.

*-----------------------------------------
* obtem documentos faturamento
*-----------------------------------------
  TRY.
      t_pdf_files = zcl_faturamento=>zif_faturamento~get_instance(
                      )->get_documentos_faturamento( EXPORTING i_ch_referencia  = wa_zsdt0001-ch_referencia
                      ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*--------------------------------------------
* valida arquivos obrigatorios
*--------------------------------------------
  TRY.
      l_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                       )->get_documentos_obrigatorios( EXPORTING i_ch_referencia    = wa_zsdt0001-ch_referencia
                                                                 t_pdf_files        = t_pdf_files
                                                       IMPORTING t_doctos_faltantes = t_doctos_faltantes
                       ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

  LOOP AT t_doctos_faltantes INTO w_doctos_faltantes.
    READ TABLE t_pdf_files INTO w_pdf_files WITH KEY tipo_doc = w_doctos_faltantes-tipo_doc.
    IF sy-subrc = 0.
      DELETE t_pdf_files INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

*-----------------------------------------
* tebale interna parav controle
*-----------------------------------------
  LOOP AT t_pdf_files            INTO w_pdf_files.
    MOVE-CORRESPONDING w_pdf_files TO wa_file_local.
    MOVE abap_true                 TO wa_file_local-bloq.
    APPEND wa_file_local           TO it_file_local.
    PERFORM f_trata_campos_tela USING w_pdf_files-filename
                                      w_pdf_files-tipo_doc.
  ENDLOOP.

  READ TABLE t_doctos_faltantes INTO w_doctos_faltantes INDEX 1.
  IF sy-subrc = 0.
    l_mensagem   = w_doctos_faltantes-mensagem && ' para envio.'.
  ENDIF.

  IF wa_zsdt0001-docs_enviado_carguero = abap_true.
    CLEAR l_mensagem.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Aguardando aprovação do Carguero...'.

*--------------------------------------------
* parceiro ov
*--------------------------------------------
  SELECT SINGLE lifnr
           FROM vbpa
           INTO @l_lifnr
          WHERE vbeln = @wa_zsdt0001-vbeln
            AND parvw = 'LR'.

  IF sy-subrc <> 0 OR l_lifnr IS INITIAL.
    SELECT SINGLE kunnr
             FROM vbpa
             INTO @l_lifnr
            WHERE vbeln = @wa_zsdt0001-vbeln
              AND parvw = 'LR'.
  ENDIF.

*--------------------------------------------
* anexar arquivos
*--------------------------------------------
  DO.
    CALL SCREEN 0100 STARTING AT  45 03
                       ENDING AT 144 13.

    IF l_abandona_tela = abap_true.
      EXIT.
    ENDIF.
  ENDDO.

*-----------------------------------------
* saida
*-----------------------------------------
  e_st_proc               = l_st_proc.
  e_docs_enviado_carguero = l_docs_enviado_carguero.

ENDFUNCTION.
