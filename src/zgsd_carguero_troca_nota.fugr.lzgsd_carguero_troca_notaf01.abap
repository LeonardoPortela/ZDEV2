*----------------------------------------------------------------------*
***INCLUDE LZGSD_CARGUERO_TROCA_NOTAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_ARQUIVOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_anexar_arquivos .

  FREE: t_rawtab,
        l_xtring,
        l_len.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = l_title
      default_extension       = '.pdf' "cl_gui_frontend_services=>FILETYPE_TEXT
*     default_filename        =
      file_filter             = '.pdf'
*     with_encoding           =
      initial_directory       = l_path
    CHANGING
      file_table              = t_file_extension
      rc                      = l_rc_file
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK sy-subrc = 0.

  READ TABLE t_file_extension INTO w_file_extension INDEX 1.
  CHECK sy-subrc = 0.

*-----------------------------------------
*-PDF FILE
*-----------------------------------------
  CHECK w_file_extension-filename CS '.PDF'.

  l_file_name = w_file_extension-filename.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_file_name
      filetype                = 'BIN'
    IMPORTING
      filelength              = l_len
    CHANGING
      data_tab                = t_rawtab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = l_len
    IMPORTING
      buffer       = l_xtring
    TABLES
      binary_tab   = t_rawtab
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  CHECK sy-subrc = 0.

  wa_file_local-tipo_doc          = '98'.
  wa_file_local-filename          = l_file_name.
  wa_file_local-data              = l_xtring.
  wa_file_local-len               = l_len.
  wa_file_local-bloq              = abap_false.
  APPEND wa_file_local           TO it_file_local.

  PERFORM f_trata_campos_tela USING l_file_name
                                    wa_file_local-tipo_doc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_ARQUIVOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_trata_campos_tela USING p_file
                               p_tipo.

  DATA: t_idd07v TYPE TABLE OF dd07v,
        w_idd07v TYPE dd07v,
        l_descr  TYPE string.

  FREE: l_ind,
        l_descr.

*-------------------
* descricao dominio
*-------------------
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZSDE_TIPODOC'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = p_tipo.
  l_descr = w_idd07v-ddtext.

  DO.
    l_ind = l_ind + 1.
    IF l_ind > 10.
      EXIT.
    ENDIF.

    l_campo = 'L_FILE' && l_ind.

    ASSIGN (l_campo) TO <f_campo>.
    CHECK sy-subrc = 0.

    IF <f_campo> IS INITIAL.
      <f_campo> = p_file.

      UNASSIGN <f_campo>.

      l_campo = 'L_TIPO' && l_ind.
      ASSIGN (l_campo) TO <f_campo>.
      <f_campo> = l_descr.

      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_elimina arquivos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_elimina_arquivos USING p_code.

  l_ind = p_code+4(2).

  l_campo = 'L_FILE' && l_ind.

  ASSIGN (l_campo)  TO <f_campo>.

  IF sy-subrc = 0.
    READ TABLE it_file_local INTO wa_file_local WITH KEY filename = <f_campo>.
    IF sy-subrc = 0.
      DELETE it_file_local INDEX sy-tabix.
      CLEAR <f_campo>.

      UNASSIGN <f_campo>.

      l_campo = 'L_TIPO' && l_ind.
      ASSIGN (l_campo)  TO <f_campo>.
      CLEAR <f_campo>.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_CARGUERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_envia_carguero.

  DATA: l_erro_conf    TYPE char1.

  FREE: t_pdf_files,
        merged_document.

  LOOP AT it_file_local            INTO wa_file_local.
    MOVE-CORRESPONDING wa_file_local TO w_pdf_files.
    APPEND w_pdf_files               TO t_pdf_files.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Enviando arquivos para Carguero...'.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      merged_document = zcl_faturamento=>zif_faturamento~get_instance(
                          )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                          ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*-----------------------------------------
* remessa / transporte
*-----------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zsdt0001-doc_transp
    IMPORTING
      output = l_tknum.

  SELECT *
    FROM likp
    INTO TABLE t_likp
   WHERE vbeln = wa_zsdt0001-doc_rem.

*-----------------------------------------
* envia aprovacao carregamento
*-----------------------------------------
  TRY .
      zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
        )->set_viagem_carregar(
        EXPORTING
          i_dt_carregamento        = sy-datlo    " Data Carregamento
          i_remessas               = t_likp[]
          i_tknum                  = l_tknum
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao).
      IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
               ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
        MESSAGE ID ex_integracao->msgid TYPE 'I'
        NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4. " RAISING erro.
      ENDIF.
      RAISE erro_aprovacao_carrega.

    CATCH zcx_error INTO DATA(ex_error).
      MESSAGE ID ex_error->msgid TYPE 'I'
      NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4." RAISING erro.
      RAISE erro_aprovacao_carrega.
  ENDTRY.

*-----------------------------------------
* Efetua upload no carguero
*-----------------------------------------
  PERFORM f_upload_carguero.

*-CS2021000696 - 10.08.2021 - JT - inicio
*-----------------------------------------
* confirmacao upload
*-----------------------------------------
  "FF #170997 - inicio
  READ TABLE t_likp TRANSPORTING NO FIELDS WITH KEY inco1 = 'CPT'.
  IF sy-subrc <> 0.
    "FF #170997 - fim

    PERFORM f_confirmacao_upload CHANGING l_erro_conf.
    IF l_erro_conf = abap_true.
      PERFORM f_remove_carguero.
      MESSAGE i024(sd) WITH 'Não foi possível efetuar confirmação'
                            ' do UPLOAD no Carguero!'.
      RAISE erro_aprovacao_carrega.
    ENDIF.
*-CS2021000696 - 10.08.2021 - JT - fim

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 100
        text       = 'Enviando arquivos para Carguero...'.

  ENDIF.
*-------------------------------------
* update zsdt0001
*-------------------------------------
  IF wa_zsdt0001-st_proc = '98'.
    l_st_proc               = '99'.
    l_docs_enviado_carguero = abap_true.
  ELSE.
    l_st_proc               = wa_zsdt0001-st_proc.
    l_docs_enviado_carguero = abap_true.
  ENDIF.

  UPDATE zsdt0001 SET docs_enviado_carguero = l_docs_enviado_carguero
                      st_proc               = l_st_proc
                WHERE ch_referencia         = wa_zsdt0001-ch_referencia.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_REMOVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_remove_carguero.

  merged_document = '0'.

  TRY.
      zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
        )->valida_remocao_docs_carguero(
             EXPORTING
               i_ch_referencia = wa_zsdt0001-ch_referencia
             IMPORTING
               e_erro          = DATA(l_erro)
               e_msg_erro      = DATA(l_msg_erro)  ).

    CATCH zcx_integracao INTO DATA(ex_integracao).
      IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
               ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
        MESSAGE ID ex_integracao->msgid TYPE 'I'
        NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4. " RAISING erro.
      ENDIF.
      RAISE erro_upload.

    CATCH zcx_error INTO DATA(ex_error).
      MESSAGE ID ex_error->msgid TYPE 'I'
      NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4." RAISING erro.
      RAISE erro_upload.
  ENDTRY.

  IF l_erro = abap_true.
    MESSAGE l_msg_erro TYPE 'I'.
    RAISE erro_upload.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Removendo arquivos do Carguero...'.

*-----------------------------------------
* Efetua upload no carguero
*-----------------------------------------
  PERFORM f_upload_carguero.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Removendo arquivos do Carguero...'.

*-------------------------------------
* update zsdt0001
*-------------------------------------
  IF wa_zsdt0001-st_proc = '99'.
    l_st_proc               = '98'.
    l_docs_enviado_carguero = abap_false.
  ELSE.
    l_st_proc               = wa_zsdt0001-st_proc.
    l_docs_enviado_carguero = abap_false.
  ENDIF.

  UPDATE zsdt0001 SET docs_enviado_carguero = l_docs_enviado_carguero
                      st_proc               = l_st_proc
                WHERE ch_referencia         = wa_zsdt0001-ch_referencia.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_confirmacao_upload CHANGING p_erro_conf.

  CLEAR p_erro_conf.

  TRY .
      zcl_integracao_upload_conf=>zif_integracao_upload_conf~get_instance(
        )->set_empresa(       EXPORTING i_bukrs         = gwa_zlest0185-bukrs
        )->set_id_referencia( EXPORTING i_id_referencia = gwa_zlest0185-viagem_id
        )->set_ds_url(        EXPORTING i_viagem_id     = gwa_zlest0185-viagem_id
        )->set_send_msg(      IMPORTING e_id_integracao = l_id_integracao
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao_tn_con).
      IF NOT ( ex_integracao_tn_con->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
               ex_integracao_tn_con->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
        p_erro_conf = abap_true.
      ENDIF.

    CATCH zcx_error INTO DATA(ex_error_tn_con).
      p_erro_conf = abap_true.
  ENDTRY.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upload_carguero.

*-----------------------------------------
* Efetua upload no carguero
*-----------------------------------------
  TRY .
      zcl_integracao_upload_exec=>zif_integracao_upload_exec~get_instance(
        )->set_empresa(       EXPORTING i_bukrs         = gwa_zlest0185-bukrs
        )->set_id_referencia( EXPORTING i_id_referencia = gwa_zlest0185-viagem_id
        )->get_json(          EXPORTING i_file_bin      = merged_document
                              IMPORTING e_json_xstring  = DATA(lc_json_tn_xstring)
        )->set_ds_url(        EXPORTING i_url_upload    = l_url_upload
        )->set_ds_data(       EXPORTING i_json_xstring  = lc_json_tn_xstring
        )->set_send_msg(      IMPORTING e_id_integracao = l_id_integracao
        ).

    CATCH zcx_integracao INTO DATA(ex_integracao_tn_ret).
      IF NOT ( ex_integracao_tn_ret->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
               ex_integracao_tn_ret->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
        RAISE erro_upload.
      ENDIF.

    CATCH zcx_error INTO DATA(ex_error_tn_ret).
      IF NOT ( ex_error_tn_ret->msgv1 CS '0201' AND
               ex_error_tn_ret->msgv1 CS 'Created' ).
        RAISE erro_upload.
      ENDIF.
  ENDTRY.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_visualiza_arquivos.

  FREE: t_pdf_files,
        merged_document.

  LOOP AT it_file_local            INTO wa_file_local.
    MOVE-CORRESPONDING wa_file_local TO w_pdf_files.
    APPEND w_pdf_files               TO t_pdf_files.
  ENDLOOP.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      merged_document = zcl_faturamento=>zif_faturamento~get_instance(
                          )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                          ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*-----------------------------------------
* Visualizar
*-----------------------------------------
  CALL FUNCTION 'ZSMARTFORMS_PDF_FILE_PREVIEW'
    EXPORTING
      pdf_data = merged_document.

ENDFORM.
