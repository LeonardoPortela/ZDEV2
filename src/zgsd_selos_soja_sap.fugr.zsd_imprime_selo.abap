FUNCTION zsd_imprime_selo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(I_SAFRA) TYPE  ZDEPM_SAFRA OPTIONAL
*"     REFERENCE(I_IMPRIME_SELO) TYPE  CHAR1 DEFAULT 'X'
*"  EXPORTING
*"     VALUE(E_XSTRING_DOCUMENT) TYPE  XSTRING
*"  EXCEPTIONS
*"      DOCUMENTO_NAO_AUTORIZADO
*"      DOCUMENTO_NAO_IMPRIMIR
*"----------------------------------------------------------------------

  FREE: w_j1bnfdoc,
        w_j1bnflin,
        g_output_options,
        g_control_parameters,
        st_job_output_info,
        l_document_output_info,
        l_job_output_info,
        l_job_output_options,
        l_xml_doc,
        l_xstring_document,
        e_xstring_document.

*-------------------------------------------
*-NF
*-------------------------------------------
  l_docnum   = i_docnum.
  l_safra    = i_safra.

*-------------------------------------------
*-selecao NF
*-------------------------------------------
  SELECT SINGLE *
    FROM j_1bnfdoc
    INTO w_j1bnfdoc
   WHERE docnum = l_docnum.

  SELECT SINGLE *
    FROM j_1bnflin
    INTO w_j1bnflin
   WHERE docnum = l_docnum.

  CHECK sy-subrc = 0.

*-------------------------------------------
*-saida impressao
*-------------------------------------------
  PERFORM f_saida_impressao.

*-------------------------------------------
*-xml SEFAZ
*-------------------------------------------
* PERFORM f_recupera_xml     CHANGING l_xml_doc.
*
* IF l_xml_doc IS INITIAL.
*   RAISE documento_nao_imprimir.
* ENDIF.

*-------------------------------------------
*-montar estrutura
*-------------------------------------------
* PERFORM f_monta_estrutura_xml.
*
* IF wg_xml_sefaz IS INITIAL.
*   RAISE documento_nao_imprimir.
* ENDIF.

*-------------------------------------------
*-montar dados Smartform
*-------------------------------------------
* PERFORM f_monta_dados.

*-------------------------------------------
*-recuperar lote e datas
*-------------------------------------------
  PERFORM f_recupera_lote.

*-------------------------------------------
*-atualiza impressao selo na NF
*-------------------------------------------
  SELECT *
    FROM zpmt0056
    INTO w_zpmt0056
      UP TO 1 ROWS
   WHERE docnum = l_docnum.
  ENDSELECT.

  IF sy-subrc <> 0.
    zpmt0056-docnum          = l_docnum.
    zpmt0056-selo            = l_selo.
    zpmt0056-lote            = l_numero_lote.
    zpmt0056-data_fabricacao = l_data_fabricacao.
    zpmt0056-data_validade   = l_data_validade.
    MODIFY zpmt0056.
    COMMIT WORK AND WAIT.
  ENDIF.

*-------------------------------------------
*-executa smartform
*-------------------------------------------
  PERFORM f_saida_formulario CHANGING l_document_output_info
                                      l_job_output_info
                                      l_job_output_options.

  MOVE l_job_output_info-otfdata[] TO otfdata[].

*-------------------------------------------
*-preview smartform
*-------------------------------------------
  IF i_imprime_selo = abap_true.
    CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
      EXPORTING
        i_otf                    = otfdata[]
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
  ENDIF.

*-------------------------------------------
*-format xstring
*-------------------------------------------
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = l_bin_fsize
      bin_file              = l_xstring_document
    TABLES
      otf                   = otfdata[]
      lines                 = t_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  IF sy-subrc = 0.
    e_xstring_document      = l_xstring_document.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
