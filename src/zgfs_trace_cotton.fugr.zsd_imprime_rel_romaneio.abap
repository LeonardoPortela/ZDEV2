FUNCTION zsd_imprime_rel_romaneio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CH_REFERENCIA) TYPE  ZCH_REF
*"     REFERENCE(I_IMPRIME) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(E_XSTRING_DOCUMENT) TYPE  XSTRING
*"  EXCEPTIONS
*"      ROMANEIO_NAO_ENCONTRADO
*"      ROMANEIO_EM_ESTORNO
*"      ERRO_IMPRESSAO_FORMULARIO
*"----------------------------------------------------------------------

  FREE: t_pdf_files,
        e_xstring_document.

  PERFORM f_selecao_dados         USING i_ch_referencia.

  t_zmmt0008_grp[] = t_zmmt0008[].

  SORT t_zmmt0008_grp BY lgort.
  DELETE ADJACENT DUPLICATES FROM t_zmmt0008_grp
                        COMPARING lgort.

*-------------------------------------
* agrupar por lote para saida smart
*-------------------------------------
  LOOP AT t_zmmt0008_grp  INTO w_zmmt0008_grp.

    FREE: tl_0008.

    LOOP AT t_zmmt0008    INTO w_zmmt0008 WHERE werks = w_zmmt0008_grp-werks
                                            AND lgort = w_zmmt0008_grp-lgort.
      MOVE   w_zmmt0008     TO tl_0008.
      APPEND tl_0008.
    ENDLOOP.

    PERFORM f_prepara_impressao.
    PERFORM f_imprime_smartforms CHANGING e_xstring_document.

    CLEAR w_pdf_files.
    w_pdf_files-data       = e_xstring_document.
    w_pdf_files-len        = xstrlen( e_xstring_document ).
    APPEND w_pdf_files    TO t_pdf_files.
  ENDLOOP.

  CHECK i_imprime = abap_true.

  CHECK t_pdf_files[] IS NOT INITIAL.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      l_pdf_xtring = zcl_faturamento=>zif_faturamento~get_instance(
                         )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                         ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*---------------------------------------
* visualizar PDF
*---------------------------------------
  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_pdf                    = l_pdf_xtring
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFUNCTION.
