FUNCTION zsd_exibir_ra_assinada.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_CG) TYPE  ZNRO_CG
*"     REFERENCE(I_CH_REFERENCIA) TYPE  ZCH_REF
*"  EXCEPTIONS
*"      PDF_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: t_pdf_files TYPE zsdt_pdf_files,
        w_pdf_files TYPE zsde_pdf_files,
        l_pdf_merge TYPE xstring.

*---------------------------------------
* API retorna PDF doctos assinados
*---------------------------------------
  TRY .
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->get_pdf_documento_assinado( EXPORTING i_nro_cgd            = i_nro_cg
                                                  i_ch_referencia      = i_ch_referencia
                                        IMPORTING t_pdf_docto_assinado = t_pdf_files ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      RAISE pdf_not_found.
    CATCH zcx_error INTO DATA(ex_error).    "  "
      RAISE pdf_not_found.
  ENDTRY.

  CHECK t_pdf_files[] IS NOT INITIAL.

* READ TABLE t_pdf_files INTO w_pdf_files INDEX 1.
* l_pdf_merge = w_pdf_files-data.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      l_pdf_merge = zcl_faturamento=>zif_faturamento~get_instance(
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
      i_pdf                    = l_pdf_merge
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFUNCTION.
