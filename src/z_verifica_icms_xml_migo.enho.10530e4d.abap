"Name: \PR:SAPLJ1BF\FO:BADI_FILL_ADDITIONAL_FIELDS\SE:END\EI
ENHANCEMENT 0 Z_VERIFICA_ICMS_XML_MIGO.

  DATA: p_iva      TYPE j_1btxsdc_,
        lva_bwart  TYPE mseg-bwart,
        vl_icms    TYPE zib_nfe_dist_ter-vl_icms_total,
        lv_xblnr   TYPE  xblnr1,
        lv_iv_icms TYPE c.

  CLEAR: lva_bwart, p_iva, vl_icms.

  DATA(_estorno) = abap_false.

  LOOP AT it_mseg INTO DATA(lwa_mseg_check).

    IF lwa_mseg_check-smbln IS NOT INITIAL.
      _estorno = abap_true.
    ENDIF.

    p_iva     = lwa_mseg_check-mwskz.
    lva_bwart = lwa_mseg_check-bwart.

  ENDLOOP.

  SELECT SINGLE *
    FROM t156 INTO @DATA(lwa_t156_check)
    WHERE bwart = @lva_bwart.

  IF ( lwa_t156_check IS NOT INITIAL AND lwa_t156_check-j_1bnfrel IS NOT INITIAL ) AND "Migo com categoria Fiscal
     ( wa_nf_doc-form       IS INITIAL     ) AND
     ( wa_nf_doc-model      EQ '55'        ) AND
     ( is_mkpf-xblnr        IS NOT INITIAL ) AND
     ( _estorno             EQ abap_false  ).

    lv_iv_icms = 'S'.

    LOOP AT wa_nf_stx INTO DATA(lwa_nf_stx_check) WHERE taxgrp = 'ICMS'.
      ADD lwa_nf_stx_check-taxval TO vl_icms.
    ENDLOOP.

    EXPORT lv_iv_icms TO MEMORY ID 'ZIV_ICMS'.
    lv_xblnr = is_mkpf-xblnr.

    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        p_lifnr            = wa_nf_doc-parid
        p_nftype           = wa_nf_doc-nftype
        p_xblnr            = lv_xblnr
        p_data             = wa_nf_doc-docdat
        p_werks            = wa_nf_doc-branch
        p_valor_icms       = vl_icms
        p_iva              = p_iva
        p_migo_with_fiscal = abap_true
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.


ENDENHANCEMENT.
