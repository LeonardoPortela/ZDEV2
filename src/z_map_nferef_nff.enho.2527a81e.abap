"Name: \PR:SAPLJ_1B_NFE\FO:MAP_NFEREF\SE:END\EI
ENHANCEMENT 0 Z_MAP_NFEREF_NFF.

  DATA vnnf TYPE j_1bnfnum9.

  LOOP AT xmlr_tab INTO ls_xmlr WHERE docnum     EQ p_docnum
                                  AND b12_refnfe IS INITIAL
                                  AND b20_nnf    IS NOT INITIAL.
    vnnf = |{ ls_xmlr-b20_nnf ALPHA = IN }|.
    READ TABLE gt_rfc_nfref_400 INTO DATA(wl_nf_ref_temp)
                            WITH KEY n_nf = vnnf             " com zeros
                                     cnpj = ls_xmlr-b20_cnpj
                                     cpf  = ls_xmlr-b20_cpf.
    IF sy-subrc NE 0.
      vnnf = |{ ls_xmlr-b20_nnf ALPHA = OUT }|.
      READ TABLE gt_rfc_nfref_400 INTO wl_nf_ref_temp
                            WITH KEY n_nf = vnnf              "Sem zeros
                                     cnpj = ls_xmlr-b20_cnpj
                                     cpf  = ls_xmlr-b20_cpf.
      IF sy-subrc NE 0.
        CLEAR: ls_rfc_nfref_400.

        ADD 1 TO lv_cont.

        ls_rfc_nfref_400-id      = lv_cont.
        ls_rfc_nfref_400-aamm    = ls_xmlr-b20_aamm.
        ls_rfc_nfref_400-c_uf    = ls_xmlr-b20_cuf.
        ls_rfc_nfref_400-cnpj    = ls_xmlr-b20_cnpj.

        PERFORM clear_field_format USING ls_xmlr-b20_ie
                                CHANGING ls_rfc_nfref_400-ie.

        ls_rfc_nfref_400-n_nf        = ls_xmlr-b20_nnf.
        ls_rfc_nfref_400-serie       = ls_xmlr-b20_serie.
        ls_rfc_nfref_400-refnfp_mod  = ls_xmlr-b20f_mod.
        ls_rfc_nfref_400-cpf         = ls_xmlr-b20_cpf.

        APPEND ls_rfc_nfref_400 TO gt_rfc_nfref_400.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDENHANCEMENT.
