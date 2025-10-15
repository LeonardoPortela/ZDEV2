"Name: \FU:J_1B_NF_OBJECT_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 Z_BLOQ_REM_ENT_FUTURA_MIGO.
if sy-dynnr = '0001' and sy-tcode = 'MIGO' .
  field-symbols: <fs_xmseg_bwart> type any.
  field-symbols: <fs_xmseg_smbln> type any.

  data: v_xblnr type xblnr1,
        v_werks type werks_d,
        v_lifnr type lifnr,
        v_total type wrbtr.

  assign ('(SAPLMBWL)XMSEG-BWART') to <fs_xmseg_bwart>.
  assign ('(SAPLMBWL)XMSEG-SMBLN') to <fs_xmseg_smbln>.

  if ( <fs_xmseg_bwart> is assigned ).

    select single *
      from t156
      into @data(i_t156)
       where  bwart = @<fs_xmseg_bwart>. "'801'.

    if  ( <fs_xmseg_bwart> = '801' or sy-tcode = 'MIGO' ) and <fs_xmseg_smbln> = '' and i_t156-shkzg = 'S'.

      read table gbobj_header into data(wa_header) index 1.
      concatenate wa_header-nfenum '-' wa_header-series into v_xblnr.

      v_werks    = wa_header-branch.
      v_lifnr    = wa_header-parid.

      loop at gbobj_item.
*        v_total = v_total + gbobj_item-netwr.
        v_total = v_total + gbobj_item-nfnett.              ""IR105059
      endloop.
      "
      if v_total > 0.
        call function 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          exporting
            p_lifnr    = v_lifnr
            p_parvw    = i_t156-j_1bparvw
            p_nftype   = i_t156-j_1bnftype
            p_xblnr    = v_xblnr
            p_data     = wa_header-docdat
            p_werks    = v_werks
            p_valor_nf = v_total
            p_bsart    = 'PSEF'
          exceptions
            error      = 1
            others     = 2.

        if not sy-subrc is initial.
          message id     sy-msgid
          type   sy-msgty
          number sy-msgno
          with   sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4 raising nf_object_error.

        endif.
      endif.
    endif.
  endif.
endif.
ENDENHANCEMENT.
