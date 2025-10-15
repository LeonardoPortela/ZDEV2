"Name: \FU:RV_INVOICE_DOCUMENT_ADD\SE:END\EI
ENHANCEMENT 0 ZRV_INVOICE_DOCUMENT_ADD.

call function 'ZSD_VF11'
  exporting
    tcode           = sy-tcode
    new_cancel_fail = new_cancel_fail
    xvbrk           = xvbrk
    xvbrp           = xvbrp.

call function 'ZSD_VF01'
  exporting
    i_vbeln = xvbrk-vbeln.


"Realizar estorno documento VF08.
* >>>>>>>Inicio melhoria CS2020000398 - Contabilização do crédito presumido / AOENNING <<<<<<
if sy-tcode eq 'VF11'.

  data(zobjk) = |ZFIS { xvbrk-vbeln } { xvbrk-fkdat(4) }|.
  select single * from zib_contabil_chv
  into @data(wa_contabil_chv)
  where obj_key eq @zobjk.
  if sy-subrc eq 0 and wa_contabil_chv-belnr is not initial.
    "Estorna documento contabil.

    data: e_msg_code type  msgno,
          e_mensagem type  string,
          zvg_budat  type budat.

    call function 'ZFI_ESTORNO_DOC_CONTABIL'
      exporting
        belnr      = wa_contabil_chv-belnr
        gjahr      = wa_contabil_chv-gjahr
        bukrs      = wa_contabil_chv-bukrs
        stgrd      = '01'
        budat      = zvg_budat
      importing
        e_msg_code = e_msg_code
        e_mensagem = e_mensagem.
    if e_msg_code ne '200'.
      message e024(sd) with e_mensagem.
    endif.
  endif.
endif.
* >>>>>>>Fim melhoria CS2020000398 - Contabilização do crédito presumido / AOENNING <<<<<<

ENDENHANCEMENT.
