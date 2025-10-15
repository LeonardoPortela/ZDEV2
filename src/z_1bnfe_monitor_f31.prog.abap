*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F31
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUMBERED_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NFE_ALV  text
*      -->P_C_1  text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form check_numbered_nfe  using    p_docnum type j_1bdocnum
                                  p_nfnum9 type j_1bnfnum9
                                  p_c      type i
                         changing p_subrc  type c.
*
* create error messages for nun numbred NF-e
  check p_nfnum9 is initial.
* set error indicator for caller
  p_subrc = c_x.
* set error message depending on the caller
  case p_c.
* called for contingency
    when c_1.  sy-msgno = '094'.
* called for Send to SSEFAZ
    when c_2.  sy-msgno = '095'.
*
  endcase.
*
  sy-msgty = 'E'.
  sy-msgid = 'J1B_NFE'.
  sy-msgv1 = wa_nfe_alv-docnum.
*
  call function 'J_1B_NFE_ERROR_PROTOKOLL'
    exporting
      i_docnum = wa_nfe_alv-docnum.
*
endform.                    " CHECK_NUMBERED_NFE
