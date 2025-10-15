*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F07
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  nf_writer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NFE_ALV_DOCNUM  text
*----------------------------------------------------------------------*
form nf_writer  using p_docnum type j_1bdocnum.

  call function 'J_1B_NF_DOC_READ_INTO_OBJECT'
    exporting
      doc_number         = p_docnum
    importing
      obj_number         = gf_nfobjn
    exceptions
      document_not_found = 1
      docum_lock         = 2
      others             = 3.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  call function 'J_1B_NF_OBJECT_DISPLAY'
    exporting
      obj_number         = gf_nfobjn
    exceptions
      object_not_found   = 1
      scr_ctrl_not_found = 2
      others             = 3.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " nf_writer
