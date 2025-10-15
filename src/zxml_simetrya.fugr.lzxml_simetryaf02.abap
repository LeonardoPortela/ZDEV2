*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALTERA_STATUS
*&---------------------------------------------------------------------*
*       Registra status de NFe
*----------------------------------------------------------------------*
form altera_status  using    it_notas      type zib_nota_fiscal_sap
                             ls_acttab_new type j_1bnfe_active
                             ls_doc_new    type j_1bnfdoc.

  call function 'Z_1B_NF_ALTERA_STATUS'
    exporting
      it_notas      = it_notas
      ls_acttab_new = ls_acttab_new
      ls_doc_new    = ls_doc_new.

endform.                    " ALTERA_STATUS

*&---------------------------------------------------------------------*
*&      Form  REGISTRA_LOG
*&---------------------------------------------------------------------*
*       registra log de eventos
*----------------------------------------------------------------------*
form registra_log  using    ls_acttab_new type j_1bnfe_active
                            ls_doc_new    type j_1bnfdoc
                            it_nota       like zib_nota_fiscal_sap.

  data: begin of wa_err_tab.
          include structure bdcmsgcoll.
  data: end of wa_err_tab,

  ls_err_tab like standard table of wa_err_tab,
  wa_zib_nfe type zib_nfe.

  call function 'CONVERSION_EXIT_ISOLA_INPUT'
    exporting
      input  = 'PT'
    importing
      output = wa_err_tab-msgspra.

  wa_err_tab-msgtyp  = 'E'.
  "wa_err_tab-msgid   = ls_acttab_new-code.
  wa_err_tab-msgid   = '000'.
  wa_err_tab-msgnr   = 1.
  wa_err_tab-msgv1   = it_nota-ms_erro+000(50).
  wa_err_tab-msgv2   = it_nota-ms_erro+050(50).
  wa_err_tab-msgv3   = it_nota-ms_erro+100(50).
  wa_err_tab-msgv4   = it_nota-ms_erro+150(50).

  append wa_err_tab to ls_err_tab.

  call function 'J_1B_NFE_ERROR_PROTOKOLL'
    exporting
      i_docnum   = ls_acttab_new-docnum
      i_tab_only = 'X'
    tables
      it_err_tab = ls_err_tab.

  if not ls_doc_new-form is initial.
    if it_nota-tp_authcod eq '1'.
      delete from zib_nfe where docnum eq ls_acttab_new-docnum.
    else.
      select single * into wa_zib_nfe
        from zib_nfe
       where docnum eq ls_acttab_new-docnum.
      if sy-subrc is initial.
        clear: wa_zib_nfe-date_aut_2,
               wa_zib_nfe-time_aut_2,
               wa_zib_nfe-user_aut_2.
        modify zib_nfe from wa_zib_nfe.
      endif.
    endif.
  endif.

endform.                    " REGISTRA_LOG


*&---------------------------------------------------------------------*
*&      Form  INSESIR_INF_SIMETRYA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NOTAS  text
*----------------------------------------------------------------------*
form insesir_inf_simetrya  using it_notas type zib_nota_fiscal_sap wa_active type j_1bnfe_active.

  data: wa_zib_nfe type zib_nfe.

  select single * into wa_zib_nfe
    from zib_nfe
   where docnum eq it_notas-nu_documento_sap.

  if not sy-subrc is initial.
    if wa_active-cancel is initial.
      wa_zib_nfe-date_aut_1 = wa_active-action_date.
      wa_zib_nfe-time_aut_1 = wa_active-action_time.
      wa_zib_nfe-user_aut_1 = wa_active-action_user.
    else.
      wa_zib_nfe-date_aut_2 = wa_active-action_date.
      wa_zib_nfe-time_aut_2 = wa_active-action_time.
      wa_zib_nfe-user_aut_2 = wa_active-action_user.
    endif.
  endif.

  if wa_active-cancel is initial.
    if wa_zib_nfe-date_ret_1 is initial.
      wa_zib_nfe-date_ret_1 = sy-datum.
      wa_zib_nfe-time_ret_1 = sy-uzeit.
      wa_zib_nfe-user_ret_1 = sy-uname.
    endif.
    wa_zib_nfe-authcod      = it_notas-authcod.
    wa_zib_nfe-code         = it_notas-code.
    wa_zib_nfe-docnum9      = it_notas-docnum9.
    wa_zib_nfe-cdv          = it_notas-cdv.
    wa_zib_nfe-ds_url_danfe = it_notas-ds_url_danfe.
    if not it_notas-ms_erro is initial.
      wa_zib_nfe-ds_url_cle   = it_notas-ms_erro(100).
    endif.
  else.
    if wa_zib_nfe-date_ret_2 is initial.
      wa_zib_nfe-date_ret_2 = sy-datum.
      wa_zib_nfe-time_ret_2 = sy-uzeit.
      wa_zib_nfe-user_ret_2 = sy-uname.
    endif.
  endif.

  wa_zib_nfe-docnum       = it_notas-nu_documento_sap.

  modify zib_nfe from wa_zib_nfe.

endform.                    " INSESIR_INF_SIMETRYA
