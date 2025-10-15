*&---------------------------------------------------------------------*
*& Report ZMMR208
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr208.

data: wa_headerdata     type bapi_incinv_create_header,
      it_itemdata       type table of bapi_incinv_create_item,
      it_accountingdata type table of bapi_incinv_create_account,
      it_contas         type table of bapi_incinv_create_gl_account,
      it_materialdata   type table of bapi_incinv_create_material,
      it_withtaxdata    type table of bapi_incinv_create_withtax,
      lt_accit          type accit_t,
      invoicedocnumber  type re_belnr,
      it_return         type table of  bapiret2,
      fiscalyear        type gjahr.

data: it_log       type table of zib_nfe_dist_log,
      wa_log       type zib_nfe_dist_log,
      lc_message   type bapi_msg,
      mt_nfse_005  type zibc_nfse_005,
      vmsgno       type zibt_nfse_005-msgno,
      lc_sequencia type zde_seq_log.


parameters: pchave  type zde_chave_doc_e no-display,
            pjsonhd type string no-display,
            pjsonit type string no-display,
            pjsonac type string no-display,
            pjsonct type string no-display,
            pjsonmt type string no-display,
            pjsonwi type string no-display,
            psimu   type char1 no-display,
            pguid   type zibt_nfse_001-guid_header no-display.


/ui2/cl_json=>deserialize(
  exporting
    json = pjsonhd
  changing
    data = wa_headerdata ).

/ui2/cl_json=>deserialize(
  exporting
    json = pjsonit
  changing
    data = it_itemdata ).

if pjsonac is not initial.
  /ui2/cl_json=>deserialize(
    exporting
      json = pjsonac
    changing
      data = it_accountingdata ).
endif.

if pjsonct is not initial.
  /ui2/cl_json=>deserialize(
    exporting
      json = pjsonct
    changing
      data = it_contas ).
endif.

if pjsonmt is not initial.
  /ui2/cl_json=>deserialize(
    exporting
      json = pjsonmt
    changing
      data = it_materialdata ).
endif.

if pjsonwi is not initial.
  /ui2/cl_json=>deserialize(
    exporting
      json = pjsonwi
    changing
      data = it_withtaxdata ).
endif.

if psimu is initial.
  call function 'BAPI_INCOMINGINVOICE_CREATE'
    exporting
      headerdata       = wa_headerdata
    importing
      invoicedocnumber = invoicedocnumber
      fiscalyear       = fiscalyear
    tables
      itemdata         = it_itemdata
      accountingdata   = it_accountingdata
      glaccountdata    = it_contas
      materialdata     = it_materialdata
      withtaxdata      = it_withtaxdata
      return           = it_return.

  select max( nr_sequencia ) into lc_sequencia
      from zib_nfe_dist_log
     where chave_nfe eq pchave.

  if lc_sequencia is initial.
    lc_sequencia = 1.
  else.
    add 1 to lc_sequencia.
  endif.
  loop at it_return into data(wa_retorno_miro).
    wa_log-chave_nfe      = pchave.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-nr_sequencia   = lc_sequencia.
    wa_log-type           = wa_retorno_miro-type.
    wa_log-id             = wa_retorno_miro-id.
    wa_log-num            = wa_retorno_miro-number.
    wa_log-message_v1     = wa_retorno_miro-message_v1.
    wa_log-message_v2     = wa_retorno_miro-message_v2.
    wa_log-message_v3     = wa_retorno_miro-message_v3.
    wa_log-message_v4     = wa_retorno_miro-message_v4.
    wa_log-bname          = sy-uname.
    wa_log-transacao      = 'ZMMR208'.

    if wa_retorno_miro-id is not initial and wa_retorno_miro-type is not initial.
      message id wa_retorno_miro-id
            type wa_retorno_miro-type
          number wa_retorno_miro-number
            into wa_log-message
            with wa_log-message_v1
                 wa_log-message_v2
                 wa_log-message_v3
                 wa_log-message_v4.
    endif.
    append wa_log       to it_log.
    add 1 to lc_sequencia.
  endloop.

  if ( invoicedocnumber is not initial ) and ( fiscalyear is not initial ).
    "Grava log
    modify zib_nfe_dist_log from table it_log.
    "
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    update zib_nfe_dist_ter set belnr = invoicedocnumber
                                gjahr = fiscalyear
    where chave_nfe = pchave.
    update zfit0007 set belnr = invoicedocnumber
                        gjahr = fiscalyear
    where invoicekey = pchave.
    commit work.
  else.
    "Grava log
    modify zib_nfe_dist_log from table it_log.
    commit work.
  endif.
else.
  call function 'MRM_SRM_INVOICE_SIMULATE' "#EC CI_USAGE_OK[2438006]
    exporting
      headerdata     = wa_headerdata
    importing
      return         = it_return
      t_accit        = lt_accit
    tables
      itemdata       = it_itemdata
      accountingdata = it_accountingdata
      glaccountdata  = it_contas
      materialdata   = it_materialdata
      withtaxdata    = it_withtaxdata.

  if pchave is not initial.
    select max( nr_sequencia ) into lc_sequencia
      from zib_nfe_dist_log
     where chave_nfe eq pchave.

    if lc_sequencia is initial.
      lc_sequencia = 1.
    else.
      add 1 to lc_sequencia.
    endif.
    loop at it_return into wa_retorno_miro.
      wa_log-chave_nfe      = pchave.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-nr_sequencia   = lc_sequencia.
      wa_log-type           = wa_retorno_miro-type.
      wa_log-id             = wa_retorno_miro-id.
      wa_log-num            = wa_retorno_miro-number.
      wa_log-message_v1     = wa_retorno_miro-message_v1.
      wa_log-message_v2     = wa_retorno_miro-message_v2.
      wa_log-message_v3     = wa_retorno_miro-message_v3.
      wa_log-message_v4     = wa_retorno_miro-message_v4.
      wa_log-bname          = sy-uname.
      wa_log-transacao      = 'ZMMR208'.

      if wa_retorno_miro-id is not initial and wa_retorno_miro-type is not initial.
        message id wa_retorno_miro-id
              type wa_retorno_miro-type
            number wa_retorno_miro-number
              into wa_log-message
              with wa_log-message_v1
                   wa_log-message_v2
                   wa_log-message_v3
                   wa_log-message_v4.
      endif.
      append wa_log       to it_log.
      add 1 to lc_sequencia.
    endloop.

    "Grava log
    if it_log[] is not initial.
      modify zib_nfe_dist_log from table it_log.
      commit work.
    endif.

    read table it_return into data(ret) with key type = 'E'.
    if sy-subrc = 0.
      update zib_nfe_dist_ter set sucesso = ' '
      where chave_nfe = pchave.
    else.
      update zib_nfe_dist_ter set sucesso = 'X'
      where chave_nfe = pchave.
    endif.

    commit work.

  else.

    loop at it_return into wa_retorno_miro.
      append initial line to mt_nfse_005 assigning field-symbol(<fs_005>).
      <fs_005>-guid_header = pguid.
      <fs_005>-dt_registro = sy-datum.
      <fs_005>-hr_registro = sy-uzeit.
      <fs_005>-msgty       = wa_retorno_miro-type.
      <fs_005>-msgid       = wa_retorno_miro-id.
      <fs_005>-msgno       = wa_retorno_miro-number.
      <fs_005>-message     = wa_retorno_miro-message.
      <fs_005>-msgv1       = wa_retorno_miro-message_v1.
      <fs_005>-msgv2       = wa_retorno_miro-message_v2.
      <fs_005>-msgv3       = wa_retorno_miro-message_v3.
      <fs_005>-msgv4       = wa_retorno_miro-message_v4.
      <fs_005>-us_registro = sy-uname.
    endloop.

    delete lt_accit where ktosl = 'KBS'.
    clear vmsgno.
    loop at lt_accit into data(w_accit).
      append initial line to mt_nfse_005 assigning <fs_005>.
      add 1 to vmsgno.
      <fs_005>-guid_header = pguid.
      <fs_005>-dt_registro = sy-datum.
      <fs_005>-hr_registro = sy-uzeit.
      <fs_005>-msgty       = 'S'.
      <fs_005>-msgid       = '999'.
      <fs_005>-msgno       = vmsgno.
      <fs_005>-message     = ''.
      <fs_005>-msgv1       = w_accit-qsskz.
      write w_accit-pswbt  to <fs_005>-msgv2.
    endloop.

    if mt_nfse_005[] is not initial.
      delete from zibt_nfse_005 where guid_header = pguid.
      commit work.
      modify zibt_nfse_005 from table mt_nfse_005.
      commit work.
    endif.


  endif.

endif.
