function zmm_nfse_get_withtax.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_HEADER) TYPE  BAPI_INCINV_CREATE_HEADER
*"     REFERENCE(I_GUID_HEADER) TYPE  ZIBT_NFSE_001-GUID_HEADER
*"  TABLES
*"      IT_ITEMDATA STRUCTURE  BAPI_INCINV_CREATE_ITEM OPTIONAL
*"      IT_ACCOUNTINGDATA STRUCTURE  BAPI_INCINV_CREATE_ACCOUNT
*"       OPTIONAL
*"      IT_GLACCOUNTDATA STRUCTURE  BAPI_INCINV_CREATE_GL_ACCOUNT
*"       OPTIONAL
*"      ET_WITHTAXDATA STRUCTURE  BAPI_INCINV_CREATE_WITHTAX OPTIONAL
*"      ET_BAPIRET2 STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  field-symbols: <read> type any table.

  data lt_lfbw type table of lfbw.
  data lt_accit	type accit_t.
  data wa_accit	type accit.
  data t_ekbe type table of  ekbe.
  data ew_bapiret2 type bapiret2.

  data: lt_itemdata       type table of bapi_incinv_create_item,
        lt_accountingdata type table of bapi_incinv_create_account,
        lt_glaccountdata  type table of  bapi_incinv_create_gl_account,
        lt_withtax        type table of bapi_incinv_create_withtax.


  clear: et_withtaxdata[], et_bapiret2[].

  check i_header is not initial.

  call function 'FI_WT_READ_LFBW'
    exporting
      i_lifnr   = i_header-diff_inv
      i_bukrs   = i_header-comp_code
    tables
      t_lfbw    = lt_lfbw
    exceptions
      not_found = 1
      others    = 2.

  if sy-subrc <> 0.
    exit.
  endif.

  loop at lt_lfbw assigning field-symbol(<fs_lfbw>).

    append initial line to lt_withtax assigning field-symbol(<fs_with>).

    <fs_with>-split_key = '000001'.
    <fs_with>-wi_tax_type = <fs_lfbw>-witht.
    <fs_with>-wi_tax_code = <fs_lfbw>-wt_withcd.

  endloop.

  call function 'ME_READ_HISTORY_HEADER'
    exporting
      i_ebeln   = '0000000000'
      i_ebelp   = 10
    tables
      t_ekbe    = t_ekbe
    exceptions
      not_found = 1
      others    = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
  assign ('(SAPLEINR)READ_DATA[]') to <read>.
  if <read> is assigned.
    refresh <read>.
  endif.

  data: lv_json_head type string,
        lv_json_item type string,
        lv_json_acct type string,
        lv_json_cont type string,
        lv_json_mate type string,
        lv_json_with type string.
  data: number           type tbtcjob-jobcount,
        name             type tbtcjob-jobname,
        print_parameters type pri_params.
  data: ck_erro type char01 value abap_false.


  lt_itemdata[]       = it_itemdata[].
  lt_accountingdata[] = it_accountingdata[].
  lt_glaccountdata[]  = it_glaccountdata[].
  lv_json_head = /ui2/cl_json=>serialize( data = i_header ).
  lv_json_item = /ui2/cl_json=>serialize( data = lt_itemdata ).
  lv_json_acct = /ui2/cl_json=>serialize( data = lt_accountingdata ).
  lv_json_cont = /ui2/cl_json=>serialize( data = lt_glaccountdata ).
  lv_json_with = /ui2/cl_json=>serialize( data = lt_withtax ).
  data(lc_user_job) = zcl_job=>get_user_job( ).

  if sy-batch = 'X'.
    call function 'MRM_SRM_INVOICE_SIMULATE'
      exporting
        headerdata     = i_header
      importing
        return         = et_bapiret2[]
        t_accit        = lt_accit
      tables
        itemdata       = it_itemdata
        accountingdata = it_accountingdata
        glaccountdata  = it_glaccountdata
        withtaxdata    = lt_withtax.
  else.
    data cguid(16).
    cguid = i_guid_header.
    concatenate 'JOB_MIRO_SIM' cguid  into name separated by '_'.

    call function 'JOB_OPEN'
      exporting
        jobname          = name
      importing
        jobcount         = number
      exceptions
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        others           = 4.

    if sy-subrc is initial.
      submit zmmr208 to sap-spool spool parameters print_parameters without spool dynpro via job name number number
        with pguid   = i_guid_header
        with pjsonhd = lv_json_head
        with pjsonit = lv_json_item
        with pjsonac = lv_json_acct
        with pjsonct = lv_json_cont
        with pjsonmt = lv_json_mate
        with pjsonwi = lv_json_with
        with psimu   = 'X'

        user lc_user_job
         and return.

      if sy-subrc is initial.
        call function 'JOB_CLOSE'
          exporting
            jobcount             = number
            jobname              = name
            strtimmed            = 'X'
          exceptions
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            others               = 8.

        if sy-subrc is not initial.
          ck_erro = abap_true.
          message id sy-msgid type sy-msgty number sy-msgno into data(mtext) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          call function 'BP_JOB_DELETE'
            exporting
              jobcount                 = number
              jobname                  = name
            exceptions
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              others                   = 18.
          if sy-subrc is not initial.
            ck_erro = abap_false.
          endif.
        endif.
      else.
        ck_erro = abap_true.
        message id sy-msgid type sy-msgty number sy-msgno into mtext with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        call function 'BP_JOB_DELETE'
          exporting
            jobcount                 = number
            jobname                  = name
          exceptions
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            others                   = 18.
        if sy-subrc is not initial.
          ck_erro = abap_false.
        endif.
      endif.
    endif.

    "Aguardar execução do job
    zcl_job=>get_instance(
     )->set_key_job( i_jobname = name i_jobcount = number
     )->get_wait_job_exec(
     ).
    "
    "retorno mensagens bapi
    select *
      from zibt_nfse_005
      into table @data(t_zibt_nfse_005)
      where guid_header = @i_guid_header.

    loop at t_zibt_nfse_005 into data(w_zibt_nfse_005).
      if w_zibt_nfse_005-msgid ne '999'  and w_zibt_nfse_005-message is not initial.
        ew_bapiret2-type       = w_zibt_nfse_005-msgty.
        ew_bapiret2-id         = w_zibt_nfse_005-msgid.
        ew_bapiret2-number     = w_zibt_nfse_005-msgno.
        ew_bapiret2-message    = w_zibt_nfse_005-message.
        ew_bapiret2-message_v1 = w_zibt_nfse_005-msgv1.
        ew_bapiret2-message_v2 = w_zibt_nfse_005-msgv2.
        ew_bapiret2-message_v3 = w_zibt_nfse_005-msgv3.
        ew_bapiret2-message_v4 = w_zibt_nfse_005-msgv4.
        append ew_bapiret2 to et_bapiret2.
      else.
        wa_accit-qsskz = w_zibt_nfse_005-msgv1.
        replace all occurences of '.' in w_zibt_nfse_005-msgv2 with ' '.
        condense w_zibt_nfse_005-msgv2.
        replace all occurences of ',' in w_zibt_nfse_005-msgv2 with '.'.
        move w_zibt_nfse_005-msgv2 to wa_accit-pswbt.
        append wa_accit to lt_accit.
      endif.

    endloop.



  endif.

  call function 'DEQUEUE_ALL'
    exporting
      _synchron = 'X'.

  delete lt_accit where ktosl = 'KBS'.

  check lt_accit is not initial.

  loop at lt_lfbw assigning <fs_lfbw>.

    read table lt_accit assigning field-symbol(<fs_accit>)
      with key qsskz = <fs_lfbw>-witht.

    check sy-subrc eq 0.

    append initial line to et_withtaxdata assigning field-symbol(<fs_tax>).

    <fs_tax>-split_key = '000001'.
    <fs_tax>-wi_tax_type = <fs_lfbw>-witht.
    <fs_tax>-wi_tax_code = <fs_lfbw>-wt_withcd.
    "<fs_tax>-WI_TAX_BASE = 0,0000
    <fs_tax>-wi_tax_amt     = <fs_accit>-pswbt.
    "<fs_tax>-WI_TAX_WITHHELD_AMT = 0,0000

    if <fs_tax>-wi_tax_amt < 0.
      <fs_tax>-wi_tax_amt = <fs_tax>-wi_tax_amt * -1.
    endif.

  endloop.

  if et_withtaxdata[] is not initial.
    clear et_bapiret2[].
  endif.

endfunction.
