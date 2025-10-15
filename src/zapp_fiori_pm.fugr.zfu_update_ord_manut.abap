function zfu_update_ord_manut.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_MSG TYPE  BAPIRET2_TAB OPTIONAL
*"      T_DADOS_ORDEM TYPE  ZPMT0082_T
*"----------------------------------------------------------------------
  data: lt_methods      type table of bapi_alm_order_method,
        lt_header       type table of bapi_alm_order_headers_i,
        lt_usrstatus    type table of bapi_alm_order_usrstat,
        ls_usrstatus    type bapi_alm_order_usrstat,
        lt_header_up    type table of bapi_alm_order_headers_up,
        it_operation    type table of bapi_alm_order_operation,
        it_operation_up type table of bapi_alm_order_operation_up,
        lv_refnum       type ifrefnum,
        zvar_ordem      type aufnr,
        et_return       type bapiret2_t,
        lt_return       type table of bapiret2,
        lv_oper_no      type objidext,
        lv_DuracaoOper  type daunor,
        lv_TrabOper     type arbeit,
        lv_NumPessoa    type anzkap,
        lv_quant_oper   type char10,
        ws_return       type bapiret2,
        var_seq_status  type ifrefnum.

  data: ls_method    type bapi_alm_order_method,
        ls_header    type bapi_alm_order_headers_i,
        ls_header_up type bapi_alm_order_headers_up.

  data(lo_bapis) = cl_iwo_bapi_ex_factory=>get_ibapi_alm_order( ).

  free: lt_methods, lt_header, ls_header_up, et_msg.
  clear: var_seq_status.



  check t_dados_ordem[] is not initial.



  loop at t_dados_ordem assigning field-symbol(<ls_dados>).
    free: lt_methods, lt_header, ls_header_up, et_msg, it_operation, it_operation_up.
    zvar_ordem = |{ <ls_dados>-aufnr alpha = out }|.
    zvar_ordem = |{ zvar_ordem alpha = in }|.

    ls_method-refnumber = '00000001'.
    ls_method-objecttype = 'HEADER'.
    ls_method-method = 'CHANGE'.
    ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
    append ls_method to lt_methods.

    clear: ls_method, lv_refnum.
    ls_method-refnumber = '00000001'. " <- mesmo número usado acima!
    ls_method-method = 'SAVE'.
    append ls_method to lt_methods.

    clear: ls_header.
    ls_header-orderid     = zvar_ordem. "Numero da ordem
    if <ls_dados>-revisao is not initial.
      ls_header-revision    = <ls_dados>-revisao.   "Revisão
    endif.

    if <ls_dados>-data_ini is not initial and <ls_dados>-data_fim is not initial.
      ls_header-start_date  = <ls_dados>-data_ini. "Inicio base
      ls_header-finish_date = <ls_dados>-data_fim. "Fim base
    endif.
*    ls_header-priority    = <ls_dados>-priok.
    ls_header-autosched   = abap_false.
    append ls_header to lt_header.

    clear: ls_header_up.
    ls_header_up-orderid      = zvar_ordem.
    if <ls_dados>-revisao is not initial.
      ls_header_up-revision     = 'X'.
    endif.

    if <ls_dados>-data_ini is not initial and <ls_dados>-data_fim is not initial.
      ls_header_up-start_date   = 'X'.
      ls_header_up-finish_date  = 'X'.
    endif.
*    ls_header_up-priority     = 'X'.
    ls_header_up-autosched    = 'X'.
    append ls_header_up to lt_header_up.

* Define o status do usuário
*    //===============//=======================
    if <ls_dados>-status_p is not initial.
      add 1 to var_seq_status.
      ls_method-refnumber = var_seq_status.
      ls_method-objecttype = 'USERSTATUS'.
      ls_method-method = 'CHANGE'.
      ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
      append ls_method to lt_methods.

      clear: ls_usrstatus.
      ls_usrstatus-user_st_text = <ls_dados>-status_p. "Status usuario primario
      ls_usrstatus-langu = sy-langu.      " Linguagem
      clear ls_usrstatus-inactive.        " '' = ativar, 'X' = inativar
      append ls_usrstatus to lt_usrstatus.
    endif.

    if <ls_dados>-status_s is not initial.
      "Verifica o status segundario atual e remove

      select a~aufnr, t~txt04 from aufk as a
      inner join jest as j on a~objnr eq j~objnr
       inner join tj30t as t on t~estat = j~stat
       inner join tj30 as i  on i~estat = t~estat and i~stsma = t~stsma
      into table @data(it_status)
      where aufnr eq @zvar_ordem
      and t~spras = @sy-langu
      and t~stsma = 'ZPM00030'
      and j~inact = ''
      and i~stonr eq @space.
      if sy-subrc eq 0.
        loop at it_status assigning field-symbol(<ws_status>).
          if <ws_status>-txt04 ne <ls_dados>-status_s.
            add 1 to var_seq_status.
            ls_method-refnumber = var_seq_status.
            ls_method-objecttype = 'USERSTATUS'.
            ls_method-method = 'CHANGE'.
            ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
            append ls_method to lt_methods.

            clear: ls_usrstatus.
            ls_usrstatus-user_st_text = <ws_status>-txt04. "Status usuario
            ls_usrstatus-langu = sy-langu.       " Linguagem
            ls_usrstatus-inactive = abap_true.          " '' = ativar, 'X' = inativar
            append ls_usrstatus to  lt_usrstatus.
          endif.
        endloop.
      endif.


      add 1 to var_seq_status.
      ls_method-refnumber = var_seq_status.
      ls_method-objecttype = 'USERSTATUS'.
      ls_method-method = 'CHANGE'.
      ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
      append ls_method to lt_methods.

      clear: ls_usrstatus.
      ls_usrstatus-user_st_text = <ls_dados>-status_s. "Status usuario
      ls_usrstatus-langu = sy-langu.       " Linguagem
      clear ls_usrstatus-inactive.          " '' = ativar, 'X' = inativar
      append ls_usrstatus to  lt_usrstatus.
    endif.


* Define o status do usuário
*    //===============//=======================
*    ls_method-refnumber = '00000001'.
*    ls_method-objecttype = 'USERSTATUS'.
*    ls_method-method = 'CHANGE'.
*    ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
*    append ls_method to lt_methods.
*
*    clear: ls_usrstatus.
*    ls_usrstatus-user_st_text = 'EPRO'. "Status usuario
*    ls_usrstatus-langu = sy-langu.      " Linguagem
*    clear ls_usrstatus-inactive.        " '' = ativar, 'X' = inativar
*    append ls_usrstatus to lt_usrstatus.
*
*    ls_method-refnumber = '00000002'.
*    ls_method-objecttype = 'USERSTATUS'.
*    ls_method-method = 'CHANGE'.
*    ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
*    append ls_method to lt_methods.
*
*    clear: ls_usrstatus.
*    ls_usrstatus-user_st_text = 'PROG'. "Status usuario
*    ls_usrstatus-langu = sy-langu.       " Linguagem
*    clear ls_usrstatus-inactive.          " '' = ativar, 'X' = inativar
*    append ls_usrstatus to lt_usrstatus.
** Fim define o status do usuário
*    //===============//=======================
* Fim define o status do usuário
*    //===============//=======================

    select a~aufnr, b~vornr
    from afko as a
    inner join afvc as b on b~aufpl eq a~aufpl
    into table @data(it_oper)
    where a~aufnr eq @zvar_ordem.
    if sy-subrc eq 0.

      "Conveter virgula por ponto.
      replace all occurrences of ',' in <ls_dados>-trab_oper with '.'.

      describe table it_oper lines lv_quant_oper.
      lv_TrabOper   = <ls_dados>-trab_oper / lv_quant_oper.
      lv_quant_oper = ( <ls_dados>-quant_pessoa * <ls_dados>-trab_oper ) / lv_quant_oper.

      lv_oper_no = zvar_ordem.
      if lv_TrabOper > 0 and lv_quant_oper > 0 and <ls_dados>-quant_pessoa > 0.
        loop at it_oper into data(ws_oper).
          lv_oper_no+12(4) = ws_oper-vornr.
          add 1 to lv_refnum.
          ls_method-refnumber = lv_refnum.
          ls_method-objecttype = 'OPERATION'.
          ls_method-method = 'CHANGE'.
          ls_method-objectkey = lv_oper_no. " <- ordem com zeros à esquerda
          append ls_method to lt_methods.

          append value #( duration_normal = lv_TrabOper  work_activity = lv_quant_oper  number_of_capacities =  <ls_dados>-quant_pessoa ) to it_operation.
          append value #( duration_normal = abap_true  work_activity = abap_true  number_of_capacities =  abap_true ) to it_operation_up.
        endloop.
      endif.
    endif.


    clear lt_return[].
*    lo_bapis->bapi_alm_order_maintain(
*      changing
*        ct_methods      = lt_methods
*        ct_header       = lt_header
*        ct_header_up    = lt_header_up
*        ct_operation    = it_operation
*        ct_operation_up = it_operation_up
*        ct_userstatus   = lt_usrstatus
*        ct_return       = et_return
**    ).


    call function 'BAPI_ALM_ORDER_MAINTAIN'
      tables
        it_methods      = lt_methods
        it_header       = lt_header
        it_header_up    = lt_header_up
        it_operation    = it_operation
        it_operation_up = it_operation_up
        it_userstatus   = lt_usrstatus
        return          = et_msg.

    " Verifica retorno
    read table et_msg into data(ls_retorn) with key type = 'E'.
    if sy-subrc = 0.
      " erro - rollback
      call function 'BAPI_TRANSACTION_ROLLBACK'.
*       tratar erro

*      move-corresponding ls_retorn to ws_return.
*      append ws_return to et_msg.

    else.
*       sucesso - commit
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      "alterar o segundo status da ordem.
*      move-corresponding ls_retorn to ws_return.
*      append ws_return to et_msg.
    endif.
  endloop.






endfunction.
