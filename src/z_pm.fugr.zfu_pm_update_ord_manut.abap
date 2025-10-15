function zfu_pm_update_ord_manut.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_DADOS_ORDEM TYPE  ZPMT0082_T
*"----------------------------------------------------------------------





  data: lt_methods   type table of bapi_alm_order_method,
        lt_header    type table of bapi_alm_order_headers_i,
        lt_header_up type table of bapi_alm_order_headers_up,
        zvar_ordem   type aufnr,
        lt_return    type table of bapiret2.

  data: ls_method    type bapi_alm_order_method,
        ls_header    type bapi_alm_order_headers_i,
        ls_header_up type bapi_alm_order_headers_up.

  free: lt_methods, lt_header, ls_header_up.


  check t_dados_ordem is not initial.

   loop at t_dados_ordem assigning field-symbol(<ls_dados>).
    zvar_ordem = |{ <ls_dados>-aufnr alpha = out }|.
    zvar_ordem = |{ zvar_ordem alpha = in }|.

    ls_method-refnumber = '00000001'.
    ls_method-objecttype = 'HEADER'.
    ls_method-method = 'CHANGE'.
    ls_method-objectkey = zvar_ordem. " <- ordem com zeros à esquerda
    append ls_method to lt_methods.

    clear ls_method.
    ls_method-refnumber = '00000001'. " <- mesmo número usado acima!
*      ls_method-objecttype = ''.
    ls_method-method = 'SAVE'.
*      ls_method-objectkey = zvar_ordem.
    append ls_method to lt_methods.

    ls_header-orderid = zvar_ordem. "Numero da ordem
    ls_header-revision = <ls_dados>-revisao.   "Revisão
    ls_header-start_date = <ls_dados>-data_ini. "Inicio base
    ls_header-finish_date = <ls_dados>-data_fim. "Fim base
    append ls_header to lt_header.

    ls_header_up-orderid  = zvar_ordem.
    ls_header_up-revision = 'X'.
    ls_header_UP-start_date  = 'X'.
    ls_header_UP-finish_date = 'X'.
    append ls_header_up to lt_header_up.

    call function 'BAPI_ALM_ORDER_MAINTAIN'
      tables
        it_methods   = lt_methods
        it_header    = lt_header
        it_header_up = lt_header_up
        return       = lt_return.

*    " Verifica retorno
*    read table lt_return into data(ls_retorn) with key type = 'E'.
*    if sy-subrc = 0.
      " erro - rollback
*        call function 'BAPI_TRANSACTION_ROLLBACK'.
      " tratar erro
*    else.
      " sucesso - commit
*        call function 'BAPI_TRANSACTION_COMMIT'
*          exporting
*            wait = 'X'.
*    endif.

  endloop.


endfunction.
