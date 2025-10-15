function zpm_aprovar_suplementacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ORDEM) TYPE  AUFNR
*"     REFERENCE(USUARIO) TYPE  UNAME
*"  EXPORTING
*"     REFERENCE(ID_ORCAMENTO) TYPE  BELNR_D
*"     REFERENCE(E_PROX_APROV) TYPE  STRING
*"  TABLES
*"      ERRORS STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  data lw_bpak   type bpak.
  data lt_bpak   type table of bpak.
  data lt_return like bapiret2 occurs 0 with header line.
  data user_data    type alm_me_user_data.
  data order_header type alm_me_order_header.
  data user_profile type alm_me_c010prf.
  data lv_ordem type aufnr.
  data: msg               type bapi_msg,
        lv_check_aprov_cc type char01. "Verifica se o aprovador esta na tabela de centro.


  clear: lw_bpak, lt_bpak.
  free: errors.

  data(lv_usuario) = usuario.

  if lv_usuario is initial.
    lv_usuario = sy-uname.
  endif.
** Pega informações detalhada da ordem
  lv_ordem = |{ ordem alpha = in }|.

  select single *
    from zpmr0006
    into @data(_suplemento)
   where aufnr = @lv_ordem
    and status = 'P'.

  select sum( vlr_estimado )
    from zpmr0006
    into @data(_vlr_estimado)
   where aufnr = @lv_ordem.


  call function 'ALM_ME_ORDER_GETDETAIL'
    exporting
      orderid       = lv_ordem
      resource      = abap_true
      userdata      = user_data
      order_profile = user_profile
    importing
      order_header  = order_header
    exceptions
      read_error    = 1.

  data(_valor_aprovacao)  = ( order_header-estimated_costs + _vlr_estimado ).

  select max( nivel )
  from zpmr0002
  into @data(nivel)
  where centro_desp eq @_suplemento-werks
  and usua_subst eq @lv_usuario "usua_subst
  and data_lim   >= @sy-datum
  and nivel <= (  select nivel
                    from zpmr0002
                    where centro_desp eq @_suplemento-werks
                    and usua_subst eq @lv_usuario "usua_subst
                    and data_lim   >= @sy-datum
                    and ( valor_de  <= @_valor_aprovacao and valor_ate >= @_valor_aprovacao )
               ).
*** Stefanini - IR238730 - 20/05/2025 - LAZAROSR - Início de Alteração
*  if sy-subrc ne 0.
  if ( nivel ) is initial.
*** Stefanini - IR238730 - 20/05/2025 - LAZAROSR - Fim de Alteração
    select max( nivel )
     from zpmr0011
     into @data(nivel_aux)
     where centro_desp eq @_suplemento-werks
     and usua_subst eq @lv_usuario "usua_subst
     and data_lim   >= @sy-datum
     and nivel <= (  select nivel
                       from zpmr0011
                       where centro_desp eq @_suplemento-werks
                       and usua_subst eq @lv_usuario "usua_subst
                       and data_lim   >= @sy-datum
                       and ( valor_de  <= @_valor_aprovacao and valor_ate >= @_valor_aprovacao )
                  ).
    if sy-subrc eq 0.
      move nivel_aux to nivel.
    endif.
  endif.

  if ( nivel ) is initial.
    select max( nivel )
    from zpmr0002
    into nivel
    where centro_desp eq _suplemento-werks
    and aprovador eq lv_usuario
    and nivel <= (  select nivel
                      from zpmr0002
                      where centro_desp eq _suplemento-werks
                      and aprovador eq lv_usuario
                      and ( valor_de  <= _valor_aprovacao and valor_ate >= _valor_aprovacao )
                 ).
*** Stefanini - IR238730 - 20/05/2025 - LAZAROSR - Início de Alteração
*    if sy-subrc ne 0.
    if ( nivel ) is initial.
*** Stefanini - IR238730 - 20/05/2025 - LAZAROSR - Fim de Alteração
      select max( nivel )
        from zpmr0011
        into nivel_aux
        where centro_desp eq _suplemento-werks
        and aprovador eq lv_usuario
        and nivel <= (  select nivel
                          from zpmr0011
                          where centro_desp eq _suplemento-werks
                          and aprovador eq lv_usuario
                          and ( valor_de  <= _valor_aprovacao and valor_ate >= _valor_aprovacao )
                     ).
      if sy-subrc eq 0.
        move nivel_aux to nivel.
      endif.
    endif.
  endif.

**  Begin of    #XXXXX  FF -  14.04.2023
  select single nivel
    from zpmr0002
    into @data(lv_nivel)
    where centro_desp = @_suplemento-werks
      and aprovador   = @lv_usuario.
  if sy-subrc ne 0.
    select single nivel
    from zpmr0011
    into @data(lv_nivel_aux)
    where centro_desp = @_suplemento-werks
      and aprovador   = @lv_usuario.
    if sy-subrc eq 0.
      move lv_nivel_aux to lv_nivel.
    endif.
  endif.

  clear: lv_check_aprov_cc.
  if sy-subrc = 0.
    data(lv_prox_nivel) = lv_nivel + 1.

    "Vericar na tabela zpmr0002
*    select *
*     from zpmr0002
*      into table @data(lt_zpmr0002)
*      where centro_desp = @_suplemento-werks
*        and aprovador   = @sy-uname
*        and nivel       = @lv_nivel.
*    if sy-subrc eq 0.
*      lv_check_aprov_cc = abap_false.
*    endif.

*    if lv_check_aprov_cc eq abap_false.
      select *
       from zpmr0002
        into table @data(lt_prox_aprov)
        where centro_desp = @_suplemento-werks
          and nivel       = @lv_prox_nivel.
*    else.
      select *
      from zpmr0011
       into table @data(lt_prox_aprov_aux)
       where centro_desp = @_suplemento-werks
         and nivel       = @lv_prox_nivel.
      if sy-subrc eq 0.
*        move-corresponding lt_prox_aprov_aux to lt_prox_aprov.
        append initial line to lt_prox_aprov. "US # - MMSILVA - 08.05.2025
        move-corresponding lt_prox_aprov_aux to lt_prox_aprov.
      endif.
*    endif.

    if sy-subrc = 0.
      loop at lt_prox_aprov assigning field-symbol(<fs_aprov>).
        if sy-tabix = 1.

          if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
            concatenate <fs_aprov>-usua_subst ',' into e_prox_aprov.
          else.
            concatenate <fs_aprov>-aprovador ',' into e_prox_aprov.
          endif.

        else.

          if <fs_aprov>-usua_subst is not initial and <fs_aprov>-data_lim >= sy-datum.
            concatenate <fs_aprov>-usua_subst ',' e_prox_aprov into e_prox_aprov.
          else.
            concatenate <fs_aprov>-aprovador ',' e_prox_aprov into e_prox_aprov.
          endif.

        endif.
      endloop.
    endif.
  endif.
** End of FF

  add 1 to _suplemento-nivel_aprovado.

  if nivel eq _suplemento-nivel_aprovado.

    lt_bpak = value #( (
                e_objnr = _suplemento-object
                bldat = sy-datum
                wert = _suplemento-vlr_estimado
                sgtext = 'Automático'
                twaer = _suplemento-currency
             ) ).

    call function 'KBPP_EXTERN_UPDATE_CO'
      exporting
        i_budget_activity      = 'KBUD'
        i_budget_activ_sup_ret = 'X'
        i_delta_amounts        = 'X'
        i_rollup_data          = 'X'
        i_check_plan_data      = 'X'
        i_commit_all           = 'X'
      tables
        it_bpak                = lt_bpak
        it_return              = errors
      exceptions
        no_update              = 1
        others                 = 2.

    loop at lt_bpak into lw_bpak.
      if lw_bpak-belnr is initial.
        continue.
      else.
        _suplemento-belnr = lw_bpak-belnr.
      endif.
    endloop.

    "Verificando se gravou o documento de suplementação.
    zcl_ordem_man=>m_check_orc_supl_ord(
      exporting
        i_aufnr  = lv_ordem  " Nº ordem
      importing
        i_retorn = data(i_return)   " Campo de texto do comprimento 1
        i_belnr  = data(i_belnr)   " Nº documento para atribuição de orçamento e planej.estrutura
    ).

    id_orcamento = i_belnr.

    if i_belnr is initial.
      errors = value #( type        = 'E'
                        message     = 'Documento de suplementação não foi gerado, verificar!' ).
    endif.


    data(errors_) = errors[].
    delete errors where type <> 'E'.

    if errors[] is not initial.
      call function 'Z_GRAVA_LOG_PM'
        tables
          t_return = errors.
    else.

      if _suplemento-belnr is initial.
        errors[] = errors_[].
        call function 'Z_GRAVA_LOG_PM'
          tables
            t_return = errors.
      else.



        _suplemento-status         = 'L'.
        _suplemento-responsavel    = lv_usuario.
        _suplemento-dt_modificacao = sy-datum.
        _suplemento-belnr          = _suplemento-belnr.

        modify zpmr0006 from _suplemento.

        delete from zpmr0006 where aufnr = _suplemento-aufnr and status = 'P'.
        commit work.

        msg = |Suplementação Liberada com o Custo Inicial de { _vlr_estimado } Nivel: { _suplemento-nivel_aprovado }!|.

        call function 'Z_GRAVA_LOG_PM'
          exporting
            i_tp_msg   = 'S'
            i_mensagem = msg
            i_tcode    = sy-tcode.
      endif.
    endif.

  else.

    update zpmr0006
       set nivel_aprovado = _suplemento-nivel_aprovado
           responsavel    = lv_usuario
           dt_modificacao = sy-datum
           status = 'P'
     where aufnr eq lv_ordem
       and belnr eq '0000000000'.

    msg = |Suplementação Aprovada com o Custo Inicial de { _vlr_estimado } Nivel: { _suplemento-nivel_aprovado }!|.

    call function 'Z_GRAVA_LOG_PM'
      exporting
        i_tp_msg   = 'S'
        i_mensagem = msg
        i_tcode    = sy-tcode.


  endif.
endfunction.
