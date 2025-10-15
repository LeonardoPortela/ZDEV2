function z_zw_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_OPERACOES STRUCTURE  ZFI_OPERACOES_ZNFW
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_ZNFW
*"      T_DOCS STRUCTURE  ZFI_DOCS_ZNFW
*"----------------------------------------------------------------------
*{   INSERT         DEVK9A297X                                        1

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  type-pools: icon.

  types:

    begin of ty_operacoes.
      include structure zfi_operacoes_imp.
  types: mark type c,
    end of ty_operacoes,

    begin of ty_estra.
      include structure zfi_estrategia_znfw.
  types: mark type c,
    end of ty_estra,

    begin of ty_docs.
      include structure zfi_docs_znfw.
  types: mark type c,
    end of ty_docs.

  types:
    begin of ty_libe,
      nivel type zfiwrt0033-nivel,
    end   of ty_libe.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  data: vflag(1),
        tg_operacoes type table of ty_operacoes,
        wg_operacoes type ty_operacoes.


** Criação de tabela dinamica
  data: wa_zfiwrt0001 type zfiwrt0001,
        wa_zfiwrt0033 type zfiwrt0033,
        wa_zfiwrt0034 type zfiwrt0034,
        wa_estra      type ty_estra,
        wa_docs       type ty_docs,
        tg_docs       type table of ty_docs,
        it_zfiwrt0001 type table of zfiwrt0001,
        it_zfiwrt0033 type table of zfiwrt0033,
        it_zfiwrt0034 type table of zfiwrt0034,

        it_estra      type table of ty_estra.


  data vflg_ico(1).

  select *
  from zfiwrt0001
  into table  it_zfiwrt0001
  where status_aprov  eq  'L'.

  check it_zfiwrt0001[] is not initial.

  select  *
  from zfiwrt0033
  into table it_zfiwrt0033
  for all entries in it_zfiwrt0001
  where dep_resp = it_zfiwrt0001-dep_resp.

  select  *
  from zfiwrt0034
  into table it_zfiwrt0034
  for all entries in it_zfiwrt0001
  where operacao = it_zfiwrt0001-operacao.


  sort: it_zfiwrt0033 by nivel ascending dep_resp ascending aprovador ascending,
        it_zfiwrt0001 by operacao ascending,
        it_zfiwrt0034 by operacao nivel aprovador ascending.

  refresh: tg_operacoes, tg_docs,it_estra.

  loop at it_zfiwrt0001 into wa_zfiwrt0001.
    wg_operacoes-operacao  = wa_zfiwrt0001-operacao.
    wg_operacoes-dep_resp  = wa_zfiwrt0001-dep_resp.
    wg_operacoes-descricao = wa_zfiwrt0001-descricao.

    wa_docs-operacao    = wa_zfiwrt0001-operacao.
    wa_docs-txt_compl   = wa_zfiwrt0001-txt_compl.
    wa_docs-dep_resp    = wa_zfiwrt0001-dep_resp.
    wa_docs-dt_ini_val  = wa_zfiwrt0001-dt_ini_val.
    wa_docs-dt_fim_val  = wa_zfiwrt0001-dt_fim_val.

    clear vflg_ico.
    loop at it_zfiwrt0033 into wa_zfiwrt0033 where dep_resp = wa_zfiwrt0001-dep_resp.

      if ( wa_zfiwrt0033-dt_val_de  lt sy-datum and
        wa_zfiwrt0033-dt_val_ate gt sy-datum )
      or
      (
        wa_zfiwrt0033-dt_val_de  eq sy-datum and
        wa_zfiwrt0033-dt_val_ate eq sy-datum and
        wa_zfiwrt0033-hr_val_de  le sy-uzeit and
        wa_zfiwrt0033-hr_val_ate ge sy-uzeit )
      or
      (
        wa_zfiwrt0033-dt_val_de  eq sy-datum and
        wa_zfiwrt0033-dt_val_ate gt sy-datum and
        wa_zfiwrt0033-hr_val_de  le sy-uzeit )
      or
      (
        wa_zfiwrt0033-dt_val_de  lt sy-datum and
        wa_zfiwrt0033-dt_val_ate eq sy-datum and
        wa_zfiwrt0033-hr_val_ate ge sy-uzeit ).

        read table it_zfiwrt0034   into wa_zfiwrt0034 with key operacao = wa_zfiwrt0001-operacao
                                                               nivel    = wa_zfiwrt0033-nivel binary search.

        if sy-subrc = 0.
          if ( wa_zfiwrt0034-aprovador = wa_zfiwrt0033-aprovador ).
            wa_estra-estado       = icon_checked .
            wa_estra-opcoes       = icon_system_undo .
            vflg_ico = 'N'.
          else.
            continue.
          endif.
        elseif vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
        else.
          if v_usuario ne wa_zfiwrt0033-aprovador.
            wa_estra-estado       =  ' '.
            wa_estra-opcoes       = icon_led_yellow  .
          else.
            wa_estra-estado       = icon_led_yellow .
            wa_estra-opcoes       = icon_set_state  .
          endif.
          vflg_ico = 'X'.
        endif.

        if vflg_ico = 'X'.
          vflg_ico = 'S'.
        endif.

        wa_estra-operacao     = wa_zfiwrt0001-operacao.
        wa_estra-dep_resp     = wa_zfiwrt0033-dep_resp.
        wa_estra-aprovador    = wa_zfiwrt0033-aprovador.
        wa_estra-nivel        = wa_zfiwrt0033-nivel.

        append wa_estra to it_estra.
      endif.
    endloop.

    append wg_operacoes to tg_operacoes.
    clear wg_operacoes.
    append wa_docs to t_docs.
    clear wa_docs.
  endloop.

  if tg_operacoes[] is not initial.
    sort it_estra by operacao aprovador.
    loop at tg_operacoes into wg_operacoes.
      clear vflag.
      loop at it_estra into wa_estra where operacao = wg_operacoes-operacao
      and   aprovador = v_usuario.
        vflag = 'X'.
        exit.
      endloop.
      loop at it_estra into wa_estra where operacao = wg_operacoes-operacao.
        move-corresponding wa_estra to t_estra.
        append t_estra.
      endloop.
      if vflag = 'X'.
        move-corresponding wg_operacoes to t_operacoes.
        append t_operacoes.
      endif.
    endloop.

    sort t_estra by operacao dep_resp nivel.

*
    if t_operacoes[] is not initial.
      msg = 'Sucesso'.
    else.
      msg = 'Não há operacoes à aprovar.'.
    endif.

  endif.

*}   INSERT
endfunction.
