function zpm_aprovar_permit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_AUFNR) TYPE  AUFK-AUFNR
*"     REFERENCE(I_APROVADOR) TYPE  UNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(RETURN) TYPE  BAPIRET2
*"     REFERENCE(ID_ORCAMENTO) TYPE  BELNR_D
*"     REFERENCE(E_PROX_APROV) TYPE  STRING
*"  TABLES
*"      T_ZPMR0002 STRUCTURE  ZPMR0002 OPTIONAL
*"  EXCEPTIONS
*"      NO_PERMISSION
*"      PERMISSION_GRANTED
*"----------------------------------------------------------------------
  types:
    begin of ty_ihsg,
      objnr      type ihsg-objnr,
      counter    type ihsg-counter,
      ernam      type ihsg-ernam,
      pmsog      type ihsg-pmsog,
      nivel      type zpmr0002-nivel,
      aprovador  type zpmr0002-aprovador,
      usua_subst type zpmr0002-usua_subst,
      data_lim   type zpmr0002-data_lim,
      check(1),
    end of ty_ihsg.

  data: lv_aufnr        type aufk-aufnr,
        lv_objnr        type aufk-objnr,
        lv_orderid      type alm_me_order_header-orderid,
        lv_erro         type oax,
        lv_num_permits  type numc2,
        gw_order_header type alm_me_order_header,
        gw_user_data    type alm_me_user_data,
        gw_user_profile type alm_me_c010prf.

  data: gt_gnsvb          like gnsvb                       occurs 0 with header line,
*        GT_ORDER_OPER   LIKE ALM_ME_ORDER_OPERATION      OCCURS 0 WITH HEADER LINE,
        gt_methods        type bapi_alm_order_method       occurs 0 with header line,
        gt_header         type bapi_alm_order_headers_i    occurs 0 with header line,
        gt_header_up      type bapi_alm_order_headers_up   occurs 0 with header line,
        gt_partner        type bapi_alm_order_partn_mul    occurs 0 with header line,
        gt_partner_up     type bapi_alm_order_partn_mul_up occurs 0 with header line,
        gt_operation      type bapi_alm_order_operation    occurs 0 with header line,
        gt_operation_up   type bapi_alm_order_operation_up occurs 0 with header line,
        gt_numbers        type bapi_alm_numbers            occurs 0 with header line,
        it_ihsg           type table of ty_ihsg,
        gw_bpge           type bpge,
        gt_return         like bapiret2                    occurs 0 with header line,
        gw_return         like bapiret2,
        gt_bpak           type table of bpak,
        gw_bpak           type bpak,
        gt_ihgns          type table of ihgns,
        gw_ihgns          type ihgns,
        gt_ihsg           type table of ihsg,
        gw_ihsg           type ihsg,
        gt_zpmr0002       type table of zpmr0002,
        gt_zpmr0011       type table of zpmr0011,
        zt_zpmr0002       type table of zpmr0002,
        gw_zpmr0002       type zpmr0002,
        zw_zpmr0002       type zpmr0002,
        nivel_            type numc10,
        r_nivel           type range of zpmr0002-nivel,
        lt_prox_aprov     type table of zpmr0002,
        lt_prox_aprov_aux type table of zpmr0011.

DATA: gt_zpmr0002_aux type table of zpmr0002 with header line.

** Pega informações detalhada da ordem
  lv_orderid =  |{ i_aufnr alpha = in }|.

  call function 'ALM_ME_ORDER_GETDETAIL'
    exporting
      orderid       = lv_orderid
      resource      = 'X'
      userdata      = gw_user_data
      order_profile = gw_user_profile
    importing
      order_header  = gw_order_header
    exceptions
      read_error    = 1.

  if sy-subrc is initial.
** Verificar se usuário tem permissão

    if i_aprovador is not initial.

      select *
      from zpmr0002
      into table gt_zpmr0002
      where aprovador eq i_aprovador
         or usua_subst eq i_aprovador.
*      if sy-subrc ne 0.
        select *
            from zpmr0011
            into corresponding fields of table gt_zpmr0011
            where aprovador  eq sy-uname
            or usua_subst eq sy-uname.
        if sy-subrc eq 0.
          move-corresponding gt_zpmr0011[] to gt_zpmr0002_aux[]. "US # - MMSILVA - 08.05.2025
          append lines of gt_zpmr0002_aux[] to gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025
        endif.

        delete adjacent duplicates from gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025
*      endif.

    else.

      select *
        from zpmr0002
        into table gt_zpmr0002
        where aprovador eq sy-uname
           or usua_subst eq sy-uname.
*      if sy-subrc ne 0.
        select *
        from zpmr0011
         into corresponding fields of table gt_zpmr0011
        where aprovador eq sy-uname
             or usua_subst eq sy-uname.
        if sy-subrc eq 0.
          move-corresponding gt_zpmr0011[] to gt_zpmr0002_aux[]. "US # - MMSILVA - 08.05.2025
          append lines of gt_zpmr0002_aux[] to gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025
        endif.

        delete adjacent duplicates from gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025
*      endif.
    endif.



    loop at gt_zpmr0002 assigning field-symbol(<w_zpmr0002>).
      if <w_zpmr0002>-aprovador is not initial
        and <w_zpmr0002>-usua_subst is not initial
        and <w_zpmr0002>-data_lim >= sy-datum.
        <w_zpmr0002>-aprovador = <w_zpmr0002>-usua_subst.
      endif.
    endloop.

    if i_aprovador is not initial.

      delete gt_zpmr0002 where aprovador ne i_aprovador.

    else.

      delete gt_zpmr0002 where aprovador ne sy-uname.

    endif.

    delete gt_zpmr0002 where centro_desp ne gw_order_header-planplant.

**  Begin of    #XXXXX  FF -  14.04.2023
    read table gt_zpmr0002 assigning field-symbol(<fs>) index 1.
    if <fs> is assigned.

      data(lv_prox_nivel) = <fs>-nivel + 1.
      select *
       from zpmr0002
        into table lt_prox_aprov
        where centro_desp = gw_order_header-planplant
          and nivel       = lv_prox_nivel.
*      if sy-subrc ne 0.
        select *
        from zpmr0011
        into table lt_prox_aprov_aux
        where centro_desp = gw_order_header-planplant
        and nivel         = lv_prox_nivel.
        if sy-subrc eq 0.
          DATA: lt_prox_aprov_aux2 type table of zpmr0002. "US # - MMSILVA - 08.05.2025
          move-corresponding lt_prox_aprov_aux[] to lt_prox_aprov_aux2[]. "US # - MMSILVA - 08.05.2025
          append lines of lt_prox_aprov_aux2[] to lt_prox_aprov[]. "US # - MMSILVA - 08.05.2025
        endif.

        delete adjacent duplicates from lt_prox_aprov[]. "US # - MMSILVA - 08.05.2025
*      endif.

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

    select distinct a~objnr a~counter a~ernam a~pmsog b~nivel b~aprovador b~usua_subst b~data_lim
    from ihsg as a
    inner join zpmr0002 as b on b~permit eq a~pmsog
    inner join ihgns as c on c~objnr eq a~objnr and c~counter eq a~counter
    into corresponding fields of table it_ihsg
    where a~objnr eq gw_order_header-object_no
    and c~geniakt eq abap_false
    and b~centro_desp = gw_order_header-plant.
    if sy-subrc ne 0.
      select distinct a~objnr a~counter a~ernam a~pmsog b~nivel b~aprovador b~usua_subst b~data_lim
      from ihsg as a
      inner join zpmr0011 as b on b~permit eq a~pmsog
      inner join ihgns as c on c~objnr eq a~objnr and c~counter eq a~counter
      into corresponding fields of table it_ihsg
      where a~objnr eq gw_order_header-object_no
      and c~geniakt eq abap_false
      and b~centro_desp = gw_order_header-plant.
    endif.

    r_nivel = value #( for ls in it_ihsg ( sign = 'I' option = 'EQ' low = ls-nivel ) ).

    if r_nivel is not initial.
      delete gt_zpmr0002 where nivel in r_nivel.
    endif.

    loop at it_ihsg assigning field-symbol(<zt_ihsg>).
      if <zt_ihsg>-aprovador is not initial
        and <zt_ihsg>-usua_subst is not initial
        and <zt_ihsg>-data_lim >= sy-datum.
        <zt_ihsg>-aprovador = <zt_ihsg>-usua_subst.
      endif.
    endloop.

    sort gt_zpmr0002 by nivel.

    loop at gt_zpmr0002 assigning field-symbol(<w002>).

      if nivel_ is initial.
        nivel_ = <w002>-nivel.
        continue.
      endif.

      add 1 to nivel_.

      if nivel_ eq <w002>-nivel.
        continue.
      else.
        clear <w002>.
      endif.

    endloop.

    delete gt_zpmr0002 where nivel is initial.

    if gt_zpmr0002 is initial.
      return = value #( type = 'E' message = 'Não possui permissão' ).
    else.
      select *
        from ihsg
        into table gt_ihsg
        where objnr eq gw_order_header-object_no
         and  lvorm eq abap_false
        order by counter.

      loop at gt_zpmr0002 into gw_zpmr0002 where centro_desp cs gw_order_header-planplant.
** Checa se encontra permit para o nível adequado do usuário
        free: gt_gnsvb[].

        read table gt_ihsg into gw_ihsg with key pmsog = gw_zpmr0002-permit.

        if sy-subrc is initial.
** Checa se há histórico de liberação da permit
          select *
            into table gt_ihgns
            from ihgns
            where objnr   eq gw_ihsg-objnr
             and  counter eq gw_ihsg-counter.

          if gt_ihgns[] is not initial.
** Libera permit quando já existe histórico
            sort gt_ihgns by gendatum descending gentime descending counter ascending.
            read table gt_ihgns into gw_ihgns index 1.

            if gw_ihgns-geniakt is initial.
              return = value #( type = 'E' message = 'Permissão já concedida' ).
            else.

              gt_gnsvb-objnr    = gw_order_header-object_no.
              gt_gnsvb-counter  = gw_ihgns-counter.
              gt_gnsvb-counters = gw_ihgns-counters.
              add 1 to gt_gnsvb-counters.
              gt_gnsvb-genpaus  = 'X'.

              if i_aprovador is initial.

                gt_gnsvb-genname  = sy-uname.

              else.
                gt_gnsvb-genname  = i_aprovador.

              endif.

              gt_gnsvb-gendatum = sy-datum.
              gt_gnsvb-gentime  = sy-uzeit.

              if i_aprovador is initial.

                gt_gnsvb-genvname = sy-uname.

              else.

                gt_gnsvb-genvname = i_aprovador.

              endif.

              gt_gnsvb-aktiv    = '2'.

              clear: gt_gnsvb-geniakt,
                     gt_gnsvb-geniname,
                     gt_gnsvb-genidate,
                     gt_gnsvb-genitime.

              append gt_gnsvb.

              call function 'PM_SP_ISSUE_POST' in update task
                exporting
                  i_belgnstab = gt_gnsvb[].
*
              commit work.
              wait up to 2 seconds.

              return = value #( type = 'S' message = 'Permissão concedida.' ).
            endif.
          else.

** Libera a permit quando ainda não há histórico
            gt_gnsvb-objnr    = gw_order_header-object_no.
            gt_gnsvb-counter  = gw_ihsg-counter.
            gt_gnsvb-counters = 1.
            gt_gnsvb-genpaus  = 'X'.
            if i_aprovador is initial.

              gt_gnsvb-genname  = sy-uname.

            else.
              gt_gnsvb-genname  = i_aprovador.

            endif.

            gt_gnsvb-gendatum = sy-datum.
            gt_gnsvb-gentime  = sy-uzeit.

            if i_aprovador is initial.

              gt_gnsvb-genvname = sy-uname.

            else.

              gt_gnsvb-genvname = i_aprovador.

            endif.

            gt_gnsvb-aktiv    = '2'.

            clear: gt_gnsvb-geniakt,
                   gt_gnsvb-geniname,
                   gt_gnsvb-genidate,
                   gt_gnsvb-genitime.

            append gt_gnsvb.

            call function 'PM_SP_ISSUE_POST' in update task
              exporting
                i_belgnstab = gt_gnsvb[].

            commit work.
            wait up to 2 seconds.

            return = value #( type = 'S' message = 'Permissão concedida.' ).

          endif.
** Checando permissão de ordem
          if  gw_order_header-estimated_costs le gw_zpmr0002-valor_ate
          or  gw_zpmr0002-valor_ate eq 0.

** Buscar dados de orçamento de ordem
            select single *
              from bpge
              into gw_bpge
              where objnr = gw_order_header-object_no
               and  vorga = 'KBUD'.

            if sy-subrc is not initial.

              gt_bpak = value #( (
                                    e_objnr = gw_order_header-object_no
                                    bldat   = sy-datum
                                    wert    = gw_order_header-estimated_costs
                                    sgtext  = 'Automático'
                                    twaer   = gw_order_header-currency
                                ) ).

**    Libera orçamento para ordem
              if sy-tcode <> 'ZPM0013'.
                sy-uname = i_aprovador. "Para a liberaçào do orçamento ficar com o usuário aprovador e não o PMOBILE.
              endif.

              call function 'KBPP_EXTERN_UPDATE_CO'
                exporting
                  i_budget_activity = 'KBUD'
                  i_commit_all      = abap_true
                importing
                  e_errors_found    = lv_erro
                tables
                  it_bpak           = gt_bpak
                  it_return         = gt_return
                exceptions
                  no_update         = 1.

              if not line_exists( gt_return[ type = 'E' ] ).

                commit work.
                wait up to 2 seconds.

                call function 'Z_GRAVA_LOG_PM'
                  tables
                    t_return = gt_return.
              else.

                call function 'Z_GRAVA_LOG_PM'
                  tables
                    t_return = gt_return.
                exit.
              endif.

            endif.

            call function 'Z_LISTA_ORDEM_PERMITS'
              exporting
                i_user  = i_aprovador
                i_aufnr = lv_orderid
              importing
                e_lines = lv_num_permits.

            if lv_num_permits is initial.



              append value #(
                              refnumber  = 1
                              objecttype = 'HEADER'
                              method     = 'RELEASE'
                              objectkey  = gw_order_header-orderid
                            ) to gt_methods.

              append value #(
                              refnumber  = 1
                              objecttype = ''
                              method     = 'SAVE'
                              objectkey  = gw_order_header-orderid
                            ) to gt_methods.

              append value #(
                              orderid =  gw_order_header-orderid
                             ) to gt_header.

              if sy-tcode <> 'ZPM0013'.
                sy-uname = i_aprovador. "Para a liberaçào do orçamento ficar com o usuário aprovador e não o PMOBILE.
              endif.

              call function 'BAPI_ALM_ORDER_MAINTAIN'
                tables
                  it_methods = gt_methods
                  it_header  = gt_header
*                 IT_HEADER_UP    = GT_HEADER_UP
*                 IT_PARTNER = GT_PARTNER
*                 IT_PARTNER_UP   = GT_PARTNER_UP
*                 IT_OPERATION    = GT_OPERATION
*                 IT_OPERATION_UP = GT_OPERATION_UP
                  return     = gt_return.
*                  ET_NUMBERS      = GT_NUMBERS.

** Gravar logs da operação de liberação da ordem
              call function 'Z_GRAVA_LOG_PM'
                tables
                  t_return = gt_return.

              if not line_exists( gt_return[ type = 'E' ] ).

                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait   = 'X'
                  importing
                    return = gt_return.

                "_____________________________________________"AOENNING 02/06/2020

                zcl_ordem_man=>m_check_orc_inic_ord(
                  exporting
                    i_aufnr  = lv_orderid    " Nº ordem
                  importing
                    i_retorn = data(w_return)  " Campo de texto do comprimento 1
                    i_belnr  = data(_belnr) " Valor total em moeda de transação
                ).

                id_orcamento = _belnr.

                "_____________________________________________
                try.
                    check w_return eq abap_true. "Retorno tem que ser abap_true. "CHECK _BELNR EQ ABAP_TRUE.
                  catch cx_sy_conversion_no_number into data(error_ref).
                    data(lva_msg) = error_ref->get_text( ). "handle exceptions here
                endtry.

                select count(*)
                  from zpmr0006
                 where aufnr eq gw_order_header-orderid
                   and status eq 'R'.

                if sy-subrc is initial.
                  delete from zpmr0006 where aufnr eq gw_order_header-orderid.
                endif.

                wait up to 3 seconds.

                select single phas1
                  from aufk
                  into @data(vl_phas1)
                 where aufnr eq @gw_order_header-orderid.

                if vl_phas1 is initial.
                  update aufk set phas0 = abap_false
                                  phas1 = abap_true
                  where aufnr eq gw_order_header-orderid.
                  commit work.
                endif.

                select single iphas
                  from afih
                  into @data(vl_iphas)
                 where aufnr eq @gw_order_header-orderid.

                if vl_iphas eq '0'.
                  update afih set iphas = '2'
                  where aufnr eq gw_order_header-orderid.
                  commit work.
                endif.

                return = value #( type = 'S' message = 'Ordem liberada.' ).
              else.
                return = value #( type = 'S' message = 'Permissão concedida.' ).

                t_zpmr0002[] = gt_zpmr0002.

              endif.
            endif.
          endif.
        else.
*          RETURN = VALUE #( TYPE = 'E' MESSAGE = 'Não possui permissão.' ).
        endif.
      endloop.

      if sy-subrc is not initial.
        return = value #( type = 'E' message = 'Não possui permissão.' ).
      endif.
    endif.
  else.
    return = value #( type = 'E' message = 'Ordem não encontrada.' ).
  endif.
endfunction.
