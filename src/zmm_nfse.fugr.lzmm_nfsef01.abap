*----------------------------------------------------------------------*
***INCLUDE LZMM_NFSEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALL
*&---------------------------------------------------------------------*
form f_refresh_all .

  clear: zsheader_data_nfse_inbound,
         zspayment_data_nfse_inbound,
         gt_scr_condition_list,
         gt_scr_popup_list,
         gt_scr_popup_search,
         gv_save_3010,
         gt_bapiret2,
         gt_service.

  free: go_znfse, go_text_2000.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SEARCH_PO
*&---------------------------------------------------------------------*
form f_search_po .

  clear: gt_scr_popup_search.

  perform f_get_po_doc
    using s_ebeln3[]
          zsheader_data_nfse_inbound-bukrs
          zsheader_data_nfse_inbound-lifnr
 changing gt_scr_popup_search.

  loop at gt_scr_popup_list assigning field-symbol(<fs_associados>).

    delete gt_scr_popup_search
      where ebeln = <fs_associados>-ebeln
        and ebelp = <fs_associados>-ebelp.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_ASSSOCIATION
*&---------------------------------------------------------------------*
form f_po_asssociation_commit.

  data: lt_nfse_002 type standard table of zibt_nfse_002.
  data: wa_nfse_003 type  zibt_nfse_003.
  data lv_dif type /tcsr/e_nfse_value.
  "
  select single * from zibt_nfse_001
       into  corresponding fields of  zibs_nfse_001
         where guid_header = zibs_nfse_001-guid_header.

  if sy-subrc ne 0.
    loop at gt_scr_popup_list into data(lw_desa).
      select single * from zibt_nfse_002 where guid_header = zibs_nfse_001-guid_header
                                          and ebeln = lw_desa-ebeln
                                          and ebelp = lw_desa-ebelp.
      if sy-subrc ne 0. "inseriu mas nao associou
        delete from zibt_nfse_003 where guid_header = zibs_nfse_001-guid_header
                              and ebeln = lw_desa-ebeln
                              and ebelp = lw_desa-ebelp.
        update zibt_nfse_001 set ebeln = ''
        where guid_header = zibs_nfse_001-guid_header.
        commit work.
      endif.
    endloop.
    message 'Gravar as informações da nota, tela anterior, para associar' type 'E'.
    exit.
  endif.
  check zibs_nfse_001-mblnr is initial.

  select single * from zibt_nfse_002
       into  @data(w_002)
         where guid_header = @zibs_nfse_001-guid_header
         and   mblnr <> ' '.

  check sy-subrc is not initial.

  lv_dif = abs( gv_popup_xml_value - gv_popup_sap_value ).
  if lv_dif < 0 or lv_dif > gv_total_tolerance.
    loop at gt_scr_popup_list into lw_desa.
      select single * from zibt_nfse_002 where guid_header = zibs_nfse_001-guid_header
                                          and ebeln = lw_desa-ebeln
                                          and ebelp = lw_desa-ebelp.
      if sy-subrc ne 0. "inseriu mas nao associou
        delete from zibt_nfse_003 where guid_header = zibs_nfse_001-guid_header
                              and ebeln = lw_desa-ebeln
                              and ebelp = lw_desa-ebelp.
        update zibt_nfse_001 set ebeln = ''
        where guid_header = zibs_nfse_001-guid_header.
        commit work.
      endif.
    endloop.
    message i000(z_mm) with 'O Valor do pedido' 'ultrapassa a tolerância'.
    exit.
  endif.
  "
  loop at gt_scr_popup_list assigning field-symbol(<fs_associ>)." WHERE checkbox EQ abap_true.

    read table gt_associated_po assigning field-symbol(<fs_associated_po>)
            with key ebeln = <fs_associ>-ebeln
                     ebelp = <fs_associ>-ebelp.

    " se já esta associado tira
    if sy-subrc ne 0.

      append initial line to lt_nfse_002 assigning field-symbol(<fs_nfse_002>).

      move-corresponding <fs_associ> to <fs_nfse_002>.

      <fs_nfse_002>-guid_header = zibs_nfse_001-guid_header.
      <fs_nfse_002>-ebeln       = <fs_associ>-ebeln.
      <fs_nfse_002>-ebelp       = <fs_associ>-ebelp.

      <fs_nfse_002>-dmbtr = <fs_associ>-netwr.
      "
      "Imposto
      clear w_wmwst.
      perform r_imposto_item using  <fs_associ>-ebelp
                                    <fs_associ>-ebeln
                                    <fs_associ>-menge
                        changing   w_valor
                                   w_wmwst.
      select single *
        from zibt_nfse_003
        into wa_nfse_003
            where ebeln = <fs_associ>-ebeln
            and   ebelp = <fs_associ>-ebelp.
      if sy-subrc = 0.
        w_wmwst = ( w_wmwst / wa_nfse_003-menge_ori ) * wa_nfse_003-menge.
      endif.

      <fs_nfse_002>-netwr_imp = w_wmwst.
      "
      <fs_nfse_002>-werks = <fs_associ>-werks.
      <fs_nfse_002>-waers = zibs_nfse_001-waers.
      <fs_nfse_002>-meins = <fs_associ>-meins_gr.

      update /tcsr/t_act
         set ebeln           = <fs_associ>-ebeln
             last_stepstatus = '01'
         where guid_header   = zibs_nfse_001-guid_header.

      if sy-subrc is initial.
        commit work.
      endif.

      " para atualizar a tela
      move-corresponding <fs_nfse_002> to zibs_nfse_001.

      update zibt_nfse_001 set ebeln = <fs_nfse_002>-ebeln
        where guid_header = zibs_nfse_001-guid_header.

    else.

      append initial line to lt_nfse_002 assigning <fs_nfse_002>.

      move-corresponding <fs_associated_po> to <fs_nfse_002>.
      move-corresponding <fs_associ> to <fs_nfse_002>.

      <fs_nfse_002>-dmbtr = <fs_associ>-netwr.

      <fs_nfse_002>-guid_header = zibs_nfse_001-guid_header.
      <fs_nfse_002>-waers = zibs_nfse_001-waers.
      <fs_nfse_002>-meins = <fs_associ>-meins_gr.
      "Imposto
      clear w_wmwst.
      perform r_imposto_item using  <fs_associ>-ebelp
                                    <fs_associ>-ebeln
                                    <fs_associ>-menge
                        changing   w_valor
                                   w_wmwst.
      select single *
        from zibt_nfse_003
        into wa_nfse_003
            where ebeln = <fs_associ>-ebeln
            and   ebelp = <fs_associ>-ebelp.
      if sy-subrc = 0.
        w_wmwst = ( w_wmwst / wa_nfse_003-menge_ori ) * wa_nfse_003-menge.
      endif.

      <fs_nfse_002>-netwr_imp = w_wmwst.
      <fs_nfse_002>-werks = <fs_associ>-werks.
      <fs_nfse_002>-waers = zibs_nfse_001-waers.
      <fs_nfse_002>-meins = <fs_associ>-meins_gr.

      update /tcsr/t_act
         set ebeln           = <fs_associ>-ebeln
             last_stepstatus = '01'
         where guid_header   = zibs_nfse_001-guid_header.

      if sy-subrc is initial.
        commit work.
      endif.

      " para atualizar a tela
      move-corresponding <fs_nfse_002> to zibs_nfse_001.

      update zibt_nfse_001 set ebeln = <fs_nfse_002>-ebeln
        where guid_header = zibs_nfse_001-guid_header.


    endif.

  endloop.

  if lt_nfse_002 is not initial.

    modify zibt_nfse_002 from table lt_nfse_002.

    if sy-subrc is initial.

      commit work and wait.

      if sy-subrc is initial.

        message 'Pedido associado com sucesso' type 'S'.

      endif.

    endif.

  endif.



endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_ASSOCIATE_PO
*&---------------------------------------------------------------------*
form f_po_associate_po .

  data lv_dif type /tcsr/e_nfse_value.

  sort: gt_po_header_data by ebeln,
        gt_po_item_data   by ebeln ebelp,
        gt_associated_po  by ebeln ebelp,.

  "APPEND LINES OF gt_scr_popup_search TO gt_searched_po.

  loop at gt_scr_popup_search assigning field-symbol(<fs_search>) where checkbox eq abap_true.

    read table gt_po_item_data assigning field-symbol(<fs_item>)
      with key ebeln = <fs_search>-ebeln
               ebelp = <fs_search>-ebelp.

    if sy-subrc is initial.

      if <fs_item>-mwskz ne 'S0' and <fs_item>-mwskz ne 'S1'.
        message i011(z_mm) with <fs_item>-ebeln.
        continue.
      endif.

      if <fs_item>-werks ne zibs_nfse_001-branch.
        message i008(z_mm) with <fs_item>-ebeln.
        continue.
      endif.

      if <fs_item>-j_1bnbm is initial.
        message i010(z_mm) with <fs_item>-ebeln <fs_item>-ebelp.
        continue.
      endif.

      if <fs_item>-loekz is not initial.
        message i009(z_mm) with <fs_item>-ebeln.
        continue.
      endif.

    endif.

    read table gt_scr_popup_list transporting no fields
      with key ebeln = <fs_item>-ebeln
               ebelp = <fs_item>-ebelp.

    if sy-subrc is not initial.

      <fs_search>-checkbox = space.

      if <fs_search>-pstyp = '9'.
        <fs_search>-icon_serv = '@AR@'.
      else.
        clear <fs_search>-icon_serv.
      endif.

*      " 15.12.2022 - RAMON limitar tolerancia -->
*      lv_dif = gv_popup_xml_value - <fs_search>-netwr.
*
*      IF lv_dif < 0 OR lv_dif > gv_total_tolerance.
*        MESSAGE i000(z_mm) WITH 'O Valor do pedido' 'ultrapassa a tolerância'.
*        EXIT.
*      ENDIF.
*      " 15.12.2022 - RAMON limitar tolerancia --><

      append <fs_search> to gt_scr_popup_list.

      delete gt_scr_popup_search
        where ebeln = <fs_item>-ebeln
          and ebelp = <fs_item>-ebelp.

    endif.

  endloop.

  sort gt_scr_popup_list by ebeln ebelp.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_UNASSIGN
*&---------------------------------------------------------------------*
form f_po_unassign.

  loop at gt_scr_popup_list into data(lw_desa) where checkbox = abap_true.

    data(lv_index) = sy-tabix.

    read table gt_associated_po assigning field-symbol(<fs_asso>)
            with key ebeln = lw_desa-ebeln
                     ebelp = lw_desa-ebelp.

*    READ TABLE gt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_asso>)
*            WITH KEY ebeln = lw_desa-ebeln
*                     ebelp = lw_desa-ebelp.

    if sy-subrc eq 0.
      select single * from zibt_nfse_001
        into  corresponding fields of  zibs_nfse_001
          where guid_header = zibs_nfse_001-guid_header.

      if zibs_nfse_001-belnr is not initial
        or zibs_nfse_001-gjahr is not initial
        or zibs_nfse_001-lblni is not initial
        or zibs_nfse_001-mblnr is not initial
        or zibs_nfse_001-mjahr is not initial.

        message 'Pedido não pode ser desassociado' type 'I'.

        exit.
      endif.

      update zibt_nfse_001 set ebeln = ''
        where guid_header = zibs_nfse_001-guid_header.

      " APAGA DO BD
      delete from zibt_nfse_002 where guid_header = zibs_nfse_001-guid_header
                             and ebeln = lw_desa-ebeln
                             and ebelp = lw_desa-ebelp.

      delete from zibt_nfse_003 where guid_header = zibs_nfse_001-guid_header.

      " APAGA DA TAB
      delete gt_nfse_002 where guid_header = zibs_nfse_001-guid_header
                             and ebeln = lw_desa-ebeln
                             and ebelp = lw_desa-ebelp.

      delete gt_associated_po where ebeln = lw_desa-ebeln
                               and ebelp = lw_desa-ebelp.


      zibs_nfse_001-ebeln = ''.
      zibt_nfse_001-ebeln = ''.

    endif.

    " apaga da tela
    delete gt_scr_popup_list index lv_index.

    commit work and wait.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_SEARCH_SAL
*&---------------------------------------------------------------------*
form f_po_search_sal .

  loop at gt_scr_popup_search assigning field-symbol(<popup_search>).
    <popup_search>-checkbox = abap_true.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_SEARCH_DAL
*&---------------------------------------------------------------------*
form f_po_search_dal .

  loop at gt_scr_popup_search assigning field-symbol(<popup_search>).
    clear <popup_search>-checkbox.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_ASSIGN_SAL
*&---------------------------------------------------------------------*
form f_po_assign_sal .

  loop at gt_scr_popup_list assigning field-symbol(<popup_list>).
    <popup_list>-checkbox = abap_true.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_ASSIGN_DAL
*&---------------------------------------------------------------------*
form f_po_assign_dal .

  loop at gt_scr_popup_list assigning field-symbol(<popup_list>).
    clear: <popup_list>-checkbox.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ASSOCIAR_PO
*&---------------------------------------------------------------------*
form f_associar_po .

  data lt_po_tab type zibc_nfse_pedidos.

  perform f_po_associados changing lt_po_tab.

  call function 'ZMM_NFSE_ASSOCIAR_PO'
    exporting
      i_header          = zsheader_data_nfse_inbound
      i_nfse            = zibs_nfse_001
    changing
      ct_nao_associados = gt_scr_popup_search
      ct_associados     = lt_po_tab.

  perform f_atualiza_campos
    using zibs_nfse_001
 changing zsheader_data_nfse_inbound
          zspayment_data_nfse_inbound.

  move-corresponding zibs_nfse_001 to zibt_nfse_001.

  if go_znfse is bound.
    go_znfse->force_update( ).
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PO_ASSOCIADOS
*&---------------------------------------------------------------------*
form f_po_associados changing p_po_tab type zibc_nfse_pedidos.

  check zibs_nfse_001-ebeln is not initial.

*  SELECT ebeln,bukrs,ekorg,ernam,aedat,waers FROM ekko
*    INTO TABLE @DATA(lt_po_header)
*    WHERE ebeln = @zibs_nfse_001-ebeln.
  select distinct ekko~ebeln,ekko~bukrs,ekko~ekorg,ekko~ernam,ekko~aedat,ekko~waers
    from zibt_nfse_002
    inner join ekko on  ekko~ebeln = zibt_nfse_002~ebeln
     into table @data(lt_po_header)
  where zibt_nfse_002~guid_header = @zibs_nfse_001-guid_header
  and   zibt_nfse_002~ebeln ne ''.

  if sy-subrc is initial.

    sort lt_po_header by ebeln.

    select s~ebeln,s~ebelp,s~matnr,txz01,
           s~werks,
           marc~steuc as j_1bnbm,
           menge,s~netwr,
           brtwr,mwskz,loekz,s~pstyp,
           s~knttp,menge_iv, s~meins as meins_gr,s~txjcd as taxjurcode,perc_iv, dmbtr_iv
      from ekpo as s
       inner join marc on s~matnr = marc~matnr
                     and  s~werks = marc~werks
      inner join zibt_nfse_002 as z on z~ebeln = s~ebeln
                                   and z~ebelp = s~ebelp
      into table @data(lt_po_item)
      for all entries in @lt_po_header
        where s~ebeln = @lt_po_header-ebeln.

    if sy-subrc is initial.

      sort lt_po_item by ebeln ebelp.

      select * from ekkn
        into table @data(lt_ekkn)
        for all entries in @lt_po_item
          where ebeln = @lt_po_item-ebeln
            and ebelp = @lt_po_item-ebelp.

    endif.

    loop at lt_po_header assigning field-symbol(<fs_header>).

      loop at lt_po_item assigning field-symbol(<fs_item>)
        where ebeln = <fs_header>-ebeln.

        read table lt_ekkn assigning field-symbol(<fs_ekkn>)
          with key ebeln = <fs_item>-ebeln
                   ebelp = <fs_item>-ebelp.

        check sy-subrc eq 0.

        append initial line to p_po_tab assigning field-symbol(<fs_po_list>).

        move-corresponding <fs_ekkn> to <fs_po_list>.
        move-corresponding <fs_header> to <fs_po_list>.
        move-corresponding <fs_item> to <fs_po_list>.

        if <fs_item>-pstyp = '9'.
          <fs_po_list>-icon_serv = '@AR@'.
        else.
          clear <fs_po_list>-icon_serv.
        endif.

      endloop.

    endloop.

  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DOC
*&---------------------------------------------------------------------*
form f_get_po_doc using p_ebeln_list type rpo_t_ebeln_range
                        p_bukrs type bukrs
                        p_lifnr type lifnr
               changing p_po_tab type zibc_nfse_pedidos.

  if p_ebeln_list is not initial.
    select single *
      from ekko
      into @data(w_ekko)
      where ebeln in @p_ebeln_list.

    if sy-subrc ne 0.
      message 'Pedido não existe!' type 'E'.
      exit.
    endif.

    if ( w_ekko-lifnr ne p_lifnr ) and  ( w_ekko-lifre ne p_lifnr ) and ( w_ekko-llief ne p_lifnr ). "IR182425
      message 'Fornecedor do pedido não é o da nota!' type 'E'.
      exit.
    endif.
    select single *
      from ekpo
      into @data(w_ekpo)
      where ebeln in @p_ebeln_list
       and exists ( select *
                     from mara
                     where matnr = ekpo~matnr
                     and   mtart = 'ZDIE' ).
    if sy-subrc ne 0.
      message 'Pedido não tem item de serviço!' type 'E'.
      exit.
    endif.
    "
  endif.

  select ebeln,bukrs,ekorg,ernam,aedat,lifnr from ekko
    into table @data(lt_po_header)
    where ebeln in @p_ebeln_list
      and bukrs eq @p_bukrs
      and ( lifnr eq @p_lifnr or lifre eq @p_lifnr )
      and frgke eq '2'. " 26.10.2022 - 93947

  select ebeln,bukrs,ekorg,ernam,aedat,lifnr from ekko
  appending table @lt_po_header
  where ebeln in @p_ebeln_list
    and bukrs eq @p_bukrs
    and ( lifnr eq @p_lifnr or lifre eq @p_lifnr )
    and exists (  select  *
                  from setleaf
                  where setname eq 'MAGGI_PEDIDOS_COUPA'
                  and valfrom   eq ekko~bsart ).


  if lt_po_header[] is not initial.

    sort lt_po_header by aedat descending.

    delete adjacent duplicates from lt_po_header comparing ebeln.

    select ebeln,ebelp,ekpo~matnr,txz01,
           ekpo~werks,
           marc~steuc as j_1bnbm,
           menge,netwr,
           brtwr,mwskz,loekz,pstyp,knttp, steuc,peinh,bprme from ekpo

      inner join marc on ekpo~matnr = marc~matnr
                     and ekpo~werks = marc~werks

      into table @data(lt_po_item)

      for all entries in @lt_po_header
      where ebeln = @lt_po_header-ebeln
      and   ( loekz ne 'L' and loekz ne 'S' )
      and   elikz = ' '
      and exists ( select *
                     from mara
                     where matnr = ekpo~matnr
                     and   mtart = 'ZDIE' ).

    if sy-subrc is initial.
      sort lt_po_item by ebeln ebelp.
    endif.

    select ebeln,ebelp from zibt_nfse_002
      into table @data(lt_exist)
        for all entries in @lt_po_header
          where ebeln = @lt_po_header-ebeln.

    loop at lt_po_header assigning field-symbol(<fs_header>).

      loop at lt_po_item assigning field-symbol(<fs_item>)
        where ebeln = <fs_header>-ebeln.
        "
        "comentado ALRS 18.12.2023
*        " se esse pedido ja esta associado, então não mostra
*        READ TABLE lt_exist TRANSPORTING NO FIELDS
*          WITH KEY ebeln = <fs_item>-ebeln
*                   ebelp = <fs_item>-ebelp.
*
*        CHECK sy-subrc NE 0.

        refresh gt_service.
        perform f_get_po_serv
              using zibs_nfse_001-guid_header
                 <fs_item>-ebeln
                 <fs_item>-ebelp
        changing gt_service.
        "
        if gt_service[] is not initial.
          <fs_item>-menge = 0.
          loop at gt_service into data(lt_serv).
            add lt_serv-menge_sal to <fs_item>-menge.
            if lt_serv-menge_sal le 0. "sem saldo
              continue.
            endif.
          endloop.
        endif.


        append initial line to p_po_tab assigning field-symbol(<fs_po_list>).

        move-corresponding <fs_header> to <fs_po_list>.
        move-corresponding <fs_item> to <fs_po_list>.

        read table gt_po_item_data transporting no fields
          with key ebeln = <fs_item>-ebeln
                   ebelp = <fs_item>-ebelp.

        if sy-subrc ne 0.

          append initial line to gt_po_item_data assigning field-symbol(<fs_item_data>).

          move-corresponding <fs_item> to <fs_item_data>.

          if <fs_item>-pstyp = '9'.
            <fs_po_list>-icon_serv = '@AR@'.
          else.
            clear <fs_po_list>-icon_serv.
          endif.

        endif.

      endloop.
    endloop.
  endif.



endform.

form r_imposto_item using     w_ebelp
                              w_ebeln
                              w_menge
                    changing  w_valor
                              w_wmwst.
  data i_taxcom like  taxcom.
  data: wa_ite  like mepoitem.
  clear wa_ite.
  call function 'MEPO_DOC_ITEM_GET'
    exporting
      im_ebelp = w_ebelp                                    "'00010'
    importing
      ex_item  = wa_ite
    exceptions
      failure  = 1
      others   = 2.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  data: begin of t_konv occurs 0.
          include structure konv.
  data: end of t_konv.

  types: ty_konv type table of komv.

  field-symbols: <wmwst> type any,
                 <lfa1>  type lfa1,
                 <ekpo>  type ekpo,
                 <ek2>   type ekpo,
                 <ekko>  type ekko,
                 <vorga> type any,
                 <konv>  type ty_konv,
                 <cva>   type any.

  assign ('(SAPLMEPO)ekpo') to <ekpo>.
  assign ('(SAPLMEPO)ekko') to <ekko>.
  assign ('(SAPLMEPO)lfa1') to <lfa1>.

  clear <ekpo>.
  select single * from ekpo into <ekpo>
    where ebeln = w_ebeln and
          ebelp = w_ebelp.


  clear <ekko>.
  select single * from ekko into <ekko>
     where ebeln = w_ebeln.

  <ekpo>-menge = w_menge.

  select single * from lfa1 into <lfa1>
     where lifnr = <ekko>-lifnr.


  if <ekko>-knumv is not initial.
    select * from konv into table t_konv
     where knumv = <ekko>-knumv.

    assign ('(SAPLMEPO)tkomv[]') to <konv>.
    <konv>[] = t_konv[].
  endif.

  if <ekpo>-ebelp is initial and  w_ebelp is not initial.
    <ekpo>-ebelp = w_ebelp.
  endif.

  assign ('(SAPLMEPO)fc_vorga') to <vorga>.
  assign ('(SAPLMEPO)cva_en') to <cva>.

  <vorga> = <cva>.

  perform kond_taxes(saplmepo) using 'D' 'X'.

  check <ekpo>-loekz = space.
  assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.


  data: w_netwr  type komp-netwr.

  w_netwr = <ekpo>-netwr.
  w_wmwst  =  <wmwst>.
  w_valor  = ( w_netwr + <wmwst> ).


endform.                    " R_IMPOSTO_ITEM

*&---------------------------------------------------------------------*
*&      Form  F_EXECUTE_ML81N
*&---------------------------------------------------------------------*
form f_execute_ml81n .

  data lv_ret type c.
  data lv_erro type c.

  data lr_ebeln type range of ebeln.
  data lt_po_tab type zibc_nfse_pedidos.

  perform f_data_vencimento changing zspayment_data_nfse_inbound lv_erro.

  check lv_erro is initial.

  zibt_nfse_001 = go_znfse->get_nfse_001( ).

  if zibt_nfse_001-dt_vencimento is initial.
    message e000(z_mm) with 'Salvar Informações, antes de gerar!'.
  endif.

  if zibt_nfse_001-forne_razao is initial.
    message e000(z_mm) with 'Salvar Informações, antes de gerar!'.
  endif.

  if zibt_nfse_001-lblni is not initial.
    message e000(z_mm) with 'Folha já gerada'.
  endif.


  perform f_popup_to_confirm
    using 'Confirmar criação da folha de serviço?'
 changing lv_ret.

  check lv_ret = '1'.

  check go_znfse is bound.

  call method go_znfse->nfse_inbound_folha
    importing
      ev_entrysheet = zibt_nfse_001-lblni
      ev_acceptance = data(lv_accept)
      ev_packno     = zibt_nfse_001-packno.

  go_znfse->show_message( ).

  if zibt_nfse_001-lblni is not initial.

    zibt_nfse_001 = go_znfse->get_nfse_001( ).

    clear gv_edit_2000.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
form f_popup_to_confirm using p_question type c
                     changing p_answer type c.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar       = sy-title
      text_question  = p_question
    importing
      answer         = p_answer
    exceptions
      text_not_found = 1
      others         = 2.

  if sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  endif.

endform.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTE_MIGO
*&---------------------------------------------------------------------*
form f_execute_migo.

  data lv_ret type c.
  data lv_erro type c.

  check go_znfse is bound.

  perform f_data_vencimento changing zspayment_data_nfse_inbound lv_erro.

  check lv_erro is initial.

  zibt_nfse_001 = go_znfse->get_nfse_001( ).

  if zibt_nfse_001-dt_vencimento is initial.
    message e000(z_mm) with 'Salvar Informações, antes de gerar!'.
  endif.

  if zibt_nfse_001-forne_razao is initial.
    message e000(z_mm) with 'Salvar Informações, antes de gerar!'.
  endif.

  if zibt_nfse_001-mblnr is not initial.
    message e000(z_mm) with 'MIGO já gerada!'.
  endif.

  if zibt_nfse_001-forne_razao is initial.
    message e000(z_mm) with 'Salvar Informações, antes de gerar!'.
  endif.
  "
  perform f_popup_to_confirm
  using 'Confirmar criação do doc. material?'
      changing lv_ret.

  check lv_ret = '1'.

  call method go_znfse->nfse_inbound_doc_material
    importing
      ev_mblnr = zibt_nfse_001-mblnr
      ev_mjahr = zibt_nfse_001-mjahr.

  go_znfse->show_message( ).

  zibt_nfse_001 = go_znfse->get_nfse_001( ).

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_MENSAGENS
*&---------------------------------------------------------------------*
form f_exibe_mensagens using p_mess_tab type bapiret2_t.

  data: l_lines type i.

  describe table p_mess_tab lines l_lines.

  if l_lines <= 1 or sy-batch = 'X'.

    loop at p_mess_tab assigning field-symbol(<fs_ret2>).

      message id <fs_ret2>-id
            type 'S'
          number <fs_ret2>-number
            with <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 display like <fs_ret2>-type.

    endloop.

  else.

    call function 'MESSAGES_INITIALIZE'.

    loop at p_mess_tab assigning <fs_ret2>.

      if <fs_ret2>-id is initial.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      endif.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.     "#EC CI_SUBRC

    endloop.

    call function 'MESSAGES_STOP'
      exceptions
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        others    = 5.     "#EC CI_SUBRC

    call function 'MESSAGES_SHOW'
      exporting
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      exceptions
        inconsistent_range  = 1
        no_messages         = 2
        others              = 3.     "#EC CI_SUBRC

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTE_MIRO
*&---------------------------------------------------------------------*
form f_execute_miro .

  data lv_ret type c.

  perform f_popup_to_confirm
    using 'Confirmar envio SE?'
 changing lv_ret.

  check lv_ret = '1'.

  check go_znfse is bound.

  try.
      call method go_znfse->nfse_sol_miro_softexpert.
    catch zcx_soft_expert_workflow into data(lo_workflow).
      call method go_znfse->nfse_sol_miro_softexpert( i_estornar = 'E' ).
  endtry.

  perform f_atualiza_tela_2000.

  go_znfse->show_message( ).



endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_NFSE_001
*&---------------------------------------------------------------------*
form f_preenche_nfse_002 using p_nfse type zibs_nfse_001.

  select * from zibt_nfse_002
    into table gt_nfse_002
      where guid_header = p_nfse-guid_header.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_PEDIDOS
*&---------------------------------------------------------------------*
form f_create_alv_pedidos .

  perform f_create_cc using 'CC_PEDIDOS' changing go_cc_pedidos.

  check go_alv_pedidos is initial.

  create object go_alv_pedidos
    exporting
      i_parent = go_cc_pedidos.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_ASSOCIADOS
*&---------------------------------------------------------------------*
form f_create_alv_associados .

  perform f_create_cc using 'CC_PASSOCIADOS' changing go_cc_associados.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_CC
*&---------------------------------------------------------------------*
form f_create_cc using p_cc_name type c
              changing po_custom type ref to cl_gui_custom_container.

  if po_custom is initial.

    " Create a custom container control for our ALV Control
    create object po_custom
      exporting
        container_name              = p_cc_name
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    if sy-subrc ne 0.
      message i000(zpp) with 'The custom control could not be created'.
      return.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_LINK
*&---------------------------------------------------------------------*
form f_call_link .


  get cursor field cursor_field
             line  cursor_line
             value cursor_value.

  if cursor_field	= 'GW_SCR_POPUP_LIST-EBELN' and cursor_value is not initial.

    data lv_ebeln type ebeln.

    lv_ebeln = cursor_value.

    call function 'ME_DISPLAY_PURCHASE_DOCUMENT'
      exporting
        i_ebeln = lv_ebeln.

  endif.

  if cursor_field	= 'GW_SCR_POPUP_LIST-ICON_SERV' and cursor_value is not initial.

    read table gt_scr_popup_list assigning field-symbol(<fs_po_list>)
      index cursor_line.

    if sy-subrc eq 0.

      call function 'ZMM_NFSE_PO_SERV'
        exporting
          i_guid  = zibs_nfse_001-guid_header
          i_ebeln = <fs_po_list>-ebeln
          i_ebelp = <fs_po_list>-ebelp
          i_save  = 'X'.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_SERV
*&---------------------------------------------------------------------*
form f_get_po_serv using p_guid type  /tcsr/e_guid_header
                         p_ebeln type  ebeln
                         p_ebelp type ebelp
                changing p_serv_tab type zibc_nfse_po_serv.

  data lv_zekkn type dzekkn value 1.
  data lv_rateio(1).
  data lv_extrow type esll-extrow.

  clear p_serv_tab.

  select * from zibt_nfse_003
    into table @data(lt_serv)
       where guid_header = @p_guid
         and ebeln = @p_ebeln
         and ebelp = @p_ebelp.

  if sy-subrc ne 0.

    select ebeln,ebelp,esll~extrow,esll~srvpos,
           esll~ktext1,esll~menge, esll~meins,esll~tbtwr, esll~netwr from eslh
      inner join esll on eslh~packno = esll~packno
      into table @data(lt_esll)
        where ebeln = @p_ebeln
          and ebelp = @p_ebelp.

    select * from ekkn
      into table @data(lt_ekkn)
        where ebeln = @p_ebeln
          and ebelp = @p_ebelp.

    select ekbe~ebeln,ekbe~ebelp, sum( s2~menge ) as menge
          into table @data(lt_ekbe)
          from ekbe
          inner join essr on essr~lblni = ekbe~belnr
          inner join esll as s1 on s1~packno = essr~packno
          inner join esll as s2 on s2~packno = s1~sub_packno
          inner join ekbe as migo
           on  migo~lfbnr = ekbe~belnr "tem Aceite
           and migo~bwart = '101'
           where ekbe~ebeln = @p_ebeln
           and ekbe~ebelp   = @p_ebelp
           and ekbe~vgabe   = '9'
           and ekbe~dmbtr   > 0
           and  not exists ( select * from ekbe as estorno where estorno~lfbnr = migo~lfbnr and estorno~bwart = '102' ) "aceite estornado
          group by ekbe~ebeln,ekbe~ebelp.

    "checar folha de aceite
    select s2~*
          into table @data(lt_esll_101)
          from ekbe
          inner join essr on essr~lblni = ekbe~belnr
          inner join esll as s1 on s1~packno = essr~packno
          inner join esll as s2 on s2~packno = s1~sub_packno
          inner join ekbe as migo
           on  migo~lfbnr = ekbe~belnr "tem Aceite
           and migo~bwart = '101'
           where ekbe~ebeln = @p_ebeln
           and ekbe~ebelp   = @p_ebelp
           and ekbe~vgabe   = '9'
           and ekbe~dmbtr   > 0
           and  not exists ( select * from ekbe as estorno where estorno~lfbnr = migo~lfbnr and estorno~bwart = '102' ). "aceite estornado

    sort lt_esll_101 by extrow.


    loop at lt_esll assigning field-symbol(<fs_esll>).

      check <fs_esll>-srvpos is not initial.

      .
      append initial line to p_serv_tab assigning field-symbol(<fs_serv>).

      read table lt_ekkn assigning field-symbol(<fs_ekkn>)
        with key ebeln = <fs_esll>-ebeln
                 ebelp = <fs_esll>-ebelp
                 zekkn = lv_zekkn.

      if sy-subrc eq 0.
        move-corresponding <fs_ekkn> to <fs_serv>.
      endif.
      <fs_serv>-guid_header = p_guid.
      <fs_serv>-ebeln = <fs_esll>-ebeln.
      <fs_serv>-ebelp = <fs_esll>-ebelp .
      <fs_serv>-extrow = <fs_esll>-extrow.
      <fs_serv>-srvpos = <fs_esll>-srvpos.
      <fs_serv>-ktext1 = <fs_esll>-ktext1.
      <fs_serv>-menge_ori = <fs_esll>-netwr. "  <fs_esll>-menge * <fs_esll>-tbtwr.
      <fs_serv>-menge = <fs_esll>-netwr. "  <fs_esll>-menge * <fs_esll>-tbtwr.
      <fs_serv>-meins = <fs_esll>-meins .
      <fs_serv>-tbtwr = <fs_esll>-tbtwr .
      <fs_serv>-waers = 'BRL' .

*      READ TABLE lt_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>)
*        WITH KEY ebeln = <fs_esll>-ebeln
*                 ebelp = <fs_esll>-ebelp.
*      IF sy-subrc = 0.
*        <fs_serv>-menge_sal = <fs_serv>-menge_ori - <fs_ekbe>-menge.
*        <fs_serv>-menge     = <fs_serv>-menge_ori - <fs_ekbe>-menge. "ALRS 18.12.2023
*      ELSE.
*        <fs_serv>-menge_sal = <fs_serv>-menge_ori.
*      ENDIF.

      read table lt_esll_101 into data(wa_esll_101) with key extrow = <fs_esll>-extrow binary search.
      if sy-subrc = 0.
        <fs_serv>-menge_sal = <fs_serv>-menge_ori - wa_esll_101-menge.
        <fs_serv>-menge     = <fs_serv>-menge_ori - wa_esll_101-menge. "ALRS 18.12.2023
      else.
        <fs_serv>-menge_sal = <fs_serv>-menge_ori.
      endif.

      add 1 to lv_zekkn.

    endloop.
    "


  else.

    loop at lt_serv assigning field-symbol(<fs_nfse2>).

      append initial line to p_serv_tab assigning <fs_serv>.

      move-corresponding <fs_nfse2> to <fs_serv>.

      if <fs_serv>-menge_ori is initial.
        <fs_serv>-menge_ori = <fs_serv>-menge.
      endif.

    endloop.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_SERV
*&---------------------------------------------------------------------*
form f_save_serv .

  data lt_nfse_003 type table of zibt_nfse_003.

  check gv_save_3010 = 'X'.

  loop at gt_service assigning field-symbol(<fs_serv>).

    append initial line to lt_nfse_003 assigning field-symbol(<fs_003>).

    move-corresponding <fs_serv> to <fs_003>.

  endloop.

  check lt_nfse_003 is not initial.

  modify zibt_nfse_003 from table lt_nfse_003.

  commit work and wait.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_CAMPOS_PO
*&---------------------------------------------------------------------*
form f_atualiza_campos using p_nfes type zibs_nfse_001
                       changing p_header type zsheader_data_nfse_inbound
                                p_payment type zspayment_data_nfse_inbound.

  check p_nfes-ebeln is not initial.

  select single ekko~waers,ekpo~mwskz
        into @data(lw_data)
        from zibt_nfse_002
        inner join ekpo
        on  ekpo~ebeln = zibt_nfse_002~ebeln
        and ekpo~ebelp = zibt_nfse_002~ebelp
        inner join ekko
        on  ekko~ebeln = ekpo~ebeln
        where zibt_nfse_002~ebeln = @p_nfes-ebeln.

  check sy-subrc eq 0.

  p_payment-waers = lw_data-waers.

  p_header-mwskz = lw_data-mwskz.

  if p_header-nfse_serie is initial.
    p_header-nfse_serie = '1'.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_2000
*&---------------------------------------------------------------------*
form f_save_2000 changing cv_erro type flag.

  data: lw_nfse_001_change   type zibt_nfse_001,
        zvlr_liq_vt          type zvlr_liq_pagar,
        zvlr_total_vt        type zvlr_liq_pagar,
        zvlr_liq_total       type zvlr_liq_pagar,
        zvlr_dif             type zvlr_liq_pagar,
        zvlr_peso_total_vt   type zvlr_liq_pagar,
        zvlr_perda_total_vt  type zvlr_liq_pagar,
        zvlr_quebra_total_vt type zvlr_liq_pagar,
        ztotal_lines         type p.


  clear: zvlr_dif, cv_erro, zvlr_liq_total, zvlr_peso_total_vt, zvlr_total_vt,zvlr_liq_vt.

  if ( zspayment_data_nfse_inbound-boleto is initial and zspayment_data_nfse_inbound-pymt_meth = 'E' ).
    message e000(z_mm) with 'Informe o boleto'.
  endif.

  perform f_data_vencimento changing zspayment_data_nfse_inbound cv_erro.

  check cv_erro is initial.

  " busca dados ja gravados
  move-corresponding zibs_nfse_001 to zibt_nfse_001.
  select single * from zibt_nfse_001
       into  corresponding fields of  zibt_nfse_001
         where guid_header = zibs_nfse_001-guid_header.

  zibt_nfse_001-forne_razao = zibs_nfse_001-name1.
  move-corresponding zspayment_data_nfse_inbound to zibs_nfse_001.
  move-corresponding zsheader_data_nfse_inbound to zibs_nfse_001.

  lw_nfse_001_change-guid_header = zibt_nfse_001-guid_header.

  zibs_nfse_001-zbvtyp = zspayment_data_nfse_inbound-bvtyp.
  zibt_nfse_001-zbvtyp = zspayment_data_nfse_inbound-bvtyp.
  zibt_nfse_001-p_cpf = zsheader_data_nfse_inbound-stcd2.
  zibt_nfse_001-p_cnpj = zsheader_data_nfse_inbound-stcd1.

  " 31.03.2023 - 107971 - RBL -->
  zibt_nfse_001-nao_retem_iss = zsheader_data_nfse_inbound-nao_reter_iss.
****     Inicio - ALX
*  IF gv_iss_2000 = abap_true.
*    zibt_nfse_001-retem_iss = abap_false.
*  ELSE.
*    zibt_nfse_001-retem_iss = abap_true.
*  ENDIF.
****     Fim - ALX
  " 31.03.2023 - 107971 - RBL --<


  check zibs_nfse_001 is not initial.

  perform f_dados_banco
    using zibs_nfse_001-bukrs
          zibs_nfse_001-lifnr
          zibs_nfse_001-zbvtyp
 changing zibt_nfse_001-housebankid.

  clear gt_bapiret2.


  perform f_valida_boleto
  changing zspayment_data_nfse_inbound-boleto
           zsheader_data_nfse_inbound-nfse_value
           zspayment_data_nfse_inbound-dt_vencimento
           cv_erro.

  perform f_save_log.

  move-corresponding zspayment_data_nfse_inbound to zibt_nfse_001.
  move-corresponding zsheader_data_nfse_inbound to zibt_nfse_001.

  "zibt_nfse_001-mwskz = zsheader_data_nfse_inbound-mwskz.

  check cv_erro is initial.

  lw_nfse_001_change = zibt_nfse_001.

  move-corresponding gv_paymen_old to lw_nfse_001_change.
  move-corresponding gv_header_old to lw_nfse_001_change.

  perform f_save_change
    using lw_nfse_001_change
          zibt_nfse_001.

  modify zibt_nfse_001 from zibt_nfse_001.

  commit work and wait.

  if go_znfse is bound.
    go_znfse->force_update( ).
  endif.

  "Salvar dados na tabela ZLEST0034.
  if zibt_nfse_001-guid_header is not initial and zibt_nfse_001-bukrs is not initial.
    select * from zlest0034
      into table @data(it_zlest0034)
      where guid_header eq @zibt_nfse_001-guid_header.
    if sy-subrc eq 0.
      if zsheader_data_nfse_inbound-reter_iss eq abap_false.
        clear zibs_nfps_001-iss_total.
      else.
        zibs_nfps_001-iss_total = value #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value default '0.00' ).
      endif.

      loop at it_zlest0034 assigning field-symbol(<ws_zlest0034>) where guid_header eq zibt_nfse_001-guid_header.
        add <ws_zlest0034>-zpeso_origem to zvlr_peso_total_vt.
        add <ws_zlest0034>-zvlr_perda to zvlr_perda_total_vt.
        add <ws_zlest0034>-zvlr_quebra to zvlr_quebra_total_vt.
      endloop.

      zvlr_liq_total = zibt_nfse_001-nfse_value - ( zvlr_perda_total_vt + zvlr_quebra_total_vt + zibs_nfps_001-iss_total ).

      "Pegar o valor liquido da nota.
      describe table it_zlest0034 lines ztotal_lines.
      loop at it_zlest0034 assigning <ws_zlest0034> where guid_header eq zibt_nfse_001-guid_header.

        if zvlr_liq_total is not initial.
          zvlr_liq_vt = ( zvlr_liq_total / zvlr_peso_total_vt ) * <ws_zlest0034>-zpeso_origem.
        endif.

        if zvlr_liq_vt is not initial.
*        <ws_zlest0034>-zvlr_liq_pagar = zibt_nfse_001-nfse_value - ( <ws_zlest0034>-zvlr_perda + <ws_zlest0034>-zvlr_quebra + zibs_nfps_001-iss_total ).
          <ws_zlest0034>-zvlr_liq_pagar = zvlr_liq_vt.
          add zvlr_liq_vt to zvlr_total_vt.

          if sy-tabix eq ztotal_lines.
            if  zvlr_total_vt ne zvlr_liq_total.
              zvlr_dif =  zvlr_total_vt - zvlr_liq_total.
              if  zvlr_dif > 0.
                <ws_zlest0034>-zvlr_liq_pagar = <ws_zlest0034>-zvlr_liq_pagar - zvlr_dif.
              else.
                <ws_zlest0034>-zvlr_liq_pagar = <ws_zlest0034>-zvlr_liq_pagar - zvlr_dif.
              endif.
            endif.
          endif.
        endif.
      endloop.

      if it_zlest0034 is not initial.
        modify zlest0034 from table it_zlest0034.
        commit work.
      endif.
    endif.
  endif.

  perform f_put_mensagem using 'S' 'Dados gravados'.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_TOTAIS_3000
*&---------------------------------------------------------------------*
form f_atualiza_totais_3000 .

  gv_popup_xml_value = zsheader_data_nfse_inbound-nfse_value.
  "gv_total_tolerance = 0.

  clear gv_popup_sap_value.

  loop at gt_scr_popup_list assigning field-symbol(<fs_associ>).
    perform f_get_po_serv
     using zibs_nfse_001-guid_header
           <fs_associ>-ebeln
           <fs_associ>-ebelp
  changing gt_service.

    if gt_service[] is not initial.
      <fs_associ>-menge_iv = 0.
      <fs_associ>-dmbtr_iv = 0.
    endif.

    loop at gt_service assigning field-symbol(<fs_service>)
          where ebeln = <fs_associ>-ebeln
            and ebelp = <fs_associ>-ebelp.
      "Imposto
      perform r_imposto_item using  <fs_associ>-ebelp
                                    <fs_associ>-ebeln
                                    <fs_service>-menge
                        changing   w_valor
                                   w_wmwst.
      "
      w_wmwst = ( w_wmwst / <fs_service>-menge_ori ) *  ( <fs_service>-menge * <fs_service>-tbtwr ).

      "
      select single bsart
        from ekko
        into @data(_bsart)
        where ebeln = @<fs_associ>-ebeln.
      if _bsart+0(1) = 'Y'.
        w_wmwst = 0.
      endif.

      add <fs_service>-menge to <fs_associ>-menge_iv.
      <fs_associ>-dmbtr_iv = <fs_associ>-dmbtr_iv + w_wmwst + ( <fs_service>-menge * <fs_service>-tbtwr ).
*      gv_popup_sap_value = gv_popup_sap_value + w_wmwst + ( <fs_service>-menge * <fs_service>-tbtwr ).
      gv_popup_sap_value = gv_popup_sap_value + w_wmwst +  <fs_service>-menge  .

    endloop.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_TOTAL_ZNFSE
*&---------------------------------------------------------------------*
form f_total_znfse  changing p_valor type ktwrt.

  clear p_valor.

  loop at gt_nfse_002 assigning field-symbol(<fs_nfse>).

    p_valor = p_valor + ( <fs_nfse>-netwr * <fs_nfse>-menge_iv ).

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
form f_put_mensagem using p_type type c
                          p_string type string.

  data lv_msgty type sy-msgty.
  data lw_bapiret type bapiret2.

  check p_string is not initial.

  if p_type is initial.
    lv_msgty = 'E'.
  else.
    lv_msgty = p_type.
  endif.

  data: lt_trtexts     type trtexts,
        lw_trtexts     type trtext,
        lv_texto(4000).

  data lv_msg1 type sy-msgv1.
  data lv_msg2 type sy-msgv1.
  data lv_msg3 type sy-msgv1.
  data lv_msg4 type sy-msgv1.

  lv_texto = p_string.

  clear lw_bapiret.

  call function 'TR_SPLIT_TEXT'
    exporting
      iv_text  = lv_texto
      iv_len   = 30
    importing
      et_lines = lt_trtexts.

  loop at lt_trtexts assigning field-symbol(<fs_line>).

    case sy-tabix.
      when 1.
        lw_bapiret-message_v1 = <fs_line>.
      when 2.
        lw_bapiret-message_v2 = <fs_line>.
      when 3.
        lw_bapiret-message_v3 = <fs_line>.
      when 4.
        lw_bapiret-message_v4 = <fs_line>.
    endcase.

  endloop.

  lw_bapiret-id = 'DS'.
  lw_bapiret-type = lv_msgty.
  lw_bapiret-number = '016'.

  message id lw_bapiret-id type 'S' number lw_bapiret-number
    with lw_bapiret-message_v1 lw_bapiret-message_v2
         lw_bapiret-message_v3 lw_bapiret-message_v4 display like lw_bapiret-type.

  append lw_bapiret to gt_bapiret2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
form f_put_mensagem2 using i_type type bapi_mtype
                           i_id  type  symsgid
                           i_number  type  symsgno
                           i_mess_v1 type any
                           i_mess_v2 type any
                           i_mess_v3 type any
                           i_mess_v4 type any.

  data lw_bapiret type bapiret2.

  data: lt_trtexts     type trtexts,
        lw_trtexts     type trtext,
        lv_texto(4000).

  clear lw_bapiret.

  call function 'TR_SPLIT_TEXT'
    exporting
      iv_text  = lv_texto
      iv_len   = 30
    importing
      et_lines = lt_trtexts.

  lw_bapiret-message_v1 = i_mess_v1.
  lw_bapiret-message_v2 = i_mess_v2.
  lw_bapiret-message_v3 = i_mess_v3.
  lw_bapiret-message_v4 = i_mess_v4.

  lw_bapiret-id = i_id.
  lw_bapiret-type = i_type.
  lw_bapiret-number = i_number.

  message id lw_bapiret-id type 'S' number lw_bapiret-number
    with lw_bapiret-message_v1 lw_bapiret-message_v2
         lw_bapiret-message_v3 lw_bapiret-message_v4 display like lw_bapiret-type.

  append lw_bapiret to gt_bapiret2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
form f_put_mensagem3 using p_type type c
                          p_string type string
                 changing p_ret2 type bapiret2_tt.

  data lv_msgty type sy-msgty.
  data lw_bapiret type bapiret2.

  check p_string is not initial.

  if p_type is initial.
    lv_msgty = 'E'.
  else.
    lv_msgty = p_type.
  endif.

  data: lt_trtexts     type trtexts,
        lw_trtexts     type trtext,
        lv_texto(4000).

  data lv_msg1 type sy-msgv1.
  data lv_msg2 type sy-msgv1.
  data lv_msg3 type sy-msgv1.
  data lv_msg4 type sy-msgv1.

  lv_texto = p_string.

  clear lw_bapiret.

  call function 'TR_SPLIT_TEXT'
    exporting
      iv_text  = lv_texto
      iv_len   = 30
    importing
      et_lines = lt_trtexts.

  loop at lt_trtexts assigning field-symbol(<fs_line>).

    case sy-tabix.
      when 1.
        lw_bapiret-message_v1 = <fs_line>.
      when 2.
        lw_bapiret-message_v2 = <fs_line>.
      when 3.
        lw_bapiret-message_v3 = <fs_line>.
      when 4.
        lw_bapiret-message_v4 = <fs_line>.
    endcase.

  endloop.

  lw_bapiret-id = 'DS'.
  lw_bapiret-type = lv_msgty.
  lw_bapiret-number = '016'.

  message id lw_bapiret-id type 'S' number lw_bapiret-number
    with lw_bapiret-message_v1 lw_bapiret-message_v2
         lw_bapiret-message_v3 lw_bapiret-message_v4 display like lw_bapiret-type.

  append lw_bapiret to p_ret2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PUT_MENSAGEM
*&---------------------------------------------------------------------*
form f_put_mensagem4 using i_type type bapi_mtype
                           i_id  type  symsgid
                           i_number  type  symsgno
                           i_mess_v1 type any
                           i_mess_v2 type any
                           i_mess_v3 type any
                           i_mess_v4 type any
                  changing p_ret2 type bapiret2_tt.

  data lw_bapiret type bapiret2.

  data: lt_trtexts     type trtexts,
        lw_trtexts     type trtext,
        lv_texto(4000).

  clear lw_bapiret.

  call function 'TR_SPLIT_TEXT'
    exporting
      iv_text  = lv_texto
      iv_len   = 30
    importing
      et_lines = lt_trtexts.

  lw_bapiret-message_v1 = i_mess_v1.
  lw_bapiret-message_v2 = i_mess_v2.
  lw_bapiret-message_v3 = i_mess_v3.
  lw_bapiret-message_v4 = i_mess_v4.

  lw_bapiret-id = i_id.
  lw_bapiret-type = i_type.
  lw_bapiret-number = i_number.

  message id lw_bapiret-id type 'S' number lw_bapiret-number
    with lw_bapiret-message_v1 lw_bapiret-message_v2
         lw_bapiret-message_v3 lw_bapiret-message_v4 into lw_bapiret-message.

  append lw_bapiret to p_ret2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_SERV
*&---------------------------------------------------------------------*
form f_call_serv .

  get cursor field cursor_field
           line  cursor_line
           value cursor_value.

  check cursor_line is not initial.

  read table gt_scr_popup_list assigning field-symbol(<fs_po_list>)
    index cursor_line.

  if sy-subrc eq 0.

    call function 'ZMM_NFSE_PO_SERV'
      exporting
        i_guid  = zibs_nfse_001-guid_header
        i_ebeln = <fs_po_list>-ebeln
        i_ebelp = <fs_po_list>-ebelp
        i_save  = 'X'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_BOLETO
*&---------------------------------------------------------------------*
form f_valida_boleto changing p_boleto type zde_nr_boleto
                              p_valor type /tcsr/e_nfse_value
                              p_vencimento type sy-datum
                              p_erro type c.

  data: lc_esrre           type bseg-esrre,
        lc_esrnr           type bseg-esrnr,
        lc_data_vencimento type zde_dt_vencimento,
        lc_qtd             type i,
        i_boleto_out       type c length 55,
        l_vlrliq           type bseg-dmbtr,
        l_vlrliq2          type  bseg-dmbtr.

  try .

      check p_boleto is not initial.

      check p_vencimento is not initial.

      data(qtd_length) = strlen( p_boleto ).

      if qtd_length lt 47.
*        RAISE EXCEPTION TYPE zcx_miro_exception
*          EXPORTING
*            textid = VALUE #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
*                              msgno = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno )
*            msgid  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
*            msgno  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno
*            msgty  = 'E'.
      endif.

      gs_nfse_4000-guid_header = zibs_nfse_001-guid_header.
      perform f_get_impostos.

      l_vlrliq = p_valor.

      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'INSS' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'PISCOFCSLL' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'PIS' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'COFI' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'CSLL' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'IRRW' ]-xml_value default '0.00' ).
      l_vlrliq = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'IGEW' ]-xml_value default '0.00' ).

      l_vlrliq2 = l_vlrliq - value #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value default '0.00' ).

      lc_data_vencimento = '19971007'.

      call function 'CONVERSION_EXIT_ZBOLE_OUTPUT'
        exporting
          input  = p_boleto
        importing
          output = i_boleto_out.

      lc_qtd = i_boleto_out+40(4).
                                                            "US163807
      if p_vencimento <= '20250221'.
        lc_data_vencimento = lc_data_vencimento + lc_qtd.
      else.
        lc_data_vencimento = '20250222'.
        lc_qtd = lc_qtd - 1000.
        lc_data_vencimento = lc_data_vencimento + lc_qtd.
      endif.
                                                            "US163807

      "lc_data_vencimento = lc_data_vencimento + lc_qtd.

      "Jogar para o próximo dia útil
      zcl_miro=>get_proximo_dia_util(
        exporting
          i_data_base = lc_data_vencimento
        receiving
          r_data      = lc_data_vencimento
        exceptions
          erro        = 1
          others      = 2 ).

      if sy-subrc is not initial.

        perform f_put_mensagem2
          using sy-msgty
                sy-msgid
                sy-msgno
                sy-msgv1
                sy-msgv2
                sy-msgv3
                sy-msgv4.

        p_erro = 'X'.

      endif.

      data: lc_barcode type c length 47.
      call function 'CONVERSION_EXIT_ZBOLE_INPUT'
        exporting
          input  = p_boleto
        importing
          output = lc_barcode.

* ---> S4 Migration - 21/06/2023 - MA
      data: e_dmbtr like bseg-dmbtr.
      e_dmbtr = conv #( p_valor ).
*      CALL FUNCTION 'CONVERT_BARCODE'
*        EXPORTING
*          barcode   = lc_barcode
*          dmbtr     = p_valor
*        IMPORTING
*          esrre     = lc_esrre
*          esrnr     = lc_esrnr
*        EXCEPTIONS
*          not_valid = 1
*          OTHERS    = 2.

      call function 'CONVERT_BARCODE'
        exporting
          barcode   = lc_barcode
          dmbtr     = e_dmbtr
        importing
          esrre     = lc_esrre
          esrnr     = lc_esrnr
        exceptions
          not_valid = 1
          others    = 2.
* <--- S4 Migration - 21/06/2023 - MA

      if sy-subrc is not initial  or sy-msgid = '8B'.
        clear sy-msgid.
        call function 'CONVERT_BARCODE'
          exporting
            barcode   = lc_barcode
            dmbtr     = l_vlrliq
          importing
            esrre     = lc_esrre
            esrnr     = lc_esrnr
          exceptions
            not_valid = 1
            others    = 2.

*Inicio Leandro Valentim Ferreira - #114554
        if sy-subrc is not initial  or sy-msgid = '8B'.
          clear sy-msgid.
          call function 'CONVERT_BARCODE'
            exporting
              barcode   = lc_barcode
              dmbtr     = l_vlrliq2
            importing
              esrre     = lc_esrre
              esrnr     = lc_esrnr
            exceptions
              not_valid = 1
              others    = 2.
          if sy-subrc is not initial  or sy-msgid = '8B'.
            perform f_put_mensagem2
              using sy-msgty
                    sy-msgid
                    sy-msgno
                    sy-msgv1
                    sy-msgv2
                    sy-msgv3
                    sy-msgv4.

            p_erro = 'X'.
          endif.
        endif.
*Fim Leandro Valentim Ferreira - #114554
      endif.

  endtry.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_LOG
*&---------------------------------------------------------------------*
form f_save_log .

  data lt_005 type table of zibt_nfse_005.

  loop at gt_bapiret2 assigning field-symbol(<fs_mess>).

    append initial line to lt_005 assigning field-symbol(<fs_005>).

    <fs_005>-guid_header = zibs_nfse_001-guid_header.
    <fs_005>-dt_registro = sy-datum.
    <fs_005>-hr_registro = sy-uzeit.
    <fs_005>-msgty = <fs_mess>-type.
    <fs_005>-msgid = <fs_mess>-id.
    <fs_005>-msgno = <fs_mess>-number.
    <fs_005>-message = <fs_mess>-message.
    <fs_005>-msgv1 = <fs_mess>-message_v1.
    <fs_005>-msgv2 = <fs_mess>-message_v2.
    <fs_005>-msgv3 = <fs_mess>-message_v3.
    <fs_005>-msgv4 = <fs_mess>-message_v4.
    <fs_005>-us_registro = sy-uname.

  endloop.

  check lt_005 is not initial.

  modify zibt_nfse_005 from table lt_005.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_TOTALS_3000
*&---------------------------------------------------------------------*
form f_fill_totals_3000 .

  clear gw_scr_popup_list-dmbtr_iv.

  loop at gt_service assigning field-symbol(<fs_service>)
    where ebeln = gw_scr_popup_list-ebeln
      and ebelp = gw_scr_popup_list-ebelp.

    " 08.12.2022 - ramon correção lçto -->
    "ADD <fs_service>-menge TO gw_scr_popup_list-dmbtr_iv.
    gw_scr_popup_list-dmbtr_iv =  gw_scr_popup_list-dmbtr_iv + ( <fs_service>-menge * <fs_service>-tbtwr ).
    " 08.12.2022 - ramon correção lçto --<

  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_PDF
*&---------------------------------------------------------------------*
form f_download_pdf using p_guid_header type /tcsr/e_guid_header.

  constants: lc_file_type type char10 value 'BIN'.

  data: lt_image_tab       type w3mimetabtype,
        lv_selected_folder type string,
        lv_file_name       type string,
        lv_size            type i.

  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title         = conv #( text-m02 ) "Diretório
      initial_folder       = 'C:\'
    changing
      selected_folder      = lv_selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

  call function '/TCSR/F_PDF_DISPLAY'
    exporting
      iv_guid_header    = p_guid_header
      iv_op_type        = '1'
    importing
      ev_image_size     = lv_size
      et_image_tab      = lt_image_tab
    exceptions
      pdf_not_found     = 1
      error_display_pdf = 2
      others            = 3.
  if sy-subrc eq 0.

    concatenate lv_selected_folder '\' 'NFSe.pdf'
           into lv_file_name.

    " Download xml to the selected destination
    cl_gui_frontend_services=>gui_download(
      exporting
        filename         = lv_file_name
        bin_filesize     = lv_size
        filetype         = lc_file_type
      changing
        data_tab         = lt_image_tab
      exceptions
        file_write_error = 1
        no_authority     = 5
        unknown_error    = 6
        access_denied    = 15
        others           = 24 ).

  endif.


endform.
*&---------------------------------------------------------------------*
*&      FORM  F_DOWNLOAD_PDF_LINE
*&---------------------------------------------------------------------*
form f_download_pdf_line using p_guid_header type /tcsr/e_guid_header.

  constants: lc_file_type type char10 value 'BIN'.

  data: lt_image_tab       type w3mimetabtype,
        lv_selected_folder type string,
        lv_file_name       type string,
        lv_size            type i.

  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title         = conv #( text-m02 ) "Diretório
      initial_folder       = 'C:\'
    changing
      selected_folder      = lv_selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

  call function '/TCSR/F_PDF_DISPLAY'
    exporting
      iv_guid_header    = p_guid_header
      iv_op_type        = '1'
    importing
      ev_image_size     = lv_size
      et_image_tab      = lt_image_tab
    exceptions
      pdf_not_found     = 1
      error_display_pdf = 2
      others            = 3.
  if sy-subrc eq 0.

    concatenate lv_selected_folder '\' 'NFSe.pdf'
           into lv_file_name.

    " Download xml to the selected destination
    cl_gui_frontend_services=>gui_download(
      exporting
        filename         = lv_file_name
        bin_filesize     = lv_size
        filetype         = lc_file_type
      changing
        data_tab         = lt_image_tab
      exceptions
        file_write_error = 1
        no_authority     = 5
        unknown_error    = 6
        access_denied    = 15
        others           = 24 ).

  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XML
*&---------------------------------------------------------------------*
form f_download_xml using p_guid_header type /tcsr/e_guid_header. .

  constants: lc_file_type type char10 value 'BIN'.

  data: lv_xml_content     type xstring,
        lv_string          type xstring,
        lv_size            type i,
        lv_selected_folder type string,
        lv_file_name       type string,
        lt_xml_tab         type dcxmllines,
        lo_dom             type ref to if_ixml_document.

  select single xmlstring
    from /tcsr/t_xml
      into @data(xml_content)
        where guid_header = @p_guid_header.

  check sy-subrc eq 0.

  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title         = conv #( text-m02 ) "Diretório
      initial_folder       = 'C:\'
    changing
      selected_folder      = lv_selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

  move xml_content to lv_xml_content.

  check lv_xml_content is not initial.

  call function 'SDIXML_XML_TO_DOM'
    exporting
      xml           = lv_xml_content
    importing
      document      = lo_dom
    exceptions
      invalid_input = 1
      others        = 2.
  if sy-subrc ne 0.
    clear: lo_dom.
  endif.

  refresh: lt_xml_tab[].

  " Convert DOM to XML doc (table)
  call function 'SDIXML_DOM_TO_XML'
    exporting
      document      = lo_dom
      pretty_print  = ' '
    importing
      xml_as_string = lv_string
      size          = lv_size
    tables
      xml_as_table  = lt_xml_tab
    exceptions
      no_document   = 1
      others        = 2.

  concatenate lv_selected_folder '\' 'NFSe.xml'
         into lv_file_name.

  cl_gui_frontend_services=>gui_download(
    exporting
      filename         = lv_file_name
      bin_filesize     = lv_size
      filetype         = lc_file_type
    changing
      data_tab         = lt_xml_tab
    exceptions
      file_write_error = 1
      no_authority     = 5
      unknown_error    = 6
      access_denied    = 15
      others           = 24 ).

endform.
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XML_LINE
*&---------------------------------------------------------------------*
form f_download_xml_line using p_guid_header type /tcsr/e_guid_header. .

  constants: lc_file_type type char10 value 'BIN'.

  data: lv_xml_content     type xstring,
        lv_string          type xstring,
        lv_size            type i,
        lv_selected_folder type string,
        lv_file_name       type string,
        lt_xml_tab         type dcxmllines,
        lo_dom             type ref to if_ixml_document.

  select single xmlstring
    from /tcsr/t_xml
      into @data(xml_content)
        where guid_header = @p_guid_header.

  check sy-subrc eq 0.

  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title         = conv #( text-m02 ) "Diretório
      initial_folder       = 'C:\'
    changing
      selected_folder      = lv_selected_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

  move xml_content to lv_xml_content.

  check lv_xml_content is not initial.

  call function 'SDIXML_XML_TO_DOM'
    exporting
      xml           = lv_xml_content
    importing
      document      = lo_dom
    exceptions
      invalid_input = 1
      others        = 2.
  if sy-subrc ne 0.
    clear: lo_dom.
  endif.

  refresh: lt_xml_tab[].

  " Convert DOM to XML doc (table)
  call function 'SDIXML_DOM_TO_XML'
    exporting
      document      = lo_dom
      pretty_print  = ' '
    importing
      xml_as_string = lv_string
      size          = lv_size
    tables
      xml_as_table  = lt_xml_tab
    exceptions
      no_document   = 1
      others        = 2.

  concatenate lv_selected_folder '\' 'NFSe.xml'
         into lv_file_name.

  cl_gui_frontend_services=>gui_download(
    exporting
      filename         = lv_file_name
      bin_filesize     = lv_size
      filetype         = lc_file_type
    changing
      data_tab         = lt_xml_tab
    exceptions
      file_write_error = 1
      no_authority     = 5
      unknown_error    = 6
      access_denied    = 15
      others           = 24 ).



endform.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_BANCO
*&---------------------------------------------------------------------*
form f_dados_banco using p_bukrs type bukrs
                         p_lifnr type lifnr
                         p_bvtyp type bvtyp
                changing p_hbkid type hbkid.

  check p_hbkid is initial.

  call function 'Z_RET_FORMA_PAGAMENTO'
    exporting
      p_bukrs          = p_bukrs
      p_lifnr          = p_lifnr
*     P_ZLSCH          =
*     P_VALOR          =
*     P_COTACAO        = 1
      p_bvtyp          = p_bvtyp
*     P_WAERS          =
    importing
*     P_FORMA_PAGAMENTO        =
      p_princ_bnc_emp  = p_hbkid
*     P_INFO_FORNECEDOR        =
*     P_BANCO_FORNECEDOR       =
    exceptions
      nao_fornecedor   = 1
      fornecedor_conta = 2
      fornecedor_banco = 3
      faixa_valor      = 4
      banco_empresa    = 5
      others           = 6.

  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR
*&---------------------------------------------------------------------*
form f_estornar .

  data lv_ret type c.

  perform f_popup_to_confirm
    using 'Confirmar processo de estorno?'
 changing lv_ret.

  check lv_ret = '1'.

  check go_znfse is bound.

  call method go_znfse->estorno( ).

  go_znfse->show_message( ).

  data(lw_ret) = go_znfse->get_nfse_001( ).

  move-corresponding lw_ret to zibt_nfse_001.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_TELA_2000
*&---------------------------------------------------------------------*
form f_atualiza_tela_2000 .

  select single * from zibt_nfse_001
    into @data(lw_header)
      where guid_header = @zibs_nfse_001-guid_header.

  check sy-subrc eq 0.

  move-corresponding lw_header to zsheader_data_nfse_inbound.
  move-corresponding lw_header to zibs_nfse_001.

  move-corresponding lw_header to zibt_nfse_001.

*  zsheader_data_nfse_inbound-se_recordid =
*  zsheader_data_nfse_inbound-se_detail
*
*    zsheader_data_nfse_inbound = c_header.
*  zibs_nfse_001 = c_nfse.
*  zspayment_data_nfse_inbound = c_payment.
*  gt_scr_condition_list = it_cond_items.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_BANCO_TELA
*&---------------------------------------------------------------------*
form f_dados_banco_tela using p_header type zsheader_data_nfse_inbound
                     changing p_data type zspayment_data_nfse_inbound.

  if p_header-lifnr is not initial.

    select bvtyp,bnka~bankl,bankn,banka from lfbk
      inner join bnka on bnka~bankl = lfbk~bankl
      into table @data(lt_payment)
        where lifnr = @p_header-lifnr.


    if sy-subrc is initial.

      if lines( lt_payment ) > 1.

        read table lt_payment assigning field-symbol(<fs_payment>)
          with key bvtyp = p_data-bvtyp.

      else.

        read table lt_payment assigning <fs_payment>
          index 1.

      endif.

      if sy-subrc eq 0.
        if zspayment_data_nfse_inbound-boleto is not initial.
          call method zcl_miro=>get_formapag_banco_empresa
            exporting
              i_bukrs           = p_header-bukrs
              i_lifnr           = p_header-lifnr
              i_bvtyp           = <fs_payment>-bvtyp
              i_zlsch           = 'E'
            importing
              e_forma_pagamento = p_data-pymt_meth.
        else.
          call method zcl_miro=>get_formapag_banco_empresa
            exporting
              i_bukrs           = p_header-bukrs
              i_lifnr           = p_header-lifnr
              i_bvtyp           = <fs_payment>-bvtyp
            importing
              e_forma_pagamento = p_data-pymt_meth.
        endif.

        p_data-bvtyp = <fs_payment>-bvtyp.
        p_data-bankn = <fs_payment>-bankn.
        p_data-bankl = <fs_payment>-bankl.
        p_data-banka = <fs_payment>-banka.

      endif.

    elseif  zspayment_data_nfse_inbound-boleto is not initial.
      call method zcl_miro=>get_formapag_banco_empresa
        exporting
          i_bukrs           = p_header-bukrs
          i_lifnr           = p_header-lifnr
          i_bvtyp           = p_data-bvtyp
          i_zlsch           = 'E'
        importing
          e_forma_pagamento = p_data-pymt_meth
          e_banco_empresa   = p_data-hbkid.
      clear: p_data-bvtyp, p_data-bankn, p_data-bankl,p_data-banka.
    endif.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_GLOBAL_NFSE_CHECK
*&---------------------------------------------------------------------*
form f_global_nfse_check using p_nfse type zibt_nfse_001.

  check p_nfse is not initial.

  if p_nfse-lifnr is not initial and p_nfse-zbvtyp is not initial.

    call function 'FI_BVTYP_CHECK'
      exporting
        i_konko     = p_nfse-lifnr
        i_koart     = 'K'
        i_bvtyp     = p_nfse-zbvtyp
      exceptions
        wrong_input = 1
        others      = 2.

    if sy-subrc <> 0.

      message id sy-msgid type 'I' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_4000_INIT
*&---------------------------------------------------------------------*
form f_alv_4000_init .

  perform f_alv_4000_init_01.

  perform f_alv_4000_init_02.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_4000_INIT_01
*&---------------------------------------------------------------------*
form f_alv_4000_init_01.

  data lt_fieldcat type lvc_t_fcat.
  data lw_settings type lvc_s_glay.
  data lw_layout type lvc_s_layo.
  data lw_variant type disvariant.

  data lo_events type ref to cl_salv_events_table.
  data lo_handle type ref to lcl_event_handler.

  if go_cc_4000_01 is initial.

    create object go_cc_4000_01
      exporting
        container_name              = 'CC_ALV_01'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    if sy-subrc ne 0.
      message i000(d2) with 'The custom control could not be created'.
      return.
    endif.

  endif.

  lw_layout-sel_mode = 'A'.

  "lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.

  if go_alv_4000_01 is initial.

    create object go_alv_4000_01
      exporting
        i_parent = go_cc_4000_01.

    perform f_monta_fieldcat using 'ZIBS_NFPS_001' changing lt_fieldcat.

    delete lt_fieldcat where fieldname = 'SELEC'.
    delete lt_fieldcat where fieldname = 'DATBG'.
    delete lt_fieldcat where fieldname = 'CHAVE_NFE'.
    delete lt_fieldcat where fieldname = 'DOCDAT'.
    delete lt_fieldcat where fieldname = 'LIFNR'.
    delete lt_fieldcat where fieldname = 'NAO_ASSOCI'.
    delete lt_fieldcat where fieldname = 'JA_ASSOCI'.
    delete lt_fieldcat where fieldname = 'COM_MIRO'.
    delete lt_fieldcat where fieldname = 'TODAS'.

    delete lt_fieldcat where fieldname = 'MAX_TOL'.
    delete lt_fieldcat where fieldname = 'NFSE_TOTAL'.
    delete lt_fieldcat where fieldname = 'VI_TOTAL'.
    delete lt_fieldcat where fieldname = 'PERDA_TOTAL'.
    delete lt_fieldcat where fieldname = 'QUEBRA_TOTAL'.
    delete lt_fieldcat where fieldname = 'ISS_TOTAL'.
    delete lt_fieldcat where fieldname = 'TOTAL_LIQ'.
    delete lt_fieldcat where fieldname = 'ESSR_BUDAT'.

    perform f_coluna_edita2 using 'REGIO_EMISSOR'  'UF.Org' 'UF.Org' changing lt_fieldcat.
    perform f_coluna_edita2 using 'REGIO_RECEPTOR'  'UF.Des' 'UF.Des' changing lt_fieldcat.
    perform f_coluna_edita2 using 'VALOR_PIS'  'PIS' 'Vlr PIS' changing lt_fieldcat.
    perform f_coluna_edita2 using 'VALOR_COFINS'  'COFINS' 'Vlr COFINS' changing lt_fieldcat.
    perform f_coluna_edita2 using 'PARC_CLIENTE' 'Parc.NF' 'Parceiro NF' changing lt_fieldcat.
    perform f_coluna_edita2 using 'NAME1' 'DescPar' 'Parceiro' changing lt_fieldcat.

    perform f_coluna_edita2 using 'ZPERDA' 'Perda' 'Perda' changing lt_fieldcat.

    perform f_coluna_edita2 using 'ZVLR_VI' 'Vlr.VI' 'Vlr.VI' changing lt_fieldcat.

    perform f_coluna_edita2 using 'RE_BELNR' 'Doc.Miro' 'Doc.Miro' changing lt_fieldcat.
    perform f_coluna_edita2 using 'RE_GJAHR' 'Ano.Miro' 'Ano.Miro' changing lt_fieldcat.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    "CREATE OBJECT lo_handle.

    "SET HANDLER lo_handle->handle_double_click FOR go_alv_4000_01.
    "set HANDLER lo_handle->
    "SET HANDLER lo_handle->handle_data_changed FOR go_005_alv.

    "SET HANDLER lo_handle->handle_top_of_page FOR go_005_alv.

    "PERFORM f_filtro_lote_alv.

    "go_cc_alv_01->SET_

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    call method go_alv_4000_01->set_table_for_first_display
      exporting
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      changing
        it_outtab       = gt_4000_alv_01
        it_fieldcatalog = lt_fieldcat.

  else.

    call method go_alv_4000_01->set_frontend_layout
      exporting
        is_layout = lw_layout.

    call method go_alv_4000_01->refresh_table_display( ).

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
form f_coluna_edita2  using p_fieldname type slis_fieldname
                            p_text_s type scrtext_s
                            p_text_l type scrtext_l
                   changing p_field_cat type lvc_t_fcat.

  read table p_field_cat assigning field-symbol(<fs_cat>)
    with key fieldname = p_fieldname.

  check sy-subrc eq 0.

  <fs_cat>-scrtext_s  = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_l.
  <fs_cat>-reptext = p_text_s.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_4000_INIT_02
*&---------------------------------------------------------------------*
form f_alv_4000_init_02.

  data lt_fieldcat type lvc_t_fcat.
  data lw_settings type lvc_s_glay.
  data lw_layout type lvc_s_layo.

  data lw_variant type disvariant.

  data lo_events type ref to cl_salv_events_table.

  data lo_handle type ref to lcl_event_handler.

  if go_cc_4000_02 is initial.

    create object go_cc_4000_02
      exporting
        container_name              = 'CC_ALV_02'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    if sy-subrc ne 0.
      message i000(d2) with 'The custom control could not be created'.
      return.
    endif.

  endif.

  check go_alv_4000_02 is initial.

  create object go_alv_4000_02
    exporting
      i_parent = go_cc_4000_02.

  "lw_layout-grid_title = sy-title.

  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  "lw_layout-no_toolbar = 'X'. XXXXX
  "lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  "lw_layout-

  "lw_settings-edt_cll_cb = 'X'.

  perform f_fieldcat_4000_alv_02
    using space
 changing lt_fieldcat.

  call method go_alv_4000_02->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  create object lo_handle.

  "SET HANDLER lo_handle->handle_double_click FOR go_alv_4000_02.
  "set HANDLER lo_handle->
  set handler lo_handle->handle_data_changed for go_alv_4000_02.

  lw_variant-report       = sy-repid.
  lw_variant-username     = sy-uname.

  " Configuration for first display.
  call method go_alv_4000_02->set_table_for_first_display
    exporting
      is_layout       = lw_layout
      is_variant      = lw_variant
      i_save          = 'A'
    changing
      it_outtab       = gt_4000_alv_02
      it_fieldcatalog = lt_fieldcat.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT_4000_1
*&---------------------------------------------------------------------*
form f_monta_fieldcat using p_struct type tabname
                   changing p_field_cat type lvc_t_fcat.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = p_struct
    changing
      ct_fieldcat            = p_field_cat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  if sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      FORM  F_DOUBLE_CLICK
*&---------------------------------------------------------------------*
form f_double_click using e_row type lvc_s_row
                          e_column type lvc_s_col
                          es_row_no type lvc_s_roid.




endform.
*&---------------------------------------------------------------------*
*&      Form  F_MARK_4001
*&---------------------------------------------------------------------*
form f_mark_4001 using po_alv type ref to cl_gui_alv_grid
                       p_alv_number type c
                       p_checked type flag.

*  DATA ls_row  TYPE lvc_s_row.
*  DATA lt_rows TYPE lvc_t_row.
*  DATA lt_no   TYPE lvc_t_roid.
*
*  DATA lv_tab TYPE i.
*
*  CHECK po_alv IS BOUND.
*
*  IF p_checked = 'X'.
*    IF p_alv_number = '01'.
*      lv_tab = lines( gt_4000_alv_01 ).
*    ELSE.
*      lv_tab = lines( gt_4000_alv_02 ).
*    ENDIF.
*  ENDIF.
*
*  DO lv_tab TIMES.
*
*    ls_row-index = syst-index.
*    APPEND ls_row TO lt_rows.
*
*  ENDDO.
*
*  CALL METHOD po_alv->set_selected_rows
*    EXPORTING
*      is_keep_other_selections = space
*      it_index_rows            = lt_rows.
*
*  "PERFORM f_refresh_grid USING 'X' space.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SEARCH_4000
*&---------------------------------------------------------------------*
form f_search_4000 using p_no_search type c.

  data lr_tknum type range of tknum.
  data lr_tdlnr type range of tdlnr.
  data lw_zlest0110 type  zlest0110.
  data lt_item_nota type table of j_1bnflin.
  data lt_zlest0110 type table of zlest0110.

  data lv_guid_initial type /tcsr/e_guid_header.

  clear gt_xnota.

  if so_datr[] is initial.

    message s016(ds) with 'Data de entrega' 'é obrigatório' display like 'E'.
    exit.

  endif.

  data(lt_data) = so_datr[].

  perform f_get_impostos.

  if p_no_search = 'X'.

    select 'I' as sign, 'EQ' as option, tknum as low
      from zlest0034
        into corresponding fields of table @lr_tknum
          where guid_header = @gs_nfse_4000-guid_header.

    clear lt_data.

  else.

    if zibs_nfps_001-tknum is not initial.
      append 'IEQ' && zibs_nfps_001-tknum to lr_tknum.
    endif.

    if so_tknum[] is not initial.
      append lines of so_tknum to lr_tknum.
    endif.

  endif.

  if gs_nfse_4000-lifnr is not initial.
    append 'IEQ' && gs_nfse_4000-lifnr to lr_tdlnr.
  endif.

*  " RAMON DEBUG REMOVER -->
*  CLEAR so_datr[].
*  CLEAR lr_tknum[].
*
*  APPEND 'IEQ' && '0003145777' TO lr_tknum.
*  APPEND 'IEQ' && '0003145793' TO lr_tknum.
*  APPEND 'IEQ' && '0003145794' TO lr_tknum.
*  APPEND 'IEQ' && '0003145795' TO lr_tknum.
*  APPEND 'IEQ' && '0003145802' TO lr_tknum.
*
*  " RAMON DEBUG REMOVER --<


  " 2.1.1  Busca dados de transporte na VTTK
  select * from vttk
    into table @data(lt_vttk)
       where datbg in @lt_data
         and tknum in @lr_tknum
         and tdlnr in @lr_tdlnr
         and vsart = '01' .

  if sy-subrc <> 0.

    message s016(ds) with 'Dados não encontrados' display like 'E'.
    exit.

  endif.

  " 2.1.2 - Seleção de dados da ZLEST0032 para identificar se existe miro gerada
  select * from zlest0032
    into table @data(lt_0032)
      for all entries in @lt_vttk
        where tknum = @lt_vttk-tknum.
  "AND belnr = @space. """"xxxxxxxxxx

*  CHECK sy-subrc EQ 0.

  " 2.1.3 - Validar se doc de transporte (TKNUM) não esta associado a uma NFS-e
  select * from zlest0034
    into table @data(lt_0034)
      for all entries in @lt_vttk
        where tknum = @lt_vttk-tknum.
  "AND guid_header = @lv_guid_initial.

*  IF sy-subrc <> 0.
*
*    MESSAGE s016(ds) WITH 'Dados não encontrados' DISPLAY LIKE 'E'.
*    EXIT.
*
*  ENDIF.

  loop at lt_vttk assigning field-symbol(<fs_vttk>).

    clear: lw_zlest0110, lt_item_nota, lt_zlest0110.

    " 2.1.4 - Opção "Processo Pagamento não Iniciado" - Usar a função Z_LES_NOTAS_VT para encontrar  dados da nota fiscal
    " vinculada ao doc de transporte

    call function 'Z_LES_NOTAS_VT'
      exporting
        i_vttk       = <fs_vttk>
      importing
        e_zlest0110  = lw_zlest0110
      tables
        it_item_nota = lt_item_nota
        it_zlest0110 = lt_zlest0110.

    if lines( lt_item_nota ) > 0.

      append initial line to gt_xnota assigning field-symbol(<fs_xnota>).

      <fs_xnota>-tknum = <fs_vttk>-tknum.
      <fs_xnota>-tdlnr = <fs_vttk>-tdlnr.
      <fs_xnota>-shtyp = <fs_vttk>-shtyp.
      <fs_xnota>-vsart = '01'.
      <fs_xnota>-add03 = <fs_vttk>-add03.
      <fs_xnota>-abfer = <fs_vttk>-abfer.

      <fs_xnota>-datbg = <fs_vttk>-datbg.
      <fs_xnota>-docnum = lt_item_nota[ 1 ]-docnum.
      <fs_xnota>-terceiro = space.
      <fs_xnota>-matnr = lt_item_nota[ 1 ]-matnr.
      <fs_xnota>-zpeso_origem = lt_item_nota[ 1 ]-menge.
      <fs_xnota>-mwskz = value #( lt_item_nota[ 1 ]-mwskz default 'S1' ).
      <fs_xnota>-meins = lt_item_nota[ 1 ]-meins.
      <fs_xnota>-netwr = lt_item_nota[ 1 ]-netwr.
      <fs_xnota>-werks = lt_item_nota[ 1 ]-werks.

      " 2.4.3 - Valor da NFS-e
      <fs_xnota>-xnfs_value = gs_nfse_4000-nfse_value.
      <fs_xnota>-dtemissao = gs_nfse_4000-dtemissao.

      <fs_xnota>-xnfs_vlr_iss = value #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value default '0.00' ).
      <fs_xnota>-waers = 'BRL'.

      read table lt_0034 assigning field-symbol(<fs_0034>)
        with key tknum = <fs_vttk>-tknum.

      if sy-subrc eq 0.

        <fs_xnota>-guid_header = <fs_0034>-guid_header.
        <fs_xnota>-re_belnr  = <fs_0034>-re_belnr.
        <fs_xnota>-re_gjahr  = <fs_0034>-re_gjahr.
        <fs_xnota>-en_docnum = <fs_0034>-en_docnum.

        <fs_xnota>-peso_descarga = <fs_0034>-zpeso_destino.
        <fs_xnota>-data_descarga = <fs_0034>-zdt_chegada.

        "<fs_xnota>-data_descarga = <fs_0034>-zdt_vencto.
        "<fs_xnota>-peso_descarga = <fs_0034>-zpeso_origem.

        <fs_xnota>-user_assoc = <fs_0034>-user_reg.
        <fs_xnota>-data_assoc = <fs_0034>-data_reg.
        <fs_xnota>-hora_assoc = <fs_0034>-hora_reg.

      endif.

      read table lt_0032 assigning field-symbol(<fs_0032>)
        with key tknum = <fs_vttk>-tknum.

      if sy-subrc eq 0.
        <fs_xnota>-belnr = <fs_0032>-belnr.
        <fs_xnota>-gjahr = <fs_0032>-gjahr.
      endif.

    endif.

    if lines( lt_zlest0110 ) > 0.

      append initial line to gt_xnota assigning <fs_xnota>.

      <fs_xnota>-tknum = <fs_vttk>-tknum.

      <fs_xnota>-tdlnr = <fs_vttk>-tdlnr.
      <fs_xnota>-shtyp = <fs_vttk>-shtyp.
      <fs_xnota>-vsart = '01'.
      <fs_xnota>-add03 = <fs_vttk>-add03.
      <fs_xnota>-abfer = <fs_vttk>-abfer.

      <fs_xnota>-datbg = <fs_vttk>-datbg.
      <fs_xnota>-terceiro = 'X'.
      <fs_xnota>-docnum = lt_zlest0110[ 1 ]-docnum.
      <fs_xnota>-matnr = lt_zlest0110[ 1 ]-material.
      <fs_xnota>-zpeso_origem = lt_zlest0110[ 1 ]-quantidade.
      <fs_xnota>-meins = lt_zlest0110[ 1 ]-unidade.
      <fs_xnota>-netwr = lt_zlest0110[ 1 ]-vl_nota_fiscal.
      <fs_xnota>-waers = 'BRL'.

      " 2.4.3 - Valor da NFS-e
      <fs_xnota>-xnfs_value = gs_nfse_4000-nfse_value.
      <fs_xnota>-dtemissao = gs_nfse_4000-dtemissao.

      <fs_xnota>-werks = lt_zlest0110[ 1 ]-werks.
      <fs_xnota>-chave = lt_zlest0110[ 1 ]-chave.
      <fs_xnota>-serie = lt_zlest0110[ 1 ]-serie.
      <fs_xnota>-numero = lt_zlest0110[ 1 ]-numero.
      <fs_xnota>-parc_cliente = lt_zlest0110[ 1 ]-cliente.
      <fs_xnota>-partyp = lt_zlest0110[ 1 ]-partyp.
      "<fs_xnota>-xnfs_value = zibs_nfse_001-nfse_value.
      <fs_xnota>-xnfs_vlr_iss = value #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value default '0.00' ).

      read table lt_0034 assigning <fs_0034>
        with key tknum = <fs_vttk>-tknum.

      if sy-subrc eq 0.

        <fs_xnota>-mwskz = <fs_0034>-iva.
        <fs_xnota>-guid_header = <fs_0034>-guid_header.
        <fs_xnota>-re_belnr  = <fs_0034>-re_belnr.
        <fs_xnota>-re_gjahr  = <fs_0034>-re_gjahr.
        <fs_xnota>-en_docnum = <fs_0034>-en_docnum.

        <fs_xnota>-peso_descarga = <fs_0034>-zpeso_destino.
        <fs_xnota>-data_descarga = <fs_0034>-zdt_chegada.

        "<fs_xnota>-data_descarga = <fs_0034>-zdt_vencto.
        "<fs_xnota>-peso_descarga = <fs_0034>-zpeso_origem.

        <fs_xnota>-user_assoc = <fs_0034>-user_reg.
        <fs_xnota>-data_assoc = <fs_0034>-data_reg.
        <fs_xnota>-hora_assoc = <fs_0034>-hora_reg.

      endif.

      read table lt_0032 assigning <fs_0032>
        with key tknum = <fs_vttk>-tknum.

      if sy-subrc eq 0.
        <fs_xnota>-belnr = <fs_0032>-belnr.
        <fs_xnota>-gjahr = <fs_0032>-gjahr.
      endif.

    endif.

  endloop.

  perform f_dados_nota changing gt_xnota.

  perform f_nota_filtro changing gt_xnota.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_GRID
*&---------------------------------------------------------------------*
form f_alv_refresh_grid_4000 using p_01 type c
                                   p_02 type c.

  if p_01 is not initial.

    if go_alv_4000_01 is not initial.
      go_alv_4000_01->refresh_table_display( ).
    endif.

  endif.

  if p_02 is not initial.

    if go_alv_4000_02 is not initial.
      go_alv_4000_02->refresh_table_display( ).
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_CAB_XNOTA
*&---------------------------------------------------------------------*
form f_dados_nota changing ct_nota type zibc_nfps_xnota.

  data lv_data_to type j_1btxdatf.

  perform f_cab_nf_sap changing ct_nota.

  perform f_cab_nf_ter changing ct_nota.

  check ct_nota is not initial.

  perform f_get_data_form changing lv_data_to.

  select * from j_1btxpis
    into table @data(lt_pis)
      for all entries in @ct_nota
        where country = 'BR'
          and gruop   = '72'
          and value   = @ct_nota-value_branch
          and validfrom >= @lv_data_to.

  select * from j_1btxcof
    into table @data(lt_cof)
      for all entries in @ct_nota
        where country = 'BR'
          and gruop   = '71'
          and value   =  @ct_nota-value_branch
          and validfrom >= @lv_data_to.

  select * from zlest0037
    into table @data(lt_0037)
    for all entries in @ct_nota
      where bukrs = @ct_nota-bukrs.

  select matnr,maktx from makt
  into table @data(lt_makt)
    for all entries in @ct_nota
      where matnr = @ct_nota-matnr
        and spras = @sy-langu.

  select * from a912
    into table @data(lt_a912)
      for all entries in @ct_nota
        where kappl = 'F'
          and kschl = 'ZMRG'
          and matnr = @ct_nota-matnr
          and datbi >= @ct_nota-datbg.

  if sy-subrc eq 0.

    select * from konp
      into table @data(lt_konp)
        for all entries in @lt_a912
          where knumh = @lt_a912-knumh
            and krech = 'A'.

  endif.

  loop at ct_nota assigning field-symbol(<fs_nota>).

    read table lt_makt assigning field-symbol(<fs_makt>)
        with key matnr = <fs_nota>-matnr.

    if sy-subrc eq 0.

      <fs_nota>-maktx = <fs_makt>-maktx.

    endif.

    read table lt_0037 assigning field-symbol(<fs_0037>)
        with key bukrs = <fs_nota>-bukrs
                 cd_modal   = '01'
                 ck_servico = 'X'.

    if sy-subrc eq 0.

      <fs_nota>-matns = <fs_0037>-matnr.

    endif.

    " 2.4.8  - Cálculo da perda KG
    read table lt_a912 assigning field-symbol(<fs_a912>)
      with key matnr = <fs_nota>-matnr.

    if sy-subrc eq 0.

      read table lt_konp assigning field-symbol(<fs_konp>)
        with key knumh = <fs_a912>-knumh.

      if sy-subrc eq 0.

        <fs_nota>-quant_tolerancia = ( <fs_nota>-zpeso_origem * ( <fs_konp>-kbetr / 10 ) ) / 100.

        "<fs_nota>-xfrete_perda = <fs_nota>-xfrete_quebra + <fs_nota>-quant_tolerancia. "XXXX

      endif.

    endif.

    perform f_calculo_linha changing <fs_nota>.

    " 2.4.6 - Cálculo da quebra KG
    "<fs_nota>-xfrete_quebra = <fs_nota>-menge - <fs_nota>-peso_descarga.

    " 2.4.7 - Valor de quebra
    "<fs_nota>-xvlr_quebra = <fs_nota>-xvlr_unit_fret * <fs_nota>-xfrete_quebra.



    " 2.4.9 - Cálculo da perda em valor
    "<fs_nota>-xfrete_vlr_perda =   <fs_nota>-xfrete_perda * <fs_nota>-xvlr_unit_fret.
    "<fs_nota>-xfrete_vlr_perda  = <fs_nota>-xfrete_perda  * ( <fs_nota>-netwr / <fs_nota>-menge ).

    " 2.4.10 - Liquido a pagar
    "<fs_nota>-xvlr_liq_vi = <fs_nota>-xfrete_netwr - <fs_nota>-xfrete_vlr_perda - <fs_nota>-xvlr_quebra - <fs_nota>-xnfs_vlr_iss.

    "<fs_nota>-xvlr_liq_nfs = <fs_nota>-xnfs_value - <fs_nota>-xfrete_vlr_perda - <fs_nota>-xvlr_quebra - <fs_nota>-xnfs_vlr_iss.

    " 2.4.12 - Alíquotas e valor dos impostos baseado no Iva - PAREI AQUI
    read table lt_pis assigning field-symbol(<fs_pis>)
      with key gruop   = '72'
               value   =  <fs_nota>-value_branch.

    if sy-subrc eq 0.

      <fs_nota>-base_pis = <fs_nota>-xfrete_netwr * ( <fs_pis>-base / 100 ).
      <fs_nota>-xvlr_pis = <fs_nota>-xfrete_netwr * ( <fs_pis>-rate / 100 ).
      "<fs_nota>-xvlr_pis = <fs_nota>-base_pis  *  ( <fs_pis>-rate / 100 ).
    endif.

    read table lt_cof assigning field-symbol(<fs_cof>)
      with key gruop   = '71'
               value   =  <fs_nota>-value_branch.

    if sy-subrc eq 0.

      <fs_nota>-base_cofins = <fs_nota>-xfrete_netwr * ( <fs_cof>-base / 100 ).
      <fs_nota>-xvlr_cofins = <fs_nota>-xfrete_netwr * ( <fs_cof>-rate / 100 ).
      "<fs_nota>-xvlr_cofins = <fs_nota>-base_cofins  *  ( <fs_cof>-rate / 100 ).
    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CAB_NF_SAP
*&---------------------------------------------------------------------*
form f_cab_nf_sap changing ct_nota type zibc_nfps_xnota.

  data(lt_nota_sap) = ct_nota.
  data lv_werks type werks_d.
  data lv_nfkey type j_1b_nfe_access_key.

  " retira as nf de terceiro
  delete lt_nota_sap where terceiro = 'X'.

  check lt_nota_sap is not initial.

  select * from j_1bnfdoc
    into table @data(lt_doc)
      for all entries in @lt_nota_sap
        where docnum = @lt_nota_sap-docnum.

  if lt_doc is not initial.

    select * from j_1bnfe_active
      into table @data(lt_nfe)
        for all entries in @lt_doc
          where docnum = @lt_doc-docnum.

    select * from j_1bnfnad
      into table @data(lt_nad)
        for all entries in @lt_doc
          where docnum = @lt_doc-docnum
            and parvw in ('PC','LR').

    select * from zlest0039
      into table @data(lt_0039)
        for all entries in @lt_doc
          where docnum = @lt_doc-docnum.


    " 16.11.2022 - RAMON -->
    select * from vfkp
      into table @data(lt_vfkp)
        for all entries in @lt_nota_sap
          where rebel = @lt_nota_sap-tknum.

    if sy-subrc eq 0.

      select * from vfsi
        into table @data(lt_vfsi)
        for all entries in @lt_vfkp
          where knumv = @lt_vfkp-knumv.

      select * from essr
        into table @data(lt_essr)
        for all entries in @lt_vfkp
          where lblni = @lt_vfkp-lblni.

    endif.
    " 16.11.2022 - RAMON --<


  endif.

  loop at lt_doc assigning field-symbol(<fs_doc>).

    read table ct_nota assigning field-symbol(<fs_nota>)
      with key docnum = <fs_doc>-docnum.

    check sy-subrc eq 0.

    <fs_nota>-value_branch = <fs_doc>-branch.

    if <fs_doc>-nfenum is not initial.
      <fs_nota>-numero = <fs_doc>-nfenum.
    else.
      <fs_nota>-numero = <fs_doc>-nfnum.
    endif.

    <fs_nota>-serie = <fs_doc>-series.
    <fs_nota>-bukrs = <fs_doc>-bukrs.
    <fs_nota>-werks = <fs_doc>-branch.
    <fs_nota>-partyp = <fs_doc>-partyp.

    if <fs_doc>-parid is not initial.

      case <fs_nota>-partyp.

        when 'B'.

          <fs_nota>-parc_forn = <fs_doc>-parid+4.

          shift <fs_nota>-parc_forn left deleting leading '0'.

          lv_werks = <fs_nota>-parc_cliente.

          unpack lv_werks to <fs_nota>-parc_forn.

        when 'C'.

          <fs_nota>-parc_cliente = <fs_doc>-parid.

        when 'V'.

          <fs_nota>-parc_forn = <fs_doc>-parid.

      endcase.

      if <fs_nota>-parc_forn is not initial.

        select single name1 from lfa1
          into <fs_nota>-name1
            where lifnr = <fs_nota>-parc_forn.

      endif.

      if <fs_nota>-parc_cliente is not initial.

        select single name1 from kna1
          into <fs_nota>-name1
            where kunnr = <fs_nota>-parc_cliente.

      endif.

    endif.

    if <fs_doc>-nfe = 'X'.

      read table lt_nfe assigning field-symbol(<fs_nfe>)
        with key docnum = <fs_doc>-docnum.

      if sy-subrc eq 0.

        move-corresponding <fs_nfe> to lv_nfkey.

        <fs_nota>-chave = lv_nfkey.

      endif.

    endif.

    read table lt_0039 assigning field-symbol(<fs_0039>)
      with key docnum = <fs_doc>-docnum.

    if sy-subrc eq 0.

      if <fs_nota>-data_descarga is initial.
        <fs_nota>-data_descarga = <fs_0039>-datatransb.
      endif.

      if <fs_nota>-peso_descarga is initial.
        <fs_nota>-peso_descarga = <fs_0039>-pesotransb .
      endif.

      if <fs_nota>-data_descarga is initial.
        <fs_nota>-data_descarga = <fs_0039>-dataterminal.
      endif.

      if <fs_nota>-peso_descarga is initial.
        <fs_nota>-peso_descarga = <fs_0039>-pesoterminal.
      endif.

    endif.

    loop at lt_nad assigning field-symbol(<fs_nad>)
      where docnum = <fs_doc>-docnum..

      if <fs_nad>-partyp = 'V'.

        <fs_nota>-parc_v = <fs_nad>-parid.

        select single regio from lfa1
          into <fs_nota>-reg_emissor
            where lifnr = <fs_nad>-parid.

      endif.

      if <fs_nad>-partyp = 'C'.

        <fs_nota>-parc_c = <fs_nad>-parid.

        select single regio from kna1
          into <fs_nota>-reg_dest
            where kunnr = <fs_nad>-parid.

      endif.

    endloop.

    read table lt_vfkp assigning field-symbol(<fs_vfkp>)
         with key rebel = <fs_nota>-tknum.

    if sy-subrc eq 0.

      read table lt_vfsi assigning field-symbol(<fs_vfsi>)
        with key knumv = <fs_vfkp>-knumv.

      if sy-subrc eq 0.

        if <fs_vfsi>-kmein = 'TO'.
          <fs_nota>-xvlr_unit_fret = <fs_vfsi>-netpr / 1000.
        endif.

        if <fs_vfsi>-kmein = 'KG'.
          <fs_nota>-xvlr_unit_fret = <fs_vfsi>-netpr.
        endif.

        read table lt_essr assigning field-symbol(<fs_essr>)
          with key lblni = <fs_vfkp>-lblni.

        if sy-subrc eq 0.

          <fs_nota>-lwert = <fs_essr>-lwert.
          <fs_nota>-lfgja = <fs_essr>-budat(4).
          <fs_nota>-essr_budat = <fs_essr>-budat.

        endif.

      endif.

      <fs_nota>-xfrete_fknum = <fs_vfkp>-fknum.
      <fs_nota>-xfrete_ebeln = <fs_vfkp>-ebeln.
      <fs_nota>-xfrete_ebelp = <fs_vfkp>-ebelp.
      <fs_nota>-xfrete_lblni = <fs_vfkp>-lblni.
      <fs_nota>-xfrete_netwr = <fs_vfkp>-netwr.
      <fs_nota>-xfrete_knumv = <fs_vfkp>-knumv.
      <fs_nota>-xfrete_waers = <fs_vfkp>-waers.
      <fs_nota>-xfrete_budat = <fs_vfkp>-budat.

    endif.


  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CAB_NF_TER
*&---------------------------------------------------------------------*
form f_cab_nf_ter changing ct_nota type zibc_nfps_xnota.

  data(lt_nota_ter) = ct_nota.

  " retira as nf sap
  delete lt_nota_ter where terceiro = space.

  check lt_nota_ter is not initial.

  select * from j_1bbranch
    into table @data(lt_branch)
      for all entries in @lt_nota_ter
        where branch = @lt_nota_ter-werks.


*********************************************************************************
* Inicio seleção na tabela de depara / BUG SOLTO 118663 / AOENNING
*********************************************************************************
  select * from zsdt_depara_cen into table @data(lt_zsdt_depara_cen)
  for all entries in @lt_nota_ter
  where centrov_1 eq @lt_nota_ter-werks.
  if sy-subrc eq 0.
    select * from j_1bbranch
        into table @data(lt_branch_aux)
          for all entries in @lt_zsdt_depara_cen
            where branch = @lt_zsdt_depara_cen-centro_real.
  endif.
*********************************************************************************
* Fim seleção na tabela de depara / BUG SOLTO 118663 / AOENNING
*********************************************************************************


  select * from vfkp
    into table @data(lt_vfkp)
      for all entries in @lt_nota_ter
        where rebel = @lt_nota_ter-tknum.

  if sy-subrc eq 0.

    select * from vfsi
      into table @data(lt_vfsi)
      for all entries in @lt_vfkp
        where knumv = @lt_vfkp-knumv.

    select * from essr
      into table @data(lt_essr)
      for all entries in @lt_vfkp
        where lblni = @lt_vfkp-lblni.

  endif.

  loop at lt_nota_ter assigning field-symbol(<fs_terc>).

    read table ct_nota assigning field-symbol(<fs_nota>)
      with key docnum = <fs_terc>-docnum.

    check sy-subrc eq 0.

    read table lt_branch assigning field-symbol(<fs_branch>)
      with key branch = <fs_nota>-werks.

    if sy-subrc eq 0.
      <fs_nota>-bukrs = <fs_branch>-bukrs.
    else.
*********************************************************************************
* Inicio seleção na tabela de depara / BUG SOLTO 118663 / AOENNING
*********************************************************************************
      read table lt_zsdt_depara_cen assigning field-symbol(<fs_depara_cen>)
      with key centrov_1 = <fs_nota>-werks.
      if sy-subrc eq 0.

        read table lt_branch_aux assigning field-symbol(<fs_branch_aux>)
        with key branch = <fs_depara_cen>-centro_real.
        if sy-subrc eq 0.
          <fs_nota>-bukrs = <fs_branch_aux>-bukrs.
        endif.
      endif.
*********************************************************************************
* Fim seleção na tabela de depara / BUG SOLTO 118663 / AOENNING
*********************************************************************************
    endif.

    if <fs_nota>-partyp = 'C'.
      "<fs_nota>-parc_cliente = <fs_nota>-parc
    else.

      <fs_nota>-parc_forn = <fs_nota>-parc_cliente.
      "CLEAR <fs_nota>-parc_cliente.

    endif.

    if <fs_nota>-parc_forn is not initial.

      select single name1 regio from lfa1
        into (<fs_nota>-name1,<fs_nota>-reg_emissor)
          where lifnr = <fs_nota>-parc_forn.

    endif.

    if <fs_nota>-parc_cliente is not initial.

      select single name1 regio from lfa1
        into (<fs_nota>-name1,<fs_nota>-reg_dest)
          where kunnr = <fs_nota>-parc_cliente.

    endif.

    read table lt_vfkp assigning field-symbol(<fs_vfkp>)
      with key rebel = <fs_nota>-tknum.

    if sy-subrc eq 0.

      read table lt_vfsi assigning field-symbol(<fs_vfsi>)
        with key knumv = <fs_vfkp>-knumv.

      if sy-subrc eq 0.

        if <fs_vfsi>-kmein = 'TO'.
          <fs_nota>-xvlr_unit_fret = <fs_vfsi>-netpr / 1000.
        endif.

        if <fs_vfsi>-kmein = 'KG'.
          <fs_nota>-xvlr_unit_fret = <fs_vfsi>-netpr.
        endif.

        read table lt_essr assigning field-symbol(<fs_essr>)
          with key lblni = <fs_vfkp>-lblni.

        if sy-subrc eq 0.

          <fs_nota>-lwert = <fs_essr>-lwert.
          <fs_nota>-lfgja = <fs_essr>-budat(4).
          <fs_nota>-essr_budat = <fs_essr>-budat(4).

        endif.

      endif.

      <fs_nota>-xfrete_fknum = <fs_vfkp>-fknum.
      <fs_nota>-xfrete_ebeln = <fs_vfkp>-ebeln.
      <fs_nota>-xfrete_ebelp = <fs_vfkp>-ebelp.
      <fs_nota>-xfrete_lblni = <fs_vfkp>-lblni.
      <fs_nota>-xfrete_netwr = <fs_vfkp>-netwr.
      <fs_nota>-xfrete_knumv = <fs_vfkp>-knumv.
      <fs_nota>-xfrete_waers = <fs_vfkp>-waers.
      <fs_nota>-xfrete_budat = <fs_vfkp>-budat.

    endif.

*    READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
*      WITH KEY matnr = <fs_nota>-matnr.
*
*    IF sy-subrc EQ 0.
*
*      <fs_nota>-maktx = <fs_makt>-maktx.
*
*    ENDIF.
*
*    READ TABLE lt_0037 ASSIGNING FIELD-SYMBOL(<fs_0037>)
*      WITH KEY bukrs = <fs_nota>-bukrs
*               cd_modal = '01'
*               ck_servico = 'X'.
*
*    IF sy-subrc EQ 0.
*
*      <fs_nota>-matns = <fs_0037>-matnr.
*
*    ENDIF.


  endloop.



endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_ALV_01
*&---------------------------------------------------------------------*
form f_preenche_alv_globais.

  clear: gt_4000_global_01,
         gt_4000_global_02,
         gt_4000_alv_01,
         gt_4000_alv_02.

  check gt_xnota is not initial.

  loop at gt_xnota assigning field-symbol(<fs_xnota>).

    if <fs_xnota>-guid_header is not initial.

      if <fs_xnota>-guid_header = gs_nfse_4000-guid_header.
        append initial line to gt_4000_global_02 assigning field-symbol(<fs_alv>).
      else.
        append initial line to gt_4000_global_01 assigning <fs_alv>.
      endif.

    else.

      append initial line to gt_4000_global_01 assigning <fs_alv>.

    endif.

    perform f_xnota_to_alv
      using <fs_xnota>
   changing <fs_alv>.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ASSOCIAR_VT
*&---------------------------------------------------------------------*
form f_associar_vt .

  data lv_erro type flag.


  check go_alv_4000_01 is bound.

  "PERFORM f_check_associar_vt CHANGING lv_erro. "DEBUG

  check lv_erro is initial.

  call method go_alv_4000_01->get_selected_rows
    importing
      et_index_rows = data(lt_selected)
      et_row_no     = data(lt_rows).

  check lt_selected is not initial.

  loop at lt_selected assigning field-symbol(<fs_index>).

    read table gt_4000_alv_01 assigning field-symbol(<fs_alv_01>)
      index <fs_index>-index.

    check sy-subrc eq 0.

    " LOCALIZA A NF NA GLOBAL
    read table gt_xnota assigning field-symbol(<fs_xnota>)
      with key chave  = <fs_alv_01>-chave_nfe
               numero = <fs_alv_01>-nfenum.

    check sy-subrc eq 0.

    if <fs_xnota>-bukrs <> gs_nfse_4000-bukrs.

      perform f_put_mensagem using 'E' 'Empresa do Doc. Transporte  é diferente da empresa da NF'.
      continue.

    endif.

*** Alteracor por Alexandre R. Martins - CS1119845

***    v_dif = <fs_alv_01>-zvlr_vi - zibs_nfse_001-nfse_value.
***    IF v_dif < 0  .
***      v_dif = v_dif * -1.
***    ENDIF.
***
***    IF v_dif > 1.
***      MOVE v_dif TO c_dif.
***      CONCATENATE 'Valores entre a nota fical e documento de transporte divengentes e acima da tolerancia: '  c_dif INTO v_text.
***      PERFORM f_put_mensagem USING 'E' v_text.
***      CONTINUE.
***
***    ENDIF.

*** Fim alteracao  - 20-07-2023

    <fs_xnota>-guid_header = gs_nfse_4000-guid_header.

    <fs_xnota>-user_assoc = sy-uname.
    <fs_xnota>-data_assoc = sy-datum.
    <fs_xnota>-hora_assoc = sy-uzeit.
    <fs_xnota>-flag_assoc = 'X'.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
form f_preenche_data changing so_data type ranges_budat_tt.

  data(lv_ini) = sy-datum.
  data(lv_fim) = sy-datum.

  check so_data[] is initial.

  subtract 90 from lv_ini.

  append 'IBT' && lv_ini && lv_fim to so_data.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV_4000
*&---------------------------------------------------------------------*
form f_atualiza_alv_4000 .

  gt_4000_alv_01[] = gt_4000_global_01[].
  gt_4000_alv_02[]  = gt_4000_global_02[].

  perform f_alv_refresh_grid_4000 using 'X' 'X'.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_4000
*&---------------------------------------------------------------------*
form f_refresh_4000 .

  clear: ok_code_4000,
         go_cc_4000_01,
         go_alv_4000_01,
         go_cc_4000_02,
         go_alv_4000_02,
         gt_4000_alv_01,
         gt_4000_alv_02,
         gt_4000_global_01,
         gt_4000_global_02,
         gt_xnota,
         gs_nfse_4000,
         zibs_nfps_001.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FREE_ALV_4000
*&---------------------------------------------------------------------*
form f_free_alv_4000 .

  if go_alv_4000_01 is bound.

    clear gt_4000_alv_01.

    go_alv_4000_01->refresh_table_display( ).

    go_cc_4000_01->free( ).

  endif.

  if go_alv_4000_02 is bound.

    clear gt_4000_alv_02.

    go_alv_4000_02->refresh_table_display( ).

    go_cc_4000_02->free( ).

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_DEASSOCIAR_VT
*&---------------------------------------------------------------------*
form f_deassociar_vt .

  data lt_tknum type range of tknum.

  check go_alv_4000_01 is bound.

  call method go_alv_4000_02->get_selected_rows
    importing
      et_index_rows = data(lt_selected)
      et_row_no     = data(lt_rows).

  check lt_selected is not initial.

  loop at lt_selected assigning field-symbol(<fs_index>).

    read table gt_4000_alv_02 assigning field-symbol(<fs_alv_02>)
      index <fs_index>-index.

    check sy-subrc eq 0.

    if <fs_alv_02>-com_miro is not initial.

      perform f_put_mensagem
        using 'E' 'Impossivel desassociar com miro gerada'.

      exit.

    endif.

    append initial line to lt_tknum assigning field-symbol(<fs_tknum>).

    <fs_tknum>-sign = 'I'.
    <fs_tknum>-option = 'EQ'.
    <fs_tknum>-low = <fs_alv_02>-tknum.

    " LOCALIZA A NF NA GLOBAL
    read table gt_xnota assigning field-symbol(<fs_xnota>)
      with key chave  = <fs_alv_02>-chave_nfe
               numero = <fs_alv_02>-nfenum.

    check sy-subrc eq 0.

    clear <fs_xnota>-guid_header.
    clear <fs_xnota>-user_assoc.
    clear <fs_xnota>-data_assoc.
    clear <fs_xnota>-hora_assoc.
    clear <fs_xnota>-flag_assoc.

  endloop.

  check lt_tknum[] is not initial.

  delete from zlest0034 where tknum in lt_tknum.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ASSOCIAR_VT_TELA
*&---------------------------------------------------------------------*
form f_associar_vt_tela .

  zibs_nfse_001-zbvtyp = zibt_nfse_001-zbvtyp.

  call function 'ZMM_NFSE_ASSOCIAR_VT'
    exporting
      is_nfse_001 = zibs_nfse_001.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_TOTAIS_4000
*&---------------------------------------------------------------------*
form f_atualiza_totais_4000 .

  clear: zibs_nfps_001-max_tol,
         zibs_nfps_001-nfse_total,
         zibs_nfps_001-vi_total,
         zibs_nfps_001-perda_total,
         zibs_nfps_001-quebra_total,
         zibs_nfps_001-iss_total,
         zibs_nfps_001-total_liq.

  zibs_nfps_001-max_tol = gv_total_tolerance.

  zibs_nfps_001-nfse_total = gs_nfse_4000-nfse_value.

  zibs_nfps_001-iss_total = value #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value default '0.00' ).

  loop at gt_4000_global_02 assigning field-symbol(<fs_alv_02>). " 28.04.2023 - 110959
    "LOOP AT gt_4000_alv_02 ASSIGNING FIELD-SYMBOL(<fs_alv_02>).

    add <fs_alv_02>-zvlr_vi to zibs_nfps_001-vi_total.
    add <fs_alv_02>-zvlr_perda to zibs_nfps_001-perda_total.
    add <fs_alv_02>-zvlr_quebra to zibs_nfps_001-quebra_total.
    "ADD <fs_alv_02>-iss_total TO zibs_nfps_001-iss_total.

  endloop.

  " 28.04.2023 - 110959 -->
  if not ( gs_nfse_4000-nao_reter_iss = space ).
    clear zibs_nfps_001-iss_total.
  endif.
  " 28.04.2023 - 110959 --<

  zibs_nfps_001-total_liq = gs_nfse_4000-nfse_value - ( zibs_nfps_001-perda_total + zibs_nfps_001-quebra_total + zibs_nfps_001-iss_total ).

  if  zibs_nfps_001-total_liq < 0.
    zibs_nfps_001-total_liq =  zibs_nfps_001-total_liq * -1.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOX_EVENT
*&---------------------------------------------------------------------*
form f_check_box_event .

  "CHECK gt_4000_global_01 IS NOT INITIAL.

  gt_4000_alv_01 = gt_4000_global_01.
  gt_4000_alv_02 = gt_4000_global_02.

  case 'X'.

    when zibs_nfps_001-nao_associ.

      delete gt_4000_alv_01 where ja_associ = 'X'.
      delete gt_4000_alv_01 where com_miro = 'X'.

      " SEGUNDO ALV -------
      delete gt_4000_alv_02 where ja_associ = 'X'.
      delete gt_4000_alv_02 where com_miro = 'X'.

    when zibs_nfps_001-ja_associ.

      delete gt_4000_alv_01 where ja_associ = space.
      delete gt_4000_alv_01 where com_miro = 'X'.

      " SEGUNDO ALV -------
      delete gt_4000_alv_02 where ja_associ = space.
      delete gt_4000_alv_02 where com_miro = 'X'.

    when zibs_nfps_001-com_miro.

      delete gt_4000_alv_01 where com_miro = space.
      delete gt_4000_alv_02 where com_miro = space.

    when zibs_nfps_001-todas.

  endcase.

  perform f_alv_refresh_grid_4000 using 'X' 'X'.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_GET_OBS_XML
*&---------------------------------------------------------------------*
form f_get_obs_xml using p_guid_header type /tcsr/e_guid_header.

  constants: lc_file_type type char10 value 'BIN'.

  data xml_table type table of smum_xmltb.
  data return type table of bapiret2.

  data: lv_xml_content     type xstring,
        lv_string          type xstring,
        lv_size            type i,
        lv_selected_folder type string,
        lv_file_name       type string,
        lt_xml_tab         type dcxmllines,
        lo_dom             type ref to if_ixml_document.

  data lo_xml type ref to cl_xml_document.

  clear gt_2000_text.

  select single xmlstring
    from /tcsr/t_xml
      into @data(xml_content)
        where guid_header = @p_guid_header.

  check sy-subrc eq 0.

  move xml_content to lv_xml_content.

  check lv_xml_content is not initial.

  call function 'SDIXML_XML_TO_DOM'
    exporting
      xml           = lv_xml_content
    importing
      document      = lo_dom
    exceptions
      invalid_input = 1
      others        = 2.
  if sy-subrc ne 0.
    clear: lo_dom.
  endif.

  refresh: lt_xml_tab[].

  " Convert DOM to XML doc (table)
  call function 'SDIXML_DOM_TO_XML'
    exporting
      document      = lo_dom
      pretty_print  = ' '
    importing
      xml_as_string = lv_string
      size          = lv_size
    tables
      xml_as_table  = lt_xml_tab
    exceptions
      no_document   = 1
      others        = 2.

  call function 'SMUM_XML_PARSE'
    exporting
      xml_input = lv_string
    tables
      xml_table = xml_table
      return    = return.

  data lv_texto(4000).
  data lt_trtexts type trtexts.


  loop at xml_table assigning field-symbol(<fs_xml>) where cname = 'Discriminacao'.

    lv_texto = <fs_xml>-cvalue.

    clear lt_trtexts.

*** Exclusão - Stefanini - IR215558 - Kenya Braga - 08.01.2025 - Início
*    call function 'TR_SPLIT_TEXT'
*      exporting
*        iv_text  = lv_texto
*        iv_len   = 70
*      importing
*        et_lines = lt_trtexts.
*** Exclusão - Stefanini - IR215558 - Kenya Braga - 08.01.2025 - Fim

*** Inclusão - Stefanini - IR215558 - Kenya Braga - 08.01.2025 - Início
    call function 'RKD_WORD_WRAP'
      exporting
        textline            = lv_texto
        outputlen           = 70
      tables
        out_lines           = lt_trtexts
      exceptions
        outputlen_too_large = 1
        others              = 2.
    if sy-subrc eq 0.
*** Inclusão - Stefanini - IR215558 - Kenya Braga - 08.01.2025 - Fim

      loop at lt_trtexts assigning field-symbol(<fs_text>).

        append initial line to gt_2000_text assigning field-symbol(<fs_split>).

        <fs_split>-hier = <fs_xml>-hier.
        <fs_split>-type = <fs_xml>-type.
        <fs_split>-cname = <fs_xml>-cname.
        <fs_split>-cvalue = <fs_text>.

        perform f_remov_spec_char
          using <fs_text>
       changing <fs_split>-cvalue.

      endloop.
    endif.   " + Stefanini - IR215558 - Kenya Braga - 08.01.2025
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_ASSOCIAR_VT
*&---------------------------------------------------------------------*
form f_check_associar_vt changing p_erro type c.

  data lv_soma type /tcsr/e_nfse_value.
  data lv_werks type werks_d.
  data lv_difer type flag.

  call method go_alv_4000_01->get_selected_rows
    importing
      et_index_rows = data(lt_selected)
      et_row_no     = data(lt_rows).

  if lt_selected is initial.
    p_erro = 'X'.
    exit.
  endif.

  loop at lt_selected assigning field-symbol(<fs_index>).

    read table gt_4000_alv_01 assigning field-symbol(<fs_alv_01>)
      index <fs_index>-index.

    check sy-subrc eq 0.

    " LOCALIZA A NF NA GLOBAL
    read table gt_xnota assigning field-symbol(<fs_xnota>)
      with key chave  = <fs_alv_01>-chave_nfe
               numero = <fs_alv_01>-nfenum.

    check sy-subrc eq 0.

    " não permitir vincular linhas de empresas diferentes
    if gs_nfse_4000-bukrs <> <fs_xnota>-bukrs.
      perform f_put_mensagem using 'E' 'Há linhas com empresa diferente da NFS-e'.
      p_erro = 'X'.
      exit.
    endif.

    " não permitir vincular linhas que não possua peso de descarga
    if <fs_xnota>-peso_descarga is initial.
      perform f_put_mensagem using 'E' 'Há linhas sem peso de descarga'.
      p_erro = 'X'.
      exit.
    endif.

    " não permitir vincular linhas de centros diferentes
    if <fs_xnota>-werks <> lv_werks and lv_werks is not initial.

      perform f_put_mensagem using 'E' 'Há linhas com centros diferentes'.
      p_erro = 'X'.
      exit.

    endif.

    " a soma dos valores das vi (dos documentos selecionados) deve ser igual ao valor da nfps
    add <fs_xnota>-xvlr_liq_vi to lv_soma.

    lv_werks = <fs_xnota>-werks.

  endloop.

  check p_erro is initial.

  if lv_soma <> gs_nfse_4000-nfse_value.

    perform f_put_mensagem using 'E' 'A soma das linhas não batem com o valor na NFS-e'.
    p_erro = 'X'.
    exit.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_4000
*&---------------------------------------------------------------------*
form f_save_4000 .

  data lt_0034 type table of zlest0034.

  data lv_guid type /tcsr/e_guid_header.

  data v_dif   type j_1bnfdoc-nftot.
  data v_text  type string.
  data c_dif   type string.

  go_alv_4000_02->check_changed_data( ).

  check gt_xnota is not initial.

*** Alteracor por Alexandre R. Martins - CS1119845

  v_dif = zibs_nfps_001-nfse_total - zibs_nfps_001-vi_total.
  if v_dif < 0  .
    v_dif = v_dif * -1.
  endif.

  if v_dif > 1.
    move v_dif to c_dif.
    concatenate 'Valores entre a nota fical e documento de transporte divengentes e acima da tolerancia: '  c_dif into v_text.
    perform f_put_mensagem using 'E' v_text.
    return.

  endif.

*** Fim alteracao  - 27-07-2023




  select * from zlest0034
    into table lt_0034
      for all entries in gt_xnota
        where tknum = gt_xnota-tknum.

  loop at gt_xnota assigning field-symbol(<fs_xnota>).

    read table gt_4000_alv_02 assigning field-symbol(<fs_alv_02>)
      with key tknum = <fs_xnota>-tknum
           ja_associ = 'X'.

    check sy-subrc eq 0.

    " somente se o peso for maior que zero.
    check <fs_alv_02>-zpeso_destino > 0.

    read table lt_0034 assigning field-symbol(<fs_0034>)
      with key tknum = <fs_xnota>-tknum.

    if sy-subrc eq 0.

      <fs_0034>-tknum = <fs_xnota>-tknum.
      <fs_0034>-fknum = <fs_xnota>-xfrete_fknum.
      <fs_0034>-bukrs = <fs_xnota>-bukrs.
      <fs_0034>-werks = <fs_xnota>-werks.
      <fs_0034>-waers = <fs_xnota>-xfrete_waers.
      <fs_0034>-kbetr = space.
      <fs_0034>-kurst = space.
      <fs_0034>-ebeln = <fs_xnota>-xfrete_ebeln.
      <fs_0034>-ebelp = <fs_xnota>-xfrete_ebelp.
      <fs_0034>-lblni = <fs_xnota>-xfrete_lblni.
      <fs_0034>-lfgja = <fs_xnota>-lfgja.
      <fs_0034>-add03 = <fs_xnota>-add03.
      <fs_0034>-belnr = space.
      <fs_0034>-gjahr = space.
      <fs_0034>-tdlnr = <fs_xnota>-tdlnr.
      <fs_0034>-shtyp = <fs_xnota>-shtyp.
      <fs_0034>-lifnr = <fs_xnota>-tdlnr.
      <fs_0034>-nfenum = <fs_xnota>-numero.
      <fs_0034>-series = <fs_xnota>-serie.
      <fs_0034>-zdt_mov = sy-datum.
      "<fs_0034>-zdt_vencto = <fs_alv_02>-zdt_chegada.

      "<fs_0034>-zdt_vencto = <fs_alv_02>-zdt_chegada. """"
      "<fs_0034>-zpeso_origem = <fs_alv_02>-zpeso_destino.""""

      <fs_0034>-zpeso_destino = <fs_alv_02>-zpeso_destino.
      <fs_0034>-zdt_chegada = <fs_alv_02>-zdt_chegada.

      perform f_nr_conhecimento
        using gs_nfse_4000-nfse_numero
     changing <fs_0034>-nr_conhec.

      "<fs_0034>-nr_conhec = gs_nfse_4000-nfse_numero.
      <fs_0034>-zdt_conhec = gs_nfse_4000-dtemissao.
      "<fs_0034>-zdt_chegada = <fs_xnota>-data_descarga.
      <fs_0034>-zpeso_origem = <fs_xnota>-zpeso_origem.
      <fs_0034>-kalsm = 'TAXBRA'.
      <fs_0034>-iva = 'S1'.
      <fs_0034>-nfe = <fs_xnota>-numero.
      <fs_0034>-gewei = <fs_xnota>-meins.
      <fs_0034>-dmbtr = <fs_xnota>-xfrete_netwr.
      <fs_0034>-dmbtr_doc = <fs_xnota>-xfrete_netwr.
      "<fs_0034>-zpeso_destino = <fs_xnota>-peso_descarga.
      <fs_0034>-zpeso_diferenca = <fs_xnota>-zpeso_diferenca.
      <fs_0034>-zquebra = <fs_xnota>-xfrete_quebra.
      <fs_0034>-zperda = <fs_xnota>-xfrete_perda.
      <fs_0034>-zvlr_quebra = <fs_xnota>-xvlr_quebra.
      <fs_0034>-zvlr_perda = <fs_xnota>-xfrete_vlr_perda.
      <fs_0034>-zvlr_liq_pagar = <fs_xnota>-xvlr_liq_nfs. "<fs_xnota>-xvlr_liq_vi . "
      <fs_0034>-matnr = <fs_xnota>-matnr.
      <fs_0034>-matns = <fs_xnota>-matns.
      <fs_0034>-bvtyp = zspayment_data_nfse_inbound-bvtyp.
      "<fs_0034>-re_belnr = <fs_xnota>-re_belnr. """""
      "<fs_0034>-re_gjahr = <fs_xnota>-re_gjahr. """"""""
      <fs_0034>-re_item = 0.
*      <fs_0034>-en_docnum = gs_nfse_4000-docnum. "Comentado.
      <fs_0034>-regio_emissor = <fs_xnota>-reg_emissor.
      <fs_0034>-regio_receptor = <fs_xnota>-reg_dest.
      <fs_0034>-base_icms = 0.
      <fs_0034>-base_pis = <fs_xnota>-base_pis.
      <fs_0034>-base_cofins = <fs_xnota>-base_cofins.
      <fs_0034>-rate_icms = 0.
      <fs_0034>-rate_pis = <fs_xnota>-rate_pis.
      <fs_0034>-rate_cofins = <fs_xnota>-rate_cofins.
      <fs_0034>-valor_icms = 0.
      <fs_0034>-valor_pis = <fs_xnota>-xvlr_pis.
      <fs_0034>-valor_cofins = <fs_xnota>-xvlr_cofins.
      <fs_0034>-valor_pedagio = 0.
      <fs_0034>-docnum = <fs_xnota>-docnum.
      <fs_0034>-valor_mercadoria = <fs_xnota>-netwr.
      <fs_0034>-multimodal = space.
      <fs_0034>-txjcd_emissor = space.

      <fs_0034>-vttk_vsart = <fs_xnota>-vsart.
      <fs_0034>-essr_budat = <fs_xnota>-essr_budat.

      <fs_0034>-guid_header = <fs_xnota>-guid_header.
      <fs_0034>-user_reg = <fs_xnota>-user_assoc.
      <fs_0034>-data_reg = <fs_xnota>-data_assoc.
      <fs_0034>-hora_reg = <fs_xnota>-hora_assoc.

    else.

      append initial line to lt_0034 assigning <fs_0034>.

      <fs_0034>-tknum = <fs_xnota>-tknum.
      <fs_0034>-fknum = <fs_xnota>-xfrete_fknum.
      <fs_0034>-bukrs = <fs_xnota>-bukrs.
      <fs_0034>-werks = <fs_xnota>-werks.
      <fs_0034>-waers = <fs_xnota>-xfrete_waers.
      <fs_0034>-kbetr = space.
      <fs_0034>-kurst = space.
      <fs_0034>-ebeln = <fs_xnota>-xfrete_ebeln.
      <fs_0034>-ebelp = <fs_xnota>-xfrete_ebelp.
      <fs_0034>-lblni = <fs_xnota>-xfrete_lblni.
      <fs_0034>-lfgja = <fs_xnota>-lfgja.
      <fs_0034>-add03 = <fs_xnota>-add03.
      <fs_0034>-belnr = <fs_xnota>-belnr.
      <fs_0034>-gjahr = <fs_xnota>-gjahr.
      <fs_0034>-tdlnr = <fs_xnota>-tdlnr.
      <fs_0034>-shtyp = <fs_xnota>-shtyp.
      <fs_0034>-lifnr = <fs_xnota>-tdlnr.
      <fs_0034>-nfenum = <fs_xnota>-numero.
      <fs_0034>-series = <fs_xnota>-serie.
      <fs_0034>-zdt_mov = sy-datum.

      <fs_0034>-zdt_vencto = <fs_alv_02>-zdt_chegada. """"
      <fs_0034>-zpeso_origem = <fs_alv_02>-zpeso_origem.

      perform f_nr_conhecimento
        using gs_nfse_4000-nfse_numero
     changing <fs_0034>-nr_conhec.

      "<fs_0034>-nr_conhec = gs_nfse_4000-nfse_numero.
      <fs_0034>-zdt_conhec = gs_nfse_4000-dtemissao.

      "<fs_0034>-zdt_chegada = <fs_xnota>-data_descarga.
      <fs_0034>-zpeso_destino = <fs_alv_02>-zpeso_destino.
      <fs_0034>-zdt_chegada = <fs_alv_02>-zdt_chegada.

      <fs_0034>-kalsm = 'TAXBRA'.
      <fs_0034>-iva = 'S1'.
      <fs_0034>-nfe = <fs_xnota>-numero.
      <fs_0034>-gewei = <fs_xnota>-meins.
      <fs_0034>-dmbtr = <fs_xnota>-xfrete_netwr.
      <fs_0034>-dmbtr_doc = <fs_xnota>-xfrete_netwr.
      "<fs_0034>-zpeso_destino = <fs_xnota>-peso_descarga.
      <fs_0034>-zpeso_diferenca = <fs_xnota>-zpeso_diferenca.
      <fs_0034>-zquebra = <fs_xnota>-xfrete_quebra.
      <fs_0034>-zperda = <fs_xnota>-xfrete_perda.
      <fs_0034>-zvlr_quebra = <fs_xnota>-xvlr_quebra.
      <fs_0034>-zvlr_perda = <fs_xnota>-xfrete_vlr_perda.
      <fs_0034>-zvlr_liq_pagar = <fs_xnota>-xvlr_liq_nfs. "<fs_xnota>-xvlr_liq_vi .
      <fs_0034>-matnr = <fs_xnota>-matnr.
      <fs_0034>-matns = <fs_xnota>-matns.
      <fs_0034>-bvtyp = zspayment_data_nfse_inbound-bvtyp.
*      <fs_0034>-re_belnr = <fs_xnota>-re_belnr. """"" "Comentado.
*      <fs_0034>-re_gjahr = <fs_xnota>-re_gjahr. """""""" "Comentado.
      <fs_0034>-re_item = 0.
*      <fs_0034>-en_docnum = gs_nfse_4000-docnum. "Comentado.
      <fs_0034>-regio_emissor = <fs_xnota>-reg_emissor.
      <fs_0034>-regio_receptor = <fs_xnota>-reg_dest.
      <fs_0034>-base_icms = 0.
      <fs_0034>-base_pis = <fs_xnota>-base_pis.
      <fs_0034>-base_cofins = <fs_xnota>-base_cofins.
      <fs_0034>-rate_icms = 0.
      <fs_0034>-rate_pis = <fs_xnota>-rate_pis.
      <fs_0034>-rate_cofins = <fs_xnota>-rate_cofins.
      <fs_0034>-valor_icms = 0.
      <fs_0034>-valor_pis = <fs_xnota>-xvlr_pis.
      <fs_0034>-valor_cofins = <fs_xnota>-xvlr_cofins.
      <fs_0034>-valor_pedagio = 0.
      <fs_0034>-docnum = <fs_xnota>-docnum.
      <fs_0034>-valor_mercadoria = <fs_xnota>-netwr.
      <fs_0034>-multimodal = space.
      <fs_0034>-txjcd_emissor = space.
      <fs_0034>-guid_header = <fs_xnota>-guid_header.
      <fs_0034>-user_reg = <fs_xnota>-user_assoc.
      <fs_0034>-data_reg = <fs_xnota>-data_assoc.
      <fs_0034>-hora_reg = <fs_xnota>-hora_assoc.

    endif.

  endloop.

  delete lt_0034 where guid_header is initial.

  if lt_0034 is not initial.

    modify zlest0034 from table lt_0034.

    commit work and wait.

    perform f_put_mensagem using 'S' 'Documentos gravados'.

  else.

    perform f_put_mensagem using 'W' 'Sem alteração'.

  endif.

  perform f_edita_4000_alv using space.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
form f_fieldcat_modi using p_fieldname type slis_fieldname
                           p_column type c
                           p_value type any
                  changing p_field_cat type lvc_t_fcat.

  read table p_field_cat assigning field-symbol(<fs_fcat>)
    with key fieldname = p_fieldname.

  check sy-subrc eq 0.

  data(lv_name) = '<FS_FCAT>-' && p_column.

  assign (lv_name) to field-symbol(<fs_colum>).

  check sy-subrc eq 0.

  <fs_colum> = p_value.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_ITENS_ASSOCI
*&---------------------------------------------------------------------*
form f_selec_itens_associ using p_guid type /tcsr/e_guid_header
                       changing ct_0034 type zlesc0034.

  clear ct_0034.

  check p_guid is not initial.

  read table gt_xnota transporting no fields
    with key guid_header = p_guid.

  if sy-subrc eq 0.

    loop at gt_xnota assigning field-symbol(<fs_xnote>)
         where guid_header = p_guid.

      "CHECK <fs_xnote>-re_belnr IS INITIAL.

      append initial line to ct_0034 assigning field-symbol(<fs_0034>).

      <fs_0034>-bukrs = <fs_xnote>-bukrs.
      <fs_0034>-werks = <fs_xnote>-werks.
      <fs_0034>-nfenum = <fs_xnote>-numero.
      <fs_0034>-tknum = <fs_xnote>-tknum.
      <fs_0034>-fknum = <fs_xnote>-xfrete_fknum.
      "<fs_0034>-peso  = <fs_xnote>-peso_descarga.
      <fs_0034>-zpeso_destino = <fs_xnote>-peso_descarga.
      <fs_0034>-zdt_chegada = <fs_xnote>-data_descarga.
      <fs_0034>-matnr = <fs_xnote>-matnr.

      <fs_0034>-dmbtr = <fs_xnote>-xfrete_netwr.
      <fs_0034>-iva = <fs_xnote>-mwskz.
      <fs_0034>-zquebra = <fs_xnote>-xvlr_quebra.
      <fs_0034>-zperda = <fs_xnote>-xfrete_vlr_perda.
      <fs_0034>-zvlr_quebra = <fs_xnote>-xfrete_quebra.
      <fs_0034>-zvlr_perda = <fs_xnote>-xfrete_perda.
      <fs_0034>-regio_emissor = <fs_xnote>-reg_emissor.
      <fs_0034>-regio_receptor = <fs_xnote>-reg_dest.
      <fs_0034>-valor_pis = <fs_xnote>-xvlr_pis.
      <fs_0034>-valor_cofins = <fs_xnote>-xvlr_cofins.

      <fs_0034>-docnum = <fs_xnote>-docnum.
      <fs_0034>-valor_mercadoria = <fs_xnote>-netwr.
      <fs_0034>-ebeln = <fs_xnote>-xfrete_ebeln.
      <fs_0034>-ebelp = <fs_xnote>-xfrete_ebelp.
      <fs_0034>-lblni = <fs_xnote>-xfrete_lblni.
      <fs_0034>-guid_header = <fs_xnote>-guid_header.

      <fs_0034>-re_belnr = <fs_xnote>-re_belnr.
      <fs_0034>-re_gjahr = <fs_xnote>-re_gjahr.


    endloop.

  else.

    select * from zlest0034
      into table ct_0034
        where guid_header = p_guid
          and re_belnr = space.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SET_STATUS_2000
*&---------------------------------------------------------------------*
form f_set_status_2000 .

  data lt_exclude type table of sy-ucomm.

  " SE NAO TEM EBELN, OCULTA TODOS OS BOTOES
  if go_znfse->get_nfse_001( )-ebeln is initial.

    append 'ML81N' to lt_exclude.
    append 'MIGO' to lt_exclude.
*    APPEND 'MIRO' TO lt_exclude.
  else.

    " SE SÓ TEM SERVIÇO, ENTÃO NAO MOSTRA BOTAO MIGO
    if go_znfse->only_serv_po( ) = 'X'.

      append 'MIGO' to lt_exclude.

      " SE NAO TEM SÓ SERVICO, MOSTRA BOTAO MIGO
    elseif go_znfse->only_mat_po( ) = 'X'.

      append 'ML81N' to lt_exclude.

    endif.

  endif.
  "break rblima.
  authority-check object 'ZACTNFPS' id 'ZACTNFPS' field '01'.

  if sy-subrc <> 0.
    append 'ASSOCIARVT' to lt_exclude.
  endif.

  authority-check object 'ZACTNFPS' id 'ZACTNFPS' field '02'.

  if sy-subrc <> 0.
    append 'MIRO_FRETE' to lt_exclude.
  endif.

  authority-check object 'ZACTNFPS' id 'ZACTNFPS' field '03'.

  if sy-subrc <> 0.
    append 'MIRO_ESTOR' to lt_exclude.
  endif.

  set pf-status '2000' excluding lt_exclude.

  set titlebar '2000'.

  perform f_dados_banco_tela
    using zsheader_data_nfse_inbound
 changing zspayment_data_nfse_inbound.

  if zspayment_data_nfse_inbound-zlspr is not initial.

    select single textl from t008t
      into zspayment_data_nfse_inbound-textl
        where spras = sy-langu
          and zahls = zspayment_data_nfse_inbound-zlspr.

  else.

    clear zspayment_data_nfse_inbound-textl.

  endif.

  if zspayment_data_nfse_inbound-pymt_meth is not initial.
    select single text1 into t042z-text1
      from t042z
     where land1 eq 'BR'
      and  zlsch eq zspayment_data_nfse_inbound-pymt_meth.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ON_DATA_CHANGED
*&---------------------------------------------------------------------*
form f_on_data_changed using io_data type ref to cl_alv_changed_data_protocol.

  data lo_type type ref to cl_abap_datadescr.

  data lv_field type c length 40.

  loop at io_data->mt_mod_cells assigning field-symbol(<fs_mod_cells>).

    read table gt_4000_alv_02 assigning field-symbol(<fs_alv>)
     index <fs_mod_cells>-row_id.

    check sy-subrc eq 0.

    lv_field = '<FS_ALV>-' && <fs_mod_cells>-fieldname.

    assign (lv_field) to field-symbol(<fs_field>).

    check <fs_field> is assigned.

    lo_type ?= cl_abap_typedescr=>describe_by_data( <fs_field> ).

    data(lv_value) = <fs_mod_cells>-value.

    case lo_type->type_kind.

      when 'P'.

        replace ',' in lv_value with ''.
        replace '.' in lv_value with ''.

        lv_value = lv_value && '.000'.

      when 'D'.

        call function 'CONVERSION_EXIT_IDATE_INPUT'
          exporting
            input  = <fs_mod_cells>-value
          importing
            output = lv_value.

      when others.
    endcase.

    <fs_field> = lv_value.

    read table gt_xnota assigning field-symbol(<fs_xnota>)
      with key numero = <fs_alv>-nfenum.

    check sy-subrc eq 0.

    <fs_xnota>-peso_descarga = <fs_alv>-zpeso_destino.
    <fs_xnota>-data_descarga = <fs_alv>-zdt_chegada.

    perform f_calculo_linha changing <fs_xnota>.

    perform f_xnota_to_alv
      using <fs_xnota>
   changing <fs_alv>.

  endloop.

  perform f_alv_refresh_grid_4000 using space 'X'.
  perform f_atualiza_totais_4000.

endform.
*&---------------------------------------------------------------------*
*& Form F_PREENCHE_X
*&---------------------------------------------------------------------*
form f_preenche_x using iv_struct type any
               changing cv_structx type any.

  data lo_linetype type ref to cl_abap_structdescr.

  assign ('IV_STRUCT') to field-symbol(<lf_data>).

  check sy-subrc eq 0.

  lo_linetype ?= cl_abap_typedescr=>describe_by_data( <lf_data> ).

  loop at lo_linetype->components assigning field-symbol(<lf_component>).

    data(lv_campo) = 'IV_STRUCT-' && <lf_component>-name.
    data(lv_campox) = 'CV_STRUCTX-' && <lf_component>-name.

    assign (lv_campo) to field-symbol(<lf_campo>).

    check sy-subrc eq 0.

    assign (lv_campox) to field-symbol(<lf_campox>).

    if <lf_campo> is not initial.
      <lf_campox> = 'X'.
    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EDITA_4000_ALV
*&---------------------------------------------------------------------*
form f_edita_4000_alv using p_edit type flag.

  data lt_fieldcat type lvc_t_fcat.

  if go_alv_4000_02 is bound.

    perform f_fieldcat_4000_alv_02
      using p_edit
   changing lt_fieldcat.

    call method go_alv_4000_02->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = lt_fieldcat.

    perform f_alv_refresh_grid_4000 using space 'X'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_4000_ALV_02
*&---------------------------------------------------------------------*
form f_fieldcat_4000_alv_02 using p_edit type c
                         changing p_field_cat type lvc_t_fcat.

  perform f_monta_fieldcat using 'ZIBS_NFPS_001' changing p_field_cat.

  delete p_field_cat where fieldname = 'SELEC'.
  delete p_field_cat where fieldname = 'DATBG'.
  delete p_field_cat where fieldname = 'CHAVE_NFE'.
  delete p_field_cat where fieldname = 'DOCDAT'.
  delete p_field_cat where fieldname = 'LIFNR'.
  delete p_field_cat where fieldname = 'NAO_ASSOCI'.
  delete p_field_cat where fieldname = 'JA_ASSOCI'.
  delete p_field_cat where fieldname = 'COM_MIRO'.
  delete p_field_cat where fieldname = 'TODAS'.

  delete p_field_cat where fieldname = 'MAX_TOL'.
  delete p_field_cat where fieldname = 'NFSE_TOTAL'.
  delete p_field_cat where fieldname = 'VI_TOTAL'.
  delete p_field_cat where fieldname = 'PERDA_TOTAL'.
  delete p_field_cat where fieldname = 'QUEBRA_TOTAL'.
  delete p_field_cat where fieldname = 'ISS_TOTAL'.
  delete p_field_cat where fieldname = 'TOTAL_LIQ'.
  delete p_field_cat where fieldname = 'ESSR_BUDAT'.

  perform f_coluna_edita2 using 'REGIO_EMISSOR'  'UF.Org' 'UF.Org' changing p_field_cat.
  perform f_coluna_edita2 using 'REGIO_RECEPTOR'  'UF.Des' 'UF.Des' changing p_field_cat.
  perform f_coluna_edita2 using 'VALOR_PIS'  'PIS' 'Vlr PIS' changing p_field_cat.
  perform f_coluna_edita2 using 'VALOR_COFINS'  'COFINS' 'Vlr COFINS' changing p_field_cat.
  perform f_coluna_edita2 using 'PARC_CLIENTE' 'Parc.NF' 'Parceiro NF' changing p_field_cat.
  perform f_coluna_edita2 using 'NAME1' 'DescPar' 'Parceiro' changing p_field_cat.

  perform f_coluna_edita2 using 'ZVLR_VI' 'Vlr.VI' 'Vlr.VI' changing p_field_cat.
  perform f_coluna_edita2 using 'ZPERDA' 'Perda' 'Perda' changing p_field_cat.

  perform f_coluna_edita2 using 'RE_BELNR' 'Doc.Miro' 'Doc.Miro' changing p_field_cat.
  perform f_coluna_edita2 using 'RE_GJAHR' 'Ano.Miro' 'Ano.Miro' changing p_field_cat.

  if p_edit = 'X'.

    perform f_fieldcat_modi using 'ZPESO_DESTINO' 'EDIT' 'X' changing p_field_cat.
    perform f_fieldcat_modi using 'ZDT_CHEGADA' 'EDIT' 'X' changing p_field_cat.

  else.

    perform f_fieldcat_modi using 'ZPESO_DESTINO' 'EDIT' space changing p_field_cat.
    perform f_fieldcat_modi using 'ZDT_CHEGADA' 'EDIT' space changing p_field_cat.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_MIRO_FRETE
*&---------------------------------------------------------------------*
form f_miro_frete .

  data lv_ret type c.
  data lv_erro type c.

  perform f_data_vencimento changing zspayment_data_nfse_inbound lv_erro.

  check lv_erro is initial.

  perform f_popup_to_confirm
    using 'Confirmar miro frete?'
 changing lv_ret.

  check lv_ret = '1'.

  check go_znfse is bound.

  " 31.03.2023 - 107971 - RBL ---------

*****     Inicio - ALX
**
**  SELECT SINGLE retem_iss
**    FROM zibt_nfse_001
**    INTO @DATA(lv_iss_aux)
**    WHERE nfse_numero = @zsheader_data_nfse_inbound-nfse_numero.
**
**  IF lv_iss_aux = gv_iss_2000.
**
**    MESSAGE e024(sd) WITH 'Necessário Salvar antes de gerar Miro Frete.'.
**
**    EXIT.
**
**  ENDIF.
**
**  CALL METHOD go_znfse->set_value_iss(
**    EXPORTING
**      i_ind_iss = gv_iss_2000 )->miro_frete( ).
**
*****     Fim - ALX

  " se criou desabilita tela
  if go_znfse->miro_frete( iv_retem_iss = zsheader_data_nfse_inbound-reter_iss ) = 0.
    gv_edit_2000 = space.
  endif.

  " 31.03.2023 - 107971 - RBL -------<

  go_znfse->show_message( ).

  zibt_nfse_001 = go_znfse->get_nfse_001( ).

  zibs_nfse_001-belnr_fret = zibt_nfse_001-belnr_fret.

endform.
*&---------------------------------------------------------------------*
*&      Form  MIRO_ESTORNO
*&---------------------------------------------------------------------*
form miro_estorno .

  data lv_ret type c.

  perform f_popup_to_confirm
    using 'Confirmar estorno miro frete?'
 changing lv_ret.

  check lv_ret = '1'.

  check go_znfse is bound.

  if go_znfse->miro_frete_estorno( ) = 0.

    zibt_nfse_001 = go_znfse->get_nfse_001( ).

    if zibt_nfse_001-belnr_fret is initial.
      zibs_nfse_001-belnr_fret = space.
      gv_edit_2000 = 'P'.
    endif.

  endif.

  go_znfse->show_message( ).

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_COMPENSACAO
*&---------------------------------------------------------------------*
form f_gera_compensacao using p_bukrs type bukrs
                              p_lifnr type lifnr
                              p_belnr_in type belnr_d
                              p_gjahr_in type gjahr
                              p_belnr_out type belnr_d
                              p_gjahr_out type gjahr
                     changing p_erro type c.

  data: wa_bkpf type bkpf.

  call function 'Z_FI_GL_COMPENSA_ESTORNO_MIRO'
    exporting
      e_bukrs       = p_bukrs
      e_lifnr       = p_lifnr
      e_invoice_in  = p_belnr_in
      e_year_in     = p_gjahr_in
      e_invoice_out = p_belnr_out
      e_year_out    = p_gjahr_out
    importing
      i_bkpf        = wa_bkpf
    exceptions
      nao_compensou = 1
      erro_bloqueio = 2
      sem_acesso    = 3
      others        = 4.

  if sy-subrc is not initial.
    message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 raising outros.
    p_erro = 'X'.
  else.
    sy-msgty = 'S'.
    sy-msgid = 'F5'.
    sy-msgno = '312'.
    sy-msgv1 = wa_bkpf-belnr.
    sy-msgv2 = wa_bkpf-gjahr.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      FORM  F_GET_CONTA_RAZAO
*&---------------------------------------------------------------------*
form f_get_conta_razao using i_shtyp  type shtyp
                             i_tcode  type tcode
                             i_fatura	type zfatura
                             i_tp_emissor	type ztp_emissor
                             i_operfrete_range  type zde_zoperfrete_ranges_t
                             i_tp_veiculo	type zde_tp_prop_veiculo_ranges_t
                             i_dt_referencia  type budat
                    changing e_it_zlest0021	type zde_zlest0021_t.

  data: lc_zlest0021 type zlest0021.

  clear: e_it_zlest0021[], e_it_zlest0021.

  data(lc_operfrete_range) = i_operfrete_range[].

  data: lc_dt_vencimento type zlest0021-dt_vencimento.

  "Checa Data Inicial
  select single * into @data(wa_zlest0021)
    from zlest0021
   where dt_vencimento eq @lc_dt_vencimento.

  if sy-subrc is initial.
    update zlest0021
       set dt_vencimento = '99991231'
     where dt_vencimento eq lc_dt_vencimento.
  endif.

  select * into table @data(it_zlest0021)
    from zlest0021
   where shtyp         eq @i_shtyp
     and tcode         eq @i_tcode
     and fatura        eq @i_fatura
     and tp_emissor    eq @i_tp_emissor
     and operfrete     in @lc_operfrete_range
     and tp_veiculo    in @i_tp_veiculo
     and dt_vencimento ge @i_dt_referencia.

  if sy-subrc is not initial.
    exit.
  endif.

*    IF I_OPERFRETE_RANGE IS NOT INITIAL.
  data(it_zlest0021_aux) = it_zlest0021[].
  sort it_zlest0021 by operfrete.
  delete adjacent duplicates from it_zlest0021_aux comparing operfrete.
  loop at it_zlest0021_aux into data(wa_zlest0021_aux).
    clear: lc_zlest0021.
    loop at it_zlest0021 into wa_zlest0021 where operfrete eq wa_zlest0021_aux-operfrete.
      if lc_zlest0021 is initial.
        lc_zlest0021 = wa_zlest0021.
      elseif wa_zlest0021-dt_vencimento lt lc_zlest0021-dt_vencimento.
        lc_zlest0021 = wa_zlest0021.
      endif.
    endloop.
    append lc_zlest0021 to e_it_zlest0021.
  endloop.

*  IF i_operfrete IS NOT INITIAL.
*    READ TABLE e_it_zlest0021 INDEX 1 INTO e_zlest0021.
*  ENDIF.

endform.
*&---------------------------------------------------------------------*
*&      FORM  F_BUSCA_CONTAS_MIRO
*&---------------------------------------------------------------------*
form f_busca_contas_miro using i_bukrs type bukrs
                               i_werks type werks_d
                               i_vsart  type vsarttr
                               i_shtyp  type shtyp
                               i_item_invoice	type rblgp
                               i_valor_item	type bapiwrbtr
                               i_valor_perda  type zvlr_perda
                               i_valor_quebra	type zvlr_quebra
*---> 31/05/2023 - Migração S4 - JS
*                              i_valor_vi	TYPE zde_vlr_vi
                               i_valor_vi type dmbtr
*<--- 31/05/2023 - Migração S4 - JS
                               i_dt_referencia  type budat
                      changing e_contas	type zbapi_incinv_gl_account_t
                               e_it_zlest0021	type zde_zlest0021_t.

  data: "it_zlest0021  TYPE TABLE OF zlest0021,
    wa_zlest0021  type zlest0021,
    wa_contas     type bapi_incinv_create_gl_account,
    lc_zvlr_frete type zde_vlr_frete,
    lc_vlr_ajuste type zde_vlr_frete.

  data i_operfrete_range type zde_zoperfrete_ranges_t.

  "CLEAR: e_contas.

  "Controle de desterminação conta razão
*      1  Cofins a Recuperar
*      2  Debito Frete
*      3  Desconto Seguro
*      4  Forn. Repon
*      5  Icms a Recuperar
*      6  Inss 20% empresa
*      7  INSS Retido
*      8  IRRF Retido
*      9  Pis a Recuperar
*      10	Seguro a Pagar
*      11	Sest/senat
*      12	Subcontratado/Fornecedor
*      13	Transitoria
*      14	Vale Pedágio
*      15	Outros
*      16	Quebra
*      17	Sobra
*      18	Perda
*      19	EM/EF Débito
*      20	EM/EF Crédito
*      21	Estadia
*      22	Compl Preço
*      23	Recuperação Seguro
*      24 Vlr total impostos
*      ...
*     31 Debito Posterior

  data: rgveicu type range of zde_tp_prop_veiculo_ctb.

  rgveicu = value #( sign = 'I' option = 'EQ' ( low = space high = space ) ( low = '0' high = '0' ) ).

  try .

      "APPEND 'IEQ2' TO i_operfrete_range.
      "APPEND 'IEQ5' TO i_operfrete_range.
      "APPEND 'IEQ14' TO i_operfrete_range.
      append 'IEQ16' to i_operfrete_range.
      append 'IEQ18' to i_operfrete_range.
      append 'IEQ19' to i_operfrete_range.
      append 'IEQ20' to i_operfrete_range.
      append 'IEQ24' to i_operfrete_range.

      perform f_get_conta_razao
        using i_shtyp
              'MIRO'
              'T'
              'T'
              i_operfrete_range
              rgveicu
              i_dt_referencia
     changing e_it_zlest0021.

    catch zcx_controle_conta_razao.    " .
      "MESSAGE e001 RAISING param_ctb.
  endtry.


*  READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '2'.
*
*  IF sy-subrc IS INITIAL.
  data(lc_metodo_antigo) = abap_true.
*  ELSE.
  "lc_metodo_antigo       = abap_true.
*  ENDIF.

  "16	Quebra
  if i_valor_quebra gt 0.

    read table e_it_zlest0021 into wa_zlest0021 with key operfrete = '16'.

    if sy-subrc is initial.

      clear: wa_contas.

      wa_contas-invoice_doc_item = i_item_invoice.

      wa_contas-gl_account       = wa_zlest0021-razaocred.

      wa_contas-item_amount      = i_valor_quebra.

      wa_contas-db_cr_ind        = 'H'.

      wa_contas-comp_code        = i_bukrs.

      wa_contas-bus_area         = i_werks.

      "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. "#corrigir

      concatenate 'Quebra de Frete' wa_contas-item_text into wa_contas-item_text separated by space.

      append wa_contas to e_contas.

    else.

      message e077 with '16' raising param_ctb.

    endif.

  endif.

  "18	Perda
  if i_valor_perda gt 0.

    read table e_it_zlest0021 into wa_zlest0021 with key operfrete = '18'.

    if sy-subrc is initial.

      clear: wa_contas.

      wa_contas-invoice_doc_item = i_item_invoice.

      wa_contas-gl_account       = wa_zlest0021-razaocred.

      wa_contas-item_amount      = i_valor_perda.

      wa_contas-db_cr_ind        = 'H'.

      wa_contas-comp_code        = i_bukrs.

      wa_contas-bus_area         = i_werks.

      "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir

      concatenate 'Perda de Frete' wa_contas-item_text into wa_contas-item_text separated by space.

      append wa_contas to e_contas.

    else.

      message e077 with '18' raising param_ctb.

    endif.

  endif.

  lc_zvlr_frete = i_valor_vi - i_valor_item.

  "19  EM/EF Débito
  if lc_zvlr_frete > 0.
    read table e_it_zlest0021 into wa_zlest0021 with key operfrete = '19'.
    if sy-subrc is initial.
      clear: wa_contas.
      wa_contas-invoice_doc_item = i_item_invoice.
      wa_contas-gl_account       = wa_zlest0021-razaodeb.
      wa_contas-item_amount      = lc_zvlr_frete.
      wa_contas-db_cr_ind        = 'S'.
      wa_contas-comp_code        = i_bukrs.
      wa_contas-bus_area         = i_werks.
      "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir
      concatenate 'EM/EF Débito' wa_contas-item_text into wa_contas-item_text separated by space.
      append wa_contas to e_contas.
    else.
      message e077 with '19' raising param_ctb.
    endif.
  endif.

  "20  EM/EF Crédito
  if lc_zvlr_frete < 0.
    read table e_it_zlest0021 into wa_zlest0021 with key operfrete = '20'.
    if sy-subrc is initial.
      clear: wa_contas.
      wa_contas-invoice_doc_item = i_item_invoice.
      wa_contas-gl_account       = wa_zlest0021-razaocred.
      wa_contas-item_amount      = abs( lc_zvlr_frete ).
      wa_contas-db_cr_ind        = 'H'.
      wa_contas-comp_code        = i_bukrs.
      wa_contas-bus_area         = i_werks.
      "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir
      concatenate 'EM/EF Crédito' wa_contas-item_text into wa_contas-item_text separated by space.
      append wa_contas to e_contas.
    else.
      message e077 with '20' raising param_ctb.
    endif.
  endif.


*  lc_vlr_ajuste = lc_zvlr_frete.
*
*  IF abs( lc_vlr_ajuste ) NE 0.
*    READ TABLE e_it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '24'.
*    IF sy-subrc IS INITIAL.
*      CLEAR: wa_contas.
*      wa_contas-invoice_doc_item = i_item_invoice.
*      wa_contas-item_amount      = abs( lc_vlr_ajuste ).
*      wa_contas-comp_code        = i_bukrs.
*      wa_contas-bus_area         = i_werks.
*      "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir
*      CONCATENATE 'Ajuste de Custo' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
*
*      IF lc_vlr_ajuste GT 0.
*        wa_contas-gl_account = wa_zlest0021-razaocred.
*        wa_contas-db_cr_ind  = 'H'.
*      ELSE.
*        wa_contas-gl_account = wa_zlest0021-razaodeb.
*        wa_contas-db_cr_ind  = 'S'.
*      ENDIF.
*      APPEND wa_contas TO e_contas.
*    ELSE.
*      MESSAGE e077 WITH '24' RAISING param_ctb.
*    ENDIF.
*  ENDIF.

*        IF lc_vlr_ajuste LT 0.
*
*          "20  EM/EF Crédito
*          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '20'.
*          IF sy-subrc IS INITIAL.
*            CLEAR: wa_contas.
*            wa_contas-invoice_doc_item = i_item_invoice.
*            wa_contas-gl_account       = wa_zlest0021-razaocred.
*            wa_contas-item_amount      = abs( lc_vlr_ajuste ).
*            wa_contas-db_cr_ind        = 'H'.
*            wa_contas-comp_code        = i_bukrs.
*            wa_contas-bus_area         = i_werks.
*            "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir
*            CONCATENATE 'EM/EF Crédito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
*            APPEND wa_contas TO e_contas.
*          ELSE.
*            MESSAGE e077 WITH '20' RAISING param_ctb.
*          ENDIF.
*
*        ELSEIF lc_vlr_ajuste GT 0.
*
*          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '19'.
*          IF sy-subrc IS INITIAL.
*            CLEAR: wa_contas.
*            wa_contas-invoice_doc_item = i_item_invoice.
*            wa_contas-gl_account       = wa_zlest0021-razaodeb.
*            wa_contas-item_amount      = lc_vlr_ajuste.
*            wa_contas-db_cr_ind        = 'S'.
*            wa_contas-comp_code        = i_bukrs.
*            wa_contas-bus_area         = i_werks.
*            "CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text. #corrigir
*            CONCATENATE 'EM/EF Crédito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
*            APPEND wa_contas TO e_contas.
*          ELSE.
*            MESSAGE e077 WITH '19' RAISING param_ctb.
*          ENDIF.
*
*        ENDIF.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_4000_ALV_02
*&---------------------------------------------------------------------*
form f_fill_4000_alv_02 .

  check gt_xnota is initial.

  perform f_search_4000 using 'X'.

*
*  SELECT * FROM zlest0034
*    INTO TABLE @DATA(lt_0034)
*      WHERE guid_header = @gs_nfse_4000-guid_header.
*
*  CHECK sy-subrc EQ 0.
*
*  LOOP AT lt_0034 ASSIGNING FIELD-SYMBOL(<fs_0034>).
*
*    append INITIAL LINE TO gt_xnota ASSIGNING FIELD-SYMBOL(<fs_xnota>).
*
*<fs_xnota>-TKNUM = <fs_0034>-TKNUM.
*<fs_xnota>-TDLNR = <fs_0034>-TDLNR.
*<fs_xnota>-SHTYP = <fs_0034>-SHTYP.
*<fs_xnota>-VSART = <fs_0034>-VSART.
*<fs_xnota>-ADD03 = <fs_0034>-ADD03.
*<fs_xnota>-ABFER = <fs_0034>-ABFER.
*<fs_xnota>-DATBG = <fs_0034>-DATBG.
*"<fs_xnota>-VALUE_BRANCH = <fs_0034>-
*"<fs_xnota>-TERCEIRO = <fs_0034>-
*<fs_xnota>-BUKRS     = <fs_0034>-BUKRS.
*<fs_xnota>-DTEMISSAO = <fs_0034>-DTEMISSAO.
*<fs_xnota>-BELNR = <fs_0034>-BELNR.
*<fs_xnota>-GJAHR = <fs_0034>-GJAHR.
*<fs_xnota>-DOCNUM = <fs_0034>-DOCNUM.
*<fs_xnota>-WERKS = <fs_0034>-WERKS.
*<fs_xnota>-MATNR = <fs_0034>-MATNR.
*<fs_xnota>-MWSKZ = <fs_0034>-MWSKZ.
*<fs_xnota>-MAKTX = <fs_0034>-MAKTX.
*<fs_xnota>-MENGE = <fs_0034>-MENGE.
*<fs_xnota>-MEINS = <fs_0034>-MEINS.
*<fs_xnota>-NETWR = <fs_0034>-NETWR.
*<fs_xnota>-WAERS = <fs_0034>-WAERS.
*<fs_xnota>-CHAVE = <fs_0034>-CHAVE.
*<fs_xnota>-SERIE = <fs_0034>-SERIE.
*<fs_xnota>-NUMERO = <fs_0034>-NUMERO.
*<fs_xnota>-PARTYP = <fs_0034>-PARTYP
*<fs_xnota>-PARC_CLIENTE = <fs_0034>-PARC_CLIENTE
*<fs_xnota>-PARC_FORN = <fs_0034>-PARC_FORN
*<fs_xnota>-NAME1 = <fs_0034>-NAME1
*<fs_xnota>-REG_EMISSOR = <fs_0034>-REG_EMISSOR
*<fs_xnota>-REG_DEST = <fs_0034>-REG_DEST
*<fs_xnota>-PARC_V = <fs_0034>-PARC_V
*<fs_xnota>-PARC_C = <fs_0034>-PARC_C
*<fs_xnota>-XFRETE_FKNUM = <fs_0034>-
*<fs_xnota>-XFRETE_EBELN = <fs_0034>-
*<fs_xnota>-XFRETE_EBELP = <fs_0034>-
*<fs_xnota>-XFRETE_LBLNI = <fs_0034>-
*<fs_xnota>-XFRETE_NETWR = <fs_0034>-
*<fs_xnota>-XFRETE_KNUMV = <fs_0034>-
*<fs_xnota>-XFRETE_WAERS = <fs_0034>-
*<fs_xnota>-XFRETE_BUDAT = <fs_0034>-
*<fs_xnota>-XVLR_UNIT_FRET = <fs_0034>-
*<fs_xnota>-XVLR_QUEBRA = <fs_0034>-
*<fs_xnota>-XFRETE_QUEBRA = <fs_0034>-
*<fs_xnota>-XFRETE_PERDA = <fs_0034>-
*<fs_xnota>-XFRETE_VLR_PERDA = <fs_0034>-
*<fs_xnota>-LWERT = <fs_0034>-
*<fs_xnota>-LFGJA = <fs_0034>-
*<fs_xnota>-MATNS = <fs_0034>-
*<fs_xnota>-ESSR_BUDAT = <fs_0034>-
*<fs_xnota>-DATA_DESCARGA = <fs_0034>-
*<fs_xnota>-PESO_DESCARGA = <fs_0034>-
*<fs_xnota>-QUANT_TOLERANCIA = <fs_0034>-
*<fs_xnota>-XVLR_LIQ_VI = <fs_0034>-
*<fs_xnota>-XVLR_LIQ_NFS = <fs_0034>-
*<fs_xnota>-XNFS_VLR_ISS = <fs_0034>-
*<fs_xnota>-XNFS_VALUE = <fs_0034>-
*<fs_xnota>-RATE_PIS = <fs_0034>-
*<fs_xnota>-BASE_PIS = <fs_0034>-
*<fs_xnota>-XVLR_PIS = <fs_0034>-
*<fs_xnota>-RATE_COFINS = <fs_0034>-
*<fs_xnota>-BASE_COFINS = <fs_0034>-
*<fs_xnota>-XVLR_COFINS = <fs_0034>-
*<fs_xnota>-GUID_HEADER = <fs_0034>-
*<fs_xnota>-RE_BELNR = <fs_0034>-
*<fs_xnota>-RE_GJAHR = <fs_0034>-
*<fs_xnota>-EN_DOCNUM = <fs_0034>-
*<fs_xnota>-USER_ASSOC = <fs_0034>-
*<fs_xnota>-DATA_ASSOC = <fs_0034>-
*<fs_xnota>-HORA_ASSOC = <fs_0034>-
*<fs_xnota>-FLAG_ASSOC = <fs_0034>-
*
*
*  ENDLOOP.
*

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_FORM
*&---------------------------------------------------------------------*
form f_get_data_form changing p_data type j_1btxdatf.

  data lw_write type c length 20.

  write sy-datum to lw_write.

  call function 'CONVERSION_EXIT_INVDT_INPUT'
    exporting
      input  = lw_write
    importing
      output = p_data.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_GET_IMPOSTOS
*&---------------------------------------------------------------------*
form f_get_impostos .

  if gt_scr_condition_list is initial.

    data lo_util type ref to /tcsr/c_util_monitor.

    create object lo_util
      exporting
        iv_guid_header = gs_nfse_4000-guid_header.

    check lo_util is bound.

    try .
        lo_util->get_xml( ).
      catch /tcsr/cx_exception into data(lo_exception).
        "Display Error message
        message id lo_exception->if_t100_message~t100key-msgid
           type 'E'
         number lo_exception->if_t100_message~t100key-msgno
           with lo_exception->if_t100_message~t100key-attr1
                lo_exception->if_t100_message~t100key-attr2
                lo_exception->if_t100_message~t100key-attr3
                lo_exception->if_t100_message~t100key-attr4.
    endtry.

    data lt_po_list type /tcsr/y_po_list.

    lo_util->get_scr_cond_list(
      exporting
        iv_tolerance_tax = gv_total_tolerance
      importing
        et_scr_cond_list = gt_scr_condition_list[]
      changing
        it_po_list       = lt_po_list
    ).

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_NOTA_FILTRO
*&---------------------------------------------------------------------*
form f_nota_filtro changing ct_nota type zibc_nfps_xnota.

  if so_chnfe[] is not initial.

    delete ct_nota where chave not in so_chnfe.

  endif.

  if so_nfe[] is not initial.

    delete ct_nota where numero not in so_nfe.

  endif.

  if so_lifnr[] is not initial.

    delete ct_nota where parc_cliente not in so_lifnr.

  endif.

  if so_ebeln[] is not initial.

    delete ct_nota where xfrete_ebeln not in so_ebeln.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CALCULO_LINHA
*&---------------------------------------------------------------------*
form f_calculo_linha changing p_nota type zibs_nfps_xnota.

  p_nota-zpeso_diferenca = 0.
  p_nota-xfrete_quebra = 0.
  p_nota-xvlr_quebra = 0.
  p_nota-xfrete_perda  =  0.
  p_nota-xfrete_vlr_perda = 0.

  p_nota-xvlr_liq_vi = 0.
  p_nota-xvlr_liq_nfs = 0.

  p_nota-zpeso_diferenca = p_nota-zpeso_origem - p_nota-peso_descarga.

  if p_nota-peso_descarga > p_nota-zpeso_origem.
    p_nota-zpeso_diferenca = p_nota-zpeso_diferenca * -1.
  endif.

  check p_nota-peso_descarga > 0 .
  "CHECK p_nota-zpeso_diferenca > 0. " 28.04.2023 - 110959

  check p_nota-peso_descarga <= p_nota-zpeso_origem.

  " 2.4.6 - Cálculo da quebra KG
  p_nota-xfrete_quebra = p_nota-zpeso_origem - p_nota-peso_descarga.

  "p_nota-xfrete_perda = p_nota-xfrete_quebra + p_nota-quant_tolerancia.

  " 2.4.7 - Valor de quebra
  p_nota-xvlr_quebra = p_nota-xvlr_unit_fret * p_nota-xfrete_quebra.

  " 2.4.8  - Cálculo da perda KG
  p_nota-xfrete_perda  =  p_nota-xfrete_quebra - p_nota-quant_tolerancia.

  if p_nota-xfrete_perda < 0.
    p_nota-xfrete_perda = 0.
  endif.

  "IF p_nota-xfrete_perda > 0. " 28.04.2023 - 110959

  " 2.4.9 - Cálculo da perda em valor
  p_nota-xfrete_vlr_perda = p_nota-xfrete_perda * p_nota-xvlr_unit_fret.
  p_nota-xfrete_vlr_perda = p_nota-xfrete_perda * ( p_nota-netwr / p_nota-zpeso_origem ).

  p_nota-xvlr_liq_vi = p_nota-xfrete_netwr - p_nota-xfrete_vlr_perda - p_nota-xvlr_quebra - p_nota-xnfs_vlr_iss.
  p_nota-xvlr_liq_nfs = p_nota-xnfs_value - p_nota-xfrete_vlr_perda - p_nota-xvlr_quebra - p_nota-xnfs_vlr_iss.

  "ENDIF. " 28.04.2023 - 110959


  if p_nota-zpeso_diferenca < 0.
    p_nota-zpeso_diferenca = 0.
  endif.

  if p_nota-xvlr_liq_vi < 0.
    p_nota-xvlr_liq_vi = 0.
  endif.

  if p_nota-xvlr_liq_nfs < 0.
    p_nota-xvlr_liq_nfs = 0.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_XNOTA_TO_ALV
*&---------------------------------------------------------------------*
form f_xnota_to_alv using p_xnota type zibs_nfps_xnota
                 changing p_alv   type zibs_nfps_001.

  p_alv-guid_header = p_xnota-guid_header.

  p_alv-chave_nfe = p_xnota-chave.
  p_alv-lifnr = p_xnota-parc_forn.
  p_alv-docdat = p_xnota-dtemissao.
  p_alv-datbg = p_xnota-datbg.

  if p_xnota-re_belnr is not initial.
    p_alv-com_miro = 'X'.
  else.
    p_alv-com_miro = space.
  endif.

  if p_xnota-guid_header is not initial.
    p_alv-ja_associ = 'X'.
  else.
    p_alv-ja_associ = space.
  endif.

  p_alv-todas = 'X'.

  p_alv-bukrs = p_xnota-bukrs.
  p_alv-werks = p_xnota-werks.
  p_alv-nfenum = p_xnota-numero.
  p_alv-tknum = p_xnota-tknum.
  p_alv-fknum = p_xnota-xfrete_fknum.
  p_alv-zpeso_origem = p_xnota-zpeso_origem.
  p_alv-zpeso_destino = p_xnota-peso_descarga.

  p_alv-re_belnr = p_xnota-re_belnr.
  p_alv-re_gjahr = p_xnota-re_gjahr.

  if p_alv-zpeso_destino > 0.

    p_alv-zquebra = p_xnota-xfrete_quebra.
    p_alv-zperda = p_xnota-xfrete_perda.
    p_alv-zvlr_quebra = p_xnota-xvlr_quebra.
    p_alv-zvlr_perda = p_xnota-xfrete_vlr_perda.

  endif.

  p_alv-zdt_chegada = p_xnota-data_descarga.
  p_alv-matnr = p_xnota-matnr.
  p_alv-maktx = p_xnota-maktx.
  p_alv-zvlr_vi = p_xnota-xfrete_netwr."<fs_xnota>-netwr. "21.11.2022
  p_alv-iva = 'S1'."<fs_xnota>-iva.
  p_alv-meins = p_xnota-meins.

  p_alv-regio_emissor = p_xnota-reg_emissor.
  p_alv-regio_receptor = p_xnota-reg_dest.
  p_alv-valor_pis = p_xnota-xvlr_pis.
  p_alv-valor_cofins = p_xnota-xvlr_cofins.
  p_alv-waers = p_xnota-waers.

  p_alv-iss_total = p_xnota-xnfs_vlr_iss.

  if p_xnota-parc_cliente is not initial.
    p_alv-parc_cliente = p_xnota-parc_cliente.
  else.
    p_alv-parc_cliente = p_xnota-parc_forn.
  endif.

  p_alv-name1 = p_xnota-name1.
  p_alv-docnum = p_xnota-docnum.
  p_alv-valor_mercadoria = p_xnota-netwr.
  p_alv-ebeln = p_xnota-xfrete_ebeln.
  p_alv-ebelp = p_xnota-xfrete_ebelp.
  p_alv-lblni = p_xnota-xfrete_lblni.

  if p_alv-zquebra < 0.
    p_alv-zquebra = p_alv-zquebra * -1.
  endif.

  if p_alv-zperda < 0.
    p_alv-zperda = p_alv-zperda * -1.
  endif.

  if p_alv-zvlr_quebra < 0.
    p_alv-zvlr_quebra = p_alv-zvlr_quebra * -1.
  endif.

  if p_alv-zvlr_perda < 0.
    p_alv-zvlr_perda = p_alv-zvlr_perda * -1.
  endif.

  if p_alv-zvlr_vi < 0.
    p_alv-zvlr_vi = p_alv-zvlr_vi * -1.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DESC_CANC
*&---------------------------------------------------------------------*
form f_preenche_desc_canc using p_cod type /tcsr/t_cancrt-cancreason
                       changing p_descr type c.

  data lt_descr type table of dd07v.

  call function 'DD_DOMVALUES_GET'
    exporting
      domname        = 'ZMOTIVO_CANCELAR'
      text           = 'X'
      langu          = sy-langu
      bypass_buffer  = 'X'
    tables
      dd07v_tab      = lt_descr
    exceptions
      wrong_textflag = 1
      others         = 2.

  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.


  read table lt_descr assigning field-symbol(<fs_descr>)
    with key domvalue_l = p_cod.

  check sy-subrc eq 0.

  p_descr = <fs_descr>-ddtext.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALORES_SAP
*&---------------------------------------------------------------------*
form f_valores_sap .

  check go_znfse is bound.

  data(lt_simu) = go_znfse->get_simulate( ).

  check lt_simu is not initial.

  loop at lt_simu assigning field-symbol(<fs_simu>).

    " --------------------------------- PISCOFCSLL
    if <fs_simu>-wi_tax_type = 'CT'.

      read table gt_scr_condition_list assigning field-symbol(<fs_cond>)
       with key kschl = 'IGEW'.

      if sy-subrc eq 0.

        <fs_cond>-sap_value = <fs_simu>-wi_tax_amt.

      endif.

    endif.

    " --------------------------------- IR
    if <fs_simu>-wi_tax_type = 'IR'.

      read table gt_scr_condition_list assigning <fs_cond>
       with key kschl = 'IRRW'.

      if sy-subrc eq 0.

        <fs_cond>-sap_value = <fs_simu>-wi_tax_amt.

      endif.

    endif.

    " --------------------------------- ISS
    if <fs_simu>-wi_tax_type = 'IS'.

      read table gt_scr_condition_list assigning <fs_cond>
       with key kschl = 'ISS'.

      if sy-subrc eq 0.

        <fs_cond>-sap_value = <fs_simu>-wi_tax_amt.

      endif.

    endif.

    " --------------------------------- INSS
    if <fs_simu>-wi_tax_type = 'IN'.

      read table gt_scr_condition_list assigning <fs_cond>
       with key kschl = 'INSS'.

      if sy-subrc eq 0.

        <fs_cond>-sap_value = <fs_simu>-wi_tax_amt.

      endif.

    endif.

  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_CHANGE
*&---------------------------------------------------------------------*
form f_save_change using p_old type zibt_nfse_001
                         p_new type zibt_nfse_001.

  data lt_fields type table of dfies.
  data lt_005 type table of zibt_nfse_005.

  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname        = 'ZIBT_NFSE_001'
    tables
      dfies_tab      = lt_fields
    exceptions
      not_found      = 1
      internal_error = 2
      others         = 3.

  check sy-subrc eq 0.

  select max( msgno ) from zibt_nfse_005
    into @data(lv_count)
      where  guid_header = @p_old-guid_header
        and msgty = @space.

  loop at lt_fields assigning field-symbol(<fs_field>).

    data(lv_old_name) = 'P_OLD-' && <fs_field>-fieldname.
    data(lv_new_name) = 'P_NEW-' && <fs_field>-fieldname.

    assign (lv_old_name) to field-symbol(<fs_old>).
    assign (lv_new_name) to field-symbol(<fs_new>).

    check <fs_old> is assigned.
    check <fs_new> is assigned.

    check <fs_old> <> <fs_new>.

    append initial line to lt_005 assigning field-symbol(<fs_005>).

    add 1 to lv_count.

    data(lv_message) = <fs_field>-scrtext_l && ` [` && <fs_field>-fieldname && `]`.

    <fs_005>-guid_header = p_old-guid_header.
    <fs_005>-dt_registro = sy-datum.
    <fs_005>-hr_registro = sy-uzeit.
    <fs_005>-us_registro = sy-uname.

    <fs_005>-message = lv_message.
    unpack lv_count to <fs_005>-msgno.
    <fs_005>-msgv1 = <fs_old>.
    <fs_005>-msgv2 = <fs_new>.

  endloop.

  check lt_005 is not initial.

  modify zibt_nfse_005 from table lt_005.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REMOV_SPEC_CHAR
*&---------------------------------------------------------------------*
form f_remov_spec_char using p_text_in type c
                    changing p_text_out type c.

  call function 'SCP_REPLACE_STRANGE_CHARS'
    exporting
      intext            = p_text_in
    importing
      outtext           = p_text_out
    exceptions
      invalid_codepage  = 1
      codepage_mismatch = 2
      internal_error    = 3
      cannot_convert    = 4
      fields_not_type_c = 5
      others            = 6.

  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_FORM_PAG
*&---------------------------------------------------------------------*
form f_preenche_form_pag .

  data lv_value type netwr_fp.

  lv_value = zsheader_data_nfse_inbound-nfse_value.

  call function 'Z_RET_FORMA_PAGAMENTO'
    exporting
      p_bukrs           = zsheader_data_nfse_inbound-bukrs
      p_lifnr           = zsheader_data_nfse_inbound-lifnr
      p_valor           = lv_value
      p_bvtyp           = zspayment_data_nfse_inbound-bvtyp
    importing
      p_forma_pagamento = zspayment_data_nfse_inbound-pymt_meth
      p_princ_bnc_emp   = zspayment_data_nfse_inbound-hbkid
    exceptions
      nao_fornecedor    = 1
      fornecedor_conta  = 2
      fornecedor_banco  = 3
      faixa_valor       = 4
      banco_empresa     = 5
      others            = 6.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_DATA_VENCIMENTO
*&---------------------------------------------------------------------*
form f_data_vencimento changing ps_payment type zspayment_data_nfse_inbound cv_erro type c.

  data: ls_lfa1    type lfa1,
        zvar_check type char01.

  data lv_dt_out type sydatum.

  clear cv_erro.

  " só valida se estiver marcado para editar campos
  check gv_edit_2000 is not initial.

  check ps_payment-dt_vencimento is not initial.


*********************************************************************************
* Inicio verifica os dados fornecedor. / BUG SOLTO 118663 / AOENNING
*********************************************************************************
  clear: ls_lfa1, zvar_check.
  if zsheader_data_nfse_inbound-lifnr is not initial.
    select single * from lfa1 into ls_lfa1
      where lifnr eq zsheader_data_nfse_inbound-lifnr.
    if ls_lfa1-dlgrp is not initial.
      zvar_check = abap_true.
    endif.
  endif.

  perform f_check_data_venc
    using ps_payment-dt_vencimento
 changing lv_dt_out
          cv_erro. "US #170323 - MMSILVA - 02.05.2025

  if ps_payment-dt_vencimento <> lv_dt_out.

    data lv_mess type c length 80.

    write ps_payment-dt_vencimento to lv_mess left-justified.


*********************************************************************************
* Inicio verifica os dados fornecedor. / BUG SOLTO 118663 / AOENNING
*********************************************************************************
    if zvar_check is not initial.

      lv_mess = `Vencimento ` && lv_mess && ` não pode ser menor que 72h úteis a partir de hoje`.
    else.
      lv_mess = `Vencimento ` && lv_mess && ` não pode ser menor que 120h úteis a partir de hoje`.
    endif.

*********************************************************************************
* Fim verifica os dados fornecedor. / BUG SOLTO 118663 / AOENNING
*********************************************************************************

    message lv_mess type 'S' display like 'E'.

    ps_payment-dt_vencimento = lv_dt_out.

    cv_erro = 'X'.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA_VENC
*&---------------------------------------------------------------------*
form f_check_data_venc using p_date type sy-datum
                    changing p_date_out type sydatum
                             cv_erro    type c. "US #170323 - MMSILVA - 02.05.2025

  data lv_dt_in type sydatum.

  lv_dt_in = p_date.

  perform f_prox_dia_util changing lv_dt_in.

  perform f_check_dia_72 using lv_dt_in changing p_date_out cv_erro.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PROX_DIA_UTIL
*&---------------------------------------------------------------------*
form f_prox_dia_util changing p_date type sy-datum.

  call method zcl_miro=>get_proximo_dia_util
    exporting
      i_data_base = p_date
      i_bukrs     = wa_bukrs_calendary "USER STORY 158527 - MMSILVA - 17.01.2025
*     i_signum    = '+'
*     i_ck_data_zles0145 =
    receiving
      r_data      = p_date
    exceptions
      erro        = 1
      others      = 2.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DIA_72
*&---------------------------------------------------------------------*
form f_check_dia_72 using p_date type sy-datum
                 changing p_date_out type sy-datum
                          lv_erro    type c. "US #170323 - MMSILVA - 02.05.2025

  data: ls_lfa1    type lfa1,
        zvar_check type char01.

* US #170323 - MMSILVA - 02.05.2025 - Inicio
*  data lv_erro type c value 'X'.
  lv_erro = 'X'.
* US #170323 - MMSILVA - 02.05.2025 - Fim

  data lv_date type sy-datum.

  lv_date = p_date.

* US #170323 - MMSILVA - 02.05.2025 - Inicio
  data obj_nfe TYPE REF TO zcl_nfe_inbound.

  CREATE OBJECT obj_nfe.

  obj_nfe->set_dt_vencimento( i_dt_vencimento = zspayment_data_nfse_inbound-dt_vencimento ).
  obj_nfe->set_fora_politica( EXPORTING i_ck_fpol = zspayment_data_nfse_inbound-ck_fpol ).
  obj_nfe->set_obs_financeira( EXPORTING i_obs_financeira = zspayment_data_nfse_inbound-obs_financeira ).
* US #170323 - MMSILVA - 02.05.2025 - Fim

  if lv_date is initial.
    exit.
  endif.


*********************************************************************************
* Inicio verifica os dados fornecedor. / BUG SOLTO 118663 / AOENNING
*********************************************************************************
  clear: ls_lfa1, zvar_check.
  if zsheader_data_nfse_inbound-lifnr is not initial.
    select single * from lfa1 into ls_lfa1
      where lifnr eq zsheader_data_nfse_inbound-lifnr.
    if ls_lfa1-dlgrp is not initial.
      zvar_check = abap_true.
    endif.
  endif.
  if zibt_nfse_001-ck_revisao is not initial.
    zvar_check = zibt_nfse_001-ck_revisao.
  endif.

  while lv_erro = 'X'.

    try.
        call method zcl_miro=>verificar_vencimento_fatura
          exporting
            i_data_vencimento = lv_date
            i_data_se         = space
            i_pymt_meth       = 'U'
            i_ck_revisao      = zvar_check
            i_valida_politica = abap_true                                  "US #170323 - MMSILVA - 02.05.2025
            i_ck_fpol         = zspayment_data_nfse_inbound-ck_fpol        "US #170323 - MMSILVA - 02.05.2025
            i_obs_financeira  = zspayment_data_nfse_inbound-obs_financeira "US #170323 - MMSILVA - 02.05.2025
            i_bukrs           = zsheader_data_nfse_inbound-bukrs.          "US #170323 - MMSILVA - 02.05.2025

        lv_erro = ''.

      catch zcx_miro_exception into data(lo_mro_exc).

*       US #170323 - MMSILVA - 02.05.2025 - Inicio
        IF zspayment_data_nfse_inbound-ck_fpol NE abap_true.
          add 1 to lv_date.

          continue.
        ELSE.

          lo_mro_exc->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

          lv_erro = abap_true.

          exit.
        ENDIF.
*       US #170323 - MMSILVA - 02.05.2025 - Fim

      catch zcx_error into data(lo_erro).
        lo_erro->zif_error~published_erro( exporting i_msgty = 'E' i_msgty_display = 'E' ).
    endtry.

  endwhile.

* US #170323 - MMSILVA - 02.05.2025 - Inicio
  IF zspayment_data_nfse_inbound-ck_fpol EQ abap_true AND lv_date LT sy-datum.
    lv_date = sy-datum.
    zspayment_data_nfse_inbound-dt_vencimento = sy-datum.
  ENDIF.
* US #170323 - MMSILVA - 02.05.2025 - Fim

  p_date_out = lv_date.

* US #170323 - MMSILVA - 02.05.2025 - Inicio
  obj_nfe->set_dt_vencimento( i_dt_vencimento = lv_date ).
  obj_nfe->set_fora_politica( EXPORTING i_ck_fpol = zspayment_data_nfse_inbound-ck_fpol ).
  obj_nfe->set_obs_financeira( EXPORTING i_obs_financeira = zspayment_data_nfse_inbound-obs_financeira ).
* US #170323 - MMSILVA - 02.05.2025 - Fim

endform.
*&---------------------------------------------------------------------*
*&      Form  F_NR_CONHECIMENTO
*&---------------------------------------------------------------------*
form f_nr_conhecimento using p_num type /tcsr/e_nfse_numero
                    changing p_conhec type j_1bnfnum9.

  data lv_temp type /tcsr/e_nfse_numero.

  unpack p_num to lv_temp.

  check lv_temp is not initial.

  p_conhec = lv_temp+6(9).

  "SHIFT p_conhec LEFT DELETING LEADING '0'.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HABILITAR_BOTOES
*&---------------------------------------------------------------------*
form f_habilitar_botoes .

  case 'X'.

    when zibs_nfps_001-nao_associ.

      gv_desa_assign = space.
      gv_desa_btn_02 = space.

    when zibs_nfps_001-ja_associ.

      gv_desa_assign = 'X'.
      gv_desa_btn_02 = space.

    when zibs_nfps_001-com_miro.

      gv_desa_assign = 'X'.
      gv_desa_btn_02 = space.

    when zibs_nfps_001-todas.

      gv_desa_assign = 'X'.
      gv_desa_btn_02 = 'X'.

  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_CANC_REASON
*&---------------------------------------------------------------------*
form f_preenche_canc_reason .

  if /tcsr/t_cancrt-cancreason is not initial.
    select single cancrdescr
      into /tcsr/t_cancrt-cancrdescr
      from /tcsr/t_cancrt
      where cancreason = /tcsr/t_cancrt-cancreason
        and langu      = sy-langu.
  else.
    clear: /tcsr/t_cancrt-cancrdescr.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_NF_DOC_CANCEL
*&---------------------------------------------------------------------*
form f_nf_doc_cancel using p_docnum type j_1bdocnum
                   changing p_doc_canc type j_1bdocnum
                            p_ret2 type bapiret2_tt
                            p_erro type c.

  clear p_erro.

  check p_docnum is not initial.

  call function 'J_1B_NF_DOCUMENT_CANCEL'
    exporting
      doc_number               = p_docnum
      ref_type                 = space
      ref_key                  = space
      can_dat                  = sy-datum
    importing
      doc_number               = p_doc_canc
    exceptions
      document_not_found       = 1
      cancel_not_possible      = 2
      nf_cancel_type_not_found = 3
      database_problem         = 4
      docum_lock               = 5
      nfe_cancel_simulation    = 6
      others                   = 7.

  if p_doc_canc is initial.

    perform f_put_mensagem4
      using 'E'
            'DS'
            '016'
            'Erro no estorno doc.fiscal:'
            p_docnum
            space
            space
   changing p_ret2.

  else.

    perform f_put_mensagem4
      using 'S'
            'DS'
            '016'
            'Doc.fiscal:'
            p_docnum
            'estornado no doc.:'
            p_doc_canc
   changing p_ret2.

    commit work and wait.

  endif.

endform.

*** Inicio - ALX
*&---------------------------------------------------------------------*
*&      Form  F_INICIA_ISS
*&---------------------------------------------------------------------*
form f_inicia_iss  changing p_iss_2000 type flag.

  gv_iss_2000 = abap_true.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ISS_4000
*&---------------------------------------------------------------------*
form f_atualiza_iss_4000 .

  if gv_iss_2000 <> abap_true.

    zibs_nfps_001-iss_total = 0.

  endif.

endform.
*** Fim - ALX
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_RETEM
*&---------------------------------------------------------------------*
form f_processa_retem using p_tela type flag.

  " FOI A TELA QUE CHAMOU O PERFORM??
  if p_tela = 'X'.

    " SE SIM, ENTÃO VER O QUE TEM NO RETER_ISS
    if zsheader_data_nfse_inbound-reter_iss = 'X'.
      zsheader_data_nfse_inbound-nao_reter_iss = space.
    else.
      zsheader_data_nfse_inbound-nao_reter_iss = 'X'.
    endif.

    " FOI CHAMADO DO PROGRAMA???
  else.

    " SE SIM, ENTÃO PROCESSO O QUE VEM DA TABELA
    if zsheader_data_nfse_inbound-nao_reter_iss = 'X'.
      zsheader_data_nfse_inbound-reter_iss = space.
    else.
      zsheader_data_nfse_inbound-reter_iss = 'X'.
    endif.

  endif.


*  "Salvar dados na tabela ZLEST0034.
*  IF zibt_nfse_001-guid_header IS NOT INITIAL AND zibt_nfse_001-bukrs IS NOT INITIAL.
*    SELECT * FROM zlest0034
*      INTO TABLE @DATA(it_zlest0034)
*      WHERE guid_header EQ @zibt_nfse_001-guid_header.
*    IF sy-subrc EQ 0.
*      IF zsheader_data_nfse_inbound-reter_iss EQ abap_false.
*        CLEAR zibs_nfps_001-iss_total.
*      ELSE.
*        zibs_nfps_001-iss_total = VALUE #( gt_scr_condition_list[ kschl = 'ISS' ]-xml_value DEFAULT '0.00' ).
*      ENDIF.
*
*      LOOP AT it_zlest0034 ASSIGNING FIELD-SYMBOL(<ws_zlest0034>) WHERE guid_header EQ zibt_nfse_001-guid_header.
*        <ws_zlest0034>-zvlr_liq_pagar = zibs_nfps_001-vi_total - ( zibs_nfps_001-perda_total + zibs_nfps_001-quebra_total + zibs_nfps_001-iss_total ).
*      ENDLOOP.
*
*      IF it_zlest0034 IS NOT INITIAL.
*        MODIFY zlest0034 FROM TABLE it_zlest0034.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  " Para ficar facilitar o entendimento:
  "     variavel nao_reter é a negação da variavel reter, logo

  "           se nao_reter = 'X', reter = space.
  "           se nao_reter = space, reter = 'X'.

endform.

*** Inicio - Rubenilson - 30.08.24 - US147735
*&---------------------------------------------------------------------*
*& Form f_valida_duplic_doc_fiscal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_valida_duplic_doc_fiscal .

  data: lv_nfe        type j_1bnfnum9,
        lv_lenght_nfe type i,
        lv_msg        type string,
        lv_msg2       type string.

  clear: lv_nfe.
  lv_lenght_nfe =  strlen( zibs_nfse_001-nfse_numero ).
  if lv_lenght_nfe > 9.
    lv_nfe = |{ zibs_nfse_001-nfse_numero+6(9) alpha = out }|.
  else.
    lv_nfe = |{ zibs_nfse_001-nfse_numero alpha = out }|.
  endif.


  lv_nfe = |{ lv_nfe  alpha = in }|.
  condense lv_nfe no-gaps.

  select *
    from zfit0212
    into @data(ls_0212)
    up to 1 rows
    where bukrs              = @zibs_nfse_001-bukrs
      and branch             = @zibs_nfse_001-branch
      and nfse_numero        = @zibs_nfse_001-nfse_numero
      and nfse_serie         = @zibs_nfse_001-nfse_serie
      and status_autorizacao eq 'A'
      and p_cnpj             = @zibs_nfse_001-stcd1.
  endselect.
  if sy-subrc is not initial.
*    SELECT *
*       FROM zibt_nfse_001
*       INTO TABLE @DATA(lt_nfse)
*       WHERE bukrs       = @zibs_nfse_001-bukrs
*         AND branch      = @zibs_nfse_001-branch
*         AND nfse_numero = @zibs_nfse_001-nfse_numero
*         AND p_cnpj      = @zibs_nfse_001-stcd1.

    "Verifica se é uma nota de serviço.
    select b~guid_header, a~bukrs, a~branch, a~lifnr, b~nfse_numero, b~nfse_serie,
    b~dtemissao, b~nfse_value, b~p_cnpj, c~name1 as forne_razao
    into table @data(lt_nfse)
    from /tcsr/t_act as a
    inner join /tcsr/t_hd as b on b~guid_header eq a~guid_header
    left join lfa1 as c on c~lifnr eq a~lifnr
     where  a~bukrs        eq @zibs_nfse_001-bukrs
       and   a~branch      eq @zibs_nfse_001-branch
       and   b~nfse_numero eq @zibs_nfse_001-nfse_numero
       and   a~lifnr       eq @zibs_nfse_001-lifnr
       and   b~p_cnpj      eq @zibs_nfse_001-stcd1.
    if sy-subrc eq 0.
      select single *
        from j_1bnfdoc
        into @data(ls_nfse)
        where bukrs       = @zibs_nfse_001-bukrs
        and   branch      = @zibs_nfse_001-branch
        and   nfenum      = @lv_nfe
        and   model       = '03'
        and   cgc         = @zibs_nfse_001-stcd1.
      if sy-subrc eq 0. "Se encontrar nota lançada, enviar o erro para tela.
        sort lt_nfse by guid_header.
        read table lt_nfse assigning field-symbol(<fs_nfse>) index 1.
        move-corresponding <fs_nfse> to ls_0212.
        ls_0212-nfse_serie = zibs_nfse_001-nfse_serie.
        ls_0212-usuario_solicitacao = sy-uname.
        ls_0212-data_solicitacao = sy-datum.
        ls_0212-hora_solicitacao = sy-uzeit.

        modify zfit0212 from ls_0212.
        if sy-subrc is initial.
          commit work.
        endif.

        raise nota_duplicada.

      endif.
    else.
      lv_msg  = 'Nota fiscal não encontrada na base de dados do SAP!'.
      lv_msg2 = 'Não indentificamos o recebimento do xml!'.
      message e000(z01) with lv_msg lv_msg2.
      exit.
    endif.

  else.

    if ls_0212-status_autorizacao eq 'R'.
      raise nota_duplic_recusada.
    elseif ls_0212-status_autorizacao is initial.
      raise nota_duplicada.
    endif.

  endif.
endform.
*** Fim - Rubenilson - 30.08.24 - US147735
