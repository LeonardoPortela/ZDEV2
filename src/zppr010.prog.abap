**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| De/para de Nf e Lote Fabricante                                           |*
**/===========================================================================\*
*&-----------------------------------------------------------------------------*
*& 26/09/2024  |DEVK9A27D3  |NSEGANTIN      |Melhoria ZPP0014 NF              *
*&----------------------------------------------------------------------------*
report zppr010.

tables: mchb, zppt0016.

types: begin of ty_mchb.
types:
  matnr type mchb-matnr,
  werks type mchb-werks,
  lgort type mchb-lgort,
  charg type mchb-charg,
  clabs type mchb-clabs,
  qtd   type int4,
  end of ty_mchb,

  begin of ty_itens.
types:
  matnr   type matnr,
  charg   type charg_d,
  zlicha  type zde_lote_forn,
  clabs   type labst,
  vfdat   type vfdat,
  dtval   type vfdat,
  chargd  type charg_d,
  werks   type mchb-werks,
  lgort   type mchb-lgort,
  clabs_d type labst,
  style   type lvc_t_styl,
  end of ty_itens,

  t_saida type table of ty_mchb with default key,
  t_itens type table of ty_itens with default key.

data: wa_mchb type mchb,
      wa_mch1 type mch1,
      wa_tela type ty_mchb.

data: gt_outtab type t_saida,
      tg_itens  type t_itens,
      tg_mchb   type table of mchb,
      w_itens   type ty_itens.
data: _style type lvc_t_styl.

data it_0016 type table of zppt0016.
data vl_clabs type labst.
data v_clabs type labst.
data total type char1.
data parcial type char1.
**<<<------"138699 - NMS - INI------>>>
data: gv_msg_err type c.
**<<<------"138699 - NMS - FIM------>>>
data docking        type ref to cl_gui_docking_container.
data splitter       type ref to cl_gui_splitter_container.
data custom_grid    type ref to cl_gui_custom_container.
data grid           type ref to cl_gui_alv_grid.
data alv_tree       type ref to cl_gui_alv_tree.

data: at_header type bapi2017_gm_head_01,
      at_item   type table of bapi2017_gm_item_create,
      l_return  type table of bapiret2,
      _return   type bapiret2,
      _document type bapi2017_gm_head_ret.

selection-screen begin of block b1 with frame title text-i01.
  select-options:
    s_matnr for mchb-matnr      no-extension no intervals obligatory,
    s_werks for mchb-werks      no-extension no intervals obligatory,
    s_lgort for mchb-lgort      no-extension no intervals obligatory,
    s_charg for zppt0016-charg  no-extension no intervals obligatory. "MCHB-CHARG
selection-screen end of block b1.

at selection-screen on value-request for s_charg-low.
  perform f4_s_charg.

class lcl_event_handler definition.
  public section.
    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
endclass.

class lcl_event_handler implementation.

  method on_data_changed.
    loop at er_data_changed->mt_good_cells into data(wl_good_cells) where fieldname = 'VFDAT'.
    endloop.
  endmethod.

endclass.

class cl_main definition.

  public section.

    class-methods run.
    methods set_title_and_status.
    methods process_before_output.
    methods create_docking.
    methods display.
    methods set_outtab_data.
    methods add_lines.
    methods process_division.
    methods estorno_lote.
    methods popup_confirm
      returning value(e_answer) type char1.
    methods create_batch
      importing
                material       type matnr
                centro         type werks_d
                deposito       type lgort_d
                lote           type charg_d
                dt_venc_real   type vfdat
                dt_vencimento  type vfdat
                lote_principal type charg_d
      changing  log            type char1.

    methods transfer_from_batch_to_batch
      importing
        header   type bapi2017_gm_head_01
        items    type bapi2017_gm_item_create_t
      exporting
        document type bapi2017_gm_head_ret.

    methods get_fieldcatalog
      returning value(fcat) type lvc_t_fcat.

    methods select_data
      exceptions
        data_not_found.
    methods get_seq returning value(return) type numc10.

  private section.

endclass.

data r_main type ref to cl_main.

class cl_main implementation.

  method run.

    create object r_main.

    r_main->select_data( exceptions data_not_found = 4 ).

    if sy-subrc is not initial.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    else.
      call screen 0001.
    endif.

  endmethod.

  method set_title_and_status.
    set titlebar 'MAIN_TITLE'.
    set pf-status 'MAIN_STATUS'.
  endmethod.

  method select_data.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = s_matnr-low
      importing
        output       = s_matnr-low
      exceptions
        length_error = 1
        others       = 2.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = s_matnr-high
      importing
        output       = s_matnr-high
      exceptions
        length_error = 1
        others       = 2.

    select single *
      from mchb
      into wa_mchb
      where matnr in s_matnr
      and werks   in s_werks
      and lgort   in s_lgort
      and charg   in s_charg.

    select single *
      from mch1
      into wa_mch1
      where matnr eq wa_mchb-matnr
      and charg eq wa_mchb-charg.

    if sy-subrc is not initial.
      message text-e02 type 'S' raising data_not_found.
    endif.

    select *
    from zppt0016
    into table it_0016
    where matnr in s_matnr
    and werks in s_werks
    and lgort in s_lgort
    and charg in s_charg.

    "Recuperar saldo do lote principal.
    if it_0016 is not initial.
      select *
      from mchb
      into table tg_mchb
      for all entries in it_0016
      where matnr eq it_0016-matnr
      and werks eq it_0016-werks
      and lgort eq it_0016-lgort
      and charg eq it_0016-chargd.
    endif.



  endmethod.

  method set_outtab_data.

    data cont type int4 value 0.
**<<<------"138699 - NMS - INI------>>>
* Verifica se há linha inserida em andamento para não ser excluída na montagem da tela do ALV.
    read table tg_itens transporting no fields with key chargd(1) = '$'.

    if sy-subrc is initial.
      data(lv_add_line) = abap_true.

    else.
**<<<------"138699 - NMS - FIM------>>>
      free tg_itens.

      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input        = wa_mchb-matnr
        importing
          output       = wa_mchb-matnr
        exceptions
          length_error = 1
          others       = 2.

      wa_tela-matnr = wa_mchb-matnr.
      wa_tela-werks = wa_mchb-werks.
      wa_tela-lgort = wa_mchb-lgort.
      wa_tela-charg = wa_mchb-charg.
      wa_tela-clabs = wa_mchb-clabs.

      tg_itens = value #( for ls in it_0016
                          (
                            matnr  = ls-matnr
                            werks  = ls-werks
                            lgort  = ls-lgort
                            charg  = ls-charg
                            zlicha = ls-zlicha
                            clabs  = ls-clabs
                            vfdat  = ls-vfdat
                            dtval  = ls-dtval
                            chargd = ls-chargd
                          )
                        ).

      loop at tg_itens assigning field-symbol(<ws_itens>).
        read table tg_mchb into data(ws_mchb) with key matnr = <ws_itens>-matnr
                                                       werks = <ws_itens>-werks
                                                       lgort = <ws_itens>-lgort
                                                       charg = <ws_itens>-chargd.
        if sy-subrc eq 0.
          <ws_itens>-clabs_d = ws_mchb-clabs.
        endif.
      endloop.

      try.
          data(linha) = lines( tg_itens ).
          add linha to cont.
        catch cx_sy_itab_line_not_found.
      endtry.
**<<<------"138699 - NMS - INI------>>>
* Ajusta o saldo do campo "Quantidade" na parte do cabeçalho da tela e
* ajusta o saldo do Lote do Fornecedor quando ele for particionado na
* Grid do ALV na tela de Identificação/Validação do Lote do Fabricante.
      read table tg_itens assigning field-symbol(<fs_itens>) with key matnr  = wa_tela-matnr
                                                                      charg  = wa_tela-charg
                                                                      chargd = wa_tela-charg.

      if sy-subrc is initial.
        loop at tg_itens into data(ls_itens) where matnr  eq wa_tela-matnr
                                               and charg  eq wa_tela-charg
                                               and chargd ne wa_tela-charg.
* Composição do saldo do campo "Volume" da linha do Lote do Fornecedor.
          subtract ls_itens-clabs from <fs_itens>-clabs.

        endloop.
* Composição do saldo do campo "Quantidade".
        wa_tela-clabs = wa_tela-clabs - <fs_itens>-clabs.
        if wa_tela-clabs < 0.
          wa_tela-clabs = ''.
        endif.

        clear ls_itens.
        unassign <fs_itens>.

      endif.

    endif.
* Verifica se o controle de linha inserida em andamento está marcado.
    if not lv_add_line is initial.
      cont = lines( tg_itens ).
      clear lv_add_line.

    endif.
**<<<------"138699 - NMS - FIM------>>>
    while cont < wa_tela-qtd.
      append value #(
                      matnr = wa_tela-matnr
                      charg = wa_tela-charg
                    ) to tg_itens.
      add 1 to cont.
    endwhile.

  endmethod.

  method process_before_output.
    "//set title
    me->set_title_and_status( ).

    "//screen components
    me->create_docking( ).

    "//set data
*    ME->SET_HEADER( ).
    me->set_outtab_data( ).

    "//display data
    me->display( ).

  endmethod.

  method create_docking.

  endmethod.

  method display.

    data(_fieldcatalog) = me->get_fieldcatalog( ).
    data(_layout)       = value lvc_s_layo( stylefname = 'STYLE' ).
    data(_botao_alv)    = value ui_functions( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
    data: grid_event    type ref to lcl_event_handler.

    loop at tg_itens assigning field-symbol(<itens>).
**<<<------"138699 - NMS - INI------>>>
*      IF <itens>-chargd IS INITIAL.
      if <itens>-chargd    is initial or
         <itens>-chargd(1) eq '$'.

        if <itens>-chargd is initial.
**<<<------"138699 - NMS - FIM------>>>
          <itens>-chargd = sy-tabix.
          <itens>-chargd = |${ <itens>-chargd+1(9) alpha = in }|.
**<<<------"138699 - NMS - INI------>>>
        endif.
**<<<------"138699 - NMS - FIM------>>>

        <itens>-style = value #( ( fieldname = 'CLABS_D'   style = cl_gui_alv_grid=>mc_style_disabled ) ).

      else.
        <itens>-style = value #(
                          ( fieldname = 'ZLICHA'  style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'CLABS'   style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'CLABS_D'   style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'VFDAT'   style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'DTVAL'   style = cl_gui_alv_grid=>mc_style_disabled )
                       ).
      endif.
    endloop.

    if custom_grid is not initial.
      call method grid->free.
      call method custom_grid->free.
    endif.

    create object custom_grid
      exporting
        container_name = 'CC'.

    create object grid
      exporting
        i_parent = custom_grid.

    sort tg_itens[] by charg.

    call method grid->set_table_for_first_display
      exporting
        it_toolbar_excluding = _botao_alv
        is_layout            = _layout
        i_save               = abap_true
      changing
        it_outtab            = tg_itens
        it_fieldcatalog      = _fieldcatalog.

    call method grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler: grid_event->on_data_changed for grid.

  endmethod.

  method get_fieldcatalog.

    fcat =
            value #(
                      ( fieldname = 'MATNR'  coltext = 'Material'        outputlen = 12 no_zero = abap_true )
                      ( fieldname = 'CHARG'  coltext = 'Lote NF'         outputlen = 10 )
                      ( fieldname = 'ZLICHA' coltext = 'Lote Fabricante' outputlen = 30 edit = abap_true )
                      ( fieldname = 'CLABS'  coltext = 'Volume'          outputlen = 10 edit = abap_true    ref_field = 'CLABS'     ref_table = 'MCHB'  )
                      ( fieldname = 'CLABS_D' coltext = 'Estoque'        outputlen = 10 edit = abap_true    ref_field = 'CLABS'     ref_table = 'MCHB'  )
                      ( fieldname = 'VFDAT'  coltext = 'Vencimento'      outputlen = 10 edit = abap_true )
                      ( fieldname = 'DTVAL'  coltext = 'Vencimento Real' outputlen = 15 edit = abap_true )
                      ( fieldname = 'CHARGD' coltext = 'Lote'            outputlen = 10 )
                   ).

  endmethod.

  method add_lines.
    call method grid->refresh_table_display.
  endmethod.

  method process_division.

    types: begin of ty_item_zlicha,
             zlicha type zde_lote_forn,
             clabs  type labst,
           end of ty_item_zlicha.

    data: check_erro type c,
          vl_dt_venc type sy-datum.

    sort tg_itens[] by zlicha.

    loop at tg_itens into data(wa).
**<<<------"138699 - NMS - INI------>>>
      data(lv_tabix) = sy-tabix.
**<<<------"138699 - NMS - FIM------>>>
      check_erro = abap_true.

      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input        = wa_tela-matnr
        importing
          output       = wa_tela-matnr
        exceptions
          length_error = 1
          others       = 2.

      select single * from zppt0016 into @data(wl_zppt0016)
        where   matnr   = @wa_tela-matnr  and
                werks   = @wa_tela-werks  and
                lgort   = @wa_tela-lgort  and
                zlicha  = @wa-zlicha.

      if ( wa-chargd eq wl_zppt0016-chargd ).
        clear: wl_zppt0016.
        continue.
      endif.

      if ( wl_zppt0016 is not initial ).

        if ( wl_zppt0016-vfdat ne wa-vfdat ).
          message 'Verificar data de vencimento, difere da existente para o lote de fabricante informado!' type 'S' display like 'E'.
          continue.
        endif.

        wa-chargd   = wl_zppt0016-chargd.
        wa-clabs    = wa-clabs.
        if wa-vfdat is initial.
          wa-vfdat    = wl_zppt0016-vfdat.
        endif.

        clear check_erro.
      endif.
**<<<------"138699 - NMS - INI------>>>
* Verifica se as Datas Vencimento e/ou Vencimento Real não estão preenchidas.
      if wa-vfdat is initial or
         wa-dtval is initial.
* Data é obrigatória
        message s722(wi) display like 'E'.
        gv_msg_err = abap_on.
        leave screen.

      endif.
**<<<------"138699 - NMS - FIM------>>>
      if wa-chargd(1) eq '$'.

        clear check_erro.

        data(seq) = me->get_seq( ).
        wa-chargd = seq.
        wa-chargd = |EMB{ wa-chargd+3(7) }|.

        call method create_batch
          exporting
            material       = wa_tela-matnr
            centro         = wa_tela-werks
            deposito       = wa_tela-lgort
            lote           = wa-chargd
            dt_venc_real   = wa-dtval
            dt_vencimento  = wa-vfdat
            lote_principal = wa_tela-charg
          changing
            log            = check_erro.

      endif.

      check ( check_erro is initial ).

      at_header = value #(
                        pstng_date  = sy-datum
                        header_txt  = |{ wa-zlicha }|
                     ).


      at_item = value #(
                         (
                            move_type  = '311'
                            plant      = wa_tela-werks
*                            material   = wa_tela-matnr   " >> ---> S4 Migration - 07/07/2023 - RZ
                            entry_qnt  = wa-clabs

                            stge_loc   = wa_tela-lgort
                            batch      = wa_tela-charg

                            move_stloc = wa_tela-lgort
                            move_batch = wa-chargd

                            expirydate = wa-vfdat
                          )
                       ).

* ---> S4 Migration - 07/07/2023 - RZ - Inicio
      read table at_item assigning field-symbol(<fs_item_aux>) index 1.
      if sy-subrc eq 0.

        data(v_len) = strlen( wa_tela-matnr ).

        if v_len > 18.
          <fs_item_aux>-material_long  =   wa_tela-matnr.
        else.
          <fs_item_aux>-material       =   wa_tela-matnr.
        endif.
      endif.
* <--- S4 Migration - 07/07/2023 - RZ - Fim
      do.

        call method transfer_from_batch_to_batch
          exporting
            header   = at_header
            items    = at_item
          importing
            document = _document.

        if not line_exists( l_return[ type = 'E' ] ).
          exit.
        else.
          message |{ l_return[ type = 'E' ]-message }| type 'I'.
**<<<------"138699 - NMS - INI------>>>
          exit.
**<<<------"138699 - NMS - FIM------>>>
        endif.

      enddo.

      if line_exists( l_return[ type = 'E' ] ).
        message _return-message type 'S' display like 'E'.
**<<<------"138699 - NMS - INI------>>>
*        EXIT.
        gv_msg_err = abap_on.
        leave screen.
**<<<------"138699 - NMS - FIM------>>>
      endif.

      data(wa_0016) = value zppt0016(
                             matnr  = wa_tela-matnr
                             werks  = wa_tela-werks
                             lgort  = wa_tela-lgort
                             chargd = wa-chargd
                             charg  = wa_tela-charg
                             zlicha = wa-zlicha
                             clabs  = cond string( when wl_zppt0016-zlicha = wa-zlicha then ( wa-clabs + wl_zppt0016-clabs ) else wa-clabs )
                             vfdat  = wa-vfdat
                             dtval  = wa-dtval
                          ).

      if ( wl_zppt0016 is not initial ).
        data(_msg_sucesso) = |Quantidade adicionada ao lote { wa-chargd }!|.
        message _msg_sucesso type 'S'.
      endif.

      modify zppt0016 from wa_0016.
*      UPDATE MCH1 SET VFDAT = WA-VFDAT WHERE MATNR = WA_TELA-MATNR AND CHARG = WA_TELA-CHARG.
**<<<------"138699 - NMS - INI------>>>
*      COMMIT WORK.
      if sy-subrc is initial.
        commit work.
* Atualiza a tbela do ALV com o Lote processado com sucesso.
        modify tg_itens from wa index lv_tabix transporting chargd.

      endif.
**<<<------"138699 - NMS - FIM------>>>
      clear: wl_zppt0016.

    endloop.

    clear: wa_tela, check_erro, wl_zppt0016.
    cl_main=>run( ).

  endmethod.

  method estorno_lote.

    clear: v_clabs, vl_clabs.

    call method grid->get_selected_rows
      importing
        et_index_rows = data(tl_selected_rows).

    try.
        data(_index) = tl_selected_rows[ 1 ]-index.
        w_itens = tg_itens[ _index ].
      catch cx_sy_itab_line_not_found.
    endtry.


    if lines( tl_selected_rows ) is initial.
      message 'Selecione 1 (uma) linha para realizar o Estorno!' type 'S' display like 'E'.
      exit.
    elseif lines( tl_selected_rows ) > 1.
      message 'Selecione somente 1 (uma) linha para realizar o Estorno!' type 'S' display like 'E'.
      exit.
    endif.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = wa_tela-matnr
      importing
        output       = wa_tela-matnr
      exceptions
        length_error = 1
        others       = 2.


    "Valida dados estorno material.
    select single * from  zppt0016
      into @data(ws_ZPPT0016)
      where matnr eq @w_itens-matnr
        and charg eq @w_itens-charg
        and chargd eq @w_itens-chargd.
*        and mblnr  ne @space. "BUG 153791 / AOENNING.
    if ws_zppt0016-mblnr is not initial.
      "Valida dados estorno material.
      message e024(sd) with |Impossivel estornar lançamento, | |lote gerado pela transação ZMM0110!| display like 'E'.
      exit.

      "BUG 153791 / AOENNING.
    else.
      if ws_zppt0016 is not initial and ws_zppt0016-chargd+0(3) ne 'EMB'.
        message e024(sd) with |Impossivel estornar lançamento, | |lote gerado pela transação ZMM0110!| display like 'E'.
        exit.
      endif.
      "BUG 153791 / AOENNING.
    endif.

    select single clabs
     from mchb
     into v_clabs
      where matnr eq wa_tela-matnr
        and werks eq wa_tela-werks
        and charg eq w_itens-chargd.

    if v_clabs is initial.
      message |Lote { w_itens-chargd } está sem Saldo!| type 'S' display like 'E'.
      exit.
    endif.

    call screen 0003 starting at 122 9 ending at 185 13.

    check sy-ucomm eq 'CONFIRMAR'.

    case abap_true.
      when total.
        data: p type c.
        data: check_erro, vl_answer type c,
        ls_selfield type slis_selfield.

        clear: p.

        data: cl_container type ref to cl_gui_dialogbox_container,
              obj_alv      type ref to cl_gui_alv_grid.

        check me->popup_confirm( ) eq '1'.

        call function 'CONVERSION_EXIT_MATN1_INPUT'
          exporting
            input        = w_itens-matnr
          importing
            output       = w_itens-matnr
          exceptions
            length_error = 1
            others       = 2.

        select * from zppt0011 into table @data(tl_0011)
          where matnr eq @w_itens-matnr and
                werks eq @wa_tela-werks  and
                lgort eq @wa_tela-lgort  and
                charg eq @w_itens-chargd.

        if ( tl_0011[] is not initial ).

          data(vl_msg) = |Lote { w_itens-charg } sendo utilizado, impossível estornar! |.

        else.

          select * from mseg into table @data(tl_mseg)
             where charg eq @w_itens-chargd
               and umcha eq @w_itens-charg.

          check ( tl_mseg[] is not initial ).

          loop at tl_mseg[] into data(_mseg).
            if ( _mseg-smbln is not initial ).
              data(_doc_estornado) = _mseg-smbln.
              delete tl_mseg[] where mblnr = _doc_estornado.
              delete tl_mseg[] where smbln = _doc_estornado.
            endif.
          endloop.

*          DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(
*          ( FIELDNAME = 'ZEILE'        SELTEXT_M = 'Selecionar' )
*          ( FIELDNAME = 'MBLNR'        SELTEXT_M = 'Nro.Documento' )
*          ( FIELDNAME = 'MJAHR'        SELTEXT_M = 'Ano' )
*          ( FIELDNAME = 'WERKS'        SELTEXT_M = 'Filial'     )
*          ( FIELDNAME = 'LGORT'        SELTEXT_M = 'Depósito'   )
*          ( FIELDNAME = 'CHARG'        SELTEXT_M = 'Lote'     )
*          ( FIELDNAME = 'MENGE'        SELTEXT_M = 'Quantidade' ) ).
*
*          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*            EXPORTING
*              I_TITLE              = 'Selecione o(s) documento(s) para estorno:'
*              I_SELECTION          = 'X'
*              I_ZEBRA              = 'X'
*              I_TABNAME            = 1
*              IT_FIELDCAT          = TL_FIELDCAT[]
*              I_ALLOW_NO_SELECTION = 'X'
*            IMPORTING
*              ES_SELFIELD          = LS_SELFIELD
*            TABLES
*              T_OUTTAB             = TL_MSEG[]
*            EXCEPTIONS
*              PROGRAM_ERROR        = 1
*              OTHERS               = 2.

*          IF ( LS_SELFIELD IS NOT INITIAL ).
          if tl_mseg is not initial.
            loop at tl_mseg assigning field-symbol(<l_mseg>).

*            DATA(WL_MSEG) = TL_MSEG[ LS_SELFIELD-TABINDEX ].

              call function 'BAPI_GOODSMVT_CANCEL'
                exporting
*                 MATERIALDOCUMENT = WL_MSEG-MBLNR
*                 MATDOCUMENTYEAR  = WL_MSEG-MJAHR
                  materialdocument = <l_mseg>-mblnr
                  matdocumentyear  = <l_mseg>-mjahr
                tables
                  return           = l_return[].

              if ( l_return[] is not initial ).

                data(wl_return) = l_return[ 1 ].
                message wl_return-message type 'I'.
                clear: w_itens.
                clear: p.
                p = abap_true.
                exit.

              else.

                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = abap_true.
              endif.
            endloop.

*            IF LINES( TL_MSEG ) > 1.

*              "Verificar o saldo na tabela MCHB e atualizar a tabela.
*              SELECT SINGLE CLABS
*              FROM MCHB
*              INTO @DATA(L_CLABS)
*              WHERE MATNR EQ @WA_TELA-MATNR
*              AND WERKS EQ @WA_TELA-WERKS
*              AND CHARG EQ @W_ITENS-CHARGD.
*
**              DATA(_NOVA_QUANTIDADE) = W_ITENS-CLABS - WL_MSEG-MENGE.
*              DATA(_NOVA_QUANTIDADE) = L_CLABS.  "Alterado 10/06/2020 - ANDERSON OENNING
*
*
*
*              UPDATE ZPPT0016 SET CLABS = _NOVA_QUANTIDADE
*                WHERE  MATNR       EQ WA_TELA-MATNR
*                                   AND  WERKS   EQ WA_TELA-WERKS
*                                   AND  LGORT   EQ WA_TELA-LGORT
*                                   AND  CHARGD  EQ W_ITENS-CHARGD
*                                   AND  CHARG   EQ WA_TELA-CHARG.
*            ELSE.
            if p is initial.

              call function 'CONVERSION_EXIT_MATN1_INPUT'
                exporting
                  input        = wa_tela-matnr
                importing
                  output       = wa_tela-matnr
                exceptions
                  length_error = 1
                  others       = 2.


              delete from zppt0016 where  matnr   eq wa_tela-matnr
                                     and  werks   eq wa_tela-werks
                                     and  lgort   eq wa_tela-lgort
                                     and  chargd  eq w_itens-chargd
                                     and  charg   eq wa_tela-charg.


*            IF ( SY-SUBRC EQ 0 ).
              message 'Estorno realizado com sucesso!' type 'S'.
            endif.

          endif.

        endif.
        clear: w_itens, vl_msg, tl_0011, p.", WL_MSEG.

      when parcial.

        data document  type bapi2017_gm_head_ret.

        _index = tl_selected_rows[ 1 ]-index.
        w_itens = tg_itens[ _index ].

        data(_header) =
        value bapi2017_gm_head_01(
                      pstng_date  = sy-datum
                      doc_date    = w_itens-vfdat
                      header_txt  = 'Devolução de Valor'
                    ).

*---> 16/06/2023 - Migração S4 - DG
        data(v_len) = strlen( w_itens-matnr ).

        if v_len > 18.
          data(lv_material_long) = w_itens-matnr .
        else.
          data(lv_material)      = w_itens-matnr .
        endif.
*<--- 16/06/2023 - Migração S4 - DG

        data(_item) =
        value bapi2017_gm_item_create_t(
                   (
                      move_type  = '309'
                      plant      = wa_mchb-werks
*---> 16/06/2023 - Migração S4 - DG
                      "material   = w_itens-matnr
                      material       = lv_material
                      material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                      entry_qnt  = vl_clabs

                      stge_loc   = wa_mchb-lgort
                      batch      = w_itens-chargd

                      move_stloc = wa_mchb-lgort
                      move_batch = w_itens-charg
                   )
                 ).

        call function 'BAPI_GOODSMVT_CREATE'  "#EC CI_USAGE_OK[2438131]
          exporting
            goodsmvt_header  = _header
            goodsmvt_code    = '06'
          importing
            materialdocument = document-mat_doc
            matdocumentyear  = document-doc_year
          tables
            goodsmvt_item    = _item
            return           = l_return.

        if ( document-mat_doc is not initial ).
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = abap_true.

          call function 'CONVERSION_EXIT_MATN1_INPUT'
            exporting
              input        = w_itens-matnr
            importing
              output       = w_itens-matnr
            exceptions
              length_error = 1
              others       = 2.

          "Verificar o saldo na tabela MCHB e atualizar a tabela.
          select single clabs
          from mchb
          into @data(l_clabs)
          where matnr eq @w_itens-matnr
          and werks eq @wa_mchb-werks
          and charg eq @w_itens-chargd.


          data: _nova_quantidade type mchb-clabs.
*          CLEAR: _NOVA_QUANTIDADE.
*          DATA(_nova_quantidade) = l_clabs.   "Alteração 10/06/2020 - ANDERSON OENNING

          _nova_quantidade = w_itens-clabs - vl_clabs. "// IR062775 wsb

          update zppt0016 set clabs = _nova_quantidade
                 where  matnr   eq w_itens-matnr
                   and  werks   eq wa_mchb-werks
                   and  lgort   eq wa_mchb-lgort
                   and  chargd  eq w_itens-chargd
                   and  charg   eq w_itens-charg.

          clear: l_clabs.

        endif.

    endcase.

    cl_main=>run( ).

  endmethod.

  method popup_confirm.

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = 'Confirmação de Estorno'
        text_question         = 'Deseja estornar o(s) lote(s) selecionado(s)?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = 'X'
      importing
        answer                = e_answer.

  endmethod.

  method create_batch.

    data dt_venc   type string.
    data new_batch type table of mcha.
    data: lv_material type mara-matnr.


    clear log.

    select single *
      from mch1
      into @data(wa_mch1)
      where charg eq @lote_principal.

    try.
        cl_abap_datfm=>conv_date_int_to_ext( exporting im_datint = dt_vencimento importing ex_datext = dt_venc ).
      catch cx_abap_datfm_format_unknown.
    endtry.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = material
      importing
        output       = lv_material
      exceptions
        length_error = 1
        others       = 2.

    data(_header)
      = value mcha(
                    matnr = lv_material
                    werks = centro
                    charg = lote
                    hsdat = wa_mch1-hsdat
                    vfdat = dt_vencimento
                  ).

    data(_characteristics)
      = value clbatch_t(
                         ( atnam = 'ZDEFENSIVO_DT_VALIDADE'     atwtb = dt_venc )
                         ( atnam = 'ZDEFENSIVO_LOTE_FABRICANTE' atwtb = lote_principal )
                       ).

    call function 'VB_CREATE_BATCH'
      exporting
        ymcha                        = _header
        new_lgort                    = deposito
        kzcla                        = '2'
        class                        = 'ZDEFENSIVO'
        no_cfc_calls                 = 'X'
      importing
        ymcha                        = _header
      tables
        char_of_batch                = _characteristics
        new_batch                    = new_batch
        return                       = l_return
      exceptions
        no_material                  = 1
        no_batch                     = 2
        no_plant                     = 3
        material_not_found           = 4
        plant_not_found              = 5
        stoloc_not_found             = 6
        lock_on_material             = 7
        lock_on_plant                = 8
        lock_on_batch                = 9
        lock_system_error            = 10
        no_authority                 = 11
        batch_exist                  = 12
        stoloc_exist                 = 13
        illegal_batch_number         = 14
        no_batch_handling            = 15
        no_valuation_area            = 16
        valuation_type_not_found     = 17
        no_valuation_found           = 18
        error_automatic_batch_number = 19
        cancelled                    = 20
        wrong_status                 = 21
        interval_not_found           = 22
        number_range_not_extern      = 23
        object_not_found             = 24
        error_check_batch_number     = 25
        no_external_number           = 26
        no_customer_number           = 27
        no_class                     = 28
        error_in_classification      = 29
        inconsistency_in_key         = 30
        region_of_origin_not_found   = 31
        country_of_origin_not_found  = 32
        others                       = 33.

    if ( sy-subrc is not initial ).
      call function 'BALW_BAPIRETURN_GET2'
        exporting
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        importing
          return = _return.

      log = abap_true.
    else.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    endif.

  endmethod.

  method transfer_from_batch_to_batch.

    "Campos de material tratados. Pseudo comentário adicionado     " >> ---> S4 Migration - 07/07/2023 - RZ
    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
      exporting
        goodsmvt_header  = header
        goodsmvt_code    = '06'
      importing
        materialdocument = document-mat_doc
        matdocumentyear  = document-doc_year
      tables
        goodsmvt_item    = at_item "items
        return           = l_return.

    if ( document-mat_doc is not initial ).

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.

    else.

      call function 'BALW_BAPIRETURN_GET2'
        exporting
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        importing
          return = _return.

      call function 'BAPI_TRANSACTION_ROLLBACK'.

    endif.
  endmethod.


  method get_seq.

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '1'
        object                  = 'ZSEQ_EBM'
      importing
        number                  = return
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        others                  = 8.

  endmethod.

endclass.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module main_pbo output.
**<<<------"138699 - NMS - INI------>>>
* Valida se no "Processamento" ocorreu alguma mesagem de ewrro
* para não fazer o REFRESH do ALV no intuito de não perder a
* o dado da linha que foi inserida.
  if gv_msg_err is initial.
**<<<------"138699 - NMS - FIM------>>>
    if r_main is initial.
      create object r_main.
    endif.

    r_main->process_before_output( ).
**<<<------"138699 - NMS - INI------>>>
  else.
    clear gv_msg_err.
    call method grid->refresh_table_display.

  endif.
**<<<------"138699 - NMS - FIM------>>>
endmodule.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module main_pai input.
  case sy-ucomm.
    when 'BNTPRO'.
      r_main->process_division( ).
    when 'BTNEST'.
      r_main->estorno_lote( ).
    when 'BACK'.
      leave to current transaction.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  QTD_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module qtd_lote input.
  r_main->add_lines( ).
endmodule.

start-of-selection.
  cl_main=>run( ).


*&---------------------------------------------------------------------*
*&      Form  F4_S_CHARG
*&---------------------------------------------------------------------*
form f4_s_charg.

  select charg from zppt0016 into table @data(tl_zppt0016) order by charg ascending.
  delete adjacent duplicates from tl_zppt0016[] comparing charg.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'CHARG'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_CHARG'
      value_org   = 'S'
    tables
      value_tab   = tl_zppt0016[].


endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0002 output.
  set titlebar 'TITLE_0002'.
  set pf-status 'STATUS_0002'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0002 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0003 output.
  set titlebar 'TITLE_0003'.
  set pf-status 'STATUS_0003'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0003 input.
  case sy-ucomm.
    when 'CANCELAR' or 'CONFIRMAR'.
      leave to screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module screen_0003 output.

  loop at screen.
    if screen-name eq 'VL_CLABS' and total is not initial.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  QTD_CLABS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module qtd_clabs input.
  if vl_clabs > v_clabs.
    message |Lote { w_itens-chargd } está com Saldo insuficiente!| type 'S' display like 'E'.
  endif.
endmodule.
