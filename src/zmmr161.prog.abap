*&---------------------------------------------------------------------*
*& Report  ZMMR161
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmmr161.

tables: zsdt0001,lfa1.
tables: icon.
type-pools : icon.

"Types
types: begin of ty_saida,
         status            type v_icon-name,
         parid             type zsdt0001-parid,
         name1             type lfa1-name1,
         matnr             type zsdt0001-matnr,
         maktx             type makt-maktx,
         ebeln(10)         type c, "EKPO-EBELN,
         ebelp             type ekpo-ebelp,
         waers             type ekko-waers,
         bvtyp             type lfbk-bvtyp,
         dt_movimento      type zsdt0001-dt_movimento,
         docdat            type zsdt0001-docdat,
         dt_vencimento     type zmmt_ee_zgr-dt_vencimento,
         peso_fiscal       type zsdt0001-peso_fiscal,
         peso_liq          type zsdt0001-peso_liq,
         meins             type ekpo-meins,
         nfnum_s(13)       type c, "zsdt0001-nfnum e ZSDT0001-SERIES
         nfnum             type zmmt0132-nfnum,
         series            type zmmt0132-series,
         netwr             type zsdt0001-netwr,
         icms_valor        type zib_nfe_dist_itm-icms_valor,
         ipi_valor         type zib_nfe_dist_itm-ipi_valor,
         pis_valor         type zib_nfe_dist_itm-pis_valor,
         cof_valor         type zib_nfe_dist_itm-cof_valor,
         mwskz             type ekpo-mwskz,
         mm_mblnr          type zmmt_ee_zgr_docs-mm_mblnr,
         mm_mjahr          type zmmt_ee_zgr_docs-mm_mjahr,
         ft_belnr          type zmmt_ee_zgr_docs-ft_belnr,
         ft_gjahr          type zmmt_ee_zgr_docs-ft_gjahr,
         docnum            type zmmt_ee_zgr_docs-docnum,
         icms_base         type zib_nfe_dist_itm-icms_base,
         icms_aqt          type zib_nfe_dist_itm-icms_aqt,
         icms_red_base     type zib_nfe_dist_itm-icms_red_base,
         chave_nfe         type zsdt0001-chave_nfe,
         ch_referencia(29) type c,
         lgort             type ekpo-lgort,
         webre             type ekpo-webre,
         charg             type eket-charg,
         zterm             type ekko-zterm,
         txjcd             type ekpo-txjcd,
         txz01             type ekpo-txz01,
         erro_log(255)     type c,
         obj_key           type zmmt_ee_zgr_docs-obj_key,
         nfnum2            type zmmt_ee_zgr-nfnum,
         authcod           type zmmt_ee_zgr-authcod,
         nfenum            type zmmt_ee_zgr-nfenum,
         docstat           type zmmt_ee_zgr-docstat,
         cdv               type zmmt_ee_zgr-cdv,
         amount_lc         type zmmt0132-amount_lc,
         j_1bnftype        type zmmt0132-j_1bnftype,
         exch_rate_v       type zmmt0132-exch_rate_v,
         celltab           type lvc_t_styl,
       end of ty_saida.


types: begin of ty_ped_compra,
         parid type zsdt0001-parid,
         name1 type lfa1-name1,
         ebeln type ekpo-ebeln,
         ebelp type ekpo-ebelp,
         waers type ekko-waers,
         matnr type ekpo-matnr,
         meins type ekpo-meins,
         lgort type ekpo-lgort,
         charg type eket-charg,
         txjcd type ekpo-txjcd,
         txz01 type ekpo-txz01,
         menge type ekpo-menge,
         mwskz type ekpo-mwskz,
         webre type ekpo-webre,
       end of   ty_ped_compra.

types: begin of ty_ekbe_tot,
         ebeln type ekbe-ebeln,
         ebelp type ekbe-ebelp,
         menge type ekbe-menge,
       end of ty_ekbe_tot.

types: begin of ty_ekpo_tot,
         ebeln type ekpo-ebeln,
         ebelp type ekpo-ebelp,
         menge type ekpo-menge,
         matnr type ekpo-matnr,
         txz01 type ekpo-txz01,
         mwskz type ekpo-mwskz,
         webre type ekpo-webre,
         meins type ekpo-meins,
         lgort type ekpo-lgort,
       end of ty_ekpo_tot,

       begin of ty_j1bnfdoc,
         belnr  type j_1bnfdoc-belnr,
         gjahr  type j_1bnfdoc-gjahr,
         docnum type j_1bnfdoc-docnum,
       end of ty_j1bnfdoc.

"Tabela Interna Global
data: git_zsdt0001         type table of zsdt0001,
      git_zmmt0132_aux     type table of zmmt0132,
      git_zmmt_ee_zgr_docs type table of zmmt_ee_zgr_docs,
      git_zib_nfe_dist_itm type table of zib_nfe_dist_itm,
      git_ekko             type table of ekko,
      git_ekpo             type table of ekpo,
      git_ekpo_aux         type table of ekpo,
      git_ekpo_tot         type table of ty_ekpo_tot,
      git_eket             type table of eket,
      git_ekbe             type table of ekbe,
      git_ekbe_tot         type table of ty_ekbe_tot,
      git_lfa1             type table of lfa1,
      git_makt             type table of makt,
      git_saida            type table of ty_saida,
      git_ped_compra       type table of ty_ped_compra,
      git_zmmt_ee_zgr      type table of zmmt_ee_zgr,
      git_zob_mensagem     type table of zob_mensagem,
      git_outreturn        type table of zfie_ret_document,
      git_filtro           type zif_screen_linha_filtro_t,
      git_item             type table of bapi2017_gm_item_create,
      git_serialnumber     type table of bapi2017_gm_serialnumber,
      git_return           type table of bapiret2,
      git_taxdata          type table of bapi_incinv_create_tax,
      git_withtaxdata      type table of bapi_incinv_create_withtax,
      git_itemdata         type table of bapi_incinv_create_item,
      git_glaccountdata    type table of bapi_incinv_create_gl_account,
      git_bdc              type table of bdcdata,
      git_j_1bnfdoc        type table of ty_j1bnfdoc,
      git_zmmt0179         type table of zmmt0179, "MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo
      t_forn_block         type standard table of  rgsb4 with header line.


"Work Área
data: gwa_zsdt0001         type zsdt0001,
      gwa_zmmt0132_aux     type zmmt0132,
      gwa_zmmt_ee_zgr      type zmmt_ee_zgr,
      gwa_zmmt_ee_zgr_docs type zmmt_ee_zgr_docs,
      gwa_zib_nfe_dist_itm type zib_nfe_dist_itm,
      gwa_ekko             type ekko,
      gwa_ekpo             type ekpo,
      gwa_ekpo_aux         type ekpo,
      gwa_ekpo_tot         type ty_ekpo_tot,
      gwa_eket             type eket,
      gwa_ekbe             type ekbe,
      gwa_ekbe_tot         type ty_ekbe_tot,
      gwa_lfa1             type lfa1,
      gwa_makt             type makt,
      gwa_saida            type ty_saida,
      gwa_saida_aux        type ty_saida,
      gwa_ped_compra       type ty_ped_compra,
      gwa_zmmt0132         type zmmt0132,
      gwa_zob_mensagem     type zob_mensagem,
      gwa_header           type bapi2017_gm_head_01,
      gwa_item             type bapi2017_gm_item_create,
      gwa_serialnumber     type bapi2017_gm_serialnumber,
      gwa_mat_doc          type bapi2017_gm_head_ret-mat_doc,
      gwa_return           type bapiret2,
      gwa_taxdata          type bapi_incinv_create_tax,
      gwa_withtaxdata      type bapi_incinv_create_withtax,
      gwa_headerdata       type bapi_incinv_create_header,
      gwa_headerdata_local type bapi_incinv_create_header,
      gwa_itemdata         type bapi_incinv_create_item,
      gwa_outreturn        type zfie_ret_document,
      gwa_bdc              type bdcdata,
      gwa_stable           type lvc_s_stbl value 'XX'.


"Variáveis
data: gva_obj_key               type zmmt_ee_zgr_docs-obj_key,
      gva_docnum                type j_1bnflin-docnum,
      gva_invoicedocnumber_migo type bapi2017_gm_head_ret,
      gva_invoicedocnumber_miro type bapi_incinv_fld-inv_doc_no,
      gva_ano_migo              type bapi2017_gm_head_ret-doc_year,
      gva_ano_miro              type bapi_incinv_fld-fisc_year,
      gva_gmcode                type bapi2017_gm_code,
      gva_erro                  type c,
      gva_message               type symsgv,
      gva_message_v1            type symsgv,
      gva_message_v2            type symsgv,
      gva_interface(2)          type c,
      gva_wrefkey               type j_1bnflin-refkey,
      gva_lifnr                 type ekko-lifnr,
      gva_witht                 type lfbw-witht,
      gva_total_item            type zmmt_eeiva_zgr-vlr_imp,
      gva_return                type string,
      gva_vr_percepcoes         type zmmt_eeiva_zgr-vlr_imp,
      gva_error_in_data         type c,
      gv_ucomm                  type sy-ucomm.

"Objetos
data:
  git_fcat         type lvc_t_fcat,
  git_fieldcat     type slis_t_fieldcat_alv,
  gob_gui_alv_grid type ref to cl_gui_alv_grid,
  wa_stable        type lvc_s_stbl.

"Ranges
ranges:  gra_obj_key for zmmt_ee_zgr_docs-obj_key,
         gra_matkl   for ekpo-matkl.

constants: c_lucas type j_1bbranc_ value '0155'.

" Classe
class lcl_event_receiver definition deferred.
class gcl_alv_events definition deferred.

data:  event_receiver   type ref to lcl_event_receiver.
data:  alv_events       type ref to gcl_alv_events.

class lcl_event_receiver definition.
  public section.
    methods:
      handle_toolbar
        for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,

      handle_user_command
        for event user_command of cl_gui_alv_grid
        importing e_ucomm,
      hotspot_click
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no ,

      handle_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed,

      semantic_checks  importing pr_data_changed type ref to cl_alv_changed_data_protocol.

endclass.

class gcl_alv_events definition.
  public section.
    methods:
      on_f4
        for event onf4 of cl_gui_alv_grid
        importing e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.
endclass.

class lcl_event_receiver implementation.

  method handle_toolbar.

    data: ls_toolbar  type stb_button.

    clear ls_toolbar.
    move 3 to ls_toolbar-butn_type.
    append ls_toolbar to e_object->mt_toolbar.

* icone e novo botão.
    clear ls_toolbar.
    move 'GMIRMIG'                to ls_toolbar-function.
    move icon_generate            to ls_toolbar-icon.
    move 'Gerar MIGO e MIRO'(111) to ls_toolbar-quickinfo.
    move 'Gerar MIGO e MIRO'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.

    clear ls_toolbar.
    move 'REFRESH'      to ls_toolbar-function.
    move icon_refresh   to ls_toolbar-icon.
    move 'Refresh'(111) to ls_toolbar-quickinfo.
    move 'Refresh'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.


    clear ls_toolbar.
    move 'EMIRMIG'                   to ls_toolbar-function.
    move icon_system_undo            to ls_toolbar-icon.
    move 'Estornar MIGO e MIRO'(111) to ls_toolbar-quickinfo.
    move 'Estornar MIGO e MIRO'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.

  endmethod.
  method handle_user_command.
    data: lit_rows type lvc_t_row.
    wa_stable-row        = 'X'.
    wa_stable-col        = 'X'.
    case e_ucomm.
      when 'GMIRMIG'.

        gv_ucomm = e_ucomm.

        call method gob_gui_alv_grid->if_cached_prop~set_prop
          exporting
            propname  = 'GridModified'
            propvalue = '1'.

        call method gob_gui_alv_grid->check_changed_data.

        if gva_error_in_data is initial.
          call method gob_gui_alv_grid->get_selected_rows
            importing
              et_index_rows = lit_rows.

          perform fm_miro_migo tables lit_rows.
*          PERFORM fm_get_docs TABLES lit_rows.
          perform fm_start_of_selection .

          perform fm_style_celltab.

          call method gob_gui_alv_grid->refresh_table_display
            exporting
              is_stable = wa_stable.

        endif.
      when 'REFRESH'.
*        PERFORM fm_get_docs TABLES lit_rows.
        perform fm_start_of_selection .
        perform fm_style_celltab.
        call method gob_gui_alv_grid->refresh_table_display
          exporting
            is_stable = wa_stable.

      when 'EMIRMIG'.

        call method gob_gui_alv_grid->if_cached_prop~set_prop
          exporting
            propname  = 'GridModified'
            propvalue = '1'.

        call method gob_gui_alv_grid->check_changed_data.

        if gva_error_in_data is initial.
          call method gob_gui_alv_grid->get_selected_rows
            importing
              et_index_rows = lit_rows.

          perform fm_miro_migo_estorno tables lit_rows.
          perform fm_dados_seleciona.
          perform fm_dados_processa.
*          PERFORM fm_get_docs TABLES space.

          perform fm_style_celltab.

          call method gob_gui_alv_grid->refresh_table_display
            exporting
              is_stable = wa_stable.

        endif.

    endcase.
  endmethod.
  method hotspot_click.
    wa_stable-row        = 'X'.
    wa_stable-col        = 'X'.
    case e_column_id.
      when 'EBELN'.
        perform get_ped_compra using e_row_id
                                     e_column_id.
      when 'MM_MBLNR'.
        data lva_opt type ctu_params.
        clear: git_bdc,
               gwa_bdc.

        read table git_saida into data(lwa_saida_migo) index e_row_id.

        check not lwa_saida_migo-mm_mblnr is initial.

*        PERFORM f_preencher_dynpro USING:
*                'X' 'SAPLMIGO'            '0001',
*                ' ' 'BDC_OKCODE'          '=MIGO_OK_GO',
*                ' ' 'BDC_SUBSCR'          'SAPLMIGO',
*                ' ' 'GODYNPRO-ACTION'     'A04',
*                ' ' 'GODYNPRO-REFDOC'     'R02',
*                ' ' 'BDC_SUBSCR'          'SAPLMIGO',
*                ' ' 'BDC_CURSOR'          'GODYNPRO-MAT_DOC',
*                ' ' 'GODYNPRO-MAT_DOC'    lwa_saida_migo-mm_mblnr,
*                ' ' 'GODYNPRO-DOC_YEAR'   lwa_saida_migo-mm_mjahr,
*                ' ' 'BDC_SUBSCR'          'SAPLMIGO'.
*
*        lva_opt-dismode = 'E'.
*        lva_opt-defsize = ' '.
*
*        CALL TRANSACTION 'MIGO' USING git_bdc OPTIONS FROM lva_opt.

        call function 'MIGO_DIALOG'
          exporting
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = lwa_saida_migo-mm_mblnr
            i_mjahr             = lwa_saida_migo-mm_mjahr
          exceptions
            illegal_combination = 1
            others              = 2.

      when 'FT_BELNR'.

        read table git_saida into data(lwa_saida_miro) index e_row_id.

        set parameter id  'RBN' field lwa_saida_miro-ft_belnr.
        set parameter id  'GJR' field lwa_saida_miro-ft_gjahr.
        call transaction  'MIR4' and skip first screen.
    endcase.

    call method gob_gui_alv_grid->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.
  method handle_data_changed.
    perform check_data using er_data_changed.
    call method semantic_checks( er_data_changed ).
  endmethod.
  method semantic_checks.
    data: ls_good       type lvc_s_modi.

    data: lit_rows type lvc_t_row.
    data: lwa_selected_line type lvc_s_row,
          lf_row_index      type int4,
          lv_lucas          type c.

    data: l_dt_vencimento type ty_saida-dt_vencimento,
          l_bvtyp         type ty_saida-bvtyp,
          l_amount_lc     type ty_saida-amount_lc,
          l_exch_rate_v   type ty_saida-exch_rate_v,
          l_j_1bnftype    type ty_saida-j_1bnftype,
          l_nfnum         type ty_saida-nfnum_s,
          l_waers         type ty_saida-waers.


    clear: gva_error_in_data.

    call method gob_gui_alv_grid->get_selected_rows
      importing
        et_index_rows = lit_rows.

    loop at lit_rows into lwa_selected_line.

      lf_row_index = lwa_selected_line-index.

      read table git_saida into gwa_saida index lf_row_index.

      perform z_check_lucas changing lv_lucas.

      if lv_lucas = abap_true.
        if gwa_saida-lgort is initial.
          call method pr_data_changed->add_protocol_entry
            exporting
              i_msgid     = '0K'
              i_msgno     = '000'
              i_msgty     = 'E'
              i_msgv1     = 'Campo Obrigatório'
              i_fieldname = 'LGORT'
              i_row_id    = lf_row_index.

          gva_error_in_data = 'X'.
        endif.
        continue.
      endif.

      if  gwa_saida-waers = 'USD'.

        if gwa_saida-amount_lc is initial.

          call method pr_data_changed->add_protocol_entry
            exporting
              i_msgid     = '0K'
              i_msgno     = '000'
              i_msgty     = 'E'
              i_msgv1     = 'Campo Obrigatório'
              i_fieldname = 'AMOUNT_LC'
              i_row_id    = lf_row_index.

          gva_error_in_data = 'X'.

        endif.

        if gwa_saida-exch_rate_v is initial.

          call method pr_data_changed->add_protocol_entry
            exporting
              i_msgid     = '0K'
              i_msgno     = '000'
              i_msgty     = 'E'
              i_msgv1     = 'Campo Obrigatório'
              i_fieldname = 'EXCH_RATE_V'
              i_row_id    = lf_row_index.

          gva_error_in_data = 'X'.
        endif.
      endif.

      if  gwa_saida-bvtyp is initial.
        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'
            i_fieldname = 'BVTYP'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      endif.

      if gwa_saida-dt_vencimento is initial.

        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'
            i_fieldname = 'DT_VENCIMENTO'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.

      elseif gwa_saida-dt_vencimento < gwa_saida-dt_movimento and
             gv_ucomm eq 'GMIRMIG'.

        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = 'ZFI'
            i_msgno     = '123'
            i_msgty     = 'E'
            i_msgv1     = ''
            i_fieldname = 'DT_VENCIMENTO'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.

      endif.

      if gwa_saida-j_1bnftype is initial.

        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'
            i_fieldname = 'J_1BNFTYPE'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      endif.

      if gwa_saida-nfnum_s is initial.
        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'
            i_fieldname = 'NFNUM_S'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      endif.

      if gwa_saida-ebeln eq icon_icon_list.
        call method pr_data_changed->add_protocol_entry
          exporting
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'
            i_fieldname = 'EBELN'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      else.
        select count(*) from ekko where ebeln = gwa_saida-ebeln.
        if sy-subrc ne 0.
          call method pr_data_changed->add_protocol_entry
            exporting
              i_msgid     = '0K'
              i_msgno     = '000'
              i_msgty     = 'E'
              i_msgv1     = 'Pedido inválido'
              i_fieldname = 'EBELN'
              i_row_id    = lf_row_index.

          gva_error_in_data = 'X'.
        endif.
      endif.
    endloop.

    if gva_error_in_data = 'X'.
      call method pr_data_changed->display_protocol.
    endif.

  endmethod.
endclass.

class gcl_alv_events implementation.
  method on_f4.
    perform on_f4 using e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  endmethod.
endclass.

class cl_salv_pop_up definition .

  public section .

    class-data: ob_salv_table type ref to cl_salv_table.

    class-data: begin of st_double_click .
    class-data: row    type salv_de_row,
                column type salv_de_column.
    class-data: end of st_double_click .

    class-methods: popup
      importing
        start_line     type i default 2
        end_line       type i default 15
        start_column   type i default 25
        end_column     type i default 150
        popup          type boolean default ' '
        value(t_table) type table .

    class-methods: double_click for event double_click of cl_salv_events_table
      importing row column,

      on_link_click for event link_click of cl_salv_events_table
        importing row column.

endclass.                    "my_cl_salv_pop_up DEFINITION
class cl_salv_pop_up implementation .
  method popup .
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = ob_salv_table
          changing
            t_table      = t_table ).

      catch cx_salv_msg.
    endtry.

    check ob_salv_table is bound.

*...HotSpot
    data: lo_cols_tab type ref to cl_salv_columns_table,
          lo_col_tab  type ref to cl_salv_column_table.

*** Selections
    data: lr_selections type ref to cl_salv_selections,
          lr_columns    type ref to cl_salv_columns_table.

    data: gr_events type ref to cl_salv_pop_up.

    lo_cols_tab = ob_salv_table->get_columns( ).

    try.
        lo_col_tab ?= lo_cols_tab->get_column( 'EBELN' ).
      catch cx_salv_not_found.
    endtry.

    try.
        call method lo_col_tab->set_cell_type
          exporting
            value = if_salv_c_cell_type=>hotspot.
      catch cx_salv_data_error .
    endtry.
*** End Hot Spot.

    ob_salv_table->set_screen_popup(
      start_column = start_column
      end_column   = end_column
      start_line   = start_line
      end_line     = end_line ).

    data: ob_salv_events    type ref to cl_salv_events_table.
    ob_salv_events = ob_salv_table->get_event( ).

    set handler double_click      for ob_salv_events .
    set handler on_link_click     for ob_salv_events.

* Column selection
    lr_selections = ob_salv_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

    lr_columns = ob_salv_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    ob_salv_table->display( ) .

* GET THE SELECTED ROWS.
    data: lwa_rows type int4,
          lt_rows  type salv_t_row.

    lt_rows = lr_selections->get_selected_rows( ).
    clear: gwa_ped_compra.
* Display the selected rows.
    loop at lt_rows into lwa_rows.
      read table git_ped_compra into gwa_ped_compra index lwa_rows.
    endloop.
  endmethod .                    "popup
  method on_link_click .
    st_double_click-row    = row .
    st_double_click-column = column .

    read table git_ped_compra into gwa_ped_compra index row.

    set parameter id 'BES' field gwa_ped_compra-ebeln.
    call transaction 'ME23N' and skip first screen.

  endmethod .                    "raise_double_click
  method double_click .

  endmethod .
endclass.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      SELECTION SCREEN
*&---------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
  select-options: s_bukrs  for zsdt0001-bukrs        no-extension no
  intervals obligatory,
                  s_branch for zsdt0001-branch       no-extension no
                  intervals obligatory,
                  s_parid  for lfa1-lifnr       no intervals no-extension,
                  s_dtmov  for zsdt0001-dt_movimento no-extension obligatory.
selection-screen: end of block b1.

start-of-selection.
  perform fm_start_of_selection.

end-of-selection.
  perform fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
form fm_start_of_selection .
  perform fm_dados_seleciona.
  if sy-batch eq abap_true.
    perform fm_dados_processa_bg.
    perform fm_miro_migo_bg.
  else.
    perform fm_dados_processa.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
form fm_dados_seleciona .

  data lit_matkl   type standard table of  rgsb4 with header line.
  data lr_matnr type range of matnr.

  perform fm_clear_data.

  if s_parid-low is not initial.
    select *
      from zsdt0001
      inner join mara on mara~matnr = zsdt0001~matnr
      inner join zmmt0176
        on  zmmt0176~branch = zsdt0001~branch
        and zmmt0176~matkl  = mara~matkl
      into corresponding fields of table git_zsdt0001
     where tp_movimento eq 'E'
      and zsdt0001~dt_movimento in s_dtmov
      and zsdt0001~bukrs   eq s_bukrs-low
      and zsdt0001~branch  eq s_branch-low
      and zsdt0001~parid   eq s_parid-low.

  else.
    if s_branch-low eq c_lucas.
      select *
      from zsdt0001
        inner join mara on mara~matnr = zsdt0001~matnr
        inner join zmmt0176
          on  zmmt0176~branch = zsdt0001~branch
          and zmmt0176~matkl  = mara~matkl
        into corresponding fields of table git_zsdt0001
       where tp_movimento eq 'E'
        and zsdt0001~dt_movimento in s_dtmov
        and zsdt0001~bukrs   eq s_bukrs-low
        and zsdt0001~branch  eq s_branch-low.
    else.
      select *
      from zsdt0001
        inner join mara on mara~matnr = zsdt0001~matnr
        inner join zmmt0176
          on  zmmt0176~branch = zsdt0001~branch
          and zmmt0176~matkl  = mara~matkl
        into corresponding fields of table git_zsdt0001
       where tp_movimento eq 'E'
        and zsdt0001~dt_movimento in s_dtmov
        and zsdt0001~bukrs   eq s_bukrs-low
        and zsdt0001~branch  eq s_branch-low
        and zsdt0001~parid   ne '0000000175'.
    endif.
  endif.

  if git_zsdt0001 is not initial.
    clear gra_obj_key.
    loop at git_zsdt0001 into gwa_zsdt0001.
      gra_obj_key-sign = 'I'.
      gra_obj_key-option = 'EQ'.
      concatenate gwa_zsdt0001-ch_referencia  gwa_zsdt0001-nr_romaneio
      into gra_obj_key-low.
      append gra_obj_key.
    endloop.

    select * from zmmt0132
      into table git_zmmt0132_aux
     where obj_key in gra_obj_key.
    if sy-subrc is initial.
      data(lt_zmmt0132) = git_zmmt0132_aux.

      sort lt_zmmt0132 by ft_belnr ft_gjahr.
      delete adjacent duplicates from lt_zmmt0132 comparing ft_belnr ft_gjahr.

*      SELECT belnr gjahr docnum
*        FROM j_1bnfdoc
*        INTO TABLE git_j_1bnfdoc
*        FOR ALL ENTRIES IN lt_zmmt0132
*        WHERE belnr = lt_zmmt0132-ft_belnr
*          AND gjahr = lt_zmmt0132-ft_gjahr.
*      IF sy-subrc IS INITIAL.
*        SORT git_j_1bnfdoc BY belnr gjahr.
*      ENDIF.

    endif.

    select * from zmmt_ee_zgr_docs
      into table git_zmmt_ee_zgr_docs
     where obj_key in gra_obj_key.

    select *
       from zmmt_ee_zgr
       into table git_zmmt_ee_zgr
      where zrg_atlz      eq '1'
        and  tp_operacao  eq '1'
        and obj_key       in gra_obj_key.

    select * from zib_nfe_dist_itm
      into table git_zib_nfe_dist_itm
      for all entries in git_zsdt0001
      where chave_nfe  eq git_zsdt0001-chave_nfe.

    select * from ekko
      inner join ekpo on ekpo~ebeln = ekko~ebeln
      inner join zmmt0176
          on  zmmt0176~branch = ekpo~werks
          and zmmt0176~matkl  = ekpo~matkl
      into corresponding fields of table git_ekko
      for all entries in git_zsdt0001
      where lifnr  eq git_zsdt0001-parid.

    "parceiro
    select * from ekko
      inner join ekpo on ekpo~ebeln = ekko~ebeln
      inner join zmmt0176
          on  zmmt0176~branch = ekpo~werks
          and zmmt0176~matkl  = ekpo~matkl
      appending corresponding fields of table git_ekko
      for all entries in git_zsdt0001
      where llief  eq git_zsdt0001-parid.
    "parceiro ALRS

    sort  git_ekko by ebeln.
    delete adjacent duplicates from git_ekko comparing ebeln.
    if git_ekko is not initial.
      select * from ekpo
        into table git_ekpo
        for all entries in git_ekko
        where ebeln  eq git_ekko-ebeln.

      if git_ekpo is not initial.
        select * from eket
          into table git_eket
          for all entries in git_ekpo
          where ebeln  eq git_ekpo-ebeln
          and ebelp eq git_ekpo-ebelp.

        select * from ekbe
          into table git_ekbe
          for all entries in git_ekpo
          where ebeln  eq git_ekpo-ebeln
          and ebelp eq git_ekpo-ebelp
          and vgabe eq '1'.
      endif.
    endif.

    select * from lfa1
      into table git_lfa1
      for all entries in git_zsdt0001
      where lifnr  eq git_zsdt0001-parid.

    select * from makt
     into table git_makt
     for all entries in git_zsdt0001
     where matnr  eq git_zsdt0001-matnr
      and spras eq 'PT'.

*>>>Begin-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo

    lr_matnr = value #( for ls_zsdt0001 in git_zsdt0001
                        ( sign   = 'I'
                          option = 'EQ'
                          low    = |{ ls_zsdt0001-matnr alpha = out }| ) ).

    select *
      from zmmt0179
      into table git_zmmt0179
      where matnr in lr_matnr.
*<<<End-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo

  else.
    message
    'Para os parâmetros informados não foi encontrado movimento.' type
    'S' display like 'E'.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
form fm_dados_processa .
  data lv_lucas        type c.
  loop at git_zsdt0001 into gwa_zsdt0001.

    " status
    gwa_saida-parid = gwa_zsdt0001-parid.

    read table git_lfa1 into gwa_lfa1 with key lifnr =
    gwa_zsdt0001-parid.
    gwa_saida-name1 = gwa_lfa1-name1.

    read table  git_makt into gwa_makt with key matnr =
    gwa_zsdt0001-matnr.

    gwa_saida-matnr = gwa_zsdt0001-matnr.
    gwa_saida-maktx = gwa_makt-maktx.

    read table git_ekko into gwa_ekko with key lifnr =
    gwa_zsdt0001-parid.

    if gwa_saida-ebeln is initial.

      move icon_icon_list  to gwa_saida-ebeln.

    endif.

    gwa_saida-waers = gwa_ekko-waers.

*    gwa_saida-dt_movimento =  gwa_zsdt0001-dt_movimento.
    gwa_saida-dt_movimento =  sy-datum.
    gwa_saida-docdat       =  gwa_zsdt0001-docdat.
    gwa_saida-peso_fiscal  =  gwa_zsdt0001-peso_fiscal.
    gwa_saida-peso_liq     =  gwa_zsdt0001-peso_liq.
*    gwa_saida-meins        =  gwa_ekpo-meins.
*    gwa_saida-mwskz        =  gwa_ekpo-mwskz.
*    gwa_saida-lgort        =  gwa_ekpo-lgort.
*    gwa_saida-webre        =  gwa_ekpo-webre.
    "gwa_saida-charg        =  gwa_ekpo-charg.

    concatenate gwa_zsdt0001-nfnum '-' gwa_zsdt0001-series into
    gwa_saida-nfnum_s.

    gwa_saida-nfnum  =  gwa_zsdt0001-nfnum.
    gwa_saida-series =  gwa_zsdt0001-series.
    gwa_saida-netwr  =  gwa_zsdt0001-netwr.

    read table git_zib_nfe_dist_itm into gwa_zib_nfe_dist_itm with key
    chave_nfe = gwa_zsdt0001-chave_nfe.

    gwa_saida-icms_valor    = gwa_zib_nfe_dist_itm-icms_valor.
    gwa_saida-ipi_valor     = gwa_zib_nfe_dist_itm-ipi_valor.
    gwa_saida-pis_valor     = gwa_zib_nfe_dist_itm-pis_valor .
    gwa_saida-cof_valor      = gwa_zib_nfe_dist_itm-cof_valor.
    gwa_saida-icms_base     = gwa_zib_nfe_dist_itm-icms_base.
    gwa_saida-icms_aqt      = gwa_zib_nfe_dist_itm-icms_aqt.
    gwa_saida-icms_red_base = gwa_zib_nfe_dist_itm-icms_red_base.
    gwa_saida-chave_nfe     = gwa_zsdt0001-chave_nfe.

    clear: gva_obj_key.
    concatenate gwa_zsdt0001-ch_referencia  gwa_zsdt0001-nr_romaneio
    into gva_obj_key.

    gwa_saida-obj_key       = gva_obj_key.


    read table git_zmmt_ee_zgr into gwa_zmmt_ee_zgr with key
    obj_key = gwa_saida-obj_key.
    gwa_saida-nfnum2  =   gwa_zmmt_ee_zgr-nfenum.
    gwa_saida-authcod =   gwa_zmmt_ee_zgr-authcod.
    gwa_saida-nfenum  =   gwa_zmmt_ee_zgr-nfenum.
    gwa_saida-docstat =   gwa_zmmt_ee_zgr-docstat.
    gwa_saida-cdv     =   gwa_zmmt_ee_zgr-cdv.

    read table git_zmmt0132_aux into gwa_zmmt0132_aux with key
     obj_key = gwa_saida-obj_key.

    if sy-subrc = 0.
      read table git_ekpo into gwa_ekpo with key ebeln = gwa_saida-ebeln.
      if sy-subrc is initial.
        gwa_saida-meins   =  gwa_ekpo-meins.
        gwa_saida-mwskz   =  gwa_ekpo-mwskz.
        gwa_saida-lgort   =  gwa_ekpo-lgort.
        gwa_saida-webre   =  gwa_ekpo-webre.
*      gwa_saida-charg   =  gwa_ekpo-charg.
        gwa_saida-txjcd  = gwa_ekpo-txjcd.
        gwa_saida-txz01  = gwa_ekpo-txz01.

        read table git_ekko into gwa_ekko with key ebeln = gwa_saida-ebeln.
        if sy-subrc is initial.
          gwa_saida-zterm  = gwa_ekko-zterm.
        endif.
      endif.
      gwa_saida-mm_mblnr        = gwa_zmmt0132_aux-mm_mblnr.
      gwa_saida-mm_mjahr        = gwa_zmmt0132_aux-mm_mjahr.
      gwa_saida-ft_belnr        = gwa_zmmt0132_aux-ft_belnr.
      gwa_saida-ft_gjahr        = gwa_zmmt0132_aux-ft_gjahr.
      gwa_saida-dt_vencimento   = gwa_zmmt0132_aux-dt_vencimento.
      gwa_saida-amount_lc       = gwa_zmmt0132_aux-amount_lc.
      gwa_saida-j_1bnftype      = gwa_zmmt0132_aux-j_1bnftype.
      gwa_saida-exch_rate_v     = gwa_zmmt0132_aux-exch_rate_v.
      gwa_saida-bvtyp           = gwa_zmmt0132_aux-bvtyp.
      gwa_saida-ebeln           = gwa_zmmt0132_aux-po_number.
      gwa_saida-ebelp           = gwa_zmmt0132_aux-po_item.
      gwa_saida-lgort           = gwa_zmmt0132_aux-lgort.
      gwa_saida-docnum          = gwa_zmmt0132_aux-docnum.
      concatenate  gwa_zmmt0132_aux-nfnum '-'  gwa_zmmt0132_aux-series into gwa_saida-nfnum_s .
      gwa_saida-charg           = gwa_zmmt0132_aux-batch .


*      READ TABLE git_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j1bnfdoc>)
*      WITH KEY belnr = gwa_zmmt0132_aux-ft_belnr
*               gjahr = gwa_zmmt0132_aux-ft_gjahr
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        gwa_saida-docnum  =   <fs_j1bnfdoc>-docnum.
*      ENDIF.

    else.
      gwa_saida-j_1bnftype      = 'NE'.
    endif.

    perform z_check_lucas changing lv_lucas.

    if ( gwa_saida-mm_mblnr is initial or gwa_saida-ft_belnr is initial ) and lv_lucas is initial.
      move icon_generate   to gwa_saida-status.
    else.
      if gwa_saida-mm_mblnr is not initial or  gwa_saida-ft_belnr is not initial.
        move icon_complete  to  gwa_saida-status .
      endif.
    endif.

    "ALRS pedido do XML
    if gwa_saida-ebeln = icon_icon_list and lv_lucas is not initial .
      if gwa_zib_nfe_dist_itm-prod_pedido_comp is not initial.
        select single * from ekko into  @data(wa_ekko)
            where ebeln = @gwa_zib_nfe_dist_itm-prod_pedido_comp
            and   lifnr = @gwa_zsdt0001-parid.

        if sy-subrc eq 0.
          select single * from ekpo into @data(wa_ekpo)
           where  ebeln = @wa_ekko-ebeln
           and    matnr = @gwa_zsdt0001-matnr.
          if sy-subrc = 0.
            gwa_saida-ebeln = wa_ekko-ebeln.
          endif.
        endif.
      endif.
    endif.
    "ALRS pedido do XML

*    gwa_saida-docnum     = gwa_zmmt_ee_zgr_docs-docnum.

    concatenate gwa_zsdt0001-ch_referencia gwa_zsdt0001-nr_romaneio into
    gwa_saida-ch_referencia.

    read table git_eket into gwa_eket with key ebeln = gwa_ekpo-ebeln
                                               ebelp = gwa_ekpo-ebelp.

*>>>Begin-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo
    data(lv_matnr) = |{ gwa_saida-matnr alpha = out }| .
    data(lv_abrev) = value #( git_zmmt0179[ matnr = lv_matnr ]-abrev optional ).

    if lv_abrev is not initial.

      gwa_saida-charg  = |{ lv_abrev }{ sy-datum+4(2) }{ sy-datum(4) }|.

    else.

      gwa_saida-charg  = gwa_zsdt0001-nr_safra.

    endif.
*<<<End-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo

*    gwa_saida-zterm  = gwa_ekko-zterm.
*    gwa_saida-txjcd  = gwa_ekpo-txjcd.
*    gwa_saida-txz01  = gwa_ekpo-txz01.

    if  gwa_saida-erro_log is not initial.
      move icon_led_red to  gwa_saida-status.
    endif.

    append gwa_saida to git_saida.
    clear: gwa_saida.

  endloop.

* Montar Saldo Pedido Compra

  "TOTMOV
  sort: git_ekbe     by ebeln ebelp.
  loop at git_ekbe into gwa_ekbe.
    move-corresponding gwa_ekbe to gwa_ekbe_tot.
    if gwa_ekbe-shkzg = 'S'.
      gwa_ekbe_tot-menge =  gwa_ekbe-menge.
    else.
      gwa_ekbe_tot-menge = gwa_ekbe-menge * -1.
    endif.
    collect gwa_ekbe_tot into git_ekbe_tot.
    clear gwa_ekbe_tot.
  endloop.

  "XTOTPED

  sort: git_ekpo     by ebeln ebelp,
        git_ekbe_tot by ebeln ebelp.

  loop at git_ekpo into gwa_ekpo.
    gwa_ekpo_tot-ebeln = gwa_ekpo-ebeln.
    gwa_ekpo_tot-ebelp = gwa_ekpo-ebelp.
    gwa_ekpo_tot-matnr = gwa_ekpo-matnr.
    gwa_ekpo_tot-txz01 = gwa_ekpo-txz01.
    gwa_ekpo_tot-mwskz = gwa_ekpo-mwskz.
    gwa_ekpo_tot-webre = gwa_ekpo-webre.
    gwa_ekpo_tot-meins = gwa_ekpo-meins.
    gwa_ekpo_tot-lgort = gwa_ekpo-lgort.
    gwa_ekpo_tot-menge = gwa_ekpo-menge.

    read table git_ekbe_tot into gwa_ekbe_tot with key
    ebeln =  gwa_ekpo_tot-ebeln
    ebelp =  gwa_ekpo_tot-ebelp.
    if sy-subrc = 0.
      gwa_ekpo_tot-menge = gwa_ekpo-menge - gwa_ekbe_tot-menge.
    endif.

    collect gwa_ekpo_tot into git_ekpo_tot.
    clear gwa_ekpo_tot.
  endloop.
  delete git_ekpo_tot where menge le 0.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
form fm_end_of_selection .
  perform fm_filtros.
  call screen 0100.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
form fm_filtros .

  data vl_text type table of textpool.

  call function 'RS_TEXTPOOL_READ'
    exporting
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    tables
      tpool      = vl_text.

  free: git_filtro.

  loop at screen.
    git_filtro = value #(
      ( parametro = '' valor = s_bukrs )
*      ( PARAMETRO = '' VALOR = P_WERKS )
    ).
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100' with
  'Geração de Entrada MIGO e MIRO – Fábricas'.

  perform fm_criar_objetos.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
form fm_criar_objetos .

  data: lit_layout type lvc_s_layo.
  data: gs_variant_c type disvariant.
  lit_layout-no_toolbar = abap_false.
  lit_layout-stylefname = 'CELLTAB'.
  lit_layout-zebra      = 'X'.
  lit_layout-no_rowmove = 'X'.
  lit_layout-no_rowins  = 'X'.
  lit_layout-no_rowmark = space.
  lit_layout-sel_mode   = 'A'.
  lit_layout-cwidth_opt = 'X'.

  wa_stable-row        = 'X'.
  wa_stable-col        = 'X'.

  data:
    lit_fun type ui_functions,
    lwa_fun type ui_func.

  data: lv_data(22) type c,
        v_icon(80)  type c,
        lv_text(20) type c.

  perform fm_cria_fieldcat.
  perform fm_style_celltab.


  if s_dtmov-high is not initial.
    concatenate s_dtmov-low+6(2) '.'  s_dtmov-low+4(2) '.' s_dtmov-low+0(4) '-'
    s_dtmov-high+6(2) '.'  s_dtmov-high+4(2) '.' s_dtmov-high+0(4) into lv_data.
  else.
    concatenate s_dtmov-low+6(2) '.'  s_dtmov-low+4(2) '.' s_dtmov-low+0(4) into lv_data.
  endif.

  " WRITE icon_led_green AS ICON TO lv_text.
  " CONCATENATE  lv_text 'Lançamentos de MIGO e MIRO não gerado' INTO v_icon SEPARATED BY space.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    exporting
       i_titulo  = 'Geração de Entrada MIGO e MIRO – Fábricas'
       i_filtros = value zif_screen_linha_filtro_t( ( parametro = 'Empresa'
       valor = s_bukrs-low ) ( parametro =
       'Filial' valor = s_branch-low )
     ( parametro =
       'Fornecedor' valor = s_parid-low )
    ( parametro =
       'Data Movimento' valor = lv_data )
       ( parametro =
       '' valor = v_icon ) )
     changing
       alv = gob_gui_alv_grid
     )
     eq abap_true.

    data: it_f4 type lvc_t_f4.
    data: wa_f4 type lvc_s_f4.

    clear wa_f4.
    wa_f4-fieldname  = 'BVTYP'.
    wa_f4-register   = 'X'.
    append wa_f4 to it_f4.

    call method gob_gui_alv_grid->register_f4_for_fields
      exporting
        it_f4 = it_f4.

    create object event_receiver.
    set handler event_receiver->handle_user_command for gob_gui_alv_grid.
    set handler event_receiver->handle_toolbar for gob_gui_alv_grid.
    set handler event_receiver->hotspot_click  for gob_gui_alv_grid.
    set handler event_receiver->handle_data_changed for gob_gui_alv_grid.
    gob_gui_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).

    create object alv_events.
    set handler alv_events->on_f4 for gob_gui_alv_grid.

    lwa_fun = cl_gui_alv_grid=>mc_fc_info.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_copy.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_cut.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_paste.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_undo.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_detail.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append lwa_fun to lit_fun.
    lwa_fun = cl_gui_alv_grid=>mc_fc_refresh.
    append lwa_fun to lit_fun.

    gs_variant_c-report      = sy-repid.
    call method gob_gui_alv_grid->set_table_for_first_display
      exporting
        is_variant                    = gs_variant_c
        it_toolbar_excluding          = lit_fun
        is_layout                     = lit_layout "csb
        i_save                        = 'X'
      changing
        it_outtab                     = git_saida
        it_fieldcatalog               = git_fcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    call method gob_gui_alv_grid->set_toolbar_interactive.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
form fm_cria_fieldcat .

  git_fcat =  value lvc_t_fcat(
  ( fieldname = 'STATUS'            coltext =  'Status'                    outputlen = '07'  icon = 'X'   )
  ( fieldname = 'PARID'             coltext =  'Fornecedor'                outputlen = '10'  no_zero = 'X' )
  ( fieldname = 'NFNUM_S'           coltext =  'Nota Fiscal-Serie'         outputlen = '13'  edit = 'X' )
  ( fieldname = 'NETWR'             coltext =  'Valor nota'                outputlen = '10'  )
  ( fieldname = 'ICMS_VALOR'        coltext =  'Valor ICMS'	               outputlen = '17'  )
  ( fieldname = 'AMOUNT_LC'         coltext =  'Valor USD'                 outputlen = '10'  edit = 'X' ref_table = 'ZMMT_EE_ZGR'  ref_field = 'AMOUNT_LC' )
  ( fieldname = 'EXCH_RATE_V'       coltext =  'Taxa Câmbio'               outputlen = '12'  edit = 'X' ref_table = 'ZIB_NFE_DIST_ITM'  ref_field = 'CTR_WKURS')
  ( fieldname = 'J_1BNFTYPE'        coltext =  'Cat.NF'                    outputlen = '06'  edit = 'X' ref_table = 'ZMMT_EE_ZGR'  ref_field = 'J_1BNFTYPE')
  ( fieldname = 'CHARG'             coltext =  'Lote'                      outputlen = '10'  edit = 'X' ref_table = 'ZMMT_EE_ZGR'  ref_field = 'BATCH')
  ( fieldname = 'NAME1'             coltext =  'Nome do Fornecedor'	       outputlen = '35'  )
  ( fieldname = 'MATNR'             coltext =  'Material'                  outputlen = '18'  lzero = '' no_zero = 'X')
  ( fieldname = 'MAKTX'             coltext =  'Descrição Material'	       outputlen = '40'  )
  ( fieldname = 'EBELN'             coltext =  'Ped. Compra'               outputlen = '10'   just = 'C' hotspot = 'X' )
  ( fieldname = 'EBELP'             coltext =  'Item'                      outputlen = '05'  )
  ( fieldname = 'WAERS'             coltext =  'Moeda'                     outputlen = '05'  )
  ( fieldname = 'LGORT'             coltext =  'Deposito'                  outputlen = '08'  edit = 'X' )
  ( fieldname = 'BVTYP'             coltext =  'Dados Bancários'           outputlen = '15'  f4availabl = 'X' edit = 'X' )
  ( fieldname = 'DT_VENCIMENTO'     coltext =  'Data Vencimento'           outputlen = '15'  edit = 'X' f4availabl = 'X' ref_table = 'ZMMT_EE_ZGR'  ref_field = 'DT_VENCIMENTO' datatype = 'DATS')
  ( fieldname = 'DT_MOVIMENTO'      coltext =  'Dt. Movimento'             outputlen = '13'  no_zero = 'X' )
  ( fieldname = 'DOCDAT'            coltext =  'Data Nota'                 outputlen = '10'  )
  ( fieldname = 'PESO_FISCAL'       coltext =  'Peso Fiscal'               outputlen = '13'  )
  ( fieldname = 'PESO_LIQ'          coltext =  'Peso. Balança'             outputlen = '13'  no_zero = 'X' )
  ( fieldname = 'MEINS'             coltext =  'Unid.'                     outputlen = '06'  )
  ( fieldname = 'MWSKZ'             coltext =  'IVA'                       outputlen = '03'  )
  ( fieldname = 'MM_MBLNR'          coltext =  'Doc. MIGO'                 outputlen = '10'  hotspot = 'X' )
  ( fieldname = 'FT_BELNR'          coltext =  'Doc. MIRO'                 outputlen = '10'  hotspot = 'X' )
  ( fieldname = 'DOCNUM'            coltext =  'Doc.Nota Fiscal'           outputlen = '10'  )
  ( fieldname = 'ICMS_BASE'         coltext =  'Valor Base ICMS'           outputlen = '17'  )
  ( fieldname = 'ICMS_AQT'          coltext =  'Aliq.ICMS'                 outputlen = '17'  )
  ( fieldname = 'ICMS_RED_BASE'     coltext =  'Perc.Red.ICMS'             outputlen = '17'  )
  ( fieldname = 'CHAVE_NFE'         coltext =  'Chave Acesso NFe'          outputlen = '10'  )
  ( fieldname = 'CH_REFERENCIA'     coltext =  'Chave Geração Lançamento'  outputlen = '44'  )
  ( fieldname = 'ZTERM'             coltext =  'Cond.Pgto'                 outputlen = '10'  )
  ( fieldname = 'TXJCD'             coltext =  'Cod.domic.Fiscal'          outputlen = '17'  )
  ( fieldname = 'ERRO_LOG'          coltext =  'Log de Erros'	             outputlen = '80'  )  ).

endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
module user_command_0100_exit input.
  leave to screen 0.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
  endcase.
endmodule.

*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
form on_f4  using f_fieldname    type lvc_fname
                  f_fieldvalue   type lvc_value
                  fw_row_no      type lvc_s_roid
                  fcl_event_data type ref to cl_alv_event_data
                  ft_bad_cells   type lvc_t_modi
                  f_display      type char01.

  data: lw_alv  type ty_saida,
        lw_modi type lvc_s_modi.

  data: l_bvtyp type lfbk-bvtyp.

  field-symbols: <lfst_modi> type lvc_t_modi.

  assign fcl_event_data->m_data->* to <lfst_modi>.
  check sy-subrc = 0.

  read table git_saida into gwa_saida index fw_row_no-row_id.

  check sy-subrc = 0.

  case f_fieldname.
    when 'BVTYP'.
      perform f4_bvtyp      using gwa_saida-parid
                            changing l_bvtyp.

      f_fieldvalue = l_bvtyp.

      clear lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value    = f_fieldvalue.
      append lw_modi to <lfst_modi>.

      fcl_event_data->m_event_handled = 'X'.

  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_BVTYP
*&---------------------------------------------------------------------*
form f4_bvtyp   using f_parid type ty_saida-parid
                changing f_bvtyp type lfbk-bvtyp.

  data: it_rettab type table of ddshretval,
        it_lfbk   type table of lfbk.

  data: wa_rettab type ddshretval.

  select *
    into table it_lfbk
    from lfbk
   where lifnr eq f_parid.

  check sy-subrc = 0.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      ddic_structure  = 'LFBK'
      retfield        = 'BVTYP'
      value_org       = 'S'
    tables
      value_tab       = it_lfbk
      return_tab      = it_rettab
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  check sy-subrc = 0.

  read table it_rettab into wa_rettab index 1.

  check sy-subrc = 0.

  f_bvtyp = wa_rettab-fieldval.

endform.
*&---------------------------------------------------------------------*
*&      Form  GET_PED_COMPRA
*&---------------------------------------------------------------------*
form get_ped_compra using e_row type lvc_s_row
                          e_column_id type  lvc_s_col.

  data: it_rettab type table of ddshretval.
  data: wa_rettab type ddshretval.

  clear: git_ped_compra.

  sort : git_ekpo_tot by ebeln ebelp,
         git_ekko by ebeln.


  read table git_saida into gwa_saida index e_row-index.

  check sy-subrc = 0.

  if gwa_saida-mm_mblnr is initial.
    loop at git_ekko into gwa_ekko where lifnr eq gwa_saida-parid or
                                         llief eq gwa_saida-parid.
      gwa_ped_compra-waers = gwa_ekko-waers.

      gwa_ped_compra-parid = gwa_saida-parid.
      gwa_ped_compra-name1 = gwa_saida-name1.
      gwa_ped_compra-meins = gwa_saida-meins.
      gwa_ped_compra-lgort = gwa_saida-lgort.
      gwa_ped_compra-charg = gwa_saida-charg.

      read table git_ekpo assigning field-symbol(<fs_ekpo>)
      with key ebeln = gwa_ekko-ebeln.
      if sy-subrc is initial.
        gwa_ped_compra-txjcd = <fs_ekpo>-txjcd.
      endif.

      loop at git_ekpo_tot into gwa_ekpo_tot where ebeln = gwa_ekko-ebeln.
        if  gwa_ekpo_tot-menge > 0.
          gwa_ped_compra-ebeln  = gwa_ekpo_tot-ebeln.
          gwa_ped_compra-ebelp  = gwa_ekpo_tot-ebelp.
          gwa_ped_compra-menge  = gwa_ekpo_tot-menge.
          gwa_ped_compra-matnr  = gwa_ekpo_tot-matnr.
          gwa_ped_compra-txz01  = gwa_ekpo_tot-txz01.

          gwa_ped_compra-meins  = gwa_ekpo_tot-meins.
          gwa_ped_compra-lgort  = gwa_ekpo_tot-lgort.
          gwa_ped_compra-mwskz  = gwa_ekpo_tot-mwskz.
          gwa_ped_compra-webre  = gwa_ekpo_tot-webre.

          clear: gwa_eket.
          read table git_eket into gwa_eket with key ebeln = gwa_ekpo-ebeln
                                     ebelp = gwa_ekpo-ebelp.

          gwa_ped_compra-charg = gwa_eket-charg.

          append  gwa_ped_compra to  git_ped_compra.
        endif.
      endloop.
      clear: gwa_ped_compra,gwa_ekpo_tot, gwa_saida_aux.
    endloop.

    sort git_ped_compra by ebeln ebelp.

    cl_salv_pop_up=>popup( exporting t_table = git_ped_compra ) .

    data(_erro) = ' '.
    if gwa_ped_compra-ebeln is not initial.
      if gwa_saida-peso_fiscal gt  gwa_ped_compra-menge.
        _erro = 'X'.
        message  'Peso fiscal maior que saldo pedido ' type 'S' display like 'E'.
      endif.
    endif.
    if gwa_ped_compra-ebeln is not initial and _erro is initial.

      gwa_saida-ebeln = gwa_ped_compra-ebeln.
      gwa_saida-ebelp = gwa_ped_compra-ebelp.

      if gwa_saida-charg is initial.
        gwa_saida-charg = gwa_ped_compra-charg.
      endif.
      gwa_saida-meins        =  gwa_ped_compra-meins.
      gwa_saida-mwskz        =  gwa_ped_compra-mwskz.
      gwa_saida-lgort        =  gwa_ped_compra-lgort.
      gwa_saida-webre        =  gwa_ped_compra-webre.

      read table git_ekko assigning field-symbol(<fs_ekko>)
      with key ebeln = gwa_ped_compra-ebeln.
      if sy-subrc is initial.
        gwa_saida-zterm  = <fs_ekko>-zterm.
      endif.

      gwa_saida-txjcd  = gwa_ped_compra-txjcd.
      gwa_saida-txz01  = gwa_ped_compra-txz01.

    else.
      clear: gwa_saida-ebeln,
             gwa_saida-ebelp.
*             gwa_saida-charg.

      move icon_icon_list  to  gwa_saida-ebeln.
    endif.

    modify git_saida from gwa_saida index e_row-index.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_MIRO_MIGO
*&---------------------------------------------------------------------*
form fm_miro_migo  tables p_et_index_rows
                        structure lvc_s_row.

  data: lwa_doc_gerados type zmmt0132,
        doc_number_ref  type j_1bdocnum,
        lv_lucas        type c.

  data: lwa_selected_line like lvc_s_row,
        lf_row_index      type lvc_index.

  data: lva_erro_est_migo type c,
        lva_erro_est_miro type c.

  loop at p_et_index_rows into lwa_selected_line.

    lf_row_index = lwa_selected_line-index.

    read table git_saida index lf_row_index into gwa_saida.

    if gwa_saida-status <> icon_complete.

      if gwa_saida-ebeln is not initial.

        perform z_check_lucas changing lv_lucas.

        gwa_zmmt0132-obj_key        = gwa_saida-obj_key.
        gwa_zmmt0132-po_number      = gwa_saida-ebeln.
        gwa_zmmt0132-lifnr          = gwa_saida-parid.
        gwa_zmmt0132-move_type      = cond #( when lv_lucas eq 'X' then '835'
                                              else '101' ).
        gwa_zmmt0132-nt_remessa     = gwa_saida-nfnum_s.
        gwa_zmmt0132-mm_mblnr       = gwa_saida-mm_mblnr.
        gwa_zmmt0132-mm_mjahr       = gwa_saida-mm_mjahr.
        gwa_zmmt0132-ft_belnr       = gwa_saida-ft_belnr.
        gwa_zmmt0132-ft_gjahr       = gwa_saida-ft_gjahr.
        gwa_zmmt0132-doc_date       = gwa_saida-docdat.
        gwa_zmmt0132-pstng_date     = gwa_saida-dt_movimento.
        gwa_zmmt0132-dt_vencimento  = gwa_saida-dt_vencimento.
        gwa_zmmt0132-po_item        = gwa_saida-ebelp.
        gwa_zmmt0132-entry_qnt      = gwa_saida-peso_fiscal.
        gwa_zmmt0132-meins          = gwa_saida-meins.
        gwa_zmmt0132-in_aviso_receb = 'N'.
        gwa_zmmt0132-peso_bruto     = gwa_saida-peso_fiscal.
        gwa_zmmt0132-comp_code      = s_bukrs-low.
        gwa_zmmt0132-tp_operacao    = '01'.
        gwa_zmmt0132-ref_doc_no     = gwa_saida-nfnum_s.
        gwa_zmmt0132-vr_bruto       = gwa_saida-netwr.
        gwa_zmmt0132-del_costs_taxc = gwa_saida-mwskz.
        gwa_zmmt0132-pmnt_block     = 'V'.
        gwa_zmmt0132-pmnttrms       = gwa_saida-zterm.
        "gwa_zmmt0132-scbank_ind     = 'BBRA'.
        gwa_zmmt0132-scbank_ind     = zcl_miro=>get_banco_forma_pagamento( i_bukrs = gwa_zmmt0132-comp_code i_forma_pagamento  = 'S' ).
        gwa_zmmt0132-plant          = s_branch-low.
        gwa_zmmt0132-j_1bnftype     = gwa_saida-j_1bnftype.
        gwa_zmmt0132-alloc_nmbr     = gwa_saida-ebeln.
        gwa_zmmt0132-gross_amount   = gwa_saida-netwr.
        gwa_zmmt0132-item_amount    = gwa_saida-netwr.
        gwa_zmmt0132-material       = gwa_saida-matnr.
        gwa_zmmt0132-nu_item        = gwa_saida-ebelp.
        gwa_zmmt0132-taxjurcode     = gwa_saida-txjcd.
        gwa_zmmt0132-zdt_atlz       = sy-datum.
        gwa_zmmt0132-zhr_atlz       = sy-uzeit.
        gwa_zmmt0132-zrg_atlz       = 0.
        gwa_zmmt0132-pymt_meth      = 'E'.
        gwa_zmmt0132-bus_area       = s_branch-low.
        gwa_zmmt0132-quantity       = gwa_saida-peso_fiscal.
        gwa_zmmt0132-batch          = gwa_saida-charg.
        gwa_zmmt0132-lgort          = gwa_saida-lgort.
*** 10.06.2022 - BUG - 81027 - Inicio
        "gwa_zmmt0132-vr_impostos    = gwa_saida-icms_base.

        gwa_zmmt0132-vr_impostos    = gwa_saida-icms_valor + gwa_saida-ipi_valor + gwa_saida-pis_valor + gwa_saida-cof_valor.

*** 10.06.2022 - BUG - 81027 - Fim
        gwa_zmmt0132-amount_lc      = gwa_saida-amount_lc.
        gwa_zmmt0132-exch_rate_v    = gwa_saida-exch_rate_v.
        gwa_zmmt0132-bvtyp          = gwa_saida-bvtyp.
        gwa_zmmt0132-nfnum          = gwa_saida-nfnum.
        gwa_zmmt0132-series         = gwa_saida-series.
        gwa_zmmt0132-webre          = gwa_saida-webre.
        gwa_zmmt0132-chave_nfe      = gwa_saida-chave_nfe.
        modify zmmt0132 from gwa_zmmt0132.
        commit work.

        data(ck_cotinue) = abap_false.

        perform verifica_miro using gwa_zmmt0132 gwa_zmmt0132-obj_key changing ck_cotinue gva_return .
        if ck_cotinue eq abap_true.
          perform z_prepara_mensagem2 using gwa_zmmt0132-obj_key 'E' gva_return gwa_zmmt0132-obj_key space '11'.
          continue.
        endif.

        if gwa_zmmt0132-mm_mblnr is initial.
          perform z_bapi_migo using gwa_zmmt0132 lwa_doc_gerados lv_lucas.
          if gva_erro is initial.
            if lv_lucas ne abap_true.
              perform z_bapi_miro using gwa_zmmt0132 lwa_doc_gerados doc_number_ref.
            endif.
          endif.
        endif.

        if gva_erro is not initial .

          move-corresponding gwa_zmmt0132 to lwa_doc_gerados.

          perform z_estorna_miro using lwa_doc_gerados
                          changing lva_erro_est_miro.
          if lva_erro_est_miro is initial.
            perform z_estorna_migo using lwa_doc_gerados
                             changing lva_erro_est_migo.
          endif.
        endif.

        clear: gwa_zmmt0132.
      else.
        message  'Pedido de Compra não informado' type 'S' display like 'E'.
        exit.
      endif.
    else.
      message  'MIRO/MIGO já gerados nessa seleção' type 'S' display like 'E'.
      exit.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_DOCS
*&---------------------------------------------------------------------*
form fm_get_docs   tables p_et_index_rows
                   structure lvc_s_row.

  data: lwa_selected_line like lvc_s_row,
        lf_row_index      type lvc_index,
        lv_obj_key        type zob_mensagem-obj_key.

*** Inicio - Rubenilson Pereira - 16.06.2022 - BUG 81324
  if p_et_index_rows[] is initial.

    data(lt_saida_aux) = git_saida.

    sort lt_saida_aux by obj_key.

    delete adjacent duplicates from lt_saida_aux comparing obj_key.

    select * from zmmt0132
      into table git_zmmt0132_aux
       for all entries in lt_saida_aux
     where obj_key eq lt_saida_aux-obj_key.
    if sy-subrc is initial.
      sort lt_saida_aux by obj_key.

*---> 05/07/2023 - Migração S4 - DL
      sort git_zmmt0132_aux by obj_key.
*<--- 05/07/2023 - Migração S4 - DL

      loop at git_saida assigning field-symbol(<fs_saida>).

        read table git_zmmt0132_aux assigning field-symbol(<fs_zmmt0132>)
        with key obj_key = <fs_saida>-obj_key
        binary search.
        if sy-subrc = 0.
          move icon_complete   to <fs_saida>-status.
          <fs_saida>-mm_mblnr        = gwa_zmmt0132_aux-mm_mblnr.
          <fs_saida>-mm_mjahr        = gwa_zmmt0132_aux-mm_mjahr.
          <fs_saida>-ft_belnr        = gwa_zmmt0132_aux-ft_belnr.
          <fs_saida>-ft_gjahr        = gwa_zmmt0132_aux-ft_gjahr.
          <fs_saida>-dt_vencimento   = gwa_zmmt0132_aux-dt_vencimento.
          <fs_saida>-amount_lc       = gwa_zmmt0132_aux-amount_lc.
          <fs_saida>-j_1bnftype      = gwa_zmmt0132_aux-j_1bnftype.
          <fs_saida>-exch_rate_v     = gwa_zmmt0132_aux-exch_rate_v.
          <fs_saida>-bvtyp           = gwa_zmmt0132_aux-bvtyp.
          <fs_saida>-ebeln           = gwa_zmmt0132_aux-po_number.
          <fs_saida>-ebelp           = gwa_zmmt0132_aux-po_item.
          concatenate gwa_zmmt0132_aux-nfnum '-' gwa_zmmt0132_aux-series into <fs_saida>-nfnum_s.
          <fs_saida>-charg           = gwa_zmmt0132_aux-batch.

        else.

          if <fs_saida>-ebeln is initial.
            move icon_icon_list  to <fs_saida>-ebeln.
          endif.

        endif.

      endloop.

    endif.

  else.
*** Fim - Rubenilson Pereira - 16.06.2022 - BUG 81324

    loop at p_et_index_rows into lwa_selected_line.

      lf_row_index = lwa_selected_line-index.

      read table git_saida index lf_row_index into gwa_saida.

      clear: git_zmmt0132_aux, gwa_zmmt0132_aux.
      select * from zmmt0132
        into table git_zmmt0132_aux
        where obj_key eq gwa_saida-obj_key.


      read table git_zmmt0132_aux into gwa_zmmt0132_aux with key
       obj_key = gwa_saida-obj_key.

      if sy-subrc = 0.
        move icon_complete   to gwa_saida-status.
        gwa_saida-mm_mblnr        = gwa_zmmt0132_aux-mm_mblnr.
        gwa_saida-mm_mjahr        = gwa_zmmt0132_aux-mm_mjahr.
        gwa_saida-ft_belnr        = gwa_zmmt0132_aux-ft_belnr.
        gwa_saida-ft_gjahr        = gwa_zmmt0132_aux-ft_gjahr.
        gwa_saida-dt_vencimento   = gwa_zmmt0132_aux-dt_vencimento.
        gwa_saida-amount_lc       = gwa_zmmt0132_aux-amount_lc.
        gwa_saida-j_1bnftype      = gwa_zmmt0132_aux-j_1bnftype.
        gwa_saida-exch_rate_v     = gwa_zmmt0132_aux-exch_rate_v.
        gwa_saida-bvtyp           = gwa_zmmt0132_aux-bvtyp.
        gwa_saida-ebeln           = gwa_zmmt0132_aux-po_number.
        if gwa_saida-ebeln is initial.
          move icon_icon_list  to gwa_saida-ebeln.
        endif.
        gwa_saida-ebelp           = gwa_zmmt0132_aux-po_item.
        concatenate gwa_zmmt0132_aux-nfnum '-' gwa_zmmt0132_aux-series into gwa_saida-nfnum_s.
        gwa_saida-charg           = gwa_zmmt0132_aux-batch.

        modify git_saida from gwa_saida index lf_row_index.

      else.

        select * from zob_mensagem into table git_zob_mensagem
          where obj_key eq gwa_saida-obj_key.

        sort git_zob_mensagem by seq_registro.

        read table git_zob_mensagem into gwa_zob_mensagem index 1.

        gwa_saida-erro_log   =  gwa_zob_mensagem-message.

        modify git_saida from gwa_saida index lf_row_index.

      endif.
    endloop.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
form event_top_of_page using   dg_dyndoc_id type ref to cl_dd_document.

endform.                    " EVENT_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  Z_BAPI_MIGO
*&---------------------------------------------------------------------*
*       Carregar BAPI MIGO
*----------------------------------------------------------------------*
form z_bapi_migo  using gwa_zmmt0132    type zmmt0132
                        lwa_doc_gerados type zmmt0132
                        p_lucas.

***Dados Header
  clear gwa_header.

  clear: gva_erro,
         gva_invoicedocnumber_migo, gva_ano_migo,
         gva_invoicedocnumber_miro, gva_ano_miro,
         git_serialnumber,
         git_return.

  gva_obj_key = gwa_zmmt0132-obj_key.

  gwa_header-doc_date   = gwa_zmmt0132-doc_date.
  gwa_header-pstng_date = sy-datum.
  gwa_header-ref_doc_no = gwa_zmmt0132-nt_remessa. "gwa_zmmt0132-po_number.

  if gwa_zmmt0132-mm_mblnr is not initial.
    gva_invoicedocnumber_migo-mat_doc  = gwa_zmmt0132-mm_mblnr.
    gva_invoicedocnumber_migo-doc_year = gwa_zmmt0132-mm_mjahr.
    gva_ano_migo                       = gwa_zmmt0132-mm_mjahr.
    exit.
  endif.

  clear gva_gmcode.
  gva_gmcode = '01'.

  if p_lucas = 'X'.
    perform z_get_refkey changing gwa_item-ref_doc.
    if gwa_item-ref_doc is initial.
      message  'Nota de saida não encontrada' type 'S' display like 'E'.
      exit.
    endif.
    gwa_item-tax_code     = 'ZG'.
    gwa_item-material     = gwa_zmmt0132-material.
    gwa_item-amount_lc    = gwa_zmmt0132-item_amount.
    gwa_item-plant        = gwa_zmmt0132-plant.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gwa_zmmt0132-lifnr
      importing
        output = gwa_item-move_plant.
    "
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = gwa_item-move_plant
      importing
        output = gwa_item-move_plant.
    gwa_header-ref_doc_no = gwa_zmmt0132-nt_remessa.
    gwa_item-entry_uom    = ''.

    gwa_item-stge_loc     = ''.
    gva_gmcode = '04'.
    clear: gwa_item-mvt_ind.
  else.
    gwa_item-mvt_ind      = 'B'.
    gwa_item-po_item      = gwa_zmmt0132-nu_item.
    gwa_item-po_number    = gwa_zmmt0132-po_number.
  endif.

  gwa_item-stge_loc     = gwa_zmmt0132-lgort.
  gwa_item-batch        = gwa_zmmt0132-batch.
  gwa_item-move_type    = gwa_zmmt0132-move_type.
  gwa_item-entry_qnt    = gwa_zmmt0132-entry_qnt.

  data(lva_move_type) = gwa_item-move_type.

  clear git_item.
  append gwa_item to git_item.
  clear gwa_item.

  append gwa_serialnumber to git_serialnumber.

  data(ck_continuar) = abap_true.

  while ck_continuar eq abap_true.

    "Executa BAPI
    "US162279-
    data(lc_user_job) = zcl_job=>get_user_job( ).
    if lc_user_job is not initial.
      sy-uname =  lc_user_job.
    endif.
    "US162279-
    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      exporting
        goodsmvt_header       = gwa_header
        goodsmvt_code         = gva_gmcode ""01  MB01 - 02 MB31 - 03 MB1A - 04 MB1B - 05 MB1C - 06 MB11 - 07 MB04
      importing
        goodsmvt_headret      = gva_invoicedocnumber_migo
        materialdocument      = gwa_mat_doc
        matdocumentyear       = gva_ano_migo
      tables
        goodsmvt_item         = git_item
        goodsmvt_serialnumber = git_serialnumber
        return                = git_return.

    if gwa_mat_doc is initial.
      data(_bloq) = abap_false.
      perform f_check_msg_bloq tables git_return changing _bloq gwa_return.
      if _bloq eq abap_false.
        ck_continuar = abap_false.
      else.
        message id gwa_return-id type 'S' number gwa_return-number with gwa_return-message_v1 gwa_return-message_v2 gwa_return-message_v3 gwa_return-message_v4
        display like 'E'.
        wait up to 1 seconds.
        clear: git_return[].
      endif.
    else.
      ck_continuar = abap_false.
    endif.
  endwhile.

  wait up to 2 seconds.

  if git_return[] is not initial.
    read table git_return into gwa_return with key type = 'E'.
    if sy-subrc = 0.
      gva_erro = 'X'.
    else.
      clear gva_erro.
    endif.
  else.
    clear gva_erro.
*Cria mensagem de retorno em caso de sucesso
    clear gwa_return.
*Mensagem: O documento <nr_doc> para o exercício <ano>
*          foi criado com sucesso.

    gwa_zmmt0132-mm_mblnr = gwa_mat_doc.
    gwa_zmmt0132-mm_mjahr = gva_ano_migo.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = gwa_zmmt0132-mm_mblnr
      importing
        output = gwa_zmmt0132-mm_mblnr.

  endif.

  if gva_erro is initial.
    perform z_bapi_trans_commit.

    modify zmmt0132 from gwa_zmmt0132.
    commit work.

  endif.

  gva_wrefkey = gva_invoicedocnumber_migo.

  select single docnum
    into gva_docnum
    from j_1bnflin
    where refkey = gva_wrefkey.

  if sy-subrc is not initial.
    perform wait_documento_fiscal in program zmmr019_02 using gva_wrefkey lva_move_type changing gva_docnum if found.
    if gva_docnum is not initial.
      sy-subrc = 0.
    else.
      sy-subrc = 1.
    endif.
  endif.

  if ( sy-subrc eq 0 ) and ( gva_wrefkey is not initial ).
*     Mensagem: O documento <nr_doc> para o exercício <ano>
*               foi criado com sucesso.
    gwa_zmmt0132-docnum = gva_docnum.

    modify zmmt0132 from gwa_zmmt0132.
    commit work and wait.

    concatenate text-001 gva_docnum text-002 gva_ano_migo text-003 into gva_message separated by space.
    clear: gva_message_v1, gva_message_v2.

    gva_message_v1 = gva_docnum.
    gva_message_v2 = gva_ano_migo.

    gwa_return-type    = 'S'.
    gwa_return-id      = 'MM'.
    gwa_return-number  = '899'.
    gwa_return-message = gva_message.
    gwa_return-message_v1 = gva_message_v1.
    gwa_return-message_v2 = gva_message_v2.

    perform z_prepara_mensagem2   using gva_obj_key
                                     gwa_return-type
                                     gwa_return-message
                                     gwa_return-message_v1
                                     gwa_return-message_v2
                                     '05'.
  endif.

  "identificador de interface MIGO
  gva_interface = '11'.
***Trata msg de erro
  loop at git_return into gwa_return.
    perform z_prepara_mensagem2   using gva_obj_key
                                       gwa_return-type
                                       gwa_return-message
                                       gwa_return-message_v1
                                       gwa_return-message_v2
                                       gva_interface.
  endloop.


endform.

*&---------------------------------------------------------------------*
*&      Form  Z_BAPI_MIRO
*&---------------------------------------------------------------------*
*       Criar BAPI MIRO
*----------------------------------------------------------------------*
form z_bapi_miro using gwa_zmmt0132   type zmmt0132
                       wa_doc_gerados type zmmt0132
                       number_ref     type j_1bdocnum.

  data: i_doc          type j_1bnfdoc,
        i_acttab       type j_1bnfe_active,
        vl_belnr       type rbkp-belnr,
        vl_tax_amount  type bapi_incinv_create_tax-tax_amount,
        vg_inco1       type inco1,
        vl_zerar_base  type c,
        vl_wi_tax_base type zmmt_eeimp_zgr-wi_tax_base,
        vl_bsart       type ekko-bsart,
        vl_lineid      type setline.

  data: number           type tbtcjob-jobcount,
        name             type tbtcjob-jobname,
        print_parameters type pri_params.
  data: ck_erro type char01 value abap_false.

  if gwa_zmmt0132-ft_belnr is not initial.
    exit.
  endif.

***Verifica tipo de documento
* Tipo de Documento

  clear: git_item,
         git_withtaxdata,
         git_taxdata,
         gva_erro,
         vl_wi_tax_base,
         vl_zerar_base.

  "Busca fornecedor para buscar o tax code
  clear: gwa_ekko.
  select single *
    into gwa_ekko
    from ekko
   where ebeln = gwa_zmmt0132-po_number.


  gva_lifnr = gwa_ekko-lifnr.
  vg_inco1 = gwa_ekko-inco1.


  clear gva_total_item.

  perform: z_cabec_item.


  clear: gwa_withtaxdata,
         gwa_taxdata.

  select single bsart
    from ekko
    into vl_bsart
   where ebeln = gwa_zmmt0132-po_number.

*  gwa_taxdata-tax_code  = gwa_zmmt0132-del_costs_taxc.
*
**  IF git_itemdata[] IS NOT INITIAL.
**    READ TABLE git_itemdata ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX 1.
**
**    "Ajustar Vr Imposto para Crédito de Pis/Cofins sobre Aquisição
**    PERFORM add_pis_cofins USING gwa_zmmt0132 CHANGING gwa_zmmt0132-vr_impostos <fs_item>-item_amount.
**  ENDIF.
*
*  gwa_taxdata-tax_amount = gwa_zmmt0132-vr_impostos.
*  gwa_taxdata-tax_base_amount = abs( gva_total_item ).
*
*  APPEND gwa_taxdata TO git_taxdata.
*  CLEAR vl_lineid.


  clear: gwa_return.
  free: git_return.

  move-corresponding gwa_headerdata to gwa_headerdata_local.

  if gva_erro is initial.

    gwa_zmmt0132-plant = s_branch-low.

    if gwa_zmmt0132-po_number is not initial.
      zcl_pedido_compra=>verif_bloq_pedido_wait( i_ebeln = gwa_zmmt0132-po_number ).
    endif.

    data(ck_continuar) = abap_true.

    while ck_continuar eq abap_true.

      "Verificar se é uma entrada de nota fiscal já gerada (GRC OutBound) """"""""""""""""
      select single * into @data(wa_zsdt0231)
        from zsdt0231
       where obj_key eq @gwa_zmmt0132-obj_key(11).

      if sy-subrc is initial.
        clear: gwa_headerdata_local-j_1bnftype.
      else.
        clear: wa_zsdt0231.
      endif.

      data: lv_json_head type string,
            lv_json_item type string,
            lv_json_acct type string,
            lv_json_cont type string,
            lv_json_mate type string,
            lv_json_with type string.

      lv_json_head = /ui2/cl_json=>serialize( data = gwa_headerdata_local ).
      lv_json_item = /ui2/cl_json=>serialize( data = git_itemdata ).
      "US162279-
      if 1 = 2.
        call function 'BAPI_INCOMINGINVOICE_CREATE'
          exporting
            headerdata       = gwa_headerdata_local
          importing
            invoicedocnumber = gva_invoicedocnumber_miro
            fiscalyear       = gva_ano_miro
          tables
            itemdata         = git_itemdata
            glaccountdata    = git_glaccountdata
            withtaxdata      = git_withtaxdata
            taxdata          = git_taxdata
            return           = git_return.
      else.
        data(lc_user_job) = zcl_job=>get_user_job( ).

        concatenate 'JOB_MIRO' gwa_zmmt0132-chave_nfe  into name separated by '_'.

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
            with pchave  = gwa_zmmt0132-chave_nfe
            with pjsonhd = lv_json_head
            with pjsonit = lv_json_item
            with pjsonac = lv_json_acct
            with pjsonct = lv_json_cont
            with pjsonmt = lv_json_mate
            with pjsonwi = lv_json_with

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
        "recupera numero da MIRO
        select single belnr gjahr
          into ( gva_invoicedocnumber_miro, gva_ano_miro )
          from zib_nfe_dist_ter
          where chave_nfe = gwa_zmmt0132-chave_nfe.

      endif.
      "US162279-
      if gva_invoicedocnumber_miro is initial.

        data(_bloq) = abap_false.
        perform f_check_msg_bloq tables git_return
                               changing _bloq gwa_return.
        if _bloq eq abap_false.
          ck_continuar = abap_false.
        else.
          message id gwa_return-id type 'S'
           number gwa_return-number
             with gwa_return-message_v1 gwa_return-message_v2 gwa_return-message_v3 gwa_return-message_v4
          display like 'E'.

          wait up to 1 seconds.
          clear: git_return[].
        endif.
      else.
        ck_continuar = abap_false.
      endif.

    endwhile.

    export p1 = 0 to memory id 'MZMMR019VLRITEM'.
    export p1 = 0 to memory id 'MZMMR019VLRITEMPIS'.
    export p1 = 0 to memory id 'MZMMR019VLRITEMCOFINS'.

    wait up to 2 seconds.

    set parameter id 'ZWERKS' field ''.
    clear gva_erro.
    if git_return[] is not initial.
      read table git_return into gwa_return with key type = 'E'.
      if sy-subrc = 0.
        gva_erro = 'X'.
      else.
        clear gva_erro.
      endif.

    endif.
  endif.

  if ( not gva_invoicedocnumber_miro is initial ) and ( not gva_ano_miro is initial ).

    if wa_zsdt0231 is not initial and wa_zsdt0231-docnum is not initial.
      data(lc_refkey) = gva_invoicedocnumber_miro && gva_ano_miro.

      update j_1bnfdoc
         set belnr = gva_invoicedocnumber_miro
             gjahr = gva_ano_miro
       where docnum eq wa_zsdt0231-docnum.

      update j_1bnflin
         set reftyp = 'LI'
             refkey = lc_refkey
             refitm = '000001'
       where docnum eq wa_zsdt0231-docnum.

    endif.

    perform z_bapi_trans_commit.

    perform change_doc_troca_texto_item in program zmmr019_04 tables git_itemdata using gva_invoicedocnumber_miro gva_ano_miro if found.

    select single belnr
      into vl_belnr
      from rbkp
     where belnr = gva_invoicedocnumber_miro
       and gjahr = gva_ano_miro.

    if sy-subrc is initial.

*  Cria mensagem de retorno em caso de sucesso
      clear gwa_return.
*  Mensagem: O documento <nr_doc> para o exercício <ano>
*            foi criado com sucesso.

      gwa_zmmt0132-ft_belnr = gva_invoicedocnumber_miro.
      gwa_zmmt0132-ft_gjahr = gva_ano_miro.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = gwa_zmmt0132-ft_belnr
        importing
          output = gwa_zmmt0132-ft_belnr.

      wa_doc_gerados-ft_belnr = gwa_zmmt0132-ft_belnr.
      wa_doc_gerados-ft_gjahr = gva_ano_miro.

      modify zmmt0132 from gwa_zmmt0132.
      commit work.

      concatenate text-001 gva_invoicedocnumber_miro
                  text-002 gva_ano_miro
                  text-003
                  into gva_message
                  separated by space.

      clear: gva_message_v1,
             gva_message_v2.

      gva_message_v1 = gva_invoicedocnumber_miro.
      gva_message_v2 = gva_ano_miro.

      gwa_return-type    = 'S'.
      gwa_return-id      = 'MM'.
      gwa_return-number  = '899'.
      gwa_return-message = gva_message.
      gwa_return-message_v1 = gva_message_v1.
      gwa_return-message_v2 = gva_message_v2.
      append gwa_return to git_return.

*  Cria mensagem de retorno em caso de sucesso
*  Retornando o docnum(fiscal) referente a MIRO criada quando existir..
      clear gwa_return.

      select single *
        into i_doc
        from j_1bnfdoc
        where belnr = gva_invoicedocnumber_miro
          and gjahr = gva_ano_miro.

      if sy-subrc eq 0.

        gwa_zmmt0132-docnum = i_doc-docnum.
        wa_doc_gerados-docnum = i_doc-docnum.
        modify zmmt0132 from gwa_zmmt0132.


        if wa_zsdt0231 is initial.

          i_doc-ntgew  = gwa_zmmt0132-entry_qnt.
          i_doc-brgew  = gwa_zmmt0132-entry_qnt.
          i_doc-inco1  = vg_inco1.
          i_doc-docref = number_ref.
          modify j_1bnfdoc from i_doc.

          if ( i_doc-nfe eq 'X' ) and ( gwa_zmmt0132-authcod is not initial ).

            select single * into i_acttab from j_1bnfe_active where docnum eq i_doc-docnum.

            if sy-subrc eq 0.

              i_doc-nfenum     = gwa_zmmt0132-nfnum.
              i_doc-authcod    = gwa_zmmt0132-authcod.
              i_doc-docstat    = gwa_zmmt0132-docstat.

              i_acttab-nfnum9  = gwa_zmmt0132-nfnum.
              i_acttab-authcod = gwa_zmmt0132-authcod.
              i_acttab-docnum9 = gwa_zmmt0132-nfenum.
              i_acttab-docsta  = gwa_zmmt0132-docstat.
              i_acttab-cdv     = gwa_zmmt0132-cdv.

              call function 'J_1B_NFE_UPDATE_ACTIVE'
                exporting
                  i_acttab  = i_acttab
                  i_doc     = i_doc
                  i_updmode = 'U'.
            endif.

          endif.

          commit work.
        endif.

*      Mensagem: O documento <nr_doc> para o exercício <ano>
*                foi criado com sucesso.
        concatenate text-001 i_doc-docnum
                    text-002 gva_ano_miro
                    text-003
                    into gva_message
                    separated by space.

        clear: gva_message_v1,
               gva_message_v2.

        gva_message_v1 = i_doc-docnum.
        gva_message_v2 = gva_ano_miro.

        gwa_return-type    = 'S'.
        gwa_return-id      = 'MM'.
        gwa_return-number  = '899'.
        gwa_return-message = gva_message.
        gwa_return-message_v1 = gva_message_v1.
        gwa_return-message_v2 = gva_message_v2.

        perform z_prepara_mensagem2   using gva_obj_key
                                         gwa_return-type
                                         gwa_return-message
                                         gwa_return-message_v1
                                         gwa_return-message_v2
                                         '05'.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_MSG_BLOQ
*&---------------------------------------------------------------------*
form f_check_msg_bloq tables p_return structure bapiret2
                    changing p_bloq   type c
                             _wl_return type bapiret2.

  data: lc_wait_int_m3_897 type i.

  p_bloq = abap_false.

  read table p_return into _wl_return with key type = 'E' id = 'ME' number = '006'.
  if sy-subrc eq 0.
    p_bloq = abap_true.
    exit.
  endif.

  read table p_return into _wl_return with key type = 'E' id = 'M3' number = '024'.
  if sy-subrc eq 0.
    p_bloq = abap_true.
    exit.
  endif.

  read table p_return into _wl_return with key type = 'E' id = 'M3' number = '682'.
  if sy-subrc eq 0.
    p_bloq = abap_true.
    exit.
  endif.

  read table p_return into _wl_return with key type = 'E' id = 'M3' number = '897'.
  if sy-subrc eq 0.

    select single *
      from setleaf into @data(_wl_set_interval_m3_897)
     where setname = 'ENT_EST_INTERVAL_M3_897'.

    if ( sy-subrc eq 0 ) and ( _wl_set_interval_m3_897-valfrom > 0 ).
      lc_wait_int_m3_897 = _wl_set_interval_m3_897-valfrom.
      wait up to lc_wait_int_m3_897 seconds.
    endif.

    select single *
      from setleaf into @data(_wl_set_exit_m3_897)
     where setname = 'ENT_EST_EXIT_M3_897'
       and valfrom = 'X'.

    if sy-subrc ne 0.
      p_bloq = abap_true.
      exit.
    endif.
  endif.

  loop at p_return into data(wa_return_interno).
    message id wa_return_interno-id type 'S'
     number wa_return_interno-number
       with wa_return_interno-message_v1 wa_return_interno-message_v2 wa_return_interno-message_v3 wa_return_interno-message_v4
    display like 'E'.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_BAPI_TRANS_COMMIT
*&---------------------------------------------------------------------*
form z_bapi_trans_commit .
  call function 'BAPI_TRANSACTION_COMMIT'
    exporting
      wait = 'X'.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM2
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
form z_prepara_mensagem2  using   p_obj_key    type any
                                  p_type       type any
                                  p_message    type any
                                  p_message_v1 type any
                                  p_message_v2 type any
                                  p_interface  type any.

  data: wa_zob_mensagem type zob_mensagem,
        wa_outreturn    type zfie_ret_document,
        it_outreturn    type table of zfie_ret_document,
        it_zob_mensagem type table of zob_mensagem.

  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = p_interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = p_type.
  wa_outreturn-id             = 'MM'.
  wa_outreturn-num            = '899'.
  wa_outreturn-message        = p_message.
  wa_outreturn-message_v1     = p_message_v1.
  wa_outreturn-message_v2     = p_message_v2.

  append wa_outreturn to it_outreturn.

  move-corresponding wa_outreturn to wa_zob_mensagem.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZOB_MENSG'
    importing
      number                  = wa_zob_mensagem-seq_registro
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.

  call function 'OIL_DATE_TO_TIMESTAMP'
    exporting
      i_date   = sy-datum
      i_time   = sy-uzeit
    importing
      e_tstamp = wa_zob_mensagem-timestamp.

  append wa_zob_mensagem to git_zob_mensagem.
  clear: wa_outreturn.
  perform z_envia_log_legado.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*       Envia log para o legado
*----------------------------------------------------------------------*
form z_envia_log_legado .

  if git_zob_mensagem[] is not initial.
    modify zob_mensagem from table git_zob_mensagem.
  endif.

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  if not git_outreturn[] is initial.
    sort git_outreturn by obj_key interface.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = git_outreturn.

    data: lv_rfc type rfcdest.

    constants: c_fm type rs38l_fnam value 'Z_FI_OUTBOUND_RETURN'.

    call function 'ZFMCPI_UTIL_GET_RFC'
      exporting
        i_fm          = c_fm
      importing
        e_rfc         = lv_rfc
      exceptions
        no_rfc        = 1
        no_rfc_config = 2
        others        = 3.

    if sy-subrc eq 0.
      call function c_fm in background task
        destination lv_rfc
        as separate unit
        tables
          outreturn = git_outreturn.
    else.
      call function c_fm in background task
        tables
          outreturn = git_outreturn.
    endif.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    commit work.

  endif.
endform.                    " Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*&      Form  Z_CABEC_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_cabec_item .

  data: vl_bsart         type ekko-bsart,
        vl_waers         type ekko-waers,
        vl_lineid        type setline,
        vl_percentual_tx type setlinet-descript,
        vl_percentual    type bprei.


  clear: git_itemdata.

  " fornecedores com partidas bloqueadas por padrão
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_ZMM0171_BLQ'
    tables
      set_values    = t_forn_block
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort t_forn_block by from.

*  IF git_item[] IS INITIAL.
*
****Dados Item
*    gwa_item-stge_loc     = gwa_zmmt0132-lgort.
*    gwa_item-move_type    = gwa_zmmt0132-move_type.
*    gwa_item-mvt_ind      = 'B'.
*    gwa_item-po_item      = gwa_zmmt0132-nu_item.
*    gwa_item-entry_qnt    = gwa_zmmt0132-entry_qnt.
*    gwa_item-po_number    = gwa_zmmt0132-po_number.
*
*    APPEND gwa_item TO git_item.
*    CLEAR gwa_item.
*
*    CONCATENATE gwa_zmmt0132-batch+0(4) '-' gwa_zmmt0132-maktx INTO gwa_headerdata-paymt_ref SEPARATED BY space.
*
*  ENDIF.

  gwa_headerdata-bline_date = gwa_zmmt0132-pstng_date.

  if gwa_zmmt0132-dt_vencimento is not initial.
    gwa_headerdata-bline_date = gwa_zmmt0132-dt_vencimento.
  endif.

  gwa_headerdata-invoice_ind    = 'X'.
  gwa_headerdata-calc_tax_ind   = 'X'.
  gwa_headerdata-comp_code      = gwa_zmmt0132-comp_code.
  gwa_headerdata-doc_type       =  'IN'.
  gwa_headerdata-doc_date       = gwa_zmmt0132-doc_date.
  gwa_headerdata-pstng_date     = gwa_zmmt0132-pstng_date.

  gwa_headerdata-del_costs_taxc = gwa_zmmt0132-del_costs_taxc.
  read table t_forn_block  with key from = gwa_zmmt0132-lifnr.
  if sy-subrc = 0.
    gwa_headerdata-pmnt_block     = 'V'.
  else.
    gwa_headerdata-pmnt_block     = ' '.                    "bug 153065
  endif.
  gwa_headerdata-pymt_meth      = gwa_zmmt0132-pymt_meth.
  gwa_headerdata-housebankid    = gwa_zmmt0132-scbank_ind.
  gwa_headerdata-bus_area       = gwa_zmmt0132-bus_area.
  gwa_headerdata-exch_rate      = gwa_zmmt0132-exch_rate_v.
  gwa_headerdata-currency       = gwa_zmmt0132-waers.

  select single waers bsart
    from ekko
    into (vl_waers, vl_bsart)
   where ebeln = gwa_zmmt0132-po_number.

  if vl_waers = 'USD'.
    gwa_headerdata-gross_amount   = gwa_zmmt0132-amount_lc.
  else.
    gwa_headerdata-gross_amount   = gwa_zmmt0132-vr_bruto.
  endif.

  gwa_headerdata-currency       = vl_waers.
  gwa_headerdata-j_1bnftype     = gwa_zmmt0132-j_1bnftype.
  gwa_headerdata-alloc_nmbr     = gwa_zmmt0132-alloc_nmbr.
  gwa_headerdata-pmnttrms       = gwa_zmmt0132-pmnttrms.
  gwa_headerdata-ref_doc_no     = gwa_zmmt0132-nt_remessa.

  gwa_itemdata-invoice_doc_item = '000001'.
  gwa_itemdata-po_number        = gwa_zmmt0132-po_number.
  gwa_itemdata-po_item          = gwa_zmmt0132-po_item.
  gwa_itemdata-tax_code         = gwa_zmmt0132-del_costs_taxc.
  gwa_itemdata-quantity         = gwa_zmmt0132-quantity.
  gwa_itemdata-po_unit          = gwa_zmmt0132-meins.


  if gwa_zmmt0132-webre = 'X'.
    gwa_itemdata-ref_doc          = gwa_zmmt0132-mm_mblnr.
    gwa_itemdata-ref_doc_year     = gwa_zmmt0132-mm_mjahr.
    gwa_itemdata-ref_doc_it        = '000001'.
  endif.


*  gwa_itemdata-item_amount      = gwa_zmmt0132-item_amount - gwa_zmmt0132-vr_impostos - gva_vr_percepcoes .
  select single *
    from ekpo
    into @data(w_ekpo)
    where ebeln = @gwa_zmmt0132-po_number
    and   ebelp = @gwa_zmmt0132-po_item.

  gwa_itemdata-item_amount =  ( w_ekpo-netpr / w_ekpo-peinh ) * gwa_zmmt0132-quantity.

  if vl_waers = 'USD'.
    gwa_itemdata-item_amount =  gwa_itemdata-item_amount / gwa_zmmt0132-exch_rate_v.
  endif.

  select single bprme
    from ekpo
    into @data(vbprme)
    where ebeln = @gwa_zmmt0132-po_number
    and   ebelp = @gwa_zmmt0132-po_item.

  if vbprme ne gwa_itemdata-po_unit.
    gwa_itemdata-po_pr_uom        = vbprme.
  endif.

  add gwa_zmmt0132-item_amount  to gva_total_item.
  append gwa_itemdata to git_itemdata.

  clear: gwa_itemdata.

endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form check_data  using   p_er_data_changed
                type ref to cl_alv_changed_data_protocol.

  data: ls_lvc_modi type lvc_s_modi.
  data: mod_data type lvc_t_modi.
  data: l_dt_vencimento type ty_saida-dt_vencimento,
        l_bvtyp         type ty_saida-bvtyp,
        l_amount_lc     type ty_saida-amount_lc,
        l_exch_rate_v   type ty_saida-exch_rate_v,
        l_netwr         type ty_saida-netwr,
        l_j_1bnftype    type ty_saida-j_1bnftype,
        l_nfnum_s       type ty_saida-nfnum_s,
        l_charg         type ty_saida-charg.

  loop at p_er_data_changed->mt_mod_cells into ls_lvc_modi.
    case ls_lvc_modi-fieldname.
      when 'DT_VENCIMENTO'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_dt_vencimento.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'DT_VENCIMENTO'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_dt_vencimento to gwa_saida-dt_vencimento.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting dt_vencimento.
        endif.

      when 'BVTYP'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_bvtyp.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'BVTYP'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_bvtyp to gwa_saida-bvtyp.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting bvtyp.
        endif.

      when 'AMOUNT_LC'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_amount_lc.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'AMOUNT_LC'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_amount_lc to gwa_saida-amount_lc.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting amount_lc.
        endif.

      when 'EXCH_RATE_V'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_exch_rate_v.

        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = 'NETWR'
          importing
            e_value     = l_netwr.

        if l_exch_rate_v is not initial and l_netwr is not initial.
          l_amount_lc = l_netwr / l_exch_rate_v.
        endif.


        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'EXCH_RATE_V'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_exch_rate_v to gwa_saida-exch_rate_v.
          move l_amount_lc to gwa_saida-amount_lc.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting exch_rate_v amount_lc.
        endif.

      when 'J_1BNFTYPE'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_j_1bnftype.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'J_1BNFTYPE'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_j_1bnftype to gwa_saida-j_1bnftype.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting j_1bnftype.
        endif.

      when 'NFNUM_S'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_nfnum_s.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'NFNUM_S'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_nfnum_s to gwa_saida-nfnum_s.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting nfnum_s.
        endif.

      when 'CHARG'.
        call method p_er_data_changed->get_cell_value
          exporting
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          importing
            e_value     = l_charg.

        read table p_er_data_changed->mt_good_cells
        into ls_lvc_modi
        with key row_id = ls_lvc_modi-row_id
        fieldname = 'CHARG'.

        if sy-subrc = 0.
          read table git_saida into gwa_saida index ls_lvc_modi-row_id.
          move l_charg to gwa_saida-charg.
          modify git_saida from gwa_saida index ls_lvc_modi-row_id transporting charg.
        endif.

    endcase.
    wa_stable-row        = 'X'.
    wa_stable-col        = 'X'.
    call method gob_gui_alv_grid->refresh_table_display
      exporting
        is_stable = wa_stable.
  endloop.



endform.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_MIRO
*&---------------------------------------------------------------------*
form verifica_miro  using     p_gwa_zmmt0132 type zmmt0132
                              p_gwa_zmmt0132_obj_key type zmmt0132-obj_key
                     changing p_ck_cotinue type char01
                              vg_return    type string.
  select single docnum
    into @data(v_docnum)
      from zsdt0231
     where obj_key eq @p_gwa_zmmt0132-obj_key(11).

  if sy-subrc is initial.

    select single refkey
      into @data(v_refkey)
        from j_1bnflin
      where docnum eq @v_docnum.

    if sy-subrc is initial.

      select single belnr
        into @data(v_belnr)
          from rbkp
        where belnr eq @v_refkey(10)
          and stblg eq @abap_false.

      if sy-subrc is initial.
        vg_return = |Já tem uma MIRO ativa para essa Entrada de estoque MIRO: { v_belnr }!|.
        p_ck_cotinue = abap_true.
      endif.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_MIRO
*&---------------------------------------------------------------------*
form z_estorna_miro using lwa_doc_gerados type zmmt0132
                 changing p_erro.

  data: vl_msg_estorno type bapiret2-message.

  data: ls_doc_new    type j_1bnfdoc,
        ls_acttab_new type j_1bnfe_active.

  data: lwa_zsdt0231 type zsdt0231.

  data: wa_invoicedocnumber type bapi_incinv_fld,
        vl_pstng_date_est   type bapi_incinv_fld-pstng_date,
        vg_number_miro_est  type bapi_incinv_fld-inv_doc_no.

  clear: wa_invoicedocnumber, vl_pstng_date_est, vg_number_miro_est, ls_doc_new, ls_acttab_new,
         p_erro, lwa_zsdt0231.

  check ( lwa_doc_gerados-ft_belnr is not initial ) and
        ( lwa_doc_gerados-ft_gjahr is not initial ).

  select single * into ls_doc_new
    from j_1bnfdoc
   where belnr = lwa_doc_gerados-ft_belnr
     and gjahr = lwa_doc_gerados-ft_gjahr.

  if ( sy-subrc = 0 ) and ( ls_doc_new-nfe eq 'X' ) and ( gwa_zmmt0132-authcod is not initial ).

    select single *
      from zsdt0231 into lwa_zsdt0231
     where docnum eq ls_doc_new-docnum.

    if sy-subrc ne 0.

      call function 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
        exporting
          i_docnum = ls_doc_new-docnum
        importing
          e_acttab = ls_acttab_new
        exceptions
          no_entry = 1
          others   = 2.

      if sy-subrc eq 0.
        "Nota Fiscal
        move '1'                   to ls_doc_new-docstat.
        "Active Nota Fiscal
        move 'C'                   to ls_acttab_new-action_requ.
        move '2'                   to ls_acttab_new-scssta.
        move 'X'                   to ls_acttab_new-cancel.
        move 'B'                   to ls_acttab_new-msstat.

        modify j_1bnfdoc      from ls_doc_new.
        modify j_1bnfe_active from ls_acttab_new.
      endif.

    endif.

  endif.

* Dados da Miro/Estorno
  wa_invoicedocnumber-inv_doc_no = lwa_doc_gerados-ft_belnr.
  wa_invoicedocnumber-fisc_year  = lwa_doc_gerados-ft_gjahr.
  wa_invoicedocnumber-reason_rev = '01'.
*  vl_pstng_date_est              = gwa_header-pstng_date.

  clear: gwa_return.
  free: git_return.

  data(ck_continuar) = abap_true.

  while ck_continuar eq abap_true.

*   BAPI Estorno da MIRO
    call function 'BAPI_INCOMINGINVOICE_CANCEL'
      exporting
        invoicedocnumber          = wa_invoicedocnumber-inv_doc_no
        fiscalyear                = wa_invoicedocnumber-fisc_year
        reasonreversal            = wa_invoicedocnumber-reason_rev
        postingdate               = vl_pstng_date_est
      importing
        invoicedocnumber_reversal = vg_number_miro_est
      tables
        return                    = git_return.

    read table git_return into gwa_return with key type = 'E'.
    if sy-subrc eq 0.

      data(_bloq) = abap_false.
      perform f_check_msg_bloq tables git_return changing _bloq gwa_return.
      if _bloq eq abap_false.
        ck_continuar = abap_false.
      else.
        message id gwa_return-id type 'S' number gwa_return-number with gwa_return-message_v1 gwa_return-message_v2 gwa_return-message_v3 gwa_return-message_v4
        display like 'E'.

        wait up to 1 seconds.
        clear: git_return[].
      endif.

    else.
      ck_continuar = abap_false.
    endif.

  endwhile.

* BAPI TRANSACTION COMMIT
  perform: z_bapi_trans_commit.

  "Identificação da interface, MIRO
  gva_interface = '12'.
  loop at git_return into gwa_return.
    clear: vl_msg_estorno.
    concatenate gwa_return-message '(Estorno)'
           into vl_msg_estorno separated by space.

    perform z_prepara_mensagem   using gva_obj_key
                                       gwa_return-type
                                       vl_msg_estorno
                                       gva_interface.
  endloop.

  read table git_return into gwa_return with key type = 'E'.
  if sy-subrc eq 0.
    p_erro = 'X'.
  else.

    call function 'ZGRC_LIMPA_REF_MIRO_FISCAL'
      exporting
        invoicedocnumber = lwa_doc_gerados-ft_belnr
        fiscalyear       = lwa_doc_gerados-ft_gjahr.

    delete git_outreturn where obj_key eq gwa_zmmt0132-obj_key
                          and type    eq 'S'
                          and message_v1 = lwa_doc_gerados-ft_belnr
                          and message_v2 = lwa_doc_gerados-ft_gjahr.

    clear: lwa_doc_gerados-ft_belnr,
           lwa_doc_gerados-ft_gjahr.

*    DELETE FROM zmmt0132 WHERE obj_key EQ lwa_doc_gerados-obj_key.
*    COMMIT WORK.

  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_MIGO
*&---------------------------------------------------------------------*
*       Estornar MIGO
*----------------------------------------------------------------------*
form z_estorna_migo using lwa_doc_gerados type zmmt0132
                    changing p_erro.

  data: lwa_estorno    type zmms003,
        lwa_doc_year   type bapi2017_gm_head_02-doc_year,
        lwa_pstng_date type bapi2017_gm_head_02-pstng_date,
        lwa_mat_doc    type bapi2017_gm_head_ret-mat_doc,
        lwa_head_ret   type bapi2017_gm_head_ret.

  data: vl_msg_estorno type bapiret2-message.

* Dados da Migo
*  MOVE: gva_invoicedocnumber_migo  TO lwa_estorno-mblnr,
*        gva_ano_migo               TO lwa_estorno-mjahr,
*        gwa_header-pstng_date      TO lwa_estorno-budat.

* Dados para Estorno
  clear: lwa_mat_doc,
         lwa_doc_year,
         lwa_pstng_date,
         p_erro.

*  lwa_mat_doc    = lwa_estorno-mblnr.
*  lwa_doc_year   = lwa_estorno-mjahr.
*  lwa_pstng_date = lwa_estorno-budat.


  check ( lwa_doc_gerados-mm_mblnr is not initial ) and
        ( lwa_doc_gerados-mm_mjahr is not initial ).

  lwa_mat_doc    = lwa_doc_gerados-mm_mblnr.
  lwa_doc_year   = lwa_doc_gerados-mm_mjahr.
  lwa_pstng_date = lwa_doc_gerados-pstng_date.


  clear: gwa_return.
  free: git_return.

  data(ck_continuar) = abap_true.

  while ck_continuar eq abap_true.

*   BAPI Estorno da MIGO
    call function 'BAPI_GOODSMVT_CANCEL'
      exporting
        materialdocument    = lwa_mat_doc
        matdocumentyear     = lwa_doc_year
        goodsmvt_pstng_date = lwa_pstng_date
      importing
        goodsmvt_headret    = lwa_head_ret
      tables
        return              = git_return.

    read table git_return into gwa_return with key type = 'E'.
    if sy-subrc eq 0.

      data(_bloq) = abap_false.
      perform f_check_msg_bloq tables git_return changing _bloq gwa_return.
      if _bloq eq abap_false.
        ck_continuar = abap_false.
      else.
        message id gwa_return-id type 'S' number gwa_return-number with gwa_return-message_v1 gwa_return-message_v2 gwa_return-message_v3 gwa_return-message_v4
        display like 'E'.

        wait up to 1 seconds.
        clear: git_return[].
      endif.
    else.
      ck_continuar = abap_false.
    endif.

  endwhile.

* BAPI TRANSACTION COMMIT
  perform: z_bapi_trans_commit.

  "Identificação da interface, MIGO
  gva_interface = '11'.
  loop at git_return into gwa_return.
    clear: vl_msg_estorno.
    concatenate gwa_return-message '(Estorno)'
           into vl_msg_estorno separated by space.

    perform z_prepara_mensagem   using gva_obj_key
                                       gwa_return-type
                                       vl_msg_estorno
                                       gva_interface.
  endloop.

  read table git_return into gwa_return with key type = 'E'.
  if sy-subrc eq 0.
    p_erro = 'X'.
  else.
    delete git_outreturn where obj_key eq gwa_zmmt0132-obj_key
                          and type    eq 'S'
                          and message_v1 = lwa_mat_doc
                          and message_v2 = lwa_doc_year.

    clear: lwa_doc_gerados-mm_mblnr,
           lwa_doc_gerados-mm_mjahr.

    delete from zmmt0132 where obj_key eq lwa_doc_gerados-obj_key.
    commit work.

  endif.


endform.                    " Z_ESTORNA_MIGO
*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
form z_prepara_mensagem  using    p_obj_key    type any
                                  p_type       type any
                                  p_message    type any
                                  p_interface  type any.

  data: wa_zob_mensagem type zob_mensagem.

  gwa_outreturn-obj_key        = p_obj_key.
  gwa_outreturn-interface      = p_interface.
  gwa_outreturn-dt_atualizacao = sy-datum.
  gwa_outreturn-hr_atualizacao = sy-uzeit.
  gwa_outreturn-type           = p_type.
  gwa_outreturn-id             = 'MM'.
  gwa_outreturn-num            = '899'.
  gwa_outreturn-message        = p_message.

  append gwa_outreturn to git_outreturn.

  move-corresponding gwa_outreturn to gwa_zob_mensagem.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZOB_MENSG'
    importing
      number                  = wa_zob_mensagem-seq_registro
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.

  call function 'OIL_DATE_TO_TIMESTAMP'
    exporting
      i_date   = sy-datum
      i_time   = sy-uzeit
    importing
      e_tstamp = wa_zob_mensagem-timestamp.

  append gwa_zob_mensagem to git_zob_mensagem.
  clear gwa_outreturn.

endform.                    " Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  ADD_PIS_COFINS
*&---------------------------------------------------------------------*
form add_pis_cofins  using p_entrada  type zmmt0132
  changing vl_imposto type bapi_rmwwr
           vl_item    type bapiwrbtr.

  data: vl_imposto_pis type bapi_rmwwr.
  data: vl_imposto_cofins type bapi_rmwwr.


  select single * into @data(wa_zmmt0125)
    from zmmt0125
   where mwskz eq @p_entrada-del_costs_taxc	.

  vl_imposto_pis =  0.
  vl_imposto_cofins = 0.

  if sy-subrc is initial.

    if wa_zmmt0125-nm_ali_pis is not initial.
      vl_imposto_pis = p_entrada-vr_bruto * ( wa_zmmt0125-nm_ali_pis / 100 ).
    endif.

    if wa_zmmt0125-nm_ali_cofins is not initial.
      vl_imposto_cofins = p_entrada-vr_bruto * ( wa_zmmt0125-nm_ali_cofins / 100 ).
    endif.

    add vl_imposto_pis to vl_imposto.
    add vl_imposto_cofins to vl_imposto.

    export p1 = vl_item to memory id 'MZMMR019VLRITEM'.
    export p1 = vl_imposto_pis to memory id 'MZMMR019VLRITEMPIS'.
    export p1 = vl_imposto_cofins to memory id 'MZMMR019VLRITEMCOFINS'.

    vl_item = vl_item - ( vl_imposto_pis + vl_imposto_cofins ).

  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  FM_STYLE_CELLTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_style_celltab .
*  Estrutura que ira alimentar a tabela com os layout especificos de cada campo
* de cada registro.
  data :
    lst_style type lvc_s_styl,
    l_tabix   type sy-tabix.

  loop at git_saida into gwa_saida.

    l_tabix = sy-tabix.


    if gwa_saida-mm_mblnr is not initial. " Ja tem MIRO/MIGO

      lst_style-fieldname = 'LGORT'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'DT_VENCIMENTO'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'J_1BNFTYPE'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'BVTYP'.
      lst_style-style = cl_gui_alv_grid=>mc_style_f4_no + cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'PED_COMP'.
      lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'NFNUM_S'.
      lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'CHARG'.
      lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'AMOUNT_LC'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'EXCH_RATE_V'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

    else.

      if gwa_saida-celltab is not initial.

        refresh:  gwa_saida-celltab.

        lst_style-fieldname = 'DT_VENCIMENTO'.
        lst_style-style     = cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'J_1BNFTYPE'.
        lst_style-style     = cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'BVTYP'.
        lst_style-style = cl_gui_alv_grid=>mc_style_f4_no + cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'PED_COMP'.
        lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'NFNUM_S'.
        lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'CHARG'.
        lst_style-style     =  cl_gui_alv_grid=>mc_style_hotspot_no + cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'AMOUNT_LC'.
        lst_style-style     = cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.

        lst_style-fieldname = 'EXCH_RATE_V'.
        lst_style-style     = cl_gui_alv_grid=>mc_style_enabled.
        insert lst_style into table gwa_saida-celltab.
      endif.
    endif.

*>>>Begin-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo
    data(lv_matnr) = |{ gwa_saida-matnr alpha = out }| .
    data(ls_zmmt0179) = value #( git_zmmt0179[ matnr = lv_matnr ] optional ).

    if ls_zmmt0179-matnr is not initial.
      lst_style-fieldname = 'CHARG'.
      lst_style-style     =  cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.
    endif.
*<<<End-MM -Correção - ZMM0171 - Notas Quimicos #144181-09.07.2024- Vitor Rienzo

    if gwa_saida-waers = 'BRL'.
      lst_style-fieldname = 'AMOUNT_LC'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.

      lst_style-fieldname = 'EXCH_RATE_V'.
      lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      insert lst_style into table gwa_saida-celltab.
    endif.


    modify git_saida from gwa_saida index l_tabix.
  endloop.

  data lr_inputui1 type ref to cl_salv_wd_fe_input_field.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1001   text
*      -->P_1002   text
*      -->P_1003   text
*----------------------------------------------------------------------*
form f_preencher_dynpro using l_start type c l_name type c l_value.

  move l_start to gwa_bdc-dynbegin.

  if l_start = 'X'.
    move:
  l_name  to gwa_bdc-program,
  l_value to gwa_bdc-dynpro.
  else.
    move:
      l_name  to gwa_bdc-fnam,
      l_value to gwa_bdc-fval.
  endif.
  append gwa_bdc to git_bdc.
  clear: gwa_bdc.

endform.
*&---------------------------------------------------------------------*
*&      Form  FM_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_clear_data .

  clear:git_zsdt0001,
        gwa_zsdt0001,
        git_zmmt0132_aux,
        gwa_zmmt0132_aux,
        git_zmmt_ee_zgr_docs,
        gwa_zmmt_ee_zgr_docs,
        git_zmmt_ee_zgr,
        gwa_zmmt_ee_zgr,
        git_zib_nfe_dist_itm,
        gwa_zib_nfe_dist_itm,
        git_ekko,
        gwa_ekko,
        git_ekpo,
        gwa_ekpo,
        git_eket,
        gwa_eket,
        git_ekbe,
        gwa_ekbe,
        git_lfa1,
        gwa_lfa1,
        git_makt,
        gwa_makt,
        gra_obj_key[],
        gra_matkl[],
        git_saida[],
        gwa_saida,
        git_ekpo_tot[].


endform.
*&---------------------------------------------------------------------*
*&      Form  FM_MIRO_MIGO_ESTORNO
*&---------------------------------------------------------------------*
form fm_miro_migo_estorno  tables p_et_index_rows
                        structure lvc_s_row.

  data: lwa_doc_gerados type zmmt0132,
        doc_number_ref  type j_1bdocnum.

  data: lwa_selected_line like lvc_s_row,
        lf_row_index      type lvc_index.

  data: lva_erro_est_migo type c,
        lva_erro_est_miro type c.

  loop at p_et_index_rows into lwa_selected_line.

    lf_row_index = lwa_selected_line-index.

    read table git_saida index lf_row_index into gwa_saida.

    "IF gwa_saida-status <> icon_complete.

    if gwa_saida-ebeln is not initial.

      check gwa_saida-mm_mblnr is not initial or
            gwa_saida-ft_belnr is not initial.

*      CHECK gwa_saida-mm_mblnr IS NOT INITIAL.
*      CHECK gwa_saida-mm_mjahr IS NOT INITIAL.
*      CHECK gwa_saida-ft_belnr IS NOT INITIAL.
*      CHECK gwa_saida-ft_gjahr IS NOT INITIAL.

      gwa_zmmt0132-obj_key        = gwa_saida-obj_key.
      gwa_zmmt0132-po_number      = gwa_saida-ebeln.
      gwa_zmmt0132-lifnr          = gwa_saida-parid.
      gwa_zmmt0132-move_type      = '101'.
      gwa_zmmt0132-nt_remessa     = gwa_saida-nfnum_s.
      gwa_zmmt0132-mm_mblnr       = gwa_saida-mm_mblnr.
      gwa_zmmt0132-mm_mjahr       = gwa_saida-mm_mjahr.
      gwa_zmmt0132-ft_belnr       = gwa_saida-ft_belnr.
      gwa_zmmt0132-ft_gjahr       = gwa_saida-ft_gjahr.
      gwa_zmmt0132-doc_date       = gwa_saida-docdat.
      gwa_zmmt0132-pstng_date     = gwa_saida-dt_movimento.
      gwa_zmmt0132-dt_vencimento  = gwa_saida-dt_vencimento.
      gwa_zmmt0132-po_item        = gwa_saida-ebelp.
      gwa_zmmt0132-entry_qnt      = gwa_saida-peso_fiscal.
      gwa_zmmt0132-meins          = gwa_saida-meins.
      gwa_zmmt0132-in_aviso_receb = 'N'.
      gwa_zmmt0132-peso_bruto     = gwa_saida-peso_fiscal.
      gwa_zmmt0132-comp_code      = s_bukrs-low.
      gwa_zmmt0132-tp_operacao    = '01'.
      gwa_zmmt0132-ref_doc_no     = gwa_saida-nfnum_s.
      gwa_zmmt0132-vr_bruto       = gwa_saida-netwr.
      gwa_zmmt0132-del_costs_taxc = gwa_saida-mwskz.
      gwa_zmmt0132-pmnt_block     = 'V'.
      gwa_zmmt0132-pmnttrms       = gwa_saida-zterm.
      "gwa_zmmt0132-scbank_ind     = 'BBRA'.

      gwa_zmmt0132-scbank_ind    = zcl_miro=>get_banco_forma_pagamento( i_bukrs = gwa_zmmt0132-comp_code i_forma_pagamento  = 'S' ).


      gwa_zmmt0132-plant          = s_branch-low.
      gwa_zmmt0132-j_1bnftype     = gwa_saida-j_1bnftype.
      gwa_zmmt0132-alloc_nmbr     = gwa_saida-ebeln.
      gwa_zmmt0132-gross_amount   = gwa_saida-netwr.
      gwa_zmmt0132-item_amount    = gwa_saida-netwr.
      gwa_zmmt0132-material       = gwa_saida-matnr.
      gwa_zmmt0132-nu_item        = gwa_saida-ebelp.
      gwa_zmmt0132-taxjurcode     = gwa_saida-txjcd.
      gwa_zmmt0132-zdt_atlz       = sy-datum.
      gwa_zmmt0132-zhr_atlz       = sy-uzeit.
      gwa_zmmt0132-zrg_atlz       = 0.
      gwa_zmmt0132-pymt_meth      = 'E'.
      gwa_zmmt0132-bus_area       = s_branch-low.
      gwa_zmmt0132-quantity       = gwa_saida-peso_fiscal.
      gwa_zmmt0132-batch          = gwa_saida-charg.
      gwa_zmmt0132-lgort          = gwa_saida-lgort.
      gwa_zmmt0132-vr_impostos    = gwa_saida-icms_base.
      gwa_zmmt0132-amount_lc      = gwa_saida-amount_lc.
      gwa_zmmt0132-exch_rate_v    = gwa_saida-exch_rate_v.
      gwa_zmmt0132-bvtyp          = gwa_saida-bvtyp.
      gwa_zmmt0132-nfnum          = gwa_saida-nfnum.
      gwa_zmmt0132-series         = gwa_saida-series.
      gwa_zmmt0132-webre          = gwa_saida-webre.

      move-corresponding gwa_zmmt0132 to lwa_doc_gerados.

      perform z_estorna_miro using lwa_doc_gerados
                      changing lva_erro_est_miro.

      if lva_erro_est_miro is initial.
        perform z_estorna_migo using lwa_doc_gerados
                         changing lva_erro_est_migo.
      endif.


      if lva_erro_est_miro is initial and lva_erro_est_migo is initial.
        concatenate gwa_saida-nfnum  '-' gwa_saida-series into gwa_saida-nfnum_s.
        gwa_saida-status = icon_generate.
        clear:  gwa_saida-mm_mblnr        ,
                gwa_saida-mm_mjahr        ,
                gwa_saida-ft_belnr        ,
                gwa_saida-ft_gjahr        ,
                gwa_saida-dt_vencimento   ,
                gwa_saida-amount_lc       ,
                gwa_saida-j_1bnftype      ,
                gwa_saida-exch_rate_v     ,
                gwa_saida-bvtyp           ,
                gwa_saida-ebeln           ,
                gwa_saida-ebelp           ,
                gwa_saida-charg           .

        gwa_saida-ebeln = icon_icon_list.

        modify git_saida from gwa_saida index lf_row_index.
      endif.

      clear: gwa_zmmt0132,
             lva_erro_est_miro,
             lva_erro_est_migo.

    else.
      message  'Pedido de Compra não informado' type 'S' display like 'E'.
      exit.
    endif.
*    ELSE.
*      MESSAGE  'MIRO/MIGO já gerados nessa seleção' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
  endloop.
endform.

*&---------------------------------------------------------------------*
*& Form z_check_lucas
*&---------------------------------------------------------------------*
form z_check_lucas changing p_lucas.

  check s_branch-low eq c_lucas.

  select count(*) into @data(lv_count)
    from zib_nfe_dist_ter as a inner join j_1bbranch as b
    on a~forne_cnpj eq b~stcd1
    where a~chave_nfe eq @gwa_saida-chave_nfe.

  if lv_count ne 0.
    p_lucas = abap_true.
  endif.
  clear lv_count.
endform.
*&---------------------------------------------------------------------*
*& Form z_get_refkey
*&---------------------------------------------------------------------*
form z_get_refkey changing p_refdoc.
  data: wa_active_key type j_1bnfe_active.

  wa_active_key-regio      = gwa_saida-chave_nfe(2).
  wa_active_key-nfyear     = gwa_saida-chave_nfe+2(2).
  wa_active_key-nfmonth    = gwa_saida-chave_nfe+4(2).
  wa_active_key-stcd1      = gwa_saida-chave_nfe+6(14).
  wa_active_key-model      = gwa_saida-chave_nfe+20(2).
  wa_active_key-serie      = gwa_saida-chave_nfe+22(3).
  wa_active_key-nfnum9     = gwa_saida-chave_nfe+25(9).
  wa_active_key-docnum9    = gwa_saida-chave_nfe+34(9).
  wa_active_key-cdv        = gwa_saida-chave_nfe+43(1).

  select single * into @data(wa_active)
   from j_1bnfe_active as a
   inner join j_1bnfdoc as b
   on b~docnum = a~docnum
   and b~direct = '2'
*   AND b~candat = ' '
  where a~regio    eq @wa_active_key-regio
    and a~nfyear   eq @wa_active_key-nfyear
    and a~nfmonth  eq @wa_active_key-nfmonth
    and a~stcd1    eq @wa_active_key-stcd1
    and a~model    eq @wa_active_key-model
    and a~serie    eq @wa_active_key-serie
    and a~nfnum9   eq @wa_active_key-nfnum9
    and a~docnum9  eq @wa_active_key-docnum9
    and a~cdv      eq @wa_active_key-cdv.

  if sy-subrc eq 0.
    select single refkey into @data(lv_refkey)
      from j_1bnflin
      where docnum = @wa_active-a-docnum.
    if sy-subrc eq 0.
      p_refdoc = lv_refkey(10).
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form z_check_pedido
*&---------------------------------------------------------------------*
form z_check_pedido .

*  SELECT * FROM ekko INTO TABLE @DATA(lt_ekko_bg)
*    WHERE ebeln = gwa_zib_nfe_dist_itm-prod_pedido_comp.
*
*    IF sy-subrc EQ 0.
*      SELECT * FROM ekpo INTO TABLE @DATA(lt_ekpo_bg)
*
*    ENDIF.

endform.
*&---------------------------------------------------------------------*
*& Form fm_dados_processa_bg
*&---------------------------------------------------------------------*
form fm_dados_processa_bg .

  loop at git_zsdt0001 into gwa_zsdt0001.

    if sy-batch eq abap_true and s_branch-low eq c_lucas.
      read table git_zmmt0132_aux into gwa_zmmt0132_aux with key obj_key = gwa_zsdt0001-chave_nfe.
      if sy-subrc eq 0.
        check gwa_zmmt0132_aux-mm_mblnr is initial.
      endif.
    endif.

    " status
    gwa_saida-parid = gwa_zsdt0001-parid.

    read table git_lfa1 into gwa_lfa1 with key lifnr = gwa_zsdt0001-parid.
    gwa_saida-name1 = gwa_lfa1-name1.

    read table  git_makt into gwa_makt with key matnr = gwa_zsdt0001-matnr.

    gwa_saida-matnr = gwa_zsdt0001-matnr.
    gwa_saida-maktx = gwa_makt-maktx.

    read table git_ekko into gwa_ekko with key lifnr = gwa_zsdt0001-parid.

    if gwa_saida-ebeln is initial.

      move icon_icon_list  to gwa_saida-ebeln.

    endif.

    gwa_saida-waers = gwa_ekko-waers.

*    gwa_saida-dt_movimento =  gwa_zsdt0001-dt_movimento.
    gwa_saida-dt_movimento =  sy-datum.
    gwa_saida-docdat       =  gwa_zsdt0001-docdat.
    gwa_saida-peso_fiscal  =  gwa_zsdt0001-peso_fiscal.
    gwa_saida-peso_liq     =  gwa_zsdt0001-peso_liq.
*    gwa_saida-meins        =  gwa_ekpo-meins.
*    gwa_saida-mwskz        =  gwa_ekpo-mwskz.
*    gwa_saida-lgort        =  gwa_ekpo-lgort.
*    gwa_saida-webre        =  gwa_ekpo-webre.
    "gwa_saida-charg        =  gwa_ekpo-charg.

    concatenate gwa_zsdt0001-nfnum '-' gwa_zsdt0001-series into
    gwa_saida-nfnum_s.

    gwa_saida-nfnum  =  gwa_zsdt0001-nfnum.
    gwa_saida-series =  gwa_zsdt0001-series.
    gwa_saida-netwr  =  gwa_zsdt0001-netwr.

    read table git_zib_nfe_dist_itm into gwa_zib_nfe_dist_itm with key
    chave_nfe = gwa_zsdt0001-chave_nfe.

    gwa_saida-icms_valor    = gwa_zib_nfe_dist_itm-icms_valor.
    gwa_saida-ipi_valor     = gwa_zib_nfe_dist_itm-ipi_valor.
    gwa_saida-pis_valor     = gwa_zib_nfe_dist_itm-pis_valor .
    gwa_saida-cof_valor      = gwa_zib_nfe_dist_itm-cof_valor.
    gwa_saida-icms_base     = gwa_zib_nfe_dist_itm-icms_base.
    gwa_saida-icms_aqt      = gwa_zib_nfe_dist_itm-icms_aqt.
    gwa_saida-icms_red_base = gwa_zib_nfe_dist_itm-icms_red_base.
    gwa_saida-chave_nfe     = gwa_zsdt0001-chave_nfe.

    clear: gva_obj_key.
    concatenate gwa_zsdt0001-ch_referencia  gwa_zsdt0001-nr_romaneio
    into gva_obj_key.

    gwa_saida-obj_key       = gva_obj_key.


    read table git_zmmt_ee_zgr into gwa_zmmt_ee_zgr with key
    obj_key = gwa_saida-obj_key.
    gwa_saida-nfnum2  =   gwa_zmmt_ee_zgr-nfenum.
    gwa_saida-authcod =   gwa_zmmt_ee_zgr-authcod.
    gwa_saida-nfenum  =   gwa_zmmt_ee_zgr-nfenum.
    gwa_saida-docstat =   gwa_zmmt_ee_zgr-docstat.
    gwa_saida-cdv     =   gwa_zmmt_ee_zgr-cdv.

    read table git_zmmt0132_aux into gwa_zmmt0132_aux with key
     obj_key = gwa_saida-obj_key.

    perform z_check_pedido.

    if sy-subrc = 0.
      gwa_saida-mm_mblnr        = gwa_zmmt0132_aux-mm_mblnr.
      gwa_saida-mm_mjahr        = gwa_zmmt0132_aux-mm_mjahr.
      gwa_saida-ft_belnr        = gwa_zmmt0132_aux-ft_belnr.
      gwa_saida-ft_gjahr        = gwa_zmmt0132_aux-ft_gjahr.
      gwa_saida-dt_vencimento   = gwa_zmmt0132_aux-dt_vencimento.
      gwa_saida-amount_lc       = gwa_zmmt0132_aux-amount_lc.
      gwa_saida-j_1bnftype      = gwa_zmmt0132_aux-j_1bnftype.
      gwa_saida-exch_rate_v     = gwa_zmmt0132_aux-exch_rate_v.
      gwa_saida-bvtyp           = gwa_zmmt0132_aux-bvtyp.
      gwa_saida-ebeln           = gwa_zmmt0132_aux-po_number.
      gwa_saida-ebelp           = gwa_zmmt0132_aux-po_item.
      gwa_saida-lgort           = gwa_zmmt0132_aux-lgort.
      gwa_saida-docnum          = gwa_zmmt0132_aux-docnum.
      concatenate  gwa_zmmt0132_aux-nfnum '-'  gwa_zmmt0132_aux-series into gwa_saida-nfnum_s .
      gwa_saida-charg           = gwa_zmmt0132_aux-batch .

      read table git_ekpo into gwa_ekpo with key ebeln = gwa_saida-ebeln.
      if sy-subrc is initial.
        gwa_saida-meins   =  gwa_ekpo-meins.
        gwa_saida-mwskz   =  gwa_ekpo-mwskz.
        gwa_saida-lgort   =  gwa_ekpo-lgort.
        gwa_saida-webre   =  gwa_ekpo-webre.
*      gwa_saida-charg   =  gwa_ekpo-charg.
        gwa_saida-txjcd  = gwa_ekpo-txjcd.
        gwa_saida-txz01  = gwa_ekpo-txz01.

        read table git_ekko into gwa_ekko with key ebeln = gwa_saida-ebeln.
        if sy-subrc is initial.
          gwa_saida-zterm  = gwa_ekko-zterm.
          perform z_dados_bancarios.
        endif.
      endif.

*      READ TABLE git_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j1bnfdoc>)
*      WITH KEY belnr = gwa_zmmt0132_aux-ft_belnr
*               gjahr = gwa_zmmt0132_aux-ft_gjahr
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        gwa_saida-docnum  =   <fs_j1bnfdoc>-docnum.
*      ENDIF.

    else.
      gwa_saida-j_1bnftype      = 'NE'.
    endif.

    if gwa_saida-mm_mblnr is initial or gwa_saida-ft_belnr is initial.
      move icon_generate   to gwa_saida-status.
    else.
      if gwa_saida-mm_mblnr is not initial or  gwa_saida-ft_belnr is not initial.
        move icon_complete  to  gwa_saida-status .
      endif.
    endif.

*    gwa_saida-docnum     = gwa_zmmt_ee_zgr_docs-docnum.

    concatenate gwa_zsdt0001-ch_referencia gwa_zsdt0001-nr_romaneio into
    gwa_saida-ch_referencia.

    read table git_eket into gwa_eket with key ebeln = gwa_ekpo-ebeln
                                               ebelp = gwa_ekpo-ebelp.

    gwa_saida-charg  = gwa_zsdt0001-nr_safra.
*    gwa_saida-zterm  = gwa_ekko-zterm.
*    gwa_saida-txjcd  = gwa_ekpo-txjcd.
*    gwa_saida-txz01  = gwa_ekpo-txz01.

    if  gwa_saida-erro_log is not initial.
      move icon_led_red to  gwa_saida-status.
    endif.

    append gwa_saida to git_saida.
    clear: gwa_saida.

  endloop.

* Montar Saldo Pedido Compra

  "TOTMOV
  sort:  git_ekbe     by ebeln ebelp.
  loop at git_ekbe into gwa_ekbe.

    gwa_ekbe_tot-ebeln = gwa_ekbe-ebeln.
    gwa_ekbe_tot-ebelp = gwa_ekbe-ebelp.
    if gwa_ekbe-shkzg = 'S'.
      gwa_ekbe_tot-menge = gwa_ekbe-menge.
    else.
      gwa_ekbe_tot-menge = gwa_ekbe-menge * -1.
    endif.
    collect gwa_ekbe_tot into git_ekbe_tot.
    clear gwa_ekbe_tot.
  endloop.

  "XTOTPED

  sort: git_ekpo     by ebeln ebelp,
        git_ekbe_tot by ebeln ebelp.

  loop at git_ekpo into gwa_ekpo.
    gwa_ekpo_tot-ebeln = gwa_ekpo-ebeln.
    gwa_ekpo_tot-ebelp = gwa_ekpo-ebelp.
    gwa_ekpo_tot-matnr = gwa_ekpo-matnr.
    gwa_ekpo_tot-txz01 = gwa_ekpo-txz01.
    gwa_ekpo_tot-mwskz = gwa_ekpo-mwskz.
    gwa_ekpo_tot-webre = gwa_ekpo-webre.
    gwa_ekpo_tot-meins = gwa_ekpo-meins.
    gwa_ekpo_tot-lgort = gwa_ekpo-lgort.
    gwa_ekpo_tot-menge = gwa_ekpo-menge.

    read table git_ekbe_tot into gwa_ekbe_tot with key
    ebeln =  gwa_ekpo_tot-ebeln
    ebelp =  gwa_ekpo_tot-ebelp.
    if sy-subrc = 0.
      gwa_ekpo_tot-menge = gwa_ekpo_tot-menge - gwa_ekbe_tot-menge.
    endif.

    append gwa_ekpo_tot to git_ekpo_tot.
    clear gwa_ekpo_tot.
  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form z_dados_bancarios
*&---------------------------------------------------------------------*
form z_dados_bancarios .
  data: wa_lfbk  type lfbk,
        lv_zfbdt type bseg-zfbdt,
        wa_t052  type t052.

  call function 'Z_RET_FORMA_PAGAMENTO'
    exporting
      p_bukrs            = gwa_ekko-bukrs
      p_lifnr            = gwa_ekko-lifnr
    importing
      p_banco_fornecedor = wa_lfbk
    exceptions
      nao_fornecedor     = 1
      fornecedor_conta   = 2
      fornecedor_banco   = 3
      faixa_valor        = 4
      banco_empresa      = 5
      others             = 6.

  call function 'FI_FIND_PAYMENT_CONDITIONS'
    exporting
      i_zterm            = gwa_ekko-zterm
      i_bldat            = gwa_saida-docdat
      i_budat            = gwa_saida-dt_movimento
    importing
      e_zfbdt            = lv_zfbdt
      e_t052             = wa_t052
    exceptions
      terms_incorrect    = 1
      terms_not_found    = 2
      no_date_entered    = 3
      no_day_limit_found = 4
      others             = 5.

  if wa_t052-ztag2 eq 0.
    lv_zfbdt = lv_zfbdt   + wa_t052-ztag1.
  endif.

  gwa_saida-dt_vencimento = lv_zfbdt.
  gwa_saida-bvtyp = wa_lfbk-bvtyp.

endform.

*&---------------------------------------------------------------------*
*&      Form  FM_MIRO_MIGO_BG
*&---------------------------------------------------------------------*
form fm_miro_migo_bg .

  data: lwa_doc_gerados type zmmt0132,
        doc_number_ref  type j_1bdocnum,
        lv_lucas        type c.

  data: lwa_selected_line like lvc_s_row,
        lf_row_index      type lvc_index.

  data: lva_erro_est_migo type c,
        lva_erro_est_miro type c.

  loop at git_saida into gwa_saida.

    if gwa_saida-status <> icon_complete.

      if gwa_saida-ebeln is not initial.

        perform z_check_lucas changing lv_lucas.

        gwa_zmmt0132-obj_key        = gwa_saida-obj_key.
        gwa_zmmt0132-po_number      = gwa_saida-ebeln.
        gwa_zmmt0132-lifnr          = gwa_saida-parid.
        gwa_zmmt0132-move_type      = cond #( when lv_lucas eq 'X' then '835'
                                              else '101' ).
        gwa_zmmt0132-nt_remessa     = gwa_saida-nfnum_s.
        gwa_zmmt0132-mm_mblnr       = gwa_saida-mm_mblnr.
        gwa_zmmt0132-mm_mjahr       = gwa_saida-mm_mjahr.
        gwa_zmmt0132-ft_belnr       = gwa_saida-ft_belnr.
        gwa_zmmt0132-ft_gjahr       = gwa_saida-ft_gjahr.
        gwa_zmmt0132-doc_date       = gwa_saida-docdat.
        gwa_zmmt0132-pstng_date     = gwa_saida-dt_movimento.
        gwa_zmmt0132-dt_vencimento  = gwa_saida-dt_vencimento.
        gwa_zmmt0132-po_item        = gwa_saida-ebelp.
        gwa_zmmt0132-entry_qnt      = gwa_saida-peso_fiscal.
        gwa_zmmt0132-meins          = gwa_saida-meins.
        gwa_zmmt0132-in_aviso_receb = 'N'.
        gwa_zmmt0132-peso_bruto     = gwa_saida-peso_fiscal.
        gwa_zmmt0132-comp_code      = s_bukrs-low.
        gwa_zmmt0132-tp_operacao    = '01'.
        gwa_zmmt0132-ref_doc_no     = gwa_saida-nfnum_s.
        gwa_zmmt0132-vr_bruto       = gwa_saida-netwr.
        gwa_zmmt0132-del_costs_taxc = gwa_saida-mwskz.
        gwa_zmmt0132-pmnt_block     = 'V'.
        gwa_zmmt0132-pmnttrms       = gwa_saida-zterm.
        "gwa_zmmt0132-scbank_ind     = 'BBRA'.
        gwa_zmmt0132-scbank_ind     = zcl_miro=>get_banco_forma_pagamento( i_bukrs = gwa_zmmt0132-comp_code i_forma_pagamento  = 'S' ).
        gwa_zmmt0132-plant          = s_branch-low.
        gwa_zmmt0132-j_1bnftype     = gwa_saida-j_1bnftype.
        gwa_zmmt0132-alloc_nmbr     = gwa_saida-ebeln.
        gwa_zmmt0132-gross_amount   = gwa_saida-netwr.
        gwa_zmmt0132-item_amount    = gwa_saida-netwr.
        gwa_zmmt0132-material       = gwa_saida-matnr.
        gwa_zmmt0132-nu_item        = gwa_saida-ebelp.
        gwa_zmmt0132-taxjurcode     = gwa_saida-txjcd.
        gwa_zmmt0132-zdt_atlz       = sy-datum.
        gwa_zmmt0132-zhr_atlz       = sy-uzeit.
        gwa_zmmt0132-zrg_atlz       = 0.
        gwa_zmmt0132-pymt_meth      = 'E'.
        gwa_zmmt0132-bus_area       = s_branch-low.
        gwa_zmmt0132-quantity       = gwa_saida-peso_fiscal.
        gwa_zmmt0132-batch          = gwa_saida-charg.
        gwa_zmmt0132-lgort          = gwa_saida-lgort.
*** 10.06.2022 - BUG - 81027 - Inicio
        "gwa_zmmt0132-vr_impostos    = gwa_saida-icms_base.

        gwa_zmmt0132-vr_impostos    = gwa_saida-icms_valor + gwa_saida-ipi_valor + gwa_saida-pis_valor + gwa_saida-cof_valor.

*** 10.06.2022 - BUG - 81027 - Fim
        gwa_zmmt0132-amount_lc      = gwa_saida-amount_lc.
        gwa_zmmt0132-exch_rate_v    = gwa_saida-exch_rate_v.
        gwa_zmmt0132-bvtyp          = gwa_saida-bvtyp.
        gwa_zmmt0132-nfnum          = gwa_saida-nfnum.
        gwa_zmmt0132-series         = gwa_saida-series.
        gwa_zmmt0132-webre          = gwa_saida-webre.
        gwa_zmmt0132-chave_nfe      = gwa_saida-chave_nfe.

        data(ck_cotinue) = abap_false.

        perform verifica_miro using gwa_zmmt0132 gwa_zmmt0132-obj_key changing ck_cotinue gva_return .
        if ck_cotinue eq abap_true.
          perform z_prepara_mensagem2 using gwa_zmmt0132-obj_key 'E' gva_return gwa_zmmt0132-obj_key space '11'.
          continue.
        endif.

        if gwa_zmmt0132-mm_mblnr is initial.
          perform z_bapi_migo using gwa_zmmt0132 lwa_doc_gerados lv_lucas.
          if gva_erro is initial.
            if lv_lucas ne abap_true.
              perform z_bapi_miro using gwa_zmmt0132 lwa_doc_gerados doc_number_ref.
            endif.
          endif.
        endif.

        if gva_erro is not initial .

          move-corresponding gwa_zmmt0132 to lwa_doc_gerados.

          perform z_estorna_miro using lwa_doc_gerados
                          changing lva_erro_est_miro.
          if lva_erro_est_miro is initial.
            perform z_estorna_migo using lwa_doc_gerados
                             changing lva_erro_est_migo.
          endif.
        endif.

        clear: gwa_zmmt0132.
      else.
        message  'Pedido de Compra não informado' type 'S' display like 'E'.
        exit.
      endif.
    else.
      message  'MIRO/MIGO já gerados nessa seleção' type 'S' display like 'E'.
      exit.
    endif.
  endloop.
endform.
