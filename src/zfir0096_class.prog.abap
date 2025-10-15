data: container_main type ref to cl_gui_custom_container,
      painel_control type ref to cl_gui_splitter_container,
      painel1        type ref to cl_gui_container,
      painel2        type ref to cl_gui_container.
class lcl_report definition deferred.
data: lo_report type ref to lcl_report.

class lcl_listener definition.
  public section.
    interfaces :
      if_salv_gui_om_edit_strct_lstr.
endclass.

class lcl_listener implementation.
  method if_salv_gui_om_edit_strct_lstr~on_check_changed_data.
    o_ui_data_modify->get_ui_changes( importing t_modified_cells = data(lt_modified) ).
  endmethod.
endclass.


class lcl_report definition.
  public section .
    methods:
      get_data,
      make_email,
      send_email importing ls_mail type ty_email,
      generate_output1,
      generate_output2,
      set_handler1,
      set_refresh1,
      set_columns_build1,
      set_columns_build2,
      set_pf_status1,
      set_layout1,
      set_edit_alv1,
      on_toolbar1
        for event toolbar of cl_gui_alv_grid
        importing
          e_object
          e_interactive
          sender,
      on_user_command1 for event added_function of cl_salv_events importing e_salv_function sender,
      on_link_click1 for event link_click of cl_salv_events_table importing row column,
      on_change_data1 for event data_changed of cl_gui_alv_grid importing e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

endclass.
class lcl_report implementation.

  method send_email.

    clear: wa_mail.

    wa_mail = ls_mail.

    data lv_lines    like sy-tabix.
    data wa_doc_data type sodocchgi1.
    data it_receivers       type table of somlreci1.
    data it_contents_txt    type table of solisti1.
    data it_packing_list    type table of sopcklsti1.
    data wa_contents_txt     type solisti1.
    data wa_packing_list     type sopcklsti1.
    data it_contents_bin    type table of solisti1 initial size 0.
    data enviado_sucesso    type c length 1.
    data it_header          type table of solisti1.
    data it_contents_hex    type solisti1.

**********************************************************************DESTINATARIOS
    free: it_receivers[].
    clear: it_receivers.
    split wa_mail-destinatarios at ';' into table data(it_destinatarios).

    loop at it_destinatarios assigning field-symbol(<_destinatarios>).
      append value #( receiver =   <_destinatarios>   rec_type =   'U'  rec_date =   sy-datum ) to it_receivers.
    endloop.
**********************************************************************HTML_BODY
    free:it_contents_txt[],it_packing_list[].
    clear: WA_contents_txt,it_packing_list,lv_lines.
    split wa_mail-html at '<!---->' into table it_contents_txt[].
    lv_lines = lines( it_contents_txt[] ).
    wa_packing_list-transf_bin = space.
    wa_packing_list-head_start = 1.
    wa_packing_list-head_num   = 1.
    wa_packing_list-body_start = 1.
    wa_packing_list-body_num   = lv_lines.
    wa_packing_list-doc_type   = 'HTM'.
    append wa_packing_list to it_packing_list[].

**********************************************************************PDF
    wa_doc_data-obj_descr = |ADIANTAMENTOS VENCIDOS – FORNECEDORES DIVERSOS - { wa_mail-gsber }|.
    "wa_doc_data-doc_size  = _size_anexo + _size_body.
    wa_doc_data-obj_langu  = sy-langu.
    "CLEAR: wa_anexo.
    clear:wa_packing_list.
**********************************************************************
    try.

        call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
          exporting
            document_data              = wa_doc_data
            put_in_outbox              = 'X'
            commit_work                = 'X'
          tables
            packing_list               = it_packing_list
            contents_txt               = it_contents_txt
            contents_bin               = it_contents_bin
            receivers                  = it_receivers
          exceptions
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            others                     = 8.

        if sy-subrc <> 0.
          clear: enviado_sucesso.
        else.
          enviado_sucesso = abap_true.
        endif.
      catch cx_root.
    endtry.
    free: it_packing_list,
it_header,
it_contents_bin,
it_contents_txt,
it_contents_hex,
it_receivers.

  endmethod.

  method make_email.
    data: rg_werks type range of gsber,
          rg_bukrs type range of bukrs.

    free: rg_werks, rg_bukrs.

    if it_saida is not initial.

      move-corresponding it_saida to it_mail.

      sort it_mail by bukrs ascending gsber ascending.

      delete adjacent duplicates from it_mail.

      perform gera_table_header.

      data: lt_html type standard table of string initial size 0.

      select *
      from zmail
      for all entries in @it_mail
      where bukrs = @it_mail-bukrs
      and werks = @it_mail-gsber
      and tcode = 'ZFI0173'
      into table @data(it_zmail).
      if sy-subrc eq 0.
        loop at it_zmail assigning field-symbol(<ws_email>).
          append value #( sign = 'I' option = 'EQ' low = <ws_email>-werks ) to rg_werks.
          append value #( sign = 'I' option = 'EQ' low = <ws_email>-bukrs ) to rg_bukrs.
        endloop.
      endif.

      delete it_mail where bukrs not in rg_bukrs and gsber not in rg_werks.

      loop at it_mail assigning field-symbol(<fs_mail>).

        read table it_saida into wa_saida with key bukrs = <fs_mail>-bukrs gsber = <fs_mail>-gsber bschl = 29.

        if sy-subrc = 0.

          perform gera_table_dados using wa_saida-bukrs wa_saida-gsber wa_saida-bschl.

        endif.

        read table it_saida into wa_saida with key bukrs = <fs_mail>-bukrs gsber = <fs_mail>-gsber bschl = 39.

        if sy-subrc = 0.

          perform gera_table_dados using wa_saida-bukrs wa_saida-gsber wa_saida-bschl.

        endif.

        if ls_table_conteudo29 is not initial or ls_table_conteudo39 is not initial.

          <fs_mail>-email = 'E-mail'.


          data(titulo) = |ADIANTAMENTOS VENCIDOS – FORNECEDORES DIVERSOS - { wa_saida-gsber }|.
          data(msg) = |Filial, segue abaixo a relação de lançamentos em aberto na conta do fornecedor, por favor, encaminhe um retorno para a equipe cscfinanceiro.fornecedor@amaggi.com.br informando sobre as devidas tratativas de baixa.|.

          data(estilo) ='<STYLE>TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; } TH, TD { PADDING: 5PX; } TH { TEXT-ALIGN: LEFT; } </STYLE>'.
          "TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; } TABLE#T02 TH {BACKGROUND-COLOR:#218bdb; COLOR: WHITE; }
          "'<style>table, th, td { border:1px solid black; border-collapse: collapse; }</style>'.

          data(abre) = |<!DOCTYPE html><!---->{ estilo }<!----><body><!----><h2>{ titulo }</h2><!---->|.
          "<html lang="pt-BR">
          "<head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Envio de Email</title></head>

          append abre to lt_html.
          clear:abre.

          data(msg_CORPO) = |<!----><br>{ msg }<br><br><br><!---->|.
          append msg_corpo to lt_html.
          clear:msg_corpo.
          "aqui colocar menssagem!

          if ls_table_conteudo29 is not initial.
            append ls_table_header to lt_html.
            append ls_table_conteudo29 to lt_html.
            clear: ls_table_conteudo29.
          endif.

          if ls_table_conteudo39 is not initial.

            data(espaco) = |<br><!----><br><!---->|.
            append espaco to lt_html.
            clear:espaco.


            append ls_table_header to lt_html.
            append ls_table_conteudo39 to lt_html.
            clear: ls_table_conteudo39.
          endif.

          data(fecha) = |</body><!----></html><!---->|.
          append fecha to lt_html.
          clear:fecha.

          concatenate lines of lt_html into <fs_mail>-html.

          free: lt_html.

        endif.

        read table it_zmail into data(w_zmail) with key bukrs = <fs_mail>-bukrs werks = <fs_mail>-gsber.
        if sy-subrc = 0.
          loop at it_zmail into data(wa_zmail) where bukrs = w_zmail-bukrs and werks = w_zmail-werks.
            data: it_destinatarios type standard table of string initial size 0.
            append wa_zmail-email to it_destinatarios.
            clear:wa_zmail.
          endloop.
          if sy-subrc = 0.
            concatenate lines of it_destinatarios into <fs_mail>-destinatarios separated by ';'.
            <fs_mail>-lista = '@B_DISP@'.
*** BUG #182525 - MMSILVA - 18.06.2025 - Ini ***
            free: it_destinatarios.
*** BUG #182525 - MMSILVA - 18.06.2025 - Fim ***
          endif.
        endif.

      endloop.

    endif.

  endmethod.

  method set_refresh1.
    lo_report->get_data( ).
    o_alv->refresh( ).
  endmethod.

  method on_change_data1.

    data: it_changed type standard table of zfise46 initial size 0.
    "CLEAR: it_changed,wa_saida.

    data(inserted_tab) = er_data_changed->mt_inserted_rows.
    data(deleted_tab)  = er_data_changed->mt_deleted_rows.

    field-symbols: <itab>        type any table,
                   <struct>      type any,
                   <it_mod_rows> type any table,
                   <wa_mod_rows> type any.


    data: lt_good_cells type lvc_t_modi,
          ls_good_cell  type lvc_s_modi.


    assign er_data_changed->mp_mod_rows->* to <it_mod_rows>.
    move-corresponding <it_mod_rows> to it_changed.

    lt_good_cells = er_data_changed->mt_good_cells.

  endmethod.

  method set_columns_build1 .
*...Get all the Columns
    data: lo_cols        type ref to cl_salv_columns,
          lo_cols_ref    type        salv_t_column_ref,
          lo_cols_list   type ref to cl_salv_column_list,
          lo_col_list    like line of lo_cols_ref,
          lo_column      type ref to cl_salv_column,
          ls_ddic_f4_ref type salv_s_ddic_reference.

    lo_cols = o_alv->get_columns( ).
    "lo_cols->set_optimize( abap_false ).

    lo_cols_ref    = lo_cols->get( ).


    try.

        loop at lo_cols_ref into lo_col_list.
          lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
          clear: ls_ddic_f4_ref.
          case lo_col_list-columnname.

            when 'BUKRS' .
              lo_cols_list->set_short_text( 'Empresa' ).
              lo_cols_list->set_medium_text( 'Empresa' ).
              lo_cols_list->set_long_text( 'Empresa' ).

*            when 'GKONT' .
*              lo_cols_list->set_short_text( 'Conta' ).
*              lo_cols_list->set_medium_text( 'Conta' ).
*              lo_cols_list->set_long_text( 'Conta' ).

            when 'LIFNR' .
              lo_cols_list->set_short_text( 'Cd Fornec.' ).
              lo_cols_list->set_medium_text( 'Cd Fornecedor' ).
              lo_cols_list->set_long_text( 'cd Fornecedor' ).

            when 'NAME1' .
              lo_cols_list->set_short_text( 'Nm Fornec.' ).
              lo_cols_list->set_medium_text( 'Nm Fornecedor' ).
              lo_cols_list->set_long_text( 'Nm Fornecedor' ).

            when 'EBELN' .
              lo_cols_list->set_short_text( 'Doc.compra' ).
              lo_cols_list->set_medium_text( 'Doc. compra' ).
              lo_cols_list->set_long_text( 'Doc. compra' ).

            when 'GSBER' .
              lo_cols_list->set_short_text( 'Divisão' ).
              lo_cols_list->set_medium_text( 'Divisão' ).
              lo_cols_list->set_long_text( 'Divisão' ).

            when 'BELNR' .
              lo_cols_list->set_short_text( 'Nº doc.' ).
              lo_cols_list->set_medium_text( 'Nº doc.' ).
              lo_cols_list->set_long_text( 'Nº doc.' ).

            when 'BUDAT' .
              lo_cols_list->set_short_text( 'Data Lçto' ).
              lo_cols_list->set_medium_text( 'Data Lçto' ).
              lo_cols_list->set_long_text( 'Data Lçto' ).

            when 'BLDAT' .
              lo_cols_list->set_short_text( 'VctoLíqu.' ).
              lo_cols_list->set_medium_text( 'Vcto Líquido' ).
              lo_cols_list->set_long_text( 'Vcto Líquido' ).

            when 'WAERS' .
              lo_cols_list->set_short_text( 'Moeda' ).
              lo_cols_list->set_medium_text( 'Moeda' ).
              lo_cols_list->set_long_text( 'Moeda' ).

            when 'DMBTR' .
              lo_cols_list->set_short_text( 'Mont.MI' ).
              lo_cols_list->set_medium_text( 'Montante MI' ).
              lo_cols_list->set_long_text( 'Montante MI' ).

            when 'DMBE2' .
              lo_cols_list->set_short_text( 'Mont.MI2' ).
              lo_cols_list->set_medium_text( 'Montante MI2' ).
              lo_cols_list->set_long_text( 'Montante MI2' ).

            when 'BLART' .
              lo_cols_list->set_short_text( 'Tipo DOC' ).
              lo_cols_list->set_medium_text( 'Tipo DOC' ).
              lo_cols_list->set_long_text( 'Tipo DOC' ).

            when 'BSCHL' .
              lo_cols_list->set_short_text( 'CL' ).
              lo_cols_list->set_medium_text( 'CL' ).
              lo_cols_list->set_long_text( 'CL' ).

            when 'ZLSPR' .
              lo_cols_list->set_short_text( 'Bloq.pgto.' ).
              lo_cols_list->set_medium_text( 'Bloqueio pgto.' ).
              lo_cols_list->set_long_text( 'Bloqueio pgto.' ).

            when 'SGTXT' .
              lo_cols_list->set_short_text( 'Texto' ).
              lo_cols_list->set_medium_text( 'Texto' ).
              lo_cols_list->set_long_text( 'Texto' ).

            when 'HKONT' .
              lo_cols_list->set_short_text( 'Razão' ).
              lo_cols_list->set_medium_text( 'Razão' ).
              lo_cols_list->set_long_text( 'Razão' ).

            when others.
              lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
          endcase.
        endloop.
      catch cx_salv_not_found.

    endtry.

    lo_cols->set_column_position( columnname = 'BUKRS' position = 01 ).
    lo_cols->set_column_position( columnname = 'HKONT' position = 02 ).
    lo_cols->set_column_position( columnname = 'LIFNR' position = 03 ).
    lo_cols->set_column_position( columnname = 'NAME1' position = 04 ).
    lo_cols->set_column_position( columnname = 'EBELN' position = 05 ).
    lo_cols->set_column_position( columnname = 'GSBER' position = 06 ).
    lo_cols->set_column_position( columnname = 'BELNR' position = 07 ).
    lo_cols->set_column_position( columnname = 'BSCHL' position = 08 ).
    lo_cols->set_column_position( columnname = 'BUDAT' position = 09 ).
    lo_cols->set_column_position( columnname = 'BLDAT' position = 10 ).
    lo_cols->set_column_position( columnname = 'WAERS' position = 10 ).
    lo_cols->set_column_position( columnname = 'DMBTR' position = 12 ).
    lo_cols->set_column_position( columnname = 'DMBE2' position = 13 ).
    lo_cols->set_column_position( columnname = 'BLART' position = 14 ).
    lo_cols->set_column_position( columnname = 'ZLSPR' position = 15 ).
    lo_cols->set_column_position( columnname = 'SGTXT' position = 16 ).
    lo_cols->set_column_position( columnname = 'GKONT' position = 17 ).



  endmethod.


  method set_columns_build2 .
*...Get all the Columns
    data: lo_cols        type ref to cl_salv_columns,
          lo_cols_ref    type        salv_t_column_ref,
          lo_cols_list   type ref to cl_salv_column_list,
          lo_col_list    like line of lo_cols_ref,
          lo_column      type ref to cl_salv_column,
          ls_ddic_f4_ref type salv_s_ddic_reference.

    lo_cols = o_alv->get_columns( ).
    "lo_cols->set_optimize( abap_false ).

    lo_cols_ref    = lo_cols->get( ).


    try.

        loop at lo_cols_ref into lo_col_list.
          lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
          clear: ls_ddic_f4_ref.
          case lo_col_list-columnname.

            when 'BUKRS' .
              lo_cols_list->set_short_text( 'Empresa' ).
              lo_cols_list->set_medium_text( 'Empresa' ).
              lo_cols_list->set_long_text( 'Empresa' ).

            when 'GSBER' .
              lo_cols_list->set_short_text( 'Divisão' ).
              lo_cols_list->set_medium_text( 'Divisão' ).
              lo_cols_list->set_long_text( 'Divisão' ).

            when 'EMAIL' .
              lo_cols_list->set_short_text( 'E-mail' ).
              lo_cols_list->set_medium_text( 'E-mail' ).
              lo_cols_list->set_long_text( 'E-mail' ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).

            when 'LISTA' .
              lo_cols_list->set_short_text( 'Lista' ).
              lo_cols_list->set_medium_text( 'Lista' ).
              lo_cols_list->set_long_text( 'Lista' ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
              "lo_cols_list->set_tooltip( value = |{ icon_display }| ).
              lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_cols_list->set_output_length( '10' ).


*                WHEN 'GKONT' .
*                  lo_cols_list->set_short_text( 'Conta' ).
*                  lo_cols_list->set_medium_text( 'Conta'  ).
*                  lo_cols_list->set_long_text( 'Conta'  ).
*
*                WHEN 'LIFNR' .
*                  lo_cols_list->set_short_text( 'Cd Fornec.' ).
*                  lo_cols_list->set_medium_text( 'Cd Fornecedor'  ).
*                  lo_cols_list->set_long_text( 'cd Fornecedor'  ).
*
*                WHEN 'NAME1' .
*                  lo_cols_list->set_short_text( 'Nm Fornec.' ).
*                  lo_cols_list->set_medium_text( 'Nm Fornecedor'  ).
*                  lo_cols_list->set_long_text( 'Nm Fornecedor'  ).
*
*                WHEN 'EBELN' .
*                  lo_cols_list->set_short_text( 'Doc.compra' ).
*                  lo_cols_list->set_medium_text( 'Doc. compra'  ).
*                  lo_cols_list->set_long_text( 'Doc. compra'  ).
*
*                WHEN 'BELNR' .
*                  lo_cols_list->set_short_text( 'Nº doc.' ).
*                  lo_cols_list->set_medium_text( 'Nº doc.' ).
*                  lo_cols_list->set_long_text( 'Nº doc.'  ).
*
*                WHEN 'BUDAT' .
*                  lo_cols_list->set_short_text( 'Data Lçto' ).
*                  lo_cols_list->set_medium_text( 'Data Lçto' ).
*                  lo_cols_list->set_long_text( 'Data Lçto'  ).
*
*                WHEN 'BLDAT' .
*                  lo_cols_list->set_short_text( 'VctoLíqu.' ).
*                  lo_cols_list->set_medium_text( 'Vcto Líquido' ).
*                  lo_cols_list->set_long_text( 'Vcto Líquido'  ).
*
*                WHEN 'RFCCUR' .
*                  lo_cols_list->set_short_text( 'Moeda' ).
*                  lo_cols_list->set_medium_text( 'Moeda' ).
*                  lo_cols_list->set_long_text( 'Moeda'  ).
*
*                WHEN 'DMBTR' .
*                  lo_cols_list->set_short_text( 'Mont.MI' ).
*                  lo_cols_list->set_medium_text( 'Montante MI' ).
*                  lo_cols_list->set_long_text( 'Montante MI'  ).
*
*                WHEN 'DMBE2' .
*                  lo_cols_list->set_short_text( 'Mont.MI2' ).
*                  lo_cols_list->set_medium_text( 'Montante MI2' ).
*                  lo_cols_list->set_long_text( 'Montante MI2'  ).
*
*                WHEN 'BLART' .
*                  lo_cols_list->set_short_text( 'Tipo DOC' ).
*                  lo_cols_list->set_medium_text( 'Tipo DOC' ).
*                  lo_cols_list->set_long_text( 'Tipo DOC'  ).
*
*                WHEN 'BSCHL' .
*                  lo_cols_list->set_short_text( 'CL' ).
*                  lo_cols_list->set_medium_text( 'CL' ).
*                  lo_cols_list->set_long_text( 'CL'  ).
*
*                WHEN 'ZLSPR' .
*                  lo_cols_list->set_short_text( 'Bloq.pgto.' ).
*                  lo_cols_list->set_medium_text( 'Bloqueio pgto.' ).
*                  lo_cols_list->set_long_text( 'Bloqueio pgto.'  ).
*
*                WHEN 'SGTXT' .
*                  lo_cols_list->set_short_text( 'Texto' ).
*                  lo_cols_list->set_medium_text( 'Texto' ).
*                  lo_cols_list->set_long_text( 'Texto'  ).
*
*                WHEN 'HKONT' .
*                  lo_cols_list->set_short_text( 'Razão' ).
*                  lo_cols_list->set_medium_text( 'Razão' ).
*                  lo_cols_list->set_long_text( 'Razão'  ).

            when others.
              lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
          endcase.
        endloop.
      catch cx_salv_not_found.

    endtry.

    lo_cols->set_column_position( columnname = 'BUKRS' position = 01 ).
    lo_cols->set_column_position( columnname = 'GSBER' position = 02 ).
    lo_cols->set_column_position( columnname = 'EMAIL' position = 03 ).
*        lo_cols->set_column_position( columnname = 'LIFNR'             position  = 03   ).
*        lo_cols->set_column_position( columnname = 'NAME1'             position  = 04   ).
*        lo_cols->set_column_position( columnname = 'BSCHL'             position  = 05   ).
*        lo_cols->set_column_position( columnname = 'HKONT'             position  = 06   ).
*        lo_cols->set_column_position( columnname = 'GKONT'             position  = 07   ).
*        lo_cols->set_column_position( columnname = 'EBELN'             position  = 08   ).
*        lo_cols->set_column_position( columnname = 'BELNR'             position  = 09   ).
*        lo_cols->set_column_position( columnname = 'BUDAT'             position  = 10   ).
*        lo_cols->set_column_position( columnname = 'BLDAT'             position  = 11   ).
*        lo_cols->set_column_position( columnname = 'RFCCUR'            position  = 12   ).
*        lo_cols->set_column_position( columnname = 'DMBTR'             position  = 13   ).
*        lo_cols->set_column_position( columnname = 'DMBE2'             position  = 14   ).
*        lo_cols->set_column_position( columnname = 'BLART'             position  = 15   ).
*        lo_cols->set_column_position( columnname = 'ZLSPR'             position  = 16   ).
*        lo_cols->set_column_position( columnname = 'SGTXT'             position  = 17   ).


  endmethod.

  method set_edit_alv1.
    data:ls_api  type ref to if_salv_gui_om_extend_grid_api,
         ls_edit type ref to if_salv_gui_om_edit_restricted.

    data: lv_ref_table  type ref to cl_abap_tabledescr,
          lv_ref_struct type ref to cl_abap_structdescr.

    ls_api = o_alv->extended_grid_api( ).
    ls_edit = ls_api->editable_restricted( ).
    lv_ref_table  ?= cl_abap_tabledescr=>describe_by_data( it_saida ).
    lv_ref_struct ?= lv_ref_table->get_table_line_type( ).
    data(lt_details)   = lv_ref_struct->components.

    loop at lt_details assigning field-symbol(<_details>).
      ls_edit->set_attributes_for_columnname(
        exporting
          columnname              = <_details>-name
          all_cells_input_enabled = abap_true
      ).

    endloop.

    data(mo_listener) = new lcl_listener( ).
    ls_edit->set_listener( mo_listener ).
    ls_edit->validate_changed_data(
).

    o_alv->refresh( ).
  endmethod.

  method get_data.

    free: it_saida.

    select
a~Bukrs,
a~gkont,
a~lifnr,
b~name1,
a~ebeln,
a~gsber,
a~belnr,
a~budat,
a~bldat,
a~waers,
a~dmbtr,
a~dmbe2,
a~blart,
a~bschl,
a~zlspr,
a~sgtxt,
a~hkont
from bsik as a
      inner join lfa1 as b on a~lifnr = b~lifnr
where 1 = 1
      and a~bukrs in @p_bukrs
      and a~gsber in @p_gsber
      and a~lifnr in @p_lifnr
      and a~blart in @p_blart
      and a~bschl in @p_bschl
      and a~hkont in @p_hkont
      and a~umskz not in ( @space,'F' )
      into   table @data(it_dados).

    if it_dados is not initial.
      move-corresponding it_dados to it_saida.

      sort it_saida by bukrs ascending gsber ascending lifnr ascending bschl ascending.

    else.
      message 'Não foram encontrados dados para esta seleção!' type 'I' display like 'I'.
      exit.
    endif.

  endmethod.


  method set_pf_status1.

    data: lo_functions type ref to cl_salv_functions_list.
    lo_functions = o_alv->get_functions( ).
    lo_functions->set_all( abap_true ).
    lo_functions->set_default( abap_true ).

    if r2 = 'X'.
      try.
          lo_functions->add_function( name     = 'ENVIAR'
                                      icon     = '@ENVCLO@'
                                      text     = 'Enviar'
                                      tooltip  = 'Enviar'
                                      position = if_salv_c_function_position=>right_of_salv_functions ).


        catch cx_root.

      endtry.
    endif.


  endmethod.

  method on_user_command1.

    data: lo_selections type ref to cl_salv_selections.
    data lt_rows type salv_t_row.
    data lt_columns type salv_t_column.
    data lt_cells type salv_t_cell.
    data qtd_rows type int4.

    free: lt_rows.
    clear: qtd_rows.

    lo_selections = o_alv->get_selections( ).
    lt_rows = lo_selections->get_selected_rows( ).
    qtd_rows = lines( lt_rows ).


    case e_salv_function.

      when 'ENVIAR'.
        if qtd_rows > 0 and r2 = 'X'.
          if it_mail is not initial and r2 = 'X'.

            loop at it_mail assigning field-symbol(<_send>).

              lo_report->send_email( ls_mail = <_send> ).
            endloop.

          else.
          endif.
        else.
          message 'Selecione ao menos uma linha!' type 'I' display like 'I'.
          exit.
        endif.
      when 'DELETE_ROW'.

        if qtd_rows > 0.
          loop at lt_rows assigning field-symbol(<_index>).
            read table it_saida assigning field-symbol(<_del>) index <_index>.
            "DELETE zmmt0185 FROM <_del>.
          endloop.

        else.
          message 'Selecione ao menos uma linha!' type 'I' display like 'I'.
          exit.
        endif.

        call method set_refresh1.

      when 'REFRESH_ROW'.
        call method set_refresh1.
      when others.
    endcase.

  endmethod.

  method on_toolbar1.

    data : mt_toolbar type stb_button.

    clear mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    append mt_toolbar to e_object->mt_toolbar.

    loop at e_object->mt_toolbar assigning field-symbol(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA
      if  <fs_tollbar>-function eq '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      elseif <fs_tollbar>-function eq '&LOCAL&CREATE_ROW'.
        "<fs_tollbar>-butn_type = '3'.
      elseif <fs_tollbar>-function eq '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      endif.
      if <fs_tollbar>-function eq '&REFRESH'.
        <fs_tollbar>-function = 'REFRESH_ROW'.
      elseif <fs_tollbar>-function eq '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-function = 'DELETE_ROW'.
      endif.
    endloop.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

  endmethod.

  method generate_output1.

    container_main = new cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      "lifetime       =  cl_gui_container=>lifetime_dynpro
      container_name = 'CONTAINER'
    ).

*** Cria Splitter Container
*  CREATE OBJECT painel_control
*    EXPORTING
*      parent  = container_main
*      rows    = 1
*      columns = 1
*      align   = 70.
**
*** Exibe Painel 1
*  CALL METHOD painel_control->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = painel1.

    data: lx_msg type ref to cx_salv_msg.


    try.
        cl_salv_table=>factory(
          exporting
            r_container    = container_main
            container_name = 'CONTAINER'
          importing
            r_salv_table   = o_alv
          changing
            t_table        = it_saida ).
      catch cx_salv_msg into lx_msg.
    endtry.

    call method set_pf_status1.

    call method set_layout1.

    call method set_HANDLER1.

    call method set_columns_build1.

*        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
*        DATA l_title              TYPE lvc_title.
*        l_title = |Nome Relatório|.
*        lr_display_settings = o_alv->get_display_settings( ).
*        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*        lr_display_settings->set_list_header( l_title ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).

    data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
    lr_selections = o_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


*        DATA: lo_cols_sorts TYPE REF TO cl_salv_sorts.
*
*        lo_cols_sorts = o_alv->get_sorts( ).
*        lo_cols_sorts->add_sort( columnname = 'BUKRS' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'GSBER' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'LIFNR' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'BSCHL' subtotal = abap_true ).

    o_alv->display( ).

*    CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      CHANGING
*        co_alv = o_alv.


  endmethod.


  method generate_output2.

    container_main = new cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      "lifetime       =  cl_gui_container=>lifetime_dynpro
      container_name = 'CONTAINER'
    ).

*** Cria Splitter Container
*  CREATE OBJECT painel_control
*    EXPORTING
*      parent  = container_main
*      rows    = 1
*      columns = 1
*      align   = 70.
**
*** Exibe Painel 1
*  CALL METHOD painel_control->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = painel1.

    data: lx_msg type ref to cx_salv_msg.


    try.
        cl_salv_table=>factory(
          exporting
            r_container    = container_main
            container_name = 'CONTAINER'
          importing
            r_salv_table   = o_alv
          changing
            t_table        = it_mail ).
      catch cx_salv_msg into lx_msg.
    endtry.

    call method set_pf_status1.

    call method set_layout1.

    call method set_HANDLER1.

    call method set_columns_build2.

*        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
*        DATA l_title              TYPE lvc_title.
*        l_title = |Nome Relatório|.
*        lr_display_settings = o_alv->get_display_settings( ).
*        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*        lr_display_settings->set_list_header( l_title ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).


    data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
    lr_selections = o_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


    data: lo_cols_sorts type ref to cl_salv_sorts.

*        lo_cols_sorts = o_alv->get_sorts( ).
*        lo_cols_sorts->add_sort( columnname = 'BUKRS' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'GSBER' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'LIFNR' subtotal = abap_true ).
*        lo_cols_sorts->add_sort( columnname = 'BSCHL' subtotal = abap_true ).

    o_alv->display( ).

*    CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      CHANGING
*        co_alv = o_alv.


  endmethod.


  method set_HANDLER1.
*
*...HotSpot
    data: lo_cols_tab type ref to cl_salv_columns_table,
          lo_col_tab  type ref to cl_salv_column_table,
          lo_events   type ref to cl_salv_events_table.

    lo_cols_tab = o_alv->get_columns( ).
    lo_events = o_alv->get_event( ).

*   event handler
    set handler lo_report->on_link_click1 for lo_events.
    set handler lo_report->on_user_command1 for lo_events.
    set handler lo_report->on_toolbar1 for all instances activation 'X'.
    set handler lo_report->on_change_data1 for all instances activation 'X'.
    "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
  endmethod.

  method on_link_click1.

    if column = 'EMAIL' and r2 = 'X'.
      read table it_mail into wa_mail index row.

      if 1 = 1.
        cl_abap_browser=>show_html( html_string = wa_mail-html title = 'E-mail' ).
      else.
        cl_demo_output=>write_text( wa_mail-html ).
        cl_demo_output=>display( ).
      endif.

    elseif column = 'LISTA' and r2 = 'X'.

      types: begin of ty_DEST,
               email type adr6-smtp_addr,
             end of ty_DEST.

      data: lt_destinatarios  type table of ty_DEST initial size 0.
      free: lt_destinatarios.

      read table it_mail into wa_mail index row.
      split wa_mail-destinatarios at ';' into table lt_destinatarios[].

      sort lt_destinatarios[].

      delete adjacent duplicates from lt_destinatarios[].

      cl_demo_output=>display( lt_destinatarios[] ).

    endif.

  endmethod.


  method set_layout1.
*
    data: lo_layout  type ref to cl_salv_layout,
          lf_variant type slis_vari,
          ls_key     type salv_s_layout_key.
*   get layout object
    lo_layout = o_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
    lf_variant = 'DEFAULT'.
    lo_layout->set_initial_layout( lf_variant ).
  endmethod.

endclass.
