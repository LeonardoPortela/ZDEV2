*&---------------------------------------------------------------------*
*& Report ZPMR0088
*&---------------------------------------------------------------------*
report zpmr0088.


*&---------------------------------------------------------------------*
*& TABELAS SELECT-OPTIONS
*&---------------------------------------------------------------------*
tables: aufk, afih, eqkt.


*&---------------------------------------------------------------------*
*&  TABELAS INTERNAS E WORKAREAS
*&---------------------------------------------------------------------*
data: it_aufk type table of aufk,
      it_eqkt type table of eqkt,
      it_afih type table of afih,
      wa_aufk type aufk.


*&---------------------------------------------------------------------*
*&  SAIDA
*&---------------------------------------------------------------------*
types: begin of ty_saida,
         aufnr         type aufk-aufnr,
         ktext         type aufk-ktext,
         werks         type aufk-werks,
         bukrs         type aufk-bukrs,
         auart         type aufk-auart,
         erdat         type aufk-erdat,
         pdat2         type aufk-pdat2,
         equnr         type afih-equnr,
         eqktx         type eqkt-eqktx,
         status(10)    type c,
         total_dias    type i,
         line_color(4) type c,
         color_cell    type lvc_t_scol,
       end of ty_saida.

data: it_saida type table of ty_saida,
      wa_saida type ty_saida.


*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

data: dg_splitter_1        type ref to cl_gui_splitter_container,
      g_grid               type ref to cl_gui_alv_grid,
      g_custom_container   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      container_1          type ref to cl_gui_container,
      cl_container_95      type ref to cl_gui_docking_container,
      obj_dyndoc_id        type ref to cl_dd_document,
      tl_function          type ui_functions,
      wl_function          type ui_func,
*
      t_fieldcat           type lvc_t_fcat,
      w_fieldcat           type lvc_s_fcat,
      t_colorcell          type table of lvc_s_scol,
      w_colorcell          type lvc_s_scol,
      t_exctab             type slis_t_extab,
      w_exctab             type slis_extab,
      w_layout             type lvc_s_layo,
      w_stable             type lvc_s_stbl,
      t_style              type lvc_t_styl,
      w_style              type lvc_s_styl,
      t_rows               type lvc_t_row,
      w_rows               type lvc_s_row,
      ok_code              type sy-ucomm,
      t_cor                type table of lvc_s_scol,
      it_color             type table of lvc_s_scol,
*
      zcl_util             type ref to zcl_util.

data: variante         like disvariant.
data: gs_variant_c type disvariant.

data: it_return type table of ddshretval,
      it_T028G  type table of t028g.


*&---------------------------------------------------------------------*
*&  CLASSES
*&---------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.


    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.

    class-methods:
      on_hotspot_click for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id.

*    CLASS-METHODS:
*      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
*        IMPORTING e_object.

*    CLASS-METHODS:
*      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
*        IMPORTING e_ucomm.

endclass.                    "LCL_EVENT_HANDLER DEFINITION



class lcl_event_handler implementation.
  method on_double_click.

  endmethod.

  method on_hotspot_click.
    data : l_columnid type lvc_s_col.

    read table it_saida into wa_saida index e_row_id-index.

    case e_column_id-fieldname.
      when 'AUFNR'.
        set parameter id 'ANR' field wa_saida-aufnr.

        call transaction 'IW33' and skip first screen.

    endcase.
  endmethod.
endclass.                   "ON_DOUBLE_CLICK


*&---------------------------------------------------------------------*
*&  PARAMETROS
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame.
  select-options: s_aufnr for aufk-aufnr,
                  s_auart for aufk-auart,
                  s_werks for aufk-werks,
                  s_bukrs for aufk-bukrs obligatory,
                  s_erdat for aufk-erdat obligatory,
                  s_pdat2 for aufk-pdat2.
selection-screen end of block b1.


*&---------------------------------------------------------------------*
*&  INICIO
*&---------------------------------------------------------------------*
initialization.


*&---------------------------------------------------------------------*
*&  START
*&---------------------------------------------------------------------*
start-of-selection.
  perform f_selecao_dados.

  perform f_manipula_dados.

  perform f_exibir_dados.


*&---------------------------------------------------------------------*
*&  SELECIONA DADOS
*&---------------------------------------------------------------------*
form f_selecao_dados.
  free: it_aufk, it_eqkt, it_afih.

  select *
    from aufk
    into table it_aufk
    where aufnr in s_aufnr
    and   auart in s_auart
    and   werks in s_werks
    and   bukrs in s_bukrs
    and   erdat in s_erdat
    and   idat2 in s_pdat2
    AND   idat3 in s_pdat2.

  if sy-subrc eq 0.
    select * from afih into table it_afih
      for all entries in it_aufk
      where aufnr eq it_aufk-aufnr.
    if sy-subrc eq 0.
      select * from eqkt into table it_eqkt
        for all entries in it_afih
        where equnr eq it_afih-equnr
          and spras eq sy-langu.
    endif.
  endif.
endform.


*&---------------------------------------------------------------------*
*&  MANIPULA DADOS
*&---------------------------------------------------------------------*
form f_manipula_dados.

  free: it_saida.
  if it_aufk is not initial.
    loop at it_aufk into wa_aufk.
      wa_saida-aufnr = wa_aufk-aufnr.
      wa_saida-ktext = wa_aufk-ktext.
      wa_saida-werks = wa_aufk-werks.
      wa_saida-bukrs = wa_aufk-bukrs.
      wa_saida-auart = wa_aufk-auart.
      wa_saida-erdat = wa_aufk-erdat.
      if wa_aufk-idat2 is not initial.
        wa_saida-pdat2 = wa_aufk-idat2.
      endif.

      if wa_aufk-idat2 is not initial.
        wa_saida-pdat2 = wa_aufk-idat2.
      endif.

      wa_saida-status = 'Aberta'.

      if wa_aufk-phas1 is not initial.
        wa_saida-status = 'Liberada'.
      elseif wa_aufk-phas2 is not initial.
        wa_saida-status = 'Encerrada'.
      elseif wa_aufk-phas3 is not initial.
        wa_saida-status = 'Encerrada'.
      endif.

      if wa_aufk-idat2 is not initial or wa_aufk-idat3 is not initial.
        if wa_aufk-idat2 is not initial.
          wa_saida-total_dias = wa_aufk-idat2 - wa_aufk-erdat.
        endif.

        if wa_aufk-idat3 is not initial.
          wa_saida-total_dias = wa_aufk-idat3 - wa_aufk-erdat.
        endif.

      else.
        wa_saida-total_dias = sy-datum - wa_aufk-erdat.
      endif.


      "Verificar se tem equipamento e pegar dados.
      read table it_afih into data(wa_afih) with key aufnr = wa_aufk-aufnr.
      if sy-subrc eq 0.
        read table it_eqkt into data(wa_eqkt) with key equnr = wa_afih-equnr.
        if sy-subrc eq 0.
          wa_saida-equnr = wa_eqkt-equnr.
          wa_saida-eqktx = wa_eqkt-eqktx.
        endif.
      endif.

      t_cor =
      value #(
               ( fname = 'ERDAT'      color-col = '3' color-int = '1'  color-inv = '1' )
               ( fname = 'PDAT2'      color-col = '3' color-int = '1'  color-inv = '1' )
               ( fname = 'TOTAL_DIAS' color-col = '3' color-int = '1'  color-inv = '1' )
               ( fname = 'STATUS'     color-col = '5' color-int = '1'  color-inv = '1' ) ).
      append lines of t_cor to it_color.

      wa_saida-color_cell[] = it_color[].

      append wa_saida to it_saida.

      clear: wa_aufk, wa_saida.
      free: it_color, t_cor.

    endloop.
  endif.
endform.


*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
form f_exibir_dados .

  call screen 100.

endform.


*&---------------------------------------------------------------------*
*& Form f_imprimir_dados
*&---------------------------------------------------------------------*
form f_init_alv .

  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.


  perform f_build_layout.

  perform f_fieldcatalog.

  variante = value #( report = sy-repid ).



  if g_grid is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida lines data(v_lines).
    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Registros:' valor = v_lines ) to i_filtros.

    p_text = 'Relatório Oficial de Ordens de Serviço'.
  endif.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.

    set handler: lcl_event_handler=>on_hotspot_click for g_grid.

    call method g_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid->set_selected_rows
        exporting
          it_index_rows = t_rows.
    endif.

  else.
    call method g_grid->refresh_table_display( is_stable = w_stable ).
  endif.

  wl_layout-colwidth_optimize = 'X'.


endform.


*&---------------------------------------------------------------------*
*& Form montar_layout
*&---------------------------------------------------------------------*
form f_fieldcatalog .

  free t_fieldcat[].

  perform f_estrutura_alv using:
 01  ''   ''   'IT_SAIDA'   'AUFNR      '            'Ordem de Manutenção    '       '45'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'KTEXT      '            'Descrição da Atividade '       '45'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'EQUNR      '            'Equipamento            '       '45'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'EQKTX      '            'Descrição equipamento  '       '45'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'WERKS      '            'Filial                 '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'BUKRS      '            'Empresa                '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'   'AUART      '            'Tipo de Ordem          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'STATUS     '            'Status                 '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'   'ERDAT      '            'Data de Criação        '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'   'PDAT2      '            'Data de Encerramento   '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA'   'TOTAL_DIAS '            'Total de Dias          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

endform.


*&---------------------------------------------------------------------*
*& Form  f_estrutura_alv
*&---------------------------------------------------------------------*
form f_estrutura_alv  using value(p_col_pos)       type i                    "1
                           value(p_ref_tabname)   like dd02d-tabname        "2
                           value(p_ref_fieldname) like dd03d-fieldname      "3
                           value(p_tabname)       like dd02d-tabname        "4
                           value(p_field)         like dd03d-fieldname      "5
                           value(p_scrtext_l)     like dd03p-scrtext_l      "6
                           value(p_outputlen)                               "7
                           value(p_edit)                                    "8
                           value(p_sum)                                     "9
                           value(p_just)                                    "10
                           value(p_hotspot)                                 "11
                           value(p_f4)                                      "12
                           value(p_checkbox)                                "13
                           value(p_style)                                   "14
                           value(p_no_out)                                  "15
                           value(p_icon)                                    "16
                           value(p_fix).                                    "17

  clear w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  if p_field = 'AUFNR'.
    w_fieldcat-hotspot = abap_true.
    w_fieldcat-no_zero = abap_true.
  endif.

  if p_field = 'EQUNR'.
    w_fieldcat-no_zero = abap_true.
  endif.

  append w_fieldcat to t_fieldcat.

endform.


form f_build_layout.
  w_layout-ctab_fname = 'COLOR_CELL'.
  w_layout-cwidth_opt = abap_true.
endform.


*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100'.

  perform f_init_alv.
endmodule.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.

    when 'EXIT' or 'CANCEL'.
      leave program.
  endcase.
endmodule.
