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
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br)                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatório para consultar e configurar chaves bancárias                    |*
**/===========================================================================\*
report zmmr0206.


**********************************************************************
* Tables
**********************************************************************
tables: ausp, inob, kssk,mara, makt, cabn.


**********************************************************************
* Types
**********************************************************************

types: begin of ty_saida,
         matnr              type mara-matnr,
         mtart              type mara-mtart,
         atinn              type ausp-atinn,
         maktx              type makt-maktx,
         atwrt              type ausp-atwrt,
         atflv              type ausp-atflv,
         atbez              type cabnt-atbez, "ATBEZ
         text_C_A           type char30,
         text_veriodicidade type char30,
         text_validade      type sy-datum,
       end of ty_saida.

**********************************************************************
* Tabela interna
**********************************************************************
data: t_alv_saida  type table of ty_saida,
      t_saida      type table of ty_saida,
      it_dados     type table of  ty_saida,
      wa_alv_saida type ty_saida.



data: vga_SOLLWERT  type cha_class_view-sollwert,
      vga_value     type cha_class_data-sollwert,
      vga_CHA_CLASS type cha_class_data-stellen.




*------------------------------------
*---- ALV
*------------------------------------
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
*
      zcl_util             type ref to zcl_util.

data: variante         like disvariant.
data: gs_variant_c type disvariant.

data: it_return type table of ddshretval,
      it_T028G  type table of t028g.


**********************************************************************
* ranges
**********************************************************************
data: rg_augdt type range of acdoca-augdt,
      wa_augdt like line  of rg_augdt.



*-----------------------------------------------------------------*
*   TYPE-POOLS                                                    *
*-----------------------------------------------------------------*
type-pools: slis.



selection-screen begin of block b3 with frame title text-001.
  select-options: s_atinn  for ausp-atinn, "obligatory.
                  s_matnr  for mara-matnr,
                  s_mtart  for mara-mtart no intervals no-extension obligatory.
selection-screen end   of block b3.


*at selection-screen on value-request for s_vgext.
*  perform ZF_SELECT_vgext.


**********************************************************************
* inicio
**********************************************************************
initialization.



**********************************************************************
* START
**********************************************************************
start-of-selection.

  perform f_selecao_dados.

  perform f_processa_dados.

  perform f_exibir_dados.


*&---------------------------------------------------------------------*
*& Form f_selecao_dados
*&---------------------------------------------------------------------*
form f_selecao_dados .

  free: t_alv_saida.
  select *
  from mara as c
  inner join makt as d on d~matnr eq c~matnr and spras eq @sy-langu
  into corresponding fields of table @t_alv_saida
  where c~matnr in @s_matnr
  and c~mtart in @s_mtart.

  free: it_dados.
  select c~matnr c~mtart a~atinn d~maktx a~atwrt a~atflv e~atbez
  from ausp as a
  inner join inob as b on b~cuobj eq a~objek
  inner join mara as c on c~matnr eq b~objek
  inner join makt as d on d~matnr eq c~matnr and spras eq sy-langu
  inner join cabnt as e on e~atinn eq a~atinn and e~spras eq sy-langu
  into table it_dados
    for all entries in t_alv_saida
  where c~matnr eq t_alv_saida-matnr
  and a~atinn in s_atinn
  and c~mtart in s_mtart.


endform.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
form f_processa_dados.

  free: t_saida.
  t_saida = t_alv_saida.
  sort t_alv_saida by matnr mtart.
  delete adjacent duplicates from t_alv_saida comparing  matnr mtart.

  loop at t_alv_saida assigning field-symbol(<ws_saida>).
      loop at it_dados assigning field-symbol(<wa_saida>) where matnr eq <ws_saida>-matnr and mtart eq <ws_saida>-mtart.
      if <wa_saida>-atwrt is initial.
        data(l_character) = conv char8( <wa_saida>-atflv ).

        vga_value = <wa_saida>-atflv.

        call function 'QSS0_FLTP_TO_CHAR_CONVERSION'
          exporting
            i_number_of_digits       = vga_CHA_CLASS
            i_fltp_value             = vga_value
            i_value_not_initial_flag = 'X'
            i_screen_fieldlength     = 16
          importing
            e_char_field             = vga_SOLLWERT.

        <wa_saida>-atwrt = vga_SOLLWERT.
      endif.

      condense <wa_saida>-atwrt no-gaps.

      case <wa_saida>-atbez.
        when 'Código C.A.'.
          <ws_saida>-text_C_A = <wa_saida>-atwrt.
        when 'Periodicidade troca EPI (Dias)'.
          <ws_saida>-text_veriodicidade = <wa_saida>-atwrt.
        when 'Validade do C.A.'.
          <ws_saida>-text_validade = <wa_saida>-atwrt.
        when others.
      endcase.
      clear: l_character.
    endloop.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
form f_exibir_dados .

  call screen 100.

endform.

*&---------------------------------------------------------------------*
*& Form f_init_alv
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


  perform f_fieldcatalog.

  variante = value #( report = sy-repid ).


  if g_grid is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table t_alv_saida lines data(v_lines).
    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Registros:' valor = v_lines ) to i_filtros.

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

    call method g_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = t_alv_saida[]
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
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
form f_fieldcatalog .

  free t_fieldcat[].

  perform f_estrutura_alv using:
 01  ''   ''   'T_ALV_SAIDA'   'MATNR             '            'Material                          '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'T_ALV_SAIDA'   'MAKTX             '            'Desc.material                     '       '45'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'T_ALV_SAIDA'   'MTART             '            'Tipo material                     '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
* 04  ''   ''   'T_ALV_SAIDA'   'ATINN             '            'Caracteristica                    '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'T_ALV_SAIDA'   'TEXT_C_A          '            'Código C.A.                       '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'T_ALV_SAIDA'   'TEXT_VERIODICIDADE'            'Periodicidade troca EPI (Dias)    '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'T_ALV_SAIDA'   'TEXT_VALIDADE     '            'Validade do C.A.                  '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

endform.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_01
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
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

  if w_fieldcat-fieldname = 'MATNR'.
    w_fieldcat-no_zero = abap_true.
  endif.


  append w_fieldcat to t_fieldcat.

endform.
*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100'.

  perform f_init_alv.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_100 input.

  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.

    when 'EXIT'.
      leave program.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*& Form fm_criar_objetos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_criar_objetos .

endform.
