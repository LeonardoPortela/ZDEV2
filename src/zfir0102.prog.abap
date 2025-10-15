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
**|    + Rodrigo Carvalho ( rodrigo.sa@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatório para consultar e configurar chaves bancárias                    |*
**/===========================================================================\*
report zfir0102.


**********************************************************************
* Tables
**********************************************************************
tables: t028g.


**********************************************************************
* Types
**********************************************************************

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

types: begin of ty_t028g,
         vgtyp type t028g-vgtyp,
         vgext type t028g-vgext,
         vozpm type t028g-vozpm,
         vgint type t028g-vgint,
       end of ty_t028g,

       begin of ty_t033f,
         eigr1 type t033f-eigr1,
         bsch1 type t033f-bsch1,
         ktos1 type t033f-ktos1,
         bsch2 type t033f-bsch2,
         ktos2 type t033f-ktos2,
       end of ty_t033f,

       begin of ty_eigr1,
         eigr1 type t033f-eigr1,
       end of ty_eigr1,

       begin of ty_ktos1,
         ktos1 type t033f-ktos1,
       end of ty_ktos1,

       begin of ty_ktos2,
         ktos2 type t033f-ktos2,
       end of ty_ktos2,

       begin of ty_konto1,
         ktosy type t033g-ktosy,
         konto type t033g-konto,
       end of ty_konto1,

       begin of ty_konto2,
         ktosy type t033g-ktosy,
         konto type t033g-konto,
       end of ty_konto2,

       begin of ty_skat,
         saknr type skat-saknr,
         txt50 type skat-txt50,
       end of ty_skat,

       begin of ty_skat_saknr,
         saknr type skat-saknr,
       end of ty_skat_saknr,


       begin of ty_alv_saida,
         vgtyp   type t028g-vgtyp, "Banco
         vgext   type t028g-vgext, "Operação Externa
         vgint   type t028g-vgint, "Regra
         vozpm   type t028g-vozpm, "Sinal +/- do extrato
         bsch1   type t033f-bsch1, "Chave conta extrato
         ktos1   type t033f-ktos1, "Símbolo cuenta 01
         xkonto1 type t033g-konto, "Conta extrato
         txt50_1 type skat-txt50,  "Descr. cuenta 1
         bsch2   type t033f-bsch2, "Chave conta transitória
         ktos2   type t033f-ktos2, "Símbolo cuenta 02
         xkonto2 type t033g-konto, "Conta transitória
         txt50_2 type skat-txt50,  "Descr. cuenta 2
       end   of ty_alv_saida.




**********************************************************************
* Tabela interna
**********************************************************************
data: t_alv_saida   type table of ty_alv_saida,
      wa_alv_saida  type ty_alv_saida,
      t_t028g       type table of ty_t028g,
      wa_t028g      type ty_t028g,
      t_t033f       type table of ty_t033f,
      wa_t033f      type ty_t033f,
      t_eigr1       type table of ty_eigr1,
      wa_eigr1      type ty_eigr1,
      t_ktos1       type table of ty_ktos1,
      wa_ktos1      type ty_ktos1,
      t_ktos2       type table of ty_ktos2,
      wa_ktos2      type ty_ktos2,
      t_konto1      type table of ty_konto1,
      wa_konto1     type ty_konto1,
      t_konto2      type table of ty_konto2,
      wa_konto2     type ty_konto2,
      t_skat        type table of ty_skat,
      wa_skat       type ty_skat,
      t_skat_saknr  type table of ty_skat_saknr,
      wa_skat_saknr type ty_skat_saknr.



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
  select-options: s_vgtyp  for t028g-vgtyp. "obligatory.
  parameter       s_vgext  type t028g-vgext.
selection-screen end   of block b3.


at selection-screen on value-request for s_vgext.
  perform ZF_SELECT_vgext.


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

  " Seleciona Atribuição de operações externas a operações internas
  if s_vgext is not initial.
    select vgtyp vgext vozpm vgint
           from t028g
           into table t_t028g
           where vgtyp in s_vgtyp
           and   vgext eq s_vgext.
  else.
    select vgtyp vgext vozpm vgint
       from t028g
       into table t_t028g
       where vgtyp in s_vgtyp.
  endif.

  if t_t028g[] is initial.
    message i005(zfi).
    stop.
  endif.

  sort t_t028g by vgtyp vgext vozpm vgint.

  loop at t_t028g into wa_t028g.
    if not wa_t028g-vgint is initial.
      wa_eigr1-eigr1 = wa_t028g-vgint.
      append wa_eigr1 to t_eigr1.
    endif.
  endloop.

  if not t_eigr1[] is initial.
    sort t_eigr1 by eigr1.
    delete adjacent duplicates from t_eigr1 comparing eigr1.

    " Seleciona Determinação de contas: regras de contabilização
    select eigr1 bsch1 ktos1 bsch2 ktos2
           from t033f
           into table t_t033f
           for all entries in t_eigr1
           where anwnd eq '0001'
           and   eigr1 eq t_eigr1-eigr1.

    sort t_t033f by eigr1.

    loop at t_t033f into wa_t033f.
      if not wa_t033f-ktos1 is initial.
        wa_ktos1-ktos1 = wa_t033f-ktos1.
        append wa_ktos1 to t_ktos1.
      endif.

      if not wa_t033f-ktos2 is initial.
        wa_ktos2-ktos2 = wa_t033f-ktos2.
        append wa_ktos2 to t_ktos2.
      endif.
    endloop.

    sort t_ktos1 by ktos1.
    delete adjacent duplicates from t_ktos1 comparing ktos1.

    sort t_ktos2 by ktos2.
    delete adjacent duplicates from t_ktos2 comparing ktos2.

  endif.

  if not t_ktos1[] is initial.

    " Seleciona Determinação de contas
    select ktosy konto
          from t033g
          into table t_konto1
          for all entries in t_ktos1
          where anwnd eq '0001'
          and   ktopl eq '0050'
          and   ktosy eq t_ktos1-ktos1.

    sort t_konto1 by ktosy.

  endif.

  loop at t_konto1 into wa_konto1.
    wa_skat_saknr-saknr = wa_konto1-konto.
    replace all occurrences of '+' in wa_skat_saknr-saknr with ' '.
    unpack wa_skat_saknr-saknr to wa_skat_saknr-saknr.
    append wa_skat_saknr to t_skat_saknr.
  endloop.

  if not t_ktos2[] is initial.

    " Seleciona Determinação de contas
    select ktosy konto
            from t033g
            into table t_konto2
            for all entries in t_ktos2
            where anwnd eq '0001'
            and   ktopl eq '0050'
            and   ktosy eq t_ktos2-ktos2.

    sort t_konto2 by ktosy.

  endif.

  loop at t_konto2 into wa_konto2.
    wa_skat_saknr-saknr = wa_konto2-konto.
    replace all occurrences of '+' in wa_skat_saknr-saknr with ' '.
    unpack wa_skat_saknr-saknr to wa_skat_saknr-saknr.
    append wa_skat_saknr to t_skat_saknr.
  endloop.

  sort t_skat_saknr by saknr.
  delete adjacent duplicates from t_skat_saknr comparing saknr.

  if not t_skat_saknr[] is initial.

    " Seleciona Mestre de contas do Razão
    select saknr txt50
           from skat
           into table t_skat
           for all entries in t_skat_saknr
           where spras eq 'E'
           and   ktopl eq '0050'
           and   saknr eq t_skat_saknr-saknr.

    sort t_skat by saknr.

  endif.



endform.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
form f_processa_dados .

  data: vl_eigr1 type t033f-eigr1.

  loop at t_t028g into wa_t028g.

    clear wa_alv_saida.

    wa_alv_saida-vgtyp      = wa_t028g-vgtyp.  "Banco
    wa_alv_saida-vgext      = wa_t028g-vgext.  "Operação Externa
    wa_alv_saida-vgint      = wa_t028g-vgint.  "Regra
    wa_alv_saida-vozpm      = wa_t028g-vozpm.  "Sinal +/- do extrato

    vl_eigr1 = wa_t028g-vgint.
    read table t_t033f into wa_t033f with key eigr1 = vl_eigr1 binary search.
    if sy-subrc eq 0.
      wa_alv_saida-bsch1     = wa_t033f-bsch1. "Chave conta extrato
      wa_alv_saida-ktos1     = wa_t033f-ktos1. "Símbolo cuenta 01

      read table t_konto1 into wa_konto1 with key ktosy = wa_t033f-ktos1 binary search.
      if sy-subrc eq 0.
        replace all occurrences of '+' in wa_konto1-konto with ' '. unpack wa_konto1-konto to wa_konto1-konto.
        wa_alv_saida-xkonto1 = wa_konto1-konto. "Conta extrato

        read table t_skat into wa_skat with key saknr = wa_konto1-konto binary search.
        if sy-subrc eq 0.

          wa_alv_saida-txt50_1 = wa_skat-txt50. "Descr. cuenta 1

        endif.

      endif.

      wa_alv_saida-bsch2       = wa_t033f-bsch2. "Chave conta transitória
      wa_alv_saida-ktos2       = wa_t033f-ktos2. "Símbolo cuenta 02

      read table t_konto2 into wa_konto2 with key ktosy = wa_t033f-ktos2 binary search.
      if sy-subrc eq 0.
        replace all occurrences of '+' in wa_konto2-konto with ' '. unpack wa_konto2-konto to wa_konto2-konto.
        wa_alv_saida-xkonto2    = wa_konto2-konto. "Conta transitória

        read table t_skat into wa_skat with key saknr = wa_konto2-konto binary search.
        if sy-subrc eq 0.

          wa_alv_saida-txt50_2 = wa_skat-txt50. "Descr. cuenta 2

        endif.

      endif.

      append wa_alv_saida to t_alv_saida.

    endif.

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
form f_exibir_dados .

  call screen 100.

endform.

include zfir0102_status_0100o01.
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
 01  ''   ''   'T_ALV_SAIDA'   'vgtyp'            'Banco'                      '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'T_ALV_SAIDA'   'vgext'            'Operação Externa'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'T_ALV_SAIDA'   'vgint'            'Regra'                      '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'T_ALV_SAIDA'   'vozpm'            'Sinal +/- do extrato'       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'T_ALV_SAIDA'   'bsch1'            'Chave conta extrato'        '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'T_ALV_SAIDA'   'ktos1'            'Símbolo cuenta 01'          '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'T_ALV_SAIDA'   'xkonto1'          'Conta extrato'              '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'T_ALV_SAIDA'   'txt50_1'          'Descr. cuenta 1'            '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'T_ALV_SAIDA'   'bsch2'            'Chave conta transit.'       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'T_ALV_SAIDA'   'ktos2'            'Símbolo cuenta 02'          '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'T_ALV_SAIDA'   'xkonto2'          'Conta transitória'          '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''   ''   'T_ALV_SAIDA'   'txt50_2'          'Descr. cuenta 2'            '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.


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


  append w_fieldcat to t_fieldcat.

endform.

include zfir0102_user_command_0100i01.
*&---------------------------------------------------------------------*
*& Form ZF_SELECT_vgext
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form ZF_SELECT_vgext .

  check not s_vgtyp[] is initial.

  select vgtyp vgext vozpm vgint into corresponding fields of table it_T028G
  from t028g
  where vgtyp in s_vgtyp.
*
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
*     DDIC_STRUCTURE  = ' '
      retfield        = 'VGEXT'
      value_org       = 'S'
    tables
      value_tab       = it_T028G
      return_tab      = it_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  loop at it_return into data(wa_return).
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    read table it_T028G into data(ws_t028g) with key vgext = wa_return-fieldval.
    if sy-subrc is initial.
      s_vgext = ws_t028g-vgext.
    endif.
  endloop.

endform.
