*----------------------------------------------------------------------*
***INCLUDE MZSETUP_USER_COMMAND_1001 .
*----------------------------------------------------------------------*

tables: zmmt_ee_zgr_imp.

types begin of tp_zmmt_ee_zgr_imp_alv.
        include structure zmmt_ee_zgr_imp.
types: bezei  type bezei20,
       text40 type text40.
types end of tp_zmmt_ee_zgr_imp_alv.

data: ok_code_1001 type sy-ucomm.

data: it_zmmt_ee_zgr_imp_alv type table of tp_zmmt_ee_zgr_imp_alv with header line,
      it_zmmt_ee_zgr_imp     type table of zmmt_ee_zgr_imp with header line,
      setup_catalogo_1001    type lvc_t_fcat,
      setup_prim_1001        type c length 1,
      setup_container_1001   type ref to cl_gui_custom_container,
      setup_alv_1001         type ref to cl_gui_alv_grid,
      setup_scroll_col       type lvc_s_col,
      setup_scroll_row       type lvc_s_roid.

constants:
     ok_inse_1001 type sy-ucomm value 'INSE_1001',
     ok_edit_1001 type sy-ucomm value 'EDIT_1001',
     ok_excl_1001 type sy-ucomm value 'EXCL_1001'.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1001 input.

  data: vg_verifica_1001 type sy-subrc.

  case ok_code_1001.
    when ok_inse_1001.
      call function 'Z_SAPMZSETUP_1001_CAD'.
      perform consulta_1001.
    when ok_edit_1001.
      perform verifica_selecao_1001 using vg_verifica_1001.
      if vg_verifica_1001 is initial.
        call function 'Z_SAPMZSETUP_1001_CAD'
          exporting
            p_cd_param = zmmt_ee_zgr_imp-cd_param.
        perform consulta_1001.
      endif.
    when ok_excl_1001.
      perform verifica_selecao_1001 using vg_verifica_1001.
      if vg_verifica_1001 is initial.
        call function 'Z_SAPMZSETUP_1001_CAD'
          exporting
            p_cd_param = zmmt_ee_zgr_imp-cd_param
            p_excluir  = c_x.

        perform consulta_1001.
      endif.
  endcase.

endmodule.                 " USER_COMMAND_1001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1001 output.

  set pf-status 'PF1001'.
  set titlebar 'TL1001'.

endmodule.                 " STATUS_1001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1001_exit input.
  call method docking->link
    exporting
      repid = sy-repid
      dynnr = '0002'.
  leave to screen 0002.
endmodule.                 " USER_COMMAND_1001_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_alv_1001 output.

  perform setup_cria_1001.

endmodule.                 " CRIA_ALV_1001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SETUP_CRIA_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_cria_1001 .

  constants: tabela_1001 type string value 'IT_ZMMT_EE_ZGR_IMP_ALV'.

  data: text_n001 type c length 50 value 'Código',
        text_n002 type c length 50 value 'Imposto',
        text_n003 type c length 50 value 'UF',
        text_n004 type c length 50 value 'Estado',
        text_n005 type c length 50 value 'Pessoa Física',
        text_n006 type c length 50 value 'Descrição Imposto'.

  data: vg_ttb_button type ttb_button.

  if setup_prim_1001 is initial.

    perform consulta_1001.

    create object setup_container_1001
      exporting
        container_name = 'ALV'.

    create object setup_alv_1001
      exporting
        i_parent = setup_container_1001.

    perform z_estrutura_fieldcat tables setup_catalogo_1001 using:
        tabela_1001 'CD_PARAM'        text_n001 ' ' 01 05 space space space space space space,
        tabela_1001 'WITHT'           text_n002 ' ' 02 05 space space space space space space,
        tabela_1001 'TEXT40'          text_n006 ' ' 03 30 space space space space space space,
        tabela_1001 'REGIO'           text_n003 ' ' 04 03 space space space space space space,
        tabela_1001 'BEZEI'           text_n004 ' ' 05 30 space space space space space space,
        tabela_1001 'P_FISICA'        text_n005 ' ' 06 04 space space space space space space.

    clear: setup_gs_layout.
    setup_gs_layout-zebra    = c_x.
    setup_gs_layout-sel_mode = 'A'.

    call method setup_alv_1001->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = setup_gs_layout
      changing
        it_fieldcatalog = setup_catalogo_1001
        it_outtab       = it_zmmt_ee_zgr_imp_alv[].

*   Create Object for Event Handler
    setup_prim_1001 = c_x.

  endif.

  call method setup_alv_1001->refresh_table_display.

  call method setup_alv_1001->set_scroll_info_via_id
    exporting
      is_col_info = setup_scroll_col
      is_row_no   = setup_scroll_row.

endform.                    " SETUP_CRIA_1001




*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
form z_estrutura_fieldcat tables it_catalogo type lvc_t_fcat
                           using p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize.

  data setup_catalog_1001 type lvc_s_fcat.
  clear setup_catalog_1001.
  setup_catalog_1001-tabname     = p_tab_name.
  setup_catalog_1001-fieldname   = p_fieldname.
  setup_catalog_1001-scrtext_l   = p_texto_grande.
  setup_catalog_1001-scrtext_m   = p_texto_grande.
  setup_catalog_1001-scrtext_s   = p_texto_grande.
  setup_catalog_1001-hotspot     = p_hot.
  setup_catalog_1001-col_pos     = p_posicao.
  setup_catalog_1001-outputlen   = p_outputlen.
  setup_catalog_1001-fix_column  = p_fix_column.
  setup_catalog_1001-convexit    = p_convexit.
  setup_catalog_1001-do_sum      = p_do_sum.
  setup_catalog_1001-icon        = p_icon.
  setup_catalog_1001-just        = p_just.
  setup_catalog_1001-emphasize   = p_emphasize.
  append setup_catalog_1001 to it_catalogo.
endform.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form consulta_1001 .

  clear: it_zmmt_ee_zgr_imp_alv[],
         it_zmmt_ee_zgr_imp[].

  select * into table it_zmmt_ee_zgr_imp
        from zmmt_ee_zgr_imp.

  loop at it_zmmt_ee_zgr_imp.

    move-corresponding it_zmmt_ee_zgr_imp to it_zmmt_ee_zgr_imp_alv.

    clear: it_zmmt_ee_zgr_imp_alv-bezei,
           it_zmmt_ee_zgr_imp_alv-text40.

    if not it_zmmt_ee_zgr_imp_alv-regio is initial.
      select single bezei into it_zmmt_ee_zgr_imp_alv-bezei
        from t005u
       where spras eq sy-langu
         and land1 eq it_zmmt_ee_zgr_imp_alv-land1
         and bland eq it_zmmt_ee_zgr_imp_alv-regio.
    endif.

    select single text40 into it_zmmt_ee_zgr_imp_alv-text40
      from t059u
     where spras eq sy-langu
       and land1 eq it_zmmt_ee_zgr_imp_alv-land1
       and witht eq it_zmmt_ee_zgr_imp_alv-witht.

    append it_zmmt_ee_zgr_imp_alv.

  endloop.

endform.                    " CONSULTA_1001

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_VERIFICA_1001  text
*----------------------------------------------------------------------*
form verifica_selecao_1001  using p_vg_verifica_1001 type sy-subrc.

  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  p_vg_verifica_1001 = 1.

  call method setup_alv_1001->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  loop at it_selected_rows into wa_selected_rows.
    read table it_zmmt_ee_zgr_imp_alv index wa_selected_rows-index.
    read table it_zmmt_ee_zgr_imp into zmmt_ee_zgr_imp with key cd_param = it_zmmt_ee_zgr_imp_alv-cd_param.
    if sy-subrc is initial.
      p_vg_verifica_1001 = 0.
    endif.
  endloop.

endform.                    " VERIFICA_SELECAO_1001
