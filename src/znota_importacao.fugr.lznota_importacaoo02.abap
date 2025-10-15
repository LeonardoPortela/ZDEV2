*----------------------------------------------------------------------*
***INCLUDE LZNOTA_IMPORTACAOO02 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0002 output.

  if imp_dynnr_002 is initial.
    imp_dynnr_002 = c_2001.
  endif.

endmodule.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_2001 output.
  perform cria_alv_di.
endmodule.                 " STATUS_2001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_DI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_alv_di .

  constants: tabela_di type string value 'IT_ZNOTA_IMPORT'.

  data: text_n000 type c length 50 value 'Nr. DI',
        text_n001 type c length 50 value 'Dt. DI',
        text_n002 type c length 50 value 'Loc. Desembaraço',
        text_n003 type c length 50 value 'UF Desembaraço',
        text_n004 type c length 50 value 'Dt. Desembaraço',
        text_n005 type c length 50 value 'Cod. Exportador',
        text_n006 type c length 50 value 'Tp. Via Transporte',
        text_n007 type c length 50 value 'Vlr. AFRMM',
        text_n008 type c length 50 value 'Forma Importação'.

  if imp_prim_di is initial.
*
**   Create object for container
    create object imp_container_di
      exporting
        container_name = 'CUSTOMCONTROL'.
*
    create object imp_alv_di
      exporting
        i_parent = imp_container_di.

    perform z_estrutura_fieldcat tables it_imp_catalog_di using:
        tabela_di 'NDI'          text_n000 'X'   01 15 space 'ZNRDI' space space space space,
        tabela_di 'DDI'          text_n001 space 02 10 space space   space space space space,
        tabela_di 'XLOCDESEMB'   text_n002 space 03 20 space space   space space space space,
        tabela_di 'UFDESEMB'     text_n003 space 04 03 space space   space space space space,
        tabela_di 'DDESEMB'      text_n004 space 05 10 space space   space space space space,
        tabela_di 'CEXPORTADOR'  text_n005 space 06 20 space space   space space space space,
        tabela_di 'TPVIATRANSP'  text_n006 space 07 15 space space   space space space space,
        tabela_di 'VAFRMM'       text_n007 space 08 15 space space   space space space space,
        tabela_di 'TPINTERMEDIO' text_n008 space 09 15 space space   space space space space.

    imp_gs_layout-zebra    = c_x.
    imp_gs_layout-sel_mode = space.

    create object imp_event_di.
    set handler imp_event_di->handle_hotspot_di for imp_alv_di.

    call method imp_alv_di->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = imp_gs_layout
      changing
        it_fieldcatalog = it_imp_catalog_di
        it_outtab       = it_znota_import[].

    imp_prim_di = c_x.

  endif.

  call method imp_alv_di->refresh_table_display.

endform.                    " CRIA_ALV_DI

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_DI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form pesquisa_di  using  p_docnum type j_1bdocnum
                         p_itmnum type j_1bitmnum.

  select * into table it_znota_import
    from znota_import
   where docnum eq p_docnum
     and itmnum eq p_itmnum.

endform.                    " PESQUISA_DI

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0002 input.

  data: vg_selecao_pr type sy-subrc.

  case ok_code.
    when ok_tabdi1.
      imp_dynnr_002         = c_2001.
      tab_control-activetab = ok_tabdi1.
      clear: ok_code.
    when ok_tabdi2.
      perform verifica_selecao_di using vg_selecao_pr.
      if vg_selecao_pr is initial.
        imp_dynnr_002         = c_2003.
        tab_control-activetab = ok_tabdi2.
        PERFORM seleciona_adicoes_di using znota_import.
      else.
        message s035.
      endif.
      clear: ok_code.
  endcase.

endmodule.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_DI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form verifica_selecao_di  using p_vg_selecao_pr type sy-subrc.

  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  p_vg_selecao_pr = 4.

  call method imp_alv_di->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  if not it_selected_rows[] is initial.
    read table it_selected_rows index 1 into wa_selected_rows.
    read table it_znota_import into znota_import index wa_selected_rows-index.
    p_vg_selecao_pr = 0.
  endif.

endform.                    " VERIFICA_SELECAO_DI
