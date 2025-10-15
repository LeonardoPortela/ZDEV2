*----------------------------------------------------------------------*
***INCLUDE LZFUNCOES999 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9999  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9999 output.

  set pf-status 'PF9999'.
  set titlebar 'TL9999'.

  perform cria_alv.

  call method cockpit_alv_msg->refresh_table_display.

endmodule.                 " STATUS_9999  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999  INPUT
*&---------------------------------------------------------------------*
module user_command_9999 input.

  case ok_code.
    when ok_sair.
      leave to screen 0.
  endcase.

endmodule.                 " USER_COMMAND_9999  INPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form cria_alv .

  data: it_exclude_fcode type ui_functions,
        wa_exclude_fcode like line of it_exclude_fcode.

  constants: tabela_msg type string value 'IT_MENSAGENS'.

  if cockpit_prim_msg is initial.

    clear: it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_FILTER'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    append wa_exclude_fcode to it_exclude_fcode.

*   Create object for container
    create object cockpit_container_msg
      exporting
        container_name = 'CTN_MSG'.

    create object cockpit_alv_msg
      exporting
        i_parent = cockpit_container_msg.

    clear: it_cockpit_catalog.

    perform z_estrutura_fieldcat tables it_cockpit_catalog using:
        tabela_msg 'ICONS'          text-m00 ' ' 01 005 space space,
        tabela_msg 'TEXTO'          text-m01 ' ' 02 120 space space.

    call method cockpit_alv_msg->set_table_for_first_display
      exporting
        i_default            = space
        is_layout            = cockpit_gs_layout
        it_toolbar_excluding = it_exclude_fcode
      changing
        it_fieldcatalog      = it_cockpit_catalog
        it_outtab            = it_mensagens[].

    cockpit_prim_msg = c_x.

  endif.

endform.                    " CRIA_ALV

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
                                 p_convexit.

  data wa_catalog type lvc_s_fcat.

  clear wa_catalog.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  append wa_catalog to it_catalogo.
endform.                    " Z_ESTRUTURA_FIELDCAT
