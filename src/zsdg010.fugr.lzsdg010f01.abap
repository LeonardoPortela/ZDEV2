*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_alv USING p_alv.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  PERFORM f_estrutura_alv USING:
    01  ''              ''             'TG_DADOS'  'WERKS'         'Centro'              '08'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    02  ''              ''             'TG_DADOS'  'LGORT'         'Depósito'            '08'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    03  ''              ''             'TG_DADOS'  'CHARG'         'Lote'                '10'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    04  ''              ''             'TG_DADOS'  'NR_ROMANEIO'   'Romaneio'            '10'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    05  ''              ''             'TG_DADOS'  'NFNUM'         'Nota Fiscal'         '12'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    06  'VBAK'          'VBELN'        'TG_DADOS'  'VBELN'         'Ordem Venda'         '12'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    07  ''              ''             'TG_DADOS'  'MENGE'         'Quantidade'          '15'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    08  ''              ''             'TG_DADOS'  'MOTORISTA'     'Motorista'           '25'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    09  ''              ''             'TG_DADOS'  'PLACA_CAV'     'Placa Cavalo'        '12'   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' '.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon).                                   "16

  CLEAR wa_fcat.
  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = p_no_out.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-style       = p_style.
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.
  wa_fcat-icon        = p_icon.
* wa_fcat-colddictxt  = 'M'.
* wa_fcat-selddictxt  = 'M'.
* wa_fcat-tipddictxt  = 'M'.
* wa_fcat-col_opt     = 'X'.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

* APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: <fs_wa_saida>,
         <fs_it_saida>[],
         tg_field_screen_key[],
         tg_dd03l[],
         tg_dd04t[],
         tg_dd04t_out[],
         tg_dd03l_out[].

ENDFORM.
