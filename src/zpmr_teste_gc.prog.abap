*&---------------------------------------------------------------------*
*& Report ZPMR_TESTE_GC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPMR_TESTE_GC.
TABLES: sflight, scarr.

TYPES:

**Tabela principal / Estrutura de saida
    BEGIN OF TY_SAIDA,
      CARRID TYPE SFLIGHT-CARRID,
      CONNID TYPE SFLIGHT-CONNID,
      FLDATE TYPE SFLIGHT-FLDATE,
      PRICE TYPE SFLIGHT-PRICE,
      CURRENCY TYPE SFLIGHT-CURRENCY,
      CARRNAME TYPE SCARR-CARRNAME,
      CURRCODE TYPE SCARR-CURRCODE,
      URL TYPE SCARR-URL,
    END OF TY_SAIDA,

**Estrutura de seleção de dados

    BEGIN OF TY_SFLIGHT,
      CARRID TYPE SFLIGHT-CARRID,
      CONNID TYPE SFLIGHT-CONNID,
      FLDATE TYPE SFLIGHT-FLDATE,
      PRICE TYPE SFLIGHT-PRICE,
      CURRENCY TYPE SFLIGHT-CURRENCY,
      END OF TY_SFLIGHT,

      BEGIN OF TY_SCARR,
         CARRID TYPE SCARR-CARRID,
         CARRNAME TYPE SCARR-CARRNAME,
         CURRCODE TYPE SCARR-CURRCODE,
         URL TYPE SCARR-URL,
      END OF TY_SCARR.

**Declaração de tabela
      DATA: IT_SFLIGHT TYPE TABLE OF TY_SFLIGHT,
            WA_SFLIGHT TYPE TY_SFLIGHT,
            IT_SCARR TYPE TABLE OF TY_SCARR,
            WA_SCARR TYPE TY_SCARR,
            IT_SAIDA TYPE TABLE OF TY_SAIDA,
            WA_SAIDA TYPE TY_SAIDA.

DATA: CHECK_INFO TYPE CHAR01.

*Declarações do ALV
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
      ok_code              type sy-ucomm.

data: variante         like disvariant.

*----------------------------------------------------------------------*

* TELA DE SELEÇÃO

*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:      P_CARRID  FOR SFLIGHT-CARRID,

                  P_CONNID    FOR SFLIGHT-CONNID,

                P_FLDATE    FOR SFLIGHT-FLDATE.

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

PERFORM PF_SELECAO_DE_DADOS.

PERFORM PF_TRATA_DADOS.

PERFORM PF_EXIBIR_DADOS.
*&---------------------------------------------------------------------*
*& Form PF_SELECAO_DE_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PF_SELECAO_DE_DADOS .

SELECT * FROM SFLIGHT INTO CORRESPONDING FIELDS OF TABLE IT_SFLIGHT
  WHERE CARRID IN P_CARRID AND CONNID IN P_CONNID AND FLDATE IN P_FLDATE.

  IF IT_SFLIGHT IS NOT INITIAL.
    SELECT * FROM SCARR INTO CORRESPONDING FIELDS OF TABLE IT_SCARR
      FOR ALL ENTRIES IN IT_SFLIGHT
      WHERE CARRID = IT_SFLIGHT-CARRID.
    ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form PF_TRATA_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PF_TRATA_DADOS .
FREE IT_SAIDA.
LOOP AT IT_SFLIGHT INTO WA_SFLIGHT.
  CLEAR: CHECK_INFO, WA_SAIDA.
  WA_SAIDA-CARRID = WA_SFLIGHT-CARRID.
  WA_SAIDA-CONNID = WA_SFLIGHT-CONNID.
  WA_SAIDA-FLDATE = WA_SFLIGHT-FLDATE.
  WA_SAIDA-CURRENCY = WA_SFLIGHT-CURRENCY.
  WA_SAIDA-PRICE = WA_SFLIGHT-PRICE.

  LOOP AT IT_SCARR INTO WA_SCARR WHERE CARRID = WA_SFLIGHT-CARRID.
    WA_SAIDA-CARRNAME = WA_SCARR-CARRNAME.
    WA_SAIDA-CURRCODE = WA_SCARR-CURRCODE.
    WA_SAIDA-URL = WA_SCARR-URL.
    APPEND WA_SAIDA TO IT_SAIDA.
    CHECK_INFO = ABAP_TRUE.
  ENDLOOP.

  IF CHECK_INFO <> ABAP_TRUE.
    APPEND WA_SAIDA TO IT_SAIDA.
  ENDIF.

ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PF_EXIBIR_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PF_EXIBIR_DADOS .
IF IT_SAIDA IS NOT INITIAL.
  CALL SCREEN 100.

ENDIF.
ENDFORM.

MODULE STATUS_0100 OUTPUT.
SET PF-STATUS 'ST_0100'.
SET TITLEBAR 'TITLE_0100'.

PERFORM PF_ALV.

ENDMODULE.

MODULE USER_COMMAND_0100 INPUT.
CASE SY-UCOMM. "Chamada do botão
  WHEN 'BACK'.
    LEAVE TO SCREEN 0.
  WHEN 'EXIT'.
    LEAVE PROGRAM.
  WHEN OTHERS.
ENDCASE.
ENDMODULE.

FORM PF_ALV .
    data: wl_layout type slis_layout_alv.

     data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.

      perform pf_fieldcatalog.

      variante = value #( report = sy-repid ).

        if g_grid is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida lines data(v_lines).
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
ENDFORM.

FORM pf_fieldcatalog .

  free t_fieldcat[].

  perform pf_estrutura_alv using:
 01  ''   ''   'IT_SAIDA'   'CARRID  '            'Companhia        '       '3  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'CONNID  '            'Conexão          '       '4  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'FLDATE  '            'Data do voo      '       '8  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'PRICE   '            'Preço            '       '15 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'CURRENCY'            'Moeda            '       '5  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'CARRNAME'            'Nome da Companhia'       '20 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'   'CURRCODE'            'Moeda Interna    '       '5  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'URL     '            'URL              '       '255'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.

form pf_estrutura_alv  using value(p_col_pos)       type i                    "1
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
