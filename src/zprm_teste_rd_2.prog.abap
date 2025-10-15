*&---------------------------------------------------------------------*
*& Report ZPRM_TESTE_RD_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprm_teste_rd_2.

TABLES: ekko, ekpo, lfa1, t001w.

TYPES:

  BEGIN OF ty_saida,
    ebeln TYPE ekko-ebeln, "Nº DO PEDIDO DE COMPRAS"
    bukrs TYPE ekko-bukrs, "CÓDIGO DA EMPRESA"
    bsart TYPE ekko-bsart, "TIPO DE PEDIDO"
    lifnr TYPE ekko-lifnr, "CÓDIGO DO FORNECEDOR"
    name1 TYPE lfa1-name1, "Nome DO FORNECEDOR"
    ebelp TYPE ekpo-ebelp, "LINHA DO PEDIDO DE COMPRAS"
    txz01 TYPE ekpo-txz01, "TEXTO DO MATERIAL"
    matnr TYPE ekpo-matnr, "CÓDIGO DO MATERIAL"
    werks TYPE ekpo-werks, "FILIAL"
    menge TYPE ekpo-menge, "QTD DA LINHA"
    netpr TYPE ekpo-netpr, "PREÇO LÍQUIDO UNITÁRIO DO PEDIDO"
    netwr TYPE ekpo-netwr, "VALOR LÍQUIDO TOTAL DO PEDIDO"
    brtwr TYPE ekpo-brtwr, "VALOR BRUTO TOTAL DO PEDIDO"
    mwskz TYPE ekpo-mwskz, "CÓDIGO IVA"
    aedat TYPE ekko-aedat, "DATA DE CRIAÇÃO"
    reswk TYPE ekko-reswk,
    name  TYPE t001w-name1,
  END OF ty_saida,


**  ESTRUTURA DA SELEÇÃO DE DADOS


  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
    bukrs TYPE ekko-bukrs,
    bsart TYPE ekko-bsart,
    lifnr TYPE ekko-lifnr,
    aedat TYPE ekko-aedat,
    reswk TYPE ekko-reswk,
  END OF ty_ekko,

  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    txz01 TYPE ekpo-txz01,
    matnr TYPE ekpo-matnr,
    werks TYPE ekpo-werks,
    menge TYPE ekpo-menge,
    netpr TYPE ekpo-netpr,
    netwr TYPE ekpo-netwr,
    brtwr TYPE ekpo-brtwr,
    mwskz TYPE ekpo-mwskz,
  END OF ty_ekpo,

  BEGIN OF ty_lfa1,
    name1 TYPE lfa1-name1,
    lifnr TYPE lfa1-lifnr,
  END OF ty_lfa1,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,
    name  TYPE t001w-name1,
  END OF ty_t001w.



**DECLARAÇÃO DAS TABELAS


DATA:
  it_saida   TYPE TABLE OF ty_saida,
  wa_saida   TYPE ty_saida,
  it_ekko    TYPE TABLE OF ty_ekko,
  wa_ekko    TYPE ty_ekko,
  it_ekpo    TYPE TABLE OF ty_ekpo,
  wa_ekpo    TYPE ty_ekpo,
  it_lfa1    TYPE TABLE OF ty_lfa1,
  wa_lfa1    TYPE ty_lfa1,
  it_t001w   TYPE TABLE OF ty_t001w,
  wa_t001w   TYPE ty_t001w,
  check_info TYPE char01.

** DECLARAÇÃO ALV


DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm.

DATA: variante         LIKE disvariant.

*-------------------------------------------------------------------------------------
*SELECT-OPTIONS
*-----------------------------------------------------------------------------------

SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_ebeln FOR ekko-ebeln,
                  p_aedat FOR ekko-aedat,
                  p_bukrs FOR ekko-bukrs,
                  p_werks FOR ekpo-werks,
                  p_bsart FOR ekko-bsart,
                  p_lifnr FOR ekko-lifnr,
                  p_mwskz FOR ekpo-mwskz.
SELECTION-SCREEN: END OF BLOCK a1.

*-------------------------------------------------------------------------------------
*
*                                  START OF SELECTION
*
*-------------------------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM pf_seleciona_dados.
  PERFORM pf_tratar_dados.
  PERFORM pf_exibir_dados.

  .

FORM pf_seleciona_dados .

  SELECT *
    FROM ekko
    INTO CORRESPONDING FIELDS OF TABLE it_ekko
    WHERE ebeln IN p_ebeln AND bukrs IN p_bukrs AND bsart IN p_bsart AND lifnr IN p_lifnr AND aedat IN p_aedat.

  IF it_ekko IS NOT INITIAL.

    LOOP AT it_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>).
      IF <fs_ekko>-bsart = 'ZUB'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_ekko>-reswk
          IMPORTING
            output = <fs_ekko>-lifnr.
      ENDIF.
    ENDLOOP.

    SELECT *
    FROM ekpo INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    FOR ALL ENTRIES IN it_ekko
    WHERE ebeln = it_ekko-ebeln
    AND werks IN p_werks AND mwskz IN p_mwskz.

    IF it_ekpo IS NOT INITIAL.

      SELECT *
      FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE it_lfa1
      FOR ALL ENTRIES IN it_ekko
      WHERE lifnr = it_ekko-lifnr.

      SELECT werks name1
        FROM t001w INTO TABLE it_t001w
        FOR ALL ENTRIES IN it_ekpo
        WHERE werks = it_ekpo-werks.
    ENDIF.
  ENDIF.

ENDFORM.

FORM pf_tratar_dados .

  FREE it_saida.
  LOOP AT it_ekko INTO wa_ekko.
    IF wa_ekko-bsart = 'ZUB'.


      CLEAR: check_info, wa_saida.
      wa_saida-ebeln = wa_ekko-ebeln.
      wa_saida-bukrs = wa_ekko-bukrs.
      wa_saida-bsart = wa_ekko-bsart.
      wa_saida-lifnr = wa_ekko-reswk.
      wa_saida-aedat = wa_ekko-aedat.

    ELSE.

      CLEAR: check_info, wa_saida.
      wa_saida-ebeln = wa_ekko-ebeln.
      wa_saida-bukrs = wa_ekko-bukrs.
      wa_saida-bsart = wa_ekko-bsart.
      wa_saida-lifnr = wa_ekko-lifnr.
      wa_saida-aedat = wa_ekko-aedat.

    ENDIF.
    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln = wa_ekko-ebeln.
      CLEAR: wa_t001w, wa_lfa1.
      READ TABLE  it_t001w[] INTO wa_t001w WITH KEY werks = wa_ekpo-werks.
      READ TABLE  it_lfa1[]  INTO wa_lfa1 WITH KEY lifnr = wa_ekko-lifnr.
      wa_saida-ebeln = wa_ekpo-ebeln.
      wa_saida-ebelp = wa_ekpo-ebelp.
      wa_saida-txz01 = wa_ekpo-txz01.
      wa_saida-matnr = wa_ekpo-matnr.
      wa_saida-werks = wa_ekpo-werks.
      wa_saida-menge = wa_ekpo-menge.
      wa_saida-netpr = wa_ekpo-netpr.
      wa_saida-netwr = wa_ekpo-netwr.
      wa_saida-brtwr = wa_ekpo-brtwr.
      wa_saida-mwskz = wa_ekpo-mwskz.
      wa_saida-name  = wa_t001w-name.
      wa_saida-name1 = wa_lfa1-name1.
      APPEND wa_saida TO it_saida.
      check_info = abap_true.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

FORM pf_exibir_dados .
  IF it_saida IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.
ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST-0100'.
  SET TITLEBAR 'TITLE_0100'.
  PERFORM pf_alv.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

FORM pf_alv .
  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.

  PERFORM pf_fieldcatalog.

  variante = VALUE #( report = sy-repid ).

  IF g_grid IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE it_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
        EXPORTING
          i_titulo  = CONV #( p_text )
          i_filtros = i_filtros
        CHANGING
          split     = dg_splitter_1
          alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.
**EXCLUSÃO DE BOTÕPES
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
**EXCLUSÃO DE BOTOES

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.
ENDFORM.

FORM pf_fieldcatalog .
  FREE t_fieldcat[].

  PERFORM pf_estrutura_alv USING:
 01  ''   ''   'IT_SAIDA'   'EBELN'            'Pedido de Compras      '       '3 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'BUKRS'            'Empresa                '       '3 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'BSART'            'Tipo do Pedido         '       '8 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'LIFNR'            'Cód. Fornecedor        '       '5 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'EBELP'            'Linha Pedido           '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'TXZ01'            'Descrição Material     '       '5 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  'MARA'   'MATNR'     'IT_SAIDA'   'MATNR'            'Cód. Material          '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'WERKS'            'Filial                 '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'   'MENGE'            'Qtd. Linha             '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'   'NETPR'            'Prç liq uni            '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA'   'NETWR'            'Vlr Liq total pedido   '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''   ''   'IT_SAIDA'   'BRTWR'            'Vlr Bruto total pedido '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 13  ''   ''   'IT_SAIDA'   'MWSKZ'            'IVA                    '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 14  ''   ''   'IT_SAIDA'   'AEDAT'            'Dt. Criação            '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 15  ''   ''   'IT_SAIDA'   'NAME1'            'Nome do fornecedor     '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 16  ''   ''   'IT_SAIDA'   'NAME'             'Nome filial            '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.

FORM pf_estrutura_alv  USING VALUE(p_col_pos)       TYPE i                    "1
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
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "17

  CLEAR w_fieldcat.
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

  IF w_fieldcat-fieldname = 'MATNR'.
    w_fieldcat-no_zero = abap_true.
    w_fieldcat-convexit = 'MATN1'.
  ENDIF.


  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.
