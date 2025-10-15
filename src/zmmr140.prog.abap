*&---------------------------------------------------------------------*
*& Report  ZMMR140
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr140.

TABLES: ekbe, ekko, ekpo.


DATA: it_saida TYPE  zde_out_zmm0141_t,
      wa_saida TYPE  zde_out_zmm0141.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_2        TYPE REF TO cl_gui_container,
      dg_parent_2a       TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      picture            TYPE REF TO cl_gui_picture,
      gs_layout          TYPE lvc_s_layo,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog    TYPE TABLE OF lvc_s_fcat,
      wa_fieldcatalog    TYPE lvc_s_fcat,
      gs_scroll_col      TYPE lvc_s_col,
      gs_scroll_row      TYPE lvc_s_roid,
      gs_variant         TYPE disvariant,
      it_exclude_fcode   TYPE ui_functions,
      wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      table_element2     TYPE REF TO cl_dd_table_element,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area,
      column_3           TYPE REF TO cl_dd_area,
      dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
      it_header          TYPE kkblo_t_listheader WITH HEADER LINE.   "Cabeçalho


DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toobar            TYPE stb_button.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR ekko-bukrs NO INTERVALS OBLIGATORY ,  "EMPRESA
                  p_matnr FOR ekbe-matnr,  "MATERIAL
                  p_ebeln FOR ekbe-ebeln,  "PEDIDO DE TRANSFERENCIA
                  p_reswk FOR ekko-reswk,  "Centro Fornecedor
                  p_werks FOR ekpo-werks,  "CENTRO RECEPTOR
                  p_budat FOR ekbe-budat OBLIGATORY.  "DATA MOVIMENTO
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: ped   RADIOBUTTON GROUP bb DEFAULT 'X', "PENDENTES
              con   RADIOBUTTON GROUP bb,             "CONCLUIDO
              todas RADIOBUTTON GROUP bb.             "TODAS
SELECTION-SCREEN END OF BLOCK b2.



INITIALIZATION.

START-OF-SELECTION.
  IF p_budat-low < '20170101'.
    MESSAGE 'Consulta permitida a patir da data 01.01.2017.' TYPE 'I'.
    EXIT.
  ELSE.
    PERFORM seleciona_dados.
    PERFORM imprimir_alv.
  ENDIF.

END-OF-SELECTION.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.

FORM seleciona_dados.

  DATA: it_it001        TYPE it001,
        it_mara         TYPE expo_mara_t,
        it_ekko         TYPE me_ekko,
        it_reswk        TYPE cvp_tt_t001w,
        it_werks        TYPE cvp_tt_t001w,
        i_sit_carga2(1) TYPE c.

  IF ped = 'X'.
    i_sit_carga2 = '1'.
  ELSEIF con = 'X'.
    i_sit_carga2 = '2'.
  ELSEIF todas = 'X'.
    i_sit_carga2 = '3'.
  ENDIF.


  SELECT *  FROM t001 INTO TABLE it_it001
   WHERE bukrs IN p_bukrs.

  IF  p_matnr IS NOT INITIAL.
    SELECT * FROM mara INTO TABLE it_mara
    WHERE matnr IN p_matnr.
  ENDIF.

  IF p_ebeln IS NOT INITIAL.
    SELECT *  FROM ekko  INTO TABLE it_ekko
    WHERE ebeln IN p_ebeln.
  ENDIF.

  IF  p_reswk IS NOT INITIAL.
    SELECT *  FROM t001w  INTO TABLE it_reswk
    WHERE werks IN p_reswk.
  ENDIF.

  IF  p_werks IS NOT INITIAL.
    SELECT *  FROM t001w  INTO TABLE it_werks
    WHERE werks IN p_werks.
  ENDIF.


  CALL FUNCTION 'ZMM_OUT_ZMM0141'
    EXPORTING
      t_t001         = it_it001
      t_mara         = it_mara
      t_ekko         = it_ekko
      t_reswk        = it_reswk
      t_werks        = it_werks
      t_budat_l      = p_budat-low
      t_budat_h      = p_budat-high
      i_sit_carga2   = i_sit_carga2
    IMPORTING
      e_out_zmmm0141 = it_saida.


ENDFORM.

FORM imprimir_alv.

  PERFORM preenche_cat USING:

        'STATUS'        'Status'                           '06'     ''      ''  ''  '' 'X',
        'BUKRS'         'Empresa'                          '07'     ''      ''  ''  '' '',
        'BUTXT'         'Nome da Empresa'                  '25'     ''      ''  ''  '' '',
        'RESWK'         'Filial Origem'                    '10'     ''      ''  ''  '' '',
        'XCF'           'Nome Filial Origem'               '25'     ''      ''  ''  '' '',
        'WERKS'         'Filial Destino'                   '10'     ''      ''  ''  '' '',
        'XCR'           'Nome Filial Destino'              '25'     ''      ''  ''  '' '',
        'EBELN'         'Nº Pedido'                        '10'     ''      'X' ''  '' '',
        'VBELN_ST'      'Doc.Remessa'                      '11'     ''      'X' ''  '' '',
        'NR_ROMANEIO'   'Romaneio'                         '08'     ''      ''  ''  '' '',
        'PLACA_CAV'     'Placa'                            '10'     ''      ''  ''  '' '',
        'MATNR'         'Material'                         '08'     ''      ''  ''  '' '',
        'MAKTX'         'Descrição Material'               '40'     ''      ''  ''  '' '',
        'NFENUM'        'Nota Fiscal'                      '11'     ''      ''  ''  '' '',
        'XDTSAIDA'      'Data Saída'                       '10'     ''      ''  ''  '' '',
        'XDOCMATSD'     'Doc. Mat Saída'                   '14'     ''      'X' ''  '' '',
        'XQTSD'         'Peso Saída'                       '10'     ''      ''  ''  '' '',
        'XDTENTR'       'Data Chegada'                     '14'     ''      ''  ''  '' '',
        'XDOCMATET'     'Doc. Mat. Entrada'                '17'     ''      'X' ''  '' '',
        'XPESOCHEGADA'  'Peso Chegada'                     '14'     ''      ''  ''  '' '',
        'XDIAS'         'Dias em Transito'                 '16'     ''      ''  ''  '' ''.

  CALL SCREEN 0100.

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_icon).

  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.


  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.
  wa_fieldcatalog-icon        = p_icon.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: url(255)                TYPE c,
        data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i.


  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 18.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 60.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv.


    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        is_variant                    = gs_variant
        i_save                        = 'A'
        it_toolbar_excluding          = it_exclude_fcode
      CHANGING
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    IF ped = 'X'.
      p_text = TEXT-003.
    ELSEIF con = 'X'.
      p_text = TEXT-004.
    ELSE.
      p_text = TEXT-005.
    ENDIF.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.
    "Empresa
    IF p_bukrs IS NOT INITIAL.
      LOOP AT p_bukrs.
        IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
          EXIT.
        ELSEIF p_bukrs-option EQ 'BT'.

          CONCATENATE 'Empresa:' p_bukrs-low  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_bukrs-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Empresa:' p_bukrs-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    "------------------
    "Material
    IF p_matnr IS NOT  INITIAL.
      LOOP AT p_matnr.
        IF p_matnr-option NE 'EQ' AND p_matnr-option NE 'BT'.
          sdydo_text_element = 'Material: Multiplas Seleções'.
          EXIT.
        ELSEIF p_matnr-option EQ 'BT'.

          CONCATENATE 'Material:' p_matnr-low  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_matnr-high  INTO sdydo_text_element SEPARATED BY space.

          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Material: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Material:' p_matnr-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    "Pedido Transferência
    IF p_ebeln IS NOT  INITIAL.
      LOOP AT p_ebeln.
        IF p_ebeln-option NE 'EQ' AND p_ebeln-option NE 'BT'.
          sdydo_text_element = 'Pedido Transferência: Multiplas Seleções'.
          EXIT.
        ELSEIF p_matnr-option EQ 'BT'.

          CONCATENATE 'Pedido Transferência:' p_ebeln-low  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_ebeln-high  INTO sdydo_text_element SEPARATED BY space.

          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Pedido Transferência: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Pedido Transferência:' p_ebeln-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    "Centro fornecedor
    IF p_reswk IS NOT  INITIAL.
      LOOP AT p_reswk.
        IF p_reswk-option NE 'EQ' AND p_reswk-option NE 'BT'.
          sdydo_text_element = 'Centro Fornecedor: Multiplas Seleções'.
          EXIT.
        ELSEIF p_matnr-option EQ 'BT'.

          CONCATENATE 'Centro Fornecedor:' p_reswk-low  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_reswk-high  INTO sdydo_text_element SEPARATED BY space.

          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Centro Fornecedor: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro Fornecedor:' p_reswk-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    "Centro Receptor
    IF p_werks IS NOT  INITIAL.
      LOOP AT p_werks.
        IF p_werks-option NE 'EQ' AND p_werks-option NE 'BT'.
          sdydo_text_element = 'Centro Receptor: Multiplas Seleções'.
          EXIT.
        ELSEIF p_matnr-option EQ 'BT'.

          CONCATENATE 'Centro Receptor:' p_werks-low  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_werks-high  INTO sdydo_text_element SEPARATED BY space.

          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Centro Receptor: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro Receptor:' p_werks-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    " Data Movimentação
    IF p_budat IS NOT INITIAL.
      CONCATENATE  p_budat-low+6(2)  '.'  p_budat-low+4(2)  '.' p_budat-low(4)  INTO data_ini.
      CONCATENATE  p_budat-high+6(2) '.'  p_budat-high+4(2) '.' p_budat-high(4) INTO data_fim.

      CONCATENATE 'Data Movimentação:  ' data_ini  INTO  sdydo_text_element SEPARATED BY space.
      IF data_fim <> '00.00.0000' .
        CONCATENATE sdydo_text_element data_fim  INTO sdydo_text_element SEPARATED BY space.
      ENDIF.

      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element,  data_ini, data_fim.
    ENDIF.

    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.
  ENDIF.
ENDMODULE.

FORM fill_gs_variant.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.

FORM pega_logo USING nome_logo CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM user_command_0100 USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE lvc_s_col
                             p_es_eow_no TYPE lvc_s_roid.

  READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

  CASE p_e_column_id-fieldname.
    WHEN 'EBELN'.
      IF wa_saida-ebeln IS NOT INITIAL.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELN_ST'.
      IF wa_saida-vbeln_st IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD wa_saida-vbeln_st.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'XDOCMATSD'.
      IF wa_saida-xdocmatsd IS NOT INITIAL.

* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD WA_SAIDA-XDOCMATSD.
*        SET PARAMETER ID 'MJA' FIELD WA_SAIDA-XDTSAIDA+0(4).  "V_ANO.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida-xdocmatsd
            i_mjahr             = wa_saida-xdtsaida+0(4).
        "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG3 - DG
      ENDIF.
    WHEN 'XDOCMATET'.
      IF wa_saida-xdocmatet IS NOT INITIAL.

*---> 19.07.2023 19:15:48 - Migração S4 - DL
*        SET PARAMETER ID 'MBN' FIELD wa_saida-xdocmatet.
*        SET PARAMETER ID 'MJA' FIELD wa_saida-xdtsaida+0(4)."V_ANO.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action            = 'A04'
          i_refdoc            = 'R02'
          i_notree            = 'X'
          i_no_auth_check     = ' '
          i_deadend           = 'X'
          i_skip_first_screen = 'X'
          i_okcode            = 'OK_GO'
          i_mblnr             = wa_saida-xdocmatet
          i_mjahr             = wa_saida-xdtsaida+0(4).

*<--- 19.07.2023 19:15:48 - Migração S4 - DL
      ENDIF.
  ENDCASE.

ENDFORM.
