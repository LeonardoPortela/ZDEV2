*&---------------------------------------------------------------------*
*&  Include           ZAA11_0100
*&---------------------------------------------------------------------*

CONSTANTS: ok_anexo  TYPE sy-ucomm VALUE 'ANEXO',
           ok_salvar TYPE sy-ucomm VALUE 'SAVE',
           c_form    TYPE rs38l_fnam VALUE 'ZAAS11'.

DATA: it_select           TYPE STANDARD TABLE OF ty_dados_imob,
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      dg_splitter_1       TYPE REF TO cl_gui_splitter_container,
      dg_parent_1         TYPE REF TO cl_gui_container,
      dg_splitter_2       TYPE REF TO cl_gui_splitter_container,
      dg_parent_2         TYPE REF TO cl_gui_container,
      dg_parent_2a        TYPE REF TO cl_gui_container,
      dg_parent_alv       TYPE REF TO cl_gui_container,
      picture             TYPE REF TO cl_gui_picture,
      ctl_alv             TYPE REF TO cl_gui_alv_grid,
      dg_dyndoc_id        TYPE REF TO cl_dd_document,
      table_element       TYPE REF TO cl_dd_table_element,
      column              TYPE REF TO cl_dd_area,
      table_element2      TYPE REF TO cl_dd_table_element,
      column_1            TYPE REF TO cl_dd_area,
      column_2            TYPE REF TO cl_dd_area,
      dg_html_cntrl       TYPE REF TO cl_gui_html_viewer,
      it_exclude_fcode    TYPE ui_functions,
      wa_exclude_fcode    LIKE LINE OF it_exclude_fcode,
      ck_gravou           TYPE c LENGTH 1,
      ck_primeira_entrada TYPE c LENGTH 1,
      gs_layout           TYPE lvc_s_layo,
      gs_variant          TYPE disvariant,
      it_fieldcatalog     TYPE lvc_t_fcat,
      wa_fieldcatalog     TYPE lvc_s_fcat,
      gs_scroll_col       TYPE lvc_s_col,
      gs_scroll_row       TYPE lvc_s_roid,
      gs_stable           TYPE lvc_s_stbl,
      it_selected_rows    TYPE lvc_t_row,
      wa_selected_rows    TYPE lvc_s_row,
      ls_stable           TYPE lvc_s_stbl.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

*  CTL_ALV->CHECK_CHANGED_DATA( ).

  CASE sy-ucomm.
    WHEN 'APROVAR'.
      PERFORM aprovar.
    WHEN 'RETORNAR'.
      PERFORM retornar.
    WHEN 'ATUALIZAR'.
      PERFORM atualizar.
    WHEN 'TERMO'.
      PERFORM termo.
    WHEN 'SOBRA'.
      PERFORM sobra_fisica.
* Ini - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
    WHEN 'SALVAR'.
      PERFORM salvar.
* Fim - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
  ENDCASE.

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  DATA: tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab.
  DATA: wl_name1  TYPE thead-tdname.

  LOOP AT it_dados_imob INTO wa_dados_imob.

    REFRESH: it_texto, tl_texto.
    CLEAR: wl_name1, wl_texto.

*-CS2019000323 - 02.09.2021 - JT - inicio
*    CONCATENATE p_gjahr-low wa_dados_imob-anln1 wa_dados_imob-anln2
*                 wa_dados_imob-gsber wa_dados_imob-kostl INTO wl_name1.
*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        id        = 'ZAA1'
*        language  = sy-langu
*        name      = wl_name1
*        object    = 'ZAA02'
*      TABLES
*        lines     = it_texto
*      EXCEPTIONS
*        id        = 1
*        language  = 2
*        name      = 3
*        not_found = 4
*        OTHERS    = 5.
*-CS2019000323 - 02.09.2021 - JT - fim

    IF ( wa_dados_imob-zimob_v = '4' AND
*-CS2019000323 - 02.09.2021 - JT - inicio
*       it_texto IS NOT INITIAL AND
        wa_dados_imob-observ IS NOT INITIAL AND
*-CS2019000323 - 02.09.2021 - JT - inicio
        wa_dados_imob-semaf = '@08@' AND
        wa_dados_imob-aprov = '@DF@' )  OR
      ( wa_dados_imob-zimob_v = '4' AND
        it_texto IS INITIAL AND
        wa_dados_imob-semaf = '@08@' AND
        wa_dados_imob-aprov = '@DF@'  ) .
      CONTINUE.
    ELSEIF
      ( wa_dados_imob-zimob_v = '4' AND
*-CS2019000323 - 02.09.2021 - JT - inicio
*       it_texto IS INITIAL AND
        wa_dados_imob-observ IS INITIAL AND
*-CS2019000323 - 02.09.2021 - JT - fim
        wa_dados_imob-semaf <> '@08@' AND
        wa_dados_imob-aprov <> '@DF@'  ) .
      MESSAGE TEXT-015 TYPE 'I'.
*     CALL SCREEN 0100.
    ENDIF.
    CLEAR wa_dados_imob.
  ENDLOOP.
  CLEAR ok_code.
  LEAVE TO SCREEN 0.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_gtext                TYPE tgsbt-gtext,
        vl_butxt                TYPE t001-butxt.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF g_custom_container IS INITIAL.

* create a container for the tree control
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

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
        height = 15.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 80.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_it_fieldcatalog USING:
          01 'SEMAF'                ''        '07'  ' '     'X'     ' '   'C'   ' '   'Valid.                                               '   ' ',
          02 'APROV'                ''        '07'  ' '     'X'     ' '   'C'   ' '   'Aprov.                                               '   ' ',
          03 'ANLN1'                'ANLZ'    '12'  ' '     ' '     ' '   ' '   ' '   'Imobilizado                                          '   ' ',
          04 'ANLN2'                'ANLZ'    '06'  ' '     ' '     ' '   ' '   ' '   'Sub Nº                                               '   ' ',
          05 'BUKRS'                'ANLZ'    '07'  ' '     ' '     ' '   ' '   ' '   'Empresa                                              '   ' ',
          06 'GSBER'                'ANLZ'    '06'  ' '     ' '     ' '   ' '   ' '   'Filial                                               '   ' ',
          07 'KOSTL'                'ANLZ'    '12'  ' '     ' '     ' '   ' '   ' '   'C. Custo                                             '   ' ',
          08 'CSKT_LTEXT'           'CSKT'    '35'  ' '     ' '     ' '   ' '   ' '   'Descrição C.Custo                                    '   ' ',  "*-CS2022000978-31.01.2023-#93774-JT
          09 'AKTIV'                'ANLA'    '12'  ' '     ' '     ' '   ' '   ' '   'Dt. Incorporação                                     '   ' ',
          10 'TXT50'                'ANLA'    '50'  ' '     ' '     ' '   ' '   ' '   'Denominação do Imobilizado                           '   ' ',
          11 'TXA50'                'ANLA'    '50'  ' '     ' '     ' '   ' '   ' '   'Denominação do Imobilizado (2)                       '   ' ',
          12 'INVNR'                'ANLA'    '25'  ' '     ' '     ' '   ' '   ' '   'Nº Chassi                                            '   ' ',
          13 'SERNR'                'ANLA'    '18'  ' '     ' '     ' '   ' '   ' '   'Nº Série                                             '   ' ',
          14 'STORT'                'ANLZ'    '12'  ' '     ' '     ' '   ' '   ' '   'Localização                                          '   ' ',
          15 'RAUMN'                'ANLZ'    '08'  ' '     ' '     ' '   ' '   ' '   'Sala                                                 '   ' ',
          16 'KFZKZ'                'ANLZ'    '15'  ' '     ' '     ' '   ' '   ' '   'Placa                                                '   ' ',
          17 'ZIMOB_V'              ''        '07'  ' '     ' '     ' '   ' '   ' '   'Status                                               '   ' ',
          18 'ZIMOB_V2'              ''        '07'  ' '     ' '     ' '   ' '   ' '   'Status Ano Anterior                                  '   ' ',
          19 'ANEXO '               ''        '07'   ' '    'X'     ' '   ' '   ' '   'Anexo                                                '   ' ',

*-CS2019000323 - 02.09.2021 - JT - inicio                                                                                         '   ' ',
*         18 'TEXTO'                ''        '07'   ' '    'X'     ' '   ' '   ' '   'Obs.                                                 '   ' ',
          20 'OBSERV'               'ZAA005'  '50'   'X'    ' '     ' '   ' '   ' '   'Observação                                           '   ' ',
*-CS2019000323 - 02.09.2021 - JT - fim                                                                                            '   ' ',
          21 'ZIMOB_P'              ''        '07'   ' '    ' '     ' '   ' '    ' '   'Plaqueta                                            '   ' ',
          "18 'ZIMOB_A'             'ZAA005'  '10'   ' '    ' '     'X'   'C'   ' '   'Nível Aprov.                                        '   ' ',
          "19 'ZIMOB_A2'            ''        '10'   ' '    ' '     'X'   'C'   ' '   'Próx. Nível                                         '   ' ',
          22 'USUAR_AA'             'ZAA004'  '14'   ' '    ' '      'X'   'C'   ' '   'Responsável                                         '   ' ',
          23 'ANLKL'                'ANLA'    '14'   ' '    ' '      ' '   ' '   ' '   'Classe do imobilizado                               '   ' ',
          24 'DESC_CLASSE'          'ANLA'    '50'   ' '    ' '      ' '   ' '   ' '   'Desc. Classe do imob                               '   ' ',   " Rubenilson - 15.01.24 - US128402
          25 'CONTA'                'T095'    '15'   ' '    ' '      ' '   ' '   ' '   'Conta                                               '   ' ',               "*-CS2022000978-31.01.2023-#93774-JT
          26 'DESC_CONTA'           'SKAT'    '40'   ' '    ' '      ' '   ' '   ' '   'Descrição da Conta                                  '   ' ',  "*-CS2022000978-31.01.2023-#93774-JT
          27 'IMG'                  ''        '42'   ' '    'X'     ' '   ' '    ' '   'Imagem                                              '   'X',  "110128 CS2023000293 Imp. dados invent. /  ZAA16 - PSA
          28 'INVENTARIANTE'        ''        '42'   ' '    'X'     ' '   ' '    ' '   'Inventariante                                       '   ' ',  "110128 CS2023000293 Imp. dados invent. /  ZAA16 - PSA
          29 'EQUNR'                ''        '20'   ' '    ''      ' '   ' '    ' '   'Equipamento                                         '   ' ',  "User Story 153788 // MMSILVA - 07.10.2024
          30 'EQKTX'                ''        '40'   ' '    ''      ' '   ' '    ' '   'Descrição Equipamento                               '   ' ',  "User Story 153788 // MMSILVA - 07.10.2024
          31 'KOSTL_ILOA'           ''        '14'   ' '    ''      ' '   ' '    ' '   'Centro de Custo Equip.                              '   ' ',  "User Story 153788 // MMSILVA - 07.10.2024
          32 'SWERK'                ''        '07'   ' '    ''      ' '   ' '    ' '   'Filial Equipamento                                  '   ' ',  "User Story 153788 // MMSILVA - 07.10.2024
          33 'VLR_AQUIS_BRL'        ''        '14'   ' '    ''      ' '   ' '    ' '   'Vlr. Aquisição BRL                                  '   ' ',  " Rubenilson - 15.01.24 - US128402
          34 'VLR_RESID_BRL'        ''        '14'   ' '    ''      ' '   ' '    ' '   'Vlr. Residual BRL                                  '   ' ',   " Rubenilson - 15.01.24 - US128402
          35 'VLR_AQUIS_USD'        ''        '14'   ' '    ''      ' '   ' '    ' '   'Vlr. Aquisição USD                                  '   ' ',  " Rubenilson - 15.01.24 - US128402
          36 'VLR_RESID_USD'        ''        '14'   ' '    ''      ' '   ' '    ' '   'Vlr. Residual USD                                  '   ' '.   " Rubenilson - 15.01.24 - US128402



*   Fill info for layout variant
    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    PERFORM exclude.

    PERFORM set_status_dropdw.

    SET HANDLER:
      lcl_eventhandler=>handle_button_click FOR ctl_alv,
      lcl_eventhandler=>on_hotspot_click FOR ctl_alv.
*      lcl_eventhandler=>on_data_changed FOR ctl_alv. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

    ctl_alv->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_dados_imob.

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

    p_text = TEXT-003.

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
    "------cabeçalho
    LOOP AT p_bukrs.
      IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
        sdydo_text_element = 'Empresa: Multiplas Seleções'.
        EXIT.
      ELSEIF p_bukrs-option EQ 'BT'.

        SELECT SINGLE butxt
          FROM t001
          INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.

        CONCATENATE 'Empresa:' p_bukrs-low vl_butxt '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: vl_butxt.

        SELECT SINGLE butxt
          FROM t001
          INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.

        CONCATENATE sdydo_text_element p_bukrs-high vl_butxt INTO sdydo_text_element SEPARATED BY space.

        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
        ELSE.

          SELECT SINGLE butxt
          FROM t001
          INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.

          CONCATENATE 'Empresa:' p_bukrs-low vl_butxt INTO sdydo_text_element SEPARATED BY space.

        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, vl_butxt, sdydo_text_element.
*    SDYDO_TEXT_ELEMENT = 'Filial: '.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    "------------------
    LOOP AT p_gsber.
      IF p_gsber-option NE 'EQ' AND p_gsber-option NE 'BT'.
        sdydo_text_element = 'Filial: Multiplas Seleções'.
        EXIT.
      ELSEIF p_gsber-option EQ 'BT'.

        SELECT SINGLE gtext
          FROM tgsbt
          INTO vl_gtext
          WHERE gsber EQ p_gsber-low
          AND spras EQ sy-langu.

        CONCATENATE 'Filial:' p_gsber-low vl_gtext '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: vl_gtext.

        SELECT SINGLE gtext
         FROM tgsbt
         INTO vl_gtext
         WHERE gsber EQ p_gsber-high
         AND spras EQ sy-langu.

        CONCATENATE sdydo_text_element p_gsber-high vl_gtext INTO sdydo_text_element SEPARATED BY space.

        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
        ELSE.

          SELECT SINGLE gtext
            FROM tgsbt
            INTO vl_gtext
            WHERE gsber EQ p_gsber-low
            AND spras EQ sy-langu.

          CONCATENATE 'Filial:' p_gsber-low vl_gtext INTO sdydo_text_element SEPARATED BY space.

        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, vl_gtext, sdydo_text_element.
    "------------------
*    SDYDO_TEXT_ELEMENT = 'Centro de Custo: '.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    "------------------
    IF p_kostl IS NOT INITIAL.
      LOOP AT p_kostl.
        IF p_kostl-option NE 'EQ' AND p_kostl-option NE 'BT'.
          sdydo_text_element = 'Centro de Custo: Multiplas Seleções'.
          EXIT.
        ELSEIF p_kostl-option EQ 'BT'.
          CONCATENATE 'Centro de Custo:' p_kostl-low '-' p_kostl-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Centro de Custo: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro de Custo:' p_kostl-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ELSE.
      sdydo_text_element = 'Centro de Custo:'.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, vl_gtext, sdydo_text_element.
    ENDIF.
    "------------------
    CONCATENATE 'Ano do Inventário:' p_gjahr-low INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: sdydo_text_element.
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
  ELSE.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

*  CALL METHOD CTL_ALV->GET_SCROLL_INFO_VIA_ID
*    IMPORTING
*      ES_COL_INFO = GS_SCROLL_COL
*      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

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

ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_lzero)
                                VALUE(p_just)
                                VALUE(p_checkbox)
                                VALUE(p_header)
                                VALUE(p_hotspot)
                                .

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-outputlen  = p_len.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-checktable = p_tabname.
  wa_fieldcatalog-lzero      = p_lzero.
  wa_fieldcatalog-just       = p_just.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-lowercase  = abap_true.
  wa_fieldcatalog-hotspot    = p_hotspot.

  IF p_fieldname = 'SEMAF' OR
     p_fieldname = 'APROV' OR
     p_fieldname = 'ANLN1' OR
     p_fieldname = 'ANLN2'.
    wa_fieldcatalog-fix_column = abap_true.
  ENDIF.

  IF p_fieldname = 'OBSERV'.
    wa_fieldcatalog-intlen = 255.
  ENDIF.

  IF p_fieldname <> 'OBSERV'.
    wa_fieldcatalog-col_opt  = 'X'.
  ENDIF.

  IF p_fieldname = 'ZIMOB_P'.
    wa_fieldcatalog-drdn_hndl = '3'.
    wa_fieldcatalog-edit = 'X'.
    "WA_FIELDCATALOG-OUTPUTLEN = 7.
    "WA_FIELDCATALOG-CHECKTABLE = '!'.
  ELSEIF p_fieldname = 'ZIMOB_V' OR p_fieldname = 'ZIMOB_V2'.
    wa_fieldcatalog-drdn_hndl = '2'.
*   wa_fieldcatalog-drdn_alias = abap_true.
    wa_fieldcatalog-edit = 'X'.
    "WA_FIELDCATALOG-OUTPUTLEN = 15.
  ENDIF.


  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_select.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_dados_imob INTO wa_dados_imob INDEX wa_selected_rows-index.
    IF sy-subrc IS INITIAL.
      APPEND wa_dados_imob TO it_select.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE
*&---------------------------------------------------------------------*
FORM exclude .

*   Excluir Buttons Toolbar
  FREE: it_exclude_fcode.

  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wa_exclude_fcode TO it_exclude_fcode.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  APROVAR
*&---------------------------------------------------------------------*
FORM aprovar .

  DATA: vl_check  TYPE char1,           "Check de verificação de inconformidade para aprovação
        wa_select TYPE ty_dados_imob.

  DATA: vl_obj_key TYPE sibflporb-instid,
        vl_lines   TYPE i,
        anexos     TYPE TABLE OF bdn_con.

  LOOP AT it_select INTO wa_select.

*-CS2019000323 - 02.09.2021 - JT - inicio
    IF vg_auth_full = abap_true.
      sy-subrc = 0.
    ELSE.
      READ TABLE it_zaa004 INTO wa_zaa004 WITH KEY bukrs    = wa_select-bukrs
                                                   kostl    = wa_select-kostl
                                                   gsber    = wa_select-gsber
                                                   gjahr    = p_gjahr-low
                                                   nivel_aa = wa_select-zimob_a + 1
                                                   usuar_aa = sy-uname.
    ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

    IF sy-subrc IS INITIAL.
      "Verificação apenas para Imob. c/ Status 2 e 3 nos para aprovadores nos níveis 1 e 2
      IF ( wa_select-zimob_v EQ 2 OR wa_select-zimob_v EQ 3 ) AND
         ( wa_select-zimob_a EQ 0 OR wa_select-zimob_a EQ 1 ).
        "Se status 2 ou 3 e não tiver texto associado marca para não aprovar
*-CS2019000323 - 02.09.2021 - JT - inicio
*       IF wa_select-texto EQ '@6Y@'.
        IF wa_select-observ IS INITIAL.
*-CS2019000323 - 02.09.2021 - JT - inicio
*          MOVE abap_true TO vl_check. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
        ENDIF.

        "Busca anexos
        CONCATENATE p_gjahr-low wa_select-anln1 wa_select-anln2
                    wa_select-gsber wa_select-kostl INTO vl_obj_key.

        CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
          EXPORTING
            classname          = 'ZAA11'
            objkey             = vl_obj_key
            client             = sy-mandt
          TABLES
            gos_connections    = anexos
          EXCEPTIONS
            no_objects_found   = 1
            internal_error     = 2
            internal_gos_error = 3
            OTHERS             = 4.

        DESCRIBE TABLE anexos LINES vl_lines.

        "Se status 3 e sem nenhum anexo marca para não aprovar
*        IF vl_lines IS INITIAL AND wa_select-zimob_v EQ 3.
*          MOVE abap_true TO vl_check. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
*        ENDIF.

      ELSEIF wa_select-zimob_v EQ 0.
        MOVE abap_true TO vl_check.
        MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      IF vl_check IS INITIAL.
        wa_zaa005-anln1    = wa_select-anln1.
        wa_zaa005-anln2    = wa_select-anln2.
        wa_zaa005-bukrs    = wa_select-bukrs.
        wa_zaa005-gsber    = wa_select-gsber.
        wa_zaa005-kostl    = wa_select-kostl.
        wa_zaa005-gjahr    = p_gjahr-low.
        wa_zaa005-zimob_v  = wa_select-zimob_v.
        wa_zaa005-zimob_a  = wa_select-zimob_a + 1.
        wa_zaa005-zimob_p  = wa_select-zimob_p.
        wa_zaa005-zimob_r  = abap_false.
*-CS2019000323 - 02.09.2021 - JT - inicio
        wa_zaa005-observ   = wa_select-observ.
        wa_zaa005-aktiv    = wa_select-aktiv.
        wa_zaa005-txt50    = wa_select-txt50.
        wa_zaa005-txa50    = wa_select-txa50.
        wa_zaa005-invnr    = wa_select-invnr.
        wa_zaa005-sernr    = wa_select-sernr.
        wa_zaa005-stort    = wa_select-stort.
        wa_zaa005-raumn    = wa_select-raumn.
        wa_zaa005-dt_proc  = sy-datum.
        wa_zaa005-hr_proc  = sy-uzeit.
        wa_zaa005-us_proc  = sy-uname.
        wa_zaa005-cc_sobra = wa_select-cc_sobra.
        wa_zaa005-manual   = wa_select-manual.
        wa_zaa005-img = wa_select-img.
        wa_zaa005-inventariante   = wa_select-inventariante.
*-CS2019000323 - 02.09.2021 - JT - fim
        IF wa_zaa005-zimob_a EQ 4.
          wa_zaa005-zusap_4 = sy-uname.
          wa_zaa005-zdtap_4 = sy-datum.
          wa_zaa005-zhrap_4 = sy-uzeit.
        ENDIF.

        "MODIFY zaa005 FROM wa_zaa005.
        UPDATE zaa005 FROM wa_zaa005.

*-CS2019000323 - 02.09.2021 - JT - inicio
*---------------------
* ----- Envia email
*---------------------
        "PERFORM f_envia_email USING wa_zaa005.   78884
*-CS2019000323 - 02.09.2021 - JT - fim
      ENDIF.
    ENDIF.
    CLEAR: vl_check, wa_zaa004, wa_zaa005.
  ENDLOOP.

  PERFORM seleciona_dados.
  PERFORM manipula_dados.
  PERFORM filtra_resultado. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

ENDFORM.

* Ini - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
FORM salvar.

  DATA: wa_select  TYPE ty_dados_imob,
        lv_cond    TYPE c,
        lv_bloq    TYPE c,
        lv_message TYPE string.

  CLEAR: lv_cond, lv_bloq, lv_message.

  IF vg_auth_full = abap_true.
    sy-subrc = 0.
  ELSE.
    READ TABLE it_zaa004 INTO wa_zaa004 WITH KEY bukrs    = wa_select-bukrs
                                                 kostl    = wa_select-kostl
                                                 gsber    = wa_select-gsber
                                                 gjahr    = p_gjahr-low
                                                 nivel_aa = wa_select-zimob_a + 1
                                                 usuar_aa = sy-uname.
  ENDIF.

  IF it_select[] IS NOT INITIAL.
    LOOP AT it_select INTO wa_select.

      READ TABLE it_zaa005 INTO wa_zaa005 WITH KEY bukrs    = wa_select-bukrs   kostl   = wa_select-kostl   gsber  = wa_select-gsber
                                                   gjahr    = p_gjahr-low       anln1   = wa_select-anln1   anln2  = wa_select-anln2
                                                   zimob_v  = wa_select-zimob_v zimob_p = wa_select-zimob_p observ = wa_select-observ.

      IF sy-subrc IS INITIAL.
        "Não alterado
        CONTINUE.
      ELSE.
        lv_cond = abap_true.
        CLEAR wa_zaa005.

        "Bloqueia salvar caso Imob não esteja no nível 0 ou 1 de aprovação
        IF NOT ( ( wa_select-zimob_a EQ 0 OR wa_select-zimob_a EQ 1 ) AND ( wa_select-usuar_aa EQ sy-uname ) ).
          lv_bloq = abap_true.
          CONTINUE.
        ENDIF.

        "Alterado
        wa_zaa005 = VALUE #(
          anln1    = wa_select-anln1
          anln2    = wa_select-anln2
          bukrs    = wa_select-bukrs
          gsber    = wa_select-gsber
          kostl    = wa_select-kostl
          gjahr    = p_gjahr-low
          zimob_v  = wa_select-zimob_v
          zimob_a  = wa_select-zimob_a
          zimob_p  = wa_select-zimob_p
          zimob_r  = abap_false
          observ   = wa_select-observ
          aktiv    = wa_select-aktiv
          txt50    = wa_select-txt50
          txa50    = wa_select-txa50
          invnr    = wa_select-invnr
          sernr    = wa_select-sernr
          stort    = wa_select-stort
          raumn    = wa_select-raumn
          dt_proc  = sy-datum
          hr_proc  = sy-uzeit
          us_proc  = sy-uname
          cc_sobra = wa_select-cc_sobra
          manual   = wa_select-manual
        ).

        MODIFY zaa005 FROM wa_zaa005.
        CLEAR wa_zaa005.
      ENDIF.
    ENDLOOP.
  ELSE.
    DATA(lv_empty) = abap_true.
  ENDIF.
  lv_message = COND #( WHEN lv_cond IS NOT INITIAL AND lv_bloq IS INITIAL THEN TEXT-111
                       WHEN lv_cond IS NOT INITIAL AND lv_bloq IS NOT INITIAL THEN TEXT-114
                       WHEN lv_empty EQ abap_true THEN TEXT-113
                       ELSE TEXT-112 ).
  MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'I'.
ENDFORM.
* Fim - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

*&---------------------------------------------------------------------*
*&      Form  envia email
*&---------------------------------------------------------------------*
FORM f_envia_email  USING p_zaa005 TYPE zaa005.

  DATA: it_destinatario TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE,
        it_assunto      TYPE sodocchgi1,
        it_texto        TYPE STANDARD TABLE OF soli WITH HEADER LINE.

  FREE: it_destinatario,
        it_texto,
        it_assunto.

  CHECK p_zaa005-zimob_a > 1.

  SELECT *
    FROM zaa004
    INTO TABLE @DATA(it_zaa004)
   WHERE bukrs = @p_zaa005-bukrs
     AND gsber = @p_zaa005-gsber
     AND kostl = @p_zaa005-kostl
     AND gjahr = @p_zaa005-gjahr.

  DELETE it_zaa004 WHERE nivel_aa < p_zaa005-zimob_a.
  SORT it_zaa004 BY nivel_aa.

  READ TABLE it_zaa004 INTO wa_zaa004 INDEX 1.
  CHECK sy-subrc = 0.

  SELECT bname, persnumber
    INTO @DATA(w_usr21)
      UP TO 1 ROWS
    FROM usr21
   WHERE bname = @wa_zaa004-usuar_aa.
  ENDSELECT.

  CHECK sy-subrc = 0.

  SELECT smtp_addr
    INTO @DATA(l_smtp_addr)
      UP TO 1 ROWS
    FROM adr6
   WHERE persnumber = @w_usr21-persnumber.
  ENDSELECT.

  CHECK sy-subrc = 0.

  it_destinatario-rec_type = 'U'.
  it_destinatario-receiver = l_smtp_addr.
  APPEND it_destinatario.

  it_assunto-obj_name  = 'Aprovação Inventario de Imobilizado'.
  it_assunto-obj_langu = sy-langu.
  CONCATENATE 'Aprovação Inventario Imobilizado - Exercício'  p_zaa005-gjahr
         INTO it_assunto-obj_descr SEPARATED BY space.

  it_texto = '<!DOCTYPE html>'.
  APPEND it_texto.
  it_texto = '<html>'.
  APPEND it_texto.
  it_texto = '<head>'.
  APPEND it_texto.
  it_texto = '<style type="text/css">'.
  APPEND it_texto.

  "Tabela do E-mail
  it_texto = '#tabela table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
  APPEND it_texto.
  it_texto = '#tabela th { width: 90px; font-size: 12px; background-color: #93DB70; color: #000000; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
  APPEND it_texto.
  it_texto = '#tabela td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
  APPEND it_texto.
  it_texto = '#msg { font-size: 14px; font-style: "Tahoma, Geneva, sans-serif"; }'.
  APPEND it_texto.

  it_texto = '</style>'.
  APPEND it_texto.
  it_texto = '</head>'.

  APPEND it_texto.
  it_texto = '<body lang="pt-br">'.
  APPEND it_texto.

  it_texto = '<div id="msg">'.
  APPEND it_texto.

  it_texto = 'Comunicamos que está liberado, para sua validação e aprovação do Inventário de Imobilizado<br/>'.
  APPEND it_texto.
  it_texto = 'no SAP, através da transação ZAA16 do centro de custo abaixo:<br/>'.
  APPEND it_texto.

  it_texto = '</div>'.
  APPEND it_texto.

  it_texto = '<br/>'.
  APPEND it_texto.
  it_texto = '<br/>'.
  APPEND it_texto.

  it_texto = '<div id="tabela">'.
  APPEND it_texto.
*
  it_texto = '<table>'.
  APPEND it_texto.
*
  it_texto = '<tr>'.
  APPEND it_texto.
  it_texto = '<th>Empresa</th>'.
  APPEND it_texto.
  it_texto = '<th>Filial</th>'.
  APPEND it_texto.
  it_texto = '<th>Centro de Custo</th>'.
  APPEND it_texto.
  it_texto = '</tr>'.
  APPEND it_texto.
*
  it_texto = '<tr>'.
  APPEND it_texto.
  CONCATENATE '<td>' p_zaa005-bukrs '</td>' INTO it_texto.
  APPEND it_texto.
  CONCATENATE '<td>' p_zaa005-gsber '</td>' INTO it_texto.
  APPEND it_texto.
  CONCATENATE '<td>' p_zaa005-kostl '</td>' INTO it_texto.
  APPEND it_texto.
  it_texto = '</tr>'.
  APPEND it_texto.
*
  it_texto = '</table>'.
  APPEND it_texto.
*
  it_texto = '</div>'.
  APPEND it_texto.
*
  it_texto = '</body>'.
  APPEND it_texto.
*
  it_texto = '</html>'.
  APPEND it_texto.

*---------------------------------------------------
* Enviar E-mail
*---------------------------------------------------
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = it_assunto
      document_type              = 'HTM'
    TABLES
      object_content             = it_texto
      receivers                  = it_destinatario
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RETORNAR
*&---------------------------------------------------------------------*
FORM retornar .

  DATA: wa_select TYPE ty_dados_imob.

  LOOP AT it_select INTO wa_select
    WHERE zimob_a NE 0.

*-CS2019000323 - 02.09.2021 - JT - inicio
    IF vg_auth_full = abap_true.
      sy-subrc = 0.
    ELSE.
      READ TABLE it_zaa004 INTO wa_zaa004 WITH KEY bukrs    = wa_select-bukrs
                                                   kostl    = wa_select-kostl
                                                   gsber    = wa_select-gsber
                                                   gjahr    = p_gjahr-low
                                                   nivel_aa = wa_select-zimob_a + 1
                                                   usuar_aa = sy-uname.
    ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

    IF sy-subrc IS INITIAL.
      wa_zaa005-anln1   = wa_select-anln1.
      wa_zaa005-anln2   = wa_select-anln2.
      wa_zaa005-bukrs   = wa_select-bukrs.
      wa_zaa005-gsber   = wa_select-gsber.
      wa_zaa005-kostl   = wa_select-kostl.
      wa_zaa005-gjahr   = p_gjahr-low.
      wa_zaa005-zimob_v = wa_select-zimob_v.
      wa_zaa005-zimob_a = wa_select-zimob_a - 1.
      wa_zaa005-zimob_p = wa_select-zimob_p.
      wa_zaa005-zimob_r = abap_true.
* Ini - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
      wa_zaa005-observ   = wa_select-observ.
      wa_zaa005-aktiv    = wa_select-aktiv.
      wa_zaa005-txt50    = wa_select-txt50.
      wa_zaa005-txa50    = wa_select-txa50.
      wa_zaa005-invnr    = wa_select-invnr.
      wa_zaa005-sernr    = wa_select-sernr.
      wa_zaa005-stort    = wa_select-stort.
      wa_zaa005-raumn    = wa_select-raumn.
      wa_zaa005-dt_proc  = sy-datum.
      wa_zaa005-hr_proc  = sy-uzeit.
      wa_zaa005-us_proc  = sy-uname.
      wa_zaa005-cc_sobra = wa_select-cc_sobra.
      wa_zaa005-manual   = wa_select-manual.
* Fim - 3000006392/IR173266 - Stefanini - PRB - Correções ZAA16
      MODIFY zaa005 FROM wa_zaa005.
    ENDIF.

    CLEAR: wa_zaa004, wa_zaa005.
  ENDLOOP.

  PERFORM seleciona_dados.
  PERFORM manipula_dados.
  PERFORM filtra_resultado. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR
*&---------------------------------------------------------------------*
FORM atualizar .

  PERFORM seleciona_dados.
  PERFORM manipula_dados.
  PERFORM filtra_resultado. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TERMO
*&---------------------------------------------------------------------*
FORM termo .

  DATA: vl_imp   TYPE char1,
        vl_total TYPE i,
        vl_lines TYPE i.

*-CS2019000323 - 02.09.2021 - JT - inicio
  IF it_select[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-110 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

  LOOP AT it_dados_imob INTO wa_dados_imob.
    IF wa_dados_imob-aprov = icon_defect.
      vl_imp = abap_true.
      EXIT.
    ELSE.
      IF wa_dados_imob-zimob_v EQ 1 OR  wa_dados_imob-zimob_v EQ 2.
        vl_total = vl_total + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.


  CLEAR: wa_dados_imob.

  "Não deixa imprimir se existir Imobilizado não finalizado
  IF vl_imp EQ abap_true.
    CLEAR: vl_imp.
    MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  "Não deixa imprimir se a pesquisa foi por mais de uma filial ou centro de custo
  "Isto não pode acontecer pois o Smart não é produzido para mais de uma filial e CC
  LOOP AT p_gsber.
    IF p_gsber-option NE 'EQ'.
      vl_imp = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE p_gsber LINES vl_lines.

  IF vl_lines NE 1.
    vl_imp = abap_true.
  ENDIF.

  CLEAR: vl_lines.

  LOOP AT p_kostl.
    IF p_kostl-option NE 'EQ'.
      vl_imp = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE p_kostl LINES vl_lines.

  IF vl_lines GT 1.
    vl_imp = abap_true.
  ENDIF.

  IF vl_imp EQ abap_true.
    CLEAR: vl_imp.
    MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  PERFORM dados_smart USING vl_total.
  PERFORM chama_smart.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DADOS_SMART
*&---------------------------------------------------------------------*
FORM dados_smart USING vl_total TYPE i.

  DATA: wa_adrc TYPE adrc,
        vl_adnr TYPE j_1bbranch-adrnr.

  FREE: it_smart_form,
        it_smart_quadro,
        it_smart_exec,
        it_dd07v.

  CLEAR: wa_smart_form.

* READ TABLE IT_SELECT INTO DATA(WA_SELECT) INDEX 1.

*-CS2019000323 - 02.09.2021 - JT - inicio
* LOOP AT it_select INTO DATA(wa_select).
  READ TABLE it_select INTO DATA(wa_select) INDEX 1.
  CHECK sy-subrc = 0.

  READ TABLE it_dados_imob INTO DATA(wa_dados_imob) INDEX 1.
  CHECK sy-subrc = 0.
*-CS2019000323 - 02.09.2021 - JT - fim

  wa_smart_form-data = sy-datum+6(2) && '/' && sy-datum+4(2) && '/' && sy-datum(4).
  wa_smart_form-dia = sy-datum+6(2).
  "WA_SMART_FORM-MES = SY-DATUM+4(2).
  wa_smart_form-ano = sy-datum(4).
  wa_smart_form-qntde = 1."vl_total. "171313 CS2023000897 Erro impressão de form. PSA
  wa_smart_form-gsber = p_gsber-low.
  "WA_SMART_FORM-KOSTL = P_KOSTL-LOW.
  wa_smart_form-kostl = wa_dados_imob-kostl. "wa_select-kostl.

  SELECT SINGLE ktext
    FROM cskt
    INTO wa_smart_form-ktext
    WHERE kostl EQ p_kostl-low
    AND spras EQ sy-langu
    AND datbi EQ '99991231'.

  SELECT SINGLE gtext
    FROM tgsbt
    INTO wa_smart_form-gtext
    WHERE gsber EQ p_gsber-low
    AND spras EQ sy-langu.

  SELECT SINGLE ltx
    FROM t247
    INTO wa_smart_form-mes
    WHERE spras EQ sy-langu
      AND   mnr EQ sy-datum+4(2).

  SELECT SINGLE adrnr
    FROM j_1bbranch
    INTO vl_adnr
    WHERE branch EQ p_gsber-low.

  SELECT SINGLE *
      FROM adrc
      INTO wa_adrc
      WHERE addrnumber EQ vl_adnr.

  wa_smart_form-cidade = wa_adrc-city1.
  wa_smart_form-estado = wa_adrc-region.

  APPEND wa_smart_form TO it_smart_form[].
* CLEAR: wa_smart_form.

*-CS2019000323 - 02.09.2021 - JT - inicio
*------------------------------------------------
* USUARIO EXECUTOR
*------------------------------------------------
  SELECT *
    FROM zaa004
    INTO TABLE @DATA(it_zaa004)
     FOR ALL ENTRIES IN @it_dados_imob
   WHERE bukrs        = @it_dados_imob-bukrs
     AND gsber        = @it_dados_imob-gsber
     AND kostl        = @it_dados_imob-kostl
     AND gjahr        = @p_gjahr-low
     AND fluxo_baixa <> @abap_true.

  IF it_zaa004[] IS NOT INITIAL.
    SELECT *
      FROM user_addr
      INTO TABLE @DATA(it_user)
       FOR ALL ENTRIES IN @it_zaa004
     WHERE bname = @it_zaa004-usuar_aa.
  ENDIF.

  SORT it_user BY bname.
  DELETE ADJACENT DUPLICATES FROM it_user
                        COMPARING bname.

  LOOP AT it_zaa004 INTO DATA(wa_zaa004).
    CLEAR wa_smart_exec.

    READ TABLE it_user INTO DATA(wa_user) WITH KEY bname = wa_zaa004-usuar_aa
                       BINARY SEARCH.
    IF sy-subrc = 0.
      wa_smart_exec-nome_colab = wa_user-name_textc.
    ENDIF.

    wa_smart_exec-nivel_aa = wa_zaa004-nivel_aa.
    wa_smart_exec-usuar_aa = wa_zaa004-usuar_aa.
    APPEND wa_smart_exec  TO it_smart_exec.
  ENDLOOP.

*------------------------------------------------
* STATUS
*------------------------------------------------
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'ZIMOB_V'
      text      = 'X'
      langu     = sy-langu
    TABLES
      dd07v_tab = it_dd07v.

  LOOP AT it_dados_imob     INTO DATA(wa_imob).
    CLEAR: wa_smart_quadro,
           wa_dd07v.

    READ TABLE it_dd07v INTO wa_dd07v  WITH KEY domvalue_l = wa_imob-zimob_v.

    SELECT SINGLE ktext
      FROM cskt
      INTO wa_smart_quadro-kostl_text
     WHERE kostl  = wa_imob-kostl
       AND spras  = sy-langu
       AND datbi >= sy-datum.

    SELECT SINGLE gtext
      FROM tgsbt
      INTO wa_smart_quadro-gsber_text
     WHERE gsber = wa_imob-gsber
       AND spras = sy-langu.

    wa_smart_quadro-gsber      = wa_imob-gsber.
    wa_smart_quadro-data       = wa_smart_form-data.
    wa_smart_quadro-kostl      = wa_imob-kostl.
    wa_smart_quadro-zimob_v    = wa_imob-zimob_v && '-' && wa_dd07v-ddtext.
    wa_smart_quadro-qntde      = 1.
    COLLECT wa_smart_quadro INTO it_smart_quadro.
  ENDLOOP.
* ENDLOOP.

  LOOP AT it_select INTO wa_select.
    CHECK sy-tabix > 1.
    READ TABLE it_smart_form INTO wa_smart_form INDEX 1.
    APPEND wa_smart_form       TO it_smart_form.
  ENDLOOP.
  IF sy-subrc <> 0.
    DELETE it_smart_form INDEX 1.
  ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CHAMA_SMART
*&---------------------------------------------------------------------*
FORM chama_smart .

  DATA: vl_fm_name           TYPE rs38l_fnam,
        w_control_parameters TYPE ssfctrlop.

  DATA(_lines) = lines( it_smart_form[] ).

  LOOP AT  it_smart_form[] INTO wa_smart_form.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = c_form
*       VARIANT            = ' '
*       DIRECT_CALL        = ' '
      IMPORTING
        fm_name            = vl_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF ( sy-tabix = 1 AND _lines >= 2 ).
      w_control_parameters-no_open    = abap_false.
      w_control_parameters-no_close   = abap_true.
    ENDIF.

    IF ( sy-tabix > 1 AND _lines >= 2 ).
      w_control_parameters-no_open    = abap_true.
      w_control_parameters-no_close   = abap_true.
    ENDIF.

    IF ( sy-tabix = _lines ).
      w_control_parameters-no_open    = abap_true.
      w_control_parameters-no_close   = abap_false.
    ENDIF.

    IF ( _lines = 1 ).
      w_control_parameters-no_open    = abap_false.
      w_control_parameters-no_close   = abap_false.
    ENDIF.

    SORT it_smart_exec BY NIVEL_AA ASCENDING. "171313 CS2023000897 Erro impressão de form. PSA

    CALL FUNCTION vl_fm_name
      EXPORTING
        control_parameters = w_control_parameters
        wa_header          = wa_smart_form
        it_smart_quadro    = it_smart_quadro
        it_smart_exec      = it_smart_exec
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM set_status_dropdw.

*-CS2019000323 - 02.09.2021 - JT - inicio
  DATA(t_status_menu) = VALUE lvc_t_drop(
*     ( handle = '2'  value  = '0' )    "int_value = 'Não Validado' )
      ( handle = '2'  value  = '1' )    "int_value = 'Encontrado' )
      ( handle = '2'  value  = '2' )    "int_value = 'Não Encontrado' )
      ( handle = '2'  value  = '3' )    "int_value = 'Baixa')
      ( handle = '2'  value  = '4' )    "int_value = 'Sobra Física')
      ( handle = '3'  value  = 'S' )    "int_value = 'Sim' )
      ( handle = '3'  value  = 'N' ) ). "int_value = 'Não' ) ).

  ctl_alv->set_drop_down_table( EXPORTING it_drop_down = t_status_menu[] ).
*-CS2019000323 - 02.09.2021 - JT - fim

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SOBRA_FISICA
*&---------------------------------------------------------------------*
FORM sobra_fisica .

  DATA: wa_select TYPE ty_dados_imob.

*-CS2019000323 - 02.09.2021 - JT - inicio
  FREE: it_imobil,
        wa_imobil.

* IF it_dados_imob[] IS INITIAL.
*   wa_imobil-bukrs      = p_bukrs-low.
*   APPEND wa_imobil    TO it_imobil.
* ELSE.
*   LOOP AT it_dados_imob INTO wa_dados_imob.
*     wa_imobil-bukrs    = wa_dados_imob-bukrs.
*     wa_imobil-anln1    = wa_dados_imob-anln1.
*     wa_imobil-anln2    = wa_dados_imob-anln2.
*     APPEND wa_imobil  TO it_imobil.
*   ENDLOOP.
* ENDIF.

* IF it_select[] IS INITIAL.
*   wa_imobil-bukrs      = p_bukrs-low.
*   APPEND wa_imobil    TO it_imobil.
* ELSE.
*   LOOP AT it_select INTO wa_select.
*     wa_imobil-bukrs    = wa_select-bukrs.
*     wa_imobil-anln1    = wa_select-anln1.
*     wa_imobil-anln2    = wa_select-anln2.
*     APPEND wa_imobil  TO it_imobil.
*   ENDLOOP.
* ENDIF.

  SELECT *
    FROM zaa005
    INTO TABLE @DATA(it_manual)
   WHERE bukrs    IN @p_bukrs
     AND gsber    IN @p_gsber
     AND kostl    IN @p_kostl
     AND gjahr    IN @p_gjahr
     AND zimob_v  IN @p_status
     AND cc_sobra IN @p_ccsobr
     AND manual   EQ @abap_true.

  IF it_manual[] IS INITIAL.
    wa_imobil-bukrs      = p_bukrs-low.
    wa_imobil-anln2      = '0000'.
    APPEND wa_imobil    TO it_imobil.
  ELSE.
    LOOP AT it_manual INTO DATA(wa_manual).
      wa_imobil-bukrs    = wa_manual-bukrs.
      wa_imobil-anln1    = wa_manual-anln1.
      wa_imobil-anln2    = wa_manual-anln2.
      APPEND wa_imobil  TO it_imobil.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'ZAA_SOBRA_FISICA'
    EXPORTING
      i_gjahr  = p_gjahr-low
    TABLES
      t_imobil = it_imobil.

*  IF sy-ucomm = 'SOBRA'.
*    CALL SCREEN '0400' STARTING AT 5 5 ENDING AT 50 13.
*  ENDIF.
*-CS2019000323 - 02.09.2021 - JT - fim

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SAVE_SOBRA_FISICA
*&---------------------------------------------------------------------*
FORM save_sobra_fisica.

  IF wa_dados_imob-anln1 IS NOT INITIAL.

    wa_sobra_fisica-bukrs = p_bukrs-low. "WA_DADOS_IMOB-BUKRS.
    wa_sobra_fisica-kostl = p_kostl-low. "WA_DADOS_IMOB-KOSTL.
    wa_sobra_fisica-anln1 = wa_dados_imob-anln1.
    wa_sobra_fisica-anln2 = wa_dados_imob-anln2.

    PERFORM seleciona_dados.
    PERFORM manipula_dados.
    PERFORM filtra_resultado. "3000006392/IR173266 - Stefanini - PRB - Correções ZAA16

    CLEAR: sy-ucomm.
    "CALL SCREEN '0100'.

  ENDIF.

ENDFORM.
