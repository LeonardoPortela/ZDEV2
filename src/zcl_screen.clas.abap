class ZCL_SCREEN definition
  public
  final
  create public .

public section.

  interfaces ZIF_SCREEN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SCREEN IMPLEMENTATION.


  METHOD zif_screen~get_pega_imagem_url.

    TYPES: BEGIN OF ty_graphic_table,
             line(255) TYPE x,
           END OF ty_graphic_table.

    DATA: graphic_table TYPE TABLE OF ty_graphic_table.

    DATA: l_graphic_xstr TYPE xstring.

    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
      EXPORTING
        p_object = 'GRAPHICS'
        p_name   = i_nome_logo
        p_id     = 'BMAP'
        p_btype  = 'BCOL'
      RECEIVING
        p_bmp    = l_graphic_xstr.

    DATA(graphic_size) = xstrlen( l_graphic_xstr ).
    DATA(l_graphic_conv) = graphic_size.
    DATA(l_graphic_offs) = 0.
    WHILE l_graphic_conv > 255.
      APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(255) ) TO graphic_table.
      l_graphic_offs = l_graphic_offs + 255.
      l_graphic_conv = l_graphic_conv - 255.
    ENDWHILE.
    APPEND VALUE #( line = l_graphic_xstr+l_graphic_offs(l_graphic_conv) ) TO graphic_table.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'IMAGE'
        subtype  = 'X-UNKNOWN'
        size     = graphic_size
        lifetime = 'T'
      TABLES
        data     = graphic_table
      CHANGING
        url      = r_url.

  ENDMETHOD.


  METHOD zif_screen~set_criar_tela_padrao_report.

    DATA: dg_splitter_2   TYPE REF TO cl_gui_splitter_container,
          picture         TYPE REF TO cl_gui_picture,
          dg_dyndoc_id    TYPE REF TO cl_dd_document,
          background_id   TYPE sdydo_key VALUE 'ALV_BACKGROUND',
          p_text_table_f  TYPE sdydo_text_table,
          p_text_table_v  TYPE sdydo_text_table,
          p_text_table_f2 TYPE sdydo_text_table,
          p_text_table_v2 TYPE sdydo_text_table.

    CLEAR r_criou.
    "
    " CLEAR: alv.
    " CLEAR: I_FILTROS[].
    " CLEAR ZCL_SCREEN=>ZIF_SCREEN~SPLIT.
    CHECK zcl_screen=>zif_screen~split IS NOT BOUND.

    zcl_screen=>zif_screen~split = NEW #( parent = cl_gui_container=>screen0 rows = 2 columns = 1 ).

    DATA(dg_parent_html) = zcl_screen=>zif_screen~split->get_container( EXPORTING row = 1 column = 1 ).

    dg_splitter_2 = NEW #( parent = dg_parent_html rows = 1 columns = 2 ).

    DATA(dg_parent_html1) = dg_splitter_2->get_container( EXPORTING row = 1 column = 1 ).

    dg_splitter_2->set_column_width( EXPORTING id = 1 width = 40 ).

    DATA(dg_parent_html2) = dg_splitter_2->get_container( EXPORTING row = 1 column = 2 ).

    picture = NEW #( parent = dg_parent_html2 ).

    picture->load_picture_from_url( EXPORTING url = zcl_screen=>zif_screen~get_pega_imagem_url( i_nome_logo = 'LOGO_NOVO' ) ).

    picture->set_display_mode( EXPORTING display_mode = picture->display_mode_fit_center ).

    DATA(dg_parent_grid) = zcl_screen=>zif_screen~split->get_container( EXPORTING row = 2 column = 1 ).

    zcl_screen=>zif_screen~split->set_row_height( EXPORTING id = 1 height = 15 ).

    alv = NEW #( i_parent = dg_parent_grid ).

    dg_dyndoc_id = NEW #( style = 'ALV_TO_HTML' background_color = 7 ).
    dg_dyndoc_id->initialize_document( ).

    dg_dyndoc_id->add_table( EXPORTING no_of_columns = 1 border = '0' width = '100%' IMPORTING table = DATA(table_element) ).
    IF i_titulo IS NOT INITIAL.
      table_element->add_column( IMPORTING column = DATA(column) ).
      table_element->set_column_style( EXPORTING col_no = 1 sap_style = cl_dd_document=>heading sap_align = 'CENTER' ).
      column->add_text( EXPORTING text = CONV #( i_titulo ) sap_style = 'HEADING' ).
    ENDIF.

    IF i_filtros[] IS NOT INITIAL.

*      READ TABLE I_FILTROS WITH KEY DIREITA = SPACE TRANSPORTING NO FIELDS.
*      IF SY-SUBRC IS INITIAL.
      dg_dyndoc_id->add_table( EXPORTING no_of_columns = 4 border = '0' width = '100%' IMPORTING table = DATA(table_element_linhas) ).
      table_element_linhas->add_column( IMPORTING column = DATA(column_1) ).
      table_element_linhas->add_column( IMPORTING column = DATA(column_2) ).
      table_element_linhas->add_column( IMPORTING column = DATA(column_3) ).
      table_element_linhas->add_column( IMPORTING column = DATA(column_4) ).

      table_element_linhas->set_column_style( EXPORTING col_no = 1 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
      table_element_linhas->set_column_style( EXPORTING col_no = 2 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).
      table_element_linhas->set_column_style( EXPORTING col_no = 3 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small sap_emphasis = cl_dd_area=>strong ).
      table_element_linhas->set_column_style( EXPORTING col_no = 4 sap_align = 'LEFT' sap_fontsize = cl_dd_document=>small ).

      LOOP AT i_filtros INTO DATA(wa_filtros).

*        IF WA_FILTROS-PARAMETRO2 IS NOT INITIAL.
*        ELSE.
*          TABLE_ELEMENT_LINHAS->SPAN_COLUMNS( EXPORTING COL_START_SPAN = COLUMN_2 NO_OF_COLS = 2 ).
*        ENDIF.

        APPEND wa_filtros-parametro TO p_text_table_f.
        APPEND wa_filtros-valor TO p_text_table_v.

        APPEND wa_filtros-parametro2 TO p_text_table_f2.
        APPEND wa_filtros-valor2 TO p_text_table_v2.

        column_1->add_text( EXPORTING text_table = p_text_table_f  fix_lines = abap_true ).
        column_2->add_text( EXPORTING text_table = p_text_table_v  fix_lines = abap_true ).
        column_3->add_text( EXPORTING text_table = p_text_table_f2 fix_lines = abap_true ).
        column_4->add_text( EXPORTING text_table = p_text_table_v2 fix_lines = abap_true ).

        CLEAR: p_text_table_f[], p_text_table_v[], p_text_table_f2[], p_text_table_v2[].

      ENDLOOP.
*      ENDIF.

*      READ TABLE I_FILTROS WITH KEY DIREITA = ABAP_TRUE TRANSPORTING NO FIELDS.
*      IF SY-SUBRC IS INITIAL.
*        DG_DYNDOC_ID->ADD_TABLE( EXPORTING NO_OF_COLUMNS = 4 BORDER = '0' WIDTH = '100%' IMPORTING TABLE = TABLE_ELEMENT_LINHAS ).
*        TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = COLUMN_1 ).
*        TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = COLUMN_2 ).
*        TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = COLUMN_3 ).
*        TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = COLUMN_4 ).
*
*        TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 1 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
*        TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 2 SAP_ALIGN = 'RIGHT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL ).
*        TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 3 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
*        TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 4 SAP_ALIGN = 'RIGHT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL ).
*
*        LOOP AT I_FILTROS INTO WA_FILTROS WHERE DIREITA EQ ABAP_TRUE.
*
**        IF WA_FILTROS-PARAMETRO2 IS NOT INITIAL.
**        ELSE.
**          TABLE_ELEMENT_LINHAS->SPAN_COLUMNS( EXPORTING COL_START_SPAN = COLUMN_2 NO_OF_COLS = 2 ).
**        ENDIF.
*
*          APPEND WA_FILTROS-PARAMETRO TO P_TEXT_TABLE_F.
*          DATA(VALOR_D) = | { WA_FILTROS-VALOR }  |.
*          APPEND VALOR_D TO P_TEXT_TABLE_V.
*
*          APPEND WA_FILTROS-PARAMETRO2 TO P_TEXT_TABLE_F2.
*          DATA(VALOR_D2) = | { WA_FILTROS-VALOR2 }  |.
*          APPEND VALOR_D2 TO P_TEXT_TABLE_V2.
*
*          COLUMN_1->ADD_TEXT( EXPORTING TEXT_TABLE = P_TEXT_TABLE_F  FIX_LINES = ABAP_TRUE ).
*          COLUMN_2->ADD_TEXT( EXPORTING TEXT_TABLE = P_TEXT_TABLE_V  FIX_LINES = ABAP_TRUE ).
*          COLUMN_3->ADD_TEXT( EXPORTING TEXT_TABLE = P_TEXT_TABLE_F2 FIX_LINES = ABAP_TRUE ).
*          COLUMN_4->ADD_TEXT( EXPORTING TEXT_TABLE = P_TEXT_TABLE_V2 FIX_LINES = ABAP_TRUE ).
*
*          CLEAR: P_TEXT_TABLE_F[], P_TEXT_TABLE_V[], P_TEXT_TABLE_F2[], P_TEXT_TABLE_V2[].
*
*        ENDLOOP.
*      ENDIF.
*
*
    ENDIF.

    html = NEW #( parent = dg_parent_html1 ).

    dg_dyndoc_id->merge_document( ).

    "DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND( PICTURE_ID = BACKGROUND_ID ).

    dg_dyndoc_id->html_control = html.

    dg_dyndoc_id->display_document( EXPORTING reuse_control = 'X' parent = dg_parent_html1 ).

    split = zcl_screen=>zif_screen~split.

    r_criou = abap_true.

  ENDMETHOD.
ENDCLASS.
