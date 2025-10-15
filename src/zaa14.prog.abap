*&---------------------------------------------------------------------*
*& Report  ZAA14
*& Relatório - Fluxo baixa Imobilizado - Aprovações
*&---------------------------------------------------------------------*
*& Transação:   ZAA19
*& Autor:       Jean Antunes
*& Data:        31/05/2018
*&---------------------------------------------------------------------*

REPORT zaa14.

*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES: zaa007, zaa008, anla, t001k, t001w.

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         status_ap(4)    TYPE c,
         aprovador_1(30) TYPE c,
         aprovador_2(30) TYPE c,
         aprovador_3(30) TYPE c,
         aprovador_4(30) TYPE c,
         deakt           TYPE anla-deakt,
         cellstyles      TYPE lvc_t_styl.
         INCLUDE STRUCTURE zaa007.
TYPES: END OF ty_saida.

*&---------------------------------------------------------------------*
*&  INTERNAL TABLES / STRUCTURES
*&---------------------------------------------------------------------*
DATA: tg_saida TYPE TABLE OF ty_saida,
      wg_saida LIKE LINE OF  tg_saida.

FIELD-SYMBOLS: <wg_saida> LIKE LINE OF tg_saida[].


*&---------------------------------------------------------------------*
*&  ALV STRUCTURE
*&---------------------------------------------------------------------*
DATA: it_fcat       TYPE TABLE OF lvc_s_fcat.

DATA: obj_container    TYPE REF TO cl_gui_custom_container,
      obj_alv          TYPE REF TO cl_gui_alv_grid,
      dg_splitter_1    TYPE REF TO cl_gui_splitter_container,
      dg_parent_1      TYPE REF TO cl_gui_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_2      TYPE REF TO cl_gui_container,
      dg_parent_2a     TYPE REF TO cl_gui_container,
      dg_parent_alv    TYPE REF TO cl_gui_container,
      picture          TYPE REF TO cl_gui_picture,
      dg_dyndoc_id     TYPE REF TO cl_dd_document,
      table_element    TYPE REF TO cl_dd_table_element,
      column           TYPE REF TO cl_dd_area,
      table_element2   TYPE REF TO cl_dd_table_element,
      column_1         TYPE REF TO cl_dd_area,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      it_exclude_fcode TYPE ui_functions,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant.

DATA: tl_function TYPE ui_functions.

CLASS       cl_gui_cfw DEFINITION LOAD.

DATA: sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table.

*&---------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:

      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

ENDCLASS.

*&---------------------------------------------------------------------*
*&  CLASS LCL_EVENT_RECEIVER DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_hotspot_report.
    PERFORM z_hotspot_report USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_button_click.
    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          ip_mode       TYPE sgs_rwmod.

    READ TABLE tg_saida[] ASSIGNING <wg_saida> INDEX es_row_no-row_id.

    DATA(vl_ano) = <wg_saida>-dt_solicitacao+0(4).

    CASE es_col_id.

      WHEN 'ANEXO'.

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

        IF ( <wg_saida>-anexo EQ '@1F@' ).        "Sem anexo

        ELSE.
          vl_ip_service = 'VIEW_ATTA'.
          ip_mode = 'R'.
        ENDIF.

        wa_bor-objkey   = |ZAA18{ <wg_saida>-anln1 }{ <wg_saida>-werks }{ vl_ano }|.
        wa_bor-objtype  = 'ZAA13'.

        anexo_obj->set_rw_mode( ip_mode = ip_mode ).
        anexo_obj->start_service_direct(
          EXPORTING
            ip_service       = vl_ip_service
            is_object        = wa_bor
          EXCEPTIONS
            no_object        = 1
            object_invalid   = 2
            execution_failed = 3
            OTHERS           = 4 ).


    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs  FOR t001k-bukrs OBLIGATORY,
                  p_werks  FOR t001w-werks, "OBLIGATORY,
                  p_anln1  FOR anla-anln1.  "OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: busca_dados, busca_anexos.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_001'.
  SET TITLEBAR 'T_001'.

  PERFORM criar_catalog_0001.
  PERFORM criar_alv_0001.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
MODULE exit_screen INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
FORM busca_dados .

  DATA: lt_celltab   TYPE lvc_t_styl,
        ls_celltab   TYPE lvc_s_styl,
        ls_color_num TYPE n.

  SELECT * FROM zaa007
    INTO TABLE @DATA(tg_imobilizado)
    WHERE bukrs IN @p_bukrs
      AND anln1 IN @p_anln1
      AND werks IN @p_werks.

  IF ( tg_imobilizado[] IS NOT INITIAL ).

    SELECT anla~bukrs, anla~anln1, anla~anln2, anla~deakt, anlz~werks
      FROM anla
      INNER JOIN anlz ON anla~bukrs EQ anlz~bukrs
                     AND anla~anln1 EQ anlz~anln1
                     AND anla~anln2 EQ anlz~anln2
      INTO TABLE @DATA(tg_anla)
      FOR ALL ENTRIES IN @tg_imobilizado
      WHERE anla~bukrs EQ @tg_imobilizado-bukrs
        AND anla~anln1 EQ @tg_imobilizado-anln1
        AND anlz~werks EQ @tg_imobilizado-werks.

    IF tg_anla[] IS NOT INITIAL.
      SORT tg_anla BY bukrs anln1 anln2 werks.
    ENDIF.


    SELECT * FROM zaa008
      INTO TABLE @DATA(tg_log)
      FOR ALL ENTRIES IN @tg_imobilizado[]
      WHERE bukrs EQ @tg_imobilizado-bukrs
        AND anln1 EQ @tg_imobilizado-anln1
        AND werks EQ @tg_imobilizado-werks.

    DELETE tg_log[] WHERE user_estorno IS NOT INITIAL.


    IF ( tg_log IS NOT INITIAL ).

      SORT tg_log BY bukrs werks anln1 nivel.

      SELECT usr21~bname, usr21~persnumber, adrp~name_text
        FROM usr21
        INNER JOIN adrp ON usr21~persnumber EQ adrp~persnumber
        INTO TABLE @DATA(tg_user)
        FOR ALL ENTRIES IN @tg_log
        WHERE bname EQ @tg_log-aprovador.

      IF ( tg_user[] IS NOT INITIAL ).
        SORT tg_user[] BY bname.
        DELETE ADJACENT DUPLICATES FROM tg_user[] COMPARING bname.
      ENDIF.

    ENDIF.

  ENDIF.

  IF ( tg_imobilizado[] IS NOT INITIAL ).

    SELECT * FROM zaa004 INTO TABLE @DATA(tg_estrat)
        FOR ALL ENTRIES IN @tg_imobilizado
        WHERE bukrs EQ @tg_imobilizado-bukrs
          AND gsber EQ @tg_imobilizado-werks
          AND kostl EQ @tg_imobilizado-kostl.

    LOOP AT tg_imobilizado[] INTO DATA(wl_imobilizado).

      MOVE-CORRESPONDING wl_imobilizado TO wg_saida.

      wg_saida-status_ap = COND #( WHEN ( wl_imobilizado-status EQ 'A' ) THEN icon_green_light
                                   WHEN ( wl_imobilizado-status EQ 'R' ) THEN icon_red_light
                                   ELSE  icon_yellow_light ).

      READ TABLE tg_anla[] INTO DATA(wl_anla) WITH KEY bukrs = wl_imobilizado-bukrs
                                                       anln1 = wl_imobilizado-anln1
                                                       anln2 = wl_imobilizado-anln2
                                                       werks = wl_imobilizado-werks
                                                       BINARY SEARCH.

      wg_saida-deakt = wl_anla-deakt.

      SORT tg_estrat[] BY bukrs gsber kostl nivel_aa DESCENDING.

      LOOP AT tg_log INTO DATA(wl_log) WHERE bukrs = wl_imobilizado-bukrs AND
                                             anln1 = wl_imobilizado-anln1 AND
                                             werks = wl_imobilizado-werks.

        CASE wl_log-nivel.
          WHEN '1'.
            READ TABLE tg_user[] INTO DATA(wl_user) WITH KEY bname = wl_log-aprovador.
            CONCATENATE '@01@' wl_user-name_text INTO wg_saida-aprovador_1.
          WHEN '2'.
            READ TABLE tg_user[] INTO wl_user WITH KEY bname = wl_log-aprovador.
            CONCATENATE '@01@' wl_user-name_text INTO wg_saida-aprovador_2.
          WHEN '3'.
            READ TABLE tg_user[] INTO wl_user WITH KEY bname = wl_log-aprovador.
            CONCATENATE '@01@' wl_user-name_text INTO wg_saida-aprovador_3.
          WHEN '4'.
            READ TABLE tg_user[] INTO wl_user WITH KEY bname = wl_log-aprovador.
            CONCATENATE '@01@' wl_user-name_text INTO wg_saida-aprovador_4.
        ENDCASE.

      ENDLOOP.

      IF ( wg_saida-aprovador_1 IS INITIAL ). "AND ( WL_IMOBILIZADO-STATUS EQ '' ).
        wg_saida-aprovador_1 = '@5D@'.
      ENDIF.

      IF ( wg_saida-aprovador_2 IS INITIAL ).
        READ TABLE tg_estrat[] WITH KEY bukrs = wl_imobilizado-bukrs
                                        gsber = wl_imobilizado-werks
                                        kostl = wl_imobilizado-kostl
                                        nivel_aa = 2 TRANSPORTING NO FIELDS.
        IF ( sy-subrc = 0 ).
          wg_saida-aprovador_2 = '@5D@'.
        ENDIF.
      ENDIF.

      IF ( wg_saida-aprovador_3 IS INITIAL ).
        READ TABLE tg_estrat[] WITH KEY bukrs = wl_imobilizado-bukrs
                                        gsber = wl_imobilizado-werks
                                        kostl = wl_imobilizado-kostl
                                        nivel_aa = 3 TRANSPORTING NO FIELDS.
        IF ( sy-subrc = 0 ).
          wg_saida-aprovador_3 = '@5D@'.
        ENDIF.
      ENDIF.
      IF ( wg_saida-aprovador_4 IS INITIAL ).
        READ TABLE tg_estrat[] WITH KEY bukrs = wl_imobilizado-bukrs
                                        gsber = wl_imobilizado-werks
                                        kostl = wl_imobilizado-kostl
                                        nivel_aa = 3 TRANSPORTING NO FIELDS.
        IF ( sy-subrc = 0 ).
          wg_saida-aprovador_4 = '@5D@'.
        ENDIF.
      ENDIF.

      IF ( wl_imobilizado-status EQ 'A' ) AND ( wg_saida-deakt IS INITIAL ).
        ls_color_num = 7.
        ls_celltab-fieldname = 'DEAKT'.
        ls_celltab-style     = ls_color_num.
        APPEND ls_celltab TO lt_celltab[].
        INSERT LINES OF lt_celltab INTO TABLE wg_saida-cellstyles.
      ENDIF.

      APPEND wg_saida TO tg_saida[].
      CLEAR: wl_log, wl_user, wg_saida, wl_anla.
      CLEAR: lt_celltab[], ls_celltab.

    ENDLOOP.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_0001
*&---------------------------------------------------------------------*
FORM criar_catalog_0001.

  IF ( tg_saida[] IS INITIAL ).
    MESSAGE 'Sem dados para os parâmetros informados!' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR: it_fcat[].

  PERFORM alv_catalog_0001 USING:

        'TG_SAIDA'  'STATUS_AP'         'Status'                '06'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'BUKRS'             'Empresa'               '06'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'WERKS'             'Filial'                '04'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'ANLN1'             'Imobilizado'           '08'    'X' ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'ANLN2'             'Sub nº'                '06'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'TXT50'             'Denominação Imob.'     '15'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'TXA50'             'Denominãção Imob.2'    '10'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'KOSTL'             'Centro Custo'          '10'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'DEAKT'             'Data Desativação'      '10'    ''  ''  'C700'  ''  'C' 'X',
        'TG_SAIDA'  'ZUGDT'             'Data Aquisição'        '10'    ''  ''  'C700'  ''  'C' 'X',
*** RMNI - CS1081418 - Alterar nomenclaturas - 12/04/2023 - Inicio
*        'TG_SAIDA'  'MOTIVO'            'Motivo Baixa'          '15'    ''  ''  'C700'  ''  'C' 'X',
        'TG_SAIDA'  'MOTIVO'            'Obs CSC Contábil'      '15'    ''  ''  'C700'  ''  'C' 'X',
*** RMNI - CS1081418 - Alterar nomenclaturas - 12/04/2023 - Fim
        'TG_SAIDA'  'VLR_AQ_BRL'        'Vlr.Aquisição BRL'     '10'    ''  'X' ''      ''  'C' 'X',
        'TG_SAIDA'  'DEPREC_BRL'        'Depreciação Acum.BRL'  '10'    ''  'X' ''      ''  'C' 'X',
        'TG_SAIDA'  'VLR_CONTABIL_BRL'  'Valor Contábil BRL'    '10'    ''  'X' ''      ''  'C' 'X',
        'TG_SAIDA'  'VLR_AQ_USD'        'Vlr.Aquisição USD'     '10'    ''  'X' ''      ''  'C' 'X',
        'TG_SAIDA'  'DEPREC_USD'        'Depreciação Acum.USD'  '10'    ''  'X' ''      ''  'C' 'X',
        'TG_SAIDA'  'VLR_CONTABIL_USD'  'Valor Contábil USD'    '10'    ''  'X' ''      ''  'C' 'X',
*** RMNI - CS1081418 - Alterar nomenclaturas - 12/04/2023 - Inicio
*        'TG_SAIDA'  'ESTADO_BEM'        'Estado do bem'         '11'    ''  ''  'C100'  ''  'C' 'X',
        'TG_SAIDA'  'ESTADO_BEM'        'Motivo Baixa'          '11'    ''  ''  'C100'  ''  'C' 'X',
*** RMNI - CS1081418 - Alterar nomenclaturas - 12/04/2023 Fim
        'TG_SAIDA'  'RESPONSAVEL'       'Responsável Aval.'     '12'    ''  ''  'C100'  ''  'C' 'X',
        'TG_SAIDA'  'ANEXO'             'Anexo'                 '05'    ''  ''  'C100'  ''  'C' 'X',
        'TG_SAIDA'  'OBS_CONTROLLER'    'Obs.Controller'        '20'    ''  ''  'C500'  ''  'C' 'X',
        'TG_SAIDA'  'SOLICITANTE'       'Solicitante'           '12'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'APROVADOR_1'       'Aprovador 1'           '12'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'APROVADOR_2'       'Aprovador 2'           '12'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'APROVADOR_3'       'Aprovador 3'           '12'    ''  ''  ''      ''  'C' 'X',
        'TG_SAIDA'  'APROVADOR_4'       'Aprovador 4'           '12'    ''  ''  ''      ''  'C' 'X'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_0001
*&---------------------------------------------------------------------*
FORM alv_catalog_0001  USING   p_table    TYPE c
                               p_campo    TYPE c
                               p_desc     TYPE c
                               p_tam      TYPE c
                               p_zero     TYPE c
                               p_sum      TYPE c
                               p_cor      TYPE c
                               p_convexit TYPE c
                               p_just     TYPE c
                               p_opt      TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat,
        p_hot   TYPE c.

  wl_fcat-tabname   = p_table.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  wl_fcat-emphasize = p_cor.
  wl_fcat-convexit  = p_convexit.
  wl_fcat-just      = p_just.
  wl_fcat-col_opt   = p_opt.

  IF ( wl_fcat-fieldname = 'ANLN1' ).
    wl_fcat-hotspot = 'X'.
  ENDIF.

  APPEND wl_fcat TO it_fcat.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_0001
*&---------------------------------------------------------------------*
FORM criar_alv_0001 .

  DATA: url(255) TYPE c,
        p_text   TYPE sdydo_text_element.

  gs_layout-zebra      = 'X'.
  gs_layout-stylefname = 'CELLSTYLES'.

  CREATE OBJECT obj_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  """"""""""""""""""""""""""""""""""""""""
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

  CREATE OBJECT dg_splitter_1
    EXPORTING
      parent  = obj_container
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
      height = 12.

  CALL METHOD dg_splitter_2->set_column_width
    EXPORTING
      id    = 1
      width = 65.

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

  CREATE OBJECT obj_alv
    EXPORTING
      i_parent          = dg_parent_alv
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  CALL METHOD obj_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = tg_saida[]
      it_fieldcatalog               = it_fcat[]
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
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    EXPORTING
      sap_style   = 'SAP_BOLD'
      style_class = 'SAP_BOLD'
    IMPORTING
      column      = column_1.

  SET HANDLER:
      lcl_event_handler=>handle_button_click FOR obj_alv,
      lcl_event_receiver=>zm_handle_hotspot_report FOR obj_alv.

  PERFORM organiza_cabecalho CHANGING p_text_table[].

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


ENDFORM.


FORM organiza_cabecalho CHANGING t_p_text_table.
*
  sdydo_text_element = 'Relatório de Solicitação de Baixa de Imobilizado: ____________________________________________________'.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  PERFORM forma_cabecalho USING p_bukrs  p_bukrs-option  p_bukrs-low  p_bukrs-high TEXT-002.
  PERFORM forma_cabecalho USING p_werks  p_werks-option  p_werks-low  p_werks-high TEXT-003.
  PERFORM forma_cabecalho USING p_anln1  p_anln1-option  p_anln1-low  p_anln1-high TEXT-004.

ENDFORM.


FORM forma_cabecalho  USING    p_status
                               p_status_option
                               p_status_low
                               p_status_high
                               p_text_006.
*
  IF p_status IS NOT INITIAL.
    IF p_status_option NE 'EQ' AND p_status_option NE 'BT'.
      sdydo_text_element = p_text_006.
      EXIT.
    ELSEIF p_status_option EQ 'BT'.
      sdydo_text_element = | { p_text_006 } { p_status_low } - { p_status_high } |.
    ELSE.
      sdydo_text_element = | { p_text_006 } { p_status_low } |.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR sdydo_text_element.
  ELSE.
    sdydo_text_element = p_text_006.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: sdydo_text_element.
  ENDIF.

ENDFORM.

*Busca a logo Marca e adiciona no cabeçario.
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
*&      Form  BUSCA_ANEXOS
*&---------------------------------------------------------------------*
FORM busca_anexos .

  DATA: lt_celltab TYPE lvc_t_styl,
        vl_obj_key TYPE sibflporb-instid,
        tl_anexos  TYPE TABLE OF bdn_con,
        vl_ano(4)  TYPE c,
        ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  "143990 CS2023000909 - ZAA19 - Imobilizado - PSA

  CLEAR: wg_saida.

*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Início de Alteração

*  LOOP AT tg_saida[] INTO wg_saida.
*
*    CLEAR: lt_celltab[], vl_ano.
*    vl_ano = wg_saida-dt_solicitacao+0(4).
*
*    IF wg_saida-anln1 IS NOT INITIAL AND wg_saida-werks  IS NOT INITIAL AND vl_ano IS NOT INITIAL .
*      vl_obj_key = |ZAA18{ wg_saida-anln1 }{ wg_saida-werks }{ vl_ano }|.

*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Fim de Alteração

  LOOP AT tg_saida[] ASSIGNING <wg_saida>.

    CLEAR: lt_celltab[], vl_ano.
    vl_ano = <wg_saida>-dt_solicitacao+0(4).

    vl_obj_key = |ZAA18{ <wg_saida>-anln1 }{ <wg_saida>-werks }{ vl_ano }|.

    IF vl_obj_key IS NOT INITIAL.

      "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
*          CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*            EXPORTING
**             LOGICAL_SYSTEM     =
*              classname          = 'ZAA13'
*              objkey             = vl_obj_key
*              client             = sy-mandt
*            TABLES
*              gos_connections    = tl_anexos
*            EXCEPTIONS
*              no_objects_found   = 1
*              internal_error     = 2
*              internal_gos_error = 3
*              OTHERS             = 4.

      SELECT SINGLE * FROM srgbtbrel WHERE instid_a = @vl_obj_key AND typeid_a = 'ZAA13' AND reltype = 'ATTA' INTO @DATA(wa_srgbtbrel).

*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Início de Alteração
      IF sy-subrc = 0.

        <wg_saida>-anexo = '@1E@'.

        l_mode                = cl_gui_alv_grid=>mc_style_disabled.
        ls_celltab-style      = cl_gui_alv_grid=>mc_style_button.
        ls_celltab-fieldname  = 'ANEXO'.
        INSERT ls_celltab INTO TABLE lt_celltab.
        INSERT LINES OF lt_celltab INTO TABLE <wg_saida>-cellstyles.

      ELSE.

        <wg_saida>-anexo = '@1F@'.

      ENDIF.

*          IF sy-subrc = 0.
*            IF ( tl_anexos[] IS NOT INITIAL ).
*
*              <wg_saida>-anexo = '@1E@'.
*
*              l_mode                = cl_gui_alv_grid=>mc_style_disabled.
*              ls_celltab-style      = cl_gui_alv_grid=>mc_style_button.
*              ls_celltab-fieldname  = 'ANEXO'.
*              INSERT ls_celltab INTO TABLE lt_celltab.
*              INSERT LINES OF lt_celltab INTO TABLE <wg_saida>-cellstyles.
*
*            ELSE.
*              <wg_saida>-anexo = '@1F@'.
*            ENDIF.
*          ELSE.
*
*          ENDIF.
*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Fim de Alteração


    ENDIF.

  ENDLOOP.
*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Início de Alteração
*    ENDIF.
*
*
*  ENDLOOP.
*** Stefanini - IR202594 - 11/10/2024 - LAZAROSR - Fim de Alteração

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_REPORT
*&---------------------------------------------------------------------*
FORM z_hotspot_report  USING    p_e_row_id
                                p_e_column_id
                                p_es_row_no.

  READ TABLE tg_saida[] INTO DATA(wl_saida) INDEX p_e_row_id.

  SET PARAMETER ID 'AN1' FIELD wl_saida-anln1.
  IF ( wl_saida-anln2 IS NOT INITIAL ).
    SET PARAMETER ID 'AN2' FIELD wl_saida-anln2.
  ENDIF.
  SET PARAMETER ID 'BUK' FIELD wl_saida-bukrs.
  CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

ENDFORM.
