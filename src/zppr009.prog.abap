**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Instrução                                                     |*
**/===========================================================================\*

REPORT zppr009.

TABLES: mchb, zppt0011, zppt0023.

TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zppt0011.
TYPES:
         maktx     TYPE makt-maktx,
         lifnr     TYPE lfa1-lifnr,
         name1     TYPE char50,
         qtd_itens TYPE string,
       END OF ty_saida,

       ty_t_saida         TYPE TABLE OF ty_saida WITH DEFAULT KEY,
       ty_t_dismemberment TYPE TABLE OF zppt0011 WITH DEFAULT KEY.


TYPES: BEGIN OF ty_dismemberments.
         INCLUDE TYPE zppt0011.
TYPES:
         menge TYPE  mseg-menge,
       END OF ty_dismemberments.


DATA dismemberments TYPE TABLE OF ty_dismemberments.
DATA       gt_outtab TYPE TABLE OF ty_saida.
DATA it_detalhe TYPE TABLE OF ty_saida.
DATA it_detalhe_alv TYPE TABLE OF ty_saida.
DATA it_exclud TYPE ui_functions.
DATA wa_outtab TYPE ty_saida.
DATA document TYPE REF TO cl_dd_document.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-i01.
  SELECT-OPTIONS:
  s_werks FOR mchb-werks NO-EXTENSION NO INTERVALS,
  s_lgort FOR mchb-lgort NO-EXTENSION NO INTERVALS,
  s_ordem FOR zppt0011-nr_ordem NO-EXTENSION NO INTERVALS.
*S_RSNUM FOR ZPPT0023-RSNUM NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

CLASS cl_main DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS run.

    METHODS set_title_and_status.

    METHODS select_data
      EXCEPTIONS
        data_not_found.

    METHODS get_dismemberments
      RETURNING VALUE(table) TYPE ty_t_dismemberment.

    METHODS get_pedidos
      RETURNING VALUE(table) TYPE ekko_tty.

    METHODS set_header.
    METHODS create_docking.

    METHODS get_material_description
      IMPORTING
        material    TYPE makt-matnr
      RETURNING
        VALUE(text) TYPE makt-maktx.

    METHODS get_werks_description
      IMPORTING
        werks       TYPE t001w-werks
      RETURNING
        VALUE(text) TYPE t001w-name1.

    METHODS get_lifnr_description
      IMPORTING
        ebeln       TYPE vbeln
      RETURNING
        VALUE(text) TYPE lfa1-name1.

    METHODS get_fieldcatalog
      IMPORTING input       TYPE char1
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.

    METHODS process_before_output.
    METHODS set_outtab_data.


    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.

    METHODS display.

    METHODS get_detail
      IMPORTING input         TYPE char25
      RETURNING VALUE(detail) TYPE ty_t_saida.

    METHODS get_order
      IMPORTING input        TYPE lvc_s_roid
      RETURNING VALUE(order) TYPE char25.

    METHODS print_smartforms.

  PRIVATE SECTION.

*    "//atributos
    DATA at_order TYPE zppt0011-nr_ordem.

*    "//tables
    DATA dismemberments TYPE TABLE OF ty_dismemberments.
    "DATA DISMEMBERMENTS         TYPE TABLE OF ZPPT0011.
    DATA pedidos                TYPE TABLE OF ekko.

*    "//Objects
    DATA docking       TYPE REF TO cl_gui_docking_container.
    DATA splitter      TYPE REF TO cl_gui_splitter_container.
    DATA splitter1     TYPE REF TO cl_gui_splitter_container.
    DATA custom_header TYPE REF TO cl_gui_container.
    DATA custom_grid   TYPE REF TO cl_gui_container.
    DATA custom_grid1  TYPE REF TO cl_gui_container.
    DATA grid          TYPE REF TO cl_gui_alv_grid.
    DATA grid1         TYPE REF TO cl_gui_alv_grid.
    DATA alv_tree      TYPE REF TO cl_gui_alv_tree.
    DATA _layout       TYPE lvc_s_layo.
    DATA _sort         TYPE lvc_t_sort.
*
ENDCLASS.

DATA r_main TYPE REF TO cl_main.

CLASS cl_main IMPLEMENTATION.

  METHOD run.

    CREATE OBJECT r_main.

    r_main->select_data( EXCEPTIONS data_not_found = 4 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.
      CALL SCREEN 0001.
    ENDIF.
  ENDMETHOD.

  METHOD process_before_output.
    "//set title
    me->set_title_and_status( ).

    "//screen components
    me->create_docking( ).

    "//set data
    me->set_header( ).
    me->set_outtab_data( ).

    "//display data
    me->display( ).
  ENDMETHOD.

  METHOD set_title_and_status.
    SET TITLEBAR 'MAIN_TITLE'.
    SET PF-STATUS 'MAIN_STATUS'.
  ENDMETHOD.

  METHOD select_data.

*    SELECT *
*      FROM ZPPT0011 AS A
*      INNER JOIN MSEG AS B
*      ON A~MBLNR = B~MBLNR  AND A~CHARG = B~CHARG
*      INTO CORRESPONDING FIELDS OF TABLE ME->DISMEMBERMENTS
*     WHERE A~WERKS IN S_WERKS
*       AND A~LGORT IN S_LGORT
*       AND NR_ORDEM IN S_ORDEM
*       AND NR_ORDEM IS NOT NULL.

*    IF S_RSNUM IS INITIAL.
*      SELECT *
*      FROM ZPPT0023 AS A
*      INNER JOIN MSEG AS B
*      ON A~MBLNR = B~MBLNR  AND A~CHARG = B~CHARG
*      INTO CORRESPONDING FIELDS OF TABLE ME->DISMEMBERMENTS
*     WHERE A~WERKS IN S_WERKS
*       AND A~LGORT IN S_LGORT
*       AND NR_ORDEM IN S_ORDEM
*       AND NR_ORDEM IS NOT NULL.
*
*    ELSE.
*
    SELECT *
        FROM zppt0023 AS a
        INNER JOIN mseg AS b
        ON a~mblnr = b~mblnr  AND a~charg = b~charg
        INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
       WHERE a~werks IN s_werks
         AND a~lgort IN s_lgort
         AND nr_ordem IN s_ordem
         AND nr_ordem IS NOT NULL.
*
*      IF DISMEMBERMENTS IS NOT INITIAL.
*        LOOP AT DISMEMBERMENTS ASSIGNING FIELD-SYMBOL(<W_DISMEMBERMENTS>).
*          <W_DISMEMBERMENTS>-RSNUM = <W_DISMEMBERMENTS>-NR_ORDEM.
*        ENDLOOP.
*
*        DELETE DISMEMBERMENTS WHERE RSNUM NE S_RSNUM.
*      ENDIF.
*    ENDIF.

    IF me->dismemberments IS NOT INITIAL.

      SELECT *
        FROM ekko
        INTO TABLE me->pedidos
       FOR ALL ENTRIES IN me->dismemberments
         WHERE ebeln = me->dismemberments-ebeln.
    ENDIF.

    IF me->dismemberments IS INITIAL.
      MESSAGE TEXT-e01 TYPE 'S' RAISING data_not_found.
    ENDIF.

  ENDMETHOD.
*
  METHOD get_dismemberments.
    MOVE me->dismemberments TO table.
  ENDMETHOD.

  METHOD get_pedidos.
    MOVE me->pedidos TO table.
  ENDMETHOD.

  METHOD set_header.

    DATA table_element  TYPE REF TO cl_dd_table_element.
    DATA table_texts     TYPE sdydo_text_table.
    DATA table_text     LIKE LINE OF table_texts.
    DATA column         TYPE REF TO cl_dd_area.

    IF ( document IS NOT BOUND ).
      CREATE OBJECT document.
    ELSE.
      document->initialize_document( ).
    ENDIF.

    "//Build title text
    document->add_text( text = 'Parâmetros de seleção:' sap_style = cl_dd_area=>heading ).
    document->new_line(  ). document->underline( ).

    document->add_table(
      EXPORTING
        no_of_columns     = 2
        border            = '0'
      IMPORTING
        table             = DATA(_doctable1)
    ).

    _doctable1->add_column( EXPORTING width = '30%' IMPORTING column = DATA(_column_key) ).
    _doctable1->add_column( IMPORTING column = DATA(_column_value) ).

    _column_key->add_text( text = 'CENTRO:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    _column_value->add_text( text = | { COND #( WHEN s_werks-high IS INITIAL AND s_werks-low IS NOT INITIAL
                                       THEN |{ s_werks-low } - { me->get_werks_description( s_werks-low ) }|
                                       WHEN s_werks-high IS NOT INITIAL
                                       THEN |{ s_werks-low } até { s_werks-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'DEPOSITO:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    _column_value->add_text( text = | { COND #( WHEN s_lgort-high IS INITIAL AND s_lgort-low IS NOT INITIAL
                                       THEN |{ s_lgort-low }|
                                       WHEN s_lgort-high IS NOT INITIAL
                                       THEN |{ s_lgort-low } até { s_lgort-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    document->merge_document( ).
    document->display_document( parent = custom_header ).

  ENDMETHOD.
*
  METHOD create_docking.

    CREATE OBJECT docking
      EXPORTING
        repid     = sy-repid
        dynnr     = '0001'
        side      = cl_gui_docking_container=>dock_at_top
        extension = 5000.

    CREATE OBJECT splitter
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        top        = 50
        parent     = docking
        rows       = 2
        columns    = 1.

    me->splitter->set_row_height( id = 1 height = 18 ).
    me->splitter->set_border( cl_gui_cfw=>false ).

    custom_header = me->splitter->get_container( row = 1 column = 1 ).
    custom_grid   = me->splitter->get_container( row = 2 column = 1 ).

    CREATE OBJECT splitter1
      EXPORTING
        parent  = custom_grid
        rows    = 1
        columns = 2.

    me->splitter1->set_column_width( id = 1 width = 100 ).
    me->splitter1->set_border( cl_gui_cfw=>false ).

    custom_grid  = splitter1->get_container( row = 1 column = 1 ).

  ENDMETHOD.
*
  METHOD set_outtab_data.
    DATA outtab TYPE ty_saida.
*
    DATA(_dismemberments) = me->get_dismemberments( ).
    DATA(_pedidos)        = me->get_pedidos( ).

    it_detalhe = VALUE #( FOR _dismemberment IN me->dismemberments
                             (
                                 nr_ordem = _dismemberment-nr_ordem
                                 clabs = _dismemberment-menge " CLABS
                                 ebeln = _dismemberment-ebeln
                                 matnr = _dismemberment-matnr
                                 werks = _dismemberment-werks
                                 lgort = _dismemberment-lgort
                                 charg = _dismemberment-charg
                                 lfabr = _dismemberment-lfabr
                                 vfdat = _dismemberment-vfdat
                                 umcha = _dismemberment-umcha
                                 rsnum = _dismemberment-rsnum
                                 mblnr = _dismemberment-mblnr
                                 usnam = _dismemberment-mblnr
                                 data_atual = _dismemberment-data_atual
                                 hora_atual = _dismemberment-hora_atual
                                 maktx = me->get_material_description( _dismemberment-matnr )
                                 name1 = me->get_lifnr_description( _dismemberment-ebeln )
*                                 NAME1 = |{ _PEDIDOS[ EBELN = _DISMEMBERMENT-EBELN ]-LIFNR } - { ME->GET_LIFNR_DESCRIPTION( _PEDIDOS[ EBELN = _DISMEMBERMENT-EBELN ]-LIFNR ) }|
                             )
                    ).

    DELETE me->dismemberments WHERE nr_ordem IS INITIAL.

    LOOP AT me->dismemberments INTO DATA(w_dismemberment).

      wa_outtab-nr_ordem   = w_dismemberment-nr_ordem.
      wa_outtab-werks      = w_dismemberment-werks.
      wa_outtab-clabs      = w_dismemberment-menge.

      COLLECT wa_outtab INTO gt_outtab.

    ENDLOOP.

    LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<f_outtab>).
      <f_outtab>-qtd_itens = |{ icon_batch } ({ REDUCE i( INIT x = 0 FOR ls IN it_detalhe WHERE ( nr_ordem EQ <f_outtab>-nr_ordem ) NEXT x = x + 1 ) })|.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_set_toolbar.
    "//Set Buttons with some exceptions
    DATA(_standard_toolbars) = e_object->mt_toolbar.
    CLEAR e_object->mt_toolbar.

    DATA(_functions) = VALUE rsis_t_range(
                            ( sign = 'I' option = 'EQ' low = '&FIND'      )
                            ( sign = 'I' option = 'EQ' low = '&MB_FILTER' )
                            ( sign = 'I' option = 'EQ' low = '&SORT_ASC'  )
                            ( sign = 'I' option = 'EQ' low = '&SORT_DSC'  )
                                         ).

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'CLOSE'
                    icon      = icon_close
                  ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'REL'
                    text = TEXT-i02
                    quickinfo = TEXT-i03
                    icon      = icon_transport_point
                  ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

    LOOP AT _standard_toolbars INTO DATA(_toolbar).
      CHECK ( _toolbar-function IN _functions ).
      APPEND _toolbar TO e_object->mt_toolbar.
    ENDLOOP.

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.

    CALL METHOD me->grid->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'CLOSE'.
        splitter1->set_column_width( id = 1 width = 100 ).
      WHEN 'REL'.
        me->print_smartforms( ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_double_click.

    me->at_order = get_order( es_row_no ).

    DATA(_fieldcatalog) = me->get_fieldcatalog( abap_false ).

    _sort = VALUE #(  ( spos ='1' fieldname = 'WERKS' )
                      ( spos ='2' fieldname = 'MATNR' subtot = abap_true )
                   ).

    "DATA(_DETALHE) = GET_DETAIL( ME->AT_ORDER ).
    it_detalhe_alv = get_detail( me->at_order ).
    _layout-cwidth_opt = abap_true.

    splitter1->set_column_width( id = 1 width = 20 ).
    me->custom_grid = splitter1->get_container( row = 1 column = 2 ).

    IF ( grid1 IS NOT INITIAL ).
      CALL METHOD grid1->free.
    ENDIF.

    CREATE OBJECT me->grid1
      EXPORTING
        i_parent = me->custom_grid.

    SET HANDLER: me->handle_user_command FOR me->grid1,
                 me->handle_set_toolbar  FOR me->grid1.

    it_exclud = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

    CALL METHOD me->grid1->set_table_for_first_display
      EXPORTING
        is_layout            = _layout
        it_toolbar_excluding = it_exclud
      CHANGING
        it_outtab            = it_detalhe_alv
        it_fieldcatalog      = _fieldcatalog
        it_sort              = _sort.

  ENDMETHOD.

  METHOD display.

    DATA(_fieldcatalog) = me->get_fieldcatalog( abap_true ).

    _layout-cwidth_opt = abap_true.

    CREATE OBJECT me->grid
      EXPORTING
        i_parent = me->custom_grid.

    SET HANDLER: me->handle_user_command FOR me->grid,
                 me->handle_double_click FOR me->grid.

    it_exclud = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

    CALL METHOD me->grid->set_table_for_first_display
      EXPORTING
        is_layout            = _layout
        it_toolbar_excluding = it_exclud
      CHANGING
        it_outtab            = gt_outtab
        it_fieldcatalog      = _fieldcatalog.

  ENDMETHOD.

  METHOD get_material_description.

    DATA: lv_material TYPE matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE maktx FROM makt INTO text WHERE matnr = lv_material.
  ENDMETHOD.

  METHOD get_werks_description.
    SELECT SINGLE name1 FROM t001w INTO text WHERE werks = werks.
  ENDMETHOD.

  METHOD get_lifnr_description.

    DATA(_pedidos) = me->get_pedidos( ).

    TRY .
        DATA(lifnr) = _pedidos[ ebeln = ebeln ]-lifnr.
      CATCH cx_sy_itab_line_not_found.
        CLEAR lifnr.
        EXIT.
    ENDTRY.

    SELECT SINGLE name1 FROM lfa1 INTO text WHERE lifnr = lifnr.
    text = |{ lifnr }-{ text }|.

  ENDMETHOD.

  METHOD get_fieldcatalog.

    fcat =
        VALUE #(
        ( fieldname = 'WERKS'     coltext = 'Centro'           outputlen = 07 no_out = SWITCH #( input WHEN abap_true  THEN abap_false
                                                                                                       WHEN abap_false THEN abap_true ) )
        ( fieldname = 'NR_ORDEM'  coltext = 'Nr Ordem'         outputlen = 12 no_out = SWITCH #( input WHEN abap_true  THEN abap_false
                                                                                                       WHEN abap_false THEN abap_true ) )
        ( fieldname = 'EBELN'     coltext = 'Pedido'           outputlen = 12 no_out = input      )
        ( fieldname = 'MATNR'     coltext = 'Material'         outputlen = 10 no_zero = abap_true no_out = input )
        ( fieldname = 'MAKTX'     coltext = 'Descrição'        outputlen = 25 no_out = input      )
        ( fieldname = 'LGORT'     coltext = 'Depósito'         outputlen = 10 no_out = input      )
        ( fieldname = 'NAME1'     coltext = 'Fornecedor'       outputlen = 25 no_zero = abap_true no_out = input )
        ( fieldname = 'CHARG'     coltext = 'Lote Amaggi'      outputlen = 13 no_out = input      )
        ( fieldname = 'LFABR'     coltext = 'Lote Fabricante'  outputlen = 13 no_out = input      )
        ( fieldname = 'MBLNR'     coltext = 'Doc Material'     outputlen = 10 no_out = input      )
        ( fieldname = 'VFDAT'     coltext = 'Validade'         outputlen = 10 no_out = input      )
        ( fieldname = 'CLABS'     coltext = 'Quantidade'       outputlen = 10 do_sum = SWITCH #( input  WHEN abap_true  THEN abap_false
                                                                                                        WHEN abap_false THEN abap_true ) )
        ( fieldname = 'QTD_ITENS' coltext = ''                 outputlen = 06 no_out = SWITCH #( input  WHEN abap_true  THEN abap_false
                                                                                                        WHEN abap_false THEN abap_true ) ) ).


  ENDMETHOD.

  METHOD get_detail.
    detail = VALUE #( FOR ls IN it_detalhe WHERE ( nr_ordem EQ input ) ( CORRESPONDING #( ls ) ) ).
  ENDMETHOD.

  METHOD get_order.
    TRY.
        order = gt_outtab[ input-row_id ]-nr_ordem.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD print_smartforms.

    DATA _collect TYPE TABLE OF zppt0011.
    DATA w_collect TYPE zppt0011.

    DATA(_detalhe) = get_detail( me->at_order ).

    DATA(itens) = VALUE zppt0011_t( FOR ls IN _detalhe WHERE ( nr_ordem = me->at_order ) ( CORRESPONDING #( ls ) ) ).

    LOOP AT itens INTO DATA(_itens).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _itens-matnr
        IMPORTING
          output       = _itens-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      w_collect-matnr = _itens-matnr.
      w_collect-clabs = _itens-clabs.
      COLLECT w_collect INTO _collect.
    ENDLOOP.

    DATA: vl_name    TYPE rs38l_fnam,
          ls_options TYPE ssfcompop,
          control    TYPE ssfctrlop.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZPPF0001'
      IMPORTING
        fm_name            = vl_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    ls_options-tddest   = 'LOCL'.     "Disposit. saída
    ls_options-tdimmed  = abap_true.  "Saída Imediata
    ls_options-tdnewid  = abap_true.  "Nova Ordem SPOOL
    ls_options-tdcovtitle = |Retirada_{ me->at_order }_{ sy-uname }_{ sy-datum }_{ sy-uzeit }|. "Titulo

    CALL FUNCTION vl_name
      EXPORTING
        user_settings    = ' '
        output_options   = ls_options
        header           = itens[ 1 ]
      TABLES
        itens            = _collect
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  cl_main=>run( ).

*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pbo OUTPUT.
  IF r_main IS INITIAL.
    CREATE OBJECT r_main.
  ENDIF.

  r_main->process_before_output( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai INPUT.
  IF sy-ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
