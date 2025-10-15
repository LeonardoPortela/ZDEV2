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
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br )                  |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Gestão de Custo de Manutenção                                             |*
**/===========================================================================\*
*
REPORT zpmr0027.

TABLES: cosp, t247, aufk, viaufkst.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bukrs FOR aufk-bukrs,
                s_pargb FOR cosp-pargb OBLIGATORY,
                s_equnr FOR viaufkst-equnr,
                s_gjahr FOR cosp-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT sy-datum(4),
                s_mes   FOR t247-mnr NO-EXTENSION,
                s_tplnr FOR viaufkst-tplnr NO-EXTENSION NO INTERVALS,
                s_kostl FOR viaufkst-kostl.
SELECTION-SCREEN: END OF BLOCK b1.

TYPES:
  BEGIN OF ty_detail,
    orderid    TYPE aufk-aufnr,
    kstar      TYPE cosp-kstar,
    order_type TYPE aufk-auart,
    enter_date TYPE aufk-erdat,
    short_text TYPE c LENGTH 40,
    vl_order   TYPE p DECIMALS 2,
  END OF ty_detail,

  BEGIN OF ty_afih,
    aufnr TYPE afih-aufnr,
    equnr TYPE afih-equnr,
  END OF ty_afih,

  BEGIN OF ty_aufk,
    objnr  TYPE aufk-objnr,
    aufnr  TYPE aufk-aufnr,
    auart  TYPE aufk-auart,
    erdat  TYPE aufk-erdat,
    ktext  TYPE aufk-ktext,
    bukrs  TYPE aufk-bukrs,
    werks  TYPE aufk-werks,
    perio  TYPE coep-perio,
    kstar  TYPE coep-kstar,
    wtgbtr TYPE coep-wtgbtr,
    vrgng  TYPE coep-vrgng,
    tplnr  TYPE iloa-tplnr,
    equnr  TYPE afih-equnr,
    eqktx  TYPE eqkt-eqktx,
  END OF ty_aufk.

TYPES: BEGIN OF ty_coep,
         objnr  TYPE coep-objnr,
         kstar  TYPE coep-kstar,
         wtgbtr TYPE coep-wtgbtr,
         perio  TYPE coep-perio,
         aufnr  TYPE aufnr,
       END OF  ty_coep.

DATA: mes TYPE string.

DATA: r_container TYPE REF TO cl_gui_custom_container,
      r_tree      TYPE REF TO cl_gui_list_tree,
      t_grupo     TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_empresa   TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_centro    TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_setor     TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_local_sup TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_local_inf TYPE TABLE OF viaufkst-tplnr WITH HEADER LINE,
      t_equi      TYPE TABLE OF viaufkst-equnr WITH HEADER LINE,
      t_mes       TYPE TABLE OF t247           WITH HEADER LINE,
*      T_SERVICO   TYPE TABLE OF CHAR10         WITH HEADER LINE,
*      T_MATERIAL  TYPE TABLE OF CHAR10         WITH HEADER LINE,
      vl_tplnr    TYPE viaufkst-tplnr,
      total_mes   TYPE wkfxxx,
      total_geral TYPE wkfxxx,
      it_detail   TYPE TABLE OF ty_detail,
      wa_detail   TYPE ty_detail,
*      IT_AUFK     TYPE TABLE OF AUFK,
      it_aufk     TYPE TABLE OF ty_aufk,
      it_aufk_aux TYPE TABLE OF ty_aufk,
      it_afih     TYPE TABLE OF ty_afih,
*      IT_COSP     TYPE TABLE OF COSP,
*      IT_COEP     TYPE TABLE OF TY_COEP WITH HEADER LINE,
*      IT_COEP_AUX TYPE TABLE OF TY_COEP WITH HEADER LINE,
      it_viaufkst TYPE TABLE OF viaufkst,
      it_eqkt     TYPE TABLE OF eqkt.

DATA: obj_grid  TYPE REF TO cl_gui_alv_grid,
      obj_cont  TYPE REF TO cl_gui_custom_container,
      wa_stable TYPE lvc_s_stbl VALUE abap_true.

DATA: t_servico  TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      t_material TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      r_conta    TYPE RANGE OF cosp-kstar WITH HEADER LINE.

TYPES: item_table_type LIKE STANDARD TABLE OF mtreeitm WITH DEFAULT KEY.

DATA: node_table  TYPE treev_ntab,
      item_table  TYPE item_table_type,
      lt_node_key TYPE treev_nks.

TYPES: BEGIN OF ty_saida,
         bukrs          TYPE aufk-bukrs,
         werks          TYPE aufk-werks,
         tplnr          TYPE viaufkst-tplnr,
         kstar          TYPE cosp-kstar,
         conta          TYPE char10,
         objnr          TYPE aufk-objnr,
         aufnr          TYPE aufk-aufnr,
         equnr          TYPE viaufkst-equnr,
         eqktx          TYPE eqkt-eqktx,
         m01            TYPE wtgxxx,
         m02            TYPE wtgxxx,
         m03            TYPE wtgxxx,
         m04            TYPE wtgxxx,
         m05            TYPE wtgxxx,
         m06            TYPE wtgxxx,
         m07            TYPE wtgxxx,
         m08            TYPE wtgxxx,
         m09            TYPE wtgxxx,
         m10            TYPE wtgxxx,
         m11            TYPE wtgxxx,
         m12            TYPE wtgxxx,
         total_servico  TYPE wtgxxx,
         total_material TYPE wtgxxx,
         total_geral    TYPE wtgxxx,
       END OF ty_saida,
       tt_saida TYPE TABLE OF ty_saida WITH DEFAULT KEY,

       BEGIN OF ty_local,
         tplnr TYPE viaufkst-tplnr,
         pltxu TYPE iflo-pltxu,
         equnr TYPE viaufkst-equnr,
         total TYPE wkfxxx,
       END OF ty_local.

DATA: it_saida    TYPE TABLE OF ty_saida WITH HEADER LINE,
      it_item_equ TYPE TABLE OF ty_saida WITH HEADER LINE,
      it_col      TYPE TABLE OF ty_saida WITH HEADER LINE,
      it_local    TYPE TABLE OF ty_local WITH HEADER LINE,
      cont        TYPE i.

TYPES: y_node_table_type LIKE STANDARD TABLE OF treev_node
                                                       WITH DEFAULT KEY,
       y_item_table_type LIKE STANDARD TABLE OF mtreeitm
                                                       WITH DEFAULT KEY.
TYPES: BEGIN OF y_tmenu01.
         INCLUDE STRUCTURE tmenu01.
         TYPES:  text            TYPE hier_text,
         tcode           TYPE sytcode,
         programm        TYPE programm,
         authority_check TYPE c,
       END OF y_tmenu01,
       BEGIN OF y_node_key,
         node_id  TYPE hier_guid,
         node_key TYPE tv_nodekey,
       END OF y_node_key.

CLASS lcl_application DEFINITION DEFERRED.
CLASS cl_gui_cfw      DEFINITION LOAD.

DATA: v_application      TYPE REF TO lcl_application,
      v_custom_container TYPE REF TO cl_gui_custom_container,
      v_html_control     TYPE REF TO cl_gui_html_viewer,
      v_tree             TYPE REF TO cl_gui_list_tree,
      v_ok_code          TYPE sy-ucomm,
      v_node_table       TYPE treev_ntab,
      v_item_table       TYPE y_item_table_type,
      v__node_table      TYPE tv_nodekey,
      v_events           TYPE cntl_simple_events,
      v_event            TYPE cntl_simple_event,
      v_node_key         TYPE tv_nodekey,
      v_item_name        TYPE tv_itmname,
      w_tmenu01          TYPE y_tmenu01,
      w_tmenu01r         TYPE tmenu01r,
      w_tmenu01t         TYPE tmenu01t,
      t_tmenu01          TYPE TABLE OF y_tmenu01,
      t_tmenu01_aux      TYPE TABLE OF y_tmenu01,
      t_node_key         TYPE TABLE OF y_node_key,
      t_tmenu01r         TYPE TABLE OF tmenu01r,
      t_tmenu01t         TYPE TABLE OF tmenu01t,
      w_node_key         TYPE y_node_key,
      v_tabix            TYPE i,
      v_count_node       TYPE i,
      v_node_id          TYPE hier_guid,
      v_node_key2        TYPE tv_nodekey,
      v_relat_key        TYPE tv_nodekey,
      v_node             LIKE treev_node,
      v_item             TYPE mtreeitm,
      v_return           TYPE flag,
      v_event2(30).

DATA: wa_bdcdata LIKE bdcdata,
      wa_message LIKE bdcmsgcoll,
      it_bdcdata LIKE STANDARD TABLE OF wa_bdcdata,
      it_message LIKE STANDARD TABLE OF wa_message.

DATA: str          TYPE REF TO data.

ASSIGN 'TY_DETAIL' TO FIELD-SYMBOL(<fs_str>).
CREATE DATA str TYPE (<fs_str>).

DATA(r_table) =
CORRESPONDING lvc_t_fcat(
cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr(
cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

******************************************************************
* SELECIONA DADOS
******************************************************************
START-OF-SELECTION.
  PERFORM seleciona_dados.

  CALL SCREEN 0100.

CLASS lcl_eventos DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.
CLASS lcl_eventos IMPLEMENTATION.

  METHOD on_hotspot_click.

    DATA: dataini TYPE c LENGTH 10,
          datafim TYPE c LENGTH 10.

    DATA: it_rsparams TYPE TABLE OF rsparams.

    CASE e_column_id-fieldname.
      WHEN: 'ORDERID'.

        TRY .
            wa_detail = it_detail[ e_row_id-index ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        PERFORM datas CHANGING dataini datafim.

        FREE it_bdcdata.

        PERFORM f_insert_shdb USING:
              ' '         ' '     'T'  'KOB1'               ' ',
              'RKAEP000'  '0110'  'X'  ' '                  ' ',
              ' '         ' '     ' '  'BDC_OKCODE'         '=%011',
              ' '         ' '     ' '  'AUFNR-LOW'          wa_detail-orderid,
              ' '         ' '     ' '  'KSTAR-LOW'          ' ',
              ' '         ' '     ' '  'R_BUDAT-LOW'        dataini,
              ' '         ' '     ' '  'R_BUDAT-HIGH'       datafim,
              'SAPLALDB'  '3000'  'X'  ' '                  ' ',
              ' '         ' '     ' '  'BDC_OKCODE'         '/EDELA',
              'SAPLALDB' 	'3000' 	'X'  ' '                  ' ',
              ' '         ' '     ' '  'BDC_SUBSCR'         'SAPLALDB                                3010SCREEN_HEADER',
              ' '         ' '     ' '  'BDC_CURSOR'         'RSCSEL_255-SLOW_I(01)',
              ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  wa_detail-kstar,
              ' '         ' '     ' '  'BDC_OKCODE'         '=ACPT',
              'RKAEP000'  '0110'  'X'  ' '                  ' ',
              ' '         ' '     ' '  'BDC_OKCODE'         '=ONLI'.

        CALL TRANSACTION 'KOB1' USING it_bdcdata MODE 'E'.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
      FOR EVENT node_double_click
                  OF cl_gui_list_tree
        IMPORTING node_key,
      handle_expand_no_children
      FOR EVENT expand_no_children
                  OF cl_gui_list_tree
        IMPORTING node_key,
      handle_item_double_click
      FOR EVENT item_double_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_button_click
      FOR EVENT button_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_link_click
      FOR EVENT link_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_item_context_menu_req
      FOR EVENT item_context_menu_request
                  OF cl_gui_list_tree
        IMPORTING node_key item_name menu,
      handle_item_context_menu_sel
      FOR EVENT item_context_menu_select
                  OF cl_gui_list_tree
        IMPORTING node_key item_name fcode,
      handle_checkbox_change
      FOR EVENT checkbox_change
                  OF cl_gui_list_tree
        IMPORTING node_key item_name checked.

ENDCLASS.                    "LCL_APPLICATION DEFINITION
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_node_double_click.

    DATA: vl_equnr  TYPE viaufkst-equnr,
          item_name TYPE  tv_itmname,
          local     TYPE viaufkst-tplnr.

    FREE: it_detail.

    v_event2 = 'NODE_DOUBLE_CLICK'.
    v_node_key = node_key.

    TRY .
        vl_equnr = item_table[ node_key = node_key
                              item_name = '301'
                             ]-text.
      CATCH cx_sy_itab_line_not_found.
        vl_equnr = ''.
    ENDTRY.

    TRY .
        local = item_table[ node_key = node_key
                           item_name = '302'
                          ]-text.
      CATCH cx_sy_itab_line_not_found.
        local = ''.
    ENDTRY.

*    CHECK VL_EQUNR IS NOT INITIAL.
    CHECK local IS NOT INITIAL.

    item_name = 1.

    LOOP AT it_saida INTO DATA(w_saida) WHERE tplnr EQ local.

      IF vl_equnr IS NOT INITIAL.
        IF w_saida-equnr NE vl_equnr.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF w_saida-conta EQ item_table[ node_key = node_key
                          item_name = 1
                         ]-text(10).
        TRY .
            DATA(wa_aufk) = it_aufk[ aufnr = w_saida-aufnr ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        APPEND VALUE #(
                        orderid    = w_saida-aufnr
                        kstar      = w_saida-kstar
                        order_type = wa_aufk-auart
                        enter_date = wa_aufk-erdat
                        short_text = wa_aufk-ktext
                        vl_order   = w_saida-total_geral
                      ) TO it_detail.

      ENDIF.
    ENDLOOP.

    IF it_detail IS NOT INITIAL.
      CALL SCREEN 0200.
    ENDIF.


  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD handle_item_double_click.
    v_event2 = 'ITEM_DOUBLE_CLICK'.
    v_node_key = node_key.
    v_item_name = item_name.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  handle_link_click.

    DATA: view_keys TYPE treev_nks.
    FREE lt_node_key.

    v_event2 = 'LINK_CLICK'.
    v_node_key = node_key.
    v_item_name = item_name.

    DATA(index) = line_index( node_table[ node_key = node_key ] ).
    DATA(nd_key) = node_key.

    CASE item_name.
      WHEN '125'.
        CALL METHOD r_tree->expand_node
          EXPORTING
            node_key            = nd_key
            level_count         = 0
            expand_subtree      = ' '
          EXCEPTIONS
            failed              = 1
            illegal_level_count = 2
            cntl_system_error   = 3
            node_not_found      = 4
            cannot_expand_leaf  = 5
            OTHERS              = 6.
      WHEN '130'.

        DO.
          TRY .
              DATA(wa_node) = node_table[ relatkey = nd_key ].
            CATCH cx_sy_itab_line_not_found.
              EXIT.
          ENDTRY.
          nd_key = wa_node-node_key.
          APPEND wa_node-relatkey TO lt_node_key.
        ENDDO.

        SORT lt_node_key.
        DELETE ADJACENT DUPLICATES FROM lt_node_key.
        CALL METHOD r_tree->expand_nodes
          EXPORTING
            node_key_table          = lt_node_key
          EXCEPTIONS
            failed                  = 1
            cntl_system_error       = 2
            error_in_node_key_table = 3
            dp_error                = 4
            OTHERS                  = 5.
        IF sy-subrc <> 0.
          MESSAGE a000(oo).
        ENDIF.

    ENDCASE.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  handle_button_click.
    v_event2 = 'BUTTON_CLICK'.
    v_node_key = node_key.
    v_item_name = item_name.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD handle_item_context_menu_req.
    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'TCOD'
        text  = 'Abrir em Nova Janela'.
  ENDMETHOD.                    "HANDLE_ITEM_CONTEXT_MENU_REQ


  METHOD handle_item_context_menu_sel.
    w_tmenu01r-ref_type = fcode.
  ENDMETHOD.                    "HANDLE_ITEM_CONTEXT_MENU_SEL

  METHOD  handle_checkbox_change.

    v_event2 = 'CHECKBOX_CHANGE'.
    v_node_key = node_key.
    v_item_name = item_name.

  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE

  METHOD handle_expand_no_children.

    DATA: node       TYPE treev_node,
          item_table TYPE y_item_table_type,
          item       TYPE mtreeitm.

    v_event2 = 'EXPAND_NO_CHILDREN'.
    v_node_key = node_key.

    CALL METHOD r_tree->add_nodes_and_items
      EXPORTING
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.

    IF sy-subrc <> 0.
      MESSAGE a088(sf) WITH 'Error'.
    ENDIF.

  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF100'.
  SET TITLEBAR 'TI100'.

  CALL METHOD cl_eaml_reporting=>ml_pai                     "EhP6 EAML
    CHANGING
      cv_ucomm = sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_TREE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_tree OUTPUT.

  PERFORM processo USING 'Criando Arvore'.

  CHECK r_container IS INITIAL.

  CREATE OBJECT v_application.

  CREATE OBJECT r_container
    EXPORTING
      container_name              = 'TREE_CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT r_tree
    EXPORTING
      parent                      = r_container
      node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
*     NODE_SELECTION_MODE         = CL_GUI_LIST_TREE=>NODE_SEL_MODE_MULTIPLE
      item_selection              = 'X'
      with_headers                = ' '
    EXCEPTIONS
      cntl_system_error           = 1
      create_error                = 2
      failed                      = 3
      illegal_node_selection_mode = 4
      lifetime_error              = 5.

* define the events which will be passed to the backend
  " node double click
  v_event-eventid = cl_gui_list_tree=>eventid_node_double_click.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " item double click
  v_event-eventid = cl_gui_list_tree=>eventid_item_double_click.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " expand no children
  v_event-eventid = cl_gui_list_tree=>eventid_expand_no_children.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " link click
  v_event-eventid = cl_gui_list_tree=>eventid_link_click.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " button click
  v_event-eventid = cl_gui_list_tree=>eventid_button_click.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " item context menu req
  v_event-eventid = cl_gui_list_tree=>eventid_item_context_menu_req.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  " item context menu sel
  v_event-eventid = cl_gui_list_tree=>eventid_item_keypress.
  v_event-appl_event = 'X'.
  APPEND v_event TO v_events.

  CALL METHOD r_tree->set_registered_events
    EXPORTING
      events                    = v_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc <> 0.
    MESSAGE a000(sf) WITH 'Error'.
  ENDIF.


  SET HANDLER v_application->handle_node_double_click     FOR r_tree.
  SET HANDLER v_application->handle_item_double_click     FOR r_tree.
  SET HANDLER v_application->handle_expand_no_children    FOR r_tree.
  SET HANDLER v_application->handle_link_click            FOR r_tree.
  SET HANDLER v_application->handle_button_click          FOR r_tree.
  SET HANDLER v_application->handle_item_context_menu_req FOR r_tree.
  SET HANDLER v_application->handle_item_context_menu_sel FOR r_tree.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  FILL_TREE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fill_tree OUTPUT.

  CHECK node_table[] IS INITIAL.

  PERFORM processo USING 'Adicionado dados na Tree View'.

  TYPES: BEGIN OF ty_hier,
           no    TYPE char30,
           pai   TYPE n LENGTH 15,
           filho TYPE n LENGTH 15,
         END OF ty_hier.

  DATA it_hier TYPE TABLE OF ty_hier WITH HEADER LINE .

  DATA: node TYPE treev_node,
        item TYPE mtreeitm.

  DATA rais TYPE n LENGTH 12.
  DATA filho TYPE n LENGTH 12.

  DATA pai  TYPE n LENGTH 12 VALUE '000000000000'.
  DATA fil  TYPE n LENGTH 12 VALUE '100000000000'.
  DATA fil1 TYPE n LENGTH 12 VALUE '200000000000'.
  DATA fil2 TYPE n LENGTH 12 VALUE '300000000000'.
  DATA fil3 TYPE n LENGTH 12 VALUE '400000000000'.
  DATA fil4 TYPE n LENGTH 12 VALUE '500000000000'.
  DATA fil5 TYPE n LENGTH 12 VALUE '600000000000'.
  DATA fil6 TYPE n LENGTH 12 VALUE '700000000000'.
  DATA fil7 TYPE n LENGTH 12 VALUE '800000000000'.
  DATA fil8 TYPE n LENGTH 12 VALUE '900000000000'.
  DATA cont1 TYPE n LENGTH 12 VALUE '000000000000'.

  DATA: campo TYPE char20.

  FIELD-SYMBOLS: <fs_campo> TYPE any.

  CLEAR node.

  DATA: l_last_key  TYPE lvc_nkey,
        l_month_key TYPE lvc_nkey.

  IF node_table[] IS INITIAL.
    LOOP AT t_grupo.

      CHECK NOT t_grupo IS INITIAL.
      ADD 1 TO pai.
      PERFORM node USING t_grupo pai abap_false abap_false abap_false.
      PERFORM item USING t_grupo pai abap_false.

      LOOP AT t_empresa.

        CHECK NOT t_empresa IS INITIAL.
        ADD 1 TO fil.
        CLEAR vl_tplnr.
        vl_tplnr = |{ t_grupo }.{ t_empresa }|.

        CHECK line_exists( it_local[ tplnr = vl_tplnr ] ).

        PERFORM node USING vl_tplnr fil abap_false pai abap_false.
        PERFORM item USING vl_tplnr fil abap_false.

        IF line_exists( it_saida[ tplnr = vl_tplnr ] ).
          PERFORM add_not_itens USING fil.
        ENDIF.

        LOOP AT t_centro.

          CHECK NOT t_centro IS INITIAL.
          ADD 1 TO fil1.
          CLEAR vl_tplnr.
          vl_tplnr = |{ t_grupo }.{ t_empresa }.{ t_centro }|.

          CHECK line_exists( it_local[ tplnr = vl_tplnr ] ).

          PERFORM node USING vl_tplnr fil1 abap_false fil abap_false.
          PERFORM item USING vl_tplnr fil1 abap_false.

          IF line_exists( it_saida[ tplnr = vl_tplnr ] ).
            PERFORM add_not_itens USING fil1.
          ENDIF.

          LOOP AT t_setor.

            CHECK NOT t_setor IS INITIAL.
            ADD 1 TO fil2.
            CLEAR vl_tplnr.
            vl_tplnr = |{ t_grupo }.{ t_empresa }.{ t_centro }.{ t_setor }|.

            CHECK line_exists( it_local[ tplnr = vl_tplnr ] ).

            PERFORM node USING vl_tplnr fil2 abap_false fil1 abap_false.
            PERFORM item USING vl_tplnr fil2 abap_false.

            IF line_exists( it_saida[ tplnr = vl_tplnr ] ).
              PERFORM add_not_itens USING fil2.
            ENDIF.

            LOOP AT t_local_sup.

              CHECK NOT t_local_sup IS INITIAL.
              ADD 1 TO fil3.
              CLEAR vl_tplnr.
              vl_tplnr = |{ t_grupo }.{ t_empresa }.{ t_centro }.{ t_setor }.{ t_local_sup }|.

              CHECK line_exists( it_local[ tplnr = vl_tplnr ] ).

              PERFORM node USING vl_tplnr fil3 abap_false fil2 abap_false.
              PERFORM item USING vl_tplnr fil3 abap_false.

              IF line_exists( it_saida[ tplnr = vl_tplnr ] ).
                PERFORM add_not_itens USING fil3.
              ENDIF.

              LOOP AT t_local_inf.

                CHECK NOT t_local_inf IS INITIAL.
                ADD 1 TO fil4.
                CLEAR vl_tplnr.
                vl_tplnr = |{ t_grupo }.{ t_empresa }.{ t_centro }.{ t_setor }.{ t_local_sup }.{ t_local_inf }|.

                CHECK line_exists( it_local[ tplnr = vl_tplnr ] ).

                PERFORM node USING vl_tplnr fil4 abap_false fil3 abap_false.
                PERFORM item USING vl_tplnr fil4 abap_false.

                IF line_exists( it_saida[ tplnr = vl_tplnr ] ).
                  PERFORM add_not_itens USING fil4.
                ENDIF.

              ENDLOOP.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  CALL METHOD r_tree->add_nodes_and_items
    EXPORTING
      node_table                     = node_table
      item_table                     = item_table
      item_table_structure_name      = 'MTREEITM'
    EXCEPTIONS
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.

  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

  IF s_tplnr-low IS NOT INITIAL.
    TRY .
        DATA(node_key_item) = item_table[ text = s_tplnr-low ]-node_key.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        DATA(line_node) = line_index( node_table[ relatkey = node_key_item ] ).
      CATCH cx_sy_itab_line_not_found.
        CLEAR line_node.
    ENDTRY.

    FREE: lt_node_key.
    LOOP AT node_table INTO DATA(wa_node).
      IF wa_node-relatkey IS INITIAL.
        CONTINUE.
      ENDIF.

      IF sy-tabix <= line_node.
        APPEND wa_node-relatkey TO lt_node_key.
      ENDIF.
    ENDLOOP.

    SORT lt_node_key.
    DELETE ADJACENT DUPLICATES FROM lt_node_key.
    CALL METHOD r_tree->expand_nodes
      EXPORTING
        node_key_table          = lt_node_key
      EXCEPTIONS
        failed                  = 1
        cntl_system_error       = 2
        error_in_node_key_table = 3
        dp_error                = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      MESSAGE a000(oo).
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXPLODIR'.
      PERFORM explodir.
    WHEN 'IMPLODIR'.
      PERFORM implodir.
    WHEN '%_GC 127 22'.
      PERFORM PF_EXIB_EQPTO.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados.

  PERFORM processo USING 'Selecionando Dados'.

  DATA: cont  TYPE n LENGTH 2,
        campo TYPE char20.

  FIELD-SYMBOLS: <fs_campo> TYPE any.

  PERFORM get_sets.

*  IF S_EQUNR IS NOT INITIAL.

*    SELECT *
*      FROM AFIH
*      INTO CORRESPONDING FIELDS OF TABLE IT_AFIH
*      WHERE EQUNR IN S_EQUNR.

*    IF IT_AFIH IS NOT INITIAL.

  SELECT b~objnr b~aufnr b~auart
         b~erdat b~ktext b~bukrs
         b~werks c~perio c~kstar
         c~wtgbtr c~vrgng i~tplnr
         a~equnr e~eqktx
       FROM aufk AS b
       INNER JOIN coep AS c ON c~objnr EQ b~objnr
       INNER JOIN afih AS a ON a~aufnr EQ b~aufnr
       INNER JOIN iloa AS i ON i~iloan EQ a~iloan
       LEFT  JOIN eqkt AS e ON e~equnr EQ a~equnr
      INTO CORRESPONDING FIELDS OF TABLE it_aufk
*        FOR ALL ENTRIES IN IT_AFIH
      WHERE b~werks IN s_pargb
        AND b~autyp EQ '30'
        AND b~kokrs EQ 'MAGI'
        AND b~bukrs IN s_bukrs
        AND c~werks IN s_pargb
        AND c~gjahr IN s_gjahr
        AND c~kstar IN r_conta
        AND b~kostl IN s_kostl
        AND a~equnr IN s_equnr.
*            AND B~AUFNR EQ IT_AFIH-AUFNR.

  SELECT  a~objnr a~aufnr  a~auart a~erdat
          a~ktext a~bukrs  a~werks c~perio
          c~kstar c~wtgbtr c~vrgng i~tplnr
          f~equnr e~eqktx
    FROM caufv AS a
    INNER JOIN afvc AS b ON a~aufpl EQ b~aufpl
    INNER JOIN coep AS c ON c~objnr EQ b~objnr
    INNER JOIN afih AS f ON f~aufnr EQ a~aufnr
    INNER JOIN iloa AS i ON i~iloan EQ f~iloan
    LEFT  JOIN eqkt AS e ON e~equnr EQ f~equnr
    APPENDING CORRESPONDING FIELDS OF TABLE it_aufk
*        FOR ALL ENTRIES IN IT_AFIH
    WHERE a~werks IN s_pargb
      AND a~autyp EQ '30'
      AND a~kokrs EQ 'MAGI'
      AND a~bukrs IN s_bukrs
      AND c~werks IN s_pargb
      AND c~gjahr IN s_gjahr
      AND c~kstar IN r_conta
      AND a~kostl IN s_kostl
      AND f~equnr IN s_equnr.
*          AND A~AUFNR EQ IT_AFIH-AUFNR.

*    ENDIF.
*
*  ELSE.
*
*    PERFORM PROCESSO USING 'Get Ordens '.
*
*    SELECT *
*       FROM AUFK AS B
*       INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
*       INNER JOIN AFIH AS A ON A~AUFNR EQ B~AUFNR
*       INNER JOIN ILOA AS I ON I~ILOAN EQ A~ILOAN
*       LEFT  JOIN EQKT AS E ON E~EQUNR EQ A~EQUNR
*       INTO CORRESPONDING FIELDS OF TABLE IT_AUFK
*       WHERE B~WERKS IN S_PARGB
*         AND B~AUTYP EQ '30'
*         AND B~KOKRS EQ 'MAGI'
*         AND B~BUKRS IN S_BUKRS
*         AND C~WERKS IN S_PARGB
*         AND C~GJAHR IN S_GJAHR
*         AND C~KSTAR IN R_CONTA.
**       ORDER BY AUFNR.
*
*    SELECT *
*      FROM CAUFV AS A
*      INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
*      INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
*      INNER JOIN AFIH AS F ON F~AUFNR EQ A~AUFNR
*      INNER JOIN ILOA AS I ON I~ILOAN EQ F~ILOAN
*      LEFT  JOIN EQKT AS E ON E~EQUNR EQ F~EQUNR
*      APPENDING CORRESPONDING FIELDS OF TABLE IT_AUFK
*      WHERE A~WERKS IN S_PARGB
*        AND A~AUTYP EQ '30'
*        AND A~KOKRS EQ 'MAGI'
*        AND A~BUKRS IN S_BUKRS
*        AND C~WERKS IN S_PARGB
*        AND C~GJAHR IN S_GJAHR
*        AND C~KSTAR IN R_CONTA.
**      ORDER BY AUFNR.
*
*  ENDIF.

*  SORT IT_AUFK_AUX BY AUFNR OBJNR.
*  DELETE ADJACENT DUPLICATES FROM IT_AUFK_AUX COMPARING AUFNR OBJNR.

*  SORT IT_AUFK BY AUFNR OBJNR.
*  DELETE ADJACENT DUPLICATES FROM IT_AUFK COMPARING AUFNR OBJNR.

*  LOOP AT IT_AUFK_AUX ASSIGNING FIELD-SYMBOL(<AUFK>).
*    IF LINE_EXISTS( IT_AUFK[ OBJNR = <AUFK>-OBJNR ] ).
*      CLEAR <AUFK>.
*    ELSE.
*      APPEND <AUFK> TO IT_AUFK.
*    ENDIF.
*  ENDLOOP.

*  APPEND LINES OF IT_AUFK_AUX TO IT_AUFK.

  SORT it_aufk BY aufnr.

*  SELECT OBJNR KSTAR WTGBTR PERIO
*   FROM COEP
*    INTO CORRESPONDING FIELDS OF TABLE IT_COEP_AUX
*    FOR ALL ENTRIES IN IT_AUFK
*    WHERE WERKS IN S_PARGB
*      AND GJAHR IN S_GJAHR
*      AND OBJNR EQ IT_AUFK-OBJNR
*      AND KSTAR IN R_CONTA
*      AND VRGNG EQ 'COIN'.

*  LOOP AT IT_COEP_AUX. MOVE IT_COEP_AUX TO IT_COEP. COLLECT IT_COEP. ENDLOOP.

*  LOOP AT IT_COEP ASSIGNING FIELD-SYMBOL(<COEP>).  TRY . <COEP>-AUFNR = IT_AUFK[ OBJNR = <COEP>-OBJNR ]-AUFNR. CATCH CX_SY_ITAB_LINE_NOT_FOUND.  ENDTRY. ENDLOOP.

*  SELECT * FROM VIAUFKST
*    INTO TABLE IT_VIAUFKST
*    FOR ALL ENTRIES IN IT_AUFK
*    WHERE AUFNR EQ IT_AUFK-AUFNR
*    AND EQUNR IN S_EQUNR.
*
*  SELECT * FROM EQKT
*    INTO TABLE IT_EQKT
*    FOR ALL ENTRIES IN IT_VIAUFKST
*      WHERE EQUNR EQ IT_VIAUFKST-EQUNR.

  PERFORM processo USING 'Organizando Dados'.

  LOOP AT it_aufk INTO DATA(wa_aufk).

    IF wa_aufk-vrgng NE 'COIN'.
      CONTINUE.
    ENDIF.

    it_saida-objnr = wa_aufk-objnr.

*    TRY.
*        DATA(WA_VIAUFKST) = IT_VIAUFKST[ AUFNR = WA_AUFK-AUFNR ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR WA_VIAUFKST.
*    ENDTRY.
*
*    TRY .
*        DATA(WA_EQKT) = IT_EQKT[ EQUNR = WA_VIAUFKST-EQUNR ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR WA_EQKT.
*    ENDTRY.

    it_saida-bukrs = wa_aufk-bukrs.
    it_saida-werks = wa_aufk-werks.
    it_saida-aufnr = wa_aufk-aufnr.
    it_saida-tplnr = wa_aufk-tplnr.
    it_saida-equnr = wa_aufk-equnr.
    it_saida-eqktx = wa_aufk-eqktx.

    t_equi = wa_aufk-equnr.
    COLLECT t_equi.
    CLEAR t_equi.

    it_saida-kstar = wa_aufk-kstar.
*    TRY .
*        IT_SAIDA-KSTAR = IT_COEP[ OBJNR = WA_AUFK-OBJNR ]-KSTAR.
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR IT_SAIDA-KSTAR.
*    ENDTRY.

*    IF LINE_EXISTS( IT_SAIDA[ AUFNR = WA_AUFK-AUFNR KSTAR = IT_SAIDA-KSTAR ] ).
*      CONTINUE.
*    ENDIF.

    FREE t_mes[].

*    LOOP AT IT_COEP INTO DATA(CO) WHERE AUFNR EQ WA_AUFK-AUFNR AND KSTAR = IT_SAIDA-KSTAR.

*      CASE CO-PERIO.
    CASE wa_aufk-perio.
      WHEN '001'. ADD wa_aufk-wtgbtr TO it_saida-m01.
      WHEN '002'. ADD wa_aufk-wtgbtr TO it_saida-m02.
      WHEN '003'. ADD wa_aufk-wtgbtr TO it_saida-m03.
      WHEN '004'. ADD wa_aufk-wtgbtr TO it_saida-m04.
      WHEN '005'. ADD wa_aufk-wtgbtr TO it_saida-m05.
      WHEN '006'. ADD wa_aufk-wtgbtr TO it_saida-m06.
      WHEN '007'. ADD wa_aufk-wtgbtr TO it_saida-m07.
      WHEN '008'. ADD wa_aufk-wtgbtr TO it_saida-m08.
      WHEN '009'. ADD wa_aufk-wtgbtr TO it_saida-m09.
      WHEN '010'. ADD wa_aufk-wtgbtr TO it_saida-m10.
      WHEN '011'. ADD wa_aufk-wtgbtr TO it_saida-m11.
      WHEN '012'. ADD wa_aufk-wtgbtr TO it_saida-m12.
    ENDCASE.

    IF line_exists( t_servico[ from = wa_aufk-kstar ] ).
      it_saida-conta = 'Serviço'.
      ADD wa_aufk-wtgbtr TO it_saida-total_servico.
    ELSE.
      it_saida-conta = 'Material'.
      ADD wa_aufk-wtgbtr TO it_saida-total_material.
    ENDIF.

*    ENDLOOP.

    it_saida-total_geral = it_saida-total_material + it_saida-total_servico.

    IF NOT it_saida-tplnr IS INITIAL.
      IF it_saida-tplnr NS s_tplnr-low.
        it_saida-tplnr = ''.
      ENDIF.
    ENDIF.

    APPEND it_saida.

    MOVE-CORRESPONDING it_saida TO it_col.

    CLEAR: it_col-kstar, it_col-objnr, it_col-aufnr, it_col-eqktx.
    COLLECT it_col.

    CLEAR: it_saida, it_col.

  ENDLOOP.

  cont = '01'.
  DO.
    IF cont GT '12'.
      EXIT.
    ENDIF.

    t_mes-mnr = cont.

    COLLECT t_mes.
    CLEAR t_mes.
    ADD 1 TO cont.
  ENDDO.


  LOOP AT it_saida.

    it_local-tplnr(4) = it_saida-tplnr(4). "4
    APPEND it_local.
    CLEAR it_local.

    it_local-tplnr(8) = it_saida-tplnr(8). "3
    APPEND it_local.
    CLEAR it_local.

    it_local-tplnr(13) = it_saida-tplnr(13). "4
    APPEND it_local.
    CLEAR it_local.

    it_local-tplnr(18) = it_saida-tplnr(18). "4
    APPEND it_local.
    CLEAR it_local.

    it_local-tplnr(24) = it_saida-tplnr(24). "5
    APPEND it_local.
    CLEAR it_local.

    it_local-tplnr(30) = it_saida-tplnr(30). "5
    APPEND it_local.
    CLEAR it_local.

    MOVE-CORRESPONDING it_saida TO it_item_equ.
    CLEAR: it_item_equ-kstar, it_item_equ-objnr, it_item_equ-aufnr, it_item_equ-conta.

    COLLECT it_item_equ.

  ENDLOOP.

  SORT it_local BY tplnr.
  DELETE ADJACENT DUPLICATES FROM it_local COMPARING tplnr.

  SELECT * FROM iflo
  INTO TABLE @DATA(it_iflo)
  FOR ALL ENTRIES IN @it_local
  WHERE tplnr EQ @it_local-tplnr.

  LOOP AT it_local ASSIGNING FIELD-SYMBOL(<local>).

    LOOP AT it_saida.

      CASE <local>-tplnr.
        WHEN it_saida-tplnr(4).  ADD it_saida-total_geral TO <local>-total.
        WHEN it_saida-tplnr(8).  ADD it_saida-total_geral TO <local>-total.
        WHEN it_saida-tplnr(13). ADD it_saida-total_geral TO <local>-total.
        WHEN it_saida-tplnr(18). ADD it_saida-total_geral TO <local>-total.
        WHEN it_saida-tplnr(24). ADD it_saida-total_geral TO <local>-total.
        WHEN it_saida-tplnr(30). ADD it_saida-total_geral TO <local>-total.
      ENDCASE.

      IF <local>-tplnr EQ it_saida-tplnr.
        <local>-equnr = it_saida-equnr.
      ENDIF.

      CLEAR it_saida.

    ENDLOOP.

    TRY .
        <local>-pltxu = it_iflo[ tplnr = <local>-tplnr ]-pltxu.
      CATCH cx_sy_itab_line_not_found.
        CLEAR <local>-pltxu.
    ENDTRY.
  ENDLOOP.

  LOOP AT it_local.

    t_grupo     = it_local-tplnr(4).    COLLECT t_grupo.
    t_empresa   = it_local-tplnr+5(3).  COLLECT t_empresa.
    t_centro    = it_local-tplnr+9(4).  COLLECT t_centro.
    t_setor     = it_local-tplnr+14(4). COLLECT t_setor.
    t_local_sup = it_local-tplnr+19(5). COLLECT t_local_sup.
    t_local_inf = it_local-tplnr+25(5). COLLECT t_local_inf.

    CLEAR: t_grupo, t_empresa, t_centro, t_setor, t_local_sup, t_local_inf.

  ENDLOOP.

  SELECT *
    FROM t247
    INTO TABLE @DATA(me)
    WHERE spras EQ @sy-langu.

  LOOP AT t_mes ASSIGNING FIELD-SYMBOL(<mes>).
    TRY .
        DATA(wme) = me[ mnr = <mes>-mnr ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    MOVE-CORRESPONDING wme TO <mes>.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_TPLNR  text
*      -->P_FIL5  text
*      -->P_ABAP_FALSE  text
*      -->P_FIL1  text
*      -->P_ABAP_TRUE  text
*----------------------------------------------------------------------*
FORM node USING p_p_dados p_pai p_f_dados p_filho p_true.

  CLEAR node.

  APPEND VALUE #(
                node_key = p_pai
                relatkey = p_filho
                relatship = cl_gui_list_tree=>relat_last_child
                isfolder = abap_true
                n_image   = icon_technical_place
                exp_image = icon_technical_place
                last_hitem = '1'
                expander = abap_true
                ) TO node_table.

  IF p_true EQ abap_true.
    LOOP AT node_table ASSIGNING FIELD-SYMBOL(<node>) WHERE node_key EQ p_pai.
      <node>-isfolder = abap_false.
      <node>-expander = abap_false.
      <node>-isfolder = abap_false.
      <node>-n_image   = abap_false.
      <node>-exp_image = abap_false.
      <node>-last_hitem = abap_false.
      <node>-expander = abap_false.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_TPLNR  text
*      -->P_FIL5  text
*      -->P_ABAP_TRUE  text
*----------------------------------------------------------------------*
FORM item USING  p_dados p_pai p_true.

  DATA: valor TYPE wkfxxx.

  IF p_true NE abap_true.

    CLEAR item.
    item-node_key   = p_pai.
    item-class      = cl_gui_list_tree=>item_class_text.
    item-length     = 40.
    item-style      = 9.
    item-item_name  = '1'.
    item-text   = p_dados.
    APPEND item TO item_table.

    item-style = 0.
    item-item_name = '2'.

    TRY .
        item-text = it_local[ tplnr = p_dados ]-pltxu.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        valor = it_local[ tplnr = p_dados ]-total.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    item-text = |{ item-text }|.
    APPEND item TO item_table.

    item-length = 18.
    item-item_name = '3'.
    item-ignoreimag = 'X'.
    item-usebgcolor = 'X'.
    item-style      = 6.
    item-text = |R${ valor }|.
    APPEND item TO item_table.

    IF node_table[ node_key = p_pai ]-expander NE abap_false.

      CLEAR item.
      item-node_key = p_pai.
      item-t_image   = icon_technical_place.
      item-class      = cl_gui_list_tree=>item_class_link.

      item-ignoreimag = abap_true.
      item-txtisqinfo = abap_true.

      item-length     = 4.
      item-item_name = '125'.
      item-t_image   = icon_technical_place.
      item-text      = 'Equipamento'.
      APPEND item TO item_table.

      item-item_name = '130'.
      item-t_image   = icon_expand_all.
      item-text      = 'Todos os objetos'.
      APPEND item TO item_table.

    ENDIF.

  ELSE.

    SELECT SINGLE eqktx
      FROM eqkt
      INTO @DATA(desc)
      WHERE equnr EQ @it_saida-equnr.

    DATA(equi) = it_saida-equnr.
    SHIFT equi LEFT DELETING LEADING '0'.

    CLEAR item.
    item-node_key = p_pai.
    item-item_name = '1'.
    item-class = cl_gui_list_tree=>item_class_text.
    item-length = 40.
    item-ignoreimag = 'X'.
    item-usebgcolor = 'X'.
    item-t_image = '@AJ@'.
    item-style      = 9.
    item-text = equi.
    APPEND item TO item_table.

    item-item_name = '2'.
    item-t_image = ''.
    item-style      = 0.
    item-text = desc.
    APPEND item TO item_table.

    item-length = 18.
    item-item_name = '3'.
    item-ignoreimag = 'X'.
    item-usebgcolor = 'X'.
    item-style      = 6.
    IF it_saida-conta EQ 'Serviço'.
      item-text = |{ it_saida-conta }  R${ it_saida-total_servico }|.
    ELSE.
      item-text = |{ it_saida-conta }  R${ it_saida-total_material }|.
    ENDIF.

    APPEND item TO item_table.

  ENDIF.

ENDFORM.

FORM item1 USING id p_dados p_pai p_true.

  DATA: valor TYPE wkfxxx.

  IF p_true NE abap_true.


    CLEAR item.

    item-text   = p_dados.

    CASE id.
      WHEN '1'.

        item-node_key   = p_pai.
        item-class      = cl_gui_list_tree=>item_class_text.
        item-length     = 40.
        item-style      = 9.
        item-item_name  = '1'.
*        ITEM-TEXT   = P_DADOS.
        APPEND item TO item_table.

      WHEN '2'.

        item-node_key   = p_pai.
        item-class      = cl_gui_list_tree=>item_class_text.
        item-length     = 40.
        item-style = 0.
        item-item_name = '2'.
*        ITEM-TEXT   = P_DADOS.
        APPEND item TO item_table.

      WHEN '3'.
        CONDENSE item-text NO-GAPS.

        item-node_key   = p_pai.
        item-class      = cl_gui_list_tree=>item_class_text.
        item-length = 18.
        item-item_name = '3'.
        item-ignoreimag = 'X'.
        item-usebgcolor = 'X'.
        item-style      = 6.
        item-text = |R${ item-text }|.
        APPEND item TO item_table.

      WHEN '4'.

        IF node_table[ node_key = p_pai ]-expander NE abap_false.

          CLEAR item.
          item-node_key = p_pai.
          item-t_image   = icon_technical_place.
          item-class      = cl_gui_list_tree=>item_class_link.

          item-ignoreimag = abap_true.
          item-txtisqinfo = abap_true.

          item-length     = 4.
          item-item_name = '125'.
          item-t_image   = icon_technical_place.
          item-text      = 'Equipamento'.
          APPEND item TO item_table.

          item-item_name = '130'.
          item-t_image   = icon_expand_all.
          item-text      = 'Todos os objetos'.
          APPEND item TO item_table.

        ENDIF.

    ENDCASE.

  ELSE.

    SELECT SINGLE eqktx
      FROM eqkt
      INTO @DATA(desc)
      WHERE equnr EQ @it_saida-equnr.

    DATA(equi) = it_saida-equnr.
    SHIFT equi LEFT DELETING LEADING '0'.

    CLEAR item.
    item-node_key = p_pai.
    item-item_name = '1'.
    item-class = cl_gui_list_tree=>item_class_text.
    item-length = 40.
    item-ignoreimag = 'X'.
    item-usebgcolor = 'X'.
    item-t_image = '@AJ@'.
    item-style      = 9.
    item-text = equi.
    APPEND item TO item_table.

    item-item_name = '2'.
    item-t_image = ''.
    item-style      = 0.
*    ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
    item-text = desc.
    APPEND item TO item_table.

    item-length = 18.
    item-item_name = '3'.
    item-ignoreimag = 'X'.
    item-usebgcolor = 'X'.
    item-style      = 6.

    IF it_saida-conta EQ 'Serviço'.
      item-text = |{ it_saida-conta }  R${ it_saida-total_servico }|.
    ELSE.
      item-text = |{ it_saida-conta }  R${ it_saida-total_material }|.
    ENDIF.

    APPEND item TO item_table.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_NOT_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_not_itens USING fil.

*  LOOP AT IT_SAIDA WHERE TPLNR = VL_TPLNR.
  LOOP AT it_item_equ WHERE tplnr = vl_tplnr.
    LOOP AT t_equi.

      IF it_item_equ-equnr EQ t_equi.
        ADD 1 TO fil5.
        PERFORM node USING it_item_equ-eqktx fil5 abap_false fil abap_false."ABAP_TRUE.
        PERFORM item1 USING '1' it_item_equ-equnr fil5 abap_false."ABAP_TRUE.
        PERFORM item1 USING '2' it_item_equ-eqktx fil5 abap_false."ABAP_TRUE.
        PERFORM item1 USING '3' it_item_equ-total_geral fil5 abap_false."ABAP_TRUE.
        PERFORM item1 USING '4' it_item_equ-equnr fil5 abap_false."ABAP_TRUE.

        DATA tabix TYPE i.

* adiciona o Mes
        DATA(mxx) = 1.
        ADD 1 TO fil6.

        PERFORM node USING abap_false fil6 abap_false fil5 abap_true.
        PERFORM item2 USING mxx '' fil6.

        LOOP AT t_mes.
          ADD 1 TO mxx.
          PERFORM item2 USING mxx t_mes-ltx fil6.
        ENDLOOP.

        ADD 1 TO mxx.
        PERFORM item2 USING mxx 'Custo_Acumulado.' fil6.

* Adiciona o Valor do Mes
        mxx = 1.
        ADD 1 TO fil7.

        PERFORM node USING abap_false fil7 abap_false fil5 abap_true.
        PERFORM item2 USING mxx 'Serviço' fil7.

* Serviço
        CLEAR total_geral.
        LOOP AT t_mes.
          ADD 1 TO mxx.

          CLEAR:  total_mes.
*          LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<SAIDA>)
          LOOP AT it_col ASSIGNING FIELD-SYMBOL(<saida>)
            WHERE tplnr = vl_tplnr
            AND equnr EQ it_item_equ-equnr
            AND conta EQ 'Serviço'.
*
            DATA(w_saida) = <saida>.
            campo = |M{ t_mes-mnr }|.
            ASSIGN COMPONENT campo OF STRUCTURE <saida> TO <fs_campo>.
*            <FS_CAMPO> = ABS( <FS_CAMPO> ).
            ADD <fs_campo> TO total_mes.
          ENDLOOP.
          PERFORM item2 USING mxx total_mes fil7.
          ADD total_mes TO total_geral.
        ENDLOOP.

        ADD 1 TO mxx.
        PERFORM item2 USING mxx total_geral fil7.
        PERFORM item2 USING '301' w_saida-equnr fil7.
        PERFORM item2 USING '302' vl_tplnr      fil7.

        mxx = 1.
        ADD 1 TO fil8.

        PERFORM node USING abap_false fil8 abap_false fil5 abap_true.
        PERFORM item2 USING mxx 'Material' fil8.

* Material
        CLEAR total_geral.
        LOOP AT t_mes.
          ADD 1 TO mxx.

          CLEAR:  total_mes.
*          LOOP AT IT_SAIDA ASSIGNING <SAIDA>
          LOOP AT it_col ASSIGNING <saida>
            WHERE tplnr = vl_tplnr
            AND equnr EQ it_item_equ-equnr
            AND conta EQ 'Material'.
*
            w_saida = <saida>.
            campo = |M{ t_mes-mnr }|.
            ASSIGN COMPONENT campo OF STRUCTURE <saida> TO <fs_campo>.
*            <FS_CAMPO> = ABS( <FS_CAMPO> ).
            ADD <fs_campo> TO total_mes.
          ENDLOOP.
          PERFORM item2 USING mxx total_mes fil8.
          ADD total_mes TO total_geral.
        ENDLOOP.

        ADD 1 TO mxx.
        PERFORM item2 USING mxx total_geral fil8.
        PERFORM item2 USING '301' w_saida-equnr fil8.
        PERFORM item2 USING '302' vl_tplnr      fil8.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITEM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MXX  text
*      -->P_T_MES_LTX  text
*      -->P_FIL6  text
*      -->P_ABAP_FALSE  text
*----------------------------------------------------------------------*
FORM item2 USING p_id p_dados p_pai.

  DATA: n TYPE i.
  DATA: n1 TYPE i.
  CLEAR item.

  n = p_id.
  item-text = p_dados.
  CONDENSE item-text NO-GAPS.

  APPEND VALUE #(
                  node_key   = p_pai
                  class      = cl_gui_list_tree=>item_class_text
                  font       = cl_gui_list_tree=>item_font_prop
                  length     = 10
                  style = COND #( WHEN p_id EQ '14' THEN 10 ELSE abap_false )
                  item_name  = p_id
                  hidden = COND #( WHEN p_id EQ '301' OR p_id EQ '302' THEN abap_true ELSE abap_false )
                  text = item-text
                ) TO item_table.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXPLODIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM explodir .
  CALL METHOD r_tree->expand_root_nodes
    EXPORTING
      level_count         = 0
      expand_subtree      = abap_true
    EXCEPTIONS
      failed              = 1
      illegal_level_count = 2
      cntl_system_error   = 3
      OTHERS              = 4.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPLODIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM implodir .
  CALL METHOD r_tree->collapse_all_nodes.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TI0200'.

  LOOP AT r_table ASSIGNING FIELD-SYMBOL(<fcat>).
    CASE <fcat>-fieldname.
      WHEN 'ORDERID'.       <fcat>-hotspot = abap_true.
      WHEN 'TIPO'.          <fcat>-scrtext_m = 'Conta'.
      WHEN 'ORDER_TYPE'.
      WHEN 'ENTER_DATE'.
      WHEN 'SHORT_TEXT'.    <fcat>-scrtext_m = 'Descrição'.
      WHEN 'VL_ORDER'.      <fcat>-scrtext_m = 'Valor da Ordem'.
    ENDCASE.
  ENDLOOP.

  IF obj_cont IS INITIAL.

    CREATE OBJECT obj_cont
      EXPORTING
        container_name = 'CC_DETALHES'.

    CREATE OBJECT obj_grid
      EXPORTING
        i_parent = obj_cont.

    CALL METHOD obj_grid->set_table_for_first_display
      EXPORTING
        i_save          = 'X'
      CHANGING
        it_fieldcatalog = r_table[]
        it_outtab       = it_detail[].

    SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_grid.

    CALL METHOD obj_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    wa_stable = abap_true.
    CALL METHOD obj_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0792   text
*      -->P_0793   text
*      -->P_0794   text
*----------------------------------------------------------------------*
FORM f_insert_shdb  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                  program   = p_program
                  dynpro    = p_dynpro
                  dynbegin  = p_start
                  fnam      = p_fnam
                  fval      = p_fval
                 ) TO it_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_DATAINI  text
*      <--P_DATAFIM  text
*----------------------------------------------------------------------*
FORM datas  CHANGING p_dataini
                     p_datafim.
  DATA: day   TYPE i,
        month TYPE i,
        year  TYPE i.

  IF s_mes-low IS NOT INITIAL AND s_mes-high IS NOT INITIAL.
    year = s_gjahr-low.
    month = s_mes-high.
  ELSEIF s_mes-low IS NOT INITIAL AND s_mes-high IS INITIAL.
    year = s_gjahr-low.
    month = s_mes-low.
  ELSEIF s_mes-low IS INITIAL AND s_mes-high IS INITIAL.
    year = s_gjahr-low.

    p_dataini = |01.01.{ s_gjahr-low }|.
    p_datafim = |31.12.{ s_gjahr-low }|.
    EXIT.
  ENDIF.

  CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
    EXPORTING
      i_date_month = month
      i_date_year  = year
    IMPORTING
      e_max_days   = day.

  p_dataini = |01.{ s_mes-low }.{ s_gjahr-low }|.
  p_datafim = |{ day }.{ s_mes-high }.{ s_gjahr-low }|.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SETS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sets .

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'Z_SERVICO'
    TABLES
      set_values    = t_servico
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  FREE r_conta.
  LOOP AT t_servico. APPEND VALUE #( option = 'EQ' sign = 'I' low = t_servico-from ) TO r_conta. ENDLOOP.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'Z_MATERIAL'
    TABLES
      set_values    = t_material
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_material. APPEND VALUE #( option = 'EQ' sign = 'I' low = t_material-from ) TO r_conta. ENDLOOP.


ENDFORM.

FORM processo USING VALUE(p_texto).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = p_texto.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_EXIB_EQPTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_exib_eqpto .

*  DATA: vl_equnr  TYPE viaufkst-equnr,
*          item_name TYPE  tv_itmname,
*          local     TYPE viaufkst-tplnr.
*
*    FREE: it_detail.
*
*    v_event2 = 'NODE_DOUBLE_CLICK'.
*    v_node_key = node_key.
*
*    TRY .
*        vl_equnr = item_table[ node_key = node_key
*                              item_name = '301'
*                             ]-text.
*      CATCH cx_sy_itab_line_not_found.
*        vl_equnr = ''.
*    ENDTRY.
*
*    TRY .
*        local = item_table[ node_key = node_key
*                           item_name = '302'
*                          ]-text.
*      CATCH cx_sy_itab_line_not_found.
*        local = ''.
*    ENDTRY.
*
**    CHECK VL_EQUNR IS NOT INITIAL.
*    CHECK local IS NOT INITIAL.
*
*    item_name = 1.

ENDFORM.
