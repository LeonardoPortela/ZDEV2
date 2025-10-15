*&---------------------------------------------------------------------*
*& Report  ZGL025
*&
*&---------------------------------------------------------------------*
*&  ZNFW0007 -- ZWRR0005 -- Estratégia Noas Writer
*&  ZGl017   -- ZGL016 	 -- Enviar E-mail
*&---------------------------------------------------------------------*

REPORT zgl025 MESSAGE-ID zctb.

*&--------------------------------------------------------------------&*
*& Declaração de Types                                                &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF ty_alv_zglt043.
TYPES:    mark            TYPE char01,
          txt50           TYPE skat-txt50,
          butxt           TYPE t001-butxt,
          land1           TYPE t001-land1,
          moeda_01        TYPE waers,
          moeda_02        TYPE waers,
          moeda_03        TYPE waers,
          icone           TYPE char04,
          icone_view      TYPE char04,
          icone_aprova    TYPE char04,
          icone_recusa    TYPE char04,
          rowcolor        TYPE char04,
          desc_bname      TYPE usalias,
          bname	          TYPE xubname,
          dep_resp        TYPE zimp_cad_depto-dep_resp,
          desc_dep_resp   TYPE zimp_cad_depto-dep_resp_desc,
          "usuario_reconc  TYPE zglt059-bn_liberacao,
          cod_nota        TYPE zfied010,
          descr_nota      TYPE zfied032,
          codigo          TYPE zfied007,
          descr	          TYPE zfied008,
          dif_mi1         TYPE zglt043-sdo_mi,
          dif_mi2         TYPE zglt043-sdo_mi,
          dif_mi3         TYPE zglt043-sdo_mi,
          cta_monet	      TYPE zfied011,
          tx_fech3        TYPE zfied020,
          tx_fech4        TYPE zfied020,
          nivel_liberador TYPE ze_nivel.
          INCLUDE TYPE zglt043.
TYPES: END OF ty_alv_zglt043.

TYPES: BEGIN OF ty_editor,
         line(72),
       END OF ty_editor,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END OF ty_fields.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: it_zglt043           TYPE TABLE OF zglt043 WITH HEADER LINE,
      it_zglt043_alv       TYPE TABLE OF ty_alv_zglt043 WITH HEADER LINE,
      it_zglt059           TYPE TABLE OF zglt059 WITH HEADER LINE,
      it_zglt043_reg       TYPE TABLE OF zglt043 WITH HEADER LINE,
      ok_code              TYPE syucomm,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      wa_layout            TYPE lvc_s_layo,
      wa_stable            TYPE lvc_s_stbl,
      t_fieldcatalog       TYPE lvc_t_fcat,
      splitter             TYPE REF TO cl_gui_splitter_container,
      container_1          TYPE REF TO cl_gui_container,
      splitter_02          TYPE REF TO cl_gui_splitter_container,
      container_2          TYPE REF TO cl_gui_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      wa_zglt043_alv       TYPE ty_alv_zglt043,
      lbl_fecha1           TYPE string,
      lbl_fecha2           TYPE string,
      lbl_fecha3           TYPE string,
      lbl_fecha4           TYPE string,
      gs_scroll_col        TYPE lvc_s_col,
      gs_scroll_row        TYPE lvc_s_roid,
      it_hints             TYPE lvc_t_qinf,
      wa_hints             TYPE lvc_s_qinf,
      manager              TYPE REF TO cl_gos_manager,
      obj                  TYPE borident,
      objtype              TYPE borident-objtype VALUE 'ZGL026',
      gb_tp_lib            TYPE zglt043-status_lib VALUE 'L'.

CLASS: lcl_alv_toolbar      DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: g_container TYPE scrfname VALUE 'TL_RECONCILIACAO',
           ok_save     TYPE syucomm  VALUE 'SAVE',
           ok_email    TYPE syucomm  VALUE 'EMAIL',
           ok_refresh  TYPE syucomm  VALUE 'REFRESH',
           ok_back     TYPE syucomm  VALUE 'BACK',
           ok_exit     TYPE syucomm  VALUE 'EXIT',
           ok_cta_a    TYPE syucomm  VALUE 'CTA_A',
           ok_cta_l    TYPE syucomm  VALUE 'CTA_L'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
    CLASS-METHODS:
      on_double_click2 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.


  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

* Método de  execução para Hotspot
  METHOD on_hotspot_click.
    IF e_row_id-rowtype IS INITIAL.
      READ TABLE it_zglt043_alv INTO wa_zglt043_alv INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        CASE e_column_id-fieldname.
          WHEN 'ICONE_VIEW'.
            PERFORM view_doc USING wa_zglt043_alv.
          WHEN 'ICONE'.
            PERFORM print_doc USING wa_zglt043_alv.
          WHEN 'ICONE_APROVA'.
            PERFORM aprovacao USING sy-tabix CHANGING wa_zglt043_alv.
          WHEN 'ICONE_RECUSA'.
            PERFORM recusar USING sy-tabix CHANGING wa_zglt043_alv.
        ENDCASE.
      ENDIF.
    ENDIF.
    LEAVE TO SCREEN 0100.
  ENDMETHOD.                    "ON_HOTSPOT_CLICK

* Método de  execução para Duplo-click
  METHOD on_double_click.
    IF e_row-rowtype IS INITIAL.
      LOOP AT it_zglt043_alv INTO wa_zglt043_alv.
        wa_zglt043_alv-rowcolor = 'C210'.
        MODIFY it_zglt043_alv INDEX sy-tabix FROM wa_zglt043_alv TRANSPORTING rowcolor.
      ENDLOOP.
      READ TABLE it_zglt043_alv INTO wa_zglt043_alv INDEX e_row-index.
      IF sy-subrc IS INITIAL.
        wa_zglt043_alv-rowcolor = 'C710'.
        MODIFY it_zglt043_alv INDEX e_row-index FROM wa_zglt043_alv TRANSPORTING rowcolor.

        CALL METHOD grid1->get_scroll_info_via_id
          IMPORTING
            es_col_info = gs_scroll_col
            es_row_no   = gs_scroll_row.
      ENDIF.
    ENDIF.
    LEAVE TO SCREEN 0100.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_double_click2.

  ENDMETHOD.                    "ON_DOUBLE_CLICK2

  METHOD on_click.

  ENDMETHOD.                    "ON_CLICK

  METHOD on_data_changed.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finisheD

  "on_data_changed_finisheD
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  APPEND ok_save  TO fcode.
  IF it_zglt059[] IS INITIAL AND it_zglt043_reg[] IS INITIAL.
    APPEND ok_email TO fcode.

    IF gb_tp_lib EQ 'L'.
      APPEND ok_cta_l TO fcode.
    ELSE.
      APPEND ok_cta_a TO fcode.
    ENDIF.

  ELSE.
    APPEND ok_cta_l TO fcode.
    APPEND ok_cta_a TO fcode.
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  DATA: waref      TYPE REF TO data.

  IF g_custom_container IS INITIAL.

    wa_layout-zebra      = abap_true.
    wa_layout-no_rowmark = abap_true.
    wa_stable-row        = abap_true.
    wa_layout-info_fname = 'ROWCOLOR'.

    "WA_LAYOUT-NO_TOOLBAR = ABAP_TRUE.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-001. "'Contas Conciliadas Disponíveis para Liberação'

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 1
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.

    PERFORM montar_layout.

    REFRESH tl_function.
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

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        it_except_qinfo      = it_hints
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_zglt043_alv[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_double_click FOR grid1,
                 lcl_event_handler=>on_data_changed_finished FOR grid1,
                 lcl_event_handler=>on_data_changed FOR grid1,
                 lcl_event_handler=>on_hotspot_click FOR grid1.

  ELSE.
    CALL METHOD grid1->refresh_table_display.

    CALL METHOD grid1->set_scroll_info_via_id
      EXPORTING
        is_col_info = gs_scroll_col
        is_row_no   = gs_scroll_row.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'ICONE_VIEW'      'Visualizar'       '04' ' ' abap_false ' ' abap_true  'C' abap_true,
        2 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'ICONE'           'Imprimir'         '04' ' ' abap_false ' ' abap_true  'C' abap_true,
        3 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'ICONE_APROVA'    'Aprovar'          '04' ' ' abap_false ' ' abap_true  'C' abap_true,
        4 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'ICONE_RECUSA'    'Recusar'          '04' ' ' abap_false ' ' abap_true  'C' abap_true,
        5 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'GJAHR'           TEXT-002           '05' ' ' abap_false ' ' abap_false 'C' abap_false,
        6 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'MONAT'           TEXT-003           '03' ' ' abap_false ' ' abap_false 'C' abap_false,
        7 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'BUKRS'           TEXT-004           '08' ' ' abap_false ' ' abap_false 'C' abap_false,

*        8 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'U_RECONC'        text-045           '14' ' ' abap_false ' ' abap_false 'L' abap_false,
        8 ''         ' '               'IT_ZGLT043_ALV' 'DESC_DEP_RESP'   TEXT-005           '30' ' ' abap_false ' ' abap_false 'L' abap_false,
        9 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'USUARIO_RECONC'           'Usuário Reconciliante'  '23' ' ' abap_false ' ' abap_false 'L' abap_false,
       10 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'SAKNR'           TEXT-006           '10' ' ' abap_false ' ' abap_false 'L' abap_false,
       11 'ZGLT036'  ' '               'IT_ZGLT043_ALV' 'TXT50'           TEXT-007           '30' ' ' abap_false ' ' abap_false 'L' abap_false,
       12 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'MOEDA_01'        TEXT-008           '08' ' ' abap_false ' ' abap_false 'C' abap_false,
       13 'ZGLT043'  'SDO_MI'          'IT_ZGLT043_ALV' 'SDO_MI'          TEXT-009           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
       14 'ZGLT043'  'SDO_REL_AUX_MI'  'IT_ZGLT043_ALV' 'SDO_REL_AUX_MI'  TEXT-010           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
       15 'ZGLT043'  'SDO_MI2'         'IT_ZGLT043_ALV' 'DIF_MI1'         TEXT-011           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
       16 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'MOEDA_02'        TEXT-012           '08' ' ' abap_false ' ' abap_false 'C' abap_false,
       17 'ZGLT043'  'SDO_MI2'         'IT_ZGLT043_ALV' 'SDO_MI2'         TEXT-013           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
       18 'ZGLT043'  'SDO_REL_AUX_MI2' 'IT_ZGLT043_ALV' 'SDO_REL_AUX_MI2' TEXT-014           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
       19 'ZGLT043'  'SDO_MI2'         'IT_ZGLT043_ALV' 'DIF_MI2'         TEXT-015           '20' ' ' abap_true  ' ' abap_false 'R' abap_false,
"       19 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'MOEDA_03'        '3ª Moeda'         '08' ' ' ABAP_FALSE ' ' ABAP_FALSE 'C' ABAP_FALSE,
"       20 'ZGLT043'  'SDO_MI3'         'IT_ZGLT043_ALV' 'SDO_MI3'         'Saldo 3ª Moeda'   '20' ' ' ABAP_TRUE  ' ' ABAP_FALSE 'R' ABAP_FALSE,
"       21 'ZGLT043'  'SDO_REL_AUX_MI3' 'IT_ZGLT043_ALV' 'SDO_REL_AUX_MI3' 'Aux 3ª Moeda'     '20' ' ' ABAP_TRUE  ' ' ABAP_FALSE 'R' ABAP_FALSE,
"       22 'ZGLT043'  'SDO_MI2'         'IT_ZGLT043_ALV' 'DIF_MI3'         'Dif. 3ª Moeda'    '20' ' ' ABAP_TRUE  ' ' ABAP_FALSE 'R' ABAP_FALSE,
       20 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'TX_FECH1'        TEXT-016           '15' ' ' abap_false ' ' abap_false 'R' abap_false.
  "       24 'ZGLT043'  ' '               'IT_ZGLT043_ALV' 'TX_FECH2'        'Tx.Fech. 3ª/1ª'   '15' ' ' ABAP_FALSE ' ' ABAP_FALSE 'R' ABAP_FALSE.

  CLEAR: it_hints[].

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_print.
  wa_hints-text      = TEXT-017. "'Imprimir'.
  wa_hints-fieldname = 'ICONE'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_set_state.
  wa_hints-text      = TEXT-018. "'Aprovar'.
  wa_hints-fieldname = 'ICONE_APROVA'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_led_yellow.
  wa_hints-text      = TEXT-019. "'Registro foi Recusado'.
  wa_hints-fieldname = 'ICONE_APROVA'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_system_undo.
  wa_hints-text      = TEXT-020. "'Desfazer Aprovação'.
  wa_hints-fieldname = 'ICONE_APROVA'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_terminated_task.
  wa_hints-text      = TEXT-021. "'Recusar'.
  wa_hints-fieldname = 'ICONE_RECUSA'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_led_yellow.
  wa_hints-text      = TEXT-022. "'Registro foi Aprovado'.
  wa_hints-fieldname = 'ICONE_RECUSA'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_system_undo.
  wa_hints-text      = TEXT-023. "'Desfazer Recusa'.
  wa_hints-fieldname = 'ICONE_RECUSA'.
  APPEND wa_hints TO it_hints.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_hotspot)
                            VALUE(p_just)
                            VALUE(p_icon).

  DATA: w_fieldcatalog TYPE lvc_s_fcat.

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.
  w_fieldcatalog-hotspot       = p_hotspot.
  w_fieldcatalog-just          = p_just.
  w_fieldcatalog-icon          = p_icon.
  APPEND w_fieldcatalog TO t_fieldcatalog.
ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA:   vflg_ico(1).

  CASE ok_code.
    WHEN ok_refresh.
      PERFORM f_refresh.
    WHEN ok_email.
      PERFORM enviar_email.
    WHEN ok_cta_l.
      gb_tp_lib = 'L'.
      PERFORM f_refresh.
    WHEN ok_cta_a.
      gb_tp_lib = 'A'.
      PERFORM f_refresh.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok_code.
    WHEN ok_back.
      IF it_zglt059[] IS INITIAL AND it_zglt043_reg[] IS INITIAL.
        SET SCREEN 0.
      ELSE.
        MESSAGE s011.
      ENDIF.
    WHEN ok_exit.
      IF it_zglt059[] IS INITIAL AND it_zglt043_reg[] IS INITIAL.
        LEAVE PROGRAM.
      ELSE.
        MESSAGE s011.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_contas_reconciliadas OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM atualiza_contas_reconciliadas.
  ENDIF.

  IF wa_zglt043_alv IS NOT INITIAL.
    MOVE wa_zglt043_alv TO it_zglt043_alv.
  ENDIF.

ENDMODULE.                 " CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dynp_values_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_REPID   text
*      -->US_DYNNR   text
*      -->US_FIELD   text
*      -->US_VALUE   text
*      -->CH_SUBRC   text
*----------------------------------------------------------------------*
FORM dynp_values_update USING us_repid
                              us_dynnr
                              us_field
                              us_value
                     CHANGING ch_subrc.

  DATA: da_dynpfield_tab LIKE dynpread OCCURS 0 WITH HEADER LINE,
        da_stepl         LIKE sy-stepl,
        da_repid         LIKE d020s-prog,
        da_dynnr         LIKE d020s-dnum.

  ch_subrc = 4.
  REFRESH da_dynpfield_tab.

  MOVE us_repid TO da_repid.
  MOVE us_dynnr TO da_dynnr.

  GET CURSOR LINE da_stepl.

  MOVE da_stepl TO da_dynpfield_tab-stepl.
  MOVE us_field TO da_dynpfield_tab-fieldname.
  MOVE us_value TO da_dynpfield_tab-fieldvalue.
  APPEND da_dynpfield_tab.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = da_repid
      dynumb               = da_dynnr
    TABLES
      dynpfields           = da_dynpfield_tab
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc EQ 0.
    ch_subrc = 0.
  ENDIF.

ENDFORM.                    " DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      Form  Atualiza_lotes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_contas_reconciliadas .

  RANGES it_status_lib FOR zglt043-status_lib.

  DATA: it_skat      TYPE TABLE OF skat WITH HEADER LINE,
        it_t001      TYPE TABLE OF t001 WITH HEADER LINE,
        it_x001      TYPE TABLE OF x001 WITH HEADER LINE,
        it_t058      TYPE TABLE OF zglt058 WITH HEADER LINE,
        it_t041      TYPE TABLE OF zglt041 WITH HEADER LINE,
        it_depa      TYPE TABLE OF zimp_cad_depto WITH HEADER LINE,
        it_usrefus   TYPE TABLE OF usrefus WITH HEADER LINE,
        it_notas     TYPE TABLE OF zglt039 WITH HEADER LINE,
        it_t059      TYPE TABLE OF zglt059 WITH HEADER LINE,
        vg_nivel     TYPE ze_nivel,
        vg_max_nivel TYPE ze_nivel,
        e_x001       LIKE x001,
        lc_value     TYPE p DECIMALS 4.

  CLEAR: it_x001[],
         it_zglt043_alv[].

  it_status_lib-sign   = 'I'.
  it_status_lib-option = 'EQ'.
  it_status_lib-low    = gb_tp_lib.
  it_status_lib-high   = gb_tp_lib.
  APPEND it_status_lib.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zglt043
    FROM zglt043 AS a
   INNER JOIN zglt041 AS i ON i~bukrs EQ a~bukrs AND i~saknr EQ a~saknr "AND A~GJAHR EQ I~GJAHR "/Modificação CS2017000372
   WHERE status_lib IN it_status_lib
     AND EXISTS ( SELECT *
                    FROM zglt042 AS t
                   WHERE a~bukrs GE t~empresa_de
                     AND a~bukrs LE t~empresa_ate
                     AND a~monat GE t~mes_de
                     AND a~monat LE t~mes_ate
                     AND a~gjahr GE t~ano_de
                     AND a~gjahr LE t~ano_ate
                     AND t~dep_resp EQ i~dep_resp2  )
   ORDER BY a~bukrs a~gjahr a~monat a~saknr.

  IF it_zglt043[] IS NOT INITIAL.
    SELECT *
      INTO TABLE it_t041
      FROM zglt041
       FOR ALL ENTRIES IN it_zglt043
     WHERE bukrs EQ it_zglt043-bukrs
       AND saknr EQ it_zglt043-saknr.
*       AND GJAHR EQ IT_ZGLT043-GJAHR. "/Modificação CS2017000372

    SORT it_t041 BY bukrs saknr.
  ENDIF.

  IF it_t041[] IS NOT INITIAL.
    SELECT *
      INTO TABLE it_notas
      FROM zglt039
       FOR ALL ENTRIES IN it_t041
     WHERE codigo   EQ it_t041-cod_clas_bal
       AND cod_nota EQ it_t041-cod_clas_not2.

    SORT it_notas BY codigo cod_nota.

    SELECT *
      INTO TABLE it_depa
      FROM zimp_cad_depto
       FOR ALL ENTRIES IN it_t041
     WHERE dep_resp EQ it_t041-dep_resp2.

    SORT it_depa BY dep_resp.

    SELECT * INTO TABLE it_usrefus
      FROM usrefus
       FOR ALL ENTRIES IN it_t041
     WHERE bname = it_t041-bname2.

    SORT it_usrefus BY bname.

  ENDIF.

  SELECT * INTO TABLE it_t058
    FROM zglt058
   WHERE bname EQ sy-uname.

  SORT it_t058 BY bukrs dep_resp.

  IF it_zglt043[] IS NOT INITIAL.

    SELECT *
      INTO TABLE it_t001
      FROM t001
       FOR ALL ENTRIES IN it_zglt043
     WHERE bukrs EQ it_zglt043-bukrs.

    SORT it_t001 BY bukrs.

    LOOP AT it_t001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs                = it_t001-bukrs
        IMPORTING
          e_x001                 = e_x001
        EXCEPTIONS
          currency_2_not_defined = 1
          currency_3_not_defined = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        APPEND e_x001 TO it_x001.
      ENDIF.

    ENDLOOP.

    SORT it_x001 BY bukrs.

    SELECT *
      INTO TABLE it_skat
      FROM skat
       FOR ALL ENTRIES IN it_zglt043
     WHERE spras EQ sy-langu
       AND saknr EQ it_zglt043-saknr.

    SORT it_skat BY ktopl saknr.

    SELECT * INTO TABLE it_t059
      FROM zglt059
       FOR ALL ENTRIES IN it_zglt043
     WHERE bukrs EQ it_zglt043-bukrs
       AND saknr EQ it_zglt043-saknr
       AND monat EQ it_zglt043-monat
       AND gjahr EQ it_zglt043-gjahr.

    SORT it_t059 BY bukrs saknr monat gjahr.

  ENDIF.

  LOOP AT it_zglt043.

    CLEAR: it_zglt043_alv.

    IF gb_tp_lib = 'A'.               "Solução encontrada ->124594 CS2023000500 - Incluir coluna usuário responsável ZGL037 Parte 2 - PSA
      READ TABLE it_t059 WITH KEY bukrs = it_zglt043-bukrs
                                  saknr = it_zglt043-saknr
                                  monat = it_zglt043-monat
                                  gjahr = it_zglt043-gjahr
                                  ck_ultimo_log = 'N'.
    ELSE.
      READ TABLE it_t059 WITH KEY bukrs = it_zglt043-bukrs
                                  saknr = it_zglt043-saknr
                                  monat = it_zglt043-monat
                                  gjahr = it_zglt043-gjahr
                                  ck_ultimo_log = 'S'.
    ENDIF.



    "Não existe registro de Liberação
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE it_t041 WITH KEY bukrs = it_zglt043-bukrs
                                saknr = it_zglt043-saknr.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

*    VG_MAX_NIVEL = 999.

    READ TABLE it_t058 WITH KEY bukrs    = it_zglt043-bukrs
                                dep_resp = it_t041-dep_resp2.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ELSE.
*      SELECT MAX( NIVEL ) INTO VG_MAX_NIVEL
*        FROM ZGLT058
*       WHERE BUKRS    EQ IT_ZGLT043-BUKRS
*         AND DEP_RESP EQ IT_T041-DEP_RESP2.
    ENDIF.

    "Se nível da pessoa for maior que nivel de quem recusou já sai.
    IF it_t058-nivel > it_t059-nivel AND it_t059-ck_recusa = 'S' AND it_t059-status_lib = 'R'.
      CONTINUE.
    ENDIF.

*    Aprovada - Somente Ultimo Nivel de Aprovação que Visualiza
    IF it_zglt043-status_lib EQ 'A' "AND IT_T058-NIVEL EQ VG_MAX_NIVEL
   AND it_t059-ck_recusa     EQ 'N'. "AND IT_T059-NIVEL EQ IT_T058-NIVEL.
      "IT_ZGLT043_ALV-ICONE_APROVA = ICON_SYSTEM_UNDO.
      it_zglt043_alv-icone_recusa = icon_terminated_task.
    ELSE.

*      IF GB_TP_LIB = 'A' AND IT_T058-NIVEL > IT_T059-NIVEL.
*        CONTINUE.
*      ENDIF.

      "Se Nivel Atual de Aprovação for Maior que do Usuário Atual
      "E não for o próximo nivel acima
      "Ignorar Registro

*->124594 CS2023000500 - Incluir coluna usuário responsável ZGL037 Parte 2 - PSA
      vg_nivel = it_t058-nivel + 1. "Meu Nível + 1
      IF ( ( it_t059-nivel > it_t058-nivel ) AND ( it_t059-ck_recusa EQ 'N' ) ) OR
      "ou nivel atual não é o proximo nível de aprovação/recusa
         ( it_t059-nivel > vg_nivel ).
        CONTINUE.
      ENDIF.

      "Se o nível anterior não for o Meu Nível - 1
      vg_nivel = it_t058-nivel - 1. "Meu Nível - 1
      IF ( it_t059-nivel <> vg_nivel ).
        "Se não for o Nivel Acnima
        vg_nivel = it_t058-nivel + 1. "Meu Nível - 1
        IF ( it_t059-nivel <> vg_nivel ).
          CONTINUE.
          "Se for o registro nivel acima e não estiver recusado
        ELSEIF it_t059-nivel = vg_nivel AND it_t059-ck_recusa EQ 'N'.
          CONTINUE.
        ENDIF.
      ENDIF.

      it_zglt043_alv-icone_aprova       = icon_set_state.
      it_zglt043_alv-icone_recusa       = icon_terminated_task.
    ENDIF.

    READ TABLE it_depa WITH KEY dep_resp = it_t041-dep_resp2.
    READ TABLE it_t001 WITH KEY bukrs    = it_zglt043-bukrs.

    MOVE-CORRESPONDING it_zglt043 TO it_zglt043_alv.
    it_zglt043_alv-icone              = icon_print.
    it_zglt043_alv-icone_view         = icon_display.
    it_zglt043_alv-dep_resp           = it_t041-dep_resp2.
    it_zglt043_alv-cta_monet          = it_t041-cta_monet.
    it_zglt043_alv-desc_dep_resp      = it_depa-dep_resp_desc.
    it_zglt043_alv-moeda_01           = it_t001-waers.
    it_zglt043_alv-butxt              = it_t001-butxt.
    it_zglt043_alv-land1              = it_t001-land1.
    it_zglt043_alv-nivel_liberador    = it_t058-nivel.

*    READ TABLE it_t059 WITH KEY bukrs    = it_zglt043-bukrs
*                                saknr    = it_zglt043-saknr
*                                monat    = it_zglt043-monat
*                                gjahr    = it_zglt043-gjahr
*                                dep_resp = it_t041-dep_resp2.
    "and NIVEL = 000. ->124594 CS2023000500 - Incluir coluna usuário responsável ZGL037 Parte 2 - PSA

    READ TABLE it_t059 WITH KEY bukrs    = it_zglt043-bukrs
     saknr    = it_zglt043-saknr
     monat    = it_zglt043-monat
     gjahr    = it_zglt043-gjahr
     nivel    = 000.

    IF sy-subrc IS INITIAL.
      it_zglt043_alv-usuario_reconc     = it_t059-bn_liberacao.
    ENDIF.

    READ TABLE it_skat WITH KEY ktopl = it_t001-ktopl saknr = it_zglt043-saknr.
    IF sy-subrc IS INITIAL.
      it_zglt043_alv-txt50 = it_skat-txt50.
    ENDIF.

    IF it_t041-bname2 IS NOT INITIAL.
      READ TABLE it_usrefus WITH KEY bname = it_t041-bname2.
      IF sy-subrc IS INITIAL.
        it_zglt043_alv-bname      = it_t041-bname2.
        it_zglt043_alv-desc_bname = it_usrefus-useralias.
      ENDIF.
    ENDIF.

    IF it_t041-cod_clas_bal  IS NOT INITIAL AND
       it_t041-cod_clas_not2 IS NOT INITIAL.
      READ TABLE it_notas WITH KEY codigo   = it_t041-cod_clas_bal
                                   cod_nota = it_t041-cod_clas_not2.
      IF sy-subrc IS INITIAL.
        it_zglt043_alv-codigo     = it_notas-codigo.
        it_zglt043_alv-descr      = it_notas-descr .
        it_zglt043_alv-cod_nota   = it_notas-cod_nota.
        it_zglt043_alv-descr_nota = it_notas-descr_nota.
      ENDIF.
    ENDIF.

    READ TABLE it_x001 WITH KEY bukrs = it_zglt043-bukrs.
    IF sy-subrc IS INITIAL.
      it_zglt043_alv-moeda_02 = it_x001-hwae2.
      it_zglt043_alv-moeda_03 = it_x001-hwae3.
    ENDIF.

    IF it_t001-land1 EQ 'BR'.
      CLEAR: it_zglt043_alv-moeda_03,
             it_zglt043_alv-sdo_mi3,
             it_zglt043_alv-sdo_rel_aux_mi3,
             it_zglt043_alv-tx_fech2.
    ENDIF.

    it_zglt043_alv-dif_mi1 = it_zglt043_alv-sdo_mi  - it_zglt043_alv-sdo_rel_aux_mi.
    it_zglt043_alv-dif_mi2 = it_zglt043_alv-sdo_mi2 - it_zglt043_alv-sdo_rel_aux_mi2.
    it_zglt043_alv-dif_mi3 = it_zglt043_alv-sdo_mi3 - it_zglt043_alv-sdo_rel_aux_mi3.

    TRY.
        lc_value = it_zglt043_alv-sdo_mi / it_zglt043_alv-sdo_mi2.
        IF lc_value GT 99999.
          it_zglt043_alv-tx_fech1 = 99999.
        ELSE.
          it_zglt043_alv-tx_fech1 = it_zglt043_alv-sdo_mi / it_zglt043_alv-sdo_mi2.
        ENDIF.
      CATCH cx_sy_zerodivide.
      CATCH cx_sy_arithmetic_overflow.
        MESSAGE i000 WITH 'Erro CX_SY_ARITHMETIC_OVERFLOW 1' it_zglt043-saknr it_zglt043_alv-txt50.
      CATCH cx_sy_conversion_overflow.
        MESSAGE i000 WITH 'Erro CX_SY_CONVERSION_OVERFLOW 1' it_zglt043-saknr it_zglt043_alv-txt50.
    ENDTRY.

    TRY.
        lc_value = it_zglt043_alv-sdo_mi / it_zglt043_alv-sdo_mi3.
        IF lc_value GT 99999.
          it_zglt043_alv-tx_fech2 = 99999.
        ELSE.
          it_zglt043_alv-tx_fech2 = it_zglt043_alv-sdo_mi / it_zglt043_alv-sdo_mi3.
        ENDIF.
      CATCH cx_sy_zerodivide.
      CATCH cx_sy_arithmetic_overflow.
        MESSAGE i000 WITH 'Erro CX_SY_ARITHMETIC_OVERFLOW 2' it_zglt043-saknr it_zglt043_alv-txt50.
      CATCH cx_sy_conversion_overflow.
        MESSAGE i000 WITH 'Erro CX_SY_CONVERSION_OVERFLOW 2' it_zglt043-saknr it_zglt043_alv-txt50.
    ENDTRY.

    it_zglt043_alv-rowcolor = 'C210'.
    APPEND it_zglt043_alv.
  ENDLOOP.

*------------

*  TYPES: BEGIN OF TY_DELETE,
*           BUKRS TYPE BUKRS.
*  TYPES: END OF TY_DELETE.
*
*  DATA: IT_DELETE TYPE TABLE OF TY_DELETE WITH HEADER LINE.
*
*  LOOP AT IT_ZGLT043_ALV.
*    IT_DELETE-BUKRS = IT_ZGLT043_ALV-BUKRS.
*    APPEND IT_DELETE.
*  ENDLOOP.
*
*  SORT  IT_DELETE BY BUKRS.
*  DELETE ADJACENT DUPLICATES FROM IT_DELETE COMPARING BUKRS.
*
*  LOOP AT IT_DELETE.
*    AUTHORITY-CHECK OBJECT 'ZFI_BUKRS'                        "Modificação 08.11.2016
*    ID 'BUKRS' FIELD IT_DELETE-BUKRS.                           "Modificação 08.11.2016
*    IF SY-SUBRC <> 0.                                         "Modificação 08.11.2016
*      DELETE IT_ZGLT043_ALV WHERE BUKRS EQ IT_DELETE-BUKRS.
*    ENDIF.
*  ENDLOOP.








  CLEAR: it_zglt043_alv.

ENDFORM.                    " Atualiza_lotes

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email TABLES tg_estra USING VALUE(wg_conta) TYPE ty_alv_zglt043 plinha.

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack           LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        objhead           LIKE solisti1   OCCURS  1 WITH HEADER LINE,
        objbin_ord        LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin_log        LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin            LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objtxt            LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        bjtxt             LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        reclist           LIKE somlreci1  OCCURS  5 WITH HEADER LINE,
        content_hex       TYPE STANDARD TABLE OF solix WITH HEADER LINE,
        it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE,
        objbin_ann        TYPE solisti1,
        objbin1           TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin         LIKE LINE OF objbin,
        doc_chng          LIKE sodocchgi1,
        tab_lines         LIKE sy-tabix,
        l_anex            TYPE string,
        l_leng            TYPE i,
        l_arq             TYPE string,
        l_tam             TYPE i,
        l_tam_ord         TYPE i,
        l_tam_log         TYPE i,
        l_email(300)      TYPE c,
        vlinha            TYPE i,
        vuser             TYPE sy-uname,
        content           TYPE string,
        wa_zglt041        TYPE zglt041,
        it_zglt058        TYPE TABLE OF zglt058 WITH HEADER LINE,
        wa_skat           TYPE skat,
        wa_t001           TYPE t001,
        wa_zimp_cad_depto TYPE zimp_cad_depto,
        e_x001            TYPE x001.

  DATA: bsmtp_addr TYPE adr6-smtp_addr.

  SELECT SINGLE * INTO wa_zglt041
    FROM zglt041
   WHERE bukrs EQ wg_conta-bukrs
     AND saknr EQ wg_conta-saknr.
*     AND GJAHR EQ WG_CONTA-GJAHR. "/Modificação CS2017000372

  CHECK sy-subrc IS INITIAL.

  SELECT *
    INTO TABLE it_zglt058
    FROM zglt058
   WHERE bukrs    EQ wa_zglt041-bukrs
     AND dep_resp EQ wa_zglt041-dep_resp2
   ORDER BY nivel.

  LOOP AT it_zglt058.

    SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
      FROM usr21
     INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
     WHERE usr21~bname = it_zglt058-bname.

*   Criação do documento de Email
    doc_chng-obj_name = 'LOG_RECONC'.

*   Assunto do Email
    doc_chng-obj_descr = TEXT-024. "'Aprovação de Reconciliação'.

    SELECT SINGLE * INTO wa_t001
      FROM t001
     WHERE bukrs EQ wg_conta-bukrs.

    SELECT SINGLE * INTO wa_skat
      FROM skat
     WHERE spras EQ sy-langu
       AND saknr EQ wg_conta-saknr
       AND ktopl EQ wa_t001-ktopl.

    SELECT SINGLE * INTO wa_zimp_cad_depto
      FROM zimp_cad_depto
     WHERE dep_resp EQ wa_zglt041-dep_resp2.

*   Texto
    objtxt-line = TEXT-025. "'Está disponível para aprovação no sistema SAP a Conta de Reconciliação'.
    APPEND objtxt.

    objtxt-line = ''. APPEND objtxt.
    objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
    APPEND objtxt.
    CONCATENATE TEXT-027 wg_conta-bukrs '-' wa_t001-butxt INTO objtxt-line SEPARATED BY space. "'Empresa:'
    APPEND objtxt.
    CONCATENATE TEXT-028 wa_zimp_cad_depto-dep_resp '-' wa_zimp_cad_depto-dep_resp_desc INTO objtxt-line SEPARATED BY space. "'Departamento:'
    APPEND objtxt.
    CONCATENATE TEXT-029 wg_conta-saknr '-' wa_skat-txt50 INTO objtxt-line SEPARATED BY space. "'Conta:'
    APPEND objtxt.
    CONCATENATE wg_conta-monat '-' wg_conta-gjahr INTO objtxt-line.
    CONCATENATE TEXT-030 objtxt-line INTO objtxt-line SEPARATED BY space. "'Mês-Ano:'
    APPEND objtxt.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs                = wg_conta-bukrs
      IMPORTING
        e_x001                 = e_x001
      EXCEPTIONS
        currency_2_not_defined = 1
        currency_3_not_defined = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA: ctotal(20),
          vdata(10).

    "1ª Moeda da Empresa
    WRITE wg_conta-sdo_mi TO ctotal CURRENCY wa_t001-waers.
    CONDENSE ctotal NO-GAPS.
    CONCATENATE TEXT-031 wa_t001-waers ctotal INTO objtxt-line SEPARATED BY space. "'1ª Moeda: '
    APPEND objtxt.

    "2ª Moeda da Empresa
    WRITE wg_conta-sdo_mi2 TO ctotal CURRENCY e_x001-hwae2.
    CONDENSE ctotal NO-GAPS.
    CONCATENATE TEXT-032 e_x001-hwae2 ctotal INTO objtxt-line SEPARATED BY space. "'2ª Moeda: '
    APPEND objtxt.

    "Somente 3º moeda em paises diferente de Brasil
    IF wa_t001-land1 NE 'BR'.
      "3ª Moeda da Empresa
      WRITE wg_conta-sdo_mi3 TO ctotal CURRENCY e_x001-hwae3.
      CONDENSE ctotal NO-GAPS.
      CONCATENATE TEXT-033 e_x001-hwae3 ctotal INTO objtxt-line SEPARATED BY space. "'3ª Moeda: '
      APPEND objtxt.
    ENDIF.
    objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
    APPEND objtxt.

    objtxt-line = ''. APPEND objtxt.
    objtxt-line = TEXT-034. "'Para aprovar clique no link "Estratégia" em anexo.' .
    APPEND objtxt.
    objtxt-line = TEXT-026. " '-------------------------------------------------------------------------------------------------------' .
    APPEND objtxt.
    CLEAR objtxt.

*   Setar tamanho da mensagem
    DESCRIBE TABLE objtxt LINES tab_lines.
    READ TABLE objtxt INDEX tab_lines.
    doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

*   Criar entrada de documento comprimido
    CLEAR objpack-transf_bin.
    objpack-head_start = 1.
    objpack-head_num   = 0.
    objpack-body_start = 1.
    objpack-body_num   = tab_lines.
    objpack-doc_type   = 'RAW'.
    APPEND objpack.

    CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
      EXPORTING
        recipient_user_id = it_zglt058-bname
        transaction       = 'ZGL037'
      IMPORTING
        content           = content
      TABLES
        shortcut_param    = it_shortcut_param.

    CLEAR : tab_lines, objbin.
    CONCATENATE content wa_objbin-line INTO wa_objbin-line.
    APPEND  wa_objbin TO objbin.

    DESCRIBE TABLE objbin LINES tab_lines.
    objhead = 'ESTRATEGIA.SAP'.
    APPEND objhead.

*  * Creation of the entry for the compressed attachment
    objpack-transf_bin = 'X'.
    objpack-head_start = 1.
    objpack-head_num   = 1.
    objpack-body_start = 1.
    objpack-body_num   = tab_lines.
    objpack-doc_type   = 'EXT'." SAP
    objpack-obj_name   = 'SAPSHORTCUTMAIL'.
    objpack-obj_descr  = 'ESTRATEGIA.SAP'.
    objpack-doc_size   = tab_lines * 255.
    APPEND objpack.

*   Alimentar destinatários do email
    IF bsmtp_addr IS INITIAL.
      MESSAGE TEXT-035 TYPE 'I'. "'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.'
      EXIT.
    ENDIF.

    reclist-receiver = bsmtp_addr.
    reclist-rec_type = 'U'.                    "Define email externo
    APPEND reclist.

*   Enviar email
    vuser = sy-uname.
    sy-uname = 'R3JOB'.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
        object_header              = objhead
        contents_bin               = objbin
        contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
        receivers                  = reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sy-uname = vuser.
  ENDLOOP.


ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Form  PRINT_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGLT043_ALV  text
*----------------------------------------------------------------------*
FORM print_doc  USING  wl_043 TYPE ty_alv_zglt043.

  DATA: vl_form      TYPE tdsfname,
        vl_name      TYPE rs38l_fnam,
        i_zglt043    TYPE zglt043,
        it_zglt044   TYPE TABLE OF zglt044 WITH HEADER LINE,
        i_zglt044    TYPE zglt044_t,
        it_zglt045   TYPE TABLE OF zglt045 WITH HEADER LINE,
        tl_tlines    LIKE tline OCCURS 0 WITH HEADER LINE,
        i_zglt045    TYPE zglt045_t,
        i_gravado(1).

  vl_form   = 'ZGLS0001'.
  i_gravado = 'S'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      i_bukrs          = wl_043-bukrs
      i_saknr          = wl_043-saknr
      i_monat          = wl_043-monat
      i_gjahr          = wl_043-gjahr
      i_zglt043        = i_zglt043
      i_gravado        = i_gravado
    TABLES
      i_zglt044        = i_zglt044
      i_zglt045        = i_zglt045
      i_tlines         = tl_tlines
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " PRINT_DOC

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  CLEAR: lbl_fecha1, lbl_fecha2, lbl_fecha3, lbl_fecha4.

  IF manager IS NOT INITIAL.
    CALL METHOD manager->unpublish.
    CLEAR: manager.
  ENDIF.

  IF it_zglt043_alv IS NOT INITIAL.

    CLEAR obj.
    obj-objtype = objtype.
    CONCATENATE it_zglt043_alv-monat it_zglt043_alv-gjahr it_zglt043_alv-bukrs it_zglt043_alv-saknr INTO obj-objkey.

    CREATE OBJECT manager
      EXPORTING
        is_object        = obj
        ip_no_commit     = 'R'
        ip_mode          = 'D'
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF it_zglt043_alv-cta_monet EQ 'N'.
      lbl_fecha1 = TEXT-036. "'Indice Hist. 1ª/2ª'.

      IF it_zglt043_alv-land1 NE 'BR'.
        lbl_fecha2 = TEXT-037. "'Indice Hist. 1ª/3ª'.
        LOOP AT SCREEN.
          IF screen-name EQ 'LBL_FECHA3' OR screen-name EQ 'LBL_FECHA4' OR
             screen-name EQ 'IT_ZGLT043_ALV-TX_FECH3' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH4'.
            screen-invisible = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT SCREEN.
          IF screen-name EQ 'LBL_FECHA2' OR screen-name EQ 'LBL_FECHA3' OR screen-name EQ 'LBL_FECHA4' OR
             screen-name EQ 'IT_ZGLT043_ALV-TX_FECH2' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH3' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH4'.
            screen-invisible = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSEIF it_zglt043_alv-cta_monet EQ 'S'.
      lbl_fecha1 = TEXT-036. "'Indice Pard. 1ª/2ª'.
      lbl_fecha2 = TEXT-036. "'Indice Fech. 1ª/2ª'.

      IF it_zglt043_alv-land1 NE 'BR'.
        lbl_fecha3 = TEXT-037. "'Indice Pard. 1ª/3ª'.
        lbl_fecha4 = TEXT-037. "'Indice Fech. 1ª/3ª'.
      ELSE.
        LOOP AT SCREEN.
          IF screen-name EQ 'LBL_FECHA3' OR screen-name EQ 'LBL_FECHA4' OR
             screen-name EQ 'IT_ZGLT043_ALV-TX_FECH3' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH4'.
            screen-invisible = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      LOOP AT SCREEN.
        IF screen-name EQ 'LBL_FECHA1' OR screen-name EQ 'LBL_FECHA2' OR
           screen-name EQ 'LBL_FECHA3' OR screen-name EQ 'LBL_FECHA4' OR
           screen-name EQ 'IT_ZGLT043_ALV-TX_FECH1' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH2' OR
           screen-name EQ 'IT_ZGLT043_ALV-TX_FECH3' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH4'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name EQ 'LBL_FECHA1' OR screen-name EQ 'LBL_FECHA2' OR
         screen-name EQ 'LBL_FECHA3' OR screen-name EQ 'LBL_FECHA4' OR
         screen-name EQ 'IT_ZGLT043_ALV-TX_FECH1' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH2' OR
         screen-name EQ 'IT_ZGLT043_ALV-TX_FECH3' OR screen-name EQ 'IT_ZGLT043_ALV-TX_FECH4'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_LOG_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_043  text
*----------------------------------------------------------------------*
FORM registrar_log_liberacao  USING p_043  TYPE zglt043
                                    status TYPE zfied022
                                    nivel  TYPE ze_nivel
                                    texto_tdobject TYPE tdobject
                                    texto_tdname   TYPE tdobname
                                    texto_tdid     TYPE tdid
                                    texto_tdspras  TYPE spras.
  DATA: wa_zglt059 TYPE zglt059,
        wa_zglt041 TYPE zglt041,
        wa_aux     TYPE zglt059.

  SELECT SINGLE * INTO wa_zglt041
    FROM zglt041
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr.
*     AND GJAHR EQ P_043-GJAHR.  "/Modificação CS2017000372

  IF status NE 'B'.
    SELECT SINGLE * INTO wa_zglt059
      FROM zglt059
     WHERE bukrs EQ p_043-bukrs
       AND saknr EQ p_043-saknr
       AND monat EQ p_043-monat
       AND gjahr EQ p_043-gjahr
       AND ck_ultimo_log EQ 'S'.

    APPEND wa_zglt059 TO it_zglt059.

    wa_zglt059-bukrs          = p_043-bukrs.
    wa_zglt059-saknr          = p_043-saknr.
    wa_zglt059-monat          = p_043-monat.
    wa_zglt059-gjahr          = p_043-gjahr.
    wa_zglt059-dep_resp       = wa_zglt041-dep_resp2.
    wa_zglt059-dt_liberacao   = sy-datum.
    wa_zglt059-hr_liberacao   = sy-uzeit.
    wa_zglt059-bn_liberacao   = sy-uname.
    wa_zglt059-status_lib     = status.
    wa_zglt059-nivel          = nivel.
    wa_zglt059-ck_ultimo_log  = 'S'.

    IF status EQ 'L'.
      wa_zglt059-ck_recusa  = 'N'.
    ELSEIF status EQ 'R'.
      wa_zglt059-ck_recusa      = 'S'.
      wa_zglt059-texto_tdobject = texto_tdobject.
      wa_zglt059-texto_tdname   = texto_tdname.
      wa_zglt059-texto_tdid     = texto_tdid.
      wa_zglt059-texto_tdspras  = texto_tdspras.
    ENDIF.

    UPDATE zglt059
       SET ck_ultimo_log = 'N'
     WHERE bukrs EQ p_043-bukrs
       AND saknr EQ p_043-saknr
       AND monat EQ p_043-monat
       AND gjahr EQ p_043-gjahr.

    MODIFY zglt059 FROM wa_zglt059.

    PERFORM verifica_aprovacao_recusa USING status p_043 wa_zglt059 nivel.

  ELSE.

    READ TABLE it_zglt059 INTO wa_zglt059
          WITH KEY bukrs = p_043-bukrs
                   saknr = p_043-saknr
                   monat = p_043-monat
                   gjahr = p_043-gjahr.

    IF sy-subrc IS INITIAL.
      CLEAR: wa_aux.
      SELECT SINGLE * INTO wa_aux
        FROM zglt059
       WHERE bukrs         EQ p_043-bukrs
         AND saknr         EQ p_043-saknr
         AND monat         EQ p_043-monat
         AND gjahr         EQ p_043-gjahr
         AND ck_ultimo_log EQ 'S'.

      IF wa_aux-texto_tdobject IS NOT INITIAL.
        CALL FUNCTION 'DELETE_TEXT'
          EXPORTING
            id        = wa_aux-texto_tdid
            language  = wa_aux-texto_tdspras
            name      = wa_aux-texto_tdname
            object    = wa_aux-texto_tdobject
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
      ENDIF.

      DELETE FROM zglt059
       WHERE bukrs         EQ p_043-bukrs
         AND saknr         EQ p_043-saknr
         AND monat         EQ p_043-monat
         AND gjahr         EQ p_043-gjahr
         AND ck_ultimo_log EQ 'S'.

      MODIFY zglt059 FROM wa_zglt059.

      DELETE it_zglt059
       WHERE bukrs EQ p_043-bukrs
         AND saknr EQ p_043-saknr
         AND monat EQ p_043-monat
         AND gjahr EQ p_043-gjahr.

      PERFORM verifica_aprovacao_recusa USING status p_043 wa_zglt059 nivel.
    ENDIF.

  ENDIF.

ENDFORM.                    " REGISTAR_LOG_LIBERACAO

*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_refresh .

  IF it_zglt059[] IS INITIAL AND it_zglt043_reg[] IS INITIAL.
    PERFORM atualiza_contas_reconciliadas.
    CLEAR: it_zglt043_alv, wa_zglt043_alv.
  ELSE.
    MESSAGE s011.
  ENDIF.
ENDFORM.                    " F_REFRESH

*&---------------------------------------------------------------------*
*&      Form  APROVACAO
*&---------------------------------------------------------------------*
FORM aprovacao  USING  p_index TYPE sytabix CHANGING wl_043 TYPE ty_alv_zglt043 .

  DATA: wa_043         TYPE zglt043,
        p_index_aux    TYPE sytabix,
        texto_tdobject TYPE tdobject,
        texto_tdname   TYPE tdobname,
        texto_tdid     TYPE tdid,
        texto_tdspras	 TYPE spras.

  MOVE-CORRESPONDING wl_043 TO wa_043.

  p_index_aux = p_index.

  "Aprovação
  IF wl_043-icone_aprova EQ icon_set_state.
    PERFORM registrar_log_liberacao USING wa_043 'L' wl_043-nivel_liberador  texto_tdobject texto_tdname texto_tdid texto_tdspras.
    wl_043-icone_aprova = icon_system_undo.
    wl_043-icone_recusa = icon_led_yellow.
    MODIFY it_zglt043_alv FROM wl_043 INDEX p_index_aux TRANSPORTING icone_aprova icone_recusa.
  ELSEIF wl_043-icone_aprova EQ icon_system_undo.
    "Retornar para Não Aprovado
    PERFORM registrar_log_liberacao USING wa_043 'B' wl_043-nivel_liberador  texto_tdobject texto_tdname texto_tdid texto_tdspras.
    wl_043-icone_aprova = icon_set_state.
    wl_043-icone_recusa = icon_terminated_task.
    MODIFY it_zglt043_alv FROM wl_043 INDEX p_index_aux TRANSPORTING icone_aprova icone_recusa.
  ENDIF.

ENDFORM.                    " APROVACAO


*&---------------------------------------------------------------------*
*&      Form  RECUSAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGLT043_ALV  text
*      -->P_SY_TABIX  text
*----------------------------------------------------------------------*
FORM recusar  USING p_index TYPE sytabix CHANGING wl_043 TYPE ty_alv_zglt043 .

  DATA: wa_043         TYPE zglt043,
        p_index_aux    TYPE sytabix,
        texto_tdobject TYPE tdobject,
        texto_tdname   TYPE tdobname,
        texto_tdid     TYPE tdid,
        texto_tdspras	 TYPE spras.

  MOVE-CORRESPONDING wl_043 TO wa_043.

  p_index_aux = p_index.

  CLEAR: texto_tdobject, texto_tdname, texto_tdid, texto_tdspras.

  "Aprovação
  IF wl_043-icone_recusa EQ icon_terminated_task.
    PERFORM informar_motivo USING wl_043 CHANGING sy-subrc texto_tdobject texto_tdname texto_tdid texto_tdspras.
    IF sy-subrc IS INITIAL.
      PERFORM registrar_log_liberacao USING wa_043 'R' wl_043-nivel_liberador texto_tdobject texto_tdname texto_tdid texto_tdspras.
      wl_043-icone_aprova = icon_led_yellow.
      wl_043-icone_recusa = icon_system_undo.
      MODIFY it_zglt043_alv FROM wl_043 INDEX p_index_aux TRANSPORTING icone_aprova icone_recusa.
    ENDIF.
  ELSEIF wl_043-icone_recusa EQ icon_system_undo.
    "Retornar para Não Aprovado
    PERFORM registrar_log_liberacao USING wa_043 'B' wl_043-nivel_liberador texto_tdobject texto_tdname texto_tdid texto_tdspras.
    IF wa_043-status_lib NE 'A'.
      wl_043-icone_aprova = icon_set_state.
    ELSE.
      CLEAR: wl_043-icone_aprova.
    ENDIF.
    wl_043-icone_recusa = icon_terminated_task.
    MODIFY it_zglt043_alv FROM wl_043 INDEX p_index_aux TRANSPORTING icone_aprova icone_recusa.
  ENDIF.

ENDFORM.                    " RECUSAR

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_APROVACAO_RECUSA
*&---------------------------------------------------------------------*
*       Ajuste de Status de Reconcilização Último Nível/Primeiro Nível
*----------------------------------------------------------------------*
FORM verifica_aprovacao_recusa  USING p_status TYPE zfied022
                                      p_043    TYPE zglt043
                                      p_059    TYPE zglt059
                                      p_nivel  TYPE ze_nivel.

  DATA: wa_041   TYPE zglt041,
        mx_nivel TYPE ze_nivel,
        mi_nivel TYPE ze_nivel.

  "Buscar depatamento responsável
  SELECT SINGLE * INTO wa_041
    FROM zglt041
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr.
*     AND GJAHR EQ P_043-GJAHR.  "/Modificação CS2017000372

  CHECK sy-subrc IS INITIAL.

  "MX_NIVEL = P_059-NIVEL -- Último Nível
  SELECT MAX( nivel ) INTO mx_nivel
    FROM zglt058
   WHERE bukrs    EQ wa_041-bukrs
     AND dep_resp EQ wa_041-dep_resp2.

  "MX_NIVEL = P_059-NIVEL -- Último Nível
  SELECT MIN( nivel ) INTO mi_nivel
    FROM zglt058
   WHERE bukrs    EQ wa_041-bukrs
     AND dep_resp EQ wa_041-dep_resp2.

  IF mx_nivel EQ p_nivel.

    CASE p_status.
      WHEN 'B'.
        "Retornar para Status Original
        READ TABLE it_zglt043_reg WITH KEY monat = p_043-monat
                                           gjahr = p_043-gjahr
                                           bukrs = p_043-bukrs
                                           saknr = p_043-saknr.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING it_zglt043_reg TO p_043.
          MODIFY zglt043 FROM p_043.
          DELETE it_zglt043_reg
           WHERE monat EQ p_043-monat
             AND gjahr EQ p_043-gjahr
             AND bukrs EQ p_043-bukrs
             AND saknr EQ p_043-saknr.
        ENDIF.
      WHEN 'R'.
        "Usuário Recusando
        IF mi_nivel EQ mx_nivel.
          APPEND p_043 TO it_zglt043_reg.
          p_043-status_lib = 'R'.
          MODIFY zglt043 FROM p_043.
        ELSE.
          APPEND p_043 TO it_zglt043_reg.
          p_043-status_lib = 'L'.
          MODIFY zglt043 FROM p_043.
        ENDIF.
      WHEN 'L'.
        "Usuário Liberando
        APPEND p_043 TO it_zglt043_reg.
        p_043-status_lib = 'A'.
        MODIFY zglt043 FROM p_043.
    ENDCASE.

  ELSEIF ( mi_nivel EQ p_nivel ) AND ( mi_nivel NE mx_nivel ).
    CASE p_status.
      WHEN 'B'.
        "Retornar para Status Original
        READ TABLE it_zglt043_reg WITH KEY monat = p_043-monat
                                           gjahr = p_043-gjahr
                                           bukrs = p_043-bukrs
                                           saknr = p_043-saknr.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING it_zglt043_reg TO p_043.
          MODIFY zglt043 FROM p_043.
          DELETE it_zglt043_reg
           WHERE monat EQ p_043-monat
             AND gjahr EQ p_043-gjahr
             AND bukrs EQ p_043-bukrs
             AND saknr EQ p_043-saknr.
        ENDIF.
      WHEN 'R'.
        "Usuário Recusando
        APPEND p_043 TO it_zglt043_reg.
        p_043-status_lib = 'R'.
        MODIFY zglt043 FROM p_043.
    ENDCASE.
  ENDIF.

*' '  Não Iniciado
*'P'  Aguardando liberação
*'L'  Liberado
*'A'  Aprovado
*'R'  Rejeitado

ENDFORM.                    " VERIFICA_APROVACAO_RECUSA

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       Envio de e-mail de Fluxo de Aprovações/Recusas
*----------------------------------------------------------------------*
FORM enviar_email .

  DATA: it_043        TYPE TABLE OF zglt043 WITH HEADER LINE,
        it_043_aux    TYPE TABLE OF zglt043 WITH HEADER LINE,
        it_041        TYPE TABLE OF zglt041 WITH HEADER LINE,
        it_dep        TYPE TABLE OF zimp_cad_depto WITH HEADER LINE,
        it_058        TYPE TABLE OF zglt058 WITH HEADER LINE,
        it_logs       TYPE TABLE OF zglt059 WITH HEADER LINE,
        it_logs_aux   TYPE TABLE OF zglt059 WITH HEADER LINE,
        it_log_recusa TYPE TABLE OF zglt059 WITH HEADER LINE,
        it_log_libera TYPE TABLE OF zglt059 WITH HEADER LINE,
        wa_logs       TYPE zglt059.

  SELECT * INTO TABLE it_dep FROM zimp_cad_depto.

  IF it_zglt059[] IS NOT INITIAL.
    SELECT * INTO TABLE it_logs
      FROM zglt059
       FOR ALL ENTRIES IN it_zglt059
     WHERE bukrs EQ it_zglt059-bukrs
       AND saknr EQ it_zglt059-saknr
       AND monat EQ it_zglt059-monat
       AND gjahr EQ it_zglt059-gjahr
       AND nivel EQ 000
       AND status_lib EQ 'L'.

    "Busca ultimo log de contas alteradas e recusadas
    SELECT * INTO TABLE it_log_recusa
      FROM zglt059
       FOR ALL ENTRIES IN it_zglt059
     WHERE bukrs EQ it_zglt059-bukrs
       AND saknr EQ it_zglt059-saknr
       AND monat EQ it_zglt059-monat
       AND gjahr EQ it_zglt059-gjahr
       AND nivel         GE 1
       AND status_lib    EQ 'R'
       AND ck_recusa     EQ 'S'
       AND ck_ultimo_log EQ 'S'.

    "Busca ultimo log de contas alteradas e recusadas
    SELECT * INTO TABLE it_log_libera
      FROM zglt059
       FOR ALL ENTRIES IN it_zglt059
     WHERE bukrs EQ it_zglt059-bukrs
       AND saknr EQ it_zglt059-saknr
       AND monat EQ it_zglt059-monat
       AND gjahr EQ it_zglt059-gjahr
       AND nivel         GE 1
       AND status_lib    EQ 'L'
       AND ck_ultimo_log EQ 'S'.

    "Envia E-mail de Contas Recusadas dentro da Estratégia de Liberação,
    "porem ainda não foram recusadas para retorno a reconcialiação (zgl026)
    LOOP AT it_log_recusa.
      PERFORM envia_email_log_recusa USING it_log_recusa.
    ENDLOOP.

    SELECT * INTO TABLE it_043
      FROM zglt043
       FOR ALL ENTRIES IN it_zglt059
     WHERE monat EQ it_zglt059-monat
       AND gjahr EQ it_zglt059-gjahr
       AND bukrs EQ it_zglt059-bukrs
       AND saknr EQ it_zglt059-saknr.

    IF sy-subrc IS INITIAL.

      SELECT * INTO TABLE it_041
        FROM zglt041
         FOR ALL ENTRIES IN it_zglt059
       WHERE bukrs EQ it_zglt059-bukrs
         AND saknr EQ it_zglt059-saknr.
*         AND GJAHR EQ IT_ZGLT059-GJAHR. "/Modificação CS2017000372

      IF ( sy-subrc IS INITIAL ) AND ( it_log_libera[] IS NOT INITIAL ).

        LOOP AT it_log_libera.
          it_log_libera-nivel = it_log_libera-nivel + 1.
          MODIFY it_log_libera INDEX sy-tabix TRANSPORTING nivel.
        ENDLOOP.

        "Aprovadores dos departamentos
        SELECT * INTO TABLE it_058
          FROM zglt058
           FOR ALL ENTRIES IN it_log_libera
         WHERE bukrs    EQ it_log_libera-bukrs
           AND dep_resp EQ it_log_libera-dep_resp
           AND nivel    EQ it_log_libera-nivel.

        "Departamento
        LOOP AT it_dep.

          "Envio de E-mail de Aprovações
          "Aprovadores da Empresa e do Departamento
          LOOP AT it_058 WHERE dep_resp EQ it_dep-dep_resp.
            CLEAR: it_043_aux[], it_logs_aux[].

            "Contas do Departamento
            LOOP AT it_041 WHERE dep_resp2 EQ it_dep-dep_resp AND bukrs EQ it_058-bukrs.

              "Contas Aprovadas da Empresa/Departamento do Aprovador
              LOOP AT it_043 WHERE status_lib EQ 'L'
                               AND bukrs      EQ it_041-bukrs
                               AND saknr      EQ it_041-saknr.
                APPEND it_043 TO it_043_aux.
              ENDLOOP.
            ENDLOOP.

            "Enviar e-mail p/ Responsável por Departamento
            IF it_043_aux[] IS NOT INITIAL.
              PERFORM enviar_email_responsavel TABLES it_043_aux it_dep USING it_058.
            ENDIF.

            "            "Contas Aprovadas da Empresa/Departamento do Aprovador
            "            LOOP AT IT_043 WHERE BUKRS      EQ IT_041-BUKRS
            "                             AND SAKNR      EQ IT_041-SAKNR.
            "              PERFORM ENVIA_EMAIL_RECUSA TABLES IT_LOGS USING IT_043 IT_DEP.
            "            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: it_zglt043_reg[], it_zglt059[], it_zglt043_alv, wa_zglt043_alv, it_zglt043_alv[], it_log_libera[].
  COMMIT WORK.

  PERFORM atualiza_contas_reconciliadas.

ENDFORM.                    " ENVIAR_EMAIL

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL_RESPONSÁVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_043_AUX  text
*      -->P_IT_058  text
*----------------------------------------------------------------------*
FORM enviar_email_responsavel  TABLES t_043 STRUCTURE zglt043
                                      t_dep STRUCTURE zimp_cad_depto
                               USING  p_058 TYPE zglt058.
  "T_043 - Contas Aprovadas
  "P_058 - Usuário Envio e-mail.

  DATA: bsmtp_addr TYPE adr6-smtp_addr,
        reclist    LIKE somlreci1  OCCURS  5 WITH HEADER LINE,
        objpack    LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        doc_chng   LIKE sodocchgi1,
        it_skat    TYPE TABLE OF skat WITH HEADER LINE,
        objtxt     LIKE solisti1 OCCURS 10 WITH HEADER LINE,
        tab_lines  TYPE i,
        lc_043     TYPE zglt043,
        lc_dep     TYPE zimp_cad_depto,
        tl_t001    TYPE TABLE OF t001 WITH HEADER LINE,
        vuser      TYPE sy-uname.

  READ TABLE t_dep INTO lc_dep WITH KEY dep_resp = p_058-dep_resp.

  "E-mail destinatário
  SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
    FROM usr21
   INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
   WHERE usr21~bname = p_058-bname.

  CHECK bsmtp_addr IS NOT INITIAL.

  "Empresas
  SELECT * INTO TABLE tl_t001
    FROM t001
     FOR ALL ENTRIES IN t_043
   WHERE bukrs EQ t_043-bukrs.

  "Nome das Contas
  SELECT * INTO TABLE it_skat
    FROM skat
    FOR ALL ENTRIES IN t_043
   WHERE spras EQ sy-langu
     AND saknr EQ t_043-saknr.

* Criação do documento de Email
  doc_chng-obj_name = 'LOGRECONCA'.

* Assunto do Email
  doc_chng-obj_descr = TEXT-024. "'Aprovação de Reconciliação'.

* Texto
  objtxt-line = TEXT-038. "'Foram Aprovadas as Contas Relacionadas'.
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.
  objtxt-line = lc_dep-dep_resp_desc. APPEND objtxt.
  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  LOOP AT t_043 INTO lc_043.
    READ TABLE tl_t001 WITH KEY bukrs = lc_043-bukrs.
    READ TABLE it_skat WITH KEY ktopl = tl_t001-ktopl saknr = lc_043-saknr.
    CONCATENATE lc_043-monat '/' lc_043-gjahr INTO objtxt-line.
    CONCATENATE lc_043-bukrs objtxt-line lc_043-saknr '-' it_skat-txt50 INTO objtxt-line SEPARATED BY space.
    APPEND objtxt.
  ENDLOOP.
  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

  objtxt-line = TEXT-039. "'Para acompanhamento da Reconciliação acesse transação: ZGL025' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      contents_txt               = objtxt
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s012.
  ENDIF.

  sy-uname = vuser.

ENDFORM.                    " ENVIAR_EMAIL_RESPONSÁVEL

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL_RECUSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM envia_email_recusa  TABLES t_logs STRUCTURE zglt059
                         USING  p_043  TYPE zglt043
                                p_dep  TYPE zimp_cad_depto.

  DATA: bsmtp_addr  TYPE adr6-smtp_addr,
        reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE,
        objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        doc_chng    LIKE sodocchgi1,
        it_skat     TYPE TABLE OF skat WITH HEADER LINE,
        objtxt      LIKE solisti1 OCCURS 10 WITH HEADER LINE,
        tab_lines   TYPE i,
        lc_043      TYPE zglt043,
        lc_dep      TYPE zimp_cad_depto,
        tl_t001     TYPE TABLE OF t001 WITH HEADER LINE,
        wa_logs     TYPE zglt059,
        wa_logs_aux TYPE zglt059,
        wa_aux      TYPE zglt059,
        vuser       TYPE sy-uname,
        tl_tlines   LIKE tline OCCURS 0 WITH HEADER LINE.

  LOOP AT t_logs INTO wa_logs
     WHERE bukrs    EQ p_043-bukrs
       AND saknr    EQ p_043-saknr
       AND monat    EQ p_043-monat
       AND gjahr    EQ p_043-gjahr
       AND dep_resp EQ p_dep-dep_resp.
    IF ( wa_logs-nivel EQ 0 ) AND ( wa_logs-status_lib EQ 'L' ) AND ( ( wa_logs_aux-dt_liberacao LT wa_logs-dt_liberacao ) OR
         ( wa_logs_aux-dt_liberacao EQ wa_logs-dt_liberacao AND
           wa_logs_aux-hr_liberacao LT wa_logs-hr_liberacao ) ).
      MOVE wa_logs TO wa_logs_aux.
    ENDIF.
  ENDLOOP.

  CHECK wa_logs_aux IS NOT INITIAL.

  "E-mail destinatário
  SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
    FROM usr21
   INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
   WHERE usr21~bname = wa_logs_aux-bn_liberacao.

  CHECK bsmtp_addr IS NOT INITIAL.



* Criação do documento de Email
  doc_chng-obj_name = 'LOGRECONCA'.

* Assunto do Email
  doc_chng-obj_descr = TEXT-040. "'Recusa de Reconciliação'.

* Texto
  objtxt-line = TEXT-041. "'Foi Recusada a Conta Relacionada'.
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.
  objtxt-line = p_dep-dep_resp_desc. APPEND objtxt.
  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.

  READ TABLE tl_t001 WITH KEY bukrs = p_043-bukrs.
  READ TABLE it_skat WITH KEY ktopl = tl_t001-ktopl saknr = p_043-saknr.
  CONCATENATE p_043-monat '/' p_043-gjahr INTO objtxt-line.
  CONCATENATE p_043-bukrs objtxt-line p_043-saknr '-' it_skat-txt50 INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.

  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

  objtxt-line = TEXT-042. "'Para ajustes em Reconciliação acesse transação: ZGL026' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

  SELECT SINGLE * INTO wa_aux
    FROM zglt059
   WHERE monat EQ p_043-monat
     AND gjahr EQ p_043-gjahr
     AND bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr
     AND ck_ultimo_log EQ 'S'.

  IF ( sy-subrc IS INITIAL ) AND ( wa_aux-texto_tdname IS NOT INITIAL ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = wa_aux-texto_tdid
        language                = wa_aux-texto_tdspras
        name                    = wa_aux-texto_tdname
        object                  = wa_aux-texto_tdobject
      TABLES
        lines                   = tl_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      objtxt-line = TEXT-043. "'Motivos de Recusa'.
      APPEND objtxt.
      objtxt-line = TEXT-026. "'-----------------------------------------------------------------------'.
      APPEND objtxt.
      LOOP AT tl_tlines.
        objtxt-line = tl_tlines-tdline. APPEND objtxt.
      ENDLOOP.
      objtxt-line = TEXT-026. "'-----------------------------------------------------------------------'.
      APPEND objtxt.
    ENDIF.

  ENDIF.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      contents_txt               = objtxt
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s012.
  ENDIF.

  sy-uname = vuser.

ENDFORM.                    " ENVIA_EMAIL_RECUSA

*&---------------------------------------------------------------------*
*&      Form  INFORMAR_MOTIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM informar_motivo  USING    wl_043         TYPE ty_alv_zglt043
                      CHANGING p_subrc        TYPE sysubrc
                               texto_tdobject TYPE tdobject
                               texto_tdname	  TYPE tdobname
                               texto_tdid	    TYPE tdid
                               texto_tdspras  TYPE spras.

  DATA: texto       TYPE sychar70,
        tl_texto    TYPE catsxt_longtext_itab,
        wl_texto    TYPE txline,
        tl_tlines   LIKE tline OCCURS 0 WITH HEADER LINE,
        wl_header   TYPE thead,
        wa_zglt059x TYPE zglt059.

  CLEAR: texto_tdobject,
         texto_tdname,
         texto_tdid,
         texto_tdspras.

  SELECT SINGLE * INTO wa_zglt059x
    FROM zglt059
   WHERE bukrs EQ wl_043-bukrs
     AND saknr EQ wl_043-saknr
     AND monat EQ wl_043-monat
     AND gjahr EQ wl_043-gjahr
     AND ck_ultimo_log EQ 'S'.

  IF wa_zglt059x-ck_recusa = 'S'  AND wa_zglt059x-status_lib = 'R'
    AND wa_zglt059x-texto_tdid     IS NOT INITIAL
    AND wa_zglt059x-texto_tdspras  IS NOT INITIAL
    AND wa_zglt059x-texto_tdname   IS NOT INITIAL
    AND wa_zglt059x-texto_tdobject IS NOT INITIAL.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = wa_zglt059x-texto_tdid
        language                = wa_zglt059x-texto_tdspras
        name                    = wa_zglt059x-texto_tdname
        object                  = wa_zglt059x-texto_tdobject
      TABLES
        lines                   = tl_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    LOOP AT tl_tlines.
      MOVE tl_tlines-tdline TO wl_texto.
      APPEND wl_texto TO tl_texto.
    ENDLOOP.

  ENDIF.

  CONCATENATE wl_043-gjahr '/' wl_043-monat '-' wl_043-bukrs '-' wl_043-saknr INTO texto.

  CONCATENATE TEXT-044 texto INTO texto SEPARATED BY space. "'Motivo de Recusa (Reconciliação) -'

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = texto
    CHANGING
      ch_text  = tl_texto.

  p_subrc = 1.

  IF sy-ucomm EQ 'CX_CONT'.
    IF tl_texto[] IS NOT INITIAL.
      LOOP AT tl_texto INTO wl_texto.
        MOVE: '*'      TO tl_tlines-tdformat,
              wl_texto TO tl_tlines-tdline.
        APPEND tl_tlines.
      ENDLOOP.

      CONCATENATE wl_043-gjahr wl_043-monat wl_043-bukrs wl_043-saknr sy-datum sy-uzeit sy-uname INTO wl_header-tdname.
      wl_header-tdobject = 'ZRECONCILI'.
      wl_header-tdid     = 'ZNEG'.
      wl_header-tdspras  = sy-langu.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header          = wl_header
          savemode_direct = 'X'
        TABLES
          lines           = tl_tlines
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.

      IF sy-subrc IS INITIAL.
        texto_tdobject = wl_header-tdobject.
        texto_tdname   = wl_header-tdname.
        texto_tdid     = wl_header-tdid.
        texto_tdspras  = wl_header-tdspras.
        p_subrc = 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " INFORMAR_MOTIVO


*&---------------------------------------------------------------------*
*&      Form  VIEW_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM view_doc  USING  wl_043 TYPE ty_alv_zglt043.

  RANGES: s_bukrs  FOR zglt042-empresa_de,
          s_depre  FOR zglt042-dep_resp,
          s_mes    FOR zglt042-mes_de,
          s_ano    FOR zglt042-ano_de,
          s_contas FOR zglt041-saknr.

  s_bukrs-sign   = 'I'.
  s_bukrs-option = 'EQ'.
  s_bukrs-low    = wl_043-bukrs.
  s_bukrs-high   = wl_043-bukrs.
  APPEND s_bukrs.

  s_depre-sign   = 'I'.
  s_depre-option = 'EQ'.
  s_depre-low    = wl_043-dep_resp.
  s_depre-high   = wl_043-dep_resp.
  APPEND s_depre.

  s_mes-sign   = 'I'.
  s_mes-option = 'EQ'.
  s_mes-low    = wl_043-monat.
  s_mes-high   = wl_043-monat.
  APPEND s_mes.

  s_ano-sign   = 'I'.
  s_ano-option = 'EQ'.
  s_ano-low    = wl_043-gjahr.
  s_ano-high   = wl_043-gjahr.
  APPEND s_ano.

  s_contas-sign   = 'I'.
  s_contas-option = 'EQ'.
  s_contas-low    = wl_043-saknr.
  s_contas-high   = wl_043-saknr.
  APPEND s_contas.

  SUBMIT zgl021 USING SELECTION-SCREEN '1000'
                 WITH s_bukrs  IN s_bukrs
                 WITH s_depre  IN s_depre
                 WITH s_mes    IN s_mes
                 WITH s_ano    IN s_ano
                 WITH s_contas IN s_contas
                 AND RETURN.

ENDFORM.                    " VIEW_DOC

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL_LOG_RECUSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LOG_RECUSA  text
*----------------------------------------------------------------------*
FORM envia_email_log_recusa  USING p_recusa TYPE zglt059.

  DATA: bsmtp_addr     TYPE ad_smtpadr,
        it_usuario     TYPE TABLE OF zglt058 WITH HEADER LINE,
        it_zglt059_rec TYPE TABLE OF zglt059 WITH HEADER LINE,
        wa_zglt059_rec TYPE zglt059,
        wa_usuario     TYPE zglt058,
        wa_t001        TYPE t001,
        wa_skat        TYPE skat,
        wa_departa     TYPE zimp_cad_depto,
        doc_chng       LIKE sodocchgi1,
        objtxt         LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        tl_tlines      LIKE tline OCCURS 0 WITH HEADER LINE,
        tab_lines      LIKE sy-tabix,
        objpack        LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        vuser          TYPE sy-uname,
        reclist        LIKE somlreci1  OCCURS  5 WITH HEADER LINE.

  p_recusa-nivel = p_recusa-nivel - 1.

  "Empresas
  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs EQ p_recusa-bukrs.

  "Nome das Contas
  SELECT SINGLE * INTO wa_skat
    FROM skat
   WHERE spras EQ sy-langu
     AND saknr EQ p_recusa-saknr.

  SELECT SINGLE * INTO wa_departa
    FROM zimp_cad_depto
   WHERE dep_resp EQ p_recusa-dep_resp.

* Criação do documento de Email
  doc_chng-obj_name = 'LOGRECONCA'.

* Assunto do Email
  doc_chng-obj_descr = TEXT-040. "'Recusa de Reconciliação'.

* Texto
  objtxt-line = TEXT-041. "'Foi Recusada a Conta Relacionada'.
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.
  objtxt-line = wa_departa-dep_resp_desc. APPEND objtxt.
  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.

  CONCATENATE p_recusa-monat '/' p_recusa-gjahr INTO objtxt-line.
  CONCATENATE p_recusa-bukrs objtxt-line p_recusa-saknr '-' wa_skat-txt50 INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.

  objtxt-line = TEXT-026. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

  objtxt-line = TEXT-042. "'Para ajustes em Reconciliação acesse transação: ZGL026' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

  IF ( sy-subrc IS INITIAL ) AND ( p_recusa-texto_tdname IS NOT INITIAL ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = p_recusa-texto_tdid
        language                = p_recusa-texto_tdspras
        name                    = p_recusa-texto_tdname
        object                  = p_recusa-texto_tdobject
      TABLES
        lines                   = tl_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      objtxt-line = TEXT-043. "'Motivos de Recusa'.
      APPEND objtxt.
      objtxt-line = TEXT-026. "'-----------------------------------------------------------------------'.
      APPEND objtxt.
      LOOP AT tl_tlines.
        objtxt-line = tl_tlines-tdline. APPEND objtxt.
      ENDLOOP.
      objtxt-line = TEXT-026. "'-----------------------------------------------------------------------'.
      APPEND objtxt.
    ENDIF.

  ENDIF.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  "--------- Modificação CS2017001300

  IF p_recusa-nivel IS INITIAL.

    SELECT *                                                  "--------- Modificação CS2017001300
      INTO CORRESPONDING FIELDS OF TABLE it_usuario
      FROM zglt062
     WHERE bukrs    EQ p_recusa-bukrs
       AND dep_resp EQ p_recusa-dep_resp.

  ELSE.

    SELECT *
      INTO TABLE it_usuario
      FROM zglt058
     WHERE bukrs    EQ p_recusa-bukrs
       AND dep_resp EQ p_recusa-dep_resp
       AND nivel    LE p_recusa-nivel.

  ENDIF.

*  SELECT *
*    INTO TABLE IT_USUARIO
*    FROM ZGLT058
*   WHERE BUKRS    EQ P_RECUSA-BUKRS
*     AND DEP_RESP EQ P_RECUSA-DEP_RESP
*     AND NIVEL    LE P_RECUSA-NIVEL.

  "Se não achar usuário pela estratégia, envia para ultimo reconciliante
  IF it_usuario IS NOT INITIAL.

    SELECT * INTO TABLE it_zglt059_rec
      FROM zglt059
     WHERE bukrs      EQ p_recusa-bukrs
       AND saknr      EQ p_recusa-saknr
       AND monat      EQ p_recusa-monat
       AND gjahr      EQ p_recusa-gjahr
       AND status_lib EQ 'L'
       AND nivel      EQ 0.

    "Usuário Reconciliante """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    CLEAR: wa_zglt059_rec.

    LOOP AT it_zglt059_rec.
      IF wa_zglt059_rec IS INITIAL.
        MOVE it_zglt059_rec TO wa_zglt059_rec.
      ELSEIF wa_zglt059_rec-dt_liberacao GT wa_zglt059_rec-dt_liberacao.
        MOVE it_zglt059_rec TO wa_zglt059_rec.
      ELSEIF it_zglt059_rec-dt_liberacao EQ wa_zglt059_rec-dt_liberacao AND it_zglt059_rec-hr_liberacao GT wa_zglt059_rec-hr_liberacao.
        MOVE it_zglt059_rec TO wa_zglt059_rec.
      ENDIF.
    ENDLOOP.

    IF wa_zglt059_rec IS NOT INITIAL.
      it_usuario-bname = wa_zglt059_rec-bn_liberacao.
      APPEND it_usuario.
    ENDIF.
    "Usuário Reconciliante """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  ENDIF.

  LOOP AT it_usuario INTO wa_usuario.

    CLEAR: reclist[], reclist, bsmtp_addr.

    "E-mail destinatário
    SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
      FROM usr21
     INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
     WHERE usr21~bname = wa_usuario-bname.

    IF bsmtp_addr IS INITIAL.
      CONTINUE.
    ENDIF.

    "Enviar email
    vuser = sy-uname.
    sy-uname = 'R3JOB'.

    reclist-receiver = bsmtp_addr.
    reclist-rec_type = 'U'. "Define email externo
    APPEND reclist.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
        contents_txt               = objtxt
        receivers                  = reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s012.
    ENDIF.

    sy-uname = vuser.

  ENDLOOP.


ENDFORM.                    " ENVIA_EMAIL_LOG_RECUSA
