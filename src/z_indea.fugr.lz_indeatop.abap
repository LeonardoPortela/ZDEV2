FUNCTION-POOL z_indea.                      "MESSAGE-ID ..

* INCLUDE LZ_INDEAD...                       " Local class definition

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: p_charg     TYPE zmmt0102-charg,
      p_nr_fase   TYPE zmmt0102-nr_fase,
      p_categoria TYPE zmmt0102-categoria,
      p_menge     TYPE zmmt0102-menge,
      p_tcode     TYPE sy-tcode,
      p_renas     TYPE atwrt,
      p_btn(1),
      p_erro(1),
      v_menge     TYPE zmmt0102-menge,
      wa_zmmt0102 TYPE zmmt0102,
      ok-code     TYPE  sy-ucomm.

*Class definition for ALV toolbar
CLASS:  lcl_alv_toolbar     DEFINITION DEFERRED.

DATA:  c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.
*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.

"ALV
DATA: grid1              TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_container        TYPE scrfname VALUE 'CC_LOTES',
      container_1        TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2        TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter           TYPE REF TO cl_gui_splitter_container,
      obg_toolbar        TYPE REF TO lcl_alv_toolbar.


DATA: wa_stable      TYPE lvc_s_stbl,
      wa_layout      TYPE lvc_s_layo,

      estrutura      TYPE TABLE OF ty_estrutura,
      wa_estrutura   TYPE ty_estrutura,

      t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      l_categoria    TYPE numc2,  "zmmt0102-categoria.  "*-CS2025000249-17.04.2025-#173311-JT
      l_descategoria TYPE char60. "                     "*-CS2025000249-17.04.2025-#173311-JT

DATA: BEGIN OF tg_lotes OCCURS 0,
        mark(1),
        charg            TYPE zmmt0102-charg,
        nr_fase          TYPE zmmt0102-nr_fase,
        categoria        TYPE zmmt0102-categoria,
        descategoria(20),
        menge            TYPE zmmt0102-menge,
        line_id          TYPE zmmt0102-line_id,
        coluna(1),
      END OF tg_lotes.

CONSTANTS: c_x               TYPE c VALUE 'X'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value.

    DATA: idd07v TYPE TABLE OF  dd07v.

    DATA  vdomvalue_l TYPE  dd07v-domvalue_l.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZCATEGSEMEN'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    LOOP AT er_data_changed->mt_good_cells
                     INTO ls_good
                     WHERE fieldname = 'CATEGORIA'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vdomvalue_l = lv_value.
      "
      READ TABLE idd07v INTO DATA(widd) WITH KEY domvalue_l = vdomvalue_l.
      lv_value = widd-ddtext.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCATEGORIA'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                     INTO ls_good
                     WHERE fieldname = 'MENGE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vdomvalue_l = lv_value.
      "
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MENGE'
          i_value     = lv_value.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed_finished.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

    wl_desactive = space.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 5.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      = icon_view_close.
    ty_toolbar-function  = 'SHOW_MSGRE'.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: tg_lotes_aux  LIKE TABLE OF tg_lotes,
          wg_lotes_aux  LIKE LINE OF tg_lotes,
          wcont         TYPE i,
          tl_index_rows TYPE lvc_t_row,
          wl_index_rows TYPE lvc_s_row.


    CASE e_ucomm.
      WHEN 'ADD'.

        tg_lotes_aux[] = tg_lotes[].
        REFRESH: tg_lotes.
        LOOP AT tg_lotes_aux INTO wg_lotes_aux.
          MOVE l_categoria     TO wg_lotes_aux-categoria.  "*-CS2025000249-17.04.2025-#173311-JT
          APPEND wg_lotes_aux  TO tg_lotes.
        ENDLOOP.

        CLEAR: wg_lotes_aux.
        DESCRIBE TABLE tg_lotes_aux LINES wcont.
        ADD 1 TO wcont.

        wg_lotes_aux-charg     = p_charg.
*-CS2025000249-17.04.2025-#173311-JT-inicio
        IF l_categoria IS NOT INITIAL.
          wg_lotes_aux-categoria    = l_categoria.
          wg_lotes_aux-descategoria = l_descategoria.
        ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim
        APPEND wg_lotes_aux   TO tg_lotes.
      WHEN 'DEL'.

        CALL METHOD grid1->get_selected_rows
          IMPORTING
            et_index_rows = tl_index_rows.

        LOOP AT tl_index_rows INTO wl_index_rows.
          READ TABLE tg_lotes INTO DATA(wlotes) INDEX wl_index_rows-index.
          DELETE FROM zmmt0102
             WHERE ebeln = wa_zmmt0102-ebeln
             AND   ebelp = wa_zmmt0102-ebelp
             AND   mblnr = ' '
             AND   charg = wlotes-charg
             AND   tcode = p_tcode.
          DELETE tg_lotes INDEX wl_index_rows-index.
        ENDLOOP.
    ENDCASE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok-code.
    WHEN 'CANCEL'
      OR 'EXIT'.
      REFRESH tg_lotes.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      CLEAR: p_erro, v_menge.
      LOOP AT  tg_lotes.
        ADD tg_lotes-menge TO v_menge.
        IF tg_lotes-nr_fase IS INITIAL.
          p_erro = 'X'.
          MESSAGE 'Informar a Fase!' TYPE 'I'.
          EXIT.
        ENDIF.
        IF tg_lotes-descategoria IS INITIAL.
          p_erro = 'X'.
          MESSAGE 'Informar a Categoria!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF p_erro = 'X'.
        EXIT.
      ENDIF.
      IF v_menge NE p_menge.
        MESSAGE 'Quantidade divergente!' TYPE 'I'.
        EXIT.
      ENDIF.
      "
      PERFORM f_grava.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
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
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_qtd(16).

  "

  IF grid1 IS NOT INITIAL.
    grid1->free( ).
    CLEAR: grid1.
  ENDIF.

  IF g_custom_container IS NOT INITIAL.
    g_custom_container->free( ).
    CLEAR: g_custom_container.
  ENDIF.

  IF g_custom_container IS INITIAL.
    WRITE p_menge TO wl_qtd.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = space.
    wa_layout-grid_title = | Quantidade total lote { p_charg  } : { wl_qtd } |.

    wa_stable-row        = c_x.
    wa_layout-box_fname  = 'MARK'.
    wa_layout-sel_mode   = 'A'.

    wa_layout-no_toolbar = space.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
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
*
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

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
        i_default            = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_lotes[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
     lcl_event_handler=>on_data_changed          FOR grid1,
     lcl_event_handler=>on_data_changed_finished FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
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
          1  'ZMMT0102'  'CHARG'         'TG_LOTES' 'CHARG'         'Lote'        '15' ' ' ' ' ' ',
          1  'ZMMT0102'  'NR_FASE'       'TG_LOTES' 'NR_FASE'       'Nr. Fase'    '25' 'X' ' ' ' ',
          1  'ZMMT0102'  'CATEGORIA'     'TG_LOTES' 'CATEGORIA'     'Categoria'   '10' ' ' ' ' ' ', "*-CS2025000249-17.04.2025-#173311-JT
          1  ' '         ' '             'TG_LOTES' 'DESCATEGORIA'  'Descrição'   '20' ' ' ' ' ' ',
          1  'ZMMT0102'  'MENGE'         'TG_LOTES' 'MENGE'         'Quantidade'  '15' 'X' 'X' ' ',
          1  ' '         ' '             'TG_LOTES' 'COLUNA'        ' '           '01' 'X' ' ' ' '.
ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

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

  IF p_field EQ 'CATEGORIA'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava.
  IF p_btn = ' '.
    DELETE FROM zmmt0102
             WHERE ebeln = wa_zmmt0102-ebeln
             AND   ebelp = wa_zmmt0102-ebelp
             AND   mblnr = ' '
             AND   charg = p_charg
             AND   tcode = p_tcode.
  ELSE.
    IF p_tcode = 'MIGO'.
      DELETE FROM zmmt0102
                WHERE ebeln   = wa_zmmt0102-ebeln
                AND   ebelp   = wa_zmmt0102-ebelp
                AND   line_id = wa_zmmt0102-line_id
                AND   mblnr   = ' '. "WA_ZMMT0102-MBLNR
*                AND   MJAHR   = WA_ZMMT0102-MJAHR.
    ELSE.
      DELETE FROM zmmt0102
           WHERE ebeln   = wa_zmmt0102-ebeln
           AND   ebelp   = wa_zmmt0102-ebelp
           AND   line_id = wa_zmmt0102-line_id "04.09.2020
           AND   charg   = wa_zmmt0102-charg
           AND   mblnr   = ' '. "WA_ZMMT0102-MBLNR

    ENDIF.
  ENDIF.
  "COMMIT WORK.
  "
  LOOP AT tg_lotes.
    wa_zmmt0102-charg     = tg_lotes-charg.
    wa_zmmt0102-nr_fase   = tg_lotes-nr_fase.
    wa_zmmt0102-categoria = tg_lotes-categoria.
    wa_zmmt0102-menge     = tg_lotes-menge.
    IF p_tcode = 'ZMM0110' AND tg_lotes-line_id IS NOT INITIAL.
      wa_zmmt0102-line_id   = tg_lotes-line_id.
    ENDIF.

    wa_zmmt0102-tcode     = p_tcode.
    MODIFY zmmt0102 FROM wa_zmmt0102.
  ENDLOOP.
  "COMMIT WORK.
  REFRESH tg_lotes.
  IF p_renas IS NOT INITIAL.
    PERFORM f_grava_renas.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_RENAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_renas .

  DATA: e_mch1    TYPE mch1,
        i_mcha    TYPE mcha,
        i_clbatch	TYPE clbatch.

  DATA: vobjecttable   TYPE bapi1003_key-objecttable,
        vclassnum      TYPE bapi1003_key-classnum,
        vclasstype     TYPE bapi1003_key-classtype,
        it_allocvalues TYPE TABLE OF  bapi1003_alloc_values_char,
        wa_allocvalues TYPE bapi1003_alloc_values_char,
        it_allocnum    TYPE TABLE OF  bapi1003_alloc_values_num,
        it_alloccur    TYPE TABLE OF  bapi1003_alloc_values_curr,
        it_return2     TYPE TABLE OF bapiret2,
        v_objek        TYPE inob-objek.

  vobjecttable = 'MCH1'.
  vclassnum = 'SEMENTES'.
  vclasstype = '023'.

  DATA  objcharg TYPE REF TO zcl_charg.
  CREATE OBJECT objcharg.

  TRY.
      CALL METHOD zcl_charg=>get_charg
        EXPORTING
          i_matnr = wa_zmmt0102-matnr
          i_charg = wa_zmmt0102-charg
        RECEIVING
          r_mch1  = DATA(r_mch1).
    CATCH zcx_charg_exception .
      i_mcha-matnr  = wa_zmmt0102-matnr.
      i_mcha-werks  = wa_zmmt0102-werks.
      i_mcha-charg  = wa_zmmt0102-charg.
      TRY.
          objcharg->set_mcha( i_mcha = i_mcha ).
          "Classe
          objcharg->set_class( i_class = 'SEMENTES' ).
          "Tipo de Classe
          objcharg->set_klart( i_klart = '023' ).
          objcharg->set_kzcla( i_kzcla = '2' ).
          objcharg->set_lgort( i_lgort =  wa_zmmt0102-lgort ).
          objcharg->set_object( i_object = CONV #( wa_zmmt0102-matnr ) ).
          objcharg->set_table( i_table = CONV #( 'MARA' ) ).
          objcharg->set_mcha( i_mcha = i_mcha ).
          TRY.
              DATA(r_mchb) = objcharg->criar_charg( IMPORTING e_mch1 = e_mch1 ).
            CATCH zcx_charg_exception .
          ENDTRY.
      ENDTRY.
  ENDTRY.

  CONCATENATE  wa_zmmt0102-matnr wa_zmmt0102-charg INTO v_objek.
  REFRESH it_allocvalues.
  wa_allocvalues-charact        = 'SEMENTE_RENASCEM'.
*--> Inicio Migração FP - 06/07/2023
*  wa_allocvalues-value_char     = p_renas.
*  wa_allocvalues-value_neutral  = p_renas.

  DATA(lv_tam) = strlen( p_renas ).

  IF lv_tam > 30.
    wa_allocvalues-value_char_long     = p_renas.
    wa_allocvalues-value_neutral_long  = p_renas.
  ELSE.
    wa_allocvalues-value_char     = p_renas.
    wa_allocvalues-value_neutral  = p_renas.
  ENDIF.

*<-- Inicio Migração FP - 06/07/2023
  wa_allocvalues-charact_descr  = 'RENASEM'.
  APPEND wa_allocvalues TO it_allocvalues.

* ---> S4 Migration - 21/06/2023 - MA
*  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
*    EXPORTING
*      objectkey          = v_objek
*      objecttable        = vobjecttable
*      classnum           = vclassnum
*      classtype          = vclasstype
*    TABLES
*      allocvaluesnumnew  = it_allocnum
*      allocvaluescharnew = it_allocvalues
*      allocvaluescurrnew = it_alloccur
*      return             = it_return2.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'   "#EC CI_USAGE_OK[2438131]
    EXPORTING
      objecttable        = vobjecttable
      classnum           = vclassnum
      classtype          = vclasstype
      objectkey_long     = v_objek
    TABLES
      allocvaluesnumnew  = it_allocnum
      allocvaluescharnew = it_allocvalues
      allocvaluescurrnew = it_alloccur
      return             = it_return2.
* <--- S4 Migration - 21/06/2023 - MA
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.
