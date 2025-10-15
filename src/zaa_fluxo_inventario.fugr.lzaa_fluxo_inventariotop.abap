FUNCTION-POOL zaa_fluxo_inventario.         "MESSAGE-ID ..

TYPE-POOLS : slis, icon, kkblo.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_sobra,
         anln1     TYPE anla-anln1,
         anln2     TYPE anla-anln2,
         gjahr     TYPE zaa005-gjahr,
         txt50     TYPE anla-txt50,
         txa50     TYPE anla-txa50,
         werks     TYPE anlz-werks,
         kostl     TYPE anlz-kostl,
         cc_sobra  TYPE zaa005-cc_sobra,
         invnr     TYPE anla-invnr,
         sernr     TYPE anla-sernr,
         stort     TYPE anlz-stort,
         raumn     TYPE anlz-raumn,
         kfzkz     TYPE anlz-kfzkz,
         aktiv     TYPE anla-aktiv,
         manual    TYPE zaa005-manual,
         duplic    TYPE c,
         celltab   TYPE lvc_t_styl,
         colorcell TYPE lvc_t_scol.
TYPES: END   OF ty_sobra.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: t_sobra             TYPE TABLE OF ty_sobra,
      w_sobra             TYPE ty_sobra,
      t_sobra_aux         TYPE TABLE OF ty_sobra,
      w_sobra_aux         TYPE ty_sobra,
      t_sobra_elim        TYPE TABLE OF ty_sobra,
      w_sobra_elim        TYPE ty_sobra,
      t_dados             TYPE TABLE OF anla,
      t_anla              TYPE TABLE OF anla,
      w_dados             TYPE anla,
      w_anla              TYPE anla,
      t_anlz              TYPE TABLE OF anlz,
      w_anlz              TYPE anlz,
      t_zaa005            TYPE TABLE OF zaa005,
      w_zaa005            TYPE zaa005,
      t_celltab           TYPE TABLE OF lvc_s_styl,
      w_celltab           TYPE lvc_s_styl,
      t_colorcell         TYPE TABLE OF lvc_s_scol,
      w_colorcell         TYPE lvc_s_scol,
*
      tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_gjahr             TYPE gjahr,
      g_container         TYPE scrfname VALUE 'CONTAINER',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button,
      ok_code             TYPE sy-ucomm,
      l_tabix             TYPE sy-tabix,
      l_cont              TYPE i,
      l_bukrs             TYPE bukrs,
      l_resp              TYPE c,
      l_tem_anla          TYPE sy-subrc,
      l_tem_zaa005        TYPE sy-subrc,
*
      t_fieldcatalog      TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab            TYPE slis_t_extab,
      w_exctab            TYPE slis_extab,
      w_item_layout       TYPE lvc_s_laci,
      w_layout            TYPE lvc_s_layo,
      ls_fieldcatalog     TYPE lvc_s_fcat,
      ls_exclude          TYPE ui_func,
      pt_exclude          TYPE ui_functions,
      pt_exclude2         TYPE ui_functions,
      t_del_rows          TYPE lvc_t_row,
      w_del_rows          TYPE lvc_s_row,
      t_sel_cols          TYPE lvc_t_col,
      w_sel_cols          TYPE lvc_s_col,
      l_row_id            TYPE lvc_s_row,
      l_column_id         TYPE lvc_s_col,
      l_stable            TYPE lvc_s_stbl,

      t_fcat_lvc          TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb          TYPE kkblo_t_fieldcat.

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zaaflux_toolbar_event_receiver.
INCLUDE zaaflux_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.
    FREE e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
    w_tool-text     = 'Novo'.
    w_tool-icon     = '@0Y@'.  "'@17@'.
    APPEND w_tool TO e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
    w_tool-text     = 'Deletar'.
    w_tool-icon     = '@18@'.
    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    DATA: l_tabix     TYPE sy-tabix,
          l_erro      TYPE c,
          t_sobra_aux TYPE TABLE OF ty_sobra.

    CASE e_ucomm.

      WHEN 'INSERT'.
        PERFORM f_ajusta_celltab.

        t_sobra_aux[] = t_sobra[].

        FREE t_sobra.

        CLEAR w_sobra.
        w_sobra-anln2   = '0000'.
        w_sobra-gjahr   = g_gjahr.
        w_sobra-aktiv   = sy-datum.
        APPEND w_sobra TO t_sobra.

        LOOP AT t_sobra_aux INTO w_sobra.
          w_sobra-anln2        = '0000'.
          w_sobra-gjahr        = g_gjahr.
          w_sobra-aktiv        = sy-datum.
          APPEND w_sobra      TO t_sobra.
        ENDLOOP.

        READ TABLE t_sobra INTO w_sobra INDEX 1.

        l_tabix = sy-tabix.

        PERFORM f_set_colunas        USING 4
                                  CHANGING w_sobra.

        w_sobra-manual    = abap_true.
        MODIFY t_sobra FROM w_sobra  INDEX l_tabix.

      WHEN 'DELETE'.
        PERFORM f_ajusta_celltab.

        FREE: t_del_rows.

        CALL METHOD g_grid->check_changed_data.
        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        CLEAR l_erro.
        LOOP AT t_del_rows   INTO w_del_rows.
          READ TABLE t_sobra INTO w_sobra INDEX w_del_rows-index.
          IF ( w_sobra-manual = abap_true )  OR
             ( w_sobra-manual = abap_false  AND
               w_sobra-duplic = abap_true ).
            l_erro = abap_false.
          ELSE.
            l_erro = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_erro = abap_true.
          MESSAGE i024(sd) WITH text-020.
          EXIT.
        ENDIF.

        IF t_del_rows[] IS NOT INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = text-001
              text_button_1         = text-002
              text_button_2         = text-003
              display_cancel_button = ''
              default_button        = '2'
            IMPORTING
              answer                = l_resp
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF l_resp = '1'.
            SORT t_del_rows BY index DESCENDING.

            LOOP AT t_del_rows   INTO w_del_rows.
              READ TABLE t_sobra INTO w_sobra INDEX w_del_rows-index.
              APPEND w_sobra       TO t_sobra_elim.
              DELETE t_sobra    INDEX w_del_rows-index.
            ENDLOOP.
          ENDIF.
        ENDIF.

    ENDCASE.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.
