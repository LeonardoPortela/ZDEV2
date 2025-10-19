*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_TOP
*&---------------------------------------------------------------------*


TYPE-POOLS icon.

TYPES: BEGIN OF ty_saida,
         finalidade       TYPE zsdt0359-finalidade,
         desc_finalidade  TYPE dd07d-DDtext,
         data_criacao     TYPE zsdt0359-data_criacao,
         hora_CRIACAO     TYPE zsdt0359-hora_criacao,
         usuario_criacao  TYPE zsdt0359-usuario_criacao,
         excluido         TYPE zsdt0359-excluido,
         data_exclusao    TYPE zsdt0359-data_exclusao,
         hora_EXCLUSAO    TYPE zsdt0359-hora_exclusao,
         usuario_EXCLUSAO TYPE zsdt0359-usuario_exclusao,
         celltab          TYPE lvc_t_styl,
       END OF ty_saida.

DATA: BEGIN OF tg_tp_doc OCCURS 0,
        blart TYPE t003-blart,
      END OF tg_tp_doc,

      BEGIN OF tg_tp_ped OCCURS 0,
        bsart TYPE t161-bsart,
      END OF tg_tp_ped,

      BEGIN OF tg_tp_ov OCCURS 0,
        auart TYPE tvak-auart,
      END OF tg_tp_ov.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida        TYPE TABLE OF ty_saida,
      it_saida_final  TYPE TABLE OF ty_saida,
      wa_saida        TYPE ty_saida,
      it_zsdt0359     TYPE TABLE OF zsdt0359,
      wa_zsdt0359     TYPE zsdt0359,
      wa_zsdt0359_aux TYPE zsdt0359.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: gt_f4    TYPE lvc_t_f4 WITH HEADER LINE,
      gv_erro  TYPE c,
      gv_modif TYPE c.

**---------------------------------------------------------------------*
**  Inicio Implementação Classes
**---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.                                           "
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.                "

  METHOD on_f4.

  ENDMETHOD.                    "ON_F4

ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION



CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_data_changed
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING et_good_cells.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.

    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'DEL'.
        PERFORM deletar_reg.
      WHEN 'DEL_TIPO_DOC'.
        PERFORM del_value USING 'TIPO_DOC'.
      WHEN 'DEL_TIPO_PED'.
        PERFORM del_value USING 'TIPO_PED'.
      WHEN 'DEL_TIPO_OV'.
        PERFORM del_value USING 'TIPO_OV'.
      WHEN 'DEL_BLOQ_PGTO'.
        PERFORM del_value USING 'BLOQ_PGTO'.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.

    DATA: ls_good    TYPE lvc_s_modi,
          lt_celltab TYPE lvc_t_styl,
          ls_style   TYPE lvc_s_styl,
          lv_lines   TYPE sy-tabix,
          lt_cell    TYPE TABLE OF lvc_s_ceno,
          lwa_cell   LIKE LINE OF lt_cell,
          lv_erro    TYPE c,
          lt_values  TYPE TABLE OF dd07v.

    IF et_good_cells IS NOT INITIAL.

      gv_modif = abap_true.

      DATA(lt_good) = et_good_cells.

      DELETE lt_good WHERE value IS INITIAL OR
                           fieldname <> 'FINALIDADE'.

      DATA(lt_saida) = it_saida.
      DELETE lt_saida WHERE desc_finalidade IS INITIAL.
      SORT lt_saida BY finalidade.

      DATA(lt_saida2) = it_saida.
      DELETE lt_saida2 WHERE desc_finalidade IS NOT INITIAL.
      SORT lt_saida2 BY finalidade.

      IF lt_good IS NOT INITIAL.
        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZFIN_EXPORT_D'
            text           = abap_true
            langu          = sy-langu
          TABLES
            dd07v_tab      = lt_values
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.
        IF sy-subrc IS INITIAL.
          SORT lt_values BY domvalue_l.
        ENDIF.

        LOOP AT lt_good ASSIGNING FIELD-SYMBOL(<fs_changed>).
          READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<fs_values>)
          WITH KEY domvalue_l = <fs_changed>-value
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_saida TRANSPORTING NO FIELDS
            WITH KEY finalidade = <fs_changed>-value
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              READ TABLE lt_saida2 TRANSPORTING NO FIELDS
              WITH KEY finalidade = <fs_changed>-value
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                lwa_cell-col_id = 1.
                lwa_cell-row_id = sy-tabix.
                APPEND lwa_cell TO lt_cell.
                gv_erro = abap_true.

              ENDIF.

            ELSE.

              READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>)
              WITH KEY finalidade = <fs_changed>-value.
              IF sy-subrc IS INITIAL.

                <fs_saida>-desc_finalidade = <fs_values>-ddtext.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

        CALL METHOD obj_alv->refresh_table_display( ).

        IF lt_cell IS NOT INITIAL.
          CALL METHOD obj_alv->set_selected_cells_id
            EXPORTING
              it_cells = lt_cell.

          MESSAGE 'Já existe um cadastro com esta Finalidade' TYPE 'S' DISPLAY LIKE 'E'.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.
*-

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: obj_toolbar      TYPE REF TO lcl_alv_toolbar.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------
DATA: vg_erro      TYPE c,
      v_prefix_ent TYPE zprefix,
      v_mensagem   TYPE bapi_msg,
      t_dir_loc_f  TYPE TABLE OF sdokpath,
      t_dir_local  TYPE TABLE OF sdokpath,
      t_dir_unix   TYPE TABLE OF epsfili,
      v_file_aux   TYPE draw-filep,
      v_file_aux2  TYPE draw-filep,
      it_xml_forn  TYPE TABLE OF zxml,
      wa_xml_forn  TYPE zxml.
