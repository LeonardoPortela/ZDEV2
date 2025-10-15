*&---------------------------------------------------------------------*
*&  Include           ZFIR0092_CLASS
*&---------------------------------------------------------------------*

* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_wizard.
    ty_toolbar-function  = 'GERAR_AVISO_REC'.
    ty_toolbar-quickinfo = 'Gerar Aviso Recebimento'.
    ty_toolbar-text      = 'Gerar Aviso Rec.'.

    IF r_atrib IS INITIAL.
      ty_toolbar-disabled  = wl_desactive.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.

    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_move.
    ty_toolbar-function  = 'ATRIB_AVISO_REC'.
    ty_toolbar-quickinfo = 'Atrib. Aviso Recebimento'.
    ty_toolbar-text      = 'Atrib. Aviso Rec.'.

    IF r_gerar IS INITIAL.
      ty_toolbar-disabled  = wl_desactive.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.

    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'ATUALIZAR'.
    ty_toolbar-quickinfo = 'Atualizar'.
    ty_toolbar-text      = ''.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'GERAR_AVISO_REC'.
        CLEAR: vg_view_transp.
        PERFORM limpar_dados_aviso.
        PERFORM carrega_dados.
      WHEN 'ATRIB_AVISO_REC'.
        CLEAR: vg_view_transp.
        PERFORM limpar_dados_aviso.
        PERFORM carrega_dados_atrib.
      WHEN 'ATUALIZAR'.
        PERFORM limpar_dados.
        PERFORM seleciona_dados.
        PERFORM processa_dados.
        CALL METHOD wa_alv_ped->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS lcl_alv_toolbar_aviso DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_alv_toolbar_aviso IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager_aviso
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_display.
    ty_toolbar-function  = 'VIEW_DADOS_COMP'.
    ty_toolbar-quickinfo = ''.
    ty_toolbar-text      = ''.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_storno.
    ty_toolbar-function  = 'EST_CTE'.
    ty_toolbar-quickinfo = 'Estornar'.
    ty_toolbar-text      = 'Estornar'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'ATUALIZAR'.
    ty_toolbar-quickinfo = 'Atualizar'.
    ty_toolbar-text      = 'Atualizar'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_retrieve.
    ty_toolbar-function  = 'RECUP_DOCS'.
    ty_toolbar-quickinfo = 'Atualizar Docs.'.
    ty_toolbar-text      = 'Atualizar Docs.'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
    IF r_atrib IS NOT INITIAL.
      ty_toolbar-icon      = icon_retrieve.
      ty_toolbar-function  = 'REMOVER_AVISO'.
      ty_toolbar-quickinfo = 'Remover Aviso'.
      ty_toolbar-text      = 'Remover Aviso'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.
    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP <<---


*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD c_alv_toolbarmanager_aviso->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'VIEW_DADOS_COMP'.
        vg_view_transp = 'X'.
        PERFORM limpar_dados_aviso.
        PERFORM preenche_dados USING wa_saida.
        PERFORM abrir_aviso_rec.

      WHEN 'ATUALIZAR'.
        PERFORM aviso_ped_selected.
      WHEN 'EST_CTE'.
        PERFORM sol_estorno_cte.
      WHEN 'RECUP_DOCS'.
        PERFORM recuperar_docs.

     "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
      WHEN 'REMOVER_AVISO'.
        PERFORM: f_remover_aviso_atribuido.
     "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP <<---

    ENDCASE.

    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
    CASE e_ucomm.
      WHEN 'REMOVER_AVISO'.
        PERFORM aviso_ped_selected.
    ENDCASE.
    "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP <<---

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION



* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS lcl_alv_toolbar_dados_aviso DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_alv_toolbar_dados_aviso IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_dados_aviso
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_wizard.
    ty_toolbar-function  = 'INF_QTDE'.
    ty_toolbar-quickinfo = 'Informar Quantidade Aviso'.
    ty_toolbar-text      = 'Informar Qtde.'.

    IF r_gerar IS NOT INITIAL.
      ty_toolbar-disabled  = wl_desactive.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD c_alv_toolbar_dados_aviso->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'INF_QTDE'.
        PERFORM informar_qtde_aviso.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS lcl_alv_toolbar_dados_veic DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_alv_toolbar_dados_veic IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_dados_veic
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    SELECT SINGLE *
     FROM lfa1
     INTO @DATA(wlfa1)
     WHERE lifnr = @wa_dados_transp-agente_frete.

    IF wlfa1-ktokk = 'ZFIC'.
      vg_tipo_frete = 'CIF'.
    ELSE.
      vg_tipo_frete = 'CPT'.
    ENDIF.

    IF r_atrib IS INITIAL.

      IF vg_tipo_frete = 'CPT'.

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

      ENDIF.

    ELSE.

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

    ENDIF.
*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD c_alv_toolbar_dados_veic->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA:  it_dados_veic_aux       TYPE TABLE OF ty_dados_veic.


    CASE e_ucomm.
      WHEN 'DEL'.
        CALL METHOD wa_alv_dados_veic->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE it_dados_veic INDEX wg_selectedcell-row_id-index.
        ENDLOOP.
      WHEN 'ADD'.
        it_dados_veic_aux[] = it_dados_veic[].
        REFRESH:it_dados_veic.
        LOOP AT it_dados_veic_aux INTO DATA(wa_dados_veic).
          APPEND wa_dados_veic TO it_dados_veic.
        ENDLOOP.
        CLEAR wa_dados_veic.
        wa_dados_veic-tp_veiculo = '0'.
        APPEND wa_dados_veic TO it_dados_veic.
    ENDCASE.

    CALL METHOD wa_alv_dados_veic->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0106 DEFINITION.

  PUBLIC SECTION.                                           "

    CLASS-METHODS:
      user_command    FOR EVENT user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_f4            FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    "
ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "


*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0106 IMPLEMENTATION.                "

  METHOD user_command.
  ENDMETHOD.                    "ON_F4

  METHOD on_data_changed_finished.

    SELECT SINGLE *
       FROM lfa1
       INTO @DATA(wlfa1)
       WHERE lifnr = @wa_dados_transp-agente_frete.

    IF wlfa1-ktokk = 'ZFIC'.
      PERFORM atualiza_dados_veic.
    ELSEIF wlfa1-ktokk <> 'ZFIC' AND r_atrib IS NOT INITIAL.
      PERFORM atualiza_dados_veic.
    ENDIF.

  ENDMETHOD.


  METHOD on_f4.
  ENDMETHOD.                    "ON_F4


  METHOD on_data_changed.
  ENDMETHOD.                    "ON_DATA_CHANGED


ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION



*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_0101 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_0101 IMPLEMENTATION.

  METHOD catch_hotspot.

    PERFORM handler_0101_cat_hot USING e_row_id
                                       e_column_id
                                       es_row_no.

  ENDMETHOD.                    "CATCH_HOTSPOT


  METHOD on_data_changed.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_f4.
  ENDMETHOD. "on_f4


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_ped DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row	e_column es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      user_command    FOR EVENT user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_ped IMPLEMENTATION.

  METHOD double_click.

    READ TABLE it_saida INTO wa_saida INDEX e_row-index.

    CHECK sy-subrc = 0.

    PERFORM atualiza_aviso USING wa_saida.


  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.


  METHOD user_command.
  ENDMETHOD.


  METHOD catch_hotspot.
    DATA: wl_tvro     TYPE tvro,
          v_messa(50).

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

    CHECK sy-subrc = 0.

    IF e_column_id = 'EBELN'.
      IF wa_saida-ebeln  NE icon_icon_list.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "CATCH_HOTSPOT

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
