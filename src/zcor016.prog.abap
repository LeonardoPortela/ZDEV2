*&---------------------------------------------------------------------*
*& Report  ZCOR016
*& Atribuir Classes de Custos x Grupo de Mercadorias PM.
*&---------------------------------------------------------------------*
*& Autor          Data          Request       Descrição
*& Marcos Faneli  18.06.2014    DEVK938394    Construção do programa
*&---------------------------------------------------------------------*

REPORT  zcor016.

TABLES: zmmt0039.

****  Estruturas
TYPES: BEGIN OF ty_zmmt0039.
        INCLUDE TYPE zmmt0039.
TYPES:END OF ty_zmmt0039.

CONSTANTS tabela TYPE c LENGTH 30 VALUE 'ZMMT0039'.

DATA: obj_erro  TYPE REF TO cx_root,
      wg_texto  TYPE c LENGTH 255.

*----------------------------------------------------------------------*
*       CLASS lcl_zmmt0039 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zmmt0039 DEFINITION.
  PUBLIC SECTION.
    DATA : wa_layout        TYPE lvc_s_layo,
           it_catalog       TYPE lvc_t_fcat,
           wa_catalog       TYPE lvc_s_fcat,
           wa_stable        TYPE lvc_s_stbl,
           it_index_row     TYPE lvc_t_row,
           wa_index_row     TYPE lvc_s_row.

    METHODS: constructor,

             set_toolbar
             FOR EVENT toolbar OF cl_gui_alv_grid
             IMPORTING e_object,

             refresh_table,
*             FOR EVENT after_user_command OF cl_gui_alv_grid
*             importing e_object,

             generate_cat,

             read_data,

             generate_alv,

             save_data,

             delete_data,

             on_data_changed
             FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

  PRIVATE SECTION.
    DATA: o_grid      TYPE REF TO   cl_gui_alv_grid,
          o_container TYPE REF TO   cl_gui_custom_container.

    DATA: it_zmmt0039 TYPE TABLE OF zmmt0039,
          it_buttons  TYPE          ui_functions.

ENDCLASS.                    "lcl_flight DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zmmt0039 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zmmt0039 IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT o_container
      EXPORTING
        container_name              = 'CONTAINER_LOCAL'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT o_grid
      EXPORTING
        i_parent          = o_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD read_data.
    SELECT *
      FROM zmmt0039
      INTO CORRESPONDING FIELDS OF TABLE it_zmmt0039.

  ENDMETHOD.                   "READ_DATA

  METHOD generate_alv.

    CALL METHOD generate_cat.
    CALL METHOD set_toolbar.

*    wa_layout-stylefname = 'ESTILO'.
    wa_layout-zebra = 'X'.

    CALL METHOD o_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
*        i_save                        = 'U'
        it_toolbar_excluding          = it_buttons
      CHANGING
        it_outtab                     = it_zmmt0039[]
        it_fieldcatalog               = it_catalog[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD o_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD o_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER: on_data_changed  FOR o_grid.

  ENDMETHOD.                    "GENERATE_ALV

  METHOD refresh_table.
    CALL METHOD me->generate_alv.
  ENDMETHOD.                    "refresh_table

  METHOD set_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_buttons.
*    wa_buttons = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_buttons.
    APPEND cl_gui_alv_grid=>mc_fc_graph             TO it_buttons.
  ENDMETHOD.                    "set_toolbar

  METHOD generate_cat.
    REFRESH: it_catalog.

    CLEAR: wa_catalog.
    wa_catalog-fieldname  = 'MATKL'.
    wa_catalog-ref_table  = tabela.
    wa_catalog-ref_field  = 'MATKL'.
    wa_catalog-tabname    = tabela.
    wa_catalog-scrtext_l  = 'Grupo de mercadorias'.
    wa_catalog-scrtext_m  = 'Grupo de mercadorias'.
    wa_catalog-scrtext_s  = 'Grupo de mercadorias'.
    wa_catalog-outputlen  = 18.
*    wa_catalog-no_zero    = p_no_zero.
*    wa_catalog-hotspot    = p_hotspot.
*    wa_catalog-emphasize  = p_cor.
*    wa_catalog-just       = p_just.
*    wa_catalog-do_sum     = space.
    wa_catalog-edit       = 'X'.
*    WA_CATALOG-F4AVAILABL = P_F4.
*    obj->wa_catalog-checkbox   = p_check.
    APPEND wa_catalog TO it_catalog.

    CLEAR: wa_catalog.
    wa_catalog-fieldname  = 'SAKNR'.
    wa_catalog-ref_table  = tabela.
    wa_catalog-ref_field  = 'SAKNR'.
    wa_catalog-tabname    = tabela.
    wa_catalog-scrtext_l  = 'Nº conta do Razão'.
    wa_catalog-scrtext_m  = 'Nº conta do Razão'.
    wa_catalog-scrtext_s  = 'Nº conta do Razão'.
    wa_catalog-outputlen  = 15.
    wa_catalog-edit       = 'X'.
    APPEND wa_catalog TO it_catalog.

    CLEAR: wa_catalog.
    wa_catalog-fieldname  = 'DT_ATUAL'.
    wa_catalog-ref_table  = tabela.
    wa_catalog-ref_field  = 'DT_ATUAL'.
    wa_catalog-tabname    = tabela.
    wa_catalog-scrtext_l  = 'Data'.
    wa_catalog-scrtext_m  = 'Data'.
    wa_catalog-scrtext_s  = 'Data'.
    wa_catalog-outputlen  = 12.
    wa_catalog-edit       = 'X'.
    APPEND wa_catalog TO it_catalog.

    CLEAR: wa_catalog.
    wa_catalog-fieldname  = 'HR_ATUAL'.
    wa_catalog-ref_table  = tabela.
    wa_catalog-ref_field  = 'HR_ATUAL'.
    wa_catalog-tabname    = tabela.
    wa_catalog-scrtext_l  = 'Hora'.
    wa_catalog-scrtext_m  = 'Hora'.
    wa_catalog-scrtext_s  = 'Hora'.
    wa_catalog-outputlen  = 12.
    wa_catalog-edit       = 'X'.
    APPEND wa_catalog TO it_catalog.

    CLEAR: wa_catalog.
    wa_catalog-fieldname  = 'USNAM'.
    wa_catalog-ref_table  = tabela.
    wa_catalog-ref_field  = 'USNAM'.
    wa_catalog-tabname    = tabela.
    wa_catalog-scrtext_l  = 'Usuário'.
    wa_catalog-scrtext_m  = 'Usuário'.
    wa_catalog-scrtext_s  = 'Usuário'.
    wa_catalog-outputlen  = 15.
    APPEND wa_catalog TO it_catalog.

  ENDMETHOD.                    "generate_cat

  METHOD on_data_changed.
    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value.

    DATA: wl_zmmt0039 TYPE ty_zmmt0039.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      MOVE: sy-uname TO wl_zmmt0039-usnam.
      MODIFY it_zmmt0039 FROM wl_zmmt0039 INDEX ls_good-row_id TRANSPORTING usnam.
    ENDLOOP.
  ENDMETHOD.                    "edit_alv

  METHOD save_data.
    CLEAR wg_texto.

    TRY.
        MODIFY zmmt0039 FROM TABLE it_zmmt0039.
      CATCH cx_root INTO obj_erro.
        wg_texto = obj_erro->get_text( ).
    ENDTRY.

    IF wg_texto IS INITIAL.
      MESSAGE s836(sd) WITH 'Dados atualizados com sucesso.'.
    ENDIF.
  ENDMETHOD.                    "DATA_CHANGE

  METHOD delete_data.
    DATA: tl_zmmt0039 TYPE TABLE OF ty_zmmt0039,
          wl_zmmt0039 TYPE          ty_zmmt0039.

    CALL METHOD o_grid->get_selected_rows
      IMPORTING
        et_index_rows = it_index_row.

    LOOP AT it_index_row INTO wa_index_row.
      READ TABLE it_zmmt0039 INTO wl_zmmt0039 INDEX wa_index_row-index.
      APPEND wl_zmmt0039 TO tl_zmmt0039.
    ENDLOOP.

    DELETE zmmt0039 FROM TABLE tl_zmmt0039.

    CALL METHOD me->read_data.
    CALL METHOD me->refresh_table.
  ENDMETHOD.                    "data_delete

ENDCLASS.                    "lcl_flight IMPLEMENTATION

***  Vavriáveis e Objetos
DATA: obj TYPE REF TO lcl_zmmt0039.

START-OF-SELECTION.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  CREATE OBJECT obj.
  CALL METHOD obj->read_data.
  CALL METHOD obj->generate_alv.

  SET PF-STATUS 'PF_ALV_DADOS'.
  SET TITLEBAR  '0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN: 'BACK'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'. "Botão para sair do programa corrente.
      LEAVE PROGRAM.
    WHEN : 'DATA_SAVE'.
      CALL METHOD obj->save_data.
      CALL METHOD obj->read_data.
      CALL METHOD obj->refresh_table.
    WHEN: 'RET'.
      CALL METHOD obj->read_data.
      CALL METHOD obj->refresh_table.
    WHEN: 'DEL'.
      CALL METHOD obj->delete_data.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
