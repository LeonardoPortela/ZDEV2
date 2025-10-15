*&---------------------------------------------------------------------*
*& Transação: ZMM0122                                                  *
*& Autor....: Enio Jesus                                               *
*& Descrição: Vinculação Cargo x Material                              *
*&---------------------------------------------------------------------*

REPORT zmmr051.
INCLUDE <cl_alv_control>.
TYPES:
  BEGIN OF ty_tab,
    cargo     TYPE pa0001-stell,
    descricao TYPE t513s-stltx,
    codagrepi TYPE zhrst_med_ag_epi-codagrepi,
  END OF ty_tab,

  BEGIN OF ty_materials,
    status  TYPE icon-id,
    matnr   TYPE makt-matnr,
    maktx   TYPE makt-maktx,
    data    TYPE zmmt0085-data,
    hora    TYPE zmmt0085-hora,
    usuario TYPE zmmt0085-usuario,
    style   TYPE lvc_t_styl,
  END OF ty_materials,

  BEGIN OF ty_agrupadores_epi,
    status    TYPE icon-id,
    codagrepi TYPE zhrst_med_ag_epi-codagrepi,
    desagrepi TYPE zhrst_med_ag_epi-denagrepi,
    data      TYPE zmmt0080-data,
    hora      TYPE zmmt0080-hora,
    usuario   TYPE zmmt0080-usuario,
    style     TYPE lvc_t_styl,
  END OF ty_agrupadores_epi,

  BEGIN OF ty_centro_cargo_materials,
    status    TYPE icon-id,
    matnr     TYPE makt-matnr,
    codagrepi TYPE zhrst_med_ag_epi-codagrepi,
    maktx     TYPE makt-maktx,
    data      TYPE zmmt0085-data,
    hora      TYPE zmmt0085-hora,
    usuario   TYPE zmmt0085-usuario,
    style     TYPE lvc_t_styl,
  END OF ty_centro_cargo_materials,

  t_materials        TYPE TABLE OF ty_materials
    WITH EMPTY KEY,

  t_epi_groups       TYPE TABLE OF ty_agrupadores_epi
    WITH EMPTY KEY,

  t_centro_materiais TYPE TABLE OF ty_centro_cargo_materials
    WITH EMPTY KEY,

  BEGIN OF ty_cadastro_cargo,
    cargo     TYPE pa0001-stell,
    descricao TYPE t513s-stltx,
    itens     TYPE t_epi_groups,
    custom    TYPE REF TO cl_gui_custom_container,
    grid      TYPE REF TO cl_gui_alv_grid,
  END OF ty_cadastro_cargo,

  BEGIN OF ty_cadastro_agrupador_epi,
    codigo    TYPE zhrst_med_ag_epi-codagrepi,
    descricao TYPE zhrst_med_ag_epi-denagrepi,
    custom    TYPE REF TO cl_gui_custom_container,
    grid      TYPE REF TO cl_gui_alv_grid,
    itens     TYPE t_materials,
  END OF ty_cadastro_agrupador_epi,

  BEGIN OF ty_cadastro_cargo_material,
    centro    TYPE t001w-werks,
    desc_cen  TYPE t001w-name1,
    cargo     TYPE pa0001-stell,
    descricao TYPE t513s-stltx,
    codagrepi TYPE zhrst_med_ag_epi-codagrepi,
    descr_gru TYPE zhrst_med_ag_epi-denagrepi,
    matnr     TYPE makt-matnr,
    maktx     TYPE makt-maktx,
    custom    TYPE REF TO cl_gui_custom_container,
    grid      TYPE REF TO cl_gui_alv_grid,
    itens     TYPE t_centro_materiais,
    customc   TYPE REF TO cl_gui_custom_container,
    gridc     TYPE REF TO cl_gui_alv_grid,
    itensc    TYPE t_centro_materiais,
  END OF ty_cadastro_cargo_material.

DATA:
  BEGIN OF tipo_vinculacao,
    cargo_agrupador    TYPE c,
    material_agrupador TYPE c,
    centro_cargo       TYPE c,
  END OF tipo_vinculacao.

DATA cadastro_cargo             TYPE ty_cadastro_cargo.
DATA cadastro_cargo_material    TYPE ty_cadastro_cargo_material.
DATA cadastro_agrupador_epi     TYPE ty_cadastro_agrupador_epi.
DATA agrupador_epi              TYPE zmmt0085-codagrepi.
DATA gt_tab                     TYPE TABLE OF ty_tab.

DATA wa_style          TYPE lvc_s_styl.
DATA style             TYPE lvc_t_styl WITH HEADER LINE.

DATA screen_vinculacao TYPE sy-dynnr VALUE 0002.

CALL SCREEN 0001.

CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    METHODS display_agrupadores.
    METHODS display_materiais.
    METHODS display_centro_cargo.
    METHODS process_before_output.
    METHODS set_status.
    METHODS set_title.

    METHODS set_invisible_button
      IMPORTING
        button TYPE sy-ucomm.

    METHODS user_command
      IMPORTING
        ucomm TYPE sy-ucomm.

    METHODS set_screen_vinculacao
      IMPORTING
        screen TYPE sy-repid.

  PRIVATE SECTION.
    METHODS search_register.
    METHODS save_register.
    METHODS atualiza_centro.
    METHODS atualiza_disp.
    METHODS insere_novo.
    METHODS insere_novo_agrupador.
    METHODS refresh_screen.
    METHODS ask_to_save.

    METHODS is_saved
      RETURNING VALUE(value) TYPE abap_bool.

    METHODS clear.

    METHODS set_edit_mode
      IMPORTING
        bool TYPE abap_bool.

    METHODS get_edit_mode
      RETURNING VALUE(value) TYPE abap_bool.

    METHODS handle_set_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object.

    METHODS handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm.

    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        er_data_changed
        e_onf4
        e_onf4_before
        e_onf4_after
        e_ucomm.

    METHODS handle_f4_popup FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

    METHODS get_fieldcatalog
      RETURNING VALUE(result) TYPE lvc_t_fcat.

    METHODS get_material_description
      IMPORTING
        input         TYPE any
      RETURNING
        VALUE(result) TYPE makt-maktx.

    METHODS get_epi_group_description
      IMPORTING
        input         TYPE any
      RETURNING
        VALUE(result) TYPE zhrst_med_ag_epi-denagrepi.

    DATA custom        TYPE REF TO cl_gui_custom_container.
    DATA edit_mode     TYPE abap_bool.
    DATA invisible_buttons TYPE TABLE OF sy-ucomm.
    DATA layout        TYPE lvc_s_layo.
    DATA alv_table     TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

CLASS cl_main IMPLEMENTATION.
  METHOD process_before_output.
    IF tipo_vinculacao-cargo_agrupador = abap_true.
      me->display_agrupadores( ).
    ELSEIF tipo_vinculacao-material_agrupador = abap_true.
      me->display_materiais( ).
    ELSEIF tipo_vinculacao-centro_cargo = abap_true.
      me->display_centro_cargo( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_invisible_button.
    APPEND 'EDIT' TO invisible_buttons.
  ENDMETHOD.

  METHOD refresh_screen.
    IF tipo_vinculacao-cargo_agrupador = abap_true.
      cadastro_cargo-grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ELSEIF tipo_vinculacao-material_agrupador = abap_true..
      cadastro_agrupador_epi-grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ELSEIF tipo_vinculacao-centro_cargo = abap_true.
      cadastro_cargo_material-grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
      cadastro_cargo_material-gridc->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.
  ENDMETHOD.

  METHOD is_saved.
    IF me->edit_mode = abap_true.
      value = abap_false.
    ELSE.
      value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR: cadastro_cargo-itens, cadastro_cargo-descricao, cadastro_cargo-cargo.
    CLEAR: cadastro_agrupador_epi-codigo, cadastro_agrupador_epi-descricao, cadastro_agrupador_epi-itens.
  ENDMETHOD.

  METHOD get_edit_mode.
    MOVE me->edit_mode TO value.
  ENDMETHOD.

  METHOD set_edit_mode.
    IF bool = abap_on.
      MOVE bool TO me->edit_mode.
      MESSAGE 'Modo edição habilitado.'   TYPE 'S'.
    ELSE.
      MOVE bool TO me->edit_mode.
      MESSAGE 'Modo edição desabilitado.' TYPE 'S'.
    ENDIF.

    "//Get Field Catalog
*    DATA(FIELDCAT) = ME->GET_FIELDCATALOG( ).

*    IF TIPO_VINCULACAO-CARGO_AGRUPADOR = ABAP_TRUE.
*      CADASTRO_CARGO-GRID->SET_FRONTEND_FIELDCATALOG( FIELDCAT ).
*    ELSE.
*      CADASTRO_AGRUPADOR_EPI-GRID->SET_FRONTEND_FIELDCATALOG( FIELDCAT ).
*    ENDIF.

*    ME->REFRESH_SCREEN( ).
  ENDMETHOD.

  METHOD handle_set_toolbar.
    e_object->mt_toolbar = VALUE #( ( butn_type = 0
                                      function  = 'ADD'
                                      icon      = icon_insert_row
                                      disabled  = SWITCH #( me->get_edit_mode( ) WHEN abap_false THEN abap_true ELSE abap_false )
                                      text      = ''
                                    )

                                    ( butn_type = 0
                                      function  = 'DEL'
                                      icon      = icon_delete_row
                                      disabled  = SWITCH #( me->get_edit_mode( ) WHEN abap_false THEN abap_true ELSE abap_false )
                                      quickinfo = '' )

*                                    ( BUTN_TYPE = 3 )
*
*                                    (
*                                      BUTN_TYPE = 0
*                                      FUNCTION  = 'CHG'
*                                      ICON      = ICON_CHANGE
*                                      QUICKINFO = 'Editar'
*                                    )
                                  ).
  ENDMETHOD.

  METHOD handle_user_command.
    DATA selected_rows TYPE lvc_t_row.
    DATA selected_row  TYPE lvc_s_row.

    IF tipo_vinculacao-cargo_agrupador = abap_true.

      CASE e_ucomm.
        WHEN 'ADD'.
          APPEND VALUE #( status = icon_led_yellow ) TO cadastro_cargo-itens.
        WHEN 'DEL'.
          cadastro_cargo-grid->get_selected_rows(
            IMPORTING et_index_rows = selected_rows ).

          FIELD-SYMBOLS <fs_grupo> LIKE LINE OF cadastro_cargo-itens.

          SORT selected_rows DESCENDING BY index.

          LOOP AT selected_rows INTO selected_row.
            READ TABLE cadastro_cargo-itens ASSIGNING <fs_grupo> INDEX selected_row-index.
            SELECT SINGLE * FROM zmmt0085 INTO @DATA(w085) WHERE codagrepi = @<fs_grupo>-codagrepi.
            IF sy-subrc NE 0.
              <fs_grupo>-status = icon_delete.
              MODIFY cadastro_cargo-itens FROM <fs_grupo> INDEX selected_row-index.
              "DELETE CADASTRO_CARGO-ITENS INDEX SELECTED_ROW-INDEX.
            ELSE.
              MESSAGE |Este grupo está vinculado ao material { w085-matnr } | TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
          ENDLOOP.

        WHEN 'CHG'.
        WHEN OTHERS.
      ENDCASE.

    ELSE.
      cadastro_agrupador_epi-grid->get_selected_rows(
        IMPORTING et_index_rows = selected_rows ).

      FIELD-SYMBOLS <fs_material> LIKE LINE OF cadastro_agrupador_epi-itens.

      CASE e_ucomm.
        WHEN 'ADD'.
          APPEND VALUE #( status = icon_led_yellow ) TO cadastro_agrupador_epi-itens.

        WHEN 'DEL'.

          SORT selected_rows DESCENDING BY index.

          LOOP AT selected_rows INTO selected_row.
            READ TABLE cadastro_agrupador_epi-itens ASSIGNING <fs_material> INDEX selected_row-index.
            SELECT SINGLE * FROM zmmt0100 INTO @DATA(w100) WHERE matnr = @<fs_material>-matnr.
            IF sy-subrc NE 0.
              <fs_material>-status = icon_delete.
              MODIFY cadastro_agrupador_epi-itens FROM <fs_material> INDEX selected_row-index.
              "  DELETE CADASTRO_AGRUPADOR_EPI-ITENS INDEX SELECTED_ROW-INDEX.
            ELSE.
              MESSAGE |Este material está vinculado ao centro/cargo { w100-werks } { w100-cargo } | TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
          ENDLOOP.

        WHEN 'CHG'.
          LOOP AT selected_rows INTO selected_row.
            READ TABLE cadastro_agrupador_epi-itens ASSIGNING <fs_material> INDEX selected_row-index.
            <fs_material>-status = icon_led_yellow.
          ENDLOOP.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD handle_data_changed.
    LOOP AT er_data_changed->mt_good_cells INTO DATA(_item_changed).

      IF tipo_vinculacao-cargo_agrupador = abap_true.
        CASE _item_changed-fieldname.
          WHEN 'CODAGREPI'.
            READ TABLE cadastro_cargo-itens ASSIGNING FIELD-SYMBOL(<fs_agrupadores_epi>) INDEX _item_changed-row_id.
            IF <fs_agrupadores_epi>-status EQ icon_led_green.
              MESSAGE |Código agrupador { _item_changed-value } não pode ser modificado.| TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF _item_changed-value IS NOT INITIAL.
              <fs_agrupadores_epi>-desagrepi = me->get_epi_group_description( _item_changed-value ).

              IF <fs_agrupadores_epi>-desagrepi IS INITIAL.
                MESSAGE |Código agrupador { _item_changed-value } não encontrado.| TYPE 'S' DISPLAY LIKE 'E'.

                CALL METHOD me->handle_f4_popup
                  EXPORTING
                    es_row_no = VALUE #( row_id = _item_changed-row_id ).

              ELSE.
                <fs_agrupadores_epi>-codagrepi = _item_changed-value.
              ENDIF.

            ELSE.
              CLEAR <fs_agrupadores_epi>-desagrepi.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.
      ELSE.

        CASE _item_changed-fieldname.
          WHEN 'MATNR'.
            READ TABLE cadastro_agrupador_epi-itens ASSIGNING FIELD-SYMBOL(<fs_material>) INDEX _item_changed-row_id.
            IF <fs_material>-status EQ icon_led_green.
              MESSAGE |Código material { _item_changed-value } não pode ser modificado.| TYPE 'S' DISPLAY LIKE 'E'.
            ELSEIF _item_changed-value IS NOT INITIAL.
              <fs_material>-maktx = me->get_material_description( _item_changed-value ).
            ELSE.
              CLEAR <fs_material>-maktx.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.
      ENDIF.

    ENDLOOP.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD handle_f4_popup.
    DATA results TYPE TABLE OF ddshretval.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'ZHRST_MED_AG_EPI'
        fieldname         = 'CODAGREPI'
        searchhelp        = 'ZHRST_MED_AG_EPI'
      TABLES
        return_tab        = results
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT results INTO DATA(_result).
      ASSIGN cadastro_cargo-itens[ es_row_no-row_id ] TO FIELD-SYMBOL(<fs_item>).
      <fs_item>-codagrepi = _result-fieldval.
      <fs_item>-desagrepi = me->get_epi_group_description( <fs_item>-codagrepi ).
    ENDLOOP.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD display_agrupadores.
    "//Display Plannings datas
    IF ( cadastro_cargo-custom IS NOT BOUND ).
      CREATE OBJECT cadastro_cargo-custom
        EXPORTING
          container_name = 'CUSTOM_AGRUPADORES_EPI'.
    ENDIF.

    "//Get Field Catalog
    DATA(fieldcat) = me->get_fieldcatalog( ).

    IF ( cadastro_cargo-grid IS NOT BOUND ).

      "//Create Alv Object
      CREATE OBJECT cadastro_cargo-grid
        EXPORTING
          i_parent = cadastro_cargo-custom.

      "//Register f4 fields
      DATA(_f4_fields) =
        VALUE lvc_t_f4( ( fieldname = 'CODAGREPI'
                          register  = abap_true
                          getbefore = abap_true ) ).

      CALL METHOD cadastro_cargo-grid->register_f4_for_fields
        EXPORTING
          it_f4 = _f4_fields.

      SET HANDLER:
        me->handle_set_toolbar  FOR cadastro_cargo-grid,
        me->handle_user_command FOR cadastro_cargo-grid,
        me->handle_f4_popup     FOR cadastro_cargo-grid,
        me->handle_data_changed FOR cadastro_cargo-grid.

      "//Display Datas
      me->layout-no_toolbar   = ' '.
      me->layout-sel_mode     = ' '.
      me->layout-stylefname = 'STYLE'.
      CALL METHOD cadastro_cargo-grid->set_table_for_first_display
        EXPORTING
          is_layout       = me->layout
          i_save          = abap_on
        CHANGING
          it_outtab       = cadastro_cargo-itens
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      CALL METHOD cadastro_cargo-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cadastro_cargo-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      cadastro_cargo-grid->set_frontend_fieldcatalog( fieldcat ).
      me->refresh_screen( ).
    ENDIF.
  ENDMETHOD.

  METHOD display_materiais.
    "//Display Plannings datas
    IF ( cadastro_agrupador_epi-custom IS NOT BOUND ).
      CREATE OBJECT cadastro_agrupador_epi-custom
        EXPORTING
          container_name = 'CUSTOM_AGRUPADOR_MATERIAL'.
    ENDIF.

    "//Get Field Catalog
    DATA(fieldcat) = me->get_fieldcatalog( ).

    IF ( cadastro_agrupador_epi-grid IS NOT BOUND ).

      "//Create Alv Object
      CREATE OBJECT cadastro_agrupador_epi-grid
        EXPORTING
          i_parent = cadastro_agrupador_epi-custom.
*
      SET HANDLER:
        me->handle_set_toolbar
        me->handle_user_command
        me->handle_data_changed FOR cadastro_agrupador_epi-grid.

      "//Display Datas
      me->layout-no_toolbar   = ' '.
      me->layout-sel_mode     = ' '.
      me->layout-stylefname = 'STYLE'.
      CALL METHOD cadastro_agrupador_epi-grid->set_table_for_first_display
        EXPORTING
          is_layout       = me->layout
          i_save          = abap_on
        CHANGING
          it_outtab       = cadastro_agrupador_epi-itens
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      CALL METHOD cadastro_agrupador_epi-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cadastro_agrupador_epi-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      cadastro_agrupador_epi-grid->set_frontend_fieldcatalog( fieldcat ).
      me->refresh_screen( ).
    ENDIF.
  ENDMETHOD.

  METHOD display_centro_cargo.
    " GRID de Materiais disponiveis para o CARGO
    IF ( cadastro_cargo_material-custom IS NOT BOUND ).
      CREATE OBJECT cadastro_cargo_material-custom
        EXPORTING
          container_name = 'CUSTOM_MATERIAL'.
    ENDIF.

    "//Get Field Catalog
    DATA(fieldcat) = me->get_fieldcatalog( ).

    IF ( cadastro_cargo_material-grid IS NOT BOUND ).

      "//Create Alv Object
      CREATE OBJECT cadastro_cargo_material-grid
        EXPORTING
          i_parent = cadastro_cargo_material-custom.


      me->layout-no_toolbar   = 'X'.
      me->layout-sel_mode   = sy-abcde(1).
      "//Display Datas
      CALL METHOD cadastro_cargo_material-grid->set_table_for_first_display
        EXPORTING
          is_layout       = me->layout
          i_save          = abap_on
        CHANGING
          it_outtab       = cadastro_cargo_material-itens
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      CALL METHOD cadastro_cargo_material-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cadastro_cargo_material-grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      cadastro_cargo_material-grid->set_frontend_fieldcatalog( fieldcat ).
      me->refresh_screen( ).
    ENDIF.
    "
    " GRID de Materiais selecionados para o CARGO no CENTRO
    IF ( cadastro_cargo_material-customc IS NOT BOUND ).
      CREATE OBJECT cadastro_cargo_material-customc
        EXPORTING
          container_name = 'CUSTOM_CENTRO_MATERIAL'.
    ENDIF.

    "//Get Field Catalog
    DATA(fieldcat2) = me->get_fieldcatalog( ).

    IF ( cadastro_cargo_material-gridc IS NOT BOUND ).

      "//Create Alv Object
      CREATE OBJECT cadastro_cargo_material-gridc
        EXPORTING
          i_parent = cadastro_cargo_material-customc.

      "//Display Datas
      me->layout-no_toolbar   = 'X'.
      me->layout-sel_mode     = sy-abcde(1).
      CALL METHOD cadastro_cargo_material-gridc->set_table_for_first_display
        EXPORTING
          is_layout       = me->layout
          i_save          = abap_on
        CHANGING
          it_outtab       = cadastro_cargo_material-itensc
          it_fieldcatalog = fieldcat2.

      "//Register Modify Events
      CALL METHOD cadastro_cargo_material-gridc->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cadastro_cargo_material-gridc->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      cadastro_cargo_material-gridc->set_frontend_fieldcatalog( fieldcat ).
      me->refresh_screen( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_status.
    SET PF-STATUS 'STATUS_MAIN' EXCLUDING invisible_buttons.
  ENDMETHOD.

  METHOD set_title.
    SET TITLEBAR 'TITLE_MAIN'.
  ENDMETHOD.

  METHOD user_command.
    CASE ucomm.
      WHEN 'BACK' OR 'CANCEL'.
        LEAVE TO SCREEN 0.

      WHEN 'EXIT'.
        LEAVE PROGRAM.

      WHEN 'TP_VINCULACAO'.
*        IF ME->IS_SAVED( ) = ABAP_FALSE.
*          ME->ASK_TO_SAVE( ).
*        ENDIF.
        clear( ).
        me->set_edit_mode( abap_false ).

        IF tipo_vinculacao-material_agrupador = abap_true.
          me->set_screen_vinculacao( '0003' ).
        ELSEIF tipo_vinculacao-cargo_agrupador = abap_true..
          me->set_screen_vinculacao( '0002' ).
        ELSE.
          me->set_screen_vinculacao( '0005' ).
        ENDIF.

      WHEN 'ENTER'.
        me->set_edit_mode( abap_false ).
        me->search_register( ).

      WHEN 'EDIT'.
        SELECT SINGLE *
           FROM zmmt0078
           INTO @DATA(wa_zmmt0078)
           WHERE usnam EQ @sy-uname
           AND   mtart EQ 'ZEPI'
           AND   werks EQ '9999'.
        IF sy-subrc = 0.
          me->set_edit_mode( abap_true ).
        ELSE.
          MESSAGE 'Sem autorização, procure o SSO Matriz!' TYPE 'S'.
        ENDIF.

      WHEN 'SAVE'.
        SELECT SINGLE *
          FROM zmmt0078
          INTO @DATA(wa2_zmmt0078)
          WHERE usnam EQ @sy-uname
          AND   mtart EQ 'ZEPI'
          AND   werks EQ '9999'.
        IF sy-subrc = 0.
          me->save_register( ).
        ELSE.
          MESSAGE 'Sem autorização, procure o SSO Matriz!' TYPE 'S'.
        ENDIF.
      WHEN 'ATU_CENTRO'.
        me->atualiza_centro( ).

      WHEN 'ATU_DISP'.
        me->atualiza_disp( ).
        me->search_register( ).
      WHEN 'INS_EPI'.
        me->insere_novo( ).
      WHEN 'INS_AGR'.
        me->insere_novo_agrupador( ).
      WHEN 'ATU_CARGO'.
        REFRESH gt_tab.
        IF cadastro_cargo_material-centro IS NOT INITIAL.
          SELECT cargo stltx codagrepi
            FROM zmmt0080
            INNER JOIN t513s
            ON t513s~stell = zmmt0080~cargo
            INTO TABLE gt_tab
            WHERE NOT EXISTS ( SELECT * FROM zmmt0100 WHERE zmmt0100~werks = cadastro_cargo_material-centro
                                                      AND   zmmt0100~cargo = zmmt0080~cargo ).

          CALL FUNCTION 'Z_EXIBE_ALV'
            TABLES
              it_alv = gt_tab.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD set_screen_vinculacao.
    MOVE screen TO screen_vinculacao.
  ENDMETHOD.

  METHOD ask_to_save.
    DATA answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Registros não foram salvos'
*       DIAGNOSE_OBJECT       = ' '
        text_question  = 'Deseja salvar os registros antes de sair?'
        text_button_1  = 'Sim'
*       ICON_BUTTON_1  = ' '
        text_button_2  = 'Não'
*       ICON_BUTTON_2  = ' '
        default_button = '1'
*       DISPLAY_CANCEL_BUTTON = 'X'
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN   = 25
*       START_ROW      = 6
*       POPUP_TYPE     =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        answer         = answer
*     TABLES
*       PARAMETER      =
*     EXCEPTIONS
*       TEXT_NOT_FOUND = 1
*       OTHERS         = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CHECK answer = '1'.
    me->save_register( ).
  ENDMETHOD.

  METHOD search_register.
    IF ( tipo_vinculacao-cargo_agrupador = abap_true ).

      SELECT SINGLE stltx
        FROM t513s
        INTO cadastro_cargo-descricao
       WHERE stell = cadastro_cargo-cargo.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zmmt0080
          INTO TABLE @DATA(_epi_groups)
         WHERE cargo = @cadastro_cargo-cargo.

        CLEAR cadastro_cargo-itens.
        REFRESH: style.
        CLEAR: wa_style.
        wa_style-fieldname = 'CODAGREPI'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .
        LOOP AT _epi_groups INTO DATA(_epi_group).
          APPEND VALUE #( codagrepi = _epi_group-codagrepi
                          desagrepi = get_epi_group_description( _epi_group-codagrepi )
                          status    = icon_led_green
                          data      = _epi_group-data
                          hora      = _epi_group-hora
                          usuario   = _epi_group-usuario
                          style     = style[]
                        ) TO cadastro_cargo-itens.
        ENDLOOP.

        IF cadastro_cargo-itens IS INITIAL.
          me->set_edit_mode( abap_true ).
        ENDIF.

        me->display_agrupadores( ).

        MESSAGE COND #( LET x = lines( _epi_groups ) IN
                       WHEN x = 1 THEN |({ x }) agrupador de EPI encontrado.|
                       WHEN x = 0 OR x > 1 THEN |({ x }) agrupadores de EPI's encontrados.| ) TYPE 'S'.

      ELSE.
        IF cadastro_cargo-itens IS NOT INITIAL.
          CLEAR: cadastro_cargo-itens, cadastro_cargo-descricao.
          me->display_agrupadores( ).
        ENDIF.

        MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ELSEIF ( tipo_vinculacao-material_agrupador = abap_true ).
      SELECT SINGLE denagrepi
        FROM zhrst_med_ag_epi
        INTO cadastro_agrupador_epi-descricao
       WHERE codagrepi = cadastro_agrupador_epi-codigo.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zmmt0085
          INTO TABLE @DATA(_materials)
         WHERE codagrepi = @cadastro_agrupador_epi-codigo.

        CLEAR cadastro_agrupador_epi-itens.
        REFRESH: style.
        CLEAR: wa_style.
        wa_style-fieldname = 'MATNR'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .
        LOOP AT _materials INTO DATA(_material).
          APPEND VALUE #( status = icon_led_green
                          matnr  = _material-matnr
                          maktx  = me->get_material_description( _material-matnr )
                          data      = _material-data
                          hora      = _material-hora
                          usuario   = _material-usuario
                          style     = style[]
                        ) TO cadastro_agrupador_epi-itens.
        ENDLOOP.

        IF cadastro_agrupador_epi-itens IS INITIAL.
          me->set_edit_mode( abap_true ).
        ENDIF.

        me->display_materiais( ).

        MESSAGE COND #( LET x = lines( _materials ) IN
                       WHEN x = 1 THEN |({ x }) material encontrado.|
                       WHEN x = 0 OR x > 1 THEN |({ x }) materiais encontrados.| ) TYPE 'S'.

      ELSE.
        CLEAR: cadastro_agrupador_epi-codigo, cadastro_agrupador_epi-descricao, cadastro_agrupador_epi-itens.
        MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSEIF ( tipo_vinculacao-centro_cargo  = abap_true ).
      SELECT SINGLE stltx
         FROM t513s
         INTO cadastro_cargo_material-descricao
        WHERE stell = cadastro_cargo_material-cargo.

      SELECT SINGLE denagrepi
        FROM zhrst_med_ag_epi
        INTO cadastro_cargo_material-descr_gru
       WHERE codagrepi = cadastro_cargo_material-codagrepi.

      SELECT SINGLE name1
        FROM  t001w
        INTO cadastro_cargo_material-desc_cen
        WHERE werks = cadastro_cargo_material-centro.

      IF cadastro_cargo_material-matnr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = cadastro_cargo_material-matnr
          IMPORTING
            output = cadastro_cargo_material-matnr.

        SELECT SINGLE maktx
             FROM  makt
             INTO cadastro_cargo_material-maktx
             WHERE matnr = cadastro_cargo_material-matnr.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
       ID 'WERKS' FIELD  cadastro_cargo_material-centro
       ID 'ACTVT' FIELD '03'.    "Alteração

      CASE sy-subrc.
        WHEN 0.
*  tem autorização!
        WHEN 4.
          MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
          SET CURSOR FIELD 'CADASTRO_CARGO_MATERIAL-CENTRO'.
          EXIT.
        WHEN 12.
          MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
          SET CURSOR FIELD 'CADASTRO_CARGO_MATERIAL-CENTRO'.
          EXIT.
        WHEN OTHERS.
      ENDCASE.

      IF cadastro_cargo_material-cargo IS NOT INITIAL AND
         cadastro_cargo_material-centro IS NOT INITIAL.
        SELECT zmmt0080~cargo, zmmt0085~matnr, zmmt0085~codagrepi
           FROM zmmt0080
           INNER JOIN zmmt0085
           ON zmmt0085~codagrepi = zmmt0080~codagrepi
           INTO TABLE @DATA(_materiais_disp)
          WHERE zmmt0080~cargo     = @cadastro_cargo_material-cargo
          AND   zmmt0080~codagrepi = @cadastro_cargo_material-codagrepi
          AND   NOT EXISTS ( SELECT * FROM zmmt0100 WHERE zmmt0100~werks      = @cadastro_cargo_material-centro
                                              AND   zmmt0100~cargo      = zmmt0080~cargo
                                              AND   zmmt0100~codagrepi  = zmmt0080~codagrepi
                                              AND   zmmt0100~matnr      = zmmt0085~matnr )
        ORDER BY zmmt0085~matnr.

        CLEAR cadastro_cargo_material-itens.

        LOOP AT _materiais_disp INTO DATA(_materiail_disp).
          SELECT SINGLE *
            FROM mard
            INTO @DATA(wzmard2)
           WHERE werks     = @cadastro_cargo_material-centro
           AND   matnr     = @_materiail_disp-matnr.
          IF sy-subrc = 0.
            APPEND VALUE #( status   = icon_led_green
                           matnr     = _materiail_disp-matnr
                           codagrepi = _materiail_disp-codagrepi
                           maktx     = me->get_material_description( _materiail_disp-matnr )
                         ) TO cadastro_cargo_material-itens.
          ELSE.
            APPEND VALUE #( status  = icon_incomplete
                          matnr     = _materiail_disp-matnr
                          codagrepi = _materiail_disp-codagrepi
                          maktx     = me->get_material_description( _materiail_disp-matnr )
                        ) TO cadastro_cargo_material-itens.
          ENDIF.
        ENDLOOP.

        IF cadastro_cargo_material-itens IS INITIAL.
          me->set_edit_mode( abap_true ).
        ENDIF.

*        ME->DISPLAY_CENTRO_CARGO( ).
*
*        MESSAGE COND #( LET X = LINES( _MATERIAIS_DISP ) IN
*                     WHEN X = 1 THEN |({ X }) material encontrado.|
*                     WHEN X = 0 OR X > 1 THEN |({ X }) materiais encontrados.| ) TYPE 'S'.
*        "
        SELECT cargo, matnr, codagrepi
            FROM zmmt0100
            INTO TABLE @DATA(_materiais_centro)
           WHERE werks       = @cadastro_cargo_material-centro
           AND   cargo       = @cadastro_cargo_material-cargo
           AND   codagrepi   = @cadastro_cargo_material-codagrepi
          ORDER BY matnr.


        CLEAR cadastro_cargo_material-itensc.

        LOOP AT _materiais_centro INTO DATA(_material_centro).
          APPEND VALUE #( status = icon_led_green
                         matnr  = _material_centro-matnr
                         codagrepi = _material_centro-codagrepi
                         maktx  = me->get_material_description( _material_centro-matnr )
                       ) TO cadastro_cargo_material-itensc.
        ENDLOOP.

        IF cadastro_cargo_material-itensc IS INITIAL.
          me->set_edit_mode( abap_true ).
        ENDIF.

        me->display_centro_cargo( ).

        MESSAGE COND #( LET x = lines( _materiais_centro ) IN
                     WHEN x = 1 THEN |({ x }) material encontrado.|
                     WHEN x = 0 OR x > 1 THEN |({ x }) materiais encontrados.| ) TYPE 'S'.


      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_fieldcatalog.

    IF tipo_vinculacao-cargo_agrupador = abap_true.
      result = VALUE #(
                        ( fieldname = 'STATUS'
                          icon      = abap_true
                          outputlen = 4 )

                        ( fieldname = 'CODAGREPI'
                          scrtext_m = 'Cód. Agrupador'
                          f4availabl = abap_true
                          outputlen = 15
                          edit      = me->get_edit_mode( )
                          no_zero   = abap_true
                           )

                        ( fieldname = 'DESAGREPI'
                          scrtext_m = 'Descrição'
                          edit      = abap_false
                          outputlen = 40 )

                        ( fieldname = 'DATA'
                          scrtext_m = 'Data'
                          edit      = abap_false
                          outputlen = 10 )

                        ( fieldname = 'HORA'
                          scrtext_m = 'Hora'
                          edit      = abap_false
                          outputlen = 10 )

                        ( fieldname = 'USUARIO'
                          scrtext_m = 'Usuario'
                          edit      = abap_false
                          outputlen = 20 )
                      ).
    ELSEIF tipo_vinculacao-material_agrupador = abap_true.
      result = VALUE #( ( fieldname = 'STATUS'
                         icon      = abap_true
                         outputlen = 4 )

                       ( fieldname = 'MATNR'
                         scrtext_m = 'Material'
                         ref_table = 'MAKT'
                         edit      = me->get_edit_mode( )
                         ref_field = 'MATNR' )

                       ( fieldname = 'MAKTX'
                         scrtext_m = 'Descrição'
                         edit      = abap_false
                         outputlen = 40 )
                      ( fieldname = 'DATA'
                          scrtext_m = 'Data'
                          edit      = abap_false
                          outputlen = 10 )

                        ( fieldname = 'HORA'
                          scrtext_m = 'Hora'
                          edit      = abap_false
                          outputlen = 10 )

                        ( fieldname = 'USUARIO'
                          scrtext_m = 'Usuario'
                          edit      = abap_false
                          outputlen = 20 )
                     ).
    ELSE.
      result = VALUE #( ( fieldname = 'STATUS'
                          icon      = abap_true
                          outputlen = 4 )

                        ( fieldname = 'MATNR'
                          scrtext_m = 'Material'
                          ref_table = 'MAKT'
                          edit      = abap_false
                          ref_field = 'MATNR' )
*
*                        (  FIELDNAME = 'CODAGREPI'
*                          SCRTEXT_M = 'Cód. Agrupador'
*                          F4AVAILABL = ABAP_TRUE
*                          OUTPUTLEN = 15
*                          EDIT      = ABAP_FALSE
*                          NO_ZERO   = ABAP_TRUE
*                           )

                        ( fieldname = 'MAKTX'
                          scrtext_m = 'Descrição'
                          edit      = abap_false
                          outputlen = 40 )
                      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_material_description.
    SELECT SINGLE maktx
      FROM makt
      INTO  result
     WHERE matnr = input.
  ENDMETHOD.

  METHOD get_epi_group_description.
    SELECT SINGLE denagrepi
      FROM zhrst_med_ag_epi
      INTO result
     WHERE codagrepi = input.
  ENDMETHOD.

  METHOD save_register.
    DATA epi_groups TYPE TABLE OF zmmt0080.
    DATA materials_for_epi_group TYPE TABLE OF zmmt0085.

    IF tipo_vinculacao-cargo_agrupador = abap_true.
      LOOP AT cadastro_cargo-itens ASSIGNING FIELD-SYMBOL(<fs_item>).
        CHECK <fs_item>-codagrepi IS NOT INITIAL.
        IF <fs_item>-status = icon_delete.
          DELETE FROM zmmt0080
          WHERE cargo     = cadastro_cargo-cargo
          AND   codagrepi = <fs_item>-codagrepi.
        ELSEIF <fs_item>-status NE icon_led_green.
          APPEND VALUE #( cargo = cadastro_cargo-cargo
                          codagrepi = <fs_item>-codagrepi
                          data      = sy-datum
                          hora      = sy-uzeit
                          usuario   = sy-uname  ) TO epi_groups.

          <fs_item>-status = icon_led_green.
        ENDIF.
      ENDLOOP.
      IF epi_groups[] IS NOT INITIAL.
        INSERT zmmt0080 FROM TABLE epi_groups.
      ENDIF.
      COMMIT WORK.
    ELSEIF ( tipo_vinculacao-material_agrupador = abap_true ).
      LOOP AT cadastro_agrupador_epi-itens ASSIGNING FIELD-SYMBOL(<fs_material>).
        IF <fs_material>-status = icon_delete.
          DELETE FROM zmmt0085
          WHERE codagrepi = cadastro_agrupador_epi-codigo
          AND   matnr     = <fs_material>-matnr.
        ELSEIF <fs_material>-status NE icon_led_green.
          APPEND VALUE #( codagrepi = cadastro_agrupador_epi-codigo
                          matnr = <fs_material>-matnr
                          data      = sy-datum
                          hora      = sy-uzeit
                          usuario   = sy-uname ) TO materials_for_epi_group.

          <fs_material>-status = icon_led_green.
        ENDIF.
      ENDLOOP.
      IF  materials_for_epi_group[] IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM materials_for_epi_group COMPARING codagrepi matnr.
***        INSERT zmmt0085 FROM TABLE materials_for_epi_group.
        MODIFY zmmt0085 FROM TABLE materials_for_epi_group.
      ENDIF.
      COMMIT WORK.
    ENDIF.

    MESSAGE text-004 TYPE 'S'.
    refresh_screen( ).
  ENDMETHOD.

  METHOD atualiza_centro.
    DATA selected_rows TYPE lvc_t_row.
    DATA selected_row  TYPE lvc_s_row.
    DATA materials_for_werks TYPE TABLE OF zmmt0100.
    DATA werro(1).

    cadastro_cargo_material-grid->get_selected_rows(
            IMPORTING et_index_rows = selected_rows ).

    SORT selected_rows DESCENDING BY index.

    LOOP AT selected_rows INTO selected_row.
      READ TABLE cadastro_cargo_material-itens INTO DATA(_itens2)  INDEX selected_row-index.
      SELECT SINGLE *
           FROM mard
           INTO @DATA(wzmard2)
           WHERE werks     = @cadastro_cargo_material-centro
           AND   matnr     = @_itens2-matnr.
      IF sy-subrc NE 0. "
        MESSAGE |EPI { _itens2-matnr }  não está expandido para a filial { cadastro_cargo_material-centro }| TYPE 'S'.
        werro = 'X'.
      ENDIF.
    ENDLOOP.

    IF werro = 'X'.
      EXIT.
    ENDIF.

    LOOP AT selected_rows INTO selected_row.
      READ TABLE cadastro_cargo_material-itens INTO DATA(_itens)  INDEX selected_row-index.
      APPEND VALUE #(  werks     = cadastro_cargo_material-centro
                       cargo     = cadastro_cargo_material-cargo
                       codagrepi = _itens-codagrepi
                       matnr     = _itens-matnr
                       data      = sy-datum
                       hora      = sy-uzeit
                       usuario   = sy-uname ) TO materials_for_werks.
    ENDLOOP.
    IF materials_for_werks[] IS NOT INITIAL.
      MODIFY zmmt0100 FROM TABLE materials_for_werks.
      COMMIT WORK.

    ENDIF.

    me->search_register( ).

  ENDMETHOD.

  METHOD insere_novo.

    DATA materials_for_werks     TYPE TABLE OF zmmt0100.
    DATA epi_groups              TYPE TABLE OF zmmt0080.
    DATA materials_for_epi_group TYPE TABLE OF zmmt0085.

    IF cadastro_cargo_material-matnr IS INITIAL.
      MESSAGE 'Informe o EPI!' TYPE 'S'.
      EXIT.
    ENDIF.
    "
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cadastro_cargo_material-matnr
      IMPORTING
        output = cadastro_cargo_material-matnr.

    SELECT SINGLE mtart
      FROM mara
      INTO @DATA(vmtart)
      WHERE matnr = @cadastro_cargo_material-matnr.

    IF NOT ( 'ZEPI_ZSSO' CS vmtart ).
      MESSAGE 'Material não é EPI!' TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
        FROM mard
        INTO @DATA(wzmard)
        WHERE werks     = @cadastro_cargo_material-centro
        AND   matnr     = @cadastro_cargo_material-matnr.

    IF sy-subrc NE 0.
      MESSAGE |EPI não está expandido para a filial { cadastro_cargo_material-centro }| TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
    FROM zmmt0100
    INTO @DATA(wzmmt0100)
    WHERE werks     = @cadastro_cargo_material-centro
    AND   cargo     = @cadastro_cargo_material-cargo
    "AND   CODAGREPI = @CADASTRO_CARGO_MATERIAL-CODAGREPI
    AND   matnr     = @cadastro_cargo_material-matnr.

    IF sy-subrc = 0.
      MESSAGE |EPI já disponivel neste centro, Grupo de EPI { wzmmt0100-codagrepi } Cargo { wzmmt0100-cargo }| TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM zmmt0085
      INTO @DATA(w085)
      WHERE matnr     = @cadastro_cargo_material-matnr.

    IF sy-subrc = 0.
      MESSAGE |Este Material está vinculado ao Grupo de EPI { w085-codagrepi } |  TYPE 'S'.
      EXIT.
    ENDIF.

    APPEND VALUE #( werks     = cadastro_cargo_material-centro
                    cargo     = cadastro_cargo_material-cargo
                    codagrepi = cadastro_cargo_material-codagrepi
                    matnr     = cadastro_cargo_material-matnr
                    data      = sy-datum
                    hora      = sy-uzeit
                    usuario   = sy-uname ) TO materials_for_werks.

    APPEND VALUE #( cargo     = cadastro_cargo_material-cargo
                    codagrepi = cadastro_cargo_material-codagrepi
                    data      = sy-datum
                    hora      = sy-uzeit
                    usuario   = sy-uname ) TO epi_groups.


    APPEND VALUE #( codagrepi = cadastro_cargo_material-codagrepi
                    matnr     = cadastro_cargo_material-matnr
                    data      = sy-datum
                    hora      = sy-uzeit
                    usuario   = sy-uname ) TO materials_for_epi_group.


    IF materials_for_werks[] IS NOT INITIAL.
      MODIFY zmmt0100 FROM TABLE materials_for_werks.
      MODIFY zmmt0080 FROM TABLE epi_groups.
      MODIFY zmmt0085 FROM TABLE materials_for_epi_group.

      COMMIT WORK.

    ENDIF.

    me->search_register( ).

  ENDMETHOD.

  METHOD insere_novo_agrupador.

    DATA epi_groups              TYPE TABLE OF zmmt0080.

    IF cadastro_cargo_material-cargo IS NOT INITIAL
         AND cadastro_cargo_material-codagrepi IS NOT INITIAL.

      SELECT SINGLE *
      FROM zmmt0080
      INTO @DATA(wzmmt0100)
      WHERE cargo     = @cadastro_cargo_material-cargo
      AND   codagrepi = @cadastro_cargo_material-codagrepi.

      IF sy-subrc = 0.
        MESSAGE |Cargo x Agrupador já cadastrado!| TYPE 'S'.
        EXIT.
      ENDIF.

      APPEND VALUE #( cargo     = cadastro_cargo_material-cargo
                      codagrepi = cadastro_cargo_material-codagrepi
                      data      = sy-datum
                      hora      = sy-uzeit
                      usuario   = sy-uname ) TO epi_groups.

      MODIFY zmmt0080 FROM TABLE epi_groups.
      COMMIT WORK.
      IF sy-subrc = 0.
        MESSAGE |Cargo x Agrupador cadastrado sucesso!| TYPE 'S'.
        EXIT.
      ENDIF.

    ENDIF.
    REFRESH: epi_groups.

  ENDMETHOD.

  METHOD atualiza_disp.
    DATA selected_rows TYPE lvc_t_row.
    DATA selected_row  TYPE lvc_s_row.
    DATA materials_for_werks TYPE TABLE OF zmmt0100.

    cadastro_cargo_material-gridc->get_selected_rows(
            IMPORTING et_index_rows = selected_rows ).

    SORT selected_rows DESCENDING BY index.

    LOOP AT selected_rows INTO selected_row.
      READ TABLE cadastro_cargo_material-itensc INTO DATA(_itens)  INDEX selected_row-index.
      DELETE FROM zmmt0100 WHERE werks       = cadastro_cargo_material-centro
                           AND   cargo       = cadastro_cargo_material-cargo
                           AND   codagrepi   = _itens-codagrepi
                           AND   matnr       = _itens-matnr.
    ENDLOOP.
    IF selected_rows[] IS NOT INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA main TYPE REF TO cl_main.

MODULE output OUTPUT.
  IF ( main IS INITIAL ).
    main = NEW cl_main( ).

    main->display_agrupadores( ).
  ENDIF.

  main->set_status( ).
  main->set_title( ).
  main->process_before_output( ).
ENDMODULE.

MODULE input INPUT.
  main->user_command( ucomm = sy-ucomm ).
ENDMODULE.
