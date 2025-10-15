*&---------------------------------------------------------------------*
*& Report  ZPMR0029
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr004.

TYPES:
  BEGIN OF ty_children_lote,
    main_charg     TYPE zppt0011-ebeln,
    children_charg TYPE zppt0011-charg,
    clabs          TYPE zppt0011-clabs,
    lgort          TYPE zppt0011-lgort,
    matnr          TYPE zppt0011-matnr,
    werks          TYPE zppt0011-werks,
    vfdat          TYPE zppt0011-vfdat,
    lfabr          TYPE zppt0011-lfabr,
    mblnr          TYPE zppt0011-mblnr,
    mjahr          TYPE zppt0011-mjahr,
    lvorm          TYPE zppt0011-lvorm,
    rsnum          TYPE zppt0011-rsnum,
    color          TYPE lvc_t_scol,
    style          TYPE lvc_t_styl,
  END OF ty_children_lote,

  ty_t_children_lote TYPE TABLE OF ty_children_lote
    WITH DEFAULT KEY,

  ty_t_zppt0011      TYPE TABLE OF zppt0011
    WITH DEFAULT KEY,

  BEGIN OF ty_main_lote,
    ebeln           TYPE ekpo-ebeln,
    ebelp           TYPE ekpo-ebelp,
    matnr           TYPE ekpo-matnr,
    lgort           TYPE ekpo-lgort,
    werks           TYPE ekpo-werks,
    maktx           TYPE makt-maktx,
    clabs           TYPE mchb-clabs,
    children_lotes  TYPE ty_t_children_lote,
    count_childrens TYPE string,
  END OF ty_main_lote,

  ty_t_main_lote TYPE TABLE OF ty_main_lote,

  BEGIN OF ty_screen_main,
    ebeln     TYPE ekko-ebeln,
    qtd_lotes TYPE char5,
  END OF ty_screen_main,

  BEGIN OF ty_fields,
    field TYPE screen-name,
    group TYPE screen-group1,
    value TYPE screen-active,
  END OF ty_fields.

CONSTANTS:
  BEGIN OF c_screen,
    empty              TYPE sy-dynnr VALUE '0005',
    main               TYPE sy-dynnr VALUE '0001',
    display            TYPE sy-dynnr VALUE '0002',
    display_explosions TYPE sy-dynnr VALUE '0003',
  END OF c_screen.

"//Screen dynpro fields
DATA main_screen_data      TYPE ty_screen_main.
DATA btn_salvar            TYPE string.
DATA quantidade_lotes      TYPE char5.
*DATA EXPLOSION_SCREEN_DATA TYPE TY_SCREEN_EXPLOSION.

"//Subscreen
DATA display_subscreen     TYPE sy-dynnr VALUE c_screen-empty.

"//Outtab
DATA outtab_main_lotes     TYPE TABLE OF ty_main_lote.
DATA outtab_children_lotes TYPE TABLE OF ty_children_lote.

"//Classes definitions;
CLASS cx_local_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION.

    METHODS get_texts
      RETURNING VALUE(return) TYPE bapiret2_t.

    METHODS constructor
      IMPORTING
        text  TYPE itex132    OPTIONAL
        texts TYPE bapiret2_t OPTIONAL.

    METHODS message
      IMPORTING
        type    TYPE c
        display TYPE c OPTIONAL.

    METHODS display.

    DATA text   TYPE itex132.
    DATA return TYPE bapiret2_t.
ENDCLASS.

CLASS cl_main_app DEFINITION ABSTRACT.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_alv_kind,
        grid TYPE string VALUE 'grid',
        tree TYPE string VALUE 'tree',
      END OF c_alv_kind.

    EVENTS evt_refresh_screen.

    CLASS-EVENTS action_input
      EXPORTING
        VALUE(ucomm) TYPE sy-ucomm
        VALUE(dynnr) TYPE sy-dynnr.

    METHODS display_alv ABSTRACT.

    METHODS process_before_output ABSTRACT.

    METHODS process_after_output  ABSTRACT FOR EVENT action_input OF cl_main_app
      IMPORTING ucomm dynnr.

    METHODS refresh_screen ABSTRACT FOR EVENT evt_refresh_screen OF cl_main_app
      IMPORTING
        sender.

    DATA alv_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS run.
    CLASS-METHODS commit.
    CLASS-METHODS factory_process_input.
    CLASS-METHODS factory_process_output
      IMPORTING
        dynnr            TYPE sy-dynnr
      RETURNING
        VALUE(reference) TYPE REF TO cl_main_app.

    CLASS-METHODS set_current_screen
      IMPORTING
        screen TYPE sy-dynnr.

    CLASS-METHODS get_current_screen
      RETURNING VALUE(screen) TYPE sy-dynnr.

    CLASS-DATA current_screen TYPE sy-dynnr.
ENDCLASS.

CLASS cl_display DEFINITION INHERITING FROM cl_main_app.
  PUBLIC SECTION.
    METHODS set_alv_kind
      IMPORTING
        alv TYPE string.

    METHODS get_alv_kind
      RETURNING VALUE(alv) TYPE string.

    METHODS get_fieldcatalog
      RETURNING VALUE(fieldscatalog) TYPE lvc_t_fcat.

    METHODS get_outtab
      EXPORTING outtab TYPE ANY TABLE.

    METHODS set_main_subscreen
      IMPORTING
        screen TYPE sy-dynnr.

    METHODS get_main_subscreen
      RETURNING VALUE(screen) TYPE sy-dynnr.

    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        es_row_no
        e_column
        e_row.

    METHODS display_alv           REDEFINITION.
    METHODS process_before_output REDEFINITION.
    METHODS process_after_output  REDEFINITION.
    METHODS refresh_screen        REDEFINITION.

  PRIVATE SECTION.
    DATA alv_kind         TYPE string.
    DATA custom_display   TYPE REF TO cl_gui_custom_container.
    "DATA ALV_GRID         TYPE REF TO CL_GUI_ALV_GRID.
ENDCLASS.

CLASS cl_display_explosions DEFINITION INHERITING FROM cl_display.
  PUBLIC SECTION.
    METHODS display_alv           REDEFINITION.
    METHODS process_before_output REDEFINITION.
    METHODS process_after_output  REDEFINITION.
    METHODS get_fieldcatalog      REDEFINITION.
    METHODS get_outtab            REDEFINITION.
    METHODS refresh_screen        REDEFINITION.

    METHODS set_layout
      CHANGING
        layout TYPE lvc_s_layo.

    METHODS set_screen_buttons.
    METHODS set_screen_fields.

    METHODS set_initial_children_lotes
      IMPORTING
        qtd TYPE char5.

    METHODS set_children_lotes.

    METHODS is_ready_to_save
      RAISING cx_local_exception.

    METHODS save
      RAISING
        cx_local_exception.

    METHODS storno
      RAISING
        cx_local_exception.

    METHODS is_saved
      RETURNING
        VALUE(value) TYPE abap_bool.

    METHODS print_tags
      IMPORTING
        selected_rows TYPE lvc_t_row.

    METHODS insert_children_lote
      IMPORTING
        row TYPE ty_children_lote.

    METHODS modify_children_lotes
      IMPORTING
        main_lote TYPE ty_main_lote OPTIONAL
        items     TYPE ty_t_children_lote.

    METHODS get_next_children_charg
      IMPORTING
        pedido         TYPE ekpo-ebeln OPTIONAL
        material       TYPE mchb-matnr OPTIONAL
        centro         TYPE mchb-werks OPTIONAL
        deposito       TYPE mchb-lgort OPTIONAL
      CHANGING
        children_charg TYPE zppt0011-charg.

    METHODS create_batch
      IMPORTING
        material        TYPE mchb-matnr
        centro          TYPE mchb-werks
        deposito        TYPE mchb-lgort
        lote            TYPE mchb-charg
        dt_vencimento   TYPE zppt0011-vfdat
        lote_fabricante TYPE zppt0011-lfabr
      RAISING
        cx_local_exception.

    METHODS set_batch_deletion_flag
      IMPORTING
        material TYPE mchb-matnr
        centro   TYPE mchb-werks
        deposito TYPE mchb-lgort
        lote     TYPE mchb-charg
      RAISING
        cx_local_exception.

    METHODS transfer_from_batch_to_batch
      IMPORTING
        header   TYPE bapi2017_gm_head_01
        items    TYPE bapi2017_gm_item_create_t
      EXPORTING
        document TYPE bapi2017_gm_head_ret
        return   TYPE bapiret2_t
      RAISING
        cx_local_exception.

    METHODS cancel_batch_movement
      IMPORTING
        document TYPE bapi2017_gm_head_ret
      RAISING
        cx_local_exception.

    METHODS set_current_main_lote
      IMPORTING
        main_lote TYPE ty_main_lote.

    METHODS get_current_main_lote
      RETURNING VALUE(main_lote) TYPE ty_main_lote.

    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    "DATA DISPLAY_CONTROLLER TYPE REF TO CL_DISPLAY.
    DATA custom_explosions  TYPE REF TO cl_gui_custom_container.
    "DATA ALV_GRID           TYPE REF TO CL_GUI_ALV_GRID.

  PRIVATE SECTION.
    DATA current_main_lote TYPE ty_main_lote.
    DATA data_is_saved     TYPE abap_bool.
    DATA children_charg    TYPE ty_children_lote-children_charg.
ENDCLASS.


CLASS cl_main DEFINITION INHERITING FROM cl_main_app.
  PUBLIC SECTION.
    "TYPES TY_T_CHILDREN_LOTE TYPE TABLE OF ZPPT0011 WITH DEFAULT KEY.

    METHODS constructor.
    METHODS process_before_output REDEFINITION.
    METHODS process_after_output  REDEFINITION.
    METHODS display_alv           REDEFINITION.
    METHODS refresh_screen        REDEFINITION.

    METHODS set_title.
    METHODS set_status.
    METHODS display_logo.

    METHODS get_charg_available_balance
      IMPORTING
        material                 TYPE zppt0011-matnr
        centro                   TYPE zppt0011-werks
        deposito                 TYPE zppt0011-lgort
        lote                     TYPE zppt0011-charg
      RETURNING
        VALUE(available_balance) TYPE mchb-clabs.

    METHODS select_main_lotes
      IMPORTING
        pedido TYPE ekko-ebeln
      CHANGING
        items  TYPE ty_t_main_lote
      RAISING
        cx_local_exception.

    METHODS get_children_lotes
      IMPORTING
        pedido       TYPE zppt0011-ebeln
        material     TYPE zppt0011-matnr
        centro       TYPE zppt0011-werks
        deposito     TYPE zppt0011-lgort
      EXPORTING
        volume_total TYPE zppt0011-clabs
      RETURNING
        VALUE(items) TYPE ty_t_children_lote.

    METHODS is_purchasing_number_valid
      IMPORTING
        purchasing_number TYPE ekko-ebeln.

    "//Data declarations
    "DATA DISPLAY_CONTROLLER    TYPE REF TO CL_DISPLAY.
    "DATA EXPLOSIONS_CONTROLLER TYPE REF TO CL_DISPLAY_EXPLOSIONS.
ENDCLASS.

"//Classes implementations;

DATA main_reference              TYPE REF TO cl_main.
DATA display_reference           TYPE REF TO cl_display.
DATA display_explosion_reference TYPE REF TO cl_display_explosions.

CLASS cl_main_app IMPLEMENTATION.
  METHOD run.

    main_reference = NEW cl_main( ) .
    display_reference = NEW cl_display( ).
    display_explosion_reference = NEW cl_display_explosions( ).

    CALL SCREEN 0001.
  ENDMETHOD.

  METHOD commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.

  METHOD factory_process_output.
    cl_main_app=>set_current_screen( dynnr ).

    main_reference =
      COND #( WHEN main_reference IS INITIAL
              THEN NEW cl_main( ) ELSE main_reference
    ).

    display_reference =
      COND #( WHEN display_reference IS INITIAL
              THEN NEW cl_display( )
              ELSE display_reference
    ).

    display_explosion_reference =
      COND #( WHEN display_explosion_reference IS INITIAL
              THEN NEW cl_display_explosions( )
              ELSE display_explosion_reference
    ).

    CASE dynnr.
      WHEN c_screen-main.
        main_reference->process_before_output( ).

      WHEN c_screen-display.
        display_reference->process_before_output( ).

      WHEN c_screen-display_explosions.
        display_explosion_reference->process_before_output( ).

      WHEN c_screen-empty.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD set_current_screen.
    cl_main_app=>current_screen = screen.
  ENDMETHOD.

  METHOD get_current_screen.
    MOVE cl_main_app=>current_screen TO screen.
  ENDMETHOD.

  METHOD factory_process_input.
    RAISE EVENT action_input EXPORTING ucomm = sy-ucomm dynnr = sy-dynnr.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  cl_main_app=>run( ).

MODULE process_after_input.
  cl_main_app=>factory_process_input( ).
ENDMODULE.

MODULE process_before_output OUTPUT.
  cl_main_app=>factory_process_output( sy-dynnr ).
ENDMODULE.

CLASS cx_local_exception IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    me->text   = text.
    me->return = texts.
  ENDMETHOD.

  METHOD get_text.
    MOVE me->text TO result.
  ENDMETHOD.

  METHOD get_texts.
    MOVE me->return TO return.
  ENDMETHOD.

  METHOD display.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = me->return.
  ENDMETHOD.

  METHOD message.
    MESSAGE me->get_text( ) TYPE type DISPLAY LIKE display.
  ENDMETHOD.
ENDCLASS.

CLASS cl_main IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    "CREATE OBJECT ME->DISPLAY_CONTROLLER.
    "CREATE OBJECT ME->EXPLOSIONS_CONTROLLER.
  ENDMETHOD.

  METHOD is_purchasing_number_valid.
*    SELECT SINGLE MATNR
*      FROM EKPO
*      INTO @DATA(_MATERIAL_NUMBER).

*    select single *
*      from mchb
*      into @data(_lote)
*     WHERE charg = PURCHASING_NUMBER.

  ENDMETHOD.

  METHOD display_alv.
  ENDMETHOD.

  METHOD refresh_screen.
  ENDMETHOD.

  METHOD set_title.
    SET TITLEBAR '0001'.
  ENDMETHOD.

  METHOD set_status.
    SET PF-STATUS '0001'.
  ENDMETHOD.

  METHOD display_logo.
    DATA custom_picture   TYPE REF TO cl_gui_custom_container.
    DATA picture_instance   TYPE REF TO cl_gui_picture.

    DATA(_image_helper) = NEW zcl_image_helper( ).

    CALL METHOD _image_helper->display
      EXPORTING
        custom_name      = 'CUSTOM_LOGO'
        url              = _image_helper->get_photo_url( name = 'LOGO_AMAGGI' )
      CHANGING
        custom_instance  = custom_picture
        picture_instance = picture_instance.
  ENDMETHOD.

  METHOD get_charg_available_balance.

    DATA: lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    SELECT SINGLE clabs
      FROM mchb
      INTO available_balance
     WHERE matnr = lv_matnr
       AND werks = centro
       AND lgort = deposito
       AND charg = lote.
  ENDMETHOD.

  METHOD select_main_lotes.
    SELECT *
      FROM ekpo AS a
      INTO CORRESPONDING FIELDS OF TABLE items
     WHERE ebeln = pedido.

*    SELECT *
*      FROM ZPPT0011
*      INTO TABLE @DATA(_CHILDREN_LOTES)
*   FOR ALL ENTRIES IN @ITEMS
*     WHERE EBELN = @ITEMS-EBELN.

    LOOP AT items ASSIGNING FIELD-SYMBOL(<fs_data>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_data>-matnr
        IMPORTING
          output       = <fs_data>-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.


      CALL METHOD me->get_children_lotes
        EXPORTING
          pedido       = <fs_data>-ebeln
          material     = <fs_data>-matnr
          centro       = <fs_data>-werks
          deposito     = <fs_data>-lgort
        IMPORTING
          volume_total = <fs_data>-clabs
        RECEIVING
          items        = <fs_data>-children_lotes.

      IF <fs_data>-clabs IS INITIAL.
        <fs_data>-clabs = me->get_charg_available_balance( material = <fs_data>-matnr centro = <fs_data>-werks deposito = <fs_data>-lgort lote = <fs_data>-ebeln ).
      ENDIF.

      <fs_data>-count_childrens = |{ icon_batch } ({ lines( <fs_data>-children_lotes ) })|.

      "//select material description;
      SELECT SINGLE maktx
        FROM makt
        INTO <fs_data>-maktx
       WHERE matnr = <fs_data>-matnr.
    ENDLOOP.

    CHECK items IS INITIAL.
    RAISE EXCEPTION TYPE cx_local_exception
      EXPORTING
        text = |Nenhum item encontrado no pedido { pedido }.|.

  ENDMETHOD.

  METHOD get_children_lotes.
    DATA: lv_material TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT *
      FROM zppt0011
      INTO TABLE @DATA(_children_lotes)
     WHERE ebeln = @pedido
       AND matnr = @lv_material
       AND werks = @centro
       AND lgort = @deposito
       AND lvorm = @abap_false.

    LOOP AT _children_lotes INTO DATA(_children_lote).
      ADD _children_lote-clabs TO volume_total.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _children_lote-matnr
        IMPORTING
          output       = _children_lote-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      APPEND VALUE #( main_charg     = _children_lote-ebeln
                      children_charg = _children_lote-charg
                      clabs          = _children_lote-clabs
                      lgort          = _children_lote-lgort
                      matnr          = _children_lote-matnr
                      werks          = _children_lote-werks
                      vfdat          = _children_lote-vfdat
                      lfabr          = _children_lote-lfabr
                      mblnr          = _children_lote-mblnr
                      mjahr          = _children_lote-mjahr
                      rsnum          = _children_lote-rsnum
                    ) TO items.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_before_output.
    SET HANDLER process_after_output.

    me->display_logo( ). "//Set header logo
    me->set_status( ).   "//Set status-bar
    me->set_title( ).    "//Set title-bar
  ENDMETHOD.

  METHOD process_after_output.
    SET HANDLER process_after_output
     ACTIVATION abap_off.

    CASE ucomm.
      WHEN 'EXIT' OR 'CANCEL'.
        LEAVE PROGRAM.

      WHEN 'BACK'.
        IF ( display_subscreen = c_screen-display_explosions ).
          display_reference->set_main_subscreen( c_screen-display ).
        ELSE.
          LEAVE PROGRAM.
        ENDIF.

      WHEN 'ENTER'.
        CHECK ( cl_main_app=>get_current_screen( ) = c_screen-empty   )
           OR ( cl_main_app=>get_current_screen( ) = c_screen-display ).

        TRY.
            CALL METHOD me->select_main_lotes
              EXPORTING
                pedido = main_screen_data-ebeln
              CHANGING
                items  = outtab_main_lotes.

            "//Create new instances to work;
            "DISPLAY_EXPLOSION_REFERENCE = NEW CL_DISPLAY_EXPLOSIONS( ).
            "DISPLAY_REFERENCE           = NEW CL_DISPLAY( ).

            "//Set screen;
            display_reference->set_main_subscreen( c_screen-display ).
            display_reference->set_alv_kind( 'grid' ).

          CATCH cx_local_exception INTO DATA(_cx).
            _cx->message( type = 'S' display = 'E' ).
        ENDTRY.
      WHEN OTHERS.
    ENDCASE.


    "SET HANDLER PROCESS_AFTER_OUTPUT ACTIVATION ABAP_OFF.
  ENDMETHOD.
ENDCLASS.

CLASS cl_display IMPLEMENTATION.
  METHOD process_after_output.
    SET HANDLER process_after_output
     ACTIVATION abap_off.

    "CHECK DYNNR = CL_MAIN_APP=>GET_CURRENT_SCREEN( ).
  ENDMETHOD.

  METHOD refresh_screen.
    CHECK sender->alv_grid IS NOT INITIAL.
    sender->alv_grid->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
  ENDMETHOD.

  METHOD process_before_output.
    "SET HANDLER PROCESS_AFTER_OUTPUT.
    SET HANDLER refresh_screen FOR ALL INSTANCES.

    me->display_alv( ).
  ENDMETHOD.


  METHOD display_alv.

    CASE me->get_alv_kind( ).
      WHEN 'grid'.

        IF me->custom_display IS INITIAL.

          CREATE OBJECT me->custom_display
            EXPORTING
              container_name = 'CUSTOM_DISPLAY'.

          CREATE OBJECT me->alv_grid
            EXPORTING
              i_parent = custom_display.

          DATA(_fieldcatalog) = me->get_fieldcatalog( ).

          me->alv_grid->set_table_for_first_display(
*          EXPORTING
*            I_BUFFER_ACTIVE               =
*            I_BYPASSING_BUFFER            =
*            I_CONSISTENCY_CHECK           =
*            I_STRUCTURE_NAME              =
*            IS_VARIANT                    =
*            I_SAVE                        =
*            I_DEFAULT                     = 'X'
*            IS_LAYOUT                     =
*            IS_PRINT                      =
*            IT_SPECIAL_GROUPS             =
*            IT_TOOLBAR_EXCLUDING          =
*            IT_HYPERLINK                  =
*            IT_ALV_GRAPHICS               =
*            IT_EXCEPT_QINFO               =
*            IR_SALV_ADAPTER               =
            CHANGING
              it_outtab                     = outtab_main_lotes
            it_fieldcatalog               = _fieldcatalog
*            IT_SORT                       =
*            IT_FILTER                     =
*          EXCEPTIONS
*            INVALID_PARAMETER_COMBINATION = 1
*            PROGRAM_ERROR                 = 2
*            TOO_MANY_LINES                = 3
*            OTHERS                        = 4
          ).
          IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.

          SET HANDLER me->handle_double_click FOR alv_grid.

        ELSE.
          me->refresh_screen( me ).
        ENDIF.

      WHEN 'tree'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD set_alv_kind.
    MOVE alv TO me->alv_kind.
  ENDMETHOD.

  METHOD get_alv_kind.
    MOVE me->alv_kind TO alv.
  ENDMETHOD.

  METHOD get_fieldcatalog.
*    DATA STR TYPE REF TO DATA.
*
*    ASSIGN 'TY_MAIN_LOTE' TO FIELD-SYMBOL(<FS_STR>).
*    CREATE DATA STR TYPE (<FS_STR>).
*
*    FIELDSCATALOG = CORRESPONDING #(
*                              CL_SALV_DATA_DESCR=>READ_STRUCTDESCR(
*                                CAST CL_ABAP_STRUCTDESCR(
*                                     CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( STR )
*                                                ) )
*                             ).
*    FIELDSCATALOG = ZCL_UTIL=>GET_STRUCTURE_DESCRIPTION( 'TY_LOTE' ).


    fieldscatalog =
        VALUE #(
        ( fieldname = 'EBELN' coltext = 'Pedido'     outputlen = 12 )
        ( fieldname = 'EBELP' coltext = 'Item'       outputlen = 5 )
        ( fieldname = 'MATNR' coltext = 'Material'   outputlen = 10 no_zero = abap_true )
        ( fieldname = 'WERKS' coltext = 'Centro'     outputlen = 7 )
        ( fieldname = 'LGORT' coltext = 'Depósito'   outputlen = 10 )
        ( fieldname = 'MAKTX' coltext = 'Descrição'  outputlen = 25 )
        ( fieldname = 'CLABS' coltext = 'Quantidade' outputlen = 10 )
        ( fieldname = 'COUNT_CHILDRENS' coltext   = '' outputlen = 7 )
        ).


*    EBELN TYPE EKPO-EBELN,
*    EBELP TYPE EKPO-EBELP,
*    MATNR TYPE EKPO-MATNR,
*    LGORT TYPE EKPO-LGORT,
*    WERKS TYPE EKPO-WERKS,
*    MAKTX TYPE MAKT-MAKTX,
  ENDMETHOD.

  METHOD get_outtab.
    MOVE outtab_main_lotes TO outtab.
  ENDMETHOD.

  METHOD set_main_subscreen.
    display_subscreen = screen.
    "MOVE ME TO REF.
  ENDMETHOD.

  METHOD get_main_subscreen.
    screen = display_subscreen.
  ENDMETHOD.

  METHOD handle_double_click.
    DATA main_lotes TYPE TABLE OF ty_main_lote.

    CALL METHOD me->get_outtab
      IMPORTING
        outtab = main_lotes.

    TRY.
        "//Set current selected main lote to be used inside DISPLAY_EXPLOSION_REFERENCE;
        display_explosion_reference->set_current_main_lote( main_lotes[ e_row-index ] ).
        display_explosion_reference->set_children_lotes( ).

        "//Set a new screen to be loaded in DISPLAY_SUBSCREEN;
        CALL METHOD me->set_main_subscreen
          EXPORTING
            screen = c_screen-display_explosions.

        LEAVE TO SCREEN 0001.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS cl_display_explosions IMPLEMENTATION.
  METHOD display_alv.
    DATA layout TYPE lvc_s_layo.

    "//Get fieldcatalog;
    DATA(_fieldscatalog) = me->get_fieldcatalog( ).

    "//Set layout
    CALL METHOD me->set_layout
      CHANGING
        layout = layout.

    IF ( me->custom_explosions IS INITIAL ).
      CREATE OBJECT me->custom_explosions
        EXPORTING
          container_name = 'CUSTOM_EXPLOSION'.

      CREATE OBJECT me->alv_grid
        EXPORTING
          i_parent = me->custom_explosions.

      SET HANDLER me->handle_set_toolbar  FOR me->alv_grid.
      SET HANDLER me->handle_user_command FOR me->alv_grid.

      CALL METHOD me->alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = layout
        CHANGING
          it_outtab       = outtab_children_lotes
          it_fieldcatalog = _fieldscatalog.

      me->alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      me->alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter    ).

    ELSE.
      me->alv_grid->set_frontend_layout( layout ).
    ENDIF.

  ENDMETHOD.

  METHOD process_before_output.
    SET HANDLER process_after_output.
    SET HANDLER refresh_screen FOR ALL INSTANCES.

    me->set_screen_buttons( ).
    me->set_screen_fields( ).
    me->display_alv( ).
  ENDMETHOD.

  METHOD process_after_output.
    SET HANDLER process_after_output
     ACTIVATION abap_off.

    "CHECK DYNNR = CL_MAIN_APP=>GET_CURRENT_SCREEN( ).

    CASE ucomm.
      WHEN 'BACK_TO_MAIN'.

        "//Set a new screen to be loaded in DISPLAY_SUBSCREEN;
        CALL METHOD me->set_main_subscreen
          EXPORTING
            screen = c_screen-display.

      WHEN 'ENTER'.

        me->set_initial_children_lotes( main_screen_data-qtd_lotes ).

      WHEN 'SAVE'.
        TRY.
            IF ( me->is_saved( ) = abap_true ).
              me->storno( ).
            ELSE.
              me->is_ready_to_save( ).
              me->save( ).
            ENDIF.

          CATCH cx_local_exception INTO DATA(_cx).
            IF _cx->get_text( ) IS INITIAL.
              _cx->display( ).
            ELSE.
              MESSAGE _cx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD get_fieldcatalog.
    fieldscatalog = VALUE #(
        ( fieldname = 'MAIN_CHARG'       coltext = 'Lote principal'  outputlen = 12 key = abap_true                      )
        ( fieldname = 'MATNR'            coltext = 'Material'        outputlen = 10 key = abap_true no_zero = abap_true  )
        ( fieldname = 'WERKS'            coltext = 'Centro'          outputlen = 10 key = abap_true                      )
        ( fieldname = 'LGORT'            coltext = 'Depósito'        outputlen = 10 key = abap_true                      )
        ( fieldname = 'CHILDREN_CHARG'   coltext = 'Lote'            outputlen = 12                                      )
        ( fieldname = 'MBLNR'            coltext = 'Documento'       outputlen = 10                                      )
        ( fieldname = 'CLABS'            coltext = 'Quantidade'      outputlen = 10                                      )

        ( fieldname = 'VFDAT' coltext = 'Data Validade'   outputlen = 13 edit = abap_true ref_table = 'ZPPT0011' ref_field = 'VFDAT' )
        ( fieldname = 'LFABR' coltext = 'Lote Fabricante' outputlen = 15 edit = abap_true                                                    )
    ).
  ENDMETHOD.

  METHOD set_layout.
    DATA(_main_lote) = me->get_current_main_lote( ).

    layout-grid_title = |{ _main_lote-ebeln } / { _main_lote-maktx } / { _main_lote-werks } / { _main_lote-lgort } /|.
    layout-ctab_fname = 'COLOR'.
    layout-stylefname = 'STYLE'.
*    LAYOUT-NO_ROWMARK = ABAP_TRUE.
    layout-sel_mode   = 'D'.
*    LAYOUT-NO_TOOLBAR = ABAP_TRUE.
  ENDMETHOD.

  METHOD set_screen_buttons.
    IF me->is_saved( ) = abap_true.
      btn_salvar = |{ icon_storno } Estornar|.
    ELSE.
      btn_salvar = |{ icon_system_save } Salvar|.
    ENDIF.
  ENDMETHOD.

  METHOD set_screen_fields.
* Description:
*  - GR1 = MAIN_SCREEN_DATA-QTD_LOTES

    LOOP AT SCREEN.
      IF ( screen-group1 = 'GR1' ).
        IF me->is_saved( ) = abap_true .
          screen-invisible = 1.
          screen-input     = 0.
        ELSE.
          screen-invisible = 0.
          screen-input     = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_initial_children_lotes.
    CLEAR outtab_children_lotes.

    DATA(_main_lote) = me->get_current_main_lote( ).
    DATA(_color)     = VALUE lvc_s_colo( col = col_key int = 1 ).

    DO qtd TIMES.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _main_lote-matnr
        IMPORTING
          output       = _main_lote-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.


      APPEND VALUE #(
        main_charg = _main_lote-ebeln
        lgort      = _main_lote-lgort
        matnr      = _main_lote-matnr
        werks      = _main_lote-werks
        clabs      = _main_lote-clabs / qtd
        color      = VALUE lvc_t_scol( ( fname = 'MAIN_CHARG' color = _color )
                                       ( fname = 'MATNR'      color = _color )
                                       ( fname = 'WERKS'      color = _color )
                                       ( fname = 'LGORT'      color = _color )
                                     )
        ) TO outtab_children_lotes.
    ENDDO.

    me->set_screen_buttons( ).
    RAISE EVENT evt_refresh_screen.
    "ME->ALV_GRID->REFRESH_TABLE_DISPLAY( ).

  ENDMETHOD.

  METHOD set_children_lotes.
    DATA(_color) = VALUE lvc_s_colo( col = col_key int = 1 ).
    DATA(_item) = me->get_current_main_lote( ).

    CLEAR outtab_children_lotes.
    LOOP AT _item-children_lotes INTO DATA(_children_lote).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _children_lote-matnr
        IMPORTING
          output       = _children_lote-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      APPEND VALUE #(
        main_charg = _children_lote-main_charg
        lgort      = _children_lote-lgort
        matnr      = _children_lote-matnr
        werks      = _children_lote-werks
        clabs      = _children_lote-clabs
        children_charg = _children_lote-children_charg
        vfdat      = _children_lote-vfdat
        lfabr      = _children_lote-lfabr
        mblnr      = _children_lote-mblnr
        mjahr      = _children_lote-mjahr
        color      = VALUE #( LET x = _color IN ( fname = 'MAIN_CHARG' color = x ) ( fname = 'MATNR' color = x ) ( fname = 'WERKS' color = x ) ( fname = 'LGORT' color = x ) )
        style      = VALUE #( LET y = cl_gui_alv_grid=>mc_style_disabled IN ( fieldname = 'VFDAT' style = y ) ( fieldname = 'LFABR' style = y ) )
        ) TO outtab_children_lotes.
    ENDLOOP.

    me->set_screen_buttons( ).
    RAISE EVENT evt_refresh_screen.
  ENDMETHOD.

  METHOD get_outtab.
    MOVE outtab_children_lotes TO outtab.
  ENDMETHOD.

  METHOD refresh_screen.
    CHECK NOT ( sender->alv_grid IS INITIAL ).
*    DISPLAY_REFERENCE->ALV_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = VALUE #( COL = ABAP_TRUE ROW = ABAP_TRUE ) ).
    sender->alv_grid->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
  ENDMETHOD.

  METHOD is_ready_to_save.
    IF line_exists( outtab_children_lotes[ vfdat = '' ] )
    OR line_exists( outtab_children_lotes[ lfabr = '' ] ).

      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          text = 'Preencher todos os dados obrigatórios'.
    ENDIF.

  ENDMETHOD.

  METHOD save.
    DATA children_lotes  TYPE TABLE OF ty_children_lote.
    DATA main_lotes      TYPE TABLE OF ty_main_lote.
    DATA message_errors  TYPE TABLE OF bapiret2.
    DATA movement_header TYPE bapi2017_gm_head_01.
    DATA movement_items  TYPE bapi2017_gm_item_create_t.
    DATA children_charg  TYPE ty_children_lote-children_charg.

    FIELD-SYMBOLS <fs_children_lote> TYPE ty_children_lote.

    CALL METHOD me->get_outtab
      IMPORTING
        outtab = children_lotes.

    DATA(_main_lote) = me->get_current_main_lote( ).
    movement_header  = VALUE #( pstng_date = sy-datum doc_date = sy-datum )."HEADER_TXT = DESCRICAO ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Salvando/gerando lotes...'.

    LOOP AT children_lotes ASSIGNING <fs_children_lote>.
      IF ( <fs_children_lote>-mblnr IS INITIAL ).

        TRY.
            IF <fs_children_lote>-children_charg IS INITIAL.

              IF ( children_charg IS INITIAL ).

                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                  EXPORTING
                    input        = <fs_children_lote>-matnr
                  IMPORTING
                    output       = <fs_children_lote>-matnr
                  EXCEPTIONS
                    length_error = 1
                    OTHERS       = 2.

                CALL METHOD me->get_next_children_charg
                  EXPORTING
                    pedido         = <fs_children_lote>-main_charg
                    material       = <fs_children_lote>-matnr
                    centro         = <fs_children_lote>-werks
                    deposito       = <fs_children_lote>-lgort
                  CHANGING
                    children_charg = children_charg.
              ELSE.
                CALL METHOD me->get_next_children_charg
                  CHANGING
                    children_charg = children_charg.
              ENDIF.

              CALL METHOD me->create_batch
                EXPORTING
                  material        = <fs_children_lote>-matnr
                  centro          = <fs_children_lote>-werks
                  deposito        = <fs_children_lote>-lgort
                  lote            = children_charg
                  dt_vencimento   = <fs_children_lote>-vfdat
                  lote_fabricante = <fs_children_lote>-lfabr.
            ELSE.
              children_charg = <fs_children_lote>-children_charg.
            ENDIF.

*---> 16/06/2023 - Migração S4 - DG
            DATA(v_len) = strlen( <fs_children_lote>-matnr ).

            IF v_len > 18.
              DATA(lv_material_long) = <fs_children_lote>-matnr .
            ELSE.
              DATA(lv_material)      = <fs_children_lote>-matnr .
            ENDIF.
*<--- 16/06/2023 - Migração S4 - DG


            APPEND VALUE #(
*---> 16/06/2023 - Migração S4 - DG
                "MATERIAL   = <FS_CHILDREN_LOTE>-MATNR
                material        = lv_material
                material_long   = lv_material_long
*---> 16/06/2023 - Migração S4 - DG
                plant      = <fs_children_lote>-werks
                stge_loc   = <fs_children_lote>-lgort
                batch      = <fs_children_lote>-main_charg
                "GR_RCPT   = ORDEM
                "ITEM_TEXT = ORDEM
                move_type  = '309'
                entry_qnt  = <fs_children_lote>-clabs
                move_batch = children_charg
                "VENDOR    = FORNECEDOR
            ) TO movement_items.

            <fs_children_lote>-children_charg = children_charg.
            <fs_children_lote>-style          = VALUE #( LET y = cl_gui_alv_grid=>mc_style_disabled IN ( fieldname = 'VFDAT' style = y ) ( fieldname = 'LFABR' style = y ) ).

          CATCH cx_local_exception INTO DATA(_ex_batch_creation).
            APPEND LINES OF _ex_batch_creation->get_texts( ) TO message_errors.
            EXIT.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    IF ( message_errors IS INITIAL ).

      TRY.
          CALL METHOD me->transfer_from_batch_to_batch
            EXPORTING
              header   = movement_header
              items    = movement_items
            IMPORTING
              document = DATA(_document).

          DATA(index) = 1.
          DO lines( children_lotes ) TIMES.

            ASSIGN children_lotes[ index ] TO <fs_children_lote>.

            IF <fs_children_lote>-mblnr IS INITIAL.
              TRY.
                  <fs_children_lote>-mblnr = _document-mat_doc.
                  <fs_children_lote>-mjahr = _document-doc_year.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.
            ENDIF.

            ADD 1 TO index.
          ENDDO.

        CATCH cx_local_exception INTO DATA(_ex_batch_transfer).
          APPEND LINES OF _ex_batch_transfer->get_texts( ) TO message_errors.
      ENDTRY.

      me->modify_children_lotes( main_lote = _main_lote items = children_lotes ).
      cl_main_app=>commit( ).

    ENDIF.

    "//Update the selected main lote;
    outtab_main_lotes[ ebeln = _main_lote-ebeln ebelp = _main_lote-ebelp ]-children_lotes  = children_lotes.
    outtab_main_lotes[ ebeln = _main_lote-ebeln ebelp = _main_lote-ebelp ]-count_childrens = |{ icon_batch } ({ lines( children_lotes ) })|.

    outtab_children_lotes = children_lotes.
    RAISE EVENT evt_refresh_screen.

    IF message_errors IS NOT INITIAL.
      MESSAGE 'Registros não foram salvos completamente.' TYPE 'S'.
      RAISE EXCEPTION TYPE cx_local_exception EXPORTING texts = message_errors.
    ELSE.
      MESSAGE 'Registros salvos/gerados com sucesso.' TYPE 'S'.
    ENDIF.

    CLEAR message_errors.
  ENDMETHOD.

  METHOD storno.
    DATA children_lotes TYPE TABLE OF ty_children_lote.
    DATA answer         TYPE char1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação de estorno'
        text_question         = 'Tem certeza que deseja estornar os lotes gerados?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY '
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK answer = '1'. "//Yes

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Estornando documentos/lotes...'.

    CALL METHOD me->get_outtab
      IMPORTING
        outtab = children_lotes.

    LOOP AT children_lotes
      INTO DATA(_children_lote)
        WHERE rsnum IS NOT INITIAL.

      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          text = |Não é possível realizar o estorno pois o lote { _children_lote-children_charg } já foi entregue.|.
    ENDLOOP.

    "//Get current selected main lote;
    DATA(_main_lote) = me->get_current_main_lote( ).
    DATA(_documents) = children_lotes.

    SORT _documents BY mblnr.
    DELETE ADJACENT DUPLICATES FROM _documents COMPARING mblnr.

    "//Cancel batch movement
    LOOP AT _documents INTO DATA(_document) WHERE mblnr IS NOT INITIAL.
      me->cancel_batch_movement( VALUE #( mat_doc  = _document-mblnr doc_year = _document-mjahr ) ).
    ENDLOOP.

    LOOP AT children_lotes ASSIGNING FIELD-SYMBOL(<fs_children_lote>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_children_lote>-matnr
        IMPORTING
          output       = <fs_children_lote>-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      "//Set deletion flag
      CALL METHOD me->set_batch_deletion_flag
        EXPORTING
          material = <fs_children_lote>-matnr
          centro   = <fs_children_lote>-werks
          deposito = <fs_children_lote>-lgort
          lote     = <fs_children_lote>-children_charg.

      <fs_children_lote>-lvorm = abap_true.
    ENDLOOP.

    MESSAGE 'Documentos/lotes estornados com sucesso!' TYPE 'S'.

    "//Commit all
    me->modify_children_lotes( main_lote = _main_lote items = children_lotes ).
    cl_main_app=>commit( ).

    CLEAR outtab_children_lotes.

    "//Update the selected main lote;
    CALL METHOD main_reference->select_main_lotes
      EXPORTING
        pedido = _main_lote-ebeln
      CHANGING
        items  = outtab_main_lotes.

    RAISE EVENT evt_refresh_screen.
  ENDMETHOD.

  METHOD is_saved.
    IF NOT line_exists( outtab_children_lotes[ mblnr = '' ] ).
      MOVE abap_true TO value.
    ENDIF.
  ENDMETHOD.

  METHOD print_tags.
    DATA items           TYPE TABLE OF zppt0011.
    DATA tag_form_name   TYPE rs38l_fnam.
    DATA return          TYPE c.

    LOOP AT selected_rows INTO DATA(_row).
      TRY.
          DATA(_item) = outtab_children_lotes[ _row-index ].

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = _item-matnr
            IMPORTING
              output       = _item-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.

          APPEND VALUE #(
            ebeln = _item-main_charg
            matnr = _item-matnr
            werks = _item-werks
            lgort = _item-lgort
            charg = _item-children_charg
            mblnr = _item-mblnr
            mjahr = _item-mjahr
            clabs = _item-clabs
            vfdat = _item-vfdat
            lfabr = _item-lfabr
            lvorm = _item-lvorm ) TO items.

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.

    DATA(_options_etiqueta) = VALUE ssfcompop( tddest = 'LOCP'    tdnoprint = abap_false tdimmed   = abap_true tdnewid = abap_true tdnoarch = abap_true ).
    DATA(_control_etiqueta) = VALUE ssfctrlop( device = 'PRINTER' preview   = abap_false no_dialog = abap_true ).

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'ZPPSF_ETIQUETA_DEFENSIVO'
      IMPORTING
        fm_name  = tag_form_name.

    CALL FUNCTION tag_form_name
      EXPORTING
        control_parameters = _control_etiqueta
        output_options     = _options_etiqueta
        user_settings      = abap_false
      TABLES
        t_lotes            = items
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
  ENDMETHOD.

  METHOD insert_children_lote.

    DATA:lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = row-matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    DATA(_children_lote) =
        VALUE zppt0011( ebeln = row-main_charg
                       matnr = lv_matnr
                       werks = row-werks
                       lgort = row-lgort
                       charg = row-children_charg
                       clabs = row-clabs
                       vfdat = row-vfdat
                       lfabr = row-lfabr
                       mblnr = row-mblnr
                       mjahr = row-mjahr
                     ).

    INSERT INTO zppt0011 VALUES _children_lote.
    COMMIT WORK.
  ENDMETHOD.

  METHOD modify_children_lotes.
    DATA children_lotes TYPE TABLE OF zppt0011.

    DATA:lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = main_lote-matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF main_lote IS NOT INITIAL.
      DELETE FROM zppt0011 WHERE ebeln = main_lote-ebeln AND matnr = lv_matnr .
    ENDIF.

    LOOP AT items INTO DATA(_item).
      _item-children_charg = SWITCH #( _item-children_charg WHEN '' THEN |$TEMP{ sy-tabix }| ELSE _item-children_charg ).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _item-matnr
        IMPORTING
          output       = _item-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      APPEND
          VALUE #( ebeln = _item-main_charg
                   matnr = _item-matnr
                   werks = _item-werks
                   lgort = _item-lgort
                   charg = _item-children_charg
                   clabs = _item-clabs
                   vfdat = _item-vfdat
                   lfabr = _item-lfabr
                   mblnr = _item-mblnr
                   mjahr = _item-mjahr
                   lvorm = _item-lvorm
                 ) TO children_lotes.
    ENDLOOP.

    MODIFY zppt0011 FROM TABLE children_lotes.
  ENDMETHOD.

  METHOD get_next_children_charg.
    "//Select the next charg sequence

    DATA: lv_material TYPE mara-matnr.

    IF children_charg IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = material
        IMPORTING
          output       = lv_material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      SELECT MAX( charg )
        FROM zppt0011
        INTO children_charg
       WHERE ebeln EQ pedido
         AND matnr EQ lv_material
         AND werks EQ centro
         AND lgort EQ deposito.

      IF children_charg IS INITIAL.
        children_charg = 'D000000001'.
      ELSE.
        children_charg = children_charg+1. ADD 1 TO children_charg.
        CONDENSE children_charg.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = children_charg
          IMPORTING
            output = children_charg.

        children_charg = |D{ children_charg+1 }|.
      ENDIF.

    ELSE.
      children_charg = children_charg+1. ADD 1 TO children_charg.
      CONDENSE children_charg.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = children_charg
        IMPORTING
          output = children_charg.

      children_charg = |D{ children_charg+1 }|.
    ENDIF.

  ENDMETHOD.

  METHOD create_batch.
    DATA return    TYPE TABLE OF bapiret2.
    DATA new_batch TYPE TABLE OF mcha.
    DATA lv_material TYPE mara-matnr.


    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    DATA(_header) = VALUE mcha( matnr = lv_material werks = centro charg = lote vfdat = dt_vencimento ).

    DATA(_characteristics) = VALUE clbatch_t(
      ( atnam = 'ZDEFENSIVO_DT_VALIDADE'     atwtb = dt_vencimento   )
      ( atnam = 'ZDEFENSIVO_LOTE_FABRICANTE' atwtb = lote_fabricante )
    ).


    CALL FUNCTION 'VB_CREATE_BATCH'
      EXPORTING
        ymcha                        = _header
        new_lgort                    = deposito
        kzcla                        = '2'
        class                        = 'ZDEFENSIVO'
        no_cfc_calls                 = 'X'
      IMPORTING
        ymcha                        = _header
      TABLES
        char_of_batch                = _characteristics
        new_batch                    = new_batch
        return                       = return
      EXCEPTIONS
        no_material                  = 1
        no_batch                     = 2
        no_plant                     = 3
        material_not_found           = 4
        plant_not_found              = 5
        stoloc_not_found             = 6
        lock_on_material             = 7
        lock_on_plant                = 8
        lock_on_batch                = 9
        lock_system_error            = 10
        no_authority                 = 11
        batch_exist                  = 12
        stoloc_exist                 = 13
        illegal_batch_number         = 14
        no_batch_handling            = 15
        no_valuation_area            = 16
        valuation_type_not_found     = 17
        no_valuation_found           = 18
        error_automatic_batch_number = 19
        cancelled                    = 20
        wrong_status                 = 21
        interval_not_found           = 22
        number_range_not_extern      = 23
        object_not_found             = 24
        error_check_batch_number     = 25
        no_external_number           = 26
        no_customer_number           = 27
        no_class                     = 28
        error_in_classification      = 29
        inconsistency_in_key         = 30
        region_of_origin_not_found   = 31
        country_of_origin_not_found  = 32
        OTHERS                       = 33.

    IF ( sy-subrc IS INITIAL ).
      DELETE return WHERE type <> 'E'.
    ELSE.
      return = VALUE #( ( id         = sy-msgid
                          type       = sy-msgty
                          number     = sy-msgno
                          message_v1 = sy-msgv1
                          message_v2 = sy-msgv2
                          message_v3 = sy-msgv3
                          message_v4 = sy-msgv4 )
                        ).
    ENDIF.

    IF NOT return IS INITIAL.
      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          texts = return.
    ENDIF.

  ENDMETHOD.

  METHOD set_batch_deletion_flag.

    DATA: lv_material TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'VB_BATCH_SET_DELETION_FLAG'
      EXPORTING
        matnr       = lv_material
        charg       = lote
        werks       = centro
        lgort       = deposito
      EXCEPTIONS
        no_material = 1
        no_batch    = 2
        no_plant    = 3
        no_stloc    = 4
        OTHERS      = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          texts = VALUE #( ( id         = sy-msgid
                             type       = sy-msgty
                             number     = sy-msgno
                             message_v1 = sy-msgv1
                             message_v2 = sy-msgv2
                             message_v3 = sy-msgv3
                             message_v4 = sy-msgv4 )
                           ).

    ENDIF.
  ENDMETHOD.

  METHOD transfer_from_batch_to_batch.
    DATA movement_header TYPE bapi2017_gm_head_01.
    DATA movement_items  TYPE bapi2017_gm_item_create_t.
    LOOP AT items INTO DATA(_item).
      APPEND _item TO movement_items.
    ENDLOOP.

*    MOVEMENT_HEADER = VALUE #(
*        PSTNG_DATE = DATA_DOCUMENTO
*        DOC_DATE   = DATA_DOCUMENTO
*        "HEADER_TXT = DESCRICAO
*     ).
*
*    MOVEMENT_ITEMS = VALUE #( (
*        MATERIAL   = MATERIAL
*        PLANT      = CENTRO
*        STGE_LOC   = DEPOSITO
*        BATCH      = BATCH_FROM
*        "GR_RCPT   = ORDEM
*        "ITEM_TEXT = ORDEM
*        MOVE_TYPE  = TIPO_MOVIMENTO
*        ENTRY_QNT  = QUANTIDADE
*        MOVE_BATCH = BATCH_TO
*        "VENDOR    = FORNECEDOR
*    ) ).

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = header
        goodsmvt_code    = '04'
      IMPORTING
        materialdocument = document-mat_doc
        matdocumentyear  = document-doc_year
      TABLES
        goodsmvt_item    = movement_items "items
        return           = return.

    DELETE return WHERE type <> 'E'.

    IF ( return IS INITIAL ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          texts = return.
    ENDIF.
  ENDMETHOD.

  METHOD cancel_batch_movement.
    DATA return TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument = document-mat_doc
        matdocumentyear  = document-doc_year
      TABLES
        return           = return.

    DELETE return WHERE type <> 'E'.

    IF return IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_local_exception EXPORTING texts = return.
    ENDIF.
  ENDMETHOD.

  METHOD set_current_main_lote.
    MOVE main_lote TO me->current_main_lote.
  ENDMETHOD.

  METHOD get_current_main_lote.
    MOVE me->current_main_lote TO main_lote.
  ENDMETHOD.

  METHOD handle_set_toolbar.
    "//Set Buttons with some exceptions
    DATA(_standard_toolbars) = e_object->mt_toolbar.
    CLEAR e_object->mt_toolbar.

    DATA(_functions) =
      VALUE rsis_t_range( ( sign = 'I' option = 'EQ' low = '&FIND'      )
                          ( sign = 'I' option = 'EQ' low = '&MB_FILTER' )
                          ( sign = 'I' option = 'EQ' low = '&SORT_ASC'  )
                          ( sign = 'I' option = 'EQ' low = '&SORT_DSC'  ) ).

    LOOP AT _standard_toolbars INTO DATA(_toolbar).
      CHECK ( _toolbar-function IN _functions ).
      APPEND _toolbar TO e_object->mt_toolbar.
    ENDLOOP.

    APPEND VALUE #( butn_type = cntb_btype_sep ) TO e_object->mt_toolbar.
    APPEND VALUE #( butn_type = cntb_btype_button function = 'PRINT_TAG' icon = icon_print text = 'Imprimir Etiqueta' ) TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CALL METHOD me->alv_grid->get_selected_rows
      IMPORTING
        et_index_rows = DATA(_selected_rows).

    CASE e_ucomm.
      WHEN 'PRINT_TAG'.
        me->print_tags( _selected_rows ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
