*&---------------------------------------------------------------------*
*& Report  ZPMR0029
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr005.

TYPES: BEGIN OF ty_dismemberment.
         INCLUDE TYPE zppt0011.
TYPES:
         check TYPE char1,
         color TYPE lvc_t_scol,
         style TYPE lvc_t_styl,

       END OF ty_dismemberment,

       ty_t_dismemberment TYPE TABLE OF ty_dismemberment WITH DEFAULT KEY,
       ty_t_zppt0011      TYPE TABLE OF zppt0011         WITH DEFAULT KEY,
       ty_t_zppt0013      TYPE TABLE OF zppt0013         WITH DEFAULT KEY,

       BEGIN OF ty_reserva,
         rsnum          TYPE resb-rsnum,
         rspos          TYPE resb-rspos,
*    SOLDF          TYPE ZPPT0012-SOLDF,
         matnr          TYPE zppt0012-matnr,
         werks          TYPE resb-werks,
         maktx          TYPE char58,
         bdmng          TYPE resb-bdmng,
         umlgo          TYPE umlgo,
         umcha          TYPE umcha,
         lgort          TYPE zppt0012-lgort,
         dismemberments TYPE ty_t_dismemberment,
         count_items    TYPE string,
       END OF ty_reserva,

       ty_t_main_lote TYPE TABLE OF ty_reserva,

       BEGIN OF ty_screen_main,
         rsnum     TYPE resb-rsnum,
         qtd_lotes TYPE char5,
       END OF ty_screen_main,

       BEGIN OF ty_fields,
         field TYPE screen-name,
         group TYPE screen-group1,
         value TYPE screen-active,
       END OF ty_fields,

       BEGIN OF ty_risk,
         title    TYPE string,
         critical TYPE string,
         warning  TYPE string,
         positive TYPE string,
       END OF ty_risk.

*  BEGIN OF TY_SCREEN_TEXT,
*    VOLUME_TOTAL TYPE STRING,
*  END OF TY_SCREEN_TEXT.

DATA:
  BEGIN OF tabstrip,
    tab1 TYPE char40,
    tab2 TYPE char40,
  END OF tabstrip,

  BEGIN OF text_footer,
    volume_total  TYPE string,
    lote_deposito TYPE string,
  END OF text_footer.

*  BEGIN OF TABSTRIP_UCOMM,
*    SUBSCREEN   LIKE SY-DYNNR,
*    PROGRAM     LIKE SY-REPID VALUE sy-repid,
*    PRESSED_TAB LIKE SY-UCOMM VALUE 'TAB_DISPONIVEIS',
*  END OF G_TS_0100.

CONTROLS itabstrip TYPE TABSTRIP.

CONSTANTS:
  BEGIN OF screen,
    empty              TYPE sy-dynnr VALUE '0005',
    main               TYPE sy-dynnr VALUE '0001',
    display            TYPE sy-dynnr VALUE '0002',
    display_explosions TYPE sy-dynnr VALUE '0003',
  END OF screen.


"//Screen dynpro fields
DATA main_screen_data     TYPE ty_screen_main.
DATA selected_reservation TYPE ty_reserva.
DATA btn_salvar           TYPE string.
DATA quantidade_lotes     TYPE char5.
*DATA EXPLOSION_SCREEN_DATA TYPE TY_SCREEN_EXPLOSION.

"//Subscreen
DATA display_subscreen     TYPE sy-dynnr VALUE screen-empty.
DATA icon_recipe           TYPE string VALUE icon_general_recipe.

DATA volume_total          TYPE resb-bdmng.
*DATA TEXT_VOLUME_TOTAL     TYPE STRING.
*DATA TEXT_LOTE_DEPOSITO    TYPE STRING.

"//Outtab
DATA outtab_reservations   TYPE TABLE OF ty_reserva.
DATA outtab_dismemberments TYPE TABLE OF ty_dismemberment.
DATA risk                  TYPE zcl_risk_of_pesticide=>ty_risk.

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
        type    TYPE c DEFAULT 'S'
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

CLASS cl_dismemberment DEFINITION INHERITING FROM cl_display.
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

    METHODS set_volume
      IMPORTING
        clabs TYPE zppt0011-clabs.

    METHODS get_volume
      RETURNING VALUE(clabs) TYPE zppt0011-clabs.

    METHODS check_priority_of_validity
      IMPORTING
        validity_date TYPE sy-datum.

    METHODS check_risk_of_validity_date
      IMPORTING
        validity_date TYPE sy-datum
      RETURNING
        VALUE(risk)   TYPE string.

    METHODS get_charg_available_balance
      IMPORTING
        material                 TYPE zppt0011-matnr
        centro                   TYPE zppt0011-werks
        deposito                 TYPE zppt0011-lgort
        lote                     TYPE zppt0011-charg
      RETURNING
        VALUE(available_balance) TYPE mchb-clabs.

    METHODS set_items
      IMPORTING
        tab LIKE itabstrip-activetab
      RAISING
        cx_local_exception.

    METHODS is_ready_to_save
      RAISING cx_local_exception.

    METHODS transfer_movement
      RAISING
        cx_local_exception.

    METHODS set_log_movements
      IMPORTING
        items TYPE ty_t_zppt0013.

    METHODS is_saved
      RETURNING
        VALUE(value) TYPE abap_bool.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        es_row_no
        e_column_id
        e_row_id.

    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        e_onf4
        e_onf4_after
        e_onf4_before
        e_ucomm
        er_data_changed.

    METHODS transfer_from_batch_to_batch
      IMPORTING
        header   TYPE bapi2017_gm_head_01
        items    TYPE bapi2017_gm_item_create_t
*        DATA_DOCUMENTO TYPE SY-DATUM
*        "DESCRICAO       TYPE BAPI2017_GM_HEAD_01-HEADER_TXT
*        MATERIAL       TYPE BAPI2017_GM_ITEM_CREATE-MATERIAL
*        CENTRO         TYPE BAPI2017_GM_ITEM_CREATE-PLANT
*        DEPOSITO       TYPE BAPI2017_GM_ITEM_CREATE-STGE_LOC
*        BATCH_FROM     TYPE BAPI2017_GM_ITEM_CREATE-BATCH
*        BATCH_TO       TYPE BAPI2017_GM_ITEM_CREATE-MOVE_BATCH
*        TIPO_MOVIMENTO TYPE BAPI2017_GM_ITEM_CREATE-MOVE_TYPE
*        QUANTIDADE     TYPE BAPI2017_GM_ITEM_CREATE-ENTRY_QNT
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

    METHODS set_selected_reservation
      IMPORTING
        reservation TYPE ty_reserva.

    METHODS get_selected_reservation
      RETURNING VALUE(reservation) TYPE ty_reserva.

    "DATA DISPLAY_CONTROLLER TYPE REF TO CL_DISPLAY.
    DATA custom_explosions  TYPE REF TO cl_gui_custom_container.
    "DATA ALV_GRID           TYPE REF TO CL_GUI_ALV_GRID.

  PRIVATE SECTION.
    DATA volume           TYPE zppt0011-clabs.
    DATA data_is_saved          TYPE abap_bool.
    DATA risks_of_validity_date TYPE TABLE OF rgsb4.
*    DATA CHILDREN_CHARG       TYPE TY_RESERVA-CHARG.
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

    METHODS get_destination_batch
      IMPORTING
        deposito     TYPE zppt0011-lgort
      RETURNING
        VALUE(batch) TYPE zppt0011-charg.

    METHODS select_reservation
      IMPORTING
        reserva TYPE resb-rsnum
      CHANGING
        items   TYPE ty_t_main_lote
      RAISING
        cx_local_exception.

    METHODS get_material_text
      IMPORTING
        material    TYPE zppt0012-matnr
      RETURNING
        VALUE(text) TYPE maktx.

    METHODS get_children_lotes
      IMPORTING
        pedido       TYPE zppt0011-ebeln
        material     TYPE zppt0011-matnr
        centro       TYPE zppt0011-werks
        deposito     TYPE zppt0011-lgort
      RETURNING
        VALUE(items) TYPE ty_t_dismemberment.

    METHODS is_purchasing_number_valid
      IMPORTING
        purchasing_number TYPE ekko-ebeln.

    "//Data declarations
    "DATA DISPLAY_CONTROLLER    TYPE REF TO CL_DISPLAY.
    "DATA EXPLOSIONS_CONTROLLER TYPE REF TO CL_DISPLAY_EXPLOSIONS.
ENDCLASS.

"//Classes implementations;

DATA r_main          TYPE REF TO cl_main.
DATA r_dismemberment TYPE REF TO cl_dismemberment.
DATA r_display       TYPE REF TO cl_display.

CLASS cl_main_app IMPLEMENTATION.
  METHOD run.

    "MAIN_REFERENCE = COND #( WHEN MAIN_REFERENCE IS INITIAL THEN NEW CL_MAIN( ) ELSE MAIN_REFERENCE ).
    "DISPLAY_REFERENCE = COND #( WHEN DISPLAY_REFERENCE IS INITIAL THEN NEW CL_DISPLAY( ) ELSE DISPLAY_REFERENCE ).
    "DISPLAY_EXPLOSION_REFERENCE = COND #( WHEN DISPLAY_EXPLOSION_REFERENCE IS INITIAL THEN NEW CL_DISPLAY_EXPLOSIONS( ) ELSE DISPLAY_EXPLOSION_REFERENCE ).

    r_dismemberment = NEW cl_dismemberment( ).
    r_display       = NEW cl_display( ).
    r_main          = NEW cl_main( ) .

    CALL SCREEN 0001.
  ENDMETHOD.

  METHOD commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.

  METHOD factory_process_output.
    cl_main_app=>set_current_screen( dynnr ).

    r_dismemberment = COND #( WHEN r_dismemberment IS INITIAL THEN NEW cl_dismemberment( ) ELSE r_dismemberment ).
    r_display       = COND #( WHEN r_display       IS INITIAL THEN NEW cl_display( )       ELSE r_display       ).
    r_main          = COND #( WHEN r_main          IS INITIAL THEN NEW cl_main( )          ELSE r_main          ).

    CASE dynnr.
      WHEN screen-main.
        r_main->process_before_output( ).

      WHEN screen-display.
        r_display->process_before_output( ).

      WHEN screen-display_explosions.
        r_dismemberment->process_before_output( ).

      WHEN screen-empty.
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

MODULE help_reservation INPUT.

*  SELECT *
*    FROM ZPPT0012
*    INTO TABLE @DATA(_ITEMS).
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      RETFIELD    = 'SOLDF'
*      DYNPPROG    = SY-REPID
*      DYNPNR      = SY-DYNNR
*      DYNPROFIELD = 'MAIN_SCREEN_DATA-SOLICITACAO'
*      VALUE_ORG   = 'S'
*    TABLES
*      VALUE_TAB   = _ITEMS.

  CALL FUNCTION 'MB_SELECT_RESERVATION'
    EXPORTING
      hilfe = 'HLPR'
    IMPORTING
      rsnum = main_screen_data-rsnum.
*      RSPOS = RM07M-RSPOS.

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

    IF r_display->get_main_subscreen( ) = screen-display_explosions.
      CASE itabstrip-activetab.
        WHEN 'TAB_DEVOLVER'.
          SET TITLEBAR '0001' WITH ': Devolução'.

        WHEN 'TAB_ENTREGAR'.
          SET TITLEBAR '0001' WITH ': Entrega'.
      ENDCASE.
    ELSE.
      SET TITLEBAR '0001' WITH ': Síntese'.
    ENDIF.
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

  METHOD get_destination_batch.
    DATA _deposito TYPE num8.

    _deposito = deposito+2.
    batch = 'EQ' && _deposito. "//EQ && 00000001 -> EQ00000001
  ENDMETHOD.

  METHOD select_reservation.
    DATA item TYPE ty_reserva.

    SELECT a~umlgo, b~rsnum, b~rspos, b~matnr, b~werks, b~lgort, b~gsber, b~bdmng, c~maktx
      FROM rkpf AS a
     INNER JOIN resb AS b ON b~rsnum = a~rsnum
     INNER JOIN makt AS c ON c~matnr = b~matnr
      INTO TABLE @DATA(_reservations)
     WHERE a~rsnum EQ @reserva
       AND b~xwaok EQ @abap_true.

*    SELECT *
*      FROM ZPPT0012
*      INTO TABLE @DATA(_SOLICITATIONS)
*     WHERE SOLDF = @RESERVA.

    LOOP AT _reservations ASSIGNING FIELD-SYMBOL(<fs_reservations>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_reservations>-matnr
        IMPORTING
          output       = <fs_reservations>-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

    ENDLOOP.

    SELECT *
      FROM zppt0011
      INTO TABLE @DATA(dismemberments)
   FOR ALL ENTRIES IN @_reservations
     WHERE matnr = @_reservations-matnr
       AND werks = @_reservations-werks
       AND lgort = @_reservations-lgort.

    LOOP AT _reservations INTO DATA(_reservation).


      item-dismemberments =
          VALUE #( FOR _dismemberment IN dismemberments
              ( ebeln = _dismemberment-ebeln
                clabs = _dismemberment-clabs
                matnr = _dismemberment-matnr
                werks = _dismemberment-werks
                vfdat = _dismemberment-vfdat
                lgort = _dismemberment-lgort
                lfabr = _dismemberment-lfabr
                mblnr = _dismemberment-mblnr
                mjahr = _dismemberment-mjahr
                lvorm = _dismemberment-lvorm
                charg = _dismemberment-charg
                umcha = _dismemberment-umcha
                umlgo = _dismemberment-umlgo
                rsnum = _dismemberment-rsnum
              ) ).

      item-count_items = |{ icon_batch } ({ lines( item-dismemberments ) })|.
      item-rsnum       = _reservation-rsnum.
      item-matnr       = _reservation-matnr.
      item-maktx       = |{ CONV i( _reservation-matnr ) } - { _reservation-maktx }|. "|{ CONV I( _RESERVATION-MATNR ) } - { ME->GET_MATERIAL_TEXT( _RESERVATION-MATNR ) }|.
      item-bdmng       = _reservation-bdmng.
      item-werks       = _reservation-werks.
      item-umlgo       = _reservation-umlgo.
      item-umcha       = me->get_destination_batch( item-umlgo ).
      item-lgort       = _reservation-lgort.
      APPEND item TO items.
    ENDLOOP.

    CHECK items IS INITIAL.
    RAISE EXCEPTION TYPE cx_local_exception
      EXPORTING
        text = |Reserva não existe ou não foi aprovada.|.

  ENDMETHOD.

  METHOD get_material_text.
    DATA(_matnr) = material.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = _matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE maktx
      FROM makt
      INTO text
     WHERE matnr = _matnr.
  ENDMETHOD.

  METHOD get_children_lotes.
*    SELECT *
*      FROM ZPPT0011
*      INTO TABLE @DATA(_CHILDREN_LOTES)
*     WHERE EBELN = @PEDIDO
*       AND MATNR = @MATERIAL
*       AND WERKS = @CENTRO
*       AND LGORT = @DEPOSITO
*       AND LVORM = @ABAP_FALSE.
*
*    LOOP AT _CHILDREN_LOTES INTO DATA(_CHILDREN_LOTE).
*      APPEND VALUE #( MAIN_CHARG     = _CHILDREN_LOTE-EBELN
*                      CHILDREN_CHARG = COND #( WHEN _CHILDREN_LOTE-CHARG(1) = '$' THEN SPACE ELSE _CHILDREN_LOTE-CHARG )
*                      CLABS          = _CHILDREN_LOTE-CLABS
*                      LGORT          = _CHILDREN_LOTE-LGORT
*                      MATNR          = _CHILDREN_LOTE-MATNR
*                      WERKS          = _CHILDREN_LOTE-WERKS
*                      VFDAT          = _CHILDREN_LOTE-VFDAT
*                      LFABR          = _CHILDREN_LOTE-LFABR
*                      MBLNR          = _CHILDREN_LOTE-MBLNR
*                      MJAHR          = _CHILDREN_LOTE-MJAHR
*                    ) TO ITEMS.
*    ENDLOOP.
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
        IF ( display_subscreen = screen-display_explosions ).
          r_display->set_main_subscreen( screen-display ).
        ELSE.
          LEAVE PROGRAM.
        ENDIF.

      WHEN 'ENTER'.
        CHECK ( cl_main_app=>get_current_screen( ) = screen-empty   )
           OR ( cl_main_app=>get_current_screen( ) = screen-display ).

        TRY.
            CLEAR outtab_reservations.

            CALL METHOD me->select_reservation
              EXPORTING
                reserva = main_screen_data-rsnum
              CHANGING
                items   = outtab_reservations.

            "//Create new instances to work;
            "DISPLAY_EXPLOSION_REFERENCE = NEW CL_DISPLAY_EXPLOSIONS( ).
            "DISPLAY_REFERENCE           = NEW CL_DISPLAY( ).

            "//Set screen;
            r_display->set_main_subscreen( screen-display ).
            r_display->set_alv_kind( 'grid' ).

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
*    CASE ITABSTRIP-ACTIVETAB.
*      WHEN 'TAB_ENTREGAR'.

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
              it_outtab                     = outtab_reservations
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
*
*      WHEN 'TAB_DEVOLVER'.

*    ENDCASE.
  ENDMETHOD.

  METHOD set_alv_kind.
    MOVE alv TO me->alv_kind.
  ENDMETHOD.

  METHOD get_alv_kind.
    MOVE me->alv_kind TO alv.
  ENDMETHOD.

  METHOD get_fieldcatalog.

    fieldscatalog =
        VALUE #(
*        ( FIELDNAME = 'RSNUM' COLTEXT = 'Reserva'    OUTPUTLEN = 12 )
*        ( FIELDNAME = 'RSPOS' COLTEXT = 'Item'       OUTPUTLEN = 5 )

*        ( FIELDNAME = 'MATNR'       COLTEXT = 'Material'  OUTPUTLEN = 40 )
        ( fieldname = 'MAKTX'       coltext = 'Material '        outputlen = 40 )
        ( fieldname = 'WERKS'       coltext = 'Centro'           outputlen = 7  )
        ( fieldname = 'LGORT'       coltext = 'Depósito origem'  outputlen = 13 )
        ( fieldname = 'BDMNG'       coltext = 'Volume'           outputlen = 10 )
        "( FIELDNAME = 'COUNT_ITEMS' COLTEXT = ''                 OUTPUTLEN = 7  )
        ).
  ENDMETHOD.

  METHOD get_outtab.
    MOVE outtab_reservations TO outtab.
  ENDMETHOD.

  METHOD set_main_subscreen.
    display_subscreen = screen.
    "MOVE ME TO REF.
  ENDMETHOD.

  METHOD get_main_subscreen.
    screen = display_subscreen.
  ENDMETHOD.

  METHOD handle_double_click.
*    DATA RESERVATIONS TYPE TABLE OF TY_RESERVA.
*
*    CALL METHOD ME->GET_OUTTAB
*      IMPORTING
*        OUTTAB = RESERVATIONS.

    TRY.
        "//Set current selected main lote to be used inside DISPLAY_EXPLOSION_REFERENCE;
        r_dismemberment->set_selected_reservation( outtab_reservations[ e_row-index ] ).
        itabstrip-activetab = 'TAB_ENTREGAR'.

        TRY.
            r_dismemberment->set_items( itabstrip-activetab ).
          CATCH cx_local_exception INTO DATA(_cx).
            _cx->message( type = 'I' display = 'E' ).
        ENDTRY.
        "//Set a new screen to be loaded in DISPLAY_SUBSCREEN;
        CALL METHOD me->set_main_subscreen
          EXPORTING
            screen = screen-display_explosions.

        LEAVE TO SCREEN 0001.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS cl_dismemberment IMPLEMENTATION.
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

      SET HANDLER me->handle_hotspot_click FOR me->alv_grid.
      SET HANDLER me->handle_data_changed  FOR me->alv_grid.

      CALL METHOD me->alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = layout
          i_save          = 'A'
        CHANGING
          it_outtab       = outtab_dismemberments
          it_fieldcatalog = _fieldscatalog.

      me->alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      me->alv_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter    ).
    ELSE.
      me->alv_grid->set_frontend_layout( layout ).
      me->alv_grid->set_frontend_fieldcatalog( _fieldscatalog ).
    ENDIF.
  ENDMETHOD.

  METHOD process_before_output.
    SET HANDLER process_after_output.
    SET HANDLER refresh_screen FOR ALL INSTANCES.

    me->set_screen_buttons( ).
    me->display_alv( ).
  ENDMETHOD.

  METHOD process_after_output.
    SET HANDLER process_after_output
     ACTIVATION abap_off.

    "CHECK DYNNR = CL_MAIN_APP=>GET_CURRENT_SCREEN( ).
    DATA cx TYPE REF TO cx_local_exception.

    CASE ucomm.
      WHEN 'TAB_ENTREGAR' OR 'TAB_DEVOLVER'.
        TRY.
            me->set_items( CONV #( ucomm ) ).
            itabstrip-activetab = ucomm.

          CATCH cx_local_exception INTO cx.
            cx->message( type = 'I' display = 'E' ).
        ENDTRY.

      WHEN 'BACK_TO_MAIN'.

        "//Set a new screen to be loaded in DISPLAY_SUBSCREEN;
        CALL METHOD me->set_main_subscreen
          EXPORTING
            screen = screen-display.

      WHEN 'ENTER'.

        "ME->SET_INITIAL_CHILDREN_LOTES( MAIN_SCREEN_DATA-QTD_LOTES ).

      WHEN 'BTN_FINISH_TRANSFER'.
        TRY.
            me->transfer_movement( ).

          CATCH cx_local_exception INTO DATA(_cx).
            IF _cx->get_text( ) IS INITIAL. _cx->display( ). ELSE. _cx->message( type = 'I' display = 'E' ). ENDIF.
        ENDTRY.

      WHEN 'SAVE'.
*        TRY.
*            IF ( ME->IS_SAVED( ) = ABAP_TRUE ).
*              ME->STORNO( ).
*            ELSE.
*              ME->IS_READY_TO_SAVE( ).
*              "              ME->TRANSFER_MOVEMENT( ).
*            ENDIF.
*
*          CATCH CX_LOCAL_EXCEPTION INTO DATA(_CX).
*            IF _CX->GET_TEXT( ) IS INITIAL.
*              _CX->DISPLAY( ).
*            ELSE.
*              MESSAGE _CX->GET_TEXT( ) TYPE 'S' DISPLAY LIKE 'E'.
*            ENDIF.
*        ENDTRY.

      WHEN 'PICK'.
        DATA field_value TYPE string.
        GET CURSOR FIELD field_value.

        CASE field_value.
          WHEN 'SELECTED_RESERVATION-UMCHA' OR 'SELECTED_RESERVATION-UMLGO'.

            SET PARAMETER ID 'MAT' FIELD selected_reservation-matnr.
            SET PARAMETER ID 'WRK' FIELD selected_reservation-werks.
            SET PARAMETER ID 'LAG' FIELD selected_reservation-umlgo.
            SET PARAMETER ID 'CHA' FIELD selected_reservation-umcha.

            CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.

          WHEN 'SELECTED_RESERVATION-RSNUM'.

            SET PARAMETER ID 'RES' FIELD selected_reservation-rsnum.
            CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.

*            DATA(_PARAMETERS) =
*                VALUE BDCDATA_TAB(  ( PROGRAM = 'ZPPR003'    DYNPRO = '0001' DYNBEGIN = ABAP_TRUE )
*                                    ( FNAM    = 'BDC_OKCODE' FVAL   = '=BTN_SEARCH'               )
*                                    ( FNAM    = 'BDC_CURSOR' FVAL   = 'SOLICITACAO-UMLGO'         )
*                                    ( PROGRAM = 'ZPPR003'    DYNPRO = '0001' DYNBEGIN = ABAP_TRUE )
*                                    ( FNAM    = 'BDC_OKCODE' FVAL   = '=ENTER'                    )
*                                    ( FNAM    = 'BDC_CURSOR' FVAL   = 'SOLICITACAO-ID'            )
*                                    ( FNAM    = 'RESB-RSNUM' FVAL   = SELECTED_RESERVATION-RSNUM  )
*                                 ).
*
*            CALL TRANSACTION 'ZPP0007' USING _PARAMETERS MODE 'E'.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD get_fieldcatalog.

*      RSNUM TYPE RESB-RSNUM,
*    RSPOS TYPE RESB-RSPOS,
*    CLABS TYPE ZPPT0011-CLABS,
*    LGORT TYPE ZPPT0011-LGORT,
*    MATNR TYPE ZPPT0011-MATNR,
*    WERKS TYPE ZPPT0011-WERKS,
*    VFDAT TYPE ZPPT0011-VFDAT,
*    LFABR TYPE ZPPT0011-LFABR,
*    MBLNR TYPE ZPPT0011-MBLNR,
*    MJAHR TYPE ZPPT0011-MJAHR,
*    LVORM TYPE ZPPT0011-LVORM,
*    CHARG TYPE ZPPT0011-CHARG,

    fieldscatalog = VALUE #(
        ( fieldname = 'CHECK' coltext = ''
          outputlen = 4
          checkbox  = abap_true
          edit      = abap_true
          key       = abap_true )
          "NO_OUT    = SWITCH #( ITABSTRIP-ACTIVETAB WHEN 'TAB_DEVOLVER' THEN ABAP_TRUE ELSE ABAP_FALSE ) )

        ( fieldname = 'MATNR' coltext = 'Material'        outputlen = 10 no_zero  = abap_true  )
        ( fieldname = 'WERKS' coltext = 'Centro'          outputlen = 10 )
        ( fieldname = 'CLABS' coltext = 'Volume'          outputlen = 10 edit = abap_on ref_table = 'ZPPT0011' ref_field = 'CLABS' )
        ( fieldname = 'CHARG' coltext = 'Lote'            outputlen = 12 hotspot = abap_true )
        "( FIELDNAME = 'MBLNR' COLTEXT = 'Documento'       OUTPUTLEN = 10 )
        ( fieldname = 'VFDAT' coltext = 'Data Validade'   outputlen = 13 )
        ( fieldname = 'LFABR' coltext = 'Lote Fabricante' outputlen = 15 )
    ).
  ENDMETHOD.

  METHOD set_layout.
    DATA(_selected_reservation) = me->get_selected_reservation( ).

    layout-grid_title = |{ _selected_reservation-maktx } / { _selected_reservation-lgort }|.
    layout-ctab_fname = 'COLOR'.
    layout-stylefname = 'STYLE'.
*    layout-BOX_FNAME  = ABAP_TRUE.
*    LAYOUT-SEL_MODE   = 'D'.
*    LAYOUT-NO_ROWMARK = ABAP_TRUE.
    layout-no_toolbar = abap_true.
*    LAYOUT-NO_VGRIDLN = ABAP_TRUE.
*    layout-NO_HGRIDLN = abap_true.
  ENDMETHOD.

  METHOD set_screen_buttons.
    IF me->is_saved( ) = abap_true.
      btn_salvar = |{ icon_storno } Estornar|.
    ELSE.
      btn_salvar = |{ icon_system_save } Salvar|.
    ENDIF.
  ENDMETHOD.

  METHOD set_volume.
    MOVE clabs TO me->volume.
  ENDMETHOD.

  METHOD get_volume.
    MOVE me->volume TO clabs.
  ENDMETHOD.

  METHOD check_priority_of_validity.

    LOOP AT outtab_dismemberments INTO DATA(_dismemberment).
      IF _dismemberment-vfdat > validity_date AND _dismemberment-check = abap_false.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_risk_of_validity_date.
    "//Short description about this method:
    "//The variable _RISK-TITLE could return tree values according with day's interval, it is:
    "// - C-Critical;
    "// - W-Warning;
    "// - P-Positive;

*    IF RISKS_OF_VALIDITY_DATE IS INITIAL.
*      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*        EXPORTING
*          SETNR           = 'ZPP_VENCIMENTO_DEFENSIVO'
*          NO_DESCRIPTIONS = ABAP_FALSE
*        TABLES
*          SET_VALUES      = RISKS_OF_VALIDITY_DATE
*        EXCEPTIONS
*          SET_NOT_FOUND   = 1
*          OTHERS          = 2.
*    ENDIF.

    LOOP AT risks_of_validity_date INTO DATA(_risk).
*      data(_from) = conv i( _risk-FROM ).
*      data(_to)   = conv i( _risk-to   ).

      DATA(_interval) = VALUE rsis_t_range( ( sign = 'I' option = 'BT' low = CONV datum( sy-datum + _risk-from ) high = CONV datum( sy-datum + _risk-to ) ) ).

      IF validity_date IN _interval.
        risk = _risk-title(1). "//See the short description to understand better;
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_charg_available_balance.

    DATA: lv_material TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE clabs
      FROM mchb
      INTO available_balance
     WHERE matnr = lv_material
       AND werks = centro
       AND lgort = deposito
       AND charg = lote.
  ENDMETHOD.

  METHOD set_items.
    FIELD-SYMBOLS <fs_item> TYPE ty_dismemberment.
    DATA(_item) = me->get_selected_reservation( ).

    CLEAR: volume_total." OUTTAB_DISMEMBERMENTS.

    CASE tab.
      WHEN 'TAB_ENTREGAR'.

        text_footer-volume_total  = |{ icon_general_recipe  } Volume à ser entregue|.
        text_footer-lote_deposito = |{ icon_include_objects } Lote/Depósito destino|.

        risk = zcl_risk_of_pesticide=>get_validity_risk( ).

        LOOP AT _item-dismemberments ASSIGNING <fs_item> WHERE rsnum = _item-rsnum OR rsnum = space.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = <fs_item>-matnr
            IMPORTING
              output       = <fs_item>-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.

          CALL METHOD me->get_charg_available_balance
            EXPORTING
              material          = <fs_item>-matnr
              centro            = <fs_item>-werks
              deposito          = <fs_item>-lgort
              lote              = <fs_item>-charg
            RECEIVING
              available_balance = <fs_item>-clabs.

          ADD <fs_item>-clabs TO volume_total.

          DATA(_validity_risk_color) =
              COND #( LET  x = zcl_risk_of_pesticide=>check_risk_of_validity_date( <fs_item>-vfdat ) IN
                      WHEN x = 'C' THEN VALUE lvc_s_colo( col = col_negative int = 1 ) "//Critical
                      WHEN x = 'W' THEN VALUE lvc_s_colo( col = col_total    int = 1 ) "//Warning
                      WHEN x = 'P' THEN VALUE lvc_s_colo( col = col_positive int = 1 ) "//Positive
                    ).

          <fs_item>-color = VALUE #( ( fname     = 'CHECK' color = VALUE lvc_s_colo( col = col_key int = 1 ) ) ( fname = 'VFDAT' color = _validity_risk_color ) ).
          <fs_item>-style = VALUE #( ( fieldname = 'CLABS' style = cl_gui_alv_grid=>mc_style_disabled ) ).
        ENDLOOP.

        DELETE _item-dismemberments WHERE clabs IS INITIAL.

      WHEN 'TAB_DEVOLVER'.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = _item-matnr
          IMPORTING
            output       = _item-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

        CALL METHOD me->get_charg_available_balance
          EXPORTING
            material          = _item-matnr
            centro            = _item-werks
            deposito          = _item-umlgo
            lote              = _item-umcha
          RECEIVING
            available_balance = volume_total.

        IF ( volume_total IS INITIAL ).
          RAISE EXCEPTION TYPE cx_local_exception EXPORTING text = TEXT-e03.
        ELSE.
          me->set_volume( volume_total ).
        ENDIF.

        DELETE _item-dismemberments WHERE umcha IS INITIAL.
        LOOP AT _item-dismemberments ASSIGNING <fs_item> WHERE rsnum = _item-rsnum.

          <fs_item>-clabs = 0.
          <fs_item>-check = abap_true.

          <fs_item>-color = VALUE #( ( fname = 'CHECK' color = VALUE lvc_s_colo( col = col_key int = 1 ) ) ).
          <fs_item>-style = VALUE #( ( fieldname = 'CLABS' style = cl_gui_alv_grid=>mc_style_enabled ) ).
        ENDLOOP.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_local_exception EXPORTING text = TEXT-e04.
        ELSE.
          CLEAR risk.
          text_footer-volume_total  = |{ icon_general_recipe  } Volume à ser devolvido|.
          text_footer-lote_deposito = |{ icon_include_objects } Lote/Depósito origem|.
        ENDIF.
    ENDCASE.

    outtab_dismemberments = _item-dismemberments.

    me->set_screen_buttons( ).
    RAISE EVENT evt_refresh_screen.
  ENDMETHOD.

  METHOD get_outtab.
    MOVE outtab_dismemberments TO outtab.
  ENDMETHOD.

  METHOD refresh_screen.
    CHECK NOT ( sender->alv_grid IS INITIAL ).
*    DISPLAY_REFERENCE->ALV_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = VALUE #( COL = ABAP_TRUE ROW = ABAP_TRUE ) ).
    sender->alv_grid->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
  ENDMETHOD.

  METHOD is_ready_to_save.
    IF line_exists( outtab_dismemberments[ vfdat = '' ] )
    OR line_exists( outtab_dismemberments[ lfabr = '' ] ).

      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          text = 'Preencher todos os dados obrigatórios'.
    ENDIF.

  ENDMETHOD.

  METHOD transfer_movement.
    DATA dismemberments_aux TYPE TABLE OF zppt0011.
    DATA log_movements      TYPE TABLE OF zppt0013.
    DATA movement_items     TYPE bapi2017_gm_item_create_t.

    FIELD-SYMBOLS <fs_dismemberment> LIKE LINE OF outtab_dismemberments.

    DATA(_selected_reservation) = me->get_selected_reservation( ).
    DATA(_movement_header)      = VALUE bapi2017_gm_head_01( pstng_date = sy-datum
                                                             doc_date   = sy-datum
                                                             ref_doc_no = _selected_reservation-rsnum
                                                             header_txt = SWITCH #( itabstrip-activetab WHEN 'TAB_ENTREGAR'
                                                                                                        THEN 'E-Entrega de defensivos'
                                                                                                        ELSE 'D-Devolução de defensivos' )
                                                           ).
    "//Set indicator in screen
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Realizando transferência de estoque...'.

    CASE itabstrip-activetab.
      WHEN 'TAB_ENTREGAR'.
        DATA outtab_dismemberment LIKE LINE OF outtab_dismemberments.

        LOOP AT outtab_dismemberments ASSIGNING <fs_dismemberment> WHERE check = abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <fs_dismemberment>-matnr
          IMPORTING
            output       = <fs_dismemberment>-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

          <fs_dismemberment>-umcha = _selected_reservation-umcha.
          <fs_dismemberment>-umlgo = _selected_reservation-umlgo.
          <fs_dismemberment>-rsnum = _selected_reservation-rsnum.

*---> 16/06/2023 - Migração S4 - DG
          DATA(v_len) = strlen( <fs_dismemberment>-matnr ).

          IF v_len > 18.
            DATA(lv_material_long) = <fs_dismemberment>-matnr .
          ELSE.
            DATA(lv_material)      = <fs_dismemberment>-matnr .
          ENDIF.
*<--- 16/06/2023 - Migração S4 - DG

          APPEND VALUE #(
              po_number  = <fs_dismemberment>-ebeln

*---> 16/06/2023 - Migração S4 - DG
                "MATERIAL   = <FS_DISMEMBERMENT>-MATNR
              material        = lv_material
              material_long   = lv_material_long
*---> 16/06/2023 - Migração S4 - DG
              plant      = <fs_dismemberment>-werks
              stge_loc   = <fs_dismemberment>-lgort
              move_stloc = <fs_dismemberment>-umlgo
              batch      = <fs_dismemberment>-charg
              move_batch = <fs_dismemberment>-umcha
              entry_qnt  = <fs_dismemberment>-clabs
*              RESERV_NO  = _SELECTED_RESERVATION-RSNUM
*              RES_ITEM   = _SELECTED_RESERVATION-RSPOS
              move_type  = '311'
          ) TO movement_items.

          APPEND VALUE #(
              ebeln     = <fs_dismemberment>-ebeln
              werks     = <fs_dismemberment>-werks
              lgort     = <fs_dismemberment>-lgort
              charg     = <fs_dismemberment>-charg
              operacao  = 'E' "//Entrega
              clabs     = <fs_dismemberment>-clabs
              umlgo     = _selected_reservation-umlgo
              umcha     = _selected_reservation-umcha
          ) TO log_movements.
        ENDLOOP.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_local_exception EXPORTING text = TEXT-e02.
        ENDIF.

      WHEN 'TAB_DEVOLVER'.

        LOOP AT outtab_dismemberments ASSIGNING <fs_dismemberment> WHERE clabs IS NOT INITIAL.
          APPEND VALUE #(
              po_number  = <fs_dismemberment>-ebeln
*   > 16/06/2023 - Migração S4 - DG
                "MATERIAL   = <FS_DISMEMBERMENT>-MATNR
              material        = lv_material
              material_long   = lv_material_long
*   > 16/06/2023 - Migração S4 - DG
              plant      = <fs_dismemberment>-werks
              stge_loc   = <fs_dismemberment>-umlgo
              move_stloc = <fs_dismemberment>-lgort
              batch      = <fs_dismemberment>-umcha
              move_batch = <fs_dismemberment>-charg
              entry_qnt  = <fs_dismemberment>-clabs
              move_type  = '311'
          ) TO movement_items.

          APPEND VALUE #(
              ebeln     = <fs_dismemberment>-ebeln
              matnr     = <fs_dismemberment>-matnr
              werks     = <fs_dismemberment>-werks
              lgort     = <fs_dismemberment>-umlgo
              charg     = <fs_dismemberment>-umcha
              operacao  = 'D' "//Devolução
              clabs     = <fs_dismemberment>-clabs
              umlgo     = <fs_dismemberment>-lgort
              umcha     = <fs_dismemberment>-charg
          ) TO log_movements.
        ENDLOOP.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_local_exception EXPORTING text = TEXT-e01.
        ENDIF.
    ENDCASE.

    CALL METHOD me->transfer_from_batch_to_batch
      EXPORTING
        header   = _movement_header
        items    = movement_items
      IMPORTING
        document = DATA(_document)
        return   = DATA(_result).

    LOOP AT log_movements ASSIGNING FIELD-SYMBOL(<movement>).
      <movement>-mblnr      = _document-mat_doc.
      <movement>-created_by = sy-uname.
      <movement>-created_at = sy-datum .
    ENDLOOP.

    me->set_log_movements( log_movements ).

    IF itabstrip-activetab = 'TAB_ENTREGAR'.
      LOOP AT outtab_dismemberments INTO outtab_dismemberment WHERE check = abap_true.
        APPEND CORRESPONDING #( outtab_dismemberment ) TO dismemberments_aux.
      ENDLOOP.
      DELETE outtab_dismemberments WHERE check = abap_true.

    ELSE.

      LOOP AT outtab_dismemberments INTO outtab_dismemberment WHERE clabs <> 0.
        CLEAR: outtab_dismemberment-umlgo, outtab_dismemberment-umcha, outtab_dismemberment-rsnum.
        APPEND CORRESPONDING #( outtab_dismemberment ) TO dismemberments_aux.
      ENDLOOP.
      DELETE outtab_dismemberments WHERE clabs <> 0.
    ENDIF.

    UPDATE zppt0011 FROM TABLE dismemberments_aux.
    COMMIT WORK.

    CLEAR: dismemberments_aux.

    _selected_reservation-dismemberments = outtab_dismemberments.
    me->set_selected_reservation( _selected_reservation  ).

    MESSAGE TEXT-s01 TYPE 'S'.
    RAISE EVENT evt_refresh_screen.
  ENDMETHOD.

  METHOD set_log_movements.
    INSERT zppt0013 FROM TABLE items.
    COMMIT WORK.
  ENDMETHOD.

  METHOD is_saved.
    IF NOT line_exists( outtab_dismemberments[ mblnr = '' ] ).
      MOVE abap_true TO value.
    ENDIF.
  ENDMETHOD.

*  METHOD INSERT_CHILDREN_LOTE.
*    DATA(_CHILDREN_LOTE) =
*        VALUE ZPPT0011( EBELN = ROW-MAIN_CHARG
*                       MATNR = ROW-MATNR
*                       WERKS = ROW-WERKS
*                       LGORT = ROW-LGORT
*                       CHARG = ROW-CHILDREN_CHARG
*                       CLABS = ROW-CLABS
*                       VFDAT = ROW-VFDAT
*                       LFABR = ROW-LFABR
*                       MBLNR = ROW-MBLNR
*                       MJAHR = ROW-MJAHR
*                     ).
*
*    INSERT INTO ZPPT0011 VALUES _CHILDREN_LOTE.
*    COMMIT WORK.
*  ENDMETHOD.
*
*  METHOD MODIFY_CHILDREN_LOTES.
*    DATA CHILDREN_LOTES TYPE TABLE OF ZPPT0011.
*
*    IF MAIN_LOTE IS NOT INITIAL.
*      DELETE FROM ZPPT0011 WHERE EBELN = MAIN_LOTE-EBELN AND MATNR = MAIN_LOTE-MATNR.
*    ENDIF.
*
*    LOOP AT ITEMS INTO DATA(_ITEM).
*      _ITEM-CHILDREN_CHARG = SWITCH #( _ITEM-CHILDREN_CHARG WHEN '' THEN |$TEMP{ SY-TABIX }| ELSE _ITEM-CHILDREN_CHARG ).
*
*      APPEND
*          VALUE #( EBELN = _ITEM-MAIN_CHARG
*                   MATNR = _ITEM-MATNR
*                   WERKS = _ITEM-WERKS
*                   LGORT = _ITEM-LGORT
*                   CHARG = _ITEM-CHILDREN_CHARG
*                   CLABS = _ITEM-CLABS
*                   VFDAT = _ITEM-VFDAT
*                   LFABR = _ITEM-LFABR
*                   MBLNR = _ITEM-MBLNR
*                   MJAHR = _ITEM-MJAHR
*                   LVORM = _ITEM-LVORM
*                 ) TO CHILDREN_LOTES.
*    ENDLOOP.
*
*    MODIFY ZPPT0011 FROM TABLE CHILDREN_LOTES.
*  ENDMETHOD.


  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN 'CHARG'.
        TRY.
            DATA(_selected_row) = outtab_dismemberments[ e_row_id-index ].

            SET PARAMETER ID 'MAT' FIELD _selected_row-matnr.
            SET PARAMETER ID 'WRK' FIELD _selected_row-werks.
            SET PARAMETER ID 'LAG' FIELD _selected_row-lgort.
            SET PARAMETER ID 'CHA' FIELD _selected_row-charg.

            CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.
    DATA(_selected_reservation) = me->get_selected_reservation( ).

    LOOP AT er_data_changed->mt_good_cells INTO DATA(_cell).
      ASSIGN outtab_dismemberments[ _cell-row_id ] TO FIELD-SYMBOL(<fs_dismemberment>).

      CASE _cell-fieldname.
        WHEN 'CHECK'.
*          <FS_DISMEMBERMENT>-CHECK = _CELL-VALUE.

          CASE _cell-value.
            WHEN abap_true.

*              IF ( VOLUME_A_RETIRAR + <FS_DISMEMBERMENT>-CLABS ) > _SELECTED_RESERVATION-BDMNG.
*                MESSAGE 'O volume à ser retirado não pode ser maior que o da reserva' TYPE 'S' DISPLAY LIKE 'E'.
*                <FS_DISMEMBERMENT>-CHECK = ABAP_FALSE.
*              ELSE.
              volume_total = volume_total + <fs_dismemberment>-clabs.
              <fs_dismemberment>-check = abap_true.
*              ENDIF.

            WHEN abap_false.
              volume_total = volume_total - <fs_dismemberment>-clabs.
              <fs_dismemberment>-check = abap_false.
          ENDCASE.

        WHEN 'CLABS'.
*          DATA VOLUME_TOTAL_OLD LIKE VOLUME_TOTAL.
*
*          IF VOLUME_TOTAL_OLD IS INITIAL.
*            VOLUME_TOTAL_OLD = VOLUME_TOTAL.
*          ENDIF.

          volume_total = me->get_volume( ).

          <fs_dismemberment>-clabs = _cell-value.

          LOOP AT outtab_dismemberments INTO DATA(_dismemberment).
            SUBTRACT _dismemberment-clabs FROM volume_total.
          ENDLOOP.

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    RAISE EVENT evt_refresh_screen.
    LEAVE TO SCREEN 0001.
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
        goodsmvt_item    = movement_items "ITEMS
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

  METHOD set_selected_reservation.
    MOVE reservation TO selected_reservation.
  ENDMETHOD.

  METHOD get_selected_reservation.
    MOVE selected_reservation TO reservation.
  ENDMETHOD.
ENDCLASS.
