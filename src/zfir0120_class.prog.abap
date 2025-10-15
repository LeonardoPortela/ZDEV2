
    DATA: CONTAINER_MAIN TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          PAINEL_CONTROL TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          PAINEL1        TYPE REF TO CL_GUI_CONTAINER,
          PAINEL2        TYPE REF TO CL_GUI_CONTAINER.
    CLASS LCL_REPORT DEFINITION DEFERRED.
    DATA: LO_REPORT TYPE REF TO LCL_REPORT.

    CLASS LCL_LISTENER DEFINITION.
      PUBLIC SECTION.
        INTERFACES :
          IF_SALV_GUI_OM_EDIT_STRCT_LSTR.
    ENDCLASS.

    CLASS LCL_LISTENER IMPLEMENTATION.
      METHOD IF_SALV_GUI_OM_EDIT_STRCT_LSTR~ON_CHECK_CHANGED_DATA.
        O_UI_DATA_MODIFY->GET_UI_CHANGES( IMPORTING T_MODIFIED_CELLS = DATA(LT_MODIFIED) ).
      ENDMETHOD.
    ENDCLASS.


    CLASS LCL_REPORT DEFINITION.
      PUBLIC SECTION .
        METHODS:
          GET_DATA,
          GENERATE_OUTPUT,
          SET_HANDLER
            CHANGING CO_ALV    TYPE REF TO CL_SALV_TABLE
                     CO_REPORT TYPE REF TO LCL_REPORT,
          SET_REFRESH
            CHANGING
              CO_ALV TYPE REF TO CL_SALV_TABLE,
          SET_COLUMNS_BUILD
            CHANGING
              CO_ALV TYPE REF TO CL_SALV_TABLE,
          SET_PF_STATUS
            CHANGING
              CO_ALV TYPE REF TO CL_SALV_TABLE,
          SET_LAYOUT
            CHANGING
              CO_ALV TYPE REF TO CL_SALV_TABLE,
          SET_EDIT_ALV
            CHANGING
              CO_ALV TYPE REF TO CL_SALV_TABLE,
          ON_TOOLBAR
            FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING
              E_OBJECT
              E_INTERACTIVE
              SENDER.

        METHODS: ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS IMPORTING E_SALV_FUNCTION SENDER,
          ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE IMPORTING ROW COLUMN,
          ON_CHANGE_DATA FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING E_ONF4 E_ONF4_AFTER E_ONF4_BEFORE E_UCOMM ER_DATA_CHANGED SENDER.

    ENDCLASS.
    CLASS LCL_REPORT IMPLEMENTATION.
      METHOD SET_REFRESH.
        LO_REPORT->GET_DATA( ).
        CO_ALV->REFRESH( ).
      ENDMETHOD.

      METHOD ON_CHANGE_DATA.

        DATA: IT_CHANGED TYPE STANDARD TABLE OF ZFISE46 INITIAL SIZE 0.
        "CLEAR: it_changed,wa_saida.

        DATA(INSERTED_TAB) = ER_DATA_CHANGED->MT_INSERTED_ROWS.
        DATA(DELETED_TAB)  = ER_DATA_CHANGED->MT_DELETED_ROWS.

        FIELD-SYMBOLS: <ITAB>        TYPE ANY TABLE,
                       <STRUCT>      TYPE ANY,
                       <IT_MOD_ROWS> TYPE ANY TABLE,
                       <WA_MOD_ROWS> TYPE ANY.


        DATA: LT_GOOD_CELLS TYPE LVC_T_MODI,
              LS_GOOD_CELL  TYPE LVC_S_MODI.


        ASSIGN ER_DATA_CHANGED->MP_MOD_ROWS->* TO <IT_MOD_ROWS>.
        MOVE-CORRESPONDING <IT_MOD_ROWS> TO IT_CHANGED.

        LT_GOOD_CELLS = ER_DATA_CHANGED->MT_GOOD_CELLS.

      ENDMETHOD.

      METHOD SET_COLUMNS_BUILD .
*
*...Get all the Columns
        DATA: LO_COLS   TYPE REF TO CL_SALV_COLUMNS,
              LO_COLUMN TYPE REF TO CL_SALV_COLUMN.

        LO_COLS = CO_ALV->GET_COLUMNS( ).
        LO_COLS->SET_OPTIMIZE( ABAP_FALSE ).


        DATA: LO_COLS_REF  TYPE        SALV_T_COLUMN_REF,
              LO_COLS_LIST TYPE REF TO CL_SALV_COLUMN_LIST,
              LO_COL_LIST  LIKE LINE OF LO_COLS_REF.

        LO_COLS = O_ALV->GET_COLUMNS( ).
        "lo_cols_c1->set_optimize( abap_false ).
        DATA: LS_DDIC_F4_REF TYPE SALV_S_DDIC_REFERENCE.

        LO_COLS_REF    = LO_COLS->GET( ).

        TRY.

            LOOP AT LO_COLS_REF INTO LO_COL_LIST.
              LO_COLS_LIST ?= LO_COL_LIST-R_COLUMN.    "Narrow casting
              CASE LO_COL_LIST-COLUMNNAME.
                WHEN 'BUKRS' .
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Empresa' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Empresa' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Empresa' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>LEFT ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
                WHEN 'WERKS'.
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Filial' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Filial' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Filial' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>LEFT ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
                WHEN 'PERIODO'.
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Mês\Ano' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Mês\Ano' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Mês\Ano' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
                WHEN 'SALDO_FIXO'.
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Saldo Fixo' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Saldo Fixo' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Saldo Fixo' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '15' ).
                WHEN 'BUTXT'.
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Nm.Empresa' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Nome Empresa' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Nome Empresa' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '20' ).
                WHEN 'NAME1'.
                  LO_COLS_LIST->SET_SHORT_TEXT( 'Nm.Filial' ).
                  LO_COLS_LIST->SET_MEDIUM_TEXT( 'Nome da Filial' ).
                  LO_COLS_LIST->SET_LONG_TEXT( 'Nome da Filial' ).
                  LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  LO_COLS_LIST->SET_OUTPUT_LENGTH( '20' ).
                WHEN OTHERS.
                  LO_COLS_LIST->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
              ENDCASE.
            ENDLOOP.
          CATCH CX_SALV_NOT_FOUND.
        ENDTRY.

      ENDMETHOD.

      METHOD SET_EDIT_ALV.
        DATA:LS_API  TYPE REF TO IF_SALV_GUI_OM_EXTEND_GRID_API,
             LS_EDIT TYPE REF TO IF_SALV_GUI_OM_EDIT_RESTRICTED.

        DATA: LV_REF_TABLE  TYPE REF TO CL_ABAP_TABLEDESCR,
              LV_REF_STRUCT TYPE REF TO CL_ABAP_STRUCTDESCR.

        LS_API = O_ALV->EXTENDED_GRID_API( ).
        LS_EDIT = LS_API->EDITABLE_RESTRICTED( ).
        LV_REF_TABLE  ?= CL_ABAP_TABLEDESCR=>DESCRIBE_BY_DATA( IT_SAIDA ).
        LV_REF_STRUCT ?= LV_REF_TABLE->GET_TABLE_LINE_TYPE( ).
        DATA(LT_DETAILS)   = LV_REF_STRUCT->COMPONENTS.

        LOOP AT LT_DETAILS ASSIGNING FIELD-SYMBOL(<_DETAILS>).
          LS_EDIT->SET_ATTRIBUTES_FOR_COLUMNNAME(
            EXPORTING
              COLUMNNAME              = <_DETAILS>-NAME
              ALL_CELLS_INPUT_ENABLED = ABAP_TRUE
          ).

        ENDLOOP.

        DATA(MO_LISTENER) = NEW LCL_LISTENER( ).
        LS_EDIT->SET_LISTENER( MO_LISTENER ).
        LS_EDIT->VALIDATE_CHANGED_DATA(
    ).

        O_ALV->REFRESH( ).
      ENDMETHOD.

      METHOD GET_DATA.

        FREE: IT_SAIDA.
        CLEAR: WA_SAIDA.
        DATA V TYPE CHAR1 VALUE '/'.
        SELECT DISTINCT
          A~BUKRS,
          B~BUTXT,
          A~WERKS,
          C~NAME1,
          A~MONAT,
          A~GJAHR,
          A~SALDO_FIXO
        FROM
          ZFIT0216 AS A
          LEFT JOIN T001 AS B ON A~BUKRS = B~BUKRS
          LEFT JOIN T001W AS C ON A~WERKS = C~WERKS
          WHERE 1 = 1
          "AND a~bukrs IN @p_bukrs
          AND A~WERKS IN @P_WERKS
          AND A~GJAHR IN @P_GJAHR
          AND A~MONAT IN @P_MONAT
          INTO TABLE @DATA(IT_SAP).

        IF IT_SAP IS INITIAL.
          MESSAGE 'Não foram encontrados dados para esta selecão!' TYPE 'I'.
          SET SCREEN 0.
          LEAVE SCREEN.
          EXIT.
        ENDIF.

        LOOP AT IT_SAP ASSIGNING FIELD-SYMBOL(<_MOVE_SALDO>).
          MOVE-CORRESPONDING <_MOVE_SALDO> TO WA_SAIDA.
          WA_SAIDA-PERIODO = |{ <_MOVE_SALDO>-MONAT }/{ <_MOVE_SALDO>-GJAHR }|.
          APPEND WA_SAIDA TO IT_SAIDA.
          CLEAR WA_SAIDA.
        ENDLOOP.

        FREE: IT_SAP.
        CLEAR:IT_SAP, IT_SAP[], WA_SAIDA.

      ENDMETHOD.


      METHOD SET_PF_STATUS.

        DATA: LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
        LO_FUNCTIONS = CO_ALV->GET_FUNCTIONS( ).
        LO_FUNCTIONS->SET_ALL( ABAP_TRUE ).
        LO_FUNCTIONS->SET_DEFAULT( ABAP_TRUE ).

      ENDMETHOD.

      METHOD ON_USER_COMMAND.

        DATA: LO_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.
        DATA LT_ROWS TYPE SALV_T_ROW.
        DATA LT_COLUMNS TYPE SALV_T_COLUMN.
        DATA LT_CELLS TYPE SALV_T_CELL.
        DATA QTD_ROWS TYPE INT4.

        FREE: LT_ROWS.
        CLEAR: QTD_ROWS.

        LO_SELECTIONS = O_ALV->GET_SELECTIONS( ).
        LT_ROWS = LO_SELECTIONS->GET_SELECTED_ROWS( ).
        QTD_ROWS = LINES( LT_ROWS ).


        CASE E_SALV_FUNCTION.

          WHEN 'GRAVAR'.

            IF IT_SAIDA IS NOT INITIAL.
              LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<_READ>).
              ENDLOOP.
            ELSE.
              MESSAGE 'Teste' TYPE 'I'.
            ENDIF.

          WHEN 'DELETE_ROW'.

            IF QTD_ROWS > 0.
              LOOP AT LT_ROWS ASSIGNING FIELD-SYMBOL(<_INDEX>).
                READ TABLE IT_SAIDA ASSIGNING FIELD-SYMBOL(<_DEL>) INDEX <_INDEX>.
                "DELETE zmmt0185 FROM <_del>.
              ENDLOOP.

            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

            CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.

          WHEN 'REFRESH_ROW'.
            CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.
          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD ON_TOOLBAR.

        DATA : MT_TOOLBAR TYPE STB_BUTTON.

        CLEAR MT_TOOLBAR.
        MT_TOOLBAR-BUTN_TYPE = '3'.   "separator
        APPEND MT_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

        LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING FIELD-SYMBOL(<FS_TOLLBAR>).
          "3 DESABILITA E 0 HABILITA
          IF  <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&COPY_ROW'.
            <FS_TOLLBAR>-BUTN_TYPE = '3'.
          ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&CREATE_ROW'.
            "<fs_tollbar>-butn_type = '3'.
          ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&APPEND'.
            <FS_TOLLBAR>-BUTN_TYPE = '3'.
          ENDIF.
          IF <FS_TOLLBAR>-FUNCTION EQ '&REFRESH'.
            <FS_TOLLBAR>-FUNCTION = 'REFRESH_ROW'.
          ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&DELETE_ROW'.
            <FS_TOLLBAR>-FUNCTION = 'DELETE_ROW'.
          ENDIF.
        ENDLOOP.

      ENDMETHOD.

      METHOD GENERATE_OUTPUT.

        CONTAINER_MAIN = NEW CL_GUI_CUSTOM_CONTAINER(
          PARENT         = CL_GUI_CONTAINER=>DEFAULT_SCREEN
          "lifetime       =  cl_gui_container=>lifetime_dynpro
          CONTAINER_NAME = 'CONTAINER'
        ).

        DATA: LX_MSG TYPE REF TO CX_SALV_MSG.


        TRY.
            CL_SALV_TABLE=>FACTORY(
              EXPORTING
                R_CONTAINER    = CONTAINER_MAIN
                CONTAINER_NAME = 'CONTAINER'
              IMPORTING
                R_SALV_TABLE   = O_ALV
              CHANGING
                T_TABLE        = IT_SAIDA ).
          CATCH CX_SALV_MSG INTO LX_MSG.
        ENDTRY.

        CALL METHOD SET_PF_STATUS
          CHANGING
            CO_ALV = O_ALV.

        CALL METHOD SET_LAYOUT
          CHANGING
            CO_ALV = O_ALV.

        CALL METHOD SET_HANDLER
          CHANGING
            CO_ALV    = O_ALV
            CO_REPORT = LO_REPORT.

        CALL METHOD ME->SET_COLUMNS_BUILD
          CHANGING
            CO_ALV = O_ALV.

        DATA LR_SELECTIONS        TYPE REF TO CL_SALV_SELECTIONS.
* Enable cell selection mode
        LR_SELECTIONS = O_ALV->GET_SELECTIONS( ).
        LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

        O_ALV->DISPLAY( ).

      ENDMETHOD.

      METHOD SET_HANDLER.
*
*...HotSpot
        DATA: LO_COLS_TAB TYPE REF TO CL_SALV_COLUMNS_TABLE,
              LO_COL_TAB  TYPE REF TO CL_SALV_COLUMN_TABLE,
              LO_EVENTS   TYPE REF TO CL_SALV_EVENTS_TABLE.

        LO_COLS_TAB = CO_ALV->GET_COLUMNS( ).
        LO_EVENTS = CO_ALV->GET_EVENT( ).

*   event handler
        SET HANDLER CO_REPORT->ON_LINK_CLICK FOR LO_EVENTS.
        SET HANDLER CO_REPORT->ON_USER_COMMAND FOR LO_EVENTS.
        SET HANDLER CO_REPORT->ON_TOOLBAR FOR ALL INSTANCES ACTIVATION 'X'.
        SET HANDLER CO_REPORT->ON_CHANGE_DATA FOR ALL INSTANCES ACTIVATION 'X'.
        "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
      ENDMETHOD.

      METHOD ON_LINK_CLICK.

      ENDMETHOD.


      METHOD SET_LAYOUT.
*
        DATA: LO_LAYOUT  TYPE REF TO CL_SALV_LAYOUT,
              LF_VARIANT TYPE SLIS_VARI,
              LS_KEY     TYPE SALV_S_LAYOUT_KEY.
*   get layout object
        LO_LAYOUT = CO_ALV->GET_LAYOUT( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
        LS_KEY-REPORT = SY-REPID.
        LO_LAYOUT->SET_KEY( LS_KEY ).
*   2. Remove Save layout the restriction.
        LO_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
*   set initial Layout
        LF_VARIANT = 'DEFAULT'.
        LO_LAYOUT->SET_INITIAL_LAYOUT( LF_VARIANT ).
      ENDMETHOD.

    ENDCLASS.
