*&---------------------------------------------------------------------*
*&  Include           ZGLT067_000X
*&---------------------------------------------------------------------*

DATA: C_ALV_TOOLBAR_4003 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBAR_4006 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBAR_4004 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBAR_4007 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

"Objetos para documentos em aberto """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4003 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4003 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4003 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4003 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_4003
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF TL_400X EQ TL_4001.
      TY_TOOLBAR-ICON      = ICON_ZOOM_IN.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_IN'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ELSEIF TL_400X EQ TL_4006.
      TY_TOOLBAR-ICON      = ICON_ZOOM_OUT.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_OUT'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_ASSIGN.
    TY_TOOLBAR-FUNCTION  = 'VINCULAR'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_4003->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ZOOM_IN'.
        TL_400X = TL_4006.
      WHEN 'ZOOM_OUT'.
        TL_400X = TL_4001.
      WHEN 'VINCULAR'.
        PERFORM VINCULAR_ITENS.
    ENDCASE.
    LEAVE TO SCREEN 0004.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_4003 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4006 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4006 DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4006 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4006 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_4006
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF TL_400X EQ TL_4001.
      TY_TOOLBAR-ICON      = ICON_ZOOM_IN.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_IN'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ELSEIF TL_400X EQ TL_4006.
      TY_TOOLBAR-ICON      = ICON_ZOOM_OUT.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_OUT'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_ASSIGN.
    TY_TOOLBAR-FUNCTION  = 'VINCULAR'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_4006->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ZOOM_IN'.
        TL_400X = TL_4006.
      WHEN 'ZOOM_OUT'.
        TL_400X = TL_4001.
      WHEN 'VINCULAR'.
        PERFORM VINCULAR_ITENS.
    ENDCASE.
    LEAVE TO SCREEN 0004.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_4006 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER_4006 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_4006 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER_4006  TYPE REF TO LCL_EVENT_HANDLER_4006.
DATA: EVENT_HANDLER_4003  TYPE REF TO LCL_EVENT_HANDLER_4006.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER_4006 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_4006 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_4006 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK_4006 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


"Objetos para documentos p/ Compensação """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4004 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4004 DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4004 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4004 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_4004
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF TL_400X EQ TL_4001.
      TY_TOOLBAR-ICON      = ICON_ZOOM_IN.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_IN'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ELSEIF TL_400X EQ TL_4006.
      TY_TOOLBAR-ICON      = ICON_ZOOM_OUT.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_OUT'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_UNASSIGN.
    TY_TOOLBAR-FUNCTION  = 'DESVINCULAR'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_4004->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ZOOM_IN'.
        TL_400X = TL_4006.
      WHEN 'ZOOM_OUT'.
        TL_400X = TL_4001.
      WHEN 'DESVINCULAR'.
        PERFORM DESVINCULAR_ITENS.
    ENDCASE.
    LEAVE TO SCREEN 0004.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_4004 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4007 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4007 DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_4007 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_4007 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_4007
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF TL_400X EQ TL_4001.
      TY_TOOLBAR-ICON      = ICON_ZOOM_IN.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_IN'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ELSEIF TL_400X EQ TL_4006.
      TY_TOOLBAR-ICON      = ICON_ZOOM_OUT.
      TY_TOOLBAR-FUNCTION  = 'ZOOM_OUT'.
      TY_TOOLBAR-BUTN_TYPE = 0.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_UNASSIGN.
    TY_TOOLBAR-FUNCTION  = 'DESVINCULAR'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_4007->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ZOOM_IN'.
        TL_400X = TL_4006.
      WHEN 'ZOOM_OUT'.
        TL_400X = TL_4001.
      WHEN 'DESVINCULAR'.
        PERFORM DESVINCULAR_ITENS.
    ENDCASE.
    LEAVE TO SCREEN 0004.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_4007 IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER_4007 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_4007 DEFINITION.
  PUBLIC SECTION.
    METHODS DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID IMPORTING E_MODIFIED ET_GOOD_CELLS.
    METHODS DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING ER_DATA_CHANGED.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
    METHODS SUBTOTAL_TEXT FOR EVENT SUBTOTAL_TEXT OF CL_GUI_ALV_GRID IMPORTING ES_SUBTOTTXT_INFO EP_SUBTOT_LINE E_EVENT_DATA.
  PRIVATE SECTION.
    DATA: ERROR_IN_DATA TYPE C.
    METHODS: PERFORM_SEMANTIC_CHECKS IMPORTING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER_4007  TYPE REF TO LCL_EVENT_HANDLER_4007.
DATA: EVENT_HANDLER_4004  TYPE REF TO LCL_EVENT_HANDLER_4007.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER_4007 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_4007 IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_4007 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK_4007 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD PERFORM_SEMANTIC_CHECKS.
    DATA: WA_BSXX_ALV_C TYPE ZDE_BSXX_COMP_ALV_C,
          LS_GOOD       TYPE LVC_S_MODI,
          LV_VALUE      TYPE LVC_VALUE,
          WL_VALOR      TYPE ZDE_PAYMENTS.

    LOOP AT PR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'VALOR_PAYMENTS'.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      WL_VALOR = LV_VALUE.

      READ TABLE IT_BSXX_ALV_C INTO WA_BSXX_ALV_C INDEX LS_GOOD-ROW_ID.

      IF ABS( WA_BSXX_ALV_C-WRBTR ) LT ABS( WL_VALOR ).
        ERROR_IN_DATA = ABAP_TRUE.

        WA_BSXX_ALV_C-WRBTR = ABS( WA_BSXX_ALV_C-WRBTR ).
        WRITE WA_BSXX_ALV_C-WRBTR TO LV_VALUE.
        CONDENSE LV_VALUE NO-GAPS.

        CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
            I_MSGID     = 'ZFI'
            I_MSGNO     = '032'
            I_MSGTY     = 'E'
            I_MSGV1     = LV_VALUE
            I_FIELDNAME = LS_GOOD-FIELDNAME
            I_ROW_ID    = LS_GOOD-ROW_ID.

      ELSEIF  ABS( WL_VALOR ) LT 0.
        ERROR_IN_DATA = ABAP_TRUE.
        CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
            I_MSGID     = 'ZFI'
            I_MSGNO     = '033'
            I_MSGTY     = 'E'
            I_FIELDNAME = LS_GOOD-FIELDNAME
            I_ROW_ID    = LS_GOOD-ROW_ID.
      ELSEIF ABS( WL_VALOR ) LT ABS( WA_BSXX_ALV_C-WRBTR ).
*        CLEAR: ZDE_MOV_FORNECEDOR.
*        CALL SCREEN 0303 STARTING AT 15 01.
*        IF ZDE_MOV_FORNECEDOR IS INITIAL.
*          ERROR_IN_DATA = ABAP_TRUE.
*          CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
*            EXPORTING
*              I_MSGID     = 'ZFI'
*              I_MSGNO     = '034'
*              I_MSGTY     = 'E'
*              I_FIELDNAME = LS_GOOD-FIELDNAME
*              I_ROW_ID    = LS_GOOD-ROW_ID.
*        ELSE.
*          MOVE ZDE_MOV_FORNECEDOR-DATA_RESIDUAL TO LV_VALUE.
*
*          CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'DATA_RESIDUAL'
*              I_VALUE     = LV_VALUE.
*
*          MOVE ZDE_MOV_FORNECEDOR-SGTXT TO LV_VALUE.
*
*          CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'SGTXT_RES'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "PERFORM_SEMANTIC_CHECKS

  METHOD DATA_CHANGED.
    ERROR_IN_DATA = SPACE.
    CALL METHOD PERFORM_SEMANTIC_CHECKS( ER_DATA_CHANGED ).
    IF ERROR_IN_DATA = 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.
  ENDMETHOD.                    "DATA_CHANGED

  METHOD DATA_CHANGED_FINISHED.

    FIELD-SYMBOLS: <FS_DOC> TYPE ZDE_BSXX_COMP_ALV_C.

    IF E_MODIFIED IS NOT INITIAL.
      " Valor Residual """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      LOOP AT IT_BSXX_ALV_C ASSIGNING <FS_DOC>.
        <FS_DOC>-VALOR_RESIDUAL = ABS( <FS_DOC>-WRBTR ) - ABS( <FS_DOC>-VALOR_PAYMENTS ).
        PERFORM SINAL_VALOR USING <FS_DOC>-KOART <FS_DOC>-SHKZG CHANGING <FS_DOC>-VALOR_RESIDUAL.
      ENDLOOP.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      PERFORM VERIFICAR_COMPENSACAO.

      LEAVE TO SCREEN 0004.
    ENDIF.
  ENDMETHOD.                    "DATA_CHANGED_FINISHED

  METHOD SUBTOTAL_TEXT.

    DATA: WA_ALV_C_1 TYPE ZDE_BSXX_COMP_ALV_C,
          WA_ALV_C_2 TYPE ZDE_BSXX_COMP_ALV_C,
          LC_SALDO 	 TYPE ZDE_PAYMENTS.

    FIELD-SYMBOLS: <FS>  TYPE ANY.
    FIELD-SYMBOLS: <FS2> TYPE ANY.

    ASSIGN E_EVENT_DATA->M_DATA->* TO <FS>.
    IF SY-SUBRC EQ 0.

      <FS> = 'Saldo'.

      IF ES_SUBTOTTXT_INFO(8) EQ 'CONTROLE'.

        ASSIGN EP_SUBTOT_LINE->* TO <FS2>.
        WA_ALV_C_1 = <FS2>.

        LC_SALDO = 0.
        LOOP AT IT_BSXX_ALV_C INTO WA_ALV_C_2.
          PERFORM SINAL_VALOR USING WA_ALV_C_2-KOART WA_ALV_C_2-SHKZG CHANGING WA_ALV_C_2-VALOR_PAYMENTS.
          ADD WA_ALV_C_2-VALOR_PAYMENTS TO LC_SALDO.
        ENDLOOP.

        WA_ALV_C_1-VALOR_PAYMENTS = LC_SALDO.
        <FS2> = WA_ALV_C_1.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "subtotal_text

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
