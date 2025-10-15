*----------------------------------------------------------------------*
***INCLUDE ZMMR114_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
  "
  DATA OBJ_TOOLBAR TYPE REF TO LCL_EVENT_TOOLBAR.
  CLEAR GT_FCAT_0110.
  PERFORM ALV_PREENCHE_CAT USING:
          'WERKS'       'Centro'         '06' 'X' '' 'C' '' 'X' '' '' '' '',
          'MATNR'       'Material'       '08' '' 'X' ''  '' 'X' 'EKPO' 'EMATN' '' '',
          'TXZ01'       'Descrição'      '25' '' ''  ''  '' ' ' '' '' '' '',
          'APROVADOR'   'Aprovador'      '25' '' ''  ''  '' 'X' '' '' '' '',
          'DATA_ATUAL'  'Data '          '10' '' ''  ''  '' ' ' '' '' '' '',
          'HORA_ATUAL'  'Hora '          '10' '' ''  ''  '' ' ' '' '' '' '',
          'USUARIO'     'Criado por '    '15' '' ''  ''  '' ' ' '' '' '' ''.



  IF ( OBJ_CUSTOM_0110 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0110
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_0110'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0110
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0110
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CREATE OBJECT OBJ_TOOLBAR
      EXPORTING
        IO_ALV_GRID = OBJ_ALV_0110.

    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING WL_EXCLUDE.
*    PERFORM REGISTER_F4_FOR_FIELDS.

    WL_LAYOUT-GRID_TITLE = 'Aprovadores contrato'.
    WL_LAYOUT-NO_ROWMARK = ' '.
    WL_LAYOUT-STYLEFNAME = 'ESTILO'.
    WL_STABLE-ROW        = 'X'.
    WL_STABLE-COL        = 'X'.

    SET HANDLER:
    LCL_EVENT_HANDLER=>ON_DATA_CHANGED
    LCL_EVENT_HANDLER=>ON_BUTTON_CLICK
    LCL_EVENT_TOOLBAR=>ON_CLICK
    LCL_EVENT_TOOLBAR=>SET_TOOLBAR
    LCL_EVENT_TOOLBAR=>GET_UCOMM FOR OBJ_ALV_0110.

    CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WL_LAYOUT
        IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = GT_SAIDA_ITENS
        IT_FIELDCATALOG               = GT_FCAT_0110
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WL_STABLE.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_WERKS INPUT.
  REFRESH: GT_RETURN_TAB, GT_DSELC.

  DATA: BEGIN OF GT_BRANCH OCCURS 0,
          BUKRS TYPE J_1BBRANCH-BUKRS,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
          ORTO1 TYPE T001W-ORT01,
          REGIO TYPE T001W-REGIO,
        END OF GT_BRANCH.

  IF ( WG_BUKRS IS NOT INITIAL ).

    SELECT *
      FROM J_1BBRANCH AS A
     INNER JOIN T001W AS B ON A~BRANCH = B~WERKS
      INTO CORRESPONDING FIELDS OF TABLE GT_BRANCH
     WHERE A~BUKRS = WG_BUKRS.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'LOW'
        DYNPPROG        = SY-REPID
        DYNPNR          = SY-DYNNR
        DYNPROFIELD     = 'R_WERKS-LOW'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = GT_BRANCH
        RETURN_TAB      = GT_RETURN_TAB
        DYNPFLD_MAPPING = GT_DSELC.
  ELSE.
    MESSAGE S836(SD) WITH TEXT-E02 'Empresa.' DISPLAY LIKE 'E' .
  ENDIF.
ENDMODULE.
