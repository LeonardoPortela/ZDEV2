*&---------------------------------------------------------------------*
*&  Include           ZMMR104_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  TRATAR_CAMPOS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATAR_FIELDS_0110 OUTPUT.
  LOOP AT GT_FIELDS INTO WL_FIELDS.
    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 = WL_FIELDS-GROUP1.

      SCREEN-INPUT        = WL_FIELDS-VALUE.
      SCREEN-INVISIBLE    = WL_FIELDS-INVISIBLE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATAR_CAMPOS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_SELECTED_ROWS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SELECTED_ROWS OUTPUT.
  CHECK ( WL_CELL IS NOT INITIAL ).

  REFRESH GT_SELECTED_ROWS.

  WL_SELECTED_ROWS = WL_CELL-ROW_ID.
  APPEND WL_SELECTED_ROWS TO GT_SELECTED_ROWS.

  CALL METHOD OBJ_ALV_0110->SET_SELECTED_ROWS
    EXPORTING
      IT_INDEX_ROWS = GT_SELECTED_ROWS.
ENDMODULE.                 " SET_SELECTED_ROWS  OUTPUT

*&---------------------------------------------------------------------*
*&  Include           Z_FORMS_MONTAR_ALV_0100
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA LT_CODE TYPE TABLE OF SY-UCOMM.

  REFRESH LT_CODE.

  IF SY-UCOMM = C_VIEW_ITEM.
    R_UTILS->TRATAR_CAMPOS( GROUP1    = 'BT1'
                            VALUE     = '0'
                            INVISIBLE = '0' ).

    APPEND: C_NOVO   TO LT_CODE,
            C_COPY   TO LT_CODE,
            C_EDIT   TO LT_CODE,
            C_DELETE TO LT_CODE,
            C_SEARCH TO LT_CODE,
            C_SAVE   TO LT_CODE.
  ELSE.

    IF ( VG_SCREEN_PRINCIPAL EQ '0110' ).
      IF ( WL_SAIDA_CABECALHO-EVRTN IS INITIAL ).
        APPEND: C_COPY TO LT_CODE,
                C_EDIT TO LT_CODE,
                C_DELETE TO LT_CODE.
      ENDIF.
    ELSE.
      APPEND: C_NOVO   TO LT_CODE,
              C_COPY   TO LT_CODE,
              C_EDIT   TO LT_CODE,
              C_DELETE TO LT_CODE,
              C_SEARCH TO LT_CODE,
              C_SAVE   TO LT_CODE,
              C_GERA_CONTRATO TO LT_CODE.
    ENDIF.
  ENDIF.

  IF WL_SAIDA_CABECALHO-VEDAT IS INITIAL.
    WL_SAIDA_CABECALHO-VEDAT = SY-DATUM.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING LT_CODE.
  SET TITLEBAR '0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0100 OUTPUT
*----------------------------------------------------------------------*
MODULE PBO_0110 OUTPUT.
  DATA OBJ_TOOLBAR TYPE REF TO LCL_EVENT_TOOLBAR.

  CLEAR GT_FCAT_0110.
  PERFORM ALV_PREENCHE_CAT USING:
 'STATUS' 'Status'         '6'  'X' '' 'C' '' ' '  '' '' '' '',
 'EVRTP'  'Item'           '5'  '' '' ''  ''  ' '  '' '' '' '',
 'EPSTP'  'Ctg Item'       '8'  '' '' ''  ''  ' ' 'EKPO' 'PSTYP' '' '',
 'KNTTP'  'Cl.Cont'        '8'  '' '' ''  ''  ' ' 'EKPO' 'KNTTP' '' '',
 'EMATN'  'Material'       '8'  '' 'X' '' ''  'X' 'EKPO' 'EMATN' '' '',
 'TXZ01'  'Descrição'      '25' '' '' ''  ''  ' '  '' '' '' '',
 'KTMNG'  'Qtd Prevista'   '12' '' '' ''  ''  'X' 'EKPO' 'KTMNG' '' '',
 'MEINS'  'Unid. Medida'   '12' '' '' ''  ''  'X' 'EKPO' 'MEINS' '' '',
 'NETPR'  'Preço Liquido'  '12' '' '' ''  ''  'X' 'EKPO' 'NETPR' '' '',
 'PEINH'  'Por'            '06' '' '' ''  ''  'X' 'EKPO' 'PEINH' '' '',
 'MATKL'  'Grp Mercadoria' '14' '' '' ''  ''  'X' 'EKPO' 'MATKL' '' '',
 'LGORT'  'Depósito'       '8'  '' '' ''  ''  'X' ' '    ' '     '' '',
 'WERKS'  'Centro'         '6' 'X' '' 'C' ''  'X' ' '    ' '     '' '',
 'MWSKZ'  'Cód IVA'        '7'  '' '' ''  ''  'X' ' '    ' '     '' ''.

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

    WL_LAYOUT-GRID_TITLE = 'Itens do contrato básico'.
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
ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_INCOTERMS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_INCOTERMS INPUT.
  REFRESH: GT_DSELC, GT_RETURN_TAB.

  SELECT INCO1 BEZEI
    FROM TINCT
    INTO TABLE GT_QPCT_INCO
   WHERE SPRAS = 'PT'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CODE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'QPCT-CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_QPCT_INCO
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                 " HELP_INCOTERMS  INPUT

MODULE HELP_ZTERM_PAYMENT.
*  SELECT ZTERM VTEXT
*    FROM TVZBT
*    INTO TABLE GT_QPCT_INCO
*   WHERE SPRAS = 'PT'.
*
*  DELETE GT_QPCT_INCO WHERE CODE(1) NE 'Z'.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      RETFIELD        = 'CODE'
*      DYNPPROG        = SY-REPID
*      DYNPNR          = SY-DYNNR
*      DYNPROFIELD     = 'QPCT-CODE'
*      VALUE_ORG       = 'S'
*    TABLES
*      VALUE_TAB       = GT_QPCT_INCO
*      RETURN_TAB      = GT_RETURN_TAB
*      DYNPFLD_MAPPING = GT_DSELC.
*
*  TRY.
*      DATA(_ZTERM) = VALUE T052-ZTERM( GT_RETURN_TAB[ 1 ]-FIELDVAL ).

  DATA ZTERM TYPE T052-ZTERM.

  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
      I_KOART       = 'K'
    IMPORTING
      E_ZTERM       = ZTERM
    EXCEPTIONS
      NOTHING_FOUND = 01.

  IF ZTERM IS NOT INITIAL.
    WL_SAIDA_CABECALHO-ZTERM = ZTERM.
  ENDIF.

*  LEAVE TO SCREEN 0100.
*    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  HELP_TP_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_TP_CONTRATO INPUT.
  REFRESH: GT_DSELC, GT_RETURN_TAB.

  SELECT BSART BATXT
    FROM T161T
    INTO TABLE GT_QPCT_INCO
   WHERE SPRAS = 'PT'
     AND BSTYP = 'K'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CODE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'QPCT-CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_QPCT_INCO
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                 " HELP_TP_CONTRATO  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.
*  CLEAR: WL_LAYOUT, GT_FCAT_0200.
*  PERFORM ALV_PREENCHE_CAT USING:
* 'EBELP' 'Nº'           '3'  '' '' '' ''  ''  '' '' '' '',
* 'VPROZ' 'Qtd/Porcent'  '12' '' '' '' ''  'X' 'EKKN' 'VPROZ' '' '',
* 'KOSTL' 'Centro Custo' '15' '' '' '' ''  'X' 'EKKN' 'KOSTL' '' '',
* 'SAKTO' 'Cta Razão'    '11' '' '' '' ''  'X' '' '' 'GT_ITEM_0200' 'X',
* 'AUFNR' 'Ordem'        '6'  '' '' '' ''  'X' 'EKKN' 'AUFNR' '' ''.
*
*  IF ( OBJ_CUSTOM_0200 IS INITIAL ).
*    CREATE OBJECT OBJ_CUSTOM_0200
*      EXPORTING
*        CONTAINER_NAME              = 'CUSTOM_0200'
*      EXCEPTIONS
*        CNTL_ERROR                  = 1
*        CNTL_SYSTEM_ERROR           = 2
*        CREATE_ERROR                = 3
*        LIFETIME_ERROR              = 4
*        LIFETIME_DYNPRO_DYNPRO_LINK = 5
*        OTHERS                      = 6.
*
*    CREATE OBJECT OBJ_ALV_0200
*      EXPORTING
*        I_PARENT          = OBJ_CUSTOM_0200
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.
*
*    PERFORM REGISTER_F4_FOR_FIELDS.
*
*    WL_LAYOUT-GRID_TITLE = ''.
*    WL_LAYOUT-NO_ROWMARK = 'X'.
*    WL_LAYOUT-STYLEFNAME = 'STYLE'.
*    WL_STABLE-ROW        = 'X'.
*    WL_STABLE-COL        = 'X'.
*
*    SET HANDLER:
*    LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR OBJ_ALV_0200,
*    LCL_EVENT_HANDLER=>ON_ONF4                  FOR OBJ_ALV_0200,
*    LCL_EVENT_TOOLBAR=>SET_TOOLBAR              FOR OBJ_ALV_0200,
*    LCL_EVENT_TOOLBAR=>GET_UCOMM                FOR OBJ_ALV_0200.
*
*    CALL METHOD OBJ_ALV_0200->SET_TABLE_FOR_FIRST_DISPLAY
*      EXPORTING
*        IS_LAYOUT                     = WL_LAYOUT
*        IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
*        I_SAVE                        = 'A'
*      CHANGING
*        IT_OUTTAB                     = GT_ITEM_0200
*        IT_FIELDCATALOG               = GT_FCAT_0200
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*
*    CALL METHOD OBJ_ALV_0200->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*    CALL METHOD OBJ_ALV_0200->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
*  ELSE.
*    CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WL_STABLE.
*  ENDIF.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0400 OUTPUT.
*  CLEAR: WL_LAYOUT, GT_FCAT_0400.
*  PERFORM ALV_PREENCHE_CAT USING:
* 'ITEM_NO'    'Nº'            '3'  '' '' '' ''  ''  '' '' '' '',
* 'SRVPOS'     'Nº serviço'    '10' '' '' '' ''  'X' 'ESLL'  'SRVPOS' 'GT_SAIDA_SERVICOS' '',
* 'KTEXT1'     'Texto breve'   '30' '' '' '' ''  'X' 'ESLL'  'MEINS'  'GT_SAIDA_SERVICOS' '',
* 'MENGE'      'Quantidade'    '10' '' '' '' ''  'X' '' '' '' '',
* 'MEINS'      'UM'            '3'  '' '' '' ''  'X' 'ESLH'  'WAERS'  'GT_SAIDA_SERVICOS' '',
* 'TBTWR'      'Preço bruto'   '11' '' '' '' ''  'X' 'RM11P' 'KOSTL' 'GT_SAIDA_SERVICOS' '',
* 'WAERS'      'Moeda'         '5'  '' '' '' ''  'X' '' '' '' '',
* 'KOSTL'      'Centro Custo'  '15' '' '' '' ''  'X' '' '' '' '',
* 'AUFNR'      'Ordem'         '6'  '' '' '' ''  'X' '' '' '' ''.
*
*  IF ( OBJ_CUSTOM_0400 IS INITIAL ).
*    CREATE OBJECT OBJ_CUSTOM_0400
*      EXPORTING
*        CONTAINER_NAME              = 'CUSTOM_0400'
*      EXCEPTIONS
*        CNTL_ERROR                  = 1
*        CNTL_SYSTEM_ERROR           = 2
*        CREATE_ERROR                = 3
*        LIFETIME_ERROR              = 4
*        LIFETIME_DYNPRO_DYNPRO_LINK = 5
*        OTHERS                      = 6.
*
*    CREATE OBJECT OBJ_ALV_0400
*      EXPORTING
*        I_PARENT          = OBJ_CUSTOM_0400
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.
*
*  ENDIF.
*
*  WL_LAYOUT-GRID_TITLE = ''.
*  WL_LAYOUT-NO_ROWMARK = 'X'.
*  WL_LAYOUT-STYLEFNAME = 'STYLE'.
*  WL_STABLE-ROW        = 'X'.
*  WL_STABLE-COL        = 'X'.
*
*  SET HANDLER:
*  LCL_EVENT_TOOLBAR=>SET_TOOLBAR  FOR OBJ_ALV_0400,
*  LCL_EVENT_TOOLBAR=>GET_UCOMM    FOR OBJ_ALV_0400.
*
*  CALL METHOD OBJ_ALV_0400->SET_TABLE_FOR_FIRST_DISPLAY
*    EXPORTING
*      IS_LAYOUT                     = WL_LAYOUT
*      IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
*      I_SAVE                        = 'A'
*    CHANGING
*      IT_OUTTAB                     = GT_SAIDA_SERVICOS
*      IT_FIELDCATALOG               = GT_FCAT_0400
*    EXCEPTIONS
*      INVALID_PARAMETER_COMBINATION = 1
*      PROGRAM_ERROR                 = 2
*      TOO_MANY_LINES                = 3
*      OTHERS                        = 4.
*
*  CALL METHOD OBJ_ALV_0400->REGISTER_EDIT_EVENT
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CALL METHOD OBJ_ALV_0400->REGISTER_EDIT_EVENT
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
*  CALL METHOD OBJ_ALV_0400->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE = WL_STABLE.
ENDMODULE.                 " PBO_0400  OUTPUT
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

  IF ( WL_SAIDA_CABECALHO-BUKRS IS NOT INITIAL ).

    SELECT *
      FROM J_1BBRANCH AS A
     INNER JOIN T001W AS B ON A~BRANCH = B~WERKS
      INTO CORRESPONDING FIELDS OF TABLE GT_BRANCH
     WHERE A~BUKRS = WL_SAIDA_CABECALHO-BUKRS.

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
ENDMODULE.                 " HELP_WERKS  INPUT

*&---------------------------------------------------------------------*
*&      Module  TRATAR_CAMPOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATAR_CAMPOS_0200 OUTPUT.
  LOOP AT GT_FIELDS INTO WL_FIELDS.
    LOOP AT SCREEN.
      CHECK ( SCREEN-GROUP1 = WL_FIELDS-GROUP1 ).

      SCREEN-INPUT        = WL_FIELDS-VALUE.
      SCREEN-INVISIBLE    = WL_FIELDS-INVISIBLE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATAR_CAMPOS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  DATA BTN_COLLAPSE TYPE CHAR4 VALUE ICON_EXPAND.

  SET PF-STATUS '0300'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0300  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  DATA: OBJ_DOCKING               TYPE REF TO CL_GUI_DOCKING_CONTAINER,
        OBJ_CUSTOM_TREE_PRINCIPAL TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

  IF ( OBJ_CUSTOM_TREE_PRINCIPAL IS INITIAL ).
    CREATE OBJECT OBJ_DOCKING
      EXPORTING
        SIDE      = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_LEFT
        EXTENSION = 230
        REPID     = SY-REPID
        DYNNR     = '0100'.

    CREATE OBJECT OBJ_CUSTOM_TREE_PRINCIPAL
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_TREE'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_TREE_0100
      EXPORTING
        PARENT              = OBJ_DOCKING
        NODE_SELECTION_MODE = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
        ITEM_SELECTION      = ''
        NO_HTML_HEADER      = 'X'
        NO_TOOLBAR          = 'X'.

    PERFORM F_BUILD_HEADER USING 30 TEXT-I02 CHANGING GW_HEADER.

    WL_FCAT-NO_OUT = 'X'.
    APPEND WL_FCAT TO GT_FCAT_0100.
    CLEAR WL_FCAT.

    CALL METHOD OBJ_ALV_TREE_0100->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_HIERARCHY_HEADER = GW_HEADER
      CHANGING
        IT_OUTTAB           = GT_MENU_TREE
        IT_FIELDCATALOG     = GT_FCAT_0100.

    PERFORM F_CREATE_HIERARCHY.

    D_SET_EVENT:
    CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK ABAP_TRUE.

    OBJ_ALV_TREE_0100->SET_REGISTERED_EVENTS(
                       EXPORTING
                       EVENTS = GT_EVENTS
                       EXCEPTIONS
                       CNTL_ERROR                = 1
                       CNTL_SYSTEM_ERROR         = 2
                       ILLEGAL_EVENT_COMBINATION = 3 ).
  ENDIF.

  SET HANDLER LCL_EVENT_HANDLER=>HANDLE_DOUBLE_CLICK
          FOR OBJ_ALV_TREE_0100.

  OBJ_ALV_TREE_0100->FRONTEND_UPDATE( ).
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0120 OUTPUT.
  DATA: LT_NODE_TABLE        TYPE TREEV_NTAB,
        LT_ITEM_TABLE        TYPE ITEM_TABLE_TYPE,
        OBJ_CUSTOM_APROVACAO TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

  IF ( OBJ_CUSTOM_APROVACAO IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_APROVACAO
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_TREE_APROVACAO'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    PERFORM F_BUILD_HEADER USING 23 TEXT-I03 CHANGING GW_HEADER.

    CREATE OBJECT OBJ_ALV_TREE_0120
      EXPORTING
        PARENT                = OBJ_CUSTOM_APROVACAO
        NODE_SELECTION_MODE   = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_MULTIPLE
        ITEM_SELECTION        = ABAP_TRUE
        HIERARCHY_COLUMN_NAME = C_COLUMN-COLUMN1
        HIERARCHY_HEADER      = GW_HEADER.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_COLUMN
      EXPORTING
        NAME        = C_COLUMN-COLUMN2
        WIDTH       = 10
        ALIGNMENT   = CL_GUI_COLUMN_TREE=>ALIGN_CENTER
        HEADER_TEXT = 'Item'.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_COLUMN
      EXPORTING
        NAME        = C_COLUMN-COLUMN6
        WIDTH       = 10
        ALIGNMENT   = CL_GUI_COLUMN_TREE=>ALIGN_CENTER
        HEADER_TEXT = 'Centro'.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_COLUMN
      EXPORTING
        NAME        = C_COLUMN-COLUMN3
        WIDTH       = 6
        ALIGNMENT   = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT
        HEADER_TEXT = ''.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_COLUMN
      EXPORTING
        NAME        = C_COLUMN-COLUMN4
        WIDTH       = 6
        ALIGNMENT   = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT
        HEADER_TEXT = ''.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_COLUMN
      EXPORTING
        NAME        = C_COLUMN-COLUMN5
        WIDTH       = 6
        ALIGNMENT   = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT
        HEADER_TEXT = ''.

    CLEAR GT_EVENTS.

    D_SET_EVENT:
    CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK ABAP_TRUE.

    OBJ_ALV_TREE_0120->SET_REGISTERED_EVENTS(
                       EXPORTING
                       EVENTS = GT_EVENTS
                       EXCEPTIONS
                       CNTL_ERROR                = 1
                       CNTL_SYSTEM_ERROR         = 2
                       ILLEGAL_EVENT_COMBINATION = 3 ).

    SET HANDLER: LCL_EVENT_HANDLER=>HANDLE_BUTTON_CLICK
                 FOR OBJ_ALV_TREE_0120.

    PERFORM F_CREATE_HIERARCHY.

    CALL METHOD OBJ_ALV_TREE_0120->ADD_NODES_AND_ITEMS
      EXPORTING
        NODE_TABLE                     = GT_NODE_TABLE
        ITEM_TABLE                     = GT_ITEM_TABLE
        ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
      EXCEPTIONS
        FAILED                         = 1
        CNTL_SYSTEM_ERROR              = 3
        ERROR_IN_TABLES                = 4
        DP_ERROR                       = 5
        TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  ENDIF.
ENDMODULE.                 " PBO_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_EXCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_EXCEL INPUT.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar !'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDMODULE.
