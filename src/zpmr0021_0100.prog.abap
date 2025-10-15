*&---------------------------------------------------------------------*
*&  Include           ZPMR0021_0100
*&---------------------------------------------------------------------*

DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
      PICTURE            TYPE REF TO CL_GUI_PICTURE,
      GS_LAYOUT          TYPE LVC_S_LAYO,
      GS_VARIANT         TYPE DISVARIANT,
      IT_FIELDCATALOG    TYPE LVC_T_FCAT,
      WA_FIELDCATALOG    TYPE LVC_S_FCAT,
      CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN             TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1           TYPE REF TO CL_DD_AREA,
      COLUMN_2           TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  "BREAK-POINT.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: URL(255)                TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
        VL_CONT                 TYPE I.

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    "create a container
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 20.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    PERFORM FILL_IT_FIELDCATALOG USING:
          01 'EQUNR'      'IT_SAIDA'    ' '     ' '   'Equipamento',
          02 'IWERK'      'IT_SAIDA'    ' '     ' '   'Centro',
          03 'AUFNR'      'IT_SAIDA'    'X'     ' '   'Ordem',
          04 'KTEXT'      'IT_SAIDA'    ' '     ' '   'Serviço',
          05 'DESCR'      'IT_SAIDA'    ' '     ' '   'Custo',
          06 'VLR_C'      'IT_SAIDA'    ' '     ' '   'Valor Total',
          07 'ZPERC_F'    'IT_SAIDA'    ' '     ' '   '% Forn.',
          08 'ZPERC_C'    'IT_SAIDA'    ' '     ' '   '% Cliente',
          09 'VLR_F'      'IT_SAIDA'    ' '     ' '   'Vlr. Forn.',
          10 'TOTCC'      'IT_SAIDA'    ' '     ' '   'Vlr. Cliente'.

    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-CWIDTH_OPT = 'X'.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    SET HANDLER: LCL_EVENTOS=>ON_HOTSPOT_CLICK FOR CTL_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        IS_VARIANT      = GS_VARIANT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_SAIDA.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 1
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT.

    CALL METHOD TABLE_ELEMENT->ADD_COLUMN
      IMPORTING
        COLUMN = COLUMN.

    CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
      EXPORTING
        COL_NO    = 1
        SAP_ALIGN = 'LEFT'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

    P_TEXT = TEXT-002.

    CALL METHOD COLUMN->ADD_TEXT
      EXPORTING
        TEXT      = P_TEXT
        SAP_STYLE = 'HEADING'.

    "------------ cabeçalho

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 2
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT2.

    CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
      EXPORTING
        SAP_STYLE   = 'SAP_BOLD'
        STYLE_CLASS = 'SAP_BOLD'
      IMPORTING
        COLUMN      = COLUMN_1.

    CLEAR: P_TEXT_TABLE.

    IF P_QMNUM IS NOT INITIAL.
      LOOP AT P_QMNUM.
        IF P_QMNUM-OPTION NE 'EQ' AND P_QMNUM-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Nota: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_QMNUM-OPTION EQ 'BT'.
          CONCATENATE 'Nota:' P_QMNUM-LOW '-' P_QMNUM-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Nota: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Nota:' P_QMNUM-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Nota:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_AUFNR IS NOT INITIAL.
      LOOP AT P_AUFNR.
        IF P_AUFNR-OPTION NE 'EQ' AND P_AUFNR-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Ordem: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_AUFNR-OPTION EQ 'BT'.
          CONCATENATE 'Ordem:' P_AUFNR-LOW '-' P_AUFNR-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Ordem: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Ordem:' P_AUFNR-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Ordem:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_EQUNR IS NOT INITIAL.
      LOOP AT P_EQUNR.
        IF P_EQUNR-OPTION NE 'EQ' AND P_EQUNR-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Equipamento: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_EQUNR-OPTION EQ 'BT'.
          CONCATENATE 'Equipamento:' P_EQUNR-LOW '-' P_EQUNR-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Equipamento: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Equipamento:' P_EQUNR-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Equipamento:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_EQART IS NOT INITIAL.
      LOOP AT P_EQART.
        IF P_EQART-OPTION NE 'EQ' AND P_EQART-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Tipo de Objeto: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_EQART-OPTION EQ 'BT'.
          CONCATENATE 'Tipo de Objeto:' P_EQART-LOW '-' P_EQART-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Tipo de Objeto: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Tipo de Objeto:' P_EQART-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Tipo de Objeto:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_BAUTL IS NOT INITIAL.
      LOOP AT P_BAUTL.
        IF P_BAUTL-OPTION NE 'EQ' AND P_BAUTL-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Conjunto: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_BAUTL-OPTION EQ 'BT'.
          CONCATENATE 'Conjunto:' P_BAUTL-LOW '-' P_BAUTL-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Conjunto: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Conjunto:' P_BAUTL-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Conjunto:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_IWERK IS NOT INITIAL.
      LOOP AT P_IWERK.
        IF P_IWERK-OPTION NE 'EQ' AND P_IWERK-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Centro: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_IWERK-OPTION EQ 'BT'.
          CONCATENATE 'Centro:' P_IWERK-LOW '-' P_IWERK-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Centro: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro:' P_IWERK-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Centro:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    IF P_QMDAT IS NOT INITIAL.
      LOOP AT P_QMDAT.
        IF P_QMDAT-OPTION NE 'EQ' AND P_QMDAT-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Período: Múltiplas Seleções'.
          EXIT.
        ELSEIF P_QMDAT-OPTION EQ 'BT'.
          CONCATENATE 'Período:' P_QMDAT-LOW+6(2) '/' P_QMDAT-LOW+4(2) '/' P_QMDAT-LOW(4) '-' P_QMDAT-HIGH+6(2) '/' P_QMDAT-HIGH+4(2) '/' P_QMDAT-HIGH(4) INTO SDYDO_TEXT_ELEMENT.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Período: Múltiplas Seleções'.
          ELSE.
            CONCATENATE 'Período:' P_QMDAT-LOW+6(2) '/' P_QMDAT-LOW+4(2) '/' P_QMDAT-LOW(4) INTO SDYDO_TEXT_ELEMENT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Período:'.
    ENDIF.

    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.

    CALL METHOD COLUMN_1->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

    "------------------

    CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_2.

    DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

    CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = DG_PARENT_2
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

  ELSE.

    GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
    GS_ALV_REFRES_COND-COL = ABAP_TRUE.

    CALL METHOD CTL_ALV->GET_FRONTEND_LAYOUT
      IMPORTING
        ES_LAYOUT = GS_LAYOUT.

    GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.

    CALL METHOD CTL_ALV->SET_FRONTEND_LAYOUT
      EXPORTING
        IS_LAYOUT = GS_LAYOUT.

    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_ALV_REFRES_COND
        "I_SOFT_REFRESH = ABAP_TRUE
      EXCEPTIONS
        FINISHED  = 1
        OTHERS    = 2.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.
  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.
  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.
  WHILE L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.
  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.

ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG USING VALUE(P_COLNUM)
                                VALUE(P_FIELDNAME)
                                VALUE(P_TABNAME)
                                VALUE(P_HOTSPOT)
                                VALUE(P_LZERO)
                                VALUE(P_HEADER).

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS    = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME  = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME    = P_TABNAME.
  WA_FIELDCATALOG-COLTEXT    = P_HEADER.
  WA_FIELDCATALOG-HOTSPOT    = P_HOTSPOT.
  WA_FIELDCATALOG-LZERO      = P_LZERO.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .
  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.
ENDFORM.                    " FILL_GS_VARIANT
