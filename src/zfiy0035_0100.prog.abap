*&---------------------------------------------------------------------*
*&  Include           ZFIY0035_0100
*&---------------------------------------------------------------------*

DATA:  G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
       DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
       DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
       DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
       PICTURE            TYPE REF TO CL_GUI_PICTURE,
       CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
       DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
       TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
       COLUMN             TYPE REF TO CL_DD_AREA,
       TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
       COLUMN_1           TYPE REF TO CL_DD_AREA,
       COLUMN_2           TYPE REF TO CL_DD_AREA,
       DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER,
       GS_LAYOUT          TYPE LVC_S_LAYO,
       GS_VARIANT         TYPE DISVARIANT,
       IT_FIELDCATALOG    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: URL(255)                TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
        VL_CONT                 TYPE I,
        VL_GTEXT                TYPE TGSBT-GTEXT,
        VL_LANDX                TYPE T005T-LANDX.

  SET PF-STATUS 'ZFIY0035_STATUS'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER0100'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    "--- Containers

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
        HEIGHT = 13.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    "--- Imagem

    PERFORM F_PEGA_IMAGEM USING    'LOGO_NOVO'
                          CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    "--- Fieldcat

    PERFORM FILL_IT_FIELDCATALOG USING:
          01 'LAND1'      ''    '07'  ' '     'X'     ' '   'C'   ' '   TEXT-003,
          02 'WITHT'      ''    '07'  ' '     'X'     ' '   'C'   ' '   TEXT-004,
          03 'TEXT402'    ''    '12'  ' '     ' '     ' '   ' '   ' '   TEXT-005,
          04 'WT_WITHCD'  ''    '06'  ' '     ' '     ' '   ' '   ' '   TEXT-006,
          05 'TEXT40'     ''    '07'  ' '     ' '     ' '   ' '   ' '   TEXT-007,
          06 'KONTS'      ''    '06'  ' '     ' '     ' '   ' '   ' '   TEXT-015,
          07 'QSCOD'      ''    '06'  ' '     ' '     ' '   ' '   ' '   TEXT-008,
          08 'QPROZ'      ''    '12'  ' '     ' '     ' '   ' '   ' '   TEXT-009,
          09 'QSATZ'      ''    '12'  ' '     ' '     ' '   ' '   ' '   TEXT-010,
          10 'WT_WTMIN'   ''    '50'  ' '     ' '     ' '   ' '   ' '   TEXT-011,
          11 'WT_WTMAX'   ''    '50'  ' '     ' '     ' '   ' '   ' '   TEXT-012,
          12 'WT_WTMINB'  ''    '25'  ' '     ' '     ' '   ' '   ' '   TEXT-013,
          13 'WT_WTBEX'   ''    '18'  ' '     ' '     ' '   ' '   ' '   TEXT-014.

*   Fill info for layout variant
*    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-CWIDTH_OPT = 'X'.
    GS_LAYOUT-SEL_MODE   = 'A'.
*    GS_LAYOUT-STYLEFNAME = 'CELLSTYLES'.
*    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
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
        "SAP_ALIGN = 'CENTER'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

    P_TEXT = TEXT-016.

    CALL METHOD COLUMN->ADD_TEXT
      EXPORTING
        TEXT      = P_TEXT
        SAP_STYLE = 'HEADING'.

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

    "---cabeçalho
    IF P_LAND1 IS NOT INITIAL.
      LOOP AT P_LAND1.
        IF P_LAND1-OPTION NE 'EQ' AND P_LAND1-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = TEXT-017.
          EXIT.
        ELSEIF P_LAND1-OPTION EQ 'BT'.
          SELECT SINGLE LANDX
            FROM T005T
            INTO VL_LANDX
            WHERE LAND1 EQ P_LAND1-LOW
              AND SPRAS EQ SY-LANGU.

          CONCATENATE TEXT-018 P_LAND1-LOW VL_LANDX '-' INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          CLEAR: VL_LANDX.

          SELECT SINGLE LANDX
            FROM T005T
            INTO VL_LANDX
            WHERE LAND1 EQ P_LAND1-LOW
              AND SPRAS EQ SY-LANGU.

          CONCATENATE SDYDO_TEXT_ELEMENT P_LAND1-HIGH VL_LANDX INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = TEXT-017.
          ELSE.
            SELECT SINGLE LANDX
              FROM T005T
              INTO VL_LANDX
              WHERE LAND1 EQ P_LAND1-LOW
                AND SPRAS EQ SY-LANGU.

            CONCATENATE TEXT-018 P_LAND1-LOW VL_LANDX INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, VL_LANDX, SDYDO_TEXT_ELEMENT.
    ELSE.
      SDYDO_TEXT_ELEMENT = TEXT-018.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ENDIF.

    "------------------
    IF P_WITHT IS NOT INITIAL.
      LOOP AT P_WITHT.
        IF P_WITHT-OPTION NE 'EQ' AND P_WITHT-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = TEXT-019.
          EXIT.
        ELSEIF P_WITHT-OPTION EQ 'BT'.
          CONCATENATE TEXT-020 P_WITHT-LOW '-' P_WITHT-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = TEXT-019.
          ELSE.
            CONCATENATE TEXT-020 P_WITHT-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ELSE.
      SDYDO_TEXT_ELEMENT = TEXT-020.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ENDIF.
    "------------------

    IF P_WITHCD IS NOT INITIAL.
      LOOP AT P_WITHCD.
        IF P_WITHCD-OPTION NE 'EQ' AND P_WITHCD-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = TEXT-021.
          EXIT.
        ELSEIF P_WITHCD-OPTION EQ 'BT'.
          CONCATENATE TEXT-022 P_WITHCD-LOW '-' P_WITHCD-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = TEXT-021.
          ELSE.
            CONCATENATE TEXT-021 P_WITHCD-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ELSE.
      SDYDO_TEXT_ELEMENT = TEXT-022.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ENDIF.

    "------------------

    CALL METHOD COLUMN_1->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

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
    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
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
                                VALUE(P_LEN)
                                VALUE(P_EDIT)
                                VALUE(P_ICON)
                                VALUE(P_LZERO)
                                VALUE(P_JUST)
                                VALUE(P_CHECKBOX)
                                VALUE(P_HEADER).

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS    = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME  = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME    = P_TABNAME.
  WA_FIELDCATALOG-OUTPUTLEN  = P_LEN.
  WA_FIELDCATALOG-COLTEXT    = P_HEADER.
  WA_FIELDCATALOG-EDIT       = P_EDIT.
  WA_FIELDCATALOG-ICON       = P_ICON.
  WA_FIELDCATALOG-REF_TABLE  = P_TABNAME.
  WA_FIELDCATALOG-CHECKTABLE = P_TABNAME.
  WA_FIELDCATALOG-LZERO      = P_LZERO.
  WA_FIELDCATALOG-JUST       = P_JUST.
  WA_FIELDCATALOG-CHECKBOX   = P_CHECKBOX.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
