*&---------------------------------------------------------------------*
*&  Include           ZPMR0018_0100
*&---------------------------------------------------------------------*

DATA:
  "IT_SELECT           TYPE STANDARD TABLE OF TY_DADOS_IMOB,
  G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
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
  DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER,
  IT_EXCLUDE_FCODE   TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE   LIKE LINE OF IT_EXCLUDE_FCODE,
  GS_LAYOUT          TYPE LVC_S_LAYO,
  GS_VARIANT         TYPE DISVARIANT,
  IT_FIELDCATALOG    TYPE LVC_T_FCAT,
  WA_FIELDCATALOG    TYPE LVC_S_FCAT,
  IT_SORT            TYPE LVC_T_SORT,
  "GS_SCROLL_COL       TYPE LVC_S_COL,
  "GS_SCROLL_ROW       TYPE LVC_S_ROID,
  "GS_STABLE           TYPE LVC_S_STBL,
  "IT_SELECTED_ROWS    TYPE LVC_T_ROW,
  "WA_SELECTED_ROWS    TYPE LVC_S_ROW,
  LS_STABLE          TYPE LVC_S_STBL.

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
        VL_CONT                 TYPE I,
        VL_BUTXT                TYPE T001-BUTXT,
        VL_DATES1               TYPE CHAR10,
        VL_DATES2               TYPE CHAR10.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

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
        HEIGHT = 16.

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

    IF P_ANALI IS NOT INITIAL.
      P_TEXT = TEXT-007.
      IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
        PERFORM FILL_IT_FIELDCATALOG USING:
              01 'BUKRS'      'VIQMEL'    '07'  ' '     ' '    ' '   'Empresa',
              02 'SWERK'      'VIQMEL'    '06'  ' '     ' '    ' '   'Filial',
              03 'TPLMA'      'IFLO'      '30'  ' '     ' '    ' '   'Local Superior',
              04 'PLTX2'      'IFLO'      '40'  ' '     ' '    ' '   'Desc. Local Superior',
              05 'TPLNR'      'VIQMEL'    '18'  ' '     ' '    ' '   'Local',
              06 'PLTXT'      'ITOB'      '40'  ' '     ' '    ' '   'Desc. Local',
              07 'OPERA'      ''          '16'  ' '     ' '    'X'   'Tpo Oper. (Min)',
              08 'DISPO'      ''          '16'  ' '     ' '    'X'   'Tpo Dispon. (Min)',
              09 'PARAD'      ''          '16'  ' '     ' '    'X'   'Tpo Parada (Min)',
              10 '%DISP'      ''          '16'  ' '     ' '    'X'   'Dispon. (%)'.

        PERFORM FILL_IT_SORT.
      ELSE.
        PERFORM FILL_IT_FIELDCATALOG USING:
            01 'BUKRS'      'VIQMEL'    '07'  ' '     ' '    ' '   'Empresa',
            02 'SWERK'      'VIQMEL'    '06'  ' '     ' '    ' '   'Filial',
            "03 'TPLNR'      'IFLO'      '18'  ' '     ' '    ' '   'Local',
            04 'PLTXT'      'IFLO'      '40'  ' '     ' '    ' '   'Desc. Local',
            05 'EARTX'      'V_EQUI'    '20'  ' '     ' '    ' '   'Tipo de Objeto',
            06 'EQUNR'      'V_EQUI'    '18'  ' '     ' '    ' '   'Equipamento',
            07 'EQKTX'      'V_EQUI'    '10'  ' '     ' '    ' '   'Desc. Equipamento',
            08 'OPERA'      ''          '16'  ' '     ' '    'X'   'Tpo Oper. (Min)',
            09 'DISPO'      ''          '16'  ' '     ' '    'X'   'Tpo Dispon. (Min)',
            10 'PARAD'      ''          '16'  ' '     ' '    'X'   'Tpo Parada (Min)',
            11 '%DISP'      ''          '16'  ' '     ' '    'X'   'Dispon. (%)'.

        PERFORM FILL_IT_SORT.
      ENDIF.
    ELSE.
      P_TEXT = TEXT-006.
      IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
        PERFORM FILL_IT_FIELDCATALOG USING:
              01 'BUKRS'      'VIQMEL'    '07'  ' '     ' '    ' '   'Empresa',
              02 'SWERK'      'VIQMEL'    '06'  ' '     ' '    ' '   'Filial',
              03 'TPLMA'      'IFLO'      '30'  ' '     ' '    ' '   'Local Superior',
              04 'PLTX2'      'IFLO'      '40'  ' '     ' '    ' '   'Desc. Local Superior',
*            05 'EQUNR'      'VIQMEL'    '18'  ' '     ' '    ' '   'Equipamento',
*            06 'SHTXT'      'ITOB'      '40'  ' '     ' '    ' '   'Desc. Equipamento',
              07 'OPERA'      ''          '16'  ' '     ' '    'X'   'Tpo Operação (Min)',
              08 'DISPO'      ''          '16'  ' '     ' '    'X'   'Tpo Disponível (Min)',
              09 'PARAD'      ''          '16'  ' '     ' '    'X'   'Tpo Parada (Min)',
              10 '%DISP'      ''          '16'  ' '     ' '    'X'   'Disponibilidade (%)'.
*      ELSEIF P_EQUNR IS NOT INITIAL.
*        PERFORM FILL_IT_FIELDCATALOG USING:
*           01 'BUKRS'      'VIQMEL'    '07'  ' '     ' '    ' '   'Empresa',
*           02 'SWERK'      'VIQMEL'    '06'  ' '     ' '    ' '   'Filial',
*           03 'TPLNR'      'IFLO'      '30'  ' '     ' '    ' '   'Local',
*           04 'PLTXT'      'IFLO'      '40'  ' '     ' '    ' '   'Desc. Local',
*           "05 'EARTX'      'V_EQUI'    '18'  ' '     ' '    ' '   'Tipo de Objeto',
*           06 'EQUNR'      'V_EQUI'    '18'  ' '     ' '    ' '   'Equipamento',
*           07 'EQKTX'      'V_EQUI'    '10'  ' '     ' '    ' '   'Desc. Equipamento',
*           08 'OPERA'      ''          '16'  ' '     ' '    'X'   'Tpo Operação (Min)',
*           09 'DISPO'      ''          '16'  ' '     ' '    'X'   'Tpo Disponível (Min)',
*           10 'PARAD'      ''          '16'  ' '     ' '    'X'   'Tpo Parada (Min)',
*           11 '%DISP'      ''          '16'  ' '     ' '    'X'   'Disponibilidade (%)'.
      ELSE.
        PERFORM FILL_IT_FIELDCATALOG USING:
             01 'BUKRS'      'VIQMEL'    '07'  ' '     ' '    ' '   'Empresa',
             02 'SWERK'      'VIQMEL'    '06'  ' '     ' '    ' '   'Filial',
             03 'TPLNR'      'IFLO'      '30'  ' '     ' '    ' '   'Local',
             04 'PLTXT'      'IFLO'      '40'  ' '     ' '    ' '   'Desc. Local',
             05 'EARTX'      'V_EQUI'    '18'  ' '     ' '    ' '   'Tipo de Objeto',
             "06 'EQUNR'      'V_EQUI'    '18'  ' '     ' '    ' '   'Equipamento',
             "07 'EQKTX'      'V_EQUI'    '10'  ' '     ' '    ' '   'Desc. Equipamento',
             08 'OPERA'      ''          '16'  ' '     ' '    'X'   'Tpo Operação (Min)',
             09 'DISPO'      ''          '16'  ' '     ' '    'X'   'Tpo Disponível (Min)',
             10 'PARAD'      ''          '16'  ' '     ' '    'X'   'Tpo Parada (Min)',
             11 '%DISP'      ''          '16'  ' '     ' '    'X'   'Disponibilidade (%)'.
      ENDIF.
    ENDIF.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-CWIDTH_OPT = 'X'.
    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    "PERFORM EXCLUDE.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_SAIDA
        IT_SORT              = IT_SORT.

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
    "------------------
    LOOP AT P_BUKRS.
      IF P_BUKRS-OPTION NE 'EQ' AND P_BUKRS-OPTION NE 'BT'.
        SDYDO_TEXT_ELEMENT = 'Empresa: Multiplas Seleções'.
        EXIT.
      ELSEIF P_BUKRS-OPTION EQ 'BT'.

        SELECT SINGLE BUTXT
          FROM T001
          INTO VL_BUTXT
          WHERE BUKRS EQ P_BUKRS-LOW
          AND SPRAS EQ SY-LANGU.

        CONCATENATE 'Empresa:' P_BUKRS-LOW VL_BUTXT '-' INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        CLEAR: VL_BUTXT.

        SELECT SINGLE BUTXT
         FROM T001
         INTO VL_BUTXT
         WHERE BUKRS EQ P_BUKRS-HIGH
         AND SPRAS EQ SY-LANGU.

        CONCATENATE SDYDO_TEXT_ELEMENT P_BUKRS-HIGH VL_BUTXT INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.

        EXIT.
      ELSE.
        VL_CONT = VL_CONT + 1.
        IF VL_CONT GT 1.
          SDYDO_TEXT_ELEMENT = 'Empresa: Multiplas Seleções'.
        ELSE.

          SELECT SINGLE BUTXT
            FROM T001
            INTO VL_BUTXT
            WHERE BUKRS EQ P_BUKRS-LOW
            AND SPRAS EQ SY-LANGU.

          CONCATENATE 'Empresa:' P_BUKRS-LOW VL_BUTXT INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.

        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, VL_BUTXT, SDYDO_TEXT_ELEMENT.
    "------------------
    IF P_SWERK IS NOT INITIAL.
      LOOP AT P_SWERK.
        IF P_SWERK-OPTION NE 'EQ' AND P_SWERK-OPTION NE 'BT'.
          SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
          EXIT.
        ELSEIF P_SWERK-OPTION EQ 'BT'.
          CONCATENATE 'Centro:' P_SWERK-LOW '-' P_SWERK-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          VL_CONT = VL_CONT + 1.
          IF VL_CONT GT 1.
            SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro:' P_SWERK-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    ELSE.
      SDYDO_TEXT_ELEMENT = 'Centro:'.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    ENDIF.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
    "------------------
    LOOP AT P_DATES.
      IF P_DATES-OPTION EQ 'BT'.
        CONCATENATE P_DATES-LOW+6(2) '.' P_DATES-LOW+4(2) '.' P_DATES-LOW(4) INTO VL_DATES1.
        CONCATENATE P_DATES-HIGH+6(2) '.' P_DATES-HIGH+4(2) '.' P_DATES-HIGH(4) INTO VL_DATES2.
        CONCATENATE 'Período:' VL_DATES1 '-' VL_DATES2 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        EXIT.
      ELSE.
        CONCATENATE P_DATES-LOW+6(2) '.' P_DATES-LOW+4(2) '.' P_DATES-LOW(4) INTO VL_DATES1.
        CONCATENATE 'Período:' VL_DATES1 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
      ENDIF.
    ENDLOOP.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT, VL_DATES1, VL_DATES2.
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

    PERFORM AJUSTA_TOTAIS.

  ELSE.

    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.

    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = LS_STABLE
      EXCEPTIONS
        FINISHED  = 1
        OTHERS    = 2.

    IF SY-SUBRC <> 0.
    ENDIF.

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

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE .

*   Excluir Buttons Toolbar
  FREE: IT_EXCLUDE_FCODE.

  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG USING VALUE(P_COLNUM)
                                VALUE(P_FIELDNAME)
                                VALUE(P_TABNAME)
                                VALUE(P_LEN)
                                VALUE(P_EDIT)
                                VALUE(P_ICON)
                                VALUE(P_DO_SUM)
                                VALUE(P_HEADER).

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS     = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME     = P_TABNAME.
  WA_FIELDCATALOG-OUTPUTLEN   = P_LEN.
  WA_FIELDCATALOG-COLTEXT     = P_HEADER.
  WA_FIELDCATALOG-EDIT        = P_EDIT.
  WA_FIELDCATALOG-ICON        = P_ICON.
  WA_FIELDCATALOG-REF_TABLE   = P_TABNAME.
  WA_FIELDCATALOG-CHECKTABLE  = P_TABNAME.
  WA_FIELDCATALOG-DO_SUM  = P_DO_SUM.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_SORT .

  DATA: WA_SORT TYPE LVC_S_SORT.

  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'SWERK'.
  "WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.

    WA_SORT-SPOS = '2'.
    WA_SORT-FIELDNAME = 'TPLMA'.
    "WA_SORT-DOWN = 'X'.
    WA_SORT-GROUP = '*'.
    WA_SORT-SUBTOT = 'X'.
    APPEND WA_SORT TO IT_SORT.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TOTAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AJUSTA_TOTAIS .

  DATA: LREF_DATA TYPE REF TO DATA.
  DATA: VL_CONTSUM TYPE I.
  FIELD-SYMBOLS: <L_SUM_TAB> TYPE TABLE,
                 <L_SUM>     TYPE TY_SAIDA.

  IF P_SINTE IS NOT INITIAL.
    "Ajuste de valores da linha de TOTAIS
    CALL METHOD CTL_ALV->GET_SUBTOTALS
      IMPORTING
        EP_COLLECT00 = LREF_DATA.

    ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
    IF <L_SUM_TAB> IS ASSIGNED.
      READ TABLE <L_SUM_TAB> ASSIGNING <L_SUM> INDEX 1.
      IF SY-SUBRC EQ 0.
        CLEAR: <L_SUM>.
        LOOP AT IT_SAIDA INTO WA_SAIDA.
          VL_CONTSUM = VL_CONTSUM + 1.
          <L_SUM>-OPERA = WA_SAIDA-OPERA.
          <L_SUM>-DISPO = <L_SUM>-DISPO + WA_SAIDA-DISPO.
          <L_SUM>-PARAD = <L_SUM>-PARAD + WA_SAIDA-PARAD.
          <L_SUM>-%DISP = <L_SUM>-%DISP + WA_SAIDA-%DISP.
        ENDLOOP.
        <L_SUM>-DISPO = <L_SUM>-DISPO / VL_CONTSUM.
        <L_SUM>-PARAD = <L_SUM>-PARAD / VL_CONTSUM.
        <L_SUM>-%DISP = <L_SUM>-%DISP / VL_CONTSUM.
        CLEAR: VL_CONTSUM.

        CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.

      ENDIF.
    ENDIF.
  ELSE.
    "Ajuste de valores da linha de TOTAIS
    CALL METHOD CTL_ALV->GET_SUBTOTALS
      IMPORTING
        EP_COLLECT00 = LREF_DATA.

    ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
    IF <L_SUM_TAB> IS ASSIGNED.
      READ TABLE <L_SUM_TAB> ASSIGNING <L_SUM> INDEX 1.
      IF SY-SUBRC EQ 0.
        CLEAR: <L_SUM>.
        LOOP AT IT_SAIDA INTO WA_SAIDA.
          VL_CONTSUM = VL_CONTSUM + 1.
          <L_SUM>-OPERA = WA_SAIDA-OPERA.
          <L_SUM>-DISPO = <L_SUM>-DISPO + WA_SAIDA-DISPO.
          <L_SUM>-PARAD = <L_SUM>-PARAD + WA_SAIDA-PARAD.
          <L_SUM>-%DISP = <L_SUM>-%DISP + WA_SAIDA-%DISP.
        ENDLOOP.
        <L_SUM>-DISPO = <L_SUM>-DISPO / VL_CONTSUM.
        <L_SUM>-PARAD = <L_SUM>-PARAD / VL_CONTSUM.
        <L_SUM>-%DISP = <L_SUM>-%DISP / VL_CONTSUM.
        CLEAR: VL_CONTSUM.

        CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.

      ENDIF.
    ENDIF.

    CLEAR: LREF_DATA.
    "Ajuste de valores da linha de SUB-TOTAIS Nível 01
    CALL METHOD CTL_ALV->GET_SUBTOTALS
      IMPORTING
        EP_COLLECT01 = LREF_DATA.

    ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
    IF <L_SUM_TAB> IS ASSIGNED.
      LOOP AT <L_SUM_TAB> ASSIGNING <L_SUM>.
        IF <L_SUM> IS ASSIGNED.
          CLEAR: <L_SUM>-DISPO, <L_SUM>-PARAD, <L_SUM>-%DISP.
          LOOP AT IT_SAIDA INTO WA_SAIDA
            WHERE SWERK EQ <L_SUM>-SWERK.
            VL_CONTSUM = VL_CONTSUM + 1.
            <L_SUM>-OPERA = WA_SAIDA-OPERA.
            <L_SUM>-DISPO = <L_SUM>-DISPO + WA_SAIDA-DISPO.
            <L_SUM>-PARAD = <L_SUM>-PARAD + WA_SAIDA-PARAD.
            <L_SUM>-%DISP = <L_SUM>-%DISP + WA_SAIDA-%DISP.
          ENDLOOP.
          <L_SUM>-DISPO = <L_SUM>-DISPO / VL_CONTSUM.
          <L_SUM>-PARAD = <L_SUM>-PARAD / VL_CONTSUM.
          <L_SUM>-%DISP = <L_SUM>-%DISP / VL_CONTSUM.
          CLEAR: VL_CONTSUM.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: LREF_DATA.
    "Ajuste de valores da linha de SUB-TOTAIS Nível 02
    CALL METHOD CTL_ALV->GET_SUBTOTALS
      IMPORTING
        EP_COLLECT02 = LREF_DATA.

    ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
    IF <L_SUM_TAB> IS ASSIGNED.
      LOOP AT <L_SUM_TAB> ASSIGNING <L_SUM>.
        IF <L_SUM> IS ASSIGNED.
          CLEAR: <L_SUM>-DISPO, <L_SUM>-PARAD, <L_SUM>-%DISP.
          LOOP AT IT_SAIDA INTO WA_SAIDA
            WHERE SWERK EQ <L_SUM>-SWERK
            AND TPLMA EQ <L_SUM>-TPLMA.
            VL_CONTSUM = VL_CONTSUM + 1.
            <L_SUM>-OPERA = WA_SAIDA-OPERA.
            <L_SUM>-DISPO = <L_SUM>-DISPO + WA_SAIDA-DISPO.
            <L_SUM>-PARAD = <L_SUM>-PARAD + WA_SAIDA-PARAD.
            <L_SUM>-%DISP = <L_SUM>-%DISP + WA_SAIDA-%DISP.
          ENDLOOP.
          <L_SUM>-DISPO = <L_SUM>-DISPO / VL_CONTSUM.
          <L_SUM>-PARAD = <L_SUM>-PARAD / VL_CONTSUM.
          <L_SUM>-%DISP = <L_SUM>-%DISP / VL_CONTSUM.
          CLEAR: VL_CONTSUM.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  CALL METHOD CTL_ALV->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = GS_LAYOUT.

  GS_LAYOUT-CWIDTH_OPT = 'X'.

  CALL METHOD CTL_ALV->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = 'X'.

ENDFORM.
