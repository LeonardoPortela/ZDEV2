*----------------------------------------------------------------------*
***INCLUDE ZGLR068_IMPRIME_ALVO01.
*----------------------------------------------------------------------*

  DATA: IT_FIELDCAT TYPE LVC_T_FCAT,
        WA_FCAT     TYPE LVC_S_FCAT.

  DATA IT_LAYOUT TYPE LVC_S_LAYO.

*&---------------------------------------------------------------------*
*&      Module  IMPRIME_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE IMPRIME_ALV OUTPUT.

    DATA O_GRID TYPE REF TO CL_GUI_ALV_GRID .
    DATA O_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER .
    DATA: O_EVENT TYPE REF TO LCL_EVENT_RECEIVER .



    IF O_GRID IS INITIAL .

      CREATE OBJECT O_CONTAINER
        EXPORTING
          CONTAINER_NAME = 'CC_ALV'.

      DATA: O_SPLITTER    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
            O_CONTAINER_1 TYPE REF TO CL_GUI_CONTAINER,
            O_CONTAINER_2 TYPE REF TO CL_GUI_CONTAINER.

      "Divide o container
      CREATE OBJECT O_SPLITTER
        EXPORTING
          PARENT  = O_CONTAINER
          ROWS    = 2
          COLUMNS = 1.

      "Seta o primeiro container
      CALL METHOD O_SPLITTER->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 1
        RECEIVING
          CONTAINER = O_CONTAINER_1.

      "Seta o segundo container
      CALL METHOD O_SPLITTER->GET_CONTAINER
        EXPORTING
          ROW       = 2
          COLUMN    = 1
        RECEIVING
          CONTAINER = O_CONTAINER_2.

      "Altura do primeiro container
      CALL METHOD O_SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID     = 1
          HEIGHT = 22.

      PERFORM HEADER.

      CREATE OBJECT O_GRID
        EXPORTING
          I_PARENT = O_CONTAINER_2.

      PERFORM FIELD_CATALOG CHANGING IT_FIELDCAT.
      PERFORM FIELDCAT_LAYOUT CHANGING IT_LAYOUT.

      CREATE OBJECT O_EVENT.
      SET HANDLER O_EVENT->ON_HOTSPOT_CLICK FOR O_GRID.

      CALL METHOD O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT       = IT_LAYOUT
        CHANGING
          IT_OUTTAB       = IT_SAIDA_ALV
          IT_FIELDCATALOG = IT_FIELDCAT.

    ELSE.

      CALL METHOD O_GRID->REFRESH_TABLE_DISPLAY.

    ENDIF.

  ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM FIELD_CATALOG CHANGING IT_FIELDCAT TYPE LVC_T_FCAT.

    DATA: COL_POS TYPE I.

    COL_POS = COL_POS + 1.
    WA_FCAT-FIELDNAME = 'VBUND' .
    WA_FCAT-COL_POS = 1.
    WA_FCAT-COLTEXT = 'Sociedade Parceira ' .
    WA_FCAT-SELTEXT = 'Sociedade Parceira ' .
    APPEND WA_FCAT TO IT_FIELDCAT .
    CLEAR WA_FCAT.

    LOOP AT IT_EMPRESAS INTO WA_EMPRESAS.
      COL_POS = COL_POS + 1.
      WA_FCAT-FIELDNAME = WA_EMPRESAS-NUM.
      WA_FCAT-COL_POS = COL_POS.
      WA_FCAT-COLTEXT = WA_EMPRESAS-ID.
      WA_FCAT-SELTEXT = WA_EMPRESAS-ID.
      WA_FCAT-DATATYPE = 'CURR'.
      WA_FCAT-INTLEN = 13.
      WA_FCAT-DO_SUM = 'X'.
      WA_FCAT-HOTSPOT = 'X'.
      APPEND WA_FCAT TO IT_FIELDCAT.
      CLEAR WA_FCAT.
    ENDLOOP.

    COL_POS = COL_POS + 1.
    WA_FCAT-FIELDNAME = 'VAL51'.
    WA_FCAT-COL_POS = COL_POS.
    WA_FCAT-COLTEXT = 'Total'.
    WA_FCAT-SELTEXT = 'Total'.
    WA_FCAT-DATATYPE = 'CURR'.
    WA_FCAT-INTLEN = 13.
    WA_FCAT-DO_SUM = 'X'.
    WA_FCAT-STYLE = '00000120'.
    "WA_FCAT-EMPHASIZE = 'C310'.
    APPEND WA_FCAT TO IT_FIELDCAT.
    CLEAR WA_FCAT.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM FIELDCAT_LAYOUT CHANGING IT_LAYOUT TYPE LVC_S_LAYO.

    "IT_LAYOUT-ZEBRA = 'X' .
    IT_LAYOUT-CWIDTH_OPT = 'X'.
    IT_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE USER_COMMAND_1001 INPUT.
    CASE SY-UCOMM.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'CANCEL'.
        LEAVE TO SCREEN 0.
      WHEN 'EXIT'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE STATUS_1001 OUTPUT.
    SET PF-STATUS 'ZSTATUS'.
    SET TITLEBAR '001'.
  ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM HEADER.

    DATA: L_DOCUMENT TYPE REF TO CL_DD_DOCUMENT,
          L_DOCTABLE TYPE REF TO CL_DD_TABLE_ELEMENT,
          L_COLUMN1  TYPE REF TO CL_DD_AREA,
          L_COLUMN2  TYPE REF TO CL_DD_AREA.

    CREATE OBJECT L_DOCUMENT.
    CALL METHOD L_DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT         = TEXT-002
        SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG.

    CALL METHOD L_DOCUMENT->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS               = 1
        CELL_BACKGROUND_TRANSPARENT = 'X'
        BORDER                      = '0'
      IMPORTING
        TABLE                       = L_DOCTABLE.

    CALL METHOD L_DOCTABLE->ADD_COLUMN
      IMPORTING
        COLUMN = L_COLUMN1.

    PERFORM TITLES CHANGING L_COLUMN1.

    CALL METHOD L_DOCUMENT->MERGE_DOCUMENT.
    CALL METHOD L_DOCUMENT->DISPLAY_DOCUMENT
      EXPORTING
        PARENT = O_CONTAINER_1.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TITLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM TITLES CHANGING DG_DYNDOC_ID TYPE REF TO CL_DD_AREA.

    DATA : DL_TEXT(255) TYPE C.  "Text

    CONCATENATE 'Empresa:' DL_TEXT_EMPR INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Pares Eliminação:' DL_TEXT_PARC INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

*    CONCATENATE 'Mês/Ano:' DL_TEXT_DATA INTO DL_TEXT SEPARATED BY SPACE.
*    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Período:' DL_TEXT_MES INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Exercício:' DL_TEXT_ANO INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Moeda:' DL_TEXT_MOED INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Data Lançamento:' DL_TEXT_DATA INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

    CONCATENATE 'Data/Hora Geração:' DL_TEXT_DTGERA INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT CHANGING DG_DYNDOC_ID DL_TEXT.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM ADD_TEXT CHANGING DG_DYNDOC_ID TYPE REF TO CL_DD_AREA
                         DL_TEXT      TYPE C .

    CONDENSE DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = DL_TEXT
        SAP_FONTSIZE = CL_DD_AREA=>LARGE.

    CALL METHOD DG_DYNDOC_ID->NEW_LINE.
    CLEAR : DL_TEXT.

  ENDFORM.
