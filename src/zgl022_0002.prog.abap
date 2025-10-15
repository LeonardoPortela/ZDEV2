
***INCLUDE ZGL022_0002 .
*----------------------------------------------------------------------*

DATA: DG_DYNDOC_ID_0002  TYPE REF TO CL_DD_DOCUMENT.


*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER_0002 DEFINITION.
  PUBLIC SECTION.
    METHODS TOP_OF_PAGE
      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
CLASS LCL_EVENT_HANDLER_0002 IMPLEMENTATION.
  METHOD TOP_OF_PAGE.
    PERFORM EVENT_TOP_OF_PAGE_0002 USING DG_DYNDOC_ID_0002.
  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
* DATA DEFINITION ALV
*&---------------------------------------------------------------------*
DATA: GF_FIRST_DISPLAY_0002 TYPE C VALUE 'X',
      EVENT_HANDLER_0002    TYPE REF TO LCL_EVENT_HANDLER_0002,
      CTL_CCCONTAINER_0002  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_0002      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_HTML_CNTRL_0002    TYPE REF TO CL_GUI_HTML_VIEWER,
      DG_PARENT_HTML_0002   TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_GRID_0002   TYPE REF TO CL_GUI_CONTAINER,
      GS_SCROLL_COL_0002    TYPE LVC_S_COL,
      GS_SCROLL_ROW_0002    TYPE LVC_S_ROID,
      CTL_ALV_PATNER        TYPE REF TO CL_GUI_ALV_GRID.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.
  PERFORM LIMPAR_CONTROLES_0002.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.

  SET PF-STATUS 'PF0002'.

  CASE WA_RESUMO-TIPO.
    WHEN 'K'. "K  Fornecedores
      SET TITLEBAR 'TLFORNECEDOR'.
    WHEN 'D'. "D  Clientes
      SET TITLEBAR 'TLCLIENTE'.
  ENDCASE.

ENDMODULE.                 " STATUS_0002  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECTS_0002 OUTPUT.

  IF GF_FIRST_DISPLAY_0002 = 'X'.

*   Create object for container
    CREATE OBJECT CTL_CCCONTAINER_0002
      EXPORTING
        CONTAINER_NAME = 'ALV_0002'.

    IF DG_DYNDOC_ID_0002 IS INITIAL.
      CREATE OBJECT DG_DYNDOC_ID_0002
        EXPORTING STYLE = 'ALV_GRID'.
    ENDIF.

    CREATE OBJECT DG_SPLITTER_0002
       EXPORTING PARENT  = CTL_CCCONTAINER_0002
                 ROWS    = 2
                 COLUMNS = 1.

    CALL METHOD DG_SPLITTER_0002->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML_0002.

    CALL METHOD DG_SPLITTER_0002->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_GRID_0002.

    CALL METHOD DG_SPLITTER_0002->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 20.

*   Create object for ALV grid inside container
    CREATE OBJECT CTL_ALV_PATNER
      EXPORTING
        I_PARENT = DG_PARENT_GRID_0002.

*   Fill field catalog
    PERFORM FILL_IT_FIELDCATALOG_0002.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    "GS_LAYOUT-SEL_MODE = 'A'.
    CLEAR: GS_LAYOUT.
    GS_LAYOUT-ZEBRA    = 'X'.

*   Send data to ALV grid
    CALL METHOD CTL_ALV_PATNER->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_PARTNER_ALV.

    CREATE OBJECT EVENT_HANDLER_0002.
    SET HANDLER EVENT_HANDLER_0002->TOP_OF_PAGE FOR CTL_ALV_PATNER.

    CALL METHOD DG_DYNDOC_ID_0002->INITIALIZE_DOCUMENT.

    CALL METHOD CTL_ALV_PATNER->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = DG_DYNDOC_ID_0002.

    CLEAR: GF_FIRST_DISPLAY_0002.

  ENDIF.

  CALL METHOD CTL_ALV_PATNER->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_PATNER->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = GS_SCROLL_COL_0002
      IS_ROW_NO   = GS_SCROLL_ROW_0002.

ENDMODULE.                 " CREATE_OBJECTS_0002  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0002 .

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT,
        LC_POS          TYPE LVC_COLPOS.

  CLEAR: IT_FIELDCATALOG.
  LC_POS = 0.

  ADD 1 TO LC_POS.
  CLEAR WA_FIELDCATALOG.
  WA_FIELDCATALOG-FIELDNAME = 'PARTNER'.
  WA_FIELDCATALOG-COL_POS   = LC_POS.
  WA_FIELDCATALOG-REPTEXT   = 'Parceiro'.
  WA_FIELDCATALOG-OUTPUTLEN = 11.
  WA_FIELDCATALOG-CONVEXIT  = 'ALPHA'.
  WA_FIELDCATALOG-JUST      = 'R'.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  ADD 1 TO LC_POS.
  CLEAR WA_FIELDCATALOG.
  WA_FIELDCATALOG-FIELDNAME = 'NOME'.
  WA_FIELDCATALOG-COL_POS   = LC_POS.
  WA_FIELDCATALOG-REPTEXT   = 'Nome'.
  WA_FIELDCATALOG-OUTPUTLEN = 42.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  ADD 1 TO LC_POS.
  CLEAR WA_FIELDCATALOG.
  WA_FIELDCATALOG-FIELDNAME = 'SALDO'.
  WA_FIELDCATALOG-COL_POS   = LC_POS.
  WA_FIELDCATALOG-REPTEXT   = 'Vl. Saldo'.
  WA_FIELDCATALOG-OUTPUTLEN = 17.
  WA_FIELDCATALOG-DO_SUM    = 'X'.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  ADD 1 TO LC_POS.
  CLEAR WA_FIELDCATALOG.
  WA_FIELDCATALOG-FIELDNAME = 'CONTA_CTR'.
  WA_FIELDCATALOG-COL_POS   = LC_POS.
  WA_FIELDCATALOG-REPTEXT   = 'Conta Conciliação'.
  WA_FIELDCATALOG-OUTPUTLEN = 11.
  WA_FIELDCATALOG-CONVEXIT  = 'ALPHA'.
  WA_FIELDCATALOG-JUST      = 'R'.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0002

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM ADD_TEXT_0002 USING P_TEXT  TYPE SDYDO_TEXT_ELEMENT
                         P_STYLE TYPE SDYDO_ATTRIBUTE
                         P_SIZE  TYPE SDYDO_ATTRIBUTE
                         P_COLOR TYPE SDYDO_ATTRIBUTE.

* Adding text
  CALL METHOD DG_DYNDOC_ID_0002->ADD_TEXT
    EXPORTING
      TEXT          = P_TEXT
      SAP_STYLE     = P_STYLE
      SAP_FONTSIZE  = P_SIZE
      SAP_COLOR     = P_COLOR
      SAP_EMPHASIS  = CL_DD_AREA=>STRONG
      SAP_FONTSTYLE = CL_DD_AREA=>SANS_SERIF.

ENDFORM.                    " ADD_TEXT

FORM EVENT_TOP_OF_PAGE_0002 USING DG_DYNDOC_ID_0002 TYPE REF TO CL_DD_DOCUMENT.

  DATA : DL_TEXT(255) TYPE C,
         WA_EMPRESA   TYPE T001.

  SELECT SINGLE * INTO WA_EMPRESA
    FROM T001
   WHERE BUKRS EQ WA_RESUMO-EMPRESA.

  CONCATENATE 'Empresa:' WA_RESUMO-EMPRESA '-' WA_EMPRESA-BUTXT INTO DL_TEXT SEPARATED BY SPACE.
  PERFORM ADD_TEXT_0002 USING DL_TEXT '' CL_DD_AREA=>MEDIUM ''.
  CALL METHOD DG_DYNDOC_ID_0002->NEW_LINE.

  CONCATENATE 'Período de Fechamento:' S_MES_F(2) '/' S_MES_F+2(4) INTO DL_TEXT SEPARATED BY SPACE.
  PERFORM ADD_TEXT_0002 USING DL_TEXT '' CL_DD_AREA=>MEDIUM ''.
  CALL METHOD DG_DYNDOC_ID_0002->NEW_LINE.

  CONCATENATE 'Conta Razão:' WA_RESUMO-CONTA '-' WA_RESUMO-DS_CONTA INTO DL_TEXT SEPARATED BY SPACE.
  PERFORM ADD_TEXT_0002 USING DL_TEXT '' CL_DD_AREA=>MEDIUM ''.
  CALL METHOD DG_DYNDOC_ID_0002->NEW_LINE.

  PERFORM CONTAINER_HTML_0002.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONTAINER_HTML_0002 .

  DATA : DL_LENGTH  TYPE I,                           " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id

  IF DG_HTML_CNTRL_0002 IS INITIAL.
    CREATE OBJECT DG_HTML_CNTRL_0002
       EXPORTING
         PARENT = DG_PARENT_HTML_0002.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
   EXPORTING
     DOCUMENT = DG_DYNDOC_ID_0002
     BOTTOM   = SPACE
   IMPORTING
     LENGTH   = DL_LENGTH.

  CALL METHOD DG_DYNDOC_ID_0002->MERGE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID_0002->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID_0002->HTML_CONTROL = DG_HTML_CNTRL_0002.

  CALL METHOD DG_DYNDOC_ID_0002->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML_0002
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.                    " CONTAINER_HTML_0002



*&---------------------------------------------------------------------*
*&      Form  LIMPAR_CONTROLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_CONTROLES_0002 .

  DG_HTML_CNTRL_0002->FREE( ).
  CTL_ALV_PATNER->FREE( ).
  DG_PARENT_GRID_0002->FREE( ).
  DG_PARENT_HTML_0002->FREE( ).
  DG_SPLITTER_0002->FREE( ).
  CTL_CCCONTAINER_0002->FREE( ).

  CLEAR: EVENT_HANDLER_0002, CTL_ALV_PATNER, DG_PARENT_GRID_0002, DG_PARENT_HTML_0002,
         DG_SPLITTER_0002, CTL_CCCONTAINER_0002, DG_HTML_CNTRL_0002.

  GF_FIRST_DISPLAY_0002 = ABAP_TRUE.
ENDFORM.                    " LIMPAR_CONTROLES
