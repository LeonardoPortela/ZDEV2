*&---------------------------------------------------------------------*
*& Report  ZPMR0064
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Abap         :  Anderson Oenning ( AO ) - Amaggi
*& Data         : 28/05/2020
*& Especialista : Cleudo Ferreira
*& Chamado/Descrição : CS2019001804 - Interface recebimento de dados referente abastecimento de combustivel frota Amaggi - Autotrac
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT ZPMR0064.

TABLES: AUFK, ZPMT0035, EQUI, ZPMT0032, ZPMT0026, ZPMT0024.

DATA: T_FCAT             TYPE LVC_T_FCAT,
      DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
      TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN             TYPE REF TO CL_DD_AREA,
      P_TEXT             TYPE SDYDO_TEXT_ELEMENT,
      OBJ_CUSTOM         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
      DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
      GS_LAYOUT          TYPE LVC_S_LAYO,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      PICTURE            TYPE REF TO CL_GUI_PICTURE,
      URL(255)           TYPE C,
      CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID.

DATA:
  TABLE_ELEMENT2          TYPE REF TO CL_DD_TABLE_ELEMENT,
  SDYDO_TEXT_ELEMENT(255),
  P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
  LS_STABLE               TYPE LVC_S_STBL,
  DG_HTML_CNTRL           TYPE REF TO CL_GUI_HTML_VIEWER,
  COLUMN_1                TYPE REF TO CL_DD_AREA.

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR           TYPE STB_BUTTON.

DATA: R_EQUNR TYPE ZRSDSSELOPTS.
DATA: R_PLACA TYPE ZRSDSSELOPTS.
DATA: R_WERKS TYPE ZRSDSSELOPTS.
DATA: R_COMBST TYPE VRM_VALUE-TEXT.
DATA: R_STAT  TYPE ZRSDSSELOPTS..
DATA: R_SUC TYPE RANGE OF CHAR1.
DATA: R_ERR TYPE RANGE OF CHAR1.
DATA: P_SIM     TYPE CHAR1,
      VL_DATES1 TYPE CHAR10,
      VL_DATES2 TYPE CHAR10,
      P_NAO     TYPE CHAR1.



DATA: T_COMB    TYPE TABLE OF ZPME0061,
      NAME_COMP TYPE VRM_ID,
      LISTA     TYPE VRM_VALUES,
      VALUE     LIKE LINE OF LISTA,
      NAME      TYPE VRM_ID,
      T_COMBUST TYPE ZPME0063_T.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(10) TEXT-004.
PARAMETERS: R_R1 RADIOBUTTON GROUP 1. "Analitico

SELECTION-SCREEN COMMENT 25(10) TEXT-005.
PARAMETERS: R_R2 RADIOBUTTON GROUP 1. "Sintético
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
*                 P_BUKRS FOR AUFK-BUKRS NO-EXTENSION NO INTERVALS,
                 P_WERKS FOR AUFK-WERKS NO-EXTENSION NO INTERVALS OBLIGATORY ,
                 P_EQUNR FOR EQUI-EQUNR ,
                 P_PLACA FOR ZPMT0024-PLACA ,
                 P_ERDAT FOR AUFK-ERDAT OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-007.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 13.
PARAMETER: P_CLASS(12) AS LISTBOX VISIBLE LENGTH 13.
SELECTION-SCREEN COMMENT 1(12) TEXT-008 FOR FIELD P_CLASS.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B3.



AT SELECTION-SCREEN OUTPUT.
  FREE LISTA.
  APPEND VALUE #( KEY = '01' TEXT  = 'Diesel' ) TO LISTA.
  APPEND VALUE #( KEY = '02' TEXT  = 'Arla' ) TO LISTA.


  NAME = 'P_CLASS'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = LISTA.


  "CLASS
CLASS LCL_EVENTHANDLER DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:

      HANDLE_NODE_DOUBLE_CLICK
                  FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_ALV_TREE
        IMPORTING NODE_KEY,

      HANDLE_ITEM_DOUBLE_CLICK
                  FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_ALV_TREE
        IMPORTING NODE_KEY
                  FIELDNAME.

ENDCLASS.



CLASS LCL_EVENTHANDLER IMPLEMENTATION.

  METHOD HANDLE_NODE_DOUBLE_CLICK.


  ENDMETHOD.


  METHOD HANDLE_ITEM_DOUBLE_CLICK.


  ENDMETHOD.



ENDCLASS.



CLASS LCL_GRID_EVENT DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS HANDLE_TOOLBAR
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT.
*...User Command
    METHODS HANDLE_COMMAND_GRID
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.

    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.



ENDCLASS. "LCL_GRID_EVENT DEFINITION



CLASS LCL_GRID_EVENT IMPLEMENTATION.

  "Method Handle_Toolbar.
  METHOD HANDLE_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_REFRESH.
    TY_TOOLBAR-FUNCTION  = 'REFRESH'.
    TY_TOOLBAR-TEXT      = 'Atualizar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
  ENDMETHOD.


  "Method onClick
  METHOD ON_DOUBLE_CLICK.

    DATA: LC_BINARIO TYPE XSTRING,
          LT_PDF     TYPE TABLE OF CHAR80.

    DATA: PLACA TYPE CHAR7.
    DATA: TEXT TYPE STRING.

    CLEAR:  TEXT ,
            PLACA.

    CHECK E_ROW-ROWTYPE IS INITIAL.

    TRY .
*        data(W_saida) = T_LOG[ E_ROW ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    CASE E_COLUMN.
      WHEN 'STATUS'.


      WHEN 'EQUNR'.
*        SET PARAMETER ID 'EQN' FIELD W_ZPME0060-EQUNR.
        CALL TRANSACTION 'IE02' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.


  METHOD HANDLE_COMMAND_GRID.

    DATA: I_EQUNR TYPE EQUNR.

    CASE E_UCOMM.
      WHEN 'REFRESH'.


    ENDCASE.


  ENDMETHOD.
ENDCLASS.

DATA: LCL_EVENT         TYPE REF TO LCL_GRID_EVENT.

START-OF-SELECTION.



  R_EQUNR  =  VALUE #( FOR L IN  P_EQUNR   ( SIGN = 'I' OPTION = 'EQ' LOW = L-LOW HIGH = L-HIGH ) ).
  R_PLACA  =  VALUE #( FOR S IN  P_PLACA   ( SIGN = 'I' OPTION = 'EQ' LOW = S-LOW ) ).
  R_WERKS  =  VALUE #( FOR T IN  P_WERKS   ( SIGN = 'I' OPTION = 'EQ' LOW = T-LOW ) ).

  IF P_CLASS IS NOT INITIAL.
    IF P_CLASS EQ '01'.
       P_CLASS = 'Diesel'.
    ELSE.
      P_CLASS = 'Arla'.
    ENDIF.

    R_COMBST  =  P_CLASS.
  ENDIF.


  "Analitico
  FREE: T_COMB, T_COMBUST.
  IF R_R1 IS NOT INITIAL.
    ZCL_INT_SAPPM_AUTOTRAC=>I_CONS_COMBUST(
    EXPORTING
    I_PLACA           = R_PLACA
    I_EQUNR           = R_EQUNR
    I_CENTRO          = R_WERKS
    I_PERIDO_INIC     = P_ERDAT-LOW
    I_PERIDO_FIM      = P_ERDAT-HIGH
    I_COMB            = R_COMBST
    IMPORTING
    T_COMB            = T_COMB ).
  ELSE.
    "Sintético
    ZCL_INT_SAPPM_AUTOTRAC=>I_CONS_COMB_SINT(
    EXPORTING
    I_PLACA           = R_PLACA
    I_EQUNR           = R_EQUNR
    I_CENTRO          = R_WERKS
    I_PERIDO_INIC     = P_ERDAT-LOW
    I_PERIDO_FIM      = P_ERDAT-HIGH
    I_COMB            = R_COMBST
    IMPORTING
    T_COMB            = T_COMBUST ).
  ENDIF.

  IF T_COMB[] IS NOT INITIAL OR T_COMBUST[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S000(Z_LES) WITH TEXT-002 DISPLAY LIKE 'S'.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  SET TITLEBAR 'TIT0100'.

  PERFORM F_EXIBE_ALV.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXIBE_ALV .



  IF R_R1 IS NOT INITIAL.
    FREE: T_FCAT.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = 'ZPME0061'
        I_CLIENT_NEVER_DISPLAY = 'X'
      CHANGING
        CT_FIELDCAT            = T_FCAT
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.
  ELSE.

    FREE: T_FCAT.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = 'ZPME0062'
        I_CLIENT_NEVER_DISPLAY = 'X'
      CHANGING
        CT_FIELDCAT            = T_FCAT
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.
  ENDIF.

  LOOP AT T_FCAT ASSIGNING FIELD-SYMBOL(<FS_FCAT>).
    IF <FS_FCAT>-FIELDNAME EQ 'QTDE' OR <FS_FCAT>-FIELDNAME EQ 'QTDE_AUT' OR <FS_FCAT>-FIELDNAME EQ 'DIF'.
      <FS_FCAT>-DO_SUM    = ABAP_TRUE.
    ENDIF.
  ENDLOOP.


  GS_LAYOUT-ZEBRA = ABAP_TRUE.       "Código Zebrado
  GS_LAYOUT-NO_ROWMARK = ABAP_TRUE. "Exclui barra standard de flag a esquerda
  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE. "Ajusta tamanho na coluna
  GS_LAYOUT-BOX_FNAME  = ABAP_TRUE. "
*  GS_LAYOUT-TOTALS_BEF = ABAP_TRUE.

  IF CTL_ALV IS INITIAL.
    CREATE OBJECT OBJ_CUSTOM
      EXPORTING
        CONTAINER_NAME              = 'ALV_COMB'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.


    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = OBJ_CUSTOM
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
        WIDTH = 65.

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

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT          = DG_PARENT_ALV
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.


    CREATE OBJECT LCL_EVENT.
*      SET HANDLER LCL_EVENT-> FOR CTL_ALV.
    SET HANDLER LCL_EVENT->HANDLE_TOOLBAR FOR CTL_ALV.
    SET HANDLER LCL_EVENT->HANDLE_COMMAND_GRID FOR CTL_ALV.
    SET HANDLER LCL_EVENT->ON_DOUBLE_CLICK  FOR CTL_ALV.


  ENDIF.

  SORT T_COMB    BY DATA_INI.
  SORT T_COMBUST BY DATA_INI.

  IF R_R1 IS NOT INITIAL.
    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = T_FCAT
        IT_OUTTAB       = T_COMB.
  ELSE.
    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = T_FCAT
        IT_OUTTAB       = T_COMBUST.
  ENDIF.

***Cabecalho.
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

  P_TEXT = TEXT-006.

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

  PERFORM CABECARIO.

*  ------------------
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


  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CABECARIO .

  DATA: L_RELAT TYPE CHAR20.

  READ TABLE T_COMB ASSIGNING FIELD-SYMBOL(<_LOG>) INDEX 1.

  CONCATENATE 'Centro:' P_WERKS-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  CLEAR: SDYDO_TEXT_ELEMENT.

  IF R_R1 IS NOT INITIAL.
    L_RELAT = 'Analitico'.
  ELSE.
    L_RELAT = 'Sintético'.
  ENDIF.

  CONCATENATE 'Tipo:' L_RELAT INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  CLEAR: SDYDO_TEXT_ELEMENT.

  "------------------
  LOOP AT P_ERDAT.
    IF P_ERDAT-OPTION EQ 'BT'.
      CONCATENATE P_ERDAT-LOW+6(2) '.' P_ERDAT-LOW+4(2) '.' P_ERDAT-LOW(4) INTO VL_DATES1.
      CONCATENATE P_ERDAT-HIGH+6(2) '.' P_ERDAT-HIGH+4(2) '.' P_ERDAT-HIGH(4) INTO VL_DATES2.
      CONCATENATE 'Período:' VL_DATES1 '-' VL_DATES2 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      EXIT.
    ELSE.
      CONCATENATE P_ERDAT-LOW+6(2) '.' P_ERDAT-LOW+4(2) '.' P_ERDAT-LOW(4) INTO VL_DATES1.
      CONCATENATE 'Período:' VL_DATES1 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    ENDIF.
    CLEAR: SDYDO_TEXT_ELEMENT.
  ENDLOOP.
*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0699   text
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

ENDFORM.
