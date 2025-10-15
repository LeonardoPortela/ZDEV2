*&---------------------------------------------------------------------*
*& Report  ZMMR119
*&
*&---------------------------------------------------------------------*
*&Relatorio de Preço Médio dos Produtos (Pedidos)
*&ANTONIO LUIZ RODRIGUES DA SILVA
*&04/08/2017
*&---------------------------------------------------------------------*
REPORT ZMMR119.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
TABLES: EKKO.


*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES:
  BEGIN OF TY_SAIDA,
    EBELN  TYPE EKKO-EBELN,
    WAERS  TYPE EKKO-WAERS,
    SAFRA  TYPE ZMMT0035-SAFRA,
    WERKS  TYPE ZMMT0035-WERKS,
    MATKL  TYPE MARA-MATKL,
    WGBEZ  TYPE T023T-WGBEZ,
    LIFNR  TYPE EKKO-LIFNR,
    NAME1  TYPE LFA1-NAME1,
    MATNR  TYPE EKPO-MATNR,
    TXZ01  TYPE EKPO-TXZ01,
    MENGE  TYPE EKPO-MENGE,
    BRTWR  TYPE EKPO-BRTWR,
    BRTWR2 TYPE EKPO-BRTWR,
  END OF TY_SAIDA.


*&--------------------------------------------------------------------&*
*& WorkAreas                                                          &*
*&--------------------------------------------------------------------&*
DATA: WA_ZMMT0035  TYPE ZMMT0035,
      WA_SAIDA     TYPE TY_SAIDA,
      WA_SAIDA_MAT TYPE TY_SAIDA.


*&--------------------------------------------------------------------&*
*& Tabelas Internas                                                   &*
*&--------------------------------------------------------------------&*
DATA: IT_SAIDA     TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      IT_SAIDA_MAT TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      IT_SAIDA_TOT TYPE TABLE OF TY_SAIDA WITH HEADER LINE.


************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar
*CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: EDITCONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTA0200 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      CONTAINER_1        TYPE REF TO CL_GUI_CONTAINER,
      CONTAINER_2        TYPE REF TO CL_GUI_CONTAINER,
      SPLITTER           TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      EDITOR             TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95    TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID      TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID            TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE          TYPE LVC_S_STBL,
      WA_AFIELD          TYPE LVC_S_FCAT,
      IT_FIELDCAT        TYPE LVC_T_FCAT,
      W_FIELDCAT         TYPE LVC_S_FCAT,
      I_SORT             TYPE LVC_T_SORT,
      WA_LAYOUT          TYPE LVC_S_LAYO,
      IS_STABLE          TYPE LVC_S_STBL VALUE 'XX',
      WG_SAVE(1)         TYPE C,
      GS_VARIANT_C       TYPE DISVARIANT,
      GS_VARIANT_2       TYPE DISVARIANT.

DATA: G_CONTAINER2 TYPE SCRFNAME VALUE 'CC_PED',
      "
      GRID2        TYPE REF TO CL_GUI_ALV_GRID.

CONSTANTS:   C_X               TYPE C VALUE 'X'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      CATCH_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD CATCH_HOTSPOT.
    IF E_ROW_ID GT 0.
      IF  E_COLUMN_ID = 'SAFRA'.
        REFRESH IT_SAIDA_MAT.
        READ TABLE IT_SAIDA_TOT INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
        LOOP AT IT_SAIDA INTO WA_SAIDA.

        ENDLOOP.
        CALL SCREEN 0200 STARTING AT 050 3
                           ENDING   AT 95 30.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
ENDCLASS.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_SAFRA FOR IT_SAIDA-SAFRA OBLIGATORY,
                S_WERKS FOR IT_SAIDA-WERKS OBLIGATORY,
                S_MATKL FOR IT_SAIDA-MATKL,
                S_BSART FOR EKKO-BSART,
                S_DATA  FOR EKKO-BEDAT OBLIGATORY.
SELECTION-SCREEN:END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM: F_SELECIONAR_DADOS,               " Form selecionar dado
           F_ORGANIZAR_DADOS,                " ORGANIZAR DADOS
           F_ALV.                            "Saida ALV

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONAR_DADOS .

  SELECT EKKO~EBELN EKKO~WAERS ZMMT0035~SAFRA ZMMT0035~WERKS EKPO~MATKL T023T~WGBEZ EKKO~LIFNR LFA1~NAME1 EKPO~MATNR EKPO~TXZ01 EKPO~MENGE EKPO~BRTWR EKPO~BRTWR
    FROM ZMMT0035
    INNER JOIN EKKO
    ON EKKO~EBELN = ZMMT0035~EBELN
    INNER JOIN EKPO
    ON EKPO~EBELN = ZMMT0035~EBELN
    INNER JOIN LFA1
    ON LFA1~LIFNR = EKKO~LIFNR
    INNER JOIN T023T
    ON T023T~MATKL = EKPO~MATKL
    AND   T023T~SPRAS EQ SY-LANGU
    INTO  TABLE IT_SAIDA
    WHERE ZMMT0035~WERKS IN S_WERKS
    AND   ZMMT0035~SAFRA IN S_SAFRA
    AND   EKPO~MATKL     IN S_MATKL
    AND   EKKO~BSART     IN S_BSART
    AND   EKKO~BEDAT     IN S_DATA.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF  NOT CL_GRID IS INITIAL.
    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = GS_VARIANT_C
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = WG_SAVE
        I_DEFAULT       = 'X'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA_TOT[].

    SET HANDLER:
              LCL_EVENT_HANDLER=>CATCH_HOTSPOT FOR CL_GRID.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ORGANIZAR_DADOS .
  SORT: IT_SAIDA BY SAFRA WERKS MATNR.

  LOOP AT IT_SAIDA.
*    CLEAR: IT_SAIDA-EBELN.
    COLLECT IT_SAIDA INTO IT_SAIDA_TOT.
  ENDLOOP.

  SORT IT_SAIDA_TOT BY SAFRA WERKS MATKL.
  LOOP AT IT_SAIDA_TOT.
    TRY .
        IT_SAIDA_TOT-BRTWR = IT_SAIDA_TOT-BRTWR / IT_SAIDA_TOT-MENGE.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
    MODIFY IT_SAIDA_TOT INDEX SY-TABIX TRANSPORTING BRTWR.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .
  PERFORM F_ALV_FIELDCAT.
  WA_LAYOUT-ZEBRA     = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.

  WA_LAYOUT-GRID_TITLE = 'Preço Médio'.

  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT = ''.
  WA_LAYOUT-BOX_FNAME  = ''.
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
           WL_HORA(8),
           WL_LINHA(60),
           WL_TEXT TYPE SDYDO_TEXT_ELEMENT.


*
*  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
*    EXPORTING
*      TEXT         = WL_TEXT
*      SAP_STYLE    = CL_DD_AREA=>HEADING
*      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  IF S_SAFRA-HIGH IS NOT INITIAL.
    CONCATENATE  'Safra:' S_SAFRA-LOW 'a' S_SAFRA-HIGH
  INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Safra:' S_SAFRA-LOW
    INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  IF S_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE  'Centro:' S_WERKS-LOW 'a' S_WERKS-HIGH
  INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Centro:' S_WERKS-LOW
    INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.
  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  CONCATENATE S_DATA-LOW+6(2) '.'  S_DATA-LOW+4(2) '.' S_DATA-LOW+0(4) INTO WL_DATA.
  IF S_DATA-HIGH IS NOT INITIAL.
    CONCATENATE  'Data:'  WL_DATA 'a'
     INTO WL_LINHA SEPARATED BY SPACE.
    CONCATENATE S_DATA-HIGH+6(2) '.'  S_DATA-HIGH+4(2) '.' S_DATA-HIGH+0(4) INTO WL_DATA.
    CONCATENATE  WL_LINHA  WL_DATA
     INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Data:' WL_DATA
    INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.


  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  IF S_MATKL-HIGH IS NOT INITIAL.
    CONCATENATE  'Grupo:' S_MATKL-LOW 'a' S_MATKL-HIGH
  INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Grupo:' S_MATKL-LOW
    INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.
  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  "Pequenas
  IF S_BSART-HIGH IS NOT INITIAL.
    CONCATENATE  'Tipo Pedido:' S_BSART-LOW 'a' S_BSART-HIGH
  INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Tipo Pedido:' S_BSART-LOW
    INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.
  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  REFRESH IT_FIELDCAT.
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA_TOT'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELN'.
  WA_AFIELD-SCRTEXT_S = 'Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 10.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WAERS'.
  WA_AFIELD-SCRTEXT_S = 'Moeda'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 10.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAFRA'.
  WA_AFIELD-SCRTEXT_S = 'Safra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WERKS'.
  WA_AFIELD-SCRTEXT_S = 'Centro'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 10.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATKL'.
  WA_AFIELD-SCRTEXT_S = 'Grupo'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME = 'WGBEZ'.
  WA_AFIELD-SCRTEXT_S = 'Descr. Grupo'.
  WA_AFIELD-SCRTEXT_L = 'Descr. Grupo'.
  WA_AFIELD-SCRTEXT_M = 'Descr. Grupo'.
  WA_AFIELD-OUTPUTLEN = 25.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Cod.Forn'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Descrição'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 25.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNR '.

  WA_AFIELD-REF_TABLE = 'MARA'.
  WA_AFIELD-REF_FIELD = 'MATNR'.

  WA_AFIELD-SCRTEXT_S = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TXZ01'.
  WA_AFIELD-SCRTEXT_S = 'Desc.Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 25.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BRTWR'.
  WA_AFIELD-SCRTEXT_S = 'Preço Médio'.
  WA_AFIELD-SCRTEXT_L = 'Preço Médio'.
  WA_AFIELD-SCRTEXT_M = 'Preço Médio'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MENGE'.
  WA_AFIELD-SCRTEXT_S = 'Quantidade'.
  WA_AFIELD-SCRTEXT_L = 'Quantidade do produto.'.
  WA_AFIELD-SCRTEXT_M = 'Quantidade produto.'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BRTWR2'.
  WA_AFIELD-SCRTEXT_S = 'Valor total'.
  WA_AFIELD-SCRTEXT_L = 'Valor total da Compra'.
  WA_AFIELD-SCRTEXT_M = 'Valor total'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT2 .
  REFRESH IT_FIELDCAT.
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELN'.
  WA_AFIELD-SCRTEXT_S = 'Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 12.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WAERS'.
  WA_AFIELD-SCRTEXT_S = 'Moeda'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 10.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


ENDFORM.
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS_0200 OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION  WITH HEADER LINE.


  WA_STABLE-ROW = 'X'.
  WA_STABLE-COL = 'X'.

  IF G_CUSTOM_CONTA0200 IS INITIAL.
    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA       = C_X.
    WA_LAYOUT-NO_ROWMARK  = SPACE.
    WA_LAYOUT-SEL_MODE    = 'B'.
    WA_LAYOUT-BOX_FNAME   = ''.

    CREATE OBJECT G_CUSTOM_CONTA0200
      EXPORTING
        CONTAINER_NAME = G_CONTAINER2.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTA0200
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_2.

    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = CONTAINER_2.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CLEAR WA_LAYOUT-CWIDTH_OPT.
    GS_VARIANT_2-REPORT = SY-REPID.

    PERFORM F_ALV_FIELDCAT.

    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = GS_VARIANT_2
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        I_DEFAULT            = 'X'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCAT[]
        IT_OUTTAB            = IT_SAIDA_MAT[].

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    SET HANDLER:
              LCL_EVENT_HANDLER=>CATCH_HOTSPOT FOR GRID2.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.

  ELSE.


  ENDIF.
ENDMODULE.
