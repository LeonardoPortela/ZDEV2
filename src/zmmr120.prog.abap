*&---------------------------------------------------------------------*
*& Report  ZMMR120
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR120.
*&
*&---------------------------------------------------------------------*
*&Relatorio de compra de insumos orçado / Estoque
*&ANTONIO LUIZ RODRIGUES DA SILVA
*&07/08/2017
*&---------------------------------------------------------------------*
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
  BEGIN OF TY_MAKT,
    MATNR TYPE MAKT-MATNR,
    MAKTX TYPE MAKT-MAKTX,
  END OF TY_MAKT,

  BEGIN OF TY_GEO,
    SAFRA TYPE ZMMT0079-SAFRA,
    WERKS TYPE ZMMT0079-WERKS,
    MATNR TYPE ZMMT0079-MATNR,
    MENGE TYPE ZMMT0079-MENGE,
  END OF TY_GEO,

  BEGIN OF TY_MARD,
    WERKS TYPE MARD-WERKS,
    MATNR TYPE MARD-MATNR,
    LABST TYPE MARD-LABST,
  END OF TY_MARD,
  BEGIN OF TY_SAIDA,
    SAFRA     TYPE ZMMT0035-SAFRA,
    WERKS     TYPE ZMMT0035-WERKS,
    MATKL     TYPE MARA-MATKL,
    MATNR     TYPE EKPO-MATNR,
    TXZ01     TYPE EKPO-TXZ01,
    MENGE     TYPE EKPO-MENGE, "Pedidos na safra
    MENGE_EST TYPE EKPO-MENGE, "Estoque SAP filial
    MENGE_GEO TYPE EKPO-MENGE, "Orçado GEO
  END OF TY_SAIDA.


*&--------------------------------------------------------------------&*
*& WorkAreas                                                          &*
*&--------------------------------------------------------------------&*
DATA: WA_ZMMT0035 TYPE ZMMT0035,
      WA_SAIDA    TYPE TY_SAIDA,
      WA_MARD     TYPE TY_MARD,
      WA_MAKT     TYPE TY_MAKT,
      WA_ZMMT0079 TYPE ZMMT0079,
      WA_GEO      TYPE TY_GEO.


*&--------------------------------------------------------------------&*
*& Tabelas Internas                                                   &*
*&--------------------------------------------------------------------&*
DATA: IT_SAIDA     TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      IT_SAIDA_TOT TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      IT_MARD_TOT  TYPE TABLE OF TY_MARD,
      IT_MARD      TYPE TABLE OF TY_MARD WITH HEADER LINE,
      IT_MAKT      TYPE TABLE OF TY_MAKT WITH HEADER LINE,
      IT_ZMMT0079  TYPE TABLE OF ZMMT0079.

DATA: T_EXCEL  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_EXCEL2 LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
* ---> S4 Migration - 19/06/2023 - MA
*      VMSG(50),
      VMSG(70),
* <--- S4 Migration - 19/06/2023 - MA
      VSAFRA   TYPE I.



CLEAR T_EXCEL.
REFRESH T_EXCEL.

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
      GS_VARIANT_C       TYPE DISVARIANT.

CONSTANTS:   C_X               TYPE C VALUE 'X'.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_SAFRA FOR IT_SAIDA-SAFRA OBLIGATORY,
                S_WERKS FOR IT_SAIDA-WERKS OBLIGATORY,
                S_MATKL FOR IT_SAIDA-MATKL,
                S_BSART FOR EKKO-BSART,
                S_DATA  FOR EKKO-BEDAT OBLIGATORY.
SELECTION-SCREEN:END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: R_MOD LIKE BSID-UMSKZ AS CHECKBOX  DEFAULT ' ',
            R_DEL LIKE BSID-UMSKZ AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK B3.

PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT ''.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
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


START-OF-SELECTION.
  "
  IF R_MOD = 'X'.
    IF R_DEL = 'X'.
      DELETE FROM ZMMT0079 WHERE SAFRA IN S_SAFRA.
      COMMIT WORK.
    ENDIF.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        FILENAME                = P_FILE
        I_BEGIN_COL             = 1
        I_BEGIN_ROW             = 2
        I_END_COL               = 4
        I_END_ROW               = 10000
      TABLES
        INTERN                  = T_EXCEL
      EXCEPTIONS
        INCONSISTENT_PARAMETERS = 1
        UPLOAD_OLE              = 2
        OTHERS                  = 3.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = 'Atualizando Dados'.

    T_EXCEL2[] = T_EXCEL[].
    SORT T_EXCEL2 BY ROW COL.
    CLEAR T_EXCEL2.
    LOOP AT T_EXCEL.
      IF T_EXCEL-ROW = T_EXCEL2-ROW.
        CONTINUE.
      ENDIF.
      CLEAR WA_GEO.
      LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
        CASE T_EXCEL2-COL.
          WHEN 1.
            CONCATENATE '20' T_EXCEL2-VALUE+1(2) '/20' T_EXCEL2-VALUE+3(2)  INTO WA_GEO-SAFRA.
          WHEN 2.
            WA_GEO-WERKS        = T_EXCEL2-VALUE.
          WHEN 3.
            WA_GEO-MATNR        = T_EXCEL2-VALUE.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_GEO-MATNR
              IMPORTING
                OUTPUT = WA_GEO-MATNR.

          WHEN 4.
            REPLACE '.' WITH ' ' INTO T_EXCEL2-VALUE.
            REPLACE ',' WITH '.' INTO T_EXCEL2-VALUE.
            WA_GEO-MENGE        = T_EXCEL2-VALUE.
        ENDCASE.
      ENDLOOP.
      MOVE-CORRESPONDING WA_GEO TO WA_ZMMT0079.
      MODIFY ZMMT0079 FROM WA_ZMMT0079.
      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        MESSAGE 'ERRO' TYPE 'I'.
      ENDIF.
      CONCATENATE 'Linha ' T_EXCEL-ROW 'Material '  WA_GEO-MATNR INTO VMSG SEPARATED BY SPACE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          TEXT = VMSG.
    ENDLOOP.

    MESSAGE 'Fim atualização' TYPE 'I'.
*&
  ELSE.
    PERFORM: F_SELECIONAR_DADOS,               " Form selecionar dado
             F_ORGANIZAR_DADOS,                " ORGANIZAR DADOS
             F_ALV.                            "Saida ALV
  ENDIF.

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

  SELECT ZMMT0035~SAFRA ZMMT0035~WERKS EKPO~MATKL EKPO~MATNR EKPO~TXZ01 EKPO~MENGE
    FROM ZMMT0035
    INNER JOIN EKKO
    ON EKKO~EBELN = ZMMT0035~EBELN
    INNER JOIN EKPO
    ON EKPO~EBELN = ZMMT0035~EBELN
    INNER JOIN LFA1
    ON LFA1~LIFNR = EKKO~LIFNR
    INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA
    WHERE ZMMT0035~WERKS IN S_WERKS
    AND   ZMMT0035~SAFRA IN S_SAFRA
    AND   EKPO~MATKL     IN S_MATKL
    AND   EKKO~BSART     IN S_BSART
  AND   EKKO~BEDAT     IN S_DATA.

  CHECK SY-SUBRC = 0.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_SAIDA
    WHERE SPRAS = SY-LANGU
    AND   MATNR = IT_SAIDA-MATNR.

  SELECT WERKS MATNR LABST
    FROM MARD
    INTO TABLE IT_MARD
    FOR ALL ENTRIES IN IT_SAIDA
    WHERE WERKS EQ IT_SAIDA-WERKS
  AND   MATNR EQ IT_SAIDA-MATNR.

  SELECT *
    FROM ZMMT0079
    INTO TABLE IT_ZMMT0079
    WHERE SAFRA IN S_SAFRA
  AND   WERKS IN S_WERKS.

  IF IT_ZMMT0079[] IS NOT INITIAL.
    SELECT MATNR MAKTX
      FROM MAKT
      APPENDING TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_ZMMT0079
      WHERE SPRAS = SY-LANGU
      AND   MATNR = IT_ZMMT0079-MATNR.

  ENDIF.

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
  DATA: TABIX TYPE SY-TABIX.

  LOOP AT IT_SAIDA.
    COLLECT IT_SAIDA INTO IT_SAIDA_TOT.
  ENDLOOP.
  LOOP AT IT_MARD.
    MOVE-CORRESPONDING IT_MARD TO WA_MARD.
    COLLECT WA_MARD INTO IT_MARD_TOT.
  ENDLOOP.

  SORT: IT_SAIDA_TOT BY SAFRA WERKS MATKL,
        IT_MARD_TOT  BY WERKS MATNR,
        IT_ZMMT0079  BY WERKS MATNR,
        IT_MAKT      BY MATNR.
  LOOP AT IT_SAIDA_TOT.
    TABIX = SY-TABIX.
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = IT_SAIDA_TOT-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_SAIDA_TOT-TXZ01 = WA_MAKT-MAKTX.
      MODIFY IT_SAIDA_TOT INDEX TABIX TRANSPORTING TXZ01.
    ENDIF.
    READ TABLE IT_MARD_TOT INTO WA_MARD WITH KEY WERKS = IT_SAIDA_TOT-WERKS
                                                 MATNR = IT_SAIDA_TOT-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_SAIDA_TOT-MENGE_EST = WA_MARD-LABST.
      MODIFY IT_SAIDA_TOT INDEX TABIX TRANSPORTING MENGE_EST.
    ENDIF.
    READ TABLE IT_ZMMT0079 INTO WA_ZMMT0079 WITH KEY WERKS = IT_SAIDA_TOT-WERKS
                                                     MATNR = IT_SAIDA_TOT-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_SAIDA_TOT-MENGE_GEO = WA_ZMMT0079-MENGE.
      MODIFY IT_SAIDA_TOT INDEX TABIX TRANSPORTING MENGE_GEO.
    ENDIF.
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
  WA_AFIELD-FIELDNAME     = 'MATNR '.
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
  WA_AFIELD-FIELDNAME     = 'MENGE'.
  WA_AFIELD-SCRTEXT_S = 'Neg.Safra'.
  WA_AFIELD-SCRTEXT_L = 'Neg.Safra'.
  WA_AFIELD-SCRTEXT_M = 'Neg.Safra'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MENGE_GEO'.
  WA_AFIELD-SCRTEXT_S = 'Orc.GEO'.
  WA_AFIELD-SCRTEXT_L = 'Orc.GEO'.
  WA_AFIELD-SCRTEXT_M = 'Orc.GEO'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MENGE_EST'.
  WA_AFIELD-SCRTEXT_S = 'Estoque'.
  WA_AFIELD-SCRTEXT_L = 'Estoque'.
  WA_AFIELD-SCRTEXT_M = 'Estoque'.
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
