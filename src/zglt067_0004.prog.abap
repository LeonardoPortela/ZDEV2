*----------------------------------------------------------------------*
***INCLUDE ZGLT067_0004 .
*----------------------------------------------------------------------*

TYPE-POOLS: ICON.

TABLES: BSIS, ZDE_BSXX_COMP.

DATA: IT_BSXK_C     TYPE TABLE OF BSIK WITH HEADER LINE,
      IT_BSXD_C     TYPE TABLE OF BSID WITH HEADER LINE,
      IT_BSXX_ALV   TYPE TABLE OF ZDE_BSXX_COMP_ALV WITH HEADER LINE,
      IT_BSXX_ALV_C TYPE TABLE OF ZDE_BSXX_COMP_ALV_C WITH HEADER LINE,
      TL_400X       TYPE SYDYNNR.

CONSTANTS: OK_VERIFICAR TYPE SY-UCOMM VALUE 'VERIFICAR',
           OK_PESQUISAR TYPE SY-UCOMM VALUE 'PESQUISAR',
           OK_SAVE      TYPE SY-UCOMM VALUE 'SAVE'.

CONSTANTS: TL_4001      TYPE SYDYNNR VALUE '4001',
           TL_4002      TYPE SYDYNNR VALUE '4002', "Filtro
           TL_4003      TYPE SYDYNNR VALUE '4003', "Documentos para Seleção
           TL_4004      TYPE SYDYNNR VALUE '4004', "Documentos para Compensação
           TL_4005      TYPE SYDYNNR VALUE '4005',
           TL_4006      TYPE SYDYNNR VALUE '4006',
           TL_4007      TYPE SYDYNNR VALUE '4007'.

"Objetos
INCLUDE ZGLT067_000X.

"Objetos para documentos em aberto """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

"(1)
DATA: G_CUSTOM_4003      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_DYNDOC_4003     TYPE REF TO CL_DD_DOCUMENT,
      DG_SPLITTER_4003   TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CON_4003       TYPE REF TO CL_GUI_CONTAINER,
      CTL_CON_4004       TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV_4003       TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAY_4003        TYPE LVC_S_LAYO,
      GS_VAR_4003        TYPE DISVARIANT,
      GS_SCROLL_COL_4003 TYPE LVC_S_COL,
      GS_SCROLL_ROW_4003 TYPE LVC_S_ROID,
      IT_CATALOG_4003    TYPE LVC_T_FCAT,
      OBG_TOOLBAR_4003   TYPE REF TO LCL_ALV_TOOLBAR_4003.

DATA: IT_SELECTED_4003   TYPE LVC_T_ROW,
      WA_SELECTED_4003   TYPE LVC_S_ROW,
      IT_EXCLUDE_4003    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_4003    LIKE LINE OF IT_EXCLUDE_4003.

"(2)
DATA: G_CUSTOM_4006       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_DYNDOC_4006      TYPE REF TO CL_DD_DOCUMENT,
      DG_SPLITTER_4006    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CON_4006        TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV_4006        TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAY_4006         TYPE LVC_S_LAYO,
      GS_VAR_4006         TYPE DISVARIANT,
      GS_SCROLL_COL_4006  TYPE LVC_S_COL,
      GS_SCROLL_ROW_4006  TYPE LVC_S_ROID,
      IT_CATALOG_4006     TYPE LVC_T_FCAT,
      OBG_TOOLBAR_4006    TYPE REF TO LCL_ALV_TOOLBAR_4006.

DATA: IT_SELECTED_4006   TYPE LVC_T_ROW,
      WA_SELECTED_4006   TYPE LVC_S_ROW,
      IT_EXCLUDE_4006    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_4006    LIKE LINE OF IT_EXCLUDE_4006.


"Objetos para documentos p/ Compensação """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

"(1)
DATA: CTL_ALV_4004        TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAY_4004         TYPE LVC_S_LAYO,
      GS_VAR_4004         TYPE DISVARIANT,
      GS_SCROLL_COL_4004  TYPE LVC_S_COL,
      GS_SCROLL_ROW_4004  TYPE LVC_S_ROID,
      IT_CATALOG_4004     TYPE LVC_T_FCAT,
      OBG_TOOLBAR_4004    TYPE REF TO LCL_ALV_TOOLBAR_4004.

DATA: IT_SELECTED_4004    TYPE LVC_T_ROW,
      WA_SELECTED_4004    TYPE LVC_S_ROW,
      IT_EXCLUDE_4004     TYPE UI_FUNCTIONS,
      WA_EXCLUDE_4004     LIKE LINE OF IT_EXCLUDE_4004.

"(2)
DATA: CTL_CON_4007        TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV_4007        TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAY_4007         TYPE LVC_S_LAYO,
      GS_VAR_4007         TYPE DISVARIANT,
      GS_SCROLL_COL_4007  TYPE LVC_S_COL,
      GS_SCROLL_ROW_4007  TYPE LVC_S_ROID,
      IT_CATALOG_4007     TYPE LVC_T_FCAT,
      OBG_TOOLBAR_4007    TYPE REF TO LCL_ALV_TOOLBAR_4007.

DATA: IT_SELECTED_4007   TYPE LVC_T_ROW,
      WA_SELECTED_4007   TYPE LVC_S_ROW,
      IT_EXCLUDE_4007    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_4007    LIKE LINE OF IT_EXCLUDE_4007.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

SELECTION-SCREEN BEGIN OF SCREEN 4002 AS SUBSCREEN.
PARAMETERS:     PBUKRC TYPE SKB1-BUKRS OBLIGATORY DEFAULT '0202',
                PLIFNR TYPE LFA1-LIFNR,
                PKUNNR TYPE KNA1-KUNNR,
                PWAERS TYPE T001-WAERS.
SELECT-OPTIONS: PBUDAT FOR BSIS-BUDAT,
                PBLDAT FOR BSIS-BLDAT.
SELECTION-SCREEN END OF SCREEN 4002.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0004 OUTPUT.

  DATA: IT_EXCLUIR TYPE TABLE OF SY-UCOMM.

  IF PDATA IS INITIAL.
    CALL SCREEN 4008 STARTING AT 5 5.
  ENDIF.

  IF PDATA IS INITIAL.
    LEAVE PROGRAM.
  ENDIF.

  CLEAR: IT_EXCLUIR.

  IF ZDE_BSXX_COMP-ICONE NE ICON_LED_GREEN.
    APPEND OK_SAVE TO IT_EXCLUIR.
  ENDIF.

  SET PF-STATUS 'PF0004' EXCLUDING IT_EXCLUIR.
  SET TITLEBAR 'TL0004' WITH PDATA.

  IF TL_400X IS INITIAL.
    TL_400X = TL_4001.
  ENDIF.

ENDMODULE.                 " STATUS_0004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0004_EXIT INPUT.

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_BSXX_ALV_C[] IS INITIAL.
    LEAVE PROGRAM.
  ELSE.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        TITEL     = TEXT-012
        TEXTLINE1 = TEXT-013
      IMPORTING
        ANSWER    = ANSWER.

    CASE ANSWER.
      WHEN 'J'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0004_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0004 INPUT.
  CASE OK_CODE.
    WHEN OK_VERIFICAR.
      PERFORM VERIFICAR_COMPENSACAO.
    WHEN OK_PESQUISAR.
      PERFORM PESQUISAR_DOCUMENTOS.
    WHEN OK_SAVE.
      PERFORM EXECUTAR_COMPENSACAO.
  ENDCASE.
  CLEAR: OK_CODE.
ENDMODULE.                 " USER_COMMAND_0004  INPUT


*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_COMPENSACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICAR_COMPENSACAO .

  PERFORM TOTALIZAR_SALDO.

  IF ZDE_BSXX_COMP-VALOR_SALDO NE 0.
    ZDE_BSXX_COMP-ICONE = ICON_LED_RED.
  ELSE.
    IF ZDE_BSXX_COMP-VALOR_PAGAR EQ 0 AND ZDE_BSXX_COMP-VALOR_RECEBER = 0.
      ZDE_BSXX_COMP-ICONE = ICON_LED_YELLOW.
    ELSE.
      ZDE_BSXX_COMP-ICONE = ICON_LED_GREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFICAR_COMPENSACAO

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_DOCUMENTOS .

  DATA: IT_LFA1 TYPE TABLE OF LFA1 WITH HEADER LINE,
        IT_KNA1 TYPE TABLE OF KNA1 WITH HEADER LINE,
        IT_TBSL TYPE TABLE OF TBSL WITH HEADER LINE.

  DATA: WA_BUDAT TYPE FKKR_BUDAT,
        WA_BLDAT TYPE FKKR_BLDAT,
        IT_BUDAT TYPE	FKK_RT_BUDAT,
        IT_BLDAT TYPE	FKK_RT_BLDAT.

  CLEAR: IT_BSXX_ALV[],
         IT_BSXK_C[],
         IT_BSXD_C[].

  LOOP AT PBUDAT.
    MOVE PBUDAT TO WA_BUDAT.
    APPEND WA_BUDAT TO IT_BUDAT.
  ENDLOOP.

  LOOP AT PBLDAT.
    MOVE PBLDAT TO WA_BLDAT.
    APPEND WA_BLDAT TO IT_BLDAT.
  ENDLOOP.

  IF PLIFNR IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        I_COMPANY   = PBUKRC
        I_MOEDA_DOC = PWAERS
        I_FORNE     = 'X'
        I_PARID     = PLIFNR
        I_BUDAT     = IT_BUDAT
        I_BLDAT     = IT_BLDAT
      TABLES
        IT_BSXK     = IT_BSXK_C.
  ENDIF.

  IF PKUNNR IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        I_COMPANY   = PBUKRC
        I_MOEDA_DOC = PWAERS
        I_CLIENTE   = 'X'
        I_PARID     = PKUNNR
        I_BUDAT     = IT_BUDAT
        I_BLDAT     = IT_BLDAT
      TABLES
        IT_BSXD     = IT_BSXD_C.
  ENDIF.

  IF PKUNNR IS INITIAL AND PLIFNR IS INITIAL.
    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        I_COMPANY   = PBUKRC
        I_MOEDA_DOC = PWAERS
        I_FORNE     = 'X'
        I_CLIENTE   = 'X'
        I_BUDAT     = IT_BUDAT
        I_BLDAT     = IT_BLDAT
      TABLES
        IT_BSXK     = IT_BSXK_C
        IT_BSXD     = IT_BSXD_C.
  ENDIF.

  IF IT_BSXK_C[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_LFA1
       FROM LFA1
      FOR ALL ENTRIES IN IT_BSXK_C
     WHERE LIFNR EQ IT_BSXK_C-LIFNR.
    SORT IT_LFA1 BY LIFNR.
  ENDIF.

  IF IT_BSXD_C[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_KNA1
      FROM KNA1
       FOR ALL ENTRIES IN IT_BSXD_C
     WHERE KUNNR EQ IT_BSXD_C-KUNNR.
    SORT IT_KNA1 BY KUNNR.
  ENDIF.

  SELECT * INTO TABLE IT_TBSL
    FROM TBSL.

  SORT IT_TBSL BY BSCHL.

  LOOP AT IT_BSXK_C.
    CLEAR: IT_BSXX_ALV.
    MOVE-CORRESPONDING IT_BSXK_C TO IT_BSXX_ALV.
    IT_BSXX_ALV-VINCULAR = ICON_CHECKED.
    IT_BSXX_ALV-PARID    = IT_BSXK_C-LIFNR.

    READ TABLE IT_LFA1 WITH KEY LIFNR = IT_BSXK_C-LIFNR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_BSXX_ALV-NAME1 = IT_LFA1-NAME1.
    ENDIF.

    READ TABLE IT_TBSL WITH KEY BSCHL = IT_BSXK_C-BSCHL BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_BSXX_ALV-KOART = IT_TBSL-KOART.
    ENDIF.

    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBTR.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-WRBTR.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBE2.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBE3.

    APPEND IT_BSXX_ALV.
  ENDLOOP.

  LOOP AT IT_BSXD_C.
    CLEAR: IT_BSXX_ALV.
    MOVE-CORRESPONDING IT_BSXD_C TO IT_BSXX_ALV.
    IT_BSXX_ALV-VINCULAR = ICON_CHECKED.
    IT_BSXX_ALV-PARID    = IT_BSXD_C-KUNNR.

    READ TABLE IT_KNA1 WITH KEY KUNNR = IT_BSXD_C-KUNNR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_BSXX_ALV-NAME1 = IT_KNA1-NAME1.
    ENDIF.

    READ TABLE IT_TBSL WITH KEY BSCHL = IT_BSXD_C-BSCHL BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_BSXX_ALV-KOART = IT_TBSL-KOART.
    ENDIF.

    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBTR.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-WRBTR.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBE2.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV-KOART IT_BSXX_ALV-SHKZG CHANGING IT_BSXX_ALV-DMBE3.

    APPEND IT_BSXX_ALV.
  ENDLOOP.

  "Limpar Documentos Vinculados
  PERFORM LIMPAR_DOCUMENTOS_VINCULADOS.

ENDFORM.                    " PESQUISAR_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  EXECUTAR_COMPENSACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXECUTAR_COMPENSACAO .
  PERFORM VERIFICAR_COMPENSACAO.
ENDFORM.                    " EXECUTAR_COMPENSACAO

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_4003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_4003 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_4003> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_BSXX_COMP_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_4003.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_4003 ASSIGNING <FS_CAT_4003>.
    <FS_CAT_4003>-COL_POS = LC_COL_POS.
    <FS_CAT_4003>-TABNAME = 'IT_BSXX_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_4003>-FIELDNAME.
      WHEN 'VINCULAR'.
        <FS_CAT_4003>-HOTSPOT   = ABAP_TRUE.
        <FS_CAT_4003>-JUST      = 'C'.
      WHEN 'BELNR'.
        <FS_CAT_4003>-HOTSPOT   = ABAP_TRUE.
*      WHEN 'CK_PAYMENT'.
*        <FS_CAT_4003>-CHECKBOX  = ABAP_TRUE.
*        <FS_CAT_4003>-EDIT      = ABAP_TRUE.
*        <FS_CAT_4003>-OUTPUTLEN = 03.
*        CLEAR: <FS_CAT_4003>-COLTEXT,
*               <FS_CAT_4003>-SCRTEXT_L,
*               <FS_CAT_4003>-SCRTEXT_M,
*               <FS_CAT_4003>-SCRTEXT_S.
*        "WHEN 'DATA_RESIDUAL'.
*        "  <FS_CAT_4003>-EDIT      = ABAP_TRUE.
*      WHEN 'VALOR_PAYMENTS'.
*        <FS_CAT_4003>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_4003>-OUTPUTLEN = 15.
*        <FS_CAT_4003>-EDIT      = ABAP_TRUE.
*      WHEN 'ZUONR'.
*        <FS_CAT_4003>-OUTPUTLEN = 10.
*      WHEN 'BUKRS' OR 'SHKZG' OR 'PARID_KOART'.
*        <FS_CAT_4003>-NO_OUT    = ABAP_TRUE.
*      WHEN 'GJAHR'.
*        <FS_CAT_4003>-NO_OUT    = ABAP_TRUE.
*      WHEN 'DMBTRH' OR 'DMBTRS' OR 'WRBTR' OR 'VALOR_RESIDUAL'.
*        "<FS_CAT_4003>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_4003>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_4003

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_4003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_4003 .

  GS_VAR_4003-REPORT      = SY-REPID.
  GS_VAR_4003-HANDLE      = '4003'.
  GS_VAR_4003-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_4003-USERNAME    = ABAP_FALSE.
  GS_VAR_4003-VARIANT     = ABAP_FALSE.
  GS_VAR_4003-TEXT        = ABAP_FALSE.
  GS_VAR_4003-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_4003

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_4004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_4004 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_4004> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_BSXX_COMP_ALV_C'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_4004.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_4004 ASSIGNING <FS_CAT_4004>.
    <FS_CAT_4004>-COL_POS = LC_COL_POS.
    <FS_CAT_4004>-TABNAME = 'IT_BSXX_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_4004>-FIELDNAME.
      WHEN 'VINCULAR'.
        <FS_CAT_4004>-HOTSPOT   = ABAP_TRUE.
        <FS_CAT_4004>-JUST      = 'C'.
      WHEN 'BELNR'.
        <FS_CAT_4004>-HOTSPOT   = ABAP_TRUE.
      WHEN 'VALOR_PAYMENTS'.
        <FS_CAT_4004>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_4004>-OUTPUTLEN = 15.
        <FS_CAT_4004>-EDIT      = ABAP_TRUE.
      WHEN 'VALOR_RESIDUAL'.
        <FS_CAT_4004>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_4004>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_4004

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_4004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_4004 .

  GS_VAR_4004-REPORT      = SY-REPID.
  GS_VAR_4004-HANDLE      = '4004'.
  GS_VAR_4004-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_4004-USERNAME    = ABAP_FALSE.
  GS_VAR_4004-VARIANT     = ABAP_FALSE.
  GS_VAR_4004-TEXT        = ABAP_FALSE.
  GS_VAR_4004-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_4004

*&---------------------------------------------------------------------*
*&      Module  STATUS_4006 OUTPUT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4006 OUTPUT.

  IF G_CUSTOM_4006 IS INITIAL.

* create a container for the tree control
    CREATE OBJECT G_CUSTOM_4006
      EXPORTING
        CONTAINER_NAME              = 'AVLS_CONTAINER2'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_DYNDOC_4006
      EXPORTING
        STYLE = 'ALV_GRID_4006'.

    CREATE OBJECT DG_SPLITTER_4006
      EXPORTING
        PARENT  = G_CUSTOM_4006
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_4006->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CON_4006.

    CALL METHOD DG_SPLITTER_4006->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CON_4007.

    CREATE OBJECT CTL_ALV_4006
      EXPORTING
        I_PARENT = CTL_CON_4006.

    CREATE OBJECT OBG_TOOLBAR_4006
      EXPORTING
        IO_ALV_GRID = CTL_ALV_4006.

    SET HANDLER OBG_TOOLBAR_4006->ON_TOOLBAR FOR CTL_ALV_4006.
    SET HANDLER OBG_TOOLBAR_4006->HANDLE_USER_COMMAND FOR CTL_ALV_4006.

    PERFORM FILL_IT_FIELDCATALOG_4006.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_4006.
*   Set layout parameters for ALV grid
*    GS_LAY_4006-GRID_TITLE = TEXT-010.
    GS_LAY_4006-SEL_MODE   = 'A'.
    GS_LAY_4006-ZEBRA      = 'X'.
    GS_LAY_4006-EDIT_MODE  = ABAP_FALSE.

    CALL METHOD CTL_ALV_4006->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_4006
        IS_VARIANT           = GS_VAR_4006
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_4006
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_4006        "IT_EXCEPT_QINFO = IT_HINTS
        IT_OUTTAB            = IT_BSXX_ALV[].
*        IT_SORT              = GT_SORT[].

    CREATE OBJECT EVENT_HANDLER_4006.
    SET HANDLER EVENT_HANDLER_4006->HANDLE_HOTSPOT_CLICK  FOR CTL_ALV_4006.
    SET HANDLER EVENT_HANDLER_4006->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_4006.

    CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT CTL_ALV_4007
      EXPORTING
        I_PARENT = CTL_CON_4007.

    CREATE OBJECT OBG_TOOLBAR_4007
      EXPORTING
        IO_ALV_GRID = CTL_ALV_4007.

    SET HANDLER OBG_TOOLBAR_4007->ON_TOOLBAR FOR CTL_ALV_4007.
    SET HANDLER OBG_TOOLBAR_4007->HANDLE_USER_COMMAND FOR CTL_ALV_4007.

    PERFORM FILL_IT_FIELDCATALOG_4007.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_4007.
*   Set layout parameters for ALV grid
*    GS_LAY_4006-GRID_TITLE = TEXT-010.
    GS_LAY_4007-SEL_MODE   = 'A'.
    GS_LAY_4007-ZEBRA      = 'X'.
    GS_LAY_4007-EDIT_MODE  = ABAP_TRUE.
    GS_LAY_4007-NO_TOTLINE = ABAP_TRUE.

    APPEND '&LOCAL&CUT'           TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&INSERT_ROW'    TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&MOVE_ROW'      TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&PASTE'         TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&UNDO'          TO IT_EXCLUDE_4007.
    APPEND '&VARI_ADMIN'          TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&APPEND'        TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&COPY'          TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&COPY_ROW'      TO IT_EXCLUDE_4007.
    APPEND '&VLOTUS'              TO IT_EXCLUDE_4007.
    APPEND '&AQW'                 TO IT_EXCLUDE_4007.
    APPEND '&PRINT'               TO IT_EXCLUDE_4007.
    APPEND '&MB_SUM'              TO IT_EXCLUDE_4007.
    APPEND '&AVERAGE'             TO IT_EXCLUDE_4007.
    APPEND '&MB_VIEW'             TO IT_EXCLUDE_4007.
    APPEND '&MB_EXPORT'           TO IT_EXCLUDE_4007.
    APPEND '&MB_FILTER'           TO IT_EXCLUDE_4007.
    APPEND '&GRAPH'               TO IT_EXCLUDE_4007.
    APPEND '&INFO'                TO IT_EXCLUDE_4007.
    APPEND '&LOCAL&DELETE_ROW'    TO IT_EXCLUDE_4007.
    APPEND '&CHECK'               TO IT_EXCLUDE_4007.

    CLEAR: GT_SORT.

    CLEAR: FS_SORT.
    FS_SORT-SPOS       = 1.          "first sorting key
    FS_SORT-FIELDNAME  = 'CONTROLE'. "fieldname for sort
    FS_SORT-UP         = 'X'. "sort ascending
    FS_SORT-SUBTOT     = 'X'. "do subtotal
    FS_SORT-NO_OUT     = 'X'. "no display
    FS_SORT-OBLIGATORY = 'X'. "sort is obligatory
    INSERT FS_SORT INTO TABLE GT_SORT. "insert to sort table

    CALL METHOD CTL_ALV_4007->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_4007
        IS_VARIANT           = GS_VAR_4007
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_4007
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_4007        "IT_EXCEPT_QINFO = IT_HINTS
        IT_OUTTAB            = IT_BSXX_ALV_C[]
        IT_SORT              = GT_SORT[].

    CALL METHOD CTL_ALV_4007->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD CTL_ALV_4007->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CREATE OBJECT EVENT_HANDLER_4007.
    SET HANDLER EVENT_HANDLER_4007->HANDLE_HOTSPOT_CLICK  FOR CTL_ALV_4007.
    SET HANDLER EVENT_HANDLER_4007->HANDLE_DOUBLE_CLICK   FOR CTL_ALV_4007.
    SET HANDLER EVENT_HANDLER_4007->DATA_CHANGED_FINISHED FOR CTL_ALV_4007.
    SET HANDLER EVENT_HANDLER_4007->DATA_CHANGED          FOR CTL_ALV_4007.
    SET HANDLER EVENT_HANDLER_4007->SUBTOTAL_TEXT         FOR CTL_ALV_4007.

    CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY.

  ELSE.
    WA_STABLE-ROW = ABAP_TRUE.
    WA_STABLE-COL = ABAP_TRUE.
    CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  CALL METHOD CTL_ALV_4006->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_4006
      ES_ROW_NO   = GS_SCROLL_ROW_4006.

  CALL METHOD CTL_ALV_4007->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_4007
      ES_ROW_NO   = GS_SCROLL_ROW_4007.

ENDMODULE.                 " STATUS_4005  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_4006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_4006 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_4006> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_BSXX_COMP_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_4006.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_4006 ASSIGNING <FS_CAT_4006>.
    <FS_CAT_4006>-COL_POS = LC_COL_POS.
    <FS_CAT_4006>-TABNAME = 'IT_BSXX_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_4006>-FIELDNAME.
      WHEN 'VINCULAR'.
        <FS_CAT_4006>-HOTSPOT   = ABAP_TRUE.
        <FS_CAT_4006>-JUST      = 'C'.
      WHEN 'BELNR'.
        <FS_CAT_4006>-HOTSPOT   = ABAP_TRUE.
*        <FS_CAT_4006>-CHECKBOX  = ABAP_TRUE.
*        <FS_CAT_4006>-EDIT      = ABAP_TRUE.
*        <FS_CAT_4006>-OUTPUTLEN = 03.
*        CLEAR: <FS_CAT_4006>-COLTEXT,
*               <FS_CAT_4006>-SCRTEXT_L,
*               <FS_CAT_4006>-SCRTEXT_M,
*               <FS_CAT_4006>-SCRTEXT_S.
*        "WHEN 'DATA_RESIDUAL'.
*        "  <FS_CAT_4006>-EDIT      = ABAP_TRUE.
*      WHEN 'VALOR_PAYMENTS'.
*        <FS_CAT_4006>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_4006>-OUTPUTLEN = 15.
*        <FS_CAT_4006>-EDIT      = ABAP_TRUE.
*      WHEN 'ZUONR'.
*        <FS_CAT_4006>-OUTPUTLEN = 10.
*      WHEN 'BUKRS' OR 'SHKZG' OR 'PARID_KOART'.
*        <FS_CAT_4006>-NO_OUT    = ABAP_TRUE.
*      WHEN 'GJAHR'.
*        <FS_CAT_4006>-NO_OUT    = ABAP_TRUE.
*      WHEN 'DMBTRH' OR 'DMBTRS' OR 'WRBTR' OR 'VALOR_RESIDUAL'.
*        "<FS_CAT_4006>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_4006>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_4005

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_4006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_4006 .

  GS_VAR_4006-REPORT      = SY-REPID.
  GS_VAR_4006-HANDLE      = '4006'.
  GS_VAR_4006-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_4006-USERNAME    = ABAP_FALSE.
  GS_VAR_4006-VARIANT     = ABAP_FALSE.
  GS_VAR_4006-TEXT        = ABAP_FALSE.
  GS_VAR_4006-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_4005

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_4007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_4007 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_4007> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_BSXX_COMP_ALV_C'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_4007.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_4007 ASSIGNING <FS_CAT_4007>.
    <FS_CAT_4007>-COL_POS = LC_COL_POS.
    <FS_CAT_4007>-TABNAME = 'IT_BSXX_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_4007>-FIELDNAME.
      WHEN 'VINCULAR'.
        <FS_CAT_4007>-HOTSPOT   = ABAP_TRUE.
        <FS_CAT_4007>-JUST      = 'C'.
      WHEN 'BELNR'.
        <FS_CAT_4007>-HOTSPOT   = ABAP_TRUE.
      WHEN 'VALOR_PAYMENTS'.
        <FS_CAT_4007>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_4007>-OUTPUTLEN = 15.
        <FS_CAT_4007>-EDIT      = ABAP_TRUE.
      WHEN 'VALOR_RESIDUAL'.
        <FS_CAT_4007>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_4007>-OUTPUTLEN = 15.
      WHEN 'CONTROLE'.
        <FS_CAT_4007>-TECH      = ABAP_TRUE.
        <FS_CAT_4007>-NO_OUT    = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_4007

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_4007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_4007 .

  GS_VAR_4007-REPORT      = SY-REPID.
  GS_VAR_4007-HANDLE      = '4007'.
  GS_VAR_4007-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_4007-USERNAME    = ABAP_FALSE.
  GS_VAR_4007-VARIANT     = ABAP_FALSE.
  GS_VAR_4007-TEXT        = ABAP_FALSE.
  GS_VAR_4007-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_4007

*&---------------------------------------------------------------------*
*&      Module  STATUS_4005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4005 OUTPUT.

  IF G_CUSTOM_4003 IS INITIAL.

* create a container for the tree control
    CREATE OBJECT G_CUSTOM_4003
      EXPORTING
        CONTAINER_NAME              = 'AVLS_CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_DYNDOC_4003
      EXPORTING
        STYLE = 'ALV_GRID_4003'.

    CREATE OBJECT DG_SPLITTER_4003
      EXPORTING
        PARENT  = G_CUSTOM_4003
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_4003->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CON_4003.

    CALL METHOD DG_SPLITTER_4003->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CON_4004.

    CALL METHOD DG_SPLITTER_4003->SET_ROW_HEIGHT
      EXPORTING
        ID     = 2
        HEIGHT = 40.

    CREATE OBJECT CTL_ALV_4003
      EXPORTING
        I_PARENT = CTL_CON_4003.

    CREATE OBJECT OBG_TOOLBAR_4003
      EXPORTING
        IO_ALV_GRID = CTL_ALV_4003.
*
    SET HANDLER OBG_TOOLBAR_4003->ON_TOOLBAR FOR CTL_ALV_4003.
    SET HANDLER OBG_TOOLBAR_4003->HANDLE_USER_COMMAND FOR CTL_ALV_4003.

    PERFORM FILL_IT_FIELDCATALOG_4003.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_4003.
*   Set layout parameters for ALV grid
*    GS_LAY_4003-GRID_TITLE = TEXT-010.
    GS_LAY_4003-SEL_MODE   = 'A'.
    GS_LAY_4003-ZEBRA      = 'X'.
    GS_LAY_4003-EDIT_MODE  = ABAP_FALSE.

    CALL METHOD CTL_ALV_4003->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_4003
        IS_VARIANT           = GS_VAR_4003
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_4003
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_4003        "IT_EXCEPT_QINFO = IT_HINTS
        IT_OUTTAB            = IT_BSXX_ALV[].
*        IT_SORT              = GT_SORT[].

    CREATE OBJECT EVENT_HANDLER_4003.
    SET HANDLER EVENT_HANDLER_4003->HANDLE_HOTSPOT_CLICK  FOR CTL_ALV_4003.
    SET HANDLER EVENT_HANDLER_4003->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_4003.

    CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT CTL_ALV_4004
      EXPORTING
        I_PARENT = CTL_CON_4004.

    CREATE OBJECT OBG_TOOLBAR_4004
      EXPORTING
        IO_ALV_GRID = CTL_ALV_4004.

    SET HANDLER OBG_TOOLBAR_4004->ON_TOOLBAR FOR CTL_ALV_4004.
    SET HANDLER OBG_TOOLBAR_4004->HANDLE_USER_COMMAND FOR CTL_ALV_4004.

    PERFORM FILL_IT_FIELDCATALOG_4004.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_4004.
*   Set layout parameters for ALV grid
*    GS_LAY_4003-GRID_TITLE = TEXT-010.
    GS_LAY_4004-SEL_MODE   = 'A'.
    GS_LAY_4004-ZEBRA      = 'X'.
    GS_LAY_4004-EDIT_MODE  = ABAP_TRUE.

    APPEND '&LOCAL&CUT'           TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&INSERT_ROW'    TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&MOVE_ROW'      TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&PASTE'         TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&UNDO'          TO IT_EXCLUDE_4004.
    APPEND '&VARI_ADMIN'          TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&APPEND'        TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&COPY'          TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&COPY_ROW'      TO IT_EXCLUDE_4004.
    APPEND '&VLOTUS'              TO IT_EXCLUDE_4004.
    APPEND '&AQW'                 TO IT_EXCLUDE_4004.
    APPEND '&PRINT'               TO IT_EXCLUDE_4004.
    APPEND '&MB_SUM'              TO IT_EXCLUDE_4004.
    APPEND '&AVERAGE'             TO IT_EXCLUDE_4004.
    APPEND '&MB_VIEW'             TO IT_EXCLUDE_4004.
    APPEND '&MB_EXPORT'           TO IT_EXCLUDE_4004.
    APPEND '&MB_FILTER'           TO IT_EXCLUDE_4004.
    APPEND '&GRAPH'               TO IT_EXCLUDE_4004.
    APPEND '&INFO'                TO IT_EXCLUDE_4004.
    APPEND '&LOCAL&DELETE_ROW'    TO IT_EXCLUDE_4004.
    APPEND '&CHECK'               TO IT_EXCLUDE_4004.

    FS_SORT-SPOS       = 1.     "first sorting key
    FS_SORT-FIELDNAME  = 'BUKRS'. "fieldname for sort
    FS_SORT-UP         = 'X'. "sort ascending
    FS_SORT-SUBTOT     = 'X'. "do subtotal
    FS_SORT-NO_OUT     = 'X'. "no display
    FS_SORT-OBLIGATORY = 'X'. "sort is obligatory
    INSERT FS_SORT INTO TABLE GT_SORT. "insert to sort table

    CLEAR: FS_SORT.
    FS_SORT-SPOS       = 2.       "first sorting key
    FS_SORT-FIELDNAME  = 'WAERS'. "fieldname for sort
    FS_SORT-UP         = 'X'.     "sort ascending
    INSERT FS_SORT INTO TABLE GT_SORT. "insert to sort table

    CALL METHOD CTL_ALV_4004->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_4004
        IS_VARIANT           = GS_VAR_4004
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_4004
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_4004        "IT_EXCEPT_QINFO = IT_HINTS
        IT_OUTTAB            = IT_BSXX_ALV_C[]
        IT_SORT              = GT_SORT[].

    CALL METHOD CTL_ALV_4004->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD CTL_ALV_4004->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CREATE OBJECT EVENT_HANDLER_4004.
    SET HANDLER EVENT_HANDLER_4004->HANDLE_HOTSPOT_CLICK  FOR CTL_ALV_4004.
    SET HANDLER EVENT_HANDLER_4004->HANDLE_DOUBLE_CLICK   FOR CTL_ALV_4004.
    SET HANDLER EVENT_HANDLER_4004->DATA_CHANGED_FINISHED FOR CTL_ALV_4004.
    SET HANDLER EVENT_HANDLER_4004->DATA_CHANGED          FOR CTL_ALV_4004.
    SET HANDLER EVENT_HANDLER_4004->SUBTOTAL_TEXT         FOR CTL_ALV_4004.

    CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY.

  ELSE.
    WA_STABLE-ROW = ABAP_TRUE.
    WA_STABLE-COL = ABAP_TRUE.
    CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  CALL METHOD CTL_ALV_4003->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_4003
      ES_ROW_NO   = GS_SCROLL_ROW_4003.

  CALL METHOD CTL_ALV_4004->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_4004
      ES_ROW_NO   = GS_SCROLL_ROW_4004.

ENDMODULE.                 " STATUS_4005  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  TOTALIZA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOTALIZAR_SALDO .

  DATA: LC_WRBTR TYPE	WRBTR.

  CLEAR: ZDE_BSXX_COMP.

  ZDE_BSXX_COMP-VALOR_PAGAR   = 0.
  ZDE_BSXX_COMP-VALOR_RECEBER = 0.
  ZDE_BSXX_COMP-VALOR_SALDO   = 0.

  LOOP AT IT_BSXX_ALV_C.
    ZDE_BSXX_COMP-MOEDA  = IT_BSXX_ALV_C-WAERS.
    LC_WRBTR             = IT_BSXX_ALV_C-VALOR_PAYMENTS.
    PERFORM SINAL_VALOR USING IT_BSXX_ALV_C-KOART IT_BSXX_ALV_C-SHKZG CHANGING LC_WRBTR.
    IF LC_WRBTR LT 0.
      ADD LC_WRBTR TO ZDE_BSXX_COMP-VALOR_PAGAR.
    ELSE.
      ADD LC_WRBTR TO ZDE_BSXX_COMP-VALOR_RECEBER.
    ENDIF.
    ADD LC_WRBTR TO ZDE_BSXX_COMP-VALOR_SALDO.
  ENDLOOP.

ENDFORM.                    " TOTALIZA_SALDO

*&---------------------------------------------------------------------*
*&      Form  SINAL_VALOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SINAL_VALOR  USING    P_KOART TYPE KOART
                           P_SHKZG TYPE SHKZG
                  CHANGING P_DMBTR.

*H  Crédito
*S  Débito

  IF P_KOART EQ 'K' AND P_SHKZG EQ 'H'.
    P_DMBTR = P_DMBTR * -1.
  ELSEIF P_KOART EQ 'D' AND P_SHKZG EQ 'S'.
    P_DMBTR = P_DMBTR * -1.
  ENDIF.

ENDFORM.                    " SINAL_VALOR

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_4006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_4006
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  DATA: WA_BSXX_ALV TYPE ZDE_BSXX_COMP_ALV.

  READ TABLE IT_BSXX_ALV INDEX ROW_ID INTO WA_BSXX_ALV.

  CASE FIELDNAME.
    WHEN 'VINCULAR'.
      PERFORM VINCULAR_ITEM USING WA_BSXX_ALV ABAP_TRUE.
    WHEN 'BELNR'.
      PERFORM CHAMA_TELA_FB03 USING WA_BSXX_ALV-BUKRS WA_BSXX_ALV-GJAHR WA_BSXX_ALV-BELNR.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_4006

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BSXX_ALV  text
*----------------------------------------------------------------------*
FORM VINCULAR_ITEM  USING P_BSXX TYPE ZDE_BSXX_COMP_ALV P_ATUALIZA_TELA TYPE CHAR01.

  DATA: WA_BSXX_ALV_C TYPE ZDE_BSXX_COMP_ALV_C.

  CLEAR: WA_BSXX_ALV_C.
  READ TABLE IT_BSXX_ALV_C INTO WA_BSXX_ALV_C INDEX 1.

  READ TABLE IT_BSXX_ALV WITH KEY BUKRS = P_BSXX-BUKRS
                                  GJAHR = P_BSXX-GJAHR
                                  BELNR = P_BSXX-BELNR
                                  BUZEI = P_BSXX-BUZEI.
  IF SY-SUBRC IS INITIAL.

    IF ( WA_BSXX_ALV_C IS NOT INITIAL ) AND ( WA_BSXX_ALV_C-WAERS NE IT_BSXX_ALV-WAERS ).
      MESSAGE S053 WITH WA_BSXX_ALV_C-WAERS.
      EXIT.
    ENDIF.

    IF ( WA_BSXX_ALV_C IS NOT INITIAL ) AND ( WA_BSXX_ALV_C-BUKRS NE IT_BSXX_ALV-BUKRS ).
      MESSAGE S054 WITH WA_BSXX_ALV_C-BUKRS.
      EXIT.
    ENDIF.

    IF P_ATUALIZA_TELA EQ ABAP_TRUE.
      DELETE IT_BSXX_ALV INDEX SY-TABIX.
    ENDIF.

    MOVE-CORRESPONDING P_BSXX TO IT_BSXX_ALV_C.
    CLEAR:
    IT_BSXX_ALV_C-VINCULAR,
    IT_BSXX_ALV_C-VALOR_PAYMENTS,
    IT_BSXX_ALV_C-VALOR_RESIDUAL,
    IT_BSXX_ALV_C-DATA_RESIDUAL,
    IT_BSXX_ALV_C-SGTXT_RES.
    IT_BSXX_ALV_C-VINCULAR       = ICON_INCOMPLETE.
    IT_BSXX_ALV_C-VALOR_PAYMENTS = ABS( IT_BSXX_ALV_C-WRBTR ).
    APPEND IT_BSXX_ALV_C.
  ENDIF.

  IF P_ATUALIZA_TELA EQ ABAP_TRUE.

    WA_STABLE-ROW = ABAP_TRUE.
    WA_STABLE-COL = ABAP_TRUE.

    IF TL_400X EQ TL_4001.
      CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ELSE.
      CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.

    LEAVE TO SCREEN 0004.

  ENDIF.

ENDFORM.                    " VINCULAR_ITEM

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_4006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_4006  USING P_ROW TYPE LVC_S_ROW.

  DATA: WA_BSXX_ALV TYPE ZDE_BSXX_COMP_ALV.

  CHECK P_ROW-ROWTYPE IS INITIAL.

  READ TABLE IT_BSXX_ALV INDEX P_ROW-INDEX INTO WA_BSXX_ALV.

  PERFORM VINCULAR_ITEM USING WA_BSXX_ALV  ABAP_TRUE.

ENDFORM.                    " HANDLE_DOUBLE_CLICK_4006

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_4007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_4007
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  DATA: WA_BSXX_ALV TYPE ZDE_BSXX_COMP_ALV_C.

  READ TABLE IT_BSXX_ALV_C INDEX ROW_ID INTO WA_BSXX_ALV.

  CASE FIELDNAME.
    WHEN 'VINCULAR'.
      PERFORM DESVINCULAR_ITEM USING WA_BSXX_ALV ABAP_TRUE.
    WHEN 'BELNR'.
      PERFORM CHAMA_TELA_FB03 USING WA_BSXX_ALV-BUKRS WA_BSXX_ALV-GJAHR WA_BSXX_ALV-BELNR.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_4007

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BSXX_ALV_C  text
*----------------------------------------------------------------------*
FORM DESVINCULAR_ITEM  USING P_BSXX TYPE ZDE_BSXX_COMP_ALV_C  P_ATUALIZA_TELA TYPE CHAR01.

  READ TABLE IT_BSXX_ALV_C WITH KEY BUKRS = P_BSXX-BUKRS
                                    GJAHR = P_BSXX-GJAHR
                                    BELNR = P_BSXX-BELNR
                                    BUZEI = P_BSXX-BUZEI.
  IF SY-SUBRC IS INITIAL.
    IF P_ATUALIZA_TELA EQ ABAP_TRUE.
      DELETE IT_BSXX_ALV_C INDEX SY-TABIX.
    ENDIF.

    MOVE-CORRESPONDING P_BSXX TO IT_BSXX_ALV.
    IT_BSXX_ALV-VINCULAR = ICON_CHECKED.
    APPEND IT_BSXX_ALV.
  ENDIF.

  IF P_ATUALIZA_TELA EQ ABAP_TRUE.
    WA_STABLE-ROW = ABAP_TRUE.
    WA_STABLE-COL = ABAP_TRUE.

    IF TL_400X EQ TL_4001.
      CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ELSE.
      CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.

      CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.

    LEAVE TO SCREEN 0004.
  ENDIF.

ENDFORM.                    " DESVINCULAR_ITEM

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_4007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_4007  USING P_ROW TYPE LVC_S_ROW.

  DATA: WA_BSXX_ALV TYPE ZDE_BSXX_COMP_ALV_C.

  CHECK P_ROW-ROWTYPE IS INITIAL.

  READ TABLE IT_BSXX_ALV_C INDEX P_ROW-INDEX INTO WA_BSXX_ALV.

  PERFORM DESVINCULAR_ITEM USING WA_BSXX_ALV ABAP_TRUE.

ENDFORM.                    " HANDLE_DOUBLE_CLICK_4007

*&---------------------------------------------------------------------*
*&      Module  STATUS_4008  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4008 OUTPUT.
  SET PF-STATUS 'PF4008'.
  SET TITLEBAR 'TL4008'.
ENDMODULE.                 " STATUS_4008  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4008 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMA'.
      CLEAR: OK_CODE.
      PDATA = BSIS-BUDAT.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_4008  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4008_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4008_EXIT INPUT.
  CLEAR: OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_4008_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_4007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4007 OUTPUT.

  DATA: ICON_NAME(20) TYPE C,
        ICON_TEXT(20) TYPE C.

  PERFORM VERIFICAR_COMPENSACAO.

  IF ZDE_BSXX_COMP-ICONE EQ ICON_LED_RED.
    ICON_NAME = 'ICON_LED_RED'.
    ICON_TEXT = TEXT-014.
  ELSEIF ZDE_BSXX_COMP-ICONE EQ ICON_LED_GREEN.
    ICON_NAME = 'ICON_LED_GREEN'.
    ICON_TEXT = TEXT-015.
  ELSEIF ZDE_BSXX_COMP-ICONE EQ ICON_LED_YELLOW.
    ICON_NAME = 'ICON_LED_YELLOW'.
    ICON_TEXT = TEXT-016.
  ENDIF.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME                  = ICON_NAME
      TEXT                  = ICON_TEXT
      INFO                  = 'Status'
      ADD_STDINF            = 'X'
    IMPORTING
      RESULT                = LC_ICO_SALDO
    EXCEPTIONS
      ICON_NOT_FOUND        = 1
      OUTPUTFIELD_TOO_SHORT = 2
      OTHERS                = 3.

ENDMODULE.                 " STATUS_4007  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_ITENS .

  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW,
        WA_BSXX_ALV      TYPE ZDE_BSXX_COMP_ALV.

  IF TL_400X EQ TL_4001.
    CALL METHOD CTL_ALV_4003->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS.
  ELSE.
    CALL METHOD CTL_ALV_4006->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS.
  ENDIF.

  CHECK IT_SELECTED_ROWS IS NOT INITIAL.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_BSXX_ALV INTO WA_BSXX_ALV INDEX WA_SELECTED_ROWS-INDEX.
    IF SY-SUBRC IS INITIAL.
      PERFORM VINCULAR_ITEM USING WA_BSXX_ALV ABAP_FALSE.
    ENDIF.
  ENDLOOP.

  PERFORM LIMPAR_DOCUMENTOS_VINCULADOS.

  WA_STABLE-ROW = ABAP_TRUE.
  WA_STABLE-COL = ABAP_TRUE.

  IF TL_400X EQ TL_4001.
    CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ELSE.
    CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  LEAVE TO SCREEN 0004.

ENDFORM.                    " VINCULAR_ITENS

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_DOCUMENTOS_VINCULADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LIMPAR_DOCUMENTOS_VINCULADOS .

  LOOP AT IT_BSXX_ALV_C.
    DELETE IT_BSXX_ALV WHERE BUKRS EQ IT_BSXX_ALV_C-BUKRS
                         AND GJAHR EQ IT_BSXX_ALV_C-GJAHR
                         AND BELNR EQ IT_BSXX_ALV_C-BELNR
                         AND BUZEI EQ IT_BSXX_ALV_C-BUZEI.
  ENDLOOP.

ENDFORM.                    " LIMPAR_DOCUMENTOS_VINCULADOS

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_ITENS .

  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW,
        WA_BSXX_ALV_C    TYPE ZDE_BSXX_COMP_ALV_C,
        IT_BSXX_ALV_C_B  TYPE TABLE OF ZDE_BSXX_COMP_ALV_C WITH HEADER LINE.

  IF TL_400X EQ TL_4001.
    CALL METHOD CTL_ALV_4004->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS.
  ELSE.
    CALL METHOD CTL_ALV_4007->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS.
  ENDIF.

  CHECK IT_SELECTED_ROWS IS NOT INITIAL.

  CLEAR: IT_BSXX_ALV_C_B[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_BSXX_ALV_C INTO WA_BSXX_ALV_C INDEX WA_SELECTED_ROWS-INDEX.
    IF SY-SUBRC IS INITIAL.
      APPEND WA_BSXX_ALV_C TO IT_BSXX_ALV_C_B.
      PERFORM DESVINCULAR_ITEM USING WA_BSXX_ALV_C ABAP_FALSE.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_BSXX_ALV_C_B INTO WA_BSXX_ALV_C.
    DELETE IT_BSXX_ALV_C WHERE BUKRS EQ WA_BSXX_ALV_C-BUKRS
                           AND GJAHR EQ WA_BSXX_ALV_C-GJAHR
                           AND BELNR EQ WA_BSXX_ALV_C-BELNR
                           AND BUZEI EQ WA_BSXX_ALV_C-BUZEI.
  ENDLOOP.

  WA_STABLE-ROW = ABAP_TRUE.
  WA_STABLE-COL = ABAP_TRUE.

  IF TL_400X EQ TL_4001.
    CALL METHOD CTL_ALV_4003->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4004->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ELSE.
    CALL METHOD CTL_ALV_4006->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CALL METHOD CTL_ALV_4007->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  LEAVE TO SCREEN 0004.

ENDFORM.                    " DESVINCULAR_ITENS
