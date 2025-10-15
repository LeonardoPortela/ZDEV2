*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_1130.
*----------------------------------------------------------------------*

DATA: CK_CONFIRMADO_1131 TYPE CHAR01.
DATA: CK_CONFIRMADO_1132 TYPE CHAR01.

CLASS LCL_ALV_TOOLBAR_1130 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,
      HANDLE_USER_COMMAND_11301 FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: CTL_ALV_1130       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_ALV_11301      TYPE REF TO CL_GUI_ALV_GRID,
      C_ALV_TOOLBAR_1130 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

DATA: IT_SELECTED_ROWS_1130  TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_1130  TYPE LVC_S_ROW,
      IT_SELECTED_ROWS_11301 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_11301 TYPE LVC_S_ROW.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_N55 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_1130 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_1130
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.

**    "Separador
*    CLEAR TY_TOOLBAR.
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*
*    "Selecionar Para Inclusão
*    CLEAR TY_TOOLBAR.
*    TY_TOOLBAR-ICON      = ICON_IMPORT.
*    TY_TOOLBAR-FUNCTION  = 'A++'.
*    TY_TOOLBAR-QUICKINFO = TEXT-004.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    IF CK_OPERACAO EQ 'C'.
*      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
*    ELSE.
*      TY_TOOLBAR-DISABLED  = ABAP_FALSE.
*    ENDIF.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    "Marcar Todos os Documentos
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = 'ADD'.
    TY_TOOLBAR-QUICKINFO = TEXT-005.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF ( CK_OPERACAO EQ 'C' )." OR ( CK_OPERACAO EQ 'A' AND ZDE_ZLEST0119_ALV-QTD_UTILIZADA NE 0 ).
      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
    ELSE.
      TY_TOOLBAR-DISABLED  = ABAP_FALSE.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    "Marcar Todos os Documentos
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = 'DEL'.
    TY_TOOLBAR-QUICKINFO = TEXT-006.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF ( CK_OPERACAO EQ 'C' ). "OR ( CK_OPERACAO EQ 'A' AND ZDE_ZLEST0119_ALV-QTD_UTILIZADA NE 0 ).
      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
    ELSE.
      TY_TOOLBAR-DISABLED  = ABAP_FALSE.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_1130->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CLEAR: IT_SELECTED_ROWS_1130, IT_SELECTED_ROWS_1130[].

    CALL METHOD CTL_ALV_1130->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS_1130.

    CLEAR: IT_ZLEST0120_SEL[].

    LOOP AT IT_SELECTED_ROWS_1130 INTO WA_SELECTED_ROWS_1130.
      READ TABLE IT_ZLEST0120_ALV INTO WA_ZLEST0120_ALV INDEX WA_SELECTED_ROWS_1130-INDEX.
      APPEND WA_ZLEST0120_ALV TO IT_ZLEST0120_SEL.
    ENDLOOP.

    CASE E_UCOMM.
      WHEN 'ADD'.
        PERFORM INCLUIR_EMPRESA.
      WHEN 'DEL'.
        PERFORM EXCLUIR_EMPRESA.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

  METHOD HANDLE_USER_COMMAND_11301.

    CLEAR: IT_SELECTED_ROWS_11301, IT_SELECTED_ROWS_11301[].

    CALL METHOD CTL_ALV_11301->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS_11301.

    CLEAR: IT_ZLEST0128_SEL[].

    LOOP AT IT_SELECTED_ROWS_11301 INTO WA_SELECTED_ROWS_11301.
      READ TABLE IT_ZLEST0128_ALV INTO WA_ZLEST0128_ALV INDEX WA_SELECTED_ROWS_11301-INDEX.
      APPEND WA_ZLEST0128_ALV TO IT_ZLEST0128_SEL.
    ENDLOOP.

    CASE E_UCOMM.
      WHEN 'ADD'.
        PERFORM INCLUIR_MATERIAL.
      WHEN 'DEL'.
        PERFORM EXCLUIR_MATERIAL.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command


ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION


DATA: OBG_TOOLBAR_1130  TYPE REF TO LCL_ALV_TOOLBAR_1130,
      OBG_TOOLBAR_11301 TYPE REF TO LCL_ALV_TOOLBAR_1130.

DATA: CTL_CON_1130  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_CON_11301 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GS_LAYOUT_1130        TYPE LVC_S_LAYO,
      GS_VARIANT_1130       TYPE DISVARIANT,
      IT_FIELDCATALOG_1130  TYPE LVC_T_FCAT,
      WA_FIELDCATALOG_1130  TYPE LVC_S_FCAT,
      IT_FIELDCATALOG_11301 TYPE LVC_T_FCAT,
      WA_FIELDCATALOG_11301 TYPE LVC_S_FCAT.

DATA: IT_EXCLUDE_FCODE_1130 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_1130 LIKE LINE OF IT_EXCLUDE_FCODE_1130.

DATA: GS_SCROLL_COL_1130  TYPE LVC_S_COL,
      GS_SCROLL_ROW_1130  TYPE LVC_S_ROID,
      GS_SCROLL_COL_11301 TYPE LVC_S_COL,
      GS_SCROLL_ROW_11301 TYPE LVC_S_ROID.

DATA: CK_ALTEROU_1130 TYPE CHAR01.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1130_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1130_EXIT INPUT.
  CLEAR: OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1130 INPUT.
  CK_ALTEROU_1130 = ABAP_TRUE.

  IF ZDE_ZLEST0119_ALV-QTD_NEGOCIADO NE 0 AND ZDE_ZLEST0119_ALV-PERC_TOLERANCIA NE 0.
    ZDE_ZLEST0119_ALV-QTD_TOLERANCIA = ZDE_ZLEST0119_ALV-QTD_NEGOCIADO * ( ZDE_ZLEST0119_ALV-PERC_TOLERANCIA / 100 ).
  ELSE.
    ZDE_ZLEST0119_ALV-QTD_TOLERANCIA = 0.
  ENDIF.

  ZDE_ZLEST0119_ALV-QTD_TOTAL = 0.
  ADD ZDE_ZLEST0119_ALV-QTD_NEGOCIADO  TO ZDE_ZLEST0119_ALV-QTD_TOTAL.
  ADD ZDE_ZLEST0119_ALV-QTD_TOLERANCIA TO ZDE_ZLEST0119_ALV-QTD_TOTAL.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_1130 INPUT.

  CALL METHOD CTL_ALV_1130->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_1130
      ES_ROW_NO   = GS_SCROLL_ROW_1130.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_1130 INPUT.
  PERFORM POPULA_SELECAO_DETALHE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_DETALHE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULA_SELECAO_DETALHE .

  CLEAR IT_SELECTED_ROWS_1130.

  CALL METHOD CTL_ALV_1130->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_1130.

  CLEAR IT_ZLEST0120_SEL[].

  LOOP AT IT_SELECTED_ROWS_1130 INTO WA_SELECTED_ROWS_1130.
    READ TABLE IT_ZLEST0120_ALV INTO WA_ZLEST0120_ALV INDEX WA_SELECTED_ROWS_1130-INDEX.
    MOVE-CORRESPONDING WA_ZLEST0120_ALV TO IT_ZLEST0120_SEL.
    APPEND IT_ZLEST0120_SEL.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1130 OUTPUT.

  DATA: IT_COMANDOS TYPE TABLE OF SY-UCOMM.

  CLEAR: IT_COMANDOS, IT_COMANDOS[].

  CASE CK_OPERACAO.
    WHEN 'I'.
      SET TITLEBAR 'TL1130' WITH TEXT-007.
    WHEN 'A'.
      SET TITLEBAR 'TL1130' WITH TEXT-008.
    WHEN 'C'.
      SET TITLEBAR 'TL1130' WITH TEXT-009.
      APPEND OK_SALVAR TO IT_COMANDOS.
  ENDCASE.

  SET PF-STATUS 'PF1130' EXCLUDING IT_COMANDOS.

  CK_ALTEROU_1130 = ABAP_FALSE.

  IF ZDE_ZLEST0119_ALV-LIFNR IS NOT INITIAL.
    SELECT SINGLE NAME1 INTO ZDE_ZLEST0119_ALV-NAME1 FROM LFA1 WHERE LIFNR = ZDE_ZLEST0119_ALV-LIFNR.
  ENDIF.

  CLEAR: ZDE_ZLEST0119_ALV-TEXT_ORIGEM,
         ZDE_ZLEST0119_ALV-TEXT_DESTINO.

  IF ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0119_ALV-TEXT_ORIGEM
      FROM J_1BTXJURT
     WHERE SPRAS   EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0119_ALV-PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM.
  ENDIF.

  IF ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0119_ALV-TEXT_DESTINO
      FROM J_1BTXJURT
     WHERE SPRAS   EQ SY-LANGU
       AND COUNTRY EQ ZDE_ZLEST0119_ALV-PAIS
       AND TAXJURCODE EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN.
  ENDIF.

  IF ZDE_ZLEST0119_ALV-MATNR IS NOT INITIAL.
    SELECT SINGLE MAKTX INTO ZDE_ZLEST0119_ALV-MAKTX
      FROM MAKT
     WHERE SPRAS   EQ SY-LANGU
       AND MATNR EQ ZDE_ZLEST0119_ALV-MATNR.
  ENDIF.

  IF GB_TP_MODAL IS NOT INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'ZDE_ZLEST0119_ALV-TP_MODAL'.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF CK_OPERACAO EQ 'I'.

    IF ZDE_ZLEST0119_ALV-CD_SEQ_LANC IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR = '01'
          OBJECT      = 'ZLES000001'
        IMPORTING
          NUMBER      = ZDE_ZLEST0119_ALV-CD_SEQ_LANC.
    ENDIF.

    IF GB_TP_MODAL IS INITIAL.
      LOOP AT SCREEN.
        IF SCREEN-NAME EQ 'ZDE_ZLEST0119_ALV-WAERK'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ZDE_ZLEST0115_ALV-DT_CADASTRO    = SY-DATUM.
    "ZDE_ZLEST0115_ALV-HR_CADASTRO    = SY-UZEIT.
    "ZDE_ZLEST0115_ALV-DS_US_CADASTRO = SY-UNAME.
  ENDIF.

  IF CK_OPERACAO EQ 'A' OR CK_OPERACAO EQ 'I'.
    "ZDE_ZLEST0115_ALV-DT_ATUALIZACAO    = SY-DATUM.
    "ZDE_ZLEST0115_ALV-HR_ATUALIZACAO    = SY-UZEIT.
    "ZDE_ZLEST0115_ALV-DS_US_ATUALIZACAO = SY-UNAME.
  ENDIF.

  IF CK_OPERACAO EQ 'C'.
    LOOP AT SCREEN.
      IF SCREEN-NAME(17) EQ 'ZDE_ZLEST0119_ALV'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF CK_OPERACAO EQ 'A' AND ZDE_ZLEST0119_ALV-QTD_UTILIZADA NE 0.
    LOOP AT SCREEN.
      IF SCREEN-NAME NE 'ZDE_ZLEST0119_ALV-WAERK' OR GB_TP_MODAL IS NOT INITIAL.
        IF SCREEN-GROUP1 EQ 'A1'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
                                                            "ALV1130
  "IT_ZLEST0116_ALV
  IF CTL_CON_1130 IS INITIAL.

    CREATE OBJECT CTL_CON_1130
      EXPORTING
        CONTAINER_NAME = 'ALV_1130'.

    CREATE OBJECT CTL_ALV_1130
      EXPORTING
        I_PARENT = CTL_CON_1130.

    CREATE OBJECT OBG_TOOLBAR_1130
      EXPORTING
        IO_ALV_GRID = CTL_ALV_1130.

    SET HANDLER OBG_TOOLBAR_1130->ON_TOOLBAR FOR CTL_ALV_1130.
    SET HANDLER OBG_TOOLBAR_1130->HANDLE_USER_COMMAND FOR CTL_ALV_1130.

    PERFORM FILL_IT_FIELDCATALOG_1130.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_1130.

    "GS_LAYOUT_1130-GRID_TITLE = TEXT-003.
    GS_LAYOUT_1130-SEL_MODE   = 'A'.

    CALL METHOD CTL_ALV_1130->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_1130
        IS_VARIANT           = GS_VARIANT_1130
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_1130
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_1130
        IT_OUTTAB            = IT_ZLEST0120_ALV[].

  ENDIF.

  IF CTL_CON_11301 IS INITIAL.

    CREATE OBJECT CTL_CON_11301
      EXPORTING
        CONTAINER_NAME = 'ALV_11301'.

    CREATE OBJECT CTL_ALV_11301
      EXPORTING
        I_PARENT = CTL_CON_11301.

    CREATE OBJECT OBG_TOOLBAR_11301
      EXPORTING
        IO_ALV_GRID = CTL_ALV_11301.

    SET HANDLER OBG_TOOLBAR_11301->ON_TOOLBAR FOR CTL_ALV_11301.
    SET HANDLER OBG_TOOLBAR_11301->HANDLE_USER_COMMAND_11301 FOR CTL_ALV_11301.

    PERFORM FILL_IT_FIELDCATALOG_11301.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_1130.

    "GS_LAYOUT_1130-GRID_TITLE = TEXT-003.
    GS_LAYOUT_1130-SEL_MODE   = 'A'.

    CALL METHOD CTL_ALV_11301->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_1130
        IS_VARIANT           = GS_VARIANT_1130
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_1130
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_11301
        IT_OUTTAB            = IT_ZLEST0128_ALV[].

  ENDIF.

  CALL METHOD CTL_ALV_11301->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_11301->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_11301
      ES_ROW_NO   = GS_SCROLL_ROW_11301.

  CALL METHOD CTL_ALV_1130->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_1130->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_1130
      ES_ROW_NO   = GS_SCROLL_ROW_1130.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1130
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_1130 .

  FIELD-SYMBOLS: <FS_CAT_1130> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0120_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_1130.

  LOOP AT IT_FIELDCATALOG_1130 ASSIGNING <FS_CAT_1130>.
    IF <FS_CAT_1130>-FIELDNAME EQ 'CD_SEQ_LANC'.
      <FS_CAT_1130>-NO_OUT = ABAP_TRUE.
    ENDIF.
    IF <FS_CAT_1130>-FIELDNAME EQ 'MANDT'.
      <FS_CAT_1130>-NO_OUT = ABAP_TRUE.
    ENDIF.
    IF <FS_CAT_1130>-FIELDNAME EQ 'BUKRS'.
      <FS_CAT_1130>-OUTPUTLEN = 6.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT_1130 .

  GS_VARIANT_1130-REPORT      = SY-REPID.
  GS_VARIANT_1130-HANDLE      = '1130'.
  GS_VARIANT_1130-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_1130-USERNAME    = ABAP_FALSE.
  GS_VARIANT_1130-VARIANT     = ABAP_FALSE.
  GS_VARIANT_1130-TEXT        = ABAP_FALSE.
  GS_VARIANT_1130-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCLUIR_EMPRESA .

  CLEAR: ZDE_ZLEST0120_ALV.
  ZDE_ZLEST0120_ALV-CD_SEQ_LANC = ZDE_ZLEST0119_ALV-CD_SEQ_LANC.
  CK_CONFIRMADO_1131 = ABAP_FALSE.

  CALL SCREEN 1131 STARTING AT 10 5.

  IF CK_CONFIRMADO_1131 EQ ABAP_TRUE.
    APPEND ZDE_ZLEST0120_ALV TO IT_ZLEST0120_ALV.
    PERFORM ATUALIZA_TELA_1131.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR.

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_ZLEST0119_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL     = TEXT-010
      TEXTLINE1 = TEXT-011
      TEXTLINE2 = TEXT-012
    IMPORTING
      ANSWER    = ANSWER.

  IF ANSWER EQ 'J'.
    CLEAR: ZDE_ZLEST0119_ALV, ZLEST0119.
    READ TABLE IT_ZLEST0119_SEL INTO ZDE_ZLEST0119_ALV INDEX 1.

    IF IT_ZLEST0119_SEL-QTD_UTILIZADA NE 0.
      MESSAGE S001.
      EXIT.
    ENDIF.

    CK_OPERACAO = 'E'.

    UPDATE ZLEST0119
       SET CK_EXCLUIDO = 'X'
     WHERE CD_SEQ_LANC EQ ZDE_ZLEST0119_ALV-CD_SEQ_LANC.

    COMMIT WORK.
    PERFORM PESQUISAR_FILTRAR.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REATIVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REATIVAR.

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_ZLEST0119_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL     = TEXT-016
      TEXTLINE1 = TEXT-017
      TEXTLINE2 = TEXT-018
    IMPORTING
      ANSWER    = ANSWER.

  IF ANSWER EQ 'J'.
    CLEAR: ZDE_ZLEST0119_ALV, ZLEST0119.
    READ TABLE IT_ZLEST0119_SEL INTO ZDE_ZLEST0119_ALV INDEX 1.

    IF IT_ZLEST0119_SEL-QTD_UTILIZADA NE 0.
      MESSAGE S001.
      EXIT.
    ENDIF.

    CK_OPERACAO = 'I'.

    UPDATE ZLEST0119
       SET CK_EXCLUIDO = ABAP_FALSE
     WHERE CD_SEQ_LANC EQ ZDE_ZLEST0119_ALV-CD_SEQ_LANC.

    COMMIT WORK.
    PERFORM PESQUISAR_FILTRAR.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_EMPRESA .
  LOOP AT IT_ZLEST0120_SEL WHERE PESO_BRUTO IS INITIAL.
    DELETE IT_ZLEST0120_ALV WHERE BUKRS EQ IT_ZLEST0120_SEL-BUKRS.
  ENDLOOP.
  CLEAR: IT_ZLEST0120_SEL[].
  PERFORM ATUALIZA_TELA_1131.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_1131
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_TELA_1131 .

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_1130->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1130 INPUT.

  DATA: WA_ZLEST0119 TYPE ZLEST0119,
        WA_ZLEST0120 TYPE ZLEST0120,
        WA_ZLEST0118 TYPE ZLEST0118,
        WA_ZLEST0128 TYPE ZLEST0128.

  CASE OK_CODE.
    WHEN OK_SALVAR.

      CLEAR OK_CODE.

      CHECK CK_ALTEROU_1130 EQ ABAP_FALSE.

      IF ZDE_ZLEST0119_ALV-DT_INICIO GT ZDE_ZLEST0119_ALV-DT_FIM.
        MESSAGE S002.
        EXIT.
      ENDIF.

      SELECT SINGLE * INTO WA_ZLEST0118
        FROM ZLEST0118
       WHERE LIFNR            EQ ZDE_ZLEST0119_ALV-LIFNR
         AND PAIS             EQ ZDE_ZLEST0119_ALV-PAIS
         AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM
         AND DOMICILIO_DESTIN EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S011.
        EXIT.
      ENDIF.

      IF ( ZDE_ZLEST0119_ALV-UND_PRECO NE ZDE_ZLEST0119_ALV-UND_NEGOCIADO ).
        MESSAGE S014.
        EXIT.
      ENDIF.

      IF ( ZDE_ZLEST0119_ALV-UND_PRECO NE 'TO' AND ZDE_ZLEST0119_ALV-UND_PRECO IS NOT INITIAL ).
        MESSAGE S015.
        EXIT.
      ENDIF.

      IF ZDE_ZLEST0119_ALV-BUKRS  IS NOT INITIAL OR
         ZDE_ZLEST0119_ALV-BRANCH IS NOT INITIAL.

        "Não permitir no mesmo período um preço do mesmo fornecedor nem mesmo material
        SELECT SINGLE * INTO WA_ZLEST0119
          FROM ZLEST0119
         WHERE CD_SEQ_LANC      NE ZDE_ZLEST0119_ALV-CD_SEQ_LANC
           AND LIFNR            EQ ZDE_ZLEST0119_ALV-LIFNR
           AND PAIS             EQ ZDE_ZLEST0119_ALV-PAIS
           AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM
           AND DOMICILIO_DESTIN EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN
           AND DT_INICIO        LE ZDE_ZLEST0119_ALV-DT_INICIO
           AND DT_FIM           GE ZDE_ZLEST0119_ALV-DT_INICIO
           AND PRECO            EQ ZDE_ZLEST0119_ALV-PRECO
           AND TP_MODAL         EQ ZDE_ZLEST0119_ALV-TP_MODAL
           AND TP_PRECO         EQ GB_TP_FRECO
           AND UND_PRECO        EQ ZDE_ZLEST0119_ALV-UND_PRECO
           AND BUKRS            EQ ZDE_ZLEST0119_ALV-BUKRS
           AND BRANCH           EQ ZDE_ZLEST0119_ALV-BRANCH.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE * INTO WA_ZLEST0119
            FROM ZLEST0119
           WHERE CD_SEQ_LANC      NE ZDE_ZLEST0119_ALV-CD_SEQ_LANC
             AND LIFNR            EQ ZDE_ZLEST0119_ALV-LIFNR
             AND PAIS             EQ ZDE_ZLEST0119_ALV-PAIS
             AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM
             AND DOMICILIO_DESTIN EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN
             AND DT_INICIO        LE ZDE_ZLEST0119_ALV-DT_FIM
             AND DT_FIM           GE ZDE_ZLEST0119_ALV-DT_FIM
             AND PRECO            EQ ZDE_ZLEST0119_ALV-PRECO
             AND TP_MODAL         EQ ZDE_ZLEST0119_ALV-TP_MODAL
             AND TP_PRECO         EQ GB_TP_FRECO
             AND UND_PRECO        EQ ZDE_ZLEST0119_ALV-UND_PRECO
             AND BUKRS            EQ ZDE_ZLEST0119_ALV-BUKRS
             AND BRANCH           EQ ZDE_ZLEST0119_ALV-BRANCH.
        ENDIF.

        "Validar Empresas Vinculadas """""""""""""""""""""""""""""""""""""""""""""""""""
        DATA(CK_EMPRESA) = ABAP_FALSE.
        LOOP AT IT_ZLEST0120_ALV WHERE BUKRS NE ZDE_ZLEST0119_ALV-BUKRS.
          CK_EMPRESA = ABAP_TRUE.
        ENDLOOP.

        IF CK_EMPRESA EQ ABAP_TRUE.
          MESSAGE W016 WITH ZDE_ZLEST0119_ALV-BUKRS.
          EXIT.
        ENDIF.
      ELSE.

        "Não permitir no mesmo período um preço do mesmo fornecedor nem mesmo material
        SELECT SINGLE * INTO WA_ZLEST0119
          FROM ZLEST0119
         WHERE CD_SEQ_LANC      NE ZDE_ZLEST0119_ALV-CD_SEQ_LANC
           AND LIFNR            EQ ZDE_ZLEST0119_ALV-LIFNR
           AND PAIS             EQ ZDE_ZLEST0119_ALV-PAIS
           AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM
           AND DOMICILIO_DESTIN EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN
           AND DT_INICIO        LE ZDE_ZLEST0119_ALV-DT_INICIO
           AND DT_FIM           GE ZDE_ZLEST0119_ALV-DT_INICIO
           AND PRECO            EQ ZDE_ZLEST0119_ALV-PRECO
           AND TP_MODAL         EQ ZDE_ZLEST0119_ALV-TP_MODAL
           AND TP_PRECO         EQ GB_TP_FRECO
           AND UND_PRECO        EQ ZDE_ZLEST0119_ALV-UND_PRECO.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE * INTO WA_ZLEST0119
            FROM ZLEST0119
           WHERE CD_SEQ_LANC      NE ZDE_ZLEST0119_ALV-CD_SEQ_LANC
             AND LIFNR            EQ ZDE_ZLEST0119_ALV-LIFNR
             AND PAIS             EQ ZDE_ZLEST0119_ALV-PAIS
             AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0119_ALV-DOMICILIO_ORIGEM
             AND DOMICILIO_DESTIN EQ ZDE_ZLEST0119_ALV-DOMICILIO_DESTIN
             AND DT_INICIO        LE ZDE_ZLEST0119_ALV-DT_FIM
             AND DT_FIM           GE ZDE_ZLEST0119_ALV-DT_FIM
             AND PRECO            EQ ZDE_ZLEST0119_ALV-PRECO
             AND TP_MODAL         EQ ZDE_ZLEST0119_ALV-TP_MODAL
             AND TP_PRECO         EQ GB_TP_FRECO
             AND UND_PRECO        EQ ZDE_ZLEST0119_ALV-UND_PRECO.
        ENDIF.

      ENDIF.

      IF SY-SUBRC IS INITIAL.
        IF WA_ZLEST0119-CK_EXCLUIDO EQ ABAP_TRUE.
          MESSAGE W007.
          MESSAGE W008.
        ELSE.
          MESSAGE S004.
        ENDIF.
        EXIT.
      ENDIF.

      IF ZDE_ZLEST0119_ALV-QTD_TOTAL LT ZDE_ZLEST0119_ALV-QTD_UTILIZADA.
        MESSAGE S009 WITH ZDE_ZLEST0119_ALV-QTD_TOTAL ZDE_ZLEST0119_ALV-QTD_UTILIZADA.
        EXIT.
      ENDIF.

      DELETE FROM ZLEST0120 WHERE CD_SEQ_LANC EQ ZDE_ZLEST0119_ALV-CD_SEQ_LANC.
      MOVE-CORRESPONDING ZDE_ZLEST0119_ALV TO WA_ZLEST0119.
      WA_ZLEST0119-TP_PRECO = GB_TP_FRECO.
      MODIFY ZLEST0119 FROM WA_ZLEST0119.

      LOOP AT IT_ZLEST0120_ALV.
        MOVE-CORRESPONDING IT_ZLEST0120_ALV TO WA_ZLEST0120.
        MODIFY ZLEST0120 FROM WA_ZLEST0120.
      ENDLOOP.

      DELETE FROM ZLEST0128 WHERE CD_SEQ_LANC EQ ZDE_ZLEST0119_ALV-CD_SEQ_LANC.
      LOOP AT IT_ZLEST0128_ALV.
        MOVE-CORRESPONDING IT_ZLEST0128_ALV TO WA_ZLEST0128.
        MODIFY ZLEST0128 FROM WA_ZLEST0128.
      ENDLOOP.

      COMMIT WORK.

      MESSAGE S006.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CADASTRAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CADASTRAR .

  CLEAR: ZDE_ZLEST0119_ALV, ZLEST0119, IT_ZLEST0120_ALV, IT_ZLEST0120_ALV[], IT_ZLEST0128_ALV, IT_ZLEST0128_ALV[]. " Modificação Luis "
  CK_OPERACAO = 'I'.

  ZDE_ZLEST0119_ALV-PAIS = 'BR'.
  ZDE_ZLEST0119_ALV-UND_NEGOCIADO = 'TO'.
  ZDE_ZLEST0119_ALV-WAERK = 'BRL'.
  ZDE_ZLEST0119_ALV-UND_PRECO = 'TO'.
  ZDE_ZLEST0119_ALV-TIPO = 'P'.
  ZDE_ZLEST0119_ALV-TP_MODAL = GB_TP_MODAL.

  CALL SCREEN 1130 STARTING AT 10 5.
  PERFORM PESQUISAR_FILTRAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR .

  IF IT_ZLEST0119_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CK_OPERACAO = 'A'.
  CLEAR: ZDE_ZLEST0119_ALV, ZLEST0119.

  READ TABLE IT_ZLEST0119_SEL INTO ZDE_ZLEST0119_ALV INDEX 1.

  IF ZDE_ZLEST0119_ALV-CK_EXCLUIDO EQ ABAP_TRUE.
    MESSAGE W008.
    EXIT.
  ENDIF.

  CALL METHOD ZCL_CTE_DIST_G=>BUSCA_VOLUME_UTILIZADO_FERRO
    EXPORTING
      I_CD_SEQ_LANC            = ZDE_ZLEST0119_ALV-CD_SEQ_LANC
    IMPORTING
      E_QTD_UTILIZADA          = ZDE_ZLEST0119_ALV-QTD_UTILIZADA
      E_QTD_UTILIZADA_EMP      = IT_SALDO_UTIL_EMP
      E_QTD_UTILIZADA_PRD      = IT_SALDO_UTIL_PRO
      E_QTD_UTILIZADA_EMP_PROD = IT_SALDO_UTIL_EP.

  PERFORM PESQUISAR_DETALHE USING ZDE_ZLEST0119_ALV.
  PERFORM PESQUISAR_DETALHE_MATERIAL USING ZDE_ZLEST0119_ALV.

  CALL SCREEN 1130 STARTING AT 10 5.
  PERFORM PESQUISAR_FILTRAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_DETALHE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZLEST0119_ALV  text
*----------------------------------------------------------------------*
FORM PESQUISAR_DETALHE  USING  P_ZDE_ZLEST0119_ALV TYPE ZDE_ZLEST0119_ALV.

  DATA: IT_EMPRESAS TYPE TABLE OF T001 WITH HEADER LINE.

  CLEAR: IT_ZLEST0120[], IT_ZLEST0120_ALV[].

  "Pesquisa Empresas
  SELECT * INTO TABLE IT_ZLEST0120
    FROM ZLEST0120
   WHERE CD_SEQ_LANC EQ P_ZDE_ZLEST0119_ALV-CD_SEQ_LANC.

  IF IT_ZLEST0120[] IS NOT INITIAL.

    SELECT * INTO TABLE IT_EMPRESAS
      FROM T001
       FOR ALL ENTRIES IN IT_ZLEST0120
     WHERE BUKRS EQ IT_ZLEST0120-BUKRS.

    SORT IT_EMPRESAS BY BUKRS.

    LOOP AT IT_ZLEST0120.
      CLEAR: IT_ZLEST0120_ALV.

      MOVE-CORRESPONDING IT_ZLEST0120 TO IT_ZLEST0120_ALV.

      READ TABLE IT_EMPRESAS WITH KEY BUKRS = IT_ZLEST0120-BUKRS BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        IT_ZLEST0120_ALV-BUTXT = IT_EMPRESAS-BUTXT.
      ENDIF.

      READ TABLE IT_SALDO_UTIL_EMP INTO DATA(WA_SALDO_UTIL_EMP) WITH KEY BUKRS = IT_ZLEST0120-BUKRS.
      IF SY-SUBRC IS INITIAL.
        IT_ZLEST0120_ALV-PESO_BRUTO = WA_SALDO_UTIL_EMP-PESO_BRUTO.
      ENDIF.
      APPEND IT_ZLEST0120_ALV.

    ENDLOOP.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_DETALHE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZLEST0119_ALV  text
*----------------------------------------------------------------------*
FORM PESQUISAR_DETALHE_MATERIAL  USING  P_ZDE_ZLEST0119_ALV TYPE ZDE_ZLEST0119_ALV.

  DATA: IT_MAKT TYPE TABLE OF MAKT WITH HEADER LINE.

  CLEAR: IT_ZLEST0128[], IT_ZLEST0128_ALV[].

  SELECT * INTO TABLE IT_ZLEST0128
    FROM ZLEST0128
   WHERE CD_SEQ_LANC EQ P_ZDE_ZLEST0119_ALV-CD_SEQ_LANC.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE IT_MAKT
    FROM MAKT
     FOR ALL ENTRIES IN IT_ZLEST0128
   WHERE MATNR EQ IT_ZLEST0128-MATNR
    AND SPRAS EQ SY-LANGU.

  SORT IT_MAKT BY MATNR.

  LOOP AT IT_ZLEST0128.
    CLEAR: IT_ZLEST0128_ALV.

    MOVE-CORRESPONDING IT_ZLEST0128 TO IT_ZLEST0128_ALV.

    READ TABLE IT_MAKT WITH KEY MATNR = IT_ZLEST0128-MATNR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0128_ALV-MAKTX = IT_MAKT-MAKTX.
    ENDIF.

    READ TABLE IT_SALDO_UTIL_PRO INTO DATA(WA_SALDO_UTIL_PRD) WITH KEY MATNR = IT_ZLEST0128-MATNR.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0128_ALV-PESO_BRUTO = WA_SALDO_UTIL_PRD-PESO_BRUTO.
    ENDIF.

    APPEND IT_ZLEST0128_ALV.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCLUIR_MATERIAL .

  CLEAR: ZDE_ZLEST0128_ALV.
  ZDE_ZLEST0128_ALV-CD_SEQ_LANC = ZDE_ZLEST0119_ALV-CD_SEQ_LANC.
  CK_CONFIRMADO_1132 = ABAP_FALSE.

  CALL SCREEN 1132 STARTING AT 10 5.

  IF CK_CONFIRMADO_1132 EQ ABAP_TRUE.
    APPEND ZDE_ZLEST0128_ALV TO IT_ZLEST0128_ALV.
    PERFORM ATUALIZA_TELA_1132.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_MATERIAL .
  LOOP AT IT_ZLEST0128_SEL WHERE PESO_BRUTO IS INITIAL.
    DELETE IT_ZLEST0128_ALV WHERE MATNR EQ IT_ZLEST0128_SEL-MATNR.
  ENDLOOP.
  CLEAR: IT_ZLEST0128_SEL[].
  PERFORM ATUALIZA_TELA_1132.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_1131
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_TELA_1132.

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_11301->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1130
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_11301.

  FIELD-SYMBOLS: <FS_CAT_11301> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0128_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_11301.

  LOOP AT IT_FIELDCATALOG_11301 ASSIGNING <FS_CAT_11301>.
    IF <FS_CAT_11301>-FIELDNAME EQ 'CD_SEQ_LANC'.
      <FS_CAT_11301>-NO_OUT = ABAP_TRUE.
    ENDIF.
    IF <FS_CAT_11301>-FIELDNAME EQ 'MANDT'.
      <FS_CAT_11301>-NO_OUT = ABAP_TRUE.
    ENDIF.
    IF <FS_CAT_11301>-FIELDNAME EQ 'MATNR'.
      <FS_CAT_11301>-OUTPUTLEN = 18.
    ENDIF.
  ENDLOOP.

ENDFORM.                   " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_11301 INPUT.

  CALL METHOD CTL_ALV_11301->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_11301
      ES_ROW_NO   = GS_SCROLL_ROW_11301.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_11301 INPUT.
  PERFORM POPULA_SELECAO_DETALHE_11301.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_DETALHE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULA_SELECAO_DETALHE_11301.

  CLEAR IT_SELECTED_ROWS_11301.

  CALL METHOD CTL_ALV_11301->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_11301.

  CLEAR IT_ZLEST0128_SEL[].

  LOOP AT IT_SELECTED_ROWS_11301 INTO WA_SELECTED_ROWS_11301.
    READ TABLE IT_ZLEST0128_ALV INTO WA_ZLEST0128_ALV INDEX WA_SELECTED_ROWS_11301-INDEX.
    MOVE-CORRESPONDING WA_ZLEST0128_ALV TO IT_ZLEST0128_SEL.
    APPEND IT_ZLEST0128_SEL.
  ENDLOOP.

ENDFORM.
