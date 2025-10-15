*----------------------------------------------------------------------*
***INCLUDE MZSEGTRANSPORTE1130.
*----------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_1130 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: CTL_ALV_1130       TYPE REF TO CL_GUI_ALV_GRID,
      C_ALV_TOOLBAR_1130 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

DATA: IT_SELECTED_ROWS_1130 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_1130 TYPE LVC_S_ROW.

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
*    "Separador
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    "Selecionar Para Inclusão
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_IMPORT.
    TY_TOOLBAR-FUNCTION  = 'A++'.
    TY_TOOLBAR-QUICKINFO = TEXT-004.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF CK_OPERACAO EQ 'C'.
      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
    ELSE.
      TY_TOOLBAR-DISABLED  = ABAP_FALSE.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    "Marcar Todos os Documentos
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = 'ADD'.
    TY_TOOLBAR-QUICKINFO = TEXT-005.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF CK_OPERACAO EQ 'C'.
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
    IF CK_OPERACAO EQ 'C'.
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

    CLEAR: IT_ZLEST0116_SEL[].

    LOOP AT IT_SELECTED_ROWS_1130 INTO WA_SELECTED_ROWS_1130.
      READ TABLE IT_ZLEST0116_ALV INTO WA_ZLEST0116_ALV INDEX WA_SELECTED_ROWS_1130-INDEX.
      APPEND WA_ZLEST0116_ALV TO IT_ZLEST0116_SEL.
    ENDLOOP.

    CASE E_UCOMM.
      WHEN 'A++'.
        PERFORM INCLUIR_GRUPOS_EXISTENTES.
      WHEN 'ADD'.
        PERFORM INCLUIR_GRUPO.
      WHEN 'DEL'.
        PERFORM EXCLUIR_GRUPO.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION


DATA: OBG_TOOLBAR_1130 TYPE REF TO LCL_ALV_TOOLBAR_1130.

DATA: CTL_CON_1130 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GS_LAYOUT_1130       TYPE LVC_S_LAYO,
      GS_VARIANT_1130      TYPE DISVARIANT,
      IT_FIELDCATALOG_1130 TYPE LVC_T_FCAT,
      WA_FIELDCATALOG_1130 TYPE LVC_S_FCAT.

DATA: IT_EXCLUDE_FCODE_1130 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_1130 LIKE LINE OF IT_EXCLUDE_FCODE_1130.

DATA: GS_SCROLL_COL_1130 TYPE LVC_S_COL,
      GS_SCROLL_ROW_1130 TYPE LVC_S_ROID.

DATA: CK_ALTEROU_1130 TYPE CHAR01.

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

  IF ZDE_ZLEST0115_ALV-CD_FORNECEDOR IS NOT INITIAL.
    SELECT SINGLE NAME1 INTO ZDE_ZLEST0115_ALV-NM_FORNECEDOR FROM LFA1 WHERE LIFNR = ZDE_ZLEST0115_ALV-CD_FORNECEDOR.
  ENDIF.

  IF CK_OPERACAO EQ 'I'.

    IF ZDE_ZLEST0115_ALV-CD_APOLICE IS INITIAL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR = '01'
          OBJECT      = 'ZAPOLICET'
        IMPORTING
          NUMBER      = ZDE_ZLEST0115_ALV-CD_APOLICE.

    ENDIF.

    ZDE_ZLEST0115_ALV-DT_CADASTRO    = SY-DATUM.
    ZDE_ZLEST0115_ALV-HR_CADASTRO    = SY-UZEIT.
    ZDE_ZLEST0115_ALV-DS_US_CADASTRO = SY-UNAME.
  ENDIF.

  IF CK_OPERACAO EQ 'A' OR CK_OPERACAO EQ 'I'.
    ZDE_ZLEST0115_ALV-DT_ATUALIZACAO    = SY-DATUM.
    ZDE_ZLEST0115_ALV-HR_ATUALIZACAO    = SY-UZEIT.
    ZDE_ZLEST0115_ALV-DS_US_ATUALIZACAO = SY-UNAME.
  ENDIF.

  IF CK_OPERACAO EQ 'C'.
    LOOP AT SCREEN.
      IF SCREEN-NAME(17) EQ 'ZDE_ZLEST0115_ALV'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
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
        IT_OUTTAB            = IT_ZLEST0116_ALV[].

  ENDIF.

  CALL METHOD CTL_ALV_1130->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_1130->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_1130
      ES_ROW_NO   = GS_SCROLL_ROW_1130.


ENDMODULE.

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
*&      Module  USER_COMMAND_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1130 INPUT.

  DATA: WA_ZLEST0115 TYPE ZLEST0115,
        WA_ZLEST0116 TYPE ZLEST0116.

  CASE OK_CODE.
    WHEN OK_SALVAR.

      CLEAR OK_CODE.

      CHECK CK_ALTEROU_1130 EQ ABAP_FALSE.

      IF ZDE_ZLEST0115_ALV-NR_APOLICE IS INITIAL AND ZDE_ZLEST0115_ALV-NR_PROPOSTA IS INITIAL.
        MESSAGE S001.
        EXIT.
      ENDIF.

      IF ZDE_ZLEST0115_ALV-DT_INICIO GT ZDE_ZLEST0115_ALV-DT_FINAL.
        MESSAGE S002.
        EXIT.
      ENDIF.

      "UK(1): Definir código do fornecedor, número da apólice e número da proposta como campos únicos;
      SELECT SINGLE * INTO WA_ZLEST0115
        FROM ZLEST0115
       WHERE CD_APOLICE    NE ZDE_ZLEST0115_ALV-CD_APOLICE
         AND CD_FORNECEDOR EQ ZDE_ZLEST0115_ALV-CD_FORNECEDOR
         AND NR_APOLICE    EQ ZDE_ZLEST0115_ALV-NR_APOLICE
         AND NR_PROPOSTA   EQ ZDE_ZLEST0115_ALV-NR_PROPOSTA.

      IF SY-SUBRC IS INITIAL.
        IF WA_ZLEST0115-CK_EXCLUIDO EQ ABAP_TRUE.
*007  Esta Apólice já está cadastrada (POREM EXCLUIDA)!
*008  Utilize a função de Reativar!
          MESSAGE W007.
          MESSAGE W008.
        ELSE.
          MESSAGE S004.
        ENDIF.
        EXIT.
      ENDIF.

      DELETE FROM ZLEST0116 WHERE CD_APOLICE EQ ZDE_ZLEST0115_ALV-CD_APOLICE.
      MOVE-CORRESPONDING ZDE_ZLEST0115_ALV TO WA_ZLEST0115.
      MODIFY ZLEST0115 FROM WA_ZLEST0115.

      LOOP AT IT_ZLEST0116_ALV.
        MOVE-CORRESPONDING IT_ZLEST0116_ALV TO WA_ZLEST0116.
        MODIFY ZLEST0116 FROM WA_ZLEST0116.
      ENDLOOP.

      COMMIT WORK.

      MESSAGE S006.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CADASTRAR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CADASTRAR_APOLICE .
  CLEAR: ZDE_ZLEST0115_ALV, ZLEST0115, IT_ZLEST0116_ALV, IT_ZLEST0116_ALV[].
  CK_OPERACAO = 'I'.
  CALL SCREEN 1130 STARTING AT 10 5.
  PERFORM PESQUISAR_FILTRAR_APOLICES.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZLEST0115_ALV  text
*----------------------------------------------------------------------*
FORM ABRIR_APOLICE.

  IF IT_ZLEST0115_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CK_OPERACAO = 'C'.
  CLEAR: ZDE_ZLEST0115_ALV, ZLEST0115.

  READ TABLE IT_ZLEST0115_SEL INTO ZDE_ZLEST0115_ALV INDEX 1.

  PERFORM PESQUISAR_DETALHE_APOLICE USING ZDE_ZLEST0115_ALV.

  CALL SCREEN 1130 STARTING AT 10 5.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZLEST0115_ALV  text
*----------------------------------------------------------------------*
FORM EDITAR_APOLICE.

  IF IT_ZLEST0115_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CK_OPERACAO = 'A'.
  CLEAR: ZDE_ZLEST0115_ALV, ZLEST0115.

  READ TABLE IT_ZLEST0115_SEL INTO ZDE_ZLEST0115_ALV INDEX 1.

  IF ZDE_ZLEST0115_ALV-CK_EXCLUIDO EQ ABAP_TRUE.
    MESSAGE W008.
    EXIT.
  ENDIF.

  PERFORM PESQUISAR_DETALHE_APOLICE USING ZDE_ZLEST0115_ALV.

  CALL SCREEN 1130 STARTING AT 10 5.
  PERFORM PESQUISAR_FILTRAR_APOLICES.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZLEST0115_ALV  text
*----------------------------------------------------------------------*
FORM EXCLUIR_APOLICE.

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_ZLEST0115_SEL[] IS INITIAL.
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
    CK_OPERACAO = 'E'.
    CLEAR: ZDE_ZLEST0115_ALV, ZLEST0115.
    READ TABLE IT_ZLEST0115_SEL INTO ZDE_ZLEST0115_ALV INDEX 1.

    UPDATE ZLEST0115
       SET CK_EXCLUIDO = 'X'
     WHERE CD_APOLICE EQ ZDE_ZLEST0115_ALV-CD_APOLICE.

    COMMIT WORK.
    PERFORM PESQUISAR_FILTRAR_APOLICES.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_DETALHE_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZLEST0115_ALV  text
*----------------------------------------------------------------------*
FORM PESQUISAR_DETALHE_APOLICE  USING P_ZLEST0115_ALV TYPE ZDE_ZLEST0115_ALV.

  DATA: IT_EMPRESAS TYPE TABLE OF T001 WITH HEADER LINE,
        IT_GRUPOS   TYPE TABLE OF T023T WITH HEADER LINE.

  CLEAR: IT_ZLEST0116[], IT_ZLEST0116_ALV[].

  SELECT * INTO TABLE IT_ZLEST0116
    FROM ZLEST0116
   WHERE CD_APOLICE EQ P_ZLEST0115_ALV-CD_APOLICE.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE IT_EMPRESAS
    FROM T001
     FOR ALL ENTRIES IN IT_ZLEST0116
   WHERE BUKRS EQ IT_ZLEST0116-CD_EMPRESA.

  SORT IT_EMPRESAS BY BUKRS.

  SELECT * INTO TABLE IT_GRUPOS
    FROM T023T
     FOR ALL ENTRIES IN IT_ZLEST0116
   WHERE SPRAS EQ SY-LANGU
     AND MATKL EQ IT_ZLEST0116-CD_GRUPO.

  SORT IT_GRUPOS BY MATKL.

  LOOP AT IT_ZLEST0116.
    CLEAR: IT_ZLEST0116_ALV.

    MOVE-CORRESPONDING IT_ZLEST0116 TO IT_ZLEST0116_ALV.

    READ TABLE IT_EMPRESAS WITH KEY BUKRS = IT_ZLEST0116-CD_EMPRESA BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0116_ALV-DS_EMPRESA = IT_EMPRESAS-BUTXT.
    ENDIF.

    READ TABLE IT_GRUPOS   WITH KEY MATKL = IT_ZLEST0116-CD_GRUPO BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0116_ALV-DS_GRUPO = IT_GRUPOS-WGBEZ.
    ENDIF.

    APPEND IT_ZLEST0116_ALV.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1130
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_1130 .

  FIELD-SYMBOLS: <FS_CAT_1130> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0116_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_1130.

  LOOP AT IT_FIELDCATALOG_1130 ASSIGNING <FS_CAT_1130>.
    IF <FS_CAT_1130>-FIELDNAME EQ 'CD_APOLICE'.
      <FS_CAT_1130>-NO_OUT = ABAP_TRUE.
    ENDIF.
    IF <FS_CAT_1130>-FIELDNAME EQ 'CD_EMPRESA'.
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
  PERFORM POPULA_SELECAO_APOLICE_DETALHE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_APOLICE_DETALHE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULA_SELECAO_APOLICE_DETALHE .

  CLEAR IT_SELECTED_ROWS_1130.

  CALL METHOD CTL_ALV_1130->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_1130.

  CLEAR IT_ZLEST0116_SEL[].

  LOOP AT IT_SELECTED_ROWS_1130 INTO WA_SELECTED_ROWS_1130.
    READ TABLE IT_ZLEST0116_ALV INTO WA_ZLEST0116_ALV INDEX WA_SELECTED_ROWS_1130-INDEX.
    MOVE-CORRESPONDING WA_ZLEST0116_ALV TO IT_ZLEST0116_SEL.
    APPEND IT_ZLEST0116_SEL.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_GRUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_GRUPO .
  LOOP AT IT_ZLEST0116_SEL.
    DELETE IT_ZLEST0116_ALV WHERE CD_EMPRESA EQ IT_ZLEST0116_SEL-CD_EMPRESA
                              AND CD_GRUPO   EQ IT_ZLEST0116_SEL-CD_GRUPO  .
  ENDLOOP.
  CLEAR: IT_ZLEST0116_SEL[].
  PERFORM ATUALIZA_TELA_1131.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_1131
*&---------------------------------------------------------------------*
*       text
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
*&      Module  ALTEROU_1130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1130 INPUT.
  CK_ALTEROU_1130 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  COPIAR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPIAR_APOLICE .

  FIELD-SYMBOLS: <FS_0116> TYPE ZDE_ZLEST0116_ALV.

  IF IT_ZLEST0115_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CLEAR: ZDE_ZLEST0115_ALV, ZLEST0115.

  READ TABLE IT_ZLEST0115_SEL INTO ZDE_ZLEST0115_ALV INDEX 1.

  IF ZDE_ZLEST0115_ALV-CK_EXCLUIDO EQ ABAP_TRUE.
    MESSAGE W008.
    EXIT.
  ENDIF.

  PERFORM PESQUISAR_DETALHE_APOLICE USING ZDE_ZLEST0115_ALV.

  CLEAR: ZDE_ZLEST0115_ALV-NR_APOLICE,
         ZDE_ZLEST0115_ALV-NR_PROPOSTA,
         ZDE_ZLEST0115_ALV-DT_INICIO,
         ZDE_ZLEST0115_ALV-DT_FINAL.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZAPOLICET'
    IMPORTING
      NUMBER      = ZDE_ZLEST0115_ALV-CD_APOLICE.

  LOOP AT IT_ZLEST0116_ALV ASSIGNING <FS_0116>.
    <FS_0116>-CD_APOLICE = ZDE_ZLEST0115_ALV-CD_APOLICE.
  ENDLOOP.

  CK_OPERACAO = 'I'.
  CALL SCREEN 1130 STARTING AT 10 5.
  PERFORM PESQUISAR_FILTRAR_APOLICES.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATIVAR_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATIVAR_APOLICE .

  IF IT_ZLEST0115_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  READ TABLE IT_ZLEST0115_SEL INTO ZDE_ZLEST0115_ALV INDEX 1.

  CHECK ZDE_ZLEST0115_ALV-CK_EXCLUIDO EQ ABAP_TRUE.

  UPDATE ZLEST0115
     SET CK_EXCLUIDO = SPACE
   WHERE CD_APOLICE EQ ZDE_ZLEST0115_ALV-CD_APOLICE.

  COMMIT WORK.

  PERFORM PESQUISAR_FILTRAR_APOLICES.

ENDFORM.
