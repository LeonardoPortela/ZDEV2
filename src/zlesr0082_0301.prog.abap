*----------------------------------------------------------------------*
***INCLUDE ZLESR0082_0301.
*----------------------------------------------------------------------*

TABLES ZDE_ZLEST0090.

CONSTANTS: OK_NOVO       TYPE SY-UCOMM VALUE 'PED_NOVO',
           OK_EDITAR     TYPE SY-UCOMM VALUE 'PED_EDITAR',
           OK_EXCLUIR    TYPE SY-UCOMM VALUE 'EXCLUIR',
           OK_VOLTAR     TYPE SY-UCOMM VALUE 'PED_VOLTAR',
           OK_EXIT       TYPE SY-UCOMM VALUE 'PED_SAIR',
           OK_COPIA      TYPE SY-UCOMM VALUE 'PED_COPIA',
           OK_CANCEL     TYPE SY-UCOMM VALUE 'PED_CANCEL',
           OK_ATUALIZAR  TYPE SY-UCOMM VALUE 'PED_ATUAL',
           VG_DYNNR_0303 TYPE SYDYNNR  VALUE '0303'.

DATA: CTL_ALV_0301       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0301       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0301        TYPE LVC_S_LAYO,
      GS_VAR_0301        TYPE DISVARIANT,
      GS_SCROLL_COL_0301 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0301 TYPE LVC_S_ROID,
      IT_CATALOG_0301    TYPE LVC_T_FCAT.

DATA: IT_SELECTED_ROWS_0301 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_0301 TYPE LVC_S_ROW,
      CK_ALTERACAO          TYPE CHAR01.

DATA: IT_ZLEST0090     TYPE TABLE OF ZLEST0090 WITH HEADER LINE,
      IT_ZLEST0090_ALV TYPE TABLE OF ZDE_ZLEST0090 WITH HEADER LINE,
      IT_ZLEST0090_SEL TYPE TABLE OF ZDE_ZLEST0090 WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF SCREEN 0303 AS SUBSCREEN.
SELECT-OPTIONS: P_BUKRS FOR ZDE_ZLEST0090-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY MATCHCODE OBJECT C_T001,
                P_BRANC FOR ZDE_ZLEST0090-WERKS MATCHCODE OBJECT ZE_BRANCH.
SELECTION-SCREEN END OF SCREEN 0303.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0301 OUTPUT.

  IF CTL_CON_0301 IS INITIAL.

    PERFORM PESQUISAR_ADMINISTRADORAS.

    CREATE OBJECT CTL_CON_0301
      EXPORTING
        CONTAINER_NAME = 'ALV_AGENTES'.

    CREATE OBJECT CTL_ALV_0301
      EXPORTING
        I_PARENT = CTL_CON_0301.

    PERFORM FILL_IT_FIELDCATALOG_0301.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0301.

    GS_LAY_0301-SEL_MODE   = 'A'.
    GS_LAY_0301-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0301->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0301
        IS_VARIANT      = GS_VAR_0301
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0301
        IT_OUTTAB       = IT_ZLEST0090_ALV[].

  ELSE.
    CALL METHOD CTL_ALV_0301->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0301->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0301
      ES_ROW_NO   = GS_SCROLL_ROW_0301.

  SET PF-STATUS 'PF0301'.
  SET TITLEBAR 'TL0301'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0301 INPUT.

  CASE OK_CODE.
    WHEN OK_NOVO.
      PERFORM NOVA_ADMINISTRADORA.
    WHEN OK_EDITAR.
      PERFORM EDITAR_ADMINISTRADORAS.
    WHEN OK_EXCLUIR.
      PERFORM DELETAR_ADMINISTRADORAS.
    WHEN OK_COPIA.
      PERFORM EXPANDIR_SELECAO.
    WHEN OK_ATUALIZAR.
      PERFORM PESQUISAR_ADMINISTRADORAS.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0301 .

  FIELD-SYMBOLS: <FS_CAT_0301> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0090'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0301.

  LOOP AT IT_CATALOG_0301 ASSIGNING <FS_CAT_0301>.
    CASE <FS_CAT_0301>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0301>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0301>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0301>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0301>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0301

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0301 .
  GS_VAR_0301-REPORT      = SY-REPID.
  GS_VAR_0301-HANDLE      = '0301'.
  GS_VAR_0301-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0301-USERNAME    = ABAP_FALSE.
  GS_VAR_0301-VARIANT     = ABAP_FALSE.
  GS_VAR_0301-TEXT        = ABAP_FALSE.
  GS_VAR_0301-DEPENDVARS  = ABAP_FALSE.
ENDFORM.                    " FILL_GS_VARIANT_0301

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_ADMINISTRADORAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_ADMINISTRADORAS .

  DATA: IT_T001   TYPE TABLE OF T001 WITH HEADER LINE,
        IT_BRANCH TYPE TABLE OF J_1BBRANCH WITH HEADER LINE,
        IT_LFA1   TYPE TABLE OF LFA1 WITH HEADER LINE.

  CLEAR: IT_ZLEST0090[], IT_ZLEST0090_ALV[].

  SELECT * INTO TABLE IT_ZLEST0090
    FROM ZLEST0090.

  IF IT_ZLEST0090[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_T001
      FROM T001
       FOR ALL ENTRIES IN IT_ZLEST0090
     WHERE BUKRS EQ IT_ZLEST0090-BUKRS.

    SORT IT_T001 BY BUKRS.

    SELECT * INTO TABLE IT_BRANCH
      FROM J_1BBRANCH
       FOR ALL ENTRIES IN IT_ZLEST0090
     WHERE BUKRS  EQ IT_ZLEST0090-BUKRS
       AND BRANCH EQ IT_ZLEST0090-WERKS.

    SORT IT_BRANCH BY BUKRS BRANCH.

    SELECT * INTO TABLE IT_LFA1
      FROM LFA1
       FOR ALL ENTRIES IN IT_ZLEST0090
     WHERE LIFNR EQ IT_ZLEST0090-AGENTEPED.

    SORT IT_LFA1 BY LIFNR.
  ENDIF.

  LOOP AT IT_ZLEST0090.
    CLEAR IT_ZLEST0090_ALV.
    MOVE-CORRESPONDING IT_ZLEST0090 TO IT_ZLEST0090_ALV.

    READ TABLE IT_T001 WITH KEY BUKRS = IT_ZLEST0090-BUKRS.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0090_ALV-BUTXT = IT_T001-BUTXT.
    ENDIF.

    READ TABLE IT_BRANCH WITH KEY BUKRS = IT_ZLEST0090-BUKRS BRANCH = IT_ZLEST0090-WERKS.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0090_ALV-WERKS_NAME = IT_BRANCH-NAME.
    ENDIF.

    READ TABLE IT_LFA1 WITH KEY LIFNR = IT_ZLEST0090-AGENTEPED.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0090_ALV-AGENTEPED_NAME = IT_LFA1-NAME1.
    ENDIF.

    APPEND IT_ZLEST0090_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_ADMINISTRADORAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETAR_ADMINISTRADORAS .

  IF IT_ZLEST0090_SEL[] IS INITIAL.
    MESSAGE S085.
    RETURN.
  ENDIF.

  LOOP AT IT_ZLEST0090_SEL.
    IF IT_ZLEST0090_SEL-TP_ADM IS NOT INITIAL.
      DELETE FROM ZLEST0090 WHERE BUKRS      EQ IT_ZLEST0090_SEL-BUKRS
                              AND WERKS      EQ IT_ZLEST0090_SEL-WERKS
                              AND TP_SERVICO EQ IT_ZLEST0090_SEL-TP_SERVICO
                              AND TP_ADM     EQ IT_ZLEST0090_SEL-TP_ADM.
    ELSE.
      DELETE FROM ZLEST0090 WHERE BUKRS  EQ IT_ZLEST0090_SEL-BUKRS
                              AND WERKS  EQ IT_ZLEST0090_SEL-WERKS.
    ENDIF.
    COMMIT WORK.
  ENDLOOP.

  PERFORM PESQUISAR_ADMINISTRADORAS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0301_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_0301 INPUT.

  CALL METHOD CTL_ALV_0301->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0301
      ES_ROW_NO   = GS_SCROLL_ROW_0301.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_0301 INPUT.

  CLEAR IT_SELECTED_ROWS_0301.

  CALL METHOD CTL_ALV_0301->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_0301.

  CLEAR IT_ZLEST0090_SEL[].

  LOOP AT IT_SELECTED_ROWS_0301 INTO WA_SELECTED_ROWS_0301.
    READ TABLE IT_ZLEST0090_ALV INDEX WA_SELECTED_ROWS_0301-INDEX.
    MOVE-CORRESPONDING IT_ZLEST0090_ALV TO IT_ZLEST0090_SEL.
    APPEND IT_ZLEST0090_SEL.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_ADMINISTRADORAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR_ADMINISTRADORAS .

  IF IT_ZLEST0090_SEL[] IS INITIAL.
    MESSAGE S085.
    RETURN.
  ENDIF.

  LOOP AT IT_ZLEST0090_SEL.
    CK_ALTERACAO = ABAP_TRUE.
    PERFORM EDITAR_ADMINISTRADORA USING IT_ZLEST0090_SEL.
  ENDLOOP.

  PERFORM PESQUISAR_ADMINISTRADORAS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_ADMINISTRADORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZLEST0090_SEL  text
*----------------------------------------------------------------------*
FORM EDITAR_ADMINISTRADORA  USING P_ADM TYPE ZDE_ZLEST0090.
  MOVE-CORRESPONDING P_ADM TO ZDE_ZLEST0090.
  CALL SCREEN 0302 STARTING AT 05 05.
  "  COMMIT WORK.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0302 INPUT.

  DATA: WA_ZLEST0090 TYPE ZLEST0090.

  IF OK_CODE EQ 'CONFIRMAR'.

     IF ( ZDE_ZLEST0090-TP_ADM     EQ '09' ) AND
        ( ZDE_ZLEST0090-TP_SERVICO EQ '1'  ). "Pedágio

       IF ZDE_ZLEST0090-TP_CARD_PED_DEFAULT IS INITIAL.
         MESSAGE I000 WITH 'Informe um Cartão Pedagio Padrão!'.
         EXIT.
       ENDIF.

     ELSE.
       CLEAR: ZDE_ZLEST0090-TP_CARD_PED_DEFAULT.
     ENDIF.

    CHECK ZDE_ZLEST0090-BUTXT IS NOT INITIAL.
    CHECK ZDE_ZLEST0090-AGENTEPED_NAME IS NOT INITIAL.
    CHECK ZDE_ZLEST0090-WERKS_NAME IS NOT INITIAL.
    CHECK ZDE_ZLEST0090-TP_SERVICO IS NOT INITIAL.

    MOVE-CORRESPONDING ZDE_ZLEST0090 TO WA_ZLEST0090.
    MODIFY ZLEST0090 FROM WA_ZLEST0090.

    IF ZDE_ZLEST0090-CK_DEFAULT IS NOT INITIAL.
      UPDATE ZLEST0090
         SET CK_DEFAULT = SPACE
       WHERE BUKRS      EQ ZDE_ZLEST0090-BUKRS
         AND WERKS      EQ ZDE_ZLEST0090-WERKS
         AND TP_SERVICO EQ ZDE_ZLEST0090-TP_SERVICO
         AND TP_ADM     NE ZDE_ZLEST0090-TP_ADM.
    ENDIF.

    MESSAGE S000 WITH 'Alteração Realizada'.
    LEAVE TO SCREEN 0.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0302 OUTPUT.

  SET PF-STATUS 'PF0302'.
  SET TITLEBAR  'TL0302'.

  IF CK_ALTERACAO = ABAP_TRUE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'A1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'ZDE_ZLEST0090-TP_CARD_PED_DEFAULT'.

        IF ( ZDE_ZLEST0090-TP_ADM     EQ '09' ) AND
           ( ZDE_ZLEST0090-TP_SERVICO EQ '1'  ). "Pedágio

          SCREEN-INPUT  = 1.
          SCREEN-ACTIVE = 1.

        ELSE.

          SCREEN-INPUT  = 0.
          SCREEN-ACTIVE = 0.

        ENDIF.

        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

  CLEAR: ZDE_ZLEST0090-BUTXT,
         ZDE_ZLEST0090-AGENTEPED_NAME,
         ZDE_ZLEST0090-WERKS_NAME.

  IF ZDE_ZLEST0090-BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT INTO ZDE_ZLEST0090-BUTXT
      FROM T001
     WHERE BUKRS EQ ZDE_ZLEST0090-BUKRS.
  ENDIF.

  IF ZDE_ZLEST0090-BUKRS IS NOT INITIAL AND ZDE_ZLEST0090-WERKS IS NOT INITIAL.
    SELECT SINGLE NAME INTO ZDE_ZLEST0090-WERKS_NAME
      FROM J_1BBRANCH
     WHERE BUKRS  EQ ZDE_ZLEST0090-BUKRS
       AND BRANCH EQ ZDE_ZLEST0090-WERKS.
  ENDIF.

  IF ZDE_ZLEST0090-AGENTEPED IS NOT INITIAL.
    SELECT SINGLE NAME1 INTO ZDE_ZLEST0090-AGENTEPED_NAME
      FROM LFA1
     WHERE LIFNR EQ ZDE_ZLEST0090-AGENTEPED.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0302_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERACAO_ADM_PED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTERACAO_ADM_PED INPUT.
  CLEAR: ZDE_ZLEST0090-BUTXT,
         ZDE_ZLEST0090-AGENTEPED_NAME,
         ZDE_ZLEST0090-WERKS_NAME.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  NOVA_ADMINISTRADORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM NOVA_ADMINISTRADORA .
  CLEAR: ZDE_ZLEST0090.
  CK_ALTERACAO = ABAP_FALSE.
  CALL SCREEN 0302 STARTING AT 05 05.
  COMMIT WORK.
  PERFORM PESQUISAR_ADMINISTRADORAS.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXPANDIR_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXPANDIR_SELECAO .

  IF IT_ZLEST0090_SEL[] IS INITIAL.
    MESSAGE S085.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0090_SEL INDEX 1 INTO ZDE_ZLEST0090.

  CALL SCREEN 0304 STARTING AT 05 05.

  PERFORM PESQUISAR_ADMINISTRADORAS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0304 OUTPUT.
  SET PF-STATUS 'PF0302'.
  SET TITLEBAR 'TL0304'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0304_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0304 INPUT.

  DATA: IT_J_1BBRANCH TYPE TABLE OF J_1BBRANCH WITH HEADER LINE,
        IT_ZLEST0090A TYPE TABLE OF ZLEST0090 WITH HEADER LINE.

  IF OK_CODE EQ 'CONFIRMAR'.
    SELECT * INTO TABLE IT_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS  IN P_BUKRS
       AND BRANCH IN P_BRANC.

    CLEAR: IT_ZLEST0090A[].

    LOOP AT IT_J_1BBRANCH.
      IT_ZLEST0090A-BUKRS          = IT_J_1BBRANCH-BUKRS.
      IT_ZLEST0090A-WERKS          = IT_J_1BBRANCH-BRANCH.
      IT_ZLEST0090A-TP_SERVICO     = ZDE_ZLEST0090-TP_SERVICO.
      IT_ZLEST0090A-TP_ADM         = ZDE_ZLEST0090-TP_ADM.
      IT_ZLEST0090A-TP_OP          = ZDE_ZLEST0090-TP_OP.
      IT_ZLEST0090A-AGENTEPED      = ZDE_ZLEST0090-AGENTEPED.
      IT_ZLEST0090A-NR_VR_XML_TIPF = ZDE_ZLEST0090-NR_VR_XML_TIPF.
      IT_ZLEST0090A-CK_DEFAULT     = ZDE_ZLEST0090-CK_DEFAULT.
      APPEND IT_ZLEST0090A.
    ENDLOOP.

    IF IT_ZLEST0090A[] IS NOT INITIAL.
      MODIFY ZLEST0090 FROM TABLE IT_ZLEST0090A.

      IF ZDE_ZLEST0090-CK_DEFAULT IS NOT INITIAL.
        LOOP AT IT_ZLEST0090A.
          UPDATE ZLEST0090
             SET CK_DEFAULT = SPACE
           WHERE BUKRS      EQ IT_ZLEST0090A-BUKRS
             AND WERKS      EQ IT_ZLEST0090A-WERKS
             AND TP_SERVICO EQ IT_ZLEST0090A-TP_SERVICO
             AND TP_ADM     NE IT_ZLEST0090A-TP_ADM.
        ENDLOOP.
      ENDIF.

      COMMIT WORK.
      MESSAGE S000 WITH 'Cópia Realizada'.
    ENDIF.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
