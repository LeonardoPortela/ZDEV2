*----------------------------------------------------------------------*
***INCLUDE MZTOPFERRO_2100.
*----------------------------------------------------------------------*

DATA: CTL_CON_2100       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_2100       TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAY_2100        TYPE LVC_S_LAYO,
      GS_VAR_2100        TYPE DISVARIANT,
      GS_SCROLL_COL_2100 TYPE LVC_S_COL,
      GS_SCROLL_ROW_2100 TYPE LVC_S_ROID,
      IT_CATALOG_2100    TYPE LVC_T_FCAT.

DATA: IT_EXCLUDE_2100 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_2100 LIKE LINE OF IT_EXCLUDE_2100.

DATA: IT_SELECTED_ROWS_2100 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_2100 TYPE LVC_S_ROW.


*&---------------------------------------------------------------------*
*&      Module  STATUS_2100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF CTL_CON_2100 IS INITIAL.

    CREATE OBJECT CTL_CON_2100
      EXPORTING
        CONTAINER_NAME = 'ALV_2010'.

    CREATE OBJECT CTL_ALV_2100
      EXPORTING
        I_PARENT = CTL_CON_2100.

*    CREATE OBJECT OBG_TOOLBAR_2100
*      EXPORTING
*        IO_ALV_GRID = CTL_ALV_2100.
*
*    SET HANDLER OBG_TOOLBAR_2100->ON_TOOLBAR FOR CTL_ALV_2100.
*    SET HANDLER OBG_TOOLBAR_2100->HANDLE_USER_COMMAND FOR CTL_ALV_2100.

    PERFORM FILL_IT_FIELDCATALOG_2100.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_2100.
*   Set layout parameters for ALV grid

    GS_LAY_2100-SEL_MODE   = SPACE.
    GS_LAY_2100-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_2100->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_2100
        IS_VARIANT           = GS_VAR_2100
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_2100
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_2100
        IT_OUTTAB            = IT_ZLEST0118_ALV[].

    "CREATE OBJECT EVENT_HANDLER_2100.
    "SET HANDLER EVENT_HANDLER_2100->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_2100.
  ELSE.
    CALL METHOD CTL_ALV_2100->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_2100->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_2100
      ES_ROW_NO   = GS_SCROLL_ROW_2100.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_2100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_2100 .

  FIELD-SYMBOLS: <FS_CAT_2100> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0118_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_2100.

  LOOP AT IT_CATALOG_2100 ASSIGNING <FS_CAT_2100>.
*    CASE <FS_CAT_2100>-FIELDNAME.
*      WHEN ''.
*        <FS_CAT_2100>-KEY     = ABAP_TRUE.
*        <FS_CAT_2100>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT_2100>-JUST    = 'C'.
*      WHEN ''.
*        <FS_CAT_2100>-HOTSPOT = ABAP_TRUE.
*      WHEN ''.
*        <FS_CAT_2100>-DO_SUM = ABAP_TRUE.
*    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_N55

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_2100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_2100 .

  GS_VAR_2100-REPORT      = SY-REPID.
  GS_VAR_2100-HANDLE      = '2100'.
  GS_VAR_2100-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_2100-USERNAME    = ABAP_FALSE.
  GS_VAR_2100-VARIANT     = ABAP_FALSE.
  GS_VAR_2100-TEXT        = ABAP_FALSE.
  GS_VAR_2100-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_2100

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2100 INPUT.

  CASE OK_CODE.
    WHEN OK_INCLUIR.
      PERFORM CADASTRAR_ITINERARIO.
      CLEAR: OK_CODE.
    WHEN OK_EXCLUIR.
      PERFORM EXCLUIR_ITINERARIO.
      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_2100 INPUT.
  CALL METHOD CTL_ALV_2100->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_2100
      ES_ROW_NO   = GS_SCROLL_ROW_2100.
ENDMODULE.                 " GET_SCROLL_INFO_2100  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_2100 INPUT.

  CLEAR IT_SELECTED_ROWS_2100.

  CALL METHOD CTL_ALV_2100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_2100.

  CLEAR IT_ZLEST0118_SEL[].

  LOOP AT IT_SELECTED_ROWS_2100 INTO WA_SELECTED_ROWS_2100.
    READ TABLE IT_ZLEST0118_ALV INTO WA_ZLEST0118_ALV INDEX WA_SELECTED_ROWS_2100-INDEX.
    MOVE-CORRESPONDING WA_ZLEST0118_ALV TO IT_ZLEST0118_SEL.
    APPEND IT_ZLEST0118_SEL.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS_2100  INPUT

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_ITINERARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_ITINERARIO .

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_ZLEST0118_SEL[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL     = TEXT-013
      TEXTLINE1 = TEXT-014
      TEXTLINE2 = TEXT-015
    IMPORTING
      ANSWER    = ANSWER.

  IF ANSWER EQ 'J'.

    SELECT SINGLE * INTO WA_ZLEST0119
      FROM ZLEST0119
     WHERE LIFNR            EQ ZDE_ZLEST0118_ALV-LIFNR
       AND PAIS             EQ ZDE_ZLEST0118_ALV-PAIS
       AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0118_ALV-DOMICILIO_ORIGEM
       AND DOMICILIO_DESTIN EQ ZDE_ZLEST0118_ALV-DOMICILIO_DESTIN.

    IF SY-SUBRC IS INITIAL.
      MESSAGE S012.
      EXIT.
    ENDIF.

    CK_OPERACAO = 'E'.
    CLEAR: ZDE_ZLEST0118_ALV, ZLEST0118.
    READ TABLE IT_ZLEST0118_SEL INTO ZDE_ZLEST0118_ALV INDEX 1.

    DELETE FROM ZLEST0118
     WHERE LIFNR EQ ZDE_ZLEST0118_ALV-LIFNR
       AND PAIS EQ ZDE_ZLEST0118_ALV-PAIS
       AND DOMICILIO_ORIGEM EQ ZDE_ZLEST0118_ALV-DOMICILIO_ORIGEM
       AND DOMICILIO_DESTIN EQ ZDE_ZLEST0118_ALV-DOMICILIO_DESTIN.
    COMMIT WORK.

    PERFORM PESQUISAR_ITINERARIOS.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_ITINERARIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_ITINERARIOS .

  DATA: IT_LFA1 TYPE TABLE OF LFA1 WITH HEADER LINE.
  DATA: IT_J_1BTXJURT TYPE TABLE OF J_1BTXJURT WITH HEADER LINE.

  CLEAR: IT_ZLEST0118_ALV[].

  SELECT * INTO TABLE IT_ZLEST0118
    FROM ZLEST0118.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE IT_LFA1
    FROM LFA1
     FOR ALL ENTRIES IN IT_ZLEST0118
   WHERE LIFNR EQ IT_ZLEST0118-LIFNR.

  SORT IT_LFA1 BY LIFNR.

  SELECT * INTO TABLE IT_J_1BTXJURT
    FROM J_1BTXJURT
     FOR ALL ENTRIES IN IT_ZLEST0118
   WHERE SPRAS   EQ SY-LANGU
     AND COUNTRY EQ IT_ZLEST0118-PAIS
     AND TAXJURCODE EQ IT_ZLEST0118-DOMICILIO_ORIGEM.

  SELECT * APPENDING TABLE IT_J_1BTXJURT
    FROM J_1BTXJURT
     FOR ALL ENTRIES IN IT_ZLEST0118
   WHERE SPRAS   EQ SY-LANGU
     AND COUNTRY EQ IT_ZLEST0118-PAIS
     AND TAXJURCODE EQ IT_ZLEST0118-DOMICILIO_DESTIN.

  SORT IT_J_1BTXJURT BY COUNTRY TAXJURCODE.

  LOOP AT IT_ZLEST0118.
    CLEAR: IT_ZLEST0118_ALV.
    MOVE-CORRESPONDING IT_ZLEST0118 TO IT_ZLEST0118_ALV.

    READ TABLE IT_LFA1 WITH KEY LIFNR = IT_ZLEST0118-LIFNR BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0118_ALV-NAME1 = IT_LFA1-NAME1.
    ENDIF.

    READ TABLE IT_J_1BTXJURT WITH KEY COUNTRY = IT_ZLEST0118-PAIS TAXJURCODE = IT_ZLEST0118-DOMICILIO_ORIGEM BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0118_ALV-TEXT_ORIGEM = IT_J_1BTXJURT-TEXT.
    ENDIF.

    READ TABLE IT_J_1BTXJURT WITH KEY COUNTRY = IT_ZLEST0118-PAIS TAXJURCODE = IT_ZLEST0118-DOMICILIO_DESTIN BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_ZLEST0118_ALV-TEXT_DESTINO = IT_J_1BTXJURT-TEXT.
    ENDIF.

    APPEND IT_ZLEST0118_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CADASTRAR_ITINERARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CADASTRAR_ITINERARIO .

  CLEAR: ZDE_ZLEST0118_ALV, ZLEST0118.
  CK_OPERACAO = 'I'.
  ZDE_ZLEST0118_ALV-PAIS = 'BR'.

  CALL SCREEN 2110 STARTING AT 03 05.
  PERFORM PESQUISAR_ITINERARIOS.

ENDFORM.
