*&---------------------------------------------------------------------*
*& Report  ZLESR0137
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0137  MESSAGE-ID ZLES.

TABLES: ZLEST0191, ZDE_ZLEST0191_ALV.

DATA: CK_ALTEROU TYPE CHAR01.

DATA: SPLITTER           TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CONTAINER_01       TYPE REF TO CL_GUI_CONTAINER,
      ALV_01             TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_01 TYPE LVC_T_FCAT,
      LAYOUT_01          TYPE LVC_S_LAYO,
      VARIANT_01         TYPE DISVARIANT,
      IT_ZLEST0191_ALV   TYPE TABLE OF ZDE_ZLEST0191_ALV,
      OK_CODE            TYPE SY-UCOMM.

START-OF-SELECTION.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: IT_CODE TYPE TABLE OF SY-UCOMM.
  CLEAR: IT_CODE[].

  IF CK_ALTEROU NE ABAP_TRUE.
    APPEND 'SAVE' TO IT_CODE.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING IT_CODE.
  SET TITLEBAR 'TL0100'.

  IF SPLITTER IS INITIAL.

    CK_ALTEROU = ABAP_FALSE.

    PERFORM SELECIONAR_REGISTROS.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 1.

    CONTAINER_01 = SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

    CREATE OBJECT ALV_01
      EXPORTING
        I_PARENT = CONTAINER_01.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME = 'ZDE_ZLEST0191_ALV'
      CHANGING
        CT_FIELDCAT      = IT_FIELDCATALOG_01.

    VARIANT_01-REPORT    = SY-REPID.
    VARIANT_01-HANDLE    = '0100'.

    LAYOUT_01-SEL_MODE   = 'A'.
    LAYOUT_01-INFO_FNAME = 'LINE_COLOR'.
    LAYOUT_01-STYLEFNAME = 'STYLE'.
    LAYOUT_01-CTAB_FNAME = 'COLOR_CELL'.

    CALL METHOD ALV_01->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = LAYOUT_01
        IS_VARIANT      = VARIANT_01
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_01
        IT_OUTTAB       = IT_ZLEST0191_ALV[].

  ENDIF.

  ALV_01->REFRESH_TABLE_DISPLAY(
    EXPORTING
      IS_STABLE      = VALUE #( ROW = ABAP_TRUE COL = ABAP_TRUE )
      I_SOFT_REFRESH = ABAP_TRUE
  ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  DATA: ANSWER TYPE C.

  ANSWER = '1'.

  IF CK_ALTEROU EQ ABAP_TRUE.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = TEXT-001
        TEXT_QUESTION         = TEXT-002
        TEXT_BUTTON_1         = TEXT-003
        ICON_BUTTON_1         = 'ICON_CHECKED'
        TEXT_BUTTON_2         = TEXT-004
        ICON_BUTTON_2         = 'ICON_INCOMPLETE'
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    IF SY-SUBRC IS NOT INITIAL.
      EXIT.
    ENDIF.

    CHECK ANSWER EQ '1'.

    CK_ALTEROU = ABAP_FALSE.

  ENDIF.

  CHECK CK_ALTEROU EQ ABAP_FALSE.

  LEAVE PROGRAM.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_REGISTROS .

  CLEAR: IT_ZLEST0191_ALV[].

  SELECT * INTO TABLE @DATA(IT_ZLEST0191)
    FROM ZLEST0191.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE @DATA(IT_ZLEST0187)
    FROM ZLEST0187
     FOR ALL ENTRIES IN @IT_ZLEST0191
   WHERE ID_REGIAO EQ @IT_ZLEST0191-ID_REGIAO.

  SELECT * INTO TABLE @DATA(IT_LFA1)
    FROM LFA1
     FOR ALL ENTRIES IN @IT_ZLEST0191
   WHERE LIFNR EQ @IT_ZLEST0191-LIFNR.

  SORT IT_ZLEST0187 BY ID_REGIAO.
  SORT IT_LFA1 BY LIFNR.

  LOOP AT IT_ZLEST0191 INTO DATA(WA_ZLEST0191).

    READ TABLE IT_ZLEST0187 WITH KEY ID_REGIAO = WA_ZLEST0191-ID_REGIAO INTO DATA(WA_ZLEST0187) BINARY SEARCH.
    READ TABLE IT_LFA1 WITH KEY LIFNR = WA_ZLEST0191-LIFNR INTO DATA(WA_LFA1) BINARY SEARCH.

    APPEND VALUE #( LIFNR     = WA_ZLEST0191-LIFNR
                    NAME1     = WA_LFA1-NAME1
                    ID_REGIAO = WA_ZLEST0191-ID_REGIAO
                    DS_REGIAO = WA_ZLEST0187-DS_REGIAO
                    TP_REGIAO = WA_ZLEST0187-TP_REGIAO ) TO IT_ZLEST0191_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN 'INCLUIR'.
      PERFORM INCLUR_FORNECEDOR.
      "WHEN 'EDITAR'.
    WHEN 'EXCLUIR'.
      PERFORM EXCLUIR_FORNECEDOR.
    WHEN 'SAVE'.
      PERFORM SALVAR.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INCLUR_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCLUR_FORNECEDOR .
  CALL SCREEN 0200 STARTING AT 30 03.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0200'.

  IF ZLEST0191-LIFNR IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(WA_LFA1)
      FROM LFA1
     WHERE LIFNR EQ @ZLEST0191-LIFNR.
    IF SY-SUBRC IS INITIAL.
      ZDE_ZLEST0191_ALV-NAME1 = WA_LFA1-NAME1.
    ENDIF.
  ELSE.
    CLEAR: ZDE_ZLEST0191_ALV-NAME1.
  ENDIF.

  IF ZLEST0191-ID_REGIAO IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(WA_ZLEST0187)
      FROM ZLEST0187
     WHERE ID_REGIAO EQ @ZLEST0191-ID_REGIAO.
    IF SY-SUBRC IS INITIAL.
      ZDE_ZLEST0191_ALV-DS_REGIAO = WA_ZLEST0187-DS_REGIAO.
      ZDE_ZLEST0191_ALV-TP_REGIAO = WA_ZLEST0187-TP_REGIAO.
    ENDIF.
  ELSE.
    CLEAR: ZDE_ZLEST0191_ALV-DS_REGIAO, ZDE_ZLEST0191_ALV-TP_REGIAO.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200_EXIT INPUT.
  CLEAR: ZLEST0191.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE.
    WHEN 'SAVE'.

      IF ZLEST0191-LIFNR IS NOT INITIAL.
        SELECT SINGLE * INTO @WA_LFA1
          FROM LFA1
         WHERE LIFNR EQ @ZLEST0191-LIFNR.
        IF SY-SUBRC IS INITIAL.
          ZDE_ZLEST0191_ALV-NAME1 = WA_LFA1-NAME1.
        ENDIF.
      ELSE.
        CLEAR: ZDE_ZLEST0191_ALV-NAME1.
      ENDIF.

      IF ZLEST0191-ID_REGIAO IS NOT INITIAL.
        SELECT SINGLE * INTO @WA_ZLEST0187
          FROM ZLEST0187
         WHERE ID_REGIAO EQ @ZLEST0191-ID_REGIAO.
        IF SY-SUBRC IS INITIAL.
          ZDE_ZLEST0191_ALV-DS_REGIAO = WA_ZLEST0187-DS_REGIAO.
          ZDE_ZLEST0191_ALV-TP_REGIAO = WA_ZLEST0187-TP_REGIAO.
        ENDIF.
      ELSE.
        CLEAR: ZDE_ZLEST0191_ALV-DS_REGIAO, ZDE_ZLEST0191_ALV-TP_REGIAO.
      ENDIF.

      READ TABLE IT_ZLEST0191_ALV
      WITH KEY LIFNR     = ZLEST0191-LIFNR
               TP_REGIAO = ZDE_ZLEST0191_ALV-TP_REGIAO
      TRANSPORTING NO FIELDS.

      IF SY-SUBRC IS INITIAL.
        MESSAGE 'Fornecedor com esse tipo de região já vinculado' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      APPEND VALUE #( LIFNR     = ZLEST0191-LIFNR
                      NAME1     = ZDE_ZLEST0191_ALV-NAME1
                      ID_REGIAO = ZLEST0191-ID_REGIAO
                      DS_REGIAO = ZDE_ZLEST0191_ALV-DS_REGIAO
                      TP_REGIAO = ZDE_ZLEST0191_ALV-TP_REGIAO ) TO IT_ZLEST0191_ALV.

      CLEAR: OK_CODE.

      CK_ALTEROU = ABAP_TRUE.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_FORNECEDOR .

  DATA: IT_DEL TYPE TABLE OF ZLEST0191.

  ALV_01->GET_SELECTED_ROWS(
    IMPORTING
      ET_INDEX_ROWS = DATA(ET_INDEX_ROWS)
  ).

  LOOP AT ET_INDEX_ROWS INTO DATA(WA_INDEX_ROWS) WHERE ROWTYPE IS INITIAL.
    READ TABLE IT_ZLEST0191_ALV INTO DATA(WA_ZLEST0191_ALV) INDEX WA_INDEX_ROWS-INDEX.
    APPEND VALUE #( LIFNR     = WA_ZLEST0191_ALV-LIFNR
                    ID_REGIAO = WA_ZLEST0191_ALV-ID_REGIAO ) TO IT_DEL.
  ENDLOOP.

  LOOP AT IT_DEL INTO DATA(WA_DEL).
    CK_ALTEROU = ABAP_TRUE.
    DELETE IT_ZLEST0191_ALV WHERE LIFNR EQ WA_DEL-LIFNR AND ID_REGIAO = WA_DEL-ID_REGIAO.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALVAR .

  DATA: IT_SAVAR TYPE TABLE OF ZLEST0191.

  CHECK CK_ALTEROU EQ ABAP_TRUE.

  LOOP AT IT_ZLEST0191_ALV INTO DATA(WA_ZLEST0191_ALV).
    APPEND VALUE #( LIFNR     = WA_ZLEST0191_ALV-LIFNR
                    ID_REGIAO = WA_ZLEST0191_ALV-ID_REGIAO ) TO IT_SAVAR.
  ENDLOOP.

  DELETE FROM ZLEST0191.
  MODIFY ZLEST0191 FROM TABLE IT_SAVAR.
  COMMIT WORK.

  CK_ALTEROU = ABAP_FALSE.

ENDFORM.
