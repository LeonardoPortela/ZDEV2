*&---------------------------------------------------------------------*
*& Report  ZAA16
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZAA16 MESSAGE-ID ZAA.

TABLES: T093B.

DATA: DG_SPLITTER     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV         TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT       TYPE LVC_S_LAYO,
      GS_VARIANT      TYPE DISVARIANT,
      IT_FIELDCATALOG TYPE LVC_T_FCAT.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS FOR T093B-BUKRS MODIF ID SO1 NO INTERVALS NO-EXTENSION OBLIGATORY,
                P_EXEPU FOR T093B-ABGJA NO INTERVALS NO-EXTENSION MODIF ID SO2, "Exercício - A ser encerrado
                P_EXEPA FOR T093B-ABGJA NO INTERVALS NO-EXTENSION MODIF ID SO2. "Exercício - Último Encerrado
SELECTION-SCREEN END OF BLOCK B2.

AT SELECTION-SCREEN ON P_BUKRS.

  CLEAR: P_EXEPU[], P_EXEPA[], P_EXEPU, P_EXEPA.

  SELECT SINGLE * INTO @DATA(WA_T093B)
    FROM T093B
   WHERE BUKRS EQ @P_BUKRS-LOW.

  IF SY-SUBRC IS INITIAL.

    "P_EXEPA  Exercício - Último Encerrado
    P_EXEPA-SIGN   = 'I'.
    P_EXEPA-OPTION = 'EQ'.
    P_EXEPA-LOW    = WA_T093B-ABGJA.
    APPEND P_EXEPA.

    "P_EXEPU  Exercício - A ser encerrado
    P_EXEPU-SIGN   = 'I'.
    P_EXEPU-OPTION = 'EQ'.
    P_EXEPU-LOW    = WA_T093B-ABGJA  + 1.
    APPEND P_EXEPU.

    IF P_EXEPU-LOW GE SY-DATUM(4).
      MESSAGE E006.
    ENDIF.

  ELSE.
    MESSAGE E005 WITH P_BUKRS-LOW.
  ENDIF.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'SO2'.
      SCREEN-INPUT = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF P_EXEPU[] IS NOT INITIAL AND P_BUKRS[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(IT_T093B)
      FROM T093B
     WHERE ABGJA IN @P_EXEPU
       AND BUKRS IN @P_BUKRS.

    IF SY-SUBRC IS INITIAL.
      CLEAR: P_EXEPU[], P_EXEPA[], P_BUKRS, P_EXEPU, P_EXEPA.
    ENDIF.
  ENDIF.

INITIALIZATION.

  CLEAR: P_BUKRS[], P_EXEPU[], P_EXEPA[], P_BUKRS, P_EXEPU, P_EXEPA.

START-OF-SELECTION.

  "Seleciona Ecerramento Anterior
  SELECT * INTO TABLE @DATA(IT_T093B)
    FROM T093B
   WHERE ABGJA IN @P_EXEPA
     AND BUKRS IN @P_BUKRS.

  "Seleciona Ecerramento Anterior
  SELECT * INTO TABLE @DATA(IT_T093B_U)
    FROM T093B
   WHERE ABGJA IN @P_EXEPU
     AND BUKRS IN @P_BUKRS.

  IF SY-SUBRC IS INITIAL.
    MESSAGE E002 WITH P_EXEPA-LOW P_BUKRS-LOW. "Exercício &1 Encerrado para Empresa &2!
  ENDIF.

END-OF-SELECTION.

  DATA: ANSWER TYPE C.

  "Ajusta Para Encerramento Atual
  LOOP AT IT_T093B ASSIGNING FIELD-SYMBOL(<FS_T093B>).
    <FS_T093B>-ABGJA = P_EXEPU-LOW.
  ENDLOOP.

  IF IT_T093B[] IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(WA_T001)
      FROM T001
     WHERE BUKRS EQ @P_BUKRS-LOW.

    DATA(LC_TEXT_QUESTION) = |Deseja realmente encerrar o período { P_EXEPA-LOW } da empresa { P_BUKRS-LOW } - { ZCL_STR=>INITCAP( I_TEXTO = CONV #( WA_T001-BUTXT ) )->GET( ) }?|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Encerramento de Período'
        TEXT_QUESTION         = LC_TEXT_QUESTION
        TEXT_BUTTON_1         = 'Sim'
        ICON_BUTTON_1         = 'ICON_CHECKED'
        TEXT_BUTTON_2         = 'Não'
        ICON_BUTTON_2         = 'ICON_INCOMPLETE'
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    IF ANSWER EQ '1'.

      "Atualiza tabela para Encerramento Atual
      MODIFY T093B FROM TABLE IT_T093B.
      COMMIT WORK AND WAIT.
      MESSAGE S002 WITH P_EXEPA-LOW P_BUKRS-LOW.

      CLEAR: IT_T093B[].

      SELECT * INTO TABLE @IT_T093B
        FROM T093B
       WHERE ABGJA IN @P_EXEPU
         AND BUKRS IN @P_BUKRS.

      CALL SCREEN 0100.

    ENDIF.

  ELSE.

    MESSAGE S004 WITH P_EXEPU-LOW P_BUKRS-LOW.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0110' WITH P_EXEPA-LOW P_BUKRS-LOW.

  IF DG_SPLITTER IS INITIAL.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 1.

    CTL_CCCONTAINER = DG_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM FILL_IT_FIELDCATALOG.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        IS_VARIANT      = GS_VARIANT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_T093B[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  IF CTL_ALV IS NOT INITIAL.
    CTL_ALV->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV.

  IF DG_SPLITTER IS NOT INITIAL.
    DG_SPLITTER->FREE( ).
  ENDIF.
  CLEAR: DG_SPLITTER.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  CLEAR: IT_FIELDCATALOG[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'T093B'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.
