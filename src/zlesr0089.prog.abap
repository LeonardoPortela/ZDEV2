*&---------------------------------------------------------------------*
*& Report  ZLESR0089
*& Interface de confirmação de peso
*&---------------------------------------------------------------------*
*& Data           : 09.02.2015
*& Analista       : Leila Mara
*& Desenvolvedor  : Marcos Faneli
*&---------------------------------------------------------------------*

REPORT  ZLESR0089.

TABLES: ZLEST0019.

** Types
TYPES: BEGIN OF TY_ZLEST0019.
        INCLUDE TYPE ZLEST0019.
TYPES: CHECK TYPE C.
TYPES: END OF TY_ZLEST0019.

** Tabelas internas
DATA: GT_ZLEST0019 TYPE TABLE OF TY_ZLEST0019,
      GT_DELETED   TYPE TABLE OF ZLEST0019.

** Work Áreas
DATA: GW_ZLEST0019 TYPE TY_ZLEST0019.

** Variáveis
DATA: GV_CONT TYPE I,
      GV_ERRO TYPE I.

** ALV
DATA: OBJ_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_GRID      TYPE REF TO CL_GUI_ALV_GRID,

      GT_FIELDCAT   TYPE LVC_T_FCAT,
      GW_FIELDCAT   TYPE LVC_S_FCAT,

      GW_LAYOUT     TYPE LVC_S_LAYO,
      GW_STABLE     TYPE LVC_S_STBL,

      GT_FUNCTION   TYPE UI_FUNCTIONS,
      GW_FUNCTION   LIKE GT_FUNCTION WITH HEADER LINE,

      C_ALV_TOOLBAR TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

DEFINE MC_PREENCHE_FIELDCAT.

  CLEAR GW_FIELDCAT.
  GW_FIELDCAT-REF_TABLE     = 'ZLEST0019'.
  GW_FIELDCAT-REF_FIELD     = &1.
  GW_FIELDCAT-FIELDNAME     = &1.
  GW_FIELDCAT-DATATYPE      = &2.
*  GW_FIELDCAT-COLTEXT       = &3.
  GW_FIELDCAT-JUST          = &3.
  GW_FIELDCAT-KEY           = &4.
*  GW_FIELDCAT-EDIT          = &5.
*  GW_FIELDCAT-OUTPUTLEN     = &7.
  APPEND GW_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    METHODS:
          CONSTRUCTOR
            IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,

          ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING  E_OBJECT,

          HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM.
ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA : WL_TOOLBAR TYPE STB_BUTTON.

*    WL_TOOLBAR-ICON      =  ICON_INSERT_ROW.
*    WL_TOOLBAR-FUNCTION  = 'ADD'.
*    WL_TOOLBAR-TEXT      = ' Inserir'.
*    APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    WL_TOOLBAR-ICON      = ICON_DELETE_ROW.
    WL_TOOLBAR-FUNCTION  = 'DEL'.
    WL_TOOLBAR-TEXT      = ' Excluir'.
    APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&INSERT_ROW'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&DELETE_ROW'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&APPEND'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&UNDO'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&CUT'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&PASTE'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&COPY'.
    DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&COPY_ROW'.

    CALL METHOD C_ALV_TOOLBAR->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    DATA: TL_SELECTED_ROWS TYPE LVC_T_ROW,
          WL_SELECTED_ROWS TYPE LVC_S_ROW.

    CASE E_UCOMM.
      WHEN 'ADD'.
        APPEND INITIAL LINE TO GT_ZLEST0019.

        CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = GW_STABLE.

      WHEN 'DEL'.
        CALL METHOD OBJ_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_SELECTED_ROWS.

        LOOP AT TL_SELECTED_ROWS INTO WL_SELECTED_ROWS.
          READ TABLE GT_ZLEST0019 INTO GW_ZLEST0019 INDEX WL_SELECTED_ROWS-INDEX.
          APPEND GW_ZLEST0019 TO GT_DELETED.
          DELETE GT_ZLEST0019 INDEX WL_SELECTED_ROWS-INDEX.
        ENDLOOP.

        CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "LCL_ALV_TOOLBAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLE DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
       ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
          IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

ENDCLASS.                    "LCL_EVENT_HANDLE DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLE IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLE IMPLEMENTATION.
  METHOD ON_DATA_CHANGED.
    DATA: WL_GOOD   TYPE LVC_S_MODI,
          LV_CAMPO  TYPE C LENGTH 6.

    FIELD-SYMBOLS: <FS_0019>  TYPE TY_ZLEST0019,
                   <FS_FIELD> TYPE ANY.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO WL_GOOD.

      READ TABLE GT_ZLEST0019 ASSIGNING <FS_0019> INDEX WL_GOOD-ROW_ID.
      ASSIGN COMPONENT WL_GOOD-FIELDNAME OF STRUCTURE <FS_0019> TO <FS_FIELD>.
      <FS_FIELD> = WL_GOOD-VALUE.
      <FS_0019>-CHECK = ABAP_TRUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = WL_GOOD-ROW_ID
          I_FIELDNAME = WL_GOOD-FIELDNAME
          I_VALUE     = WL_GOOD-VALUE.

    ENDLOOP.

    UNASSIGN: <FS_0019>, <FS_FIELD>.
  ENDMETHOD.                    "ON_DATA_CHANGED
ENDCLASS.                    "LCL_EVENT_HANDLE IMPLEMENTATION

DATA: OBJ_TOOLBAR   TYPE REF TO LCL_ALV_TOOLBAR.


SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_INTER FOR ZLEST0019-IDINTER OBLIGATORY NO INTERVALS NO-EXTENSION,
                P_BUKRS FOR ZLEST0019-BUKRS   NO INTERVALS,
                P_BRANC FOR ZLEST0019-BRANCH,
                P_NFENU FOR ZLEST0019-NFENUM NO INTERVALS,
                P_NFNUM FOR ZLEST0019-NR_NF_TERCEIRO NO INTERVALS,
                P_VAGAO FOR ZLEST0019-IDVAGAO NO INTERVALS,
                P_DCL   FOR ZLEST0019-DCL NO INTERVALS,
                P_UNAME FOR ZLEST0019-UNAME NO INTERVALS NO-EXTENSION,
                P_ERDAT FOR ZLEST0019-ERDAT.
SELECTION-SCREEN: END OF BLOCK A1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

  ENDLOOP.

START-OF-SELECTION.
  CLEAR: GV_CONT, GV_ERRO.
  IF P_INTER IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_BUKRS IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_BRANC IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_NFNUM IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_NFENU IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_VAGAO IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_DCL IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_UNAME IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.
  IF P_ERDAT  IS NOT INITIAL.
    ADD 1 TO GV_CONT.
  ENDIF.

  IF GV_CONT EQ 1.
    MESSAGE TEXT-E01 TYPE 'I' DISPLAY LIKE 'E'.
    ADD 1 TO GV_ERRO.
  ELSE.
    IF  P_INTER[] IS NOT INITIAL
    AND P_NFENU[] IS NOT INITIAL
    AND P_BUKRS[] IS INITIAL
    AND P_BRANC[] IS INITIAL.
      MESSAGE TEXT-E02 TYPE 'I' DISPLAY LIKE 'E'.
      ADD 1 TO GV_ERRO.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
  IF GV_ERRO IS INITIAL.
    PERFORM: F_SELECIONAR_DADOS,
*           F_ORGANIZAR_DADOS,
             F_IMPRIMIR_DADOS.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONAR_DADOS .
  DATA: LV_TP_REG   TYPE RANGE OF ZLEST0019-TP_REG  WITH HEADER LINE,
        LV_TP_MOVI  TYPE RANGE OF ZLEST0019-TP_MOVI WITH HEADER LINE,

        LV_TP_REG2  TYPE RANGE OF ZLEST0019-TP_REG  WITH HEADER LINE,
        LV_TP_MOVI2 TYPE RANGE OF ZLEST0019-TP_MOVI WITH HEADER LINE,

        LV_IDINTER  TYPE ZLEST0019-IDINTER,

        LV_DCL      TYPE ZLEST0019-DCL,
        LV_NFE      TYPE ZLEST0019-NFENUM,
        LV_NF_TER   TYPE ZLEST0019-NR_NF_TERCEIRO,
        LV_VAGAO    TYPE ZLEST0019-IDVAGAO,
        LV_CONT     TYPE I.

  FREE GT_DELETED.

  IF P_ERDAT IS NOT INITIAL.
    CASE P_INTER-LOW.
      WHEN 'L1'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '30'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'E'.
        APPEND LV_TP_MOVI2.

        CLEAR LV_TP_REG2.
        LV_TP_REG2-SIGN   = 'I'.
        LV_TP_REG2-OPTION = 'EQ'.
        LV_TP_REG2-LOW    = '10'.
        APPEND LV_TP_REG2.

        LV_IDINTER  = 'L1'.
      WHEN 'L2'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'S'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '30'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '20'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'S'.
        APPEND LV_TP_MOVI2.

        CLEAR LV_TP_REG2.
        LV_TP_REG2-SIGN   = 'I'.
        LV_TP_REG2-OPTION = 'EQ'.
        LV_TP_REG2-LOW    = '10'.
        APPEND LV_TP_REG2.

        LV_IDINTER  = 'L2'.

      WHEN 'L3'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '20'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '30'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'E'.
        APPEND LV_TP_MOVI2.

        CLEAR LV_TP_REG2.
        LV_TP_REG2-SIGN   = 'I'.
        LV_TP_REG2-OPTION = 'EQ'.
        LV_TP_REG2-LOW    = '10'.
        APPEND LV_TP_REG2.

        LV_IDINTER  = 'L3'.

    ENDCASE.
  ELSEIF P_NFENU IS NOT INITIAL
      OR P_NFNUM IS NOT INITIAL.
    CASE P_INTER-LOW.
      WHEN 'L1'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '30'.
        APPEND LV_TP_REG.

      WHEN 'L2'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'S'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '30'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'S'.
        APPEND LV_TP_MOVI2.

        CLEAR LV_TP_REG2.
        LV_TP_REG2-SIGN   = 'I'.
        LV_TP_REG2-OPTION = 'EQ'.
        LV_TP_REG2-LOW    = '20'.
        APPEND LV_TP_REG2.

        LV_IDINTER  = 'L2'.

      WHEN 'L3'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN   = 'I'.
        LV_TP_REG-OPTION = 'EQ'.
        LV_TP_REG-LOW    = '30'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'E'.
        APPEND LV_TP_MOVI2.

        CLEAR LV_TP_REG2.
        LV_TP_REG2-SIGN   = 'I'.
        LV_TP_REG2-OPTION = 'EQ'.
        LV_TP_REG2-LOW    = '20'.
        APPEND LV_TP_REG2.

        LV_IDINTER  = 'L3'.

    ENDCASE.
  ELSEIF P_DCL IS NOT INITIAL.
    CASE P_INTER-LOW.
      WHEN 'L2'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'S'.
        APPEND LV_TP_MOVI.

      WHEN 'L3'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

    ENDCASE.
  ELSEIF P_VAGAO IS NOT INITIAL.
    CASE P_INTER-LOW.
      WHEN 'L2'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'S'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '20'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW    = 'S'.
        APPEND LV_TP_MOVI2.

        LV_IDINTER  = 'L2'.

      WHEN 'L3'.
        CLEAR LV_TP_MOVI.
        LV_TP_MOVI-SIGN   = 'I'.
        LV_TP_MOVI-OPTION = 'EQ'.
        LV_TP_MOVI-LOW    = 'E'.
        APPEND LV_TP_MOVI.

        CLEAR LV_TP_REG.
        LV_TP_REG-SIGN    = 'I'.
        LV_TP_REG-OPTION  = 'EQ'.
        LV_TP_REG-LOW     = '20'.
        APPEND LV_TP_REG.

        CLEAR LV_TP_MOVI2.
        LV_TP_MOVI2-SIGN   = 'I'.
        LV_TP_MOVI2-OPTION = 'EQ'.
        LV_TP_MOVI2-LOW = 'S'.
        APPEND LV_TP_MOVI2.

        LV_IDINTER  = 'L3'.

    ENDCASE.
  ENDIF.

  IF P_NFENU[] IS NOT INITIAL.
    LOOP AT P_NFENU.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_NFENU-LOW
        IMPORTING
          OUTPUT = LV_NFE.

      P_NFENU-LOW = LV_NFE.

      MODIFY P_NFENU INDEX SY-TABIX.

    ENDLOOP.
  ENDIF.

  IF P_NFNUM[] IS NOT INITIAL.
    LOOP AT P_NFNUM.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_NFNUM-LOW
        IMPORTING
          OUTPUT = LV_NF_TER.

      P_NFNUM-LOW = LV_NF_TER.

      MODIFY P_NFNUM INDEX SY-TABIX.

    ENDLOOP.
  ENDIF.

  IF  P_DCL[] IS NOT INITIAL
  AND P_INTER-LOW EQ 'L2'.
    LOOP AT P_DCL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_DCL-LOW
        IMPORTING
          OUTPUT = LV_DCL.

      P_DCL-LOW = LV_DCL.

      MODIFY P_DCL INDEX SY-TABIX.

    ENDLOOP.
  ENDIF.

  IF  P_VAGAO[] IS NOT INITIAL
  AND P_INTER EQ 'L3'.
    LOOP AT P_VAGAO.
      LV_VAGAO = P_VAGAO-LOW.
      LV_CONT = STRLEN( P_VAGAO-LOW ) - 1.
      LV_VAGAO = LV_VAGAO(LV_CONT).
      P_VAGAO-LOW = LV_VAGAO.

      MODIFY P_VAGAO INDEX SY-TABIX.

    ENDLOOP.
  ENDIF.

  SELECT *
    FROM ZLEST0019
    INTO TABLE GT_ZLEST0019
    WHERE BUKRS           IN P_BUKRS
     AND  IDINTER         IN P_INTER
     AND  BRANCH          IN P_BRANC
     AND  DCL             IN P_DCL
     AND  UNAME           IN P_UNAME
     AND  NR_NF_TERCEIRO  IN P_NFNUM
     AND  NFENUM          IN P_NFENU
     AND  IDVAGAO         IN P_VAGAO
     AND  ERDAT           IN P_ERDAT
     AND  TP_REG          IN LV_TP_REG
     AND  TP_MOVI         IN LV_TP_MOVI.

  IF GT_ZLEST0019 IS NOT INITIAL.
    IF P_ERDAT IS NOT INITIAL.

      IF P_VAGAO IS NOT INITIAL.
        SORT GT_ZLEST0019 BY ID_REFKEY.
        DELETE GT_ZLEST0019 WHERE ID_REFKEY = ''.
      ENDIF.

      SELECT *
        APPENDING TABLE GT_ZLEST0019
        FROM ZLEST0019
        FOR ALL ENTRIES IN GT_ZLEST0019
        WHERE IDINTER      EQ LV_IDINTER
         AND  ERDAT        IN P_ERDAT
         AND  TP_REG       IN LV_TP_REG2
         AND  TP_MOVI      IN LV_TP_MOVI2
         AND  ID_ZLEST0019 EQ GT_ZLEST0019-ID_REFKEY.

    ENDIF.

    IF  ( P_NFENU IS NOT INITIAL OR P_NFNUM IS NOT INITIAL )
    AND P_INTER-LOW NE 'L1'.

      SELECT *
        APPENDING TABLE GT_ZLEST0019
        FROM ZLEST0019
        FOR ALL ENTRIES IN GT_ZLEST0019
        WHERE IDINTER   EQ LV_IDINTER
         AND  ERDAT     IN P_ERDAT
         AND  TP_REG    IN LV_TP_REG2
         AND  TP_MOVI   IN LV_TP_MOVI2
         AND  ID_REFKEY EQ GT_ZLEST0019-ID_REFKEY.

    ENDIF.

    IF P_VAGAO IS NOT INITIAL.
      SORT GT_ZLEST0019 BY ID_REFKEY.
      DELETE GT_ZLEST0019 WHERE ID_REFKEY = ''.

      SELECT *
        APPENDING TABLE GT_ZLEST0019
        FROM ZLEST0019
        FOR ALL ENTRIES IN GT_ZLEST0019
        WHERE IDINTER      EQ LV_IDINTER
         AND  ERDAT        IN P_ERDAT
         AND  TP_REG       IN LV_TP_REG2
         AND  TP_MOVI      IN LV_TP_MOVI2
         AND  DCL          EQ GT_ZLEST0019-DCL.

    ENDIF.

    CALL SCREEN 0100.

  ELSE.
    MESSAGE TEXT-E03 TYPE 'I' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.                    " F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ORGANIZAR_DADOS .

ENDFORM.                    " F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIMIR_DADOS .
  IF OBJ_CONT IS INITIAL.
    CLEAR: GW_LAYOUT, GW_STABLE.

    GW_LAYOUT-ZEBRA      = ABAP_TRUE.
*    GW_LAYOUT-NO_ROWMARK = ABAP_TRUE.
    GW_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
    GW_LAYOUT-SEL_MODE   = 'A'.
    GW_STABLE-ROW        = ABAP_TRUE.

    CREATE OBJECT OBJ_CONT
      EXPORTING
        CONTAINER_NAME = 'OBJ_CONT'.

    CREATE OBJECT OBJ_GRID
      EXPORTING
        I_PARENT = OBJ_CONT.

    CREATE OBJECT OBJ_TOOLBAR
      EXPORTING
        IO_ALV_GRID = OBJ_GRID.

    SET HANDLER: OBJ_TOOLBAR->ON_TOOLBAR FOR OBJ_GRID,
                 OBJ_TOOLBAR->HANDLE_USER_COMMAND FOR OBJ_GRID.

    PERFORM: F_MONTAR_LAYOUT.

    CALL METHOD OBJ_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GW_LAYOUT
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = GT_FIELDCAT[]
        IT_OUTTAB       = GT_ZLEST0019[].

    CALL METHOD OBJ_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD OBJ_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_HANDLE=>ON_DATA_CHANGED FOR OBJ_GRID.

  ELSE.
    CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GW_STABLE.

  ENDIF.
ENDFORM.                    " F_IMPRIMIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT.
  MC_PREENCHE_FIELDCAT:
*        'MANDT         '  'CHAR'      'L'   ' '   ' ',
            'IDINTER       '  'CHAR'      'L'   ' ',
        'TP_MOVI       '  'CHAR'      'L'   ' ',
        'TP_REG        '  'CHAR'      'L'   ' ',
        'CHAVE         '  'CHAR'      'L'   ' ',
        'DCL           '  'CHAR'      'L'   ' ',
        'SERIEDCL      '  'CHAR'      'L'   ' ',
        'CNPJFERRO     '  'CHAR'      'L'   ' ',
        'NOMEMPFERRO   '  'CHAR'      'L'   ' ',
        'DTAENVIO      '  'DATS'      'L'   ' ',
        'HORAENVIO     '  'TIME'      'L'   ' ',
        'OBS           '  'CHAR'      'L'   ' ',
        'IDVAGAO       '  'CHAR'      'L'   ' ',
        'PESOVAGAO     '  'CURR'      'L'   ' ',
        'DTADECARGA    '  'DATS'      'L'   ' ',
        'HORADESCARGA  '  'TIME'      'L'   ' ',
        'CNPJCLIENTE   '  'CHAR'      'L'   ' ',
        'BUKRS         '  'CHAR'      'L'   ' ',
        'BRANCH        '  'CHAR'      'L'   ' ',
        'NFENUM        '  'CHAR'      'L'   ' ',
        'NFNUM         '  'CHAR'      'L'   ' ',
        'PESONF        '  'CURR'      'L'   ' ',
        'PESODVAGAO    '  'CHAR'      'L'   ' ',
        'DTACHEGADA    '  'DATS'      'L'   ' ',
        'PRODUTO       '  'CHAR'      'L'   ' ',
        'ERDAT         '  'DATS'      'L'   ' ',
        'ERZET         '  'TIME'      'L'   ' ',
        'UNAME         '  'CHAR'      'L'   ' ',
        'NR_NF_TERCEIRO'  'CHAR'      'L'   ' ',
        'COD_FORNECEDOR'  'CHAR'      'L'   ' ',
        'ID_ZLEST0019  '  'CHAR'      'L'   ' ',
        'ID_REFKEY     '  'CHAR'      'L'   ' ',
        'STATUS_DUPLICA'  'CHAR'      'L'   ' ',
        'OBSERVACAO    '  'CHAR'      'L'   ' '.

ENDFORM.                    "f_monta_layout
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: LV_CONT TYPE I,
        LV_ANSWER.

  FIELD-SYMBOLS: <FS_0019>  TYPE TY_ZLEST0019.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      READ TABLE GT_ZLEST0019 INTO GW_ZLEST0019 WITH KEY CHECK = ABAP_TRUE.
      IF SY-SUBRC IS INITIAL.
        ADD 1 TO LV_CONT.
      ENDIF.

      IF GT_DELETED IS NOT INITIAL.
        ADD 1 TO LV_CONT.
      ENDIF.

      IF LV_CONT IS NOT INITIAL.
        CLEAR LV_CONT.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Salvar alterações'
            TEXT_QUESTION         = 'Deseja sair sem salvar as alterações?'
            TEXT_BUTTON_1         = 'Sim'
            ICON_BUTTON_1         = 'ICON_CHECKED'
            TEXT_BUTTON_2         = 'Não'
            ICON_BUTTON_2         = 'ICON_CANCEL'
            DISPLAY_CANCEL_BUTTON = ' '
            POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
          IMPORTING
            ANSWER                = LV_ANSWER.

        IF LV_ANSWER EQ '1'.
          LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'SAVE'.
** Remove registros
      IF GT_DELETED IS NOT INITIAL.
        DELETE ZLEST0019 FROM TABLE GT_DELETED.
      ENDIF.

      FREE GT_DELETED.

** Atualiza registros
      LOOP AT GT_ZLEST0019 ASSIGNING <FS_0019> WHERE CHECK EQ ABAP_TRUE.
        MODIFY ZLEST0019 FROM <FS_0019>.
        CLEAR <FS_0019>-CHECK.
      ENDLOOP.

      MESSAGE TEXT-S01 TYPE 'I' DISPLAY LIKE 'S'.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_CRIAR_OBJETO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_CRIAR_OBJETO OUTPUT.
  PERFORM F_IMPRIMIR_DADOS.
ENDMODULE.                 " MO_CRIAR_OBJETO  OUTPUT
