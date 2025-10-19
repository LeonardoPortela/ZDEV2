*----------------------------------------------------------------------*
***INCLUDE LZLESO02 .
*----------------------------------------------------------------------*

CONSTANTS: OK_SAVE_0101      TYPE SY-UCOMM VALUE 'SAVE',
           OK_CANCEL_0101    TYPE SY-UCOMM VALUE 'CANCEL',
           C_X_0101          TYPE C LENGTH 1 VALUE 'X',
           C_GRID_COLOR_C300 TYPE C LENGTH 04 VALUE 'C300',
           C_GRID_COLOR_C400 TYPE C LENGTH 04 VALUE 'C400'.

DATA:      OK_CODE_0101    TYPE SY-UCOMM,
           VG_LEITURA      TYPE C LENGTH 1,
           VG_CANCELADO    TYPE C LENGTH 1,
           IT_IMP_RETIDOS  TYPE TABLE OF ZLES0043_IMP_RETIDOS WITH HEADER LINE,
           WA_IMP_RETIDOS  TYPE ZLES0043_IMP_RETIDOS,
           VG_PRIM_IMP_RET TYPE CHAR01,
           VG_TEXT_EMPRESA TYPE BUTXT,
           VG_TEXT_FORN    TYPE NAME1_GP.


*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_AFTER_USER_COMMAND
                  FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM
                  E_SAVED
                  E_NOT_PROCESSED.
ENDCLASS.                    "lcl_event_handler DEFINITION

*&---------------------------------------------------------------------*
*& Variáveis de tela
*&---------------------------------------------------------------------*

DATA: SCROLL_COL_IMP_RETIDOS TYPE LVC_S_COL,
      SCROLL_ROW_IMP_RETIDOS TYPE LVC_S_ROID,
      GS_LAYOUT_IMP_RETIDOS  TYPE LVC_S_LAYO,
      CATALOGO_IMP_RETIDOS   TYPE LVC_T_FCAT,
      CONTAINER_IMP_RETIDOS  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_IMP_RETIDOS        TYPE REF TO CL_GUI_ALV_GRID,
      EVENT_HANDLER_IMP_RET  TYPE REF TO LCL_EVENT_HANDLER.
*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD HANDLE_AFTER_USER_COMMAND.

    IF ( E_UCOMM EQ '&CHECK' ) OR ( E_UCOMM EQ '&REFRESH' ).
      LOOP AT IT_IMP_RETIDOS INTO WA_IMP_RETIDOS.
        IF ( WA_IMP_RETIDOS-BASE GT 0 ) AND ( WA_IMP_RETIDOS-RATE GT 0 ).
          WA_IMP_RETIDOS-TAXVAL = WA_IMP_RETIDOS-BASE * ( WA_IMP_RETIDOS-RATE / 100 ).
        ELSE.
          WA_IMP_RETIDOS-TAXVAL = 0.
        ENDIF.
        MODIFY IT_IMP_RETIDOS FROM WA_IMP_RETIDOS INDEX SY-TABIX TRANSPORTING TAXVAL.
      ENDLOOP.

      CALL METHOD ALV_IMP_RETIDOS->REFRESH_TABLE_DISPLAY.
    ENDIF.
  ENDMETHOD.                    "handle_after_user_command
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CARREGA_IMPOSTOS_RETIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IMP_RETIDOS  text
*      -->P_P_VISUALIZA  text
*----------------------------------------------------------------------*
FORM CARREGA_IMPOSTOS_RETIDOS  TABLES   P_IMP_RETIDOS STRUCTURE ZLES0043_IMP_RETIDOS
                               USING    P_BUKRS       TYPE BUKRS
                                        P_LIFNR       TYPE LIFNR
                                        P_P_VISUALIZA TYPE C
                                        P_P_CANCELADO TYPE C.
  VG_LEITURA = P_P_VISUALIZA.
  MOVE P_IMP_RETIDOS[] TO IT_IMP_RETIDOS[].

  SELECT SINGLE BUTXT INTO VG_TEXT_EMPRESA
    FROM T001
   WHERE BUKRS EQ P_BUKRS.

  SELECT SINGLE NAME1 INTO VG_TEXT_FORN
    FROM LFA1
   WHERE LIFNR EQ P_LIFNR.

  CLEAR: VG_CANCELADO.

  CALL SCREEN 0101 STARTING AT 20 05 ENDING AT 93 15.

  P_P_CANCELADO = VG_CANCELADO.
  MOVE IT_IMP_RETIDOS[] TO P_IMP_RETIDOS[].

ENDFORM.                    " CARREGA_IMPOSTOS_RETIDOS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  SET TITLEBAR 'TL0101'.

  SET PF-STATUS 'PF0101'.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.
  LEAVE TO SCREEN 0.
  VG_CANCELADO = C_X_0101.
ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  LOOP AT IT_IMP_RETIDOS.
    IF ( IT_IMP_RETIDOS-BASE GT 0 ) AND ( IT_IMP_RETIDOS-RATE GT 0 ).
      IT_IMP_RETIDOS-TAXVAL = IT_IMP_RETIDOS-BASE * ( IT_IMP_RETIDOS-RATE / 100 ).
    ELSE.
      IT_IMP_RETIDOS-TAXVAL = 0.
    ENDIF.
    MODIFY IT_IMP_RETIDOS INDEX SY-TABIX TRANSPORTING TAXVAL.
  ENDLOOP.

  CALL METHOD ALV_IMP_RETIDOS->REFRESH_TABLE_DISPLAY.

  CASE OK_CODE_0101.
    WHEN OK_SAVE_0101.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0101.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_IMPOSTOS_RETIDOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_ALV_IMPOSTOS_RETIDOS OUTPUT.

  PERFORM PLAN_CRIA_IMPOSTOS_RETIDOS.

ENDMODULE.                 " CRIA_ALV_IMPOSTOS_RETIDOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_IMPOSTOS_RETIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PLAN_CRIA_IMPOSTOS_RETIDOS .

  CONSTANTS: TABELA_IMP_RET TYPE STRING VALUE 'IT_IMP_RETIDOS'.

  DATA: TEXT_N000        TYPE C LENGTH 50 VALUE 'Gerar',
        TEXT_N001        TYPE C LENGTH 50 VALUE 'Imp',
        TEXT_N002        TYPE C LENGTH 50 VALUE 'Descrição',
        TEXT_N003        TYPE C LENGTH 50 VALUE 'Valor Base',
        TEXT_N004        TYPE C LENGTH 50 VALUE 'Perc.',
        TEXT_N005        TYPE C LENGTH 50 VALUE 'Valor Imposto',
        IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
        WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

  IF VG_PRIM_IMP_RET IS INITIAL.

    CREATE OBJECT CONTAINER_IMP_RETIDOS
      EXPORTING
        CONTAINER_NAME = 'CTN_IMP_RET'.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = ALV_IMP_RETIDOS.

    CREATE OBJECT ALV_IMP_RETIDOS
      EXPORTING
        I_PARENT = CONTAINER_IMP_RETIDOS.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_IMP_RETIDOS USING:
        TABELA_IMP_RET 'MARK'   TEXT_N000 SPACE 01 05 'X'   SPACE SPACE SPACE SPACE SPACE             'X'   'X',
        TABELA_IMP_RET 'WITHT'  TEXT_N001 SPACE 02 05 'X'   SPACE SPACE SPACE 'X'   SPACE             SPACE SPACE,
        TABELA_IMP_RET 'TEXT40' TEXT_N002 SPACE 03 20 SPACE SPACE SPACE SPACE 'X'   SPACE             SPACE SPACE,
        TABELA_IMP_RET 'BASE'   TEXT_N003 SPACE 04 15 SPACE SPACE SPACE SPACE SPACE C_GRID_COLOR_C300 'X'   SPACE,
        TABELA_IMP_RET 'RATE'   TEXT_N004 SPACE 05 05 SPACE SPACE SPACE SPACE SPACE C_GRID_COLOR_C300 'X'   SPACE,
        TABELA_IMP_RET 'TAXVAL' TEXT_N005 SPACE 06 15 SPACE SPACE 'X'   SPACE SPACE C_GRID_COLOR_C400 SPACE SPACE.

    CLEAR: GS_LAYOUT_IMP_RETIDOS.
    GS_LAYOUT_IMP_RETIDOS-ZEBRA      = C_X_0101.
    GS_LAYOUT_IMP_RETIDOS-SEL_MODE   = SPACE.
    GS_LAYOUT_IMP_RETIDOS-EDIT_MODE  = C_X_0101.

    WA_EXCLUDE_FCODE = '&LOCAL&CUT'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&INSERT_ROW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&MOVE_ROW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&PASTE'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&PASTE_NEW_ROW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&UNDO'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&VARI_ADMIN'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&APPEND'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&DELETE_ROW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&COPY'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&LOCAL&COPY_ROW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&VLOTUS'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&AQW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

    WA_EXCLUDE_FCODE = '&PRINT'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&MB_SUM'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&AVERAGE'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&MB_VIEW'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&MB_EXPORT'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&MB_FILTER'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&GRAPH'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = '&INFO'.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

    CALL METHOD ALV_IMP_RETIDOS->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT            = SPACE
        IS_LAYOUT            = GS_LAYOUT_IMP_RETIDOS
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = CATALOGO_IMP_RETIDOS
        IT_OUTTAB            = IT_IMP_RETIDOS[].

    CREATE OBJECT EVENT_HANDLER_IMP_RET.
    SET HANDLER EVENT_HANDLER_IMP_RET->HANDLE_AFTER_USER_COMMAND FOR ALV_IMP_RETIDOS.

    CALL METHOD ALV_IMP_RETIDOS->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    VG_PRIM_IMP_RET = C_X_0101.
  ENDIF.

  CALL METHOD ALV_IMP_RETIDOS->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_IMP_RETIDOS->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = SCROLL_COL_IMP_RETIDOS
      IS_ROW_NO   = SCROLL_ROW_IMP_RETIDOS.

ENDFORM.                    " PLAN_CRIA_IMPOSTOS_RETIDOS



*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM Z_ESTRUTURA_FIELDCAT TABLES IT_CATALOGO TYPE LVC_T_FCAT
                           USING P_TAB_NAME
                                 P_FIELDNAME
                                 P_TEXTO_GRANDE
                                 P_HOT
                                 P_POSICAO
                                 P_OUTPUTLEN
                                 P_FIX_COLUMN
                                 P_CONVEXIT
                                 P_DO_SUM
                                 P_ICON
                                 P_JUST
                                 P_EMPHASIZE
                                 P_EDIT
                                 P_CHECKBOX.

  DATA CATALOG TYPE LVC_S_FCAT.
  CATALOG-TABNAME     = P_TAB_NAME.
  CATALOG-FIELDNAME   = P_FIELDNAME.
  CATALOG-SCRTEXT_L   = P_TEXTO_GRANDE.
  CATALOG-SCRTEXT_M   = P_TEXTO_GRANDE.
  CATALOG-SCRTEXT_S   = P_TEXTO_GRANDE.
  CATALOG-HOTSPOT     = P_HOT.
  CATALOG-COL_POS     = P_POSICAO.
  CATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  CATALOG-FIX_COLUMN  = P_FIX_COLUMN.
  CATALOG-CONVEXIT    = P_CONVEXIT.
  CATALOG-DO_SUM      = P_DO_SUM.
  CATALOG-ICON        = P_ICON.
  CATALOG-JUST        = P_JUST.
  CATALOG-EMPHASIZE   = P_EMPHASIZE.
  IF VG_LEITURA EQ C_X_0101.
    CATALOG-EDIT        = SPACE.
  ELSE.
    CATALOG-EDIT        = P_EDIT.
  ENDIF.
  CATALOG-CHECKBOX    = P_CHECKBOX.
  IF NOT P_EMPHASIZE IS INITIAL.
    CATALOG-CURRENCY  = 'BRL'.
  ENDIF.

  APPEND CATALOG TO IT_CATALOGO.
ENDFORM.                    " Z_ESTRUTURA_FIELDCAT
