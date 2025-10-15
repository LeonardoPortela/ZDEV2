************************************************************************
* Program        : ZOPENSIS_REL_004                                    *
* Transaction    : ZOPENSIS_009                                        *
* Title          : Relatório do Gestor                                 *
* Developer      : Fernando Oliveira                                   *
* Date           : 10/08/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     10/08/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
REPORT  ZOPENSIS_REL_004.

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
TABLES: ZTOPENSIS_006, SSCRFIELDS.

*======================================================================*
*         T   Y   P   E   S                                            *
*======================================================================*
TYPES: BEGIN OF TY_OUT,
         ZCOD_ROLE_COMPOSTO TYPE ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO,
         ZDSC_CARGO         TYPE ZTOPENSIS_006-ZDSC_CARGO,
         ZCOD_USER          TYPE ZTOPENSIS_006-ZCOD_USER,
         ZDSC_USER          TYPE ZTOPENSIS_006-ZDSC_USER,
         ZCOD_OPUS          TYPE ZTOPENSIS_006-ZCOD_OPUS,
         ZCOD_SIGAM         TYPE ZTOPENSIS_006-ZCOD_SIGAM,
       END OF TY_OUT.

TYPES: BEGIN OF TY_FILTRO,
         ZCOD_GESTOR TYPE ZTOPENSIS_006-ZCOD_GESTOR,
       END OF TY_FILTRO.

*======================================================================*
*         S  T  R  U  C  T  U  R  E  S                                 *
*======================================================================*

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
DATA: T_OUT    TYPE TABLE OF TY_OUT.

*======================================================================*
*        W O R K - A R E A S                                           *
*======================================================================*
DATA: W_OUT    TYPE TY_OUT,
      W_FILTRO TYPE TY_FILTRO.

*======================================================================*
*        R A N G E S                                                   *
*======================================================================*


*======================================================================*
*        C  O  N  S  T  A  N  T  S                                     *
*======================================================================*


*======================================================================*
*        F. S  Y  M  B  O  L  S                                        *
*======================================================================*


*======================================================================*
*        V  A  R  I  A  N  T  S                                        *
*======================================================================*
DATA: GC_ERROR ,
      GV_AUTH  TYPE C,
      GC_LINES TYPE C LENGTH 08,
      VI_LINES TYPE I.

DATA: T_FIELDCAT     TYPE LVC_T_FCAT,
      T_FIELDCAT_BKP TYPE LVC_T_FCAT.

"Variáveis para o ALV
DATA: W_FIELDCAT          TYPE        LVC_S_FCAT,
      W_VARIANT           TYPE        DISVARIANT,
      G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_GRID1             TYPE REF TO CL_GUI_ALV_GRID,
      G_CONTAINER1        TYPE        SCRFNAME VALUE 'CONTAINER1',
      G_LAYOUT1           TYPE        LVC_S_LAYO,
      GS_F4               TYPE        LVC_S_F4                    , "F4
      GT_F4               TYPE        LVC_T_F4                    . "F4

*======================================================================*
*        C  L  A  S  S                                                 *
*======================================================================*
*======================================================================*
*  CLASS lcl_event_handler IMPLEMENTATION                              *
*======================================================================*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      "Overrinding Standard Functions.
      HANDLE_BEFORE_USER_COMMAND   FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      "Overrinding Standard Functions.
      HANDLE_AFTER_USER_COMMAND    FOR EVENT AFTER_USER_COMMAND  OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_BEFORE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN '&XXL' OR '&PC'.

        T_FIELDCAT_BKP[] = T_FIELDCAT[].

        LOOP AT T_FIELDCAT INTO W_FIELDCAT.
          W_FIELDCAT-CHECKBOX = ''.
          MODIFY T_FIELDCAT FROM W_FIELDCAT INDEX SY-TABIX.
        ENDLOOP.

        CALL METHOD G_GRID1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = T_FIELDCAT.

        CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_before_user_command

  METHOD HANDLE_AFTER_USER_COMMAND.

    CASE E_UCOMM.
      WHEN '&XXL' OR '&PC'.
        T_FIELDCAT[] = T_FIELDCAT_BKP[].

        CALL METHOD G_GRID1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = T_FIELDCAT[].

        CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_after_user_command

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: EVENT_RECEIVER       TYPE REF TO LCL_EVENT_RECEIVER.

*======================================================================*
*        A L V                                                         *
*======================================================================*
DATA: O_COLU        TYPE REF TO CL_SALV_COLUMN,
      O_ALV         TYPE REF TO CL_SALV_TABLE,
      O_COLUMNS     TYPE REF TO CL_SALV_COLUMNS_TABLE,
      O_COLUMN      TYPE REF TO CL_SALV_COLUMN_TABLE,
      GR_SORTS      TYPE REF TO CL_SALV_SORTS,
      GR_DISPLAY    TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      O_GRID        TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
      O_GRID_END    TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
      O_CONTENT     TYPE REF TO CL_SALV_FORM_ELEMENT,
      O_CONTENT_END TYPE REF TO CL_SALV_FORM_ELEMENT.

DATA: T_SORT  TYPE LVC_T_SORT,
      FS_SORT TYPE LVC_S_SORT.

*======================================================================*
*        S  C  R  E  E  N                                              *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

"Gestor
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (50) TEXT-T01.
SELECT-OPTIONS: S_GESTOR  FOR ZTOPENSIS_006-ZCOD_GESTOR NO INTERVALS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

*======================================================================*
*        I  N  I  T  I  A  L  I  Z  A  T  I  O  N                      *
*======================================================================*

*======================================================================*
*        S  E  L  .      S  C  R  E  E  N                              *
*======================================================================*
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_GESTOR-LOW.
  PERFORM ZF_EXIBIR_FILTRO_S_GESTOR.

*======================================================================*
*        M  A  I  N       P  R  O  C  E  S  S  I  N  G                 *
*======================================================================*
START-OF-SELECTION.

  PERFORM Z_SEL_DADOS.

  CHECK GC_ERROR IS INITIAL.

  PERFORM Z_ORG_DADOS.

  CHECK GC_ERROR IS INITIAL.

  PERFORM Z_EXIBE_RELATORIO.

END-OF-SELECTION.
*======================================================================*
*        F  O  R  M  S                                                 *
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  Z_SEL_DADOS                                              *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM Z_SEL_DADOS .

  CLEAR GC_ERROR.

  SELECT ZCOD_ROLE_COMPOSTO
         ZDSC_CARGO
         ZCOD_USER
         ZDSC_USER
         ZCOD_OPUS
         ZCOD_SIGAM
    FROM ZTOPENSIS_006
    INTO TABLE T_OUT
    WHERE ZCOD_GESTOR        IN S_GESTOR.

  IF SY-SUBRC <> 0.
    MESSAGE 'Não existe dados para essa seleção.' TYPE 'S' DISPLAY LIKE 'E'.
    GC_ERROR = 'X'.
  ENDIF.

ENDFORM.                    " Z_SEL_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_ORG_DADOS                                              *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM Z_ORG_DADOS .

ENDFORM.                    " Z_ORG_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_RELATORIO                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM Z_EXIBE_RELATORIO .

  IF T_OUT[] IS NOT INITIAL.
    SORT T_OUT BY ZCOD_ROLE_COMPOSTO DESCENDING ZDSC_CARGO DESCENDING.
    DELETE ADJACENT DUPLICATES FROM T_OUT COMPARING ALL FIELDS.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.                    " Z_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_CUSTOM_ALV                                            *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM ZF_CUSTOM_ALV .

  PERFORM ZF_APPEND_FIELDCAT USING:
 "01                  02      03       04   05 06 07 08  09 10 11   12  13  14  15
 'ZCOD_ROLE_COMPOSTO' 'T_OUT' TEXT-C01 '15' '' '' '' 'C' '' '' '1'  ' ' ' ' ' ' '', "Perfil Composto
 'ZDSC_CARGO'         'T_OUT' TEXT-C02 '15' '' '' '' 'C' '' '' '2'  ' ' ' ' ' ' '', "Cargo
 'ZCOD_USER'          'T_OUT' TEXT-C03 '12' '' '' '' 'L' '' '' '3'  ' ' ' ' ' ' '', "Usuário
 'ZDSC_USER'          'T_OUT' TEXT-C04 '20' '' '' '' 'L' '' '' '3'  ' ' ' ' ' ' '', "Nome Completo
 'ZCOD_OPUS'          'T_OUT' TEXT-C05 '10' '' '' '' 'L' '' '' '4'  ' ' ' ' ' ' '', "OPUS
 'ZCOD_SIGAM'         'T_OUT' TEXT-C06 '10' '' '' '' 'L' '' '' '4'  ' ' ' ' ' ' ''. "SIGAM

ENDFORM.                    " ZF_CUSTOM_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

  PERFORM ZF_CUSTOM_ALV.
  PERFORM ZF_SORT_ALV.
  PERFORM ZF_DISPLAY_ALV.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE SY-UCOMM.
    WHEN  'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_DISPLAY_ALV .

  IF G_CUSTOM_CONTAINER1 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER1
      EXPORTING
        CONTAINER_NAME = G_CONTAINER1.

    CREATE OBJECT G_GRID1
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER1.

    G_LAYOUT1-ZEBRA       = 'X'    .
    G_LAYOUT1-SEL_MODE    = 'X'    .
    G_LAYOUT1-NO_F4       = SPACE  .
    G_LAYOUT1-STYLEFNAME  = 'CELLTAB'.
    DESCRIBE TABLE T_OUT LINES VI_LINES.
    GC_LINES = VI_LINES.
    CONDENSE GC_LINES NO-GAPS.
    CONCATENATE 'Total de'
                'Registros Encontrados:'
                GC_LINES
                INTO G_LAYOUT1-GRID_TITLE  SEPARATED BY ' '.

    W_VARIANT-REPORT = SY-REPID.

    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR G_GRID1. "EXCEL
    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND  FOR G_GRID1. "EXCEL

    CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_SAVE          = 'A'
        I_DEFAULT       = 'X'
        IS_LAYOUT       = G_LAYOUT1
        IS_VARIANT      = W_VARIANT
      CHANGING
        IT_FIELDCATALOG = T_FIELDCAT[]
        IT_OUTTAB       = T_OUT[]
        IT_SORT         = T_SORT.

    CALL METHOD G_GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD G_GRID1->SET_TOOLBAR_INTERACTIVE.

  ENDIF.

ENDFORM.                    " ZF_DISPLAY_ALV
*  &---------------------------------------------------------------------*
*  &      Form  ZF_APPEND_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM ZF_APPEND_FIELDCAT USING P_FIELDNAME  "01-Nome do Campo
                              P_TABNAME    "02-Nome da Tabela Interna
                              P_COLTEXT    "03-Texto Coluna
                              P_OUTPUTLEN  "04-Largura da Coluna
                              P_ROLLNAME   "05-Elemento de Dados
                              P_INTTYPE    "06-Tipo de Dados
                              P_F4         "07-Ação do F4
                              P_JUST       "08-Alinhar Coluna
                              P_HOTSPOT    "09-Ação do Click
                              P_NO_OUT     "10-Não Saida no Relatório
                              P_POSITION   "11-Posição da Coluna
                              P_REF_FIELD  "12-Referencia Standard Campo
                              P_REF_TABLE  "13-Referencia Standard Tabela
                              P_CHECKBOX   "14-Checkbox
                              P_CONVEXIT . "15-Exit de conversão

  CLEAR W_FIELDCAT.
  W_FIELDCAT-FIELDNAME        = P_FIELDNAME. "01-Nome do Campo
  W_FIELDCAT-TABNAME          = P_TABNAME  . "02-Nome da Tabela Interna
  W_FIELDCAT-COLTEXT          = P_COLTEXT  . "03-Texto Coluna
  W_FIELDCAT-OUTPUTLEN        = P_OUTPUTLEN. "04-Largura da Coluna
  W_FIELDCAT-ROLLNAME         = P_ROLLNAME . "05-Elemento de Dados
  W_FIELDCAT-INTTYPE          = P_INTTYPE  . "06-Typo de Dados
  W_FIELDCAT-F4AVAILABL       = P_F4       . "07-Ação do F4
  W_FIELDCAT-JUST             = P_JUST     . "08-Alinhar Coluna
  W_FIELDCAT-HOTSPOT          = P_HOTSPOT  . "09-Ação do Click
  W_FIELDCAT-NO_OUT           = P_NO_OUT   . "10-Não Saida no Relatório
  W_FIELDCAT-COL_POS          = P_POSITION . "11-Posição da Coluna
  W_FIELDCAT-REF_FIELD        = P_REF_FIELD. "12-Referencia Standard Campo
  W_FIELDCAT-REF_TABLE        = P_REF_TABLE. "13-Referencia Standard Tabela
  W_FIELDCAT-CHECKBOX         = P_CHECKBOX . "14-Checkbox
  W_FIELDCAT-CONVEXIT         = P_CONVEXIT.  "15-Exit de conversão

  APPEND W_FIELDCAT TO T_FIELDCAT    .

ENDFORM.                    " ZF_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_SORT_ALV .

  FS_SORT-SPOS      = '1'.
  FS_SORT-FIELDNAME = 'ZCOD_ROLE_COMPOSTO'.
  FS_SORT-DOWN      = 'X'.
*  fs_sort-subtot    = 'X'.
  APPEND FS_SORT TO T_SORT.
  CLEAR FS_SORT.

  FS_SORT-SPOS      = '2'.
  FS_SORT-FIELDNAME = 'ZDSC_CARGO'.
  FS_SORT-DOWN      = 'X'.
  APPEND FS_SORT TO T_SORT.
  CLEAR FS_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_FILTRO_S_GESTOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_EXIBIR_FILTRO_S_GESTOR .

  DATA : VL_FIELD TYPE DFIES-FIELDNAME,
         VL_DYNP  TYPE HELP_INFO-DYNPROFLD,
         VL_REPID TYPE SY-REPID.

  DATA: LT_OUT    TYPE TABLE OF ZTOPENSIS_006 WITH HEADER LINE,
        LT_FILTRO TYPE TABLE OF TY_FILTRO     WITH HEADER LINE.

  SELECT *
    FROM ZTOPENSIS_006
    INTO TABLE LT_OUT.

  SORT LT_OUT BY ZCOD_GESTOR ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_OUT COMPARING ZCOD_GESTOR.
  DELETE LT_OUT WHERE ZCOD_GESTOR = SPACE.

  LOOP AT LT_OUT.
    LT_FILTRO-ZCOD_GESTOR = LT_OUT-ZCOD_GESTOR.
    COLLECT LT_FILTRO.
  ENDLOOP.

  "Monta a caixa de seleção
  VL_FIELD = 'ZCOD_GESTOR'.
  VL_DYNP  = 'S_GESTOR-LOW'.
  VL_REPID = SY-REPID.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD    = VL_FIELD
      DYNPPROG    = VL_REPID
      DYNPNR      = SY-DYNNR
      DYNPROFIELD = VL_DYNP
      VALUE_ORG   = 'S'
    TABLES
      VALUE_TAB   = LT_FILTRO[].

ENDFORM.
