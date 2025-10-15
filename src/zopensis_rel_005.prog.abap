************************************************************************
* Program        : ZOPENSIS_REL_005                                    *
* Transaction    : ZOPENSIS_010                                        *
* Title          : Relatório por Usuário                               *
* Developer      : Fernando Oliveira                                   *
* Date           : 11/08/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     11/08/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
REPORT  ZOPENSIS_REL_005.

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
TABLES: ZTOPENSIS_006, ZTOPENSIS_007, SSCRFIELDS.

*======================================================================*
*         T   Y   P   E   S                                            *
*======================================================================*
TYPES: BEGIN OF TY_OUT,
         ZCOD_USER          TYPE ZTOPENSIS_007-ZCOD_USER,
         ZCOD_ROLE_SIMPLES  TYPE ZTOPENSIS_007-ZCOD_ROLE_SIMPLES,
         ZCOD_TCODE         TYPE ZTOPENSIS_007-ZCOD_TCODE,
         ZDSC_TTEXT         TYPE ZTOPENSIS_007-ZDSC_TTEXT,
         ZIND_RISCO         TYPE ZTOPENSIS_007-ZIND_RISCO,
         ZIND_MANTER        TYPE ZTOPENSIS_007-ZIND_MANTER,
         ZIND_REMOVER       TYPE ZTOPENSIS_007-ZIND_REMOVER,
         DATA_AVALIACAO     TYPE ZTOPENSIS_007-DATA_AVALIACAO,
         HORA_AVALIACAO     TYPE ZTOPENSIS_007-HORA_AVALIACAO,
         AVALIADOR          TYPE ZTOPENSIS_007-AVALIADOR,
         MANTER_ORIG        TYPE C,
         REMOVER_ORIG       TYPE C,
         ZCOD_ROLE_COMPOSTO TYPE ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO,
         ZCOD_ROLE_RESTRITO TYPE ZTOPENSIS_006-ZCOD_ROLE_RESTRITO,
         NOME               TYPE ZTOPENSIS_006-ZDSC_USER,
         DPTO               TYPE ZTOPENSIS_006-ZDSC_DEPTO,
         CARGO              TYPE ZTOPENSIS_006-ZDSC_CARGO,
       END OF TY_OUT.

TYPES: BEGIN OF TY_FILTRO,
         ZCOD_USER TYPE ZTOPENSIS_007-ZCOD_USER,
       END OF TY_FILTRO.

TYPES: BEGIN OF TY_ZTOPENSIS_006,
         ZCOD_USER          TYPE ZTOPENSIS_006-ZCOD_USER,
         ZCOD_ROLE_COMPOSTO TYPE ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO,
         ZCOD_ROLE_RESTRITO TYPE ZTOPENSIS_006-ZCOD_ROLE_RESTRITO,
         ZDSC_USER          TYPE ZTOPENSIS_006-ZDSC_USER,
         ZDSC_DEPTO         TYPE ZTOPENSIS_006-ZDSC_DEPTO,
         ZDSC_CARGO         TYPE ZTOPENSIS_006-ZDSC_CARGO,
       END OF TY_ZTOPENSIS_006.

*======================================================================*
*         S  T  R  U  C  T  U  R  E  S                                 *
*======================================================================*

*======================================================================*
*        T  A  B  L  E  S                                              *
*======================================================================*
DATA: T_OUT           TYPE TABLE OF TY_OUT,
      T_007_ORIGEM    TYPE TABLE OF ZTOPENSIS_007,
      T_ZTOPENSIS_007 TYPE TABLE OF ZTOPENSIS_007,
      T_ZTOPENSIS_006 TYPE TABLE OF TY_ZTOPENSIS_006.

*======================================================================*
*        W O R K - A R E A S                                           *
*======================================================================*
DATA: W_OUT           TYPE TY_OUT,
      W_FILTRO        TYPE TY_FILTRO,
      W_007_ORIGEM    TYPE ZTOPENSIS_007,
      W_ZTOPENSIS_007 TYPE ZTOPENSIS_007,
      W_ZTOPENSIS_006 TYPE TY_ZTOPENSIS_006.

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
      GV_AUTH     TYPE C,
      GC_LINES    TYPE C LENGTH 08,
      VI_LINES    TYPE I,
      GC_COMPOSTO TYPE ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO,
      GC_RESTRITO TYPE ZTOPENSIS_006-ZCOD_ROLE_RESTRITO,
      GC_NOME     TYPE ZTOPENSIS_006-ZDSC_USER,
      GC_DPTO     TYPE ZTOPENSIS_006-ZDSC_DEPTO,
      GC_CARGO    TYPE ZTOPENSIS_006-ZDSC_CARGO,
      GC_QTD_REST TYPE I,
      GC_QTD_USER TYPE I.

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

"The Below Definitions Must.....
DATA: DG_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,            "Reference to document
      DG_SPLITTER    TYPE REF TO CL_GUI_SPLITTER_CONTAINER, "Reference to split container
      DG_PARENT_GRID TYPE REF TO CL_GUI_CONTAINER,          "Reference to grid container
      DG_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER,        "Reference to html container
      DG_PARENT_HTML TYPE REF TO CL_GUI_CONTAINER.          "Reference to html container
"up to here

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
        IMPORTING E_UCOMM,

      TOP_OF_PAGE                  FOR EVENT TOP_OF_PAGE         OF CL_GUI_ALV_GRID
        IMPORTING E_DYNDOC_ID,

      "Barra de Ferramentas
      HANDLE_TOOLBAR1      FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT.

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

  METHOD TOP_OF_PAGE.                   "implementation

    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.

  ENDMETHOD.                            "top_of_page

  "Chamada da barra de ferramentas (Criação)
  METHOD HANDLE_TOOLBAR1.

    PERFORM ZF_ADICIONAR_EXCLUIR_BOTOES CHANGING E_OBJECT.

  ENDMETHOD.                    "handle_toolbar

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

"Data
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (50) TEXT-T01.
SELECT-OPTIONS: S_USER  FOR ZTOPENSIS_007-ZCOD_USER NO INTERVALS  .
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_USER-LOW.
  PERFORM ZF_EXIBIR_FILTRO_S_USER.

*======================================================================*
*        M  A  I  N       P  R  O  C  E  S  S  I  N  G                 *
*======================================================================*
START-OF-SELECTION.

  IF S_USER[] IS INITIAL.
    MESSAGE 'Incluir ao menos um usuário na seleção' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

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

  DATA: LT_ZTOPENSIS_006 TYPE TABLE OF TY_ZTOPENSIS_006,
        LT_ZTOPENSIS_007 TYPE TABLE OF ZTOPENSIS_007.

  CLEAR GC_ERROR.

  SELECT ZCOD_USER
         ZCOD_ROLE_SIMPLES
         ZCOD_TCODE
         ZDSC_TTEXT
         ZIND_RISCO
         ZIND_MANTER
         ZIND_REMOVER
         DATA_AVALIACAO
         HORA_AVALIACAO
         AVALIADOR
    FROM ZTOPENSIS_007
    INTO TABLE T_OUT
    WHERE ZCOD_USER IN S_USER.

  IF SY-SUBRC <> 0.
    MESSAGE 'Não existe dados para essa seleção.' TYPE 'S' DISPLAY LIKE 'E'.
    GC_ERROR = 'X'.
    EXIT.
  ENDIF.

  SELECT *
    FROM ZTOPENSIS_007
    INTO TABLE T_007_ORIGEM
    WHERE ZCOD_USER IN S_USER.

  IF SY-SUBRC = 0.
    LT_ZTOPENSIS_007[] = T_007_ORIGEM[].
    SORT LT_ZTOPENSIS_007 BY ZCOD_USER.
    DELETE ADJACENT DUPLICATES FROM LT_ZTOPENSIS_007 COMPARING ZCOD_USER.
    DESCRIBE TABLE LT_ZTOPENSIS_007.
    GC_QTD_USER = SY-TFILL.
  ENDIF.

  SELECT ZCOD_USER ZCOD_ROLE_RESTRITO ZCOD_ROLE_COMPOSTO ZDSC_USER ZDSC_DEPTO ZDSC_CARGO
    FROM ZTOPENSIS_006
    INTO TABLE T_ZTOPENSIS_006
    WHERE ZCOD_USER IN S_USER.

ENDFORM.                    " Z_SEL_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_ORG_DADOS                                              *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM Z_ORG_DADOS .

  DATA: LI_INDEX TYPE I.

  LOOP AT T_OUT INTO W_OUT.

    LI_INDEX = SY-TABIX.

    CLEAR W_ZTOPENSIS_006.
    READ TABLE T_ZTOPENSIS_006 INTO W_ZTOPENSIS_006 WITH KEY ZCOD_USER = W_OUT-ZCOD_USER.
    W_OUT-MANTER_ORIG        = W_OUT-ZIND_MANTER                 . "Manter Sim
    W_OUT-REMOVER_ORIG       = W_OUT-ZIND_REMOVER                . "Remover Não
    W_OUT-ZCOD_ROLE_RESTRITO = W_ZTOPENSIS_006-ZCOD_ROLE_RESTRITO. "Função Restrita
    W_OUT-ZCOD_ROLE_COMPOSTO = W_ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO. "Função Composta
    W_OUT-NOME               = W_ZTOPENSIS_006-ZDSC_USER.
    W_OUT-DPTO               = W_ZTOPENSIS_006-ZDSC_DEPTO.
    W_OUT-CARGO              = W_ZTOPENSIS_006-ZDSC_CARGO.

    IF W_OUT-ZCOD_ROLE_SIMPLES CS '#N/'.
      CLEAR W_OUT-ZCOD_ROLE_SIMPLES.
    ENDIF.

    IF W_OUT-ZCOD_ROLE_RESTRITO CS '#N/'.
      CLEAR W_OUT-ZCOD_ROLE_RESTRITO.
    ENDIF.

    IF W_OUT-ZDSC_TTEXT CS '#N/'.
      CLEAR W_OUT-ZDSC_TTEXT.
    ENDIF.

    IF W_OUT-ZIND_RISCO CS '#N/'.
      CLEAR W_OUT-ZIND_RISCO.
    ENDIF.

    IF W_OUT-ZCOD_ROLE_SIMPLES  IS INITIAL OR
       W_OUT-ZCOD_ROLE_RESTRITO IS INITIAL OR
       W_OUT-ZDSC_TTEXT         IS INITIAL OR
       W_OUT-ZIND_RISCO         IS INITIAL.
      DELETE T_OUT INDEX LI_INDEX.
      CONTINUE.
    ENDIF.

    GC_COMPOSTO = W_ZTOPENSIS_006-ZCOD_ROLE_COMPOSTO.
    GC_RESTRITO = W_ZTOPENSIS_006-ZCOD_ROLE_RESTRITO.
    GC_NOME     = W_ZTOPENSIS_006-ZDSC_USER.
    GC_DPTO     = W_ZTOPENSIS_006-ZDSC_DEPTO.
    GC_CARGO    = W_ZTOPENSIS_006-ZDSC_CARGO.

    MODIFY T_OUT FROM W_OUT.
  ENDLOOP.

ENDFORM.                    " Z_ORG_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_RELATORIO                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM Z_EXIBE_RELATORIO .

  IF T_OUT[] IS NOT INITIAL.
    SORT T_OUT BY ZCOD_ROLE_SIMPLES ASCENDING ZCOD_TCODE ASCENDING.
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

  IF GC_QTD_USER = 1.
    PERFORM ZF_APPEND_FIELDCAT USING:
   "01                  02      03   	    04  05 06 07 08  09 10  11   12  13  14  15
   'ZCOD_USER'          'T_OUT' TEXT-C11 '12' '' '' '' 'L' '' 'X' '01'  ' ' ' ' ' ' '' , "Usuário
   'ZCOD_ROLE_RESTRITO' 'T_OUT' TEXT-C10 '25' '' '' '' 'L' '' 'X' '04'  ' ' ' ' ' ' '' , "Perfil Restrito
   'ZCOD_ROLE_COMPOSTO' 'T_OUT' TEXT-C15 '25' '' '' '' 'L' '' 'X' '05'  ' ' ' ' ' ' '' , "Perfil Composto
   'NOME'               'T_OUT' TEXT-C12 '20' '' '' '' 'L' '' 'X' '02'  ' ' ' ' ' ' '' , "Nome Completo
   'DPTO'               'T_OUT' TEXT-C13 '15' '' '' '' 'L' '' 'X' '13'  ' ' ' ' ' ' '' , "Departamento
   'CARGO'              'T_OUT' TEXT-C14 '15' '' '' '' 'L' '' 'X' '14'  ' ' ' ' ' ' '' . "Cargo
  ELSE.
    PERFORM ZF_APPEND_FIELDCAT USING:
   "01                  02      03       04   05 06 07 08  09 10  11    12  13  14  15
   'ZCOD_USER'          'T_OUT' TEXT-C11 '12' '' '' '' 'L' '' ' ' '01'  ' ' ' ' ' ' '' , "Usuário
   'ZCOD_ROLE_RESTRITO' 'T_OUT' TEXT-C10 '25' '' '' '' 'L' '' ' ' '04'  ' ' ' ' ' ' '' , "Perfil Restrito
   'ZCOD_ROLE_COMPOSTO' 'T_OUT' TEXT-C15 '25' '' '' '' 'L' '' ' ' '05'  ' ' ' ' ' ' '' , "Perfil Composto
   'NOME'               'T_OUT' TEXT-C12 '20' '' '' '' 'L' '' ' ' '02'  ' ' ' ' ' ' '' , "Nome Completo
   'DPTO'               'T_OUT' TEXT-C13 '15' '' '' '' 'L' '' ' ' '13'  ' ' ' ' ' ' '' , "Departamento
   'CARGO'              'T_OUT' TEXT-C14 '15' '' '' '' 'L' '' ' ' '14'  ' ' ' ' ' ' '' . "Cargo
  ENDIF.

  PERFORM ZF_APPEND_FIELDCAT USING:
   "01                02      03   	    04  05 06 07 08  09 10  11    12  13  14  15
 'ZCOD_ROLE_SIMPLES'  'T_OUT' TEXT-C01 '25' '' '' '' 'L' '' ''  '03'  ' ' ' ' ' ' '' , "Perfil Simples
 'ZCOD_TCODE'         'T_OUT' TEXT-C02 '15' '' '' '' 'L' '' ''  '06'  ' ' ' ' ' ' '' , "Transação
 'ZDSC_TTEXT'         'T_OUT' TEXT-C03 '30' '' '' '' 'L' '' ''  '07'  ' ' ' ' ' ' '' , "Descrição
 'ZIND_RISCO'         'T_OUT' TEXT-C04 '10' '' '' '' 'C' '' ''  '08'  ' ' ' ' ' ' '' , "Risco
 'ZIND_MANTER'        'T_OUT' TEXT-C05 '10' '' '' '' 'C' '' ''  '09'  ' ' ' ' 'X' 'X', "Manter
 'ZIND_REMOVER'       'T_OUT' TEXT-C06 '10' '' '' '' 'C' '' ''  '10'  ' ' ' ' 'X' 'X', "Remover
 'AVALIADOR'          'T_OUT' TEXT-C07 '17' '' '' '' 'L' '' ' ' '11'  ' ' ' ' ' ' '' , "Avaliador
 'DATA_AVALIACAO'     'T_OUT' TEXT-C08 '18' '' '' '' 'L' '' ' ' '12'  ' ' ' ' ' ' '' , "Data Execução
 'HORA_AVALIACAO'     'T_OUT' TEXT-C09 '19' '' '' '' 'L' '' ' ' '13'  ' ' ' ' ' ' '' . "Hora Execução

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

  DATA: LC_VALID.

  CALL METHOD G_GRID1->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN  'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM ZF_VERIFICA_SE_HOUVE_ALTERACAO CHANGING LC_VALID.

      IF LC_VALID IS NOT INITIAL.
        CLEAR LC_VALID.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = TEXT-I01 "Log
            TEXT_QUESTION         = TEXT-I02 "Deseja realmente sair sem salvar os dados?
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '2'
            DISPLAY_CANCEL_BUTTON = SPACE
          IMPORTING
            ANSWER                = LC_VALID.
        CHECK LC_VALID = '1'.
      ENDIF.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      PERFORM ZF_VALIDA_DADOS  CHANGING LC_VALID .
      IF LC_VALID IS INITIAL.
        PERFORM ZF_SALVA_ALTERACAO.
      ENDIF.

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

    "Create TOP-Document
    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    "Create Splitter for custom_container
    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER1
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_GRID.

    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 23.

    CREATE OBJECT G_GRID1
      EXPORTING
        I_PARENT = DG_PARENT_GRID.

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
    SET HANDLER EVENT_RECEIVER->TOP_OF_PAGE                FOR G_GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR1            FOR G_GRID1.

    CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_SAVE          = 'A'
*       I_DEFAULT       = 'X'
        IS_LAYOUT       = G_LAYOUT1
        IS_VARIANT      = W_VARIANT
      CHANGING
        IT_FIELDCATALOG = T_FIELDCAT[]
        IT_OUTTAB       = T_OUT[]
        IT_SORT         = T_SORT.

*    CALL METHOD G_GRID1->REGISTER_EDIT_EVENT
*      EXPORTING
*        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*    CALL METHOD G_GRID1->SET_TOOLBAR_INTERACTIVE.

    "Initializing document
    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

    "Processing events
    CALL METHOD G_GRID1->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = DG_DYNDOC_ID.

    "Set editable cells to ready for input initially
    CALL METHOD G_GRID1->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.
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
                              P_EDIT   .   "15-Edit

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
  W_FIELDCAT-EDIT             = P_EDIT.      "15-Edit

  APPEND W_FIELDCAT TO T_FIELDCAT    .

ENDFORM.                    " ZF_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_SORT_ALV .

  FS_SORT-SPOS      = '1'.
  FS_SORT-FIELDNAME = 'ZCOD_ROLE_SIMPLES'.
  FS_SORT-UP        = 'X'.
  APPEND FS_SORT TO T_SORT.
  CLEAR FS_SORT.

  FS_SORT-SPOS      = '2'.
  FS_SORT-FIELDNAME = 'ZCOD_TCODE'.
  FS_SORT-UP        = 'X'.
  APPEND FS_SORT TO T_SORT.
  CLEAR FS_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_FILTRO_S_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_EXIBIR_FILTRO_S_USER .

  DATA : VL_FIELD TYPE DFIES-FIELDNAME,
         VL_DYNP  TYPE HELP_INFO-DYNPROFLD,
         VL_REPID TYPE SY-REPID.

  DATA: LT_OUT    TYPE TABLE OF ZTOPENSIS_007 WITH HEADER LINE,
        LT_FILTRO TYPE TABLE OF TY_FILTRO     WITH HEADER LINE.

  SELECT *
    FROM ZTOPENSIS_007
    INTO TABLE LT_OUT.

  SORT LT_OUT BY ZCOD_USER ASCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_OUT COMPARING ZCOD_USER.
  DELETE LT_OUT WHERE ZCOD_USER = SPACE.

  LOOP AT LT_OUT.
    LT_FILTRO-ZCOD_USER = LT_OUT-ZCOD_USER.
    COLLECT LT_FILTRO.
  ENDLOOP.

  "Monta a caixa de seleção
  VL_FIELD = 'ZCOD_USER'.
  VL_DYNP  = 'S_USER-LOW'.
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
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE USING   DG_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

  DATA : DL_TEXT(255) TYPE C.  "Text

  IF GC_QTD_USER > 1.
    CLEAR : DL_TEXT.
    CONCATENATE 'Usuário: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Perfil Restrito: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Perfil Composto: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Nome Completo: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Departamento: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Cargo: ' '...' INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.
  ELSE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Usuário: ' S_USER-LOW INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Perfil Restrito: ' GC_RESTRITO INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Perfil Composto: ' GC_COMPOSTO INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Nome Completo: ' GC_NOME INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Departamento: ' GC_DPTO INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
    CALL METHOD DG_DYNDOC_ID->NEW_LINE.

    CLEAR : DL_TEXT.
    CONCATENATE 'Cargo: ' GC_CARGO INTO DL_TEXT SEPARATED BY SPACE.
    PERFORM ADD_TEXT USING DL_TEXT.
  ENDIF.

  CALL METHOD DG_DYNDOC_ID->NEW_LINE.

  PERFORM HTML.

ENDFORM.                    " EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM ADD_TEXT USING P_TEXT TYPE SDYDO_TEXT_ELEMENT.
* Adding text
  CALL METHOD DG_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = P_TEXT
      SAP_EMPHASIS = CL_DD_AREA=>HEADING.
ENDFORM.                    " ADD_TEXT
*&---------------------------------------------------------------------*
*&      Form  HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HTML.

  DATA : DL_LENGTH        TYPE I,                           " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id
  DATA: LC_TVARV_LOGO TYPE TVARV-LOW,
        LC_LOGO       TYPE SDYDO_KEY.

  SELECT SINGLE LOW
    FROM TVARVC
    INTO LC_TVARV_LOGO
    WHERE NAME = 'ZOPENSIS_LOGO'.

  "Creating html control
  IF DG_HTML_CNTRL IS INITIAL.
    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML.
  ENDIF.

  "Reuse_alv_grid_commentary_set
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    IMPORTING
      LENGTH   = DL_LENGTH.

  "Get TOP->HTML_TABLE ready
  CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

  "Set wallpaper
  CALL METHOD DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  "Connect TOP document to HTML-Control
  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

  CALL METHOD DG_DYNDOC_ID->ADD_GAP
    EXPORTING
      WIDTH = 120.

  LC_LOGO = LC_TVARV_LOGO.

  CALL METHOD DG_DYNDOC_ID->ADD_PICTURE
    EXPORTING
      PICTURE_ID = LC_LOGO.
*      PICTURE_ID = 'LOGO_NOVO'.

  "Display TOP document
  CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.
  IF SY-SUBRC NE 0.
*    MESSAGE S999 WITH 'Error in displaying top-of-page'(036).
    MESSAGE 'Error in displaying top-of-page' TYPE 'E'.
  ENDIF.

ENDFORM.                    " HTML
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_SE_HOUVE_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_VERIFICA_SE_HOUVE_ALTERACAO  CHANGING PC_VALID.

  LOOP AT T_OUT INTO W_OUT.

    IF ( W_OUT-ZIND_MANTER    IS INITIAL AND
         W_OUT-MANTER_ORIG    IS NOT INITIAL ) OR
       ( W_OUT-ZIND_REMOVER   IS INITIAL AND
         W_OUT-REMOVER_ORIG   IS NOT INITIAL ) .
      PC_VALID = 'X'.
      EXIT.
    ELSEIF ( W_OUT-ZIND_MANTER  IS NOT INITIAL AND
             W_OUT-MANTER_ORIG  IS INITIAL ) OR
           ( W_OUT-ZIND_REMOVER IS NOT INITIAL AND
             W_OUT-REMOVER_ORIG IS INITIAL ) .
      PC_VALID = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_VALIDA_DADOS CHANGING PC_VALID.

  DATA: LC_MSG TYPE C LENGTH 60.

  LOOP AT T_OUT INTO W_OUT.

    CLEAR W_007_ORIGEM.
    READ TABLE T_007_ORIGEM INTO W_007_ORIGEM WITH KEY ZCOD_USER  = W_OUT-ZCOD_USER
                                                       ZCOD_TCODE = W_OUT-ZCOD_TCODE.

    IF W_OUT-ZIND_MANTER  IS NOT INITIAL AND
       W_OUT-ZIND_REMOVER IS NOT INITIAL.
      "Não é possivel marcar Justificavel e Não Justificavel.
      LC_MSG   = TEXT-E01.
      PC_VALID = 'E'.
      EXIT.
    ELSEIF ( ( W_OUT-ZIND_MANTER   IS INITIAL AND
               W_OUT-ZIND_REMOVER  IS NOT INITIAL ) OR
             ( W_OUT-ZIND_MANTER   IS NOT INITIAL AND
               W_OUT-ZIND_REMOVER  IS INITIAL ) OR
             ( W_OUT-ZIND_MANTER   IS INITIAL AND
               W_OUT-MANTER_ORIG   IS NOT INITIAL ) OR
             ( W_OUT-ZIND_REMOVER  IS INITIAL AND
               W_OUT-REMOVER_ORIG  IS NOT INITIAL ) ) AND
           ( W_OUT-ZIND_MANTER <> W_OUT-MANTER_ORIG OR
             W_OUT-ZIND_REMOVER <> W_OUT-REMOVER_ORIG ).
      PC_VALID = 'S'.

      MOVE-CORRESPONDING W_007_ORIGEM TO W_ZTOPENSIS_007.
      W_ZTOPENSIS_007-ZIND_MANTER = W_OUT-ZIND_MANTER.
      W_ZTOPENSIS_007-ZIND_REMOVER = W_OUT-ZIND_REMOVER.
      W_ZTOPENSIS_007-AVALIADOR      = SY-UNAME.
      W_ZTOPENSIS_007-DATA_AVALIACAO = SY-DATUM.
      W_ZTOPENSIS_007-HORA_AVALIACAO = SY-UZEIT.

*      IF w_ztopensis_007-zind_manter IS NOT INITIAL.
*        w_ztopensis_007-status = icon_green_light.
*      ELSEIF gs_ztopensis_001-zind_remover IS NOT INITIAL.
*        gs_ztopensis_001-status = icon_red_light.
*      ELSEIF gs_ztopensis_001-zind_manter IS INITIAL AND
*             gs_ztopensis_001-zind_remover IS INITIAL.
*        gs_ztopensis_001-status = icon_yellow_light.
*      ENDIF.
      APPEND W_ZTOPENSIS_007 TO T_ZTOPENSIS_007.
    ENDIF.

  ENDLOOP.

  IF PC_VALID IS INITIAL.
    "Não existe ajuste para ser salvo.
    LC_MSG   = TEXT-E02.
    PC_VALID = 'E'.
  ELSEIF PC_VALID = 'S'.
    CLEAR PC_VALID.
    EXIT.
  ENDIF.

  MESSAGE LC_MSG TYPE 'E' DISPLAY LIKE 'I'.
  FREE: T_ZTOPENSIS_007[].
  CLEAR W_ZTOPENSIS_007.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVA_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_SALVA_ALTERACAO .

  MODIFY ZTOPENSIS_007 FROM TABLE T_ZTOPENSIS_007.
  COMMIT WORK AND WAIT.
  MESSAGE 'Registros salvo com sucesso' TYPE 'S'.
  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_ADICIONAR_EXCLUIR_BOTOES CHANGING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

  DATA: LS_TOOLBAR  TYPE STB_BUTTON.

  DEFINE DEL_BUTTON.
    clear ls_toolbar.
    ls_toolbar-function  = &1.
    ls_toolbar-icon      = &2.
    ls_toolbar-text      = &3.
    ls_toolbar-quickinfo = &4.
    ls_toolbar-disabled  = space.
    delete e_object->mt_toolbar where function = ls_toolbar-function.
  END-OF-DEFINITION.

  DEL_BUTTON '&CHECK'             SPACE SPACE SPACE.
  DEL_BUTTON '&REFRESH'           SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&CUT'         SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&COPY'        SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&PASTE'       SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&UNDO'        SPACE SPACE SPACE.
  DEL_BUTTON '&&SEP00'            SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&APPEND'      SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&INSERT_ROW'  SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&DELETE_ROW'  SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&COPY_ROW'    SPACE SPACE SPACE.
  DEL_BUTTON '&&SEP02'            SPACE SPACE SPACE.
  DEL_BUTTON '&PRINT_BACK'        SPACE SPACE SPACE.
  DEL_BUTTON '&DETAIL'            SPACE SPACE SPACE.
  DEL_BUTTON '&PC'                SPACE SPACE SPACE.

ENDFORM.
