*----------------------------------------------------------------------*
* ID........:                                                          *
* Programa..: ZLESR0150                                                *
* Tipo......: R - Report                                               *
* Transação.:                                                          *
* Descrição.: Cadastro Parâmetros – Identificar Frete Insumo           *
* Autor.....: Alexandre                                                *
* Data......: 10.11.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 11.11.20   |            |ALEXANDRE     | Codificação Inicial         *
*----------------------------------------------------------------------*
REPORT ZLESR0150.

TABLES: ZLEST0208.

*----------------------------------------------------------------------*
* Declaração de Tipos
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_SAIDA,
    STATUS(20)  TYPE C,
    SAKNR(10)   TYPE C,"ZLEST0208-SAKNR,
    DESCR_SAKNR TYPE SKAT-TXT20,
    KOSTL(10)   TYPE C,"ZLEST0208-KOSTL,
    DESCR_KOSTL TYPE CSKT-LTEXT,
    MATNR       TYPE ZLEST0208-MATNR,
    DESCR_MATNR TYPE MAKT-MAKTX,
    MATKL       TYPE ZLEST0208-MATKL,
    DESCR_MATKL TYPE T023T-WGBEZ60,
    USNAM       TYPE ZLEST0208-USNAM,
    ZDT_ATUAL   TYPE ZLEST0208-ZDT_ATUAL,
    ZHR_ATUAL   TYPE ZLEST0208-ZHR_ATUAL,
    ACAO        TYPE C,
    CELLTAB     TYPE LVC_T_STYL,
  END OF   TY_SAIDA.


*&---------------------------------------------------------------------*
*& Variáveis
*&---------------------------------------------------------------------*
DATA: V_INFO TYPE C.

*&---------------------------------------------------------------------*
*&     ALV
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
* Declaração de Instância de Métodos
*&---------------------------------------------------------------------*
DATA: V_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

*&---------------------------------------------------------------------*
*&  Declaração de Container
*&---------------------------------------------------------------------*
DATA: V_CONTAINER_H TYPE REF TO CL_GUI_CONTAINER,
      V_CONTAINER_I TYPE REF TO CL_GUI_CONTAINER.

*&---------------------------------------------------------------------*
*&  Declaração de GRID
*&---------------------------------------------------------------------*
DATA: V_GRID     TYPE REF TO CL_GUI_ALV_GRID,
      V_DOCKING  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      V_SPLITTER TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      V_DOCUMENT TYPE REF TO CL_DD_DOCUMENT.

*&---------------------------------------------------------------------*
* Declaração de Tabelas
*&---------------------------------------------------------------------*
DATA: T_SAIDA     TYPE TABLE OF TY_SAIDA,
      T_LOG       TYPE TABLE OF BAPIRET2,
      T_ZLEST0208 TYPE TABLE OF ZLEST0208,
      T_CELLTAB   TYPE LVC_T_STYL,
      T_CELLTAB2  TYPE LVC_T_STYL,
      T_FCAT      TYPE          LVC_T_FCAT.

DATA: T_SKAT      TYPE TABLE OF SKAT.
DATA: T_CSKT      TYPE TABLE OF CSKT.
DATA: T_MAKT      TYPE TABLE OF MAKT.
DATA: T_T023T     TYPE TABLE OF T023T.

*&---------------------------------------------------------------------*
* Declaração de Constantes
*&---------------------------------------------------------------------*
CONSTANTS: BEGIN OF GC_ACAO,
             DELETE_ROW TYPE C VALUE 'D',
             INSERT_ROW TYPE C VALUE 'I',
             UPDATE_ROW TYPE C VALUE 'U',
           END OF GC_ACAO.
*&---------------------------------------------------------------------*
* Declaração de Estrutura
*&---------------------------------------------------------------------*
DATA: W_DISVARIANT TYPE DISVARIANT,
      W_LAYOUT     TYPE LVC_S_LAYO.

DATA: W_SKAT      TYPE SKAT.
DATA: W_CSKT      TYPE CSKT.
DATA: W_MAKT      TYPE MAKT.
DATA: W_T023T     TYPE T023T.

*&---------------------------------------------------------------------*
* Declaração de Field-Symbol
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <FS_SAIDA> LIKE LINE OF T_SAIDA.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       Definição da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                  E_ONF4
                  E_ONF4_BEFORE
                  E_ONF4_AFTER
                  E_UCOMM,

      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT
                  E_INTERACTIVE,

      HANDLE_TOOLBAR_DETALHES FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*       Implementação da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_USER_COMMAND.

    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = E_UCOMM.

    PERFORM ZF_HANDLE_USER_COMMAND USING E_UCOMM.

  ENDMETHOD.                    "handle_user_command

  METHOD HANDLE_DATA_CHANGED.

    DATA: T_RETURN TYPE TABLE OF BAPIRET2.

    DATA: W_ROW  TYPE LVC_S_ROW,
          W_GOOD TYPE LVC_S_MODI.

    DATA: L_INDEX(3)  TYPE C,
          L_LINHA(30) TYPE C,
          L_ROW_ID    TYPE INT4.

    FIELD-SYMBOLS: <FS_VALUE> TYPE ANY.

    IF T_SAIDA[] IS INITIAL.
      PERFORM ZF_INSERT_ROW.
    ENDIF.

    DATA(T_SAIDA_AUX) = T_SAIDA.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO W_GOOD.

      READ TABLE T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>) INDEX W_GOOD-ROW_ID.

      CHECK <FS_SAIDA> IS ASSIGNED.

      ASSIGN COMPONENT W_GOOD-FIELDNAME OF STRUCTURE <FS_SAIDA> TO <FS_VALUE>.
      CHECK <FS_VALUE> IS ASSIGNED.

      TRANSLATE W_GOOD-VALUE TO UPPER CASE.
      <FS_VALUE>      = W_GOOD-VALUE.
      <FS_SAIDA>-ACAO = GC_ACAO-UPDATE_ROW.

      IF <FS_SAIDA>-SAKNR IS NOT INITIAL.
        READ TABLE T_SKAT INTO W_SKAT WITH KEY SAKNR = <FS_SAIDA>-SAKNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FS_SAIDA>-DESCR_SAKNR = W_SKAT-TXT20.
        ELSE.
          <FS_SAIDA>-DESCR_SAKNR = ''.
        ENDIF.
      ELSE.
        <FS_SAIDA>-DESCR_SAKNR = ''.
      ENDIF.

      IF <FS_SAIDA>-KOSTL IS NOT INITIAL.
        READ TABLE T_CSKT INTO W_CSKT WITH KEY KOSTL = <FS_SAIDA>-KOSTL BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FS_SAIDA>-DESCR_KOSTL = W_CSKT-LTEXT.
        ELSE.
          <FS_SAIDA>-DESCR_KOSTL = ''.
        ENDIF.
      ELSE.
        <FS_SAIDA>-DESCR_KOSTL = ''.
      ENDIF.

      IF <FS_SAIDA>-MATNR IS NOT INITIAL.
        READ TABLE T_MAKT INTO W_MAKT WITH KEY MATNR = <FS_SAIDA>-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FS_SAIDA>-DESCR_MATNR = W_MAKT-MAKTX.
        ELSE.
          <FS_SAIDA>-DESCR_MATNR = ''.
        ENDIF.
      ELSE.
        <FS_SAIDA>-DESCR_MATNR = ''.
      ENDIF.
      IF <FS_SAIDA>-MATKL IS NOT INITIAL.
        READ TABLE T_T023T INTO W_T023T WITH KEY MATKL = <FS_SAIDA>-MATKL BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FS_SAIDA>-DESCR_MATKL  = W_T023T-WGBEZ60.
        ELSE.
          <FS_SAIDA>-DESCR_MATKL  = ''.
        ENDIF.
      ELSE.
        <FS_SAIDA>-DESCR_MATKL  = ''.
      ENDIF.
    ENDLOOP.
    CALL METHOD V_GRID->REFRESH_TABLE_DISPLAY.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_TOOLBAR.
    PERFORM ZF_ADICIONA_BOTOES_HEADER USING E_OBJECT.
    PERFORM ZF_ELIMINA_BOTOES_HEADER  USING E_OBJECT.
  ENDMETHOD.                    "handle_toolbar                                              "on_f4

  METHOD HANDLE_TOOLBAR_DETALHES.
    REFRESH: E_OBJECT->MT_TOOLBAR.
  ENDMETHOD.

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
* Definição de Icones
*----------------------------------------------------------------------*

DATA: BEGIN OF W_ICONES,
        ICON_CREATE(50) TYPE C,
        ICON_CHANGE(50) TYPE C,
        ICON_DELETE(50) TYPE C,
      END OF W_ICONES.

*----------------------------------------------------------------------*
* Declaração de Variáveis
*----------------------------------------------------------------------*
DATA: V_FUNCT      TYPE C,
      V_OK9000     TYPE SY-UCOMM, "Captura evento
      V_TELA_UCOMM TYPE SY-UCOMM,
      V_DATUM      TYPE SY-DATUM,
      V_TABIX      TYPE C.

START-OF-SELECTION.

  PERFORM ZF_PROCESSAMENTO.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_TAB_REGISTROS .

  DATA: L_UKURS TYPE C LENGTH 20.

  LOOP AT T_SAIDA ASSIGNING <FS_SAIDA>.

    T_CELLTAB[] = T_CELLTAB2[].

    REFRESH: <FS_SAIDA>-CELLTAB[].
    IF V_OK9000 <> 'COPIAR' AND <FS_SAIDA>-ACAO <> 'T'.

      PERFORM ZF_ATUALIZA_CELL USING <FS_SAIDA>-SAKNR
                                     <FS_SAIDA>-KOSTL
                                     <FS_SAIDA>-MATNR.

      REFRESH: <FS_SAIDA>-CELLTAB[].
      INSERT LINES OF T_CELLTAB[] INTO TABLE <FS_SAIDA>-CELLTAB[].

    ENDIF.

    IF V_OK9000 = 'COPIAR' AND <FS_SAIDA>-ACAO = 'C'.


      PERFORM ZF_ATUALIZA_CELL USING <FS_SAIDA>-SAKNR
                                     <FS_SAIDA>-KOSTL
                                     <FS_SAIDA>-MATNR.

      REFRESH: <FS_SAIDA>-CELLTAB[].
      INSERT LINES OF T_CELLTAB[] INTO TABLE <FS_SAIDA>-CELLTAB[].

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM ZF_ELIMINA_BOTOES_HEADER  USING  E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

*    elimina itens desnecessarios da barra do container
  DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&APPEND'
                                 OR FUNCTION = '&LOCAL&INSERT_ROW'
                                 OR FUNCTION = '&LOCAL&DELETE_ROW'
                                 OR FUNCTION = '&LOCAL&COPY_ROW'
                                 OR FUNCTION = '&LOCAL&CUT'
                                 OR FUNCTION = '&LOCAL&COPY'
                                 OR FUNCTION = '&LOCAL&PASTE'
                                 OR FUNCTION = '&REFRESH'
                                 OR FUNCTION = '&CHECK'
                                 OR FUNCTION = '&GRAPH'
                                 OR FUNCTION = '&INFO'
                                 OR FUNCTION = '&LOCAL&UNDO'
                                 OR FUNCTION = '&MB_VIEW'
                                 OR FUNCTION = '&MB_VARIANT'
                                 OR FUNCTION =  '&MB_EXPORT'
                                 OR FUNCTION =  '&MB_SUM'
                                 OR FUNCTION =  '&MB_SUBTOT'
                                 OR FUNCTION =  '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM ZF_ADICIONA_BOTOES_HEADER  USING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

* Add Button
  DATA: W_TOOLBAR  TYPE STB_BUTTON.

* inclui novo item na barra do container
  CLEAR W_TOOLBAR.
  MOVE 3 TO W_TOOLBAR-BUTN_TYPE.
  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*----------------------------------------------------------------------*
* novo item na barra do container
  CLEAR W_TOOLBAR.
  MOVE 'INCLUIR'                     TO W_TOOLBAR-FUNCTION.
  MOVE ICON_INSERT_ROW   TO W_TOOLBAR-ICON.
  MOVE '0 '                          TO W_TOOLBAR-BUTN_TYPE.
  MOVE 'Novo'(007)                TO W_TOOLBAR-QUICKINFO.

  IF V_FUNCT IS INITIAL.
    MOVE ' ' TO W_TOOLBAR-DISABLED.
  ELSE.
    MOVE 'X' TO W_TOOLBAR-DISABLED.
  ENDIF.
  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*----------------------------------------------------------------------*
  CLEAR W_TOOLBAR.
  MOVE 'EXCLUIR'                     TO W_TOOLBAR-FUNCTION.
  MOVE ICON_DELETE_ROW               TO W_TOOLBAR-ICON.
  MOVE '0 '                          TO W_TOOLBAR-BUTN_TYPE.
  MOVE 'Excluir'(009)                TO W_TOOLBAR-QUICKINFO.

  IF V_FUNCT IS INITIAL.
    MOVE ' ' TO W_TOOLBAR-DISABLED.
  ELSE.
    MOVE 'X' TO W_TOOLBAR-DISABLED.
  ENDIF.
  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*----------------------------------------------------------------------*
  CLEAR W_TOOLBAR.
  MOVE 'ALTERAR'                   TO W_TOOLBAR-FUNCTION.
  MOVE ICON_CHANGE                 TO W_TOOLBAR-ICON.
  MOVE '0 '                        TO W_TOOLBAR-BUTN_TYPE.
  MOVE 'Modificar'(008)            TO W_TOOLBAR-QUICKINFO.

  IF V_FUNCT IS INITIAL.
    MOVE ' ' TO W_TOOLBAR-DISABLED.
  ELSE.
    MOVE 'X' TO W_TOOLBAR-DISABLED.
  ENDIF.

  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*----------------------------------------------------------------------*
  CLEAR W_TOOLBAR.
  MOVE 'COPIAR'                 TO W_TOOLBAR-FUNCTION.
  MOVE ICON_SYSTEM_COPY         TO W_TOOLBAR-ICON.
  MOVE '0 '                     TO W_TOOLBAR-BUTN_TYPE.
  MOVE 'Copiar'(008)            TO W_TOOLBAR-QUICKINFO.

  IF V_FUNCT IS INITIAL.
    MOVE ' ' TO W_TOOLBAR-DISABLED.
  ELSE.
    MOVE 'X' TO W_TOOLBAR-DISABLED.
  ENDIF.

  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*----------------------------------------------------------------------*
  CLEAR W_TOOLBAR.
  MOVE 'INFO'                 TO W_TOOLBAR-FUNCTION.
  MOVE ICON_INFORMATION       TO W_TOOLBAR-ICON.
  MOVE '0 '                   TO W_TOOLBAR-BUTN_TYPE.
  MOVE 'Legendas'(008)        TO W_TOOLBAR-QUICKINFO.

  IF V_FUNCT IS INITIAL.
    MOVE ' ' TO W_TOOLBAR-DISABLED.
  ELSE.
    MOVE 'X' TO W_TOOLBAR-DISABLED.
  ENDIF.

  APPEND W_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_EXIT INPUT.

  CASE V_OK9000.
    WHEN 'BACK' OR 'CANC'.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_PREPARAR_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_PREPARAR_ALV OUTPUT.
  PERFORM ZF_PREPARAR_ALV.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_PREPARAR_ALV .

  DATA: T_SORT TYPE LVC_T_SORT.

  DATA: W_VARIANT TYPE DISVARIANT,
        W_LAYOUT  TYPE LVC_S_LAYO.

  W_LAYOUT-CWIDTH_OPT = 'X'.
  W_LAYOUT-STYLEFNAME = 'CELLTAB'.
  W_LAYOUT-SEL_MODE   = 'D'.

  W_VARIANT-REPORT    = SY-REPID.

  IF V_GRID IS INITIAL.
    PERFORM ZF_SPLIT_SCREEN.
    PERFORM ZF_MONTAR_FIELDCAT     CHANGING T_SAIDA T_FCAT.
    PERFORM ZF_AJUSTE_DESCR_CAMPOS CHANGING T_FCAT.
  ENDIF.

  PERFORM ZF_TAB_REGISTROS.

*  SORT T_SAIDA BY COMP_ATVBIOL.

  IF V_GRID IS INITIAL.

    CREATE OBJECT V_GRID
      EXPORTING
        I_PARENT          = V_CONTAINER_I
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CREATE OBJECT V_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*** Define eventos
*----------------------------------------------------------------------*
* Setando os métodos de eventos para ALV O.O

*** User Command
    SET HANDLER V_EVENT_RECEIVER->HANDLE_USER_COMMAND FOR V_GRID.

*** Data Change
    SET HANDLER V_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR V_GRID.

* Status-GUI (Toolbar)
    SET HANDLER V_EVENT_RECEIVER->HANDLE_TOOLBAR FOR V_GRID.

    CALL METHOD V_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    W_LAYOUT-SEL_MODE = 'A'.

    CALL METHOD V_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT                    = W_VARIANT
        I_SAVE                        = 'A'
        IS_LAYOUT                     = W_LAYOUT
      CHANGING
        IT_FIELDCATALOG               = T_FCAT
        IT_OUTTAB                     = T_SAIDA
*       IT_SORT                       = T_SORT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD V_GRID->SET_TOOLBAR_INTERACTIVE.

  ELSE.
    CALL METHOD V_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM ZF_MONTAR_FIELDCAT  CHANGING PT_TABELA   TYPE ANY TABLE
                                  PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    L_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
    L_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
    L_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
    L_DATA         TYPE REF TO DATA.
  FIELD-SYMBOLS:
    <F_TABLE>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA L_DATA LIKE PT_TABELA.
  ASSIGN L_DATA->* TO <F_TABLE>.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = L_SALV_TABLE
        CHANGING
          T_TABLE      = <F_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  L_COLUMNS      = L_SALV_TABLE->GET_COLUMNS( ).
  L_AGGREGATIONS = L_SALV_TABLE->GET_AGGREGATIONS( ).

* Monta o fieldcat
  PT_FIELDCAT = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG( R_COLUMNS      = L_COLUMNS
                                                                   R_AGGREGATIONS = L_AGGREGATIONS ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM ZF_AJUSTE_DESCR_CAMPOS  CHANGING PT_FCAT TYPE LVC_T_FCAT.

  DATA: W_COLOR   TYPE LVC_S_COLO,
        W_COL     TYPE LVC_S_SCOL,
        W_CELLTAB TYPE LVC_S_STYL.

  REFRESH: T_CELLTAB2.
  LOOP AT PT_FCAT ASSIGNING FIELD-SYMBOL(<FS_FCAT>).

    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'USNAM'.
        <FS_FCAT>-NO_OUT = 'X'.
      WHEN 'ZDT_ATUAL'.
        <FS_FCAT>-NO_OUT = 'X'.
      WHEN 'ZHR_ATUAL'.
        <FS_FCAT>-NO_OUT = 'X'.
      WHEN 'ACAO'.
        <FS_FCAT>-NO_OUT = 'X'.
      WHEN 'SAKNR'.
        <FS_FCAT>-REF_TABLE = 'SKA1'.
      WHEN 'KOSTL'.
        <FS_FCAT>-REF_TABLE = 'CSKS'.
      WHEN 'MATNR'.
        <FS_FCAT>-REF_TABLE = 'MARA'.
      WHEN 'MATKL'.
        <FS_FCAT>-REF_TABLE = 'T023T'.
      WHEN OTHERS.
*           <FS_FCAT>-style = cl_gui_alv_grid=>mc_style_enabled.
    ENDCASE.

    W_CELLTAB-FIELDNAME = <FS_FCAT>-FIELDNAME.
    W_CELLTAB-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

    INSERT W_CELLTAB INTO TABLE T_CELLTAB2.
    CLEAR W_CELLTAB.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ATUALIZA_CELL USING P_SAKNR P_KOSTL P_MATNR.

  CASE V_OK9000.
    WHEN 'INCLUIR'.

      IF P_SAKNR IS INITIAL AND P_KOSTL IS INITIAL AND P_MATNR IS INITIAL.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-SAKNR   'SAKNR'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-KOSTL   'KOSTL'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATNR   'MATNR'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATKL   'MATKL'.
      ENDIF.

    WHEN 'ALTERAR'.

      IF P_SAKNR IS INITIAL AND P_KOSTL IS INITIAL AND P_MATNR IS INITIAL.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-SAKNR   'SAKNR'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-KOSTL   'KOSTL'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATNR   'MATNR'.
      ENDIF.

      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATKL       'MATKL'.

    WHEN 'EXCLUIR'.

      IF P_SAKNR IS INITIAL AND P_KOSTL IS INITIAL AND P_MATNR IS INITIAL.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-SAKNR   'SAKNR'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-KOSTL   'KOSTL'.
        PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATNR   'MATNR'.
      ENDIF.

      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATKL       'MATKL'.

    WHEN 'COPIAR'.

      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-SAKNR   'SAKNR'.
      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-KOSTL   'KOSTL'.
      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATNR   'MATNR'.
      PERFORM ZF_PREENCHE_CELL_TAB USING: <FS_SAIDA>-MATKL       'MATKL'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_CELL_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAIDA_KURST  text
*      -->P_1476   text
*----------------------------------------------------------------------*
FORM ZF_PREENCHE_CELL_TAB  USING P_EDIT  TYPE ANY
                                 P_CAMPO TYPE ANY.

  DATA: W_CELLTAB TYPE LVC_S_STYL,
        W_COLOR   TYPE LVC_S_COLO.

  FIELD-SYMBOLS: <FS_CELLTAB> TYPE LVC_S_STYL,
                 <FS_COL>     TYPE LVC_S_SCOL.

  READ TABLE T_CELLTAB ASSIGNING <FS_CELLTAB> WITH KEY FIELDNAME = P_CAMPO.
  IF <FS_CELLTAB> IS ASSIGNED.
    <FS_CELLTAB>-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM ZF_HANDLE_USER_COMMAND  USING  P_UCOMM.

  CLEAR V_OK9000.
  V_OK9000 = P_UCOMM.

  CASE V_OK9000.
    WHEN 'INCLUIR'.
      PERFORM ZF_INSERT_ROW.
    WHEN 'ALTERAR'.
      PERFORM ZF_CHANGE_ROW.
    WHEN 'EXCLUIR'.
      PERFORM ZF_DELETE_ROW.
    WHEN 'COPIAR'.
      PERFORM ZF_COPY_ROW.
    WHEN 'INFO'.
      PERFORM ZF_EXPAND_LEGENDA.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_INSERT_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERT_ROW .

  APPEND INITIAL LINE TO T_SAIDA ASSIGNING <FS_SAIDA>.
  PERFORM ZF_ATUALIZA_CELL USING <FS_SAIDA>-SAKNR
                                 <FS_SAIDA>-KOSTL
                                 <FS_SAIDA>-MATNR.

  IF <FS_SAIDA> IS ASSIGNED.
    <FS_SAIDA>-ACAO  = 'I'.
  ENDIF.

  IF V_GRID->IS_READY_FOR_INPUT( ) EQ 0.

* set edit enabled cells ready for input
    CALL METHOD V_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DELETE_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_DELETE_ROW.

  DATA: T_DELETE     TYPE TABLE OF ZFIT0162,
        T_INDEX_ROWS TYPE LVC_T_ROW,                        "#EC NEEDED
        T_ROW_NO     TYPE LVC_T_ROID,
        W_DELETE     LIKE LINE OF T_DELETE,
        W_ROW_NO     LIKE LINE OF T_ROW_NO,

        L_LINHAS     TYPE SY-TABIX,
        L_ANSWER     TYPE C.

  CALL METHOD V_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = T_INDEX_ROWS
      ET_ROW_NO     = T_ROW_NO.

  DESCRIBE TABLE T_ROW_NO LINES L_LINHAS.

  IF L_LINHAS = 0.
    "Selecionar uma linha.
    MESSAGE I000(Z_FI) WITH 'Selecionar uma linha'(044) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT T_ROW_NO INTO DATA(W_ROW).

    READ TABLE T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>) INDEX W_ROW-ROW_ID.

    CHECK  SY-SUBRC IS INITIAL.

    IF L_ANSWER IS INITIAL
      AND ( <FS_SAIDA>-SAKNR <> SPACE OR <FS_SAIDA>-KOSTL <> SPACE
                 OR <FS_SAIDA>-MATNR <> SPACE OR <FS_SAIDA>-MATKL <> SPACE ).

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'(020)
          TEXT_QUESTION         = 'Deseja realmente eliminar os registros ?'(021)
          TEXT_BUTTON_1         = 'Sim'(022)
          TEXT_BUTTON_2         = 'Não'(023)
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = L_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

    ELSE.
      <FS_SAIDA>-ACAO = GC_ACAO-DELETE_ROW.
    ENDIF.

    IF L_ANSWER = 1.
      <FS_SAIDA>-ACAO = GC_ACAO-DELETE_ROW.
      MOVE-CORRESPONDING <FS_SAIDA> TO W_DELETE.
      APPEND W_DELETE TO T_DELETE.
    ELSE.
      EXIT.
    ENDIF.

  ENDLOOP.

* Eliminar linhas da tabela Interna
  DELETE T_SAIDA WHERE ACAO = GC_ACAO-DELETE_ROW.

  IF T_DELETE[] IS NOT INITIAL.
    DELETE ZFIT0162 FROM TABLE T_DELETE.
    COMMIT WORK.
    MESSAGE S000(Z_FI) WITH 'Registros Eliminados'(043).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECIONAR_DADOS .

* Busca parÂmetros
  REFRESH: T_SAIDA.
  SELECT * FROM ZLEST0208
    INTO CORRESPONDING FIELDS OF TABLE T_SAIDA.

  SORT  T_SAIDA BY SAKNR KOSTL MATNR. "  COMP_ATVBIOL.
  IF T_SAIDA[] IS NOT INITIAL.
    PERFORM ZF_TRATAR_DADOS.
  ENDIF.

  SELECT * INTO TABLE T_SKAT
    FROM SKAT
    WHERE SPRAS = SY-LANGU
      AND KTOPL = '0050'.
  SORT T_SKAT BY SAKNR.

  SELECT * INTO TABLE T_CSKT
     FROM CSKT
    WHERE SPRAS = SY-LANGU
      AND KOKRS = 'MAGI'
      AND DATBI >= SY-DATUM.
  SORT T_CSKT BY KOSTL.

  SELECT * INTO TABLE T_MAKT
     FROM MAKT
    WHERE SPRAS = SY-LANGU.
  SORT T_MAKT BY MATNR.

  SELECT * INTO TABLE T_T023T
     FROM T023T
     WHERE SPRAS = SY-LANGU.
  SORT T_T023T BY MATKL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ATUALIZAR_REGISTROS .

  DATA: T_GRAVAR TYPE TABLE OF ZFIT0162,
        W_GRAVAR LIKE LINE OF T_GRAVAR.

  DELETE T_SAIDA WHERE ACAO <> GC_ACAO-UPDATE_ROW.

  REFRESH: T_GRAVAR.
  LOOP AT T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>).

    CHECK <FS_SAIDA>-ACAO = 'U'.

    MOVE-CORRESPONDING <FS_SAIDA> TO W_GRAVAR.

    W_GRAVAR-USNAM      = SY-UNAME.
    W_GRAVAR-ZDT_ATUAL  = SY-DATUM.
    W_GRAVAR-ZHR_ATUAL  = SY-UZEIT.
    APPEND W_GRAVAR TO  T_GRAVAR.

  ENDLOOP.

  IF T_GRAVAR[] IS NOT INITIAL.

    MODIFY ZLEST0208 FROM TABLE T_GRAVAR.
    COMMIT WORK.

    MESSAGE S000(Z_FI) WITH 'Dados carregados com sucesso'(024).

  ENDIF.

  PERFORM ZF_SELECIONAR_DADOS.

  IF V_GRID->IS_READY_FOR_INPUT( ) NE 0.

* lock edit enabled cells against input
    CALL METHOD V_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHANGE_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_CHANGE_ROW .
  PERFORM ZF_AJUSTA_STATUS_CELL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTA_STATUS_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_AJUSTA_STATUS_CELL .

  IF V_GRID->IS_READY_FOR_INPUT( ) EQ 0.

* set edit enabled cells ready for input
    CALL METHOD V_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ELSE.

* lock edit enabled cells against input
    CALL METHOD V_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_PROCESSAMENTO .

  PERFORM ZF_LIMPA_PARAMETROS.
  PERFORM ZF_SELECIONAR_DADOS.

  CALL SCREEN 9000.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_TRATAR_DADOS .

  FIELD-SYMBOLS: <FS_VALUE> TYPE ANY.

*  DATA: W_REGRAS2 LIKE LINE OF T_REGRAS2.
*
*  LOOP AT T_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>).
*
*    REFRESH: T_REGRAS.
*    SPLIT <FS_SAIDA>-REGRAS AT '/' INTO TABLE T_REGRAS.
*
*    LOOP AT T_REGRAS INTO DATA(W_REGRAS).
*
*      CLEAR W_REGRAS2.
*      SPLIT W_REGRAS-LINHAS AT '=' INTO W_REGRAS2-TIPO W_REGRAS2-VALOR.
*
*      ASSIGN COMPONENT W_REGRAS2-TIPO OF STRUCTURE <FS_SAIDA> TO <FS_VALUE>.
*      CHECK <FS_VALUE> IS ASSIGNED.
*      <FS_VALUE>      = W_REGRAS2-VALOR.
*
*    ENDLOOP.
*
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SPLIT_SCREEN .

  CLEAR: V_DOCKING, V_SPLITTER, V_CONTAINER_H, V_CONTAINER_I.

  CREATE OBJECT V_DOCKING
    EXPORTING
      REPID = SY-REPID
      DYNNR = SY-DYNNR
      RATIO = '95'.

  CREATE OBJECT V_SPLITTER
    EXPORTING
      PARENT  = V_DOCKING
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD V_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = V_CONTAINER_H.

  CALL METHOD V_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = V_CONTAINER_I.

  CALL METHOD V_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 0.

  PERFORM ZF_PREPARAR_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_PREPARAR_HEADER .

  DATA: L_DOCUMENT  TYPE REF TO CL_DD_DOCUMENT,
        L_DOCTABLE  TYPE REF TO CL_DD_TABLE_ELEMENT,
        L_COLUMN1   TYPE REF TO CL_DD_AREA,
        L_COLUMN2   TYPE REF TO CL_DD_AREA,
        L_TEXT(255) TYPE C.  "Text

  CREATE OBJECT L_DOCUMENT.

*----------------------------------------------------------------------
* Linha 1
*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT      = 'Legenda: '(m12)
      SAP_STYLE = CL_DD_AREA=>HEADING.
*      SAP_EMPHASIS = 'STRONG'.

*----------------------------------------------------------------------
* Linha 2
*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'T'(m12)
*     SAP_STYLE    = CL_DD_AREA=>HEADING.
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Transação para executar e pegar dados para nota '
      SAP_EMPHASIS = 'STRONG'.
*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'LC'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Linha do relatório para pegar os valores "Custos" '
      SAP_EMPHASIS = 'STRONG'.
*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.
  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'LT'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Linha do relatório para pegar os valores "Transf.Prod.p/Estque" '
      SAP_EMPHASIS = 'STRONG'.
*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.
  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'C'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Conta Razão para busca do movimento do período '
      SAP_EMPHASIS = 'STRONG'.

*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.
  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'GO'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Grupo de Ordem  '
      SAP_EMPHASIS = 'STRONG'.

*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.
  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'TM'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Tipo de Movimento de Imobilizado  '
      SAP_EMPHASIS = 'STRONG'.

*----------------------------------------------------------------------
  CALL METHOD L_DOCUMENT->NEW_LINE.
  CALL METHOD L_DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 183.
  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = 'CL'(m12)
      SAP_EMPHASIS = 'STRONG'.

  CALL METHOD L_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = ' = Classe do Imobilizado '
      SAP_EMPHASIS = 'STRONG'.

*********************************************
  CALL METHOD L_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      PARENT = V_CONTAINER_H.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPA_PARAMETROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_LIMPA_PARAMETROS .
  REFRESH: T_SAIDA. CLEAR V_INFO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_STATUS OUTPUT.
  SET PF-STATUS 'ZGCTB_9000'.
  SET TITLEBAR  'ZUMM_TIT_PAR'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_USER_COMMAND INPUT.

  CASE V_OK9000.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SALVAR'.
      PERFORM ZF_SALVAR.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SALVAR .

  DATA: L_ERRO TYPE C.

  CALL METHOD V_GRID->CHECK_CHANGED_DATA.

  IF L_ERRO IS INITIAL.
    PERFORM ZF_ATUALIZAR_REGISTROS.
  ELSE.
    MESSAGE S000(Z_FI) WITH 'Dados Incompletos'(042) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXPAND_LEGENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_EXPAND_LEGENDA .

  IF V_INFO IS INITIAL.
    CALL METHOD V_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 27.

    V_INFO = 'X'.
  ELSE.
    CALL METHOD V_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 0.

    CLEAR V_INFO.
  ENDIF.

  CALL METHOD V_GRID->REFRESH_TABLE_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_COPY_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_COPY_ROW .

  DATA: T_DELETE     TYPE TABLE OF ZFIT0162,
        T_INDEX_ROWS TYPE LVC_T_ROW,                        "#EC NEEDED
        T_ROW_NO     TYPE LVC_T_ROID,
        W_DELETE     LIKE LINE OF T_DELETE,
        W_ROW_NO     LIKE LINE OF T_ROW_NO,

        L_LINHAS     TYPE SY-TABIX,
        L_ANSWER     TYPE C.

  CALL METHOD V_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = T_INDEX_ROWS
      ET_ROW_NO     = T_ROW_NO.

  DESCRIBE TABLE T_ROW_NO LINES L_LINHAS.

  IF L_LINHAS = 0.
    "Selecionar uma linha.
    MESSAGE I000(Z_FI) WITH 'Selecionar uma linha'(044) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(T_SAIDA_AUX) = T_SAIDA.

  LOOP AT T_ROW_NO INTO DATA(W_ROW).

    READ TABLE T_SAIDA_AUX ASSIGNING FIELD-SYMBOL(<FS_SAIDA_AUX>) INDEX W_ROW-ROW_ID.
    IF SY-SUBRC IS INITIAL.
      <FS_SAIDA_AUX>-ACAO = 'C'.
      APPEND <FS_SAIDA_AUX> TO T_SAIDA.
    ENDIF.

  ENDLOOP.

  IF V_GRID->IS_READY_FOR_INPUT( ) EQ 0.

* set edit enabled cells ready for input
    CALL METHOD V_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ENDIF.

ENDFORM.
