FUNCTION-POOL Z_TREINA.                     "MESSAGE-ID ..

* INCLUDE LZ_TREINAD...                      " Local class definition


TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

DATA: OK-CODE     TYPE SY-UCOMM,
      WL_CONT     TYPE I,
      WL_ERRO(1),
      WA_ZMMT0105 TYPE ZMMT0105.

DATA: P_LIFNR     TYPE EKKO-LIFNR,
      P_EBELN     TYPE EKPO-EBELN,
      P_EBELP     TYPE EKPO-EBELP,
      P_SAKNR     TYPE SKA1-SAKNR,
      P_MATNR     TYPE MARA-MATNR,
      P_CPF       TYPE ZMMT0104-CPF,
      P_TIPO(1),
      W_ANSWER(1),
      P_BTN(1).


*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR     DEFINITION DEFERRED.

DATA:  C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.
*Declaration for toolbar buttons
DATA: TY_TOOLBAR TYPE STB_BUTTON.

"ALV
DATA: GRID1              TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CONTAINER        TYPE SCRFNAME VALUE 'CC_TREINA',
      CONTAINER_1        TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2        TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER           TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      OBG_TOOLBAR        TYPE REF TO LCL_ALV_TOOLBAR.


DATA: WA_STABLE      TYPE LVC_S_STBL,
      WA_LAYOUT      TYPE LVC_S_LAYO,

      ESTRUTURA      TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA   TYPE TY_ESTRUTURA,

      T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT.

DATA: BEGIN OF TG_TREINA OCCURS 0,
        MARK(1),
        ICON(4),
        EBELP          TYPE ZMMT0105-EBELP,
        CPF            TYPE ZMMT0104-CPF,
        ID_LMS         TYPE ZMMT0104-ID_LMS,
        NOME_CURSO     TYPE ZMMT0104-NOME_CURSO,
        C_CUSTO        TYPE ZMMT0104-C_CUSTO,
        ID_TURMA       TYPE ZMMT0104-ID_TURMA,
        TP_SOLICITACAO TYPE ZMMT0104-TP_SOLICITACAO,
        ATENCAO        TYPE ZMMT0105-ATENCAO,
      END OF TG_TREINA.


CONSTANTS: C_X               TYPE C VALUE 'X'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_DOUBLE_CLICK.
    IF  E_ROW GT 0.
      READ TABLE TG_TREINA INTO DATA(WL_TREINA) INDEX  E_ROW.
      SELECT SINGLE *
        FROM ZMMT0105
        INTO @DATA(WA_ZMMT0105)
        WHERE ZMMT0105~EBELN  NE @P_EBELN
        AND   ZMMT0105~EBELP  NE @P_EBELP
        AND   ZMMT0105~ID_LMS EQ @WL_TREINA-ID_LMS.

      IF SY-SUBRC NE 0.
        IF WL_TREINA-ICON = ICON_CHECKED.
          CLEAR WL_TREINA-ICON.
        ELSE.
          WL_TREINA-ICON = ICON_CHECKED.
        ENDIF.
        MODIFY TG_TREINA FROM WL_TREINA INDEX E_ROW TRANSPORTING ICON.
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      ELSE.
*        MESSAGE |Conta ja utilizada para este ID LMS/Matricula'  { WL_TREINA-SAKNR } pedido/Item { WA_ZMMT0105-EBELN } / { WA_ZMMT0105-EBELP }| TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      CONSTRUCTOR
        IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    WL_DESACTIVE = SPACE.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = 'ADD'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = 'DEL'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 5.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*   variable for Toolbar Button
    TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  = 'SHOW_MSGRE'.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: TG_TREINA_AUX LIKE TABLE OF TG_TREINA,
          WG_TREINA_AUX LIKE LINE OF TG_TREINA,
          WCONT         TYPE I,
          TL_INDEX_ROWS TYPE LVC_T_ROW,
          WL_INDEX_ROWS TYPE LVC_S_ROW.


    CASE E_UCOMM.
      WHEN 'ADD'.

        TG_TREINA_AUX[] = TG_TREINA[].
        REFRESH: TG_TREINA.
        LOOP AT TG_TREINA_AUX INTO WG_TREINA_AUX.
          APPEND WG_TREINA_AUX TO TG_TREINA.
        ENDLOOP.

        CLEAR: WG_TREINA_AUX.
        DESCRIBE TABLE TG_TREINA_AUX LINES WCONT.
        ADD 1 TO WCONT.

        APPEND WG_TREINA_AUX TO TG_TREINA.
      WHEN 'DEL'.

        CALL METHOD GRID1->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_INDEX_ROWS.

        LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
          DELETE TG_TREINA INDEX WL_INDEX_ROWS-INDEX.
        ENDLOOP.
    ENDCASE.

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.


  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA.
  "
  DATA WCONT TYPE I.

  IF P_BTN = ' '.
    DELETE FROM ZMMT0105
             WHERE EBELN   = ' '
             AND   LIFNR   = P_LIFNR
             AND   USUARIO = SY-UNAME
             AND   DATA    = SY-DATUM
             AND   TIPO    = P_TIPO.
  ELSE.
    IF P_EBELN IS NOT INITIAL.
      DELETE FROM ZMMT0105
            WHERE EBELN   = P_EBELN
            AND   EBELP   = P_EBELP
            AND   LIFNR   = P_LIFNR
            AND   TIPO    = P_TIPO.
    ELSE.
      DELETE FROM ZMMT0105
           WHERE EBELN   = P_EBELN
           AND   EBELP   = P_EBELP
           AND   LIFNR   = P_LIFNR
           AND   USUARIO = SY-UNAME
           AND   DATA    = SY-DATUM
           AND   TIPO    = P_TIPO.
    ENDIF.

  ENDIF.
  "
  CLEAR WCONT.
  LOOP AT  TG_TREINA.
    IF TG_TREINA-ICON EQ ICON_CHECKED OR P_BTN = ' '.
      CLEAR WA_ZMMT0105.
      MOVE-CORRESPONDING TG_TREINA TO WA_ZMMT0105.
      WA_ZMMT0105-LIFNR   = P_LIFNR.
      WA_ZMMT0105-EBELN   = P_EBELN.
      IF P_BTN = ' '.
        WA_ZMMT0105-EBELP   = TG_TREINA-EBELP.
      ELSE.
        WA_ZMMT0105-EBELP   = P_EBELP.
      ENDIF.
      "
      WA_ZMMT0105-TIPO    = P_TIPO.
      WA_ZMMT0105-DATA    = SY-DATUM.
      WA_ZMMT0105-HORA    = SY-UZEIT.
      WA_ZMMT0105-USUARIO = SY-UNAME.
      IF WA_ZMMT0105-ATENCAO IS INITIAL.
        IF W_ANSWER = '1'.
          WA_ZMMT0105-ATENCAO = 'S'.
        ELSEIF W_ANSWER = '2'.
          WA_ZMMT0105-ATENCAO = 'N'.
        ENDIF.
      ENDIF.
      MODIFY ZMMT0105 FROM WA_ZMMT0105.
      ADD 1 TO WCONT.
    ENDIF.
  ENDLOOP.
  "
  IF W_ANSWER IS NOT INITIAL AND WCONT = 0. " Atenção 1 ou 2
    CLEAR WA_ZMMT0105.
    WA_ZMMT0105-LIFNR   = P_LIFNR.
    WA_ZMMT0105-EBELP   = P_EBELP.
*    WA_ZMMT0105-MATNR   = P_MATNR.
*    WA_ZMMT0105-SAKNR   = P_SAKNR.
    IF W_ANSWER = '1'.
      WA_ZMMT0105-ATENCAO = 'S'.
    ELSE.
      WA_ZMMT0105-ATENCAO = 'N'.
    ENDIF.
    "
    WA_ZMMT0105-TIPO    = P_TIPO.
    WA_ZMMT0105-DATA    = SY-DATUM.
    WA_ZMMT0105-HORA    = SY-UZEIT.
    WA_ZMMT0105-USUARIO = SY-UNAME.
    MODIFY ZMMT0105 FROM WA_ZMMT0105.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .

  PERFORM MONTAR_ESTRUTURA USING:
          1  ' '         ' '             'TG_TREINA' 'ICON'          'Sel.'                '03' ' ' ' ' ' ',
          1  'ZMMT0104'  'ID_LMS'        'TG_TREINA' 'ID_LMS'        'ID Treinamento'      '12' ' ' ' ' ' ',
          1  'ZMMT0104'  'CPF'           'TG_TREINA' 'CPF'           'CPF'                 '12' ' ' ' ' ' ',
          1  'ZMMT0104'  'NOME_CURSO'    'TG_TREINA' 'NOME_CURSO'    'Nome Treinamento'    '40' ' ' ' ' ' ',
          1  'ZMMT0104'  'C_CUSTO'       'TG_TREINA' 'C_CUSTO'       'C.Custo'             '12' ' ' ' ' ' ',
          1  'ZMMT0104'  'TP_SOLICITACAO'      'TG_TREINA' 'TP_SOLICITACAO'      'Id. Turma'           '12' ' ' ' ' ' ',
          1  'ZMMT0104'  'ID_TURMA'      'TG_TREINA' 'ID_TURMA'      'Tipo'                '08' ' ' ' ' ' '.

ENDFORM.

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.

  IF P_FIELD EQ 'ICON'.
    W_FIELDCATALOG-ICON = C_X.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE,
        WL_QTD(16).

  "
  IF G_CUSTOM_CONTAINER IS INITIAL.
    WA_LAYOUT-NO_TOOLBAR = C_X.
    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = SPACE.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-GRID_TITLE = 'Selecione um curso'.


    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER_1.


    PERFORM MONTAR_LAYOUT.
*
    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    REFRESH TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        I_DEFAULT            = 'X'
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_TREINA[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
     LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.
  ELSE.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN 'CANCEL'
      OR 'EXIT'.
      REFRESH TG_TREINA.
      IF W_ANSWER = '1'.
        SELECT SINGLE *
          FROM ZMMT0105
          INTO @DATA(W105)
             WHERE EBELN   = ' '
             AND   LIFNR   = @P_LIFNR
             AND   USUARIO = @SY-UNAME
             AND   DATA    = @SY-DATUM
             AND   TIPO    = @P_TIPO.
        IF SY-SUBRC NE 0.
          PERFORM F_GRAVA.
        ENDIF.

      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      CLEAR WL_CONT.
      LOOP AT  TG_TREINA.
        IF TG_TREINA-ICON EQ ICON_CHECKED.
          ADD 1 TO WL_CONT.
        ENDIF.
      ENDLOOP.
      IF WL_CONT = 0.
        MESSAGE 'Selecione um curso!' TYPE 'I'.
        EXIT.
      ENDIF.
      "
      PERFORM F_GRAVA.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
