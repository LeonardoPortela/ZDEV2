************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 17.08.2016                                          *
* Objetivo    ...: Cadastro de Valores do Frete                        *
* Transação   ...: ZSDT0056                                            *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 17.08.2016  Welgem Barbosa   Criação      16:00                      *
************************************************************************
REPORT ZSDR0055.

TABLES ZSDT0046.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: V_BUKRS FOR ZSDT0046-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,  " EMRPESA
                V_AUAR  FOR ZSDT0046-AUART,                                 " TIPO DE OV
                V_SEQ   FOR ZSDT0046-SEQ_DESC,                              " TIPO DE DESCONTO
                V_VAL   FOR ZSDT0046-VAL_ATE.                               " VALIDADE

SELECTION-SCREEN END OF BLOCK B1.

DATA: BEGIN OF TL_UCOMM OCCURS 0,
        UCOMM TYPE  SY-UCOMM,
      END OF TL_UCOMM.

DATA: IT_ZSDT0046 TYPE TABLE OF ZSDT0046,
      WA_ZSDT0046 TYPE ZSDT0046.

DATA: IT_0037 TYPE TABLE OF ZSDT0046,
      WA_0037 TYPE ZSDT0046.

DATA: IT_SAIDA TYPE TABLE OF ZSDT0046.

DATA: V_EDIT TYPE N.

DATA: WA_LAYOUT TYPE LVC_S_LAYO,
      WA_STABLE TYPE LVC_S_STBL,
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONT_  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV_   TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT   TYPE LVC_T_FCAT,
      WA_FCAT   TYPE LVC_S_FCAT,
      C_ALV_TM  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      IT_S_ROWS TYPE LVC_T_ROW,
      WA_S_ROWS TYPE LVC_S_ROW,
      C_TELA    TYPE SY-DYNNR VALUE '0102'.

FIELD-SYMBOLS: <WA_FCAT>     TYPE LVC_S_FCAT,
               <WA_ZSDT0046> TYPE ZSDT0046.

CLASS ZCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR IMPORTING IO_ALV_GRID  TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR  FOR EVENT TOOLBAR               OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_INTERACTIVE SENDER,
      ON_DT_CH    FOR EVENT DATA_CHANGED          OF CL_GUI_ALV_GRID IMPORTING ER_DATA_CHANGED  E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,
      ON_HANDLE   FOR EVENT USER_COMMAND          OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,
      ON_CLICK    FOR EVENT HOTSPOT_CLICK         OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.

CLASS ZCL_EVENTS IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    CREATE OBJECT C_ALV_TM
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.

  METHOD ON_TOOLBAR.

    FIELD-SYMBOLS: <LS_TOOLBAR>  TYPE STB_BUTTON.

    LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING <LS_TOOLBAR>.
      CASE <LS_TOOLBAR>-FUNCTION.
        WHEN '&CHECK' OR '&REFRESH' OR '&LOCAL&CUT' OR '&LOCAL&COPY' OR '&LOCAL&PASTE' OR '&LOCAL&UNDO'
          OR '&LOCAL&APPEND' OR '&LOCAL&INSERT_ROW' OR '&LOCAL&DELETE_ROW' OR '&LOCAL&COPY_ROW'.
          DELETE E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
      ENDCASE.
    ENDLOOP.

    CALL METHOD C_ALV_TM->REORGANIZE( IO_ALV_TOOLBAR = E_OBJECT ).

  ENDMETHOD.

  METHOD ON_DT_CH.
    DATA: LS_GOOD   TYPE LVC_S_MODI.
    FIELD-SYMBOLS <WA_ZSDT0046> TYPE ZSDT0046.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      READ TABLE IT_ZSDT0046 ASSIGNING <WA_ZSDT0046> INDEX LS_GOOD-ROW_ID.
      CONDENSE LS_GOOD-VALUE NO-GAPS.

      CASE LS_GOOD-FIELDNAME.
        WHEN 'VAL_ATE'.   <WA_ZSDT0046>-VAL_ATE   = LS_GOOD-VALUE.
        WHEN 'VLR_DESC'.  <WA_ZSDT0046>-VLR_DESC  = LS_GOOD-VALUE.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD ON_HANDLE. ENDMETHOD.
  METHOD ON_CLICK. ENDMETHOD.

ENDCLASS.

DATA: OBG_EVENTS         TYPE REF TO ZCL_EVENTS.

START-OF-SELECTION.
  AUTHORITY-CHECK OBJECT 'M_MATE_BUK' ID 'BUKRS' FIELD V_BUKRS-LOW.
  IF SY-SUBRC IS INITIAL.
    PERFORM SELECIONA_DADOS.
  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Usuário sem Permissão para Empresa ' V_BUKRS-LOW.
    EXIT.
  ENDIF.

END-OF-SELECTION.

FORM SELECIONA_DADOS.

  SELECT *
    FROM ZSDT0046
      INTO TABLE IT_ZSDT0046
        WHERE BUKRS IN V_BUKRS
          AND AUART IN V_AUAR
          AND SEQ_DESC IN V_SEQ
          AND VAL_ATE IN V_VAL.

  FREE: V_EDIT.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  PERFORM MANAGER_BTN.


  SET PF-STATUS 'MAIN100' EXCLUDING TL_UCOMM.
  SET TITLEBAR 'MAIN100'.


  PERFORM MONTA_LAYOUT.
  PERFORM MONTA_CATALOGO.

  IF WA_CONT IS INITIAL.

    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME = 'C_01'.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_SHELLSTYLE    = 0
        I_PARENT        = WA_CONT
        I_APPL_EVENTS   = SPACE
        I_FCAT_COMPLETE = SPACE.

    CREATE OBJECT OBG_EVENTS
      EXPORTING
        IO_ALV_GRID = WA_ALV.

    SET HANDLER: OBG_EVENTS->ON_DT_CH    FOR WA_ALV,
                 OBG_EVENTS->ON_TOOLBAR  FOR WA_ALV,
                 OBG_EVENTS->ON_HANDLE   FOR WA_ALV,
                 OBG_EVENTS->ON_CLICK    FOR WA_ALV.

    CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = 'X'
      CHANGING
        IT_OUTTAB       = IT_ZSDT0046
        IT_FIELDCATALOG = IT_FCAT.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD WA_ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
    CALL METHOD WA_ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).

  ELSE.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

  IF WA_CONT_ IS INITIAL.

    CREATE OBJECT WA_CONT_
      EXPORTING
        CONTAINER_NAME = 'C_02'.

    CREATE OBJECT WA_ALV_
      EXPORTING
        I_SHELLSTYLE    = 0
        I_PARENT        = WA_CONT_
        I_APPL_EVENTS   = SPACE
        I_FCAT_COMPLETE = SPACE.

    CREATE OBJECT OBG_EVENTS
      EXPORTING
        IO_ALV_GRID = WA_ALV_.

    SET HANDLER: OBG_EVENTS->ON_TOOLBAR  FOR WA_ALV_,
                 OBG_EVENTS->ON_DT_CH    FOR WA_ALV.

    CALL METHOD WA_ALV_->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = 'X'
      CHANGING
        IT_OUTTAB       = IT_SAIDA
        IT_FIELDCATALOG = IT_FCAT.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD WA_ALV_->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
    CALL METHOD WA_ALV_->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).

  ELSE.
    CALL METHOD WA_ALV_->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>FLUSH.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.

  C_TELA = '0102'.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EDIT'.
      PERFORM EDITAR_TABLE.
    WHEN 'SAVE'.
      PERFORM SALVAR.
    WHEN 'DEL'.
      PERFORM DELETAR.
    WHEN 'ADD'.
      PERFORM ADICIONAR.
    WHEN 'COPY'.
      PERFORM COPIAR.
    WHEN 'REFRESH'.
      PERFORM SELECIONA_DADOS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_LAYOUT .

  WA_LAYOUT-ZEBRA      = ABAP_TRUE.
  WA_LAYOUT-STYLEFNAME = SPACE .
  WA_LAYOUT-GRID_TITLE = SPACE .
  WA_LAYOUT-INFO_FNAME = 'COLOR'.
  WA_LAYOUT-SEL_MODE   = 'C'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_CATALOGO .

  FREE: IT_FCAT, WA_FCAT.

  DEFINE ALV.
    WA_FCAT-HOTSPOT   = &1.
    WA_FCAT-REF_TABLE = &2.
    WA_FCAT-REF_FIELD = &3.
    WA_FCAT-TABNAME   = &4.
    WA_FCAT-FIELDNAME = &5.
    WA_FCAT-SCRTEXT_L = &6.
    WA_FCAT-SCRTEXT_M = &6.
    WA_FCAT-NO_ZERO   = &7.
    WA_FCAT-OUTPUTLEN = &8.
    WA_FCAT-EDIT      = &9.

    APPEND WA_FCAT TO IT_FCAT.
    CLEAR WA_FCAT.
  END-OF-DEFINITION.

  ALV:
    '' 'ZSDT0046' 'BUKRS'    'IT_ZSDT0046' 'BUKRS'    ''                 '' '05' '',
    '' ''         ''         'IT_ZSDT0046' 'SEQ_DESC' 'Tipo de Desconto' '' ''   '',
    '' 'ZSDT0046' 'VAL_DE'   'IT_ZSDT0046' 'VAL_DE'   ''                 '' '12' '',
    '' 'ZSDT0046' 'AUART'    'IT_ZSDT0046' 'AUART'    ''                 '' '05' '',
    '' 'ZSDT0046' 'VAL_ATE'  'IT_ZSDT0046' 'VAL_ATE'  ''                 '' '12'   '',
    '' 'ZSDT0046' 'VLR_DESC' 'IT_ZSDT0046' 'VLR_DESC' ''                 '' '10' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDITAR_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR_TABLE .

  DATA: DIVI TYPE N.

  ADD 1 TO V_EDIT.
  DIVI = V_EDIT MOD 2.
  LOOP AT IT_FCAT ASSIGNING <WA_FCAT> WHERE FIELDNAME EQ 'VAL_ATE'
                                         OR FIELDNAME EQ 'VLR_DESC'.
    IF NOT DIVI IS INITIAL.
      <WA_FCAT>-EDIT = ABAP_TRUE.
    ENDIF.
  ENDLOOP.

  CALL METHOD WA_ALV->SET_FRONTEND_FIELDCATALOG( IT_FIELDCATALOG = IT_FCAT ).

  CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.


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

  C_TELA = '0102'.

  DELETE FROM ZSDT0046
          WHERE BUKRS IN V_BUKRS
            AND SEQ_DESC IN V_SEQ
            AND VAL_DE IN V_VAL
            AND AUART IN V_AUAR.

  LOOP AT IT_ZSDT0046 INTO WA_ZSDT0046.
    APPEND WA_ZSDT0046 TO IT_SAIDA.
  ENDLOOP.

  DELETE IT_SAIDA WHERE SEQ_DESC EQ ''
                     OR VAL_DE EQ ''
                     OR AUART EQ ''.

  MODIFY ZSDT0046 FROM TABLE IT_SAIDA.

  PERFORM BLOCK_CAMPOS.
  PERFORM SELECIONA_DADOS.

  FREE: IT_SAIDA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETAR.

  FREE: IT_S_ROWS[], WA_S_ROWS, IT_SAIDA.

  CALL METHOD WA_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_S_ROWS.

  LOOP AT IT_S_ROWS INTO WA_S_ROWS.
    READ TABLE IT_ZSDT0046 ASSIGNING <WA_ZSDT0046> INDEX WA_S_ROWS-INDEX.
    <WA_ZSDT0046>-BUKRS = ''.
  ENDLOOP.

  DELETE IT_ZSDT0046 WHERE BUKRS EQ ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADICIONAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADICIONAR.

  FREE WA_ZSDT0046.
  DATA: CONTADOR TYPE SY-TABIX.

  C_TELA = '0101'.

  WHILE CONTADOR NE 5.
    WA_0037-BUKRS = V_BUKRS-LOW.
    APPEND WA_0037 TO IT_0037.
    ADD 1 TO CONTADOR.
  ENDWHILE.

  IT_SAIDA = IT_0037.

  LOOP AT IT_FCAT ASSIGNING <WA_FCAT>.
    IF <WA_FCAT>-FIELDNAME NE 'BUKRS'.
      <WA_FCAT>-EDIT = ABAP_TRUE.
    ENDIF.
  ENDLOOP.

  CALL METHOD WA_ALV_->SET_FRONTEND_FIELDCATALOG( IT_FIELDCATALOG = IT_FCAT ).

  CALL METHOD WA_ALV_->REFRESH_TABLE_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COPIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPIAR .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MANAGER_BTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANAGER_BTN .

  FREE TL_UCOMM.

  CASE SY-UCOMM.
    WHEN 'ADD'.
      TL_UCOMM = 'REFRESH'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'EDIT'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'DEL'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'COPY'. APPEND TL_UCOMM. CLEAR TL_UCOMM.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BLOCK_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BLOCK_CAMPOS .

  LOOP AT IT_FCAT ASSIGNING <WA_FCAT>.
    <WA_FCAT>-EDIT = ABAP_FALSE.
  ENDLOOP.

  CALL METHOD WA_ALV->SET_FRONTEND_FIELDCATALOG( IT_FIELDCATALOG = IT_FCAT ).

  CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

ENDFORM.
