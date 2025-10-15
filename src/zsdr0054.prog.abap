************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 17.08.2016                                          *
* Objetivo    ...: Cadratro de Valores de Frete                        *
* Transação   ...: ZSDT0079                                            *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 17.08.2016  Welgem Barbosa   Criação      07:40                      *
************************************************************************
REPORT ZSDR0054.

TABLES ZSDT0037.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: V_BUKRS FOR ZSDT0037-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,  " EMRPESA
                V_F_O FOR ZSDT0037-FILIAL_ORIGEM,                                 " FILIAL ORIGEM
                V_F_D FOR ZSDT0037-FILIAL_DESTINO,                                " FILIAL DESTINO
                V_MATKL FOR ZSDT0037-MATKL.                                       " GRUPO DE MERCADORIA
*                V_WAERS FOR ZSDT0037-WAERS.                                      " MOEDA
SELECTION-SCREEN END OF BLOCK B1.

TYPES BEGIN OF TY_0117.
        INCLUDE STRUCTURE ZSDT0117.
TYPES STATUS(8) TYPE C.
TYPES END OF TY_0117.

DATA: BEGIN OF TL_UCOMM OCCURS 0,
        UCOMM TYPE  SY-UCOMM,
      END OF TL_UCOMM.

DATA: IT_ZSDT0037 TYPE TABLE OF ZSDT0037,
      WA_ZSDT0037 TYPE ZSDT0037.

DATA: IT_0037 TYPE TABLE OF ZSDT0037,
      WA_0037 TYPE ZSDT0037.

DATA: IT_0117 TYPE TABLE OF ZSDT0117.
DATA: LT_0117 TYPE TABLE OF TY_0117.
DATA: WA_0117 TYPE ZSDT0117.

DATA: P_KURSK TYPE ZSDT0117-KURSK.

DATA: IT_SAIDA TYPE TABLE OF ZSDT0037.

DATA: V_EDIT TYPE N.
DATA: NUM TYPE N.

DATA: WA_LAYOUT TYPE LVC_S_LAYO,
      WA_STABLE TYPE LVC_S_STBL,
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONT_NEW  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV_NEW   TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONT_H TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV_H  TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT   TYPE LVC_T_FCAT,
      WA_FCAT   TYPE LVC_S_FCAT,
      IT_FCAT_H TYPE LVC_T_FCAT,
      WA_FCAT_H TYPE LVC_S_FCAT,
      C_ALV_TM  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      IT_S_ROWS TYPE LVC_T_ROW,
      WA_S_ROWS TYPE LVC_S_ROW,
      C_TELA    TYPE SY-DYNNR VALUE '0102'.

FIELD-SYMBOLS: <WA_FCAT>     TYPE LVC_S_FCAT,
               <WA_ZSDT0037> TYPE ZSDT0037.

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
    FIELD-SYMBOLS <WA_ZSDT0037> TYPE ZSDT0037.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      READ TABLE IT_ZSDT0037 ASSIGNING <WA_ZSDT0037> INDEX LS_GOOD-ROW_ID.
      CONDENSE LS_GOOD-VALUE NO-GAPS.

      CASE LS_GOOD-FIELDNAME.
        WHEN 'VAL_DE'.    <WA_ZSDT0037>-VAL_DE    = LS_GOOD-VALUE.
        WHEN 'VAL_ATE'.   <WA_ZSDT0037>-VAL_ATE   = LS_GOOD-VALUE.
        WHEN 'VLR_FRETE'. <WA_ZSDT0037>-VLR_FRETE = LS_GOOD-VALUE.
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
    FROM ZSDT0037
      INTO TABLE IT_ZSDT0037
        WHERE BUKRS IN V_BUKRS
          AND FILIAL_ORIGEM IN V_F_O
          AND FILIAL_DESTINO IN V_F_D
          AND MATKL IN V_MATKL.
*          AND WAERS IN V_WAERS.

  SELECT *
      FROM ZSDT0117
      INTO TABLE IT_0117
      WHERE BUKRS IN V_BUKRS.

  READ TABLE IT_0117 INTO WA_0117 WITH KEY BUKRS      = V_BUKRS-LOW
                                           DESATIVADO = ABAP_FALSE.

  MOVE WA_0117-KURSK TO P_KURSK.

  PERFORM AGRUPA_DADOS_HISTORICO.


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
        IT_OUTTAB       = IT_ZSDT0037
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

  IF WA_CONT_NEW IS INITIAL.

    CREATE OBJECT WA_CONT_NEW
      EXPORTING
        CONTAINER_NAME = 'C_02'.

    CREATE OBJECT WA_ALV_NEW
      EXPORTING
        I_SHELLSTYLE    = 0
        I_PARENT        = WA_CONT_NEW
        I_APPL_EVENTS   = SPACE
        I_FCAT_COMPLETE = SPACE.

    CREATE OBJECT OBG_EVENTS
      EXPORTING
        IO_ALV_GRID = WA_ALV_NEW.

    SET HANDLER: OBG_EVENTS->ON_TOOLBAR  FOR WA_ALV_NEW,
                 OBG_EVENTS->ON_DT_CH    FOR WA_ALV.

    CALL METHOD WA_ALV_NEW->SET_TABLE_FOR_FIRST_DISPLAY
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

    CALL METHOD WA_ALV_NEW->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
    CALL METHOD WA_ALV_NEW->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).

  ELSE.
    CALL METHOD WA_ALV_NEW->REFRESH_TABLE_DISPLAY.
  ENDIF.


  IF NOT WA_CONT_H IS INITIAL.
    CALL METHOD WA_CONT_H->FREE
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2.
  ENDIF.

  IF SY-UCOMM EQ 'HIST'.

    CREATE OBJECT WA_CONT_H
      EXPORTING
        CONTAINER_NAME = 'C_03'. "HISTORICO

    CREATE OBJECT WA_ALV_H
      EXPORTING
        I_PARENT = WA_CONT_H.

    CALL METHOD WA_ALV_H->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_OUTTAB       = LT_0117
        IT_FIELDCATALOG = IT_FCAT_H.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
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
    '' 'ZSDT0037' 'BUKRS'          'IT_ZSDT0037' 'BUKRS'          '' '' '06' '',
    '' 'ZSDT0037' 'MATKL'          'IT_ZSDT0037' 'MATKL'          '' '' '12' '',
    '' 'ZSDT0037' 'FILIAL_ORIGEM'  'IT_ZSDT0037' 'FILIAL_ORIGEM'  '' '' '05' '',
    '' 'ZSDT0037' 'MEINS'          'IT_ZSDT0037' 'MEINS'          '' '' ''   '',
    '' 'ZSDT0037' 'FILIAL_DESTINO' 'IT_ZSDT0037' 'FILIAL_DESTINO' '' '' '05' '',
    '' 'ZSDT0037' 'VAL_DE'         'IT_ZSDT0037' 'VAL_DE'         '' '' ''   '',
    '' 'ZSDT0037' 'VAL_ATE'        'IT_ZSDT0037' 'VAL_ATE'        '' '' ''   '',
    '' 'ZSDT0037' 'VLR_FRETE'      'IT_ZSDT0037' 'VLR_FRETE'      '' '' ''   '',
    '' 'ZSDT0037' 'WAERS'          'IT_ZSDT0037' 'WAERS'          '' '' ''   ''.

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
                                         OR FIELDNAME EQ 'VAL_DE'
                                         OR FIELDNAME EQ 'VLR_FRETE'.
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

  DELETE FROM ZSDT0037
          WHERE BUKRS IN V_BUKRS
            AND FILIAL_ORIGEM IN V_F_O
            AND FILIAL_DESTINO IN V_F_D
            AND MATKL IN V_MATKL.
*            AND WAERS IN V_WAERS.

  LOOP AT IT_ZSDT0037 INTO WA_ZSDT0037.
    APPEND WA_ZSDT0037 TO IT_SAIDA.
  ENDLOOP.

  DELETE IT_SAIDA WHERE FILIAL_ORIGEM EQ ''
                     OR FILIAL_DESTINO EQ ''
                     OR MATKL EQ ''
                     OR WAERS EQ ''.

  MODIFY ZSDT0037 FROM TABLE IT_SAIDA.

* VERIFICA SE TEM DIFERENÇA NAS TAXAS
  IF P_KURSK NE WA_0117-KURSK.
* SE A BUSCA RETORNAR VALOR DESATIVA O ANTERIOR E INSERE UM NOVO REGISTRO


    IF NOT WA_0117 IS INITIAL.

* UPDATE PARA DESATIVAR A ANTERIOR
      UPDATE ZSDT0117
        SET DESATIVADO = ABAP_TRUE
            WHERE BUKRS      EQ V_BUKRS-LOW
              AND KURSK      EQ WA_0117-KURSK
              AND DESATIVADO EQ ABAP_FALSE.

* LOG DE DESATIVAÇÃO
      NUM = LINES( IT_0117 ).
      ADD 1 TO NUM.

      WA_0117-SEQ         = NUM.
      WA_0117-DESATIVADO  = ABAP_FALSE.
      WA_0117-KURSK       = P_KURSK.
      WA_0117-USNAM       = SY-UNAME.
      WA_0117-DATA_ATUAL  = SY-DATUM.
      WA_0117-HORA_ATUAL  = SY-UZEIT.

    ELSE.

      WA_0117-SEQ         = 1.
      WA_0117-BUKRS       = V_BUKRS-LOW.
      WA_0117-DESATIVADO  = ABAP_FALSE.
      WA_0117-KURSK       = P_KURSK.
      WA_0117-USNAM       = SY-UNAME.
      WA_0117-DATA_ATUAL  = SY-DATUM.
      WA_0117-HORA_ATUAL  = SY-UZEIT.

    ENDIF.

* INSERE UM NOVO REGISTRO
    INSERT INTO ZSDT0117 VALUES WA_0117.

  ENDIF.

  PERFORM BLOCK_CAMPOS.
  PERFORM SELECIONA_DADOS.

  FREE: IT_SAIDA, WA_0117, NUM.

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
    READ TABLE IT_ZSDT0037 ASSIGNING <WA_ZSDT0037> INDEX WA_S_ROWS-INDEX.
    <WA_ZSDT0037>-BUKRS = ''.
  ENDLOOP.

  DELETE IT_ZSDT0037 WHERE BUKRS EQ ''.

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

  FREE WA_ZSDT0037.
  DATA: CONTADOR TYPE SY-TABIX.

  C_TELA = '0101'.

  WHILE CONTADOR NE 5.
    WA_0037-BUKRS = V_BUKRS-LOW.
    WA_0037-WAERS = 'BRL'.
    APPEND WA_0037 TO IT_0037.
    ADD 1 TO CONTADOR.
  ENDWHILE.

  IT_SAIDA = IT_0037.

  LOOP AT IT_FCAT ASSIGNING <WA_FCAT>.
    CASE <WA_FCAT>-FIELDNAME.
      WHEN 'BUKRS' OR 'WAERS'.
        <WA_FCAT>-EDIT = ABAP_FALSE.
      WHEN OTHERS.
        <WA_FCAT>-EDIT = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

  CALL METHOD WA_ALV_NEW->SET_FRONTEND_FIELDCATALOG( IT_FIELDCATALOG = IT_FCAT ).

  CALL METHOD WA_ALV_NEW->REFRESH_TABLE_DISPLAY.

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
      TL_UCOMM = 'HIST'. APPEND TL_UCOMM. CLEAR TL_UCOMM.
    WHEN 'HIST'.
      TL_UCOMM = 'ADD'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'EDIT'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'DEL'.  APPEND TL_UCOMM. CLEAR TL_UCOMM.
      TL_UCOMM = 'COPY'. APPEND TL_UCOMM. CLEAR TL_UCOMM.
    WHEN 'EDIT'.
      TL_UCOMM = 'HIST'. APPEND TL_UCOMM. CLEAR TL_UCOMM.
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
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0103 OUTPUT.

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_KURSK'.

        IF SY-UCOMM EQ 'EDIT'.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.

        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_DADOS_HISTORICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AGRUPA_DADOS_HISTORICO.

  DATA: LS_0117 TYPE TY_0117.
  FREE: IT_FCAT_H, WA_FCAT_H, LT_0117.

  LOOP AT IT_0117 INTO DATA(LW_0117).

    MOVE-CORRESPONDING LW_0117 TO LS_0117.

    CASE LW_0117-DESATIVADO.
      WHEN ABAP_TRUE. MOVE 'Inativo' TO LS_0117-STATUS.
      WHEN ABAP_FALSE. MOVE 'Ativo' TO LS_0117-STATUS.
    ENDCASE.

    APPEND LS_0117 TO LT_0117.
    SORT LT_0117 BY SEQ DESCENDING.

  ENDLOOP.

  DEFINE HISTORICO.

    WA_FCAT_H-HOTSPOT   = &1.
    WA_FCAT_H-REF_TABLE = &2.
    WA_FCAT_H-REF_FIELD = &3.
    WA_FCAT_H-TABNAME   = &4.
    WA_FCAT_H-FIELDNAME = &5.
    WA_FCAT_H-SCRTEXT_L = &6.
    WA_FCAT_H-SCRTEXT_M = &6.
    WA_FCAT_H-NO_ZERO   = &7.
    WA_FCAT_H-OUTPUTLEN = &8.
    WA_FCAT_H-JUST      = &9.

    APPEND WA_FCAT_H TO IT_FCAT_H.
    CLEAR WA_FCAT_H.

  END-OF-DEFINITION.


  HISTORICO:
    '' '' '' 'LT_0117' 'BUKRS'       'Empresa'            '' '07' 'C',
    '' '' '' 'LT_0117' 'KURSK'       'Taxa Cambio'        '' '12' 'C',
    '' '' '' 'LT_0117' 'STATUS'      'Status'             '' '06' 'C',
    '' '' '' 'LT_0117' 'USNAM'       'Usuário Cadastrado' '' '15' 'C',
    '' '' '' 'LT_0117' 'DATA_ATUAL'  'Data Cadastro'      '' '17' 'C',
    '' '' '' 'LT_0117' 'HORA_ATUAL'  'Hora Cadastro'      '' '17' 'C'.

ENDFORM.
