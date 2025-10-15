**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Transportadoras para cotação de Frete – Montagem Carga Insumos|*
**/===========================================================================\*
*

REPORT ZSDR0089.

TYPES BEGIN OF TY_0163.
        INCLUDE TYPE ZSDT0163.
TYPES DESC_EMP TYPE CHAR50.
TYPES DESC_TRA TYPE CHAR50.
TYPES END OF TY_0163.

DATA: IT_0163 TYPE TABLE OF TY_0163,
      IT_SAVE TYPE TABLE OF ZSDT0163,
      _STR    TYPE REF TO DATA,
      _CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      _ALV    TYPE REF TO CL_GUI_ALV_GRID,
      _STABLE TYPE LVC_S_STBL,
      _FCAT   TYPE LVC_T_FCAT,
      _LAYOUT TYPE LVC_S_LAYO,
      _ADD    TYPE C,
      _DEL    TYPE C.

DATA: TG_MSG_RET      TYPE TABLE OF ZFIWRS0002,
      WG_MENSAGEM(30).

INITIALIZATION.


START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.

  CALL SCREEN 1000.


CLASS ZCL_EVENTS DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,

      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

ENDCLASS.

CLASS ZCL_EVENTS IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

    DATA: _BUKRS TYPE BUKRS,
          _LIFNR TYPE LIFNR.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(_WA)
      WHERE FIELDNAME EQ 'BUKRS' OR FIELDNAME EQ 'LIFNR'.

      LOOP AT IT_0163 ASSIGNING FIELD-SYMBOL(<_WA>).

        CHECK _WA-ROW_ID EQ SY-TABIX.

        CASE _WA-FIELDNAME.
          WHEN 'BUKRS'.

            _BUKRS = |{ _WA-VALUE ALPHA = IN }|.
            SELECT SINGLE BUTXT
              FROM T001
              INTO <_WA>-DESC_EMP
               WHERE BUKRS EQ _BUKRS.

            IF SY-SUBRC IS NOT INITIAL.
              <_WA>-BUKRS = ''.
              <_WA>-DESC_EMP = ''.
            ENDIF.

          WHEN 'LIFNR'.

            _LIFNR = |{ _WA-VALUE ALPHA = IN }|.
            SELECT SINGLE NAME1
              FROM LFA1
              INTO <_WA>-DESC_TRA
               WHERE LIFNR EQ _WA-VALUE
                 AND DLGRP EQ '0001'.

            IF SY-SUBRC IS NOT INITIAL.
              <_WA>-LIFNR = ''.
              <_WA>-DESC_TRA = ''.
            ENDIF.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED.
    _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).
    PERFORM VERIFICA_DADOS.
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR 'TI1000'.

  ASSIGN 'TY_0163' TO FIELD-SYMBOL(<FS_STR>).
  CREATE DATA _STR TYPE (<FS_STR>).

  _FCAT = CORRESPONDING LVC_T_FCAT( CL_SALV_DATA_DESCR=>READ_STRUCTDESCR( CAST CL_ABAP_STRUCTDESCR( CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( _STR ) ) ) ).

  LOOP AT _FCAT ASSIGNING FIELD-SYMBOL(<FCAT>).

    CASE <FCAT>-FIELDNAME.
      WHEN 'DESC_EMP'.
        <FCAT>-SCRTEXT_L = 'Descrição Empresa'.
        <FCAT>-OUTPUTLEN = 30.
        <FCAT>-COL_POS = 2.
      WHEN 'DESC_TRA'.
        <FCAT>-SCRTEXT_L = 'Descrição Transportadora'.
        <FCAT>-OUTPUTLEN = 30.
        <FCAT>-COL_POS = 4.
      WHEN 'BUKRS'.
        <FCAT>-EDIT = COND #( WHEN _ADD IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE ).
        <FCAT>-SCRTEXT_L = 'Empresa'.
        <FCAT>-OUTPUTLEN = 6.
        <FCAT>-COL_POS = 1.
      WHEN 'LIFNR'.
        <FCAT>-EDIT = COND #( WHEN _ADD IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE ).
        <FCAT>-SCRTEXT_L = 'Cod. Transportadora'.
        <FCAT>-OUTPUTLEN = 10.
        <FCAT>-COL_POS = 3.
      WHEN OTHERS.
        <FCAT>-NO_OUT = ABAP_TRUE.
    ENDCASE.

    <FCAT>-REPTEXT = <FCAT>-SCRTEXT_S = <FCAT>-SCRTEXT_M = <FCAT>-SCRTEXT_L.

  ENDLOOP.

  _LAYOUT-ZEBRA      = ABAP_TRUE.
  _LAYOUT-NO_ROWINS  = ABAP_TRUE.
  _STABLE            = ABAP_TRUE.

  IF _CONT IS INITIAL.

    CREATE OBJECT _CONT
      EXPORTING
        CONTAINER_NAME = 'CC_'.

    CREATE OBJECT _ALV
      EXPORTING
        I_PARENT = _CONT.

    PERFORM SELECIONA_DADOS.


    _ALV->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT       = _LAYOUT
        I_SAVE          = 'X'
      CHANGING
        IT_OUTTAB       = IT_0163
        IT_FIELDCATALOG = _FCAT ).

    _ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
    _ALV->REGISTER_EDIT_EVENT( I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).

    SET HANDLER: ZCL_EVENTS=>ON_DATA_CHANGED          FOR _ALV,
                 ZCL_EVENTS=>ON_DATA_CHANGED_FINISHED FOR _ALV.

  ELSE.
    _ALV->SET_FRONTEND_FIELDCATALOG( EXPORTING IT_FIELDCATALOG = _FCAT ).
    _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).
  ENDIF.

  PERFORM VERIFICA_DADOS.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      PERFORM CANCELAR.
      "LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM SAVE_DADOS.
    WHEN 'ADD'.
      PERFORM ADD_DADOS.
    WHEN 'DEL'.
      PERFORM DEL_DADOS.
    WHEN 'SHOW_MSGRE'.

      PERFORM VERIFICA_DADOS.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = 'X'
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .
  SELECT A~MANDT A~BUKRS A~LIFNR A~USNAM A~DATA_ATUAL A~HORA_ATUAL A~ESTORNO A~USNAM_E A~DATA_ATUAL_E A~HORA_ATUAL_E B~BUTXT C~NAME1
    FROM ZSDT0163 AS A
    INNER JOIN T001 AS B ON B~BUKRS EQ A~BUKRS
    INNER JOIN LFA1 AS C ON C~LIFNR EQ A~LIFNR
    INTO TABLE IT_0163
    WHERE A~ESTORNO EQ ABAP_FALSE
      AND C~DLGRP EQ '0001'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_DADOS.

  DATA: CONT TYPE SY-TABIX.

  _ADD = ABAP_TRUE.

  "FREE IT_0163.

  "DO.
  " IF CONT EQ 10.
  "   EXIT.
  " ENDIF.
  APPEND VALUE #( ) TO IT_0163.
  "ADD 1 TO CONT.
  " ENDDO.

  "CONT = 0.

  _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEL_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_DADOS.

  DATA: _ROWS TYPE LVC_T_ROW.

  CALL METHOD _ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = _ROWS.

  CHECK NOT _ROWS IS INITIAL.

  LOOP AT _ROWS INTO DATA(LINE).
    LOOP AT IT_0163 ASSIGNING FIELD-SYMBOL(<WA>).
      CHECK LINE-INDEX EQ SY-TABIX.
      <WA>-ESTORNO = ABAP_TRUE.
      <WA>-USNAM_E = SY-UNAME.
      <WA>-DATA_ATUAL_E = SY-DATUM.
      <WA>-HORA_ATUAL_E = SY-UZEIT.
      APPEND <WA> TO IT_SAVE.
    ENDLOOP.
  ENDLOOP.

  DELETE IT_0163 WHERE ESTORNO EQ ABAP_TRUE.

  _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).

ENDFORM.


FORM CANCELAR.

  _ADD = ABAP_FALSE.
  _DEL = ABAP_FALSE.

  FREE: IT_0163.
  PERFORM SELECIONA_DADOS.

  _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAVE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DADOS.

  CHECK TG_MSG_RET IS INITIAL.

  CASE ABAP_TRUE.
    WHEN _ADD.
      IT_SAVE = VALUE #(
        FOR LS IN IT_0163
                  WHERE ( BUKRS IS NOT INITIAL
                      AND LIFNR IS NOT INITIAL )
                    (
                        BUKRS      = |{ LS-BUKRS ALPHA = IN }|
                        LIFNR      = |{ LS-LIFNR ALPHA = IN }|
                        USNAM      = SY-UNAME
                        DATA_ATUAL = SY-DATUM
                        HORA_ATUAL = SY-UZEIT
                     )
                    ).
  ENDCASE.

  MODIFY ZSDT0163 FROM TABLE IT_SAVE.
  IF SY-SUBRC IS INITIAL.
    COMMIT WORK.
  ENDIF.

  FREE: IT_SAVE, IT_0163.
  _ADD = ABAP_FALSE.
  _DEL = ABAP_FALSE.
  PERFORM SELECIONA_DADOS.

  _ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).

  MESSAGE S836(SD) WITH 'Transportadoras cadastradas'
                         'com sucesso!'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_DADOS .

  FREE: TG_MSG_RET.

  LOOP AT IT_0163 INTO DATA(_WA).

    CHECK _WA-BUKRS IS NOT INITIAL AND _WA-LIFNR IS NOT INITIAL.

    _WA-BUKRS = |{ _WA-BUKRS ALPHA = IN }|.
    SELECT COUNT(*)
    FROM T001
    WHERE BUKRS EQ _WA-BUKRS.

    IF SY-SUBRC IS NOT INITIAL.
      APPEND VALUE #(
                      FIELD = 'BUKRS'
                      MSG   = |Empresa { _WA-BUKRS ALPHA = IN } não existe!|
                    ) TO TG_MSG_RET.
    ENDIF.

    _WA-LIFNR = |{ _WA-LIFNR ALPHA = IN }|.
    SELECT COUNT(*)
      FROM LFA1
       WHERE LIFNR EQ _WA-LIFNR
         AND DLGRP EQ '0001'.

    IF SY-SUBRC IS NOT INITIAL.
      APPEND VALUE #(
                      FIELD = 'BUKRS'
                      MSG   = |Transportadora { _WA-LIFNR ALPHA = OUT } não existe!|
                    ) TO TG_MSG_RET.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
      I_REPID       = SY-REPID
      I_POPUP       = 0
      I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
      I_SET_CELL    = 'WG_CELL'
      I_SET_OBJ     = 'WG_OBJ'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

ENDFORM.
