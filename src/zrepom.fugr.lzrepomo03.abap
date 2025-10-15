*----------------------------------------------------------------------*
***INCLUDE LZREPOMO03.
*----------------------------------------------------------------------*

DATA: IT_SELECTED_ROWS_0102 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_0102 TYPE LVC_S_ROW.

CLASS LCL_EVENT_HANDLER_0102 DEFINITION.
  PUBLIC SECTION.
    "METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS LCL_ALV_TOOLBAR_0300 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR         IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION



DATA: EVENT_HANDLER_0102      TYPE REF TO LCL_EVENT_HANDLER_0102.

DATA: CTL_ALV_0102    TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0102    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0102     TYPE LVC_S_LAYO,
      GS_VAR_0102     TYPE DISVARIANT,
      IT_CATALOG_0102 TYPE LVC_T_FCAT,
      IT_EXCLUDE_0102 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0102 LIKE LINE OF IT_EXCLUDE_0102.

DATA: CTL_CON_0300       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_0300       TYPE REF TO CL_GUI_ALV_GRID,
      C_ALV_TOOLBAR_0300 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      GS_LAY_0300        TYPE LVC_S_LAYO,
      GS_VAR_0300        TYPE DISVARIANT,
      GS_SCROLL_COL_0300 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0300 TYPE LVC_S_ROID,
      IT_CATALOG_0300    TYPE LVC_T_FCAT,
      OBG_TOOLBAR_0300   TYPE REF TO LCL_ALV_TOOLBAR_0300,
      IT_EXCLUDE_0300    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0300    LIKE LINE OF IT_EXCLUDE_0300,
      IT_SELECTED_0300   TYPE LVC_T_ROW,
      WA_SELECTED_0300   TYPE LVC_S_ROW.

CLASS LCL_EVENT_HANDLER_0102 IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK_0102 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0300 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR_0300 IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBAR_0300
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: TY_TOOLBAR   TYPE STB_BUTTON.
*    "Separador
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    "Marcar Todos os Documentos
    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = 'ADD'.
    TY_TOOLBAR-QUICKINFO = TEXT-005.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF CK_CONSULTA EQ ABAP_TRUE.
      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    "Marcar Todos os Documentos
    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = 'DEL'.
    TY_TOOLBAR-QUICKINFO = TEXT-006.
    TY_TOOLBAR-BUTN_TYPE = 0.
    IF CK_CONSULTA EQ ABAP_TRUE.
      TY_TOOLBAR-DISABLED  = ABAP_TRUE.
    ENDIF.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBAR_0300->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CLEAR: IT_SELECTED_0300, IT_SELECTED_0300[].

    CALL METHOD CTL_ALV_0300->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_0300.

    CLEAR: IT_ZLEST0124_SEL[].

    LOOP AT IT_SELECTED_0300 INTO WA_SELECTED_0300.
      READ TABLE IT_ZLEST0124_ALV INTO IT_ZLEST0124_ALV INDEX WA_SELECTED_0300-INDEX.
      APPEND IT_ZLEST0124_ALV TO IT_ZLEST0124_SEL.
    ENDLOOP.

    CASE E_UCOMM.
      WHEN 'ADD'.
        PERFORM INCLUIR_DOCUMENTO.
      WHEN 'DEL'.
        PERFORM DELETAR_DOCUMENTO.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

DATA: GS_SCROLL_COL_0102 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0102 TYPE LVC_S_ROID.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'TL0102'.

  IF CTL_CON_0102 IS INITIAL.

    CREATE OBJECT CTL_CON_0102
      EXPORTING
        CONTAINER_NAME = 'ALV_0102'.

    CREATE OBJECT CTL_ALV_0102
      EXPORTING
        I_PARENT = CTL_CON_0102.

    PERFORM FILL_IT_FIELDCATALOG_0102.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0102.
*   Set layout parameters for ALV grid

    GS_LAY_0102-SEL_MODE = 'A'.
    GS_LAY_0102-ZEBRA    = ABAP_TRUE.

    CALL METHOD CTL_ALV_0102->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0102
        IS_VARIANT           = GS_VAR_0102
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0102
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0102
        IT_OUTTAB            = IT_ZLEST0122_ALV[].

    CALL METHOD CTL_ALV_0102->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT EVENT_HANDLER_0102.
    SET HANDLER EVENT_HANDLER_0102->HANDLE_DOUBLE_CLICK FOR CTL_ALV_0102.

  ELSE.
    CALL METHOD CTL_ALV_0102->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0102->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0102
      ES_ROW_NO   = GS_SCROLL_ROW_0102.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102_EXIT INPUT.
  PERFORM SAIR_0102.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.

      IF IT_ZLEST0122_SEL[] IS INITIAL.
        MESSAGE S016(ZREPOM).
        RETURN.
      ENDIF.

      MOVE IT_ZLEST0122_SEL TO IT_ZLEST0122_ALV.
      CK_INFORMADO = ABAP_TRUE.

      PERFORM SAIR_0102.
      LEAVE TO SCREEN 0.

      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_0102 INPUT.

  CALL METHOD CTL_ALV_0102->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0102
      ES_ROW_NO   = GS_SCROLL_ROW_0102.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_0102  USING P_ROW TYPE LVC_S_ROW.

  DATA: LC_ROW           TYPE LVC_T_ROW,
        WA_ZLEST0122_ALV TYPE ZDE_ZLEST0122_ALV.

  IF P_ROW-ROWTYPE IS INITIAL.

    APPEND P_ROW TO LC_ROW.

    CALL METHOD CTL_ALV_0102->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_ZLEST0122_ALV INDEX P_ROW-INDEX INTO IT_ZLEST0122_ALV.
    IF SY-SUBRC IS INITIAL.
      CK_INFORMADO = ABAP_TRUE.
      PERFORM SAIR_0102.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_0102 INPUT.

  CLEAR IT_SELECTED_ROWS_0102.

  CALL METHOD CTL_ALV_0102->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_0102.

  CLEAR IT_ZLEST0122_SEL[].

  LOOP AT IT_SELECTED_ROWS_0102 INTO WA_SELECTED_ROWS_0102.
    READ TABLE IT_ZLEST0122_ALV INTO IT_ZLEST0122_ALV INDEX WA_SELECTED_ROWS_0102-INDEX.
    MOVE-CORRESPONDING IT_ZLEST0122_ALV TO IT_ZLEST0122_SEL.
    APPEND IT_ZLEST0122_SEL.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0102.

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0102> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0122_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0102.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0102 ASSIGNING <FS_CAT_0102>.
    <FS_CAT_0102>-COL_POS = LC_COL_POS.
    <FS_CAT_0102>-TABNAME = 'IT_ZLEST0122_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_0102>-FIELDNAME.
      WHEN 'ID_ROTA'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'ID_ROTA_REPOM'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'ID_PERCURSO_REPOM'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'DS_PERCURSO_REPOM'.
        <FS_CAT_0102>-OUTPUTLEN = 40.
      WHEN 'CD_PAIS'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'CD_CID_ORIGEM'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'DS_CID_ORIGEM'.
        <FS_CAT_0102>-OUTPUTLEN = 30.
      WHEN 'CD_CID_DESTINO'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
      WHEN 'DS_CID_DESTINO'.
        <FS_CAT_0102>-OUTPUTLEN = 30.
      WHEN 'NR_KM_IDA'.
        "<FS_CAT_0102>-NO_OUT = ABAP_TRUE.
        <FS_CAT_0102>-OUTPUTLEN = 10.
      WHEN 'NR_KM_VOLTA'.
        "<FS_CAT_0102>-NO_OUT = ABAP_TRUE.
        <FS_CAT_0102>-OUTPUTLEN = 10.
      WHEN 'TP_PROC_TRANSP'.
        <FS_CAT_0102>-NO_OUT = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0102

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0102 .

  GS_VAR_0102-REPORT      = SY-REPID.
  GS_VAR_0102-HANDLE      = '0102'.
  GS_VAR_0102-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0102-USERNAME    = ABAP_FALSE.
  GS_VAR_0102-VARIANT     = ABAP_FALSE.
  GS_VAR_0102-TEXT        = ABAP_FALSE.
  GS_VAR_0102-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0102

*&---------------------------------------------------------------------*
*&      Form  CADASTRO_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CADASTRO_PEDAGIO .

  DATA: IT_ZLEST0123_T TYPE ZDE_ZLEST0123_T,
        IT_ZLEST0124_T TYPE ZDE_ZLEST0124_T.

  CLEAR: IT_ZLEST0123[], IT_ZLEST0123, WA_ZLEST0123, IT_ZLEST0123_T, IT_ZLEST0124[], IT_ZLEST0124_ALV[], IT_ZLEST0124, IT_ZLEST0124_ALV.

  CK_NOVO_PEDAGIO = ABAP_FALSE.

  OBJ_PEDAGIO->GET_REGISTRO( IMPORTING E_REGISTRO = WA_ZLEST0123 ).

  IF WA_ZLEST0123-ID_PROC_CLIENTE IS INITIAL.
    CK_NOVO_PEDAGIO = ABAP_TRUE.
  ENDIF.

  CLEAR: ZDE_ZLEST0123_ALV.

  MOVE-CORRESPONDING WA_ZLEST0123 TO ZDE_ZLEST0123_ALV.

  CALL METHOD OBJ_PEDAGIO->GET_DOCUMENTOS
    RECEIVING
      I_DOCUMENTOS = IT_ZLEST0124_T.

  LOOP AT IT_ZLEST0124_T INTO WA_ZLEST0124.
    CLEAR: IT_ZLEST0124_ALV.
    MOVE-CORRESPONDING WA_ZLEST0124 TO IT_ZLEST0124_ALV.
    APPEND IT_ZLEST0124_ALV.
  ENDLOOP.

  CALL SCREEN 0300 STARTING AT 50 01.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.

  DATA: P_CAMPO TYPE NAME_FELD,
        STR1    TYPE STRING,
        STR2    TYPE STRING.

  CLEAR: IT_UCOMM.

  IF CK_CONSULTA EQ ABAP_TRUE.
    APPEND OK_GRAVAR TO IT_UCOMM.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING IT_UCOMM.

  IF CK_CONSULTA = ABAP_TRUE.
    SET TITLEBAR 'TL0300' WITH TEXT-001.
  ELSE.
    CASE CK_NOVA_ROTA.
      WHEN ABAP_TRUE.
        SET TITLEBAR 'TL0300' WITH TEXT-002.
      WHEN ABAP_FALSE.
        SET TITLEBAR 'TL0300' WITH TEXT-003.
    ENDCASE.
  ENDIF.

  CLEAR: ZDE_ZLEST0123_ALV-BUTXT,
         ZDE_ZLEST0123_ALV-NAME,
         ZDE_ZLEST0123_ALV-DS_CID_ORIGEM,
         ZDE_ZLEST0123_ALV-DS_CID_DESTINO.

  IF ZDE_ZLEST0123_ALV-BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT INTO ZDE_ZLEST0123_ALV-BUTXT FROM T001 WHERE BUKRS EQ ZDE_ZLEST0123_ALV-BUKRS.

    IF ZDE_ZLEST0123_ALV-BRANCH IS NOT INITIAL.
      SELECT SINGLE NAME INTO ZDE_ZLEST0123_ALV-NAME FROM J_1BBRANCH WHERE BUKRS EQ ZDE_ZLEST0123_ALV-BUKRS AND BRANCH EQ ZDE_ZLEST0123_ALV-BRANCH.
    ENDIF.
  ENDIF.

  IF ZDE_ZLEST0123_ALV-CD_CID_ORIGEM IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0123_ALV-DS_CID_ORIGEM
      FROM J_1BTXJURT
     WHERE SPRAS       EQ SY-LANGU
       AND COUNTRY     EQ ZDE_ZLEST0123_ALV-CD_PAIS
       AND TAXJURCODE  EQ ZDE_ZLEST0123_ALV-CD_CID_ORIGEM.
  ENDIF.

  IF ZDE_ZLEST0123_ALV-CD_CID_DESTINO IS NOT INITIAL.
    SELECT SINGLE TEXT INTO ZDE_ZLEST0123_ALV-DS_CID_DESTINO
      FROM J_1BTXJURT
     WHERE SPRAS       EQ SY-LANGU
       AND COUNTRY     EQ ZDE_ZLEST0123_ALV-CD_PAIS
       AND TAXJURCODE  EQ ZDE_ZLEST0123_ALV-CD_CID_DESTINO.
  ENDIF.


  IF CK_ALTEROU_0300_MOTO EQ ABAP_TRUE.
    CALL METHOD OBJ_PEDAGIO->SET_MOTORISTA_COD( EXPORTING I_MOTORISTA_COD = ZDE_ZLEST0123_ALV-MOTORISTA_COD ).
    ZDE_ZLEST0123_ALV-MOTORISTA_CPF  = OBJ_PEDAGIO->GET_MOTORISTA_CPF( ).
    ZDE_ZLEST0123_ALV-MOTORISTA_NOME = OBJ_PEDAGIO->GET_MOTORISTA_NOME( ).
    ZDE_ZLEST0123_ALV-MOTORISTA_RG   = OBJ_PEDAGIO->GET_MOTORISTA_RG( ).
    ZDE_ZLEST0123_ALV-MOTORISTA_FONE = OBJ_PEDAGIO->GET_MOTORISTA_FONE( ).
    CK_ALTEROU_0300_MOTO = ABAP_FALSE.
  ENDIF.

  IF CK_ALTEROU_0300_PLC EQ ABAP_TRUE.
    CALL METHOD OBJ_PEDAGIO->SET_VEICULO_PLACA( EXPORTING I_VEICULO_PLACA = ZDE_ZLEST0123_ALV-VEICULO_PLACA ).
    ZDE_ZLEST0123_ALV-VEICULO_EIXOS  = OBJ_PEDAGIO->GET_VEICULO_EIXOS( ).
    CK_ALTEROU_0300_PLC = ABAP_FALSE.
  ENDIF.

  IF CK_ALTEROU_0300_PL1 EQ ABAP_TRUE.
    CALL METHOD OBJ_PEDAGIO->SET_VEICULO_PLACA_C1( EXPORTING I_VEICULO_PLACA_C1 = ZDE_ZLEST0123_ALV-VEICULO_PLACA_C1 ).
    ZDE_ZLEST0123_ALV-VEICULO_EIXOS_C1 = OBJ_PEDAGIO->GET_VEICULO_EIXOS_C1( ).
    CK_ALTEROU_0300_PL1 = ABAP_FALSE.
  ENDIF.

  IF CK_ALTEROU_0300_PL2 EQ ABAP_TRUE.
    CALL METHOD OBJ_PEDAGIO->SET_VEICULO_PLACA_C2( EXPORTING I_VEICULO_PLACA_C2 = ZDE_ZLEST0123_ALV-VEICULO_PLACA_C2 ).
    ZDE_ZLEST0123_ALV-VEICULO_EIXOS_C2 = OBJ_PEDAGIO->GET_VEICULO_EIXOS_C2( ).
    CK_ALTEROU_0300_PL2 = ABAP_FALSE.
  ENDIF.

  IF CK_ALTEROU_0300_PL3 EQ ABAP_TRUE.
    CALL METHOD OBJ_PEDAGIO->SET_VEICULO_PLACA_C3( EXPORTING I_VEICULO_PLACA_C3 = ZDE_ZLEST0123_ALV-VEICULO_PLACA_C3 ).
    ZDE_ZLEST0123_ALV-VEICULO_EIXOS_C3 = OBJ_PEDAGIO->GET_VEICULO_EIXOS_C3( ).
    CK_ALTEROU_0300_PL3 = ABAP_FALSE.
  ENDIF.

  IF CK_ALTEROU_0300_PER EQ ABAP_TRUE AND ZDE_ZLEST0123_ALV-DS_PERCURSO_REPOM IS INITIAL.
    SELECT SINGLE DS_PERCURSO_REPOM INTO ZDE_ZLEST0123_ALV-DS_PERCURSO_REPOM
      FROM ZLEST0122
     WHERE ID_ROTA           EQ ZDE_ZLEST0123_ALV-ID_ROTA
       AND ID_ROTA_REPOM     EQ ZDE_ZLEST0123_ALV-ID_ROTA_REPOM
       AND ID_PERCURSO_REPOM EQ ZDE_ZLEST0123_ALV-ID_PERCURSO_REPOM.
    CK_ALTEROU_0300_PER = ABAP_FALSE.
  ENDIF.

  IF CK_CONSULTA EQ ABAP_TRUE.
    LOOP AT SCREEN .
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME(17) EQ 'ZDE_ZLEST0123_ALV'.
        SPLIT SCREEN-NAME AT '-' INTO STR1 STR2.
        P_CAMPO = STR2.
        IF P_CAMPO = 'DS_NR_CARTAO' OR P_CAMPO = 'DS_PERCURSO_REPOM' OR P_CAMPO = 'BUTXT' OR P_CAMPO = 'NAME' OR P_CAMPO = 'DS_CID_ORIGEM' OR P_CAMPO = 'DS_CID_DESTINO'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
        IF OBJ_PEDAGIO->VALIDA_ATRIBUTO_ALTERAVEL( EXPORTING I_CAMPO = P_CAMPO ) EQ ABAP_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'BTN_CARTAO'.
        P_CAMPO = 'DS_NR_CARTAO'.
        IF OBJ_PEDAGIO->VALIDA_ATRIBUTO_ALTERAVEL( EXPORTING I_CAMPO = P_CAMPO ) EQ ABAP_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'BTN_PERCURSO'.
        P_CAMPO = 'ID_ROTA'.
        IF OBJ_PEDAGIO->VALIDA_ATRIBUTO_ALTERAVEL( EXPORTING I_CAMPO = P_CAMPO ) EQ ABAP_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.

  CK_ALTEROU_0300 = ABAP_FALSE.

  "'ALV_DOCUMENTOS'
  IF CTL_CON_0300 IS INITIAL.

    CREATE OBJECT CTL_CON_0300
      EXPORTING
        CONTAINER_NAME = 'ALV_DOCUMENTOS'.

    CREATE OBJECT CTL_ALV_0300
      EXPORTING
        I_PARENT = CTL_CON_0300.

    CREATE OBJECT OBG_TOOLBAR_0300
      EXPORTING
        IO_ALV_GRID = CTL_ALV_0300.

    SET HANDLER OBG_TOOLBAR_0300->ON_TOOLBAR FOR CTL_ALV_0300.
    SET HANDLER OBG_TOOLBAR_0300->HANDLE_USER_COMMAND FOR CTL_ALV_0300.

    PERFORM FILL_IT_FIELDCATALOG_0300.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0300.
*   Set layout parameters for ALV grid

    GS_LAY_0300-SEL_MODE   = 'A'.
    GS_LAY_0300-ZEBRA      = ABAP_TRUE.
    GS_LAY_0300-GRID_TITLE = TEXT-006.

    CALL METHOD CTL_ALV_0300->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0300
        IS_VARIANT           = GS_VAR_0300
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0300
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0300
        IT_OUTTAB            = IT_ZLEST0124_ALV[].

    CALL METHOD CTL_ALV_0300->REFRESH_TABLE_DISPLAY.

  ELSE.
    CALL METHOD CTL_ALV_0300->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0300->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0300
      ES_ROW_NO   = GS_SCROLL_ROW_0300.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PEDAGIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_PEDAGIO INPUT.
  CK_ALTEROU_0300 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  DATA: I_CARTAO     TYPE ZDE_REPOM_CARTAO,
        CK_VALIDADO  TYPE CHAR01,
        E_PERCURSO   TYPE ZLEST0122,
        I_DOCUMENTOS TYPE ZDE_ZLEST0124_T.

  CHECK CK_ALTEROU_0300     EQ ABAP_FALSE.
  CHECK CK_ALTEROU_0300_PLC EQ ABAP_FALSE.
  CHECK CK_ALTEROU_0300_PL1 EQ ABAP_FALSE.
  CHECK CK_ALTEROU_0300_PL2 EQ ABAP_FALSE.
  CHECK CK_ALTEROU_0300_PL3 EQ ABAP_FALSE.
  CHECK CK_ALTEROU_0300_MOTO NE ABAP_TRUE.

  CASE OK_CODE.
    WHEN OK_GRAVAR.

      CLEAR: I_DOCUMENTOS.
      LOOP AT IT_ZLEST0124_ALV.
        MOVE-CORRESPONDING IT_ZLEST0124_ALV TO WA_ZLEST0124.
        APPEND WA_ZLEST0124 TO I_DOCUMENTOS.
      ENDLOOP.

      OBJ_PEDAGIO->SET_BUKRS( EXPORTING I_BUKRS = ZDE_ZLEST0123_ALV-BUKRS ).
      OBJ_PEDAGIO->SET_BRANCH( EXPORTING I_BRANCH = ZDE_ZLEST0123_ALV-BRANCH ).
      OBJ_PEDAGIO->SET_CD_CID_ORIGEM( EXPORTING I_CD_CID_ORIGEM = ZDE_ZLEST0123_ALV-CD_CID_ORIGEM ).
      OBJ_PEDAGIO->SET_CD_CID_DESTINO( EXPORTING I_CD_CID_DESTINO = ZDE_ZLEST0123_ALV-CD_CID_DESTINO ).
      OBJ_PEDAGIO->SET_MOEDA_PEDAGIO( EXPORTING I_MOEDA_PEDAGIO = ZDE_ZLEST0123_ALV-MOEDA_PEDAGIO ).
      OBJ_PEDAGIO->SET_DOCUMENTOS( EXPORTING I_DOCUMENTOS = I_DOCUMENTOS ).

      IF OBJ_PEDAGIO->GRAVAR( ) EQ ABAP_TRUE.
        CK_GRAVADO  = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
      CLEAR OK_CODE.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_MOTORISTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_MOTORISTA INPUT.
  CK_ALTEROU_0300_MOTO = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VEICULO_PLACA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_VEICULO_PLACA INPUT.
  CK_ALTEROU_0300_PLC = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VEICULO_PLACA_C1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_VEICULO_PLACA_C1 INPUT.
  CK_ALTEROU_0300_PL1 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VEICULO_PLACA_C2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_VEICULO_PLACA_C2 INPUT.
  CK_ALTEROU_0300_PL2 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VEICULO_PLACA_C3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_VEICULO_PLACA_C3 INPUT.
  CK_ALTEROU_0300_PL3 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300_EXIT INPUT.

  CASE OK_CODE.
    WHEN OK_INFO_PERCURSO.

      IF ZDE_ZLEST0123_ALV-BUKRS IS INITIAL.
        MESSAGE I000(ZREPOM).
        RETURN.
      ENDIF.

      IF ZDE_ZLEST0123_ALV-BRANCH IS INITIAL.
        MESSAGE I001(ZREPOM).
        RETURN.
      ENDIF.

      IF ZDE_ZLEST0123_ALV-CD_CID_ORIGEM IS INITIAL.
        MESSAGE I003(ZREPOM).
        RETURN.
      ENDIF.

      IF ZDE_ZLEST0123_ALV-CD_CID_DESTINO IS INITIAL.
        MESSAGE I004(ZREPOM).
        RETURN.
      ENDIF.

      CALL FUNCTION 'Z_REPOM_INFORMA_PERCURSO'
        EXPORTING
          I_BRANCH         = ZDE_ZLEST0123_ALV-BRANCH
          I_BUKRS          = ZDE_ZLEST0123_ALV-BUKRS
          I_CD_CID_ORIGEM  = ZDE_ZLEST0123_ALV-CD_CID_ORIGEM
          I_CD_CID_DESTINO = ZDE_ZLEST0123_ALV-CD_CID_DESTINO
        IMPORTING
          E_INFORMADO      = CK_VALIDADO
          E_PERCURSO       = E_PERCURSO
        EXCEPTIONS
          SEM_PERCURSO     = 1
          OTHERS           = 2.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        IF CK_VALIDADO EQ ABAP_TRUE.
          CALL METHOD OBJ_PEDAGIO->SET_ID_ROTA_REPOM( EXPORTING I_ID_ROTA_REPOM = E_PERCURSO-ID_ROTA_REPOM ).
          CALL METHOD OBJ_PEDAGIO->SET_ID_PERCURSO_REPOM( EXPORTING I_ID_PERCURSO_REPOM = E_PERCURSO-ID_PERCURSO_REPOM ).
          CALL METHOD OBJ_PEDAGIO->SET_ID_ROTA( EXPORTING I_ID_ROTA = E_PERCURSO-ID_ROTA ).
          ZDE_ZLEST0123_ALV-ID_ROTA_REPOM     = E_PERCURSO-ID_ROTA_REPOM.
          ZDE_ZLEST0123_ALV-ID_PERCURSO_REPOM = E_PERCURSO-ID_PERCURSO_REPOM.
          ZDE_ZLEST0123_ALV-ID_ROTA           = E_PERCURSO-ID_ROTA.
          ZDE_ZLEST0123_ALV-DS_PERCURSO_REPOM = E_PERCURSO-DS_PERCURSO_REPOM.
        ENDIF.
      ENDIF.

    WHEN OK_INFO_CARTAO.

      IF ZDE_ZLEST0123_ALV-BUKRS IS INITIAL.
        MESSAGE I000(ZREPOM).
        RETURN.
      ENDIF.

      IF ZDE_ZLEST0123_ALV-BRANCH IS INITIAL.
        MESSAGE I001(ZREPOM).
        RETURN.
      ENDIF.

      CLEAR: I_CARTAO.
      I_CARTAO-BUKRS  = ZDE_ZLEST0123_ALV-BUKRS.
      I_CARTAO-BRANCH = ZDE_ZLEST0123_ALV-BRANCH.

      CALL FUNCTION 'Z_REPOM_INFORMA_CARTAO_PED'
        IMPORTING
          CK_VALIDADO = CK_VALIDADO
        CHANGING
          I_CARTAO    = I_CARTAO.

      IF CK_VALIDADO EQ ABAP_TRUE.
        ZDE_ZLEST0123_ALV-DS_NR_CARTAO = I_CARTAO-NR_CARTAO.
        CALL METHOD OBJ_PEDAGIO->SET_DS_NR_CARTAO( EXPORTING I_DS_NR_CARTAO = ZDE_ZLEST0123_ALV-DS_NR_CARTAO ).
      ENDIF.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0300 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0300> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZLEST0124'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0300.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0300 ASSIGNING <FS_CAT_0300>.
    <FS_CAT_0300>-COL_POS = LC_COL_POS.
    <FS_CAT_0300>-TABNAME = 'IT_ZLEST0124_ALV'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_0300>-FIELDNAME.
      WHEN 'ID_PROC_CLIENTE' OR 'MANDT' OR 'ID_FILIAL_CLIENTE'.
        <FS_CAT_0300>-NO_OUT = ABAP_TRUE.
      WHEN 'DS_MODELO_DOC'.
        <FS_CAT_0300>-OUTPUTLEN = 05.
      WHEN 'DS_NUMERO_DOC'.
        <FS_CAT_0300>-OUTPUTLEN = 15.
      WHEN 'DS_SERIE_DOC'.
        <FS_CAT_0300>-OUTPUTLEN = 05.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0300

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0300 .

  GS_VAR_0300-REPORT      = SY-REPID.
  GS_VAR_0300-HANDLE      = '0300'.
  GS_VAR_0300-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0300-USERNAME    = ABAP_FALSE.
  GS_VAR_0300-VARIANT     = ABAP_FALSE.
  GS_VAR_0300-TEXT        = ABAP_FALSE.
  GS_VAR_0300-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0300

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCLUIR_DOCUMENTO .

  CLEAR: ZLEST0124, CK_INFORMADO_DOC.

  ZLEST0124-DS_MODELO_DOC = '55'.

  CALL SCREEN 0301 STARTING AT 40 05.

  IF CK_INFORMADO_DOC EQ ABAP_TRUE.
    APPEND ZLEST0124 TO IT_ZLEST0124_ALV.
    PERFORM ATUALIZA_DOCUMENTO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETAR_DOCUMENTO .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0301_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0301 INPUT.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.
      CLEAR: OK_CODE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = ZLEST0124-DS_NUMERO_DOC
        IMPORTING
          OUTPUT = ZLEST0124-DS_NUMERO_DOC.

      CK_INFORMADO_DOC = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0301 OUTPUT.
  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TL0301'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_DOCUMENTO .

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_0300->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAIR_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIR_0102 .

  CLEAR: EVENT_HANDLER_0102.

  IF CTL_ALV_0102 IS NOT INITIAL.
    CTL_ALV_0102->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_0102.

  IF CTL_CON_0102 IS NOT INITIAL.
    CTL_CON_0102->FREE( ).
  ENDIF.
  CLEAR: CTL_CON_0102.

ENDFORM.
