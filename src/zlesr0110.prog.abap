*&---------------------------------------------------------------------*
*& Report  ZLESR0110
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0110 MESSAGE-ID ZAVSEGURO.

TABLES: ZLEST0150, ZLEST0162, ZLEST0163.

DATA: CAD_AVERBACAO TYPE REF TO ZCL_AVERBACAO_SEGURO,
      CAD_TOKEN     TYPE REF TO ZCL_AVERBACAO_SEGURO_TOKEN.

*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS C_SERVICE DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS GET_PIC_TAB               IMPORTING MIME_URL TYPE CSEQUENCE EXPORTING PIC_TAB  TYPE STANDARD TABLE.
ENDCLASS.                    "c_service DEFINITION

CLASS LCL_DRAGDROP_OBJ_TREE DEFINITION.
  PUBLIC SECTION.
    DATA: NODE  TYPE TV_NODEKEY.
ENDCLASS.                    "lcl_dragdrop_obj_d0100 DEFINITION

CLASS LCL_DRAGDROP_OBJ_TREE_MULT DEFINITION.
  PUBLIC SECTION.
    DATA: NODE TYPE TABLE OF TV_NODEKEY,
          TIPO TYPE CHAR01.
ENDCLASS.                    "lcl_dragdrop_obj_d0100 DEFINITION

CONSTANTS: CS_LINE_COLOR_FINALIZADO TYPE C LENGTH 4 VALUE 'C500',
           CS_LINE_COLOR_ALTERADO   TYPE C LENGTH 4 VALUE 'C300'.

DATA: CTL_CCCONTAINER_PICTURE TYPE REF TO CL_GUI_CONTAINER,
      SPLITTER                TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      PICTURE                 TYPE REF TO CL_GUI_PICTURE,
      REPOSITORY              TYPE REF TO ZCL_REPOSITORY_CLASSES.

DATA: OK_CODE TYPE SY-UCOMM.

DATA: DOCKING       TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      TREE          TYPE REF TO CL_GUI_COLUMN_TREE,
      TREE_DCLS     TYPE REF TO CL_GUI_COLUMN_TREE,
      G_DROPEFFECT  TYPE I,
      DRAGDROP_TREE TYPE REF TO CL_DRAGDROP,
      G_HANDLE_TREE TYPE I,
      G_HANDLE_ALV  TYPE I,
      DRAGDROP_ALV  TYPE REF TO CL_DRAGDROP.

DATA: DG_SPLITTER     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_DOC TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      WA_STABLE       TYPE LVC_S_STBL,
      GS_SCROLL_COL   TYPE LVC_S_COL,
      GS_SCROLL_ROW   TYPE LVC_S_ROID,
      SB_TELA_0200    TYPE SY-DYNNR.

DATA: IT_FIELDCATALOG TYPE LVC_T_FCAT,
      WA_FIELDCATALOG TYPE LVC_S_FCAT.

DATA: IT_EXCEPT_QINFO TYPE LVC_T_QINF,
      WA_EXCEPT_QINFO TYPE LVC_S_QINF.

DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
      WA_SELECTED_ROWS TYPE LVC_S_ROW.


DATA: GS_LAYOUT  TYPE LVC_S_LAYO,
      GS_VARIANT TYPE DISVARIANT.

DATA: NODE_TABLE TYPE TREEV_NTAB,
      ITEM_TABLE TYPE STANDARD TABLE OF MTREEITM.

DATA: NODE_TABLE_DOCS TYPE TREEV_NTAB,
      ITEM_TABLE_DOCS TYPE STANDARD TABLE OF MTREEITM.

"Informações do Conhecimento de Transporte

START-OF-SELECTION.

  CALL SCREEN 0001.

INITIALIZATION.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN ''.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  LEAVE PROGRAM.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.


  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF SPLITTER IS INITIAL.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_PICTURE.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = CTL_CCCONTAINER_PICTURE
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB USING PICTURE.

  ENDIF.

ENDMODULE.

##PERF_NO_TYPE
FORM LOAD_PIC_FROM_DB  USING  GUI_PICTURE TYPE REF TO CL_GUI_PICTURE.

  DATA URL(255).
  TYPES PIC_LINE(1022) TYPE X.
  DATA  PIC_TAB TYPE TABLE OF PIC_LINE.

  CLEAR URL.
  URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  C_SERVICE=>GET_PIC_TAB(
        EXPORTING MIME_URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
        IMPORTING PIC_TAB  = PIC_TAB ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE    = 'image'
      SUBTYPE = 'GIF'
    TABLES
      DATA    = PIC_TAB
    CHANGING
      URL     = URL
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD GUI_PICTURE->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL.

ENDFORM.                               " LOAD_PIC_FROM_DB

CLASS C_SERVICE IMPLEMENTATION.
  METHOD GET_PIC_TAB.
    DATA PIC_WA TYPE XSTRING.
    DATA LENGTH TYPE I.
    DATA MIME_API TYPE REF TO IF_MR_API.
    MIME_API = CL_MIME_REPOSITORY_API=>GET_API( ).
    MIME_API->GET( EXPORTING I_URL = MIME_URL
                   IMPORTING E_CONTENT = PIC_WA
                   EXCEPTIONS OTHERS = 4 ).
    IF SY-SUBRC = 4.
      RETURN.
    ENDIF.
    CLEAR PIC_TAB.
    LENGTH = XSTRLEN( PIC_WA ).
    WHILE LENGTH >= 1022.
      APPEND PIC_WA(1022) TO PIC_TAB.
      SHIFT PIC_WA BY 1022 PLACES LEFT IN BYTE MODE.
      LENGTH = XSTRLEN( PIC_WA ).
    ENDWHILE.
    IF LENGTH > 0.
      APPEND PIC_WA TO PIC_TAB.
    ENDIF.

  ENDMETHOD.                    "get_pic_tab

ENDCLASS.                    "c_service IMPLEMENTATION

FORM LIMPAR_TELA.

  IF DRAGDROP_ALV IS NOT INITIAL.
    DRAGDROP_ALV->DESTROY( ).
  ENDIF.

  CLEAR: DRAGDROP_ALV.

  IF DRAGDROP_TREE IS NOT INITIAL.
    DRAGDROP_TREE->DESTROY( ).
  ENDIF.

  CLEAR: DRAGDROP_TREE.

  IF TREE IS NOT INITIAL.
    TREE->FREE( ).
  ENDIF.
  CLEAR: TREE.

  IF DOCKING IS NOT INITIAL.
    DOCKING->FREE( ).
  ENDIF.

  CLEAR: DOCKING.
  IF REPOSITORY IS NOT INITIAL.
    REPOSITORY->FREE( ).
  ENDIF.

  CLEAR: REPOSITORY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.

  LEAVE PROGRAM.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

*023  Sem Acesso para Averbar Seguro! Objeto ZACSEGURO Campo ZAC_SEGURO=1
*024  Sem Acesso para Token! Objeto ZACSEGURO Campo ZAC_SEGURO=2
*025  Sem Acesso para WebService! Objeto ZACSEGURO Campo ZAC_SEGURO=3
*026  Sem Acesso para Diretório! Objeto ZACSEGURO Campo ZAC_SEGURO=4

  CASE OK_CODE.
    WHEN 'TOKEM'.
      CLEAR: OK_CODE.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '2'.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S024 DISPLAY LIKE 'E'.
      ELSE.
        SUBMIT ZLESR0110_TOKEN VIA SELECTION-SCREEN AND RETURN.
      ENDIF.
    WHEN 'WEBSERVICE'.
      CLEAR: OK_CODE.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '3'.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S025 DISPLAY LIKE 'E'.
      ELSE.
        SUBMIT ZLESR0110_URI VIA SELECTION-SCREEN AND RETURN.
      ENDIF.
    WHEN 'DIRXML'.
      CLEAR: OK_CODE.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '4'.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S026 DISPLAY LIKE 'E'.
      ELSE.
        SELECT SINGLE * INTO ZLEST0150 FROM ZLEST0150.
        CALL SCREEN 9001 STARTING AT 10 5.
      ENDIF.
    WHEN 'CADASTRO'.
      SUBMIT ZLESR0110_AVERBACAO VIA SELECTION-SCREEN AND RETURN.
    WHEN 'CONTINGENC'.
      CLEAR: OK_CODE.
      CALL SCREEN 8001 STARTING AT 05 05.
    WHEN 'FRETEAQUA'.
      SUBMIT ZLESR0110_AQUAV VIA SELECTION-SCREEN AND RETURN.
    WHEN 'FROTA'.
      SUBMIT ZLESR0110_FROTA_PROPRIA VIA SELECTION-SCREEN AND RETURN.
    WHEN 'USMAIL'.
      CLEAR: OK_CODE.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '1'.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S023 DISPLAY LIKE 'E'.
      ELSE.
        SUBMIT zregister_data WITH p_db_tab = 'ZLEST0224'
                      WITH p_stcnam = 'ZLEST0224'
                      WITH p_scmant = '0137'
                      WITH p_title  = 'Usuários p/ Envio de Email - Averbação Seguro'
                AND RETURN.
      ENDIF.

  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  CASE OK_CODE.
    WHEN 'SAVE'.
      MODIFY ZLEST0150.
      COMMIT WORK.
      MESSAGE S001.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.
ENDMODULE.

MODULE STATUS_8001 OUTPUT.

  SET PF-STATUS 'PF8001'.
  SET TITLEBAR 'T8001'.

  SELECT SINGLE * INTO ZLEST0162 FROM ZLEST0162.

  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'A'.
        IF ZLEST0162-CK_CONTINGENCIA IS INITIAL.
          SCREEN-INPUT = '1'.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'B'.
        IF ZLEST0162-CK_CONTINGENCIA IS INITIAL.
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INPUT = '1'.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDMODULE.

MODULE USER_COMMAND_8001_EXIT INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.

MODULE USER_COMMAND_8001 INPUT.

  CASE OK_CODE.
    WHEN 'HABI'.

      ZLEST0162-CK_CONTINGENCIA = ABAP_TRUE.
      MODIFY ZLEST0162.

      "Registra Log
      PERFORM F_REGISTRA_LOG_ZLEST0162 USING ZLEST0162.

      COMMIT WORK.
      CLEAR: OK_CODE.
    WHEN 'DESA'.

      ZLEST0162-CK_CONTINGENCIA = ABAP_FALSE.
      MODIFY ZLEST0162.

      "Registra Log
      PERFORM F_REGISTRA_LOG_ZLEST0162 USING ZLEST0162.

      COMMIT WORK.
      CLEAR: OK_CODE.
  ENDCASE.


ENDMODULE.

FORM F_REGISTRA_LOG_ZLEST0162 USING P_ZLEST0162 TYPE ZLEST0162.

  CLEAR: ZLEST0163.
  MOVE-CORRESPONDING P_ZLEST0162 TO ZLEST0163.

  SELECT MAX( ID_LOG )
    FROM ZLEST0163 INTO @DATA(_ID_LOG).

  ADD 1 TO _ID_LOG.
  ZLEST0163-ID_LOG      = _ID_LOG.
  ZLEST0163-DT_REGISTRO = SY-DATUM.
  ZLEST0163-HR_REGISTRO = SY-UZEIT.
  ZLEST0163-US_REGISTRO = SY-UNAME.
  MODIFY ZLEST0163.

ENDFORM.
