*&---------------------------------------------------------------------*
*& Report  ZLESR0140
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0140.

TYPE-POOLS ICON.

TYPES: BEGIN OF TY_SAIDA,
         STATUS  TYPE ICON-ID,
         OBJ_KEY TYPE CHAR20,
         BELNR   TYPE CHAR10,
         SEQITEM TYPE NUM6,
         BUKRS   TYPE BUKRS,
         DESCR   TYPE CHAR50,
       END OF TY_SAIDA,


       BEGIN OF TY_TVRO,
         ROUTE   TYPE TVRO-ROUTE,
         TRAZT   TYPE TVRO-TRAZT,
         MESSAGE TYPE BAPI_MSG,



       END OF TY_TVRO.



CONSTANTS: C_0VTC   TYPE SY-TCODE       VALUE '0VTC',
           C_S(1)   TYPE C              VALUE 'S',
           C_N(1)   TYPE C              VALUE 'N',
           C_007(3) TYPE C              VALUE '007',
           C_VY(2)  TYPE C              VALUE 'VY'.

DATA: OBJ_CUSTOM         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_CUSTOM_LOG     TYPE REF TO CL_GUI_CONTAINER,
      OBJ_SPLITTER       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      OBJ_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      GT_PLANILHA        LIKE STANDARD TABLE OF ALSMEX_TABLINE,
      GT_MSG_RETURN      TYPE TABLE OF ZFIWRS0002,
      GT_V_TVRO_COM      TYPE TABLE OF TY_TVRO,
      GT_ZIBCONTABIL_ERR TYPE TABLE OF ZIB_CONTABIL_ERR,
      GT_FCAT            TYPE TABLE OF LVC_S_FCAT,
      GT_SAIDA           TYPE TABLE OF TY_SAIDA,
      WL_PLANILHA        LIKE ALSMEX_TABLINE,
      WL_MSG_RETURN      TYPE ZFIWRS0002,
      WL_SAIDA           TYPE TY_SAIDA,
      WL_TVRO_COM        TYPE TY_TVRO,
      WL_LAYOUT          TYPE LVC_S_LAYO,
      WL_MENSAGEM        TYPE CHAR30,
      WL_STABLE          TYPE LVC_S_STBL,
      WL_ZIBCONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
      WL_ZIBCONTABIL_ERR TYPE ZIB_CONTABIL_ERR,
      WL_TOOLBAR         TYPE STB_BUTTON,
      OK_CODE            LIKE SY-UCOMM,
      P_FILE             TYPE RLGRAP-FILENAME,
      WA_BDC             TYPE          BDCDATA,
      VL_MODE            TYPE          C,
      TI_MSG             TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      WA_MSG             TYPE          BDCMSGCOLL,
      TI_BDC             TYPE TABLE OF BDCDATA    WITH HEADER LINE,
      WA_TVRO            TYPE TVRO,
      TI_VTRO            TYPE TABLE OF TY_TVRO,
      _RETURN            TYPE BAPIRET2.

DATA: REPID           LIKE SY-REPID.
DATA: FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
DATA: LAYOUT          TYPE SLIS_LAYOUT_ALV.
DATA: PRINT           TYPE SLIS_PRINT_ALV.
DATA: SORT      TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      EVENTS    TYPE SLIS_T_EVENT,
      XS_EVENTS TYPE SLIS_ALV_EVENT.
DATA: W_TIT(70).

START-OF-SELECTION.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT       USING: P_CAMPO         TYPE C
                                   P_DESC          TYPE C
                                   P_TAM           TYPE C
                                   P_HOT           TYPE C
                                   P_ZERO          TYPE C
                                   P_SUM           TYPE C
                                   P_ICON          TYPE C.
  DATA:
  WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-FIELDNAME  = P_CAMPO.
  WL_FCAT-SCRTEXT_L  = P_DESC.
  WL_FCAT-SCRTEXT_M  = P_DESC.
  WL_FCAT-SCRTEXT_S  = P_DESC.
  WL_FCAT-HOTSPOT    = P_HOT.
  WL_FCAT-NO_ZERO    = P_ZERO.
  WL_FCAT-OUTPUTLEN  = P_TAM.
  WL_FCAT-ICON       = P_ICON.

  APPEND WL_FCAT TO GT_FCAT.
ENDFORM.                    "ALV_PREENCHE_CAT



*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      SET_TOOLBAR  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT.

    "CLASS-METHODS:
    "GET_UCOMM   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
    "IMPORTING E_UCOMM.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION.

  METHOD SET_TOOLBAR.
    CLEAR: WL_TOOLBAR.

    WL_TOOLBAR-BUTN_TYPE    = 3.
    APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR WL_TOOLBAR.
  ENDMETHOD.                    "SET_TOOLBAR

ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  REFRESH GT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
    'ROUTE'         'Itinerário'          '15'   ''  ''  '' '',
    'MESSAGE'       'Mensagem'            '60'  ''  ''  ''  ''.

  IF ( OBJ_CUSTOM IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV
      EXPORTING
        I_PARENT          = OBJ_CUSTOM
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    WL_LAYOUT-ZEBRA      = 'X'.
  ENDIF.

  CREATE OBJECT OBJ_SPLITTER
    EXPORTING
      PARENT  = OBJ_CUSTOM
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD OBJ_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = OBJ_CUSTOM_LOG.

  SET HANDLER:
  LCL_EVENT_TOOLBAR=>SET_TOOLBAR     FOR OBJ_ALV.
  "LCL_EVENT_TOOLBAR=>GET_UCOMM       FOR OBJ_ALV.

  CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      I_DEFAULT                     = 'X'
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = TI_VTRO
      IT_FIELDCATALOG               = GT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD OBJ_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 100.
ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BTN_EXECUTAR'.
      IF ( P_FILE IS INITIAL ).
        MESSAGE TEXT-E01 TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        CHECK ( GT_MSG_RETURN IS INITIAL ).
        PERFORM TRATAR_ARQUIVO.
      ENDIF.
      "WHEN 'SHOW_MSG'.
      "PERFORM SHOW_MSG.
    WHEN OTHERS.
  ENDCASE.

  CLEAR SY-UCOMM.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  CARREGA_ARQUIVO  INPUT
*&---------------------------------------------------------------------*
MODULE CARREGA_ARQUIVO INPUT.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDMODULE.                 " CARREGA_ARQUIVO  INPUT
*&---------------------------------------------------------------------*
*&      Form  TRATAR_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRATAR_ARQUIVO .

  REFRESH: GT_PLANILHA, GT_SAIDA, GT_V_TVRO_COM.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = TEXT-I01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 2
      I_END_ROW               = 10000
    TABLES
      INTERN                  = GT_PLANILHA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  LOOP AT GT_PLANILHA INTO WL_PLANILHA.
    AT NEW ROW.
      CLEAR WL_TVRO_COM.
    ENDAT.

    IF WL_PLANILHA-VALUE(1) = SPACE.
      SHIFT WL_PLANILHA-VALUE LEFT DELETING LEADING SPACE.
    ENDIF.

    CASE WL_PLANILHA-COL.
      WHEN 1.
        WL_TVRO_COM-ROUTE = WL_PLANILHA-VALUE.
      WHEN 2.
        WL_TVRO_COM-TRAZT = WL_PLANILHA-VALUE.
    ENDCASE.

    AT END OF ROW.
      APPEND WL_TVRO_COM TO GT_V_TVRO_COM.
    ENDAT.
  ENDLOOP.

  SORT GT_SAIDA BY SEQITEM.


  PERFORM GERA_SHDB.

ENDFORM.



FORM GERA_SHDB.
  "BREAK-POINT.
  FREE: TI_VTRO.
  LOOP AT GT_V_TVRO_COM INTO DATA(WA_V_TVRO_COM).
    FREE TI_BDC.

    SELECT COUNT(*)
     FROM TVRO
      INTO WA_TVRO
      WHERE ROUTE = WA_V_TVRO_COM-ROUTE.

    IF SY-SUBRC IS NOT INITIAL.
      WA_V_TVRO_COM-MESSAGE = 'Itinerario não existe na base de Dados.'.
      APPEND WA_V_TVRO_COM TO TI_VTRO.
    ELSE.

      DATA VL_CA TYPE CHAR10.

      VL_CA = WA_V_TVRO_COM-TRAZT.
      REPLACE '.' WITH ',' INTO VL_CA.
      CONDENSE VL_CA NO-GAPS.

      PERFORM ZF_BDC USING:
    'X' 'SAPL0VTR' '2000'    ,
    ' ' 'BDC_CURSOR'  'V_TVRO_COM-TDVZND(01)' ,
    ' ' 'BDC_OKCODE'  '=POSI',
    ' ' 'BDC_SUBSCR'  'SAPLSVCM                               0101NAVIGATION',

    'X' 'SAPLSPO4'  '0300',
    ' ' 'BDC_CURSOR'    'SVALD-VALUE(01)',
    ' ' 'BDC_OKCODE'  '=FURT',
    ' ' 'SVALD-VALUE(01)'  WA_V_TVRO_COM-ROUTE,

     'X' 'SAPL0VTR'  '2000'  ,
     ' ' 'BDC_CURSOR' 'V_TVRO_COM-TRAZTD(01)',
     ' ' 'BDC_OKCODE'  '/00',
     ' '  'V_TVRO_COM-TRAZTD(01)' VL_CA,
     ' ' 'BDC_SUBSCR'  'SAPLSVCM                                0101NAVIGATION',

     'X' 'SAPL0VTR'  '2000',
     ' ' 'BDC_CURSOR'  'V_TVRO_COM-TRAZTD(01)',
     ' ' 'BDC_OKCODE'  '=SAVE',
     ' ' 'BDC_SUBSCR'  'SAPLSVCM                                0101NAVIGATION',

     'X' 'SAPL0VTR'  '2000',
     ' ' 'BDC_CURSOR'  'V_TVRO_COM-TRAZTD(01)',
     ' ' 'BDC_OKCODE'  '=BACK',
     ' ' 'BDC_SUBSCR'  'SAPLSVCM                                0101NAVIGATION'.

      VL_MODE = C_N.

      CALL TRANSACTION C_0VTC
          USING TI_BDC
          MODE   VL_MODE
          UPDATE C_S
          MESSAGES INTO TI_MSG.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          TYPE   = SY-MSGTY
          CL     = SY-MSGID
          NUMBER = SY-MSGNO
          PAR1   = SY-MSGV1
          PAR2   = SY-MSGV2
          PAR3   = SY-MSGV3
          PAR4   = SY-MSGV4
        IMPORTING
          RETURN = _RETURN.


      IF _RETURN-MESSAGE_V1 IS NOT INITIAL.
        WA_V_TVRO_COM-MESSAGE = 'Atualização falhou!'.
        APPEND WA_V_TVRO_COM TO TI_VTRO.
      ENDIF.

    ENDIF.


    FREE TI_MSG .
  ENDLOOP.



  MESSAGE TEXT-S02 TYPE 'I' DISPLAY LIKE 'S'.

  "PERFORM ALV_ERRO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN text
*      -->P_NAME     text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM ZF_BDC USING P_DYNBEGIN TYPE ANY
                  P_NAME     TYPE ANY
                  P_VALUE    TYPE ANY.

  IF P_DYNBEGIN EQ 'X'.

    WA_BDC-PROGRAM  = P_NAME.
    WA_BDC-DYNPRO   = P_VALUE.
    WA_BDC-DYNBEGIN = P_DYNBEGIN.

    APPEND WA_BDC TO TI_BDC.

  ELSE.

    WA_BDC-FNAM = P_NAME.
    WA_BDC-FVAL = P_VALUE.
    APPEND WA_BDC TO TI_BDC.

  ENDIF.

  CLEAR WA_BDC.


ENDFORM.                    "zf_bdc


*----------------------------------------------------------------------*
FORM MONTA_FIELDCAT USING
               X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_QFIELD
               X_HOTSPOT X_KEY X_ZERO.
*----------------------------------------------------------------------*
*

  FIELDCAT-FIELDNAME     = X_FIELD.
  FIELDCAT-TABNAME       = X_TAB.
  FIELDCAT-REF_TABNAME   = X_REF.
  FIELDCAT-DO_SUM        = X_SUM.
  FIELDCAT-JUST          = X_JUST.
  FIELDCAT-QFIELDNAME    = X_QFIELD.
  FIELDCAT-HOTSPOT       = X_HOTSPOT.
  FIELDCAT-KEY           = X_KEY.
  FIELDCAT-NO_ZERO       = X_ZERO.

  APPEND FIELDCAT.
  CLEAR FIELDCAT.
*
ENDFORM.                               " MONTA_FIELDCAT
