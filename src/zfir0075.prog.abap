*======================================================================*
* Report  ZFIR0075                                                     *
*                                                                      *
*======================================================================*
*                                                                      *
*                                                                      *
*======================================================================*
REPORT ZFIR0075.
*======================================================================*
* Tables                                                               *
*======================================================================*
TABLES: ZFIT0083.
*======================================================================*
* Variáveis                                                            *
*======================================================================*
DATA: IT_SAIDA        TYPE STANDARD TABLE OF ZFIT0083,
      IT_SAIDA_ALT    TYPE STANDARD TABLE OF ZFIT0083,
      IT_ZFIT0083_AUX TYPE STANDARD TABLE OF ZFIT0083,
      WA_SAIDA        TYPE ZFIT0083,
      WA_SAIDA_ALT    TYPE ZFIT0083,
      WA_ZFIT0083_AUX TYPE ZFIT0083,
      CONT            TYPE I,
      OK_CODE         TYPE SY-UCOMM.

DATA: GR_ALVGRID             TYPE REF TO CL_GUI_ALV_GRID,                 "Grid
      GC_CUSTOM_CONTROL_NAME TYPE SCRFNAME VALUE 'CC_ALV',                "CControl
      GR_CCONTAINER          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,         "CContainer
      IT_EXCLUDE             TYPE UI_FUNCTIONS,                           "Botões(Excluir)
      LS_EXCLUDE             TYPE UI_FUNC.                                "Botões(Excluir)

DATA: IT_FIELDCAT TYPE LVC_T_FCAT,                                        "Fieldcat
      WA_LAYOUT   TYPE LVC_S_LAYO.                                        "Layout
*======================================================================*
* Selection Screen                                                     *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_TRADE FOR ZFIT0083-SOURCE_REF.
SELECTION-SCREEN END OF BLOCK B1.
*======================================================================*
* Start of Selection                                                   *
*======================================================================*
START-OF-SELECTION.

  PERFORM TRATA_PARAMETRO.
  PERFORM SELECIONA_DADOS.
  CALL SCREEN 1010.

*&---------------------------------------------------------------------*
*&      Form  TRATA_PARAMETRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TRATA_PARAMETRO .

  FIELD-SYMBOLS: <S_TRADE> LIKE LINE OF S_TRADE.
  DATA: FIELD TYPE CHAR15.

  LOOP AT S_TRADE ASSIGNING <S_TRADE>.
    IF <S_TRADE>-OPTION EQ 'EQ'.
      MOVE 'CP' TO <S_TRADE>-OPTION.
      CONCATENATE <S_TRADE>-LOW '*' INTO FIELD.
      MOVE FIELD TO <S_TRADE>-LOW.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM ZFIT0083
    INTO TABLE IT_SAIDA
    WHERE SOURCE_REF IN S_TRADE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1010 OUTPUT.

  SET PF-STATUS 'ZPADRAO'.
  SET TITLEBAR 'T001'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.

  PERFORM DISPLAY_ALV.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .

  IF GR_ALVGRID IS INITIAL .

    CREATE OBJECT GR_CCONTAINER
      EXPORTING
        CONTAINER_NAME              = GC_CUSTOM_CONTROL_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
    ENDIF.

    CREATE OBJECT GR_ALVGRID
      EXPORTING
        I_PARENT          = GR_CCONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
    ENDIF.

    PERFORM PREPARE_FIELD_CATALOG.
    PERFORM PREPARE_LAYOUT.
    PERFORM EXCLUIR_BOTOES.

    CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA
        IT_FIELDCATALOG               = IT_FIELDCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC <> 0.
    ENDIF.

    CALL METHOD GR_ALVGRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
      EXCEPTIONS
        ERROR      = 1
        OTHERS     = 2.

  ELSE .

*    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
*      EXCEPTIONS
*        FINISHED = 1
*        OTHERS   = 2.
*
*    IF SY-SUBRC <> 0.
*    ENDIF.

  ENDIF .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_FIELD_CATALOG.

  PERFORM PREENCHE_FIELDCAT USING:
        'SOURCE_REF'          'C'         '10'        'Source Ref'              'Source Ref'                ' ',
        'DEAL_TYPE'           'C'         '10'        'Deal Type'               'Deal Type'                 ' ',
        'TRADER_NAME'         'C'         '10'        'Trader Name'             'Trader Name'               ' ',
        'COUNTERPARTY_NAM'    'C'         '10'        'Counterparty Name'       'Counterparty Name'         ' ',
        'DATE_CONFIRMED'      'C'         '10'        'Date Confirmed'          'Date Confirmed'            ' ',
        'TIME_CONFIRMED'      'C'         '10'        'Time Confirmed'          'Time Confirmed'            ' ',
        'AMOUNT_DEALT'        'C'         '10'        'Amount Dealt'            'Amount Dealt'              ' ',
        'DEALT_CURRENCY'      'C'         '10'        'Dealt Currency'          'Dealt Currency'            ' ',
        'COUNTER_AMOUNT'      'C'         '10'        'Counter Amount'          'Counter Amount'            ' ',
        'COUNTER_CURRENCY'    'C'         '10'        'Counter Currency'        'Counter Currency'          ' ',
        'BUKRS'               'C'         '10'        'Empresa'                 'Empresa'                   'X'.

*  DATA LS_FCAT TYPE LVC_S_FCAT.
*  LS_FCAT-FIELDNAME = 'TRADE_ID' .
*  LS_FCAT-INTTYPE = 'C' .
*  LS_FCAT-OUTPUTLEN = '10' .
*  LS_FCAT-COLTEXT = 'Trade ID' .
*  LS_FCAT-SELTEXT = 'Trade ID' .
*  APPEND LS_FCAT TO PT_FIELDCAT .
*  CLEAR LS_FCAT .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREENCHE_FIELDCAT  USING   P_FNAME    TYPE C
                                P_TYPE     TYPE C
                                P_LEN      TYPE C
                                P_COLTEXT  TYPE C
                                P_SELTEXT  TYPE C
                                P_EDIT     TYPE C.

  DATA LS_FCAT TYPE LVC_S_FCAT.

  LS_FCAT-FIELDNAME = P_FNAME.
  LS_FCAT-COLTEXT   = P_COLTEXT.
  LS_FCAT-SELTEXT   = P_SELTEXT.
  LS_FCAT-EDIT      = P_EDIT.
  APPEND LS_FCAT TO IT_FIELDCAT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_LAYOUT.

  WA_LAYOUT-ZEBRA = 'X' .
  WA_LAYOUT-GRID_TITLE = 'Operações Bloomberg'.
  WA_LAYOUT-CWIDTH_OPT = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EXCLUIR_BOTOES .

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1010 INPUT.
  CASE OK_CODE.
    WHEN 'CANCEL' OR 'EXIT' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM CHECK_ALTERACAO.
      IF IT_SAIDA_ALT IS NOT INITIAL.
        LOOP AT IT_SAIDA_ALT INTO WA_SAIDA_ALT.
          UPDATE ZFIT0083 FROM WA_SAIDA_ALT.
          IF SY-SUBRC NE 0.
            MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
            STOP.
          ENDIF.
        ENDLOOP.
        MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'S'.
      ELSE.
        MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CHECK_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ALTERACAO .

  IT_SAIDA_ALT = IT_SAIDA.

  SELECT *
    FROM ZFIT0083
    INTO TABLE IT_ZFIT0083_AUX.

  LOOP AT IT_SAIDA_ALT INTO WA_SAIDA_ALT.
    CONT = SY-TABIX.
    READ TABLE IT_ZFIT0083_AUX  INTO WA_ZFIT0083_AUX WITH KEY MSG_TYPE          = WA_SAIDA_ALT-MSG_TYPE
                                                              DEAL_TYPE         = WA_SAIDA_ALT-DEAL_TYPE
                                                              SIDE              = WA_SAIDA_ALT-SIDE
                                                              PRODUCT           = WA_SAIDA_ALT-PRODUCT
                                                              SOURCE_REF        = WA_SAIDA_ALT-SOURCE_REF
                                                              TRANS_TYPE        = WA_SAIDA_ALT-TRANS_TYPE
                                                              REV_TRADE         = WA_SAIDA_ALT-REV_TRADE
                                                              TRADE_ID          = WA_SAIDA_ALT-TRADE_ID
                                                              BLOCK_ID          = WA_SAIDA_ALT-BLOCK_ID
                                                              TRADER_ID         = WA_SAIDA_ALT-TRADER_ID
                                                              TRADER_NAME       = WA_SAIDA_ALT-TRADER_NAME
                                                              COUNTERPARTY_ID   = WA_SAIDA_ALT-COUNTERPARTY_ID
                                                              COUNTERPARTY_NAM  = WA_SAIDA_ALT-COUNTERPARTY_NAM
                                                              DATE_OF_DEAL      = WA_SAIDA_ALT-DATE_OF_DEAL
                                                              TIME_OF_DEAL      = WA_SAIDA_ALT-TIME_OF_DEAL.

    "sy-subrc sempre zero
    IF WA_ZFIT0083_AUX-BUKRS EQ WA_SAIDA_ALT-BUKRS.
      MOVE 'DEL' TO WA_SAIDA_ALT-BUKRS.
      MODIFY IT_SAIDA_ALT FROM WA_SAIDA_ALT.
    ELSE.
      MOVE SY-DATUM TO WA_SAIDA_ALT-DATA_ATUAL.
      MOVE SY-UZEIT TO WA_SAIDA_ALT-HORA_ATUAL.
      MOVE SY-UNAME TO WA_SAIDA_ALT-USUARIO.
      MODIFY IT_SAIDA_ALT FROM WA_SAIDA_ALT.
    ENDIF.
  ENDLOOP.
  CLEAR: WA_SAIDA_ALT.
  DELETE IT_SAIDA_ALT WHERE BUKRS EQ 'DEL'.

ENDFORM.
