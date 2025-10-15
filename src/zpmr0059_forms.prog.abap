*&---------------------------------------------------------------------*
*&  Include           ZPMR0059_FORMS
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  SET TITLEBAR  'TIT0100'.

  REFRESH IT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
  01 'IWERK   ' '' '10' ''  ''  ' ' 'Centro              ',
  02 'ARBPL   ' '' '10' ''  ''  ' ' 'Cent.Trab           ',
  03 'HR_PLAN ' '' '14' ''  ''  'X' 'T.hr capacidade     ',
  04 'HRSPER  ' '' '13' ''  ''  'X' 'T.hr planejada      ',
  05 'HR_REAL ' '' '13' ''  ''  'X' 'T.hr realizada      ',
  06 'BACKL   ' '' '11' ''  ''  'X' 'Backlog/dia         '.

  PERFORM FILL_IT_SORT.

  WA_LAYOUT-EXCP_CONDS     = 'X'.
  WA_LAYOUT-ZEBRA          = 'X'.
  WA_LAYOUT-SEL_MODE       = 'A'.
*  WA_LAYOUT-CWIDTH_OPT     = 'X'.     "  Otimizar colunas na tela
  WA_LAYOUT-TOTALS_BEF     = ' '.

  IF ( OBJ_CUSTOM_0110 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0110
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0110
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0110
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  SET HANDLER: LCL_EVENTS_HANDLER=>HANDLE_DOUBLE_CLICK FOR OBJ_ALV_0110.


* load data into the grid and display them
  CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_LAYOUT
*     I_SAVE          = 'A'
    CHANGING
      IT_FIELDCATALOG = IT_FCAT
      IT_SORT         = IT_SORT
      IT_OUTTAB       = T_BACKLOG.



  CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0123   text
*      -->P_0124   text
*      -->P_0125   text
*      -->P_0126   text
*      -->P_0127   text
*      -->P_0128   text
*      -->P_0129   text
*      -->P_0130   text
*      -->P_0131   text
*      -->P_0132   text
*      -->P_0133   text
*      -->P_0134   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING: VALUE(P_COLNUM)
                                VALUE(P_FIELDNAME)
                                VALUE(P_TABNAME)
                                VALUE(P_LEN)
                                VALUE(P_EDIT)
                                VALUE(P_ICON)
                                VALUE(P_DO_SUM)
                                VALUE(P_HEADER).

  DATA: WL_FCAT TYPE LVC_S_FCAT.
  CLEAR: WA_LAYOUT, WL_FCAT.

  WL_FCAT-COL_POS     = P_COLNUM.
  WL_FCAT-FIELDNAME   = P_FIELDNAME.
  WL_FCAT-TABNAME     = P_TABNAME.
  WL_FCAT-OUTPUTLEN   = P_LEN.
  WL_FCAT-COLTEXT     = P_HEADER.
  WL_FCAT-EDIT        = P_EDIT.
  WL_FCAT-ICON        = P_ICON.
  WL_FCAT-REF_TABLE   = P_TABNAME.
  WL_FCAT-CHECKTABLE  = P_TABNAME.
  WL_FCAT-DO_SUM      = P_DO_SUM.
  APPEND WL_FCAT TO IT_FCAT.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM SEL_LISTA  USING    P_ROW P_COLUMN_FIELDNAME.

  FREE CLICKS.
  ADD 1 TO CLICKS.
  TRY .
      DATA(WA_BACKLOG) = T_BACKLOG[ P_ROW ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.

  CASE P_COLUMN_FIELDNAME.
    WHEN 'ARBPL'.
      SET PARAMETER ID 'WRK' FIELD WA_BACKLOG-IWERK.
      SET PARAMETER ID 'AGR' FIELD WA_BACKLOG-ARBPL.
      CALL TRANSACTION 'IR03' AND SKIP FIRST SCREEN.

    WHEN 'HRSPER'.

*      BREAK-POINT.
      LCL_DADOS_ORDENS=>SHDB_IW49( EXPORTING WERKS = WA_BACKLOG-IWERK
                                             ARBPL = WA_BACKLOG-ARBPL )..

  ENDCASE.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SHDB_IW49
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHDB_IW49 .

ENDFORM.


FORM FILL_IT_SORT .

  DATA: WA_SORT TYPE LVC_S_SORT.

  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'IWERK'.
  "WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

*  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
*
*    WA_SORT-SPOS = '2'.
*    WA_SORT-FIELDNAME = 'TPLMA'.
*    "WA_SORT-DOWN = 'X'.
*    WA_SORT-GROUP = '*'.
*    WA_SORT-SUBTOT = 'X'.
*    APPEND WA_SORT TO IT_SORT.

*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDMETHOD  text
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

  APPEND VALUE #(
                PROGRAM   = P_PROGRAM
                DYNPRO    = P_DYNPRO
                DYNBEGIN  = P_START
                FNAM      = P_FNAM
                FVAL      = P_FVAL
  ) TO TI_BDCDATA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1264   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION  USING P_TRANS CHANGING P_ERRO.

  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT    TYPE SY-TABIX,
        WL_MODE(1).

*  FREE IT_MSG .
  CLEAR WL_MODE.
  CLEAR WL_CONT.
  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
                           MODE WL_MODE
                           MESSAGES INTO IT_MSG.

  CLEAR: WL_CONT.

  IF LINE_EXISTS( IT_MSG[ MSGTYP = 'A' ] ).
    P_ERRO = ABAP_TRUE.
  ELSE.
    IF LINE_EXISTS( IT_MSG[ MSGTYP = 'E' ] ).
      P_ERRO = ABAP_TRUE.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_ARBPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECT_ARBPL .

  SELECT WERKS ARBPL
  INTO TABLE IT_ARBPL
  FROM CRHD
  WHERE WERKS IN S_WERKS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD    = 'ARBPL'
      DYNPPROG    = SY-REPID
      DYNPNR      = SY-DYNNR
      DYNPROFIELD = 'S_ARBPL'
      VALUE_ORG   = 'S'
    TABLES
      VALUE_TAB   = IT_ARBPL.

ENDFORM.
