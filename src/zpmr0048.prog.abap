*&-------------------------------------------------------------------------------------------------------*
*& Report  ZPMR0048                                                                                      *
*&                                                                                                       *
*& Data           : 04/06/2019                                                                           *
*& Especificado   : Anderson Oenning                                                                     *
*& Desenvolvimento: Anderson Oenning                                                                     *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*--------------------------------------------------------------------------------------------------------*
REPORT ZPMR0048.

TABLES: IFLO, JEST, TJ02T, ILOA.

DATA: BEGIN OF IJSTAT OCCURS   0.
        INCLUDE STRUCTURE JSTAT.
      DATA: END OF IJSTAT.

DATA STATUS_RANGE TYPE RANGE OF  STAT.

DATA:
  WA_LAYOUT        TYPE LVC_S_LAYO,
  LS_LAYOUT        TYPE LVC_S_LAYO,
  WT_STABLE        TYPE LVC_S_STBL,
  WA_STABLE        TYPE LVC_S_STBL,
  IT_FCAT          TYPE TABLE OF LVC_S_FCAT,
  OBJ_CUSTOM_0110  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  OBJ_ALV_0110     TYPE REF TO CL_GUI_ALV_GRID,
  GT_EXC_BUTTON    TYPE UI_FUNCTIONS,
  OBJ_ALV_STATUS   TYPE REF TO CL_GUI_ALV_GRID,
  LINES            TYPE SY-TABIX,
  IT_SELECTED_ROWS TYPE LVC_T_ROW,
  WA_SELECTED_ROWS TYPE LVC_S_ROW,
  WA_TOOLBAR       TYPE STB_BUTTON,
*  IT_UCOMM         TYPE TABLE OF TY_UCOMM,
  G_CONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*Paramentros de seleção.
*==============================================================
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 1 .
PARAMETER: P_ATIV AS CHECKBOX DEFAULT 'X' USER-COMMAND ABC.
SELECTION-SCREEN COMMENT 05(12) TEXT-003 FOR FIELD P_ATIV.

SELECTION-SCREEN POSITION 20.
PARAMETER: P_INCT AS CHECKBOX USER-COMMAND ABC.
SELECTION-SCREEN COMMENT 25(14) TEXT-004 FOR FIELD P_INCT.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
                 P_WERKS FOR IFLO-SWERK,
                 P_TPLNR FOR IFLO-TPLNR.
SELECTION-SCREEN: END OF BLOCK B2.


*Seleção de dados.
CLASS ZCL_LOCAL DEFINITION.
  PUBLIC SECTION.
    METHODS SELEC_LOCAL
      EXPORTING
        GW_LOCAL TYPE ZPME0024_T.

    METHODS EXIBIR_DADOS
      IMPORTING
        GS_LOCAL TYPE ZPME0024_T.
ENDCLASS.

CLASS ZCL_LOCAL IMPLEMENTATION.

*Method para selecionar os locais de instalação por centro.
  METHOD: SELEC_LOCAL.
    DATA: GT_STATUS TYPE TABLE OF JEST.
    DATA: CONT_STATUS TYPE P.

    IF P_ATIV IS NOT INITIAL.
      DATA(A) = 'A'.
    ENDIF.

    IF P_INCT IS NOT INITIAL.
      DATA(I) = 'I'.
    ENDIF.

    STATUS_RANGE = VALUE #( ( SIGN    = 'I' OPTION  = 'EQ' LOW   = A )
                            ( SIGN    = 'I' OPTION  = 'EQ' LOW   = I  ) ).

    CLEAR: CONT_STATUS.

    SELECT *
    FROM IFLO
    INTO CORRESPONDING FIELDS OF TABLE GW_LOCAL
    WHERE SWERK IN P_WERKS
      AND TPLNR IN P_TPLNR.

    LOOP AT GW_LOCAL ASSIGNING FIELD-SYMBOL(<_LOCAL>).
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          CLIENT      = SY-MANDT
          OBJNR       = <_LOCAL>-OBJNR
          ONLY_ACTIVE = 'X'
        TABLES
          STATUS      = IJSTAT.

      CLEAR CONT_STATUS.
      LOOP AT IJSTAT ASSIGNING FIELD-SYMBOL(<_STATUS>).
        ADD 1 TO CONT_STATUS.
      ENDLOOP.

      IF <_STATUS>-STAT = 'I0098' AND CONT_STATUS EQ '1'.
        <_LOCAL>-STATUS = 'A'.
      ELSE.
        <_LOCAL>-STATUS = 'I'.
      ENDIF.
    ENDLOOP.

    SORT GW_LOCAL ASCENDING BY STATUS.
    DELETE GW_LOCAL WHERE STATUS NOT IN STATUS_RANGE.
    SORT GW_LOCAL ASCENDING BY ILOAN.

  ENDMETHOD.

*==========================================================================
*Method para criar a ALV Container e exibir os dados.
  METHOD EXIBIR_DADOS.

    IF GS_LOCAL IS NOT INITIAL.
      CALL SCREEN 0100.

    ENDIF.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.

*Seleção de dados.

  DATA: OBJCT_LOC TYPE REF TO ZCL_LOCAL.
  CREATE OBJECT OBJCT_LOC.

  CALL METHOD OBJCT_LOC->SELEC_LOCAL
    IMPORTING
      GW_LOCAL = DATA(_LOCAL).


  CALL METHOD OBJCT_LOC->EXIBIR_DADOS
    EXPORTING
      GS_LOCAL = _LOCAL.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST001'.
  SET TITLEBAR 'T001'.

  IF ( OBJ_CUSTOM_0110 IS INITIAL ).


    PERFORM ALV_PREENCHE_CAT USING:

  1 'SWERK     ' ''   '60'  ''  ''   ''  'Centro         '  ''  ''  '',
  2 'ILOAN     ' ''   '60'  ''  ''   ''  'Chave          '  ''  ''  'X',
  3 'TPLNR     ' ''   '30'  ''  ''   ''  'Local Instação '  ''  ''  '',
  4 'PLTXT     ' ''   '10'  ''  ''   ''  'Desc Local     '  ''  ''  ''.
*  5 'TXT04     ' ''   '20'  ''  ''   ''  'Status         '  ''  ''  ''.


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


*    SET HANDLER:
*        LCL_EVENT_HANDLER=>ON_DATA_CHANGED  FOR OBJ_ALV_0110,
*        LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR OBJ_ALV_0110,
*        LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK  FOR OBJ_ALV_0110,
*        LCL_EVENT_HANDLER=>SET_TOOLBAR      FOR OBJ_ALV_0110,
*        LCL_EVENT_HANDLER=>GET_UCOMM        FOR OBJ_ALV_0110.


    CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING          = GT_EXC_BUTTON
        I_SAVE                        = 'A'
      CHANGING
        IT_FIELDCATALOG               = IT_FCAT
        IT_OUTTAB                     = _LOCAL
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.



    CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  ENDIF.

  CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.


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
*      -->P_2      text
*      -->P_0386   text
*      -->P_0387   text
*      -->P_0388   text
*      -->P_0389   text
*      -->P_0390   text
*      -->P_0391   text
*      -->P_0392   text
*      -->P_0393   text
*      -->P_0394   text
*      -->P_0395   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING    VALUE(P_COLNUM)
                                    VALUE(P_FIELDNAME)
                                    VALUE(P_TABNAME)
                                    VALUE(P_LEN)
                                    VALUE(P_EDIT)
                                    VALUE(P_ICON)
                                    VALUE(P_DO_SUM)
                                    VALUE(P_HEADER)
                                    VALUE(P_EMPHASIZE)
                                    VALUE(P_HOTSPOT)
                                    VALUE(P_ZERO).


  DATA:  WL_FCAT  TYPE LVC_S_FCAT.

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
  WL_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WL_FCAT-HOTSPOT     = P_HOTSPOT.
  WL_FCAT-NO_ZERO     = P_ZERO.

  WA_LAYOUT-CTAB_FNAME    = 'CELL_COLOR'.
  WA_LAYOUT-EXCP_CONDS    = 'X'.
  WA_LAYOUT-ZEBRA         = 'X'.
  WA_LAYOUT-SEL_MODE      = 'A'.
  WA_LAYOUT-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  WA_LAYOUT-TOTALS_BEF    = ''.

  APPEND WL_FCAT TO IT_FCAT.

*WL_FCAT.
*IT_FCAT.
*LVC_S_FCAT.
ENDFORM.
