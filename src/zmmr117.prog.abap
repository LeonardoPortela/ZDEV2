*&---------------------------------------------------------------------*
*& Report  ZMMR117
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR117.
TYPES:
  BEGIN OF TY_SAIDA,
    RSNUM      TYPE RESB-RSNUM,
    UNAME      TYPE ZMMT0003-UNAME,
    DT_VAL_DE  TYPE ZMMT0003-DT_VAL_DE,
    DT_VAL_ATE TYPE ZMMT0003-DT_VAL_ATE,
  END OF TY_SAIDA,

  BEGIN OF TY_SAIDA2,
    RSNUM        TYPE ZMMT0009-RSNUM,
    RSPOS        TYPE ZMMT0009-RSPOS,
    KOSTL        TYPE ZMMT0009-KOSTL,
    UNAME        TYPE ZMMT0009-UNAME,
    DT_APROVACAO TYPE ZMMT0009-DT_APROVACAO,
    HR_APROVACAO TYPE ZMMT0009-HR_APROVACAO,
  END OF TY_SAIDA2.


DATA: WA_RKPF     TYPE RKPF,
      WA_ZMMT0003 TYPE ZMMT0003,
      WA_ZMMT0009 TYPE ZMMT0009,
      WA_SAIDA    TYPE TY_SAIDA,
      WA_SAIDA2   TYPE TY_SAIDA2,
      V_KOSTL     TYPE RKPF-KOSTL.


DATA: IT_ZMMT0003 TYPE TABLE OF ZMMT0003,
      IT_ZMMT0009 TYPE TABLE OF ZMMT0009,
      IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      IT_SAIDA2   TYPE TABLE OF TY_SAIDA2.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar
*CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: EDITCONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      CONTAINER_1        TYPE REF TO CL_GUI_CONTAINER,
      CONTAINER_2        TYPE REF TO CL_GUI_CONTAINER,
      SPLITTER           TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      EDITOR             TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95    TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID      TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID            TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE          TYPE LVC_S_STBL,
      WA_AFIELD          TYPE LVC_S_FCAT,
      IT_FIELDCAT        TYPE LVC_T_FCAT,
      W_FIELDCAT         TYPE LVC_S_FCAT,
      I_SORT             TYPE LVC_T_SORT,
      WA_LAYOUT          TYPE LVC_S_LAYO,
      IS_STABLE          TYPE LVC_S_STBL VALUE 'XX',
      WG_SAVE(1)         TYPE C,
      GS_VARIANT_C       TYPE DISVARIANT.

CONSTANTS:   C_X               TYPE C VALUE 'X'.


*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: CATCH_HOTSPOT
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD CATCH_HOTSPOT.
    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.

    CHECK SY-SUBRC = 0.

    IF E_COLUMN_ID = 'RSNUM'.
      SET PARAMETER ID 'RES'  FIELD WA_SAIDA-RSNUM.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RSNUM TYPE RESB-RSNUM OBLIGATORY.
SELECTION-SCREEN:END OF BLOCK B1.

PARAMETERS:
  R_ST_A RADIOBUTTON GROUP RAD1 DEFAULT 'X',
  R_ST_E RADIOBUTTON GROUP RAD1.

START-OF-SELECTION.
  PERFORM: F_SELECIONAR_DADOS,               " Form selecionar dado
           F_ORGANIZAR_DADOS,                " ORGANIZAR DADOS
           F_ALV.                            "Saida ALV

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONAR_DADOS .
  SELECT SINGLE *
    FROM RKPF
    INTO WA_RKPF
    WHERE RSNUM = P_RSNUM.

  CHECK SY-SUBRC = 0.

  IF WA_RKPF-BWART = '311'.
    V_KOSTL = WA_RKPF-WEMPF+0(10).
  ELSE.
    V_KOSTL = WA_RKPF-KOSTL.
  ENDIF.
  IF R_ST_A = 'X'.
    SELECT *
      FROM ZMMT0009
      INTO TABLE IT_ZMMT0009
      WHERE RSNUM = P_RSNUM.
  ELSE.
    SELECT *
      FROM ZMMT0003
      INTO TABLE IT_ZMMT0003
      WHERE KOSTL = V_KOSTL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF  NOT CL_GRID IS INITIAL.
    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    IF R_ST_A = 'X'.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT      = GS_VARIANT_C
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
          I_DEFAULT       = 'X'
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA2[].
    ELSE.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT      = GS_VARIANT_C
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
          I_DEFAULT       = 'X'
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA[].
    ENDIF.
    SET HANDLER:
         LCL_EVENT_HANDLER=>CATCH_HOTSPOT            FOR CL_GRID.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ORGANIZAR_DADOS .
  IF R_ST_A = 'X'.
    LOOP AT IT_ZMMT0009 INTO WA_ZMMT0009.
      MOVE-CORRESPONDING WA_ZMMT0009 TO WA_SAIDA2.
      APPEND WA_SAIDA2 TO IT_SAIDA2.
    ENDLOOP.
  ELSE.
    LOOP AT IT_ZMMT0003 INTO WA_ZMMT0003.
      MOVE-CORRESPONDING WA_ZMMT0003 TO WA_SAIDA.
      WA_SAIDA-RSNUM = P_RSNUM.
      APPEND WA_SAIDA TO IT_SAIDA.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .
  PERFORM F_ALV_FIELDCAT.
  WA_LAYOUT-ZEBRA     = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.

  WA_LAYOUT-GRID_TITLE = 'Aprovadores'.

  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT = ''.
  WA_LAYOUT-BOX_FNAME  = ''.
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
           WL_HORA(8),
           WL_LINHA(60),
           WL_TEXT TYPE SDYDO_TEXT_ELEMENT.



  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  CONCATENATE  'Reserva:' P_RSNUM
  INTO WL_LINHA SEPARATED BY SPACE.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CONCATENATE  'Centro de Custo:' V_KOSTL
  INTO WL_LINHA SEPARATED BY SPACE.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  DATA I TYPE I.
  IF R_ST_A = 'X'.
    REFRESH IT_FIELDCAT.

    WA_AFIELD-TABNAME     = 'IT_SAIDA2'.
    WA_AFIELD-COLDDICTXT = 'M'.
    WA_AFIELD-SELDDICTXT = 'M'.
    WA_AFIELD-TIPDDICTXT = 'M'.
    WA_AFIELD-COL_OPT = 'X'.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'RSNUM'.
    WA_AFIELD-SCRTEXT_S = 'Reserva'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 12.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'RSPOS'.
    WA_AFIELD-SCRTEXT_S = 'linha'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 8.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'KOSTL'.
    WA_AFIELD-SCRTEXT_S = 'C.Custo'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 10.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'UNAME'.
    WA_AFIELD-SCRTEXT_S = 'Usuario'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 30.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'DT_APROVACAO'.
    WA_AFIELD-SCRTEXT_S = 'dt.Aprovação'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 10.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'HR_APROVACAO'.
    WA_AFIELD-SCRTEXT_S = 'hr.Aprovação'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 10.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.
  ELSE.

    REFRESH IT_FIELDCAT.

    WA_AFIELD-TABNAME     = 'IT_SAIDA'.
    WA_AFIELD-COLDDICTXT = 'M'.
    WA_AFIELD-SELDDICTXT = 'M'.
    WA_AFIELD-TIPDDICTXT = 'M'.
    WA_AFIELD-COL_OPT = 'X'.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'RSNUM'.
    WA_AFIELD-SCRTEXT_S = 'Reserva'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 30.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.


    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'UNAME'.
    WA_AFIELD-SCRTEXT_S = 'Usuario'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 30.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'DT_VAL_DE'.
    WA_AFIELD-SCRTEXT_S = 'dt.Inicio'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 15.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'DT_VAL_ATE'.
    WA_AFIELD-SCRTEXT_S = 'dt.Final'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN = 15.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-HOTSPOT       = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
