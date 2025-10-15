*&---------------------------------------------------------------------*
*& Report  ZFIR072
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR072.


TABLES: ZFIT0148.


DATA: IT_SAIDA     TYPE TABLE OF  ZFIT0148,
      WA_SAIDA     TYPE ZFIT0148,
      IT_SAIDA_AUX TYPE TABLE OF  ZFIT0148,
      WA_SAIDA_AUX TYPE ZFIT0148,
      IT_ZFIT0148  TYPE TABLE OF ZFIT0148,
      WA_ZFIT0148  TYPE ZFIT0148.


DATA: G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAYOUT          TYPE LVC_S_LAYO,
      TL_FUNCTION        TYPE UI_FUNCTIONS,
      WL_FUNCTION        LIKE TL_FUNCTION WITH HEADER LINE,
      LS_FCAT            TYPE LVC_S_FCAT,
      PT_FIELDCAT        TYPE LVC_T_FCAT,
      TG_SELECTEDCELL    TYPE LVC_T_CELL,
      WG_SELECTEDCELL    TYPE LVC_S_CELL,
      TG_SELECTEDROW     TYPE LVC_T_ROW,
      WG_SELECTEDROW     TYPE LVC_S_ROW,
      WA_STABLE          TYPE LVC_S_STBL VALUE 'XX'.

DATA:  C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
       TY_TOOLBAR           TYPE STB_BUTTON.

DATA: ABAP  TYPE C,
      VMODF TYPE C.


CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.

    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,

      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      USER_COMMAND  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.

CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING E_ONF4 E_ONF4_AFTER E_ONF4_BEFORE E_UCOMM ER_DATA_CHANGED.
ENDCLASS.


CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD  ON_DATA_CHANGED.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(WA_DATA_CHANGED).

      LOOP AT IT_SAIDA INTO WA_SAIDA.

        CHECK WA_DATA_CHANGED-ROW_ID EQ SY-TABIX.

        CASE WA_DATA_CHANGED-FIELDNAME.
          WHEN 'CFOP'.
            WA_SAIDA_AUX-CFOP = WA_DATA_CHANGED-VALUE.
          WHEN 'DATA_INICIAL'.
            WA_SAIDA_AUX-DATA_INICIAL = WA_DATA_CHANGED-VALUE.
          WHEN 'DATA_FIM'.
            WA_SAIDA_AUX-DATA_FIM = WA_DATA_CHANGED-VALUE.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.


CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.

  METHOD CONSTRUCTOR.
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.

  METHOD ON_TOOLBAR.
    TY_TOOLBAR-ICON     = ICON_CREATE.
    TY_TOOLBAR-FUNCTION = 'NOVO'.
    TY_TOOLBAR-TEXT     = 'Novo'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON     = ICON_CHANGE.
    TY_TOOLBAR-FUNCTION = 'MODIFICAR'.
    TY_TOOLBAR-TEXT     = 'Modificar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.


    TY_TOOLBAR-ICON     = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION = 'EXCLUIR'.
    TY_TOOLBAR-TEXT     = 'Excluir'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
  ENDMETHOD.


  METHOD USER_COMMAND.

    DATA:  ANS   TYPE C.

    CASE E_UCOMM.
      WHEN 'NOVO'.
        VMODF = ' '.
        CALL SCREEN 0110 STARTING AT 10 05.

        PERFORM: Z_SELECIONA_DADOS,
                 Z_TRATAR_DADOS.

        LEAVE TO SCREEN 0100.

      WHEN  'MODIFICAR'.

        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.

        IF TG_SELECTEDROW[] IS  INITIAL.
          MESSAGE  'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF LINES( TG_SELECTEDROW ) NE 1.
          MESSAGE  'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE TG_SELECTEDROW INTO WG_SELECTEDROW INDEX 1.

        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.
        IF SY-SUBRC = 0.
          CLEAR WA_ZFIT0148.

          MOVE-CORRESPONDING WA_SAIDA TO WA_ZFIT0148.

          VMODF = 'X'.

          CALL SCREEN 0110 STARTING AT 10 05 .

          PERFORM: Z_SELECIONA_DADOS,
                   Z_TRATAR_DADOS.

          LEAVE TO SCREEN 0100.

        ENDIF.


      WHEN  'EXCLUIR'.
        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.

        IF TG_SELECTEDROW[] IS NOT INITIAL.
          VMODF = ' '.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR      = 'Confirmação'
              TEXT_QUESTION = 'Deseja  excluir a(s) linha(s) selecionada(s)?'
              TEXT_BUTTON_1 = 'Sim'
              ICON_BUTTON_1 = 'ICON_CHECKED'
              TEXT_BUTTON_2 = 'Não'
              ICON_BUTTON_2 = 'ICON_CANCEL'
              POPUP_TYPE    = 'ICON_MESSAGE_ERROR'
            IMPORTING
              ANSWER        = ANS.

          CASE ANS.
            WHEN 2 OR 'A'.
              LEAVE TO CURRENT TRANSACTION.
            WHEN 1.
              PERFORM Z_EXCLUIR.
          ENDCASE.
        ELSE.
          MESSAGE 'Favor selecionar uma linha!' TYPE 'I'.
          EXIT.
        ENDIF.
    ENDCASE.

    IF G_GRID IS NOT INITIAL.
      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
      "EXPORTING
      "   IS_STABLE = WA_STABLE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


INITIALIZATION.

  PERFORM: Z_SELECIONA_DADOS,
           Z_TRATAR_DADOS,
           Z_ALV.

  CALL SCREEN 0100.

FORM Z_SELECIONA_DADOS.

  SELECT * FROM ZFIT0148  INTO TABLE IT_SAIDA_AUX.

ENDFORM.

FORM Z_TRATAR_DADOS.

  REFRESH IT_SAIDA.
  LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.

    CLEAR: WA_SAIDA.

    MOVE-CORRESPONDING WA_SAIDA_AUX TO WA_SAIDA.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: OBG_TOOLBAR  TYPE REF TO LCL_ALV_TOOLBAR.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR  'TITULO'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CONTAINER'.

    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = G_GRID.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    GS_LAYOUT-SEL_MODE   = 'A'.
    WA_STABLE-ROW         = 'X'.
    WA_STABLE-COL         = 'X'.

    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR   FOR G_GRID,
                 OBG_TOOLBAR->USER_COMMAND FOR G_GRID.


    CALL METHOD CL_GUI_CFW=>FLUSH.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_OUTTAB            = IT_SAIDA
        IT_FIELDCATALOG      = PT_FIELDCAT.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
  ELSE.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.
*&-----------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_ALV.
  PERFORM PREENCHE_CAT USING:
         'CFOP'           'CFOP'          '10'   ''   ''    ''    '',
         'DATA_INICIAL'   'Data Inicial'  '10'   ''   ''    ''    '',
         'DATA_FIM'       'Data Final'    '10'   ''   ''    ''    ''.
ENDFORM.

FORM PREENCHE_CAT USING VALUE(P_CAMPO)
                        VALUE(P_DESC)
                        VALUE(P_TAM)
                        VALUE(P_EDIT)
                        VALUE(P_ZERO)
                        VALUE(P_FIELD)
                        VALUE(P_TABLE).

  LS_FCAT-FIELDNAME   =  P_CAMPO.
  LS_FCAT-COLTEXT     =  P_DESC.
  LS_FCAT-OUTPUTLEN   =  P_TAM.
  LS_FCAT-EDIT        =  P_EDIT.
  LS_FCAT-NO_ZERO     =  P_ZERO.
  LS_FCAT-REF_FIELD   =  P_FIELD.
  LS_FCAT-REF_TABLE   =  P_TABLE.

  APPEND LS_FCAT TO PT_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_EXCLUIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_EXCLUIR .

  LOOP AT TG_SELECTEDROW INTO WG_SELECTEDROW.
    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.
    DELETE  ZFIT0148 FROM WA_SAIDA.
  ENDLOOP.

  CLEAR: WA_SAIDA, WA_ZFIT0148.
  REFRESH: IT_SAIDA, IT_ZFIT0148.


  PERFORM: Z_SELECIONA_DADOS,
           Z_TRATAR_DADOS.

  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'TITULO_01'.

  REFRESH IT_ZFIT0148.

  IF VMODF = 'X'.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'WA_ZFIT0148-CFOP'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONF'.
      IF WA_ZFIT0148-CFOP IS INITIAL.
        MESSAGE 'Favor informar  o CFOP!' TYPE 'I'.
        EXIT.
      ELSEIF WA_ZFIT0148-DATA_INICIAL IS INITIAL.
        MESSAGE 'Favor informar a Data Inicial!' TYPE 'I'.
        EXIT.
      ELSEIF WA_ZFIT0148-DATA_FIM IS INITIAL.
        MESSAGE 'Favor informar a Data Final!' TYPE 'I'.
        EXIT.
      ELSE.
        APPEND WA_ZFIT0148 TO IT_ZFIT0148.
        MODIFY ZFIT0148 FROM TABLE IT_ZFIT0148.
        FREE IT_ZFIT0148.
        CLEAR WA_ZFIT0148.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
