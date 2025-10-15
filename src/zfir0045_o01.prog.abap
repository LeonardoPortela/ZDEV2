*----------------------------------------------------------------------*
***INCLUDE ZFIR0045_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  APPEND C_DELDOC TO FCODE.
  IF WG_ACAO IS INITIAL OR WG_ACAO = C_DISPLA.
    APPEND C_SAVE TO FCODE.
    IF XMODIF = 'X' OR WG_ACAO IS INITIAL.
      APPEND C_MODIF TO FCODE.
    ENDIF.
  ELSEIF XMODIF = 'X' .
    APPEND C_MODIF TO FCODE.
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA_TELA OUTPUT.
  IF WG_ACAO IS INITIAL.
    REFRESH: TG_FIELDS.

    PERFORM TRATA_CAMPOS USING SPACE
                               'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_1       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.                 " INICIALIZA_TELA  OUTPUT

*----------------------------------------------------------------------*
*  MODULE TRATA_FIELDS OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.
ENDMODULE.                    "TRATA_FIELDS OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT TYPE CNTL_SIMPLE_EVENT,
            EVENTS TYPE CNTL_SIMPLE_EVENTS,
            TL_FILTER           TYPE LVC_T_FILT,
            WL_FILTER           TYPE LVC_S_FILT,
            TL_FUNCTION         TYPE UI_FUNCTIONS,
            WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE,
            LT_F4               TYPE LVC_T_F4    WITH HEADER LINE.


  IF G_CUSTOM_CONTAINER IS INITIAL.

*    WA_LAYOUT-ZEBRA      = 'X'.
*    WA_LAYOUT-NO_ROWMOVE = 'X'.
*    WA_LAYOUT-NO_ROWINS  = 'X'.
*    WA_LAYOUT-NO_ROWMARK = SPACE.
*    WA_LAYOUT-SEL_MODE   = 'A'.
*    WA_LAYOUT-CWIDTH_OPT   = 'X'.
*    WA_LAYOUT-BOX_FNAME    = 'MARK'.

    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = space.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-CWIDTH_OPT   = 'X'.
    WA_LAYOUT-BOX_FNAME    = 'MARK'.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER_1.

    PERFORM MONTAR_LAYOUT.


    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.


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


    PERFORM BUSCA_DADOS.

    WA_LAYOUT-NO_TOOLBAR = ''.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ITENS[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    LT_F4-FIELDNAME = 'BSCHL'.
    LT_F4-REGISTER = 'X'.
    LT_F4-GETBEFORE = 'X'.
    LT_F4-CHNGEAFTER ='X'.
    APPEND LT_F4.

    LT_F4-FIELDNAME = 'COD_OPER'.
    LT_F4-REGISTER = 'X'.
    LT_F4-GETBEFORE = 'X'.
    LT_F4-CHNGEAFTER ='X'.
    APPEND LT_F4.

    LT_F4-FIELDNAME = 'HKONT'.
    LT_F4-REGISTER = 'X'.
    LT_F4-GETBEFORE = 'X'.
    LT_F4-CHNGEAFTER ='X'.

*    LT_F4-FIELDNAME = 'TP_OPERACAO'.
*    LT_F4-REGISTER = 'X'.
*    LT_F4-GETBEFORE = 'X'.
*    LT_F4-CHNGEAFTER ='X'.

    APPEND LT_F4.

    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_ONF4         FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 2
        HEIGHT = 100.
  ELSE.
    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
