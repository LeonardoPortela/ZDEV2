*----------------------------------------------------------------------*
***INCLUDE ZSDR0051_PBO .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIAR_OBJETOS OUTPUT.

  IF OBJ_CONTAINER_XML IS INITIAL.

    PERFORM REFRESH_OBJETOS.
    PERFORM CRIAR_FIELD_CATALOG_XML.

    CREATE OBJECT OBJ_CONTAINER_XML
      EXPORTING
        CONTAINER_NAME = 'CC_XML'.

    CREATE OBJECT OBJ_ALV_XML
      EXPORTING
        I_PARENT = OBJ_CONTAINER_XML.

    CREATE OBJECT OBJ_TOOLBAR_XML
      EXPORTING
        IO_ALV_GRID = OBJ_ALV_XML.

    GS_LAYOUT-ZEBRA      = 'X'.
    GS_VARIANT-REPORT  = SY-REPID.

    SET HANDLER: OBJ_TOOLBAR_XML->ON_TOOLBAR          FOR OBJ_ALV_XML,
                 OBJ_TOOLBAR_XML->HANDLE_USER_COMMAND FOR OBJ_ALV_XML.

    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

    CALL METHOD OBJ_ALV_XML->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        IS_VARIANT           = GS_VARIANT
      CHANGING
        IT_FIELDCATALOG      = IT_FCAT
        IT_OUTTAB            = IT_SAIDA_XML.

    CALL METHOD OBJ_ALV_XML->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD OBJ_ALV_XML->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ELSE.
    CALL METHOD OBJ_ALV_XML->REFRESH_TABLE_DISPLAY
       EXPORTING
         IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.                 " CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
*  SET TITLEBAR 'xxx'.

  IF ( WA_DOC_ELET-BRANCH IS NOT INITIAL ) AND
     ( WA_DOC_ELET-NU_CNPJ_DEST IS NOT INITIAL ).

    READ TABLE IT_J_1BBRANCH INTO WA_J_1BBRANCH WITH KEY BUKRS  = WA_DOC_ELET-BUKRS
                                                         BRANCH = WA_DOC_ELET-BRANCH
                                                         STCD1  = WA_DOC_ELET-NU_CNPJ_DEST.
    IF SY-SUBRC = 0.
      WA_DOC_ELET-BUKRS       = WA_J_1BBRANCH-BUKRS.
      WA_DOC_ELET-DESTINO_IE  = WA_J_1BBRANCH-STATE_INSC.
      VG_BLOQ_FILIAL          = 'X'.
    ENDIF.

  ENDIF.


ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_FILIAL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_FILIAL OUTPUT.
*
*  LOOP AT SCREEN.
*
*    CASE SCREEN-NAME.
*      WHEN 'WA_DOC_ELET-BRANCH'.
*
*        IF VG_BLOQ_FILIAL IS NOT INITIAL.
*          SCREEN-INPUT = 0.
*        ELSE.
*          SCREEN-INPUT = 1.
*        ENDIF.
*
*        MODIFY SCREEN.
*
*    ENDCASE.
*
*  ENDLOOP.

ENDMODULE.
