*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_CREATE.
    TY_TOOLBAR-FUNCTION  = C_NOVO.
    TY_TOOLBAR-TEXT      = 'Novo'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_CHANGE.
    TY_TOOLBAR-FUNCTION  = C_CHANGE.
    TY_TOOLBAR-TEXT      = 'Modificar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = C_DEL.
    TY_TOOLBAR-TEXT      = 'Deletar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: VAR_ANSWER TYPE C.

    CASE E_UCOMM.
      WHEN C_DEL.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF LINES( IT_SEL_ROWS ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Confirmação'
            TEXT_QUESTION         = 'Deseja realmente deletar o registro?'
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = ''
          IMPORTING
            ANSWER                = VAR_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.

        CHECK VAR_ANSWER EQ '1'.

        READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

        CHECK SY-SUBRC = 0.

        DELETE FROM ZSDT0169 WHERE CODIGO_RA = WA_SAIDA_0100-CODIGO_RA.

        IF SY-SUBRC = 0.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: F_SELECIONAR_DADOS,
                   F_PROCESSA_DADOS.

          LEAVE TO SCREEN 0100.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

      WHEN C_NOVO.

        VG_OPERACAO = E_UCOMM.

        CLEAR: ZSDT0169.

        ZSDT0169-DT_REGISTRO = SY-DATUM.
        ZSDT0169-HR_REGISTRO = SY-UZEIT.
        ZSDT0169-US_REGISTRO = SY-UNAME.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 90 09 .

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.

        LEAVE TO SCREEN 0100.

      WHEN C_CHANGE.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF LINES( IT_SEL_ROWS ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

        CHECK SY-SUBRC = 0.

        CLEAR: ZSDT0169.

        MOVE-CORRESPONDING WA_SAIDA_0100 TO ZSDT0169.

        VG_OPERACAO = E_UCOMM.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 90 09 .

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.

        LEAVE TO SCREEN 0100.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_ALV_TOOLBAR_0120 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = C_NOVO.
    TY_TOOLBAR-TEXT      = 'Inserir'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = C_DEL.
    TY_TOOLBAR-TEXT      = 'Deletar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: VAR_ANSWER TYPE C.

    CASE E_UCOMM.
      WHEN C_NOVO.
        CLEAR: WA_SAIDA_0120.
        APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
        PERFORM F_REFRESH_ALV USING '0120'.
      WHEN C_DEL.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0120->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF LINES( IT_SEL_ROWS ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Confirmação'
            TEXT_QUESTION         = 'Deseja realmente deletar o registro?'
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = ''
          IMPORTING
            ANSWER                = VAR_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.

        CHECK VAR_ANSWER EQ '1'.

        READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

        READ TABLE IT_SAIDA_0120 INTO WA_SAIDA_0120 INDEX WA_SEL_ROWS-INDEX.

        CHECK SY-SUBRC = 0.

        SELECT SINGLE *
          FROM ZSDT0167 INTO @DATA(_WL_0167)
         WHERE CODIGO_URF = @WA_SAIDA_0120-CODIGO_URF.
        IF ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0120-CODIGO_URF IS NOT INITIAL ).
          DELETE FROM ZSDT0167 WHERE CODIGO_URF = WA_SAIDA_0120-CODIGO_URF.
        ELSE.
          SY-SUBRC = 0.
        ENDIF.

        IF SY-SUBRC = 0.
          DELETE IT_SAIDA_0120 WHERE CODIGO_URF = WA_SAIDA_0120-CODIGO_URF.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: F_REFRESH_ALV USING '0120'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS LCL_ALV_TOOLBAR_0130 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = C_NOVO.
    TY_TOOLBAR-TEXT      = 'Inserir'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = C_DEL.
    TY_TOOLBAR-TEXT      = 'Deletar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: VAR_ANSWER TYPE C.

    CASE E_UCOMM.
      WHEN C_NOVO.
        CLEAR: WA_SAIDA_0130.
        APPEND WA_SAIDA_0130 TO IT_SAIDA_0130.
        PERFORM F_REFRESH_ALV USING '0130'.
      WHEN C_DEL.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0130->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF LINES( IT_SEL_ROWS ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Confirmação'
            TEXT_QUESTION         = 'Deseja realmente deletar o registro?'
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = ''
          IMPORTING
            ANSWER                = VAR_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.

        CHECK VAR_ANSWER EQ '1'.

        READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

        READ TABLE IT_SAIDA_0130 INTO WA_SAIDA_0130 INDEX WA_SEL_ROWS-INDEX.

        CHECK SY-SUBRC = 0.

        SELECT SINGLE *
          FROM ZSDT0168 INTO @DATA(_WL_0168)
         WHERE CODIGO_RA = @WA_SAIDA_0130-CODIGO_RA.
        IF ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0130-CODIGO_RA IS NOT INITIAL ).
          DELETE FROM ZSDT0168 WHERE CODIGO_RA = WA_SAIDA_0130-CODIGO_RA.
        ELSE.
          SY-SUBRC = 0.
        ENDIF.

        IF SY-SUBRC = 0.
          DELETE IT_SAIDA_0130 WHERE CODIGO_RA = WA_SAIDA_0130-CODIGO_RA.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: F_REFRESH_ALV USING '0130'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_EVENT_HANDLER_0100 IMPLEMENTATION.

  METHOD CATCH_HOTSPOT.

    CASE E_COLUMN_ID.
      WHEN 'X'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_EVENT_HANDLER_0120 IMPLEMENTATION.

  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.

  METHOD ON_DATA_CHANGED.

    DATA: IT_GOOD_CELLS_AUX TYPE LVC_T_MODI.

    IT_GOOD_CELLS_AUX = ER_DATA_CHANGED->MT_GOOD_CELLS.

    SORT IT_GOOD_CELLS_AUX BY ROW_ID.
    DELETE ADJACENT DUPLICATES FROM IT_GOOD_CELLS_AUX COMPARING ROW_ID.

    LOOP AT IT_GOOD_CELLS_AUX INTO DATA(LS_GOOD).
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'CK_MODIFY'
          I_VALUE     = 'X'.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.


CLASS LCL_EVENT_HANDLER_0130 IMPLEMENTATION.

  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.

  METHOD ON_DATA_CHANGED.

    DATA: IT_GOOD_CELLS_AUX TYPE LVC_T_MODI.

    IT_GOOD_CELLS_AUX = ER_DATA_CHANGED->MT_GOOD_CELLS.

    SORT IT_GOOD_CELLS_AUX BY ROW_ID.
    DELETE ADJACENT DUPLICATES FROM IT_GOOD_CELLS_AUX COMPARING ROW_ID.

    LOOP AT IT_GOOD_CELLS_AUX INTO DATA(LS_GOOD).
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'CK_MODIFY'
          I_VALUE     = 'X'.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
