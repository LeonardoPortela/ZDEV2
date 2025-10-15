*&---------------------------------------------------------------------*
*&  Include           ZLESR0119_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_OKAY.
    TY_TOOLBAR-FUNCTION  = C_BAIXAR.
    TY_TOOLBAR-TEXT      = 'Baixar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_CANCEL.
    TY_TOOLBAR-FUNCTION  = C_CANC_BAIXA.
    TY_TOOLBAR-TEXT      = 'Cancelar Baixar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: VAR_ANSWER TYPE C.

    CASE E_UCOMM.
      WHEN C_CANC_BAIXA.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
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

        DATA(_DEL_REG) = ''.
        LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

          READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.
          CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0100-MOTIVO_BAIXA IS NOT INITIAL ).

          DELETE FROM ZLEST0158 WHERE CH_REFERENCIA = WA_SAIDA_0100-CH_REFERENCIA.
          IF SY-SUBRC = 0.
            _DEL_REG = 'X'.
          ENDIF.
        ENDLOOP.

        IF _DEL_REG IS NOT INITIAL.
          MESSAGE 'Baixas removidas com sucesso!' TYPE 'S'.
          PERFORM: F_SELECIONAR_DADOS,
                   F_PROCESSA_DADOS.

          LEAVE TO SCREEN 0100.
        ENDIF.

      WHEN C_BAIXAR.

        CLEAR: ZLEST0158.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 130 05 .

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.

        LEAVE TO SCREEN 0100.

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
