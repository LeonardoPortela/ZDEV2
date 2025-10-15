*&---------------------------------------------------------------------*
*&  Include           ZFIR063_CLASS
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

        DELETE FROM ZFIT0140 WHERE TXJCD = WA_SAIDA_0100-TXJCD.

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

        CLEAR: ZFIT0140.

        ZFIT0140-DT_REGISTRO = SY-DATUM.
        ZFIT0140-HR_REGISTRO = SY-UZEIT.
        ZFIT0140-US_REGISTRO = SY-UNAME.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 55 10 .

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

        CLEAR: ZFIT0140.

        MOVE-CORRESPONDING WA_SAIDA_0100 TO ZFIT0140.

        VG_OPERACAO = E_UCOMM.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 55 10 .

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
