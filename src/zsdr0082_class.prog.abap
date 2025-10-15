*&---------------------------------------------------------------------*
*&  Include           ZSDR0082_CLASS
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

        DELETE FROM ZSDT0155 WHERE BUKRS   = WA_SAIDA_0100-BUKRS
                               AND AUART   = WA_SAIDA_0100-AUART
                               AND TP_TRIB = WA_SAIDA_0100-TP_TRIB
                               AND DT_INI  = WA_SAIDA_0100-DT_INI.

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

        CLEAR: ZSDT0155.

        ZSDT0155-DT_REGISTRO = SY-DATUM.
        ZSDT0155-HR_REGISTRO = SY-UZEIT.
        ZSDT0155-US_REGISTRO = SY-UNAME.

        CALL SCREEN 0110 STARTING AT 10 05 . "ENDING AT 62 20 .

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

        IF ( WA_SAIDA_0100-CANCELADO = 'X' ) AND ( WA_SAIDA_0100-DT_INI < SY-DATUM ).
          MESSAGE 'Parâmetro já foi cancelado!' TYPE 'S'.
          EXIT.
        ENDIF.

        CLEAR: ZSDT0155.

        MOVE-CORRESPONDING WA_SAIDA_0100 TO ZSDT0155.

        VG_OPERACAO = E_UCOMM.

        CALL SCREEN 0110 STARTING AT 10 05 ."ENDING AT 62 20 .

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
