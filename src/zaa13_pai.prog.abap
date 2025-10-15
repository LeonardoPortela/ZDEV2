*&---------------------------------------------------------------------*
*&  Include           ZAA13_PAI
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  DATA: LT_CELLTAB        TYPE LVC_T_STYL.
  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,                                  "Tabela de linhas selecionadas na alv de saída
        WA_SELECTED_ROWS TYPE LVC_S_ROW.

  CASE SY-UCOMM.
    WHEN 'ENVIAR'. "Enviar baixa de Imobilizado para aprovação.

      IF ( WA_ALV_0001 IS NOT INITIAL ).

        CALL METHOD WA_ALV_0001->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SELECTED_ROWS.

        CHECK ( IT_SELECTED_ROWS IS NOT INITIAL ).

        LOOP AT IT_SELECTED_ROWS INTO DATA(WL_SELECTED_ROWS).

          READ TABLE TG_SAIDA[] INTO DATA(WL_SAIDA) INDEX WL_SELECTED_ROWS-INDEX.

          IF ( WL_SAIDA-ESTADO_BEM  IS INITIAL ) OR ( WL_SAIDA-RESPONSAVEL IS INITIAL ) OR ( WL_SAIDA-CHECK_ANEXO IS INITIAL ).
            MESSAGE 'Campos ESTADO DO BEM/RESPONSÁVEL/ANEXO obrigatórios' TYPE 'I'.
            EXIT.
          ENDIF.

          IF ( WL_SAIDA-STATUS EQ 'A' ).
            MESSAGE 'Solicitação já aprovada!' TYPE 'I'.
            EXIT.
          ELSEIF ( WL_SAIDA-STATUS EQ 'E' ).
            MESSAGE 'Solicitação já enviada!' TYPE 'I'.
            EXIT.
          ELSE.
            IF WL_SAIDA-status = 'R'. ""128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
               CLEAR: WL_SAIDA-status.
            ENDIF.
            APPEND WL_SAIDA TO TG_ENVIA[].
          ENDIF.

        ENDLOOP.

        CLEAR: WL_SELECTED_ROWS, IT_SELECTED_ROWS[].

        CHECK ( TG_ENVIA[] IS NOT INITIAL ).
        PERFORM ENVIA_SOLICITACAO.

      ELSE.

        MESSAGE 'Sem informações!' TYPE 'I'.
        EXIT.

      ENDIF.                      "/ENVIA SOLICITAÇÃO

    WHEN 'EDITAR'.

      CHECK ( WA_ALV_0001 IS NOT INITIAL ).

      CALL METHOD WA_ALV_0001->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS.

      CHECK ( IT_SELECTED_ROWS[] IS NOT INITIAL ).

      LOOP AT IT_SELECTED_ROWS INTO WL_SELECTED_ROWS.

        READ TABLE TG_SAIDA[] ASSIGNING <WG_SAIDA> INDEX WL_SELECTED_ROWS-INDEX.

        IF ( <WG_SAIDA>-STATUS EQ 'A' ).
          MESSAGE 'Solicitação de Baixa aprovada. Impossível editar!' TYPE 'I'.
          EXIT.
        ELSE.
          <WG_SAIDA>-EDICAO = 'X'.
          PERFORM FILL_CELLTAB USING: 'D' 'ESTADO_BEM' CHANGING LT_CELLTAB,
                                      'D' 'RESPONSAVEL' CHANGING LT_CELLTAB,
                                      'D' 'ANEXO'  CHANGING LT_CELLTAB.

          <WG_SAIDA>-CELLSTYLES = LT_CELLTAB.
        ENDIF.
      ENDLOOP.

      CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.

    WHEN 'EXCLUIR'.

      CHECK ( WA_ALV_0001 IS NOT INITIAL ).

      CALL METHOD WA_ALV_0001->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS.

      CHECK ( IT_SELECTED_ROWS[] IS NOT INITIAL ).

      LOOP AT IT_SELECTED_ROWS INTO WL_SELECTED_ROWS.

        READ TABLE TG_SAIDA[] ASSIGNING <WG_SAIDA> INDEX WL_SELECTED_ROWS-INDEX.

        IF ( <WG_SAIDA>-STATUS EQ 'A' ).
          MESSAGE 'Solicitação de Baixa aprovada. Impossível Eliminar!' TYPE 'I'.
          EXIT.
        ELSE.

          DATA: VL_ANSWER TYPE C.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR              = 'Confirmação'
              TEXT_QUESTION         = 'Confirma exclusão da Solicitação?'
              TEXT_BUTTON_1         = 'Sim'
              ICON_BUTTON_1         = 'ICON_CHECKED'
              DISPLAY_CANCEL_BUTTON = 'X'
            IMPORTING
              ANSWER                = VL_ANSWER.

          IF ( VL_ANSWER EQ '1' ).

            DELETE FROM ZAA007 WHERE BUKRS EQ <WG_SAIDA>-BUKRS
                                 AND WERKS EQ <WG_SAIDA>-WERKS
                                 AND ANLN1 EQ <WG_SAIDA>-ANLN1.

            UPDATE ZAA008
              SET DT_ESTORNO    = SY-DATUM
                  HR_ESTORNO    = SY-UZEIT
                  USER_ESTORNO  = SY-UNAME
              WHERE BUKRS        EQ <WG_SAIDA>-BUKRS
                AND WERKS        EQ <WG_SAIDA>-WERKS
                AND ANLN1        EQ <WG_SAIDA>-ANLN1
                AND USER_ESTORNO EQ ''.

            COMMIT WORK.

            IF ( SY-SUBRC EQ 0 ).
              PERFORM BUSCA_DADOS.
              CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.
              MESSAGE 'Solicitação eliminada com Sucesso!' TYPE 'S'.
            ENDIF.

          ELSE.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_SCREEN INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
