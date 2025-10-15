*&---------------------------------------------------------------------*
*&  Include           ZLESR0107_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'CALL_ZLES0145'.
      TRY.
        CALL TRANSACTION 'ZLES0145' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
      CATCH CX_SY_AUTHORIZATION_ERROR.
      ENDTRY.
    WHEN 'GERAR_REG'.

      IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_OPERACAO NE C_EDIT ).
        CALL METHOD OBJ_ALV_0100->FREE.
        CALL METHOD CL_GUI_CFW=>FLUSH.
        FREE: OBJ_ALV_0100.
      ENDIF.

      VG_OPERACAO = C_EDIT.

      PERFORM F_GERAR_LINHAS.
    WHEN 'CONSULTAR'.

      IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_OPERACAO NE C_VIEW ).
        CALL METHOD OBJ_ALV_0100->FREE.
        CALL METHOD CL_GUI_CFW=>FLUSH.
        FREE: OBJ_ALV_0100.
      ENDIF.

      VG_OPERACAO = C_VIEW.

      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSAR_DADOS.
      LEAVE TO SCREEN 0100.
    WHEN 'SAVE'.

      IF VG_OPERACAO NE C_EDIT.
        MESSAGE 'Só é possível salvar em modo de edição!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_OPERACAO NE C_VIEW ).
        CALL METHOD OBJ_ALV_0100->FREE.
        CALL METHOD CL_GUI_CFW=>FLUSH.
        FREE: OBJ_ALV_0100.
      ENDIF.

      PERFORM F_SALVAR_REG.

    WHEN 'ANEXAR'.
      DATA: V_OBJ_KEY  TYPE SIBFLPORB-INSTID.
      DATA: V_NR_ANEXO TYPE P DECIMALS 0.

      IF VG_OPERACAO EQ C_EDIT.
        MESSAGE 'Vinculação não permitida em modo de edição!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF WA_CAB_0100-BUKRS IS INITIAL.
        MESSAGE 'Informe a empresa!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF WA_CAB_0100-LIFNR IS INITIAL.
        MESSAGE 'Informe o fornecedor!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF WA_CAB_0100-DT_INI IS INITIAL.
        MESSAGE 'Informe a data inicial!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

      CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SEL_ROWS.

      IF IT_SEL_ROWS[] IS INITIAL.
        MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT MAX( INSTID_A )
        FROM SRGBTBREL INTO @DATA(_MAX_INSTID_A)
       WHERE RELTYPE  EQ 'ATTA'
         AND TYPEID_A EQ 'ZLES0144-G'.

      IF ( _MAX_INSTID_A IS INITIAL ).
        V_NR_ANEXO = 1.
      ELSE.
        V_NR_ANEXO = _MAX_INSTID_A.
        ADD 1 TO V_NR_ANEXO.
      ENDIF.

      IF WA_CAB_0100-DT_FIM IS INITIAL.
        WA_CAB_0100-DT_FIM = WA_CAB_0100-DT_INI.
      ENDIF.

      WA_CAB_0100-ANEXO_LINK = V_NR_ANEXO.

      PERFORM F_CRIAR_ANEXO USING WA_CAB_0100-ANEXO_LINK
                                  'G'. "Geral.


      LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

        CHECK SY-SUBRC = 0.

        UPDATE ZLEST0140 SET ANEXO_REF = WA_CAB_0100-ANEXO_LINK
         WHERE BUKRS = WA_SAIDA_0100-BUKRS
           AND DATA  = WA_SAIDA_0100-DATA.

      ENDLOOP.

      COMMIT WORK.

      LEAVE TO SCREEN 0100.

      WHEN 'EXIBIR'.
      PERFORM LOG_PROCESSAMENTO.

  ENDCASE.

ENDMODULE.
