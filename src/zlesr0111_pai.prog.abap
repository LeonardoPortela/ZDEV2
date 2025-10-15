*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'CLOCK'.
      LEAVE TO SCREEN 0100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.

      PERFORM F_ATUALIZA_TIME.
      IF ( VG_TST_LIM > VG_TST_ATUAL ).
        GO_CLOCK->INTERVAL = VG_TIME_INTERVAL.
        CALL METHOD GO_CLOCK->RUN.
      ENDIF.

      "IF OBJ_ALV_0100 IS NOT INITIAL.
      "  CALL METHOD OBJ_ALV_0100->FREE.
      "  CALL METHOD CL_GUI_CFW=>FLUSH.
      "  FREE: OBJ_ALV_0100.
      "ENDIF.

      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.

  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0101 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF ( VG_CNPJ_TRANSP IS INITIAL ) AND ( VG_CPF_TRANSP IS INITIAL ).
        MESSAGE 'Informe o CNPJ ou CPF' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( VG_CNPJ_TRANSP IS NOT INITIAL ) AND ( VG_CPF_TRANSP IS NOT INITIAL ).
        MESSAGE 'Informe o CNPJ ou CPF' TYPE 'S'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja atribuir esse CNPJ/CPF para a(s) NF-e(s) selecionada(s)?'
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

      LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

        READ TABLE IT_SAIDA_0100_01 INTO WA_SAIDA_0100_01 INDEX WA_SEL_ROWS-INDEX.

        CHECK ( SY-SUBRC = 0 ).

        CHECK ( WA_SAIDA_0100_01-CHAVE_NFE IS NOT INITIAL ) OR ( WA_SAIDA_0100_01-CHAVE_NFF IS NOT INITIAL ).

*        IF WA_SAIDA_0100_01-CNPJ_CPF_TRANSP_INF IS INITIAL. "CNPJ/CPF Informado manualmente, pode ser alterado novamente.
*          CHECK ( WA_SAIDA_0100_01-CNPJ_TRANSP IS INITIAL ) AND ( WA_SAIDA_0100_01-CPF_TRANSP IS INITIAL ).
*        ENDIF.

        WA_SAIDA_0100_01-CNPJ_TRANSP = VG_CNPJ_TRANSP.
        WA_SAIDA_0100_01-CPF_TRANSP  = VG_CPF_TRANSP.

        UPDATE ZLEST0142 SET CNPJ_CPF_TRANSP_INF = 'X' " Informado Manual
                             CNPJ_TRANSP         = VG_CNPJ_TRANSP
                             CPF_TRANSP          = VG_CPF_TRANSP
                       WHERE CHAVE_NFE = WA_SAIDA_0100_01-CHAVE_NFE
                         AND CHAVE_NFF = WA_SAIDA_0100_01-CHAVE_NFF.

        MODIFY IT_SAIDA_0100_01 FROM WA_SAIDA_0100_01 INDEX WA_SEL_ROWS-INDEX.

      ENDLOOP.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE CANCEL_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0102 INPUT.

  DATA: IT_RSPARAMS   TYPE TABLE OF RSPARAMS,
        WA_RSPARAMS   TYPE RSPARAMS.


  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF VG_DTINI_PROC_ROM IS INITIAL.
        MESSAGE 'Informe uma data de Inicio de processamento!' TYPE 'S'.
        EXIT.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
        ID 'BUKRS' FIELD  P_BUKRS-LOW
        ID 'ACTVT' FIELD '03'.    "Visualização

      CASE SY-SUBRC.
        WHEN 0.
          "  tem autorização!
        WHEN 4.
          MESSAGE 'Sem autorização para esta empresa' TYPE 'I'.
          EXIT.
        WHEN 12.
          MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
          EXIT.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      IF ( VG_DTINI_PROC_ROM < WG_PAR_CCT-DT_INI_ROM ) OR ( WG_PAR_CCT-DT_INI_ROM IS INITIAL ).
        MESSAGE 'Data de processamento não permitida!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR: WA_RSPARAMS.
      WA_RSPARAMS-KIND    = 'S'.
      WA_RSPARAMS-SIGN    = 'I'.
      WA_RSPARAMS-OPTION  = 'EQ'.

      WA_RSPARAMS-SELNAME = 'P_BUKRS'.
      WA_RSPARAMS-LOW     = P_BUKRS-LOW.
      APPEND WA_RSPARAMS TO IT_RSPARAMS.

      WA_RSPARAMS-SELNAME = 'P_BRANCH'.
      WA_RSPARAMS-LOW     = P_BRANCH-LOW.
      APPEND WA_RSPARAMS TO IT_RSPARAMS.

      WA_RSPARAMS-SELNAME = 'P_DTINI'.
      WA_RSPARAMS-LOW     = VG_DTINI_PROC_ROM.
      APPEND WA_RSPARAMS TO IT_RSPARAMS.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Confirma o processamento dos romaneios?'
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

      SUBMIT ZLESR0113 WITH SELECTION-TABLE IT_RSPARAMS
                        AND RETURN.

      MESSAGE 'Processamento concluído!' TYPE 'S'.


    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
