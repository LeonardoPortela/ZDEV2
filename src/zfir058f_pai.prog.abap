*&---------------------------------------------------------------------*
*&  Include           ZFIR058_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  DATA: VL_ERROR    TYPE C,
        VL_COMP     TYPE C,
        VL_DOC_COMP TYPE BSAD-BELNR.

  CASE SY-UCOMM.
    WHEN 'MONI_RC_VR'.
      PERFORM F_CALL_SCREEN_0120.
    WHEN 'PROC_DOC'.
      CLEAR: VL_COMP.

      PERFORM F_SELECIONAR_DADOS.
      CHECK VG_NOT_FOUND IS INITIAL.
      PERFORM: F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.

      IF P_CPLIB IS NOT INITIAL. "Compensar Adiantamentos com Saldo Zerado
        LOOP AT IT_SAIDA_0100 INTO WA_SAIDA_0100 WHERE ST_COMP = '1' OR ST_COMP = '3'.
          PERFORM F_GET_PART_COMP USING WA_SAIDA_0100.
          PERFORM F_BAPI_F51 USING ABAP_FALSE
                          CHANGING WA_SAIDA_0100
                                   VL_ERROR
                                   VL_DOC_COMP.
          VL_COMP = 'X'.
        ENDLOOP.

        IF VL_COMP IS NOT INITIAL.
          PERFORM F_RENOVAR_CONS.
        ENDIF.
      ENDIF.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.
  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      PERFORM F_COMPENSAR_ADT USING WA_SAIDA_0100.
    WHEN 'APLIC_TEXT_DEF'.
      PERFORM F_APLIC_TEXT_DEF.
    WHEN 'BTN_DESC'.
      PERFORM F_DESC USING WA_SAIDA_0100.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE USER_COMMAND_0120 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
