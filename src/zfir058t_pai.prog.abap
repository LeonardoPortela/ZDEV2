*&---------------------------------------------------------------------*
*&  Include           ZFIR058_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  DATA: vl_error    TYPE c,
        vl_comp     TYPE c,
        vl_doc_comp TYPE bsad-belnr.

  CASE sy-ucomm.
    WHEN 'MONI_RC_VR'.
      PERFORM f_call_screen_0120.
    WHEN 'PROC_DOC'.
      CLEAR: vl_comp.
      DATA  verro(1).
      CLEAR verro.
      LOOP AT p_bukrs .
        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'ACTVT' FIELD '03'
           ID 'BUKRS' FIELD p_bukrs-low.
        IF sy-subrc NE 0.
          MESSAGE | Sem acesso a empresa: { p_bukrs-low } | TYPE 'I'.
          verro = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF verro IS INITIAL.
        PERFORM f_selecionar_dados.
        CHECK vg_not_found IS INITIAL.
        PERFORM: f_processa_dados,
                 f_refresh_alv USING '0100'.

        IF p_cplib IS NOT INITIAL. "Compensar Adiantamentos com Saldo Zerado
          LOOP AT it_saida_0100 INTO wa_saida_0100 WHERE st_comp = '1' OR st_comp = '3'.
            PERFORM f_get_part_comp USING wa_saida_0100.
            PERFORM f_bapi_f51 USING abap_false
                            CHANGING wa_saida_0100
                                     vl_error
                                     vl_doc_comp.
            vl_comp = 'X'.
          ENDLOOP.

          IF vl_comp IS NOT INITIAL.
            PERFORM f_renovar_cons.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

MODULE user_command_0110 INPUT.
  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      PERFORM f_compensar_adt USING wa_saida_0100.
    WHEN 'APLIC_TEXT_DEF'.
      PERFORM f_aplic_text_def.
    WHEN 'BTN_DESC'.
      PERFORM f_desc USING wa_saida_0100.
    WHEN 'CANCEL'.
      vg_cancel = 'X'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE user_command_0120 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
