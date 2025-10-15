*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE pai INPUT.
FREE MEMORY ID 'ZLESR0154'.
  FREE: l_leave.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING ''.
      when 'TRANSB'.
IF p_submit = 'ZLESR0154'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0154 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.

      ENDIF.
when 'ARMZ'.
  IF p_submit = 'ZLESR0156'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0154 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0156'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
  when 'PREST_SER'.
    IF p_submit = 'ZLESR0155'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0155 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.

MODULE pai_manter INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      DATA(_error) = abap_false.
      PERFORM f_exit_0002 USING <fs_wa_registro_manter> CHANGING _error.

      CHECK _error IS INITIAL.

      IF vg_operacao EQ c_novo.

        CLEAR: <fs_wa_saida_tmp>.

        MOVE-CORRESPONDING  <fs_wa_registro_manter> TO <fs_wa_saida_tmp>.

        "Criar condição dinamica
        PERFORM f_get_cond_chave USING '<FS_WA_SAIDA_TMP>'
                              CHANGING vg_cond.

        CHECK vg_cond-where_tab[] IS NOT INITIAL.

        SELECT SINGLE *
          FROM (p_db_tab) INTO <fs_wa_registro_manter_tmp>
         WHERE (vg_cond-where_tab).

        IF sy-subrc = 0.
          MESSAGE 'Registro já cadastrado!' TYPE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      PERFORM f_exit_0003 CHANGING <fs_wa_registro_manter>.

      PERFORM f_exit_0011 CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

      MODIFY (p_db_tab) FROM <fs_wa_registro_manter>.

      IF sy-subrc = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM f_exit_0016 USING sy-ucomm CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_MANTER_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_manter_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
