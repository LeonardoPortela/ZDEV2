*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE pai INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING ''.
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

*-CS2025000249-17.04.2025-#173311-JT-inicio
      IF p_nosave = abap_false.
        MODIFY (p_db_tab) FROM <fs_wa_registro_manter>.
      ELSE.
        sy-subrc = 0.
      ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

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

MODULE pai_manter_0138 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      _error = abap_false.
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

      MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM f_exit_0016 USING sy-ucomm CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0001 INPUT.
  PERFORM f_exit_0017 USING '0001'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0002 INPUT.
  PERFORM f_exit_0017 USING '0002'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0003 INPUT.
  PERFORM f_exit_0017 USING '0003'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0004 INPUT.
  PERFORM f_exit_0017 USING '0004'.
ENDMODULE.

MODULE pai_manter_search INPUT.

  CASE sy-ucomm.
    WHEN 'SEARCH'.

      CLEAR: gwa_cond_search.

      DATA(_error_search) = abap_false.
      PERFORM f_exit_0019 USING <fs_wa_registro_search> CHANGING _error_search.

      CHECK _error_search IS INITIAL.

      PERFORM: f_selecionar_dados,
               f_processa_dados.

      CALL SCREEN 0001.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
