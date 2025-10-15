*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMI02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CLEAR: l_clear.
  CASE ok_code.
    WHEN '&SALVAR'.
      CLEAR l_ok.
      CALL METHOD g_grid->check_changed_data.

      PERFORM f_valida_dados USING l_ok.

      IF l_ok = abap_true.
        PERFORM f_grava_transp.

        CALL METHOD g_custom_container->free.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&ELIMINAR'.
      CALL METHOD g_grid->check_changed_data.

      PERFORM f_elimina_dados.

      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN '&CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
*** US - 92467 - CBRAND - Inicio
    WHEN 'ATUALIZA_MODAL'.
      g_safra     = l_safra_ordem_car.
      g_nr_ordem  = l_nro_ordem_car.
      PERFORM atualiza_campos_modal.

    WHEN 'CLEAR_MODAL'.
      REFRESH: t_transp[].
      l_clear = 'X'.
*** US - 92467 - CBRAND - Inicio
  ENDCASE.
  CLEAR ok_code.
* ok_code = 'XXXX'.  "*-CS2024000522-12.09.2024-JT-#152417-inicio

ENDMODULE.

*-CS2024000522-12.09.2024-JT-#152417-inicio
MODULE f_atualiza_campos_modal.

  CHECK l_clear = abap_false.

  FREE: t_transp.

  g_safra     = l_safra_ordem_car.
  g_nr_ordem  = l_nro_ordem_car.

  PERFORM atualiza_campos_modal.

ENDMODULE.
*-CS2024000522-12.09.2024-JT-#152417-fim

*&---------------------------------------------------------------------*
*&      Module  F_COD_MOTORISTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_cod_motorista INPUT.

  FREE: l_nome_motorista,
        l_cpf_motorista.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_cod_motorista
    IMPORTING
      output = l_cod_motorista.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = l_cod_motorista
      p_partype    = 'V'
    CHANGING
      wa_info_part = w_info_k
      wa_info_c    = w_info_c.

  IF w_info_k-ktokk = 'ZMOT'.
    l_cod_motorista  = w_info_k-lifnr.
    l_nome_motorista = w_info_k-name1.

    IF      w_info_k-stcd1 IS NOT INITIAL.
      l_cpf_motorista = w_info_k-stcd1.
    ELSEIF w_info_k-stcd2 IS NOT INITIAL.
      l_cpf_motorista = w_info_k-stcd2.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CLEAR_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_screen INPUT.
  IF l_clear = 'X'.
    FREE:  l_cod_motorista,
           l_nome_motorista,
           l_cpf_motorista,
           l_peso_bruto,
           l_peso_tara,
           l_safra_ordem_car,
           l_nro_ordem_car.
  ENDIF.
ENDMODULE.
