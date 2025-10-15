*----------------------------------------------------------------------*
***INCLUDE LZGFS_DEFENSIVOSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: pt_extab, ok_code.

  IF l_edit = abap_false OR t_lotes[] IS INITIAL.
    APPEND VALUE #( fcode = 'SALVAR' ) TO pt_extab.
  ENDIF.

  SET PF-STATUS 'ZSDT0112' EXCLUDING pt_extab.
  SET TITLEBAR 'ZSDT0112'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  w_stable = 'XX'.

  CASE ok_code.

    WHEN 'SALVAR'.
      PERFORM f_salvar_lote CHANGING l_erro.

      CHECK l_erro = abap_false.

      CALL METHOD g_custom_container->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

    WHEN 'VOLTAR'.
      l_erro = abap_true.

      CALL METHOD g_custom_container->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      l_erro = abap_true.

      CALL METHOD g_custom_container->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  CALL METHOD g_grid2->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDMODULE.
