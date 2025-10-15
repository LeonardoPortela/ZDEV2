*----------------------------------------------------------------------*
***INCLUDE ZMMR0045_STATUS_0100O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: w_fcode TYPE slis_extab,
        t_fcode TYPE slis_t_extab.

  CLEAR ok_code.

  IF p_submit = abap_false.
    APPEND VALUE #( fcode = 'VOLTAR'  ) TO t_fcode[].
  ELSE.
    APPEND VALUE #( fcode = 'REFRESH' ) TO t_fcode[].
  ENDIF.

  SET PF-STATUS 'ZMMR0045'       EXCLUDING t_fcode.
  SET TITLEBAR 'ZMMR0045'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.

    WHEN 'REFRESH'.
      FREE: t_rows.

      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      CALL METHOD cl_container_95->free.
      FREE: g_grid, g_custom_container, obj_dyndoc_id, cl_container_95.
*     CALL METHOD obj_dyndoc_id->custom_container->free( ).
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.
