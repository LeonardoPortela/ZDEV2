*----------------------------------------------------------------------*
***INCLUDE ZMMR0045_STATUS_0200O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  CLEAR ok_code2.

  SET PF-STATUS 'ZMMR0045_B'.

  PERFORM f_init_alv_parc.
  PERFORM f_init_alv_wf.
  PERFORM f_init_alv_item.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code2.

    WHEN 'VOLTAR'.
      CALL METHOD g_custom_container_item->free.
      FREE: g_grid_item, g_custom_container_item.
      LEAVE TO SCREEN 0.

    WHEN 'BTN_ANEXOS'.
      PERFORM f_exibir_anexos.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container_item->free.
      FREE: g_grid_item, g_custom_container_item.
      LEAVE TO SCREEN 0.

  ENDCASE.

  FREE ok_code2.

ENDMODULE.
