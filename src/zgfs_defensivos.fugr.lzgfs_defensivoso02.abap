*----------------------------------------------------------------------*
***INCLUDE LZGFS_DEFENSIVOSO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  FREE: ok_code.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5821'.

  PERFORM mostra_pop_5821.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.

  CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
    EXPORTING
      chave = vg_cg_para_rom.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  w_stable = 'XX'.

  CASE ok_code.

    WHEN 'SALVAR'.
      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          chave = vg_cg_para_rom.

      CALL METHOD g_custom_container_pop_5821->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

    WHEN 'VOLTAR'.
      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          chave = vg_cg_para_rom.

      CALL METHOD g_custom_container_pop_5821->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          chave = vg_cg_para_rom.

      CALL METHOD g_custom_container_pop_5821->free.
      CALL METHOD g_custom_container2->free.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
