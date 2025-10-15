*----------------------------------------------------------------------*
***INCLUDE LZSD_SIGAM_INTERFACEI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  gv_9000_ucomm = sy-ucomm.

  CASE gv_9000_ucomm.
    WHEN 'SELECAOPR'.
      PERFORM f_redefine_param.
    WHEN 'PESQUISAR'.

      PERFORM f_pesquisar.

    WHEN 'NOVO'.

      PERFORM f_executa_busca_sigam.

    WHEN 'BACK'.
      gv_screen_status = 'B'.
      LEAVE TO SCREEN 0.
    WHEN 'LEAVE'.
      gv_screen_status = 'L'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      gv_screen_status = 'C'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  RADIO_CLICKED  INPUT
*&---------------------------------------------------------------------*
MODULE radio_clicked INPUT.

  PERFORM f_radio_clicked.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.

  gv_9100_ucomm = sy-ucomm.

  CASE gv_9100_ucomm.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9200 INPUT.

  gv_9200_ucomm = sy-ucomm.

  CASE gv_9200_ucomm.

    WHEN 'LOT'.

      PERFORM f_filtro_lote_alv.
      clear gt_0006[].
      PERFORM f_refresh_grid USING 'X' 'X'.

    WHEN 'SELECAOPR'.

      PERFORM f_redefine_param.

      zsde0004-bukrs = gv_param-bukrs.
      zsde0004-matnr = gv_param-matnr.

    WHEN 'BUSCAR'.

      PERFORM f_buscar_lote.
      PERFORM f_buscar_ordens USING space.
      "PERFORM f_filtro_lote_alv.

    WHEN 'PROCESSAR'.

      PERFORM f_processar.

    WHEN 'SAVE'.

      PERFORM f_valida_selecao.

      PERFORM f_call_param_transaction.

    WHEN 'BACK'.

      gv_screen_status = 'B'.
      LEAVE TO SCREEN 0.

    WHEN 'LEAVE'.

      gv_screen_status = 'L'.
      LEAVE PROGRAM.

    WHEN 'CANCEL'.

      gv_screen_status = 'C'.
      LEAVE PROGRAM.

  ENDCASE.

ENDMODULE.
