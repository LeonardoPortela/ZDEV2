*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0100I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  gv_ucomm = sy-ucomm.

  CASE gv_ucomm.
    WHEN 'OK'.

      PERFORM f_processar_apos_ok.

      IF gv_error = abap_false.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'ENTER'.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_FIELDS  INPUT
*&---------------------------------------------------------------------*
MODULE update_fields INPUT.

  MODIFY gt_saldos
    FROM zsds_tela_calculo_alv
      INDEX grd_9000-current_line
        TRANSPORTING saldo_usado.

  PERFORM f_atualiza_campos.

  PERFORM f_atualiza_vlr_fin.


ENDMODULE.
