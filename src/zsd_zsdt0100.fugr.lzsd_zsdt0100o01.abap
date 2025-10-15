*----------------------------------------------------------------------*
***INCLUDE LZSD_ZSDT0100O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SCREEN_ALV_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE screen_alv_9000 OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'ZSDS_TELA_CALCULO_ALV-SALDO_USADO'.

      IF zsds_tela_calculo_alv-vlr_ttl_disp IS INITIAL.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.


      IF zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn >= zsds_tela_calculo_aprovacao-vlr_ttl_lib.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.


    ENDIF.

  ENDLOOP.

ENDMODULE.
