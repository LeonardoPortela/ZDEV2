*----------------------------------------------------------------------*
***INCLUDE LZGFSD0004I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  gv_ucomm_9000 = sy-ucomm.

  CASE gv_ucomm_9000.
    WHEN 'CHK'.
      PERFORM f_filtra_alv_01.
    WHEN 'ENTR'.

      PERFORM f_verificar_campos USING gv_erro_9000.

      IF gv_erro_9000 IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'ESC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
