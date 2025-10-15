*----------------------------------------------------------------------*
***INCLUDE LZGFSD001I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  gv_ucomm_9000 = sy-ucomm.

  CASE gv_ucomm_9000.

    WHEN 'CHK'.

      PERFORM f_check_event.

    WHEN 'OK'.

      PERFORM f_get_motivo_9000.

      PERFORM f_check_selec_9000 CHANGING gv_erro_9000.

      IF gv_erro_9000 NE 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANC'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX_SEARCH  INPUT
*&---------------------------------------------------------------------*
MODULE checkbox_search INPUT.

  MODIFY gt_alv_9000
    FROM zsde013
      INDEX tc_alv-current_line
        TRANSPORTING selec.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE m_update INPUT.

  "PERFORM f_update_vlr_real.

ENDMODULE.
