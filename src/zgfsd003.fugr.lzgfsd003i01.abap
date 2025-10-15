*----------------------------------------------------------------------*
***INCLUDE LZGFSD003I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  gv_ucomm_9000 = sy-ucomm.
  gv_ucomm_7000 = sy-ucomm.

  CASE gv_ucomm_9000.

    WHEN 'UPD_TAX'.
      PERFORM f_atualiza_taxa_curva.
      PERFORM f_upd_fiels_9000 CHANGING gv_erro.

    WHEN 'CHK'.
      PERFORM f_check_event.
    WHEN 'OK'.

      PERFORM f_upd_fiels_9000 CHANGING gv_erro.

      CHECK gv_erro IS INITIAL.

      PERFORM f_check_fields CHANGING gv_erro.

      CHECK gv_erro IS INITIAL.

      PERFORM f_upd_fiels_9000 CHANGING gv_erro.

      CHECK gv_erro IS INITIAL.

      PERFORM f_check_com_popup CHANGING gv_erro.

      CHECK gv_erro IS INITIAL.

      IF sy-dynnr = 9000.
        REFRESH CONTROL: 'TC_ALV' FROM SCREEN sy-dynnr.
      ELSE.
        REFRESH CONTROL: 'TC_ALV_7' FROM SCREEN sy-dynnr.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM f_upd_fiels_9000 CHANGING gv_erro.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX_SEARCH  INPUT
*&---------------------------------------------------------------------*
MODULE checkbox_search INPUT.

  IF sy-dynnr = 9000.

    MODIFY gt_alv_9000
      FROM zsds079
        INDEX tc_alv-current_line
          TRANSPORTING selec.

  ELSE.

    MODIFY gt_alv_9000
    FROM zsds079
      INDEX tc_alv_trv_7000-current_line
        TRANSPORTING selec.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VALDT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_field_9000 INPUT.

  PERFORM f_check_valdt.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE exit_command_9001 INPUT.

  zsde0035-answer = '0'.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'CONT'.

      CASE 'X'.
        WHEN zsde0035-opt1.
          zsde0035-answer = '1'.
        WHEN zsde0035-opt2.
          zsde0035-answer = '2'.
        WHEN OTHERS.
      ENDCASE.

      SET SCREEN 0.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIELD_7000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_field_7000 INPUT.

  CHECK zsds078-kunnr IS INITIAL AND sy-ucomm NE 'CANC'.

  MESSAGE 'Preencher Cliente' TYPE 'E'.

ENDMODULE.
