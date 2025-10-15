*----------------------------------------------------------------------*
***INCLUDE LZGFSD003O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_9000 OUTPUT.

  DESCRIBE TABLE gt_alv_9000 LINES tc_alv-lines.

  IF tc_alv-lines IS INITIAL .
    REFRESH CONTROL: 'TC_ALV' FROM SCREEN sy-dynnr.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_7000 OUTPUT.

  "REFRESH CONTROL: 'TC_ALV' FROM SCREEN sy-dynnr.

  DESCRIBE TABLE gt_alv_7000 LINES tc_alv_7-lines.

  IF tc_alv_7-lines IS INITIAL .
    REFRESH CONTROL: 'TC_ALV_7' FROM SCREEN sy-dynnr.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_9000 OUTPUT.

  IF gv_edit_9000 = 'X'.

    LOOP AT SCREEN.

      IF screen-name = 'ZSDS078-TAXA_NEG'
      OR screen-name = 'ZSDS078-DTPREVPAG_N'
      OR screen-name = 'ZSDS078-VLR_PREV'
      OR screen-name = 'ZSDS078-JUROS_PREV'
      OR screen-name = 'ZSDS078-MULTA_PREV'.

        screen-input = 1.

        "IF screen-name = 'ZSDS078-DTPREVPAG_N'.
        "screen-required = 1.
        "ENDIF.

        MODIFY SCREEN.

      ENDIF.

    ENDLOOP.

  ENDIF.

  LOOP AT SCREEN.

    IF screen-name = 'ZSDS078-FAT_FLG'
      OR screen-name = 'ZSDS078-AFAT_FLG'
      OR screen-name = 'ZSDS078-TT_FAT'
      OR screen-name = 'ZSDS078-TT_AFAT'.

      IF gv_fatu_9000 = 'X'.
        screen-invisible = 0.
      ELSE.
        screen-invisible = 1.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF gv_exibe_travas = space.

      IF screen-name = 'SC_001'.

        screen-invisible = 1.

        MODIFY SCREEN.

      ENDIF.

    ENDIF.

  ENDLOOP.

  LOOP AT SCREEN.

    READ TABLE gt_desab ASSIGNING FIELD-SYMBOL(<fs_desab>)
      WITH KEY fieldname = screen-name.

    CHECK sy-subrc EQ 0.

    screen-input = 0.

    MODIFY SCREEN.

  ENDLOOP.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_LINES_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE tc_lines_9000 OUTPUT.

  DESCRIBE TABLE gt_alv_9000 LINES tc_alv-lines.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9001'.
  "SET TITLEBAR sy-title.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  QTDE_TRAN_7000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE qtde_tran_7000 INPUT.

  MODIFY gt_alv_7000 FROM zsds081
                     INDEX tc_alv_7-current_line
                         TRANSPORTING qt_tran.

  "PERFORM f_calc_vlr_qtd_7000 USING 'V'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_alv_9000 OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'ZSDS079-SELEC'.

      IF gt_alv_9000 IS INITIAL.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SUBSCREEN_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE subscreen_7000 OUTPUT.

  LOOP AT SCREEN.

    READ TABLE gt_desab ASSIGNING <fs_desab>
      WITH KEY fieldname = screen-name.

    CHECK sy-subrc EQ 0.

    screen-input = 0.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
