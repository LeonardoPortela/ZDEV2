*&---------------------------------------------------------------------*
*&  Include           MZPMR0066O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN100'.
  IF g_custom_container IS NOT INITIAL AND gv_name1 IS INITIAL.
    REFRESH gt_outtab.
    CALL METHOD g_grid->finalize.
    CALL METHOD g_grid->free.
    CALL METHOD g_custom_container->finalize.
    CALL METHOD g_custom_container->free.
  ENDIF.
  IF gv_name1 IS INITIAL.
    PERFORM create_and_init_alv CHANGING gt_outtab[]
                                         gt_fieldcat.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN100'.

  LOOP AT SCREEN.
    IF screen-name = 'EG_ZPMT0044-LABST'.
      eg_zpmt0044-labst = 1.
      screen-input      = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
