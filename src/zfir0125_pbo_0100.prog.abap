*&---------------------------------------------------------------------*
*& Include          ZFIR0125_PBO_0100
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* PBO Module 0100
*----------------------------------------------------------------------*
MODULE pbo_100 OUTPUT.

  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR  'TITLE_100'.

  IF go_container IS INITIAL.

    PERFORM create_alv.

    PERFORM get_data.

    PERFORM display_alv.
  ENDIF.

ENDMODULE.
