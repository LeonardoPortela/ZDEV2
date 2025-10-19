*&---------------------------------------------------------------------*
*& Report  ZSDR0138
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0138.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_vbeln TYPE vbrk-vbeln.
SELECTION-SCREEN: END OF BLOCK b1.

INCLUDE zsdr0138_top.
INCLUDE zsdr0138_f01.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM clear.
  PERFORM data_selection.
  PERFORM print_report.
