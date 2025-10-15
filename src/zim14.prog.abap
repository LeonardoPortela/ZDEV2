*&---------------------------------------------------------------------*
*& Report  ZIM14
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZIM14.
DATA: BEGIN OF T_INV OCCURS 0.
        INCLUDE STRUCTURE ZIM01_SOL_AP_INV.
      DATA: END OF T_INV.

PERFORM F_ZIM02(ZIM13) TABLES T_INV.

PERFORM F_ZIM02_ADD(ZIM13) TABLES T_INV.
