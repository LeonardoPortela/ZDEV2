*&---------------------------------------------------------------------*
*& Report  ZPMR0042
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0042 MESSAGE-ID ZPMMSG.

INCLUDE ZPMR0042_TOPO.
INCLUDE ZPMR0042_CLASS.
INCLUDE ZPMR0042_PERF.

START-OF-SELECTION.

  CALL METHOD ZCL_ORDEM->SLE_ORDENS.
