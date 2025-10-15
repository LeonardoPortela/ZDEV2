*&---------------------------------------------------------------------*
*& Report Z01VARIA10
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z01VARIA10.

DATA: qtd_prod TYPE zde_qtd_carg,
      qtd_mat  TYPE brgew_ap,
      qtd_gfg  TYPE string.

TYPES: BEGIN OF ty_kna1,
        KUNNR type kna1-KUNNR,
        LAND1 type kna1-LAND1,
        NAME1 type kna1-NAME1,
       END OF   ty_kna1.


DATA: ti_kna1 type TABLE OF ty_kna1.

" Tela de seleção
PARAMETERS: p_kunnr TYPE kna1-kunnr.

START-OF-SELECTION.
