*&---------------------------------------------------------------------*
*& PoolMóds.         ZIM09
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE zim09_top                               .    " global Data
INCLUDE zim09_o01                               .  " PBO-Modules
INCLUDE zim09_i01                               .  " PAI-Modules
INCLUDE zim09_f01                               .  " FORM-Routines



SELECTION-SCREEN: BEGIN OF SCREEN 111 AS SUBSCREEN.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.
SELECT-OPTIONS: s_bukrs FOR  zim02_sol_ap_ctl-bukrs      .
PARAMETERS:     p_datai LIKE zim02_sol_ap_ctl-dt_aprov_in,
                p_dataf LIKE zim02_sol_ap_ctl-dt_aprov_fim,
                p_dtini LIKE zim02_sol_ap_ctl-dt_inicio.
SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN: END OF SCREEN 111.

SELECTION-SCREEN: BEGIN OF SCREEN 112 AS SUBSCREEN.
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-h02.
SELECT-OPTIONS: s_bukrs2 FOR  zim02_sol_ap_ctl-bukrs.
PARAMETERS:     p_gjahr2 LIKE zim02_sol_ap_ctl-ano,
                p_safra2 LIKE zim02_sol_ap_ctl-safra.
SELECTION-SCREEN: END OF BLOCK b2.
SELECTION-SCREEN: END OF SCREEN 112.


SELECTION-SCREEN: BEGIN OF SCREEN 113 AS SUBSCREEN.
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-h02.
SELECT-OPTIONS: s_bukrs3 FOR  zim02_sol_ap_ctl-bukrs.
PARAMETERS:     p_gjahr3 LIKE zim02_sol_ap_ctl-ano,
                p_safra3 LIKE zim02_sol_ap_ctl-safra,
                p_fase3  LIKE zim02_sol_ap_ctl-fase as listbox visible length 20.
PARAMETERS:     p_datai3 LIKE zim02_sol_ap_ctl-dt_aprov_in,
                p_dataf3 LIKE zim02_sol_ap_ctl-dt_aprov_fim,
                p_dtini3 LIKE zim02_sol_ap_ctl-dt_inicio.
SELECTION-SCREEN: END OF BLOCK b3.
SELECTION-SCREEN: END OF SCREEN 113.
