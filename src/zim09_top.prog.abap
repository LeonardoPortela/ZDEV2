*&---------------------------------------------------------------------*
*& Include ZIM09_TOP                                         PoolMóds.        ZIM09
*&
*&---------------------------------------------------------------------*

PROGRAM  zim09.

TABLES: zim02_sol_ap_ctl.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'ABA_SELECAO'
CONSTANTS: BEGIN OF c_aba_selecao,
             tab1 LIKE sy-ucomm VALUE 'ABA_SELECAO_FC1',
             tab2 LIKE sy-ucomm VALUE 'ABA_SELECAO_FC2',
             tab3 LIKE sy-ucomm VALUE 'ABA_SELECAO_FC3',
           END OF c_aba_selecao.
*&SPWIZARD: DATA FOR TABSTRIP 'ABA_SELECAO'
CONTROLS:  aba_selecao TYPE TABSTRIP.
DATA:      BEGIN OF g_aba_selecao,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZIM09',
             pressed_tab LIKE sy-ucomm VALUE c_aba_selecao-tab1,
           END OF g_aba_selecao.
DATA:      ok_code LIKE sy-ucomm.

DATA: novo_exerc   LIKE zim02_sol_ap_ctl-ano,
      novo_safra   LIKE zim02_sol_ap_ctl-ano,
      modelo_exerc LIKE zim02_sol_ap_ctl-ano,
      modelo_safra LIKE zim02_sol_ap_ctl-ano.


FIELD-SYMBOLS: <r_bukrs>  TYPE ANY TABLE,
               <r_bukrs2> TYPE ANY TABLE,
               <r_bukrs3> TYPE ANY TABLE,
               <r_datai>  TYPE ANY,
               <r_dataf>  TYPE ANY,
               <r_dtini>  TYPE ANY,
               <r_gjahr2> TYPE ANY,
               <r_safra2> TYPE ANY,
               <r_gjahr3> TYPE ANY,
               <r_safra3> TYPE ANY,
               <r_fase3>  TYPE ANY,
               <r_datai3> TYPE ANY,
               <r_dataf3> TYPE ANY,
               <r_dtini3> TYPE ANY.
