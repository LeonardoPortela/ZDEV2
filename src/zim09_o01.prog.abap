*&---------------------------------------------------------------------*
*&  Include           ZIM09_O01
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TS 'ABA_SELECAO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE aba_selecao_active_tab_set OUTPUT.
  aba_selecao-activetab = g_aba_selecao-pressed_tab.
  CASE g_aba_selecao-pressed_tab.
    WHEN c_aba_selecao-tab1.
      g_aba_selecao-subscreen = '0011'.
    WHEN c_aba_selecao-tab2.
      g_aba_selecao-subscreen = '0012'.
    WHEN c_aba_selecao-tab3.
      g_aba_selecao-subscreen = '0013'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "ABA_SELECAO_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0010 OUTPUT.
  SET PF-STATUS '10'.
  SET TITLEBAR '10'.
ENDMODULE.                 " STATUS_0010  OUTPUT
