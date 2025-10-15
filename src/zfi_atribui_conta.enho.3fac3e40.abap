"Name: \PR:SAPMF02K\FO:LFB1_LESEN\SE:BEGIN\EI
ENHANCEMENT 0 ZFI_ATRIBUI_CONTA.
*
*-CS2022000535-03.02.2023-#78407-JT-inicio
  PERFORM f_recupera_conta USING lfb1-bukrs
                                 lfa1-lifnr
                                 lfa1-stcd1
                                 lfa1-stcd2
                                 abap_true
                        CHANGING lfb1-akont.
*-CS2022000535-03.02.2023-#78407-JT-fim
*
ENDENHANCEMENT.
