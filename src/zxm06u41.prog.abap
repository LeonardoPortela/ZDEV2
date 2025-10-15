*&---------------------------------------------------------------------*
*&  Include           ZXM06U41
*&---------------------------------------------------------------------*
*EKKO_CI-ZICMS = I_EKKO-ZICMS.
*EKKO_CI-ZDESTI = I_EKKO-ZDESTI.

ekko_ci-ztipcontra   = i_ekko-ztipcontra.

ekpo_ci-zckfreteent  = i_ekpo-zckfreteent.
ekpo_ci-zkvgr3       = i_ekpo-zkvgr3.
ekpo_ci-zkvgr4       = i_ekpo-zkvgr4.
ekpo_ci-ztrocanota   = i_ekpo-ztrocanota.


*&---------------------------------------------------------------------*
* Campo utilizado pelo Botão Recap na ABA dados do cliente nas transações
* ME22n e Me21n
*&---------------------------------------------------------------------*
IF ekpo_ci-recap IS INITIAL.
  ekpo_ci-recap   = i_ekpo-recap.
ENDIF.

IF ekpo_ci-reidi IS INITIAL.
  ekpo_ci-reidi   = i_ekpo-reidi.
ENDIF.

IF ekpo_ci-aliquota IS INITIAL.
  ekpo_ci-aliquota   = i_ekpo-aliquota.
ENDIF.
