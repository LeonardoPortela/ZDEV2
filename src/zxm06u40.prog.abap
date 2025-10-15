*&---------------------------------------------------------------------*
*&  Include           ZXM06U40
*&---------------------------------------------------------------------*

IF e_ci_ekpo-zckfreteent NE ekpo_ci-zckfreteent.
  e_ci_ekpo-zckfreteent = ekpo_ci-zckfreteent.
  e_ci_update = abap_true.
ENDIF.

IF e_ci_ekpo-zkvgr3 NE ekpo_ci-zkvgr3.
  e_ci_ekpo-zkvgr3 = ekpo_ci-zkvgr3.
  e_ci_update = abap_true.
ENDIF.

IF e_ci_ekpo-zkvgr4 NE ekpo_ci-zkvgr4.
  e_ci_ekpo-zkvgr4 = ekpo_ci-zkvgr4.
  e_ci_update = abap_true.
ENDIF.

IF e_ci_ekpo-ztrocanota NE ekpo_ci-ztrocanota.
   e_ci_ekpo-ztrocanota = ekpo_ci-ztrocanota.
   e_ci_update = abap_true.
ENDIF.

*&---------------------------------------------------------------------*
* Campo utilizado pelo Botão Recap na ABA dados do cliente nas transações
* ME22n e Me21n
*&---------------------------------------------------------------------*
IF e_ci_ekpo-recap NE ekpo_ci-recap.
  e_ci_ekpo-recap = ekpo_ci-recap.
  e_ci_update = abap_true.
ENDIF.

IF e_ci_ekpo-reidi NE ekpo_ci-reidi.
  e_ci_ekpo-reidi = ekpo_ci-reidi.
  e_ci_update = abap_true.
ENDIF.


IF e_ci_ekpo-aliquota NE ekpo_ci-aliquota.
  e_ci_ekpo-aliquota = ekpo_ci-aliquota.
  e_ci_update = abap_true.
ENDIF.
