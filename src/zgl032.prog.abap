*&---------------------------------------------------------------------*
*& Report  ZGL032
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZGL032.

TABLES: BSIS,BSAD.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS  FOR BSIS-BUKRS OBLIGATORY NO-EXTENSION NO INTERVALS, "Empresa
                S_BUDAT  FOR BSIS-BUDAT OBLIGATORY, "Data de Lançamento
                S_HKTVR  FOR BSIS-HKONT,           "Conta Variação
                S_WAERS  FOR BSIS-WAERS,            "Moeda
                S_WAERS2 FOR BSIS-WAERS.            "Moeda Variação
SELECTION-SCREEN: END OF BLOCK B1.

* Analise Relatório ----------*
SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_AR1 RADIOBUTTON GROUP TP2 DEFAULT 'X', "Analise Calculo Variação
            R_AR2 RADIOBUTTON GROUP TP2.             "Reclassificação de Conta de Variação
SELECTION-SCREEN: END OF BLOCK B2.

* Registros Calculados ----------*
SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(39) TEXT-003.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: S_LMPOS FOR BSIS-DMBE2 NO-EXTENSION NO INTERVALS, "(+)
                S_LMNEG FOR BSIS-DMBE2 NO-EXTENSION NO INTERVALS. "(-)

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(39) TEXT-005.
SELECTION-SCREEN END OF LINE.

PARAMETERS: R_RC1 RADIOBUTTON GROUP TP3,              "Com Diferença
            R_RC2 RADIOBUTTON GROUP TP3,              "Validados
            R_RC3 RADIOBUTTON GROUP TP3  DEFAULT 'X'. "Todos Lançamentos
SELECTION-SCREEN: END OF BLOCK B3.

* Tipo de Conta ----------*
SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-006.
PARAMETERS: R_TP1 RADIOBUTTON GROUP TP4 DEFAULT 'X', "Fornecedor
            R_TP2 RADIOBUTTON GROUP TP4.             "Cliente

SELECT-OPTIONS: S_PARID FOR BSAD-KUNNR, "Código
                S_HKTRC FOR BSIS-HKONT. "Conta de Reconciliação

SELECTION-SCREEN: END OF BLOCK B4.


*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZGL032_TOP.
INCLUDE ZGL032_CLASS.
INCLUDE ZGL032_FORM.
INCLUDE ZGL032_PBO.
INCLUDE ZGL032_PAI.
 "#EC CI_DB_OPERATION_OK[2389136]
*----------------------------------------------------------------------* "#EC CI_DB_OPERATION_OK[2431747]
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM INICIAR_VARIAVEIS.
  PERFORM SELECIONA_DADOS.

  IF VG_NOT_FOUND IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM PROCESSA_DADOS.
  PERFORM IMPRIME_DADOS.
