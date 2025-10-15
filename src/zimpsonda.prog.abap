*&---------------------------------------------------------------------*
*& Report ZIMPSONDA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zimpsonda.

TABLES:
        t001.

INCLUDE zimpsonda_top.
INCLUDE zimpsonda_scr.
INCLUDE zimpsonda_f01.

INITIALIZATION.

p_button = 'Codigo de Imposto'.
p_butt   = 'Codigo de Obrigação'.
p_tp     = 'Codigo Tp Ajuste'.
p_del    = 'Limpar Dados das Tabelas'.
p_lanc   = 'Dados da Tabela de Ajustes'.
p_guia   = 'Dados da Tabela de Guias'.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM seleciona_dados.
  PERFORM grava_dados.
