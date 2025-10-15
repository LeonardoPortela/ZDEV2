*&---------------------------------------------------------------------*
*& Report  ZPMR0061
*&
*&---------------------------------------------------------------------*
*& Analista funcional: Cleudo Ferreira
*& ABAP              : Anderson Oenning
*&---------------------------------------------------------------------*
REPORT ZPMR0061.

*****Criar documento de medição dos veiculos frota propria Amaggi.

***Methdo para seleção dos dados abastecimento combustivel frota Amaggi.
ZCL_EXC_APONT_MED=>SELEC_DADOS_COMB( ).
