*&-------------------------------------------- -------------------------*
*& Report  ZPMR0071
*&
*&                 AMAGGI - Projeto - Mobile Amaggi Manutenção
*&---------------------------------------------------------------------*
*& ABAP:  Anderson Oenning ( AO ) - Amaggi
*& Data      : 21/03/2021
*& Funcional : Cleudo Ferreira
*& Programa para o JOB Z_PROC_DAD_NOT_MANUT EXECUTAR.
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&---------------------------------------------------------------------*
*& Alimenta base de dados mestre para o aplicativo Amaggi Man consumir
*&---------------------------------------------------------------------*

REPORT zpmr0071.

START-OF-SELECTION.

  DATA: t_ordem TYPE TABLE OF ztpm_d_m_ordem.


  "Processa dados locais de instalação
  zcl_mobile_manutencao=>me_set_local( ).

  "Processa dados equipamento/veiculos.
  zcl_mobile_manutencao=>me_set_eqpto( ).

  "Processa dados ordem de manutenção.
  zcl_mobile_manutencao=>me_set_ordem(
  IMPORTING t_ordem = t_ordem ).

  "Processa dados operação ordem.
  zcl_mobile_manutencao=>me_set_oper_ordem(
  EXPORTING  i_ordem =  t_ordem ).

  "Processa dados ordem de manutenção.
  zcl_mobile_manutencao=>me_set_user( ).


  "Processa dados nota de manutenção.
  zcl_mobile_manutencao=>zif_mobile_manutencao~get_instance(
  RECEIVING r_if_mobile_manutencao = DATA(r_if_mobile_manutencao)
  ).r_if_mobile_manutencao->zfi_set_nota_man( ).

  zcl_mobile_manutencao=>me_registra_log( ).
