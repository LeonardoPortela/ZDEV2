*&---------------------------------------------------------------------*
*& Report  ZPMR0055
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Criado por:  Anderson Oenning ( AO ) - Amaggi
*& Data      : 07/08/2019
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição : CS2019001199 - Interface recebimento de faturas de combustivel frota Amaggi - Posto Miriam
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT zpmr0055.

START-OF-SELECTION.
***Criar migo e miro para pedidos fatura de combustivel.
  TRY .
      zcl_webservic_protheus=>get_nfe( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).

  ENDTRY.
