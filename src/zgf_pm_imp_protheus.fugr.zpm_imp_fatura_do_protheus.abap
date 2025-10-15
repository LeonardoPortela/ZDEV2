FUNCTION ZPM_IMP_FATURA_DO_PROTHEUS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FATURA) TYPE  ZPME0032_T
*"  EXPORTING
*"     REFERENCE(L_FATURA) TYPE  ZPME0033_T
*"----------------------------------------------------------------------
*&                 AMAGGI - Projeto integração SAP x Posto Mirian
*&---------------------------------------------------------------------*
*& ABAP:  Anderson Oenning ( AO ) - Amaggi
*& Data      : 07/08/2019
*& Funcional: Cleudo Ferreira
*& Chamado/Descrição : CS2019001199 - Interface recebimento de faturas de combustivel frota Amaggi - Posto Miriam
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------

*&--------------------------------------------------------------------

*****************Recebendo dados da fatura*****************************.
  DATA(T_FATURA) = I_FATURA.
  ZCL_WEBSERVIC_PROTHEUS=>POST_FATURA(  EXPORTING I_FATURA = T_FATURA
                                        RECEIVING E_FATURA = L_FATURA ).

ENDFUNCTION.
