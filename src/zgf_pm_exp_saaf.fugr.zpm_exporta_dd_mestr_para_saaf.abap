FUNCTION ZPM_EXPORTA_DD_MESTR_PARA_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      WIT_M_VEIC_SAAF STRUCTURE  ZTPM_M_VEIC_SAAF
*"      WIT_M_USER_SAAF STRUCTURE  ZTPM_M_USER_SAAF
*"      WIT_M_PROD_SAAF STRUCTURE  ZTPM_M_PROD_SAAF
*"      WIT_B_PLAN_SAAF STRUCTURE  ZTPM_B_PLAN_SAAF
*"      WIT_LOC_AB_SAAF STRUCTURE  ZTPM_LOC_AB_SAAF
*"      WIT_R_VE_OP_SAAF STRUCTURE  ZTPM_R_VE_OP_SAAF
*"      WIT_B_COMP_SAAF STRUCTURE  ZTPM_B_COMP_SAAF
*"      WIT_B_CAUS_SAAF STRUCTURE  ZTPM_B_CAUS_SAAF
*"      WIT_C_VEIC_SAAF STRUCTURE  ZTPM_C_VEIC_SAAF
*"      WIT_B_CENT_SAAF STRUCTURE  ZTPM_B_CENT_SAAF
*"----------------------------------------------------------------------
*&                 AMAGGI - Projeto Abaco
*&---------------------------------------------------------------------*
*& Criado por:  José Godoy ( JAP ) - Ábaco Consultores
*& Data      : 18/05/2017
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição :xx Interface Exportação Dds.Mestres para SAAF
*& Request: DEVK971109 - PM - 25.05.2017 - Interface SAP X SAAF [  ] - JAP
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
* Verifica se tabela de entrada está vazia
  SELECT * INTO TABLE IT_EXP_P_SAAF
     FROM ZTPM_EXP_P_SAAF.
*
* View de Veículos
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_M_VEIC_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_03.
*
    IF NOT IT_M_VEIC_SAAF[] IS INITIAL.
      WIT_M_VEIC_SAAF[] = IT_M_VEIC_SAAF[].
    ENDIF.
*
  ELSE.
    PERFORM ZF_INSERIR_NOVOS_TABELA_03.
*
    IF NOT IT_M_VEIC_SAAF[] IS INITIAL.
      WIT_M_VEIC_SAAF[] = IT_M_VEIC_SAAF[].
    ENDIF.
*
  ENDIF.
****
* View de Usuários
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_M_USER_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_04.
*
    IF NOT IT_M_USER_SAAF[] IS INITIAL.
      WIT_M_USER_SAAF[] = IT_M_USER_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Produtos
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_M_PROD_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_05.
*
    IF NOT IT_M_PROD_SAAF[] IS INITIAL.
      WIT_M_PROD_SAAF[] = IT_M_PROD_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Plano de Operação
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_B_PLAN_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_06.
*
    IF NOT IT_B_PLAN_SAAF[] IS INITIAL.
      WIT_B_PLAN_SAAF[] = IT_B_PLAN_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Local de Abastecimento
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_LOC_AB_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_09.
*
    IF NOT IT_LOC_AB_SAAF[] IS INITIAL.
      WIT_LOC_AB_SAAF[] = IT_LOC_AB_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Veículo X Operação
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_R_VE_OP_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_10.
*
    IF NOT IT_R_VE_OP_SAAF[] IS INITIAL.
      WIT_R_VE_OP_SAAF[] = IT_R_VE_OP_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Compartimentos
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_B_COMP_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_15.
*
    IF NOT IT_B_COMP_SAAF[] IS INITIAL.
      WIT_B_COMP_SAAF[] = IT_B_COMP_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Causa Manutenção
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_B_CAUS_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_16.
*
    IF NOT IT_B_CAUS_SAAF[] IS INITIAL.
      WIT_B_CAUS_SAAF[] = IT_B_CAUS_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Veículo Compartimento
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_C_VEIC_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_18.
*
    IF NOT IT_C_VEIC_SAAF[] IS INITIAL.
      WIT_C_VEIC_SAAF[] = IT_C_VEIC_SAAF[].
    ENDIF.
  ENDIF.
****
* View de Centro de Custo
  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_B_CENT_SAAF.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM ZF_INSERIR_TODA_TABELA_19.
*
    IF NOT IT_B_CENT_SAAF[] IS INITIAL.
      WIT_B_CENT_SAAF[] = IT_B_CENT_SAAF[].
    ENDIF.
  ENDIF.
****
ENDFUNCTION.
