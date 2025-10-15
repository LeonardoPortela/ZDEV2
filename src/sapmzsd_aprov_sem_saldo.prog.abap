*&-------------------------------------------------------------------------------------------------------*
*& Método         : SAPMZSD_APROV_SEM_SALDO (PoolMóds.)                                                  *
*& Chamado        : USER STORY 169312                                                                    *
*& Data           : 21/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 21/03/2025|DEVK9A1XAW |NSEGATIN       | Aprovar NFL sem Saldo a Vincular. Desenvolvimento inicial.    *
*--------------------------------------------------------------------------------------------------------*
PROGRAM sapmzsd_aprov_sem_saldo.

INCLUDE mzsd_aprov_sem_saldo_top.  " Global Data
INCLUDE mzsd_aprov_sem_saldo_o01.  " PBO-Modules
INCLUDE mzsd_aprov_sem_saldo_i01.  " PAI-Modules
INCLUDE mzsd_aprov_sem_saldo_f01.  " FORM-Routines
