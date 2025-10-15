*&---------------------------------------------------------------------*
*& PoolMóds.         SAPMZLES001
*&
*&---------------------------------------------------------------------*

************************************************************************
*"                     GRUPO ANDRÉ MAGGI                              "*
*"--------------------------------------------------------------------"*
*" Autor        : Robson Motta             - BBKO                     "*
*" Data         : 21/07/2010                                          "*
*" Objetivo     : Cockpit Automação Posto                             "*
*" Versão       : V001  - Request: DEVK908588                         "*
*" Nota         : O estorno do adiantamento do lote é feito pela:     "*
*"                BADI: AC_QUANTITY_GET - Impl: ZCL_LES_ESTORNO_ADT.  "*
*"--------------------------------------------------------------------"*
*" Histórico de modificações                                          "*
*" Data         :                                                     "*
*" Autor        :                                                     "*
*" Descrição    :                                                     "*
*" Versão       :                                                     "*
************************************************************************

INCLUDE mzles001top                             .  " global Data

INCLUDE mzles001o01                             .  " PBO-Modules
INCLUDE mzles001i01                             .  " PAI-Modules
INCLUDE mzles001f01                             .  " FORM-Routines
