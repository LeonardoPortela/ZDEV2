function z_les_cockpit_automacao_posto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(RT_TRASNPORTADOR) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_POSTO) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_LOTE) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_CONHECIMENTO) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_CARTA_FRETE) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_PERIODO) TYPE  LXHME_RANGE_DATE_T OPTIONAL
*"     REFERENCE(RT_FECHAMENTO) TYPE  LXHME_RANGE_DATE_T OPTIONAL
*"     REFERENCE(RT_VENCIMENTO) TYPE  LXHME_RANGE_DATE_T OPTIONAL
*"     REFERENCE(RT_STATUS) TYPE  LXHME_RANGE_C1_T OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MSGERR) TYPE  BAPI_MSG
*"  TABLES
*"      T_LOTES TYPE  ZLES_COCKPIT_LOTE_T
*"      T_LANCTOS TYPE  ZLES_COCKPIT_LANCTO_T
*"      T_DELTAS TYPE  ZLES_COCKPIT_DELTA_T OPTIONAL
*"      T_CONFER TYPE  ZLES_COCKPIT_CONFER_T OPTIONAL
*"      T_ACRDECR TYPE  ZLES_COCKPIT_ACRESCDECRES_T OPTIONAL
*"      T_MSG STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
**
************************************************************************
*"                     GRUPO ANDRÉ MAGGI                              "*
*"--------------------------------------------------------------------"*
*" Autor        : Robson Motta             - BBKO                     "*
*" Data         : 28.07.2010                                          "*
*" Objetivo     : Cockpit Automação Posto                             "*
*" Versão       : V001  - Request: DEVK908704 Chamado: PROJ.EVOLUIR   "*
*" Nota         : O estorno do adiantamento do lote é feito pela:     "*
*"                BADI: AC_QUANTITY_GET - Impl: ZCL_LES_ESTORNO_ADT.  "*
*"--------------------------------------------------------------------"*
*" Histórico de modificações                                          "*
*" Data         :                                                     "*
*" Autor        :                                                     "*
*" Descrição    :                                                     "*
*" Versão       :                                                     "*
************************************************************************

* Inicializações
  perform yf_inicializacao tables t_lotes
                                  t_lanctos
                                  t_deltas
                                  t_confer
                                  t_acrdecr
                                  t_msg
                            using rt_trasnportador
                                  rt_posto
                                  rt_lote
                                  rt_conhecimento
                                  rt_carta_frete
                                  rt_periodo
                                  rt_fechamento
                                  rt_vencimento
                                  rt_status
                         changing e_msgerr.

 " wait up to 05 seconds.

** Busca informações de cabeçalho de conhecimentos
  perform yf_busca_hdr_conhecimentos.
  check: e_msgerr is initial.

* Busca informações de detalhes de conhecimentos
  perform yf_busca_dtl_conhecimentos.
  check: e_msgerr is initial.

* Carrega dados para retorno
  perform yf_juncao_conhecimentos .
  perform yf_classifica_tabelas_selecoes.
  perform yf_carrega_tabsaida_conhec.

* Carrega documentos gerados
  perform yf_documentos_gerados tables t_lanctos
                                       t_confer.

* Classifica tabelas resultantes
  perform yf_classifica_resultado tables t_lotes
                                         t_lanctos
                                         t_deltas
                                         t_confer
                                         t_acrdecr.

endfunction.
