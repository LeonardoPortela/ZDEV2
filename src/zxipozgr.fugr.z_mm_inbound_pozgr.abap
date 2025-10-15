FUNCTION Z_MM_INBOUND_POZGR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ZMM_PO_ZGR STRUCTURE  ZMMT_PO_ZGR
*"  CHANGING
*"     VALUE(I_SAP_UNITARIO) TYPE  CHAR01 DEFAULT ABAP_FALSE
*"----------------------------------------------------------------------
*" Histórico de modificações                                          "*
*" Data         :                                                     "*
*" Autor        :                                                     "*
*" Descrição    :                                                     "*
*" Versão       :                                                     "*
************************************************************************
*
* Inicializações
  REFRESH: YT_LOG_POZGR.", it_zmmpo_item_zgr.

*---> 04/07/2023 - Migração S4 - WS
  SORT T_ZMM_PO_ZGR BY OBJ_KEY.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM T_ZMM_PO_ZGR COMPARING OBJ_KEY.

  "it_zmmpo_item_zgr[] =  t_zmmt_po_item_zgr[].
  "SORT it_zmmpo_item_zgr BY obj_key ebelp.

* Processa solicitação de pedido
  PERFORM YF_EXECUTA_PEDIDO_SIGAM TABLES T_ZMM_PO_ZGR.


* Retorna LOG de processamento
  PERFORM YF_RETORNA_LOG_SIGAM USING I_SAP_UNITARIO.

ENDFUNCTION.
