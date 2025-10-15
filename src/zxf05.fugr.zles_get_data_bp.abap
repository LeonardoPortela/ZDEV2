FUNCTION zles_get_data_bp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(ES_TIPO_PARCEIRO) TYPE  CHAR20
*"     VALUE(ES_BUT000) TYPE  BUT000
*"     VALUE(ES_LFA1) TYPE  LFA1
*"     VALUE(ET_LFB1) TYPE  CVIS_LFB1_T
*"     VALUE(ET_LFBK) TYPE  LFBK_T
*"     VALUE(ET_LFBW) TYPE  CVIS_LFBW_T
*"     VALUE(ET_LFM1) TYPE  CVIS_LFM1_T
*"     VALUE(ES_KNA1) TYPE  KNA1
*"     VALUE(ES_KNVK) TYPE  KNVK
*"     VALUE(ET_KNB1) TYPE  CVIS_KNB1_T
*"     VALUE(ET_KNBW) TYPE  CVIS_KNBW_T
*"     VALUE(ET_KNVI) TYPE  CVIS_KNVI_T
*"     VALUE(ET_KNVV) TYPE  CVIS_KNVV_T
*"----------------------------------------------------------------------

  FREE: es_tipo_parceiro.

  CASE abap_true.
    WHEN gs_bapi_fornece.
      es_tipo_parceiro = 'FORNECEDOR'.
    WHEN gs_bapi_cliente.
      es_tipo_parceiro = 'CLIENTE'.
  ENDCASE.

  es_but000            = gs_but000.  "*-CS2021000253-#149089-07.01.2025-JT
*
  es_lfa1              = gs_lfa1.
  et_lfb1[]            = gt_lfb1[].
  et_lfbk[]            = gt_lfbk[].
  et_lfbw[]            = gt_lfbw[].
  et_lfm1[]            = gt_lfm1[].
*
  es_kna1              = gs_kna1.
  es_knvk              = gs_knvk.
  et_knb1[]            = gt_knb1[].
  et_knbw[]            = gt_knbw[].
  et_knvi[]            = gt_knvi[].
  et_knvv[]            = gt_knvv[].

ENDFUNCTION.
