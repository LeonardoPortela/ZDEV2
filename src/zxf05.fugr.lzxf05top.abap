FUNCTION-POOL zxf05.                        "MESSAGE-ID ..

DATA: gs_bapi_fornece TYPE char01,
      gs_bapi_cliente TYPE char01,
*
      gs_but000       TYPE but000, "*-CS2021000253-#149089-07.01.2025-JT-inicio
*
      gs_lfa1         TYPE lfa1,
      gt_lfb1         TYPE cvis_lfb1_t,
      gt_lfbk         TYPE lfbk_t,
      gt_lfbw         TYPE cvis_lfbw_t,
      gt_lfm1         TYPE cvis_lfm1_t,
*
      gs_kna1         TYPE kna1,
      gs_knvk         TYPE knvk,
      gt_knb1         TYPE cvis_knb1_t,
      gt_knbw         TYPE cvis_knbw_t,
      gt_knvi         TYPE cvis_knvi_t,
      gt_knvv         TYPE cvis_knvv_t,
      lc_error        TYPE cvis_message.

*---------------------------------------------------------------------------------------------
*---------------------------------------------------------------------------------------------
