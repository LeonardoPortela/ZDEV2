*&---------------------------------------------------------------------*
*& Report ZSD_AJUSTA_INTEGRA_OV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_ajusta_integra_ov.

DATA: w_zsdt0213_trace TYPE zsdt0213_trace,
      l_sol_ov         TYPE zsdt0213_trace-nro_sol_ov,
      l_posnr          TYPE zsdt0213_trace-posnr.

START-OF-SELECTION.

  SELECT *
    FROM zintegracao
    INTO TABLE @DATA(t_integra)
   WHERE id_interface  = '168'
     AND tp_referencia = 'TRACE COTTON-Ordem Venda'.

  LOOP AT t_integra INTO DATA(w_integra).
    SPLIT w_integra-id_referencia AT '/' INTO l_sol_ov l_posnr.

    w_zsdt0213_trace-mandt      = sy-mandt.
    w_zsdt0213_trace-nro_sol_ov = l_sol_ov.
    w_zsdt0213_trace-posnr      = l_posnr.
    w_zsdt0213_trace-integrado  = abap_true.
    w_zsdt0213_trace-data_reg   = sy-datum.
    w_zsdt0213_trace-hora_reg   = sy-uzeit.
    w_zsdt0213_trace-user_reg   = 'JOBADM'.

    MODIFY zsdt0213_trace    FROM w_zsdt0213_trace.
  ENDLOOP.

  COMMIT WORK.
