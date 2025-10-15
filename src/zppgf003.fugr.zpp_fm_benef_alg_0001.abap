FUNCTION zpp_fm_benef_alg_0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  ranges: lra_status_registro FOR zppt0002-status_registro.

  TYPES: BEGIN OF ty_tbtco,
           jobname  TYPE tbtco-jobname,
           jobcount TYPE tbtco-jobcount,
           status   TYPE tbtco-status,
         END OF ty_tbtco.

  DATA: lva_dt_corte TYPE sy-datum.

  DATA: lit_zppt0002_in_proc TYPE TABLE OF zppt0002,
        lit_tbtco            TYPE TABLE OF ty_tbtco.

  lva_dt_corte = sy-datum - 30.

  SELECT *
    FROM zppt0002 INTO TABLE lit_zppt0002_in_proc
   WHERE laeda                GE lva_dt_corte
     AND status_processamento EQ 'A'. "Em processamento

  CHECK lit_zppt0002_in_proc[] IS NOT INITIAL.

  SELECT jobname, jobcount,  status
    FROM tbtco INTO TABLE @lit_tbtco
    FOR ALL ENTRIES IN @lit_zppt0002_in_proc
   WHERE jobname  = @lit_zppt0002_in_proc-jobname_process
     AND jobcount = @lit_zppt0002_in_proc-jobcount_process.

  LOOP AT lit_zppt0002_in_proc INTO DATA(lwa_zppt0002_in_proc).

    DATA(_restart_proc) = abap_false.

    IF ( lwa_zppt0002_in_proc-jobname_process  IS INITIAL ) OR
       ( lwa_zppt0002_in_proc-jobcount_process IS INITIAL ).
      _restart_proc = abap_true.
    ELSE.
      READ TABLE lit_tbtco INTO DATA(lwa_tbtco) WITH KEY jobname  = lwa_zppt0002_in_proc-jobname_process
                                                         jobcount = lwa_zppt0002_in_proc-jobcount_process.

      "Se não encontrou job, ou o mesmo foi cancelado ou finalizado
      IF sy-subrc NE 0 OR
         lwa_tbtco-status = ZCL_JOB=>st_status_cancelado or
         lwa_tbtco-status = ZCL_JOB=>st_status_finalizado.
        _restart_proc = abap_true.
      ENDIF.
    ENDIF.

    IF _restart_proc EQ abap_true.
      lwa_zppt0002_in_proc-status_processamento = 'P'. "Processamento Pendente
      MODIFY zppt0002 FROM lwa_zppt0002_in_proc.
    ENDIF.

  ENDLOOP.




ENDFUNCTION.
