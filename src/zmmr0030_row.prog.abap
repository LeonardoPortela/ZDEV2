REPORT zmmr0030_row.

DATA: vg_job            TYPE i,
      number            TYPE tbtcjob-jobcount,
      name              TYPE tbtcjob-jobname,
      user              TYPE sy-uname,
      s_cotton          TYPE zrsdsselopts,
      wa_zppt0026       TYPE zppt0026,
      job_ultimo        TYPE zppt0026-jobname,
      i_inbound         TYPE zppt016,
      qtd_processamento TYPE i,
      qtd_itens         TYPE int4,
      print_parameters  TYPE pri_params.

IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    EXIT.
  ENDIF.
ENDIF.

SELECT SINGLE valfrom
  FROM setleaf
  INTO @DATA(vl_on_off)
  WHERE setname = 'ZMMR0030_REMOVE_SEGUNDO'.

SELECT SINGLE valfrom
  FROM setleaf
  INTO @DATA(vl_qtd_processamento)
  WHERE setname = 'ZMMR0030_ROW_QTD_EVENTOS'.

qtd_processamento = vl_qtd_processamento.

DO.

  SELECT SINGLE *
    FROM zppt0026
    INTO wa_zppt0026
      WHERE seq IN ( SELECT MIN( seq ) FROM zppt0026 WHERE dt_fim EQ '00000000' ).

  IF sy-subrc IS NOT INITIAL.
    WRITE / 'Fila Vazia'.
    EXIT.
  ENDIF.

  e_qtd = 0.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZMMR0030_JOB' IMPORTING e_qtd = e_qtd ).
    CATCH zcx_job.
      e_qtd = 0.
  ENDTRY.

  IF e_qtd GT qtd_processamento.
    CONTINUE.
  ENDIF.

  IF job_ultimo EQ wa_zppt0026-jobname.
    CLEAR wa_zppt0026.
    WAIT UP TO 1 SECONDS.
    CONTINUE.
  ENDIF.

  SELECT COUNT(*)
    FROM tbtco
    WHERE jobname EQ wa_zppt0026-jobname
    AND status <> 'A'.

  IF sy-subrc IS INITIAL.

    SELECT COUNT(*)
      FROM tbtco
      WHERE jobname = wa_zppt0026-jobname
      AND status EQ 'F'.

    IF sy-subrc IS INITIAL.
      wa_zppt0026-dt_fim = sy-datum.
      MODIFY zppt0026 FROM wa_zppt0026.
      COMMIT WORK.
    ENDIF.

    WAIT UP TO 1 SECONDS.
    CONTINUE.
  ENDIF.

  SELECT SINGLE ds_body
    FROM zintegracao
    INTO @DATA(ds_body)
    WHERE id_referencia = @wa_zppt0026-id_referencia.

  IF sy-subrc IS NOT INITIAL.

    ADD 1 TO wa_zppt0026-qtd_exec.
    MODIFY zppt0026 FROM wa_zppt0026.
    COMMIT WORK.
    WRITE / 'Tabela ZINTEGRAÇÃO não encontrado: ' && wa_zppt0026-id_referencia && ' Nº: ' && wa_zppt0026-qtd_exec.

    IF wa_zppt0026-qtd_exec >= 20.

      wa_zppt0026-dt_fim = sy-datum.
      MODIFY zppt0026 FROM wa_zppt0026.
      COMMIT WORK.

      CONTINUE.

    ENDIF.

    WAIT UP TO 1 SECONDS.

    CONTINUE.

  ENDIF.

  /ui2/cl_json=>deserialize( EXPORTING json = ds_body CHANGING data = i_inbound ).
  s_cotton = VALUE #( FOR ls IN i_inbound ( sign = 'I' option = 'EQ' low = ls-nr_fardo_origem ) ).

  qtd_itens = 0.
  qtd_itens = lines( s_cotton ).

  SORT s_cotton BY low.
  DELETE ADJACENT DUPLICATES FROM s_cotton COMPARING low.

  IF s_cotton  IS INITIAL.
    WRITE / 'Tabela ZINTEGRAÇÃO não encontrado: ' && wa_zppt0026-id_referencia.
    CONTINUE.
  ENDIF.

  name = wa_zppt0026-jobname.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = name
    IMPORTING
      jobcount         = number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc IS INITIAL.

    SUBMIT zmmr0030_job WITH in_ctt IN s_cotton
                        WITH in_ref = wa_zppt0026-id_referencia
                        VIA JOB name NUMBER number
                        AND RETURN.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = number
          jobname              = name
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.

      wa_zppt0026-qtd_item = qtd_itens.
      wa_zppt0026-dt_fim = sy-datum.
      wa_zppt0026-hr_fim = sy-uzeit.

      MODIFY zppt0026 FROM wa_zppt0026.

      WRITE / 'JOB Iniciado' && wa_zppt0026-jobname.

    ENDIF.
  ENDIF.

  COMMIT WORK.

  IF vl_on_off EQ '1'.
    WAIT UP TO 2 SECONDS.
  ENDIF.

  job_ultimo = wa_zppt0026-jobname.

ENDDO.
