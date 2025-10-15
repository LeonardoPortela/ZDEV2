*&---------------------------------------------------------------------*
*& Report ZJOB0005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjob0005.

TABLES: tbtco.

PARAMETERS: p_times TYPE i DEFAULT 1,
            p_wait  TYPE i DEFAULT 10.

SELECT-OPTIONS: p_prog  FOR tbtco-intreport NO INTERVALS OBLIGATORY.


DATA: lva_jobnm TYPE btcjob,
      lva_stepc TYPE btcstepcnt,
      lva_job   TYPE i.

START-OF-SELECTION.

  CLEAR: lva_jobnm, lva_stepc, lva_job.

  IF sy-batch EQ abap_true.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = lva_jobnm
        stepcount       = lva_stepc
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    SELECT SINGLE COUNT(*) INTO lva_job
         FROM tbtco
        WHERE jobname EQ lva_jobnm
          AND status EQ 'R'.

    CHECK lva_job EQ 1.

  ENDIF.

  DO p_times TIMES.

    PERFORM f_escalonar_jobs_fila.

    WAIT UP TO p_wait SECONDS.

  ENDDO.

FORM f_escalonar_jobs_fila.

  DATA: lva_number           TYPE tbtcjob-jobcount,
        lva_name             TYPE tbtcjob-jobname,
        lit_print_parameters TYPE pri_params,
        lit_seltab           TYPE TABLE OF rsparams.

  SELECT *
     FROM zjob0003 INTO TABLE @DATA(lit_zjob0003)
    WHERE progname IN @p_prog
      AND jobcount EQ @space
      AND cancel   EQ @space.

  CHECK lit_zjob0003[] IS NOT INITIAL.

  IF lit_zjob0003[] IS NOT INITIAL.
    SELECT *
       FROM zjob0004 INTO TABLE @DATA(lit_zjob0004)
      FOR ALL ENTRIES IN @lit_zjob0003
      WHERE id_fila_job EQ @lit_zjob0003-id_fila_job.
  ENDIF.

  LOOP AT lit_zjob0003 INTO DATA(lwa_zjob0003).

    CLEAR: lva_number,lva_name,lit_print_parameters, lit_seltab[].

    lva_name = lwa_zjob0003-jobname.

    LOOP AT lit_zjob0004 INTO DATA(lwa_job0004) WHERE id_fila_job = lwa_zjob0003-id_fila_job.
      APPEND INITIAL LINE TO lit_seltab ASSIGNING FIELD-SYMBOL(<fs_param>).

      <fs_param>-selname       = lwa_job0004-selname.
      <fs_param>-kind          = lwa_job0004-kind.
      <fs_param>-sign          = lwa_job0004-sign.
      <fs_param>-option        = lwa_job0004-opcao.
      <fs_param>-low           = lwa_job0004-low.
      <fs_param>-high          = lwa_job0004-high.
    ENDLOOP.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lva_name
      IMPORTING
        jobcount         = lva_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      SUBMIT (lwa_zjob0003-progname) TO SAP-SPOOL SPOOL PARAMETERS lit_print_parameters WITHOUT SPOOL DYNPRO VIA JOB lva_name NUMBER lva_number
        WITH SELECTION-TABLE lit_seltab
        USER lwa_zjob0003-user_job
         AND RETURN.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lva_number
            jobname              = lva_name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc IS INITIAL.
          lwa_zjob0003-jobcount = lva_number.
          MODIFY zjob0003 FROM lwa_zjob0003.
          COMMIT WORK.
        ELSE.
          lwa_zjob0003-cancel = abap_true.
          MODIFY zjob0003 FROM lwa_zjob0003.
          COMMIT WORK.

          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = lva_number
              jobname                  = lva_name
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18.
        ENDIF.
      ELSE.

        lwa_zjob0003-cancel = abap_true.
        MODIFY zjob0003 FROM lwa_zjob0003.
        COMMIT WORK.

        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = lva_number
            jobname                  = lva_name
          EXCEPTIONS
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            OTHERS                   = 18.

      ENDIF.
    ENDIF.

  ENDLOOP.




ENDFORM.
