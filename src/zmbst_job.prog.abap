*&---------------------------------------------------------------------*
*& Report  ZMBST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmbst_job.

TABLES: mkpf.

DATA:
  number  TYPE tbtcjob-jobcount,
  name    TYPE tbtcjob-jobname  VALUE 'JOB_ZMBST',
  ck_erro TYPE char01 VALUE abap_false,
              PRINT_PARAMETERS TYPE PRI_PARAMS.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
PARAMETERS: dt_estor LIKE mkpf-budat OBLIGATORY,
            dt_anodo LIKE mkpf-mjahr OBLIGATORY.
SELECT-OPTIONS: nr_docum FOR mkpf-mblnr NO INTERVALS.
PARAMETERS: user_job LIKE sy-uname DEFAULT sy-uname.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.


START-OF-SELECTION.

  PERFORM estorna_documentos.


*&---------------------------------------------------------------------*
*&      Form  ESTORNA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM estorna_documentos.


  DATA(lc_user_job) = user_job.

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

    SUBMIT zmbst
    TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS
    WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
    WITH dt_estor = dt_estor
    WITH dt_anodo = dt_anodo
    WITH nr_docum IN nr_docum
    USER LC_USER_JOB
    AND RETURN.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = number
          jobname              = name
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

      IF sy-subrc IS NOT INITIAL.
        ck_erro = abap_true.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = number
            jobname                  = name
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
  ENDIF.


ENDFORM.                    " ESTORNA_DOCUMENTOS
