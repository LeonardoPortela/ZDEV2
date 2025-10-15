REPORT zmmr0039.

INCLUDE: zmmr0039_t01, "Variáveis globais.
         zmmr0039_s01, "Tela de seleção.
         zmmr0039_cd01,"Definição de classes.
         zmmr0039_ci01."Implementação de classes.

START-OF-SELECTION.

  DATA: xv_jobnm TYPE btcjob.
  DATA: xv_stepc TYPE btcstepcnt.
  DATA: vg_job      TYPE i.
  "
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
*     EVENTID         =
*     EVENTPARM       =
*     EXTERNAL_PROGRAM_ACTIVE       =
*     JOBCOUNT        = XV_JOBCN
      jobname         = xv_jobnm
      stepcount       = xv_stepc
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.

  IF p_type = 'K'.
    SELECT SINGLE COUNT(*) INTO vg_job
   FROM tbtco
     WHERE jobname EQ 'ZMMR0039_APROV_COUPA_K'
    AND status EQ 'R'.
  ELSE.
    SELECT SINGLE COUNT(*) INTO vg_job
   FROM tbtco
     WHERE jobname EQ 'ZMMR0039_APROV_COUPA_E'
    AND status EQ 'R'.
  ENDIF.

  IF ( vg_job EQ 1 ).
    CONCATENATE 'LCL_LOAD_APPROVER_' p_type INTO gv_object_reference.

    TRY.
        CREATE OBJECT go_load_approver TYPE (gv_object_reference)
          EXPORTING iv_begin_date = p_date.
      CATCH cx_sy_create_object_error.
        MESSAGE TEXT-m01 TYPE 'I' DISPLAY LIKE 'E'. "text-m01: Verificar classe e tipo de execução!!
        RETURN.
    ENDTRY.


    go_load_approver->load( ).

    CREATE OBJECT go_approver_table_handler.

    go_approver_table_handler->apply_changes( it_zprovcoupa01
                                                = go_load_approver->get_aprovadores( ) ).

    IF p_type = 'K'.
      SUBMIT zmmr0035
            WITH s_lookup = 'CC'
            WITH p_batch = ' '  AND RETURN.
    ELSE.
      SUBMIT zmmr0035
        WITH s_lookup = 'CL'
        WITH p_batch = ' '  AND RETURN.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
