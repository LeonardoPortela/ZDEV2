FUNCTION z_auto_cad_tarifa_ferr
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CONDICAO) TYPE  CHAR1
*"     REFERENCE(I_SHTYP) TYPE  KOMG-SHTYP
*"     REFERENCE(I_TDLNR) TYPE  KOMG-TDLNR
*"     REFERENCE(I_LZONEA) TYPE  KOMG-LZONEA
*"     REFERENCE(I_LZONEZ) TYPE  KOMG-LZONEZ
*"     REFERENCE(I_KBETR) TYPE  KONP-KBETR
*"     REFERENCE(I_MATNR) TYPE  KOMG-MATNR OPTIONAL
*"     REFERENCE(I_PSTLZA) TYPE  PSTLZ OPTIONAL
*"     REFERENCE(I_VSTEL) TYPE  KOMG-VSTEL OPTIONAL
*"     REFERENCE(I_DATAB) TYPE  RV13A-DATAB
*"  EXPORTING
*"     REFERENCE(E_MESSAGE) TYPE  STRING
*"----------------------------------------------------------------------
 " You can use the template 'functionModuleParameter' to add here the signature!
.


  DATA: lv_jobname  TYPE tbtcjob-jobname,
        lv_jobcount TYPE tbtcjob-jobcount,
        lv_variant  TYPE raldb-variant VALUE 'ZMMR0209_JOB',
        lv_report   TYPE sy-cprog VALUE 'ZMMR0209',
        lv_finished TYPE abap_bool.

  DATA: lt_rsparams TYPE STANDARD TABLE OF rsparams,
        ls_rsparams TYPE rsparams.

  DATA: ls_vari_desc TYPE varid,
        lt_vari_text TYPE STANDARD TABLE OF varit,
        ls_vari_text TYPE varit.

  lv_jobname = |ZMMR0209_{ sy-datum }{ sy-uzeit }|.

  "Limpa mensagem de retorno
  CLEAR e_message.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc = 0.

    TYPES: BEGIN OF ty_zmmr0209,
             condicao TYPE char1,
             shtyp    TYPE komg-shtyp,
             tdlnr    TYPE komg-tdlnr,
             lzonea   TYPE komg-lzonea,
             lzonez   TYPE komg-lzonez,
             kbetr    TYPE konp-kbetr,
             matnr    TYPE komg-matnr,
             pstlza   TYPE pstlz,
             vstel    TYPE komg-vstel,
             datab    TYPE rv13a-datab,
           END OF ty_zmmr0209.

    DATA: ls_zmmr0209 TYPE ty_zmmr0209.

    ls_zmmr0209-condicao = '2'.
    ls_zmmr0209-shtyp   =  I_shtyp.
    ls_zmmr0209-tdlnr    = I_tdlnr.
    ls_zmmr0209-lzonea   = I_lzonea.
    ls_zmmr0209-lzonez   = I_lzonez.
    ls_zmmr0209-kbetr   = i_kbetr.
    ls_zmmr0209-matnr    = i_matnr.
    ls_zmmr0209-pstlza    = i_pstlza.
    ls_zmmr0209-vstel    = I_vstel.
    ls_zmmr0209-datab    = I_datab.

    FREE MEMORY ID 'MEMORY_ZMMR0209'.

    EXPORT ls_zmmr0209 = ls_zmmr0209 TO MEMORY ID 'MEMORY_ZMMR0209'.

    SUBMIT zmmr0209
    AND RETURN.

    FREE MEMORY ID 'MEMORY_ZMMR0209'.

    DATA: p_date TYPE sy-datum,
          p_time TYPE sy-uzeit.

    IF sy-subrc = 0.
      "Agenda para execução imediata
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = lv_jobname
          sdlstrtdt            = p_date
          sdlstrttm            = p_time
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.

      IF sy-subrc = 0.

        " CALL FUNCTION 'BT_JOB_STATUS_GET'
        " EXPORTING
        "   jobcount  = lv_jobcount
        "   jobname   = lv_jobname
        " IMPORTING
        "   job_status = lv_status

        DATA: _t TYPE i.

        lv_finished = abap_false.
        DO 5 TIMES. " máximo de tentativas (ajuste conforme necessário)

          _t = _t + 1.

          SELECT SINGLE status
            FROM tbtco
            WHERE jobname  = @lv_jobname
              AND jobcount = @lv_jobcount INTO @DATA(lv_jobstatus).

          IF sy-subrc = 0.

            IF lv_jobstatus = 'F' OR lv_jobstatus = 'C'.
              lv_finished = abap_true.
              EXIT.
            ELSEIF lv_jobstatus = 'A' OR lv_jobstatus = 'R'. " Active or Released
              WAIT UP TO 3 SECONDS. " espera antes de checar novamente
            ELSE.

            ENDIF.
          ELSE.
            EXIT.
          ENDIF.

          IF _t >= 5.
            EXIT.
          ENDIF.

        ENDDO.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
