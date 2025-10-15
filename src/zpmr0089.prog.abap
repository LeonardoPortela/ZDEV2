*&---------------------------------------------------------------------*&
*& Report  ZPMR0089                                                    *&
*&                                                                     *&
*& Descrição....: JOB para devolução automática de equipamentos        *&
*& Analista.....: Gabriel Costa                                        *&
*& Desenvolvedor: Rubenilson Pereira.                                  *&
*& Data 26/12/2024.                                                    *&
*&                                                                     *&
*& Modulo.......: PM          Transação: ZPM0109                       *&
*&---------------------------------------------------------------------*&
*&---------------------------------------------------------------------*
*& Report ZPMR0089
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpmr0089.
INCLUDE zpmr0089_top.
INCLUDE zpmr0089_cls.
INCLUDE zpmr0089_f01.

DATA: lv_jobcount TYPE tbtcjob-jobcount,
      lv_jobname  TYPE tbtcjob-jobname.

START-OF-SELECTION.

  IF sy-batch IS NOT INITIAL.

    lv_jobname = sy-repid.

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
    IF sy-subrc IS INITIAL.

      SELECT *
        FROM zequi_emprestimo
        INTO TABLE @DATA(lt_equi_emp)
        WHERE dt_fim_emprestimo = @sy-datum
          AND devolucao_automatica = @abap_true.
      IF sy-subrc IS INITIAL..

        LOOP AT lt_equi_emp ASSIGNING FIELD-SYMBOL(<fs_equi_emp>).

          APPEND INITIAL LINE TO it_saida_dev_equi ASSIGNING FIELD-SYMBOL(<fs_equi_Dev>).
          MOVE-CORRESPONDING <fs_equi_emp> TO <fs_equi_dev>.

        ENDLOOP.

        obj_main = NEW zbapis( ).
        CREATE OBJECT: obj_dev.

        obj_main->z_iniciar_processo_devolucao( ).

      ENDIF.

      " Fechar e executar imediatamente
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = lv_jobname
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.

    ENDIF.

  ENDIF.
