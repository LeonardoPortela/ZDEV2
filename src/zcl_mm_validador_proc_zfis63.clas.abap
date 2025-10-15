class ZCL_MM_VALIDADOR_PROC_ZFIS63 definition
  public
  final
  create public .

public section.

  class-methods IGNORAR_VALIDACAO
    returning
      value(R_V_IGNORAR) type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_VALIDADOR_PROC_ZFIS63 IMPLEMENTATION.


  METHOD ignorar_validacao.

    DATA:
          lv_jobname TYPE tbtcm-jobname.

    r_v_ignorar = abap_false.

    IF sy-batch IS NOT INITIAL.

      TRY.

          CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
            IMPORTING
              jobname         = lv_jobname
            EXCEPTIONS
              no_runtime_info = 1
              OTHERS          = 2.

          " Checa se o job em execução é da ZFIS63
          IF  sy-subrc   IS INITIAL
          AND lv_jobname CS 'JOB_MIRO_SIM'.

            r_v_ignorar = abap_true.

          ENDIF.

        CATCH cx_root.

          r_v_ignorar = abap_false.

      ENDTRY.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
