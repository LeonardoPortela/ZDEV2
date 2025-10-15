CLASS lhc_zc_lib_pagto_imposto DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    DATA gv_ok  TYPE boolean.
    DATA gv_wait  TYPE boolean.
    DATA gv_msg TYPE string.

    METHODS get_result IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_lib_pagto_imposto RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_lib_pagto_imposto RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_lib_pagto_imposto.

    METHODS aprovar FOR MODIFY
      IMPORTING keys FOR ACTION zc_lib_pagto_imposto~aprovar.

    METHODS rejeitar FOR MODIFY
      IMPORTING keys FOR ACTION zc_lib_pagto_imposto~rejeitar.

ENDCLASS.

CLASS lhc_zc_lib_pagto_imposto IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD aprovar.

    DATA: lv_taskname TYPE char10.

    CLEAR: gv_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-lote.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION13_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_id       = ls_key-lote
        in_decision = 'L'. "Aprovar

    WAIT UNTIL gv_wait IS NOT INITIAL.

    IF gv_ok = abap_true.

      APPEND VALUE #(
              %msg = new_message(
              id      = '00'
              number      = 001
              severity  = if_abap_behv_message=>severity-success
              v1      = gv_msg
              )
          ) TO reported-zc_lib_pagto_imposto.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-success
            v1      = 'Aprovação executada com sucesso'
            )
        ) TO reported-zc_lib_pagto_imposto.

    ELSE.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-error
            v1      = gv_msg
            )
        ) TO reported-zc_lib_pagto_imposto.

      APPEND VALUE #(
          %msg = new_message(
          id      = '00'
          number      = 001
          severity  = if_abap_behv_message=>severity-error
          v1      = 'Erro ao executar aprovação'
          )
      ) TO reported-zc_lib_pagto_imposto.

    ENDIF.

  ENDMETHOD.

  METHOD rejeitar.

    DATA:         lv_taskname TYPE char10.

    CLEAR: gv_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-lote.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION13_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_id       = ls_key-lote
        in_decision = 'R'. "Rejeitar

    WAIT UNTIL gv_wait IS NOT INITIAL.

    IF gv_ok = abap_true.

      APPEND VALUE #(
           %msg = new_message(
           id      = '00'
           number      = 001
           severity  = if_abap_behv_message=>severity-success
           v1      = gv_msg
           )
       ) TO reported-zc_lib_pagto_imposto.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-success
            v1      = 'Reinicilização executada com sucesso'
            )
        ) TO reported-zc_lib_pagto_imposto.

    ELSE.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-error
            v1      = gv_msg
            )
        ) TO reported-zc_lib_pagto_imposto.

      APPEND VALUE #(
          %msg = new_message(
          id      = '00'
          number      = 001
          severity  = if_abap_behv_message=>severity-error
          v1      = 'Erro ao executar aprovação'
          )
      ) TO reported-zc_lib_pagto_imposto.

    ENDIF.

  ENDMETHOD.

  METHOD get_result.

    RECEIVE RESULTS FROM FUNCTION 'ZSVPL_FCN_SETDECISION12_APP'
    IMPORTING
        e_msg       = gv_msg
        e_ok        = gv_ok.

    gv_wait = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zc_lib_pagto_imposto DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zc_lib_pagto_imposto IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
