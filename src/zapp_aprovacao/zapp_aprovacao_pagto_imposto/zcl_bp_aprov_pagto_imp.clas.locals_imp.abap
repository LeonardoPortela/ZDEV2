CLASS lhc_zc_aprov_pagto_imp DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    DATA gv_ok  TYPE boolean.
    DATA gv_wait  TYPE boolean.
    DATA gv_msg TYPE string.

    METHODS get_result IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_aprov_pagto_imp RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_aprov_pagto_imp RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_aprov_pagto_imp.

    METHODS rba_item FOR READ
      IMPORTING keys_rba FOR READ zc_aprov_pagto_imp\_item FULL result_requested RESULT result LINK association_links.

    METHODS cba_item FOR MODIFY
      IMPORTING entities_cba FOR CREATE zc_aprov_pagto_imp\_item.
    METHODS aprovar FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_pagto_imp~aprovar.

    METHODS rejeitar FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_pagto_imp~rejeitar.

ENDCLASS.

CLASS lhc_zc_aprov_pagto_imp IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_item.
  ENDMETHOD.

  METHOD cba_item.
  ENDMETHOD.

  METHOD aprovar.

    DATA: lv_taskname TYPE char10.

    CLEAR: gv_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-lote.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION05_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_id       = CONV sww_wiid( ls_key-lote )
        in_decision = CONV swr_decikey( '0001' ). "Aprovar

    WAIT UNTIL gv_wait IS NOT INITIAL.

    IF gv_ok = abap_true.

      APPEND VALUE #(
              %msg = new_message(
              id      = '00'
              number      = 001
              severity  = if_abap_behv_message=>severity-success
              v1      = gv_msg
              )
          ) TO reported-zc_aprov_pagto_imp.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-success
            v1      = 'Aprovação executada com sucesso'
            )
        ) TO reported-zc_aprov_pagto_imp.

    ELSE.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-error
            v1      = gv_msg
            )
        ) TO reported-zc_aprov_pagto_imp.

      APPEND VALUE #(
          %msg = new_message(
          id      = '00'
          number      = 001
          severity  = if_abap_behv_message=>severity-error
          v1      = 'Erro ao executar aprovação'
          )
      ) TO reported-zc_aprov_pagto_imp.

    ENDIF.


  ENDMETHOD.

  METHOD rejeitar.

    DATA:         lv_taskname TYPE char10.

    CLEAR: gv_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-lote.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION05_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_id       = CONV sww_wiid( ls_key-lote )
        in_decision = CONV swr_decikey( '0003' ). "Rejeitar

    WAIT UNTIL gv_wait IS NOT INITIAL.


    IF gv_ok IS NOT INITIAL AND gv_msg IS NOT INITIAL.

      APPEND VALUE #(
          %msg = new_message(
          id      = '00'
          number      = 001
          severity  = if_abap_behv_message=>severity-success
          v1      = 'Processamento concluido'
          )
      ) TO reported-zc_aprov_pagto_imp.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-success
            v1      = 'Rejeição executada com sucesso'
            )
        ) TO reported-zc_aprov_pagto_imp.

    ELSE.

      APPEND VALUE #(
            %msg = new_message(
            id      = '00'
            number      = 001
            severity  = if_abap_behv_message=>severity-error
            v1      = gv_msg
            )
        ) TO reported-zc_aprov_pagto_imp.

      APPEND VALUE #(
          %msg = new_message(
          id      = '00'
          number      = 001
          severity  = if_abap_behv_message=>severity-error
          v1      = 'Erro ao executar aprovação'
          )
      ) TO reported-zc_aprov_pagto_imp.

    ENDIF.



  ENDMETHOD.

  METHOD get_result.

    RECEIVE RESULTS FROM FUNCTION 'ZSVPL_FCN_SETDECISION05_APP'
    IMPORTING
        e_msg       = gv_msg
        e_ok        = gv_ok.

    gv_wait = abap_true.


  ENDMETHOD.

ENDCLASS.

CLASS lhc_zc_aprov_pagto_imp_it DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_aprov_pagto_imp_it RESULT result.

    METHODS rba_header FOR READ
      IMPORTING keys_rba FOR READ zc_aprov_pagto_imp_it\_header FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_zc_aprov_pagto_imp_it IMPLEMENTATION.

  METHOD read.
  ENDMETHOD.

  METHOD rba_header.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zc_aprov_pagto_imp DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zc_aprov_pagto_imp IMPLEMENTATION.

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
