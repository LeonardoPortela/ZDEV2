CLASS lhc_zc_aprov_orc_ordem_pm DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    DATA gv_ok  TYPE boolean.
    DATA gv_wait  TYPE boolean.
    DATA gt_msg TYPE bapiret2_tab.

    METHODS get_result IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_aprov_orc_ordem_pm RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_aprov_orc_ordem_pm RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_aprov_orc_ordem_pm.

    METHODS aprovar FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_orc_ordem_pm~aprovar.

    METHODS rejeitar FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_orc_ordem_pm~rejeitar.
    METHODS solicita FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_orc_ordem_pm~solicita.
    METHODS mudavalor FOR MODIFY
      IMPORTING keys FOR ACTION zc_aprov_orc_ordem_pm~mudavalor.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zc_aprov_orc_ordem_pm RESULT result.

ENDCLASS.

CLASS lhc_zc_aprov_orc_ordem_pm IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD aprovar.

    DATA: lv_taskname TYPE char10.

    CLEAR: gt_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-aufnr.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION08_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_id       = ls_key-aufnr
        in_decision = 'L'. "Aprovar

    WAIT UNTIL gv_wait IS NOT INITIAL UP TO 1 SECONDS.


    LOOP AT gt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>).

      APPEND VALUE #(
        %msg = new_message(
        id      = <fs_msg>-id
        number  = <fs_msg>-number
        severity = CONV #( <fs_msg>-type )
        v1      = <fs_msg>-message_v1
        v2      = <fs_msg>-message_v2
        v3      = <fs_msg>-message_v3
        v4      = <fs_msg>-message_v4
        )
    ) TO reported-zc_aprov_orc_ordem_pm.

    ENDLOOP.

  ENDMETHOD.

  METHOD rejeitar.

    DATA: lv_taskname TYPE char10.

    CLEAR: gt_msg, gv_ok, gv_wait.

    DATA(ls_key) = keys[ 1 ].

    lv_taskname = ls_key-aufnr.

    CALL FUNCTION 'ZSVPL_FCN_SETDECISION08_APP'
      DESTINATION 'NONE'
      STARTING NEW TASK lv_taskname
      CALLING get_result ON END OF TASK
      EXPORTING
        in_motivo   = 'Rejeitado APP'
        in_id       = ls_key-aufnr
        in_decision = 'R'. "Rejeitar

    WAIT UNTIL gv_wait IS NOT INITIAL UP TO 1 SECONDS.

    LOOP AT gt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>).

      APPEND VALUE #(
        %msg = new_message(
        id      = <fs_msg>-id
        number  = <fs_msg>-number
        severity = CONV #( <fs_msg>-type )
        v1      = <fs_msg>-message_v1
        v2      = <fs_msg>-message_v2
        v3      = <fs_msg>-message_v3
        v4      = <fs_msg>-message_v4
        )
    ) TO reported-zc_aprov_orc_ordem_pm.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_result.

    RECEIVE RESULTS FROM FUNCTION 'ZSVPL_FCN_SETDECISION08_APP'
    IMPORTING
      et_msg       = gt_msg
      e_ok        = gv_ok.

    gv_wait = abap_true.

  ENDMETHOD.

  METHOD solicita.

    DATA(ls_param) = VALUE #( keys[ 1 ]-%param OPTIONAL ).

    DATA(lo_action) = NEW zclpm_aprova_sup( ).

    reported-zc_aprov_orc_ordem_pm = VALUE #(
                                        FOR ls_msg IN lo_action->solicita( ls_param )
                                           (
                                                %msg = new_message(
                                                       id       = ls_msg-id
                                                       number   = ls_msg-number
                                                       severity = CONV #( ls_msg-type )
                                                       v1       = ls_msg-message_v1
                                                       v2       = ls_msg-message_v2
                                                       v3       = ls_msg-message_v3
                                                       v4       = ls_msg-message_v4  )
                                           )
                                     ).

  ENDMETHOD.

  METHOD mudavalor.

    DATA(ls_param) = VALUE #( keys[ 1 ]-%param OPTIONAL ).
    DATA(lv_ordem) = VALUE #( keys[ 1 ]-aufnr OPTIONAL ).

    DATA(lo_action) = NEW zclpm_aprova_sup( ).

    reported-zc_aprov_orc_ordem_pm = VALUE #(
                                        FOR ls_msg IN lo_action->muda_valor( iv_ordem = lv_ordem
                                                                             is_param = ls_param )
                                           (
                                                %msg = new_message(
                                                       id       = ls_msg-id
                                                       number   = ls_msg-number
                                                       severity = CONV #( ls_msg-type )
                                                       v1       = ls_msg-message_v1
                                                       v2       = ls_msg-message_v2
                                                       v3       = ls_msg-message_v3
                                                       v4       = ls_msg-message_v4  )
                                           )
                                     ).

  ENDMETHOD.

  METHOD get_instance_features.

    DATA: lt_ordens TYPE zpmtt_aprov_orc_ordem_pm.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST08_APP'
      EXPORTING
        in_usuario = sy-uname
      IMPORTING
        et_ordens  = lt_ordens.

    SORT: lt_ordens BY aufnr equnr werks.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key>).

      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<fs_result>).
      <fs_result>-%tky = <fs_key>-%tky.

      READ TABLE lt_ordens ASSIGNING FIELD-SYMBOL(<fs_ordem>)
              WITH KEY aufnr = <fs_key>-aufnr
                       equnr = <fs_key>-equnr
                       werks = <fs_key>-werks
                       BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_ordem>-color = '1' AND <fs_ordem>-tipo = '2'.

          <fs_result>-%action-mudavalor = if_abap_behv=>fc-o-enabled.
        ELSE.
          <fs_result>-%action-mudavalor = if_abap_behv=>fc-o-disabled.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zc_aprov_orc_ordem_pm DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zc_aprov_orc_ordem_pm IMPLEMENTATION.

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
