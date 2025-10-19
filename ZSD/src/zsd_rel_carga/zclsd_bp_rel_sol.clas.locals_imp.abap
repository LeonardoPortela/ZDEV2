CLASS lhc_sol DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR sol RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ sol RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK sol.

    METHODS rba_cargas FOR READ
      IMPORTING keys_rba FOR READ sol\_cargas FULL result_requested RESULT result LINK association_links.

    METHODS rba_solfilha FOR READ
      IMPORTING keys_rba FOR READ sol\_solfilha FULL result_requested RESULT result LINK association_links.

    METHODS cba_cargas FOR MODIFY
      IMPORTING entities_cba FOR CREATE sol\_cargas.

    METHODS cba_solfilha FOR MODIFY
      IMPORTING entities_cba FOR CREATE sol\_solfilha.

ENDCLASS.

CLASS lhc_sol IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_cargas.
  ENDMETHOD.

  METHOD rba_solfilha.
  ENDMETHOD.

  METHOD cba_cargas.
  ENDMETHOD.

  METHOD cba_solfilha.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_cargas DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ cargas RESULT result.

    METHODS rba_ol FOR READ
      IMPORTING keys_rba FOR READ cargas\sol FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_cargas IMPLEMENTATION.

  METHOD read.
  ENDMETHOD.

  METHOD rba_ol.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_solfilhas DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ solfilhas RESULT result.

    METHODS rba_ol FOR READ
      IMPORTING keys_rba FOR READ solfilhas\sol FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_solfilhas IMPLEMENTATION.

  METHOD read.
  ENDMETHOD.

  METHOD rba_ol.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zi_sd_rel_solicitacao DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zi_sd_rel_solicitacao IMPLEMENTATION.

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
