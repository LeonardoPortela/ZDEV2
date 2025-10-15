FUNCTION zws38_condicao_wf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(SENDER) TYPE  SIBFLPORB
*"     VALUE(EVENT) TYPE  SIBFEVENT
*"     VALUE(RECTYPE) TYPE  SWFERECTYP
*"     REFERENCE(EVENT_CONTAINER) TYPE REF TO
*"        IF_SWF_IFS_PARAMETER_CONTAINER
*"  EXCEPTIONS
*"      REQUISITION_TYPE_IS_RCC
*"----------------------------------------------------------------------
* requisition of purchase number (numero do documento de compras)
  DATA: requisition_number LIKE eban-banfn.
* requisition of purchase item (item do documento de compras)
  DATA: requisition_item LIKE eban-bnfpo.
* requisition of purchase type (tipo de documento de compras)
  DATA: requisition_type LIKE eban-bsart.
  DATA: container_if      TYPE REF TO  if_swf_cnt_container.
  DATA: save_container    TYPE STANDARD TABLE OF swcont,
        save_container_st TYPE swcont.
  DATA: lcx_exception     TYPE REF TO  cx_swf_ifs_exception.
*"----------------------------------------------------------------------


  TRY.
      container_if ?= event_container. " cast to full container interface.
    CATCH cx_root.
      container_if ?= cl_swf_ifs_container_base=>create( ).
      TRY.
          CALL METHOD container_if->import_from_param_container
            EXPORTING
              parameter_container    = event_container
              only_import_param      = space
              only_export_param      = space
              overwrite              = 'X'
              copy                   = space
              import_system_elements = 'X'
              import_null_values     = 'X'.
        CATCH cx_swf_cnt_container.
      ENDTRY.
  ENDTRY.

  TRY.
      CALL METHOD container_if->to_bor_container
        IMPORTING
          values = save_container.
    CATCH cx_swf_cnt_element INTO lcx_exception.
  ENDTRY.


  READ TABLE save_container INTO save_container_st
       WITH KEY element = '_EVT_OBJKEY'.
  IF sy-subrc = 0.
    requisition_number = save_container_st-value(10).
    requisition_item = save_container_st-value+10(5).
  ENDIF.

  SELECT SINGLE bsart
    FROM eban
    INTO requisition_type
    WHERE banfn = requisition_number AND
          bnfpo = requisition_item.

  IF requisition_type = 'RCC'.
    RAISE requisition_type_is_rcc.
  ENDIF.

* note: raising the exception cancels the workflow event coupling, eg.
* to the SAP workflow template WS00000038

ENDFUNCTION.
