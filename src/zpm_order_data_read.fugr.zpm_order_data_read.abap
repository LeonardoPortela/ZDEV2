FUNCTION zpm_order_data_read.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(ORDER_NUMBER) TYPE  AUFK-AUFNR OPTIONAL
*"  TABLES
*"      IAFVGD STRUCTURE  AFVGD
*"----------------------------------------------------------------------

  CALL FUNCTION 'PM_ORDER_DATA_READ'
    EXPORTING
      order_number    = order_number
    TABLES
      iafvgd          = iafvgd
    EXCEPTIONS
      order_not_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFUNCTION.
