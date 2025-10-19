FUNCTION zsd_deliv_head_cust_validate .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     REFERENCE(EX_MESSAGE) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  CLEAR ex_message.

  IF gv_transporte IS INITIAL OR
     gv_incoterms IS INITIAL OR
     gv_cuit IS INITIAL OR
     gv_transportista IS INITIAL OR
     gv_documento IS INITIAL OR
     gv_chasis IS INITIAL OR
     gv_acoplado IS INITIAL.

    ex_message = text-e01.

  ENDIF.

ENDFUNCTION.
