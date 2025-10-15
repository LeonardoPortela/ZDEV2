FUNCTION ZHTTP_RET_DS_STATUS_RESPONSE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CODE) TYPE  CHAR03
*"  EXPORTING
*"     REFERENCE(E_DESC_STATUS) TYPE  STRING
*"----------------------------------------------------------------------

  CLEAR: E_DESC_STATUS.

  "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status

  CASE I_CODE.
    WHEN '400'.
      E_DESC_STATUS = 'Bad Request'.
    WHEN '401'.
      E_DESC_STATUS = 'Unauthorized'.
    WHEN '402'.
      E_DESC_STATUS = 'Payment Required'.
    WHEN '403'.
      E_DESC_STATUS = 'Forbidden'.
    WHEN '404'.
      E_DESC_STATUS = 'Not Found'.
    WHEN '405'.
      E_DESC_STATUS = 'Method Not Allowed'.
    WHEN '406'.
      E_DESC_STATUS = 'Not Acceptable'.
  ENDCASE.



ENDFUNCTION.
