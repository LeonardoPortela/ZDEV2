FUNCTION Z_WF_SD_DESBLOQUEIO_REMESSA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(SALESDOCUMENT) LIKE  BAPIVBELN-VBELN
*"     VALUE(ORDERHEADERIN) LIKE  BAPISDH1 STRUCTURE  BAPISDH1 OPTIONAL
*"     VALUE(ORDERHEADERINX) LIKE  BAPISDH1X STRUCTURE  BAPISDH1X
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  data RET2 TYPE BAPIRET2.

  ORDERHEADERINX-DLV_BLOCK = 'X'.
  ORDERHEADERINX-UPDATEFLAG = 'U'.
  ORDERHEADERIN-DLV_BLOCK = ' '.
  ORDERHEADERINX-CUST_GROUP = 'X'.
  ORDERHEADERIN-CUST_GROUP = ' '.


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument               = salesdocument
     ORDER_HEADER_IN             = ORDERHEADERIN
      order_header_inx            = ORDERHEADERINX
  tables
    return                      = RETURN.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
   EXPORTING
     WAIT          = 'X'
   IMPORTING
     RETURN        = RET2.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
