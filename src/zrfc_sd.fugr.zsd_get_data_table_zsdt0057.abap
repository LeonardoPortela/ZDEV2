FUNCTION ZSD_GET_DATA_TABLE_ZSDT0057 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_RET_ZSDT0057 TYPE  ZSDST0042
*"----------------------------------------------------------------------

  SELECT *
    FROM zsdt0057
    INTO CORRESPONDING FIELDS OF TABLE et_ret_zsdt0057.
  IF sy-subrc IS INITIAL.
    SORT et_ret_zsdt0057 BY tp_venda bezei.
  ENDIF.


ENDFUNCTION.
