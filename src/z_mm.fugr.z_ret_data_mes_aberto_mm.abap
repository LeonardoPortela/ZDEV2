FUNCTION Z_RET_DATA_MES_ABERTO_MM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DATA_ENT) TYPE  DATUM
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(P_DATA_VAL) TYPE  DATUM
*"  EXCEPTIONS
*"      PERIODO_NAO_ENCONTRADO
*"      SEM_PERIODO
*"----------------------------------------------------------------------

  DATA: WA_MARV TYPE MARV,
        P_DATA_MP TYPE DATUM,
        P_ANO     TYPE C LENGTH 4,
        P_MES     TYPE C LENGTH 2.


  CLEAR: P_DATA_VAL.

*  CALL FUNCTION 'MR_PERIOD_DETERMINE'
*    EXPORTING
*      i_bukrs                = p_bukrs
*      i_budat                = p_data_ent
*    EXCEPTIONS
*      invalid_posting_period = 1
*      marv_no_entry          = 2
*      OTHERS                 = 3.
*
*  IF sy-subrc IS INITIAL.
*    p_data_val = p_data_ent.
*  ENDIF.
*
*  CHECK p_data_val IS INITIAL.

  SELECT SINGLE * INTO WA_MARV
    FROM MARV
   WHERE BUKRS EQ P_BUKRS.

  IF SY-SUBRC IS INITIAL.

    IF NOT WA_MARV-XRUEM IS INITIAL.
      P_ANO = WA_MARV-VMGJA.
      P_MES = WA_MARV-VMMON.
      CONCATENATE P_ANO P_MES '01' INTO P_DATA_MP.

      IF ( P_DATA_ENT(4) = P_ANO ) AND ( P_DATA_ENT+4(2) = P_MES ).
        P_DATA_VAL = P_DATA_ENT.
      ELSEIF  P_DATA_MP GE P_DATA_ENT  . " >=
        P_DATA_VAL = P_DATA_MP.
      ENDIF.

    ENDIF.

    CHECK P_DATA_VAL IS INITIAL.

    P_ANO = WA_MARV-LFGJA.
    P_MES = WA_MARV-LFMON.
    CONCATENATE P_ANO P_MES '01' INTO P_DATA_MP.

    IF ( P_DATA_ENT(4) = P_ANO ) AND ( P_DATA_ENT+4(2) = P_MES ).
      P_DATA_VAL = P_DATA_ENT.
    ELSEIF P_DATA_MP GE P_DATA_ENT.
      P_DATA_VAL = P_DATA_MP.
    ENDIF.

    IF P_DATA_VAL IS INITIAL.
      "MESSAGE E002 RAISING SEM_PERIODO WITH P_DATA_ENT.
    ENDIF.

  ELSE.
    "MESSAGE E002 RAISING PERIODO_NAO_ENCONTRADO WITH P_BUKRS.
  ENDIF.

ENDFUNCTION.
