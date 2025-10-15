FUNCTION ZUPDATE_CH_REF_IN_LIKP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBRK-VBELN
*"     REFERENCE(I_CH_REF) TYPE  ZCH_REF
*"  EXCEPTIONS
*"      DOCUMENT_BLOCKED
*"      UPDATE_NO_SUCCESS
*"----------------------------------------------------------------------

  CONSTANTS: NOT_PRINTED LIKE VBRK-XBLNR VALUE ' '.

  DATA:      V_CH_REFERENCIA LIKE LIKP-BEROT.

*Should XBLNR be checked first? If so,additional function call necessary

  V_CH_REFERENCIA = I_CH_REF.

  IF V_CH_REFERENCIA IS INITIAL.
    V_CH_REFERENCIA = NOT_PRINTED.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EVVBLKE'
       EXPORTING
            VBELN          = I_VBELN
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

  IF SY-SUBRC <> 0.
    RAISE DOCUMENT_BLOCKED.
  ENDIF.

  UPDATE LIKP SET BEROT = V_CH_REFERENCIA
   WHERE VBELN = I_VBELN.

  IF SY-SUBRC <> 0.
    RAISE UPDATE_NO_SUCCESS.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_EVVBLKE'
       EXPORTING
            VBELN     = I_VBELN
            _SCOPE    = '3'
            _SYNCHRON = 'X'.

ENDFUNCTION.
