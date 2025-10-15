FUNCTION Z_01_DRE_STATUS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(TEXTO) DEFAULT SPACE
*"     REFERENCE(P_TOTAL) TYPE  I
*"     REFERENCE(P_POSICAO) TYPE  I
*"----------------------------------------------------------------------

  DATA: VMSG(100),
        P_PERCENTAGE(20),
        P_PERCE TYPE I.

  MOVE TEXTO TO VMSG.

  IF P_TOTAL NE 0.
    P_PERCE = ( P_POSICAO * 100 ) / P_TOTAL.
  ENDIF.

  WRITE P_PERCE TO P_PERCENTAGE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = P_PERCENTAGE
      TEXT       = VMSG.

ENDFUNCTION.
