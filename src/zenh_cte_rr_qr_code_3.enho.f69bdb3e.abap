"Name: \FU:J_1B_CTE_MAP_TO_XML\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_CTE_RR_QR_CODE_3.
*
** DATA lv_qr_code TYPE string.
**  DATA lv_dummy TYPE string.
**
**  IF i_nfdoc-MODEL = 57 AND i_nfdoc-REGIO ='RR'.
**
**    PERFORM fill_qr_code IN PROGRAM saplj_1b_cte IF FOUND
**      USING    i_nfdoc
**               i_active
**      CHANGING lv_qr_code.
**
**    IF lv_qr_code IS INITIAL.
**      MESSAGE e569(j1b_nfe) WITH i_active-docnum INTO lv_dummy.
**      CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'
**        EXPORTING
**          i_docnum = i_active-docnum.
**    ENDIF.
**
**
**  ENDIF.


ENDENHANCEMENT.
