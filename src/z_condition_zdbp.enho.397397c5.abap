"Name: \FU:J_1B_MAP_CONDITION_VALUES\SE:END\EI
ENHANCEMENT 0 Z_CONDITION_ZDBP.
*
  data: W_CAMPO(40),
        VBSART type ekko-BSART,
        VBEDNR TYPE ekpo-BEDNR,
        W_EKPO TYPE EKPO.
*
*  FIELD-SYMBOLS: <FS_EBELN>  TYPE ANY.
*
*  W_CAMPO = '(SAPLMR1M)RM08M-EBELN'.
*  ASSIGN (W_CAMPO) TO <FS_EBELN>.
*  IF <FS_EBELN> IS ASSIGNED.
*      SELECT SINGLE bsart into VBSART from ekko where ebeln = <FS_EBELN>.
*      if VBSART = 'ZDBP'.
*          SELECT SINGLE BEDNR INTO VBEDNR FROM EKPO WHERE EBELN = <FS_EBELN>.
*          SELECT SINGLE * INTO W_EKPO FROM EKPO WHERE EBELN = VBEDNR.
*          IF W_EKPO-KNTTP = ''.
*              LOOP AT mm_iv_data  INTO ls_mm_iv_data.
**                IF ls_mm_iv_data-MATKL is INITIAL and ls_mm_iv_data-menge = 0.
**                    delete GT_TAXDATA where buzei = ls_mm_iv_data-buzei.
**                ENDIF.
*                IF ls_mm_iv_data-MATKL is NOT INITIAL and ls_mm_iv_data-menge gt 0. "Elimina
*                    delete GT_TAXDATA where buzei = ls_mm_iv_data-buzei.
*                ENDIF.
*              ENDLOOP.
*          ENDIF.
*      endif.
*  ENDIF.
ENDENHANCEMENT.
