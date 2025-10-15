"Name: \PR:SAPMV13A\EX:MV13AO0C_01\EN:OIC_SAPMV13A\SE:END\EI
ENHANCEMENT 0 Z_BLOQUEIO_MODIFICACAO_TK13.
*
  IF SY-TCODE = 'TK13'.
    lv_tcode = 'TK12'.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
    IF sy-subrc NE 0.
      ausschluss = fcode_change. APPEND ausschluss.
      ausschluss = fcode_create. APPEND ausschluss.
      ausschluss = fcode_create_model. APPEND ausschluss.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
