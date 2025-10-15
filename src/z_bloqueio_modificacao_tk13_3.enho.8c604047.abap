"Name: \PR:SAPMV130\FO:TRANSAKTIONS_INIT\SE:END\EI
ENHANCEMENT 0 Z_BLOQUEIO_MODIFICACAO_TK13_3.
*
  data: zlc_tcode TYPE sy-tcode.

  IF SY-TCODE = 'TK13'.
    zlc_tcode = 'TK12'.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD zlc_tcode.
    IF sy-subrc NE 0.
      ausschluss_call = fcode_change. APPEND ausschluss_call.
      ausschluss_call = fcode_create. APPEND ausschluss_call.
      ausschluss_call = fcode_create_model. APPEND ausschluss_call.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
