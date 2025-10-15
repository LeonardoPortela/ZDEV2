"Name: \PR:SAPMIEQ0\FO:LEAVE_TO_TRANSACTION\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_ENVIA_DADOS_IE01_IE02.

  "Desativar api mobman/ BUG SOLTO 187013 / AOENNING / 31/07/2025.

**  Begin of    #105966  FF
*IF sy-tcode = 'IE01' OR sy-tcode = 'IE02'.
*  IF sy-ucomm = 'BU'. "Salvar
*    WAIT UP TO 5 SECONDS.
*    IF equi-equnr IS NOT INITIAL.
*      SUBMIT zpmr0078
*       WITH s_equnr = equi-equnr
*       AND RETURN.
*      IF sy-tcode EQ 'IE01'.
*        MESSAGE s816(is) WITH sy-msgv1.
*      ELSEIF sy-tcode EQ 'IE02'.
*        MESSAGE s817(is) WITH sy-msgv1.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.
*ENDIF.
** End of FF

  "Desativar api mobman/ BUG SOLTO 187013 / AOENNING / 31/07/2025.

ENDENHANCEMENT.
