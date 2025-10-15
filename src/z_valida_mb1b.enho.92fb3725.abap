"Name: \PR:SAPMM07M\FO:FUSSZEILE_NEU\SE:BEGIN\EI
ENHANCEMENT 0 Z_VALIDA_MB1B.
*

  DATA: BEGIN OF WA_SETLEAF.
          INCLUDE STRUCTURE SETLEAF.
  DATA: END OF WA_SETLEAF.
  IF SY-TCODE EQ 'MB1B'.
   SELECT SINGLE *
      FROM SETLEAF
      INTO WA_SETLEAF
    WHERE SETNAME EQ 'MAGGI_CLASE_MOVIMIENTOS'
       AND VALFROM EQ rm07m-bwartwa.

   IF ( SY-SUBRC EQ 0 ).
     IF rm07m-MTSNR IS INITIAL.
        MESSAGE E000(Z01) WITH 'Notifique numero de prestamo o remito no campo'
                               'Vale de material.'.
* ---> S4 Migration - 19/07/2023 - DG
*      LEAVE TO TRANSACTION 'MB1B'.
      LEAVE TO TRANSACTION 'MIGO'.
* <--- S4 Migration - 19/07/2023 - DG3 - DG
     ENDIF.
   ENDIF.
ENDIF.

  if sy-tcode eq 'MB1A'
    AND rm07m-bwartwa IS NOT INITIAL.
      SELECT SINGLE *
        FROM SETLEAF
        INTO WA_SETLEAF
         WHERE SETNAME EQ 'MB1A_RESERVA'
           AND VALFROM EQ rm07m-bwartwa.

        IF SY-SUBRC IS INITIAL.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para o Tipo de movimento informado é obrigatório'
                                                 'ser baixado por reserva.'.
* ---> S4 Migration - 19/07/2023 - DG
*      LEAVE TO TRANSACTION 'MB1A'.
      LEAVE TO TRANSACTION 'MIGO'.
* <--- S4 Migration - 19/07/2023 - DG3 - DG
        ENDIF.
  ENDIF.

ENDENHANCEMENT.
