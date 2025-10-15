"Name: \PR:SAPLIWOL\FO:CREATE_NOTIFICATION_F05\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CHECK_TP_NOTA.
*Check relacionameto ordem-> nota de manutenção.
**Implementação referente o projeto Agro PM - USER STORY 96113 / Anderson Oenning.

  data: RETURNLOG TYPE  BAPIRETURN1.

   "CHECK SET.
  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_PM_PERM_IW31'
        AND valfrom EQ @caufvd-BUKRS.

IF SY-SUBRC EQ 0.

IF caufvd-auart IS NOT INITIAL AND riwol0-qmart IS NOT INITIAL.


  CALL FUNCTION 'ZFPM_CHECK_RELAC_ORDEM'
   EXPORTING
     I_AUART       = caufvd-auart
     I_QMART       = riwol0-qmart
     I_WERKS       = caufvd-werks
   IMPORTING
     returnlog     = RETURNLOG.

  IF RETURNLOG-type EQ 'E'.
     message I024(SD)  with |Não é permitido esse tipo de nota |
                           |{ riwol0-qmart } para o tipo de ordem { caufvd-auart }|
                           |Verifique cadastro na ZPM0092| DISPLAY LIKE 'E' .
     SY-SUBRC = 0.
     exit.
  ENDIF.
 ENDIF.
ENDIF.
            .
**Implementação referente o projeto Agro PM - USER STORY 96113 / Anderson Oenning.
ENDENHANCEMENT.
