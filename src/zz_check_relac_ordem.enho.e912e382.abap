"Name: \PR:SAPLIQS0\FO:CHECK_CUSTOMER_CREDIT_F40\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CHECK_RELAC_ORDEM.
*Check relacionameto ordem-> nota de manutenção.
**Implementação referente o projeto Agro PM - USER STORY 96113 / Anderson Oenning.

  data: RETURNLOG TYPE  BAPIRETURN1,
        lv_auart TYPE AUFART.

  CLEAR: lv_auart.

   "CHECK SET.
  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_PM_PERM_IW31'
        AND valfrom EQ @VIQMEL-BUKRS.

IF SY-SUBRC EQ 0.

   IF caufvd-auart IS INITIAL AND viqmel-qmart IS NOT INITIAL.
      lv_auart = riwo00-auart.
    ELSE.
      lv_auart = caufvd-auart.
    ENDIF.

 IF lv_auart IS NOT INITIAL.
  CALL FUNCTION 'ZFPM_CHECK_RELAC_ORDEM'
   EXPORTING
     I_AUART       = lv_auart
     I_QMART       = viqmel-qmart
     I_WERKS       = VIQMEL-IWERK
   IMPORTING
     returnlog     = RETURNLOG.

  IF RETURNLOG-type EQ 'E'.
     message i024(SD)  with |Não é permitido esse tipo de nota |
                           |{ viqmel-qmart } para o tipo de ordem { lv_auart }|
                           |Verifique cadastro na ZPM0092| DISPLAY LIKE 'E'.

     p_exit = 8. EXIT.
  ENDIF.
   ENDIF.
ENDIF.
            .
**Implementação referente o projeto Agro PM - USER STORY 96113 / Anderson Oenning.
ENDENHANCEMENT.
