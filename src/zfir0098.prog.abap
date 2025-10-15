**********************************************************************
*                         Consultoria                                *
**********************************************************************
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 09.12.2022                                              *
* Descrição: Exit para alimentar conta conciliacao fornecedor        *
* Report   : ZFIR0098                                                *
**********************************************************************
* Projeto  : CS2022000535
**********************************************************************

**********************************************************************
* recuperar conta parametrizada
**********************************************************************
FORM f_recupera_conta   USING p_bukrs
                              p_lifnr
                              p_stcd1
                              p_stcd2
                              p_mensagem
                     CHANGING p_akont.

  SELECT SINGLE akont
    INTO p_akont
    FROM lfb1
   WHERE lifnr = p_lifnr
     AND bukrs = p_bukrs.

  IF sy-subrc = 0 AND p_akont IS NOT INITIAL.
    EXIT.
  ENDIF.

*---------------------
* parametro
*---------------------
  SELECT *
    INTO @DATA(w_0190)
    FROM zfit0190
      UP TO 1 ROWS
   WHERE bukrs     = @p_bukrs
     AND cpf       = @p_stcd2
     AND cnpj_raiz = @abap_off.
  ENDSELECT.

  IF sy-subrc = 0.
    p_akont = w_0190-akont.
    IF p_mensagem = abap_true.
      MESSAGE i024(sd) WITH 'Fornecedor parte Relacionada!'.
    ENDIF.
  ELSE.
    SELECT *
      INTO w_0190
      FROM zfit0190
        UP TO 1 ROWS
     WHERE bukrs     = p_bukrs
       AND cpf       = abap_off
       AND cnpj_raiz = p_stcd1(8).
    ENDSELECT.

    IF sy-subrc = 0.
      p_akont = w_0190-akont.
      IF p_mensagem = abap_true.
        MESSAGE i024(sd) WITH 'Fornecedor parte Relacionada!'.
      ENDIF.
    ELSE.
      "Verificar se cadastro esta com alguma conta parametrizada.
      SELECT *
           INTO w_0190
           FROM zfit0190
             UP TO 1 ROWS
          WHERE akont     = p_akont.
      ENDSELECT.
      IF sy-subrc EQ 0.
*        IF p_mensagem = abap_true.
*        p_mensagem = abap_true.
        MESSAGE e024(sd) WITH 'Cta.de reconciliação' p_akont 'não permitida'.
        CLEAR: p_akont.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
**********************************************************************
