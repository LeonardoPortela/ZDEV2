*----------------------------------------------------------------------*
***INCLUDE LZDOC_MEMO_FORMO01 .
*----------------------------------------------------------------------*

TYPE-POOLS: zmemo.

*&---------------------------------------------------------------------*
*&      Module  TRAVACAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE travacampos OUTPUT.

  LOOP AT SCREEN.

    IF ( screen-name = 'ZDOC_MEMO_FORM-NR_MEMO_FORM' ) OR ( screen-name = 'ZDOC_MEMO_FORM-DT_LANCAMENTO' ).
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF ( screen-name = 'ZDOC_MEMO_FORM-BUKRS' ) OR ( screen-name = 'ZDOC_MEMO_FORM-BRANCH' ).
      IF NOT zdoc_memo_form-nr_memo_form IS INITIAL.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.                 " TRAVACAMPOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZGERANUMERO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zgeranumero INPUT.

  CHECK zdoc_memo_form-nr_memo_form IS INITIAL.

  DATA: vn_parametro TYPE zdoc_memo_form-nr_memo_form.

  SELECT MAX( nr_memo_form )
    INTO vn_parametro
    FROM zdoc_memo_form.

  IF vn_parametro IS INITIAL.
    vn_parametro = 1.
  ELSE.
    vn_parametro = vn_parametro + 1.
  ENDIF.

  zdoc_memo_form-dt_lancamento = sy-datum.
  zdoc_memo_form-nr_memo_form  = vn_parametro.

ENDMODULE.                 " ZGERANUMERO  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_INICIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_inicial INPUT.
  PERFORM verifica.
ENDMODULE.                 " ZVERIFICA_INICIAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_FINAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_final INPUT.
  PERFORM verifica.
ENDMODULE.                 " ZVERIFICA_FINAL  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica .

  "Implementar Validação Incial
  DATA : lt_zdoc_memo_form TYPE TABLE OF zdoc_memo_form WITH HEADER LINE,
         wa_zdoc_memo_form TYPE zdoc_memo_form,
         lt_zdoc_memorando TYPE TABLE OF zmemo_memorando,
         wa_zdoc_memorando TYPE zmemo_memorando,
         vg_representante  TYPE kunnr.

*NR_MEMO_FORM   Z_MEMORANDO NUMC  10  0 Número incremental de memorando
*BUKRS          BUKRS CHAR  4 0 Empresa
*BRANCH         J_1BBRANC_  CHAR  4 0 Local de negócios
*FORM_INICIAL   Z_MEMO_NUMERO NUMC  5 0 Número de Memorando de Exportação
*FORM_FINAL     Z_MEMO_NUMERO NUMC  5 0 Número de Memorando de Exportação
*DT_LANCAMENTO  J_1BDOCDAT  DATS  8 0 Data do documento

  DATA : nr_inicial      TYPE z_memo_numero,
         nr_final        TYPE z_memo_numero,
         nr_inicial_aux  TYPE z_memo_numero,
         nr_final_aux    TYPE z_memo_numero,
         nr_form_memo    TYPE z_memo_numero.

  IF zdoc_memo_form-form_inicial EQ 0 OR zdoc_memo_form-form_final EQ 0.
    MESSAGE e063(zmemorando).
  ELSE.
    IF zdoc_memo_form-form_inicial GE zdoc_memo_form-form_final.
      MESSAGE e063(zmemorando).
    ENDIF.
  ENDIF.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE lt_zdoc_memo_form
   FROM zdoc_memo_form AS z
   WHERE z~bukrs        EQ zdoc_memo_form-bukrs
     AND z~branch       EQ zdoc_memo_form-branch
     AND z~form_inicial BETWEEN zdoc_memo_form-form_inicial AND zdoc_memo_form-form_final
     AND nr_memo_form   NE zdoc_memo_form-nr_memo_form.

  IF lt_zdoc_memo_form[] IS NOT INITIAL.
    nr_inicial = zdoc_memo_form-form_inicial.
    nr_final   = zdoc_memo_form-form_final.
    CLEAR: zdoc_memo_form-form_inicial,
           zdoc_memo_form-form_final.
    MESSAGE e064(zmemorando).
  ENDIF.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE lt_zdoc_memo_form
   FROM zdoc_memo_form AS z
   WHERE z~bukrs        EQ zdoc_memo_form-bukrs
     AND z~branch       EQ zdoc_memo_form-branch
     AND z~form_final   BETWEEN zdoc_memo_form-form_inicial AND zdoc_memo_form-form_final
     AND nr_memo_form   NE zdoc_memo_form-nr_memo_form.

  IF lt_zdoc_memo_form[] IS NOT INITIAL.
    nr_inicial = zdoc_memo_form-form_inicial.
    nr_final   = zdoc_memo_form-form_final.
    CLEAR: zdoc_memo_form-form_inicial,
           zdoc_memo_form-form_final.
    MESSAGE e064(zmemorando).
  ENDIF.

  "Verifica se formulario alterado saiu do range de utilização de memorandos já lançados
  IF NOT zdoc_memo_form-nr_memo_form IS INITIAL.

    SELECT SINGLE *
      INTO wa_zdoc_memo_form
      FROM zdoc_memo_form
      WHERE nr_memo_form EQ zdoc_memo_form-nr_memo_form.

    nr_inicial = wa_zdoc_memo_form-form_inicial.
    nr_final   = wa_zdoc_memo_form-form_final.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zdoc_memo_form-branch
      IMPORTING
        output = vg_representante.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_zdoc_memorando
      FROM zdoc_memorando
      WHERE representante EQ vg_representante
        AND formulario    GE nr_inicial
        AND formulario    LE nr_final .

    SORT lt_zdoc_memorando BY nr_memorando.

    LOOP AT lt_zdoc_memorando INTO wa_zdoc_memorando.
      nr_form_memo = wa_zdoc_memorando-formulario.
      IF NOT ( nr_form_memo >= nr_inicial AND nr_form_memo <= nr_final ) .
        MESSAGE e062(zmemorando) WITH wa_zdoc_memorando-nr_memorando wa_zdoc_memorando-numero_memo.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VERIFICA
