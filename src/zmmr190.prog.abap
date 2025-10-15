*&---------------------------------------------------------------------*
*& Report ZMMR190
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr190.

DATA: aux_but050  TYPE STANDARD TABLE OF but050 WITH HEADER LINE,
      aux_but051  TYPE STANDARD TABLE OF but051 WITH HEADER LINE,
      aux_but052  TYPE STANDARD TABLE OF but052 WITH HEADER LINE,
      aux_ZMMR190 TYPE STANDARD TABLE OF zmmr190 WITH HEADER LINE,
      txt_msg     TYPE text150,
      qtd         TYPE int8,
      qtdtot      TYPE int8,
      aux_qtd     TYPE int8.


**********************************************************************
*BUT050 START
SELECT a~* FROM but050 AS a
INTO TABLE @aux_but050[]
WHERE NOT EXISTS ( SELECT * FROM but000 AS b WHERE b~partner = a~partner2 ).

IF  aux_but050[] IS NOT INITIAL.

  CLEAR: aux_qtd.

  LOOP AT aux_but050[] ASSIGNING FIELD-SYMBOL(<but050>).

    aux_qtd = aux_qtd + 1.

    DELETE FROM but050 WHERE relnr = <but050>-relnr AND partner1 = <but050>-partner1 AND partner2 = <but050>-partner2 AND date_to = <but050>-date_to.
  ENDLOOP.

  aux_ZMMR190-dt = sy-datum.
  aux_ZMMR190-hr = sy-uzeit.
  aux_ZMMR190-tb = 'BUT050'.
  aux_ZMMR190-qtd = aux_qtd.

  qtdtot = qtdtot + aux_ZMMR190-qtd.

  INSERT zmmr190 FROM aux_zmmr190.

  CLEAR: aux_ZMMR190-dt,aux_ZMMR190-hr,aux_ZMMR190-tb,aux_ZMMR190-qtd.

ENDIF.
*BUT050 END
**********************************************************************

*BUT051 START
SELECT a~* FROM but051 AS a
INTO TABLE @aux_but051[]
WHERE NOT EXISTS ( SELECT * FROM but000 AS b WHERE b~partner = a~partner2 ).

IF  aux_but051[] IS NOT INITIAL.

  CLEAR: aux_qtd.

  LOOP AT aux_but051[] ASSIGNING FIELD-SYMBOL(<but051>).

    aux_qtd = aux_qtd + 1.

    DELETE FROM but051 WHERE relnr = <but051>-relnr AND partner1 = <but051>-partner1 AND partner2 = <but051>-partner2 AND date_to = <but051>-date_to.
  ENDLOOP.

  aux_ZMMR190-dt = sy-datum.
  aux_ZMMR190-hr = sy-uzeit.
  aux_ZMMR190-tb = 'BUT051'.
  aux_ZMMR190-qtd = aux_qtd.

  qtdtot = qtdtot + aux_ZMMR190-qtd.

  INSERT zmmr190 FROM aux_zmmr190.

  CLEAR: aux_ZMMR190-dt,aux_ZMMR190-hr,aux_ZMMR190-tb,aux_ZMMR190-qtd.

ENDIF.
*BUT051 END
**********************************************************************

*BUT052 START
SELECT a~* FROM but052 AS a
INTO TABLE @aux_but052[]
WHERE NOT EXISTS ( SELECT * FROM but000 AS b WHERE b~partner = a~partner2 ).

IF  aux_but052[] IS NOT INITIAL.

  CLEAR: aux_qtd.

  LOOP AT aux_but052[] ASSIGNING FIELD-SYMBOL(<but052>).

    aux_qtd = aux_qtd + 1.

    DELETE FROM but052 WHERE relnr = <but052>-relnr AND partner1 = <but052>-partner1 AND partner2 = <but052>-partner2 AND date_to = <but052>-date_to.
  ENDLOOP.

  aux_ZMMR190-dt = sy-datum.
  aux_ZMMR190-hr = sy-uzeit.
  aux_ZMMR190-tb = 'BUT052'.
  aux_ZMMR190-qtd = aux_qtd.

  qtdtot = qtdtot + aux_ZMMR190-qtd.

  INSERT zmmr190 FROM aux_zmmr190.

  CLEAR: aux_ZMMR190-dt,aux_ZMMR190-hr,aux_ZMMR190-tb,aux_ZMMR190-qtd.

ENDIF.
*BUT052 END
**********************************************************************


qtdtot = qtdtot + aux_ZMMR190-qtd.

txt_msg = |Foram excluidos o total de { qtdtot } registros!|.

MESSAGE txt_msg TYPE 'I' DISPLAY LIKE 'S'.

clear: qtdtot.
