*&---------------------------------------------------------------------*
*& Report  ZFIR085
*& Rodar o Relatório de Conferências (ZFIR0079) e gerar arquivo TXT
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir085.

TABLES: zhrst_efd_e1250m.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs FOR zhrst_efd_e1250m-bukrs NO INTERVALS  NO-EXTENSION,
                p_budat FOR zhrst_efd_e1250m-budat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  IF ( p_budat[] IS INITIAL ).
    MESSAGE 'Data de Lançamento é obrigatória!' TYPE 'e'.
    EXIT.
  ENDIF.

  if p_bukrs is INITIAL.
    p_bukrs[] = VALUE #( ( sign = 'I' option = 'NE' low = '9999' ) ).
   ENDIF.


  SUBMIT zfir0079
  WITH p_bukrs IN p_bukrs
  WITH p_budat IN p_budat
  WITH anal = abap_true
  WITH tds = abap_true
  WITH p_txt = abap_true AND RETURN.
