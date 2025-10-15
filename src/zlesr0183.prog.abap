*&---------------------------------------------------------------------*
*& Report ZLESR0183
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlesr0183.

INCLUDE zlesr0183top.
INCLUDE zlesr0183f01.
INCLUDE zlesr0183o01.
INCLUDE zlesr0183i01.

START-OF-SELECTION.
  PERFORM f_valida.
  PERFORM f_monta_estrutura.
  PERFORM f_seleciona_dados.

  CALL SCREEN 9001.
