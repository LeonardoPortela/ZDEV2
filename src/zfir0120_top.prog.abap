TABLES: zfit0216,t001w.

TYPES:
  BEGIN OF ty_saldo,
    bukrs      TYPE zfit0216-bukrs,
    butxt      TYPE t001-butxt,
    werks      TYPE zfit0216-werks,
    name1      TYPE t001w-name1,
    periodo    TYPE char10,
    saldo_fixo TYPE zfit0216-saldo_fixo,
  END OF ty_saldo." zfit0216

DATA: o_alv    TYPE REF TO cl_salv_table,
      it_saida TYPE STANDARD TABLE OF ty_saldo INITIAL SIZE 0,
      wa_saida TYPE ty_saldo.


SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
  SELECT-OPTIONS:
                  p_werks   FOR t001w-werks,"OBLIGATORY,
                  p_gjahr FOR zfit0216-gjahr NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zyear,
                  p_monat FOR zfit0216-monat NO-EXTENSION NO INTERVALS MATCHCODE OBJECT z_help_meses.
SELECTION-SCREEN END OF BLOCK part1.

START-OF-SELECTION.

  CALL SCREEN '0100'.
