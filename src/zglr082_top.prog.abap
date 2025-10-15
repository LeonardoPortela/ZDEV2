*&---------------------------------------------------------------------*
*& Include          ZFIS46_PARAMS_TOP
*&---------------------------------------------------------------------*

      TYPE-POOLS: esp1.

    "DATA: wa_saida TYPE zglt0114.

        DATA: o_alv    TYPE REF TO cl_salv_table,
          it_saida TYPE STANDARD TABLE OF zglt0114 INITIAL SIZE 0,
          wa_saida TYPE zglt0114.

      START-OF-SELECTION.

        CALL SCREEN '0100'.
