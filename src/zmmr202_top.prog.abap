*&---------------------------------------------------------------------*
*& Include          ZFIS46_PARAMS_TOP
*&---------------------------------------------------------------------*

      TYPE-POOLS: esp1.

      DATA: o_alv    TYPE REF TO cl_salv_table,
            it_saida TYPE STANDARD TABLE OF zmmt0186 INITIAL SIZE 0,
            wa_saida TYPE zmmt0186.                         "ZMME0186.

      START-OF-SELECTION.

        CALL SCREEN '0100'.
