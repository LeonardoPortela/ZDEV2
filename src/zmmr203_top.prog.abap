data: it_screen_status type table of sy-ucomm.

TYPES: BEGIN OF ty_saida,
         matnr      TYPE matnr, "A
         zepi_ca    TYPE atwrt30, "B
         zepi_peri  TYPE atwrt30, "C
         zepi_valca TYPE atwrt30, "D
         status     TYPE icon-name, "E
         menssagem  TYPE atwrt30, "F
       END OF ty_saida.

data: o_alv           type ref to cl_salv_table,
       it_ZAA005 TYPE STANDARD TABLE OF zaa005 INITIAL SIZE 0,
      wa_ZAA005 TYPE zaa005,
      it_saida        type standard table of ty_saida initial size 0,
      wa_saida        type standard table of ty_saida initial size 0,
      t_salv          type standard table of ref to cl_salv_table.
