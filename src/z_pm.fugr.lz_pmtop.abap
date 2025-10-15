FUNCTION-POOL Z_PM.                         "MESSAGE-ID ..

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.


DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

*// Tipos de Dados
DATA: BEGIN OF it_saida OCCURS 0.
        INCLUDE STRUCTURE kaep_coac.
      DATA: END OF it_saida.

INCLUDE lz_pmd01.
