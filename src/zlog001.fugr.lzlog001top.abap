FUNCTION-POOL zlog001.                      "MESSAGE-ID ..

* INCLUDE LZLOG001D...                       " Local class definition

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: git_periodo_inatividade TYPE TABLE of ZLOG1001 WITH HEADER LINE.
DATA: git_periodo_atividade   TYPE TABLE of ZLOG1002 WITH HEADER LINE.

*DATA: BEGIN OF git_periodo_inatividade OCCURS 0,
*        usnam                     TYPE usnam,
*        erdat_ini                 TYPE erdat,
*        erzet_ini                 TYPE erzet,
*        erdat_fim                 TYPE erdat,
*        erzet_fim                 TYPE erzet,
*        tempo_inatividade_seconds TYPE int4.
*DATA: END OF git_periodo_inatividade.

*DATA: BEGIN OF git_total_inatividade OCCURS 0,
*        usnam                     TYPE usnam,
*        tempo_inatividade_seconds TYPE int4.
*DATA: END OF git_total_inatividade.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.


"DATA: git_dados_sm20n TYPE rsau_t_result.
