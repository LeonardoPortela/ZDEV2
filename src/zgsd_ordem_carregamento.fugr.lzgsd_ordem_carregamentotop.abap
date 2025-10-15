FUNCTION-POOL zgsd_ordem_carregamento.      "MESSAGE-ID ..

*******************************************************************************************
* Tabelas
*******************************************************************************************
TABLES: zsdt0001od,
        zlest0185.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_texto,
         t001_butxt_rec       TYPE t001-butxt,
         j_1bbranch_name_rec  TYPE j_1bbranch-name,
         t001_butxt_age       TYPE t001-butxt,
         j_1bbranch_name_age  TYPE j_1bbranch-name,
         lfa1_name1_coleta    TYPE lfa1-name1,
         lfa1_name1_destino   TYPE lfa1-name1,
         kna1_name1_descarga  TYPE kna1-name1,
         mara_maktx           TYPE makt-maktx,
         lfa1_name1_motorista TYPE lfa1-name1.
TYPES: END   OF ty_texto.

*******************************************************************************************
* variaveis, tabelas
*******************************************************************************************
DATA: w_texto TYPE ty_texto,
      e_code  TYPE syst_ucomm.

*******************************************************************************************
*******************************************************************************************



* INCLUDE LZGSD_ORDEM_CARREGAMENTOD...       " Local class definition
