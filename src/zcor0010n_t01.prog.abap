"Tabelas
TABLES: t001.
"Interfaces
INTERFACE if_load_account_balance DEFERRED.
"Classes
CLASS: lcl_account_balance_ativo     DEFINITION DEFERRED,
       lcl_account_balance_resultado DEFINITION DEFERRED,
       lcl_alv_builder               DEFINITION DEFERRED,
       lcl_alv_saldo_contas_eventos  DEFINITION DEFERRED.
"Objetos
DATA: go_saldo_contas     TYPE REF TO if_load_account_balance,
      go_saldo_contas_alv TYPE REF TO lcl_alv_builder.
"Tipos
TYPES: BEGIN OF ty_alv_header,
         text   TYPE string,
         row    TYPE i,
         column TYPE i,
         label  TYPE xflag,
         flow   TYPE xflag,
       END OF ty_alv_header.

TYPES: BEGIN OF ty_bsis,
         bukrs TYPE bsis-bukrs,
         hkont TYPE bsis-hkont,
         belnr TYPE bsis-belnr,
         gjahr TYPE bsis-gjahr,
         buzei TYPE bsis-buzei,
         dmbtr TYPE bsis-dmbtr,
         dmbe2 TYPE bsis-dmbe2,
         dmbe3 TYPE bsis-dmbe3,
         shkzg TYPE bsis-shkzg,
       END OF ty_bsis.
"Tipo de tabelas.
TYPES: tt_alv_header TYPE STANDARD TABLE OF ty_alv_header,
       tt_bsis       TYPE STANDARD TABLE OF ty_bsis.
"Tabelas internas
DATA: gt_alv_header   TYPE tt_alv_header,
      gs_file_name    TYPE string,
      gt_saldo_contas TYPE zsco_saldo_contas_tt.
