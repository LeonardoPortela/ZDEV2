"Tabelas
TABLES: lfa1.
"Tipos
TYPES: BEGIN OF ty_lifnr,
         lifnr TYPE lfa1-lifnr,
       END OF ty_lifnr.
"Tipos de tabela
TYPES: tt_lifnr TYPE STANDARD TABLE OF ty_lifnr.
"Classes
CLASS lcl_supplier_data_coupa DEFINITION DEFERRED.
CLASS lcl_alv_return          DEFINITION DEFERRED.
"Objetos
DATA: go_supplier_data_coupa TYPE REF TO lcl_supplier_data_coupa,
      go_alv_return          TYPE REF TO lcl_alv_return.
"Estruturas
DATA: gs_integracao_return      TYPE zintegracao_log,
      gs_import_data            TYPE zcoupa_import_data,
      gs_zcoupa_integration_key TYPE zcoupa_integration_key.
"Tabelas internas
DATA: gt_import_data TYPE zcoupa_import_data_tab.
