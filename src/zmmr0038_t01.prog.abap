"Classes
CLASS: lcl_invoice_data_coupa DEFINITION DEFERRED,
       lcl_alv_return         DEFINITION DEFERRED.
"Objetos
DATA: go_invoice_data_coupa TYPE REF TO lcl_invoice_data_coupa,
      go_alv_return         TYPE REF TO lcl_alv_return.
"Estruturas
DATA: gs_integracao_return      TYPE zintegracao_log,
      gs_zcoupa_integration_key TYPE zcoupa_integration_key.
"Tabelas
TABLES: ekbe.
