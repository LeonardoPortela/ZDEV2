"Interfaces.
INTERFACE if_process_lookup_values DEFERRED.
"Classes.
CLASS lcl_coupa_integration_log DEFINITION DEFERRED.
CLASS lcl_alv_return            DEFINITION DEFERRED.
"Objetos.
DATA: go_process_lookup_values TYPE REF TO if_process_lookup_values,
      go_alv_return            TYPE REF TO lcl_alv_return.
"Variáveis.
DATA: gv_object_reference TYPE c LENGTH 40,
      gv_job_name         TYPE tbtcm-jobname,
      gv_job_count        TYPE tbtcm-jobcount.
"Tipos.
TYPES: BEGIN OF ty_csks,
         kokrs TYPE csks-kokrs,
         kostl TYPE csks-kostl,
       END OF ty_csks.
TYPES: BEGIN OF ty_ska1,
         ktopl TYPE ska1-ktopl,
         saknr TYPE ska1-saknr,
       END OF ty_ska1.
"Tabelas internas
DATA: gt_import_data TYPE zcoupa_import_data_tab.
"Estruturas
DATA: gs_integracao_return      TYPE zintegracao_log,
      gs_zcoupa_integration_key TYPE zcoupa_integration_key,
      var_ordem                 TYPE afko-aufnr.
