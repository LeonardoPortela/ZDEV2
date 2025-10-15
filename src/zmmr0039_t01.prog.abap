"Interfaces
INTERFACE if_load_approver DEFERRED.
"Classes
CLASS: lcl_load_approver_k        DEFINITION DEFERRED,
       lcl_load_approver_e        DEFINITION DEFERRED,
       lcl_approver_table_handler DEFINITION DEFERRED.
"Objetos
DATA: go_load_approver          TYPE REF TO if_load_approver,
      go_approver_table_handler TYPE REF TO lcl_approver_table_handler.
"Variáveis
DATA: gv_object_reference TYPE c LENGTH 60.
