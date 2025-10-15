CLASS lcl_utilities DEFINITION. "Utilidades para todos as clases.

  PUBLIC SECTION.

    CLASS-METHODS: calculate_days_by_datum
      IMPORTING
        iv_number_of_days TYPE i
      RETURNING
        VALUE(rv_date)    TYPE d.

    CLASS-METHODS: remover_zeros_esquerda
      IMPORTING
        iv_input TYPE clike
      EXPORTING
        ev_ouput TYPE clike.

    CLASS-METHODS: adicionar_zeros_esquerda
      IMPORTING
        iv_input TYPE clike
      EXPORTING
        ev_ouput TYPE clike.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_coupa_integration_log DEFINITION. "Classe responsável por administrar log de integração - Tabela ZINTEGRCOUPA01.

  PUBLIC SECTION.

    METHODS insert_new_log_key
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key.

    METHODS set_executed_status
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key.

    METHODS save_log.

    METHODS check_existence_of_key
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key
      RETURNING
        VALUE(rv_existence)       TYPE flag.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "Tabelas Internas
    DATA: gt_zintegrcoupa01 TYPE STANDARD TABLE OF zintegrcoupa01.

ENDCLASS.

CLASS lcl_supplier_data_coupa DEFINITION. "Responsabilidade -> Buscar dados fornecedores para importação Coupa.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_days  TYPE i
        iv_lifnr TYPE ace_generic_range_t.

    METHODS execute.

    METHODS get_import_data_create
      RETURNING VALUE(rt_import_data) TYPE zcoupa_supplier_create_tt.

    METHODS get_import_data_modify
      RETURNING VALUE(rt_import_data) TYPE zcoupa_supplier_modify_tt.

    "Objetos
    DATA: go_coupa_integration_log TYPE REF TO lcl_coupa_integration_log.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS build_data.

    "Variáveis
    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.
    "Tabelas internas
    DATA: gt_lifnr                 TYPE tt_lifnr,
          rg_lifnr                 TYPE ace_generic_range_t,
          gt_coupa_supplier_modify TYPE zcoupa_supplier_modify_tt,
          gt_coupa_supplier_create TYPE zcoupa_supplier_create_tt.
    "Estruturas
    DATA: gs_coupa_supplier_modify TYPE zcoupa_supplier_modify,
          gs_coupa_supplier_create TYPE zcoupa_supplier_create.

ENDCLASS.

CLASS lcl_alv_return DEFINITION. "ALV de retorno do processamento.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS append_new_line
      IMPORTING
        iv_import_data     TYPE zcoupa_import_data
        iv_integration_log TYPE zintegracao_log.

    METHODS display.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS config_alv_columns.

    METHODS config_alv_functions.

    METHODS config_alv_events.

    METHODS on_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.

    DATA: gt_alv_data TYPE zcoupa_import_data_alv_tab.

    DATA: go_alv     TYPE REF TO cl_salv_table,
          go_events  TYPE REF TO cl_salv_events_table,
          go_funct   TYPE REF TO cl_salv_functions,
          go_columns TYPE REF TO cl_salv_columns_table,
          go_column  TYPE REF TO cl_salv_column_table.

ENDCLASS.
