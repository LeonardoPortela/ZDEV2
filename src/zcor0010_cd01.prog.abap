INTERFACE if_load_account_balance. "Responsabilidade -> Gerenciar as diversas formas que existem de se obter o saldo de contas contábeis

  METHODS build_account_balance
    RETURNING VALUE(rt_saldo_contas) TYPE zsco_saldo_contas_tt.

ENDINTERFACE.

CLASS lcl_account_balance_ativo DEFINITION. "Responsabilidade -> Buscar o saldo de contas contábeis de ativo passivo.

  PUBLIC SECTION.

    INTERFACES if_load_account_balance.

    METHODS constructor
      IMPORTING
        iv_empresas TYPE ace_ds_bukrs_range_t
        iv_ano      TYPE gjahr
        iv_mes      TYPE month.

  PROTECTED SECTION.

  PRIVATE SECTION.

    "Métodos
    METHODS sum_values_per_month
      IMPORTING
        iv_saldo_contas TYPE zde_fi_gl_saldo_faglflext
      EXPORTING
        ev_indicador_dc TYPE c
      RETURNING
        VALUE(rt_saldo) TYPE hslvt12.

    METHODS set_contas_busca.

    "Variáveis
    DATA: gv_ano   TYPE gjahr,
          gv_mes   TYPE month,
          gv_meses TYPE i.

    "Tabelas internas
    DATA: gt_contas_busca TYPE zttco_saldo_contas_busca,
          gt_empresas     TYPE ace_ds_bukrs_range_t.

ENDCLASS.

CLASS lcl_account_balance_resultado DEFINITION. "Responsabilidade -> Buscar o saldo de contas contábeis de resultado

  PUBLIC SECTION.

    INTERFACES if_load_account_balance.

    METHODS constructor
      IMPORTING
        iv_empresas TYPE ace_ds_bukrs_range_t
        iv_ano      TYPE gjahr
        iv_mes      TYPE month.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS load_accounts_zglt041
      IMPORTING
                iv_classificacao_balanco TYPE ace_generic_range_t
                iv_classificacao_nota    TYPE ace_generic_range_t
      RETURNING VALUE(rt_zglt041)        TYPE ztt_zglt041.

    METHODS load_balance_note_base
      IMPORTING
        iv_versn   TYPE versn_011
      EXPORTING
        et_niveis  TYPE ztt_zglt047
        et_balance TYPE ace_generic_range_t
        et_note    TYPE ace_generic_range_t.

    METHODS load_balance_note_cost_center
      IMPORTING
        it_balance             TYPE ace_generic_range_t
        it_note                TYPE ace_generic_range_t
      EXPORTING
        et_balance_cost_center TYPE ace_generic_range_t
        et_note_cost_center    TYPE ace_generic_range_t
        et_type_cost_center    TYPE ace_generic_range_t
        et_zglt049c            TYPE zglt049c_tt.

    METHODS load_balance_note_profit_c
      IMPORTING
        it_balance               TYPE ace_generic_range_t
        it_note                  TYPE ace_generic_range_t
      EXPORTING
        et_balance_profit_center TYPE ace_generic_range_t
        et_note_profit_center    TYPE ace_generic_range_t
        et_profit_center         TYPE ace_generic_range_t
        et_account_aerea         TYPE ace_generic_range_t
        et_zglt049l              TYPE zglt049l_tt.

    METHODS load_balance_note_mat_group
      IMPORTING
        it_balance                TYPE ace_generic_range_t
        it_note                   TYPE ace_generic_range_t
      EXPORTING
        et_balance_material_group TYPE ace_generic_range_t
        et_note_material_group    TYPE ace_generic_range_t
        et_material_group         TYPE ace_generic_range_t
        et_zglt049m               TYPE zglt049m_tt.

    METHODS load_saldo_mensal_dre
      IMPORTING
        it_accounts   TYPE ztt_zglt041
        it_kosar      TYPE ace_generic_range_t OPTIONAL
        it_prctr      TYPE ace_generic_range_t OPTIONAL
        it_matkl      TYPE ace_generic_range_t OPTIONAL
      EXPORTING
        et_dre_values TYPE ztt_zglt_dre_02.

    METHODS sum_values_centro_custo
      IMPORTING
        it_dre_values TYPE ztt_zglt_dre_02
      EXPORTING
        et_sum_values TYPE zglt_dre_cc_tt.

    METHODS sum_values_centro_lucro
      IMPORTING
        it_dre_values TYPE ztt_zglt_dre_02
      EXPORTING
        et_sum_values TYPE zglt_dre_cl_tt.

    METHODS sum_values_grupo_mercadoria
      IMPORTING
        it_dre_values TYPE ztt_zglt_dre_02
      EXPORTING
        et_sum_values TYPE zglt_dre_gm_tt.

    METHODS sum_values_razao
      IMPORTING
        it_dre_values      TYPE ztt_zglt_dre_02
        it_zglt049c        TYPE zglt049c_tt
        it_zglt049l        TYPE zglt049l_tt
        it_zglt049m        TYPE zglt049m_tt
        it_accounts_reason TYPE ztt_zglt041
        it_cc_processado   TYPE zglt_conta_centro_custo_tt
        it_cl_processado   TYPE zglt_conta_centro_lucro_tt
        it_gm_processado   TYPE zglt_conta_grupo_mercadoria_tt
      EXPORTING
        et_sum_values      TYPE zglt_dre_r_tt.

    METHODS set_contas_busca.

    "Métodos
    METHODS sum_values_per_month
      IMPORTING
        iv_saldo_contas TYPE zde_fi_gl_saldo_faglflext
      EXPORTING
        ev_indicador_dc TYPE c
      RETURNING
        VALUE(rt_saldo) TYPE hslvt12.

    "Variáveis
    DATA: gv_ano     TYPE gjahr,
          gv_mes     TYPE month,
          gv_meses   TYPE ace_generic_range_t,
          gv_meses_i TYPE i.

    "Tabelas internas
    DATA: gt_empresas     TYPE ace_ds_bukrs_range_t,
          gt_contas_busca TYPE zttco_saldo_contas_busca.

    DATA: gt_zglt099 TYPE STANDARD TABLE OF zglt099,
          gt_bsis    TYPE tt_bsis.

ENDCLASS.

CLASS lcl_alv_saldo_contas_eventos DEFINITION. "Responsabilidade - Lidar com eventos do ALV de balancente contábil.

  PUBLIC SECTION.
    METHODS on_function_code FOR EVENT added_function OF cl_salv_events.

ENDCLASS.

CLASS lcl_alv_builder DEFINITION. "Responsabilidade - Demonstrar ALV.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS build_alv
      CHANGING rt_table TYPE ANY TABLE.

    METHODS config_alv_header
      IMPORTING
        iv_text   TYPE string
        iv_row    TYPE i
        iv_column TYPE i
        iv_label  TYPE xflag OPTIONAL
        iv_flow   TYPE xflag OPTIONAL.

    METHODS config_alv_functions
      IMPORTING iv_pf_status TYPE sypfkey
                iv_report    TYPE syrepid
                iv_functions TYPE salv_de_constant.

    METHODS config_columns
      IMPORTING iv_set_optimize        TYPE sap_bool
                iv_set_striped_pattern TYPE sap_bool
                iv_set_list_header     TYPE lvc_title.

    METHODS display.

    METHODS config_events.

    METHODS config_sort_columns
      IMPORTING iv_column   TYPE lvc_fname
                iv_sequence TYPE salv_de_sort_sequence.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: go_alv    TYPE REF TO cl_salv_table,
          go_header TYPE REF TO cl_salv_form_layout_grid.

ENDCLASS.
