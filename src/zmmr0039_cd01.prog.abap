CLASS lcl_hierarquia_organizacional DEFINITION. "Responsabilidade -> Receber um aprovador e buscar todos os gestores acima do mesmo, percorrendo toda estrutura organizacional.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        it_pernr TYPE ztt_pernr.

    METHODS build_data
      EXPORTING ev_dados_hierarquia TYPE zsdados_hierarquia_hcm.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gv_nivel            TYPE hrp1010-hilfm,
          gt_pernr            TYPE ztt_pernr,
          gs_dados_hierarquia TYPE zsdados_hierarquia_hcm.

ENDCLASS.

INTERFACE if_load_approver.

  METHODS load.

  METHODS get_aprovadores
    RETURNING VALUE(rt_aprovadores) TYPE zprovcoupa01_tt.

  DATA: go_hierarquia_organizacional TYPE REF TO lcl_hierarquia_organizacional.

  DATA: gt_aprovadores TYPE STANDARD TABLE OF zprovcoupa01.

ENDINTERFACE.

CLASS lcl_load_approver_k DEFINITION. "Responsabilidade -> Efetuar carga de aprovadores centro de custo

  PUBLIC SECTION.

    INTERFACES if_load_approver.

    METHODS constructor
      IMPORTING iv_begin_date TYPE erdat.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gt_range_date TYPE ace_ds_date_range_t.

ENDCLASS.

CLASS lcl_load_approver_e DEFINITION. "Responsabilidade -> Efetuar carga de aprovadores centro/estoque.

  PUBLIC SECTION.

    INTERFACES if_load_approver.

    METHODS constructor
      IMPORTING iv_begin_date TYPE erdat.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gt_range_date TYPE ace_ds_date_range_t.

ENDCLASS.

CLASS lcl_approver_table_handler DEFINITION. "Responsabilidade -> Gerenciar log e armazenar registros na tabela de aprovadores.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS apply_changes
      IMPORTING
        it_zprovcoupa01 TYPE zprovcoupa01_tt.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS save_log.

ENDCLASS.
