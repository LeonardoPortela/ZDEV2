*&---------------------------------------------------------------------*
*& PoolMóds.        ZHCMR_BN0026
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_bn0026.

TYPES: BEGIN OF ty_saida,
         cname                 TYPE zhcms_websempre_func_nov_adm_s-cname, "Nome completo
         pernr                 TYPE char14, "zhcms_websempre_func_nov_adm_s-pernr, "Nº pessoal
         cpf_nr                TYPE zhcms_websempre_func_nov_adm_s-cpf_nr, "Número no Cadastro de Pessoa Física (CPF)
         gbdat                 TYPE zhcms_websempre_func_nov_adm_s-gbdat, "Data de nascimento
         fcnam_m               TYPE zhcms_websempre_func_nov_adm_s-fcnam_m, "Nome completo
         fcnam_p               TYPE zhcms_websempre_func_nov_adm_s-fcnam_p, "Nome completo
         famst                 TYPE zhcms_websempre_func_nov_adm_s-famst, "Chave para estado civil
         gesch                 TYPE zhcms_websempre_func_nov_adm_s-gesch, "Chave do gênero
         natio                 TYPE zhcms_websempre_func_nov_adm_s-natio, "Nacionalidade
         escol                 TYPE zhcms_websempre_func_nov_adm_s-escol, "Grau de instrução
         ident_nr              TYPE zhcms_websempre_func_nov_adm_s-ident_nr, "Número do Documento de Identidade
         es_emis               TYPE zhcms_websempre_func_nov_adm_s-es_emis, "Estado (UF)
         doc_issuer            TYPE zhcms_websempre_func_nov_adm_s-doc_issuer, "Órgão emissor do documento
         dt_emis               TYPE zhcms_websempre_func_nov_adm_s-dt_emis, "Data de Emissão Documento
         usrid_long            TYPE zhcms_websempre_func_nov_adm_s-usrid_long, "Comunicação identificação/nº descritivo
         ident_nr_s            TYPE zhcms_websempre_func_nov_adm_s-ident_nr_s, "Número do Documento de Identidade
         bopti                 TYPE zhcms_websempre_func_nov_adm_s-bopti, "Benefícios complementares opção plano de seguro saúde
         werks                 TYPE zhcms_websempre_func_nov_adm_s-werks, "Área de recursos humanos
         arearh                TYPE zhcms_websempre_func_nov_adm_s-arearh, "Nome
         kostl                 TYPE zhcms_websempre_func_nov_adm_s-kostl, "Centro de custo
         ccusto                TYPE zhcms_websempre_func_nov_adm_s-ccusto, "Denominação geral
         orgeh                 TYPE zhcms_websempre_func_nov_adm_s-orgeh, "Unidade organizacional
         uniorg                TYPE zhcms_websempre_func_nov_adm_s-uniorg, "Denominação de objeto
         data01                TYPE zhcms_websempre_func_nov_adm_s-data01, "Início da validade
         posicao               TYPE zhcms_websempre_func_nov_adm_s-posicao, "Posição
         descricao_posicao     TYPE zhcms_websempre_func_nov_adm_s-descricao_posicao, "Denominação de objeto
         cargo                 TYPE zhcms_websempre_func_nov_adm_s-cargo, "Cargo
         descricao_cargo       TYPE zhcms_websempre_func_nov_adm_s-descricao_cargo, "Denominação de objeto
         bukrs                 TYPE zhcms_websempre_func_nov_adm_s-bukrs, "Empresa
         hrca_company          TYPE zhcms_websempre_func_nov_adm_s-hrca_company, "Denominação de empresa ou sociedade
*         bet01                 TYPE zhcms_websempre_func_nov_adm_s-bet01, "Montante de rubrica salarial para pagamentos
*         periculosidade        TYPE zhcms_websempre_func_nov_adm_s-periculosidade, "Montante de rubrica salarial para pagamentos
         dt_cadastro           TYPE zhcms_websempre_func_nov_adm_s-dt_cadastro, "Data de modificação
         stat2                 TYPE zhcms_websempre_func_nov_adm_s-stat2, "Status da ocupação
         situacao              TYPE zhcms_websempre_func_nov_adm_s-situacao, "Texto com comprimento 40
         pernr_gestor_imediato TYPE zhcms_websempre_func_nov_adm_s-pernr_gestor_imediato, "Nº pessoal
         nome_gestor_imediato  TYPE zhcms_websempre_func_nov_adm_s-nome_gestor_imediato, "Nome completo
         pernr_gestor_mediato  TYPE zhcms_websempre_func_nov_adm_s-pernr_gestor_mediato, "Nº pessoal
         nome_gestor_mediato   TYPE zhcms_websempre_func_nov_adm_s-nome_gestor_mediato, "Nome completo
         stext                 TYPE zhcms_websempre_func_nov_adm_s-stext, "Denominação subinfotipo
         stras                 TYPE zhcms_websempre_func_nov_adm_s-stras, "Rua e nº
         hsnmr                 TYPE zhcms_websempre_func_nov_adm_s-hsnmr, "Nº
         posta                 TYPE zhcms_websempre_func_nov_adm_s-posta, "Identificação da habitação no edifício
         ort02                 TYPE zhcms_websempre_func_nov_adm_s-ort02, "Bairro
         ort01                 TYPE zhcms_websempre_func_nov_adm_s-ort01, "Cidade
         state                 TYPE zhcms_websempre_func_nov_adm_s-state, "Região (estado federado, província, county)
         pstlz                 TYPE zhcms_websempre_func_nov_adm_s-pstlz, "Código postal
       END   OF ty_saida.

DATA: ok_code TYPE sy-ucomm.

DATA: t_arearh    TYPE TABLE OF zhcms_werks,
      t_kostl     TYPE TABLE OF zhcms_kostl,
      t_uniorg    TYPE TABLE OF zhcms_uniorg,
      t_saida     TYPE TABLE OF ty_saida,
      t_saida_aux TYPE TABLE OF zhcmf_return_func_dem_str, "BUG - 151348 - CBRAND
      t_func      TYPE TABLE OF zhcms_websempre_func_nov_adm_s,
      t_depend    TYPE TABLE OF zhcms_websempre_func_dep_nov_s.

DATA: wa_arearh TYPE zhcms_werks,
      wa_kostl  TYPE zhcms_kostl,
      wa_uniorg TYPE zhcms_uniorg.

DATA: t_fieldcat  TYPE TABLE OF slis_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      wa_layout   TYPE slis_layout_alv.


*DATA: t_fldcat_emp TYPE lvc_t_fcat,
*      t_fldcat_dep TYPE lvc_t_fcat.

DATA: go_container_emp TYPE REF TO cl_gui_custom_container,
      go_container_dep TYPE REF TO cl_gui_custom_container,
      go_alv_emp       TYPE REF TO cl_gui_alv_grid,
      go_alv_dep       TYPE REF TO cl_gui_alv_grid.

DATA: alv             TYPE REF TO cl_salv_table,
      message         TYPE REF TO cx_salv_msg,
      functions       TYPE REF TO cl_salv_functions_list,
      columns         TYPE REF TO cl_salv_columns_table,
      column          TYPE REF TO cl_salv_column,
      layout_settings TYPE REF TO cl_salv_layout,
      display         TYPE REF TO cl_salv_display_settings,
      layout_key      TYPE salv_s_layout_key.

FIELD-SYMBOLS: <fs_dyn_tab> TYPE STANDARD TABLE.

CONSTANTS: c_u        TYPE c       VALUE 'U',
           c_pernr    TYPE pernr_d VALUE '70001085',
           c_tipo_sol TYPE c       VALUE '1'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row
                                                                          e_column
                                                                          es_row_no.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    REFRESH t_depend.
    READ TABLE <fs_dyn_tab> ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX e_row-index.

    CHECK sy-subrc EQ 0.
    ASSIGN COMPONENT 'CPF_NR' OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_value>).
    IF <fs_value> IS ASSIGNED.
      CALL FUNCTION 'ZHCMF_WEBSEMPRE_FUNC_DEP_NOV'
        EXPORTING
          cpf_nr   = <fs_value>
        TABLES
          it_saida = t_depend.

      go_alv_dep->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.                    "catch_hotspot_4
ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_begda TYPE begda,
              p_endda TYPE endda.

  SELECT-OPTIONS: s_arearh FOR wa_arearh-werks NO INTERVALS MODIF ID mod,
                  s_kostl  FOR wa_kostl-kostl  NO INTERVALS MODIF ID mod,
                  s_uniorg FOR wa_uniorg-orgeh NO INTERVALS MODIF ID mod.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check1 RADIOBUTTON GROUP g1 USER-COMMAND hide DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(70) TEXT-001.    "Inclusão de titulares e seus dependentes - Admissão
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check2 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(70) TEXT-002.    "Inclusão de titulares  - Opção
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check3 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(70) TEXT-003.    "Inclusão de dependentes - Opção
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check4 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(70) TEXT-004.    "Exclusão de titulares e seus dependentes  - Rescisão ou Opção
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check5 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(70) TEXT-005.    "Transferência de titulares - Acomodação
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  p_check6 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 3(70) TEXT-006.    "Transferência de titulares - Lotação
  SELECTION-SCREEN END OF LINE.
*
*  PARAMETERS: p_check1 RADIOBUTTON GROUP g1 ,                                "Inclusão de titulares e seus dependentes - Admissão
*              p_check2 RADIOBUTTON GROUP g1,                                  "Inclusão de titulares  - Opção
*              p_check3 RADIOBUTTON GROUP g1,                                 "Inclusão de dependentes - Opção
*              p_check4 RADIOBUTTON GROUP g1,                                 "Exclusão de titulares e seus dependentes  - Rescisão ou Opção
*              p_check5 RADIOBUTTON GROUP g1,                                 "Transferência de titulares - Acomodação
*              p_check6 RADIOBUTTON GROUP g1.                                 "Transferência de titulares - Lotação
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'MOD'.
      IF p_check1 IS INITIAL.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  PERFORM f_call_function.
  PERFORM f_show_alv.



  INCLUDE zhcmr_bn0026_o01                        .  " PBO-Modules
  INCLUDE zhcmr_bn0026_i01                        .  " PAI-Modules
  INCLUDE zhcmr_bn0026_f01                        .  " FORM-Routines
