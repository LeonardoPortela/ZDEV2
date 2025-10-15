INTERFACE zif_cria_modifica_nota_mobman
  PUBLIC .


  CLASS-DATA at_cria_modifica_nota_mobman TYPE REF TO zif_cria_modifica_nota_mobman .
  CLASS-DATA at_servico TYPE string .
  DATA at_json TYPE string .
  DATA at_comboio TYPE zpmt0058_t .
  CLASS-DATA at_return TYPE bapiret2 .
  CLASS-DATA at_dados_abast TYPE zpmt0058 .
  CLASS-DATA at_epto TYPE equnr .
  CLASS-DATA at_material TYPE matnr .
  CLASS-DATA at_deposito TYPE station_t .
  CLASS-DATA at_ponto_medicao TYPE z_imptt_t .
  CLASS-DATA at_centro TYPE werks_d .
  CLASS-DATA at_doc_med TYPE zimrg_docm_t .
  CLASS-DATA at_date TYPE sy-datum .
  CLASS-DATA at_hora TYPE sy-uzeit .
  CLASS-DATA at_dados_equipamento TYPE bapi_itob .
  CLASS-DATA at_status_proc TYPE char01 .
  CLASS-DATA at_combust TYPE zde_comboio_abastecimento_t .
  CLASS-DATA at_nota TYPE zepm_d_nota .
  CLASS-DATA AT_atividades TYPE ZPM_t_atividades .

  CLASS-METHODS get_instance
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao .
  METHODS set_ds_url
    IMPORTING
      !i_ativ                          TYPE char1 OPTIONAL
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao .
  METHODS set_ds_data
    IMPORTING
      !i_json                               TYPE string
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao .
  METHODS set_int_comb
    RETURNING
      VALUE(r_if_integracao_comb) TYPE REF TO zif_integracao_comb
    RAISING
      zcx_integracao
      zcx_error .
  METHODS get_json
    EXPORTING
      !e_json                               TYPE string
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao .
  "FF - 24/04/2024 - inicio #131818
  METHODS get_json_ativ
    EXPORTING
      !e_json                               TYPE string
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao .
  "FF - 24/04/2024 - fim #131818

  METHODS set_send_msg
    EXPORTING
      !e_id_integracao                      TYPE zde_id_integracao
      !e_integracao                         TYPE zintegracao
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao
      zcx_error .
  METHODS set_id_referencia
    RETURNING
      VALUE(r_if_permissao_usuario) TYPE REF TO zif_permissao_usuarios .
  METHODS get_id_referencia
    EXPORTING
      !e_referencia                 TYPE zde_chave_referencia
    RETURNING
      VALUE(r_if_permissao_usuario) TYPE REF TO zif_permissao_usuarios .
  METHODS get_int_comb
    EXPORTING
      !e_id_integracao            TYPE zde_id_integracao
      !e_integracao               TYPE zintegracao
      !e_comboio                  TYPE zde_comboio_abastecimento_t
    RETURNING
      VALUE(r_if_integracao_comb) TYPE REF TO zif_integracao_comb
    RAISING
      zcx_integracao
      zcx_error .
  METHODS set_processa_transferencia
    IMPORTING
      !i_comboio                  TYPE zpmt0058_t
    RETURNING
      VALUE(r_if_integracao_comb) TYPE REF TO zif_integracao_comb
    RAISING
      zcx_integracao
      zcx_error .
  METHODS set_processa_consumo
    IMPORTING
      !i_comboio                  TYPE zpmt0058_t
    RETURNING
      VALUE(r_if_integracao_comb) TYPE REF TO zif_integracao_comb .
  METHODS check_veiculo .
  METHODS check_material .
  METHODS check_periodo .
  METHODS check_deposito .
  METHODS set_log
    IMPORTING
      !i_dados        TYPE zpmt0058
      VALUE(i_return) TYPE bapiret2 .
  METHODS set_ponto_medicao .
  METHODS set_dados_eqpto
    IMPORTING
      !i_dados TYPE zpmt0058 .
  METHODS estorna_doc_medicao .
  METHODS proc_contador_odom_hom .
  METHODS proc_baixa_estoque .
  METHODS regist_justif
    IMPORTING
      VALUE(i_line)     TYPE char72 OPTIONAL
      VALUE(w_zpmt0058) TYPE zpmt0058 OPTIONAL
      VALUE(i_odometro) TYPE imrc_cntrc OPTIONAL .
  METHODS set_ds_url_put
    RETURNING
      VALUE(r_if_integracao_comb) TYPE REF TO zif_integracao_comb .
  METHODS post_cria_modifica_nota_mobman
    IMPORTING
      !i_nota                               TYPE zepm_d_nota
      !i_atividades                         TYPE zpm_t_atividades OPTIONAL
    EXPORTING
      !e_id_integracao                      TYPE zde_id_integracao
      !e_integracao                         TYPE zintegracao
      !e_data                               TYPE string
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman
    RAISING
      zcx_integracao
      zcx_error .
  METHODS monta_json
    IMPORTING
      !i_comboio    TYPE zde_comboio_abastecimento_t
    RETURNING
      VALUE(e_json) TYPE string .
  METHODS set_dados_nota
    IMPORTING
      !i_data                               TYPE zepm_d_nota
      !I_ATIVIDADEs                         TYPE zpm_t_atividades OPTIONAL
    RETURNING
      VALUE(r_if_cria_modifica_nota_mobman) TYPE REF TO zif_cria_modifica_nota_mobman .
ENDINTERFACE.
