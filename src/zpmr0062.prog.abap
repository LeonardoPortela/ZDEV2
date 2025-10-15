*&---------------------------------------------------------------------*
*& Report  ZPMR0062
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Abap         :  Anderson Oenning ( AO ) - Amaggi
*& Data         : 08/05/2020
*& Especialista : Cleudo Ferreira
*& Chamado/Descrição : CS2019001804 - Interface recebimento de dados referente abastecimento de combustivel frota Amaggi - Autotrac
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT zpmr0062.


START-OF-SELECTION.


  DATA: w_apont TYPE zpme0058.
*Consultar cadastro veiculo / Sistema Autotrac.
  zcl_int_sappm_autotrac=>i_cons_cadastro_veic(
    IMPORTING
      t_cadastro_veic =  DATA(t_cadastro_veic) "Retorno com dados de cadastro veiculos.
  ).

  IF t_cadastro_veic IS NOT INITIAL.
    "Buscando a ultima execução / Data e Hora.
    zcl_int_sappm_autotrac=>m_get_dt_hr_proc(
      IMPORTING
        i_hr_fim =  DATA(i_hr_fim)   " Campo do sistema ABAP: hora atual do servidor de aplicação
        i_hora   =  DATA(i_hr_inic)   " Campo do sistema ABAP: contador de loops
        i_id     =  DATA(i_id)   " ID
        i_data_fim = DATA(i_data_fim)
        i_data   =  DATA(i_data)   " Campo do sistema ABAP: data atual do servidor de aplicação
    ).

    zcl_int_sappm_autotrac=>i_cons_dados_abast(
      EXPORTING
        t_dados_veic       =   t_cadastro_veic  " Dados de abastecimento de combustivel
        i_data             =   i_data
        i_id               =   i_id
        i_hr_inic          =   i_hr_inic
        i_data_fim         =   i_data_fim
        i_hr_fim           =   i_hr_fim
      IMPORTING
        t_dados_abast_veic =     DATA(t_dados) " Estrutura de dados consumo de combustivel SAP x Autotrac
    ).
  ELSE.


    "Processa log.
    zcl_exc_apont_med=>set_doc(
    EXPORTING
    w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
    i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
    i_msgv1    =  'Falha na comunicação com Autotrac'   " Campo do sistema ABAP: variável da mensagem
    i_item     = '99'
    ).

    CLEAR: w_apont.
  ENDIF.
