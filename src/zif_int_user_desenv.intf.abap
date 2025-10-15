INTERFACE zif_int_user_desenv
  PUBLIC .


  TYPES:
    BEGIN OF ty_saida,
      code           TYPE     char10, "pa0000-pernr,
      name           TYPE     pa0002-cname,
      login          TYPE     pa0465-cpf_nr,
      password       TYPE     pa0465-cpf_nr,
      status         TYPE     char1,
      email          TYPE     string,
      phone          TYPE     char1,
      positioncode   TYPE     char10,
      locationcode   TYPE     pa0001-kostl,
      stationcode    TYPE     char1,
      stationname    TYPE     char1,
      teamcode       TYPE     char10, "pa0001-orgeh,
      teamname       TYPE     hrp1000-stext,
      teamleadercode TYPE     char10, "pa9002-pernimed,
*      profilecode    TYPE     char4,
    END OF ty_saida .
  TYPES:
    tyt_saida TYPE STANDARD TABLE OF ty_saida .

  CONSTANTS gc_service TYPE /ui2/service_name VALUE 'DESENV_INT_ENVIA_USER' ##NO_TEXT.
  CONSTANTS gc_token TYPE /ui2/service_name VALUE 'DESENV_INT_TOKEN' ##NO_TEXT.
  CLASS-DATA at_servico TYPE /ui2/service_name .
  DATA at_struct TYPE tyt_saida .
  DATA at_auth_ws TYPE zauth_webservice .
  DATA at_token_ws TYPE zauth_webservice .
  DATA at_body TYPE string .

  METHODS get_instance .
  METHODS set_req_compra_buscar .
  METHODS set_ds_url
    RETURNING
      VALUE(r_if_int_user_desenv) TYPE REF TO zif_int_user_desenv .
  METHODS set_xml .
  METHODS set_ds_data
    RETURNING
      VALUE(r_if_int_user_desenv) TYPE REF TO zif_int_user_desenv .
  METHODS set_id_referencia
    RETURNING
      VALUE(r_if_int_user_desenv) TYPE REF TO zif_int_user_desenv .
  METHODS get_id_referencia
    EXPORTING
      !e_referencia               TYPE zde_chave_referencia
    RETURNING
      VALUE(r_if_int_user_desenv) TYPE REF TO zif_int_user_desenv .
  METHODS set_send_msg
    EXPORTING
      !e_id_integracao            TYPE zde_id_integracao
      !e_integracao               TYPE zintegracao
    RETURNING
      VALUE(r_if_int_user_desenv) TYPE REF TO zif_int_user_desenv .
  METHODS set_servico
    IMPORTING
      !i_servico TYPE /ui2/service_name .
  METHODS enviar_desenvolve
    IMPORTING
      !it_saida             TYPE tyt_saida
    EXPORTING
      !ev_return_code       TYPE string
      !ev_return_msg        TYPE string
    RETURNING
      VALUE(r_return_value) TYPE string
    RAISING
      zcx_integracao
      zcx_error .
ENDINTERFACE.
