*&---------------------------------------------------------------------*
*& Report ZTESTE_RFC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTESTE_RFC.


* Teste de envio de requisição sincrona para o CPI via CL_HTTP_CLIENT
*
* Prerequisitos:
* 1 - Importar os certificados digitais do tenant do CPI na STRUST, SSL Client (Standard)
* 2 - Criar o Destination na SM59 do tipo G, apontando para o endpoint do tenant do CPI.
*     Target Host = <tenant_id>-iflmap.hcisbt.us2.hana.ondemand.com
*     Service No = 443
*     Path Prefix = aqui pode ser vazio ou direto para seu serviço, ex: /http/SalesOrder
*     Aba Logon/Security, Informar Basic Authentication + Usuário e senha do CPI. SSL Active e SSL Certificate = SSL Client (Standard).


START-OF-SELECTION.

  data   lv_destination TYPE RFCDEST.
  data:   lv_abap_false TYPE  string ,
         EV_PATH_PREFIX type STRING.


  DATA:
    lo_client     TYPE REF TO if_http_client,
    lv_res_data_bin TYPE xstring,
    lv_res_data_str TYPE string,
    lv_req_data_bin TYPE xstring,
    lv_req_data_str TYPE string,
    lo_conv       TYPE REF TO cl_abap_conv_in_ce.

  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = lv_destination
    IMPORTING
      client                   = lo_client
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6
  ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

   CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = lv_destination
        authority_check         = lv_abap_false
      IMPORTING
        path_prefix             = ev_path_prefix
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
