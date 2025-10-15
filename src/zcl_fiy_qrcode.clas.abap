class ZCL_FIY_QRCODE definition
  public
  final
  create public .

public section.

  methods SET_JSON_TO_OPUS
    importing
      value(I_URL) type STRING
      value(I_URL_TOKEN) type STRING
      value(I_JSON) type STRING
    returning
      value(R_RETORNO) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FIY_QRCODE IMPLEMENTATION.


  METHOD set_json_to_opus.
    DATA: e_reason   TYPE string.
    DATA: emptybuffer TYPE xstring.

    DATA(obj_webservice) = NEW zcl_webservice( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url                = i_url
      IMPORTING
        client             = DATA(e_http)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).


    emptybuffer = ''.
    CALL METHOD e_http->request->set_data
      EXPORTING
        data = emptybuffer.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.
    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json; charset=UTF-8'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Accept'
        value = 'application/json'.

    zcl_webservice=>zif_webservice~add_token_opus_http_cliente(
      EXPORTING
        i_url_destino              = i_url
        i_url_token                = i_url_token
      CHANGING
        i_http                     = e_http
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    obj_webservice->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
        i_xml                      = i_json
        i_not_content_length       = 'X'
      IMPORTING
        e_reason                   = e_reason
      RECEIVING
        e_resultado                = r_retorno
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

  ENDMETHOD.
ENDCLASS.
