class ZCL_MONITORA_SAP definition
  public
  final
  create public .

public section.

  data AT_AMBIENTE type SY-SYSID .

  methods GET_DUMPS .
  methods CRIA_ARTEFATO
    importing
      !JSON_REQUEST type STRING
    returning
      value(JSON_RETORNO) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MONITORA_SAP IMPLEMENTATION.


  METHOD cria_artefato.

    DATA: e_reason   TYPE string.

    CHECK ( json_request IS NOT INITIAL ).

    SELECT SINGLE * FROM zauth_webservice
      INTO @DATA(lwa_webservice)
      WHERE service = 'AZURE_BOARDS_PROBLEMA'.

    IF ( sy-subrc = 0 ).

      DATA(obj_webservice) = NEW zcl_webservice( ).

      cl_http_client=>create_by_url(
        EXPORTING
          url                = CONV #( lwa_webservice-url )
        IMPORTING
          client             = DATA(e_http)
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      CALL METHOD e_http->request->set_header_field( name  = '~request_method'  value = 'POST' ).
*      CALL METHOD e_http->request->set_header_field( name  = '~server_protocol' value = 'HTTP/1.1' ).
      CALL METHOD e_http->request->set_header_field( name  = 'Authorization' value = 'Basic amVhbi5hbnR1bmVzQGFtYWdnaS5jb20uYnI6em5mNjV3ZWVia25rZmZwYWF6emp5c3ZnbG5nN2FyaXlqdG9mbnZ3NTNmczVieGVnN2djcQ==' ).
      CALL METHOD e_http->request->set_header_field( name  = 'Content-Type' value = 'application/json-patch+json' ).
*      CALL METHOD e_http->request->set_header_field( name  = 'Accept' value = 'application/json' ).

      e_http->propertytype_logon_popup = if_http_client=>co_disabled.
      e_http->authenticate( username = CONV #( lwa_webservice-username )  password = CONV #( lwa_webservice-password ) ).


      obj_webservice->zif_webservice~consultar(
        EXPORTING
          i_http                     = e_http
          i_xml                      = json_request
        IMPORTING
          e_reason                   = e_reason
        RECEIVING
          e_resultado                = json_retorno
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).

    ENDIF.

  ENDMETHOD.


  METHOD get_dumps.

    DATA: lt_dumps           TYPE rsdumptab,
          lt_dumps_resumo    TYPE rsdumptab,
          lva_descricao_dump TYPE string,
          lva_json_output    TYPE string,
          lt_requisicao_item TYPE zbc_tb_monitora_req_body,
          lr_data            TYPE REF TO data.

    TYPES: ty_rg_programname  TYPE RANGE OF progname.
    TYPES: ty_rg_dumpid       TYPE RANGE OF dumpid.

*   Tipos de DUMP ignorados
    DATA(rg_dumpid_excecao) = VALUE ty_rg_dumpid(
      ( sign = 'I' option = 'EQ' low = 'MESSAGE_TYPE_X' )
      ( sign = 'I' option = 'EQ' low = 'RAISE_EXCEPTION' )
      ( sign = 'I' option = 'EQ' low = 'TYPELOAD_NEW_VERSION' )
      ( sign = 'I' option = 'EQ' low = 'LOAD_PROGRAM_TABLE_MISMATCH' )
      ( sign = 'I' option = 'EQ' low = 'LOAD_TYPE_VERSION_MISMATCH' )
      ( sign = 'I' option = 'EQ' low = 'LOAD_PROGRAM_CLASS_MISMATCH' )
    ).

*   Range de programas ignorados pelo monitoramento
    DATA(rg_program_excecao) = VALUE ty_rg_programname(
      ( sign = 'I' option = 'EQ' low = 'CL_GUI_FRONTEND_SERVICES' )
      ( sign = 'I' option = 'EQ' low = 'CL_GUI_FRONTEND_SERVICES======CP' )
    ).

    DATA(lva_data_base) = sy-datum.

*    Busca os dumps no dia informado
    CALL FUNCTION 'RS_ST22_GET_DUMPS'
      EXPORTING
        p_day        = lva_data_base
      IMPORTING
        p_infotab    = lt_dumps
      EXCEPTIONS
        no_authority = 1
        OTHERS       = 2.

    IF sy-subrc = 0.

      SELECT * FROM zbc_monitora_log
        INTO TABLE @DATA(lt_monitora_log)
        WHERE data = @lva_data_base
          AND tipo = 'DUMPS'
        ORDER BY id_dump.

*     Eliminar exceções listadas acima
      DELETE lt_dumps[] WHERE dumpid IN rg_dumpid_excecao[] OR
                              programname IN rg_program_excecao[].

      LOOP AT lt_dumps[] INTO DATA(lwa_dumps)
         GROUP BY ( dumpip = lwa_dumps-dumpid
                    size   = GROUP SIZE
                    index  = GROUP INDEX ) ASCENDING
                    REFERENCE INTO DATA(group_dump).

*      Buscar os dumps com mais de X ocorrências no dia
        IF ( group_dump->size >= 3 ) OR ( group_dump->dumpip = 'SYNTAX_ERROR' ).

          READ TABLE lt_monitora_log ASSIGNING FIELD-SYMBOL(<lfs_monitora>) WITH KEY id_dump = group_dump->dumpip.
*          Se bug já foi registrado, verificar quantidade
          IF ( sy-subrc = 0 )." AND ( <lfs_monitora>-dump_count = group_dump->size ).

            CONTINUE.

*          Caso não tenha sido registrado, criar "Problema" no TFS
          ELSE.

            DATA(lva_title) = |{ sy-sysid }: { group_dump->size } dumps - { group_dump->dumpip }|.
            DATA(lva_tags)  = |{ sy-sysid }, DUMP |.

            lt_dumps_resumo[] = lt_dumps[].
            DELETE lt_dumps_resumo[] WHERE dumpid <> group_dump->dumpip.

            LOOP AT GROUP group_dump ASSIGNING FIELD-SYMBOL(<lfs_dump>) WHERE dumpid = group_dump->dumpip.

              DATA(lva_hora)  = |{ <lfs_dump>-sytime+0(2) }:{ <lfs_dump>-sytime+2(2) }:{ <lfs_dump>-sytime+4(2) }|.
              lva_descricao_dump = |{ lva_descricao_dump } <b>Programa:</b> { <lfs_dump>-programname } <b>Usuario:</b> { <lfs_dump>-syuser } <b>Hora:</b> { lva_hora } <br>|.
              CLEAR: lva_hora.

              DATA(lwa_monitora_log) = VALUE zbc_monitora_log(
                    data         = lva_data_base
                    tipo         = 'DUMPS'
                    id_dump      = group_dump->dumpip
                    programname  = <lfs_dump>-programname
                    dump_count   = group_dump->size
                    usuario      = <lfs_dump>-syuser
                    artefato_tfs = ''               ).

            ENDLOOP.

            lt_requisicao_item = VALUE #(
              ( op = 'add' path = '/fields/System.Title'        from = 'null' value = lva_title  )
              ( op = 'add' path = '/fields/System.Description'  from = 'null' value = lva_descricao_dump  )
              ( op = 'add' path = '/fields/System.tags'         from = 'null' value = lva_tags  )
             ).

            lva_json_output = /ui2/cl_json=>serialize(
                  data = lt_requisicao_item
                  compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

            IF ( lva_json_output IS NOT INITIAL ).

              DATA(lva_json_retorno) = me->cria_artefato( json_request = lva_json_output ).

              IF ( lva_json_retorno IS NOT INITIAL ).

                lr_data = /ui2/cl_json=>generate( json = lva_json_retorno ).

                /ui2/cl_data_access=>create( ir_data = lr_data
                                 iv_component = 'id' )->value(
                                    IMPORTING ev_data = lwa_monitora_log-artefato_tfs ).

                CONDENSE lwa_monitora_log-artefato_tfs.

                IF ( lwa_monitora_log IS NOT INITIAL ).
                  MODIFY zbc_monitora_log FROM lwa_monitora_log.
                ENDIF.

              ENDIF.

            ENDIF.

            CLEAR: lva_title, lva_descricao_dump, lva_tags, lt_requisicao_item[], lva_json_output, lva_json_retorno.

          ENDIF.


        ENDIF.



      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
