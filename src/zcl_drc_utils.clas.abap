class ZCL_DRC_UTILS definition
  public
  final
  create public .

public section.

  class-methods GET_XML_DOCUMENTO_ELETRONICO
    importing
      !I_CHAVES type ZCHAVES_DOC_ELETRONICOS optional
      !I_CHAVE type ZDE_CHAVE_DOC_E optional
      !I_DIRECAO type CHAR03
      !I_EVENTO type NUMC06 optional
    exporting
      value(E_XMLS) type ZXMLS_DOC_ELETRONICOS
      !E_XML_RAW type EDOC_FILE
    returning
      value(R_XML) type STRING .
  class-methods GET_XML_INBOUND_DRC
    importing
      !I_CHAVES type ZCHAVES_DOC_ELETRONICOS
      !I_EVENTO type NUMC06 optional
    returning
      value(R_XMLS) type ZXMLS_DOC_ELETRONICOS .
  class-methods GET_XML_GRC
    importing
      !I_CHAVES type ZCHAVES_DOC_ELETRONICOS
      !I_DIRECAO type CHAR03
      !I_EVENTO type NUMC06 optional
    returning
      value(R_XMLS) type ZXMLS_DOC_ELETRONICOS .
  class-methods GET_XML_OUTBOUND_DRC
    importing
      !I_CHAVES type ZCHAVES_DOC_ELETRONICOS
      !I_EVENTO type NUMC06 optional
    returning
      value(R_XMLS) type ZXMLS_DOC_ELETRONICOS .
  class-methods EXCLUDE_EDOC_NOT_VALIDATED
    importing
      !I_DISCARD_CANCELED type CHAR01 optional
    changing
      !C_EDOCUMENTS type EDOCUMENT_TAB .
  class-methods EXCLUDE_EDOC_TRANSFERED_TO_ZIB
    changing
      !C_EDOCUMENTS type EDOCUMENT_TAB .
  class-methods GET_HOST_PORT_URL_DOCUMENTO
    changing
      !C_HTTP_URL type IHTTPURLS2 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DRC_UTILS IMPLEMENTATION.


  METHOD exclude_edoc_not_validated.

    "Documentos Autorizados
    DATA: lra_proc_status_nfe TYPE RANGE OF edocument-proc_status,
          lra_edoc_type_nfe   TYPE RANGE OF edocument-edoc_type,
          lra_proc_status_cte TYPE RANGE OF edocument-proc_status,
          lra_edoc_type_cte   TYPE RANGE OF edocument-edoc_type.

    "Documentos Cancelados
    DATA: lra_proc_status_nfe_c TYPE RANGE OF edocument-proc_status,
          lra_edoc_type_nfe_c   TYPE RANGE OF edocument-edoc_type,
          lra_proc_status_cte_c TYPE RANGE OF edocument-proc_status,
          lra_edoc_type_cte_c   TYPE RANGE OF edocument-edoc_type.


    CHECK c_edocuments[] IS NOT INITIAL.

    CLEAR: lra_proc_status_nfe[],
           lra_edoc_type_nfe[],
           lra_proc_status_cte[],
           lra_edoc_type_cte[],

           lra_proc_status_nfe_c[],
           lra_edoc_type_nfe_c[],
           lra_proc_status_cte_c[],
           lra_edoc_type_cte_c[].

*------------------------------------------------------------------------------------------*
*   Filtros NF-e Autorizada
*------------------------------------------------------------------------------------------*
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_IN_BP'  ) TO lra_edoc_type_nfe.
*** US #173081 - MMSILVA - 07.07.2025 - Ini ***
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_IN_EVT' ) TO lra_edoc_type_nfe.
*** US #173081 - MMSILVA - 07.07.2025 - Fim ***

    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'STA_AUTH'      ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'XML_OK'        ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NFE_ACCPT'     ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'DANFE_ENT'     ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NFE_AUTH'      ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'ERP_OK'        ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'ERP_NOK'       ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'COMP_OK'       ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'COMP_MAN'      ) TO lra_proc_status_nfe.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'MD_SENT'       ) TO lra_proc_status_nfe.

*------------------------------------------------------------------------------------------*
*   Filtros CT-e Autorizada
*------------------------------------------------------------------------------------------*

    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_INCTEF' ) TO lra_edoc_type_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_INCTEOF' ) TO lra_edoc_type_cte.

    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'STA_AUTH'    ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIBF_OK'   ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIBF_ERR'  ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'DACTE_ENT'   ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'CTE_AUTH'    ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIAF_OK'   ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIAF_ERR'  ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NOTIF_SENT'  ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NOTIF_ERR'   ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NOTIF_CANC'  ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NOTIF_NSNT'  ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'COMP_OK'     ) TO lra_proc_status_cte.
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'COMP_MAN'    ) TO lra_proc_status_cte.


*------------------------------------------------------------------------------------------*
*   Filtros NF-e Cancelada
*------------------------------------------------------------------------------------------*
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_IN_CP' ) TO lra_edoc_type_nfe_c.

*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'SIGN_OK'     ) TO lra_proc_status_nfe_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'STA_AUTH'    ) TO lra_proc_status_nfe_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'XML_ARRIV'   ) TO lra_proc_status_nfe_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIC_OK'    ) TO lra_proc_status_nfe_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NO_DELIV'    ) TO lra_proc_status_nfe_c.

*------------------------------------------------------------------------------------------*
*   Filtros CT-e Cancelado
*------------------------------------------------------------------------------------------*
    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BR_INCTECP' ) TO lra_edoc_type_cte_c.

*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'SIGN_OK'     ) TO lra_proc_status_cte_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'STA_AUTH'    ) TO lra_proc_status_cte_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'XML_ARRIV'   ) TO lra_proc_status_cte_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'BADIC_OK'    ) TO lra_proc_status_cte_c.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low =  'NO_DELIV'    ) TO lra_proc_status_cte_c.

*------------------------------------------------------------------------------------------*
*   Filtros CT-e/CT-e Cancelado
*------------------------------------------------------------------------------------------*

*------------------------------------------------------------------------------------------*
*   Inicio aplicãção Filtros
*------------------------------------------------------------------------------------------*

    DATA(lit_edocuments)     = c_edocuments[].
    DATA(lit_edocuments_aux) = c_edocuments[].

    LOOP AT lit_edocuments_aux INTO DATA(lwa_edocuments_aux).

      DATA(_validated) = abap_false.

      "Documento NF-e Autorizado
      IF ( lwa_edocuments_aux-edoc_type   IN lra_edoc_type_nfe  AND
           lwa_edocuments_aux-proc_status IN lra_proc_status_nfe ).

        _validated = abap_true.


        "Documento CTe-e Autorizado
      ELSEIF ( lwa_edocuments_aux-edoc_type   IN lra_edoc_type_cte AND
               lwa_edocuments_aux-proc_status IN lra_proc_status_cte ).

        _validated = abap_true.


      ENDIF.

      IF _validated EQ abap_false AND i_discard_canceled EQ abap_false.

        "Documento NF-e Cancelado
        IF ( lwa_edocuments_aux-edoc_type   IN lra_edoc_type_nfe_c  AND
             lwa_edocuments_aux-proc_status IN lra_proc_status_nfe_c ).

          _validated = abap_true.

          "Documento CT-e Cancelado
        ELSEIF ( lwa_edocuments_aux-edoc_type   IN lra_edoc_type_cte_c  AND
                 lwa_edocuments_aux-proc_status IN lra_proc_status_cte_c ).

          _validated = abap_true.

        ENDIF.

      ENDIF.

      IF _validated NE abap_true.
        DELETE lit_edocuments WHERE edoc_guid = lwa_edocuments_aux-edoc_guid.
      ENDIF.
    ENDLOOP.


    c_edocuments = lit_edocuments[].

  ENDMETHOD.


  METHOD exclude_edoc_transfered_to_zib.


    TYPES: BEGIN OF ty_access_key,
             edoc_guid      TYPE edobrincoming-edoc_guid,
             accesskey      TYPE edobrincoming-accesskey,
             event_sequence TYPE edobrevent-event_sequence, "SMC US #173081 - 15-09-2025
           END   OF ty_access_key.

    DATA: lit_access_key TYPE TABLE OF ty_access_key.

    DATA: lva_status_xml TYPE zib_nfe_forn-st_nota.
    CONSTANTS: c_status_xml_auth TYPE zib_nfe_forn-st_nota VALUE 1.

    CHECK c_edocuments[] IS NOT INITIAL.

    DATA(lit_edocument) = c_edocuments[].

    SELECT edoc_guid accesskey
      FROM edobrincoming
      INTO TABLE lit_access_key
       FOR ALL ENTRIES IN lit_edocument
     WHERE edoc_guid = lit_edocument-edoc_guid.

    SELECT edoc_guid accesskey
      FROM edobrcteincoming
      APPENDING TABLE lit_access_key
       FOR ALL ENTRIES IN lit_edocument
     WHERE edoc_guid = lit_edocument-edoc_guid.

*** US #173081 - MMSILVA - 08.07.2025 - Ini ***
    SELECT edoc_guid
           accesskey
           event_sequence "SMC - US #173081 15-09-2025
      FROM edobrevent
      APPENDING TABLE lit_access_key
      FOR ALL ENTRIES IN lit_edocument
      WHERE edoc_guid = lit_edocument-edoc_guid.
*** US #173081 - MMSILVA - 08.07.2025 - Fim ***

    IF lit_access_key[] IS NOT INITIAL.
      SELECT nu_chave , st_nota, nu_chave_modelo
        FROM zib_nfe_forn INTO TABLE @DATA(lit_zib_nfe_forn)
         FOR ALL ENTRIES IN @lit_access_key
       WHERE nu_chave EQ @lit_access_key-accesskey.

      DATA(lit_zib_nfe_forn_55) = lit_zib_nfe_forn[].
      DELETE lit_zib_nfe_forn_55 WHERE nu_chave_modelo NE '55'.

      IF lit_zib_nfe_forn_55[] IS NOT INITIAL.
        SELECT chave_nfe
          FROM zib_nfe_dist_itm INTO TABLE @DATA(lit_zib_dist_itm)
           FOR ALL ENTRIES IN @lit_zib_nfe_forn_55
         WHERE chave_nfe EQ @lit_zib_nfe_forn_55-nu_chave.
      ENDIF.

*** US #173081 - MMSILVA - 08.07.2025 - Ini ***
      SELECT chave_nfe ,
             seq_evento "SMC - #173081 15-09-2025
        FROM zib_nfe_dist_avb
        INTO TABLE @DATA(lit_zib_dist_avb)
        FOR ALL ENTRIES IN @lit_access_key
        WHERE chave_nfe EQ @lit_access_key-accesskey.
*** US #173081 - MMSILVA - 08.07.2025 - Fim ***
    ENDIF.

    SORT lit_access_key   BY edoc_guid.
    SORT lit_zib_nfe_forn BY nu_chave st_nota.

    LOOP AT lit_edocument INTO DATA(lwa_edocument).

      DATA(lva_nao_transferir) = abap_false.

      READ TABLE lit_access_key INTO DATA(lwa_access_key) WITH KEY edoc_guid = lwa_edocument-edoc_guid.
      IF sy-subrc NE 0.
        lva_nao_transferir = abap_true.
      ELSE.

        IF ( lwa_edocument-edoc_type  = 'BR_IN_CP'    OR  "Processo Cancelamento NF-e
             lwa_edocument-edoc_type  = 'BR_INCTECP' ).   "Processo Cancelamento CT-e

          lva_status_xml = '2'.

          "Se for cancelamento, só replicar para as tabelas ZIB o status de cancelamento
          "se já tiver ocorrido a replicação dos dados da autorização anteriormente.
          READ TABLE lit_zib_nfe_forn INTO DATA(lwa_zib_nfe) WITH KEY nu_chave = lwa_access_key-accesskey
                                                                      st_nota  = c_status_xml_auth.
          IF sy-subrc NE 0.
            lva_nao_transferir = abap_true.
          ENDIF.

        ELSE.
          lva_status_xml = '1'.
        ENDIF.

        READ TABLE lit_zib_nfe_forn INTO lwa_zib_nfe WITH KEY nu_chave = lwa_access_key-accesskey
                                                              st_nota  = lva_status_xml.

        IF sy-subrc EQ 0. "Documento já transferido para tabelas ZIB
          IF lva_status_xml = '1' AND lwa_zib_nfe-nu_chave+20(2) = '55'.
            READ TABLE lit_zib_dist_itm WITH KEY chave_nfe = lwa_zib_nfe-nu_chave TRANSPORTING NO FIELDS.
            IF sy-subrc EQ 0.
              lva_nao_transferir = abap_true.
            ENDIF.
          ELSE.
            lva_nao_transferir = abap_true.
          ENDIF.
        ENDIF.

*** US #173081 - MMSILVA - 08.07.2025 - Ini ***
        IF lwa_access_key-event_sequence IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_access_key-event_sequence
            IMPORTING
              output = lwa_access_key-event_sequence.

          READ TABLE lit_zib_dist_avb INTO DATA(lwa_zib_avb) WITH KEY chave_nfe  = lwa_access_key-accesskey
                                                                      seq_evento = lwa_access_key-event_sequence. "SMC - #173081 15-09-2025
          IF sy-subrc EQ 0.
            lva_nao_transferir = abap_true.
          ENDIF.
        ENDIF.
*** US #173081 - MMSILVA - 08.07.2025 - Fim ***

      ENDIF.


      IF lva_nao_transferir EQ abap_true.
        DELETE c_edocuments WHERE edoc_guid = lwa_edocument-edoc_guid.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_host_port_url_documento.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarv_host_url)
     WHERE name = 'ZDRC_HOST_URL'.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarv_port_url)
     WHERE name = 'ZDRC_PORT_URL'.

    CHECK sy-subrc EQ 0.

    c_http_url-host = lwa_tvarv_host_url-low.
    c_http_url-port = lwa_tvarv_port_url-low.


  ENDMETHOD.


  METHOD get_xml_documento_eletronico.

    DATA: lva_read_xml_drc     TYPE c,
          lra_chaves           TYPE RANGE OF edobrincoming-accesskey,
          lit_chaves           TYPE zchaves_doc_eletronicos,
          lit_chaves_not_found TYPE zchaves_doc_eletronicos.

    CLEAR: e_xmls, lva_read_xml_drc, e_xml_raw, r_xml.

    lit_chaves = i_chaves.

    IF i_chave IS NOT INITIAL.
      APPEND i_chave TO lit_chaves.
    ENDIF.

    CHECK lit_chaves[] IS NOT INITIAL.

    LOOP AT lit_chaves INTO DATA(lwa_chave).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_chave ) TO lra_chaves.
    ENDLOOP.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_read_xml_drc)
     WHERE name = 'DRC_CONFIG'
       AND low  = 'READ_XML_DRC'.

    IF sy-subrc EQ 0.
      lva_read_xml_drc = abap_true.
    ENDIF.

    CASE lva_read_xml_drc.
      WHEN abap_true.

        CASE i_direcao.
          WHEN 'OUT'. "Outbound
            e_xmls = zcl_drc_utils=>get_xml_outbound_drc( i_chaves = lit_chaves
                                                          i_evento = i_evento ).
          WHEN 'IN'.
            e_xmls = zcl_drc_utils=>get_xml_inbound_drc( i_chaves =  lit_chaves
                                                         i_evento = i_evento ).
        ENDCASE.

        "Verificar se alguma das chaves solicitadas, não foi encontrado o XML pelo DRC, caso encontre, buscar xml no GRC
        CLEAR: lit_chaves_not_found[].
        LOOP AT lit_chaves INTO lwa_chave.
          READ TABLE e_xmls INTO DATA(lwa_xml) WITH KEY chave = lwa_chave.
          IF sy-subrc NE 0.
            APPEND lwa_chave TO lit_chaves_not_found.
          ENDIF.
        ENDLOOP.

        IF lit_chaves_not_found[] IS NOT INITIAL.
          DATA(r_xmls_grc) = zcl_drc_utils=>get_xml_grc( EXPORTING i_chaves  = lit_chaves_not_found
                                                                   i_direcao = i_direcao
                                                                   i_evento  = i_evento  ).

          LOOP AT r_xmls_grc INTO DATA(xml_grc).
            APPEND xml_grc TO e_xmls.
          ENDLOOP.
        ENDIF.

      WHEN abap_false.

        e_xmls = zcl_drc_utils=>get_xml_grc( EXPORTING i_chaves  = lit_chaves
                                                       i_direcao = i_direcao
                                                       i_evento  = i_evento ).

    ENDCASE.

    IF ( i_chave IS NOT INITIAL ) AND ( i_chaves[] IS INITIAL ). "Leitura de XML Unitaria
      READ TABLE e_xmls INTO lwa_xml WITH KEY chave = i_chave.
      IF sy-subrc EQ 0.
        r_xml     = lwa_xml-xml.
        e_xml_raw = lwa_xml-xml_raw.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_xml_grc.

    DATA: lv_destination TYPE char40,
          lv_xml_xstring TYPE fpcontent,
          lva_chave      TYPE zde_chave_nfe.

    CHECK I_CHAVES[] IS NOT INITIAL.

    CLEAR: r_xmls[].

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_destination_grc)
     WHERE name EQ 'DESTINATION_GRC'.

    CHECK sy-subrc EQ 0 AND lwa_destination_grc-low IS NOT INITIAL.

    lv_destination = lwa_destination_grc-low.

    LOOP AT i_chaves INTO DATA(lwa_chave).

      lva_chave = lwa_chave.

      CLEAR: lv_xml_xstring.

      "Função que Le o XML no GRC
      CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
        EXPORTING
          i_nfeid         = lva_chave
          i_direcao       = i_direcao
          i_evento        = i_evento
        IMPORTING
          e_xmlstring     = lv_xml_xstring
        EXCEPTIONS
          erro_nenhum_xml = 1
          OTHERS          = 2.

      CHECK lv_xml_xstring IS NOT INITIAL.

      DATA(r_xml_string) = zcl_string=>xstring_to_string( i_xstring =  lv_xml_xstring ).

      APPEND VALUE #( chave = lwa_chave xml = r_xml_string xml_raw = lv_xml_xstring ) TO r_xmls.

    ENDLOOP.


*    IF e_xml_xstring IS INITIAL. "Código Temporario para Busca XML em mais de um Ambiente GRC
*      SELECT * FROM setleaf INTO TABLE @DATA(lit_destinos_aux) WHERE setname EQ 'GRC_DESTINOS_AUX'.
*      DELETE lit_destinos_aux WHERE valfrom EQ lv_destination.
*
*      LOOP AT lit_destinos_aux INTO DATA(lwa_destino_aux).
*        lv_destination = CONV #( lwa_destino_aux-valfrom ).
*
*        CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
*          EXPORTING
*            i_nfeid         = lv_nfeid
*          IMPORTING
*            e_xmlstring     = e_xml_xstring
*          EXCEPTIONS
*            erro_nenhum_xml = 1
*            OTHERS          = 2.
*        IF ( sy-subrc EQ 0 ).
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.


  ENDMETHOD.


  METHOD get_xml_inbound_drc.

    TYPES: BEGIN OF ty_guids,
             edoc_guid TYPE edobrincoming-edoc_guid,
             accesskey TYPE edobrincoming-accesskey,
           END OF ty_guids,

           BEGIN OF ty_edocument,
             edoc_guid     TYPE edocument-edoc_guid,
             proc_status   TYPE edocument-proc_status,
             last_procstep TYPE edocument-last_procstep,
             error_flag    TYPE edocument-error_flag,
             process       TYPE edocument-process,

           END OF ty_edocument,

           BEGIN OF ty_edocumentfile,
             edoc_guid TYPE edocumentfile-edoc_guid,
             file_raw  TYPE edocumentfile-file_raw,
             file_type TYPE edocumentfile-file_type,
           END OF ty_edocumentfile.

    DATA: lra_chaves_55        TYPE RANGE OF edobrincoming-accesskey,
          lra_chaves_57        TYPE RANGE OF edobrincoming-accesskey,
          lit_guids            TYPE TABLE OF ty_guids,
          lit_edocument        TYPE TABLE OF edocument,
          lit_edocumentfile    TYPE TABLE OF ty_edocumentfile,
          lva_discard_canceled TYPE char01,
          lva_xml              TYPE string.

    CLEAR: lit_guids[].

    LOOP AT i_chaves INTO DATA(lwa_chave).
      CHECK strlen( lwa_chave ) EQ 44.
      CASE lwa_chave+20(2).
        WHEN '55'.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_chave ) TO lra_chaves_55.
        WHEN '57'.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_chave ) TO lra_chaves_57.
      ENDCASE.
    ENDLOOP.

    IF lra_chaves_55[] IS NOT INITIAL.
      SELECT edoc_guid accesskey
        FROM edobrincoming APPENDING TABLE lit_guids
       WHERE accesskey IN lra_chaves_55.
    ENDIF.

    IF lra_chaves_57[] IS NOT INITIAL.
      SELECT edoc_guid accesskey
        FROM edobrcteincoming APPENDING TABLE lit_guids
       WHERE accesskey IN lra_chaves_57.
    ENDIF.

    CHECK lit_guids[] IS NOT INITIAL.

    SELECT *
      FROM edocument INTO TABLE lit_edocument
      FOR ALL ENTRIES IN lit_guids
     WHERE edoc_guid EQ lit_guids-edoc_guid.

    IF i_evento = '110111'.
      lva_discard_canceled = abap_false.
    ELSE.
      lva_discard_canceled = abap_true.
    ENDIF.

    "Tratar aqui para ler somente os XML de documentos Validos..
    zcl_drc_utils=>exclude_edoc_not_validated( EXPORTING i_discard_canceled = lva_discard_canceled
                                               CHANGING  c_edocuments       = lit_edocument ).

    CHECK lit_edocument[] IS NOT INITIAL.

    SELECT edoc_guid file_raw file_type
      FROM edocumentfile INTO TABLE lit_edocumentfile
      FOR ALL ENTRIES IN lit_edocument
     WHERE edoc_guid EQ lit_edocument-edoc_guid.

    IF i_evento = '110111'. "Cancelamento
      DELETE lit_edocumentfile WHERE file_type NE 'CANC_XML'.
    ELSE.
      DELETE lit_edocumentfile WHERE file_type EQ 'CANC_XML'.
    ENDIF.

    LOOP AT lit_edocumentfile INTO DATA(lwa_edocumentfile).

      READ TABLE lit_guids INTO DATA(lwa_guid) WITH KEY edoc_guid = lwa_edocumentfile-edoc_guid.

      CHECK sy-subrc EQ 0.

      lva_xml = zcl_string=>xstring_to_string( i_xstring =  CONV #( lwa_edocumentfile-file_raw ) ).

      CONCATENATE '<?xml version="1.0" encoding="UTF-8"?>' lva_xml INTO lva_xml.

      APPEND VALUE #( xml =  lva_xml chave = lwa_guid-accesskey xml_raw = lwa_edocumentfile-file_raw ) TO r_xmls.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_xml_outbound_drc.

    DATA: lra_candat TYPE RANGE OF j_1bnfdoc-candat,
          lra_cancel TYPE RANGE OF j_1bnfdoc-cancel.

    DATA: lob_download          TYPE REF TO zcl_j_1bnfe_xml_download,
          lob_mdfe_processor    TYPE REF TO cl_nfe_cloud_mdfe_processor,
          lva_doctype           TYPE j_1b_nfe_doctype,
          lva_rfcdest           TYPE rfcdest,
          lva_no_check_use_drc  TYPE c,
          lwa_event             TYPE j_1bnfe_event,
          l_op_type             TYPE abap_bool,                             "*#127333 - 04.12.2023 - JT
          l_is_event            TYPE abap_bool,                             "*#127333 - 04.12.2023 - JT
          "lva_docnum            TYPE j_1bnfe_active-docnum,                 "*#127333 - 04.12.2023 - JT
          lv_is_valid_for_cloud TYPE abap_bool,                             "*#127333 - 04.12.2023 - JT
          lo_download_cloud     TYPE REF TO cl_nfe_cloud_download,          "*#127333 - 04.12.2023 - JT
          mo_download_any_type  TYPE REF TO cl_nfe_cloud_download_any_type, "*#127333 - 04.12.2023 - JT
          mo_local_file         TYPE REF TO cl_nfe_local_file,              "*#127333 - 04.12.2023 - JT
          mo_nfe_convert_wrap   TYPE REF TO if_nfe_convert_wrap,            "*#127333 - 04.12.2023 - JT
          ls_download           TYPE nfe_cloud_document_file,               "*#127333 - 04.12.2023 - JT
          ls_file_content       TYPE string,                                "*#127333 - 04.12.2023 - JT
          ls_path               TYPE string,                                "*#127333 - 04.12.2023 - JT
          ls_file_name          TYPE string,                                "*#127333 - 04.12.2023 - JT
          lv_xstring_content    TYPE xstring.                               "*#127333 - 04.12.2023 - JT

    CLEAR: lva_rfcdest, lwa_event.

    "Recuperar Destination DRC

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_destination_drc)
     WHERE name EQ 'DESTINATION_DRC'.

    CHECK sy-subrc EQ 0 AND lwa_destination_drc-low IS NOT INITIAL.

    lva_rfcdest = lwa_destination_drc-low.

    LOOP AT i_chaves INTO DATA(lwa_chave).

      CHECK strlen( lwa_chave ) EQ 44.

      CASE lwa_chave+20(2).
        WHEN '55'.
          IF i_evento IS NOT INITIAL.
            lva_doctype = 'EVE'. "Evento NF-e
          ELSE.
            lva_doctype = 'NFE'. "Nota Fiscal Eletronica (NF-e)
          ENDIF.
        WHEN '57'.
          IF i_evento IS NOT INITIAL.
            lva_doctype = 'EV2'. "Evento CT-e
          ELSE.
            lva_doctype = 'CTE'. "Conhecimento do transporte Eletronico (CT-e)
          ENDIF.
        WHEN '58'.
          IF i_evento IS NOT INITIAL.
            lva_doctype = 'EV3'. "Evento MDF-e
          ELSE.
            lva_doctype = 'MFE'. "Manifesto de Documento Fiscal Eletrônico
          ENDIF.
      ENDCASE.

      CHECK lva_doctype IS NOT INITIAL.


      IF NOT ( i_evento IS NOT INITIAL AND i_evento = '110111' ).  "Se solicitar XML Evento de Cancelamento, pode buscar o numero de documento estornado
        APPEND VALUE #( sign = 'I' option = 'EQ' low = '00000000' ) TO lra_candat.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ' ' ) TO lra_cancel.
      ENDIF.

      SELECT SINGLE a~* INTO @DATA(ls_active)
        FROM j_1bnfe_active AS a INNER JOIN j_1bnfdoc AS b ON a~docnum = b~docnum
       WHERE a~regio    EQ @lwa_chave(2)
         AND a~nfyear   EQ @lwa_chave+2(2)
         AND a~nfmonth  EQ @lwa_chave+4(2)
         AND a~stcd1    EQ @lwa_chave+6(14)
         AND a~model    EQ @lwa_chave+20(2)
         AND a~serie    EQ @lwa_chave+22(3)
         AND a~nfnum9   EQ @lwa_chave+25(9)
         AND a~docnum9  EQ @lwa_chave+34(9)
         AND a~cdv      EQ @lwa_chave+43(1)
         AND a~form     NE ' '
         AND b~candat   IN @lra_candat
         AND b~cancel   IN @lra_cancel
         AND b~doctyp   IN ('1','2','6','3').

      CHECK sy-subrc EQ 0 AND ls_active  IS NOT INITIAL.

      IF i_evento IS NOT INITIAL.
        SELECT SINGLE *
          FROM j_1bnfe_event INTO lwa_event
         WHERE docnum    EQ ls_active-docnum
           AND ext_event EQ i_evento
           AND docsta    EQ '1' "Autorizado
           AND seqnum    EQ ( SELECT MAX( seqnum )
                                FROM j_1bnfe_event
                               WHERE docnum    EQ ls_active-docnum
                                 AND ext_event EQ i_evento
                                 AND docsta    EQ '1' ).
      ENDIF.

      lva_no_check_use_drc = abap_true.
      EXPORT lva_no_check_use_drc FROM lva_no_check_use_drc TO MEMORY ID 'Z_NO_CHECK_USE_BRANCH_DRC'.

*-----------------------------------------------------------------------------------------------*
*     Classe Download Especifica para Documentos MDF-e
*-----------------------------------------------------------------------------------------------*
      IF lwa_chave+20(2) = '58'.

        CREATE OBJECT lob_mdfe_processor.

        IF i_evento IS NOT INITIAL.

        ELSE.
          TRY.
              DATA(lwa_document_info) = lob_mdfe_processor->get_file(
                EXPORTING
                  iv_environment_type = CONV #( ls_active-tpamb )
                  iv_action           = 'AUTHORIZE'
                  iv_uuid             = CONV #( ls_active-cloud_guid ) ).


              IF lwa_document_info-file_content IS NOT INITIAL.
                DATA(r_xml_string) = zcl_string=>base64_to_string( i_texto =  CONV #( lwa_document_info-file_content ) ).

                CALL FUNCTION 'SCMS_STRING_TO_XSTRING' "
                  EXPORTING
                    text   = r_xml_string
                  IMPORTING
                    buffer = lv_xstring_content
                  EXCEPTIONS
                    failed = 1.

                APPEND VALUE #( chave = lwa_chave xml = r_xml_string xml_raw = lv_xstring_content ) TO r_xmls.
              ENDIF.
            CATCH cx_nfe_cloud_download_error.
          ENDTRY.
        ENDIF.

        CONTINUE.
      ENDIF.


*-----------------------------------------------------------------------------------------------*
*     Metodo Download pela clase CL_NFE_CLOUD_DOWNLOAD
*-----------------------------------------------------------------------------------------------*
      CREATE OBJECT: lo_download_cloud,
                     mo_download_any_type,
                     mo_nfe_convert_wrap TYPE cl_nfe_convert_wrap.

      lv_is_valid_for_cloud = lo_download_cloud->is_valid_for_cloud(
        iv_document_number     = ls_active-docnum
        iv_company_code        = ls_active-bukrs
        iv_business_place      = ls_active-branch
        is_electronic_document = ls_active
        is_event               = l_is_event ).

      IF lv_is_valid_for_cloud = abap_true.                                                                   "2932848 "3039634
        TRY.
            ls_download = mo_download_any_type->get_file(
              is_electronic_nota_fiscal = ls_active
              is_nfe_event              = lwa_event
              iv_uuid                   = COND #( WHEN lwa_event-cloud_uuid IS NOT INITIAL THEN lwa_event-cloud_uuid  ""#131037 - 09.01.2024 - JT
                                                                                           ELSE ls_active-cloud_guid )
              iv_direction              = 'OUTB' ).

            lv_xstring_content = mo_nfe_convert_wrap->base64_to_xstring( ls_download-file_content ).

            IF lv_xstring_content IS NOT INITIAL.
              r_xml_string = zcl_string=>xstring_to_string( i_xstring =  lv_xstring_content ).
              APPEND VALUE #( chave = lwa_chave xml = r_xml_string xml_raw = lv_xstring_content ) TO r_xmls.
            ENDIF.

          CATCH cx_nfe_cloud_badi_file_handler INTO DATA(lx_file_handler).
            CONTINUE.
          CATCH cx_nfe_local_file_error        INTO DATA(lx_nfe_local_file_error).
            CONTINUE.
          CATCH cx_nfe_cloud_download_error    INTO DATA(lx_nfe_cloud_download_error).
            CONTINUE.
          CATCH cx_nfe INTO DATA(lx_nfe).
            CONTINUE.
        ENDTRY.
        CONTINUE.
      ENDIF.

*-----------------------------------------------------------------------------------------------*
*     Metodo Download pela classe ZCL_J_1BNFE_XML_DOWNLOAD
*-----------------------------------------------------------------------------------------------*

      TRY.
          CREATE OBJECT lob_download
            EXPORTING
              iv_xml_key = CONV #( lwa_chave )
              iv_rfc     = lva_rfcdest.

          lob_download->load_xml_content( EXPORTING iv_docnum       = CONV #( ls_active-docnum )
                                                    iv_direction    = 'OUT'
                                                    iv_event_type   = CONV #( lwa_event-ext_event )
                                                    iv_event_seqnum = CONV #( lwa_event-seqnum )
                                                    iv_doctype      = lva_doctype ).
        CATCH zcx_error INTO DATA(zcx_error).
          CONTINUE.
      ENDTRY.

      DATA(lva_xml_xstring) = lob_download->get_xml_content( ).
      IF lva_xml_xstring IS NOT INITIAL.
        r_xml_string = zcl_string=>xstring_to_string( i_xstring =  lva_xml_xstring ).
        APPEND VALUE #( chave = lwa_chave xml = r_xml_string xml_raw = lva_xml_xstring ) TO r_xmls.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
