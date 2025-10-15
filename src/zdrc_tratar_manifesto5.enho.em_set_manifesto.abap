METHOD set_manifesto .

  DATA: lo_parser    TYPE REF TO if_edoc_source_parser,
        lo_params    TYPE REF TO cl_edoc_br_create_entity_param,
        md_xml       TYPE REF TO data,
        ls_xml       TYPE xstring,
        ls_protocolo TYPE char15,
        ls_nfeproc   TYPE zdrc_ret_manifesto_0001,
        ls_event     TYPE zde_ret_event_inb,
        t_callstack  TYPE abap_callstack,
        wa_zsdt0127  TYPE zsdt0127.

  FIELD-SYMBOLS: <ls_nfe> TYPE any.

*  CALL FUNCTION 'SYSTEM_CALLSTACK'
*    EXPORTING
*      max_level = 0
*    IMPORTING
*      callstack = t_callstack.
*
**------------------
** transacao foi executada pela ZSST0110?
**------------------
*  READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZSDR0059'.
*
*  CHECK sy-subrc = 0.
*
**-------------------------------------
**-- Importar manifesto p/ memoria (zcl_manifesto_dest)ENVIAR_MANIFESTO
**-------------------------------------
*  IMPORT wa_zsdt0127 = wa_zsdt0127 FROM MEMORY ID 'ZSDT0127'.
*
*  CHECK sy-subrc = 0.
*
*-------------------------------------
*-- parse XML
*-------------------------------------
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = i_response-xml
    IMPORTING
      buffer = ls_xml
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  TRY.
      CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
      CREATE OBJECT lo_params
        EXPORTING
          iv_xml = ls_xml.

      lo_parser->load_source( lo_params->get_xml( ) ).
      md_xml   = lo_parser->parse_to_ddic( ls_nfeproc ).

      ASSIGN md_xml->* TO <ls_nfe>.
      ls_nfeproc        = <ls_nfe>.
      ls_protocolo      = ls_nfeproc-nferecepcaoeventonfresult-retenvevento-retevento-infevento-nprot.
    CATCH cx_edocument.
      EXIT.
  ENDTRY.

*-------------------------------------
*-- monta estrutura
*-------------------------------------
  ls_event-chave        = i_response-accesskey.
  ls_event-ext_event    = i_response-tpevento.
  ls_event-code         = i_response-cstat.
  ls_event-motivo       = i_response-xmotivo.
  ls_event-protocolo    = ls_protocolo.
  ls_event-dhregevento  = i_response-dhregevento.

*-------------------------------------
*-- gravar informacoes manifesto
*-------------------------------------
  CALL FUNCTION 'Z_GRC_RETORNO_EVENT_INB'
    EXPORTING
      i_event = ls_event.

ENDMETHOD.
