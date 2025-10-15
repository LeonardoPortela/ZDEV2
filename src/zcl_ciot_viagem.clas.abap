class ZCL_CIOT_VIAGEM definition
  public
  final
  create public .

*"* public components of class ZCL_CIOT_VIAGEM
*"* do not include other source files here!!!
public section.

  data XML_VIAGEM type STRING .

  methods CONSTRUCTOR .
  methods MONTA_XML_PARCEIRO
    importing
      !P_PARCEIROS type ZCIOT_CONTRATANTE .
  methods POPULA_CIOT
    importing
      !CTE_DOCNUM type J_1BDOCNUM
      !EMITIR_VIAGEM_ADM type CHAR01 optional
      !CREDITAR_VIAGEM_ADM type CHAR01 optional
      !RESCINDIR_VIAGEM_ADM type CHAR01 optional
      !CONSULTAR_STATUS_VIAGEM type CHAR01 optional
      !I_REASON type J_1BNFE_CANCEL_REASON optional
      !I_REASON1 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON2 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON3 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON4 type J_1BNFE_CANCEL_TEXT optional
      !I_FATURAMENTO_AUTOM type CHAR01 optional
      !I_CH_REFERENCIA type ZCH_REF optional
    exporting
      value(P_PROTOCOLO) type ZPROTOCOLO
    exceptions
      NAO_CIOT
      ERRO_STATUS
      ERRO_STATUS_CRED
      ERRO_STATUS_CANC
      ERRO_WEB_SERVICE
      ERRO_SOLICITACAO
      ERRO_XML_SOLICITA .
  methods IMPRIMIR_RESUMO
    importing
      !CTE_DOCNUM type J_1BDOCNUM
    exceptions
      NAO_CIOT
      ERRO_STATUS .
  methods IMPRIMIR_CONTRATO
    importing
      !CTE_DOCNUM type J_1BDOCNUM
      !IMPRIMIR type CHAR01 default 'X'
    exporting
      !E_URL type STRING
    exceptions
      NAO_CIOT
      ERRO_STATUS .
  methods IMPRIMIR_PEDAGIO
    importing
      !CTE_DOCNUM type J_1BDOCNUM
    exceptions
      NAO_CIOT
      ERRO_STATUS .
  methods CARGA_PEDAGIO
    importing
      !CTE_DOCNUM type J_1BDOCNUM
    exceptions
      NAO_CIOT
      ERRO_STATUS .
  methods SOLICITA_VIAGEM
    importing
      !I_FATURAMENTO_AUTOM type CHAR01 optional
      !I_CH_REFERENCIA type ZCH_REF optional
    exporting
      value(P_PROTOCOLO) type ZPROTOCOLO
    exceptions
      ERRO_STATUS
      ERRO_WEB_SERVICE
      ERRO_XML_SOLICITA
      ERRO_VALORES .
  methods SOLICITA_VIAGEM_REQ
    exporting
      value(P_PROTOCOLO) type ZPROTOCOLO
    exceptions
      SEM_PROTOCOLO
      ERRO_STATUS
      ERRO_WEB_SERVICE .
  methods RESCINDIR_VIAGEM
    exceptions
      ERRO_STATUS .
  methods CREDITA_VIAGEM
    importing
      !I_FATURAMENTO_AUTOM type CHAR01 optional
      !I_CH_REFERENCIA type ZCH_REF optional
    exceptions
      ERRO_STATUS
      ERRO_WEB_SERVICE .
  methods CANCELA_VIAGEM
    exceptions
      ERRO_STATUS
      ERRO_WEB_SERVICE
      ERRO_CANCELAMENTO .
  methods MONTA_XML_RESCISAO .
  methods MONTA_XML_CREDITO .
  methods MONTA_XML_CANCELA
    importing
      !P_REASON1 type J_1BNFE_CANCEL_TEXT
      !P_REASON2 type J_1BNFE_CANCEL_TEXT
      !P_REASON3 type J_1BNFE_CANCEL_TEXT
      !P_REASON4 type J_1BNFE_CANCEL_TEXT .
  methods MONTA_XML
    exporting
      value(CNPJ) type STCD1
      value(COD_SAP) type TDLNR
    raising
      ZCX_WEBSERVICE .
  methods MONTA_XML_SOLICITA_REQ
    exporting
      !P_PROTOCOLO type ZPROTOCOLO
    exceptions
      SEM_PROTOCOLO .
  methods AJUSTAR_VIAGEM
    importing
      !I_NUCONTRATO type ZNUCONTRATO
      !I_CHVID type ZCHVID
    exporting
      !E_MENSAGEM type CHAR300
    returning
      value(R_STATUS_OK) type CHAR01 .
  methods ATUALIZA_VIAGEM_AJUSTE
    importing
      !I_NUCONTRATO type ZNUCONTRATO
      !I_CHVID type ZCHVID
      !I_CODIGO_INTEGRACAO type CHAR04 optional
      !I_MSG_INTEGRACAO type CHAR300 optional .
protected section.
*"* protected components of class ZCL_CIOT_VIAGEM
*"* do not include other source files here!!!

  data CIOT type ZCL_CIOT_T .

  methods ENVIAR_XML_WEB_SERVICE .
  methods VALIDA_VIAGEM
    exceptions
      ERRO_VALORES .
private section.
*"* private components of class ZCL_CIOT_VIAGEM
*"* do not include other source files here!!!

  data REASON type J_1BNFE_CANCEL_REASON .
  data REASON1 type J_1BNFE_CANCEL_TEXT .
  data REASON2 type J_1BNFE_CANCEL_TEXT .
  data REASON3 type J_1BNFE_CANCEL_TEXT .
  data REASON4 type J_1BNFE_CANCEL_TEXT .
  data DOCNUM type J_1BDOCNUM .
  data CIOT_RET type ZCIOT_RET .

  methods CTNAB
    importing
      !TAG type CLIKE .
  methods CTNFE
    importing
      !TAG type CLIKE .
  methods CTNNAONULO
    importing
      !TAG type CLIKE
      !TAM type POSNR
      !VALOR type CLIKE .
  methods CTNAV
    importing
      !TAG type CLIKE
      !VALOR type CLIKE .
  methods CTNAVN
    importing
      !TAG type CLIKE
      !VALOR type NUMERIC
      !PRECISAO type INTEGER .
  methods CTNAFV
    importing
      !TAG type STRING
      !VALOR type NUMERIC
      !PRECISAO type INTEGER .
  methods CTNAF
    importing
      !TAG type CLIKE
      !VALOR type CLIKE .
  methods CTNDTN
    importing
      !TAG type CLIKE
      !VALOR type CLIKE .
  methods CTNDHN
    importing
      !TAG type CLIKE
      !DATA type CLIKE
      !HORA type CLIKE .
ENDCLASS.



CLASS ZCL_CIOT_VIAGEM IMPLEMENTATION.


METHOD CANCELA_VIAGEM.

  TYPES: BEGIN OF TY_XML_VIAGEM.
  TYPES:   XML TYPE STRING.
  TYPES: END OF TY_XML_VIAGEM.

  DATA: OB_CIOT          TYPE REF TO ZCL_CIOT,
        VIAGEM_CANCELADA TYPE CHAR01,
        WA_XML           TYPE TY_XML_VIAGEM,
        IT_XML           TYPE STANDARD TABLE OF TY_XML_VIAGEM,
        NAME_FILE        TYPE STRING,
        XML_CANCELA      TYPE STRING,
        MENSAGEM         TYPE STRING,
        MSG_VAR1         TYPE SYMSGV,
        MSG_VAR2         TYPE SYMSGV,
        WA_DOCNUM        TYPE J_1BDOCNUM,
        MSG_VAR3         TYPE SYMSGV,
        MSG_VAR4         TYPE SYMSGV,
        URL_END          TYPE STRING.

  LOOP AT CIOT INTO OB_CIOT.

    CALL METHOD OB_CIOT->CANCELAR
      EXPORTING
        P_VALIDAR   = 'X'
        I_REASON    = ME->REASON
        I_REASON1   = ME->REASON1
        I_REASON2   = ME->REASON2
        I_REASON3   = ME->REASON3
        I_REASON4   = ME->REASON4
      EXCEPTIONS
        ERRO_STATUS = 1
        CANCEL_CTE  = 2
        OTHERS      = 3.

    IF SY-SUBRC = 1.
      MESSAGE E017 WITH SY-MSGV1 RAISING ERRO_STATUS.
    ELSEIF SY-SUBRC = 2.
      MESSAGE E038(ZCIOT) RAISING ERRO_CANCELAMENTO.
    ELSE.
      CALL METHOD ME->MONTA_XML_CANCELA
        EXPORTING
          P_REASON1 = OB_CIOT->REASON1
          P_REASON2 = OB_CIOT->REASON2
          P_REASON3 = OB_CIOT->REASON3
          P_REASON4 = OB_CIOT->REASON4.
    ENDIF.

  ENDLOOP.

  IF 1 = 2.

    CLEAR: IT_XML.
    WA_XML-XML = XML_VIAGEM.
    APPEND WA_XML TO IT_XML.

    CONCATENATE '\\' SY-HOST '\INTERFACES\PF_e\PFCanc' ME->DOCNUM '.xml' INTO NAME_FILE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = NAME_FILE
      TABLES
        DATA_TAB                = IT_XML
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_SD_WEBSERVICE_ADM_CANCEL'
    EXPORTING
      XML                        = XML_VIAGEM
    IMPORTING
      XML_CANCELA                = XML_CANCELA
      VIAGEM_CANCELADA           = VIAGEM_CANCELADA
      MENSAGEM                   = MENSAGEM
      URL_END                    = URL_END
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      ERRO_WEB_SERVICE           = 5
      OTHERS                     = 6.

  IF ( SY-SUBRC EQ 0 ).
    CALL FUNCTION 'Z_SALVA_XML_PFE'
      EXPORTING
        P_XML       = XML_VIAGEM
        P_PROTOCOLO = ''
        P_USUARIO   = SY-UNAME
        P_URL       = URL_END
        P_TIPO      = 'CANC'.
  ENDIF.

  CASE SY-SUBRC.
    WHEN 1 OR 2 OR 3 OR 4 OR 6.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN 5.
      MESSAGE E032(ZSIMETRYA) WITH SY-MSGV1 RAISING ERRO_WEB_SERVICE.
    WHEN 0.
      IF NOT VIAGEM_CANCELADA IS INITIAL.
        LOOP AT CIOT INTO OB_CIOT.
          CALL METHOD OB_CIOT->CANCELAR
            EXCEPTIONS
              ERRO_STATUS = 1
              OTHERS      = 2.
        ENDLOOP.
      ELSE.
        IF NOT MENSAGEM IS INITIAL.
          CALL FUNCTION 'ZMESSAGE_PREPARE'
            EXPORTING
              MSG_COMPLETA = SPACE
            CHANGING
              MSG_TEXT     = MENSAGEM
              MSG_VAR1     = MSG_VAR1
              MSG_VAR2     = MSG_VAR2
              MSG_VAR3     = MSG_VAR3
              MSG_VAR4     = MSG_VAR4.
          MESSAGE E000 WITH MSG_VAR1 MSG_VAR2 MSG_VAR3 MSG_VAR4 RAISING ERRO_CANCELAMENTO.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF 1 = 2.

    CLEAR: IT_XML.
    WA_XML-XML = XML_CANCELA.
    APPEND WA_XML TO IT_XML.

    CONCATENATE '\\' SY-HOST '\INTERFACES\PF_e\PFCancResp' ME->DOCNUM '.xml' INTO NAME_FILE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = NAME_FILE
      TABLES
        DATA_TAB                = IT_XML
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDMETHOD.


  method CARGA_PEDAGIO.

  DATA: IT_OB_CIOT  TYPE ZCL_CIOT_T,
        WA_OB_CIOT  TYPE REF TO ZCL_CIOT,
        WA_ZCTE_RET TYPE ZCIOT_RET,
        VG_ST_CIOT  TYPE ZST_CIOT.

  CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
    EXPORTING
      P_CTE_AVULSO = CTE_DOCNUM
    TABLES
      IT_ZCL_CIOT  = IT_OB_CIOT
    EXCEPTIONS
      NAO_CIOT     = 1
      OTHERS       = 2.

  IF SY-SUBRC = 1.
    MESSAGE E008 WITH SY-MSGV1 RAISING NAO_CIOT.
  ENDIF.

  CLEAR: ME->CIOT.

  ME->DOCNUM = CTE_DOCNUM.

  MOVE IT_OB_CIOT[] TO CIOT[].

  LOOP AT CIOT INTO WA_OB_CIOT.

    CALL METHOD WA_OB_CIOT->GET_ST_CIOT
      IMPORTING
        P_ST_CIOT = VG_ST_CIOT.

    IF ( VG_ST_CIOT EQ '5' ) OR ( VG_ST_CIOT EQ '6' ).
      CALL METHOD WA_OB_CIOT->CARGA_PEDAGIO.
    ELSE.
      MESSAGE E024 WITH CTE_DOCNUM RAISING NAO_CIOT.
    ENDIF.
  ENDLOOP.

  endmethod.


method CONSTRUCTOR.
endmethod.


METHOD credita_viagem.

  TYPES: BEGIN OF ty_xml_viagem.
  TYPES:   xml TYPE string.
  TYPES: END OF ty_xml_viagem.

*-#133089-21.02.2024-JT-inicio
  DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.
  IF i_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  DATA: ob_ciot            TYPE REF TO zcl_ciot,
        viagem_autorizada  TYPE char01,
        name_file          TYPE string,
        link_contrato      TYPE agr_url,
        link_resumo        TYPE agr_url,
        link_pedagio       TYPE agr_url,
        link_carga_pedagio TYPE agr_url,
        wa_xml             TYPE ty_xml_viagem,
        it_xml             TYPE STANDARD TABLE OF ty_xml_viagem,
        xml_credito        TYPE string.

  LOOP AT ciot INTO ob_ciot.

    CALL METHOD ob_ciot->creditar
      EXPORTING
        p_validar   = 'X'
      EXCEPTIONS
        erro_status = 1
        OTHERS      = 2.

    IF sy-subrc = 1.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e017 WITH sy-msgv1 RAISING erro_status.
        WHEN abap_true.
          MESSAGE e017 WITH sy-msgv1 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
          RAISE erro_status.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

  ENDLOOP.

  CALL METHOD me->monta_xml_credito.

  IF 1 = 2.

    CLEAR: it_xml.
    wa_xml-xml = xml_viagem.
    APPEND wa_xml TO it_xml.

    CONCATENATE '\\' sy-host '\INTERFACES\PF_e\PFAut' me->docnum '.xml' INTO name_file.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = name_file
      TABLES
        data_tab                = it_xml
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_SD_WEBSERVICE_ADM_CRED'
    EXPORTING
      xml                        = xml_viagem
    IMPORTING
      xml_credito                = xml_credito
      viagem_autorizada          = viagem_autorizada
      link_contrato              = link_contrato
      link_resumo                = link_resumo
      link_pedagio               = link_pedagio
      link_carga_pedagio         = link_carga_pedagio
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      erro_web_service           = 5
      erro_ws_convenio           = 6
      OTHERS                     = 7.

  CASE sy-subrc.
    WHEN 1 OR 2 OR 3 OR 4 OR 6 OR 7.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN abap_true.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    WHEN 5.
      MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
    WHEN 0.
      IF NOT viagem_autorizada IS INITIAL.
        LOOP AT ciot INTO ob_ciot.
          CALL METHOD ob_ciot->creditar
            EXPORTING
              p_link_contrato      = link_contrato
              p_link_resumo        = link_resumo
              p_link_pedagio       = link_pedagio
              p_link_carga_pedagio = link_carga_pedagio
            EXCEPTIONS
              erro_status          = 1
              OTHERS               = 2.
        ENDLOOP.
      ENDIF.
  ENDCASE.

  IF 1 = 2.

    CLEAR: it_xml.
    wa_xml-xml = xml_credito.
    APPEND wa_xml TO it_xml.

    CONCATENATE '\\' sy-host '\INTERFACES\PF_e\PFAutResp' me->docnum '.xml' INTO name_file.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = name_file
      TABLES
        data_tab                = it_xml
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD ctnab.
  CONCATENATE xml_viagem '<' tag '>' INTO xml_viagem.
ENDMETHOD.


METHOD ctnaf.

  IF valor IS NOT INITIAL.
    CONCATENATE xml_viagem '<' tag '>' valor '</' tag '>' INTO xml_viagem.
  ENDIF.

ENDMETHOD.


METHOD ctnafv.
  DATA: xvalor TYPE string.
  IF valor IS NOT INITIAL.
    xvalor = valor.
    CONCATENATE xml_viagem '<' tag '>' xvalor '</' tag '>' INTO xml_viagem.
  ENDIF.
ENDMETHOD.


method CTNAV.
  CONCATENATE xml_viagem '<' tag '>' valor '</' tag '>' INTO xml_viagem.
endmethod.


method ctnavn.

  data: xvalor type string,
        xvalor_decimal type string,
        tm_string      type i.
  data: result_tab type match_result_tab.

  field-symbols <match> like line of result_tab.

  if valor is not initial.
    xvalor = valor.
    find first occurrence of '.' in xvalor results result_tab.
    read table result_tab index 1 assigning <match>.
    <match>-offset = <match>-offset + 1.

    tm_string      = strlen( xvalor ).
    tm_string      = tm_string - <match>-offset - 1.
    xvalor_decimal = xvalor+<match>-offset(tm_string).
    tm_string      = <match>-offset - 1.
    xvalor         = xvalor(tm_string).

    tm_string = precisao - strlen( xvalor_decimal ).

    do tm_string times.
      concatenate xvalor_decimal '0' into xvalor_decimal.
    enddo.

    concatenate xvalor xvalor_decimal into xvalor.

    concatenate xml_viagem '<' tag '>' xvalor '</' tag '>' into xml_viagem.
  else.
    concatenate xml_viagem '<' tag '>' '0' '</' tag '>' into xml_viagem.
  endif.

endmethod.


METHOD ctndhn.
  DATA: dtvalor TYPE string,
        hrvalor TYPE string.

  CONCATENATE data+6(2) data+4(2) data(4) INTO dtvalor.
  CONCATENATE hora(2) hora+2(2) hora+4(2) INTO hrvalor.
  CONCATENATE xml_viagem '<' tag '>' dtvalor hrvalor '</' tag '>' INTO xml_viagem.
ENDMETHOD.


METHOD ctndtn.
  DATA: dtvalor TYPE string.
  CONCATENATE valor+6(2) valor+4(2) valor(4) INTO dtvalor.
  CONCATENATE xml_viagem '<' tag '>' dtvalor '</' tag '>' INTO xml_viagem.
ENDMETHOD.


METHOD ctnfe.
  CONCATENATE xml_viagem '</' tag '>' INTO xml_viagem.
ENDMETHOD.


method ctnnaonulo.

  data: text_ type c length 100.

  if valor is not initial.
    concatenate xml_viagem '<' tag '>' valor '</' tag '>' into xml_viagem.
  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = '0'
      importing
        output = text_.
    concatenate xml_viagem '<' tag '>' text_(tam) '</' tag '>' into xml_viagem.
  endif.

endmethod.


METHOD enviar_xml_web_service.

  DATA: it_pfe  TYPE TABLE OF zpfe_xml_viagem,
        wa_pfe  TYPE zpfe_xml_viagem.

  wa_pfe-nu_documento_sap = me->docnum.
  wa_pfe-tx_xml           = me->xml_viagem.
  APPEND wa_pfe TO it_pfe.

  CALL FUNCTION 'Z_SD_OUTBOUND_PFE_XML'
    TABLES
      it_pfe = it_pfe.

ENDMETHOD.


METHOD IMPRIMIR_CONTRATO.

  DATA: IT_OB_CIOT  TYPE ZCL_CIOT_T,
        WA_OB_CIOT  TYPE REF TO ZCL_CIOT,
        WA_ZCTE_RET TYPE ZCIOT_RET,
        VG_ST_CIOT  TYPE ZST_CIOT.

  CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
    EXPORTING
      P_CTE_AVULSO = CTE_DOCNUM
    TABLES
      IT_ZCL_CIOT  = IT_OB_CIOT
    EXCEPTIONS
      NAO_CIOT     = 1
      OTHERS       = 2.

  IF SY-SUBRC = 1.
    MESSAGE E008 WITH SY-MSGV1 RAISING NAO_CIOT.
  ENDIF.

  CLEAR: ME->CIOT.

  ME->DOCNUM = CTE_DOCNUM.

  MOVE IT_OB_CIOT[] TO CIOT[].

  LOOP AT CIOT INTO WA_OB_CIOT.

    CALL METHOD WA_OB_CIOT->GET_ST_CIOT
      IMPORTING
        P_ST_CIOT = VG_ST_CIOT.

    IF ( VG_ST_CIOT EQ '5' ) OR ( VG_ST_CIOT EQ '6' ).
      CALL METHOD WA_OB_CIOT->IMPRIMIR
        EXPORTING
          IMPRIMIR = IMPRIMIR    " Campo de texto do comprimento 1
        IMPORTING
          E_URL    = E_URL.
    ELSE.
      MESSAGE E024 WITH CTE_DOCNUM RAISING NAO_CIOT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD IMPRIMIR_PEDAGIO.

  DATA: IT_OB_CIOT  TYPE ZCL_CIOT_T,
        WA_OB_CIOT  TYPE REF TO ZCL_CIOT,
        WA_ZCTE_RET TYPE ZCIOT_RET,
        VG_ST_CIOT  TYPE ZST_CIOT.

  CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
    EXPORTING
      P_CTE_AVULSO = CTE_DOCNUM
    TABLES
      IT_ZCL_CIOT  = IT_OB_CIOT
    EXCEPTIONS
      NAO_CIOT     = 1
      OTHERS       = 2.

  IF SY-SUBRC = 1.
    MESSAGE E008 WITH SY-MSGV1 RAISING NAO_CIOT.
  ENDIF.

  CLEAR: ME->CIOT.

  ME->DOCNUM = CTE_DOCNUM.

  MOVE IT_OB_CIOT[] TO CIOT[].

  LOOP AT CIOT INTO WA_OB_CIOT.

    CALL METHOD WA_OB_CIOT->GET_ST_CIOT
      IMPORTING
        P_ST_CIOT = VG_ST_CIOT.

    IF ( VG_ST_CIOT EQ '5' ) OR ( VG_ST_CIOT EQ '6' ).
      CALL METHOD WA_OB_CIOT->IMPRIMIR_PEDAGIO.
    ELSE.
      MESSAGE E024 WITH CTE_DOCNUM RAISING NAO_CIOT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


method imprimir_resumo.

  data: it_ob_ciot  type zcl_ciot_t,
        wa_ob_ciot  type ref to zcl_ciot,
        wa_zcte_ret type zciot_ret,
        vg_st_ciot  type zst_ciot.

  call function 'Z_SD_INFO_CTE_CIOT'
    exporting
      p_cte_avulso = cte_docnum
    tables
      it_zcl_ciot  = it_ob_ciot
    exceptions
      nao_ciot     = 1
      others       = 2.

  if sy-subrc = 1.
    message e008 with sy-msgv1 raising nao_ciot.
  endif.

  clear: me->ciot.

  me->docnum = cte_docnum.

  move it_ob_ciot[] to ciot[].

  loop at ciot into wa_ob_ciot.

    call method wa_ob_ciot->get_st_ciot
      importing
        p_st_ciot = vg_st_ciot.

    if ( vg_st_ciot eq '5' ) or ( vg_st_ciot eq '6' ).
      call method wa_ob_ciot->imprimir_resumo.
    else.
      message e024 with cte_docnum raising nao_ciot.
    endif.
  endloop.

endmethod.


METHOD monta_xml.

  DATA: ob_ciot                  TYPE REF TO zcl_ciot,
        cd_ciot                  TYPE zciot,
        cd_trans                 TYPE tknum,

        "CIOT_RET        TYPE ZCIOT_RET,
        wa_parceiros             TYPE zciot_contratante,
        wa_nota                  TYPE zcte_info_nota,
        wa_act_nota              TYPE j_1bnfe_active,
        wa_veiculo               TYPE zcte_trans,
        lc_eixosi                TYPE i,
        lc_eixos                 TYPE c LENGTH 1,
        vg_fiscal                TYPE c LENGTH 10,
        vg_transp                TYPE c LENGTH 10,
        vl_frete                 TYPE zvlr_unit_frete,
        vl_chave                 TYPE zcte_info_nota-chave,
        idsolicitacaocontratante TYPE c LENGTH 20,
        dt_dif                   TYPE sy-datum,
        wa_agencia               TYPE bankl,
        obj_zcl_tipcard          TYPE REF TO zcl_webservice_tipcard,
        "CL_EXCEPTION             TYPE REF TO ZCX_WEBSERVICE,
        var_chave                TYPE char32,
        var_msg                  TYPE string,
        vl_margem                TYPE zlest0103-margadto,
        lc_branch                TYPE werks_d,
        wa_j_1bbranch            TYPE j_1bbranch,
        var_versao_xml_tip       TYPE char10,
        lv_algodao               TYPE char01, "*-CS2024000886-09.12.2024-#154506-JT
        lv_flag_calculo          TYPE char01. "*-CS2024000886-09.12.2024-#154506-JT

  DATA: wa_zcte_ciot TYPE zcte_ciot.

  CLEAR: xml_viagem.
  READ TABLE ciot INTO ob_ciot INDEX 1.

  CALL METHOD ob_ciot->get_cd_ciot
    IMPORTING
      pcd_ciot = cd_ciot.

  CALL METHOD ob_ciot->visualizar
    IMPORTING
      p_zcte_ret = ciot_ret.

  CALL METHOD ob_ciot->get_tknum
    IMPORTING
      p_tknum = cd_trans.

  SELECT SINGLE * INTO @DATA(wa_vttk)
    FROM vttk
   WHERE tknum EQ @cd_trans.

  TRY .
      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wa_vttk-tdlnr
        )->ck_parceiro_local_negocio(
        ).
    CATCH zcx_parceiros INTO DATA(ex_parceiros).
      MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty
       NUMBER ex_parceiros->msgno
         WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 .
  ENDTRY.

  MOVE: wa_vttk-tdlnr+6(4) TO lc_branch.

  CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
    EXPORTING
      centro               = lc_branch
    IMPORTING
      wa_j_1bbranch        = wa_j_1bbranch
    EXCEPTIONS
      informar_centro      = 1
      nao_centro_r_virtual = 2
      informar_centro_out  = 3
      informar_centro_v    = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE ciot_ret-parceiros INTO wa_parceiros WITH KEY tipo = '00'.
  me->docnum = ciot_ret-docnum.
  cnpj    = wa_parceiros-cnpj.
  cod_sap = ciot_ret-emissor.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ciot_ret-docnum
    IMPORTING
      output = vg_fiscal.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ciot_ret-tknum
    IMPORTING
      output = vg_transp.

  CONCATENATE vg_fiscal vg_transp INTO idsolicitacaocontratante.


**=============================================
  "Stvarv versão XML/TIP.
  IF ciot_ret-valores_servico-nr_vr_xml_tipf IS INITIAL.
    CLEAR: var_versao_xml_tip.
    SELECT SINGLE low FROM tvarvc
    INTO var_versao_xml_tip
    WHERE name = 'Z_VERSAO_XML_TIP'.
    IF sy-subrc EQ 0.
      ciot_ret-valores_servico-nr_vr_xml_tipf = var_versao_xml_tip.
    ELSE.
      ciot_ret-valores_servico-nr_vr_xml_tipf = '1.17'.
    ENDIF.
  ENDIF.
**=============================================

  CALL METHOD me->ctnab EXPORTING tag = 'viagem'.

*  IF ciot_ret-valores_servico-nr_vr_xml_tipf IS INITIAL.
*    CALL METHOD me->ctnav EXPORTING tag = 'versao' valor = '1.0.0'.
*  ELSE.
  CALL METHOD me->ctnav EXPORTING tag = 'versao' valor = ciot_ret-valores_servico-nr_vr_xml_tipf.

  IF ciot_ret-valores_servico-nr_vr_xml_tipf GE '1.15'.
*  "Método para montar a chave de acesso de comunicação com o WebService da Tipcard.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @wa_j_1bbranch-bukrs
       AND branch EQ @wa_j_1bbranch-branch.

    CREATE OBJECT obj_zcl_tipcard.
    var_chave = obj_zcl_tipcard->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
    me->ctnav( tag   = 'chave' valor = var_chave ).

    IF ciot_ret-id_rota IS NOT INITIAL AND ciot_ret-valores_servico-vlr_pedagio NE 0.
      "Rota para a Viagem
      CALL METHOD me->ctnav EXPORTING tag = 'rota' valor = ciot_ret-id_rota.
      CALL METHOD me->ctnav EXPORTING tag = 'admPedagio' valor = ciot_ret-valores_servico-tp_card_ped.

      IF ciot_ret-valores_servico-nr_vr_xml_tipf GE '1.17'.
        lc_eixos = 0.
        LOOP AT ciot_ret-veiculos INTO wa_veiculo.
          MOVE wa_veiculo-qtd_eixo TO lc_eixosi.
          ADD lc_eixosi TO lc_eixos.
        ENDLOOP.
        IF lc_eixos GT 10.
          lc_eixos = 10.
        ENDIF.
        CALL METHOD me->ctnav EXPORTING tag = 'eixos' valor = lc_eixos.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: lc_km TYPE i.
  DATA: lc_km_text TYPE c LENGTH 9.
  lc_km = ciot_ret-distancia.
  lc_km_text = lc_km.
  CONDENSE lc_km_text NO-GAPS.
  CALL METHOD me->ctnav EXPORTING tag = 'distanciaTotal' valor = lc_km_text.
*  ENDIF.

  CALL METHOD me->ctnav EXPORTING tag = 'cnpjContratante' valor = wa_parceiros-cnpj.
  CALL METHOD me->ctnav EXPORTING tag = 'idSolicitacaoContratante' valor = idsolicitacaocontratante.
  CALL METHOD me->ctndhn EXPORTING tag = 'dataSolicitacao' data = sy-datlo hora = sy-timlo.

  IF ciot_ret-dt_origem NE sy-datlo.
    dt_dif = ciot_ret-dt_termin - ciot_ret-dt_origem.
    ciot_ret-dt_origem = sy-datlo.
    ciot_ret-dt_termin = sy-datlo + dt_dif.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataInicioFrete' valor = ciot_ret-dt_origem.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataFimFrete' valor = ciot_ret-dt_termin.
    "CALL METHOD ME->CTNDTN EXPORTING TAG = 'dataFimFrete' VALOR = SPACE.

    CALL METHOD ob_ciot->set_dt_origem
      EXPORTING
        p_dt_origem = ciot_ret-dt_origem.

    CALL METHOD ob_ciot->set_dt_termino
      EXPORTING
        p_dt_termin = ciot_ret-dt_termin.

    CALL METHOD ob_ciot->gravar.

  ELSE.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataInicioFrete' valor = ciot_ret-dt_origem.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataFimFrete' valor = ciot_ret-dt_termin.
  ENDIF.

  "Parceiros""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Ajustes do Destinatorio para TipCard quando for Remessa e Formação de Lote
  CLEAR: wa_zcte_ciot.
  SELECT SINGLE * FROM zcte_ciot INTO wa_zcte_ciot WHERE docnum EQ ciot_ret-docnum.

  "Substituir CIOT_RET-PARCEIROS TIPO 2 para Informação do Local de Entrega
  IF ( sy-subrc EQ 0 ) AND ( NOT wa_zcte_ciot-lr_codigo IS INITIAL ).

    DELETE ciot_ret-parceiros WHERE tipo = '02'.

    wa_parceiros-tipo         = '02'.
    wa_parceiros-codigo       = wa_zcte_ciot-lr_codigo.
    wa_parceiros-nome         = wa_zcte_ciot-lr_nome.
    wa_parceiros-razao        = wa_zcte_ciot-lr_razao.
    wa_parceiros-cnpj         = wa_zcte_ciot-lr_cnpj.
    wa_parceiros-cpf          = wa_zcte_ciot-lr_cpf.
    wa_parceiros-logradouro   = wa_zcte_ciot-lr_logradouro.
    wa_parceiros-numero       = wa_zcte_ciot-lr_numero.
    wa_parceiros-complemento  = wa_zcte_ciot-lr_complemento.
    wa_parceiros-bairro       = wa_zcte_ciot-lr_bairro.
    wa_parceiros-uf           = wa_zcte_ciot-lr_uf.
    wa_parceiros-municipio    = wa_zcte_ciot-lr_municipio.
    wa_parceiros-cep          = wa_zcte_ciot-lr_cep.

    APPEND wa_parceiros TO ciot_ret-parceiros.

    SORT ciot_ret-parceiros BY tipo ASCENDING.
  ENDIF.


  "00 - COntratante
  "05 - Contratado
  CALL METHOD me->ctnab EXPORTING tag = 'parceiros'.
  LOOP AT ciot_ret-parceiros INTO wa_parceiros WHERE tipo NE '00' AND tipo NE '99'.
    CALL METHOD me->ctnab EXPORTING tag = 'parceiro'.
    CALL METHOD me->monta_xml_parceiro
      EXPORTING
        p_parceiros = wa_parceiros.
    CALL METHOD me->ctnfe EXPORTING tag = 'parceiro'.
  ENDLOOP.
  CALL METHOD me->ctnfe EXPORTING tag = 'parceiros'.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Carga""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CALL METHOD me->ctnab EXPORTING tag = 'carga'.
  CALL METHOD me->ctnav EXPORTING tag = 'codigoNaturezaCarga' valor = ciot_ret-carga-natureza.
  CALL METHOD me->ctnavn EXPORTING tag = 'quantidadeTotal' valor = ciot_ret-carga-quantidade precisao = 4.
  CALL METHOD me->ctnaf EXPORTING tag = 'precisao' valor = '04'.
  CALL METHOD me->ctnav EXPORTING tag = 'unidadeMedida' valor = ciot_ret-carga-unidade.
  CALL METHOD me->ctnav EXPORTING tag = 'descricaoProduto' valor = ciot_ret-carga-descmatnr.
  CALL METHOD me->ctnav EXPORTING tag = 'tipoCarga' valor = ciot_ret-inf_acessorias-tipo_carga.
  CALL METHOD me->ctnab EXPORTING tag = 'notas'.

  lv_algodao = abap_false. "*-CS2024000886-09.12.2024-#154506-JT

  LOOP AT ciot_ret-carga-notas INTO wa_nota.
    CALL METHOD me->ctnab EXPORTING tag = 'nota'.
    CALL METHOD me->ctnav EXPORTING tag = 'numeroNF' valor = wa_nota-numero.
    CALL METHOD me->ctnav EXPORTING tag = 'serie' valor = wa_nota-serie.
    CALL METHOD me->ctnav EXPORTING tag = 'modelo' valor = wa_nota-modelo.

    "Busco a Chave na Tabela J_1BNFE_ACTIVE para garantir que a chave esta correta.
    SELECT SINGLE * INTO wa_act_nota
      FROM j_1bnfe_active
     WHERE docnum EQ wa_nota-docnum_nf.

    IF sy-subrc IS INITIAL.
      CONCATENATE wa_act_nota-regio
                  wa_act_nota-nfyear
                  wa_act_nota-nfmonth
                  wa_act_nota-stcd1
                  wa_act_nota-model
                  wa_act_nota-serie
                  wa_act_nota-nfnum9
                  wa_act_nota-docnum9
                  wa_act_nota-cdv INTO vl_chave.
      IF vl_chave <> wa_nota-chave.
        wa_nota-docnum9 = wa_act_nota-docnum9 .
        wa_nota-chave   = vl_chave.
        MODIFY zcte_info_nota FROM wa_nota."transporting chave docnum9.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CALL METHOD me->ctnaf EXPORTING tag = 'chaveNFe' valor = wa_nota-chave.

    READ TABLE ciot_ret-parceiros INTO wa_parceiros WITH KEY tipo = '01'.
    IF sy-subrc IS INITIAL.
      CALL METHOD me->ctnav EXPORTING tag = 'remetenteNome' valor = wa_parceiros-nome.
      CALL METHOD me->ctnaf EXPORTING tag = 'remetenteCPF' valor = wa_parceiros-cpf.
      CALL METHOD me->ctnaf EXPORTING tag = 'remetenteCNPJ' valor = wa_parceiros-cnpj.
    ENDIF.

    CALL METHOD me->ctnavn EXPORTING tag = 'volumeTotalNota' valor = wa_nota-quantidade precisao = 4.
    CALL METHOD me->ctnaf EXPORTING tag = 'precisaoVolTotalNota' valor = '04'.
    CALL METHOD me->ctnav EXPORTING tag = 'unidadeMedida' valor = wa_nota-unidade.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorTotalNota' valor = wa_nota-vl_nota_fiscal precisao = 2.
    CALL METHOD me->ctnaf EXPORTING tag = 'precisaoTotalNota' valor = '02'.
    CALL METHOD me->ctnfe EXPORTING tag = 'nota'.

*-CS2024000886-09.12.2024-#154506-JT-inicio
    SELECT SINGLE matkl
      INTO @DATA(_matkl)
      FROM mara
     WHERE matnr = @wa_nota-material.

    IF sy-subrc = 0.
      SELECT SINGLE low
        INTO @DATA(_grp_mat)
        FROM tvarvc
       WHERE name = 'MAGGI_GR_ALGODAO_PLUMA'
         AND low  = @_matkl.

      IF sy-subrc = 0.
        lv_algodao = abap_true.
      ENDIF.
    ENDIF.
*-CS2024000886-09.12.2024-#154506-JT-fim

  ENDLOOP.
  CALL METHOD me->ctnfe EXPORTING tag = 'notas'.
  CALL METHOD me->ctnfe EXPORTING tag = 'carga'.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "assessoria"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CALL METHOD me->ctnab EXPORTING tag = 'informacoesAcessorias'.
  CALL METHOD me->ctnavn EXPORTING tag = 'percentualTolerancia' valor = ciot_ret-inf_acessorias-perc_tolerancia precisao = 4.
  CALL METHOD me->ctnav EXPORTING tag = 'precisaoTolerancia' valor = '04'.
  CALL METHOD me->ctnavn EXPORTING tag = 'valorUnitMercPerda' valor = ciot_ret-inf_acessorias-vlr_unit_merc precisao = 4.
  CALL METHOD me->ctnav EXPORTING tag = 'precisaoUnitMercPerda' valor = '04'.
  CALL METHOD me->ctnav EXPORTING tag = 'unidMedUnitMercPerda' valor = ciot_ret-inf_acessorias-unid_vlr_merc.

  IF ciot_ret-inf_acessorias-unid_vlr_frete EQ 'TO'.
    vl_frete = ciot_ret-inf_acessorias-vlr_unit_frete / 1000.
  ELSE.
    vl_frete = ciot_ret-inf_acessorias-vlr_unit_frete.
  ENDIF.

  CALL METHOD me->ctnavn EXPORTING tag = 'valorUnitNegociadoFrete' valor = vl_frete precisao = 6.
  CALL METHOD me->ctnav EXPORTING tag = 'precisaoValorNegociado' valor = '06'.
  CALL METHOD me->ctnav EXPORTING tag = 'unidMedUnitNegociadoFrete' valor = 'KG'.

*-CS2024000886-09.12.2024-#154506-JT-inicio
  IF ciot_ret-inf_acessorias-peso_chegada EQ 'X'.
    lv_flag_calculo = 'S'.
*   CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoPerda' valor = 'S'.
*   CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoQuebra' valor = 'S'.
  ELSE.
    lv_flag_calculo = 'N'.
*   CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoPerda' valor = 'N'.
*   CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoQuebra' valor = 'N'.
  ENDIF.

  IF lv_algodao = abap_true.
    lv_flag_calculo = 'N'.
  ENDIF.

  CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoPerda' valor = lv_flag_calculo.
  CALL METHOD me->ctnav EXPORTING tag = 'flagCalculoQuebra' valor = lv_flag_calculo.
*-CS2024000886-09.12.2024-#154506-JT-fim

  CALL METHOD me->ctnav EXPORTING tag = 'flagLiberacaoAdiantamento' valor = ciot_ret-inf_acessorias-flag_lib_adto.
  CALL METHOD me->ctnav EXPORTING tag = 'flagLiberacaoFinalizacao' valor = ciot_ret-inf_acessorias-flag_lib_sldo.
  CALL METHOD me->ctnfe EXPORTING tag = 'informacoesAcessorias'.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  CALL METHOD me->ctnab EXPORTING tag = 'itens'.

  LOOP AT ciot INTO ob_ciot.
    CLEAR: ciot_ret.
    CALL METHOD ob_ciot->get_cd_ciot
      IMPORTING
        pcd_ciot = cd_ciot.

    CALL METHOD ob_ciot->visualizar
      IMPORTING
        p_zcte_ret = ciot_ret.

    CALL METHOD me->ctnab EXPORTING tag = 'item'.

    "contratado"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    READ TABLE ciot_ret-parceiros INTO wa_parceiros WITH KEY tipo = '99'.
    IF sy-subrc IS INITIAL.
      CALL METHOD me->ctnab EXPORTING tag = 'contratado'.
      CALL METHOD me->ctnav EXPORTING tag = 'rntrc' valor = ciot_ret-rntrc.

      CALL METHOD me->monta_xml_parceiro
        EXPORTING
          p_parceiros = wa_parceiros.

      CALL METHOD me->ctnav EXPORTING tag = 'banco' valor = ciot_ret-inf_acessorias-nr_bc_banco.
      CALL METHOD me->ctnav EXPORTING tag = 'agencia' valor = ciot_ret-inf_acessorias-nr_bc_agencia.
      CALL METHOD me->ctnav EXPORTING tag = 'conta' valor = ciot_ret-inf_acessorias-nr_bc_conta.
      CALL METHOD me->ctnav EXPORTING tag = 'tipoconta' valor = 'C'.
      CALL METHOD me->ctnav EXPORTING tag = 'cnpjRecebedor' valor = ciot_ret-inf_acessorias-cnpj_recebedor.
      CALL METHOD me->ctnfe EXPORTING tag = 'contratado'.
    ENDIF.

    "veiculos"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ciot_ret-veiculos IS NOT INITIAL.
      CALL METHOD me->ctnab EXPORTING tag = 'veiculos'.
      LOOP AT ciot_ret-veiculos INTO wa_veiculo.
        CALL METHOD me->ctnab EXPORTING tag = 'veiculo'.
        CALL METHOD me->ctnav EXPORTING tag = 'placa' valor = wa_veiculo-pc_veiculo.
        CALL METHOD me->ctnav EXPORTING tag = 'renavam' valor = wa_veiculo-cd_renavam.
        CALL METHOD me->ctnav EXPORTING tag = 'codigoMunicipio' valor = wa_veiculo-taxjurcode+3(7).
        CALL METHOD me->ctnav EXPORTING tag = 'tipoVeiculo' valor = wa_veiculo-tp_veiculo.
        CALL METHOD me->ctnav EXPORTING tag = 'rntrc' valor = wa_veiculo-prop_rntrc.
        IF ciot_ret-valores_servico-nr_vr_xml_tipf GE '1.15' AND ciot_ret-id_rota IS NOT INITIAL.
          CALL METHOD me->ctnav EXPORTING tag = 'tagCaminhao' valor = wa_veiculo-nr_card_ped.
          MOVE wa_veiculo-qtd_eixo TO lc_eixosi.
          MOVE lc_eixosi TO lc_eixos.
          CALL METHOD me->ctnav EXPORTING tag = 'eixos' valor = lc_eixos.
        ENDIF.

*-CS2024001181-16.12.2024-#160717-JT-inicio
        IF ciot_ret-valores_servico-tp_card_ped = 'G' AND wa_veiculo-tp_veiculo = '0'.
          CALL METHOD me->ctnav EXPORTING tag = 'numeroTagStrada' valor = wa_veiculo-nr_tag_strada.
        ENDIF.
*-CS2024001181-16.12.2024-#160717-JT-fim

        CALL METHOD me->ctnfe EXPORTING tag = 'veiculo'.
      ENDLOOP.
      CALL METHOD me->ctnfe EXPORTING tag = 'veiculos'.
    ENDIF.

    IF ciot_ret-valores_servico-vlr_triagem GT 0.
      CALL METHOD me->ctnab EXPORTING tag = 'ajustes'.
      IF ciot_ret-valores_servico-vlr_triagem GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'ajuste'.
        CALL METHOD me->ctnav EXPORTING tag = 'tipo' valor = '22'.
        me->ctnavn( tag = 'valor' valor = ciot_ret-valores_servico-vlr_triagem precisao = 2 ).
        "CALL METHOD ME->CTNAVN EXPORTING TAG = 'valor'      VALOR    = CIOT_RET-VALORES_SERVICO-VLR_TRIAGEM = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnav EXPORTING tag = 'observacao' valor = 'TRIAGEM'.
        CALL METHOD me->ctnfe EXPORTING tag = 'ajuste'.
      ENDIF.
      CALL METHOD me->ctnfe EXPORTING tag = 'ajustes'.
    ENDIF.

    "servico""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnab EXPORTING tag = 'servico'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorPedagio' valor = ciot_ret-valores_servico-vlr_pedagio precisao = 2.
    CALL METHOD me->ctnav EXPORTING tag = 'precisaoPedagio' valor = '02'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorSeguro' valor = ciot_ret-valores_servico-vlr_seguro precisao = 2.
    CALL METHOD me->ctnav EXPORTING tag = 'precisaoSeguro' valor = '02'.

    IF ciot_ret-inf_acessorias-vlr_inss GT 0 OR
       ciot_ret-inf_acessorias-vlr_sest GT 0 OR
       ciot_ret-inf_acessorias-vlr_irpf GT 0 OR
       ciot_ret-inf_acessorias-vlr_iss  GT 0 OR
       ciot_ret-inf_acessorias-vlr_iof  GT 0 .
      CALL METHOD me->ctnab EXPORTING tag = 'impostos'.
      IF ciot_ret-inf_acessorias-vlr_inss GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav EXPORTING tag = 'nome' valor = 'INSS'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor' valor = ciot_ret-inf_acessorias-vlr_inss precisao = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe EXPORTING tag = 'imposto'.
      ENDIF.
      IF ciot_ret-inf_acessorias-vlr_sest GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav EXPORTING tag = 'nome' valor = 'SEST'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor' valor = ciot_ret-inf_acessorias-vlr_sest precisao = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe EXPORTING tag = 'imposto'.
      ENDIF.
      IF ciot_ret-inf_acessorias-vlr_irpf GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav EXPORTING tag = 'nome' valor = 'IRPF'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor' valor = ciot_ret-inf_acessorias-vlr_irpf precisao = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe EXPORTING tag = 'imposto'.
      ENDIF.
      IF ciot_ret-inf_acessorias-vlr_iof GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav EXPORTING tag = 'nome' valor = 'IOF'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor' valor = ciot_ret-inf_acessorias-vlr_iof precisao = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe EXPORTING tag = 'imposto'.
      ENDIF.
      IF ciot_ret-inf_acessorias-vlr_iss GT 0.
        CALL METHOD me->ctnab EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav EXPORTING tag = 'nome' valor = 'ISS'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor' valor = ciot_ret-inf_acessorias-vlr_iss precisao = 2.
        CALL METHOD me->ctnav EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe EXPORTING tag = 'imposto'.
      ENDIF.
      CALL METHOD me->ctnfe EXPORTING tag = 'impostos'.
    ENDIF.

    CALL METHOD me->ctnavn EXPORTING tag = 'valorContratoFrete' valor = ciot_ret-valores_servico-vlr_frete precisao = 2.
    CALL METHOD me->ctnav EXPORTING tag = 'precisaoContratoFrete' valor = '02'.
    CALL METHOD me->ctnavn EXPORTING tag = 'limiteAdiantamento' valor = ciot_ret-valores_servico-vlr_adiantamento precisao = 2.
    CALL METHOD me->ctnav EXPORTING tag = 'precisaoLimAdiantamento' valor = '02'.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataPrevisaoAdiantamento' valor = sy-datum.
    CALL METHOD me->ctnfe EXPORTING tag = 'servico'.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnfe EXPORTING tag = 'item'.
  ENDLOOP.

  CALL METHOD me->ctnfe EXPORTING tag = 'itens'.
  CALL METHOD me->ctnav EXPORTING tag = 'idViagemStradaLog' valor = ciot_ret-id_viagem. "*#127471-18.04.2024-JT
  CALL METHOD me->ctnfe EXPORTING tag = 'viagem'.

ENDMETHOD.


METHOD MONTA_XML_CANCELA.

  DATA: OB_CIOT         TYPE REF TO ZCL_CIOT,
        CD_CIOT         TYPE ZCIOT,
        WA_CIOT         TYPE ZCIOT_RET,
        WA_PARCEIROS    TYPE ZCIOT_CONTRATANTE,
        WA_NOTA         TYPE ZCTE_INFO_NOTA,
        WA_VEICULO      TYPE ZCTE_TRANS,
        VG_MODEL        TYPE J_1BMODEL,
        VG_SERIES       TYPE J_1BSERIES,
        VG_NUMERO       TYPE J_1BNFNUM9,
        VG_MOTIVO_CAN   TYPE STRING,
        VAR_MSG         TYPE STRING,
        CL_EXCEPTION    TYPE REF TO ZCX_WEBSERVICE,
        VAR_CHAVE       TYPE CHAR32,
        OBJ_ZCL_TIPCARD TYPE REF TO ZCL_WEBSERVICE_TIPCARD.

  DATA: LC_BUKRS      TYPE BUKRS,
        LC_BRANCH	    TYPE J_1BBRANC_,
        WA_J_1BBRANCH TYPE  J_1BBRANCH.

  CLEAR: XML_VIAGEM.

  READ TABLE CIOT INTO OB_CIOT INDEX 1.

  CALL METHOD OB_CIOT->GET_CD_CIOT
    IMPORTING
      PCD_CIOT = CD_CIOT.

  CALL METHOD OB_CIOT->GET_INFO_CIOT
    EXPORTING
      PCD_CIOT   = CD_CIOT
      PVISUALIZA = SPACE
    IMPORTING
      INF_CIOT   = WA_CIOT.

  CALL METHOD OB_CIOT->GET_INF_CONHEC
    IMPORTING
      P_MODEL  = VG_MODEL
      P_SERIES = VG_SERIES
      P_NUMERO = VG_NUMERO.

  "Buscar Motivo do Cancelamento ob_ciot
  IF P_REASON1 IS NOT INITIAL.
    VG_MOTIVO_CAN = P_REASON1.
  ENDIF.
  IF P_REASON2  IS NOT INITIAL.
    IF VG_MOTIVO_CAN IS INITIAL.
      VG_MOTIVO_CAN = P_REASON2.
    ELSE.
      CONCATENATE VG_MOTIVO_CAN P_REASON2 INTO VG_MOTIVO_CAN SEPARATED BY SPACE.
    ENDIF.
  ENDIF.
  IF P_REASON3 IS NOT INITIAL.
    IF VG_MOTIVO_CAN IS INITIAL.
      VG_MOTIVO_CAN = P_REASON3.
    ELSE.
      CONCATENATE VG_MOTIVO_CAN P_REASON3 INTO VG_MOTIVO_CAN SEPARATED BY SPACE.
    ENDIF.
  ENDIF.
  IF P_REASON4 IS NOT INITIAL.
    IF VG_MOTIVO_CAN IS INITIAL.
      VG_MOTIVO_CAN = P_REASON4.
    ELSE.
      CONCATENATE VG_MOTIVO_CAN P_REASON4 INTO VG_MOTIVO_CAN SEPARATED BY SPACE.
    ENDIF.
  ENDIF.

  OB_CIOT->GET_TKNUM( IMPORTING P_TKNUM = DATA(LC_TKNUM) ).

  SELECT SINGLE * INTO @DATA(WA_VTTK)
    FROM VTTK
   WHERE TKNUM EQ @LC_TKNUM.

  TRY .
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
        )->SET_PARCEIRO( I_PARCEIRO = WA_VTTK-TDLNR
        )->CK_PARCEIRO_LOCAL_NEGOCIO(
        ).
    CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
      MESSAGE ID EX_PARCEIROS->MSGID TYPE EX_PARCEIROS->MSGTY
       NUMBER EX_PARCEIROS->MSGNO
         WITH EX_PARCEIROS->MSGV1 EX_PARCEIROS->MSGV2 EX_PARCEIROS->MSGV3 EX_PARCEIROS->MSGV4 .
  ENDTRY.

  MOVE: WA_VTTK-TDLNR+6(4) TO LC_BRANCH.

  CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
    EXPORTING
      CENTRO               = LC_BRANCH
    IMPORTING
      WA_J_1BBRANCH        = WA_J_1BBRANCH
    EXCEPTIONS
      INFORMAR_CENTRO      = 1
      NAO_CENTRO_R_VIRTUAL = 2
      INFORMAR_CENTRO_OUT  = 3
      INFORMAR_CENTRO_V    = 4
      OTHERS               = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD ME->CTNAB EXPORTING TAG = 'confirmacaoViagem'.
  CALL METHOD ME->CTNAV EXPORTING TAG = 'tipoOperacao' VALOR = '02'.
  CALL METHOD ME->CTNAV EXPORTING TAG = 'idOperacaoViagem' VALOR = WA_CIOT-ID_OP_VIAGEM_ADM.
  CALL METHOD ME->CTNDHN EXPORTING TAG = 'dataRequisicao' DATA = SY-DATLO HORA = SY-TIMLO.

  CREATE OBJECT OBJ_ZCL_TIPCARD.
  TRY.
      SELECT SINGLE * INTO @DATA(WA_ZLEST0160)
        FROM ZLEST0160
       WHERE BUKRS  EQ @WA_J_1BBRANCH-BUKRS
         AND BRANCH EQ @WA_J_1BBRANCH-BRANCH.

      VAR_CHAVE = OBJ_ZCL_TIPCARD->CHAVE_SEGURANCA( I_GRUPO = WA_ZLEST0160-DS_GRUPO ).
      ME->CTNAV( TAG   = 'chave' VALOR = VAR_CHAVE ).
    CATCH ZCX_WEBSERVICE INTO CL_EXCEPTION.
      VAR_MSG = CL_EXCEPTION->GET_TEXT( ).
      MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
  ENDTRY.

  CALL METHOD ME->CTNAV EXPORTING TAG = 'modeloConhecimento' VALOR = VG_MODEL.
  CALL METHOD ME->CTNAV EXPORTING TAG = 'serieConhecimento' VALOR = VG_SERIES.
  CALL METHOD ME->CTNAV EXPORTING TAG = 'numeroConhecimento' VALOR = VG_NUMERO.
  CALL METHOD ME->CTNAF EXPORTING TAG = 'motivoCancelamento' VALOR = VG_MOTIVO_CAN.
  CALL METHOD ME->CTNAF EXPORTING TAG = 'usuarioContratante' VALOR = SY-UNAME.
  CALL METHOD ME->CTNFE EXPORTING TAG = 'confirmacaoViagem'.

ENDMETHOD.


METHOD monta_xml_credito.

  DATA: ob_ciot         TYPE REF TO zcl_ciot,
        cd_ciot         TYPE zciot,
        wa_ciot         TYPE zciot_ret,
        wa_parceiros    TYPE zciot_contratante,
        wa_nota         TYPE zcte_info_nota,
        wa_veiculo      TYPE zcte_trans,
        vg_model        TYPE j_1bmodel,
        vg_series       TYPE j_1bseries,
        vg_numero       TYPE j_1bnfnum9,
        var_msg         TYPE string,
        cl_exception    TYPE REF TO zcx_webservice,
        var_chave       TYPE char32,
        obj_zcl_tipcard TYPE REF TO zcl_webservice_tipcard.

  DATA: lc_bukrs      TYPE bukrs,
        lc_branch	    TYPE j_1bbranc_,
        wa_j_1bbranch TYPE  j_1bbranch,
        t_zsdt0105    TYPE TABLE OF zsdt0105,  "*#127471-18.04.2024-JT
        t_zsdt0102    TYPE TABLE OF zsdt0102,  "*#127471-18.04.2024-JT
        l_chave_mdfe  TYPE zde_chave_nfe,      "*#127471-18.04.2024-JT
        zcl_util      TYPE REF TO zcl_util.    "*#127471-18.04.2024-JT

  CLEAR: xml_viagem, t_zsdt0105, t_zsdt0102.  "*#127471-18.04.2024-JT

  READ TABLE ciot INTO ob_ciot INDEX 1.

  CALL METHOD ob_ciot->get_cd_ciot
    IMPORTING
      pcd_ciot = cd_ciot.

  CALL METHOD ob_ciot->get_info_ciot
    EXPORTING
      pcd_ciot   = cd_ciot
      pvisualiza = space
    IMPORTING
      inf_ciot   = wa_ciot.

  CALL METHOD ob_ciot->get_inf_conhec
    IMPORTING
      p_model  = vg_model
      p_series = vg_series
      p_numero = vg_numero.

  ob_ciot->get_tknum( IMPORTING p_tknum = DATA(lc_tknum) ).

  SELECT SINGLE * INTO @DATA(wa_vttk)
    FROM vttk
   WHERE tknum EQ @lc_tknum.

  TRY .
      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wa_vttk-tdlnr
        )->ck_parceiro_local_negocio(
        ).
    CATCH zcx_parceiros INTO DATA(ex_parceiros).
      MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty
       NUMBER ex_parceiros->msgno
         WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 .
  ENDTRY.

  MOVE: wa_vttk-tdlnr+6(4) TO lc_branch.

  CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
    EXPORTING
      centro               = lc_branch
    IMPORTING
      wa_j_1bbranch        = wa_j_1bbranch
    EXCEPTIONS
      informar_centro      = 1
      nao_centro_r_virtual = 2
      informar_centro_out  = 3
      informar_centro_v    = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*#127471-18.04.2024-JT-inicio
  SELECT *
    FROM zsdt0105
    INTO TABLE t_zsdt0105
   WHERE docnum = wa_ciot-docnum.

  IF t_zsdt0105[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0102
      INTO TABLE t_zsdt0102
       FOR ALL ENTRIES IN t_zsdt0105
     WHERE docnum       = t_zsdt0105-docnum_ref
       AND autorizado   = abap_true
       AND estornado    = abap_false
       AND cancel       = abap_false.
  ENDIF.
*#127471-18.04.2024-JT-fim

  CALL METHOD me->ctnab EXPORTING tag = 'confirmacaoViagem'.
  CALL METHOD me->ctnav EXPORTING tag = 'tipoOperacao' valor = '01'.
  CALL METHOD me->ctnav EXPORTING tag = 'idOperacaoViagem' valor = wa_ciot-id_op_viagem_adm.
  CALL METHOD me->ctndhn EXPORTING tag = 'dataRequisicao' data = sy-datlo hora = sy-timlo.

  CREATE OBJECT obj_zcl_tipcard.
  TRY.

      SELECT SINGLE * INTO @DATA(wa_zlest0160)
        FROM zlest0160
       WHERE bukrs  EQ @wa_j_1bbranch-bukrs
         AND branch EQ @wa_j_1bbranch-branch.

      var_chave = obj_zcl_tipcard->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      me->ctnav( tag   = 'chave' valor = var_chave ).
    CATCH zcx_webservice INTO cl_exception.
      var_msg = cl_exception->get_text( ).
      MESSAGE e007(zwebservice) WITH var_msg.
  ENDTRY.

  CALL METHOD me->ctnav EXPORTING tag = 'modeloConhecimento' valor = vg_model.
  CALL METHOD me->ctnav EXPORTING tag = 'serieConhecimento' valor = vg_series.
  CALL METHOD me->ctnav EXPORTING tag = 'numeroConhecimento' valor = vg_numero.
  CALL METHOD me->ctnaf EXPORTING tag = 'motivoCancelamento' valor = space.
  CALL METHOD me->ctnaf EXPORTING tag = 'usuarioContratante' valor = space.

*#127471-18.04.2024-JT-inicio
  CREATE OBJECT zcl_util.

  CALL METHOD me->ctnab EXPORTING tag = 'mdfes'.

  LOOP AT t_zsdt0102 INTO DATA(w_0102).
    l_chave_mdfe  = zcl_util->get_chave_nfe( w_0102-docnum ).
    CALL METHOD me->ctnab EXPORTING tag = 'mdfe'.
    CALL METHOD me->ctnav EXPORTING tag = 'numero' valor = w_0102-nmdfe.
    CALL METHOD me->ctnav EXPORTING tag = 'chave' valor = l_chave_mdfe.
    CALL METHOD me->ctnfe EXPORTING tag = 'mdfe'.
  ENDLOOP.

  CALL METHOD me->ctnfe EXPORTING tag = 'mdfes'.
*#127471-18.04.2024-JT-fim

  CALL METHOD me->ctnfe EXPORTING tag = 'confirmacaoViagem'.

ENDMETHOD.


METHOD MONTA_XML_PARCEIRO.

  DATA: VG_CEP  TYPE STRING,
        VG_FONE TYPE TELF1.

  IF P_PARCEIROS-NOME IS NOT INITIAL.

    IF P_PARCEIROS-TIPO NE '99'.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'tipo'  VALOR = P_PARCEIROS-TIPO.
    ENDIF.

    CALL METHOD ME->CTNAV     EXPORTING  TAG   = 'nome'       VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-NOME ) ) ).

    IF P_PARCEIROS-TIPO EQ '05' AND CIOT_RET-VALORES_SERVICO-NR_VR_XML_TIPF GE '1.15'.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'nomeMae'        VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-NOMEMAE ) ) ).
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'sexo'           VALOR = P_PARCEIROS-SEXO.
      CALL METHOD ME->CTNDTN  EXPORTING  TAG   = 'dataNascimento' VALOR = P_PARCEIROS-DT_NASCIMENTO.
    ENDIF.

    CALL METHOD ME->CTNAF     EXPORTING  TAG   = 'numeroCNPJ'     VALOR = P_PARCEIROS-CNPJ.
    CALL METHOD ME->CTNAF     EXPORTING  TAG   = 'numeroCPF'      VALOR = P_PARCEIROS-CPF.

    IF P_PARCEIROS-TIPO EQ '05'.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'numeroRG'       VALOR = P_PARCEIROS-RG.
      IF CIOT_RET-VALORES_SERVICO-NR_VR_XML_TIPF GE '1.15'.
        CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'orgaoEmissorRG' VALOR = P_PARCEIROS-ORG_RG.
      ENDIF.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'ufRG'           VALOR = P_PARCEIROS-UF_RG.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'numeroCNH'      VALOR = P_PARCEIROS-CNH.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'ufCNH'          VALOR = P_PARCEIROS-UF_CNH.
      CALL METHOD ME->CTNAV   EXPORTING  TAG   = 'numeroPIS'      VALOR = P_PARCEIROS-PIS.
    ENDIF.

    "Endereço"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF P_PARCEIROS-TIPO NE '99'.
      CALL METHOD ME->CTNAB   EXPORTING  TAG = 'endereco'.
    ENDIF.
    CALL METHOD ME->CTNAV     EXPORTING  TAG   = 'logradouro'      VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-LOGRADOURO ) ) ).
    CALL METHOD ME->CTNAV     EXPORTING  TAG   = 'numero'          VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-NUMERO ) ) ).
    CALL METHOD ME->CTNAF     EXPORTING  TAG   = 'complemento'     VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-COMPLEMENTO ) ) ).
    CALL METHOD ME->CTNAV     EXPORTING  TAG   = 'bairro'          VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( P_PARCEIROS-BAIRRO ) ) ).
    CALL METHOD ME->CTNAV     EXPORTING  TAG   = 'codigoMunicipio' VALOR = P_PARCEIROS-MUNICIPIO.
    VG_CEP = P_PARCEIROS-CEP.
    REPLACE ALL OCCURRENCES OF '-' IN VG_CEP WITH '' IGNORING CASE.
    CALL METHOD ME->CTNNAONULO  EXPORTING TAG   = 'cep' TAM   = 08  VALOR = VG_CEP.

    VG_FONE = P_PARCEIROS-FONE.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN VG_FONE WITH '' IGNORING CASE.
    CALL METHOD ME->CTNNAONULO EXPORTING TAG   = 'fone' TAM   = 12 VALOR = VG_FONE.
    IF P_PARCEIROS-TIPO NE '99'.
      CALL METHOD ME->CTNFE  EXPORTING TAG = 'endereco'.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDIF.

ENDMETHOD.


METHOD MONTA_XML_RESCISAO.

  DATA: ob_ciot        TYPE REF TO zcl_ciot,
        cd_ciot        TYPE zciot,
        wa_ciot        TYPE zciot_ret,
        wa_parceiros   TYPE zciot_contratante,
        wa_nota        type zcte_info_nota,
        wa_veiculo     type zcte_trans.

  CLEAR: xml_viagem.

  READ TABLE ciot INTO ob_ciot INDEX 1.

  CALL METHOD ob_ciot->get_cd_ciot
    IMPORTING
      pcd_ciot = cd_ciot.

  CALL METHOD ob_ciot->get_info_ciot
    EXPORTING
      pcd_ciot   = cd_ciot
      pvisualiza = space
    IMPORTING
      inf_ciot   = wa_ciot.

  read table wa_ciot-parceiros into wa_parceiros with key tipo = '00'.

  CALL METHOD me->ctnab  EXPORTING tag = 'viagem'.
  CALL METHOD me->ctnav  EXPORTING tag = 'idContratante'            valor = wa_parceiros-cnpj.
  CALL METHOD me->ctnav  EXPORTING tag = 'idSolicitacaoContratante' valor = wa_ciot-docnum.
  CALL METHOD me->ctndhn EXPORTING tag = 'dataSolicitacao'          data  = sy-datlo hora = sy-timlo.

  CALL METHOD me->ctnab  EXPORTING tag = 'itens'.
  LOOP AT ciot INTO ob_ciot.
    clear: wa_ciot.
    CALL METHOD ob_ciot->get_cd_ciot   IMPORTING pcd_ciot = cd_ciot.
    CALL METHOD ob_ciot->get_info_ciot EXPORTING pcd_ciot = cd_ciot pvisualiza = space IMPORTING inf_ciot = wa_ciot.

    CALL METHOD me->ctnab  EXPORTING tag = 'item'.
    CALL METHOD me->ctnav  EXPORTING tag = 'rntrcContratado'        valor = wa_ciot-rntrc.
    CALL METHOD me->ctnav  EXPORTING tag = 'codigoMunicipioInicio'  valor = wa_ciot-municipio_origem.
    CALL METHOD me->ctnav  EXPORTING tag = 'codigoMunicipioTermino' valor = wa_ciot-municipio_termin.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataInicioFrete'        valor = wa_ciot-dt_origem.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataFimFrete'           valor = wa_ciot-dt_termin.
    "Parceiros""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnab  EXPORTING tag = 'parceiros'.
    LOOP AT wa_ciot-parceiros INTO wa_parceiros WHERE tipo NE '00'.
      IF wa_parceiros-nome IS NOT INITIAL.
        CALL METHOD me->ctnab  EXPORTING tag = 'parceiro'.
        CALL METHOD me->ctnav  EXPORTING tag = 'tipo'       valor = wa_parceiros-tipo.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'       valor = wa_parceiros-nome.
        CALL METHOD me->ctnaf  EXPORTING tag = 'numeroCNPJ' valor = wa_parceiros-cnpj.
        CALL METHOD me->ctnaf  EXPORTING tag = 'numeroCPF'  valor = wa_parceiros-cpf.
        "Endereço"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        CALL METHOD me->ctnab  EXPORTING tag = 'endereco'.
        CALL METHOD me->ctnav  EXPORTING tag = 'logradouro'      valor = wa_parceiros-logradouro.
        CALL METHOD me->ctnav  EXPORTING tag = 'numero'          valor = wa_parceiros-numero.
        CALL METHOD me->ctnaf  EXPORTING tag = 'complemento'     valor = wa_parceiros-complemento.
        CALL METHOD me->ctnav  EXPORTING tag = 'bairro'          valor = wa_parceiros-bairro.
        CALL METHOD me->ctnav  EXPORTING tag = 'codigoMunicipio' valor = wa_parceiros-municipio.
        CALL METHOD me->ctnav  EXPORTING tag = 'cep'             valor = wa_parceiros-cep.
        CALL METHOD me->ctnav  EXPORTING tag = 'fone'            valor = space.
        CALL METHOD me->ctnfe  EXPORTING tag = 'endereco'.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        CALL METHOD me->ctnfe  EXPORTING tag = 'parceiro'.
      ENDIF.
    ENDLOOP.
    CALL METHOD me->ctnfe  EXPORTING tag = 'parceiros'.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Carga""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnab  EXPORTING tag = 'carga'.
    CALL METHOD me->ctnav  EXPORTING tag = 'codigoNaturezaCarga' valor = wa_ciot-carga-natureza.
    CALL METHOD me->ctnavn EXPORTING tag = 'quantidadeTotal'     valor = wa_ciot-carga-quantidade precisao = 4.
    CALL METHOD me->ctnaf  EXPORTING tag = 'precisao'            valor = '04'.
    CALL METHOD me->ctnav  EXPORTING tag = 'unidadeMedida'       valor = wa_ciot-carga-unidade.
    CALL METHOD me->ctnab  EXPORTING tag = 'notas'.
    LOOP AT wa_ciot-notas into wa_nota.
      CALL METHOD me->ctnab  EXPORTING tag = 'nota'.
      CALL METHOD me->ctnav  EXPORTING tag = 'numeroNF' valor = wa_nota-numero.
      CALL METHOD me->ctnav  EXPORTING tag = 'serie'    valor = wa_nota-serie.
      CALL METHOD me->ctnav  EXPORTING tag = 'modelo'   valor = wa_nota-modelo.
      CALL METHOD me->ctnaf  EXPORTING tag = 'chaveNFe' valor = wa_nota-chave.
      READ TABLE wa_ciot-parceiros INTO wa_parceiros WITH KEY tipo = '01'.
      IF sy-subrc IS INITIAL.
        CALL METHOD me->ctnav  EXPORTING tag = 'remetenteNome' valor = wa_parceiros-nome.
        CALL METHOD me->ctnaf  EXPORTING tag = 'remetenteCNPJ' valor = wa_parceiros-cnpj.
        CALL METHOD me->ctnaf  EXPORTING tag = 'remetenteCPF'  valor = wa_parceiros-cpf.
      ENDIF.
      CALL METHOD me->ctnavn EXPORTING tag = 'volumeTotalNota'      valor = wa_nota-quantidade precisao = 4.
      CALL METHOD me->ctnaf  EXPORTING tag = 'precisaoVolTotalNota' valor = '04'.
      CALL METHOD me->ctnav  EXPORTING tag = 'unidadeMedida'        valor = wa_nota-unidade.
      CALL METHOD me->ctnavn EXPORTING tag = 'volumeTotalNota'      valor = wa_nota-vl_nota_fiscal precisao = 2.
      CALL METHOD me->ctnaf  EXPORTING tag = 'precisaoVolTotalNota' valor = '02'.
      CALL METHOD me->ctnfe  EXPORTING tag = 'nota'.
    ENDLOOP.
    CALL METHOD me->ctnfe  EXPORTING tag = 'notas'.
    CALL METHOD me->ctnfe  EXPORTING tag = 'carga'.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "veiculos"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF wa_ciot-veiculos IS NOT INITIAL.
      CALL METHOD me->ctnab  EXPORTING tag = 'veiculos'.
      LOOP AT wa_ciot-veiculos INTO wa_veiculo.
        CALL METHOD me->ctnab  EXPORTING tag = 'veiculo'.
        CALL METHOD me->ctnav  EXPORTING tag = 'placa'           valor = wa_veiculo-pc_veiculo.
        CALL METHOD me->ctnav  EXPORTING tag = 'Renavam'         valor = wa_veiculo-cd_renavam.
        CALL METHOD me->ctnav  EXPORTING tag = 'codigoMunicipio' valor = wa_veiculo-cd_cidade.
        CALL METHOD me->ctnav  EXPORTING tag = 'tipoVeiculo'     valor = wa_veiculo-tp_veiculo.
        CALL METHOD me->ctnav  EXPORTING tag = 'rntrc'           valor = wa_veiculo-prop_rntrc.
        CALL METHOD me->ctnfe  EXPORTING tag = 'veiculo'.
      ENDLOOP.
      CALL METHOD me->ctnfe  EXPORTING tag = 'veiculos'.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "servico""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnab  EXPORTING tag = 'servico'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorPedagio'             valor = wa_ciot-valores_servico-vlr_pedagio precisao = 2.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoPedagio'          valor = '02'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorSeguro'              valor = wa_ciot-valores_servico-vlr_pedagio precisao = 2.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoSeguro'           valor = '02'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorContratoFrete'       valor = wa_ciot-valores_servico-vlr_frete   precisao = 2.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoContratoFrete'    valor = '02'.
    CALL METHOD me->ctnavn EXPORTING tag = 'limiteAdiantamento'       valor = wa_ciot-valores_servico-vlr_adiantamento precisao = 2.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoLimAdiantamento'  valor = '02'.
    CALL METHOD me->ctndtn EXPORTING tag = 'dataPrevisaoAdiantamento' valor = sy-datum.
    IF wa_ciot-inf_acessorias-vlr_inss GT 0 OR
       wa_ciot-inf_acessorias-vlr_sest GT 0 OR
       wa_ciot-inf_acessorias-vlr_irpf GT 0 OR
       wa_ciot-inf_acessorias-vlr_iss  GT 0 OR
       wa_ciot-inf_acessorias-vlr_iof gt 0 .
      CALL METHOD me->ctnab  EXPORTING tag = 'impostos'.
      IF wa_ciot-inf_acessorias-vlr_inss GT 0.
        CALL METHOD me->ctnab  EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'     valor = 'INSS'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor'    valor = wa_ciot-inf_acessorias-vlr_inss precisao = 2.
        CALL METHOD me->ctnav  EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe  EXPORTING tag = 'imposto'.
      ENDIF.
      IF wa_ciot-inf_acessorias-vlr_sest GT 0.
        CALL METHOD me->ctnab  EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'     valor = 'SEST'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor'    valor = wa_ciot-inf_acessorias-vlr_sest precisao = 2.
        CALL METHOD me->ctnav  EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe  EXPORTING tag = 'imposto'.
      ENDIF.
      IF wa_ciot-inf_acessorias-vlr_irpf GT 0.
        CALL METHOD me->ctnab  EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'     valor = 'IRPF'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor'    valor = wa_ciot-inf_acessorias-vlr_irpf precisao = 2.
        CALL METHOD me->ctnav  EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe  EXPORTING tag = 'imposto'.
      ENDIF.
      IF wa_ciot-inf_acessorias-vlr_iof GT 0.
        CALL METHOD me->ctnab  EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'     valor = 'IOF'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor'    valor = wa_ciot-inf_acessorias-vlr_iof precisao = 2.
        CALL METHOD me->ctnav  EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe  EXPORTING tag = 'imposto'.
      ENDIF.
      IF wa_ciot-inf_acessorias-vlr_iss GT 0.
        CALL METHOD me->ctnab  EXPORTING tag = 'imposto'.
        CALL METHOD me->ctnav  EXPORTING tag = 'nome'     valor = 'ISS'.
        CALL METHOD me->ctnavn EXPORTING tag = 'valor'    valor = wa_ciot-inf_acessorias-vlr_iss precisao = 2.
        CALL METHOD me->ctnav  EXPORTING tag = 'precisao' valor = '02'.
        CALL METHOD me->ctnfe  EXPORTING tag = 'imposto'.
      ENDIF.
      CALL METHOD me->ctnfe  EXPORTING tag = 'impostos'.
    ENDIF.
    CALL METHOD me->ctnfe  EXPORTING tag = 'servico'.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "assessoria"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnab  EXPORTING tag = 'assessoria'.
    CALL METHOD me->ctnavn EXPORTING tag = 'percentualTolerancia'      valor = wa_ciot-inf_acessorias-perc_tolerancia precisao = 4.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoTolerancia'        valor = '04'.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorUnitMercPerda'        valor = wa_ciot-inf_acessorias-vlr_unit_merc precisao = 4.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoUnitMercPerda'     valor = '04'.
    CALL METHOD me->ctnav  EXPORTING tag = 'unidMedUnitMercPerda'      valor = wa_ciot-inf_acessorias-unid_vlr_merc.
    CALL METHOD me->ctnavn EXPORTING tag = 'valorUnitNegociadoFrete'   valor = wa_ciot-inf_acessorias-vlr_unit_frete precisao = 4.
    CALL METHOD me->ctnav  EXPORTING tag = 'precisaoValorNegociado'    valor = '04'.
    CALL METHOD me->ctnav  EXPORTING tag = 'unidMedUnitNegociadoFrete' valor = wa_ciot-inf_acessorias-unid_vlr_frete.
    IF wa_ciot-inf_acessorias-peso_chegada EQ 'X'.
      CALL METHOD me->ctnav  EXPORTING tag = 'flagCalculoPerda'  valor = 'S'.
      CALL METHOD me->ctnav  EXPORTING tag = 'flagCalculoQuebra' valor = 'S'.
    ELSE.
      CALL METHOD me->ctnav  EXPORTING tag = 'flagCalculoPerda'  valor = 'N'.
      CALL METHOD me->ctnav  EXPORTING tag = 'flagCalculoQuebra' valor = 'N'.
    ENDIF.
    CALL METHOD me->ctnfe  EXPORTING tag = 'assessoria'.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->ctnfe  EXPORTING tag = 'item'.
  ENDLOOP.
  CALL METHOD me->ctnfe  EXPORTING tag = 'itens'.
  CALL METHOD me->ctnfe  EXPORTING tag = 'viagem'.

ENDMETHOD.


METHOD MONTA_XML_SOLICITA_REQ.

  DATA: WA_ZCTE_VIAGEM  TYPE ZCTE_VIAGEM,
        VG_PROTOCOLO    TYPE ZPROTOCOLO,
        OB_CIOT         TYPE REF TO ZCL_CIOT,
        VG_VIAGEM       TYPE ZIDVIAGEMSOLI,
        VG_DOCNUM       TYPE J_1BDOCNUM,
        VG_TKNUM        TYPE TKNUM,
        VAR_MSG         TYPE STRING,
        CL_EXCEPTION    TYPE REF TO ZCX_WEBSERVICE,
        VAR_CHAVE       TYPE CHAR32,
        OBJ_ZCL_TIPCARD TYPE REF TO ZCL_WEBSERVICE_TIPCARD.

  DATA: LC_BUKRS      TYPE BUKRS,
        LC_BRANCH	    TYPE J_1BBRANC_,
        WA_J_1BBRANCH TYPE  J_1BBRANCH.

  READ TABLE CIOT INTO OB_CIOT INDEX 1.

  CALL METHOD OB_CIOT->GET_DOCNUM
    IMPORTING
      P_DOCNUM = VG_DOCNUM.

  CALL METHOD OB_CIOT->GET_TKNUM
    IMPORTING
      P_TKNUM = VG_TKNUM.

  SELECT SINGLE * INTO @DATA(WA_VTTK)
    FROM VTTK
   WHERE TKNUM EQ @VG_TKNUM.

  TRY .
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
        )->SET_PARCEIRO( I_PARCEIRO = WA_VTTK-TDLNR
        )->CK_PARCEIRO_LOCAL_NEGOCIO(
        ).
    CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
      MESSAGE ID EX_PARCEIROS->MSGID TYPE EX_PARCEIROS->MSGTY
       NUMBER EX_PARCEIROS->MSGNO
         WITH EX_PARCEIROS->MSGV1 EX_PARCEIROS->MSGV2 EX_PARCEIROS->MSGV3 EX_PARCEIROS->MSGV4 RAISING SEM_PROTOCOLO.
  ENDTRY.

  MOVE: WA_VTTK-TDLNR+6(4) TO LC_BRANCH.

  CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
    EXPORTING
      CENTRO               = LC_BRANCH
    IMPORTING
      WA_J_1BBRANCH        = WA_J_1BBRANCH
    EXCEPTIONS
      INFORMAR_CENTRO      = 1
      NAO_CENTRO_R_VIRTUAL = 2
      INFORMAR_CENTRO_OUT  = 3
      INFORMAR_CENTRO_V    = 4
      OTHERS               = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING SEM_PROTOCOLO.
  ENDIF.

  CONCATENATE VG_DOCNUM VG_TKNUM INTO VG_VIAGEM.

  SELECT SINGLE * INTO WA_ZCTE_VIAGEM
    FROM ZCTE_VIAGEM
   WHERE ST_CIOT   EQ '1'
     AND ID_VIAGEM EQ VG_VIAGEM
     AND ST_ULTIMO EQ 'X'.

  IF SY-SUBRC IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZCTE_VIAGEM-CD_PROTOCOLO
      IMPORTING
        OUTPUT = VG_PROTOCOLO.

    P_PROTOCOLO = WA_ZCTE_VIAGEM-CD_PROTOCOLO.

    CALL METHOD ME->CTNAB
      EXPORTING
        TAG = 'consultaViagem'.
    CALL METHOD ME->CTNAV
      EXPORTING
        TAG   = 'numeroProtocolo'
        VALOR = VG_PROTOCOLO.
    CALL METHOD ME->CTNDHN
      EXPORTING
        TAG  = 'dataRequisicao'
        DATA = SY-DATLO
        HORA = SY-TIMLO.

    CREATE OBJECT OBJ_ZCL_TIPCARD.
    TRY.

        SELECT SINGLE * INTO @DATA(WA_ZLEST0160)
          FROM ZLEST0160
         WHERE BUKRS  EQ @WA_J_1BBRANCH-BUKRS
           AND BRANCH EQ @WA_J_1BBRANCH-BRANCH.

        VAR_CHAVE = OBJ_ZCL_TIPCARD->CHAVE_SEGURANCA( I_GRUPO = WA_ZLEST0160-DS_GRUPO ).
        ME->CTNAV( TAG   = 'chave' VALOR = VAR_CHAVE ).
      CATCH ZCX_WEBSERVICE INTO CL_EXCEPTION.
        VAR_MSG = CL_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
    ENDTRY.

    CALL METHOD ME->CTNFE
      EXPORTING
        TAG = 'consultaViagem'.
  ELSE.
    MESSAGE S023 WITH ME->DOCNUM RAISING SEM_PROTOCOLO.
  ENDIF.

ENDMETHOD.


METHOD popula_ciot.

  DATA: it_ob_ciot  TYPE zcl_ciot_t,
        wa_ob_ciot  TYPE REF TO zcl_ciot,
        wa_zcte_ret TYPE zciot_ret.

*-#133089-21.02.2024-JT-inicio
  DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.
  IF i_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
    EXPORTING
      p_cte_avulso        = cte_docnum
      p_faturamento_autom = i_faturamento_autom "*-#133089-21.02.2024-JT-inicio
      p_ch_referencia     = i_ch_referencia     "*-#133089-21.02.2024-JT-inicio
    TABLES
      it_zcl_ciot         = it_ob_ciot
    EXCEPTIONS
      nao_ciot            = 1
      OTHERS              = 2.

  IF sy-subrc = 1.
    MESSAGE e008 WITH sy-msgv1 RAISING nao_ciot.
  ENDIF.

  CLEAR: me->ciot.

  me->docnum = cte_docnum.

  MOVE it_ob_ciot[] TO me->ciot[].

  READ TABLE me->ciot INTO DATA(ob_ciot) INDEX 1.
  IF sy-subrc IS INITIAL.
    ob_ciot->get_st_ciot( IMPORTING p_st_ciot = DATA(p_st_ciot) ).
    CHECK p_st_ciot NE zcl_ciot=>c_9.
  ENDIF.

  IF emitir_viagem_adm IS NOT INITIAL.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_fat_contigencia_ecc)
     WHERE name EQ 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low  EQ @sy-uname.

    IF sy-subrc EQ 0.
      MESSAGE 'Ação nao autorizada! Faturamento contingencia!' TYPE 'E'.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(lwa_lin)
     WHERE docnum EQ  @me->docnum.

    IF sy-subrc EQ 0.
      CASE lwa_lin-reftyp.
        WHEN 'BI'.
          SELECT SINGLE *
            FROM zsdt0001 INTO @DATA(lwa_zsdt0001_rom)
           WHERE fatura_frete EQ @lwa_lin-refkey(10).

          IF sy-subrc EQ 0 AND lwa_zsdt0001_rom-fat_contingencia_ecc EQ abap_true.
            MESSAGE 'Ação nao autorizada! Romaneio Faturamento contingencia ECC!' TYPE 'E'.
          ELSE.
            SELECT SINGLE *
              FROM zlest0108 INTO @DATA(lwa_zlest0108)
             WHERE fatura_frete EQ @lwa_lin-refkey(10).

            IF sy-subrc EQ 0 AND lwa_zsdt0001_rom-fat_contingencia_ecc EQ abap_true.
              MESSAGE 'Ação nao autorizada! Romaneio Faturamento contingencia ECC!' TYPE 'E'.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    CALL METHOD me->solicita_viagem_req
      IMPORTING
        p_protocolo      = p_protocolo
      EXCEPTIONS
        sem_protocolo    = 1
        erro_status      = 2
        erro_web_service = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.

      CALL METHOD me->solicita_viagem
        EXPORTING
          i_faturamento_autom = i_faturamento_autom "*-#133089-21.02.2024-JT-inicio
          i_ch_referencia     = i_ch_referencia     "*-#133089-21.02.2024-JT-inicio
        IMPORTING
          p_protocolo         = p_protocolo
        EXCEPTIONS
          erro_status         = 1
          erro_web_service    = 2
          erro_xml_solicita   = 3
          erro_valores        = 4
          OTHERS              = 5.

      IF sy-subrc = 1.
        MESSAGE e012 WITH sy-msgv1 RAISING erro_status.
      ELSEIF sy-subrc = 2.
        MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
      ELSEIF sy-subrc = 3.
        MESSAGE e023(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_xml_solicita.
      ELSEIF sy-subrc = 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_xml_solicita.
      ENDIF.

      MESSAGE s011 WITH cte_docnum.

    ENDIF.

  ELSEIF creditar_viagem_adm IS NOT INITIAL.

    CALL METHOD me->credita_viagem
      EXPORTING
        i_faturamento_autom = i_faturamento_autom "*-#133089-21.02.2024-JT-inicio
        i_ch_referencia     = i_ch_referencia     "*-#133089-21.02.2024-JT-inicio
      EXCEPTIONS
        erro_status         = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e017 WITH sy-msgv1 RAISING erro_status_cred.
        WHEN abap_true.
          MESSAGE e017 WITH sy-msgv1 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
          RAISE erro_status_cred.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

    MESSAGE s016 WITH cte_docnum.

  ELSEIF rescindir_viagem_adm IS NOT INITIAL.

    me->reason  = i_reason .
    me->reason1 = i_reason1.
    me->reason2 = i_reason2.
    me->reason3 = i_reason3.
    me->reason4 = i_reason4.

    CALL METHOD me->cancela_viagem
      EXCEPTIONS
        erro_status       = 1
        erro_web_service  = 2
        erro_cancelamento = 3
        OTHERS            = 4.

*    call method me->rescindir_viagem
*      exceptions
*        erro_status = 1
*        others      = 2.
    CASE sy-subrc.
      WHEN 1 OR 2 OR 4.
        MESSAGE e019 WITH sy-msgv1 RAISING erro_status_canc.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_solicitacao.
    ENDCASE.

    MESSAGE s020 WITH cte_docnum.

  ELSEIF consultar_status_viagem IS NOT INITIAL.

    CALL METHOD me->solicita_viagem_req
      IMPORTING
        p_protocolo      = p_protocolo
      EXCEPTIONS
        sem_protocolo    = 1
        erro_status      = 2
        erro_web_service = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_solicitacao.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD rescindir_viagem.

  DATA: ob_ciot   TYPE REF TO zcl_ciot.

  CALL METHOD me->monta_xml_rescisao.

  LOOP AT ciot INTO ob_ciot.

    CALL METHOD ob_ciot->rescindir
      EXCEPTIONS
        erro_status = 1
        OTHERS      = 2.

    IF sy-subrc = 1.
      MESSAGE e019 WITH sy-msgv1 RAISING erro_status.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD solicita_viagem.

  TYPES: BEGIN OF ty_xml_viagem.
  TYPES:   xml TYPE string.
  TYPES: END OF ty_xml_viagem.

  DATA: ob_ciot          TYPE REF TO zcl_ciot,
        wa_xml           TYPE ty_xml_viagem,
        xml_protocolo    TYPE string,
        it_xml           TYPE STANDARD TABLE OF ty_xml_viagem,
        name_file        TYPE string,
        cnpj_contratante TYPE stcd1,
        id_viagem        TYPE zidviagemsoli,
        id_contratante   TYPE tdlnr,
        ds_msg           TYPE string,
        dt_solicitacao   TYPE j_1bcredat,
        hr_solicitacao   TYPE j_1bcretim,
        dt_retorno       TYPE j_1bcredat,
        hr_retorno       TYPE j_1bcretim,
        url_end          TYPE string,
        wa_zcte_viagem   TYPE zcte_viagem,
        it_obs           TYPE TABLE OF zcte_viagem_obs,
        wa_obs           TYPE zcte_viagem_obs,
        vg_msgv1         TYPE symsgv,
        vg_msgv2         TYPE symsgv,
        vg_msgv3         TYPE symsgv,
        vg_msgv4         TYPE symsgv,
        pid_seq          TYPE z_sequencia,
        l_type           TYPE bapi_mtype. "*-#133089-21.02.2024-JT

*-#133089-21.02.2024-JT-inicio
  DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.
  IF i_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  LOOP AT ciot INTO ob_ciot.
    CALL METHOD ob_ciot->enviar
      EXCEPTIONS
        erro_status = 1
        OTHERS      = 2.
    IF sy-subrc = 1.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e012 WITH sy-msgv1 RAISING erro_status.
        WHEN abap_true.
          MESSAGE e012 WITH sy-msgv1 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'SVIA' ).
          RAISE erro_status.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDLOOP.

  CALL METHOD me->monta_xml
    IMPORTING
      cnpj    = cnpj_contratante
      cod_sap = id_contratante.

  CALL METHOD me->valida_viagem
    EXCEPTIONS
      erro_valores = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
    CASE i_faturamento_autom.
      WHEN abap_off.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_valores.
      WHEN abap_true.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'SVIA' ).
        RAISE erro_valores.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN xml_viagem WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN xml_viagem WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN xml_viagem WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN xml_viagem WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN xml_viagem WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN xml_viagem WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '&'      IN xml_viagem WITH 'E' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '§'      IN xml_viagem WITH '' IGNORING CASE.

  IF 1 = 2.
    CLEAR: it_xml.
    wa_xml-xml = xml_viagem.
    APPEND wa_xml TO it_xml.

    CONCATENATE '\\' sy-host '\INTERFACES\PF_e\PF' me->docnum '.xml' INTO name_file.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = name_file
      TABLES
        data_tab                = it_xml
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_SD_WEBSERVICE_ADM'
    EXPORTING
      xml                        = xml_viagem
    IMPORTING
      xml_protocolo              = xml_protocolo
      cd_protocolo               = p_protocolo
      id_viagem                  = id_viagem      "cnpj_contratante = cnpj_contratante
      ds_msg                     = ds_msg
      dt_solicitacao             = dt_solicitacao
      hr_solicitacao             = hr_solicitacao
      dt_retorno                 = dt_retorno
      hr_retorno                 = hr_retorno
      url_end                    = url_end
    TABLES
      it_obs                     = it_obs
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      erro_web_service           = 5
      erro_xml_solicita          = 6
      OTHERS                     = 7.

  IF ( sy-subrc EQ 0 ).
    CALL FUNCTION 'Z_SALVA_XML_PFE'
      EXPORTING
        p_xml       = xml_viagem
        p_protocolo = p_protocolo
        p_usuario   = sy-uname
        p_url       = url_end
        p_tipo      = 'SOLI'.
  ENDIF.


  IF 1 = 2.
    CLEAR: it_xml.
    wa_xml-xml = xml_protocolo.
    APPEND wa_xml TO it_xml.

    CONCATENATE '\\' sy-host '\INTERFACES\PF_e\PFResp' me->docnum '.xml' INTO name_file.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = name_file
      TABLES
        data_tab                = it_xml
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  IF ( sy-subrc IS NOT INITIAL ) AND ( sy-subrc NE 5 ) AND ( sy-subrc NE 6 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF sy-subrc EQ 5.
    MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
  ELSEIF sy-subrc EQ 6.
    LOOP AT ciot INTO ob_ciot.
      vg_msgv1 = sy-msgv1.
      vg_msgv2 = sy-msgv2.
      vg_msgv3 = sy-msgv3.
      vg_msgv4 = sy-msgv4.
      CALL METHOD ob_ciot->erro
        EXCEPTIONS
          erro_status = 1
          OTHERS      = 2.
    ENDLOOP.

*-#133089-21.02.2024-JT-inicio
    CASE i_faturamento_autom.
      WHEN abap_off.
        MESSAGE e023(zsimetrya) WITH vg_msgv1 vg_msgv2 vg_msgv3 vg_msgv4 RAISING erro_xml_solicita.
      WHEN abap_true.
        MESSAGE e023(zsimetrya) WITH vg_msgv1 vg_msgv2 vg_msgv3 vg_msgv4 INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'SVIA' ).
*-US 172686-01.04.2025-JT-inicio
        UPDATE zcte_viagem
           SET ds_msg     = l_mesg
         WHERE docnum     = me->docnum
           AND st_ciot    = '1'
           AND st_ultimo  = 'X'.
*-US 172686-01.04.2025-JT-fim
        RAISE erro_xml_solicita.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ELSE.
    wa_zcte_viagem-cd_protocolo     = p_protocolo.
    wa_zcte_viagem-docnum           = me->docnum.
    wa_zcte_viagem-cnpj_contratante = cnpj_contratante.
    wa_zcte_viagem-id_contratante   = id_contratante.
    wa_zcte_viagem-id_viagem        = id_viagem.
    wa_zcte_viagem-ds_msg           = ds_msg.
    wa_zcte_viagem-dt_solicitacao   = dt_solicitacao.
    wa_zcte_viagem-hr_solicitacao   = hr_solicitacao.
    wa_zcte_viagem-dt_retorno       = dt_retorno.
    wa_zcte_viagem-hr_retorno       = hr_retorno.
    "Enviado
    wa_zcte_viagem-st_ciot          = '1'.
    wa_zcte_viagem-st_ultimo        = 'X'.

    UPDATE zcte_viagem
       SET st_ultimo = space
     WHERE id_viagem EQ id_viagem
       AND st_ciot   EQ '1'
       AND st_ultimo EQ 'X'.

    MODIFY zcte_viagem FROM wa_zcte_viagem.

    DELETE FROM zcte_viagem_obs WHERE cd_protocolo EQ wa_zcte_viagem-cd_protocolo.

    pid_seq = 1.
    LOOP AT it_obs INTO wa_obs.
      wa_obs-id_seq = pid_seq.
      wa_obs-cd_protocolo = wa_zcte_viagem-cd_protocolo.
      MODIFY zcte_viagem_obs FROM wa_obs.
      ADD 1 TO pid_seq.

*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
        WHEN abap_true.
          l_mesg = wa_obs-ds_protocolo.
          l_type = COND #( WHEN wa_zcte_viagem-ds_msg = 'OK' THEN 'W'
                                                             ELSE 'E' ).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = l_type i_msg = CONV #( l_mesg ) i_status = 'SVIA' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDLOOP.

  ENDIF.

  LOOP AT ciot INTO ob_ciot.
    CALL METHOD ob_ciot->gravar.
  ENDLOOP.

ENDMETHOD.


METHOD SOLICITA_VIAGEM_REQ.

  TYPES: BEGIN OF TY_XML_VIAGEM.
  TYPES:   XML TYPE STRING.
  TYPES: END OF TY_XML_VIAGEM.

  DATA: WA_XML              TYPE TY_XML_VIAGEM,
        XML_PROTOCOLO       TYPE STRING,
        IT_XML              TYPE STANDARD TABLE OF TY_XML_VIAGEM,
        NAME_FILE           TYPE STRING,
        OB_CIOT             TYPE REF TO ZCL_CIOT,
        DS_MSG              TYPE STRING,
        DT_SOLICITACAO      TYPE J_1BCREDAT,
        HR_SOLICITACAO      TYPE J_1BCRETIM,
        DT_RETORNO          TYPE J_1BCREDAT,
        HR_RETORNO          TYPE J_1BCRETIM,
        ID_OP_VIAGEM_ADM    TYPE ZIDOPERACAOVIAGEM,
        P_ST_CIOT_R         TYPE ZST_CIOT,
        IT_RETORNO_CONTRATO TYPE TABLE OF ZCONTRATO_CIOT_T,
        WA_RETORNO_CONTRATO TYPE ZCONTRATO_CIOT_T,
        P_RNTRC	            TYPE ZRNTRC,
        WA_ZCTE_VIAGEM      TYPE ZCTE_VIAGEM,
        V_PROTOCOLO         TYPE ZPROTOCOLO,
        URL_END             TYPE STRING.

  CLEAR: XML_VIAGEM, DS_MSG.

  LOOP AT CIOT INTO OB_CIOT.
    CALL METHOD OB_CIOT->ENVIADO
      EXCEPTIONS
        ERRO_STATUS = 1
        OTHERS      = 2.
    IF SY-SUBRC = 1.
      MESSAGE E012 WITH SY-MSGV1 RAISING ERRO_STATUS.
    ENDIF.
  ENDLOOP.

  CALL METHOD ME->MONTA_XML_SOLICITA_REQ
    IMPORTING
      P_PROTOCOLO   = P_PROTOCOLO
    EXCEPTIONS
      SEM_PROTOCOLO = 1
      OTHERS        = 2.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 RAISING SEM_PROTOCOLO.
    WHEN 2.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.

  CLEAR: IT_XML.

  IF 1 = 2.
    WA_XML-XML = XML_VIAGEM.
    APPEND WA_XML TO IT_XML.

    CONCATENATE '\\' SY-HOST '\INTERFACES\PF_e\PFREQ' ME->DOCNUM '.xml' INTO NAME_FILE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = NAME_FILE
      TABLES
        DATA_TAB                = IT_XML
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_SD_WEBSERVICE_ADM'
    EXPORTING
      XML                        = ME->XML_VIAGEM
      P_ST_CIOT                  = '1'
    IMPORTING
      P_ST_CIOT_R                = P_ST_CIOT_R
      CD_PROTOCOLO               = V_PROTOCOLO
      XML_PROTOCOLO              = XML_PROTOCOLO
      DS_MSG                     = DS_MSG
      DT_SOLICITACAO             = DT_SOLICITACAO
      HR_SOLICITACAO             = HR_SOLICITACAO
      DT_RETORNO                 = DT_RETORNO
      HR_RETORNO                 = HR_RETORNO
      URL_END                    = URL_END
    TABLES
      IT_RETORNO_CONTRATO        = IT_RETORNO_CONTRATO
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      ERRO_WEB_SERVICE           = 5
      OTHERS                     = 6.

  IF ( SY-SUBRC EQ 0 ).
    CALL FUNCTION 'Z_SALVA_XML_PFE'
      EXPORTING
        P_XML       = XML_VIAGEM
        P_PROTOCOLO = V_PROTOCOLO
        P_USUARIO   = SY-UNAME
        P_URL       = URL_END
        p_tipo      = 'RESG'.
  ENDIF.

  IF 1 = 2.
    CLEAR: IT_XML.
    WA_XML-XML = XML_PROTOCOLO.
    APPEND WA_XML TO IT_XML.

    CONCATENATE '\\' SY-HOST '\INTERFACES\PF_e\PFREQResp' ME->DOCNUM '.xml' INTO NAME_FILE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = NAME_FILE
      TABLES
        DATA_TAB                = IT_XML
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

  IF ( SY-SUBRC IS NOT INITIAL ) AND ( SY-SUBRC NE 5 ).
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSEIF SY-SUBRC EQ 5.
    MESSAGE E032(ZSIMETRYA) WITH SY-MSGV1 RAISING ERRO_WEB_SERVICE.
  ELSE.

    IF DS_MSG IS NOT INITIAL.

      SELECT SINGLE * INTO WA_ZCTE_VIAGEM
        FROM ZCTE_VIAGEM
       WHERE CD_PROTOCOLO EQ P_PROTOCOLO.

      IF SY-SUBRC IS INITIAL.
        WA_ZCTE_VIAGEM-DS_MSG = DS_MSG.
        MODIFY ZCTE_VIAGEM FROM WA_ZCTE_VIAGEM.
      ENDIF.

    ENDIF.

    IF P_ST_CIOT_R = 1.

      LOOP AT CIOT INTO OB_CIOT.

        CALL METHOD OB_CIOT->GET_RNTRC
          IMPORTING
            P_RNTRC = P_RNTRC.

        READ TABLE IT_RETORNO_CONTRATO INTO WA_RETORNO_CONTRATO INDEX 1.
        IF SY-SUBRC IS INITIAL.
          CALL METHOD OB_CIOT->ENVIADO
            EXPORTING
              CONTRATO    = WA_RETORNO_CONTRATO
            EXCEPTIONS
              ERRO_STATUS = 1
              OTHERS      = 2.
        ENDIF.

        IF SY-SUBRC = 1.
          MESSAGE E012 WITH SY-MSGV1 RAISING ERRO_STATUS.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF P_ST_CIOT_R = 2.
      LOOP AT CIOT INTO OB_CIOT.

        CALL METHOD OB_CIOT->GET_RNTRC
          IMPORTING
            P_RNTRC = P_RNTRC.

        READ TABLE IT_RETORNO_CONTRATO INTO WA_RETORNO_CONTRATO INDEX 1.
        IF SY-SUBRC IS INITIAL.
          CALL METHOD OB_CIOT->AUTORIZAR
            EXPORTING
              P_CONTRATO  = WA_RETORNO_CONTRATO
            EXCEPTIONS
              ERRO_STATUS = 1
              OTHERS      = 2.
        ENDIF.

        IF SY-SUBRC = 1.
          MESSAGE E012 WITH SY-MSGV1 RAISING ERRO_STATUS.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF P_ST_CIOT_R = 3.

      LOOP AT CIOT INTO OB_CIOT.
        CALL METHOD OB_CIOT->ERRO
          EXCEPTIONS
            ERRO_STATUS = 1
            OTHERS      = 2.
      ENDLOOP.

      CALL FUNCTION 'ZMESSAGE_PREPARE'
        EXPORTING
          MSG_COMPLETA           = SPACE
        CHANGING
          MSG_TEXT               = DS_MSG
          MSG_VAR1               = SY-MSGV1
          MSG_VAR2               = SY-MSGV2
          MSG_VAR3               = SY-MSGV3
          MSG_VAR4               = SY-MSGV4
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.

      MESSAGE E000 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO_STATUS.

    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD VALIDA_VIAGEM.

  DATA: WA_VTTK       TYPE VTTK,
        WA_TTDS       TYPE TTDS,
        LC_BUKRS      TYPE BUKRS,
        LC_BRANCH     TYPE J_1BBRANC_,
        WA_J_1BBRANCH TYPE J_1BBRANCH,
        CONTRATADO    TYPE ZCIOT_CONTRATANTE,
        VL_MARGEM     TYPE ZLEST0103-MARGADTO,
        VL_MARGEM_CAL TYPE ZLEST0103-MARGADTO,
        INT_AUX       TYPE I.

  IF CIOT_RET-VALORES_SERVICO-VLR_ADIANTAMENTO GT 0.

    "Calcula Margem Atual Utilizada """"""""""""""""""""""""""""""""""""
    VL_MARGEM_CAL = ( CIOT_RET-VALORES_SERVICO-VLR_ADIANTAMENTO * 100 ) /
                    ( CIOT_RET-VALORES_SERVICO-VLR_FRETE +
                      CIOT_RET-VALORES_SERVICO-VLR_PIS_COFINS +
                      CIOT_RET-VALORES_SERVICO-VLR_INSS_LUCRO +
                      CIOT_RET-VALORES_SERVICO-VLR_TRIAGEM   ).
    INT_AUX       = VL_MARGEM_CAL.
    VL_MARGEM_CAL = INT_AUX.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT SINGLE * INTO WA_VTTK
      FROM VTTK
     WHERE TKNUM EQ CIOT_RET-TKNUM.

    SELECT SINGLE * INTO WA_TTDS
      FROM TTDS
     WHERE TPLST = WA_VTTK-TPLST.

    MOVE: WA_VTTK-TPLST TO LC_BRANCH,
          WA_TTDS-BUKRS TO LC_BUKRS.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        BUKRS                = LC_BUKRS
        CENTRO               = LC_BRANCH
      IMPORTING
        WA_J_1BBRANCH        = WA_J_1BBRANCH
      EXCEPTIONS
        INFORMAR_CENTRO      = 1
        NAO_CENTRO_R_VIRTUAL = 2
        INFORMAR_CENTRO_OUT  = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO_VALORES.
    ENDIF.

    READ TABLE CIOT_RET-PARCEIROS INTO CONTRATADO WITH KEY TIPO = '05'.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E034 RAISING ERRO_VALORES.
    ELSE.
      SELECT SINGLE MARGADTO
        INTO VL_MARGEM
        FROM ZLEST0103
       WHERE BUKRS  = WA_J_1BBRANCH-BUKRS
         AND BRANCH = WA_J_1BBRANCH-BRANCH
         AND TDLNR  = CONTRATADO-CODIGO.

      IF SY-SUBRC IS NOT INITIAL.
        SELECT SINGLE MARGADTO
          INTO VL_MARGEM
          FROM ZLEST0103
         WHERE BUKRS  = WA_J_1BBRANCH-BUKRS
           AND BRANCH = WA_J_1BBRANCH-BRANCH.
      ENDIF.

      IF SY-SUBRC IS INITIAL.
        IF VL_MARGEM_CAL GT VL_MARGEM.
          MESSAGE E022(ZLES) WITH VL_MARGEM RAISING ERRO_VALORES.
        ENDIF.
      ELSE.
        MESSAGE E031(ZLES) WITH WA_J_1BBRANCH-BUKRS WA_J_1BBRANCH-BRANCH RAISING ERRO_VALORES.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD ajustar_viagem.

    DATA: lv_branch      TYPE werks_d,
          w_j_1bbranch   TYPE j_1bbranch,
          lv_chave       TYPE char32,
          lv_xml_retorno TYPE string,
          lv_valor       TYPE zcte_ajustes-vlr_abon_quebra_frete,
          w_xml_retorno  TYPE zcte_ret_ajuste_viagem,
          lc_zcl_tipcard TYPE REF TO zcl_webservice_tipcard,
          lc_integra_tip TYPE REF TO zcl_integracao_tip.

    FREE: xml_viagem, w_xml_retorno, r_status_ok.

    CREATE OBJECT: lc_zcl_tipcard, lc_integra_tip.

*------------------------------------
*---OP Transporte
*------------------------------------
    SELECT SINGLE *
      INTO @DATA(_zcte_ciot)
      FROM zcte_ciot
     WHERE nucontrato = @i_nucontrato.

    IF sy-subrc <> 0.
      r_status_ok = abap_off.
      e_mensagem  = 'ZCTE_CIOT: Contrato nao Encontrado!'.
      me->atualiza_viagem_ajuste( i_nucontrato = i_nucontrato i_chvid = i_chvid i_msg_integracao = e_mensagem ).
      RETURN.
    ENDIF.

*------------------------------------
*---Ajuste viagem
*------------------------------------
    SELECT SINGLE *
      INTO @DATA(_zcte_ajustes)
      FROM zcte_ajustes
     WHERE nucontrato = @i_nucontrato
       AND chvid      = @i_chvid.

    IF sy-subrc <> 0.
      r_status_ok = abap_off.
      e_mensagem  = 'ZCTE_AJUSTES: Ajuste nao Encontrado!'.
      me->atualiza_viagem_ajuste( i_nucontrato = i_nucontrato i_chvid = i_chvid i_msg_integracao = e_mensagem ).
      RETURN.
    ENDIF.

*------------------------------------
*---Transporte
*------------------------------------
    SELECT SINGLE *
      INTO @DATA(_vttk)
      FROM vttk
     WHERE tknum = @_zcte_ciot-tknum.

    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = _vttk-tdlnr
          )->ck_parceiro_local_negocio(
          ).
      CATCH zcx_parceiros INTO DATA(ex_parceiros).
        r_status_ok = abap_off.
        MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty NUMBER ex_parceiros->msgno
              WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 INTO e_mensagem.
        RETURN.
    ENDTRY.

    MOVE: _vttk-tdlnr+6(4) TO lv_branch.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = lv_branch
      IMPORTING
        wa_j_1bbranch        = w_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      r_status_ok = abap_off.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_mensagem.
      me->atualiza_viagem_ajuste( i_nucontrato = i_nucontrato i_chvid = i_chvid i_msg_integracao = e_mensagem ).
      RETURN.
    ENDIF.

*------------------------------------
*---Parametrizacao Chave
*------------------------------------
    SELECT SINGLE *
      INTO @DATA(_zlest0160)
      FROM zlest0160
     WHERE bukrs  = @w_j_1bbranch-bukrs
       AND branch = @w_j_1bbranch-branch.

*------------------------------------
*---buacar chave acesso
*------------------------------------
    TRY.
        lv_chave = lc_zcl_tipcard->chave_seguranca( i_grupo = _zlest0160-ds_grupo ).

      CATCH zcx_webservice INTO DATA(ex_webservice).
        r_status_ok = abap_off.
        MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty NUMBER ex_parceiros->msgno
              WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 INTO e_mensagem.
        me->atualiza_viagem_ajuste( i_nucontrato = i_nucontrato i_chvid = i_chvid i_msg_integracao = e_mensagem ).
        RETURN.
    ENDTRY.

    lv_valor = _zcte_ajustes-vlr_abon_perda_merc + _zcte_ajustes-vlr_abon_quebra_frete.

*------------------------------------
*---montar XML
*------------------------------------
    me->ctnab(  EXPORTING tag = 'ajusteViagem' ).
    me->ctnav(  EXPORTING tag = 'chave'           valor = lv_chave ).
    me->ctnav(  EXPORTING tag = 'cnpjContratante' valor = _zcte_ciot-ct_cnpj ).
    me->ctnav(  EXPORTING tag = 'contrato'        valor = _zcte_ciot-nucontrato ).
    me->ctnab(  EXPORTING tag = 'ajustes' ).
    me->ctnab(  EXPORTING tag = 'ajuste' ).
    me->ctnav(  EXPORTING tag = 'tipo'            valor = _zcte_ajustes-chvid ).
    me->ctnavn( EXPORTING tag = 'valor'           valor = lv_valor precisao = 2 ).
    me->ctnav(  EXPORTING tag = 'precisao'        valor = '2' ).
    me->ctnav(  EXPORTING tag = 'observacao'      valor = _zcte_ajustes-obs_integracao ).
    me->ctnfe(  EXPORTING tag = 'ajuste' ).
    me->ctnfe(  EXPORTING tag = 'ajustes' ).
    me->ctnfe(  EXPORTING tag = 'ajusteViagem' ).

*------------------------------------
*---integrar TIP
*------------------------------------
    TRY .
        lv_xml_retorno = lc_integra_tip->set_integra_viagem( EXPORTING i_servico = 'AJUSTAR_VIAGEM' i_json = xml_viagem ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        r_status_ok = abap_off.
        MESSAGE ID ex_integra->msgid TYPE ex_integra->msgty NUMBER ex_integra->msgno
              WITH ex_integra->msgv1 ex_integra->msgv2 ex_integra->msgv3 ex_integra->msgv4 INTO e_mensagem.
        me->atualiza_viagem_ajuste( i_nucontrato = _zcte_ajustes-nucontrato i_chvid = _zcte_ajustes-chvid i_msg_integracao = e_mensagem ).
        RETURN.

      CATCH zcx_error INTO DATA(ex_error).
        r_status_ok = abap_off.
        MESSAGE ID ex_error->msgid TYPE ex_error->msgty NUMBER ex_error->msgno
              WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO e_mensagem.
        me->atualiza_viagem_ajuste( i_nucontrato = _zcte_ajustes-nucontrato i_chvid = _zcte_ajustes-chvid i_msg_integracao = e_mensagem ).
        RETURN.
    ENDTRY.

*------------------------------------
*---verificar retorno integracao
*------------------------------------
    DATA(_json) = zcl_string=>xml_to_json( i_xml    = lv_xml_retorno ).

    /ui2/cl_json=>deserialize( EXPORTING json       = _json
                               CHANGING  data       = w_xml_retorno ).

    me->atualiza_viagem_ajuste( i_nucontrato        = _zcte_ajustes-nucontrato
                                i_chvid             = _zcte_ajustes-chvid
                                i_codigo_integracao = CONV #( w_xml_retorno-ajusteviagemretorno-codigomensagem )
                                i_msg_integracao    = CONV #( w_xml_retorno-ajusteviagemretorno-mensagem ) ).

    IF w_xml_retorno-ajusteviagemretorno-codigomensagem = '0000'.
      r_status_ok = abap_true.
      e_mensagem  = w_xml_retorno-ajusteviagemretorno-mensagem.
    ELSE.
      r_status_ok = abap_false.
      e_mensagem  = w_xml_retorno-ajusteviagemretorno-mensagem.
    ENDIF.

  ENDMETHOD.


  METHOD atualiza_viagem_ajuste.

    DATA(lv_status) = COND #( WHEN i_codigo_integracao = '0000' THEN 'S'
                                                                ELSE 'E' ).

    UPDATE zcte_ajustes SET status_integracao = lv_status
                            codigo_integracao = i_codigo_integracao
                            msg_integracao    = i_msg_integracao
                      WHERE nucontrato        = i_nucontrato
                        AND chvid             = i_chvid.

  ENDMETHOD.
ENDCLASS.
