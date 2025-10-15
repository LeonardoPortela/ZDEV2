class ZCL_CIOT definition
  public
  final
  create public .

*"* public components of class ZCL_CIOT
*"* do not include other source files here!!!
public section.

  constants C_0 type ZST_CIOT value 0 ##NO_TEXT.
  constants C_1 type ZST_CIOT value 1 ##NO_TEXT.
  constants C_2 type ZST_CIOT value 2 ##NO_TEXT.
  constants C_3 type ZST_CIOT value 3 ##NO_TEXT.
  constants C_4 type ZST_CIOT value 4 ##NO_TEXT.
  constants C_5 type ZST_CIOT value 5 ##NO_TEXT.
  constants C_6 type ZST_CIOT value 6 ##NO_TEXT.
  constants C_7 type ZST_CIOT value 7 ##NO_TEXT.
  constants C_8 type ZST_CIOT value 8 ##NO_TEXT.
  constants C_9 type ZST_CIOT value 9 ##NO_TEXT.
  data REASON type J_1BNFE_CANCEL_REASON .
  data REASON1 type J_1BNFE_CANCEL_TEXT .
  data REASON2 type J_1BNFE_CANCEL_TEXT .
  data REASON3 type J_1BNFE_CANCEL_TEXT .
  data REASON4 type J_1BNFE_CANCEL_TEXT .
  data TIPO_TRANSP type ZTIPOTRANSP .
  constants ST_TP_PLANO_POS_PAGO type ZDE_TP_PLANO_ADMNISTRADORA value '01' ##NO_TEXT.
  constants ST_TP_PLANO_PRE_PAGO type ZDE_TP_PLANO_ADMNISTRADORA value '02' ##NO_TEXT.
  constants ST_TP_CARTAO_TIP type ZDE_TIPO_CARTAO value 'T' ##NO_TEXT.
  constants ST_TP_CARTAO_TIP_MARTERCARD type ZDE_TIPO_CARTAO value 'M' ##NO_TEXT.
  constants ST_TP_PLANO_POS_PAGO_TIP type ZDE_TIPO_PAGAMENTO_CONTRATO value 'O' ##NO_TEXT.
  constants ST_TP_PLANO_PRE_PAGO_TIP type ZDE_TIPO_PAGAMENTO_CONTRATO value 'P' ##NO_TEXT.
  class-data XML_VIAGEM type STRING .

  methods CONSTRUCTOR .
  methods NOVO
    importing
      !P_ZCTE_TRANS type ZCTE_TRANS
    exporting
      !P_ZCTE_IDENTIFICA type ZCTE_IDENTIFICA
    exceptions
      INF_DOCNUM
      INF_PROPVEICULO
      NAO_DOCNUM
      NAO_RTRC
      NAO_CONTA_CORRENTE
      TIPO_CARGA
      ITINERARIO
      MATERIAL_NAO_ENCONTRADO
      MATERIAL_NAO_EXPANDIDO .
  methods CLEAR .
  methods GRAVAR .
  methods CANCELAR
    importing
      !P_VALIDAR type CHAR01 optional
      !I_REASON type J_1BNFE_CANCEL_REASON optional
      !I_REASON1 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON2 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON3 type J_1BNFE_CANCEL_TEXT optional
      !I_REASON4 type J_1BNFE_CANCEL_TEXT optional
    exceptions
      ERRO_STATUS
      CANCEL_CTE .
  methods VISUALIZAR
    exporting
      !P_ZCTE_RET type ZCIOT_RET
      !P_ZCTE_CIOT type ZCTE_CIOT
      !P_ZCTE_PARCEIROS type ZCTE_CIOT_PARCE_T .
  methods RESCINDIR
    exceptions
      ERRO_STATUS .
  methods ERRO
    exceptions
      ERRO_STATUS .
  methods ENVIAR
    exceptions
      ERRO_STATUS .
  methods ENVIADO
    importing
      !CONTRATO type ZCONTRATO_CIOT_T optional
    preferred parameter CONTRATO
    exceptions
      ERRO_STATUS .
  methods CREDITAR
    importing
      !P_VALIDAR type CHAR01 optional
      !P_LINK_CONTRATO type AGR_URL optional
      !P_LINK_RESUMO type AGR_URL optional
      !P_LINK_PEDAGIO type AGR_URL optional
      !P_LINK_CARGA_PEDAGIO type AGR_URL optional
    preferred parameter P_VALIDAR
    exceptions
      ERRO_STATUS .
  methods AUTORIZAR
    importing
      !P_CONTRATO type ZCONTRATO_CIOT_T optional
    exceptions
      ERRO_STATUS .
  methods IMPRIMIR_RESUMO .
  methods IMPRIMIR_PEDAGIO .
  methods CARGA_PEDAGIO .
  methods IMPRIMIR
    importing
      !IMPRIMIR type CHAR01 default 'X'
    exporting
      !E_URL type STRING .
  methods GET_PDF_ARQUIVOS_VIAGEM
    importing
      !I_CD_CIOT type ZCIOT
      !I_TIPOARQ type STRING
      !I_IMPRIMIR type CHAR01 optional
    returning
      value(E_PDF_FILE) type ZDE_DATA_XSTRING
    exceptions
      ZCX_ERROR .
  methods SET_ZCTE_CIOT
    importing
      !P_CTE_CIOT type ZCTE_CIOT
    exporting
      !P_CIOT_RET type ZCIOT_RET .
  methods SET_DT_ORIGEM
    importing
      !P_DT_ORIGEM type ZCIOT_DT_INICIO .
  methods SET_DT_TERMINO
    importing
      !P_DT_TERMIN type ZCIOT_DT_FIM .
  methods GET_INFO_CIOT
    importing
      !PCD_CIOT type ZCIOT
      !PVISUALIZA type CHAR01 optional
    exporting
      !INF_CIOT type ZCIOT_RET
    exceptions
      NAO_ENCONTRADO .
  methods GET_NR_CIOT
    exporting
      !PNR_CIOT type ZCIOT .
  methods GET_CD_CIOT
    exporting
      !PCD_CIOT type ZCIOT .
  methods GET_LINK_CONTRATO
    exporting
      !P_LINK_CONTRATO type AGR_URL .
  methods GET_LINK_PEDAGIO
    exporting
      !P_LINK_PEDAGIO type AGR_URL .
  methods GET_LINK_CARGA_PEDAGIO
    exporting
      !P_LINK_CARGA_PEDAGIO type AGR_URL .
  methods GET_LINK_RESUMO
    exporting
      !P_LINK_RESUMO type AGR_URL .
  methods GET_ID_OP_VIAGEM_ADM
    exporting
      !PID_OP_VIAGEM_ADM type ZIDOPERACAOVIAGEM .
  methods GET_ST_CIOT
    exporting
      !P_ST_CIOT type ZST_CIOT .
  methods GET_RNTRC
    exporting
      !P_RNTRC type ZRNTRC .
  methods GET_DOCNUM
    exporting
      !P_DOCNUM type J_1BDOCNUM .
  methods GET_TKNUM
    exporting
      !P_TKNUM type TKNUM .
  methods GET_NUCONTRATO
    exporting
      !P_NUCONTRATO type ZNUCONTRATO .
  methods GET_ID_VIAGEM
    exporting
      !P_ID_VIAGEM type ZDE_VIAGEM_ID .
  methods GET_EMISSOR
    exporting
      !P_EMISSOR type TDLNR .
  methods GET_CANCELADO
    exporting
      !P_CANCELADO type J_1BCANCEL .
  methods GET_CARGA
    exporting
      !P_CARGA type ZCIOT_CARGA .
  methods GET_PARCEIROS
    exporting
      !P_PARCEIROS type ZCIOT_PARCEIROS_T .
  methods GET_MUNICIPIO_ORIGEM
    exporting
      !P_UF_ORIGEM type REGIO
      !P_MUNICIPIO_ORIGEM type CHAR07
      !P_DT_ORIGEM type ZCIOT_DT_INICIO .
  methods GET_MUNICIPIO_TERMIN
    exporting
      !P_UF_TERMIN type REGIO
      !P_MUNICIPIO_TERMIN type CHAR07
      !P_DT_TERMIN type ZCIOT_DT_FIM .
  methods GET_VEICULOS
    exporting
      !P_VEICULO type ZCIOT_VEICULOS_T .
  methods GET_VALORES_SERVICO
    exporting
      !P_VALORES_SERVICO type ZCIOT_VALORES .
  methods GET_INF_ACESSORIAS
    exporting
      !P_INF_ACESSORIAS type ZCIOT_ACESSORIAS .
  methods GET_INF_CONHEC
    exporting
      !P_MODEL type J_1BMODEL
      !P_SERIES type J_1BSERIES
      !P_NUMERO type J_1BNFNUM9 .
  methods GET_TIPO_TRANSP
    exporting
      !P_TIPO_TRANSP type ZTIPOTRANSP .
  methods SET_DT_CREDITO
    importing
      !I_DT_CREDITO type BUDAT .
  methods GET_DT_CREDITO
    exporting
      !E_DT_CREDITO type BUDAT .
  methods SET_HR_CREDITO
    importing
      !I_HR_CREDITO type UTIME .
  methods GET_HR_CREDITO
    exporting
      !E_HR_CREDITO type UTIME .
  methods SET_ID_ROTA
    importing
      !P_ID_ROTA type CHAR13 .
  methods GET_ID_ROTA
    exporting
      !E_ID_ROTA type CHAR13 .
  methods GET_TP_PLANO_ADMNISTRADORA
    exporting
      !E_TP_PLANO_ADMINISTRADORA type ZDE_TP_PLANO_ADMNISTRADORA .
  methods SET_DISTANCIA
    importing
      !I_DISTZ type DISTZ .
  methods GET_DISTANCIA
    exporting
      !E_DISTZ type DISTZ .
protected section.

*"* protected components of class ZCL_CIOT
*"* do not include other source files here!!!
  data LINK_CONTRATO type AGR_URL .
  data LINK_PEDAGIO type AGR_URL .
  data LINK_CARGA_PEDAGIO type AGR_URL .
  data LINK_RESUMO type AGR_URL .

  methods SET_RNTRC
    importing
      !P_RNTRC type ZRNTRC .
  methods SET_ST_CIOT
    importing
      !P_ST_CIOT type ZST_CIOT .
  methods SET_CD_CIOT
    importing
      !PCD_CIOT type ZCIOT .
  methods SET_ID_OP_VIAGEM_ADM
    importing
      !PID_OP_VIAGEM_ADM type ZIDOPERACAOVIAGEM .
  methods SET_NR_CIOT
    importing
      !PNR_CIOT type ZCIOT .
  methods SET_TKNUM
    importing
      !P_TKNUM type TKNUM .
  methods SET_DOCNUM
    importing
      !P_DOCNUM type J_1BDOCNUM .
  methods SET_NUCONTRATO
    importing
      !P_NUCONTRATO type ZNUCONTRATO .
  methods SET_ID_VIAGEM
    importing
      !P_ID_VIAGEM type ZDE_VIAGEM_ID .
  methods SET_EMISSOR
    importing
      !P_EMISSOR type TDLNR .
  methods SET_CANCELADO
    importing
      !P_CANCELADO type J_1BCANCEL .
  methods SET_LINK_CONTRATO
    importing
      !P_LINK_CONTRATO type AGR_URL .
  methods SET_LINK_PEDAGIO
    importing
      !P_LINK_PEDAGIO type AGR_URL .
  methods SET_LINK_CARGA_PEDAGIO
    importing
      !P_LINK_CARGA_PEDAGIO type AGR_URL .
  methods SET_LINK_RESUMO
    importing
      !P_LINK_RESUMO type AGR_URL .
  methods SET_CARGA
    importing
      !P_CARGA type ZCIOT_CARGA .
  methods SET_PARCEIROS
    importing
      !P_PARCEIROS type ZCIOT_PARCEIROS_T .
  methods SET_MUNICIPIO_ORIGEM
    importing
      !P_UF_ORIGEM type REGIO
      !P_MUNICIPIO_ORIGEM type CHAR07
      !P_DT_ORIGEM type ZCIOT_DT_INICIO .
  methods SET_MUNICIPIO_TERMIN
    importing
      !P_UF_TERMIN type REGIO
      !P_MUNICIPIO_TERMIN type CHAR07
      !P_DT_TERMIN type ZCIOT_DT_FIM .
  methods SET_VEICULOS
    importing
      !P_VEICULO type ZCIOT_VEICULOS_T .
  methods SET_VALORES_SERVICO
    importing
      !P_VALORES_SERVICO type ZCIOT_VALORES .
  methods SET_INF_ACESSORIAS
    importing
      !P_INF_ACESSORIAS type ZCIOT_ACESSORIAS .
  methods SET_TIPO_TRANSP
    importing
      !P_TIPO_TRANSP type ZTIPOTRANSP .
private section.

*"* private components of class ZCL_CIOT
*"* do not include other source files here!!!
  data CD_CIOT type ZCIOT .
  data ID_OP_VIAGEM_ADM type ZIDOPERACAOVIAGEM .
  data NR_CIOT type ZCIOT .
  data ST_CIOT type ZST_CIOT .
  data RNTRC type ZRNTRC .
  data NUCONTRATO type ZNUCONTRATO .
  data ID_VIAGEM type ZDE_VIAGEM_ID .
  data DOCNUM type J_1BDOCNUM .
  data TKNUM type TKNUM .
  data EMISSOR type TDLNR .
  data CANCELADO type J_1BCANCEL .
  data PARCEIROS type ZCIOT_PARCEIROS_T .
  data UF_ORIGEM type REGIO .
  data MUNICIPIO_ORIGEM type ZMUNIC_IBGE .
  data DT_ORIGEM type ZCIOT_DT_INICIO .
  data UF_TERMIN type REGIO .
  data MUNICIPIO_TERMIN type ZMUNIC_IBGE .
  data DT_TERMIN type ZCIOT_DT_FIM .
  data CARGA type ZCIOT_CARGA .
  data VEICULOS type ZCIOT_VEICULOS_T .
  data VALORES_SERVICO type ZCIOT_VALORES .
  data INF_ACESSORIAS type ZCIOT_ACESSORIAS .
  data TP_PLANO_ADMINISTRADORA type ZDE_TP_PLANO_ADMNISTRADORA .
  data AT_DT_CREDITO type BUDAT .
  data AT_HR_CREDITO type UTIME .
  data AT_ID_ROTA type CHAR13 .
  data DISTANCIA type DISTZ .

  methods SET_CTNAB
    importing
      !I_TAG type CLIKE .
  methods SET_CTNAV
    importing
      !I_TAG type CLIKE
      !I_VALOR type CLIKE .
  methods SET_CTNAF
    importing
      !I_TAG type CLIKE .
  methods SET_TP_PLANO_ADMINISTRADORA
    importing
      !I_TP_PLANO_ADMINISTRADORA type ZDE_TP_PLANO_ADMNISTRADORA .
ENDCLASS.



CLASS ZCL_CIOT IMPLEMENTATION.


  METHOD AUTORIZAR.

*0  Pendente
*1  Enviado
*2  Autorizado a Emissão CT-e
*3  Rejeitado
*4  Creditado
*5  Fechado (Pago Cockpit)

    IF ( ME->ST_CIOT EQ C_1 ).

      IF P_CONTRATO-NUMEROCIOT IS NOT INITIAL.
        ME->SET_NR_CIOT( PNR_CIOT = P_CONTRATO-NUMEROCIOT ).
      ENDIF.

      ME->SET_ID_OP_VIAGEM_ADM( PID_OP_VIAGEM_ADM = P_CONTRATO-ID_OP_VIAGEM_ADM ).
      ME->SET_NUCONTRATO( P_NUCONTRATO = P_CONTRATO-NUCONTRATO ).
      ME->SET_TIPO_TRANSP( P_TIPO_TRANSP = P_CONTRATO-TIPOCONTRATADO ).

      CASE P_CONTRATO-TIPOPAGAMENTOCONTRATO.
        WHEN ZCL_CIOT=>ST_TP_PLANO_POS_PAGO_TIP.
          "Pós-Pago
          ME->SET_TP_PLANO_ADMINISTRADORA( I_TP_PLANO_ADMINISTRADORA = ZCL_CIOT=>ST_TP_PLANO_POS_PAGO ).
        WHEN ZCL_CIOT=>ST_TP_PLANO_PRE_PAGO_TIP.
          "Pré-Pago
          ME->SET_TP_PLANO_ADMINISTRADORA( I_TP_PLANO_ADMINISTRADORA = ZCL_CIOT=>ST_TP_PLANO_PRE_PAGO ).
      ENDCASE.

      IF ( ( P_CONTRATO-TIPOCONTRATADO EQ 'T' OR P_CONTRATO-TIPOCONTRATADO EQ 'E' OR P_CONTRATO-TIPOCONTRATADO EQ 'C' ) AND
           ( P_CONTRATO-NUMEROCIOT IS NOT INITIAL ) ) OR
         ( P_CONTRATO-TIPOCONTRATADO EQ 'N' ).

        IF P_CONTRATO-NUMEROCIOT IS NOT INITIAL.
          MESSAGE S030 WITH P_CONTRATO-NUCONTRATO P_CONTRATO-NUMEROCIOT.
        ELSE.
          MESSAGE S029 WITH P_CONTRATO-NUCONTRATO.
        ENDIF.

        ME->SET_ST_CIOT( P_ST_CIOT = C_2 ).

      ELSE.
        IF NOT P_CONTRATO-NUCONTRATO IS INITIAL.
          MESSAGE S031 WITH P_CONTRATO-NUCONTRATO.
        ENDIF.
      ENDIF.

      "029  Contrato gerado &1 via Administradora!
      "030  Contrato gerado &1 com CIOT &2 via Administradora!

      CALL METHOD ME->GRAVAR.

    ELSEIF ME->ST_CIOT EQ '9'.
      MESSAGE E039 RAISING ERRO_STATUS.

    ELSE.
      MESSAGE E013 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD CANCELAR.

    DATA: P_NR_CIOT	         TYPE ZCIOT,
          P_CANCELADO        TYPE J_1BCANCEL,
          WA_DOCNUM          TYPE J_1BDOCNUM,
          WA_ZCTE_IDENTIFICA TYPE ZCTE_IDENTIFICA.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    IF ( ME->ST_CIOT EQ C_1 ) OR ( ME->ST_CIOT EQ C_2 ) OR ( ME->ST_CIOT EQ C_3 ) OR ( ME->ST_CIOT EQ C_5 ).

      CALL METHOD ME->GET_DOCNUM
        IMPORTING
          P_DOCNUM = WA_DOCNUM.

      IF P_VALIDAR IS INITIAL.

        P_CANCELADO = 'X'.

        CALL METHOD ME->SET_CANCELADO
          EXPORTING
            P_CANCELADO = P_CANCELADO.

        CALL METHOD ME->SET_ST_CIOT
          EXPORTING
            P_ST_CIOT = C_8.

        SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
          FROM ZCTE_IDENTIFICA
         WHERE DOCNUM EQ WA_DOCNUM.

        WA_ZCTE_IDENTIFICA-REASON  = ME->REASON.
        WA_ZCTE_IDENTIFICA-REASON1 = ME->REASON1.
        WA_ZCTE_IDENTIFICA-REASON2 = ME->REASON2.
        WA_ZCTE_IDENTIFICA-REASON3 = ME->REASON3.
        WA_ZCTE_IDENTIFICA-REASON4 = ME->REASON4.
        MODIFY ZCTE_IDENTIFICA FROM WA_ZCTE_IDENTIFICA.
        CALL METHOD ME->GRAVAR.

        MESSAGE S018 WITH ME->CD_CIOT.

      ELSE.
        ""Método informar o motivo do cancelamento

        "Cancelar primeiro a Dacte
        SELECT SINGLE *
          FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE)
         WHERE DOCNUM EQ @WA_DOCNUM.

        IF SY-SUBRC EQ 0.
          IF ( _WL_ACTIVE-DOCSTA EQ '1' ) AND
             ( _WL_ACTIVE-SCSSTA NE '2' ) AND
             ( _WL_ACTIVE-CANCEL IS INITIAL ). "2 = Autorizado o Cancelamento
            MESSAGE E038(ZCIOT) RAISING CANCEL_CTE.
          ENDIF.

          "IF ( _WL_ACTIVE-SCSSTA = '0' ) OR ( _WL_ACTIVE-SCSSTA = '3' ).
          "  MESSAGE E042(ZCIOT) RAISING CANCEL_CTE.
          "ENDIF.

          SELECT SINGLE *
            FROM J_1BNFDOC INTO @DATA(_WL_DOC)
           WHERE DOCNUM EQ @WA_DOCNUM.

          IF SY-SUBRC EQ 0.
            IF ( _WL_ACTIVE-DOCSTA NE '1' ) AND ( _WL_ACTIVE-NFNUM9 IS NOT INITIAL ) AND _WL_DOC-xmlvers <= 3.
              CASE _WL_ACTIVE-TPEMIS.
                WHEN '1'. "Emissão em contingencia
                  IF ( _WL_ACTIVE-SCSSTA NE '4' AND _WL_ACTIVE-SCSSTA NE 'A'  ) OR ( _WL_ACTIVE-ACTION_REQU IS INITIAL ).
                    "Não autorizado, e não foi inutilizado
                    MESSAGE E043(ZCIOT) RAISING CANCEL_CTE.
                  ENDIF.
                WHEN OTHERS.
                  SELECT SINGLE *
                    FROM ZSDT0254 INTO @DATA(WL_0254)
                   WHERE DOCNUM EQ @_WL_ACTIVE-DOCNUM.
                  IF SY-SUBRC NE 0.
                    "Não autorizado, e não foi gravado na tabela de documentos para inutilização futura
                    MESSAGE E043(ZCIOT) RAISING CANCEL_CTE.
                  ENDIF.
              ENDCASE.
            ENDIF.
          ENDIF.

        ENDIF.

        SELECT SINGLE * INTO WA_ZCTE_IDENTIFICA
          FROM ZCTE_IDENTIFICA
         WHERE DOCNUM EQ WA_DOCNUM.

        ME->REASON   = WA_ZCTE_IDENTIFICA-REASON.
        ME->REASON1  = WA_ZCTE_IDENTIFICA-REASON1.
        ME->REASON2  = WA_ZCTE_IDENTIFICA-REASON2.
        ME->REASON3  = WA_ZCTE_IDENTIFICA-REASON3.
        ME->REASON4  = WA_ZCTE_IDENTIFICA-REASON4.

        IF ( WA_ZCTE_IDENTIFICA-REASON  IS INITIAL ) AND
           ( WA_ZCTE_IDENTIFICA-REASON1 IS INITIAL ) AND
           ( WA_ZCTE_IDENTIFICA-REASON2 IS INITIAL ) AND
           ( WA_ZCTE_IDENTIFICA-REASON3 IS INITIAL ) AND
           ( WA_ZCTE_IDENTIFICA-REASON4 IS INITIAL ).

          CALL FUNCTION 'Z_SD_INFO_CTE_CANCELAR'
            EXPORTING
              P_DOCNUM  = WA_DOCNUM
              I_REASON  = I_REASON
              I_REASON1 = I_REASON1
              I_REASON2 = I_REASON2
              I_REASON3 = I_REASON3
              I_REASON4 = I_REASON4
            IMPORTING
              REASON    = ME->REASON
              REASON1   = ME->REASON1
              REASON2   = ME->REASON2
              REASON3   = ME->REASON3
              REASON4   = ME->REASON4
            EXCEPTIONS
              ERROR     = 1
              OTHERS    = 2.

          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO_STATUS.
          ENDIF.

        ENDIF.

      ENDIF.
    ELSEIF ME->ST_CIOT EQ '9'.
      MESSAGE E039 RAISING ERRO_STATUS.

    ELSE.
      MESSAGE E012 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD CARGA_PEDAGIO.

    DATA: WA_ZCTE_CIOT_IMP TYPE ZCTE_CIOT_IMP,
          LC_AGR_URL       TYPE AGR_URL.

    IF ME->LINK_CARGA_PEDAGIO IS NOT INITIAL.

      MOVE ME->LINK_CARGA_PEDAGIO TO LC_AGR_URL.

      CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
        EXPORTING
          NODE_DATA = LC_AGR_URL.

    ENDIF.

  ENDMETHOD.


  METHOD CLEAR.

    CLEAR: CD_CIOT,
           ST_CIOT,
           ID_OP_VIAGEM_ADM,
           PARCEIROS,
           CARGA,
           INF_ACESSORIAS,
           MUNICIPIO_ORIGEM,
           MUNICIPIO_TERMIN,
           NR_CIOT,
           RNTRC,
           NUCONTRATO,
           VALORES_SERVICO,
           VEICULOS.

  ENDMETHOD.


  METHOD CONSTRUCTOR.
    CALL METHOD ME->CLEAR.
  ENDMETHOD.


  METHOD CREDITAR.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    IF ( ME->ST_CIOT EQ C_2 OR  ME->ST_CIOT EQ C_8 ).

      IF P_VALIDAR IS INITIAL.
        ME->SET_ST_CIOT( EXPORTING P_ST_CIOT = C_5 ).
        ME->SET_LINK_CONTRATO( EXPORTING P_LINK_CONTRATO = P_LINK_CONTRATO ).
        ME->SET_LINK_RESUMO( EXPORTING P_LINK_RESUMO = P_LINK_RESUMO ).
        ME->SET_LINK_PEDAGIO( EXPORTING P_LINK_PEDAGIO = P_LINK_PEDAGIO ).
        ME->SET_LINK_CARGA_PEDAGIO( EXPORTING P_LINK_CARGA_PEDAGIO = P_LINK_CARGA_PEDAGIO ).
        ME->SET_DT_CREDITO( EXPORTING I_DT_CREDITO = SY-DATUM ).
        ME->SET_HR_CREDITO( EXPORTING I_HR_CREDITO = SY-UZEIT ).
        MESSAGE S018 WITH ME->CD_CIOT.
        CALL METHOD ME->GRAVAR.
      ENDIF.

    ELSEIF ME->ST_CIOT EQ '9'.
      MESSAGE E039 RAISING ERRO_STATUS.

    ELSE.
      MESSAGE E012 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD ENVIADO.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    IF ( ME->ST_CIOT EQ C_1 ).

      IF NOT CONTRATO-NUCONTRATO IS INITIAL.

        CALL METHOD ME->SET_ST_CIOT
          EXPORTING
            P_ST_CIOT = C_1.

        CALL METHOD ME->SET_ID_OP_VIAGEM_ADM
          EXPORTING
            PID_OP_VIAGEM_ADM = CONTRATO-ID_OP_VIAGEM_ADM.

        CALL METHOD ME->SET_NUCONTRATO
          EXPORTING
            P_NUCONTRATO = CONTRATO-NUCONTRATO.

        MESSAGE S018 WITH ME->CD_CIOT.
        CALL METHOD ME->GRAVAR.

      ENDIF.

    ELSE.
      MESSAGE E012 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD ENVIAR.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado
*9  Não Gera Contrato Administradora (Frete c/ Veículo Próprio)

    IF ( ME->ST_CIOT EQ C_0 ) OR ( ME->ST_CIOT EQ C_3 ) OR ( ME->ST_CIOT EQ C_1 AND ME->NUCONTRATO IS INITIAL ).

      CALL METHOD ME->SET_ST_CIOT
        EXPORTING
          P_ST_CIOT = C_1.

      MESSAGE S010 WITH ME->CD_CIOT.

    ELSEIF ME->ST_CIOT EQ '9'.
      MESSAGE E039 RAISING ERRO_STATUS.
    ELSE.
      MESSAGE E012 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD ERRO.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    IF ( ME->ST_CIOT EQ C_0 ) OR ( ME->ST_CIOT EQ C_1 ).

      CALL METHOD ME->SET_ST_CIOT
        EXPORTING
          P_ST_CIOT = C_3.

      CALL METHOD ME->GRAVAR.

      MESSAGE S025 WITH ME->CD_CIOT.

    ELSE.
      MESSAGE E012 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CANCELADO.
    P_CANCELADO = ME->CANCELADO.
  ENDMETHOD.


  METHOD GET_CARGA.
    P_CARGA = ME->CARGA.
  ENDMETHOD.


  METHOD GET_CD_CIOT.
    PCD_CIOT = ME->CD_CIOT.
  ENDMETHOD.


  METHOD GET_DISTANCIA.
    E_DISTZ = ME->DISTANCIA.
  ENDMETHOD.


  METHOD GET_DOCNUM.
    P_DOCNUM = ME->DOCNUM.
  ENDMETHOD.


  METHOD GET_DT_CREDITO.
    E_DT_CREDITO = ME->AT_DT_CREDITO.
  ENDMETHOD.


  METHOD GET_EMISSOR.
    P_EMISSOR = ME->EMISSOR.
  ENDMETHOD.


  METHOD GET_HR_CREDITO.
    E_HR_CREDITO = ME->AT_HR_CREDITO.
  ENDMETHOD.


  METHOD GET_ID_OP_VIAGEM_ADM.
    PID_OP_VIAGEM_ADM = ME->ID_OP_VIAGEM_ADM.
  ENDMETHOD.


  METHOD GET_ID_ROTA.

    E_ID_ROTA = ME->AT_ID_ROTA.

  ENDMETHOD.


  METHOD get_info_ciot.

    DATA: wa_zcte_ciot TYPE zcte_ciot,
          it_notas     TYPE TABLE OF zcte_info_nota,
          wa_act_nota  TYPE j_1bnfe_active,
          it_act_nota  TYPE TABLE OF j_1bnfe_active,
          wa_notas     TYPE zcte_info_nota,
          it_trans     TYPE TABLE OF zcte_trans,
          wa_trans     TYPE zcte_trans,
          wa_vttk      TYPE vttk,       "*#127471-18.04.2024-JT
          wa_parceiros TYPE zciot_contratante,
          wa_makt      TYPE makt.

    DATA: vg_matnr TYPE char18.

    DATA: vl_chave     TYPE zcte_info_nota-chave.

    CLEAR: inf_ciot, wa_vttk.  "*#127471-18.04.2024-JT

    SELECT SINGLE * INTO wa_zcte_ciot
      FROM zcte_ciot
     WHERE cd_ciot EQ pcd_ciot.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e001 WITH pcd_ciot RAISING nao_encontrado.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_parceiros)
      FROM zcte_ciot_parce
     WHERE cd_ciot EQ @pcd_ciot.

    SELECT * INTO TABLE it_notas
      FROM zcte_info_nota
     WHERE docnum EQ wa_zcte_ciot-docnum.

*-#127471-18.04.2024-JT-inicio
    SELECT SINGLE *
      INTO wa_vttk
      FROM vttk
     WHERE tknum EQ wa_zcte_ciot-tknum.
*-#127471-18.04.2024-JT-fim

*  select single * into wa_act_nota
*    from j_1bnfe_active
*   where docnum eq wa_nota_cte-DOCNUM_NF.

    SELECT * INTO TABLE it_trans
      FROM zcte_trans
     WHERE docnum EQ wa_zcte_ciot-docnum.

    "Informações CIOT
    inf_ciot-cd_ciot                    = wa_zcte_ciot-cd_ciot.
    inf_ciot-nr_ciot                    = wa_zcte_ciot-nr_ciot.
    inf_ciot-rntrc                      = wa_zcte_ciot-rntrc.
    inf_ciot-st_ciot                    = wa_zcte_ciot-st_ciot.
    inf_ciot-uf_origem                  = wa_zcte_ciot-uf_origem.
    inf_ciot-municipio_origem           = wa_zcte_ciot-municipio_origem.
    inf_ciot-dt_origem                  = wa_zcte_ciot-dt_origem.
    inf_ciot-uf_termin                  = wa_zcte_ciot-uf_termin.
    inf_ciot-municipio_termin           = wa_zcte_ciot-municipio_termin.
    inf_ciot-dt_termin                  = wa_zcte_ciot-dt_termin.
    inf_ciot-docnum                     = wa_zcte_ciot-docnum.
    inf_ciot-tknum                      = wa_zcte_ciot-tknum.
    inf_ciot-emissor                    = wa_zcte_ciot-emissor.
    inf_ciot-cancelado                  = wa_zcte_ciot-cancelado.
    inf_ciot-id_op_viagem_adm           = wa_zcte_ciot-id_op_viagem_adm.
    inf_ciot-nucontrato                 = wa_zcte_ciot-nucontrato.
    inf_ciot-link_contrato              = wa_zcte_ciot-link_contrato.
    inf_ciot-link_resumo                = wa_zcte_ciot-link_resumo.
    inf_ciot-link_pedagio               = wa_zcte_ciot-link_pedagio.
    inf_ciot-link_carga_pedagio         = wa_zcte_ciot-link_carga_pedagio.
    inf_ciot-tipo_transp                = wa_zcte_ciot-tipo_transp.
    inf_ciot-id_rota                    = wa_zcte_ciot-id_rota.
    inf_ciot-tp_plano_administradora    = wa_zcte_ciot-tp_plano_administradora.
    inf_ciot-distancia                  = wa_zcte_ciot-distancia.
    inf_ciot-id_viagem                  = wa_vttk-id_viagem.   "*-#127471-18.04.2024-JT

    "Contratante
    IF wa_zcte_ciot-ct_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo           = '00'.
      wa_parceiros-codigo         = wa_zcte_ciot-ct_codigo.

      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome           = wa_zcte_ciot-ct_nome.
        wa_parceiros-razao          = wa_zcte_ciot-ct_razao.

      ELSE.
        wa_parceiros-nome           = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao          = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
      ENDIF.

      wa_parceiros-cnpj           = wa_zcte_ciot-ct_cnpj.
      wa_parceiros-cpf            = wa_zcte_ciot-ct_cpf.
      wa_parceiros-logradouro     = wa_zcte_ciot-ct_logradouro.
      wa_parceiros-numero         = wa_zcte_ciot-ct_numero.
      wa_parceiros-complemento    = wa_zcte_ciot-ct_complemento.
      wa_parceiros-bairro         = wa_zcte_ciot-ct_bairro.
      wa_parceiros-uf             = wa_zcte_ciot-ct_uf.
      wa_parceiros-municipio      = wa_zcte_ciot-ct_municipio.
      wa_parceiros-cep            = wa_zcte_ciot-ct_cep.
      wa_parceiros-fone           = wa_zcte_ciot-ct_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Remetente
    IF wa_zcte_ciot-rm_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo           = '01'.
      wa_parceiros-codigo         = wa_zcte_ciot-rm_codigo.

      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome           = wa_zcte_ciot-rm_nome.
        wa_parceiros-razao          = wa_zcte_ciot-rm_razao.

      ELSE.
        wa_parceiros-nome         = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao        = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
      ENDIF.

      wa_parceiros-cnpj           = wa_zcte_ciot-rm_cnpj.
      wa_parceiros-cpf            = wa_zcte_ciot-rm_cpf.
      wa_parceiros-logradouro     = wa_zcte_ciot-rm_logradouro.
      wa_parceiros-numero         = wa_zcte_ciot-rm_numero.
      wa_parceiros-complemento    = wa_zcte_ciot-rm_complemento.
      wa_parceiros-bairro         = wa_zcte_ciot-rm_bairro.
      wa_parceiros-uf             = wa_zcte_ciot-rm_uf.
      wa_parceiros-municipio      = wa_zcte_ciot-rm_municipio.
      wa_parceiros-cep            = wa_zcte_ciot-rm_cep.
      wa_parceiros-fone           = wa_zcte_ciot-rm_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Destinatário
    IF wa_zcte_ciot-dt_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo          = '02'.
      wa_parceiros-codigo        = wa_zcte_ciot-dt_codigo.

      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome          = wa_zcte_ciot-dt_nome.
        wa_parceiros-razao         = wa_zcte_ciot-dt_razao.

      ELSE.
        wa_parceiros-nome          = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao         = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.


      ENDIF.

      wa_parceiros-cnpj          = wa_zcte_ciot-dt_cnpj.
      wa_parceiros-cpf           = wa_zcte_ciot-dt_cpf.
      wa_parceiros-logradouro    = wa_zcte_ciot-dt_logradouro.
      wa_parceiros-numero        = wa_zcte_ciot-dt_numero.
      wa_parceiros-complemento   = wa_zcte_ciot-dt_complemento.
      wa_parceiros-bairro        = wa_zcte_ciot-dt_bairro.
      wa_parceiros-uf            = wa_zcte_ciot-dt_uf.
      wa_parceiros-municipio     = wa_zcte_ciot-dt_municipio.
      wa_parceiros-cep           = wa_zcte_ciot-dt_cep.
      wa_parceiros-fone          = wa_zcte_ciot-dt_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Subcontratante
    IF wa_zcte_ciot-sb_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo        = '03'.
      wa_parceiros-codigo      = wa_zcte_ciot-sb_codigo.
      wa_parceiros-nome        = wa_zcte_ciot-sb_nome.
      wa_parceiros-razao       = wa_zcte_ciot-sb_razao.
      wa_parceiros-cnpj        = wa_zcte_ciot-sb_cnpj.
      wa_parceiros-cpf         = wa_zcte_ciot-sb_cpf.
      wa_parceiros-logradouro  = wa_zcte_ciot-sb_logradouro.
      wa_parceiros-numero      = wa_zcte_ciot-sb_numero.
      wa_parceiros-bairro      = wa_zcte_ciot-sb_bairro.
      wa_parceiros-uf          = wa_zcte_ciot-sb_uf.
      wa_parceiros-municipio   = wa_zcte_ciot-sb_municipio.
      wa_parceiros-cep         = wa_zcte_ciot-sb_cep.
      wa_parceiros-fone        = wa_zcte_ciot-sb_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Consignatário
    IF wa_zcte_ciot-cs_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo         = '04'.
      wa_parceiros-codigo       = wa_zcte_ciot-cs_codigo.
      wa_parceiros-nome         = wa_zcte_ciot-cs_nome.
      wa_parceiros-razao        = wa_zcte_ciot-cs_razao.
      wa_parceiros-cnpj         = wa_zcte_ciot-cs_cnpj.
      wa_parceiros-cpf          = wa_zcte_ciot-cs_cpf.
      wa_parceiros-logradouro   = wa_zcte_ciot-cs_logradouro.
      wa_parceiros-numero       = wa_zcte_ciot-cs_numero.
      wa_parceiros-complemento  = wa_zcte_ciot-cs_complemento.
      wa_parceiros-bairro       = wa_zcte_ciot-cs_bairro.
      wa_parceiros-uf           = wa_zcte_ciot-cs_uf.
      wa_parceiros-municipio    = wa_zcte_ciot-cs_municipio.
      wa_parceiros-cep          = wa_zcte_ciot-cs_cep.
      wa_parceiros-fone         = wa_zcte_ciot-cs_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    IF wa_zcte_ciot-mt_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo          = '05'.
      wa_parceiros-codigo        = wa_zcte_ciot-mt_codigo.
      wa_parceiros-nome          = wa_zcte_ciot-mt_nome.
      wa_parceiros-razao         = wa_zcte_ciot-mt_razao.
      wa_parceiros-cpf           = wa_zcte_ciot-mt_cpf.
      wa_parceiros-logradouro    = wa_zcte_ciot-mt_logradouro.
      wa_parceiros-numero        = wa_zcte_ciot-mt_numero.
      wa_parceiros-complemento   = wa_zcte_ciot-mt_complemento.
      wa_parceiros-bairro        = wa_zcte_ciot-mt_bairro.
      wa_parceiros-uf            = wa_zcte_ciot-mt_uf.
      wa_parceiros-municipio     = wa_zcte_ciot-mt_municipio.
      wa_parceiros-cep           = wa_zcte_ciot-mt_cep.
      wa_parceiros-rg            = wa_zcte_ciot-mt_rg.
      wa_parceiros-cnh           = wa_zcte_ciot-mt_cnh.
      wa_parceiros-pis           = wa_zcte_ciot-mt_pis.
      wa_parceiros-uf_rg         = wa_zcte_ciot-mt_uf_rg.
      wa_parceiros-uf_cnh        = wa_zcte_ciot-mt_uf_cnh.
      wa_parceiros-fone          = wa_zcte_ciot-mt_fone.
      wa_parceiros-nomemae       = wa_zcte_ciot-mt_nomemae.
      wa_parceiros-sexo          = wa_zcte_ciot-mt_sexo.
      wa_parceiros-dt_nascimento = wa_zcte_ciot-mt_dt_nascimento.
      wa_parceiros-org_rg        = wa_zcte_ciot-mt_org_rg.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Contratado
    IF wa_zcte_ciot-tr_nome IS NOT INITIAL.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo         = '99'.
      wa_parceiros-codigo       = wa_zcte_ciot-tr_codigo.

      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome         = wa_zcte_ciot-tr_nome.
        wa_parceiros-razao        = wa_zcte_ciot-tr_razao.

      ELSE.
        wa_parceiros-nome         = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao        = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.

      ENDIF.

      wa_parceiros-cnpj         = wa_zcte_ciot-tr_cnpj.
      wa_parceiros-cpf          = wa_zcte_ciot-tr_cpf.
      wa_parceiros-logradouro   = wa_zcte_ciot-tr_logradouro.
      wa_parceiros-numero       = wa_zcte_ciot-tr_numero.
      wa_parceiros-complemento  = wa_zcte_ciot-tr_complemento.
      wa_parceiros-bairro       = wa_zcte_ciot-tr_bairro.
      wa_parceiros-uf           = wa_zcte_ciot-tr_uf.
      wa_parceiros-municipio    = wa_zcte_ciot-tr_municipio.
      wa_parceiros-cep          = wa_zcte_ciot-tr_cep.
      wa_parceiros-fone         = wa_zcte_ciot-tr_fone.
      IF wa_parceiros IS NOT INITIAL.
        APPEND wa_parceiros TO inf_ciot-parceiros.
      ENDIF.
    ENDIF.

    "Local de Coleta
    LOOP AT it_parceiros INTO DATA(wa_parceiro) WHERE tipo EQ '06'.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo           = wa_parceiro-tipo.
      wa_parceiros-codigo         = wa_parceiro-codigo.
      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome         = wa_parceiro-nome.
        wa_parceiros-razao        = wa_parceiro-razao.
      ELSE.
        wa_parceiros-nome         = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao        = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
      ENDIF.
      wa_parceiros-cnpj           = wa_parceiro-cnpj.
      wa_parceiros-cpf            = wa_parceiro-cpf.
      wa_parceiros-logradouro     = wa_parceiro-logradouro.
      wa_parceiros-numero         = wa_parceiro-numero.
      wa_parceiros-complemento    = wa_parceiro-complemento.
      wa_parceiros-bairro         = wa_parceiro-bairro.
      wa_parceiros-uf             = wa_parceiro-uf.
      wa_parceiros-municipio      = wa_parceiro-municipio.
      wa_parceiros-cep            = wa_parceiro-cep.
      wa_parceiros-fone           = wa_parceiro-fone.
      APPEND wa_parceiros TO inf_ciot-parceiros.
    ENDLOOP.

    "Local de Entrega
    LOOP AT it_parceiros INTO wa_parceiro WHERE tipo EQ '07'.
      CLEAR: wa_parceiros.
      wa_parceiros-tipo           = wa_parceiro-tipo.
      wa_parceiros-codigo         = wa_parceiro-codigo.
      IF ( sy-mandt EQ 300 ).
        wa_parceiros-nome         = wa_parceiro-nome.
        wa_parceiros-razao        = wa_parceiro-razao.
      ELSE.
        wa_parceiros-nome         = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
        wa_parceiros-razao        = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO – SEM VALOR FISCAL'.
      ENDIF.
      wa_parceiros-cnpj           = wa_parceiro-cnpj.
      wa_parceiros-cpf            = wa_parceiro-cpf.
      wa_parceiros-logradouro     = wa_parceiro-logradouro.
      wa_parceiros-numero         = wa_parceiro-numero.
      wa_parceiros-complemento    = wa_parceiro-complemento.
      wa_parceiros-bairro         = wa_parceiro-bairro.
      wa_parceiros-uf             = wa_parceiro-uf.
      wa_parceiros-municipio      = wa_parceiro-municipio.
      wa_parceiros-cep            = wa_parceiro-cep.
      wa_parceiros-fone           = wa_parceiro-fone.
      APPEND wa_parceiros TO inf_ciot-parceiros.
    ENDLOOP.

    "Informações da Carga
    inf_ciot-carga-natureza             = wa_zcte_ciot-natureza.
    inf_ciot-carga-quantidade           = wa_zcte_ciot-quantidade.
    inf_ciot-carga-unidade              = wa_zcte_ciot-unidade.

    IF it_notas[] IS NOT INITIAL.
      SELECT  *
        INTO TABLE @it_act_nota
        FROM j_1bnfe_active
        FOR ALL ENTRIES IN @it_notas
       WHERE docnum EQ @it_notas-docnum_nf
         AND docnum NE @space .
    ENDIF.

    SORT it_act_nota BY docnum.

    LOOP AT it_notas INTO wa_notas.

      "Adiciona Descrição do Produto - Material
      CLEAR: vg_matnr.
      vg_matnr = |{ wa_notas-material ALPHA = OUT }|.
      vg_matnr = |{ vg_matnr ALPHA = IN }|.
      wa_notas-material = vg_matnr.

      SELECT SINGLE * FROM makt INTO wa_makt WHERE spras = sy-langu AND matnr EQ wa_notas-material.
      IF ( sy-subrc EQ 0 ).
        inf_ciot-carga-descmatnr = wa_makt-maktx.
      ENDIF.

      "Busco a Chave na Tabela J_1BNFE_ACTIVE para garantir que a chave esta correta.


      READ TABLE it_act_nota INTO wa_act_nota WITH KEY docnum = wa_notas-docnum_nf BINARY SEARCH.

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

        IF vl_chave <> wa_notas-chave.
          wa_notas-chave   = vl_chave.
          wa_notas-docnum9 = wa_act_nota-docnum9 .
          MODIFY zcte_info_nota FROM wa_notas."transporting chave docnum9.
          COMMIT WORK.
        ENDIF.

      ENDIF.

      APPEND wa_notas TO inf_ciot-carga-notas.
    ENDLOOP.

    "Informações veiculo
    LOOP AT it_trans INTO wa_trans.
      APPEND wa_trans TO inf_ciot-veiculos.
    ENDLOOP.

    "Informações Valores de Serviço
    inf_ciot-valores_servico-vlr_combustivel  = wa_zcte_ciot-vlr_combustivel.
    inf_ciot-valores_servico-vlr_pedagio      = wa_zcte_ciot-vlr_pedagio.

    inf_ciot-valores_servico-tp_card_ped      = wa_zcte_ciot-tp_card_ped.
    inf_ciot-valores_servico-nr_card_ped      = wa_zcte_ciot-nr_card_ped.
    inf_ciot-valores_servico-nr_tag_strada    = wa_zcte_ciot-nr_tag_strada.  "*-CS2024001181-16.12.2024-#160717-JT
    inf_ciot-valores_servico-id_rota          = wa_zcte_ciot-id_rota.
    inf_ciot-valores_servico-qtd_eixo         = wa_zcte_ciot-qtd_eixo.
    inf_ciot-valores_servico-nr_vr_xml_tipf   = wa_zcte_ciot-nr_vr_xml_tipf.

    inf_ciot-valores_servico-vlr_impostos     = wa_zcte_ciot-vlr_impostos.
    inf_ciot-valores_servico-vlr_frete        = wa_zcte_ciot-vlr_frete.
    inf_ciot-valores_servico-vlr_adiantamento = wa_zcte_ciot-vlr_adiantamento.
    inf_ciot-valores_servico-vlr_triagem      = wa_zcte_ciot-vlr_triagem.
    inf_ciot-valores_servico-vlr_saldo        = wa_zcte_ciot-vlr_saldo.
    inf_ciot-valores_servico-vlr_seguro       = wa_zcte_ciot-vlr_seguro.

    inf_ciot-valores_servico-vlr_pis_cofins   = wa_zcte_ciot-vlr_pis_cofins.
    inf_ciot-valores_servico-vlr_inss_lucro   = wa_zcte_ciot-vlr_inss_lucro.

    inf_ciot-valores_servico-moeda            = wa_zcte_ciot-moeda.

    "Informações Acessórias

    inf_ciot-inf_acessorias-vlr_inss        = wa_zcte_ciot-vlr_inss.
    inf_ciot-inf_acessorias-vlr_iss         = wa_zcte_ciot-vlr_iss.
    inf_ciot-inf_acessorias-vlr_sest        = wa_zcte_ciot-vlr_sest.
    inf_ciot-inf_acessorias-vlr_irpf        = wa_zcte_ciot-vlr_irpf.
    inf_ciot-inf_acessorias-vlr_iof         = wa_zcte_ciot-vlr_iof.

    inf_ciot-inf_acessorias-perc_tolerancia = wa_zcte_ciot-perc_tolerancia.
    inf_ciot-inf_acessorias-vlr_unit_merc   = wa_zcte_ciot-vlr_unit_merc.
    inf_ciot-inf_acessorias-unid_vlr_merc   = wa_zcte_ciot-unid_vlr_merc.
    inf_ciot-inf_acessorias-vlr_unit_frete  = wa_zcte_ciot-vlr_unit_frete.
    inf_ciot-inf_acessorias-unid_vlr_frete  = wa_zcte_ciot-unid_vlr_frete.
    inf_ciot-inf_acessorias-valor_previsto  = wa_zcte_ciot-valor_previsto.
    inf_ciot-inf_acessorias-resp_pagamento  = wa_zcte_ciot-resp_pagamento.
    inf_ciot-inf_acessorias-nr_bc_banco     = wa_zcte_ciot-nr_bc_banco.
    inf_ciot-inf_acessorias-nr_bc_agencia   = wa_zcte_ciot-nr_bc_agencia.
    inf_ciot-inf_acessorias-nr_bc_conta     = wa_zcte_ciot-nr_bc_conta.
    inf_ciot-inf_acessorias-cnpj_recebedor  = wa_zcte_ciot-cnpj_recebedor.
    inf_ciot-inf_acessorias-peso_chegada    = wa_zcte_ciot-peso_chegada.
    inf_ciot-inf_acessorias-flag_lib_adto   = wa_zcte_ciot-flag_lib_adto.
    inf_ciot-inf_acessorias-flag_lib_sldo   = wa_zcte_ciot-flag_lib_sldo.
    inf_ciot-inf_acessorias-tipo_carga      = wa_zcte_ciot-tipo_carga.


    me->set_cd_ciot( EXPORTING pcd_ciot = inf_ciot-cd_ciot ).
    me->set_id_op_viagem_adm( EXPORTING pid_op_viagem_adm = inf_ciot-id_op_viagem_adm ).
    me->set_nucontrato( EXPORTING p_nucontrato = inf_ciot-nucontrato ).
    me->set_id_viagem( EXPORTING p_id_viagem = inf_ciot-id_viagem ).  "*-#127471-18.04.2024-JT-inicio
    me->set_tp_plano_administradora( EXPORTING i_tp_plano_administradora = inf_ciot-tp_plano_administradora ).
    me->set_nr_ciot( EXPORTING pnr_ciot = inf_ciot-nr_ciot ).
    me->set_rntrc( EXPORTING p_rntrc = inf_ciot-rntrc ).
    me->set_st_ciot( EXPORTING p_st_ciot = inf_ciot-st_ciot ).
    me->set_distancia( i_distz = inf_ciot-distancia ).
    me->set_emissor( EXPORTING p_emissor = inf_ciot-emissor ).
    me->set_docnum( EXPORTING p_docnum = inf_ciot-docnum ).
    me->set_link_contrato( EXPORTING p_link_contrato = inf_ciot-link_contrato ).
    me->set_link_pedagio( EXPORTING p_link_pedagio = inf_ciot-link_pedagio ).
    me->set_link_carga_pedagio( EXPORTING p_link_carga_pedagio = inf_ciot-link_carga_pedagio ).
    me->set_link_resumo( EXPORTING p_link_resumo = inf_ciot-link_resumo ).
    me->set_tknum( EXPORTING p_tknum = inf_ciot-tknum ).
    me->set_parceiros( EXPORTING p_parceiros = inf_ciot-parceiros ).

    me->set_municipio_origem( EXPORTING
        p_uf_origem        = inf_ciot-uf_origem
        p_municipio_origem = inf_ciot-municipio_origem
        p_dt_origem        = inf_ciot-dt_origem ).

    me->set_municipio_termin(
      EXPORTING
        p_uf_termin        = inf_ciot-uf_termin
        p_municipio_termin = inf_ciot-municipio_termin
        p_dt_termin        = inf_ciot-dt_termin ).

    me->set_carga( EXPORTING p_carga = inf_ciot-carga ).
    me->set_veiculos( EXPORTING p_veiculo = inf_ciot-veiculos ).
    me->set_valores_servico( EXPORTING p_valores_servico = inf_ciot-valores_servico ).
    me->set_inf_acessorias( EXPORTING p_inf_acessorias = inf_ciot-inf_acessorias ).
    me->set_tipo_transp( EXPORTING p_tipo_transp = inf_ciot-tipo_transp ).
    me->set_id_rota( EXPORTING p_id_rota = inf_ciot-id_rota ).

    CHECK pvisualiza IS NOT INITIAL.

    me->visualizar( ).

  ENDMETHOD.


  METHOD GET_INF_ACESSORIAS.
    P_INF_ACESSORIAS = ME->INF_ACESSORIAS.
  ENDMETHOD.


  METHOD GET_INF_CONHEC.

    DATA: WA_J_1BNFDOC TYPE J_1BNFDOC.

    SELECT SINGLE * INTO WA_J_1BNFDOC
      FROM J_1BNFDOC
     WHERE DOCNUM EQ ME->DOCNUM.

    P_SERIES = WA_J_1BNFDOC-SERIES.
    P_MODEL  = WA_J_1BNFDOC-MODEL.

    IF WA_J_1BNFDOC-NFE EQ 'X'.
      P_NUMERO = WA_J_1BNFDOC-NFENUM.
    ELSE.
      MOVE WA_J_1BNFDOC-NFNUM TO P_NUMERO.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_NUMERO
        IMPORTING
          OUTPUT = P_NUMERO.
    ENDIF.

  ENDMETHOD.


  METHOD GET_LINK_CARGA_PEDAGIO.
    P_LINK_CARGA_PEDAGIO = ME->LINK_CARGA_PEDAGIO.
  ENDMETHOD.


  METHOD GET_LINK_CONTRATO.
    P_LINK_CONTRATO = ME->LINK_CONTRATO.
  ENDMETHOD.


  METHOD GET_LINK_PEDAGIO.
    P_LINK_PEDAGIO = ME->LINK_PEDAGIO.
  ENDMETHOD.


  METHOD GET_LINK_RESUMO.
    P_LINK_RESUMO = ME->LINK_RESUMO.
  ENDMETHOD.


  METHOD GET_MUNICIPIO_ORIGEM.
    P_UF_ORIGEM        = ME->UF_ORIGEM.
    P_MUNICIPIO_ORIGEM = ME->MUNICIPIO_ORIGEM.
    P_DT_ORIGEM        = ME->DT_ORIGEM.
  ENDMETHOD.


  METHOD GET_MUNICIPIO_TERMIN.
    P_UF_TERMIN        = ME->UF_TERMIN.
    P_MUNICIPIO_TERMIN = ME->MUNICIPIO_TERMIN.
    P_DT_TERMIN        = ME->DT_TERMIN.
  ENDMETHOD.


  METHOD GET_NR_CIOT.
    PNR_CIOT = ME->NR_CIOT.
  ENDMETHOD.


  METHOD GET_NUCONTRATO.
    P_NUCONTRATO = ME->NUCONTRATO.
  ENDMETHOD.


  METHOD GET_PARCEIROS.
    P_PARCEIROS = ME->PARCEIROS.
  ENDMETHOD.


  METHOD GET_RNTRC.
    P_RNTRC = ME->RNTRC.
  ENDMETHOD.


  METHOD GET_ST_CIOT.
    P_ST_CIOT = ME->ST_CIOT.
  ENDMETHOD.


  METHOD GET_TIPO_TRANSP.
    P_TIPO_TRANSP = ME->TIPO_TRANSP.
  ENDMETHOD.


  METHOD GET_TKNUM.
    P_TKNUM = ME->TKNUM.
  ENDMETHOD.


  METHOD GET_TP_PLANO_ADMNISTRADORA.
    E_TP_PLANO_ADMINISTRADORA = ME->TP_PLANO_ADMINISTRADORA.
  ENDMETHOD.


  METHOD GET_VALORES_SERVICO.
    P_VALORES_SERVICO = ME->VALORES_SERVICO.
  ENDMETHOD.


  METHOD GET_VEICULOS.
    P_VEICULO = ME->VEICULOS.
  ENDMETHOD.


  METHOD GRAVAR.

    DATA: P_CD_CIOT	   TYPE ZCIOT,
          WA_ZCTE_CIOT TYPE ZCTE_CIOT,
          WA_ZCTE_RET  TYPE ZCIOT_RET,
          IT_PARCEIROS TYPE ZCTE_CIOT_PARCE_T.

    "Informações CIOT

    CALL METHOD ME->GET_CD_CIOT
      IMPORTING
        PCD_CIOT = P_CD_CIOT.

    IF P_CD_CIOT IS INITIAL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR = '01'
          OBJECT      = 'ZCIOT'
        IMPORTING
          NUMBER      = P_CD_CIOT.

      CALL METHOD ME->SET_CD_CIOT
        EXPORTING
          PCD_CIOT = P_CD_CIOT.

    ENDIF.

    CALL METHOD ME->VISUALIZAR
      IMPORTING
        P_ZCTE_RET       = WA_ZCTE_RET
        P_ZCTE_CIOT      = WA_ZCTE_CIOT
        P_ZCTE_PARCEIROS = IT_PARCEIROS.

    MODIFY ZCTE_CIOT FROM WA_ZCTE_CIOT.
    DELETE FROM ZCTE_CIOT_PARCE WHERE CD_CIOT EQ WA_ZCTE_CIOT-CD_CIOT AND CD_CIOT NE SPACE.
    MODIFY ZCTE_CIOT_PARCE FROM TABLE IT_PARCEIROS.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD imprimir.

    DATA: wa_zcte_ciot_imp TYPE zcte_ciot_imp,
          lc_agr_url       TYPE agr_url.

*-#130491-04.04.2024-JT-inicio
    IF me->link_contrato IS NOT INITIAL.
      IF me->link_contrato(4) <> 'http'.
*---- recuperar PDF
        TRY .
            me->get_pdf_arquivos_viagem( i_cd_ciot = me->cd_ciot i_tipoarq = 'CONTRATO' i_imprimir = imprimir ).
          CATCH zcx_error INTO DATA(ex_error).
        ENDTRY.
        e_url = me->link_contrato.
      ELSE.
        MOVE me->link_contrato TO lc_agr_url.
        e_url = me->link_contrato.
        IF imprimir EQ abap_true.
          CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
            EXPORTING
              node_data = lc_agr_url.
        ENDIF.
      ENDIF.

      wa_zcte_ciot_imp-cd_ciot      = me->cd_ciot.
      wa_zcte_ciot_imp-tp_impressao = 'C'.
      wa_zcte_ciot_imp-dt_impressao = sy-datum.
      wa_zcte_ciot_imp-hr_impressao = sy-uzeit.
      wa_zcte_ciot_imp-usuario      = sy-uname.
      MODIFY zcte_ciot_imp FROM wa_zcte_ciot_imp.
    ENDIF.
*-#130491-04.04.2024-JT-fim

*-#130491-04.04.2024-JT-inicio
*    IF NOT me->link_contrato IS INITIAL.
*      MOVE me->link_contrato TO lc_agr_url.
*      e_url = me->link_contrato.
*      IF imprimir EQ abap_true.
*        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
*          EXPORTING
*            node_data = lc_agr_url.
*      ENDIF.
*
*      wa_zcte_ciot_imp-cd_ciot      = me->cd_ciot.
*      wa_zcte_ciot_imp-tp_impressao = 'C'.
*      wa_zcte_ciot_imp-dt_impressao = sy-datum.
*      wa_zcte_ciot_imp-hr_impressao = sy-uzeit.
*      wa_zcte_ciot_imp-usuario      = sy-uname.
*      MODIFY zcte_ciot_imp FROM wa_zcte_ciot_imp.
*    ENDIF.
*-#130491-04.04.2024-JT-fim

  ENDMETHOD.


  METHOD imprimir_pedagio.

    DATA: wa_zcte_ciot_imp TYPE zcte_ciot_imp,
          lc_agr_url       TYPE agr_url.

*-#130491-04.04.2024-JT-inicio
    IF me->link_pedagio IS NOT INITIAL.
      IF me->link_pedagio(4) <> 'http'.
*---- recuperar PDF
        TRY .
            me->get_pdf_arquivos_viagem( i_cd_ciot = me->cd_ciot i_tipoarq = 'PEDAGIO' i_imprimir = abap_true ).
          CATCH zcx_error INTO DATA(ex_error).
        ENDTRY.
      ELSE.
        MOVE me->link_pedagio TO lc_agr_url.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            node_data = lc_agr_url.
      ENDIF.

      wa_zcte_ciot_imp-cd_ciot      = me->cd_ciot.
      wa_zcte_ciot_imp-tp_impressao = 'P'.
      wa_zcte_ciot_imp-dt_impressao = sy-datum.
      wa_zcte_ciot_imp-hr_impressao = sy-uzeit.
      wa_zcte_ciot_imp-usuario      = sy-uname.
      MODIFY zcte_ciot_imp FROM wa_zcte_ciot_imp.
    ENDIF.
*-#130491-04.04.2024-JT-fim

*-#130491-04.04.2024-JT-inicio
*    IF NOT me->link_pedagio IS INITIAL.
*      MOVE me->link_pedagio TO lc_agr_url.
*
*      CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
*        EXPORTING
*          node_data = lc_agr_url.
*
*      wa_zcte_ciot_imp-cd_ciot      = me->cd_ciot.
*      wa_zcte_ciot_imp-tp_impressao = 'P'.
*      wa_zcte_ciot_imp-dt_impressao = sy-datum.
*      wa_zcte_ciot_imp-hr_impressao = sy-uzeit.
*      wa_zcte_ciot_imp-usuario      = sy-uname.
*      MODIFY zcte_ciot_imp FROM wa_zcte_ciot_imp.
*    ENDIF.
*-#130491-04.04.2024-JT-fim

  ENDMETHOD.


  METHOD IMPRIMIR_RESUMO.

    DATA: WA_ZCTE_CIOT_IMP TYPE ZCTE_CIOT_IMP,
          LC_AGR_URL       TYPE AGR_URL.

    IF NOT ME->LINK_RESUMO IS INITIAL.

      MOVE ME->LINK_RESUMO TO LC_AGR_URL.

      CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
        EXPORTING
          NODE_DATA = LC_AGR_URL.

      WA_ZCTE_CIOT_IMP-CD_CIOT      = ME->CD_CIOT.
      WA_ZCTE_CIOT_IMP-TP_IMPRESSAO = 'R'.
      WA_ZCTE_CIOT_IMP-DT_IMPRESSAO = SY-DATUM.
      WA_ZCTE_CIOT_IMP-HR_IMPRESSAO = SY-UZEIT.
      WA_ZCTE_CIOT_IMP-USUARIO      = SY-UNAME.
      MODIFY ZCTE_CIOT_IMP FROM WA_ZCTE_CIOT_IMP.

    ENDIF.

  ENDMETHOD.


  METHOD novo.

    DATA: wa_j_1bnfdoc      TYPE j_1bnfdoc,
          wa_j_1bnfdoc_nf   TYPE j_1bnfdoc,
          wa_marc           TYPE marc,
          wa_j_1bnflin      TYPE j_1bnflin,
          wa_lfa1           TYPE lfa1,
          wa_lfa1_rec       TYPE lfa1,
          wa_adrc           TYPE adrc,
          inf_ciot          TYPE zciot_ret,
          wa_zcte_parceiros TYPE zcte_parceiros,
          wa_zcte_info_nota TYPE zcte_info_nota,
          wa_zcte_trans     TYPE zcte_trans,
          wa_zcte_motorista TYPE zcte_motorista,
          wa_fatura_servico TYPE vbrp,
          wa_ordem_venda    TYPE vbak,
          wa_doc_transp     TYPE vttk,
          wa_banco_prop     TYPE lfbk,
          wa_banco_rec      TYPE lfbk,
          wa_cte_item       TYPE j_1bnflin,
          it_item_transp    TYPE TABLE OF vttp,
          it_lips           TYPE TABLE OF lips,
          it_liberacao      TYPE TABLE OF zlest0046,
          wa_lips           TYPE lips,
          wk_a912           TYPE a912,
          wk_konp           TYPE konp,
          wa_parceiro       TYPE zciot_contratante,
          wa_lfb1           TYPE lfb1,
          wa_vttk           TYPE vttk,
          wa_zcte           TYPE zcte_ciot,
          wa_liberacao      TYPE zlest0046,
          wa_agencia        TYPE bankl,
          wa_makt           TYPE makt,
          wa_mara           TYPE mara,
          wa_zlest0193      TYPE zlest0193,
          it_qtd_eixo       TYPE TABLE OF zcte_trans,
          wa_qtd_eixo       TYPE zcte_trans,
          gw_zlest0084      TYPE zlest0084,
          gw_zlest0091      TYPE zlest0091,
          q                 TYPE i.


    DATA: p_nr_ciot	         TYPE zciot,
          p_rntrc	           TYPE zrntrc,
          p_emissor          TYPE tdlnr,
          p_st_ciot          TYPE zst_ciot,
          p_cancelado	       TYPE j_1bcancel,
          p_parceiros  	     TYPE zciot_parceiros_t,
          p_uf_origem        TYPE regio,
          p_municipio_origem TYPE char07,
          p_dt_origem        TYPE zciot_dt_inicio,
          p_uf_termin        TYPE regio,
          p_municipio_termin TYPE char07,
          p_dt_termin        TYPE zciot_dt_fim,
          p_carga	           TYPE zciot_carga,
          p_veiculos         TYPE zciot_veiculos_t,
          p_valores_servico	 TYPE zciot_valores,
          p_inf_acessorias   TYPE zciot_acessorias.

    DATA:   qtd_eixo TYPE i.
    DATA: vg_matnr TYPE char18.

    DATA: result_tab TYPE match_result_tab.

    "" BUSCA DE VALORES DOCNUM
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF p_zcte_trans-docnum IS INITIAL.
      MESSAGE e002 RAISING inf_docnum.
    ENDIF.

    SELECT SINGLE * INTO wa_j_1bnfdoc FROM j_1bnfdoc WHERE docnum EQ p_zcte_trans-docnum.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e003 WITH p_zcte_trans-docnum RAISING nao_docnum.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE RNTRC
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF p_zcte_trans-proprietario IS INITIAL.
      MESSAGE e006 RAISING inf_propveiculo.
    ENDIF.

    SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr EQ p_zcte_trans-proprietario.

    IF ( sy-subrc IS NOT INITIAL ) OR ( wa_lfa1-bahns IS INITIAL ).
      MESSAGE e005 WITH p_zcte_trans-proprietario RAISING nao_rtrc.
    ELSE.
      "" BUSCA DE INF. CONTRATADO
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""




      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Busca Endereço Proprietário do Veículo

      SELECT SINGLE * INTO wa_adrc
        FROM adrc
       WHERE addrnumber = wa_lfa1-adrnr.

      IF sy-subrc IS INITIAL.
        CLEAR: wa_parceiro.
        wa_parceiro-tipo        = '99'.
        wa_parceiro-codigo      = wa_lfa1-lifnr.
        wa_parceiro-nome        = wa_lfa1-name1.
        wa_parceiro-razao       = wa_lfa1-name1.
        wa_parceiro-cnpj        = wa_lfa1-stcd1.
        wa_parceiro-cpf         = wa_lfa1-stcd2.
        wa_parceiro-fone        = wa_lfa1-telf1.
        wa_parceiro-logradouro  = wa_adrc-street(25).
        IF NOT wa_adrc-house_num1 IS INITIAL.
          wa_parceiro-numero    = wa_adrc-house_num1.
        ENDIF.
        wa_parceiro-bairro      = wa_adrc-city2.
        wa_parceiro-municipio   = wa_adrc-taxjurcode+3(7).
        wa_parceiro-cep         = wa_adrc-post_code1.
        wa_parceiro-uf          = wa_adrc-region.
        FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
        IF NOT result_tab IS INITIAL.
          wa_parceiro-numero = 'S/N'.
        ENDIF.
        "move wa_adrc-tel_number to wa_parceiro-fone.
        APPEND wa_parceiro TO p_parceiros.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.

    p_rntrc = wa_lfa1-bahns.

    SELECT SINGLE * INTO wa_banco_prop
      FROM lfbk
     WHERE lifnr EQ wa_lfa1-lifnr
       AND bvtyp EQ '0001'.

    IF wa_lfa1-bahns IS INITIAL.
      MESSAGE e040 WITH wa_lfa1-lifnr RAISING nao_conta_corrente.
    ENDIF.

    CLEAR: wa_agencia.

    IF NOT ( wa_lfa1-lnrza IS INITIAL ).

      SELECT SINGLE * FROM lfa1 INTO wa_lfa1_rec WHERE lifnr EQ wa_lfa1-lnrza.

      IF ( sy-subrc EQ 0 ).

        CLEAR: wa_banco_rec.

        SELECT SINGLE * INTO wa_banco_rec
          FROM lfbk
         WHERE lifnr EQ wa_lfa1_rec-lifnr
           AND bvtyp EQ '0001'.

        CONCATENATE wa_banco_rec-bankl+4(11) wa_banco_rec-bkont INTO wa_agencia.

        p_inf_acessorias-nr_bc_banco       = wa_banco_rec-bankl(3).
        p_inf_acessorias-nr_bc_agencia     = wa_agencia.
        p_inf_acessorias-nr_bc_conta       = wa_banco_rec-bankn.
        p_inf_acessorias-cnpj_recebedor    = wa_lfa1_rec-stcd1.

      ENDIF.

    ELSE.

      IF wa_banco_prop IS NOT INITIAL.
        CONCATENATE wa_banco_prop-bankl+4(11) wa_banco_prop-bkont INTO wa_agencia.
        p_inf_acessorias-nr_bc_banco       = wa_banco_prop-bankl(3).
        p_inf_acessorias-nr_bc_agencia     = wa_agencia.
        p_inf_acessorias-nr_bc_conta       = wa_banco_prop-bankn.
        p_inf_acessorias-cnpj_recebedor    = ''.
      ENDIF.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA LIBERAÇÃO DE CRÉDITO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT *
      FROM zlest0046
      INTO TABLE it_liberacao
     WHERE lifnr   EQ p_zcte_trans-proprietario
       AND status EQ 'A'.

    SELECT SINGLE *
       FROM vttk
       INTO wa_doc_transp
     WHERE tknum EQ p_zcte_identifica-tknum.

    TRY .

        zcl_itinerario=>zif_itinerario~get_instance(
          )->set_itinerario( i_route = wa_doc_transp-route
          )->get_distancia( IMPORTING e_distancia = DATA(e_distancia)
          ).
      CATCH zcx_itinerario INTO DATA(ex_itinerario).
        ex_itinerario->zif_error~published_erro( EXPORTING i_msgty = 'S' ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING itinerario.
    ENDTRY.

    me->set_distancia( i_distz = e_distancia ).

    READ TABLE it_liberacao INTO wa_liberacao WITH KEY route = wa_doc_transp-route.

    IF sy-subrc IS INITIAL.
      p_inf_acessorias-flag_lib_adto =  wa_liberacao-st_adto.
      p_inf_acessorias-flag_lib_sldo =  wa_liberacao-st_saldo.
    ELSEIF it_liberacao[] IS NOT INITIAL.
      READ TABLE it_liberacao INTO wa_liberacao INDEX 1.
      p_inf_acessorias-flag_lib_adto =  wa_liberacao-st_adto.
      p_inf_acessorias-flag_lib_sldo =  wa_liberacao-st_saldo.
    ELSE.
      p_inf_acessorias-flag_lib_adto =  'S'.
      p_inf_acessorias-flag_lib_sldo =  'S'.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. CONTRATANTE
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO wa_zcte_parceiros
      FROM zcte_parceiros
     WHERE docnum EQ p_zcte_trans-docnum.

    CLEAR: wa_parceiro.
    wa_parceiro-tipo        = '00'.
    wa_parceiro-codigo      = wa_zcte_parceiros-emit_codigo.
    wa_parceiro-nome        = wa_zcte_parceiros-emit_xnome.
    wa_parceiro-razao       = wa_zcte_parceiros-emit_xnome.
    wa_parceiro-cnpj        = wa_zcte_parceiros-emit_cnpj.
    wa_parceiro-logradouro  = wa_zcte_parceiros-emit_xlgr.
    wa_parceiro-numero      = wa_zcte_parceiros-emit_nro.
    wa_parceiro-bairro      = wa_zcte_parceiros-emit_xbairro.
    wa_parceiro-uf          = wa_zcte_parceiros-emit_uf.
    wa_parceiro-municipio   = wa_zcte_parceiros-emit_cmun.
    wa_parceiro-cep         = wa_zcte_parceiros-emit_cep.
    wa_parceiro-fone        = wa_zcte_parceiros-emit_fone.
    FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
    IF NOT result_tab IS INITIAL.
      wa_parceiro-numero = 'S/N'.
    ENDIF.
    APPEND wa_parceiro TO p_parceiros.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. REMETENTE
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR: wa_parceiro.
    wa_parceiro-tipo        = '01'.
    wa_parceiro-codigo      = wa_zcte_parceiros-reme_codigo.
    wa_parceiro-nome        = wa_zcte_parceiros-reme_xnome.
    wa_parceiro-razao       = wa_zcte_parceiros-reme_xnome.
    wa_parceiro-cnpj        = wa_zcte_parceiros-reme_cnpj.
    wa_parceiro-cpf         = wa_zcte_parceiros-reme_cpf.
    wa_parceiro-logradouro  = wa_zcte_parceiros-reme_xlgr.
    wa_parceiro-numero      = wa_zcte_parceiros-reme_nro.
    wa_parceiro-bairro      = wa_zcte_parceiros-reme_xbairro.
    wa_parceiro-uf          = wa_zcte_parceiros-reme_uf.
    wa_parceiro-municipio   = wa_zcte_parceiros-reme_cmun.
    wa_parceiro-cep         = wa_zcte_parceiros-reme_cep.
    wa_parceiro-fone        = wa_zcte_parceiros-reme_fone.
    FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
    IF NOT result_tab IS INITIAL.
      wa_parceiro-numero = 'S/N'.
    ENDIF.
    APPEND wa_parceiro TO p_parceiros.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. DESTINATÁRIO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR: wa_parceiro.
    wa_parceiro-tipo        = '02'.
    wa_parceiro-codigo      = wa_zcte_parceiros-dest_codigo.
    wa_parceiro-nome        = wa_zcte_parceiros-dest_xnome.
    wa_parceiro-razao       = wa_zcte_parceiros-dest_xnome.
    wa_parceiro-cnpj        = wa_zcte_parceiros-dest_cnpj.
    wa_parceiro-cpf         = wa_zcte_parceiros-dest_cpf.
    wa_parceiro-logradouro  = wa_zcte_parceiros-dest_xlgr.
    wa_parceiro-numero      = wa_zcte_parceiros-dest_nro.
    wa_parceiro-bairro      = wa_zcte_parceiros-dest_xbairro.
    wa_parceiro-uf          = wa_zcte_parceiros-dest_uf.
    wa_parceiro-municipio   = wa_zcte_parceiros-dest_cmun.
    wa_parceiro-cep         = wa_zcte_parceiros-dest_cep.
    wa_parceiro-fone        = wa_zcte_parceiros-dest_fone.
    FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
    IF NOT result_tab IS INITIAL.
      wa_parceiro-numero = 'S/N'.
    ENDIF.
    APPEND wa_parceiro TO p_parceiros.

    "" BUSCA DE INF. LOCAL DE COLETA
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * INTO TABLE @DATA(it_parceiros)
       FROM vtpa
     WHERE vbeln EQ @p_zcte_identifica-tknum.

    READ TABLE it_parceiros INTO DATA(w_parceiros) WITH KEY parvw = 'PC'.
    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO wa_lfa1
        FROM lfa1 AS j
       WHERE lifnr EQ w_parceiros-lifnr.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber = wa_lfa1-adrnr.

        CLEAR: wa_parceiro.
        wa_parceiro-tipo        = '06'.
        wa_parceiro-codigo      = wa_lfa1-lifnr.
        wa_parceiro-nome        = wa_lfa1-name1.
        wa_parceiro-razao       = wa_lfa1-name1.
        wa_parceiro-cnpj        = wa_lfa1-stcd1.
        wa_parceiro-cpf         = wa_lfa1-stcd2.
        wa_parceiro-logradouro  = wa_adrc-street(25).
        IF NOT wa_adrc-house_num1 IS INITIAL.
          wa_parceiro-numero    = wa_adrc-house_num1.
        ENDIF.
        wa_parceiro-bairro      = wa_adrc-city2.
        wa_parceiro-uf          = wa_adrc-region.
        wa_parceiro-municipio   = wa_adrc-taxjurcode+3(7).
        wa_parceiro-cep         = wa_adrc-post_code1.
        wa_parceiro-fone        = wa_lfa1-telf1.
        FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
        IF NOT result_tab IS INITIAL.
          wa_parceiro-numero = 'S/N'.
        ENDIF.
        APPEND wa_parceiro TO p_parceiros.
      ENDIF.
    ENDIF.

    "" BUSCA DE INF. LOCAL DE ENTREGA
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    READ TABLE it_parceiros INTO w_parceiros WITH KEY parvw = 'LR'.
    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO @DATA(wa_kna1)
        FROM kna1
       WHERE kunnr EQ @w_parceiros-kunnr.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber = wa_kna1-adrnr.

        CLEAR: wa_parceiro.
        wa_parceiro-tipo        = '07'.
        wa_parceiro-codigo      = wa_kna1-kunnr.
        wa_parceiro-nome        = wa_kna1-name1.
        wa_parceiro-razao       = wa_kna1-name1.
        wa_parceiro-cnpj        = wa_kna1-stcd1.
        wa_parceiro-cpf         = wa_kna1-stcd2.
        wa_parceiro-logradouro  = wa_adrc-street(25).
        IF NOT wa_adrc-house_num1 IS INITIAL.
          wa_parceiro-numero    = wa_adrc-house_num1.
        ENDIF.
        wa_parceiro-bairro      = wa_adrc-city2.
        wa_parceiro-uf          = wa_adrc-region.
        wa_parceiro-municipio   = wa_adrc-taxjurcode+3(7).
        wa_parceiro-cep         = wa_adrc-post_code1.
        wa_parceiro-fone        = wa_kna1-telf1.
        FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
        IF NOT result_tab IS INITIAL.
          wa_parceiro-numero = 'S/N'.
        ENDIF.
        APPEND wa_parceiro TO p_parceiros.
      ENDIF.
    ENDIF.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. LOCAL DE ENTREGA - TIPCARD
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CLEAR: wa_parceiro.
    wa_parceiro-tipo        = '88'.
    wa_parceiro-codigo      = wa_zcte_parceiros-lr_codigo.
    wa_parceiro-nome        = wa_zcte_parceiros-lr_nome.
    wa_parceiro-razao       = wa_zcte_parceiros-lr_razao.
    wa_parceiro-cnpj        = wa_zcte_parceiros-lr_cnpj.
    wa_parceiro-cpf         = wa_zcte_parceiros-lr_cpf.
    wa_parceiro-logradouro  = wa_zcte_parceiros-lr_logradouro.
    wa_parceiro-numero      = wa_zcte_parceiros-lr_numero.
    wa_parceiro-bairro      = wa_zcte_parceiros-lr_bairro.
    wa_parceiro-uf          = wa_zcte_parceiros-lr_uf.
    wa_parceiro-municipio   = wa_zcte_parceiros-lr_municipio.
    wa_parceiro-cep         = wa_zcte_parceiros-lr_cep.
    wa_parceiro-fone        = wa_zcte_parceiros-lr_fone.
    FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
    IF NOT result_tab IS INITIAL.
      wa_parceiro-numero = 'S/N'.
    ENDIF.
    APPEND wa_parceiro TO p_parceiros.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. MOTORISTA
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR: wa_parceiro, wa_lfa1, wa_adrc.

    SELECT SINGLE * INTO wa_zcte_motorista
      FROM zcte_motorista
     WHERE docnum EQ p_zcte_trans-docnum.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO wa_lfa1
        FROM lfa1 AS j
       WHERE lifnr EQ wa_zcte_motorista-lifnr.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber = wa_lfa1-adrnr.

        CLEAR: wa_parceiro.
        wa_parceiro-tipo        = '05'.
        wa_parceiro-codigo      = wa_lfa1-lifnr.
        wa_parceiro-nome        = wa_lfa1-name1.
        wa_parceiro-razao       = wa_lfa1-name1.
        wa_parceiro-cpf         = wa_lfa1-stcd2.
        wa_parceiro-fone        = wa_lfa1-telf1.
        wa_parceiro-logradouro  = wa_adrc-street(25).
        IF NOT wa_adrc-house_num1 IS INITIAL.
          wa_parceiro-numero    = wa_adrc-house_num1.
        ENDIF.
        wa_parceiro-bairro      = wa_adrc-city2.
        wa_parceiro-municipio   = wa_adrc-taxjurcode+3(7).
        wa_parceiro-cep         = wa_adrc-post_code1.
        wa_parceiro-uf          = wa_adrc-region.

        wa_parceiro-rg          = wa_lfa1-stcd3.
        wa_parceiro-cnh         = wa_lfa1-stcd4.
        wa_parceiro-pis         = wa_lfa1-stenr.

        SELECT SINGLE * INTO wa_lfb1
          FROM lfb1
         WHERE lifnr EQ wa_zcte_motorista-lifnr
           AND bukrs EQ wa_j_1bnfdoc-bukrs.

        IF sy-subrc IS INITIAL.
          wa_parceiro-uf_rg       = wa_lfb1-eikto.
          wa_parceiro-uf_cnh      = wa_lfb1-zsabe.
        ENDIF.
        FIND ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_parceiro-numero RESULTS result_tab.
        IF NOT result_tab IS INITIAL.
          wa_parceiro-numero = 'S/N'.
        ENDIF.

        "Informações Versão 1.15 - Manual de WebService e Integração TipFrete
        wa_parceiro-nomemae       = wa_lfa1-profs.
        CASE wa_lfa1-sexkz.
          WHEN '1'.
            wa_parceiro-sexo          = 'M'.
          WHEN '2'.
            wa_parceiro-sexo          = 'F'.
        ENDCASE.
        wa_parceiro-dt_nascimento = wa_lfa1-gbdat.
        IF wa_lfa1-gbort IS NOT INITIAL.
          q = strlen( wa_lfa1-gbort ).
          q = q - 3.
          IF q GT 3.
            wa_parceiro-uf_rg  = wa_lfa1-gbort(2).
            wa_parceiro-org_rg = wa_lfa1-gbort+3(q).
          ENDIF.
        ENDIF.
        APPEND wa_parceiro TO p_parceiros.
      ENDIF.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""


    "" BUSCA DE INF. MERCADORIA
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    p_carga-quantidade             = 0.
    p_inf_acessorias-vlr_unit_merc = 0.

    SELECT *
      INTO wa_zcte_info_nota
      FROM zcte_info_nota
     WHERE docnum EQ p_zcte_trans-docnum.

      CLEAR: vg_matnr.
      vg_matnr = |{ wa_zcte_info_nota-material ALPHA = OUT }|.
      vg_matnr = |{ vg_matnr ALPHA = IN }|.
      CLEAR: wa_zcte_info_nota-material.
      wa_zcte_info_nota-material = vg_matnr.

*-BUG 149458-09.10.2024-JT-#149458-inicio
      IF wa_zcte_info_nota-material IS INITIAL.
        MESSAGE e044 RAISING material_nao_encontrado.
      ENDIF.
*-BUG 149458-09.10.2024-JT-#149458-fim

      IF NOT wa_zcte_info_nota-docnum_nf IS INITIAL.
        SELECT SINGLE * INTO wa_j_1bnfdoc_nf
          FROM j_1bnfdoc
         WHERE docnum EQ wa_zcte_info_nota-docnum_nf.
      ELSE.
        wa_j_1bnfdoc_nf-branch = wa_j_1bnfdoc-branch.
      ENDIF.

      SELECT SINGLE * INTO wa_marc
        FROM marc
       WHERE matnr EQ wa_zcte_info_nota-material
         AND werks EQ wa_j_1bnfdoc_nf-branch.

*-BUG 149458-09.10.2024-JT-#149458-inicio
      IF sy-subrc <> 0.
        MESSAGE e045 WITH wa_zcte_info_nota-material wa_j_1bnfdoc_nf-branch RAISING material_nao_expandido.
      ENDIF.
*-BUG 149458-09.10.2024-JT-#149458-fim

      SELECT SINGLE * FROM makt INTO wa_makt
        WHERE matnr EQ wa_zcte_info_nota-material
          AND spras EQ sy-langu.

      p_carga-descmatnr  = wa_makt-maktx.

      p_carga-natureza   = wa_marc-steuc(4).
      ADD wa_zcte_info_nota-quantidade  TO p_carga-quantidade.
      ADD wa_zcte_info_nota-vl_produtos TO p_inf_acessorias-vlr_unit_merc.
      p_carga-unidade    = wa_zcte_info_nota-unidade.
      APPEND wa_zcte_info_nota TO p_carga-notas.
      "" BUSCA DE INF. ACESSÓRIAS
*      IF P_CARGA-QUANTIDADE GT 0.
*        P_INF_ACESSORIAS-VLR_UNIT_MERC = P_INF_ACESSORIAS-VLR_UNIT_MERC / P_CARGA-QUANTIDADE.
*      ELSE.
*        P_INF_ACESSORIAS-VLR_UNIT_MERC = 0.
*      ENDIF.
      p_inf_acessorias-unid_vlr_merc = wa_zcte_info_nota-unidade.


      SELECT SINGLE * FROM mara INTO wa_mara
        WHERE matnr EQ  wa_zcte_info_nota-material.

      SELECT SINGLE * FROM zlest0193 INTO wa_zlest0193
        WHERE matkl EQ wa_mara-matkl.

      IF sy-subrc <> 0.
        MESSAGE e041 WITH wa_mara-matkl RAISING tipo_carga.
      ELSE.
        p_inf_acessorias-tipo_carga = wa_zlest0193-tipo_carga.
      ENDIF.
    ENDSELECT.

    IF p_carga-quantidade GT 0.
      p_inf_acessorias-vlr_unit_merc = p_inf_acessorias-vlr_unit_merc / p_carga-quantidade.
    ELSE.
      p_inf_acessorias-vlr_unit_merc = 0.
    ENDIF.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DE INF. VEÍCULO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * INTO wa_zcte_trans
      FROM zcte_trans
     WHERE docnum EQ p_zcte_trans-docnum.
      APPEND wa_zcte_trans TO p_veiculos.
    ENDSELECT.

    TRY .

        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
          EXPORTING
            i_docnum    = p_zcte_trans-docnum    " Nº documento
          IMPORTING
            e_pag_frete = DATA(e_pag_frete)
        ).

        CASE e_pag_frete.
          WHEN abap_true.
            p_st_ciot  = '0'.
          WHEN abap_false.
            p_st_ciot  = '9'.
        ENDCASE.

      CATCH zcx_faturamento.    "
      CATCH zcx_error.    " .
    ENDTRY.


    SELECT * INTO wa_j_1bnflin
      FROM j_1bnflin
      UP TO 1 ROWS
     WHERE docnum EQ p_zcte_trans-docnum.

      "Fatura do Serviço
      SELECT SINGLE * INTO wa_fatura_servico
        FROM vbrp
       WHERE vbeln = wa_j_1bnflin-refkey(10)
         AND posnr = wa_j_1bnflin-refitm.

      IF sy-subrc IS INITIAL.

        "Ordem de Venda
        SELECT SINGLE * INTO wa_ordem_venda
          FROM vbak
         WHERE vbeln = wa_fatura_servico-aubel.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE * INTO wa_doc_transp
            FROM vttk
           WHERE tknum = wa_ordem_venda-tknum.

          IF sy-subrc IS INITIAL.
            dt_origem = wa_doc_transp-datbg.
            dt_termin = wa_doc_transp-daten.
          ENDIF.

          SELECT * INTO TABLE it_item_transp
            FROM vttp
           WHERE tknum = wa_ordem_venda-tknum.

          IF sy-subrc IS INITIAL.

            SELECT *
              FROM lips
              INTO TABLE it_lips
               FOR ALL ENTRIES IN it_item_transp
             WHERE vbeln EQ it_item_transp-vbeln.

            IF sy-subrc IS INITIAL.

              READ TABLE it_lips INTO wa_lips INDEX 1.

              SELECT *
                INTO wk_a912
                FROM a912
               WHERE kappl = 'F'
                 AND kschl = 'ZMRG'
                 AND matnr = wa_lips-matnr
                 AND datab <= p_zcte_identifica-dhemi
                 AND datbi >= p_zcte_identifica-dhemi.

                SELECT *
                  INTO wk_konp
                  FROM konp
                 WHERE knumh = wk_a912-knumh.
                  p_inf_acessorias-perc_tolerancia = wk_konp-kbetr / 10.
                ENDSELECT.

              ENDSELECT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDSELECT.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA VALORES DO SERVIÇO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO p_zcte_identifica
      FROM zcte_identifica
     WHERE docnum EQ p_zcte_trans-docnum.

    IF sy-subrc IS INITIAL.

      p_emissor = p_zcte_identifica-emissor.

      p_valores_servico-vlr_combustivel  = p_zcte_identifica-vlr_combustivel.
      p_valores_servico-vlr_pedagio      = p_zcte_identifica-vlr_pedagio.
      p_valores_servico-tp_card_ped      = p_zcte_identifica-tp_card_ped.
      p_valores_servico-nr_card_ped      = p_zcte_identifica-nr_card_ped.
      p_valores_servico-nr_tag_strada    = p_zcte_identifica-nr_tag_strada.  "*-CS2024001181-16.12.2024-#160717-JT
      p_valores_servico-id_rota	         = p_zcte_identifica-id_rota.
      p_valores_servico-qtd_eixo         = p_zcte_identifica-qtd_eixo.
      p_valores_servico-nr_vr_xml_tipf   = p_zcte_identifica-nr_vr_xml_tipf.
      p_valores_servico-vlr_impostos     = p_zcte_identifica-vlr_inss + p_zcte_identifica-vlr_sest +
                                           p_zcte_identifica-vlr_irpf + p_zcte_identifica-vlr_iof  + p_zcte_identifica-vlr_iss.
      p_valores_servico-vlr_frete        = p_zcte_identifica-vtprest.
      p_valores_servico-vlr_adiantamento = p_zcte_identifica-vlr_adiantamento.
      p_valores_servico-vlr_seguro       = p_zcte_identifica-vlr_seguro.
      p_valores_servico-moeda            = p_zcte_identifica-waers.
      p_valores_servico-vlr_saldo        = p_zcte_identifica-vtprest - p_valores_servico-vlr_adiantamento.
      p_valores_servico-vlr_triagem      = p_zcte_identifica-vlr_triagem.

      p_valores_servico-vlr_pis_cofins   = p_zcte_identifica-vlr_pis_cofins.
      p_valores_servico-vlr_inss_lucro   = p_zcte_identifica-vlr_inss_lucro.

      "" BUSCA DE INF. ACESSÓRIAS
      p_inf_acessorias-vlr_unit_frete    = p_zcte_identifica-vlr_unit_frete.
      p_inf_acessorias-unid_vlr_frete    = p_zcte_identifica-unid_vlr_frete.
      p_inf_acessorias-vlr_inss          = p_zcte_identifica-vlr_inss.
      p_inf_acessorias-vlr_iss           = p_zcte_identifica-vlr_iss.
      p_inf_acessorias-vlr_sest          = p_zcte_identifica-vlr_sest.
      p_inf_acessorias-vlr_irpf          = p_zcte_identifica-vlr_irpf.
      p_inf_acessorias-vlr_iof           = p_zcte_identifica-vlr_iof .

      p_uf_origem        = p_zcte_identifica-ufini.
      p_municipio_origem = p_zcte_identifica-cmunini.

      p_uf_termin        = p_zcte_identifica-uffim.
      p_municipio_termin = p_zcte_identifica-cmunfim.

      "Frete Locação não calcula quebra/perda
      IF p_zcte_identifica-frete_lotacao EQ 'X'.
        p_inf_acessorias-peso_chegada = space.
      ELSE.
        p_inf_acessorias-peso_chegada = 'X'.
      ENDIF.

      IF ( p_zcte_trans-tp_veiculo EQ '0' ) AND ( p_rntrc IS NOT INITIAL ) .
        "p_zcte_identifica-rodo_rntrc = p_rntrc.
        IF p_zcte_identifica-rodo_dt_prev IS NOT INITIAL.
          dt_termin = p_zcte_identifica-rodo_dt_prev.
        ELSE.
          p_zcte_identifica-rodo_dt_prev = dt_termin.
        ENDIF.
        MODIFY zcte_identifica FROM p_zcte_identifica.
      ENDIF.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Rota da Viagem
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
*  CLEAR: QTD_EIXO.
*
*  SELECT * FROM ZCTE_TRANS
*    INTO TABLE IT_QTD_EIXO
*  WHERE DOCNUM EQ P_ZCTE_TRANS-DOCNUM.
*
*  LOOP AT IT_QTD_EIXO INTO WA_QTD_EIXO.
*    QTD_EIXO = QTD_EIXO + WA_QTD_EIXO-QTD_EIXO.
*  ENDLOOP.
*
*  SELECT SINGLE * FROM ZLEST0091
*    INTO GW_ZLEST0091
*  WHERE QTD_EIXO EQ QTD_EIXO.
*
*  SELECT SINGLE * FROM ZLEST0084
*    INTO GW_ZLEST0084
*  WHERE CAT_VEICULO   EQ GW_ZLEST0091-CATEGORIA
*    AND MUNIC_ORIGEM  EQ P_MUNICIPIO_ORIGEM
*    AND MUNIC_DESTINO EQ P_MUNICIPIO_TERMIN.
*
*  IF ( SY-SUBRC EQ 0 ).
*    P_ZCTE_IDENTIFICA-ROTA = GW_ZLEST0084-ID_ROTA.
*  ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" BUSCA DATA DE INÍCIO E PREVISTA PARA FIM
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    p_dt_origem = p_zcte_identifica-rodo_dt_inicio.
    p_dt_termin = p_zcte_identifica-rodo_dt_prev.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "" ATUALIZA VALORES OBEJTO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL METHOD me->set_st_ciot
      EXPORTING
        p_st_ciot = p_st_ciot.

    CALL METHOD me->set_rntrc
      EXPORTING
        p_rntrc = p_rntrc.

    CALL METHOD me->set_docnum
      EXPORTING
        p_docnum = p_zcte_trans-docnum.

    CALL METHOD me->set_tknum
      EXPORTING
        p_tknum = p_zcte_identifica-tknum.

    CALL METHOD me->set_emissor
      EXPORTING
        p_emissor = p_emissor.

    CALL METHOD me->set_parceiros
      EXPORTING
        p_parceiros = p_parceiros.

    CALL METHOD me->set_municipio_origem
      EXPORTING
        p_uf_origem        = p_uf_origem
        p_municipio_origem = p_municipio_origem
        p_dt_origem        = p_dt_origem.

    CALL METHOD me->set_municipio_termin
      EXPORTING
        p_uf_termin        = p_uf_termin
        p_municipio_termin = p_municipio_termin
        p_dt_termin        = p_dt_termin.

    CALL METHOD me->set_carga
      EXPORTING
        p_carga = p_carga.

    CALL METHOD me->set_veiculos
      EXPORTING
        p_veiculo = p_veiculos.

    CALL METHOD me->set_valores_servico
      EXPORTING
        p_valores_servico = p_valores_servico.

    CALL METHOD me->set_inf_acessorias
      EXPORTING
        p_inf_acessorias = p_inf_acessorias.

    CALL METHOD me->set_id_rota
      EXPORTING
        p_id_rota = p_zcte_identifica-rota.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.


  METHOD RESCINDIR.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    IF ( ME->ST_CIOT EQ C_2 ) OR ( ME->ST_CIOT EQ C_5 ).

      CALL METHOD ME->SET_ST_CIOT
        EXPORTING
          P_ST_CIOT = C_7.

      MESSAGE S021 WITH ME->CD_CIOT.

      CALL METHOD ME->GRAVAR.

    ELSEIF ME->ST_CIOT EQ '9'.
      MESSAGE E039 RAISING ERRO_STATUS.

    ELSE.
      MESSAGE E019 WITH ME->CD_CIOT RAISING ERRO_STATUS.
    ENDIF.

  ENDMETHOD.


  METHOD SET_CANCELADO.
    ME->CANCELADO = P_CANCELADO.
  ENDMETHOD.


  METHOD SET_CARGA.
    ME->CARGA = P_CARGA.
  ENDMETHOD.


  METHOD SET_CD_CIOT.
    ME->CD_CIOT = PCD_CIOT.
  ENDMETHOD.


  METHOD SET_DISTANCIA.
    ME->DISTANCIA = I_DISTZ.
  ENDMETHOD.


  METHOD SET_DOCNUM.
    ME->DOCNUM = P_DOCNUM.
  ENDMETHOD.


  METHOD SET_DT_CREDITO.
    ME->AT_DT_CREDITO = I_DT_CREDITO.
  ENDMETHOD.


  METHOD SET_DT_ORIGEM.
    ME->DT_ORIGEM = P_DT_ORIGEM.
  ENDMETHOD.


  METHOD SET_DT_TERMINO.
    ME->DT_TERMIN = P_DT_TERMIN.
  ENDMETHOD.


  METHOD SET_EMISSOR.
    ME->EMISSOR = P_EMISSOR.
  ENDMETHOD.


  METHOD SET_HR_CREDITO.
    ME->AT_HR_CREDITO = I_HR_CREDITO.
  ENDMETHOD.


  METHOD SET_ID_OP_VIAGEM_ADM.
    ME->ID_OP_VIAGEM_ADM = PID_OP_VIAGEM_ADM.
  ENDMETHOD.


  METHOD SET_ID_ROTA.

    ME->AT_ID_ROTA = P_ID_ROTA.

  ENDMETHOD.


  METHOD SET_INF_ACESSORIAS.
    ME->INF_ACESSORIAS = P_INF_ACESSORIAS.
  ENDMETHOD.


  METHOD SET_LINK_CARGA_PEDAGIO.
     ME->LINK_CARGA_PEDAGIO = P_LINK_CARGA_PEDAGIO.
  ENDMETHOD.


  METHOD SET_LINK_CONTRATO.
    ME->LINK_CONTRATO = P_LINK_CONTRATO.
  ENDMETHOD.


  METHOD SET_LINK_PEDAGIO.
    ME->LINK_PEDAGIO = P_LINK_PEDAGIO.
  ENDMETHOD.


  METHOD SET_LINK_RESUMO.
    ME->LINK_RESUMO = P_LINK_RESUMO.
  ENDMETHOD.


  METHOD SET_MUNICIPIO_ORIGEM.
    ME->MUNICIPIO_ORIGEM = P_MUNICIPIO_ORIGEM.
    ME->UF_ORIGEM = P_UF_ORIGEM.
    ME->DT_ORIGEM = P_DT_ORIGEM.
  ENDMETHOD.


  METHOD SET_MUNICIPIO_TERMIN.
    ME->UF_TERMIN        = P_UF_TERMIN.
    ME->MUNICIPIO_TERMIN = P_MUNICIPIO_TERMIN.
    ME->DT_TERMIN        = P_DT_TERMIN.
  ENDMETHOD.


  METHOD SET_NR_CIOT.
    ME->NR_CIOT = PNR_CIOT.
  ENDMETHOD.


  METHOD SET_NUCONTRATO.
    ME->NUCONTRATO = P_NUCONTRATO.
  ENDMETHOD.


  METHOD SET_PARCEIROS.
    ME->PARCEIROS = P_PARCEIROS.
  ENDMETHOD.


  METHOD SET_RNTRC.
    ME->RNTRC = P_RNTRC.
  ENDMETHOD.


  METHOD SET_ST_CIOT.
    ME->ST_CIOT = P_ST_CIOT.
  ENDMETHOD.


  METHOD SET_TIPO_TRANSP.
    ME->TIPO_TRANSP = P_TIPO_TRANSP.
  ENDMETHOD.


  METHOD SET_TKNUM.
    ME->TKNUM = P_TKNUM.
  ENDMETHOD.


  METHOD SET_TP_PLANO_ADMINISTRADORA.
    ME->TP_PLANO_ADMINISTRADORA = I_TP_PLANO_ADMINISTRADORA.
  ENDMETHOD.


  METHOD SET_VALORES_SERVICO.
    ME->VALORES_SERVICO = P_VALORES_SERVICO.
  ENDMETHOD.


  METHOD SET_VEICULOS.
    ME->VEICULOS = P_VEICULO.
  ENDMETHOD.


  METHOD set_zcte_ciot.

    DATA: p_parceiros TYPE zciot_parceiros_t,
          w_parceiro  TYPE zciot_contratante.

    p_ciot_ret-cd_ciot                          = p_cte_ciot-cd_ciot.

    CALL METHOD me->set_id_op_viagem_adm
      EXPORTING
        pid_op_viagem_adm = p_ciot_ret-id_op_viagem_adm.

    p_ciot_ret-id_op_viagem_adm                 = p_cte_ciot-id_op_viagem_adm.

    CALL METHOD me->set_cd_ciot
      EXPORTING
        pcd_ciot = p_ciot_ret-cd_ciot.

    p_ciot_ret-nr_ciot                          = p_cte_ciot-nr_ciot.

    CALL METHOD me->set_nr_ciot
      EXPORTING
        pnr_ciot = p_ciot_ret-nr_ciot.

    p_ciot_ret-st_ciot                          = p_cte_ciot-nr_ciot.

    CALL METHOD me->set_st_ciot
      EXPORTING
        p_st_ciot = p_ciot_ret-st_ciot.

    p_ciot_ret-rntrc                            = p_cte_ciot-rntrc.


    me->set_distancia( i_distz = p_ciot_ret-distancia ).

    p_ciot_ret-distancia                        = p_cte_ciot-distancia.

    CALL METHOD me->set_rntrc
      EXPORTING
        p_rntrc = p_ciot_ret-rntrc.

    p_ciot_ret-uf_origem                        = p_cte_ciot-uf_origem.
    p_ciot_ret-municipio_origem                 = p_cte_ciot-municipio_origem.
    p_ciot_ret-dt_origem                        = p_cte_ciot-dt_origem.

    CALL METHOD me->set_municipio_origem
      EXPORTING
        p_uf_origem        = p_ciot_ret-uf_origem
        p_municipio_origem = p_ciot_ret-municipio_origem
        p_dt_origem        = p_ciot_ret-dt_origem.

    p_ciot_ret-uf_termin                        = p_cte_ciot-uf_termin.
    p_ciot_ret-municipio_termin                 = p_cte_ciot-municipio_termin.
    p_ciot_ret-dt_termin                        = p_cte_ciot-dt_termin.

    CALL METHOD me->set_municipio_termin
      EXPORTING
        p_uf_termin        = p_ciot_ret-uf_termin
        p_municipio_termin = p_ciot_ret-municipio_termin
        p_dt_termin        = p_ciot_ret-dt_termin.

    p_ciot_ret-docnum                           = p_cte_ciot-docnum.
    p_ciot_ret-tp_plano_administradora          = p_cte_ciot-tp_plano_administradora.

    CALL METHOD me->set_docnum
      EXPORTING
        p_docnum = p_ciot_ret-docnum.

    p_ciot_ret-cancelado                        = p_cte_ciot-cancelado.

    CALL METHOD me->set_cancelado
      EXPORTING
        p_cancelado = p_ciot_ret-cancelado.

    CLEAR: w_parceiro.
    w_parceiro-tipo        = '00'.
    w_parceiro-codigo      = p_cte_ciot-ct_codigo.
    w_parceiro-nome        = p_cte_ciot-ct_nome.
    w_parceiro-razao       = p_cte_ciot-ct_razao.
    w_parceiro-cnpj        = p_cte_ciot-ct_cnpj.
    w_parceiro-cpf         = p_cte_ciot-ct_cpf.
    w_parceiro-logradouro  = p_cte_ciot-ct_logradouro.
    w_parceiro-numero      = p_cte_ciot-ct_numero.
    w_parceiro-complemento = p_cte_ciot-ct_complemento.
    w_parceiro-bairro      = p_cte_ciot-ct_bairro.
    w_parceiro-uf          = p_cte_ciot-ct_uf.
    w_parceiro-municipio   = p_cte_ciot-ct_municipio.
    w_parceiro-cep         = p_cte_ciot-ct_cep.
    APPEND w_parceiro TO p_parceiros.

    CLEAR: w_parceiro.
    w_parceiro-tipo        = '02'.
    w_parceiro-codigo      =  p_cte_ciot-dt_codigo.
    w_parceiro-nome        =  p_cte_ciot-dt_nome.
    w_parceiro-razao       =  p_cte_ciot-dt_razao.
    w_parceiro-cnpj        =  p_cte_ciot-dt_cnpj.
    w_parceiro-cpf         =  p_cte_ciot-dt_cpf.
    w_parceiro-logradouro  =  p_cte_ciot-dt_logradouro.
    w_parceiro-numero      =  p_cte_ciot-dt_numero.
    w_parceiro-complemento =  p_cte_ciot-dt_complemento.
    w_parceiro-bairro      =  p_cte_ciot-dt_bairro.
    w_parceiro-uf          =  p_cte_ciot-dt_uf.
    w_parceiro-municipio   =  p_cte_ciot-dt_municipio.
    w_parceiro-cep         =  p_cte_ciot-dt_cep.
    APPEND w_parceiro TO p_parceiros.

    CLEAR: w_parceiro.
    w_parceiro-tipo        = '03'.
    w_parceiro-codigo      = p_cte_ciot-sb_codigo.
    w_parceiro-nome        = p_cte_ciot-sb_nome.
    w_parceiro-razao       = p_cte_ciot-sb_razao.
    w_parceiro-cnpj        = p_cte_ciot-sb_cnpj.
    w_parceiro-cpf         = p_cte_ciot-sb_cpf.
    w_parceiro-logradouro  = p_cte_ciot-sb_logradouro.
    w_parceiro-numero      = p_cte_ciot-sb_numero.
    w_parceiro-bairro      = p_cte_ciot-sb_bairro.
    w_parceiro-uf          = p_cte_ciot-sb_uf.
    w_parceiro-municipio   = p_cte_ciot-sb_municipio.
    w_parceiro-cep         = p_cte_ciot-sb_cep.
    APPEND w_parceiro TO p_parceiros.

    CLEAR: w_parceiro.
    w_parceiro-tipo        = '04'.
    w_parceiro-codigo      = p_cte_ciot-cs_codigo.
    w_parceiro-nome        = p_cte_ciot-cs_nome.
    w_parceiro-razao       = p_cte_ciot-cs_razao.
    w_parceiro-cnpj        = p_cte_ciot-cs_cnpj.
    w_parceiro-cpf         = p_cte_ciot-cs_cpf.
    w_parceiro-logradouro  = p_cte_ciot-cs_logradouro.
    w_parceiro-numero      = p_cte_ciot-cs_numero.
    w_parceiro-complemento = p_cte_ciot-cs_complemento.
    w_parceiro-bairro      = p_cte_ciot-cs_bairro.
    w_parceiro-uf          = p_cte_ciot-cs_uf.
    w_parceiro-municipio   = p_cte_ciot-cs_municipio.
    w_parceiro-cep         = p_cte_ciot-cs_cep.
    APPEND w_parceiro TO p_parceiros.

    CALL METHOD me->set_parceiros
      EXPORTING
        p_parceiros = p_parceiros.

    p_ciot_ret-carga-natureza                   = p_cte_ciot-natureza.
    p_ciot_ret-carga-quantidade                 = p_cte_ciot-quantidade.
    p_ciot_ret-carga-unidade                    = p_cte_ciot-unidade.

    CALL METHOD me->set_carga
      EXPORTING
        p_carga = p_ciot_ret-carga.

    p_ciot_ret-valores_servico-vlr_combustivel  = p_cte_ciot-vlr_combustivel.
    p_ciot_ret-valores_servico-vlr_pedagio      = p_cte_ciot-vlr_pedagio.

    p_ciot_ret-id_rota = p_cte_ciot-id_rota.

    p_ciot_ret-valores_servico-tp_card_ped      = p_cte_ciot-tp_card_ped.
    p_ciot_ret-valores_servico-nr_card_ped      = p_cte_ciot-nr_card_ped.
    p_ciot_ret-valores_servico-nr_tag_strada    = p_cte_ciot-nr_tag_strada. "*-CS2024001181-16.12.2024-#160717-JT
    p_ciot_ret-valores_servico-id_rota          = p_cte_ciot-id_rota.
    p_ciot_ret-valores_servico-qtd_eixo         = p_cte_ciot-qtd_eixo.
    p_ciot_ret-valores_servico-nr_vr_xml_tipf   = p_cte_ciot-nr_vr_xml_tipf.

    p_ciot_ret-valores_servico-vlr_impostos     = p_cte_ciot-vlr_impostos.
    p_ciot_ret-valores_servico-vlr_frete        = p_cte_ciot-vlr_frete.
    p_ciot_ret-valores_servico-vlr_triagem      = p_cte_ciot-vlr_triagem.
    p_ciot_ret-valores_servico-vlr_adiantamento = p_cte_ciot-vlr_adiantamento.
    p_ciot_ret-valores_servico-vlr_saldo        = p_cte_ciot-vlr_saldo.
    p_ciot_ret-valores_servico-vlr_seguro       = p_cte_ciot-vlr_seguro.
    p_ciot_ret-valores_servico-moeda            = p_cte_ciot-moeda.

    CALL METHOD me->set_valores_servico
      EXPORTING
        p_valores_servico = p_ciot_ret-valores_servico.

    p_ciot_ret-inf_acessorias-vlr_inss          = p_cte_ciot-vlr_inss.
    p_ciot_ret-inf_acessorias-vlr_iss           = p_cte_ciot-vlr_iss.
    p_ciot_ret-inf_acessorias-vlr_sest          = p_cte_ciot-vlr_sest.
    p_ciot_ret-inf_acessorias-vlr_irpf          = p_cte_ciot-vlr_irpf.
    p_ciot_ret-inf_acessorias-vlr_iof           = p_cte_ciot-vlr_iof.
    p_ciot_ret-inf_acessorias-perc_tolerancia   = p_cte_ciot-perc_tolerancia.
    p_ciot_ret-inf_acessorias-vlr_unit_merc     = p_cte_ciot-vlr_unit_merc.
    p_ciot_ret-inf_acessorias-unid_vlr_merc     = p_cte_ciot-unid_vlr_merc.
    p_ciot_ret-inf_acessorias-vlr_unit_frete    = p_cte_ciot-vlr_unit_frete.
    p_ciot_ret-inf_acessorias-unid_vlr_frete    = p_cte_ciot-unid_vlr_frete.
    p_ciot_ret-inf_acessorias-valor_previsto    = p_cte_ciot-valor_previsto.
    p_ciot_ret-inf_acessorias-resp_pagamento    = p_cte_ciot-resp_pagamento.
    p_ciot_ret-inf_acessorias-nr_bc_banco       = p_cte_ciot-nr_bc_banco.
    p_ciot_ret-inf_acessorias-nr_bc_agencia     = p_cte_ciot-nr_bc_agencia.
    p_ciot_ret-inf_acessorias-nr_bc_conta       = p_cte_ciot-nr_bc_conta.
    p_ciot_ret-inf_acessorias-nr_bc_conta       = p_cte_ciot-nr_bc_conta.
    p_ciot_ret-inf_acessorias-peso_chegada      = p_cte_ciot-peso_chegada.

    p_ciot_ret-inf_acessorias-cnpj_recebedor    = p_cte_ciot-cnpj_recebedor.
    p_ciot_ret-inf_acessorias-flag_lib_adto     = p_cte_ciot-flag_lib_adto.
    p_ciot_ret-inf_acessorias-flag_lib_sldo     = p_cte_ciot-flag_lib_sldo.

    CALL METHOD me->set_inf_acessorias
      EXPORTING
        p_inf_acessorias = p_ciot_ret-inf_acessorias.

  ENDMETHOD.


  METHOD visualizar.

    DATA: wa_parceiros TYPE zciot_contratante,
          wa_parceiro  TYPE zcte_ciot_parce.

    "Informações CIOT
    me->get_cd_ciot( IMPORTING pcd_ciot = p_zcte_ret-cd_ciot ).
    "Informações CIOT
    me->get_id_op_viagem_adm( IMPORTING pid_op_viagem_adm = p_zcte_ret-id_op_viagem_adm ).
    "Informações CIOT
    me->get_st_ciot( IMPORTING p_st_ciot = p_zcte_ret-st_ciot ).
    "Informações CIOT
    me->get_nr_ciot( IMPORTING pnr_ciot = p_zcte_ret-nr_ciot ).
    "Registro Nacional de Tranportador Rodoviário de Carga (RNTRC)
    me->get_rntrc( IMPORTING p_rntrc = p_zcte_ret-rntrc ).
    "Numero do Contrato de Viagem
    me->get_nucontrato( IMPORTING p_nucontrato = p_zcte_ret-nucontrato ).
    "ID Viagem
    me->get_id_viagem( IMPORTING p_id_viagem = p_zcte_ret-id_viagem ).  "*#127471-18.04.2024-JT
    "DOCNUM
    me->get_docnum( IMPORTING p_docnum = p_zcte_ret-docnum ).
    "TKNUM
    me->get_tknum( IMPORTING p_tknum = p_zcte_ret-tknum ).
    "Emissor
    me->get_emissor( IMPORTING p_emissor = p_zcte_ret-emissor ).
    "Link contrato
    me->get_link_contrato( IMPORTING p_link_contrato = p_zcte_ret-link_contrato ).
    "Link resumo
    me->get_link_resumo( IMPORTING p_link_resumo = p_zcte_ret-link_resumo ).
    "Link Pedagio
    me->get_link_pedagio( IMPORTING  p_link_pedagio = p_zcte_ret-link_pedagio ).
    "Link Carga Pedagio
    me->get_link_carga_pedagio( IMPORTING  p_link_carga_pedagio = p_zcte_ret-link_carga_pedagio ).
    "Cancelado
    me->get_cancelado( IMPORTING p_cancelado = p_zcte_ret-cancelado ).
    "Distância
    me->get_distancia( IMPORTING e_distz = p_zcte_ret-distancia ).

    "Município de Origem
    me->get_municipio_origem(
      IMPORTING
        p_uf_origem        = p_zcte_ret-uf_origem
        p_municipio_origem = p_zcte_ret-municipio_origem
        p_dt_origem        = p_zcte_ret-dt_origem ).

    "Município de Término
    me->get_municipio_termin(
      IMPORTING
        p_uf_termin        = p_zcte_ret-uf_termin
        p_municipio_termin = p_zcte_ret-municipio_termin
        p_dt_termin        = p_zcte_ret-dt_termin ).

    "Parceiros
    me->get_parceiros( IMPORTING p_parceiros = p_zcte_ret-parceiros ).

    "Informações da Carga
    me->get_carga( IMPORTING p_carga = p_zcte_ret-carga ).

    "Informações veiculo
    me->get_veiculos( IMPORTING p_veiculo = p_zcte_ret-veiculos ).

    "Informações Valores de Serviço
    me->get_valores_servico( IMPORTING p_valores_servico = p_zcte_ret-valores_servico ).

    "Informações Acessórias
    me->get_inf_acessorias( IMPORTING p_inf_acessorias = p_zcte_ret-inf_acessorias ).
    me->get_tipo_transp( IMPORTING p_tipo_transp = p_zcte_ret-tipo_transp ).
    "Informações da Data de Credito / Hora Credito.
    "Rota
    me->get_dt_credito( IMPORTING e_dt_credito = p_zcte_ret-dt_credito ).
    me->get_hr_credito( IMPORTING e_hr_credito = p_zcte_ret-hr_credito ).
    me->get_id_rota( IMPORTING e_id_rota = p_zcte_ret-id_rota ).
    me->get_tp_plano_admnistradora( IMPORTING e_tp_plano_administradora = p_zcte_ret-tp_plano_administradora ).
    " Tipo do Plano de Pagamento de Frete via Administradora


    p_zcte_ciot-cd_ciot                 = p_zcte_ret-cd_ciot.
    p_zcte_ciot-id_op_viagem_adm        = p_zcte_ret-id_op_viagem_adm.
    p_zcte_ciot-nr_ciot                 = p_zcte_ret-nr_ciot.
    p_zcte_ciot-st_ciot                 = p_zcte_ret-st_ciot.
    p_zcte_ciot-rntrc                   = p_zcte_ret-rntrc.
    p_zcte_ciot-nucontrato              = p_zcte_ret-nucontrato.
    p_zcte_ciot-id_viagem               = p_zcte_ret-id_viagem.  "*#127471-18.04.2024-JT
    p_zcte_ciot-uf_origem               = p_zcte_ret-uf_origem.
    p_zcte_ciot-municipio_origem        = p_zcte_ret-municipio_origem.
    p_zcte_ciot-dt_origem               = p_zcte_ret-dt_origem.
    p_zcte_ciot-uf_termin               = p_zcte_ret-uf_termin.
    p_zcte_ciot-municipio_termin        = p_zcte_ret-municipio_termin.
    p_zcte_ciot-dt_termin               = p_zcte_ret-dt_termin.
    p_zcte_ciot-docnum                  = p_zcte_ret-docnum.
    p_zcte_ciot-tknum                   = p_zcte_ret-tknum.
    p_zcte_ciot-emissor                 = p_zcte_ret-emissor.
    p_zcte_ciot-cancelado               = p_zcte_ret-cancelado.
    p_zcte_ciot-link_contrato           = p_zcte_ret-link_contrato.
    p_zcte_ciot-link_resumo             = p_zcte_ret-link_resumo.
    p_zcte_ciot-link_pedagio            = p_zcte_ret-link_pedagio.
    p_zcte_ciot-link_carga_pedagio      = p_zcte_ret-link_carga_pedagio.
    p_zcte_ciot-tipo_transp             = p_zcte_ret-tipo_transp.
    p_zcte_ciot-id_rota                 = p_zcte_ret-id_rota.
    p_zcte_ciot-distancia               = p_zcte_ret-distancia.
    p_zcte_ciot-tp_plano_administradora = p_zcte_ret-tp_plano_administradora.

    LOOP AT p_zcte_ret-parceiros INTO wa_parceiros.
      CLEAR: wa_parceiro.
      wa_parceiro-cd_ciot             = p_zcte_ret-cd_ciot.
      wa_parceiro-tipo                = wa_parceiros-tipo.
      wa_parceiro-codigo              = wa_parceiros-codigo.
      wa_parceiro-nome                = wa_parceiros-nome.
      wa_parceiro-razao               = wa_parceiros-razao.
      wa_parceiro-cnpj                = wa_parceiros-cnpj.
      wa_parceiro-cpf                 = wa_parceiros-cpf.
      wa_parceiro-logradouro          = wa_parceiros-logradouro.
      wa_parceiro-numero              = wa_parceiros-numero.
      wa_parceiro-complemento         = wa_parceiros-complemento.
      wa_parceiro-bairro              = wa_parceiros-bairro.
      wa_parceiro-uf                  = wa_parceiros-uf.
      wa_parceiro-municipio           = wa_parceiros-municipio.
      wa_parceiro-cep                 = wa_parceiros-cep.
      wa_parceiro-rg                  = wa_parceiros-rg.
      wa_parceiro-cnh                 = wa_parceiros-cnh.
      wa_parceiro-pis                 = wa_parceiros-pis.
      wa_parceiro-uf_rg               = wa_parceiros-uf_rg.
      wa_parceiro-uf_cnh              = wa_parceiros-uf_cnh.
      wa_parceiro-fone                = wa_parceiros-fone.
      wa_parceiro-nomemae             = wa_parceiros-nomemae.
      wa_parceiro-sexo                = wa_parceiros-sexo.
      wa_parceiro-dt_nascimento       = wa_parceiros-dt_nascimento.
      wa_parceiro-org_rg              = wa_parceiros-org_rg.
      APPEND wa_parceiro TO p_zcte_parceiros.
    ENDLOOP.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '00'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-ct_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-ct_nome           = wa_parceiros-nome.
      p_zcte_ciot-ct_razao          = wa_parceiros-razao.
      p_zcte_ciot-ct_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-ct_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-ct_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-ct_numero         = wa_parceiros-numero.
      p_zcte_ciot-ct_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-ct_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-ct_uf             = wa_parceiros-uf.
      p_zcte_ciot-ct_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-ct_cep            = wa_parceiros-cep.
      p_zcte_ciot-ct_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '01'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-rm_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-rm_nome           = wa_parceiros-nome.
      p_zcte_ciot-rm_razao          = wa_parceiros-razao.
      p_zcte_ciot-rm_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-rm_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-rm_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-rm_numero         = wa_parceiros-numero.
      p_zcte_ciot-rm_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-rm_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-rm_uf             = wa_parceiros-uf.
      p_zcte_ciot-rm_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-rm_cep            = wa_parceiros-cep.
      p_zcte_ciot-rm_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '02'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-dt_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-dt_nome           = wa_parceiros-nome.
      p_zcte_ciot-dt_razao          = wa_parceiros-razao.
      p_zcte_ciot-dt_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-dt_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-dt_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-dt_numero         = wa_parceiros-numero.
      p_zcte_ciot-dt_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-dt_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-dt_uf             = wa_parceiros-uf.
      p_zcte_ciot-dt_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-dt_cep            = wa_parceiros-cep.
      p_zcte_ciot-dt_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '03'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-sb_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-sb_nome           = wa_parceiros-nome.
      p_zcte_ciot-sb_razao          = wa_parceiros-razao.
      p_zcte_ciot-sb_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-sb_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-sb_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-sb_numero         = wa_parceiros-numero.
      p_zcte_ciot-sb_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-sb_uf             = wa_parceiros-uf.
      p_zcte_ciot-sb_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-sb_cep            = wa_parceiros-cep.
      p_zcte_ciot-sb_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '04'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-cs_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-cs_nome           = wa_parceiros-nome.
      p_zcte_ciot-cs_razao          = wa_parceiros-razao.
      p_zcte_ciot-cs_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-cs_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-cs_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-cs_numero         = wa_parceiros-numero.
      p_zcte_ciot-cs_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-cs_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-cs_uf             = wa_parceiros-uf.
      p_zcte_ciot-cs_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-cs_cep            = wa_parceiros-cep.
      p_zcte_ciot-cs_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '05'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-mt_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-mt_nome           = wa_parceiros-nome.
      p_zcte_ciot-mt_razao          = wa_parceiros-razao.
      p_zcte_ciot-mt_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-mt_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-mt_numero         = wa_parceiros-numero.
      p_zcte_ciot-mt_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-mt_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-mt_uf             = wa_parceiros-uf.
      p_zcte_ciot-mt_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-mt_cep            = wa_parceiros-cep.
      p_zcte_ciot-mt_rg             = wa_parceiros-rg.
      p_zcte_ciot-mt_cnh            = wa_parceiros-cnh.
      p_zcte_ciot-mt_pis            = wa_parceiros-pis.
      p_zcte_ciot-mt_uf_rg          = wa_parceiros-uf_rg.
      p_zcte_ciot-mt_uf_cnh         = wa_parceiros-uf_cnh.
      p_zcte_ciot-mt_fone           = wa_parceiros-fone.
      p_zcte_ciot-mt_nomemae        = wa_parceiros-nomemae.
      p_zcte_ciot-mt_sexo           = wa_parceiros-sexo.
      p_zcte_ciot-mt_dt_nascimento  = wa_parceiros-dt_nascimento.
      p_zcte_ciot-mt_org_rg         = wa_parceiros-org_rg.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '99'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-tr_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-tr_nome           = wa_parceiros-nome.
      p_zcte_ciot-tr_razao          = wa_parceiros-razao.
      p_zcte_ciot-tr_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-tr_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-tr_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-tr_numero         = wa_parceiros-numero.
      p_zcte_ciot-tr_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-tr_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-tr_uf             = wa_parceiros-uf.
      p_zcte_ciot-tr_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-tr_cep            = wa_parceiros-cep.
      p_zcte_ciot-tr_fone           = wa_parceiros-fone.
    ENDIF.

    READ TABLE p_zcte_ret-parceiros INTO wa_parceiros WITH KEY tipo = '88'.
    IF sy-subrc IS INITIAL.
      p_zcte_ciot-lr_codigo         = wa_parceiros-codigo.
      p_zcte_ciot-lr_nome           = wa_parceiros-nome.
      p_zcte_ciot-lr_razao          = wa_parceiros-razao.
      p_zcte_ciot-lr_cnpj           = wa_parceiros-cnpj.
      p_zcte_ciot-lr_cpf            = wa_parceiros-cpf.
      p_zcte_ciot-lr_logradouro     = wa_parceiros-logradouro.
      p_zcte_ciot-lr_numero         = wa_parceiros-numero.
      p_zcte_ciot-lr_complemento    = wa_parceiros-complemento.
      p_zcte_ciot-lr_bairro         = wa_parceiros-bairro.
      p_zcte_ciot-lr_uf             = wa_parceiros-uf.
      p_zcte_ciot-lr_municipio      = wa_parceiros-municipio.
      p_zcte_ciot-lr_cep            = wa_parceiros-cep.
      p_zcte_ciot-lr_fone           = wa_parceiros-fone.
    ENDIF.

    p_zcte_ciot-natureza          = p_zcte_ret-carga-natureza.
    p_zcte_ciot-quantidade        = p_zcte_ret-carga-quantidade.
    p_zcte_ciot-unidade           = p_zcte_ret-carga-unidade.

    p_zcte_ciot-vlr_combustivel   = p_zcte_ret-valores_servico-vlr_combustivel.
    p_zcte_ciot-vlr_pedagio       = p_zcte_ret-valores_servico-vlr_pedagio.
    p_zcte_ciot-tp_card_ped       = p_zcte_ret-valores_servico-tp_card_ped.
    p_zcte_ciot-nr_card_ped       = p_zcte_ret-valores_servico-nr_card_ped.
    p_zcte_ciot-nr_tag_strada     = p_zcte_ret-valores_servico-nr_tag_strada. "*-CS2024001181-16.12.2024-#160717-JT
    p_zcte_ciot-id_rota           = p_zcte_ret-valores_servico-id_rota.
    p_zcte_ciot-qtd_eixo          = p_zcte_ret-valores_servico-qtd_eixo.
    p_zcte_ciot-nr_vr_xml_tipf    = p_zcte_ret-valores_servico-nr_vr_xml_tipf.

    p_zcte_ciot-vlr_impostos      = p_zcte_ret-valores_servico-vlr_impostos.
    p_zcte_ciot-vlr_frete         = p_zcte_ret-valores_servico-vlr_frete.
    p_zcte_ciot-vlr_adiantamento  = p_zcte_ret-valores_servico-vlr_adiantamento.
    p_zcte_ciot-vlr_triagem       = p_zcte_ret-valores_servico-vlr_triagem.
    p_zcte_ciot-vlr_saldo         = p_zcte_ret-valores_servico-vlr_saldo.
    p_zcte_ciot-vlr_seguro        = p_zcte_ret-valores_servico-vlr_seguro.
    p_zcte_ciot-vlr_pis_cofins    = p_zcte_ret-valores_servico-vlr_pis_cofins.
    p_zcte_ciot-vlr_inss_lucro    = p_zcte_ret-valores_servico-vlr_inss_lucro.
    p_zcte_ciot-moeda             = p_zcte_ret-valores_servico-moeda.

    p_zcte_ciot-vlr_inss          = p_zcte_ret-inf_acessorias-vlr_inss.
    p_zcte_ciot-vlr_iss           = p_zcte_ret-inf_acessorias-vlr_iss.
    p_zcte_ciot-vlr_sest          = p_zcte_ret-inf_acessorias-vlr_sest.
    p_zcte_ciot-vlr_irpf          = p_zcte_ret-inf_acessorias-vlr_irpf.
    p_zcte_ciot-vlr_iof           = p_zcte_ret-inf_acessorias-vlr_iof.
    p_zcte_ciot-perc_tolerancia   = p_zcte_ret-inf_acessorias-perc_tolerancia.
    p_zcte_ciot-vlr_unit_merc     = p_zcte_ret-inf_acessorias-vlr_unit_merc.
    p_zcte_ciot-unid_vlr_merc     = p_zcte_ret-inf_acessorias-unid_vlr_merc.
    p_zcte_ciot-vlr_unit_frete    = p_zcte_ret-inf_acessorias-vlr_unit_frete.
    p_zcte_ciot-unid_vlr_frete    = p_zcte_ret-inf_acessorias-unid_vlr_frete.
    p_zcte_ciot-valor_previsto    = p_zcte_ret-inf_acessorias-valor_previsto.
    p_zcte_ciot-resp_pagamento    = p_zcte_ret-inf_acessorias-resp_pagamento.
    p_zcte_ciot-nr_bc_banco       = p_zcte_ret-inf_acessorias-nr_bc_banco.
    p_zcte_ciot-nr_bc_agencia     = p_zcte_ret-inf_acessorias-nr_bc_agencia.
    p_zcte_ciot-nr_bc_conta       = p_zcte_ret-inf_acessorias-nr_bc_conta.
    p_zcte_ciot-nr_bc_conta       = p_zcte_ret-inf_acessorias-nr_bc_conta.
    p_zcte_ciot-peso_chegada      = p_zcte_ret-inf_acessorias-peso_chegada.
    p_zcte_ciot-cnpj_recebedor    = p_zcte_ret-inf_acessorias-cnpj_recebedor.
    p_zcte_ciot-flag_lib_adto     = p_zcte_ret-inf_acessorias-flag_lib_adto.
    p_zcte_ciot-flag_lib_sldo     = p_zcte_ret-inf_acessorias-flag_lib_sldo.
    p_zcte_ciot-tipo_carga        = p_zcte_ret-inf_acessorias-tipo_carga.

    "Informação da Data de Credito e Hora de Credito
    p_zcte_ciot-dt_credito = p_zcte_ret-dt_credito.
    p_zcte_ciot-hr_credito = p_zcte_ret-hr_credito.

  ENDMETHOD.


  METHOD get_id_viagem.
    p_id_viagem = me->id_viagem.
  ENDMETHOD.


  METHOD get_pdf_arquivos_viagem.

    DATA: l_branch        TYPE j_1bbranc_,
          l_var_chave     TYPE char32,
          l_var_msg       TYPE string,
          w_j_1bbranch    TYPE j_1bbranch,
          cl_exception    TYPE REF TO zcx_webservice,
          obj_zcl_tipcard TYPE REF TO zcl_webservice_tipcard.

    FREE: xml_viagem, e_pdf_file.

    CREATE OBJECT obj_zcl_tipcard.

*----------------------
*-- recupera CIOT
*----------------------
    SELECT SINGLE *
      FROM zcte_ciot
      INTO @DATA(_zcte_ciot)
     WHERE cd_ciot = @i_cd_ciot.

    CHECK sy-subrc = 0.

    SELECT SINGLE tdlnr
      FROM vttk
      INTO @DATA(_tdlnr)
     WHERE tknum = @_zcte_ciot-tknum.

    l_branch = _tdlnr+6(4).

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = l_branch
      IMPORTING
        wa_j_1bbranch        = w_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    CHECK sy-subrc = 0.

*----------------------
*-- monta XML
*----------------------
    me->set_ctnab( i_tag = 'consultaArquivosViagem' ).

*----------------------
*-- recupera TOKEN
*----------------------
    TRY.
        SELECT SINGLE *
          INTO @DATA(w_zlest0160)
          FROM zlest0160
         WHERE bukrs  EQ @w_j_1bbranch-bukrs
           AND branch EQ @w_j_1bbranch-branch.

        l_var_chave = obj_zcl_tipcard->chave_seguranca( i_grupo = w_zlest0160-ds_grupo ).
        me->set_ctnav( i_tag = 'chave' i_valor = l_var_chave ).

      CATCH zcx_webservice INTO cl_exception.
        l_var_msg = cl_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH l_var_msg RAISING zcx_error.
    ENDTRY.

*----------------------
*-- monta XML
*----------------------
    me->set_ctnav( i_tag = 'cnpjContratante'        i_valor = _zcte_ciot-ct_cnpj ).
    me->set_ctnav( i_tag = 'nuContrato'             i_valor = _zcte_ciot-nucontrato ).
    me->set_ctnav( i_tag = 'tipoArquivo'            i_valor = i_tipoarq ).
    me->set_ctnaf( i_tag = 'consultaArquivosViagem' ).

*----------------------
*-- obter PDF
*----------------------
    CALL FUNCTION 'Z_SD_WEBSERVICE_ADM_FILE_PDF'
      EXPORTING
        xml                        = xml_viagem
        i_docnum                   = _zcte_ciot-docnum "*-US 140617-30.12.2024-#140617-JT
      IMPORTING
        e_pdf_file                 = e_pdf_file
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        erro_web_service           = 5
        erro_xml_solicita          = 6
        OTHERS                     = 7.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zcx_error.
    ENDIF.

*----------------------
*-- visualiza PDF
*----------------------
    IF i_imprimir = abap_true.
      CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
        EXPORTING
          i_pdf                    = e_pdf_file
        EXCEPTIONS
          convert_otf_to_pdf_error = 1
          cntl_error               = 2
          OTHERS                   = 3.
    ENDIF.

  ENDMETHOD.


  METHOD set_ctnab.

    CONCATENATE xml_viagem '<' i_tag '>' INTO xml_viagem.

  ENDMETHOD.


  METHOD set_ctnaf.

    CONCATENATE xml_viagem '</' i_tag '>' INTO xml_viagem.

  ENDMETHOD.


  METHOD set_ctnav.

    CONCATENATE xml_viagem '<' i_tag '>' i_valor '</' i_tag '>' INTO xml_viagem.

  ENDMETHOD.


  METHOD set_id_viagem.
    me->id_viagem = p_id_viagem.
  ENDMETHOD.
ENDCLASS.
