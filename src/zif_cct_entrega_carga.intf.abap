interface ZIF_CCT_ENTREGA_CARGA
  public .


  data AT_ENTREGA type ZSDT0179 .
  data AT_DOCUMENTOS type ZSDT0180_T .
  data AT_DOCUMENTOS_CARGA type ZSDT0181_T .
  data CK_ALTEROU type CHAR01 .
  data AT_TOKEN type ref to ZCL_TOKEN_SISCOMEX .

  methods SET_CABECALHO
    importing
      !I_ZSDT0179 type ZSDT0179 .
  methods SET_TOKEN
    importing
      !I_TOKEN type ref to ZCL_TOKEN_SISCOMEX .
  methods ENVIAR_ENTREGA
    returning
      value(R_ENVIADA) type CHAR01
    exceptions
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_TIMEOUT
      ZCX_ENVIO .
  methods MONTA_XML
    returning
      value(R_XML) type STRING .
  methods GRAVAR_REGISTRO
    returning
      value(R_GRAVOU) type CHAR01 .
  methods VALIDAR_REGISTRO
    returning
      value(R_VALIDOU) type CHAR01 .
  methods LIMPAR_REGISTRO .
  methods REGISTRO_ENTREGA
    importing
      !I_REGISTRO_ENTREGA type ZDE_REGISTRO_ENTREGA
      !I_ZSDT0170 type ZSDT0170_T optional .
  methods ADD_DOCUMENTO
    importing
      !I_ZSDT0180 type ZSDT0180 .
  methods ADD_DOCUMENTO_CARGA
    importing
      !I_ZSDT0181 type ZSDT0181
    returning
      value(R_ADICIONADO) type CHAR01 .
  methods SET_CNPJ_RESPONSAVEL
    importing
      !I_CNPJ_RESPONSAVEL type STCD1 optional .
  methods CANCELAR_ENTREGA
    returning
      value(R_CANCELADA) type CHAR01 .
endinterface.
