interface ZIF_NFE_XML
  public .


  class-data AT_NFE_XML type ref to ZIF_NFE_XML .
  data IB_NFE type ZIB_NFE_DIST_TER .
  data IB_NFE_ITM type ZIB_NFE_DIST_ITM_T .

  class-methods GET_INSTANCE
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML .
  methods SET_REGISTRO
    importing
      !I_CHAVE type ZDE_CHAVE_DOC_E
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
  methods GET_VALIDAR
    importing
      !I_MATERIAL type MATNR optional
      !I_GRUPO_MATERIAL type MATKL optional
    exporting
      !E_VALIDACAO type ZDE_VAL_XML_NFE
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML .
  methods SET_CLEAR
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML .
  methods GET_CK_NFE_LOCALIZADO
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
  methods GET_CK_NFE_AUTORIZADA
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
  methods GET_CK_CFOP_EXPORTACAO
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
  methods GET_CK_NCM_EXPORTACAO
    importing
      !I_MATERIAL type MATNR optional
      !I_GRUPO_MATERIAL type MATKL optional
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
  methods GET_WEB_SERVICE_VALIDAR
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_NFE_XML) type ref to ZIF_NFE_XML
    raising
      ZCX_NFE_XML .
endinterface.
