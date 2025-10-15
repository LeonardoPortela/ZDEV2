class ZCX_DUE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZUSER',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  constants:
    begin of ZCX_OBG_INF_BUKRS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BUKRS .
  constants:
    begin of ZCX_OBG_INF_BRANCH,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_BRANCH .
  constants:
    begin of ZCX_OBG_INF_COD_URF_DESPACHO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_COD_URF_DESPACHO .
  constants:
    begin of ZCX_OBG_INF_COD_RA_DESPACHO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_COD_RA_DESPACHO .
  constants:
    begin of ZCX_OBG_INF_FORMA_EXPORTACAO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_FORMA_EXPORTACAO .
  constants:
    begin of ZCX_OBG_INF_SIT_ESPECIAL,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_SIT_ESPECIAL .
  constants:
    begin of ZCX_OBG_INF_MOEDA_CAMBIO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_MOEDA_CAMBIO .
  constants:
    begin of ZCX_OBG_INF_CASO_ESPEC_TRANSP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_CASO_ESPEC_TRANSP .
  constants:
    begin of ZCX_OBG_INF_OBSERV_GERAIS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_OBSERV_GERAIS .
  constants:
    begin of ZCX_OBG_INF_MOTIVO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_MOTIVO .
  constants:
    begin of ZCX_OBG_INF_CNPJ_DECLARANTE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_CNPJ_DECLARANTE .
  constants:
    begin of ZCX_OBG_INF_COD_URF_EMBARQUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_COD_URF_EMBARQUE .
  constants:
    begin of ZCX_OBG_INF_COD_RA_EMBARQUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_COD_RA_EMBARQUE .
  constants:
    begin of ZCX_OBG_INF_TIPO_DUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_TIPO_DUE .
  constants:
    begin of ZCX_OBG_INF_ITENS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '015',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITENS .
  constants:
    begin of ZCX_OBJ_NRO_DUE_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_NRO_DUE_NOT_FOUND .
  constants:
    begin of ZCX_ERRO_GRAVAR_CAB_DUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_GRAVAR_CAB_DUE .
  constants:
    begin of ZCX_ERRO_GRAVAR_ITM_DUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_GRAVAR_ITM_DUE .
  constants:
    begin of ZCX_ERRO_GRAVAR_ITM_FAT_REF,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '019',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_GRAVAR_ITM_FAT_REF .
  constants:
    begin of ZCX_OBG_INF_ID_DUE_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ID_DUE_ITEM .
  constants:
    begin of ZCX_FOUND_ID_DUE_ITEM_DUPL,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FOUND_ID_DUE_ITEM_DUPL .
  constants:
    begin of ZCX_OBG_INF_ITENS_FAT_REF,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '022',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITENS_FAT_REF .
  constants:
    begin of ZCX_AUTENTICACAO_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '024',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_AUTENTICACAO_NOT_FOUND .
  constants:
    begin of ZCX_PAR_AUTENTICACAO_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '025',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PAR_AUTENTICACAO_NOT_FOUND .
  constants:
    begin of ZCX_DUE_REGISTRADA_PORTAL,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '026',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DUE_REGISTRADA_PORTAL .
  constants:
    begin of ZCX_WEBSERVICE_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGTY',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_WEBSERVICE_NOT_FOUND .
  constants:
    begin of ZCX_XML_DUE_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '028',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_XML_DUE_NOT_FOUND .
  constants:
    begin of ZCX_DUE_REF_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '029',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DUE_REF_NOT_FOUND .
  constants:
    begin of ZCX_FALHA_COMUNICACAO_WS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '030',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FALHA_COMUNICACAO_WS .
  constants:
    begin of ZCX_STATUS_COMUNICACAO_WS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_STATUS_COMUNICACAO_WS .
  constants:
    begin of ZCX_FALHA_PROCESSAMENTO_WS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '032',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FALHA_PROCESSAMENTO_WS .
  constants:
    begin of ZCX_TIMEOUT_WS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '033',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TIMEOUT_WS .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_400,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '034',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_400 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_401,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '035',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_401 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_403,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '036',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_403 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_404,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '037',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_404 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_422,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '038',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_422 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_500,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '039',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_500 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_503,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '040',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_503 .
  constants:
    begin of ZCX_RETORNO_SISCOMEX_OTHERS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '041',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_RETORNO_SISCOMEX_OTHERS .
  constants:
    begin of ZCX_RETORNO_SISCOMEX,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '042',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_RETORNO_SISCOMEX .
  constants:
    begin of ZCX_ERRO_DESCONHECIDO_SISCOMEX,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '043',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_DESCONHECIDO_SISCOMEX .
  constants:
    begin of ZCX_OBG_INF_TP_LOC_DESPACHO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '044',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_TP_LOC_DESPACHO .
  constants:
    begin of ZCX_OBJ_INF_CNPJ_CPF_RESP_LD,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '045',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_INF_CNPJ_CPF_RESP_LD .
  constants:
    begin of ZCX_OBJ_INF_END_LOC_DESP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '048',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_INF_END_LOC_DESP .
  constants:
    begin of ZCX_OBJ_INF_LONGITUDE_LOC_DESP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '046',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_INF_LONGITUDE_LOC_DESP .
  constants:
    begin of ZCX_OBJ_INF_LATITUDE_LOC_DESP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '047',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_INF_LATITUDE_LOC_DESP .
  constants:
    begin of ZCX_ERROR_DEL_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '049',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_DEL_ITEM .
  constants:
    begin of ZCX_ERROR_DEL_ITEM_FT_REF,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '050',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_DEL_ITEM_FT_REF .
  constants:
    begin of ZCX_ERROR_DEL_ITEM_PAIS_DEST,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '051',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_DEL_ITEM_PAIS_DEST .
  constants:
    begin of ZCX_ERRO_GRAVAR_ITM_PAIS_DEST,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '052',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_GRAVAR_ITM_PAIS_DEST .
  constants:
    begin of ZCX_OBG_INF_PAISES_DESTINO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '053',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_PAISES_DESTINO .
  constants:
    begin of ZCX_OBG_INF_ITEM_NF,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '054',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_NF .
  constants:
    begin of ZCX_PAIS_INVALIDO_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '055',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PAIS_INVALIDO_ITEM .
  constants:
    begin of ZCX_PAIS_QTDE_ITEM_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '056',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PAIS_QTDE_ITEM_NOT_FOUND .
  constants:
    begin of ZCX_OBG_INF_ITEM_DADOS_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '057',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_DADOS_EXP .
  constants:
    begin of ZCX_OBG_INF_ITEM_MATERIAL,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '068',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_MATERIAL .
  constants:
    begin of ZCX_OBG_INF_ITEM_DADOS_IMP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '058',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_DADOS_IMP .
  constants:
    begin of ZCX_OBG_INF_ITEM_TP_COD_FAT,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '059',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_TP_COD_FAT .
  constants:
    begin of ZCX_OBG_INF_ITEM_MOTIVO_DISP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '060',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_MOTIVO_DISP .
  constants:
    begin of ZCX_OBG_INF_ITEM_VLR_LOC_EMB,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '061',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_VLR_LOC_EMB .
  constants:
    begin of ZCX_OBG_INF_ITEM_VLR_COND_VEND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '062',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_VLR_COND_VEND .
  constants:
    begin of ZCX_OBG_INF_ITEM_DESC_MERC,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '063',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_DESC_MERC .
  constants:
    begin of ZCX_OBG_INF_ITEM_NCM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '064',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_NCM .
  constants:
    begin of ZCX_OBG_INF_ITEM_QTDE_UE_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '065',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_QTDE_UE_EXP .
  constants:
    begin of ZCX_OBG_INF_ITEM_UC_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '066',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_UC_EXP .
  constants:
    begin of ZCX_OBG_INF_ITEM_QTDE_UC_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '067',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_QTDE_UC_EXP .
  constants:
    begin of ZCX_OBG_INF_ITEM_COD_COND_VDA,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '069',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_COD_COND_VDA .
  constants:
    begin of ZCX_OBG_INF_ITEM_PESO_LIQ_TOT,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '070',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_PESO_LIQ_TOT .
  constants:
    begin of ZCX_OBG_INF_ITEM_COD_ENQ,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '071',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_ITEM_COD_ENQ .
  constants:
    begin of ZCX_OBG_INF_REGIO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '072',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_REGIO .
  constants:
    begin of ZCX_OBG_INF_REGIO_INVALID,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '073',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_REGIO_INVALID .
  constants:
    begin of ZCX_DUE_VINC_NF_REMETENTE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '077',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_DUE_VINC_NF_REMETENTE .
  constants:
    begin of ZCX_OBG_ATRIB_NOMEACAO_TRANS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '075',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_ATRIB_NOMEACAO_TRANS .
  constants:
    begin of ZCX_DUE_VINC_REMETENTE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '098',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DUE_VINC_REMETENTE .
  constants:
    begin of ZCX_OBG_INF_TP_EXPORTACAO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '095',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_TP_EXPORTACAO .
  constants:
    begin of ZCX_VAL_QTDE_UE_ITEM_PAIS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '101',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_QTDE_UE_ITEM_PAIS .
  constants:
    begin of ZCX_VAL_UE_ITEM_PAIS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '102',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_UE_ITEM_PAIS .
  constants:
    begin of ZCX_VAL_DUPL_PAIS_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '103',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_DUPL_PAIS_ITEM .
  constants:
    begin of ZCX_VAL_PESO_LIQ_ITEM_PAIS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '109',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_PESO_LIQ_ITEM_PAIS .
  constants:
    begin of ZCX_VAL_PESO_LIQ_ITEM_FAT,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '110',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_PESO_LIQ_ITEM_FAT .
  constants:
    begin of ZCX_VAL_FAT_SEM_CCT_EXP_DIR,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '111',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_FAT_SEM_CCT_EXP_DIR .
  constants:
    begin of ZCX_OBG_INF_KUNNR,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '112',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_KUNNR .
  constants:
    begin of ZCX_LEITURA_OPUS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '114',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LEITURA_OPUS .
  constants:
    begin of ZCX_LEITURA_NAO_REALIZADA_OPUS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '115',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LEITURA_NAO_REALIZADA_OPUS .
  constants:
    begin of ZCX_N_ITENS_DUE_SEM_NFE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '118',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_N_ITENS_DUE_SEM_NFE .
  constants:
    begin of ZCX_NCM_INVALIDO_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '119',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NCM_INVALIDO_ITEM .
  constants:
    begin of ZCX_DUE_NOT_AUTORIZADA,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '120',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DUE_NOT_AUTORIZADA .
  constants:
    begin of ZCX_BLOQ_INTERNO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '121',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BLOQ_INTERNO .
  constants:
    begin of ZCX_REG_ELIMINADO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '122',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_REG_ELIMINADO .
  constants:
    begin of ZCX_LEITURA_LIB_OPUS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '114',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LEITURA_LIB_OPUS .
  constants:
    begin of ZCX_REGISTRO_DUE_NOT_FOUND,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '086',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_REGISTRO_DUE_NOT_FOUND .
  constants:
    begin of ZCX_REGISTRO_NOT_LIB_OPUS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '124',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_REGISTRO_NOT_LIB_OPUS .
  constants:
    begin of ZCX_OBG_INF_CHAVE_ACESSO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '125',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_CHAVE_ACESSO .
  constants:
    begin of ZCX_OBG_INF_NUMERO_DUE,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '126',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_NUMERO_DUE .
  constants:
    begin of ZCX_OBG_INF_NUMERO_RUC,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '127',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_NUMERO_RUC .
  constants:
    begin of ZCX_VAL_PESO_LIQ_ITEM_FAT_SCCT,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '128',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_VAL_PESO_LIQ_ITEM_FAT_SCCT .
  constants:
    begin of ZCX_DUE_SEM_BLOQ_INTERNO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '146',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DUE_SEM_BLOQ_INTERNO .
  constants:
    begin of ZCX_VINC_OPUS,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '146',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VINC_OPUS .
  constants:
    begin of ZCX_OBG_INF_PRECO_TON,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '147',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_PRECO_TON .
  constants:
    begin of ZCX_PRECO_TON_INCORRETO,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '148',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PRECO_TON_INCORRETO .
  constants:
    begin of ZCX_OBG_INF_LPCO_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '149',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_LPCO_ITEM .
  constants:
    begin of ZCX_ERROR_GRAVAR_LPCO_ITEM,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '150',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_GRAVAR_LPCO_ITEM .
  constants:
    begin of ZCX_OBG_INF_DT_REG_PORTAL,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '151',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBG_INF_DT_REG_PORTAL .
  constants:
    begin of ZCX_NF_SAP_NOT_FOUND_XML_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '167',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NF_SAP_NOT_FOUND_XML_EXP .
  constants:
    begin of ZCX_NF_EXP_NOT_FOUND_SAP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '168',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NF_EXP_NOT_FOUND_SAP .
  constants:
    begin of ZCX_QTDE_VENDA_SAP_DIF_XML_EXP,
      msgid type symsgid value 'ZDUE',
      msgno type symsgno value '169',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_QTDE_VENDA_SAP_DIF_XML_EXP .
  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYST_MSGTY optional
      !MSGNO type SYST_MSGNO optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !MSGID type SYST_MSGID optional
      !TRANSACAO type TCODE optional .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DUE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGID = MSGID .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD PUBLISHED_ERRO.

    DATA: P_MSGTY	        TYPE SYST_MSGTY,
          P_MSGTY_DISPLAY	TYPE SYST_MSGTY.


    IF I_MSGTY IS NOT INITIAL.
      P_MSGTY = I_MSGTY.
    ELSE.
      P_MSGTY = ME->MSGTY.
    ENDIF.

    IF I_MSGTY_DISPLAY IS NOT INITIAL.
      P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.
    ELSE.
      P_MSGTY_DISPLAY = ME->MSGTY.
    ENDIF.

    IF ME->TRANSACAO IS NOT INITIAL.
      IF ME->MSGV1 IS INITIAL.
        ME->MSGV1 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV2 IS INITIAL.
        ME->MSGV2 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV3 IS INITIAL.
        ME->MSGV3 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV4 IS INITIAL.
        ME->MSGV4 = ME->TRANSACAO.
      ENDIF.
    ENDIF.

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
