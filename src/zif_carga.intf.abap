interface ZIF_CARGA
  public .


  data CK_ALTEROU type CHAR01 .
  data CK_EXECUTAR_MANUTENCAO_ENTRADA type CHAR01 .
  data CK_EXECUTAR_REVERSAO_ENTRADA type CHAR01 .
  constants ST_STATUS_ABERTO type ZDE_STATUS_CARGA value 'AB' ##NO_TEXT.
  constants ST_STATUS_FECHADO type ZDE_STATUS_CARGA value 'FE' ##NO_TEXT.
  constants ST_STATUS_CONFERIDO type ZDE_STATUS_CARGA value 'CO' ##NO_TEXT.
  constants ST_STATUS_CANCELADA type ZDE_STATUS_CARGA value 'CA' ##NO_TEXT.
  constants ST_GMO_NAO_TESTADO type ZDE_IN_GMO value '0' ##NO_TEXT.
  constants ST_GMO_POSITIVO type ZDE_IN_GMO value '1' ##NO_TEXT.
  constants ST_GMO_NEGATIVO type ZDE_IN_GMO value '2' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_UMIDADE type ZDE_TP_CARACTERISTICA value '01' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_IMPUREZA type ZDE_TP_CARACTERISTICA value '02' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_AVARIADO type ZDE_TP_CARACTERISTICA value '03' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_ARDIDO type ZDE_TP_CARACTERISTICA value '04' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_QUEBRADO type ZDE_TP_CARACTERISTICA value '05' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_ESVERDEADO type ZDE_TP_CARACTERISTICA value '06' ##NO_TEXT.
  constants ST_TP_CARACT_CLASS_CARUNCHADO type ZDE_TP_CARACTERISTICA value '07' ##NO_TEXT.
  constants ST_TP_FRETE_EXW type ZDE_TP_FRETE value 'EXW' ##NO_TEXT.
  constants ST_TP_FRETE_FCA type ZDE_TP_FRETE value 'FCA' ##NO_TEXT.
  constants ST_TP_FRETE_FAS type ZDE_TP_FRETE value 'FAS' ##NO_TEXT.
  constants ST_TP_FRETE_FOB type ZDE_TP_FRETE value 'FOB' ##NO_TEXT.
  constants ST_TP_FRETE_CFR type ZDE_TP_FRETE value 'CFR' ##NO_TEXT.
  constants ST_TP_FRETE_CIF type ZDE_TP_FRETE value 'CIF' ##NO_TEXT.
  constants ST_TP_FRETE_CPT type ZDE_TP_FRETE value 'CPT' ##NO_TEXT.
  constants ST_TP_FRETE_CIP type ZDE_TP_FRETE value 'CIP' ##NO_TEXT.
  constants ST_TP_FRETE_DAF type ZDE_TP_FRETE value 'DAF' ##NO_TEXT.
  constants ST_TP_FRETE_DES type ZDE_TP_FRETE value 'DES' ##NO_TEXT.
  constants ST_TP_FRETE_DEQ type ZDE_TP_FRETE value 'DEQ' ##NO_TEXT.
  constants ST_TP_FRETE_DDU type ZDE_TP_FRETE value 'DDU' ##NO_TEXT.
  constants ST_TP_FRETE_DDP type ZDE_TP_FRETE value 'DDP' ##NO_TEXT.
  constants ST_MODEL_FISCAL_PAPEL type J_1BMODEL value '01' ##NO_TEXT.
  constants ST_MODEL_FISCAL_ELETRONICO type J_1BMODEL value '55' ##NO_TEXT.
  constants ST_TP_PRODUTO_CARGA_GRANEL type ZDE_TP_PRODUTO_CARGA value '01' ##NO_TEXT.
  constants ST_TP_PRODUTO_CARGA_ALGODAO type ZDE_TP_PRODUTO_CARGA value '02' ##NO_TEXT.
  data CARGA type ZSDT0001CG .
  data CLASSIFICACAO type ZSDT0001CL .
  data RESULTADO type ZDE_ZSDT0001RS_T .
  data RESULTADO_AVARIADO type ZDE_ZSDT0001RS_03_T .
  data DOCUMENTO_FISCAL type ZDE_ZSDT0001NT_T .
  data DOCUMENTO_FISCAL_IMP_RET type ZDE_ZSDT0001NT_RET_T .
  data CLASSIFICACAO_NOTAS type ZDE_ZSDT0001CL_T .
  data ORDEM_VENDA type ZDE_ZSDT0001OV_T .
  data PEDIDO_COMPRA type ZDE_ZSDT0001EK_T .
  data SOLICITACOES type ZDE_ZSDT0001ACB_T .
  data TAKE_UP type ZDE_ZSDT0001TK_T .
  data BLOCOS type ZDE_ZSDT0001FD_T .
  constants ST_STATUS_ESTORNO_SEM type ZDE_STATUS_ESTORNO value ' ' ##NO_TEXT.
  constants ST_STATUS_ESTORNO_SOLICITADO type ZDE_STATUS_ESTORNO value '1' ##NO_TEXT.
  constants ST_STATUS_ESTORNO_EXECUTADO type ZDE_STATUS_ESTORNO value '2' ##NO_TEXT.
  constants ST_STATUS_ESTORNO_ERRO type ZDE_STATUS_ESTORNO value '3' ##NO_TEXT.
  constants ST_STATUS_ESTORNO_BLOQUEIO type ZDE_STATUS_ESTORNO value '4' ##NO_TEXT.
  constants ST_INTERFACE_AVISO type ZE_INTERFACE value '19' ##NO_TEXT.
  constants ST_INTERFACE_MIGO type ZE_INTERFACE value '20' ##NO_TEXT.
  constants ST_INTERFACE_MIRO type ZE_INTERFACE value '21' ##NO_TEXT.
  data AT_NAO_GERAR_BLOQUEIOS type CHAR01 .
  data AT_TIPO_FRETE_ORDEM_VENDA type ZDE_TP_FRETE .
  data AT_MANUTENCAO type CHAR01 .
  class-data AT_CARGA type ref to ZIF_CARGA .
  data SOLICITACAO_MANUTENCAO type ZSDT0001ACB .
  constants ST_STATUS_MANUT_ABERTO type ZDE_ST_SOL_AJUSTE value '0' ##NO_TEXT.
  constants ST_STATUS_MANUT_ENVIADO type ZDE_ST_SOL_AJUSTE value '1' ##NO_TEXT.
  constants ST_STATUS_MANUT_APROVADO type ZDE_ST_SOL_AJUSTE value '2' ##NO_TEXT.
  constants ST_STATUS_MANUT_RECUSADA type ZDE_ST_SOL_AJUSTE value '3' ##NO_TEXT.
  constants ST_STATUS_MANUT_CANCELADA type ZDE_ST_SOL_AJUSTE value '4' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_CO type ZDE_TP_TRANG value 'CO' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_T1 type ZDE_TP_TRANG value 'T1' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_T2 type ZDE_TP_TRANG value 'T2' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_D1 type ZDE_TP_TRANG value 'D1' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_D2 type ZDE_TP_TRANG value 'D2' ##NO_TEXT.
  constants ST_TP_TRANSGENIASE_PA type ZDE_TP_TRANG value 'PA' ##NO_TEXT.
  constants ST_TP_ACEITE_MANUT_FILIAL type ZDE_TP_ROM_ACEITE_MANUT value 'LN' ##NO_TEXT.
  constants ST_TP_ACEITE_MANUT_FISCAL type ZDE_TP_ROM_ACEITE_MANUT value 'FI' ##NO_TEXT.
  constants ST_TP_ACEITE_MANUT_COMERCIAL type ZDE_TP_ROM_ACEITE_MANUT value 'CO' ##NO_TEXT.
  constants ST_RS_ACEITE_MANUT_RECUSADA type ZDE_TP_ROM_RESPOSTA value 'R' ##NO_TEXT.
  constants ST_RS_ACEITE_MANUT_APROVADA type ZDE_TP_ROM_RESPOSTA value 'A' ##NO_TEXT.
  constants ST_RS_ACEITE_MANUT_ESPERA type ZDE_TP_ROM_RESPOSTA value 'W' ##NO_TEXT.
  constants ST_RS_ACEITE_MANUT_NAO_GERA type ZDE_TP_ROM_RESPOSTA value 'S' ##NO_TEXT.
  data CK_DIGITADO_UMIDADE type CHAR01 .
  data CK_DIGITADO_IMPUREZA type CHAR01 .
  data CK_DIGITADO_ARDIDO type CHAR01 .
  data CK_DIGITADO_AVARIADO type CHAR01 .
  data CK_DIGITADO_QUEBRADO type CHAR01 .
  data CK_DIGITADO_ESVERDEADO type CHAR01 .
  data CK_DIGITADO_CARUNCHADO type CHAR01 .
  constants ST_TP_CARGA_ENTRADA_FOB type ZDE_TP_CARGA value '00' ##NO_TEXT.
  constants ST_TP_CARGA_SAIDA_OPUS type ZDE_TP_CARGA value '01' ##NO_TEXT.
  constants ST_TP_CARGA_SAIDA_ENT_FOB type ZDE_TP_CARGA value '02' ##NO_TEXT.
  data ORDEM_VENDA_ROMANEIOS type ZDE_ZSDT0001OVRO_T .
  data ROMANEIOS type ZDE_ZSDT0001_T .
  data CK_DIGITADO_AVA_ARQ type CHAR01 .
  data CK_DIGITADO_AVA_QUE type CHAR01 .
  data CK_DIGITADO_AVA_MOF type CHAR01 .
  data CK_DIGITADO_AVA_PIC type CHAR01 .
  data CK_DIGITADO_AVA_FER type CHAR01 .
  data CK_DIGITADO_AVA_GER type CHAR01 .
  data CK_DIGITADO_AVA_ARD type CHAR01 .
  data CK_DIGITADO_AVA_GES type CHAR01 .
  constants ST_TP_CARACT_SUB_ARQ type CHAR01 value '1' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_QUE type CHAR01 value '2' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_MOF type CHAR01 value '3' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_PIC type CHAR01 value '4' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_FER type CHAR01 value '5' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_GER type CHAR01 value '6' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_ARD type CHAR01 value '7' ##NO_TEXT.
  constants ST_TP_CARACT_SUB_GES type CHAR01 value '8' ##NO_TEXT.
  constants ST_GRUPO_ALGODAO_PLUMA type MATKL value '700140' ##NO_TEXT.
  constants ST_ACTION_GRAO type CHAR4 value 'GRAO' ##NO_TEXT.
  constants ST_ACTION_ALGO type CHAR4 value 'ALGO' ##NO_TEXT.
  constants ST_GRUPO_CAROCO_ALGODAO_TER type MATKL value '700320' ##NO_TEXT.
  data ST_VALIDA_ENTRADA_FOB type ZSTRUCT_VALIDA_ENTRADA_FOB . "EUDR Comentado 16-01-2025
  data ST_RET_ENTRADA_FOB type ZDE_RET_ENTRADA_FOB_EUD . "EUDR Comentado 16-01-2025
  constants ST_AUART_ZIND type AUART value 'ZIND' ##NO_TEXT.
  constants ST_DOCUMENT_TYPE_OV type CHAR02 value 'OV' ##NO_TEXT.
  constants ST_DOCUMENT_TYPE_PD type CHAR02 value 'PD' ##NO_TEXT.

  class-methods GERAR_CARGA_SAIDA
    importing
      !I_CARGA_ENTRADA type ref to ZIF_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_ORDEM_CARREGAMENTO .
  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA .
  class-methods GET_VERIFICA_EXISTE_SAIDA
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_EXCLUIR_ROMANEIO type CHAR01 optional
    raising
      ZCX_CARGA
      ZCX_CADASTRO .
  class-methods SET_NOTA_ENTRADA_PROPRIA
    importing
      !I_DOCNUM type J_1BDOCNUM
    raising
      ZCX_CADASTRO
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_JOB
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_ORDEM_CARREGAMENTO .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_CARGA .
  class-methods DESVINCULAR_ROMANEIO_CARGA
    importing
      value(I_ID_CARGA) type ZDE_ID_CARGA optional
      value(I_CH_REFERENCIA) type ZCH_REF optional
    raising
      ZCX_CARGA .
  methods SET_DADOS_NOTA_PROPRIA
    importing
      value(I_DOC_FISCAL) type ZSDT0001NT
    exporting
      value(E_DADOS) type ZDE_PROCESSO
      value(E_OBJKEY_NP) type AWKEY
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ERROR .
  methods SET_EMITIR_DOC_SAIDAS
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_JOB .
  methods SET_GERAR_REMESSAS
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods ADD_NOTA_FISCAL
    importing
      !I_NOTA type ZDE_ZSDT0001NT_ALV
    exporting
      !E_NOTA type ZDE_ZSDT0001NT_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS .
  methods SET_BLOQUEAR_ROMANEIOS_SAIDA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_DESBLOQUEAR_ROMANEIOS_SAID
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods ADD_ORDEM_VENDA
    importing
      !I_ORDEM_VENDA type ZDE_ZSDT0001OV_ALV
    exporting
      !E_ORDEM_VENDA type ZDE_ZSDT0001OV_ALV
      !E_CARGA type ZDE_ZSDT0001CG_ALV
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ENQUEUE_ROMANEIO
    importing
      !I_CHAVE_REFERENCIA type ZCH_REF
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods BLOQUEAR_ENTRADA
    importing
      !I_OBJ_KEY type AWKEY
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_DENQUEUE_ROMANEIO
    importing
      !I_CHAVE_REFERENCIA type ZCH_REF
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods DESBLOQUEAR_ENTRADA
    importing
      !I_OBJ_KEY type AWKEY
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods EXCLUIR_NOTA_FISCAL
    importing
      !I_NOTA type ZDE_ZSDT0001NT_ALV
    exporting
      !E_NOTAS type ZDE_ZSDT0001NT_ALV_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods EXCLUIR_REGISTRO
    exporting
      !E_EXCLUIU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods FREE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CALCULAR_SUBTOTAL
    importing
      !I_PESO_BRUTO type ZDE_NM_PESO_BRUTO
      !I_PESO_TARA type ZDE_NM_PESO_TARA
    exporting
      !E_PESO_SUBTOTAL type ZDE_NM_PESO_SUBTOTAL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CHECK_JOB_EXECUCAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CHECK_JOB_EXECUCAO_ESTORNO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CK_CARGA_SEM_SOLIC_MANUT
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CK_EXCLUIR_ROMANEIO_SAIDA
    importing
      !I_CK_OPUS type CHAR01 default ABAP_FALSE
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_DOCUMENTO_ENT_ESTORNADO
    importing
      !I_OBJ_KEY type AWKEY
    exporting
      !R_ESTORNADO type CHAR01
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA .
  methods GET_FACTORY_TP_TRANSGENIA
    importing
      !I_CLASSIFICACAO type ZSDT0001CL
    exporting
      !E_TP_TRANSGENIA type ZDE_TP_TRANG
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_INFO_ALV_APRESENTACAO
    exporting
      !E_APRESENTACAO type ZDE_CARGA_APRESENTACAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_ORDEM_CARREGAMENTO
      ZCX_CARGA .
  methods GET_INFO_ALV_APRESENTACAO_LOG
    importing
      !I_DT_REGISTRO type ZDE_DT_REGISTRO
      !I_HR_REGISTRO type ZDE_HR_REGISTRO
      !I_US_REGISTRO type ZDE_US_REGISTRO
    exporting
      !E_APRESENTACAO type ZDE_CARGA_APRESENTACAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_ORDEM_CARREGAMENTO
      ZCX_CARGA .
  methods GET_INFO_MESSAGEM_ESTORNO
    importing
      !I_OBJ_KEY type AWKEY
      !I_INTERFACE type ZE_INTERFACE
      !I_MESSAGE_V1 type SYMSGV optional
      !I_MESSAGE_V2 type SYMSGV optional
      !I_MESSAGE_V3 type SYMSGV optional
      !I_MESSAGE_V4 type SYMSGV optional
    exporting
      !E_MENSAGEM type ZOB_MENSAGEM
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_INFO_ORDEM_VENDA
    importing
      !I_ORDEM_VENDA type VBELN
    exporting
      !E_ORDEM type ZDE_ZSDT0001OV_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_INFO_PEDIDO_COMPRA
    importing
      !I_PEDIDO_COMPRA type EBELN
    exporting
      !E_PEDIDO type ZDE_ZSDT0001EK_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_INFO_PLACA
    importing
      !I_TIPO_FRETE type ZDE_TP_FRETE
      !I_PLACA type ZPC_VEICULO
      !I_VALIDAR type CHAR01 default ' '
      !I_TRACAO type CHAR01 default ' '
    exporting
      !E_ZLEST0002 type ZLEST0002
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_LOGS_HISTORICO
    exporting
      !E_LOGS type ZDE_LOG_REGISTRO_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_MENS_INTERFACE_ENTRADA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    exporting
      !E_MENSAGENS type ZDE_OB_MENSAGEM_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NEW_ID_CARGA
    exporting
      !E_ID_CARGA type ZDE_ID_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NEW_ID_CLASSIFICAO
    exporting
      value(E_ID_CLASSIFICACAO) type ZDE_ID_CLASSIFICACAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NEW_ID_ENTRADA_ESTOQUE
    exporting
      !E_ID_CARGA type ZDE_ID_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NEW_ID_SOLICITACAO_MANUT
    exporting
      !E_ID_SOLICITACAO type ZDE_ID_SOL_AJUSTE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NOTA_FORNECEDOR_IE
    importing
      !I_STCD3 type STCD3
    exporting
      !E_NOTA type ZDE_ZSDT0001NT_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CLASSIFICAO_NOTAS
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
      !I_ID_CLASSIFICACAO type ZDE_ID_CLASSIFICACAO
    exporting
      !E_CLASSIFICACAO type ZSDT0001CL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_PARTINER_ORDEM_VENDA
    importing
      !I_ORDEM_VENDA type VBELN
      !I_FUNCAO_PARTINER type PARVW
    exporting
      !E_PARTINER type VBPA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_PEDIDO_COMPRA
    importing
      !I_ID_NOTA type ZDE_ID_NOTA
    exporting
      !E_INFO_PEDIDO type ZDE_INFO_PEDIDO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_PEDIDO_COMPRA_GERAL
    importing
      !I_ID_BUKRS type BUKRS
      !I_ID_ENTRADA type ZDE_ID_ENTRADA
      !I_ID_MOD_FISCAL type J_1BMODEL
      !I_ID_BRANCH type ZDE_BRANCH_RECEB
      !I_NR_SAFRA type ZDE_NR_SAFRA
      !I_ID_PRODUTO type MATNR
      !I_ID_FORNECEDOR type LIFNR
    exporting
      !E_INFO_PEDIDO type ZDE_INFO_PEDIDO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_RATEIA_DESCONTOS
    importing
      !I_DESCONTOS type ZDE_ZSDT0001RS_T
      !I_PESO_LIQUIDO type ZDE_NM_PESO_LIQUIDO
    exporting
      !E_PESO_SUBTOTAL type ZDE_NM_PESO_SUBTOTAL
      !E_RATEIO type ZDE_ZSDT0001RS_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_RECUPERAR_ENTRADA
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA .
  methods GET_CLASSIFICACAO
    exporting
      !E_REGISTRO type ZSDT0001CL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_REGISTRO
    exporting
      !E_REGISTRO type ZSDT0001CG
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_RESULT_DESC_CLASSIFICACAO
    importing
      !I_CLASSIFICACAO type ZDE_PES_RESULTADO_CLASS
    exporting
      !E_RESULTADO type ZDE_PES_RESULTADO_CLASS_R
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_ROMANEIO_ENTRADA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
      !I_ID_NOTA type ZDE_ID_NOTA optional
      !I_FORCE type CHAR01 default ABAP_FALSE
      !I_CANCELADOS type CHAR01 default ABAP_FALSE
    exporting
      !E_ROMANEIOS type ZDE_ZSDT0001_T
      !E_ROMANEIOS_CANCEL type ZDE_ZSDT0001_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_ROMANEIO_SAIDA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    exporting
      !E_ROMANEIOS type ZDE_ZSDT0001_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_TP_STATUS
    exporting
      !E_TP_STATUS type ZDE_STATUS_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_VALIDAR_NOTA_FISCAL
    changing
      !I_NOTA_FISCAL type ZSDT0001NT
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GRAVAR_REGISTRO
    exporting
      !E_GRAVOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_ORDEM_CARREGAMENTO .
  methods LIMPAR_REGISTRO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods NOVO_REGISTRO
    importing
      !I_ID_BRANCH type ZDE_BRANCH_RECEB optional
    exporting
      !E_CARGA_RECEBIMENTO type ZDE_ZSDT0001CG_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods PESQUISAR
    importing
      !I_FILTROS type ZDE_FILTRO_ZSDT0001CG
    exporting
      !E_REGISTROS type ZDE_ZSDT0001CG_ALV_T
      !E_PESQUISOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods PESQUISAR_SOLICITACAO_MANUT
    importing
      !I_FILTROS type ZDE_FILTRO_ZSDT0001ACG
    exporting
      !E_REGISTROS type ZDE_ZSDT0001ACG_ALV_T
      !E_PESQUISOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SEND_CARGA_TO_OPUS
    exporting
      !E_CODE type I
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SEND_ESTORNO_CARGA_TO_OPUS
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ABRIR
    importing
      !I_NR_SAFRA type ZDE_NR_SAFRA optional
      !I_ID_BUKRS type ZDE_BUKRS_RECEB optional
      !I_ID_BRANCH type ZDE_BRANCH_RECEB optional
      !I_TIPO_PRODUTO type ZDE_TP_PRODUTO_CARGA optional
    exporting
      !E_CARGA_RECEBIMENTO type ZDE_ZSDT0001CG_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ORDEM_CARREGAMENTO
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_SOFT_EXPERT_WORKFLOW
      ZCX_JOB .
  methods SET_CANCELAR
    exporting
      !E_CANCELOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_CARREGAMENTO
      ZCX_ORDEM_VENDA .
  methods SET_CANCELAR_SOLIC_MANUT
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_CLASSIFICACAO
    importing
      !I_CLASSIFICACAO type ZSDT0001CL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_CARGA
    importing
      !I_CARGA type ZDE_ZSDT0001CG_ALV
    exporting
      !E_CARGA_RECEBIMENTO type ZDE_ZSDT0001CG_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_CONFERIDO
    importing
      !I_PROXIMO_PASSO_AUTOMATICO type CHAR01 default ABAP_FALSE
    exporting
      !E_CONFERIU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_CADASTRO
      ZCX_ORDEM_VENDA
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_MIRO_EXCEPTION
      ZCX_ORDEM_CARREGAMENTO
      ZCX_JOB
      ZCX_ERROR .
  methods SET_CONFERIR_SOLIC_MANUT
    exporting
      !E_APROVACAO_AUTOMATICA type CHAR01
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_CRIA_MANUTENCAO
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ORDEM_CARREGAMENTO
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_DENQUEUE
    importing
      !I_CARGA type ZDE_ID_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ENQUEUE
    importing
      !I_CARGA type ZDE_ID_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_FECHAR
    exporting
      !E_FECHOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ORDEM_CARREGAMENTO
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_JOB
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_GERAR_ENTRADA_ESTOQUE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_PARCEIROS
      ZCX_MIRO_EXCEPTION
      ZCX_JOB .
  methods SET_GERAR_ESTORNO_ESTOQUE
    importing
      !I_DT_ESTORNO type ZDE_DT_MOVIMENTO optional
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods SET_GERAR_ROMANEIO_ENTRADA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_CADASTRO
      ZCX_PARCEIROS .
  methods SET_GERAR_ROMANEIO_SAIDA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_CADASTRO
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA .
  methods SET_ID_CARGA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ID_CLASSIFICACAO
    importing
      !I_ID_CLASSIFICACAO type ZDE_ID_CLASSIFICACAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA .
  methods SET_ID_SOLIC_MANUT
    importing
      !I_ID_SOLICITACAO type ZDE_ID_SOL_AJUSTE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_LIMPA_ROMANEIO_SAIDA
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_LOGS_ALTERACAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_OBSERVACAO_NOTA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
      !I_ID_NOTA type ZDE_ID_NOTA
      !I_DS_OBSERVACAO type ZDE_OBSERVACAO
    exporting
      !E_NOTA type ZDE_ZSDT0001NT_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ORDEM_CARREGAMENTO
    importing
      !I_NR_SAFRA type ZDE_NR_SAFRA
      !I_ID_BUKRS type ZDE_BUKRS_RECEB
      !I_ID_BRANCH type ZDE_BRANCH_RECEB
      !I_NR_ORDEM type ZDE_NR_ORDEM
      !I_VBELN type VBELN_VA optional
    exporting
      !E_ORDEM_CARRGAMENTO type ZDE_ZSDT0001OD_ALV
    changing
      !I_CARGA_ALV type ZDE_ZSDT0001CG_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_ORDEM_CARREGAMENTO
      ZCX_CARGA
      ZCX_PARCEIROS .
*    EXPORTING
*      !e_ordem_carrgamento TYPE zde_zsdt0001od_alv
  methods SET_ORDEM_CARREGAMENTO_EXT
    importing
      !I_NR_SAFRA type ZDE_NR_SAFRA
      !I_ID_BUKRS type ZDE_BUKRS_RECEB
      !I_ID_BRANCH type ZDE_BRANCH_RECEB
      !I_NR_ORDEM type ZDE_NR_ORDEM
      !I_VBELN type VBELN_VA optional
    changing
      !I_CARGA_ALV type ZDE_ZSDT0001CG_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_ORDEM_CARREGAMENTO
      ZCX_CARGA
      ZCX_PARCEIROS .
  methods SET_ORDEM_VENDA
    importing
      !I_ORDEM_VENDA type VBELN
    exporting
      !E_CARGA type ZDE_ZSDT0001CG_ALV
    changing
      !C_ZDE_ZSDT0001OV_ALV type ZDE_ZSDT0001OV_ALV optional
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS .
  methods SET_PESOS_NOTAS
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
      !I_ID_NOTA type ZDE_ID_NOTA
      !I_PESO_SUBTOTAL type ZDE_NM_PESO_SUBTOTAL
      !I_PESO_LIQUIDO type ZDE_NM_PESO_LIQUIDO
      !I_SEM_CONSULTA type CHAR01 default ABAP_FALSE
    exporting
      !E_NOTA type ZDE_ZSDT0001NT_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_PROCESSAR_ENTRADA
    importing
      !I_SOMENTE_ATUALIZAR type CHAR01 optional
    exporting
      !E_GEROU_ENTRADA type CHAR01
      !E_MSG_ERRO type STRING
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CADASTRO
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_JOB
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods SET_PROCESSAR_ESTORNO
    exporting
      !E_ESTORNOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CADASTRO
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_JOB .
  methods SET_REGISTRO
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
      !I_NO_ENQUEUE type CHAR01 default ' '
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_REGISTRO_MANUTENCAO
    importing
      !I_ID_SOLICITACAO type ZDE_ID_SOL_AJUSTE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_TIPO_FRETE_ORDEM_VENDA
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_TRANSGENIA
    changing
      !I_IN_GMO type ZDE_IN_GMO
      !I_NR_RESULTADO_01 type ZDE_NR_RESULTADO_01
      !I_NR_RESULTADO_02 type ZDE_NR_RESULTADO_02
      !I_NR_RESULTADO_03 type ZDE_NR_RESULTADO_03 optional
      !I_NR_RESULTADO_04 type ZDE_NR_RESULTADO_04 optional
      !I_NR_RES_RR1_RR2 type ZDE_NR_RES_RR1_RR2
      !I_IN_GMO_03 type ZDE_IN_GMO_03
      !I_IN_SRR_ORIGEM_PARTIC type ZDE_IN_SRR_ORIGEM_PARTIC
      !I_ID_OUTRO_PARTIC type ZDE_ID_OUTRO_PARTIC
      !I_IN_SRR_DECLARADO type ZDE_IN_SRR_DECLARADO
      !I_IN_TESTE_SRR type ZDE_IN_TESTE_SRR
      !I_IN_SRR_DECLARADO_2 type ZDE_IN_SRR_DECLARADO_2
      !I_IN_TESTE_SRR_2 type ZDE_IN_TESTE_SRR_2
      !I_ID_CLASSIFICADORA type ZDE_ID_CLASSIFICADORA
      !I_ID_CK_CLASS_DEST type ZDE_CK_CLASS_DEST optional
      !I_TP_TRANSGENIA type ZDE_TP_TRANG
      !I_NR_NOTA type ZDE_NR_NOTA optional
      !I_CLASSIFICACAO type ZDE_ID_CLASSIFICACAO optional
      !I_NR_FITA type ZDE_NR_FITA optional
    returning
      value(R_INSTANCIA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VALIDAR_EXCLUSAO
    exporting
      !E_VALIDOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VALIDAR_REGISTRO
    importing
      !I_PROCESSO type CHAR100 optional
    exporting
      !E_VALIDOU type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_ORDEM_CARREGAMENTO .
  methods VALIDAR_REGISTRO_ORDEM_VENDA
    importing
      !I_PROCESSO type CHAR100 optional
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ORDEM_VENDA .
  methods VALIDAR_REGISTRO_PEDIDO_COMPRA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS .
  methods VALIDA_ATRIBUTO_ALTERAVEL
    importing
      !I_CAMPO type NAME_FELD
      !I_MODELO_FISCAL type J_1BMODEL optional
      !I_ID_ENTRADA type ZDE_ID_ENTRADA optional
      !I_ID_EMPRESA type BUKRS optional
    exporting
      !E_PERMITIDO type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VERIF_ESTORNO_PENDENTE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VERIF_ORDEM_CARREGAMENTO
    importing
      value(I_ORDEM) type ZDE_ZSDT0001OD_ALV optional
      !I_CK_VERIFICAR_CARGA type CHAR01 optional
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_ORDEM_CARREGAMENTO .
  methods SET_ACEITE_SOLI_MANUTENCAO
    importing
      !I_TP_APROVACAO type ZDE_TP_ROM_ACEITE_MANUT
      !I_TP_RESPOSTA type ZDE_TP_ROM_RESPOSTA
      !I_MOTIVO_RESPOSTA type ZDE_DS_ROM_RESPOSTA optional
      !I_SOMENTE_PROCESSAR type CHAR01 optional
    exporting
      !E_MENSSAGEM_RETORNO type STRING
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS
      ZCX_ORDEM_VENDA
      ZCX_ORDEM_CARREGAMENTO
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_JOB
      ZCX_MIRO_EXCEPTION .
  methods SET_PROCESSAR_SOBRA_FOB
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_JOB .
  methods GET_CK_GERA_SOBRA
    exporting
      !E_ZMMT0074 type ZMMT0074
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CK_SAIDA_AUTOMATICA
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_PROCESSAR_ESTORNO_SOBRA
    importing
      !I_DT_ESTORNO type ZDE_DT_MOVIMENTO optional
    returning
      value(R_INSTANCE) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_JOB
      ZCX_MIRO_EXCEPTION .
  methods SET_PROCESSAR_MANUTENCAO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_CADASTRO
      ZCX_PARCEIROS
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_JOB
      ZCX_ORDEM_VENDA
      ZCX_MIRO_EXCEPTION
      ZCX_ORDEM_CARREGAMENTO .
  methods VERIF_ALTERACAO_MANUT_ROMANEIO
    exporting
      !E_CK_ACEITE_FILIAL type ZDE_CK_ACEITE_FILIAL
      !E_CK_ACEITE_COMERCIAL type ZDE_CK_ACEITE_FISCAL
      !E_CK_ACEITE_FISCAL type ZDE_CK_ACEITE_COMERCIAL
      !E_CARGA_ORIGINAL type ref to ZIF_CARGA
      !E_ALTERACOES type ZDE_ALTERACOES_CARGA
      !E_OBS_ALTERACAO type STRING
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CABECALHO_CARGA
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    exporting
      !E_ZSDT0001CG type ZSDT0001CG
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CK_SOL_AJUSTE_NAO_PROC
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VERIF_PESO_NOTAS
    raising
      ZCX_CARGA .
  methods VERIF_SALDO_ORDEM_VENDA
    raising
      ZCX_CARGA .
  methods VERIF_TICKET_PESAGEM
    raising
      ZCX_CARGA .
  methods VERIF_BLOQ_LOTE_MATERIAL_WAIT
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_GERA_IMPOSTO_NOTA
    importing
      !I_NOTA type ZSDT0001NT
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA
      ZCX_PARCEIROS .
  methods SET_GERAR_FATURA
    exporting
      !E_LES_ROM type ZDE_LES_SAIDA_ZSDT0001_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_GERAR_TRANSPORTE
    importing
      value(I_LES_ROM) type ZDE_LES_SAIDA_ZSDT0001_T
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CK_SUB_CARAC_AVA_MATERIAL
    importing
      !I_SUB_CARAC_AVA type ZDE_TP_SUB_CARAC_AVARIADO
    exporting
      !E_CK_CARAC type CHAR01
      !E_CK_OBRIGATORIO type CHAR01
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_NAME_JOB_SAIDA_AUTOMATICA
    exporting
      !E_NAME type BTCJOB
    returning
      value(R_CARGA) type ref to ZIF_CARGA .
  methods GET_CK_DOC_TRANSPORTE_EXISTE
    exporting
      !E_VTTK type VTTK
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ESTORNA_FRETE_ENTRADA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ADD_BLOCO
    importing
      !I_BLOCO type ZDE_ZSDT0001FD_ALV
    exporting
      !E_BLOCO type ZDE_ZSDT0001FD_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ADD_TAKE_UP
    importing
      !I_TAKEUP type ZDE_ZSDT0001TK_ALV
    exporting
      !E_TAKEUP type ZDE_ZSDT0001TK_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VERIF_CK_SALDO_TAKEUP_BLOCO
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods VERIF_CK_PLUMA_NOTA_FISCAL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_EXC_TAKE_UP
    importing
      !I_TAKEUP type ZDE_ZSDT0001TK_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_EXCLUIR_BLOCO
    importing
      !I_BLOCO type ZDE_ZSDT0001FD_ALV
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_AJUSTAR_RAT_DESC_GERAL
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_PRODUTOR_FILIAL_SAFRA
    importing
      !ID_PRODUTOR type ZDE_ID_PRODUTOR
      !ID_NR_SAFRA type ZDE_NR_SAFRA
      !ID_BUKRS type ZDE_BUKRS_RECEB
      !ID_BRANCH type ZDE_BRANCH_RECEB
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_ADD_FORN_FILIAL_SAFRA
    importing
      !I_ID_PRODUTOR type ZDE_ID_PRODUTOR
      !I_ID_NR_SAFRA type ZDE_NR_SAFRA
      !I_ID_BUKRS type ZDE_BUKRS_RECEB
      !I_ID_BRANCH type ZDE_BRANCH_RECEB
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_AJUSTAR_RAT_ORDEM_VENDA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_VOLUME_ORDEM_VENDA
    importing
      !I_VBELN type VBELN_VA
      !I_VOLUME type VOLUM_AP
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_EXCLUIR_ORDEM_VENDA
    importing
      !I_VBELN type VBELN_VA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_LOCAL_ENTREGA
    importing
      !I_ID_LOCAL_ENTREGA type ZDE_ID_LOCAL_ENTREGA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_INFO_FRETE_WITHOUT_OV
    importing
      !I_ID_PROPRIETARIO type ZDE_PROP_VEICULO
      !I_DS_PROPRIETARIO type ZDE_DS_PROP_VEICULO
      !I_DS_PLACA_TRATOR type ZDE_PLACA_TRATOR
      !I_DS_PLACA_REBOQ_1 type ZDE_PLACA_REBOQUE1
      !I_DS_PLACA_REBOQ_2 type ZDE_PLACA_REBOQUE2
      !I_DS_PLACA_REBOQ_3 type ZDE_PLACA_REBOQUE3
      !I_ID_MOTORISTA type ZDE_ID_MOTORISTA
      !I_DS_MOTORISTA type ZDE_DS_MOTORISTA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_AGENTE_FRETE
    importing
      !I_ID_AGENT_FRETE type ZDE_ID_AGENT_FRETE
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_INFO_FRETE_WITHOUT_OC
    importing
      !I_ID_PROPRIETARIO type ZDE_PROP_VEICULO
      !I_DS_PROPRIETARIO type ZDE_DS_PROP_VEICULO
      !I_DS_PLACA_TRATOR type ZDE_PLACA_TRATOR
      !I_DS_PLACA_REBOQ_1 type ZDE_PLACA_REBOQUE1
      !I_DS_PLACA_REBOQ_2 type ZDE_PLACA_REBOQUE2
      !I_DS_PLACA_REBOQ_3 type ZDE_PLACA_REBOQUE3
      !I_ID_MOTORISTA type ZDE_ID_MOTORISTA
      !I_DS_MOTORISTA type ZDE_DS_MOTORISTA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_GERAR_NOTA_PROPRIA
    raising
      ZCX_ERROR
      ZCX_CARGA .
  methods GET_NEW_ID_NOTA_PROPRIA
    exporting
      value(E_ID_NOTA_PROPRIA) type ZDE_ID_NOTA_PROPRIA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods SET_CANCELAR_NOTA_PROPRIA
    raising
      ZCX_ERROR
      ZCX_CARGA .
  methods SET_VALIDAR_SAFRA
    importing
      !I_SAFRA type ZDE_NR_SAFRA
      !I_ACAO type CHAR4
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods GET_CONTRATO_ORDEM_VENDA
    importing
      !I_ORDEM_VENDA type VBELN
    exporting
      !E_ID_CONTRATO type VBELN_VA
    returning
      value(R_CARGA) type ref to ZIF_CARGA
    raising
      ZCX_CARGA .
  methods CHECK_ROMANEIOS_CARGA_OPUS
    importing
      !I_ID_CARGA type ZDE_ID_CARGA
    raising
      ZCX_CARGA .
  methods ADD_CLASSIFICACAO
    importing
      !I_NOTA type ZDE_ZSDT0001NT_ALV .
  methods CHECK_OB_VALIDA_ENTRADA_FOB
    raising
      ZCX_CARGA .
endinterface.
