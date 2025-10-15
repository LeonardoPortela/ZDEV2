class ZCX_BOLETIM_PRODUCAO definition
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
    begin of ZCX_OBJ_NRO_BOL_NOT_FOUND,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_OBJ_NRO_BOL_NOT_FOUND .
  constants:
    begin of ZCX_ERROR_GRAVAR_DADOS,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_GRAVAR_DADOS .
  constants:
    begin of ZCX_DADOS_INCOMPLETOS,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DADOS_INCOMPLETOS .
  constants:
    begin of ZCX_PRODUTO_BOLETIM_NOT_PARAM,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PRODUTO_BOLETIM_NOT_PARAM .
  constants:
    begin of ZCX_DATA_NOT_INFORMED,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_NOT_INFORMED .
  constants:
    begin of ZCX_NF_VINCULADA_BOLETIM,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NF_VINCULADA_BOLETIM .
  constants:
    begin of ZCX_NF_SEM_SALDO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_NF_SEM_SALDO .
  constants:
    begin of ZCX_BOLETIM_SEM_SALDO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BOLETIM_SEM_SALDO .
  constants:
    begin of ZCX_CONTROL_SALDO_NF_NOT_FOUND,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CONTROL_SALDO_NF_NOT_FOUND .
  constants:
    begin of ZCX_NF_NAO_VINCULADA_BOLETIM,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NF_NAO_VINCULADA_BOLETIM .
  constants:
    begin of ZCX_DOCUMENTOS_GERADOS,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOCUMENTOS_GERADOS .
  constants:
    begin of ZCX_DATA_NOT_FOUND,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_NOT_FOUND .
  constants:
    begin of ZCX_ERROR_GENERATE_DOC,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '016',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERROR_GENERATE_DOC .
  constants:
    begin of ZCX_DOCUMENT_GENERATE,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOCUMENT_GENERATE .
  constants:
    begin of ZCX_DOCUMENT_NOT_GENERATE,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '018',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOCUMENT_NOT_GENERATE .
  constants:
    begin of ZCX_LOTE_NF_DIVERGENTE_BOLETIM,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LOTE_NF_DIVERGENTE_BOLETIM .
  constants:
    begin of ZCX_FILIAL_NF_DIVERGENTE_BOL,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FILIAL_NF_DIVERGENTE_BOL .
  constants:
    begin of ZCX_INCONSISTENCY_WERKS_LGORT,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_INCONSISTENCY_WERKS_LGORT .
  constants:
    begin of ZCX_BOLETIM_COM_SALDO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '022',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BOLETIM_COM_SALDO .
  constants:
    begin of ZCX_ERRO_TOT_VINCULADO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '023',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_TOT_VINCULADO .
  constants:
    begin of ZCX_ERRO_RATEIO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_RATEIO .
  constants:
    begin of ZCX_SEM_SALDO_GERACAO_DOC,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_SALDO_GERACAO_DOC .
  constants:
    begin of ZCX_DOCUMENTOS_VINCULADOS,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOCUMENTOS_VINCULADOS .
  constants:
    begin of ZCX_FILIAL_INVALIDA,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FILIAL_INVALIDA .
  constants:
    begin of ZCX_NOT_ACESS_FILIAL,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '028',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOT_ACESS_FILIAL .
  constants:
    begin of ZCX_CHANGE_FILIAL,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '029',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CHANGE_FILIAL .
  constants:
    begin of ZCX_BOL_SEM_NOTA_FISCAL,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '030',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BOL_SEM_NOTA_FISCAL .
  constants:
    begin of ZCX_ERRO_PRECO_PAUTA,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_PRECO_PAUTA .
  constants:
    begin of ZCX_BOL_APROVADO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '033',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BOL_APROVADO .
  constants:
    begin of ZCX_BOL_NOT_APROVADO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '034',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BOL_NOT_APROVADO .
  constants:
    begin of ZCX_QTDE_SI_INVALIDA,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '035',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_QTDE_SI_INVALIDA .
  constants:
    begin of ZCX_DOCUMENTOS_SUBSEQUENTES,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '036',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_DOCUMENTOS_SUBSEQUENTES .
  constants:
    begin of ZCX_ESTOQUE_NOT_FOUND,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '037',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ESTOQUE_NOT_FOUND .
  constants:
    begin of ZCX_MATERIAL_CENTRO_BLOQUEADO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '038',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_MATERIAL_CENTRO_BLOQUEADO .
  constants:
    begin of ZCX_MATERIAL_LOTE_BLOQUEADO,
      msgid type symsgid value 'ZBOLETIM_PROD',
      msgno type symsgno value '039',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_MATERIAL_LOTE_BLOQUEADO .
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



CLASS ZCX_BOLETIM_PRODUCAO IMPLEMENTATION.


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
