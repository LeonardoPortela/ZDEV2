class ZCX_FATURAMENTO definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_ERRO_CHAMADA_GET_01,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_CHAMADA_GET_01 .
  constants:
    begin of ZCX_SEM_ROMANEIO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_ROMANEIO .
  constants:
    begin of ZCX_ROM_SEM_CAVALO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ROM_SEM_CAVALO .
  constants:
    begin of ZCX_VEICULO_SEM_CAD,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VEICULO_SEM_CAD .
  constants:
    begin of ZCX_VEICULO_PROPRIETARIO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VEICULO_PROPRIETARIO .
  constants:
    begin of ZCX_REMETENTE_FORN,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_REMETENTE_FORN .
  constants:
    begin of ZCX_DOC_FISCAL,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_FISCAL .
  constants:
    begin of ZCX_SEM_DOC_MATERIAL,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_DOC_MATERIAL .
  constants:
    begin of ZCX_SEM_DOC_FATURA,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_DOC_FATURA .
  constants:
    begin of ZCX_SEM_DOC_REMESSA,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_DOC_REMESSA .
  constants:
    begin of ZCX_SEM_ROMANEIO_SAIDA,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_ROMANEIO_SAIDA .
  constants:
    begin of ZCX_SEM_ORDEM_VENDA_FRETE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_ORDEM_VENDA_FRETE .
  constants:
    begin of ZCX_SEM_DOC_TRANSPORTE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_DOC_TRANSPORTE .
  constants:
    begin of ZCX_SEM_NOTA_CTE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_NOTA_CTE .
  constants:
    begin of ZCX_VEICULO_NAO_PROPRIO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VEICULO_NAO_PROPRIO .
  constants:
    begin of ZCX_SEM_AGENTE_FRETE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_AGENTE_FRETE .
  constants:
    begin of ZCX_PROC_MDFE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PROC_MDFE .
  constants:
    begin of ZCX_PROC_MDFE_NFE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PROC_MDFE_NFE .
  constants:
    begin of ZCX_PROC_MDFE_CTE,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '019',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PROC_MDFE_CTE .
  constants:
    begin of ZCX_PROC_SEGURO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PROC_SEGURO .
  constants:
    begin of ZCX_SEM_LIFNR_EMI_ORDEM,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_LIFNR_EMI_ORDEM .
  constants:
    begin of ZCX_SEM_DOC_ZNFW,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '022',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_DOC_ZNFW .
  constants:
    begin of ZCX_DOC_ZNFW_SEM_PED,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '023',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_ZNFW_SEM_PED .
  constants:
    begin of ZCX_AVISOS_MESMA_NF,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_AVISOS_MESMA_NF .
  constants:
    begin of ZCX_DOC_ZNFW_SEM_TRANSP,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_ZNFW_SEM_TRANSP .
  constants:
    begin of ZCX_DOC_ZNFW_SEM_PARC_WL,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_ZNFW_SEM_PARC_WL .
  constants:
    begin of ZCX_PARC_SP_NOT_FOUND,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PARC_SP_NOT_FOUND .
  constants:
    begin of ZCX_PARC_LR_NOT_FOUND,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '028',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PARC_LR_NOT_FOUND .
  constants:
    begin of ZCX_PARC_Z1_NOT_FOUND,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '029',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_PARC_Z1_NOT_FOUND .
  constants:
    begin of ZCX_TP_MOV_OBRIGATORIO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '030',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TP_MOV_OBRIGATORIO .
  constants:
    begin of ZCX_TP_EXPED_OBRIGATORIO,
      msgid type symsgid value 'ZFATURAMENTO',
      msgno type symsgno value '031',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TP_EXPED_OBRIGATORIO .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGID type SYST_MSGID optional
      !MSGNO type SYST_MSGNO optional
      !MSGTY type SYST_MSGTY optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !TRANSACAO type TCODE optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_FATURAMENTO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGID = MSGID
MSGNO = MSGNO
MSGTY = MSGTY
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
TRANSACAO = TRANSACAO
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
