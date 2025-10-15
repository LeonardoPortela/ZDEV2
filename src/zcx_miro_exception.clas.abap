class ZCX_MIRO_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_FI_MM_FECHADO,
      msgid type symsgid value 'Z_FI',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_FI_MM_FECHADO .
  constants:
    begin of ZCX_VENCIMENTO_72_HORAS,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VENCIMENTO_72_HORAS .
  constants:
    begin of ZCX_VENCIMENTO_FINAL_SEMANA,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VENCIMENTO_FINAL_SEMANA .
  constants:
    begin of ZCX_VENCIMENTO_FERIADO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VENCIMENTO_FERIADO .
  constants:
    begin of ZCX_MIRO_COMPENSADA,
      msgid type symsgid value 'ZOB_MIRO',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_MIRO_COMPENSADA .
  constants:
    begin of ZCX_CAT_FISCAL_NE_NFE,
      msgid type symsgid value 'ZNFE_DISTRI',
      msgno type symsgno value '073',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CAT_FISCAL_NE_NFE .
  constants:
    begin of ZCX_FORMA_PAGAMENTO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FORMA_PAGAMENTO .
  constants:
    begin of ZCX_BANCO_EMPRESA,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BANCO_EMPRESA .
  constants:
    begin of ZCX_TIPO_PEDIDO_FATURA,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TIPO_PEDIDO_FATURA .
  constants:
    begin of ZCX_TIPO_PEDIDO_BLOQUEIO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TIPO_PEDIDO_BLOQUEIO .
  constants:
    begin of ZCX_SOMENTE_VIA_SIGAM,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SOMENTE_VIA_SIGAM .
  constants:
    begin of ZCX_DT_ATUAL,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DT_ATUAL .
  constants:
    begin of ZCX_VALOR_DIVERGENTE,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VALOR_DIVERGENTE .
  constants:
    begin of ZCX_COD_BARRA_BOLETO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_COD_BARRA_BOLETO .
  constants:
    begin of ZCX_COD_BARRA_BOLETO_ERRADO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_COD_BARRA_BOLETO_ERRADO .
  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  constants:
    begin of ZCX_DATA_BARRA_BOLETO_VCTO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_BARRA_BOLETO_VCTO .
  constants:
    begin of ZCX_FORMA_PAGA_BANCO,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_FORMA_PAGA_BANCO .
  constants:
    begin of ZCX_DT_VENCIMENTO_CALCULADA,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_DT_VENCIMENTO_CALCULADA .
  constants:
    begin of ZCX_COND_PAG_NOT_FOUND,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_COND_PAG_NOT_FOUND .
  constants:
    begin of ZCX_SEM_BANCO_EMPRESA,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_BANCO_EMPRESA .
  constants:
    begin of ZCX_DATA_VENC_CSC_FINAN,
      msgid type symsgid value 'ZMIRO',
      msgno type symsgno value '018',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_VENC_CSC_FINAN .
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



CLASS ZCX_MIRO_EXCEPTION IMPLEMENTATION.


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

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
