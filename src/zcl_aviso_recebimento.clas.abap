class ZCL_AVISO_RECEBIMENTO definition
  public
  inheriting from ZCL_DELIVERY
  final
  create public .

public section.

  methods CRIAR_AVISO_RECEBIMENTO
    importing
      !I_PARTICAO_LOTE type CHAR01 default ' '
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DELIVERY .
  methods SET_CABECALHO
    importing
      !I_CABECALHO type ZDE_BAPI_REMESSA_CAB .
  methods SET_FORNECEDOR
    importing
      !I_LIFNR type LIFNR .
  methods SET_ROUTE
    importing
      !I_ROUTE type ROUTE .
  methods SET_PEDIDO_COMPRA
    importing
      !I_EBELN type EBELN .
  methods SET_DATA_LANCAMENTO
    importing
      !I_BLDAT type BLDAT .
  methods SET_ITEM
    importing
      !I_ITEM type ZDE_BAPI_REMESSA_ITEM .
  methods SET_LC_COLETA_PARID
    importing
      !I_PARID type J_1BPARID .
  methods SET_SP_FRETE_PARID
    importing
      !I_PARID type J_1BPARID .
  methods SET_LC_COLETA_PARTYP
    importing
      !I_PARTYP type J_1BPARTYP .
  methods SET_SP_FRETE_PARTYP
    importing
      !I_PARTYP type J_1BPARTYP .
  methods SET_LC_ENTREGA_PARID
    importing
      !I_PARID type J_1BPARID .
  methods SET_LC_ENTREGA_PARTYP
    importing
      !I_PARTYP type J_1BPARTYP .
  methods SET_VALOR_NOTA
    importing
      !I_VALOR_NOTA type J_1BBASE .
  methods SET_XBLNR
    importing
      !I_XBLNR type XBLNR_V1 .
  methods SET_SHIP_POINT
    importing
      !I_SHIP_POINT type VSTEL .
  methods SET_VFDAT
    importing
      !I_VFDAT type VFDAT .
  methods SET_WL_FORN_MERC_PARID
    importing
      !I_PARID type J_1BPARID .
  methods SET_WL_FORN_MERC_PARTYP
    importing
      !I_PARTYP type J_1BPARTYP .
  methods SET_CH_REFERENCIA
    importing
      !I_CH_REFERENCIA type ZCH_REF .
  methods SET_CK_ROUTE_VALIDAR
    importing
      !I_CK_ROUTE_VALIDAR type CHAR01 .
  methods SET_CHAVE_NFE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E .
protected section.
private section.

  constants I_TIPO_DELIVERY type CHAR01 value 'E' ##NO_TEXT.
  data I_CABECALHO type ZDE_BAPI_REMESSA_CAB .
  data I_ITEMS type ZDE_BAPI_REMESSA_ITEM_T .
  data I_PARCEIROS type ZDE_BAPI_REMESSA_PARCEIROS_T .
ENDCLASS.



CLASS ZCL_AVISO_RECEBIMENTO IMPLEMENTATION.


  METHOD CRIAR_AVISO_RECEBIMENTO.

    IF ME->I_CABECALHO-XBLNR IS INITIAL.
      "001  Deve ser Informado o Número de Documento de Referência!
      RAISE EXCEPTION TYPE ZCX_DELIVERY
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DELIVERY=>ZCX_NUMERO_REFERENCIA-MSGID
                            MSGNO = ZCX_DELIVERY=>ZCX_NUMERO_REFERENCIA-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_DELIVERY=>ZCX_NUMERO_REFERENCIA-MSGID
          MSGNO  = ZCX_DELIVERY=>ZCX_NUMERO_REFERENCIA-MSGNO.
    ENDIF.

    CALL METHOD ME->CRIAR
      EXPORTING
        I_TIPO_DELIVERY = ME->I_TIPO_DELIVERY
        I_CABECALHO     = ME->I_CABECALHO
        I_ITEMS         = ME->I_ITEMS
        I_PARCEIROS     = ME->I_PARCEIROS
*       I_GERAR_MOVIMENTO              = 'X'
        I_PARTICAO_LOTE = I_PARTICAO_LOTE
      RECEIVING
        R_GEROU         = R_GEROU.

  ENDMETHOD.


  METHOD SET_CABECALHO.
    ME->I_CABECALHO = I_CABECALHO.
  ENDMETHOD.


  method SET_CHAVE_NFE.
    ME->I_CABECALHO-CHAVE_NFE = I_CHAVE_NFE.
  endmethod.


  method SET_CH_REFERENCIA.

    ME->I_CABECALHO-CH_REFERENCIA = I_CH_REFERENCIA.

  endmethod.


  METHOD SET_CK_ROUTE_VALIDAR.
    ME->I_CABECALHO-CK_ROUTE_VALIDAR = I_CK_ROUTE_VALIDAR.
  ENDMETHOD.


  method SET_DATA_LANCAMENTO.
    ME->I_CABECALHO-BLDAT = I_BLDAT.
    ME->I_CABECALHO-LFDAT = I_BLDAT.
  endmethod.


  method SET_FORNECEDOR.
    ME->I_CABECALHO-LIFNR = I_LIFNR.
  endmethod.


  METHOD SET_ITEM.
    APPEND I_ITEM TO ME->I_ITEMS.
  ENDMETHOD.


  method SET_LC_COLETA_PARID.
    ME->I_CABECALHO-LC_COLETA_PARID = I_PARID.
  endmethod.


  method SET_LC_COLETA_PARTYP.
    ME->I_CABECALHO-LC_COLETA_PARTYP = I_PARTYP.
  endmethod.


  method SET_LC_ENTREGA_PARID.
    ME->I_CABECALHO-LC_ENTREGA_PARID = I_PARID.
  endmethod.


  METHOD SET_LC_ENTREGA_PARTYP.
    ME->I_CABECALHO-LC_ENTREGA_PARTYP = I_PARTYP.
  ENDMETHOD.


  METHOD SET_PEDIDO_COMPRA.
    ME->I_CABECALHO-EBELN = I_EBELN.
  ENDMETHOD.


  method SET_ROUTE.
    ME->I_CABECALHO-ROUTE = I_ROUTE.
  endmethod.


  METHOD SET_SHIP_POINT.
    ME->I_CABECALHO-SHIP_POINT = I_SHIP_POINT.
  ENDMETHOD.


  method SET_SP_FRETE_PARID.
    ME->I_CABECALHO-SP_FRETE_PARID = I_PARID.
  endmethod.


  method SET_SP_FRETE_PARTYP.
    ME->I_CABECALHO-SP_FRETE_PARTYP = I_PARTYP.
  endmethod.


  METHOD SET_VALOR_NOTA.
    ME->I_CABECALHO-VL_TOTAL_FATURA = I_VALOR_NOTA.
  ENDMETHOD.


  METHOD SET_VFDAT.
    ME->I_CABECALHO-VFDAT = I_VFDAT.
  ENDMETHOD.


  method SET_WL_FORN_MERC_PARID.

    ME->I_CABECALHO-WL_FORN_MERC_PARID = I_PARID.

  endmethod.


  method SET_WL_FORN_MERC_PARTYP.

    ME->I_CABECALHO-WL_FORN_MERC_PARTYP = I_PARTYP.

  endmethod.


  METHOD SET_XBLNR.
    ME->I_CABECALHO-XBLNR = I_XBLNR.
  ENDMETHOD.
ENDCLASS.
