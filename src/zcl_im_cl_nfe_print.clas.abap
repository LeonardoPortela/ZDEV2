class ZCL_IM_CL_NFE_PRINT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_CL_NFE_PRINT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_CL_NFE_PRINT .

  methods VERIFICA_ICMS_NAO_TRIBUTADO
    importing
      !P_BUKRS type BUKRS
      !P_BRANCH type J_1BBRANC_
      !P_PARTYP type J_1BPARTYP
      !P_PARID type J_1BPARID
      !P_TAXLW1 type J_1BTAXLW1
      !P_PAIS type LAND1
      !P_REGIO type REGIO
    exporting
      !P_NAO_TRIBUTADO type CHAR01
      !P_CK_LEI_FISCAL type CHAR01
      !P_RATE type J_1BTXRATE .
  methods VERIFICA_ICMS_DESONERADO
    importing
      !P_BUKRS type BUKRS
      !P_BRANCH type J_1BBRANC_
      !P_PARTYP type J_1BPARTYP
      !P_PARID type J_1BPARID
      !P_TAXLW1 type J_1BTAXLW1
      !P_PAIS type LAND1
      !P_REGIO type REGIO
    exporting
      !P_DESONERADO type CHAR01
      !P_RATE type J_1BTXRATE
      !P_CK_LEI_FISCAL type CHAR01 .
  methods VERIFICA_ICMS_RO_RO
    importing
      !P_BUKRS type BUKRS
      !P_BRANCH type J_1BBRANC_
      !P_PARTYP type J_1BPARTYP
      !P_PARID type J_1BPARID
      !P_TAXLW1 type J_1BTAXLW1
    exporting
      !P_DESONERADO type CHAR01
      !P_RATE type J_1BTXRATE .
  methods SELECT_NF_EXPORTACAO_RECUSA
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_OBSERVACAO type STRING .
  class-methods GET_RESP_TECNICO
    importing
      !I_UF type REGIO
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    returning
      value(R_RESPONSAVEL) type J1B_NF_XML_TEC_RESP .
  methods SELECI_KNVI
    importing
      !P_DOCNUM type J_1BDOCNUM
      !P_PARID type KUNNR
      !P_EMPRESA type BUKRS
    exporting
      !P_ZONA_FRANCA type CHAR1 .
  methods GET_DADOS_CTE_ANTERIOR
    importing
      !IT_IDDOCANTELE type J_1BCTE_INT_IDDOCANTELE_TAB
    returning
      value(R_CTEPROC) type EDOC_BR_CTE_IF_ROOT .
  class-methods GET_UNID_TRIBUT_ITEM_NF
    importing
      !I_LIN type J_1BNFLIN
    exporting
      !E_UNID_TRIB type GEWEI
      !E_QTD_TRIB type BRGEW_15
      !E_MSG_ERROR type CHAR200 .
  methods RECUPERA_ATRIBUTO
    importing
      !IV_NOME_ATTR type STRING
      !IV_MATNR type MARA-MATNR
      !IV_CHARG type J_1BNFLIN-CHARG
    exporting
      value(EV_VALOR) type STRING .
protected section.
*"* protected components of class ZCL_IM_CL_NFE_PRINT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_CL_NFE_PRINT
*"* do not include other source files here!!!

  methods SELECT_ZSDT0004
    importing
      !P_DOCNUM type J_1BDOCNUM optional
    exporting
      !T_ZSDT0004 type ZSDT0004_T .
  methods SELECT_ZSDT0001
    importing
      !P_VBELN type VBAK-VBELN optional
      !P_WERKS type VBAP-WERKS optional
      !P_BUKRS type VBAK-BUKRS_VF optional
      !P_VGBEL type VBAK-VGBEL optional
    exporting
      !T_ZSDT0001 type ZSDT0001_T .
  methods SELECT_VBAK
    importing
      !P_AUBEL type VBRP-AUBEL optional
    exporting
      !T_VBAK type ZNFE_VBAK_T .
  methods SELECT_VBAP
    importing
      !P_VBELN type VBAK-VBELN optional
      !P_POSNR type J_1BNFLIN-ITMNUM optional
    exporting
      !T_VBAP type ZNFE_VBAP_T .
  methods SELECT_VBKD
    importing
      !P_VBELN type VBAK-VBELN optional
      !P_POSNR type J_1BNFLIN-ITMNUM optional
    exporting
      !T_VBKD type ZNFE_VBKD_T .
  methods SELECT_VBRP
    importing
      !P_REFKEY type J_1BNFLIN-REFKEY optional
      !P_POSNR type J_1BNFLIN-ITMNUM optional
    exporting
      !T_VBRP type ZNFE_VBRP_T .
  methods SELECT_LIPS
    importing
      !P_VGBEL type VBAK-VGBEL optional
      !P_POSNR type VBAP-POSNR optional
    exporting
      !T_LIPS type ZNFE_LIPS_T .
  methods SELECT_ZDCO_PRODUTOR
    importing
      !P_VBELN type VBAK-VBELN optional
    exporting
      !T_PRODUTOR type ZDCO_PRODUTOR_T .
  methods SELECT_ZDCO_NF_ENTRADA
    importing
      !P_NU_DCO type ZDCO_NF_ENTRADA-NU_DCO optional
    exporting
      !T_NF_ENTRADA type ZDCO_NF_ENTRADA_T .
  methods SELECT_J1BNFDOC
    importing
      !P_DOCNUM_EN type J_1BNFDOC-DOCNUM optional
    exporting
      !P_PARID type J_1BNFDOC-PARID
      !P_NFENUM type J_1BNFDOC-NFENUM .
  methods SELECT_LFA1
    importing
      !P_LIFNR type LFA1-LIFNR optional
    exporting
      !S_LFA1 type ZNFE_LFA1 .
  methods SELECT_T006A
    importing
      !P_MSEHI type T006A-MSEHI optional
    exporting
      !S_T006A type ZNFE_T006A .
  methods SELECT_KONV
    importing
      !P_KNUMV type KONV-KNUMV optional
      !P_KPOSN type KONV-KPOSN optional
      !P_KSCHL type KONV-KSCHL optional
    exporting
      !T_KONV type ZNFE_KONV_T .
  methods SELECIONA_DADOS
    importing
      !P_DOCNUM type J_1BDOCNUM optional
      !P_ITMNUM type J_1BITMNUM optional
      !P_REFKEY type J_1BREFKEY optional
    exporting
      !P_NFENUM type J_1BNFDOC-NFENUM
      !S_LFA1 type ZNFE_LFA1
      !S_T006A type ZNFE_T006A
      !T_ZSDT0004 type ZSDT0004_T
      !T_ZSDT0001 type ZSDT0001_T
      !T_VBAK type ZNFE_VBAK_T
      !T_VBAP type ZNFE_VBAP_T
      !T_VBRP type ZNFE_VBRP_T
      !T_LIPS type ZNFE_LIPS_T
      !T_PRODUTOR type ZDCO_PRODUTOR_T
      !T_NF_ENTRADA type ZDCO_NF_ENTRADA_T
      !T_ZSDT0006 type ZSDT0006_T
      !S_LIKP type ZNFE_LIKP
      !T_ZSDT0294 type ZSDT0294_T .
  methods SELECT_ZSDT0006
    importing
      !P_AUART type VBAK-AUART optional
    exporting
      !T_ZSDT0006 type ZSDT0006_T .
  methods FUNCTION_PARTNER
    importing
      !P_VBELN type VBAK-VBELN optional
    exporting
      !T_VBPAVB type VBPAVB_TAB
      !T_SADRVB type SADRVB_TAB .
  methods SELECT_LIKP
    importing
      !P_VBELN type LIKP-VBELN
    exporting
      !S_LIKP type ZNFE_LIKP .
  methods FUNCTION_CLASSIFICATION
    importing
      !P_MATNR type MARA-MATNR optional
      !P_CHARG type J_1BNFLIN-CHARG optional
    exporting
      value(P_TEXTOS) type STRING .
  methods SELECT_KNA1
    importing
      !P_KUNNR type KNA1-KUNNR optional
    exporting
      !S_KNA1 type ZNFE_KNA1 .
  methods SELECI_KNB1
    importing
      !P_PARID type KUNNR
      !P_BUKRS type BUKRS
    exporting
      !S_KVERM type KVERM .
  methods SELECT_SEGURO
    importing
      !P_DOCNUM type J_1BDOCNUM
      !P_WAERK type WAERK
      !P_DATA type J_1BDOCDAT
      !P_ITMNUM type J_1BITMNUM
      !P_REFKEY type J_1BREFKEY
      !P_C1_UF type REGIO
    exporting
      !E_OBSERVACAO type STRING .
  methods SELECT_EXPORTACAO
    importing
      !IN_DOC type J_1BNFDOC
      !IN_PARTNER type VBPAVB_TAB
    exporting
      !E_OBSERVACAO type STRING .
  methods BLOCK_U
    importing
      !IN_DOC type J_1BNFDOC
    exporting
      !E_COBRANCA type J1B_NF_XML_U1 .
  methods SELECT_NF_PRODUTOR
    importing
      !P_DOCNUM type J_1BDOCNUM
      !P_ITMNUM type J_1BITMNUM
    exporting
      !E_OBSERVACAO type STRING .
  methods BLOCK_W
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_VBELN type VBELN_VA optional
    exporting
      !E_COMEX type CHAR01
      !E_CPAIS type CHAR04
      !E_UFEMBARQ type CHAR2
      !E_XLOCEMBARQ type CHAR_60
      !E_OBSERVACAO type STRING
      !E_XLOCDESPACHO type J_1BNFE_DISPATCH .
  methods BLOCK_H2
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_ITMNUM type J_1BITMNUM optional
      !I_PREENCHE_DI type CHAR01 optional
    exporting
      !O_IMPORTACAO type CHAR01
      !O_OUT_ITEM type J1B_NF_XML_BADI_ITEM
      !O_OUT_IMPORT type J1B_NF_XML_H4_TAB
      !O_CPAIS type CHAR04 .
  methods SELECT_NF_REMESSA_FORM_LOTE
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_OBSERVACAO type STRING .
  methods SELECT_COLETA_PRODUTOR
    importing
      !P_ITEM_NF type J_1BNFLIN
    exporting
      !E_OBSERVACAO type STRING .
  methods DETERMINAR_PARCEIROS
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER
    changing
      !C_OUT_HEADER type J1B_NF_XML_BADI_HEADER .
  methods READ_PARTNER
    importing
      !I_PARVW type J_1BPARVW
      !I_J_1BNFNAD_TAB type J_1BNFNAD_TAB
    changing
      !C_J_1BINNAD type J_1BINNAD
    returning
      value(R_OK) type CHAR01 .
  methods ADD_DOC_REFERENCE
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER
      !I_DOC_REFERENCE type J1B_NF_XML_B12 optional
      !I_CHAVE_NFE type CHAR44 optional .
  methods FILL_DOCS_REFERENCE
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER .
  methods ADD_REF_NF
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER
      !I_DOCNUM type J_1BDOCNUM .
  methods ADD_REF_NF_CCT
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER
      !I_ZLEST0147 type ZLEST0147 .
  methods SET_DADOS_TRANSPORTADORA
    importing
      !I_CLEAR type CHAR01 optional
    changing
      !I_XML_HEADER type J1B_NF_XML_BADI_HEADER .
  methods FORMATA_TEXTO_VEICULO
    importing
      !I_CTE_TRANS type ZCTE_TRANS
    returning
      value(R_TEXTO) type STRING .
  methods FILL_REFERENCE_NF_FIM_ESP
    importing
      !I_XML_HEADER type J1B_NF_XML_HEADER
      !I_ITEM type J_1BNFLIN .
  methods GET_DADOS_VEICULO_NFE
    importing
      !IN_XML_HEADER type J1B_NF_XML_HEADER
    exporting
      !E_CODIGO_MOT type LIFNR
      !E_CPF_MOT type STCD2
      !E_NOME_MOT type NAME1_GP
      !E_PLACA type CHAR07
      !E_PLACA_UF type CHAR02 "#EC CI_USAGE_OK[2689873]
      !E_PLACA1 type CHAR07
      !E_PLACA1_UF type CHAR02 "#EC CI_USAGE_OK[2689873]
      !E_PLACA2 type CHAR07
      !E_PLACA2_UF type CHAR02 "#EC CI_USAGE_OK[2689873]
      !E_PLACA3 type CHAR07
      !E_PLACA3_UF type CHAR02 "#EC CI_USAGE_OK[2689873]
      !E_RNTRC type CHAR20 .
  methods GET_TEXTOS_CONTRIB_UF
    importing
      !I_LIN type J_1BNFLIN
    returning
      value(R_TEXTOS) type STRING .
  methods GET_TEXTOS_NF_INDUSTRIALIZACAO
    importing
      !I_LIN type J_1BNFLIN
    returning
      value(R_TEXTOS) type STRING .
  methods GET_DADOS_TRANSPORTADORA
    returning
      value(R_LFA1_TRANSP) type LFA1 .
  methods FILL_DOC_VOLUMES
    importing
      !IN_DOC type J_1BNFDOC
      !IN_PARTNER type VBPAVB_TAB
    exporting
      !E_VOLUME type J1B_NF_XML_T6 .
  methods SELECT_ZSDT0294
    importing
      !P_AUART type VBAK-AUART optional
    exporting
      !T_ZSDT0294 type ZSDT0294_T .
  methods SELECT_MSEG
    importing
      !I_REFKEY type J_1BREFKEY
      !I_REFITM type J_1BREFITM
    returning
      value(R_MSEG) type MB_MSEG .
  methods SELECT_RSEG
    importing
      !I_REFKEY type J_1BREFKEY
      !I_REFITM type J_1BREFITM
    returning
      value(R_RSEG) type J_1B_TT_RSEG .
ENDCLASS.



CLASS ZCL_IM_CL_NFE_PRINT IMPLEMENTATION.


  method ADD_DOC_REFERENCE.

    FIELD-SYMBOLS: <XMLR>          TYPE J1B_NF_XML_B12,
                   <XMLR_TAB>      TYPE J1B_NF_XML_B12_TAB.

    DATA: WL_DOC_REF TYPE J1B_NF_XML_B12.

    ASSIGN ('(SAPLJ_1B_NFE)XMLR_TAB')  TO <xmlr_tab>.

    CHECK <XMLR_TAB> IS ASSIGNED.

    IF I_CHAVE_NFE IS NOT INITIAL.

      CLEAR: WL_DOC_REF.

      WL_DOC_REF-DOCNUM      = I_XML_HEADER-DOCNUM.
      WL_DOC_REF-B12_REFNFE  = I_CHAVE_NFE.

      APPEND WL_DOC_REF TO <XMLR_TAB>.

    ELSE.

      APPEND I_DOC_REFERENCE TO <XMLR_TAB>.

    ENDIF.




  endmethod.


  METHOD ADD_REF_NF.

    DATA: WL_DOC_REF     TYPE J1B_NF_XML_B12,
          WA_INFO_V      TYPE LFA1,
          WA_INFO_C      TYPE KNA1,
          V_REGIO_PARID  TYPE KNA1-REGIO,
          V_CHAVE        TYPE C LENGTH 44.

    FIELD-SYMBOLS: <XMLR_TAB>  TYPE J1B_NF_XML_B12_TAB.

    CLEAR: WL_DOC_REF, WA_INFO_V, WA_INFO_C, V_CHAVE.

    ASSIGN ('(SAPLJ_1B_NFE)XMLR_TAB')  TO <XMLR_TAB>.
    CHECK <XMLR_TAB> IS ASSIGNED.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(WL_DOC)
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC EQ 0.


    CASE WL_DOC-NFE.
      WHEN ABAP_TRUE.

        CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
          EXPORTING
            I_DOCNUM = I_DOCNUM
            I_VALIDA = 'X'
          RECEIVING
            E_CHAVE  = V_CHAVE.

        WL_DOC_REF-B12_REFNFE = V_CHAVE.

      WHEN ABAP_FALSE.

        CLEAR: V_REGIO_PARID.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WL_DOC-NFNUM
          IMPORTING
            OUTPUT = WL_DOC-NFENUM.

        IF ( WL_DOC-REGIO IS INITIAL ) OR
           ( WL_DOC-STKZN IS INITIAL AND WL_DOC-CGC IS INITIAL ) OR
           ( WL_DOC-STKZN IS NOT INITIAL AND WL_DOC-CPF IS INITIAL ) OR
           ( WL_DOC-STAINS IS INITIAL ).

          CALL FUNCTION 'Z_PARCEIRO_INFO'
            EXPORTING
              P_PARCEIRO   = WL_DOC-PARID
              P_PARTYPE    = WL_DOC-PARTYP
            CHANGING
              WA_INFO_PART = WA_INFO_V
              WA_INFO_C    = WA_INFO_C.

          IF WL_DOC-PARTYP EQ 'V'.
            V_REGIO_PARID      = WA_INFO_V-REGIO.
            WL_DOC_REF-B20_IE  = WA_INFO_V-STCD3.

            IF WA_INFO_V-STKZN IS INITIAL.
              WL_DOC_REF-B20_CNPJ = WA_INFO_V-STCD1.
            ELSE.
              WL_DOC_REF-B20_CPF  = WA_INFO_V-STCD2.
            ENDIF.

          ELSEIF WL_DOC-PARTYP EQ 'C'.

            V_REGIO_PARID      =  WA_INFO_C-REGIO.
            WL_DOC_REF-B20_IE  =  WA_INFO_C-STCD3.

            IF WA_INFO_C-STKZN IS INITIAL.
              WL_DOC_REF-B20_CNPJ = WA_INFO_C-STCD1.
            ELSE.
              WL_DOC_REF-B20_CPF =  WA_INFO_C-STCD2.
            ENDIF.

          ENDIF.

          WL_DOC_REF-B20_AAMM  = WL_DOC-DOCDAT+2(2) && WL_DOC-DOCDAT+4(2).
          WL_DOC_REF-B20F_MOD  = WL_DOC-MODEL.
          WL_DOC_REF-B20_SERIE = WL_DOC-SERIES.
          WL_DOC_REF-B20_NNF   = WL_DOC-NFENUM.

        ELSE.

          V_REGIO_PARID        = WL_DOC-REGIO.
          WL_DOC_REF-B20_AAMM  = WL_DOC-DOCDAT+2(2) && WL_DOC-DOCDAT+4(2).
          WL_DOC_REF-B20_IE    = WL_DOC-STAINS.
          WL_DOC_REF-B20F_MOD  = WL_DOC-MODEL.
          WL_DOC_REF-B20_SERIE = WL_DOC-SERIES.
          WL_DOC_REF-B20_NNF   = WL_DOC-NFENUM.

          IF WL_DOC-STKZN IS INITIAL.
            WL_DOC_REF-B20_CNPJ = WL_DOC-CGC.
          ELSE.
            WL_DOC_REF-B20_CPF  = WL_DOC-CPF.
          ENDIF.

        ENDIF.

        ZCL_ESTADO=>ZIF_ESTADO~GET_INSTANCE( )->GET_ID_BACEN(
          EXPORTING
            I_UF        = CONV #( V_REGIO_PARID )
          IMPORTING
            E_ID_BACEN  = DATA(E_ID_BACEN)
         ).

        WL_DOC_REF-B20_CUF = E_ID_BACEN.

    ENDCASE.

    WL_DOC_REF-DOCNUM = I_XML_HEADER-DOCNUM.

    APPEND WL_DOC_REF TO <XMLR_TAB>.

  ENDMETHOD.


  method ADD_REF_NF_CCT.

    DATA: WL_DOC_REF     TYPE J1B_NF_XML_B12,
          WA_INFO_V      TYPE LFA1,
          WA_INFO_C      TYPE KNA1,
          V_REGIO_PARID  TYPE KNA1-REGIO,
          V_CHAVE        TYPE C LENGTH 44.

    FIELD-SYMBOLS: <XMLR_TAB>  TYPE J1B_NF_XML_B12_TAB.

    CLEAR: WL_DOC_REF, WA_INFO_V, WA_INFO_C, V_CHAVE.

    ASSIGN ('(SAPLJ_1B_NFE)XMLR_TAB')  TO <XMLR_TAB>.
    CHECK <XMLR_TAB> IS ASSIGNED.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(WL_DOC)
     WHERE DOCNUM EQ @I_ZLEST0147-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    IF I_ZLEST0147-MODEL = '55'.

      WL_DOC_REF-B12_REFNFE = I_ZLEST0147-CHAVE_NFE.

    ELSE.

      CLEAR: V_REGIO_PARID.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          P_PARCEIRO   = WL_DOC-PARID
          P_PARTYPE    = WL_DOC-PARTYP
        CHANGING
          WA_INFO_PART = WA_INFO_V
          WA_INFO_C    = WA_INFO_C.

      IF WL_DOC-PARTYP EQ 'V'.

        V_REGIO_PARID     = WA_INFO_V-REGIO.
        WL_DOC_REF-B20_IE =  WA_INFO_V-STCD3.

        IF WA_INFO_V-STKZN IS INITIAL.
          WL_DOC_REF-B20_CNPJ =  WA_INFO_V-STCD1.
        ELSE.
          WL_DOC_REF-B20_CPF  =  WA_INFO_V-STCD2.
        ENDIF.

      ELSEIF WL_DOC-PARTYP EQ 'C'.

        V_REGIO_PARID      = WA_INFO_C-REGIO.
        WL_DOC_REF-B20_IE  = WA_INFO_C-STCD3.

        IF WA_INFO_C-STKZN IS INITIAL.
          WL_DOC_REF-B20_CNPJ = WA_INFO_C-STCD1.
        ELSE.
          WL_DOC_REF-B20_CPF  = WA_INFO_C-STCD2.
        ENDIF.

      ENDIF.

      WL_DOC_REF-B20_AAMM  = I_ZLEST0147-DT_EMISSAO+2(2) && I_ZLEST0147-DT_EMISSAO+4(2).
      WL_DOC_REF-B20F_MOD  = I_ZLEST0147-MODEL.
      WL_DOC_REF-B20_SERIE = I_ZLEST0147-SERIE.
      WL_DOC_REF-B20_NNF   = I_ZLEST0147-NFNUM.

      ZCL_ESTADO=>ZIF_ESTADO~GET_INSTANCE( )->GET_ID_BACEN(
        EXPORTING
          I_UF        = CONV #( V_REGIO_PARID )
        IMPORTING
          E_ID_BACEN  = DATA(E_ID_BACEN) ).

      WL_DOC_REF-B20_CUF = E_ID_BACEN.

    ENDIF.

    WL_DOC_REF-DOCNUM = I_XML_HEADER-DOCNUM.

    APPEND WL_DOC_REF TO <XMLR_TAB>.


  endmethod.


method block_h2.


  data: it_j_1bnflin       type table of j_1bnflin initial size 0,
        wa_j_1bnflin       type j_1bnflin,
        wa_j_1bnfdoc       type j_1bnfdoc,
        wa_znota_import_ii type znota_import_ii,
        it_znota_import    type table of znota_import,
        it_znota_import_ad type table of znota_import_ad,
        wa_znota_import    type znota_import,
        wa_znota_import_ad type znota_import_ad,
        wa_out_import      type j1b_nf_xml_h4,
        wa_info_part       type lfa1,
        wa_info_c          type kna1.

  check not i_docnum is initial.

  clear o_importacao.

  if i_itmnum is initial.
    select * into table it_j_1bnflin
      from j_1bnflin
     where docnum eq i_docnum.
  else.
    select * into table it_j_1bnflin
      from j_1bnflin
     where docnum eq i_docnum
       and itmnum eq i_itmnum.
  endif.

  loop at it_j_1bnflin into wa_j_1bnflin.
    if wa_j_1bnflin-cfop(1) eq '3'.
      o_importacao = 'X'.
    endif.
  endloop.

  if not o_importacao is initial .

    select single * into wa_j_1bnfdoc
      from j_1bnfdoc
     where docnum eq i_docnum.

    if sy-subrc is initial.
      clear: wa_info_part, wa_info_c.

      call function 'Z_PARCEIRO_INFO'
        exporting
          p_parceiro   = wa_j_1bnfdoc-parid
          p_partype    = wa_j_1bnfdoc-partyp
        changing
          wa_info_part = wa_info_part
          wa_info_c    = wa_info_c.

      if wa_j_1bnfdoc-partyp eq 'C'.
        if wa_info_c-land1 ne 'BR'.
          select single bacen into o_cpais
            from zpais
           where land1 eq wa_info_c-land1.
        endif.
      elseif wa_j_1bnfdoc-partyp eq 'V'.
        if wa_info_part-land1 ne 'BR'.
          select single bacen into o_cpais
            from zpais
           where land1 eq wa_info_part-land1.
        endif.
      endif.
    endif.

    o_out_item-o_ii       = 'X'.
    o_out_item-o_vbc      = 0.
    o_out_item-o_vdespadu = 0.
    o_out_item-o_vii      = 0.
    o_out_item-o_viof     = 0.

    select single *
      into wa_znota_import_ii
      from znota_import_ii
     where docnum  eq i_docnum
       and itmnum  eq i_itmnum.

    if sy-subrc is initial.
      o_out_item-o_vbc      = wa_znota_import_ii-o_vbc.
      o_out_item-o_vdespadu = wa_znota_import_ii-o_vdespadu.
      o_out_item-o_vii      = wa_znota_import_ii-o_vii.
      o_out_item-o_viof     = wa_znota_import_ii-o_viof.
    endif.
  endif.

  if not o_importacao is initial and not i_preenche_di is initial.

    select * into table it_znota_import
      from znota_import
     where docnum  eq i_docnum
       and itmnum  eq i_itmnum.

    if sy-subrc is initial.
      select * into table it_znota_import_ad
        from znota_import_ad
         for all entries in it_znota_import
       where docnum  eq it_znota_import-docnum
         and itmnum  eq it_znota_import-itmnum
         and itdidoc eq it_znota_import-itdidoc.
    endif.

    loop at it_znota_import into wa_znota_import.

      clear: wa_out_import.

      wa_out_import-docnum      = wa_znota_import-docnum.
      wa_out_import-itmnum      = wa_znota_import-itmnum.

      wa_out_import-ndi         = wa_znota_import-ndi.
      wa_out_import-ddi         = wa_znota_import-ddi.
      wa_out_import-xlocdesemb  = wa_znota_import-xlocdesemb.
      wa_out_import-ufdesemb    = wa_znota_import-ufdesemb.
      wa_out_import-ddesemb     = wa_znota_import-ddesemb.
      wa_out_import-cexportador = wa_znota_import-cexportador.

      loop at it_znota_import_ad into wa_znota_import_ad where docnum  eq wa_znota_import-docnum
                                                           and itmnum  eq wa_znota_import-itmnum
                                                           and itdidoc eq wa_znota_import-itdidoc.

        wa_out_import-nadicao     = wa_znota_import_ad-nr_adicao.
        wa_out_import-nseqadic    = wa_znota_import_ad-nr_seq_adicao.
        wa_out_import-cfabricante = wa_znota_import_ad-cfabricante.
        wa_out_import-vdescdi     = wa_znota_import_ad-vlr_desconto.
        append wa_out_import to o_out_import.
      endloop.
    endloop.

  endif.

endmethod.


METHOD block_u.

*  DATA: wa_item  TYPE j_1bnflin,
*        wa_vbrk  TYPE vbrk,
*        vg_vbeln TYPE vbeln_vf.
*
*  SELECT SINGLE *
*    INTO wa_item
*    FROM j_1bnflin
*   WHERE docnum EQ in_doc-docnum.
*
*  IF wa_item-reftyp EQ 'BI'.
*
*    vg_vbeln = wa_item-refkey(10).
*
*    SELECT SINGLE * INTO wa_vbrk
*      FROM vbrk
*     WHERE vbeln EQ vg_vbeln.
*
*    IF sy-subrc EQ 0.
*      e_cobranca-nfat  = wa_vbrk-vbeln.
*      e_cobranca-vorig = wa_vbrk-netwr.
*      e_cobranca-vdesc = 0.
*      e_cobranca-vliq  = wa_vbrk-netwr.
*    ENDIF.
*
*  ENDIF.

ENDMETHOD.


METHOD BLOCK_W.


  TYPES: BEGIN OF TY_CONTROLE_NOTAS,
          DOCNUM TYPE J_1BNFLIN-DOCNUM,
          NFENUM TYPE J_1BNFDOC-NFNUM,
          MENGE  TYPE J_1BNFLIN-MENGE,
          TIPO   TYPE C LENGTH 1,
          END OF TY_CONTROLE_NOTAS,

          BEGIN OF TY_J_1BNFLIN,
           DOCNUM       TYPE J_1BNFLIN-DOCNUM,
           REFKEY       TYPE J_1BNFLIN-REFKEY,
           MENGE        TYPE J_1BNFLIN-MENGE,
           REFKEY_VBELN TYPE VBFA-VBELN,
          END OF TY_J_1BNFLIN.


  DATA: IT_ITENS TYPE TABLE OF J_1BNFLIN INITIAL SIZE 0,
        WA_ITENS TYPE J_1BNFLIN,
        WA_NOTA  TYPE J_1BNFDOC,
        WA_ENDE  TYPE SADR.

  DATA: WA_INFO_PART   TYPE LFA1,
        WA_INFO_C      TYPE KNA1,
        P_PARCEIRO     TYPE J_1BPARID,
        IT_ZSDT_EXPORT TYPE TABLE OF ZSDT_EXPORT,
        WA_ZSDT_EXPORT TYPE ZSDT_EXPORT,
        WA_DATA        TYPE C LENGTH 10,
        WA_ZNFECOMEX   TYPE ZNFECOMEX.

  DATA: IT_CONTROLE_NOTAS TYPE TABLE OF TY_CONTROLE_NOTAS,
        WA_CONTROLE_NOTAS TYPE TY_CONTROLE_NOTAS,
        IT_ZSDT_RETLOTE   TYPE TABLE OF ZSDT_RETLOTE,
        WA_ZSDT_RETLOTE   TYPE ZSDT_RETLOTE,
        IT_ZSDT0053       TYPE TABLE OF ZSDT0053,
        IT_J_1BNFLIN      TYPE TABLE OF TY_J_1BNFLIN,
        WA_J_1BNFLIN      TYPE TY_J_1BNFLIN,
        IT_J_1BNFDOC      TYPE TABLE OF J_1BNFDOC,
        WA_J_1BNFDOC      TYPE J_1BNFDOC,
        IT_VBFA           TYPE TABLE OF VBFA,
        WA_VBFA           TYPE VBFA.


  CLEAR: E_COMEX,
         E_CPAIS,
         E_UFEMBARQ,
         E_XLOCEMBARQ.

  SELECT * INTO TABLE IT_ITENS
    FROM J_1BNFLIN
   WHERE DOCNUM EQ I_DOCNUM.

  IF SY-SUBRC IS INITIAL.
    READ TABLE IT_ITENS INDEX 1 INTO WA_ITENS.
    IF WA_ITENS-CFOP(1) EQ '7'.
      E_COMEX      = 'X'.

*      SELECT SINGLE * INTO WA_ZNFECOMEX FROM ZNFECOMEX WHERE DOCNUM EQ I_DOCNUM.
*      IF SY-SUBRC IS INITIAL.
*        E_COMEX      = 'X'.
*        E_UFEMBARQ   = WA_ZNFECOMEX-UFEMBARQ.
*        E_XLOCEMBARQ = WA_ZNFECOMEX-XLOCEMBARQ.
*      ENDIF.


      "Ajuste para NF-e 3.1
      CALL FUNCTION 'Z_SD_INFO_NFE_EXPORTACAO'
        EXPORTING
          P_DOCNUM       = I_DOCNUM
        IMPORTING
          E_ZNFECOMEX    = WA_ZNFECOMEX
        EXCEPTIONS
          NAO_LOCALIZADO = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      E_UFEMBARQ     = WA_ZNFECOMEX-UFEMBARQ.
      E_XLOCEMBARQ   = WA_ZNFECOMEX-XLOCEMBARQ.
      E_XLOCDESPACHO = WA_ZNFECOMEX-NAME1.       "Utilizado para GRC

      SELECT SINGLE * INTO WA_NOTA
        FROM J_1BNFDOC
       WHERE DOCNUM EQ I_DOCNUM.

      IF SY-SUBRC IS INITIAL.
        CLEAR: WA_INFO_PART, WA_INFO_C.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = WA_NOTA-PARID
            P_PARTYPE    = WA_NOTA-PARTYP
          CHANGING
            WA_INFO_PART = WA_INFO_PART
            WA_INFO_C    = WA_INFO_C.

        IF WA_NOTA-PARTYP EQ 'C'.
          IF WA_INFO_C-LAND1 NE 'BR'.
            SELECT SINGLE BACEN INTO E_CPAIS
              FROM ZPAIS
             WHERE LAND1 EQ WA_INFO_C-LAND1.
          ENDIF.
        ELSEIF WA_NOTA-PARTYP EQ 'V'.
          IF WA_INFO_PART-LAND1 NE 'BR'.
            SELECT SINGLE BACEN INTO E_CPAIS
              FROM ZPAIS
             WHERE LAND1 EQ WA_INFO_PART-LAND1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT ( I_VBELN IS INITIAL ).
    SELECT * INTO TABLE IT_ZSDT_EXPORT
      FROM ZSDT_EXPORT
     WHERE ORDEM EQ I_VBELN.

    IF ( SY-SUBRC EQ 0 ).
      CLEAR: E_OBSERVACAO.

      LOOP AT IT_ZSDT_EXPORT INTO WA_ZSDT_EXPORT.

        CLEAR: WA_NOTA.

        SELECT SINGLE * INTO WA_NOTA
          FROM J_1BNFDOC
         WHERE DOCNUM EQ WA_ZSDT_EXPORT-DOCNUM.

        IF WA_NOTA-NFE IS INITIAL.
          WA_NOTA-NFNUM = WA_NOTA-NFENUM.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_NOTA-NFENUM
            IMPORTING
              OUTPUT = WA_NOTA-NFENUM.
        ENDIF.

        WRITE WA_NOTA-DOCDAT TO WA_DATA.

        IF E_OBSERVACAO IS INITIAL.
          CONCATENATE 'NF Retor. - Nr:' WA_NOTA-NFENUM 'Série:' WA_NOTA-SERIES 'Mod:' WA_NOTA-MODEL 'Dt.Emi:' WA_DATA INTO E_OBSERVACAO SEPARATED BY SPACE.
        ELSE.
          CONCATENATE E_OBSERVACAO 'NF Retor. - Nr:' WA_NOTA-NFENUM 'Série:' WA_NOTA-SERIES 'Mod:' WA_NOTA-MODEL 'Dt.Em:' WA_DATA INTO E_OBSERVACAO SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  FIELD-SYMBOLS: <FS_J_1BNFLIN> TYPE TY_J_1BNFLIN.

  SELECT * FROM J_1BNFDOC
    INTO TABLE IT_J_1BNFDOC
  WHERE DOCNUM EQ I_DOCNUM.

  SELECT DOCNUM REFKEY MENGE
    FROM J_1BNFLIN
    INTO TABLE IT_J_1BNFLIN
    FOR ALL ENTRIES IN IT_J_1BNFDOC
  WHERE DOCNUM EQ IT_J_1BNFDOC-DOCNUM.

  LOOP AT IT_J_1BNFLIN ASSIGNING <FS_J_1BNFLIN>.
    <FS_J_1BNFLIN>-REFKEY_VBELN = <FS_J_1BNFLIN>-REFKEY.
  ENDLOOP.

  SELECT * FROM VBFA
    INTO TABLE IT_VBFA
    FOR ALL ENTRIES IN IT_J_1BNFLIN
  WHERE VBELN   EQ IT_J_1BNFLIN-REFKEY_VBELN
    AND VBTYP_N EQ 'M'
    AND VBTYP_V EQ 'J'.

  IF ( SY-SUBRC EQ 0 ).

    SELECT * FROM ZSDT_RETLOTE
      INTO TABLE IT_ZSDT_RETLOTE
      FOR ALL ENTRIES IN IT_VBFA
    WHERE VBELN EQ IT_VBFA-VBELV.

    IF IT_ZSDT_RETLOTE[] IS INITIAL.
      "===========================================================================================
      " Algodão Notas de Retorno Simbolico e Remessa Formação Lote.
      "===========================================================================================
      CLEAR: IT_ZSDT0053[].
      SELECT *
        FROM ZSDT0053 INTO TABLE IT_ZSDT0053
         FOR ALL ENTRIES IN IT_VBFA
       WHERE REMESSA_EXP EQ IT_VBFA-VBELV.

      DELETE IT_ZSDT0053 WHERE DOCNUM_RT IS INITIAL.

      IF IT_ZSDT0053[] IS NOT INITIAL.
        SELECT *
          FROM ZSDT_RETLOTE INTO TABLE IT_ZSDT_RETLOTE
           FOR ALL ENTRIES IN IT_ZSDT0053
         WHERE DOCNUM_RET EQ IT_ZSDT0053-DOCNUM_RT.
      ENDIF.
    ENDIF.

    IF IT_ZSDT_RETLOTE[] IS NOT INITIAL.

      REFRESH: IT_J_1BNFLIN[].
      CLEAR: WA_J_1BNFLIN.

      SELECT DOCNUM REFKEY MENGE
        FROM J_1BNFLIN
        INTO TABLE IT_J_1BNFLIN
        FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
      WHERE DOCNUM EQ IT_ZSDT_RETLOTE-DOCNUM.


      LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN.

        READ TABLE IT_ZSDT_RETLOTE INTO WA_ZSDT_RETLOTE WITH KEY DOCNUM = WA_J_1BNFLIN-DOCNUM.

        IF ( SY-SUBRC NE 0 ).
          CLEAR: WA_ZSDT_RETLOTE, WA_J_1BNFLIN.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_ZSDT_RETLOTE-NFENUM
          IMPORTING
            OUTPUT = WA_CONTROLE_NOTAS-NFENUM.

        MOVE: WA_J_1BNFLIN-DOCNUM TO WA_CONTROLE_NOTAS-DOCNUM,
              WA_J_1BNFLIN-MENGE  TO WA_CONTROLE_NOTAS-MENGE.

        IF ( WA_ZSDT_RETLOTE-QUANT_VINC < WA_J_1BNFLIN-MENGE ).
          MOVE: 'Parcial' TO WA_CONTROLE_NOTAS-TIPO.
        ENDIF.

        APPEND WA_CONTROLE_NOTAS TO IT_CONTROLE_NOTAS.

        CLEAR: WA_J_1BNFLIN, WA_J_1BNFDOC, WA_ZSDT_RETLOTE, WA_CONTROLE_NOTAS.

      ENDLOOP.

      READ TABLE IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WITH KEY TIPO  = 'Parcial'.
      IF ( SY-SUBRC EQ 0 ).
        CONCATENATE E_OBSERVACAO 'NF-Rem.Form.Lote: (Parcial)' INTO E_OBSERVACAO  SEPARATED BY SPACE.
      ELSE.
        CONCATENATE E_OBSERVACAO 'NF-Rem.Form.Lote: '          INTO E_OBSERVACAO  SEPARATED BY SPACE.
      ENDIF.

      LOOP AT IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WHERE TIPO EQ 'Parcial'.
        CONCATENATE E_OBSERVACAO WA_CONTROLE_NOTAS-NFENUM INTO E_OBSERVACAO SEPARATED BY SPACE.
        CLEAR: WA_CONTROLE_NOTAS.
      ENDLOOP.

      LOOP AT IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WHERE TIPO NE 'Parcial'.
        CONCATENATE E_OBSERVACAO WA_CONTROLE_NOTAS-NFENUM INTO E_OBSERVACAO SEPARATED BY SPACE.
        CLEAR: WA_CONTROLE_NOTAS.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  CHECK NOT I_VBELN IS INITIAL.
*  SELECT * INTO TABLE IT_ZSDT_EXPORT
*    FROM ZSDT_EXPORT
*   WHERE ORDEM EQ I_VBELN.
*
*  CLEAR: E_OBSERVACAO.
*
*  LOOP AT IT_ZSDT_EXPORT INTO WA_ZSDT_EXPORT.
*
*    CLEAR: WA_NOTA.
*
*    SELECT SINGLE * INTO WA_NOTA
*      FROM J_1BNFDOC
*     WHERE DOCNUM EQ WA_ZSDT_EXPORT-DOCNUM.
*
*    IF WA_NOTA-NFE IS INITIAL.
*      WA_NOTA-NFNUM = WA_NOTA-NFENUM.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_NOTA-NFENUM
*        IMPORTING
*          OUTPUT = WA_NOTA-NFENUM.
*    ENDIF.
*
*    WRITE WA_NOTA-DOCDAT TO WA_DATA.
*
*    IF E_OBSERVACAO IS INITIAL.
*      CONCATENATE 'NF Retor. - Nr:' WA_NOTA-NFENUM 'Série:' WA_NOTA-SERIES 'Mod:' WA_NOTA-MODEL 'Dt.Emi:' WA_DATA INTO E_OBSERVACAO SEPARATED BY SPACE.
*    ELSE.
*      CONCATENATE E_OBSERVACAO 'NF Retor. - Nr:' WA_NOTA-NFENUM 'Série:' WA_NOTA-SERIES 'Mod:' WA_NOTA-MODEL 'Dt.Em:' WA_DATA INTO E_OBSERVACAO SEPARATED BY SPACE.
*    ENDIF.
*  ENDLOOP.
*
*  IF NOT ( IT_ZSDT_EXPORT[] IS INITIAL ).
*
*    SELECT * FROM ZSDT_RETLOTE
*      INTO TABLE IT_ZSDT_RETLOTE
*      FOR ALL ENTRIES IN IT_ZSDT_EXPORT
*    WHERE DOCNUM_RET EQ IT_ZSDT_EXPORT-DOCNUM.
*
*    SELECT * FROM J_1BNFLIN
*      INTO TABLE IT_J_1BNFLIN
*      FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
*    WHERE DOCNUM EQ IT_ZSDT_RETLOTE-DOCNUM.
*
*    SELECT * FROM J_1BNFDOC
*      INTO TABLE IT_J_1BNFDOC
*      FOR ALL ENTRIES IN IT_J_1BNFLIN
*    WHERE DOCNUM EQ IT_J_1BNFLIN-DOCNUM.
*
*
*    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN.
*      READ TABLE IT_ZSDT_RETLOTE INTO WA_ZSDT_RETLOTE WITH KEY DOCNUM = WA_J_1BNFLIN-DOCNUM.
*      READ TABLE IT_J_1BNFDOC    INTO WA_J_1BNFDOC    WITH KEY DOCNUM = WA_J_1BNFLIN-DOCNUM.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          INPUT  = WA_J_1BNFDOC-NFENUM
*        IMPORTING
*          OUTPUT = WA_CONTROLE_NOTAS-NFENUM.
*
*      MOVE: WA_J_1BNFLIN-DOCNUM TO WA_CONTROLE_NOTAS-DOCNUM,
*            WA_J_1BNFLIN-MENGE  TO WA_CONTROLE_NOTAS-MENGE.
*
*      IF ( WA_ZSDT_RETLOTE-QUANT_VINC < WA_J_1BNFLIN-MENGE ).
*        MOVE: 'Parcial' TO WA_CONTROLE_NOTAS-TIPO.
*      ENDIF.
*
*      APPEND WA_CONTROLE_NOTAS TO IT_CONTROLE_NOTAS.
*
*      CLEAR: WA_J_1BNFLIN, WA_J_1BNFDOC, WA_ZSDT_RETLOTE, WA_CONTROLE_NOTAS.
*    ENDLOOP.
*
*    READ TABLE IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WITH KEY TIPO  = 'Parcial'.
*    IF ( SY-SUBRC EQ 0 ).
*      CONCATENATE E_OBSERVACAO 'NF-Rem.Form.Lote: (Parcial)' INTO E_OBSERVACAO  SEPARATED BY SPACE.
*    ELSE.
*      CONCATENATE E_OBSERVACAO 'NF-Rem.Form.Lote: '          INTO E_OBSERVACAO  SEPARATED BY SPACE.
*    ENDIF.
*
*    LOOP AT IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WHERE TIPO EQ 'Parcial'.
*      CONCATENATE E_OBSERVACAO WA_CONTROLE_NOTAS-NFENUM INTO E_OBSERVACAO SEPARATED BY SPACE.
*      CLEAR: WA_CONTROLE_NOTAS.
*    ENDLOOP.
*
*    LOOP AT IT_CONTROLE_NOTAS INTO WA_CONTROLE_NOTAS WHERE TIPO NE 'Parcial'.
*      CONCATENATE E_OBSERVACAO WA_CONTROLE_NOTAS-NFENUM INTO E_OBSERVACAO SEPARATED BY SPACE.
*      CLEAR: WA_CONTROLE_NOTAS.
*    ENDLOOP.
*
*  ENDIF.


ENDMETHOD.


  METHOD determinar_parceiros.

    DATA: it_vbpa       TYPE TABLE OF vbpa,
          wl_vbpa       TYPE vbpa,

          it_j_1bad     TYPE TABLE OF j_1bad,
          wl_j_1bad     TYPE j_1bad,

          it_j_1bnfnad  TYPE j_1bnfnad_tab,
          wl_j_1bnfnad  LIKE LINE OF it_j_1bnfnad,


          it_zfiwrt0015 TYPE TABLE OF zfiwrt0015,
          wl_zfiwrt0015 TYPE zfiwrt0015,

          wl_j_1binnad  TYPE j_1binnad,

          ls_ttxd       TYPE ttxd,

          wl_lfa1_br    TYPE lfa1-lifnr.

    CLEAR: it_j_1bnfnad[], it_vbpa[], it_j_1bad[], it_zfiwrt0015[].

    ls_ttxd-leng1 = '03'.

    CHECK c_out_header-docnum IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(_wl_lin)
     WHERE docnum EQ @c_out_header-docnum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_doc)
     WHERE docnum EQ @c_out_header-docnum.

    SELECT *
      FROM j_1bad INTO TABLE it_j_1bad.

    CASE _wl_lin-reftyp.
      WHEN 'BI'.

        SELECT SINGLE *
          FROM vbrp INTO @DATA(_wl_vbrp)
         WHERE vbeln EQ @_wl_lin-refkey(10).

        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM likp INTO @DATA(_wl_likp)
         WHERE vbeln EQ @_wl_vbrp-vgbel.

        CHECK sy-subrc EQ 0.

        SELECT *
          FROM vbpa INTO TABLE it_vbpa
         WHERE vbeln EQ _wl_likp-vbeln.

        LOOP AT it_vbpa INTO wl_vbpa.

          CLEAR: wl_j_1bnfnad.

          READ TABLE it_j_1bad INTO wl_j_1bad WITH KEY parvw = wl_vbpa-parvw.

          CHECK sy-subrc EQ 0.

          wl_j_1bnfnad-docnum = c_out_header-docnum.
          wl_j_1bnfnad-parvw  = wl_j_1bad-parvw.
          wl_j_1bnfnad-partyp = wl_j_1bad-partyp.

          CASE wl_j_1bnfnad-partyp .
            WHEN 'V'.
              wl_j_1bnfnad-parid = wl_vbpa-lifnr.
            WHEN 'C'.
              wl_j_1bnfnad-parid = wl_vbpa-kunnr.
          ENDCASE.

          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

        ENDLOOP.

      WHEN 'LI'.

        IF _wl_doc-entrad EQ abap_true.

          "Ponto de Coleta
          wl_j_1bnfnad-docnum = _wl_doc-docnum.
          wl_j_1bnfnad-parvw  = 'PC'.
          wl_j_1bnfnad-partyp = _wl_doc-partyp.
          wl_j_1bnfnad-parid  = _wl_doc-parid.
          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

          "Local de Entrega
          wl_j_1bnfnad-docnum = _wl_doc-docnum.
          wl_j_1bnfnad-parvw  = 'LR'.
          wl_j_1bnfnad-partyp = 'C'.
          wl_j_1bnfnad-parid  = |{ _wl_lin-werks ALPHA = IN }|.
          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

        ENDIF.


      WHEN 'ZW'.

        SELECT SINGLE *
          FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
         WHERE seq_lcto EQ @_wl_lin-refkey(10).

        CHECK ( sy-subrc EQ 0 ).

        SELECT *
          FROM zfiwrt0015 INTO TABLE it_zfiwrt0015
         WHERE seq_lcto EQ _wl_zfiwrt0008-seq_lcto
           AND parvw    IN ('PC', 'LR').

        LOOP AT it_zfiwrt0015 INTO wl_zfiwrt0015.
          CLEAR: wl_j_1bnfnad.

          wl_j_1bnfnad-docnum = c_out_header-docnum.
          wl_j_1bnfnad-parvw  = wl_zfiwrt0015-parvw.

          CASE wl_j_1bnfnad-parvw.
              WHEN 'PC'.
              wl_j_1bnfnad-partyp = 'V'.
              WHEN 'LR'.
              wl_j_1bnfnad-partyp = 'C'.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          wl_j_1bnfnad-parid = wl_zfiwrt0015-parid.

          APPEND wl_j_1bnfnad TO it_j_1bnfnad.
        ENDLOOP.

*       SELECT SINGLE *
*         FROM LIKP INTO _WL_LIKP
*        WHERE BEROT EQ _WL_ZFIWRT0008-CH_REFERENCIA.
*
*       CHECK SY-SUBRC EQ 0.
*
*       SELECT *
*         FROM VBPA INTO TABLE IT_VBPA
*        WHERE VBELN EQ _WL_LIKP-VBELN.
*
*       LOOP AT IT_VBPA INTO WL_VBPA.
*
*         CLEAR: WL_J_1BNFNAD.
*
*         READ TABLE IT_J_1BAD INTO WL_J_1BAD WITH KEY PARVW = WL_VBPA-PARVW.
*
*         CHECK SY-SUBRC EQ 0.
*
*         WL_J_1BNFNAD-DOCNUM = C_OUT_HEADER-DOCNUM.
*         WL_J_1BNFNAD-PARVW  = WL_J_1BAD-PARVW.
*         WL_J_1BNFNAD-PARTYP = WL_J_1BAD-PARTYP.
*
*         CASE WL_J_1BNFNAD-PARTYP .
*           WHEN 'V'.
*             WL_J_1BNFNAD-PARID = WL_VBPA-LIFNR.
*           WHEN 'C'.
*             WL_J_1BNFNAD-PARID = WL_VBPA-KUNNR.
*         ENDCASE.
*
*         APPEND WL_J_1BNFNAD TO IT_J_1BNFNAD.
*
*       ENDLOOP.

    ENDCASE.

    CHECK it_j_1bnfnad[] IS NOT INITIAL.

    TRY.

        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
          EXPORTING
            i_docnum        = CONV #( c_out_header-docnum )
          IMPORTING
            e_conhecimento  = DATA(emite_conhecimento)
            e_tp_frete      = DATA(tp_frete) ).


        IF ( _wl_doc-model = '55' ) AND
           ( ( emite_conhecimento EQ abap_false ) OR ( _wl_lin-reftyp EQ 'LI' ) ) AND
*-CS2022000285 - 16.03.2022 - JT - inicio
             ( tp_frete NE 'CPT' AND tp_frete NE 'CIF' ).
*-CS2022000285 - 16.03.2022 - JT - fim

          "-----------------------------------------------*
          " Local de Retirada
          "-----------------------------------------------*

          "IF C_OUT_HEADER-F_RETIRADA EQ ABAP_FALSE.

          CALL METHOD me->read_partner
            EXPORTING
              i_parvw         = 'PC'
              i_j_1bnfnad_tab = it_j_1bnfnad
            CHANGING
              c_j_1binnad     = wl_j_1binnad
            RECEIVING
              r_ok            = DATA(_ok).

          IF ( _ok EQ abap_true ) AND
             ( i_xml_header-c_cnpj NE wl_j_1binnad-cgc ). "Remetente diferente do Ponto de Coleta

            c_out_header-f_retirada  = abap_true.
            c_out_header-f_cnpj      = wl_j_1binnad-cgc.
            c_out_header-f_cpf       = wl_j_1binnad-cpf.
            c_out_header-f_xlgr      = wl_j_1binnad-street.
            c_out_header-f_nro       = wl_j_1binnad-house_num1.
            c_out_header-f_xcpl      = wl_j_1binnad-house_num2.
            c_out_header-f_xbairro   = wl_j_1binnad-ort02.
            c_out_header-f_cmun      = wl_j_1binnad-txjcd+ls_ttxd-leng1.
            c_out_header-f_xmun      = wl_j_1binnad-ort01.
            c_out_header-f_uf        = wl_j_1binnad-regio.
          ENDIF.

          "ENDIF. " IF C_OUT_HEADER-F_RETIRADA IS INITIAL.

          "-----------------------------------------------*
          " Local de Entrega
          "-----------------------------------------------*

          "IF C_OUT_HEADER-ENTREGA EQ ABAP_FALSE.

          CALL METHOD me->read_partner
            EXPORTING
              i_parvw         = 'Z1'
              i_j_1bnfnad_tab = it_j_1bnfnad
            CHANGING
              c_j_1binnad     = wl_j_1binnad
            RECEIVING
              r_ok            = DATA(_EXISTS_Z1).

          CALL METHOD me->read_partner
            EXPORTING
              i_parvw         = 'LR'
              i_j_1bnfnad_tab = it_j_1bnfnad
            CHANGING
              c_j_1binnad     = wl_j_1binnad
            RECEIVING
              r_ok            = _ok.

          IF ( _ok EQ abap_true ) AND
             ( _EXISTS_Z1 eq abap_false ) AND

             (
               ( i_xml_header-e_cnpj NE wl_j_1binnad-cgc AND i_xml_header-e_cnpj NE '00000000000000' ) OR
               ( i_xml_header-e_cpf  NE wl_j_1binnad-cpf AND i_xml_header-e_cpf  NE '00000000000' )
             ).

            c_out_header-entrega     = abap_true.
            c_out_header-g_cnpj      = wl_j_1binnad-cgc.
            c_out_header-g_cpf       = wl_j_1binnad-cpf.
            c_out_header-g_xlgr      = wl_j_1binnad-street.
            c_out_header-g_nro       = wl_j_1binnad-house_num1.
            c_out_header-g_xcpl      = wl_j_1binnad-house_num2.
            c_out_header-g_xbairro   = wl_j_1binnad-ort02.
            c_out_header-g_cmun      = wl_j_1binnad-txjcd+ls_ttxd-leng1.
            c_out_header-g_xmun      = wl_j_1binnad-ort01.
            c_out_header-g_uf        = wl_j_1binnad-regio.

          ENDIF.



          "ENDIF. "IF C_OUT_HEADER-ENTREGA EQ ABAP_FALSE.

        ENDIF.


      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.



  ENDMETHOD.


  METHOD fill_docs_reference.

    "Internal Tables
    DATA: lt_vbrk             TYPE TABLE OF vbrk,
          lt_vbfa             TYPE TABLE OF vbfa,
          lt_zdoc_exp         TYPE TABLE OF zdoc_exp,
          lt_zprog_reme       TYPE TABLE OF znom_prog_reme,
          lt_znom_reme_notas  TYPE TABLE OF znom_reme_notas,
          lt_zdoc_nf_produtor TYPE TABLE OF zdoc_nf_produtor,
          lt_zsdt_retlote     TYPE TABLE OF zsdt_retlote,
          lt_zsdt_retlote_ter TYPE TABLE OF zsdt_retlote_ter,
          lt_zsdt0053         TYPE TABLE OF zsdt0053,
          lt_zfiwrt0020       TYPE TABLE OF zfiwrt0020.

    "Classes
    DATA: lobj_util           TYPE REF TO zcl_util.

    CLEAR: lt_vbrk[], lt_vbfa[],  lt_zdoc_exp[],     lt_zprog_reme[], lt_znom_reme_notas[],
           lt_zdoc_nf_produtor[], lt_zsdt_retlote[], lt_zsdt0053[], lt_zfiwrt0020[].


    DATA: zcl_util            TYPE REF TO zcl_util.


    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(wl_lin)
     WHERE docnum EQ @i_xml_header-docnum.

    CHECK sy-subrc EQ 0.

    CREATE OBJECT lobj_util.

    CASE i_xml_header-finnfe.

      WHEN: '4'.

        "Retorno Simbolico
        SELECT *
          FROM zsdt_retlote INTO TABLE lt_zsdt_retlote
         WHERE docnum_ret EQ i_xml_header-docnum.

        LOOP AT lt_zsdt_retlote INTO DATA(lw_zsdt_retlote).
          me->add_ref_nf( i_xml_header = i_xml_header
                          i_docnum     = lw_zsdt_retlote-docnum ).
        ENDLOOP.

        "Notas RFL Terceiro Vinculado em Retorno Simbolico
        CLEAR: lt_zsdt_retlote_ter[].
        SELECT *
          FROM zsdt_retlote_ter INTO TABLE lt_zsdt_retlote_ter
         WHERE docnum_ret EQ i_xml_header-docnum.

        LOOP AT lt_zsdt_retlote_ter INTO DATA(lw_zsdt_retlote_ter).
          me->add_doc_reference( EXPORTING i_xml_header = i_xml_header
                                           i_chave_nfe  = CONV #( lw_zsdt_retlote_ter-chave_nfe ) ).
        ENDLOOP.

      WHEN: '1'.

        SELECT * FROM vbrk
          INTO TABLE lt_vbrk
        WHERE vbeln EQ wl_lin-refkey(10).

        IF ( sy-subrc EQ 0 ).

          SELECT * FROM vbfa
            INTO TABLE lt_vbfa
            FOR ALL ENTRIES IN lt_vbrk
         WHERE vbeln EQ lt_vbrk-vbeln
           AND vbtyp_n EQ 'M'
           AND vbtyp_v EQ 'J'.

          IF ( sy-subrc EQ 0 ).

            SELECT *  FROM zdoc_exp
              INTO TABLE lt_zdoc_exp
              FOR ALL ENTRIES IN lt_vbfa
            WHERE vbeln EQ lt_vbfa-vbelv.

            IF ( sy-subrc EQ 0 ).

              SELECT * FROM znom_prog_reme
                INTO TABLE lt_zprog_reme
                FOR ALL ENTRIES IN lt_zdoc_exp
              WHERE id_nomeacao_tran EQ lt_zdoc_exp-id_nomeacao_tran
                AND id_remessa       EQ lt_zdoc_exp-vbeln.

              IF ( sy-subrc EQ 0 ).

                CREATE OBJECT lobj_util.

                "===========================================================================================
                " Notas de Retorno Simbolico e Remessa Formação Lote.
                "===========================================================================================
                SELECT *
                  FROM zsdt_retlote
                  INTO TABLE lt_zsdt_retlote
                  FOR ALL ENTRIES IN lt_vbfa
                 WHERE vbeln EQ lt_vbfa-vbelv.

                IF lt_zsdt_retlote[] IS NOT INITIAL.

*                  "Retorno Simbolico
*                  READ TABLE LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE INDEX 1.
*
*                  IF ( LW_ZSDT_RETLOTE-DOCNUM_RET IS NOT INITIAL ).
*                    ME->ADD_REF_NF( I_XML_HEADER = I_XML_HEADER
*                                    I_DOCNUM     = LW_ZSDT_RETLOTE-DOCNUM_RET ).                                            .
*                  ENDIF.

                  "Remessas Formação Lote
                  LOOP AT lt_zsdt_retlote INTO lw_zsdt_retlote.
                    me->add_ref_nf( i_xml_header = i_xml_header
                                    i_docnum     = lw_zsdt_retlote-docnum ).
                  ENDLOOP.

                  "Notas RFL Terceiro Vinculado em Retorno Simbolico
                  READ TABLE lt_zsdt_retlote INTO lw_zsdt_retlote INDEX 1.
                  IF ( sy-subrc EQ 0 ) AND ( lw_zsdt_retlote-docnum_ret IS NOT INITIAL ).

                    CLEAR: lt_zsdt_retlote_ter[].
                    SELECT *
                      FROM zsdt_retlote_ter INTO TABLE lt_zsdt_retlote_ter
                     WHERE docnum_ret EQ lw_zsdt_retlote-docnum_ret.

                    LOOP AT lt_zsdt_retlote_ter INTO lw_zsdt_retlote_ter.
                      me->add_doc_reference( EXPORTING i_xml_header = i_xml_header
                                                       i_chave_nfe  = CONV #( lw_zsdt_retlote_ter-chave_nfe ) ).
                    ENDLOOP.

                  ENDIF.

                ENDIF.

                "Notas de Complemento para Fim Especifico
                me->fill_reference_nf_fim_esp( EXPORTING i_xml_header = i_xml_header
                                                         i_item       = wl_lin ).

              ENDIF.
            ENDIF. "IF ( SY-SUBRC EQ 0 ). "SELECT *  FROM ZDOC_EXP
          ENDIF. "IF ( SY-SUBRC EQ 0 ). "SELECT * FROM VBFA
        ENDIF. "IF ( SY-SUBRC EQ 0 ).  "SELECT * FROM VBRK


        "DEVK9A1XAW - 25.03.2024 SD - Ajustar Criação Fluxo exportação Granel e A #136393 RSA
        "===========================================================================================
        " Vinculo entre a formação de lote, e quais notas de Compra para fim especifico foram utilizadas
        "===========================================================================================

        CREATE OBJECT zcl_util.

        IF NOT wl_lin-docnum IS INITIAL.

          " Seleciona vinculo entre a formação de lote
          SELECT docnum_flote, chave_nfe
                 FROM zsdtvinc_p_flote
                 INTO TABLE @DATA(lt_zsdtvinc_p_flote)
                 WHERE docnum_flote  EQ @wl_lin-docnum
                 AND   vinculada_xml EQ @abap_true
                 AND   cancel        EQ @abap_false.

          SORT lt_zsdtvinc_p_flote BY docnum_flote.
          LOOP AT lt_zsdtvinc_p_flote INTO DATA(wa_zsdtvinc_p_flote).
            IF NOT wa_zsdtvinc_p_flote-chave_nfe IS INITIAL.
              me->add_doc_reference( EXPORTING i_xml_header = i_xml_header
                                               i_chave_nfe  = CONV #( wa_zsdtvinc_p_flote-chave_nfe ) ).
            ENDIF.
          ENDLOOP.

        ENDIF.
        "DEVK9A1XAW - 25.03.2024 SD - Ajustar Criação Fluxo exportação Granel e A #136393 RSA



        "===========================================================================================
        " Algodão Notas de Retorno Simbolico e Remessa Formação Lote.
        "===========================================================================================
        IF lt_vbfa[] IS NOT INITIAL.

          CLEAR: lt_zsdt0053[], lt_zsdt_retlote[].

          SELECT *
            FROM zsdt0053 INTO TABLE lt_zsdt0053
             FOR ALL ENTRIES IN lt_vbfa
           WHERE remessa_exp EQ lt_vbfa-vbelv.

          IF ( lt_zsdt0053[] IS NOT INITIAL ).

            SELECT *
              FROM zsdt_retlote INTO TABLE lt_zsdt_retlote
               FOR ALL ENTRIES IN lt_zsdt0053
             WHERE docnum_ret EQ lt_zsdt0053-docnum_rt.

            "Retorno Simbolico
            READ TABLE lt_zsdt_retlote INTO lw_zsdt_retlote INDEX 1.

            IF ( sy-subrc EQ 0 ) AND ( lw_zsdt_retlote-docnum_ret IS NOT INITIAL ).
              me->add_ref_nf( i_xml_header = i_xml_header
                              i_docnum     = lw_zsdt_retlote-docnum_ret ).
            ENDIF.

            "Remessas Formação Lote
            LOOP AT lt_zsdt_retlote INTO lw_zsdt_retlote.
              me->add_ref_nf( i_xml_header = i_xml_header
                              i_docnum     = lw_zsdt_retlote-docnum ).
            ENDLOOP.

            "Notas de Complemento para Fim Especifico
            me->fill_reference_nf_fim_esp( EXPORTING i_xml_header = i_xml_header
                                                     i_item       = wl_lin ).

          ENDIF. "IF ( SY-SUBRC EQ 0 ) AND ( LT_ZSDT0053[] IS NOT INITIAL ).

        ENDIF.

        IF wl_lin-reftyp = 'ZW'.
          SELECT *
            FROM zfiwrt0020 INTO TABLE lt_zfiwrt0020
           WHERE seq_lcto EQ wl_lin-refkey.

          LOOP AT lt_zfiwrt0020 INTO DATA(wl_zfiwrt0020).
            SELECT SINGLE *
              FROM j_1bnfe_active INTO @DATA(_active)
             WHERE docnum EQ @wl_zfiwrt0020-docnum.

            IF sy-subrc EQ 0.
              me->add_ref_nf( i_xml_header = i_xml_header
                              i_docnum     = wl_zfiwrt0020-docnum ).
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN OTHERS.


    ENDCASE.

    SELECT SINGLE * INTO @DATA(wa_zsdt0231)
      FROM zsdt0231
     WHERE docnum EQ @i_xml_header-docnum.

    CHECK sy-subrc IS INITIAL.

    SELECT * INTO TABLE @DATA(it_zsdt0232)
      FROM zsdt0232
     WHERE obj_key EQ @wa_zsdt0231-obj_key.

    CHECK sy-subrc IS INITIAL.

    FIELD-SYMBOLS: <xmlr_tab>  TYPE j1b_nf_xml_b12_tab.
    DATA: wl_doc_ref TYPE j1b_nf_xml_b12.
    ASSIGN ('(SAPLJ_1B_NFE)XMLR_TAB')  TO <xmlr_tab>.
    CHECK <xmlr_tab> IS ASSIGNED.

    LOOP AT it_zsdt0232 INTO DATA(wa_zsdt0232).
      CLEAR: wl_doc_ref.
      IF wa_zsdt0232-refnfe IS NOT INITIAL.
        wl_doc_ref-b12_refnfe = wa_zsdt0232-refnfe.

      ELSE.
        wl_doc_ref-b20_cuf   = wa_zsdt0232-cuf.
        wl_doc_ref-b20_aamm  = wa_zsdt0232-aamm.
        wl_doc_ref-b20_ie    = wa_zsdt0232-ie.
        wl_doc_ref-b20f_mod  = wa_zsdt0232-mod.
        wl_doc_ref-b20_serie = wa_zsdt0232-serie.
        wl_doc_ref-b20_nnf   = wa_zsdt0232-nnf.
        wl_doc_ref-b20_cnpj  = wa_zsdt0232-cnpj.
        wl_doc_ref-b20_cpf   = wa_zsdt0232-cpf.
      ENDIF.
      IF wl_doc_ref IS NOT INITIAL.
        wl_doc_ref-docnum = i_xml_header-docnum.
        APPEND wl_doc_ref TO <xmlr_tab>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_doc_volumes.

    DATA: BEGIN OF wa_item_nota,
            reftyp TYPE j_1breftyp, "Tipo referência = BI
            refkey TYPE j_1brefkey, "Referência ao documento de origem
            refitm TYPE j_1brefitm, "Item de referência ao documento de origem
            matnr  TYPE matnr, "Item de referência ao documento de origem
          END OF wa_item_nota,

          BEGIN OF wa_cab_fat,
            vbeln TYPE vbeln_vf, "Documento de faturamento
            fkart TYPE fkart   , "Tipo documento de faturamento
            waerk TYPE waerk   , "Moeda do documento SD
            kurrf TYPE kurrf   , "Taxa de câmbio para lançamentos FI
            netwr TYPE netwr   , "Valor líquido na moeda do documento
          END OF wa_cab_fat,

          BEGIN OF wa_itm_fat,
            vgbel TYPE vgbel, "Nº documento do documento de referência
            vgpos TYPE vgpos, "Nº item do item comercial modelo
            volum TYPE volum_15, "Volume
            voleh TYPE voleh, "Unidade de volume
            aubel TYPE vbeln_va, "Documento de vendas Ordem de Venda
          END OF wa_itm_fat,

          BEGIN OF wa_dox_exp,
            id_doc_exp       TYPE zid_doc, "ID Documento
            nr_registro_expo TYPE znr_reg, "Num. Registro Exportação
            id_nomeacao_tran TYPE zid_nom, "Nomeação de Transporte
            nr_dde           TYPE znr_dde, "Número da Declaração de Despacho de Exportação
            numero_due       TYPE zde_numero_due,
            id_due           TYPE zde_id_due,
          END OF wa_dox_exp.

    DATA: obs_moeda      TYPE string,
          obs_export     TYPE string,
          obs_bl         TYPE string,
          obs_navio      TYPE string,
          obs_dco        TYPE string,
          obs_leilao     TYPE string,
          test_taxa      TYPE c LENGTH 20,
          test_valor     TYPE c LENGTH 20,
          v_id_conhec    TYPE znom_conhec-id_conhec,
          wa_znom_conhec TYPE znom_conhec,
          test_navio     TYPE c LENGTH 20,
          ls_j_1binnad   TYPE vbpavb,
          lv_ntgew       TYPE ntgew_15,

          valor_d         TYPE p DECIMALS 0,
          valor_s         TYPE string,

          lv_brgew       TYPE brgew_15,
          vg_doc_exp     TYPE docnum,
          ls_retorno     TYPE zsdt_retlote,
          wa_item_ret    LIKE wa_item_nota,
          sl_produtor    TYPE zdco_produtor.

    DATA: tb_retorno      TYPE TABLE OF zsdt_retlote,
          tb_produtor     TYPE TABLE OF zdco_produtor,
          tb_zsdt_export  TYPE TABLE OF zsdt_export,
          tb_zsdt_retlote TYPE TABLE OF zsdt_retlote,
          l_matkl         TYPE mara-matkl,
          tb_matkl        TYPE TABLE OF rgsb4,
          wl_matkl        TYPE rgsb4,
          lwa_volume      TYPE J1B_NF_XML_T6.

    DATA: wa_zfiwrt0019 TYPE zfiwrt0019.

    DATA: lv_foreign TYPE xfeld.                              "V1.10

    FREE: tb_matkl, l_matkl.

    CLEAR: e_volume, lwa_volume.


*-CS2021001056-#68233-07.06.2022-JT-inicio
*-----------------------
*- set volume
*-----------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr      = 'MATKL_VOLUME_NFE'
        class      = '0000'
      TABLES
        set_values = tb_matkl.
*-CS2021001056-#68233-07.06.2022-JT-fim

    SELECT SINGLE reftyp refkey refitm matnr
      INTO wa_item_nota
      FROM j_1bnflin
     WHERE docnum EQ in_doc-docnum.

    SELECT SINGLE matkl
       INTO l_matkl
       FROM mara
      WHERE matnr = wa_item_nota-matnr.

    CLEAR: obs_export, obs_moeda, obs_bl, obs_navio.

    "READ TABLE tb_matkl INTO wl_matkl WITH KEY from = l_matkl.
    "CHECK ( sy-subrc eq 0 ) AND ( l_matkl is NOT INITIAL ) AND ( tb_matkl[] is NOT INITIAL ).

    "Volumes Nota Fiscal Esportação.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = in_doc-ntgew
        unit_in              = in_doc-gewei
        unit_out             = 'KG'
      IMPORTING
        output               = lv_ntgew
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.
    IF sy-subrc EQ 0.
      lwa_volume-t4_pesol = lv_ntgew.
    ENDIF.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = in_doc-brgew
        unit_in              = in_doc-gewei
        unit_out             = 'KG'
      IMPORTING
        output               = lv_brgew
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.

    IF sy-subrc EQ 0.
      lwa_volume-t4_pesob = lv_brgew.
    ENDIF.

    IF wa_item_nota-reftyp EQ 'BI'. "

      SELECT SINGLE vbeln fkart waerk kurrf netwr
        INTO wa_cab_fat
        FROM vbrk
       WHERE vbeln EQ wa_item_nota-refkey(10).

      "Se achou Documento de faturamento.
      IF sy-subrc EQ 0.

        "Busca Documento de Remessa.
        SELECT SINGLE vgbel vgpos volum voleh aubel
          INTO wa_itm_fat
          FROM vbrp
         WHERE vbeln EQ wa_cab_fat-vbeln.

        SELECT SINGLE msehl INTO lwa_volume-t4_esp
          FROM t006a
         WHERE msehi EQ wa_itm_fat-voleh
           AND spras EQ sy-langu.

        lwa_volume-t4_qvol = wa_itm_fat-volum.

      ENDIF.


    ELSEIF ( wa_item_nota-reftyp EQ 'ZW' ).

      SELECT SINGLE * FROM zfiwrt0019
        INTO wa_zfiwrt0019
      WHERE seq_lcto EQ wa_item_nota-refkey.

      IF ( sy-subrc EQ 0 ).
        lwa_volume-t4_qvol  = wa_zfiwrt0019-anzpk.
        lwa_volume-t4_esp   = wa_zfiwrt0019-shpunt.
        lwa_volume-t4_pesol = wa_zfiwrt0019-ntgew.
        lwa_volume-t4_pesob = wa_zfiwrt0019-brgew.
      ENDIF.

    ENDIF.

    IF ( lwa_volume-t4_pesob IS NOT INITIAL ) AND
       ( lwa_volume-t4_pesol IS NOT INITIAL ) AND
       ( lwa_volume-t4_qvol  IS INITIAL     ) AND
       ( lwa_volume-t4_esp   IS INITIAL     ) .

      IF ( in_doc-anzpk IS NOT INITIAL ).

        lwa_volume-t4_qvol = in_doc-anzpk.
        lwa_volume-t4_esp  = ''.

        SELECT SINGLE mseh6 INTO @lwa_volume-t4_esp
          FROM t006a
         WHERE msehi EQ @in_doc-shpunt
           AND spras EQ 'P'.

        lwa_volume-t4_esp = zcl_string=>tira_acentos( zcl_string=>upper( i_str = CONV #( lwa_volume-t4_esp ) ) ).

      ELSEIF ( in_doc-brgew IS NOT INITIAL ).

        valor_d = lv_brgew.
        valor_s = valor_d.

        REPLACE '.' IN valor_s WITH ' '.

        lwa_volume-t4_qvol = valor_s.

      ENDIF.

    ENDIF.


    IF ( lwa_volume-t4_pesob IS NOT INITIAL ) AND
       ( lwa_volume-t4_pesol IS NOT INITIAL ).
      e_volume = lwa_volume.
    ENDIF.


  ENDMETHOD.


  method FILL_REFERENCE_NF_FIM_ESP.

  "Internal Tables
  DATA: LT_VBRK             TYPE TABLE OF VBRK,
        LT_VBFA             TYPE TABLE OF VBFA,
        LT_ZDOC_EXP         TYPE TABLE OF ZDOC_EXP,
        LT_ZPROG_REME       TYPE TABLE OF ZNOM_PROG_REME,
        LT_ZNOM_REME_NOTAS  TYPE TABLE OF ZNOM_REME_NOTAS,
        LT_ZDOC_NF_PRODUTOR TYPE TABLE OF ZDOC_NF_PRODUTOR,
        LT_ZSDT_RETLOTE     TYPE TABLE OF ZSDT_RETLOTE,
        LT_ZSDT0053         TYPE TABLE OF ZSDT0053,
        LT_ZFIWRT0020       TYPE TABLE OF ZFIWRT0020.

  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.

  CREATE OBJECT LOBJ_UTIL.

  CHECK ( I_ITEM-CFOP(4) EQ '7501' ) OR ( I_ITEM-CFOP(4) EQ '3503' ).

  SELECT *
    FROM VBRK INTO TABLE LT_VBRK
   WHERE VBELN EQ I_ITEM-REFKEY(10).

  CHECK LT_VBRK[] IS NOT INITIAL.

  SELECT *
    FROM VBFA INTO TABLE LT_VBFA
     FOR ALL ENTRIES IN LT_VBRK
   WHERE VBELN EQ LT_VBRK-VBELN
     AND VBTYP_N EQ 'M'
     AND VBTYP_V EQ 'J'.

  CHECK LT_VBFA[] IS NOT INITIAL.

  SELECT *
    FROM ZDOC_EXP INTO TABLE LT_ZDOC_EXP
     FOR ALL ENTRIES IN LT_VBFA
   WHERE VBELN EQ LT_VBFA-VBELV.

  CHECK LT_ZDOC_EXP[] IS NOT INITIAL.

  SELECT *
    FROM ZDOC_NF_PRODUTOR INTO TABLE LT_ZDOC_NF_PRODUTOR
     FOR ALL ENTRIES IN LT_ZDOC_EXP
   WHERE VBELN EQ LT_ZDOC_EXP-VBELN.

  CHECK ( LT_ZDOC_NF_PRODUTOR[] IS NOT INITIAL ).

  READ TABLE LT_VBRK INTO DATA(LW_VBRK) INDEX 1.

  CHECK ( SY-SUBRC EQ 0 ).

  CASE LW_VBRK-FKART.

    WHEN: 'ZEXI'.

      READ TABLE LT_VBFA     INTO DATA(LW_VBFA)     WITH KEY VBELN = LW_VBRK-VBELN.
      CHECK ( SY-SUBRC EQ 0 ).

      READ TABLE LT_ZDOC_EXP INTO DATA(LW_ZDOC_EXP) WITH KEY VBELN = LW_VBFA-VBELV.
      CHECK ( SY-SUBRC EQ 0 ).

      LOOP AT LT_ZDOC_NF_PRODUTOR INTO DATA(LW_ZDOC_NF_PRODUTOR).

        SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE) WHERE DOCNUM EQ @LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD.
        IF SY-SUBRC EQ 0.

          ME->ADD_REF_NF( I_XML_HEADER = I_XML_HEADER
                          I_DOCNUM     = LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD ).

          "Check se é uma entrada Propria
          SELECT SINGLE * INTO @DATA(LW_ZLEST0147)
            FROM ZLEST0147 AS A
           WHERE A~DOCNUM          EQ @LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD
             AND A~ENTRADA_PROPRIA EQ @ABAP_TRUE
             AND EXISTS ( SELECT ID_RECEPCAO
                            FROM ZLEST0146 AS B
                           WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                             AND B~CANCEL      EQ @ABAP_FALSE ).
          IF SY-SUBRC EQ 0.
            ME->ADD_REF_NF_CCT( I_XML_HEADER = I_XML_HEADER
                                I_ZLEST0147  = LW_ZLEST0147 ).
          ENDIF.

        ELSE.
          ME->ADD_REF_NF( I_XML_HEADER = I_XML_HEADER
                          I_DOCNUM     = LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD ).
        ENDIF.

      ENDLOOP.

  ENDCASE.











  endmethod.


  method FORMATA_TEXTO_VEICULO.

    CLEAR: R_TEXTO.

    CONCATENATE I_CTE_TRANS-PC_VEICULO '-' I_CTE_TRANS-CD_CIDADE INTO DATA(_VL_XTEXTO1).
    CONCATENATE I_CTE_TRANS-CD_UF '-' I_CTE_TRANS-PROP_NOME '- CNPJ:' I_CTE_TRANS-PROP_CNPJ INTO DATA(_VL_XTEXTO2).
    CONCATENATE _VL_XTEXTO1 '/' _VL_XTEXTO2 INTO R_TEXTO SEPARATED BY SPACE.

  endmethod.


METHOD function_classification.

  DATA: tl_class       TYPE TABLE OF sclass          ,
        tl_objectdata  TYPE TABLE OF clobjdat        ,
        sl_object      TYPE clobjdat                 ,
        vl_class       TYPE klah-class               ,
        vl_classtext   TYPE rmclm-anzukz VALUE 'X'   ,
        vl_classtype   TYPE klah-klart               ,
        vl_clint       TYPE klah-clint               ,
        vl_object      TYPE ausp-objek               ,
        vl_objecttable TYPE tcla-obtab   VALUE 'MCH1',
        vl_obtab       TYPE tclao-obtab              ,
        vl_cuobj       TYPE inob-cuobj               ,
        vl_textos      TYPE string                   ,
        lv_space       TYPE char22 VALUE '                      ',
        vl_aux         TYPE string                   .

* Material + Lote
  CONCATENATE p_matnr
              p_charg
         INTO vl_object SEPARATED BY lv_space.
  vl_obtab = 'MCH1'.

  SELECT SINGLE tcla~klart
    INTO vl_classtype
    FROM tcla JOIN tclao
      ON tcla~klart = tclao~klart
  WHERE tcla~obtab    EQ 'MCHA'
    AND tcla~intklart EQ space
    AND tcla~multobj  EQ 'X'
    AND tclao~obtab   EQ vl_obtab.

  SELECT cuobj UP TO 1 ROWS
    FROM inob
    INTO vl_cuobj
  WHERE klart EQ vl_classtype
    AND obtab EQ vl_obtab
    AND objek EQ vl_object.
    EXIT.
  ENDSELECT.

  SELECT clint UP TO 1 ROWS
    FROM kssk
    INTO vl_clint
  WHERE objek EQ vl_cuobj
    AND mafid EQ 'O'
    AND klart EQ vl_classtype.
    EXIT.
  ENDSELECT.

  SELECT SINGLE class
    FROM klah
    INTO vl_class
  WHERE clint EQ vl_clint.

  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      class              = vl_class
      classtext          = vl_classtext
      classtype          = vl_classtype
      clint              = vl_clint
      object             = vl_object
      objecttable        = vl_objecttable
    TABLES
      t_class            = tl_class
      t_objectdata       = tl_objectdata
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.

  DELETE tl_objectdata WHERE ausp1 = '?'.

  CHECK NOT tl_objectdata[] IS INITIAL.

  LOOP AT tl_objectdata INTO sl_object.


    IF ( sl_object-smbez NE 'POSICAO' ) AND ( sl_object-smbez NE 'PILHA' ) OR ( sl_object-smbez NE 'PILHA' ) AND ( sl_object-smbez NE 'POSICAO' ).

      IF sy-tabix EQ 1.
        CONCATENATE sl_object-smbez
                    sl_object-ausp1
               INTO vl_textos SEPARATED BY ':'.
      ELSE.
        CONCATENATE sl_object-smbez
                    sl_object-ausp1
               INTO vl_aux SEPARATED BY ':'.
        CONCATENATE vl_textos
                    vl_aux
               INTO vl_textos SEPARATED BY space.
      ENDIF.

      CLEAR: sl_object,
             vl_aux   .

    ENDIF.
  ENDLOOP.

  IF NOT vl_textos IS INITIAL.
    p_textos = vl_textos.
  ENDIF.

ENDMETHOD.


METHOD function_partner.

  REFRESH: t_sadrvb,
           t_vbpavb.

  CALL FUNCTION 'SD_PARTNER_READ'
    EXPORTING
      f_vbeln  = p_vbeln
      object   = 'VBPA'
    TABLES
      i_xvbadr = t_sadrvb
      i_xvbpa  = t_vbpavb.

ENDMETHOD.


  METHOD GET_DADOS_CTE_ANTERIOR.

    DATA: w_iddocantele TYPE j_1bcte_int_iddocantele,
          ls_cteproc    TYPE edoc_br_cte_if_root,
          lo_parser     TYPE REF TO if_edoc_source_parser,
          lo_params     TYPE REF TO cl_edoc_br_create_entity_param,
          md_xml        TYPE REF TO data.

    FIELD-SYMBOLS: <ls_cte> TYPE any.

    FREE: r_cteproc.

    READ TABLE it_iddocantele INTO w_iddocantele INDEX 1.

    CHECK sy-subrc = 0.

*--------------------------------
*-- obter XML
*--------------------------------
    zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( w_iddocantele-chave )
                                                           i_direcao = CONV #( 'IN' )
                                                 IMPORTING e_xml_raw = DATA(o_xml_raw) ).

    CHECK o_xml_raw IS NOT INITIAL.

*-------------------------------------
* Parse XML
*-------------------------------------
    TRY.
        CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
        CREATE OBJECT lo_params
          EXPORTING
            iv_xml = o_xml_raw.

        lo_parser->load_source( lo_params->get_xml( ) ).
        md_xml   = lo_parser->parse_to_ddic( ls_cteproc ).

        ASSIGN md_xml->*    TO <ls_cte>.
        r_cteproc            = <ls_cte>.

      CATCH cx_edocument.
        EXIT.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_DADOS_TRANSPORTADORA.

    FIELD-SYMBOLS: <fs_xmlh> TYPE j1b_nf_xml_header.

    DATA: t_sadrvb TYPE TABLE OF sadrvb,
          t_vbpavb TYPE TABLE OF vbpavb.

    TYPES: y_vbtyp_v TYPE RANGE OF vbfa-vbtyp_v.

    DATA: r_vbtyp_v TYPE y_vbtyp_v.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'J' ) TO r_vbtyp_v.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'M' ) TO r_vbtyp_v.

    ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.

    CHECK <fs_xmlh> IS ASSIGNED.

    CLEAR: r_lfa1_transp.

    if ( <fs_xmlh>-t1_cnpj IS NOT INITIAL AND
         <fs_xmlh>-t1_cnpj NE '00000000000000' ). "BUG SOLTO 183042 - RGA - 30.06.2025
      SELECT SINGLE *
        from lfa1 INTO @DATA(lwa_lfa1)
       WHERE stcd1 = @<fs_xmlh>-t1_cnpj.

      IF sy-subrc eq 0.
        r_lfa1_transp = lwa_lfa1.
      ENDIF.

      RETURN.
    endif.

    if ( <fs_xmlh>-t1_cpf IS NOT INITIAL  ).

      SELECT SINGLE *
        from lfa1 INTO lwa_lfa1
       WHERE stcd2 = <fs_xmlh>-t1_cpf.

      IF sy-subrc eq 0.
        r_lfa1_transp = lwa_lfa1.
      ENDIF.

      RETURN.

    endif.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(vj_1bnflin)
     WHERE docnum EQ @<fs_xmlh>-docnum.

    CHECK ( sy-subrc EQ 0 ) AND ( vj_1bnflin-refkey is NOT INITIAL ).

    SELECT SINGLE *
      FROM vbfa INTO @DATA(vvbfa)
     WHERE vbeln   EQ @vj_1bnflin-refkey(10)
       AND posnn   EQ @vj_1bnflin-refitm
       AND vbtyp_v IN @r_vbtyp_v.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'SD_PARTNER_READ'
        EXPORTING
          f_vbeln  = vvbfa-vbelv
          object   = 'VBPA'
        TABLES
          i_xvbadr = t_sadrvb
          i_xvbpa  = t_vbpavb.

      DELETE t_vbpavb WHERE parvw NE 'SP'.

      CHECK t_vbpavb[] IS NOT INITIAL.

      READ TABLE t_vbpavb INTO DATA(sl_xvbpa) INDEX 1.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(s_lfa1)
       WHERE lifnr EQ @sl_xvbpa-lifnr.

      CHECK sy-subrc EQ 0.

      r_lfa1_transp = s_lfa1.

    ELSE.

      SELECT SINGLE *
        FROM zfiwrt0019 INTO @DATA(lw_zfiwrt0019)
       WHERE seq_lcto EQ @vj_1bnflin-refkey(10).

      CHECK ( sy-subrc EQ 0 ) AND ( lw_zfiwrt0019-lifnr is NOT INITIAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_zfiwrt0019-lifnr
        IMPORTING
          output = lw_zfiwrt0019-lifnr.

      SELECT SINGLE *
        FROM lfa1 INTO s_lfa1
       WHERE lifnr EQ lw_zfiwrt0019-lifnr.

      CHECK ( sy-subrc EQ 0 ).

      r_lfa1_transp = s_lfa1.

    ENDIF.


  ENDMETHOD.


  method GET_DADOS_VEICULO_NFE.

  DATA: WL_ZFIWRT0019 TYPE ZFIWRT0019,
        WL_LFA1       TYPE LFA1.

  CLEAR: E_CODIGO_MOT,
         E_CPF_MOT,
         E_NOME_MOT,
         E_PLACA,
         E_PLACA_UF,
         E_PLACA1,
         E_PLACA1_UF,
         E_PLACA2,
         E_PLACA2_UF,
         E_PLACA3,
         E_PLACA3_UF,
         E_RNTRC.

  SELECT SINGLE *
    FROM J_1BNFLIN INTO @DATA(_WL_LIN)
   WHERE DOCNUM EQ @IN_XML_HEADER-DOCNUM.

  CHECK ( SY-SUBRC = 0 ).

  IF ( IN_XML_HEADER-T2_PLACA IS NOT INITIAL ).
    E_PLACA    = IN_XML_HEADER-T2_PLACA.
    E_PLACA_UF = IN_XML_HEADER-T2_UF1.
    E_RNTRC    = IN_XML_HEADER-T2_RNTC.
  ELSE.

    CLEAR: WL_ZFIWRT0019, WL_LFA1.

    IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
      SELECT SINGLE *
        FROM ZFIWRT0019 INTO WL_ZFIWRT0019
       WHERE SEQ_LCTO EQ _WL_LIN-REFKEY(10).

      IF SY-SUBRC = 0.
        SELECT SINGLE *
          FROM LFA1 INTO WL_LFA1
         WHERE LIFNR EQ WL_ZFIWRT0019-LIFNR.
      ENDIF.
    ENDIF.

    IF ( WL_ZFIWRT0019 IS NOT INITIAL ) AND ( WL_ZFIWRT0019-PLACA IS NOT INITIAL ).
      E_PLACA    = WL_ZFIWRT0019-PLACA.
      E_PLACA_UF = WL_ZFIWRT0019-UFPLACA.
      E_RNTRC    = WL_LFA1-BAHNS.
    ELSE.

      IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
        SELECT SINGLE *
          FROM VBFA INTO @DATA(_WL_VBFA)
         WHERE VBELN   EQ @_WL_LIN-REFKEY(10)
           AND POSNN   EQ @_WL_LIN-REFITM
           AND VBTYP_V EQ 'J'.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
            FROM ZSDT0001 INTO @DATA(_WL_ZSDT0001)
           WHERE DOC_REM      EQ @_WL_VBFA-VBELV
             AND TP_MOVIMENTO EQ 'S'.
          IF ( SY-SUBRC EQ 0 ) AND ( _WL_ZSDT0001-PLACA_CAV IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ZLEST0002 INTO @DATA(_WL_ZLEST0002)
             WHERE PC_VEICULO EQ @_WL_ZSDT0001-PLACA_CAV.
            IF ( SY-SUBRC EQ 0 ) OR ( _WL_ZSDT0001-REGION IS NOT INITIAL ).
              CLEAR: WL_LFA1.
              IF _WL_ZLEST0002-PROPRIETARIO IS NOT INITIAL.
                SELECT SINGLE *
                  FROM LFA1 INTO WL_LFA1
                 WHERE LIFNR EQ _WL_ZLEST0002-PROPRIETARIO.
              ENDIF.
              IF ( SY-SUBRC EQ 0 ) OR ( _WL_ZSDT0001-REGION IS NOT INITIAL ).
                IF _WL_ZSDT0001-REGION IS NOT INITIAL.
                  _WL_ZLEST0002-CD_UF = _WL_ZSDT0001-REGION.
                ENDIF.

                "Placa Cavalo |----------------------------------------------|

                E_PLACA      = _WL_ZSDT0001-PLACA_CAV.
                E_PLACA_UF   = _WL_ZLEST0002-CD_UF.
                E_RNTRC      = WL_LFA1-BAHNS.

                "Placa Reboque 1 |-------------------------------------------|
                E_PLACA1 = _WL_ZSDT0001-PLACA_CAR1.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO @DATA(_WL_ZLEST0002_TMP)
                 WHERE PC_VEICULO EQ @E_PLACA1.

                IF ( SY-SUBRC EQ 0 ) AND ( E_PLACA1 IS NOT INITIAL ).
                  E_PLACA1_UF = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.

                "Placa Reboque 2 |-------------------------------------------|
                E_PLACA2 = _WL_ZSDT0001-PLACA_CAR2.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO _WL_ZLEST0002_TMP
                 WHERE PC_VEICULO EQ E_PLACA2.

                IF ( SY-SUBRC EQ 0 ) AND ( E_PLACA2 IS NOT INITIAL ).
                  E_PLACA2_UF = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.

                "Placa Reboque 3 |-------------------------------------------|

                E_PLACA3 = _WL_ZSDT0001-PLACA_CAR3.

                SELECT SINGLE *
                  FROM ZLEST0002 INTO _WL_ZLEST0002_TMP
                 WHERE PC_VEICULO EQ E_PLACA3.

                IF ( SY-SUBRC EQ 0 ) AND ( E_PLACA3 IS NOT INITIAL ).
                  E_PLACA3_UF = _WL_ZLEST0002_TMP-CD_UF.
                ENDIF.

                E_CODIGO_MOT = |{ _WL_ZSDT0001-MOTORISTA ALPHA = IN }|.

                SELECT SINGLE *
                  FROM LFA1 INTO @DATA(_LFA1_MOT)
                 WHERE LIFNR EQ @E_CODIGO_MOT.

                IF ( SY-SUBRC EQ 0 ) AND ( E_CODIGO_MOT IS NOT INITIAL ).
                  E_CPF_MOT  = _LFA1_MOT-STCD2.
                  E_NOME_MOT = _LFA1_MOT-NAME1.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF. " IF _WL_LIN-REFKEY(10) IS NOT INITIAL.
    ENDIF.
  ENDIF.


  endmethod.


  METHOD GET_RESP_TECNICO.

    CLEAR: R_RESPONSAVEL.

    CONSTANTS: C_CSRT TYPE ZE_TIPO_NFE VALUE 'CSRT'.

    IF I_UF IS INITIAL.

      SELECT SINGLE REGIO INTO @DATA(VL_REGION) FROM T001W WHERE WERKS EQ @I_BRANCH.

      IF SY-SUBRC IS NOT INITIAL.
        SELECT SINGLE T001W~REGIO INTO @VL_REGION
          FROM T001W
         INNER JOIN T001K ON T001W~BWKEY EQ T001K~BWKEY
         WHERE T001K~BUKRS EQ @I_BUKRS
           AND T001W~J_1BBRANCH EQ @I_BRANCH.
      ENDIF.

    ELSE.
      VL_REGION = I_UF.
    ENDIF.

    SELECT SINGLE *
      FROM ZTNFE_ADD_RESP
      INTO @DATA(SL_ADD_RESP)
     WHERE REGIO EQ @VL_REGION
       AND TIPO  EQ @C_CSRT.

    IF SY-SUBRC IS INITIAL AND SL_ADD_RESP-CNPJ IS NOT INITIAL.
      R_RESPONSAVEL-CNPJ      = SL_ADD_RESP-CNPJ.
      R_RESPONSAVEL-X_CONTATO = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( SL_ADD_RESP-CONTACT ) ) ).
      R_RESPONSAVEL-EMAIL     = SL_ADD_RESP-EMAIL.
      R_RESPONSAVEL-FONE      = SL_ADD_RESP-PHONE.
      IF SL_ADD_RESP-IDCSRT NE '00'.
        R_RESPONSAVEL-ID_CSRT   = SL_ADD_RESP-IDCSRT.
        R_RESPONSAVEL-CSRT      = SL_ADD_RESP-CSRT.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TEXTOS_CONTRIB_UF.

    TYPES: BEGIN OF TY_ZFIWRT0009,
             MATKL TYPE MARA-MATKL.
             INCLUDE TYPE ZFIWRT0009.
           TYPES  END OF TY_ZFIWRT0009.

    DATA: VL_VBELN     TYPE VBRP-VBELN,
          WL_VBRP      TYPE VBRP,
          BASE(20)     TYPE C,
          ALIQUOTA(20) TYPE C,
          VLR_FET(20)  TYPE C,
          V_DMBTR_CTB  TYPE BSIS-DMBTR.


    "DATA  WL_ZFIWRT0009 TYPE TY_ZFIWRT0009. " WITH HEADER LINE.

    CLEAR: R_TEXTOS.


    CHECK I_LIN-REFKEY IS NOT INITIAL.

    CASE I_LIN-REFTYP.

      WHEN 'BI'.

        SELECT SINGLE  *
          FROM VBRP   INTO WL_VBRP
         WHERE VBELN EQ I_LIN-REFKEY(10)
           AND POSNR EQ I_LIN-REFITM.

        CHECK SY-SUBRC EQ 0.

        SELECT SINGLE  *
          FROM VBRK INTO @DATA(WL_VBRK)
         WHERE VBELN EQ @WL_VBRP-VBELN.

        CHECK SY-SUBRC EQ 0.

        SELECT SINGLE *
         FROM J_1BBRANCH INTO @DATA(_WL_BRANCH)
        WHERE BUKRS  = @WL_VBRK-VKORG
           AND BRANCH = @WL_VBRP-WERKS.

        CHECK SY-SUBRC EQ 0.

        SELECT SINGLE *
          FROM ADRC INTO @DATA(_WL_ADRC)
         WHERE ADDRNUMBER EQ @_WL_BRANCH-ADRNR.

        CHECK ( SY-SUBRC = 0 ) AND  ( _WL_BRANCH-ADRNR IS NOT INITIAL ) AND ( _WL_ADRC-REGION EQ 'TO' ).


        SELECT SINGLE * FROM ZSDT0155 INTO @DATA(WA_0155)
         WHERE BUKRS EQ @WL_VBRK-VKORG
                      AND AUART EQ @WL_VBRK-FKART
                       AND REGION EQ 'TO'
                       AND DT_INI    <= @SY-DATUM
                       AND DT_FIM    >= @SY-DATUM
                       AND CANCELADO = ''
                       AND ( MATNR EQ @WL_VBRP-MATNR  OR
                             MATKL EQ @WL_VBRP-MATKL )
                       AND TP_TRIB EQ '08' " FET
                       AND CALC_VLR_PER EQ @ABAP_TRUE.

        CHECK ( SY-SUBRC = 0 ).
        V_DMBTR_CTB  = ( WL_VBRP-NETWR * WA_0155-PERC_UPF ) / 100.

        WRITE  WL_VBRP-NETWR TO BASE.
        WRITE  WA_0155-PERC_UPF TO ALIQUOTA .
        WRITE  V_DMBTR_CTB TO VLR_FET.

        CONDENSE BASE NO-GAPS.
        CONDENSE ALIQUOTA NO-GAPS.
        CONDENSE VLR_FET NO-GAPS.

        CONCATENATE '/ Base Cálculo FET - :' BASE '/ Alíquota FET - ' ALIQUOTA  '/ Valor FET -' VLR_FET INTO R_TEXTOS SEPARATED BY SPACE.

      WHEN 'ZW'.

        SELECT SINGLE *
          FROM ZFIWRT0008 INTO @DATA(WL_ZFIWT00008)
         WHERE SEQ_LCTO EQ @I_LIN-REFKEY(10).

        CHECK SY-SUBRC EQ 0 .

        SELECT SINGLE * FROM ZFIWRT0001 INTO @DATA(WL_ZFIWT00001)
         WHERE OPERACAO EQ @WL_ZFIWT00008-OPERACAO
          AND LM_CONTRI_UF = 'S'.

        CHECK SY-SUBRC EQ 0 .

        SELECT SINGLE *
            FROM  ZFIWRT0009 INTO @DATA(WL_ZFIWRT0009)
           WHERE SEQ_LCTO EQ @WL_ZFIWT00008-SEQ_LCTO.

        CHECK SY-SUBRC EQ 0 .

        SELECT SINGLE MATKL
          FROM MARA
          INTO @DATA(P_MATKL)
          WHERE MATNR = @WL_ZFIWRT0009-MATNR.

        SELECT SINGLE *
                 FROM J_1BBRANCH INTO @DATA(_WL_BRANCH_ZNFW)
                WHERE BUKRS  = @WL_ZFIWT00008-BUKRS
                   AND BRANCH = @WL_ZFIWT00008-BRANCH.


        CHECK SY-SUBRC EQ 0.

        SELECT SINGLE *
          FROM ADRC INTO @DATA(_WL_ADDRC_ZNFW)
         WHERE ADDRNUMBER EQ @_WL_BRANCH_ZNFW-ADRNR.

        CHECK ( SY-SUBRC = 0 ) AND  ( _WL_BRANCH_ZNFW-ADRNR IS NOT INITIAL ) AND ( _WL_ADDRC_ZNFW-REGION EQ 'TO' ).

        SELECT SINGLE * FROM ZSDT0155 INTO @DATA(WA_0155_ZNFW)
        WHERE BUKRS =  @WL_ZFIWT00008-BUKRS
         AND LANC_ZNFW = 'X'
         AND REGION = 'TO'
         AND DT_INI    <= @SY-DATUM
         AND DT_FIM    >= @SY-DATUM
         AND CANCELADO = ''
         AND ( MATNR EQ @WL_ZFIWRT0009-MATNR  OR
               MATKL EQ @P_MATKL )
         AND TP_TRIB EQ '08' " FET
         AND CALC_VLR_PER EQ @ABAP_TRUE.


        CHECK ( SY-SUBRC = 0 ).
        V_DMBTR_CTB  = ( WL_ZFIWRT0009-NETWR * WA_0155_ZNFW-PERC_UPF ) / 100.

        WRITE  WL_ZFIWRT0009-NETWR TO BASE.
        WRITE  WA_0155_ZNFW-PERC_UPF TO ALIQUOTA .
        WRITE  V_DMBTR_CTB TO VLR_FET.

        CONDENSE BASE NO-GAPS.
        CONDENSE ALIQUOTA NO-GAPS.
        CONDENSE VLR_FET NO-GAPS.

        CONCATENATE '/ Base Cálculo FET - :' BASE '/ Alíquota FET - ' ALIQUOTA  '/ Valor FET -' VLR_FET INTO R_TEXTOS SEPARATED BY SPACE.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_TEXTOS_NF_INDUSTRIALIZACAO.

    DATA: T_ZSDT0249           TYPE TABLE OF ZSDT0249,
          T_ZFIWRT0020         TYPE TABLE OF ZFIWRT0020,
          T_DOC_DEV            TYPE TABLE OF J_1BNFDOC,
          T_DOC_DEV_AGR_DT     TYPE TABLE OF J_1BNFDOC,

          V_NR_NF              TYPE C LENGTH 9,
          V_SERIE_NF           TYPE C LENGTH 3,
          V_NR_SERIE_NF        TYPE C LENGTH 12,
          V_DT_NF              TYPE C LENGTH 12,
          V_SEPARADOR          TYPE C LENGTH 1,
          V_SEPARADOR_HIFEN    TYPE C LENGTH 1,
          V_TEXTOS_REMETENTE   TYPE STRING,
          V_TEXTO_CIDADE_UF    TYPE STRING,
          V_TEXTO_IE           TYPE STRING,
          V_TEXTO_UF           TYPE STRING.

    DATA: WL_ADDRESS_BRANCH   TYPE SADR,
          WL_BRANCH_RECORD    TYPE J_1BBRANCH.


    CLEAR: R_TEXTOS.

    CHECK I_LIN-REFKEY IS NOT INITIAL.

    CASE I_LIN-REFTYP.

      WHEN 'ZW'.

*---------------------------------------------------------------------------------------------------------------------------------------*
*       NF Devolução
*---------------------------------------------------------------------------------------------------------------------------------------*

        SELECT SINGLE *
          FROM ZSDT0252 INTO @DATA(WL_ZSDT0252)
         WHERE SEQLCTO_DEVOL EQ @I_LIN-REFKEY(10).

        IF ( SY-SUBRC EQ 0 ).

          CLEAR: T_ZSDT0249[], T_DOC_DEV[].

          SELECT SINGLE *
            FROM J_1BBRANCH INTO @DATA(WL_BRANCH)
           WHERE BRANCH EQ @WL_ZSDT0252-BRANCH.

          CHECK SY-SUBRC EQ 0.

          SELECT *
            FROM ZSDT0249 INTO TABLE T_ZSDT0249
           WHERE ID_BOLETIM EQ WL_ZSDT0252-ID_BOLETIM
             AND BRANCH     EQ WL_ZSDT0252-BRANCH
             AND CHARG      EQ WL_ZSDT0252-CHARG
             AND ID_AGRP    EQ WL_ZSDT0252-ID_AGRP.

          CHECK T_ZSDT0249[] IS NOT INITIAL.

          SELECT *
            FROM J_1BNFDOC INTO TABLE T_DOC_DEV
             FOR ALL ENTRIES IN T_ZSDT0249
           WHERE DOCNUM EQ T_ZSDT0249-DOCNUM.

          CHECK T_DOC_DEV[] IS NOT INITIAL.

          T_DOC_DEV_AGR_DT[] = T_DOC_DEV[].

          SORT T_DOC_DEV_AGR_DT BY DOCDAT.
          DELETE ADJACENT DUPLICATES FROM T_DOC_DEV_AGR_DT COMPARING DOCDAT.

           V_SEPARADOR_HIFEN = ABAP_FALSE.

          LOOP AT T_DOC_DEV_AGR_DT INTO DATA(WL_NF_DEV_DT).

            LOOP AT T_DOC_DEV INTO DATA(WL_DOC_DEV) WHERE DOCDAT EQ WL_NF_DEV_DT-DOCDAT.

              V_NR_NF        = WL_DOC_DEV-NFENUM.
              V_SERIE_NF     = WL_DOC_DEV-SERIES.

              V_NR_NF        = |{ V_NR_NF    ALPHA = OUT  }|.
              V_SERIE_NF     = |{ V_SERIE_NF ALPHA = OUT  }|.

              CONCATENATE V_NR_NF '-' V_SERIE_NF  INTO V_NR_SERIE_NF.

              IF R_TEXTOS IS INITIAL.
                CONCATENATE 'Retorno ref. NFs' V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ELSE.

                IF V_SEPARADOR_HIFEN EQ ABAP_TRUE.
                  V_SEPARADOR       = '-'.
                  V_SEPARADOR_HIFEN = ABAP_FALSE.
                ELSE.
                  V_SEPARADOR = ','.
                ENDIF.

                CONCATENATE R_TEXTOS V_SEPARADOR V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ENDIF.

            ENDLOOP.

            CONCATENATE WL_NF_DEV_DT-DOCDAT+6(2) '/' WL_NF_DEV_DT-DOCDAT+4(2) '/' WL_NF_DEV_DT-DOCDAT(4) INTO V_DT_NF.

            CONCATENATE R_TEXTOS 'de' V_DT_NF INTO R_TEXTOS SEPARATED BY SPACE.

            V_SEPARADOR_HIFEN = ABAP_TRUE.

          ENDLOOP.

          CHECK R_TEXTOS IS NOT INITIAL.

          CLEAR: WL_ADDRESS_BRANCH, WL_BRANCH_RECORD, V_TEXTOS_REMETENTE.

          CALL FUNCTION 'J_1B_BRANCH_READ'
            EXPORTING
              BRANCH           = WL_BRANCH-BRANCH
              COMPANY          = WL_BRANCH-BUKRS
            IMPORTING
              ADDRESS          = WL_ADDRESS_BRANCH
              BRANCH_RECORD    = WL_BRANCH_RECORD.

          CHECK SY-SUBRC EQ 0.

          CONCATENATE WL_ADDRESS_BRANCH-ORT01 '-' WL_ADDRESS_BRANCH-REGIO INTO V_TEXTO_CIDADE_UF.
          CONCATENATE 'IE:' WL_BRANCH_RECORD-STATE_INSC                   INTO V_TEXTO_IE.

          CONCATENATE WL_ADDRESS_BRANCH-NAME1   "Nome
                      WL_ADDRESS_BRANCH-STRAS   "Endereço
                      WL_ADDRESS_BRANCH-ORT02   "Bairro
                      V_TEXTO_CIDADE_UF         "Cidade-UF
                      V_TEXTO_IE                "IE
                 INTO V_TEXTOS_REMETENTE SEPARATED BY ','.

          CONCATENATE R_TEXTOS 'Remetidas por' V_TEXTOS_REMETENTE INTO R_TEXTOS SEPARATED BY SPACE.

          RETURN.

        ENDIF.

*---------------------------------------------------------------------------------------------------------------------------------------*
*       NF Industrialização
*---------------------------------------------------------------------------------------------------------------------------------------*

        SELECT SINGLE *
          FROM ZSDT0252 INTO WL_ZSDT0252
         WHERE SEQLCTO_IND EQ I_LIN-REFKEY(10).

        IF SY-SUBRC EQ 0.

          CLEAR: T_ZFIWRT0020[], T_DOC_DEV[].

          SELECT *
            FROM ZFIWRT0020 INTO TABLE T_ZFIWRT0020
           WHERE SEQ_LCTO EQ I_LIN-REFKEY(10).

          CHECK T_ZFIWRT0020[] IS NOT INITIAL.

          SELECT *
            FROM J_1BNFDOC INTO TABLE T_DOC_DEV
             FOR ALL ENTRIES IN T_ZFIWRT0020
           WHERE DOCNUM EQ T_ZFIWRT0020-DOCNUM.

          CHECK T_DOC_DEV[] IS NOT INITIAL.

          T_DOC_DEV_AGR_DT[] = T_DOC_DEV[].

          SORT T_DOC_DEV_AGR_DT BY DOCDAT.
          DELETE ADJACENT DUPLICATES FROM T_DOC_DEV_AGR_DT COMPARING DOCDAT.

          V_SEPARADOR_HIFEN = ABAP_FALSE.

          LOOP AT T_DOC_DEV_AGR_DT INTO WL_NF_DEV_DT.

            LOOP AT T_DOC_DEV INTO WL_DOC_DEV WHERE DOCDAT EQ WL_NF_DEV_DT-DOCDAT.

              V_NR_NF        = WL_DOC_DEV-NFENUM.
              V_SERIE_NF     = WL_DOC_DEV-SERIES.

              V_NR_NF        = |{ V_NR_NF    ALPHA = OUT  }|.
              V_SERIE_NF     = |{ V_SERIE_NF ALPHA = OUT  }|.

              CONCATENATE V_NR_NF '-' V_SERIE_NF  INTO V_NR_SERIE_NF.

              IF R_TEXTOS IS INITIAL.
                CONCATENATE 'Industrialização ref. NF de Devolução' V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ELSE.

                IF V_SEPARADOR_HIFEN EQ ABAP_TRUE.
                  V_SEPARADOR       = '-'.
                  V_SEPARADOR_HIFEN = ABAP_FALSE.
                ELSE.
                  V_SEPARADOR = ','.
                ENDIF.

                CONCATENATE R_TEXTOS V_SEPARADOR V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ENDIF.

            ENDLOOP.

            CONCATENATE WL_NF_DEV_DT-DOCDAT+6(2) '/' WL_NF_DEV_DT-DOCDAT+4(2) '/' WL_NF_DEV_DT-DOCDAT(4) INTO V_DT_NF.

            CONCATENATE R_TEXTOS 'de' V_DT_NF INTO R_TEXTOS SEPARATED BY SPACE.

            V_SEPARADOR_HIFEN = ABAP_TRUE.

          ENDLOOP.

          RETURN.
        ENDIF.

*---------------------------------------------------------------------------------------------------------------------------------------*
*       NF Remessa Conta e Ordem 01 02 03
*---------------------------------------------------------------------------------------------------------------------------------------*

        SELECT SINGLE *
          FROM ZSDT0252 INTO WL_ZSDT0252
         WHERE SEQLCTO_RCO_01 EQ I_LIN-REFKEY(10).

        IF SY-SUBRC NE 0.
          SELECT SINGLE *
            FROM ZSDT0252 INTO WL_ZSDT0252
           WHERE SEQLCTO_RCO_02 EQ I_LIN-REFKEY(10).
        ENDIF.

        IF SY-SUBRC NE 0.
          SELECT SINGLE *
            FROM ZSDT0252 INTO WL_ZSDT0252
           WHERE SEQLCTO_RCO_03 EQ I_LIN-REFKEY(10).
        ENDIF.

        IF SY-SUBRC EQ 0.

          CLEAR: T_ZFIWRT0020[], T_DOC_DEV[].

          SELECT *
            FROM ZFIWRT0020 INTO TABLE T_ZFIWRT0020
           WHERE SEQ_LCTO EQ I_LIN-REFKEY(10).

          CHECK T_ZFIWRT0020[] IS NOT INITIAL.

          SELECT *
            FROM J_1BNFDOC INTO TABLE T_DOC_DEV
             FOR ALL ENTRIES IN T_ZFIWRT0020
           WHERE DOCNUM EQ T_ZFIWRT0020-DOCNUM.

          CHECK T_DOC_DEV[] IS NOT INITIAL.

          T_DOC_DEV_AGR_DT[] = T_DOC_DEV[].

          SORT T_DOC_DEV_AGR_DT BY DOCDAT.
          DELETE ADJACENT DUPLICATES FROM T_DOC_DEV_AGR_DT COMPARING DOCDAT.

          V_SEPARADOR_HIFEN = ABAP_FALSE.

          LOOP AT T_DOC_DEV_AGR_DT INTO WL_NF_DEV_DT.

            LOOP AT T_DOC_DEV INTO WL_DOC_DEV WHERE DOCDAT EQ WL_NF_DEV_DT-DOCDAT.

              V_NR_NF        = WL_DOC_DEV-NFENUM.
              V_SERIE_NF     = WL_DOC_DEV-SERIES.

              V_NR_NF        = |{ V_NR_NF    ALPHA = OUT  }|.
              V_SERIE_NF     = |{ V_SERIE_NF ALPHA = OUT  }|.

              CONCATENATE V_NR_NF '-' V_SERIE_NF  INTO V_NR_SERIE_NF.

              IF R_TEXTOS IS INITIAL.
                CONCATENATE 'Remessa ref. NF Form. Lote' V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ELSE.

                IF V_SEPARADOR_HIFEN EQ ABAP_TRUE.
                  V_SEPARADOR       = '-'.
                  V_SEPARADOR_HIFEN = ABAP_FALSE.
                ELSE.
                  V_SEPARADOR = ','.
                ENDIF.

                CONCATENATE R_TEXTOS V_SEPARADOR V_NR_SERIE_NF INTO R_TEXTOS SEPARATED BY SPACE.
              ENDIF.

            ENDLOOP.

            CONCATENATE WL_NF_DEV_DT-DOCDAT+6(2) '/' WL_NF_DEV_DT-DOCDAT+4(2) '/' WL_NF_DEV_DT-DOCDAT(4) INTO V_DT_NF.

            CONCATENATE R_TEXTOS 'de' V_DT_NF INTO R_TEXTOS SEPARATED BY SPACE.

            V_SEPARADOR_HIFEN = ABAP_TRUE.

          ENDLOOP.

          RETURN.

        ENDIF.


    ENDCASE.

  ENDMETHOD.


  METHOD get_unid_tribut_item_nf.
*<<<< ** INICIO   User Story 151256 - AOENNING >>>
    DATA: wa_mara     TYPE mara,
          wa_zmmt0107 TYPE zmmt0107,
          wa_vbrp     TYPE vbrp.

    CLEAR: e_msg_error, e_unid_trib, e_qtd_trib,  wa_mara, wa_zmmt0107, wa_vbrp.

    CHECK i_lin-matnr IS NOT INITIAL AND i_lin-refkey IS NOT INITIAL AND i_lin-refitm IS NOT INITIAL.

    "Verifica cadastro material mara.
    SELECT SINGLE * INTO wa_mara
    FROM mara
    WHERE matnr EQ i_lin-matnr.
    IF sy-subrc NE 0.
      e_msg_error = |Cadastro material { i_lin-matnr } não encontrado!|.
      EXIT.
    ENDIF.

    SELECT SINGLE * INTO wa_zmmt0107
    FROM zmmt0107
    WHERE matkl EQ wa_mara-matkl
    AND  direcao EQ 'S'
    AND  nbm EQ ''.
    IF sy-subrc EQ 0.

      CASE i_lin-reftyp.
        WHEN 'BI'.

          SELECT SINGLE * INTO wa_vbrp
            FROM vbrp
           WHERE vbeln EQ i_lin-refkey+0(10)
             AND posnr EQ i_lin-refitm.

          IF sy-subrc EQ 0.

            CHECK ( wa_zmmt0107-und_trib NE wa_vbrp-meins ).

            IF ( wa_zmmt0107-und_trib EQ wa_vbrp-gewei ).
              e_unid_trib = wa_vbrp-gewei.
              e_qtd_trib  = wa_vbrp-brgew.
            ELSE.
              e_msg_error = |Não foi possivel converter a quantidade da Unidade tributavél para o documento fiscal!|.
            ENDIF.

          ELSE.
            e_msg_error = |Não foi possivel converter a quantidade da Unidade tributavél para o documento fiscal!|.
          ENDIF.

        WHEN OTHERS.
          "e_msg_error = |Não foi possivel converter a quantidade da Unidade tributavél para o documento fiscal!|.
      ENDCASE.



    ENDIF.

*<<<< ** FIM   User Story 151256 - AOENNING >>>
  ENDMETHOD.


METHOD if_ex_cl_nfe_print~call_rsnast00.

*  break abap.

ENDMETHOD.


METHOD IF_EX_CL_NFE_PRINT~CHECK_SUBSEQUENT_DOCUMENTS.

*  "656 - Rejeicao: Consumo indevido pelo aplicativo da empresa.
*  IF IS_ACTIVE-CODE EQ '656'.
*    CH_SUBRC = 1.
*  ENDIF.

ENDMETHOD.


method IF_EX_CL_NFE_PRINT~DETERMINE_MATDOC_CANCEL_DATE.

endmethod.


method IF_EX_CL_NFE_PRINT~EXCLUDE_NFES_FROM_BATCH.

endmethod.


  method IF_EX_CL_NFE_PRINT~FILL_ADD_INFLIN.
  endmethod.


  method IF_EX_CL_NFE_PRINT~FILL_AUTXML.

    DATA: WL_AUTXML TYPE J_1BNFE_S_BADI_AUTXML,
          IT_0103   TYPE TABLE OF ZSDT0103,
          V_CENTRO  TYPE ZSDT0103-WERKS.

    DATA: LVA_LIFNR_SEL   TYPE LFA1-LIFNR,
          LVA_STCD1_DEST TYPE LFA1-STCD1,
          LVA_STCD2_DEST TYPE LFA1-STCD2.

    DATA: LIT_J_1BNFNAD TYPE TABLE OF J_1BNFNAD.

    CLEAR: IT_0103[], LIT_J_1BNFNAD[].

    CHECK IS_HEADER-BRANCH IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT    = IS_HEADER-BRANCH
      IMPORTING
        OUTPUT   = V_CENTRO.

    SELECT *
      FROM ZSDT0103 INTO TABLE IT_0103
     WHERE WERKS EQ V_CENTRO.

    LOOP AT IT_0103 INTO DATA(WL_0103).

      CLEAR: WL_AUTXML.

      IF STRLEN( WL_0103-STCD1 ) EQ 14.
        WL_AUTXML-CNPJ = WL_0103-STCD1.
      ELSE.
        WL_AUTXML-CPF  = WL_0103-STCD1.
      ENDIF.

      APPEND WL_AUTXML TO ET_AUTXML.

    ENDLOOP.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(LWA_DOC_SEL)
     WHERE DOCNUM EQ @IS_HEADER-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    CLEAR: LVA_STCD1_DEST , LVA_STCD2_DEST.

    CASE LWA_DOC_SEL-PARTYP.
      WHEN 'C'.
        SELECT SINGLE *
          FROM KNA1 INTO @DATA(LWA_KNA1_DEST)
         WHERE KUNNR EQ @LWA_DOC_SEL-PARID.

        CHECK SY-SUBRC EQ 0.

        IF LWA_KNA1_DEST-STCD1 IS NOT INITIAL.
          LVA_STCD1_DEST  = LWA_KNA1_DEST-STCD1.
        ELSEIF LWA_KNA1_DEST-STCD2 IS NOT INITIAL.
          LVA_STCD2_DEST  = LWA_KNA1_DEST-STCD2.
        ENDIF.

      WHEN 'V'.

        SELECT SINGLE *
          FROM LFA1 INTO @DATA(LWA_LFA1_DEST)
         WHERE LIFNR EQ @LWA_DOC_SEL-PARID.

        CHECK SY-SUBRC EQ 0.

        IF LWA_LFA1_DEST-STCD1 IS NOT INITIAL.
          LVA_STCD1_DEST  = LWA_LFA1_DEST-STCD1.
        ELSEIF LWA_LFA1_DEST-STCD2 IS NOT INITIAL.
          LVA_STCD2_DEST  = LWA_LFA1_DEST-STCD2.
        ENDIF.

      WHEN 'B'.
        LVA_LIFNR_SEL = LWA_DOC_SEL-PARID+4(4).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT         = LVA_LIFNR_SEL
          IMPORTING
            OUTPUT        = LVA_LIFNR_SEL.

        SELECT SINGLE *
          FROM LFA1 INTO LWA_LFA1_DEST
         WHERE LIFNR EQ LVA_LIFNR_SEL.

        CHECK SY-SUBRC EQ 0.

        IF LWA_LFA1_DEST-STCD1 IS NOT INITIAL.
          LVA_STCD1_DEST  = LWA_LFA1_DEST-STCD1.
        ELSEIF LWA_LFA1_DEST-STCD2 IS NOT INITIAL.
          LVA_STCD2_DEST  = LWA_LFA1_DEST-STCD2.
        ENDIF.

    ENDCASE.


    "INCLUSÃO PARCEIRO Z1 E LR
    SELECT * INTO TABLE LIT_J_1BNFNAD
      FROM J_1BNFNAD
     WHERE DOCNUM EQ IS_HEADER-DOCNUM
       AND PARVW  IN ( 'Z1', 'LR' ).

    LOOP AT LIT_J_1BNFNAD INTO DATA(LWA_J_1BNFNAD) WHERE PARID IS NOT INITIAL.
      CASE LWA_J_1BNFNAD-PARTYP.
        WHEN 'C'.

          SELECT SINGLE *
            FROM KNA1 INTO @DATA(LWA_KNA1_SEL)
           WHERE KUNNR EQ @LWA_J_1BNFNAD-PARID.

          IF ( SY-SUBRC EQ 0 ).
            IF LWA_KNA1_SEL-STCD1 IS NOT INITIAL.
              WL_AUTXML-CNPJ = LWA_KNA1_SEL-STCD1.
            ELSEIF LWA_KNA1_SEL-STCD2 IS NOT INITIAL.
              WL_AUTXML-CPF  = LWA_KNA1_SEL-STCD2.
            ENDIF.

            APPEND WL_AUTXML TO ET_AUTXML.
          ENDIF.

        WHEN 'V'.

          SELECT SINGLE *
            FROM LFA1 INTO @DATA(LWA_LFA1_SEL)
           WHERE LIFNR EQ @LWA_J_1BNFNAD-PARID.

          IF ( SY-SUBRC EQ 0 ).
            IF LWA_LFA1_SEL-STCD1 IS NOT INITIAL.
              WL_AUTXML-CNPJ = LWA_LFA1_SEL-STCD1.
            ELSEIF LWA_LFA1_SEL-STCD2 IS NOT INITIAL.
              WL_AUTXML-CPF = LWA_LFA1_SEL-STCD2.
            ENDIF.

            APPEND WL_AUTXML TO ET_AUTXML.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    IF LVA_STCD1_DEST IS NOT INITIAL.
      DELETE ET_AUTXML WHERE CNPJ EQ LVA_STCD1_DEST.
    ELSEIF LVA_STCD2_DEST IS NOT INITIAL.
      DELETE ET_AUTXML WHERE CPF EQ LVA_STCD2_DEST.
    ENDIF.

    SORT ET_AUTXML BY CNPJ CPF.
    DELETE ADJACENT DUPLICATES FROM ET_AUTXML COMPARING CNPJ CPF.

  endmethod.


  method IF_EX_CL_NFE_PRINT~FILL_CTE.
  endmethod.


  method IF_EX_CL_NFE_PRINT~FILL_CTE_200.
  endmethod.


  METHOD if_ex_cl_nfe_print~fill_cte_300.

    CONSTANTS: c_01(2)          TYPE c VALUE '01',
               c_02(2)          TYPE c VALUE '02',
               c_03(2)          TYPE c VALUE '03',
               c_04(2)          TYPE c VALUE '04',
               c_kg(2)          TYPE c VALUE 'KG',
               c_sac(4)         TYPE c VALUE 'SAC',
               c_to(2)          TYPE c VALUE 'TO',
               c_un(2)          TYPE c VALUE 'UN',
               c_lt(2)          TYPE c VALUE 'LT',
               c_peso_bruto(10) TYPE c VALUE 'PESO BRUTO',
               c_peso_liqui(12) TYPE c VALUE 'PESO LIQUIDO',
               c_caixa(5)       TYPE c VALUE 'CAIXA',
               c_litragem(8)    TYPE c VALUE 'LITRAGEM'.

    TYPES: y_j_j1bnfdoc TYPE j_1bnfdoc,
           y_j_1bnfnad  TYPE j_1bnfnad,
           y_j_1bnflin  TYPE j_1bnflin,
           y_j_1bnfstx  TYPE j_1bnfstx,
           y_j_1bnfftx  TYPE j_1bnfftx,
           y_j_1bnfref  TYPE j_1bnfref,

           BEGIN OF y_lin,
             docnum TYPE j_1bnflin-docnum,
           END OF y_lin.

    TYPES: BEGIN OF y_material,
             matnr TYPE j_1bnflin-matnr,
             menge TYPE j_1bnflin-menge,
           END OF y_material,

           BEGIN OF y_unidade,
             meins TYPE j_1bnflin-meins,
             menge TYPE j_1bnflin-menge,
           END OF y_unidade.

    DATA: ti_partner      TYPE TABLE OF y_j_1bnfnad,
          ti_item         TYPE TABLE OF y_j_1bnflin,
          ti_item_tax     TYPE TABLE OF y_j_1bnfstx,
          ti_header_msg   TYPE TABLE OF y_j_1bnfftx,
          ti_refer_msg    TYPE TABLE OF y_j_1bnfref,
          ti_header_p     TYPE TABLE OF y_j_j1bnfdoc,
          ti_partner_p    TYPE TABLE OF y_j_1bnfnad,
          ti_vbrp         TYPE TABLE OF vbrp,
          ti_vbak         TYPE TABLE OF vbak,
          ti_vttp         TYPE TABLE OF vttp,
          ti_item_p       TYPE TABLE OF y_j_1bnflin,
          ti_item_tax_p   TYPE TABLE OF y_j_1bnfstx,
          ti_header_msg_p TYPE TABLE OF y_j_1bnfftx,
          ti_refer_msg_p  TYPE TABLE OF y_j_1bnfref,
          wa_vttk         TYPE vttk,
          wa_header       TYPE j_1bnfdoc,
          wa_vbak         TYPE vbak,
          wa_vbrk         TYPE vbrk,
          wa_tdlnr        TYPE lfa1,
          wa_ttds         TYPE ttds,
          wa_lin          TYPE y_lin,
          ti_lin          TYPE TABLE OF y_lin,
          vl_tot_merc     TYPE j_1bnflin-netwr,
          wa_material     TYPE y_material,
          tl_material     TYPE TABLE OF y_material,
          wa_unidade      TYPE y_unidade,
          tl_unidade      TYPE TABLE OF y_unidade,
          wa_ct_infq      LIKE LINE OF ct_infq,
          p_identifica    TYPE zcte_identifica,
          it_notas_info   TYPE TABLE OF zcte_info_nota,
          it_cte_trans    TYPE TABLE OF zcte_trans,
          it_cte_moto     TYPE TABLE OF zcte_motorista,
          it_ciot         TYPE TABLE OF zcte_ciot,
          wl_zlest0061    TYPE zlest0061,
          wl_nfdoc_tmp    TYPE j_1bnfdoc,
          p_zlest0066     TYPE zlest0066,
          it_vfkp         TYPE TABLE OF vfkp,
          it_konv         TYPE TABLE OF konv,
          it_vttp         TYPE TABLE OF vttp,
          it_vbfa         TYPE TABLE OF vbfa,
          it_vbkd         TYPE TABLE OF vbkd,
          it_obs_geral    TYPE TABLE OF zcte_obs_gerais,
          it_zlest0067    TYPE TABLE OF zlest0067.

    DATA: vl_cte_complemento TYPE c,
          vl_cte_ref         TYPE j_1bnfdoc-docref,
          tx_obs_geral       TYPE string,
          wa_obs_geral       TYPE zcte_obs_gerais,
          vlr_dolar          TYPE c LENGTH 20,
          vlr_taxa           TYPE c LENGTH 20,
          v_qtdrat           TYPE p DECIMALS 2,
          wl_dados_compl     TYPE j_1bcte_int_compl.

    DATA: wl_infunitransp    TYPE j_1bcte_int_infunidtransp.

    DATA: v_id_infunidtransp TYPE j_1bcte_int_infunidtransp-id,
          v_id_text          TYPE j_1bcte_int_text-id,
          v_id_text_seq_no   TYPE j_1bcte_int_text-seq_no.


    SELECT SINGLE *
      FROM zcte_identifica
     WHERE docnum = @is_nfdoc-docnum
      INTO @DATA(wa_zcte_identifica).

    IF wa_zcte_identifica-tpserv = '1'.
      SELECT SINGLE *
        FROM zcte_doc_ant
       WHERE docnum = @is_nfdoc-docnum
        INTO @DATA(wa_zcte_doc_ant).

      IF sy-subrc IS INITIAL.

        APPEND INITIAL LINE TO  ct_iddocant ASSIGNING FIELD-SYMBOL(<fs_ct_iddocant>).
        <fs_ct_iddocant>-id              = '1'.
        <fs_ct_iddocant>-seq_no          = '1'.
        <fs_ct_iddocant>-iddocantele_ref = '1'.

        APPEND INITIAL LINE TO ct_iddocantele ASSIGNING FIELD-SYMBOL(<fs_ct_iddocantele>).
        <fs_ct_iddocantele>-id     = '1'.
        <fs_ct_iddocantele>-seq_no = '1'.
        <fs_ct_iddocantele>-chave = wa_zcte_doc_ant-c57_chave_acesso.

        APPEND INITIAL LINE TO ct_emidocant ASSIGNING FIELD-SYMBOL(<fs_ct_emidocant>).
        <fs_ct_emidocant>-id = '1'.
        <fs_ct_emidocant>-iddocant_ref = '1'.
        <fs_ct_emidocant>-cnpj  = wa_zcte_doc_ant-emit_ant_cnpj.
        <fs_ct_emidocant>-ie    = wa_zcte_doc_ant-emit_ant_ie.
        <fs_ct_emidocant>-uf    = wa_zcte_doc_ant-emit_ant_uf.
        <fs_ct_emidocant>-xnome = wa_zcte_doc_ant-emit_ant_nome.
      ENDIF.
    ENDIF.

    CLEAR: vl_cte_complemento, vl_cte_ref.
    SELECT SINGLE *
      FROM j_1bnfdoc INTO @wl_nfdoc_tmp
     WHERE docnum EQ @is_nfdoc-docnum
       AND doctyp EQ '2'
       AND model  EQ @zif_doc_eletronico=>at_st_model_cte.

    IF ( sy-subrc = 0 ) AND ( wl_nfdoc_tmp-docref IS NOT INITIAL ). "DOCUMENTO É TIPO COMPLEMENTAR
      vl_cte_complemento = 'X'.
      vl_cte_ref         = wl_nfdoc_tmp-docref.
    ENDIF.

    "VERIFICAR SE O DOCNUM ESTA GRAVADO NA TABELA DO AQUAVIÁRIO.
    SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE docnum EQ is_nfdoc-docnum.

    IF ( sy-subrc NE 0 ) AND ( vl_cte_ref IS NOT INITIAL ).
      CLEAR: wl_zlest0061.
      SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE docnum EQ vl_cte_ref.
    ENDIF.

    IF wl_zlest0061 IS INITIAL."( SY-SUBRC NE 0 ).

      CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
        EXPORTING
          p_cte_avulso       = is_nfdoc-docnum
          p_chamar_tela      = space
          p_gravar_dados     = abap_true
        IMPORTING
*         P_J_1BNFDOC        =
          p_identifica       = p_identifica
*         P_PARCEIROS        =
        TABLES
          it_notas_info      = it_notas_info
          it_trans           = it_cte_trans
          it_ciot            = it_ciot
          it_motorista       = it_cte_moto
          it_obs_geral       = it_obs_geral
*         IT_SEGURO          =
        EXCEPTIONS
          n_info_cte         = 1
          n_loc_cte          = 2
          n_eletronico       = 3
          n_modelo_57        = 4
          n_status           = 5
          inf_docnum         = 6
          inf_propveiculo    = 7
          nao_docnum         = 8
          nao_rtrc           = 9
          nao_conta_corrente = 10
          moto_nao_pf        = 11
          n_placa_cad        = 12
          sem_notas          = 13
          OTHERS             = 14.


      CALL FUNCTION 'Z_SD_NFES_DA_CTE'
        EXPORTING
          mp_docnum          = is_nfdoc-docnum
        TABLES
          mt_partner         = ti_partner
          mt_item            = ti_item
          mt_item_tax        = ti_item_tax
          mt_header_msg      = ti_header_msg
          mt_refer_msg       = ti_refer_msg
          mt_vbrp            = ti_vbrp
          mt_vbak            = ti_vbak
          mt_vttp            = ti_vttp
          mt_headre_nota_nfe = ti_header_p
          mt_item_nota       = ti_item_p
          mt_partner_p       = ti_partner_p
          mt_item_tax_p      = ti_item_tax_p
          mt_header_msg_p    = ti_header_msg_p
          mt_refer_msg_p     = ti_refer_msg_p
        CHANGING
          mr_vttk            = wa_vttk
          mr_header_nota_cte = wa_header
          mr_vbak            = wa_vbak
          mr_vbrk            = wa_vbrk
          mr_lfa1            = wa_tdlnr
          mr_ttds            = wa_ttds.

      IF wa_zcte_identifica-tpserv NE '1'.
        CHECK ti_vbrp  IS NOT INITIAL.
        CHECK wa_vbrk  IS NOT INITIAL.
        CHECK ti_vbak  IS NOT INITIAL.
        CHECK wa_vttk  IS NOT INITIAL.
        CHECK wa_tdlnr IS NOT INITIAL.
        CHECK wa_ttds  IS NOT INITIAL.
        CHECK ti_vttp  IS NOT INITIAL.
        CHECK wa_vbak  IS NOT INITIAL.
      ENDIF.

      LOOP AT ti_item_p INTO DATA(wa_item_p).
        wa_lin-docnum = wa_item_p-docnum.
        APPEND wa_lin TO ti_lin.
      ENDLOOP.

      SELECT SINGLE * FROM zlest0061 INTO @DATA(wa_zlest0061) WHERE docnum EQ @is_nfdoc-docnum.

      READ TABLE ti_vbrp INTO DATA(wa_vbrp) INDEX 1.

      DATA vl_tabix TYPE sy-tabix.

      LOOP AT ti_partner ASSIGNING FIELD-SYMBOL(<fs_partiner>).

        CHECK: ( <fs_partiner>-cgc IS INITIAL   AND
                 <fs_partiner>-cpf IS INITIAL ) OR
                 <fs_partiner>-name1 IS INITIAL OR
                 <fs_partiner>-stains IS INITIAL.
        vl_tabix = sy-tabix.

        SELECT SINGLE *
          INTO @DATA(wa_kna1)
          FROM kna1
         WHERE kunnr EQ @<fs_partiner>-parid.

        IF <fs_partiner>-name1 IS INITIAL.
          <fs_partiner>-name1 = wa_kna1-name1.
        ENDIF.
        IF <fs_partiner>-cgc IS INITIAL.
          <fs_partiner>-cgc = wa_kna1-stcd1.
        ENDIF.
        IF <fs_partiner>-cpf IS INITIAL.
          <fs_partiner>-cpf = wa_kna1-stcd2.
        ENDIF.
        IF <fs_partiner>-stains IS INITIAL.
          <fs_partiner>-stains = wa_kna1-stcd3.
        ENDIF.

      ENDLOOP.

      CLEAR vl_tot_merc.
      IF wa_header-ntgew IS NOT INITIAL AND wa_header-brgew IS NOT INITIAL AND wa_header-gewei EQ 'KG'.
        CLEAR: wa_ct_infq.
        wa_ct_infq-id     = 1.
        wa_ct_infq-cunid  = c_01.
        wa_ct_infq-tpmed  = c_peso_bruto.
        wa_ct_infq-qcarga = wa_header-brgew.
        APPEND wa_ct_infq TO ct_infq.

        CLEAR: wa_ct_infq.
        wa_ct_infq-id     = 2.
        wa_ct_infq-cunid  = c_01.
        wa_ct_infq-tpmed  = c_peso_liqui.
        wa_ct_infq-qcarga = wa_header-ntgew.
        APPEND wa_ct_infq TO ct_infq.
      ENDIF.

      IF ct_infq IS NOT INITIAL.
        DATA(lc_nao_incluir_unidade) = abap_true.
      ELSE.
        lc_nao_incluir_unidade = abap_false.
      ENDIF.

      IF ti_item_p[] IS NOT INITIAL.
        LOOP AT ti_item_p INTO wa_item_p.
          ADD wa_item_p-netwr TO vl_tot_merc.

          wa_material-matnr = wa_item_p-matnr.
          wa_material-menge = wa_item_p-menge.
          COLLECT wa_material INTO tl_material.

          IF lc_nao_incluir_unidade EQ abap_false.
            wa_unidade-meins = wa_item_p-meins.
            wa_unidade-menge = wa_item_p-menge.
            COLLECT wa_unidade INTO tl_unidade.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT it_notas_info INTO DATA(wa_notas_info).
          ADD wa_notas_info-vl_nota_fiscal TO vl_tot_merc.

          wa_material-matnr = wa_notas_info-material.
          wa_material-menge = wa_notas_info-quantidade.
          COLLECT wa_material INTO tl_material.

          IF lc_nao_incluir_unidade EQ abap_false.
            wa_unidade-meins = wa_notas_info-unidade.
            wa_unidade-menge = wa_notas_info-quantidade.
            COLLECT wa_unidade INTO tl_unidade.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SORT tl_material BY menge DESCENDING.
      READ TABLE tl_material INTO wa_material INDEX 1.

      SELECT SINGLE maktx INTO @DATA(nm_pro_pred)
        FROM makt
       WHERE matnr EQ @wa_material-matnr
         AND spras EQ @sy-langu.

      IF sy-subrc NE 0.
        IF wa_zcte_identifica-tpserv = '1'.

          SELECT SINGLE *
            FROM zlest0194
           WHERE docnum_sub = @is_nfdoc-docnum
            INTO @DATA(wa_zlest0194).

          IF sy-subrc IS INITIAL.
            nm_pro_pred = wa_zlest0194-ds_prod_pred.
          ENDIF.
        ENDIF.
      ENDIF.

*      ST_VTTK-VSART
*      01	RODOVIARIO
*      02	FERROVIÁRIO
*      03	NAVEGAÇÃO FLUVIAL
*      04	MARÍTIMO
*      05	AÉREO
*      06	CORREIO, SERV.POSTAL
*      07	MULTIMODAL

*      PREENCHER COM:
*      01-RODOVIÁRIO;
*      02-AÉREO;
*      03-AQUAVIÁRIO;
*      04-FERROVIÁRIO;
*      05-DUTOVIÁRIO;
*      06-MULTIMODAL

*---------------------------------------------------------------------------------------------*
*   Indentificação
*---------------------------------------------------------------------------------------------*

      IF p_identifica-modal IS NOT INITIAL.
        cs_ide-modal = p_identifica-modal.
      ELSE.
        CASE wa_vttk-vsart.
          WHEN '01'.
            cs_ide-modal = '01'.
          WHEN '02'.
            cs_ide-modal = '04'.
          WHEN '03'.
            cs_ide-modal = '03'.
          WHEN '04'.
            cs_ide-modal = '03'.
          WHEN '05'.
            cs_ide-modal = '02'.
          WHEN '07'.
            cs_ide-modal = '06'.
        ENDCASE.
      ENDIF.

      CASE cs_ide-modal.
        WHEN '01'.
          cs_rodo-rntrc = p_identifica-rodo_rntrc.

          IF p_identifica-rodo_frete_lota EQ '1'.
            "PEDÁGIO
            IF p_identifica-vlr_pedagio GE 0.
            ENDIF.
          ENDIF.
      ENDCASE.

      cs_ide-tp_serv   = zcl_string=>tira_acentos( CONV #( p_identifica-tpserv ) ).  "*-#127337-21.11.2023-JT
      cs_ide-c_mun_ini = zcl_string=>tira_acentos( CONV #( p_identifica-cmunini ) ). "*-#127337-21.11.2023-JT
      cs_ide-ufini     = p_identifica-ufini.
      cs_ide-x_mun_ini = zcl_string=>upper( i_str = CONV #( p_identifica-nmunini ) ).
      cs_ide-x_mun_ini = zcl_string=>tira_acentos( CONV #( cs_ide-x_mun_ini ) ).     "*-#127337-21.11.2023-JT

      cs_ide-uffim     = p_identifica-uffim.
      cs_ide-c_mun_fim = zcl_string=>tira_acentos( CONV #( p_identifica-cmunfim ) ). "*-#127337-21.11.2023-JT
      cs_ide-x_mun_fim = zcl_string=>upper( i_str = CONV #( p_identifica-nmunfim ) )."*-#127337-21.11.2023-JT
      cs_ide-x_mun_fim = zcl_string=>tira_acentos( CONV #( cs_ide-x_mun_fim ) ).     "*-#127337-21.11.2023-JT

*---------------------------------------------------------------------------------------------*
*   Inf. Carga
*---------------------------------------------------------------------------------------------*

      cs_infcarga-v_carga       = vl_tot_merc.
      cs_infcarga-pro_pred      = nm_pro_pred.
      cs_infcarga-v_carga_averb = vl_tot_merc.

*---------------------------------------------------------------------------------------------*
*   Inf. Quantidade
*---------------------------------------------------------------------------------------------*
      IF lc_nao_incluir_unidade EQ abap_false.
        LOOP AT tl_unidade INTO wa_unidade.
          CLEAR: wa_ct_infq.
          wa_ct_infq-id = sy-tabix.

          CASE wa_unidade-meins.
            WHEN c_kg.
              wa_ct_infq-cunid = c_01.
              wa_ct_infq-tpmed = c_peso_bruto.
            WHEN c_to.
              wa_ct_infq-cunid = c_02.
              wa_ct_infq-tpmed = c_peso_bruto.
            WHEN c_un.
              wa_ct_infq-cunid = c_03.
              wa_ct_infq-tpmed = c_caixa.
            WHEN c_lt.
              wa_ct_infq-cunid = c_04.
              wa_ct_infq-tpmed = c_litragem.
            WHEN OTHERS.
              wa_ct_infq-cunid = c_01.
              wa_ct_infq-tpmed = c_peso_bruto.
          ENDCASE.

          wa_ct_infq-qcarga = wa_unidade-menge.
          APPEND wa_ct_infq TO ct_infq.
        ENDLOOP.
      ENDIF.

*---------------------------------------------------------------------------------------------*
*   Notas Fiscais
*---------------------------------------------------------------------------------------------*

      DATA: wa_infdoc_infnfe TYPE j_1bcte_int_infnfe_200.
      DATA: wa_infdoc_infnf  TYPE j_1bcte_int_infnf_200.

      LOOP AT it_notas_info INTO wa_notas_info.
        IF wa_notas_info-modelo EQ '55'.
          CLEAR: wa_infdoc_infnfe.
          wa_infdoc_infnfe-chave = wa_notas_info-chave.
          wa_infdoc_infnfe-pin   = wa_notas_info-pin_suframa.
          APPEND wa_infdoc_infnfe TO ct_infdoc_infnfe.
        ELSE.
          CLEAR: wa_infdoc_infnf.
          wa_infdoc_infnf-demi  = wa_notas_info-dtemissao.
          wa_infdoc_infnf-mod   = wa_notas_info-modelo.
          wa_infdoc_infnf-serie = wa_notas_info-serie.
          wa_infdoc_infnf-ndoc  = wa_notas_info-numero.
          wa_infdoc_infnf-vbc   = wa_notas_info-vl_bc.
          wa_infdoc_infnf-vicms = wa_notas_info-vl_icms.
          wa_infdoc_infnf-vbcst = wa_notas_info-vl_bc_st.
          wa_infdoc_infnf-vst   = wa_notas_info-vl_st.
          wa_infdoc_infnf-vprod = wa_notas_info-vl_produtos.
          wa_infdoc_infnf-vnf   = wa_notas_info-vl_nota_fiscal.
          wa_infdoc_infnf-ncfop = wa_notas_info-cfop(4).
          wa_infdoc_infnf-npeso = wa_notas_info-quantidade.
          wa_infdoc_infnf-pin   = wa_notas_info-pin_suframa.

          IF wa_notas_info-unidade NE 'KG'.
            CALL FUNCTION 'ME_CONVERSION_MEINS'
              EXPORTING
                i_matnr             = wa_notas_info-material
                i_mein1             = wa_notas_info-unidade
                i_meins             = 'KG'
                i_menge             = wa_notas_info-quantidade
              IMPORTING
                menge               = wa_notas_info-quantidade
              EXCEPTIONS
                error_in_conversion = 1
                no_success          = 2
                OTHERS              = 3.
          ENDIF.

          IF sy-subrc IS INITIAL.
            wa_infdoc_infnf-npeso = wa_notas_info-quantidade.
          ENDIF.

          IF ( wa_notas_info-unidade NE 'KG' ) AND ( wa_notas_info-peso_fiscal > 0 ).
            CALL FUNCTION 'ME_CONVERSION_MEINS'
              EXPORTING
                i_matnr             = wa_notas_info-material
                i_mein1             = wa_notas_info-unidade
                i_meins             = 'KG'
                i_menge             = wa_notas_info-peso_fiscal
              IMPORTING
                menge               = wa_notas_info-peso_fiscal
              EXCEPTIONS
                error_in_conversion = 1
                no_success          = 2
                OTHERS              = 3.

            IF sy-subrc IS INITIAL.
              wa_infdoc_infnf-npeso = wa_notas_info-quantidade.
            ENDIF.
          ENDIF.
          APPEND wa_infdoc_infnf TO ct_infdoc_infnf.

        ENDIF.
      ENDLOOP.

*---------------------------------------------------------------------------------------------*
*   Obs. Complementares
*---------------------------------------------------------------------------------------------*
      DATA: vg_kinak           TYPE kinak,
            vg_kwert           TYPE kwert,
            valor_pedagio      TYPE c LENGTH 20,
            vg_contrato_viagem TYPE znucontrato,
            texto_pedagio      TYPE string,
            tx_ciot            TYPE string,
            vl_xtexto_contrib  TYPE string,
            vl_xtexto          TYPE string.

      CLEAR: tx_obs_geral, it_vfkp[], texto_pedagio, vg_kwert, vg_contrato_viagem, it_konv[], vg_kwert.

      LOOP AT it_obs_geral INTO wa_obs_geral.
        CONCATENATE tx_obs_geral wa_obs_geral-texto INTO tx_obs_geral SEPARATED BY space.
      ENDLOOP.

      SELECT *
        FROM vfkp
        INTO CORRESPONDING FIELDS OF TABLE it_vfkp
       WHERE refty EQ '8'
         AND rebel EQ p_identifica-tknum.

      IF it_vfkp[] IS NOT INITIAL.
*---> S4 MIGRATION 10/07/2023 - MA
*        SELECT *
*          FROM KONV
*          INTO CORRESPONDING FIELDS OF TABLE IT_KONV
*          FOR ALL ENTRIES IN IT_VFKP
*         WHERE KNUMV EQ IT_VFKP-KNUMV
*           AND KSCHL EQ 'ZPED'
*           AND KINAK EQ VG_KINAK
*           AND KWERT GT 0.
        DATA: lc_kschl TYPE kschl VALUE 'ZPED',
              lc_kwert TYPE kwert VALUE 0.

        SELECT *
          FROM v_konv
          INTO TABLE @DATA(lt_konv)
          FOR ALL ENTRIES IN @it_vfkp
         WHERE knumv EQ @it_vfkp-knumv
           AND kschl EQ @lc_kschl
           AND kinak EQ @vg_kinak
           AND kwert GT @lc_kwert.

        MOVE-CORRESPONDING lt_konv[] TO it_konv[].
*---> S4 MIGRATION 10/07/2023 - MA


        IF sy-subrc EQ 0.
          LOOP AT it_konv INTO DATA(wl_konv).
            IF wl_konv-kwert GT 0.
              vg_kwert = vg_kwert + wl_konv-kwert.
            ENDIF.
          ENDLOOP.
          IF vg_kwert GT 0.
            WRITE vg_kwert TO valor_pedagio.
            SHIFT valor_pedagio LEFT DELETING LEADING space.
            CONCATENATE 'Valor do pedágio:' valor_pedagio INTO texto_pedagio SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDIF.

      IF texto_pedagio IS NOT INITIAL.
        IF tx_obs_geral IS INITIAL.
          tx_obs_geral = texto_pedagio.
        ELSE.
          CONCATENATE tx_obs_geral texto_pedagio INTO tx_obs_geral SEPARATED BY space.
        ENDIF.
      ENDIF.

      LOOP AT it_ciot INTO DATA(wa_it_ciot).
        vg_contrato_viagem = wa_it_ciot-nucontrato.
        IF NOT wa_it_ciot-nr_ciot IS INITIAL.
          CONCATENATE 'Nr. CIOT:' wa_it_ciot-nr_ciot INTO tx_ciot SEPARATED BY space.

          IF tx_obs_geral IS INITIAL.
            tx_obs_geral = tx_ciot.
          ELSE.
            CONCATENATE tx_obs_geral tx_ciot INTO tx_obs_geral SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF NOT vg_contrato_viagem IS INITIAL.
        CONCATENATE 'Nr. Contrato de Viagem Administradora:' vg_contrato_viagem INTO tx_ciot SEPARATED BY space.

        IF tx_obs_geral IS INITIAL.
          tx_obs_geral = tx_ciot.
        ELSE.
          CONCATENATE tx_obs_geral tx_ciot INTO tx_obs_geral SEPARATED BY space.
        ENDIF.
      ENDIF.

      "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - inicio
      SELECT * FROM vttp
        INTO CORRESPONDING FIELDS OF TABLE it_vttp
       WHERE tknum EQ p_identifica-tknum.

      IF it_vttp[] IS NOT INITIAL.
        SELECT * FROM vbfa
          INTO CORRESPONDING FIELDS OF TABLE it_vbfa
          FOR ALL ENTRIES IN it_vttp
        WHERE vbeln EQ it_vttp-vbeln.

        IF it_vbfa[] IS NOT INITIAL.
          SELECT * FROM vbkd
           INTO CORRESPONDING FIELDS OF TABLE it_vbkd
           FOR ALL ENTRIES IN it_vbfa
          WHERE vbeln EQ it_vbfa-vbelv
            AND posnr EQ '000010'.
        ENDIF.

        IF NOT it_vbkd[] IS INITIAL.
          LOOP AT it_vbkd INTO DATA(wl_vbkd).
            CONCATENATE tx_obs_geral wl_vbkd-bstkd_e INTO tx_obs_geral SEPARATED BY space.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - fim

      cs_compl-xobs = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( tx_obs_geral ) ) ).

*-#127337-21.11.2023-JT-inicio
      CONDENSE cs_compl-xobs.
*-#127337-21.11.2023-JT-fim

      "Observações Contribuinte.
      SELECT SINGLE *
        FROM vttk INTO @DATA(wl_doc_transp)
       WHERE tknum EQ @p_identifica-tknum.

      IF ( sy-subrc EQ 0 ) AND ( it_cte_trans[] IS NOT INITIAL ) AND ( p_identifica-docnum IS NOT INITIAL ).

        CLEAR: v_id_text_seq_no.
        ADD 1 TO v_id_text.

        cs_compl-text_id_obscont = v_id_text.

        "Placa do cavalo
        IF wl_doc_transp-text1 IS NOT INITIAL.
          READ TABLE it_cte_trans INTO DATA(_wl_cte_trans) WITH KEY pc_veiculo = wl_doc_transp-text1.
          IF sy-subrc = 0.
            vl_xtexto = me->formata_texto_veiculo( i_cte_trans = _wl_cte_trans ).

            ADD 1 TO v_id_text_seq_no.

            APPEND VALUE #( id = v_id_text seq_no = v_id_text_seq_no typ = 'Placa Cavalo' text = vl_xtexto  ) TO ct_text.
          ENDIF.
        ENDIF.

        "Placa Carreta 1
        IF wl_doc_transp-text2 IS NOT INITIAL.
          READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text2.
          IF sy-subrc = 0.
            vl_xtexto = me->formata_texto_veiculo( i_cte_trans = _wl_cte_trans ).

            ADD 1 TO v_id_text_seq_no.

            APPEND VALUE #( id = v_id_text seq_no = v_id_text_seq_no typ = 'Placa Carreta 1' text = vl_xtexto ) TO ct_text.
          ENDIF.
        ENDIF.

        "Placa Carreta 2
        IF wl_doc_transp-text3 IS NOT INITIAL.
          READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text3.
          IF sy-subrc = 0.
            vl_xtexto = me->formata_texto_veiculo( i_cte_trans = _wl_cte_trans ).

            ADD 1 TO v_id_text_seq_no.

            APPEND VALUE #( id = v_id_text  seq_no = v_id_text_seq_no  typ = 'Placa Carreta 2' text = vl_xtexto ) TO ct_text.
          ENDIF.
        ENDIF.

        "Placa Carreta 3
        IF wl_doc_transp-text4 IS NOT INITIAL.
          READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text4.
          IF sy-subrc = 0.
            vl_xtexto = me->formata_texto_veiculo( i_cte_trans = _wl_cte_trans ).

            ADD 1 TO v_id_text_seq_no.

            APPEND VALUE #( id = v_id_text  seq_no = v_id_text_seq_no typ = 'Placa Carreta 3' text = vl_xtexto ) TO ct_text.
          ENDIF.
        ENDIF.

        LOOP AT it_cte_moto INTO DATA(_wl_cte_moto).
          CONCATENATE _wl_cte_moto-xnome '- CPF:' _wl_cte_moto-cpf INTO vl_xtexto.

          ADD 1 TO v_id_text_seq_no.

          APPEND VALUE #( id = v_id_text  seq_no = v_id_text_seq_no  typ = 'Motorista' text = vl_xtexto ) TO ct_text.
        ENDLOOP.
      ENDIF.

    ELSE.

      CLEAR: wa_ct_infq.

      CALL FUNCTION 'Z_LES_INFO_AQUAV'
        EXPORTING
          p_cte_avulso  = is_nfdoc-docnum
          p_complemento = vl_cte_complemento
        IMPORTING
          p_zlest0066   = p_zlest0066
        TABLES
          it_zlest0067  = it_zlest0067
          it_cte_obsg   = it_obs_geral.

*---------------------------------------------------------------------------------------------*
*   Identificação
*---------------------------------------------------------------------------------------------*
      cs_ide-modal              = p_zlest0066-modal.
      cs_ide-tp_serv            = p_zlest0066-tpserv.

      cs_ide-c_mun_ini          = p_zlest0066-cmunini.
      cs_ide-ufini              = p_zlest0066-ufini.
      cs_ide-x_mun_ini          = p_zlest0066-xmunini.

      cs_ide-c_mun_fim          = p_zlest0066-cmunfim.
      cs_ide-uffim              = p_zlest0066-uffim.
      cs_ide-x_mun_fim          = p_zlest0066-xmunfim.

*---------------------------------------------------------------------------------------------*
*   Obs. Complementares
*---------------------------------------------------------------------------------------------*
      LOOP AT it_obs_geral INTO wa_obs_geral.
        CONCATENATE cs_compl-xobs wa_obs_geral-texto INTO cs_compl-xobs SEPARATED BY space.
      ENDLOOP.

      cs_compl-xobs = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( cs_compl-xobs ) ) ).

*-#127337-21.11.2023-JT-inicio
      CONDENSE cs_compl-xobs.
*-#127337-21.11.2023-JT-fim

*---------------------------------------------------------------------------------------------*
*   Valor Prestação
*---------------------------------------------------------------------------------------------*


*---------------------------------------------------------------------------------------------*
*   Inf. Carga
*---------------------------------------------------------------------------------------------*
      cs_infcarga-v_carga   = p_zlest0066-vcarga.
      cs_infcarga-pro_pred  = p_zlest0066-propred.


*---------------------------------------------------------------------------------------------*
*   Inf. Quantidade
*---------------------------------------------------------------------------------------------*
      CLEAR: wa_ct_infq.
      wa_ct_infq-id = 1.
      wa_ct_infq-cunid  = p_zlest0066-cunid.
      wa_ct_infq-tpmed  = p_zlest0066-tpmed.
      wa_ct_infq-qcarga = p_zlest0066-qcarga.
      APPEND wa_ct_infq TO ct_infq.

*---------------------------------------------------------------------------------------------*
*   Notas Fiscais
*---------------------------------------------------------------------------------------------*
      DATA(_rateio_nf) = 'X'.
      LOOP AT it_zlest0067 INTO DATA(wl_zlest0067) WHERE chave IS NOT INITIAL.

        SELECT SINGLE *
          FROM zlest0060 INTO @DATA(_wl_0060)
         WHERE chave_nfe = @wl_zlest0067-chave.

        CHECK sy-subrc = 0.

        SELECT SINGLE *
          FROM zlest0063 INTO @DATA(_wl_0063)
         WHERE bukrs       =  @_wl_0060-bukrs
           AND werks       =  @_wl_0060-werks
           AND nr_viagem   =  @_wl_0060-nr_viagem
           AND ano_viagem  =  @_wl_0060-ano_viagem
           AND embarcacao  =  @_wl_0060-embarcacao
           AND nome_emb    =  @_wl_0060-nome_emb.

        CHECK sy-subrc = 0.

        "INSUMOS
        SELECT SINGLE *
          FROM setleaf INTO @DATA(_wl_setleaf)
         WHERE setname EQ 'MAGGI_ZLES0077_GR_MAT'
           AND valfrom EQ @_wl_0063-gr_material.

        CHECK sy-subrc = 0.

        _rateio_nf = ''.

        EXIT.
      ENDLOOP.

      CLEAR: v_id_infunidtransp.
      LOOP AT it_zlest0067 INTO wl_zlest0067.

        CLEAR: wa_infdoc_infnfe, wa_infdoc_infnf.

        IF _rateio_nf IS NOT INITIAL.
          CLEAR: wl_infunitransp.
          ADD 1 TO v_id_infunidtransp.
          REPLACE ALL OCCURRENCES OF  '-' IN wl_zlest0067-nome_emb WITH ''.
          CONDENSE wl_zlest0067-nome_emb NO-GAPS.
          v_qtdrat = wl_zlest0067-qcarga / 1000.
          wa_infdoc_infnfe-infunidtransp_ref = v_id_infunidtransp.
          wl_infunitransp-id                 = v_id_infunidtransp.
          wl_infunitransp-idunidtransp       = wl_zlest0067-nome_emb.
          wl_infunitransp-tpunidtransp       = '4'."4 = BALSA
          wl_infunitransp-qtdrat             = v_qtdrat.
          APPEND wl_infunitransp TO ct_infunidtransp.
          wa_infdoc_infnf-infunidtransp_ref = v_id_infunidtransp.
          wa_infdoc_infnfe-infunidtransp_ref = v_id_infunidtransp.
        ENDIF.

        IF wl_zlest0067-chave(1) = 'F'.

          SELECT SINGLE *
            FROM zlest0060 INTO @DATA(_wa_0060)
           WHERE chave_nfe = @wl_zlest0067-chave.

          SELECT SINGLE cfop
            INTO @DATA(_cfop)
            FROM zsdt0001
           WHERE ch_referencia = @_wa_0060-ch_referencia.

          IF sy-subrc NE 0.
            SELECT SINGLE *
              INTO @DATA(lwa_zlest0205)
              FROM zlest0205
              WHERE chave_nfe = @_wa_0060-chave_nfe.

            IF sy-subrc EQ 0.
              _cfop = lwa_zlest0205-cfop.
            ENDIF.
          ENDIF.

          REPLACE '/' WITH space INTO _cfop.
          CONDENSE _cfop NO-GAPS.


          wa_infdoc_infnf-demi  = _wa_0060-docdat.
          wa_infdoc_infnf-mod   = '04'.
          wa_infdoc_infnf-serie = _wa_0060-series.
          wa_infdoc_infnf-ndoc  = _wa_0060-nfnum.
          wa_infdoc_infnf-vbc   = _wa_0060-netwr.
          wa_infdoc_infnf-vicms = 0.
          wa_infdoc_infnf-vbcst = 0.
          wa_infdoc_infnf-vst   = 0.
          wa_infdoc_infnf-vprod = _wa_0060-netwr.
          wa_infdoc_infnf-vnf   = _wa_0060-netwr.
          wa_infdoc_infnf-ncfop = _cfop(4).
          APPEND wa_infdoc_infnf TO ct_infdoc_infnf.
        ELSE.
          wa_infdoc_infnfe-chave = wl_zlest0067-chave.
          APPEND wa_infdoc_infnfe TO ct_infdoc_infnfe.
        ENDIF.

      ENDLOOP.

*---------------------------------------------------------------------------------------------*
*   Informações Modal
*---------------------------------------------------------------------------------------------*
      cs_aquav-v_prest = p_zlest0066-vprest.
      cs_aquav-v_afrmm = p_zlest0066-vafrmm.
      cs_aquav-x_navio = p_zlest0066-xnavio.

      ADD 1 TO v_id_text.
      cs_aquav-text_id_balsa = v_id_text.
      APPEND VALUE #( id = v_id_text text = p_zlest0066-xbalsa ) TO ct_text.

      cs_aquav-n_viag = |{ p_zlest0066-nviag ALPHA = OUT }| ..
      cs_aquav-direc  = p_zlest0066-direc.
      cs_aquav-irin   = p_zlest0066-irin.

    ENDIF.


*---------------------------------------------------------------------------------------------*
*   Outras Tratativas
*---------------------------------------------------------------------------------------------*

    IF sy-sysid NE 'PRD'.
      LOOP AT ct_partner ASSIGNING FIELD-SYMBOL(<fs_partenr>).
        <fs_partenr>-x_nome = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
*       <fs_partenr>-x_fant = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
*       <fs_partenr>-x_lgr  = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD IF_EX_CL_NFE_PRINT~FILL_CTE_400.

    CONSTANTS: C_01(2)          TYPE C VALUE '01',
               C_02(2)          TYPE C VALUE '02',
               C_03(2)          TYPE C VALUE '03',
               C_04(2)          TYPE C VALUE '04',
               C_KG(2)          TYPE C VALUE 'KG',
               C_SAC(4)         TYPE C VALUE 'SAC',
               C_TO(2)          TYPE C VALUE 'TO',
               C_UN(2)          TYPE C VALUE 'UN',
               C_LT(2)          TYPE C VALUE 'LT',
               C_PESO_BRUTO(10) TYPE C VALUE 'PESO BRUTO',
               C_PESO_LIQUI(12) TYPE C VALUE 'PESO LIQUIDO',
               C_CAIXA(5)       TYPE C VALUE 'CAIXA',
               C_LITRAGEM(8)    TYPE C VALUE 'LITRAGEM'.

    TYPES: Y_J_J1BNFDOC TYPE J_1BNFDOC,
           Y_J_1BNFNAD  TYPE J_1BNFNAD,
           Y_J_1BNFLIN  TYPE J_1BNFLIN,
           Y_J_1BNFSTX  TYPE J_1BNFSTX,
           Y_J_1BNFFTX  TYPE J_1BNFFTX,
           Y_J_1BNFREF  TYPE J_1BNFREF,

           BEGIN OF Y_LIN,
             DOCNUM TYPE J_1BNFLIN-DOCNUM,
           END OF Y_LIN.

    TYPES: BEGIN OF Y_MATERIAL,
             MATNR TYPE J_1BNFLIN-MATNR,
             MENGE TYPE J_1BNFLIN-MENGE,
           END OF Y_MATERIAL,

           BEGIN OF Y_UNIDADE,
             MEINS TYPE J_1BNFLIN-MEINS,
             MENGE TYPE J_1BNFLIN-MENGE,
           END OF Y_UNIDADE.

    DATA: TI_PARTNER      TYPE TABLE OF Y_J_1BNFNAD,
          TI_ITEM         TYPE TABLE OF Y_J_1BNFLIN,
          TI_ITEM_TAX     TYPE TABLE OF Y_J_1BNFSTX,
          TI_HEADER_MSG   TYPE TABLE OF Y_J_1BNFFTX,
          TI_REFER_MSG    TYPE TABLE OF Y_J_1BNFREF,
          TI_HEADER_P     TYPE TABLE OF Y_J_J1BNFDOC,
          TI_PARTNER_P    TYPE TABLE OF Y_J_1BNFNAD,
          TI_VBRP         TYPE TABLE OF VBRP,
          TI_VBAK         TYPE TABLE OF VBAK,
          TI_VTTP         TYPE TABLE OF VTTP,
          TI_ITEM_P       TYPE TABLE OF Y_J_1BNFLIN,
          TI_ITEM_TAX_P   TYPE TABLE OF Y_J_1BNFSTX,
          TI_HEADER_MSG_P TYPE TABLE OF Y_J_1BNFFTX,
          TI_REFER_MSG_P  TYPE TABLE OF Y_J_1BNFREF,
          WA_VTTK         TYPE VTTK,
          WA_HEADER       TYPE J_1BNFDOC,
          WA_VBAK         TYPE VBAK,
          WA_VBRK         TYPE VBRK,
          WA_TDLNR        TYPE LFA1,
          WA_TTDS         TYPE TTDS,
          WA_LIN          TYPE Y_LIN,
          TI_LIN          TYPE TABLE OF Y_LIN,
          VL_TOT_MERC     TYPE J_1BNFLIN-NETWR,
          WA_MATERIAL     TYPE Y_MATERIAL,
          TL_MATERIAL     TYPE TABLE OF Y_MATERIAL,
          WA_UNIDADE      TYPE Y_UNIDADE,
          TL_UNIDADE      TYPE TABLE OF Y_UNIDADE,
          WA_CT_INFQ      LIKE LINE OF CT_INFQ,
          P_IDENTIFICA    TYPE ZCTE_IDENTIFICA,
          IT_NOTAS_INFO   TYPE TABLE OF ZCTE_INFO_NOTA,
          IT_CTE_TRANS    TYPE TABLE OF ZCTE_TRANS,
          IT_CTE_MOTO     TYPE TABLE OF ZCTE_MOTORISTA,
          IT_CIOT         TYPE TABLE OF ZCTE_CIOT,
          WL_ZLEST0061    TYPE ZLEST0061,
          WL_NFDOC_TMP    TYPE J_1BNFDOC,
          P_ZLEST0066     TYPE ZLEST0066,
          IT_VFKP         TYPE TABLE OF VFKP,
          IT_KONV         TYPE TABLE OF KONV,
          IT_VTTP         TYPE TABLE OF VTTP,
          IT_VBFA         TYPE TABLE OF VBFA,
          IT_VBKD         TYPE TABLE OF VBKD,
          IT_OBS_GERAL    TYPE TABLE OF ZCTE_OBS_GERAIS,
          IT_ZLEST0067    TYPE TABLE OF ZLEST0067.

    DATA: VL_CTE_COMPLEMENTO TYPE C,
          VL_CTE_REF         TYPE J_1BNFDOC-DOCREF,
          TX_OBS_GERAL       TYPE STRING,
          WA_OBS_GERAL       TYPE ZCTE_OBS_GERAIS,
          VLR_DOLAR          TYPE C LENGTH 20,
          VLR_TAXA           TYPE C LENGTH 20,
          V_QTDRAT           TYPE P DECIMALS 2,
          WL_DADOS_COMPL     TYPE J_1BCTE_INT_COMPL,
          L_OBSERVACAO       TYPE STRING, "#130983-05.01.2024-JT-inicio
          L_CTEPROC          TYPE EDOC_BR_CTE_IF_ROOT.   "*-#132427-25.01.2024-JT-inicio

    DATA: WL_INFUNITRANSP    TYPE J_1BCTE_INT_INFUNIDTRANSP.

    DATA: V_ID_INFUNIDTRANSP TYPE J_1BCTE_INT_INFUNIDTRANSP-ID,
          V_ID_TEXT          TYPE J_1BCTE_INT_TEXT-ID,
          V_ID_TEXT_SEQ_NO   TYPE J_1BCTE_INT_TEXT-SEQ_NO,
          W_TVARV1           TYPE TVARVC,  "*-#133089-21.02.2024-JT
          W_TVARV2           TYPE TVARVC.  "*-#133089-21.02.2024-JT

    SELECT SINGLE *
      FROM ZCTE_IDENTIFICA
     WHERE DOCNUM = @IS_NFDOC-DOCNUM
      INTO @DATA(WA_ZCTE_IDENTIFICA).

    IF WA_ZCTE_IDENTIFICA-TPSERV = '1'.
      SELECT SINGLE *
        FROM ZCTE_DOC_ANT
       WHERE DOCNUM = @IS_NFDOC-DOCNUM
        INTO @DATA(WA_ZCTE_DOC_ANT).

      IF SY-SUBRC IS INITIAL.

        APPEND INITIAL LINE TO  CT_IDDOCANT ASSIGNING FIELD-SYMBOL(<FS_CT_IDDOCANT>).
        <FS_CT_IDDOCANT>-ID              = '1'.
        <FS_CT_IDDOCANT>-SEQ_NO          = '1'.
        <FS_CT_IDDOCANT>-IDDOCANTELE_REF = '1'.

        APPEND INITIAL LINE TO CT_IDDOCANTELE ASSIGNING FIELD-SYMBOL(<FS_CT_IDDOCANTELE>).
        <FS_CT_IDDOCANTELE>-ID     = '1'.
        <FS_CT_IDDOCANTELE>-SEQ_NO = '1'.
        <FS_CT_IDDOCANTELE>-CHAVE = WA_ZCTE_DOC_ANT-C57_CHAVE_ACESSO.

        APPEND INITIAL LINE TO CT_EMIDOCANT ASSIGNING FIELD-SYMBOL(<FS_CT_EMIDOCANT>).
        <FS_CT_EMIDOCANT>-ID = '1'.
        <FS_CT_EMIDOCANT>-IDDOCANT_REF = '1'.
        <FS_CT_EMIDOCANT>-CNPJ  = WA_ZCTE_DOC_ANT-EMIT_ANT_CNPJ.
        <FS_CT_EMIDOCANT>-IE    = WA_ZCTE_DOC_ANT-EMIT_ANT_IE.
        <FS_CT_EMIDOCANT>-UF    = WA_ZCTE_DOC_ANT-EMIT_ANT_UF.
        <FS_CT_EMIDOCANT>-XNOME = WA_ZCTE_DOC_ANT-EMIT_ANT_NOME.
      ENDIF.
    ENDIF.

    CLEAR: VL_CTE_COMPLEMENTO, VL_CTE_REF.
    SELECT SINGLE *
      FROM J_1BNFDOC INTO @WL_NFDOC_TMP
     WHERE DOCNUM EQ @IS_NFDOC-DOCNUM
       AND DOCTYP EQ '2'
       AND MODEL  EQ @ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

    IF ( SY-SUBRC = 0 ) AND ( WL_NFDOC_TMP-DOCREF IS NOT INITIAL ). "DOCUMENTO É TIPO COMPLEMENTAR
      VL_CTE_COMPLEMENTO = 'X'.
      VL_CTE_REF         = WL_NFDOC_TMP-DOCREF.
    ENDIF.

    "VERIFICAR SE O DOCNUM ESTA GRAVADO NA TABELA DO AQUAVIÁRIO.
    SELECT SINGLE * FROM ZLEST0061 INTO WL_ZLEST0061 WHERE DOCNUM EQ IS_NFDOC-DOCNUM.

    IF ( SY-SUBRC NE 0 ) AND ( VL_CTE_REF IS NOT INITIAL ).
      CLEAR: WL_ZLEST0061.
      SELECT SINGLE * FROM ZLEST0061 INTO WL_ZLEST0061 WHERE DOCNUM EQ VL_CTE_REF.
    ENDIF.

    IF WL_ZLEST0061 IS INITIAL."( SY-SUBRC NE 0 ).

      CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
        EXPORTING
          P_CTE_AVULSO       = IS_NFDOC-DOCNUM
          P_CHAMAR_TELA      = SPACE
          P_GRAVAR_DADOS     = ABAP_TRUE
        IMPORTING
*         P_J_1BNFDOC        =
          P_IDENTIFICA       = P_IDENTIFICA
*         P_PARCEIROS        =
        TABLES
          IT_NOTAS_INFO      = IT_NOTAS_INFO
          IT_TRANS           = IT_CTE_TRANS
          IT_CIOT            = IT_CIOT
          IT_MOTORISTA       = IT_CTE_MOTO
          IT_OBS_GERAL       = IT_OBS_GERAL
*         IT_SEGURO          =
        EXCEPTIONS
          N_INFO_CTE         = 1
          N_LOC_CTE          = 2
          N_ELETRONICO       = 3
          N_MODELO_57        = 4
          N_STATUS           = 5
          INF_DOCNUM         = 6
          INF_PROPVEICULO    = 7
          NAO_DOCNUM         = 8
          NAO_RTRC           = 9
          NAO_CONTA_CORRENTE = 10
          MOTO_NAO_PF        = 11
          N_PLACA_CAD        = 12
          SEM_NOTAS          = 13
          OTHERS             = 14.


      CALL FUNCTION 'Z_SD_NFES_DA_CTE'
        EXPORTING
          MP_DOCNUM          = IS_NFDOC-DOCNUM
        TABLES
          MT_PARTNER         = TI_PARTNER
          MT_ITEM            = TI_ITEM
          MT_ITEM_TAX        = TI_ITEM_TAX
          MT_HEADER_MSG      = TI_HEADER_MSG
          MT_REFER_MSG       = TI_REFER_MSG
          MT_VBRP            = TI_VBRP
          MT_VBAK            = TI_VBAK
          MT_VTTP            = TI_VTTP
          MT_HEADRE_NOTA_NFE = TI_HEADER_P
          MT_ITEM_NOTA       = TI_ITEM_P
          MT_PARTNER_P       = TI_PARTNER_P
          MT_ITEM_TAX_P      = TI_ITEM_TAX_P
          MT_HEADER_MSG_P    = TI_HEADER_MSG_P
          MT_REFER_MSG_P     = TI_REFER_MSG_P
        CHANGING
          MR_VTTK            = WA_VTTK
          MR_HEADER_NOTA_CTE = WA_HEADER
          MR_VBAK            = WA_VBAK
          MR_VBRK            = WA_VBRK
          MR_LFA1            = WA_TDLNR
          MR_TTDS            = WA_TTDS.

      IF WA_ZCTE_IDENTIFICA-TPSERV NE '1'.
        CHECK TI_VBRP  IS NOT INITIAL.
        CHECK WA_VBRK  IS NOT INITIAL.
        CHECK TI_VBAK  IS NOT INITIAL.
        CHECK WA_VTTK  IS NOT INITIAL.
        CHECK WA_TDLNR IS NOT INITIAL.
        CHECK WA_TTDS  IS NOT INITIAL.
        CHECK TI_VTTP  IS NOT INITIAL.
        CHECK WA_VBAK  IS NOT INITIAL.
      ENDIF.

      LOOP AT TI_ITEM_P INTO DATA(WA_ITEM_P).
        WA_LIN-DOCNUM = WA_ITEM_P-DOCNUM.
        APPEND WA_LIN TO TI_LIN.
      ENDLOOP.

      SELECT SINGLE * FROM ZLEST0061 INTO @DATA(WA_ZLEST0061) WHERE DOCNUM EQ @IS_NFDOC-DOCNUM.

      READ TABLE TI_VBRP INTO DATA(WA_VBRP) INDEX 1.

      DATA VL_TABIX TYPE SY-TABIX.

      LOOP AT TI_PARTNER ASSIGNING FIELD-SYMBOL(<FS_PARTINER>).

        CHECK: ( <FS_PARTINER>-CGC IS INITIAL   AND
                 <FS_PARTINER>-CPF IS INITIAL ) OR
                 <FS_PARTINER>-NAME1 IS INITIAL OR
                 <FS_PARTINER>-STAINS IS INITIAL.
        VL_TABIX = SY-TABIX.

        SELECT SINGLE *
          INTO @DATA(WA_KNA1)
          FROM KNA1
         WHERE KUNNR EQ @<FS_PARTINER>-PARID.

        IF <FS_PARTINER>-NAME1 IS INITIAL.
          <FS_PARTINER>-NAME1 = WA_KNA1-NAME1.
        ENDIF.
        IF <FS_PARTINER>-CGC IS INITIAL.
          <FS_PARTINER>-CGC = WA_KNA1-STCD1.
        ENDIF.
        IF <FS_PARTINER>-CPF IS INITIAL.
          <FS_PARTINER>-CPF = WA_KNA1-STCD2.
        ENDIF.
        IF <FS_PARTINER>-STAINS IS INITIAL.
          <FS_PARTINER>-STAINS = WA_KNA1-STCD3.
        ENDIF.

      ENDLOOP.

      CLEAR VL_TOT_MERC.
      IF WA_HEADER-NTGEW IS NOT INITIAL AND WA_HEADER-BRGEW IS NOT INITIAL AND WA_HEADER-GEWEI EQ 'KG'.
        CLEAR: WA_CT_INFQ.
        WA_CT_INFQ-ID     = 1.
        WA_CT_INFQ-CUNID  = C_01.
        WA_CT_INFQ-TPMED  = C_PESO_BRUTO.
        WA_CT_INFQ-QCARGA = WA_HEADER-BRGEW.
        APPEND WA_CT_INFQ TO CT_INFQ.

        CLEAR: WA_CT_INFQ.
        WA_CT_INFQ-ID     = 2.
        WA_CT_INFQ-CUNID  = C_01.
        WA_CT_INFQ-TPMED  = C_PESO_LIQUI.
        WA_CT_INFQ-QCARGA = WA_HEADER-NTGEW.
        APPEND WA_CT_INFQ TO CT_INFQ.
      ENDIF.

      IF CT_INFQ IS NOT INITIAL.
        DATA(LC_NAO_INCLUIR_UNIDADE) = ABAP_TRUE.
      ELSE.
        LC_NAO_INCLUIR_UNIDADE = ABAP_FALSE.
      ENDIF.

      IF TI_ITEM_P[] IS NOT INITIAL.
        LOOP AT TI_ITEM_P INTO WA_ITEM_P.
          ADD WA_ITEM_P-NETWR TO VL_TOT_MERC.

          WA_MATERIAL-MATNR = WA_ITEM_P-MATNR.
          WA_MATERIAL-MENGE = WA_ITEM_P-MENGE.
          COLLECT WA_MATERIAL INTO TL_MATERIAL.

          IF LC_NAO_INCLUIR_UNIDADE EQ ABAP_FALSE.
            WA_UNIDADE-MEINS = WA_ITEM_P-MEINS.
            WA_UNIDADE-MENGE = WA_ITEM_P-MENGE.
            COLLECT WA_UNIDADE INTO TL_UNIDADE.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT IT_NOTAS_INFO INTO DATA(WA_NOTAS_INFO).
          ADD WA_NOTAS_INFO-VL_NOTA_FISCAL TO VL_TOT_MERC.

          WA_MATERIAL-MATNR = WA_NOTAS_INFO-MATERIAL.
          WA_MATERIAL-MENGE = WA_NOTAS_INFO-QUANTIDADE.
          COLLECT WA_MATERIAL INTO TL_MATERIAL.

          IF LC_NAO_INCLUIR_UNIDADE EQ ABAP_FALSE.
            WA_UNIDADE-MEINS = WA_NOTAS_INFO-UNIDADE.
            WA_UNIDADE-MENGE = WA_NOTAS_INFO-QUANTIDADE.
            COLLECT WA_UNIDADE INTO TL_UNIDADE.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SORT TL_MATERIAL BY MENGE DESCENDING.
      READ TABLE TL_MATERIAL INTO WA_MATERIAL INDEX 1.

      SELECT SINGLE MAKTX INTO @DATA(NM_PRO_PRED)
        FROM MAKT
       WHERE MATNR EQ @WA_MATERIAL-MATNR
         AND SPRAS EQ @SY-LANGU.

      IF SY-SUBRC NE 0.
        IF WA_ZCTE_IDENTIFICA-TPSERV = '1'.

          SELECT SINGLE *
            FROM ZLEST0194
           WHERE DOCNUM_SUB = @IS_NFDOC-DOCNUM
            INTO @DATA(WA_ZLEST0194).

          IF SY-SUBRC IS INITIAL.
            NM_PRO_PRED = WA_ZLEST0194-DS_PROD_PRED.
          ENDIF.
        ENDIF.
      ENDIF.

*      ST_VTTK-VSART
*      01	RODOVIARIO
*      02	FERROVIÁRIO
*      03	NAVEGAÇÃO FLUVIAL
*      04	MARÍTIMO
*      05	AÉREO
*      06	CORREIO, SERV.POSTAL
*      07	MULTIMODAL

*      PREENCHER COM:
*      01-RODOVIÁRIO;
*      02-AÉREO;
*      03-AQUAVIÁRIO;
*      04-FERROVIÁRIO;
*      05-DUTOVIÁRIO;
*      06-MULTIMODAL

*---------------------------------------------------------------------------------------------*
*   Indentificação
*---------------------------------------------------------------------------------------------*

      IF P_IDENTIFICA-MODAL IS NOT INITIAL.
        CS_IDE-MODAL = P_IDENTIFICA-MODAL.
      ELSE.
        CASE WA_VTTK-VSART.
          WHEN '01'.
            CS_IDE-MODAL = '01'.
          WHEN '02'.
            CS_IDE-MODAL = '04'.
          WHEN '03'.
            CS_IDE-MODAL = '03'.
          WHEN '04'.
            CS_IDE-MODAL = '03'.
          WHEN '05'.
            CS_IDE-MODAL = '02'.
          WHEN '07'.
            CS_IDE-MODAL = '06'.
        ENDCASE.
      ENDIF.

      CASE CS_IDE-MODAL.
        WHEN '01'.
          CS_RODO-RNTRC = P_IDENTIFICA-RODO_RNTRC.

          IF P_IDENTIFICA-RODO_FRETE_LOTA EQ '1'.
            "PEDÁGIO
            IF P_IDENTIFICA-VLR_PEDAGIO GE 0.
            ENDIF.
          ENDIF.
      ENDCASE.

      CS_IDE-TP_SERV   = ZCL_STRING=>TIRA_ACENTOS( CONV #( P_IDENTIFICA-TPSERV ) ).  "*-#127337-21.11.2023-JT
      CS_IDE-C_MUN_INI = ZCL_STRING=>TIRA_ACENTOS( CONV #( P_IDENTIFICA-CMUNINI ) ). "*-#127337-21.11.2023-JT
      CS_IDE-UFINI     = P_IDENTIFICA-UFINI.
      CS_IDE-X_MUN_INI = ZCL_STRING=>UPPER( I_STR = CONV #( P_IDENTIFICA-NMUNINI ) ).
      CS_IDE-X_MUN_INI = ZCL_STRING=>TIRA_ACENTOS( CONV #( CS_IDE-X_MUN_INI ) ).     "*-#127337-21.11.2023-JT

      CS_IDE-UFFIM     = P_IDENTIFICA-UFFIM.
      CS_IDE-C_MUN_FIM = ZCL_STRING=>TIRA_ACENTOS( CONV #( P_IDENTIFICA-CMUNFIM ) ). "*-#127337-21.11.2023-JT
      CS_IDE-X_MUN_FIM = ZCL_STRING=>UPPER( I_STR = CONV #( P_IDENTIFICA-NMUNFIM ) )."*-#127337-21.11.2023-JT
      CS_IDE-X_MUN_FIM = ZCL_STRING=>TIRA_ACENTOS( CONV #( CS_IDE-X_MUN_FIM ) ).     "*-#127337-21.11.2023-JT

*---------------------------------------------------------------------------------------------*
*   Inf. Carga
*---------------------------------------------------------------------------------------------*
      CS_INFCARGA-V_CARGA       = VL_TOT_MERC.
      CS_INFCARGA-PRO_PRED      = NM_PRO_PRED.
      CS_INFCARGA-V_CARGA_AVERB = VL_TOT_MERC.

*-#132427-25.01.2024-JT-inicio
      IF CS_INFCARGA-V_CARGA IS INITIAL.
        L_CTEPROC           = GET_DADOS_CTE_ANTERIOR( CT_IDDOCANTELE ).
        CS_INFCARGA-V_CARGA = L_CTEPROC-CTE-INFCTE-INFCTENORM-INFCARGA-VCARGA.
      ENDIF.
*-#132427-25.01.2024-JT-fim

*---------------------------------------------------------------------------------------------*
*   retira
*---------------------------------------------------------------------------------------------*
      CS_IDE-RETIRA = '1'.

*---------------------------------------------------------------------------------------------*
*   Inf. Quantidade
*---------------------------------------------------------------------------------------------*
      IF LC_NAO_INCLUIR_UNIDADE EQ ABAP_FALSE.
        LOOP AT TL_UNIDADE INTO WA_UNIDADE.
          CLEAR: WA_CT_INFQ.
          WA_CT_INFQ-ID = SY-TABIX.

          CASE WA_UNIDADE-MEINS.
            WHEN C_KG.
              WA_CT_INFQ-CUNID = C_01.
              WA_CT_INFQ-TPMED = C_PESO_BRUTO.
            WHEN C_TO.
              WA_CT_INFQ-CUNID = C_02.
              WA_CT_INFQ-TPMED = C_PESO_BRUTO.
            WHEN C_UN.
              WA_CT_INFQ-CUNID = C_03.
              WA_CT_INFQ-TPMED = C_CAIXA.
            WHEN C_LT.
              WA_CT_INFQ-CUNID = C_04.
              WA_CT_INFQ-TPMED = C_LITRAGEM.
            WHEN OTHERS.
              WA_CT_INFQ-CUNID = C_01.
              WA_CT_INFQ-TPMED = C_PESO_BRUTO.
          ENDCASE.

          WA_CT_INFQ-QCARGA = WA_UNIDADE-MENGE.
          APPEND WA_CT_INFQ TO CT_INFQ.
        ENDLOOP.
      ENDIF.

*---------------------------------------------------------------------------------------------*
*   Notas Fiscais
*---------------------------------------------------------------------------------------------*

      DATA: WA_INFDOC_INFNFE TYPE J_1BCTE_INT_INFNFE_200.
      DATA: WA_INFDOC_INFNF  TYPE J_1BCTE_INT_INFNF_200.

      LOOP AT IT_NOTAS_INFO INTO WA_NOTAS_INFO.
        IF WA_NOTAS_INFO-MODELO EQ '55'.
          CLEAR: WA_INFDOC_INFNFE.
          WA_INFDOC_INFNFE-CHAVE = WA_NOTAS_INFO-CHAVE.
          WA_INFDOC_INFNFE-PIN   = WA_NOTAS_INFO-PIN_SUFRAMA.
          APPEND WA_INFDOC_INFNFE TO CT_INFDOC_INFNFE.
        ELSE.
          CLEAR: WA_INFDOC_INFNF.
          WA_INFDOC_INFNF-DEMI  = WA_NOTAS_INFO-DTEMISSAO.
          WA_INFDOC_INFNF-MOD   = WA_NOTAS_INFO-MODELO.
          WA_INFDOC_INFNF-SERIE = WA_NOTAS_INFO-SERIE.
          WA_INFDOC_INFNF-NDOC  = WA_NOTAS_INFO-NUMERO.
          WA_INFDOC_INFNF-VBC   = WA_NOTAS_INFO-VL_BC.
          WA_INFDOC_INFNF-VICMS = WA_NOTAS_INFO-VL_ICMS.
          WA_INFDOC_INFNF-VBCST = WA_NOTAS_INFO-VL_BC_ST.
          WA_INFDOC_INFNF-VST   = WA_NOTAS_INFO-VL_ST.
          WA_INFDOC_INFNF-VPROD = WA_NOTAS_INFO-VL_PRODUTOS.
          WA_INFDOC_INFNF-VNF   = WA_NOTAS_INFO-VL_NOTA_FISCAL.
          WA_INFDOC_INFNF-NCFOP = WA_NOTAS_INFO-CFOP(4).
          WA_INFDOC_INFNF-NPESO = WA_NOTAS_INFO-QUANTIDADE.
          WA_INFDOC_INFNF-PIN   = WA_NOTAS_INFO-PIN_SUFRAMA.

          IF WA_NOTAS_INFO-UNIDADE NE 'KG'.
            CALL FUNCTION 'ME_CONVERSION_MEINS'
              EXPORTING
                I_MATNR             = WA_NOTAS_INFO-MATERIAL
                I_MEIN1             = WA_NOTAS_INFO-UNIDADE
                I_MEINS             = 'KG'
                I_MENGE             = WA_NOTAS_INFO-QUANTIDADE
              IMPORTING
                MENGE               = WA_NOTAS_INFO-QUANTIDADE
              EXCEPTIONS
                ERROR_IN_CONVERSION = 1
                NO_SUCCESS          = 2
                OTHERS              = 3.
          ENDIF.

          IF SY-SUBRC IS INITIAL.
            WA_INFDOC_INFNF-NPESO = WA_NOTAS_INFO-QUANTIDADE.
          ENDIF.

          IF ( WA_NOTAS_INFO-UNIDADE NE 'KG' ) AND ( WA_NOTAS_INFO-PESO_FISCAL > 0 ).
            CALL FUNCTION 'ME_CONVERSION_MEINS'
              EXPORTING
                I_MATNR             = WA_NOTAS_INFO-MATERIAL
                I_MEIN1             = WA_NOTAS_INFO-UNIDADE
                I_MEINS             = 'KG'
                I_MENGE             = WA_NOTAS_INFO-PESO_FISCAL
              IMPORTING
                MENGE               = WA_NOTAS_INFO-PESO_FISCAL
              EXCEPTIONS
                ERROR_IN_CONVERSION = 1
                NO_SUCCESS          = 2
                OTHERS              = 3.

            IF SY-SUBRC IS INITIAL.
              WA_INFDOC_INFNF-NPESO = WA_NOTAS_INFO-QUANTIDADE.
            ENDIF.
          ENDIF.
          APPEND WA_INFDOC_INFNF TO CT_INFDOC_INFNF.

        ENDIF.
      ENDLOOP.

*---------------------------------------------------------------------------------------------*
*   Obs. Complementares
*---------------------------------------------------------------------------------------------*
      DATA: VG_KINAK           TYPE KINAK,
            VG_KWERT           TYPE KWERT,
            VALOR_PEDAGIO      TYPE C LENGTH 20,
            VG_CONTRATO_VIAGEM TYPE ZNUCONTRATO,
            TEXTO_PEDAGIO      TYPE STRING,
            TX_CIOT            TYPE STRING,
            VL_XTEXTO_CONTRIB  TYPE STRING,
            VL_XTEXTO          TYPE STRING.

      CLEAR: TX_OBS_GERAL, IT_VFKP[], TEXTO_PEDAGIO, VG_KWERT, VG_CONTRATO_VIAGEM, IT_KONV[], VG_KWERT.

      LOOP AT IT_OBS_GERAL INTO WA_OBS_GERAL.
        CONCATENATE TX_OBS_GERAL WA_OBS_GERAL-TEXTO INTO TX_OBS_GERAL SEPARATED BY SPACE.
      ENDLOOP.

      SELECT *
        FROM VFKP
        INTO CORRESPONDING FIELDS OF TABLE IT_VFKP
       WHERE REFTY EQ '8'
         AND REBEL EQ P_IDENTIFICA-TKNUM.

      IF IT_VFKP[] IS NOT INITIAL.
*---> S4 MIGRATION 10/07/2023 - MA
*        SELECT *
*          FROM KONV
*          INTO CORRESPONDING FIELDS OF TABLE IT_KONV
*          FOR ALL ENTRIES IN IT_VFKP
*         WHERE KNUMV EQ IT_VFKP-KNUMV
*           AND KSCHL EQ 'ZPED'
*           AND KINAK EQ VG_KINAK
*           AND KWERT GT 0.
        DATA: LC_KSCHL TYPE KSCHL VALUE 'ZPED',
              LC_KWERT TYPE KWERT VALUE 0.

        SELECT *
          FROM V_KONV
          INTO TABLE @DATA(LT_KONV)
          FOR ALL ENTRIES IN @IT_VFKP
         WHERE KNUMV EQ @IT_VFKP-KNUMV
           AND KSCHL EQ @LC_KSCHL
           AND KINAK EQ @VG_KINAK
           AND KWERT GT @LC_KWERT.

        MOVE-CORRESPONDING LT_KONV[] TO IT_KONV[].
*---> S4 MIGRATION 10/07/2023 - MA


        IF SY-SUBRC EQ 0.
          LOOP AT IT_KONV INTO DATA(WL_KONV).
            IF WL_KONV-KWERT GT 0.
              VG_KWERT = VG_KWERT + WL_KONV-KWERT.
            ENDIF.
          ENDLOOP.
          IF VG_KWERT GT 0.
            WRITE VG_KWERT TO VALOR_PEDAGIO.
            SHIFT VALOR_PEDAGIO LEFT DELETING LEADING SPACE.
            CONCATENATE 'Valor do pedágio:' VALOR_PEDAGIO INTO TEXTO_PEDAGIO SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF TEXTO_PEDAGIO IS NOT INITIAL.
        IF TX_OBS_GERAL IS INITIAL.
          TX_OBS_GERAL = TEXTO_PEDAGIO.
        ELSE.
          CONCATENATE TX_OBS_GERAL TEXTO_PEDAGIO INTO TX_OBS_GERAL SEPARATED BY SPACE.
        ENDIF.
      ENDIF.

      LOOP AT IT_CIOT INTO DATA(WA_IT_CIOT).
        VG_CONTRATO_VIAGEM = WA_IT_CIOT-NUCONTRATO.
        IF NOT WA_IT_CIOT-NR_CIOT IS INITIAL.
          CONCATENATE 'Nr. CIOT:' WA_IT_CIOT-NR_CIOT INTO TX_CIOT SEPARATED BY SPACE.

          IF TX_OBS_GERAL IS INITIAL.
            TX_OBS_GERAL = TX_CIOT.
          ELSE.
            CONCATENATE TX_OBS_GERAL TX_CIOT INTO TX_OBS_GERAL SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF NOT VG_CONTRATO_VIAGEM IS INITIAL.
        CONCATENATE 'Nr. Contrato de Viagem Administradora:' VG_CONTRATO_VIAGEM INTO TX_CIOT SEPARATED BY SPACE.

        IF TX_OBS_GERAL IS INITIAL.
          TX_OBS_GERAL = TX_CIOT.
        ELSE.
          CONCATENATE TX_OBS_GERAL TX_CIOT INTO TX_OBS_GERAL SEPARATED BY SPACE.
        ENDIF.
      ENDIF.

      "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - inicio
      SELECT * FROM VTTP
        INTO CORRESPONDING FIELDS OF TABLE IT_VTTP
       WHERE TKNUM EQ P_IDENTIFICA-TKNUM.

      IF IT_VTTP[] IS NOT INITIAL.
        SELECT * FROM VBFA
          INTO CORRESPONDING FIELDS OF TABLE IT_VBFA
          FOR ALL ENTRIES IN IT_VTTP
        WHERE VBELN EQ IT_VTTP-VBELN.

        IF IT_VBFA[] IS NOT INITIAL.
          SELECT * FROM VBKD
           INTO CORRESPONDING FIELDS OF TABLE IT_VBKD
           FOR ALL ENTRIES IN IT_VBFA
          WHERE VBELN EQ IT_VBFA-VBELV
            AND POSNR EQ '000010'.
        ENDIF.

        IF NOT IT_VBKD[] IS INITIAL.
          LOOP AT IT_VBKD INTO DATA(WL_VBKD).
            CONCATENATE TX_OBS_GERAL WL_VBKD-BSTKD_E INTO TX_OBS_GERAL SEPARATED BY SPACE.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - fim

      CS_COMPL-XOBS = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( TX_OBS_GERAL ) ) ).

*-#127337-21.11.2023-JT-inicio
      CONDENSE CS_COMPL-XOBS.
*-#127337-21.11.2023-JT-fim

      "Observações Contribuinte.
      SELECT SINGLE *
        FROM VTTK INTO @DATA(WL_DOC_TRANSP)
       WHERE TKNUM EQ @P_IDENTIFICA-TKNUM.

      IF ( SY-SUBRC EQ 0 ) AND ( IT_CTE_TRANS[] IS NOT INITIAL ) AND ( P_IDENTIFICA-DOCNUM IS NOT INITIAL ).

        CLEAR: V_ID_TEXT_SEQ_NO.
        ADD 1 TO V_ID_TEXT.

        CS_COMPL-TEXT_ID_OBSCONT = V_ID_TEXT.

        "Placa do cavalo
        IF WL_DOC_TRANSP-TEXT1 IS NOT INITIAL.
          READ TABLE IT_CTE_TRANS INTO DATA(_WL_CTE_TRANS) WITH KEY PC_VEICULO = WL_DOC_TRANSP-TEXT1.
          IF SY-SUBRC = 0.
            VL_XTEXTO = ME->FORMATA_TEXTO_VEICULO( I_CTE_TRANS = _WL_CTE_TRANS ).

            ADD 1 TO V_ID_TEXT_SEQ_NO.

            APPEND VALUE #( ID = V_ID_TEXT SEQ_NO = V_ID_TEXT_SEQ_NO TYP = 'Placa Cavalo' TEXT = VL_XTEXTO  ) TO CT_TEXT.
          ENDIF.
        ENDIF.

        "Placa Carreta 1
        IF WL_DOC_TRANSP-TEXT2 IS NOT INITIAL.
          READ TABLE IT_CTE_TRANS INTO _WL_CTE_TRANS WITH KEY PC_VEICULO = WL_DOC_TRANSP-TEXT2.
          IF SY-SUBRC = 0.
            VL_XTEXTO = ME->FORMATA_TEXTO_VEICULO( I_CTE_TRANS = _WL_CTE_TRANS ).

            ADD 1 TO V_ID_TEXT_SEQ_NO.

            APPEND VALUE #( ID = V_ID_TEXT SEQ_NO = V_ID_TEXT_SEQ_NO TYP = 'Placa Carreta 1' TEXT = VL_XTEXTO ) TO CT_TEXT.
          ENDIF.
        ENDIF.

        "Placa Carreta 2
        IF WL_DOC_TRANSP-TEXT3 IS NOT INITIAL.
          READ TABLE IT_CTE_TRANS INTO _WL_CTE_TRANS WITH KEY PC_VEICULO = WL_DOC_TRANSP-TEXT3.
          IF SY-SUBRC = 0.
            VL_XTEXTO = ME->FORMATA_TEXTO_VEICULO( I_CTE_TRANS = _WL_CTE_TRANS ).

            ADD 1 TO V_ID_TEXT_SEQ_NO.

            APPEND VALUE #( ID = V_ID_TEXT  SEQ_NO = V_ID_TEXT_SEQ_NO  TYP = 'Placa Carreta 2' TEXT = VL_XTEXTO ) TO CT_TEXT.
          ENDIF.
        ENDIF.

        "Placa Carreta 3
        IF WL_DOC_TRANSP-TEXT4 IS NOT INITIAL.
          READ TABLE IT_CTE_TRANS INTO _WL_CTE_TRANS WITH KEY PC_VEICULO = WL_DOC_TRANSP-TEXT4.
          IF SY-SUBRC = 0.
            VL_XTEXTO = ME->FORMATA_TEXTO_VEICULO( I_CTE_TRANS = _WL_CTE_TRANS ).

            ADD 1 TO V_ID_TEXT_SEQ_NO.

            APPEND VALUE #( ID = V_ID_TEXT  SEQ_NO = V_ID_TEXT_SEQ_NO TYP = 'Placa Carreta 3' TEXT = VL_XTEXTO ) TO CT_TEXT.
          ENDIF.
        ENDIF.

        LOOP AT IT_CTE_MOTO INTO DATA(_WL_CTE_MOTO).
          CONCATENATE _WL_CTE_MOTO-XNOME '- CPF:' _WL_CTE_MOTO-CPF INTO VL_XTEXTO.

          ADD 1 TO V_ID_TEXT_SEQ_NO.

          APPEND VALUE #( ID = V_ID_TEXT  SEQ_NO = V_ID_TEXT_SEQ_NO  TYP = 'Motorista' TEXT = VL_XTEXTO ) TO CT_TEXT.
        ENDLOOP.
      ENDIF.

    ELSE.

      CLEAR: WA_CT_INFQ.

      CALL FUNCTION 'Z_LES_INFO_AQUAV'
        EXPORTING
          P_CTE_AVULSO  = IS_NFDOC-DOCNUM
          P_COMPLEMENTO = VL_CTE_COMPLEMENTO
        IMPORTING
          P_ZLEST0066   = P_ZLEST0066
        TABLES
          IT_ZLEST0067  = IT_ZLEST0067
          IT_CTE_OBSG   = IT_OBS_GERAL.

*---------------------------------------------------------------------------------------------*
*   Identificação
*---------------------------------------------------------------------------------------------*
      CS_IDE-MODAL              = P_ZLEST0066-MODAL.
      CS_IDE-TP_SERV            = P_ZLEST0066-TPSERV.

      CS_IDE-C_MUN_INI          = P_ZLEST0066-CMUNINI.
      CS_IDE-UFINI              = P_ZLEST0066-UFINI.
      CS_IDE-X_MUN_INI          = P_ZLEST0066-XMUNINI.

      CS_IDE-C_MUN_FIM          = P_ZLEST0066-CMUNFIM.
      CS_IDE-UFFIM              = P_ZLEST0066-UFFIM.
      CS_IDE-X_MUN_FIM          = P_ZLEST0066-XMUNFIM.
      CS_IDE-RETIRA             = '1'.

*---------------------------------------------------------------------------------------------*
*   Obs. Complementares
*---------------------------------------------------------------------------------------------*
      LOOP AT IT_OBS_GERAL INTO WA_OBS_GERAL.
        CONCATENATE CS_COMPL-XOBS WA_OBS_GERAL-TEXTO INTO CS_COMPL-XOBS SEPARATED BY SPACE.
      ENDLOOP.

      "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - INICIO
      ZCL_EUDR_UTILS=>GET_TEXTOS_EUDR_NF_SAIDA(
        EXPORTING
          I_DOCNUM = IS_NFDOC-DOCNUM              " Nº documento
        RECEIVING
          R_TEXTOS = DATA(LVA_TEXTOS_EUDR) ).             " Textos
      IF LVA_TEXTOS_EUDR IS NOT INITIAL.
        CONCATENATE CS_COMPL-XOBS LVA_TEXTOS_EUDR INTO CS_COMPL-XOBS SEPARATED BY SPACE.
      ENDIF.
      "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - FIM

      CS_COMPL-XOBS = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( CS_COMPL-XOBS ) ) ).

*-#127337-21.11.2023-JT-inicio
      CONDENSE CS_COMPL-XOBS.
*-#127337-21.11.2023-JT-fim

*---------------------------------------------------------------------------------------------*
*   Valor Prestação
*---------------------------------------------------------------------------------------------*


*---------------------------------------------------------------------------------------------*
*   Inf. Carga
*---------------------------------------------------------------------------------------------*
      CS_INFCARGA-V_CARGA   = P_ZLEST0066-VCARGA.
      CS_INFCARGA-PRO_PRED  = P_ZLEST0066-PROPRED.


*---------------------------------------------------------------------------------------------*
*   Inf. Quantidade
*---------------------------------------------------------------------------------------------*
      CLEAR: WA_CT_INFQ.
      WA_CT_INFQ-ID = 1.
      WA_CT_INFQ-CUNID  = P_ZLEST0066-CUNID.
      WA_CT_INFQ-TPMED  = P_ZLEST0066-TPMED.
      WA_CT_INFQ-QCARGA = P_ZLEST0066-QCARGA.
      APPEND WA_CT_INFQ TO CT_INFQ.

*---------------------------------------------------------------------------------------------*
*   Notas Fiscais
*---------------------------------------------------------------------------------------------*
      DATA(_RATEIO_NF) = 'X'.
      LOOP AT IT_ZLEST0067 INTO DATA(WL_ZLEST0067) WHERE CHAVE IS NOT INITIAL.

        SELECT SINGLE *
          FROM ZLEST0060 INTO @DATA(_WL_0060)
         WHERE CHAVE_NFE = @WL_ZLEST0067-CHAVE.

        CHECK SY-SUBRC = 0.

        SELECT SINGLE *
          FROM ZLEST0063 INTO @DATA(_WL_0063)
         WHERE BUKRS       =  @_WL_0060-BUKRS
           AND WERKS       =  @_WL_0060-WERKS
           AND NR_VIAGEM   =  @_WL_0060-NR_VIAGEM
           AND ANO_VIAGEM  =  @_WL_0060-ANO_VIAGEM
           AND EMBARCACAO  =  @_WL_0060-EMBARCACAO
           AND NOME_EMB    =  @_WL_0060-NOME_EMB.

        CHECK SY-SUBRC = 0.

        "INSUMOS
        SELECT SINGLE *
          FROM SETLEAF INTO @DATA(_WL_SETLEAF)
         WHERE SETNAME EQ 'MAGGI_ZLES0077_GR_MAT'
           AND VALFROM EQ @_WL_0063-GR_MATERIAL.

        CHECK SY-SUBRC = 0.

        _RATEIO_NF = ''.

        EXIT.
      ENDLOOP.

      CLEAR: V_ID_INFUNIDTRANSP.
      LOOP AT IT_ZLEST0067 INTO WL_ZLEST0067.

        CLEAR: WA_INFDOC_INFNFE, WA_INFDOC_INFNF.

        IF _RATEIO_NF IS NOT INITIAL.
          CLEAR: WL_INFUNITRANSP.
          ADD 1 TO V_ID_INFUNIDTRANSP.
          REPLACE ALL OCCURRENCES OF  '-' IN WL_ZLEST0067-NOME_EMB WITH ''.
          CONDENSE WL_ZLEST0067-NOME_EMB NO-GAPS.
          V_QTDRAT = WL_ZLEST0067-QCARGA / 1000.
          WA_INFDOC_INFNFE-INFUNIDTRANSP_REF = V_ID_INFUNIDTRANSP.
          WL_INFUNITRANSP-ID                 = V_ID_INFUNIDTRANSP.
          WL_INFUNITRANSP-IDUNIDTRANSP       = WL_ZLEST0067-NOME_EMB.
          WL_INFUNITRANSP-TPUNIDTRANSP       = '4'."4 = BALSA
          WL_INFUNITRANSP-QTDRAT             = V_QTDRAT.
          APPEND WL_INFUNITRANSP TO CT_INFUNIDTRANSP.
          WA_INFDOC_INFNF-INFUNIDTRANSP_REF = V_ID_INFUNIDTRANSP.
          WA_INFDOC_INFNFE-INFUNIDTRANSP_REF = V_ID_INFUNIDTRANSP.
        ENDIF.

        IF WL_ZLEST0067-CHAVE(1) = 'F'.

          SELECT SINGLE *
            FROM ZLEST0060 INTO @DATA(_WA_0060)
           WHERE CHAVE_NFE = @WL_ZLEST0067-CHAVE.

          SELECT SINGLE CFOP
            INTO @DATA(_CFOP)
            FROM ZSDT0001
           WHERE CH_REFERENCIA = @_WA_0060-CH_REFERENCIA.

          IF SY-SUBRC NE 0.
            SELECT SINGLE *
              INTO @DATA(LWA_ZLEST0205)
              FROM ZLEST0205
              WHERE CHAVE_NFE = @_WA_0060-CHAVE_NFE.

            IF SY-SUBRC EQ 0.
              _CFOP = LWA_ZLEST0205-CFOP.
            ENDIF.
          ENDIF.

          REPLACE '/' WITH SPACE INTO _CFOP.
          CONDENSE _CFOP NO-GAPS.


          WA_INFDOC_INFNF-DEMI  = _WA_0060-DOCDAT.
          WA_INFDOC_INFNF-MOD   = '04'.
          WA_INFDOC_INFNF-SERIE = _WA_0060-SERIES.
          WA_INFDOC_INFNF-NDOC  = _WA_0060-NFNUM.
          WA_INFDOC_INFNF-VBC   = _WA_0060-NETWR.
          WA_INFDOC_INFNF-VICMS = 0.
          WA_INFDOC_INFNF-VBCST = 0.
          WA_INFDOC_INFNF-VST   = 0.
          WA_INFDOC_INFNF-VPROD = _WA_0060-NETWR.
          WA_INFDOC_INFNF-VNF   = _WA_0060-NETWR.
          WA_INFDOC_INFNF-NCFOP = _CFOP(4).
          APPEND WA_INFDOC_INFNF TO CT_INFDOC_INFNF.
        ELSE.
          WA_INFDOC_INFNFE-CHAVE = WL_ZLEST0067-CHAVE.
          APPEND WA_INFDOC_INFNFE TO CT_INFDOC_INFNFE.
        ENDIF.

      ENDLOOP.

*---------------------------------------------------------------------------------------------*
*   Informações Modal
*---------------------------------------------------------------------------------------------*
      CS_AQUAV-V_PREST = P_ZLEST0066-VPREST.
      CS_AQUAV-V_AFRMM = P_ZLEST0066-VAFRMM.
      CS_AQUAV-X_NAVIO = P_ZLEST0066-XNAVIO.

      ADD 1 TO V_ID_TEXT.
      CS_AQUAV-TEXT_ID_BALSA = V_ID_TEXT.
      APPEND VALUE #( ID = V_ID_TEXT TEXT = P_ZLEST0066-XBALSA ) TO CT_TEXT.

      CS_AQUAV-N_VIAG = |{ P_ZLEST0066-NVIAG ALPHA = OUT }| ..
      CS_AQUAV-DIREC  = P_ZLEST0066-DIREC.
      CS_AQUAV-IRIN   = P_ZLEST0066-IRIN.

    ENDIF.

*-#130983-05.01.2024-JT-inicio
*---------------------------------------------------------------------------------------------*
*-- Impostos
*---------------------------------------------------------------------------------------------*
    CALL FUNCTION 'Z_SD_IMPOSTOS_CTE'
      EXPORTING
        IT_NFLIN     = IT_NFLIN
      IMPORTING
        E_OBSERVACAO = L_OBSERVACAO.

    CS_COMPL-XOBS = CS_COMPL-XOBS && L_OBSERVACAO.
**-#130983-05.01.2024-JT-fim

*---------------------------------------------------------------------------------------------*
*   Outras Tratativas
*---------------------------------------------------------------------------------------------*

    IF SY-SYSID NE 'PRD'.

*-#133089-21.02.2024-JT-inicio
      FREE: W_TVARV1, W_TVARV2.

      SELECT *
        FROM TVARVC
        INTO TABLE @DATA(T_TVARVC)
        WHERE NAME = 'TEXTO_XML_CTE_QAS'.

      READ TABLE T_TVARVC INTO W_TVARV1 WITH KEY LOW = 'MODAL03'.
      READ TABLE T_TVARVC INTO W_TVARV2 WITH KEY LOW = 'OUTROMODAL'.
*-#133089-21.02.2024-JT-fim

      LOOP AT CT_PARTNER ASSIGNING FIELD-SYMBOL(<FS_PARTENR>).
        IF CS_IDE-MODAL NE '03'.
          <FS_PARTENR>-X_NOME = COND #( WHEN W_TVARV2-HIGH IS INITIAL THEN 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'
                                                                      ELSE W_TVARV2-HIGH ). "*-#133089-21.02.2024-JT-inicio
*       <fs_partenr>-x_fant = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
*       <fs_partenr>-x_lgr  = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'.
        ELSE.
          <FS_PARTENR>-X_NOME = COND #( WHEN W_TVARV1-HIGH IS INITIAL THEN 'CTE EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'
                                                                      ELSE W_TVARV1-HIGH ). "*-#133089-21.02.2024-JT-inicio
        ENDIF.
      ENDLOOP.
    ENDIF.

*-------------------------------------
*-- REFORMA TRIBUTÁRIA --*
*-------------------------------------


* T (04) 000 - Tributação integral
* R (52) 200 - Alíquota reduzida
* N (22) 410 - Imunidade e não incidência
* D (02) 510 - Diferimento
* S (20) 550 - Suspensão
* B (02) 010 - Tributação com alíquotas uniformes
* G (03) 210 - Redução de alíquota com redutor de base de cálculo
* H (03) 220 - Alíquota fixa
* I (01) 221 - Alíquota fixa rateada
* J (01) 222 - Redução de Base de Cálculo
* M (06) 620 - Tributação Monofásica
* C (02) 800 - Transferência de crédito
* A (01) 810 - Ajustes
* K (06) 820 - Tributação em declaração de regime específico
* F (05) 011 - Tributação com alíquotas uniformes reduzidas
* I (01) 400 - Isenção
* E (01) 830 - Exclusão da Base de Cálculo

*    FIELD-SYMBOLS: <FS_NFLIN>           TYPE J_1BNFLIN.
*    ASSIGN ('(SAPLJ_1B_CTE)GT_NFLIN')          TO <FS_NFLIN>.
*
*      LOOP AT IT_NFLIN ASSIGNING FIELD-SYMBOL(<FS_NFLIN>).
*    IF <FS_NFLIN>-TAXSITUATION(1) EQ 'R'.
*
*
*      READ TABLE IT_NFSTX ASSIGNING FIELD-SYMBOL(<FS_IB3S>)
*        WITH KEY TAXTYP = 'IB3S'
*                 ITMNUM = <FS_NFLIN>-ITMNUM.
*
*      IF SY-SUBRC = 0.
*        <FS_NFLIN>-PIBSUF = <FS_IB3S>-RATE4DEC.
*      ENDIF.
*
*      READ TABLE IT_NFSTX ASSIGNING FIELD-SYMBOL(<FS_CBS3>)
*        WITH KEY TAXTYP = 'CBS3'
*                 ITMNUM = <FS_NFLIN>-ITMNUM.
*
*      IF SY-SUBRC = 0.
*        <FS_NFLIN>-PCBS = <FS_CBS3>-RATE4DEC.
*      ENDIF.
*
*    ENDIF.
    " ENDLOOP.

  ENDMETHOD.


  method IF_EX_CL_NFE_PRINT~FILL_EXPORT.

  "Internal Table and WorkArea
  DATA: LT_VBRK             TYPE TABLE OF VBRK,
        LW_VBRK             TYPE VBRK,
        LT_VBFA             TYPE TABLE OF VBFA,
        LW_VBFA             TYPE VBFA,
        LT_ZDOC_EXP         TYPE TABLE OF ZDOC_EXP,
        LW_ZDOC_EXP         TYPE ZDOC_EXP,
        LT_ZDOC_NF_PRODUTOR TYPE TABLE OF ZDOC_NF_PRODUTOR,
        LW_ZDOC_NF_PRODUTOR TYPE ZDOC_NF_PRODUTOR,
        WL_EXPORTACAO_IND   TYPE J_1BNFE_S_BADI_EXPORT,
        LW_CHAVE            TYPE C LENGTH 44.

  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.

  FIELD-SYMBOLS: <FS_XMLH>  TYPE J1B_NF_XML_HEADER.

  ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <FS_XMLH>.

  CHECK <FS_XMLH> IS ASSIGNED.

  CHECK ( IN_ITEM-CFOP(4) = '7501' ) OR ( IN_ITEM-CFOP(4) = '3503' ).

  CHECK ( <FS_XMLH>-FINNFE EQ '1' ).

  CLEAR: LT_VBRK[], LT_VBFA[], LT_ZDOC_EXP[], LT_ZDOC_NF_PRODUTOR[].

  "===========================================================================================
  " Notas de Complemento para Fim Especifico.
  "===========================================================================================
  SELECT *
    FROM VBRK
    INTO TABLE LT_VBRK
   WHERE VBELN EQ IN_ITEM-REFKEY(10).

  CHECK ( LT_VBRK[] IS NOT INITIAL ).

  SELECT *
    FROM VBFA INTO TABLE LT_VBFA
     FOR ALL ENTRIES IN LT_VBRK
   WHERE VBELN EQ LT_VBRK-VBELN
     AND VBTYP_N EQ 'M'
     AND VBTYP_V EQ 'J'.

  CHECK ( LT_VBFA[] IS NOT INITIAL ).

  SELECT *
    FROM ZDOC_EXP INTO TABLE LT_ZDOC_EXP
     FOR ALL ENTRIES IN LT_VBFA
  WHERE VBELN EQ LT_VBFA-VBELV.

  CHECK ( LT_ZDOC_EXP[] IS NOT INITIAL ).

  SELECT *
    FROM ZDOC_NF_PRODUTOR INTO TABLE LT_ZDOC_NF_PRODUTOR
     FOR ALL ENTRIES IN LT_ZDOC_EXP
   WHERE VBELN EQ LT_ZDOC_EXP-VBELN.

  CHECK ( LT_ZDOC_NF_PRODUTOR[] IS NOT INITIAL ).

  READ TABLE LT_VBRK INTO LW_VBRK INDEX 1.

  CHECK ( SY-SUBRC EQ 0 ).

  CASE LW_VBRK-FKART.

    WHEN: 'ZEXI'.

      READ TABLE LT_VBFA     INTO LW_VBFA     WITH KEY VBELN = LW_VBRK-VBELN.
      READ TABLE LT_ZDOC_EXP INTO LW_ZDOC_EXP WITH KEY VBELN = LW_VBFA-VBELV.

      IF ( SY-SUBRC EQ 0 ).

        CREATE OBJECT LOBJ_UTIL.

        LOOP AT LT_ZDOC_NF_PRODUTOR INTO LW_ZDOC_NF_PRODUTOR.

          CLEAR: WL_EXPORTACAO_IND, LW_CHAVE.

          SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE) WHERE DOCNUM EQ @LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD.
          CHECK SY-SUBRC EQ 0.

          REPLACE REGEX '[/]' IN LW_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.
          REPLACE REGEX '[-]' IN LW_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.

          IF LW_ZDOC_EXP-NR_REGISTRO_EXPO IS INITIAL.
            LW_ZDOC_EXP-NR_REGISTRO_EXPO = '000000000000'.
          ENDIF.

          "Check se é uma entrada Propria
          SELECT SINGLE * INTO @DATA(LW_ZLEST0147)
            FROM ZLEST0147 AS A
           WHERE A~DOCNUM          EQ @LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD
             AND A~ENTRADA_PROPRIA EQ @ABAP_TRUE
             AND EXISTS ( SELECT ID_RECEPCAO
                            FROM ZLEST0146 AS B
                           WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                             AND B~CANCEL      EQ @ABAP_FALSE ).

          IF ( SY-SUBRC EQ 0 ) AND ( LW_ZLEST0147-MODEL EQ '55' ).
            LW_CHAVE = LW_ZLEST0147-CHAVE_NFE.
          ELSE.
            CALL METHOD LOBJ_UTIL->MONTA_CHAVE_NFE
              EXPORTING
                I_DOCNUM   = LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD
                I_VALIDA   = 'X'
              RECEIVING
                E_CHAVE    = LW_CHAVE
              EXCEPTIONS
                ERRO_CHAVE = 1
                OTHERS     = 2.
          ENDIF.

          "Monta Exportação Indireta
          WL_EXPORTACAO_IND-NRE     = LW_ZDOC_EXP-NR_REGISTRO_EXPO.
          WL_EXPORTACAO_IND-CHNFE   = LW_CHAVE.
          WL_EXPORTACAO_IND-QEXPORT = LW_ZDOC_NF_PRODUTOR-MENGE.

          APPEND WL_EXPORTACAO_IND TO OUT_EXPORT.
        ENDLOOP.

      ENDIF.

  ENDCASE.


  endmethod.


  METHOD if_ex_cl_nfe_print~fill_fuel.

    DATA wa_fuel_origin TYPE zrsi_mono_fuel.
    DATA: ls_address TYPE sadr.


    CHECK out_fuel IS NOT INITIAL.

    LOOP AT in_item ASSIGNING FIELD-SYMBOL(<fs_item>).

      SELECT SINGLE p_orig p_bio
               FROM zrsi_mono_fuel
               INTO CORRESPONDING FIELDS OF wa_fuel_origin
               WHERE matnr EQ <fs_item>-matnr.

      CHECK wa_fuel_origin IS NOT INITIAL.
      READ TABLE OUT_FUEL  ASSIGNING FIELD-SYMBOL(<fs_out_fuel>) WITH KEY itmnum = <fs_item>-itmnum.
      APPEND INITIAL LINE TO OUT_FUEL_ORIGIN ASSIGNING FIELD-SYMBOL(<fs_fuel_origin>).



      <fs_out_fuel>-p_bio = wa_fuel_origin-p_bio.
      <fs_fuel_origin>-docnum = <fs_item>-docnum.
      <fs_fuel_origin>-itmnum = <fs_item>-itmnum.
      <fs_fuel_origin>-docnum = <fs_item>-matorg.
      <fs_fuel_origin>-ind_import = <fs_item>-matorg.
      <fs_fuel_origin>-p_orig = wa_fuel_origin-p_orig.

      CALL FUNCTION 'J_1B_BRANCH_READ'
        EXPORTING
          branch            = in_header-branch
          company           = in_header-bukrs
        IMPORTING
          address           = ls_address
        EXCEPTIONS
          branch_not_found  = 1
          address_not_found = 2
          company_not_found = 3
          OTHERS            = 4.

     " <fs_fuel_origin>-c_uf_origin = ls_address-txjcd+3(2).
      <fs_fuel_origin>-c_uf_origin = ls_address-regio.
       UNASSIGN <fs_out_fuel>.
       UNASSIGN <fs_fuel_origin>.
    ENDLOOP.


  ENDMETHOD.


METHOD if_ex_cl_nfe_print~fill_header.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 05/02/2025|DEVK9A1XAW |NSEGATIN       | Ajuste para Informar Dados NF PROD na mensagem da NF Saida.   *
*&                                       | Chamado 178644.                                               *
*--------------------------------------------------------------------------------------------------------*

  TYPES: BEGIN OF ty_icms_inter,
           vbcufdest      TYPE j_1btaxval, "
           pfcpufdest     TYPE j_1btaxval,
           picmsufdest    TYPE j_1btaxval,
           picmsinter     TYPE j_1btaxval,
           picmsinterpart TYPE j_1btaxval,
           vfcpufdest     TYPE j_1btaxval,
           vicmsufdest    TYPE j_1btaxval,
           vicmsufremet   TYPE j_1btaxval,
         END OF ty_icms_inter.

  DATA: tl_zsdt0004              TYPE TABLE OF zsdt0004,
        tl_zsdt0001              TYPE TABLE OF zsdt0001,
        tl_vbak                  TYPE TABLE OF znfe_vbak,
        tl_vbap                  TYPE TABLE OF znfe_vbap,
        tl_vbrp                  TYPE TABLE OF znfe_vbrp,
        tl_lips                  TYPE TABLE OF znfe_lips,
        tl_produtor              TYPE TABLE OF zdco_produtor,
        tl_nf_entrada            TYPE TABLE OF zdco_nf_entrada,
        tl_konv                  TYPE TABLE OF znfe_konv,
        tl_zsdt0006              TYPE TABLE OF zsdt0006,
        tl_zsdt0294              TYPE TABLE OF zsdt0294,
        tl_lin                   TYPE TABLE OF j_1bnflin,
        tl_1bnfstx               TYPE TABLE OF j_1bnfstx,
        tl_xvbadr                TYPE TABLE OF sadrvb,
        tl_xvbpa                 TYPE TABLE OF vbpavb,
        tl_lines                 TYPE TABLE OF tline,
        sl_lfa1                  TYPE znfe_lfa1,
        sl_t006a                 TYPE znfe_t006a,
        sl_lin                   TYPE j_1bnflin,
        tl_itm_xml_tab           TYPE j1b_nf_xml_item_tab,
        sl_itm_xml               TYPE j1b_nf_xml_item,
        sl_1bnfstx               TYPE j_1bnfstx,
        sl_zsdt0006              TYPE zsdt0006,
        sl_zsdt0001              TYPE zsdt0001,
        sl_zsdt0004              TYPE zsdt0004,
        sl_vbrp                  TYPE znfe_vbrp,
        sl_vbap                  TYPE znfe_vbap,
        sl_vbak                  TYPE znfe_vbak,
        sl_lips                  TYPE znfe_lips,
        sl_likp                  TYPE znfe_likp,
        sl_produtor              TYPE zdco_produtor,
        sl_nf_entrada            TYPE zdco_nf_entrada,
        sl_xvbpa                 TYPE vbpavb,
        sl_lfa1_ter              TYPE znfe_lfa1,
        sl_lfa1_tra              TYPE znfe_lfa1,
        sl_lfa1_pc               TYPE znfe_lfa1,
        sl_lfa1_parc             TYPE znfe_lfa1,
        sl_konv                  TYPE znfe_konv,
        sl_tline                 TYPE tline,
        sl_text                  TYPE j_1bnfftx,
        vl_field                 TYPE string,
        vl_textos                TYPE string,
        vl_lips                  TYPE string,
        vl_lips2                 TYPE string,
        vl_seguro                TYPE string,
        vl_exportacao            TYPE string,
        vl_aux                   TYPE string,
        vl_textos_pd             TYPE string,
        vl_textos_rec            TYPE string,
        vl_textos_z1             TYPE string,
        vl_textos_csoc           TYPE zmsg_cert,
        vl_textos_cl_prod        TYPE string,
        vl_textos_icms_inter     TYPE string,
        vl_textos_transf         TYPE string,
        vl_textos_veic           TYPE string,
        vl_textos_obs_filial     TYPE string,
        vl_textos_cent_descarte  TYPE string,
        vl_textos_loc_arm_prod   TYPE string,
        vl_textos_rot_entrega    TYPE string,
        vl_textos_parceiros      TYPE string,
        vl_textos_parceiros_init TYPE string,
        vl_textos_trib_mono      TYPE string,       "*-Equalização RISE x PRD - 28.08.2023 - JT
        vl_textos_icms_mono      TYPE c LENGTH 200, "*-Equalização RISE x PRD - 28.08.2023 - JT
        vl_textos_icms_mono_dif  TYPE c LENGTH 200, "*-Equalização RISE x PRD - 28.08.2023 - JT
        vl_icms_inter_calc       TYPE c,
        vl_nfenum                TYPE j_1bnfdoc-nfenum,
        vl_quant                 TYPE char10,
        vl_kbetr                 TYPE char16,
        vl_volum                 TYPE char19,
        vl_id	                   TYPE thead-tdid,
        vl_language              TYPE thead-tdspras,
        vl_name                  TYPE thead-tdname,
        vl_object                TYPE thead-tdobject,
        vl_fone                  TYPE adrc-tel_number,
        vl_adrnr                 TYPE kna1-adrnr,
        lva_lifnr_emissor        TYPE lfa1-lifnr,
        vl_cnpj                  TYPE char18,
        vl_rem_transf            TYPE vbfa-vbeln,
        sl_kna1                  TYPE znfe_kna1,
        zona_franca              TYPE c LENGTH 1,
        icms_desonerado          TYPE c LENGTH 1,
        icms_nao_tribut          TYPE c LENGTH 1,
        ck_lei_fiscal            TYPE c LENGTH 1,
        vl_base_n_tribut         TYPE j_1bnfstx-basered1,
        vl_aliq_n_tribut         TYPE j_1bnfstx-rate,
        vl_perc_n_tribut         TYPE p DECIMALS 6,
        var_rate                 TYPE j_1btxrate,
        wa_j_1bnflin             TYPE j_1bnflin,
        vl_cst_icms              TYPE c LENGTH 2,
        tx_kverm                 TYPE text30,
        valor_deconto            TYPE j_1bexcbas,
        wa_desconto              TYPE flag,             "<<RIM-SKM-IR139581 "*-Equalização RISE x PRD - 28.08.2023 - JT
        volumes                  TYPE j1b_nf_xml_t6,
        cobranca                 TYPE j1b_nf_xml_u1,
        o_importacao             TYPE char01,
        wa_zfiwrt0015            TYPE zfiwrt0015,
        wa_zjcnd_branch          TYPE zjcnd_branch,
        wa_j_1bbranch            TYPE j_1bbranch,
        wa_adrc                  TYPE adrc,
        wa_tpart                 TYPE tpart,
        vg_vtext                 TYPE vtxtk,
        wa_j_1bnfnad             TYPE j_1bnfnad,
        tl_vbrk                  TYPE TABLE OF vbrk,
        wa_vbrk                  TYPE vbrk,
        it_konv                  TYPE TABLE OF konv,
        wa_konv                  TYPE konv,
*---> s4 migration 10/07/2023 - MA
*        wa_konh                  TYPE konh,
*        wa_konh_ant              TYPE konh,
*        it_konh                  TYPE TABLE OF konh,

        wa_konh                  TYPE konh_kks,
        wa_konh_ant              TYPE konh_kks,
        it_konh                  TYPE TABLE OF konh_kks,
*<--- s4 migration 10/07/2023 - MA
        wl_icms_inter            TYPE ty_icms_inter,
        wa_zsdt0231              TYPE zsdt0231.

  DATA: vl_vbelv               TYPE vbfa-vbelv,
        wa_vbak                TYPE vbak,
        wa_lfa1                TYPE lfa1,
        wa_kna1                TYPE kna1,
        wa_vbap                TYPE vbap,
        wl_zsdt0134            TYPE zsdt0134,
        wl_zsdt0139            TYPE zsdt0139,
        wl_zsdt0140            TYPE zsdt0140,
        wl_zsdt0082            TYPE zsdt0082,
        wl_zsdt0284            TYPE zsdt0284,
        wl_zsdt0001_transf_arm TYPE zsdt0001,
        it_zmmt0065            TYPE TABLE OF zmmt0065,
        wa_zmmt0065            TYPE zmmt0065,
        it_zfiwrt0008          TYPE TABLE OF zfiwrt0008,
        wa_zfiwrt0008          TYPE zfiwrt0008,
        it_zfiwrt0015          TYPE TABLE OF zfiwrt0015,
        wa_lifnr               TYPE c LENGTH 10,
        ls_j_1binnad           TYPE j_1binnad.

  DATA: it_regio_deson TYPE TABLE OF regio,
        wa_regio_deson TYPE regio.

  DATA: lv_separador(3)  TYPE c VALUE ' / '.
  DATA: it_obs   TYPE TABLE OF zsdt0294.
  DATA: wa_zsdt0098 TYPE zsdt0098.
  DATA  v_cprod     TYPE char18.  "*-Equalização RISE x PRD - 28.08.2023 - JT
  DATA  lv_cprod    TYPE char18.

  DATA: wa_info_part TYPE lfa1,
        wa_info_c    TYPE kna1,
        v_lifnr      TYPE lfa1-lifnr.

  DATA: lv_ntgew           TYPE ntgew_15,
        lv_brgew           TYPE brgew_15,
        valor_d            TYPE p DECIMALS 0,
        vl_shpunt          TYPE string,
        valor_s            TYPE string,
        vl_umrez_s         TYPE string,
        vg_txjcd           TYPE lfa1-txjcd,
        it_znota_import_ii TYPE TABLE OF znota_import_ii,
        lva_preenche_pc    TYPE c,
*******Ponto de codigo de País para o DRC****** WUS****
        lv_e1_cpais        TYPE j1b_nf_xml_badi_header-e1_cpais.

  DATA: v_texto_cont_180 TYPE c LENGTH 180,
        it_text          TYPE j_1bnfftx_tab. "*-#137670-11.04.2024-JT-inicio

  FIELD-SYMBOLS:
    <fs_xmlh>      TYPE j1b_nf_xml_header,     " (SAPLJ_1B_NFE)XMLH
    <fs_xmlh_310>  TYPE j_1bnfe_s_layout_310,    "Utilizado para GRC
    <fs_xmlh_badi> TYPE j1b_nf_xml_badi_header.


*  DATA  v_cprod     TYPE char18.


  FIELD-SYMBOLS: <itens>         TYPE STANDARD TABLE,
                 <itens_xml_tab> TYPE j1b_nf_xml_item_tab,
                 <itens_xml>     TYPE j1b_nf_xml_item,
                 <item_tax>      TYPE STANDARD TABLE.

  CLEAR: out_add_info[], out_add_info.

  TYPES: ty_cst_not_deson TYPE RANGE OF char2.
  DATA: r_cst_not_deson TYPE ty_cst_not_deson.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '51' ) TO r_cst_not_deson.

  MOVE-CORRESPONDING in_xml_header TO out_header.

  "Documentos Referenciados.
  me->fill_docs_reference( i_xml_header = in_xml_header ).  "Utilizado para GRC
  "LP - 27/10/2023 o desenvolvimento de dados adicionais não parece ser mais necessário, visto que essa informação quando writer vim pela estrutura da OBJ_TEXTS da bapi : BAPI_J_1B_NF_CREATEFROMDATA
  "chegando neste ponto com sl_text-manual vázio, será mantido provisoriamente, reavaliar quando possivel

*-#137670-11.04.2024-JT-inicio
  it_text[] = in_text[].

  SORT it_text BY message docnum seqnum linnum.
  DELETE ADJACENT DUPLICATES FROM it_text
                        COMPARING message.
  SORT it_text BY docnum seqnum linnum.
*-#137670-11.04.2024-JT-fim

  IF in_doc-manual <> 'X'.
*   LOOP AT in_text INTO sl_text.
    LOOP AT it_text INTO sl_text.  "*-#137670-11.04.2024-JT-inicio
      IF sl_text-manual IS INITIAL
*** Stefanini - IR230643 - 15/04/2025 - LAZAROSR - Início de Alteração
      AND in_doc-nftype NE 'ZT'.
*** Stefanini - IR230643 - 15/04/2025 - LAZAROSR - Fim de Alteração
        IF out_header-infadfisco_v2 IS INITIAL.
          out_header-infadfisco_v2 = sl_text-message.
        ELSE.
          CONCATENATE out_header-infadfisco_v2 sl_text-message INTO out_header-infadfisco_v2 SEPARATED BY space.
        ENDIF.
      ELSE.
        IF out_header-infcomp IS INITIAL.
          out_header-infcomp = sl_text-message.
        ELSE.
          CONCATENATE out_header-infcomp sl_text-message INTO out_header-infcomp SEPARATED BY space.
        ENDIF.
      ENDIF.
      CLEAR sl_text.
    ENDLOOP.
  ENDIF.
  ""<< FIM LP - 2710/2023
  "TEXTO ADICIONAL NOTA FISCAL
  SELECT *
    FROM zmmt0065
    INTO TABLE it_zmmt0065
    WHERE docnum = in_doc-docnum
    ORDER BY seqnum.

  LOOP AT it_zmmt0065 INTO wa_zmmt0065.
    IF out_header-infcomp IS INITIAL.
      out_header-infcomp = wa_zmmt0065-message.
    ELSE.
      CONCATENATE out_header-infcomp wa_zmmt0065-message INTO out_header-infcomp SEPARATED BY space.
    ENDIF.
  ENDLOOP.

*** CS2020001298 - Inicio - CBRAND
  SELECT *
    FROM zfiwrt0008
    INTO TABLE it_zfiwrt0008
    WHERE docnum = in_doc-docnum.

  IF it_zfiwrt0008 IS NOT INITIAL.

    SELECT *
    FROM zfiwrt0015
      INTO TABLE it_zfiwrt0015
    FOR ALL ENTRIES IN it_zfiwrt0008
   WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto.

    READ TABLE it_zfiwrt0008 INTO DATA(lwa_zfiwrt0008_sel) INDEX 1.
    IF ( sy-subrc EQ 0 ) AND ( lwa_zfiwrt0008_sel-parvw IS NOT INITIAL ).
      DELETE it_zfiwrt0015 WHERE parvw = lwa_zfiwrt0008_sel-parvw.
    ENDIF.

    LOOP AT it_zfiwrt0015 INTO wa_zfiwrt0015.

      CHECK  wa_zfiwrt0015-parvw NE 'Z1'.

      CLEAR: wa_tpart.
      SELECT SINGLE * INTO wa_tpart
        FROM tpart
       WHERE spras EQ sy-langu
      AND parvw EQ wa_zfiwrt0015-parvw.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
        FROM j_1bad INTO @DATA(lwa_j_1bad)
       WHERE parvw EQ @wa_zfiwrt0015-parvw.

      CHECK sy-subrc EQ 0.

      DATA: lva_info_ie_parc TYPE string.

      CLEAR: lva_info_ie_parc.

      CASE lwa_j_1bad-partyp.
        WHEN 'V'.

          CALL METHOD me->select_lfa1
            EXPORTING
              p_lifnr = wa_zfiwrt0015-parid
            IMPORTING
              s_lfa1  = sl_lfa1_parc.

          CHECK sl_lfa1_parc IS NOT INITIAL.

          CLEAR: vl_cnpj.
          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              input  = sl_lfa1_parc-stcd1
            IMPORTING
              output = vl_cnpj.

          IF sl_lfa1_parc-stcd3 IS NOT INITIAL.
            CONCATENATE '- IE:' sl_lfa1_parc-stcd3 INTO lva_info_ie_parc SEPARATED BY space.
          ENDIF.

          CLEAR: vl_textos_parceiros_init.
          CONCATENATE wa_tpart-vtext ':' sl_lfa1_parc-name1
                      '- CNPJ/CPF:' vl_cnpj sl_lfa1_parc-stcd2
                      lva_info_ie_parc
                      '-' sl_lfa1_parc-stras '-' sl_lfa1_parc-pstlz '-' sl_lfa1_parc-ort01 '-' sl_lfa1_parc-regio
                 INTO vl_textos_parceiros_init SEPARATED BY space.

          IF vl_textos_parceiros IS NOT INITIAL.
            CONCATENATE vl_textos_parceiros vl_textos_parceiros_init INTO vl_textos_parceiros SEPARATED BY space.
          ELSE.
            vl_textos_parceiros = vl_textos_parceiros_init .
          ENDIF.

        WHEN 'C'.

          CALL METHOD me->select_kna1
            EXPORTING
              p_kunnr = wa_zfiwrt0015-parid
            IMPORTING
              s_kna1  = sl_kna1.

          CHECK sl_kna1 IS NOT INITIAL.

          CLEAR: vl_cnpj.
          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              input  = sl_kna1-stcd1
            IMPORTING
              output = vl_cnpj.

          IF sl_kna1-stcd3 IS NOT INITIAL.
            CONCATENATE '- IE:' sl_kna1-stcd3 INTO lva_info_ie_parc SEPARATED BY space.
          ENDIF.

          CLEAR: vl_textos_parceiros_init.
          CONCATENATE wa_tpart-vtext ':' sl_kna1-name1
                      '- CNPJ/CPF:' vl_cnpj sl_kna1-stcd2
                      lva_info_ie_parc
                      '-' sl_kna1-stras '-'  sl_kna1-pstlz '-' sl_kna1-ort01 '-' sl_kna1-regio
                 INTO vl_textos_parceiros_init SEPARATED BY space.

          IF vl_textos_parceiros IS NOT INITIAL.
            CONCATENATE vl_textos_parceiros vl_textos_parceiros_init INTO vl_textos_parceiros SEPARATED BY space.
          ELSE.
            vl_textos_parceiros = vl_textos_parceiros_init .
          ENDIF.

      ENDCASE.

    ENDLOOP.


  ENDIF.

  IF vl_textos_parceiros IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_parceiros INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_parceiros.
    ENDIF.
  ENDIF.

  "Informar o formato da impressão.
  out_header-tpimp = '2'. "CS2022000911 Ajustes emissão NFe / Anderson Oenning



*** CS2020001298 - Fim - CBRAND

*  IF NOT out_header-infadfisco IS INITIAL.
*    IF NOT out_header-infcomp IS INITIAL.
*      CONCATENATE out_header-infadfisco
*                  out_header-infcomp
*             INTO out_header-infcomp SEPARATED BY space.
*    ELSE.
*      out_header-infcomp = out_header-infadfisco.
*    ENDIF.
*    CLEAR out_header-infadfisco.
*  ENDIF.

  ASSIGN ('(SAPLJ_1B_NFE)XMLI')     TO <itens_xml>.
  ASSIGN ('(SAPLJ_1B_NFE)XMLI_TAB') TO <itens_xml_tab>.

  ASSIGN ('(SAPLJ_1B_NFE)XMLH_310') TO <fs_xmlh_310>.    "Utilizado para GRC
  ASSIGN ('(SAPLJ_1B_NFE)XMLH_BADI') TO <fs_xmlh_badi>.

  ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.

  IF <itens_xml> IS ASSIGNED.
    sl_itm_xml = <itens_xml>.
  ENDIF.

  IF <itens_xml_tab> IS ASSIGNED.

    LOOP AT <itens_xml_tab> ASSIGNING FIELD-SYMBOL(<fs_item>).
      <fs_item>-infadprod = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_item>-infadprod ) ) ).
      <fs_item>-xprod = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_item>-xprod ) ) ).
      <fs_item>-utrib = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_item>-utrib ) ) ).
      <fs_item>-ucom = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_item>-ucom ) ) ).

      "Ajuste de Valor Unitário Comercial e Tributário
      IF <fs_item>-qtrib_v20 GT 0.
        <fs_item>-vuntrib_v20 = <fs_item>-vprod / <fs_item>-qtrib_v20.
      ENDIF.

      IF <fs_item>-qtrib GT 0.
        <fs_item>-vuntrib     = <fs_item>-vprod / <fs_item>-qtrib.
      ENDIF.

      IF <fs_item>-qcom GT 0.
        <fs_item>-vuncom = <fs_item>-vprod / <fs_item>-qcom.
      ENDIF.

      IF <fs_item>-qcom_v20 GT 0.
        <fs_item>-vuncom_v20 = <fs_item>-vprod / <fs_item>-qcom_v20.
      ENDIF.
    ENDLOOP.

    tl_itm_xml_tab = <itens_xml_tab>.
  ENDIF.

  vl_field =  '(SAPLJ_1B_NFE)WK_ITEM[]'.
  ASSIGN (vl_field) TO <itens>.
  CLEAR vl_field.

  CHECK <itens> IS ASSIGNED.
  tl_lin[] = <itens>.
  READ TABLE tl_lin INTO sl_lin INDEX 1.

* Seleciona Dados Relevântes
  CALL METHOD me->seleciona_dados
    EXPORTING
      p_docnum     = in_doc-docnum
      p_itmnum     = sl_lin-itmnum
      p_refkey     = sl_lin-refkey
    IMPORTING
      p_nfenum     = vl_nfenum
      s_lfa1       = sl_lfa1
      s_t006a      = sl_t006a
      t_zsdt0004   = tl_zsdt0004
      t_zsdt0001   = tl_zsdt0001
      t_vbak       = tl_vbak
      t_vbap       = tl_vbap
      t_vbrp       = tl_vbrp
      t_lips       = tl_lips
      t_produtor   = tl_produtor
      t_nf_entrada = tl_nf_entrada
      t_zsdt0006   = tl_zsdt0006
      t_zsdt0294   = tl_zsdt0294
      s_likp       = sl_likp.

  DELETE tl_zsdt0294 WHERE inf_add_prod EQ abap_true. "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>

*   Número da Doc SAP
  IF NOT in_doc-docnum IS INITIAL.
    IF out_header-infcomp IS INITIAL.
      CONCATENATE 'Número do documento:' in_doc-docnum INTO out_header-infcomp.
    ELSE.
      CONCATENATE out_header-infcomp 'Número do documento:' INTO out_header-infcomp SEPARATED BY space.
      CONCATENATE out_header-infcomp in_doc-docnum INTO out_header-infcomp.
    ENDIF.
  ENDIF.

  IF NOT tl_zsdt0006[] IS INITIAL.

    READ TABLE tl_zsdt0006 INTO sl_zsdt0006 WITH KEY branch = in_doc-branch.
    IF ( sy-subrc NE 0 ).
      READ TABLE: tl_zsdt0006 INTO sl_zsdt0006 WITH KEY branch = space.
    ENDIF.

    READ TABLE: tl_vbrp       INTO sl_vbrp       INDEX 1,
                tl_vbak       INTO sl_vbak       INDEX 1,
                tl_vbap       INTO sl_vbap       INDEX 1,
*                tl_lips       INTO sl_lips       INDEX 1,
                tl_produtor   INTO sl_produtor   INDEX 1,
                tl_zsdt0001   INTO sl_zsdt0001   INDEX 1,
                tl_zsdt0004   INTO sl_zsdt0004   INDEX 1,
                tl_nf_entrada INTO sl_nf_entrada INDEX 1.

*   Safra
    IF NOT sl_zsdt0006-safra    IS INITIAL AND
       NOT sl_zsdt0001-nr_safra IS INITIAL.
      CONCATENATE 'Safra:'
                  sl_zsdt0001-nr_safra
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   Número da Ordem
    IF NOT sl_zsdt0006-num_ord IS INITIAL AND
       NOT sl_vbrp-aubel       IS INITIAL.
      IF vl_textos IS INITIAL.
        CONCATENATE 'Número da Ordem:' sl_vbrp-aubel INTO vl_textos.
      ELSE.
        CONCATENATE vl_textos 'Número da Ordem:' INTO vl_textos SEPARATED BY space.
        CONCATENATE vl_textos sl_vbrp-aubel INTO vl_textos.
      ENDIF.
    ENDIF.

*   Número da Remessa
    IF ( NOT sl_likp-vbeln IS INITIAL ) AND ( sl_zsdt0006-num_lote IS INITIAL ).
      IF vl_textos IS INITIAL.
        CONCATENATE 'Número do Fornecimento:' sl_likp-vbeln INTO vl_textos.
      ELSE.
        CONCATENATE vl_textos 'Número do Fornecimento:' INTO vl_textos SEPARATED BY space.
        CONCATENATE vl_textos sl_likp-vbeln INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

*   Número Leilão
    IF NOT sl_zsdt0006-num_leil IS INITIAL AND
       NOT sl_produtor-nu_aviso IS INITIAL.
      CONCATENATE vl_textos
                  'Número Leilão:'
                  sl_produtor-nu_aviso
                  sl_produtor-cd_tipo_leilao
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   DCO
    IF NOT sl_zsdt0006-dco    IS INITIAL AND
       NOT sl_produtor-nr_dco IS INITIAL.
      CONCATENATE 'DCO:'
                  sl_produtor-nr_dco(2)
                  '.'
                  sl_produtor-nr_dco+2(3)
                  '.'
                  sl_produtor-nr_dco+5(4)
                  '-'
                  sl_produtor-nr_dco+9(1)
             INTO vl_aux.
      CONCATENATE vl_textos
                  vl_aux
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   BL
    IF NOT sl_zsdt0006-bl    IS INITIAL AND
       NOT sl_zsdt0004-numbl IS INITIAL.
      CONCATENATE vl_textos
                  'BL:'
                  sl_zsdt0004-numbl
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   RE
    IF NOT sl_zsdt0006-re    IS INITIAL AND
       NOT sl_zsdt0004-regre IS INITIAL.
      CONCATENATE vl_textos
                  'RE:'
                  sl_zsdt0004-regre
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   DDE
    IF NOT sl_zsdt0006-dde   IS INITIAL AND
       NOT sl_zsdt0004-numde IS INITIAL.
      CONCATENATE vl_textos
                  'DDE:'
                  sl_zsdt0004-numde
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   NF COMPRA
    IF NOT sl_zsdt0006-nf_compra IS INITIAL AND
       NOT vl_nfenum             IS INITIAL.
      CONCATENATE vl_textos
                  'NF COMPRA:'
                  vl_nfenum
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   CNPJ
    IF NOT sl_zsdt0006-cnpj IS INITIAL AND
       NOT sl_lfa1-stcd2    IS INITIAL.
      CONCATENATE vl_textos
                  'CNPJ:'
                  sl_lfa1-stcd2
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   QUANTIDADE
    IF NOT sl_zsdt0006-quant     IS INITIAL AND
       NOT sl_nf_entrada-qt_nota IS INITIAL.
      vl_quant = sl_nf_entrada-qt_nota.
      CONDENSE vl_quant NO-GAPS.
      CONCATENATE vl_textos
                  'QUANTIDADE:'
                  vl_quant
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*    " Le Tabela da memória
*    IF NOT <TOTAL> IS ASSIGNED.
*      ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <TOTAL>.
*    ENDIF.

*   Dados do Terminal
    IF NOT sl_zsdt0006-dados_ter IS INITIAL.
      CALL METHOD me->function_partner
        EXPORTING
          p_vbeln  = sl_vbrp-vbeln
        IMPORTING
          t_vbpavb = tl_xvbpa
          t_sadrvb = tl_xvbadr.
      DELETE tl_xvbpa WHERE parvw NE 'Z1'.
      IF NOT tl_xvbpa[] IS INITIAL.
        READ TABLE tl_xvbpa INTO sl_xvbpa INDEX 1.
        CALL METHOD me->select_lfa1
          EXPORTING
            p_lifnr = sl_xvbpa-lifnr
          IMPORTING
            s_lfa1  = sl_lfa1_ter.
      ENDIF.
      IF NOT sl_lfa1_ter IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_lfa1_ter-stcd1
          IMPORTING
            output = vl_cnpj.
        CONCATENATE vl_textos
                    'Local de Entrega:'
                    sl_lfa1_ter-name1
                    '-'
                    vl_cnpj
                    '-'
                    sl_lfa1_ter-stcd3
                    '-'
                    sl_lfa1_ter-stras
                    '-'
                    sl_lfa1_ter-pstlz
                    '-'
                    sl_lfa1_ter-ort01
                    '-'
                    sl_lfa1_ter-regio
               INTO vl_textos SEPARATED BY space.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = sl_vbak-kunnr "Emissor da Ordem
          IMPORTING
            output = wa_lifnr.

        IF sl_lfa1_ter-lifnr NE wa_lifnr.
          CLEAR: wa_adrc.

          SELECT SINGLE * INTO wa_adrc
            FROM adrc
           WHERE addrnumber EQ sl_lfa1_ter-adrnr.

          out_header-entrega = 'X'.
* G02

          out_header-g_cnpj    = sl_lfa1_ter-stcd1.
          out_header-g_cpf     = sl_lfa1_ter-stcd2.
* G03
          out_header-g_xlgr    = wa_adrc-street.
* G04
          out_header-g_nro     = wa_adrc-house_num1.
* G05
          out_header-g_xcpl    = wa_adrc-house_num2.
* G06
          out_header-g_xbairro = wa_adrc-city2.
* G07
          out_header-g_cmun    = wa_adrc-taxjurcode+3.
* G08
          out_header-g_xmun    = wa_adrc-city1.
* G09
          out_header-g_uf      = wa_adrc-region.

        ENDIF.
      ENDIF.
    ENDIF.


*   Dados do Transbordo
    IF NOT sl_zsdt0006-dados_trans IS INITIAL.
      CALL METHOD me->function_partner
        EXPORTING
          p_vbeln  = sl_vbrp-vbeln
        IMPORTING
          t_vbpavb = tl_xvbpa
          t_sadrvb = tl_xvbadr.
      DELETE tl_xvbpa WHERE parvw NE 'LR'.
      IF NOT tl_xvbpa[] IS INITIAL.
        READ TABLE tl_xvbpa INTO sl_xvbpa INDEX 1.
        CALL METHOD me->select_kna1
          EXPORTING
            p_kunnr = sl_xvbpa-kunnr
          IMPORTING
            s_kna1  = sl_kna1.
      ENDIF.
      IF NOT sl_kna1 IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_kna1-stcd1
          IMPORTING
            output = vl_cnpj.
        CONCATENATE vl_textos
                    'Local de Transbordo:'
                    sl_kna1-name1
                    '-'
                    vl_cnpj
                    '-'
                    sl_kna1-stcd3
                    '-'
                    sl_kna1-stras
                    '-'
                    sl_kna1-pstlz
                    '-'
                    sl_kna1-ort01
                    '-'
                    sl_kna1-regio
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF NOT sl_zsdt0006-tx_fatura IS INITIAL.
*   Textos da Fatura
      REFRESH tl_lines.
      vl_id       = '0002'.
      vl_language = 'PT'.
      vl_object   = 'VBBK'.

      vl_name = sl_lin-refkey.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = vl_id
          language                = vl_language
          name                    = vl_name
          object                  = vl_object
        TABLES
          lines                   = tl_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF NOT tl_lines[] IS INITIAL.
        LOOP AT tl_lines INTO sl_tline.
          CONCATENATE vl_textos
                      sl_tline-tdline
                 INTO vl_textos SEPARATED BY space.
          CLEAR sl_tline.
        ENDLOOP.
      ENDIF.
    ENDIF.

*   FACS
    IF NOT sl_zsdt0006-facs IS INITIAL.
      CALL METHOD me->select_konv
        EXPORTING
          p_knumv = sl_vbak-knumv
          p_kposn = sl_vbap-posnr
          p_kschl = 'ZFAC'
        IMPORTING
          t_konv  = tl_konv.
      READ TABLE tl_konv INTO sl_konv INDEX 1.
      IF sy-subrc      IS INITIAL AND NOT
         sl_konv-kbetr IS INITIAL.
        vl_kbetr = sl_konv-kbetr.
        CONDENSE vl_kbetr NO-GAPS.
        CONCATENATE vl_textos
                    'FACS:'
                    vl_kbetr
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

*   FETHAB
    IF NOT sl_zsdt0006-facs IS INITIAL.
      CALL METHOD me->select_konv
        EXPORTING
          p_knumv = sl_vbak-knumv
          p_kposn = sl_vbap-posnr
          p_kschl = 'ZFES'
        IMPORTING
          t_konv  = tl_konv.
      READ TABLE tl_konv INTO sl_konv INDEX 1.
      IF sy-subrc      IS INITIAL AND NOT
         sl_konv-kbetr IS INITIAL.
        vl_kbetr = sl_konv-kbetr.
        CONDENSE vl_kbetr NO-GAPS.
        CONCATENATE vl_textos
                    'FETHAB SOJA:'
                    vl_kbetr
               INTO vl_textos SEPARATED BY space.
      ENDIF.
      CALL METHOD me->select_konv
        EXPORTING
          p_knumv = sl_vbak-knumv
          p_kposn = sl_vbap-posnr
          p_kschl = 'ZFEA'
        IMPORTING
          t_konv  = tl_konv.
      READ TABLE tl_konv INTO sl_konv INDEX 1.
      IF sy-subrc      IS INITIAL AND NOT
         sl_konv-kbetr IS INITIAL.
        vl_kbetr = sl_konv-kbetr.
        CONDENSE vl_kbetr NO-GAPS.
        CONCATENATE vl_textos
                    'FETHAB ALGODÃO:'
                    vl_kbetr
               INTO vl_textos SEPARATED BY space.
      ENDIF.
      CALL METHOD me->select_konv
        EXPORTING
          p_knumv = sl_vbak-knumv
          p_kposn = sl_vbap-posnr
          p_kschl = 'ZFEG'
        IMPORTING
          t_konv  = tl_konv.
      READ TABLE tl_konv INTO sl_konv INDEX 1.
      IF sy-subrc      IS INITIAL AND NOT
         sl_konv-kbetr IS INITIAL.
        vl_kbetr = sl_konv-kbetr.
        CONDENSE vl_kbetr NO-GAPS.
        CONCATENATE vl_textos
                    'FETHAB GADO:'
                    vl_kbetr
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

*   PLACA CAVALO
*    IF NOT sl_zsdt0001-placa_cav IS INITIAL.
*      CONCATENATE 'PLACA CAVALO:'
*                  sl_zsdt0001-placa_cav
*             INTO vl_field SEPARATED BY space.
*      CONDENSE vl_field NO-GAPS.
*      CONCATENATE vl_textos
*                  vl_field
*             INTO vl_textos SEPARATED BY space.
*    ENDIF.

*   PLACA DO VEICULO
    IF sl_zsdt0006-placa_cav IS NOT INITIAL AND
       sl_zsdt0001-placa_cav IS NOT INITIAL.
      CONCATENATE 'PLACA DO VEICULO:'
                  sl_zsdt0001-placa_cav
             INTO vl_field SEPARATED BY space.
      CONDENSE vl_field NO-GAPS.
      CONCATENATE vl_textos
                  vl_field
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   PLACA DA CARRETA 1
    IF NOT sl_zsdt0006-placa_1    IS INITIAL AND
       NOT sl_zsdt0001-placa_car1 IS INITIAL.
      CONCATENATE 'PLACA DA CARRETA(1):'
                  sl_zsdt0001-placa_car1
             INTO vl_field SEPARATED BY space.
      CONDENSE vl_field NO-GAPS.
      CONCATENATE vl_textos
                  vl_field
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   PLACA DA CARRETA 2
    IF NOT sl_zsdt0006-placa_2    IS INITIAL AND
       NOT sl_zsdt0001-placa_car2 IS INITIAL.
      CONCATENATE 'PLACA DA CARRETA(2):'
                  sl_zsdt0001-placa_car2
             INTO vl_field SEPARATED BY space.
      CONDENSE vl_field NO-GAPS.
      CONCATENATE vl_textos
                  vl_field
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   PLACA DA CARRETA 3
    IF NOT sl_zsdt0006-placa_3    IS INITIAL AND
       NOT sl_zsdt0001-placa_car3 IS INITIAL.
      CONCATENATE 'PLACA DA CARRETA(3):'
                  sl_zsdt0001-placa_car3
             INTO vl_field SEPARATED BY space.
      CONDENSE vl_field NO-GAPS.
      CONCATENATE vl_textos
                  vl_field
             INTO vl_textos SEPARATED BY space.
    ENDIF.

*   QUANTIDADE DE FARDOS
    IF NOT sl_zsdt0006-qtd_far IS INITIAL.
      IF NOT sl_lips-volum IS INITIAL.
        vl_volum = sl_lips-volum.
        CONDENSE vl_volum NO-GAPS.
        CONCATENATE vl_textos
                    'QUANTIDADE DE FARDOS:'
                    vl_volum
                    sl_lips-voleh
                    sl_t006a-msehl
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

*   Ponto de Coleta
    CALL METHOD me->function_partner
      EXPORTING
        p_vbeln  = sl_vbrp-vbeln
      IMPORTING
        t_vbpavb = tl_xvbpa
        t_sadrvb = tl_xvbadr.
    DELETE tl_xvbpa WHERE parvw NE 'PC'.
    IF NOT tl_xvbpa[] IS INITIAL.
      READ TABLE tl_xvbpa INTO sl_xvbpa INDEX 1.
      CALL METHOD me->select_lfa1
        EXPORTING
          p_lifnr = sl_xvbpa-lifnr
        IMPORTING
          s_lfa1  = sl_lfa1_pc.
    ENDIF.
    IF NOT sl_lfa1_pc IS INITIAL.

*        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*          EXPORTING
*            INPUT  = SL_LFA1_PC-STCD1
*          IMPORTING
*            OUTPUT = VL_CNPJ.

      IF sl_lfa1_pc-stcd1 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_PBRCF_OUTPUT'
          EXPORTING
            input  = sl_lfa1_pc-stcd1
          IMPORTING
            output = vl_cnpj.
      ELSEIF sl_lfa1_pc-stcd2 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_PBRCF_OUTPUT'
          EXPORTING
            input  = sl_lfa1_pc-stcd2
          IMPORTING
            output = vl_cnpj.
      ENDIF.

      "Inicio CS2022000908 Ajuste mensagem da NF, parceiro PONTO DE COLETA / Anderson Oenning
      CLEAR: wa_tpart.
      SELECT SINGLE * INTO wa_tpart
      FROM tpart
      WHERE spras EQ sy-langu
      AND parvw EQ 'PC'.

      CLEAR: vg_vtext.
      IF wa_tpart IS NOT INITIAL.
        vg_vtext = wa_tpart-vtext.
      ELSE.
        vg_vtext = 'PC'.
      ENDIF.


      CONCATENATE vl_textos
                  vg_vtext
                  sl_lfa1_pc-name1
                  '-'
                  vl_cnpj
                  '-'
                  sl_lfa1_pc-stcd3
                  '-'
                  sl_lfa1_pc-stras
                  '-'
                  sl_lfa1_pc-pstlz
                  '-'
                  sl_lfa1_pc-ort02
                  '-'
                  sl_lfa1_pc-ort01
                  '-'
                  sl_lfa1_pc-regio
             INTO vl_textos SEPARATED BY space.

      "Fim CS2022000908 Ajuste mensagem da NF, parceiro PONTO DE COLETA / Anderson Oenning

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_vbap-werks
        IMPORTING
          output = wa_lifnr.
      "=========================================================================================CS2021000679  - AOENNING.
      "Verificar se OV esta cadastrada na tabela ZSDT0284, conforme novo processo CS2021000679  - AOENNING.
      CLEAR: wl_zsdt0284, lva_preenche_pc.
      SELECT SINGLE * FROM zsdt0284 INTO wl_zsdt0284 WHERE vbeln EQ sl_vbap-vbeln.
      IF sy-subrc EQ 0.
        lva_preenche_pc = abap_true.
      ENDIF.
      "=========================================================================================CS2021000679  - AOENNING.

      "Alterar dados Retirada no XML se parc Ponto Coleta <> Centro do item
      IF sl_lfa1_pc-lifnr NE wa_lifnr OR lva_preenche_pc EQ abap_true. "CS2021000679  - AOENNING.
        CLEAR: wa_adrc.

        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber EQ sl_lfa1_pc-adrnr.

        out_header-f_retirada = 'X'.
* F02
        out_header-f_cnpj    = sl_lfa1_pc-stcd1.
        out_header-f_cpf     = sl_lfa1_pc-stcd2.
* F03
        out_header-f_xlgr    = wa_adrc-street.
* F04
        out_header-f_nro     = wa_adrc-house_num1.
* F05
        out_header-f_xcpl    = wa_adrc-house_num2.
* F06
        out_header-f_xbairro = wa_adrc-city2.
* F07
        out_header-f_cmun    = wa_adrc-taxjurcode+3.
* F08
        out_header-f_xmun    = wa_adrc-city1.
* F09
        out_header-f_uf      = wa_adrc-region.

      ENDIF.
      "Fim Retirada XML

    ENDIF.

*   NUMERO DO LOTE

*    IF NOT sl_zsdt0006-num_lote IS INITIAL AND
*       NOT sl_lips-charg        IS INITIAL.

    IF NOT sl_zsdt0006-num_lote IS INITIAL.

      CLEAR: vl_lips.

      LOOP AT tl_lips INTO sl_lips.

        IF vl_lips IS INITIAL.
          CONCATENATE 'Número do Fornecimento:' sl_lips-vbeln '-' sl_lips-posnr INTO vl_lips.
        ELSE.
          CONCATENATE 'Número do Fornecimento:' sl_lips-vbeln '-' sl_lips-posnr INTO vl_lips2.
          CONCATENATE vl_lips vl_lips2 INTO vl_lips SEPARATED BY space.
        ENDIF.

        CONCATENATE vl_lips 'NUMERO DO LOTE:' sl_lips-charg INTO vl_lips SEPARATED BY space.

        " Classificação Material
        CLEAR: vl_lips2.

        CALL METHOD me->function_classification
          EXPORTING
            p_matnr  = sl_lips-matnr
            p_charg  = sl_lips-charg
          IMPORTING
            p_textos = vl_lips2.

        IF vl_lips2 IS NOT INITIAL.
          CONCATENATE vl_lips vl_lips2 INTO vl_lips SEPARATED BY space.
        ENDIF.

      ENDLOOP.

      IF vl_lips IS NOT INITIAL.
        IF vl_textos IS INITIAL.
          vl_textos = vl_lips.
        ELSE.
          CONCATENATE vl_textos vl_lips INTO vl_textos SEPARATED BY space.
        ENDIF.
      ENDIF.

    ENDIF.

*   LOCAL DE RETIRADA
    CALL METHOD me->function_partner
      EXPORTING
        p_vbeln  = sl_vbrp-vbeln
      IMPORTING
        t_vbpavb = tl_xvbpa
        t_sadrvb = tl_xvbadr.

    DELETE tl_xvbpa WHERE parvw NE 'PC'.
    IF NOT tl_xvbpa[] IS INITIAL.
      READ TABLE tl_xvbpa INTO sl_xvbpa INDEX 1.
      CALL METHOD me->select_lfa1
        EXPORTING
          p_lifnr = sl_xvbpa-lifnr
        IMPORTING
          s_lfa1  = sl_lfa1_ter.
    ENDIF.

    CALL METHOD me->function_partner
      EXPORTING
        p_vbeln  = sl_vbrp-vbeln
      IMPORTING
        t_vbpavb = tl_xvbpa
        t_sadrvb = tl_xvbadr.

    DELETE tl_xvbpa WHERE parvw NE 'SP'.
    IF NOT tl_xvbpa[] IS INITIAL.
      READ TABLE tl_xvbpa INTO sl_xvbpa INDEX 1.
      CALL METHOD me->select_lfa1
        EXPORTING
          p_lifnr = sl_xvbpa-lifnr
        IMPORTING
          s_lfa1  = sl_lfa1_tra.
    ENDIF.

    IF NOT sl_lfa1_ter IS INITIAL AND
       NOT sl_lfa1_tra IS INITIAL.
      IF sl_lfa1_ter-lifnr NE sl_lfa1_tra-lifnr.
        CONCATENATE vl_textos
                    'LOCAL DE RETIRADA:'
                    sl_lips-charg
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

*   Doc. Faturamento
    IF NOT sl_lin-refkey IS INITIAL.
      CONCATENATE vl_textos
                  'Doc. Faturamento:'
             INTO vl_textos SEPARATED BY space.

      CONCATENATE vl_textos
                  sl_lin-refkey
             INTO vl_textos.
    ENDIF.

    IF NOT sl_zsdt0006-tx_ov IS INITIAL.
*   TEXTOS DE ORDENS DE VENDA.
      REFRESH tl_lines.
      vl_id       = '0001'.
      vl_language = 'PT'.
      vl_name     = sl_vbak-vbeln.
      vl_object   = 'VBBK'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = vl_id
          language                = vl_language
          name                    = vl_name
          object                  = vl_object
        TABLES
          lines                   = tl_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF NOT tl_lines[] IS INITIAL.
        LOOP AT tl_lines INTO sl_tline.
          CONCATENATE vl_textos
                      sl_tline-tdline
                 INTO vl_textos SEPARATED BY space.
          CLEAR sl_tline.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF NOT sl_zsdt0006-tx_item_ov IS INITIAL.
*   TEXTOS DE ITENS ORDENS DE VENDA.
      REFRESH tl_lines.
      vl_id       = '0001'.
      vl_language = 'PT'.
      vl_object   = 'VBBP'.
      CONCATENATE sl_vbak-vbeln
                  sl_vbap-posnr
             INTO vl_name.

      REFRESH tl_lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = vl_id
          language                = vl_language
          name                    = vl_name
          object                  = vl_object
        TABLES
          lines                   = tl_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF NOT tl_lines[] IS INITIAL.
        LOOP AT tl_lines INTO sl_tline.
          CONCATENATE vl_textos
                      sl_tline-tdline
                 INTO vl_textos SEPARATED BY space.
          CLEAR sl_tline.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF NOT sl_zsdt0006-tx_material IS INITIAL.
*   TEXTOS DO MATERIAL.
      REFRESH tl_lines.

      CALL METHOD zcl_pp_services=>get_text_sd
        EXPORTING
          matnr = sl_vbap-matnr   " Nº do material
          werks = sl_vbap-werks   " Centro
        IMPORTING
          text  = tl_lines.       " Categoria de tabela para estrutura Tline

*      vl_id       = '0001'.
*      vl_language = 'PT'.
*      vl_object   = 'MVKE'.
*      CONCATENATE sl_lips-matnr
*                  sl_likp-vkorg
*                  sl_lips-vtweg
*             INTO vl_name.
*
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          id                      = vl_id
*          language                = vl_language
*          name                    = vl_name
*          object                  = vl_object
*        TABLES
*          lines                   = tl_lines
*        EXCEPTIONS
*          id                      = 1
*          language                = 2
*          name                    = 3
*          not_found               = 4
*          object                  = 5
*          reference_check         = 6
*          wrong_access_to_archive = 7
*          OTHERS                  = 8.

      IF NOT tl_lines[] IS INITIAL.
        LOOP AT tl_lines INTO sl_tline.
          CONCATENATE vl_textos
                      sl_tline-tdline
                 INTO vl_textos SEPARATED BY space.
          CLEAR sl_tline.
        ENDLOOP.
      ENDIF.

    ENDIF.

*   Observação do Romaneio
    IF sl_zsdt0006-tx_obs_rom IS NOT INITIAL AND sl_zsdt0001-ds_obs IS NOT INITIAL.
      CONCATENATE 'Observação Romaneio:'
                  sl_zsdt0001-ds_obs
             INTO vl_textos SEPARATED BY space.
    ENDIF.

    IF out_header-infcomp IS INITIAL.
      out_header-infcomp = vl_textos.
    ELSE.
      CONCATENATE out_header-infcomp vl_textos INTO out_header-infcomp SEPARATED BY space.
    ENDIF.
    CLEAR: vl_textos.
  ELSE.

    SELECT SINGLE * INTO wa_j_1bnfnad
      FROM j_1bnfnad
     WHERE docnum EQ in_doc-docnum
       AND parvw  EQ 'Z1'.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE lifnr name1 stcd1 stcd2 stcd3
                    stras pstlz ort01 ort02 regio
        FROM lfa1
        INTO sl_lfa1_ter
       WHERE lifnr EQ wa_j_1bnfnad-parid.

      IF NOT sl_lfa1_ter IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_lfa1_ter-stcd1
          IMPORTING
            output = vl_cnpj.
        CONCATENATE vl_textos
                    'Terminal de Entrega:'
                    sl_lfa1_ter-name1
                    '-'
                    vl_cnpj
                    '-'
                    sl_lfa1_ter-stcd3
                    '-'
                    sl_lfa1_ter-stras
                    '-'
                    sl_lfa1_ter-pstlz
                    '-'
                    sl_lfa1_ter-ort01
                    '-'
                    sl_lfa1_ter-regio
               INTO vl_textos SEPARATED BY space.
      ENDIF.
    ENDIF.

    CASE sl_lin-reftyp.
      WHEN 'MD'.

        SELECT SINGLE *
          FROM ekbe INTO @DATA(wl_ekbe_tmp)
         WHERE belnr EQ @sl_lin-refkey(10).

        IF ( sy-subrc EQ 0 ) AND ( sl_lin-refkey IS NOT INITIAL ).

          SELECT SINGLE *
            FROM ekko INTO @DATA(wl_ekko_tmp)
           WHERE ebeln = @wl_ekbe_tmp-ebeln.

          IF ( sy-subrc EQ 0 ) AND ( wl_ekko_tmp-bsart EQ 'ZUB' ).

            REFRESH tl_lines.
            vl_id       = 'F02'.
            vl_language = 'PT'.
            vl_object   = 'EKKO'.
            vl_name     = wl_ekko_tmp-ebeln.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = vl_id
                language                = vl_language
                name                    = vl_name
                object                  = vl_object
              TABLES
                lines                   = tl_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

            IF tl_lines[] IS NOT INITIAL.
              LOOP AT tl_lines INTO sl_tline.
                CONCATENATE vl_textos sl_tline-tdline
                       INTO vl_textos SEPARATED BY space.
              ENDLOOP.
            ENDIF.

            SELECT SINGLE *
              FROM ekpa INTO @DATA(wl_ekpa_tmp)
             WHERE ebeln = @wl_ekko_tmp-ebeln
               AND parvw = 'ZT'.

            IF sy-subrc EQ 0.

              SELECT SINGLE *
                FROM j_1bnfnad INTO @DATA(wl_j_1bnfnad_tmp)
               WHERE docnum EQ @in_doc-docnum
                 AND parvw  EQ 'LR'.

              IF sy-subrc EQ 0.

                CALL METHOD me->select_kna1
                  EXPORTING
                    p_kunnr = CONV #( wl_j_1bnfnad_tmp-parid )
                  IMPORTING
                    s_kna1  = sl_kna1.

                IF sl_kna1 IS NOT INITIAL.

                  CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
                    EXPORTING
                      input  = sl_kna1-stcd1
                    IMPORTING
                      output = vl_cnpj.

                  CONCATENATE vl_textos
                              'Local de Transbordo:'
                              sl_kna1-name1
                              '-'
                              vl_cnpj
                              '-'
                              sl_kna1-stcd3
                              '-'
                              sl_kna1-stras
                              '-'
                              sl_kna1-pstlz
                              '-'
                              sl_kna1-ort01
                              '-'
                              sl_kna1-regio
                         INTO vl_textos SEPARATED BY space.
                ENDIF.

              ENDIF. " IF SY-SUBRC EQ 0. PARCEIRO LR NO DOCUMENTO

            ENDIF. "IF SY-SUBRC EQ 0. PARCEIRO ZT NO PEDIDO

          ENDIF. " IF ( SY-SUBRC EQ 0 ) AND ( WL_EKKO_TMP-BSART EQ 'ZUB' ).

        ENDIF. "IF ( SY-SUBRC EQ 0 ) AND ( SL_LIN-REFKEY IS NOT INITIAL ).

    ENDCASE. " CASE SL_LIN-REFTYP.

  ENDIF.

  " Observações do Romaneio de Algodão - Ini (Comentario Temp)
*  READ TABLE TL_ZSDT0001 INTO SL_ZSDT0001 INDEX 1.
*
*  IF SL_ZSDT0001-ID_CARGA IS NOT INITIAL.
*
*    SELECT SINGLE * INTO @DATA(SL_ZSDT0001CG)
*      FROM ZSDT0001CG
*     WHERE ID_CARGA         EQ @SL_ZSDT0001-ID_CARGA
*       AND TP_PRODUTO_CARGA EQ @ZIF_CARGA=>ST_TP_PRODUTO_CARGA_ALGODAO.
*
*    IF SL_ZSDT0006-TX_FATURA IS INITIAL AND SY-SUBRC IS INITIAL.
*
*      REFRESH TL_LINES.
*      VL_ID       = '0002'.
*      VL_LANGUAGE = 'PT'.
*      VL_OBJECT   = 'VBBK'.
*
*      VL_NAME = SL_LIN-REFKEY.
*
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          ID                      = VL_ID
*          LANGUAGE                = VL_LANGUAGE
*          NAME                    = VL_NAME
*          OBJECT                  = VL_OBJECT
*        TABLES
*          LINES                   = TL_LINES
*        EXCEPTIONS
*          ID                      = 1
*          LANGUAGE                = 2
*          NAME                    = 3
*          NOT_FOUND               = 4
*          OBJECT                  = 5
*          REFERENCE_CHECK         = 6
*          WRONG_ACCESS_TO_ARCHIVE = 7
*          OTHERS                  = 8.
*
*      IF NOT TL_LINES[] IS INITIAL.
*        LOOP AT TL_LINES INTO SL_TLINE.
*          CONCATENATE VL_TEXTOS
*                      SL_TLINE-TDLINE
*                 INTO VL_TEXTOS SEPARATED BY SPACE.
*          CLEAR SL_TLINE.
*        ENDLOOP.
*      ENDIF.
*
*    ENDIF.
*
*    IF SL_ZSDT0001CG IS NOT INITIAL.
*      SELECT * INTO TABLE @DATA(IT_ZSDT0001_ITEM_FD)
*        FROM ZSDT0001_ITEM_FD
*       WHERE CH_REFERENCIA EQ @SL_ZSDT0001-CH_REFERENCIA.
*
*      LOOP AT IT_ZSDT0001_ITEM_FD INTO DATA(WA_ZSDT0001_ITEM_FD).
*        DATA(LC_TEXTO_BLOCO_) = |Bloco: { WA_ZSDT0001_ITEM_FD-NM_BLOCO } Qtd. Fardos: { WA_ZSDT0001_ITEM_FD-QT_FARDOS } |.
*        CONCATENATE VL_TEXTOS LC_TEXTO_BLOCO_ INTO VL_TEXTOS SEPARATED BY SPACE.
*      ENDLOOP.
*
*    ENDIF.
*    CLEAR: SL_ZSDT0001CG.
*
*  ENDIF.
  " Observações do Romaneio de Algodão - Fim

* Número Telefone

  CLEAR out_header-c1_fone.

  lva_lifnr_emissor = in_doc-branch.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lva_lifnr_emissor
    IMPORTING
      output = lva_lifnr_emissor.

  SELECT SINGLE adrnr
    FROM lfa1 INTO vl_adrnr
  WHERE lifnr EQ lva_lifnr_emissor
    AND ktokk EQ 'ZFIC'.

  IF ( sy-subrc EQ 0 ) AND ( lva_lifnr_emissor IS NOT INITIAL ) AND ( vl_adrnr IS NOT INITIAL ).
    SELECT SINGLE tel_number
      FROM adrc
      INTO vl_fone
    WHERE addrnumber EQ vl_adrnr.

    IF ( sy-subrc EQ 0 ) AND ( vl_fone IS NOT INITIAL ).
      out_header-c1_fone = vl_fone.
    ENDIF.
  ENDIF.


** Classificação Material
*  CALL METHOD me->function_classification
*    EXPORTING
*      p_matnr  = sl_lin-matnr
*      p_charg  = sl_lin-charg
*    IMPORTING
*      p_textos = vl_textos.

  CLEAR: vl_seguro.

  CALL METHOD me->select_seguro
    EXPORTING
      p_docnum     = in_doc-docnum
      p_waerk      = in_doc-waerk
      p_data       = in_doc-docdat
      p_itmnum     = sl_lin-itmnum
      p_refkey     = sl_lin-refkey
      p_c1_uf      = in_xml_header-c1_uf
    IMPORTING
      e_observacao = vl_seguro.

  CLEAR: volumes.

  CALL METHOD me->select_exportacao
    EXPORTING
      in_doc       = in_doc
      in_partner   = tl_xvbpa
    IMPORTING
      e_observacao = vl_exportacao.
*     e_volume     = volumes.  "*-CS2021001056-#68233-07.06.2022-JT-inicio

  CALL METHOD me->fill_doc_volumes
    EXPORTING
      in_doc     = in_doc
      in_partner = tl_xvbpa
    IMPORTING
      e_volume   = volumes.

  IF volumes IS NOT INITIAL.
    APPEND volumes TO out_volume.
  ENDIF.

  "Volumes - Ajustes GRC Ini
*  IF out_volume[] IS INITIAL.
*
*    CLEAR: volumes, lv_ntgew, lv_brgew, valor_s, valor_d.
*
*    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*      EXPORTING
*        input                = in_doc-ntgew
*        unit_in              = in_doc-gewei
*        unit_out             = 'KG'
*      IMPORTING
*        output               = lv_ntgew
*      EXCEPTIONS
*        conversion_not_found = 1
*        division_by_zero     = 2
*        input_invalid        = 3
*        output_invalid       = 4
*        overflow             = 5
*        type_invalid         = 6
*        units_missing        = 7
*        unit_in_not_found    = 8
*        unit_out_not_found   = 9
*        OTHERS               = 10.
*    IF sy-subrc EQ 0.
*      volumes-t4_pesol = lv_ntgew.
*    ENDIF.
*
*    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*      EXPORTING
*        input                = in_doc-brgew
*        unit_in              = in_doc-gewei
*        unit_out             = 'KG'
*      IMPORTING
*        output               = lv_brgew
*      EXCEPTIONS
*        conversion_not_found = 1
*        division_by_zero     = 2
*        input_invalid        = 3
*        output_invalid       = 4
*        overflow             = 5
*        type_invalid         = 6
*        units_missing        = 7
*        unit_in_not_found    = 8
*        unit_out_not_found   = 9
*        OTHERS               = 10.
*
*    IF sy-subrc EQ 0.
*      volumes-t4_pesob = lv_brgew.
*    ENDIF.
*
*    IF ( in_doc-anzpk IS NOT INITIAL ).
*
*      volumes-t4_qvol = in_doc-anzpk.
*      volumes-t4_esp  = ''.
*      SELECT SINGLE mseh6 INTO @volumes-t4_esp
*        FROM t006a
*       WHERE msehi EQ @in_doc-shpunt
*         AND spras EQ 'P'.
*
*      volumes-t4_esp = zcl_string=>tira_acentos( zcl_string=>upper( i_str = CONV #( volumes-t4_esp ) ) ).
*
*    ELSEIF ( in_doc-brgew IS NOT INITIAL ).
*
*      valor_d = lv_brgew.
*      valor_s = valor_d.
*
*      REPLACE '.' IN valor_s WITH ' '.
*
*      volumes-t4_qvol = valor_s.
*
*    ENDIF.
*
*    IF ( volumes-t4_pesob IS NOT INITIAL ) AND
*       ( volumes-t4_pesol IS NOT INITIAL ).
*      APPEND volumes TO out_volume.
*    ENDIF.
*
*  ENDIF.
  "Ajustes GRC Fim

  CLEAR: cobranca.

  CALL METHOD me->block_u
    EXPORTING
      in_doc     = in_doc
    IMPORTING
      e_cobranca = cobranca.

  IF cobranca IS NOT INITIAL.
    out_header-cobr = 'X'.
    MOVE-CORRESPONDING cobranca TO out_header.
  ENDIF.

  CLEAR: valor_deconto.

  SELECT * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ in_doc-docnum.

    CLEAR: vl_cst_icms.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1bnflin-taxsit
      IMPORTING
        output = vl_cst_icms.

    SELECT SINGLE *
      FROM zsdt0098 INTO wa_zsdt0098
     WHERE taxlw1 EQ wa_j_1bnflin-taxlw1
       AND cfop   EQ wa_j_1bnflin-cfop(4).

    IF sy-subrc NE 0.
      SELECT SINGLE * FROM zsdt0098 INTO wa_zsdt0098 WHERE taxlw1 EQ wa_j_1bnflin-taxlw1.
    ENDIF.

    IF ( sy-subrc EQ 0 ) AND ( vl_cst_icms NOT IN r_cst_not_deson ) and wa_zsdt0098-MOTIVO IS NOT INITIAL.

      CLEAR: zona_franca.
      IF in_doc-partyp EQ 'C'.

        CALL METHOD me->seleci_knvi
          EXPORTING
            p_docnum      = in_doc-docnum
            p_parid       = in_doc-parid
            p_empresa     = in_doc-bukrs
          IMPORTING
            p_zona_franca = zona_franca.

        IF zona_franca EQ 'X'.

          CALL METHOD me->seleci_knb1
            EXPORTING
              p_parid = in_doc-parid
              p_bukrs = in_doc-bukrs
            IMPORTING
              s_kverm = tx_kverm.

          out_header-e1_isuf = tx_kverm.

          vl_field = '(SAPLJ_1B_NFE)WK_ITEM_TAX[]'.
          ASSIGN (vl_field) TO <item_tax>.
          CLEAR vl_field.
          tl_1bnfstx[] = <item_tax>.

          CLEAR: valor_deconto.
          LOOP AT tl_lin INTO sl_lin.
            LOOP AT tl_1bnfstx INTO sl_1bnfstx WHERE itmnum EQ sl_lin-itmnum .
              IF ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND ( sl_1bnfstx-excbas GT 0 ).
                sl_1bnfstx-excbas = ( sl_1bnfstx-excbas / ( ( 100 - 12 ) / 100 ) ).
                valor_deconto = valor_deconto + ( sl_1bnfstx-excbas * ( 12 / 100 ) ).
              ENDIF.
            ENDLOOP.
          ENDLOOP.
          ""Comentado cenário ocorreu somente 1 vez -- aguardar --LP
**          IF valor_deconto GT 0.
**            IF wa_desconto IS INITIAL.                        "<<RIM-SKM-IR139581
**              ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.
**              <fs_xmlh>-s1_vprod = <fs_xmlh>-s1_vprod + valor_deconto.
**              <fs_xmlh>-s1_vnf   = <fs_xmlh>-s1_vnf.
**              <fs_xmlh>-s1_vdesc = <fs_xmlh>-s1_vdesc + valor_deconto.
**              CLEAR: out_header-vicmsdeson, out_header-vdesc.   "<<RIM-SKM-IR139581
**              wa_desconto = 'X'.                                "<<RIM-SKM-IR139581
**              ADD valor_deconto TO out_header-vicmsdeson.
**              ADD valor_deconto TO out_header-vdesc.
**              "DATA(CK_SOMAR_VICMSDESON) = ABAP_TRUE.
**              "DATA(CK_SOMAR_VDESC)      = ABAP_TRUE.
**            ENDIF.                                            "<<RIM-SKM-IR139581
**          ENDIF.


        ENDIF.

      ENDIF.

      IF zona_franca IS INITIAL.

        REFRESH: it_regio_deson.

        SELECT SINGLE * INTO wa_j_1bbranch
          FROM j_1bbranch
         WHERE bukrs  EQ in_doc-bukrs
           AND branch EQ in_doc-branch.

        "Endereço Entrada
        SELECT SINGLE * INTO @DATA(wa_adrc_2)
          FROM adrc
         WHERE addrnumber EQ @wa_j_1bbranch-adrnr.

        wa_regio_deson = wa_adrc_2-region.
        APPEND wa_regio_deson TO it_regio_deson.

        CLEAR: icms_desonerado.

        LOOP AT it_regio_deson INTO wa_regio_deson.

          IF icms_desonerado IS NOT INITIAL.
            EXIT.
          ENDIF.

          CALL METHOD me->verifica_icms_desonerado
            EXPORTING
              p_bukrs         = in_doc-bukrs
              p_branch        = in_doc-branch
              p_partyp        = in_doc-partyp
              p_parid         = in_doc-parid
              p_taxlw1        = wa_j_1bnflin-taxlw1
              p_pais          = 'BR'
              p_regio         = wa_regio_deson
            IMPORTING
              p_desonerado    = icms_desonerado
              p_rate          = var_rate
              p_ck_lei_fiscal = ck_lei_fiscal.   "Utilizado para GRC

          DATA(estado_selecionado) = wa_regio_deson.

        ENDLOOP.

        IF NOT icms_desonerado IS INITIAL.

          IF NOT <item_tax> IS ASSIGNED.
            vl_field = '(SAPLJ_1B_NFE)WK_ITEM_TAX[]'.
            ASSIGN (vl_field) TO <item_tax>.
            CLEAR vl_field.
            tl_1bnfstx[] = <item_tax>.
          ENDIF.

          CLEAR: valor_deconto.
          LOOP AT tl_lin INTO sl_lin WHERE itmnum EQ wa_j_1bnflin-itmnum.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração
*            IF sl_lin-itmtyp EQ 'T5' OR sl_lin-itmtyp EQ '01'.
*
*              SELECT SINGLE matnr
*                FROM zmmt_ex_icms_des
*                INTO @DATA(lv_matnr)
*                WHERE matnr EQ @sl_lin-matnr
*                AND   parid EQ @in_doc-parid.
*
*              IF lv_matnr IS NOT INITIAL.
*
*                CONTINUE.
*
*              ENDIF.
*
*            ENDIF.
*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração

            LOOP AT tl_1bnfstx INTO sl_1bnfstx WHERE itmnum EQ wa_j_1bnflin-itmnum.
              IF ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND ( sl_1bnfstx-excbas GT 0 ).

                " sl_1bnfstx-excbas = ( sl_1bnfstx-excbas / ( ( 100 - var_rate ) / 100 ) ). US 119643
                "  sl_1bnfstx-excbas = ( sl_1bnfstx-excbas / ( ( 100 - var_rate ) / 100 ) ).
                valor_deconto = valor_deconto + ( sl_1bnfstx-excbas * ( var_rate / 100 ) ). "*-Equalização RISE x PRD - 28.08.2023 - JT

              ELSEIF ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND ( sl_1bnfstx-othbas GT 0 ).

                " sl_1bnfstx-othbas = ( sl_1bnfstx-othbas / ( ( 100 - var_rate ) / 100 ) ).
                " sl_1bnfstx-othbas = ( sl_1bnfstx-othbas / ( ( 100 - var_rate ) / 100 ) ).   "*-Equalização RISE x PRD - 28.08.2023 - JT
                valor_deconto = valor_deconto + ( sl_1bnfstx-othbas * ( var_rate / 100 ) ).

              ENDIF.
            ENDLOOP.
          ENDLOOP.

          IF valor_deconto GT 0.
            <fs_xmlh>-s1_vnf   = <fs_xmlh>-s1_vnf.
*            "<FS_XMLH>-S1_VDESC = 0.
*
            IF ck_lei_fiscal EQ abap_true.
              <fs_xmlh>-s1_vprod = <fs_xmlh>-s1_vprod + valor_deconto.
              <fs_xmlh>-s1_vdesc = <fs_xmlh>-s1_vdesc + valor_deconto.
            ENDIF.
*
            ADD valor_deconto TO out_header-vicmsdeson.
            ADD valor_deconto TO out_header-vdesc.
            "CK_SOMAR_VICMSDESON = ABAP_TRUE.
            "CK_SOMAR_VDESC      = ABAP_TRUE.
          ENDIF.

        ENDIF.

        IF icms_desonerado IS INITIAL.

          CLEAR: icms_nao_tribut, valor_deconto.

          LOOP AT it_regio_deson INTO wa_regio_deson.

            IF icms_nao_tribut IS NOT INITIAL.
              EXIT.
            ENDIF.

            CALL METHOD me->verifica_icms_nao_tributado
              EXPORTING
                p_bukrs         = in_doc-bukrs
                p_branch        = in_doc-branch
                p_partyp        = in_doc-partyp
                p_parid         = in_doc-parid
                p_taxlw1        = wa_j_1bnflin-taxlw1
                p_pais          = 'BR'
                p_regio         = wa_regio_deson
              IMPORTING
                p_nao_tributado = icms_nao_tribut
                p_ck_lei_fiscal = ck_lei_fiscal.

          ENDLOOP.

          IF icms_nao_tribut IS NOT INITIAL.

            ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.

            IF ( <fs_xmlh>       IS ASSIGNED    ) AND
               ( tl_itm_xml_tab  IS NOT INITIAL ) AND
               ( sl_itm_xml      IS NOT INITIAL ).

              IF NOT <item_tax> IS ASSIGNED.
                vl_field = '(SAPLJ_1B_NFE)WK_ITEM_TAX[]'.
                ASSIGN (vl_field) TO <item_tax>.
                CLEAR vl_field.
                tl_1bnfstx[] = <item_tax>.
              ENDIF.

              DATA: v_basered1 TYPE j_1bnfstx-basered1,
                    v_base_tot TYPE j_1bnfstx-base.

              LOOP AT tl_lin INTO sl_lin WHERE itmnum EQ wa_j_1bnflin-itmnum.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração
*                IF sl_lin-itmtyp EQ 'T5' OR sl_lin-itmtyp EQ '01'.
*
*                  SELECT SINGLE matnr
*                    FROM zmmt_ex_icms_des
*                    INTO @lv_matnr
*                    WHERE matnr EQ @sl_lin-matnr
*                    AND   parid EQ @in_doc-parid.
*
*                  IF lv_matnr IS NOT INITIAL.
*
*                    CONTINUE.
*
*                  ENDIF.
*
*                ENDIF.
*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração

                READ TABLE tl_itm_xml_tab INTO sl_itm_xml WITH KEY docnum = sl_lin-docnum
                                                                   itmnum = sl_lin-itmnum.
                IF ( sy-subrc = 0 ) AND ( sl_itm_xml-vprod > 0  ).

                  LOOP AT tl_1bnfstx INTO sl_1bnfstx WHERE itmnum EQ wa_j_1bnflin-itmnum.
                    CLEAR: vl_base_n_tribut, vl_aliq_n_tribut, vl_perc_n_tribut, v_basered1,v_base_tot.

                    v_base_tot = sl_1bnfstx-base + sl_1bnfstx-excbas.
                    IF v_base_tot > 0..
                      v_basered1 = ( sl_1bnfstx-base / v_base_tot ) * 100.
                    ENDIF.

                    IF ( sy-subrc  = 0                 ) AND
                       ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND
                       ( v_basered1 > 0       ) AND
                       ( v_basered1 < 100     ) AND
                       ( sl_1bnfstx-rate     > 0       ).

                      vl_base_n_tribut = ( 100 - v_basered1 ) / 100.
                      vl_aliq_n_tribut = vl_base_n_tribut * sl_1bnfstx-rate.
                      vl_perc_n_tribut = ( 100 - vl_aliq_n_tribut ) / 100.

                      IF vl_perc_n_tribut NE 0.
                        "VALOR_DECONTO = VALOR_DECONTO + ( ( SL_ITM_XML-VPROD / VL_PERC_N_TRIBUT ) * ( VL_ALIQ_N_TRIBUT / 100 ) ).
                        valor_deconto = valor_deconto + sl_itm_xml-vdesc.
                      ENDIF.

                    ELSEIF v_basered1 EQ 0.

                      IF ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND ( sl_1bnfstx-excbas GT 0 ).

                        sl_1bnfstx-excbas = ( sl_1bnfstx-excbas / ( ( 100 - var_rate ) / 100 ) ).
                        valor_deconto = valor_deconto + ( sl_1bnfstx-excbas * ( var_rate / 100 ) ).

                      ELSEIF ( sl_1bnfstx-taxtyp(3) EQ 'ICM' ) AND ( sl_1bnfstx-othbas GT 0 ).

                        sl_1bnfstx-othbas = ( sl_1bnfstx-othbas / ( ( 100 - var_rate ) / 100 ) ).
                        valor_deconto = valor_deconto + ( sl_1bnfstx-othbas * ( var_rate / 100 ) ).

                      ENDIF.

                    ENDIF.

                  ENDLOOP.
                ELSE.
                  CLEAR: valor_deconto.
                  EXIT.
                ENDIF.

              ENDLOOP.

              IF valor_deconto GT 0.
                IF NOT <fs_xmlh> IS ASSIGNED.
                  ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.
                ENDIF.
                IF ck_lei_fiscal EQ abap_true.
                  <fs_xmlh>-s1_vprod = <fs_xmlh>-s1_vprod + valor_deconto.
                  <fs_xmlh>-s1_vnf   = <fs_xmlh>-s1_vnf.
                  <fs_xmlh>-s1_vdesc = <fs_xmlh>-s1_vdesc + valor_deconto.
                ENDIF.
                ADD valor_deconto TO out_header-vicmsdeson.
                ADD valor_deconto TO out_header-vdesc.
                "CK_SOMAR_VICMSDESON = ABAP_TRUE.
                "CK_SOMAR_VDESC      = ABAP_TRUE.
              ENDIF.

            ENDIF. " IF <TOTAL> IS ASSIGNED.

          ENDIF. " IF ICMS_NAO_TRIBUT IS NOT INITIAL.

        ENDIF. "IF ICMS_DESONERADO IS INITIAL.

      ENDIF. "IF ZONA_FRANCA IS INITIAL.


    ENDIF.
    CLEAR: wa_zsdt0098.

  ENDSELECT.

*  IF CK_SOMAR_VICMSDESON EQ ABAP_TRUE OR
*     CK_SOMAR_VDESC EQ ABAP_TRUE.
*
*    OUT_HEADER-VICMSDESON = 0.
*    OUT_HEADER-VDESC = 0.
*    LOOP AT TL_LIN INTO DATA(WA_LIN).
*      ADD WA_LIN-VICMSDESON TO OUT_HEADER-VICMSDESON.
*    ENDLOOP.
*
*    IF OUT_HEADER-VICMSDESON IS INITIAL.
*      ADD <FS_XMLH_310>-VICMSDESON TO OUT_HEADER-VICMSDESON.
*    ENDIF.
*
*    IF <ITENS_XML_TAB> IS ASSIGNED.
*      LOOP AT <ITENS_XML_TAB> INTO DATA(WA_ITENS_XML).
*        ADD WA_ITENS_XML-VDESC TO OUT_HEADER-VDESC.
*      ENDLOOP.
*    ENDIF.
*
*  ENDIF.

  IF NOT vl_textos IS INITIAL.
    CONCATENATE out_header-infcomp
                vl_textos
           INTO out_header-infcomp SEPARATED BY space.
  ENDIF.

  IF NOT vl_seguro IS INITIAL.
    CONCATENATE out_header-infcomp
                vl_seguro
           INTO out_header-infcomp SEPARATED BY space.
  ENDIF.

  IF NOT vl_exportacao IS INITIAL.
    CONCATENATE out_header-infcomp
                vl_exportacao
           INTO out_header-infcomp SEPARATED BY space.
  ENDIF.

  CALL METHOD me->select_nf_produtor
    EXPORTING
      p_docnum     = sl_lin-docnum
      p_itmnum     = sl_lin-itmnum
    IMPORTING
      e_observacao = vl_textos_pd.

  IF vl_textos_pd IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_pd INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_pd.
    ENDIF.
  ENDIF.

  CALL METHOD me->select_nf_remessa_form_lote
    EXPORTING
      i_docnum     = sl_lin-docnum
    IMPORTING
      e_observacao = vl_textos_pd.

  IF vl_textos_pd IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_pd INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_pd.
    ENDIF.
  ENDIF.

  CLEAR: vl_textos_pd.

  CALL METHOD me->select_coleta_produtor
    EXPORTING
      p_item_nf    = sl_lin
    IMPORTING
      e_observacao = vl_textos_pd.

  IF vl_textos_pd IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_pd INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_pd.
    ENDIF.
  ENDIF.

  READ TABLE tl_vbrp INTO sl_vbrp INDEX 1.

  CLEAR: vl_textos_pd.

  CLEAR: lv_e1_cpais.

  IF NOT tl_vbrp[] IS INITIAL.

    CALL METHOD me->block_w
      EXPORTING
        i_docnum       = in_doc-docnum
        i_vbeln        = sl_vbrp-aubel
      IMPORTING
        e_comex        = out_header-comex
        e_cpais        = lv_e1_cpais
        e_ufembarq     = out_header-ufembarq
        e_xlocembarq   = out_header-xlocembarq
        e_xlocdespacho = out_header-xlocdespacho   "Utilizado para GRC
        e_observacao   = vl_textos_pd.

    IF NOT vl_textos_pd IS INITIAL.
      IF out_header-infcomp IS NOT INITIAL.
        CONCATENATE out_header-infcomp vl_textos_pd INTO out_header-infcomp SEPARATED BY space.
      ELSE.
        out_header-infcomp = vl_textos_pd.
      ENDIF.
    ENDIF.

  ELSE.

    CALL METHOD me->block_w
      EXPORTING
        i_docnum     = in_doc-docnum
      IMPORTING
        e_comex      = out_header-comex
        e_cpais      = lv_e1_cpais
        e_ufembarq   = out_header-ufembarq
        e_xlocembarq = out_header-xlocembarq
        e_observacao = vl_textos_pd.

    IF NOT vl_textos_pd IS INITIAL.
      IF out_header-infcomp IS NOT INITIAL.
        CONCATENATE out_header-infcomp vl_textos_pd INTO out_header-infcomp SEPARATED BY space.
      ELSE.
        out_header-infcomp = vl_textos_pd.
      ENDIF.
    ENDIF.

  ENDIF.

  IF lv_e1_cpais IS NOT INITIAL.
    out_header-e1_cpais = lv_e1_cpais.
  ENDIF.

  CALL METHOD me->block_h2
    EXPORTING
      i_docnum     = in_doc-docnum
    IMPORTING
      o_importacao = o_importacao
      o_cpais      = out_header-e1_cpais.

*DRC-#127483-14.11.2023-JT-inicio

  "Comentando 29-11-2023
*  SELECT SINGLE *
*           FROM j_1befd_country
*           INTO @DATA(w_country)
*          WHERE land1 = @in_doc-land1.
*
*  SHIFT w_country-cod_pais LEFT DELETING LEADING '0'.
*
  out_header-c1_cpais = '1058'. "Brasil - Pais Emissor

*DRC-#127483-14.11.2023-JT-inicio

  CALL METHOD me->select_nf_exportacao_recusa
    EXPORTING
      i_docnum     = in_doc-docnum
    IMPORTING
      e_observacao = vl_textos_rec.

  IF NOT vl_textos_rec IS INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_rec INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_rec.
    ENDIF.
  ENDIF.

  IF sl_lin-reftyp EQ 'ZW'.

    CLEAR: vl_textos_z1, wa_info_part.

    wa_zfiwrt0015-seq_lcto = sl_lin-refkey(10).

    SELECT SINGLE * INTO wa_zfiwrt0015
      FROM zfiwrt0015
     WHERE seq_lcto EQ wa_zfiwrt0015-seq_lcto
       AND parvw    EQ 'Z1'.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = wa_zfiwrt0015-parid
          p_partype    = 'V'
        CHANGING
          wa_info_part = wa_info_part.

      IF NOT wa_info_part IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = vl_cnpj.

        CONCATENATE 'Local de Entrega:'
                     wa_info_part-name1
                    '-'
                    vl_cnpj
                    '-'
                    wa_info_part-stcd3
                    '-'
                    wa_info_part-stras
                    '-'
                    wa_info_part-pstlz
                    '-'
                    wa_info_part-ort01
                    '-'
                    wa_info_part-regio
               INTO vl_textos_z1 SEPARATED BY space.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_z1 INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_z1.
        ENDIF.
      ENDIF.
    ENDIF.

    "26.02.2018 - CS2018000295 - Ini
    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
     WHERE seq_lcto = @sl_lin-refkey(10).

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM zfiwrt0009 INTO @DATA(_wl_zfiwrt0009)
       WHERE seq_lcto = @_wl_zfiwrt0008-seq_lcto.

      IF sy-subrc = 0.
        IF ( _wl_zfiwrt0008-bukrs IS NOT INITIAL ) AND ( _wl_zfiwrt0009-matnr IS NOT INITIAL ).
          " TEXTOS DO MATERIAL.
          REFRESH tl_lines.
          vl_id       = '0001'.
          vl_language = 'PT'.
          vl_object   = 'MVKE'.
          CONCATENATE _wl_zfiwrt0009-matnr
                      _wl_zfiwrt0008-bukrs
                      '10' INTO vl_name.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = vl_id
              language                = vl_language
              name                    = vl_name
              object                  = vl_object
            TABLES
              lines                   = tl_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

          IF NOT tl_lines[] IS INITIAL.
            LOOP AT tl_lines INTO sl_tline.
              IF out_header-infcomp IS NOT INITIAL.
                CONCATENATE out_header-infcomp sl_tline-tdline INTO out_header-infcomp SEPARATED BY space.
              ELSE.
                out_header-infcomp = sl_tline-tdline.
              ENDIF.
              CLEAR sl_tline.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "26.02.2018 - CS2018000295 - Fim

  ENDIF.

  "Textos NF Industrialização
  DATA(_textos_nf_industrializacao) = me->get_textos_nf_industrializacao( i_lin = sl_lin ).
  IF _textos_nf_industrializacao IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp _textos_nf_industrializacao INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = _textos_nf_industrializacao.
    ENDIF.
  ENDIF.

  "Textos Controle Certificação Socioambiental.
  CLEAR: vl_textos_csoc.
  CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
    EXPORTING
      i_tp_mov           = 'S'
      i_docnum           = in_doc-docnum
    IMPORTING
      e_msg_certificacao = vl_textos_csoc.

  IF NOT vl_textos_csoc IS INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_csoc INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_csoc.
    ENDIF.
  ENDIF.
  "Fim Textos Controle Certificação Socioambiental.

  "Classificação Produto - 02.04.2018
  READ TABLE tl_lips INTO sl_lips INDEX 1.                                                                         "Soja
  IF ( sy-subrc = 0 ) AND ( tl_lips[] IS NOT INITIAL ) AND ( sl_lips-kvgr3 IS NOT INITIAL ) AND ( sl_lips-matkl EQ '700110' ) .
    CLEAR: vl_textos_cl_prod.

    CASE sl_lips-kvgr3.
      WHEN 'R'.
        vl_textos_cl_prod = 'Classificação: SOJA TRANSGENICO'.
      WHEN 'C'.
        vl_textos_cl_prod = 'Classificação: SOJA CONVENCIONAL'.
    ENDCASE.

    IF vl_textos_cl_prod IS NOT INITIAL.
      IF out_header-infcomp IS NOT INITIAL.
        CONCATENATE out_header-infcomp vl_textos_cl_prod INTO out_header-infcomp SEPARATED BY space.
      ELSE.
        out_header-infcomp = vl_textos_cl_prod.
      ENDIF.
    ENDIF.

  ENDIF.
  "Fim Classificação Produto.

  "Texto ICMS Interestadual
  CLEAR: vl_textos_icms_inter.

  CALL FUNCTION 'ZCALC_ICMS_VENDA_INTERESTADUAL'
    EXPORTING
      i_docnum         = in_doc-docnum
    IMPORTING
      e_calculado      = vl_icms_inter_calc
    CHANGING
      c_msg_nfe        = vl_textos_icms_inter
      c_vbcufdest      = wl_icms_inter-vbcufdest
      c_pfcpufdest     = wl_icms_inter-pfcpufdest
      c_picmsufdest    = wl_icms_inter-picmsufdest
      c_picmsinter     = wl_icms_inter-picmsinter
      c_picmsinterpart = wl_icms_inter-picmsinterpart
      c_vfcpufdest     = wl_icms_inter-vfcpufdest
      c_vicmsufdest    = wl_icms_inter-vicmsufdest
      c_vicmsufremet   = wl_icms_inter-vicmsufremet.

  IF vl_icms_inter_calc IS NOT INITIAL.
    ADD wl_icms_inter-vfcpufdest   TO <fs_xmlh_310>-v_fcpufdest.
    ADD wl_icms_inter-vicmsufdest  TO <fs_xmlh_310>-v_icmsufdest.
    ADD wl_icms_inter-vicmsufremet TO <fs_xmlh_310>-v_icmsufremet.
  ENDIF.


  IF ( vl_icms_inter_calc IS NOT INITIAL ) AND ( vl_textos_icms_inter IS NOT INITIAL ).
    IF out_header-infcomp IS NOT INITIAL.
      CONCATENATE out_header-infcomp vl_textos_icms_inter INTO out_header-infcomp SEPARATED BY space.
    ELSE.
      out_header-infcomp = vl_textos_icms_inter.
    ENDIF.
  ENDIF.
  "Fim Textos ICMS Interestadual

  "Texto Remessa/Material Transferencia.
  CLEAR: vl_textos_transf, vl_rem_transf, wl_zsdt0001_transf_arm, vl_textos_veic.
  CASE sl_lin-reftyp.
    WHEN 'MD'.
      SELECT SINGLE l~vbeln
        INTO vl_rem_transf
        FROM vbfa AS v  INNER JOIN lips     AS l  ON l~vbeln    = v~vbelv
                        INNER JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln
       WHERE v~vbeln         EQ sl_lin-refkey(10)
         AND re~tp_movimento EQ 'S'
         AND v~vbtyp_n       EQ 'R'
         AND v~vbtyp_v       EQ 'J'.  "Entrega via documento de material

      IF sy-subrc = 0.
        CONCATENATE 'Número do Fornecimento:' vl_rem_transf '/'
                    'Número do Doc.Material:' sl_lin-refkey(10)
               INTO vl_textos_transf SEPARATED BY space.
        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_transf INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_transf.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt0001 INTO wl_zsdt0001_transf_arm
         WHERE doc_rem     = vl_rem_transf
           AND fatura_prod = sl_lin-refkey(10).

      ENDIF.

      "Dados Veiculo
      IF wl_zsdt0001_transf_arm IS INITIAL.
        SELECT SINGLE *
          FROM zsdt0001 INTO wl_zsdt0001_transf_arm
         WHERE doc_material = sl_lin-refkey(10).
      ENDIF.

      IF wl_zsdt0001_transf_arm IS NOT INITIAL.

        CLEAR: vl_textos_veic.

*       PLACA DO VEICULO
        IF wl_zsdt0001_transf_arm-placa_cav IS NOT INITIAL.
          CONCATENATE 'PLACA DO VEICULO:'
                      wl_zsdt0001_transf_arm-placa_cav
                 INTO vl_field SEPARATED BY space.
          CONDENSE vl_field NO-GAPS.
          CONCATENATE vl_textos_veic
                      vl_field
                 INTO vl_textos_veic SEPARATED BY space.
        ENDIF.

*       PLACA DA CARRETA 1
        IF wl_zsdt0001_transf_arm-placa_car1 IS NOT INITIAL.
          CONCATENATE 'PLACA DA CARRETA(1):'
                      wl_zsdt0001_transf_arm-placa_car1
                 INTO vl_field SEPARATED BY space.
          CONDENSE vl_field NO-GAPS.
          CONCATENATE vl_textos_veic
                      vl_field
                 INTO vl_textos_veic SEPARATED BY space.
        ENDIF.

*       PLACA DA CARRETA 2
        IF wl_zsdt0001_transf_arm-placa_car2 IS NOT INITIAL.
          CONCATENATE 'PLACA DA CARRETA(2):'
                      wl_zsdt0001_transf_arm-placa_car2
                 INTO vl_field SEPARATED BY space.
          CONDENSE vl_field NO-GAPS.
          CONCATENATE vl_textos_veic
                      vl_field
                 INTO vl_textos_veic SEPARATED BY space.
        ENDIF.

*       PLACA DA CARRETA 3
        IF wl_zsdt0001_transf_arm-placa_car3 IS NOT INITIAL.
          CONCATENATE 'PLACA DA CARRETA(3):'
                      wl_zsdt0001_transf_arm-placa_car3
                 INTO vl_field SEPARATED BY space.
          CONDENSE vl_field NO-GAPS.
          CONCATENATE vl_textos_veic
                      vl_field
                 INTO vl_textos_veic SEPARATED BY space.
        ENDIF.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_veic INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_veic.
        ENDIF.

      ENDIF.

  ENDCASE.
  "Fim Texto Remessa/Material Transferencia.

  READ TABLE tl_itm_xml_tab INTO DATA(wa_itm_xml_tab) INDEX 1.

  "Textos - Central de descarte de embalagens vazias (CEARPA)/ Roteiro de entrega informado na ZSDT0081 / Local de armazenagem do produto  - Ini
  IF sl_zsdt0001-ch_referencia IS NOT INITIAL.

    CLEAR: wl_zsdt0134, wl_zsdt0139, wl_zsdt0140, wl_zsdt0082.

    SELECT SINGLE *
      FROM zsdt0134 INTO wl_zsdt0134
     WHERE ch_referencia = sl_zsdt0001-ch_referencia.

    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM zsdt0139 INTO wl_zsdt0139
       WHERE nro_cgd = wl_zsdt0134-nro_cg.

      SELECT SINGLE *
        FROM zsdt0140 INTO wl_zsdt0140
       WHERE nro_cgd  EQ wl_zsdt0134-nro_cg
         AND vbeln    EQ wl_zsdt0134-vbeln.

      IF wl_zsdt0140 IS NOT INITIAL.
        SELECT SINGLE *
          FROM zsdt0082 INTO wl_zsdt0082
         WHERE nro_sol = wl_zsdt0140-nro_sol
           AND seq     = wl_zsdt0140-seq.
      ENDIF.
    ENDIF.

    "Central de descarte de embalagens vazias (CEARPA)
    "SOMENTE DEFENSIVO
    IF wa_itm_xml_tab-cprod IS NOT INITIAL.
*-Equalização RISE x PRD - 28.08.2023 - JT - inicio
      DATA: sl_matnr TYPE matnr.                   "<<RIM-SKM-IR139581-18.08.23
      sl_matnr = wa_itm_xml_tab-cprod.             "<<RIM-SKM-IR139581-18.08.23
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'  "<<RIM-SKM-IR139581-18.08.23
        EXPORTING
          input        = sl_matnr
        IMPORTING
          output       = sl_matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      SELECT SINGLE * INTO @DATA(wa_mara)
        FROM mara
*       WHERE matnr EQ @wa_itm_xml_tab-cprod.     ">>RIM-SKM-IR139581-18.08.23
        WHERE matnr EQ @sl_matnr.                  "<<RIM-SKM-IR139581-18.08.23
*-Equalização RISE x PRD - 28.08.2023 - JT - fim

      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO @DATA(wa_matkl)
          FROM t023t
         WHERE matkl EQ @wa_mara-matkl
           AND spras EQ 'P'.
        IF sy-subrc IS INITIAL AND wa_matkl-wgbez CS 'DEFENSIVOS'.
          DATA(lc_ck_defensivos) = abap_true.
        ELSE.
*** Inicio - Rubenilson - 27.09.24 - #144012
          IF sy-subrc IS INITIAL AND wa_matkl-wgbez CS 'SEMENTE'.
            DATA(lv_sementes) = abap_true.
          ENDIF.
          lc_ck_defensivos = abap_false.
*** Fim - Rubenilson - 27.09.24 - #144012
        ENDIF.
      ELSE.
        lc_ck_defensivos = abap_false.
      ENDIF.

    ENDIF.

    CLEAR: vl_textos_cent_descarte.
    IF wl_zsdt0139 IS NOT INITIAL AND lc_ck_defensivos EQ abap_true.
      SELECT SINGLE *
        FROM lfa1 INTO @DATA(_wl_lfa1)
       WHERE lifnr EQ @wl_zsdt0139-cod_ce.

      IF sy-subrc EQ 0.
        CONCATENATE 'Descarte de embalagens:' _wl_lfa1-name1 _wl_lfa1-name2 _wl_lfa1-ort01 _wl_lfa1-stras _wl_lfa1-ort02 'CEP:' _wl_lfa1-pstlz '-' _wl_lfa1-ort01 '-' _wl_lfa1-regio
               INTO vl_textos_cent_descarte SEPARATED BY space.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_cent_descarte INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_cent_descarte.
        ENDIF.
      ENDIF.

      "(Comentario Temp) - Ini -  CS2019001202- ZNFE Info.add Nf.Veicul.Motor. - LH
    ELSEIF lc_ck_defensivos = abap_false.

      SELECT SINGLE *  FROM lfa1 INTO @DATA(wl_lfa1) WHERE lifnr EQ @sl_zsdt0001-motorista.

      SELECT SINGLE * FROM zsdt0134 INTO @DATA(wa_zsdt0134)
         WHERE ch_referencia EQ @sl_zsdt0001-ch_referencia.

      CONCATENATE ' Motorista: ' wl_lfa1-name1 'Fase: ' wa_zsdt0134-nr_fase INTO vl_textos_cent_descarte SEPARATED BY space.

      IF out_header-infcomp IS NOT INITIAL.
        CONCATENATE out_header-infcomp vl_textos_cent_descarte INTO out_header-infcomp SEPARATED BY space.
      ELSE.
        out_header-infcomp = vl_textos_cent_descarte.
      ENDIF.

      "(Comentario Temp) - Fim

    ENDIF.

    "Roteiro de entrega informado na ZSDT0081
    CLEAR:  vl_textos_rot_entrega.
    IF wl_zsdt0082-nr_rot IS NOT INITIAL.
      REFRESH tl_lines.
      vl_id       = 'ZROT'.
      vl_language = 'PT'.
      vl_name     = wl_zsdt0082-nr_rot.
      vl_object   = 'ZSDROTEIRO'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = vl_id
          language                = vl_language
          name                    = vl_name
          object                  = vl_object
        TABLES
          lines                   = tl_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF tl_lines[] IS NOT INITIAL.
        vl_textos_rot_entrega = 'Roteiro:'.
        LOOP AT tl_lines INTO sl_tline.
          CONCATENATE vl_textos_rot_entrega
                      sl_tline-tdline
                 INTO  vl_textos_rot_entrega SEPARATED BY space.
          CLEAR sl_tline.
        ENDLOOP.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_rot_entrega INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_rot_entrega.
        ENDIF.
      ENDIF.
    ENDIF.

    "Local de armazenagem do produto
    CLEAR: vl_textos_loc_arm_prod.
    IF ( wl_zsdt0139-cod_tr IS NOT INITIAL ) AND ( lc_ck_defensivos EQ abap_true ).
      SELECT SINGLE *
        FROM lfa1 INTO _wl_lfa1
       WHERE lifnr EQ wl_zsdt0139-cod_ar.

      IF sy-subrc EQ 0.
        CONCATENATE 'Mercadoria saira diretamente da' _wl_lfa1-name1 _wl_lfa1-stras
                    _wl_lfa1-ort02 'CEP:' _wl_lfa1-pstlz '-'
                    _wl_lfa1-ort01 '-' _wl_lfa1-regio 'CNPJ:' _wl_lfa1-stcd1 'IE:' _wl_lfa1-stcd3
               INTO vl_textos_loc_arm_prod SEPARATED BY space.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_loc_arm_prod INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_loc_arm_prod.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
  "Textos - Central de descarte de embalagens vazias (CEARPA)/ Roteiro de entrega informado na ZSDT0081 / Local de armazenagem do produto  - Fim
*** Inicio - Rubenilson - 27.09.24 - #144012
  IF lv_sementes IS NOT INITIAL AND  wl_zsdt0139-nro_cgd IS NOT INITIAL.
    SELECT *
      FROM zsdt0133
      INTO @DATA(ls_zsdt0133)
      UP TO 1 ROWS
      WHERE nro_cg = @wl_zsdt0139-nro_cgd.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM lfa1 INTO _wl_lfa1
       WHERE lifnr EQ ls_zsdt0133-armazem_org.

      IF sy-subrc EQ 0.
        CONCATENATE 'Mercadoria saira diretamente da' _wl_lfa1-name1 _wl_lfa1-stras
                    _wl_lfa1-ort02 'CEP:' _wl_lfa1-pstlz '-'
                    _wl_lfa1-ort01 '-' _wl_lfa1-regio 'CNPJ:' _wl_lfa1-stcd1 'IE:' _wl_lfa1-stcd3
               INTO vl_textos_loc_arm_prod SEPARATED BY space.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_loc_arm_prod INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_loc_arm_prod.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*** Fim - Rubenilson - 27.09.24 - #144012

*-Equalização RISE x PRD - 28.08.2023 - JT - inicio
  "Textos ICMS Monofasico - Ini
  IF sl_lin-v_icms_mono IS NOT INITIAL.
    CLEAR: vl_cst_icms.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = sl_lin-taxsit
      IMPORTING
        output = vl_cst_icms.


    CASE vl_cst_icms.
      WHEN '53'.

        CLEAR: vl_textos_trib_mono.

        WRITE sl_lin-v_icms_mono     TO vl_textos_icms_mono.
        WRITE sl_lin-v_icms_mono_dif TO vl_textos_icms_mono_dif.

        CONDENSE: vl_textos_icms_mono, vl_textos_icms_mono_dif NO-GAPS.

        vl_textos_trib_mono = |ICMS monofasico sobre combustiveis diferido conforme Convenio ICMS 199/2022; Valor| &&
                              | calculado a ser recolhido pelo produtor, no valor de { vl_textos_icms_mono } reais | &&
                              | e pelo adquirente no valor de { vl_textos_icms_mono_dif } reais|.

        IF out_header-infcomp IS NOT INITIAL.
          CONCATENATE out_header-infcomp vl_textos_trib_mono INTO out_header-infcomp SEPARATED BY space.
        ELSE.
          out_header-infcomp = vl_textos_trib_mono.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDIF.
  "Textos ICMS Monofasico - Fim
*-Equalização RISE x PRD - 28.08.2023 - JT - fim

  "PBI - 66384
  "Textos Adicionais por Tp.OV/ Filial - Ini - Novo
  CLEAR: vl_textos_obs_filial, it_obs.
  IF ( tl_zsdt0294[] IS NOT INITIAL ).

    DATA: l_matkl TYPE mara-matkl.

    LOOP AT <itens_xml_tab> ASSIGNING FIELD-SYMBOL(<fs_item_obs>).

      CLEAR: l_matkl.

*** cs1152158 - ir153805 - rmn - arm - 28.09.2023

      v_cprod = <fs_item_obs>-cprod.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_cprod
        IMPORTING
          output = v_cprod.
*** Fim alteração ARM.

      "1º REGRA BUSCAR TP OV/FILIAL/MATERIAL/
      LOOP AT tl_zsdt0294 INTO DATA(_wl_0294) WHERE auart  = sl_vbak-auart
                                               AND  branch = in_doc-branch
                                               AND  matnr  = v_cprod. "<fs_item_obs>-cprod.


        IF _wl_0294-id_obs IS NOT INITIAL.
          APPEND _wl_0294 TO it_obs.
          CLEAR:_wl_0294.
        ENDIF.

      ENDLOOP.

      "2º REGRA BUSCAR TP FILIAL/MATERIAL/
      LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = abap_off
                                        AND   branch = in_doc-branch
                                        AND   matnr  = v_cprod. "<fs_item_obs>-cprod.
        IF _wl_0294-id_obs IS NOT INITIAL.
          APPEND _wl_0294 TO it_obs.
          CLEAR:_wl_0294.
        ENDIF.
      ENDLOOP.

      "3º REGRA BUSCAR TP OV/MATERIAL
      LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = sl_vbak-auart
                                        AND   branch = ' '
                                        AND   matnr  = v_cprod. "<fs_item_obs>-cprod.

        IF _wl_0294-id_obs IS NOT INITIAL.
          APPEND _wl_0294 TO it_obs.
          CLEAR:_wl_0294.
        ENDIF.
      ENDLOOP.

      "4º REGRA BUSCAR MATERIAL
      LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = abap_off
                                          AND branch = ' '
                                          AND matnr  = v_cprod. "<fs_item_obs>-cprod.

        IF _wl_0294-id_obs IS NOT INITIAL.
          APPEND _wl_0294 TO it_obs.
          CLEAR:_wl_0294.
        ENDIF.
      ENDLOOP.

*-CS2022000324-25.08.2022-#84903-JT-inicio
      SELECT matkl
        INTO l_matkl
        FROM mara
          UP TO 1 ROWS
       WHERE matnr = v_cprod.
      ENDSELECT.

*-Equalização RISE x PRD - 28.08.2023 - JT - inicio
*** Inicio Alteracao ARM RIMINI CS1129362 IR147541 - 15.08.2023 -- usar DATA(lv_CPROD) =  |{  <ITEM>-CPROD  ALPHA = OUT }|.
      "JAIME >>> usar  EXEMPLO : DATA(lv_CPROD) =  |{  <ITEM>-CPROD  ALPHA = OUT }|.  conversão s/4han
      IF l_matkl IS INITIAL.
        v_cprod = <fs_item_obs>-cprod.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_cprod
          IMPORTING
            output = v_cprod.

*        v_cprod = |{ <fs_item_obs>-cprod  ALPHA = IN }|.

        SELECT matkl
          INTO (l_matkl)
          FROM mara
            UP TO 1 ROWS
         WHERE matnr = v_cprod.
        ENDSELECT.
      ENDIF.

*** Fim Alteracao ARM  RIMINI
*-Equalização RISE x PRD - 28.08.2023 - JT - fim

      IF l_matkl IS NOT INITIAL. "Buscas utilizando Grupo de Mercadoria

        "1º REGRA BUSCAR TP OV/FILIAL/GRP.MATERIAL/
        LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = sl_vbak-auart
                                            AND branch = in_doc-branch
                                            AND matkl  = l_matkl.
          IF _wl_0294-id_obs IS NOT INITIAL.
            APPEND _wl_0294 TO it_obs.
            CLEAR:_wl_0294.
          ENDIF.
        ENDLOOP.

        "2º REGRA BUSCAR TP FILIAL/GRP.MATERIAL/
        LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = abap_off
                                            AND branch = in_doc-branch
                                            AND matkl  = l_matkl.
          IF _wl_0294-id_obs IS NOT INITIAL.
            APPEND _wl_0294 TO it_obs.
            CLEAR:_wl_0294.
          ENDIF.
        ENDLOOP.

        "3º REGRA BUSCAR TP OV/GRP.MATERIAL
        LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = sl_vbak-auart
                                            AND branch = ' '
                                            AND matkl  = l_matkl.
          IF _wl_0294-id_obs IS NOT INITIAL.
            APPEND _wl_0294 TO it_obs.
            CLEAR:_wl_0294.
          ENDIF.
        ENDLOOP.

        "4º REGRA BUSCAR GRP.MATERIAL
        LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = abap_off
                                            AND branch = ' '
                                            AND matkl  = l_matkl.
          IF _wl_0294-id_obs IS NOT INITIAL.
            APPEND _wl_0294 TO it_obs.
            CLEAR:_wl_0294.
          ENDIF.
        ENDLOOP.

      ENDIF.

*-CS2022000324-25.08.2022-#84903-JT-fim

    ENDLOOP.

    "3º Regra buscar TP OV / Filial
    LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = sl_vbak-auart
                                      AND   branch = in_doc-branch
                                      AND   matnr  = ' '
                                      AND   matkl  = ' ' .  "*-CS2022000324-25.08.2022-#84903-JT-fim
      IF _wl_0294-id_obs IS NOT INITIAL.
        APPEND _wl_0294 TO it_obs.
        CLEAR:_wl_0294.
      ENDIF.
    ENDLOOP.

    "3º Regra buscar Filial
    LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = abap_off
                                      AND   branch = in_doc-branch
                                      AND   matnr  = ' '
                                      AND   matkl  = ' ' .  "*-CS2022000324-25.08.2022-#84903-JT-fim
      IF _wl_0294-id_obs IS NOT INITIAL.
        APPEND _wl_0294 TO it_obs.
        CLEAR:_wl_0294.
      ENDIF.
    ENDLOOP.
    "4º REGRA BUSCAR POR TP OV

    LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE auart  = sl_vbak-auart
                                      AND   branch = ' '
                                      AND   matnr  = ' '
                                      AND   matkl  = ' ' .  "*-CS2022000324-25.08.2022-#84903-JT-fim
      IF _wl_0294-id_obs IS NOT INITIAL.
        APPEND _wl_0294 TO it_obs.
        CLEAR:_wl_0294.
      ENDIF.

    ENDLOOP.

    IF it_obs[] IS NOT INITIAL.
      SORT it_obs BY id_obs.
      DELETE ADJACENT DUPLICATES FROM it_obs COMPARING id_obs.
      CLEAR:_wl_0294.
      LOOP AT it_obs INTO _wl_0294.

        SELECT SINGLE   observ , observ2
          FROM zsdt0287
          INTO (@DATA(lv_observ), @DATA(lv_observ2) )
          WHERE id_obs    EQ @_wl_0294-id_obs
            AND cancelado  = @abap_off.

        IF sy-subrc EQ 0 .
          CONCATENATE vl_textos_obs_filial
                    lv_observ
                    lv_observ2
               INTO vl_textos_obs_filial SEPARATED BY space .

          CONCATENATE vl_textos_obs_filial
          lv_separador
          INTO vl_textos_obs_filial SEPARATED BY space .

        ENDIF.

      ENDLOOP.

    ENDIF.

    IF vl_textos_obs_filial IS NOT INITIAL.
      IF out_header-infcomp IS NOT INITIAL.
        CONCATENATE out_header-infcomp vl_textos_obs_filial INTO out_header-infcomp SEPARATED BY lv_separador.
      ELSE.
        out_header-infcomp = vl_textos_obs_filial.
      ENDIF.
    ENDIF.

    "Textos Adicionais por Tp.OV/ Filial - Fim - Novo



*   PBI - 66384 -- CS2021000908 Ajuste Impressão NFe - materiais ASSEGURADO GMP+FSA  - Etapa 2
*   "Textos Adicionais por Tp.OV/ Filial - Ini
*    READ TABLE tl_zsdt0294 INTO DATA(_wl_0294) WITH KEY branch = in_doc-branch.
*    IF ( sy-subrc EQ 0 ) AND ( in_doc-branch is NOT INITIAL ).
*
*      IF ( _wl_0294-observ IS NOT INITIAL ) OR ( _wl_0294-observ2 IS NOT INITIAL ).
*        CONCATENATE vl_textos_obs_filial
*                    _wl_0294-observ
*                    _wl_0294-observ2
*               INTO vl_textos_obs_filial SEPARATED BY space.
*      ENDIF.
*
*      IF vl_textos_obs_filial IS NOT INITIAL.
*        IF out_header-infcomp IS NOT INITIAL.
*          CONCATENATE out_header-infcomp vl_textos_obs_filial INTO out_header-infcomp SEPARATED BY space.
*        ELSE.
*          out_header-infcomp = vl_textos_obs_filial.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDIF. "IF ( tl_zsdt0294[] IS NOT INITIAL ).
**"Textos Adicionais por Tp.OV/ Filial - Fim

  SELECT SINGLE * INTO wa_j_1bbranch
    FROM j_1bbranch
   WHERE bukrs       EQ in_doc-bukrs
     AND branch      EQ in_doc-branch.

  IF sy-subrc IS INITIAL.

    CLEAR: wa_adrc.

    SELECT SINGLE * INTO wa_adrc
      FROM adrc
     WHERE addrnumber EQ wa_j_1bbranch-adrnr.

    IF ( wa_adrc-country EQ 'BR' ) AND ( wa_adrc-region EQ 'MT' ).

      SELECT SINGLE * INTO wa_zjcnd_branch
        FROM zjcnd_branch
       WHERE bukrs       EQ in_doc-bukrs
         AND branch      EQ in_doc-branch
         AND dt_emissao  LE in_doc-pstdat
         AND dt_validade GE in_doc-pstdat.

      IF sy-subrc IS INITIAL.
        CONCATENATE 'Nro. CND:' wa_zjcnd_branch-cd_cnd 'Cod. Autenticidade' wa_zjcnd_branch-cod_aut out_header-infadfisco_v2 INTO out_header-infadfisco_v2 SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

  "Preço de Pauta
  DATA: vlr_zivp   TYPE konv-kbetr,
        vlr_pr00   TYPE konv-kbetr,
        vlr_zvip_s TYPE string.


  REFRESH: tl_vbrk, tl_konv.
  CLEAR: wa_vbrk, wa_konv, vlr_zivp, vlr_pr00.

  SELECT * FROM vbrk INTO TABLE tl_vbrk WHERE vbeln EQ sl_lin-refkey.

  IF ( sy-subrc EQ 0 ).
    READ TABLE tl_vbrk INTO wa_vbrk INDEX 1.

*---> S4 MIGRATION 10/07/2023 - MA
*    SELECT * FROM konv INTO TABLE it_konv WHERE knumv EQ wa_vbrk-knumv
*                                            AND kschl IN ('ZIVP','ICMI').

    TYPES lr_kschl_type TYPE RANGE OF kschl.

    DATA : lr_kschl TYPE lr_kschl_type.


    lr_kschl = VALUE lr_kschl_type( LET s = 'I'
                                        o = 'EQ'
                                    IN sign   = s
                                       option = o
                                       ( low = 'ZIVP' )
                                       ( low = 'ICMI' ) ).


    SELECT * FROM v_konv INTO TABLE @DATA(lt_konv) WHERE knumv EQ @wa_vbrk-knumv
                                            AND kschl IN @lr_kschl.

    MOVE-CORRESPONDING lt_konv[] TO it_konv.
*---> S4 MIGRATION 10/07/2023 - MA

    CHECK NOT it_konv[] IS INITIAL.

    READ TABLE it_konv INTO wa_konv WITH KEY kschl = 'ZIVP'.
    vlr_zivp = wa_konv-kbetr.

    CLEAR: wa_konh.
    SELECT SINGLE * FROM konh INTO wa_konh WHERE knumh EQ wa_konv-knumh.

*---> S4 MIGRATION 10/07/2023 - MA
*    SELECT *
*      FROM konh
*      INTO TABLE it_konh
*      WHERE vakey = wa_konh-vakey.

    SELECT *
      FROM konh_kks
      INTO TABLE it_konh
      WHERE vakey = wa_konh-vakey.

*<--- S4 MIGRATION 10/07/2023 - MA
    CLEAR wa_konh_ant.
    SORT it_konh BY datab.
    LOOP AT it_konh INTO wa_konh.
      IF wa_konh-datab GT sy-datum.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING wa_konh TO wa_konh_ant.
    ENDLOOP.
    MOVE-CORRESPONDING wa_konh_ant TO wa_konh.


    CLEAR: wa_konv.
    READ TABLE it_konv INTO wa_konv WITH KEY kschl = 'ICMI'.
    vlr_pr00 = wa_konv-kbetr.
    "inicio

    CLEAR vl_vbelv.
    SELECT vbelv INTO vl_vbelv
                 FROM vbfa
             UP TO 1 ROWS
                WHERE vbeln = sl_likp-vbeln
                  AND vbtyp_n = 'J'
                  AND vbtyp_v = 'C'.
    ENDSELECT.

    IF NOT vl_vbelv IS INITIAL.
      SELECT SINGLE * INTO wa_vbak
        FROM vbak
       WHERE vbeln = vl_vbelv.

      SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ wa_vbak-kunnr.
      SELECT SINGLE * INTO wa_vbap
        FROM vbap
      WHERE vbeln = wa_vbak-vbeln.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_vbap-werks
        IMPORTING
          output = wa_lifnr.

      SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_lifnr.
    ENDIF.

    IF wa_kna1-regio IS NOT INITIAL AND ( wa_kna1-regio EQ  wa_lfa1-regio ) AND ( wa_vbak-auart NE 'ZRFL').
      " Não refaz o calculo nesta condição
    ELSE.
      IF ( vlr_zivp > vlr_pr00 ).
        vlr_zvip_s = vlr_zivp.

        DATA: lva_ds_sefaz TYPE string.

        lva_ds_sefaz = '- SEFAZ/MT'.

        SELECT SINGLE * INTO @DATA(lwa_branch_emi)
          FROM j_1bbranch
         WHERE bukrs  EQ @in_doc-bukrs
           AND branch EQ @in_doc-branch.

        IF sy-subrc EQ 0.
          SELECT SINGLE * INTO @DATA(lwa_adrc_emi)
            FROM adrc
           WHERE addrnumber EQ @lwa_branch_emi-adrnr.

          IF sy-subrc EQ 0.
            lva_ds_sefaz = '- SEFAZ/' && lwa_adrc_emi-region.
          ENDIF.
        ENDIF.

        CONCATENATE out_header-infadfisco_v2 'ICMS CALCULADO PELA PAUTA'  sl_likp-inco1 'R$' vlr_zvip_s '/KG CONF PORTARIA N°' wa_konh-kosrt lva_ds_sefaz INTO out_header-infadfisco_v2 SEPARATED BY space.
      ENDIF.
    ENDIF.

  ENDIF.

  "Comentario Temp - Ini
  DATA: texto_destinacao TYPE string.
  PERFORM retorna_texto IN PROGRAM z_destinacao_mercadoria IF FOUND USING in_doc-docnum CHANGING texto_destinacao.
  IF texto_destinacao IS NOT INITIAL.
    IF out_header-infcomp IS NOT INITIAL.
      out_header-infcomp = |{ out_header-infcomp } - { texto_destinacao }| .
    ELSE.
      out_header-infcomp = texto_destinacao.
    ENDIF.
  ENDIF.
  "Comentario Temp - Fim

  "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - INICIO
  zcl_eudr_utils=>get_textos_eudr_nf_saida(
    EXPORTING
      i_docnum = in_doc-docnum                " Nº documento
    RECEIVING
      r_textos = DATA(lva_textos_eudr) ).             " Textos
  IF lva_textos_eudr IS NOT INITIAL.
    CONCATENATE out_header-infcomp lva_textos_eudr INTO out_header-infcomp SEPARATED BY space.
  ENDIF.
  "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - FIM
**<<<------"178644 - NMS - INI------>>>
* Textos NF Saída - Fluxo Exportação.
  zcl_im_cl_fluxo_exportacao=>get_textos_flxexp_nf_saida(
    EXPORTING
      i_docnum = in_doc-docnum                "Nº documento
    RECEIVING
      r_textos = DATA(lva_textos_flxexp) ).   "Textos

  IF lva_textos_flxexp IS NOT INITIAL.
    CONCATENATE out_header-infcomp lva_textos_flxexp INTO out_header-infcomp SEPARATED BY space.

  ENDIF.
**<<<------"178644 - NMS - FIM------>>>
  "Observações Contribuinte
  me->get_dados_veiculo_nfe(
    EXPORTING
      in_xml_header = in_xml_header
    IMPORTING
      e_codigo_mot  = DATA(_codigo_moto)
      e_cpf_mot     = DATA(_cpf_mot)
      e_nome_mot    = DATA(_nome_mot)
      e_placa       = DATA(_placa)
      e_placa_uf    = DATA(_placa_uf)
      e_placa1      = DATA(_placa1)
      e_placa1_uf   = DATA(_placa1_uf)
      e_placa2      = DATA(_placa2)
      e_placa2_uf   = DATA(_placa2_uf)
      e_placa3      = DATA(_placa3)
      e_placa3_uf   = DATA(_placa3_uf)
      e_rntrc       = DATA(_rntrc)
  ).


  "================================================== USER STORY 83741 / Anderson Oenning
  "Preencher domicilio fiscal se a empresa do parametro estiver cadastrado no SET ->.
  "Preencher domicilio fiscal se a OV e o centro estiver cadastrado na transação ZSDT0188.

  CLEAR: wl_zsdt0284.
  SELECT SINGLE * FROM zsdt0284 INTO wl_zsdt0284 WHERE vbeln EQ sl_vbap-vbeln.

  IF wl_zsdt0284 IS NOT INITIAL.
    SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
    WHERE setname EQ 'MAGI_EMP_DOM_FISCAL'
    AND valfrom EQ @wl_zsdt0284-bukrs.

    IF sy-subrc EQ 0.
      v_lifnr = wl_zsdt0284-werks.
      v_lifnr = |{ v_lifnr ALPHA = IN }|.
      SELECT SINGLE txjcd FROM lfa1 INTO vg_txjcd WHERE lifnr EQ v_lifnr.
      IF sy-subrc EQ 0.
        APPEND VALUE #( mandt = sy-mandt docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Microregiao'
                     xtexto    =  vg_txjcd ) TO out_add_info.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: vg_txjcd, i_data, v_lifnr, wl_zsdt0284.
  "================================================= USER STORY 83741 / Anderson Oenning

  IF _placa IS NOT INITIAL.
    v_texto_cont_180 =  _placa && '-' && _placa_uf.

    IF _rntrc IS NOT INITIAL.
      v_texto_cont_180 = v_texto_cont_180 && '/ RNTC:' && _rntrc.
    ENDIF.

    APPEND VALUE #( mandt = sy-mandt docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Placa Veiculo'
                    xtexto    =  v_texto_cont_180+000(60) ) TO out_add_info.
  ENDIF.

  IF _placa1 IS NOT INITIAL.
    v_texto_cont_180 =  _placa1 && '-' && _placa1_uf.

    APPEND VALUE #( mandt = sy-mandt  docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Placa Reboque 1'
                    xtexto    =  v_texto_cont_180+000(60) ) TO out_add_info.
  ENDIF.

  IF _placa2 IS NOT INITIAL.
    v_texto_cont_180 =  _placa2 && '-' && _placa2_uf.

    APPEND VALUE #( mandt = sy-mandt  docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Placa Reboque 2'
                    xtexto    =  v_texto_cont_180+000(60) ) TO out_add_info.
  ENDIF.

  IF _placa3 IS NOT INITIAL.
    v_texto_cont_180 =  _placa3 && '-' && _placa3_uf.

    APPEND VALUE #( mandt = sy-mandt  docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Placa Reboque 3'
                    xtexto    =  v_texto_cont_180+000(60) ) TO out_add_info.
  ENDIF.



  IF _cpf_mot IS NOT INITIAL.
    v_texto_cont_180 =  _cpf_mot && '-' && zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( CONV #( _nome_mot ) ) ).

    CONCATENATE 'CPF:' v_texto_cont_180 INTO v_texto_cont_180 SEPARATED BY space.

    APPEND VALUE #( mandt = sy-mandt  docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Motorista'
                    xtexto    =  v_texto_cont_180+000(60)
                    xtexto2   =  v_texto_cont_180+060(60)
                    xtexto3   =  v_texto_cont_180+120(60) ) TO out_add_info.
  ENDIF.

  IF in_xml_header-t3_placa IS NOT INITIAL.
    v_texto_cont_180 =  in_xml_header-t3_placa && '-' && in_xml_header-t3_uf1.

    IF in_xml_header-t3_rntc IS NOT INITIAL.
      v_texto_cont_180 = v_texto_cont_180 && '/ RNTC:' && in_xml_header-t3_rntc.
    ENDIF.

    APPEND VALUE #( mandt     = sy-mandt  docnum = in_xml_header-docnum
                    inf_usage = '1' xcampo = 'Placa Reboque'
                    xtexto    =  v_texto_cont_180+000(60)
                    xtexto2   =  v_texto_cont_180+060(60)
                    xtexto3   =  v_texto_cont_180+120(60) ) TO out_add_info.
  ENDIF.

  DATA: vnftype TYPE j_1bdydoc-nftype,
        vdoctyp TYPE j_1baa-doctyp.

  " Tratar campo FINNFE
  "IF IN_XML_HEADER-FINNFE IS INITIAL.
  SELECT SINGLE nftype
    FROM j_1bnfdoc
    INTO vnftype
    WHERE docnum = in_doc-docnum.

  IF sy-subrc = 0.

    SELECT SINGLE doctyp
      FROM j_1baa
      INTO vdoctyp
      WHERE nftype EQ vnftype .

    "Alteração para a versão do novo Layou tda NFe 3.1
    "Finalidade de Emissão da Nf-e: Devolução/Retorno
    "Existem 4 tipos de finalidade sendo elas;
    " 1 - Nf-e normal
    " 2 - Nf-e complementar
    " 3 - Nf-e de ajuste
    " 4 - Devolução/Retorno
    "Dentro da MAGGI a 4 já esta sendo utilizado para outra finalidade, mas será criado a regra abaixo
    "definindo quando for 6 (Devolução/Retorno) será atribuido o valor 4 na TAG finNFE.

    IF ( vdoctyp EQ '6' ).
      out_header-finnfe = '4'.
    ELSE.
      out_header-finnfe = vdoctyp.
    ENDIF.

    out_header-dsaient = in_doc-docdat.

    "ENDIF.
  ELSE.
    out_header-finnfe = in_xml_header-finnfe.
  ENDIF.

  "OUT_HEADER-FINNFE = IN_XML_HEADER-FINNFE.

  CALL METHOD me->determinar_parceiros
    EXPORTING
      i_xml_header = in_xml_header
    CHANGING
      c_out_header = out_header.

  "Utilizado para GRC

  SELECT SINGLE * INTO @DATA(wa_j_1bnfe_tec_resp)
    FROM j_1bnfe_tec_resp
   WHERE docnum EQ @in_doc-docnum.

  IF sy-subrc IS INITIAL.
    out_tech_resp-cnpj      = wa_j_1bnfe_tec_resp-cnpj.
    out_tech_resp-csrt      = wa_j_1bnfe_tec_resp-csrt.
    out_tech_resp-email     = wa_j_1bnfe_tec_resp-email.
    out_tech_resp-fone      = wa_j_1bnfe_tec_resp-phone.
    out_tech_resp-id_csrt   = wa_j_1bnfe_tec_resp-idcsrt.
    out_tech_resp-x_contato = wa_j_1bnfe_tec_resp-contact.
  ELSE.
    out_tech_resp = zcl_im_cl_nfe_print=>get_resp_tecnico(
      i_uf     = in_xml_header-c1_uf
      i_bukrs  = in_doc-bukrs
      i_branch = in_doc-branch ).
  ENDIF.

  DATA(_sem_frete) = ''.

  SELECT SINGLE *
    FROM j_1bnflin INTO @DATA(vj_1bnflin)
   WHERE docnum EQ @in_doc-docnum.

  CASE in_doc-inco1.
    WHEN 'FOB'." OR 'CPT'. "//ISSUE-188371 WBARBOSA 19/08/25
      "Ajuste Tipo de Frete para fatura agrupada a partir da ZLEST0106/ZLEST0136
      SELECT SINGLE *
        FROM j_1bnfnad INTO @DATA(wl_j_1bnfnad)
       WHERE docnum = @in_doc-docnum
         AND parvw  = 'LR'.
      IF sy-subrc = 0.
        DATA(vl_parid_tmp) = in_doc-parid.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vl_parid_tmp
          IMPORTING
            output = vl_parid_tmp.

        SELECT SINGLE *
          FROM zsdt0121 INTO @DATA(wl_zsdt0121)
         WHERE werks EQ @vl_parid_tmp+6(4)
           AND matnr EQ @vj_1bnflin-matnr
           AND kunnr EQ @wl_j_1bnfnad-parid.

        IF ( sy-subrc = 0 ) AND ( vj_1bnflin-refkey(10) IS NOT INITIAL ).
          SELECT SINGLE *
            FROM zsdt0001 INTO @DATA(wl_zsdt0001)
           WHERE fatura_prod EQ @vj_1bnflin-refkey(10).
          IF sy-subrc EQ 0.
            _sem_frete = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'CFR'.
      IF vj_1bnflin-refkey(10) IS NOT INITIAL.
        SELECT SINGLE *
          FROM zsdt0001 INTO wl_zsdt0001
         WHERE fatura_prod = vj_1bnflin-refkey(10).

        IF ( sy-subrc EQ 0 ) AND
           ( wl_zsdt0001-id_interface = '48' OR
             wl_zsdt0001-id_interface = '51' OR
             wl_zsdt0001-id_interface = '52' ).
          _sem_frete = 'X'.
        ENDIF.
      ENDIF.

      IF in_doc-bukrs EQ '0035'.
        _sem_frete = 'X'.
      ENDIF.

  ENDCASE.


  "Codigo Antes  NT2021.004 v1.21 - US 78246 - Ini
*  IF _sem_frete IS NOT INITIAL.
*    out_header-t_modfrete = '9'.
*  ELSEIF out_header-finnfe EQ '2'.
*    CASE in_doc-inco1.
*      WHEN 'FOB'.
*        out_header-t_modfrete = '1'.
*      WHEN OTHERS.
*        out_header-t_modfrete = '0'.
*    ENDCASE.
*  ELSE.
*    CASE in_doc-inco1.
*      WHEN: 'CIF' OR 'CPT'.
*        out_header-t_modfrete = '0'.
*      WHEN: 'FOB'.
*        out_header-t_modfrete = '1'.
*      WHEN: 'FCA'.
*        out_header-t_modfrete = '9'.
*      WHEN OTHERS.
*        out_header-t_modfrete = '1'.
*    ENDCASE.
*  ENDIF.
*
**==================================================================================================*
** "Informações da transportadora
**==================================================================================================*
*  me->set_dados_transportadora( CHANGING i_xml_header = out_header ).
  "Codigo Antes  NT2021.004 v1.21 - US 78246 - Fim


  "Codigo depois NT2021.004 v1.21 - US 78246  Ini
  DATA(lwa_lfa1_transp) =  me->get_dados_transportadora( ).
  DATA(lva_inf_dados_transp_xml) = abap_true.

  DATA: lva_cnpj_raiz_emitente     TYPE c LENGTH 8,
        lva_cnpj_raiz_destinatario TYPE c LENGTH 8,
        lva_cpf_destinatario       TYPE c LENGTH 11.

  CLEAR: lva_cnpj_raiz_emitente,  lva_cnpj_raiz_destinatario, lva_cpf_destinatario.

  lva_cnpj_raiz_emitente = <fs_xmlh>-c_cnpj(8).

  IF ( <fs_xmlh>-e_cnpj IS NOT INITIAL ) AND ( <fs_xmlh>-e_cnpj NE '00000000000000' ).
    lva_cnpj_raiz_destinatario = <fs_xmlh>-e_cnpj(8).
  ELSEIF ( <fs_xmlh>-e_cpf IS NOT INITIAL ) AND ( <fs_xmlh>-e_cpf NE '00000000000' ).
    lva_cpf_destinatario       = <fs_xmlh>-e_cpf.
  ENDIF.

  IF _sem_frete IS NOT INITIAL.
    out_header-t_modfrete    = '9'.
    lva_inf_dados_transp_xml = abap_false.
  ELSE.
    CASE in_doc-inco1.
      WHEN: 'CIF' OR 'CPT' OR 'FOB'.

        IF lwa_lfa1_transp IS INITIAL.

          IF in_doc-inco1 EQ 'FOB'.
            out_header-t_modfrete = '1'.
          ELSE.
            out_header-t_modfrete = '0'.
          ENDIF.

          lva_inf_dados_transp_xml = abap_false.

        ELSEIF lwa_lfa1_transp-stcd1 IS NOT INITIAL.

          IF ( ( lwa_lfa1_transp-stcd1(8) NE  lva_cnpj_raiz_emitente     ) AND  "CNPJ Raiz Agente Frete <> Emitente Merc.
               ( lwa_lfa1_transp-stcd1(8) NE  lva_cnpj_raiz_destinatario ) ).   "CNPJ Raiz Agente Frete <> Destinatario Merc..

            "US 166904 CS2025000178 Erro tomdo serv de Frete terc oper transf PSA - INI
*            IF in_doc-inco1 EQ 'FOB'.
*              out_header-t_modfrete = '1'.
*            ELSE.
*              out_header-t_modfrete = '0'.
*            ENDIF.

            IF in_doc-inco1 EQ 'FOB'.
              out_header-t_modfrete = '1'.
            ELSEIF in_doc-inco1 EQ 'CIF'.
              out_header-t_modfrete = '0'.
            ELSEIF in_doc-inco1 EQ 'CPT'.

              DATA: it_set_lines_basic TYPE STANDARD TABLE OF rgsbv INITIAL SIZE 0.

              FREE: it_set_lines_basic.

              CALL FUNCTION 'G_SET_FETCH'
                EXPORTING
                  setnr           = 'MAGGI_CFOP_TRANSFERENCIA'
                TABLES
                  set_lines_basic = it_set_lines_basic
                EXCEPTIONS
                  no_authority    = 1
                  set_is_broken   = 2
                  set_not_found   = 3
                  OTHERS          = 4.

              IF sy-subrc = 0.
                IF it_set_lines_basic IS NOT INITIAL.
                  READ TABLE it_set_lines_basic INTO DATA(wa_set_lines_basic) WITH KEY from = vj_1bnflin-cfop.
                  IF sy-subrc = 0.
                    out_header-t_modfrete = '1'.
                  ELSE.
                    out_header-t_modfrete = '0'.
                  ENDIF.
                ENDIF.
              ELSE.
                out_header-t_modfrete = '0'.
              ENDIF.
            ELSE.
              out_header-t_modfrete = '0'.
            ENDIF.

            "166904 CS2025000178 Erro tomdo serv de Frete terc oper transf PSA - Fim

          ELSEIF ( lwa_lfa1_transp-stcd1(8) EQ  lva_cnpj_raiz_emitente ).  "CNPJ Raiz Agente Frete = Emitente Merc.

            out_header-t_modfrete = '3'.
            lva_inf_dados_transp_xml = abap_false.

          ELSEIF ( lwa_lfa1_transp-stcd1(8) EQ  lva_cnpj_raiz_destinatario ). "CNPJ Raiz Agente Frete = Destinatario Merc..

            out_header-t_modfrete = '4'.
            lva_inf_dados_transp_xml = abap_false.

          ENDIF.


        ELSEIF lwa_lfa1_transp-stcd2 IS NOT INITIAL.

          IF ( lwa_lfa1_transp-stcd2 EQ  lva_cpf_destinatario ). "CPF Agente Frete = Destinatario Merc..
            out_header-t_modfrete = '4'.
            lva_inf_dados_transp_xml = abap_false.
          ELSE.
            IF in_doc-inco1 EQ 'FOB'.
              out_header-t_modfrete = '1'.
            ELSE.
              out_header-t_modfrete = '0'.
            ENDIF.
          ENDIF.

        ENDIF.

      WHEN: 'FCA'.
        out_header-t_modfrete    = '9'.
        lva_inf_dados_transp_xml = abap_false.
      WHEN OTHERS.
        out_header-t_modfrete    = '1'.
        lva_inf_dados_transp_xml = abap_false.
    ENDCASE.
  ENDIF.

*==================================================================================================*
* "Informações da transportadora
*==================================================================================================*
* IF lva_inf_dados_transp_xml EQ abap_true.   "*-Equalização RISE x PRD - 28.08.2023 - JT
  IF lva_inf_dados_transp_xml EQ abap_true OR wa_vbak-auart = 'ZBIO' OR vj_1bnflin-matkl = '700400'. "WPP - 04/08/23  -Informar transportador sempre caso OV for do tio ZBIO. GCOSTA - 17/01/24 - US128313 - envia transp qdo matkl=700400
    me->set_dados_transportadora( CHANGING i_xml_header = out_header ).
  ELSE.
    me->set_dados_transportadora( EXPORTING i_clear      = abap_true
                                  CHANGING  i_xml_header = out_header ).
  ENDIF.
  "Codigo depois  NT2021.004 v1.21 - US 78246  - Fim


  "Forma de Pagamento"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " ALINHAR COM FISCAL """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*  IF ( IN_DOC-CGC IS NOT INITIAL AND IN_DOC-STAINS  EQ 'ISENTO' ) OR IN_DOC-CPF IS NOT INITIAL.
*    OUT_HEADER-IND_IEDEST  = '9'.
*    OUT_HEADER-IND_FINAL   = '1'.
*
*    IF <FS_XMLH_310> IS ASSIGNED.
*      <FS_XMLH_310>-IND_IEDEST = '9'.
*      <FS_XMLH_310>-IND_FINAL  = '1'.
*    ENDIF.
*
*  ENDIF.

  "Indicador Consumidor final
  IF ( <fs_xmlh>-e1_ie IS INITIAL ).
    "Não Contribuinte, que pod eou não possuir inscrição estadual no cadastro de Contribuinte do ICMS
    out_header-ind_final     = '1'.
    <fs_xmlh_310>-ind_final  = '1'.
  ELSE.
    "Atribuir o valor 0 - Normal (Layout da Nf-e 3.1)
    out_header-ind_final     = '0'.
    <fs_xmlh_310>-ind_final  = '0'.
  ENDIF.

  "Indicador IE Destinario
  IF ( <fs_xmlh>-e1_ie EQ 'ISENTO' ).
    "Contribuinte isento de Inscrição no cadastro de contribuinte do ICMS.
    <fs_xmlh_310>-ind_iedest = '2'.
    <fs_xmlh_310>-ind_iedest = '2'.

  ELSEIF ( <fs_xmlh>-e1_ie IS INITIAL ).

    "Não Contribuinte, que pod eou não possuir inscrição estadual no cadastro de Contribuinte do ICMS
    <fs_xmlh_310>-ind_iedest = '9'.
    <fs_xmlh_310>-ind_iedest = '9'.

  ELSEIF NOT ( <fs_xmlh>-e1_ie IS INITIAL AND <fs_xmlh>-e1_ie NE 'ISENTO').

    "Contribuinte ICMS (informar a IE do Destinatário).
    <fs_xmlh_310>-ind_iedest = '1'.
    <fs_xmlh_310>-ind_iedest = '1'.
  ENDIF.

  ASSIGN ('(SAPLJ_1B_NFE)XMLH_BADI') TO <fs_xmlh_badi>.

  <fs_xmlh_badi>-id_dest    = <fs_xmlh_310>-id_dest.
  <fs_xmlh_badi>-ind_final  = <fs_xmlh_310>-ind_final.
  <fs_xmlh_badi>-ind_pres   = <fs_xmlh_310>-ind_pres.
  <fs_xmlh_badi>-ind_iedest = <fs_xmlh_310>-ind_iedest.

  out_header-indpag = '2'.
  " indicativo do intermediador: indIntermed
  <fs_xmlh_badi>-indintermed = 0.
  IF <fs_xmlh_badi>-ind_pres NE '2' AND
    <fs_xmlh_badi>-ind_pres NE '3' AND
    <fs_xmlh_badi>-ind_pres NE '4' AND
    <fs_xmlh_badi>-ind_pres NE '9'.
    CLEAR : <fs_xmlh_badi>-indintermed.
  ENDIF.

  CLEAR: wa_zsdt0231.

  SELECT SINGLE * INTO @wa_zsdt0231
    FROM zsdt0231
   WHERE docnum EQ @in_doc-docnum.

  IF ch_payment[] IS INITIAL.

    IF wa_zsdt0231 IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_zsdt0233)
        FROM zsdt0233
       WHERE obj_key EQ @wa_zsdt0231-obj_key.

      LOOP AT it_zsdt0233 INTO DATA(wa_zsdt0233).
        APPEND VALUE #(
           t_pag      = wa_zsdt0233-tpag
           v_pag      = wa_zsdt0233-vpag
           cnpj       = wa_zsdt0233-cnpj
           t_band     = wa_zsdt0233-tband
           c_aut      = wa_zsdt0233-caut
           docnum     = in_doc-docnum
           ) TO ch_payment.
      ENDLOOP.

    ELSE.

      CASE out_header-finnfe.
        WHEN '4' OR '3'.
          APPEND VALUE #(
            t_pag      = '90'
            docnum     = in_doc-docnum
            "TP_INTEGRA = '2'
            ) TO ch_payment.
        WHEN OTHERS.

          SELECT SINGLE *
            FROM setleaf INTO @DATA(lwa_nfe_tpag)
           WHERE setname EQ 'XML_NFE_VALUE_TPAG'.

          IF ( sy-subrc EQ 0 ) AND ( lwa_nfe_tpag-valfrom IS NOT INITIAL ).
            APPEND VALUE #(
            t_pag      = lwa_nfe_tpag-valfrom
            v_pag      = in_doc-nftot
            docnum     = in_doc-docnum
            "TP_INTEGRA = '2'
            ) TO ch_payment.
          ELSE.
            APPEND VALUE #(
              t_pag      = '99'
              v_pag      = in_doc-nftot
              docnum     = in_doc-docnum
              x_pag      = 'Outros'
              "TP_INTEGRA = '2'
              ) TO ch_payment.
          ENDIF.

      ENDCASE.

    ENDIF.


  ENDIF.

  IF wa_zsdt0231-finnfe IS NOT INITIAL.
    <fs_xmlh>-finnfe = wa_zsdt0231-finnfe.
    <fs_xmlh_badi>-finnfe = wa_zsdt0231-finnfe.
  ENDIF.

  out_header-vdesc = <fs_xmlh>-s1_vdesc.

  "Dados Cobrança Fatura...
  IF out_header-vdesc IS NOT INITIAL.
    out_header-vorig = <fs_xmlh>-s1_vprod.
    out_header-vliq  = out_header-vorig - out_header-vdesc.
  ENDIF.

  TRY .

      "Emitente:
      CLEAR: <fs_xmlh>-c1_im.

      <fs_xmlh>-c1_xlgr = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xlgr ) ) ).
      <fs_xmlh>-c1_xcpl = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xcpl ) ) ).
      <fs_xmlh>-c1_xbairro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xbairro ) ) ).
      <fs_xmlh>-c1_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xmun ) ) ).
      <fs_xmlh>-c_xfant = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c_xfant ) ) ).
      <fs_xmlh>-c1_xpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xpais ) ) ) ).
      <fs_xmlh>-c1_fone = zcl_string=>replace( EXPORTING i_str = CONV #( <fs_xmlh>-c1_fone )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).

      <fs_xmlh>-t1_xnome = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-t1_xnome ) ) ).
      <fs_xmlh>-t1_xend = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-t1_xend ) ) ).
      <fs_xmlh>-t1_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-t1_xmun ) ) ).

      out_header-c1_fone = zcl_string=>replace( EXPORTING i_str = CONV #( out_header-c1_fone )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).

      <fs_xmlh>-e_xnome = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e_xnome ) ) ).
      <fs_xmlh>-e1_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_xmun ) ) ).
      <fs_xmlh>-e1_xlgr = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_xlgr ) ) ).
      <fs_xmlh>-e1_xcpl = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_xcpl ) ) ).
      <fs_xmlh>-e1_xbairro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_xbairro ) ) ).
      <fs_xmlh>-e1_xpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_xpais ) ) ) ).
      <fs_xmlh>-e1_fone = zcl_string=>replace( EXPORTING i_str = CONV #( <fs_xmlh>-e1_fone )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).

      out_header-c1_cpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-c1_cpais ) ) ) ).
      out_header-e1_cpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-e1_cpais ) ) ) ).

      out_header-c_xfant = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-c_xfant ) ) ).
      out_header-f_xlgr  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xlgr ) ) ).
      out_header-f_xcpl  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xcpl ) ) ).
      out_header-f_xbairro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xbairro ) ) ).
      out_header-f_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xmun ) ) ).
      out_header-f_xnome = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xnome ) ) ).
      out_header-f_xpais = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xpais ) ) ).
      out_header-f_fone  = zcl_string=>replace( EXPORTING i_str = CONV #( out_header-f_fone )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).

      out_header-g_xlgr = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_xlgr ) ) ).
      out_header-g_xcpl = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_xcpl ) ) ).
      out_header-g_xbairro  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_xbairro ) ) ).
      out_header-g_xmun  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_xmun ) ) ).
      out_header-g_xnome  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_xnome ) ) ).
      out_header-g_fone  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_fone ) ) ).
      out_header-g_email  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-g_email ) ) ).
      out_header-g_fone  = zcl_string=>replace( EXPORTING i_str = CONV #( out_header-g_fone )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).

      out_header-infadfisco = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-infadfisco ) ) ).
      out_header-infadfisco_v2 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = out_header-infadfisco_v2 ) ).
      out_header-infcomp = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = out_header-infcomp ) ).
      out_header-xlocembarq = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-xlocembarq ) ) ).
      out_header-xped = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-xped ) ) ).
      out_header-xcont = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-xcont ) ) ).
      out_header-c_xcampo = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-c_xcampo ) ) ).
      out_header-c_xtexto = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-c_xtexto ) ) ).
      out_header-f_xcampo = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xcampo ) ) ).
      out_header-f_xtexto = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-f_xtexto ) ) ).
      out_header-nproc = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-nproc ) ) ).
      out_header-indproc = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-indproc ) ) ).
      out_header-email = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-email ) ) ).
      out_header-natop = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_header-natop ) ) ).

    CATCH zcx_error.

  ENDTRY.

  "CS2022000911 Ajustes emissão NFe / AOENNING
  "Somar totais importação.
  FREE: it_znota_import_ii.
  SELECT *
  INTO TABLE it_znota_import_ii
  FROM znota_import_ii
 WHERE docnum  EQ in_doc-docnum.

  IF it_znota_import_ii IS NOT INITIAL.
    LOOP AT it_znota_import_ii INTO DATA(ws_import).
      ADD ws_import-o_vii TO out_header-s1_vii.
    ENDLOOP.
  ENDIF.
  "CS2022000911 Ajustes emissão NFe / AOENNING

  "Utilizado para GRC

  " Atualização Data Emissão HEMI
  IF ( in_doc-hemi IS INITIAL OR out_header-hemi IS INITIAL ) AND in_doc-cretim IS NOT INITIAL.

    out_header-hemi = in_doc-cretim.

  ENDIF.
*  <<<<NOTA TECNICA 2024.003 - LP
  SELECT SINGLE * INTO @DATA(wa_guiatrans)
    FROM zagroguiatrans
  WHERE docnum = @in_doc-docnum.

  out_header-tpguia = wa_guiatrans-tpguia.
  out_header-ufguia = wa_guiatrans-ufguia.
  out_header-serieguia = wa_guiatrans-serieguia.
  out_header-nguia = wa_guiatrans-nguia.
* >>>>
ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_import.

    DATA: it_j_1bnflin       TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
          wa_j_1bnflin       TYPE j_1bnflin,
          wa_j_1bnfdoc       TYPE j_1bnfdoc,
          it_znota_import    TYPE TABLE OF znota_import,
          it_znota_import_ad TYPE TABLE OF znota_import_ad,
          wa_znota_import    TYPE znota_import,
          wa_znota_import_ad TYPE znota_import_ad,
          wa_import_di       TYPE j_1bnfe_s_badi_di_310,
          wa_import_adi      TYPE j_1bnfe_s_badi_adi_310,
          v_importacao       TYPE c.

    CHECK in_header-docnum IS NOT INITIAL.

    CLEAR v_importacao.

    SELECT * INTO TABLE it_j_1bnflin
      FROM j_1bnflin
     WHERE docnum EQ in_item-docnum
       AND itmnum EQ in_item-itmnum.

    LOOP AT it_j_1bnflin INTO wa_j_1bnflin.
      IF wa_j_1bnflin-cfop(1) EQ '3'.
        v_importacao = 'X'.
      ENDIF.
    ENDLOOP.

    CHECK v_importacao IS NOT INITIAL .

    SELECT SINGLE * INTO wa_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ in_item-docnum.

    CHECK sy-subrc IS INITIAL.

    SELECT * INTO TABLE it_znota_import
      FROM znota_import
     WHERE docnum  EQ in_item-docnum
       AND itmnum  EQ in_item-itmnum.

    IF sy-subrc IS INITIAL.
      SELECT * INTO TABLE it_znota_import_ad
        FROM znota_import_ad
         FOR ALL ENTRIES IN it_znota_import
       WHERE docnum  EQ it_znota_import-docnum
         AND itmnum  EQ it_znota_import-itmnum
         AND itdidoc EQ it_znota_import-itdidoc.
    ENDIF.

    LOOP AT it_znota_import INTO wa_znota_import.

      CLEAR: wa_import_di.

      wa_import_di-docnum             = wa_znota_import-docnum.
      wa_import_di-itmnum             = wa_znota_import-itmnum.
     " wa_import_di-ndi                = wa_znota_import-ndi.
      wa_import_di-NDI_400_2                = wa_znota_import-ndi.
      wa_import_di-ddi                = wa_znota_import-ddi.
      wa_import_di-xlocdesemb         = wa_znota_import-xlocdesemb.
      wa_import_di-ufdesemb           = wa_znota_import-ufdesemb.
      wa_import_di-ddesemb            = wa_znota_import-ddesemb.
      wa_import_di-cexportador        = wa_znota_import-cexportador.
      wa_import_di-intermediate_mode  = wa_znota_import-tpintermedio.
      wa_import_di-transport_mode     = wa_znota_import-tpviatransp.
      wa_import_di-maritime_freight   = wa_znota_import-vafrmm. "CS2022000911 Ajustes emissão NFe / Anderson Oenning


*    o_out_item-o_ii       = 'X'.
*    o_out_item-o_vbc      = 0.
*    o_out_item-o_vdespadu = 0.
*    o_out_item-o_vii      = 0.
*    o_out_item-o_viof     = 0.
*
*    select single *
*      into wa_znota_import_ii
*      from znota_import_ii
*     where docnum  eq i_docnum
*       and itmnum  eq i_itmnum.
*
*    if sy-subrc is initial.
*      o_out_item-o_vbc      = wa_znota_import_ii-o_vbc.
*      o_out_item-o_vdespadu = wa_znota_import_ii-o_vdespadu.
*      o_out_item-o_vii      = wa_znota_import_ii-o_vii.
*      o_out_item-o_viof     = wa_znota_import_ii-o_viof.
*    endif.


      APPEND wa_import_di TO out_importing_di.

      LOOP AT it_znota_import_ad INTO wa_znota_import_ad WHERE docnum  EQ wa_znota_import-docnum
                                                           AND itmnum  EQ wa_znota_import-itmnum
                                                           AND itdidoc EQ wa_znota_import-itdidoc.
        CLEAR: wa_import_adi.

        wa_import_adi-id           = wa_znota_import-itdidoc.
        wa_import_adi-ndi          = wa_znota_import-ndi.
        wa_import_adi-NDI_400_2          = wa_znota_import-ndi.
        wa_import_adi-n_adicao     = wa_znota_import_ad-nr_adicao.
        wa_import_adi-n_seq_adic   = wa_znota_import_ad-nr_seq_adicao.
        wa_import_adi-N_SEQ_ADIC_400_2   = wa_znota_import_ad-nr_seq_adicao.
        wa_import_adi-c_fabricante = wa_znota_import_ad-cfabricante.
        wa_import_adi-v_desc_di    = wa_znota_import_ad-vlr_desconto.
       " wa_import_adi-n_draw       = wa_znota_import_ad-nr_drawback.
        wa_import_adi-N_DRAW_400_2      = wa_znota_import_ad-nr_drawback.

        APPEND wa_import_adi TO out_importing_adi.

      ENDLOOP.

    ENDLOOP.



  ENDMETHOD.


METHOD if_ex_cl_nfe_print~fill_item.

  DATA: it_j1b_nf_xml_badi_item TYPE z_j1b_nf_xml_badi_item_t.

  DATA: vl_textos        TYPE string,
        vl_classif       TYPE string,
        tl_lines         TYPE TABLE OF tline,
        tl_lips          TYPE TABLE OF znfe_lips,
        tl_lips_aux      TYPE TABLE OF znfe_lips,
        t_vbrp           TYPE TABLE OF znfe_vbrp,
        sl_vbrp          TYPE znfe_vbrp,
        sl_likp	         TYPE znfe_likp,
        t_vbak           TYPE TABLE OF znfe_vbak,
        sl_vbak          TYPE znfe_vbak,
        sl_vbap          TYPE znfe_vbap,
        sl_vbkd          TYPE znfe_vbkd,
        t_vbap           TYPE TABLE OF znfe_vbap,
        t_vbkd           TYPE TABLE OF znfe_vbkd,
        vl_id	           TYPE thead-tdid,
        vl_language      TYPE thead-tdspras,
        vl_name          TYPE thead-tdname,
        vl_object        TYPE thead-tdobject,
        sl_tline         TYPE tline,
        zona_franca      TYPE c LENGTH 1,
        icms_desonerado  TYPE c LENGTH 1,
        ck_lei_fiscal    TYPE c LENGTH 1,
        icms_nao_tribut  TYPE c LENGTH 1,
        vl_base_n_tribut TYPE j_1bnfstx-basered1,
        vl_aliq_n_tribut TYPE j_1bnfstx-rate,
        vl_perc_n_tribut TYPE p DECIMALS 6,
        valor_deconto    TYPE j_1bexcbas,
        sl_tax           TYPE j_1bnfstx,
        t_zsdt0006       TYPE TABLE OF zsdt0006,
        wa_zsdt0006      TYPE zsdt0006,
        sl_lips          TYPE znfe_lips,
        sl_lips_aux      TYPE znfe_lips,
        vl_cst_icms      TYPE c LENGTH 2,
**<<<------" User Story 151256 - AOENNING
        vg_j_1bnflin     TYPE j_1bnflin,
        vg_mara          TYPE mara,
        vg_zmmt0107      TYPE zmmt0107,
**<<<------" User Story 151256 - AOENNING
        vl_lips          TYPE string,
        vl_lips2         TYPE string,
        vl_lips3         TYPE string,        "*-Equalização RISE x PRD - 21.08.2023 - JT
        v_off            TYPE i,             "*-Equalização RISE x PRD - 21.08.2023 - JT
        lv_peso_tot      TYPE p DECIMALS 2.  "*-Equalização RISE x PRD - 21.08.2023 - JT

  TYPES: ty_cst_not_deson TYPE RANGE OF char2.
  DATA: r_cst_not_deson TYPE ty_cst_not_deson.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '51' ) TO r_cst_not_deson.

  DATA: wa_zsdt0098 TYPE zsdt0098.
  DATA: var_rate    TYPE j_1btxrate.

  DATA: it_regio_deson TYPE TABLE OF regio,
        wa_regio_deson TYPE regio.

  FIELD-SYMBOLS: <item>           TYPE j1b_nf_xml_item,
                 <xmlh_310>       TYPE j_1bnfe_s_layout_310,      "Utilizado para GRC
                 <it_1bnfstx_tab> TYPE j_1bnfstx_tab,
                 <xmlh>           TYPE j1b_nf_xml_header.

  MOVE-CORRESPONDING in_xml_item TO out_item.

  CLEAR: vl_cst_icms.

  CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
    EXPORTING
      input  = in_lin-taxsit
    IMPORTING
      output = vl_cst_icms.

  IF vl_cst_icms NOT IN r_cst_not_deson.
    out_item-vicmsdeson = in_lin-vicmsdeson.      "Utilizado para GRC
    out_item-motdeson   = in_lin-motdesicms.      "Utilizado para GRC
  ENDIF.

  ASSIGN ('(SAPLJ_1B_NFE)XMLI')          TO <item>.
  ASSIGN ('(SAPLJ_1B_NFE)XMLH_310')      TO <xmlh_310>.       "Utilizado para GRC
  ASSIGN ('(SAPLJ_1B_NFE)XMLH')          TO <xmlh>.
  ASSIGN ('(SAPLJ_1B_NFE)WK_ITEM_TAX[]') TO <it_1bnfstx_tab>. "Utilizado para GRC

  CLEAR: zona_franca.

  CALL METHOD me->select_vbrp
    EXPORTING
      p_refkey = in_lin-refkey
      p_posnr  = in_lin-refitm
    IMPORTING
      t_vbrp   = t_vbrp.

  READ TABLE t_vbrp INTO sl_vbrp INDEX 1.

  CALL METHOD me->select_vbak
    EXPORTING
      p_aubel = sl_vbrp-aubel
    IMPORTING
      t_vbak  = t_vbak.

  READ TABLE t_vbak INTO sl_vbak INDEX 1.

  CALL METHOD me->select_vbap
    EXPORTING
      p_vbeln = sl_vbak-vbeln
      p_posnr = in_lin-refitm
    IMPORTING
      t_vbap  = t_vbap.

  READ TABLE t_vbap INTO sl_vbap INDEX 1.

  CLEAR: sl_vbkd.
  CALL METHOD me->select_vbkd
    EXPORTING
      p_vbeln = sl_vbak-vbeln
      p_posnr = in_lin-refitm
    IMPORTING
      t_vbkd  = t_vbkd.

  READ TABLE t_vbkd INTO sl_vbkd INDEX 1.

  CLEAR: vl_textos.

  IF NOT sl_vbak-auart IS INITIAL.

    CALL METHOD me->select_zsdt0006
      EXPORTING
        p_auart    = sl_vbak-auart
      IMPORTING
        t_zsdt0006 = t_zsdt0006.

    IF NOT t_zsdt0006[] IS INITIAL.

      READ TABLE t_zsdt0006 INTO wa_zsdt0006 WITH KEY branch = in_doc-branch.
      IF ( sy-subrc NE 0 ).
        READ TABLE t_zsdt0006 INTO wa_zsdt0006 WITH KEY branch = space.
      ENDIF.

      IF NOT wa_zsdt0006-tx_item_ov_itm_ IS INITIAL.
*       TEXTOS DE ITENS ORDENS DE VENDA.
        REFRESH tl_lines.
        vl_id       = '0001'.
        vl_language = 'PT'.
        vl_object   = 'VBBP'.
        CONCATENATE sl_vbak-vbeln
                    sl_vbap-posnr
               INTO vl_name.

        REFRESH tl_lines.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = vl_id
            language                = vl_language
            name                    = vl_name
            object                  = vl_object
          TABLES
            lines                   = tl_lines
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF NOT tl_lines[] IS INITIAL.

          LOOP AT tl_lines INTO sl_tline.
            REPLACE ALL OCCURRENCES OF REGEX '[<>()]' IN sl_tline WITH '' IGNORING CASE.
            CONCATENATE vl_textos sl_tline-tdline INTO vl_textos SEPARATED BY space.
            CLEAR sl_tline.
          ENDLOOP.
        ENDIF.

      ENDIF.

      IF wa_zsdt0006-tx_item_rem IS NOT INITIAL. "Texto Item Remessa

        CLEAR: sl_likp.

        " Seleciona Dados Relevântes
        CALL METHOD me->seleciona_dados
          EXPORTING
            p_docnum = in_doc-docnum
            p_itmnum = in_lin-itmnum
            p_refkey = in_lin-refkey
          IMPORTING
            t_lips   = tl_lips
            s_likp   = sl_likp.

        LOOP AT tl_lips INTO sl_lips WHERE matnr EQ in_lin-matnr
                                       AND charg EQ in_lin-charg.

          SELECT SINGLE mtart
            FROM mara INTO @DATA(v_mtart)
           WHERE matnr = @sl_lips-matnr.

          CONCATENATE sl_likp-vstel '-' v_mtart INTO DATA(_centro_mtart).

          SELECT SINGLE *
            FROM setleaf INTO @DATA(wl_set_tx_mat_nf)
           WHERE setname = 'MAGGI_CENTRO_TX_MAT_NF'
             AND valfrom = @_centro_mtart.

          IF ( sy-subrc = 0 ) AND ( _centro_mtart IS NOT INITIAL  ).

*           TEXTOS DO MATERIAL.
            REFRESH tl_lines.
            vl_id       = '0001'.
            vl_language = 'PT'.
            vl_object   = 'MVKE'.
            CONCATENATE sl_lips-matnr
                        sl_likp-vkorg
                        sl_lips-vtweg
                   INTO vl_name.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = vl_id
                language                = vl_language
                name                    = vl_name
                object                  = vl_object
              TABLES
                lines                   = tl_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

            IF NOT tl_lines[] IS INITIAL.
              LOOP AT tl_lines INTO sl_tline.
                CONCATENATE vl_textos
                            sl_tline-tdline
                       INTO vl_textos SEPARATED BY space.
                CLEAR sl_tline.
              ENDLOOP.
            ENDIF.

          ELSE.

*           TEXTOS DE ITENS REMESSA.
            REFRESH tl_lines.
            vl_id       = '0001'.
            vl_language = 'PT'.
            vl_object   = 'VBBP'.
            CONCATENATE sl_lips-vbeln
                        sl_lips-posnr
                   INTO vl_name.

            REFRESH tl_lines.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = vl_id
                language                = vl_language
                name                    = vl_name
                object                  = vl_object
              TABLES
                lines                   = tl_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

            IF NOT tl_lines[] IS INITIAL.

              LOOP AT tl_lines INTO sl_tline.
                REPLACE ALL OCCURRENCES OF REGEX '[<>()]' IN sl_tline WITH '' IGNORING CASE.
                CONCATENATE vl_textos sl_tline-tdline INTO vl_textos SEPARATED BY space.
                CLEAR sl_tline.
              ENDLOOP.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF. "IF WA_ZSDT0006-TX_ITEM_REM IS NOT INITIAL. "Texto Item Remessa

      IF wa_zsdt0006-tx_item_material IS NOT INITIAL. "Texto Item Material

        CLEAR: sl_likp, tl_lips[].

        " Seleciona Dados Relevântes
        CALL METHOD me->seleciona_dados
          EXPORTING
            p_docnum = in_doc-docnum
            p_itmnum = in_lin-itmnum
            p_refkey = in_lin-refkey
          IMPORTING
            t_lips   = tl_lips
            s_likp   = sl_likp.

        LOOP AT tl_lips INTO sl_lips WHERE matnr EQ in_lin-matnr
                                       AND charg EQ in_lin-charg.

          CALL METHOD zcl_pp_services=>get_text_sd
            EXPORTING
              matnr = sl_vbap-matnr   " Nº do material
              werks = sl_vbap-werks   " Centro
            IMPORTING
              text  = tl_lines.       " Categoria de tabela para estrutura Tline

**         TEXTOS DO MATERIAL.
*          REFRESH tl_lines.
*          vl_id       = '0001'.
*          vl_language = 'PT'.
*          vl_object   = 'MVKE'.
*          CONCATENATE sl_lips-matnr
*                      sl_likp-vkorg
*                      sl_lips-vtweg
*                 INTO vl_name.
*
*          CALL FUNCTION 'READ_TEXT'
*            EXPORTING
*              id                      = vl_id
*              language                = vl_language
*              name                    = vl_name
*              object                  = vl_object
*            TABLES
*              lines                   = tl_lines
*            EXCEPTIONS
*              id                      = 1
*              language                = 2
*              name                    = 3
*              not_found               = 4
*              object                  = 5
*              reference_check         = 6
*              wrong_access_to_archive = 7
*              OTHERS                  = 8.

          IF NOT tl_lines[] IS INITIAL.
            LOOP AT tl_lines INTO sl_tline.
              CONCATENATE vl_textos
                          sl_tline-tdline
                     INTO vl_textos SEPARATED BY space.
              CLEAR sl_tline.
            ENDLOOP.
          ENDIF.

        ENDLOOP.

      ENDIF. "IF WA_ZSDT0006-TX_ITEM_REM IS NOT INITIAL. "Texto Item Material

      DATA: str_lfimg(13) TYPE c,
            regis         TYPE sy-tabix,
            lv_categ_sem1 TYPE string,  "*-US190739-15.09.2025-#190739-JT-inicio
            lv_categ_sem2 TYPE string.  "*-US190739-15.09.2025-#190739-JT-inicio

      IF NOT ( wa_zsdt0006-num_lote_item IS INITIAL ).

        CLEAR: sl_lips.

        IF wa_zsdt0006-tx_item_rem IS INITIAL. "Verifica se já fez essa seleção no processo anterior para não fazer denovo
          " Seleciona Dados Relevântes
          CALL METHOD me->seleciona_dados
            EXPORTING
              p_docnum = in_doc-docnum
              p_itmnum = in_lin-itmnum
              p_refkey = in_lin-refkey
            IMPORTING
              t_lips   = tl_lips.
        ENDIF.

        DELETE tl_lips WHERE charg IS INITIAL.

        LOOP AT tl_lips INTO sl_lips WHERE matnr EQ in_lin-matnr
                                       AND charg EQ in_lin-charg.
          regis = sy-tabix.

          CONCATENATE 'LOTE:' sl_lips-charg INTO vl_lips SEPARATED BY space.

          CALL METHOD me->function_classification
            EXPORTING
              p_matnr  = sl_lips-matnr
              p_charg  = sl_lips-charg
            IMPORTING
              p_textos = vl_lips2.

          IF vl_lips2 IS NOT INITIAL.

*-US190739-15.09.2025-#190739-JT-inicio
            CALL METHOD me->recupera_atributo
              EXPORTING
                iv_nome_attr = 'CATEGORIA SEMENTE'
                iv_matnr     = sl_lips-matnr
                iv_charg     = sl_lips-charg
              IMPORTING
                ev_valor     = DATA(lv_cat_sem).

            IF lv_cat_sem IS NOT INITIAL.
              SELECT SINGLE nome
                INTO @DATA(_nome_categ_sem)
                FROM zsdt0199
               WHERE id_cat_sementes = @lv_cat_sem.

              IF sy-subrc = 0.
                lv_categ_sem1 = 'CATEGORIA SEMENTE:' && lv_cat_sem.
                lv_categ_sem2 = 'CATEGORIA SEMENTE:' && _nome_categ_sem.
                REPLACE ALL OCCURRENCES OF lv_categ_sem1 IN vl_lips2 WITH lv_categ_sem2.
              ENDIF.
            ENDIF.
*-US190739-15.09.2025-#190739-JT-fim

            IF sl_lips-lfimg IS NOT INITIAL.
*-Equalização RISE x PRD - 21.08.2023 - JT - inicio
              "US:112406 -LP
              FIND 'BAG:' IN vl_lips2 MATCH OFFSET v_off.

              IF  sy-subrc  EQ 0.

                DATA(lv_bag) = vl_lips2+v_off.
                REPLACE ALL OCCURRENCES OF REGEX '([[:alpha:]])' IN lv_bag WITH space.
                REPLACE ALL OCCURRENCES OF SUBSTRING ':' IN lv_bag WITH space.
                TRANSLATE lv_bag USING '. '.   "<< RIM-SKM-IR146464-02.08.23
                TRANSLATE lv_bag USING ',.'.
                CONDENSE lv_bag NO-GAPS.


                CLEAR: str_lfimg.
                str_lfimg = sl_lips-lfimg.
                TRANSLATE str_lfimg USING '.,'.
                CONDENSE str_lfimg NO-GAPS.
                CONCATENATE vl_lips2 'QTD:' str_lfimg INTO vl_lips2 SEPARATED BY space.

                CLEAR: str_lfimg.
***Stefanini - IR230799 - 31/03/2025 - GGARAUJO1 - Inicio de alteração
***Stefanini - IR230799 - 28/03/2025 - GGARAUJO1 - Inicio de alteração
*
*                FIND '.' in lv_bag MATCH OFFSET DATA(lv_casas_decimais).
*
*                lv_casas_decimais = lv_casas_decimais + 2.
*
*                lv_bag = lv_bag(lv_casas_decimais).
*
****Stefanini - IR230799 - 28/03/2025 - GGARAUJO1 - Fim de alteração

                CALL METHOD me->recupera_atributo
                  EXPORTING
                    iv_nome_attr = 'BAG'
                    iv_matnr     = sl_lips-matnr
                    iv_charg     = sl_lips-charg
                  IMPORTING
                    ev_valor     = lv_bag.
***Stefanini - IR230799 - 31/03/2025 - GGARAUJO1 - Fim de alteração

                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - fIM
                IF lv_bag IS INITIAL.
                  CALL METHOD me->recupera_atributo
                    EXPORTING
                      iv_nome_attr = 'PESO BAG'
                      iv_matnr     = sl_lips-matnr
                      iv_charg     = sl_lips-charg
                    IMPORTING
                      ev_valor     = lv_bag.

                  lv_bag = zcl_util=>get_string_numeric( CONV #( lv_bag ) ).
                ENDIF.
                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - fIM

                lv_peso_tot = sl_lips-lfimg * lv_bag.

                str_lfimg = lv_peso_tot.
                TRANSLATE str_lfimg USING '.,'.
                CONDENSE str_lfimg NO-GAPS.

*              CLEAR: str_lfimg.
*              str_lfimg = sl_lips-lfimg.
*              TRANSLATE str_lfimg USING '.,'.
*              CONDENSE str_lfimg NO-GAPS.
*              CONCATENATE vl_lips2 'QTD:' str_lfimg INTO vl_lips2 SEPARATED BY space.
                CONCATENATE vl_lips3 'PESO TOTAL:' str_lfimg 'KG' INTO vl_lips3 SEPARATED BY space.
              ENDIF.
            ENDIF.

*           CONCATENATE vl_lips vl_lips2 INTO vl_lips SEPARATED BY space.
            CONCATENATE vl_lips vl_lips2 vl_lips3  INTO vl_lips SEPARATED BY space.
*-Equalização RISE x PRD - 21.08.2023 - JT - fim
          ENDIF.
        ENDLOOP.

        IF ( sy-subrc NE 0 ).

          LOOP AT tl_lips INTO sl_lips WHERE matnr EQ in_lin-matnr
                                         AND vgpos EQ in_lin-itmnum.
            regis = sy-tabix.

            CALL METHOD me->function_classification
              EXPORTING
                p_matnr  = sl_lips-matnr
                p_charg  = sl_lips-charg
              IMPORTING
                p_textos = vl_lips2.

            IF vl_lips2 IS NOT INITIAL.

*-US190739-15.09.2025-#190739-JT-inicio
              CALL METHOD me->recupera_atributo
                EXPORTING
                  iv_nome_attr = 'CATEGORIA SEMENTE'
                  iv_matnr     = sl_lips-matnr
                  iv_charg     = sl_lips-charg
                IMPORTING
                  ev_valor     = lv_cat_sem.

              IF lv_cat_sem IS NOT INITIAL.
                SELECT SINGLE nome
                  INTO @_nome_categ_sem
                  FROM zsdt0199
                 WHERE id_cat_sementes = @lv_cat_sem.

                IF sy-subrc = 0.
                  lv_categ_sem1 = 'CATEGORIA SEMENTE:' && lv_cat_sem.
                  lv_categ_sem2 = 'CATEGORIA SEMENTE:' && _nome_categ_sem.
                  REPLACE ALL OCCURRENCES OF lv_categ_sem1 IN vl_lips2 WITH lv_categ_sem2.
                ENDIF.
              ENDIF.
*-US190739-15.09.2025-#190739-JT-fim

              IF sl_lips-lfimg IS NOT INITIAL.
                CLEAR: str_lfimg.
                str_lfimg = sl_lips-lfimg.
                TRANSLATE str_lfimg USING '.,'.
                CONDENSE str_lfimg NO-GAPS.
                CONCATENATE vl_lips2 'QTD:' str_lfimg INTO vl_lips2 SEPARATED BY space.
              ENDIF.

              IF ( vl_lips IS INITIAL ).
                CONCATENATE 'LOTE:' sl_lips-charg vl_lips vl_lips2 INTO vl_lips SEPARATED BY space.
              ELSE.
                CONCATENATE vl_lips ' / LOTE:' sl_lips-charg vl_lips2 INTO vl_lips SEPARATED BY space.
              ENDIF.


            ENDIF.

            CLEAR: vl_lips2, sl_lips.
          ENDLOOP.

        ENDIF.

        IF vl_lips IS NOT INITIAL.
          IF vl_textos IS INITIAL.
            vl_textos = vl_lips.
          ELSE.
            CONCATENATE vl_textos vl_lips INTO vl_textos SEPARATED BY space.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.
  ELSE.

    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>
    IF in_lin-reftyp NE 'BI'. "Diferente de "Faturamento"

      DATA: it_obs_item_nf    TYPE TABLE OF zsdt0294,
            l_matkl           TYPE mara-matkl,
            lit_textos_mat    TYPE zty_tline,
            lv_separador(3)   TYPE c VALUE ' / ',
            vl_textos_observ  TYPE string,
            vl_textos_item_nf TYPE string.

      CALL METHOD me->select_zsdt0294
        IMPORTING
          t_zsdt0294 = DATA(tl_zsdt0294).

      DELETE tl_zsdt0294 WHERE NOT ( auart IS INITIAL AND inf_add_prod IS NOT INITIAL ).

      IF tl_zsdt0294[] IS NOT INITIAL.

        CLEAR: vl_textos_item_nf, it_obs_item_nf[].

        CLEAR: l_matkl.

        SELECT SINGLE matkl
          FROM mara INTO l_matkl
         WHERE matnr EQ in_lin-matnr.

        "2º REGRA BUSCAR FILIAL/MATERIAL/
        LOOP AT tl_zsdt0294 INTO DATA(_wl_0294) WHERE branch EQ in_doc-branch
                                                  AND matnr  EQ in_lin-matnr.
          APPEND _wl_0294 TO it_obs_item_nf.
        ENDLOOP.

        "4º REGRA BUSCAR MATERIAL
        LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE branch EQ space
                                            AND matnr  EQ in_lin-matnr.
          APPEND _wl_0294 TO it_obs_item_nf.
        ENDLOOP.

        IF l_matkl IS NOT INITIAL. "Buscas utilizando Grupo de Mercadoria

          "2º REGRA BUSCAR FILIAL/GRP.MATERIAL/
          LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE branch EQ in_doc-branch
                                              AND matkl  EQ l_matkl.
            APPEND _wl_0294 TO it_obs_item_nf.
          ENDLOOP.

          "4º REGRA BUSCAR GRP.MATERIAL
          LOOP AT tl_zsdt0294 INTO _wl_0294 WHERE branch EQ space
                                              AND matkl  EQ l_matkl.
            APPEND _wl_0294 TO it_obs_item_nf.
          ENDLOOP.

        ENDIF.

        IF it_obs_item_nf[] IS NOT INITIAL.

          "Buscar Observaçoes Standard
          DATA(_tx_item_material) = abap_false.
          LOOP AT it_obs_item_nf  INTO DATA(lwa_obs_item) WHERE tx_item_material EQ abap_true.
            _tx_item_material = abap_true.
          ENDLOOP.


          IF _tx_item_material IS NOT INITIAL.
            CLEAR: lit_textos_mat[].
            CALL METHOD zcl_pp_services=>get_text_sd
              EXPORTING
                matnr = in_lin-matnr   " Nº do material
                werks = in_lin-werks   " Centro
              IMPORTING
                text  = lit_textos_mat.       " Categoria de tabela para estrutura Tline

            LOOP AT lit_textos_mat INTO DATA(sl_tline_mat).
              CONCATENATE vl_textos_item_nf sl_tline_mat-tdline INTO vl_textos_item_nf SEPARATED BY space.
            ENDLOOP.
          ENDIF.

          "Buscar Observaçoes Cadastro Z
          DELETE it_obs_item_nf WHERE id_obs IS INITIAL.
          SORT it_obs_item_nf BY id_obs.
          DELETE ADJACENT DUPLICATES FROM it_obs_item_nf COMPARING id_obs.

          LOOP AT it_obs_item_nf INTO _wl_0294.

            CLEAR: vl_textos_observ.

            SELECT SINGLE   observ , observ2
              FROM zsdt0287 INTO (@DATA(lv_observ), @DATA(lv_observ2) )
             WHERE id_obs    EQ @_wl_0294-id_obs
               AND cancelado EQ @abap_off.

            CHECK sy-subrc EQ 0 .

            CONCATENATE lv_observ lv_observ2 INTO vl_textos_observ SEPARATED BY space .

            IF vl_textos_item_nf IS INITIAL.
              vl_textos_item_nf = vl_textos_observ.
            ELSE.
              CONCATENATE vl_textos_item_nf lv_separador vl_textos_observ INTO vl_textos_item_nf SEPARATED BY space.
            ENDIF.

          ENDLOOP.

          IF vl_textos_item_nf IS NOT INITIAL.
            IF vl_textos IS NOT INITIAL.
              CONCATENATE vl_textos vl_textos_item_nf INTO vl_textos SEPARATED BY space.
            ELSE.
              vl_textos = vl_textos_item_nf.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP <<---



*-CS2025000249-12.05.2025-#174358-JT-inicio
*-------------------------------------
*-- Documentos MM - Exibir LOTES
*-------------------------------------
    DATA(t_mseg) = me->select_mseg( i_refkey = in_lin-refkey
                                    i_refitm = in_lin-refitm ).

    IF t_mseg[] IS NOT INITIAL.




      DATA: lv_categ_semente1 TYPE string,
            lv_categ_semente2 TYPE string.

      FREE: vl_lips, vl_lips2, vl_lips3, str_lfimg.

      DELETE t_mseg WHERE charg IS INITIAL.

      LOOP AT t_mseg INTO DATA(w_mseg).
        CONCATENATE 'LOTE:' w_mseg-charg INTO vl_lips SEPARATED BY space.

        CALL METHOD me->function_classification
          EXPORTING
            p_matnr  = w_mseg-matnr
            p_charg  = w_mseg-charg
          IMPORTING
            p_textos = vl_lips2.

        IF vl_lips2 IS NOT INITIAL.
          IF w_mseg-menge IS NOT INITIAL.
            FIND 'BAG:' IN vl_lips2 MATCH OFFSET v_off.

            IF sy-subrc  EQ 0.
*             lv_bag = vl_lips2+v_off.
*             REPLACE ALL OCCURRENCES OF REGEX '([[:alpha:]])' IN lv_bag WITH space.
*             REPLACE ALL OCCURRENCES OF SUBSTRING ':' IN lv_bag WITH space.
*             TRANSLATE lv_bag USING '. '.
*             TRANSLATE lv_bag USING ',.'.
*             CONDENSE lv_bag NO-GAPS.

              str_lfimg = w_mseg-menge.
              TRANSLATE str_lfimg USING '.,'.
              CONDENSE  str_lfimg NO-GAPS.
              CONCATENATE vl_lips2 'QTD:' str_lfimg INTO vl_lips2 SEPARATED BY space.

              CALL METHOD me->recupera_atributo
                EXPORTING
                  iv_nome_attr = 'PESO BAG'
                  iv_matnr     = w_mseg-matnr
                  iv_charg     = w_mseg-charg
                IMPORTING
                  ev_valor     = lv_bag.

              CALL METHOD me->recupera_atributo
                EXPORTING
                  iv_nome_attr = 'CATEGORIA SEMENTE'
                  iv_matnr     = w_mseg-matnr
                  iv_charg     = w_mseg-charg
                IMPORTING
                  ev_valor     = DATA(lv_cat_semente).

              IF lv_cat_semente IS NOT INITIAL.
                SELECT SINGLE nome
                  INTO @DATA(_nome_categ)
                  FROM zsdt0199
                 WHERE id_cat_sementes = @lv_cat_semente.

                IF sy-subrc = 0.
                  lv_categ_semente1 = 'CATEGORIA SEMENTE:' && lv_cat_semente.
                  lv_categ_semente2 = 'CATEGORIA SEMENTE:' && _nome_categ.
                  REPLACE ALL OCCURRENCES OF lv_categ_semente1 IN vl_lips2 WITH lv_categ_semente2.
                ENDIF.
              ENDIF.

              lv_bag      = zcl_util=>get_string_numeric( CONV #( lv_bag ) ). "*-CS2025000249-07.05.2025-#174157-JT
              lv_peso_tot = w_mseg-menge * lv_bag.

              str_lfimg = lv_peso_tot.
              TRANSLATE str_lfimg USING '.,'.
              CONDENSE  str_lfimg NO-GAPS.

              CONCATENATE vl_lips3 'PESO TOTAL:' str_lfimg 'KG' INTO vl_lips3 SEPARATED BY space.
            ENDIF.
          ENDIF.

          CONCATENATE vl_lips vl_lips2 vl_lips3  INTO vl_lips SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF vl_lips IS NOT INITIAL.
        IF vl_textos IS INITIAL.
          vl_textos = vl_lips.
        ELSE.
          CONCATENATE vl_textos vl_lips INTO vl_textos SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
*-CS2025000249-12.05.2025-#174358-JT-fim

*-CS2025000249-27.05.2025-#175255-JT-inicio
*-------------------------------------
*-- Documentos MM - Exibir LOTES
*-------------------------------------
    DATA(t_rseg) = me->select_rseg( i_refkey = in_lin-refkey
                                    i_refitm = in_lin-refitm ).

    IF t_rseg[] IS NOT INITIAL.
      FREE: vl_lips, vl_lips2, vl_lips3, str_lfimg.

      DELETE t_rseg WHERE charg IS INITIAL.

      LOOP AT t_rseg INTO DATA(w_rseg).
        CONCATENATE 'LOTE:' w_rseg-charg INTO vl_lips SEPARATED BY space.

        CALL METHOD me->function_classification
          EXPORTING
            p_matnr  = w_rseg-matnr
            p_charg  = w_rseg-charg
          IMPORTING
            p_textos = vl_lips2.

        IF vl_lips2 IS NOT INITIAL.
          IF w_rseg-menge IS NOT INITIAL.
            FIND 'BAG:' IN vl_lips2 MATCH OFFSET v_off.

            IF sy-subrc  EQ 0.
*             lv_bag = vl_lips2+v_off.
*             REPLACE ALL OCCURRENCES OF REGEX '([[:alpha:]])' IN lv_bag WITH space.
*             REPLACE ALL OCCURRENCES OF SUBSTRING ':' IN lv_bag WITH space.
*             TRANSLATE lv_bag USING '. '.
*             TRANSLATE lv_bag USING ',.'.
*             CONDENSE lv_bag NO-GAPS.

              str_lfimg = w_rseg-menge.
              TRANSLATE str_lfimg USING '.,'.
              CONDENSE  str_lfimg NO-GAPS.
              CONCATENATE vl_lips2 'QTD:' str_lfimg INTO vl_lips2 SEPARATED BY space.

              CALL METHOD me->recupera_atributo
                EXPORTING
                  iv_nome_attr = 'PESO BAG'
                  iv_matnr     = w_rseg-matnr
                  iv_charg     = w_rseg-charg
                IMPORTING
                  ev_valor     = lv_bag.

              CALL METHOD me->recupera_atributo
                EXPORTING
                  iv_nome_attr = 'CATEGORIA SEMENTE'
                  iv_matnr     = w_rseg-matnr
                  iv_charg     = w_rseg-charg
                IMPORTING
                  ev_valor     = lv_cat_semente.

              IF lv_cat_semente IS NOT INITIAL.
                SELECT SINGLE nome
                  INTO @_nome_categ
                  FROM zsdt0199
                 WHERE id_cat_sementes = @lv_cat_semente.

                IF sy-subrc = 0.
                  lv_categ_semente1 = 'CATEGORIA SEMENTE:' && lv_cat_semente.
                  lv_categ_semente2 = 'CATEGORIA SEMENTE:' && _nome_categ.
                  REPLACE ALL OCCURRENCES OF lv_categ_semente1 IN vl_lips2 WITH lv_categ_semente2.
                ENDIF.
              ENDIF.

              lv_bag      = zcl_util=>get_string_numeric( CONV #( lv_bag ) ). "*-CS2025000249-07.05.2025-#174157-JT
              lv_peso_tot = w_rseg-menge * lv_bag.

              str_lfimg = lv_peso_tot.
              TRANSLATE str_lfimg USING '.,'.
              CONDENSE  str_lfimg NO-GAPS.

              CONCATENATE vl_lips3 'PESO TOTAL:' str_lfimg 'KG' INTO vl_lips3 SEPARATED BY space.
            ENDIF.
          ENDIF.

          CONCATENATE vl_lips vl_lips2 vl_lips3  INTO vl_lips SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF vl_lips IS NOT INITIAL.
        IF vl_textos IS INITIAL.
          vl_textos = vl_lips.
        ELSE.
          CONCATENATE vl_textos vl_lips INTO vl_textos SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim
  ENDIF.
*-CS2025000249-12.05.2025-#174358-JT-fim

  "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - INICIO
  zcl_eudr_utils=>get_textos_eudr_nf_saida(
    EXPORTING
      i_docnum = in_lin-docnum                " Nº documento
    RECEIVING
      r_textos = DATA(lva_textos_eudr) ).             " Textos
  IF lva_textos_eudr IS NOT INITIAL.
    CONCATENATE vl_textos lva_textos_eudr INTO vl_textos SEPARATED BY space.
  ENDIF.
  "Customizar Dados Adicionais/Carimbo EUDR NF-e - BG #153326 - FIM

  DATA(_textos_contrib_uf) = me->get_textos_contrib_uf( i_lin = in_lin ).
  IF _textos_contrib_uf IS NOT INITIAL.
    CONCATENATE vl_textos _textos_contrib_uf INTO vl_textos SEPARATED BY space.
  ENDIF.

  IF ( <item>-l1_51_cst IS NOT INITIAL ) OR ( vl_cst_icms EQ '51' ).

    SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
      FROM j_1bbranch
     WHERE bukrs  EQ @in_doc-bukrs
       AND branch EQ @in_doc-branch.

    "Endereço Entrada
    SELECT SINGLE * INTO @DATA(wa_adrc_2)
      FROM adrc
     WHERE addrnumber EQ @wa_j_1bbranch-adrnr.

    SELECT SINGLE * FROM j_1btxic1
      INTO @DATA(wa_j_1btxic1)
     WHERE land1     EQ 'BR'
       AND shipfrom  EQ @wa_adrc_2-region
       AND shipto    EQ @in_doc-regio.

    IF ( sy-subrc EQ 0 ).
      DATA(p_rate) = wa_j_1btxic1-rate.
      LOOP AT in_tax INTO sl_tax WHERE itmnum = in_lin-itmnum AND taxtyp CS 'ICM'.

        IF <item>-l1_51_cst IS NOT INITIAL. "Quando transmitido para simetrya

          <item>-l1_51_picms  = p_rate.
          <item>-l1_51_vicms  = <item>-l1_51_vbc * ( p_rate / 100 ).
          ADD <item>-l1_51_vbc TO <xmlh>-s1_vbc.

        ELSE. "Quando transmitido para GRC

          IF <it_1bnfstx_tab> IS ASSIGNED.

            LOOP AT <it_1bnfstx_tab> ASSIGNING FIELD-SYMBOL(<fs_stx_item>) WHERE docnum = sl_tax-docnum
                                                                             AND itmnum = sl_tax-itmnum
                                                                             AND taxtyp = sl_tax-taxtyp.

              <fs_stx_item>-rate = p_rate.

              IF <fs_stx_item>-base IS INITIAL.

                IF <fs_stx_item>-excbas IS NOT INITIAL.

                  <fs_stx_item>-othbas = <fs_stx_item>-excbas.
                  CLEAR: <fs_stx_item>-excbas.

                ENDIF.

                ADD <fs_stx_item>-othbas TO <xmlh>-s1_vbc.

                out_item-vicmsdif = <fs_stx_item>-othbas * ( p_rate / 100 ).
                out_item-vicmsop  = out_item-vicmsdif.
                out_item-picmsdif = 100.

                IF vl_cst_icms  EQ '51'. "09/08/2023 - LP  CST 51
                  out_item-picmsdef = p_rate.

                ENDIF.

**  *-*-*-*-*-**   ICMS MONOFASICO - LP -10/08/2023
*                IF vl_cst_icms EQ '53'.
*
*                  CASE in_lin-taxsit.
*
*                    WHEN 'E'.
*
*                      SELECT SINGLE *
*                        FROM zrsi_icms_mono
*                        INTO ls_icms_mono
*                       WHERE ncm      EQ doc_item-nbm
*                         AND matnr    EQ doc_item-matnr.
*

*                      IF NOT sy-subrc IS INITIAL.
*
*                        SELECT SINGLE *
*                          FROM zrsi_icms_mono
*                          INTO ls_icms_mono
*                         WHERE ncm      EQ doc_item-nbm
*                           AND matnr    EQ space.
*
*                      ENDIF.
*
*                      IF sy-subrc IS INITIAL.
*
*                        doc_item-p_dif = ls_icms_mono-p_dif.
*                        doc_item-q_bc_mono = doc_item-menge.
*                        doc_item-adrem_icms = ls_icms_mono-amount.
*                        doc_item-v_icms_mono_op = doc_item-q_bc_mono * doc_item-adrem_icms.
*                        doc_item-v_icms_mono_dif = doc_item-v_icms_mono_op * ( ls_icms_mono-p_dif / 100 ).
*                        doc_item-v_icms_mono = doc_item-v_icms_mono_op - doc_item-v_icms_mono_dif.
*
*                      ENDIF.
*
*                    WHEN 'F'.
*
*                      SELECT SINGLE *
*                        FROM zrsi_icms_mono
*                        INTO ls_icms_mono
*                       WHERE ncm      EQ doc_item-nbm
*                         AND matnr    EQ doc_item-matnr.
*
*                      IF NOT sy-subrc IS INITIAL.
*
*                        SELECT SINGLE *
*                          FROM zrsi_icms_mono
*                          INTO ls_icms_mono
*                         WHERE ncm      EQ doc_item-nbm
*                           AND matnr    EQ space.
*
*                      ENDIF.
*
*                      IF sy-subrc IS INITIAL.
*
*                        IF ls_icms_mono-factor IS INITIAL.
*                          ls_icms_mono-factor = 1.
*                        ENDIF.
*
*                        doc_item-adrem_icms_ret  = ls_icms_mono-amount.
*                        doc_item-v_icms_mono_ret = ls_icms_mono-amount * doc_item-menge.
*
*                      ENDIF.
*
*                    WHEN OTHERS.
*
*                  ENDCASE.
*
*
*
*                ENDIF.


* **  *-*-*-*-*-** <<<END ICMS MONOFASICO - LP -10/08/2023




              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDIF.

  SELECT SINGLE *
    FROM zsdt0098
    INTO @wa_zsdt0098
   WHERE taxlw1 EQ @in_lin-taxlw1
     AND cfop   EQ @in_lin-cfop(4).

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0098
      INTO @wa_zsdt0098
     WHERE taxlw1 EQ @in_lin-taxlw1.
  ENDIF.

  IF sy-subrc EQ 0.
    out_item-cbenef = wa_zsdt0098-cbenf.     "Utilizado para GRC
  ENDIF.

  IF ( sy-subrc EQ 0 ) AND ( vl_cst_icms NOT IN r_cst_not_deson ).

    CLEAR: zona_franca.

    IF in_doc-partyp EQ 'C'.

      CALL METHOD me->seleci_knvi
        EXPORTING
          p_docnum      = in_doc-docnum
          p_parid       = in_doc-parid
          p_empresa     = in_doc-bukrs
        IMPORTING
          p_zona_franca = zona_franca.

      IF zona_franca EQ 'X' and wa_zsdt0098-MOTIVO  IS NOT INITIAL.

        CLEAR: valor_deconto.

        LOOP AT in_tax INTO sl_tax WHERE itmnum = in_lin-itmnum.
          IF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND ( sl_tax-excbas GT 0 ).
            sl_tax-excbas = ( sl_tax-excbas / ( ( 100 - 12 ) / 100 ) ).
            valor_deconto = valor_deconto + ( sl_tax-excbas * ( 12 / 100 ) ).
          ENDIF.
        ENDLOOP.

        IF valor_deconto GT 0.
          out_item-infadprod = valor_deconto.
          CONCATENATE vl_textos 'VALOR DO ICMS ABATIDO: R$' out_item-infadprod INTO vl_textos SEPARATED BY space.
          <item>-vprod          = <item>-vprod + valor_deconto.
          <item>-vdesc          = <item>-vdesc + valor_deconto.

          out_item-vicmsdeson   = valor_deconto.
          out_item-motdeson     = wa_zsdt0098-motivo.

          ADD valor_deconto TO <xmlh_310>-vicmsdeson.                              "Utilizado para GRC
          <item>-l1_00_vicms    = <item>-l1_00_vicms + valor_deconto.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zona_franca IS INITIAL.

      REFRESH: it_regio_deson.

      SELECT SINGLE * INTO wa_j_1bbranch
        FROM j_1bbranch
       WHERE bukrs  EQ in_doc-bukrs
         AND branch EQ in_doc-branch.

      "Endereço Entrada
      SELECT SINGLE * INTO wa_adrc_2
        FROM adrc
       WHERE addrnumber EQ wa_j_1bbranch-adrnr.

      wa_regio_deson = wa_adrc_2-region.
      APPEND wa_regio_deson TO it_regio_deson.

      CLEAR: icms_desonerado.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração

*      IF in_lin-itmtyp EQ 'T5' OR in_lin-itmtyp EQ '01'.
*        SELECT SINGLE matnr
*          FROM zmmt_ex_icms_des
*          INTO @DATA(lv_matnr)
*          WHERE matnr EQ @in_lin-matnr
*          AND   parid EQ @in_doc-parid.
*      ENDIF.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração

      LOOP AT it_regio_deson INTO wa_regio_deson.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração
        "IF icms_desonerado IS NOT INITIAL OR lv_matnr IS NOT INITIAL.
          IF icms_desonerado IS NOT INITIAL .

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração
          EXIT.
        ENDIF.
        CALL METHOD me->verifica_icms_desonerado
          EXPORTING
            p_bukrs         = in_doc-bukrs
            p_branch        = in_doc-branch
            p_partyp        = in_doc-partyp
            p_parid         = in_doc-parid
            p_taxlw1        = in_lin-taxlw1
            p_pais          = 'BR'
            p_regio         = wa_regio_deson
          IMPORTING
            p_desonerado    = icms_desonerado
            p_rate          = var_rate
            p_ck_lei_fiscal = ck_lei_fiscal. "Utilizado para GRC

        "Utilizado para Simetrya
*        CLEAR: CK_LEI_FISCAL.
*
*        CASE WA_REGIO_DESON.
*          WHEN 'RO'.
*
*            IF ( IN_LIN-TAXLW1 EQ 'R58' ) OR
*               ( IN_LIN-TAXLW1 EQ 'M86' ).
*              CK_LEI_FISCAL = ABAP_TRUE.
*            ENDIF.
*
*          WHEN 'MA'.
*
*            IF ( IN_LIN-TAXLW1 EQ 'O46' ) OR
*               ( IN_LIN-TAXLW1 EQ 'O50' ) OR
*               ( IN_LIN-TAXLW1 EQ 'O48' ).
*             CK_LEI_FISCAL = ABAP_TRUE.
*            ENDIF.
*
*          WHEN 'TO'.
*
*            IF ( IN_LIN-TAXLW1 EQ 'T45' ) OR
*               ( IN_LIN-TAXLW1 EQ 'T49' ) OR
*               ( IN_LIN-TAXLW1 EQ 'T47' ).
*              CK_LEI_FISCAL = ABAP_TRUE.
*            ENDIF.
*
*          WHEN 'PI'.
*
*            IF ( IN_LIN-TAXLW1 EQ 'PI4' ) OR
*               ( IN_LIN-TAXLW1 EQ 'PI7' ).
*              CK_LEI_FISCAL = ABAP_TRUE.
*            ENDIF.
*
*          WHEN OTHERS.
*
*        ENDCASE.
        "Utilizado para Simetrya


        "IF ICMS_DESONERADO IS NOT INITIAL.
        DATA(estado_selecionado) = wa_regio_deson.
        "ENDIF.

      ENDLOOP.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração
*      IF NOT icms_desonerado IS INITIAL AND out_item-vicmsdeson.
     " IF NOT icms_desonerado IS INITIAL AND out_item-vicmsdeson IS INITIAL AND lv_matnr IS INITIAL.
          IF NOT icms_desonerado IS INITIAL AND out_item-vicmsdeson IS INITIAL and wa_zsdt0098-MOTIVO is NOT INITIAL.

*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração
        CLEAR: valor_deconto.
        LOOP AT in_tax INTO sl_tax WHERE itmnum = in_lin-itmnum.
          IF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND ( sl_tax-excbas GT 0 ).

            "   sl_tax-excbas = ( sl_tax-excbas / ( ( 100 - var_rate ) / 100 ) ).
            valor_deconto = valor_deconto + ( sl_tax-excbas * ( var_rate / 100 ) ).

          ELSEIF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND ( sl_tax-othbas GT 0 ).

            "  sl_tax-othbas = ( sl_tax-othbas / ( ( 100 - var_rate ) / 100 ) ).
            valor_deconto = valor_deconto + ( sl_tax-othbas * ( var_rate / 100 ) ).

          ENDIF.
        ENDLOOP.

        IF valor_deconto GT 0.
*          SELECT SINGLE *
*            FROM TVARVC INTO @DATA(WL_TVARVC)
*           WHERE NAME EQ 'ZUF_ICMS_DESON_NF'
*             AND LOW  EQ @ESTADO_SELECIONADO.
*
*          IF SY-SUBRC EQ 0.
*            CONCATENATE VL_TEXTOS 'VALOR DO ICMS ABATIDO: R$' OUT_ITEM-INFADPROD INTO VL_TEXTOS SEPARATED BY SPACE.
*          ENDIF.

          IF ck_lei_fiscal EQ abap_true.
            <item>-vprod       = <item>-vprod + valor_deconto.
            <item>-vdesc       = <item>-vdesc + valor_deconto.
            out_item-infadprod = valor_deconto.
            CONDENSE out_item-infadprod NO-GAPS.

            CONCATENATE vl_textos 'VALOR DO ICMS ABATIDO: R$' out_item-infadprod INTO vl_textos SEPARATED BY space.
          ENDIF.

          out_item-vicmsdeson   = valor_deconto.
          out_item-motdeson     = wa_zsdt0098-motivo.

          ADD valor_deconto TO <xmlh_310>-vicmsdeson.                         "Utilizado para GRC
          <item>-l1_00_vicms    = <item>-l1_00_vicms + valor_deconto.

        ENDIF.

      ENDIF.
*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Início de Alteração
*      IF icms_desonerado IS INITIAL.
    "  IF icms_desonerado IS INITIAL AND lv_matnr IS INITIAL.
         IF icms_desonerado IS INITIAL AND  wa_zsdt0098-MOTIVO IS NOT INITIAL.
*** Stefanini - IR256168 - 15/09/2025 - RBRIBEIRO - Fim de Alteração
        CLEAR: icms_nao_tribut, valor_deconto.

        LOOP AT it_regio_deson INTO wa_regio_deson.

          IF icms_nao_tribut IS NOT INITIAL.
            EXIT.
          ENDIF.

          CALL METHOD me->verifica_icms_nao_tributado
            EXPORTING
              p_bukrs         = in_doc-bukrs
              p_branch        = in_doc-branch
              p_partyp        = in_doc-partyp
              p_parid         = in_doc-parid
              p_taxlw1        = in_lin-taxlw1
              p_pais          = 'BR'
              p_regio         = wa_regio_deson
            IMPORTING
              p_nao_tributado = icms_nao_tribut
              p_rate          = var_rate          "Utilizado para GRC
              p_ck_lei_fiscal = ck_lei_fiscal.    "Utilizado para GRC

          "Utilizado para Simetrya
*          CLEAR: CK_LEI_FISCAL.
*
*          CASE WA_REGIO_DESON.
*            WHEN 'MA'.
*
*              IF ( IN_LIN-TAXLW1 EQ 'O47' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'O49' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'O51' ).
*                CK_LEI_FISCAL = ABAP_TRUE.
*              ENDIF.
*
*            WHEN 'TO'.
*
*              IF ( IN_LIN-TAXLW1 EQ 'T46' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'T48' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'T50' ).
*                CK_LEI_FISCAL = ABAP_TRUE.
*              ENDIF.
*
*            WHEN 'PI'.
*
*              IF ( IN_LIN-TAXLW1 EQ 'PI5' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'PI6' ) OR
*                 ( IN_LIN-TAXLW1 EQ 'PI8' ).
*                CK_LEI_FISCAL = ABAP_TRUE.
*              ENDIF.
*
*            WHEN OTHERS.
*
*          ENDCASE.
          "Utilizado para Simetrya


        ENDLOOP.

        IF icms_nao_tribut IS NOT INITIAL.

          IF <item> IS ASSIGNED.

            DATA: v_basered1 TYPE j_1bnfstx-basered1,
                  v_base_tot TYPE j_1bnfstx-base.

            LOOP AT in_tax INTO sl_tax WHERE itmnum = in_lin-itmnum.

              CLEAR: vl_base_n_tribut, vl_aliq_n_tribut, vl_perc_n_tribut, v_basered1, v_base_tot.

              v_base_tot = sl_tax-base + sl_tax-excbas.
              IF v_base_tot > 0.
                v_basered1 = ( sl_tax-base / v_base_tot ) * 100.
              ENDIF.

              IF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND
                 ( v_basered1 > 0       ) AND
                 ( v_basered1 < 100     ) AND
                 ( sl_tax-rate     > 0       ) AND
                 ( <item>-vprod    > 0       ).

                vl_base_n_tribut = ( 100 - v_basered1 ) / 100.
                vl_aliq_n_tribut = vl_base_n_tribut * sl_tax-rate.
                vl_perc_n_tribut = ( 100 - vl_aliq_n_tribut ) / 100.
                IF vl_perc_n_tribut NE 0.
                  valor_deconto = valor_deconto + ( ( <item>-vprod / vl_perc_n_tribut ) * ( vl_aliq_n_tribut / 100 ) ).
                ENDIF.

              ELSEIF v_basered1 EQ 0.

                IF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND ( sl_tax-excbas GT 0 ).

                  sl_tax-excbas = ( sl_tax-excbas / ( ( 100 - var_rate ) / 100 ) ).
                  valor_deconto = valor_deconto + ( sl_tax-excbas * ( var_rate / 100 ) ).

                ELSEIF ( sl_tax-taxtyp(3) EQ 'ICM' ) AND ( sl_tax-othbas GT 0 ).

                  sl_tax-othbas = ( sl_tax-othbas / ( ( 100 - var_rate ) / 100 ) ).
                  valor_deconto = valor_deconto + ( sl_tax-othbas * ( var_rate / 100 ) ).

                ENDIF.

              ENDIF.

            ENDLOOP.

            IF valor_deconto GT 0.
              IF ck_lei_fiscal EQ abap_true.
                <item>-vprod       = <item>-vprod + valor_deconto.
                <item>-vdesc       = <item>-vdesc + valor_deconto.
                out_item-infadprod = valor_deconto.
                CONDENSE out_item-infadprod NO-GAPS.

                CONCATENATE vl_textos 'VALOR DO ICMS ABATIDO: R$' out_item-infadprod INTO vl_textos SEPARATED BY space.
              ENDIF.

              out_item-vicmsdeson   = valor_deconto.
              out_item-motdeson     = wa_zsdt0098-motivo.

              ADD valor_deconto TO <xmlh_310>-vicmsdeson.                          "Utilizado para GRC
              <item>-l1_00_vicms    = <item>-l1_00_vicms + valor_deconto.
            ENDIF.

          ENDIF. " IF <ITEM> IS ASSIGNED.

        ENDIF. " IF ICMS_NAO_TRIBUT IS NOT INITIAL.

      ENDIF. "IF ICMS_DESONERADO IS INITIAL.

    ENDIF.


  ENDIF. "IF ( SY-SUBRC EQ 0 ). SELECT * FROM ZSDT0098


  "Enquadramento do IPI """"""""""""""""""""""""""""""""""""""""""""""""

  IF out_item-n_cenq IS INITIAL.
    SELECT SINGLE *
      FROM zfit0092 INTO @DATA(lw_zfit0092)
     WHERE cst = @in_lin-taxlw2.

    IF ( sy-subrc IS INITIAL ).
      out_item-n_cenq  = lw_zfit0092-cenq.
    ENDIF.
  ENDIF.

  IF ( in_doc-doctyp EQ '6' ) AND
     ( in_doc-direct EQ '2' ) AND
     ( <xmlh>-finnfe EQ '4' ) AND
     <item>-n1_vipi IS NOT INITIAL.

    out_item-vipidevol = <item>-n1_vipi.

    IF in_lin-docref IS NOT INITIAL.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(wl_lin_ref)
       WHERE docnum EQ @in_lin-docref
        AND itmnum  EQ @in_lin-itmref.

      IF ( sy-subrc EQ 0 ) AND ( wl_lin_ref-menge IS NOT INITIAL ).
        out_item-pipidevol = ( in_lin-menge / wl_lin_ref-menge ) * 100.
        "OUT_ITEM-PIPIDEVOL = <ITEM>-N1_PIPI.
      ENDIF.
    ENDIF.
  ENDIF.

  IF out_item-n_clenq IS INITIAL.
    SELECT SINGLE *
      FROM j_1bnflin INTO @wl_lin_ref
     WHERE docnum EQ @in_lin-docref
      AND itmnum  EQ @in_lin-itmref.

    IF sy-subrc IS INITIAL.
      <item>-n_ipi   = 'X'.
      <item>-n_clenq = wl_lin_ref-taxsi2.
    ENDIF.
  ENDIF.

*  IF <ITEM>-N2_CST IS INITIAL AND <ITEM>-N1_CST IS INITIAL.
*
*    SELECT SINGLE * INTO @DATA(WA_CST)
*      FROM J_1BATL2
*     WHERE TAXLAW EQ @IN_LIN-TAXLW2.
*
*    IF SY-SUBRC IS INITIAL.
*      <ITEM>-N2_CST = WA_CST-TAXSIT.
*    ENDIF.
*
*  ENDIF.

  IF ( out_item-utrib EQ 'BAG' ) OR ( out_item-utribl = 'BAG' ).
    out_item-utrib  = 'SAC'.
    out_item-utribl = 'SAC'.
  ENDIF.

  IF <item> IS ASSIGNED AND out_item-utrib EQ 'TO'.

    out_item-utrib = 'TON'.

  ELSEIF <item> IS ASSIGNED AND out_item-utrib NE 'TO'.

    DATA(vl_ncm_exp) = <item>-ncm.
    REPLACE ALL OCCURRENCES OF '.' IN vl_ncm_exp WITH '' IGNORING CASE.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(_wl_j_1bbranch)
     WHERE bukrs  EQ @in_doc-bukrs
       AND branch EQ @in_doc-branch.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM adrc INTO @DATA(_wl_adrc_branch)
       WHERE addrnumber EQ @_wl_j_1bbranch-adrnr
         AND region     NE ''.

      IF ( sy-subrc = 0 ) AND ( _wl_adrc_branch-region IS NOT INITIAL ).
        SELECT SINGLE *
          FROM setleaf INTO @DATA(_wl_set_uf_utrib)
         WHERE setname = 'MAGGI_UF_UTRIB_EXP'
           AND valfrom = @_wl_adrc_branch-region.
      ENDIF.

    ENDIF.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_set_cfop_utrib)
     WHERE setname = 'MAGGI_CFOP_UTRIB_EXP'
       AND valfrom = @<item>-cfop(4).

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_set_ncm_utrib)
     WHERE setname = 'MAGGI_NCM_UTRIB_EXP'
       AND valfrom = @vl_ncm_exp.

    IF ( _wl_set_cfop_utrib IS NOT INITIAL ) AND ( _wl_set_ncm_utrib IS NOT INITIAL ) AND ( _wl_set_uf_utrib IS INITIAL ). "UF de Exceção

      DATA: vl_qtrib_exp TYPE j1b_nf_xml_item-qtrib_v20.

      IF <item>-qtrib > 0 .
        vl_qtrib_exp = <item>-qtrib.
      ELSEIF <item>-qtrib_v20 > 0.
        vl_qtrib_exp = <item>-qtrib_v20.
      ENDIF.

      out_item-utrib  = 'TON'.
      out_item-utribl = 'TON'.

      IF vl_qtrib_exp > 0.
        "Converter Tonelada
        CASE <item>-utrib.
          WHEN 'KG'.
            vl_qtrib_exp = vl_qtrib_exp / 1000.
        ENDCASE.

        "Não esta usando a funçao abaixo, por causa do tipo de dados do parametro i_menge. Depois deve adaptar a função para receber tipo dados,
        "com quantidade com 4 decimais.

*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            I_MATNR              = <ITEM>-CPROD
*            I_IN_ME              = <ITEM>-UTRIB
*            I_OUT_ME             = 'TO'
*            I_MENGE              = VL_QTRIB_EXP
*          IMPORTING
*            E_MENGE              = VL_QTRIB_EXP
*          EXCEPTIONS
*            ERROR_IN_APPLICATION = 1
*            ERROR                = 2
*            OTHERS               = 3.

        out_item-qtrib = vl_qtrib_exp.

        "Utilizado para GRC
        out_item-qtrib_v20 = vl_qtrib_exp.

        IF vl_qtrib_exp IS NOT INITIAL.
          out_item-vuntrib_v20 = <item>-vprod / vl_qtrib_exp.
        ENDIF.
        ""Utilizado para GRC

      ENDIF.
    ENDIF.

**<<<------" User Story 151256 - AOENNING - Início------>>>
    me->get_unid_tribut_item_nf(
      EXPORTING
        i_lin       = in_lin                " Partidas individuais da nota fiscal
      IMPORTING
        e_unid_trib = DATA(vga_gewei)       " Unidade de peso
        e_qtd_trib  = DATA(vga_brgew)       " Peso bruto
    ).

    IF vga_gewei IS NOT INITIAL AND vga_brgew IS NOT INITIAL.
      vl_qtrib_exp = vga_brgew.
      <item>-utrib = vga_gewei.
      out_item-utrib  = vga_gewei.
      out_item-utribl = vga_gewei.
      out_item-qtrib = vl_qtrib_exp.
      out_item-qtrib_v20 = vl_qtrib_exp.
      out_item-vuntrib_v20 = <item>-vprod / vl_qtrib_exp.
    ENDIF.

**<<<------" User Story 151256 - AOENNING - Fim------>>>

  ENDIF.
  "Fim CS2017000796


  IF in_lin-matnr IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(wa_mara)
      FROM mara
     WHERE matnr EQ @in_lin-matnr.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE * INTO @DATA(wa_matkl)
        FROM t023t
       WHERE matkl EQ @wa_mara-matkl
         AND spras EQ 'P'.
      IF sy-subrc IS INITIAL AND wa_matkl-wgbez CS 'SEMENTE'.
        DATA(lc_ck_semente) = abap_true.
      ELSE.
        lc_ck_semente = abap_false.
      ENDIF.
    ELSE.
      lc_ck_semente = abap_false.
    ENDIF.

    IF lc_ck_semente EQ abap_true.
      "SDFSDF
    ENDIF.

  ENDIF.

  out_item-infadprod = vl_textos.

  IF sl_vbkd-bstkd_e IS NOT INITIAL.
    out_item-xped = sl_vbkd-bstkd_e.
  ENDIF.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
  IF zcl_util_sd=>ck_integration_luft_active( i_direcao = 'S' ) EQ abap_true.
    DATA(lwa_romaneio) = zcl_les_utils=>get_romaneio_documento_fiscal( i_docnum = in_doc-docnum ).
    IF lwa_romaneio-id_interface = '48' AND lwa_romaneio-nro_cg IS NOT INITIAL.
      zcl_carga_saida_insumos=>busca_dados_carga(
        EXPORTING
          i_nr_carga_single = lwa_romaneio-nro_cg
        IMPORTING
          e_cargas          = DATA(lit_carga)
          e_solicitacoes    = DATA(lit_solicitacoes) ).
      READ TABLE lit_solicitacoes INTO DATA(lwa_sol) WITH KEY vbeln  = lwa_romaneio-vbeln
                                                              nr_rot = lwa_romaneio-nr_rot.
      IF sy-subrc EQ 0.
        READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
        IF sy-subrc EQ 0 AND lit_carga[] IS NOT INITIAL AND lwa_carga-nro_pedido_luft IS NOT INITIAL.
          out_item-xped      = lwa_carga-nro_pedido_luft.

          IF lwa_sol-seq_carregamento_luft IS NOT INITIAL.
            out_item-nitemped  = lwa_sol-seq_carregamento_luft.
          ELSE.
            out_item-nitemped  = lwa_sol-seq_entrega.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

  "Utilizado para GRC
  IF out_item-cean IS INITIAL.
    out_item-cean = 'SEM GTIN'. " SEM GTIN
  ENDIF.

  IF out_item-ceantrib IS INITIAL.
    out_item-ceantrib = out_item-cean.
  ENDIF.
  "Utilizado para GRC

  CALL METHOD me->block_h2
    EXPORTING
      i_docnum      = in_lin-docnum
      i_itmnum      = in_lin-itmnum
      i_preenche_di = 'X'
    IMPORTING
      o_out_item    = out_item
      o_out_import  = out_import.

  IF in_lin-reftyp EQ 'ZW'.

    "Textos Devolução Remessa Industrialização - Inicio
    DATA: vl_textos_rem_ind TYPE string,
          vl_qtde_prod      TYPE zsdt0249-qtde_vinc,
          vl_qtde_prod_txt  TYPE c LENGTH 200.

    SELECT SINGLE *
      FROM zsdt0252 INTO @DATA(wl_zsdt0252)
     WHERE seqlcto_devol EQ @in_lin-refkey(10).

    IF sy-subrc EQ 0.

      CLEAR: vl_textos_rem_ind.

      vl_textos_rem_ind = 'Produtos resultantes da industrialização:'.

      SELECT SINGLE *
        FROM zsdt0248 INTO @DATA(wl_zsdt0248_fh)
       WHERE id_boletim          EQ @wl_zsdt0252-id_boletim
         AND tp_produto_producao EQ 'FH'. "Farelo Hipro

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0248_fh-qtde > 0 ).
        vl_qtde_prod     = ( wl_zsdt0248_fh-qtde * wl_zsdt0252-perc_vinc_agr  ) / 100.
        WRITE vl_qtde_prod TO vl_qtde_prod_txt.
        CONDENSE vl_qtde_prod_txt NO-GAPS.
        CONCATENATE vl_textos_rem_ind 'FARELO SOJA HIPRO PROD PROPRIA NCM 2304.00.90:' vl_qtde_prod_txt '(KG)' INTO vl_textos_rem_ind SEPARATED BY space.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0248 INTO @DATA(wl_zsdt0248_fc)
       WHERE id_boletim          EQ @wl_zsdt0252-id_boletim
         AND tp_produto_producao EQ 'FC'. "Farelo Comum

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0248_fc-qtde > 0 ).
        vl_qtde_prod     = ( wl_zsdt0248_fc-qtde * wl_zsdt0252-perc_vinc_agr  ) / 100.
        WRITE vl_qtde_prod TO vl_qtde_prod_txt.
        CONDENSE vl_qtde_prod_txt NO-GAPS.
        CONCATENATE vl_textos_rem_ind 'FARELO SOJA COMUM PROD PROPRIA NCM 2304.00.10:' vl_qtde_prod_txt '(KG)' INTO vl_textos_rem_ind SEPARATED BY space.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0248 INTO @DATA(wl_zsdt0248_od)
       WHERE id_boletim          EQ @wl_zsdt0252-id_boletim
         AND tp_produto_producao EQ 'OD'. "Oleo Degomado

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0248_od-qtde > 0 ).
        vl_qtde_prod     = ( wl_zsdt0248_od-qtde * wl_zsdt0252-perc_vinc_agr ) / 100.
        WRITE vl_qtde_prod TO vl_qtde_prod_txt.
        CONDENSE vl_qtde_prod_txt NO-GAPS.
        CONCATENATE vl_textos_rem_ind '/ OLEO SOJA DEGOMADO PROD PROPRIA NCM 1507.10.00:' vl_qtde_prod_txt '(KG)' INTO vl_textos_rem_ind SEPARATED BY space.
      ENDIF.


      SELECT SINGLE *
        FROM zsdt0248 INTO @DATA(wl_zsdt0248_cm)
       WHERE id_boletim          EQ @wl_zsdt0252-id_boletim
         AND tp_produto_producao EQ 'CM'. "Casca Moida

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0248_cm-qtde > 0 ).
        vl_qtde_prod     = ( wl_zsdt0248_cm-qtde * wl_zsdt0252-perc_vinc_agr  ) / 100.
        WRITE vl_qtde_prod TO vl_qtde_prod_txt.
        CONDENSE vl_qtde_prod_txt NO-GAPS.
        CONCATENATE vl_textos_rem_ind '/ CASCA MOIDA PROD PROPRIA NCM 2710.12.10:' vl_qtde_prod_txt '(KG)' INTO vl_textos_rem_ind SEPARATED BY space.
      ENDIF.

      CONCATENATE out_item-infadprod vl_textos_rem_ind INTO out_item-infadprod SEPARATED BY space.

    ENDIF.

    ""Textos Devolução Remessa Industrialização - Fim

  ENDIF.

  "" LP:USER STORY 78164 : CS2022000513 Ajustar envio no código de material no Arquivo XML retirar zeros a esquerda quando não contiver no material.
  DATA(lv_cprod) =  |{  <item>-cprod  ALPHA = OUT }|.

  <item>-cprod = lv_cprod.
  "Aguardar Subida para PRD.




  out_item-infadprod = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_item-infadprod ) ) ).

  out_item-xped      = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( out_item-xped ) ) ).


*-------------------------------------
*-- REFORMA TRIBUTÁRIA --*
*-------------------------------------

* T (04) 000 - Tributação integral
* R (52) 200 - Alíquota reduzida
* N (22) 410 - Imunidade e não incidência
* D (02) 510 - Diferimento
* S (20) 550 - Suspensão
* B (02) 010 - Tributação com alíquotas uniformes
* G (03) 210 - Redução de alíquota com redutor de base de cálculo
* H (03) 220 - Alíquota fixa
* I (01) 221 - Alíquota fixa rateada
* J (01) 222 - Redução de Base de Cálculo
* M (06) 620 - Tributação Monofásica
* C (02) 800 - Transferência de crédito
* A (01) 810 - Ajustes
* K (06) 820 - Tributação em declaração de regime específico
* F (05) 011 - Tributação com alíquotas uniformes reduzidas
* I (01) 400 - Isenção
* E (01) 830 - Exclusão da Base de Cálculo


  IF in_lin-taxsituation(1) EQ 'D'.


    <item>-pibsuf = <it_1bnfstx_tab>[ taxtyp = 'IB3S' ]-rate4dec.
    CLEAR <item>-vibs.

    " Para IB3S  - Quando ajustado na determinação de imposto comentar
    SORT <it_1bnfstx_tab> BY taxtyp itmnum.
    READ TABLE <it_1bnfstx_tab> ASSIGNING FIELD-SYMBOL(<fs_ib3s>)
      WITH KEY taxtyp = 'IB3S' itmnum = <item>-itmnum.

    IF sy-subrc = 0.
      CLEAR <fs_ib3s>-taxval.
    ENDIF.
    UNASSIGN <fs_ib3s>.

    " Para CBS3 Quando ajustado na determinação de imposto comentar
    READ TABLE <it_1bnfstx_tab> ASSIGNING FIELD-SYMBOL(<fs_cbs3>)
      WITH KEY taxtyp = 'CBS3' itmnum = <item>-itmnum.

    IF sy-subrc = 0.
      CLEAR <fs_cbs3>-taxval.
    ENDIF.
    UNASSIGN <fs_cbs3>.

    out_item-pdifibsuf = 100.
    IF <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-base  IS INITIAL.
      out_item-vdifibsuf =  ( <item>-pibsuf * <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-excbas ) / 100.
    ELSE.
      out_item-vdifibsuf =  ( <item>-pibsuf * <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-base ) / 100.
    ENDIF.

    out_item-pdifibsmun = 100.
    out_item-vdifibsmun = 0.

    out_item-pdifcbs = 100.
    out_item-vdifcbs = 0.

    <item>-pcbs = <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec.
    out_item-pdifcbs = 100.
    IF <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-base IS INITIAL.
      out_item-vdifcbs = ( <item>-pcbs * <item>-vprod * <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-excbas ) / 100 .

    ELSE.
      out_item-vdifcbs = ( <item>-pcbs  * <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-base ) / 100 .
    ENDIF.


  ENDIF.

  IF in_lin-taxsituation(1) EQ 'S'.

    out_item-cstreg = '000' .
    out_item-cclasstribreg = '000001' .
    out_item-paliqefetregibsuf = <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec.
    SORT <it_1bnfstx_tab> BY taxtyp itmnum.
    IF <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-base IS INITIAL.
      out_item-vtribregibsuf =  ( <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-excbas ) / 100.
    ELSE.
      out_item-vtribregibsuf =  ( <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-base ) / 100.
    ENDIF.

    out_item-paliqefetregibsmun = <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-rate4dec.

    IF <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-base IS INITIAL.
      out_item-vtribregibsmun =  ( <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-excbas ) / 100.
    ELSE.
      out_item-vtribregibsmun =  ( <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'IB3M' itmnum = <item>-itmnum ]-base ) / 100.
    ENDIF.

    out_item-paliqefetregcbs = <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec.

    IF <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-base IS INITIAL.
      out_item-vtribregcbs =  ( <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-excbas ) / 100.
    ELSE.
      out_item-vtribregcbs =  ( <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec * <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-base ) / 100.
    ENDIF.

  ENDIF.

  IF in_lin-taxsituation(1) EQ 'R'.

    <item>-pibsuf = <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec.
    out_item-paliqefetregibsuf = <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec - ( <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-rate4dec * ( <item>-predaliqibsuf / 100 ) ).
    " OUT_ITEM-PALIQEFETREGIBSMUN = <IT_1BNFSTX_TAB>[ TAXTYP = 'IB3M' ]-RATE4DEC -  (  <ITEM>-PREDALIQIBSUF / 1000 ).

    out_item-paliqefetregcbs = <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec - ( <it_1bnfstx_tab>[ taxtyp = 'CBS3' itmnum = <item>-itmnum ]-rate4dec * ( <item>-predaliqibsuf / 100 ) ).

    <item>-vibs = <it_1bnfstx_tab>[ taxtyp = 'IB3S' itmnum = <item>-itmnum ]-base * out_item-paliqefetregibsuf / 100.
    " Para IB3S  - Quando ajustado na determinação de imposto comentar
    SORT <it_1bnfstx_tab> BY taxtyp itmnum.
    READ TABLE <it_1bnfstx_tab> ASSIGNING <fs_ib3s>
      WITH KEY taxtyp = 'IB3S' itmnum = <item>-itmnum.

    IF sy-subrc = 0.
      <fs_ib3s>-rate4dec = out_item-paliqefetregibsuf.
      <fs_ib3s>-taxval = <item>-vibs.
    ENDIF.

*      READ TABLE <IT_1BNFSTX_TAB> ASSIGNING FIELD-SYMBOL(<FS_IB3M>)
*      WITH KEY TAXTYP = 'IB3M'.
*
*    IF SY-SUBRC = 0.
*      <FS_IB3M>-RATE4DEC = OUT_ITEM-PALIQEFETREGIBSMUN.
*    ENDIF.
    SORT <it_1bnfstx_tab> BY taxtyp itmnum.
    READ TABLE <it_1bnfstx_tab> ASSIGNING <fs_cbs3>
     WITH KEY taxtyp = 'CBS3' itmnum = <item>-itmnum.

    IF sy-subrc = 0.
      <item>-pcbs = <fs_cbs3>-rate4dec.
      <fs_cbs3>-rate4dec = out_item-paliqefetregcbs.
      <fs_cbs3>-taxval = <fs_cbs3>-base * out_item-paliqefetregcbs / 100 .
    ENDIF.




  ENDIF.












  "Export Item to Memory -- Utilizado para Simetrya...
  CLEAR: it_j1b_nf_xml_badi_item[].

  IMPORT it_j1b_nf_xml_badi_item TO it_j1b_nf_xml_badi_item FROM MEMORY ID 'IT_J1B_NF_XML_BADI_ITEM'.
  APPEND out_item TO it_j1b_nf_xml_badi_item.

  EXPORT it_j1b_nf_xml_badi_item FROM it_j1b_nf_xml_badi_item TO MEMORY ID 'IT_J1B_NF_XML_BADI_ITEM'.


ENDMETHOD.


  method IF_EX_CL_NFE_PRINT~FILL_NVE.
  endmethod.


  method IF_EX_CL_NFE_PRINT~FILL_REFNFE.
  endmethod.


  METHOD if_ex_cl_nfe_print~fill_trace.

    "eliminar informações de rastro.
*    IF in_item IS NOT INITIAL.
*      LOOP AT out_trace INTO DATA(wa_trace).
*        IF wa_trace-nlote IS NOT INITIAL.
*          DELETE out_trace WHERE docnum = wa_trace-docnum
*                             AND itmnum = wa_trace-itmnum.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

    CLEAR: out_trace[], out_trace.

  ENDMETHOD.


method IF_EX_CL_NFE_PRINT~GET_SERVER.

endmethod.


  method IF_EX_CL_NFE_PRINT~GET_SERVER_DFE.
  endmethod.


  method IF_EX_CL_NFE_PRINT~IS_ICMS_PART_IN_EXCEPTION_LIST.
  endmethod.


method IF_EX_CL_NFE_PRINT~RESET_SUBRC.

endmethod.


method IF_EX_CL_NFE_PRINT~SET_COMMIT.

endmethod.


method IF_EX_CL_NFE_PRINT~SET_ORDER_FOR_BATCH.

endmethod.


  method READ_PARTNER.

  DATA: LS_PARTNER   TYPE J_1BNFNAD.

  CLEAR: C_J_1BINNAD, R_OK.

  CHECK I_PARVW IS NOT INITIAL.

  READ TABLE I_J_1BNFNAD_TAB INTO LS_PARTNER
    WITH KEY PARVW  = I_PARVW.

  CHECK SY-SUBRC EQ 0.

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      PARTNER_TYPE                 = LS_PARTNER-PARTYP
      PARTNER_ID                   = LS_PARTNER-PARID
      READ_ADDRESS                 = ABAP_TRUE
    IMPORTING
      PARNAD                       = C_J_1BINNAD
    EXCEPTIONS
      PARTNER_NOT_FOUND            = 1
      PARTNER_TYPE_NOT_FOUND       = 2
      OTHERS                       = 3.

   IF SY-SUBRC EQ 0.
     R_OK = ABAP_TRUE.
   ENDIF.

  endmethod.


METHOD recupera_atributo.
***Stefanini - IR230799 - 31/03/2025 - GGARAUJO1 - Inicio de alteração
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Ajuste da busca do Atributo BAG e da  &*
*&                                    |formatação do valor para numérico.    &*
*&                                    |Chamado: 169508.                      &*
*&---------------------------------------------------------------------------&*

  DATA: tl_class       TYPE TABLE OF sclass,
        tl_objectdata  TYPE TABLE OF clobjdat,
        sl_object      TYPE clobjdat,
        vl_class       TYPE klah-class,
        vl_classtext   TYPE rmclm-anzukz VALUE 'X',
        vl_classtype   TYPE klah-klart,
        vl_clint       TYPE klah-clint,
        vl_object      TYPE ausp-objek,
        vl_objecttable TYPE tcla-obtab   VALUE 'MCH1',
        vl_obtab       TYPE tclao-obtab,
        vl_cuobj       TYPE inob-cuobj,
        vl_textos      TYPE string,
        lv_space       TYPE char22 VALUE '                      ',
        vl_aux         TYPE string.

* Material + Lote
  CONCATENATE iv_matnr
              iv_charg
         INTO vl_object SEPARATED BY lv_space.

  vl_obtab = 'MCH1'.

  SELECT SINGLE tcla~klart
    INTO vl_classtype
    FROM tcla JOIN tclao
      ON tcla~klart = tclao~klart
  WHERE tcla~obtab    EQ 'MCHA'
    AND tcla~intklart EQ space
    AND tcla~multobj  EQ 'X'
    AND tclao~obtab   EQ vl_obtab.

  SELECT cuobj UP TO 1 ROWS
    FROM inob
    INTO vl_cuobj
  WHERE klart EQ vl_classtype
    AND obtab EQ vl_obtab
    AND objek EQ vl_object.
    EXIT.
  ENDSELECT.

  SELECT clint UP TO 1 ROWS
    FROM kssk
    INTO vl_clint
  WHERE objek EQ vl_cuobj
    AND mafid EQ 'O'
    AND klart EQ vl_classtype.
    EXIT.
  ENDSELECT.

  SELECT SINGLE class
    FROM klah
    INTO vl_class
  WHERE clint EQ vl_clint.

  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      class              = vl_class
      classtext          = vl_classtext
      classtype          = vl_classtype
      clint              = vl_clint
      object             = vl_object
      objecttable        = vl_objecttable
    TABLES
      t_class            = tl_class
      t_objectdata       = tl_objectdata
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.

  DELETE tl_objectdata WHERE ausp1 = '?'.

  CHECK NOT tl_objectdata[] IS INITIAL.

**<<<------"169508 - NMS - INI------>>>
*  IF iv_nome_attr CS 'BAG'.
*
*    LOOP AT tl_objectdata INTO sl_object WHERE smbez CS iv_nome_attr.
*      ev_valor = zcl_util=>get_string_numeric( CONV #( sl_object-ausp1 ) ).
*      EXIT.
*    ENDLOOP.
*
*  ELSE.
*<<<------"169508 - NMS - FIM------>>>

  READ TABLE tl_objectdata INTO sl_object WITH KEY smbez = iv_nome_attr.

  IF sy-subrc = 0.
    ev_valor = sl_object-ausp1.
  ENDIF.

**<<<------"169508 - NMS - INI------>>>
  "  ENDIF.
**<<<------"169508 - NMS - FIM------>>>

***Stefanini - IR230799 - 31/03/2025 - GGARAUJO1 - Fim de alteração
ENDMETHOD.


METHOD seleciona_dados.

  DATA: sl_vbrp      TYPE znfe_vbrp,
        sl_vbak      TYPE znfe_vbak,
        sl_vbap      TYPE znfe_vbap,
        sl_lips      TYPE znfe_lips,
        sl_produtor  TYPE zdco_produtor,
        s_nf_entrada TYPE zdco_nf_entrada,
        vl_parid     TYPE j_1bnfdoc-parid.

  CALL METHOD me->select_zsdt0004
    EXPORTING
      p_docnum   = p_docnum
    IMPORTING
      t_zsdt0004 = t_zsdt0004.

  CALL METHOD me->select_vbrp
    EXPORTING
      p_refkey = p_refkey
      p_posnr  = p_itmnum
    IMPORTING
      t_vbrp   = t_vbrp.

  READ TABLE t_vbrp INTO sl_vbrp INDEX 1.
  CALL METHOD me->select_vbak
    EXPORTING
      p_aubel = sl_vbrp-aubel
    IMPORTING
      t_vbak  = t_vbak.

  READ TABLE t_vbak INTO sl_vbak INDEX 1.
  CALL METHOD me->select_vbap
    EXPORTING
      p_vbeln = sl_vbak-vbeln
      p_posnr = p_itmnum
    IMPORTING
      t_vbap  = t_vbap.

  READ TABLE t_vbap INTO sl_vbap INDEX 1.
  CALL METHOD me->select_zsdt0001
    EXPORTING
      p_vbeln    = sl_vbap-vbeln
      p_werks    = sl_vbap-werks
      p_bukrs    = sl_vbak-bukrs_vf
      p_vgbel    = sl_vbrp-vgbel
    IMPORTING
      t_zsdt0001 = t_zsdt0001.

  CALL METHOD me->select_lips
    EXPORTING
      p_vgbel = sl_vbak-vgbel
      p_posnr = p_itmnum
    IMPORTING
      t_lips  = t_lips.

  IF t_lips IS INITIAL.
    CALL METHOD me->select_lips
      EXPORTING
        p_vgbel = sl_vbrp-vgbel
        p_posnr = p_itmnum
      IMPORTING
        t_lips  = t_lips.
  ENDIF.

  IF sl_vbak-auart = 'ZTRI'.
    IF t_lips IS  INITIAL.
      SELECT  SINGLE * FROM vbfa  INTO @DATA(wl_vbfa)
        WHERE vbeln EQ @sl_vbak-vbeln
        AND   posnv EQ @p_itmnum
        AND   vbtyp_v EQ 'J'.

      IF sy-subrc EQ 0.
        CALL METHOD me->select_lips
          EXPORTING
            p_vgbel = wl_vbfa-vbelv
            p_posnr = p_itmnum
          IMPORTING
            t_lips  = t_lips.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE t_lips INTO sl_lips INDEX 1.
  CALL METHOD me->select_likp
    EXPORTING
      p_vbeln = sl_lips-vbeln
    IMPORTING
      s_likp  = s_likp.

  CALL METHOD me->select_zdco_produtor
    EXPORTING
      p_vbeln    = sl_vbrp-aubel
    IMPORTING
      t_produtor = t_produtor.

  CALL METHOD me->select_zdco_nf_entrada
    EXPORTING
      p_nu_dco     = sl_produtor-nu_dco
    IMPORTING
      t_nf_entrada = t_nf_entrada.

  CALL METHOD me->select_j1bnfdoc
    EXPORTING
      p_docnum_en = s_nf_entrada-docnum
    IMPORTING
      p_parid     = vl_parid
      p_nfenum    = p_nfenum.

  CALL METHOD me->select_lfa1
    EXPORTING
      p_lifnr = vl_parid
    IMPORTING
      s_lfa1  = s_lfa1.

  CALL METHOD me->select_zsdt0006
    EXPORTING
      p_auart    = sl_vbak-auart
    IMPORTING
      t_zsdt0006 = t_zsdt0006.

  CALL METHOD me->select_zsdt0294
    EXPORTING
      p_auart    = sl_vbak-auart
    IMPORTING
      t_zsdt0294 = t_zsdt0294.

ENDMETHOD.


METHOD seleci_knb1.

  CLEAR s_kverm.

  CHECK NOT p_parid IS INITIAL.
  CHECK NOT p_bukrs IS INITIAL.

  SELECT SINGLE kverm
    INTO s_kverm
    FROM knb1
   WHERE kunnr EQ p_parid
     AND bukrs EQ p_bukrs.

ENDMETHOD.


METHOD seleci_knvi.

  DATA: v_j_1bnflin TYPE j_1bnflin,
        v_j_1btxsdc TYPE j_1btxsdc_,
        v_knb1      TYPE knb1.

  CLEAR p_zona_franca.

  CHECK NOT p_docnum IS INITIAL.
  CHECK NOT p_parid IS INITIAL.
  CHECK NOT p_empresa IS INITIAL.

  SELECT SINGLE * INTO v_knb1
    FROM knb1
   WHERE kunnr EQ p_parid
     AND bukrs EQ p_empresa.

  CHECK ( sy-subrc IS INITIAL ) AND ( NOT v_knb1-kverm IS INITIAL ).

  SELECT SINGLE * INTO v_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ p_docnum.

  CHECK sy-subrc IS INITIAL.

  "suframa somente existe para brasil
  SELECT SINGLE taxkd
    INTO p_zona_franca
    FROM knvi
   WHERE kunnr EQ p_parid
     AND taxkd EQ '2'
     AND tatyp EQ 'IBRX'
     AND aland EQ 'BR '.

  IF sy-subrc EQ 0.
    IF p_zona_franca EQ '2'.
      p_zona_franca = 'X'.
    ELSE.
      CLEAR p_zona_franca.
    ENDIF.
  ELSE.
    CLEAR p_zona_franca.
  ENDIF.

ENDMETHOD.


METHOD select_coleta_produtor.

  CHECK p_item_nf-reftyp EQ 'MD'.

  DATA: wa_mkpf TYPE mkpf,
        wa_likp TYPE likp,
        wa_lips TYPE lips,
        it_lips TYPE TABLE OF lips,
        wa_ekpa TYPE ekpa,
        it_ekpa TYPE TABLE OF ekpa,
        wa_info_part TYPE lfa1,
        vg_cpf   TYPE c LENGTH 14,
        vg_cnpj  TYPE c LENGTH 18,
        vg_texto TYPE c LENGTH 18.

  SELECT SINGLE * INTO wa_mkpf
    FROM mkpf
   WHERE mblnr EQ p_item_nf-refkey(10)
     AND mjahr EQ p_item_nf-refkey+10(4).

  CHECK sy-subrc IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_mkpf-le_vbeln
    IMPORTING
      output = wa_mkpf-le_vbeln.

  SELECT SINGLE * INTO wa_likp
    FROM likp
   WHERE vbeln EQ wa_mkpf-le_vbeln.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_lips
    FROM lips
   WHERE vbeln EQ wa_likp-vbeln.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_ekpa
    FROM ekpa
     FOR ALL ENTRIES IN it_lips
   WHERE ebeln EQ it_lips-vgbel
     AND parvw EQ 'PR'.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_ekpa INTO wa_ekpa.

    CLEAR: wa_info_part.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_ekpa-lifn2
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_part.

    IF wa_info_part IS NOT INITIAL.

      IF wa_info_part-stkzn IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = vg_cnpj.
        WRITE vg_cnpj TO vg_texto.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd2
          IMPORTING
            output = vg_cpf.
        WRITE vg_cpf TO vg_texto.
      ENDIF.

      CONCATENATE e_observacao
                  'PC:'
                  wa_info_part-name1
                  '-'
                  vg_texto
                  '-'
                  wa_info_part-stcd3
                  '-'
                  wa_info_part-stras
                  '-'
                  wa_info_part-pstlz
                  '-'
                  wa_info_part-ort02
                  '-'
                  wa_info_part-ort01
                  '-'
                  wa_info_part-regio
             INTO e_observacao SEPARATED BY space.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD select_exportacao.

  DATA: BEGIN OF wa_item_nota,
          reftyp TYPE j_1breftyp, "Tipo referência = BI
          refkey TYPE j_1brefkey, "Referência ao documento de origem
          refitm TYPE j_1brefitm, "Item de referência ao documento de origem
        END OF wa_item_nota,

        BEGIN OF wa_cab_fat,
          vbeln TYPE vbeln_vf, "Documento de faturamento
          fkart TYPE fkart   , "Tipo documento de faturamento
          waerk TYPE waerk   , "Moeda do documento SD
          kurrf TYPE kurrf   , "Taxa de câmbio para lançamentos FI
          netwr TYPE netwr   , "Valor líquido na moeda do documento
        END OF wa_cab_fat,

        BEGIN OF wa_itm_fat,
          vgbel TYPE vgbel, "Nº documento do documento de referência
          vgpos TYPE vgpos, "Nº item do item comercial modelo
          volum TYPE volum_15, "Volume
          voleh TYPE voleh, "Unidade de volume
          aubel TYPE vbeln_va, "Documento de vendas Ordem de Venda
        END OF wa_itm_fat,

        BEGIN OF wa_dox_exp,
          id_doc_exp       TYPE zid_doc, "ID Documento
          nr_registro_expo TYPE znr_reg, "Num. Registro Exportação
          id_nomeacao_tran TYPE zid_nom, "Nomeação de Transporte
          nr_dde           TYPE znr_dde, "Número da Declaração de Despacho de Exportação
          numero_due       TYPE zde_numero_due,
          id_due           TYPE zde_id_due,
        END OF wa_dox_exp.

  DATA: obs_moeda      TYPE string,
        obs_export     TYPE string,
        obs_bl         TYPE string,
        obs_navio      TYPE string,
        obs_dco        TYPE string,
        obs_leilao     TYPE string,
        lv_qtd_db      TYPE char16, "RJF #113631-18.01.2024-JT-inicio
        test_taxa      TYPE c LENGTH 20,
        test_valor     TYPE c LENGTH 20,
        v_id_conhec    TYPE znom_conhec-id_conhec,
        wa_znom_conhec TYPE znom_conhec,
        test_navio     TYPE c LENGTH 20,
        ls_j_1binnad   TYPE vbpavb,
        lv_ntgew       TYPE ntgew_15,
        lv_brgew       TYPE brgew_15,
        vg_doc_exp     TYPE docnum,
        ls_retorno     TYPE zsdt_retlote,
        wa_item_ret    LIKE wa_item_nota,
        sl_produtor    TYPE zdco_produtor.

  DATA: tb_retorno      TYPE TABLE OF zsdt_retlote,
        tb_produtor     TYPE TABLE OF zdco_produtor,
        tb_zsdt_export  TYPE TABLE OF zsdt_export,
        tb_zsdt_retlote TYPE TABLE OF zsdt_retlote.

  DATA: wa_zfiwrt0019 TYPE zfiwrt0019.

  DATA: lv_foreign TYPE xfeld.                              "V1.10

  SELECT SINGLE reftyp refkey refitm
    INTO wa_item_nota
    FROM j_1bnflin
   WHERE docnum EQ in_doc-docnum.

  CLEAR: obs_export, obs_moeda, obs_bl, obs_navio.

  IF wa_item_nota-reftyp EQ 'BI'. "

    SELECT SINGLE vbeln fkart waerk kurrf netwr
      INTO wa_cab_fat
      FROM vbrk
     WHERE vbeln EQ wa_item_nota-refkey(10).

    "Se achou Documento de faturamento.
    IF sy-subrc EQ 0.

      "Busca Documento de Remessa.
      SELECT SINGLE vgbel vgpos volum voleh aubel
        INTO wa_itm_fat
        FROM vbrp
       WHERE vbeln EQ wa_cab_fat-vbeln.

      IF wa_cab_fat-fkart EQ 'ZEXI' OR "Nota Fiscal (NF) Exportação
         wa_cab_fat-fkart EQ 'ZEXP' OR "Nota Fiscal (NF) Exportação
         wa_cab_fat-fkart EQ 'ZFEX'.

        "Busca Informações de Valores Moeda Estrangeira
        IF wa_cab_fat-waerk NE 'BRL'.
          WRITE wa_cab_fat-kurrf TO test_taxa.
          SHIFT test_taxa LEFT DELETING LEADING space.
          WRITE wa_cab_fat-netwr TO test_valor.
          SHIFT test_valor LEFT DELETING LEADING space.
          CONCATENATE 'Moeda:' wa_cab_fat-waerk 'Taxa:' test_taxa 'Valor:' test_valor INTO obs_moeda SEPARATED BY space.
        ENDIF.

        "Busca de Registro de Exportação
        SELECT SINGLE id_doc_exp nr_registro_expo id_nomeacao_tran nr_dde numero_due id_due
          INTO wa_dox_exp
          FROM zdoc_exp
         WHERE vbeln EQ wa_itm_fat-vgbel.

        IF sy-subrc EQ 0.

          IF ( wa_dox_exp-id_due IS NOT INITIAL ).

            SELECT SINGLE *
              FROM zsdt0170 INTO @DATA(_wl_0170)
             WHERE id_due = @wa_dox_exp-id_due.

            IF sy-subrc EQ 0.
              CONCATENATE 'DU-e:' _wl_0170-numero_due INTO obs_export SEPARATED BY space.
              CONCATENATE obs_export 'RUC:'  _wl_0170-numero_ruc INTO obs_export SEPARATED BY space.
            ENDIF.

          ELSEIF wa_dox_exp-nr_registro_expo IS NOT INITIAL.
            CONCATENATE 'RE:' wa_dox_exp-nr_registro_expo INTO obs_export SEPARATED BY space.
          ELSE.

            SELECT SINGLE *
              FROM zsdt0053 INTO @DATA(lwa_zsdt0053)
             WHERE vbeln EQ @wa_itm_fat-aubel.

            IF ( sy-subrc EQ 0 ) AND ( wa_itm_fat-aubel IS NOT INITIAL ).

              IF ( lwa_zsdt0053-numero_ruc IS NOT INITIAL ).
                CONCATENATE obs_export 'RUC:'  lwa_zsdt0053-numero_ruc INTO obs_export SEPARATED BY space.
              ENDIF.

* RJF - INI - CS2023000420 Incluir colunas de drawback na ZSDT0062 para exportação de algodão #113631
              IF ( lwa_zsdt0053-tp_ato IS NOT INITIAL ).
                CONCATENATE obs_export 'Tipo Drawback:'  lwa_zsdt0053-tp_ato INTO obs_export SEPARATED BY space.
              ENDIF.
              IF ( lwa_zsdt0053-nr_drawback IS NOT INITIAL ).
                CONCATENATE obs_export 'Numero do Ato:'  lwa_zsdt0053-nr_drawback INTO obs_export SEPARATED BY space.
              ENDIF.
              IF ( lwa_zsdt0053-qtd_drawback IS NOT INITIAL ).
                lv_qtd_db = lwa_zsdt0053-qtd_drawback.
                CONCATENATE obs_export 'Quantidade Drawback(KG):'  lv_qtd_db INTO obs_export SEPARATED BY space.
              ENDIF.
* RJF - FIM - CS2023000420 Incluir colunas de drawback na ZSDT0062 para exportação de algodão #113631

              SELECT SINGLE *
                FROM zsdt0051 INTO @DATA(lwa_zsdt0051)
               WHERE nro_sol_ov EQ @lwa_zsdt0053-nro_sol_ov.

              IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt0053-nro_sol_ov IS NOT INITIAL ) AND ( lwa_zsdt0051-bstkd IS NOT INITIAL ).
                CONCATENATE obs_export 'CONTRATO:'  lwa_zsdt0051-bstkd INTO obs_export SEPARATED BY space.
              ENDIF.
            ENDIF.

          ENDIF.

          IF NOT wa_dox_exp-nr_dde IS INITIAL.
            CONCATENATE 'DDE:'  wa_dox_exp-nr_dde obs_export INTO obs_export SEPARATED BY space.
          ENDIF.

          "Busca B/L (Conhecimento de Transporte)
          SELECT ct~id_conhec INTO v_id_conhec
            FROM zdoc_rem_bl AS bls
            INNER JOIN znom_conhec     AS ct ON ct~id_conhec EQ bls~id_conhec
            INNER JOIN znom_transporte AS tr ON tr~id_nomeacao_tran EQ ct~id_nomeacao_tran
           WHERE bls~id_doc_exp EQ wa_dox_exp-id_doc_exp
             AND tr~id_nomeacao_tran EQ wa_dox_exp-id_nomeacao_tran.

            SELECT SINGLE * INTO wa_znom_conhec
              FROM znom_conhec
             WHERE id_conhec        EQ v_id_conhec
               AND id_nomeacao_tran EQ wa_dox_exp-id_nomeacao_tran.

            IF sy-subrc IS INITIAL.
              SHIFT wa_znom_conhec-nr_conhec LEFT DELETING LEADING space.
              IF obs_bl IS INITIAL.
                CONCATENATE wa_znom_conhec-ds_tipo ':' INTO obs_bl.
                CONCATENATE obs_bl wa_znom_conhec-nr_conhec INTO obs_bl SEPARATED BY space.
              ELSE.
                CONCATENATE obs_bl ',' INTO obs_bl.
                CONCATENATE obs_bl wa_znom_conhec-ds_tipo INTO obs_bl SEPARATED BY space.
                CONCATENATE obs_bl ':' INTO obs_bl.
                CONCATENATE obs_bl wa_znom_conhec-nr_conhec INTO obs_bl SEPARATED BY space.
              ENDIF.
            ENDIF.

          ENDSELECT.

          SELECT tr~ds_nome_transpor INTO test_navio
            FROM zdoc_rem_bl AS bls
            INNER JOIN znom_conhec     AS ct ON ct~id_conhec EQ bls~id_conhec
            INNER JOIN znom_transporte AS tr ON tr~id_nomeacao_tran EQ ct~id_nomeacao_tran
           WHERE bls~id_doc_exp      EQ wa_dox_exp-id_doc_exp
             AND tr~id_nomeacao_tran EQ wa_dox_exp-id_nomeacao_tran
           GROUP BY tr~ds_nome_transpor.
            SHIFT test_navio LEFT DELETING LEADING space.
            IF obs_navio IS INITIAL.
              CONCATENATE 'Navio:' test_navio INTO obs_navio SEPARATED BY space.
              CONCATENATE obs_navio ',' INTO obs_navio.
            ELSE.
              CONCATENATE obs_navio test_navio ',' INTO obs_navio SEPARATED BY space.
              CONCATENATE obs_navio ',' INTO obs_navio.
            ENDIF.
          ENDSELECT.
        ENDIF.

        READ TABLE in_partner INTO ls_j_1binnad WITH KEY parvw = 'SP'.
        CHECK NOT sy-subrc IS INITIAL.
      ENDIF.
    ENDIF.

  ENDIF.

  CONCATENATE obs_moeda obs_export obs_bl obs_navio INTO e_observacao SEPARATED BY space.

ENDMETHOD.


METHOD select_j1bnfdoc.

  CLEAR: p_parid ,
         p_nfenum.
  CHECK NOT p_docnum_en IS INITIAL.

  SELECT SINGLE parid nfenum
    FROM j_1bnfdoc
    INTO (p_parid, p_nfenum)
  WHERE  docnum EQ p_docnum_en.

ENDMETHOD.


METHOD select_kna1.

  CLEAR s_kna1.
  CHECK NOT p_kunnr IS INITIAL.

  SELECT SINGLE kunnr
                name1
                stcd1
                stcd2
                stcd3
                stras
                pstlz
                ort01
                regio
    FROM kna1
    INTO s_kna1
  WHERE  kunnr EQ p_kunnr.

ENDMETHOD.


METHOD select_konv.

  CHECK NOT p_knumv IS INITIAL.
  REFRESH t_konv.

*---> S4 MIGRATION 10/07/2023 - MA
*  SELECT knumv kposn stunr
*         zaehk kschl kbetr
*    FROM konv
*    INTO TABLE t_konv
*  WHERE  knumv EQ p_knumv
*    AND  kposn EQ p_kposn.

  SELECT knumv, kposn, stunr,
         zaehk, kschl, kbetr
    FROM v_konv
    INTO TABLE @DATA(lt_konv)
  WHERE  knumv EQ @p_knumv
    AND  kposn EQ @p_kposn.

  MOVE-CORRESPONDING lt_konv[] TO t_konv[].
*---> S4 MIGRATION 10/07/2023 - MA

  IF NOT p_kschl IS INITIAL.
    DELETE t_konv WHERE knumv NE p_knumv.
  ENDIF.

ENDMETHOD.


METHOD select_lfa1.

  CLEAR s_lfa1.
  CHECK NOT p_lifnr IS INITIAL.

  SELECT SINGLE lifnr
                name1
                stcd1
                stcd2
                stcd3
                stras
                pstlz
                ort01
                ort02
                regio
                ADRNR
    FROM lfa1
    INTO s_lfa1
  WHERE  lifnr EQ p_lifnr.

ENDMETHOD.


METHOD select_likp.

  CLEAR s_likp.
  CHECK NOT p_vbeln IS INITIAL.

  SELECT SINGLE vbeln vkorg inco1 vstel
    FROM likp
    INTO s_likp
  WHERE  vbeln EQ p_vbeln.

ENDMETHOD.


METHOD select_lips.

  REFRESH t_lips.
  CHECK NOT p_vgbel IS INITIAL.

    SELECT vbeln posnr pstyv
           lgort charg volum
           voleh vtweg matnr
           LFIMG VRKME uecha VGPOS KVGR3
           MATKL
      FROM lips
      INTO TABLE t_lips
    WHERE  vbeln eq p_vgbel.

*    SELECT vbeln posnr lgort
*           charg volum voleh
*           vtweg matnr
*      FROM lips
*      INTO TABLE t_lips
*    WHERE  vbeln EQ p_vgbel
*      AND  posnr EQ p_posnr.



ENDMETHOD.


METHOD select_nf_exportacao_recusa.

  DATA: wa_zdoc_exp_recusa TYPE zdoc_exp_recusa,
        vg_vbeln_ft_rec	   TYPE vbeln_vf,
        vg_posnr_ft_rec	   TYPE posnr_vf,
        wa_j_1bnfdoc       TYPE j_1bnfdoc,
        wa_j_1bnflin       TYPE j_1bnflin,
        wa_fatura_export   TYPE vbrp.

  CLEAR: e_observacao.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ i_docnum
     AND reftyp EQ 'BI'.

  CHECK sy-subrc IS INITIAL.

  "REFKEY J_1BREFKEY  CHAR  35  0 Referência ao documento de origem
  "REFITM J_1BREFITM  NUMC  6 0 Item de referência ao documento de origem

  vg_vbeln_ft_rec = wa_j_1bnflin-refkey(10).
  vg_posnr_ft_rec = wa_j_1bnflin-refitm.

  SELECT SINGLE * INTO wa_zdoc_exp_recusa
    FROM zdoc_exp_recusa
   WHERE vbeln_ft_rec EQ vg_vbeln_ft_rec
     AND posnr_ft_rec EQ vg_posnr_ft_rec.

  CHECK sy-subrc IS INITIAL.

  "Fatura da Exportação
  SELECT SINGLE * INTO wa_fatura_export
    FROM vbrp
   WHERE vgbel EQ wa_zdoc_exp_recusa-vbeln_re_exp
     AND vgpos EQ wa_zdoc_exp_recusa-posnr_re_exp
    AND draft EQ space. "---> S4 Migration - 10/07/2023 - MA.

  CHECK sy-subrc IS INITIAL.

  wa_j_1bnflin-refkey = wa_fatura_export-vbeln.
  wa_j_1bnflin-refitm = wa_fatura_export-posnr.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE reftyp EQ 'BI'
     AND refkey EQ wa_j_1bnflin-refkey
     AND refitm EQ wa_j_1bnflin-refitm.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO wa_j_1bnfdoc
    FROM j_1bnfdoc
   WHERE docnum EQ wa_j_1bnflin-docnum.

  CHECK sy-subrc IS INITIAL.

  IF wa_j_1bnfdoc-nfe IS INITIAL.

    MOVE wa_j_1bnfdoc-nfnum TO wa_j_1bnfdoc-nfenum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-nfenum
      IMPORTING
        output = wa_j_1bnfdoc-nfenum.

  ENDIF.

  CONCATENATE wa_j_1bnfdoc-docdat+6(2) '/' wa_j_1bnfdoc-docdat+4(2) '/' wa_j_1bnfdoc-docdat(4) INTO e_observacao.

  CONCATENATE 'Entrada Referente a Nota Fiscal de Exportação'
              'Número:' wa_j_1bnfdoc-nfenum
              'Série:'  wa_j_1bnfdoc-series
              'Modelo:' wa_j_1bnfdoc-model
              'Dt.Emissão:' e_observacao
         INTO e_observacao SEPARATED BY space.

ENDMETHOD.


METHOD select_nf_produtor.

  TYPES: BEGIN OF ty_qtd_ncm.
  TYPES:   ncm TYPE steuc,
           uni TYPE meins,
           qtd TYPE j_1bnetqty.
  TYPES: END OF ty_qtd_ncm.

  TYPES: BEGIN OF ty_notas.
  TYPES: cpf_cnpj     TYPE string,
         name1        TYPE name1_gp,
         vg_nunf      TYPE j_1bnfnum9,
         model        TYPE j_1bmodel,
         series       TYPE j_1bseries,
         menge        TYPE j_1bnetqty,
         menge_string TYPE c LENGTH 16,
         meins        TYPE j_1bnetunt,
         nbm          TYPE steuc.
  TYPES: END OF ty_notas.

  TYPES: BEGIN OF ty_notas_agrupa.
  TYPES: cpf_cnpj     TYPE string,
         name1        TYPE name1_gp,
         model        TYPE j_1bmodel,
         series       TYPE j_1bseries,
         meins        TYPE j_1bnetunt,
         nbm          TYPE steuc.
  TYPES: END OF ty_notas_agrupa.

  DATA: it_soma     TYPE TABLE OF ty_qtd_ncm INITIAL SIZE 0,
        it_prod     TYPE TABLE OF zdoc_nf_produtor INITIAL SIZE 0,
        it_nfdoc    TYPE TABLE OF j_1bnfdoc INITIAL SIZE 0,
        it_nflin    TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
        it_notas    TYPE TABLE OF ty_notas INITIAL SIZE 0,
        it_agrupa   TYPE TABLE OF ty_notas_agrupa INITIAL SIZE 0,
        wa_soma     TYPE ty_qtd_ncm,
        wa_soma_aux TYPE ty_qtd_ncm.

  DATA: wa_lin       TYPE j_1bnflin,
        wa_vbeln     TYPE vbeln_vf,
        wa_vbrp      TYPE vbrp,
        wa_prod      TYPE zdoc_nf_produtor,
        wa_nfdoc     TYPE j_1bnfdoc,
        wa_nflin     TYPE j_1bnflin,
        wa_part      TYPE lfa1,
        wa_notas     TYPE ty_notas,
        wa_agrupa    TYPE ty_notas_agrupa,
        vg_cpf       TYPE c LENGTH 14,
        vg_cnpj      TYPE c LENGTH 18,
        vg_nunf      TYPE j_1bnfnum9,
        cpf_cnpj     TYPE string,
        vg_nr_notas  TYPE string,
        vg_menge     TYPE j_1bnetqty,
        menge_string TYPE c LENGTH 16,
        it_numeros   TYPE TABLE OF ztnumeronfe INITIAL SIZE 0,
        wa_numeros   TYPE ztnumeronfe.

  CLEAR: e_observacao.

  SELECT SINGLE *
    INTO wa_lin
    FROM j_1bnflin
   WHERE docnum EQ p_docnum
     AND itmnum EQ p_itmnum.

  IF wa_lin-reftyp EQ 'BI'.

    wa_vbeln = wa_lin-refkey(10).

    SELECT SINGLE *
      INTO wa_vbrp
      FROM vbrp
     WHERE vbeln EQ wa_vbeln.

    IF wa_vbrp-vgbel IS NOT INITIAL.

      CLEAR: it_soma.

      SELECT *
        INTO TABLE it_prod
        FROM zdoc_nf_produtor
       WHERE vbeln EQ wa_vbrp-vgbel.

      CHECK sy-subrc IS INITIAL.

      SELECT *
        INTO TABLE it_nfdoc
        FROM j_1bnfdoc
         FOR ALL ENTRIES IN it_prod
       WHERE docnum EQ it_prod-docnum_prod.

      CHECK sy-subrc IS INITIAL.

      SORT it_nfdoc BY docnum.

      SELECT *
        INTO TABLE it_nflin
        FROM j_1bnflin
        FOR ALL ENTRIES IN it_prod
       WHERE docnum EQ it_prod-docnum_prod
         AND itmnum EQ it_prod-itmnum_prod.

      CHECK sy-subrc IS INITIAL.

      SORT it_nflin BY docnum itmnum.

      LOOP AT it_prod INTO wa_prod.

        READ TABLE it_nfdoc INTO wa_nfdoc WITH KEY docnum = wa_prod-docnum_prod BINARY SEARCH.
        READ TABLE it_nflin INTO wa_nflin WITH KEY docnum = wa_prod-docnum_prod itmnum = wa_prod-itmnum_prod BINARY SEARCH.

        IF NOT ( wa_nfdoc-name1 IS NOT INITIAL AND
             ( ( wa_nfdoc-stkzn IS NOT INITIAL AND wa_nfdoc-cpf IS NOT INITIAL ) OR
               ( wa_nfdoc-stkzn IS INITIAL AND wa_nfdoc-cgc IS NOT INITIAL ) ) ).

          CALL FUNCTION 'Z_PARCEIRO_INFO'
            EXPORTING
              p_parceiro   = wa_nfdoc-parid
              p_partype    = wa_nfdoc-partyp
            CHANGING
              wa_info_part = wa_part.

          wa_nfdoc-stkzn = wa_part-stkzn.
          wa_nfdoc-name1 = wa_part-name1.

          IF wa_part-stkzn IS INITIAL.
            wa_nfdoc-cgc = wa_part-stcd1.
          ELSE.
            wa_nfdoc-cpf = wa_part-stcd2.
          ENDIF.

        ENDIF.

        IF wa_nfdoc-stkzn IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
            EXPORTING
              input  = wa_nfdoc-cpf
            IMPORTING
              output = vg_cpf.
          CONCATENATE 'CPF:' vg_cpf INTO cpf_cnpj SEPARATED BY space.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              input  = wa_nfdoc-cgc
            IMPORTING
              output = vg_cnpj.
          CONCATENATE 'CNPJ:' vg_cnpj INTO cpf_cnpj SEPARATED BY space.
        ENDIF.

        IF wa_nfdoc-nfe IS NOT INITIAL.
          vg_nunf = wa_nfdoc-nfenum.
        ELSE.
          WRITE wa_nfdoc-nfnum TO vg_nunf.
          SHIFT vg_nunf LEFT DELETING LEADING space.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vg_nunf
            IMPORTING
              output = vg_nunf.
        ENDIF.

        WRITE wa_prod-menge TO menge_string.
        SHIFT menge_string LEFT DELETING LEADING space.
        CLEAR: wa_notas.
        wa_notas-cpf_cnpj     = cpf_cnpj.
        wa_notas-name1        = wa_nfdoc-name1.
        wa_notas-vg_nunf      = vg_nunf.
        wa_notas-model        = wa_nfdoc-model.
        wa_notas-series       = wa_nfdoc-series.
        wa_notas-menge        = wa_prod-menge.
        wa_notas-menge_string = menge_string.
        "A vinculação sempre é em KG
        wa_notas-meins        = 'KG'.
        wa_notas-nbm          = wa_nflin-nbm.
        APPEND wa_notas TO it_notas.
      ENDLOOP.

      SORT it_notas BY nbm meins.

      LOOP AT it_notas INTO wa_notas.
        wa_soma-ncm = wa_notas-nbm.
        wa_soma-uni = wa_notas-meins.
        wa_soma-qtd = wa_notas-menge.
        READ TABLE it_soma INTO wa_soma_aux WITH KEY ncm = wa_notas-nbm uni = wa_notas-meins.
        IF sy-subrc EQ 0.
          wa_soma_aux-qtd = wa_soma_aux-qtd + wa_notas-menge.
          MODIFY it_soma INDEX sy-tabix FROM wa_soma_aux TRANSPORTING qtd.
        ELSE.
          APPEND wa_soma TO it_soma.
        ENDIF.
        wa_agrupa-cpf_cnpj = wa_notas-cpf_cnpj.
        wa_agrupa-name1    = wa_notas-name1.
        wa_agrupa-model    = wa_notas-model.
        wa_agrupa-series   = wa_notas-series.
        wa_agrupa-meins    = wa_notas-meins.
        wa_agrupa-nbm      = wa_notas-nbm.
        APPEND wa_agrupa TO it_agrupa.
      ENDLOOP.

      SORT it_notas  BY cpf_cnpj name1 model series meins nbm.
      SORT it_agrupa BY cpf_cnpj name1 model series meins nbm.

      DELETE ADJACENT DUPLICATES FROM it_agrupa COMPARING ALL FIELDS.

      LOOP AT it_agrupa INTO wa_agrupa.
        CONCATENATE e_observacao wa_agrupa-cpf_cnpj 'Nome:' wa_agrupa-name1 INTO e_observacao SEPARATED BY space.
        vg_menge = 0.
        CLEAR: vg_nr_notas, it_numeros.
        LOOP AT it_notas INTO wa_notas WHERE cpf_cnpj = wa_agrupa-cpf_cnpj
                                         AND name1    = wa_agrupa-name1
                                         AND model    = wa_agrupa-model
                                         AND series   = wa_agrupa-series
                                         AND meins    = wa_agrupa-meins
                                         AND nbm      = wa_agrupa-nbm .
          vg_menge = vg_menge + wa_notas-menge.
          wa_numeros-nfenum = wa_notas-vg_nunf.
          APPEND wa_numeros TO it_numeros.
        ENDLOOP.

        CALL FUNCTION 'Z_AGRUPA_NUMERACAO'
          IMPORTING
            txt_agrupa = vg_nr_notas
          TABLES
            it_numeros = it_numeros.

        WRITE vg_menge TO menge_string.
        SHIFT menge_string LEFT DELETING LEADING space.
        CONCATENATE e_observacao 'Nr.:' vg_nr_notas 'Modelo:' wa_agrupa-model 'Série:' wa_agrupa-series 'Qtd.:' menge_string wa_agrupa-meins 'NCM:' wa_agrupa-nbm INTO e_observacao SEPARATED BY space.
      ENDLOOP.

      LOOP AT it_soma INTO wa_soma.
        WRITE wa_soma-qtd TO menge_string.
        SHIFT menge_string LEFT DELETING LEADING space.
        CONCATENATE e_observacao 'NCM:' wa_soma-ncm 'Qtd.:' menge_string wa_soma-uni INTO e_observacao SEPARATED BY space.
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD select_nf_remessa_form_lote.

  TYPES: BEGIN OF ty_notas_agrupa.
  TYPES: cpf_cnpj TYPE string,
         name1    TYPE name1_gp,
         model    TYPE j_1bmodel,
         series   TYPE j_1bseries,
         meins    TYPE j_1bnetunt,
         nbm      TYPE steuc.
  TYPES: END OF ty_notas_agrupa.

  TYPES: BEGIN OF ty_notas.
  TYPES: cpf_cnpj     TYPE string,
         name1        TYPE name1_gp,
         vg_nunf      TYPE j_1bnfnum9,
         model        TYPE j_1bmodel,
         series       TYPE j_1bseries,
         menge        TYPE j_1bnetqty,
         menge_string TYPE c LENGTH 16,
         meins        TYPE j_1bnetunt,
         nbm          TYPE steuc.
  TYPES: END OF ty_notas.

  TYPES: BEGIN OF ty_qtd_ncm.
  TYPES: ncm TYPE steuc,
         uni TYPE meins,
         qtd TYPE j_1bnetqty.
  TYPES: END OF ty_qtd_ncm.

  DATA: it_zsdt_retlote TYPE TABLE OF zsdt_retlote INITIAL SIZE 0,
        it_agrupa       TYPE TABLE OF ty_notas_agrupa INITIAL SIZE 0,
        wa_zsdt_retlote TYPE zsdt_retlote,
        wa_agrupa       TYPE ty_notas_agrupa,
        it_nfdoc        TYPE TABLE OF j_1bnfdoc INITIAL SIZE 0,
        it_nflin        TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
        it_notas        TYPE TABLE OF ty_notas INITIAL SIZE 0,
        it_soma         TYPE TABLE OF ty_qtd_ncm INITIAL SIZE 0,
        wa_nfdoc        TYPE j_1bnfdoc,
        wa_nflin        TYPE j_1bnflin,
        wa_info_c       TYPE kna1,
        vg_cpf          TYPE c LENGTH 14,
        vg_cnpj         TYPE c LENGTH 18,
        menge_string    TYPE c LENGTH 16,
        cpf_cnpj        TYPE string,
        vg_nunf         TYPE j_1bnfnum9,
        wa_notas        TYPE ty_notas,
        wa_soma         TYPE ty_qtd_ncm,
        wa_soma_aux     TYPE ty_qtd_ncm,
        vg_nr_notas     TYPE string,
        vg_menge        TYPE j_1bnetqty,
        it_numeros      TYPE TABLE OF ztnumeronfe INITIAL SIZE 0,
        wa_numeros      TYPE ztnumeronfe.

  CLEAR e_observacao.

  CHECK NOT i_docnum IS INITIAL.

  SELECT * INTO TABLE it_zsdt_retlote
    FROM zsdt_retlote
   WHERE docnum_ret EQ i_docnum.

  CHECK sy-subrc IS INITIAL.

  SELECT *
    INTO TABLE it_nfdoc
    FROM j_1bnfdoc
     FOR ALL ENTRIES IN it_zsdt_retlote
   WHERE docnum EQ it_zsdt_retlote-docnum.

  CHECK sy-subrc IS INITIAL.

  SORT it_nfdoc BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_nfdoc COMPARING docnum.

  SELECT *
    INTO TABLE it_nflin
    FROM j_1bnflin
    FOR ALL ENTRIES IN it_nfdoc
   WHERE docnum EQ it_nfdoc-docnum.

  CHECK sy-subrc IS INITIAL.

*---> S4 MIGRATION 10/07/2023 - MA
  SORT it_nfdoc BY docnum.
  SORT it_nflin BY docnum.
*<--- S4 MIGRATION 10/07/2023 - MA

  LOOP AT it_zsdt_retlote INTO wa_zsdt_retlote.

    READ TABLE it_nfdoc INTO wa_nfdoc WITH KEY docnum = wa_zsdt_retlote-docnum BINARY SEARCH.
    READ TABLE it_nflin INTO wa_nflin WITH KEY docnum = wa_zsdt_retlote-docnum BINARY SEARCH.

    IF NOT ( wa_nfdoc-name1 IS NOT INITIAL AND
         ( ( wa_nfdoc-stkzn IS NOT INITIAL AND wa_nfdoc-cpf IS NOT INITIAL ) OR
           ( wa_nfdoc-stkzn IS INITIAL AND wa_nfdoc-cgc IS NOT INITIAL ) ) ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro = wa_nfdoc-parid
          p_partype  = wa_nfdoc-partyp
        CHANGING
          wa_info_c  = wa_info_c.

      wa_nfdoc-stkzn = wa_info_c-stkzn.
      wa_nfdoc-name1 = wa_info_c-name1.

      IF wa_info_c-stkzn IS INITIAL.
        wa_nfdoc-cgc = wa_info_c-stcd1.
      ELSE.
        wa_nfdoc-cpf = wa_info_c-stcd2.
      ENDIF.

    ENDIF.

    IF wa_nfdoc-stkzn IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = wa_nfdoc-cpf
        IMPORTING
          output = vg_cpf.
      CONCATENATE 'CPF:' vg_cpf INTO cpf_cnpj SEPARATED BY space.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = wa_nfdoc-cgc
        IMPORTING
          output = vg_cnpj.
      CONCATENATE 'CNPJ:' vg_cnpj INTO cpf_cnpj SEPARATED BY space.
    ENDIF.

    IF wa_nfdoc-nfe IS NOT INITIAL.
      vg_nunf = wa_nfdoc-nfenum.
    ELSE.
      WRITE wa_nfdoc-nfnum TO vg_nunf.
      SHIFT vg_nunf LEFT DELETING LEADING space.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_nunf
        IMPORTING
          output = vg_nunf.
    ENDIF.

    WRITE wa_zsdt_retlote-quant_vinc TO menge_string.
    SHIFT menge_string LEFT DELETING LEADING space.
    CLEAR: wa_notas.
    wa_notas-cpf_cnpj     = cpf_cnpj.
    wa_notas-name1        = wa_nfdoc-name1.
    wa_notas-vg_nunf      = vg_nunf.
    wa_notas-model        = wa_nfdoc-model.
    wa_notas-series       = wa_nfdoc-series.
    wa_notas-menge        = wa_zsdt_retlote-quant_vinc.
    wa_notas-menge_string = menge_string.
    wa_notas-meins        = wa_nflin-meins.
    wa_notas-nbm          = wa_nflin-nbm.
    APPEND wa_notas TO it_notas.
  ENDLOOP.

  SORT it_notas BY nbm meins.

  LOOP AT it_notas INTO wa_notas.
    wa_soma-ncm = wa_notas-nbm.
    wa_soma-uni = wa_notas-meins.
    wa_soma-qtd = wa_notas-menge.
    READ TABLE it_soma INTO wa_soma_aux WITH KEY ncm = wa_notas-nbm uni = wa_notas-meins.
    IF sy-subrc EQ 0.
      wa_soma_aux-qtd = wa_soma_aux-qtd + wa_notas-menge.
      MODIFY it_soma INDEX sy-tabix FROM wa_soma_aux TRANSPORTING qtd.
    ELSE.
      APPEND wa_soma TO it_soma.
    ENDIF.
    wa_agrupa-cpf_cnpj = wa_notas-cpf_cnpj.
    wa_agrupa-name1    = wa_notas-name1.
    wa_agrupa-model    = wa_notas-model.
    wa_agrupa-series   = wa_notas-series.
    wa_agrupa-meins    = wa_notas-meins.
    wa_agrupa-nbm      = wa_notas-nbm.
    APPEND wa_agrupa TO it_agrupa.
  ENDLOOP.

  SORT it_notas  BY cpf_cnpj name1 model series meins nbm.
  SORT it_agrupa BY cpf_cnpj name1 model series meins nbm.

  DELETE ADJACENT DUPLICATES FROM it_agrupa COMPARING ALL FIELDS.

  LOOP AT it_agrupa INTO wa_agrupa.
    CONCATENATE e_observacao wa_agrupa-cpf_cnpj 'Nome:' wa_agrupa-name1 INTO e_observacao SEPARATED BY space.
    vg_menge = 0.
    CLEAR: vg_nr_notas, it_numeros.
    LOOP AT it_notas INTO wa_notas WHERE cpf_cnpj = wa_agrupa-cpf_cnpj
                                     AND name1    = wa_agrupa-name1
                                     AND model    = wa_agrupa-model
                                     AND series   = wa_agrupa-series
                                     AND meins    = wa_agrupa-meins
                                     AND nbm      = wa_agrupa-nbm .
      vg_menge = vg_menge + wa_notas-menge.

      wa_numeros-nfenum = wa_notas-vg_nunf.
      APPEND wa_numeros TO it_numeros.
    ENDLOOP.

    CALL FUNCTION 'Z_AGRUPA_NUMERACAO'
      IMPORTING
        txt_agrupa = vg_nr_notas
      TABLES
        it_numeros = it_numeros.

    WRITE vg_menge TO menge_string.
    SHIFT menge_string LEFT DELETING LEADING space.
    "Comentado a
    "CONCATENATE e_observacao 'Nr.:' vg_nr_notas 'Modelo:' wa_agrupa-model 'Série:' wa_agrupa-series
    "      'Qtd.:' menge_string wa_agrupa-meins 'NCM:' wa_agrupa-nbm INTO e_observacao SEPARATED BY space.
  ENDLOOP.

  LOOP AT it_soma INTO wa_soma.
    WRITE wa_soma-qtd TO menge_string.
    SHIFT menge_string LEFT DELETING LEADING space.
    CONCATENATE e_observacao 'NCM:' wa_soma-ncm 'Qtd.:' menge_string wa_soma-uni INTO e_observacao SEPARATED BY space.
  ENDLOOP.

ENDMETHOD.


METHOD select_seguro.

  DATA: BEGIN OF wa_parid,
          parid  TYPE j_1bparid,
          partyp TYPE j_1bpartyp,
          txjcd  TYPE txjcd,
        END OF wa_parid.

  DATA: BEGIN OF wa_parid_z1,
          parid  TYPE j_1bparid,
          partyp TYPE j_1bpartyp,
          txjcd  TYPE txjcd,
        END OF wa_parid_z1.

  DATA: vg_cd_material TYPE matnr,
        vg_quantidade  TYPE j_1bnetqty,
        vg_tot_aux     TYPE p DECIMALS 2,
        test_string    TYPE c LENGTH 20,
        part           TYPE lfa1.

  DATA: tl_vbrp       TYPE TABLE OF znfe_vbrp,
        sl_vbrp       TYPE znfe_vbrp         ,
        tl_xvbadr     TYPE TABLE OF sadrvb   ,
        tl_xvbpa      TYPE TABLE OF vbpavb   ,
        wa_xvbpa      TYPE vbpavb,
        wa_zvalor     TYPE zvalor_seg_terc,
        wa_a924       TYPE a924,
        wa_konp       TYPE konp,
        vr_pauta_ton  TYPE zvalor_seg_terc-vr_tot_ton.

  CALL METHOD me->select_vbrp
    EXPORTING
      p_refkey = p_refkey
      p_posnr  = p_itmnum
    IMPORTING
      t_vbrp   = tl_vbrp.

  READ TABLE tl_vbrp INTO sl_vbrp INDEX 1.

  CALL METHOD me->function_partner
    EXPORTING
      p_vbeln  = sl_vbrp-vbeln
    IMPORTING
      t_vbpavb = tl_xvbpa
      t_sadrvb = tl_xvbadr.

  DELETE tl_xvbpa WHERE parvw NE 'Z1'.

  IF NOT tl_xvbpa[] IS INITIAL.

    READ TABLE tl_xvbpa[] INTO wa_xvbpa INDEX 1.

    wa_parid_z1-parid  = wa_xvbpa-lifnr.
    wa_parid_z1-partyp = 'V'.

    SELECT SINGLE matnr INTO vg_cd_material
      FROM j_1bnflin
     WHERE docnum EQ p_docnum.

    SELECT SINGLE menge INTO vg_quantidade
      FROM j_1bnflin
     WHERE docnum EQ p_docnum.

    SELECT SINGLE parid partyp
      INTO wa_parid
      FROM j_1bnfnad
     WHERE docnum EQ p_docnum
       AND parvw  EQ 'LR'.

    IF sy-subrc EQ 0.

      vr_pauta_ton = 0.

      SELECT SINGLE * INTO wa_a924
        FROM a924
       WHERE kappl    EQ 'V'
         AND kschl    EQ 'ZIVP'
         AND aland    EQ 'BR'
         AND txreg_sf EQ p_c1_uf
         AND inco1    EQ 'CIF'
         AND matnr    EQ vg_cd_material
         AND kfrst    EQ space
         AND datab    LE p_data
         AND datbi    GT p_data.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO wa_konp
          FROM konp
         WHERE knumh EQ wa_a924-knumh
           AND kappl EQ wa_a924-kappl
           AND kschl EQ wa_a924-kschl.
        IF sy-subrc IS INITIAL.
          IF wa_konp-kmein EQ 'KG'.
            vr_pauta_ton = ( wa_konp-kbetr * 1000 ).
          ELSEIF wa_konp-kmein EQ 'TO'.
            vr_pauta_ton = wa_konp-kbetr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF wa_parid-parid NE wa_parid_z1-parid.

        CLEAR: part.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = wa_parid-parid
            p_partype    = wa_parid-partyp
          CHANGING
            wa_info_part = part.

        wa_parid-txjcd = part-txjcd.

        CLEAR: part.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = wa_parid_z1-parid
            p_partype    = wa_parid_z1-partyp
          CHANGING
            wa_info_part = part.

        wa_parid_z1-txjcd = part-txjcd.

        IF ( NOT wa_parid_z1-txjcd IS INITIAL ) AND ( NOT wa_parid-txjcd IS INITIAL ).

          SELECT * INTO wa_zvalor
            FROM zvalor_seg_terc
           WHERE cd_cidade_ini EQ wa_parid-txjcd
             AND cd_cidade_fim EQ wa_parid_z1-txjcd
             AND cd_material   EQ vg_cd_material
             AND dt_inicio     LE p_data.

            IF (  wa_zvalor-dt_inicio LE p_data AND wa_zvalor-dt_final IS INITIAL ) OR ( wa_zvalor-dt_inicio LE p_data AND wa_zvalor-dt_final GT p_data ).

              IF vr_pauta_ton GT wa_zvalor-vr_tot_ton.
                wa_zvalor-vr_tot_ton = vr_pauta_ton.
              ENDIF.

              vg_tot_aux = ( wa_zvalor-vr_tot_ton / 1000 ) * vg_quantidade.
              WRITE vg_tot_aux TO test_string.
              SHIFT test_string LEFT DELETING LEADING space.
              CONCATENATE 'VALOR PARA INDENIZACAO: R$' test_string INTO e_observacao SEPARATED BY space.

              vg_tot_aux = wa_zvalor-vr_tot_ton.
              WRITE vg_tot_aux TO test_string.
              SHIFT test_string LEFT DELETING LEADING space.
              CONCATENATE e_observacao 'TON: R$' test_string INTO e_observacao SEPARATED BY space.
            ENDIF.

          ENDSELECT.
        ENDIF.

      ENDIF.
      "B  Local de negócios
      "C  Cliente
      "V  Forneced.

    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD select_t006a.

  CHECK NOT p_msehi IS INITIAL.

  SELECT SINGLE spras msehi msehl
    FROM t006a
    INTO s_t006a
  WHERE  spras EQ 'PT'
    AND  msehi EQ p_msehi.

ENDMETHOD.


METHOD select_vbak.

  REFRESH t_vbak.
  CHECK NOT p_aubel IS INITIAL.

  SELECT vbeln auart vgbel bukrs_vf
         knumv KUNNR
    FROM vbak
    INTO TABLE t_vbak
  WHERE  vbeln EQ p_aubel.

ENDMETHOD.


METHOD select_vbap.

  REFRESH t_vbap.
  CHECK NOT p_vbeln IS INITIAL.

  SELECT vbeln posnr werks matnr charg
    FROM vbap
    INTO TABLE t_vbap
  WHERE  vbeln EQ p_vbeln
    AND  posnr EQ p_posnr.

ENDMETHOD.


  method SELECT_VBKD.

    REFRESH t_vbkd.
    CHECK NOT p_vbeln IS INITIAL.

    SELECT vbeln posnr bstkd_e
      FROM vbkd INTO TABLE t_vbkd
     WHERE vbeln EQ p_vbeln
       AND posnr EQ p_posnr.

  endmethod.


METHOD select_vbrp.

  DATA vl_vbeln TYPE vbrp-vbeln.

  REFRESH t_vbrp.
  CHECK NOT p_refkey IS INITIAL.
  vl_vbeln = p_refkey.

  SELECT vbeln posnr aubel vgbel
    FROM vbrp
    INTO TABLE t_vbrp
  WHERE  vbeln EQ vl_vbeln
   AND   posnr EQ p_posnr.

ENDMETHOD.


METHOD select_zdco_nf_entrada.

  REFRESH t_nf_entrada.
  CHECK NOT p_nu_dco IS INITIAL.

  SELECT *
    FROM zdco_nf_entrada
    INTO TABLE t_nf_entrada
  WHERE  nu_dco EQ p_nu_dco.

ENDMETHOD.


METHOD select_zdco_produtor.

  REFRESH t_produtor.
  CHECK NOT p_vbeln IS INITIAL.

  SELECT *
    FROM zdco_produtor
    INTO TABLE t_produtor
  WHERE  vbeln EQ p_vbeln.

ENDMETHOD.


METHOD select_zsdt0001.

  REFRESH t_zsdt0001.
  CHECK NOT p_vbeln IS INITIAL.

  SELECT *
    FROM zsdt0001
    INTO TABLE t_zsdt0001
  WHERE  vbeln   EQ p_vbeln
    AND  bukrs   EQ p_bukrs
    AND  branch  EQ p_werks
    AND  doc_rem EQ p_vgbel
    AND  tp_movimento EQ 'S'.

ENDMETHOD.


METHOD select_zsdt0004.

  REFRESH t_zsdt0004.

  CHECK NOT p_docnum IS INITIAL.

  SELECT *
    FROM zsdt0004
    INTO TABLE t_zsdt0004
  WHERE  docnum EQ p_docnum.

ENDMETHOD.


METHOD select_zsdt0006.

  REFRESH t_zsdt0006.
  CHECK NOT p_auart IS INITIAL.

  SELECT *
    FROM zsdt0006
    INTO TABLE t_zsdt0006
  WHERE  auart EQ p_auart.

ENDMETHOD.


  METHOD SELECT_ZSDT0294.

    REFRESH t_zsdt0294.
    "CHECK NOT p_auart IS INITIAL.

    IF p_auart IS NOT INITIAL.
      SELECT *
        FROM zsdt0294
        INTO TABLE t_zsdt0294
       WHERE auart     EQ p_auart
         AND cancelado NE 'X'.
    ENDIF.

    SELECT *
      FROM zsdt0294
      APPENDING TABLE t_zsdt0294
     WHERE auart     EQ abap_off
       AND cancelado NE 'X'.

  ENDMETHOD.


  METHOD set_dados_transportadora.

    FIELD-SYMBOLS: <fs_xmlh> TYPE j1b_nf_xml_header.

    DATA: t_sadrvb TYPE TABLE OF sadrvb,
          t_vbpavb TYPE TABLE OF vbpavb.

*    TYPES: y_vbtyp_v TYPE RANGE OF vbfa-vbtyp_v.
*
*    DATA: r_vbtyp_v TYPE y_vbtyp_v.
*
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'J' ) TO r_vbtyp_v.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'M' ) TO r_vbtyp_v.

    ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.

    CHECK <fs_xmlh> IS ASSIGNED.

    IF I_CLEAR EQ ABAP_TRUE.
      CLEAR: <fs_xmlh>-t1_cnpj,
             <fs_xmlh>-t1_cpf,
             <fs_xmlh>-t1_xnome,
             <fs_xmlh>-t1_ie,
             <fs_xmlh>-t1_xend,
             <fs_xmlh>-t1_xmun,
             <fs_xmlh>-t1_uf.
      RETURN.
    ENDIF.

    DATA(lwa_lfa1_transp) =  me->get_dados_transportadora( ).

    <fs_xmlh>-t1_cnpj  = lwa_lfa1_transp-stcd1.
    <fs_xmlh>-t1_xnome = lwa_lfa1_transp-name1.
    <fs_xmlh>-t1_ie    = zcl_string=>replace( EXPORTING i_str = CONV #( lwa_lfa1_transp-stcd3 )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).
    <fs_xmlh>-t1_xend  = lwa_lfa1_transp-stras.
    <fs_xmlh>-t1_xmun  = lwa_lfa1_transp-ort01.
    <fs_xmlh>-t1_uf    = lwa_lfa1_transp-txjcd(2).

    IF lwa_lfa1_transp-stkzn IS NOT INITIAL.
      <fs_xmlh>-t1_cpf = lwa_lfa1_transp-stcd2.
    ELSE.
      CLEAR: <fs_xmlh>-t1_cpf.
    ENDIF.


*    CHECK ( <fs_xmlh>-t1_cnpj IS INITIAL ) AND ( <fs_xmlh>-t1_cpf IS INITIAL ).
*
*    SELECT SINGLE *
*      FROM j_1bnflin INTO @DATA(vj_1bnflin)
*     WHERE docnum EQ @<fs_xmlh>-docnum.
*
*    CHECK sy-subrc EQ 0.
*
*    SELECT SINGLE *
*      FROM vbfa INTO @DATA(vvbfa)
*     WHERE vbeln   EQ @vj_1bnflin-refkey(10)
*       AND posnn   EQ @vj_1bnflin-refitm
*       AND vbtyp_v IN @r_vbtyp_v.
*
*    IF sy-subrc EQ 0.
*
*      CALL FUNCTION 'SD_PARTNER_READ'
*        EXPORTING
*          f_vbeln  = vvbfa-vbelv
*          object   = 'VBPA'
*        TABLES
*          i_xvbadr = t_sadrvb
*          i_xvbpa  = t_vbpavb.
*
*      DELETE t_vbpavb WHERE parvw NE 'SP'.
*
*      CHECK t_vbpavb[] IS NOT INITIAL.
*
*      READ TABLE t_vbpavb INTO DATA(sl_xvbpa) INDEX 1.
*
*      SELECT SINGLE *
*        FROM lfa1 INTO @DATA(s_lfa1)
*       WHERE lifnr EQ @sl_xvbpa-lifnr.
*
*      CHECK sy-subrc EQ 0.
*
*      <fs_xmlh>-t1_cnpj  = s_lfa1-stcd1.
*      <fs_xmlh>-t1_xnome = s_lfa1-name1.
*      <fs_xmlh>-t1_ie    = zcl_string=>replace( EXPORTING i_str = CONV #( s_lfa1-stcd3 )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).
*      <fs_xmlh>-t1_xend  = s_lfa1-stras.
*      <fs_xmlh>-t1_xmun  = s_lfa1-ort01.
*      <fs_xmlh>-t1_uf    = s_lfa1-txjcd(2).
*
*      IF s_lfa1-stkzn IS NOT INITIAL.
*        <fs_xmlh>-t1_cpf = s_lfa1-stcd2.
*      ENDIF.
*
*    ELSE.
*
*      SELECT SINGLE *
*        FROM zfiwrt0019 INTO @DATA(lw_zfiwrt0019)
*       WHERE seq_lcto EQ @vj_1bnflin-refkey(10).
*
*      CHECK ( sy-subrc EQ 0 ).
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lw_zfiwrt0019-lifnr
*        IMPORTING
*          output = lw_zfiwrt0019-lifnr.
*
*      SELECT SINGLE *
*        FROM lfa1 INTO s_lfa1
*       WHERE lifnr EQ lw_zfiwrt0019-lifnr.
*
*      CHECK sy-subrc EQ 0.
*
*      <fs_xmlh>-t1_cnpj  = s_lfa1-stcd1.
*      <fs_xmlh>-t1_xnome = s_lfa1-name1.
*      <fs_xmlh>-t1_ie    = zcl_string=>replace( EXPORTING i_str = CONV #( s_lfa1-stcd3 )  i_with_regex = abap_true i_char_regex =  '[^0-9]' ).
*      <fs_xmlh>-t1_xend  = s_lfa1-stras.
*      <fs_xmlh>-t1_xmun  = s_lfa1-ort01.
*      <fs_xmlh>-t1_uf    = s_lfa1-txjcd(2).
*
*      IF s_lfa1-stkzn IS NOT INITIAL.
*        <fs_xmlh>-t1_cpf = s_lfa1-stcd2.
*      ENDIF.
*
*    ENDIF.


  ENDMETHOD.


  METHOD verifica_icms_desonerado.

    DATA: p_pais_def  TYPE  land1,
          p_regio_def TYPE  regio.

    CLEAR: p_desonerado, p_rate.

*------------------------------------------------------------------------------------------------------------------*
*  Aqui colocamos somente as leis fiscais que dever sair o desconto da NF na operações DENTRO DO ESTADO
*  Temos um estado de exceção que é o de RO, que dsair o desconto na NF independente se é dentro ou fora do estado
*------------------------------------------------------------------------------------------------------------------*

    CLEAR: p_ck_lei_fiscal. "Utilizado para GRC

    "CHECK P_PARTYP EQ 'C'.

    IF p_pais EQ 'BR'.

      CASE p_regio.
        WHEN 'PA'.
          IF ( p_taxlw1 EQ 'P89' ) OR
             ( p_taxlw1 EQ 'P80' ).
            p_ck_lei_fiscal = abap_true.  "Utilizado para GRC
          ENDIF.
        WHEN 'RR'.

          IF ( p_taxlw1 EQ 'RR2' ) OR
             ( p_taxlw1 EQ 'RR3' ) OR
             ( p_taxlw1 EQ 'RR4' ) OR
             ( p_taxlw1 EQ 'RR5' ).
            p_ck_lei_fiscal = abap_true.  "Utilizado para GRC
          ENDIF.

        WHEN 'RO'.

          IF ( p_taxlw1 EQ 'R58' ) OR
             ( p_taxlw1 EQ 'M86' )

" IR229922 - Ajuste regra conforme sinalizado somente ‘RO’ - 19/03/2025 - Inicio
             OR ( p_taxlw1 EQ 'R60' )
             OR ( p_taxlw1 EQ 'R75' ).
            " IR229922 - Ajuste regra conforme sinalizado somente ‘RO’ - 19/03/2025 - Fim

            p_ck_lei_fiscal = abap_true.  "Utilizado para GRC
          ENDIF.

        WHEN 'MA'.

          IF ( p_taxlw1 EQ 'O46' ) OR
             ( p_taxlw1 EQ 'O50' ) OR
             ( p_taxlw1 EQ 'O48' ).
            p_ck_lei_fiscal = abap_true.  "Utilizado para GRC
          ENDIF.

        WHEN 'TO'.

          IF ( p_taxlw1 EQ 'T45' ) OR
             ( p_taxlw1 EQ 'T49' ) OR
             ( p_taxlw1 EQ 'T47' ).
            p_ck_lei_fiscal = abap_true.  "Utilizado para GRC
          ENDIF.

        WHEN 'PI'.

          IF ( p_taxlw1 EQ 'PI4' ) OR
             ( p_taxlw1 EQ 'PI7' ).
            p_ck_lei_fiscal = abap_true.   "Utilizado para GRC
          ENDIF.

        WHEN OTHERS.


      ENDCASE.

      CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
        EXPORTING
          p_bukrs       = p_bukrs
          p_branch      = p_branch
          p_partyp_1    = p_partyp
          p_parid_1     = p_parid
          p_pais_def    = p_pais
          p_regio_def   = p_regio
          p_ck_parid_ex = abap_true
        IMPORTING
          p_dentro_def  = p_desonerado
          p_rate        = p_rate.

      "FI - Adicionado LEI R75 e R60 IR229922 / AOENNING
*      IF ( P_TAXLW1 EQ 'M86' ).
*        P_DESONERADO = 'X'.
*        P_CK_LEI_FISCAL = ABAP_TRUE.   "Utilizado para GRC
*      ENDIF.

      IF p_regio = 'RO' AND p_ck_lei_fiscal EQ abap_true.
        p_desonerado = 'X'.
      ENDIF.
      "FI - Adicionado LEI R75 e R60 IR229922 / AOENNING

    ENDIF.

  ENDMETHOD.


  method VERIFICA_ICMS_NAO_TRIBUTADO.

    DATA: P_PAIS_DEF  TYPE  LAND1,
          P_REGIO_DEF TYPE  REGIO.

*------------------------------------------------------------------------------------------------------------------*
*  Aqui colocamos somente as leis fiscais que dever sair o desconto da NF nas operações FORA DO ESTADO
*------------------------------------------------------------------------------------------------------------------*

    CLEAR: P_NAO_TRIBUTADO.

    CLEAR: P_RATE. "Utilizado para GRC

    CLEAR: P_CK_LEI_FISCAL. "Utilizado para GRC

    "CHECK P_PARTYP EQ 'C'.

    IF P_PAIS EQ 'BR'.

      CASE P_REGIO.
         WHEN 'RR'.

          IF ( P_TAXLW1 EQ 'RR2' ) OR
             ( P_TAXLW1 EQ 'RR3' ) OR
             ( P_TAXLW1 EQ 'RR4' ) OR
             ( P_TAXLW1 EQ 'RR5' ).
            P_CK_LEI_FISCAL = ABAP_TRUE.  "Utilizado para GRC
          ENDIF.

        WHEN 'MA'.

          IF ( P_TAXLW1 EQ 'O47' ) OR
             ( P_TAXLW1 EQ 'O49' ) OR
             ( P_TAXLW1 EQ 'O51' ).
            P_CK_LEI_FISCAL = ABAP_TRUE. "Utilizado para GRC
          ENDIF.

        WHEN 'TO'.

          IF ( P_TAXLW1 EQ 'T46' ) OR
             ( P_TAXLW1 EQ 'T48' ) OR
             ( P_TAXLW1 EQ 'T50' ).
            P_CK_LEI_FISCAL = ABAP_TRUE. "Utilizado para GRC
          ENDIF.

        WHEN 'PI'.

          IF ( P_TAXLW1 EQ 'PI5' ) OR
             ( P_TAXLW1 EQ 'PI6' ) OR
             ( P_TAXLW1 EQ 'PI8' ).
            P_CK_LEI_FISCAL = ABAP_TRUE. "Utilizado para GRC
          ENDIF.

        WHEN OTHERS.

      ENDCASE.

      CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
        EXPORTING
          P_BUKRS      = P_BUKRS
          P_BRANCH     = P_BRANCH
          P_PARTYP_1   = P_PARTYP
          P_PARID_1    = P_PARID
          P_PAIS_DEF   = P_PAIS
          P_REGIO_DEF  = P_REGIO
          P_CK_PARID_EX = ABAP_TRUE
        IMPORTING
          P_INTERESTADUAL_DEF = P_NAO_TRIBUTADO
          P_RATE              = P_RATE.

    ENDIF.

  endmethod.


METHOD VERIFICA_ICMS_RO_RO.

  DATA: P_PAIS_DEF  TYPE  LAND1,
        P_REGIO_DEF TYPE  REGIO.

*------------------------------------------------------------------------------------------------------------------*
*  Esse metodo , não se utiliza mais - ELe não é chamado em lugar nenhum
*------------------------------------------------------------------------------------------------------------------*

  CLEAR P_DESONERADO.

  CHECK ( P_PARTYP EQ 'C' ) AND ( P_TAXLW1 EQ 'R58'  ). " OR P_TAXLW1 EQ 'R75' OR P_TAXLW1 EQ 'R60' ). "Adicionado LEI R75 e R60 IR229922 / AOENNING.

  P_PAIS_DEF  = 'BR'.
  P_REGIO_DEF = 'RO'.

  CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
    EXPORTING
      P_BUKRS      = P_BUKRS
      P_BRANCH     = P_BRANCH
      P_PARTYP_1   = P_PARTYP
      P_PARID_1    = P_PARID
      P_PAIS_DEF   = P_PAIS_DEF
      P_REGIO_DEF  = P_REGIO_DEF
    IMPORTING
      P_DENTRO_DEF = P_DESONERADO
      P_RATE       = P_RATE.

ENDMETHOD.


  METHOD select_mseg.

    DATA: lv_zeile TYPE mblpo.

    FREE: r_mseg.

    CHECK i_refkey IS NOT INITIAL.

    DATA(_mblnr) = i_refkey(10).
    DATA(_mjahr) = i_refkey+10(4).
    lv_zeile     = i_refitm.

    SELECT *
      FROM mseg
      INTO TABLE r_mseg
     WHERE mblnr = _mblnr
       AND mjahr = _mjahr
       AND zeile = lv_zeile.

  ENDMETHOD.


  METHOD select_rseg.

    DATA: lv_buzei TYPE rblgp.

    FREE: r_rseg.

    CHECK i_refkey IS NOT INITIAL.

    DATA(_belnr) = i_refkey(10).
    DATA(_gjahr) = i_refkey+10(4).
    lv_buzei     = i_refitm.

    SELECT *
      FROM rseg
      INTO TABLE r_rseg
     WHERE belnr = _belnr
       AND gjahr = _gjahr
       AND buzei = lv_buzei.

    CHECK sy-subrc = 0.

    SELECT *
     INTO TABLE @DATA(t_jlin)
     FROM j_1bnflin
    WHERE refkey = @i_refkey
      AND refitm = @i_refitm.

    CHECK sy-subrc = 0.

    LOOP AT t_jlin INTO DATA(_jlin).
      _belnr          = _jlin-refkey(10).
      _gjahr          = _jlin-refkey+10(4).
      lv_buzei        = _jlin-refitm.

      READ TABLE r_rseg INTO DATA(_rseg) WITH KEY belnr = _belnr
                                                  gjahr = _gjahr
                                                  buzei = lv_buzei.
      IF sy-subrc = 0.
        _rseg-charg   = _jlin-charg.
        MODIFY r_rseg FROM _rseg INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method IF_EX_CL_NFE_PRINT~FILL_PRESUMED_CREDIT.
  endmethod.
ENDCLASS.
