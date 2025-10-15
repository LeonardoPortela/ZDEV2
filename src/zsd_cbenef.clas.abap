class ZSD_CBENEF definition
  public
  final
  create public .

public section.
  type-pools ZNFE .

  interfaces IF_BADI_INTERFACE .
  interfaces IF_J_1BNF_ADD_DATA .

  methods SET_CAB_OBSERVACAO_FISCO
    importing
      !I_HEADER type J_1BNFDOC
      !IT_NFLIN type J_1BNFLIN_TAB
    changing
      !E_HEADER type J_1BNF_BADI_HEADER .
  methods SET_ITEM_BENEFICIO
    importing
      !I_HEADER type J_1BNFDOC
      !I_ITEM_NOTA type J_1BNFLIN
      !I_NFSTX type J_1BNFSTX_TAB
    changing
      !E_ITEM_NFE type J_1BNF_BADI_ITEM .
  methods SET_ITEM_IMPORT
    importing
      !I_HEADER type J_1BNFDOC
      !I_ITEM_NOTA type J_1BNFLIN
      !I_NFSTX type J_1BNFSTX_TAB
    changing
      !E_ITEM_NFE type J_1BNF_BADI_ITEM
      !E_IMPORT_ADI type J_1BNFE_T_ADD_BADI_ADI_310
      !E_IMPORT_DI type J_1BNFE_T_BADI_DI_310 .
  methods SET_ITEM_UNID_TRIB
    importing
      !I_HEADER type J_1BNFDOC
      !I_ITEM_NOTA type J_1BNFLIN
      !I_NFSTX type J_1BNFSTX_TAB
    changing
      !E_ITEM_NFE type J_1BNF_BADI_ITEM .
  methods SET_ADD_DATA
    importing
      !IV_APPLIC type J_1B_APPLIC optional
      !IS_HEADER type J_1BNFDOC
      !IT_NFLIN type J_1BNFLIN_TAB
      !IT_NFSTX type J_1BNFSTX_TAB
      !IT_PARTNER type J_1B_TT_NFNAD
      !IT_OT_PARTNER type J_1B_TT_NFCPD optional
      !IS_RBKP type RBKP optional
      !IT_NFFTX type J_1BNFFTX_TAB optional
      !IT_NFREF type J_1BNFREF_TAB optional
      !IT_RSEG type J_1B_TT_RSEG optional
      !IS_MKPF type MKPF optional
      !IT_MSEG type TY_T_MSEG optional
      !IS_VBRK type VBRKVB optional
      !IT_VBRP type VBRP_TAB optional
      !IT_VBFA type VBFA_T optional
      !IO_XML_IN type ref to CL_J_1BNFE_XML optional
    changing
      !ES_HEADER type J_1BNF_BADI_HEADER
      !ET_ITEM type J_1BNF_BADI_ITEM_TAB
      !ET_TRANSVOL type J_1BNFTRANSVOL_TAB
      !ET_TRAILER type J_1BNFTRAILER_TAB
      !ET_TRADENOTES type J_1BNFTRADENOTES_TAB
      !ET_REFPROC type J_1BNFREFPROC_TAB
      !ET_ADD_INFO type J_1BNFADD_INFO_TAB
      !ET_SUGARSUPPL type J_1BNFSUGARSUPPL_TAB
      !ET_SUGARDEDUC type J_1BNFSUGARDEDUC_TAB
      !ET_PHARMACEUT type J_1BNFPHARMACEUT_TAB
      !ET_VEHICLE type J_1BNFVEHICLE_TAB
      !ET_FUEL type J_1BNFFUEL_TAB
      !ET_EXPORT type J_1BNFE_T_ADD_BADI_EXPORT optional
      !ET_IMPORT_ADI type J_1BNFE_T_ADD_BADI_ADI_310 optional
      !ET_IMPORT_DI type J_1BNFE_T_BADI_DI_310 optional
      !ET_NVE type J_1BNFE_T_BADI_NVE_310 optional
      !ET_TRACEABILITY type J_1BNFE_T_BADI_TRACE_400 optional
      !ET_PHARMA type J_1BNFE_T_BADI_PHARMA_400 optional
      !ET_PAYMENT type J_1BNFE_T_BADI_PAYMENT_400 optional
      !ES_TEC_RESP type J_1BNFE_S_BADI_TEC_RESP optional .
  methods VERIF_ICMS_NAO_TRIBUTADO
    importing
      !P_BUKRS type BUKRS
      !P_BRANCH type J_1BBRANC_
      !P_PARTYP type J_1BPARTYP
      !P_PARID type J_1BPARID
      !P_TAXLW1 type J_1BTAXLW1
      !P_PAIS type LAND1
      !P_REGIO type REGIO
    exporting
      !P_NAO_TRIBUTADO type CHAR01 .
  class-methods GET_RESP_TECNICO
    importing
      !I_UF type REGIO
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    returning
      value(R_RESPONSAVEL) type J_1BNFE_S_BADI_TEC_RESP .
  methods VERIF_ICMS_DESONERADO
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
      !P_RATE type J_1BTXRATE .
protected section.
private section.

  data AT_LFA1 type ZNFE_LFA1 .
  data AT_T006A type ZNFE_T006A .
  data AT_TI_ZSDT0004 type ZDE_ZSDT0004_T .
  data AT_TI_ZSDT0001 type ZSDT0001_T .
  data AT_TI_VBAK type ZNFE_VBAK_T .
  data AT_TI_VBAP type ZNFE_VBAP_T .
  data AT_TI_VBRP type ZNFE_VBRP_T .
  data AT_TI_LIPS type ZNFE_LIPS_T .
  data AT_TI_PRODUTOR type ZDE_ZDCO_PRODUTOR_T .
  data AT_TI_NF_ENTRADA type ZDE_ZDCO_NF_ENTRADA_T .
  data AT_TI_ZSDT0006 type ZDE_ZSDT0006_T .
  data AT_TI_ZSDT0191 type ZDE_ZSDT0191_T .
  data AT_LIKP type ZNFE_LIKP .
ENDCLASS.



CLASS ZSD_CBENEF IMPLEMENTATION.


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
      R_RESPONSAVEL-CNPJ    = SL_ADD_RESP-CNPJ.
      R_RESPONSAVEL-CONTACT = SL_ADD_RESP-CONTACT.
      R_RESPONSAVEL-EMAIL   = SL_ADD_RESP-EMAIL.
      R_RESPONSAVEL-PHONE   = SL_ADD_RESP-PHONE.
      R_RESPONSAVEL-IDCSRT  = SL_ADD_RESP-IDCSRT.
      R_RESPONSAVEL-CSRT    = SL_ADD_RESP-CSRT.
    ENDIF.

  ENDMETHOD.


  METHOD IF_J_1BNF_ADD_DATA~ADD_DATA.

    ME->SET_ADD_DATA(
      EXPORTING
        IV_APPLIC     = IV_APPLIC
        IS_HEADER     = IS_HEADER
        IT_NFLIN      = IT_NFLIN
        IT_NFSTX      = IT_NFSTX
        IT_PARTNER    = IT_PARTNER
        IT_OT_PARTNER = IT_OT_PARTNER
        IS_RBKP       = IS_RBKP
        IT_NFFTX      = IT_NFFTX
        IT_NFREF      = IT_NFREF
        IT_RSEG       = IT_RSEG
        IS_MKPF       = IS_MKPF
        IT_MSEG       = IT_MSEG
        IS_VBRK       = IS_VBRK
        IT_VBRP       = IT_VBRP
        IT_VBFA       = IT_VBFA
        IO_XML_IN     = IO_XML_IN
      CHANGING
        ES_HEADER     = ES_HEADER
        ET_ITEM       = ET_ITEM
        ET_TRANSVOL   = ET_TRANSVOL
        ET_TRAILER    = ET_TRAILER
        ET_TRADENOTES = ET_TRADENOTES
        ET_REFPROC    = ET_REFPROC
        ET_ADD_INFO   = ET_ADD_INFO
        ET_SUGARSUPPL = ET_SUGARSUPPL
        ET_SUGARDEDUC = ET_SUGARDEDUC
        ET_PHARMACEUT = ET_PHARMACEUT
        ET_VEHICLE    = ET_VEHICLE
        ET_FUEL       = ET_FUEL
        ET_EXPORT     = ET_EXPORT
        ET_IMPORT_ADI = ET_IMPORT_ADI
        ET_IMPORT_DI  = ET_IMPORT_DI
        ET_NVE        = ET_NVE
        ET_TRACEABILITY = ET_TRACEABILITY
        ET_PHARMA     = ET_PHARMA
        ET_PAYMENT    = ET_PAYMENT
        ES_TEC_RESP   = ES_TEC_RESP ).

  ENDMETHOD.


  METHOD IF_J_1BNF_ADD_DATA~ADD_DATA_J1B1N.

    ME->SET_ADD_DATA(
      EXPORTING
*        IV_APPLIC     =
        IS_HEADER     = IS_HEADER
        IT_NFLIN      = IT_NFLIN
        IT_NFSTX      = IT_NFSTX
        IT_PARTNER    = IT_PARTNER
        IT_OT_PARTNER = IT_OT_PARTNER
        IT_NFFTX      = IT_NFFTX
        IT_NFREF      = IT_NFREF
*       IT_RSEG       =
*       IS_MKPF       =
*       IT_MSEG       =
*       IS_VBRK       =
*       IT_VBRP       =
*       IT_VBFA       =
*       IO_XML_IN     =
      CHANGING
        ES_HEADER     = ES_HEADER
        ET_ITEM       = ET_ITEM
        ET_TRANSVOL   = ET_TRANSVOL
        ET_TRAILER    = ET_TRAILER
        ET_TRADENOTES = ET_TRADENOTES
        ET_REFPROC    = ET_REFPROC
        ET_ADD_INFO   = ET_ADD_INFO
        ET_SUGARSUPPL = ET_SUGARSUPPL
        ET_SUGARDEDUC = ET_SUGARDEDUC
        ET_PHARMACEUT = ET_PHARMACEUT
        ET_VEHICLE    = ET_VEHICLE
        ET_FUEL       = ET_FUEL
        ET_EXPORT     = ET_EXPORT
        ET_IMPORT_ADI = ET_IMPORT_ADI
        ET_IMPORT_DI  = ET_IMPORT_DI
        ET_NVE        = ET_NVE
        ET_TRACEABILITY = ET_TRACEABILITY
        ET_PHARMA     = ET_PHARMA
        ET_PAYMENT    = ET_PAYMENT
        ES_TEC_RESP   = ES_TEC_RESP
      ).


  ENDMETHOD.


  method IF_J_1BNF_ADD_DATA~FILL_AUTXML.
  endmethod.


  method IF_J_1BNF_ADD_DATA~FILL_B2B_DATA.
  endmethod.


  method IF_J_1BNF_ADD_DATA~FILL_COD_SIT.
  endmethod.


  METHOD if_j_1bnf_add_data~fill_exparameters.

    FIELD-SYMBOLS: <fs_xmlh> TYPE j1b_nf_xml_header.
    ASSIGN ('(SAPLJ_1B_NFE)XMLH') TO <fs_xmlh>.

    TRY .

        <fs_xmlh>-c1_xlgr = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xlgr ) ) ).
        <fs_xmlh>-c1_xcpl = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xcpl ) ) ).
        <fs_xmlh>-c1_xbairro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xbairro ) ) ).
        <fs_xmlh>-c1_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xmun ) ) ).
        <fs_xmlh>-c_xfant = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c_xfant ) ) ).
        <fs_xmlh>-c1_xpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_xpais ) ) ) ).

        <fs_xmlh>-natop = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-natop ) ) ).
        <fs_xmlh>-c1_cpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c1_cpais ) ) ) ).
        <fs_xmlh>-e1_cpais = zcl_string=>upper( i_str = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-e1_cpais ) ) ) ).
        <fs_xmlh>-c_xfant = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c_xfant ) ) ).
        <fs_xmlh>-f_xlgr  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xlgr ) ) ).
        <fs_xmlh>-f_xcpl  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xcpl ) ) ).
        <fs_xmlh>-f_xbairro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xbairro ) ) ).
        <fs_xmlh>-f_xmun = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xmun ) ) ).
        <fs_xmlh>-f_xnome = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xnome ) ) ).
        <fs_xmlh>-f_xpais = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xpais ) ) ).
        <fs_xmlh>-g_xlgr = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_xlgr ) ) ).
        <fs_xmlh>-g_xcpl = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_xcpl ) ) ).
        <fs_xmlh>-g_xbairro  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_xbairro ) ) ).
        <fs_xmlh>-g_xmun  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_xmun ) ) ).
        <fs_xmlh>-g_xnome  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_xnome ) ) ).
        <fs_xmlh>-g_fone  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_fone ) ) ).
        <fs_xmlh>-g_email  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-g_email ) ) ).
        <fs_xmlh>-infadfisco = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-infadfisco ) ) ).
        <fs_xmlh>-infadfisco_v2 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = <fs_xmlh>-infadfisco_v2 ) ).
        <fs_xmlh>-infcomp = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = <fs_xmlh>-infcomp ) ).
        <fs_xmlh>-xlocembarq = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-xlocembarq ) ) ).
        <fs_xmlh>-xped = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-xped ) ) ).
        <fs_xmlh>-xcont = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-xcont ) ) ).
        <fs_xmlh>-c_xcampo = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c_xcampo ) ) ).
        <fs_xmlh>-c_xtexto = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-c_xtexto ) ) ).
        <fs_xmlh>-f_xcampo = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xcampo ) ) ).
        <fs_xmlh>-f_xtexto = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-f_xtexto ) ) ).
        <fs_xmlh>-nproc = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-nproc ) ) ).
        <fs_xmlh>-indproc = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-indproc ) ) ).
        <fs_xmlh>-email = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( <fs_xmlh>-email ) ) ).
      CATCH zcx_error.
    ENDTRY.

  ENDMETHOD.


  method IF_J_1BNF_ADD_DATA~FILL_PICMSINTER.
  endmethod.


  METHOD SET_ADD_DATA.

    ES_TEC_RESP = ZSD_CBENEF=>GET_RESP_TECNICO( I_UF = IS_HEADER-REGIO I_BUKRS = IS_HEADER-BUKRS I_BRANCH = IS_HEADER-BRANCH ).

    DATA(_SEM_FRETE) = ''.

    READ TABLE IT_NFLIN INTO DATA(WA_NFLIN) INDEX 1.

    CASE IS_HEADER-INCO1.
      WHEN 'FOB'." OR 'CPT'. "//ISSUE-188371 WBARBOSA 19/08/25

        "Ajuste Tipo de Frete para fatura agrupada a partir da ZLEST0106/ZLEST0136
        READ TABLE IT_PARTNER INTO DATA(WA_PARTNER) WITH KEY PARVW = 'LR'.

        IF SY-SUBRC = 0.
          DATA(VL_PARID_TMP) = IS_HEADER-PARID.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = VL_PARID_TMP
            IMPORTING
              OUTPUT = VL_PARID_TMP.

          SELECT SINGLE *
            FROM ZSDT0121 INTO @DATA(WL_ZSDT0121)
           WHERE WERKS EQ @VL_PARID_TMP+6(4)
             AND MATNR EQ @WA_NFLIN-MATNR
             AND KUNNR EQ @WA_PARTNER-PARID.

          IF ( SY-SUBRC = 0 ) AND ( WA_NFLIN-REFKEY(10) IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ZSDT0001 INTO @DATA(WL_ZSDT0001)
             WHERE FATURA_PROD EQ @WA_NFLIN-REFKEY(10).
            IF SY-SUBRC EQ 0.
              _SEM_FRETE = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'CFR'.
        IF WA_NFLIN-REFKEY(10) IS NOT INITIAL.
          SELECT SINGLE *
            FROM ZSDT0001 INTO WL_ZSDT0001
           WHERE FATURA_PROD = WA_NFLIN-REFKEY(10).

          IF ( SY-SUBRC EQ 0 ) AND
             ( WL_ZSDT0001-ID_INTERFACE = '48' OR
               WL_ZSDT0001-ID_INTERFACE = '51' OR
               WL_ZSDT0001-ID_INTERFACE = '52' ).
            _SEM_FRETE = 'X'.
          ENDIF.
        ENDIF.
    ENDCASE.

    "Finalidade de Emissão da Nf-e: Devolução/Retorno
    "Existem 4 tipos de finalidade sendo elas;
    " 1 - Nf-e normal
    " 2 - Nf-e complementar
    " 3 - Nf-e de ajuste
    " 4 - Devolução/Retorno
    "Dentro da MAGGI a 4 já esta sendo utilizado para outra finalidade, mas será criado a regra abaixo
    "definindo quando for 6 (Devolução/Retorno) será atribuido o valor 4 na TAG finNFE.

    CASE IS_HEADER-DOCTYP.
      WHEN '1' OR 'R'.
        DATA(LC_FINNFE) = '1'.
      WHEN '2'.
        LC_FINNFE = '2'.
      WHEN '8'.
        LC_FINNFE = '3'.
      WHEN '6'.
        LC_FINNFE = '4'.
      WHEN OTHERS.
    ENDCASE.

    IF _SEM_FRETE IS NOT INITIAL.
      ES_HEADER-MODFRETE = '9'.
    ELSEIF LC_FINNFE EQ '2'.
      CASE IS_HEADER-INCO1.
        WHEN 'FOB'.
          ES_HEADER-MODFRETE = '1'.
        WHEN OTHERS.
          ES_HEADER-MODFRETE = '0'.
      ENDCASE.
    ELSE.
      CASE IS_HEADER-INCO1.
        WHEN: 'CIF' OR 'CPT'.
          ES_HEADER-MODFRETE = '0'.
        WHEN: 'FOB'.
          ES_HEADER-MODFRETE = '1'.
        WHEN: 'FCA'.
          ES_HEADER-MODFRETE = '9'.
        WHEN OTHERS.
          ES_HEADER-MODFRETE = '1'.
      ENDCASE.
    ENDIF.

    APPEND VALUE #(
       T_PAG      = '99'
       V_PAG      = IS_HEADER-NFTOT
       DOCNUM     = IS_HEADER-DOCNUM
       TP_INTEGRA = '2'
       ) TO ET_PAYMENT.

    IF ( IS_HEADER-CGC IS NOT INITIAL AND IS_HEADER-STAINS  EQ 'ISENTO' ) OR IS_HEADER-CPF IS NOT INITIAL.
      ES_HEADER-IND_FINAL  = '1'.
    ENDIF.

    ES_HEADER-INDPAG = '2'.

    ME->SET_CAB_OBSERVACAO_FISCO( EXPORTING I_HEADER = IS_HEADER IT_NFLIN = IT_NFLIN CHANGING E_HEADER = ES_HEADER ).

    "" Ajuste de Itens da Nota Fiscal """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" Ajuste de Itens da Nota Fiscal """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" Ajuste de Itens da Nota Fiscal """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" Ajuste de Itens da Nota Fiscal """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT IT_NFLIN ASSIGNING FIELD-SYMBOL(<FS_NFLIN>).
      READ TABLE ET_ITEM ASSIGNING FIELD-SYMBOL(<FS_ITEM>) WITH KEY ITMNUM = <FS_NFLIN>-ITMNUM.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      ME->SET_ITEM_BENEFICIO( EXPORTING I_HEADER = IS_HEADER  I_ITEM_NOTA = <FS_NFLIN> I_NFSTX = IT_NFSTX CHANGING E_ITEM_NFE  = <FS_ITEM> ).

      ME->SET_ITEM_UNID_TRIB( EXPORTING I_HEADER = IS_HEADER  I_ITEM_NOTA = <FS_NFLIN> I_NFSTX = IT_NFSTX CHANGING E_ITEM_NFE  = <FS_ITEM> ).

      ME->SET_ITEM_IMPORT(
        EXPORTING
          I_HEADER     = IS_HEADER
          I_ITEM_NOTA  = <FS_NFLIN>
          I_NFSTX      = IT_NFSTX
        CHANGING
          E_ITEM_NFE   = <FS_ITEM>
          E_IMPORT_ADI = ET_IMPORT_ADI
          E_IMPORT_DI  = ET_IMPORT_DI ).

      IF <FS_ITEM>-CEAN IS INITIAL.
        <FS_ITEM>-CEAN = 'SEM GTIN'.
      ENDIF.

      IF <FS_ITEM>-CEAN_TRIB IS INITIAL.
        <FS_ITEM>-CEAN_TRIB = <FS_ITEM>-CEAN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD SET_CAB_OBSERVACAO_FISCO.

*    SELECT *
*      FROM ZMMT0065
*      INTO TABLE @DATA(IT_ZMMT0065)
*     WHERE DOCNUM EQ @I_HEADER-DOCNUM
*     ORDER BY SEQNUM.
*
*    LOOP AT IT_ZMMT0065 INTO DATA(WA_ZMMT0065).
*      IF E_HEADER-INFADFISCO IS INITIAL.
*        E_HEADER-INFADFISCO = WA_ZMMT0065-MESSAGE.
*      ELSE.
*        CONCATENATE E_HEADER-INFADFISCO WA_ZMMT0065-MESSAGE INTO E_HEADER-INFADFISCO SEPARATED BY SPACE.
*      ENDIF.
*      CONDENSE E_HEADER-INFADFISCO.
*    ENDLOOP.

    SELECT SINGLE * INTO @DATA(WA_J_1BBRANCH)
      FROM J_1BBRANCH
     WHERE BUKRS  EQ @I_HEADER-BUKRS
       AND BRANCH EQ @I_HEADER-BRANCH.

    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ADRC)
        FROM ADRC
       WHERE ADDRNUMBER EQ @WA_J_1BBRANCH-ADRNR.

      IF ( WA_ADRC-COUNTRY EQ 'BR' ) AND ( WA_ADRC-REGION EQ 'MT' ).

        SELECT SINGLE * INTO @DATA(WA_ZJCND_BRANCH)
          FROM ZJCND_BRANCH
         WHERE BUKRS       EQ @I_HEADER-BUKRS
           AND BRANCH      EQ @I_HEADER-BRANCH
           AND DT_EMISSAO  LE @I_HEADER-PSTDAT
           AND DT_VALIDADE GE @I_HEADER-PSTDAT.

        IF SY-SUBRC IS INITIAL.
          CONCATENATE E_HEADER-INFADFISCO 'Nro. CND:' WA_ZJCND_BRANCH-CD_CND 'Cod. Autenticidade' WA_ZJCND_BRANCH-COD_AUT INTO E_HEADER-INFADFISCO SEPARATED BY SPACE.
          CONDENSE E_HEADER-INFADFISCO.
        ENDIF.
      ENDIF.
    ENDIF.

*    READ TABLE IT_NFLIN INDEX 1 INTO DATA(WA_NFLIN).
*
*    "Preço de Pauta """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    DATA: VLR_ZIVP   TYPE KONV-KBETR,
*          VLR_PR00   TYPE KONV-KBETR,
*          VLR_ZVIP_S TYPE STRING.
*
*    SELECT * FROM VBRK INTO TABLE @DATA(TL_VBRK) WHERE VBELN EQ @WA_NFLIN-REFKEY.
*
*    IF SY-SUBRC IS INITIAL.
*
*      READ TABLE TL_VBRK INTO DATA(WA_VBRK) INDEX 1.
*
*      SELECT * FROM KONV INTO TABLE @DATA(IT_KONV) WHERE KNUMV EQ @WA_VBRK-KNUMV AND KSCHL IN ('ZIVP','ICMI').
*
*      IF IT_KONV[] IS NOT INITIAL.
*
*        READ TABLE IT_KONV INTO DATA(WA_KONV) WITH KEY KSCHL = 'ZIVP'.
*        IF SY-SUBRC IS INITIAL.
*          VLR_ZIVP = WA_KONV-KBETR.
*        ELSE.
*          VLR_ZIVP = 0.
*        ENDIF.
*
*        SELECT SINGLE * FROM KONH INTO @DATA(WA_KONH) WHERE KNUMH EQ @WA_KONV-KNUMH.
*        IF SY-SUBRC IS INITIAL.
*          SELECT * FROM KONH INTO TABLE @DATA(IT_KONH) WHERE VAKEY EQ @WA_KONH-VAKEY.
*        ENDIF.
*
*        SORT IT_KONH BY DATAB.
*
*        LOOP AT IT_KONH INTO WA_KONH.
*          IF WA_KONH-DATAB GT SY-DATUM.
*            EXIT.
*          ENDIF.
*          DATA(WA_KONH_ANT) = WA_KONH.
*        ENDLOOP.
*
*        CLEAR: WA_KONV.
*        READ TABLE IT_KONV INTO WA_KONV WITH KEY KSCHL = 'ICMI'.
*        VLR_PR00 = WA_KONV-KBETR.
*
*        SELECT VBELV INTO @DATA(VL_VBELV) FROM VBFA UP TO 1 ROWS
*         WHERE VBELN   = SL_LIKP-VBELN
*           AND VBTYP_N = 'J'
*           AND VBTYP_V = 'C'.
*        ENDSELECT.
*
*        IF NOT VL_VBELV IS INITIAL.
*          SELECT SINGLE * INTO WA_VBAK
*            FROM VBAK
*           WHERE VBELN = VL_VBELV.
*
*          SELECT SINGLE * FROM KNA1 INTO WA_KNA1 WHERE KUNNR EQ WA_VBAK-KUNNR.
*          SELECT SINGLE * INTO WA_VBAP
*            FROM VBAP
*          WHERE VBELN = WA_VBAK-VBELN.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              INPUT  = WA_VBAP-WERKS
*            IMPORTING
*              OUTPUT = WA_LIFNR.
*
*          SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_LIFNR.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

    E_HEADER-INFADFISCO = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = E_HEADER-INFADFISCO ) ).

  ENDMETHOD.


  METHOD SET_ITEM_BENEFICIO.

    DATA: IT_REGIO_DESON  TYPE TABLE OF REGIO,
          WA_REGIO_DESON  TYPE REGIO,
          ICMS_DESONERADO TYPE C LENGTH 1,
          ICMS_NAO_TRIBUT TYPE C LENGTH 1,
          VAR_RATE        TYPE J_1BTXRATE,
          VALOR_DECONTO   TYPE J_1BEXCBAS,
          TX_VALOR        TYPE C LENGTH 30.

    "MOTDESICMS
    " VICMSDESON

    "Lei Fiscal com Benefício
    SELECT SINGLE *
      FROM ZSDT0098 INTO @DATA(WL_ZSDT0098)
     WHERE TAXLW1 EQ @I_ITEM_NOTA-TAXLW1
       AND CFOP   EQ @I_ITEM_NOTA-CFOP(4).

    IF SY-SUBRC IS NOT INITIAL.
      SELECT SINGLE *
        FROM ZSDT0098 INTO @WL_ZSDT0098
       WHERE TAXLW1 EQ @I_ITEM_NOTA-TAXLW1.
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    E_ITEM_NFE-CBENEF     = WL_ZSDT0098-CBENF.
    E_ITEM_NFE-MOTDESICMS = WL_ZSDT0098-MOTIVO.

    REFRESH: IT_REGIO_DESON.
    WA_REGIO_DESON = 'RO'.
    APPEND WA_REGIO_DESON TO IT_REGIO_DESON.
    WA_REGIO_DESON = 'MA'.
    APPEND WA_REGIO_DESON TO IT_REGIO_DESON.
    WA_REGIO_DESON = 'TO'.
    APPEND WA_REGIO_DESON TO IT_REGIO_DESON.
    WA_REGIO_DESON = 'PI'.
    APPEND WA_REGIO_DESON TO IT_REGIO_DESON.
    WA_REGIO_DESON = 'MT'.
    APPEND WA_REGIO_DESON TO IT_REGIO_DESON.

    CLEAR: ICMS_DESONERADO.
    LOOP AT IT_REGIO_DESON INTO WA_REGIO_DESON.

      IF ICMS_DESONERADO IS NOT INITIAL.
        EXIT.
      ENDIF.

      CALL METHOD ME->VERIF_ICMS_DESONERADO
        EXPORTING
          P_BUKRS      = I_HEADER-BUKRS
          P_BRANCH     = I_HEADER-BRANCH
          P_PARTYP     = I_HEADER-PARTYP
          P_PARID      = I_HEADER-PARID
          P_TAXLW1     = I_ITEM_NOTA-TAXLW1
          P_PAIS       = 'BR'
          P_REGIO      = WA_REGIO_DESON
        IMPORTING
          P_DESONERADO = ICMS_DESONERADO
          P_RATE       = VAR_RATE.

    ENDLOOP.

    IF NOT ICMS_DESONERADO IS INITIAL.
      CLEAR: VALOR_DECONTO.
      LOOP AT I_NFSTX INTO DATA(SL_TAX) WHERE ITMNUM = I_ITEM_NOTA-ITMNUM.
        IF ( SL_TAX-TAXTYP(3) EQ 'ICM' ) AND ( SL_TAX-EXCBAS GT 0 ).
          SL_TAX-EXCBAS = ( SL_TAX-EXCBAS / ( ( 100 - VAR_RATE ) / 100 ) ).
          VALOR_DECONTO = VALOR_DECONTO + ( SL_TAX-EXCBAS * ( VAR_RATE / 100 ) ).
        ELSEIF ( SL_TAX-TAXTYP(3) EQ 'ICM' ) AND ( SL_TAX-OTHBAS GT 0 ).
          SL_TAX-OTHBAS = ( SL_TAX-OTHBAS / ( ( 100 - VAR_RATE ) / 100 ) ).
          VALOR_DECONTO = VALOR_DECONTO + ( SL_TAX-OTHBAS * ( VAR_RATE / 100 ) ).
        ENDIF.
      ENDLOOP.
      E_ITEM_NFE-VICMSDESON = VALOR_DECONTO.

      WRITE E_ITEM_NFE-VICMSDESON TO TX_VALOR.
      CONDENSE TX_VALOR NO-GAPS.

      CONCATENATE E_ITEM_NFE-INFADPROD 'VALOR DO ICMS ABATIDO: R$' TX_VALOR INTO E_ITEM_NFE-INFADPROD
        SEPARATED BY SPACE.

      CONDENSE E_ITEM_NFE-INFADPROD.
    ELSE.

*      CLEAR: ICMS_NAO_TRIBUT, VALOR_DECONTO.
*
*      LOOP AT IT_REGIO_DESON INTO WA_REGIO_DESON.
*
*        IF ICMS_NAO_TRIBUT IS NOT INITIAL.
*          EXIT.
*        ENDIF.
*
*        CALL METHOD ME->VERIF_ICMS_NAO_TRIBUTADO
*          EXPORTING
*            P_BUKRS         = I_HEADER-BUKRS
*            P_BRANCH        = I_HEADER-BRANCH
*            P_PARTYP        = I_HEADER-PARTYP
*            P_PARID         = I_HEADER-PARID
*            P_TAXLW1        = I_ITEM_NOTA-TAXLW1
*            P_PAIS          = 'BR'
*            P_REGIO         = WA_REGIO_DESON
*          IMPORTING
*            P_NAO_TRIBUTADO = ICMS_NAO_TRIBUT.
*      ENDLOOP.
*
*      IF ICMS_NAO_TRIBUT IS NOT INITIAL.
*
*        DATA: V_BASERED1       TYPE J_1BNFSTX-BASERED1,
*              V_BASE_TOT       TYPE J_1BNFSTX-BASE,
*              VL_BASE_N_TRIBUT TYPE J_1BNFSTX-BASERED1,
*              VL_ALIQ_N_TRIBUT TYPE J_1BNFSTX-RATE,
*              VL_PERC_N_TRIBUT TYPE P DECIMALS 6.
*
*        LOOP AT I_NFSTX INTO SL_TAX WHERE ITMNUM = I_ITEM_NOTA-ITMNUM.
*          CLEAR: VL_BASE_N_TRIBUT, VL_ALIQ_N_TRIBUT, VL_PERC_N_TRIBUT, V_BASERED1, V_BASE_TOT.
*          V_BASE_TOT = SL_TAX-BASE + SL_TAX-EXCBAS.
*          IF V_BASE_TOT > 0..
*            V_BASERED1 = ( SL_TAX-BASE / V_BASE_TOT ) * 100.
*          ENDIF.
*          IF ( SL_TAX-TAXTYP(3) EQ 'ICM' ) AND
*             ( V_BASERED1 GT 0       ) AND
*             ( V_BASERED1 LT 100     ) AND
*             ( SL_TAX-RATE GT 0      ) AND
*             ( I_ITEM_NOTA-NFNETT GT 0 ).
*
*            VL_BASE_N_TRIBUT = ( 100 - V_BASERED1 ) / 100.
*            VL_ALIQ_N_TRIBUT = VL_BASE_N_TRIBUT * SL_TAX-RATE.
*            VL_PERC_N_TRIBUT = ( 100 - VL_ALIQ_N_TRIBUT ) / 100.
*            IF VL_PERC_N_TRIBUT NE 0.
*              VALOR_DECONTO = VALOR_DECONTO + ( ( I_ITEM_NOTA-NFNETT / VL_PERC_N_TRIBUT ) * ( VL_ALIQ_N_TRIBUT / 100 ) ).
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*
*        E_ITEM_NFE- =
*
*      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD SET_ITEM_IMPORT.



  ENDMETHOD.


  METHOD SET_ITEM_UNID_TRIB.

    DATA(VL_NCM_EXP) = I_ITEM_NOTA-NBM.
    REPLACE ALL OCCURRENCES OF '.' IN VL_NCM_EXP WITH '' IGNORING CASE.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_UF_UTRIB)
     WHERE SETNAME EQ 'MAGGI_UF_UTRIB_EXP'
       AND VALFROM EQ @I_HEADER-REGIO.

    CHECK SY-SUBRC IS NOT INITIAL.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_CFOP_UTRIB)
     WHERE SETNAME = 'MAGGI_CFOP_UTRIB_EXP'
       AND VALFROM = @I_ITEM_NOTA-CFOP(4).

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_NCM_UTRIB)
     WHERE SETNAME = 'MAGGI_NCM_UTRIB_EXP'
       AND VALFROM = @VL_NCM_EXP.

    CHECK SY-SUBRC IS INITIAL.

    IF ( _WL_SET_CFOP_UTRIB IS NOT INITIAL ) AND
       ( _WL_SET_NCM_UTRIB IS NOT INITIAL ) AND
       ( _WL_SET_UF_UTRIB IS INITIAL ). "UF de Exceção

      IF E_ITEM_NFE-MENGE_TRIB GT 0.

        DATA: LC_MENGE TYPE EKPO-MENGE.
        LC_MENGE = E_ITEM_NFE-MENGE_TRIB.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            I_MATNR              = I_ITEM_NOTA-MATNR
            I_IN_ME              = E_ITEM_NFE-MEINS_TRIB
            I_OUT_ME             = 'TO'
            I_MENGE              = LC_MENGE
          IMPORTING
            E_MENGE              = LC_MENGE
          EXCEPTIONS
            ERROR_IN_APPLICATION = 1
            ERROR                = 2
            OTHERS               = 3.

        IF SY-SUBRC = 0.
          E_ITEM_NFE-MEINS_TRIB = 'TO'.
          E_ITEM_NFE-MENGE_TRIB = LC_MENGE.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method VERIF_ICMS_DESONERADO.

    DATA: P_PAIS_DEF  TYPE  LAND1,
          P_REGIO_DEF TYPE  REGIO.

    CLEAR: P_DESONERADO, P_RATE.

    CHECK P_PARTYP EQ 'C'.

    IF P_PAIS EQ 'BR'.

      CASE P_REGIO.
        WHEN 'RO'.
          CHECK ( P_TAXLW1 EQ 'R58' ) OR
                ( P_TAXLW1 EQ 'M86' ).
        WHEN 'MA'.
          CHECK ( P_TAXLW1 EQ 'O46' ) OR
                ( P_TAXLW1 EQ 'O50' ) OR
                ( P_TAXLW1 EQ 'O48' ).
        WHEN 'TO'.
          CHECK ( P_TAXLW1 EQ 'T45' ) OR
                ( P_TAXLW1 EQ 'T49' ) OR
                ( P_TAXLW1 EQ 'T47' ).
        WHEN 'PI'.
          CHECK ( P_TAXLW1 EQ 'PI4' ) OR
                ( P_TAXLW1 EQ 'PI7' ).
        WHEN 'MT'.

        WHEN OTHERS.
          EXIT.
      ENDCASE.

      CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
        EXPORTING
          P_BUKRS      = P_BUKRS
          P_BRANCH     = P_BRANCH
          P_PARTYP_1   = P_PARTYP
          P_PARID_1    = P_PARID
          P_PAIS_DEF   = P_PAIS
          P_REGIO_DEF  = P_REGIO
        IMPORTING
          P_DENTRO_DEF = P_DESONERADO
          P_RATE       = P_RATE.

      IF ( P_TAXLW1 EQ 'M86' ).
        P_DESONERADO = 'X'.
      ENDIF.

    ENDIF.

  endmethod.


  method VERIF_ICMS_NAO_TRIBUTADO.

    DATA: P_PAIS_DEF  TYPE  LAND1,
          P_REGIO_DEF TYPE  REGIO.

    CLEAR: P_NAO_TRIBUTADO.

    CHECK P_PARTYP EQ 'C'.

    IF P_PAIS EQ 'BR'.

      CASE P_REGIO.
        WHEN 'MA'.
          CHECK ( P_TAXLW1 EQ 'O47' ) OR
                ( P_TAXLW1 EQ 'O49' ) OR
                ( P_TAXLW1 EQ 'O51' ).
        WHEN 'TO'.
          CHECK ( P_TAXLW1 EQ 'T46' ) OR
                ( P_TAXLW1 EQ 'T48' ) OR
                ( P_TAXLW1 EQ 'T50' ).
        WHEN 'PI'.
          CHECK ( P_TAXLW1 EQ 'PI5' ) OR
                ( P_TAXLW1 EQ 'PI6' ) OR
                ( P_TAXLW1 EQ 'PI8' ).
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      CALL FUNCTION 'Z_VERIFICA_DENTRO_FORA_ESTADO'
        EXPORTING
          P_BUKRS      = P_BUKRS
          P_BRANCH     = P_BRANCH
          P_PARTYP_1   = P_PARTYP
          P_PARID_1    = P_PARID
          P_PAIS_DEF   = P_PAIS
          P_REGIO_DEF  = P_REGIO
        IMPORTING
          P_INTERESTADUAL_DEF = P_NAO_TRIBUTADO.

    ENDIF.

  endmethod.
ENDCLASS.
