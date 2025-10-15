*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print electronic fiscal document                                   *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  ZBRCTE_DACTE_NAST MESSAGE-ID 8B.

TABLES: J_1BNFDOC,
        NAST,                          "Messages
        *NAST,                         "Messages
        TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive parameters
        TOA_DARA,                      "Archive parameters
        ADDR_KEY.                      "Adressnumber for ADDRESS

DATA: OUTPUT_OPTIONS TYPE SSFCOMPOP.
DATA: CONTROL_PARAMETERS TYPE SSFCTRLOP.

DATA: GS_NFDOC       TYPE J_1BNFDOC,
      GT_NFDOC_ADD   TYPE J_1BINDOC,
      GT_NFNAD       TYPE TABLE OF J_1BNFNAD,
      GT_NFLIN       TYPE TABLE OF J_1BNFLIN,
      GT_NFSTX       TYPE TABLE OF J_1BNFSTX,
      GT_NFDOC_MSG   TYPE TABLE OF J_1BNFFTX,
      GT_NFREF       TYPE TABLE OF J_1BNFREF,
      GT_NFCPD       TYPE TABLE OF J_1BNFCPD,
      GT_CTE_RES     TYPE TABLE OF J_1BCTE_D_RES,
      GT_CTE_DOCREF  TYPE TABLE OF J_1BCTE_D_DOCREF,
      GS_OBSCONT     TYPE CHAR4000,
      GS_OBSERVACOES TYPE CHAR4000,
      GT_DOCS_ORIGIN TYPE ZDE_DOCS_ORIGIN_CTE_T.

DATA:
 GS_NFEACTIVE TYPE J_1BNFE_ACTIVE.

DATA:
  GS_PRNFEHD   TYPE J_1BPRNFEHD,
  GT_PRNFETEXT TYPE J_1BPRNFETEXT_TAB,
  GT_PRNFESTX  TYPE TABLE OF J_1BPRNFESTX.

DATA:
 GS_LIN TYPE J_1BNFLIN.

* CT-E SPECIFIC PARTNERS
DATA:
  GS_ISSUER           TYPE J_1BINNAD,
  GS_GOODS_SENDER     TYPE J_1BPRNFEINNAD,
  GS_CARGO_DISPATCHER TYPE J_1BPRNFEINNAD,
  GS_CARGO_RECIPIENT  TYPE J_1BPRNFEINNAD,
  GS_DESTINATION      TYPE J_1BPRNFEINNAD,
  GS_SERVICE_TAKER    TYPE J_1BPRNFEINNAD,
  GS_COMPONENTES      TYPE ZDE_DOCS_COMPONENTES.

CONSTANTS:
  GC_GOODS_SENDER     TYPE J_1BCTE_TOMALI VALUE '0',
  GC_CARGO_DISPATCHER TYPE J_1BCTE_TOMALI VALUE '1',
  GC_CARGO_RECIPIENT  TYPE J_1BCTE_TOMALI VALUE '2',
  GC_DESTINATION      TYPE J_1BCTE_TOMALI VALUE '3',
  GC_OTHER            TYPE J_1BCTE_TOMALI VALUE '4'.

DATA:
  GV_DOCNUM TYPE J_1BDOCNUM.

DATA:
  RETCODE LIKE SY-SUBRC,           " RETURN CODE INDICATOR
  XSCREEN.                         " OUTPUT ON PRINTER OR SCREEN

DATA: WG_XML_SEFAZ TYPE ZCTE_XML_SEFAZ_AUTH.



DATA: GS_IDE_DACTE       TYPE ZDE_IDE_XML_DACTE,
      GS_INF_MODAL_AQUAV TYPE ZDE_INF_MODAL_AQUAV_DACTE,
      WG_DACTE           TYPE ZBRCTE_DACTE.

CONSTANTS:
    LC_EDIT_MASK(54) TYPE C VALUE
      '____.____.____.____.____.____.____.____.____.____.____'.

INITIALIZATION.
  PERFORM F_TESTE.

FORM ENTRY USING RETURN_CODE US_SCREEN.

  DATA: OTFDATA TYPE TSFOTF.

  PERFORM IMPRIMIR_DACTE USING RETURN_CODE US_SCREEN
                      CHANGING OTFDATA.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      I_OTF                    = OTFDATA
    EXCEPTIONS
      CONVERT_OTF_TO_PDF_ERROR = 1
      CNTL_ERROR               = 2
      OTHERS                   = 3.

ENDFORM.                               " ENTRY

FORM ENTRY2 USING RETURN_CODE US_SCREEN P_NAST TYPE NAST
         CHANGING OTFDATA TYPE TSFOTF.

  MOVE-CORRESPONDING P_NAST TO NAST.
  TNAPR-SFORM = 'Z_DACTE_ROAD_FTL'.

  PERFORM IMPRIMIR_DACTE USING RETURN_CODE US_SCREEN CHANGING OTFDATA.

ENDFORM.                               " ENTRY

FORM IMPRIMIR_DACTE USING RETURN_CODE
                          US_SCREEN
                 CHANGING OTFDATA  TYPE TSFOTF.

  CLEAR RETCODE.

  CLEAR RETURN_CODE.
  XSCREEN = US_SCREEN.

  PERFORM GET_DADOS_XML_CTE.

  CHECK WG_XML_SEFAZ IS NOT INITIAL.

  PERFORM BUILD_INFO_INFO_XML.

  PERFORM PREPARE_CROSS_MODEL.

  PERFORM PREPARE_MODEL57.

  PERFORM PRINTING CHANGING OTFDATA.

*  IF RETCODE NE 0.
*    RETURN_CODE = 1.
*
*    CALL FUNCTION 'DEQUEUE_E_J1BNFE'
*      EXPORTING
*        MODE_J_1BNFE_ACTIVE = 'E'
*        DOCNUM              = GV_DOCNUM.
*
*    CALL FUNCTION 'DEQUEUE_EJ_1BNFE'
*      EXPORTING
*        DOCNUM = GV_DOCNUM.
*  ENDIF.

ENDFORM.

FORM GET_DADOS_XML_TMP.


  DATA XML_STRING TYPE STRING.

  DATA: T_ELEMENT_ARRAY TYPE ZDE_ELEMENT_ARRAY_T.


*  xml_string = '<?xml version="1.0" encoding="UTF-8"?><cteProc><CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte Id="CTe11190484590892000380570000000140431061231787" versao="3.00">'.
*  xml_string = xml_string &&  '<ide><cUF>11</cUF><cCT>06123178</cCT><CFOP>6353</CFOP><natOp>Prestacao serv. transp. estab. comercial</natOp><mod>57</mod><serie>0</serie><nCT>14043</nCT>'.
*  xml_string = xml_string &&  '<dhEmi>2019-04-02T09:10:54-04:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>7</cDV><tpAmb>1</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi>'.
*  xml_string = xml_string &&  '<verProc>Simetrya CTe v2.21-B</verProc><cMunEnv>1100205</cMunEnv><xMunEnv>Porto Velho</xMunEnv><UFEnv>RO</UFEnv><modal>03</modal><tpServ>0</tpServ>'.
*  xml_string = xml_string &&  '<cMunIni>1100205</cMunIni><xMunIni>Porto Velho</xMunIni><UFIni>RO</UFIni><cMunFim>1301902</cMunFim><xMunFim>Itacoatiara</xMunFim><UFFim>AM</UFFim>'.
*  xml_string = xml_string &&  '<retira>1</retira><indIEToma>1</indIEToma><toma3><toma>0</toma></toma3></ide><compl><xObs>ICMS ISENTO CONFORME ITEM 70 DO ANEXO I DA TABELA I DO RICMS/RO. '.
*  xml_string = xml_string &&  'SUSPENSaO DE PIS/COFINS ART 40   6o,-A, DA LEI 10.865/2004 - E ADE N 503, DE 29 DE NOVEMBRO DE 2012. Nr. O.V: 0011662602 Nr. Fatura 0093595788 Nr. Viagem '.
*  xml_string = xml_string &&  '0039 Classificacao Transgenico Vlr. Dolar 32061.02 Tx. Dolar 3.86820</xObs></compl><emit><CNPJ>84590892000380</CNPJ><IE>00000000580635</IE><xNome>HERMASA '.
*  xml_string = xml_string &&  'NAVEGACAO DA AMAZONIA S/A</xNome><xFant>HERMASA NAVEGACAO DA AMAZONIA</xFant><enderEmit><xLgr>RUA TERMINAL DOS MILAGRES</xLgr><nro>400</nro>'.
*  xml_string = xml_string &&  '<xBairro>BALSA</xBairro><cMun>1100205</cMun><xMun>Porto Velho</xMun><CEP>78900000</CEP><UF>RO</UF><fone>6932181007</fone></enderEmit></emit>'.
*  xml_string = xml_string &&  '<rem><CNPJ>77294254001913</CNPJ><IE>00000000587125</IE><xNome>AMAGGI EXPORT E IMPORT LTDA</xNome><xFant>AMAGGI</xFant><enderReme><xLgr>RUA TERMINAL DOS MILAGRES</xLgr>'.
*  xml_string = xml_string &&  '<nro>400-A</nro><xBairro>BALSA</xBairro><cMun>1100205</cMun><xMun>Porto Velho</xMun><CEP>78900000</CEP><UF>RO</UF></enderReme></rem><dest><CNPJ>77294254001913</CNPJ>'.
*  xml_string = xml_string &&  '<IE>00000000587125</IE><xNome>AMAGGI EXP.E IMP.LTDA</xNome><enderDest><xLgr>RUA TERMINAL DOS MILAGRES</xLgr><nro>S/N</nro><xBairro>BALSA</xBairro><cMun>1100205</cMun>'.
*  xml_string = xml_string &&  '<xMun>Porto Velho</xMun><CEP>78900000</CEP><UF>RO</UF></enderDest></dest><vPrest><vTPrest>124018.44</vTPrest><vRec>124018.44</vRec><Comp><xNome>FRETE PESO</xNome>'.
*  xml_string = xml_string &&  '<vComp>124018.44</vComp></Comp></vPrest><imp><ICMS><ICMS45><CST>40</CST></ICMS45></ICMS></imp><infCTeNorm><infCarga><vCarga>2122959.30</vCarga>'.
*  xml_string = xml_string &&  '<proPred>SOJA EM GRAOS ADQ TERCEIROS</proPred><infQ><cUnid>01</cUnid><tpMed>PESO BRUTO</tpMed><qCarga>1444190.0000</qCarga></infQ></infCarga><infDoc>'.
*  xml_string = xml_string &&  '<infNFe><chave>11190377294254001913550000000169521730350510</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp>'.
*  xml_string = xml_string &&  '<qtdRat>50.30</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169531705995470</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.96</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169541118828426</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>22.08</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169551536863917</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>3.94</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169561253460509</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.96</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169571796775150</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.92</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169581396900914</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.98</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169591012150813</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>49.38</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169601611977198</chave><infUnidTransp>'.
*  xml_string = xml_string &&  '<tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.02</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169611090665450</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.14</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169621768363373</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>50.32</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169631502504738</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp>'.
*  xml_string = xml_string &&  '<qtdRat>1.76</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169641598918420</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>51.10</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169651829338707</chave><infUnidTransp>'.
*  xml_string = xml_string &&  '<tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.40</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169661492070457</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>40.00</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169671751745924</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.52</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169681858100049</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.00</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169691009549600</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>3.92</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169701927792216</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.96</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169711204921596</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>3.48</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169721929790274</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp>'.
*  xml_string = xml_string &&  '<qtdRat>31.00</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169731722324026</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.44</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169741456542001</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>5.58</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169751214502374</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>47.64</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169761044431533</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp>'.
*  xml_string = xml_string &&  '<qtdRat>54.78</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169771383005950</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>39.80</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169781449620123</chave><infUnidTransp>'.
*  xml_string = xml_string &&  '<tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>44.90</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190377294254001913550000000169791725887564</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.94</qtdRat></infUnidTransp></infNFe><infNFe>'.
*  xml_string = xml_string &&  '<chave>11190377294254001913550000000169801349415585</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>51.32</qtdRat>'.
*  xml_string = xml_string &&  '</infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000169981623949225</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp>'.
*  xml_string = xml_string &&  '<qtdRat>36.98</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000169991434104365</chave><infUnidTransp><tpUnidTransp>4</tpUnidTransp>'.
*  xml_string = xml_string &&  '<idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>1.68</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170001073220740</chave><infUnidTransp>'.
*  xml_string = xml_string &&  '<tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>23.17</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170011683801027</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>49.36</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170021218454687</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>10.50</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170031877525712</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>32.12</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170041241240463</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>41.34</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170051460164239</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>44.52</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170061215764935</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.40</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170071723568690</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>36.26</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170081882651948</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>48.16</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170091090256001</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>37.03</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170101366232552</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>7.81</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170121977406390</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>30.96</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170141682992480</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>4.85</qtdRat></infUnidTransp></infNFe><infNFe><chave>11190477294254001913550000000170151522049175</chave>'.
*  xml_string = xml_string &&  '<infUnidTransp><tpUnidTransp>4</tpUnidTransp><idUnidTransp>AMAGGI022</idUnidTransp><qtdRat>4.51</qtdRat></infUnidTransp></infNFe></infDoc><infModal versaoModal="3.00"><aquav><vPrest>124018.44</vPrest>'.
*  xml_string = xml_string &&  '<vAFRMM>0.00</vAFRMM><xNavio>E/M ITIQUIRA-001-020366-4</xNavio><balsa><xBalsa>AMAGGI 022-</xBalsa></balsa><nViag>39</nViag><direc>N</direc><irin>PS2930</irin></aquav></infModal></infCTeNorm></infCte>'.
*  xml_string = xml_string &&  '</CTe></cteProc>'.


*  xml_string = '<?xml version="1.0" encoding="UTF-8"?><cteProc><CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte Id="CTe51190877294254001670570000011277431998301913" versao="3.00">'.
*  xml_string = xml_string &&  '<ide><cUF>51</cUF><cCT>99830191</cCT><CFOP>6352</CFOP><natOp>Prestacao serv. transp. estab. Industrial</natOp><mod>57</mod><serie>0</serie><nCT>1127743</nCT>'.
*  xml_string = xml_string &&  '<dhEmi>2019-08-08T00:18:48-04:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>3</cDV><tpAmb>1</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi>'.
*  xml_string = xml_string &&  '<verProc>Simetrya CTe v2.21-I</verProc><cMunEnv>5103304</cMunEnv><xMunEnv>Comodoro</xMunEnv><UFEnv>MT</UFEnv><modal>01</modal><tpServ>0</tpServ>'.
*  xml_string = xml_string &&  '<cMunIni>5103304</cMunIni><xMunIni>Comodoro</xMunIni><UFIni>MT</UFIni><cMunFim>1100262</cMunFim><xMunFim>Rio Crespo</xMunFim><UFFim>RO</UFFim>'.
*  xml_string = xml_string &&  '<retira>1</retira><indIEToma>1</indIEToma><toma3><toma>0</toma></toma3></ide><compl><xObs>Numero do Transporte: 0002452066 Numero do '.
*  xml_string = xml_string &&  'Faturamento: 0093833343 Subcontratado: J A DA SILVA TRANSPORTE ESTRADA PIRES DE SA S/N - Nr:  Bairro: ZONA RURAL - Muni: VILHENA - '.
*  xml_string = xml_string &&  'RO CEP: 78995-000 RNTRC: 12411366 07.901.867/0001-67 IE-00000001465252 Local de entrega: Transbordo: MARIZETE CARINA PERKOSKI  00.000.150/004 IE-00000004860250 '.
*  xml_string = xml_string &&  'Placa Cavalo-MZP2772-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Placa Carreta 1-NBE8592-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Placa '.
*  xml_string = xml_string &&  'Carreta 2-NBE8592-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Nr. CIOT: 133000297575 Nr. Contrato de Viagem Administradora: 201901494513</xObs>'.
*  xml_string = xml_string &&  '<ObsCont xCampo="Placa Cavalo"><xTexto>MZP2772-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 1">'.
*  xml_string = xml_string &&  '<xTexto>NBE8592-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 2">'.
*  xml_string = xml_string &&  '<xTexto>NBE8592-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Motorista">'.
*  xml_string = xml_string &&  '<xTexto>JOSUE ANTONIO DA SILVA- CPF:14409879987</xTexto></ObsCont></compl><emit><CNPJ>77294254001670</CNPJ><IE>131511726</IE>'.
*  xml_string = xml_string &&  '<xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome><xFant>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xFant><enderEmit><xLgr>AVENIDA ANDRE ANTONIO MAGGI</xLgr>'.
*  xml_string = xml_string &&  '<nro>303</nro><xCpl>3 ANDAR</xCpl><xBairro>ALVORADA</xBairro><cMun>5103403</cMun><xMun>Cuiaba</xMun><CEP>78049080</CEP><UF>MT</UF><fone>6536455000</fone>'.
*  xml_string = xml_string &&  '</enderEmit></emit><rem><CNPJ>77294254007520</CNPJ><IE>135946328</IE><xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome>'.
*  xml_string = xml_string &&  '<xFant>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xFant><fone>6536455000</fone><enderReme><xLgr>ROD BR364 KM 120 ENTRONCA</xLgr>'.
*  xml_string = xml_string &&  '<nro>S/N</nro><xBairro>DISTRITO AGROINDUSTRIAL</xBairro><cMun>5103304</cMun><xMun>Comodoro</xMun><CEP>78310000</CEP><UF>MT</UF></enderReme></rem><dest>'.
*  xml_string = xml_string &&  '<CPF>00150004001</CPF><IE>00000004860250</IE><xNome>MARIZETE CARINA PERKOSKI</xNome><enderDest><xLgr>LINHA C 85 GLEBA</xLgr><nro>27</nro><xBairro>ZONA RURAL</xBairro>'.
*  xml_string = xml_string &&  '<cMun>1100262</cMun><xMun>Rio Crespo</xMun><CEP>76863000</CEP><UF>RO</UF></enderDest></dest><vPrest><vTPrest>1265.00</vTPrest><vRec>1265.00</vRec>'.
*  xml_string = xml_string &&  '<Comp><xNome>FRETE PESO</xNome><vComp>1265.00</vComp></Comp></vPrest><imp><ICMS><ICMS00><CST>00</CST><vBC>1265.00</vBC><pICMS>12.00</pICMS>'.
*  xml_string = xml_string &&  '<vICMS>151.80</vICMS></ICMS00></ICMS></imp><infCTeNorm><infCarga><vCarga>22400.61</vCarga><proPred>FERT CLORETO DE POTASSIO 60% BB</proPred>'.
*  xml_string = xml_string &&  '<infQ><cUnid>01</cUnid><tpMed>PESO BRUTO</tpMed><qCarga>11000.0000</qCarga></infQ></infCarga><infDoc><infNFe>'.
*  xml_string = xml_string &&  '<chave>51190877294254007520550000000378481182101412</chave></infNFe></infDoc><infModal versaoModal="3.00"><rodo>'.
*  xml_string = xml_string &&  '<RNTRC>12458812</RNTRC></rodo></infModal></infCTeNorm></infCte></CTe></cteProc>'.

  "CT-e Aquav NF-f
*  XML_STRING = '<?xml version="1.0" encoding="utf-8"?><cteProc xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00">'.
*  XML_STRING = XML_STRING &&  '<CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte versao="3.00" Id="CTe13190915143827000207570000000009921612732110">'.
*  XML_STRING = XML_STRING &&  '<ide><cUF>13</cUF><cCT>61273211</cCT><CFOP>5353</CFOP><natOp>Prestação serv. transp. estab. comercial</natOp>'.
*  XML_STRING = XML_STRING &&  '<mod>57</mod><serie>0</serie><nCT>992</nCT><dhEmi>2019-09-12T10:00:17-04:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>0</cDV>'.
*  XML_STRING = XML_STRING &&  '<tpAmb>2</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi><verProc>008</verProc><cMunEnv>1301902</cMunEnv><xMunEnv>ITACOATIARA</xMunEnv>'.
*  XML_STRING = XML_STRING &&  '<UFEnv>AM</UFEnv><modal>03</modal><tpServ>0</tpServ><cMunIni>1302603</cMunIni><xMunIni>MANAUS</xMunIni><UFIni>AM</UFIni>'.
*  XML_STRING = XML_STRING &&  '<cMunFim>1301902</cMunFim><xMunFim>ITACOATIARA</xMunFim><UFFim>AM</UFFim><retira>1</retira><indIEToma>1</indIEToma><toma4>'.
*  XML_STRING = XML_STRING &&  '<toma>4</toma><CNPJ>77294254002138</CNPJ><IE>041330471</IE><xNome>CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome><enderToma>'.
*  XML_STRING = XML_STRING &&  '<xLgr>ESTRADA DAS INDUSTRIAS KM 7,5</xLgr><nro>SN</nro><xBairro>ZONA RURAL</xBairro><cMun>1301902</cMun><xMun>ITACOATIARA</xMun>'.
*  XML_STRING = XML_STRING &&  '<CEP>69100000</CEP><UF>AM</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderToma></toma4></ide><compl>'.
*  XML_STRING = XML_STRING &&  '<xObs>Nao Incidencia do ICMS conforme Art. 4o inciso II RICMS/AM SUSPENSAO DE PIS/COFINS ART 40 Sec. 6(o),-A, DA LEI 10.865/2004 - '.
*  XML_STRING = XML_STRING &&  'E ADE N 503, DE 29 DE NOVEMBRO DE 2012. Nr. O.V: 0011661325 Nr. Fatura 0093592157 Nr. Viagem 0014 Classificacao Convencional Vlr. '.
*  XML_STRING = XML_STRING &&  'Dolar 375.00 Tx. Dolar 3.90000</xObs></compl><emit><CNPJ>84590892000207</CNPJ><IE>041479831</IE><xNome>CT-E EMITIDO EM AMBIENTE DE '.
*  XML_STRING = XML_STRING &&  'HOMOLOGACAO - SEM VALOR FISCAL</xNome><xFant>TRANSPORTES</xFant><enderEmit><xLgr>EST. DAS INDUSTRIAS</xLgr><nro>S/N</nro><xCpl>KM 7,5</xCpl>'.
*  XML_STRING = XML_STRING &&  '<xBairro>CENTRO</xBairro><cMun>1301902</cMun><xMun>ITACOATIARA</xMun><CEP>69100000</CEP><UF>AM</UF><fone>6933222644</fone></enderEmit>'.
*  XML_STRING = XML_STRING &&  '</emit><rem><CNPJ>84590892000118</CNPJ><IE>041043677</IE><xNome>CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome>'.
*  XML_STRING = XML_STRING &&  '<fone>9231832000</fone><enderReme><xLgr>AV DJALMA BATISTA SLS 1105 E 1106</xLgr><nro>1661</nro><xBairro>CHAPADA</xBairro>'.
*  XML_STRING = XML_STRING &&  '<cMun>1302603</cMun><xMun>MANAUS</xMun><CEP>69050010</CEP><UF>AM</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderReme>'.
*  XML_STRING = XML_STRING &&  '</rem><exped><CNPJ>84590892000118</CNPJ><IE>041043677</IE><xNome>CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome>'.
*  XML_STRING = XML_STRING &&  '<fone>9231832000</fone><enderExped><xLgr>AV DJALMA BATISTA SLS 1105 E 1106</xLgr><nro>1661</nro><xBairro>CHAPADA</xBairro>'.
*  XML_STRING = XML_STRING &&  '<cMun>1302603</cMun><xMun>MANAUS</xMun><CEP>69050010</CEP><UF>AM</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderExped>'.
*  XML_STRING = XML_STRING &&  '</exped><receb><CNPJ>84590892000207</CNPJ><IE>041059166</IE><xNome>CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome><enderReceb>'.
*  XML_STRING = XML_STRING &&  '<xLgr>ESTRADA DAS INDUSTRIAS KM 7,5</xLgr><nro>SN</nro><xBairro>ZONA URBANA</xBairro><cMun>1301902</cMun><xMun>ITACOATIARA</xMun>'.
*  XML_STRING = XML_STRING &&  '<CEP>69100000</CEP><UF>AM</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderReceb></receb><dest><CNPJ>84590892000207</CNPJ>'.
*  XML_STRING = XML_STRING &&  '<IE>041059166</IE><xNome>CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL</xNome><enderDest><xLgr>ESTRADA DAS INDUSTRIAS KM 7,5</xLgr>'.
*  XML_STRING = XML_STRING &&  '<nro>SN</nro><xBairro>ZONA URBANA</xBairro><cMun>1301902</cMun><xMun>ITACOATIARA</xMun><CEP>69100000</CEP><UF>AM</UF><cPais>1058</cPais>'.
*  XML_STRING = XML_STRING &&  '<xPais>Brasil</xPais></enderDest></dest><vPrest><vTPrest>1462.50</vTPrest><vRec>1462.50</vRec></vPrest><imp><ICMS><ICMS45><CST>41</CST>'.
*  XML_STRING = XML_STRING &&  '</ICMS45></ICMS></imp><infCTeNorm><infCarga><vCarga>33000.00</vCarga><proPred>SOJA EM GRAOS ADQ TERCEIROS</proPred><infQ><cUnid>01</cUnid>'.
*  XML_STRING = XML_STRING &&  '<tpMed>PESO BRUTO</tpMed><qCarga>30000.0000</qCarga></infQ></infCarga><infDoc><infNF><mod>04</mod><serie>1</serie><nDoc>000000130</nDoc>'.
*  XML_STRING = XML_STRING &&  '<dEmi>2019-08-20</dEmi><vBC>33000.00</vBC><vICMS>0.00</vICMS><vBCST>0.00</vBCST><vST>0.00</vST><vProd>33000.00</vProd><vNF>33000.00</vNF>'.
*  XML_STRING = XML_STRING &&  '<nCFOP>6502</nCFOP></infNF></infDoc><infModal versaoModal="3.00"><aquav><vPrest>1462.50</vPrest><vAFRMM>0.00</vAFRMM>'.
*  XML_STRING = XML_STRING &&  '<xNavio>E/M BONIFACIO SACHETTI-001-021648-1</xNavio><balsa><xBalsa>AMAGGI 040-</xBalsa></balsa><nViag>14</nViag><direc>N</direc>'.
*  XML_STRING = XML_STRING &&  '<irin>PS5573</irin></aquav></infModal></infCTeNorm></infCte><Signature xmlns="http://www.w3.org/2000/09/xmldsig#"><SignedInfo>'.
*  XML_STRING = XML_STRING &&  '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>'.
*  XML_STRING = XML_STRING &&  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>'.
*  XML_STRING = XML_STRING &&  '<Reference URI="#CTe13190984590892000207570000000009921612732110"><Transforms><Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>'.
*  XML_STRING = XML_STRING &&  '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/></Transforms><DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>'.
*  XML_STRING = XML_STRING &&  '<DigestValue>aCBcjcAPAEOOXByDF/VLGfB2B6U=</DigestValue></Reference></SignedInfo><SignatureValue>bBidwzUmxl9XZazOCLkPK'.
*  XML_STRING = XML_STRING &&  'eZLoHawYoLGvhmqp9a/rlVnSGQmEi7KwcX6RGcaMKvxe+CNLny3R0IE'.
*  XML_STRING = XML_STRING &&  'uvcKV+DpeO9yPJj//QKX+Rh8DsD9PXJkPDKrxELiAO1irdiD7Ag5e/aXK7yocvgojIrz1F7w39i4'.
*  XML_STRING = XML_STRING &&  'QOi29htSLXE50fCA9HAxjo42jtHN1wL1NfqfXXJbvZV40gOsZ0Vuy6T9JDQz01PrTF9Aq9Z+QCRT'.
*  XML_STRING = XML_STRING &&  '1MSYYISMaACuaQYQR1BUoLGBDC2KqzHO+vUWmBQagL3XdLdh2/srROei40TJWfbqQmBVKFQO5DDm'.
*  XML_STRING = XML_STRING &&  'fKTKm1iHS6r10kXpz26bjO1ItNuDvcjOwhUuoQ==</SignatureValue><KeyInfo><X509Data><X509Certificate>MIIHozCCBYugAwIBAgIIKMKnZJua0b0wDQ'.
*  XML_STRING = XML_STRING &&  'YJKoZIhvcNAQELBQAwdTELMAkGA1UEBhMCQlIxEzAR'.
*  XML_STRING = XML_STRING &&  'BgNVBAoMCklDUC1CcmFzaWwxNjA0BgNVBAsMLVNlY3JldGFyaWEgZGEgUmVjZWl0YSBGZWRlcmFs'.
*  XML_STRING = XML_STRING &&  'IGRvIEJyYXNpbCAtIFJGQjEZMBcGA1UEAwwQQUMgU0VSQVNBIFJGQiB2NTAeFw0xODEwMDMyMDQ0'.
*  XML_STRING = XML_STRING &&  'MDBaFw0xOTEwMDMyMDQ0MDBaMIHhMQswCQYDVQQGEwJCUjELMAkGA1UECAwCQU0xDzANBgNVBAcM'.
*  XML_STRING = XML_STRING &&  'Bk1BTkFVUzETMBEGA1UECgwKSUNQLUJyYXNpbDE2MDQGA1UECwwtU2VjcmV0YXJpYSBkYSBSZWNl'.
*  XML_STRING = XML_STRING &&  'aXRhIEZlZGVyYWwgZG8gQnJhc2lsIC0gUkZCMRYwFAYDVQQLDA1SRkIgZS1DTlBKIEExMRUwEwYD'.
*  XML_STRING = XML_STRING &&  'VQQLDAxBUiBDRExDVUlBQkExODA2BgNVBAMML0hFUk1BU0EgTkFWRUdBQ0FPIERBIEFNQVpPTklB'.
*  XML_STRING = XML_STRING &&  'IFNBOjg0NTkwODkyMDAwMTE4MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAjk44QJDy'.
*  XML_STRING = XML_STRING &&  'tpmEjXe+YDQBjoXET2EzsGYh7Ll92dbikiWdyn9w1naMNXm/6a/mKTx9So2FfiSDqD+9Uy3m4Zzw'.
*  XML_STRING = XML_STRING &&  'p2UP2ATNB+/RcIujQQzTviBnYZvKtaxteQqmXCJKX/zuvan8XibW2Y/qrkz6//JbdV7nXwOqT/dc'.
*  XML_STRING = XML_STRING &&  'FxOrvc5Bjo4Z+q61IQ8ZqgTS2mGYK7GmbhARip/QHJ/U22HxLJ5DNILbJ7hpvMYJ7osTToMSoEPB'.
*  XML_STRING = XML_STRING &&  'ko7QaAQK1goUfIIWTtCacZNI9wb5STVYnYrpZsouusANnCarOGXhgSamu5wKzBbvj7ARpK5sL5Ck'.
*  XML_STRING = XML_STRING &&  'SoCQoTGuM1lreQpLAJrEyZxwnaoxmwIDAQABo4ICyDCCAsQwCQYDVR0TBAIwADAfBgNVHSMEGDAW'.
*  XML_STRING = XML_STRING &&  'gBTs8UFRV6jmOules6Ai+QiKtTqHjzCBmQYIKwYBBQUHAQEEgYwwgYkwSAYIKwYBBQUHMAKGPGh0'.
*  XML_STRING = XML_STRING &&  'dHA6Ly93d3cuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9jYWRlaWFzL3NlcmFzYXJmYnY1LnA3'.
*  XML_STRING = XML_STRING &&  'YjA9BggrBgEFBQcwAYYxaHR0cDovL29jc3AuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9zZXJh'.
*  XML_STRING = XML_STRING &&  'c2FyZmJ2NTCBtwYDVR0RBIGvMIGsgRtST0dFUlZBTC5ESUFTQEFNQUdHSS5DT00uQlKgHwYFYEwB'.
*  XML_STRING = XML_STRING &&  'AwKgFhMUU0VSR0lPIExVSVogUElaWkFUVE+gGQYFYEwBAwOgEBMOODQ1OTA4OTIwMDAxMTigOAYF'.
*  XML_STRING = XML_STRING &&  'YEwBAwSgLxMtMjMxMDE5NjAzMzM1MzI1NTkxNTAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwoBcG'.
*  XML_STRING = XML_STRING &&  'BWBMAQMHoA4TDDAwMDAwMDAwMDAwMDBxBgNVHSAEajBoMGYGBmBMAQIBDTBcMFoGCCsGAQUFBwIB'.
*  XML_STRING = XML_STRING &&  'Fk5odHRwOi8vcHVibGljYWNhby5jZXJ0aWZpY2Fkb2RpZ2l0YWwuY29tLmJyL3JlcG9zaXRvcmlv'.
*  XML_STRING = XML_STRING &&  'L2RwYy9kZWNsYXJhY2FvLXJmYi5wZGYwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMEMIGd'.
*  XML_STRING = XML_STRING &&  'BgNVHR8EgZUwgZIwSqBIoEaGRGh0dHA6Ly93d3cuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9y'.
*  XML_STRING = XML_STRING &&  'ZXBvc2l0b3Jpby9sY3Ivc2VyYXNhcmZidjUuY3JsMESgQqBAhj5odHRwOi8vbGNyLmNlcnRpZmlj'.
*  XML_STRING = XML_STRING &&  'YWRvcy5jb20uYnIvcmVwb3NpdG9yaW8vbGNyL3NlcmFzYXJmYnY1LmNybDAOBgNVHQ8BAf8EBAMC'.
*  XML_STRING = XML_STRING &&  'BeAwDQYJKoZIhvcNAQELBQADggIBAKC4wZeELGf/eCZI6dvIWpG49dAxFFtoEr12zoQKxypjuf8p'.
*  XML_STRING = XML_STRING &&  'cpvjrP2K6OgpTDVi6HBalsbUkdEZSgkjHhicg0UIWhhkpSFE02r1u9OPZweRdNb3WVCah86sTaEg'.
*  XML_STRING = XML_STRING &&  'Ji0GIHxTGRsV0FaQWK6fua49AdLxxdpnSFfOEr1K4UdDSteYxA4etfcD1z3leBcUa9AzPVfYigGe'.
*  XML_STRING = XML_STRING &&  'J22cQ3ifTyeEuG7WWCrV5BXFBlcuMO2z77bz0n7V+udtSm40wZLmDYXGT+N2ctVNxjv1h/AONgAa'.
*  XML_STRING = XML_STRING &&  'l+IXiu3jkyjYpMICNhkJIzilCjMy/X05Pyve8QGHypvh/TKfO/5oBnSaK4rdGAMWvHr7KTJW5dpS'.
*  XML_STRING = XML_STRING &&  'BdSqfrrhq2kWP2Uoes45o4dAC+BtdFYJLbq7bYYqvNS0xhxd4bumEKFcSzMx6kmlNtTVLIeO3Oq1'.
*  XML_STRING = XML_STRING &&  '/AMZ3mNMGL2LkuRtQx/zZ07l3lNU055H4FopV4Jz4q6vU5KX/i9D3x6I3cRcWKGzQaF5oJL/sPT4'.
*  XML_STRING = XML_STRING &&  'cpp0jT4rqc6vEfemkaoirQRK15f1CIXzqnQROrgaxcHqhLjaCOinlgkhwLSSdUs/78eJQ+ZRBomq'.
*  XML_STRING = XML_STRING &&  'QaEU9gt/vnLSlRtHMNMQsA0+x1nT4hBXKP2EWwLVpARgvE/uMnanuYOPhXzLMbWn1yI0xFBze+3F'.
*  XML_STRING = XML_STRING &&  'WFMMC/TbGyLPMTh5ULhASxKk8oqv</X509Certificate></X509Data></KeyInfo></Signature></CTe>'.
*  XML_STRING = XML_STRING &&  '<protCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00"><infProt Id="CTe313190000004124"><tpAmb>2</tpAmb>'.
*  XML_STRING = XML_STRING &&  '<verAplic>RS20190814112917</verAplic><chCTe>13190984590892000207570000000009921612732110</chCTe>'.
*  XML_STRING = XML_STRING &&  '<dhRecbto>2019-09-12T11:01:21-03:00</dhRecbto><nProt>313190000004124</nProt>'.
*  XML_STRING = XML_STRING &&  '<digVal>aCBcjcAPAEOOXByDF/VLGfB2B6U=</digVal><cStat>100</cStat><xMotivo>Autorizado o uso do CT-e</xMotivo></infProt></protCTe></cteProc>'.

* XML CT-e Unitapajos...
*    XML_STRING = XML_STRING &&  '<?xml version="1.0" encoding="utf-8"?><cteProc xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00">'.
*    XML_STRING = XML_STRING &&  '<CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte versao="3.00" Id="CTe15190911338257000174570000000085511667343556"><ide>'.
*    XML_STRING = XML_STRING &&  '<cUF>15</cUF><cCT>66734355</cCT><CFOP>5353</CFOP><natOp>Prestação serv. transp. estab. comercial</natOp><mod>57</mod>'.
*    XML_STRING = XML_STRING &&  '<serie>0</serie><nCT>8551</nCT><dhEmi>2019-09-30T14:45:57-03:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>6</cDV>'.
*    XML_STRING = XML_STRING &&  '<tpAmb>1</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi><verProc>008</verProc><cMunEnv>1501303</cMunEnv><xMunEnv>BARCARENA</xMunEnv>'.
*    XML_STRING = XML_STRING &&  '<UFEnv>PA</UFEnv><modal>03</modal><tpServ>0</tpServ><cMunIni>1503606</cMunIni><xMunIni>ITAITUBA</xMunIni><UFIni>PA</UFIni>'.
*    XML_STRING = XML_STRING &&  '<cMunFim>1501303</cMunFim><xMunFim>BARCARENA</xMunFim><UFFim>PA</UFFim><retira>1</retira><indIEToma>1</indIEToma><toma3>'.
*    XML_STRING = XML_STRING &&  '<toma>0</toma></toma3></ide><compl><xObs>Nao incidencia do ICMS conforme Liminar no Mandado de Seguranca n. 0868620-10.2018.8.14.0301 '.
*    XML_STRING = XML_STRING &&  'SUSPENSAO DE PIS/COFINS ART 40 . 6.,-A, DA LEI 10.865/2004 - E ADE N 503, DE 29 DE NOVEMBRO DE 2012. Nr. O.V: 0011810115 Nr. Fatura '.
*    XML_STRING = XML_STRING &&  '0093926118 Nr. Viagem 0050 Classificacao Transgenico Vlr. Dolar 9974.29 Tx. Dolar 4.15870</xObs></compl><emit><CNPJ>11338257000174</CNPJ>'.
*    XML_STRING = XML_STRING &&  '<IE>153941618</IE><xNome>NAVEGACOES UNIDAS TAPAJOS SA</xNome><xFant>NAVEG. UN.TAPAJOS - MATRIZ</xFant><enderEmit><xLgr>AVE BEIRA MAR '.
*    XML_STRING = XML_STRING &&  'PREDIO ADMINISTRATIV</xLgr><nro>SN</nro><xBairro>VILA ITUPANEMA</xBairro><cMun>1501303</cMun><xMun>BARCARENA</xMun><CEP>68447000</CEP>'.
*    XML_STRING = XML_STRING &&  '<UF>PA</UF><fone>09137548363</fone></enderEmit></emit><rem><CNPJ>77294254006982</CNPJ><IE>134652142</IE><xNome>AMAGGI EXPORTACAO E '.
*    XML_STRING = XML_STRING &&  'IMPORTACAO LTDA</xNome><fone>6536455290</fone><enderReme><xLgr>ROD BR 163 KM 1046 A DIREITA</xLgr><nro>S/N</nro><xBairro>ZONA RURAL</xBairro>'.
*    XML_STRING = XML_STRING &&  '<cMun>5105606</cMun><xMun>MATUPA</xMun><CEP>78525000</CEP><UF>MT</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderReme></rem><exped>'.
*    XML_STRING = XML_STRING &&  '<CNPJ>23771214000248</CNPJ><IE>155425579</IE><xNome>TERMINAL FRONTEIRA NORTE LOGISTICA</xNome><fone>6536455178</fone><enderExped>'.
*    XML_STRING = XML_STRING &&  '<xLgr>LOT GLEBA SANTA CRUZ</xLgr><nro>S/N</nro><xBairro>MIRITITUBA</xBairro><cMun>1503606</cMun><xMun>ITAITUBA</xMun><CEP>68191400</CEP>'.
*    XML_STRING = XML_STRING &&  '<UF>PA</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderExped></exped><receb><CNPJ>23771214000167</CNPJ><IE>155212737</IE>'.
*    XML_STRING = XML_STRING &&  '<xNome>TERMINAL FRONTEIRA DO NORTE LOGISTI LOGISTICA S.A.</xNome><enderReceb><xLgr>AV BEIRA MAR</xLgr><nro>S/N</nro>'.
*    XML_STRING = XML_STRING &&  '<xBairro>VILA DE ITUPANEMA</xBairro><cMun>1501303</cMun><xMun>BARCARENA</xMun><CEP>68447000</CEP><UF>PA</UF><cPais>1058</cPais>'.
*    XML_STRING = XML_STRING &&  '<xPais>Brasil</xPais></enderReceb></receb><dest><CNPJ>77294254006982</CNPJ><IE>134652142</IE><xNome>AMAGGI EXPORTACAO E IMPORTACAO '.
*    XML_STRING = XML_STRING &&  'LTDA</xNome><fone>6536455290</fone><enderDest><xLgr>ROD BR 163 KM 1046 A DIREITA</xLgr><nro>S/N</nro><xBairro>ZONA RURAL</xBairro>'.
*    XML_STRING = XML_STRING &&  '<cMun>5105606</cMun><xMun>MATUPA</xMun><CEP>78525000</CEP><UF>MT</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderDest></dest><vPrest>'.
*    XML_STRING = XML_STRING &&  '<vTPrest>41480.08</vTPrest><vRec>41480.08</vRec></vPrest><imp><ICMS><ICMS45><CST>41</CST></ICMS45></ICMS></imp><infCTeNorm><infCarga>'.
*    XML_STRING = XML_STRING &&  '<vCarga>544397.12</vCarga><proPred>MILHO EM GRAOS ADQ TERCEIROS</proPred><infQ><cUnid>01</cUnid><tpMed>PESO BRUTO</tpMed>'.
*    XML_STRING = XML_STRING &&  '<qCarga>960914.0000</qCarga></infQ></infCarga><infDoc><infNFe><chave>51190977294254006982550000000325951901192646</chave></infNFe>'.
*    XML_STRING = XML_STRING &&  '<infNFe><chave>51190977294254006982550000000325711809969816</chave></infNFe><infNFe><chave>51190977294254006982550000000326021512046785</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325871303315459</chave></infNFe><infNFe><chave>51190977294254006982550000000325891769973235</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325641886447611</chave></infNFe><infNFe><chave>51190977294254006982550000000325601550402737</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325561752053510</chave></infNFe><infNFe><chave>51190977294254006982550000000325671507831100</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325721643720071</chave></infNFe><infNFe><chave>51190977294254006982550000000325791964579587</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325661811388680</chave></infNFe><infNFe><chave>51190977294254006982550000000325971787571722</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000324321180489825</chave></infNFe><infNFe><chave>51190977294254006982550000000325581368230570</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000326081258544050</chave></infNFe><infNFe><chave>51190977294254006982550000000326051731694237</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000326091798786108</chave></infNFe><infNFe><chave>51190977294254006982550000000326061611050984</chave>'.
*    XML_STRING = XML_STRING &&  '</infNFe><infNFe><chave>51190977294254006982550000000325961528512730</chave></infNFe></infDoc><infModal versaoModal="3.00"><aquav><vPrest>41480.08</vPrest>'.
*    XML_STRING = XML_STRING &&  '<vAFRMM>0.00</vAFRMM><xNavio>E/M JOAO UGLIENGO-</xNavio><balsa><xBalsa>HT040-001-1439793</xBalsa></balsa><nViag>50</nViag><direc>N</direc><irin>PY2006</irin>'.
*    XML_STRING = XML_STRING &&  '</aquav></infModal></infCTeNorm></infCte>'.
*    XML_STRING = XML_STRING &&  '</CTe><protCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00"><infProt Id="CTe315190004417496"><tpAmb>1</tpAmb>'.
*    XML_STRING = XML_STRING &&  '<verAplic>RS20190924140950</verAplic><chCTe>15190911338257000174570000000085511667343556</chCTe><dhRecbto>2019-09-30T14:47:31-03:00</dhRecbto>'.
*    XML_STRING = XML_STRING &&  '<nProt>315190004417496</nProt><digVal>Kzxy+ESVa752aMPPjKIdQtN11Zo=</digVal><cStat>100</cStat><xMotivo>Autorizado o uso do CT-e</xMotivo>'.
*    XML_STRING = XML_STRING &&  '</infProt></protCTe></cteProc>'.

  "Qr Code Rodoviario

  XML_STRING = XML_STRING &&  '<?xml version="1.0" encoding="utf-8"?><cteProc xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00">'.
  XML_STRING = XML_STRING &&  '<CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte versao="3.00" Id="CTe51191077294254001670570000011553881221648016">'.
  XML_STRING = XML_STRING &&  '<ide><cUF>51</cUF><cCT>22164801</cCT><CFOP>5352</CFOP><natOp>Prestação serv. transp. estab. industrial</natOp><mod>57</mod>'.
  XML_STRING = XML_STRING &&  '<serie>0</serie><nCT>1155388</nCT><dhEmi>2019-10-05T00:27:13-04:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>6</cDV>'.
  XML_STRING = XML_STRING &&  '<tpAmb>1</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi><verProc>008</verProc><cMunEnv>5103403</cMunEnv><xMunEnv>CUIABA</xMunEnv>'.
  XML_STRING = XML_STRING &&  '<UFEnv>MT</UFEnv><modal>01</modal><tpServ>0</tpServ><cMunIni>5105259</cMunIni><xMunIni>LUCAS DO RIO VERDE</xMunIni><UFIni>MT</UFIni>'.
  XML_STRING = XML_STRING &&  '<cMunFim>5100300</cMunFim><xMunFim>ALTO ARAGUAIA</xMunFim><UFFim>MT</UFFim><retira>1</retira><indIEToma>1</indIEToma>'.
  XML_STRING = XML_STRING &&  '<toma3><toma>0</toma></toma3></ide><compl><xObs>NAO INCIDENCIA DE ICMS CONF. ART 5, PARAGRAFO 9 DO RICMS/MT 2014 Numero do Transporte: '.
  XML_STRING = XML_STRING &&  '0002522787 Numero do Faturamento: 0093936375 Subcontratado: DEOMAR COSMA ME RUA HORIZONTINA - Nr: 449 E Bairro: PIONEIRO - '.
  XML_STRING = XML_STRING &&  'Muni: Lucas do Rio Verde - MT CEP: 78455-000 RNTRC: 12898995 10.592.229/0001-16 IE-133675955 Local de entrega: Transbordo: '.
  XML_STRING = XML_STRING &&  'ALL AM LAT LOG MALHA NORTE S.A 24.962.466/0010-27 IE- Placa Cavalo-OBO6937-Lucas do Rio Verde /MT-DEOMAR COSMA '.
  XML_STRING = XML_STRING &&  'ME-CNPJ-10592229000116 Placa Carreta 1-AKH1299-Lucas do Rio Verde /MT-DEOMAR COSMA ME-CNPJ-10592229000116 Placa Carreta '.
  XML_STRING = XML_STRING &&  '2-AKH1303-Lucas do Rio Verde /MT-DEOMAR COSMA ME-CNPJ-10592229000116 Valor do pedagio: 288,90 Nr. CIOT: 138000360381 Nr. '.
  XML_STRING = XML_STRING &&  'Contrato de Viagem Administradora: 201901557498</xObs><ObsCont xCampo="Placa Cavalo"><xTexto>OBO6937-Lucas do Rio Verde / MT-DEOMAR '.
  XML_STRING = XML_STRING &&  'COSMA ME- CNPJ:10592229000116</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 1"><xTexto>AKH1299-Lucas do Rio Verde / MT-DEOMAR COSMA '.
  XML_STRING = XML_STRING &&  'ME- CNPJ:10592229000116</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 2"><xTexto>AKH1303-Lucas do Rio Verde / MT-DEOMAR COSMA '.
  XML_STRING = XML_STRING &&  'ME- CNPJ:10592229000116</xTexto></ObsCont><ObsCont xCampo="Motorista"><xTexto>AIRES RUVIARO- CPF:54440688004</xTexto></ObsCont>'.
  XML_STRING = XML_STRING &&  '</compl><emit><CNPJ>77294254001670</CNPJ><IE>131511726</IE><xNome>AMAGGI EXPORT E IMPORT LTDA</xNome><xFant>TRANSPORTADORA</xFant>'.
  XML_STRING = XML_STRING &&  '<enderEmit><xLgr>AV ANDRE ANTONIO MAGGI</xLgr><nro>303</nro><xBairro>ALVORADA</xBairro><cMun>5103403</cMun><xMun>CUIABA</xMun>'.
  XML_STRING = XML_STRING &&  '<CEP>78049080</CEP><UF>MT</UF><fone>6634113000</fone></enderEmit></emit><rem><CNPJ>77294254005587</CNPJ><IE>133241556</IE>'.
  XML_STRING = XML_STRING &&  '<xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome><fone>6535499300</fone><enderReme><xLgr>AVENIDA DAS INDUSTRIAS UNIDADE 1</xLgr>'.
  XML_STRING = XML_STRING &&  '<nro>SN</nro><xBairro>DISTRITO INDUSTRIAL</xBairro><cMun>5105259</cMun><xMun>LUCAS DO RIO VERDE</xMun><CEP>78455000</CEP><UF>MT</UF>'.
  XML_STRING = XML_STRING &&  '<cPais>1058</cPais><xPais>Brasil</xPais></enderReme></rem><exped><CNPJ>77294254005587</CNPJ><IE>133241556</IE><xNome>AMAGGI '.
  XML_STRING = XML_STRING &&  'EXPORTACAO E IMPORTACAO LTDA</xNome><fone>6535499300</fone><enderExped><xLgr>AVENIDA DAS INDUSTRIAS UNIDADE 1</xLgr><nro>SN</nro>'.
  XML_STRING = XML_STRING &&  '<xBairro>DISTRITO INDUSTRIAL</xBairro><cMun>5105259</cMun><xMun>LUCAS DO RIO VERDE</xMun><CEP>78455000</CEP><UF>MT</UF><cPais>1058</cPais>'.
  XML_STRING = XML_STRING &&  '<xPais>Brasil</xPais></enderExped></exped><receb><CNPJ>24962466001027</CNPJ><IE>ISENTO</IE><xNome>ALL AM LAT LOG MALHA NORTE S.A</xNome>'.
  XML_STRING = XML_STRING &&  '<enderReceb><xLgr>ROD KM 14 DA BR-364</xLgr><nro>SN</nro><xBairro>TERMINAL A. ARAGUAIA</xBairro><cMun>5100300</cMun><xMun>ALTO ARAGUAIA</xMun>'.
  XML_STRING = XML_STRING &&  '<CEP>78780000</CEP><UF>MT</UF><cPais>1058</cPais><xPais>Brasil</xPais></enderReceb></receb><dest><CNPJ>77294254005587</CNPJ>'.
  XML_STRING = XML_STRING &&  '<IE>133241556</IE><xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome><enderDest><xLgr>AVENIDA DAS INDUSTRIAS UNIDADE 001</xLgr>'.
  XML_STRING = XML_STRING &&  '<nro>SN</nro><xBairro>DISTRITO INDUSTRIAL</xBairro><cMun>5105259</cMun><xMun>LUCAS DO RIO VERDE</xMun><CEP>78455000</CEP><UF>MT</UF>'.
  XML_STRING = XML_STRING &&  '<cPais>1058</cPais><xPais>Brasil</xPais></enderDest></dest><vPrest><vTPrest>5935.40</vTPrest><vRec>5935.40</vRec></vPrest><imp><ICMS>'.
  XML_STRING = XML_STRING &&  '<ICMS45><CST>41</CST></ICMS45></ICMS></imp><infCTeNorm><infCarga><vCarga>73941.00</vCarga><proPred>FARELO SOJA HIPRO PROD PROPRIA</proPred>'.
  XML_STRING = XML_STRING &&  '<infQ><cUnid>01</cUnid><tpMed>PESO BRUTO</tpMed><qCarga>50300.0000</qCarga></infQ><vCargaAverb>73941.00</vCargaAverb></infCarga><infDoc>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '<infNFe><chave>51191077294254005587550000002993141218106650</chave></infNFe>'.
  XML_STRING = XML_STRING &&  '</infDoc><infModal versaoModal="3.00"><rodo>'.
  XML_STRING = XML_STRING &&  '<RNTRC>12458812</RNTRC></rodo></infModal></infCTeNorm></infCte><infCTeSupl>'.
  XML_STRING = XML_STRING &&  '<qrCodCTe>HTTPS://DFE-PORTAL.SVRS.RS.GOV.BR/CTE/QRCODE?chCTe=51191077294254001670570000011553881221648016&amp;tpAmb=1</qrCodCTe>'.
  XML_STRING = XML_STRING &&  '</infCTeSupl></CTe><protCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00">'.
  XML_STRING = XML_STRING &&  '<infProt><tpAmb>1</tpAmb><verAplic>3.00</verAplic><chCTe>51191077294254001670570000011553881221648016</chCTe>'.
  XML_STRING = XML_STRING &&  '<dhRecbto>2019-10-05T00:28:37-04:00</dhRecbto><nProt>151190319388887</nProt><digVal>W++3/zV7qLfcmi0QOuJz3c9pw3U=</digVal>'.
  XML_STRING = XML_STRING &&  '<cStat>100</cStat><xMotivo>Autorizado o uso da CT-e</xMotivo></infProt></protCTe></cteProc>'.


*    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN XML_STRING WITH 'a' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN XML_STRING WITH 'e' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF        'í'     IN XML_STRING WITH 'i' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN XML_STRING WITH 'o' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN XML_STRING WITH 'u' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN XML_STRING WITH 'c' IGNORING CASE.
*    REPLACE ALL OCCURRENCES OF        '&'     IN XML_STRING WITH '&#38;'.
*    REPLACE ALL OCCURRENCES OF        ''''    IN XML_STRING WITH '&#39;'.
*    REPLACE ALL OCCURRENCES OF        'º'     IN XML_STRING WITH 'o' IGNORING CASE.


  APPEND 'infNFe'  TO T_ELEMENT_ARRAY.
  APPEND 'infCTe'  TO T_ELEMENT_ARRAY.
  APPEND 'infNF'   TO T_ELEMENT_ARRAY.
  APPEND 'ObsCont' TO T_ELEMENT_ARRAY.

  DATA(_JSON) = ZCL_STRING=>XML_TO_JSON( I_XML =  XML_STRING I_ELEMENT_ARRAY = T_ELEMENT_ARRAY ).


  CALL METHOD /UI2/CL_JSON=>DESERIALIZE
    EXPORTING
      JSON = _JSON
    CHANGING
      DATA = WG_XML_SEFAZ.



ENDFORM.

FORM BUILD_INFO_INFO_XML.

  DATA: V_QTDE_AUX  TYPE J_1BNFLIN-MENGE,
        V_VLR_AUX   TYPE J_1BNFLIN-NETWR,
        VL_CST_ICMS TYPE C LENGTH 2.

  DATA: WL_DOC_ORIGIN  TYPE ZDE_DOCS_ORIGIN_CTE,
        V_OBS_CONT_TMP TYPE STRING.

  CLEAR: GT_DOCS_ORIGIN[], GS_OBSERVACOES, GS_OBSCONT, VL_CST_ICMS.

  CHECK WG_XML_SEFAZ IS NOT INITIAL.

  SELECT SINGLE *
    FROM J_1BNFLIN INTO @DATA(WL_LIN)
   WHERE DOCNUM EQ @GV_DOCNUM.

  IF SY-SUBRC EQ 0.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        INPUT  = WL_LIN-TAXSIT
      IMPORTING
        OUTPUT = VL_CST_ICMS.

    WG_DACTE-CST_ICMS = VL_CST_ICMS.

    CASE VL_CST_ICMS.
      WHEN '00'.
        WG_DACTE-CST_ICMS_DS = 'Tributado integralmente'.
      WHEN '10'.
        WG_DACTE-CST_ICMS_DS = 'Tributado e com cobrança do ICMS por substituição tributária'.
      WHEN '20'.
        WG_DACTE-CST_ICMS_DS = 'Com redução de base de cálculo'.
      WHEN '30'.
        WG_DACTE-CST_ICMS_DS = 'Isento ou não tributado e com cobrança do ICMS por substituição tributária'.
      WHEN '40'.
        WG_DACTE-CST_ICMS_DS = 'Isento'.
      WHEN '41'.
        WG_DACTE-CST_ICMS_DS = 'Não tributado'.
      WHEN '50'.
        WG_DACTE-CST_ICMS_DS = 'Suspensão'.
      WHEN '51'.
        WG_DACTE-CST_ICMS_DS = 'Diferimento'.
      WHEN '60'.
        WG_DACTE-CST_ICMS_DS = 'ICMS cobrado anteriormente por substituição tributária'.
      WHEN '70'.
        WG_DACTE-CST_ICMS_DS = 'Com redução de base de cálculo e cobrança do ICMS por substituição tributária'.
      WHEN '90'.
        WG_DACTE-CST_ICMS_DS = 'Outros'.
    ENDCASE.

  ENDIF.

  WG_DACTE-CHAVE     = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-A_ID+3(44).
  WG_DACTE-QRCODECTE = WG_XML_SEFAZ-CTEPROC-CTE-INFCTESUPL-QRCODCTE.


  "Inicio- Fim Prestação
  WG_DACTE-UFINI   = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-UFINI.
  WG_DACTE-XMUNINI = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-XMUNINI.

  WG_DACTE-UFFIM   = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-UFFIM.
  WG_DACTE-XMUNFIM = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-XMUNFIM.


  "CFOP - Natureza Operação
  WG_DACTE-CFOP  = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-CFOP.
  WG_DACTE-NATOP = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-NATOP.

*------------------------------------------------------------------------------------------*
* Informações Carga
*------------------------------------------------------------------------------------------*

  V_VLR_AUX = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFCARGA-VCARGA.
  WRITE V_VLR_AUX TO WG_DACTE-VCARGA DECIMALS 2 LEFT-JUSTIFIED NO-GAP.

  WG_DACTE-PROPRED = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFCARGA-PROPRED.

  CLEAR: V_QTDE_AUX, GS_COMPONENTES.

  DATA: LC_VALOR  TYPE CHAR100.

  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFCARGA-INFQ INTO DATA(WA_INFQ).

    CLEAR: V_QTDE_AUX.

    CASE WA_INFQ-CUNID.
      WHEN '01'. "KG
        V_QTDE_AUX = WA_INFQ-QCARGA.
      WHEN '02'. "TON
        V_QTDE_AUX = WA_INFQ-QCARGA.
        V_QTDE_AUX = V_QTDE_AUX * 1000.
    ENDCASE.

    CASE WA_INFQ-TPMED.
      WHEN 'PESO BRUTO'.
        WRITE V_QTDE_AUX TO WG_DACTE-PESO_BRUTO_KG DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        LC_VALOR  = WG_DACTE-PESO_BRUTO_KG.
      WHEN 'PESO LIQUIDO'.
        WRITE V_QTDE_AUX TO WG_DACTE-PESO_LIQUI_KG DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        LC_VALOR  = WG_DACTE-PESO_LIQUI_KG.
      WHEN OTHERS.
        IF WG_DACTE-PESO_BRUTO_KG IS INITIAL.
          WRITE V_QTDE_AUX TO WG_DACTE-PESO_BRUTO_KG DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        ELSE.
          WRITE V_QTDE_AUX TO LC_VALOR DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        ENDIF.

    ENDCASE.

    IF GS_COMPONENTES-NOME1 IS INITIAL.
      GS_COMPONENTES-NOME1  = WA_INFQ-TPMED.
      GS_COMPONENTES-VALOR1 = LC_VALOR.
    ELSEIF GS_COMPONENTES-NOME2 IS INITIAL.
      GS_COMPONENTES-NOME2  = WA_INFQ-TPMED.
      GS_COMPONENTES-VALOR2 = LC_VALOR.
    ELSEIF GS_COMPONENTES-NOME3 IS INITIAL.
      GS_COMPONENTES-NOME3  = WA_INFQ-TPMED.
      GS_COMPONENTES-VALOR3 = LC_VALOR.
    ELSEIF GS_COMPONENTES-NOME4 IS INITIAL.
      GS_COMPONENTES-NOME4  = WA_INFQ-TPMED.
      GS_COMPONENTES-VALOR4 = LC_VALOR.
    ENDIF.

  ENDLOOP.

  IF WG_DACTE-PESO_LIQUI_KG IS INITIAL.
    WG_DACTE-PESO_LIQUI_KG = WG_DACTE-PESO_BRUTO_KG.
  ENDIF.

*------------------------------------------------------------------------------------------*
* Dados Identificação
*------------------------------------------------------------------------------------------*

  GS_IDE_DACTE-MODAL = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-MODAL.

*------------------------------------------------------------------------------------------*
* Inf. Complementares
*------------------------------------------------------------------------------------------*

  GS_OBSERVACOES = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-COMPL-XOBS.

  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-COMPL-OBSCONT INTO DATA(WL_OBSCONT).
    CLEAR: V_OBS_CONT_TMP.
    CONCATENATE WL_OBSCONT-A_XCAMPO ':' WL_OBSCONT-XTEXTO INTO V_OBS_CONT_TMP.
    CONCATENATE GS_OBSCONT V_OBS_CONT_TMP  INTO GS_OBSCONT SEPARATED BY SPACE.
  ENDLOOP.

*------------------------------------------------------------------------------------------*
* Inf. Modal Aquav.
*------------------------------------------------------------------------------------------*
  GS_INF_MODAL_AQUAV-NAVIO  = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFMODAL-AQUAV-XNAVIO.

  WRITE WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFMODAL-AQUAV-VAFRMM TO GS_INF_MODAL_AQUAV-VAFRMM.
  WRITE WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFMODAL-AQUAV-VPREST TO GS_INF_MODAL_AQUAV-VPREST.


  CLEAR: GS_INF_MODAL_AQUAV-XBALSA.
  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFMODAL-AQUAV-BALSA INTO DATA(WA_BALSA).
    GS_INF_MODAL_AQUAV-XBALSA = ZCL_STRING=>CONCAT( EXPORTING S1 = CONV #( GS_INF_MODAL_AQUAV-XBALSA ) S2 = WA_BALSA-XBALSA SP = ';' ).
  ENDLOOP.

*------------------------------------------------------------------------------------------*
* Documentos Originarios
*------------------------------------------------------------------------------------------*
  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFDOC-INFNFE INTO DATA(WL_INF_NFE).
    CLEAR: WL_DOC_ORIGIN.

    WG_DACTE-TIPO_DOC_REF = 'NFE'.

    WL_DOC_ORIGIN-TIPO_DOC       = 'NF-e'.

    WRITE WL_INF_NFE-CHAVE
       TO WL_DOC_ORIGIN-CHAVE_ACESSO
      USING EDIT MASK LC_EDIT_MASK.

    WL_DOC_ORIGIN-CNPJ_CPF_EMIT  = WL_INF_NFE-CHAVE+6(14).

    WRITE WL_INF_NFE-INFUNIDTRANSP-QTDRAT TO WL_DOC_ORIGIN-PESO_RATEADO.

    APPEND WL_DOC_ORIGIN TO GT_DOCS_ORIGIN.
    CLEAR: WL_DOC_ORIGIN.
  ENDLOOP.

****
  CLEAR: WL_DOC_ORIGIN.
  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-DOCANT-EMIDOCANT ASSIGNING FIELD-SYMBOL(<FS_EMIDOCANT>).

    READ TABLE <FS_EMIDOCANT>-IDDOCANT ASSIGNING FIELD-SYMBOL(<FS_IDDOCANT>)      INDEX 1.

    READ TABLE <FS_IDDOCANT>-IDDOCANTELE ASSIGNING FIELD-SYMBOL(<FS_IDDOCANTELE>) INDEX 1.

    WG_DACTE-TIPO_DOC_REF          = 'NFE'.
    WL_DOC_ORIGIN-TIPO_DOC         = 'CT-e'.
    WL_DOC_ORIGIN-CNPJ_CPF_EMIT  = <FS_EMIDOCANT>-CNPJ.

    WRITE <FS_IDDOCANTELE>-CHCTE
       TO WL_DOC_ORIGIN-CHAVE_ACESSO
      USING EDIT MASK LC_EDIT_MASK.
    APPEND WL_DOC_ORIGIN TO GT_DOCS_ORIGIN.
  ENDLOOP.
***


  LOOP AT WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-INFCTENORM-INFDOC-INFNF INTO DATA(WL_INF_NF).
    CLEAR: WL_DOC_ORIGIN.

    WG_DACTE-TIPO_DOC_REF = 'NFF'.

    WL_DOC_ORIGIN-TIPO_DOC       = 'NF'.

    WL_DOC_ORIGIN-NR_DOC = WL_INF_NF-NDOC.
    WL_DOC_ORIGIN-SERIE  = WL_INF_NF-SERIE.

    IF WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-REM-CPF IS NOT INITIAL.
      WL_DOC_ORIGIN-CNPJ_CPF_EMIT  = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-REM-CPF.
    ELSE.
      WL_DOC_ORIGIN-CNPJ_CPF_EMIT  = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-REM-CNPJ.
    ENDIF.

    WRITE WL_INF_NFE-INFUNIDTRANSP-QTDRAT TO WL_DOC_ORIGIN-PESO_RATEADO.

    APPEND WL_DOC_ORIGIN TO GT_DOCS_ORIGIN.
  ENDLOOP.

ENDFORM.


FORM F_TESTE.

  DATA: OTFDATA1      TYPE TSFOTF,
        VQTDE         TYPE J_1BNFLIN-MENGE,
        WL_DOC_ORIGIN TYPE ZDE_DOCS_ORIGIN_CTE.


  TNAPR-SFORM = 'Z_DACTE_ROAD_FTL'.

  PERFORM GET_DADOS_XML_TMP.

  PERFORM BUILD_INFO_INFO_XML.

  PERFORM PRINTING CHANGING OTFDATA1.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      I_OTF                    = OTFDATA1
    EXCEPTIONS
      CONVERT_OTF_TO_PDF_ERROR = 1
      CNTL_ERROR               = 2
      OTHERS                   = 3.

  CHECK 1 = 2.

  PERFORM ENTRY USING 0 '100'.

ENDFORM.

FORM PREPARE_CROSS_MODEL.
  DATA:
    ISSUER_TYP   TYPE J_1BPARTYP,
    ISSUER_ID    TYPE J_1BPARID,
    ISSUER_PARVW TYPE J_1BPARVW.

  DATA:
    DEST_TYP   TYPE J_1BPARTYP,
    DEST_ID    TYPE J_1BPARID,
    DEST_PARVW TYPE J_1BPARVW.

  DATA:
    LS_SADR         TYPE SADR,
    LV_CGC          TYPE J_1BCGC,
    LS_BRANCH       TYPE J_1BBRANCH,
    LS_ADDR1_VAL    TYPE ADDR1_VAL,
    LS_BRANCH_INNAD TYPE J_1BINNAD.

  DATA:
    LS_EXT_HEADER TYPE J_1BINDOC,
    LS_ACCESS_KEY TYPE J_1B_NFE_ACCESS_KEY.

  DATA:
    LV_TIM TYPE UZEIT,
    LV_DAT TYPE DATUM.

  MOVE NAST-OBJKY TO GV_DOCNUM.

*----------------------------------------------------------------------*
*    enqueue
*----------------------------------------------------------------------*

* lock active
*  CALL FUNCTION 'ENQUEUE_E_J1BNFE'
*    EXPORTING
*      MODE_J_1BNFE_ACTIVE = 'E'
*      DOCNUM              = GV_DOCNUM
*    EXCEPTIONS
*      FOREIGN_LOCK        = 1
*      SYSTEM_FAILURE      = 2
*      OTHERS              = 3.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ADD 1 TO RETCODE.
*    PERFORM PROTOCOL.
*  ENDIF.

* lock j1bnfdoc
*  CALL FUNCTION 'ENQUEUE_EJ_1BNFE'
*    EXPORTING
*      MODE_J_1BNFDOC = 'E'
*      DOCNUM         = GV_DOCNUM
*    EXCEPTIONS
*      FOREIGN_LOCK   = 1
*      SYSTEM_FAILURE = 2
*      OTHERS         = 3.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ADD 1 TO RETCODE.
*    PERFORM PROTOCOL.
*  ENDIF.
*
*  CHECK RETCODE = 0.

  CLEAR GS_NFEACTIVE.


* Get NF-e record from data base and check if it can be (re)printed
  CALL FUNCTION 'J_1B_NFE_VERIFY_PRINTABLE'             "1723841
    EXPORTING                                           "1723841
      IV_DOCNUM          = GV_DOCNUM                         "1723841
    IMPORTING                                           "1723841
      ES_NF_HEADER       = GS_NFDOC                          "1723841
      ES_ACTTAB          = GS_NFEACTIVE                      "1723841
    TABLES                                              "1723841
      NF_PARTNER         = GT_NFNAD                          "1723841
      NF_ITEM            = GT_NFLIN                          "1723841
      NF_ITEM_TAX        = GT_NFSTX                          "1723841
      NF_HEADER_MSG      = GT_NFDOC_MSG                      "1723841
      NF_REFER_MSG       = GT_NFREF                          "1723841
      NF_OT_PARTNER      = GT_NFCPD                          "1723841
      NF_CTE_RES         = GT_CTE_RES                        "1723841
      NF_CTE_DOCREF      = GT_CTE_DOCREF                     "1723841
    EXCEPTIONS                                          "1723841
      NO_ENTRY           = 1                                 "1723841
      STATUS_NOT_ALLOWED = 2                          "1723841
      OTHERS             = 3.                                "1723841

  IF SY-SUBRC <> 0.                                         "1772654
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO       "1772654
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.      "1772654
    ADD 1 TO RETCODE.                                       "1772654
    PERFORM PROTOCOL.                                       "1772654
  ENDIF.                                                    "1772654
                                                            "1772654
  CHECK RETCODE = 0.                                        "1772654

  IF GS_NFDOC-NFE NE 'X'.                                   "1723841
    MESSAGE E143(J1B_NFE) WITH GV_DOCNUM.                   "1723841
    PERFORM PROTOCOL.                                       "1772654
  ENDIF.                                                    "1723841



*       set printed flag
*  GS_NFDOC-PRINTD = 'X'.
*  GS_NFEACTIVE-PRINTD = 'X'.
*
*  PERFORM NFDOC_UPDATE ON COMMIT.
*  PERFORM ACTIVE_UPDATE ON COMMIT.

*----------------------------------------------------------------------*
*    issuer
*----------------------------------------------------------------------*

  CLEAR LS_BRANCH_INNAD.
  CALL FUNCTION 'J_1B_BRANCH_READ'
    EXPORTING
      BRANCH            = GS_NFDOC-BRANCH
      COMPANY           = GS_NFDOC-BUKRS
    IMPORTING
      ADDRESS           = LS_SADR
      BRANCH_RECORD     = LS_BRANCH
      CGC_NUMBER        = LV_CGC
      ADDRESS_VALUE     = LS_ADDR1_VAL
    EXCEPTIONS
      BRANCH_NOT_FOUND  = 1
      ADDRESS_NOT_FOUND = 2
      COMPANY_NOT_FOUND = 3
      OTHERS            = 4.

  IF SY-SUBRC <> 0.
    PERFORM PROTOCOL.
  ENDIF.
  CHECK RETCODE IS INITIAL.

  MOVE-CORRESPONDING LS_SADR      TO LS_BRANCH_INNAD.
  MOVE-CORRESPONDING LS_ADDR1_VAL TO LS_BRANCH_INNAD.
  MOVE-CORRESPONDING LS_BRANCH    TO LS_BRANCH_INNAD.
  MOVE LV_CGC                     TO LS_BRANCH_INNAD-CGC.   "1920785
  MOVE LS_BRANCH-STATE_INSC       TO LS_BRANCH_INNAD-STAINS. "1920785

  GS_ISSUER      = LS_BRANCH_INNAD.

*----------------------------------------------------------------------*
*    extended header information
*----------------------------------------------------------------------*

  CLEAR GS_PRNFEHD.
  MOVE-CORRESPONDING GS_NFEACTIVE TO LS_ACCESS_KEY.
  MOVE LS_ACCESS_KEY TO GS_PRNFEHD-ACCESS_KEY.

  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
    EXPORTING
      NF_HEADER   = GS_NFDOC
    IMPORTING
      EXT_HEADER  = LS_EXT_HEADER
    TABLES
      NF_ITEM     = GT_NFLIN
      NF_ITEM_TAX = GT_NFSTX
*     EXT_ITEM    =
*     EXT_TOTAL_TAX =
    .
  MOVE LS_EXT_HEADER-NFNET TO GS_PRNFEHD-NFNET.
  MOVE LS_EXT_HEADER-NFTOT TO GS_PRNFEHD-NFTOT.

  IF GS_NFDOC-DIRECT NE '1' OR
     GS_NFDOC-ENTRAD EQ 'X'.
    CONVERT TIME STAMP GS_NFDOC-CRE_TIMESTAMP
          TIME ZONE LS_SADR-TZONE
          INTO DATE LV_DAT
               TIME LV_TIM.
    IF SY-SUBRC = 0.
      GS_PRNFEHD-CREDAT_ISSUER = LV_DAT.
      GS_PRNFEHD-CRETIM_ISSUER = LV_TIM.
    ENDIF.
  ENDIF.

ENDFORM.                    "prepare_cross_model

FORM GET_DADOS_XML_CTE.

  DATA: T_ELEMENT_ARRAY TYPE ZDE_ELEMENT_ARRAY_T.

  MOVE NAST-OBJKY TO GV_DOCNUM.

  CHECK GV_DOCNUM IS NOT INITIAL.

  TRY .
      ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = GV_DOCNUM
        )->SET_REGISTRO( EXPORTING I_DOCNUM       =  GV_DOCNUM
                                   I_SEM_BLOQUEIO = ABAP_TRUE
        )->GET_CK_AUTORIZADO_USO(
        )->GET_XML_GRC( IMPORTING E_XML_STRING = DATA(_XML_DOC) ).
    CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC).    " .
      EX_DOC->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
      RETURN.
  ENDTRY.

  CHECK _XML_DOC IS NOT INITIAL.

  APPEND 'infNFe'       TO T_ELEMENT_ARRAY.
  APPEND 'infCTe'       TO T_ELEMENT_ARRAY.
  APPEND 'infNF'        TO T_ELEMENT_ARRAY.
  APPEND 'ObsCont'      TO T_ELEMENT_ARRAY.
  APPEND 'balsa'        TO T_ELEMENT_ARRAY.
  APPEND 'infQ'         TO T_ELEMENT_ARRAY.
  APPEND 'emiDocAnt'    TO T_ELEMENT_ARRAY.
  APPEND 'idDocAnt'     TO T_ELEMENT_ARRAY.
  APPEND 'idDocAntEle'  TO T_ELEMENT_ARRAY.

  DATA(_JSON) = ZCL_STRING=>XML_TO_JSON( I_XML           = _XML_DOC
                                         I_ELEMENT_ARRAY =  T_ELEMENT_ARRAY ).

  CALL METHOD /UI2/CL_JSON=>DESERIALIZE
    EXPORTING
      JSON = _JSON
    CHANGING
      DATA = WG_XML_SEFAZ.

ENDFORM.

FORM NFDOC_UPDATE.

  UPDATE J_1BNFDOC FROM GS_NFDOC.

  IF SY-SUBRC <> 0.
    MESSAGE A022(J1B_NFE) WITH GS_NFDOC-DOCNUM.
  ENDIF.

ENDFORM.                    "nfdoc_update
*&---------------------------------------------------------------------*
*&      Form  ACTIVE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ACTIVE_UPDATE.

  UPDATE J_1BNFE_ACTIVE FROM GS_NFEACTIVE.

  IF SY-SUBRC <> 0.
    MESSAGE A021(J1B_NFE) WITH GS_NFEACTIVE-DOCNUM.
  ENDIF.

ENDFORM.                    "active_update


*&---------------------------------------------------------------------*
*&      Form  protocol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROTOCOL.

  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = SYST-MSGID
      MSG_NR    = SYST-MSGNO
      MSG_TY    = SYST-MSGTY
      MSG_V1    = SYST-MSGV1
      MSG_V2    = SYST-MSGV2
      MSG_V3    = SYST-MSGV3
      MSG_V4    = SYST-MSGV4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                    "protocol
*&---------------------------------------------------------------------*
*&      Form  GET_DOMTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LC_DOMNAME  text
*      -->P_LV_VALPOS  text
*      <--P_LV_DOMTEXT  text
*----------------------------------------------------------------------*
FORM APPEND_DOMTEXT  USING PV_DOMNAME
                           PV_DOMVALUE TYPE DOMVALUE_L.

  DATA:
    LT_DD07V     TYPE TABLE OF DD07V,
    LS_DD07V     TYPE DD07V,
    LV_RC        TYPE SY-SUBRC,
    LS_PRNFETEXT TYPE J_1BPRNFETEXT.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      DOMNAME   = PV_DOMNAME
      TEXT      = 'X'
      LANGU     = NAST-SPRAS
    IMPORTING
      RC        = LV_RC
    TABLES
      DD07V_TAB = LT_DD07V.

  IF LV_RC <> 0.
    PERFORM PROTOCOL.
  ENDIF.

  READ TABLE LT_DD07V INTO LS_DD07V
    WITH KEY
     DOMVALUE_L = PV_DOMVALUE.

  CHECK SY-SUBRC = 0.

  LS_PRNFETEXT-NAME = PV_DOMNAME.
  LS_PRNFETEXT-VALUE = LS_DD07V-DDTEXT.

  APPEND LS_PRNFETEXT TO GT_PRNFETEXT.

ENDFORM.                    "append_domtext
*&---------------------------------------------------------------------*
*&      Form  APPEND_CFOPTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_CFOPTEXT .

  DATA:
    LV_VERSION   TYPE J_1BCFOP_VER,
    LS_J_1BAGNT  TYPE J_1BAGNT,
    LS_PRNFETEXT TYPE J_1BPRNFETEXT.

  CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
    EXPORTING
      LAND1             = GS_DESTINATION-LAND1
      REGION            = GS_DESTINATION-REGIO
      DATE              = GS_NFDOC-PSTDAT
    IMPORTING
      VERSION           = LV_VERSION
    EXCEPTIONS
      DATE_MISSING      = 1
      VERSION_NOT_FOUND = 2
      OTHERS            = 3.

  CHECK SY-SUBRC = 0.

  SELECT SINGLE * FROM  J_1BAGNT INTO LS_J_1BAGNT
       WHERE  SPRAS    = NAST-SPRAS
       AND    VERSION  = LV_VERSION
       AND    CFOP     = GS_LIN-CFOP.

  CHECK SY-SUBRC = 0.

  LS_PRNFETEXT-NAME = 'J_1BCFOTXT'.
  LS_PRNFETEXT-VALUE = LS_J_1BAGNT-CFOTXT.
  APPEND LS_PRNFETEXT TO GT_PRNFETEXT.


ENDFORM.                    " APPEND_CFOPTEXT
*&---------------------------------------------------------------------*
*&      Form  APPEND_TAXSITTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_TAXSITTEXT.

  DATA:
    LS_ATL1T     TYPE J_1BATL1T,
    LS_PRNFETEXT TYPE J_1BPRNFETEXT.

  CONSTANTS:
    LC_TAXLAW TYPE NAME_FELD VALUE 'J_1BTAXLW1'.

  SELECT SINGLE * FROM J_1BATL1T INTO LS_ATL1T
   WHERE LANGU = NAST-SPRAS
   AND   TAXLAW = GS_LIN-TAXLW1.

  CHECK SY-SUBRC = 0.

  LS_PRNFETEXT-NAME = LC_TAXLAW.
  LS_PRNFETEXT-VALUE = LS_ATL1T-DESCRIP.
  APPEND LS_PRNFETEXT TO GT_PRNFETEXT.

ENDFORM.                    " APPEND_TAXSITTEXT
*&---------------------------------------------------------------------*
*&      Form  APPEND_TXJURTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_TXJURTEXT .
  DATA:
    LS_TXJURT    TYPE J_1BTXJURT,
    LS_PRNFETEXT TYPE J_1BPRNFETEXT.

  CONSTANTS:
    LC_S_TXJUR TYPE NAME_FELD VALUE 'J_1BCTE_SJCD',
    LC_E_TXJUR TYPE NAME_FELD VALUE 'J_1BCTE_EJCD'.

  SELECT SINGLE * FROM J_1BTXJURT INTO LS_TXJURT
    WHERE SPRAS =  NAST-SPRAS
      AND COUNTRY = 'BR'
      AND TAXJURCODE = GS_NFDOC-CTE_STRT_LCT.

  IF SY-SUBRC = 0.
    LS_PRNFETEXT-NAME = LC_S_TXJUR.
    LS_PRNFETEXT-VALUE = LS_TXJURT-TEXT.
    APPEND LS_PRNFETEXT TO GT_PRNFETEXT.
  ENDIF.

  SELECT SINGLE * FROM J_1BTXJURT INTO LS_TXJURT
    WHERE SPRAS =  NAST-SPRAS
      AND COUNTRY = 'BR'
      AND TAXJURCODE = GS_NFDOC-CTE_END_LCT.

  IF SY-SUBRC = 0.
    LS_PRNFETEXT-NAME = LC_E_TXJUR.
    LS_PRNFETEXT-VALUE = LS_TXJURT-TEXT.
    APPEND LS_PRNFETEXT TO GT_PRNFETEXT.
  ENDIF.

ENDFORM.                    " APPEND_TXJURTEXT


FORM PRINTING CHANGING OTFDATA TYPE TSFOTF.

  DATA: T_JOB_OUTPUT_INFO TYPE  SSFCRESCL,
        FM_NAME           TYPE RS38L_FNAM.

  TNAPR-SFORM = 'Z_DACTE_ROAD_FTL'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = TNAPR-SFORM
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  OUTPUT_OPTIONS-TDNEWID  = 'X'.
  OUTPUT_OPTIONS-TDDEST   = NAST-LDEST.
  OUTPUT_OPTIONS-TDIMMED  = NAST-DIMME.                     "1897281
  OUTPUT_OPTIONS-TDDELETE = 'X'.                            "1897281

  CLEAR: CONTROL_PARAMETERS.

  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  CONTROL_PARAMETERS-DEVICE    = 'PRINTER'.
  CONTROL_PARAMETERS-PREVIEW   = ' '.
  CONTROL_PARAMETERS-GETOTF    = 'X'.

  CALL FUNCTION FM_NAME
    EXPORTING
      CONTROL_PARAMETERS   = CONTROL_PARAMETERS
      OUTPUT_OPTIONS       = OUTPUT_OPTIONS
      USER_SETTINGS        = ''
      S_DESTINATION        = GS_DESTINATION
      S_ISSUER             = GS_ISSUER
      IS_DOC               = GS_NFDOC
      IS_ADDITIONAL_FIELDS = GS_PRNFEHD
      IS_LIN               = GS_LIN
      IS_ACTIVE            = GS_NFEACTIVE
      IS_ISSUER            = GS_ISSUER
      IS_GOODS_SENDER      = GS_GOODS_SENDER
      IS_DESTINATION       = GS_DESTINATION
      IS_CARGO_DISPATCHER  = GS_CARGO_DISPATCHER
      IS_CARGO_RECIPIENT   = GS_CARGO_RECIPIENT
      IS_SERVICE_TAKER     = GS_SERVICE_TAKER
      I_OBSCONT            = GS_OBSCONT
      I_OBSERVACOES        = GS_OBSERVACOES
      I_IDE_DACTE          = GS_IDE_DACTE
      I_INF_MODAL_AQUAV    = GS_INF_MODAL_AQUAV
      I_DADOS_CTE          = WG_DACTE
      I_COMPONENTES        = GS_COMPONENTES
    IMPORTING
      JOB_OUTPUT_INFO      = T_JOB_OUTPUT_INFO
    TABLES
      IT_DOC_REF           = GT_CTE_DOCREF
      IT_TAX               = GT_PRNFESTX
      IT_CARRIER           = GT_CTE_RES
      IT_TEXT_FIELDS       = GT_PRNFETEXT
      IT_DOCS_ORIGIN       = GT_DOCS_ORIGIN
    EXCEPTIONS
      FORMATTING_ERROR     = 1
      INTERNAL_ERROR       = 2
      SEND_ERROR           = 3
      USER_CANCELED        = 4
      OTHERS               = 5.

  IF SY-SUBRC = 0.
    MOVE T_JOB_OUTPUT_INFO-OTFDATA TO OTFDATA.
  ENDIF.

ENDFORM.                    "printing

FORM PREPARE_MODEL57 .

  CONSTANTS:
    LC_TRANSPTN_MODE TYPE DOMNAME VALUE 'J_1BCTE_TRANSPTN_MODE',
    LC_SERVICE_TAKER TYPE DOMNAME VALUE 'J_1BCTE_SERVICE_TAKER',
    LC_CTTYPE        TYPE DOMNAME VALUE 'J_1BCTTYPE',
    LC_SERV_TP       TYPE DOMNAME VALUE 'J_1BCTE_SERV_TP'.

  CONSTANTS:
    LC_ICMS TYPE J_1BTAXGRP VALUE 'ICMS',
    LC_ICST TYPE J_1BTAXGRP VALUE 'ICST'.

  DATA:
    LS_PARTNER  TYPE J_1BNFNAD,
    LS_NFCPD    TYPE J_1BNFCPD,
    LS_ADR_PR   TYPE J_1BPRNFEINNAD,
    LS_NFSTX    TYPE J_1BNFSTX,
    LS_PRNFESTX TYPE J_1BPRNFESTX,
    LS_J_1BAJ   TYPE J_1BAJ.

  DATA:
    LV_DOMVALUE    TYPE DOMVALUE_L,
    LV_DOMTEXT(60) TYPE C.

  DATA:
   LV_TPCTE TYPE C.

  READ TABLE GT_NFLIN INTO GS_LIN INDEX 1.

* extend tax tables.
  LOOP AT GT_NFSTX INTO LS_NFSTX.

    CALL FUNCTION 'J_1BAJ_READ'
      EXPORTING
        TAXTYPE              = LS_NFSTX-TAXTYP
      IMPORTING
        E_J_1BAJ             = LS_J_1BAJ
      EXCEPTIONS
        NOT_FOUND            = 1
        PARAMETERS_INCORRECT = 2
        OTHERS               = 3.
    CHECK SY-SUBRC = 0.
    CHECK LS_J_1BAJ-TAXGRP = LC_ICMS OR
          LS_J_1BAJ-TAXGRP = LC_ICST.

    CLEAR LS_PRNFESTX.
    MOVE-CORRESPONDING LS_NFSTX TO LS_PRNFESTX.
    MOVE LS_J_1BAJ-TAXGRP TO LS_PRNFESTX-TAXGRP.

    APPEND LS_PRNFESTX TO GT_PRNFESTX.

  ENDLOOP.

* partner addresses according to CT-e roles
  CLEAR:
   GS_GOODS_SENDER, GS_CARGO_DISPATCHER,
   GS_CARGO_RECIPIENT, GS_DESTINATION.

* main partner standard case
  IF NOT GS_NFDOC-CTE_PARTNER IS INITIAL OR
      GS_NFDOC-CTE_SERV_TAKER = GC_OTHER.

    CLEAR LS_ADR_PR.
    IF GS_NFDOC-PARXCPDK IS INITIAL.
      MOVE-CORRESPONDING GS_NFDOC TO LS_ADR_PR.
    ELSE.
* main partner is onetime partner
      PERFORM ONETIME_TO_ADR
          USING GS_NFDOC-PARVW
          CHANGING LS_ADR_PR.

      IF LS_ADR_PR-SPRAS IS INITIAL.
        MOVE GS_NFDOC-SPRAS TO LS_ADR_PR-SPRAS.

      ENDIF.
    ENDIF.

    PERFORM COMPLETE_AND_ASSIGN_ADR
                USING GS_NFDOC-CTE_PARTNER
                CHANGING LS_ADR_PR.

*   for type other, address is assigned twice
    IF GS_NFDOC-CTE_SERV_TAKER = GC_OTHER.
      GS_SERVICE_TAKER = LS_ADR_PR.
    ENDIF.

  ENDIF.


  LOOP AT GT_NFNAD INTO LS_PARTNER.
    CLEAR LS_ADR_PR.
* normal partner
    IF LS_PARTNER-XCPDK IS INITIAL.
      MOVE-CORRESPONDING LS_PARTNER TO LS_ADR_PR.
* onetime partner
    ELSE.
      PERFORM ONETIME_TO_ADR
         USING LS_PARTNER-PARVW
         CHANGING LS_ADR_PR.

    ENDIF.
    PERFORM COMPLETE_AND_ASSIGN_ADR
                USING LS_PARTNER-CTE_PARTNER
                CHANGING LS_ADR_PR.
  ENDLOOP.


* address of service taker
  CASE GS_NFDOC-CTE_SERV_TAKER.
    WHEN GC_GOODS_SENDER.
      GS_SERVICE_TAKER = GS_GOODS_SENDER.
    WHEN GC_CARGO_DISPATCHER.
      GS_SERVICE_TAKER = GS_CARGO_DISPATCHER.
    WHEN GC_CARGO_RECIPIENT.
      GS_SERVICE_TAKER = GS_CARGO_RECIPIENT.
    WHEN GC_DESTINATION.
      GS_SERVICE_TAKER = GS_DESTINATION.
  ENDCASE.

*
  CALL FUNCTION 'J_1B_CTE_MAP_CTETYPE_TO_XML'
    EXPORTING
      IV_NFTYPE = GS_NFDOC-NFTYPE
    IMPORTING
      EV_TPCTE  = LV_TPCTE
*     ES_J_1BAA =
* EXCEPTIONS
*     NOT_FOUND = 1
*     OTHERS    = 2
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  GS_PRNFEHD-CTE_TYPE_XML = LV_TPCTE.


* domain texts
  LV_DOMVALUE =  GS_NFDOC-TRANSP_MODE.
  PERFORM APPEND_DOMTEXT USING LC_TRANSPTN_MODE
                               LV_DOMVALUE.

  LV_DOMVALUE = GS_NFDOC-CTE_SERV_TAKER.
  PERFORM APPEND_DOMTEXT USING LC_SERVICE_TAKER
                               LV_DOMVALUE.

*  LV_DOMVALUE = GS_NFDOC-SERV_TP.
  LV_DOMVALUE = WG_XML_SEFAZ-CTEPROC-CTE-INFCTE-IDE-TPSERV.
  PERFORM APPEND_DOMTEXT USING LC_SERV_TP
                               LV_DOMVALUE.

* table texts
  PERFORM APPEND_CFOPTEXT.

  PERFORM APPEND_TAXSITTEXT.

  PERFORM APPEND_TXJURTEXT.


ENDFORM.                    "prepare_model57
*&---------------------------------------------------------------------*
*&      Form  COMPLETE_AND_ASSIGN_ADR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM COMPLETE_AND_ASSIGN_ADR
              USING    LV_CTE_PARTNER TYPE J_1BCTE_TOMALI
              CHANGING CS_ADR_PR TYPE J_1BPRNFEINNAD.


  SELECT SINGLE LANDX FROM T005T INTO CS_ADR_PR-LANDX
     WHERE SPRAS = NAST-SPRAS AND
           LAND1 = CS_ADR_PR-LAND1.

  CASE LV_CTE_PARTNER.
    WHEN GC_GOODS_SENDER.
      GS_GOODS_SENDER = CS_ADR_PR.
    WHEN GC_CARGO_DISPATCHER.
      GS_CARGO_DISPATCHER = CS_ADR_PR.
    WHEN GC_CARGO_RECIPIENT.
      GS_CARGO_RECIPIENT = CS_ADR_PR.
    WHEN GC_DESTINATION.
      GS_DESTINATION = CS_ADR_PR.
  ENDCASE.

ENDFORM.                    " COMPLETE_AND_ASSIGN_ADR
*&---------------------------------------------------------------------*
*&      Form  ONETIME_TO_ADR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_NFCPD  text
*      <--P_LS_ADR_PR  text
*----------------------------------------------------------------------*
FORM ONETIME_TO_ADR  USING    LV_PARVW TYPE J_1BPARVW
                     CHANGING LS_ADR_PR TYPE J_1BPRNFEINNAD.

  DATA:
   LS_NFCPD TYPE J_1BNFCPD.

  READ TABLE GT_NFCPD INTO LS_NFCPD
       WITH KEY PARVW = LV_PARVW.
  CHECK SY-SUBRC = 0.

  MOVE-CORRESPONDING LS_NFCPD TO LS_ADR_PR.

  MOVE LS_NFCPD-STCD1 TO LS_ADR_PR-CGC.
  MOVE LS_NFCPD-STCD2 TO LS_ADR_PR-CPF.
  MOVE LS_NFCPD-J_1BSTAINS TO LS_ADR_PR-STAINS.
  MOVE LS_NFCPD-J_1BMUNINS TO LS_ADR_PR-MUNINS.


ENDFORM.                    " ONETIME_TO_ADR
