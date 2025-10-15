*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print electronic fiscal document                                   *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  z_1bnfepr MESSAGE-ID 8b.

TABLES: j_1bnfdoc,
        NAST,                          "Messages
        *NAST,                         "Messages
        TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive parameters
        TOA_DARA,                      "Archive parameters
        ADDR_KEY.                      "Adressnumber for ADDRESS

DATA: output_options TYPE ssfcompop.
DATA: control_parameters TYPE ssfctrlop.

DATA: gs_nfdoc       TYPE j_1bnfdoc,
      gt_nfdoc_add   TYPE j_1bindoc,
      gt_nfnad       TYPE TABLE OF j_1bnfnad,
      gt_nflin       TYPE TABLE OF j_1bnflin,
      gt_nfstx       TYPE TABLE OF j_1bnfstx,
      gt_nfdoc_msg   TYPE TABLE OF j_1bnfftx,
      gt_nfref       TYPE TABLE OF j_1bnfref,
      gt_nfcpd       TYPE TABLE OF j_1bnfcpd,
      gt_cte_res     TYPE TABLE OF j_1bcte_d_res,
      gt_cte_docref  TYPE TABLE OF j_1bcte_d_docref,
      gs_obscont     TYPE char4000,
      gs_observacoes TYPE char4000,
      gt_docs_origin TYPE zde_docs_origin_cte_t.

DATA:
 gs_nfeactive TYPE j_1bnfe_active.

DATA:
  gs_prnfehd   TYPE j_1bprnfehd,
  gt_prnfetext TYPE j_1bprnfetext_tab,
  gt_prnfestx  TYPE TABLE OF j_1bprnfestx.

DATA:
 gs_lin TYPE j_1bnflin.

* CT-E SPECIFIC PARTNERS
DATA:
  gs_issuer           TYPE j_1binnad,
  gs_goods_sender     TYPE j_1bprnfeinnad,
  gs_cargo_dispatcher TYPE j_1bprnfeinnad,
  gs_cargo_recipient  TYPE j_1bprnfeinnad,
  gs_destination      TYPE j_1bprnfeinnad,
  gs_service_taker    TYPE j_1bprnfeinnad.

CONSTANTS:
  gc_goods_sender     TYPE j_1bcte_tomali VALUE '0',
  gc_cargo_dispatcher TYPE j_1bcte_tomali VALUE '1',
  gc_cargo_recipient  TYPE j_1bcte_tomali VALUE '2',
  gc_destination      TYPE j_1bcte_tomali VALUE '3',
  gc_other            TYPE j_1bcte_tomali VALUE '4'.

DATA:
  gv_docnum TYPE j_1bdocnum.

DATA:
  retcode LIKE sy-subrc,           " RETURN CODE INDICATOR
  xscreen.                         " OUTPUT ON PRINTER OR SCREEN

DATA: wg_xml_sefaz TYPE zcte_xml_sefaz_auth.



DATA: gs_ide_dacte       TYPE zde_ide_xml_dacte,
      gs_inf_modal_aquav TYPE zde_inf_modal_aquav_dacte.


"INITIALIZATION.
"PERFORM F_TESTE.

FORM entry USING return_code us_screen.

  DATA: otfdata TYPE tsfotf.

  PERFORM imprimir_dacte USING return_code us_screen
                      CHANGING otfdata.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_otf                    = otfdata
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFORM.                               " ENTRY

FORM entry2 USING return_code us_screen p_nast TYPE nast
         CHANGING otfdata TYPE tsfotf.

  MOVE-CORRESPONDING p_nast TO nast.
  tnapr-sform = 'Z_DACTE_ROAD_FTL'.

  PERFORM imprimir_dacte USING return_code us_screen CHANGING otfdata.

ENDFORM.                               " ENTRY

FORM imprimir_dacte USING return_code
                          us_screen
                 CHANGING otfdata  TYPE tsfotf.

  CLEAR retcode.

  CLEAR return_code.
  xscreen = us_screen.

  PERFORM get_dados_xml_cte.

  PERFORM build_info_info_xml.

  PERFORM prepare_cross_model.

  PERFORM prepare_model57.

  PERFORM printing CHANGING otfdata.

  IF retcode NE 0.
    return_code = 1.

    CALL FUNCTION 'DEQUEUE_E_J1BNFE'
      EXPORTING
        mode_j_1bnfe_active = 'E'
        docnum              = gv_docnum.

    CALL FUNCTION 'DEQUEUE_EJ_1BNFE'
      EXPORTING
        docnum = gv_docnum.
  ENDIF.

ENDFORM.

FORM get_dados_xml_tmp.


  DATA xml_string TYPE string.

  DATA: t_element_array TYPE zde_element_array_t.


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


  xml_string = '<?xml version="1.0" encoding="UTF-8"?><cteProc><CTe xmlns="http://www.portalfiscal.inf.br/cte"><infCte Id="CTe51190877294254001670570000011277431998301913" versao="3.00">'.
  xml_string = xml_string &&  '<ide><cUF>51</cUF><cCT>99830191</cCT><CFOP>6352</CFOP><natOp>Prestacao serv. transp. estab. Industrial</natOp><mod>57</mod><serie>0</serie><nCT>1127743</nCT>'.
  xml_string = xml_string &&  '<dhEmi>2019-08-08T00:18:48-04:00</dhEmi><tpImp>1</tpImp><tpEmis>1</tpEmis><cDV>3</cDV><tpAmb>1</tpAmb><tpCTe>0</tpCTe><procEmi>0</procEmi>'.
  xml_string = xml_string &&  '<verProc>Simetrya CTe v2.21-I</verProc><cMunEnv>5103304</cMunEnv><xMunEnv>Comodoro</xMunEnv><UFEnv>MT</UFEnv><modal>01</modal><tpServ>0</tpServ>'.
  xml_string = xml_string &&  '<cMunIni>5103304</cMunIni><xMunIni>Comodoro</xMunIni><UFIni>MT</UFIni><cMunFim>1100262</cMunFim><xMunFim>Rio Crespo</xMunFim><UFFim>RO</UFFim>'.
  xml_string = xml_string &&  '<retira>1</retira><indIEToma>1</indIEToma><toma3><toma>0</toma></toma3></ide><compl><xObs>Numero do Transporte: 0002452066 Numero do '.
  xml_string = xml_string &&  'Faturamento: 0093833343 Subcontratado: J A DA SILVA TRANSPORTE ESTRADA PIRES DE SA S/N - Nr:  Bairro: ZONA RURAL - Muni: VILHENA - '.
  xml_string = xml_string &&  'RO CEP: 78995-000 RNTRC: 12411366 07.901.867/0001-67 IE-00000001465252 Local de entrega: Transbordo: MARIZETE CARINA PERKOSKI  00.000.150/004 IE-00000004860250 '.
  xml_string = xml_string &&  'Placa Cavalo-MZP2772-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Placa Carreta 1-NBE8592-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Placa '.
  xml_string = xml_string &&  'Carreta 2-NBE8592-Vilhena /RO-J A DA SILVA TRANSPORTE-CNPJ-07901867000167 Nr. CIOT: 133000297575 Nr. Contrato de Viagem Administradora: 201901494513</xObs>'.
  xml_string = xml_string &&  '<ObsCont xCampo="Placa Cavalo"><xTexto>MZP2772-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 1">'.
  xml_string = xml_string &&  '<xTexto>NBE8592-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Placa Carreta 2">'.
  xml_string = xml_string &&  '<xTexto>NBE8592-Vilhena / RO-J A DA SILVA TRANSPORTE- CNPJ:07901867000167</xTexto></ObsCont><ObsCont xCampo="Motorista">'.
  xml_string = xml_string &&  '<xTexto>JOSUE ANTONIO DA SILVA- CPF:14409879987</xTexto></ObsCont></compl><emit><CNPJ>77294254001670</CNPJ><IE>131511726</IE>'.
  xml_string = xml_string &&  '<xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome><xFant>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xFant><enderEmit><xLgr>AVENIDA ANDRE ANTONIO MAGGI</xLgr>'.
  xml_string = xml_string &&  '<nro>303</nro><xCpl>3 ANDAR</xCpl><xBairro>ALVORADA</xBairro><cMun>5103403</cMun><xMun>Cuiaba</xMun><CEP>78049080</CEP><UF>MT</UF><fone>6536455000</fone>'.
  xml_string = xml_string &&  '</enderEmit></emit><rem><CNPJ>77294254007520</CNPJ><IE>135946328</IE><xNome>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome>'.
  xml_string = xml_string &&  '<xFant>AMAGGI EXPORTACAO E IMPORTACAO LTDA</xFant><fone>6536455000</fone><enderReme><xLgr>ROD BR364 KM 120 ENTRONCA</xLgr>'.
  xml_string = xml_string &&  '<nro>S/N</nro><xBairro>DISTRITO AGROINDUSTRIAL</xBairro><cMun>5103304</cMun><xMun>Comodoro</xMun><CEP>78310000</CEP><UF>MT</UF></enderReme></rem><dest>'.
  xml_string = xml_string &&  '<CPF>00150004001</CPF><IE>00000004860250</IE><xNome>MARIZETE CARINA PERKOSKI</xNome><enderDest><xLgr>LINHA C 85 GLEBA</xLgr><nro>27</nro><xBairro>ZONA RURAL</xBairro>'.
  xml_string = xml_string &&  '<cMun>1100262</cMun><xMun>Rio Crespo</xMun><CEP>76863000</CEP><UF>RO</UF></enderDest></dest><vPrest><vTPrest>1265.00</vTPrest><vRec>1265.00</vRec>'.
  xml_string = xml_string &&  '<Comp><xNome>FRETE PESO</xNome><vComp>1265.00</vComp></Comp></vPrest><imp><ICMS><ICMS00><CST>00</CST><vBC>1265.00</vBC><pICMS>12.00</pICMS>'.
  xml_string = xml_string &&  '<vICMS>151.80</vICMS></ICMS00></ICMS></imp><infCTeNorm><infCarga><vCarga>22400.61</vCarga><proPred>FERT CLORETO DE POTASSIO 60% BB</proPred>'.
  xml_string = xml_string &&  '<infQ><cUnid>01</cUnid><tpMed>PESO BRUTO</tpMed><qCarga>11000.0000</qCarga></infQ></infCarga><infDoc><infNFe>'.
  xml_string = xml_string &&  '<chave>51190877294254007520550000000378481182101412</chave></infNFe></infDoc><infModal versaoModal="3.00"><rodo>'.
  xml_string = xml_string &&  '<RNTRC>12458812</RNTRC></rodo></infModal></infCTeNorm></infCte></CTe></cteProc>'.

  APPEND 'infNFe'  TO t_element_array.
  APPEND 'ObsCont' TO t_element_array.

  DATA(_json) = zcl_string=>xml_to_json( i_xml =  xml_string ).


  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = wg_xml_sefaz.



ENDFORM.

FORM build_info_info_xml.

  DATA: wl_doc_origin  TYPE zde_docs_origin_cte,
        v_obs_cont_tmp TYPE string.

  CLEAR: gt_docs_origin[], gs_observacoes, gs_obscont.

*------------------------------------------------------------------------------------------*
* Dados Identificação
*------------------------------------------------------------------------------------------*

  gs_ide_dacte-modal = wg_xml_sefaz-cteproc-cte-infcte-ide-modal.

*------------------------------------------------------------------------------------------*
* Inf. Complementares
*------------------------------------------------------------------------------------------*

  gs_observacoes = wg_xml_sefaz-cteproc-cte-infcte-compl-xobs.

  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-compl-obscont INTO DATA(wl_obscont).
    CLEAR: v_obs_cont_tmp.
    CONCATENATE wl_obscont-a_xcampo ':' wl_obscont-xtexto INTO v_obs_cont_tmp.
    CONCATENATE gs_obscont v_obs_cont_tmp  INTO gs_obscont SEPARATED BY space.
  ENDLOOP.

*------------------------------------------------------------------------------------------*
* Inf. Modal Aquav.
*------------------------------------------------------------------------------------------*
  gs_inf_modal_aquav-navio  = wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infmodal-aquav-xnavio.

  WRITE wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infmodal-aquav-vafrmm TO gs_inf_modal_aquav-vafrmm.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infmodal-aquav-vprest TO gs_inf_modal_aquav-vprest.

*------------------------------------------------------------------------------------------*
* Documentos Originarios
*------------------------------------------------------------------------------------------*
  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infdoc-infnfe INTO DATA(wl_inf_nfe).
    CLEAR: wl_doc_origin.

    wl_doc_origin-tipo_doc       = 'NF-e'.
    wl_doc_origin-chave_acesso   = wl_inf_nfe-chave.
    wl_doc_origin-cnpj_cpf_emit  = wl_inf_nfe-chave+6(14).

    WRITE wl_inf_nfe-infunidtransp-qtdrat TO wl_doc_origin-peso_rateado.

    APPEND wl_doc_origin TO gt_docs_origin.
  ENDLOOP.

ENDFORM.


FORM f_teste.

  DATA: otfdata1      TYPE tsfotf,
        vqtde         TYPE j_1bnflin-menge,
        wl_doc_origin TYPE zde_docs_origin_cte.


  tnapr-sform = 'Z_DACTE_ROAD_FTL'.


  PERFORM get_dados_xml_tmp.

  PERFORM build_info_info_xml.

  PERFORM printing CHANGING otfdata1.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_otf                    = otfdata1
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFORM.

FORM prepare_cross_model.
  DATA:
    issuer_typ   TYPE j_1bpartyp,
    issuer_id    TYPE j_1bparid,
    issuer_parvw TYPE j_1bparvw.

  DATA:
    dest_typ TYPE j_1bpartyp,
    dest_id  TYPE j_1bparid,
    dest_parvw TYPE j_1bparvw.

  DATA:
   ls_sadr TYPE sadr,
   lv_cgc       TYPE j_1bcgc,
   ls_branch    TYPE j_1bbranch,
   ls_addr1_val TYPE addr1_val,
   ls_branch_innad TYPE j_1binnad.

  DATA:
   ls_ext_header TYPE j_1bindoc,
   ls_access_key TYPE j_1b_nfe_access_key.

  DATA:
    lv_tim TYPE uzeit,
    lv_dat TYPE datum.

  MOVE nast-objky TO gv_docnum.

*----------------------------------------------------------------------*
*    enqueue
*----------------------------------------------------------------------*

* lock active
  CALL FUNCTION 'ENQUEUE_E_J1BNFE'
    EXPORTING
      mode_j_1bnfe_active = 'E'
      docnum              = gv_docnum
    EXCEPTIONS
      foreign_lock        = 1
      system_failure      = 2
      OTHERS              = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ADD 1 TO retcode.
    PERFORM protocol.
  ENDIF.

* lock j1bnfdoc
  CALL FUNCTION 'ENQUEUE_EJ_1BNFE'
    EXPORTING
      mode_j_1bnfdoc = 'E'
      docnum         = gv_docnum
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ADD 1 TO retcode.
    PERFORM protocol.
  ENDIF.

  CHECK retcode = 0.

  CLEAR gs_nfeactive.


* Get NF-e record from data base and check if it can be (re)printed
    CALL FUNCTION 'J_1B_NFE_VERIFY_PRINTABLE'             "1723841
      EXPORTING                                           "1723841
        iv_docnum     = gv_docnum                         "1723841
      IMPORTING                                           "1723841
        ES_NF_HEADER  = gs_nfdoc                          "1723841
        eS_ACTTAB     = gs_nfeactive                      "1723841
      TABLES                                              "1723841
        NF_PARTNER    = gt_nfnad                          "1723841
        NF_ITEM       = gt_nflin                          "1723841
        NF_ITEM_TAX   = gt_nfstx                          "1723841
        NF_HEADER_MSG = gt_nfdoc_msg                      "1723841
        NF_REFER_MSG  = gt_nfref                          "1723841
        NF_ot_partner = gt_nfcpd                          "1723841
        NF_cte_res    = gt_cte_res                        "1723841
        NF_cte_docref = gt_cte_docref                     "1723841
      EXCEPTIONS                                          "1723841
        no_entry      = 1                                 "1723841
        status_not_allowed   = 2                          "1723841
        OTHERS        = 3.                                "1723841

  IF sy-subrc <> 0.                                       "1772654
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno     "1772654
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.    "1772654
    ADD 1 TO retcode.                                     "1772654
    PERFORM protocol.                                     "1772654
  ENDIF.                                                  "1772654
                                                          "1772654
  CHECK retcode = 0.                                      "1772654

*   print program only for NF-E                           "1723841
    IF gs_nfdoc-nfe ne 'X'.                               "1723841
      MESSAGE e143(j1b_nfe) WITH gv_docnum.               "1723841
      PERFORM protocol.                                   "1772654
    endif.                                                "1723841



*       set printed flag
  gs_nfdoc-printd = 'X'.
  gs_nfeactive-printd = 'X'.

  PERFORM nfdoc_update ON COMMIT.
  PERFORM active_update ON COMMIT.

*----------------------------------------------------------------------*
*    issuer
*----------------------------------------------------------------------*

  CLEAR ls_branch_innad.
  CALL FUNCTION 'J_1B_BRANCH_READ'
    EXPORTING
      branch            = gs_nfdoc-branch
      company           = gs_nfdoc-bukrs
    IMPORTING
      address           = ls_sadr
      branch_record     = ls_branch
      cgc_number        = lv_cgc
      address_value     = ls_addr1_val
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  IF sy-subrc <> 0.
    PERFORM protocol.
  ENDIF.
  CHECK retcode IS INITIAL.

  MOVE-CORRESPONDING ls_sadr      TO ls_branch_innad.
  MOVE-CORRESPONDING ls_addr1_val TO ls_branch_innad.
  MOVE-CORRESPONDING ls_branch    TO ls_branch_innad.
  MOVE lv_cgc                     TO ls_branch_innad-cgc.       "1920785
  MOVE ls_branch-state_insc       TO ls_branch_innad-stains.    "1920785

  gs_issuer      = ls_branch_innad.

*----------------------------------------------------------------------*
*    extended header information
*----------------------------------------------------------------------*

  CLEAR gs_prnfehd.
  MOVE-CORRESPONDING gs_nfeactive TO ls_access_key.
  MOVE ls_access_key TO gs_prnfehd-access_key.

  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
    EXPORTING
      nf_header     = gs_nfdoc
    IMPORTING
      ext_header    = ls_ext_header
    TABLES
      nf_item       = gt_nflin
      nf_item_tax   = gt_nfstx
*     EXT_ITEM      =
*     EXT_TOTAL_TAX =
    .
  MOVE ls_ext_header-nfnet TO gs_prnfehd-nfnet.
  MOVE ls_ext_header-nftot TO gs_prnfehd-nftot.

  IF gs_nfdoc-direct NE '1' OR
     gs_nfdoc-entrad EQ 'X'.
    CONVERT TIME STAMP gs_nfdoc-cre_timestamp
          TIME ZONE ls_sadr-tzone
          INTO DATE lv_dat
               TIME lv_tim.
    IF sy-subrc = 0.
      gs_prnfehd-credat_issuer = lv_dat.
      gs_prnfehd-cretim_issuer = lv_tim.
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

  APPEND 'infNFe'  TO T_ELEMENT_ARRAY.
  APPEND 'ObsCont' TO T_ELEMENT_ARRAY.

  DATA(_JSON) = ZCL_STRING=>XML_TO_JSON( I_XML           = _XML_DOC
                                         I_ELEMENT_ARRAY =  T_ELEMENT_ARRAY ).

  CALL METHOD /UI2/CL_JSON=>DESERIALIZE
    EXPORTING
      JSON = _JSON
    CHANGING
      DATA = WG_XML_SEFAZ.


ENDFORM.

FORM nfdoc_update.

  UPDATE j_1bnfdoc FROM gs_nfdoc.

  IF sy-subrc <> 0.
    MESSAGE a022(j1b_nfe) WITH gs_nfdoc-docnum.
  ENDIF.

ENDFORM.                    "nfdoc_update
*&---------------------------------------------------------------------*
*&      Form  ACTIVE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM active_update.

  UPDATE j_1bnfe_active FROM gs_nfeactive.

  IF sy-subrc <> 0.
    MESSAGE a021(j1b_nfe) WITH gs_nfeactive-docnum.
  ENDIF.

ENDFORM.                    "active_update


*&---------------------------------------------------------------------*
*&      Form  protocol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM protocol.

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
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
FORM append_domtext  USING pv_domname
                           pv_domvalue TYPE domvalue_l.

  DATA:
   lt_dd07v TYPE TABLE OF dd07v,
   ls_dd07v TYPE dd07v,
   lv_rc    TYPE sy-subrc,
   ls_prnfetext TYPE j_1bprnfetext.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = pv_domname
      text      = 'X'
      langu     = nast-spras
    IMPORTING
      rc        = lv_rc
    TABLES
      dd07v_tab = lt_dd07v.

  IF lv_rc <> 0.
    PERFORM protocol.
  ENDIF.

  READ TABLE lt_dd07v INTO ls_dd07v
    WITH KEY
     domvalue_l = pv_domvalue.

  CHECK sy-subrc = 0.

  ls_prnfetext-name = pv_domname.
  ls_prnfetext-value = ls_dd07v-ddtext.

  APPEND ls_prnfetext TO gt_prnfetext.

ENDFORM.                    "append_domtext
*&---------------------------------------------------------------------*
*&      Form  APPEND_CFOPTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_cfoptext .

  DATA:
      lv_version   TYPE j_1bcfop_ver,
      ls_j_1bagnt TYPE j_1bagnt,
      ls_prnfetext TYPE j_1bprnfetext.

  CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
    EXPORTING
      land1             = gs_destination-land1
      region            = gs_destination-regio
      date              = gs_nfdoc-pstdat
    IMPORTING
      version           = lv_version
    EXCEPTIONS
      date_missing      = 1
      version_not_found = 2
      OTHERS            = 3.

  CHECK sy-subrc = 0.

  SELECT SINGLE * FROM  j_1bagnt INTO ls_j_1bagnt
       WHERE  spras    = nast-spras
       AND    version  = lv_version
       AND    cfop     = gs_lin-cfop.

  CHECK sy-subrc = 0.

  ls_prnfetext-name = 'J_1BCFOTXT'.
  ls_prnfetext-value = ls_j_1bagnt-cfotxt.
  APPEND ls_prnfetext TO gt_prnfetext.


ENDFORM.                    " APPEND_CFOPTEXT
*&---------------------------------------------------------------------*
*&      Form  APPEND_TAXSITTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_taxsittext.

  DATA:
   ls_atl1t TYPE j_1batl1t,
   ls_prnfetext TYPE j_1bprnfetext.

  CONSTANTS:
    lc_taxlaw TYPE name_feld VALUE 'J_1BTAXLW1'.

  SELECT SINGLE * FROM j_1batl1t INTO ls_atl1t
   WHERE langu = nast-spras
   AND   taxlaw = gs_lin-taxlw1.

  CHECK sy-subrc = 0.

  ls_prnfetext-name = lc_taxlaw.
  ls_prnfetext-value = ls_atl1t-descrip.
  APPEND ls_prnfetext TO gt_prnfetext.

ENDFORM.                    " APPEND_TAXSITTEXT
*&---------------------------------------------------------------------*
*&      Form  APPEND_TXJURTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_txjurtext .
  DATA:
    ls_txjurt    TYPE j_1btxjurt,
    ls_prnfetext TYPE j_1bprnfetext.

  CONSTANTS:
  lc_s_txjur TYPE name_feld VALUE 'J_1BCTE_SJCD',
  lc_e_txjur TYPE name_feld VALUE 'J_1BCTE_EJCD'.

  SELECT SINGLE * FROM j_1btxjurt INTO ls_txjurt
    WHERE spras =  nast-spras
      AND country = 'BR'
      AND taxjurcode = gs_nfdoc-cte_strt_lct.

  IF sy-subrc = 0.
    ls_prnfetext-name = lc_s_txjur.
    ls_prnfetext-value = ls_txjurt-text.
    APPEND ls_prnfetext TO gt_prnfetext.
  ENDIF.

  SELECT SINGLE * FROM j_1btxjurt INTO ls_txjurt
    WHERE spras =  nast-spras
      AND country = 'BR'
      AND taxjurcode = gs_nfdoc-cte_end_lct.

  IF sy-subrc = 0.
    ls_prnfetext-name = lc_e_txjur.
    ls_prnfetext-value = ls_txjurt-text.
    APPEND ls_prnfetext TO gt_prnfetext.
  ENDIF.

ENDFORM.                    " APPEND_TXJURTEXT


FORM printing CHANGING OTFDATA TYPE TSFOTF.

  DATA: t_job_output_info TYPE  ssfcrescl,
        fm_name           TYPE rs38l_fnam.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = tnapr-sform
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  output_options-tdnewid  = 'x'.
  output_options-tddest   = nast-ldest.
  output_options-tdimmed  = nast-dimme.                     "1897281
  output_options-tddelete = nast-delet.                     "1897281

  CLEAR: control_parameters.

  control_parameters-NO_DIALOG = 'X'.
  control_parameters-DEVICE    = 'PRINTER'.
  control_parameters-PREVIEW   = ' '.
  control_parameters-GETOTF    = 'X'.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters    = control_parameters
      output_options        = output_options
      user_settings         = ''

      s_destination         = gs_destination
      s_issuer              = gs_issuer
      is_doc                = gs_nfdoc
      is_additional_fields  = gs_prnfehd
      is_lin                = gs_lin
      is_active             = gs_nfeactive
      is_issuer             = gs_issuer
      is_goods_sender       = gs_goods_sender
      is_destination        = gs_destination
      is_cargo_dispatcher   = gs_cargo_dispatcher
      is_cargo_recipient    = gs_cargo_recipient
      is_service_taker      = gs_service_taker
      i_obscont             = gs_obscont
      i_observacoes         = gs_observacoes
      i_ide_dacte           = gs_ide_dacte
      i_inf_modal_aquav     = gs_inf_modal_aquav
    IMPORTING
      job_output_info       = t_job_output_info
    TABLES
      it_doc_ref            = gt_cte_docref
      it_tax                = gt_prnfestx
      it_carrier            = gt_cte_res
      it_text_fields        = gt_prnfetext
      it_docs_origin        = gt_docs_origin
   EXCEPTIONS
     formatting_error       = 1
     internal_error         = 2
     send_error             = 3
     user_canceled          = 4
     OTHERS                 = 5
            .
  IF sy-subrc = 0.
    MOVE t_job_output_info-OTFDATA TO OTFDATA.
  ENDIF.



ENDFORM.                    "printing

FORM prepare_model57 .

  CONSTANTS:
   lc_transptn_mode TYPE domname VALUE 'J_1BCTE_TRANSPTN_MODE',
   lc_service_taker TYPE domname VALUE 'J_1BCTE_SERVICE_TAKER',
   lc_cttype        TYPE domname VALUE 'J_1BCTTYPE',
   lc_serv_tp       TYPE domname VALUE 'J_1BCTE_SERV_TP'.

  CONSTANTS:
   lc_icms TYPE j_1btaxgrp VALUE 'ICMS',
   lc_icst TYPE j_1btaxgrp VALUE 'ICST'.

  DATA:
   ls_partner TYPE j_1bnfnad,
   ls_nfcpd TYPE j_1bnfcpd,
   ls_adr_pr  TYPE j_1bprnfeinnad,
   ls_nfstx TYPE j_1bnfstx,
   ls_prnfestx TYPE j_1bprnfestx,
   ls_j_1baj TYPE j_1baj.

  DATA:
   lv_domvalue TYPE domvalue_l,
   lv_domtext(60) TYPE c.

  DATA:
   lv_tpcte TYPE c.

  READ TABLE gt_nflin INTO gs_lin INDEX 1.

* extend tax tables.
  LOOP AT gt_nfstx INTO ls_nfstx.

    CALL FUNCTION 'J_1BAJ_READ'
      EXPORTING
        taxtype              = ls_nfstx-taxtyp
      IMPORTING
        e_j_1baj             = ls_j_1baj
      EXCEPTIONS
        not_found            = 1
        parameters_incorrect = 2
        OTHERS               = 3.
    CHECK sy-subrc = 0.
    CHECK ls_j_1baj-taxgrp = lc_icms OR
          ls_j_1baj-taxgrp = lc_icst.

    CLEAR ls_prnfestx.
    MOVE-CORRESPONDING ls_nfstx TO ls_prnfestx.
    MOVE ls_j_1baj-taxgrp TO ls_prnfestx-taxgrp.

    APPEND ls_prnfestx TO gt_prnfestx.

  ENDLOOP.

* partner addresses according to CT-e roles
  CLEAR:
   gs_goods_sender, gs_cargo_dispatcher,
   gs_cargo_recipient, gs_destination.

* main partner standard case
  IF NOT gs_nfdoc-cte_partner IS INITIAL OR
      gs_nfdoc-cte_serv_taker = gc_other.

    CLEAR ls_adr_pr.
    IF gs_nfdoc-parxcpdk IS INITIAL.
      MOVE-CORRESPONDING gs_nfdoc TO ls_adr_pr.
    ELSE.
* main partner is onetime partner
      PERFORM onetime_to_adr
          USING gs_nfdoc-parvw
          CHANGING ls_adr_pr.

      IF ls_adr_pr-spras IS INITIAL.
        MOVE gs_nfdoc-spras TO ls_adr_pr-spras.

      ENDIF.
    ENDIF.

    PERFORM complete_and_assign_adr
                USING gs_nfdoc-cte_partner
                CHANGING ls_adr_pr.

*   for type other, address is assigned twice
    IF gs_nfdoc-cte_serv_taker = gc_other.
      gs_service_taker = ls_adr_pr.
    ENDIF.

  ENDIF.


  LOOP AT gt_nfnad INTO ls_partner.
    CLEAR ls_adr_pr.
* normal partner
    IF ls_partner-xcpdk IS INITIAL.
      MOVE-CORRESPONDING ls_partner TO ls_adr_pr.
* onetime partner
    ELSE.
      PERFORM onetime_to_adr
         USING ls_partner-parvw
         CHANGING ls_adr_pr.

    ENDIF.
    PERFORM complete_and_assign_adr
                USING ls_partner-cte_partner
                CHANGING ls_adr_pr.
  ENDLOOP.


* address of service taker
  CASE gs_nfdoc-cte_serv_taker.
    WHEN gc_goods_sender.
      gs_service_taker = gs_goods_sender.
    WHEN gc_cargo_dispatcher.
      gs_service_taker = gs_cargo_dispatcher.
    WHEN gc_cargo_recipient.
      gs_service_taker = gs_cargo_recipient.
    WHEN gc_destination.
      gs_service_taker = gs_destination.
  ENDCASE.

*
  CALL FUNCTION 'J_1B_CTE_MAP_CTETYPE_TO_XML'
    EXPORTING
      iv_nftype       = gs_nfdoc-nftype
    IMPORTING
      ev_tpcte        = lv_tpcte
*   ES_J_1BAA       =
* EXCEPTIONS
*   NOT_FOUND       = 1
*   OTHERS          = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  gs_prnfehd-cte_type_xml = lv_tpcte.


* domain texts
  lv_domvalue =  gs_nfdoc-transp_mode.
  PERFORM append_domtext USING lc_transptn_mode
                               lv_domvalue.

  lv_domvalue = gs_nfdoc-cte_serv_taker.
  PERFORM append_domtext USING lc_service_taker
                               lv_domvalue.

  lv_domvalue = gs_nfdoc-serv_tp.
  PERFORM append_domtext USING lc_serv_tp
                               lv_domvalue.

* table texts
  PERFORM append_cfoptext.

  PERFORM append_taxsittext.

  PERFORM append_txjurtext.


ENDFORM.                    "prepare_model57
*&---------------------------------------------------------------------*
*&      Form  COMPLETE_AND_ASSIGN_ADR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM complete_and_assign_adr
              USING    lv_cte_partner TYPE j_1bcte_tomali
              CHANGING cs_adr_pr TYPE j_1bprnfeinnad.


  SELECT SINGLE landx FROM t005t INTO cs_adr_pr-landx
     WHERE spras = nast-spras AND
           land1 = cs_adr_pr-land1.

  CASE lv_cte_partner.
    WHEN gc_goods_sender.
      gs_goods_sender = cs_adr_pr.
    WHEN gc_cargo_dispatcher.
      gs_cargo_dispatcher = cs_adr_pr.
    WHEN gc_cargo_recipient.
      gs_cargo_recipient = cs_adr_pr.
    WHEN gc_destination.
      gs_destination = cs_adr_pr.
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
FORM onetime_to_adr  USING    lv_parvw TYPE j_1bparvw
                     CHANGING ls_adr_pr TYPE j_1bprnfeinnad.

  DATA:
   ls_nfcpd TYPE j_1bnfcpd.

  READ TABLE gt_nfcpd INTO ls_nfcpd
       WITH KEY parvw = lv_parvw.
  CHECK sy-subrc = 0.

  MOVE-CORRESPONDING ls_nfcpd TO ls_adr_pr.

  MOVE ls_nfcpd-stcd1 TO ls_adr_pr-cgc.
  MOVE ls_nfcpd-stcd2 TO ls_adr_pr-cpf.
  MOVE ls_nfcpd-j_1bstains TO ls_adr_pr-stains.
  MOVE ls_nfcpd-j_1bmunins TO ls_adr_pr-munins.


ENDFORM.                    " ONETIME_TO_ADR
