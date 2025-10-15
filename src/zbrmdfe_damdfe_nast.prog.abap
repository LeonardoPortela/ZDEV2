*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print electronic fiscal document                                   *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  zbrmdfe_damdfe_nast MESSAGE-ID 8b.

TABLES: j_1bnfdoc, nast, tnapr.

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
  gv_docnum TYPE j_1bdocnum.

DATA:
  retcode LIKE sy-subrc,           " RETURN CODE INDICATOR
  xscreen.                         " OUTPUT ON PRINTER OR SCREEN

DATA: wg_xml_sefaz TYPE zmdfe_xml_sefaz_auth,
      wg_xml_conti TYPE zmdfe_xml_sefaz,
      wg_mdfe      TYPE zbrmdfe_damdfe.

DATA: gs_ide_dacte       TYPE zde_ide_xml_dacte,
      gs_inf_modal_aquav TYPE zde_inf_modal_aquav_dacte.

CONSTANTS:
    lc_edit_mask(54) TYPE c VALUE
      '____.____.____.____.____.____.____.____.____.____.____'.



INITIALIZATION.
  PERFORM f_teste.

FORM entry USING return_code us_screen.

  DATA: otfdata TYPE tsfotf.

  tnapr-sform = 'ZBRMDFE_DAMDFE_V1'.

  PERFORM imprimir_damdfe USING return_code us_screen
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
  tnapr-sform = 'ZBRMDFE_DAMDFE_V1'.

  PERFORM imprimir_damdfe USING return_code us_screen CHANGING otfdata.

*-#138188-12.04.2024-JT-inicio
  DATA: debug TYPE char1.
  IF debug = abap_on OR p_nast-tdarmod = '9'.
    CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
      EXPORTING
        i_otf                    = otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
  ENDIF.
*-#138188-12.04.2024-JT-fim

ENDFORM.                               " ENTRY

FORM imprimir_damdfe USING return_code
                           us_screen
                 CHANGING otfdata  TYPE tsfotf.

  CLEAR retcode.

  CLEAR return_code.
  xscreen = us_screen.

  PERFORM get_dados_xml_mdfe.

  CHECK wg_xml_sefaz IS NOT INITIAL.

  PERFORM build_info_info_xml.

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

*  XML_STRING = '<?xml version="1.0" encoding="utf-8"?><mdfeProc xmlns="http://www.portalfiscal.inf.br/mdfe" versao="3.00">' &&
*  '<MDFe xmlns="http://www.portalfiscal.inf.br/mdfe"><infMDFe versao="3.00" Id="MDFe11190984590892000380580000000005441443041434"><ide><cUF>11</cUF>' &&
*  '<tpAmb>1</tpAmb><tpEmit>1</tpEmit><tpTransp>1</tpTransp><mod>58</mod><serie>0</serie><nMDF>544</nMDF><cMDF>44304143</cMDF><cDV>4</cDV><modal>3</modal>' &&
*  '<dhEmi>2019-09-30T13:59:35-04:00</dhEmi><tpEmis>1</tpEmis><procEmi>0</procEmi><verProc>001</verProc><UFIni>RO</UFIni><UFFim>AM</UFFim><infMunCarrega>' &&
*  '<cMunCarrega>1100205</cMunCarrega><xMunCarrega>PORTO VELHO</xMunCarrega></infMunCarrega><dhIniViagem>2019-09-30T13:59:35-04:00</dhIniViagem>' &&
*  '<indCanalVerde>1</indCanalVerde></ide><emit><CNPJ>84590892000380</CNPJ><IE>00000000580635</IE><xNome>HERMASA NAVEGACAO DA AMAZONIA LTDA</xNome>' &&
*  '<xFant>PORTO VELHO</xFant><enderEmit><xLgr>RUA TERMINAL DOS MILAGRES</xLgr><nro>400</nro><xBairro>BALCA</xBairro><cMun>1100205</cMun>' &&
*  '<xMun>PORTO VELHO</xMun><CEP>76801370</CEP><UF>RO</UF></enderEmit></emit><infModal versaoModal="3.00"><aquav><irin>PQ3819</irin>' &&
*  '<tpEmb>11</tpEmb><cEmbar>ANDRE MAGG</cEmbar><xEmbar>E/M ANDRE MAGGI</xEmbar><nViag>117</nViag><cPrtEmb>BRPVH</cPrtEmb><cPrtDest>BRITA</cPrtDest>' &&
*  '<infTermCarreg><cTermCarreg>BRPVH001</cTermCarreg><xTermCarreg>TEMINAL FLUVIAL PUBLICO DE PORTO VELHO</xTermCarreg></infTermCarreg><infTermDescarreg>' &&
*  '<cTermDescarreg>BRITA001</cTermDescarreg><xTermDescarreg>TERMINAL HERMASA DA AMAZONIA LTDA</xTermDescarreg></infTermDescarreg></aquav>' &&
*  '</infModal><infDoc><infMunDescarga><cMunDescarga>1301902</cMunDescarga><xMunDescarga>ITACOATIARA</xMunDescarga><infCTe>' &&
*  '<chCTe>11190984590892000380570000000150501599393101</chCTe></infCTe><infCTe><chCTe>11190984590892000380570000000150491755055254</chCTe></infCTe>' &&
*  '</infMunDescarga></infDoc><tot><qCTe>2</qCTe><vCarga>0.00</vCarga><cUnid>01</cUnid><qCarga>0.0000</qCarga></tot></infMDFe><infMDFeSupl>' &&
*  '<qrCodMDFe>HTTPS://DFE-PORTAL.SVRS.RS.GOV.BR/MDFE/QRCODE?chMDFe=11190984590892000380580000000005441443041434&amp;tpAmb=1</qrCodMDFe>' &&
*  '</infMDFeSupl><Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' &&
*  '<SignedInfo><CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>' &&
*  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/><Reference URI="#MDFe11190984590892000380580000000005441443041434">' &&
*  '<Transforms><Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>' &&
*  '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/></Transforms><DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>' &&
*  '<DigestValue>IhMMXpspe3pFCsrc1MUpWDr7bgc=</DigestValue></Reference></SignedInfo><SignatureValue>' &&
*'VJi4FUaq7MaMfOg+AAYLFu9sy4YDzvusrFn+5b5dj1Pg+3d6xfliGlDN1yALGrueZ39CV9EsfBgF' &&
*'ilGBqXyglexDaj/MlEFmM+D2jOIhvSrJY5e7CK+GqmBWZ2ejWU9VS4HTVa20dpq0aWIMx/ADBnp0' &&
*'APYnp1DU+kPB91ZC2rPec33sFu6d/ZTrJymJCjWa0f0rlofjI9PGcjjEcg0nshbAEZ9jOo4rOAuP' &&
*'rrVIep+ZQ+y/Fb11N5Y/Ybjujvh85lunriRaB/8rYzVrvUC6SjUXTLwhsk0OFXDCRNI6ppwftDES' &&
*'fi3XauzD/RqDcMc62gGKfAAA77rbrcTM2Ny4aA==</SignatureValue><KeyInfo><X509Data>' &&
*'<X509Certificate>MIIHozCCBYugAwIBAgIIKMKnZJua0b0wDQYJKoZIhvcNAQELBQAwdTELMAkGA1UEBhMCQlIxEzAR' &&
*'BgNVBAoMCklDUC1CcmFzaWwxNjA0BgNVBAsMLVNlY3JldGFyaWEgZGEgUmVjZWl0YSBGZWRlcmFs' &&
*'IGRvIEJyYXNpbCAtIFJGQjEZMBcGA1UEAwwQQUMgU0VSQVNBIFJGQiB2NTAeFw0xODEwMDMyMDQ0' &&
*'MDBaFw0xOTEwMDMyMDQ0MDBaMIHhMQswCQYDVQQGEwJCUjELMAkGA1UECAwCQU0xDzANBgNVBAcM' &&
*'Bk1BTkFVUzETMBEGA1UECgwKSUNQLUJyYXNpbDE2MDQGA1UECwwtU2VjcmV0YXJpYSBkYSBSZWNl' &&
*'aXRhIEZlZGVyYWwgZG8gQnJhc2lsIC0gUkZCMRYwFAYDVQQLDA1SRkIgZS1DTlBKIEExMRUwEwYD' &&
*'VQQLDAxBUiBDRExDVUlBQkExODA2BgNVBAMML0hFUk1BU0EgTkFWRUdBQ0FPIERBIEFNQVpPTklB' &&
*'IFNBOjg0NTkwODkyMDAwMTE4MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAjk44QJDy' &&
*'tpmEjXe+YDQBjoXET2EzsGYh7Ll92dbikiWdyn9w1naMNXm/6a/mKTx9So2FfiSDqD+9Uy3m4Zzw' &&
*'p2UP2ATNB+/RcIujQQzTviBnYZvKtaxteQqmXCJKX/zuvan8XibW2Y/qrkz6//JbdV7nXwOqT/dc' &&
*'FxOrvc5Bjo4Z+q61IQ8ZqgTS2mGYK7GmbhARip/QHJ/U22HxLJ5DNILbJ7hpvMYJ7osTToMSoEPB' &&
*'ko7QaAQK1goUfIIWTtCacZNI9wb5STVYnYrpZsouusANnCarOGXhgSamu5wKzBbvj7ARpK5sL5Ck' &&
*'SoCQoTGuM1lreQpLAJrEyZxwnaoxmwIDAQABo4ICyDCCAsQwCQYDVR0TBAIwADAfBgNVHSMEGDAW' &&
*'gBTs8UFRV6jmOules6Ai+QiKtTqHjzCBmQYIKwYBBQUHAQEEgYwwgYkwSAYIKwYBBQUHMAKGPGh0' &&
*'dHA6Ly93d3cuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9jYWRlaWFzL3NlcmFzYXJmYnY1LnA3' &&
*'YjA9BggrBgEFBQcwAYYxaHR0cDovL29jc3AuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9zZXJh' &&
*'c2FyZmJ2NTCBtwYDVR0RBIGvMIGsgRtST0dFUlZBTC5ESUFTQEFNQUdHSS5DT00uQlKgHwYFYEwB' &&
*'AwKgFhMUU0VSR0lPIExVSVogUElaWkFUVE+gGQYFYEwBAwOgEBMOODQ1OTA4OTIwMDAxMTigOAYF' &&
*'YEwBAwSgLxMtMjMxMDE5NjAzMzM1MzI1NTkxNTAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwoBcG' &&
*'BWBMAQMHoA4TDDAwMDAwMDAwMDAwMDBxBgNVHSAEajBoMGYGBmBMAQIBDTBcMFoGCCsGAQUFBwIB' &&
*'Fk5odHRwOi8vcHVibGljYWNhby5jZXJ0aWZpY2Fkb2RpZ2l0YWwuY29tLmJyL3JlcG9zaXRvcmlv' &&
*'L2RwYy9kZWNsYXJhY2FvLXJmYi5wZGYwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMEMIGd' &&
*'BgNVHR8EgZUwgZIwSqBIoEaGRGh0dHA6Ly93d3cuY2VydGlmaWNhZG9kaWdpdGFsLmNvbS5ici9y' &&
*'ZXBvc2l0b3Jpby9sY3Ivc2VyYXNhcmZidjUuY3JsMESgQqBAhj5odHRwOi8vbGNyLmNlcnRpZmlj' &&
*'YWRvcy5jb20uYnIvcmVwb3NpdG9yaW8vbGNyL3NlcmFzYXJmYnY1LmNybDAOBgNVHQ8BAf8EBAMC' &&
*'BeAwDQYJKoZIhvcNAQELBQADggIBAKC4wZeELGf/eCZI6dvIWpG49dAxFFtoEr12zoQKxypjuf8p' &&
*'cpvjrP2K6OgpTDVi6HBalsbUkdEZSgkjHhicg0UIWhhkpSFE02r1u9OPZweRdNb3WVCah86sTaEg' &&
*'Ji0GIHxTGRsV0FaQWK6fua49AdLxxdpnSFfOEr1K4UdDSteYxA4etfcD1z3leBcUa9AzPVfYigGe' &&
*'J22cQ3ifTyeEuG7WWCrV5BXFBlcuMO2z77bz0n7V+udtSm40wZLmDYXGT+N2ctVNxjv1h/AONgAa' &&
*'l+IXiu3jkyjYpMICNhkJIzilCjMy/X05Pyve8QGHypvh/TKfO/5oBnSaK4rdGAMWvHr7KTJW5dpS' &&
*'BdSqfrrhq2kWP2Uoes45o4dAC+BtdFYJLbq7bYYqvNS0xhxd4bumEKFcSzMx6kmlNtTVLIeO3Oq1' &&
*'/AMZ3mNMGL2LkuRtQx/zZ07l3lNU055H4FopV4Jz4q6vU5KX/i9D3x6I3cRcWKGzQaF5oJL/sPT4' &&
*'cpp0jT4rqc6vEfemkaoirQRK15f1CIXzqnQROrgaxcHqhLjaCOinlgkhwLSSdUs/78eJQ+ZRBomq' &&
*'QaEU9gt/vnLSlRtHMNMQsA0+x1nT4hBXKP2EWwLVpARgvE/uMnanuYOPhXzLMbWn1yI0xFBze+3F' &&
*'WFMMC/TbGyLPMTh5ULhASxKk8oqv</X509Certificate></X509Data></KeyInfo></Signature>' &&
*'</MDFe><protMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="3.00"><infProt Id="MDFe911190000445195">' &&
*'<tpAmb>1</tpAmb><verAplic>RS20190726112009</verAplic><chMDFe>11190984590892000380580000000005441443041434</chMDFe>' &&
*'<dhRecbto>2019-09-30T18:59:45-03:00</dhRecbto><nProt>911190000445195</nProt><digVal>IhMMXpspe3pFCsrc1MUpWDr7bgc=</digVal>' &&
*'<cStat>100</cStat><xMotivo>Autorizado o uso do MDF-e</xMotivo></infProt></protMDFe></mdfeProc>'.

  xml_string = '<?xml version="1.0" encoding="utf-8"?><mdfeProc xmlns="http://www.portalfiscal.inf.br/mdfe" versao="3.00">'.
  xml_string = xml_string &&  '<MDFe xmlns="http://www.portalfiscal.inf.br/mdfe"><infMDFe versao="3.00" Id="MDFe29190815143827001379580000000000131134143413"><ide>'.
  xml_string = xml_string &&  '<cUF>29</cUF><tpAmb>2</tpAmb><tpEmit>1</tpEmit><tpTransp>2</tpTransp><mod>58</mod><serie>0</serie><nMDF>13</nMDF><cMDF>13414341</cMDF>'.
  xml_string = xml_string &&  '<cDV>3</cDV><modal>3</modal><dhEmi>2019-08-27T15:57:34-03:00</dhEmi><tpEmis>1</tpEmis><procEmi>0</procEmi><verProc>001</verProc>'.
  xml_string = xml_string &&  '<UFIni>BA</UFIni><UFFim>BA</UFFim><infMunCarrega><cMunCarrega>2911105</cMunCarrega><xMunCarrega>Formosa do Rio Preto</xMunCarrega>'.
  xml_string = xml_string &&  '</infMunCarrega><dhIniViagem>2019-08-27T15:57:34-03:00</dhIniViagem><indCanalVerde>1</indCanalVerde></ide><emit><CNPJ>10962697001379</CNPJ>'.
  xml_string = xml_string &&  '<IE>145988023</IE><xNome>AMAGGI LOUIS DREYFUS ZEN-NOH GRAOS</xNome><xFant>TRANSPORTE LEM</xFant><enderEmit>'.
  xml_string = xml_string &&  '<xLgr>Rod BR Duzentos e quarenta e dois</xLgr><nro>S/N</nro><xBairro>Zona Rural</xBairro><cMun>2919553</cMun>'.
  xml_string = xml_string &&  '<xMun>LUIS EDUARDO MAGALHAES</xMun><CEP>47850000</CEP><UF>BA</UF></enderEmit></emit><infModal versaoModal="3.00">'.

  xml_string = xml_string &&  '<aquav><irin>PQ3352</irin><tpEmb>11</tpEmb><cEmbar>SABINO</cEmbar><xEmbar>E/M SABINO PISSOLLO</xEmbar><nViag>116</nViag>'.
  xml_string = xml_string &&  '<cPrtEmb>BRPVH</cPrtEmb><cPrtDest>BRITA</cPrtDest><infTermCarreg><cTermCarreg>BRPVH001</cTermCarreg>'.
  xml_string = xml_string &&  '<xTermCarreg>TEMINAL FLUVIAL PUBLICO DE PORTO VELHO</xTermCarreg></infTermCarreg><infTermDescarreg><cTermDescarreg>BRITA001</cTermDescarreg>'.
  xml_string = xml_string &&  '<xTermDescarreg>TERMINAL HERMASA DA AMAZONIA LTDA</xTermDescarreg></infTermDescarreg></aquav>'.


  "XML_STRING = XML_STRING &&  '<rodo><infANTT><RNTRC>05051050</RNTRC><infCIOT><CIOT>130000000001</CIOT><CNPJ>17367859000136</CNPJ></infCIOT>'.
  "XML_STRING = XML_STRING &&  '<infContratante><CNPJ>10962697000720</CNPJ></infContratante></infANTT><veicTracao><cInt>0000521827</cInt>'.
  "XML_STRING = XML_STRING &&  '<placa>NZS2856</placa><RENAVAM>460126296</RENAVAM><tara>9850</tara><capKG>74000</capKG><capM3>60</capM3>'.
  "XML_STRING = XML_STRING &&  '<prop><CNPJ>17367859000136</CNPJ><RNTRC>46260212</RNTRC><xNome>FERREIRA DIAS TRANSPORTES LTDA</xNome><IE>106060160</IE>'.
  "XML_STRING = XML_STRING &&  '<UF>BA</UF><tpProp>0</tpProp></prop><condutor><xNome>ALISSON FERREIRA DOS REIS</xNome><CPF>06468849560</CPF></condutor>'.
  "XML_STRING = XML_STRING &&  '<tpRod>03</tpRod><tpCar>03</tpCar><UF>BA</UF></veicTracao><veicReboque><cInt>0000521827</cInt><placa>ALL6703</placa>'.
  "XML_STRING = XML_STRING &&  '<RENAVAM>819761753</RENAVAM><tara>9850</tara><capKG>74000</capKG><capM3>60</capM3><prop><CNPJ>17367859000136</CNPJ>'.
  "XML_STRING = XML_STRING &&  '<RNTRC>46260212</RNTRC><xNome>FERREIRA DIAS TRANSPORTES LTDA</xNome><IE>106060160</IE><UF>BA</UF><tpProp>0</tpProp>'.
  "XML_STRING = XML_STRING &&  '</prop><tpCar>03</tpCar><UF>BA</UF></veicReboque></rodo>'.

  xml_string = xml_string &&  '</infModal><infDoc><infMunDescarga><cMunDescarga>2927408</cMunDescarga>'.
  xml_string = xml_string &&  '<xMunDescarga>Salvador</xMunDescarga>'.

  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303837</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303833</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303834</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303835</chCTe></infCTe>'.
  xml_string = xml_string &&  '<infCTe><chCTe>29190810962697001379570000000018011595303836</chCTe></infCTe>'.


  xml_string = xml_string &&  '</infMunDescarga>'.
  xml_string = xml_string &&  '</infDoc><seg><infResp><respSeg>1</respSeg></infResp><infSeg><xSeg>STARR INTERNATIONAL BRASIL SEG</xSeg><CNPJ>17341270000169</CNPJ>'.
  xml_string = xml_string &&  '</infSeg><nApol>P-00315457</nApol><nAver>99999121910962697001379570000000018012XX</nAver></seg><tot><qCTe>1</qCTe>'.
  xml_string = xml_string &&  '<vCarga>46179.00</vCarga><cUnid>01</cUnid><qCarga>51310.0000</qCarga></tot></infMDFe><infMDFeSupl>'.
  xml_string = xml_string &&  '<qrCodMDFe>HTTPS://DFE-PORTAL.SVRS.RS.GOV.BR/MDFE/QRCODE?chMDFe=29190810962697001379580000000000131134143413&amp;tpAmb=2</qrCodMDFe>'.
  xml_string = xml_string &&  '</infMDFeSupl><Signature xmlns="http://www.w3.org/2000/09/xmldsig#"><SignedInfo>'.
  xml_string = xml_string &&  '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>'.
  xml_string = xml_string &&  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>'.
  xml_string = xml_string &&  '<Reference URI="#MDFe29190810962697001379580000000000131134143413"><Transforms><Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>'.
  xml_string = xml_string &&  '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/></Transforms>'.
  xml_string = xml_string &&  '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/><DigestValue>gsTsy04CSy4jd/vlsPUarIxmVsY=</DigestValue></Reference>'.
  xml_string = xml_string &&  '</SignedInfo>'.
  xml_string = xml_string &&  '<SignatureValue>YzE3pQ84qAAdQDDkOGVeCixVOYtnyM6lwM+OCOtDYbtfZOZoTpIfI++VVNwaqiuNtUXQsgEkkx+v'.
  xml_string = xml_string &&  'gBfuP5xf4zyQjZaexWIBcpBU54monsI1SQUJuqzjNsa183p/bTpJ6wCICrGGc+GckfKhnX0huY+b'.
  xml_string = xml_string &&  '27gGRWnqSTWeheGgvGIy7+PzYOekYHfrd8Awb0szpTC1OAcyaCdUGmUPFaGCV9A+36qrn2K41i1y'.
  xml_string = xml_string &&  'CIqY0FT+0JaK24djuhbLgGwCI1GhijmtIXx13AnWPEWCfIBosxdPINadUXrzngcVoXPQ0dRRKBYv'.
  xml_string = xml_string &&  'vqFZ2ZLFfdAxS7IVSwwtOFGSuXCZf5EbfPjPEQ==</SignatureValue><KeyInfo><X509Data>'.
  xml_string = xml_string &&  '<X509Certificate>MIIH5TCCBc2gAwIBAgIIOsZkRywbuq8wDQYJKoZIhvcNAQELBQAwdjELMAkGA1UEBhMCQlIxEzAR'.
  xml_string = xml_string &&  'BgNVBAoTCklDUC1CcmFzaWwxNTAzBgNVBAsTLEF1dG9yaWRhZGUgQ2VydGlmaWNhZG9yYSBWQUxJ'.
  xml_string = xml_string &&  'RCAtIEFDIFZBTElEIHY1MRswGQYDVQQDExJBQyBWQUxJRCBCUkFTSUwgdjUwHhcNMTkwODIxMjE1'.
  xml_string = xml_string &&  'NzQyWhcNMjAwODIwMjE1NzQyWjCB4TELMAkGA1UEBhMCQlIxCzAJBgNVBAgTAkJBMR8wHQYDVQQH'.
  xml_string = xml_string &&  'ExZMVUlTIEVEVUFSRE8gTUFHQUxIQUVTMRMwEQYDVQQKEwpJQ1AtQnJhc2lsMRgwFgYDVQQLEw9B'.
  xml_string = xml_string &&  'QyBWQUxJRCBCUkFTSUwxGzAZBgNVBAsTElBlc3NvYSBKdXJpZGljYSBBMTEOMAwGA1UECxMFVkFM'.
  xml_string = xml_string &&  'SUQxFzAVBgNVBAsTDjE2NDY0NzU1MDAwMTg3MS8wLQYDVQQDEyZBTUFHR0kgTE9VSVMgRFJFWUZV'.
  xml_string = xml_string &&  'UyBaRU4tTk9IIEdSQU9TIFMgQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAOw4LvHB'.
  xml_string = xml_string &&  'YaRlm2tBVem8z6+C1vBAC5bcXZSD0qAkZl8bmDPPwnBG3us+DjnItOus8pActEEsq7Nf3HvvdOfK'.
  xml_string = xml_string &&  '2S/3fqPXC9+T9nS6ynFLCXlwMqjPhzgKNq2K3y5mG/OuyPXEKMOVGf5+Q/AyDB7RZJEl8ktbys5L'.
  xml_string = xml_string &&  '/UOScF8SINIDhgww+2EmsladLQP5y8Yl7G4j1BsGE6T7WhbTlBiY3QbaP1MvaGEG4tik3biYDeAV'.
  xml_string = xml_string &&  '9AnTh+z6BDauIbf+DpCPJtM2pGz0hS739p6giH1Pby2JvgMn1hxk5iLLlIAzxT17SE6N8yRaNbba'.
  xml_string = xml_string &&  'oVkYSZcItux5rqIiYDKQGcp8jvC188UCAwEAAaOCAwkwggMFMIGiBggrBgEFBQcBAQSBlTCBkjBb'.
  xml_string = xml_string &&  'BggrBgEFBQcwAoZPaHR0cDovL2ljcC1icmFzaWwudmFsaWRjZXJ0aWZpY2Fkb3JhLmNvbS5ici9h'.
  xml_string = xml_string &&  'Yy12YWxpZGJyYXNpbC9hYy12YWxpZGJyYXNpbHY1LnA3YjAzBggrBgEFBQcwAYYnaHR0cDovL29j'.
  xml_string = xml_string &&  'c3B2NS52YWxpZGNlcnRpZmljYWRvcmEuY29tLmJyMAkGA1UdEwQCMAAwHwYDVR0jBBgwFoAUB99X'.
  xml_string = xml_string &&  'oxOYQwhc+eJMDhv7HngutokwdgYDVR0gBG8wbTBrBgZgTAECASQwYTBfBggrBgEFBQcCARZTaHR0'.
  xml_string = xml_string &&  'cDovL2ljcC1icmFzaWwudmFsaWRjZXJ0aWZpY2Fkb3JhLmNvbS5ici9hYy12YWxpZGJyYXNpbC9k'.
  xml_string = xml_string &&  'cGMtYWMtdmFsaWRicmFzaWx2NS5wZGYwgcIGA1UdHwSBujCBtzBZoFegVYZTaHR0cDovL2ljcC1i'.
  xml_string = xml_string &&  'cmFzaWwudmFsaWRjZXJ0aWZpY2Fkb3JhLmNvbS5ici9hYy12YWxpZGJyYXNpbC9sY3ItYWMtdmFs'.
  xml_string = xml_string &&  'aWRicmFzaWx2NS5jcmwwWqBYoFaGVGh0dHA6Ly9pY3AtYnJhc2lsMi52YWxpZGNlcnRpZmljYWRv'.
  xml_string = xml_string &&  'cmEuY29tLmJyL2FjLXZhbGlkYnJhc2lsL2xjci1hYy12YWxpZGJyYXNpbHY1LmNybDAOBgNVHQ8B'.
  xml_string = xml_string &&  'Af8EBAMCBeAwHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMEMIHFBgNVHREEgb0wgbqBF2dp'.
  xml_string = xml_string &&  'c2xhaW5lLmNydXpAbGRjb20uY29toDgGBWBMAQMEoC8ELTI4MDYxOTgwMDMzOTI4MzE0OTIwMDAw'.
  xml_string = xml_string &&  'MDAwMDAwMDAwMDAwMDAwMDAwMDAwMKAxBgVgTAEDAqAoBCZNQVVSSUNJTyBIQVJETUFOIFRBVkFS'.
  xml_string = xml_string &&  'RVMgREUgTUVMTyBGSUxIT6AZBgVgTAEDA6AQBA4xMDk2MjY5NzAwMDEzNaAXBgVgTAEDB6AOBAww'.
  xml_string = xml_string &&  'MDAwMDAwMDAwMDAwDQYJKoZIhvcNAQELBQADggIBAIjnXlpoiBnTt/pUWAgUehzSdlsgkwUHhSzo'.
  xml_string = xml_string &&  '5gbq44mZdXNW0SK6pyYWWocMG5FJwekSp96OFjhwoNEoYO/+kL1/pMODVyqaYC9PBuT0zjsvRUsS'.
  xml_string = xml_string &&  '1OExJTPElnaKWhtKQC8KlAUY66PRjTu99+QEZ5x1koBxFxv/gFWGlPz0aHFsgZOLQwe1j4oLE8Fe'.
  xml_string = xml_string &&  '9ckidVtrBwyYq4+hJycv3rhSje/Qpp8fnm8EPLqIvpukGGcmi3pDG9G6aTJghsHcjUo7KQYjZCTH'.
  xml_string = xml_string &&  'XHliman8HIhRoyqMTPSRHsp04L4xCBvRBsI8sa88/To39cx2xKttjMIf4X89Llb0Mzyn5+LdWhZD'.
  xml_string = xml_string &&  'akpNYZg1AxfiWSmrVbqmAQLw1lH3JFrWpnxqhXDUP/sYanuWRw9bqBKVsBNElIBjq6+94sX6WYn/'.
  xml_string = xml_string &&  'B2igx2J10y4RfMOltaV364v8dr/YKnxZoqIKaJ4lYFcuFz++B7x1OJLfXQ5QdsnnQ5Y3bbznf2rc'.
  xml_string = xml_string &&  'XZzwsu08K0Mqr2CvD4MVEctTClYO0ds0ewyTNP37FVJDQtx973z1vDj0rdl68BU6qy+aE09yKjXV'.
  xml_string = xml_string &&  'adQhoEvSzaAtxAsrrCpkZs6ftyr0yDklHFlwl9gOZMKJ3ptq6YzoZcf1E19KsG2csHS3LzXJ6iOo'.
  xml_string = xml_string &&  '7gphH4KIoG3Ofo64vWUmDKxny1WMwLjc6F550Fjt</X509Certificate></X509Data></KeyInfo></Signature></MDFe>'.
  xml_string = xml_string &&  '<protMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="3.00"><infProt Id="MDFe929190000005008">'.
  xml_string = xml_string &&  '<tpAmb>2</tpAmb><verAplic>RS20190726112021</verAplic><chMDFe>29190810962697001379580000000000131134143413</chMDFe>'.
  xml_string = xml_string &&  '<dhRecbto>2019-08-27T19:57:42-03:00</dhRecbto><nProt>929190000005008</nProt>'.
  xml_string = xml_string &&  '<digVal>gsTsy04CSy4jd/vlsPUarIxmVsY=</digVal><cStat>100</cStat><xMotivo>Autorizado o uso do MDF-e</xMotivo>'.
  xml_string = xml_string &&  '</infProt></protMDFe></mdfeProc>'.

  APPEND 'infNFe'         TO t_element_array.
  APPEND 'infCTe'         TO t_element_array.
  "APPEND 'infMunCarrega'  TO t_element_array.

  DATA(_json) = zcl_string=>xml_to_json( i_xml           = xml_string
                                         i_element_array = t_element_array ).

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = wg_xml_sefaz.

ENDFORM.

FORM build_info_info_xml.

  DATA: wl_doc_origin  TYPE zde_docs_origin_cte,
        v_obs_cont_tmp TYPE string,
        v_chave_aux    TYPE c LENGTH 100,
        t_reboque      TYPE zde_inf_mdfe_md_rodo_veic_rebt,  "*-CS2022000953-31.01.2023-#93900-JT
        w_reboque      TYPE zde_inf_mdfe_md_rodo_veic_rebo.  "*-CS2022000953-31.01.2023-#93900-JT

  FREE: wg_mdfe.
  CLEAR: gt_docs_origin[], gs_observacoes, gs_obscont.

*------------------------------------------------------------------------------------------*
* Dados Identificação
*------------------------------------------------------------------------------------------*


  wg_mdfe-sadr-name1 = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-xnome.
  wg_mdfe-sadr-stras = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-xlgr.
  wg_mdfe-sadr-strs2 = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-xcpl.
  wg_mdfe-sadr-hausn = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-nro.
  wg_mdfe-sadr-ort02 = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-xbairro.
  wg_mdfe-sadr-pstlz = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-cep.
  wg_mdfe-sadr-ort01 = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-xmun.
  wg_mdfe-sadr-telf1 = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-fone.
  wg_mdfe-sadr-regio = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-enderemit-uf.


* WRITE wg_xml_sefaz-mdfeproc-mdfe-infmdfe-a_id+4(44)
  WRITE wg_xml_sefaz-mdfeproc-mdfe-infmdfe-id+4(44)   "*-#138188-12.04.2024-JT-inicio
    TO wg_mdfe-chave_mask
    USING EDIT MASK lc_edit_mask.

* wg_mdfe-chave      = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-a_id+4(44).  ""*-#138188-12.04.2024-JT-inicio
  wg_mdfe-chave      = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-id+4(44).
  wg_mdfe-cnpj       = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-cnpj.
  wg_mdfe-modelo     = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-mod.
  wg_mdfe-serie      = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-serie.
  wg_mdfe-numero     = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-nmdf.
  wg_mdfe-docnum     = gv_docnum.
  wg_mdfe-modal      = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-modal.

  wg_mdfe-ie         = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-emit-ie.

  PERFORM zf_formata_cgc USING wg_mdfe-cnpj CHANGING wg_mdfe-cnpj.

  CASE wg_mdfe-modal.
    WHEN '1'.
      wg_mdfe-ds_modal = 'Rodoviário'.
    WHEN '3'.
      wg_mdfe-ds_modal = 'Aquaviário'.
  ENDCASE.

  wg_mdfe-peso_total     = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-tot-qcarga.
  wg_mdfe-valor_carga    = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-tot-vcarga.

  WRITE wg_mdfe-peso_total TO wg_mdfe-peso_total_txt
    DECIMALS 3
    LEFT-JUSTIFIED
    NO-GAP.

  WRITE wg_mdfe-valor_carga TO wg_mdfe-valor_carga_txt
    DECIMALS 2
    LEFT-JUSTIFIED
    NO-GAP.

  wg_mdfe-protocolo     = wg_xml_sefaz-mdfeproc-protmdfe-infprot-nprot. "PROTOCOLO DE AUTORIZAÇÃO

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-dhemi
                           CHANGING wg_mdfe-dtemi
                                    wg_mdfe-hremi.

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-mdfeproc-protmdfe-infprot-dhrecbto
                           CHANGING wg_mdfe-data_aut
                                    wg_mdfe-hora_aut.

  wg_mdfe-qrcodmdfe          = wg_xml_sefaz-mdfeproc-mdfe-infmdfesupl-qrcodmdfe.
  wg_mdfe-qtde_cte           = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-tot-qcte.
  wg_mdfe-qtde_nfe           = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-tot-qnfe.
  wg_mdfe-uf_carregamento    = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-ufini.
  wg_mdfe-uf_descarregamento = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-ide-uffim.


*------------------------------------------------------------------------------------------*
* Inf. Modal Aquav.
*------------------------------------------------------------------------------------------*

*------------------------------------------------------------------------------------------*
* Inf. Modal Rodoviario.
*------------------------------------------------------------------------------------------*

  wg_mdfe-inf_modal_rodo-ciot             = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-infantt-infciot-ciot.
  wg_mdfe-inf_modal_rodo-rntrc            = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-infantt-rntrc.

  wg_mdfe-inf_modal_rodo-motorista_cpf     = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-veictracao-condutor-cpf.
  wg_mdfe-inf_modal_rodo-motorista_nome    = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-veictracao-condutor-xnome.
  wg_mdfe-inf_modal_rodo-placa             = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-veictracao-placa.

*-CS2022000953-31.01.2023-#93900-JT-inicio
  t_reboque[] = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-rodo-veicreboque[].

  LOOP AT t_reboque INTO w_reboque.
    wg_mdfe-inf_modal_rodo-placa = | { wg_mdfe-inf_modal_rodo-placa } { '/' } { w_reboque-placa } |.
  ENDLOOP.

  CONDENSE wg_mdfe-inf_modal_rodo-placa.
*-CS2022000953-31.01.2023-#93900-JT-fim

  wg_mdfe-inf_modal_aquav-term_car_codigo  = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-aquav-inftermcarreg-ctermcarreg.
  wg_mdfe-inf_modal_aquav-term_car_nome    = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-aquav-inftermcarreg-xtermcarreg.
  wg_mdfe-inf_modal_aquav-term_desc_codigo = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-aquav-inftermdescarreg-ctermdescarreg.
  wg_mdfe-inf_modal_aquav-term_desc_nome   = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infmodal-aquav-inftermdescarreg-xtermdescarreg.

*------------------------------------------------------------------------------------------*
* Informações Composição Carga
*------------------------------------------------------------------------------------------*
  LOOP AT wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infdoc-infmundescarga-infcte INTO DATA(wl_inf_cte).

    WRITE wl_inf_cte-chcte
       TO v_chave_aux
      USING EDIT MASK lc_edit_mask.

    APPEND VALUE #( chave = v_chave_aux ) TO wg_mdfe-docs_mdfe.

  ENDLOOP.

  LOOP AT wg_xml_sefaz-mdfeproc-mdfe-infmdfe-infdoc-infmundescarga-infnfe INTO DATA(wl_inf_nfe).

    WRITE wl_inf_nfe-chnfe
       TO v_chave_aux
      USING EDIT MASK lc_edit_mask.

    APPEND VALUE #( chave = v_chave_aux ) TO wg_mdfe-docs_mdfe.
  ENDLOOP.

*------------------------------------------------------------------------------------------*
* Informações Seguro
*------------------------------------------------------------------------------------------*

  wg_mdfe-inf_seguro_carga-apolice         = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-seg-napol.
  wg_mdfe-inf_seguro_carga-cnpj_seguradora = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-seg-infseg-cnpj.
  wg_mdfe-inf_seguro_carga-nome_seguradora = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-seg-infseg-xseg.
  wg_mdfe-inf_seguro_carga-nr_averbacao    = wg_xml_sefaz-mdfeproc-mdfe-infmdfe-seg-naver.

  CASE wg_xml_sefaz-mdfeproc-mdfe-infmdfe-seg-infresp-respseg.
    WHEN '1'.
      wg_mdfe-inf_seguro_carga-responsavel = 'Emissor'.
    WHEN '2'.
      wg_mdfe-inf_seguro_carga-responsavel = 'Contratante'.
  ENDCASE.

ENDFORM.


FORM f_teste.

  DATA: otfdata1      TYPE tsfotf,
        vqtde         TYPE j_1bnflin-menge,
        wl_doc_origin TYPE zde_docs_origin_cte.

  tnapr-sform = 'ZBRMDFE_DAMDFE_V1'.

  PERFORM get_dados_xml_tmp.

  PERFORM build_info_info_xml.

  "WG_MDFE-QRCODMDFE = 'HTTPS://DFE-PORTAL.SVRS.RS.GOV.BR/MDFE/QRCODE?chMDFe=29190810962697001379580000000000131134143413&tpAmb=2'.
  "WG_MDFE-MODAL = '03'.
  "WG_MDFE-SADR-TELF1 = '6699612533'.

  PERFORM printing CHANGING otfdata1.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_otf                    = otfdata1
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFORM.


FORM printing CHANGING otfdata TYPE tsfotf.

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

  output_options-tdnewid  = 'X'.
  output_options-tddest   = nast-ldest.
  output_options-tdimmed  = nast-dimme.                     "1897281
  output_options-tddelete = 'X'.                            "1897281

  CLEAR: control_parameters.

  control_parameters-no_dialog = 'X'.
  control_parameters-device    = 'PRINTER'.
  control_parameters-preview   = ' '.
  control_parameters-getotf    = 'X'.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = control_parameters
      output_options     = output_options
      user_settings      = ''
      i_dados_mdfe       = wg_mdfe
    IMPORTING
      job_output_info    = t_job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc = 0.
    MOVE t_job_output_info-otfdata TO otfdata.
  ENDIF.



ENDFORM.                    "printing


FORM get_dados_xml_mdfe.

  DATA: t_element_array TYPE zde_element_array_t,
*-#138188-12.04.2024-JT-inicio
        lo_parser       TYPE REF TO if_edoc_source_parser,
        lo_params       TYPE REF TO cl_edoc_br_create_entity_param,
        md_xml          TYPE REF TO data,
        l_xml           TYPE xstring.

  FIELD-SYMBOLS: <ls_mdfe> TYPE any.
*-#138188-12.04.2024-JT-fim

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  DATA: ls_acckey             TYPE j_1b_nfe_access_key,
        ls_active             TYPE j_1bnfe_active,
        lv_rfcdest            TYPE rfcdest,
        lo_nfse               TYPE REF TO if_j_1bnfse,
        lo_download_cloud     TYPE REF TO cl_nfe_cloud_download, "2932848
        lo_log_error          TYPE REF TO cl_j_1bnfe_error_log,
        lv_is_valid_for_cloud TYPE abap_bool,                         "2932848 "3039634
        lv_cloud_uuid         TYPE nfe_document_uuid,       "2932848
        lv_access_key         TYPE j_1b_nfe_access_key_dtel44,
        lv_direction          TYPE j_1b_nfe_direction,
        lob_mdfe_processor    TYPE REF TO cl_nfe_cloud_mdfe_processor,
        lv_xstring_content    TYPE xstring,                               "*#127333 - 04.12.2023 - JT
        mo_local_file         TYPE REF TO cl_nfe_local_file,
        mo_nfe_convert_wrap   TYPE REF TO if_nfe_convert_wrap,
        lv_path               TYPE string,
        w_result              TYPE bapiret2,
        t_result              TYPE bapirettab,
        lv_length             TYPE i,
        t_binary              TYPE STANDARD TABLE OF x255.

  CONSTANTS lc_model_nfe  TYPE j_1b_nfe_doctype VALUE 'NFE'.
  CONSTANTS lc_model_cte  TYPE j_1b_nfe_doctype VALUE 'CTE'.
  CONSTANTS lc_direct_in  TYPE j_1b_nfe_direction VALUE 'INBD'.
  CONSTANTS lc_direct_out TYPE j_1b_nfe_direction VALUE 'OUTB'.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  MOVE nast-objky TO gv_docnum.

  CHECK gv_docnum IS NOT INITIAL.

  FREE: wg_xml_sefaz, wg_xml_conti.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  SELECT SINGLE contingencia
    INTO @DATA(lv_contingencia)
    FROM zsdt0102
   WHERE docnum = @gv_docnum.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  IF lv_contingencia = abap_false.
    TRY .
        zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = gv_docnum
          )->set_registro( EXPORTING i_docnum       = gv_docnum
                                     i_sem_bloqueio = abap_true
          )->get_ck_autorizado_uso(
          )->get_xml_grc( IMPORTING e_xml_string = DATA(_xml_doc) ).
      CATCH zcx_doc_eletronico INTO DATA(ex_doc).    " .
        ex_doc->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.
  ELSE.
    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO ls_active
     WHERE docnum = gv_docnum.

    MOVE-CORRESPONDING ls_active TO ls_acckey.

    PERFORM get_rfc_destination(z_1bnfe_monitor)  USING ls_active
                                               CHANGING lv_rfcdest.
* Service Nota Fiscal (NFS-e)                                                                                         "2520709
    lo_nfse = cl_j_1bnfse=>get_instance( ).
    IF lo_nfse->is_service_notafiscal( iv_document_number = ls_active-docnum ) = abap_true. "2520709.
      lv_rfcdest = if_j_1bnfse=>mc_nfse_downloadxml_key.    "2520709
      lv_access_key = ls_active-rps.                        "3001273
    ENDIF.                                                  "2520709

    TRY.
        CREATE OBJECT lob_mdfe_processor.
        DATA(lwa_document_info) = lob_mdfe_processor->get_file(
          EXPORTING
            iv_environment_type = CONV #( ls_active-tpamb )
            iv_action           = 'AUTHORIZE'
            iv_uuid             = CONV #( ls_active-cloud_guid ) ).
      CATCH cx_nfe_cloud_download_error INTO DATA(ex_cloud).
        RETURN.
    ENDTRY.

    IF lwa_document_info-file_content IS NOT INITIAL.
      CREATE OBJECT mo_nfe_convert_wrap TYPE cl_nfe_convert_wrap.
      lv_xstring_content  = mo_nfe_convert_wrap->base64_to_xstring( lwa_document_info-file_content ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xstring_content
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = t_binary.

      CALL FUNCTION 'SCMS_BINARY_TO_STRING'
        EXPORTING
          input_length = lv_length
        IMPORTING
          text_buffer  = _xml_doc
        TABLES
          binary_tab   = t_binary
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
    ENDIF.
  ENDIF.

  CHECK _xml_doc IS NOT INITIAL.

*-#138188-12.04.2024-JT-inicio
*------------------------------------------------------------------
*-alterado metodo de parse do XML, pq a TAG REBOQUE no XML
*-nao estava sendo aberta corretamente, quando havia apenas 1 linhas
*------------------------------------------------------------------
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = _xml_doc
    IMPORTING
      buffer = l_xml
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  TRY.
      CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
      CREATE OBJECT lo_params
        EXPORTING
          iv_xml = l_xml.

      lo_parser->load_source( lo_params->get_xml( ) ).

      IF lv_contingencia = abap_false.
        md_xml   = lo_parser->parse_to_ddic( wg_xml_sefaz ).
        ASSIGN md_xml->*                 TO <ls_mdfe>.
        wg_xml_sefaz                      = <ls_mdfe>.
      ELSE.
        md_xml   = lo_parser->parse_to_ddic( wg_xml_conti ).
        ASSIGN md_xml->*                 TO <ls_mdfe>.
        wg_xml_conti                      = <ls_mdfe>.
        MOVE-CORRESPONDING wg_xml_conti  TO wg_xml_sefaz-mdfeproc.
      ENDIF.
    CATCH cx_edocument.
      EXIT.
  ENDTRY.

  FREE: _xml_doc, l_xml.
*-#138188-12.04.2024-JT-fim

*-#138188-12.04.2024-JT-inicio
*  APPEND 'infNFe'  TO t_element_array.
*  APPEND 'infCTe'  TO t_element_array.
*  APPEND 'ObsCont' TO t_element_array.
*
*  DATA(_json) = zcl_string=>xml_to_json( i_xml           = _xml_doc
*                                         i_element_array =  t_element_array ).
*
*  CALL METHOD /ui2/cl_json=>deserialize
*    EXPORTING
*      json = _json
*    CHANGING
*      data = wg_xml_sefaz.
*-#138188-12.04.2024-JT-fim

ENDFORM.


FORM f_get_data_hora_utc  USING p_data_hora
                       CHANGING c_data
                                c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  PERFORM f_get_data_utc USING p_data_hora
                      CHANGING c_data.

  PERFORM f_get_hora_utc USING p_data_hora
                      CHANGING c_hora.

ENDFORM.


FORM f_get_data_utc USING p_data_hora
                 CHANGING c_data.

  CHECK p_data_hora IS NOT INITIAL.

  c_data = p_data_hora(4) &&
           p_data_hora+05(02) &&
           p_data_hora+08(02).

ENDFORM.

FORM f_get_hora_utc USING p_data_hora
                 CHANGING c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  c_hora  = p_data_hora+11(02) &&
            p_data_hora+14(02) &&
            p_data_hora+17(02).

ENDFORM.

FORM zf_formata_cgc  USING    p_cgc
                     CHANGING p_cgc_format.


  DATA lc_cgc_aux TYPE pbr99_cgc.

  "CLEAR p_cgc_format.

  CHECK NOT p_cgc IS INITIAL.

  CHECK p_cgc > 0.

  lc_cgc_aux = p_cgc.

  CALL FUNCTION 'HR_BR_CHECK_CGC_FORMAT'
    EXPORTING
      cgc_number               = lc_cgc_aux
    IMPORTING
      cgc_number_formatted     = lc_cgc_aux
    EXCEPTIONS
      cgc_format_not_supported = 1
      cgc_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cgc_aux TO p_cgc_format.

ENDFORM.                    " ZF_FORMATA_CGC


*&---------------------------------------------------------------------*
*&      Form  zf_formata_cpf
*&---------------------------------------------------------------------*
* Formata CPF
*----------------------------------------------------------------------*
FORM zf_formata_cpf USING    p_cpf
                    CHANGING p_cpf_format.

  DATA lc_cpf_aux TYPE pbr99_cpf.

  CLEAR p_cpf_format.

  CHECK NOT p_cpf IS INITIAL.

  lc_cpf_aux = p_cpf.

  CALL FUNCTION 'HR_BR_CHECK_CPF_FORMAT'
    EXPORTING
      cpf_number               = lc_cpf_aux
    IMPORTING
      cpf_number_formatted     = lc_cpf_aux
    EXCEPTIONS
      cpf_format_not_supported = 1
      cpf_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cpf_aux TO p_cpf_format.

ENDFORM.                    "zf_formata_cpf
