*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print electronic fiscal document                                   *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  zbrcte_dacte_nast_copy MESSAGE-ID 8b.

TABLES: j_1bnfdoc,
        nast,                          "Messages
        *nast,                         "Messages
        tnapr,                         "Programs & Forms
        itcpo,                         "Communicationarea for Spool
        arc_params,                    "Archive parameters
        toa_dara,                      "Archive parameters
        addr_key.                      "Adressnumber for ADDRESS

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
  gs_service_taker    TYPE j_1bprnfeinnad,
  gs_componentes      TYPE zde_docs_componentes.

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
      gs_inf_modal_aquav TYPE zde_inf_modal_aquav_dacte,
      wg_dacte           TYPE zbrcte_dacte.

CONSTANTS:
  lc_edit_mask(54)      TYPE c VALUE '____.____.____.____.____.____.____.____.____.____.____',
  lc_edit_mask_cnpj(20) TYPE c VALUE '__.___.___/____-__',
  lc_edit_mask_cep(9)   TYPE c VALUE '_____-___',
  lc_edit_mask_cpf(20)  TYPE c VALUE '___.___.___-__',
  lc_edit_mask_fone(20) TYPE c VALUE '__ ____ ____',
  lc_service_taker      TYPE domname VALUE 'J_1BCTE_SERVICE_TAKER'.



INITIALIZATION.

  PERFORM f_teste.

FORM entry USING return_code us_screen.

  DATA: otfdata TYPE tsfotf.

  PERFORM imprimir_dacte USING space return_code us_screen
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
  tnapr-sform = 'ZBRCTE_DACTE'.

  PERFORM imprimir_dacte USING space return_code us_screen CHANGING otfdata.

ENDFORM.                               " ENTRY

FORM entry3 USING xml_doc TYPE string
                  return_code
                  us_screen
         CHANGING otfdata TYPE tsfotf.

  CLEAR: nast.
  tnapr-sform = 'ZBRCTE_DACTE'.

  PERFORM imprimir_dacte USING xml_doc return_code us_screen CHANGING otfdata.

ENDFORM.                               " ENTRY

FORM imprimir_dacte USING p_xml_doc TYPE string
                          return_code
                          us_screen
                 CHANGING otfdata  TYPE tsfotf.

  CLEAR retcode.

  CLEAR return_code.
  xscreen = us_screen.

  PERFORM get_dados_xml_cte USING p_xml_doc.

  CHECK wg_xml_sefaz IS NOT INITIAL.

  PERFORM build_info_info_xml.

  "PERFORM prepare_model57.

  PERFORM printing CHANGING otfdata.

ENDFORM.

FORM get_dados_xml_tmp.


  DATA xml_string TYPE string.

  DATA: t_element_array TYPE zde_element_array_t.

  xml_string = '<?xml version="1.0" encoding="utf-8"?>'.
  xml_string = xml_string &&  '<cteProc xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00"> '.
  xml_string = xml_string &&  '<CTe xmlns="http://www.portalfiscal.inf.br/cte"> '.
  xml_string = xml_string &&  '<infCte versao="3.00" Id="CTe41220177294254002057570000000322281113955167"> '.
  xml_string = xml_string &&  '<ide> '.
  xml_string = xml_string &&  '<cUF> '.
  xml_string = xml_string &&  '17</cUF>'.
  xml_string = xml_string &&  '<cCT> '.
  xml_string = xml_string &&  '34094480</cCT> '.
  xml_string = xml_string &&  '<CFOP> '.
  xml_string = xml_string &&  '6353</CFOP> '.
  xml_string = xml_string &&  '<natOp> '.
  xml_string = xml_string &&  'Prestação serv. transp. estab. comercial</natOp> '.
  xml_string = xml_string &&  '<mod> '.
  xml_string = xml_string &&  '57</mod>'.
  xml_string = xml_string &&  '<serie> '.
  xml_string = xml_string &&  '0</serie> '.
  xml_string = xml_string &&  '<nCT> '.
  xml_string = xml_string &&  '4870</nCT> '.
  xml_string = xml_string &&  '<dhEmi> '.
  xml_string = xml_string &&  '2021-02-18T09:55:36-03:00</dhEmi> '.
  xml_string = xml_string &&  '<tpImp> '.
  xml_string = xml_string &&  '1</tpImp> '.
  xml_string = xml_string &&  '<tpEmis> '.
  xml_string = xml_string &&  '1</tpEmis> '.
  xml_string = xml_string &&  '<cDV> '.
  xml_string = xml_string &&  '1</cDV> '.
  xml_string = xml_string &&  '<tpAmb> '.
  xml_string = xml_string &&  '1</tpAmb> '.
  xml_string = xml_string &&  '<tpCTe> '.
  xml_string = xml_string &&  '0</tpCTe> '.
  xml_string = xml_string &&  '<procEmi> '.
  xml_string = xml_string &&  '0</procEmi> '.
  xml_string = xml_string &&  '<verProc> '.
  xml_string = xml_string &&  '008</verProc> '.
  xml_string = xml_string &&  '<cMunEnv> '.
  xml_string = xml_string &&  '1705508</cMunEnv> '.
  xml_string = xml_string &&  '<xMunEnv> '.
  xml_string = xml_string &&  'COLINAS DO TOCANTINS</xMunEnv> '.
  xml_string = xml_string &&  '<UFEnv> '.
  xml_string = xml_string &&  'TO</UFEnv>'.
  xml_string = xml_string &&  '<modal> '.
  xml_string = xml_string &&  '01</modal>'.
  xml_string = xml_string &&  '<tpServ> '.
  xml_string = xml_string &&  '0</tpServ> '.
  xml_string = xml_string &&  '<cMunIni> '.
  xml_string = xml_string &&  '1707652</cMunIni> '.
  xml_string = xml_string &&  '<xMunIni> '.
  xml_string = xml_string &&  'FIGUEIROPOLIS</xMunIni> '.
  xml_string = xml_string &&  '<UFIni> '.
  xml_string = xml_string &&  'TO</UFIni> '.
  xml_string = xml_string &&  '<cMunFim> '.
  xml_string = xml_string &&  '2111300</cMunFim> '.
  xml_string = xml_string &&  '<xMunFim> '.
  xml_string = xml_string &&  'SAO LUIS</xMunFim> '.
  xml_string = xml_string &&  '<UFFim> '.
  xml_string = xml_string &&  'MA</UFFim>'.
  xml_string = xml_string &&  '<retira> '.
  xml_string = xml_string &&  '1</retira> '.
  xml_string = xml_string &&  '<indIEToma> '.
  xml_string = xml_string &&  '1</indIEToma> '.
  xml_string = xml_string &&  '<toma3> '.
  xml_string = xml_string &&  '<toma> '.
  xml_string = xml_string &&  '0</toma> '.
  xml_string = xml_string &&  '</toma3> '.
  xml_string = xml_string &&  '</ide> '.
  xml_string = xml_string &&  '<compl> '.
  xml_string = xml_string &&  '<xObs> '.
  xml_string = xml_string &&  'Numero do Transporte: 0003039368 Numero do Faturamento: 0094723197 Subcontratado: HU - '.
  xml_string = xml_string &&  'TRANSPORTE RODOVIARIO LTDA AVENIDA RIO GRANDE DO NORTE - Nr: 1859 Bairro: Centro - Muni: Paranavai - PR '.
  xml_string = xml_string &&  'CEP: 87705-010 RNTRC: 00031974 00.124.733/0001-38 IE-9011316783 Local de entrega: Transbordo: AMAGGI '.
  xml_string = xml_string &&  'LOUIS DREYFUS ZEN NOH TERM 15.143.827/0002-02 IE-124079172 Placa Cavalo-BDY0I56-Maringa /PR-HU - '.
  xml_string = xml_string &&  'TRANSPORTE RODOVIARIO LTDA-CNPJ-00124733000138 Placa Carreta 1-AWW5382-Maringa /PR-TRANSPORTADORA '.
  xml_string = xml_string &&  'EQUADOR LTDA ME-CNPJ-04492373000188 Placa Carreta 2-AWW5383-Maringa /PR-TRANSPORTADORA EQUADOR LTDA '.
  xml_string = xml_string &&  'ME-CNPJ-04492373000188 Nr. CIOT: 133000923517 Nr. Contrato de Viagem Administradora: 202102131968</xObs> '.
  xml_string = xml_string &&  '<ObsCont xCampo="Placa Cavalo"> '.
  xml_string = xml_string &&  '<xTexto> '.
  xml_string = xml_string &&  'BDY0I56-Maringa / PR-HU - TRANSPORTE RODOVIARIO LTDA- CNPJ:00124733000138</xTexto> '.
  xml_string = xml_string &&  '</ObsCont> '.
  xml_string = xml_string &&  '<ObsCont xCampo="Placa Carreta 1"> '.
  xml_string = xml_string &&  '<xTexto> '.
  xml_string = xml_string &&  'AWW5382-Maringa / PR-TRANSPORTADORA EQUADOR LTDA ME- CNPJ:04492373000188</xTexto> '.
  xml_string = xml_string &&  '</ObsCont> '.
  xml_string = xml_string &&  '<ObsCont xCampo="Placa Carreta 2"> '.
  xml_string = xml_string &&  '<xTexto> '.
  xml_string = xml_string &&  'AWW5383-Maringa / PR-TRANSPORTADORA EQUADOR LTDA ME- CNPJ:04492373000188</xTexto> '.
  xml_string = xml_string &&  '</ObsCont> '.
  xml_string = xml_string &&  '<ObsCont xCampo="Motorista"> '.
  xml_string = xml_string &&  '<xTexto> '.
  xml_string = xml_string &&  'GILMAR HOLSBACH DA SILVA- CPF:59250496168</xTexto> '.
  xml_string = xml_string &&  '</ObsCont> '.
  xml_string = xml_string &&  '</compl> '.
  xml_string = xml_string &&  '<emit> '.
  xml_string = xml_string &&  '<CNPJ> '.
  xml_string = xml_string &&  '10962697001298</CNPJ> '.
  xml_string = xml_string &&  '<IE> '.
  xml_string = xml_string &&  '294912878</IE> '.
  xml_string = xml_string &&  '<xNome> '.
  xml_string = xml_string &&  'AMAGGI LOUIS DREYFUS ZEN NOH GRAOS</xNome> '.
  xml_string = xml_string &&  '<xFant> '.
  xml_string = xml_string &&  'ALZ TRANSPORTES COLINAS - TO</xFant> '.
  xml_string = xml_string &&  '<enderEmit> '.
  xml_string = xml_string &&  '<xLgr> '.
  xml_string = xml_string &&  'ROD BR 153</xLgr> '.
  xml_string = xml_string &&  '<nro>'.
  xml_string = xml_string &&  'S/N</nro> '.
  xml_string = xml_string &&  '<xBairro> '.
  xml_string = xml_string &&  'SETOR CAMPINAS</xBairro> '.
  xml_string = xml_string &&  '<cMun> '.
  xml_string = xml_string &&  '1705508</cMun> '.
  xml_string = xml_string &&  '<xMun> '.
  xml_string = xml_string &&  'COLINAS DO TOCANTINS</xMun> '.
  xml_string = xml_string &&  '<CEP> '.
  xml_string = xml_string &&  '77760000</CEP> '.
  xml_string = xml_string &&  '<UF> '.
  xml_string = xml_string &&  'TO</UF> '.
  xml_string = xml_string &&  '<fone> '.
  xml_string = xml_string &&  '6332197700</fone> '.
  xml_string = xml_string &&  '</enderEmit> '.
  xml_string = xml_string &&  '</emit> '.
  xml_string = xml_string &&  '<rem> '.
  xml_string = xml_string &&  '<CNPJ> '.
  xml_string = xml_string &&  '10962697001107</CNPJ> '.
  xml_string = xml_string &&  '<IE> '.
  xml_string = xml_string &&  '294724133</IE> '.
  xml_string = xml_string &&  '<xNome> '.
  xml_string = xml_string &&  'AMAGGI LOUIS DREYFUS ZEN NOH GRAOS</xNome> '.
  xml_string = xml_string &&  '<fone>  '.
  xml_string = xml_string &&  '7736392200</fone>  '.
  xml_string = xml_string &&  '<enderReme>  '.
  xml_string = xml_string &&  '<xLgr>  '.
  xml_string = xml_string &&  'ROD BR 153 KM 717</xLgr>  '.
  xml_string = xml_string &&  '<nro>  '.
  xml_string = xml_string &&  'SN</nro>  '.
  xml_string = xml_string &&  '<xBairro>  '.
  xml_string = xml_string &&  'ZONA RURAL</xBairro>  '.
  xml_string = xml_string &&  '<cMun>  '.
  xml_string = xml_string &&  '1707652</cMun>  '.
  xml_string = xml_string &&  '<xMun>  '.
  xml_string = xml_string &&  'FIGUEIROPOLIS</xMun>  '.
  xml_string = xml_string &&  '<CEP>  '.
  xml_string = xml_string &&  '77465000</CEP>  '.
  xml_string = xml_string &&  '<UF>'.
  xml_string = xml_string &&  'TO</UF>'.
  xml_string = xml_string &&  '<cPais> '.
  xml_string = xml_string &&  '1058</cPais>'.
  xml_string = xml_string &&  '<xPais>'.
  xml_string = xml_string &&  'Brasil</xPais> '.
  xml_string = xml_string &&  '</enderReme>'.
  xml_string = xml_string &&  '</rem>'.
  xml_string = xml_string &&  '<exped>'.
  xml_string = xml_string &&  '<CNPJ>'.
  xml_string = xml_string &&  '10962697001107</CNPJ> '.
  xml_string = xml_string &&  '<IE>'.
  xml_string = xml_string &&  '294724133</IE> '.
  xml_string = xml_string &&  '<xNome>'.
  xml_string = xml_string &&  'AMAGGI LOUIS DREYFUS ZEN NOH GRAOS</xNome> '.
  xml_string = xml_string &&  '<fone>'.
  xml_string = xml_string &&  '7736392200</fone> '.
  xml_string = xml_string &&  '<enderExped>'.
  xml_string = xml_string &&  '<xLgr>'.
  xml_string = xml_string &&  'ROD BR 153 KM 717</xLgr> '.
  xml_string = xml_string &&  '<nro>'.
  xml_string = xml_string &&  'SN</nro> '.
  xml_string = xml_string &&  '<xBairro>'.
  xml_string = xml_string &&  'ZONA RURAL</xBairro> '.
  xml_string = xml_string &&  '<cMun>'.
  xml_string = xml_string &&  '1707652</cMun> '.
  xml_string = xml_string &&  '<xMun>'.
  xml_string = xml_string &&  'FIGUEIROPOLIS</xMun>'.
  xml_string = xml_string &&  '<CEP>'.
  xml_string = xml_string &&  '77465000</CEP> '.
  xml_string = xml_string &&  '<UF> '.
  xml_string = xml_string &&  'TO</UF>'.
  xml_string = xml_string &&  '<cPais> '.
  xml_string = xml_string &&  '1058</cPais> '.
  xml_string = xml_string &&  '<xPais> '.
  xml_string = xml_string &&  'Brasil</xPais> '.
  xml_string = xml_string &&  '</enderExped> '.
  xml_string = xml_string &&  '</exped> '.
  xml_string = xml_string &&  '<receb> '.
  xml_string = xml_string &&  '<CNPJ> '.
  xml_string = xml_string &&  '15143827000202</CNPJ> '.
  xml_string = xml_string &&  '<IE> '.
  xml_string = xml_string &&  '124079172</IE> '.
  xml_string = xml_string &&  '<xNome> '.
  xml_string = xml_string &&  'AMAGGI LOUIS DREYFUS ZEN NOH TERM INAIS PORTUARIOS SA</xNome> '.
  xml_string = xml_string &&  '<fone>'.
  xml_string = xml_string &&  '6631996195</fone> '.
  xml_string = xml_string &&  '<enderReceb>'.
  xml_string = xml_string &&  '<xLgr>'.
  xml_string = xml_string &&  'AV DOS PORTUGUESES</xLgr> '.
  xml_string = xml_string &&  '<nro> '.
  xml_string = xml_string &&  '100</nro> '.
  xml_string = xml_string &&  '<xBairro> '.
  xml_string = xml_string &&  'ITAQUI</xBairro> '.
  xml_string = xml_string &&  '<cMun> '.
  xml_string = xml_string &&  '2111300</cMun> '.
  xml_string = xml_string &&  '<xMun> '.
  xml_string = xml_string &&  'SAO LUIS</xMun> '.
  xml_string = xml_string &&  '<CEP> '.
  xml_string = xml_string &&  '65085582</CEP> '.
  xml_string = xml_string &&  '<UF> '.
  xml_string = xml_string &&  'MA</UF>'.
  xml_string = xml_string &&  '<cPais> '.
  xml_string = xml_string &&  '1058</cPais> '.
  xml_string = xml_string &&  '<xPais>'.
  xml_string = xml_string &&  'Brasil</xPais> '.
  xml_string = xml_string &&  '</enderReceb> '.
  xml_string = xml_string &&  '</receb> '.
  xml_string = xml_string &&  '<dest> '.
  xml_string = xml_string &&  '<CNPJ> '.
  xml_string = xml_string &&  '10962697001107</CNPJ> '.
  xml_string = xml_string &&  '<IE> '.
  xml_string = xml_string &&  '294724133</IE> '.
  xml_string = xml_string &&  '<xNome> '.
  xml_string = xml_string &&  'AMAGGI LOUIS DREYFUS ZEN NOH GRAOS</xNome> '.
  xml_string = xml_string &&  '<enderDest> '.
  xml_string = xml_string &&  '<xLgr>'.
  xml_string = xml_string &&  'ROD BR 153 KM 717</xLgr> '.
  xml_string = xml_string &&  '<nro> '.
  xml_string = xml_string &&  'SN</nro> '.
  xml_string = xml_string &&  '<xBairro> '.
  xml_string = xml_string &&  'ZONA RURAL</xBairro> '.
  xml_string = xml_string &&  '<cMun> '.
  xml_string = xml_string &&  '1707652</cMun> '.
  xml_string = xml_string &&  '<xMun> '.
  xml_string = xml_string &&  'FIGUEIROPOLIS</xMun> '.
  xml_string = xml_string &&  '<CEP> '.
  xml_string = xml_string &&  '77465000</CEP> '.
  xml_string = xml_string &&  '<UF> '.
  xml_string = xml_string &&  'TO</UF> '.
  xml_string = xml_string &&  '<cPais> '.
  xml_string = xml_string &&  '1058</cPais> '.
  xml_string = xml_string &&  '<xPais> '.
  xml_string = xml_string &&  'Brasil</xPais> '.
  xml_string = xml_string &&  '</enderDest> '.
  xml_string = xml_string &&  '</dest> '.
  xml_string = xml_string &&  '<vPrest> '.
  xml_string = xml_string &&  '<vTPrest> '.
  xml_string = xml_string &&  '9226.40</vTPrest> '.
  xml_string = xml_string &&  '<vRec> '.
  xml_string = xml_string &&  '9226.40</vRec> '.
  xml_string = xml_string &&  '</vPrest> '.
  xml_string = xml_string &&  '<imp> '.
  xml_string = xml_string &&  '<ICMS> '.
  xml_string = xml_string &&  '<ICMS00> '.
  xml_string = xml_string &&  '<CST>'.
  xml_string = xml_string &&  '00</CST>'.
  xml_string = xml_string &&  '<vBC> '.
  xml_string = xml_string &&  '9226.40</vBC> '.
  xml_string = xml_string &&  '<pICMS> '.
  xml_string = xml_string &&  '12.00</pICMS> '.
  xml_string = xml_string &&  '<vICMS> '.
  xml_string = xml_string &&  '1107.17</vICMS> '.
  xml_string = xml_string &&  '</ICMS00> '.
  xml_string = xml_string &&  '</ICMS> '.
  xml_string = xml_string &&  '</imp> '.
  xml_string = xml_string &&  '<infCTeNorm> '.
  xml_string = xml_string &&  '<infCarga> '.
  xml_string = xml_string &&  '<vCarga> '.
  xml_string = xml_string &&  '121400.00</vCarga> '.
  xml_string = xml_string &&  '<proPred>'.
  xml_string = xml_string &&  'SOJA EM GRAOS ADQ TERCEIROS</proPred> '.
  xml_string = xml_string &&  '<infQ> '.
  xml_string = xml_string &&  '<cUnid> '.
  xml_string = xml_string &&  '01</cUnid> '.
  xml_string = xml_string &&  '<tpMed> '.
  xml_string = xml_string &&  'PESO BRUTO</tpMed> '.
  xml_string = xml_string &&  '<qCarga> '.
  xml_string = xml_string &&  '48560.0000</qCarga> '.
  xml_string = xml_string &&  '</infQ> '.
  xml_string = xml_string &&  '<infQ> '.
  xml_string = xml_string &&  '<cUnid> '.
  xml_string = xml_string &&  '01</cUnid> '.
  xml_string = xml_string &&  '<tpMed> '.
  xml_string = xml_string &&  'PESO LIQUIDO</tpMed> '.
  xml_string = xml_string &&  '<qCarga> '.
  xml_string = xml_string &&  '48560.0000</qCarga> '.
  xml_string = xml_string &&  '</infQ> '.
  xml_string = xml_string &&  '<vCargaAverb> '.
  xml_string = xml_string &&  '121400.00</vCargaAverb> '.
  xml_string = xml_string &&  '</infCarga> '.
  xml_string = xml_string &&  '<infDoc> '.
  xml_string = xml_string &&  '<infNFe>'.
  xml_string = xml_string &&  '<chave>'.
  xml_string = xml_string &&  '17210210962697001107550020000183241338751036</chave> '.
  xml_string = xml_string &&  '</infNFe> '.
  xml_string = xml_string &&  '</infDoc> '.
  xml_string = xml_string &&  '<infModal versaoModal="3.00"> '.
  xml_string = xml_string &&  '<rodo> '.
  xml_string = xml_string &&  '<RNTRC> '.
  xml_string = xml_string &&  '50510500</RNTRC> '.
  xml_string = xml_string &&  '</rodo> '.
  xml_string = xml_string &&  '</infModal> '.
  xml_string = xml_string &&  '</infCTeNorm> '.
  xml_string = xml_string &&  '</infCte>'.
  xml_string = xml_string &&  '<infCTeSupl> '.
  xml_string = xml_string &&  '<qrCodCTe>'.
  xml_string = xml_string &&  'https://dfe-portal.svrs.rs.gov.br/cte/qrCode?chCTe=17210210962697001298570000000048701340944801&amp;tpAmb=1</qrCodCTe> '.
  xml_string = xml_string &&  '</infCTeSupl> '.
  xml_string = xml_string &&  '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"> '.
  xml_string = xml_string &&  '<SignedInfo>'.
  xml_string = xml_string &&  '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/> '.
  xml_string = xml_string &&  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/> '.
  xml_string = xml_string &&  '<Reference URI="#CTe17210210962697001298570000000048701340944801"> '.
  xml_string = xml_string &&  '<Transforms> '.
  xml_string = xml_string &&  '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/> '.
  xml_string = xml_string &&  '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/> '.
  xml_string = xml_string &&  '</Transforms> '.
  xml_string = xml_string &&  '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/> '.
  xml_string = xml_string &&  '<DigestValue> '.
  xml_string = xml_string &&  '99VoVLqX6NKHgdsULHUx4aBNrZA=</DigestValue> '.
  xml_string = xml_string &&  '</Reference> '.
  xml_string = xml_string &&  '</SignedInfo> '.
  xml_string = xml_string &&  '<SignatureValue> '.
  xml_string = xml_string &&  'J5O8MRj7O304/GVQLLTp0g9MFxG/0bVx5f7a7qRaIu4OOi9rcMju+XsHXySDOUoR3zVzALYo91e8'.
  xml_string = xml_string &&  '2SDe4Pv8gLecSOj+dWEiSK/QJW3JZ5FTZ/MsD4a5b6+jpCpSGXW9DI/gfbYkf0ws8AC5mhdRkyay'.
  xml_string = xml_string &&  'q7+U0wkSE7m2F4DkO9wP2tKOWVU9ZFCRVw18PJR/G6PAXRUzNc8DLzov1AF+B8eKrX81WTYDq+tz'.
  xml_string = xml_string &&  'TCUVzpX/FXkcZ2qDsxbZlilj1p5CdeZ2tRMYjA3iB7cK62vFAI/sMDvx2dn+ciWMjUWRinIILE0e'.
  xml_string = xml_string &&  'ywQUKvnRt1BTHOVe9Q8TXjLZO3LbH8hXfXgWeQ==</SignatureValue> '.
  xml_string = xml_string &&  '<KeyInfo>'.
  xml_string = xml_string &&  '<X509Data>'.
  xml_string = xml_string &&  '<X509Certificate>'.
  xml_string = xml_string &&  'MIIIAjCCBeqgAwIBAgIIOGWQnRAWG/EwDQYJKoZIhvcNAQELBQAwdDELMAkGA1UEBhMCQlIxEzAR'.
  xml_string = xml_string &&  'BgNVBAoTCklDUC1CcmFzaWwxNjA0BgNVBAsTLVNlY3JldGFyaWEgZGEgUmVjZWl0YSBGZWRlcmFs'.
  xml_string = xml_string &&  'IGRvIEJyYXNpbCAtIFJGQjEYMBYGA1UEAxMPQUMgVkFMSUQgUkZCIHY1MB4XDTIwMDcyNzE3MDAx'.
  xml_string = xml_string &&  'OVoXDTIxMDcyNzE3MDAxOVowggEZMQswCQYDVQQGEwJCUjELMAkGA1UECBMCQkExHzAdBgNVBAcT'.
  xml_string = xml_string &&  'FkxVSVMgRURVQVJETyBNQUdBTEhBRVMxEzARBgNVBAoTCklDUC1CcmFzaWwxNjA0BgNVBAsTLVNl'.
  xml_string = xml_string &&  'Y3JldGFyaWEgZGEgUmVjZWl0YSBGZWRlcmFsIGRvIEJyYXNpbCAtIFJGQjEWMBQGA1UECxMNUkZC'.
  xml_string = xml_string &&  'IGUtQ05QSiBBMTEeMBwGA1UECxMVQVIgQUNBTyBDRVJUSUZJQ0FET1JBMRcwFQYDVQQLEw4yMzcz'.
  xml_string = xml_string &&  'MTMwODAwMDEwMjE+MDwGA1UEAxM1QU1BR0dJIExPVUlTIERSRVlGVVMgWkVOIE5PSCBHUkFPUyBT'.
  xml_string = xml_string &&  'IEE6MTA5NjI2OTcwMDAxMzUwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDaJxJ713l5'.
  xml_string = xml_string &&  'xBnCPAeurjLs4hXrbqHeHHBu5BXXg6OW93nWfuwQY5wrbpqYhYLDCDLD6VMIGgQ5vYHpXzEk6gSS'.
  xml_string = xml_string &&  '53L7R1cH7Q1ssTRpzLevvXznwHAGgL3TEVo72qxxP/WosLCs45GdswteopeyKX7vEw9zyclpj/Kv'.
  xml_string = xml_string &&  'BPSBSt2/h/acaje9R1Vxc3ydIHy9K3PixAAkOeGhAZOe17tQ59axS5GNg0MJYD+VekaVd3uXuNbw'.
  xml_string = xml_string &&  '1bkHYOKo50HEVwONegXngMrUmiHX0gz3T5F2CaHm8P25Qg267dcxxF9rPLJvU6Pvang4L9wCidaP'.
  xml_string = xml_string &&  '8IwAlLyZqFLvxp1yrr5Hz8eG5AlvAgMBAAGjggLvMIIC6zCBnAYIKwYBBQUHAQEEgY8wgYwwVQYI'.
  xml_string = xml_string &&  'KwYBBQUHMAKGSWh0dHA6Ly9pY3AtYnJhc2lsLnZhbGlkY2VydGlmaWNhZG9yYS5jb20uYnIvYWMt'.
  xml_string = xml_string &&  'dmFsaWRyZmIvYWMtdmFsaWRyZmJ2NS5wN2IwMwYIKwYBBQUHMAGGJ2h0dHA6Ly9vY3NwdjUudmFs'.
  xml_string = xml_string &&  'aWRjZXJ0aWZpY2Fkb3JhLmNvbS5icjAJBgNVHRMEAjAAMB8GA1UdIwQYMBaAFFPLpeR1UJlALL5b'.
  xml_string = xml_string &&  'FUXJvsswqonFMHAGA1UdIARpMGcwZQYGYEwBAgElMFswWQYIKwYBBQUHAgEWTWh0dHA6Ly9pY3At'.
  xml_string = xml_string &&  'YnJhc2lsLnZhbGlkY2VydGlmaWNhZG9yYS5jb20uYnIvYWMtdmFsaWRyZmIvZHBjLWFjLXZhbGlk'.
  xml_string = xml_string &&  'cmZidjUucGRmMIG2BgNVHR8Ega4wgaswU6BRoE+GTWh0dHA6Ly9pY3AtYnJhc2lsLnZhbGlkY2Vy'.
  xml_string = xml_string &&  'dGlmaWNhZG9yYS5jb20uYnIvYWMtdmFsaWRyZmIvbGNyLWFjLXZhbGlkcmZidjUuY3JsMFSgUqBQ'.
  xml_string = xml_string &&  'hk5odHRwOi8vaWNwLWJyYXNpbDIudmFsaWRjZXJ0aWZpY2Fkb3JhLmNvbS5ici9hYy12YWxpZHJm'.
  xml_string = xml_string &&  'Yi9sY3ItYWMtdmFsaWRyZmJ2NS5jcmwwDgYDVR0PAQH/BAQDAgXgMB0GA1UdJQQWMBQGCCsGAQUF'.
  xml_string = xml_string &&  'BwMCBggrBgEFBQcDBDCBwwYDVR0RBIG7MIG4gRVHaXNsYWluZS5jcnV6QGxkYy5jb22gOAYFYEwB'.
  xml_string = xml_string &&  'AwSgLwQtMjgwNjE5ODAwMzM5MjgzMTQ5MjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwoDEGBWBM'.
  xml_string = xml_string &&  'AQMCoCgEJk1BVVJJQ0lPIEhBUkRNQU4gVEFWQVJFUyBERSBNRUxPIEZJTEhPoBkGBWBMAQMDoBAE'.
  xml_string = xml_string &&  'DjEwOTYyNjk3MDAwMTM1oBcGBWBMAQMHoA4EDDAwMDAwMDAwMDAwMDANBgkqhkiG9w0BAQsFAAOC'.
  xml_string = xml_string &&  'AgEAzDGYrZ+OkGxXyPPLwJjHDfZQAecMTs1aZyruMR+sZkDNRP+GKnntsKbr4fF9T+0w+VBB82S0'.
  xml_string = xml_string &&  '1xKYwvV099GbISWB5k6JpmMmqTKWmUtkBOty5tkCPt08tYGtp3Ncu0RQFpBBDUxQEem2fh+RAXYX'.
  xml_string = xml_string &&  'iTcqLMIKloMTv7cSuZW4PRr1mI2r7xNwIQyMZ/G8nq9yprnsBSTgYOHwYkhiD9zOdDiBgoXC/6PE'.
  xml_string = xml_string &&  'qT4iqR6GooRzO/OMxgsyiEPAfr4LuYYnePb2bIPMpoNRt1AK9mAdXnZ+edyqDaudDLYo5jwk4dl6'.
  xml_string = xml_string &&  'NlG2cffCWrD7IavJXCG36G+vte6TdJ1Vg7SEscoQwFRsH9UCrUhO6WBpMtxqknT/CKnxsqoghhxy'.
  xml_string = xml_string &&  '5d5+b0sPdTxPioEJIxcHtdnwpfpmY8ZsihHxirkMMEqTu6Vs3JOt8Ca31Br1M/hmfnPizn4GageL'.
  xml_string = xml_string &&  'q5V8vT0HUxCv/qnw3AyM5UQHoWFdexVfAKfWlmqxxhTbO3GtMfRV9NiISdrDh3V1vwyhBN0BlZp6'.
  xml_string = xml_string &&  'FfcHU4Bs/gNPzzTkZV+8wElw1hsyCHfRTsF5LHtEnBeH6IGdSBujHyPT7w0KLMXIFFNxVFtQmprn'.
  xml_string = xml_string &&  'pRzWGBUtAnzgKI5j7Ekuh+lTkOVyQLWZI1WUYnCGrTlzLXB1UYJulVyHhBsXgod7TEGw5bTgwbbE'.
  xml_string = xml_string &&  'n1Q=</X509Certificate> '.
  xml_string = xml_string &&  '</X509Data>'.
  xml_string = xml_string &&  '</KeyInfo>'.
  xml_string = xml_string &&  '</Signature> '.
  xml_string = xml_string &&  '</CTe>'.
  xml_string = xml_string &&  '<protCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="3.00"> '.
  xml_string = xml_string &&  '<infProt Id="CTe317210000473715"> '.
  xml_string = xml_string &&  '<tpAmb>'.
  xml_string = xml_string &&  '1</tpAmb> '.
  xml_string = xml_string &&  '<verAplic>'.
  xml_string = xml_string &&  'RS20200915181434</verAplic> '.
  xml_string = xml_string &&  '<chCTe>'.
  xml_string = xml_string &&  '17210210962697001298570000000048701340944801</chCTe> '.
  xml_string = xml_string &&  '<dhRecbto> '.
  xml_string = xml_string &&  '2021-02-18T09:57:12-03:00</dhRecbto> '.
  xml_string = xml_string &&  '<nProt> '.
  xml_string = xml_string &&  '317210000473715</nProt> '.
  xml_string = xml_string &&  '<digVal> '.
  xml_string = xml_string &&  '99VoVLqX6NKHgdsULHUx4aBNrZA=</digVal> '.
  xml_string = xml_string &&  '<cStat>'.
  xml_string = xml_string &&  '100</cStat>'.
  xml_string = xml_string &&  '<xMotivo>'.
  xml_string = xml_string &&  'Autorizado o uso do CT-e</xMotivo> '.
  xml_string = xml_string &&  '</infProt>'.
  xml_string = xml_string &&  '</protCTe>'.
  xml_string = xml_string &&  '</cteProc>'.


*  xml_string = '<?xml version="1.0" encoding="utf-8"?>'.
*  xml_string = xml_string &&  '<cteProc versao="3.00" xmlns="http://www.portalfiscal.inf.br/cte">'.
*  xml_string = xml_string &&  '<CTe>'.
*  xml_string = xml_string &&  '<infCte Id="CTe51201277294254001670570000012944191863080620" versao="3.00">'.
*  xml_string = xml_string &&  '<ide>'.
*  xml_string = xml_string &&  '<cUF>'.
*  xml_string = xml_string &&  '51</cUF>'.
*  xml_string = xml_string &&  '<cCT>'.
*  xml_string = xml_string &&  '86308062</cCT>'.
*  xml_string = xml_string &&  '<CFOP>'.
*  xml_string = xml_string &&  '5352</CFOP>'.
*  xml_string = xml_string &&  '<natOp>'.
*  xml_string = xml_string &&  'Prestação serv. transp. estab. industrial</natOp>'.
*  xml_string = xml_string &&  '<mod>'.
*  xml_string = xml_string &&  '57</mod>'.
*  xml_string = xml_string &&  '<serie>'.
*  xml_string = xml_string &&  '0</serie>'.
*  xml_string = xml_string &&  '<nCT>'.
*  xml_string = xml_string &&  '1294419'.
*  xml_string = xml_string &&  '</nCT>'.
*  xml_string = xml_string &&  '<dhEmi>'.
*  xml_string = xml_string &&  '2020-12-31T06:29:42-04:00</dhEmi>'.
*  xml_string = xml_string &&  '<tpImp>'.
*  xml_string = xml_string &&  '1</tpImp>'.
*  xml_string = xml_string &&  '<tpEmis>'.
*  xml_string = xml_string &&  '1</tpEmis>'.
*  xml_string = xml_string &&  '<cDV>'.
*  xml_string = xml_string &&  '0</cDV>'.
*  xml_string = xml_string &&  '<tpAmb>'.
*  xml_string = xml_string &&  '1</tpAmb>'.
*  xml_string = xml_string &&  '<tpCTe>'.
*  xml_string = xml_string &&  '0</tpCTe>'.
*  xml_string = xml_string &&  '<procEmi>'.
*  xml_string = xml_string &&  '0</procEmi>'.
*  xml_string = xml_string &&  '<verProc>'.
*  xml_string = xml_string &&  '008</verProc>'.
*  xml_string = xml_string &&  '<cMunEnv>'.
*  xml_string = xml_string &&  '5103403</cMunEnv>'.
*  xml_string = xml_string &&  '<xMunEnv>'.
*  xml_string = xml_string &&  'CUIABA</xMunEnv>'.
*  xml_string = xml_string &&  '<UFEnv>'.
*  xml_string = xml_string &&  'MT</UFEnv>'.
*  xml_string = xml_string &&  '<modal>'.
*  xml_string = xml_string &&  '01</modal>'.
*  xml_string = xml_string &&  '<tpServ>'.
*  xml_string = xml_string &&  '0</tpServ>'.
*  xml_string = xml_string &&  '<cMunIni>'.
*  xml_string = xml_string &&  '5103304</cMunIni>'.
*  xml_string = xml_string &&  '<xMunIni>'.
*  xml_string = xml_string &&  'COMODORO</xMunIni>'.
*  xml_string = xml_string &&  '<UFIni>'.
*  xml_string = xml_string &&  'MT</UFIni>'.
*  xml_string = xml_string &&  '<cMunFim>'.
*  xml_string = xml_string &&  '5107248</cMunFim>'.
*  xml_string = xml_string &&  '<xMunFim>'.
*  xml_string = xml_string &&  'SANTA CARMEM</xMunFim>'.
*  xml_string = xml_string &&  '<UFFim>'.
*  xml_string = xml_string &&  'MT</UFFim>'.
*  xml_string = xml_string &&  '<retira>'.
*  xml_string = xml_string &&  '1</retira>'.
*  xml_string = xml_string &&  '<indIEToma>'.
*  xml_string = xml_string &&  '1</indIEToma>'.
*  xml_string = xml_string &&  '<toma3>'.
*  xml_string = xml_string &&  '<toma>'.
*  xml_string = xml_string &&  '0</toma>'.
*  xml_string = xml_string &&  '</toma3>'.
*  xml_string = xml_string &&  '</ide>'.
*  xml_string = xml_string &&  '<compl>'.
*  xml_string = xml_string &&  '<xObs>'.
*  xml_string = xml_string &&  'ICMS DIFERIDO CONF ART. 37, INC. I DO ANEXO VII DO RICMS/MT 2014 Numero do Transporte: 0003006386 Numero'.
*  xml_string = xml_string &&  ' do Faturamento: 0094670889 Subcontratado: VEIGA E BUCCO COMERCIO E TRANSPORTE AVENIDA MAJOR AMARANTE - Nr: 3058 Bairro: CENTRO - Muni: Vilhena - RO CEP:'.
*  xml_string = xml_string &&  ' 76987-247 RNTRC: 44845980 14.262.283/0001-54 IE-00000003407900 Local de entrega: Transbordo: ANDRE GIMENEZ HOFFMANN 00.002.651/868 IE-137051867 Placa '.
*  xml_string = xml_string &&  ' Cavalo-AKD1H40-Vera Cruz do Oeste /PR-VEIGA E BUCCO COMERCIO E TRANSPORTE-CNPJ-14262283000154 Placa Carreta 1-AKP9887-Ji-Parana /RO-VEIGA E BUCCO COMERCIO '.
*  xml_string = xml_string &&  ' E TRANSPORTE-CNPJ-14262283000154 Placa Carreta 2-AKP9899-Ji-Parana /RO-VEIGA E BUCCO COMERCIO E TRANSPORTE-CNPJ-14262283000154 Valor do pedagio: 107,10'.
*  xml_string = xml_string &&  ' Nr. CIOT: 136000876162 Nr. Contrato de Viagem Administradora: 202002084327</xObs>'.
*  xml_string = xml_string &&  '<ObsCont xCampo="Placa Cavalo">'.
*  xml_string = xml_string &&  '<xTexto>'.
*  xml_string = xml_string &&  'AKD1H40-Vera Cruz do Oeste /'.
*  xml_string = xml_string &&  ' PR-VEIGA E BUCCO COMERCIO E TRANSPORTE- CNPJ:14262283000154</xTexto>'.
*  xml_string = xml_string &&  '</ObsCont>'.
*  xml_string = xml_string &&  '<ObsCont xCampo="Placa Carreta 1">'.
*  xml_string = xml_string &&  '<xTexto>'.
*  xml_string = xml_string &&  'AKP9887-Ji-Parana'.
*  xml_string = xml_string &&  ' / RO-VEIGA E BUCCO COMERCIO E TRANSPORTE- CNPJ:14262283000154</xTexto>'.
*  xml_string = xml_string &&  '</ObsCont>'.
*  xml_string = xml_string &&  '<ObsCont xCampo="Placa Carreta 2">'.
*  xml_string = xml_string &&  '<xTexto>'.
*  xml_string = xml_string &&  'AKP9899-Ji-Parana'.
*  xml_string = xml_string &&  ' / RO-VEIGA E BUCCO COMERCIO E TRANSPORTE- CNPJ:14262283000154</xTexto>'.
*  xml_string = xml_string &&  '</ObsCont>'.
*  xml_string = xml_string &&  '<ObsCont xCampo="Motorista">'.
*  xml_string = xml_string &&  '<xTexto>'.
*  xml_string = xml_string &&  'MARCOS AURELIO VASSOLER'.
*  xml_string = xml_string &&  ' - CPF:98769278172</xTexto>'.
*  xml_string = xml_string &&  '</ObsCont>'.
*  xml_string = xml_string &&  '</compl>'.
*  xml_string = xml_string &&  '<emit>'.
*  xml_string = xml_string &&  '<CNPJ>'.
*  xml_string = xml_string &&  '77294254001670</CNPJ>'.
*  xml_string = xml_string &&  '<IE>'.
*  xml_string = xml_string &&  '131511726</IE>'.
*  xml_string = xml_string &&  '<xNome>'.
*  xml_string = xml_string &&  'AMAGGI EXPORT E IMPORT LTDA</xNome>'.
*  xml_string = xml_string &&  ' <xFant>'.
*  xml_string = xml_string &&  'TRANSPORTADORA</xFant>'.
*  xml_string = xml_string &&  '<enderEmit>'.
*  xml_string = xml_string &&  '<xLgr>'.
*  xml_string = xml_string &&  'AV ANDRE ANTONIO MAGGI</xLgr>'.
*  xml_string = xml_string &&  '<nro>'.
*  xml_string = xml_string &&  '303</nro>'.
*  xml_string = xml_string &&  '<xBairro>'.
*  xml_string = xml_string &&  'ALVORADA</xBairro>'.
*  xml_string = xml_string &&  '<cMun>'.
*  xml_string = xml_string &&  '5103403</cMun>'.
*  xml_string = xml_string &&  ' <xMun>'.
*  xml_string = xml_string &&  'CUIABA</xMun>'.
*  xml_string = xml_string &&  '<CEP>'.
*  xml_string = xml_string &&  '78049080</CEP>'.
*  xml_string = xml_string &&  '<UF>'.
*  xml_string = xml_string &&  'MT</UF>'.
*  xml_string = xml_string &&  '<fone>'.
*  xml_string = xml_string &&  '6634113000</fone>'.
*  xml_string = xml_string &&  '</enderEmit>'.
*  xml_string = xml_string &&  '</emit>'.
*  xml_string = xml_string &&  '<rem>'.
*  xml_string = xml_string &&  '<CNPJ>'.
*  xml_string = xml_string &&  '77294254007520</CNPJ>'.
*  xml_string = xml_string &&  '<IE>'.
*  xml_string = xml_string &&  '135946328'.
*  xml_string = xml_string &&  ' </IE>'.
*  xml_string = xml_string &&  '<xNome>'.
*  xml_string = xml_string &&  'AMAGGI EXPORTACAO E IMPORTACAO LTDA</xNome>'.
*  xml_string = xml_string &&  '<fone>'.
*  xml_string = xml_string &&  '6536455000</fone>'.
*  xml_string = xml_string &&  '<enderReme>'.
*  xml_string = xml_string &&  '<xLgr>'.
*  xml_string = xml_string &&  'ROD BR364 KM 120 ENTRONCAMENTO COM BR 174'.
*  xml_string = xml_string &&  ' </xLgr>'.
*  xml_string = xml_string &&  '<nro>'.
*  xml_string = xml_string &&  'SN</nro>'.
*  xml_string = xml_string &&  '<xBairro>'.
*  xml_string = xml_string &&  'DISTRITO AGROINDUSTRIAL</xBairro>'.
*  xml_string = xml_string &&  '<cMun>'.
*  xml_string = xml_string &&  '5103304</cMun>'.
*  xml_string = xml_string &&  '<xMun>'.
*  xml_string = xml_string &&  'COMODORO</xMun>'.
*  xml_string = xml_string &&  '<CEP>'.
*  xml_string = xml_string &&  '78310000</CEP>'.
*  xml_string = xml_string &&  '<UF>'.
*  xml_string = xml_string &&  'MT</UF>'.
*  xml_string = xml_string &&  '<cPais>'.
*  xml_string = xml_string &&  ' 1058</cPais>'.
*  xml_string = xml_string &&  '<xPais>'.
*  xml_string = xml_string &&  'Brasil</xPais>'.
*  xml_string = xml_string &&  '</enderReme>'.
*  xml_string = xml_string &&  '</rem>'.
*  xml_string = xml_string &&  '<exped>'.
*  xml_string = xml_string &&  '<CNPJ>'.
*  xml_string = xml_string &&  '77294254007520</CNPJ>'.
*  xml_string = xml_string &&  '<IE>'.
*  xml_string = xml_string &&  '135946328</IE>'.
*  xml_string = xml_string &&  '<xNome>'.
*  xml_string = xml_string &&  'AMAGGI EXPORTACAO E IMPORTACAO'.
*  xml_string = xml_string &&  ' LTDA</xNome>'.
*  xml_string = xml_string &&  '<fone>'.
*  xml_string = xml_string &&  '6536455000</fone>'.
*  xml_string = xml_string &&  '<enderExped>'.
*  xml_string = xml_string &&  '<xLgr>'.
*  xml_string = xml_string &&  'ROD BR364 KM 120 ENTRONCAMENTO COM BR 174</xLgr>'.
*  xml_string = xml_string &&  '<nro>'.
*  xml_string = xml_string &&  'SN</nro>'.
*  xml_string = xml_string &&  '<xBairro>'.
*  xml_string = xml_string &&  'DISTRITO AGROINDUSTRIAL'.
*  xml_string = xml_string &&  ' </xBairro>'.
*  xml_string = xml_string &&  '<cMun>'.
*  xml_string = xml_string &&  '5103304</cMun>'.
*  xml_string = xml_string &&  '<xMun>'.
*  xml_string = xml_string &&  'COMODORO</xMun>'.
*  xml_string = xml_string &&  '<CEP>'.
*  xml_string = xml_string &&  '78310000</CEP>'.
*  xml_string = xml_string &&  '<UF>'.
*  xml_string = xml_string &&  'MT</UF>'.
*  xml_string = xml_string &&  '<cPais>'.
*  xml_string = xml_string &&  '1058</cPais>'.
*  xml_string = xml_string &&  '<xPais>'.
*  xml_string = xml_string &&  'Brasil</xPais>'.
*  xml_string = xml_string &&  '</enderExped>'.
*  xml_string = xml_string &&  '</exped>'.
*  xml_string = xml_string &&  ' <receb>'.
*  xml_string = xml_string &&  '<CPF>'.
*  xml_string = xml_string &&  '02651868116</CPF>'.
*  xml_string = xml_string &&  '<IE>'.
*  xml_string = xml_string &&  '137051867</IE>'.
*  xml_string = xml_string &&  '<xNome>'.
*  xml_string = xml_string &&  'ANDRE GIMENEZ HOFFMANN</xNome>'.
*  xml_string = xml_string &&  '<enderReceb>'.
*  xml_string = xml_string &&  '<xLgr>'.
*  xml_string = xml_string &&  'ESTRADA ARIZONA</xLgr>'.
*  xml_string = xml_string &&  '<nro>'.
*  xml_string = xml_string &&  'SN</nro>'.
*  xml_string = xml_string &&  '<xCpl>'.
*  xml_string = xml_string &&  'FAZENDA</xCpl>'.
*  xml_string = xml_string &&  '<xBairro>'.
*  xml_string = xml_string &&  'ZONA RURAL</xBairro>'.
*  xml_string = xml_string &&  '<cMun>'.
*  xml_string = xml_string &&  '5107248</cMun>'.
*  xml_string = xml_string &&  '<xMun>'.
*  xml_string = xml_string &&  'SANTA CARMEM</xMun>'.
*  xml_string = xml_string &&  '<CEP>'.
*  xml_string = xml_string &&  '78545000</CEP>'.
*  xml_string = xml_string &&  '<UF>'.
*  xml_string = xml_string &&  'MT</UF>'.
*  xml_string = xml_string &&  '<cPais>'.
*  xml_string = xml_string &&  '1058</cPais>'.
*  xml_string = xml_string &&  '<xPais>'.
*  xml_string = xml_string &&  'Brasil</xPais>'.
*  xml_string = xml_string &&  '</enderReceb>'.
*  xml_string = xml_string &&  '</receb>'.
*  xml_string = xml_string &&  '<dest>'.
*  xml_string = xml_string &&  '<CPF>'.
*  xml_string = xml_string &&  '02651868116</CPF>'.
*  xml_string = xml_string &&  '<IE>'.
*  xml_string = xml_string &&  '137051867</IE>'.
*  xml_string = xml_string &&  '<xNome>'.
*  xml_string = xml_string &&  'ANDRE GIMENEZ HOFFMANN</xNome>'.
*  xml_string = xml_string &&  '<enderDest>'.
*  xml_string = xml_string &&  '<xLgr>'.
*  xml_string = xml_string &&  'ESTRADA ARIZONA</xLgr>'.
*  xml_string = xml_string &&  '<nro>'.
*  xml_string = xml_string &&  'SN</nro>'.
*  xml_string = xml_string &&  '<xCpl>'.
*  xml_string = xml_string &&  'FAZENDA</xCpl>'.
*  xml_string = xml_string &&  '<xBairro>'.
*  xml_string = xml_string &&  'ZONA RURAL</xBairro>'.
*  xml_string = xml_string &&  '<cMun>'.
*  xml_string = xml_string &&  '5107248</cMun>'.
*  xml_string = xml_string &&  '<xMun>'.
*  xml_string = xml_string &&  'SANTA CARMEM</xMun>'.
*  xml_string = xml_string &&  '<CEP>'.
*  xml_string = xml_string &&  '78545000</CEP>'.
*  xml_string = xml_string &&  '<UF>'.
*  xml_string = xml_string &&  'MT</UF>'.
*  xml_string = xml_string &&  '<cPais>'.
*  xml_string = xml_string &&  '1058</cPais>'.
*  xml_string = xml_string &&  '<xPais>'.
*  xml_string = xml_string &&  'Brasil</xPais>'.
*  xml_string = xml_string &&  '</enderDest>'.
*  xml_string = xml_string &&  '</dest>'.
*  xml_string = xml_string &&  '<vPrest>'.
*  xml_string = xml_string &&  '<vTPrest>'.
*  xml_string = xml_string &&  '4815.00</vTPrest>'.
*  xml_string = xml_string &&  '<vRec>'.
*  xml_string = xml_string &&  '4815.00</vRec>'.
*  xml_string = xml_string &&  '</vPrest>'.
*  xml_string = xml_string &&  '<imp>'.
*  xml_string = xml_string &&  '<ICMS>'.
*  xml_string = xml_string &&  '<ICMS45>'.
*  xml_string = xml_string &&  '<CST>'.
*  xml_string = xml_string &&  '51</CST>'.
*  xml_string = xml_string &&  '</ICMS45>'.
*  xml_string = xml_string &&  '</ICMS>'.
*  xml_string = xml_string &&  '</imp>'.
*  xml_string = xml_string &&  '<infCTeNorm>'.
*  xml_string = xml_string &&  '<infCarga>'.
*  xml_string = xml_string &&  '<vCarga>'.
*  xml_string = xml_string &&  '95078.53</vCarga>'.
*  xml_string = xml_string &&  '<proPred>'.
*  xml_string = xml_string &&  'FERT N MAIZ 20.00.20 8%S BB</proPred>'.
*  xml_string = xml_string &&  '<infQ>'.
*  xml_string = xml_string &&  '<cUnid>'.
*  xml_string = xml_string &&  '01</cUnid>'.
*  xml_string = xml_string &&  '<tpMed>'.
*  xml_string = xml_string &&  'PESO BRUTO</tpMed>'.
*  xml_string = xml_string &&  '<qCarga>'.
*  xml_string = xml_string &&  '48150.0000</qCarga>'.
*  xml_string = xml_string &&  '</infQ>'.
*  xml_string = xml_string &&  '<infQ>'.
*  xml_string = xml_string &&  '<cUnid>'.
*  xml_string = xml_string &&  '01</cUnid>'.
*  xml_string = xml_string &&  '<tpMed>'.
*  xml_string = xml_string &&  'PESO LIQUIDO</tpMed>'.
*  xml_string = xml_string &&  '<qCarga>'.
*  xml_string = xml_string &&  '48150.0000</qCarga>'.
*  xml_string = xml_string &&  '</infQ>'.
*  xml_string = xml_string &&  '<vCargaAverb>'.
*  xml_string = xml_string &&  '95078.53</vCargaAverb>'.
*  xml_string = xml_string &&  '</infCarga>'.
*  xml_string = xml_string &&  '<infDoc>'.
*  xml_string = xml_string &&  '<infNFe>'.
*  xml_string = xml_string &&  '<chave>'.
*  xml_string = xml_string &&  '51201277294254007520550000000679191134805330</chave>'.
*  xml_string = xml_string &&  '</infNFe>'.
*  xml_string = xml_string &&  '</infDoc>'.
*  xml_string = xml_string &&  '<infModal versaoModal="3.00">'.
*  xml_string = xml_string &&  '<rodo>'.
*  xml_string = xml_string &&  '<RNTRC>'.
*  xml_string = xml_string &&  '12458812</RNTRC>'.
*  xml_string = xml_string &&  '</rodo>'.
*  xml_string = xml_string &&  '</infModal>'.
*  xml_string = xml_string &&  '</infCTeNorm>'.
*  xml_string = xml_string &&  '</infCte>'.
*  xml_string = xml_string &&  '<infCTeSupl>'.
*  xml_string = xml_string &&  '<qrCodCTe>'.
*  xml_string = xml_string &&  'https://www.sefaz.mt.gov.br/cte/qrcode?chCTe=51201277294254001670570000012944191863080620&amp;tpAmb=1</qrCodCTe>'.
*  xml_string = xml_string &&  '</infCTeSupl>'.
*  xml_string = xml_string &&  '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">'.
*  xml_string = xml_string &&  '<SignedInfo>'.
*  xml_string = xml_string &&  '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>'.
*  xml_string = xml_string &&  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>'.
*  xml_string = xml_string &&  '<Reference URI="#CTe51201277294254001670570000012944191863080620">'.
*  xml_string = xml_string &&  '<Transforms>'.
*  xml_string = xml_string &&  '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>'.
*  xml_string = xml_string &&  '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>'.
*  xml_string = xml_string &&  '</Transforms>'.
*  xml_string = xml_string &&  '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>'.
*  xml_string = xml_string &&  '<DigestValue>'.
*  xml_string = xml_string &&  '+hUe8zlqfTEZm13PahNXuyEXoOU=</DigestValue>'.
*  xml_string = xml_string &&  '</Reference>'.
*  xml_string = xml_string &&  '</SignedInfo>'.
*  xml_string = xml_string &&  '<SignatureValue>'.
*  xml_string = xml_string &&  'lkFLnca0e3Xr5SuiyGWdgX8QNDwEssJdxCRyN6MycoQ0JcYYSjRkCEcCDTsWm00mPn0QTC/3KwAv'.
*  xml_string = xml_string &&  '/82bIf5uCpFp8IjOyQTt/9g1m8SNfxjXuL1B4jLZ+YJoGKivAfL8SG3JC3lBspBrxJVO/teEzl5Y'.
*  xml_string = xml_string &&  '8Fz/AWLhtuXIaTb/SFEwNRcsQ8uGJQlzvO4o4NWaFqTJqcs1NGVzpNeX3DAX5ftYxbsxfDZchw3I'.
*  xml_string = xml_string &&  'LTRal+1yP7s1iW3mlMghL5awlvRonbGFRqS7OtVWtxJpsx5nHr+sRxD1nAai4m49UkK1BfDSmBQ5'.
*  xml_string = xml_string &&  'yobRkRnLMsKkXeGbRtmdFKa/0QDRy8DmOKT+BA==</SignatureValue>'.
*  xml_string = xml_string &&  '<KeyInfo>'.
*  xml_string = xml_string &&  '<X509Data>'.
*  xml_string = xml_string &&  '<X509Certificate>'.
*  xml_string = xml_string &&  'MIIIDjCCBfagAwIBAgIIBbHPOyeNdHEwDQYJKoZIhvcNAQELBQAwdTELMAkGA1UEBhMCQlIxEzAR'.
*  xml_string = xml_string &&  'BgNVBAoMCklDUC1CcmFzaWwxNjA0BgNVBAsMLVNlY3JldGFyaWEgZGEgUmVjZWl0YSBGZWRlcmFs'.
*  xml_string = xml_string &&  'IGRvIEJyYXNpbCAtIFJGQjEZMBcGA1UEAwwQQUMgU0VSQVNBIFJGQiB2NTAeFw0yMDA5MDgxMjI5'.
*  xml_string = xml_string &&  'MDBaFw0yMTA5MDgxMjI5MDBaMIIBMDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1UMQ8wDQYDVQQH'.
*  xml_string = xml_string &&  'DAZDVUlBQkExEzARBgNVBAoMCklDUC1CcmFzaWwxGDAWBgNVBAsMDzAwMDAwMTAwOTgxMDAxMzE2'.
*  xml_string = xml_string &&  'MDQGA1UECwwtU2VjcmV0YXJpYSBkYSBSZWNlaXRhIEZlZGVyYWwgZG8gQnJhc2lsIC0gUkZCMRYw'.
*  xml_string = xml_string &&  'FAYDVQQLDA1SRkIgZS1DTlBKIEExMRkwFwYDVQQLDBBBQyBTRVJBU0EgUkZCIHY1MRcwFQYDVQQL'.
*  xml_string = xml_string &&  'DA4wMzIwODYxODAwMDEzMDETMBEGA1UECwwKUFJFU0VOQ0lBTDE7MDkGA1UEAwwyQU1BR0dJIEVY'.
*  xml_string = xml_string &&  'UE9SVEFDQU8gRSBJTVBPUlRBQ0FPIExUREE6NzcyOTQyNTQwMDAxOTQwggEiMA0GCSqGSIb3DQEB'.
*  xml_string = xml_string &&  'AQUAA4IBDwAwggEKAoIBAQC/ix3dCFOtKx+eejipdJrhdwJK+nwhif6xHY1GfASEKMjbl0x+m6cy'.
*  xml_string = xml_string &&  'OmP9jipyIQkukw+f6f3tDQe8dkqwLi8tfii1Hv8floTQ0iQCqCjXc7PQKG+pD8uE8800ByAfgWRt'.
*  xml_string = xml_string &&  'NoNzCNZ7uY+YQ7B47t9C2Bzw6trbN3ZFMH4JeSKzLkgUu/JiVak3npScfHe9hfqEQRNzgLp/JZyD'.
*  xml_string = xml_string &&  'zjzo0Ddc+BKnzuH2ZuK2IgkvUm+E7XApOli5b39ZS06+FKt50rMBlITm6O9XD6p1YIsOBNHqAVsd'.
*  xml_string = xml_string &&  '7NA1MnqGiFgMHmb6E05P9lUYP+jdC+Tp6jB64DuqPHw1kn8C+I2Mdv0TMUdZAgMBAAGjggLjMIIC'.
*  xml_string = xml_string &&  '3zAJBgNVHRMEAjAAMB8GA1UdIwQYMBaAFOzxQVFXqOY66V6zoCL5CIq1OoePMIGZBggrBgEFBQcB'.
*  xml_string = xml_string &&  'AQSBjDCBiTBIBggrBgEFBQcwAoY8aHR0cDovL3d3dy5jZXJ0aWZpY2Fkb2RpZ2l0YWwuY29tLmJy'.
*  xml_string = xml_string &&  'L2NhZGVpYXMvc2VyYXNhcmZidjUucDdiMD0GCCsGAQUFBzABhjFodHRwOi8vb2NzcC5jZXJ0aWZp'.
*  xml_string = xml_string &&  'Y2Fkb2RpZ2l0YWwuY29tLmJyL3NlcmFzYXJmYnY1MIGzBgNVHREEgaswgaiBHE1BUklBTkEuRFVB'.
*  xml_string = xml_string &&  'UlRFQEFNQUdHSS5DT00uQlKgGgYFYEwBAwKgERMPR1VOTkFSIE5FQkVMVU5HoBkGBWBMAQMDoBAT'.
*  xml_string = xml_string &&  'Djc3Mjk0MjU0MDAwMTk0oDgGBWBMAQMEoC8TLTI1MTExOTY5NzQ1NDAxNDk5MjAwMDAwMDAwMDAw'.
*  xml_string = xml_string &&  'MDAwMDAwMDAwMDAwMDAwMKAXBgVgTAEDB6AOEwwwMDAwMDAwMDAwMDAwcQYDVR0gBGowaDBmBgZg'.
*  xml_string = xml_string &&  'TAECAQ0wXDBaBggrBgEFBQcCARZOaHR0cDovL3B1YmxpY2FjYW8uY2VydGlmaWNhZG9kaWdpdGFs'.
*  xml_string = xml_string &&  'LmNvbS5ici9yZXBvc2l0b3Jpby9kcGMvZGVjbGFyYWNhby1yZmIucGRmMB0GA1UdJQQWMBQGCCsG'.
*  xml_string = xml_string &&  'AQUFBwMCBggrBgEFBQcDBDCBnQYDVR0fBIGVMIGSMEqgSKBGhkRodHRwOi8vd3d3LmNlcnRpZmlj'.
*  xml_string = xml_string &&  'YWRvZGlnaXRhbC5jb20uYnIvcmVwb3NpdG9yaW8vbGNyL3NlcmFzYXJmYnY1LmNybDBEoEKgQIY+'.
*  xml_string = xml_string &&  'aHR0cDovL2xjci5jZXJ0aWZpY2Fkb3MuY29tLmJyL3JlcG9zaXRvcmlvL2xjci9zZXJhc2FyZmJ2'.
*  xml_string = xml_string &&  'NS5jcmwwHQYDVR0OBBYEFCZPZLb/aWD4YEAvRZdsYAxuGBLxMA4GA1UdDwEB/wQEAwIF4DANBgkq'.
*  xml_string = xml_string &&  'hkiG9w0BAQsFAAOCAgEArWg8kpL9jqnxbQ6X5UPOejfwMcyZcXGauONf7PjyLqmXX8s20xS7SYjK'.
*  xml_string = xml_string &&  'Qd4fePoZtXqb16YiYlZx87RXDZTAyunrQtUQI6nGSixhun2s67glYVJC+TSMmXp/7RSZ/9T9HF/d'.
*  xml_string = xml_string &&  'ZJC2dN3Z+LxF1jzx+wj7zic0UsGKpHvfQ8TD/9PWSS/iyA2eFykN+RI4CiHsDfCClpF0qCy16B3S'.
*  xml_string = xml_string &&  'E6p0e2NISqY+aLtwFBRXEgyIyiL89VbocR5GDwxzhJg5dWBIoHNXB58oPY92Q7rfekWJGBuuVPQe'.
*  xml_string = xml_string &&  'Lg2gRpuZBRAQun0zkZy4SbvIPFufXIueo1Tyj6HbgNJjxp/U/cDFhXSf/Fq1Ao+vlqW5Mgn7smk+'.
*  xml_string = xml_string &&  'kWAie9CQnQteN+yESP+EgF0HUBLwVATxmqelY+bqLCgN8kA9pQmhwu7govs6NGsK7QIOOanvv3R9'.
*  xml_string = xml_string &&  '/ESlGb9t3HtaqeetZS4qWyMb/fIifEfP/vQpf2Usp7qHTPFWtqhYdoxlRwuVenPyCMmAKAGBymXh'.
*  xml_string = xml_string &&  '3aqhevTohxEH1cw0NB0G+UNQ/cauW3g0P/uOW8BAjZerooeBUoQkvrSrsCZusD1oISDwt7eJSIJV'.
*  xml_string = xml_string &&  '7QfPOFwOzU3u/Dmppo17j2qesbpqIbG+bzxeczf68u5ZeD+5+f6rYzHHtAyTNscqfEgTOYcKxY6z'.
*  xml_string = xml_string &&  'qWl6h8mDIZaERf0E/oY=</X509Certificate>'.
*  xml_string = xml_string &&  '</X509Data>'.
*  xml_string = xml_string &&  '</KeyInfo>'.
*  xml_string = xml_string &&  '</Signature>'.
*  xml_string = xml_string &&  '</CTe>'.
*  xml_string = xml_string &&  '<protCTe versao="3.00">'.
*  xml_string = xml_string &&  '<infProt>'.
*  xml_string = xml_string &&  '<tpAmb>'.
*  xml_string = xml_string &&  '1</tpAmb>'.
*  xml_string = xml_string &&  '<verAplic>'.
*  xml_string = xml_string &&  '3.00</verAplic>'.
*  xml_string = xml_string &&  '<chCTe>'.
*  xml_string = xml_string &&  '51201277294254001670570000012944191863080620</chCTe>'.
*  xml_string = xml_string &&  '<dhRecbto>'.
*  xml_string = xml_string &&  '2020-12-31T06:30:34-04:00</dhRecbto>'.
*  xml_string = xml_string &&  '<nProt>'.
*  xml_string = xml_string &&  '151200431892356</nProt>'.
*  xml_string = xml_string &&  '<digVal>'.
*  xml_string = xml_string &&  '+hUe8zlqfTEZm13PahNXuyEXoOU=</digVal>'.
*  xml_string = xml_string &&  '<cStat>'.
*  xml_string = xml_string &&  '100</cStat>'.
*  xml_string = xml_string &&  '<xMotivo>'.
*  xml_string = xml_string &&  'Autorizado o uso da CT-e</xMotivo>'.
*  xml_string = xml_string &&  '</infProt>'.
*  xml_string = xml_string &&  '</protCTe>'.
*  xml_string = xml_string &&  '</cteProc>'.


  APPEND 'infNFe'  TO t_element_array.
  APPEND 'infCTe'  TO t_element_array.
  APPEND 'infNF'   TO t_element_array.
  APPEND 'ObsCont' TO t_element_array.

  DATA(_json) = zcl_string=>xml_to_json( i_xml =  xml_string i_element_array = t_element_array ).

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = wg_xml_sefaz.

ENDFORM.

FORM build_info_info_xml.

  DATA: wl_doc_origin  TYPE zde_docs_origin_cte,
        v_obs_cont_tmp TYPE string.

  DATA: lv_formatted_key(54) TYPE c,
        v_qtde_aux           TYPE j_1bnflin-menge,
        v_vlr_aux            TYPE j_1bnflin-netwr,
        vl_cst_icms          TYPE j_1bnflin-netwr,
        vl_dt_aut            TYPE char10,
        vl_hr_aut(14)        TYPE c,
        vl_chave             TYPE string,
        lv_domvalue          TYPE domvalue_l.


  CLEAR: gt_docs_origin[], gs_observacoes, gs_obscont, vl_cst_icms, lv_formatted_key, vl_dt_aut, vl_hr_aut, lv_domvalue, vl_chave .

  CHECK wg_xml_sefaz IS NOT INITIAL.


  "Tag ICMS
  IF  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms00 IS NOT INITIAL .
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms00-cst.    "Situação Tributaria
    vl_cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms00-vbc.
    WRITE vl_cst_icms   TO wg_dacte-icms_vbc DECIMALS 2 LEFT-JUSTIFIED NO-GAP."Base de Calculo
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms00-picms.
    WRITE vl_cst_icms TO wg_dacte-picms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.  "Al.ICMS
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms00-vicms.
    WRITE vl_cst_icms TO wg_dacte-vicms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.  "Valor ICMS
    CLEAR vl_cst_icms.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms20 IS NOT INITIAL.
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms20-cst.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms20-vbc.
    WRITE vl_cst_icms TO  wg_dacte-icms_vbc DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms20-picms.
    WRITE  vl_cst_icms TO  wg_dacte-picms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms20-vicms.
    WRITE  vl_cst_icms TO  wg_dacte-vicms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms45 IS NOT INITIAL.
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms45-cst.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms45-vbc.
    WRITE vl_cst_icms TO  wg_dacte-icms_vbc DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms45-picms.
    WRITE  vl_cst_icms TO  wg_dacte-picms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms45-vicms.
    WRITE  vl_cst_icms TO  wg_dacte-vicms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms60 IS NOT INITIAL.
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms60-cst.
    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms60-vbc.
    WRITE vl_cst_icms TO  wg_dacte-icms_vbc DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms60-picms.
    WRITE  vl_cst_icms TO  wg_dacte-picms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms60-vicms.
    WRITE  vl_cst_icms TO  wg_dacte-vicms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms90 IS NOT INITIAL.
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms90-cst.
    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms90-vbc.
    WRITE vl_cst_icms TO  wg_dacte-icms_vbc DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms90-picms.
    WRITE  vl_cst_icms TO  wg_dacte-picms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

    vl_cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icms90-vicms.
    WRITE  vl_cst_icms TO  wg_dacte-vicms DECIMALS 2 LEFT-JUSTIFIED NO-GAP.
    CLEAR vl_cst_icms.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icmsoutrauf IS NOT INITIAL.
    wg_dacte-cst_icms = wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icmsoutrauf-cst.

  ELSEIF wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icmssn IS NOT INITIAL.
    wg_dacte-cst_icms =  wg_xml_sefaz-cteproc-cte-infcte-imp-icms-icmssn-cst.
  ENDIF.


  CASE wg_dacte-cst_icms.
    WHEN '00'.
      wg_dacte-cst_icms_ds = 'Tributado integralmente'.
    WHEN '10'.
      wg_dacte-cst_icms_ds = 'Tributado e com cobrança do ICMS por substituição tributária'.
    WHEN '20'.
      wg_dacte-cst_icms_ds = 'Com redução de base de cálculo'.
    WHEN '30'.
      wg_dacte-cst_icms_ds = 'Isento ou não tributado e com cobrança do ICMS por substituição tributária'.
    WHEN '40'.
      wg_dacte-cst_icms_ds = 'Isento'.
    WHEN '41'.
      wg_dacte-cst_icms_ds = 'Não tributado'.
    WHEN '50'.
      wg_dacte-cst_icms_ds = 'Suspensão'.
    WHEN '51'.
      wg_dacte-cst_icms_ds = 'Diferimento'.
    WHEN '60'.
      wg_dacte-cst_icms_ds = 'ICMS cobrado anteriormente por substituição tributária'.
    WHEN '70'.
      wg_dacte-cst_icms_ds = 'Com redução de base de cálculo e cobrança do ICMS por substituição tributária'.
    WHEN '90'.
      wg_dacte-cst_icms_ds = 'Outros'.
  ENDCASE.


*  vl_chave   = wg_xml_sefaz-cteproc-cte-infcte-a_id+3(44).
*  WRITE vl_chave TO lv_formatted_key USING EDIT MASK lc_edit_mask.
*  wg_dacte-chave = lv_formatted_key.

  wg_dacte-chave = wg_xml_sefaz-cteproc-cte-infcte-a_id+3(44).

  wg_dacte-qrcodecte = wg_xml_sefaz-cteproc-cte-infctesupl-qrcodcte.


  "Inicio- Fim Prestação
  wg_dacte-ufini   = wg_xml_sefaz-cteproc-cte-infcte-ide-ufini.
  wg_dacte-xmunini = wg_xml_sefaz-cteproc-cte-infcte-ide-xmunini.

  wg_dacte-uffim   = wg_xml_sefaz-cteproc-cte-infcte-ide-uffim.
  wg_dacte-xmunfim = wg_xml_sefaz-cteproc-cte-infcte-ide-xmunfim.

  "CFOP - Natureza Operação
  wg_dacte-cfop  = wg_xml_sefaz-cteproc-cte-infcte-ide-cfop.
  wg_dacte-natop = wg_xml_sefaz-cteproc-cte-infcte-ide-natop.

  CASE wg_xml_sefaz-cteproc-cte-infcte-ide-tpcte.
    WHEN '0'.
      wg_dacte-tp_cte      = 'Normal'.
    WHEN '1'.
      wg_dacte-tp_cte      = 'Complemento de Valores'.
    WHEN '2'.
      wg_dacte-tp_cte      = 'Anulação de Valores'.
    WHEN '3'.
      wg_dacte-tp_cte      = 'Substituto'.
  ENDCASE.

  CASE wg_xml_sefaz-cteproc-cte-infcte-ide-tpserv.
    WHEN '0'.
      wg_dacte-tp_servico  =  'Normal'.
    WHEN '1'.
      wg_dacte-tp_servico  = 'Subcontratação'.
    WHEN '2'.
      wg_dacte-tp_servico  = 'Redespacho'.
    WHEN '3'.
      wg_dacte-tp_servico  = 'Redespacho Intermediário'.
    WHEN '4'.
      wg_dacte-tp_servico  = 'Serviço Vinculado a Multimodal'.
  ENDCASE.

*----------------------------------------------------------------------*
*    enqueue
*----------------------------------------------------------------------*
  "Dados Emitente
  wg_dacte-name1      = wg_xml_sefaz-cteproc-cte-infcte-emit-xnome.
  wg_dacte-name2      = wg_xml_sefaz-cteproc-cte-infcte-emit-xfant.
  wg_dacte-stains     = wg_xml_sefaz-cteproc-cte-infcte-emit-ie.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-cnpj USING EDIT MASK lc_edit_mask_cnpj TO  wg_dacte-cgc.

  wg_dacte-street     = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-xlgr.
  wg_dacte-house_num1 = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-nro.
  wg_dacte-ort01      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-xbairro.
  wg_dacte-ort02      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-cmun.
  wg_dacte-regio      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-uf.

  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-fone USING EDIT MASK lc_edit_mask_fone TO  wg_dacte-telf1.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-cep  USING EDIT MASK lc_edit_mask_cep  TO  wg_dacte-pstlz.

*----------------------------------------------------------------------*
*    extended header information
*----------------------------------------------------------------------*
  gs_prnfehd-access_key = wg_xml_sefaz-cteproc-cte-infcte-a_id+3(44).

*------------------------------------------------------------------------------------------*
* Informações Carga
*------------------------------------------------------------------------------------------*
  DATA: lc_valor  TYPE char100.

  CLEAR: v_qtde_aux, gs_componentes.

  v_vlr_aux = wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infcarga-vcarga.
  WRITE v_vlr_aux TO wg_dacte-vcarga DECIMALS 2 LEFT-JUSTIFIED NO-GAP.

  wg_dacte-propred = wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infcarga-propred.


  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infcarga-infq INTO DATA(wa_infq).

    CLEAR: v_qtde_aux.

    CASE wa_infq-cunid.
      WHEN '01'. "KG
        v_qtde_aux = wa_infq-qcarga.
      WHEN '02'. "TON
        v_qtde_aux = wa_infq-qcarga.
        v_qtde_aux = v_qtde_aux * 1000.
    ENDCASE.

    CASE wa_infq-tpmed.
      WHEN 'PESO BRUTO'.
        WRITE v_qtde_aux TO wg_dacte-peso_bruto_kg DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        lc_valor  = wg_dacte-peso_bruto_kg.
      WHEN 'PESO LIQUIDO'.
        WRITE v_qtde_aux TO wg_dacte-peso_liqui_kg DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        lc_valor  = wg_dacte-peso_liqui_kg.
      WHEN OTHERS.
        IF wg_dacte-peso_bruto_kg IS INITIAL.
          WRITE v_qtde_aux TO wg_dacte-peso_bruto_kg DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        ELSE.
          WRITE v_qtde_aux TO lc_valor DECIMALS 3 LEFT-JUSTIFIED NO-GAP.
        ENDIF.
    ENDCASE.

    IF wg_dacte-comp_nome1 IS INITIAL.
      wg_dacte-comp_nome1 = wa_infq-tpmed.
      wg_dacte-comp_valor1     = lc_valor.
    ELSEIF  wg_dacte-comp_nome2 IS INITIAL.
      wg_dacte-comp_nome2  = wa_infq-tpmed.
      wg_dacte-comp_valor2 = lc_valor.
    ELSEIF wg_dacte-comp_nome3 IS INITIAL.
      wg_dacte-comp_nome3  = wa_infq-tpmed.
      wg_dacte-comp_valor3 = lc_valor.
    ELSEIF  wg_dacte-comp_nome4 IS INITIAL.
      wg_dacte-comp_nome4  = wa_infq-tpmed.
      wg_dacte-comp_valor4 = lc_valor.
    ENDIF.
  ENDLOOP.

  IF wg_dacte-peso_liqui_kg IS INITIAL.
    wg_dacte-peso_liqui_kg = wg_dacte-peso_bruto_kg.
  ENDIF.

*------------------------------------------------------------------------------------------*
* Dados Identificação
*------------------------------------------------------------------------------------------*
  wg_dacte-modal  = wg_xml_sefaz-cteproc-cte-infcte-ide-modal.
  wg_dacte-nfenum = wg_xml_sefaz-cteproc-cte-infcte-ide-nct.
  wg_dacte-series = wg_xml_sefaz-cteproc-cte-infcte-ide-serie.
  wg_dacte-mod    = wg_xml_sefaz-cteproc-cte-infcte-ide-mod.

  CONCATENATE wg_xml_sefaz-cteproc-cte-infcte-ide-dhemi+8(2) '/' wg_xml_sefaz-cteproc-cte-infcte-ide-dhemi+5(2) '/' wg_xml_sefaz-cteproc-cte-infcte-ide-dhemi+0(4)
  INTO wg_dacte-authdate.
  wg_dacte-authtime = wg_xml_sefaz-cteproc-cte-infcte-ide-dhemi+11(8).

  gs_ide_dacte-cuf   = wg_xml_sefaz-cteproc-cte-infcte-ide-cuf.
  gs_ide_dacte-modal = wg_xml_sefaz-cteproc-cte-infcte-ide-modal.
  gs_ide_dacte-nct   = wg_xml_sefaz-cteproc-cte-infcte-ide-nct.

  CLEAR v_vlr_aux.
  v_vlr_aux = wg_xml_sefaz-cteproc-cte-infcte-vprest-vtprest.
  WRITE v_vlr_aux  TO wg_dacte-vl_prest DECIMALS 2 LEFT-JUSTIFIED NO-GAP.

  CLEAR v_vlr_aux.
  v_vlr_aux = wg_xml_sefaz-cteproc-cte-infcte-vprest-vrec.
  WRITE v_vlr_aux  TO wg_dacte-vl_rec DECIMALS 2 LEFT-JUSTIFIED NO-GAP.

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

  CLEAR: gs_inf_modal_aquav-xbalsa.
  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infmodal-aquav-balsa INTO DATA(wa_balsa).
    gs_inf_modal_aquav-xbalsa = zcl_string=>concat( EXPORTING s1 = CONV #( gs_inf_modal_aquav-xbalsa ) s2 = wa_balsa-xbalsa sp = ';' ).
  ENDLOOP.

*------------------------------------------------------------------------------------------*
* Documentos Originarios
*------------------------------------------------------------------------------------------*
  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infdoc-infnfe INTO DATA(wl_inf_nfe).
    wg_dacte-tipo_doc_ref  = 'NFE'.
    wl_doc_origin-tipo_doc = 'NF-e'.
    wl_doc_origin-cnpj_cpf_emit = wl_inf_nfe-chave+6(14).
    WRITE wl_inf_nfe-chave  TO  wl_doc_origin-chave_acesso USING EDIT MASK lc_edit_mask.
    WRITE wl_inf_nfe-infunidtransp-qtdrat TO wl_doc_origin-peso_rateado.
    APPEND wl_doc_origin TO gt_docs_origin.
  ENDLOOP.

****
  CLEAR: wl_doc_origin.
  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-docant-emidocant ASSIGNING FIELD-SYMBOL(<fs_emidocant>).
    READ TABLE <fs_emidocant>-iddocant ASSIGNING FIELD-SYMBOL(<fs_iddocant>)      INDEX 1.
    READ TABLE <fs_iddocant>-iddocantele ASSIGNING FIELD-SYMBOL(<fs_iddocantele>) INDEX 1.

    wg_dacte-tipo_doc_ref          = 'NFE'.
    wl_doc_origin-tipo_doc         = 'CT-e'.
    wl_doc_origin-cnpj_cpf_emit  = <fs_emidocant>-cnpj.

    WRITE <fs_iddocantele>-chcte TO wl_doc_origin-chave_acesso USING EDIT MASK lc_edit_mask.
    APPEND wl_doc_origin TO gt_docs_origin.
  ENDLOOP.
***
  LOOP AT wg_xml_sefaz-cteproc-cte-infcte-infctenorm-infdoc-infnf INTO DATA(wl_inf_nf).
    CLEAR: wl_doc_origin.

    wg_dacte-tipo_doc_ref    = 'NFF'.
    wl_doc_origin-tipo_doc   = 'NF'.

    wl_doc_origin-nr_doc = wl_inf_nf-ndoc.
    wl_doc_origin-serie  = wl_inf_nf-serie.

    IF wg_xml_sefaz-cteproc-cte-infcte-rem-cpf IS NOT INITIAL.
      wl_doc_origin-cnpj_cpf_emit  = wg_xml_sefaz-cteproc-cte-infcte-rem-cpf.
    ELSE.
      wl_doc_origin-cnpj_cpf_emit  = wg_xml_sefaz-cteproc-cte-infcte-rem-cnpj.
    ENDIF.

    WRITE wl_inf_nfe-infunidtransp-qtdrat TO wl_doc_origin-peso_rateado.

    APPEND wl_doc_origin TO gt_docs_origin.
  ENDLOOP.

*------------------------------------------------------------------------------------------*
*Protocolo de Autorização
*------------------------------------------------------------------------------------------*

  CONCATENATE wg_xml_sefaz-cteproc-protcte-infprot-dhrecbto+8(2) '/' wg_xml_sefaz-cteproc-protcte-infprot-dhrecbto+5(2) '/' wg_xml_sefaz-cteproc-protcte-infprot-dhrecbto+0(4)
    INTO vl_dt_aut.

  vl_hr_aut = wg_xml_sefaz-cteproc-protcte-infprot-dhrecbto+11(8).

  CONCATENATE  wg_xml_sefaz-cteproc-protcte-infprot-nprot vl_dt_aut  vl_hr_aut
   INTO wg_dacte-protocolo_aut SEPARATED BY space.



  "Dados Remetente
  wg_dacte-rem_name1  = wg_xml_sefaz-cteproc-cte-infcte-rem-xnome.
  wg_dacte-rem_name2  = wg_xml_sefaz-cteproc-cte-infcte-rem-xfant.
  wg_dacte-rem_stains = wg_xml_sefaz-cteproc-cte-infcte-rem-ie.
  IF wg_xml_sefaz-cteproc-cte-infcte-rem-cnpj IS NOT INITIAL.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-rem_cgc.
  ELSE.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-rem_cgc.
  ENDIF.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-rem_telf1.
  wg_dacte-rem_stras  = wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xlgr. "Endereco
  wg_dacte-rem_ort02  = wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xbairro. "Bairro
  wg_dacte-rem_ort01  = wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xmun. " Municipio
  WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-cep USING EDIT MASK lc_edit_mask_cep TO wg_dacte-rem_pstlz . " CEP
  wg_dacte-rem_regio  = wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-uf. "Estado
  wg_dacte-rem_landx  = wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xpais. "Pais


  "Dados Expedidor
  wg_dacte-exped_name1  = wg_xml_sefaz-cteproc-cte-infcte-exped-xnome.
  wg_dacte-exped_name2  = wg_xml_sefaz-cteproc-cte-infcte-exped-xfant.
  wg_dacte-exped_stains = wg_xml_sefaz-cteproc-cte-infcte-exped-ie.
  IF wg_xml_sefaz-cteproc-cte-infcte-exped-cnpj IS NOT INITIAL.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-exped_cgc.
  ELSE.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-exped_cgc.
  ENDIF.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-exped_telf1.
  wg_dacte-exped_stras  = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xlgr.
  wg_dacte-exped_ort02  = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xbairro.
  wg_dacte-exped_ort01  = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xmun.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-cep USING EDIT MASK lc_edit_mask_cep TO  wg_dacte-exped_pstlz.
  wg_dacte-exped_regio  = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-uf.
  wg_dacte-exped_landx  = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xpais.


  "Dados Recebedor
  wg_dacte-receb_name1  = wg_xml_sefaz-cteproc-cte-infcte-receb-xnome.
  wg_dacte-receb_name2  = wg_xml_sefaz-cteproc-cte-infcte-receb-xfant.
  wg_dacte-receb_stains = wg_xml_sefaz-cteproc-cte-infcte-receb-ie.
  IF wg_xml_sefaz-cteproc-cte-infcte-receb-cnpj IS NOT INITIAL.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-receb_cgc.
  ELSE.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-receb_cgc.
  ENDIF.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-receb_telf1.
  wg_dacte-receb_strass = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xlgr.
  wg_dacte-receb_ort02  = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xbairro.
  wg_dacte-receb_ort01  = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xmun.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-cep USING EDIT MASK lc_edit_mask_cep TO wg_dacte-receb_pstlz.
  wg_dacte-receb_regio  = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-uf.
  wg_dacte-receb_landx  = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xpais.

  "Dados Destinatario
  wg_dacte-dest_name1  = wg_xml_sefaz-cteproc-cte-infcte-dest-xnome.
  wg_dacte-dest_stains = wg_xml_sefaz-cteproc-cte-infcte-dest-ie.
  IF wg_xml_sefaz-cteproc-cte-infcte-dest-cnpj IS NOT INITIAL.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-cnpj USING EDIT MASK lc_edit_mask_cnpj TO  wg_dacte-dest_cgc.
  ELSE.
    WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-cpf USING EDIT MASK lc_edit_mask_cpf TO  wg_dacte-dest_cgc.
  ENDIF.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-dest_telf1.
  wg_dacte-dest_stras  = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xlgr.
  wg_dacte-dest_ort02  = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xbairro.
  wg_dacte-dest_ort01  = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xmun.
  WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-cep USING EDIT MASK lc_edit_mask_cep TO  wg_dacte-dest_pstlz.
  wg_dacte-dest_regio  = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-uf.
  wg_dacte-dest_landx  = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xpais.

  IF wg_xml_sefaz-cteproc-cte-infcte-ide-toma4 IS NOT INITIAL. "BUG 60991 - IR064208 - aoenning
    DATA(ws_tomador) = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-toma.
  ELSE.
    ws_tomador = wg_xml_sefaz-cteproc-cte-infcte-ide-toma3-toma.
  ENDIF.

  CASE ws_tomador."wg_xml_sefaz-cteproc-cte-infcte-ide-toma3-toma. "BUG 60991 - IR064208 - aoenning
    WHEN 0.
      "Dados Remetente
      wg_dacte-toma_name1      =  wg_xml_sefaz-cteproc-cte-infcte-rem-xnome.
      wg_dacte-toma_name2      =  wg_xml_sefaz-cteproc-cte-infcte-rem-xfant.
      wg_dacte-toma_stains     =  wg_xml_sefaz-cteproc-cte-infcte-rem-ie.
      IF wg_xml_sefaz-cteproc-cte-infcte-rem-cnpj IS NOT INITIAL.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-toma_cgc.
      ELSE.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-toma_cgc.
      ENDIF.
      WRITE  wg_xml_sefaz-cteproc-cte-infcte-rem-fone USING EDIT MASK lc_edit_mask_fone TO  wg_dacte-toma_telf1 .
      wg_dacte-toma_stras      =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xlgr.
      wg_dacte-toma_house_num1 =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-nro.
      wg_dacte-toma_ort02      =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xbairro.
      wg_dacte-toma_ort01      =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xmun.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-cep USING EDIT MASK lc_edit_mask_cep TO  wg_dacte-toma_pstlz.
      wg_dacte-toma_regio      =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-uf.
      wg_dacte-toma_landx      =  wg_xml_sefaz-cteproc-cte-infcte-rem-enderreme-xpais.

    WHEN 1.
      "Dados Expedidor
      wg_dacte-toma_name1      = wg_xml_sefaz-cteproc-cte-infcte-exped-xnome.
      wg_dacte-toma_name2      = wg_xml_sefaz-cteproc-cte-infcte-exped-xfant.
      wg_dacte-toma_stains     = wg_xml_sefaz-cteproc-cte-infcte-exped-ie.
      IF wg_xml_sefaz-cteproc-cte-infcte-exped-cnpj IS NOT INITIAL.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-cnpj USING  EDIT MASK lc_edit_mask_cnpj TO wg_dacte-toma_cgc.
      ELSE.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-cpf USING  EDIT MASK lc_edit_mask_cpf TO wg_dacte-toma_cgc.
      ENDIF.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-fone USING EDIT MASK lc_edit_mask_fone TO  wg_dacte-toma_telf1.
      wg_dacte-toma_stras      = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xlgr.
      wg_dacte-toma_house_num1 = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-nro.
      wg_dacte-toma_ort02      = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xbairro.
      wg_dacte-toma_ort01      = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xmun.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-cep USING EDIT MASK lc_edit_mask_cep TO wg_dacte-toma_pstlz.
      wg_dacte-toma_regio      = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-uf.
      wg_dacte-toma_landx      = wg_xml_sefaz-cteproc-cte-infcte-exped-enderexped-xpais.
    WHEN 2.
      "Dados Recebedor
      wg_dacte-toma_name1      = wg_xml_sefaz-cteproc-cte-infcte-receb-xnome.
      wg_dacte-toma_name2      = wg_xml_sefaz-cteproc-cte-infcte-receb-xfant.
      wg_dacte-toma_stains     = wg_xml_sefaz-cteproc-cte-infcte-receb-ie.
      IF wg_xml_sefaz-cteproc-cte-infcte-receb-cnpj IS NOT INITIAL.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-toma_cgc.
      ELSE.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-toma_cgc.
      ENDIF.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-toma_telf1.
      wg_dacte-toma_stras      = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xlgr.
      wg_dacte-toma_house_num1 = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-nro.
      wg_dacte-toma_ort02      = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xbairro.
      wg_dacte-toma_ort01      = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xmun.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-cep USING EDIT MASK lc_edit_mask_cep TO wg_dacte-toma_pstlz.
      wg_dacte-toma_regio      = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-uf.
      wg_dacte-toma_landx      = wg_xml_sefaz-cteproc-cte-infcte-receb-enderreceb-xpais.

    WHEN 3.
      "Dados Destinatario
      wg_dacte-toma_name1      = wg_xml_sefaz-cteproc-cte-infcte-dest-xnome.
      wg_dacte-toma_stains     = wg_xml_sefaz-cteproc-cte-infcte-dest-ie.
      IF wg_xml_sefaz-cteproc-cte-infcte-dest-cnpj IS NOT INITIAL.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-cnpj USING EDIT MASK lc_edit_mask_cnpj TO  wg_dacte-toma_cgc.
      ELSE.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-cpf USING EDIT MASK lc_edit_mask_cpf TO  wg_dacte-toma_cgc.
      ENDIF.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-toma_telf1.

      wg_dacte-toma_stras      = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xlgr.
      wg_dacte-toma_house_num1 = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-nro.
      wg_dacte-toma_ort02      = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xbairro.
      wg_dacte-toma_ort01      = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xmun.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-cep USING EDIT MASK lc_edit_mask_cep TO  wg_dacte-toma_pstlz.
      wg_dacte-toma_regio      = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-uf.
      wg_dacte-toma_landx      = wg_xml_sefaz-cteproc-cte-infcte-dest-enderdest-xpais.

    WHEN 4.
      "Dados Tomador
      wg_dacte-toma_name1  = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-xnome.
      wg_dacte-toma_name2  = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-xfant.
      wg_dacte-toma_stains = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-ie.

      IF wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-cnpj IS NOT INITIAL.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-cnpj USING EDIT MASK lc_edit_mask_cnpj TO wg_dacte-toma_cgc.
      ELSE.
        WRITE wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-cpf USING EDIT MASK lc_edit_mask_cpf TO wg_dacte-toma_cgc.
      ENDIF.

      WRITE wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-fone USING EDIT MASK lc_edit_mask_fone TO wg_dacte-toma_telf1.
      wg_dacte-toma_stras      = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-xlgr.
      wg_dacte-toma_house_num1 = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-nro.
      wg_dacte-toma_ort02      = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-xbairro.
      wg_dacte-toma_ort01      = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-xmun.
      WRITE wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-cep USING EDIT MASK lc_edit_mask_cep TO wg_dacte-toma_pstlz.
      wg_dacte-toma_regio      = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-uf.
      wg_dacte-toma_landx      = wg_xml_sefaz-cteproc-cte-infcte-ide-toma4-endertoma-xpais.

  ENDCASE.

  lv_domvalue = ws_tomador. "Tipo de tomador do serviço
  PERFORM append_domtext USING lc_service_taker
                               lv_domvalue.
ENDFORM.


FORM f_teste.

  DATA: otfdata1      TYPE tsfotf,
        vqtde         TYPE j_1bnflin-menge,
        wl_doc_origin TYPE zde_docs_origin_cte.


  tnapr-sform = 'ZBRCTE_DACTE'.

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

  CHECK 1 = 2.

  PERFORM entry USING 0 '100'.

ENDFORM.

*FORM prepare_cross_model.
**  DATA:
**    issuer_typ   TYPE j_1bpartyp,
**    issuer_id    TYPE j_1bparid,
**    issuer_parvw TYPE j_1bparvw.
**
**  DATA:
**    dest_typ   TYPE j_1bpartyp,
**    dest_id    TYPE j_1bparid,
**    dest_parvw TYPE j_1bparvw.
**
**  DATA:
**    ls_sadr         TYPE sadr,
**    lv_cgc          TYPE j_1bcgc,
**    ls_branch       TYPE j_1bbranch,
**    ls_addr1_val    TYPE addr1_val,
**    ls_branch_innad TYPE j_1binnad.
**
**  DATA:
**    ls_ext_header TYPE j_1bindoc,
**    ls_access_key TYPE j_1b_nfe_access_key.
**
**  DATA:
**    lv_tim TYPE uzeit,
**    lv_dat TYPE datum.
**
**  MOVE nast-objky TO gv_docnum.
*
**----------------------------------------------------------------------*
**    enqueue
**----------------------------------------------------------------------*
*  "Dados Emitente
*  wg_dacte-name1      = wg_xml_sefaz-cteproc-cte-infcte-emit-xnome.
*  wg_dacte-name2      = wg_xml_sefaz-cteproc-cte-infcte-emit-xfant.
*  wg_dacte-stains     = wg_xml_sefaz-cteproc-cte-infcte-emit-ie.
*  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-cnpj USING EDIT MASK lc_edit_mask_cnpj TO  wg_dacte-cgc.
*
*  wg_dacte-street     = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-xlgr.
*  wg_dacte-house_num1 = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-nro.
*  wg_dacte-ort01      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-xbairro.
*  wg_dacte-ort02      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-cmun.
*  wg_dacte-regio      = wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-uf.
*
*  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-fone USING EDIT MASK lc_edit_mask_fone TO  wg_dacte-telf1.
*  WRITE wg_xml_sefaz-cteproc-cte-infcte-emit-enderemit-cep  USING EDIT MASK lc_edit_mask_cep  TO  wg_dacte-pstlz.
*
**----------------------------------------------------------------------*
**    extended header information
**----------------------------------------------------------------------*
*
*  gs_prnfehd-access_key = wg_xml_sefaz-cteproc-cte-infcte-a_id+3(44).
*
**  CLEAR gs_prnfehd.
**  MOVE-CORRESPONDING gs_nfeactive TO ls_access_key.
**  MOVE ls_access_key TO gs_prnfehd-access_key.
**
**  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
**    EXPORTING
**      nf_header   = gs_nfdoc
**    IMPORTING
**      ext_header  = ls_ext_header
**    TABLES
**      nf_item     = gt_nflin
**      nf_item_tax = gt_nfstx
***     EXT_ITEM    =
***     EXT_TOTAL_TAX =
**    .
**  MOVE ls_ext_header-nfnet TO gs_prnfehd-nfnet.
**  MOVE ls_ext_header-nftot TO gs_prnfehd-nftot.
**
**  IF gs_nfdoc-direct NE '1' OR
**     gs_nfdoc-entrad EQ 'X'.
**    CONVERT TIME STAMP gs_nfdoc-cre_timestamp
**          TIME ZONE ls_sadr-tzone
**          INTO DATE lv_dat
**               TIME lv_tim.
**    IF sy-subrc = 0.
**      gs_prnfehd-credat_issuer = lv_dat.
**      gs_prnfehd-cretim_issuer = lv_tim.
**    ENDIF.
**  ENDIF.
*
*ENDFORM.                    "prepare_cross_model

FORM get_dados_xml_cte USING p_xml_doc TYPE string.

  DATA: t_element_array TYPE zde_element_array_t,
        lva_xml_doc     TYPE string.

  CLEAR: wg_xml_sefaz, lva_xml_doc.

  IF p_xml_doc IS NOT INITIAL.

    lva_xml_doc = p_xml_doc.

  ELSE.

    MOVE nast-objky TO gv_docnum.

    CHECK gv_docnum IS NOT INITIAL.


    TRY .
        zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = gv_docnum
          )->set_registro( EXPORTING i_docnum       =  gv_docnum
                                     i_sem_bloqueio = abap_true
          )->get_ck_autorizado_uso(
          )->get_xml_grc( IMPORTING e_xml_string = lva_xml_doc ).
      CATCH zcx_doc_eletronico INTO DATA(ex_doc).    " .
        ex_doc->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.

  ENDIF.

  CHECK lva_xml_doc IS NOT INITIAL.

  APPEND 'infNFe'       TO t_element_array.
  APPEND 'infCTe'       TO t_element_array.
  APPEND 'infNF'        TO t_element_array.
  APPEND 'ObsCont'      TO t_element_array.
  APPEND 'balsa'        TO t_element_array.
  APPEND 'infQ'         TO t_element_array.
  APPEND 'emiDocAnt'    TO t_element_array.
  APPEND 'idDocAnt'     TO t_element_array.
  APPEND 'idDocAntEle'  TO t_element_array.

  DATA(_json) = zcl_string=>xml_to_json( i_xml           = lva_xml_doc
                                         i_element_array =  t_element_array ).

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = wg_xml_sefaz.

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
    lt_dd07v     TYPE TABLE OF dd07v,
    ls_dd07v     TYPE dd07v,
    lv_rc        TYPE sy-subrc,
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
    ls_j_1bagnt  TYPE j_1bagnt,
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
    ls_atl1t     TYPE j_1batl1t,
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


FORM printing CHANGING otfdata TYPE tsfotf.

  DATA: t_job_output_info TYPE  ssfcrescl,
        fm_name           TYPE rs38l_fnam.


  tnapr-sform = 'ZBRCTE_DACTE'.

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
      control_parameters   = control_parameters
      output_options       = output_options
      user_settings        = ''
      s_destination        = gs_destination
      s_issuer             = gs_issuer
      is_doc               = gs_nfdoc
      is_additional_fields = gs_prnfehd
      is_lin               = gs_lin
      is_active            = gs_nfeactive
      is_issuer            = gs_issuer
      is_goods_sender      = gs_goods_sender
      is_destination       = gs_destination
      is_cargo_dispatcher  = gs_cargo_dispatcher
      is_cargo_recipient   = gs_cargo_recipient
      is_service_taker     = gs_service_taker
      i_obscont            = gs_obscont
      i_observacoes        = gs_observacoes
      i_ide_dacte          = gs_ide_dacte
      i_inf_modal_aquav    = gs_inf_modal_aquav
      i_dados_cte          = wg_dacte
      i_componentes        = gs_componentes
    IMPORTING
      job_output_info      = t_job_output_info
    TABLES
      it_doc_ref           = gt_cte_docref
      it_tax               = gt_prnfestx
      it_carrier           = gt_cte_res
      it_text_fields       = gt_prnfetext
      it_docs_origin       = gt_docs_origin
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  IF sy-subrc = 0.
    MOVE t_job_output_info-otfdata TO otfdata.
  ENDIF.

ENDFORM.                    "printing

*FORM prepare_model57 .
*
*  CONSTANTS:
*    lc_transptn_mode TYPE domname VALUE 'J_1BCTE_TRANSPTN_MODE',
*    lc_service_taker TYPE domname VALUE 'J_1BCTE_SERVICE_TAKER',
*    lc_cttype        TYPE domname VALUE 'J_1BCTTYPE',
*    lc_serv_tp       TYPE domname VALUE 'J_1BCTE_SERV_TP'.
*
*  CONSTANTS:
*    lc_icms TYPE j_1btaxgrp VALUE 'ICMS',
*    lc_icst TYPE j_1btaxgrp VALUE 'ICST'.
*
*  DATA:
*    ls_partner  TYPE j_1bnfnad,
*    ls_nfcpd    TYPE j_1bnfcpd,
*    ls_adr_pr   TYPE j_1bprnfeinnad,
*    ls_nfstx    TYPE j_1bnfstx,
*    ls_prnfestx TYPE j_1bprnfestx,
*    ls_j_1baj   TYPE j_1baj.
*
*  DATA:
*    lv_domvalue    TYPE domvalue_l,
*    lv_domtext(60) TYPE c,
*    lv_tpcte       TYPE c.
*
*
** domain texts
*  lv_domvalue =  gs_nfdoc-transp_mode.
*  PERFORM append_domtext USING lc_transptn_mode
*                               lv_domvalue.
*
*  lv_domvalue = gs_nfdoc-cte_serv_taker.
*  PERFORM append_domtext USING lc_service_taker
*                               lv_domvalue.
*
**  LV_DOMVALUE = GS_NFDOC-SERV_TP.
*  lv_domvalue = wg_xml_sefaz-cteproc-cte-infcte-ide-tpserv.
*  PERFORM append_domtext USING lc_serv_tp
*                               lv_domvalue.
*
** table texts
*  PERFORM append_cfoptext.
*
*  PERFORM append_taxsittext.
*
*  PERFORM append_txjurtext.
*ENDFORM.                    "prepare_model57


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
