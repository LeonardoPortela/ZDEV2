FUNCTION-POOL zxml_simetrya MESSAGE-ID zsimetrya.

* Definição de tabelas e estruturas necessárias para obtenção dos dados da nota fiscal
* através da execução da função J_1B_NFDOC_DOCUMENT_READ
* Tipos:
TYPES: y_j_j1bnfdoc TYPE j_1bnfdoc,
       y_j_1bnfnad  TYPE j_1bnfnad,
       y_j_1bnflin  TYPE j_1bnflin,
       y_j_1bnfstx  TYPE j_1bnfstx,
       y_j_1bnfftx  TYPE j_1bnfftx,
       y_j_1bnfref  TYPE j_1bnfref.

TYPES: BEGIN OF y_vbfa,
         vbeln        TYPE vbfa-vbeln,
         posnn        TYPE vbfa-posnn,
         vbeln_35(35) TYPE c,
       END OF y_vbfa,

       BEGIN OF y_vbfa_docm,
         vbeln        TYPE vbfa-vbeln,
         mjahr        TYPE vbfa-mjahr,
         posnn        TYPE vbfa-posnn,
         vbeln_35(35) TYPE c,
       END OF y_vbfa_docm,

       BEGIN OF y_lips,
         vbeln        TYPE lips-vbeln,
         vgbel        TYPE lips-vgbel,
         vgpos        TYPE lips-vgpos,
         werks        TYPE lips-werks,
         vbeln_16(16) TYPE c,
       END OF y_lips,

       BEGIN OF y_t173t,
         vsart TYPE t173t-vsart,
         bezei TYPE t173t-bezei,
       END OF y_t173t,

       BEGIN OF y_lin,
         docnum TYPE j_1bnflin-docnum,
       END OF y_lin,

       BEGIN OF y_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         gjahr TYPE ekbe-gjahr,
         belnr TYPE ekbe-belnr,
         xblnr TYPE ekbe-xblnr,
       END OF y_ekbe,

       BEGIN OF y_rbkp,
         belnr TYPE rbkp-belnr,
         gjahr TYPE rbkp-gjahr,
       END OF y_rbkp,

       BEGIN OF y_reflin,
         refkey(35) TYPE c,
       END OF y_reflin,

       BEGIN OF y_j_1bbranch,
         bukrs    TYPE j_1bbranch-bukrs,
         branch   TYPE j_1bbranch-branch,
         industry TYPE j_1bbranch-industry,
       END OF y_j_1bbranch,

       BEGIN OF y_t052,
         zterm TYPE t052-zterm,
       END OF y_t052,

       BEGIN OF y_j_1btreg_city,
         taxjurcode TYPE j_1btreg_city-taxjurcode,
       END OF y_j_1btreg_city,

       BEGIN OF y_j_1bnfe_active,
         regio   TYPE j_1bnfe_active-regio,
         nfyear  TYPE j_1bnfe_active-nfyear,
         nfmonth TYPE j_1bnfe_active-nfmonth,
         stcd1   TYPE j_1bnfe_active-stcd1,
         model   TYPE j_1bnfe_active-model,
         serie   TYPE j_1bnfe_active-serie,
         nfnum9  TYPE j_1bnfe_active-nfnum9,
         docnum9 TYPE j_1bnfe_active-docnum9,
         cdv     TYPE j_1bnfe_active-cdv,
       END OF y_j_1bnfe_active,

       BEGIN OF y_end,
         adrnr      TYPE kna1-adrnr,
         city2      TYPE adrc-city2,                "Bairro
         post_code1 TYPE adrc-post_code1,           "CEP
         street     TYPE adrc-street,               "Rua
         house_num1 TYPE adrc-house_num1,           "Número
         region     TYPE adrc-region,               "Estado
       END OF y_end,

       BEGIN OF y_doctos,
         name       TYPE j_1bbranch-name,
         state_insc TYPE j_1bbranch-state_insc,
         stcd1      TYPE j_1bbranch-stcd1,
         stcd2      TYPE j_1bbranch-stcd2,
       END OF y_doctos,

       BEGIN OF y_kna1,
         name1 TYPE kna1-name1,
         stcd1 TYPE kna1-stcd1,    "CNPJ
         stcd2 TYPE kna1-stcd2,    "CPF
         stcd3 TYPE kna1-stcd3,    "IE
       END OF y_kna1,

       BEGIN OF y_lfa1,
         name1  TYPE lfa1-name1,
         stcd1  TYPE lfa1-stcd1,    "CNPJ
         stcd2  TYPE lfa1-stcd2,    "CPF
         stcd3  TYPE lfa1-stcd3,    "IE
         bahns  TYPE lfa1-bahns,    "Registro Nacional dos Transp.de Cargas
         region TYPE adrc-region,   "UF
         adrnr  TYPE lfa1-adrnr,
       END OF y_lfa1,

       BEGIN OF y_vbap,
         vbeln TYPE vbap-vbeln,
         werks TYPE vbap-werks,
         vstel TYPE vbap-vstel,
       END OF y_vbap,

       BEGIN OF y_pc_veiculo,
         placa      TYPE c LENGTH 7,
         uf         TYPE c LENGTH 2,

         placa_car1 TYPE c LENGTH 7,
         uf_car1    TYPE c LENGTH 2,

         placa_car2 TYPE c LENGTH 7,
         uf_car2    TYPE c LENGTH 2,

         placa_car3 TYPE c LENGTH 7,
         uf_car3    TYPE c LENGTH 2,

         codigo_mot TYPE lfa1-lifnr,
         cpf_mot    TYPE lfa1-stcd2,
         nome_mot   TYPE lfa1-name1,


         rntc       TYPE c LENGTH 20,
       END OF y_pc_veiculo,

       BEGIN OF y_vtpa,
         parvw       TYPE vtpa-parvw,
         cli_for(10) TYPE c,
         name1       TYPE lfa1-name1,
         stcd1       TYPE lfa1-stcd1,
         stcd2       TYPE lfa1-stcd2,
         stcd3       TYPE lfa1-stcd3,
       END OF y_vtpa.

"Informações do status da NF-e
DATA: docstat_j_1bnfe_active TYPE j_1bnfe_active.


* Tabelas interna:
DATA: ti_partner       TYPE TABLE OF y_j_1bnfnad,
      ti_item          TYPE TABLE OF y_j_1bnflin,
      ti_item_tax      TYPE TABLE OF y_j_1bnfstx,
      ti_header_msg    TYPE TABLE OF y_j_1bnfftx,
      ti_refer_msg     TYPE TABLE OF y_j_1bnfref,
      ti_header_p      TYPE TABLE OF y_j_j1bnfdoc,
      ti_partner_p     TYPE TABLE OF y_j_1bnfnad,
      ti_item_p        TYPE TABLE OF y_j_1bnflin,
      ti_item_tax_p    TYPE TABLE OF y_j_1bnfstx,
      ti_header_msg_p  TYPE TABLE OF y_j_1bnfftx,
      ti_refer_msg_p   TYPE TABLE OF y_j_1bnfref,
      ti_vbrp          TYPE TABLE OF vbrp,
      ti_vbrp_p        TYPE TABLE OF vbrp,
      ti_vbak          TYPE TABLE OF vbak,
      ti_vtpa          TYPE TABLE OF y_vtpa,
      ti_vbap          TYPE TABLE OF y_vbap,
      ti_vbap_p        TYPE TABLE OF y_vbap,
      ti_vttp          TYPE TABLE OF vttp,
      ti_lips          TYPE TABLE OF y_lips,
      ti_vbfap         TYPE TABLE OF y_vbfa,
      ti_vbfap_docm    TYPE TABLE OF y_vbfa_docm,
      ti_lin           TYPE TABLE OF y_lin,
      ti_ekbe          TYPE TABLE OF y_ekbe,
      ti_rbkp          TYPE TABLE OF y_rbkp,
      ti_reflin        TYPE TABLE OF y_reflin,
      ti_t001w         TYPE TABLE OF t001w,
      ti_vfkp          TYPE TABLE OF vfkp,
      ti_konv          TYPE TABLE OF konv,
      wa_komv          TYPE komv,
      ti_t052          TYPE TABLE OF y_t052,
      ti_j_1btreg_city TYPE TABLE OF y_j_1btreg_city,
      ti_znota_import  TYPE TABLE OF znota_import.


DATA: vg_limpo TYPE string.



DATA: BEGIN OF ti_line OCCURS 0.
        INCLUDE STRUCTURE tline.
DATA: END OF ti_line.

* Estrutura:
DATA: wk_header         TYPE j_1bnfdoc,
      wk_header_p       TYPE j_1bnfdoc,
      st_partner        TYPE y_j_1bnfnad,
      st_partner_p      TYPE y_j_1bnfnad,
      st_item_p         TYPE y_j_1bnflin,
      st_vbrp           TYPE vbrp,
      st_vbrp_p         TYPE vbrp,
      st_vbrk           TYPE vbrk,
      st_vbrk_p         TYPE vbrk,
      st_vbak           TYPE vbak,
      st_vtpa           TYPE y_vtpa,
      st_vbap           TYPE y_vbap,
      st_vttk           TYPE vttk,
      st_t173t          TYPE y_t173t,
      st_tdlnr          TYPE lfa1,
      st_ttds           TYPE ttds,
      st_t001w          TYPE t001w,
      st_j_1bbranch     TYPE y_j_1bbranch,
      st_rbkp           TYPE y_rbkp,
      st_reflin         TYPE y_reflin,
      st_lin            TYPE y_lin,
      st_tvro           TYPE tvro,
      st_vfkp           TYPE vfkp,
      st_konv           TYPE konv,
      st_zlest0002      TYPE zlest0002,
      st_j_1btreg_city  TYPE y_j_1btreg_city,
      st_j_1bnfe_active TYPE y_j_1bnfe_active,
      st_end            TYPE y_end,
      st_kna1           TYPE y_kna1,
      st_lfa1           TYPE y_lfa1,
      st_doctos         TYPE y_doctos,
      st_line           TYPE tline,
      st_pc_veiculo     TYPE y_pc_veiculo,
      st_vbfa_docm      TYPE y_vbfa_docm,
      wa_znota_import   TYPE znota_import.

* Campos
DATA: vg_cpgto         TYPE c,
      vg_consignatario TYPE c,
      vg_dta_ent       TYPE sy-datum,
      vg_dt_hora       TYPE c LENGTH 19,
      vg_pstlz(8)      TYPE n,
      vg_parid_ag      TYPE j_1bnfnad-parid,
      vg_parid_rg      TYPE j_1bnfnad-parid,
      vg_parid         TYPE j_1bnfdoc-parid,
      vg_werks         TYPE j_1bnflin-werks,
      vg_xml_versao    TYPE j_1bnfexmlversion,
      vg_venda_merc_ro TYPE char01,
      vg_venda_merc_ma TYPE char01,
      vg_venda_merc_to TYPE char01,
      vg_venda_merc_pi TYPE char01,
      vg_venda_merc_mt TYPE char01,
      vg_zona_franca   TYPE char01,
      vg_ambiente      TYPE zamb_homolog-ambiente,
      xml_cte_1_04     TYPE char01.
"Constantes para NFe
CONSTANTS: padrao            TYPE c LENGTH 36 VALUE '?xml version="1.0" encoding="UTF-8"?',
           intgnfe           TYPE c LENGTH 07 VALUE 'IntgNFe',

           " Tag de identificação de nota fiscal
           ide               TYPE c LENGTH 03 VALUE 'ide',
           natop             TYPE c LENGTH 05 VALUE 'natOp',
           indpag            TYPE c LENGTH 06 VALUE 'indPag',
           autxml            TYPE c LENGTH 06 VALUE 'autXML',
           serie             TYPE c LENGTH 05 VALUE 'serie',
           nnf               TYPE c LENGTH 03 VALUE 'nNF',
           demi              TYPE c LENGTH 05 VALUE 'dhEmi',
           tpnf              TYPE c LENGTH 04 VALUE 'tpNF',
           cmunfg            TYPE c LENGTH 06 VALUE 'cMunFG',
           finnfe            TYPE c LENGTH 06 VALUE 'finNFe',
           dsai              TYPE c LENGTH 08 VALUE 'dhSaiEnt',
           hsaient           TYPE c LENGTH 07 VALUE 'hSaiEnt',
           indfinal          TYPE c LENGTH 08 VALUE 'indFinal',
           indpres           TYPE c LENGTH 07 VALUE 'indPres',
           indietoma         TYPE c LENGTH 09 VALUE 'indIEToma', "CS2017002143 CT-e 3.0 Nova Tag

           "Tag de informações das NFe referenciadas
           nfref             TYPE c LENGTH 05 VALUE 'NFref',
           refnfe            TYPE c LENGTH 06 VALUE 'refNFe',
           refnf             TYPE c LENGTH 06 VALUE 'refNF',
           siglauf           TYPE c LENGTH 07 VALUE 'siglaUF',
           cuf               TYPE c LENGTH 03 VALUE 'cUF',
           refaamm           TYPE c LENGTH 04 VALUE 'AAMM',
           refcnpj           TYPE c LENGTH 04 VALUE 'CNPJ',
           refmod            TYPE c LENGTH 03 VALUE 'Mod',
           refserie          TYPE c LENGTH 05 VALUE 'Serie',
           refnnf            TYPE c LENGTH 03 VALUE 'nNF',
           "Tag de informações de NFe ref. produtor
           refnfp            TYPE c LENGTH 06 VALUE 'refNFP',
           refnfpsiglauf     TYPE c LENGTH 07 VALUE 'siglaUF',
           refnfpaamm        TYPE c LENGTH 04 VALUE 'AAMM',
           refnfpcnpj        TYPE c LENGTH 04 VALUE 'CNPJ',
           refnfpcpf         TYPE c LENGTH 03 VALUE 'CPF',
           refnfpie          TYPE c LENGTH 03 VALUE 'IE',
           refnfpmod         TYPE c LENGTH 03 VALUE 'mod',
           refnfpserie       TYPE c LENGTH 05 VALUE 'serie',
           refnfpnnf         TYPE c LENGTH 03 VALUE 'nNF',

           "Tag de Emitente
           emit              TYPE c LENGTH 04 VALUE 'emit',
           cnpj              TYPE c LENGTH 04 VALUE 'CNPJ',
           ie                TYPE c LENGTH 02 VALUE 'IE',
           iest              TYPE c LENGTH 04 VALUE 'IEST',

           "Tag de destino
           dest              TYPE c LENGTH 04 VALUE 'dest',
           destxnome         TYPE c LENGTH 05 VALUE 'xNome',
           destcnpj          TYPE c LENGTH 04 VALUE 'CNPJ',
           destcpf           TYPE c LENGTH 03 VALUE 'CPF',

           "Tag de endereço do destino
           destenderdest     TYPE c LENGTH 09 VALUE 'enderDest',
           destxlgr          TYPE c LENGTH 04 VALUE 'xLgr',
           destnro           TYPE c LENGTH 03 VALUE 'nro',
           destxcpl          TYPE c LENGTH 04 VALUE 'xCpl',
           destxbairro       TYPE c LENGTH 07 VALUE 'xBairro',
           destcmun          TYPE c LENGTH 04 VALUE 'cMun',
           destfone          TYPE c LENGTH 04 VALUE 'fone',
           destie            TYPE c LENGTH 02 VALUE 'IE',
           destisuf          TYPE c LENGTH 04 VALUE 'ISUF',
           destcep           TYPE c LENGTH 03 VALUE 'CEP',
           destcpais         TYPE c LENGTH 05 VALUE 'cPais',
           destemailxml      TYPE c LENGTH 08 VALUE 'eMailXML',
           destemaildanfe    TYPE c LENGTH 10 VALUE 'eMailDANFE',
           destemailxmlsi    TYPE c LENGTH 08 VALUE 'email',
           destiddest        TYPE c LENGTH 06 VALUE 'idDest',
           destidestrangeiro TYPE c LENGTH 13 VALUE 'idEstrangeiro',
           destindiedest     TYPE c LENGTH 09 VALUE 'indIEDest',

           "Tag de Local de Retirada da Mercadoria
           retirada          TYPE c LENGTH 08 VALUE 'retirada',
           retiradacnpj      TYPE c LENGTH 08 VALUE 'CNPJ',
           retiradacpf       TYPE c LENGTH 08 VALUE 'CPF',
           retiradaxlgr      TYPE c LENGTH 08 VALUE 'xLgr',
           retiradanro       TYPE c LENGTH 08 VALUE 'nro',
           retiradaxcpl      TYPE c LENGTH 08 VALUE 'xCpl',
           retiradaxbairro   TYPE c LENGTH 08 VALUE 'xBairro',
           retiradacmun      TYPE c LENGTH 08 VALUE 'cMun',

           "Tag de Local de Entrega da Mercadoria
           entrega           TYPE c LENGTH 08 VALUE 'entrega',
           entregacnpj       TYPE c LENGTH 08 VALUE 'CNPJ',
           entregacpf        TYPE c LENGTH 08 VALUE 'CPF',
           entregaxlgr       TYPE c LENGTH 08 VALUE 'xLgr',
           entreganro        TYPE c LENGTH 08 VALUE 'nro',
           entregaxcpl       TYPE c LENGTH 08 VALUE 'xCpl',
           entregaxbairro    TYPE c LENGTH 08 VALUE 'xBairro',
           entregacmun       TYPE c LENGTH 08 VALUE 'cMun',

           "Tag de Totais
           total             TYPE c LENGTH 05 VALUE 'total',
           icmstot           TYPE c LENGTH 07 VALUE 'ICMSTot',
           vbc               TYPE c LENGTH 03 VALUE 'vBC',
           vicms             TYPE c LENGTH 05 VALUE 'vICMS',
           vbcst             TYPE c LENGTH 05 VALUE 'vBCST',
           vst               TYPE c LENGTH 03 VALUE 'vST',
           vprod             TYPE c LENGTH 05 VALUE 'vProd',
           vfrete            TYPE c LENGTH 06 VALUE 'vFrete',
           vseg              TYPE c LENGTH 04 VALUE 'vSeg',
           vdesc             TYPE c LENGTH 05 VALUE 'vDesc',
           vii               TYPE c LENGTH 03 VALUE 'vII',
           vipi              TYPE c LENGTH 04 VALUE 'vIPI',
           vpis              TYPE c LENGTH 04 VALUE 'vPIS',
           vcofins           TYPE c LENGTH 07 VALUE 'vCOFINS',
           voutro            TYPE c LENGTH 06 VALUE 'vOutro',
           vnf               TYPE c LENGTH 03 VALUE 'vNF',
           vfcp              TYPE c LENGTH 04 VALUE 'vFCP',
           vfcpst            TYPE c LENGTH 06 VALUE 'vFCPST',
           vfcpstret         TYPE c LENGTH 09 VALUE 'vFCPSTRet',
           vipidevol         TYPE c LENGTH 09 VALUE 'vIPIDevol',

           "Grupo de Pagamento.
           pag               TYPE c LENGTH 05 VALUE 'pag',
           detpag            TYPE c LENGTH 06 VALUE 'detPag',
           tpag              TYPE c LENGTH 06 VALUE 'tPag',
           vpag              TYPE c LENGTH 06 VALUE 'vPag',

           "Tag de Retenção de Tributos
           rettrib           TYPE c LENGTH 07 VALUE 'retTrib',
           vretpis           TYPE c LENGTH 07 VALUE 'vRetPIS',
           vretcofins        TYPE c LENGTH 09 VALUE 'vRetCOFIN',
           vretcsll          TYPE c LENGTH 08 VALUE 'vRetCSLL',
           vbcirrf           TYPE c LENGTH 07 VALUE 'vBCIRRF',
           virrf             TYPE c LENGTH 05 VALUE 'vIRRF',
           vbcretprev        TYPE c LENGTH 10 VALUE 'vBCRetPrev',
           vretprev          TYPE c LENGTH 08 VALUE 'vRetPrev',

           "Tag de informação de transporte
           transp            TYPE c LENGTH 06 VALUE 'transp',
           modfrete          TYPE c LENGTH 08 VALUE 'modFrete',

           "Tag de informações da trabsportadora
           transporta        TYPE c LENGTH 10 VALUE 'transporta',
           transportacnpj    TYPE c LENGTH 04 VALUE 'CNPJ',
           transportacpf     TYPE c LENGTH 03 VALUE 'CPF',
           transportaxnome   TYPE c LENGTH 05 VALUE 'xNome',
           transportaie      TYPE c LENGTH 02 VALUE 'IE',
           transportaxender  TYPE c LENGTH 06 VALUE 'xEnder',
           transportaxmun    TYPE c LENGTH 04 VALUE 'xMun',
           transportacmun    TYPE c LENGTH 04 VALUE 'cMun',
           transportauf      TYPE c LENGTH 04 VALUE 'UF',

           "Tag de Retenção de ICMS do Transportador
           rettransp         TYPE c LENGTH 09 VALUE 'retTransp',
           rettranspvserv    TYPE c LENGTH 05 VALUE 'vServ',
           rettranspvbcret   TYPE c LENGTH 06 VALUE 'vBCRet',
           rettransppicmsret TYPE c LENGTH 08 VALUE 'pICMSRet',
           rettranspvicmsret TYPE c LENGTH 08 VALUE 'vICMSRet',
           rettranspcfop     TYPE c LENGTH 04 VALUE 'CFOP',
           rettranspcmun     TYPE c LENGTH 04 VALUE 'cMun',

           "Tag de Veículo
           veictransp        TYPE c LENGTH 10 VALUE 'veicTransp',
           veictranspplaca   TYPE c LENGTH 05 VALUE 'placa',
           veictranspuf      TYPE c LENGTH 02 VALUE 'UF',
           veictransprntc    TYPE c LENGTH 04 VALUE 'RNTC',

           "Tag de Reboque
           reboque           TYPE c LENGTH 07 VALUE 'reboque',
           reboqueplaca      TYPE c LENGTH 05 VALUE 'placa',
           reboqueuf         TYPE c LENGTH 02 VALUE 'UF',
           reboquerntc       TYPE c LENGTH 04 VALUE 'RNTC',

           "Tag de Volumes
           vol               TYPE c LENGTH 03 VALUE 'vol',
           volqvol           TYPE c LENGTH 04 VALUE 'qVol',
           volesp            TYPE c LENGTH 03 VALUE 'esp',
           volmarca          TYPE c LENGTH 05 VALUE 'marca',
           volnvol           TYPE c LENGTH 04 VALUE 'nVol',
           volpesol          TYPE c LENGTH 05 VALUE 'pesoL',
           volpesob          TYPE c LENGTH 05 VALUE 'pesoB',

           "Tag de Lacres dos volumes
           lacres            TYPE c LENGTH 06 VALUE 'lacres',
           lacresnlacres     TYPE c LENGTH 07 VALUE 'nLacres',

           "Tag de informações de Cobrança
           cobr              TYPE c LENGTH 04 VALUE 'cobr',
           fat               TYPE c LENGTH 03 VALUE 'fat',
           fatnfat           TYPE c LENGTH 04 VALUE 'nFat',
           fatvorig          TYPE c LENGTH 05 VALUE 'vOrig',
           fatvdesc          TYPE c LENGTH 05 VALUE 'vDesc',
           fatvliq           TYPE c LENGTH 04 VALUE 'vLiq',
           fatdup            TYPE c LENGTH 03 VALUE 'dup',
           fatndup           TYPE c LENGTH 04 VALUE 'nDup',
           fatdvenc          TYPE c LENGTH 05 VALUE 'dVenc',
           fatvdup           TYPE c LENGTH 04 VALUE 'vDup',

           "Tag de Informações Adicionais da NFe
           infadic           TYPE c LENGTH 07 VALUE 'infAdic',
           infadfisco        TYPE c LENGTH 10 VALUE 'infAdFisco',
           infcpi            TYPE c LENGTH 06 VALUE 'infCpl',
           obscont           TYPE c LENGTH 07 VALUE 'obsCont',
           xcampo            TYPE c LENGTH 06 VALUE 'xCampo',
           xtexto            TYPE c LENGTH 06 VALUE 'xTexto',

           "Tag de informações de Comércio Exterior
           exporta           TYPE c LENGTH 07 VALUE 'exporta',
           ufembarq          TYPE c LENGTH 08 VALUE 'UFEmbarq',
           xlocembarq        TYPE c LENGTH 10 VALUE 'xLocEmbarq',
           ufsaidapais       TYPE c LENGTH 11 VALUE 'UFSaidaPais',
           xlocexporta       TYPE c LENGTH 11 VALUE 'xLocExporta',
           xlocdespacho      TYPE c LENGTH 12 VALUE 'xLocDespacho',


           "Tag de Informações de Compras
           compra            TYPE c LENGTH 06 VALUE 'compra',
           xped              TYPE c LENGTH 04 VALUE 'xPed',
           xcont             TYPE c LENGTH 05 VALUE 'xCont',

           "Tag do Grupo de informações de itens
           det               TYPE c LENGTH 03 VALUE 'det',
           nitem             TYPE c LENGTH 05 VALUE 'nItem',
           infadprod         TYPE c LENGTH 09 VALUE 'infAdProd',
           detprod           TYPE c LENGTH 04 VALUE 'prod',
           detcprod          TYPE c LENGTH 05 VALUE 'cProd',
           detcean           TYPE c LENGTH 04 VALUE 'cEAN',
           detxprod          TYPE c LENGTH 05 VALUE 'xProd',
           detncm            TYPE c LENGTH 03 VALUE 'NCM',
           detextipi         TYPE c LENGTH 06 VALUE 'EXTIPI',
           detgenero         TYPE c LENGTH 06 VALUE 'genero',
           detcfop           TYPE c LENGTH 04 VALUE 'CFOP',
           detucom           TYPE c LENGTH 04 VALUE 'uCom',
           detqcom           TYPE c LENGTH 04 VALUE 'qCom',
           detvuncom         TYPE c LENGTH 06 VALUE 'vUnCom',
           detvprod          TYPE c LENGTH 05 VALUE 'vProd',
           detcest           TYPE c LENGTH 04 VALUE 'CEST',
           detceantrib       TYPE c LENGTH 08 VALUE 'cEANTrib',
           detutrib          TYPE c LENGTH 05 VALUE 'uTrib',
           detqtrib          TYPE c LENGTH 05 VALUE 'qTrib',
           detvuntrib        TYPE c LENGTH 07 VALUE 'vUnTrib',
           detvfrete         TYPE c LENGTH 06 VALUE 'vFrete',
           detvseg           TYPE c LENGTH 04 VALUE 'vSeg',
           detvdesc          TYPE c LENGTH 05 VALUE 'vDesc',
           detindtot         TYPE c LENGTH 06 VALUE 'indTot',
           detxped           TYPE c LENGTH 04 VALUE 'xPed',

           "Declarações de Importação
           di                TYPE c LENGTH 02 VALUE 'DI',
           dindi             TYPE c LENGTH 03 VALUE 'nDI',
           diddi             TYPE c LENGTH 03 VALUE 'dDI',
           dixlocdesemb      TYPE c LENGTH 10 VALUE 'xLocDesemb',
           diufdesemb        TYPE c LENGTH 08 VALUE 'UFDesemb',
           diddesemb         TYPE c LENGTH 07 VALUE 'dDesemb',
           dicexportador     TYPE c LENGTH 11 VALUE 'cExportador',
           ditpviatransp     TYPE c LENGTH 11 VALUE 'tpViaTransp',
           divafrmm          TYPE c LENGTH 06 VALUE 'vAFRMM',
           ditpintermedio    TYPE c LENGTH 12 VALUE 'tpIntermedio',

           "Adições da Declaração de Importação
           adi               TYPE c LENGTH 03 VALUE 'adi',
           nadicao           TYPE c LENGTH 07 VALUE 'nAdicao',
           nseqadic          TYPE c LENGTH 08 VALUE 'nSeqAdic',
           cfabricante       TYPE c LENGTH 11 VALUE 'cFabricante',
           vdescdi           TYPE c LENGTH 07 VALUE 'vDescDI',


           "Grupo de exportação indireta.
           detexport         TYPE c LENGTH 09 VALUE 'detExport',
           exportind         TYPE c LENGTH 09 VALUE 'exportInd',
           nre               TYPE c LENGTH 03 VALUE 'nRE',
           qexport           TYPE c LENGTH 07 VALUE 'qExport',


           "Impostos
           imposto           TYPE c LENGTH 08 VALUE 'imposto',

           "ICMS
           imicms            TYPE c LENGTH 04 VALUE 'ICMS',
           imorig            TYPE c LENGTH 04 VALUE 'orig',
           imcst             TYPE c LENGTH 03 VALUE 'CST',
           immodbc           TYPE c LENGTH 05 VALUE 'modBC',
           imvbc             TYPE c LENGTH 03 VALUE 'vBC',
           impicms           TYPE c LENGTH 05 VALUE 'pICMS',
           imvicms           TYPE c LENGTH 05 VALUE 'vICMS',
           immodbcst         TYPE c LENGTH 07 VALUE 'modBCST',
           impmvast          TYPE c LENGTH 06 VALUE 'pMVAST',
           impredbcst        TYPE c LENGTH 08 VALUE 'pRedBCST',
           imvbcst           TYPE c LENGTH 05 VALUE 'vBCST',
           impicmsst         TYPE c LENGTH 07 VALUE 'pICMSST',
           imvicmsst         TYPE c LENGTH 07 VALUE 'vICMSST',
           imvicmsop         TYPE c LENGTH 07 VALUE 'vICMSOp',
           impdif            TYPE c LENGTH 04 VALUE 'pDif',
           imvicmsdif        TYPE c LENGTH 08 VALUE 'vICMSDif',
           impredbc          TYPE c LENGTH 06 VALUE 'pRedBC',
           impmotdesicms     TYPE c LENGTH 10 VALUE 'motDesICMS',
           impvicmsdeson     TYPE c LENGTH 10 VALUE 'vICMSDeson',

           impicmsufdest     TYPE c LENGTH 10 VALUE  'ICMSUFDest',
           impvbcufdest      TYPE c LENGTH 09 VALUE  'vBCUFDest',
           imppfcpufdest     TYPE c LENGTH 10 VALUE  'pFCPUFDest',
           imppicmsufdest    TYPE c LENGTH 11 VALUE  'pICMSUFDest',
           imppicmsinter     TYPE c LENGTH 10 VALUE  'pICMSInter',
           imppicmsinterpart TYPE c LENGTH 14 VALUE  'pICMSInterPart',
           impvfcpufdest     TYPE c LENGTH 10 VALUE  'vFCPUFDest',
           impvicmsufdest    TYPE c LENGTH 11 VALUE  'vICMSUFDest',
           impvicmsufremet   TYPE c LENGTH 12 VALUE  'vICMSUFRemet',

           "IPI
           ipi               TYPE c LENGTH 03 VALUE 'IPI',
           ipicienq          TYPE c LENGTH 05 VALUE 'cIEnq',
           ipicenq           TYPE c LENGTH 04 VALUE 'cEnq',
           ipicnpjprod       TYPE c LENGTH 08 VALUE 'CNPJProd',
           ipitrib           TYPE c LENGTH 07 VALUE 'IPITrib',
           ipicselo          TYPE c LENGTH 05 VALUE 'cSelo',
           ipiqselo          TYPE c LENGTH 05 VALUE 'qSelo',
           ipicst            TYPE c LENGTH 03 VALUE 'CST',
           ipivbc            TYPE c LENGTH 03 VALUE 'vBC',
           ipiqunid          TYPE c LENGTH 05 VALUE 'qUnid',
           ipivunid          TYPE c LENGTH 05 VALUE 'vUnid',
           ipipipi           TYPE c LENGTH 04 VALUE 'pIPI',
           ipivipi           TYPE c LENGTH 04 VALUE 'vIPI',

           "II
           ii                TYPE c LENGTH 02 VALUE 'II',
           iivbc             TYPE c LENGTH 03 VALUE 'vBC',
           iivdespadu        TYPE c LENGTH 08 VALUE 'vDespAdu',
           iivii             TYPE c LENGTH 03 VALUE 'vII',
           iiviof            TYPE c LENGTH 04 VALUE 'vIOF',

           "PIS
           pis               TYPE c LENGTH 03 VALUE 'PIS',
           pisst             TYPE c LENGTH 05 VALUE 'PISST',
           piscst            TYPE c LENGTH 03 VALUE 'CST',
           pisvbc            TYPE c LENGTH 03 VALUE 'vBC',
           pisppis           TYPE c LENGTH 04 VALUE 'pPIS',
           pisvpis           TYPE c LENGTH 04 VALUE 'vPIS',
           pisqbcprod        TYPE c LENGTH 07 VALUE 'qBCProd',
           pisvaliqprod      TYPE c LENGTH 09 VALUE 'vAliqProd',

           "COFINS
           cofins            TYPE c LENGTH 06 VALUE 'COFINS',
           cofinsst          TYPE c LENGTH 08 VALUE 'COFINSST',
           cofinscst         TYPE c LENGTH 03 VALUE 'CST',
           cofinsvbc         TYPE c LENGTH 03 VALUE 'vBC',
           cofinspcofins     TYPE c LENGTH 07 VALUE 'pCOFINS',
           cofinsvcofins     TYPE c LENGTH 07 VALUE 'vCOFINS',
           cofinsqbcprod     TYPE c LENGTH 07 VALUE 'qBCProd',
           cofinsvaliqprod   TYPE c LENGTH 09 VALUE 'vAliqProd',

           "ISSQN
           issqn             TYPE c LENGTH 05 VALUE 'ISSQN',
           issqnvbc          TYPE c LENGTH 03 VALUE 'vBC',
           issqnvaliq        TYPE c LENGTH 05 VALUE 'vAliq',
           issqnvissqn       TYPE c LENGTH 06 VALUE 'vISSQN',
           issqncmunfg       TYPE c LENGTH 06 VALUE 'cMunFG',
           issqnclistserv    TYPE c LENGTH 09 VALUE 'cListServ',

           "Total de Tributos
           vtottrib          TYPE c LENGTH 8 VALUE 'vTotTrib'.

"Contantes para CTe
CONSTANTS: ctepadrao           TYPE c LENGTH 36 VALUE '?xml version="1.0" encoding="UTF-8"?',
           intgcte             TYPE c LENGTH 07 VALUE 'intgCTe',

           "Tag de Identificação do Conhecimento de Transporte Eletrônico
           cteide              TYPE c LENGTH 03 VALUE 'ide',
           ctecfop             TYPE c LENGTH 04 VALUE 'CFOP',
           ctenatop            TYPE c LENGTH 05 VALUE 'natOp',
           cteforpag           TYPE c LENGTH 06 VALUE 'forPag', "CS2017002143 CT-e 3.0 Removida
           cteserie            TYPE c LENGTH 05 VALUE 'serie',
           ctenct              TYPE c LENGTH 03 VALUE 'nCT',
           ctedhemi            TYPE c LENGTH 05 VALUE 'dhEmi',
           ctetpcte            TYPE c LENGTH 05 VALUE 'tpCTe',
           cterefcte           TYPE c LENGTH 06 VALUE 'refCTE',
           ctemodal            TYPE c LENGTH 05 VALUE 'modal',
           ctetpserv           TYPE c LENGTH 06 VALUE 'tpServ',
           ctecmunenv          TYPE c LENGTH 07 VALUE 'cMunEnv',
           ctecmunini          TYPE c LENGTH 07 VALUE 'cMunIni',
           ctecmunfim          TYPE c LENGTH 07 VALUE 'cMunFim',
           cteretira           TYPE c LENGTH 06 VALUE 'retira',
           ctexdetretira       TYPE c LENGTH 10 VALUE 'xdetretira',
           ctetoma             TYPE c LENGTH 04 VALUE 'toma',

           "TAG de grupo das informações complementares do CT-e
           ctecompl            TYPE c LENGTH 05 VALUE 'compl',
           ctexobs             TYPE c LENGTH 04 VALUE 'xObs',
           cteobscont          TYPE c LENGTH 07 VALUE 'ObsCont',
           ctexcampo           TYPE c LENGTH 06 VALUE 'xCampo',
           ctextexto           TYPE c LENGTH 06 VALUE 'xTexto',

           "Tomador de serviços
           ctetomaoutros       TYPE c LENGTH 10 VALUE 'tomaOutros',
           ctetomacnpj         TYPE c LENGTH 04 VALUE 'CNPJ',
           ctetomacpf          TYPE c LENGTH 03 VALUE 'CPF',
           ctetomaie           TYPE c LENGTH 02 VALUE 'IE',
           ctetomaxnome        TYPE c LENGTH 05 VALUE 'xNome',
           ctetomaxfan         TYPE c LENGTH 04 VALUE 'xFan',
           ctetomafone         TYPE c LENGTH 04 VALUE 'fone',

           "Endereço do Tomador do Serviço
           ctetomaendertoma    TYPE c LENGTH 09 VALUE 'enderToma',
           ctetomaxlgr         TYPE c LENGTH 04 VALUE 'xLgr',
           ctetomaxnum         TYPE c LENGTH 04 VALUE 'xNum',
           ctetomaxcpl         TYPE c LENGTH 04 VALUE 'xCpl',
           ctetomaxbairro      TYPE c LENGTH 07 VALUE 'xBairro',
           ctetomacmun         TYPE c LENGTH 04 VALUE 'cMun',
           ctetomacep          TYPE c LENGTH 03 VALUE 'CEP',
           ctetomapais         TYPE c LENGTH 05 VALUE 'cPais',

           "Tag de Emitente do CTe
           cteemit             TYPE c LENGTH 04 VALUE 'emit',
           cteemitcnpj         TYPE c LENGTH 04 VALUE 'CNPJ',
           cteemitie           TYPE c LENGTH 02 VALUE 'IE',

           "TAG de grupo de informações do remetente das mercadorias transportadas pelo CT-e
           cterem              TYPE c LENGTH 03 VALUE 'rem',
           cteremcnpj          TYPE c LENGTH 04 VALUE 'CNPJ',
           cteremcpf           TYPE c LENGTH 03 VALUE 'CPF',
           cteremie            TYPE c LENGTH 02 VALUE 'IE',
           cteremxnome         TYPE c LENGTH 05 VALUE 'xNome',
           cteremxfant         TYPE c LENGTH 05 VALUE 'xFant',
           cteremfone          TYPE c LENGTH 04 VALUE 'fone',

           "Tag de endereço do remetente da mercadoria
           cteremenderreme     TYPE c LENGTH 09 VALUE 'enderReme',
           cteremxlgr          TYPE c LENGTH 04 VALUE 'xLgr',
           cteremnro           TYPE c LENGTH 03 VALUE 'nro',
           cteremxcpl          TYPE c LENGTH 04 VALUE 'xCpl',
           cteremxbairro       TYPE c LENGTH 07 VALUE 'xBairro',
           cteremcmun          TYPE c LENGTH 04 VALUE 'cMun',
           cteremcep           TYPE c LENGTH 03 VALUE 'CEP',

           "TAG de grupos de informações das NF
           ctereminfnf         TYPE c LENGTH 05 VALUE 'infNF',
           ctereminfnfnroma    TYPE c LENGTH 05 VALUE 'nRoma',
           ctereminfnfnped     TYPE c LENGTH 04 VALUE 'nPed',
           ctereminfnfserie    TYPE c LENGTH 05 VALUE 'serie',
           ctereminfnfndoc     TYPE c LENGTH 04 VALUE 'nDoc',
           ctereminfnfdemi     TYPE c LENGTH 04 VALUE 'dEmi',
           ctereminfnfvbc      TYPE c LENGTH 03 VALUE 'vBC',
           ctereminfnfvicms    TYPE c LENGTH 05 VALUE 'vICMS',
           ctereminfnfvbcst    TYPE c LENGTH 05 VALUE 'vBCST',
           ctereminfnfvst      TYPE c LENGTH 03 VALUE 'vST',
           ctereminfnfvprod    TYPE c LENGTH 05 VALUE 'vProd',
           ctereminfnfvnf      TYPE c LENGTH 03 VALUE 'vNF',
           ctereminfnfncfop    TYPE c LENGTH 05 VALUE 'nCFOP',
           ctereminfnfnpeso    TYPE c LENGTH 05 VALUE 'nPeso',
           ctereminfnfpin      TYPE c LENGTH 03 VALUE 'PIN',

           "TAG de grupos de informações das NFe
           cteinfnfe           TYPE c LENGTH 06 VALUE 'infNFe',
           cteinfchave         TYPE c LENGTH 05 VALUE 'chave',
           cteinfpin           TYPE c LENGTH 03 VALUE 'PIN',

           "TAG de grupos de informações das N
*           CTEINFNF            TYPE C LENGTH 05 VALUE 'infNF',
           ctenmod             TYPE c LENGTH 03 VALUE 'mod',
           cteinfserie         TYPE c LENGTH 05 VALUE 'serie',
           cteinfndoc          TYPE c LENGTH 04 VALUE 'nDoc',
           cteinfunidrat       TYPE c LENGTH 07 VALUE 'unidRat',
           ctedemi             TYPE c LENGTH 04 VALUE 'dEmi',
           ctevbc              TYPE c LENGTH 03 VALUE 'vBC',
           ctevicms            TYPE c LENGTH 05 VALUE 'vICMS',
           ctevbcst            TYPE c LENGTH 05 VALUE 'vBCST',
           ctevst              TYPE c LENGTH 03 VALUE 'vST',
           ctevprod            TYPE c LENGTH 05 VALUE 'vProd',
           ctevnf              TYPE c LENGTH 03 VALUE 'vNF',
           ctencfop            TYPE c LENGTH 05 VALUE 'nCFOP',

           "Grupo Unidades de Transporte
           cteinfunidtransp    TYPE c LENGTH 13 VALUE 'infUnidTransp',
           ctetpunidtransp     TYPE c LENGTH 12 VALUE 'tpUnidTransp',
           cteidunidtransp     TYPE c LENGTH 12 VALUE 'idUnidTransp',
           cteqtdrat           TYPE c LENGTH 06 VALUE 'qtdRat',

           "TAG de grupos de informações das Notas
           cteinfnf            TYPE c LENGTH 05 VALUE 'infNF',
           cteinfnfnroma       TYPE c LENGTH 05 VALUE 'nRoma',
           cteinfnfnped        TYPE c LENGTH 04 VALUE 'nPed',
           cteinfnfserie       TYPE c LENGTH 05 VALUE 'serie',
           cteinfnfmod         TYPE c LENGTH 03 VALUE 'mod',
           cteinfnfndoc        TYPE c LENGTH 04 VALUE 'nDoc',
           cteinfnfdemi        TYPE c LENGTH 04 VALUE 'dEmi',
           cteinfnfvbc         TYPE c LENGTH 03 VALUE 'vBC',
           cteinfnfvicms       TYPE c LENGTH 05 VALUE 'vICMS',
           cteinfnfvbcst       TYPE c LENGTH 05 VALUE 'vBCST',
           cteinfnfvst         TYPE c LENGTH 03 VALUE 'vST',
           cteinfnfvprod       TYPE c LENGTH 05 VALUE 'vProd',
           cteinfnfvnf         TYPE c LENGTH 03 VALUE 'vNF',
           cteinfnfncfop       TYPE c LENGTH 05 VALUE 'nCFOP',
           cteinfnfnpeso       TYPE c LENGTH 05 VALUE 'nPeso',
           cteinfnfpin         TYPE c LENGTH 03 VALUE 'PIN',

           ctelocret           TYPE c LENGTH 06 VALUE 'locRet',
           ctelocretcnpj       TYPE c LENGTH 04 VALUE 'CNPJ',
           ctelocretcpf        TYPE c LENGTH 03 VALUE 'CPF',
           ctelocretxnome      TYPE c LENGTH 05 VALUE 'xNome',
           ctelocretxlgr       TYPE c LENGTH 04 VALUE 'xLgr',
           ctelocretnro        TYPE c LENGTH 03 VALUE 'nro',
           ctelocretxcpl       TYPE c LENGTH 04 VALUE 'xCpl',
           ctelocretxxbairro   TYPE c LENGTH 07 VALUE 'xBairro',
           ctelocretxcmun      TYPE c LENGTH 04 VALUE 'cMun',

           "TAG de grupo de informações do destinatário do CT-e
           ctedestremdest      TYPE c LENGTH 04 VALUE 'dest',
           ctedestremcnpj      TYPE c LENGTH 04 VALUE 'CNPJ',
           ctedestremcpf       TYPE c LENGTH 03 VALUE 'CPF',
           ctedestremxnome     TYPE c LENGTH 05 VALUE 'xNome',
           ctedestremfone      TYPE c LENGTH 04 VALUE 'fone',
           ctedestremie        TYPE c LENGTH 02 VALUE 'IE',
           ctedestremisuf      TYPE c LENGTH 04 VALUE 'ISUF',

           "TAG de grupo de informações de endereço do destinatário do CT-e
           cteenddestenderdest TYPE c LENGTH 09 VALUE 'enderDest',
           cteenddestxlgr      TYPE c LENGTH 04 VALUE 'xLgr',
           cteenddestnro       TYPE c LENGTH 03 VALUE 'nro',
           cteenddestxcpl      TYPE c LENGTH 04 VALUE 'xCpl',
           cteenddestxbairro   TYPE c LENGTH 07 VALUE 'xBairro',
           cteenddestcmun      TYPE c LENGTH 04 VALUE 'cMun',
           cteenddestcep       TYPE c LENGTH 03 VALUE 'CEP',
           cteenddestpais      TYPE c LENGTH 05 VALUE 'cPais',

           "TAG de grupo de valores da prestação de serviço
           ctevlrpserv         TYPE c LENGTH 06 VALUE 'vPrest',
           ctevlrpservvtprest  TYPE c LENGTH 07 VALUE 'vTPrest',
           ctevlrpservvrec     TYPE c LENGTH 04 VALUE 'vRec',

           "TAG de grupo de componentes do valor da prestação
           ctecompcomp         TYPE c LENGTH 04 VALUE 'comp',
           ctecompxnome        TYPE c LENGTH 05 VALUE 'xNome',
           ctecompvcomp        TYPE c LENGTH 05 VALUE 'vComp',

           "TAG de grupo de informações relativas aos impostos
           "TAG de grupo de informações relativas ao ICMS
           cteimpimp           TYPE c LENGTH 03 VALUE 'imp',
           cteimpicms          TYPE c LENGTH 04 VALUE 'ICMS',
           cteimpcst           TYPE c LENGTH 03 VALUE 'CST',
           cteimpvbc           TYPE c LENGTH 03 VALUE 'vBC',
           cteimppicms         TYPE c LENGTH 05 VALUE 'pICMS',
           cteimpvicms         TYPE c LENGTH 05 VALUE 'vICMS',
           cteimppredbc        TYPE c LENGTH 06 VALUE 'pRedBC',
           cteimpvcred         TYPE c LENGTH 05 VALUE 'vCred',
           cteimpinfadfisco    TYPE c LENGTH 10 VALUE 'infAdFisco',

           "TAG de grupo de informações do CTe Normal ou CTe emitido
           "em hipótese de anulação de débito
           cteinfinfctenorm    TYPE c LENGTH 10 VALUE 'infCTeNorm',

           "TAG de grupo de informações da carga do CT-e
           ctecargainfcarga    TYPE c LENGTH 08 VALUE 'infCarga',
           ctecargavmerc       TYPE c LENGTH 05 VALUE 'vMerc',
           ctecargavcarga      TYPE c LENGTH 06 VALUE 'vCarga',
           ctecargapropred     TYPE c LENGTH 07 VALUE 'proPred',
           ctecargaxoutcat     TYPE c LENGTH 07 VALUE 'xOutCat',

           "TAG de grupo de informações da carga do CT-e
           cteqtdeinfq         TYPE c LENGTH 04 VALUE 'infQ',
           cteqtdecunid        TYPE c LENGTH 05 VALUE 'cUnid',
           cteqtdetpmed        TYPE c LENGTH 05 VALUE 'tpMed',
           cteqtdeqcarga       TYPE c LENGTH 06 VALUE 'qCarga',

           "TAG de grupo de informações específicas do Transporte Rodoviário
           ctemodalrodo        TYPE c LENGTH 04 VALUE 'rodo',
           ctemodalrntrc       TYPE c LENGTH 05 VALUE 'RNTRC',
           ctemodaldprev       TYPE c LENGTH 05 VALUE 'dPrev', "CS2017002143 CT-e 3.0 Removida
           ctemodallota        TYPE c LENGTH 04 VALUE 'lota',  "CS2017002143 CT-e 3.0 Removida
           ctemodalciot        TYPE c LENGTH 04 VALUE 'CIOT',  "CS2017002143 CT-e 3.0 Removida

           "TAG de grupo de informações dos veículos           "CS2017002143 CT-e 3.0 Grupo Removido
           cteveicveic         TYPE c LENGTH 04 VALUE 'veic',
           cteveicclnt         TYPE c LENGTH 04 VALUE 'clnt',
           cteveicrenavam      TYPE c LENGTH 07 VALUE 'RENAVAM',
           cteveicplaca        TYPE c LENGTH 05 VALUE 'placa',
           cteveictara         TYPE c LENGTH 04 VALUE 'tara',
           cteveiccapkg        TYPE c LENGTH 05 VALUE 'capKG',
           cteveiccapm3        TYPE c LENGTH 05 VALUE 'capM3',
           cteveictpprop       TYPE c LENGTH 06 VALUE 'tpProp',
           cteveictpveic       TYPE c LENGTH 06 VALUE 'tpVeic',
           cteveictprod        TYPE c LENGTH 05 VALUE 'tpRod',
           cteveictpcar        TYPE c LENGTH 05 VALUE 'tpCar',
           cteveicuf           TYPE c LENGTH 02 VALUE 'UF',
           cteemail            TYPE c LENGTH 05 VALUE 'email',

           "TAG de grupo de informações de proprietários do veículo
           ctepropprop         TYPE c LENGTH 04 VALUE 'prop',
           ctepropcpf          TYPE c LENGTH 03 VALUE 'CPF',
           ctepropcnpj         TYPE c LENGTH 04 VALUE 'CNPJ',
           ctepropcrntrc       TYPE c LENGTH 05 VALUE 'RNTRC',
           ctepropxnome        TYPE c LENGTH 05 VALUE 'xNome',
           ctepropie           TYPE c LENGTH 02 VALUE 'IE',
           ctepropuf           TYPE c LENGTH 02 VALUE 'UF',
           cteproptpprop       TYPE c LENGTH 06 VALUE 'tpProp',

           " TAG de grupo de informações dos motoristas            "CS2017002143 CT-e 3.0 Grupo Removido
           ctemotomoto         TYPE c LENGTH 04 VALUE 'moto',
           ctemotoxnome        TYPE c LENGTH 05 VALUE 'xNome',
           ctemotocpf          TYPE c LENGTH 03 VALUE 'CPF',

           "Tag de Grupo de Seguro da Carga                         "CS2017002143 CT-e 3.0 Grupo Removido
           cteseg              TYPE c LENGTH 03 VALUE 'seg',
           cterespseg          TYPE c LENGTH 07 VALUE 'respSeg',
           ctexseg             TYPE c LENGTH 04 VALUE 'xSeg',
           ctenapol            TYPE c LENGTH 05 VALUE 'nApol',
           ctenaver            TYPE c LENGTH 05 VALUE 'nAver',
           ctevcarga           TYPE c LENGTH 06 VALUE 'vCarga',

           "Tag de grupo de informações de pedágio                 "CS2017002143 CT-e 3.0 Grupo Removido
           ctevaleped          TYPE c LENGTH 07 VALUE 'valePed',
           ctevtvaleped        TYPE c LENGTH 09 VALUE 'vTValePed',
           cteresppg           TYPE c LENGTH 06 VALUE 'respPg',


           "TAG de grupo de informações específicas do Transporte Aquaviário
           ctemodalaquav       TYPE c LENGTH 05 VALUE 'aquav',
           ctevprestaquav      TYPE c LENGTH 06 VALUE 'vPrest',
           ctevafrmm           TYPE c LENGTH 06 VALUE 'vAFRMM',
           ctenbooking         TYPE c LENGTH 08 VALUE 'nBooking', "CS2017002143 CT-e 3.0 Removida
           ctenctrl            TYPE c LENGTH 05 VALUE 'nCtrl',    "CS2017002143 CT-e 3.0 Removida
           ctexnavio           TYPE c LENGTH 06 VALUE 'xNavio',
           ctexbalsa           TYPE c LENGTH 06 VALUE 'xBalsa',
           ctebalsa            TYPE c LENGTH 05 VALUE 'balsa',
           ctenviag            TYPE c LENGTH 05 VALUE 'nViag',
           ctedirec            TYPE c LENGTH 05 VALUE 'direc',
           cteprtemb           TYPE c LENGTH 06 VALUE 'prtEmb',    "CS2017002143 CT-e 3.0 Removida
           cteprttrans         TYPE c LENGTH 08 VALUE 'prtTrans',  "CS2017002143 CT-e 3.0 Removida
           cteprtdest          TYPE c LENGTH 07 VALUE 'prtDest',   "CS2017002143 CT-e 3.0 Removida
           ctetpnav            TYPE c LENGTH 05 VALUE 'tpNav',     "CS2017002143 CT-e 3.0 Removida
           cteirin             TYPE c LENGTH 04 VALUE 'irin',
           ctencont            TYPE c LENGTH 05 VALUE 'nCont',
           ctenlacre           TYPE c LENGTH 06 VALUE 'nLacre',
           cteinfnfeaquav      TYPE c LENGTH 06 VALUE 'infNFe',

           "Detalhamento do CT-e complementado
           cteinfctecomp       TYPE c LENGTH  10 VALUE 'infCteComp',
           ctechave            TYPE c LENGTH  05 VALUE 'chave'.



CONSTANTS: cancnfe         TYPE c LENGTH 07 VALUE 'CancNFe',
           cancnfecnpj     TYPE c LENGTH 04 VALUE 'CNPJ',
           cancnfeie       TYPE c LENGTH 04 VALUE 'IE',
           cancnfennota    TYPE c LENGTH 05 VALUE 'nNota',
           cancnfenserie   TYPE c LENGTH 06 VALUE 'nSerie',
           cancnfexjust    TYPE c LENGTH 05 VALUE 'xJust',
           cancnfedataemis TYPE c LENGTH 08 VALUE 'dataEmis'.

CONSTANTS: canccte         TYPE c LENGTH 07 VALUE 'CancCTe',
           cancctecnpj     TYPE c LENGTH 04 VALUE 'CNPJ',
           canccteie       TYPE c LENGTH 04 VALUE 'IE',
           cancctenct      TYPE c LENGTH 05 VALUE 'nCT',
           cancctenserie   TYPE c LENGTH 06 VALUE 'nSerie',
           cancctexjust    TYPE c LENGTH 05 VALUE 'xJust',
           cancctedataemis TYPE c LENGTH 08 VALUE 'dataEmis'.
"Carta de Correção NFe
CONSTANTS: ccenfechave  TYPE c LENGTH 11 VALUE 'CCeNFeChave',
           cc_chnfe     TYPE c LENGTH 05 VALUE 'chNFe',
           cc_xcorrecao TYPE c LENGTH 09 VALUE 'xCorrecao'.

"Carta de Correção CTe
CONSTANTS: ccectechave     TYPE c LENGTH 11 VALUE 'CCeCTe',
           chcte           TYPE c LENGTH 05 VALUE 'chCTe',
           evccecte        TYPE c LENGTH 08 VALUE 'evCCeCTe',
           descevento      TYPE c LENGTH 10 VALUE 'descEvento',
           infcorrecao     TYPE c LENGTH 11 VALUE 'infCorrecao',
           grupoalterado   TYPE c LENGTH 13 VALUE 'grupoAlterado',
           campoalterado   TYPE c LENGTH 13 VALUE 'campoAlterado',
           valoralterado   TYPE c LENGTH 13 VALUE 'xCorrecao',
           nroitemalterado TYPE c LENGTH 15 VALUE 'nroItemAlterado',
           xconduso        TYPE c LENGTH 08 VALUE 'xCondUso'.


" Constantes diversas
CONSTANTS: c_pt(2)             TYPE c VALUE 'PT',
           c_0                 TYPE c VALUE '0',
           c_00                TYPE c LENGTH 2 VALUE '00',
           c_1                 TYPE c VALUE '1',
           c_2                 TYPE c VALUE '2',
           c_3                 TYPE c VALUE '3',
           c_4                 TYPE c VALUE '4',
           c_5                 TYPE c VALUE '5',
           c_40(2)             TYPE c VALUE '40',
           c_864(3)            TYPE c VALUE '863',       "Movimento de estorno
           c_c                 TYPE c VALUE 'C',
           c_j                 TYPE c VALUE 'J',        "Entrega
           c_m                 TYPE c VALUE 'M',        "Fatura
           c_n                 TYPE c VALUE 'N',
           c_r                 TYPE c VALUE 'R',        "Movimento de Mercadoria
           c_bi(2)             TYPE c VALUE 'BI',       "Fatura
           c_md(2)             TYPE c VALUE 'MD',       "Documento de material
           c_q                 TYPE c VALUE 'Q',
           c_li(2)             TYPE c VALUE 'LI',
           c_8                 TYPE c VALUE '8',
           c_7                 TYPE c VALUE '7',
           c_9                 TYPE c VALUE '9',
           c_f                 TYPE c VALUE 'F',
           c_p                 TYPE c VALUE 'P',
           c_s                 TYPE c VALUE 'S',
           c_t                 TYPE c VALUE 'T',
           c_x                 TYPE c VALUE 'X',
           c_pc(2)             TYPE c VALUE 'PC',
           c_lr(2)             TYPE c VALUE 'LR',
           c_lf(2)             TYPE c VALUE 'LF',
           c_ag(2)             TYPE c VALUE 'AG',
           c_mt(2)             TYPE c VALUE 'MT',
           c_pv(2)             TYPE c VALUE 'PV',
           c_rg(2)             TYPE c VALUE 'RG',
           c_we(2)             TYPE c VALUE 'WE',
           c_sg(2)             TYPE c VALUE 'SG',
           c_z001(4)           TYPE c VALUE 'Z001',
           c_z004(4)           TYPE c VALUE 'Z004',
           c_z005(4)           TYPE c VALUE 'Z005', "CS2021001045 - 08.0.022 - JT
           c_z009(4)           TYPE c VALUE 'Z009',
           c_z020(4)           TYPE c VALUE 'Z020',
           c_z021(4)           TYPE c VALUE 'Z021',
           c_z026(4)           TYPE c VALUE 'Z026',
           c_z018(4)           TYPE c VALUE 'Z018',
           c_z027(4)           TYPE c VALUE 'Z027',
           c_zicm(4)           TYPE c VALUE 'ZICM',
           c_zicc(4)           TYPE c VALUE 'ZICC',
           c_zipt(4)           TYPE c VALUE 'ZIPT',
           c_zped(4)           TYPE c VALUE 'ZPED',
           c_zset(4)           TYPE c VALUE 'ZSET',
           c_zirf(4)           TYPE c VALUE 'ZIRF',
           c_ziof(4)           TYPE c VALUE 'ZIOF',
           z_zfre(4)           TYPE c VALUE 'ZFRE',
           z_zlot(4)           TYPE c VALUE 'ZLOT',
           c_zseg(4)           TYPE c VALUE 'ZSEG',
           c_zadm(4)           TYPE c VALUE 'ZADM',
           c_zins(4)           TYPE c VALUE 'ZINS',
           c_ziss(4)           TYPE c VALUE 'ZISS',
           c_zvct(4)           TYPE c VALUE 'ZVCT',

           c_zadh(4)           TYPE c VALUE 'ZADH',
           c_zhi1(4)           TYPE c VALUE 'ZHI1',
           c_zbh1(4)           TYPE c VALUE 'ZBH1',
           c_zsgh(4)           TYPE c VALUE 'ZSGH',
           c_zifh(4)           TYPE c VALUE 'ZIFH',
           c_zdeh(4)           TYPE c VALUE 'ZDEH',


           c_br(2)             TYPE c VALUE 'BR',
           c_ro(2)             TYPE c VALUE 'RO',
           c_ma(2)             TYPE c VALUE 'MA',
           c_pi(2)             TYPE c VALUE 'PI',
           c_frete_peso(11)    TYPE c VALUE 'FRETE PESO',
           c_01(2)             TYPE c VALUE '01',
           c_02(2)             TYPE c VALUE '02',
           c_03(2)             TYPE c VALUE '03',
           c_04(2)             TYPE c VALUE '04',
           c_05(2)             TYPE c VALUE '05',
           c_10(2)             TYPE c VALUE '10',
           c_20(2)             TYPE c VALUE '20',
           c_30(2)             TYPE c VALUE '30',
           c_41(2)             TYPE c VALUE '41',
           c_50(2)             TYPE c VALUE '50',
           c_51(2)             TYPE c VALUE '51',
           c_60(2)             TYPE c VALUE '60',
           c_70(2)             TYPE c VALUE '70',
           c_90(2)             TYPE c VALUE '90',
           c_kg(2)             TYPE c VALUE 'KG',
           c_to(2)             TYPE c VALUE 'TO',
           c_un(2)             TYPE c VALUE 'UN',
           c_lt(2)             TYPE c VALUE 'LT',
           c_sn(2)             TYPE c VALUE 'SN',
           c_r56(3)            TYPE c VALUE 'R56',
           c_r58(3)            TYPE c VALUE 'R58',
           c_o46(3)            TYPE c VALUE 'O46',
           c_o47(3)            TYPE c VALUE 'O47',
           c_o48(3)            TYPE c VALUE 'O48',
           c_o49(3)            TYPE c VALUE 'O49',
           c_o50(3)            TYPE c VALUE 'O50',
           c_o51(3)            TYPE c VALUE 'O51',
           c_t45(3)            TYPE c VALUE 'T45',
           c_t46(3)            TYPE c VALUE 'T46',
           c_t47(3)            TYPE c VALUE 'T47',
           c_t48(3)            TYPE c VALUE 'T48',
           c_t49(3)            TYPE c VALUE 'T49',
           c_t50(3)            TYPE c VALUE 'T50',
           c_pi4(3)            TYPE c VALUE 'PI4',
           c_pi5(3)            TYPE c VALUE 'PI5',
           c_pi6(3)            TYPE c VALUE 'PI6',
           c_pi7(3)            TYPE c VALUE 'PI7',
           c_pi8(3)            TYPE c VALUE 'PI8',
           c_m86(3)            TYPE c VALUE 'M86',
           c_peso_bruto(10)    TYPE c VALUE 'PESO BRUTO',
           c_caixa(5)          TYPE c VALUE 'CAIXA',
           c_litragem(8)       TYPE c VALUE 'LITRAGEM',
           c_0001              TYPE thead-tdid  VALUE '0001',
           c_0002(4)           TYPE c VALUE '0002',
           c_000010(6)         TYPE c VALUE '000010',
           c_0000000001(10)    TYPE c VALUE '0000000001',
           c_vbbk              TYPE thead-tdobject VALUE 'VBBK',
           c_vbbp              TYPE thead-tdobject VALUE 'VBBP',
           c_subcontratado(13) TYPE c VALUE 'Subcontratado',
           c_cnpj(4)           TYPE c VALUE 'CNPJ',
           c_cpf(3)            TYPE c VALUE 'CPF',
           c_ie(2)             TYPE c VALUE 'IE',
           c_placa_cavalo(12)  TYPE c VALUE 'Placa Cavalo',
           c_placa_carreta(13) TYPE c VALUE 'Placa Carreta',
           c_transbordo(30)    TYPE c VALUE 'Local de entrega: Transbordo',
           c_hifen             TYPE c VALUE '-',
           c_barra             TYPE c VALUE '/',
           c_toma_ped0         TYPE c VALUE '0',
           c_toma_ped1         TYPE c VALUE '1',
           c_toma_ped2         TYPE c VALUE '2',
           c_toma_ped3         TYPE c VALUE '3',
           c_toma_ped4         TYPE c VALUE '4',
           c_toma_ped5         TYPE c VALUE '5',
           c_zrem              TYPE c LENGTH 4 VALUE 'ZREM'.

********************************************************************************************************************
* Estruturas para boleto
********************************************************************************************************************
TYPES:
  BEGIN OF ty_zsdt0054,
    nro_sol_ov TYPE zsdt0054-nro_sol_ov,
    posnr      TYPE zsdt0054-posnr,
    valdt      TYPE zsdt0054-valdt,
    dmbtr      TYPE zsdt0054-dmbtr,
    adiant     TYPE zsdt0054-adiant,
  END OF ty_zsdt0054,

  BEGIN OF ty_zsdt0051,
    nro_sol_ov TYPE zsdt0051-nro_sol_ov,
    vkorg      TYPE zsdt0051-vkorg,
  END OF ty_zsdt0051,

  BEGIN OF ty_j_1bnflin,
    docnum TYPE j_1bnflin-docnum,
    refkey TYPE j_1bnflin-refkey,
    reftyp TYPE j_1bnflin-reftyp,
    awkey  TYPE bkpf-awkey,
  END OF ty_j_1bnflin,

  BEGIN OF ty_j_1bnfdoc,
    docnum TYPE j_1bnfdoc-docnum,
    bukrs  TYPE j_1bnfdoc-bukrs,
    pstdat TYPE j_1bnfdoc-pstdat,
    nfenum TYPE j_1bnfdoc-nfenum,
  END OF ty_j_1bnfdoc,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
    belnr TYPE bkpf-belnr,
  END OF ty_bkpf,

  BEGIN OF ty_bsid,
    bukrs   TYPE bsid-bukrs,
    belnr   TYPE bsid-belnr,
    gjahr   TYPE bsid-gjahr,
    dmbtr   TYPE bsid-dmbtr,
    zbd1t   TYPE bsid-zbd1t,
    zfbdt   TYPE bsid-zfbdt,
    gsber   TYPE bsid-gsber,
    hbkid   TYPE bsid-hbkid,
    kunnr   TYPE bsid-kunnr,
    lifnr_e TYPE lfa1-lifnr,
    lifnr_f TYPE lfa1-lifnr,
  END OF ty_bsid,

  BEGIN OF ty_zfit0048,
    bukrs         TYPE zfit0048-bukrs,
    werks         TYPE zfit0048-werks,
    kunnr         TYPE zfit0048-kunnr,
    txt_instrucao TYPE zfit0048-txt_instrucao,
  END OF ty_zfit0048,

  BEGIN OF ty_t045t,
    bukrs TYPE t045t-bukrs,
    hbkid TYPE t045t-hbkid,
    dtaid TYPE t045t-dtaid,
  END OF ty_t045t,

  BEGIN OF ty_t012k,
    bukrs TYPE t012k-bukrs,
    hbkid TYPE t012k-hbkid,
    bankn TYPE t012k-bankn,
    bkont TYPE t012k-bkont,
  END OF ty_t012k,

  BEGIN OF ty_t012,
    bukrs TYPE t012-bukrs,
    hbkid TYPE t012-hbkid,
    bankl TYPE t012-bankl,
  END OF ty_t012,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
    stras TYPE lfa1-stras,
    ort02 TYPE lfa1-ort02,
    pstlz TYPE lfa1-pstlz,
    ort01 TYPE lfa1-ort01,
    regio TYPE lfa1-regio,
    stcd1 TYPE lfa1-stcd1,
  END OF ty_lfa1,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
    stras TYPE kna1-stras,
    ort02 TYPE kna1-ort02,
    pstlz TYPE kna1-pstlz,
    ort01 TYPE kna1-ort01,
    regio TYPE kna1-regio,
    stcd1 TYPE kna1-stcd1,
    stcd2 TYPE kna1-stcd2, "BUG - 83643
    stkzn TYPE kna1-stkzn, "BUG - 83643
  END OF ty_kna1,

  BEGIN OF ty_saida,
    xename1                TYPE lfa1-name1,
    dtaid                  TYPE t045t-dtaid,
    xestras                TYPE lfa1-stras,
    xeort02                TYPE lfa1-ort02,
    xeort01                TYPE lfa1-ort01,
    xeregio                TYPE lfa1-regio,
    xepstlz                TYPE lfa1-pstlz,
    nfenum                 TYPE j_1bnfdoc-nfenum,
    refkey                 TYPE j_1bnflin-refkey,
    xestcd1                TYPE lfa1-stcd1,
    zbd1t                  TYPE bsid-zfbdt,
    dmbtr                  TYPE bsid-dmbtr,
    name1                  TYPE kna1-name1,
    stcd1                  TYPE kna1-stcd1,
    txt_instrucao          TYPE zfit0048-txt_instrucao,
    data_sist              TYPE sy-datum,
    bankl                  TYPE t012-bankl,
    bankn                  TYPE t012k-bankn,
    bkont                  TYPE t012k-bkont,
    stras                  TYPE kna1-stras,
    ort01                  TYPE kna1-ort01,
    regio                  TYPE kna1-regio,
    pstlz                  TYPE kna1-pstlz,
    xfname1                TYPE lfa1-name1,
    xfstcd1                TYPE lfa1-stcd1,
    var_cod_barras_fim(44),
    var_linha_dig(54),
  END OF ty_saida.



DATA: vdocnum               TYPE j_1bnfdoc-docnum,
      vl_form               TYPE tdsfname,
      vl_name               TYPE rs38l_fnam,
      vdv_nosso(1),
      var_total_trib        TYPE j_1btaxval,
      var_total_trib2       TYPE j_1btaxval,
      var_total_deson       TYPE j_1btaxval,
      var_total_vbicm_51    TYPE j_1btaxval,
      var_total_desc        TYPE j_1btaxval,
      var_total_outros      TYPE j_1btaxval,
      var_tot_icms_uf_dest  TYPE j_1btaxval,
      var_tot_icms_uf_remet TYPE j_1btaxval,
      var_suframa           TYPE c.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: it_j_1bnflin TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnfdoc TYPE TABLE OF ty_j_1bnfdoc,
      it_bkpf      TYPE TABLE OF ty_bkpf,
      it_bsid      TYPE TABLE OF ty_bsid,
      it_zfit0048  TYPE TABLE OF ty_zfit0048,
      it_t045t     TYPE TABLE OF ty_t045t,
      it_t012k     TYPE TABLE OF ty_t012k,
      it_t012      TYPE TABLE OF ty_t012,
      it_lfa1      TYPE TABLE OF ty_lfa1,
      it_kna1      TYPE TABLE OF ty_kna1,
      it_zsdt0051  TYPE TABLE OF ty_zsdt0051,
      it_zsdt0054  TYPE TABLE OF ty_zsdt0054,
      it_saida     TYPE TABLE OF zfi_boleto. " TY_SAIDA.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_j_1bnflin TYPE ty_j_1bnflin,
      wa_j_1bnfdoc TYPE ty_j_1bnfdoc,
      wa_bkpf      TYPE ty_bkpf,
      wa_bsid      TYPE ty_bsid,
      wa_zfit0048  TYPE ty_zfit0048,
      wa_t045t     TYPE ty_t045t,
      wa_t012k     TYPE ty_t012k,
      wa_t012      TYPE ty_t012,
      wa_lfa1      TYPE ty_lfa1,
      wa_kna1      TYPE ty_kna1,
      wa_zsdt0051  TYPE ty_zsdt0051,
      wa_zsdt0054  TYPE ty_zsdt0054,
      wa_saida     TYPE zfi_boleto. "TY_SAIDA.


*----------------------------------------------------------------------*
* POPUP IMPRESSAO
*----------------------------------------------------------------------*
DATA: rb_email      VALUE 'X',
      rb_imprimir,
      p_email(100),
      p_xblnr       TYPE bkpf-xblnr,
      t_doc_numero  TYPE  j_1bdocnum,
      wl_instrucoes TYPE  zfi_boleto,
      t_tipo        TYPE  char1,
      t_hbkid       TYPE  t012t-hbkid,
      ok-code       TYPE  sy-ucomm.

DATA: vg_netoth    TYPE j_1bnflin-netoth.

DATA: vg_versao_cte TYPE j_1bnfdoc-xmlvers,
      vg_versao_nfe TYPE j_1bnfdoc-xmlvers.


DATA: it_j1b_nf_xml_badi_item TYPE z_j1b_nf_xml_badi_item_t.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01,
      vg_ch_referencia          TYPE zsdt0001-ch_referencia.
*-#133089-12.02.2024-JT-fim

*-US 140617-30.12.2024-#140617-JT-inicio
DATA: lc_integra_tip          TYPE REF TO zcl_integracao_tip.
*-US 140617-30.12.2024-#140617-JT-fim
