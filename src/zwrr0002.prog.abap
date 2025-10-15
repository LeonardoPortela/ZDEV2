*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 03/08/2010                                              &*
*& Descrição: Automatização Nota Fiscal Writer                        &*
*& Transação: ZNFW0001                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*& Marcos Faneli   DEVK937099   29.04.2014                            &*
*& Sara Oikawa     DEVK9A0J9H   23.04.2020 [CS2019001879]             &*
*&--------------------------------------------------------------------&*

REPORT  ZWRR0002_031121 MESSAGE-ID ZNFW.
TYPE-POOLS: VRM.
INCLUDE <ICON>.
TABLES: ZFIWRT0008,
        ZSDT0075.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF TY_FISCAL,
         NFTYPE         TYPE ZFIWRT0001-NFTYPE,
         ITMTYP         TYPE ZFIWRT0001-ITMTYP,
         DIAS           TYPE ZFIWRT0001-DIAS,
         RETORNO        TYPE ZFIWRT0001-RETORNO,
         ZPESAGEM       TYPE ZFIWRT0001-ZPESAGEM,
         IMOBILIZADO    TYPE ZFIWRT0001-IMOBILIZADO,
         TP_MV_IMOB     TYPE ZFIWRT0001-TP_MV_IMOB,
         DOCREF         TYPE ZFIWRT0008-DOCREF,
         NR_ROMANEIO    TYPE ZFIWRT0008-NR_ROMANEIO,
         MOVE_PLANT     TYPE ZFIWRT0008-MOVE_PLANT,
         MOVE_STLOC     TYPE ZFIWRT0008-MOVE_STLOC,
         CTRL_ZRFL      TYPE ZFIWRT0001-CTRL_ZRFL,
         ENERGIA        TYPE ZFIWRT0008-ENERGIA,
         DISP_NF_CCT    TYPE ZFIWRT0008-DISP_NF_CCT,
         SERVICO        TYPE ZFIWRT0008-SERVICO,
         TRANSF_ICMS    TYPE ZFIWRT0001-TRANSF_ICMS,
         COMPLEMENTO    TYPE ZFIWRT0008-COMPLEMENTO,
         INCO1          TYPE J_1BDYDOC-INCO1,
         INCO2          TYPE J_1BDYDOC-INCO2,
         AVISO_REC      TYPE ZFIWRT0001-AVISO_REC,
         LM_ESTOQUE     TYPE ZFIWRT0001-LM_ESTOQUE,
         EBELN          TYPE ZFIWRT0008-EBELN,
         VBELN          TYPE VBAP-VBELN,
         KOSTL          TYPE ZFIWRT0008-KOSTL,
         REFERENCIA     TYPE ZFIWRT0008-REFERENCIA,
         ACCESS_KEY(54), "  TYPE ZFIWRT0008-ACCESS_KEY,
         KONTO          TYPE ZFIWRT0008-KONTO,
         MOVE_MAT       TYPE ZFIWRT0008-MOVE_MAT,
         MOVE_BATCH     TYPE ZFIWRT0008-MOVE_BATCH,
         BKTXT          TYPE ZFIWRT0008-BKTXT,
         MTSNR          TYPE ZFIWRT0008-MTSNR,
       END OF TY_FISCAL,

       BEGIN OF TY_SEQ_LCTO,
         SEQ_LCTO TYPE ZFIWRT0008-SEQ_LCTO,
       END OF TY_SEQ_LCTO  ,

       BEGIN OF TY_DIREITOS,
         CFOP     TYPE ZFIWRT0006-CFOP,
         TAXLW1   TYPE ZFIWRT0006-TAXLW1,
         TAXLW2   TYPE ZFIWRT0006-TAXLW2,
         TAXLW4   TYPE ZFIWRT0006-TAXLW4,
         TAXLW5   TYPE ZFIWRT0006-TAXLW5,
         INDCOPER TYPE ZFIWRT0006-INDCOPER,
         OPERTYP  TYPE ZFIWRT0006-OPERTYP,
         TAXCODE  TYPE ZFIWRT0006-TAXCODE,
       END OF TY_DIREITOS,

       BEGIN OF TY_FIELDS,
         CAMPO(30) TYPE C,
         GROUP1(5) TYPE C,
         VALUE     TYPE SY-TABIX,
         INVISIBLE TYPE SY-TABIX,
       END   OF TY_FIELDS,

       BEGIN OF TY_EDITOR,
         LINE(72),
       END   OF TY_EDITOR,

       BEGIN OF TY_APROV,
         NIVEL_APROV  TYPE ZFIWRT0007-NIVEL_APROV,
         USNAM        TYPE ZFIWRT0007-USNAM,
         NOME(80),
         DEPARTAMENTO TYPE ZFIWRT0007-DEPARTAMENTO,
       END OF TY_APROV,

       BEGIN OF TY_PARC,
         PARVW    TYPE ZFIWRT0015-PARVW,
         PARID    TYPE ZFIWRT0015-PARID,
         NOME(80),
         STYLE    TYPE LVC_T_STYL,
       END OF TY_PARC,

       BEGIN OF TY_DOCS,
         DOCNUM        TYPE ZFIWRT0008-DOCNUM,
         BELNR         TYPE ZFIWRT0008-BELNR,
         MBLNR         TYPE ZFIWRT0008-MBLNR,
         BUDAT         TYPE ZFIWRT0008-BUDAT,
         BLDAT         TYPE ZFIWRT0008-BLDAT,
         BRANCH        TYPE ZFIWRT0008-BRANCH,
         NFENUM        TYPE J_1BNFDOC-NFENUM,
         SERIES        TYPE J_1BNFDOC-SERIES,
         TCODE_ORG     TYPE ZFIWRT0008-TCODE_ORG,
         NOT_CHECK_XML TYPE ZFIWRT0008-NOT_CHECK_XML,
         LOC_CARREGA   TYPE ZFIWRT0008-LOC_CARREGA, "CS2020001418 - CSB
       END OF TY_DOCS,

       BEGIN OF TY_MATNR_OV_PD,
         VBELN TYPE VBAK-VBELN,
         POSNR TYPE VBAP-POSNR,
         EBELN TYPE EKKO-EBELN,
         MATNR TYPE VBAP-MATNR,
         MEINS TYPE EKPO-MEINS,
         MAKTX TYPE MAKT-MAKTX,
         STEUC TYPE MARC-STEUC,
         WERKS TYPE EKPO-WERKS,
         LGORT TYPE EKPO-LGORT,
         CHARG TYPE EKET-CHARG,
       END OF TY_MATNR_OV_PD,

       BEGIN OF TY_TRANS,
         LIFNR      TYPE ZFIWRT0019-LIFNR,
         PLACA      TYPE ZFIWRT0019-PLACA,
         ANZPK      TYPE ZFIWRT0019-ANZPK,
         SHPUNT     TYPE ZFIWRT0019-SHPUNT,
         NTGEW      TYPE ZFIWRT0019-NTGEW,
         BRGEW      TYPE ZFIWRT0019-BRGEW,
         UFPLACA    TYPE ZFIWRT0019-UFPLACA,
         PLACA_CAR1 TYPE ZFIWRT0019-PLACA_CAR1,
         PLACA_CAR2 TYPE ZFIWRT0019-PLACA_CAR2,
         PLACA_CAR3 TYPE ZFIWRT0019-PLACA_CAR3,
         MOTORISTA  TYPE ZFIWRT0019-MOTORISTA,
       END OF TY_TRANS.

TYPES: BEGIN OF TY_DOC_REFS.
         INCLUDE STRUCTURE ZFIWRT0020_ALV.
TYPES:  END OF TY_DOC_REFS.

*-CS2021001266 - 15.12.2021 - JT- inicio
TYPES: BEGIN OF TY_REGULA,
         OPERACAO TYPE ZFIWRT0008-OPERACAO,
         RATE     TYPE ZFIWRT0010-RATE,
         SHIPTO   TYPE LFA1-REGIO.
TYPES: END   OF TY_REGULA.

DATA: T_REGULA TYPE TABLE OF TY_REGULA,
      W_REGULA TYPE TY_REGULA.
*-CS2021001266 - 15.12.2021 - JT- fim

DATA: W_ZFIWRT0001   TYPE ZFIWRT0001.  "*-CS2023000043-09.02.2023-#102019-JT

DATA: MANAGER        TYPE REF TO CL_GOS_MANAGER.
DATA: T_ZFIWRT0032 TYPE STANDARD TABLE OF ZFIWRT0032 INITIAL SIZE 0.

*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED,
            LCL_ALV_TOOLBAR2  DEFINITION DEFERRED,
            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED. "US #169751 - MMSILVA - 07.03.2025
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER           TYPE SCRFNAME VALUE 'CC_ITENS_NOTA',
      G_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1           TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2           TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER              TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                 TYPE REF TO CL_GUI_ALV_GRID,
      GRID2                 TYPE REF TO CL_GUI_ALV_GRID,
      GRID4                 TYPE REF TO CL_GUI_ALV_GRID,
      GRID5                 TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR           TYPE REF TO LCL_ALV_TOOLBAR,
      OBG_TOOLBAR2          TYPE REF TO LCL_ALV_TOOLBAR2,
      OBG_TOOLBAR3          TYPE REF TO LCL_ALV_TOOLBAR3, "US #169751 - MMSILVA - 07.03.2025
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBARMANAGER2 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBARMANAGER3 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      G_DESCBOX             TYPE SCRFNAME VALUE 'CC_DESC',
      G_CUSTOM_CONT_DESC    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_DESCBOX           TYPE REF TO CL_GUI_TEXTEDIT,
      OBG_DOCKING           TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GRID3                 TYPE REF TO CL_GUI_ALV_GRID,
      OBG_CONTEINER_CONTAB  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_CONTEINER_APROV   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_CONTEINER_PARC    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CC_CONTAB           TYPE SCRFNAME VALUE 'CC_CONTAB',
      G_CC_APROV            TYPE SCRFNAME VALUE 'CC_APROV',
      G_CC_PARC             TYPE SCRFNAME VALUE 'CC_PARC',
      WA_STYLE              TYPE LVC_S_STYL,
      STYLE                 TYPE LVC_T_STYL  WITH HEADER LINE,
      STYLE2                TYPE LVC_T_STYL WITH HEADER LINE.

*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.
*** TREE DE MENSAGENS.
DATA NODE_ITAB LIKE NODE_STR OCCURS 0.
DATA NODE      LIKE NODE_STR.

DATA CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA SPLITTER_MSG TYPE REF TO CL_GUI_EASY_SPLITTER_CONTAINER.
DATA RIGHT        TYPE REF TO CL_GUI_CONTAINER.
DATA LEFT         TYPE REF TO CL_GUI_CONTAINER.

DATA EDITOR TYPE REF TO CL_GUI_TEXTEDIT.
DATA TREE   TYPE REF TO CL_GUI_SIMPLE_TREE.

DATA BEHAVIOUR_LEFT  TYPE REF TO CL_DRAGDROP.
DATA BEHAVIOUR_RIGHT TYPE REF TO CL_DRAGDROP.

DATA HANDLE_TREE TYPE I.
DATA NUM_ROW     TYPE I VALUE 0.
DATA VINICIOU_LCTO_ZNFW0009.
DATA: VLR_TOTAL_NOTA LIKE ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B,
      VLR_TOTAL_ITEM LIKE ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B.
DATA VSUGERE_ITENS_ZNFW0009.
DATA VLANCAMENTO_ZNFW0009(3) TYPE C.

DATA: VL_PRAZO TYPE ZE_PRAZO.

DATA: LIT_ZIB_NFE_DIST_ITM TYPE TABLE OF ZIB_NFE_DIST_ITM.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
           C_1               TYPE C VALUE '1',
           C_2               TYPE C VALUE '2',
           C_B               TYPE C VALUE 'B',
           C_S               TYPE C VALUE 'S',
           C_L               TYPE C VALUE 'L',
           C_X               TYPE C VALUE 'X',
           C_D               TYPE C VALUE 'D',
           C_K               TYPE C VALUE 'K',
           C_W               TYPE C VALUE 'W',
           C_F               TYPE C VALUE 'F',
           C_T               TYPE C VALUE 'T',
           C_I               TYPE C VALUE 'I',
           C_N               TYPE C VALUE 'N',
           C_H               TYPE C VALUE 'H',
           C_AG(2)           TYPE C VALUE 'AG',
           C_SP(2)           TYPE C VALUE 'SP',
           C_LR(2)           TYPE C VALUE 'LR',
           C_PC(2)           TYPE C VALUE 'PC',
           C_NE(2)           TYPE C VALUE 'NE',
           C_01(2)           TYPE C VALUE '01',
           C_30(2)           TYPE C VALUE '30',
           C_40(2)           TYPE C VALUE '40',
           C_43(2)           TYPE C VALUE '43', "RJF - Antonio solicitou voltar versão
           C_31(2)           TYPE C VALUE '31', "RJF - Antonio solicitou voltar versão
           C_50(4)           TYPE C VALUE '0050',
           C_76(2)           TYPE C VALUE '76',
           C_71(2)           TYPE C VALUE '71',
           C_72(2)           TYPE C VALUE '72',
           C_BR(2)           TYPE C VALUE 'BR',
           C_LF(2)           TYPE C VALUE 'LF',
           "C_LR(2)           TYPE C VALUE 'LR',
           C_Z1(2)           TYPE C VALUE 'Z1',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DG1(3)          TYPE C VALUE 'DG1',
           C_DG2(3)          TYPE C VALUE 'DG2',
           C_DUMMY_HEADER(3) TYPE C VALUE '099',
           C_DUMMY_ITENS(3)  TYPE C VALUE '098',
           C_ICM3(4)         TYPE C VALUE 'ICM3',
           C_IPIS(4)         TYPE C VALUE 'IPIS',
           C_ICOF(4)         TYPE C VALUE 'ICOF',
           C_ICS1(4)         TYPE C VALUE 'ICS1',
           C_ICOP(4)         TYPE C VALUE 'ICOP',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_ROOT(4)         TYPE C VALUE 'ROOT',
           C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
           C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_DESAT(5)        TYPE C VALUE 'DESAT',
           C_DMBTR(5)        TYPE C VALUE 'DMBTR',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_GERAR_NF(8)     TYPE C VALUE 'GERAR_NF',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE',
           C_DISPLAY(10)     TYPE C VALUE 'ZNFW0002_D',
           C_LIM(3)          TYPE C VALUE 'LIM',
           C_EXEC(4)         TYPE C VALUE 'EXEC'. "US 163043 - MMSILVA - 18.03.2025

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE              TYPE SY-UCOMM,
      P_SEQ_LCTO           TYPE ZFIWRT0008-SEQ_LCTO,
      P_OPERACAO           TYPE ZFIWRT0001-OPERACAO,
      P_OPERACAO_OLD       TYPE ZFIWRT0001-OPERACAO,  "*-CS2023000043-09.02.2023-#102019-JT
      P_BUKRS              TYPE ZFIWRT0008-BUKRS,
      P_BRANCH             TYPE ZFIWRT0008-BRANCH,
      P_PARVW              TYPE ZFIWRT0008-PARVW,
      P_PARID              TYPE ZFIWRT0008-PARID,
      P_LOEKZ(6)           TYPE C,
      P_CHAVE_ACESSO       TYPE ZFIT0230-CHAVE_ACESSO, "US #172277 - MMSILVA - 28.03.2025
*      p_chave_acesso       type zib_nfe_dist_ter-chave_nfe, "US #163043 - MMSILVA - 18.03.2025 //// US #172277 - MMSILVA - 28.03.2025 - Comentado devido nova formatação
      WG_DESC_OPERACAO(30),
      WG_DESC_BRANCH(30),
      WG_DESC_PARVW(30),
      WG_DESC_PARID(30),
      WG_DESC_BUKRS(30),
      WG_OP_FISCAL(50),
      WG_DESC_NFTYPE       TYPE J_1BAAT-NFTTXT,
      WG_DESC_ITMTYP       TYPE J_1BITEMTYPEST-TEXT,
      WG_DESC_CFOP         TYPE J_1BAGNT-CFOTXT,
      WG_DESC_TAXLW1       TYPE J_1BATL1T-DESCRIP,
      WG_DESC_TAXLW2       TYPE J_1BATL2T-DESCRIP,
      WG_DESC_TAXLW4       TYPE J_1BATL4T-DESCRIP,
      WG_DESC_TAXLW5       TYPE J_1BATL5T-DESCRIP,
      WG_DESC_TAXCODE      TYPE J_1BTXSDCT-TXT,
      WG_FISCAL            TYPE TY_FISCAL,
      WG_DIREITOS          TYPE TY_DIREITOS,
      WG_DOCS              TYPE TY_DOCS,
      WG_TRANSPORTE        TYPE TY_TRANS,
      WG_TITULO(50)        VALUE 'Dados de Cabeçalho',
      WG_TITULO2(50)       VALUE 'Dados de Itens',
      VG_SUBSCREEN1        TYPE SY-DYNNR VALUE '213',
      VG_SUBSCREEN2        TYPE SY-DYNNR VALUE '214',
      WG_DG1(4)            VALUE C_MINIMIZAR,
      WG_DG2(4)            VALUE C_MINIMIZAR,
      WG_SHIPFROM          TYPE LFA1-REGIO,
      WG_SHIPTO            TYPE LFA1-REGIO,
      WG_EDITOR            TYPE TY_EDITOR,
      TG_EDITOR            TYPE TABLE OF TY_EDITOR,
      TG_TBSL              TYPE TABLE OF TBSL WITH HEADER LINE,
      TG_FIELDS            TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_SELECTEDCELL      TYPE LVC_T_CELL,
      WG_SELECTEDCELL      TYPE LVC_S_CELL,
      WG_DCLKNODEKEY       TYPE TV_NODEKEY,
      WG_SUGERE_TRANSP     TYPE C,
      V_AUTOMATICO_MEMO    TYPE CHAR1,

      WG_ACAO(30),
      WG_FLAG,
      X_FIELD(30),
      WG_MENSAGEM(30),
      TG_APROV             TYPE TABLE OF TY_APROV WITH HEADER LINE,
      TG_PARC              TYPE TABLE OF TY_PARC WITH HEADER LINE,
      G_INIT_ONCE,
      XBOL(2),
      V_ZLSCH              TYPE ZFIWRT0011-ZLSCH,
      IT_SEQ_LCTO          TYPE TABLE OF TY_SEQ_LCTO WITH HEADER LINE,
      VG_MOD_FIELDCAT      TYPE BOOLEAN.

DATA: T_FIELDCATALOG     TYPE LVC_T_FCAT,
      T_FIELDCATALOG_GET TYPE LVC_T_FCAT,
      W_FIELDCATALOG     TYPE LVC_S_FCAT,
      WA_LAYOUT          TYPE LVC_S_LAYO,
      WA_STABLE_ITENS    TYPE LVC_S_STBL,
      WA_STABLE          TYPE LVC_S_STBL.

FIELD-SYMBOLS: <FS_GET> TYPE LVC_S_FCAT.

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

DATA: BEGIN OF TG_IMPO2 OCCURS 0,

        LINE     TYPE I,
        TAXTYP   TYPE ZFIWRT0010-TAXTYP,
        TTYPETXT TYPE J_1BTTYTXT,
        TAXGRP   TYPE J_1BTAXGRP,
        BASE     TYPE ZFIWRT0010-BASE,
        RATE     TYPE ZFIWRT0010-RATE,
        TAXVAL   TYPE ZFIWRT0010-TAXVAL,
        EXCBAS   TYPE ZFIWRT0010-EXCBAS,
        OTHBAS   TYPE ZFIWRT0010-OTHBAS,
        COLOR    TYPE C LENGTH 4,
      END OF TG_IMPO2.

** fim alteração Alexandre  ref; CS1016278 - IR107256 - 27.02.2003


DATA: BEGIN OF TG_IMPO OCCURS 0,
*      mark(1),
*      itmnum   TYPE zfiwrt0010-itmnum,
        TAXTYP     TYPE ZFIWRT0010-TAXTYP,
        TTYPETXT   TYPE J_1BTTYTXT,
        TAXGRP     TYPE J_1BTAXGRP,
        BASE       TYPE ZFIWRT0010-BASE,
        RATE       TYPE ZFIWRT0010-RATE,
        TAXVAL     TYPE ZFIWRT0010-TAXVAL,
        EXCBAS     TYPE ZFIWRT0010-EXCBAS,
        OTHBAS     TYPE ZFIWRT0010-OTHBAS,
        COLOR      TYPE C LENGTH 4,
*      incluido por alexandre Rimini 30.03.2023
        LINE       TYPE I,
        ICMS_BASE  TYPE ZFIWRT0010-BASE, "US #163043 - MMSILVA - 18.03.2025
        ICMS_RATE  TYPE ZFIWRT0010-RATE, "US #163043 - MMSILVA - 18.03.2025
        ICMS_VALUE TYPE ZFIWRT0010-TAXVAL, "US #163043 - MMSILVA - 18.03.2025
      END OF TG_IMPO,

      BEGIN OF TG_ITENS OCCURS 0,
        MARK(1),
        ITMNUM         TYPE ZFIWRT0009-ITMNUM,
        MATNR          TYPE ZFIWRT0009-MATNR,
        MAKTX          TYPE MAKT-MAKTX,
        CFOP(10),    "        TYPE ZFIWRT0006-CFOP,
        CHARG          TYPE ZFIWRT0009-CHARG,
        NR_FASE        TYPE ZFIWRT0009-NR_FASE, "// US-168932 WBARBOSA 20/05/2025
        WERKS          TYPE T001W-WERKS,
        LGORT          TYPE ZFIWRT0009-LGORT,
        MENGE          TYPE ZFIWRT0009-MENGE,
        MEINS          TYPE ZFIWRT0009-MEINS,
        NETPR          TYPE ZFIWRT0009-NETPR,
        NETWR          TYPE ZFIWRT0009-NETWR,
        ANLN1          TYPE ZFIWRT0009-ANLN1,
        ANLN2          TYPE ZFIWRT0009-ANLN2,
        STEUC          TYPE MARC-STEUC,
        VBELN          TYPE VBAP-VBELN,
        POSNR          TYPE VBAP-POSNR,
*-CS2020001331 - 06.10.2021 - JT - inicio
        POSSUI_ICMS_ST TYPE ZPOSSUI_ICMS_ST,
*-CS2020001331 - 06.10.2021 - JT - fim
        PROD_DESCRICAO TYPE ZIB_NFE_DIST_ITM-PROD_DESCRICAO, "US #163043 - MMSILVA - 18.03.2025
        VLR_ITEN_XML   TYPE ZFIWRT0009-NETWR,
        NCM_XML        TYPE ZIB_NFE_DIST_ITM-PROD_NCM,
        VLR_ICMS_XML   TYPE ZIB_NFE_DIST_ITM-ICMS_VALOR,
        RENAS          TYPE ATWRT,
        FASE(4),
        VBELN_R        TYPE ZFIWRT0009-VBELN_R,
        STYLE2         TYPE LVC_T_STYL,
        NETDIS         TYPE J_1BNETDIS,
        NETFRE         TYPE J_1BNETFRE,
        NETINS         TYPE J_1BNETINS,
        NETOTH         TYPE J_1BNETOTH,
        ADD_TEXT       TYPE CHAR01,
      END OF TG_ITENS,

      TG_DOCREFS TYPE TABLE OF TY_DOC_REFS WITH HEADER LINE,

      BEGIN OF TG_CONTAB OCCURS 0,
        BSCHL      TYPE ZFIWRT0011-BSCHL,
        HKONT      TYPE ZFIWRT0011-HKONT,
        TXT50      TYPE SKAT-TXT50,
        WAERS_I    TYPE ZFIWRT0011-WAERS_I,
        DMBTR      TYPE ZFIWRT0011-DMBTR,
        CURHA	     TYPE ZFIWRT0011-CURHA,
        DMBE2      TYPE ZFIWRT0011-DMBE2,
        CURIN	     TYPE ZFIWRT0011-CURIN,
        DMBE3      TYPE ZFIWRT0011-DMBE3,
        WAERS      TYPE ZFIWRT0011-WAERS,
        WRBTR      TYPE ZFIWRT0011-WRBTR,
        ARTNR      TYPE ZFIWRT0011-ARTNR,
        TAXTYP     TYPE ZFIWRT0011-TAXTYP,
        ESTORNO    TYPE ZFIWRT0011-ESTORNO,
        NEWBW      TYPE ZFIWRT0011-NEWBW,
        ZLSCH      TYPE ZFIWRT0011-ZLSCH,
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        TAXA_JUROS TYPE ZFIWRT0011-TAXA_JUROS,
        TAXA_MULTA TYPE ZFIWRT0011-TAXA_MULTA,
        HBKID      TYPE ZFIWRT0011-HBKID,
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        ZFBDT      TYPE ZFIWRT0011-ZFBDT,
        KOSTL      TYPE ZFIWRT0011-KOSTL,
        UMSKZ      TYPE ZFIWRT0011-UMSKZ,
        VBUND      TYPE ZFIWRT0011-VBUND,
        STYLE2     TYPE LVC_T_STYL,
      END OF TG_CONTAB,

      BEGIN OF TG_MENSAGEMS OCCURS 0,
        SEQNUM  TYPE ZFIWRT0005-SEQNUM,
        LINNUM  TYPE ZFIWRT0005-LINNUM,
        MESSAGE TYPE ZFIWRT0005-MESSAGE,
      END OF TG_MENSAGEMS,

      BEGIN OF TG_MOVEST OCCURS 0,
        BWART   TYPE ZFIWRT0004-BWART,
        TCODE   TYPE ZFIWRT0004-TCODE,
        MWSKZ1  TYPE ZFIWRT0004-MWSKZ1,
        ESTORNO TYPE ZFIWRT0004-ESTORNO,
      END OF TG_MOVEST,

      BEGIN OF GIT_MATNR_OV_PD OCCURS 0,
        VBELN TYPE VBAK-VBELN,
        POSNR TYPE VBAP-POSNR,
        EBELN TYPE EKKO-EBELN,
        MATNR TYPE VBAP-MATNR,
        MEINS TYPE EKPO-MEINS,
        MAKTX TYPE MAKT-MAKTX,
        STEUC TYPE MARC-STEUC,
        WERKS TYPE EKPO-WERKS,
        LGORT TYPE EKPO-LGORT,
        CHARG TYPE EKET-CHARG,
      END OF GIT_MATNR_OV_PD,

      BEGIN OF TG_IMPO_COMP OCCURS 0,
        ITMNUM TYPE ZFIWRT0010-ITMNUM,
        EDIT   TYPE CHAR1.
        INCLUDE STRUCTURE TG_IMPO.
DATA: END OF TG_IMPO_COMP,

*-CS2023000043-09.02.2023-#102019-JT-inicio
BEGIN OF TG_IMPO_GERA OCCURS 0.
  INCLUDE STRUCTURE TG_IMPO.
DATA: END OF TG_IMPO_GERA,
*-CS2023000043-09.02.2023-#102019-JT-fim

BEGIN OF TG_MARA_ITENS OCCURS 0,
  MATNR TYPE MARA-MATNR,
  MATKL TYPE MARA-MATKL,
END OF TG_MARA_ITENS.

DATA: TG_MSG_RET TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.
DATA: TG_IMPO_AUX LIKE TABLE OF TG_IMPO WITH HEADER LINE.
DATA: TG_MENSAGEMS_AUX LIKE TABLE OF TG_MENSAGEMS WITH HEADER LINE.
DATA: TG_MENSAGEMS_LIM LIKE TABLE OF TG_MENSAGEMS WITH HEADER LINE.
DATA: TG_MENSAGEMS_IMOB LIKE TABLE OF TG_MENSAGEMS WITH HEADER LINE.

*-CS2021000595 - 22.06.2021 - JT - inicio
TYPES: BEGIN OF TY_NCM_MAT,
         STEUC TYPE MARC-STEUC,
         MATNR TYPE MARC-MATNR.
TYPES: END   OF TY_NCM_MAT.

TYPES: BEGIN OF TY_DTLCTO,
         UNAME TYPE UNAME,
         BUDAT TYPE ZFIWRT0008-BUDAT.
TYPES: END   OF TY_DTLCTO.

DATA: T_TVARV   TYPE TABLE OF TVARVC,
      W_TVARV   TYPE TVARVC,
      T_NCM_MAT TYPE TABLE OF TY_NCM_MAT,
      W_NCM_MAT TYPE TY_NCM_MAT,
      T_SET     TYPE TABLE OF RGSB4,
      W_SET     TYPE RGSB4,
      T_DTLCTO  TYPE TABLE OF TY_DTLCTO,
      L_DATA    TYPE CHAR10,
      W_DTLCTO  TYPE TY_DTLCTO.
*-CS2021000595 - 22.06.2021 - JT - fim

DATA: IT_ITENS_NF LIKE TABLE OF TG_ITENS,
      WL_ITENS_NF LIKE LINE OF TG_ITENS.

* US #163043 - MMSILVA - 18.03.2025 - Inicio
TYPES: BEGIN OF TY_ICMS_XML,
         ICMS_BASE        TYPE ZIB_NFE_DIST_ITM-ICMS_BASE,
         ICMS_AQT         TYPE ZIB_NFE_DIST_ITM-ICMS_AQT,
         PROD_VLR_TOTAL_B TYPE ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B,
         IPI_VALOR        TYPE ZIB_NFE_DIST_ITM-IPI_VALOR,
         ICMS_ST_VALOR    TYPE ZIB_NFE_DIST_ITM-ICMS_ST_VALOR,
         PROD_QTD_COMERCI TYPE ZIB_NFE_DIST_ITM-PROD_QTD_COMERCI,
         PROD_VL_DESCONTO TYPE ZIB_NFE_DIST_ITM-PROD_VL_DESCONTO,
         ICMS_VALOR       TYPE ZIB_NFE_DIST_ITM-ICMS_VALOR,
       END OF TY_ICMS_XML.

DATA: T_ICMS_XML  TYPE TABLE OF TY_ICMS_XML,
      WA_ICMS_XML TYPE TY_ICMS_XML,
      WA_CTE_XML  TYPE ZIB_CTE_DIST_TER.

TYPES: BEGIN OF TY_ICMS_J1BTXIC3,
         ICMS_BASE TYPE ZFIWRT0010-BASE,
         ICMS_RATE TYPE ZFIWRT0010-RATE,
       END OF TY_ICMS_J1BTXIC3.

DATA: T_ICMS_J1BTXIC3  TYPE TABLE OF TY_ICMS_J1BTXIC3,
      WA_ICMS_J1BTXIC3 TYPE  TY_ICMS_J1BTXIC3.

DATA: WL_PARC TYPE TY_PARC.
* US #163043 - MMSILVA - 18.03.2025 - Fim

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF C_TAB_STRIP_NF,
             TAB1  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC1',
             TAB2  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC2',
             TAB3  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC3',
             TAB4  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC4',
             TAB5  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC5',
             TAB6  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC6',
             TAB7  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC7',
             TAB8  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC8',
             TAB9  LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC9',
             TAB10 LIKE SY-UCOMM VALUE 'TAB_STRIP_NF_FC10',
           END OF C_TAB_STRIP_NF.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  TAB_STRIP_NF TYPE TABSTRIP.
DATA: BEGIN OF G_TAB_STRIP_NF,
        SUBSCREEN   LIKE SY-DYNNR,
        PROG        LIKE SY-REPID VALUE 'ZWRR0002',
        PRESSED_TAB LIKE SY-UCOMM VALUE C_TAB_STRIP_NF-TAB1,
      END OF G_TAB_STRIP_NF.
DATA:      OK_CODE   LIKE SY-UCOMM.
DATA:      VAR_UCOMM TYPE SY-UCOMM.

DATA:
  GF_AUTHORIZATION_FT_09 TYPE C. "Workflow de Documentos

**********************************************************************
*DATA API LIM
**********************************************************************
DATA: IVALS           TYPE TABLE OF SVAL,
      XVALS           TYPE SVAL,
      WA_PARAM        TYPE ZSTRUCT_GET_DATA_LIM,
      V_IDENTIFICADOR TYPE STRING,
      LR_CONTENT      TYPE ZNFWE0002, "Estrutura Content
      LR_RESULTS      TYPE ZNFWE0003. "Estrutura com os campos do Content

DATA: VG_PARID TYPE WERKS_D.


*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.
  GF_AUTHORIZATION_FT_09 = ABAP_TRUE.

*-CS2021000595 - 22.06.2021 - JT - inicio
  FREE V_AUTOMATICO_MEMO.
  IMPORT V_AUTOMATICO_MEMO FROM MEMORY ID 'AUTOMATICO'.
  DELETE FROM MEMORY ID 'AUTOMATICO'.

  IF V_AUTOMATICO_MEMO = ABAP_TRUE.
    PERFORM INICIAR_LANCAMENTO_NOTAS.
    PERFORM BUSCA_DADOS.
    PERFORM BUSCA_DESCRICOES.
    PERFORM SALVAR_PROCESSO.
  ELSE.
    CALL SCREEN 100.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
    CLASS-METHODS:
      ON_DATA_CHANGED4 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED2 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.
    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED3 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED4 FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
CLASS LCL_DRAG_OBJECT DEFINITION.
  PUBLIC SECTION.
    DATA TEXT TYPE MTREESNODE-TEXT.
ENDCLASS.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_DRAGDROP_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      NODE_DOUBLE_CLICK FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_SIMPLE_TREE
        IMPORTING NODE_KEY.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar2 DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler2
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR2 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID2 TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

* US #163043 - MMSILVA - 18.03.2025 - Inicio
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar3 DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR3 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID3 TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
* US #163043 - MMSILVA - 18.03.2025 - Fim

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.
*
*   Add customized toolbar buttons.
    IF WG_DOCS-DOCNUM IS INITIAL.
      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
        WITH KEY GROUP1 = 'GR1'.
      IF SY-SUBRC IS INITIAL.
        IF WG_FISCAL-RETORNO EQ 'S'.
          WL_DESACTIVE = 1.
        ELSE.
          WL_DESACTIVE = SPACE.
        ENDIF.
      ELSE.
        WL_DESACTIVE = 1.
      ENDIF.
    ELSE.
      WL_DESACTIVE = 1.
    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF WG_DOCS-DOCNUM IS INITIAL.
      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
        WITH KEY GROUP1 = 'GR1'.
      IF SY-SUBRC IS INITIAL.
        WL_DESACTIVE = SPACE.
      ELSE.
        WL_DESACTIVE = 1.
      ENDIF.
    ELSE.
      WL_DESACTIVE = 1.
    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*   variable for Toolbar Button
    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
          WL_ITENS     LIKE LINE OF TG_ITENS,
          WL_LINES     TYPE SY-TABIX.
    REFRESH: TL_ITENS_AUX.

    IF P_OPERACAO IS   NOT INITIAL
       AND P_BUKRS IS  NOT INITIAL
       AND P_BRANCH IS NOT INITIAL.

*      IF WG_FISCAL-LM_ESTOQUE EQ 'S'
*        AND P_PARVW IS  NOT INITIAL
*        AND P_PARID IS NOT INITIAL.
*        EXIT.
*      ENDIF.

      CASE E_UCOMM.
        WHEN C_CLOS_MSG.
          IF GRID2 IS NOT INITIAL.
            CALL METHOD GRID2->FREE.
            FREE: CONTAINER_2, GRID2.
          ENDIF.
*    posiciona spliter na altura x
          IF SPLITTER IS NOT INITIAL.
            CALL METHOD SPLITTER->SET_ROW_HEIGHT
              EXPORTING
                ID     = 1
                HEIGHT = 100.
          ENDIF.
          LEAVE TO SCREEN 100.
        WHEN C_ADD.
          TL_ITENS_AUX[] = TG_ITENS[].
          REFRESH: TG_ITENS.
          LOOP AT TL_ITENS_AUX INTO WL_ITENS.
            WL_ITENS-ITMNUM = SY-TABIX * 10.
            WL_ITENS-FASE = ICON_DISPLAY_MORE.
            APPEND WL_ITENS TO TG_ITENS.
          ENDLOOP.
          DESCRIBE TABLE TG_ITENS LINES WL_LINES.
          CLEAR: WL_ITENS.
          WL_ITENS-FASE = ICON_DISPLAY_MORE.
          WL_ITENS-ITMNUM = ( WL_LINES + 1 ) * 10 .
          APPEND WL_ITENS TO TG_ITENS.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        WHEN C_DEL.
          CALL METHOD GRID1->GET_SELECTED_CELLS
            IMPORTING
              ET_CELL = TG_SELECTEDCELL.

          LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
            DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

          ENDLOOP.
          IF WG_FISCAL-RETORNO EQ 'N'.
            TL_ITENS_AUX[] = TG_ITENS[].
            REFRESH: TG_ITENS.
            LOOP AT TL_ITENS_AUX INTO WL_ITENS.
              WL_ITENS-ITMNUM = SY-TABIX * 10.
              APPEND WL_ITENS TO TG_ITENS.
            ENDLOOP.
          ENDIF.
          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR2 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER2
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID2.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.
*   Add customized toolbar buttons.
    IF WG_DOCS-DOCNUM IS INITIAL.
      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
        WITH KEY GROUP1 = 'GR1'.
      IF SY-SUBRC IS INITIAL.
        WL_DESACTIVE = SPACE.

      ELSE.
        WL_DESACTIVE = 1.
      ENDIF.
    ELSE.
      WL_DESACTIVE = 1.
    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    IF WG_DOCS-DOCNUM IS INITIAL.
      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
            WITH KEY GROUP1 = 'GR1'.
      IF SY-SUBRC IS INITIAL.
        WL_DESACTIVE = SPACE.
      ELSE.
        WL_DESACTIVE = 1.
      ENDIF.
    ELSE.
      WL_DESACTIVE = 1.
    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*    ty_toolbar-butn_type = 3.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   variable for Toolbar Button
*    ty_toolbar-icon      =  icon_view_close.
*    ty_toolbar-function  =  c_clos_msg.
*    ty_toolbar-disabled  = space.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER2->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    CASE E_UCOMM.
      WHEN C_ADD.
        APPEND INITIAL LINE TO TG_PARC.
      WHEN C_DEL.
        CALL METHOD GRID5->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE TG_PARC INTO TG_PARC INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

          IF TG_PARC-PARVW EQ P_PARVW
          AND TG_PARC-PARID EQ P_PARID.
            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não é possivel excluir o'
                                                   'registro!'.
          ELSE.
            DELETE TG_PARC INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

          ENDIF.

        ENDLOOP.
    ENDCASE.

    CALL METHOD GRID5->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION

* US #169751 - MMSILVA - 07.03.2025 - Inicio
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar3 IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR3 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER3
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID3.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    TY_TOOLBAR-ICON      = ICON_EXECUTE_OBJECT.
    TY_TOOLBAR-FUNCTION  = C_EXEC.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    TY_TOOLBAR-QUICKINFO = 'Atualizar ICMS'.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBARMANAGER3->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    CASE E_UCOMM.
      WHEN C_EXEC.
        SELECT SINGLE USUARIO_LIB, WERKS FROM ZMMT0131 INTO @DATA(LS_ZMMT0131) WHERE USUARIO_LIB EQ @SY-UNAME AND WERKS EQ @P_BRANCH.

        IF SY-SUBRC IS INITIAL.

          IF WG_DIREITOS-TAXCODE EQ 'V2' OR WG_DIREITOS-TAXCODE EQ 'V1'.

            IF P_CHAVE_ACESSO IS NOT INITIAL.
              DATA: RESP TYPE C.
              CLEAR RESP.

              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  TITLEBAR              = 'Confirmar'
                  TEXT_QUESTION         = 'Deseja confirmar a alteração?'
                  TEXT_BUTTON_1         = 'Sim'
                  TEXT_BUTTON_2         = 'Não'
                  DEFAULT_BUTTON        = '2'
                  DISPLAY_CANCEL_BUTTON = ''
                IMPORTING
                  ANSWER                = RESP
                EXCEPTIONS
                  TEXT_NOT_FOUND        = 1
                  OTHERS                = 2.

              IF RESP = '1'.
                PERFORM ATUALIZA_ICMS.
              ELSE.
                EXIT.
              ENDIF.

            ELSE.
              MESSAGE 'Preencher chave de acesso.' TYPE 'I' DISPLAY LIKE 'E'.
            ENDIF.

          ELSE.
            MESSAGE 'Código Imposto não permitido.' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.

        ELSE.
          MESSAGE 'Sem permissão.' TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.
    ENDCASE.

    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
* US #169751 - MMSILVA - 07.03.2025 - Fim

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.
    DATA: WL_ITENS LIKE LINE OF TG_ITENS.

    NUM_ROW = E_ROW.  "*-CS2023000043-09.02.2023-#102019-JT
    SELECT SINGLE *
         FROM ZFIWRT0001
         INTO @DATA(WL_0001_1)
          WHERE OPERACAO EQ @P_OPERACAO.

    IF E_ROW GT 0.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX E_ROW.
      IF ( WG_FISCAL-COMPLEMENTO EQ 'N' AND WL_0001_1-COMPLEMENT_ICMS = 'N' )
      OR WG_FISCAL-COMPLEMENTO IS INITIAL.
        IF WL_ITENS-MATNR IS NOT INITIAL
        AND WL_ITENS-WERKS IS NOT INITIAL
        AND WL_ITENS-MENGE IS NOT INITIAL
        AND WL_ITENS-NETPR IS NOT INITIAL.
*    posiciona spliter na altura x
          CALL METHOD SPLITTER->SET_ROW_HEIGHT
            EXPORTING
              ID     = 1
              HEIGHT = 50.

          VG_SUBSCREEN1 = C_DUMMY_HEADER.

          IF GRID2 IS NOT INITIAL.
            CALL METHOD GRID2->FREE.

          ENDIF.

          FREE: CONTAINER_2, GRID2, TG_IMPO_AUX.

          CALL METHOD SPLITTER->GET_CONTAINER
            EXPORTING
              ROW       = 2
              COLUMN    = 1
            RECEIVING
              CONTAINER = CONTAINER_2.
          IF GRID2 IS INITIAL.

            WA_LAYOUT-NO_TOOLBAR = SPACE. "US #169751 - MMSILVA - 18.03.2025
            CREATE OBJECT GRID2
              EXPORTING
                I_PARENT = CONTAINER_2.

            "US #169751 - MMSILVA - 07.03.2025 - Inicio
            CREATE OBJECT OBG_TOOLBAR3
              EXPORTING
                IO_ALV_GRID3 = GRID2.

            SET HANDLER OBG_TOOLBAR3->ON_TOOLBAR FOR GRID2.
            SET HANDLER OBG_TOOLBAR3->HANDLE_USER_COMMAND FOR GRID2.
            "US #169751 - MMSILVA - 07.03.2025 - Fim

            WA_LAYOUT-CWIDTH_OPT = C_X.
            WA_LAYOUT-EDIT = SPACE.
            WA_LAYOUT-INFO_FNAME = 'COLOR'.
*          wa_layout-grid_title = 'Impostos'.
            CONDENSE E_ROW NO-GAPS.
            CONCATENATE 'Impostos do Item' '-' WL_ITENS-ITMNUM INTO WA_LAYOUT-GRID_TITLE SEPARATED BY SPACE.
            PERFORM MONTAR_LAYOUT_IMPOSTOS.
            PERFORM MONTA_IMPOSTOS TABLES TG_IMPO_AUX
                                   USING E_ROW.
            PERFORM F_MONTA_COR_IMPOSTO.
            CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
              EXPORTING
                IS_LAYOUT       = WA_LAYOUT
              CHANGING
                IT_FIELDCATALOG = T_FIELDCATALOG[]
                IT_OUTTAB       = TG_IMPO_AUX[].

*-CS2023000043-09.02.2023-#102019-JT-inicio
            CALL METHOD GRID2->REGISTER_EDIT_EVENT
              EXPORTING
                I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

            CALL METHOD GRID2->REGISTER_EDIT_EVENT
              EXPORTING
                I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

            SET HANDLER: LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED4 FOR GRID2,
                         LCL_EVENT_HANDLER=>ON_DATA_CHANGED4 FOR GRID2.
*-CS2023000043-09.02.2023-#102019-JT-fim

*      *** Método de atualização de dados na Tela
            CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = WA_STABLE.
          ELSE.
*      *** Método de atualização de dados na Tela
            CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = WA_STABLE.

          ENDIF.
          WG_DG1 = C_MAXIMIZAR.
          LEAVE TO SCREEN 100.
        ELSE.
*    posiciona spliter na altura x
          CALL METHOD SPLITTER->SET_ROW_HEIGHT
            EXPORTING
              ID     = 1
              HEIGHT = 100.
        ENDIF.
      ELSEIF ( WG_FISCAL-COMPLEMENTO EQ 'S' AND WL_ITENS-MATNR IS NOT INITIAL ) OR WL_0001_1-COMPLEMENT_ICMS = 'S'.

        NUM_ROW = E_ROW.
*    posiciona spliter na altura x
        CALL METHOD SPLITTER->SET_ROW_HEIGHT
          EXPORTING
            ID     = 1
            HEIGHT = 50.

        VG_SUBSCREEN1 = C_DUMMY_HEADER.

        IF GRID2 IS NOT INITIAL.
          CALL METHOD GRID2->FREE.

        ENDIF.

        FREE: CONTAINER_2, GRID2, TG_IMPO_AUX.

        CALL METHOD SPLITTER->GET_CONTAINER
          EXPORTING
            ROW       = 2
            COLUMN    = 1
          RECEIVING
            CONTAINER = CONTAINER_2.
        IF GRID2 IS INITIAL.
          WA_LAYOUT-NO_TOOLBAR = SPACE.
          CREATE OBJECT GRID2
            EXPORTING
              I_PARENT = CONTAINER_2.

          WA_LAYOUT-CWIDTH_OPT = C_X.
          WA_LAYOUT-NO_TOOLBAR = C_X.
          WA_LAYOUT-INFO_FNAME = 'COLOR'.
*          wa_layout-grid_title = 'Impostos'.
          CONDENSE E_ROW NO-GAPS.
          CONCATENATE 'Impostos do Item' '-' WL_ITENS-ITMNUM INTO WA_LAYOUT-GRID_TITLE SEPARATED BY SPACE.
          PERFORM MONTAR_LAYOUT_IMPOSTOS.
          PERFORM MONTA_IMPOSTOS TABLES TG_IMPO_AUX
                                 USING E_ROW.
          PERFORM F_MONTA_COR_IMPOSTO.
          CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING
              IS_LAYOUT       = WA_LAYOUT
            CHANGING
              IT_FIELDCATALOG = T_FIELDCATALOG[]
              IT_OUTTAB       = TG_IMPO_AUX[].

          CALL METHOD GRID2->REGISTER_EDIT_EVENT
            EXPORTING
              I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

          CALL METHOD GRID2->REGISTER_EDIT_EVENT
            EXPORTING
              I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

          SET HANDLER:
*              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1,
                    LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED4 FOR GRID2,
                    LCL_EVENT_HANDLER=>ON_DATA_CHANGED4 FOR GRID2.

*      *** Método de atualização de dados na Tela
          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ELSE.
*      *** Método de atualização de dados na Tela
          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.

        ENDIF.
        WG_DG1 = C_MAXIMIZAR.
        LEAVE TO SCREEN 100.

      ELSE.
*    POSICIONA SPLITER NA ALTURA X
        CALL METHOD SPLITTER->SET_ROW_HEIGHT
          EXPORTING
            ID     = 1
            HEIGHT = 100.
      ENDIF.
    ENDIF.

** Método de atualização de dados na Tela
*    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD ON_HOTSPOT_CLICK.
    DATA I_EBELP TYPE EKPO-EBELP.

    READ TABLE TG_ITENS INTO WL_ITENS_NF  INDEX E_ROW_ID-INDEX.
    I_EBELP = WL_ITENS_NF-ITMNUM.
    IF WL_ITENS_NF-CHARG IS NOT INITIAL AND
       WL_ITENS_NF-WERKS IS NOT INITIAL AND
       WL_ITENS_NF-LGORT IS NOT INITIAL AND
       WL_ITENS_NF-MENGE GT 0.
      CALL FUNCTION 'Z_MM_INDEA_LOTE'
        EXPORTING
          I_EBELN = P_SEQ_LCTO
          I_EBELP = I_EBELP
          I_MATNR = WL_ITENS_NF-MATNR
          I_CHARG = WL_ITENS_NF-CHARG
          I_WERKS = WL_ITENS_NF-WERKS
          I_LGORT = WL_ITENS_NF-LGORT
          I_MENGE = WL_ITENS_NF-MENGE
          I_RENAS = WL_ITENS_NF-RENAS
          I_BTN   = 'X'
          I_TCODE = SY-TCODE.
    ENDIF.
    "
  ENDMETHOD.

  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD          TYPE LVC_S_MODI,
          LV_VALUE         TYPE LVC_VALUE,
          LV_ANLN1         TYPE ANLA-ANLN1,
          VL_TABIX         TYPE SY-TABIX,
          VL_VALUE         TYPE LVC_VALUE,
          WL_MARA          TYPE MARA,
          WL_VBAP          TYPE VBAP,
          WL_MAKT          TYPE MAKT,
          WL_T001W         TYPE T001W,
          WL_MCH1          TYPE MCH1,
          WL_ITENS         LIKE LINE OF TG_ITENS,
          WL_WHERE(30),
          WL_MARC          TYPE MARC,
          WL_MBEW          TYPE MBEW,
          WL_1BBRANCH      TYPE J_1BBRANCH,
          WL_1BAA          TYPE J_1BAA,
          WL_1BAPN         TYPE  J_1BAPN,
          WL_DIRECT        TYPE J_1BAPNV-DIRECT,
          WL_DSTCAT        TYPE J_1BAPNV-DSTCAT,
          TL_ANEP          TYPE TABLE OF ANEP,
          TL_ANLA          TYPE TABLE OF ANLA,
          TL_ZFIWRT0009    TYPE TABLE OF ZFIWRT0009,
          WL_ZFIWRT0009    TYPE ZFIWRT0009,
          WL_ZFIWRT0008    TYPE ZFIWRT0008,
          WL_J_1BNFDOC     TYPE J_1BNFDOC,
          WL_J_1BNFDOC_AUX TYPE J_1BNFDOC,
          WL_TOT_ANBTR     TYPE ANEP-ANBTR,
          WL_MENGE         TYPE MSEG-MENGE,
          WL_DMBTR         TYPE MSEG-DMBTR,
          WL_ANLN1         TYPE ANLA-ANLN1,
          WL_ANLN2         TYPE ANLA-ANLN2,
          WL_ANLA          TYPE ANLA,
          WL_MSG_RET       TYPE ZFIWRS0002,
          TL_ANLC          TYPE TABLE OF ANLC.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'MATNR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM MARA
        INTO WL_MARA
          WHERE MATNR EQ LV_VALUE.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE *
          FROM MAKT
          INTO WL_MAKT
           WHERE SPRAS EQ SY-LANGU
             AND MATNR EQ WL_MARA-MATNR.

        IF SY-SUBRC IS INITIAL.
          MOVE: WL_MAKT-MAKTX TO LV_VALUE.

          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'MAKTX'
              I_VALUE     = LV_VALUE.

        ENDIF.
        MOVE: WL_MARA-MEINS TO LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'MEINS'
            I_VALUE     = LV_VALUE.
      ELSE.
        CLEAR: LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'MATNR'
            I_VALUE     = LV_VALUE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Material selecionado não foi encontrado!'.
      ENDIF.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'WERKS'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM T001W
        INTO WL_T001W
         WHERE WERKS EQ LV_VALUE.

      IF SY-SUBRC IS INITIAL.
*      select *
      ELSE.
        CLEAR: LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'WERKS'
            I_VALUE     = LV_VALUE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Centro/Filial não encontrado.'.
      ENDIF.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'CHARG'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF WL_ITENS IS NOT INITIAL.
        WL_WHERE = 'MATNR EQ WL_ITENS-MATNR'.
      ENDIF.
      IF LV_VALUE IS NOT INITIAL.
        .

        IF SY-SUBRC IS INITIAL.
*      select *
        ELSE.
          CLEAR: LV_VALUE.

          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'CHARG'
              I_VALUE     = LV_VALUE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não existe lote para este material.'.
        ENDIF.
      ENDIF.
    ENDLOOP.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'VBELN' .

      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      IF WL_ITENS-POSNR IS INITIAL.
        WL_ITENS-POSNR = 10.
        MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-TABIX TRANSPORTING POSNR.
      ENDIF.
      SELECT SINGLE *
        FROM VBAP
        INTO WL_VBAP
        WHERE VBELN = LV_VALUE
        AND   POSNR = WL_ITENS-POSNR.
      IF SY-SUBRC NE 0.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ordem selecionada não foi encontrada!'.
      ENDIF.
      "
      MOVE: WL_VBAP-MATNR TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'MATNR'
          I_VALUE     = LV_VALUE.

      MOVE: WL_VBAP-WERKS TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'WERKS'
          I_VALUE     = LV_VALUE.

      MOVE: WL_VBAP-LGORT TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'LGORT'
          I_VALUE     = LV_VALUE.

      MOVE: WL_VBAP-CHARG TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'CHARG'
          I_VALUE     = LV_VALUE.

      IF VINICIOU_LCTO_ZNFW0009 IS INITIAL.
        MOVE: WL_VBAP-NETPR TO LV_VALUE.
        CONDENSE LV_VALUE NO-GAPS.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NETPR'
            I_VALUE     = LV_VALUE.
      ENDIF.

      SELECT SINGLE *
        FROM MARA
        INTO WL_MARA
          WHERE MATNR EQ WL_VBAP-MATNR.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE *
          FROM MAKT
          INTO WL_MAKT
           WHERE SPRAS EQ SY-LANGU
             AND MATNR EQ WL_MARA-MATNR.

        IF SY-SUBRC IS INITIAL.
          MOVE: WL_MAKT-MAKTX TO LV_VALUE.

          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'MAKTX'
              I_VALUE     = LV_VALUE.

        ENDIF.
        MOVE: WL_MARA-MEINS TO LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'MEINS'
            I_VALUE     = LV_VALUE.
      ELSE.
        CLEAR: LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'MATNR'
            I_VALUE     = LV_VALUE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Material selecionado não foi encontrado!'.
      ENDIF.

    ENDLOOP.

*** PBI - 73759 - Inicio - CBRAND
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'ANLN1' OR FIELDNAME = 'ANLN2'  .

      CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_TABIX     = LS_GOOD-TABIX
          I_FIELDNAME = 'ANLN1'
        IMPORTING
          E_VALUE     = WL_ANLN1.

      CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_TABIX     = LS_GOOD-TABIX
          I_FIELDNAME = 'ANLN2'
        IMPORTING
          E_VALUE     = WL_ANLN2.

      CLEAR: TL_ANEP, TL_ZFIWRT0009, WL_TOT_ANBTR.
      SELECT *
        FROM ANEP
        INTO TABLE TL_ANEP
         WHERE BUKRS = P_BUKRS
          AND  ANLN1 = WL_ANLN1
          AND  ANLN2 = WL_ANLN2
          AND  AFABE    = 01.

      IF TL_ANEP IS NOT INITIAL.
        LOOP AT TL_ANEP INTO DATA(WL_ANEP).
          ADD WL_ANEP-ANBTR TO  WL_TOT_ANBTR.
        ENDLOOP.

        IF WG_FISCAL-IMOBILIZADO = 'S' AND WG_FISCAL-TP_MV_IMOB <> 'V' AND WL_TOT_ANBTR > 0.

          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_GOOD-ROW_ID
              I_FIELDNAME = 'NETPR'
              I_VALUE     = WL_TOT_ANBTR.

*          SELECT *
*            FROM zfiwrt0009
*          INTO TABLE tl_zfiwrt0009
*            WHERE anln1  = wl_anln1
*               AND anln2 = wl_anln2.
*
*          IF tl_zfiwrt0009 IS NOT INITIAL.
*
*            LOOP AT tl_zfiwrt0009 INTO wl_zfiwrt0009.
*
*              SELECT SINGLE *
*                FROM zfiwrt0008
*                INTO wl_zfiwrt0008
*                  WHERE seq_lcto = wl_zfiwrt0009-seq_lcto.
*
*              SELECT SINGLE *
*                FROM j_1bnfdoc
*                INTO wl_j_1bnfdoc
*                  WHERE docnum = wl_zfiwrt0008-docnum.
*
*              IF wl_j_1bnfdoc-direct = 2 AND  wl_j_1bnfdoc-candat IS INITIAL.
*
*                SELECT SINGLE *
*                  FROM j_1bnfdoc
*                  INTO wl_j_1bnfdoc_aux
*                  WHERE  direct  = '1'
*                    AND  partyp  = 'B'
*                    AND  bukrs   = wl_j_1bnfdoc-bukrs
*                    AND  branch  = wl_j_1bnfdoc-parid+4(4)   "Empresa+Centro
*                    AND  docdat  = wl_j_1bnfdoc-docdat
*                    AND  nfenum  = wl_j_1bnfdoc-nfenum.
*
*                IF wl_j_1bnfdoc_aux IS INITIAL.
*                  MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Existe saida para esse Imobilizado sem Entrada na Filial Destino, Nota;' wl_j_1bnfdoc-nfenum.
*                ENDIF.
*              ENDIF.
*              CLEAR: wl_zfiwrt0009, wl_zfiwrt0008.
*            ENDLOOP.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*** PBI - 73759 - Fim - CBRAND

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'NETDIS'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF SY-SUBRC IS INITIAL.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NETDIS'
            I_VALUE     = LV_VALUE.

        SELECT SINGLE *
          FROM ZFIWRT0009
          INTO @DATA(LS_ZFIWRT0009)
          WHERE SEQ_LCTO = @P_SEQ_LCTO
            AND ITMNUM = @WL_ITENS-ITMNUM.
        IF SY-SUBRC IS INITIAL.
          LS_ZFIWRT0009-NETDIS = LV_VALUE.
          MODIFY ZFIWRT0009 FROM LS_ZFIWRT0009.
          COMMIT WORK.
        ENDIF.

      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'NETFRE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF SY-SUBRC IS INITIAL.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NETFRE'
            I_VALUE     = LV_VALUE.

        SELECT SINGLE *
          FROM ZFIWRT0009
          INTO @LS_ZFIWRT0009
          WHERE SEQ_LCTO = @P_SEQ_LCTO
            AND ITMNUM = @WL_ITENS-ITMNUM.
        IF SY-SUBRC IS INITIAL.
          LS_ZFIWRT0009-NETFRE = LV_VALUE.
          MODIFY ZFIWRT0009 FROM LS_ZFIWRT0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'NETINS'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF SY-SUBRC IS INITIAL.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NETINS'
            I_VALUE     = LV_VALUE.

        SELECT SINGLE *
          FROM ZFIWRT0009
          INTO @LS_ZFIWRT0009
          WHERE SEQ_LCTO = @P_SEQ_LCTO
            AND ITMNUM = @WL_ITENS-ITMNUM.
        IF SY-SUBRC IS INITIAL.
          LS_ZFIWRT0009-NETINS = LV_VALUE.
          MODIFY ZFIWRT0009 FROM LS_ZFIWRT0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'NETOTH'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF SY-SUBRC IS INITIAL.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NETOTH'
            I_VALUE     = LV_VALUE.

        SELECT SINGLE *
          FROM ZFIWRT0009
          INTO @LS_ZFIWRT0009
          WHERE SEQ_LCTO = @P_SEQ_LCTO
            AND ITMNUM = @WL_ITENS-ITMNUM.
        IF SY-SUBRC IS INITIAL.
          LS_ZFIWRT0009-NETOTH = LV_VALUE.
          MODIFY ZFIWRT0009 FROM LS_ZFIWRT0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

*Inicio Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'ANLN1'.
      LV_ANLN1 = LV_VALUE = LS_GOOD-VALUE.

      CONDENSE LV_VALUE NO-GAPS.

*** BUG #184879 - MMSILVA - 10.07.2025 - Ini ***
      "Comentado devido estar atualizando valores da primeira linha quando é alterado a segunda linha - BUG #184879 - MMSILVA - 10.07.2025
*      READ TABLE TG_ITENS ASSIGNING FIELD-SYMBOL(<WL_ITENS>) INDEX LS_GOOD-TABIX.
      "Comentado devido estar atualizando valores da primeira linha quando é alterado a segunda linha - BUG #184879 - MMSILVA - 10.07.2025

      READ TABLE TG_ITENS ASSIGNING FIELD-SYMBOL(<WL_ITENS>) INDEX LS_GOOD-ROW_ID.
*** BUG #184879 - MMSILVA - 10.07.2025 - Fim ***

      IF SY-SUBRC IS INITIAL.
        "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - inicio1
        "ELE PRIMEIRO BUSCA O ANLN2 PARA DEPOIS IR BUSCAR O VALOR DO IMOBILIZADO
        CLEAR TL_ANLA[].
        SELECT *
               INTO TABLE TL_ANLA
               FROM ANLA
               WHERE BUKRS EQ P_BUKRS
                 AND ANLN1 EQ LV_ANLN1.

        SORT TL_ANLA BY ANLN1 ANLN2 DESCENDING.
        READ TABLE TL_ANLA INTO WL_ANLA INDEX 1.
        IF SY-SUBRC EQ 0.
          CONDENSE LV_ANLN1 NO-GAPS.
          IF WL_ANLA-ANLN2 IS NOT INITIAL.
            LV_VALUE = WL_ANLA-ANLN2.
            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
              EXPORTING
                I_ROW_ID    = LS_GOOD-ROW_ID
                I_FIELDNAME = 'ANLN2'
                I_VALUE     = LV_VALUE.
          ENDIF.
        ENDIF.

        IF WG_FISCAL-IMOBILIZADO EQ 'S'. " AND WG_FISCAL-TP_MV_IMOB NE 'V'. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
          SELECT *
                 INTO TABLE TL_ANLC
                 FROM ANLC
                 WHERE BUKRS EQ P_BUKRS
                   AND ANLN1 EQ LV_ANLN1
                   AND ANLN2 EQ LV_VALUE
                   AND AFABE EQ '1'.

          IF SY-SUBRC EQ 0.
            SORT TL_ANLC BY GJAHR DESCENDING. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
            DELETE TL_ANLC WHERE KANSW IS INITIAL.
            READ TABLE TL_ANLC INTO DATA(WL_ANLC) INDEX 1.
            IF WL_ANLC-KANSW > 0.
              LV_VALUE = WL_ANLC-KANSW.
              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                EXPORTING
                  I_ROW_ID    = LS_GOOD-ROW_ID
                  I_FIELDNAME = 'NETPR'
                  I_VALUE     = LV_VALUE.

              <WL_ITENS>-NETWR = ( ( <WL_ITENS>-MENGE * LV_VALUE ) + <WL_ITENS>-NETFRE + <WL_ITENS>-NETINS + <WL_ITENS>-NETOTH ) - <WL_ITENS>-NETDIS.

              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                EXPORTING
                  I_ROW_ID    = LS_GOOD-ROW_ID
                  I_FIELDNAME = 'NETWR'
                  I_VALUE     = <WL_ITENS>-NETWR.
            ENDIF.
          ENDIF.
        ENDIF.
        ""Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - fIM1
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                         INTO LS_GOOD
                         WHERE FIELDNAME = 'NETPR'.
      LV_ANLN1 = LV_VALUE = LS_GOOD-VALUE.

      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-TABIX.
      IF SY-SUBRC IS INITIAL.

        IF WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB NE 'V'.
          SELECT *
                 INTO TABLE TL_ANLC
                 FROM ANLC
                 WHERE BUKRS EQ P_BUKRS
                   AND ANLN1 EQ WL_ITENS-ANLN1
                   AND AFABE EQ '1'.

          IF SY-SUBRC EQ 0.
            SORT TL_ANLC BY GJAHR DESCENDING. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
            DELETE TL_ANLC WHERE KANSW IS INITIAL."Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
            READ TABLE TL_ANLC INTO DATA(WL_ANLC1) INDEX 1.
* INICIO - IR250767- RRIBEIRO - 01.08.2025 - STEFANINI
*            IF WL_ITENS-NETPR IS NOT INITIAL.
            IF ls_good-value IS NOT INITIAL.
* FIM - IR250767- RRIBEIRO - 01.08.2025 - STEFANINI
              IF WL_ANLC1-KANSW > 0 AND WL_ANLC1-KANSW < LV_VALUE.
                LV_VALUE = WL_ANLC1-KANSW.
* INICIO - IR250767- RRIBEIRO - 01.08.2025 - STEFANINI
              ELSE.
                LV_VALUE = LS_GOOD-VALUE.
* FIM - IR250767- RRIBEIRO - 01.08.2025 - STEFANINI
              ENDIF.
              CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                EXPORTING
                  I_ROW_ID    = LS_GOOD-ROW_ID
                  I_FIELDNAME = 'NETPR'
                  I_VALUE     = LV_VALUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*Fim Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631


    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
*    LOOP AT er_data_changed->mt_good_cells
*                             INTO ls_good.
*
*      READ TABLE tg_itens INTO wl_itens INDEX ls_good-ROW_ID.
****> Determina o CFOP
*      IF wg_direitos-cfop IS INITIAL.
*        SELECT SINGLE *
*          FROM marc
*          INTO wl_marc
*           WHERE matnr EQ wl_itens-matnr.
*
*        IF sy-subrc IS INITIAL.
*          SELECT SINGLE *
*            FROM mbew
*            INTO wl_mbew
*             WHERE matnr EQ wl_itens-matnr
*               AND bwkey EQ wl_itens-werks.
*
*          IF sy-subrc IS INITIAL.
*            SELECT SINGLE *
*              FROM j_1bbranch
*               INTO wl_1bbranch
*               WHERE bukrs  EQ p_bukrs
*                 AND branch EQ p_branch.
*
*            IF sy-subrc IS INITIAL.
*              SELECT SINGLE *
*                FROM j_1baa
*                INTO wl_1baa
*                 WHERE nftype EQ wg_fiscal-nftype.
*
*              IF wl_1baa-entrad EQ c_x.
*                wl_direct = c_1.
*              ELSE.
*                wl_direct = c_2.
*              ENDIF.
*
*              IF wg_direitos-indcoper EQ c_d.
*                wl_dstcat = c_0.
*
*              ELSE.
*                wl_dstcat = c_1.
*
*              ENDIF.
*
*              SELECT SINGLE *
*                FROM j_1bapn
*                INTO wl_1bapn
*                 WHERE direct EQ wl_direct
*                   AND dstcat EQ wl_dstcat
*                   AND indus3 EQ wl_marc-indus
*                   AND itmtyp EQ wg_fiscal-itmtyp
*                   AND ownpro EQ wl_mbew-ownpr
*                   AND matuse EQ wl_mbew-mtuse
*                   AND indus1 EQ wl_1bbranch-industry.
*
*              IF sy-subrc IS INITIAL.
*                lv_value = wl_1bapn-cfop.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        lv_value = wg_direitos-cfop.
*
*      ENDIF.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'CFOP'
*          i_value     = lv_value.
*
*      WL_ITENS-NETWR = wl_itens-menge * wl_itens-netpr.
*      lv_value = WL_ITENS-NETWR.
*      CONDENSE LV_VALUE.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'NETWR'
*          i_value     = lv_value.
*    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED4.
    DATA: LS_GOOD   TYPE LVC_S_MODI,
          LV_VALUE  TYPE LVC_VALUE,
          WL_RATE   TYPE ZFIWRT0010-RATE,
          WL_BASE   TYPE ZFIWRT0010-BASE,
          WL_TAXVAL TYPE ZFIWRT0010-TAXVAL.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                 INTO LS_GOOD
                                 WHERE FIELDNAME = 'BASE'
                                   OR  FIELDNAME = 'RATE'
                                   OR  FIELDNAME = 'EXCBAS'
                                   OR  FIELDNAME = 'OTHBAS'.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      IF LS_GOOD-FIELDNAME EQ 'BASE'.
        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'RATE'
          IMPORTING
            E_VALUE     = WL_RATE.

***  Realiza o calculo do campo "Valor da Venda"
        TRY.
            WL_TAXVAL = LV_VALUE * ( WL_RATE / 100 ).
          CATCH CX_SY_ZERODIVIDE.
        ENDTRY.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'TAXVAL'
            I_VALUE     = WL_TAXVAL.
      ELSEIF  LS_GOOD-FIELDNAME EQ 'RATE'.
        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_TABIX     = LS_GOOD-TABIX
            I_FIELDNAME = 'BASE'
          IMPORTING
            E_VALUE     = WL_BASE.

***  Realiza o calculo do campo "TAXVAL"
        TRY.
            WL_TAXVAL = WL_BASE * ( LV_VALUE / 100 ).
          CATCH CX_SY_ZERODIVIDE.
        ENDTRY.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'TAXVAL'
            I_VALUE     = WL_TAXVAL.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED4
  METHOD ON_DATA_CHANGED_FINISHED.
    DATA: WL_ITENS     LIKE LINE OF TG_ITENS,
          LS_GOOD      TYPE LVC_S_MODI,
          LV_VALUE     TYPE LVC_VALUE,
          WL_MARC      TYPE MARC,
          WL_MBEW      TYPE MBEW,
          WL_1BBRANCH  TYPE J_1BBRANCH,
          WL_1BAA      TYPE J_1BAA,
          WL_1BAPN     TYPE  J_1BAPN,
          WL_DIRECT    TYPE J_1BAPNV-DIRECT,
          WL_DSTCAT    TYPE J_1BAPNV-DSTCAT,
          TL_ANEP      TYPE TABLE OF ANEP,
          WL_TOT_ANBTR TYPE ANEP-ANBTR.
*    BREAK ABAP.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
       WHERE TABIX GT 0.

      READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.

      IF SY-SUBRC IS INITIAL.

        SELECT SINGLE *
          FROM ZFIWRT0001
          INTO @DATA(WL_0001_1)
           WHERE OPERACAO EQ @P_OPERACAO.

        IF WG_DIREITOS-CFOP IS INITIAL AND WL_0001_1-LM_INDEA NE 'S' AND
           WL_0001_1-COMPLEMENT_ICMS NE 'S'. "*-CS2023000043-09.02.2023-#102019-JT
          SELECT SINGLE *
            FROM MARC
            INTO WL_MARC
             WHERE MATNR EQ WL_ITENS-MATNR
               AND WERKS EQ P_BRANCH.

          IF SY-SUBRC IS INITIAL.
            WL_ITENS-STEUC = WL_MARC-STEUC.
            SELECT SINGLE *
              FROM MBEW
              INTO WL_MBEW
               WHERE MATNR EQ WL_ITENS-MATNR
                 AND BWKEY EQ WL_ITENS-WERKS.

            IF SY-SUBRC IS INITIAL.
              SELECT SINGLE *
                FROM J_1BBRANCH
                 INTO WL_1BBRANCH
                 WHERE BUKRS  EQ P_BUKRS
                   AND BRANCH EQ P_BRANCH.

              IF SY-SUBRC IS INITIAL.
                SELECT SINGLE *
                  FROM J_1BAA
                  INTO WL_1BAA
                   WHERE NFTYPE EQ WG_FISCAL-NFTYPE.

                IF WL_1BAA-ENTRAD EQ C_X.
                  WL_DIRECT = C_1.
                ELSE.
                  WL_DIRECT = C_2.
                ENDIF.

                IF WG_DIREITOS-INDCOPER EQ C_D.
                  WL_DSTCAT = C_0.

                ELSE.
                  WL_DSTCAT = C_1.

                ENDIF.

                SELECT SINGLE *
                  FROM J_1BAPN
                  INTO WL_1BAPN
                   WHERE DIRECT EQ WL_DIRECT
                     AND DSTCAT EQ WL_DSTCAT
                     AND INDUS3 EQ WL_MARC-INDUS
                     AND ITMTYP EQ WG_FISCAL-ITMTYP
                     AND OWNPRO EQ WL_MBEW-OWNPR
                     AND MATUSE EQ WL_MBEW-MTUSE
                     AND INDUS1 EQ WL_1BBRANCH-INDUSTRY.

                IF SY-SUBRC IS INITIAL.
                  WL_ITENS-CFOP = WL_1BAPN-CFOP.
                ELSE.
                  CLEAR: WL_ITENS-CFOP.
                ENDIF.
              ELSE.
                CLEAR: WL_ITENS-CFOP.
              ENDIF.
            ELSE.
              CLEAR: WL_ITENS-CFOP.
            ENDIF.
          ELSE.
            CLEAR: WL_ITENS-CFOP, WL_ITENS-STEUC.
          ENDIF.
        ELSEIF WL_0001_1-LM_INDEA NE 'S'.
          SELECT SINGLE *
            FROM MARC
            INTO WL_MARC
             WHERE MATNR EQ WL_ITENS-MATNR
               AND WERKS EQ P_BRANCH.

          WL_ITENS-STEUC = WL_MARC-STEUC.

*-CS2023000043-09.02.2023-#102019-JT-inicio
          IF WL_0001_1-COMPLEMENT_ICMS NE 'S'.
            WL_ITENS-CFOP = WG_DIREITOS-CFOP.
          ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim
        ENDIF.

        IF ( VINICIOU_LCTO_ZNFW0009 IS NOT INITIAL ) AND
           ( WG_FISCAL-EBELN IS NOT INITIAL OR WG_FISCAL-VBELN IS NOT INITIAL ) AND
           ( VLANCAMENTO_ZNFW0009 = 'MIC' OR VLANCAMENTO_ZNFW0009 = 'ONF'  ).

          IF WL_ITENS-MENGE IS NOT INITIAL AND WL_ITENS-NETPR IS NOT INITIAL.

*            wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
            WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_ITENS-NETPR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS.

            IF WL_ITENS-NETWR <> WL_ITENS-VLR_ITEN_XML.

              DATA(LVA_LIBERA_DIFERENCA) = ABAP_FALSE.

              IF WG_FISCAL-ACCESS_KEY IS NOT INITIAL.
                DATA: LVA_DIFERENCA      TYPE ZFIWRT0009-NETWR,
                      LVA_TOLERANCIA_MIN TYPE ZFIT0141-TOLERANCIA,
                      LVA_TOLERANCIA_MAX TYPE ZFIT0141-TOLERANCIA.

                LVA_DIFERENCA = WL_ITENS-NETWR - WL_ITENS-VLR_ITEN_XML.

                IF ABS( LVA_DIFERENCA ) > 1.

                  SELECT SINGLE TOLERANCIA
                    FROM ZFIT0141
                    INTO @DATA(TOLERANCIA)
                    WHERE CHAVE EQ @WG_FISCAL-ACCESS_KEY.

                  IF SY-SUBRC IS INITIAL.
                    LVA_TOLERANCIA_MIN = TOLERANCIA * -1.
                    LVA_TOLERANCIA_MAX = TOLERANCIA.

                    IF LVA_DIFERENCA BETWEEN LVA_TOLERANCIA_MIN AND LVA_TOLERANCIA_MAX.
                      LVA_LIBERA_DIFERENCA = ABAP_TRUE.
                    ENDIF.
                  ENDIF.

                  IF LVA_LIBERA_DIFERENCA EQ ABAP_FALSE.
                    MESSAGE 'Valor total do item não corresponde ao valor total da nota' TYPE 'I'.
                    CLEAR: WL_ITENS-MENGE, WL_ITENS-NETPR.
                    WL_ITENS-NETWR = WL_ITENS-VLR_ITEN_XML.
                    MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID.
                    EXIT.
                  ENDIF.

                ENDIF.

              ENDIF.

            ELSE.
              MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID.
            ENDIF.
          ENDIF.
        ELSE.
*          wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
          WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_ITENS-NETPR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS.
          MODIFY TG_ITENS FROM WL_ITENS INDEX LS_GOOD-ROW_ID.
        ENDIF.


*** pbi - 73759 - inicio - cbrand
        CLEAR: TL_ANEP, WL_TOT_ANBTR.
        SELECT *
          FROM ANEP
          INTO TABLE TL_ANEP
           WHERE BUKRS = P_BUKRS
            AND  ANLN1 = WL_ITENS-ANLN1
            AND  ANLN2 = WL_ITENS-ANLN2
            AND  AFABE    = 01.

        IF TL_ANEP IS NOT INITIAL.

          LOOP AT TL_ANEP INTO DATA(WL_ANEP).
            ADD WL_ANEP-ANBTR TO  WL_TOT_ANBTR.
          ENDLOOP.

          IF WG_FISCAL-IMOBILIZADO = 'S' AND WG_FISCAL-TP_MV_IMOB <> 'V' AND WL_TOT_ANBTR > 0.

            REFRESH: STYLE2, TG_ITENS-STYLE2.

            WA_STYLE-FIELDNAME = 'NETPR'.
            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

            INSERT WA_STYLE INTO TABLE STYLE2.
            INSERT LINES OF STYLE2 INTO TABLE TG_ITENS-STYLE2.
            MOVE-CORRESPONDING TG_ITENS TO WL_ITENS.
            MODIFY TG_ITENS[] FROM WL_ITENS INDEX LS_GOOD-ROW_ID TRANSPORTING STYLE2.
          ENDIF.
        ENDIF.
*** PBI - 73759 - inicio - CBRAND

      ENDIF.
    ENDLOOP.

    PERFORM MONTA_CONTABIL.
    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
*** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.


    PERFORM VERIFICA_ERROS.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD ON_DATA_CHANGED_FINISHED2.
    DATA: LS_GOOD TYPE LVC_S_MODI,
          WL_PARC LIKE LINE OF TG_PARC.
*    break abap.
    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
    WHERE TABIX GT 0.

      READ TABLE TG_PARC INTO WL_PARC INDEX LS_GOOD-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        IF WL_PARC-PARVW EQ C_AG
        OR WL_PARC-PARVW EQ C_LR.
          SELECT SINGLE NAME1
            FROM KNA1
             INTO WL_PARC-NOME
             WHERE KUNNR EQ WL_PARC-PARID.

        ELSEIF WL_PARC-PARVW EQ C_BR
           OR WL_PARC-PARVW  EQ C_Z1
           OR  WL_PARC-PARVW EQ C_LF
           OR  WL_PARC-PARVW EQ C_PC
           OR  WL_PARC-PARVW EQ C_SP.
          SELECT SINGLE NAME1
            FROM LFA1
             INTO WL_PARC-NOME
              WHERE LIFNR EQ WL_PARC-PARID.
        ENDIF.
        MODIFY TG_PARC FROM WL_PARC INDEX LS_GOOD-ROW_ID.
        CLEAR: WL_PARC.
      ENDIF.
    ENDLOOP.
    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
*** Método de atualização de dados na Tela
    CALL METHOD GRID5->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "on_data_changed2

  METHOD ON_DATA_CHANGED_FINISHED3.
    DATA: LS_GOOD   TYPE LVC_S_MODI,
          LV_VALUE  TYPE LVC_VALUE,
          WL_CONTAB LIKE LINE OF TG_CONTAB,
          V_CLI.

    DATA: WA_ZSDT0075 TYPE ZSDT0075.
    CLEAR: WL_CONTAB.

*    break abap.
    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
    WHERE TABIX GT 0.

      IF LS_GOOD-FIELDNAME EQ 'ZLSCH'.
        IF LS_GOOD-VALUE EQ 'D'.
          READ TABLE TG_CONTAB ASSIGNING FIELD-SYMBOL(<FS_CONTAB>) INDEX LS_GOOD-ROW_ID.
          IF SY-SUBRC IS INITIAL.
            SELECT SINGLE *
                   INTO @DATA(WL_ZFIT0196)
                   FROM ZFIT0196.
            IF <FS_CONTAB>-TAXA_JUROS IS INITIAL.
              <FS_CONTAB>-TAXA_JUROS = WL_ZFIT0196-JUROS.
            ENDIF.
            IF <FS_CONTAB>-TAXA_MULTA IS INITIAL.
              <FS_CONTAB>-TAXA_MULTA = WL_ZFIT0196-MULTA.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE TG_CONTAB ASSIGNING <FS_CONTAB> INDEX LS_GOOD-ROW_ID.
          IF SY-SUBRC IS INITIAL.
            CLEAR: <FS_CONTAB>-TAXA_JUROS, <FS_CONTAB>-TAXA_MULTA.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE TG_CONTAB INTO TG_CONTAB INDEX LS_GOOD-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        READ TABLE TG_CONTAB INTO WL_CONTAB
          WITH KEY HKONT = TG_CONTAB-HKONT
                   ESTORNO = 'X'.
        IF SY-SUBRC IS INITIAL.
          MOVE: TG_CONTAB-ZLSCH TO WL_CONTAB-ZLSCH,
                TG_CONTAB-ZFBDT TO WL_CONTAB-ZFBDT,
                TG_CONTAB-KOSTL TO WL_CONTAB-KOSTL.

          MODIFY TG_CONTAB FROM WL_CONTAB INDEX SY-TABIX."ls_good-row_id.
          CLEAR: WL_CONTAB, TG_CONTAB.
        ENDIF.

      ENDIF.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      READ TABLE TG_CONTAB INTO WL_CONTAB INDEX LS_GOOD-ROW_ID.

      CLEAR: WA_ZSDT0075.
      SELECT SINGLE * FROM ZSDT0075 INTO WA_ZSDT0075
       WHERE KUNNR = WL_CONTAB-HKONT
         AND BDATU >= SY-DATUM.

      IF ( SY-SUBRC NE 0 ) AND ( WL_CONTAB-ZLSCH NE C_D ).
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para este cliente é obrigatório ser emitido Boleto de Cobrança ajustar a forma de pagamento'.
      ENDIF.



*      IF XBOL = 'N'.                                                  "Validação pela função "AG"
*        IF LV_VALUE <> 'D'.                                           "A formapgto na aba contabilização é tipo "D"
*          CLEAR: LV_VALUE.                                            "Limpa a variável
*          READ TABLE TG_CONTAB INTO WL_CONTAB INDEX LS_GOOD-ROW_ID.
*          MOVE: LV_VALUE TO WL_CONTAB-ZLSCH.
*          MODIFY TG_CONTAB FROM WL_CONTAB INDEX SY-TABIX."ls_good-row_id.
*          CLEAR: WL_CONTAB, TG_CONTAB.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para este cliente é obrigatório ser emitido Boleto de Cobrança ajustar a forma de pagamento'.
*        ENDIF.
*      ELSE.


      "ENDIF.




    ENDLOOP.

    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

*** Método de atualização de dados na Tela
    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED3
  METHOD ON_DATA_CHANGED_FINISHED4.

    DATA: LS_GOOD      TYPE LVC_S_MODI,
          WL_IMPO_AUX  LIKE LINE OF TG_IMPO_AUX,
          WL_IMPO_COMP LIKE LINE OF TG_IMPO_COMP,
          WL_ITENS2    LIKE LINE OF TG_ITENS.
    CLEAR: WL_IMPO_AUX.
*    break abap.
    READ TABLE TG_ITENS INTO WL_ITENS2 INDEX NUM_ROW.
    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
    WHERE TABIX GT 0.

      READ TABLE TG_IMPO_AUX INTO TG_IMPO_AUX INDEX LS_GOOD-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        READ TABLE TG_IMPO_AUX INTO WL_IMPO_AUX
          WITH KEY TAXTYP = TG_IMPO_AUX-TAXTYP.

        IF SY-SUBRC IS INITIAL.
          MOVE: TG_IMPO_AUX-BASE   TO WL_IMPO_AUX-BASE,
                TG_IMPO_AUX-RATE   TO WL_IMPO_AUX-RATE,
                TG_IMPO_AUX-TAXVAL TO WL_IMPO_AUX-TAXVAL,
                TG_IMPO_AUX-EXCBAS TO WL_IMPO_AUX-EXCBAS,
                TG_IMPO_AUX-OTHBAS TO WL_IMPO_AUX-OTHBAS.
          MODIFY TG_IMPO_AUX FROM WL_IMPO_AUX INDEX SY-TABIX."ls_good-row_id.
          "CLEAR: wl_impo_aux, tg_impo_aux.
        ENDIF.
        READ TABLE TG_IMPO_COMP INTO WL_IMPO_COMP WITH KEY ITMNUM = WL_ITENS2-ITMNUM
                                                           TAXTYP = TG_IMPO_AUX-TAXTYP BINARY SEARCH.
        IF SY-SUBRC NE 0.
          MOVE-CORRESPONDING: WL_IMPO_AUX TO WL_IMPO_COMP.
          MOVE: WL_ITENS2-ITMNUM TO WL_IMPO_COMP-ITMNUM,
                ABAP_TRUE        TO WL_IMPO_COMP-EDIT.
          APPEND WL_IMPO_COMP TO TG_IMPO_COMP.
        ELSE.
          MOVE-CORRESPONDING: WL_IMPO_AUX TO WL_IMPO_COMP.
          MOVE: WL_ITENS2-ITMNUM TO WL_IMPO_COMP-ITMNUM,
                ABAP_TRUE        TO WL_IMPO_COMP-EDIT.
          MODIFY TG_IMPO_COMP INDEX SY-TABIX FROM WL_IMPO_COMP.
        ENDIF.
        CLEAR: TG_IMPO_COMP.
        CLEAR: WL_IMPO_AUX, TG_IMPO_AUX.
      ENDIF.
    ENDLOOP.

*    PERFORM verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen      = '100'
*        i_show        = space
*        i_repid       = sy-repid
*        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*        i_set_field   = 'X_FIELD'
*      IMPORTING
*        e_messagem    = wg_mensagem
*      TABLES
*        it_msgs       = tg_msg_ret.

*** Método de atualização de dados na Tela
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED3
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_DRAGDROP_RECEIVER IMPLEMENTATION.
  METHOD NODE_DOUBLE_CLICK.
    DATA: WL_MENSAGEMS LIKE LINE OF TG_MENSAGEMS,
          TL_EDITOR    TYPE TABLE OF TY_EDITOR,
          WL_EDITOR    TYPE TY_EDITOR.

    REFRESH: TL_EDITOR.
    IF NODE_KEY EQ C_ROOT.
      LOOP AT TG_MENSAGEMS_AUX INTO WL_MENSAGEMS.

        WL_EDITOR-LINE = WL_MENSAGEMS-MESSAGE.
        APPEND WL_EDITOR TO TL_EDITOR.
      ENDLOOP.

      CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TL_EDITOR.

      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ELSE.
      LOOP AT TG_MENSAGEMS_AUX INTO WL_MENSAGEMS
         WHERE SEQNUM EQ NODE_KEY.

        WL_EDITOR-LINE = WL_MENSAGEMS-MESSAGE.
        APPEND WL_EDITOR TO TL_EDITOR.
      ENDLOOP.

      CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TL_EDITOR.

      READ TABLE TG_MENSAGEMS INTO WL_MENSAGEMS
        WITH KEY SEQNUM = NODE_KEY.

      IF SY-SUBRC IS NOT INITIAL.
        READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
          WITH KEY GROUP1 = 'GR1'.
        IF SY-SUBRC IS INITIAL.
          CALL METHOD EDITOR->SET_READONLY_MODE
            EXPORTING
              READONLY_MODE = 0.
          WG_DCLKNODEKEY  = NODE_KEY.
        ELSE.
          CALL METHOD EDITOR->SET_READONLY_MODE
            EXPORTING
              READONLY_MODE = 1.
        ENDIF.
      ELSE.
        CALL METHOD EDITOR->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT.
  IF SY-CALLD = 'X' AND  P_SEQ_LCTO IS INITIAL.
    GET PARAMETER ID 'SEQ' FIELD  P_SEQ_LCTO.
    OK-CODE = C_SEARCH.
  ENDIF.
  PERFORM VERIFICA_ERROS.
  TAB_STRIP_NF-ACTIVETAB = G_TAB_STRIP_NF-PRESSED_TAB.
  CASE G_TAB_STRIP_NF-PRESSED_TAB.
    WHEN C_TAB_STRIP_NF-TAB1.
      G_TAB_STRIP_NF-SUBSCREEN = '0213'.
    WHEN C_TAB_STRIP_NF-TAB2.
      G_TAB_STRIP_NF-SUBSCREEN = '0220'.
    WHEN C_TAB_STRIP_NF-TAB3.
      G_TAB_STRIP_NF-SUBSCREEN = '0230'.
    WHEN C_TAB_STRIP_NF-TAB4.
      G_TAB_STRIP_NF-SUBSCREEN = '0241'.
    WHEN C_TAB_STRIP_NF-TAB5.
      G_TAB_STRIP_NF-SUBSCREEN = '0250'.
    WHEN C_TAB_STRIP_NF-TAB6.
      G_TAB_STRIP_NF-SUBSCREEN = '0215'.                    "'0211'.
    WHEN C_TAB_STRIP_NF-TAB7.
      G_TAB_STRIP_NF-SUBSCREEN = '0212'.
    WHEN C_TAB_STRIP_NF-TAB8.
      G_TAB_STRIP_NF-SUBSCREEN = '0260'.
    WHEN C_TAB_STRIP_NF-TAB9.
      G_TAB_STRIP_NF-SUBSCREEN = '0270'.
    WHEN   C_TAB_STRIP_NF-TAB10.
      G_TAB_STRIP_NF-SUBSCREEN = '0280'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE TAB_STRIP_NF_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'LIM'.
      PERFORM F_GET_DATA_LIM.
    WHEN C_TAB_STRIP_NF-TAB1.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB1.
    WHEN C_TAB_STRIP_NF-TAB2.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB2.
    WHEN C_TAB_STRIP_NF-TAB3.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB3.
    WHEN C_TAB_STRIP_NF-TAB4.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB4.
    WHEN C_TAB_STRIP_NF-TAB5.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB5.
    WHEN C_TAB_STRIP_NF-TAB6.

      PERFORM F_CHECK_REGRAS_ZNFW0009.

      IF ( WG_FISCAL-EBELN IS INITIAL AND WG_FISCAL-VBELN IS INITIAL ) AND
         ( VINICIOU_LCTO_ZNFW0009 = 'X'   ) AND
         ( VLANCAMENTO_ZNFW0009   = 'MIC' ) AND SY-SUBRC EQ 0.
        MESSAGE 'Não informado o Nr. Pedido ou lançamento sem Ordem de Venda!' TYPE  'S'.
        EXIT.
      ELSE.
        PERFORM Z_SUGERE_ITENS.
        G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB6.
      ENDIF.
    WHEN C_TAB_STRIP_NF-TAB7.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB7.
    WHEN C_TAB_STRIP_NF-TAB8.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB8.
    WHEN C_TAB_STRIP_NF-TAB9.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB9.
    WHEN C_TAB_STRIP_NF-TAB10.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB10.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.



ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: FCODE.
  READ TABLE TG_FIELDS
   WITH KEY GROUP1 = 'GR1'.
  IF SY-SUBRC IS NOT INITIAL.
    APPEND C_SAVE TO FCODE.
    APPEND C_ATUALI TO FCODE.
    APPEND C_DELDOC TO FCODE.
  ENDIF.

  IF SY-UCOMM <> C_ADD. "LIM PSA Inclui no FCODE para ele exluir do menu
    APPEND  C_LIM TO FCODE.
  ENDIF.

  IF SY-TCODE EQ C_DISPLAY.
    SET PF-STATUS 'Z002' EXCLUDING FCODE.
  ELSE.
    SET PF-STATUS 'Z001' EXCLUDING FCODE.
  ENDIF.
  IF TG_ITENS[] IS NOT INITIAL.
*    SET CURSOR FIELD 'WG_DESC_OPERACAO'.
  ENDIF.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*& salvar processo
*&---------------------------------------------------------------------*
FORM SALVAR_PROCESSO.

  "133290 CS2024000037 Liberar tabela de vinculação de NCM vs produto SAP - znfw0009 Lib. tab. vinc. de NCM vs prod. SAP  PBALVES
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM Z_SUGERE_ITENS.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DELETE TG_ITENS WHERE MATNR IS INITIAL
                    AND WERKS IS INITIAL
                    AND NETWR IS INITIAL.

  IF V_AUTOMATICO_MEMO = ABAP_FALSE.
    CALL METHOD GRID1->CHECK_CHANGED_DATA.
  ENDIF.

  PERFORM VERIFICA_ERROS.

  IF TG_MSG_RET[] IS INITIAL.
    PERFORM GRAVA_DADOS.
    REFRESH: TG_FIELDS.
*-CS2023000043-09.02.2023-#102019-JT-inicio
    CLEAR: WG_ACAO.
    IF GRID2 IS NOT INITIAL.
      CALL METHOD GRID2->FREE.
    ENDIF.
    FREE: CONTAINER_2, GRID2, TG_IMPO_AUX.
*-CS2023000043-09.02.2023-#102019-JT-fim

  ELSE.
    IF V_AUTOMATICO_MEMO = ABAP_FALSE.
      MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: WL_0008       TYPE ZFIWRT0008,
        WL_ZIB_CHV    TYPE ZIB_CONTABIL_CHV,
        WL_ZIB_OBJKEY TYPE ZIB_CONTABIL-OBJ_KEY,
        MSG(255),
        RESPOSTA,
        OPT           TYPE CTU_PARAMS,
        TL_BDC        TYPE TABLE OF BDCDATA,
        WL_BDC        TYPE BDCDATA,
        WL_DOC_REF    TYPE ZFIWRT0008-SEQ_LCTO.

  REFRESH: TL_BDC.

  READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
    WITH KEY GROUP1 = 'GR1'.
  IF SY-SUBRC IS INITIAL.
    IF OK-CODE = C_SEARCH.
      OK-CODE = C_ATUALI.
    ENDIF.

  ENDIF.

  CASE OK-CODE.
    WHEN 'ANEXAR'.
      PERFORM HABILITAR_WORKFLOW_DOCUMENTOS.
    WHEN C_DELDOC.
      PERFORM ELEMINA_DOC.
      PERFORM LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      CLEAR: P_OPERACAO, P_SEQ_LCTO, P_PARVW, P_PARID, P_BRANCH, P_BUKRS, WG_DESC_PARID,
             WG_DESC_OPERACAO, WG_DESC_PARVW, WG_DESC_BRANCH, WG_DESC_BUKRS, P_LOEKZ.
    WHEN C_SEARCH.
      PERFORM LIMPA_CAMPOS.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DESCRICOES.
    WHEN C_SAVE.
*-CS2021000595 - 22.06.2021 - JT - inicio
      PERFORM SALVAR_PROCESSO.
*-CS2021000595 - 22.06.2021 - JT - fim

    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = P_SEQ_LCTO.
      WL_DOC_REF = P_SEQ_LCTO.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = WL_DOC_REF.
      SET SCREEN 0.

    WHEN C_ADD.

      PERFORM Z_NOVO_LAN.

      WG_ACAO = C_ADD.  "*-CS2023000043-09.02.2023-#102019-JT

*      IF WG_FLAG IS INITIAL.
*
*        WG_SUGERE_TRANSP = ABAP_TRUE.
*
*        PERFORM LIMPA_CAMPOS.
*        CLEAR: P_OPERACAO, P_SEQ_LCTO, P_PARVW, P_PARID, P_BRANCH, P_BUKRS, WG_DESC_PARID,
*               WG_DESC_OPERACAO, WG_DESC_PARVW, WG_DESC_BRANCH, WG_DESC_BUKRS, P_LOEKZ.
*        PERFORM GET_NEXT_NUMBER IN PROGRAM ZWRR0001 USING  'ZSEQ_LCTO'
*                                                        '1'
*                                               CHANGING P_SEQ_LCTO.
*
*      ENDIF.
*      PERFORM TRATA_CAMPOS USING SPACE
*                                  'GR1'
*                                     C_1       "INPUT 1     NO INPUT 0
*                                     C_0.      "INVISIBLE 1 VISIBLE 0
*      PERFORM TRATA_CAMPOS USING SPACE
*                                'GR2'
*                                 C_0       "INPUT 1     NO INPUT 0
*                                 C_0.      "INVISIBLE 1 VISIBLE 0
**      wg_docs-bldat = sy-datum.
*      WG_DOCS-BUDAT = SY-DATUM.
    WHEN C_CANCEL.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = P_SEQ_LCTO.

      WL_DOC_REF = P_SEQ_LCTO.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = WL_DOC_REF.

      LEAVE TO TRANSACTION 'ZNFW0002'.
      REFRESH: TG_FIELDS.
      CLEAR: P_OPERACAO, P_SEQ_LCTO, P_PARVW, P_PARID, P_BRANCH, P_BUKRS, WG_DESC_PARID,
             WG_DESC_OPERACAO, WG_DESC_PARVW, WG_DESC_BRANCH, WG_DESC_BUKRS, P_LOEKZ.
      PERFORM LIMPA_CAMPOS.
*      PERFORM busca_dados_doc.

    WHEN C_ATUALI.
*      refresh: tg_parc.
      PERFORM BUSCA_DADOS.
*      PERFORM busca_dados_doc.
      PERFORM BUSCA_DESCRICOES.
    WHEN C_MODIF.

      IF P_SEQ_LCTO IS NOT INITIAL.
        TRY.
            ZCL_BOLETIM_PRODUCAO=>ZIF_BOLETIM_PRODUCAO~GET_INSTANCE(
            )->CHECK_PERMISSAO_MODIFICACAO( I_SEQ_LCTO_ZNFW = P_SEQ_LCTO ).
          CATCH ZCX_BOLETIM_PRODUCAO INTO DATA(ZCX_BOL_PROD).
            ZCX_BOL_PROD->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'I' I_MSGTY_DISPLAY = 'W' ).
            RETURN.
        ENDTRY.
      ENDIF.

      REFRESH: TG_FIELDS.
      SELECT SINGLE *
        FROM ZFIWRT0008
         WHERE SEQ_LCTO EQ P_SEQ_LCTO
           AND LOEKZ EQ SPACE.
      IF SY-SUBRC IS INITIAL
      AND ZFIWRT0008-OPERACAO EQ P_OPERACAO
      AND ZFIWRT0008-BRANCH   EQ P_BRANCH
      AND ZFIWRT0008-PARID    EQ P_PARID.

        CLEAR: WL_ZIB_OBJKEY, WL_ZIB_CHV.
        CONCATENATE 'ZGF' ZFIWRT0008-SEQ_LCTO ZFIWRT0008-BUDAT(4) INTO WL_ZIB_OBJKEY.

        SELECT SINGLE *
          FROM ZIB_CONTABIL_CHV
           INTO WL_ZIB_CHV
            WHERE OBJ_KEY EQ WL_ZIB_OBJKEY.

        IF  ZFIWRT0008-DOCNUM IS INITIAL AND ZFIWRT0008-MBLNR  IS INITIAL    AND WL_ZIB_CHV-BELNR IS INITIAL.
          CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
            EXPORTING
*             MODE_ZFIWRT0008       = 'E'
*             MANDT          = SY-MANDT
              SEQ_LCTO       = P_SEQ_LCTO
*             X_SEQ_LCTO     = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC NE 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ELSE.

            PERFORM TRATA_CAMPOS USING SPACE
                                        'GR1'
                                           C_1       "INPUT 1     NO INPUT 0
                                           C_0.      "INVISIBLE 1 VISIBLE 0
            PERFORM TRATA_CAMPOS USING SPACE
                                      'GR2'
                                       C_0       "INPUT 1     NO INPUT 0
                                       C_0.      "INVISIBLE 1 VISIBLE 0

            IF TG_PARC[] IS INITIAL.

            ENDIF.
          ENDIF.
          WG_ACAO = C_MODIF.
        ELSE.
          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'O documento, não pode ser'
                                                      'modificado.'.
        ENDIF.
      ENDIF.
    WHEN C_GERAR_NF.
      CLEAR: WL_0008.
      SELECT SINGLE *
        FROM ZFIWRT0008
         INTO WL_0008
         WHERE SEQ_LCTO EQ P_SEQ_LCTO.
      IF SY-SUBRC IS INITIAL.

*        PERFORM F_PREENCHER_DYNPRO USING:
*        'X' 'ZWRR0004'             '0100',
*        ' ' 'P_SEQ_LCTO'           P_SEQ_LCTO,
*        ' ' 'BDC_OKCODE'           'SEARCH'.
*
*        OPT-DISMODE = 'E'.
*        OPT-DEFSIZE = ' '.
*
*        CALL TRANSACTION 'ZNFW0005' USING TL_BDC OPTIONS FROM OPT.
        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD P_SEQ_LCTO.
        SUBMIT ZWRR0004  AND RETURN.
      ENDIF.
    WHEN C_EXIT.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = P_SEQ_LCTO.
      WL_DOC_REF = P_SEQ_LCTO.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          SEQ_LCTO = WL_DOC_REF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_DG'
CONSTANTS: BEGIN OF C_TAB_STRIP_DG,
             TAB1 LIKE SY-UCOMM VALUE 'TAB_STRIP_DG_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'TAB_STRIP_DG_FC2',
             TAB3 LIKE SY-UCOMM VALUE 'TAB_STRIP_DG_FC3',
           END OF C_TAB_STRIP_DG.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_DG'
CONTROLS:  TAB_STRIP_DG TYPE TABSTRIP.
DATA: BEGIN OF G_TAB_STRIP_DG,
        SUBSCREEN   LIKE SY-DYNNR,
        PROG        LIKE SY-REPID VALUE 'ZWRR0002',
        PRESSED_TAB LIKE SY-UCOMM VALUE C_TAB_STRIP_DG-TAB1,
      END OF G_TAB_STRIP_DG.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_DG'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE TAB_STRIP_DG_ACTIVE_TAB_SET OUTPUT.
  TAB_STRIP_DG-ACTIVETAB = G_TAB_STRIP_DG-PRESSED_TAB.
  CASE G_TAB_STRIP_DG-PRESSED_TAB.
    WHEN C_TAB_STRIP_DG-TAB1.
      G_TAB_STRIP_DG-SUBSCREEN = '0215'.                    "'0211'.
    WHEN C_TAB_STRIP_DG-TAB2.
      G_TAB_STRIP_DG-SUBSCREEN = '0212'.
    WHEN C_TAB_STRIP_DG-TAB3.
      G_TAB_STRIP_DG-SUBSCREEN = '0220'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_DG_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_DG'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE TAB_STRIP_DG_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TAB_STRIP_DG-TAB1.
      G_TAB_STRIP_DG-PRESSED_TAB = C_TAB_STRIP_DG-TAB1.
    WHEN C_TAB_STRIP_DG-TAB2.
      G_TAB_STRIP_DG-PRESSED_TAB = C_TAB_STRIP_DG-TAB2.
    WHEN C_TAB_STRIP_DG-TAB3.
      G_TAB_STRIP_DG-PRESSED_TAB = C_TAB_STRIP_DG-TAB3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_DG_ACTIVE_TAB_GET INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_IMPOSTOS' ITSELF
CONTROLS: TC_IMPOSTOS TYPE TABLEVIEW USING SCREEN 0220.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_IMPOSTOS_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE TG_IMPO LINES TC_IMPOSTOS-LINES.
ENDMODULE.                    "TC_IMPOSTOS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_ITENS' ITSELF
CONTROLS: TC_ITENS TYPE TABLEVIEW USING SCREEN 0211.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_ITENS'
DATA:     G_TC_ITENS_LINES  LIKE SY-LOOPC.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_ITENS_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE TG_ITENS LINES TC_ITENS-LINES.
ENDMODULE.                    "TC_ITENS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC_ITENS_GET_LINES OUTPUT.
  G_TC_ITENS_LINES = SY-LOOPC.
ENDMODULE.                    "TC_ITENS_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TC_ITENS_MODIFY INPUT.
  DATA: WL_LINE      TYPE SY-TABIX,
        TL_ITENS_AUX LIKE TG_ITENS OCCURS 0 WITH HEADER LINE,
        WL_MARC      TYPE MARC,
        WL_MBEW      TYPE MBEW,
        WL_1BBRANCH  TYPE J_1BBRANCH,
        WL_1BAA      TYPE J_1BAA,
        WL_1BAPN     TYPE  J_1BAPN,
        WL_DIRECT    TYPE J_1BAPNV-DIRECT,
        WL_DSTCAT    TYPE J_1BAPNV-DSTCAT.

***> Determina o CFOP
  IF WG_DIREITOS-CFOP IS INITIAL.
    SELECT SINGLE *
      FROM MARC
      INTO WL_MARC
       WHERE MATNR EQ TG_ITENS-MATNR.

    IF SY-SUBRC IS INITIAL.
      TG_ITENS-STEUC  = WL_MARC-STEUC.

      SELECT SINGLE *
        FROM MBEW
        INTO WL_MBEW
         WHERE MATNR EQ TG_ITENS-MATNR
           AND BWKEY EQ TG_ITENS-WERKS.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE *
          FROM J_1BBRANCH
           INTO WL_1BBRANCH
           WHERE BUKRS  EQ P_BUKRS
             AND BRANCH EQ P_BRANCH.

        IF SY-SUBRC IS INITIAL.
          SELECT SINGLE *
            FROM J_1BAA
            INTO WL_1BAA
             WHERE NFTYPE EQ WG_FISCAL-NFTYPE.

          IF WL_1BAA-ENTRAD EQ C_X.
            WL_DIRECT = C_1.
          ELSE.
            WL_DIRECT = C_2.
          ENDIF.

          IF WG_DIREITOS-INDCOPER EQ C_D.
            WL_DSTCAT = C_0.

          ELSE.
            WL_DSTCAT = C_1.

          ENDIF.

          SELECT SINGLE *
            FROM J_1BAPN
            INTO WL_1BAPN
             WHERE DIRECT EQ WL_DIRECT
               AND DSTCAT EQ WL_DSTCAT
               AND INDUS3 EQ WL_MARC-INDUS
               AND ITMTYP EQ WG_FISCAL-ITMTYP
               AND OWNPRO EQ WL_MBEW-OWNPR
               AND MATUSE EQ WL_MBEW-MTUSE
               AND INDUS1 EQ WL_1BBRANCH-INDUSTRY.

          IF SY-SUBRC IS INITIAL.
            TG_ITENS-CFOP = WL_1BAPN-CFOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM MARC
      INTO WL_MARC
       WHERE MATNR EQ TG_ITENS-MATNR.

    TG_ITENS-STEUC  = WL_MARC-STEUC.
    TG_ITENS-CFOP = WG_DIREITOS-CFOP.
  ENDIF.
***<
***> Total
*  tg_itens-netwr = tg_itens-menge * tg_itens-netpr.
  TG_ITENS-NETWR = ( ( TG_ITENS-MENGE * TG_ITENS-NETPR ) + TG_ITENS-NETFRE + TG_ITENS-NETINS + TG_ITENS-NETOTH ) - TG_ITENS-NETDIS.
***<

  TL_ITENS_AUX[] = TG_ITENS[].
  DELETE TL_ITENS_AUX WHERE ITMNUM IS INITIAL.
  SORT: TL_ITENS_AUX BY ITMNUM.
  DESCRIBE TABLE TL_ITENS_AUX LINES WL_LINE.
  READ TABLE TL_ITENS_AUX INDEX WL_LINE TRANSPORTING ITMNUM.
***Verifica entrada na tabela de itens
  ADD 10 TO TL_ITENS_AUX-ITMNUM.
  MOVE: TL_ITENS_AUX-ITMNUM TO TG_ITENS-ITMNUM.

  MODIFY TG_ITENS
    INDEX TC_ITENS-CURRENT_LINE.

  IF SY-SUBRC IS NOT INITIAL.
    TG_ITENS-FASE = ICON_DISPLAY_MORE.
    APPEND TG_ITENS.

  ENDIF.
ENDMODULE.                    "TC_ITENS_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TC_ITENS_MARK INPUT.
  DATA: G_TC_ITENS_WA2 LIKE LINE OF TG_ITENS.
  IF TC_ITENS-LINE_SEL_MODE = 1
  AND TG_ITENS-MARK = 'X'.
    LOOP AT TG_ITENS INTO G_TC_ITENS_WA2
      WHERE MARK = 'X'.
      G_TC_ITENS_WA2-MARK = ''.
      MODIFY TG_ITENS
        FROM G_TC_ITENS_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY TG_ITENS
    INDEX TC_ITENS-CURRENT_LINE
    TRANSPORTING MARK.
ENDMODULE.                    "TC_ITENS_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TC_ITENS_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_ITENS'
                              'TG_ITENS'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.                    "TC_ITENS_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                         P_TABLE_NAME
                         P_MARK_NAME
                CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: L_OK     TYPE SY-UCOMM,
        L_OFFSET TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH P_OK FOR P_TC_NAME.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  L_OFFSET = STRLEN( P_TC_NAME ) + 1.
  L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE L_OK.
    WHEN 'INSR'.                      "insert row
      PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                        P_TABLE_NAME.
      CLEAR P_OK.

    WHEN 'DELE'.                      "delete row
      PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME.
      CLEAR P_OK.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                            L_OK.
      CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME   .
      CLEAR P_OK.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                          P_TABLE_NAME
                                          P_MARK_NAME .
      CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_INSERT_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_LINES_NAME       LIKE FELD-NAME.
  DATA L_SELLINE          LIKE SY-STEPL.
  DATA L_LASTLINE         TYPE I.
  DATA L_LINE             TYPE I.
  DATA L_TABLE_NAME       LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
  ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE L_SELLINE.
  IF SY-SUBRC <> 0.                   " append line to table
    L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
    IF L_SELLINE > <LINES>.
      <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
    ELSE.
      <TC>-TOP_LINE = 1.
    ENDIF.
  ELSE.                               " insert line into table
    L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
    L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
  <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE L_LINE.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_DELETE_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME
                       P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    IF <MARK_FIELD> = 'X'.
      DELETE <TABLE> INDEX SYST-TABIX.
      IF SY-SUBRC = 0.
        <TC>-LINES = <TC>-LINES - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                      P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TC_NEW_TOP_LINE     TYPE I.
  DATA L_TC_NAME             LIKE FELD-NAME.
  DATA L_TC_LINES_NAME       LIKE FELD-NAME.
  DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
  ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
  IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
    L_TC_NEW_TOP_LINE = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        ENTRY_ACT      = <TC>-TOP_LINE
        ENTRY_FROM     = 1
        ENTRY_TO       = <TC>-LINES
        LAST_PAGE_FULL = 'X'
        LOOPS          = <LINES>
        OK_CODE        = P_OK
        OVERLAPPING    = 'X'
      IMPORTING
        ENTRY_NEW      = L_TC_NEW_TOP_LINE
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD L_TC_FIELD_NAME
             AREA  L_TC_NAME.

  IF SYST-SUBRC = 0.
    IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  SET_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUBSCREEN OUTPUT.

ENDMODULE.                 " SET_SUBSCREEN  OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_CONTAB' ITSELF
CONTROLS: TC_CONTAB TYPE TABLEVIEW USING SCREEN 0240.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_CONTAB_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE TG_CONTAB LINES TC_CONTAB-LINES.
ENDMODULE.                    "TC_CONTAB_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_MOVEST' ITSELF
CONTROLS: TC_MOVEST TYPE TABLEVIEW USING SCREEN 0250.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_MOVEST_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE TG_MOVEST LINES TC_MOVEST-LINES.
ENDMODULE.                    "TC_MOVEST_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_0001     TYPE ZFIWRT0001,
        TL_0002     TYPE TABLE OF ZFIWRT0002 WITH HEADER LINE,
        TL_0003     TYPE TABLE OF ZFIWRT0003 WITH HEADER LINE,
        TL_0004     TYPE TABLE OF ZFIWRT0004 WITH HEADER LINE,
        TL_0005     TYPE TABLE OF ZFIWRT0005 WITH HEADER LINE,
        TL_0006     TYPE TABLE OF ZFIWRT0006 WITH HEADER LINE,
        TL_0007     TYPE TABLE OF ZFIWRT0007 WITH HEADER LINE,
        WL_0019     TYPE  ZFIWRT0019,
        TL_1BAJ     TYPE TABLE OF J_1BAJ     WITH HEADER LINE,
        TL_1BAJT    TYPE TABLE OF J_1BAJT   WITH HEADER LINE,
        TL_TBSL     TYPE TABLE OF TBSL       WITH HEADER LINE,
        TL_SKAT     TYPE TABLE OF SKAT       WITH HEADER LINE,
        TL_CSKB     TYPE TABLE OF CSKB       WITH HEADER LINE,
        TL_USER     TYPE TABLE OF USER_ADDR  WITH HEADER LINE,
        WL_KNA1     TYPE KNA1,
        WL_LFA1     TYPE LFA1,
        WL_T001W    TYPE T001W,
        WL_T001     TYPE T001,
        WL_1BBRANCH TYPE J_1BBRANCH,
        WL_1BAD     TYPE J_1BAD,
        WL_1BADT    TYPE J_1BADT,
        WL_1BAA     TYPE J_1BAA.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: LT_RETURNS         TYPE TABLE OF BAPIRET2,
        LS_COELDES         TYPE BAPI1030_CEOUTPUTLIST,
        LS_CSKB            LIKE LINE OF TL_CSKB,
        LV_CONTROLLINGAREA TYPE  BAPI1030_GEN-CO_AREA,
        LV_COSTELEMENT     TYPE  BAPI1030_GEN-COST_ELEM,
        LV_KEYDATE         TYPE  BAPI1030_GEN-SOME_DATE.
* <--- S4 Migration - 18/07/2023 - CA

  REFRESH: TL_0002, TL_0003, TL_0004, TL_0005, TL_0006, TL_1BAJ, TL_1BAJT, TL_TBSL,
           TL_SKAT, TL_CSKB.
  CLEAR: WL_0001, TL_0002, TL_0003, TL_0004, TL_0005, TL_0006, TL_1BAJ, TL_1BAJT,
         WL_KNA1, WL_LFA1, WL_T001W, TL_TBSL, TL_SKAT, TL_CSKB, WL_1BAA.

  P_PARID = |{ P_PARID ALPHA = IN }|.

  SELECT SINGLE *
  FROM ZFIWRT0008
  INTO WL_0008
   WHERE SEQ_LCTO EQ P_SEQ_LCTO.
  IF SY-SUBRC IS INITIAL.
    CONCATENATE 'Ao atualizar os campos, você pode perder as informacões originais'
                ' do documento, têm certeza que deseja atualizar?' INTO MSG.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = TEXT-006
        TEXT_QUESTION         = MSG
        TEXT_BUTTON_1         = 'Sim'
        ICON_BUTTON_1         = '@0V@'
        TEXT_BUTTON_2         = 'Não'
        ICON_BUTTON_2         = '@0W@'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = RESPOSTA.
    IF RESPOSTA EQ C_1.

    ELSE.
      IF V_AUTOMATICO_MEMO = ABAP_FALSE.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDIF.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF P_OPERACAO <> P_OPERACAO_OLD.
    FREE: WG_DIREITOS-TAXLW1,
          WG_DIREITOS-TAXLW2,
          WG_DIREITOS-TAXLW4,
          WG_DIREITOS-TAXLW5,
          TG_IMPO_COMP.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

* US #163043 - MMSILVA - 18.03.2025 - Inicio
  IF P_OPERACAO IS NOT INITIAL AND P_CHAVE_ACESSO IS NOT INITIAL.
    PERFORM PREENCHE_DADOS_NFE.
  ENDIF.
* US #163043 - MMSILVA - 18.03.2025 - Fim

  IF P_BUKRS IS NOT INITIAL.
    SELECT COUNT(*)
      FROM T001
       WHERE BUKRS EQ P_BUKRS.
    IF SY-SUBRC IS NOT INITIAL.
      IF V_AUTOMATICO_MEMO = ABAP_FALSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cód. de Empresa não existe!'.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM T001W
    INTO WL_T001W
     WHERE WERKS EQ P_BRANCH.

  IF P_OPERACAO IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZFIWRT0001
      INTO WL_0001
       WHERE OPERACAO EQ P_OPERACAO.

    IF SY-SUBRC IS INITIAL.

      W_ZFIWRT0001 = WL_0001.  "*-CS2023000043-09.02.2023-#102019-JT

      IF ( WL_0001-DISP_NF_CCT = 'S' ) AND ( P_SEQ_LCTO IS NOT INITIAL ).
        SELECT SINGLE *
          FROM ZFIWRT0008 INTO @DATA(_WL_0008)
         WHERE SEQ_LCTO EQ @P_SEQ_LCTO.

        IF ( SY-SUBRC NE 0 ) AND ( WG_SUGERE_TRANSP EQ ABAP_TRUE ) AND ( P_OPERACAO = '0019' ). "Form. Lote Produto Acabado Itacoatiara
          CLEAR: WG_SUGERE_TRANSP.
          WG_TRANSPORTE-LIFNR = '0000001002'.
        ENDIF.
      ENDIF.

      SELECT SINGLE *
        FROM J_1BAA
        INTO WL_1BAA
         WHERE NFTYPE EQ WL_0001-NFTYPE.

      IF WL_0001-PARVW EQ C_AG.
        SELECT SINGLE *
          FROM KNA1
          INTO WL_KNA1
           WHERE KUNNR EQ P_PARID.

      ELSEIF WL_0001-PARVW EQ C_BR
       OR   WL_0001-PARVW EQ C_LF.
        SELECT SINGLE *
          FROM LFA1
          INTO WL_LFA1
           WHERE LIFNR EQ P_PARID.

      ENDIF.
      IF WL_0001-LM_ESTOQUE = 'S'.
        MOVE: WL_0001-DESCRICAO TO WG_DESC_OPERACAO.
      ELSE.
        SELECT *
          FROM ZFIWRT0002
          INTO TABLE TL_0002
           WHERE OPERACAO EQ P_OPERACAO.

        IF SY-SUBRC IS INITIAL.
          MOVE: WL_0001-DESCRICAO TO WG_DESC_OPERACAO.

          SELECT *
            FROM J_1BAJ
            INTO TABLE TL_1BAJ
             FOR ALL ENTRIES IN TL_0002
             WHERE TAXTYP EQ TL_0002-TAXTYP.

          SELECT *
            FROM J_1BAJT
            INTO TABLE TL_1BAJT
             FOR ALL ENTRIES IN TL_0002
            WHERE  SPRAS  EQ SY-LANGU
              AND  TAXTYP EQ TL_0002-TAXTYP.
        ELSE.
          CLEAR: WG_DESC_OPERACAO.
          IF V_AUTOMATICO_MEMO = ABAP_FALSE.
            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cód. de Operação não existe!'.
            LEAVE TO SCREEN 100.
          ELSE.
            LEAVE TO SCREEN 0.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT *
        FROM ZFIWRT0003
        INTO TABLE TL_0003
         WHERE OPERACAO EQ P_OPERACAO.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM TBSL
          INTO TABLE TL_TBSL
           FOR ALL ENTRIES IN TL_0003
           WHERE BSCHL EQ TL_0003-BSCHL.

        SELECT *
          FROM SKAT
          INTO TABLE TL_SKAT
           FOR ALL ENTRIES IN TL_0003
            WHERE SPRAS EQ SY-LANGU
              AND KTOPL EQ C_50
              AND SAKNR EQ TL_0003-HKONT.

* ---> S4 Migration - 18/07/2023 - CA
*        SELECT *
*        FROM cskb
*        INTO TABLE tl_cskb
*         FOR ALL ENTRIES IN tl_0003
*          WHERE kstar EQ tl_0003-hkont
*            AND  ( datbi GE sy-datum
*              AND datab LE sy-datum )
*            AND katyp EQ '01'.

        "Seleção não possuía filtro por controlling; selecionar todos
        SELECT KOKRS
          FROM TKA01
          INTO TABLE @DATA(TL_TKA01)
          WHERE KOKRS <> @SPACE.
        IF SY-SUBRC IS INITIAL.

          LOOP AT TL_TKA01 INTO DATA(LS_TKA01).

            LV_CONTROLLINGAREA  = LS_TKA01-KOKRS.

            LOOP AT TL_0003 INTO DATA(LS_0003).

              LV_COSTELEMENT      = LS_0003-HKONT.
              LV_KEYDATE          = SY-DATUM.

              CLEAR: LT_RETURNS[], LS_COELDES.

              CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
                EXPORTING
                  CONTROLLINGAREA   = LV_CONTROLLINGAREA
                  COSTELEMENT       = LV_COSTELEMENT
                  KEYDATE           = LV_KEYDATE
                IMPORTING
                  COSTELEMENTDETAIL = LS_COELDES
                TABLES
                  RETURN            = LT_RETURNS.

              READ TABLE LT_RETURNS TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
              IF SY-SUBRC <> 0 AND
                 LS_COELDES-CELEM_CATEGORY = '01'.

                LS_CSKB-KOKRS = LS_TKA01-KOKRS.
                LS_CSKB-KSTAR = LS_0003-HKONT.
                LS_CSKB-KATYP = LS_COELDES-CELEM_CATEGORY.

                APPEND LS_CSKB TO TL_CSKB.
                CLEAR LS_CSKB.
              ENDIF.

              CLEAR: LS_0003.
            ENDLOOP.

            CLEAR: LS_TKA01.
          ENDLOOP.

        ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
      ENDIF.
      SELECT *
      FROM ZFIWRT0004
      INTO TABLE TL_0004
       WHERE OPERACAO EQ P_OPERACAO.

      SELECT *
        FROM ZFIWRT0005
        INTO TABLE TL_0005
         WHERE OPERACAO EQ P_OPERACAO.

      SELECT *
        FROM ZFIWRT0006
        INTO TABLE TL_0006
         WHERE OPERACAO EQ P_OPERACAO.

      SELECT *
        FROM ZFIWRT0007
        INTO TABLE TL_0007
         WHERE OPERACAO EQ P_OPERACAO
           AND BRANCH   EQ P_BRANCH
           AND TIPO     EQ C_W.
**
      IF TL_0007[] IS NOT INITIAL.
        SELECT *
          FROM USER_ADDR
          INTO TABLE TL_USER
           FOR ALL ENTRIES IN TL_0007
            WHERE BNAME EQ TL_0007-USNAM.

      ENDIF.

      SELECT SINGLE *
        FROM ZFIWRT0019
        INTO WL_0019
     WHERE SEQ_LCTO EQ P_SEQ_LCTO.

      PERFORM PREENCHE_CAMPOS TABLES TL_0002
                                     TL_0003
                                     TL_0004
                                     TL_0005
                                     TL_0006
                                     TL_0007
                                     TL_1BAJ
                                     TL_1BAJT
                                     TL_TBSL
                                     TL_SKAT
                                     TL_CSKB
                                     TL_USER
                              USING  WL_0001
                                     WL_KNA1
                                     WL_LFA1
                                     WL_T001W
                                     WL_1BAA
                                     WL_0019.

      PERFORM Z_SUGERE_ITENS.
    ELSE.
      IF V_AUTOMATICO_MEMO = ABAP_FALSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cód. de operação nao existe!'.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ELSE.
    IF V_AUTOMATICO_MEMO = ABAP_FALSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cód. de operação nao existe!'.
      LEAVE TO SCREEN 100.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

  P_OPERACAO_OLD = P_OPERACAO.  "*-CS2023000043-09.02.2023-#102019-JT

*  ELSE.
*    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É preciso preencher todos os campos'
*                                           'obrigatorios!'.
*  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_0001  text
*      -->P_TL_0002  text
*      -->P_TL_0003  text
*      -->P_TL_0004  text
*      -->P_TL_0005  text
*      -->P_TL_0006  text
*      -->P_TL_1BAJ  text
*      -->P_TL_1BAJT  text
*----------------------------------------------------------------------*
FORM PREENCHE_CAMPOS  TABLES   TL_0002  STRUCTURE ZFIWRT0002
                               TL_0003  STRUCTURE ZFIWRT0003
                               TL_0004  STRUCTURE ZFIWRT0004
                               TL_0005  STRUCTURE ZFIWRT0005
                               TL_0006  STRUCTURE ZFIWRT0006
                               TL_0007  STRUCTURE ZFIWRT0007
                               TL_1BAJ  STRUCTURE J_1BAJ
                               TL_1BAJT STRUCTURE J_1BAJT
                               TL_TBSL  STRUCTURE TBSL
                               TL_SKAT  STRUCTURE SKAT
                               TL_CSKB  STRUCTURE CSKB
                               TL_USER  STRUCTURE USER_ADDR
                      USING    WL_0001 TYPE ZFIWRT0001
                               WL_KNA1 TYPE KNA1
                               WL_LFA1 TYPE LFA1
                               WL_T001W TYPE T001W
                               WL_1BAA TYPE J_1BAA
                               WL_0019 TYPE ZFIWRT0019.


  DATA: WL_INDCOPER         TYPE ZFIWRT0006-INDCOPER,
        WL_TEXTO_FISCAL(30),
        TL_VALUES           TYPE VRM_VALUES,
        WL_VALUES           TYPE LINE OF VRM_VALUES,
        WL_CONT             TYPE SY-TABIX,
        WL_CONT_AUX         TYPE SY-TABIX,
        WL_CONT_AUX2        TYPE SY-TABIX,
        WL_TAB_LIN          TYPE SY-TABIX.

  DATA: LV_PARID_KEY    TYPE LFA1-LIFNR.

** Preenche valores da tela.
  IF WL_0001-OPR_BLQ NE C_L.
    IF V_AUTOMATICO_MEMO = ABAP_FALSE.

*=#134309 - BUG DUMP - 09.04.2024 - JAIME - inicio ====================
*      IF v_automatico_memo = abap_false.
      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
*      ENDIF.
      DESCRIBE TABLE NODE_ITAB LINES WL_TAB_LIN.
      IF WL_TAB_LIN LT 2.
        REFRESH: NODE_ITAB.
        IF TREE IS NOT INITIAL.
*          IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
          CALL METHOD TREE->DELETE_ALL_NODES.
*           PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

          PERFORM PREENCHE_TREE USING C_ROOT
                                   SPACE
                                   C_X
                                   SPACE
                                   'Mensagens da Nota'
                                   SPACE.
*          ENDIF.
        ENDIF.
      ENDIF.
*=#134309 - BUG DUMP - 09.04.2024 - JAIME - fim    ====================

      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Operação bloqueada para uso!'.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
***Dados Gerais
****Header
    MOVE: WL_0001-NFTYPE        TO WG_FISCAL-NFTYPE,
          WL_0001-ITMTYP        TO WG_FISCAL-ITMTYP,
          WL_0001-PARVW         TO P_PARVW,
          WL_0001-DIAS          TO WG_FISCAL-DIAS,
          WL_0001-RETORNO       TO WG_FISCAL-RETORNO,
          WL_0001-ZPESAGEM      TO WG_FISCAL-ZPESAGEM,
          WL_0001-IMOBILIZADO   TO WG_FISCAL-IMOBILIZADO,
          WL_0001-TP_MV_IMOB    TO WG_FISCAL-TP_MV_IMOB,
          WL_0001-CTRL_ZRFL     TO WG_FISCAL-CTRL_ZRFL,
          WL_0001-ENERGIA       TO WG_FISCAL-ENERGIA,
          WL_0001-SERVICO       TO WG_FISCAL-SERVICO,
          WL_0001-DISP_NF_CCT   TO WG_FISCAL-DISP_NF_CCT,
          WL_0001-TRANSF_ICMS   TO WG_FISCAL-TRANSF_ICMS,
          WL_0001-COMPLEMENTO   TO WG_FISCAL-COMPLEMENTO,
          WL_0001-AVISO_REC     TO WG_FISCAL-AVISO_REC,
          WL_0001-LM_ESTOQUE    TO WG_FISCAL-LM_ESTOQUE.

    "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - inicio
    IF WG_FISCAL-IMOBILIZADO EQ 'S'.
*     STF - IR247430 – Ajustes transação ZNFW002 - 31/07/2025 - Inicio
*      WG_FISCAL-MOVE_PLANT = P_PARID+6(4).
      WG_FISCAL-MOVE_PLANT = P_BRANCH.
*     STF - IR247430 – Ajustes transação ZNFW002 - 31/07/2025 - Fim
    ENDIF.
    "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - fim


    PERFORM F_DEFINE_ORIGEM_DESTINO USING WL_KNA1
                                          WL_LFA1
                                          WL_T001W
                                          WL_1BAA
                                 CHANGING WL_INDCOPER
                                          WL_TEXTO_FISCAL.

**  Busca texto de tipo de operação
    CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
      EXPORTING
        I_TABLE_NAME = 'ZFIWRT0006'
        I_FIELD_NAME = 'OPERTYP'
      IMPORTING
        E_T_LIST     = TL_VALUES.

    READ TABLE TL_0006
      WITH KEY INDCOPER = WL_INDCOPER.

    MOVE: TL_0006-CFOP     TO WG_DIREITOS-CFOP,
*         tl_0006-taxlw1   TO wg_direitos-taxlw1,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw2   TO wg_direitos-taxlw2,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw4   TO wg_direitos-taxlw4,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw5   TO wg_direitos-taxlw5,  "*-CS2023000043-09.02.2023-#102019-JT
          TL_0006-TAXCODE  TO WG_DIREITOS-TAXCODE,
          TL_0006-INDCOPER TO WG_DIREITOS-INDCOPER,
          TL_0006-OPERTYP  TO WG_DIREITOS-OPERTYP.

*-CS2023000043-09.02.2023-#102019-JT-inicio
*    US #163043 - MMSILVA - 21.05.2025 - Comentado devido não atualizar quando já preenchido - Inicio
*    IF WG_DIREITOS-TAXLW1 IS INITIAL.
    MOVE TL_0006-TAXLW1  TO WG_DIREITOS-TAXLW1.
*    ENDIF.
*    US #163043 - MMSILVA - 21.05.2025 - Comentado devido não atualizar quando já preenchido - Fim
    IF WG_DIREITOS-TAXLW2 IS INITIAL.
      MOVE TL_0006-TAXLW2  TO WG_DIREITOS-TAXLW2.
    ENDIF.
    IF WG_DIREITOS-TAXLW4 IS INITIAL.
      MOVE TL_0006-TAXLW4  TO WG_DIREITOS-TAXLW4.
    ENDIF.
    IF WG_DIREITOS-TAXLW5 IS INITIAL.
      MOVE TL_0006-TAXLW5  TO WG_DIREITOS-TAXLW5.
    ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

    READ TABLE TL_VALUES INTO WL_VALUES
      WITH KEY KEY = TL_0006-OPERTYP.

    CONCATENATE WL_TEXTO_FISCAL '-' WL_VALUES-TEXT INTO WG_OP_FISCAL SEPARATED BY SPACE.
***      descricao da operacao.
    REFRESH: TG_EDITOR.
    CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
    WL_CONT = STRLEN( WL_0001-TXT_COMPL ).
    WL_CONT_AUX = WL_CONT / 72.

    DO.
      MOVE: WL_0001-TXT_COMPL+WL_CONT_AUX2 TO WG_EDITOR-LINE.
      ADD 72 TO WL_CONT_AUX2.
      APPEND WG_EDITOR TO TG_EDITOR.

      IF WL_CONT_AUX2 GT WL_CONT.
        EXIT.

      ENDIF.
    ENDDO.

    IF V_AUTOMATICO_MEMO = ABAP_FALSE.
      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
    ENDIF.
***Impostos
    IF WG_DOCS-DOCNUM IS INITIAL.
      REFRESH: TG_IMPO.
      LOOP AT TL_0002.
        READ TABLE TL_1BAJ
          WITH KEY TAXTYP = TL_0002-TAXTYP.

        READ TABLE TL_1BAJT
          WITH KEY TAXTYP = TL_0002-TAXTYP.

        MOVE: TL_0002-TAXTYP   TO TG_IMPO-TAXTYP,
              TL_1BAJT-TTYPETXT TO TG_IMPO-TTYPETXT,
              TL_1BAJ-TAXGRP  TO TG_IMPO-TAXGRP.

        IF TL_0002-TAXTYP EQ C_ICM3.
          IF WG_DIREITOS-OPERTYP EQ C_T.

          ELSEIF WG_DIREITOS-OPERTYP EQ C_I.

          ELSEIF WG_DIREITOS-OPERTYP EQ C_N.

          ENDIF.
        ELSE.

        ENDIF.

        APPEND TG_IMPO.
        CLEAR: TG_IMPO.
      ENDLOOP.
    ENDIF.

***Contabilidade
    IF WG_DOCS-BELNR IS INITIAL.
      REFRESH: TG_CONTAB, TG_CONTAB-STYLE2, STYLE2.
      LOOP AT TL_0003.
*        where estorno is initial.
        READ TABLE TL_SKAT
          WITH KEY SAKNR = TL_0003-HKONT.

        READ TABLE TL_TBSL
          WITH KEY BSCHL = TL_0003-BSCHL.

        READ TABLE TL_CSKB
           WITH KEY KSTAR = TL_0003-HKONT.

        IF SY-SUBRC IS NOT INITIAL.
*        and tl_0003-estorno is not initial.
          WA_STYLE-FIELDNAME = 'KOSTL'.
          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          INSERT  WA_STYLE INTO TABLE STYLE2.
        ELSE.
          IF TL_0003-ESTORNO IS NOT INITIAL.
            WA_STYLE-FIELDNAME = 'KOSTL'.
            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            INSERT  WA_STYLE INTO TABLE STYLE2.
          ENDIF.
        ENDIF.

        IF TL_TBSL-KOART EQ C_K
        OR TL_TBSL-KOART EQ C_D.

          SELECT SINGLE *
            FROM J_1BAD INTO @DATA(LWA_J_1BAD)
           WHERE PARVW EQ @P_PARVW.

          IF SY-SUBRC EQ 0.

            LV_PARID_KEY = P_PARID.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = LV_PARID_KEY
              IMPORTING
                OUTPUT = LV_PARID_KEY.

            CASE LWA_J_1BAD-PARTYP.
              WHEN 'C'.

                SELECT SINGLE *
                  FROM KNA1 INTO @DATA(_WL_KNA1)
                 WHERE KUNNR = @LV_PARID_KEY.

                IF SY-SUBRC EQ 0.
                  IF TL_TBSL-KOART = C_D.
                    MOVE P_PARID        TO TG_CONTAB-HKONT.
                  ELSEIF TL_TBSL-KOART = C_K.
                    MOVE _WL_KNA1-LIFNR TO TG_CONTAB-HKONT.
                  ENDIF.
                ENDIF.

              WHEN 'B' OR 'V'.

                SELECT SINGLE *
                  FROM LFA1 INTO @DATA(_WL_LFA1)
                 WHERE LIFNR = @LV_PARID_KEY.

                IF SY-SUBRC EQ 0.
                  IF TL_TBSL-KOART = C_K.
                    MOVE P_PARID        TO TG_CONTAB-HKONT.
                  ELSEIF TL_TBSL-KOART = C_D.
                    MOVE _WL_LFA1-KUNNR TO TG_CONTAB-HKONT.
                  ENDIF.
                ENDIF.

            ENDCASE.


            IF TL_0003-ESTORNO IS INITIAL.
*              wa_style-fieldname = 'ZFBDT'.
*              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*              insert  wa_style into table style2.
*
*              wa_style-fieldname = 'ZLSCH'.
*              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*              insert  wa_style into table style2.
*              insert lines of style2 into table tg_contab-style2.
            ELSE.
              WA_STYLE-FIELDNAME = 'ZFBDT'.
              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              INSERT  WA_STYLE INTO TABLE STYLE2.

              WA_STYLE-FIELDNAME = 'ZLSCH'.
              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              INSERT  WA_STYLE INTO TABLE STYLE2.
            ENDIF.

          ENDIF.



        ELSE.
          MOVE: TL_0003-HKONT   TO TG_CONTAB-HKONT.

          WA_STYLE-FIELDNAME = 'ZFBDT'.
          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          INSERT  WA_STYLE INTO TABLE STYLE2.

          WA_STYLE-FIELDNAME = 'ZLSCH'.
          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          INSERT  WA_STYLE INTO TABLE STYLE2.
        ENDIF.
        MOVE: TL_0003-BSCHL   TO TG_CONTAB-BSCHL,
*            tl_0003-hkont   TO tg_contab-hkont,
              TL_SKAT-TXT50   TO TG_CONTAB-TXT50,
              TL_0003-TAXTYP  TO TG_CONTAB-TAXTYP,
              TL_0003-ESTORNO TO TG_CONTAB-ESTORNO,
              TL_0003-NEWBW   TO TG_CONTAB-NEWBW,
              TL_0003-UMSKZ   TO TG_CONTAB-UMSKZ.

        INSERT LINES OF STYLE2 INTO TABLE TG_CONTAB-STYLE2.
        APPEND TG_CONTAB.
        CLEAR: TG_CONTAB.
        REFRESH: STYLE2.
      ENDLOOP.

      TG_TBSL[] = TL_TBSL[].
    ENDIF.

***Movimentacao de estoque
    IF WG_DOCS-MBLNR IS INITIAL.
      REFRESH: TG_MOVEST.
      LOOP AT TL_0004.
        MOVE: TL_0004-BWART  TO TG_MOVEST-BWART,
              TL_0004-TCODE  TO TG_MOVEST-TCODE,
              TL_0004-MWSKZ1 TO TG_MOVEST-MWSKZ1,
              TL_0004-ESTORNO TO TG_MOVEST-ESTORNO.

*            tl_0004-lgort  TO tg_movest-lgort.

        APPEND TG_MOVEST.
        CLEAR: TG_MOVEST.
      ENDLOOP.
    ENDIF.

*=#134309 - BUG DUMP - 09.04.2024 - JAIME - inicio ====================
*    IF wg_docs-docnum IS INITIAL
*    AND wg_docs-belnr IS INITIAL
*    AND wg_docs-mblnr IS INITIAL
*    AND wg_docs-nfenum IS INITIAL.
*=#134309 - BUG DUMP - 09.04.2024 - JAIME - fim    ====================
    DESCRIBE TABLE NODE_ITAB LINES WL_TAB_LIN.
    IF WL_TAB_LIN LT 2.
      REFRESH: NODE_ITAB.
      IF TREE IS NOT INITIAL.
        IF V_AUTOMATICO_MEMO = ABAP_FALSE.
          CALL METHOD TREE->DELETE_ALL_NODES.
*           PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

          PERFORM PREENCHE_TREE USING C_ROOT
                                   SPACE
                                   C_X
                                   SPACE
                                   'Mensagens da Nota'
                                   SPACE.
        ENDIF.
        SORT TL_0005 BY SEQNUM LINNUM.
        LOOP AT TL_0005.
          ON CHANGE OF TL_0005-SEQNUM.
            IF V_AUTOMATICO_MEMO = ABAP_FALSE.
              PERFORM PREENCHE_TREE USING TL_0005-SEQNUM
                                          C_ROOT
                                          SPACE
                                          CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                          TL_0005-MESSAGE
                                          HANDLE_TREE.
            ENDIF.
          ENDON.
          TG_MENSAGEMS-SEQNUM  = TL_0005-SEQNUM.
          TG_MENSAGEMS-LINNUM  = TL_0005-LINNUM.
          TG_MENSAGEMS-MESSAGE = TL_0005-MESSAGE.
          APPEND TG_MENSAGEMS.         " msgs q foram parametrizadas na operacao
          APPEND TG_MENSAGEMS TO TG_MENSAGEMS_AUX.  " tabela q iram conter todas as msgs
          " inclusive as adcionadas na criacao da NF
        ENDLOOP.

        IF TG_MENSAGEMS_LIM IS NOT INITIAL.
          APPEND LINES OF TG_MENSAGEMS_LIM[] TO TG_MENSAGEMS_AUX[].
        ENDIF.
      ENDIF.
*    CALL METHOD tree->add_nodes
*      EXPORTING
*        node_table           = node_itab
*        table_structure_name = 'NODE_STR'.



*
*      PERFORM preenche_tree USING wl_lines
*                                  'Root'
*                                  space
*                                  cl_gui_simple_tree=>relat_last_child
*                                  space
*                                  handle_tree.
*
*
*      CALL METHOD tree->add_nodes
*        EXPORTING
*          node_table           = node_itab
*          table_structure_name = 'NODE_STR'.
    ENDIF.
    REFRESH: TG_APROV.
    LOOP AT TL_0007.
      READ TABLE TL_USER
       WITH KEY BNAME = TL_0007-USNAM.

      MOVE: TL_0007-NIVEL_APROV  TO TG_APROV-NIVEL_APROV,
            TL_0007-USNAM        TO TG_APROV-USNAM,
            TL_0007-DEPARTAMENTO TO TG_APROV-DEPARTAMENTO.

      CONCATENATE TL_USER-NAME_FIRST TL_USER-NAME_LAST INTO TG_APROV-NOME SEPARATED BY SPACE.

      APPEND TG_APROV.
      CLEAR: TG_APROV.
    ENDLOOP.

** Parceiros da Nota Fiscal
    REFRESH: TG_PARC, TG_PARC-STYLE, STYLE.
    IF P_PARVW IS NOT INITIAL
    AND P_PARID IS NOT INITIAL.
      TG_PARC-PARVW = P_PARVW.
      TG_PARC-PARID = P_PARID.
      IF TG_PARC-PARVW EQ C_AG.
        TG_PARC-NOME  = WL_KNA1-NAME1.    "wg_desc_parid.

      ELSEIF TG_PARC-PARVW EQ C_LF
          OR TG_PARC-PARVW EQ C_BR.
        TG_PARC-NOME  = WL_LFA1-NAME1.  "wg_desc_parid.

      ENDIF.
      WA_STYLE-FIELDNAME = 'PARVW'.
      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT  WA_STYLE INTO TABLE STYLE .
      WA_STYLE-FIELDNAME = 'PARID'.
      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          APPEND WA_STYLE TO STYLE.
      INSERT  WA_STYLE INTO TABLE STYLE .
      INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.
    ENDIF.
    APPEND TG_PARC.
    REFRESH: STYLE.
*    ENDIF.  "*=#134309 - BUG DUMP - 09.04.2024 - JAIME - ====================

  ENDIF.

  "Transporte
  IF NOT ( WL_0019 IS INITIAL ).
    MOVE: WL_0019-LIFNR  TO WG_TRANSPORTE-LIFNR,
          WL_0019-PLACA  TO WG_TRANSPORTE-PLACA,
          WL_0019-ANZPK  TO WG_TRANSPORTE-ANZPK,
          WL_0019-SHPUNT TO WG_TRANSPORTE-SHPUNT,
          WL_0019-NTGEW  TO WG_TRANSPORTE-NTGEW,
          WL_0019-BRGEW  TO WG_TRANSPORTE-BRGEW,
          WL_0019-UFPLACA TO WG_TRANSPORTE-UFPLACA,

          WL_0019-PLACA_CAR1 TO WG_TRANSPORTE-PLACA_CAR1,
          WL_0019-PLACA_CAR2 TO WG_TRANSPORTE-PLACA_CAR2,
          WL_0019-PLACA_CAR3 TO WG_TRANSPORTE-PLACA_CAR3,
          WL_0019-MOTORISTA  TO WG_TRANSPORTE-MOTORISTA.
  ENDIF.


ENDFORM.                    " PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  VALIDA_PARAMETROS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_PARAMETROS INPUT.
  REFRESH: TG_ITENS.
  MOVE: C_SEARCH TO OK-CODE.

ENDMODULE.                 " VALIDA_PARAMETROS  OUTPUT
MODULE VALIDA_PARAMETROS2 INPUT.
  REFRESH: TG_ITENS.

ENDMODULE.                 " VALIDA_PARAMETROS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_OPER INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.
  DATA: BEGIN OF T_FIELDTAB OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF T_FIELDTAB.

  DATA: BEGIN OF TL_OPERACAO OCCURS 0,
          OPERACAO  TYPE ZFIWRT0001-OPERACAO,
          DESCRICAO TYPE ZFIWRT0001-DESCRICAO,
        END OF TL_OPERACAO.

  REFRESH: TL_OPERACAO, T_FIELDTAB.
  CLEAR:   TL_OPERACAO, T_FIELDTAB.

  SELECT OPERACAO DESCRICAO
    FROM ZFIWRT0001
    INTO TABLE TL_OPERACAO
     WHERE OPR_BLQ EQ C_L.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'OPERACAO'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'P_OPERACAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_OPERACAO
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_PARID INPUT.
  DATA: BEGIN OF DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF DYNPFIELDS.

* Parameter für F4IF_FIELD_VALUE_REQUEST
  DATA: MC_OBJ LIKE HELP_INFO-MCOBJ.
  DATA: RETURN_VALUES LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE.
  DATA: DA_DISPLAY TYPE C.

  REFRESH:DYNPFIELDS.

  MOVE 'P_PARVW'  TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.
  MOVE 'P_BUKRS'  TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.
  MOVE 'P_PARID'  TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME                   = SY-REPID
      DYNUMB                   = SY-DYNNR
      PERFORM_INPUT_CONVERSION = 'X'
    TABLES
      DYNPFIELDS               = DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA     = 1
      INVALID_DYNPROFIELD      = 2
      INVALID_DYNPRONAME       = 3
      INVALID_DYNPRONUMMER     = 4
      INVALID_REQUEST          = 5
      NO_FIELDDESCRIPTION      = 6
      INVALID_PARAMETER        = 7
      UNDEFIND_ERROR           = 8
      OTHERS                   = 9.

  READ TABLE DYNPFIELDS
      WITH KEY FIELDNAME = 'P_PARVW'.

  IF P_PARVW EQ C_AG
  OR DYNPFIELDS-FIELDVALUE EQ C_AG.

    READ TABLE DYNPFIELDS
       WITH KEY FIELDNAME = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    MC_OBJ = 'C_KUNNR'.
    IF DYNPFIELDS-FIELDINP EQ SPACE.
      DA_DISPLAY = C_X. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        TABNAME           = SPACE
        FIELDNAME         = SPACE
        SEARCHHELP        = MC_OBJ
        DYNPROFIELD       = 'X'
        VALUE             = DYNPFIELDS-FIELDVALUE
        DISPLAY           = DA_DISPLAY
      TABLES
        RETURN_TAB        = RETURN_VALUES
      EXCEPTIONS
        FIELD_NOT_FOUND   = 1
        NO_HELP_FOR_FIELD = 2
        INCONSISTENT_HELP = 3
        NO_VALUES_FOUND   = 4
        OTHERS            = 5.
    IF SY-SUBRC = 0 AND
       DYNPFIELDS-FIELDINP NE SPACE.
      P_PARID = RETURN_VALUES-FIELDVAL.
    ENDIF.

  ELSEIF P_PARVW EQ C_BR
    OR   P_PARVW EQ C_LF
    OR DYNPFIELDS-FIELDVALUE EQ C_BR
    OR DYNPFIELDS-FIELDVALUE EQ C_LF.

    READ TABLE DYNPFIELDS
       WITH KEY FIELDNAME = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    MC_OBJ = 'KRED_C'.
    IF DYNPFIELDS-FIELDINP EQ SPACE.
      DA_DISPLAY = C_X. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        TABNAME           = SPACE
        FIELDNAME         = SPACE
        SEARCHHELP        = MC_OBJ
        DYNPROFIELD       = 'X'
        VALUE             = DYNPFIELDS-FIELDVALUE
        DISPLAY           = DA_DISPLAY
      TABLES
        RETURN_TAB        = RETURN_VALUES
      EXCEPTIONS
        FIELD_NOT_FOUND   = 1
        NO_HELP_FOR_FIELD = 2
        INCONSISTENT_HELP = 3
        NO_VALUES_FOUND   = 4
        OTHERS            = 5.
    IF SY-SUBRC = 0 AND
       DYNPFIELDS-FIELDINP NE SPACE.
      P_PARID = RETURN_VALUES-FIELDVAL.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DESCRICOES .

*-CS2023000043-09.02.2023-#102019-JT-inicio
  CLEAR: WG_DESC_TAXLW1,
         WG_DESC_TAXLW2,
         WG_DESC_TAXLW4,
         WG_DESC_TAXLW5.
*-CS2023000043-09.02.2023-#102019-JT-fim

  IF WG_FISCAL-NFTYPE IS NOT INITIAL.
    SELECT SINGLE NFTTXT
      FROM J_1BAAT
      INTO WG_DESC_NFTYPE
       WHERE SPRAS  EQ SY-LANGU
         AND NFTYPE EQ WG_FISCAL-NFTYPE.

  ENDIF.

  IF WG_FISCAL-ITMTYP IS NOT INITIAL.
    SELECT SINGLE TEXT
      FROM J_1BITEMTYPEST
      INTO WG_DESC_ITMTYP
       WHERE SPRAS  EQ SY-LANGU
         AND ITMTYP EQ WG_FISCAL-ITMTYP.

  ENDIF.

  IF WG_DIREITOS-CFOP IS NOT INITIAL.
    SELECT SINGLE CFOTXT
     FROM J_1BAGNT
     INTO WG_DESC_CFOP
      WHERE SPRAS  EQ SY-LANGU
        AND CFOP EQ WG_DIREITOS-CFOP.
  ENDIF.

  IF WG_DIREITOS-TAXLW1 IS NOT INITIAL.
    SELECT SINGLE DESCRIP
     FROM J_1BATL1T
     INTO WG_DESC_TAXLW1
      WHERE LANGU  EQ SY-LANGU
        AND TAXLAW EQ WG_DIREITOS-TAXLW1.
  ENDIF.

  IF WG_DIREITOS-TAXLW2 IS NOT INITIAL.
    SELECT SINGLE DESCRIP
     FROM J_1BATL2T
     INTO WG_DESC_TAXLW2
      WHERE LANGU  EQ SY-LANGU
        AND TAXLAW EQ WG_DIREITOS-TAXLW2.
  ENDIF.

  IF WG_DIREITOS-TAXLW4 IS NOT INITIAL.
    SELECT SINGLE DESCRIP
     FROM J_1BATL4T
     INTO WG_DESC_TAXLW4
      WHERE LANGU  EQ SY-LANGU
        AND TAXLAW EQ WG_DIREITOS-TAXLW4.
  ENDIF.

  IF WG_DIREITOS-TAXLW5 IS NOT INITIAL.
    SELECT SINGLE DESCRIP
     FROM J_1BATL5T
     INTO WG_DESC_TAXLW5
      WHERE LANGU  EQ SY-LANGU
        AND TAXLAW EQ WG_DIREITOS-TAXLW5.
  ENDIF.

  IF WG_DIREITOS-TAXCODE IS NOT INITIAL.
    SELECT SINGLE TXT
     FROM J_1BTXSDCT
     INTO WG_DESC_TAXCODE
      WHERE LANGU   EQ SY-LANGU
        AND TAXCODE EQ  WG_DIREITOS-TAXCODE.
  ENDIF.
ENDFORM.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Module  VALIDA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_MATERIAL INPUT.
  DATA: WL_MARA TYPE MARA,
        WL_MAKT TYPE MAKT.
  IF TG_ITENS-MATNR IS NOT INITIAL.
    SELECT SINGLE *
      FROM MARA
      INTO WL_MARA
       WHERE MATNR EQ TG_ITENS-MATNR.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE MAKTX
        FROM MAKT
        INTO TG_ITENS-MAKTX
         WHERE SPRAS EQ SY-LANGU
           AND MATNR EQ TG_ITENS-MATNR.

    ELSE.
      MESSAGE E836(SD) WITH 'Número do material não existe!'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " VALIDA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_CENTRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_CENTRO INPUT.
  DATA: WL_T001W TYPE T001W.

  IF TG_ITENS-WERKS IS NOT INITIAL.
    SELECT SINGLE *
      FROM T001W
      INTO WL_T001W
       WHERE WERKS EQ TG_ITENS-WERKS.

    IF SY-SUBRC IS INITIAL.
*      select *
    ELSE.
      MESSAGE E836(SD) WITH 'Centro/Filial não encontrado.'.
    ENDIF.
  ELSE.
  ENDIF.
ENDMODULE.                 " VALIDA_CENTRO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_SUBCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CALL_SUBCREEN INPUT.
  CASE SY-UCOMM.
    WHEN C_DG1.
      IF VG_SUBSCREEN1 EQ C_DUMMY_HEADER.
        VG_SUBSCREEN1 = '213'.
        WG_DG1 = C_MINIMIZAR.
      ELSE.
        VG_SUBSCREEN1 = C_DUMMY_HEADER.
        WG_DG1 = C_MAXIMIZAR.
      ENDIF.
    WHEN C_DG2.
      IF VG_SUBSCREEN2 EQ C_DUMMY_ITENS.
        VG_SUBSCREEN2 = '214'.
        WG_DG2 = C_MINIMIZAR.
      ELSE.
        VG_SUBSCREEN2 = C_DUMMY_ITENS.
        WG_DG2 = C_MAXIMIZAR.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " CALL_SUBCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.
  DATA: WAREF      TYPE REF TO DATA.

  DATA: WA_OBJ  TYPE BORIDENT,
        IP_MODE TYPE SGS_RWMOD.

  "workflow documentos
  IF MANAGER IS NOT INITIAL.
    CALL METHOD MANAGER->UNPUBLISH.
    CLEAR: MANAGER.
  ENDIF.


  IF IT_SEQ_LCTO[] IS NOT INITIAL.
    READ TABLE IT_SEQ_LCTO INDEX 1.
    WA_OBJ-OBJTYPE = 'ZWRR0002'.
    CONCATENATE SY-MANDT IT_SEQ_LCTO-SEQ_LCTO INTO WA_OBJ-OBJKEY.

    IF GF_AUTHORIZATION_FT_09 EQ ABAP_TRUE.
      IP_MODE = 'E'.
    ELSE.
      IP_MODE = 'D'.
    ENDIF.

    CREATE OBJECT MANAGER
      EXPORTING
        IS_OBJECT        = WA_OBJ
        IP_NO_COMMIT     = 'R'
        IP_MODE          = IP_MODE
      EXCEPTIONS
        OBJECT_INVALID   = 1
        CALLBACK_INVALID = 2
        OTHERS           = 3.

    SET TITLEBAR 'TLWORK' WITH P_SEQ_LCTO.

  ELSE.
*    IF SY-TCODE EQ C_DISPLAY.
*      SET PF-STATUS 'Z002' EXCLUDING FCODE.
*    ELSE.
*      SET PF-STATUS 'Z001' EXCLUDING FCODE.
*    ENDIF.
  ENDIF.

  IF G_CUSTOM_CONTAINER IS INITIAL.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA      = C_X.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
*    WA_LAYOUT-COL_OPT    = C_X.
    WA_STABLE-ROW        = C_X.

    WA_LAYOUT-STYLEFNAME = 'STYLE2'.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER_1.

    PERFORM MONTAR_LAYOUT.
*-CS2020001331 - 06.10.2021 - JT - inicio
    PERFORM F_DROPDOWN_TABLE.
*-CS2020001331 - 06.10.2021 - JT - fim

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.
    SET HANDLER LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

*BUG 75507 - CBRAND - Inicio
    GRID1->ACTIVATE_DISPLAY_PROTOCOL( SPACE ).
*BUG 75507 - CBRAND - Fim

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ITENS[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_HOTSPOT_CLICK FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.



  ELSE.
    WA_LAYOUT-STYLEFNAME = 'STYLE2'.
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG[].
    PERFORM MONTAR_LAYOUT.
    IF WG_DOCS-DOCNUM IS INITIAL.
      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
        WITH KEY GROUP1 = 'GR1'.
      IF SY-SUBRC IS INITIAL.
        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
          WHERE FIELDNAME EQ 'MATNR'
             OR FIELDNAME EQ 'CHARG'
             OR FIELDNAME EQ 'WERKS'
             OR FIELDNAME EQ 'LGORT'
             OR FIELDNAME EQ 'NETPR'
             OR FIELDNAME EQ 'MENGE'
             OR FIELDNAME EQ 'MEINS'
             OR FIELDNAME EQ 'ANLN1'
             OR FIELDNAME EQ 'ANLN2'
             OR FIELDNAME EQ 'VBELN'
             OR FIELDNAME EQ 'POSNR'
             OR FIELDNAME EQ 'MEINS'
             OR FIELDNAME EQ 'RENAS'
             OR FIELDNAME EQ 'POSSUI_ICMS_ST'
             OR FIELDNAME EQ 'PROD_DESCRICAO'. "US #163043 - MMSILVA - 18.03.2025

          IF WG_FISCAL-RETORNO EQ 'S'.
            IF P_PARVW EQ C_AG.
              IF  W_FIELDCATALOG-FIELDNAME EQ 'MENGE'.
                W_FIELDCATALOG-EDIT = C_X.
              ELSE.
                W_FIELDCATALOG-EDIT = SPACE.
              ENDIF.
            ELSEIF P_PARVW EQ C_BR.
*            IF  w_fieldcatalog-fieldname EQ 'MENGE'.
*              w_fieldcatalog-edit = c_x.
*            ELSE.
              W_FIELDCATALOG-EDIT = SPACE.
*            ENDIF.
            ENDIF.
          ELSE .
            W_FIELDCATALOG-EDIT = C_X.
          ENDIF.


          IF W_FIELDCATALOG-FIELDNAME EQ 'ANLN1'
           OR W_FIELDCATALOG-FIELDNAME EQ 'ANLN2'.
            IF WG_FISCAL-IMOBILIZADO EQ C_S.
              W_FIELDCATALOG-EMPHASIZE = C_X.
            ELSE.
              W_FIELDCATALOG-EMPHASIZE = SPACE.
            ENDIF.
          ENDIF.
          SELECT SINGLE *
              FROM ZFIWRT0001
              INTO @DATA(WL_0001_)
               WHERE OPERACAO EQ @P_OPERACAO.

          IF WL_0001_-GE_REMESSA = 'S'.
            IF W_FIELDCATALOG-FIELDNAME EQ 'MATNR'
               OR W_FIELDCATALOG-FIELDNAME EQ 'CHARG'
               OR W_FIELDCATALOG-FIELDNAME EQ 'WERKS'
               OR W_FIELDCATALOG-FIELDNAME EQ 'LGORT'.
              W_FIELDCATALOG-EDIT = SPACE.
            ENDIF.
          ENDIF.
          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
        ENDLOOP.
      ELSE.
        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
         WHERE FIELDNAME EQ 'MATNR'
            OR FIELDNAME EQ 'CHARG'
            OR FIELDNAME EQ 'WERKS'
            OR FIELDNAME EQ 'LGORT'
            OR FIELDNAME EQ 'NETPR'
            OR FIELDNAME EQ 'MENGE'
            OR FIELDNAME EQ 'MEINS'
            OR FIELDNAME EQ 'ANLN1'
            OR FIELDNAME EQ 'ANLN2'
            OR FIELDNAME EQ 'VBELN'
            OR FIELDNAME EQ 'POSNR'
            OR FIELDNAME EQ 'RENAS'
            OR FIELDNAME EQ 'POSSUI_ICMS_ST'
            OR FIELDNAME EQ 'PROD_DESCRICAO'. "US #163043 - MMSILVA - 18.03.2025
          W_FIELDCATALOG-EDIT = SPACE.
          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    " ALRS
    IF GRID2 IS NOT INITIAL AND  WG_FISCAL-COMPLEMENTO = 'S'.
      CALL METHOD GRID2->GET_FRONTEND_FIELDCATALOG
        IMPORTING
          ET_FIELDCATALOG = T_FIELDCATALOG[].

      IF WG_DOCS-DOCNUM IS INITIAL.
        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
            WHERE FIELDNAME EQ 'TAXTYP'
               OR FIELDNAME EQ 'TTYPETXT'
               OR FIELDNAME EQ 'TAXGRP'.
          W_FIELDCATALOG-EDIT = SPACE.
          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
        ENDLOOP.
        READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
          WITH KEY GROUP1 = 'GR1'.
        IF SY-SUBRC IS INITIAL.
          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
            WHERE FIELDNAME EQ 'BASE'
               OR FIELDNAME EQ 'RATE'
               OR FIELDNAME EQ 'EXCBAS'
               OR FIELDNAME EQ 'TAXVAL'
               OR FIELDNAME EQ 'OTHBAS'.
            W_FIELDCATALOG-EDIT = C_X.
            MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
          ENDLOOP.
        ELSE.
          LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
            WHERE FIELDNAME EQ 'BASE'
               OR FIELDNAME EQ 'RATE'
               OR FIELDNAME EQ 'EXCBAS'
               OR FIELDNAME EQ 'TAXVAL'
               OR FIELDNAME EQ 'OTHBAS'.

            W_FIELDCATALOG-EDIT = SPACE.
            MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
          ENDLOOP.
        ENDIF.
      ENDIF.
      CALL METHOD GRID2->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = T_FIELDCATALOG[].

      CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.
  ENDIF.
  CLEAR WA_LAYOUT-STYLEFNAME.

*  ELSEIF sy-dynnr EQ 212.
  IF G_CUSTOM_CONT_DESC IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESC
      EXPORTING
        CONTAINER_NAME = G_DESCBOX.

    IF G_CUSTOM_CONT_DESC IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOX
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESC
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 255.

      CALL METHOD OBG_DESCBOX->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.
*  ELSEIF sy-dynnr EQ 241.
  IF OBG_CONTEINER_CONTAB IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_CONTAB
      EXPORTING
        CONTAINER_NAME = G_CC_CONTAB.

    CREATE OBJECT GRID3
      EXPORTING
        I_PARENT = OBG_CONTEINER_CONTAB.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
*    wa_layout-cwidth_opt = c_x.
    WA_LAYOUT-NO_TOOLBAR = SPACE.
*     WA_LAYOUT-COL_OPT    = C_X.
    WA_LAYOUT-STYLEFNAME = 'STYLE2'.
    WA_LAYOUT-GRID_TITLE = 'Contabilização'.
    PERFORM MONTAR_LAYOUT_CONTAB.

    WL_FILTER-FIELDNAME = 'DMBTR'."c_dmbtr.
    WL_FILTER-SIGN      = 'I'. "c_i.
    WL_FILTER-OPTION    = 'NE'. "c_ne.
    WL_FILTER-LOW       = '0'.

    APPEND WL_FILTER TO TL_FILTER.

    WL_FILTER-FIELDNAME = 'ESTORNO'."c_dmbtr.
    WL_FILTER-SIGN      = 'I'. "c_i.
    WL_FILTER-OPTION    = 'EQ'. "c_ne.
    WL_FILTER-LOW       = SPACE.

    APPEND WL_FILTER TO TL_FILTER.

    CALL METHOD GRID3->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_CONTAB[].

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID3->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED3 FOR GRID3.

  ELSE.
    CALL METHOD GRID3->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG.

    READ TABLE TG_FIELDS
      WITH KEY GROUP1 = 'GR1'.
    IF SY-SUBRC IS INITIAL.
*      MOVE: c_x TO wa_layout-edit.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
         WHERE FIELDNAME EQ 'ZFBDT'
            OR FIELDNAME EQ 'ZLSCH'
            OR FIELDNAME EQ 'KOSTL'
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
            OR FIELDNAME EQ 'HBKID'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

        W_FIELDCATALOG-EDIT = C_X.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
*
    ELSE.
*      MOVE : space TO wa_layout-edit.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
         WHERE FIELDNAME EQ 'ZFBDT'
            OR FIELDNAME EQ 'ZLSCH'
            OR FIELDNAME EQ 'KOSTL'
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
            OR FIELDNAME EQ 'HBKID'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610


        W_FIELDCATALOG-EDIT = SPACE.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
    ENDIF.

    CALL METHOD GRID3->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.

    CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
*  ENDIF.

** MENSAGENS DA NF
  IF CONTAINER IS INITIAL.
    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CC_MSG_NF'.
    CREATE OBJECT SPLITTER_MSG
      EXPORTING
        PARENT      = CONTAINER
        ORIENTATION = 1.
    LEFT = SPLITTER_MSG->TOP_LEFT_CONTAINER.
    RIGHT = SPLITTER_MSG->BOTTOM_RIGHT_CONTAINER.
    CREATE OBJECT EDITOR
      EXPORTING
        PARENT            = RIGHT
        WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION = 72
        MAX_NUMBER_CHARS  = 255.
    CALL METHOD EDITOR->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = 1.

    CREATE OBJECT TREE
      EXPORTING
        PARENT              = LEFT
        NODE_SELECTION_MODE = TREE->NODE_SEL_MODE_SINGLE.

** Definition of drag drop behaviour
*    CREATE OBJECT behaviour_left.
*    CALL METHOD behaviour_left->add
*        EXPORTING
*              flavor = 'Tree_move_to_Edit'
*              dragsrc = 'X'
*              droptarget = ' '
*              effect = cl_dragdrop=>copy.
*   CALL METHOD behaviour_left->add
*        EXPORTING
*              flavor = 'Tree_copy_to_Edit'
*              dragsrc = 'X'
*              droptarget = ' '
*              effect = cl_dragdrop=>copy.
*
*    CALL METHOD behaviour_left->get_handle
*         IMPORTING handle = handle_tree.
*
** Drag Drop behaviour of tree control nodes are defined in the node
** structure
*    PERFORM fill_tree.
    PERFORM PREENCHE_TREE USING C_ROOT
                                 SPACE
                                 C_X
                                 SPACE
                                 'Mensagens da Nota'
                                 SPACE.
    CALL METHOD TREE->ADD_NODES
      EXPORTING
        NODE_TABLE           = NODE_ITAB
        TABLE_STRUCTURE_NAME = 'NODE_STR'.

    EVENT-EVENTID = CL_GUI_SIMPLE_TREE=>EVENTID_NODE_DOUBLE_CLICK.
    EVENT-APPL_EVENT = 'X'.
    APPEND EVENT TO EVENTS.

    CALL METHOD TREE->SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = EVENTS.

* registration of drag and drop events
    DATA DRAGDROP TYPE REF TO LCL_DRAGDROP_RECEIVER.
    CREATE OBJECT DRAGDROP.
    SET HANDLER DRAGDROP->NODE_DOUBLE_CLICK FOR TREE.


    CALL METHOD TREE->EXPAND_NODE
      EXPORTING
        NODE_KEY = 'ROOT'. "c_Root.

  ELSE.
    IF NODE_ITAB[] IS NOT INITIAL.
*      IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
      CALL METHOD TREE->DELETE_ALL_NODES.
      CALL METHOD TREE->ADD_NODES
        EXPORTING
          NODE_TABLE           = NODE_ITAB
          TABLE_STRUCTURE_NAME = 'NODE_STR'.

      CALL METHOD TREE->EXPAND_NODE
        EXPORTING
          NODE_KEY = 'ROOT'. "c_Root.
*      ENDIF.
    ENDIF.
  ENDIF.

  IF OBG_CONTEINER_APROV IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_APROV
      EXPORTING
        CONTAINER_NAME = G_CC_APROV.

    CREATE OBJECT GRID4
      EXPORTING
        I_PARENT = OBG_CONTEINER_APROV.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.

*    wa_layout-cwidth_opt = c_x.
    WA_LAYOUT-NO_TOOLBAR = SPACE.
*     WA_LAYOUT-COL_OPT    = C_X.
    WA_LAYOUT-GRID_TITLE = 'Aprovadores'.
    PERFORM MONTAR_LAYOUT_APROV.

*    WL_FILTER-FIELDNAME = 'DMBTR'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = '0'.
*
*    APPEND WL_FILTER TO TL_FILTER.

    CALL METHOD GRID4->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
*       IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_APROV[].

  ELSE.
    CALL METHOD GRID4->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

  IF OBG_CONTEINER_PARC IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_PARC
      EXPORTING
        CONTAINER_NAME = G_CC_PARC.

    CREATE OBJECT GRID5
      EXPORTING
        I_PARENT = OBG_CONTEINER_PARC.

    REFRESH: TL_FUNCTION.
    CREATE OBJECT OBG_TOOLBAR2
      EXPORTING
        IO_ALV_GRID2 = GRID5.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR2->ON_TOOLBAR FOR GRID5.
    SET HANDLER OBG_TOOLBAR2->HANDLE_USER_COMMAND FOR GRID5.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

*    wa_layout-cwidth_opt = c_x.
    WA_LAYOUT-NO_TOOLBAR = SPACE.
*     WA_LAYOUT-COL_OPT    = C_X.
*    wa_layout-edit = c_x.
    WA_LAYOUT-STYLEFNAME = 'STYLE'.
    WA_LAYOUT-GRID_TITLE = 'Parceiros da Nota'.
    PERFORM MONTAR_LAYOUT_PARC.
*    PERFORM montar_style.
*    WL_FILTER-FIELDNAME = 'DMBTR'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = '0'.
*
*    APPEND WL_FILTER TO TL_FILTER.
    CALL METHOD GRID5->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
*       IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_PARC[].

    CALL METHOD GRID5->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED2 FOR GRID5.
*              lcl_event_handler=>on_data_changed2 FOR grid5.

  ELSE.
*    PERFORM montar_style.
    CALL METHOD GRID5->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = T_FIELDCATALOG.

    READ TABLE TG_FIELDS
      WITH KEY GROUP1 = 'GR1'.
    IF SY-SUBRC IS INITIAL.
*      MOVE: c_x TO wa_layout-edit.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
         WHERE FIELDNAME EQ 'PARVW'
            OR FIELDNAME EQ 'PARID'.

        W_FIELDCATALOG-EDIT = C_X.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
*
    ELSE.
*      MOVE : space TO wa_layout-edit.
      LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
         WHERE FIELDNAME EQ 'PARVW'
            OR FIELDNAME EQ 'PARID'.

        W_FIELDCATALOG-EDIT = SPACE.
        MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
      ENDLOOP.
    ENDIF.

    CALL METHOD GRID5->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG.

    CALL METHOD GRID5->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM F_DROPDOWN_TABLE.

  DATA: LT_DROPDOWN TYPE LVC_T_DRAL,
        LS_DROPDOWN TYPE LVC_S_DRAL.

  FREE: LT_DROPDOWN.

  LS_DROPDOWN-HANDLE  = '1'.
  LS_DROPDOWN-INT_VALUE = 'S'.
  LS_DROPDOWN-VALUE   = 'S Sim'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-HANDLE  = '1'.
  LS_DROPDOWN-INT_VALUE = 'N'.
  LS_DROPDOWN-VALUE   = 'N Não'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-HANDLE  = '1'.
  LS_DROPDOWN-INT_VALUE = ' '.
  LS_DROPDOWN-VALUE   = '   '.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  CALL METHOD GRID1->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN_ALIAS = LT_DROPDOWN.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  REFRESH T_FIELDCATALOG.
  SELECT SINGLE *
      FROM ZFIWRT0001
      INTO @DATA(WL_0001)
       WHERE OPERACAO EQ @P_OPERACAO.
  IF SY-SUBRC = 0.
    IF WL_0001-GE_REMESSA = 'S'.
      PERFORM MONTAR_ESTRUTURA USING:
        1 'VBAP'        'VBELN'   'TG_ITENS' 'VBELN'  'Ord.Venda'   '10' ' ' 'X' 'X'.
    ENDIF.

  ENDIF.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZFIWRT0009'  'ITMNUM'  'TG_ITENS' 'ITMNUM' ' '           '04' ' ' ' ' ' ',
        1 'MARA'        'MATNR'   'TG_ITENS' 'MATNR'  ' '           '15' ' ' ' ' 'X',
        2 'MAKT'        'MAKTX'   'TG_ITENS' 'MAKTX'  ' '           '25' ' ' ' ' ' ',
        2 'ZIB_NFE_DIST_ITM'  'PROD_DESCRICAO' 'TG_ITENS' 'PROD_DESCRICAO'  'Descrição Item NF-e' '20' 'X' ' ' ' ', "US #163043 - MMSILVA - 18.03.2025
        2 'MARC'        'STEUC'   'TG_ITENS' 'STEUC'  'NCM'         '07' ' ' ' ' ' '.
  IF WL_0001-LM_INDEA        = 'S' OR
     WL_0001-COMPLEMENT_ICMS = 'S' AND ( WG_ACAO = C_MODIF OR WG_ACAO = C_ADD ).  "*-CS2023000043-09.02.2023-#102019-JT
    PERFORM MONTAR_ESTRUTURA USING:
          3 ' '    ' '    'TG_ITENS' 'CFOP'   'CFOP'           '07' 'X' ' ' ' '.
  ELSE.
    PERFORM MONTAR_ESTRUTURA USING:
          3 ' '    ' '    'TG_ITENS' 'CFOP'   'CFOP'           '07' ' ' ' ' ' '.
  ENDIF.
  PERFORM MONTAR_ESTRUTURA USING:
      4 ' '           ' '       'TG_ITENS' 'CHARG'  'Lote'        '10' ' ' ' ' ' ',
      4 'ZFIWRT0009'  'NR_FASE' 'TG_ITENS' 'NR_FASE'  'Nr. Fase'  '20' 'X' ' ' ' ', "// US-168932 WBARBOSA 20/05/2025
      5 'T001W'       'WERKS'   'TG_ITENS' 'WERKS'  ' '           '05' ' ' ' ' 'X',
      5 'ZFIWRT0009'  'LGORT'   'TG_ITENS' 'LGORT'  ' '           '05' ' ' ' ' ' ',
      6 'ZFIWRT0009'  'ANLN1'   'TG_ITENS' 'ANLN1'  ' '           '10' 'X' ' ' ' ',
      7 'ZFIWRT0009'  'ANLN2'   'TG_ITENS' 'ANLN2'  ' '           '10' ' ' 'X' ' ',
*-CS2020001331 - 06.10.2021 - JT - inicio
      8 'ZFIWRT0009'  'POSSUI_ICMS_ST' 'TG_ITENS' 'POSSUI_ICMS_ST'  'ICMS ST?' '08' 'X' ' ' ' ',
*-CS2020001331 - 06.10.2021 - JT - fim
      9 'MSEG'        'MENGE'   'TG_ITENS' 'MENGE'  'Quantidade'          '10' ' ' 'X' 'X',
     10 'MARA'        'MEINS'   'TG_ITENS' 'MEINS'  ' '                   '05' ' ' ' ' 'X'.

  IF  WG_FISCAL-IMOBILIZADO = 'S' AND
    ( WG_FISCAL-TP_MV_IMOB =  'T' OR WG_FISCAL-TP_MV_IMOB = 'C' ).
    PERFORM MONTAR_ESTRUTURA USING:
          11 'ZFIWRT0009'  'NETPR'   'TG_ITENS' 'NETPR'  'Preço'               '10' ' ' ' ' 'X'.
  ELSE.
    PERFORM MONTAR_ESTRUTURA USING:
          11 'ZFIWRT0009'  'NETPR'   'TG_ITENS' 'NETPR'  'Preço'               '10' 'X' ' ' 'X'.
  ENDIF.

  PERFORM MONTAR_ESTRUTURA USING:
 12 'ZFIWRT0009'  'NETDIS'  'TG_ITENS' 'NETDIS' 'Desconto'            '10' 'X' ' ' 'X',
 13 'ZFIWRT0009'  'NETFRE'  'TG_ITENS' 'NETFRE' 'Frete'               '10' 'X' ' ' 'X',
 14 'ZFIWRT0009'  'NETINS'  'TG_ITENS' 'NETINS' 'Seguro'              '10' 'X' ' ' 'X',
 15 'ZFIWRT0009'  'NETOTH'  'TG_ITENS' 'NETOTH' 'Despesas acessórias' '20' 'X' ' ' 'X',
 16 'MSEG'        'DMBTR'   'TG_ITENS' 'NETWR'  'Total'               '10' ' ' 'X' 'X'.


  IF WL_0001-LM_INDEA = 'S'.
    PERFORM MONTAR_ESTRUTURA USING:
       13 'ZMMT0102'    'RENAS'   'TG_ITENS' 'RENAS'  'Renasem'     '15' 'X' ' ' ' ',
       14 ' '           ' '       'TG_ITENS' 'FASE'   'Fase'        '05' ' ' ' ' ' '.
  ENDIF.

  PERFORM F_TEXTO_SD USING WL_0001-TEXTO_SD.
  PERFORM F_TEXTO_IMBOLIZADO.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  IF WG_FISCAL-COMPLEMENTO = 'S'.
    P_EDIT = 'X'.
  ENDIF.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD = 'FASE'.
    W_FIELDCATALOG-ICON = C_X.
    W_FIELDCATALOG-HOTSPOT  = C_X.
  ENDIF.
  IF P_FIELD EQ 'ESTORNO'.
    W_FIELDCATALOG-CHECKBOX = C_X.
  ENDIF.

  IF P_FIELD EQ 'PARID'.
    W_FIELDCATALOG-KEY           = 'X'.
    W_FIELDCATALOG-KEY_SEL       = 'X'.
  ENDIF.

  IF P_FIELD EQ 'ANLN1'.
*  OR P_FIELD EQ 'ANLN2'.
    W_FIELDCATALOG-F4AVAILABL = C_X.
*W_FIELDCATALOG-AUTO_VALUE
* BUG - 75507 - Inicio - CBRAND
    W_FIELDCATALOG-CHECKTABLE = '!'.
* BUG - 75507 - Fim - CBRAND
  ENDIF.

  IF P_FIELD EQ 'NR_FASE'.
    W_FIELDCATALOG-F4AVAILABL = C_X.
  ENDIF.

  IF P_FIELD EQ 'ZLSCH'.
    W_FIELDCATALOG-F4AVAILABL = C_X.
  ENDIF.

  IF P_FIELD EQ 'POSSUI_ICMS_ST'.
    W_FIELDCATALOG-DRDN_HNDL = '1'.
    W_FIELDCATALOG-DRDN_ALIAS = ABAP_TRUE.
  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_IMPOSTOS .

  DATA(L_EDIT) = COND #( WHEN W_ZFIWRT0001-COMPLEMENT_ICMS = 'S' AND ( WG_ACAO = C_MODIF OR WG_ACAO = C_ADD ) THEN ABAP_TRUE
                                                                                                              ELSE ABAP_OFF ).

  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'J_1BAJ'       'TAXTYP'   'TL_IMPO_AUX' 'TAXTYP'  ' '  ' ' ' ' ' ' ' ',
        2 'J_1BAJT'      'TTYPETXT' 'TL_IMPO_AUX' 'TTYPETXT'  ' '  ' ' ' ' ' ' ' ',
        3 'J_1BAJ'       'TAXGRP'   'TL_IMPO_AUX' 'TAXGRP'  ' '  ' ' ' ' ' ' ' ',
        4 'ZFIWRT0010'   'BASE'     'TL_IMPO_AUX' 'BASE'    ' '  ' ' L_EDIT ' ' ' ',
        5 'ZFIWRT0010'   'RATE'     'TL_IMPO_AUX' 'RATE'    ' '  ' ' L_EDIT ' ' ' ',
        6 'ZFIWRT0010'   'TAXVAL'   'TL_IMPO_AUX' 'TAXVAL'  ' '  ' ' L_EDIT ' ' ' ',
        7 'ZFIWRT0010'   'EXCBAS'   'TL_IMPO_AUX' 'EXCBAS'  ' '  ' ' L_EDIT ' ' ' ',
        8 'ZFIWRT0010'   'OTHBAS'   'TL_IMPO_AUX' 'OTHBAS'  ' '  ' ' L_EDIT ' ' ' '.

*       US #163043 - MMSILVA - 18.03.2025 - Inicio
  IF WG_DIREITOS-TAXCODE EQ 'V2' OR WG_DIREITOS-TAXCODE EQ 'V1'.
    PERFORM MONTAR_ESTRUTURA USING:
          9  'ZFIWRT0010' 'ICMS_BASE ' 'TL_IMPO_AUX' 'ICMS_BASE '  'Base XML ' ' ' ' ' ' ' ' ',
          10 'ZFIWRT0010' 'ICMS_RATE ' 'TL_IMPO_AUX' 'ICMS_RATE '  'Aliq. XML' ' ' ' ' ' ' ' ',
          11 'ZFIWRT0010' 'ICMS_VALUE' 'TL_IMPO_AUX' 'ICMS_VALUE'  'Vlr. XML ' ' ' ' ' ' ' ' '.
  ENDIF.
*       US #163043 - MMSILVA - 18.03.2025 - Fim

ENDFORM.                    " MONTAR_LAYOUT_IMPOSTOS

*&---------------------------------------------------------------------*
*&      Form  MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
FORM F_CALCULO_DIFAL USING  P_ITENS   STRUCTURE TG_ITENS
                            P_TAXTYP       TYPE ZFIWRT0010-TAXTYP
                   CHANGING P_IMPO    STRUCTURE TG_IMPO.

  DATA: L_LIFNR_LCNEG TYPE LFA1-LIFNR,
        L_LIFNR_PARCE TYPE LFA1-LIFNR,
        L_REGIO_LCNEG TYPE LFA1-REGIO,
        L_REGIO_PARCE TYPE LFA1-REGIO,
        L_RATE_LCNEG  TYPE J_1BTXIC1-RATE,
        L_RATE_ORIGE  TYPE J_1BTXIC1-RATE,
        L_SUBTOT_ALIQ TYPE J_1BTXIC1-RATE,
        L_TOTAL_ITEM  TYPE ZFIWRT0010-TAXVAL.

  CLEAR:  L_LIFNR_LCNEG,
          L_LIFNR_PARCE,
          L_REGIO_LCNEG,
          L_REGIO_PARCE,
          L_RATE_LCNEG,
          L_RATE_ORIGE.

  CHECK P_ITENS-POSSUI_ICMS_ST = 'N'
    AND P_TAXTYP               = C_ICOP.

*----------------------------------
* operacao tem ICOP?
*----------------------------------
  SELECT TAXTYP
    INTO @DATA(L_TAXTYP)
    FROM ZFIWRT0002
      UP TO 1 ROWS
   WHERE OPERACAO = @P_OPERACAO
     AND TAXTYP   = @C_ICOP.
  ENDSELECT.

  CHECK SY-SUBRC = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_BRANCH
    IMPORTING
      OUTPUT = L_LIFNR_LCNEG.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_PARID
    IMPORTING
      OUTPUT = L_LIFNR_PARCE.

  SELECT REGIO
    INTO L_REGIO_LCNEG
    FROM LFA1
      UP TO 1 ROWS
   WHERE LIFNR = L_LIFNR_LCNEG.
  ENDSELECT.

  SELECT REGIO
    INTO L_REGIO_PARCE
    FROM LFA1
      UP TO 1 ROWS
   WHERE LIFNR = L_LIFNR_PARCE.
  ENDSELECT.

  CHECK L_REGIO_LCNEG <> L_REGIO_PARCE.

  SELECT RATE
    INTO L_RATE_LCNEG
    FROM J_1BTXIC1
      UP TO 1 ROWS
   WHERE LAND1    = C_BR
     AND SHIPFROM	=	L_REGIO_LCNEG
     AND SHIPTO	  =	L_REGIO_LCNEG.
  ENDSELECT.

  SELECT RATE
    INTO L_RATE_ORIGE
    FROM J_1BTXIC1
      UP TO 1 ROWS
   WHERE LAND1    = C_BR
     AND SHIPFROM	=	L_REGIO_PARCE
     AND SHIPTO	  =	L_REGIO_LCNEG.
  ENDSELECT.

*----------------------------
* calculo
*----------------------------
  P_IMPO-BASE   = P_ITENS-NETWR.
  P_IMPO-RATE   = L_RATE_LCNEG  - L_RATE_ORIGE.
  P_IMPO-TAXVAL = P_IMPO-BASE * ( P_IMPO-RATE / 100 ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM MONTA_IMPOSTOS  TABLES TL_IMPO STRUCTURE TG_IMPO
                     USING   E_ROW.
  DATA: BEGIN OF WL_1BTXIC,
          RATE TYPE J_1BTXIC3-RATE,
          BASE TYPE J_1BTXIC3-BASE,
        END OF WL_1BTXIC.

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

*data:   tg_impo_aux2  like table of tg_impo2 with header line,
*        wa_impo_aux2  like tg_impo2,
*        wa_impo_aux3  like tg_impo,
  DATA: V_LINE TYPE I,
        V_ICS1 TYPE ZFIWRT0010-TAXVAL.

  V_ICS1 = 0.
  V_LINE = 1.

** Fim Alteração feita por Alexandre

  DATA: WL_ITENS     LIKE LINE OF TG_ITENS,
        WL_1BAA      TYPE J_1BAA,
        WL_BASE_AUX  TYPE J_1BTXIC3-BASE,
        WL_A924      TYPE A924,
        WL_KONP      TYPE KONP,
        WL_T001W     TYPE T001W,
        WL_1BTXSDC   TYPE J_1BTXSDC,
        WL_1BTXPIS   TYPE J_1BTXPIS,
        WL_1BTXCOF   TYPE J_1BTXCOF,
        WL_IMPO_COMP LIKE LINE OF TG_IMPO_COMP,
        V_DMBTR      TYPE ZFIWRT0011-DMBTR,
        L_ROW        TYPE SY-TABIX.

  DATA: VL_CST_ICMS TYPE C LENGTH 2.

* US #163043 - MMSILVA - 18.03.2025 - Inicio
  DATA: WA_ITMNUM TYPE ZIB_NFE_DIST_ITM-PROD_ITEM.
* US #163043 - MMSILVA - 18.03.2025 - Fim

* US #164022 - MMSILVA - 21.03.2025 - Inicio
  DATA: WL_CALCULO_PIS    LIKE LINE OF TG_IMPO,
        WL_CALCULO_COFINS LIKE LINE OF TG_IMPO.
* US #164022 - MMSILVA - 21.03.2025 - Fim

  L_ROW = E_ROW.


**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

  READ TABLE TG_IMPO WITH KEY TAXTYP = 'ICS1'.
*
  IF SY-SUBRC = 0.

    MOVE 99 TO TG_IMPO-LINE.
    MODIFY TG_IMPO INDEX SY-TABIX TRANSPORTING LINE.
    SORT TG_IMPO BY LINE DESCENDING.

  ENDIF.

** Fim Alteração feita por Alexandre

  "PERFORM impostos_complemento USING e_row.
**  Alteracao feita por Lucas ref; CS1129419 - IR147555 - 21.08.2023
*  READ TABLE tg_itens INTO wl_itens INDEX e_row.
  READ TABLE TG_ITENS INTO WL_ITENS INDEX L_ROW.
** Fim Alteração feita por Lucas

  DATA(_VISUAL_LCTO) = ABAP_FALSE.
  SELECT SINGLE *
    FROM ZFIWRT0008
   WHERE SEQ_LCTO EQ P_SEQ_LCTO.

  IF ( SY-SUBRC EQ 0 ) AND ( P_SEQ_LCTO IS NOT INITIAL ).
    READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
      WITH KEY GROUP1 = 'GR1'.

    IF SY-SUBRC IS NOT INITIAL.
      _VISUAL_LCTO = ABAP_TRUE.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM J_1BAA
    INTO WL_1BAA
     WHERE NFTYPE EQ WG_FISCAL-NFTYPE.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S'.
    READ TABLE TG_IMPO_COMP WITH KEY ITMNUM = WL_ITENS-ITMNUM
                                     EDIT   = ABAP_TRUE.
    IF SY-SUBRC = 0.
      LOOP AT TG_IMPO_COMP WHERE ITMNUM = WL_ITENS-ITMNUM.
        MOVE-CORRESPONDING TG_IMPO_COMP TO TL_IMPO.
        APPEND TL_IMPO.
      ENDLOOP.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

  DATA(_CALCULA_VALOR_NF) = ABAP_TRUE.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(I_DATA)
   WHERE SETNAME EQ 'MAGI_ZNFW0002_PAUTA'
     AND VALFROM EQ @P_BUKRS.

  IF ( SY-SUBRC EQ 0 ) AND ( WL_1BAA-FORM IS NOT INITIAL ).
    _CALCULA_VALOR_NF = ABAP_FALSE.
  ENDIF.

  IF ( WL_1BAA-DIRECT EQ '1' ).

    CLEAR: WL_A924, WL_KONP, WL_T001W, WL_1BTXSDC.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM J_1BTXSDC
      INTO WL_1BTXSDC
       WHERE TAXCODE EQ WG_DIREITOS-TAXCODE.

    LOOP AT TG_IMPO.
      CLEAR WL_1BTXIC.
      READ TABLE TG_IMPO_COMP INTO WL_IMPO_COMP WITH KEY ITMNUM = WL_ITENS-ITMNUM
                                                         TAXTYP = TG_IMPO-TAXTYP BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 AND  WG_FISCAL-COMPLEMENTO EQ 'S' ) OR
         ( SY-SUBRC EQ 0 AND  _VISUAL_LCTO EQ ABAP_TRUE    ).

        MOVE-CORRESPONDING: WL_IMPO_COMP TO TL_IMPO.
        MOVE :  TG_IMPO-TTYPETXT  TO TL_IMPO-TTYPETXT,
                TG_IMPO-TAXGRP    TO TL_IMPO-TAXGRP.

*-CS2020001331 - 06.10.2021 - JT - inicio
        PERFORM F_CALCULO_DIFAL  USING WL_ITENS
                                       TG_IMPO-TAXTYP
                              CHANGING TL_IMPO.
*-CS2020001331 - 06.10.2021 - JT - fim

        APPEND TL_IMPO.
      ELSEIF TG_IMPO[] IS NOT INITIAL.
        IF TG_IMPO-TAXTYP EQ C_ICM3.
          IF WG_DIREITOS-OPERTYP EQ C_T.
*          SELECT SINGLE *
*            FROM J_1BAA
*            INTO WL_1BAA
*             WHERE ITMTYP EQ WG_FISCAL-ITMTYP.

            DATA(_UTILIZA_BASE_NF) = ABAP_FALSE.
            IF WL_1BAA-ENTRAD EQ C_X.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              PERFORM F_J1BTAX_43_31  USING WL_ITENS
                                      CHANGING WL_1BTXIC.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF  WL_1BTXIC IS INITIAL.
                SELECT SINGLE RATE BASE
                  FROM J_1BTXIC3
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO
                     AND GRUOP    = C_30
                     AND VALUE    = P_PARID
                     AND VALUE2	  =	WL_ITENS-MATNR.

                IF SY-SUBRC IS NOT INITIAL.
                  SELECT SINGLE RATE BASE
                    FROM J_1BTXIC3
                    INTO WL_1BTXIC
                     WHERE LAND1    = C_BR
                       AND SHIPFROM	=	WG_SHIPFROM
                       AND SHIPTO	  =	WG_SHIPTO
                       AND GRUOP    = C_40
                       AND VALUE    = P_PARID.

                  IF SY-SUBRC IS NOT INITIAL.
                    IF P_PARVW NE C_BR
                    AND P_PARVW NE C_AG.
                      SELECT SINGLE RATE BASE
                        FROM J_1BTXIC2
                        INTO WL_1BTXIC
                         WHERE LAND1    = C_BR
                           AND SHIPFROM	=	WG_SHIPFROM
                           AND SHIPTO	  =	WG_SHIPTO
                           AND MATNR    = WL_ITENS-MATNR.
                    ENDIF.
                    IF SY-SUBRC IS NOT INITIAL.
                      SELECT SINGLE RATE
                        FROM J_1BTXIC1
                        INTO WL_1BTXIC
                         WHERE LAND1    = C_BR
                           AND SHIPFROM	=	WG_SHIPFROM
                           AND SHIPTO	  =	WG_SHIPTO.

                      IF SY-SUBRC EQ 0.
                        _UTILIZA_BASE_NF = ABAP_TRUE.
                      ENDIF.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              PERFORM F_J1BTAX_43_31  USING WL_ITENS
                                      CHANGING WL_1BTXIC.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF WL_1BTXIC IS INITIAL.
                SELECT SINGLE RATE BASE
                  FROM J_1BTXIC3
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO
                     AND GRUOP    = C_76
                     AND VALUE    = P_PARID
                     AND VALUE2	  =	WL_ITENS-MATNR.

                IF SY-SUBRC IS NOT INITIAL.
                  IF P_PARVW NE C_BR
                  AND P_PARVW NE C_AG.
                    SELECT SINGLE RATE BASE
                      FROM J_1BTXIC2
                      INTO WL_1BTXIC
                       WHERE LAND1    = C_BR
                         AND SHIPFROM	=	WG_SHIPFROM
                         AND SHIPTO	  =	WG_SHIPTO
                         AND MATNR    = WL_ITENS-MATNR.
                  ENDIF.
                  IF SY-SUBRC IS NOT INITIAL.
                    SELECT SINGLE RATE
                      FROM J_1BTXIC1
                      INTO WL_1BTXIC
                       WHERE LAND1    = C_BR
                         AND SHIPFROM = WG_SHIPFROM
                         AND SHIPTO   = WG_SHIPTO.

                    IF SY-SUBRC EQ 0.
                      _UTILIZA_BASE_NF = ABAP_TRUE.
                    ENDIF.
                  ENDIF.
                ENDIF.

              ENDIF.
            ENDIF.

            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            SELECT SINGLE *
              FROM T001W
              INTO WL_T001W
               WHERE WERKS EQ WL_ITENS-WERKS.
            IF SY-SUBRC IS INITIAL.
              IF ( WL_1BAA-DIRECT NE '1' ).

                SELECT SINGLE *
                  FROM A924
                  INTO WL_A924
                   WHERE KSCHL    EQ 'ZIVP'
                     AND ALAND    EQ 'BR'
                     AND TXREG_SF EQ WL_T001W-REGIO
                     AND MATNR    EQ WL_ITENS-MATNR
                     AND DATAB    LE SY-DATUM
                     AND DATBI    GE SY-DATUM.

                IF SY-SUBRC IS INITIAL.


                  SELECT SINGLE *
                    FROM KONP
                    INTO WL_KONP
                     WHERE KNUMH EQ WL_A924-KNUMH.

                ENDIF.
              ENDIF.
            ENDIF.
            IF WL_1BTXIC-BASE IS INITIAL.

**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _CALCULA_VALOR_NF EQ ABAP_TRUE.
                IF WL_KONP-KBETR GT WL_ITENS-NETPR.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_KONP-KBETR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
*              tl_impo-base   = wl_itens-netwr.

              IF _UTILIZA_BASE_NF EQ ABAP_TRUE.
                TL_IMPO-BASE   = WL_ITENS-NETWR.
              ELSE.
                TL_IMPO-BASE   = WL_1BTXIC-BASE.  "ALRS Se a J1NTAX for base ZERO 23/'0/2023
              ENDIF.

              TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
              TL_IMPO-OTHBAS = 0.

              "US #163043 - MMSILVA - 18.03.2025 - Inicio
              IF P_CHAVE_ACESSO IS NOT INITIAL.
                WA_ITMNUM = WL_ITENS-ITMNUM / 10.
                SELECT SINGLE ICMS_BASE, ICMS_AQT, PROD_VLR_TOTAL_B, IPI_VALOR, ICMS_ST_VALOR, PROD_QTD_COMERCI, PROD_VL_DESCONTO, ICMS_VALOR INTO @WA_ICMS_XML FROM ZIB_NFE_DIST_ITM WHERE CHAVE_NFE EQ @P_CHAVE_ACESSO AND PROD_ITEM EQ @WA_ITMNUM.

                IF SY-SUBRC IS INITIAL.

                  TL_IMPO-ICMS_BASE  = WA_ICMS_XML-ICMS_BASE.
                  TL_IMPO-ICMS_RATE  = WA_ICMS_XML-ICMS_AQT.
                  TL_IMPO-ICMS_VALUE = WA_ICMS_XML-ICMS_VALOR.

                ELSE.

                  SELECT SINGLE * FROM ZIB_CTE_DIST_TER INTO @WA_CTE_XML WHERE CD_CHAVE_CTE EQ @P_CHAVE_ACESSO.

                  IF SY-SUBRC IS INITIAL.

                    TL_IMPO-ICMS_BASE  = WA_CTE_XML-VALOR_BASE_ICMS.
                    TL_IMPO-ICMS_RATE  = ( WA_CTE_XML-VALOR_ICMS / WA_CTE_XML-VALOR_BASE_ICMS ) * 100.
                    TL_IMPO-ICMS_VALUE = WA_CTE_XML-VALOR_ICMS.

                  ENDIF.

                ENDIF.
              ENDIF.
              "US #163043 - MMSILVA - 18.03.2025 - Fim

            ELSE.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _CALCULA_VALOR_NF EQ ABAP_TRUE.
                IF WL_KONP-KBETR GT WL_ITENS-NETPR.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_KONP-KBETR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ).

              CLEAR: VL_CST_ICMS.

              SELECT SINGLE *
                FROM J_1BATL1 INTO @DATA(WL_J_1BATL1)
               WHERE TAXLAW = @WG_DIREITOS-TAXLW1.

              IF SY-SUBRC EQ 0.
                CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
                  EXPORTING
                    INPUT  = WL_J_1BATL1-TAXSIT
                  IMPORTING
                    OUTPUT = VL_CST_ICMS.
              ENDIF.

              IF VL_CST_ICMS EQ '20'.
                TL_IMPO-EXCBAS = WL_ITENS-NETWR - TL_IMPO-BASE.
              ELSE.
                TL_IMPO-OTHBAS = WL_ITENS-NETWR - TL_IMPO-BASE.
              ENDIF.

              "US #163043 - MMSILVA - 18.03.2025 - Inicio
              IF P_CHAVE_ACESSO IS NOT INITIAL.
                WA_ITMNUM = WL_ITENS-ITMNUM / 10.
                SELECT SINGLE ICMS_BASE, ICMS_AQT, PROD_VLR_TOTAL_B, IPI_VALOR, ICMS_ST_VALOR, PROD_QTD_COMERCI, PROD_VL_DESCONTO, ICMS_VALOR INTO @WA_ICMS_XML FROM ZIB_NFE_DIST_ITM WHERE CHAVE_NFE EQ @P_CHAVE_ACESSO AND PROD_ITEM EQ @WA_ITMNUM.

                IF SY-SUBRC IS INITIAL.

                  TL_IMPO-ICMS_BASE  = WA_ICMS_XML-ICMS_BASE.
                  TL_IMPO-ICMS_RATE  = WA_ICMS_XML-ICMS_AQT.
                  TL_IMPO-ICMS_VALUE = WA_ICMS_XML-ICMS_VALOR.

                ELSE.

                  SELECT SINGLE * FROM ZIB_CTE_DIST_TER INTO @WA_CTE_XML WHERE CD_CHAVE_CTE EQ @P_CHAVE_ACESSO.

                  IF SY-SUBRC IS INITIAL.

                    TL_IMPO-ICMS_BASE  = WA_CTE_XML-VALOR_BASE_ICMS.
                    TL_IMPO-ICMS_RATE  = ( WA_CTE_XML-VALOR_ICMS / WA_CTE_XML-VALOR_BASE_ICMS ) * 100.
                    TL_IMPO-ICMS_VALUE = WA_CTE_XML-VALOR_ICMS.

                  ENDIF.

                ENDIF.
              ENDIF.
              "US #163043 - MMSILVA - 18.03.2025 - Fim

            ENDIF.
            TL_IMPO-RATE = WL_1BTXIC-RATE.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ELSEIF WG_DIREITOS-OPERTYP EQ C_I.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-EXCBAS.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ELSEIF WG_DIREITOS-OPERTYP EQ C_N.
            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ENDIF.
        ELSEIF WL_1BTXSDC-PIS EQ C_X
           AND TG_IMPO-TAXTYP EQ C_IPIS.

          SELECT SINGLE *
            FROM J_1BTXPIS
            INTO WL_1BTXPIS
             WHERE COUNTRY EQ C_BR
               AND GRUOP   EQ C_72
               AND VALUE   EQ WL_ITENS-WERKS.


          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          IF SY-SUBRC IS INITIAL.
            "US #164022 - MMSILVA - 21.03.2025 - Inicio
            READ TABLE TL_IMPO INTO WL_CALCULO_PIS WITH KEY TAXTYP = 'ICM3'.
            "US #164022 - MMSILVA - 21.03.2025 - Fim
            TL_IMPO-BASE   = ( WL_ITENS-NETWR - WL_CALCULO_PIS-TAXVAL ) + V_ICS1. " Alexandre Rimini 09.05.2023 chamado 1016278 - Antes era: wl_itens-netwr. // US #164022 - MMSILVA - 21.03.2025 - Acrescentado wl_calculo_pis
            TL_IMPO-RATE   = WL_1BTXPIS-RATE.
            TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXPIS-RATE / 100 ).
            TL_IMPO-OTHBAS = 0.
          ELSE.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
          ENDIF.
          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.
          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXPIS.

        ELSEIF WL_1BTXSDC-COFINS EQ C_X
           AND TG_IMPO-TAXTYP EQ C_ICOF.
          SELECT SINGLE *
            FROM J_1BTXCOF
            INTO WL_1BTXCOF
             WHERE COUNTRY EQ C_BR
               AND GRUOP   EQ C_71
               AND VALUE   EQ WL_ITENS-WERKS.

          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          IF SY-SUBRC IS INITIAL.
            "US #164022 - MMSILVA - 21.03.2025 - Inicio
            READ TABLE TL_IMPO INTO WL_CALCULO_COFINS WITH KEY TAXTYP = 'ICM3'.
            "US #164022 - MMSILVA - 21.03.2025 - Fim
            TL_IMPO-BASE   = ( WL_ITENS-NETWR - WL_CALCULO_COFINS-TAXVAL ) + V_ICS1. "US #164022 - MMSILVA - 21.03.2025 - Acrescentado wl_calculo_cofins
            TL_IMPO-RATE   = WL_1BTXCOF-RATE.
            IF  TL_IMPO-BASE > 0 AND WL_1BTXCOF-RATE  > 0.
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXCOF-RATE / 100 ).
            ENDIF.
            TL_IMPO-OTHBAS = 0.
          ELSE.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
          ENDIF.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.
          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXCOF.

        ELSEIF  TG_IMPO-TAXTYP EQ C_ICS1.
          SELECT SINGLE *
           FROM J_1BAA
           INTO WL_1BAA
            WHERE ITMTYP EQ WG_FISCAL-ITMTYP.

          IF WL_1BAA-ENTRAD EQ C_X.
            SELECT SINGLE RATE BASE
              FROM J_1BTXIC3
              INTO WL_1BTXIC
               WHERE LAND1    = C_BR
                 AND SHIPFROM	=	WG_SHIPFROM
                 AND SHIPTO	  =	WG_SHIPTO
                 AND GRUOP    = C_30
                 AND VALUE    = P_PARID
                 AND VALUE2	  =	WL_ITENS-MATNR.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE RATE BASE
                FROM J_1BTXIC3
                INTO WL_1BTXIC
                 WHERE LAND1    = C_BR
                   AND SHIPFROM	=	WG_SHIPFROM
                   AND SHIPTO	  =	WG_SHIPTO
                   AND GRUOP    = C_40
                   AND VALUE    = P_PARID.

              IF SY-SUBRC IS NOT INITIAL.
                SELECT SINGLE RATE
                  FROM J_1BTXIC1
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO.

              ENDIF.

            ENDIF.

          ELSE.

            SELECT SINGLE RATE BASE
              FROM J_1BTXIC3
              INTO WL_1BTXIC
               WHERE LAND1    = C_BR
                 AND SHIPFROM	=	WG_SHIPFROM
                 AND SHIPTO	  =	WG_SHIPTO
                 AND GRUOP    = C_76
                 AND VALUE    = P_PARID
                 AND VALUE2	  =	WL_ITENS-MATNR.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE RATE
                FROM J_1BTXIC1
                INTO WL_1BTXIC
                 WHERE LAND1    = C_BR
                   AND SHIPFROM = WG_SHIPFROM
                   AND SHIPTO   = WG_SHIPTO.
            ENDIF.


          ENDIF.
          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.

*      IF WL_1BTXIC-BASE IS INITIAL.
*        TL_IMPO-BASE   = WL_ITENS-NETWR.
*        TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
*        TL_IMPO-OTHBAS = 0.
*
*      ELSE.
*        TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
          TL_IMPO-RATE =  WL_1BTXIC-RATE .
          IF WL_1BTXIC-BASE > 0 AND  WL_1BTXIC-RATE > 0.
            TL_IMPO-BASE = WL_ITENS-NETWR / ( 1 - ( ( WL_1BTXIC-RATE * ( WL_1BTXIC-BASE / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
*            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          ENDIF.

*          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
*            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
*          ENDIF.

          IF TL_IMPO-BASE > 0 AND  TL_IMPO-RATE > 0.
            IF  WL_1BTXIC-BASE > 0.
**Inicio USER STORY #81382 - Anderson Oenning
              TL_IMPO-BASE   = TL_IMPO-BASE * ( WL_1BTXIC-BASE / 100 ).
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( TL_IMPO-RATE / 100 ).
            ELSE.
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( TL_IMPO-RATE / 100 ).
            ENDIF.
*            v_ics1 = tl_impo-taxval. " Alexandre - Rimini - CS1016278 - INC107256 - 09.05.2023
**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.



*      ENDIF.
*      TL_IMPO-RATE = WL_1BTXIC-RATE.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.

          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXIC.

*-CS2020001331 - 06.10.2021 - JT - inicio
        ELSEIF  TG_IMPO-TAXTYP EQ C_ICOP.

          MOVE-CORRESPONDING: TG_IMPO  TO TL_IMPO.

          PERFORM F_CALCULO_DIFAL   USING WL_ITENS
                                          TG_IMPO-TAXTYP
                                 CHANGING TL_IMPO.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.

          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXIC.
*-CS2020001331 - 06.10.2021 - JT - fim

        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.

          APPEND TL_IMPO.
          CLEAR: TL_IMPO.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF ( WL_1BAA-DIRECT EQ '2' ).

    CLEAR: WL_A924, WL_KONP, WL_T001W, WL_1BTXSDC.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM J_1BTXSDC
      INTO WL_1BTXSDC
       WHERE TAXCODE EQ WG_DIREITOS-TAXCODE.

    LOOP AT TG_IMPO.
      CLEAR WL_1BTXIC.
      READ TABLE TG_IMPO_COMP INTO WL_IMPO_COMP WITH KEY ITMNUM = WL_ITENS-ITMNUM
                                                         TAXTYP = TG_IMPO-TAXTYP BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 AND  WG_FISCAL-COMPLEMENTO EQ 'S' ) OR
         ( SY-SUBRC EQ 0 AND _VISUAL_LCTO EQ ABAP_TRUE     ).
        MOVE-CORRESPONDING: WL_IMPO_COMP TO TL_IMPO.
        MOVE :  TG_IMPO-TTYPETXT  TO TL_IMPO-TTYPETXT,
                TG_IMPO-TAXGRP    TO TL_IMPO-TAXGRP.
        APPEND TL_IMPO.
      ELSEIF TG_IMPO[] IS NOT INITIAL.
        IF TG_IMPO-TAXTYP EQ C_ICM3.
          IF WG_DIREITOS-OPERTYP EQ C_T.
            SELECT SINGLE *
              FROM J_1BAA
              INTO WL_1BAA
               WHERE ITMTYP EQ WG_FISCAL-ITMTYP.

            IF WL_1BAA-ENTRAD EQ C_X.
* ini - ir122480 - znfw0002 - j1btax #103385 rjf - 2023.12.05
              PERFORM F_J1BTAX_43_31  USING WL_ITENS
                                      CHANGING WL_1BTXIC.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF WL_1BTXIC IS INITIAL.
                SELECT SINGLE RATE BASE
                  FROM J_1BTXIC3
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO
                     AND GRUOP    = C_30
                     AND VALUE    = P_PARID
                     AND VALUE2	  =	WL_ITENS-MATNR.

                IF SY-SUBRC IS NOT INITIAL.
                  SELECT SINGLE RATE BASE
                    FROM J_1BTXIC3
                    INTO WL_1BTXIC
                     WHERE LAND1    = C_BR
                       AND SHIPFROM	=	WG_SHIPFROM
                       AND SHIPTO	  =	WG_SHIPTO
                       AND GRUOP    = C_40
                       AND VALUE    = P_PARID.

                  IF SY-SUBRC IS NOT INITIAL.
                    IF P_PARVW NE C_BR
                    AND P_PARVW NE C_AG.
                      SELECT SINGLE RATE BASE
                        FROM J_1BTXIC2
                        INTO WL_1BTXIC
                         WHERE LAND1    = C_BR
                           AND SHIPFROM	=	WG_SHIPFROM
                           AND SHIPTO	  =	WG_SHIPTO
                           AND MATNR    = WL_ITENS-MATNR.
                    ENDIF.
                    IF SY-SUBRC IS NOT INITIAL.
                      SELECT SINGLE RATE
                        FROM J_1BTXIC1
                        INTO WL_1BTXIC
                         WHERE LAND1    = C_BR
                           AND SHIPFROM	=	WG_SHIPFROM
                           AND SHIPTO	  =	WG_SHIPTO.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
* ini - ir122480 - znfw0002 - j1btax #103385 rjf - 2023.12.05
              PERFORM F_J1BTAX_43_31  USING WL_ITENS
                                      CHANGING WL_1BTXIC.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF WL_1BTXIC IS INITIAL.
                SELECT SINGLE RATE BASE
                  FROM J_1BTXIC3
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO
                     AND GRUOP    = C_76
                     AND VALUE    = P_PARID
                     AND VALUE2	  =	WL_ITENS-MATNR.

                IF SY-SUBRC IS NOT INITIAL.
                  IF P_PARVW NE C_BR
                  AND P_PARVW NE C_AG.
                    SELECT SINGLE RATE BASE
                      FROM J_1BTXIC2
                      INTO WL_1BTXIC
                       WHERE LAND1    = C_BR
                         AND SHIPFROM	=	WG_SHIPFROM
                         AND SHIPTO	  =	WG_SHIPTO
                         AND MATNR    = WL_ITENS-MATNR.
                  ENDIF.
                  IF SY-SUBRC IS NOT INITIAL.
                    SELECT SINGLE RATE
                      FROM J_1BTXIC1
                      INTO WL_1BTXIC
                       WHERE LAND1    = C_BR
                         AND SHIPFROM = WG_SHIPFROM
                         AND SHIPTO   = WG_SHIPTO.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            SELECT SINGLE *
              FROM T001W
              INTO WL_T001W
               WHERE WERKS EQ WL_ITENS-WERKS.
            IF SY-SUBRC IS INITIAL.


              SELECT SINGLE *
                FROM A924
                INTO WL_A924
                 WHERE KSCHL    EQ 'ZIVP'
                   AND ALAND    EQ 'BR'
                   AND TXREG_SF EQ WL_T001W-REGIO
                   AND MATNR    EQ WL_ITENS-MATNR
                   AND DATAB    LE SY-DATUM
                   AND DATBI    GE SY-DATUM.

              IF SY-SUBRC IS INITIAL.


                SELECT SINGLE *
                  FROM KONP
                  INTO WL_KONP
                   WHERE KNUMH EQ WL_A924-KNUMH.

              ENDIF.

            ENDIF.

*-CS2021001266 - 15.12.2021 - JT- inicio
            READ TABLE T_REGULA INTO W_REGULA WITH KEY OPERACAO = P_OPERACAO
                                              BINARY SEARCH.
            IF SY-SUBRC = 0.
              IF WG_SHIPFROM = WG_SHIPTO. " AND wg_shipto = w_regula-shipto.
                WL_1BTXIC-RATE = W_REGULA-RATE.
              ENDIF.
            ENDIF.
*-CS2021001266 - 15.12.2021 - JT- fim

            IF WL_1BTXIC-BASE IS INITIAL.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _CALCULA_VALOR_NF EQ ABAP_TRUE.
                IF WL_KONP-KBETR GT WL_ITENS-NETPR.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_KONP-KBETR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              TL_IMPO-BASE   = WL_ITENS-NETWR.
              TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
              TL_IMPO-OTHBAS = 0.

            ELSE.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _CALCULA_VALOR_NF EQ ABAP_TRUE.
                IF WL_KONP-KBETR GT WL_ITENS-NETPR.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  WL_ITENS-NETWR = ( ( WL_ITENS-MENGE * WL_KONP-KBETR ) + WL_ITENS-NETFRE + WL_ITENS-NETINS + WL_ITENS-NETOTH ) - WL_ITENS-NETDIS..
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ).

              CLEAR: VL_CST_ICMS.

              SELECT SINGLE *
                FROM J_1BATL1 INTO WL_J_1BATL1
               WHERE TAXLAW = WG_DIREITOS-TAXLW1.

              IF SY-SUBRC EQ 0.
                CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
                  EXPORTING
                    INPUT  = WL_J_1BATL1-TAXSIT
                  IMPORTING
                    OUTPUT = VL_CST_ICMS.
              ENDIF.

              IF VL_CST_ICMS EQ '20'.
                TL_IMPO-EXCBAS = WL_ITENS-NETWR - TL_IMPO-BASE.
              ELSE.
                TL_IMPO-OTHBAS = WL_ITENS-NETWR - TL_IMPO-BASE.
              ENDIF.

            ENDIF.
            TL_IMPO-RATE = WL_1BTXIC-RATE.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
*#138081 -  ITSOUZA - 31.05.2024 11:53:29 - Inicio
            DATA(ICMS_VAL) = TL_IMPO-TAXVAL.
*#138081 -  ITSOUZA - 31.05.2024 11:53:29 - Fim
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ELSEIF WG_DIREITOS-OPERTYP EQ C_I.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-EXCBAS.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ELSEIF WG_DIREITOS-OPERTYP EQ C_N.
            MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
            IF WG_FISCAL-COMPLEMENTO EQ 'S'.
              CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                     TL_IMPO-EXCBAS.
            ENDIF.
            APPEND TL_IMPO.
            CLEAR: TL_IMPO.
          ENDIF.
        ELSEIF WL_1BTXSDC-PIS EQ C_X
           AND TG_IMPO-TAXTYP EQ C_IPIS.

          SELECT SINGLE *
            FROM J_1BTXPIS
            INTO WL_1BTXPIS
             WHERE COUNTRY EQ C_BR
               AND GRUOP   EQ C_72
               AND VALUE   EQ WL_ITENS-WERKS.

          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          IF SY-SUBRC IS INITIAL.

*            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  _itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
*#138081 -  ITSOUZA - 31.05.2024 11:54:40 - Inicio
            TL_IMPO-BASE   = COND #( WHEN WG_DIREITOS-TAXCODE EQ 'V1' THEN WL_ITENS-NETWR - ICMS_VAL
                                     ELSE WL_ITENS-NETWR ).
*            tl_impo-base   =  wl_itens-netwr.
*#138081 -  ITSOUZA - 31.05.2024 11:54:40 - Fim
            TL_IMPO-RATE   = WL_1BTXPIS-RATE.
            TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXPIS-RATE / 100 ).
            TL_IMPO-OTHBAS = 0.
          ELSE.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
          ENDIF.
          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.
          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXPIS.

        ELSEIF WL_1BTXSDC-COFINS EQ C_X
           AND TG_IMPO-TAXTYP EQ C_ICOF.
          SELECT SINGLE *
            FROM J_1BTXCOF
            INTO WL_1BTXCOF
             WHERE COUNTRY EQ C_BR
               AND GRUOP   EQ C_71
               AND VALUE   EQ WL_ITENS-WERKS.

          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          IF SY-SUBRC IS INITIAL.
*            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  w-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
*#138081 -  ITSOUZA - 31.05.2024 11:54:40 - Inicio
            TL_IMPO-BASE   = COND #( WHEN WG_DIREITOS-TAXCODE EQ 'V1' THEN WL_ITENS-NETWR - ICMS_VAL
                                     ELSE WL_ITENS-NETWR ).
*            tl_impo-base   =  wl_itens-netwr.
*#138081 -  ITSOUZA - 31.05.2024 11:54:40 - Fim
            TL_IMPO-RATE   = WL_1BTXCOF-RATE.
            IF  TL_IMPO-BASE > 0 AND WL_1BTXCOF-RATE  > 0.
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( WL_1BTXCOF-RATE / 100 ).
            ENDIF.
            TL_IMPO-OTHBAS = 0.
          ELSE.
            MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.
          ENDIF.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.
          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXCOF.

        ELSEIF  TG_IMPO-TAXTYP EQ C_ICS1.
          SELECT SINGLE *
           FROM J_1BAA
           INTO WL_1BAA
            WHERE ITMTYP EQ WG_FISCAL-ITMTYP.

          IF WL_1BAA-ENTRAD EQ C_X.
            SELECT SINGLE RATE BASE
              FROM J_1BTXIC3
              INTO WL_1BTXIC
               WHERE LAND1    = C_BR
                 AND SHIPFROM	=	WG_SHIPFROM
                 AND SHIPTO	  =	WG_SHIPTO
                 AND GRUOP    = C_30
                 AND VALUE    = P_PARID
                 AND VALUE2	  =	WL_ITENS-MATNR.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE RATE BASE
                FROM J_1BTXIC3
                INTO WL_1BTXIC
                 WHERE LAND1    = C_BR
                   AND SHIPFROM	=	WG_SHIPFROM
                   AND SHIPTO	  =	WG_SHIPTO
                   AND GRUOP    = C_40
                   AND VALUE    = P_PARID.

              IF SY-SUBRC IS NOT INITIAL.
                SELECT SINGLE RATE
                  FROM J_1BTXIC1
                  INTO WL_1BTXIC
                   WHERE LAND1    = C_BR
                     AND SHIPFROM	=	WG_SHIPFROM
                     AND SHIPTO	  =	WG_SHIPTO.

              ENDIF.

            ENDIF.

          ELSE.

            SELECT SINGLE RATE BASE
              FROM J_1BTXIC3
              INTO WL_1BTXIC
               WHERE LAND1    = C_BR
                 AND SHIPFROM	=	WG_SHIPFROM
                 AND SHIPTO	  =	WG_SHIPTO
                 AND GRUOP    = C_76
                 AND VALUE    = P_PARID
                 AND VALUE2	  =	WL_ITENS-MATNR.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE RATE
                FROM J_1BTXIC1
                INTO WL_1BTXIC
                 WHERE LAND1    = C_BR
                   AND SHIPFROM = WG_SHIPFROM
                   AND SHIPTO   = WG_SHIPTO.
            ENDIF.

          ENDIF.
          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.

*      IF WL_1BTXIC-BASE IS INITIAL.
*        TL_IMPO-BASE   = WL_ITENS-NETWR.
*        TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
*        TL_IMPO-OTHBAS = 0.
*
*      ELSE.
*        TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
          TL_IMPO-RATE =  WL_1BTXIC-RATE .
          IF WL_1BTXIC-BASE > 0 AND  WL_1BTXIC-RATE > 0. "=A1/(1 - ( (B1 * (B2/100)) / 100))
            TL_IMPO-BASE = WL_ITENS-NETWR / ( 1 - ( ( WL_1BTXIC-RATE * ( WL_1BTXIC-BASE / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
*            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          ENDIF.

*          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
*            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
*          ENDIF.

          IF TL_IMPO-BASE > 0 AND  TL_IMPO-RATE > 0.
            IF  WL_1BTXIC-BASE > 0.

**Inicio USER STORY #81382 - Anderson Oenning
              TL_IMPO-BASE   = TL_IMPO-BASE * ( WL_1BTXIC-BASE / 100 ).
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( TL_IMPO-RATE / 100 ).
            ELSE.
              TL_IMPO-TAXVAL = TL_IMPO-BASE * ( TL_IMPO-RATE / 100 ).
            ENDIF.
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
*            v_ics1 = tl_impo-taxval.

**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.




*      ENDIF.
*      TL_IMPO-RATE = WL_1BTXIC-RATE.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.

          APPEND TL_IMPO.
          CLEAR: TL_IMPO, WL_1BTXIC.
        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: TG_IMPO TO TL_IMPO.
          MOVE: WL_ITENS-NETWR TO TL_IMPO-OTHBAS.

          IF WG_FISCAL-COMPLEMENTO EQ 'S'.
            CLEAR: TL_IMPO-RATE, TL_IMPO-BASE, TL_IMPO-TAXVAL, TL_IMPO-OTHBAS,
                   TL_IMPO-EXCBAS.
          ENDIF.

          APPEND TL_IMPO.
          CLEAR: TL_IMPO.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  TG_IMPO_GERA[] = TL_IMPO[].  "*-CS2023000043-09.02.2023-#102019-JT

  PERFORM IMPOSTOS_COMPLEMENTO USING L_ROW.

ENDFORM.                    " MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
*&      Module  BUSCA_VALOR_CONT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_VALOR_CONT OUTPUT.
  PERFORM MONTA_CONTABIL.

*  ENDLOOP.
ENDMODULE.                 " BUSCA_VALOR_CONT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CONTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_CONTAB.
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZFIWRT0011'  'BSCHL'   'TG_CONTAB' 'BSCHL'    ' '  '4' ' ' ' ' ' ',
        2 'ZFIWRT0011'  'HKONT'   'TG_CONTAB' 'HKONT'    ' '  '8' ' ' ' ' ' ',
        3 'SKAT'        'TXT50'   'TG_CONTAB' 'TXT50'    ' '  '25'  ' ' ' ' ' ',
        3 ''            ' '       'TG_CONTAB' 'UMSKZ'    'Rz.Esp'  '8'  ' ' ' ' ' ',
        4 'ZFIWRT0011'  'DMBTR'   'TG_CONTAB' 'DMBTR'    'Valor'  '13'  ' ' ' ' ' ',
        5 'ZFIWRT0011'  'TAXTYP'  'TG_CONTAB' 'TAXTYP'   ' '  '8'  ' ' ' ' ' ',
        9 'ZFIWRT0011'  'ESTORNO' 'TG_CONTAB' 'ESTORNO'  'Estorno'  '5' ' ' ' ' ' ',
        6 'ZFIWRT0011'  'NEWBW'   'TG_CONTAB' 'NEWBW'    ' '  '8' ' ' ' ' ' ',
        7 'ZFIWRT0011'  'ZLSCH'   'TG_CONTAB' 'ZLSCH'    ' '  '8' ' ' ' ' ' ',
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        8 'ZFIWRT0011'  'TAXA_JUROS'   'TG_CONTAB' 'TAXA_JUROS'    ' '  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'TAXA_MULTA'   'TG_CONTAB' 'TAXA_MULTA'    ' '  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'HBKID'        'TG_CONTAB' 'HBKID'         ' '  '8' ' ' ' ' ' ',
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        8 'ZFIWRT0011'  'ZFBDT'   'TG_CONTAB' 'ZFBDT'    'Data Vencimento'  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'KOSTL'   'TG_CONTAB' 'KOSTL'    'Centro de Custos'  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'VBUND'   'TG_CONTAB' 'VBUND'    'Soc.Parceira'  '8' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_CONTAB
*&---------------------------------------------------------------------*
*&      Form  FILL_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_TREE .
  DATA: NODE LIKE MTREESNODE.

* node table of the left tree
  CLEAR NODE.
  NODE-NODE_KEY = C_ROOT.
  NODE-ISFOLDER = 'X'.
  NODE-TEXT = 'Mensagens da Nota'.
  NODE-DRAGDROPID = ' '.
  APPEND NODE TO NODE_ITAB.

*  CLEAR node.
*  node-node_key = 'Child1'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 1'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child2'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 2'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child3'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 3'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child4'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 4'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child5'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 5'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child6'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 6'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child7'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 7'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
ENDFORM.                    " FILL_TREE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_230  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_230 INPUT.
  DATA: WL_LINES         TYPE SY-TABIX,
        WL_LINES_AUX(6),
        WL_KEY           TYPE TV_NODEKEY,
        TL_NODES         TYPE REF TO CL_GUI_OBJECT,
        WL_SELECTED_NODE TYPE TV_NODEKEY,
        TL_EDITOR        TYPE TABLE OF TY_EDITOR.
  .
*        node LIKE mtreesnode.

  DESCRIBE TABLE NODE_ITAB LINES WL_LINES.

  CASE SY-UCOMM.
    WHEN C_ADD_MSG.
      PERFORM PREENCHE_TREE USING WL_LINES
                                  C_ROOT
                                  SPACE
                                  CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                  SPACE
                                  HANDLE_TREE.

    WHEN C_DEL_MSG.
      CALL METHOD TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY = WL_SELECTED_NODE.

      IF WL_SELECTED_NODE NE C_ROOT
      AND WL_SELECTED_NODE IS NOT INITIAL.
        READ TABLE TG_MENSAGEMS_AUX
          WITH KEY SEQNUM = WL_SELECTED_NODE.

        READ TABLE TG_MENSAGEMS
         WITH KEY MESSAGE = TG_MENSAGEMS_AUX-MESSAGE.
        IF SY-SUBRC IS NOT INITIAL.
          DELETE NODE_ITAB WHERE NODE_KEY EQ WL_SELECTED_NODE.
          DELETE TG_MENSAGEMS_AUX WHERE SEQNUM = WL_SELECTED_NODE.
          REFRESH: TG_EDITOR.
          CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
            EXPORTING
              TABLE = TL_EDITOR.

          CALL METHOD TREE->DELETE_NODE
            EXPORTING
              NODE_KEY = WL_SELECTED_NODE.
        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não é possivel eliminar essa'
                                                 'mensagem!'.
        ENDIF.
      ENDIF.
    WHEN C_SAVE_MSG.
*       tg_mensagems_aux

      CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = TG_EDITOR.

      CLEAR: TG_MENSAGEMS_AUX, TG_MENSAGEMS.
      LOOP AT TG_EDITOR INTO WG_EDITOR.
        IF SY-TABIX EQ 1.
          PERFORM PREENCHE_TREE USING WG_DCLKNODEKEY "tl_0005-seqnum
                                    C_ROOT
                                    SPACE
                                    CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                    WG_EDITOR-LINE
                                    HANDLE_TREE.
*          tg_mensagems-seqnum     = wl_line.
*          tg_mensagems-message    = wg_editor-line.
*          MODIFY tg_mensagems INDEX wl_line.
*
*          IF sy-subrc IS NOT INITIAL.
*            APPEND tg_mensagems.
*
*          ENDIF.

          DELETE TG_MENSAGEMS_AUX WHERE SEQNUM EQ WG_DCLKNODEKEY.
        ENDIF.


        TG_MENSAGEMS_AUX-SEQNUM = WG_DCLKNODEKEY.
        ADD 1 TO TG_MENSAGEMS_AUX-LINNUM.
        TG_MENSAGEMS_AUX-MESSAGE = WG_EDITOR-LINE.

*        READ TABLE tg_mensagems_aux TRANSPORTING NO FIELDS
*          WITH KEY seqnum = wl_line
*                   linnum = tg_mensagems_aux-linnum.
*        IF sy-subrc IS INITIAL.
*          MODIFY tg_mensagems_aux INDEX sy-tabix.
*
*        ELSE.
        APPEND TG_MENSAGEMS_AUX.

*        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_230  INPUT
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_LINES  text
*      -->P_6516   text
*      -->P_CL_GUI_SIMPLE_TREE=>RELAT_LAST  text
*      -->P_SPACE  text
*      -->P_HANDLE_TREE  text
*----------------------------------------------------------------------*
FORM PREENCHE_TREE  USING    P_KEY
                             VALUE(P_6516)
                             VALUE(P_ISFOLDER)
                             VALUE(P_RELAT_LAST)
                             VALUE(P_TEXT)
                             P_HANDLE_TREE.

  CLEAR NODE.
  NODE-NODE_KEY = P_KEY.
  NODE-ISFOLDER = P_ISFOLDER.
  NODE-RELATKEY = P_6516.
  NODE-RELATSHIP = P_RELAT_LAST.
  NODE-TEXT = P_TEXT.
  NODE-DRAGDROPID = P_HANDLE_TREE.       " handle of behaviour
  READ TABLE NODE_ITAB TRANSPORTING NO FIELDS
 WITH KEY NODE_KEY = P_KEY.
  IF SY-SUBRC IS INITIAL.
    MODIFY  NODE_ITAB INDEX SY-TABIX FROM NODE .
  ELSE.
    APPEND NODE TO NODE_ITAB.

  ENDIF.

ENDFORM.                    " PREENCHE_TREE
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .

  CLEAR: TG_MENSAGEMS_AUX, TG_MENSAGEMS, TG_ITENS, TG_IMPO, TG_CONTAB, TG_MOVEST,
         WG_OP_FISCAL, WG_DESC_NFTYPE, WG_DESC_ITMTYP, WG_DESC_CFOP, WG_DESC_TAXLW1,
         WG_DESC_TAXLW2, WG_DESC_TAXLW4, WG_DESC_TAXLW5, WG_FISCAL, WG_DIREITOS, WG_SHIPFROM,
         WG_SHIPTO, WG_EDITOR, TG_EDITOR, TG_TBSL, TG_SELECTEDCELL, WG_FLAG, X_FIELD, WG_DOCS,
         WG_DESC_TAXCODE, G_INIT_ONCE,
         WG_ACAO. "*-CS2023000043-09.02.2023-#102019-JT

  REFRESH: TG_MENSAGEMS_AUX, TG_MENSAGEMS, TG_IMPO, TG_IMPO_COMP,
           TG_CONTAB, TG_MOVEST, TG_EDITOR,
           TG_TBSL, TG_SELECTEDCELL, TG_ITENS, TG_APROV. "NODE_ITAB.

*  CALL METHOD TREE->DELETE_ALL_NODES.
*  PERFORM PREENCHE_TREE USING C_ROOT
*                         SPACE
*                         C_X
*                         SPACE
*                         'Mensagens da Nota'
*                         SPACE.
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: WL_INPUT_0008 TYPE ZFIWRT0008,
        TL_INPUT_0009 TYPE TABLE OF ZFIWRT0009 WITH HEADER LINE,
        TL_INPUT_0010 TYPE TABLE OF ZFIWRT0010 WITH HEADER LINE,
        TL_INPUT_0011 TYPE TABLE OF ZFIWRT0011 WITH HEADER LINE,
        TL_INPUT_0012 TYPE TABLE OF ZFIWRT0012 WITH HEADER LINE,
        TL_INPUT_0013 TYPE TABLE OF ZFIWRT0013 WITH HEADER LINE,
        TL_INPUT_0015 TYPE TABLE OF ZFIWRT0015 WITH HEADER LINE,
        TL_INPUT_0019 TYPE TABLE OF ZFIWRT0019 WITH HEADER LINE,
        TL_INPUT_0020 TYPE TABLE OF ZFIWRT0020 WITH HEADER LINE,
        TL_IMPO_AUX   LIKE TABLE OF TG_IMPO WITH HEADER LINE,
        WL_0008_AUX   TYPE ZFIWRT0008,
        WL_0020       TYPE ZFIWRT0020.

  DATA: LT_0020       TYPE TABLE OF ZFIWRT0020.
  DATA: LT_LIN        TYPE TABLE OF J_1BNFLIN.
  DATA: LT_0094       TYPE TABLE OF ZSDT0094.
  DATA: LS_0094       TYPE ZSDT0094.
  DATA(OBJ_TX_CURVA_DB) = NEW ZCL_TAXA_CURVA_DB( ).

  WL_INPUT_0008-SEQ_LCTO = P_SEQ_LCTO.


  SELECT SINGLE *
    FROM ZFIWRT0008
    INTO WL_0008_AUX
     WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.

  IF SY-SUBRC IS NOT INITIAL.
    MOVE:  SY-UNAME TO WL_INPUT_0008-USNAM,
           SY-DATUM TO WL_INPUT_0008-DT_CRIACAO,
           SY-UZEIT TO WL_INPUT_0008-HR_CRIACAO.
    CLEAR WL_INPUT_0008-CH_REFERENCIA.
  ELSE.
    MOVE: WL_0008_AUX-USNAM      TO WL_INPUT_0008-USNAM,
          WL_0008_AUX-DT_CRIACAO TO WL_INPUT_0008-DT_CRIACAO,
          WL_0008_AUX-HR_CRIACAO TO WL_INPUT_0008-HR_CRIACAO,
          WL_0008_AUX-CH_REFERENCIA TO WL_INPUT_0008-CH_REFERENCIA.
  ENDIF.

*  DELETE FROM zfiwrt0008 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
** ZFIWRT0008
  MOVE : SY-MANDT             TO WL_INPUT_0008-MANDT,
         P_OPERACAO           TO WL_INPUT_0008-OPERACAO,
         P_BUKRS              TO WL_INPUT_0008-BUKRS,
         P_BRANCH             TO WL_INPUT_0008-BRANCH,
         P_PARVW              TO WL_INPUT_0008-PARVW,
         P_PARID              TO WL_INPUT_0008-PARID,
         WG_FISCAL-NFTYPE     TO WL_INPUT_0008-NFTYPE,
         WG_FISCAL-MOVE_PLANT TO WL_INPUT_0008-MOVE_PLANT,
         WG_FISCAL-MOVE_STLOC TO WL_INPUT_0008-MOVE_STLOC,
         WG_FISCAL-CTRL_ZRFL  TO WL_INPUT_0008-CTRL_ZRFL,
         WG_FISCAL-ACCESS_KEY   TO WL_INPUT_0008-ACCESS_KEY,
         WG_FISCAL-NR_ROMANEIO  TO WL_INPUT_0008-NR_ROMANEIO,
         WG_FISCAL-DOCREF       TO WL_INPUT_0008-DOCREF,
         WG_FISCAL-IMOBILIZADO  TO WL_INPUT_0008-IMOBILIZADO,
         WG_FISCAL-TP_MV_IMOB   TO WL_INPUT_0008-TP_MV_IMOB,
         WG_FISCAL-KOSTL        TO WL_INPUT_0008-KOSTL,
         WG_FISCAL-ZPESAGEM     TO WL_INPUT_0008-ZPESAGEM,
         WG_FISCAL-DIAS         TO WL_INPUT_0008-DIAS,
         WG_FISCAL-RETORNO      TO WL_INPUT_0008-RETORNO,
         WG_FISCAL-ENERGIA      TO WL_INPUT_0008-ENERGIA,
         WG_FISCAL-SERVICO      TO WL_INPUT_0008-SERVICO,
         WG_FISCAL-COMPLEMENTO  TO WL_INPUT_0008-COMPLEMENTO,
         WG_FISCAL-INCO1        TO WL_INPUT_0008-INCO1,
         WG_FISCAL-INCO2        TO WL_INPUT_0008-INCO2,
         WG_FISCAL-EBELN        TO WL_INPUT_0008-EBELN,
         WG_FISCAL-REFERENCIA   TO WL_INPUT_0008-REFERENCIA,
         WG_FISCAL-LM_ESTOQUE   TO WL_INPUT_0008-LM_ESTOQUE,
         WG_FISCAL-KONTO        TO WL_INPUT_0008-KONTO,
         WG_FISCAL-MOVE_MAT     TO WL_INPUT_0008-MOVE_MAT,
         WG_FISCAL-MOVE_BATCH   TO WL_INPUT_0008-MOVE_BATCH,
         WG_FISCAL-BKTXT        TO WL_INPUT_0008-BKTXT,
         WG_FISCAL-MTSNR        TO WL_INPUT_0008-MTSNR,

         WG_DIREITOS-CFOP       TO WL_INPUT_0008-CFOP,
         WG_DIREITOS-TAXLW1     TO WL_INPUT_0008-TAXLW1,
         WG_DIREITOS-TAXLW2     TO WL_INPUT_0008-TAXLW2,
         WG_DIREITOS-TAXLW4     TO WL_INPUT_0008-TAXLW4,
         WG_DIREITOS-TAXLW5     TO WL_INPUT_0008-TAXLW5,
         WG_DIREITOS-OPERTYP    TO WL_INPUT_0008-OPERTYP,
         WG_DIREITOS-TAXCODE    TO WL_INPUT_0008-TAXCODE,
         SY-UNAME               TO WL_INPUT_0008-USUARIO_ULT_MOD,
         SY-DATUM               TO WL_INPUT_0008-DT_ULT_MOD,
         SY-UZEIT               TO WL_INPUT_0008-HR_ULT_MOD,
         WG_DOCS-NFENUM         TO WL_INPUT_0008-NFENUM,
         WG_DOCS-BUDAT          TO WL_INPUT_0008-BUDAT,
         WG_DOCS-BLDAT          TO WL_INPUT_0008-BLDAT,
         WG_DOCS-SERIES         TO WL_INPUT_0008-SERIES,
         WG_DOCS-BELNR          TO WL_INPUT_0008-BELNR,
         WG_DOCS-MBLNR          TO WL_INPUT_0008-MBLNR,
         WG_DOCS-DOCNUM         TO WL_INPUT_0008-DOCNUM,
         WG_DOCS-LOC_CARREGA    TO WL_INPUT_0008-LOC_CARREGA, "CS2020001418 - CSB
         WL_0008_AUX-MJAHR      TO WL_INPUT_0008-MJAHR,
         WL_0008_AUX-LOEKZ      TO WL_INPUT_0008-LOEKZ,
         WL_0008_AUX-STATUS     TO WL_INPUT_0008-STATUS,
         WL_0008_AUX-OBJ_KEY    TO WL_INPUT_0008-OBJ_KEY,
         WL_INPUT_0008-SEQ_LCTO TO TL_INPUT_0019-SEQ_LCTO,
         WG_TRANSPORTE-LIFNR    TO TL_INPUT_0019-LIFNR,
         WG_TRANSPORTE-PLACA    TO TL_INPUT_0019-PLACA,
         WG_TRANSPORTE-ANZPK    TO TL_INPUT_0019-ANZPK,
         WG_TRANSPORTE-SHPUNT   TO TL_INPUT_0019-SHPUNT,
         WG_TRANSPORTE-NTGEW    TO TL_INPUT_0019-NTGEW,
         WG_TRANSPORTE-BRGEW    TO TL_INPUT_0019-BRGEW,
         WG_TRANSPORTE-UFPLACA  TO TL_INPUT_0019-UFPLACA,

         WG_TRANSPORTE-PLACA_CAR1 TO TL_INPUT_0019-PLACA_CAR1,
         WG_TRANSPORTE-PLACA_CAR2 TO TL_INPUT_0019-PLACA_CAR2,
         WG_TRANSPORTE-PLACA_CAR3 TO TL_INPUT_0019-PLACA_CAR2,
         WG_TRANSPORTE-MOTORISTA  TO TL_INPUT_0019-MOTORISTA.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = p_parid
*    IMPORTING
*      output = wl_input_0008-parid.

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    LOOP AT TG_EDITOR INTO WG_EDITOR.
      IF SY-TABIX EQ 1.
        WL_INPUT_0008-TXT_COMPL = WG_EDITOR-LINE.

      ELSEIF SY-TABIX GE 2.
        CONCATENATE WL_INPUT_0008-TXT_COMPL  WG_EDITOR-LINE INTO WL_INPUT_0008-TXT_COMPL SEPARATED BY SPACE.

      ENDIF.
    ENDLOOP.
  ENDIF.
***<
*DOCNUM
*BELNR
** ZFIWRT0009
  DELETE FROM ZFIWRT0009 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
  DELETE FROM ZFIWRT0010 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.

  "Documentos Referenciados
  LOOP AT TG_DOCREFS.
    MOVE:SY-MANDT               TO TL_INPUT_0020-MANDT,
         WL_INPUT_0008-SEQ_LCTO TO TL_INPUT_0020-SEQ_LCTO,
         TG_DOCREFS-DOCNUM      TO TL_INPUT_0020-DOCNUM.
    APPEND TL_INPUT_0020.
  ENDLOOP.

  "Verificação de Documento Anulado Referênciado
  "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
  "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
  READ TABLE TL_INPUT_0009 INDEX 1.

  IF TL_INPUT_0009-CFOP(4) EQ '1206' OR TL_INPUT_0009-CFOP(4) EQ '2206'.
    SELECT * INTO WL_0020
      FROM ZFIWRT0020
     WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
      "Anular Documento.
      CALL METHOD ZCL_CTE=>SET_ANULAR_CTE_SAIDA
        EXPORTING
          I_DOCNUM = WL_0020-DOCNUM
          I_ANULAR = ABAP_FALSE.
    ENDSELECT.
  ENDIF.

  DELETE FROM ZFIWRT0020 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.

  LOOP AT TG_ITENS.
    MOVE:SY-MANDT               TO TL_INPUT_0009-MANDT,
         WL_INPUT_0008-SEQ_LCTO TO TL_INPUT_0009-SEQ_LCTO,
*         sy-tabix               TO tl_input_0009-itmnum,
         TG_ITENS-ITMNUM        TO TL_INPUT_0009-ITMNUM,
         TG_ITENS-MATNR         TO TL_INPUT_0009-MATNR,
         TG_ITENS-CFOP          TO TL_INPUT_0009-CFOP,
         TG_ITENS-CHARG         TO TL_INPUT_0009-CHARG,
         TG_ITENS-NR_FASE       TO TL_INPUT_0009-NR_FASE, "// US-168932 WBARBOSA 20/05/2025
         TG_ITENS-MENGE         TO TL_INPUT_0009-MENGE,
         TG_ITENS-MEINS         TO TL_INPUT_0009-MEINS,
         TG_ITENS-NETPR         TO TL_INPUT_0009-NETPR,
         TG_ITENS-NETWR         TO TL_INPUT_0009-NETWR,
         WG_FISCAL-ITMTYP       TO TL_INPUT_0009-ITMTYP,
         TG_ITENS-WERKS         TO TL_INPUT_0009-BWKEY,
         TG_ITENS-LGORT         TO TL_INPUT_0009-LGORT,
         TG_ITENS-ANLN1         TO TL_INPUT_0009-ANLN1,
         TG_ITENS-ANLN2         TO TL_INPUT_0009-ANLN2,
         TG_ITENS-VBELN         TO TL_INPUT_0009-VBELN,
         TG_ITENS-POSNR         TO TL_INPUT_0009-POSNR,
*-CS2020001331 - 06.10.2021 - JT - inicio
         TG_ITENS-POSSUI_ICMS_ST TO TL_INPUT_0009-POSSUI_ICMS_ST,
*-CS2020001331 - 06.10.2021 - JT - fim
         TG_ITENS-NETDIS        TO TL_INPUT_0009-NETDIS,
         TG_ITENS-NETFRE        TO TL_INPUT_0009-NETFRE,
         TG_ITENS-NETINS        TO TL_INPUT_0009-NETINS,
         TG_ITENS-NETOTH        TO TL_INPUT_0009-NETOTH.

*    MULTIPLY tl_input_0009-itmnum BY 10.

*    CONDENSE tl_input_0009-itmnum NO-GAPS.
    APPEND TL_INPUT_0009.
***    ZFIWRT0010

*-CS2023000043-09.02.2023-#102019-JT-inicio
    IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S'.
      LOOP AT TG_IMPO_COMP WHERE ITMNUM = TG_ITENS-ITMNUM.
        MOVE: SY-MANDT               TO TL_INPUT_0010-MANDT,
              WL_INPUT_0008-SEQ_LCTO TO TL_INPUT_0010-SEQ_LCTO,
              TL_INPUT_0009-ITMNUM   TO TL_INPUT_0010-ITMNUM,
              TG_IMPO_COMP-TAXTYP    TO TL_INPUT_0010-TAXTYP,
              TG_IMPO_COMP-BASE      TO TL_INPUT_0010-BASE,
              TG_IMPO_COMP-RATE      TO TL_INPUT_0010-RATE,
              TG_IMPO_COMP-TAXVAL    TO TL_INPUT_0010-TAXVAL,
              TG_IMPO_COMP-EXCBAS    TO TL_INPUT_0010-EXCBAS,
              TG_IMPO_COMP-OTHBAS    TO TL_INPUT_0010-OTHBAS.

        APPEND TL_INPUT_0010.
      ENDLOOP.
    ELSE.
      PERFORM MONTA_IMPOSTOS TABLES TL_IMPO_AUX
                             USING SY-TABIX."tl_input_0009-itmnum.
      LOOP AT TL_IMPO_AUX.
        MOVE: SY-MANDT               TO TL_INPUT_0010-MANDT,
              WL_INPUT_0008-SEQ_LCTO TO TL_INPUT_0010-SEQ_LCTO,
              TL_INPUT_0009-ITMNUM   TO TL_INPUT_0010-ITMNUM,
              TL_IMPO_AUX-TAXTYP     TO TL_INPUT_0010-TAXTYP,
              TL_IMPO_AUX-BASE       TO TL_INPUT_0010-BASE,
              TL_IMPO_AUX-RATE       TO TL_INPUT_0010-RATE,
              TL_IMPO_AUX-TAXVAL     TO TL_INPUT_0010-TAXVAL,
              TL_IMPO_AUX-EXCBAS     TO TL_INPUT_0010-EXCBAS,
              TL_IMPO_AUX-OTHBAS     TO TL_INPUT_0010-OTHBAS.

        APPEND TL_INPUT_0010.
      ENDLOOP.
    ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

    CLEAR: TL_INPUT_0009.
  ENDLOOP.
** ZFIWRT0011
  DELETE FROM ZFIWRT0011 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
  PERFORM MONTA_CONTABIL.
  LOOP AT TG_CONTAB.
    MOVE: SY-MANDT                TO TL_INPUT_0011-MANDT,
          WL_INPUT_0008-SEQ_LCTO  TO TL_INPUT_0011-SEQ_LCTO,
          TG_CONTAB-BSCHL         TO TL_INPUT_0011-BSCHL,
          TG_CONTAB-HKONT         TO TL_INPUT_0011-HKONT,
          TG_CONTAB-TAXTYP        TO TL_INPUT_0011-TAXTYP,
          TG_CONTAB-WAERS_I       TO TL_INPUT_0011-WAERS_I,
          TG_CONTAB-DMBTR         TO TL_INPUT_0011-DMBTR,
          TG_CONTAB-CURHA	        TO TL_INPUT_0011-CURHA,
          TG_CONTAB-DMBE2         TO TL_INPUT_0011-DMBE2,
          TG_CONTAB-CURIN         TO TL_INPUT_0011-CURIN,
          TG_CONTAB-DMBE3         TO TL_INPUT_0011-DMBE3,
          TG_CONTAB-WAERS         TO TL_INPUT_0011-WAERS,
          TG_CONTAB-WRBTR         TO TL_INPUT_0011-WRBTR,
          TG_CONTAB-ARTNR         TO TL_INPUT_0011-ARTNR,
          TG_CONTAB-ESTORNO       TO TL_INPUT_0011-ESTORNO,
          TG_CONTAB-ZLSCH         TO TL_INPUT_0011-ZLSCH,
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          TG_CONTAB-TAXA_JUROS    TO TL_INPUT_0011-TAXA_JUROS,
          TG_CONTAB-TAXA_MULTA    TO TL_INPUT_0011-TAXA_MULTA,
          TG_CONTAB-HBKID         TO TL_INPUT_0011-HBKID,
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          TG_CONTAB-ZFBDT         TO TL_INPUT_0011-ZFBDT,
          TG_CONTAB-KOSTL         TO TL_INPUT_0011-KOSTL,
          TG_CONTAB-UMSKZ         TO TL_INPUT_0011-UMSKZ,
          TG_CONTAB-VBUND         TO TL_INPUT_0011-VBUND.

    TL_INPUT_0011-BUZEI  = SY-TABIX.

    APPEND TL_INPUT_0011.
    CLEAR: TL_INPUT_0011.
  ENDLOOP.

** ZFIWRT0012
  DELETE FROM ZFIWRT0012 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
  LOOP AT TG_MOVEST.
    MOVE: SY-MANDT                TO  TL_INPUT_0012-MANDT,
          WL_INPUT_0008-SEQ_LCTO  TO  TL_INPUT_0012-SEQ_LCTO,
          TG_MOVEST-BWART         TO  TL_INPUT_0012-BWART,
          TG_MOVEST-TCODE         TO  TL_INPUT_0012-TCODE,
          TG_MOVEST-MWSKZ1        TO  TL_INPUT_0012-MWSKZ1,
          TG_MOVEST-ESTORNO       TO  TL_INPUT_0012-ESTORNO.

    APPEND TL_INPUT_0012.
    CLEAR: TL_INPUT_0012.
  ENDLOOP.

** ZFIWRT0013
  DELETE FROM ZFIWRT0013 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
  LOOP AT TG_MENSAGEMS_AUX.
    MOVE: SY-MANDT                 TO TL_INPUT_0013-MANDT,
          WL_INPUT_0008-SEQ_LCTO   TO TL_INPUT_0013-SEQ_LCTO,
          TG_MENSAGEMS_AUX-SEQNUM  TO TL_INPUT_0013-SEQNUM,
          TG_MENSAGEMS_AUX-LINNUM  TO TL_INPUT_0013-LINNUM,
          TG_MENSAGEMS_AUX-MESSAGE TO TL_INPUT_0013-MESSAGE.

    APPEND TL_INPUT_0013.
    CLEAR: TL_INPUT_0013.
  ENDLOOP.

** ZFIWRT0015
  DELETE FROM ZFIWRT0015 WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
  LOOP AT TG_PARC.
    MOVE: SY-MANDT                 TO TL_INPUT_0015-MANDT,
          WL_INPUT_0008-SEQ_LCTO   TO TL_INPUT_0015-SEQ_LCTO,
          TG_PARC-PARVW            TO TL_INPUT_0015-PARVW,
          TG_PARC-PARID            TO TL_INPUT_0015-PARID.

    APPEND TL_INPUT_0015.
    CLEAR: TL_INPUT_0015.
  ENDLOOP.

  LOOP AT TL_INPUT_0011 INTO DATA(WA_11) WHERE KOSTL IS NOT INITIAL.
    WL_INPUT_0008-KOSTL = WA_11-KOSTL.
  ENDLOOP.


  SELECT SINGLE *
    FROM J_1BAA INTO @DATA(_WL_J_1BAA)
   WHERE NFTYPE = @WL_INPUT_0008-NFTYPE.

  IF SY-SUBRC EQ 0.
    WL_INPUT_0008-FORM = _WL_J_1BAA-FORM.
  ENDIF.

  IF ( VINICIOU_LCTO_ZNFW0009 IS NOT INITIAL ) AND
     ( VLANCAMENTO_ZNFW0009   IS NOT INITIAL ).
    WL_INPUT_0008-TCODE_ORG = 'ZNFW0009'.
  ENDIF.

  MODIFY ZFIWRT0008 FROM WL_INPUT_0008.
  MODIFY ZFIWRT0009 FROM TABLE TL_INPUT_0009.
  MODIFY ZFIWRT0010 FROM TABLE TL_INPUT_0010.
  MODIFY ZFIWRT0011 FROM TABLE TL_INPUT_0011.
  MODIFY ZFIWRT0012 FROM TABLE TL_INPUT_0012.
  MODIFY ZFIWRT0013 FROM TABLE TL_INPUT_0013.
  MODIFY ZFIWRT0015 FROM TABLE TL_INPUT_0015.
  MODIFY ZFIWRT0020 FROM TABLE TL_INPUT_0020.
  MODIFY ZFIWRT0019 FROM TL_INPUT_0019.

  COMMIT WORK AND WAIT.

  IF P_OPERACAO = 561.
    SELECT * INTO TABLE LT_0020
      FROM ZFIWRT0020
     WHERE SEQ_LCTO EQ WL_INPUT_0008-SEQ_LCTO.
    IF LT_0020[] IS INITIAL.
      LT_0020[] = TL_INPUT_0020[].
    ENDIF.
    IF LT_0020[] IS NOT INITIAL.
      SELECT * INTO TABLE LT_LIN
        FROM J_1BNFLIN
        FOR ALL ENTRIES IN LT_0020
        WHERE DOCNUM = LT_0020-DOCNUM.
      IF SY-SUBRC = 0.
        DELETE LT_LIN WHERE REFKEY IS INITIAL.

*---> 04/07/2023 - Migração S4 - WS
        SORT LT_LIN BY REFKEY.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM LT_LIN COMPARING REFKEY.
        IF LT_LIN[] IS NOT INITIAL.
          SELECT * INTO TABLE LT_0094
            FROM ZSDT0094
            FOR ALL ENTRIES IN LT_LIN
            WHERE NRO_SOL_OV = LT_LIN-REFKEY(10)
              AND ESTORNO = 0.
          IF LT_0094[] IS NOT INITIAL.
            SORT LT_0094 BY NRO_SOL_OV.
            DELETE LT_0094 WHERE NRO_SOL_OV IS INITIAL.

*---> 04/07/2023 - Migração S4 - WS
            SORT LT_0094 BY NRO_SOL_OV.
*<--- 04/07/2023 - Migração S4 - WS
            DELETE ADJACENT DUPLICATES FROM LT_0094 COMPARING NRO_SOL_OV.
            LOOP AT LT_0094 INTO LS_0094.
              OBJ_TX_CURVA_DB->ESTORNO_AQV( LS_0094-NRO_SOL_OV ).
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  MESSAGE S836(SD) WITH 'Lançamento' WL_INPUT_0008-SEQ_LCTO ', criado/modificado com sucesso!'.


ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_CONTABIL .
  DATA: TL_IMPO_AUX LIKE TABLE OF TG_IMPO WITH HEADER LINE,
        TL_IMPO     LIKE TABLE OF TG_IMPO WITH HEADER LINE,
        WL_TABIX    TYPE SY-TABIX,
        WL_CONTAB   LIKE LINE OF TG_CONTAB,
        WA_TKA02    TYPE TKA02,
        V_COUNT     TYPE I,
        VG_BSEG(1),
        T_HKONT     TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
        V_PARID     TYPE ZFIWRT0008-PARID,
        V_KOART     TYPE TBSL-KOART,
        V_DMBTR     TYPE ZFIWRT0011-DMBTR,
        V_VBUND     TYPE BSEG-VBUND.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'CONTAS_EC-CS'
    TABLES
      SET_VALUES    = T_HKONT
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  SELECT SINGLE *
    FROM TKA02
    INTO WA_TKA02
    WHERE BUKRS = P_BUKRS.

  SELECT SINGLE *
     FROM ZFIWRT0001
     INTO W_ZFIWRT0001
      WHERE OPERACAO EQ P_OPERACAO.

  CONCATENATE 'CE4' WA_TKA02-KOKRS '_ACCT' INTO DATA(TABCO1).
  CONCATENATE 'CE4' WA_TKA02-KOKRS         INTO DATA(TABCO2).

  REFRESH: TL_IMPO, TL_IMPO_AUX.
  CLEAR: TL_IMPO, TL_IMPO_AUX, V_KOART.

  LOOP AT TG_CONTAB.
    READ TABLE TG_TBSL
    WITH KEY BSCHL = TG_CONTAB-BSCHL.
    IF TG_TBSL-KOART = 'D' OR TG_TBSL-KOART = 'K'.
      V_KOART = TG_TBSL-KOART.
    ENDIF.
    MOVE: 0 TO TG_CONTAB-DMBTR.
    MODIFY TG_CONTAB.
  ENDLOOP.


  LOOP AT TG_ITENS.
    PERFORM MONTA_IMPOSTOS TABLES TL_IMPO_AUX
                           USING SY-TABIX.

    LOOP AT TL_IMPO_AUX.
      MOVE-CORRESPONDING TL_IMPO_AUX TO TL_IMPO.
      COLLECT TL_IMPO.
    ENDLOOP.
    REFRESH: TL_IMPO_AUX.
  ENDLOOP.

*-CS2020001331 - 06.10.2021 - JT - inicio
  READ TABLE TL_IMPO WITH KEY TAXTYP = C_ICOP.
  IF SY-SUBRC = 0.
    LOOP AT TG_CONTAB.
      TG_CONTAB-TAXTYP = C_ICOP.
      MODIFY TG_CONTAB INDEX SY-TABIX.
    ENDLOOP.
  ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

  LOOP AT TG_CONTAB.
    WL_TABIX = SY-TABIX.
    READ TABLE TG_TBSL
    WITH KEY BSCHL = TG_CONTAB-BSCHL.

    "Sociedade Parceira
    CLEAR: TG_CONTAB-VBUND, V_VBUND.
    READ TABLE T_HKONT WITH KEY FROM = TG_CONTAB-HKONT.
    IF SY-SUBRC = 0.
      IF V_KOART = 'D'.
        SELECT SINGLE VBUND INTO V_VBUND FROM KNA1
          WHERE KUNNR = P_PARID "soc parceira do emissor
          AND   KTOKD IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
      ELSEIF V_KOART = 'K'.
        SELECT SINGLE VBUND INTO V_VBUND FROM LFA1
          WHERE LIFNR = P_PARID "soc parceira do emissor
          AND   KTOKK IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
      ENDIF.
      TG_CONTAB-VBUND = V_VBUND.
      MODIFY TG_CONTAB INDEX WL_TABIX TRANSPORTING VBUND.
    ENDIF.
    "

    IF TG_CONTAB-TAXTYP IS INITIAL.
      IF WG_FISCAL-COMPLEMENTO = 'S' OR W_ZFIWRT0001-COMPLEMENT_ICMS = 'S'.
        LOOP AT TL_IMPO
          WHERE TAXTYP EQ C_ICM3.
          IF TG_TBSL-SHKZG EQ C_H.
            SUBTRACT  TL_IMPO-TAXVAL FROM TG_CONTAB-DMBTR.
          ELSE.
            ADD TL_IMPO-TAXVAL TO TG_CONTAB-DMBTR.
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY TG_CONTAB INDEX WL_TABIX.
      IF WG_FISCAL-ENERGIA EQ C_N .
        LOOP AT TG_ITENS.
          IF TG_TBSL-SHKZG EQ C_H.
            SUBTRACT  TG_ITENS-NETWR FROM TG_CONTAB-DMBTR.
*          AT LAST.
*            MULTIPLY tg_contab-dmbtr BY -1.
*          ENDAT.
          ELSE.
            ADD TG_ITENS-NETWR TO TG_CONTAB-DMBTR.
          ENDIF.
        ENDLOOP.
      ELSEIF WG_FISCAL-ENERGIA EQ C_S.
        LOOP AT TL_IMPO
          WHERE TAXTYP EQ C_ICS1.
          IF TG_TBSL-SHKZG EQ C_H.

*====== "Inicio USER STORY 81382  "Anderson Oenning
*            SUBTRACT  tl_impo-base FROM tg_contab-dmbtr.
            CLEAR: V_DMBTR.
            V_DMBTR = ( TG_ITENS-NETWR + TL_IMPO-TAXVAL ). "Valor total do item + imposto
            SUBTRACT  V_DMBTR FROM TG_CONTAB-DMBTR.

          ELSE.
*            ADD tl_impo-base TO tg_contab-dmbtr.
            CLEAR: V_DMBTR.
            V_DMBTR = ( TG_ITENS-NETWR + TL_IMPO-TAXVAL ). "Valor total do item + imposto
            ADD V_DMBTR TO TG_CONTAB-DMBTR.
*            ADD tl_impo-base TO tg_contab-dmbtr.
*====== "Inicio USER STORY 81382  "Anderson Oenning
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY TG_CONTAB INDEX WL_TABIX.
    ELSE.
      READ TABLE TL_IMPO
        WITH KEY TAXTYP = TG_CONTAB-TAXTYP.
      IF SY-SUBRC IS INITIAL.
        IF TG_TBSL-SHKZG EQ C_H.
          MOVE: TL_IMPO-TAXVAL TO TG_CONTAB-DMBTR.
          MULTIPLY TG_CONTAB-DMBTR BY -1.
        ELSE.
          MOVE: TL_IMPO-TAXVAL TO TG_CONTAB-DMBTR.
        ENDIF.
        MODIFY TG_CONTAB INDEX WL_TABIX.
      ENDIF.

    ENDIF.

    CLEAR: WL_TABIX, TL_IMPO, TG_TBSL.
  ENDLOOP.

  CLEAR VG_BSEG.
  LOOP AT TG_DOCREFS INTO DATA(WA_DOCREFS).

    SELECT * INTO TABLE @DATA(ITENS_DOCUMENTO)
      FROM J_1BNFLIN
     WHERE DOCNUM EQ @WA_DOCREFS-DOCNUM.

    SELECT SINGLE PARID
      FROM J_1BNFDOC
      INTO V_PARID
      WHERE DOCNUM EQ WA_DOCREFS-DOCNUM.

    LOOP AT ITENS_DOCUMENTO INTO DATA(WA_ITENS_DOCUMENTO).

*-CS2020001331 - 06.10.2021 - JT - inicio
      IF WA_ITENS_DOCUMENTO-REFKEY IS NOT INITIAL.
        CONCATENATE WA_ITENS_DOCUMENTO-REFKEY '%' INTO WA_ITENS_DOCUMENTO-REFKEY.

        SELECT * INTO TABLE @DATA(IT_BKPF)
          FROM BKPF
         WHERE AWKEY LIKE @WA_ITENS_DOCUMENTO-REFKEY.
      ELSE.
        SY-SUBRC = 4.
      ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

      IF SY-SUBRC IS INITIAL.

* ---> S4 Migration - 19/06/2023 - JS
*        SELECT * INTO TABLE @DATA(it_bseg)
*          FROM bseg
*           FOR ALL ENTRIES IN @it_bkpf
*         WHERE bukrs EQ @it_bkpf-bukrs
*           AND belnr EQ @it_bkpf-belnr
*           AND gjahr EQ @it_bkpf-gjahr.
        DATA IT_BSEG TYPE TABLE OF BSEG.

        CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
          EXPORTING
            IT_FOR_ALL_ENTRIES = IT_BKPF
            I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
          IMPORTING
            ET_BSEG            = IT_BSEG
          EXCEPTIONS
            NOT_FOUND          = 1.

        IF SY-SUBRC = 0 AND LINES( IT_BSEG ) > 0.
          SY-DBCNT = LINES( IT_BSEG ).
        ELSE.
          SY-SUBRC = 4.
          SY-DBCNT = 0.
        ENDIF.
* <--- S4 Migration - 19/06/2023 - JS

        SELECT * INTO TABLE @DATA(IT_TBSL)
          FROM TBSL
           FOR ALL ENTRIES IN @IT_BSEG
         WHERE BSCHL EQ @IT_BSEG-BSCHL.

        SORT IT_TBSL BY BSCHL.
        CLEAR V_KOART.
        LOOP AT IT_BSEG INTO DATA(WA_BSEG2).
          READ TABLE IT_TBSL INTO DATA(WA_TBSL2) WITH KEY BSCHL = WA_BSEG2-BSCHL BINARY SEARCH.
          IF WA_TBSL2-KOART = 'D' OR WA_TBSL2-KOART = 'K'.
            V_KOART = WA_TBSL2-KOART.
          ENDIF.
        ENDLOOP.


        LOOP AT IT_BSEG INTO DATA(WA_BSEG).
          VG_BSEG = 'X'.
          CLEAR: WL_CONTAB.
          READ TABLE IT_BKPF INTO DATA(WA_BKPF) WITH KEY BUKRS = WA_BSEG-BUKRS
                                                         BELNR = WA_BSEG-BELNR
                                                         GJAHR = WA_BSEG-GJAHR.
          WL_CONTAB-WAERS   = WA_BKPF-WAERS.
          WL_CONTAB-WAERS_I = WA_BKPF-HWAER.
          WL_CONTAB-CURHA	  = WA_BKPF-HWAE2.
          WL_CONTAB-CURIN	  = WA_BKPF-HWAE3.
          READ TABLE IT_TBSL INTO DATA(WA_TBSL) WITH KEY BSCHL = WA_BSEG-BSCHL BINARY SEARCH.

          CASE WA_TBSL-KOART.
            WHEN 'D'.
              WL_CONTAB-HKONT  = WA_BSEG-KUNNR.
            WHEN 'K'.
              WL_CONTAB-HKONT  = WA_BSEG-LIFNR.
            WHEN OTHERS.
              WL_CONTAB-HKONT  = WA_BSEG-HKONT.
          ENDCASE.

          WL_CONTAB-BSCHL  = WA_TBSL-STBSL.
          "WL_CONTAB-ZLSCH  = 'P'.
*
          IF WA_TBSL-SHKZG EQ C_H.
            ADD WA_BSEG-DMBTR TO WL_CONTAB-DMBTR.
            WL_CONTAB-DMBTR = WL_CONTAB-DMBTR * -1.
            "
            ADD WA_BSEG-DMBE2 TO WL_CONTAB-DMBE2.
            MULTIPLY WL_CONTAB-DMBE2 BY -1.
            "
            ADD WA_BSEG-DMBE3 TO WL_CONTAB-DMBE3.
            MULTIPLY WL_CONTAB-DMBE3 BY -1.

            ADD WA_BSEG-WRBTR TO WL_CONTAB-WRBTR.
            MULTIPLY WL_CONTAB-WRBTR BY -1.
          ELSE.
            ADD WA_BSEG-DMBTR TO WL_CONTAB-DMBTR.
            ADD WA_BSEG-DMBE2 TO WL_CONTAB-DMBE2.
            ADD WA_BSEG-DMBE3 TO WL_CONTAB-DMBE3.
            ADD WA_BSEG-WRBTR TO WL_CONTAB-WRBTR.
          ENDIF.

          CLEAR WL_CONTAB-ARTNR.
          IF WA_BSEG-PAOBJNR IS NOT INITIAL AND WA_BSEG-PAOBJNR GT 0.
            CLEAR V_COUNT.
            SELECT COUNT(*) FROM  DD02L
                INTO    V_COUNT
                WHERE   TABNAME = TABCO1
                AND     AS4LOCAL  = 'A'.
            IF V_COUNT GT 0.
              SELECT SINGLE ARTNR
                FROM (TABCO1) "CE4MAGI_ACCT
                INTO WL_CONTAB-ARTNR
              WHERE PAOBJNR = WA_BSEG-PAOBJNR.
            ENDIF.

            IF SY-SUBRC NE 0 OR V_COUNT = 0.
              CLEAR  V_COUNT.
              SELECT COUNT(*) FROM  DD02L
                INTO    V_COUNT
                WHERE   TABNAME = TABCO2
                AND     AS4LOCAL  = 'A'.
              IF V_COUNT GT 0.
                SELECT SINGLE ARTNR
                  FROM (TABCO2)
                  INTO WL_CONTAB-ARTNR
                WHERE PAOBJNR = WA_BSEG-PAOBJNR.
              ENDIF.
            ENDIF.
          ENDIF.
          "
          WL_CONTAB-TAXTYP  = WA_BSEG-TAXPS.
          "WL_CONTAB-ESTORNO = ABAP_TRUE.
          "WL_CONTAB-NEWBW  = .
          "WL_CONTAB-ZLSCH  = .
          WL_CONTAB-ZFBDT   = WA_BSEG-ZFBDT.
          WL_CONTAB-KOSTL   = WA_BSEG-KOSTL.
          WL_CONTAB-UMSKZ   = WA_BSEG-UMSKS.
          "Sociedade Parceira
          CLEAR: TG_CONTAB-VBUND, V_VBUND.
          READ TABLE T_HKONT WITH KEY FROM = WL_CONTAB-HKONT.
          IF SY-SUBRC = 0.
            IF V_KOART = 'D'.
              SELECT SINGLE VBUND INTO V_VBUND FROM KNA1
                WHERE KUNNR = V_PARID "soc parceira do emissor
                AND   KTOKD IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
              WL_CONTAB-VBUND = V_VBUND.
            ELSEIF V_KOART = 'K'.
              SELECT SINGLE VBUND INTO V_VBUND FROM LFA1
                WHERE LIFNR = V_PARID "soc parceira do emissor
                AND   KTOKK IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
              WL_CONTAB-VBUND = V_VBUND.
            ENDIF.
          ENDIF.
          "
          APPEND WL_CONTAB TO TG_CONTAB.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF VG_BSEG = 'X'.
    DELETE TG_CONTAB WHERE DMBTR = 0.
  ENDIF.

  SORT TG_CONTAB BY TAXTYP BSCHL.
ENDFORM.                    " MONTA_CONTABIL
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.

  DATA: WA_SETLEAF         TYPE SETLEAF,
        WA_ZFIWRT0026      TYPE ZFIWRT0026,
        VG_CONTINGENCIA(1).
  CLEAR VG_CONTINGENCIA.
  SELECT COUNT( * )
             FROM TVARVC
             WHERE NAME = 'FAT_CONTINGENCIA_GOLIVE_US'
             AND LOW  = SY-UNAME.
  IF SY-SUBRC = 0.
    VG_CONTINGENCIA = 'X'.
  ENDIF.



  CLEAR: WL_1BAA.
  SELECT SINGLE *
    FROM J_1BAA
    INTO WL_1BAA
     WHERE NFTYPE EQ WG_FISCAL-NFTYPE.

  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1 ).
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

*  CLEAR: wa_setleaf.
*  SELECT SINGLE *
*    FROM setleaf
*    INTO wa_setleaf
*   WHERE setname = 'VF01_USUARIO'
*     AND valfrom = sy-uname.
  "AUTHORITY-CHECK OBJECT 'ZNFW_BUDAT' ID 'ACTVT' FIELD '03'.

  CLEAR: WA_ZFIWRT0026.
  SELECT SINGLE *
    FROM ZFIWRT0026
    INTO WA_ZFIWRT0026
  WHERE USNAME EQ  SY-UNAME.

  IF ( SY-SUBRC IS INITIAL ).
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'WG_DOCS-BUDAT' ) AND ( WG_DOCS-BUDAT IS NOT INITIAL ).
        SCREEN-INPUT     = C_1.
        SCREEN-INVISIBLE = C_0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF WG_DOCS-DOCNUM IS INITIAL.
    READ TABLE TG_FIELDS
      WITH KEY GROUP1 = 'GR1'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-ZPESAGEM NE '01'.
        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'WG_FISCAL-NR_ROMANEIO'.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
      "ALRS
      LOOP AT SCREEN.
        IF  WG_FISCAL-LM_ESTOQUE = 'S'.
          IF SCREEN-NAME EQ 'WG_FISCAL-INCO1'      OR
             SCREEN-NAME EQ 'WG_FISCAL-INCO2'      OR
             SCREEN-NAME EQ 'WG_FISCAL-DOCREF'     OR
             SCREEN-NAME EQ 'WG_FISCAL-REFERENCIA'.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

        IF SCREEN-NAME EQ 'WG_FISCAL-TP_MV_IMOB' OR
           SCREEN-NAME EQ 'WG_FISCAL-KOSTL' OR
           SCREEN-NAME EQ 'TXTCCUSTO'.
          IF WG_FISCAL-IMOBILIZADO EQ 'S' .
            "SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            "SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.
          IF SCREEN-NAME EQ 'WG_FISCAL-KOSTL' OR
             SCREEN-NAME EQ 'TXTCCUSTO'.
            IF WG_FISCAL-TP_MV_IMOB = 'T'.  " transferência
              SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
              SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
            ELSE.
              SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
              SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.

        "04.10.2018
        IF SCREEN-NAME EQ 'WG_FISCAL-ACCESS_KEY' OR
           SCREEN-NAME EQ 'TXT_CHAVE_ACESSO'.

          "18.02.2019 CS2018003078
          IF VLANCAMENTO_ZNFW0009 IS NOT INITIAL.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.

*          IF ( WL_1BAA-NFE    EQ 'X'     ) AND
*             ( WL_1BAA-FORM   IS INITIAL ) AND
*             ( WL_1BAA-DIRECT EQ '2' OR WL_1BAA-NFTYPE EQ 'YI' ).
*            SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
*            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
*          ELSE.
*            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
*            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
*          ENDIF.
          MODIFY SCREEN.
        ENDIF.


        "11.10.2018
        IF SCREEN-NAME EQ 'WG_FISCAL-KONTO'.
          IF WG_FISCAL-LM_ESTOQUE = 'S'. "alrs
            SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF WG_FISCAL-RETORNO EQ 'N'.
        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'WG_FISCAL-DOCREF'.
            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.


      IF ( WG_FISCAL-AVISO_REC EQ 'S' ) OR
         ( WG_FISCAL-VBELN IS INITIAL ) AND "Não é lançamento sobre Ordem Venda
         ( VINICIOU_LCTO_ZNFW0009 IS NOT INITIAL ).

        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'WG_FISCAL-EBELN'.
            SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF WL_1BAA-FORM IS NOT INITIAL.
        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'WG_DOCS-NFENUM'
          OR SCREEN-NAME EQ 'WG_DOCS-SERIES'
          OR SCREEN-NAME EQ 'WG_DOCS-BLDAT'
          OR SCREEN-NAME EQ 'WG_DOCS-BUDAT'.
            IF SCREEN-NAME NE 'WG_DOCS-BUDAT' AND VG_CONTINGENCIA NE 'X'.
              SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
              SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
            IF WG_FISCAL-COMPLEMENTO = 'S' AND ( SCREEN-NAME EQ 'WG_DOCS-BLDAT' OR SCREEN-NAME EQ 'WG_DOCS-BUDAT' ).
              SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
              SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
            MODIFY SCREEN.
          ENDIF.

        ENDLOOP.

        IF VLANCAMENTO_ZNFW0009 EQ ABAP_FALSE.
          CLEAR: WG_DOCS-NFENUM, WG_DOCS-SERIES.
          IF WG_FISCAL-COMPLEMENTO NE 'S' AND VG_CONTINGENCIA NE 'X'..
            WG_DOCS-BUDAT = SY-DATUM.
            WG_DOCS-BLDAT = SY-DATUM.
          ENDIF.
        ENDIF.
      ELSE.

        LOOP AT SCREEN.
          IF WG_FISCAL-COMPLEMENTO = 'S' AND ( SCREEN-NAME EQ 'WG_DOCS-BLDAT' OR SCREEN-NAME EQ 'WG_DOCS-BUDAT' ).
            SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ELSE.

    ENDIF.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'WG_DOCS-NFENUM'
      OR SCREEN-NAME EQ 'WG_DOCS-SERIES'
      OR SCREEN-NAME EQ 'WG_DOCS-BLDAT'
      OR SCREEN-NAME EQ 'WG_DOCS-BUDAT'
      OR SCREEN-NAME EQ 'WG_FISCAL-NR_ROMANEIO'
      OR SCREEN-NAME EQ 'WG_FISCAL-DOCREF'
      OR SCREEN-NAME EQ 'P_OPERACAO'
      OR SCREEN-NAME EQ 'P_BUKRS'
      OR SCREEN-NAME EQ 'P_BRANCH'
      OR SCREEN-NAME EQ 'P_PARID'.
        IF SCREEN-NAME NE 'WG_DOCS-BUDAT'.
          SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
          SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.
        IF WG_FISCAL-COMPLEMENTO = 'S' AND ( SCREEN-NAME EQ 'WG_DOCS-BLDAT' OR SCREEN-NAME EQ 'WG_DOCS-BUDAT' ).
          SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
          SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF  SCREEN-NAME EQ 'PBADD'
      OR SCREEN-NAME EQ 'PBDEL'
      OR SCREEN-NAME EQ 'PBSAVE'.
        SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
        SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF WG_DOCS-DOCNUM IS INITIAL.
    IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S' AND ( WG_ACAO = C_MODIF OR WG_ACAO = C_ADD ).
      LOOP AT SCREEN.
        IF SCREEN-NAME EQ 'WG_DIREITOS-TAXLW1' OR
           SCREEN-NAME EQ 'WG_DIREITOS-TAXLW2' OR
           SCREEN-NAME EQ 'WG_DIREITOS-TAXLW4' OR
           SCREEN-NAME EQ 'WG_DIREITOS-TAXLW5'.
          SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

* US #163043 - MMSILVA - 18.03.2025 - Inicio
  SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WG_FISCAL-NFTYPE.
  IF WL_1BAA-DIRECT EQ '1' AND WG_DOCS-DOCNUM IS INITIAL AND WG_ACAO = C_ADD.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'P_CHAVE_ACESSO'.
        SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
        SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF WL_1BAA-DIRECT EQ '1' AND ( WG_DOCS-DOCNUM IS NOT INITIAL OR WG_ACAO = ABAP_FALSE ).
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'P_CHAVE_ACESSO'.
        SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
        SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'P_CHAVE_ACESSO'.
        SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
        SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
* US #163043 - MMSILVA - 18.03.2025 - Fim

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.
** Depois de tratados os valores da tabela tem que ser eliminados para um proximo tratamento.
*  REFRESH tg_fields.
  CLEAR: TG_FIELDS.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0112   text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SEQ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_SEQ INPUT.
  DATA: BEGIN OF TL_SEQ_LANC OCCURS 0,
          SEQ_LCTO  TYPE ZFIWRT0008-SEQ_LCTO,
          OPERACAO  TYPE ZFIWRT0008-OPERACAO,
          BUKRS     TYPE ZFIWRT0008-BUKRS,
          BRANCH    TYPE ZFIWRT0008-BRANCH,
          PARVW     TYPE ZFIWRT0008-PARVW,
          PARID     TYPE ZFIWRT0008-PARID,
          DOCNUM    TYPE ZFIWRT0008-DOCNUM,
          NRONF     TYPE ZFIWRT0008-NFENUM,
          DESCRICAO TYPE ZFIWRT0001-DESCRICAO,
          NAME      TYPE LFA1-NAME1,
        END OF TL_SEQ_LANC.

  DATA: TL_LFA1   TYPE TABLE OF LFA1 WITH HEADER LINE,
        TL_KNA1   TYPE TABLE OF KNA1 WITH HEADER LINE,
        TL_0001   TYPE TABLE OF ZFIWRT0001 WITH HEADER LINE,
        TL_BNFDOC TYPE TABLE OF J_1BNFDOC WITH HEADER LINE.

  REFRESH: TL_SEQ_LANC, T_FIELDTAB, TL_BNFDOC, TL_LFA1, TL_KNA1, TL_0001.
  CLEAR:   TL_SEQ_LANC, T_FIELDTAB, TL_BNFDOC, TL_LFA1, TL_KNA1, TL_0001.

  SELECT SEQ_LCTO OPERACAO BUKRS BRANCH PARVW PARID
         DOCNUM NFENUM
    FROM ZFIWRT0008
    INTO TABLE TL_SEQ_LANC.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM LFA1
       INTO TABLE TL_LFA1
       FOR ALL ENTRIES IN TL_SEQ_LANC
        WHERE LIFNR EQ TL_SEQ_LANC-PARID.

    SELECT *
      FROM KNA1
      INTO TABLE TL_KNA1
       FOR ALL ENTRIES IN TL_SEQ_LANC
        WHERE KUNNR EQ TL_SEQ_LANC-PARID.

    SELECT *
      FROM ZFIWRT0001
      INTO TABLE TL_0001
       FOR ALL ENTRIES IN TL_SEQ_LANC
        WHERE OPERACAO EQ TL_SEQ_LANC-OPERACAO.

    SELECT *
      FROM J_1BNFDOC
      INTO TABLE TL_BNFDOC
       FOR ALL ENTRIES IN TL_SEQ_LANC
       WHERE DOCNUM EQ TL_SEQ_LANC-DOCNUM.

  ENDIF.

  SORT: TL_KNA1 BY KUNNR,
        TL_LFA1 BY LIFNR,
        TL_0001 BY OPERACAO,
        TL_BNFDOC BY DOCNUM.

  LOOP AT TL_SEQ_LANC.
    IF TL_SEQ_LANC-PARVW EQ C_AG.
      READ TABLE TL_KNA1
        WITH KEY KUNNR = TL_SEQ_LANC-PARID
                 BINARY SEARCH.
      MOVE: TL_KNA1-NAME1 TO TL_SEQ_LANC-NAME.
    ELSEIF TL_SEQ_LANC-PARVW EQ C_LF
        OR TL_SEQ_LANC-PARVW EQ C_BR.
      READ TABLE TL_LFA1
        WITH KEY LIFNR = TL_SEQ_LANC-PARID
                 BINARY SEARCH.
      MOVE: TL_LFA1-NAME1 TO TL_SEQ_LANC-NAME.
    ENDIF.
    READ TABLE TL_0001
      WITH KEY OPERACAO = TL_SEQ_LANC-OPERACAO
                   BINARY SEARCH.

    READ TABLE TL_BNFDOC
      WITH KEY DOCNUM = TL_SEQ_LANC
               BINARY SEARCH.

    MOVE: TL_0001-DESCRICAO TO TL_SEQ_LANC-DESCRICAO.

    IF TL_BNFDOC-NFENUM IS INITIAL.
      MOVE: TL_BNFDOC-NFNUM TO TL_SEQ_LANC-NRONF.
    ELSE.
      MOVE: TL_BNFDOC-NFENUM TO TL_SEQ_LANC-NRONF.
    ENDIF.
    MODIFY TL_SEQ_LANC.
    CLEAR: TL_BNFDOC, TL_LFA1, TL_KNA1, TL_0001.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SEQ_LCTO'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'P_SEQ_LCTO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_SEQ_LANC
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_SEQ  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS_DOC .
  DATA: WL_0001       TYPE ZFIWRT0001,
        WL_0008       TYPE ZFIWRT0008,
        TL_0009       TYPE TABLE OF ZFIWRT0009 WITH HEADER LINE,
        TL_0010       TYPE TABLE OF ZFIWRT0010 WITH HEADER LINE,
        TL_0011       TYPE TABLE OF ZFIWRT0011 WITH HEADER LINE,
        TL_0012       TYPE TABLE OF ZFIWRT0012 WITH HEADER LINE,
        TL_0013       TYPE TABLE OF ZFIWRT0013 WITH HEADER LINE,
        TL_0015       TYPE TABLE OF ZFIWRT0015 WITH HEADER LINE,
        TL_0020       TYPE TABLE OF ZFIWRT0020 WITH HEADER LINE,
        TL_1BAJ       TYPE TABLE OF J_1BAJ     WITH HEADER LINE,
        TL_1BAJT      TYPE TABLE OF J_1BAJT    WITH HEADER LINE,
        TL_TBSL       TYPE TABLE OF TBSL       WITH HEADER LINE,
        TL_SKAT       TYPE TABLE OF SKAT       WITH HEADER LINE,
        TL_CSKB       TYPE TABLE OF CSKB       WITH HEADER LINE,
        TL_MAKT       TYPE TABLE OF MAKT       WITH HEADER LINE,
        TL_0005       TYPE TABLE OF ZFIWRT0005 WITH HEADER LINE,
        TL_0007       TYPE TABLE OF ZFIWRT0007 WITH HEADER LINE,
        TL_USER       TYPE TABLE OF USER_ADDR  WITH HEADER LINE,
        WL_0019       TYPE ZFIWRT0019,
        TL_KNPAR      TYPE TABLE OF KNA1       WITH HEADER LINE,
        TL_LFPAR      TYPE TABLE OF LFA1       WITH HEADER LINE,
        TL_MARC       TYPE TABLE OF MARC       WITH HEADER LINE,
        WL_ACTIVE     TYPE J_1BNFE_ACTIVE,
        WL_ZIB_CHV    TYPE ZIB_CONTABIL_CHV,
        WL_KNA1       TYPE KNA1,
        WL_LFA1       TYPE LFA1,
        WL_T001W      TYPE T001W,
        WL_T001       TYPE T001,
        WL_1BAD       TYPE J_1BAD,
        WL_1BADT      TYPE J_1BADT,
        WL_1BAA       TYPE J_1BAA,
        WL_ZIB_OBJKEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: LT_RETURNS         TYPE TABLE OF BAPIRET2,
        LS_COELDES         TYPE BAPI1030_CEOUTPUTLIST,
        LV_CONTROLLINGAREA TYPE BAPI1030_GEN-CO_AREA,
        LV_COSTELEMENT     TYPE BAPI1030_GEN-COST_ELEM,
        LV_KEYDATE         TYPE BAPI1030_GEN-SOME_DATE.
* <--- S4 Migration - 18/07/2023 - CA
  REFRESH: TL_0009, TL_0010, TL_0011, TL_0012, TL_0013, TL_0015, TL_1BAJ, TL_1BAJT, TL_TBSL,
        TL_TBSL, TL_SKAT, TL_MAKT, TL_0005, TL_0007, TL_USER, TL_KNPAR, TL_LFPAR,
        TL_MARC, TL_CSKB.

  IF P_SEQ_LCTO IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZFIWRT0008
      INTO WL_0008
       WHERE SEQ_LCTO EQ P_SEQ_LCTO.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Nº de Documento não encontrado!'.
      LEAVE TO SCREEN 100.
    ELSE.

      SELECT SINGLE PRAZO FROM ZSDT_RETLOTE INTO VL_PRAZO WHERE DOCNUM_RET = WL_0008-DOCNUM_RETORNO.

      SELECT SINGLE * FROM ZFIWRT0019 INTO WL_0019 WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      SELECT * INTO TABLE TL_0020 FROM ZFIWRT0020 WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WL_0008-NFTYPE.

      SELECT SINGLE * FROM T001W INTO WL_T001W WHERE WERKS EQ WL_0008-BRANCH.

      SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO WL_ACTIVE WHERE DOCNUM EQ WL_0008-DOCNUM.

      CONCATENATE 'ZGF' WL_0008-SEQ_LCTO WL_0008-BUDAT(4) INTO WL_ZIB_OBJKEY.

      SELECT SINGLE * FROM ZIB_CONTABIL_CHV INTO WL_ZIB_CHV WHERE OBJ_KEY EQ WL_ZIB_OBJKEY.

      SELECT SINGLE * FROM ZFIWRT0001 INTO WL_0001 WHERE OPERACAO EQ WL_0008-OPERACAO.

      SELECT * FROM ZFIWRT0005 INTO TABLE TL_0005 WHERE OPERACAO EQ WL_0008-OPERACAO.

      IF WL_0008-PARVW EQ C_AG.
        SELECT SINGLE * FROM KNA1 INTO WL_KNA1 WHERE KUNNR EQ WL_0008-PARID.

      ELSEIF WL_0008-PARVW EQ C_BR
        OR   WL_0008-PARVW EQ C_LF.
        SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE LIFNR EQ WL_0008-PARID.

      ENDIF.
      SELECT * FROM ZFIWRT0015 INTO TABLE TL_0015 WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      IF SY-SUBRC IS INITIAL.
        SELECT * FROM LFA1 INTO TABLE TL_LFPAR FOR ALL ENTRIES IN TL_0015 WHERE LIFNR EQ TL_0015-PARID.
        SELECT * FROM KNA1 INTO TABLE TL_KNPAR FOR ALL ENTRIES IN TL_0015 WHERE KUNNR EQ TL_0015-PARID.
      ENDIF.

      SELECT *
        FROM ZFIWRT0009 INTO TABLE TL_0009
       WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM MAKT
          INTO TABLE TL_MAKT
          FOR ALL ENTRIES IN TL_0009
           WHERE SPRAS EQ SY-LANGU
             AND MATNR EQ TL_0009-MATNR.

        SELECT *
          FROM MARC
          INTO TABLE TL_MARC
           FOR ALL ENTRIES IN TL_0009
           WHERE MATNR EQ TL_0009-MATNR
             AND WERKS EQ TL_0009-BWKEY.
      ENDIF.

      SELECT *
        FROM ZFIWRT0010
        INTO TABLE TL_0010
         WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM J_1BAJ
          INTO TABLE TL_1BAJ
           FOR ALL ENTRIES IN TL_0010
           WHERE TAXTYP EQ TL_0010-TAXTYP.

        SELECT *
          FROM J_1BAJT
          INTO TABLE TL_1BAJT
           FOR ALL ENTRIES IN TL_0010
          WHERE  SPRAS  EQ SY-LANGU
            AND  TAXTYP EQ TL_0010-TAXTYP.

      ENDIF.

      SELECT *
        FROM ZFIWRT0011
        INTO TABLE TL_0011
         WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.
      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM TBSL
          INTO TABLE TL_TBSL
           FOR ALL ENTRIES IN TL_0011
           WHERE BSCHL EQ TL_0011-BSCHL.

        SELECT *
          FROM SKAT
          INTO TABLE TL_SKAT
           FOR ALL ENTRIES IN TL_0011
            WHERE SPRAS EQ SY-LANGU
              AND KTOPL EQ C_50
              AND SAKNR EQ TL_0011-HKONT.

* ---> S4 Migration - 18/07/2023 - CA
*        SELECT *
*          FROM cskb
*          INTO TABLE tl_cskb
*          FOR ALL ENTRIES IN tl_0011
*            WHERE kstar EQ tl_0011-hkont
*              AND ( datbi GE wl_0008-dt_criacao
*                AND datab LE wl_0008-dt_criacao )
*              AND katyp EQ '01'.

        SELECT KOKRS UP TO 1 ROWS
          FROM TKA02
          INTO @DATA(LV_KOKRS)
          WHERE BUKRS = @WL_0008-BUKRS.
        ENDSELECT.
        IF SY-SUBRC = 0.

          LOOP AT TL_0011 INTO DATA(LS_0011).

            LV_CONTROLLINGAREA  = LV_KOKRS.
            LV_COSTELEMENT      = LS_0011-HKONT.
            LV_KEYDATE          = WL_0008-DT_CRIACAO.

            CLEAR: LT_RETURNS[], LS_COELDES.

            CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
              EXPORTING
                CONTROLLINGAREA   = LV_CONTROLLINGAREA
                COSTELEMENT       = LV_COSTELEMENT
                KEYDATE           = LV_KEYDATE
              IMPORTING
                COSTELEMENTDETAIL = LS_COELDES
              TABLES
                RETURN            = LT_RETURNS.

            READ TABLE LT_RETURNS TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
            IF SY-SUBRC <> 0.
              TL_CSKB-KOKRS = LV_KOKRS.
              TL_CSKB-KSTAR = LS_0011-HKONT.
              TL_CSKB-KATYP = LS_COELDES-CELEM_CATEGORY.



              APPEND TL_CSKB.
              CLEAR TL_CSKB.
            ENDIF.

            CLEAR LS_0011.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
      ENDIF.
      SELECT *
        FROM ZFIWRT0012
        INTO TABLE TL_0012
         WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      SELECT *
        FROM ZFIWRT0013
        INTO TABLE TL_0013
         WHERE SEQ_LCTO EQ WL_0008-SEQ_LCTO.

      SELECT *
       FROM ZFIWRT0007
       INTO TABLE TL_0007
        WHERE OPERACAO EQ WL_0008-OPERACAO
          AND TIPO     EQ C_W.

      IF TL_0007[] IS NOT INITIAL.
        SELECT *
          FROM USER_ADDR
          INTO TABLE TL_USER
           FOR ALL ENTRIES IN TL_0007
            WHERE BNAME EQ TL_0007-USNAM.

      ENDIF.

      W_ZFIWRT0001 = WL_0001.  "*-CS2023000043-09.02.2023-#102019-JT

      PERFORM PREENCHE_CAMPOS_DOC TABLES TL_0009
                                         TL_0010
                                         TL_0011
                                         TL_0012
                                         TL_0013
                                         TL_0015
                                         TL_0020
                                         TL_TBSL
                                         TL_SKAT
                                         TL_CSKB
                                         TL_1BAJ
                                         TL_1BAJT
                                         TL_MAKT
                                         TL_0005
                                         TL_0007
                                         TL_USER
                                         TL_KNPAR
                                         TL_LFPAR
                                         TL_MARC
                                  USING  WL_0001
                                         WL_0008
                                         WL_KNA1
                                         WL_LFA1
                                         WL_T001W
                                         WL_ACTIVE
                                         WL_1BAA
                                         WL_ZIB_CHV
                                         WL_0019.

    ENDIF.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0009  text
*      -->P_TL_0010  text
*      -->P_TL_0011  text
*      -->P_TL_0012  text
*      -->P_TL_0013  text
*      -->P_TL_TBSL  text
*      -->P_TL_SKAT  text
*      -->P_TL_1BAJ  text
*      -->P_TL_1BAJT  text
*      -->P_WL_0001  text
*      -->P_WL_0008  text
*      -->P_WL_KNA1  text
*      -->P_WL_LFA1  text
*      -->P_WL_T001W  text
*----------------------------------------------------------------------*
FORM PREENCHE_CAMPOS_DOC  TABLES   TL_0009 STRUCTURE ZFIWRT0009
                                   TL_0010 STRUCTURE ZFIWRT0010
                                   TL_0011 STRUCTURE ZFIWRT0011
                                   TL_0012 STRUCTURE ZFIWRT0012
                                   TL_0013 STRUCTURE ZFIWRT0013
                                   TL_0015 STRUCTURE ZFIWRT0015
                                   TL_0020 STRUCTURE ZFIWRT0020
                                   TL_TBSL STRUCTURE TBSL
                                   TL_SKAT STRUCTURE SKAT
                                   TL_CSKB STRUCTURE CSKB
                                   TL_1BAJ STRUCTURE J_1BAJ
                                   TL_1BAJT STRUCTURE J_1BAJT
                                   TL_MAKT  STRUCTURE MAKT
                                   TL_0005  STRUCTURE ZFIWRT0005
                                   TL_0007  STRUCTURE ZFIWRT0007
                                   TL_USER  STRUCTURE USER_ADDR
                                   TL_KNPAR STRUCTURE KNA1
                                   TL_LFPAR STRUCTURE LFA1
                                   TL_MARC  STRUCTURE MARC
                          USING    WL_0001 TYPE ZFIWRT0001
                                   WL_0008 TYPE ZFIWRT0008
                                   WL_KNA1 TYPE KNA1
                                   WL_LFA1 TYPE LFA1
                                   WL_T001W TYPE T001W
                                   WL_ACTIVE TYPE J_1BNFE_ACTIVE
                                   WL_1BAA   TYPE J_1BAA
                                   WL_ZIB_CHV TYPE ZIB_CONTABIL_CHV
                                   WL_0019    TYPE ZFIWRT0019.

  DATA: WL_INDCOPER         TYPE ZFIWRT0006-INDCOPER,
        WL_TEXTO_FISCAL(30),
        TL_VALUES           TYPE VRM_VALUES,
        WL_VALUES           TYPE LINE OF VRM_VALUES,
        WL_CONT             TYPE SY-TABIX,
        WL_CONT_AUX         TYPE SY-TABIX,
        WL_CONT_AUX2        TYPE SY-TABIX,
        WL_IMPO_COMP        LIKE LINE OF TG_IMPO_COMP,
        WL_0011             TYPE ZFIWRT0011,
        "TL_ZSDT0075 TYPE TABLE OF TY_ZSDT0075,
        WL_0075             TYPE ZSDT0075,
        I_EBELP             TYPE EKPO-EBELP,
        IT_J_1BNFDOC        TYPE TABLE OF J_1BNFDOC WITH HEADER LINE.

  P_OPERACAO = WL_0008-OPERACAO.
  P_BUKRS    = WL_0008-BUKRS.
  P_BRANCH   = WL_0008-BRANCH.
  P_PARVW    = WL_0008-PARVW.
  P_PARID    = WL_0008-PARID.

  IF WL_0008-PARVW = 'AG'.
    READ TABLE TL_0011 INTO WL_0011
    WITH KEY BSCHL = '01'
             SEQ_LCTO = WL_0008-SEQ_LCTO.

    V_ZLSCH = WL_0011-ZLSCH.

    SELECT SINGLE *
      FROM ZSDT0075
      INTO WL_0075
      WHERE KUNNR = WL_0008-PARID
      AND BDATU >= SY-DATUM.

    IF SY-SUBRC IS INITIAL.
      XBOL = 'X'.
    ELSE.
      XBOL = 'N'.
    ENDIF.
  ENDIF.

  IF WL_0008-LOEKZ IS NOT INITIAL.
    P_LOEKZ = '@11@'.
  ELSE.
    P_LOEKZ = SPACE.
  ENDIF.

** Preenche valores da tela.
***Dados Gerais
****Header
  MOVE: WL_0008-NFTYPE       TO WG_FISCAL-NFTYPE,
        WL_0008-DIAS         TO WG_FISCAL-DIAS,
        WL_0008-RETORNO      TO WG_FISCAL-RETORNO,
        WL_0008-ZPESAGEM     TO WG_FISCAL-ZPESAGEM,
        WL_0008-IMOBILIZADO  TO WG_FISCAL-IMOBILIZADO,
        WL_0008-TP_MV_IMOB   TO WG_FISCAL-TP_MV_IMOB,
        WL_0008-KOSTL        TO WG_FISCAL-KOSTL,
        WL_0008-DOCREF       TO WG_FISCAL-DOCREF,
        WL_0008-NR_ROMANEIO  TO WG_FISCAL-NR_ROMANEIO,
        WL_0008-MOVE_PLANT   TO WG_FISCAL-MOVE_PLANT,
        WL_0008-MOVE_STLOC   TO WG_FISCAL-MOVE_STLOC,
        WL_0008-CTRL_ZRFL    TO WG_FISCAL-CTRL_ZRFL,
        WL_0008-ENERGIA      TO WG_FISCAL-ENERGIA,
        WL_0008-SERVICO      TO WG_FISCAL-SERVICO,
        WL_0008-COMPLEMENTO  TO WG_FISCAL-COMPLEMENTO,
        WL_0008-INCO1        TO WG_FISCAL-INCO1,
        WL_0008-INCO2        TO WG_FISCAL-INCO2,
        WL_0008-REFERENCIA   TO WG_FISCAL-REFERENCIA,
        WL_0008-ACCESS_KEY   TO WG_FISCAL-ACCESS_KEY,
        WL_0008-EBELN        TO WG_FISCAL-EBELN,
        WL_0008-DOCNUM       TO WG_DOCS-DOCNUM,
        WL_0008-BELNR        TO WG_DOCS-BELNR,
        WL_0008-MBLNR        TO WG_DOCS-MBLNR,
        WL_0008-BUDAT        TO WG_DOCS-BUDAT,
        WL_0008-BRANCH       TO WG_DOCS-BRANCH,
        WL_0008-BLDAT        TO WG_DOCS-BLDAT,
        WL_0008-SERIES       TO WG_DOCS-SERIES,
        WL_0001-TRANSF_ICMS  TO WG_FISCAL-TRANSF_ICMS,
        WL_0008-LM_ESTOQUE   TO WG_FISCAL-LM_ESTOQUE,
        WL_0008-KONTO        TO WG_FISCAL-KONTO,
        WL_0008-MOVE_MAT     TO WG_FISCAL-MOVE_MAT,
        WL_0008-MOVE_BATCH   TO WG_FISCAL-MOVE_BATCH,
        WL_0008-BKTXT        TO WG_FISCAL-BKTXT,
        WL_0008-MTSNR        TO WG_FISCAL-MTSNR,
        WL_0008-TCODE_ORG    TO WG_DOCS-TCODE_ORG,
        WL_0008-NOT_CHECK_XML TO WG_DOCS-NOT_CHECK_XML,
        WL_ZIB_CHV-BELNR     TO WG_DOCS-BELNR.


  IF ( WL_1BAA-FORM IS INITIAL ) OR ( WL_0008-TCODE_ORG EQ 'ZNFW0009' ).
    MOVE: WL_0008-NFENUM   TO WG_DOCS-NFENUM.
  ELSE.
    MOVE: WL_ACTIVE-NFNUM9 TO WG_DOCS-NFENUM.
  ENDIF.

  PERFORM F_DEFINE_ORIGEM_DESTINO USING WL_KNA1
                                        WL_LFA1
                                        WL_T001W
                                        WL_1BAA
                               CHANGING WL_INDCOPER
                                        WL_TEXTO_FISCAL.

**  Busca texto de tipo de operação
  CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
    EXPORTING
      I_TABLE_NAME = 'ZFIWRT0006'
      I_FIELD_NAME = 'OPERTYP'
    IMPORTING
      E_T_LIST     = TL_VALUES.

  MOVE: WL_0008-CFOP     TO WG_DIREITOS-CFOP,
        WL_0008-TAXLW1   TO WG_DIREITOS-TAXLW1,
        WL_0008-TAXLW2   TO WG_DIREITOS-TAXLW2,
        WL_0008-TAXLW4   TO WG_DIREITOS-TAXLW4,
        WL_0008-TAXLW5   TO WG_DIREITOS-TAXLW5,
*        Wl_0008-indcoper TO wg_direitos-indcoper,
        WL_0008-OPERTYP  TO WG_DIREITOS-OPERTYP,
        WL_0008-TAXCODE  TO WG_DIREITOS-TAXCODE.

  READ TABLE TL_VALUES INTO WL_VALUES
      WITH KEY KEY = WG_DIREITOS-OPERTYP.

  CONCATENATE WL_TEXTO_FISCAL '-' WL_VALUES-TEXT INTO WG_OP_FISCAL SEPARATED BY SPACE.
***      descricao da operacao.
*  REFRESH: tg_editor.
*  MOVE: wl_0001-txt_compl TO wg_editor.
*  APPEND wg_editor TO tg_editor.
  REFRESH: TG_EDITOR.
  CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
  WL_CONT = STRLEN( WL_0008-TXT_COMPL ).
  WL_CONT_AUX = WL_CONT / 72.

  DO.
    MOVE: WL_0008-TXT_COMPL+WL_CONT_AUX2 TO WG_EDITOR-LINE.
    ADD 72 TO WL_CONT_AUX2.
    APPEND WG_EDITOR TO TG_EDITOR.

    IF WL_CONT_AUX2 GT WL_CONT.
      EXIT.

    ENDIF.
  ENDDO.
  CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = TG_EDITOR.

***Impostos
  REFRESH: TG_IMPO, TG_IMPO_COMP.
  LOOP AT TL_0010.
    READ TABLE TL_1BAJ
      WITH KEY TAXTYP = TL_0010-TAXTYP.

    READ TABLE TL_1BAJT
      WITH KEY TAXTYP = TL_0010-TAXTYP.

    MOVE: TL_0010-TAXTYP    TO TG_IMPO-TAXTYP,
          TL_1BAJT-TTYPETXT TO TG_IMPO-TTYPETXT,
          TL_1BAJ-TAXGRP    TO TG_IMPO-TAXGRP.

    APPEND TG_IMPO.
    MOVE-CORRESPONDING: TL_0010 TO WL_IMPO_COMP.
    APPEND WL_IMPO_COMP TO TG_IMPO_COMP.

    CLEAR: TG_IMPO.
  ENDLOOP.

  SORT: TG_IMPO      BY TAXTYP TTYPETXT TAXGRP,
        TG_IMPO_COMP BY ITMNUM TAXTYP .

  DELETE ADJACENT DUPLICATES FROM TG_IMPO COMPARING ALL FIELDS.
***Contabilidade
  REFRESH: TG_CONTAB, TG_CONTAB-STYLE2, STYLE2.

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
  SELECT SINGLE *
         INTO @DATA(WL_ZFIT0196)
         FROM ZFIT0196.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

  LOOP AT TL_0011.
*    where estorno is initial.
    READ TABLE TL_SKAT
      WITH KEY SAKNR = TL_0011-HKONT.

    READ TABLE TL_TBSL
      WITH KEY BSCHL = TL_0011-BSCHL.

    READ TABLE TL_CSKB
      WITH KEY KSTAR = TL_0011-HKONT.
    IF SY-SUBRC IS NOT INITIAL.
      WA_STYLE-FIELDNAME = 'KOSTL'.
      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT  WA_STYLE INTO TABLE STYLE2.
    ELSE.
      IF TL_0011-ESTORNO IS NOT INITIAL.
        WA_STYLE-FIELDNAME = 'KOSTL'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT  WA_STYLE INTO TABLE STYLE2.
      ENDIF.
    ENDIF.

    IF TL_TBSL-KOART EQ C_K
    OR TL_TBSL-KOART EQ C_D
    AND TL_0011-ESTORNO IS INITIAL.
*      wa_style-fieldname = 'ZFBDT'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*      insert  wa_style into table style2.
*
*      wa_style-fieldname = 'ZLSCH'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*      insert  wa_style into table style2.
*      insert lines of style2 into table tg_contab-style2.
*      MOVE p_parid      TO tg_contab-hkont.
    ELSE.
      WA_STYLE-FIELDNAME = 'ZFBDT'.
      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT  WA_STYLE INTO TABLE STYLE2.

      WA_STYLE-FIELDNAME = 'ZLSCH'.
      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT  WA_STYLE INTO TABLE STYLE2.
*      MOVE: tl_0011-hkont   TO tg_contab-hkont.
    ENDIF.

    MOVE: TL_0011-BSCHL   TO TG_CONTAB-BSCHL,
          TL_0011-HKONT   TO TG_CONTAB-HKONT,
          TL_SKAT-TXT50   TO TG_CONTAB-TXT50,
          TL_0011-TAXTYP  TO TG_CONTAB-TAXTYP,
          TL_0011-ESTORNO TO TG_CONTAB-ESTORNO,
          TL_0011-NEWBW   TO TG_CONTAB-NEWBW,
          TL_0011-ZFBDT   TO TG_CONTAB-ZFBDT,
          TL_0011-ZLSCH   TO TG_CONTAB-ZLSCH,
          TL_0011-KOSTL   TO TG_CONTAB-KOSTL,

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          TL_0011-HBKID   TO TG_CONTAB-HBKID.

    IF TG_CONTAB-ZLSCH EQ 'D'.
      IF TL_0011-TAXA_JUROS IS NOT INITIAL.
        MOVE: TL_0011-TAXA_JUROS   TO TG_CONTAB-TAXA_JUROS.
      ELSE.
        MOVE: WL_ZFIT0196-JUROS   TO TG_CONTAB-TAXA_JUROS.
      ENDIF.

      IF TL_0011-TAXA_MULTA IS NOT INITIAL.
        MOVE: TL_0011-TAXA_MULTA   TO TG_CONTAB-TAXA_MULTA.
      ELSE.
        MOVE: WL_ZFIT0196-MULTA   TO TG_CONTAB-TAXA_MULTA.
      ENDIF.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

    INSERT LINES OF STYLE2 INTO TABLE TG_CONTAB-STYLE2.
    APPEND TG_CONTAB.
    CLEAR: TG_CONTAB.
    REFRESH: STYLE2.
  ENDLOOP.

  TG_TBSL[] = TL_TBSL[].
  SORT: TL_MAKT BY MATNR,
        TL_MARC BY MATNR WERKS.
*** Itens da Nota
  REFRESH: TG_ITENS.
  LOOP AT TL_0009.
    READ TABLE TL_MAKT
      WITH KEY MATNR = TL_0009-MATNR
               BINARY SEARCH.

    READ TABLE TL_MARC
      WITH KEY MATNR = TL_0009-MATNR
               WERKS = TL_0009-BWKEY
               BINARY SEARCH.

    MOVE: TL_0009-ITMNUM  TO TG_ITENS-ITMNUM,
          TL_0009-MATNR   TO TG_ITENS-MATNR,
          TL_MAKT-MAKTX   TO TG_ITENS-MAKTX,
          TL_0009-CFOP    TO TG_ITENS-CFOP,
          TL_0009-CHARG   TO TG_ITENS-CHARG,
          TL_0009-NR_FASE TO TG_ITENS-NR_FASE, "// US-168932 WBARBOSA 20/05/2025
          TL_0009-BWKEY   TO TG_ITENS-WERKS,
          TL_0009-LGORT   TO TG_ITENS-LGORT,
          TL_0009-MENGE   TO TG_ITENS-MENGE,
          TL_0009-MEINS   TO TG_ITENS-MEINS,
          TL_0009-NETPR   TO TG_ITENS-NETPR,
          TL_0009-NETWR   TO TG_ITENS-NETWR,
          TL_0009-ITMTYP  TO WG_FISCAL-ITMTYP,
          TL_0009-ANLN1   TO TG_ITENS-ANLN1,
          TL_0009-ANLN2   TO TG_ITENS-ANLN2,
          TL_0009-VBELN   TO TG_ITENS-VBELN,
          TL_0009-POSNR   TO TG_ITENS-POSNR,
          TL_MARC-STEUC   TO TG_ITENS-STEUC,
*-CS2020001331 - 06.10.2021 - JT - inicio
          TL_0009-POSSUI_ICMS_ST TO TG_ITENS-POSSUI_ICMS_ST.
*-CS2020001331 - 06.10.2021 - JT - fim
    TG_ITENS-NETDIS = TL_0009-NETDIS.
    TG_ITENS-NETFRE = TL_0009-NETFRE.
    TG_ITENS-NETINS = TL_0009-NETINS.
    TG_ITENS-NETOTH = TL_0009-NETOTH.
    TG_ITENS-NETWR = ( ( TG_ITENS-MENGE * TG_ITENS-NETPR ) + TG_ITENS-NETFRE + TG_ITENS-NETINS + TG_ITENS-NETOTH ) - TG_ITENS-NETDIS.

    TG_ITENS-FASE = ICON_DISPLAY_MORE.
    I_EBELP = TG_ITENS-ITMNUM.
    "
    SELECT SINGLE RENAS FROM ZMMT0102 INTO TG_ITENS-RENAS
      WHERE EBELN = P_SEQ_LCTO
      AND   EBELP = I_EBELP.
    "
    APPEND TG_ITENS.
    CLEAR: TG_ITENS.
  ENDLOOP.

***Movimentacao de estoque
  REFRESH: TG_MOVEST.
  LOOP AT TL_0012.
    MOVE: TL_0012-BWART   TO TG_MOVEST-BWART,
          TL_0012-TCODE   TO TG_MOVEST-TCODE,
          TL_0012-MWSKZ1  TO TG_MOVEST-MWSKZ1,
          TL_0012-ESTORNO TO TG_MOVEST-ESTORNO.

    APPEND TG_MOVEST.
    CLEAR: TG_MOVEST.
  ENDLOOP.

  REFRESH: NODE_ITAB.
  IF TREE IS NOT INITIAL.
*    IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
    CALL METHOD TREE->DELETE_ALL_NODES.
*      PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

    PERFORM PREENCHE_TREE USING C_ROOT
                             SPACE
                             C_X
                             SPACE
                             'Mensagens da Nota'
                             SPACE.
*    ENDIF.

    CLEAR: TL_0013.
*---> 06/07/2023 - Migração S4 - WS
    SORT TL_0013 BY SEQNUM.
*<--- 06/07/2023 - Migração S4 - WS
    LOOP AT TL_0013.
      ON CHANGE OF TL_0013-SEQNUM.
*       AT NEW SEQNUM.
        PERFORM PREENCHE_TREE USING TL_0013-SEQNUM
                                    C_ROOT
                                    SPACE
                                    CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                    TL_0013-MESSAGE
                                    HANDLE_TREE.
      ENDON.
*      ENDAT.
      TG_MENSAGEMS-SEQNUM  = TL_0013-SEQNUM.
      TG_MENSAGEMS-LINNUM  = TL_0013-LINNUM.
      TG_MENSAGEMS-MESSAGE = TL_0013-MESSAGE.

      READ TABLE TL_0005
        WITH KEY MESSAGE = TL_0013-MESSAGE.
      IF SY-SUBRC IS INITIAL.
        APPEND TG_MENSAGEMS.         " msgs q foram parametrizadas na operacao
      ENDIF.
      APPEND TG_MENSAGEMS TO TG_MENSAGEMS_AUX.  " tabela q iram conter todas as msgs
      " inclusive as adcionadas na criacao da NF
    ENDLOOP.
  ENDIF.

  REFRESH: TG_APROV.
  LOOP AT TL_0007.
    READ TABLE TL_USER
     WITH KEY BNAME = TL_0007-USNAM.

    MOVE: TL_0007-NIVEL_APROV  TO TG_APROV-NIVEL_APROV,
          TL_0007-USNAM        TO TG_APROV-USNAM,
          TL_0007-DEPARTAMENTO TO TG_APROV-DEPARTAMENTO.

    CONCATENATE TL_USER-NAME_FIRST TL_USER-NAME_LAST INTO TG_APROV-NOME SEPARATED BY SPACE.

    APPEND TG_APROV.
    CLEAR: TG_APROV.
  ENDLOOP.

** Parceiros da Nota fiscal
  REFRESH: TG_PARC, STYLE.

  CLEAR: TG_PARC.

  TG_PARC-PARVW = P_PARVW.
  TG_PARC-PARID = P_PARID.

  IF P_PARVW EQ C_AG.
    TG_PARC-NOME = WL_KNA1-NAME1.

  ELSEIF P_PARVW EQ C_BR
      OR P_PARVW EQ C_LF
      OR P_PARVW EQ C_Z1.
    TG_PARC-NOME  = WL_LFA1-NAME1.

  ENDIF.
  WA_STYLE-FIELDNAME = 'PARVW'.
  WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE .
  WA_STYLE-FIELDNAME = 'PARID'.
  WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE .
  INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.

  APPEND TG_PARC.
  REFRESH: STYLE.
  CLEAR: TG_PARC.

  LOOP AT TL_0015.
    READ TABLE TG_PARC TRANSPORTING NO FIELDS
      WITH KEY PARVW = TL_0015-PARVW
               PARID = TL_0015-PARID.

    IF SY-SUBRC IS NOT INITIAL.
      IF TL_0015-PARVW EQ C_AG OR
         TL_0015-PARVW EQ C_LR.

        READ TABLE TL_KNPAR
          WITH KEY KUNNR = TL_0015-PARID.

        TG_PARC-NOME  = TL_KNPAR-NAME1.

      ELSEIF TL_0015-PARVW EQ C_BR
        OR   TL_0015-PARVW EQ C_LF
        OR   TL_0015-PARVW EQ C_Z1
        OR   TL_0015-PARVW EQ C_PC
        OR   TL_0015-PARVW EQ C_SP.
        READ TABLE TL_LFPAR
          WITH KEY LIFNR = TL_0015-PARID.

        TG_PARC-NOME  = TL_LFPAR-NAME1.

      ENDIF.
      TG_PARC-PARVW = TL_0015-PARVW.
      TG_PARC-PARID = TL_0015-PARID.

      APPEND TG_PARC.
      CLEAR: TG_PARC, TL_LFPAR, TL_KNPAR.
    ENDIF.
  ENDLOOP.

  IF NOT ( WL_0019 IS INITIAL ).
    MOVE: WL_0019-LIFNR  TO WG_TRANSPORTE-LIFNR,
          WL_0019-PLACA  TO WG_TRANSPORTE-PLACA,
          WL_0019-ANZPK  TO WG_TRANSPORTE-ANZPK,
          WL_0019-SHPUNT TO WG_TRANSPORTE-SHPUNT,
          WL_0019-NTGEW  TO WG_TRANSPORTE-NTGEW,
          WL_0019-BRGEW  TO WG_TRANSPORTE-BRGEW,
          WL_0019-UFPLACA TO WG_TRANSPORTE-UFPLACA,

          WL_0019-PLACA_CAR1 TO WG_TRANSPORTE-PLACA_CAR1,
          WL_0019-PLACA_CAR2 TO WG_TRANSPORTE-PLACA_CAR2,
          WL_0019-PLACA_CAR3 TO WG_TRANSPORTE-PLACA_CAR3,
          WL_0019-MOTORISTA  TO WG_TRANSPORTE-MOTORISTA.
  ENDIF.

  CLEAR: TG_DOCREFS[].

  IF TL_0020[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_J_1BNFDOC
      FROM J_1BNFDOC
       FOR ALL ENTRIES IN TL_0020
     WHERE DOCNUM EQ TL_0020-DOCNUM.

    SORT IT_J_1BNFDOC BY DOCNUM.
  ENDIF.

  LOOP AT TL_0020 .
    READ TABLE IT_J_1BNFDOC WITH KEY DOCNUM = TL_0020-DOCNUM BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    TG_DOCREFS-DOCNUM = IT_J_1BNFDOC-DOCNUM.
    TG_DOCREFS-DOCDAT = IT_J_1BNFDOC-DOCDAT.
    TG_DOCREFS-MODEL  = IT_J_1BNFDOC-MODEL .
    TG_DOCREFS-SERIES = IT_J_1BNFDOC-SERIES.
    TG_DOCREFS-NFENUM = IT_J_1BNFDOC-NFENUM.
    TG_DOCREFS-NFTOT  = IT_J_1BNFDOC-NFTOT.
    APPEND TG_DOCREFS.
  ENDLOOP.

ENDFORM.                    " PREENCHE_CAMPOS_DOC
*&---------------------------------------------------------------------*
*&      Form  VALIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDA_DADOS .
  DATA: WL_0001  TYPE ZFIWRT0001,
        WL_T001  TYPE T001,
*        wl_1bbranch TYPE j_1bbranch,
        WL_1BAD  TYPE J_1BAD,
        WL_1BADT TYPE J_1BADT,
        WL_KNA1  TYPE KNA1,
        WL_LFA1  TYPE LFA1,
        WL_0008  TYPE ZFIWRT0008.


  CLEAR: WL_0001, WL_T001, WL_1BBRANCH, WL_1BAD, WL_1BADT, WL_KNA1, WL_LFA1, WL_0008.


  IF P_OPERACAO IS  NOT INITIAL
  AND P_BUKRS IS    NOT INITIAL
   AND P_BRANCH IS  NOT INITIAL
    AND P_PARVW IS  NOT INITIAL
     AND P_PARID IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZFIWRT0001
      INTO WL_0001
       WHERE OPERACAO EQ P_OPERACAO.



    IF  SY-SUBRC IS INITIAL.
*          MOVE: wl_0001-descricao TO wg_desc_operacao.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cód. de Operação não existe!'.
      LEAVE TO SCREEN 100.
    ENDIF.

    SELECT SINGLE *
      FROM T001
      INTO WL_T001
       WHERE BUKRS EQ P_BUKRS.

    IF SY-SUBRC IS INITIAL.
*          MOVE: wl_t001-butxt TO wg_desc_bukrs.

      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_1BBRANCH
         WHERE BUKRS  EQ P_BUKRS
           AND BRANCH EQ P_BRANCH.

      IF SY-SUBRC IS INITIAL.
*            MOVE: wl_1bbranch-name TO wg_desc_branch.

      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'O Local de negocio nao foi encontrado para'
                              'essa empresa!'.
        LEAVE TO SCREEN 100.
      ENDIF.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'A empresa não existe!'.
      LEAVE TO SCREEN 100.
    ENDIF.

    SELECT SINGLE *
      FROM J_1BAD
      INTO WL_1BAD
       WHERE PARVW EQ P_PARVW.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE *
        FROM J_1BADT
        INTO WL_1BADT
         WHERE SPRAS EQ SY-LANGU
           AND PARVW EQ P_PARVW.

*          MOVE: wl_1badt-partxt TO wg_desc_parvw.

      IF P_PARVW EQ C_AG.
        SELECT SINGLE *
          FROM KNA1
          INTO WL_KNA1
           WHERE KUNNR EQ P_PARID.

        IF SY-SUBRC IS INITIAL.
*              MOVE: wl_kna1-name1 TO wg_desc_parid.

        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Cliente selecionado, não foi encontrado!'.
          LEAVE TO SCREEN 100.
        ENDIF.

      ELSEIF P_PARVW EQ C_LF
        OR   P_PARVW EQ C_BR.
        SELECT SINGLE *
          FROM LFA1
          INTO WL_LFA1
           WHERE LIFNR EQ P_PARID.

        IF SY-SUBRC IS INITIAL.
*              MOVE: wl_lfa1-name1 TO wg_desc_parid.

        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Fornecedor/Filial selecionado, não foi encontrado!'.
          LEAVE TO SCREEN 100.
        ENDIF.

      ENDIF.

    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'A função de Parceiro selecionada, não foi'
                            'encontrada!'.
      LEAVE TO SCREEN 100.
    ENDIF.
*        IF tg_itens[] IS INITIAL.
*          tg_itens-itmnum = 10.
*          APPEND tg_itens.
*        ENDIF.
*   ok-code = c_atuali.
  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Preencher os campos obrigatorios!'.
    LEAVE TO SCREEN 100.
  ENDIF.
*ENDIF.

  IF WG_FISCAL-RETORNO EQ C_S
  AND WG_FISCAL-DOCREF IS INITIAL.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do '
                                           'campo "Doc. Original".'.
    G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB1.
    X_FIELD = 'WG_FISCAL-DOCREF'.
    LEAVE TO SCREEN 100.
  ENDIF.

  IF WG_FISCAL-ZPESAGEM EQ C_01
  AND WG_FISCAL-NR_ROMANEIO IS INITIAL.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do '
                                           'campo "Romaneio".'.
    X_FIELD = 'WG_FISCAL-NR_ROMANEIO'.
    G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB1.
    LEAVE TO SCREEN 100.
  ENDIF.

  LOOP AT TG_ITENS.
    IF TG_ITENS-MATNR IS INITIAL
    OR TG_ITENS-WERKS IS INITIAL
    OR TG_ITENS-NETWR IS INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Preencher informações obrigatorias,'
                                             'nos itens da nota.'.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB6.
      LEAVE TO SCREEN 100.
    ENDIF.
    IF WG_FISCAL-IMOBILIZADO EQ C_S
    AND TG_ITENS-ANLN1 IS INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Preencher informações obrigatorias,'
                                              'nos itens da nota.'.
      G_TAB_STRIP_NF-PRESSED_TAB = C_TAB_STRIP_NF-TAB6.
      LEAVE TO SCREEN 100.
    ENDIF.
  ENDLOOP.

  IF ZFIWRT0008-PARVW = 'AG'.

  ENDIF.

**********************************************************************
* 102147 CS2023000072 Validação centro de custo transferências IMOBILIZADO - PSA
**********************************************************************

*  IF tg_parc-parvw = 'BR' AND wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob = 'T' AND tg_parc-parid <> p_parid.
*
*    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Escrever aqui menssagem de erro!,'
*                                                  'Continua!'.
*    g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
*    LEAVE TO SCREEN 100.
*
*  ENDIF.


ENDFORM.                    " VALIDA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ELEMINA_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ELEMINA_DOC .
  DATA: WL_0008 TYPE ZFIWRT0008.

  SELECT  SINGLE *
    FROM ZFIWRT0008
    INTO WL_0008
     WHERE SEQ_LCTO EQ P_SEQ_LCTO.

  IF SY-SUBRC IS INITIAL.
    IF WL_0008-STATUS IS INITIAL OR WL_0008-DOCNUM IS INITIAL.
      MOVE: C_X TO WL_0008-LOEKZ.
      MODIFY ZFIWRT0008 FROM WL_0008.
      MESSAGE S836(SD) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'foi marcado para processamento!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELEMINA_DOC
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .

  DATA: WL_0001          TYPE ZFIWRT0001,
        WL_0011          TYPE ZFIWRT0011,
        WL_T001          TYPE T001,
        WL_T001W         TYPE T001W,
        WA_ZMMT0102      TYPE ZMMT0102,
        WL_1BBRANCH      TYPE J_1BBRANCH,
        WL_1BAD          TYPE J_1BAD,
        WL_KNA1          TYPE KNA1,
        WL_KNB1          TYPE KNB1,
        WL_ANLA          TYPE ANLA,
        WL_LFA1          TYPE LFA1,
        WL_1BADT         TYPE J_1BADT,
        WL_ZLEST0002     TYPE ZLEST0002,
        TL_ANLC          TYPE TABLE OF ANLC,
        TL_2000          TYPE TABLE OF ZFIWRT2000 WITH HEADER LINE,
        TL_2001          TYPE TABLE OF ZFIWRT2001 WITH HEADER LINE,
        TL_2002          TYPE TABLE OF ZFIWRT2002 WITH HEADER LINE,
        TL_T042Z         TYPE TABLE OF T042Z WITH HEADER LINE,
        TL_TBSL          TYPE TABLE OF TBSL       WITH HEADER LINE,
        TL_CSKB          TYPE TABLE OF CSKB       WITH HEADER LINE,
        TL_CSKS          TYPE TABLE OF CSKS       WITH HEADER LINE,
        WL_CSKS          TYPE CSKS,
        WL_LINHA(6),
        V_DATA_NOTA(10),
        LC_QTD_LINHAS    TYPE I,
        TL_MCHA          TYPE TABLE OF MCHA WITH HEADER LINE,
        TL_T001L         TYPE TABLE OF T001L WITH HEADER LINE,
        TL_KNB1          TYPE TABLE OF KNB1 WITH HEADER LINE,
        TL_LFB1          TYPE TABLE OF LFB1 WITH HEADER LINE,
        TL_MARA          TYPE TABLE OF MARA WITH HEADER LINE,
        WL_1BAA          TYPE J_1BAA,
        WL_MARC          TYPE MARC,
        WL_MBEW          TYPE MBEW,
        TL_0009          TYPE TABLE OF ZFIWRT0009 WITH HEADER LINE,
        TL_T001K         TYPE TABLE OF T001K WITH HEADER LINE,
        WL_SEQ_LCTO      TYPE ZFIWRT0008-SEQ_LCTO,
        WL_ZIB_NFE_FORN  TYPE ZIB_NFE_FORN,
        WL_CNPJ          TYPE LFA1-STCD1,
        WL_PARID         TYPE ZFIWRT0008-PARID,
        WL_PARID2        TYPE ZFIWRT0008-PARID,
        WL_TOT_CONTAB    TYPE DMBTR,
        WL_VBAP          TYPE VBAP,
        P_XBLNR          TYPE  XBLNR1,
        WA_SETLEAF       TYPE SETLEAF,
        WL_TKA02         TYPE TKA02,
        VKOKRS           TYPE TKA02-KOKRS,
        WA_ZSDT0075      TYPE ZSDT0075,
        LS_ACCKEY_STR    TYPE J_1B_NFE_ACCESS_KEY,
        LV_STCD1_CK2     TYPE J_1B_NFE_ACCESS_KEY-STCD1,
        LV_SERIES        TYPE J_1BSERIES,
        LV_NFNUM9        TYPE J_1BNFNUM9,
        LV_PARID_KEY     TYPE LFA1-LIFNR,

        LV_TOTDIFF       TYPE ZFIWRT0009-NETWR,
        LV_TOTREF        TYPE ZFIWRT0009-NETWR,
        LV_TOTITENS      TYPE ZFIWRT0009-NETWR,
        LV_VLR_ITENS     TYPE WRBTR,
        LV_STCD1_CK      TYPE LFA1-STCD1,
        V_EBELP          TYPE EKPO-EBELP,
        LVA_ANLN1        TYPE ANLA-ANLN1,
        WL_DOC           TYPE J_1BNFDOC,
        WA_J_1BNFDOC     TYPE J_1BNFDOC,
        WA_ZFIWRT0020    TYPE ZFIWRT0020,
        WA_ZFIWRT0008    TYPE ZFIWRT0008,
        VG_CANDAT        TYPE J_1BNFDOC-CANDAT,
        LVA_MSG_TMP      TYPE STRING,
        LV_AVAILABLE     TYPE C,
        LV_IV_ICMS       TYPE C,   "*-CS2023000043-14.02.2023-#102019-JT-COMENTADO
        TL_ZFIWRT0009    TYPE TABLE OF ZFIWRT0009,
        WL_ZFIWRT0009    TYPE ZFIWRT0009,
        WL_ZFIWRT0008    TYPE ZFIWRT0008,
        WL_J_1BNFDOC     TYPE J_1BNFDOC,
        WL_J_1BNFDOC_AUX TYPE J_1BNFDOC.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: LT_RETURNS         TYPE TABLE OF BAPIRET2,
        LS_COELDES         TYPE BAPI1030_CEOUTPUTLIST,
        LS_CSKB            LIKE LINE OF TL_CSKB,
        LV_CONTROLLINGAREA TYPE  BAPI1030_GEN-CO_AREA,
        LV_COSTELEMENT     TYPE  BAPI1030_GEN-COST_ELEM,
        LV_KEYDATE         TYPE  BAPI1030_GEN-SOME_DATE.
* <--- S4 Migration - 18/07/2023 - CA

  REFRESH: TG_MSG_RET, TL_MCHA, TL_T001L, TL_0009, TL_T001K, TL_T042Z, TL_TBSL,
           TL_KNB1, TL_LFB1, TL_CSKB, TL_CSKS.
  CLEAR: TG_MSG_RET, WL_1BAA, WL_SEQ_LCTO, WL_PARID, TL_T001K,
         WL_KNB1. ", lv_iv_icms. *-CS2023000043-14.02.2023-#102019-JT-COMENTADO
  DATA: OBJECT  TYPE SIBFLPORB,
        LT_STAT TYPE SGS_T_ACNT.

  "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - INICIO

* Determinação área de contabilidade de custos
  IF WG_FISCAL-KOSTL IS NOT INITIAL.
    SELECT SINGLE * FROM TKA02
      INTO @DATA(W_TKA02)
      WHERE BUKRS	= @P_BUKRS.
    SELECT SINGLE *
      FROM CSKS
      INTO WL_CSKS
        WHERE KOKRS EQ  W_TKA02-KOKRS
        AND KOSTL   EQ  WG_FISCAL-KOSTL
        AND GSBER   EQ  WG_FISCAL-MOVE_PLANT.

    IF SY-SUBRC IS NOT INITIAL.

      MOVE: 'Centro de Custo informado não é desta filial.'                   TO TG_MSG_RET-MSG,
               C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
               'WG_FISCAL-KOSTL'          TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ENDIF.
  ENDIF.
  "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - FIM
  CONDENSE WG_FISCAL-ACCESS_KEY NO-GAPS.
  IF P_SEQ_LCTO GT 0.
    CASE P_OPERACAO .
      WHEN '131' OR '186' OR '241' OR '216' OR '221' OR '621' OR '176' OR '20'.
        MOVE 'BO' TO OBJECT-CATID.
        OBJECT-TYPEID = 'ZWRR0002'.
        CONCATENATE SY-MANDT P_SEQ_LCTO INTO OBJECT-INSTID.
        REFRESH  LT_STAT.
        CALL METHOD CL_GOS_ATTACHMENT_QUERY=>COUNT_FOR_OBJECT
          EXPORTING
            IS_OBJECT = OBJECT
            IP_ARL    = SPACE
          RECEIVING
            RT_STAT   = LT_STAT.
        IF LT_STAT[] IS INITIAL.
          MOVE: TEXT-E57                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
            'WG_FISCAL-INCO1'          TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
    ENDCASE.
  ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 14.06.23 - #108893
  IF W_ZFIWRT0001-VALIDA_CFOP EQ 'S'.
    IF WG_DOCS-NFENUM IS NOT INITIAL AND
       WG_DOCS-BLDAT IS NOT INITIAL AND
       P_PARID IS NOT INITIAL.

      SELECT SINGLE *
             INTO @DATA(WL_LFA1_NEW)
             FROM LFA1
             WHERE LIFNR EQ @P_PARID.

      SELECT SINGLE *
             INTO @DATA(WL_ZIB_NFE_DIST_TER)
             FROM ZIB_NFE_DIST_TER
             WHERE FORNE_CNPJ EQ @WL_LFA1_NEW-STCD1
               AND NUMERO     EQ @WG_DOCS-NFENUM
               AND DT_EMISSAO EQ @WG_DOCS-BLDAT.

      IF WL_ZIB_NFE_DIST_TER-CHAVE_NFE IS NOT INITIAL.

        SELECT  *
               INTO TABLE @DATA(TL_ZIB_NFE_DIST_ITM)
               FROM ZIB_NFE_DIST_ITM
               WHERE CHAVE_NFE EQ @WL_ZIB_NFE_DIST_TER-CHAVE_NFE.
      ENDIF.

      SELECT  SINGLE *
        FROM T001W
        INTO @DATA(_T001W)
        WHERE WERKS = @P_BRANCH.

      LOOP AT TL_ZIB_NFE_DIST_ITM INTO DATA(WL_ZIB_NFE_DIST_ITM).
        IF WL_ZIB_NFE_DIST_ITM-PROD_CFOP IS NOT INITIAL AND WL_LFA1_NEW-REGIO IS NOT INITIAL.
          SELECT *
                 INTO TABLE @DATA(TL_ZFIWRT0028)
                 FROM ZFIWRT0028
                 WHERE OPERACAO EQ @P_OPERACAO
                   AND UF       EQ @_T001W-REGIO
                   AND CFOP     EQ @WL_ZIB_NFE_DIST_ITM-PROD_CFOP.

          IF SY-SUBRC NE 0.
            MOVE: TEXT-E95                   TO TG_MSG_RET-MSG, "CFOP não permitido nesta operação!
                  C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 14.06.23 - #108893

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S'.
    SELECT SINGLE *
      INTO @DATA(W_BATL1)
      FROM J_1BATL1
     WHERE TAXLAW = @WG_DIREITOS-TAXLW1.

    IF SY-SUBRC <> 0.
      MOVE: TEXT-E91                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_DIREITOS-TAXLW1'        TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(W_BATL2)
      FROM J_1BATL2
     WHERE TAXLAW = @WG_DIREITOS-TAXLW2.

    IF SY-SUBRC <> 0.
      MOVE: TEXT-E92                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_DIREITOS-TAXLW2'        TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(W_BATL4A)
      FROM J_1BATL4A
     WHERE TAXLAW = @WG_DIREITOS-TAXLW4.

    IF SY-SUBRC <> 0.
      MOVE: TEXT-E93                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_DIREITOS-TAXLW4'        TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(W_BATL5)
      FROM J_1BATL5
     WHERE TAXLAW = @WG_DIREITOS-TAXLW5.

    IF SY-SUBRC <> 0.
      MOVE: TEXT-E94                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_DIREITOS-TAXLW5'        TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

*** PBI - 73759 - Inicio - CBRAND
  IF TG_ITENS[] IS NOT INITIAL.

    LOOP AT TG_ITENS.
**      Ajuste chamado - #103385 - RJF
      IF TG_ITENS-ANLN1 IS NOT INITIAL. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
        SELECT SINGLE *
         FROM ANLA
         INTO WL_ANLA
         WHERE  BUKRS = P_BUKRS "RJF S4Hana
            AND ANLN1 = TG_ITENS-ANLN1
            AND ANLN2 = TG_ITENS-ANLN2.

        IF SY-SUBRC = 0.

          "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - INICIO
          IF WG_FISCAL-IMOBILIZADO = 'S' AND WG_FISCAL-TP_MV_IMOB NE 'C'.
            CLEAR TL_ANLC.
            SELECT *
                   INTO TABLE TL_ANLC
                   FROM ANLC
                   WHERE BUKRS EQ P_BUKRS
                     AND ANLN1 EQ TG_ITENS-ANLN1
                     AND AFABE EQ '5'.

            IF SY-SUBRC EQ 0.
              SORT TL_ANLC BY GJAHR DESCENDING.
              READ TABLE TL_ANLC INTO DATA(W_ANLC) INDEX 1.
              IF W_ANLC-ANSAZ > 0.
                TG_MSG_RET-MSG = ' EXISTE ADIANTAMENTO EM ABERTO PARA O IMOBILIZADO INFORMADO'.
                MOVE: C_TAB_STRIP_NF-TAB3 TO TG_MSG_RET-ABA,
                     'TG_ITENS-ANLN1'  TO TG_MSG_RET-FIELD.
                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET.
              ENDIF.
            ENDIF.
          ENDIF.
          "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - FIM

*** BUG - 79101 - Inicio - CBRAND
          CLEAR: TL_ZFIWRT0009,
                 WL_ZFIWRT0009,
                 WL_ZFIWRT0008,
                 LVA_ANLN1,
                 WL_J_1BNFDOC,
                 WL_J_1BNFDOC_AUX.
          SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WG_FISCAL-NFTYPE.
          IF WG_FISCAL-IMOBILIZADO = 'S' AND WG_FISCAL-TP_MV_IMOB <> 'V'
             AND WL_1BAA-DIRECT = '2'.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = TG_ITENS-ANLN1
              IMPORTING
                OUTPUT = LVA_ANLN1.

            SELECT *
              FROM ZFIWRT0009
            INTO TABLE TL_ZFIWRT0009
              WHERE ANLN1  = LVA_ANLN1
                 AND ANLN2 = TG_ITENS-ANLN2.

            IF TL_ZFIWRT0009 IS NOT INITIAL.

              LOOP AT TL_ZFIWRT0009 INTO WL_ZFIWRT0009.

                SELECT SINGLE *
                  FROM ZFIWRT0008
                  INTO WL_ZFIWRT0008
                    WHERE SEQ_LCTO = WL_ZFIWRT0009-SEQ_LCTO.

                SELECT SINGLE *
                  FROM J_1BNFDOC
                  INTO WL_J_1BNFDOC
                    WHERE DOCNUM = WL_ZFIWRT0008-DOCNUM.

                IF WL_J_1BNFDOC-DIRECT = 2 AND  WL_J_1BNFDOC-CANDAT IS INITIAL.
                  SELECT SINGLE *
                    FROM J_1BNFDOC
                    INTO WL_J_1BNFDOC_AUX
                    WHERE  DIRECT  = '1'
                      AND  PARTYP  = 'B'
                      AND  BUKRS   = WL_J_1BNFDOC-BUKRS
                      AND  BRANCH  = WL_J_1BNFDOC-PARID+4(4)   "Empresa+Centro
                      AND  DOCDAT  = WL_J_1BNFDOC-DOCDAT
                      AND  NFENUM  = WL_J_1BNFDOC-NFENUM.

*                IF sy-subrc NE 0.
*                  CONCATENATE wl_j_1bnfdoc-docdat+6(2) '.' wl_j_1bnfdoc-docdat+4(2) '.' wl_j_1bnfdoc-docdat+0(4) INTO v_data_nota.
*                  CONCATENATE 'Existe saida para o Imobilizado sem Entrada na Filial Destino, SeqLct:' wl_zfiwrt0009-seq_lcto  'NOTA: '  wl_j_1bnfdoc-nfenum 'Item' tg_itens-itmnum '-' v_data_nota INTO tg_msg_ret-msg SEPARATED BY space.
*                  MOVE:   c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
*                     'TG_ITENS-ANLN1'      TO tg_msg_ret-field.
*                  APPEND tg_msg_ret.
*                  CLEAR: tg_msg_ret.
*                ENDIF.
                ENDIF.
                CLEAR: WL_ZFIWRT0009, WL_ZFIWRT0008.
              ENDLOOP.
            ENDIF.
          ENDIF.
*** BUG - 79101 - Fim - CBRAND


          IF WG_FISCAL-IMOBILIZADO = 'S' AND WG_FISCAL-TP_MV_IMOB <> 'C'.
            IF WL_ANLA-BUKRS <> P_BUKRS.
              CONCATENATE 'Este Imobilizado pertence a empresa!' WL_ANLA-BUKRS 'Item' TG_ITENS-ITMNUM  INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

              MOVE: C_TAB_STRIP_NF-TAB3  TO TG_MSG_RET-ABA,
                   'P_BUKRS'  TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.

          IF WL_ANLA-DEAKT IS NOT INITIAL.

            CONCATENATE 'Este Imobilizado esta Desativado!!' 'Item' TG_ITENS-ITMNUM  INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            MOVE:   C_TAB_STRIP_NF-TAB3  TO TG_MSG_RET-ABA,
               'TG_ITENS-ANLN1'      TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.

          IF  WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB EQ 'V'. " "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188

            IF WL_ANLA-LEART = '03' OR  WL_ANLA-LEART = '04'.
              CONCATENATE 'Ativo imobilizado com restrições de movimentação. Por favor entrar em contato com imobilizado@amaggi.com.br;'
               'Item' TG_ITENS-ITMNUM  INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
              MOVE:  C_TAB_STRIP_NF-TAB3  TO TG_MSG_RET-ABA,
                      'TG_ITENS-ANLN1'    TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.

        ELSE.
          IF  WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB <> 'C'.
            CONCATENATE 'Este Imobilizado não existe na tabela ANLA!!' 'Item' TG_ITENS-ITMNUM  INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            MOVE:   C_TAB_STRIP_NF-TAB3  TO TG_MSG_RET-ABA,
                    'TG_ITENS-ANLN1'     TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ELSE.
        IF  WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB <> 'C'.
          CONCATENATE 'o Imobilizado não foi informado!!' 'Item' TG_ITENS-ITMNUM  INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          MOVE:   C_TAB_STRIP_NF-TAB3  TO TG_MSG_RET-ABA,
                  'TG_ITENS-ANLN1'     TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
**      Ajuste chamado - #103385 - RJF
    ENDLOOP.
  ENDIF.

*** PBI - 73759 - Fim - CBRAND
  SELECT SINGLE *
     FROM TKA02
     INTO WL_TKA02
     WHERE BUKRS  = P_BUKRS.

  MOVE WL_TKA02-KOKRS TO VKOKRS.

  IF WG_FISCAL-COMPLEMENTO EQ 'N'
  OR WG_FISCAL-COMPLEMENTO EQ 'S'
  OR WG_FISCAL-COMPLEMENTO IS INITIAL.
    IF TG_ITENS[] IS NOT INITIAL.
      SELECT *
        FROM MCHA
        INTO TABLE TL_MCHA
         FOR ALL ENTRIES IN TG_ITENS
         WHERE MATNR EQ TG_ITENS-MATNR
           AND WERKS EQ TG_ITENS-WERKS
           AND CHARG EQ TG_ITENS-CHARG.

      SELECT *
        FROM T001L
        INTO TABLE TL_T001L
         FOR ALL ENTRIES IN TG_ITENS
         WHERE WERKS EQ TG_ITENS-WERKS
           AND LGORT EQ TG_ITENS-LGORT.


      SELECT *
        FROM MARA
        INTO TABLE TL_MARA
         FOR ALL ENTRIES IN TG_ITENS
         WHERE MATNR EQ TG_ITENS-MATNR.

      SELECT *
        FROM T001K
        INTO TABLE TL_T001K
         FOR ALL ENTRIES IN TG_ITENS
         WHERE BWKEY EQ TG_ITENS-WERKS.


    ENDIF.
    IF TG_MOVEST[] IS NOT INITIAL.
      SELECT  *
        FROM ZFIWRT2000
        INTO TABLE TL_2000
         FOR ALL ENTRIES IN TG_MOVEST
         WHERE BWART EQ TG_MOVEST-BWART.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM ZFIWRT2001
          INTO TABLE TL_2001
          FOR ALL ENTRIES IN TL_2000
           WHERE BWART       EQ TL_2000-BWART
             AND FUNCTION_NAME EQ TL_2000-FUNCTION_NAME.

        IF SY-SUBRC IS INITIAL.
          SELECT *
            FROM ZFIWRT2002
            INTO TABLE TL_2002
            FOR ALL ENTRIES IN TL_2001
             WHERE BWART EQ TL_2001-BWART
               AND FUNCTION_NAME EQ TL_2001-FUNCTION_NAME.

        ENDIF.

      ENDIF.

    ELSEIF WG_FISCAL-LM_ESTOQUE EQ 'S'.
      MOVE: 'Não existe parametro para movimentação do estoque!'                  TO TG_MSG_RET-MSG,
         C_TAB_STRIP_NF-TAB5        TO TG_MSG_RET-ABA,
         'P_PARID'                  TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    SELECT SINGLE *
      FROM ZFIWRT0001
      INTO @DATA(WL_0001_1)
       WHERE OPERACAO EQ @P_OPERACAO.

    IF WG_FISCAL-LM_ESTOQUE EQ 'S' AND WL_0001_1-LM_INDEA NE 'S'.
      IF TG_APROV[] IS INITIAL.
        MOVE: 'Não existe aprovadores para mov. estoque!'                  TO TG_MSG_RET-MSG,
         C_TAB_STRIP_NF-TAB5        TO TG_MSG_RET-ABA,
         'P_PARID'                  TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.


    IF TG_CONTAB[] IS NOT INITIAL.
      SELECT *
        FROM T042Z
        INTO TABLE TL_T042Z
         FOR ALL ENTRIES IN TG_CONTAB
         WHERE ZLSCH EQ TG_CONTAB-ZLSCH
           AND LAND1 EQ 'BR'.

      SELECT *
        FROM TBSL
        INTO TABLE TL_TBSL
        FOR ALL ENTRIES IN TG_CONTAB
         WHERE BSCHL EQ TG_CONTAB-BSCHL.

* ---> S4 Migration - 18/07/2023 - CA
*      SELECT *
*        FROM cskb
*        INTO TABLE tl_cskb
*        FOR ALL ENTRIES IN tg_contab
*         WHERE kstar EQ tg_contab-hkont
*           AND  ( datbi GE sy-datum
*              AND datab LE sy-datum )
*           AND katyp EQ '01'.

      LOOP AT TG_CONTAB INTO DATA(LS_CONTAB).

        LV_CONTROLLINGAREA  = VKOKRS.
        LV_COSTELEMENT      = LS_CONTAB-HKONT.
        LV_KEYDATE          = SY-DATUM.

        CLEAR: LT_RETURNS[], LS_COELDES.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            CONTROLLINGAREA   = LV_CONTROLLINGAREA
            COSTELEMENT       = LV_COSTELEMENT
            KEYDATE           = LV_KEYDATE
          IMPORTING
            COSTELEMENTDETAIL = LS_COELDES
          TABLES
            RETURN            = LT_RETURNS.

        READ TABLE LT_RETURNS TRANSPORTING NO FIELDS WITH KEY TYPE = 'E'.
        IF SY-SUBRC <> 0 AND
           LS_COELDES-CELEM_CATEGORY = '01'.

          LS_CSKB-KOKRS = VKOKRS.
          LS_CSKB-KSTAR = LS_CONTAB-HKONT.
          LS_CSKB-KATYP = LS_COELDES-CELEM_CATEGORY.

          APPEND LS_CSKB TO TL_CSKB.
          CLEAR LS_CSKB.
        ENDIF.

        CLEAR: LS_CONTAB.
      ENDLOOP.
* <--- S4 Migration - 18/07/2023 - CA

      SELECT *
        FROM CSKS
        INTO TABLE TL_CSKS
         FOR ALL ENTRIES IN TG_CONTAB
         WHERE  KOKRS = VKOKRS
           AND  KOSTL EQ TG_CONTAB-KOSTL
           AND  ( DATBI GE SY-DATUM
              AND DATAB LE SY-DATUM ).
    ENDIF.
    IF TG_PARC[] IS NOT INITIAL.
      SELECT *
        FROM KNB1
        INTO TABLE TL_KNB1
         FOR ALL ENTRIES IN TG_PARC
         WHERE KUNNR EQ TG_PARC-PARID
           AND BUKRS EQ P_BUKRS.

      SELECT *
        FROM LFB1
        INTO TABLE TL_LFB1
         FOR ALL ENTRIES IN TG_PARC
         WHERE LIFNR EQ TG_PARC-PARID
           AND BUKRS EQ P_BUKRS.
    ENDIF.

    IF WL_0001_1-AVISO_REC EQ ABAP_TRUE.

      "Verificar Parceiro SP
      READ TABLE TG_PARC WITH KEY PARVW = 'SP' TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: C_TAB_STRIP_NF-TAB9     TO TG_MSG_RET-ABA.
        TG_MSG_RET-MSG = TEXT-E82.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      "Verificar Parceiro LC
      READ TABLE TG_PARC WITH KEY PARVW = 'LC' TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: C_TAB_STRIP_NF-TAB9     TO TG_MSG_RET-ABA.
        TG_MSG_RET-MSG = TEXT-E83.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      "Verificar Parceiro LR
      READ TABLE TG_PARC WITH KEY PARVW = 'LR' TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: C_TAB_STRIP_NF-TAB9     TO TG_MSG_RET-ABA.
        TG_MSG_RET-MSG = TEXT-E84.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.

    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'PARID'.
    IF SY-SUBRC IS INITIAL.
      IF P_PARID IS INITIAL.
        MOVE: TEXT-E79                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'P_PARID'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'MOVE_STLOC'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-MOVE_STLOC IS INITIAL.
        MOVE: TEXT-E25                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-MOVE_STLOC'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

    IF P_PARVW = C_BR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = P_PARID
        IMPORTING
          OUTPUT = WL_PARID2.
      CONDENSE WL_PARID2 NO-GAPS.
      IF STRLEN( WL_PARID2 ) GT 4.
        MOVE: 'Código de parceiro Filial Inválido!'                  TO TG_MSG_RET-MSG,
        C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
        'P_PARID'                  TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

    IF WG_FISCAL-LM_ESTOQUE NE 'S'.
      IF WG_FISCAL-INCO1 IS INITIAL.
        MOVE: TEXT-E28                   TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
              'WG_FISCAL-INCO1'          TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

      IF WG_FISCAL-INCO2 IS INITIAL.
        MOVE: TEXT-E28                   TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
              'WG_FISCAL-INCO2'          TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'MOVE_PLANT'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-MOVE_PLANT IS INITIAL.
        MOVE: TEXT-E27                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-MOVE_PLANT'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'KONTO'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-KONTO IS INITIAL.
        MOVE: TEXT-E65                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-KONTO'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WG_FISCAL-KONTO
          IMPORTING
            OUTPUT = WG_FISCAL-KONTO.
      ENDIF.

    ENDIF.


    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'MOVE_MAT'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-MOVE_MAT IS INITIAL.
        MOVE: TEXT-E66                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-MOVE_MAT'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WG_FISCAL-MOVE_MAT
          IMPORTING
            OUTPUT = WG_FISCAL-MOVE_MAT.
      ENDIF.
    ENDIF.

    READ TABLE TL_2002
      WITH KEY PAR_VALUE2 = 'MOVE_BATCH'.
    IF SY-SUBRC IS INITIAL.
      IF WG_FISCAL-MOVE_BATCH IS INITIAL.
        MOVE: TEXT-E67                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-MOVE_BATCH'     TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WG_FISCAL-TP_MV_IMOB = 'T'. " transferência
      IF WG_FISCAL-MOVE_PLANT IS INITIAL.
        MOVE: TEXT-E27                TO TG_MSG_RET-MSG,
           C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
           'WG_FISCAL-MOVE_PLANT'     TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSE.
        SELECT SINGLE *
          FROM T001W
          INTO WL_T001W
          WHERE WERKS = WG_FISCAL-MOVE_PLANT.
        IF SY-SUBRC NE 0.
          MOVE: TEXT-E48             TO TG_MSG_RET-MSG,
          C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
          'WG_FISCAL-MOVE_PLANT'     TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

      IF WG_FISCAL-KOSTL IS INITIAL.
        MOVE: TEXT-E44                TO TG_MSG_RET-MSG,
         C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
         'WG_FISCAL-KOSTL'     TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WG_FISCAL-KOSTL
          IMPORTING
            OUTPUT = WG_FISCAL-KOSTL.
        SELECT SINGLE *
             FROM CSKS
             INTO WL_CSKS
             WHERE  KOKRS = VKOKRS
                AND   KOSTL EQ WG_FISCAL-KOSTL
                AND  ( DATBI GE SY-DATUM
                   AND DATAB LE SY-DATUM ).
        IF SY-SUBRC NE 0.
          MOVE: TEXT-E45                TO TG_MSG_RET-MSG,
          C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
          'WG_FISCAL-KOSTL'     TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
    ENDIF.

    "Valida Request
    IF XBOL = 'N'.
      LOOP AT TG_CONTAB.
        IF TG_CONTAB-BSCHL EQ '01'
          AND TG_CONTAB-ZLSCH NE 'D'.
          MOVE: TEXT-E43       TO TG_MSG_RET-MSG,
                'ZLSCH'        TO TG_MSG_RET-FIELD.

          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET, WG_DESC_OPERACAO.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WG_FISCAL-NFTYPE.

    SELECT SINGLE * FROM ZFIWRT0001 INTO WL_0001 WHERE OPERACAO EQ P_OPERACAO.

    IF WL_0001 IS INITIAL.
      MOVE: TEXT-E02            TO TG_MSG_RET-MSG,
*            C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            'P_OPERACAO'        TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET, WG_DESC_OPERACAO.
    ELSE.
      IF ( WL_0001-REFERENCIA EQ 'S' ) AND ( WG_FISCAL-REFERENCIA IS INITIAL ).
        MOVE: TEXT-E46               TO TG_MSG_RET-MSG,
              'WG_FISCAL-REFERENCIA' TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET, WG_DESC_OPERACAO.
      ELSE.
        MOVE: WL_0001-DESCRICAO TO WG_DESC_OPERACAO.
      ENDIF.

*>>>>>>>Melhoria 150184 CS2024000781 Aprovações ZNFW - PSA
      IF  WL_0001-OPERACAO IS NOT INITIAL AND ( WL_0001-DT_FIM_VAL < SY-DATUM OR WL_0001-STATUS_APROV <> 'A' ). "150184 CS2024000781 Aprovações ZNFW - PSA
        MOVE:
        C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
        TG_MSG_RET-MSG = TEXT-E96.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*>>>>>>>Melhoria 150184 CS2024000781 Aprovações ZNFW - PSA

    ENDIF.

    IF NOT ( WG_FISCAL-REFERENCIA IS INITIAL ).

      SELECT SINGLE * FROM J_1BNFDOC
        INTO WL_DOC
      WHERE DOCNUM EQ WG_FISCAL-REFERENCIA.

      IF SY-SUBRC NE 0.
        MOVE: TEXT-E47                 TO TG_MSG_RET-MSG,
              'WG_FISCAL-REFERENCIA'   TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET, WG_DESC_OPERACAO.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM T001 INTO WL_T001 WHERE BUKRS EQ P_BUKRS.

    IF SY-SUBRC IS INITIAL.
      MOVE: WL_T001-BUTXT TO WG_DESC_BUKRS.

      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_1BBRANCH
         WHERE BUKRS  EQ P_BUKRS
           AND BRANCH EQ P_BRANCH.

      IF SY-SUBRC IS NOT  INITIAL.
        MOVE: TEXT-E05            TO TG_MSG_RET-MSG,
             'P_BRANCH'           TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET, WG_DESC_BRANCH.
      ELSE.
        MOVE: WL_1BBRANCH-NAME TO WG_DESC_BRANCH.
      ENDIF.
    ELSE.
      MOVE: TEXT-E11            TO TG_MSG_RET-MSG,
            'P_BUKRS'           TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET, WG_DESC_BUKRS.
    ENDIF.

    IF WG_FISCAL-LM_ESTOQUE NE 'S'.
      SELECT SINGLE *
        FROM J_1BAD
        INTO WL_1BAD
         WHERE PARVW EQ P_PARVW.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE *
          FROM J_1BADT
          INTO WL_1BADT
           WHERE SPRAS EQ SY-LANGU
             AND PARVW EQ P_PARVW.

        MOVE: WL_1BADT-PARTXT TO WG_DESC_PARVW.

        IF P_PARVW EQ C_AG.
          SELECT SINGLE *
            FROM KNA1
            INTO WL_KNA1
             WHERE KUNNR EQ P_PARID.

          IF SY-SUBRC IS NOT INITIAL.
            MOVE: TEXT-E06            TO TG_MSG_RET-MSG,
                 'P_PARID'            TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET, WG_DESC_PARID.
          ELSE.
            IF WL_KNA1-SPERR = 'X'.
              CONCATENATE TEXT-E56 ''  WL_PARID TEXT-E55 '' WL_LINHA INTO  TG_MSG_RET-MSG.
              MOVE: 'P_PARID'            TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET, WG_DESC_PARID.
            ELSE.
              SELECT SINGLE *
                FROM KNB1
                INTO WL_KNB1
                 WHERE KUNNR EQ P_PARID
                   AND BUKRS EQ P_BUKRS.
              IF SY-SUBRC IS NOT INITIAL.
                MOVE: TEXT-E31             TO TG_MSG_RET-MSG,
                      'P_PARID'            TO TG_MSG_RET-FIELD.
                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET, WG_DESC_PARID.
              ELSE.
                MOVE: WL_KNA1-NAME1 TO WG_DESC_PARID,
                      WL_KNA1-STCD1 TO WL_CNPJ.
              ENDIF.
            ENDIF.
          ENDIF.

        ELSEIF P_PARVW EQ C_LF
          OR   P_PARVW EQ C_BR.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = P_PARID
            IMPORTING
              OUTPUT = WL_PARID.

          SELECT SINGLE *
            FROM LFA1
            INTO WL_LFA1
             WHERE LIFNR EQ WL_PARID.

          IF SY-SUBRC IS NOT  INITIAL.
            MOVE: TEXT-E07            TO TG_MSG_RET-MSG,
                 'P_PARID'            TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET, WG_DESC_PARID.
          ELSE.
            IF WL_LFA1-SPERR = 'X'.
              CONCATENATE TEXT-E54 ''  WL_PARID TEXT-E55 '' WL_LINHA INTO TG_MSG_RET-MSG.
              MOVE: 'P_PARID'            TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET, WG_DESC_PARID.
            ELSE.
              MOVE: WL_LFA1-NAME1 TO WG_DESC_PARID,
                    WL_LFA1-STCD1 TO WL_CNPJ.
            ENDIF.
          ENDIF.

        ENDIF.

      ELSE.
        MOVE: TEXT-E09            TO TG_MSG_RET-MSG,
              'P_PARVW'           TO TG_MSG_RET-FIELD.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET, WG_DESC_PARVW.
      ENDIF.
    ENDIF.
    SORT TL_LFB1 BY LIFNR.
    SORT TL_KNB1 BY KUNNR.

    IF WG_FISCAL-LM_ESTOQUE NE 'S'.
      LOOP AT TG_PARC.
        WL_LINHA = SY-TABIX.
        IF TG_PARC-PARID NE P_PARID.
          IF TG_PARC-PARVW EQ 'LF'
          OR TG_PARC-PARVW EQ 'BR'
          OR TG_PARC-PARVW EQ 'Z1'
          OR TG_PARC-PARVW EQ 'SP'
          OR TG_PARC-PARVW EQ 'PC'.
            READ TABLE TL_LFB1 WITH KEY LIFNR = TG_PARC-PARID
                                        BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              MOVE: C_TAB_STRIP_NF-TAB9     TO TG_MSG_RET-ABA.

              CONCATENATE TEXT-E37 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ELSEIF TL_LFB1-SPERR = 'X'.
              CONCATENATE TEXT-E54 ''  WL_PARID TEXT-E55 '' WL_LINHA INTO  TG_MSG_RET-MSG.
              MOVE: 'P_PARID'            TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET, WG_DESC_PARID.
            ENDIF.
          ELSEIF TG_PARC-PARVW IS INITIAL.
            MOVE: C_TAB_STRIP_NF-TAB1     TO TG_MSG_RET-ABA.

            CONCATENATE TEXT-E35 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ELSE.
            READ TABLE TL_KNB1 WITH KEY KUNNR = TG_PARC-PARID
                                         BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              MOVE: C_TAB_STRIP_NF-TAB9     TO TG_MSG_RET-ABA.

              CONCATENATE TEXT-E36 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ELSEIF TL_KNA1-SPERR = 'X'.
              CONCATENATE TEXT-E56 ''  WL_PARID TEXT-E55 '' WL_LINHA INTO  TG_MSG_RET-MSG.
              MOVE: 'P_PARID'            TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET, WG_DESC_PARID.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF WG_FISCAL-CTRL_ZRFL EQ C_S.
      READ TABLE TG_PARC
        WITH KEY PARVW = 'Z1'.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: TEXT-E40            TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB9 TO TG_MSG_RET-ABA.
*          'WG_FISCAL-DOCREF'  to tg_msg_ret-field.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.


    IF ( WG_FISCAL-RETORNO EQ C_S  ) AND ( WG_FISCAL-DOCREF IS INITIAL ) AND ( NOT ( TG_DOCREFS[] IS NOT INITIAL AND WG_DOCS-TCODE_ORG IS NOT INITIAL ) ).
      MOVE: TEXT-E03            TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1 TO TG_MSG_RET-ABA,
            'WG_FISCAL-DOCREF'  TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.


    IF WG_FISCAL-AVISO_REC EQ C_S
    AND WG_FISCAL-EBELN IS INITIAL.
      MOVE: TEXT-E42            TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1 TO TG_MSG_RET-ABA,
            'WG_FISCAL-EBELN'  TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_FISCAL-ZPESAGEM EQ C_01
    AND WG_FISCAL-NR_ROMANEIO IS INITIAL.
      MOVE: TEXT-E04                TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1     TO TG_MSG_RET-ABA,
            'WG_FISCAL-NR_ROMANEIO' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ENDIF.


    IF WG_DOCS-BUDAT  IS INITIAL.
      MOVE: TEXT-E17                TO TG_MSG_RET-MSG,
            SPACE                   TO TG_MSG_RET-ABA,
            'WG_DOCS-BUDAT' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_DOCS-BLDAT  IS INITIAL.
      MOVE: TEXT-E18                TO TG_MSG_RET-MSG,
            SPACE                   TO TG_MSG_RET-ABA,
            'WG_DOCS-BLDAT' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    "Validação Parametros e Montagem da Contabilização...
    SELECT *
      FROM ZFIWRT0003 INTO TABLE @DATA(LIT_0003)
     WHERE OPERACAO EQ @P_OPERACAO.

    LOOP AT LIT_0003 INTO DATA(LWA_0003).

      READ TABLE TL_TBSL WITH KEY BSCHL = LWA_0003-BSCHL.

      CHECK SY-SUBRC EQ 0.

      IF ( TL_TBSL-KOART EQ C_K ) OR ( TL_TBSL-KOART EQ C_D ).

        SELECT SINGLE *
          FROM J_1BAD INTO @DATA(LWA_J_1BAD)
         WHERE PARVW EQ @P_PARVW.

        IF SY-SUBRC EQ 0.

          LV_PARID_KEY = P_PARID.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LV_PARID_KEY
            IMPORTING
              OUTPUT = LV_PARID_KEY.

          CASE LWA_J_1BAD-PARTYP.
            WHEN 'C'.

              SELECT SINGLE *
                FROM KNA1 INTO @DATA(_WL_KNA1)
               WHERE KUNNR = @LV_PARID_KEY.

              IF SY-SUBRC EQ 0.
                IF TL_TBSL-KOART = C_D.
                  MOVE P_PARID        TO TG_CONTAB-HKONT.
                ELSEIF TL_TBSL-KOART = C_K.

                  IF _WL_KNA1-LIFNR IS INITIAL.
                    CONCATENATE 'Cliente:' _WL_KNA1-KUNNR 'sem fornecedor vinculado na XD03/Aba Controle!' INTO LVA_MSG_TMP SEPARATED BY SPACE.
                    APPEND VALUE #( ABA = C_TAB_STRIP_NF-TAB1
                                    MSG = LVA_MSG_TMP ) TO TG_MSG_RET.
                  ELSE.
                    MOVE _WL_KNA1-LIFNR TO TG_CONTAB-HKONT.
                  ENDIF.

                ENDIF.
              ENDIF.

            WHEN 'B' OR 'V'.

              SELECT SINGLE *
                FROM LFA1 INTO @DATA(_WL_LFA1)
               WHERE LIFNR = @LV_PARID_KEY.

              IF SY-SUBRC EQ 0.
                IF TL_TBSL-KOART = C_K.
                  MOVE P_PARID        TO TG_CONTAB-HKONT.
                ELSEIF TL_TBSL-KOART = C_D.
                  IF _WL_LFA1-KUNNR IS INITIAL.
                    CONCATENATE 'Fornecedor:' _WL_LFA1-LIFNR 'sem cliente vinculado na XK03/Aba Controle!' INTO LVA_MSG_TMP SEPARATED BY SPACE.
                    APPEND VALUE #( ABA = C_TAB_STRIP_NF-TAB1
                                    MSG = LVA_MSG_TMP ) TO TG_MSG_RET.
                  ELSE.
                    MOVE _WL_LFA1-KUNNR TO TG_CONTAB-HKONT.
                  ENDIF.
                ENDIF.
              ENDIF.

          ENDCASE.

        ENDIF.

      ENDIF.

    ENDLOOP.

    LOOP AT TG_CONTAB.

      ADD TG_CONTAB-DMBTR TO  WL_TOT_CONTAB.

    ENDLOOP.
    IF WL_TOT_CONTAB NE 0.
      MOVE: TEXT-E24                TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB4     TO TG_MSG_RET-ABA,
            SPACE                   TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    SORT: TL_T042Z BY ZLSCH,
          TL_TBSL  BY BSCHL,
          TL_CSKB  BY KSTAR,
          TL_CSKS  BY KOSTL.

    CLEAR: WL_LINHA.

    LOOP AT TG_CONTAB.

      IF TG_CONTAB-DMBTR IS NOT INITIAL
      AND TG_CONTAB-ESTORNO IS INITIAL.
        ADD 1 TO WL_LINHA.
      ENDIF.
      READ TABLE TL_TBSL
        WITH KEY BSCHL = TG_CONTAB-BSCHL
                 BINARY SEARCH.

      "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
      "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
      IF TG_ITENS-CFOP(4) NE '1206' AND TG_ITENS-CFOP(4) NE '2206'.
        IF TL_TBSL-KOART EQ C_K OR TL_TBSL-KOART EQ C_D.

          IF TG_CONTAB-ZLSCH IS INITIAL
          AND TG_CONTAB-ESTORNO IS INITIAL.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                  C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E32 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ELSE.
            READ TABLE TL_T042Z
              WITH KEY ZLSCH = TG_CONTAB-ZLSCH
                       BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL
            AND TG_CONTAB-ESTORNO IS INITIAL.
              MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                    C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
              CONCATENATE TEXT-E33 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.

          IF TG_CONTAB-ZFBDT IS INITIAL
          AND TG_CONTAB-ESTORNO IS INITIAL.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                   C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E34 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE TL_CSKB
        WITH KEY KSTAR = TG_CONTAB-HKONT
      BINARY SEARCH.

      IF SY-SUBRC IS INITIAL AND TG_CONTAB-DMBTR IS NOT INITIAL AND TG_CONTAB-ESTORNO EQ ABAP_FALSE.
        IF TG_CONTAB-KOSTL IS INITIAL AND TL_CSKB-KATYP EQ '01'.
          MOVE: C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E38 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ELSEIF TG_CONTAB-KOSTL IS NOT INITIAL AND TL_CSKB-KATYP NE '01'.
          MOVE: C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
          MESSAGE S001 WITH TG_CONTAB-HKONT WL_LINHA INTO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ELSEIF TG_CONTAB-KOSTL IS NOT INITIAL.
          READ TABLE TL_CSKS WITH KEY KOSTL = TG_CONTAB-KOSTL BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            MOVE: C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E39 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.


      SELECT SINGLE * FROM ZFIWRT0008 INTO @DATA(WL_0008)
        WHERE SEQ_LCTO EQ  @P_SEQ_LCTO.

      IF  WL_0008-TCODE_ORG NE 'ZSDT0165'.

        IF TG_ITENS-CFOP(4) NE '1206' AND TG_ITENS-CFOP(4) NE '2206'.
          IF NOT ( TG_CONTAB-ZLSCH IS INITIAL ).
            CLEAR: WA_ZSDT0075.
            SELECT SINGLE * FROM ZSDT0075 INTO WA_ZSDT0075
             WHERE KUNNR = TG_CONTAB-HKONT
               AND BDATU >= SY-DATUM.

            IF ( SY-SUBRC NE 0 ) AND ( TG_CONTAB-ZLSCH NE C_D ).
              MOVE: C_TAB_STRIP_NF-TAB4 TO TG_MSG_RET-ABA.
              CONCATENATE TEXT-E43 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    DESCRIBE TABLE TG_ITENS LINES WL_LINHA.
    IF WL_LINHA EQ 0 .
      MOVE:    TEXT-E10            TO TG_MSG_RET-MSG,
               C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.



    IF WG_FISCAL-LM_ESTOQUE NE 'S'.
      IF WL_1BAA-FORM IS INITIAL.
        IF WG_DOCS-NFENUM  IS INITIAL.
          MOVE: TEXT-E16                TO TG_MSG_RET-MSG,
                SPACE                   TO TG_MSG_RET-ABA,
                'WG_DOCS-NFENUM' TO TG_MSG_RET-FIELD.

          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
        "
        IF WG_DOCS-SERIES EQ SPACE.
          MOVE: TEXT-E22                TO TG_MSG_RET-MSG,
                SPACE                   TO TG_MSG_RET-ABA,
                'WG_DOCS-SERIES' TO TG_MSG_RET-FIELD.

          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

      IF ( NOT WG_DOCS-NFENUM IS INITIAL ) AND ( NOT WG_DOCS-SERIES IS INITIAL ).

        CONCATENATE WG_DOCS-NFENUM '-' WG_DOCS-SERIES INTO P_XBLNR.

        DATA: V_CK_SOMENTE_DUP TYPE CHAR01.
        DATA: P_WERKS TYPE J_1BNFLIN-WERKS.
        P_WERKS = P_BRANCH.

        IF WG_DOCS-NOT_CHECK_XML EQ ABAP_TRUE.
          V_CK_SOMENTE_DUP = ABAP_TRUE.
        ELSE.
          V_CK_SOMENTE_DUP = ABAP_FALSE.
        ENDIF.

        CLEAR: LV_TOTITENS.

        LV_TOTITENS  = REDUCE J_1BNETVAL( INIT X TYPE J_1BNETVAL FOR B IN TG_ITENS NEXT X = X + B-NETWR ).
        LV_VLR_ITENS = LV_TOTITENS.

        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            P_LIFNR          = P_PARID
            P_PARVW          = P_PARVW
            P_NFTYPE         = WG_FISCAL-NFTYPE
            P_XBLNR          = P_XBLNR
            P_DATA           = WG_DOCS-BLDAT
            P_WERKS          = P_WERKS
            P_VALOR_NF       = LV_VLR_ITENS
            P_CK_SOMENTE_DUP = V_CK_SOMENTE_DUP
          EXCEPTIONS
            ERROR            = 1
            OTHERS           = 2.
        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO TG_MSG_RET-MSG.
          TG_MSG_RET-FIELD = 'P_PARID'.
          APPEND TG_MSG_RET.
          TG_MSG_RET-FIELD = 'WG_DOCS-NFENUM'.
          APPEND TG_MSG_RET.
          TG_MSG_RET-FIELD = 'WG_DOCS-SERIES'.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.


        SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WG_FISCAL-NFTYPE.
        "CS2017000751/CS2017002639
        IF WL_1BAA-DIRECT = '1'. "ENTRADA
          SELECT SINGLE *
              FROM ZFIWRT0008
              INTO WA_ZFIWRT0008
            WHERE PARID   = P_PARID "fornecedor
            AND   NFENUM  = WG_DOCS-NFENUM
            AND   SERIES  = WG_DOCS-SERIES
            AND   BLDAT   = WG_DOCS-BLDAT.
        ELSE.
          SELECT SINGLE *
              FROM ZFIWRT0008
              INTO WA_ZFIWRT0008
            WHERE NFENUM  = WG_DOCS-NFENUM
            AND   SERIES  = WG_DOCS-SERIES
            AND   BLDAT   = WG_DOCS-BLDAT
            AND   BRANCH  = P_BRANCH. "filial
        ENDIF.


        IF SY-SUBRC = 0.
          IF WA_ZFIWRT0008-SEQ_LCTO NE P_SEQ_LCTO AND WA_ZFIWRT0008-LOEKZ IS INITIAL.
            CLEAR VG_CANDAT.
            IF WA_ZFIWRT0008-DOCNUM IS NOT INITIAL.
              SELECT SINGLE CANDAT
                   INTO VG_CANDAT
                   FROM J_1BNFDOC
                   WHERE DOCNUM = WA_ZFIWRT0008-DOCNUM.

              IF VG_CANDAT IS INITIAL.
                CONCATENATE TEXT-E53  WA_ZFIWRT0008-SEQ_LCTO INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
                MOVE:   SPACE           TO TG_MSG_RET-ABA,
                       'WG_DOCS-NFENUM' TO TG_MSG_RET-FIELD.

                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      "18.02.2019 CS2018003078
      "IF ( WL_1BAA-FORM IS INITIAL ) AND ( WL_1BAA-NFE IS NOT INITIAL ) AND ( WL_1BAA-DIRECT EQ '2' OR WL_1BAA-NFTYPE EQ 'YI'  ).
      IF VLANCAMENTO_ZNFW0009 IS NOT INITIAL.

        IF WG_FISCAL-ACCESS_KEY IS INITIAL.
          MOVE: TEXT-E63                TO TG_MSG_RET-MSG,
             C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
             'WG_FISCAL-ACCESS_KEY'     TO TG_MSG_RET-FIELD.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ELSE.
          CLEAR: LS_ACCKEY_STR, LV_NFNUM9, LV_SERIES, LV_PARID_KEY.
          "Check Fields Acesso
          MOVE WG_FISCAL-ACCESS_KEY TO LS_ACCKEY_STR.

          DATA(_INVALID) = ABAP_FALSE.

          IF STRLEN( WG_FISCAL-ACCESS_KEY ) NE 44.
            _INVALID = ABAP_TRUE.
          ENDIF.

          IF NOT  WG_DOCS-BLDAT+2(2) = LS_ACCKEY_STR-NFYEAR OR
             NOT  WG_DOCS-BLDAT+4(2) = LS_ACCKEY_STR-NFMONTH.
            _INVALID = ABAP_TRUE.
          ENDIF.

          IF NOT WL_1BAA-MODEL IS INITIAL AND
             NOT WL_1BAA-MODEL = LS_ACCKEY_STR-MODEL.
            _INVALID = ABAP_TRUE.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WG_DOCS-NFENUM
            IMPORTING
              OUTPUT = LV_NFNUM9.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WG_DOCS-SERIES
            IMPORTING
              OUTPUT = LV_SERIES.

          IF NOT LV_NFNUM9 IS INITIAL AND
             NOT LV_NFNUM9 = LS_ACCKEY_STR-NFNUM9.
            _INVALID = ABAP_TRUE.
          ENDIF.

          IF LV_SERIES <> LS_ACCKEY_STR-SERIE.
            _INVALID = ABAP_TRUE.
          ENDIF.

          "Validar CNPJ Emissor da Chave de Acesso
          CLEAR: LV_STCD1_CK.

          IF VLANCAMENTO_ZNFW0009 = 'MIC'.

            LV_PARID_KEY = P_BRANCH.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = LV_PARID_KEY
              IMPORTING
                OUTPUT = LV_PARID_KEY.

            SELECT SINGLE *
              FROM LFA1 INTO _WL_LFA1
             WHERE LIFNR EQ LV_PARID_KEY.

            IF SY-SUBRC EQ 0.
              LV_STCD1_CK = _WL_LFA1-STCD1.
            ENDIF.

          ELSE.
            SELECT SINGLE *
              FROM J_1BAD INTO @DATA(WL_J_1BAD)
             WHERE PARVW EQ @P_PARVW.

            IF SY-SUBRC EQ 0.

              LV_PARID_KEY = P_PARID.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = LV_PARID_KEY
                IMPORTING
                  OUTPUT = LV_PARID_KEY.

              CASE WL_J_1BAD-PARTYP.
                WHEN 'C'.

                  SELECT SINGLE *
                    FROM KNA1 INTO _WL_KNA1
                   WHERE KUNNR = LV_PARID_KEY.

                  IF SY-SUBRC EQ 0.
                    LV_STCD1_CK = _WL_KNA1-STCD1.
                  ENDIF.

                WHEN 'B' OR 'V'.

                  SELECT SINGLE *
                    FROM LFA1 INTO _WL_LFA1
                   WHERE LIFNR = LV_PARID_KEY.

                  IF SY-SUBRC EQ 0.
                    IF _WL_LFA1-STCD1 IS NOT INITIAL.
                      LV_STCD1_CK = _WL_LFA1-STCD1.
                    ELSE. "CPF
                      LV_STCD1_CK2 = WL_LFA1-STCD2.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          INPUT  = LV_STCD1_CK2
                        IMPORTING
                          OUTPUT = LV_STCD1_CK2.
                      LV_STCD1_CK = LV_STCD1_CK2.
                    ENDIF.
                  ENDIF.
              ENDCASE.
            ENDIF.
          ENDIF.

          " 23.04.2024 - 133290 - RAMON -->
          DATA LV_STCD1(14).
          "lv_stcd1_ck         = |{ lv_stcd1_ck ALPHA = IN }|.
          LV_STCD1 = |{ LV_STCD1_CK ALPHA = IN }|.
          LV_STCD1_CK = LV_STCD1.
          " 23.04.2024 - 133290 - RAMON --<

          LS_ACCKEY_STR-STCD1 = |{ LS_ACCKEY_STR-STCD1 ALPHA = IN }|.

          IF LV_STCD1_CK <> LS_ACCKEY_STR-STCD1.
            _INVALID = ABAP_TRUE.
          ENDIF.

*-CS2021000595 - 22.06.2021 - JT - inicio
*         IF _invalid EQ abap_true.
*         IF _invalid EQ abap_true AND p_operacao <> '0093'.
          IF _INVALID EQ ABAP_TRUE AND NOT ( LV_SERIES GE '890' AND LV_SERIES LE '899' ).
*-CS2021000595 - 22.06.2021 - JT - fim
            MOVE: TEXT-E64             TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
            'WG_FISCAL-ACCESS_KEY'     TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    SORT: TL_MCHA BY MATNR WERKS CHARG,
          TL_T001L BY WERKS LGORT,
          TL_MARA BY MATNR,
          TL_T001K BY BWKEY.

    LOOP AT TG_ITENS.
      WL_LINHA = SY-TABIX.
      "CS2021000825
      IF P_BRANCH NE TG_ITENS-WERKS.
        SELECT SINGLE J_1BBRANCH
          FROM T001W
          INTO @DATA(_LOCAL)
        WHERE WERKS =  @TG_ITENS-WERKS.
        IF _LOCAL NE P_BRANCH.
          MOVE:
          C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E85 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
      "CS2021000825
      "
      SELECT SINGLE *
      FROM ZFIWRT0001
      INTO @DATA(WL_0001_)
       WHERE OPERACAO EQ @P_OPERACAO.
      IF SY-SUBRC = 0.
        IF  WL_0001_-OPERACAO IS NOT INITIAL AND ( WL_0001_-DT_FIM_VAL < SY-DATUM OR WL_0001_-STATUS_APROV <> 'A' ). "150184 CS2024000781 Aprovações ZNFW - PSA
          MOVE:
          C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          TG_MSG_RET-MSG = TEXT-E96.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

        IF WL_0001_-LM_INDEA = 'S'.
          V_EBELP = TG_ITENS-ITMNUM.
          SELECT SINGLE * FROM ZMMT0102 INTO WA_ZMMT0102 WHERE EBELN = P_SEQ_LCTO AND EBELP = V_EBELP.
          IF SY-SUBRC NE 0.
            MOVE:
            C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E78 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
*          IF TG_ITENS-RENAS IS INITIAL.
*            MOVE C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
*            CONCATENATE TEXT-E80 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*            APPEND TG_MSG_RET.
*            CLEAR: TG_MSG_RET.
*          ENDIF.
        ENDIF.
        IF WL_0001_-GE_REMESSA = 'S'.
          IF  TG_ITENS-VBELN IS INITIAL.
            MOVE:
            C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E68 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
          IF  TG_ITENS-POSNR IS INITIAL.
            MOVE:
            C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E69 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
          IF  TG_ITENS-VBELN IS NOT INITIAL AND
              TG_ITENS-POSNR IS NOT INITIAL.
            SELECT SINGLE *
              FROM VBAP
              INTO WL_VBAP
              WHERE VBELN = TG_ITENS-VBELN
              AND   POSNR = TG_ITENS-POSNR.
            IF  SY-SUBRC NE 0.
              MOVE:
              C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
              CONCATENATE TEXT-E70 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ELSEIF WL_VBAP-MATNR NE TG_ITENS-MATNR.
              MOVE:
              C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
              CONCATENATE TEXT-E71 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ELSE.
              IF P_BRANCH NE WL_VBAP-WERKS.
                MOVE:
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
                CONCATENATE TEXT-E74 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET.
              ENDIF.
              SELECT SINGLE *
                FROM VBAK
                INTO @DATA(WL_VBAK)
                WHERE VBELN = @TG_ITENS-VBELN.

              SELECT SINGLE *
                FROM KNA1
                INTO @DATA(WKNA1)
                WHERE KUNNR = @WL_VBAK-KUNNR.

              SELECT SINGLE *
                FROM J_1BAD INTO LWA_J_1BAD
               WHERE PARVW EQ P_PARVW.

              IF SY-SUBRC NE 0.
                TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB6.
                TG_MSG_RET-MSG = 'Cadastro tipo parceiro não encontrado:' && P_PARVW.
                APPEND TG_MSG_RET.
              ELSE.

                CASE LWA_J_1BAD-PARTYP.
                  WHEN 'C' OR 'B'.
                    IF WKNA1-KUNNR NE P_PARID.
                      TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB6.
                      CONCATENATE 'Cliente OV: ' WKNA1-KUNNR 'não corresponde ao parceiro do lançamento:' P_PARID INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
                      APPEND TG_MSG_RET.
                    ENDIF.
                  WHEN 'V'.

                    IF WKNA1-LIFNR IS INITIAL.

                      CONCATENATE 'Cliente:' WKNA1-KUNNR 'sem fornecedor vinculado na XD03/Aba Controle!' INTO LVA_MSG_TMP SEPARATED BY SPACE.
                      APPEND VALUE #( ABA = C_TAB_STRIP_NF-TAB1
                                      MSG = LVA_MSG_TMP ) TO TG_MSG_RET.
                    ELSE.

                      IF WKNA1-LIFNR NE P_PARID.
                        MOVE:
                        C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
                        CONCATENATE TEXT-E72 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
                        APPEND TG_MSG_RET.
                        CLEAR: TG_MSG_RET.
                      ELSE.
                        SELECT SINGLE *
                           FROM LFA1
                           INTO @DATA(WLFA1)
                           WHERE LIFNR = @P_PARID.
                        IF WKNA1-STCD1 NE WLFA1-STCD1.
                          MOVE:
                          C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
                          CONCATENATE TEXT-E73 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
                          APPEND TG_MSG_RET.
                          CLEAR: TG_MSG_RET.
                        ENDIF.
                      ENDIF.

                    ENDIF.

                ENDCASE.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.


        IF WL_0001_-AVISO_REC = 'S'.

          SELECT SINGLE * FROM EKPO INTO @DATA(WEKPO)
              WHERE EBELN EQ @WG_FISCAL-EBELN
               AND  EBELP EQ '00010'.

          IF WEKPO-BSTAE IS INITIAL.
            MOVE: C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E81 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'J_1B_MATERIAL_READ'
        EXPORTING
          MATNR               = TG_ITENS-MATNR
          VAL_AREA            = TG_ITENS-WERKS
          VAL_TYPE            = SPACE
          LANGUAGE            = SY-LANGU
          I_WERKS             = P_BRANCH
*      IMPORTING
*         nbm                 = sl_item-nbm
*         matuse              = sl_item-matuse
*         matorg              = sl_item-matorg
*         material_text_record = wa_material_text_record
*         e_matkl             = sl_item-matkl
        EXCEPTIONS
          MATERIAL_NOT_FOUND  = 1
          VALUATION_NOT_FOUND = 2
          OTHERS              = 3.
      IF SY-SUBRC IS NOT INITIAL.
*      wl_linha = sy-tabix.
        MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E15 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF TG_ITENS-MATNR IS INITIAL
      OR TG_ITENS-WERKS IS INITIAL
      OR TG_ITENS-NETWR IS INITIAL
      OR ( WG_FISCAL-IMOBILIZADO EQ C_S
      AND TG_ITENS-ANLN1 IS INITIAL ).
        IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S' AND TG_ITENS-NETWR IS INITIAL.
        ELSE.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E01 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

      ENDIF.

      IF TG_ITENS-MATNR IS NOT INITIAL
      AND TG_ITENS-WERKS IS NOT INITIAL
      AND TG_ITENS-CFOP IS INITIAL.
        IF  WG_FISCAL-LM_ESTOQUE NE 'S'.
          MOVE:   C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E12 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

      ENDIF.

      IF TG_ITENS-CHARG IS NOT INITIAL.
*      wl_linha = sy-tabix.
        READ TABLE TL_MCHA
          WITH KEY MATNR = TG_ITENS-MATNR
                   WERKS = TG_ITENS-WERKS
                   CHARG = TG_ITENS-CHARG
                   BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E13 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631
      IF TG_ITENS-ANLN1 IS NOT INITIAL.
        IF WG_FISCAL-IMOBILIZADO NE 'S' AND WG_FISCAL-TP_MV_IMOB NE 'C'.
          SELECT SINGLE *
           FROM ANLA
           INTO WL_ANLA
           WHERE  ANLN1 EQ TG_ITENS-ANLN1
            AND BUKRS EQ P_BUKRS.
          IF SY-SUBRC NE 0.
            CONCATENATE 'Este Imobilizado não pertence a empresa!' P_BUKRS INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

            MOVE: C_TAB_STRIP_NF-TAB3 TO TG_MSG_RET-ABA,
                 'TG_ITENS-ANLN1'  TO TG_MSG_RET-FIELD.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
        "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - inicio
        "validação duplicada ja faz na linha 9548
*        IF WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB EQ 'V'.
*          CLEAR WL_ANLA.
*          SELECT SINGLE *
*                 FROM ANLA
*                 INTO WL_ANLA
*                 WHERE ANLN1 EQ TG_ITENS-ANLN1
*                   AND BUKRS EQ P_BUKRS
*                   AND LEART IN ('03','04').
*
*          IF SY-SUBRC EQ 0.
*            TG_MSG_RET-MSG = 'Ativo imobilizado com restrições de movimentação. Por favor entrar em contato com imobilizado@amaggi.com.br'.
*            MOVE: C_TAB_STRIP_NF-TAB3 TO TG_MSG_RET-ABA,
*                 'TG_ITENS-ANLN1'  TO TG_MSG_RET-FIELD.
*            APPEND TG_MSG_RET.
*            CLEAR: TG_MSG_RET.
*          ENDIF.
*        ENDIF.
        "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - fim
        IF WG_FISCAL-IMOBILIZADO EQ 'S' AND WG_FISCAL-TP_MV_IMOB EQ 'T'.
          CLEAR TL_ANLC.
          SELECT *
                 INTO TABLE TL_ANLC
                 FROM ANLC
                 WHERE BUKRS EQ P_BUKRS
                   AND ANLN1 EQ TG_ITENS-ANLN1
                   AND AFABE EQ '5'.

          IF SY-SUBRC EQ 0.
            SORT TL_ANLC BY GJAHR DESCENDING.
            READ TABLE TL_ANLC INTO DATA(WL_ANLC) INDEX 1.
            IF WL_ANLC-ANSAZ > 0.
              TG_MSG_RET-MSG = ' EXISTE ADIANTAMENTO EM ABERTO PARA O IMOBILIZADO INFORMADO'.
              MOVE: C_TAB_STRIP_NF-TAB3 TO TG_MSG_RET-ABA,
                   'TG_ITENS-ANLN1'  TO TG_MSG_RET-FIELD.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.

***  =======================================================================================cs2023000072 - validação centro de custo transferências imobilizado / aoenning
*
*          "Se Centro de destino for diferente do centro informado no ID Parceiro: Bloquear lançamento.
*          vg_parid = |{ p_parid ALPHA = IN }|.
*          IF wg_fiscal-move_plant <> vg_parid.
*            tg_msg_ret-msg = 'Centro de destino é diferente do ID Parceiro'.
*            MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
*                  'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
*            APPEND tg_msg_ret.
*            CLEAR: tg_msg_ret.
*          ENDIF.
*
*          IF wg_fiscal-kostl IS NOT INITIAL.
*
*            "Consultar centro de custo.
*            SELECT SINGLE gsber FROM csks INTO @DATA(vg_gsber)
*              WHERE kostl EQ @wg_fiscal-kostl.
*            IF vg_gsber <> vg_parid. "Se Centro de custo for diferente do centro informado no ID Parceiro: Bloquear lançamento.
*              tg_msg_ret-msg = 'Centro de custo não pertence ao ID Parceiro informado'.
*              MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
*                  'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
*              APPEND tg_msg_ret.
*              CLEAR: tg_msg_ret.
*            ENDIF.
*          ENDIF.
        ENDIF.
*        CLEAR: vg_gsber, vg_parid.

***=============================================================================================CS2023000072 - Validação centro de custo transferências IMOBILIZADO / AOENNING

      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631

      IF WG_FISCAL-SERVICO EQ 'N'.
        IF TG_ITENS-STEUC IS INITIAL
        OR TG_ITENS-STEUC EQ 'NCM00'.
*        wl_linha = sy-tabix.

          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E19 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.

        ENDIF.
      ENDIF.
      IF TG_ITENS-LGORT IS NOT INITIAL.
*      wl_linha = sy-tabix.
        READ TABLE TL_T001L
          WITH KEY WERKS = TG_ITENS-WERKS
                   LGORT = TG_ITENS-LGORT
                   BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E14 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.


      READ TABLE TL_MARA
          WITH KEY MATNR = TG_ITENS-MATNR
                   BINARY SEARCH.

      IF TG_ITENS-CHARG IS INITIAL.
        IF TL_MARA-XCHPF IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E29 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

*      >>>>>> Inicio ajuste USER STORY 155374 / AOENNING.
      IF TL_MARA-MSTAE IS NOT INITIAL AND TL_MARA-MSTAE NE '03'.
        MOVE:  C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E58 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*      >>>>>> Fim ajuste USER STORY 155374 / AOENNING.

      IF TG_ITENS-WERKS IS NOT INITIAL.

        READ TABLE TL_T001K
          WITH KEY BWKEY = TG_ITENS-WERKS
                   BINARY SEARCH.
        IF TL_T001K-BUKRS NE P_BUKRS.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E30 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

        CLEAR WL_MARC.
        SELECT SINGLE *
          FROM MARC
          INTO WL_MARC
          WHERE MATNR = TG_ITENS-MATNR
          AND   WERKS = TG_ITENS-WERKS.
        IF WL_MARC-MMSTA  IS NOT INITIAL.
          MOVE:  C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E58 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

        IF WL_MARC-STEUC  IS INITIAL.
          MOVE:  C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
          CONCATENATE TEXT-E61 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

        IF TL_MARA-MTART NE 'ABF'.
          CLEAR WL_MBEW.
          SELECT SINGLE *
            FROM MBEW
            INTO WL_MBEW
            WHERE MATNR = TG_ITENS-MATNR
            AND   BWKEY = TG_ITENS-WERKS.
          IF WL_MBEW-MTUSE  IS INITIAL.
            MOVE:  C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E59 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
          IF WL_MBEW-MTORG   IS INITIAL.
            MOVE:  C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E60 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

      ENDIF.

      READ TABLE TL_2002
        WITH KEY PAR_VALUE  = 'ZFIWRT0009'
                 PAR_VALUE2 = 'LGORT'.
      IF SY-SUBRC IS INITIAL.
        IF TG_ITENS-LGORT IS INITIAL.
          MOVE: TEXT-E25                TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6     TO TG_MSG_RET-ABA.
*          'WG_FISCAL-MOVE_STLOC'     TO tg_msg_ret-field.
          CONCATENATE TEXT-E26 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.

        ENDIF.
      ENDIF.

      "Verificação de Documento Anulado Referênciado
      "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
      "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
      IF TG_ITENS-CFOP(4) EQ '1206' OR TG_ITENS-CFOP(4) EQ '2206'.
        DESCRIBE TABLE TG_DOCREFS LINES LC_QTD_LINHAS.
        IF LC_QTD_LINHAS IS INITIAL.
          TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB1.
          CONCATENATE TEXT-E49 'LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ELSE.
          LOOP AT TG_DOCREFS.

            "Verificar se Documento Existe
            SELECT SINGLE * INTO WA_J_1BNFDOC
              FROM J_1BNFDOC
             WHERE DOCNUM EQ TG_DOCREFS-DOCNUM.

            IF SY-SUBRC IS NOT INITIAL.
              TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB1.
              CONCATENATE TEXT-E51 'LINHA:' WL_LINHA 'DOC.:'  TG_DOCREFS-DOCNUM INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
              APPEND TG_MSG_RET.
              CLEAR: TG_MSG_RET.
            ELSE.
              "Verificar se Está cancelado
              IF WA_J_1BNFDOC-CANCEL EQ ABAP_TRUE.
                TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB1.
                CONCATENATE TEXT-E50 'LINHA:' WL_LINHA 'DOC.:'  TG_DOCREFS-DOCNUM INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
                APPEND TG_MSG_RET.
                CLEAR: TG_MSG_RET.
              ELSE.

                "Procura se o Documento está referênciado em outro documento
                SELECT SINGLE * INTO WA_ZFIWRT0020
                  FROM ZFIWRT0020
                 WHERE DOCNUM   EQ TG_DOCREFS-DOCNUM
                   AND SEQ_LCTO NE P_SEQ_LCTO.

                IF SY-SUBRC IS INITIAL.
                  SELECT SINGLE * INTO WA_ZFIWRT0008
                    FROM ZFIWRT0008
                   WHERE SEQ_LCTO EQ WA_ZFIWRT0020-SEQ_LCTO.

                  IF SY-SUBRC IS INITIAL AND WA_ZFIWRT0008-DOCNUM IS NOT INITIAL.

                    SELECT SINGLE * INTO WA_J_1BNFDOC
                      FROM J_1BNFDOC
                     WHERE DOCNUM EQ WA_ZFIWRT0008-DOCNUM
                       AND CANCEL EQ ABAP_FALSE.

                    IF SY-SUBRC IS INITIAL.
                      TG_MSG_RET-ABA = C_TAB_STRIP_NF-TAB1.
                      CONCATENATE TEXT-E52 'LINHA:' WL_LINHA 'DOC.:'  TG_DOCREFS-DOCNUM 'DOC.:' WA_ZFIWRT0008-DOCNUM INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
                      APPEND TG_MSG_RET.
                      CLEAR: TG_MSG_RET.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF WL_LINHA EQ 1.

          CLEAR: LV_TOTREF, LV_TOTITENS.

          LV_TOTREF   = REDUCE J_1BNFTOT( INIT X TYPE J_1BNFTOT FOR A IN TG_DOCREFS NEXT X = X + A-NFTOT ).
          LV_TOTITENS = REDUCE J_1BNETVAL( INIT X TYPE J_1BNETVAL FOR B IN TG_ITENS NEXT X = X + B-NETWR ).
          LV_TOTDIFF = ABS( LV_TOTREF - LV_TOTITENS ).
          IF LV_TOTDIFF GT '0.01'.
            APPEND VALUE #(
                            ABA = C_TAB_STRIP_NF-TAB1
                       MSG = 'Valor Total dos Documentos Referênciados Difere do Valor Total dos Itens Nota!'
                     ) TO TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

*-CS2020001331 - 06.10.2021 - JT - inicio
      READ TABLE TG_IMPO_COMP WITH KEY ITMNUM = TG_ITENS-ITMNUM
                                       TAXTYP = C_ICOP.
      IF SY-SUBRC = 0.
        IF TG_ITENS-POSSUI_ICMS_ST IS INITIAL.
          CONCATENATE 'Informar campo ICMS ST. Linha:' WL_LINHA INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

*Inicio CS2024001169 / AOENNING*
      IF TG_ITENS-NETWR < 0.
        CONCATENATE 'Valor total do item não pode ser negativo' WL_LINHA INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*Fim CS2024001169 / AOENNING*
    ENDLOOP.

    DATA: SOMA_VLR_TOTAL  LIKE TG_ITENS-NETWR,
          DIFERENCA       TYPE ZFIT0141-TOLERANCIA,
          DIFERENCA_ICMS  TYPE ZFIT0141-TOLERANCIA,
          TOLERANCIA_MIN  TYPE ZFIT0141-TOLERANCIA,
          TOLERANCIA_MAX  TYPE ZFIT0141-TOLERANCIA,
          LIT_MATNR_OV_PD LIKE TABLE OF GIT_MATNR_OV_PD,
          LVA_PEDIDO_OV   TYPE C LENGTH 20,
          LVA_VLR_IMPOSTO TYPE ZIB_NFE_DIST_ITM-ICMS_VALOR,
          LVA_VALOR_ICMS  TYPE ZIB_NFE_DIST_ITM-ICMS_VALOR,
          LVA_VLR_STR_1   TYPE C LENGTH 50,
          LVA_VLR_STR_2   TYPE C LENGTH 50,
          LIT_IMPO_AUX    LIKE TABLE OF TG_IMPO WITH HEADER LINE,
          VALIDA.

    IF WG_FISCAL-ACCESS_KEY IS NOT INITIAL.
      SELECT SINGLE TOLERANCIA
        FROM ZFIT0141
        INTO @DATA(TOLERANCIA)
        WHERE CHAVE EQ @WG_FISCAL-ACCESS_KEY.
    ENDIF.

    TOLERANCIA_MIN = TOLERANCIA * -1.
    TOLERANCIA_MAX = TOLERANCIA.
    SOMA_VLR_TOTAL = 0.

    IF ( VINICIOU_LCTO_ZNFW0009 IS NOT INITIAL ) AND ( TG_ITENS[] IS NOT INITIAL ) AND ( VLANCAMENTO_ZNFW0009 = 'MIC' OR VLANCAMENTO_ZNFW0009 = 'ONF' ).

      PERFORM F_GET_MATNR_OV_PD TABLES LIT_MATNR_OV_PD.

      DATA(LVA_VALIDA_ICMS) = ABAP_TRUE.

      SELECT *
        FROM SETLEAF INTO TABLE @DATA(LIT_SET_NCM_EXCECAO)
       WHERE SETNAME EQ 'ZNFW0002_VALIDA_NCM_EXC'.

*      select single *
*        from setleaf into @data(lwa_setleaf)
*       where setname eq 'ZNFW0002_VALIDA_ICMS_EXC'
*         and valfrom eq @p_operacao.
*
*      if sy-subrc eq 0.
*        lva_valida_icms = abap_false.
*      endif.

      CLEAR: TG_MARA_ITENS[].
      IF TG_ITENS[] IS NOT INITIAL.
        SELECT MATNR MATKL
          FROM MARA INTO TABLE TG_MARA_ITENS
           FOR ALL ENTRIES IN TG_ITENS
         WHERE MATNR EQ TG_ITENS-MATNR.
      ENDIF.

      LOOP AT TG_ITENS INTO DATA(WL_ITENS).

        DATA(LVA_TABIX) = SY-TABIX.

        DATA(LVA_VALIDA_NCM) = ABAP_TRUE.

        SOMA_VLR_TOTAL =  SOMA_VLR_TOTAL + WL_ITENS-NETWR.

        IF ( WG_FISCAL-EBELN IS NOT INITIAL OR WG_FISCAL-VBELN IS NOT INITIAL ).

          IF WG_FISCAL-EBELN IS NOT INITIAL.
            LVA_PEDIDO_OV = WG_FISCAL-EBELN.
          ELSEIF WG_FISCAL-VBELN IS NOT INITIAL..
            LVA_PEDIDO_OV = WG_FISCAL-VBELN.
          ENDIF.

          READ TABLE  LIT_MATNR_OV_PD TRANSPORTING NO FIELDS WITH KEY MATNR = WL_ITENS-MATNR.
          IF SY-SUBRC <> 0.
            CONCATENATE 'O material informado:' WL_ITENS-MATNR ' não existe no pedido/ordem de venda:' LVA_PEDIDO_OV '!'
                   INTO DATA(LVA_MSG_ERRO) SEPARATED BY SPACE.

            APPEND VALUE #(
                     ABA =  C_TAB_STRIP_NF-TAB6
                     MSG = LVA_MSG_ERRO
                        ) TO TG_MSG_RET.
          ENDIF.

        ENDIF.

        READ TABLE TG_MARA_ITENS WITH KEY MATNR = WL_ITENS-MATNR.
        IF ( SY-SUBRC EQ 0 ) AND ( TG_MARA_ITENS-MATKL IS NOT INITIAL ).
          READ TABLE LIT_SET_NCM_EXCECAO WITH KEY VALFROM = TG_MARA_ITENS-MATKL TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            LVA_VALIDA_NCM = ABAP_FALSE.
          ENDIF.
        ENDIF.

        IF LVA_VALIDA_NCM EQ ABAP_TRUE.
          DATA(LVA_NCM_ITEM_XML) = WL_ITENS-NCM_XML.
          DATA(LVA_NCM_ITEM_LCT) = WL_ITENS-STEUC.

          REPLACE ALL OCCURRENCES OF: '.' IN LVA_NCM_ITEM_XML  WITH ' ',
                                      '.' IN LVA_NCM_ITEM_LCT  WITH ' '.

          CONDENSE: LVA_NCM_ITEM_LCT, LVA_NCM_ITEM_XML NO-GAPS.
          "retirado CS2023000477
*          if lva_ncm_item_xml <> lva_ncm_item_lct.
*            concatenate 'NCM' lva_ncm_item_lct 'do material:' wl_itens-matnr 'não corresponde ao NCM do item do XML:' lva_ncm_item_xml '!'
*                     into lva_msg_erro separated by space.
*
*            append value #(
*                       aba =  c_tab_strip_nf-tab6
*                       msg = lva_msg_erro
*                          ) to tg_msg_ret.
*          endif.
          "retirado CS2023000477
        ENDIF.
*&====================================Inicio comentado e adicionado nova validação diferença ICMS. BUG SOLTO 145073/IR159163 / AOENNING
        "Validação Valor ICMS
*        if lva_valida_icms eq abap_true.

*          clear: lit_impo_aux[], lva_vlr_imposto.
*          perform monta_impostos tables lit_impo_aux using lva_tabix.
*
*          loop at lit_impo_aux into data(lwa_impo_aux) where taxtyp eq c_icm3.
*            add lwa_impo_aux-taxval to lva_vlr_imposto.
*          endloop.

*          IF ( lva_vlr_imposto <> wl_itens-vlr_icms_xml ).
*
*            diferenca_icms = lva_vlr_imposto - wl_itens-vlr_icms_xml.
*
*            IF ( NOT ( diferenca_icms BETWEEN tolerancia_min AND tolerancia_max ) ) AND abs( diferenca_icms ) > 1.
*
*              WRITE: lva_vlr_imposto       TO lva_vlr_str_1,
*                     wl_itens-vlr_icms_xml TO lva_vlr_str_2.
*
*              CONDENSE: lva_vlr_str_1, lva_vlr_str_2 NO-GAPS.
*
*              CONCATENATE 'Valor ICMS:' lva_vlr_str_1 'do item:' wl_itens-itmnum ' diferente do valor do ICMS do XML:' lva_vlr_str_2 '!'
*                       INTO lva_msg_erro SEPARATED BY space.
*
*              APPEND VALUE #(
*                         aba =  c_tab_strip_nf-tab6
*                         msg = lva_msg_erro
*                            ) TO tg_msg_ret.
*            ENDIF.
*
*          ENDIF.

*        endif.
*&====================================Fim comentado e adicionado nova validação diferença ICMS. BUG SOLTO 145073/IR159163 / AOENNING
      ENDLOOP.

*&====================================Inicio adicionado nova validação diferença ICMS. BUG SOLTO 145073/IR159163 / AOENNING
      SELECT SINGLE *
      FROM SETLEAF INTO @DATA(LWA_SETLEAF)
      WHERE SETNAME EQ 'ZNFW0002_VALIDA_ICMS_EXC'
      AND VALFROM EQ @P_OPERACAO.

      IF SY-SUBRC EQ 0.
        LVA_VALIDA_ICMS = ABAP_FALSE.
      ENDIF.

      IF LVA_VALIDA_ICMS EQ ABAP_TRUE.
        IF ( TG_ITENS[] IS NOT INITIAL ).
          "Validação Valor ICMS
          CLEAR: WL_ITENS.
          LOOP AT TG_ITENS INTO WL_ITENS.
            LVA_TABIX = SY-TABIX.

            CLEAR: LIT_IMPO_AUX, LVA_VLR_IMPOSTO. "Limpeza variavel IR175651.
            PERFORM MONTA_IMPOSTOS TABLES LIT_IMPO_AUX USING LVA_TABIX.
          ENDLOOP.

          LOOP AT LIT_IMPO_AUX INTO DATA(LWA_IMPO_AUX) WHERE TAXTYP EQ C_ICM3.
            ADD LWA_IMPO_AUX-TAXVAL TO LVA_VALOR_ICMS.
          ENDLOOP.

          LV_IV_ICMS = 'S'.
          EXPORT LV_IV_ICMS TO MEMORY ID 'ZIV_ICMS'.

          CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
            EXPORTING
              P_LIFNR      = P_PARID
              P_PARVW      = P_PARVW
              P_NFTYPE     = WG_FISCAL-NFTYPE
              P_XBLNR      = P_XBLNR
              P_DATA       = WG_DOCS-BLDAT
              P_WERKS      = P_WERKS
              P_VALOR_ICMS = LVA_VALOR_ICMS
              P_IVA        = WG_DIREITOS-TAXCODE
            EXCEPTIONS
              ERROR        = 1
              OTHERS       = 2.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO TG_MSG_RET-MSG.
            TG_MSG_RET-FIELD = 'P_PARID'.
            APPEND TG_MSG_RET.
            TG_MSG_RET-FIELD = 'WG_DOCS-NFENUM'.
            APPEND TG_MSG_RET.
            TG_MSG_RET-FIELD = 'WG_DOCS-SERIES'.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.

        ENDIF.
      ENDIF.
*&====================================Fim adicionado nova validação diferença ICMS. BUG SOLTO 145073/IR159163 / AOENNING


      "==================================================Ajuste no valor total BUG SOLTO #137346/ AOENNING
      "Verifica se o vlr total da nota esta preenchido.
      IF VLR_TOTAL_NOTA IS INITIAL.
        IF LIT_ZIB_NFE_DIST_ITM IS NOT INITIAL.
          LOOP AT LIT_ZIB_NFE_DIST_ITM INTO DATA(LWA_ZIB_NFE_DIST_ITM).
            CLEAR: VLR_TOTAL_ITEM.
            VLR_TOTAL_ITEM = LWA_ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B - LWA_ZIB_NFE_DIST_ITM-PROD_VL_DESCONTO + LWA_ZIB_NFE_DIST_ITM-PROD_VL_FRETE + LWA_ZIB_NFE_DIST_ITM-PROD_VL_SEGURO + LWA_ZIB_NFE_DIST_ITM-PROD_VL_OUTRO.
            ADD VLR_TOTAL_ITEM TO VLR_TOTAL_NOTA.
            CLEAR: LWA_ZIB_NFE_DIST_ITM.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "==================================================Ajuste no valor total BUG SOLTO #137346 / AOENNING

      DIFERENCA = SOMA_VLR_TOTAL - VLR_TOTAL_NOTA.

      IF ABS( DIFERENCA ) > 1.

        IF ( SOMA_VLR_TOTAL <> VLR_TOTAL_NOTA ) AND NOT ( DIFERENCA BETWEEN TOLERANCIA_MIN AND TOLERANCIA_MAX ).
          APPEND VALUE #(
                     ABA =  C_TAB_STRIP_NF-TAB6
                     MSG = 'O Valor total dos itens  não corresponde ao Valor da Nota Fiscal!'
                         ) TO TG_MSG_RET.
        ENDIF.
      ENDIF.

*** PBI - 68354 - Inicio - CBRAND
    ELSE.
      IF ( TG_ITENS[] IS NOT INITIAL ).
        "Validação Valor ICMS
        CLEAR: WL_ITENS.
        LOOP AT TG_ITENS INTO WL_ITENS.
          LVA_TABIX = SY-TABIX.

          CLEAR: LIT_IMPO_AUX, LVA_VLR_IMPOSTO. "Limpeza variavel IR175651.
          PERFORM MONTA_IMPOSTOS TABLES LIT_IMPO_AUX USING LVA_TABIX.
        ENDLOOP.

        LOOP AT LIT_IMPO_AUX INTO LWA_IMPO_AUX WHERE TAXTYP EQ C_ICM3.
          ADD LWA_IMPO_AUX-TAXVAL TO LVA_VALOR_ICMS.
        ENDLOOP.

*-CS2023000043-14.02.2023-#102019-JT-COMENTADO
        LV_IV_ICMS = 'S'.
        EXPORT LV_IV_ICMS TO MEMORY ID 'ZIV_ICMS'.
*-CS2023000043-14.02.2023-#102019-JT-COMENTADO

        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            P_LIFNR      = P_PARID
            P_PARVW      = P_PARVW
            P_NFTYPE     = WG_FISCAL-NFTYPE
            P_XBLNR      = P_XBLNR
            P_DATA       = WG_DOCS-BLDAT
            P_WERKS      = P_WERKS
            P_VALOR_ICMS = LVA_VALOR_ICMS
            P_IVA        = WG_DIREITOS-TAXCODE
          EXCEPTIONS
            ERROR        = 1
            OTHERS       = 2.
        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO TG_MSG_RET-MSG.
          TG_MSG_RET-FIELD = 'P_PARID'.
          APPEND TG_MSG_RET.
          TG_MSG_RET-FIELD = 'WG_DOCS-NFENUM'.
          APPEND TG_MSG_RET.
          TG_MSG_RET-FIELD = 'WG_DOCS-SERIES'.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.

      ENDIF.
*** PBI - 68354 - Fim - CBRAND
    ENDIF.


    IF ( WG_FISCAL-RETORNO EQ 'S' ) AND ( NOT ( TG_DOCREFS[] IS NOT INITIAL AND WG_DOCS-TCODE_ORG IS NOT INITIAL ) ).
      WL_SEQ_LCTO = WG_FISCAL-DOCREF.

      CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
        EXPORTING
          I_BUKRS       = P_BUKRS
          I_BRANCH      = P_BRANCH
          I_PARVW       = P_PARVW
          I_PARID       = P_PARID
          I_SEQ_LCTO    = WL_SEQ_LCTO
          I_SEQ_MODIFY  = P_SEQ_LCTO
          I_IMOBILIZADO = WG_FISCAL-IMOBILIZADO
        TABLES
          ET_ITENS      = TL_0009
        EXCEPTIONS
          SEM_SALDO     = 1
          OTHERS        = 2.

      IF SY-SUBRC <> 0.
        MOVE: TEXT-E21            TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB1 TO TG_MSG_RET-ABA,
              'WG_FISCAL-DOCREF'  TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      LOOP AT TL_0009.
        READ TABLE TG_ITENS
          WITH KEY ITMNUM = TL_0009-ITMNUM.
        IF SY-SUBRC IS INITIAL.
          IF TG_ITENS-MENGE GT TL_0009-MENGE.
            WL_LINHA = SY-TABIX.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            CONCATENATE TEXT-E20 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.
            CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.



*---------------------------------------------------------------------
  "Valida formulario
*---------------------------------------------------------------------
  IF WL_0001_1-NFTYPE IS NOT INITIAL
    AND P_BUKRS IS NOT INITIAL
     AND P_BRANCH IS NOT INITIAL.

    "Verifica-se existe formulario cadastrado
    SELECT SINGLE NFTYPE, FORM FROM J_1BAA
      INTO @DATA(WA_1BAA)
      WHERE NFTYPE = @WL_0001_1-NFTYPE.

    IF ( SY-SUBRC EQ 0 ) AND ( WA_1BAA-FORM IS NOT INITIAL ).

      "Verifica se existe formulário cadastrado
      SELECT SINGLE BUKRS, BRANCH, FORM FROM J_1BB2
        INTO @DATA(WL_1BB2)
        WHERE BUKRS  = @P_BUKRS
          AND BRANCH = @P_BRANCH
          AND FORM   = @WA_1BAA-FORM.

      IF ( SY-SUBRC NE 0 ) OR ( WL_1BB2-FORM IS INITIAL ).

        MESSAGE S000(Z_LES) WITH
'Formulário de impressão não configurado para a ' 'filial informada. Por favor criar ' 'FI para o departamento fiscal.'  INTO TG_MSG_RET-MSG.
        MOVE: C_TAB_STRIP_NF-TAB1        TO TG_MSG_RET-ABA,
              'WG_FISCAL-NFTYPE'         TO TG_MSG_RET-FIELD.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

    ENDIF.

  ENDIF.
*---------------------------------------------------------------------

  IF WG_FISCAL-COMPLEMENTO EQ 'S'.
    REFRESH TG_MSG_RET.
    IF WG_DOCS-BUDAT  IS INITIAL.
      MOVE: TEXT-E17                TO TG_MSG_RET-MSG,
            SPACE                   TO TG_MSG_RET-ABA,
            'WG_DOCS-BUDAT' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_DOCS-BLDAT  IS INITIAL.
      MOVE: TEXT-E18                TO TG_MSG_RET-MSG,
            SPACE                   TO TG_MSG_RET-ABA,
            'WG_DOCS-BLDAT' TO TG_MSG_RET-FIELD.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
    LOOP AT TG_ITENS.
      WL_LINHA = SY-TABIX.
      IF TG_ITENS-WERKS IS INITIAL.
        MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
              C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
        CONCATENATE TEXT-E41 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF WG_FISCAL-DISP_NF_CCT EQ 'S'.
    IF WG_TRANSPORTE-LIFNR IS INITIAL.
      MOVE: TEXT-E62                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB10       TO TG_MSG_RET-ABA,
            'WG_TRANSPORTE-LIFNR'      TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
    IF WG_TRANSPORTE-PLACA_CAR1 IS NOT INITIAL.
      SELECT SINGLE * FROM ZLEST0002 INTO WL_ZLEST0002 WHERE PC_VEICULO =  WG_TRANSPORTE-PLACA_CAR1.
      MOVE: TEXT-E75                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB10       TO TG_MSG_RET-ABA,
            'WG_TRANSPORTE-PLACA_CAR1'      TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
    IF WG_TRANSPORTE-PLACA_CAR2 IS NOT INITIAL.
      SELECT SINGLE * FROM ZLEST0002 INTO WL_ZLEST0002 WHERE PC_VEICULO =  WG_TRANSPORTE-PLACA_CAR2.
      MOVE: TEXT-E76                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB10       TO TG_MSG_RET-ABA,
            'WG_TRANSPORTE-PLACA_CAR2'      TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
    IF WG_TRANSPORTE-PLACA_CAR3 IS NOT INITIAL.
      SELECT SINGLE * FROM ZLEST0002 INTO WL_ZLEST0002 WHERE PC_VEICULO =  WG_TRANSPORTE-PLACA_CAR3.
      MOVE: TEXT-E76                   TO TG_MSG_RET-MSG,
            C_TAB_STRIP_NF-TAB10       TO TG_MSG_RET-ABA,
            'WG_TRANSPORTE-PLACA_CAR3'      TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  SORT TG_MSG_RET."150184 CS2024000781 Aprovações ZNFW - PSA
  DELETE ADJACENT DUPLICATES FROM TG_MSG_RET COMPARING MSG.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
*     I_SHOW        = C_X
      I_REPID       = SY-REPID
      I_PRESSED_TAB = 'G_TAB_STRIP_NF-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.


ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_APROV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_APROV .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZFIWRT0007' 'NIVEL_APROV'    'TG_APROV' 'NIVEL_APROV'     'Nivel Aprov.'  '10' ' ' ' ' ' ',
        2 'ZFIWRT0007' 'USNAM'          'TG_APROV' 'USNAM'           ' '  '8' ' ' ' ' ' ',
        3 'USER_ADDR'  ' '              'TG_APROV' 'NOME'            'Nome Completo'  '25'  ' ' ' ' ' ',
        4 'ZFIWRT0007' 'DEPARTAMENTO'   'TG_APROV' 'DEPARTAMENTO'    'Departamento'  '25'  ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_APROV
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_PARC .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
          1 'ZFIWRT0015' 'PARVW'        'TG_PARC' 'PARVW'     'Função Parceiro'  '10' ' ' ' ' ' ',
          2 'ZFIWRT0015' 'PARID'        'TG_PARC' 'PARID'     'Parceiro'  '8' ' ' ' ' ' ',
          3 ' '          ' '            'TG_PARC' 'NOME'      'Nome do Parceiro'  '25'  ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_PARC
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0260  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0260 INPUT.

ENDMODULE.                 " USER_COMMAND_0260  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_STYLE .
  DATA : STYLE TYPE LVC_T_STYL WITH HEADER LINE.

  LOOP AT TG_PARC.
    REFRESH: STYLE.
    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
      STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
      APPEND STYLE.
    ENDLOOP.
    INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.
    MODIFY TG_PARC.
  ENDLOOP.
ENDFORM.                    " MONTAR_STYLE
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM F_PREENCHER_DYNPRO USING L_START TYPE C L_NAME TYPE C L_VALUE.

  MOVE L_START TO WL_BDC-DYNBEGIN.
  IF L_START = 'X'.
    MOVE:
  L_NAME  TO WL_BDC-PROGRAM,
  L_VALUE TO WL_BDC-DYNPRO.
  ELSE.
    MOVE:
      L_NAME  TO WL_BDC-FNAM,
      L_VALUE TO WL_BDC-FVAL.
  ENDIF.
  APPEND WL_BDC TO TL_BDC.
  CLEAR: WL_BDC.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Module  MATCHCODE_DOCREF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MATCHCODE_DOCREF INPUT.
  DATA: BEGIN OF TL_DOCREF OCCURS 0,
          SEQ_LCTO TYPE ZFIWRT0008-SEQ_LCTO,
*         OPERACAO TYPE ZFIWRT0008-OPERACAO,
          BUKRS    TYPE ZFIWRT0008-BUKRS,
          BRANCH   TYPE ZFIWRT0008-BRANCH,
          PARVW    TYPE ZFIWRT0008-PARVW,
          PARID    TYPE ZFIWRT0008-PARID,
          NFENUM   TYPE ZFIWRT0008-NFENUM,
          SERIES   TYPE ZFIWRT0008-SERIES,
        END OF TL_DOCREF.

  DATA: TL_ITENS TYPE TABLE OF ZFIWRT0009 WITH HEADER LINE,
*        tl_bnfdoc type table of j_1bnfdoc with header line,
        TL_0008  TYPE TABLE OF ZFIWRT0008 WITH HEADER LINE.

  REFRESH: TL_DOCREF, T_FIELDTAB, TL_ITENS, TL_0008, TL_BNFDOC.
  CLEAR:   TL_DOCREF, T_FIELDTAB, TL_ITENS, TL_0008, TL_BNFDOC.

  CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
    EXPORTING
      I_BUKRS       = P_BUKRS
      I_BRANCH      = P_BRANCH
      I_PARVW       = P_PARVW
      I_PARID       = P_PARID
      I_IMOBILIZADO = WG_FISCAL-IMOBILIZADO
*     I_SEQ_LCTO    =
    TABLES
      ET_ITENS      = TL_ITENS
    EXCEPTIONS
      SEM_SALDO     = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não ha documentos com saldo'
                                           'existente para nota fiscal'
                                           'de retorno.'.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    SORT TL_ITENS BY SEQ_LCTO.
    DELETE ADJACENT DUPLICATES FROM TL_ITENS COMPARING SEQ_LCTO.
    IF TL_ITENS[] IS NOT INITIAL.
      SELECT *
        FROM ZFIWRT0008
        INTO TABLE TL_0008
         FOR ALL ENTRIES IN TL_ITENS
         WHERE SEQ_LCTO EQ TL_ITENS-SEQ_LCTO.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM J_1BNFDOC
          INTO TABLE TL_BNFDOC
           FOR ALL ENTRIES IN TL_0008
           WHERE DOCNUM EQ TL_0008-DOCNUM.

      ENDIF.
    ENDIF.

    SORT: TL_0008 BY SEQ_LCTO,
          TL_BNFDOC BY DOCNUM.

    LOOP AT TL_ITENS.
      READ TABLE TL_0008
        WITH KEY SEQ_LCTO = TL_ITENS-SEQ_LCTO
                    BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        READ TABLE TL_BNFDOC
          WITH KEY DOCNUM = TL_0008-DOCNUM
                    BINARY SEARCH.

      ENDIF.

      IF TL_BNFDOC-NFENUM IS NOT INITIAL.
        MOVE: TL_BNFDOC-NFENUM TO TL_DOCREF-NFENUM,
              TL_BNFDOC-SERIES TO TL_DOCREF-SERIES.
      ELSE.
        MOVE: TL_BNFDOC-NFNUM  TO TL_DOCREF-NFENUM,
              TL_BNFDOC-SERIES TO TL_DOCREF-SERIES.
      ENDIF.

      MOVE: TL_ITENS-SEQ_LCTO TO TL_DOCREF-SEQ_LCTO,
            P_BUKRS           TO TL_DOCREF-BUKRS,
            P_BRANCH          TO TL_DOCREF-BRANCH,
            P_PARVW           TO TL_DOCREF-PARVW,
            P_PARID           TO TL_DOCREF-PARID.

      APPEND TL_DOCREF.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'SEQ_LCTO'
        DYNPPROG        = SY-REPID                          "'ZFINR018'
        DYNPNR          = SY-DYNNR
        DYNPROFIELD     = 'WG_FISCAL-DOCREF'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = TL_DOCREF
        RETURN_TAB      = TL_RETURN_TAB
        DYNPFLD_MAPPING = TL_DSELC.
  ENDIF.
ENDMODULE.                 " MATCHCODE_DOCREF  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ITENS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ITENS INPUT.
  DATA: WL_DOCREF TYPE ZFIWRT0008-SEQ_LCTO,
        TL_MARC   TYPE TABLE OF MARC WITH HEADER LINE,
        TL_MAKT   TYPE TABLE OF MAKT WITH HEADER LINE,
        WL_HEADER TYPE ZFIWRT0008.

  WL_DOCREF = WG_FISCAL-DOCREF.

  IF WG_FISCAL-RETORNO EQ 'S'.
    REFRESH: TL_ITENS.

    CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
      EXPORTING
        I_BUKRS    = P_BUKRS
        I_BRANCH   = P_BRANCH
        I_PARVW    = P_PARVW
        I_PARID    = P_PARID
        I_SEQ_LCTO = WL_DOCREF
        I_OPERACAO = P_OPERACAO
        I_SHIPFROM = WG_SHIPFROM
        I_SHIPTO   = WG_SHIPTO
      IMPORTING
        E_HEADER   = WL_HEADER
      TABLES
        ET_ITENS   = TL_ITENS
      EXCEPTIONS
        SEM_SALDO  = 1
        OTHERS     = 2.
    IF SY-SUBRC IS INITIAL.
      CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
        EXPORTING
*         MODE_ZFIWRT0008       = 'E'
*         MANDT          = SY-MANDT
          SEQ_LCTO       = WL_DOCREF
*         X_SEQ_LCTO     = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC NE 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.

        IF P_PARVW EQ C_BR.
          MOVE: WL_HEADER-NFENUM TO WG_DOCS-NFENUM,
                WL_HEADER-SERIES TO WG_DOCS-SERIES,
                WL_HEADER-BLDAT  TO WG_DOCS-BLDAT.
        ENDIF.

        IF TL_ITENS[] IS NOT INITIAL.
          SELECT *
            FROM MARC
            INTO TABLE TL_MARC
             FOR ALL ENTRIES IN TL_ITENS
             WHERE MATNR EQ TL_ITENS-MATNR
               AND WERKS EQ TL_ITENS-BWKEY.

          SELECT *
            FROM MAKT
            INTO TABLE TL_MAKT
            FOR ALL ENTRIES IN TL_ITENS
             WHERE SPRAS EQ SY-LANGU
               AND MATNR EQ TL_ITENS-MATNR.

        ENDIF.

        SORT: TL_MARC BY MATNR WERKS,
              TL_MAKT BY MATNR.
        REFRESH: TG_ITENS.
        LOOP AT TL_ITENS.
          READ TABLE TL_MARC
            WITH KEY MATNR = TL_ITENS-MATNR
                     WERKS = TL_ITENS-BWKEY
                     BINARY SEARCH.

          READ TABLE TL_MAKT
            WITH KEY MATNR = TL_ITENS-MATNR
                     BINARY SEARCH.

          MOVE-CORRESPONDING: TL_ITENS TO TG_ITENS.
          MOVE: TL_ITENS-BWKEY TO TG_ITENS-WERKS,
                TL_MARC-STEUC  TO TG_ITENS-STEUC,
                TL_MAKT-MAKTX  TO TG_ITENS-MAKTX.

          TG_ITENS-FASE = ICON_DISPLAY_MORE.
          APPEND TG_ITENS.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " GET_ITENS  INPUT
*&---------------------------------------------------------------------*
*&      Form  IMPOSTOS_COMPLEMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM IMPOSTOS_COMPLEMENTO  USING P_ROW.
  DATA: WL_ITENS LIKE LINE OF TG_ITENS.

  SORT TG_IMPO_COMP BY ITMNUM TAXTYP.

  READ TABLE TG_ITENS INTO WL_ITENS INDEX P_ROW.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF W_ZFIWRT0001-COMPLEMENT_ICMS = 'S'.
    LOOP AT TG_IMPO_GERA.
      CLEAR TG_IMPO_COMP.
      MOVE-CORRESPONDING: TG_IMPO_GERA TO TG_IMPO_COMP.
      READ TABLE TG_IMPO_COMP WITH KEY ITMNUM = WL_ITENS-ITMNUM
                                       TAXTYP = TG_IMPO_GERA-TAXTYP. " BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE: WL_ITENS-ITMNUM  TO TG_IMPO_COMP-ITMNUM.
        APPEND TG_IMPO_COMP.
      ELSE.
        MODIFY TG_IMPO_COMP INDEX SY-TABIX.
      ENDIF.
      CLEAR: TG_IMPO_COMP.
    ENDLOOP.
  ELSE.
*-CS2023000043-09.02.2023-#102019-JT-fim
    LOOP AT TG_IMPO.
      CLEAR TG_IMPO_COMP.
      MOVE-CORRESPONDING: TG_IMPO TO TG_IMPO_COMP.
      READ TABLE TG_IMPO_COMP WITH KEY ITMNUM = WL_ITENS-ITMNUM
                                       TAXTYP = TG_IMPO-TAXTYP. " BINARY SEARCH.
      IF SY-SUBRC NE 0.
        MOVE: WL_ITENS-ITMNUM  TO TG_IMPO_COMP-ITMNUM.
        APPEND TG_IMPO_COMP.
      ELSE.
        MODIFY TG_IMPO_COMP INDEX SY-TABIX.
      ENDIF.
      CLEAR: TG_IMPO_COMP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " IMPOSTOS_COMPLEMENTO
*&---------------------------------------------------------------------*
*&      Module  VALIDA_MENSAGEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_MENSAGEM INPUT.
  DATA:  WL_TAB TYPE SY-TABIX.
  DATA: WL_0001 TYPE ZFIWRT0001.
  DESCRIBE TABLE NODE_ITAB LINES WL_TAB.
  " IF WL_TAB LT 2.
  REFRESH: NODE_ITAB.
  IF TREE IS NOT INITIAL.
*    IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
    CALL METHOD TREE->DELETE_ALL_NODES.
*    ENDIF.
  ENDIF.
  REFRESH: TG_MENSAGEMS_AUX, TG_MENSAGEMS.
  IF P_OPERACAO IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZFIWRT0001
      INTO WL_0001
       WHERE OPERACAO EQ P_OPERACAO.
    IF WL_0001-COMPLEMENTO = 'S'.
      CLEAR WG_DOCS-BUDAT.
    ENDIF.
  ENDIF.
  "ENDIF.
ENDMODULE.                 " VALIDA_MENSAGEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FORMA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_FORMA INPUT.
  DATA: TL_RETURN_TAB4 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC4      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_FORMA OCCURS 0,
          ZLSCH TYPE T042Z-ZLSCH,
          TEXT1 TYPE T042Z-TEXT1,
        END OF TL_FORMA.

  SELECT  ZLSCH TEXT1
     FROM  T042Z INTO TABLE TL_FORMA
    WHERE LAND1 = 'BR'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZLSCH'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'BSEG-ZLSCH'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_FORMA
      RETURN_TAB      = TL_RETURN_TAB4
      DYNPFLD_MAPPING = TL_DSELC4.
ENDMODULE.                 " SEARCH_FORMA  INPUT

INCLUDE ZWRR0002_0213.
*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HABILITAR_WORKFLOW_DOCUMENTOS .
  REFRESH: IT_SEQ_LCTO.
  CLEAR: IT_SEQ_LCTO.

  "Somente validar acesso para modificar
  IF P_SEQ_LCTO IS NOT INITIAL.
    IT_SEQ_LCTO-SEQ_LCTO = P_SEQ_LCTO .
    APPEND IT_SEQ_LCTO.
  ENDIF.



ENDFORM.

FORM F_DEFINE_ORIGEM_DESTINO USING P_KNA1  TYPE KNA1
                                   P_LFA1  TYPE LFA1
                                   P_T001W TYPE T001W
                                   P_1BAA  TYPE J_1BAA
                          CHANGING P_INDCOPER
                                   P_TEXTO_FISCAL.

  IF P_PARVW EQ C_AG.
    IF P_KNA1-REGIO EQ P_T001W-REGIO.
      P_INDCOPER = C_D.
      P_TEXTO_FISCAL = 'Dentro do Estado'.
    ELSE.
      P_INDCOPER = C_F.
      P_TEXTO_FISCAL = 'Fora do Estado'.
    ENDIF.
    IF P_1BAA-DIRECT EQ 1.
      MOVE: P_KNA1-REGIO TO WG_SHIPFROM.
    ELSE.
      MOVE: P_KNA1-REGIO TO WG_SHIPTO.
    ENDIF.
  ELSEIF P_PARVW EQ C_BR
     OR  P_PARVW EQ C_LF.
    IF P_LFA1-REGIO EQ P_T001W-REGIO.
      P_INDCOPER = C_D.
      P_TEXTO_FISCAL = 'Dentro do Estado'.
    ELSE.
      P_INDCOPER = C_F.
      P_TEXTO_FISCAL = 'Fora do Estado'.
    ENDIF.

    IF P_1BAA-DIRECT EQ 1.
      MOVE: P_LFA1-REGIO TO WG_SHIPFROM.
    ELSE.
      MOVE: P_LFA1-REGIO TO WG_SHIPTO.
    ENDIF.
  ENDIF.

  IF P_1BAA-DIRECT EQ 1.
    MOVE: P_T001W-REGIO TO WG_SHIPTO.
  ELSE.
    MOVE: P_T001W-REGIO TO WG_SHIPFROM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  INICIA_LANCTO  OUTPUT
*&---------------------------------------------------------------------*
FORM INICIAR_LANCAMENTO_NOTAS.

  DATA: V_BUKRS_MEMO       TYPE T001-BUKRS,
        V_BRANCH_MEMO      TYPE ZIB_NFE_DIST_TER-BRANCH,
        V_NUMERO_MEMO      TYPE ZIB_NFE_DIST_TER-NUMERO,
        V_SERIE_MEMO       TYPE ZIB_NFE_DIST_TER-SERIE,
        V_DT_EMISSAO_MEMO  TYPE ZIB_NFE_DIST_TER-DT_EMISSAO,
        V_LIFNR_MEMO       TYPE LFA1-LIFNR,
        V_OPERACAO_MEMO    TYPE ZFIWRT0001-OPERACAO,
        V_CHAVE_NFE_MEMO   TYPE ZIB_NFE_DIST_TER-CHAVE_NFE,
        V_LCTO_MEMO        TYPE C LENGTH 10,
        V_AGENTE           TYPE ZIB_NFE_DIST_TER-F_TRANSPORTE,
        V_COLETA           TYPE ZIB_NFE_DIST_TER-PC_PARTINER,
        V_ENTREGA          TYPE ZIB_NFE_DIST_TER-LR_PARTINER,
        V_LOC_CARREGA      TYPE ZFIWRT0008-LOC_CARREGA, "CS2020001418
        LVA_PEDIDO_OV      TYPE C LENGTH 10,
        T_ZIB_NFE_DIST_ITM TYPE TABLE OF ZIB_NFE_DIST_ITM WITH HEADER LINE.

*-CS2021000595 - 22.06.2021 - JT - inicio
*----------------------------------------------------------
* busca TVARV NCM x MAterial
*----------------------------------------------------------
  FREE: T_NCM_MAT, T_SET, T_DTLCTO.
  "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA

*  if sy-uname is not initial .
*    "Modelo anterior
**
*    select *
*    from tvarvc
*    into table t_tvarv
*   where name = 'ZWRR0002_NCM_MAT'.
*
*    loop at t_tvarv into w_tvarv.
*      clear w_ncm_mat.
*
*      call function 'CONVERSION_EXIT_MATN1_INPUT'
*        exporting
*          input        = w_tvarv-high
*        importing
*          output       = w_tvarv-high
*        exceptions
*          length_error = 1
*          others       = 2.
*
*      w_ncm_mat-steuc    = w_tvarv-low.
*      w_ncm_mat-matnr    = w_tvarv-high.
*      append w_ncm_mat  to t_ncm_mat.
*    endloop.
*
*  else.
*  Modelo Novo


  CLEAR: T_ZFIWRT0032.
  SELECT DISTINCT * FROM ZFIWRT0032
    WHERE LTRIM( OPERACAO,'0' ) = LTRIM( @P_OPERACAO,'0' )
    INTO TABLE @T_ZFIWRT0032.

  LOOP AT T_ZFIWRT0032 ASSIGNING FIELD-SYMBOL(<FS_ZFIWRT0032>).
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = <FS_ZFIWRT0032>-MATERIAL
      IMPORTING
        OUTPUT       = <FS_ZFIWRT0032>-MATERIAL
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    W_NCM_MAT-STEUC    = <FS_ZFIWRT0032>-NCM.
    W_NCM_MAT-MATNR    = <FS_ZFIWRT0032>-MATERIAL.
    APPEND W_NCM_MAT  TO T_NCM_MAT.
    CLEAR:W_NCM_MAT.
  ENDLOOP.
*  endif.


  "************************************************************
  "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA
*  select *
*    from tvarvc
*    into table t_tvarv
*   where name = 'ZWRR0002_NCM_MAT'.
*
*  loop at t_tvarv into w_tvarv.
*    clear w_ncm_mat.
*
*    call function 'CONVERSION_EXIT_MATN1_INPUT'
*      exporting
*        input        = w_tvarv-high
*      importing
*        output       = w_tvarv-high
*      exceptions
*        length_error = 1
*        others       = 2.
*
*    w_ncm_mat-steuc    = w_tvarv-low.
*    w_ncm_mat-matnr    = w_tvarv-high.
*    append w_ncm_mat  to t_ncm_mat.
*  endloop.
*  ************************************************************
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'ZNFW0009_DTLCTO_AUT'
      NO_DESCRIPTIONS = ABAP_FALSE
    TABLES
      SET_VALUES      = T_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  LOOP AT T_SET   INTO W_SET.
    L_DATA = W_SET-TITLE+6(4) &&  W_SET-TITLE+3(2) &&  W_SET-TITLE(2).
    W_DTLCTO-UNAME   = W_SET-FROM.
    W_DTLCTO-BUDAT   = L_DATA.
    APPEND W_DTLCTO TO T_DTLCTO.
  ENDLOOP.

  SELECT SINGLE * FROM ZFIWRT2005 INTO @DATA(WS_ZFIWRT2005)
  WHERE USUARIO EQ @SY-UNAME
  AND OPERACAO EQ @V_OPERACAO_MEMO.
  IF SY-SUBRC EQ 0.
    W_DTLCTO-UNAME   = WS_ZFIWRT2005-USUARIO.
    W_DTLCTO-BUDAT   = WS_ZFIWRT2005-DT_LANC.
    APPEND W_DTLCTO TO T_DTLCTO.
  ENDIF.
  CLEAR: WS_ZFIWRT2005.

  SORT T_DTLCTO BY UNAME.
*-CS2021000595 - 22.06.2021 - JT - inicio

*-CS2021001266 - 15.12.2021 - JT- inicio
  FREE: T_SET,
        T_REGULA.

  "CS2022000878 / AOENNING.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'SET_MAGGI_ZNFW_ICMS'
      NO_DESCRIPTIONS = ABAP_FALSE
    TABLES
      SET_VALUES      = T_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  LOOP AT T_SET    INTO W_SET.
    W_REGULA-OPERACAO = W_SET-FROM.
    W_REGULA-RATE     = W_SET-TITLE.
    W_REGULA-SHIPTO   = 'MT'. "w_set-title.
    APPEND W_REGULA  TO T_REGULA.
  ENDLOOP.

  SORT T_REGULA BY OPERACAO.
  DELETE ADJACENT DUPLICATES FROM T_REGULA
                        COMPARING OPERACAO.
*-CS2021001266 - 15.12.2021 - JT- fim

  IF SY-CALLD EQ 'X' AND VINICIOU_LCTO_ZNFW0009 NE 'X'.

    VINICIOU_LCTO_ZNFW0009 = 'X'.

    IMPORT V_BUKRS_MEMO      FROM MEMORY ID 'EMPRESA'.
    IMPORT V_BRANCH_MEMO     FROM MEMORY ID 'FILIAL'.
    IMPORT V_NUMERO_MEMO     FROM MEMORY ID 'NFE'.
    IMPORT V_SERIE_MEMO      FROM MEMORY ID 'SERIE'.
    IMPORT V_DT_EMISSAO_MEMO FROM MEMORY ID 'DT_EMISSAO'.
    IMPORT V_LIFNR_MEMO      FROM MEMORY ID 'IDPARCEIRO'.
    IMPORT V_OPERACAO_MEMO   FROM MEMORY ID 'OPERACAO'.
    IMPORT V_CHAVE_NFE_MEMO  FROM MEMORY ID 'CHAVE'.
    IMPORT V_LCTO_MEMO       FROM MEMORY ID 'LANCAMENTO'.
    IMPORT V_AGENTE          FROM MEMORY ID 'AGENTE'.
    IMPORT V_COLETA          FROM MEMORY ID 'COLETA'.
    IMPORT V_ENTREGA         FROM MEMORY ID 'ENTREGA'.
    IMPORT V_LOC_CARREGA     FROM MEMORY ID 'LOC_CARREGA'. "CS2020001418 - Inicio - CSB

    DELETE FROM MEMORY ID 'EMPRESA'.
    DELETE FROM MEMORY ID 'FILIAL'.
    DELETE FROM MEMORY ID 'NFE'.
    DELETE FROM MEMORY ID 'SERIE'.
    DELETE FROM MEMORY ID 'DT_EMISSAO'.
    DELETE FROM MEMORY ID 'IDPARCEIRO'.
    DELETE FROM MEMORY ID 'OPERACAO'.
    DELETE FROM MEMORY ID 'CHAVE'.
    DELETE FROM MEMORY ID 'LANCAMENTO'.
    DELETE FROM MEMORY ID 'AGENTE'.
    DELETE FROM MEMORY ID 'COLETA'.
    DELETE FROM MEMORY ID 'ENTREGA'.
    DELETE FROM MEMORY ID 'LOC_CARREGA'.


    VLANCAMENTO_ZNFW0009 = V_LCTO_MEMO.

    IF  V_BUKRS_MEMO      IS NOT INITIAL   AND  V_BRANCH_MEMO        IS NOT INITIAL  AND
        V_NUMERO_MEMO     IS NOT INITIAL   AND  V_SERIE_MEMO         IS NOT INITIAL  AND
        V_DT_EMISSAO_MEMO IS NOT INITIAL   AND  V_OPERACAO_MEMO      IS NOT INITIAL  AND
        V_CHAVE_NFE_MEMO  IS NOT INITIAL   AND  VLANCAMENTO_ZNFW0009 IS NOT INITIAL.

      PERFORM Z_NOVO_LAN.

      P_BUKRS              = V_BUKRS_MEMO.
      P_BRANCH             = V_BRANCH_MEMO.
      P_PARID              = V_LIFNR_MEMO.
      P_OPERACAO           = V_OPERACAO_MEMO.
      WG_DOCS-NFENUM       = V_NUMERO_MEMO.
      WG_DOCS-SERIES       = V_SERIE_MEMO.
      WG_DOCS-BUDAT        = SY-DATUM.
      WG_DOCS-LOC_CARREGA  = V_LOC_CARREGA.

      "CS2022000878 / AOENNING.
      CLEAR: WS_ZFIWRT2005.
      SELECT SINGLE * FROM ZFIWRT2005 INTO WS_ZFIWRT2005
      WHERE USUARIO EQ SY-UNAME
      AND OPERACAO EQ V_OPERACAO_MEMO.
      IF SY-SUBRC EQ 0.
        WG_DOCS-BUDAT = WS_ZFIWRT2005-DT_LANC .
      ELSE.
        WG_DOCS-BUDAT = SY-DATUM.
      ENDIF.
      CLEAR: WS_ZFIWRT2005.

      IF VLANCAMENTO_ZNFW0009 = 'MIC'.
        WG_DOCS-BUDAT     = V_DT_EMISSAO_MEMO.
      ELSE.
*-CS2021000595 - 22.06.2021 - JT - inicio
        READ TABLE T_DTLCTO INTO W_DTLCTO WITH KEY UNAME = SY-UNAME.
        IF SY-SUBRC = 0.
          WG_DOCS-BUDAT = W_DTLCTO-BUDAT.
        ENDIF.
      ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

      WG_FISCAL-INCO1      = 'CIF'.
      WG_FISCAL-INCO2      = 'CIF'.
      WG_DOCS-BLDAT        = V_DT_EMISSAO_MEMO.
      WG_FISCAL-ACCESS_KEY = V_CHAVE_NFE_MEMO.

      CLEAR: T_ZIB_NFE_DIST_ITM[].
      SELECT *
        FROM ZIB_NFE_DIST_ITM INTO TABLE T_ZIB_NFE_DIST_ITM
       WHERE CHAVE_NFE        EQ V_CHAVE_NFE_MEMO
         AND PROD_PEDIDO_COMP NE SPACE.

      SORT T_ZIB_NFE_DIST_ITM BY PROD_PEDIDO_COMP.
      DELETE ADJACENT DUPLICATES FROM T_ZIB_NFE_DIST_ITM COMPARING PROD_PEDIDO_COMP.

      LOOP AT T_ZIB_NFE_DIST_ITM.
        LVA_PEDIDO_OV = T_ZIB_NFE_DIST_ITM-PROD_PEDIDO_COMP.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LVA_PEDIDO_OV
          IMPORTING
            OUTPUT = LVA_PEDIDO_OV.

        SELECT SINGLE *
          FROM EKKO INTO @DATA(LWA_EKKO)
         WHERE EBELN EQ @LVA_PEDIDO_OV.

        IF SY-SUBRC EQ 0.
          WG_FISCAL-EBELN = LVA_PEDIDO_OV.
        ELSE.
          SELECT SINGLE *
            FROM VBAK INTO @DATA(LWA_VBAK)
           WHERE VBELN EQ @LVA_PEDIDO_OV.

          IF SY-SUBRC EQ 0.
            WG_FISCAL-VBELN = LVA_PEDIDO_OV.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF V_AGENTE IS NOT INITIAL.
        TG_PARC-PARVW = 'SP'.
        TG_PARC-PARID = V_AGENTE.
        TG_PARC-NOME = CAST ZCL_FORNECEDORES( ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE( )->SET_PARCEIRO( I_PARCEIRO = V_AGENTE ) )->AT_LFA1-NAME1.
        WA_STYLE-FIELDNAME = 'PARVW'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        WA_STYLE-FIELDNAME = 'PARID'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.
        APPEND TG_PARC.
        REFRESH: STYLE, TG_PARC-STYLE.
      ENDIF.

      IF V_COLETA  IS NOT INITIAL.
        TG_PARC-PARVW = 'PC'.
        TG_PARC-PARID = V_COLETA.
        TG_PARC-NOME = CAST ZCL_FORNECEDORES( ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE( )->SET_PARCEIRO( I_PARCEIRO = V_COLETA ) )->AT_LFA1-NAME1.
        WA_STYLE-FIELDNAME = 'PARVW'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        WA_STYLE-FIELDNAME = 'PARID'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.
        APPEND TG_PARC.
        REFRESH: STYLE, TG_PARC-STYLE.
      ENDIF.

      IF V_ENTREGA IS NOT INITIAL.
        TG_PARC-PARVW = 'LR'.
        TG_PARC-PARID = V_ENTREGA.
        TG_PARC-NOME  = CAST ZCL_CLIENTES( ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE( )->SET_PARCEIRO( I_PARCEIRO = V_ENTREGA ) )->AT_KNA1-NAME1.
        WA_STYLE-FIELDNAME = 'PARVW'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        WA_STYLE-FIELDNAME = 'PARID'.
        WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT WA_STYLE INTO TABLE STYLE .
        INSERT LINES OF STYLE INTO TABLE TG_PARC-STYLE.
        APPEND TG_PARC.
        REFRESH: STYLE, TG_PARC-STYLE.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  INICIA_LANCTO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIA_LANCTO OUTPUT.

*-CS2021000595 - 22.06.2021 - JT - inicio
  PERFORM INICIAR_LANCAMENTO_NOTAS.
*-CS2021000595 - 22.06.2021 - JT - fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  Z_NOVO_LAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_NOVO_LAN .

  IF WG_FLAG IS INITIAL.

    WG_SUGERE_TRANSP = ABAP_TRUE.

    PERFORM LIMPA_CAMPOS.
    CLEAR: P_OPERACAO, P_SEQ_LCTO, P_PARVW, P_PARID, P_BRANCH, P_BUKRS, WG_DESC_PARID,
           WG_DESC_OPERACAO, WG_DESC_PARVW, WG_DESC_BRANCH, WG_DESC_BUKRS, P_LOEKZ.
    PERFORM GET_NEXT_NUMBER IN PROGRAM ZWRR0001 USING  'ZSEQ_LCTO'
                                                    '1'
                                           CHANGING P_SEQ_LCTO.

  ENDIF.
  PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                                 C_1       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
  PERFORM TRATA_CAMPOS USING SPACE
                            'GR2'
                             C_0       "INPUT 1     NO INPUT 0
                             C_0.      "INVISIBLE 1 VISIBLE 0
*      wg_docs-bldat = sy-datum.
  WG_DOCS-BUDAT = SY-DATUM.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_SUGERE_ITENS.

  DATA: LWA_ITEM        LIKE LINE OF TG_ITENS,
        LVA_NCM         TYPE MARC-STEUC,
        LVA_FATOR_CONV  TYPE I,
        LVA_COUNT_ITENS TYPE I,
        LVA_MATNR_XML   TYPE MARA-MATNR,
        LVA_MATNR_18    TYPE MATNR18,
        LIT_MATNR_OV_PD TYPE TABLE OF TY_MATNR_OV_PD,
        VG_SEM(1).

  IF TG_ITENS[] IS INITIAL.
    CLEAR: VSUGERE_ITENS_ZNFW0009.
  ENDIF.
  CLEAR VG_SEM.

  "133290 CS2024000037 Liberar tabela de vinculação de NCM vs produto SAP - znfw0009 Lib. tab. vinc. de NCM vs prod. SAP  PBALVES
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PERFORM INICIAR_LANCAMENTO_NOTAS.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CHECK ( VINICIOU_LCTO_ZNFW0009 = 'X'  AND VSUGERE_ITENS_ZNFW0009 IS INITIAL ) AND ( VLANCAMENTO_ZNFW0009 = 'MIC' OR VLANCAMENTO_ZNFW0009 = 'ONF' ).

  VSUGERE_ITENS_ZNFW0009 = 'X'.

  CLEAR: VLR_TOTAL_NOTA, LVA_COUNT_ITENS, LIT_MATNR_OV_PD[], LIT_ZIB_NFE_DIST_ITM[].

  PERFORM F_GET_MATNR_OV_PD TABLES LIT_MATNR_OV_PD.

  SELECT  *
    FROM ZIB_NFE_DIST_ITM INTO TABLE LIT_ZIB_NFE_DIST_ITM
   WHERE CHAVE_NFE EQ WG_FISCAL-ACCESS_KEY.

  LOOP AT LIT_ZIB_NFE_DIST_ITM INTO DATA(LWA_ZIB_NFE_DIST_ITM).

    CLEAR: LWA_ITEM.

    LVA_FATOR_CONV = 1.

    LVA_MATNR_XML = LWA_ZIB_NFE_DIST_ITM-PROD_CODIGO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LVA_MATNR_XML
      IMPORTING
        OUTPUT = LVA_MATNR_18.
    LVA_MATNR_XML = LVA_MATNR_18.

    PERFORM F_FORMAT_NCM USING LWA_ZIB_NFE_DIST_ITM-PROD_NCM CHANGING LVA_NCM.

    LWA_ITEM-ITMNUM = ( LVA_COUNT_ITENS + 1 ) * 10.

    "Identificação Material Pedido/Ordem Venda
    CASE VLANCAMENTO_ZNFW0009.
      WHEN 'MIC'.
        READ TABLE LIT_MATNR_OV_PD INTO DATA(LWA_MATNR_OV_PD) WITH KEY MATNR = LVA_MATNR_XML "Notas da MIC, os codigos do materiais do pedido/ordem venda são iguais ao do XML
                                                                       STEUC = LVA_NCM.

        IF SY-SUBRC NE 0. "Só adicionar o item se encontrar a correspondencia exata(ncm e matnr) no pedido/ov.
          PERFORM F_CHECK_REGRAS_ZNFW0009. "Ajustes feito para bypass
          IF SY-SUBRC EQ 0.
            CONTINUE.
          ENDIF.
        ENDIF.
      WHEN 'ONF'.

        DATA(LVA_COUNT_NCM_PD_OV) = 0.
        DATA(LVA_COUNT_NCM_PD) = 0.
        LOOP AT LIT_MATNR_OV_PD INTO LWA_MATNR_OV_PD WHERE STEUC = LVA_NCM.
          ADD 1 TO LVA_COUNT_NCM_PD_OV.
        ENDLOOP.
        LOOP AT LIT_MATNR_OV_PD INTO LWA_MATNR_OV_PD.
          ADD 1 TO LVA_COUNT_NCM_PD.
        ENDLOOP.

        IF LVA_COUNT_NCM_PD_OV EQ 1. "Se encontrou somente um item no pedido/ov com o ncm selecionado, utilizar o item em questão..
          SY-SUBRC = 0.
        ELSE.

          READ TABLE T_NCM_MAT INTO W_NCM_MAT WITH KEY STEUC = LVA_NCM.

          IF SY-SUBRC = 0.
*---------- Descricao material
            SELECT MAKTX
              INTO @DATA(L_MAKTX)
              FROM MAKT
                UP TO 1 ROWS
             WHERE MATNR = @W_NCM_MAT-MATNR
               AND SPRAS = @SY-LANGU.
            ENDSELECT.

            "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA
            LOOP AT T_ZFIWRT0032 ASSIGNING FIELD-SYMBOL(<GET_ZFIWRT0032>) WHERE MATERIAL = W_NCM_MAT-MATNR AND NCM = W_NCM_MAT-STEUC AND OPERACAO = P_OPERACAO.

              IF <GET_ZFIWRT0032>-LOTE_AUT IS NOT INITIAL.
                LWA_ITEM-CHARG = SY-DATUM+0(4).
              ELSE.
                LWA_ITEM-CHARG = <GET_ZFIWRT0032>-LOTE.
              ENDIF.

              LWA_ITEM-MATNR = <GET_ZFIWRT0032>-MATERIAL.
              LWA_ITEM-MEINS = 'KG'.
              LWA_ITEM-STEUC = <GET_ZFIWRT0032>-NCM.
              LWA_ITEM-MAKTX = L_MAKTX.
              LWA_ITEM-WERKS = P_BRANCH.
              LWA_ITEM-LGORT = <GET_ZFIWRT0032>-DEPOSITO.
            ENDLOOP.



            "Bloquear Alterações Campos
*            APPEND VALUE #( fieldname = 'MATNR' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
*            APPEND VALUE #( fieldname = 'WERKS' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.

            SY-SUBRC = 2.
            IF LVA_COUNT_NCM_PD = 0 AND WG_FISCAL-EBELN IS NOT INITIAL.
              VG_SEM = 'X'.
            ENDIF.
          ELSE.
            SY-SUBRC = 4.
          ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
        ENDIF.

      WHEN OTHERS.
        SY-SUBRC = 4.
    ENDCASE.

    IF SY-SUBRC = 0.
      LWA_ITEM-MATNR = LWA_MATNR_OV_PD-MATNR.
      LWA_ITEM-MEINS = LWA_MATNR_OV_PD-MEINS.
      LWA_ITEM-STEUC = LWA_MATNR_OV_PD-STEUC.
      LWA_ITEM-MAKTX = LWA_MATNR_OV_PD-MAKTX.
      LWA_ITEM-WERKS = LWA_MATNR_OV_PD-WERKS.
      LWA_ITEM-LGORT = LWA_MATNR_OV_PD-LGORT.
      LWA_ITEM-CHARG = LWA_MATNR_OV_PD-CHARG.

      LWA_ITEM-VBELN = LWA_MATNR_OV_PD-VBELN.
      LWA_ITEM-POSNR = LWA_MATNR_OV_PD-POSNR.

*      "Bloquear Alterações Campos
*      APPEND VALUE #( fieldname = 'MATNR' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
*      APPEND VALUE #( fieldname = 'WERKS' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
    ENDIF.

    IF ( LWA_ZIB_NFE_DIST_ITM-PROD_UND_COMERCI = 'TO'  ) OR
       ( LWA_ZIB_NFE_DIST_ITM-PROD_UND_COMERCI = 'TON' ) OR
       ( LWA_ZIB_NFE_DIST_ITM-PROD_UND_COMERCI = 'TL'  ) OR
       ( LWA_ZIB_NFE_DIST_ITM-PROD_UND_COMERCI = 'TN'  ).
*
*      SELECT SINGLE *
*        FROM marc INTO @DATA(lwa_marc)
*       WHERE steuc EQ @lva_ncm
*         AND werks EQ @p_branch.


      SELECT SINGLE *
        FROM MARC INTO @DATA(LWA_MARC)
       WHERE MATNR EQ @LWA_ITEM-MATNR
         AND WERKS EQ @P_BRANCH.

      IF SY-SUBRC EQ 0.
        SELECT SINGLE *
          FROM MARA INTO @DATA(LWA_MARA)
         WHERE MATNR EQ @LWA_MARC-MATNR.

        IF SY-SUBRC EQ 0 AND ( LWA_MARA-MEINS EQ 'KG' ).
          LVA_FATOR_CONV = 1000.
        ENDIF.
      ENDIF.
    ENDIF.

    LWA_ITEM-CFOP           = WG_DIREITOS-CFOP.
    LWA_ITEM-MENGE          = LWA_ZIB_NFE_DIST_ITM-PROD_QTD_COMERCI * LVA_FATOR_CONV.
    LWA_ITEM-NETPR          = LWA_ZIB_NFE_DIST_ITM-PROD_VLR_UND_COM / LVA_FATOR_CONV.
*    lwa_item-netwr          = lwa_zib_nfe_dist_itm-prod_vlr_total_b.

*&------------------------------------------------------------------ajuste bug / aoenning
    LWA_ITEM-NETDIS         = LWA_ZIB_NFE_DIST_ITM-PROD_VL_DESCONTO.
    LWA_ITEM-NETOTH         = LWA_ZIB_NFE_DIST_ITM-PROD_VL_OUTRO.
    LWA_ITEM-NETFRE         = LWA_ZIB_NFE_DIST_ITM-PROD_VL_FRETE.
    LWA_ITEM-NETINS         = LWA_ZIB_NFE_DIST_ITM-PROD_VL_SEGURO.
*&------------------------------------------------------------------Ajuste BUG / AOENNING

    LWA_ITEM-NETWR          = LWA_ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B - LWA_ITEM-NETDIS + LWA_ITEM-NETFRE + LWA_ITEM-NETINS + LWA_ITEM-NETOTH.
    LWA_ITEM-VLR_ITEN_XML   = LWA_ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B.
    LWA_ITEM-NCM_XML        = LWA_ZIB_NFE_DIST_ITM-PROD_NCM.
    LWA_ITEM-VLR_ICMS_XML   = LWA_ZIB_NFE_DIST_ITM-ICMS_VALOR.

    IF LWA_ITEM-MATNR IS NOT INITIAL AND VG_SEM IS INITIAL. "ALRS
      APPEND LWA_ITEM TO TG_ITENS.

      ADD 1 TO LVA_COUNT_ITENS.

*      ADD lwa_zib_nfe_dist_itm-prod_vlr_total_b TO vlr_total_nota.
*&------------------------------------------------------------------ajuste bug / aoenning
      ADD LWA_ITEM-NETWR TO VLR_TOTAL_NOTA.
*&------------------------------------------------------------------Ajuste BUG / AOENNING
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM F_FORMAT_NCM USING P_NCM TYPE ZIB_NFE_DIST_ITM-PROD_NCM
               CHANGING C_NCM TYPE MARC-STEUC.

  CLEAR: C_NCM.

  CHECK P_NCM IS NOT INITIAL.

  C_NCM = P_NCM.
  REPLACE ALL OCCURRENCES OF '.' IN C_NCM WITH ' '.
  CONDENSE C_NCM NO-GAPS.
  CONCATENATE C_NCM+0(4) '.' C_NCM+4(2) '.' C_NCM+6(2) INTO C_NCM.


ENDFORM.

FORM F_GET_MATNR_OV_PD TABLES T_MATNR_OV_PD STRUCTURE GIT_MATNR_OV_PD.

  DATA: LIT_MARC  TYPE TABLE OF MARC,
        LVA_CHARG TYPE EKET-CHARG.

  CLEAR: T_MATNR_OV_PD[].

  IF WG_FISCAL-EBELN IS NOT INITIAL.

    SELECT *
      FROM EKPO INTO TABLE @DATA(IT_EKPO_TMP)
     WHERE EBELN EQ @WG_FISCAL-EBELN
       AND LOEKZ EQ @ABAP_FALSE.

    LOOP AT IT_EKPO_TMP INTO DATA(LWA_EKPO_TMP).

      CLEAR: LVA_CHARG.

      SELECT SINGLE  *
        FROM EKET INTO @DATA(LWA_EKET)
       WHERE EBELN EQ @LWA_EKPO_TMP-EBELN.

      IF SY-SUBRC EQ 0.
        LVA_CHARG = LWA_EKET-CHARG.
      ENDIF.

      APPEND VALUE #( EBELN = LWA_EKPO_TMP-EBELN
                      MATNR = LWA_EKPO_TMP-MATNR
                      MEINS = LWA_EKPO_TMP-MEINS
                      WERKS = LWA_EKPO_TMP-WERKS
                      LGORT = LWA_EKPO_TMP-LGORT
                      STEUC = LWA_EKPO_TMP-J_1BNBM
                      CHARG = LVA_CHARG ) TO T_MATNR_OV_PD[].
    ENDLOOP.

  ELSEIF WG_FISCAL-VBELN IS NOT INITIAL.

    SELECT *
      FROM VBAP INTO TABLE @DATA(IT_VBAP_TMP)
     WHERE VBELN EQ @WG_FISCAL-VBELN.

    LOOP AT IT_VBAP_TMP INTO DATA(LWA_VBAP_TMP).
      APPEND VALUE #( VBELN = LWA_VBAP_TMP-VBELN
                      POSNR = LWA_VBAP_TMP-POSNR
                      MATNR = LWA_VBAP_TMP-MATNR
                      MEINS = LWA_VBAP_TMP-MEINS
                      WERKS = LWA_VBAP_TMP-WERKS
                      LGORT = LWA_VBAP_TMP-LGORT
                      CHARG = LWA_VBAP_TMP-CHARG ) TO T_MATNR_OV_PD[].
    ENDLOOP.

  ENDIF.

  CHECK T_MATNR_OV_PD[] IS NOT INITIAL.

  LOOP AT T_MATNR_OV_PD ASSIGNING FIELD-SYMBOL(<FS_MATNR_OV_PD>).

    SELECT SINGLE *
      FROM MAKT INTO @DATA(WL_MAKT)
     WHERE MATNR EQ @<FS_MATNR_OV_PD>-MATNR
       AND SPRAS EQ @SY-LANGU.

    IF SY-SUBRC EQ 0.
      <FS_MATNR_OV_PD>-MAKTX = WL_MAKT-MAKTX.
    ENDIF.

    SELECT SINGLE *
      FROM MARC INTO @DATA(LWA_MARC)
     WHERE MATNR EQ @<FS_MATNR_OV_PD>-MATNR
       AND WERKS EQ @P_BRANCH.

    IF SY-SUBRC EQ 0 .
      IF <FS_MATNR_OV_PD>-STEUC IS INITIAL.
        <FS_MATNR_OV_PD>-STEUC = LWA_MARC-STEUC.
      ENDIF.
    ENDIF.

  ENDLOOP.

  DELETE T_MATNR_OV_PD WHERE NOT ( MAKTX IS NOT INITIAL AND STEUC IS NOT INITIAL ).

ENDFORM.

*FORM z_sugere_itens.
*
*  DATA: tl_itens_aux LIKE TABLE OF tg_itens,
*        wl_itens     LIKE LINE OF tg_itens,
*        wl_lines     TYPE sy-tabix,
*        v_ncm        TYPE marc-steuc,
*        l_lines.
*
*  IF tg_itens[] IS INITIAL.
*    CLEAR: vsugere_itens_znfw0009.
*  ENDIF.
*
*  IF ( viniciou_lcto_znfw0009 = 'X' AND vsugere_itens_znfw0009 IS INITIAL ) AND
*     ( vlancamento_znfw0009 = 'MIC'  AND wg_fiscal-ebeln IS NOT INITIAL OR
*       vlancamento_znfw0009 = 'ONF'  AND wg_fiscal-ebeln IS NOT INITIAL ).
*
*    CLEAR: vlr_total_nota.
*
*    vsugere_itens_znfw0009 = 'X'.
*
*    SELECT *
*       FROM ekpo INTO TABLE @DATA(it_ekpo)
*      WHERE ebeln EQ @wg_fiscal-ebeln.
*
*    SELECT  *
*      FROM zib_nfe_dist_itm INTO TABLE @DATA(it_zib_nfe_dist_itm)
*     WHERE chave_nfe EQ @wg_fiscal-access_key.
*
*    REFRESH: tl_itens_aux.
*
*    DESCRIBE TABLE it_ekpo LINES l_lines.
*
*    IF l_lines > 1.
*
*      LOOP AT  it_zib_nfe_dist_itm INTO DATA(wa_zib_nfe_dist_itm).
*        REFRESH: style2, wl_itens-style2.
*        tl_itens_aux[] = tg_itens[].
*        REFRESH: tg_itens.
*        LOOP AT tl_itens_aux INTO wl_itens.
*          wl_itens-itmnum = sy-tabix * 10.
*          APPEND wl_itens TO tg_itens.
*        ENDLOOP.
*        DESCRIBE TABLE tg_itens LINES wl_lines.
*        CLEAR: wl_itens.
*        wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
*        "
*        v_ncm = wa_zib_nfe_dist_itm-prod_ncm.
*        REPLACE ALL OCCURRENCES OF '.' IN v_ncm WITH ' '.
*        CONDENSE v_ncm NO-GAPS.
*        CONCATENATE v_ncm+0(4) '.' v_ncm+4(2) '.' v_ncm+6(2) INTO v_ncm.
*        SELECT SINGLE matnr
*          FROM marc
*          INTO @DATA(_matnr)
*          WHERE steuc = @v_ncm.
*        "
*        IF sy-subrc = 0.
*          READ TABLE  it_ekpo INTO DATA(wa_ekpo_) WITH KEY matnr = _matnr.
*          IF sy-subrc = 0.
*            wl_itens-matnr = wa_ekpo_-matnr.
*            wl_itens-meins = wa_ekpo_-meins.
*            SELECT SINGLE steuc
*            FROM marc
*            INTO wl_itens-steuc
*             WHERE matnr EQ wl_itens-matnr
*               AND werks EQ p_branch.
*
*            SELECT SINGLE *
*              FROM makt INTO @DATA(wl_makt)
*             WHERE spras EQ @sy-langu
*               AND matnr EQ @wa_ekpo_-matnr.
*
*            IF sy-subrc = 0.
*              wl_itens-maktx = wl_makt-maktx.
*            ENDIF.
*
*            wl_itens-werks = wa_ekpo_-werks.
*            wl_itens-lgort = wa_ekpo_-lgort.
*
*            SELECT SINGLE  *
*              FROM eket INTO @DATA(wa_eket_)
*             WHERE ebeln EQ @wa_ekpo_-ebeln.
*
*            IF sy-subrc EQ 0.
*              wl_itens-charg = wa_eket_-charg.
*            ENDIF.
*            wa_style-fieldname = 'WERKS'.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*            INSERT  wa_style INTO TABLE style2.
*            "
*            wa_style-fieldname = 'MATNR'.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*            INSERT  wa_style INTO TABLE style2.
*          ENDIF.
*        ENDIF.
*        "
*
*        wl_itens-cfop = wg_direitos-cfop.
*        "
*        DATA(_fator) = 1000.
*        _fator = 1.
**        IF V_LCTO_MEMO = 'MIC'.
*        IF wa_zib_nfe_dist_itm-prod_und_comerci = 'TO' AND wa_ekpo_-meins = 'KG'.
*          _fator = 1000.
**          ENDIF.
*        ENDIF.
*        vlr_total_nota          = vlr_total_nota +  wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-menge          = wa_zib_nfe_dist_itm-prod_qtd_comerci * _fator.
*        wl_itens-netpr          = wa_zib_nfe_dist_itm-prod_vlr_und_com / _fator.
*        wl_itens-netwr          = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-vlr_iten_xml   = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*
*        INSERT LINES OF style2 INTO TABLE wl_itens-style2.
*        APPEND wl_itens TO tg_itens[].
*        CLEAR wl_itens.
*      ENDLOOP.
*
*    ELSEIF l_lines = 1.
*
*      LOOP AT  it_zib_nfe_dist_itm INTO wa_zib_nfe_dist_itm.
*        REFRESH: style2, wl_itens-style2.
*        READ TABLE  it_ekpo INTO DATA(wa_ekpo) INDEX 1.
*
*        tl_itens_aux[] = tg_itens[].
*        REFRESH: tg_itens.
*        LOOP AT tl_itens_aux INTO wl_itens.
*          wl_itens-itmnum = sy-tabix * 10.
*          APPEND wl_itens TO tg_itens.
*        ENDLOOP.
*        DESCRIBE TABLE tg_itens LINES wl_lines.
*        CLEAR: wl_itens.
*        wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
*
*        wl_itens-matnr = wa_ekpo-matnr.
*        wl_itens-meins = wa_ekpo-meins.
*        SELECT SINGLE steuc
*          FROM marc
*          INTO wl_itens-steuc
*           WHERE matnr EQ wl_itens-matnr
*             AND werks EQ p_branch.
*
*        SELECT SINGLE *
*          FROM makt INTO @DATA(wl_makt_)
*         WHERE spras EQ @sy-langu
*           AND matnr EQ @wa_ekpo-matnr.
*
*        IF sy-subrc = 0.
*          wl_itens-maktx = wl_makt_-maktx.
*        ENDIF.
*
*        wl_itens-werks = wa_ekpo-werks.
*        wl_itens-lgort = wa_ekpo-lgort.
*
*        SELECT SINGLE  *
*          FROM eket INTO @DATA(wa_eket)
*         WHERE ebeln EQ @wa_ekpo-ebeln.
*
*        IF sy-subrc EQ 0.
*          wl_itens-charg = wa_eket-charg.
*        ENDIF.
*
*        wa_style-fieldname = 'WERKS'.
*        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT  wa_style INTO TABLE style2.
*        "
*        wa_style-fieldname = 'MATNR'.
*        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT  wa_style INTO TABLE style2.
*        "
*        wl_itens-cfop = wg_direitos-cfop.
*        "
*        DATA(_fator2) = 1000.
*        _fator2 = 1.
**        IF V_LCTO_MEMO = 'MIC'.
*        IF wa_zib_nfe_dist_itm-prod_und_comerci = 'TO' AND wa_ekpo-meins = 'KG'.
*          _fator2 = 1000.
*        ENDIF.
**        ENDIF.
*        wl_itens-menge          = wa_zib_nfe_dist_itm-prod_qtd_comerci * _fator2.
*        wl_itens-netpr          = wa_zib_nfe_dist_itm-prod_vlr_und_com / _fator2.
*        wl_itens-netwr          = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-vlr_iten_xml   = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        vlr_total_nota          = vlr_total_nota +  wa_zib_nfe_dist_itm-prod_vlr_total_b.
*
*        INSERT LINES OF style2 INTO TABLE wl_itens-style2.
*        APPEND wl_itens TO tg_itens[].
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ENTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE ENTER INPUT.
**      refresh: tg_parc.
*  PERFORM BUSCA_DADOS.
**      PERFORM busca_dados_doc.
*  PERFORM BUSCA_DESCRICOES.
*ENDMODULE.

FORM F_CHECK_REGRAS_ZNFW0009.

  CONCATENATE P_BUKRS P_OPERACAO INTO DATA(LVA_BUKRS_OPR_NOT_CHECK).

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(LWA_SETLEAF)
   WHERE SETNAME EQ 'ZNFW0002_REGRAS_ZNFW0009'
     AND VALFROM EQ @LVA_BUKRS_OPR_NOT_CHECK.

  IF SY-SUBRC EQ 0. "Se parametrizado
    SY-SUBRC = 4. "Não deve checar regras da ZNFW0009
  ELSE.
    SY-SUBRC = 0. "Deve checar regras da ZNFW0009
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_SD
*&---------------------------------------------------------------------*
FORM F_TEXTO_SD  USING  P_TEXTO_SD.

  DATA: LVA_NAME TYPE THEAD-TDNAME.

  DATA: LT_LINES_AUX TYPE TABLE OF TLINE,
        LT_LINES     TYPE TABLE OF TLINE.

  DATA: LWA_LINES TYPE TLINE.

  IF P_TEXTO_SD IS NOT INITIAL.
    IF P_BUKRS IS NOT INITIAL.
      SELECT SINGLE VKORG
        FROM TVKO
        INTO @DATA(LVA_VKORG)
        WHERE BUKRS = @P_BUKRS.

      LOOP AT TG_ITENS INTO DATA(LWA_ITENS).
        PERFORM F_INPUT_MATNR CHANGING LWA_ITENS-MATNR.

        CONCATENATE LWA_ITENS-MATNR
                    LVA_VKORG
                    '10'
                    INTO LVA_NAME.

        REFRESH LT_LINES_AUX.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = '0001'
            LANGUAGE                = SY-LANGU
            NAME                    = LVA_NAME
            OBJECT                  = 'MVKE'
          TABLES
            LINES                   = LT_LINES_AUX
          EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

        IF LT_LINES_AUX[] IS NOT INITIAL.

          DESCRIBE TABLE TG_MENSAGEMS LINES DATA(LVA_LINES).
          DATA(LIT_MENSAGENS) = TG_MENSAGEMS[].
          SORT: LIT_MENSAGENS BY SEQNUM DESCENDING.
          READ TABLE LIT_MENSAGENS INTO DATA(LWA_MSG) INDEX 1.
          DATA(LVA_SEQNUM) = LWA_MSG-SEQNUM.

          LOOP AT LT_LINES_AUX INTO LWA_LINES.
            LVA_LINES = LVA_LINES + 1.
            LVA_SEQNUM = LVA_SEQNUM + 1.
            TG_MENSAGEMS-LINNUM = LVA_LINES.
            TG_MENSAGEMS-SEQNUM = LVA_SEQNUM.
            TG_MENSAGEMS-MESSAGE = LWA_LINES-TDLINE.
            APPEND TG_MENSAGEMS.
          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INPUT_OUTPUT_MATNR
*&---------------------------------------------------------------------*
FORM F_INPUT_MATNR  CHANGING P_MATNR TYPE MATNR.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = P_MATNR
    IMPORTING
      OUTPUT       = P_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_COR_IMPOSTO
*&---------------------------------------------------------------------*
FORM F_MONTA_COR_IMPOSTO .
  IF VL_PRAZO = 'D'.
    LOOP AT TG_IMPO_AUX ASSIGNING FIELD-SYMBOL(<LFS_IMPO>) WHERE TAXTYP = 'ICM3'.
      <LFS_IMPO>-COLOR = 'C310'.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM F_GET_DATA_LIM. "PSA

  XVALS-TABNAME   = 'ZSTRUCT_GET_DATA_LIM'.
  XVALS-FIELDNAME = 'ID'.
  APPEND XVALS TO IVALS.



  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      POPUP_TITLE     = 'LIM'
    TABLES
      FIELDS          = IVALS
    EXCEPTIONS
      ERROR_IN_FIELDS = 1
      OTHERS          = 2.

  CONDENSE XVALS-VALUE NO-GAPS.

  READ TABLE IVALS INTO XVALS WITH KEY FIELDNAME = 'ID'.
  IF SY-SUBRC  = 0.
    WA_PARAM  =  VALUE #(  ID = XVALS-VALUE ENDPOINT = '' METHOD = 'POST'  ).

    TRY.
        ZCL_INT_OB_GET_DATA_LIM=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE(
        )->EXECUTE_REQUEST(
          EXPORTING
            I_INFO_REQUEST           = WA_PARAM
          IMPORTING
            E_ID_INTEGRACAO          = DATA(RESUL_ID)    " Id. de Integração
            E_INTEGRACAO             = DATA(RESULT_JSON)    " Tabela de Integração
        ).

        IF RESULT_JSON IS NOT INITIAL.

          CALL METHOD /UI2/CL_JSON=>DESERIALIZE
            EXPORTING
              JSON = RESULT_JSON-DS_DATA_RETORNO
            CHANGING
              DATA = LR_CONTENT.

          IF LR_CONTENT-CONTENT  IS NOT INITIAL.
**********************************************************************
*POPULA CAMPOS ZNFW0002
**********************************************************************

            PERFORM POPULA_ITENS_API.

*            REFRESH: tg_parc.
            PERFORM BUSCA_DADOS.
*      PERFORM busca_dados_doc.
            PERFORM BUSCA_DESCRICOES.

**********************************************************************
*POPULA MENSSAGEM
**********************************************************************
            APPEND VALUE #( SEQNUM = 1 LINNUM = 1 MESSAGE = WA_PARAM-ID ) TO  TG_MENSAGEMS_LIM[].


          ENDIF.
        ENDIF.

      CATCH ZCX_INTEGRACAO INTO DATA(ZCX_INTEGRACAO).
        MESSAGE ID ZCX_INTEGRACAO->ZIF_ERROR~MSGID TYPE 'I'
         NUMBER ZCX_INTEGRACAO->ZIF_ERROR~MSGNO
           WITH ZCX_INTEGRACAO->ZIF_ERROR~MSGV1
                ZCX_INTEGRACAO->ZIF_ERROR~MSGV2
                ZCX_INTEGRACAO->ZIF_ERROR~MSGV3
                ZCX_INTEGRACAO->ZIF_ERROR~MSGV4.


      CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).
*        MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
*         NUMBER zcx_error->zif_error~msgno
*           WITH zcx_error->zif_error~msgv1
*                zcx_error->zif_error~msgv2
*                zcx_error->zif_error~msgv3
*                zcx_error->zif_error~msgv4.



        MESSAGE E024(SD) WITH 'A LIM não foi encontrada!'.

    ENDTRY.


  ENDIF.


ENDFORM.


FORM POPULA_ITENS_API.

  CLEAR: P_OPERACAO,P_BUKRS,P_BRANCH,P_PARID.

  P_OPERACAO = LR_CONTENT-CONTENT-OPERACAO.
  P_BUKRS = LR_CONTENT-CONTENT-ID_EMPRESA_SAP.
  P_BRANCH = LR_CONTENT-CONTENT-ID_FILIAL_SAP.
  P_PARID = LR_CONTENT-CONTENT-CODDOCLIENTE.


  DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
        WL_ITENS     LIKE LINE OF TG_ITENS,
        WL_LINES     TYPE SY-TABIX.
  REFRESH: TL_ITENS_AUX.

  TL_ITENS_AUX[] = TG_ITENS[].

  REFRESH: TG_ITENS.
  CLEAR: WL_ITENS.

  LOOP AT TL_ITENS_AUX INTO WL_ITENS.

    WL_ITENS-ITMNUM = SY-TABIX * 10.
    WL_ITENS-FASE = ICON_DISPLAY_MORE.
    APPEND WL_ITENS TO TG_ITENS.

  ENDLOOP.

  DESCRIBE TABLE TG_ITENS LINES WL_LINES.


  WL_ITENS-FASE = ICON_DISPLAY_MORE.
  WL_ITENS-ITMNUM = ( WL_LINES + 1 ) * 10 .
  WL_ITENS-MATNR = LR_CONTENT-CONTENT-CODMATERIAL.
  WL_ITENS-WERKS = LR_CONTENT-CONTENT-CENTROCUSTO.
  WL_ITENS-ANLN1 = LR_CONTENT-CONTENT-CODIMOBILIZADO.
  WL_ITENS-ANLN2 = LR_CONTENT-CONTENT-CODIMOBSUB.
  WL_ITENS-MENGE = LR_CONTENT-CONTENT-QUANTIDADE2.
  WL_ITENS-MEINS = LR_CONTENT-CONTENT-UNIDADE.
  WL_ITENS-NETPR = LR_CONTENT-CONTENT-VENDADIRETA.


  DATA: API_MATERIAL TYPE MARA-MATNR.
  API_MATERIAL = |{ LR_CONTENT-CONTENT-CODMATERIAL ALPHA = IN }| .

  SELECT SINGLE A~MAKTG,B~STEUC FROM M_MAT1M AS A
    INNER JOIN MARC AS B ON B~MATNR = A~MATNR
    INNER JOIN MBEW AS C ON C~MATNR = A~MATNR AND C~BWKEY = B~WERKS
    WHERE B~WERKS = @LR_CONTENT-CONTENT-ID_FILIAL_SAP
    AND A~MATNR = @API_MATERIAL
    INTO (@WL_ITENS-MAKTX,@WL_ITENS-STEUC).

  WG_FISCAL-MOVE_PLANT = LR_CONTENT-CONTENT-CODDOCLIENTE.
  APPEND WL_ITENS TO TG_ITENS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  INICIA_DOC_LIM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIA_DOC_LIM OUTPUT.

  IF SY-UCOMM EQ C_LIM OR SY-UCOMM EQ 'FURT'.
    IF TG_MENSAGEMS_LIM[] IS NOT INITIAL.
      LOOP AT TG_MENSAGEMS_LIM.
        IF V_AUTOMATICO_MEMO = ABAP_FALSE.
          PERFORM PREENCHE_TREE USING TG_MENSAGEMS_LIM-SEQNUM
                                      C_ROOT
                                      SPACE
                                      CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                      TG_MENSAGEMS_LIM-MESSAGE
                                      HANDLE_TREE.
        ENDIF.

      ENDLOOP.

      APPEND LINES OF TG_MENSAGEMS_LIM[] TO TG_MENSAGEMS_AUX[].

      PERFORM VERIFICA_ERROS.

      PERFORM BUSCA_DADOS.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INICIA_CHECK_IMOBILIZADO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIA_CHECK_IMOBILIZADO OUTPUT.
  IF WG_FISCAL-IMOBILIZADO EQ 'S'. "Melhoria movimentações de ativos imobilizado ZNFW - BG #155188
    IF TG_ITENS[] IS NOT INITIAL.
      LOOP AT TG_ITENS ASSIGNING FIELD-SYMBOL(<WS_ITENS>).

        IF <WS_ITENS>-ADD_TEXT EQ ABAP_TRUE.
          CONTINUE.
        ENDIF.

        IF TG_MENSAGEMS_AUX[] IS NOT INITIAL.
          SORT TG_MENSAGEMS_AUX BY SEQNUM DESCENDING LINNUM DESCENDING.
          READ TABLE TG_MENSAGEMS_AUX ASSIGNING FIELD-SYMBOL(<FS_MESSAGE>) INDEX 1.
          IF SY-SUBRC EQ 0.
            ADD 1 TO <FS_MESSAGE>-LINNUM.
            ADD 1 TO <FS_MESSAGE>-SEQNUM.

            <FS_MESSAGE>-MESSAGE = 'Imobilizado: ' && <WS_ITENS>-ANLN1 && <WS_ITENS>-ANLN2.

            <WS_ITENS>-ADD_TEXT = ABAP_TRUE.
          ENDIF.
        ELSE.
          APPEND INITIAL LINE TO TG_MENSAGEMS_AUX ASSIGNING <FS_MESSAGE>.
          ADD 1 TO <FS_MESSAGE>-LINNUM.
          ADD 1 TO <FS_MESSAGE>-SEQNUM.
          <FS_MESSAGE>-MESSAGE = 'Imobilizado: ' && <WS_ITENS>-ANLN1 && <WS_ITENS>-ANLN2.
          <WS_ITENS>-ADD_TEXT = ABAP_TRUE.
        ENDIF.
      ENDLOOP.

      LOOP AT TG_MENSAGEMS_AUX.

        PERFORM PREENCHE_TREE USING TG_MENSAGEMS_AUX-SEQNUM
                                        C_ROOT
                                        SPACE
                                        CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD
                                        TG_MENSAGEMS_AUX-MESSAGE
                                        HANDLE_TREE.
      ENDLOOP.

*    PERFORM verifica_erros.

*    PERFORM busca_dados.
    ENDIF.
  ENDIF."Melhoria movimentações de ativos imobilizado ZNFW - BG #155188

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_IMBOLIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_TEXTO_IMBOLIZADO .

ENDFORM.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
*&---------------------------------------------------------------------*
*& Form f_j1btax_43_31
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- WL_1BTXIC
*&---------------------------------------------------------------------*
FORM F_J1BTAX_43_31 USING WL_ITENS     LIKE LINE OF TG_ITENS
                    CHANGING WL_1BTXIC TYPE ANY.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
  IF WL_ITENS-WERKS IS NOT INITIAL AND WL_ITENS-MATNR IS NOT INITIAL AND WL_1BTXIC IS INITIAL.

    SELECT SINGLE EXTWG
      FROM MARA
      INTO @DATA(WL_EXTWG)
        WHERE MATNR EQ @WL_ITENS-MATNR.

    IF SY-SUBRC IS INITIAL.
      IF WL_EXTWG IS NOT INITIAL.
        " Grupo 43
*        IF wl_1btxic IS INITIAL.
        SELECT SINGLE RATE BASE
                       FROM J_1BTXIC3
                       INTO WL_1BTXIC
                        WHERE LAND1     = C_BR
                          AND SHIPFROM  = WG_SHIPFROM
                          AND SHIPTO    = WG_SHIPTO
                          AND GRUOP     = C_43
                          AND VALUE     = WL_EXTWG
                          AND VALUE2    = WL_ITENS-WERKS.
*        ELSE.
*          SELECT SINGLE rate base
*                         FROM j_1btxic3
*                         INTO wl_1btxic
*                          WHERE land1     = c_br
*                            AND shipfrom  = wg_shipfrom
*                            AND shipto    = wg_shipto
*                            AND gruop     = c_43
*                            AND value     = wl_extwg
*                            AND value2    = wl_itens-werks.
*        ENDIF.
      ENDIF.

      IF ( WL_EXTWG IS INITIAL OR WL_1BTXIC IS INITIAL ).
        " Grupo 31
        SELECT SINGLE RATE BASE
                       FROM J_1BTXIC3
                       INTO WL_1BTXIC
                        WHERE LAND1     = C_BR
                          AND SHIPFROM  = WG_SHIPFROM
                          AND SHIPTO    = WG_SHIPTO
                          AND GRUOP     = C_31
                          AND VALUE     = WL_ITENS-WERKS
                          AND VALUE2    = WL_ITENS-MATNR
                          AND VALUE3    = P_PARID.
      ENDIF.

      IF WL_1BTXIC IS INITIAL.
        SELECT SINGLE RATE BASE
                       FROM J_1BTXIC3
                       INTO WL_1BTXIC
                        WHERE LAND1     = C_BR
                          AND SHIPFROM  = WG_SHIPFROM
                          AND SHIPTO    = WG_SHIPTO
                          AND GRUOP     = C_31
                          AND VALUE     = WL_ITENS-WERKS
                          AND VALUE2    = WL_ITENS-MATNR
                          AND VALUE3    = ''.
      ENDIF.
    ENDIF. "Mara
  ENDIF. "Itens matnr werks
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
ENDFORM.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05

* US #163043 - MMSILVA - 18.03.2024 - Inicio
FORM PREENCHE_DADOS_NFE.
  IF P_OPERACAO IS NOT INITIAL.
    SELECT SINGLE * FROM J_1BAA INTO WL_1BAA WHERE NFTYPE EQ WG_FISCAL-NFTYPE.
    IF WL_1BAA-DIRECT = '1'.

      LOOP AT SCREEN.

        IF SCREEN-NAME EQ 'P_CHAVE_ACESSO'.

          SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
          SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

      IF P_CHAVE_ACESSO IS NOT INITIAL.

* ------> US #172277 - MMSILVA - 28.03.2025 - Inicio <------
        REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN P_CHAVE_ACESSO WITH ''.
        CONDENSE P_CHAVE_ACESSO NO-GAPS.
* ------> US #172277 - MMSILVA - 28.03.2025 - Fim <------

        MOVE 'CIF' TO WG_FISCAL-INCO1.
        MOVE 'CIF' TO WG_FISCAL-INCO2.

        SELECT SINGLE BUKRS, BRANCH, P_EMISSOR, NUMERO, DT_EMISSAO, SERIE FROM ZIB_NFE_DIST_TER INTO @DATA(WA_ZIB_NFE_DIST_TER) WHERE CHAVE_NFE EQ @P_CHAVE_ACESSO.

        SELECT SINGLE * FROM ZIB_CTE_DIST_TER INTO @DATA(WA_ZIB_CTE_DIST_TER) WHERE CD_CHAVE_CTE EQ @P_CHAVE_ACESSO.

        IF WA_ZIB_NFE_DIST_TER IS NOT INITIAL OR WA_ZIB_CTE_DIST_TER IS NOT INITIAL.

          "Preenche dados empresa
          IF WA_ZIB_NFE_DIST_TER-BUKRS IS NOT INITIAL.
            MOVE WA_ZIB_NFE_DIST_TER-BUKRS TO P_BUKRS.
          ELSEIF WA_ZIB_CTE_DIST_TER-E_TOMADORA IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-E_TOMADORA TO P_BUKRS.
          ENDIF.

          IF WA_ZIB_NFE_DIST_TER-BRANCH IS NOT INITIAL.
            MOVE WA_ZIB_NFE_DIST_TER-BRANCH TO P_BRANCH.
          ELSEIF WA_ZIB_CTE_DIST_TER-F_TOMADORA IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-F_TOMADORA TO P_BRANCH.
          ENDIF.

          "Preenche dados emissor
          IF ( WA_ZIB_NFE_DIST_TER-P_EMISSOR IS NOT INITIAL ).
            MOVE WA_ZIB_NFE_DIST_TER-P_EMISSOR TO P_PARID.
          ELSEIF WA_ZIB_CTE_DIST_TER-P_EMISSOR IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-P_EMISSOR TO P_PARID.
          ENDIF.

          "Preenche dados nota fiscal
          IF ( WA_ZIB_NFE_DIST_TER-NUMERO IS NOT INITIAL ).
            MOVE WA_ZIB_NFE_DIST_TER-NUMERO TO WG_DOCS-NFENUM.
          ELSEIF WA_ZIB_CTE_DIST_TER-NUMR_CTE IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-NUMR_CTE TO WG_DOCS-NFENUM.
          ENDIF.

          IF ( WA_ZIB_NFE_DIST_TER-DT_EMISSAO IS NOT INITIAL ).
            MOVE WA_ZIB_NFE_DIST_TER-DT_EMISSAO TO WG_DOCS-BLDAT.
          ELSEIF WA_ZIB_CTE_DIST_TER-DT_EMISSAO IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-DT_EMISSAO TO WG_DOCS-BLDAT.
          ENDIF.

          IF ( WA_ZIB_NFE_DIST_TER-SERIE IS NOT INITIAL ).
            MOVE WA_ZIB_NFE_DIST_TER-SERIE TO WG_DOCS-SERIES.
          ELSEIF WA_ZIB_CTE_DIST_TER-NUMR_SERIE IS NOT INITIAL.
            MOVE WA_ZIB_CTE_DIST_TER-NUMR_SERIE TO WG_DOCS-SERIES.
          ENDIF.

        ENDIF.

        "Preenche aba Itens Nota
*          free: tg_itens.
        SELECT * FROM ZIB_NFE_DIST_ITM INTO TABLE @DATA(IT_ZIB_NFE_DIST_ITM) WHERE CHAVE_NFE EQ @P_CHAVE_ACESSO.

        SELECT * FROM ZIB_CTE_DIST_TER INTO TABLE @DATA(IT_ZIB_CTE_DIST_TER) WHERE CD_CHAVE_CTE EQ @P_CHAVE_ACESSO.

        IF IT_ZIB_NFE_DIST_ITM IS NOT INITIAL OR IT_ZIB_CTE_DIST_TER IS NOT INITIAL.
          DATA: WL_ITENS LIKE LINE OF TG_ITENS.

          "NF-e
          LOOP AT IT_ZIB_NFE_DIST_ITM INTO DATA(WA_ZIB_NFE_DIST_ITM).
            READ TABLE TG_ITENS INTO WL_ITENS INDEX SY-TABIX.

            IF SY-SUBRC IS NOT INITIAL.
              APPEND WL_ITENS TO TG_ITENS.
            ENDIF.

*              clear: wl_itens.
            WL_ITENS-FASE = ICON_DISPLAY_MORE.
            WL_ITENS-ITMNUM = ( WA_ZIB_NFE_DIST_ITM-PROD_ITEM ) * 10.
            WL_ITENS-MENGE = WA_ZIB_NFE_DIST_ITM-PROD_QTD_COMERCI.
            WL_ITENS-NETPR = ( WA_ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B  + WA_ZIB_NFE_DIST_ITM-IPI_VALOR + WA_ZIB_NFE_DIST_ITM-ICMS_ST_VALOR - WA_ZIB_NFE_DIST_ITM-PROD_VL_DESCONTO - WA_ZIB_NFE_DIST_ITM-ICMS_VL_DESONERADO ) /
WA_ZIB_NFE_DIST_ITM-PROD_QTD_COMERCI.
            WL_ITENS-PROD_DESCRICAO = WA_ZIB_NFE_DIST_ITM-PROD_DESCRICAO.
            WL_ITENS-WERKS = WA_ZIB_NFE_DIST_TER-BRANCH.
*              append wl_itens to tg_itens.
            MODIFY TG_ITENS FROM WL_ITENS INDEX SY-TABIX.
          ENDLOOP.

          "CTE
          LOOP AT IT_ZIB_CTE_DIST_TER INTO DATA(LWA_ZIB_CTE_DIST_TER).
            READ TABLE TG_ITENS INTO WL_ITENS INDEX SY-TABIX.

            IF SY-SUBRC IS NOT INITIAL.
              APPEND WL_ITENS TO TG_ITENS.
            ENDIF.

*              clear: wl_itens.
            WL_ITENS-FASE = ICON_DISPLAY_MORE.
            WL_ITENS-ITMNUM = '10'.
            WL_ITENS-MENGE = '1'.
            WL_ITENS-NETPR = LWA_ZIB_CTE_DIST_TER-VALOR_PRESTACAO.
            WL_ITENS-PROD_DESCRICAO = LWA_ZIB_CTE_DIST_TER-DS_PROD_PRED.
            WL_ITENS-WERKS = LWA_ZIB_CTE_DIST_TER-F_TOMADORA.
*              append wl_itens to tg_itens.
            MODIFY TG_ITENS FROM WL_ITENS INDEX SY-TABIX.
          ENDLOOP.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ENDIF.

        "Preenche aba Parceiro
        FREE: TG_PARC.
        WL_PARC-PARVW = P_PARVW.
        WL_PARC-PARID = WA_ZIB_NFE_DIST_TER-P_EMISSOR.

        SELECT SINGLE NAME1 FROM LFA1 INTO @DATA(WL_LFA1_NAME1) WHERE LIFNR EQ @WA_ZIB_NFE_DIST_TER-P_EMISSOR.
        WL_PARC-NOME = WL_LFA1_NAME1.
        APPEND WL_PARC TO TG_PARC.

        CALL METHOD GRID5->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.

        "Preenche os campos wg_shipto e wg_shipfrom
        SELECT SINGLE REGIO FROM T001W INTO @DATA(WA_T001W_REGIO) WHERE WERKS EQ @P_BRANCH.

        IF SY-SUBRC IS INITIAL.
          MOVE WA_T001W_REGIO TO WG_SHIPTO.
        ENDIF.

        SELECT SINGLE REGIO FROM LFA1 INTO @DATA(WA_LFA1_REGIO) WHERE LIFNR EQ @P_PARID.

        IF SY-SUBRC IS INITIAL.
          MOVE WA_LFA1_REGIO TO WG_SHIPFROM.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM ATUALIZA_ICMS.
  CONSTANTS:
    READ(4) TYPE C VALUE 'READ',
    EDIT(4) TYPE C VALUE 'EDIT',
    SAVE(4) TYPE C VALUE 'SAVE'.

  DATA: WA_J_1BTXIC3    TYPE J_1BTXIC3.

  DATA: LT_RETURNS  TYPE TABLE OF BAPIRET2,
        WL_IMPO_AUX LIKE LINE OF TG_IMPO_AUX.

  DATA: CHDAT(8)   TYPE C,
        HOUTPUT(8) TYPE N.

  DATA  FUNCTION    LIKE SY-UCOMM.
  DATA: VARIANT_FOR_SELECTION LIKE TVIMV-VARIANT,
        VIEW_NAME             LIKE  DD02V-TABNAME.

  DATA: BEGIN OF STATUS_J_1BTXIC3V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
  DATA: END OF STATUS_J_1BTXIC3V.

  DATA: BEGIN OF HEADER OCCURS 1.
          INCLUDE STRUCTURE VIMDESC.
  DATA: END OF HEADER.

  DATA: BEGIN OF NAMTAB OCCURS 50.
          INCLUDE STRUCTURE VIMNAMTAB.
  DATA: END OF NAMTAB.

  DATA: RANGETAB    TYPE TABLE OF VIMSELLIST INITIAL SIZE 50
         WITH HEADER LINE,
        OC_RANGETAB TYPE TABLE OF VIMSELLIST INITIAL SIZE 50.

  DATA: DPL_SELLIST    TYPE TABLE OF VIMSELLIST INITIAL SIZE 50 WITH HEADER LINE.

  DATA: BEGIN OF E071K_TAB OCCURS 100.    "keys of changed entries
          INCLUDE STRUCTURE E071K.       "(used as parameter for VIEWPROC)
  DATA: END OF E071K_TAB.

  DATA: ORG_CRIT_INST TYPE VIMTY_OC_TYPE, LOCKUSER TYPE SY-UNAME.
  DATA: EXCL_CUA_FUNCT LIKE VIMEXCLFUN OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF J_1BTXIC3V_EXTRACT OCCURS 0010.
          INCLUDE STRUCTURE J_1BTXIC3V.
          INCLUDE STRUCTURE VIMFLAGTAB.
  DATA: END OF J_1BTXIC3V_EXTRACT.
* Table for all entries loaded from database
  DATA: BEGIN OF J_1BTXIC3V_TOTAL OCCURS 0010.
          INCLUDE STRUCTURE J_1BTXIC3V.
          INCLUDE STRUCTURE VIMFLAGTAB.
  DATA: END OF J_1BTXIC3V_TOTAL.


  REFRESH NAMTAB.
  CLEAR   NAMTAB.
  REFRESH HEADER.
  CLEAR   HEADER.
  REFRESH RANGETAB.
  CLEAR   RANGETAB.
  CALL FUNCTION 'VIEW_GET_DDIC_INFO'
    EXPORTING
      VIEWNAME              = 'J_1BTXIC3V' "view_name
      VARIANT_FOR_SELECTION = VARIANT_FOR_SELECTION
    TABLES
      X_HEADER              = HEADER
      X_NAMTAB              = NAMTAB
      SELLIST               = RANGETAB
    EXCEPTIONS
      NO_TVDIR_ENTRY        = 3
      TABLE_NOT_FOUND       = 5.

  "gravar GRUOP 31  rangetab
  READ TABLE RANGETAB ASSIGNING FIELD-SYMBOL(<RANGETAB>) WITH KEY VIEWFIELD = 'GRUOP'.
  IF SY-SUBRC = 0.
    <RANGETAB>-VALUE = '31'.
  ENDIF.
  READ TABLE RANGETAB ASSIGNING <RANGETAB> WITH KEY VIEWFIELD = 'LAND1'.
  IF SY-SUBRC = 0.
    <RANGETAB>-VALUE = 'BR'.
  ENDIF.

  READ TABLE HEADER INDEX 1.
  CALL FUNCTION 'VIEWPROC_J_1BTXIC3V'
    EXPORTING
      FCODE                     = READ
      VIEW_ACTION               = 'U' "view_action
      VIEW_NAME                 = 'J_1BTXIC3V' "view_name
    TABLES
      EXCL_CUA_FUNCT            = EXCL_CUA_FUNCT
      EXTRACT                   = J_1BTXIC3V_EXTRACT
      TOTAL                     = J_1BTXIC3V_TOTAL
      X_HEADER                  = HEADER
      X_NAMTAB                  = NAMTAB
      DBA_SELLIST               = RANGETAB "dba_sellist
      DPL_SELLIST               = DPL_SELLIST
      CORR_KEYTAB               = E071K_TAB
    EXCEPTIONS
      MISSING_CORR_NUMBER       = 1
      NO_VALUE_FOR_SUBSET_IDENT = 2.
  CASE SY-SUBRC.
    WHEN 1.
      RAISE MISSING_CORR_NUMBER.
    WHEN 2.
      RAISE NO_VALUE_FOR_SUBSET_IDENT.
  ENDCASE.
  "
  STATUS_J_1BTXIC3V-UPD_FLAG = 'X'.
*
  REFRESH:  J_1BTXIC3V_EXTRACT, J_1BTXIC3V_TOTAL.
  "
  DATA: WL_ITENS LIKE LINE OF TG_ITENS.

  IF P_PARID IS NOT INITIAL.
    SELECT SINGLE REGIO FROM LFA1 INTO @DATA(WA_REGIO_PARID) WHERE LIFNR EQ @P_PARID.
  ENDIF.

  IF P_BRANCH IS NOT INITIAL.
    SELECT SINGLE REGIO FROM T001W INTO @DATA(WA_REGIO_BRANCH) WHERE WERKS EQ @P_BRANCH.
  ENDIF.

  CALL METHOD GRID1->GET_SELECTED_CELLS
    IMPORTING
      ET_CELL = TG_SELECTEDCELL.

  READ TABLE TG_SELECTEDCELL[] INTO WG_SELECTEDCELL INDEX SY-INDEX.

  READ TABLE TG_ITENS[] INTO WL_ITENS INDEX WG_SELECTEDCELL-ROW_ID.

  DATA: WA_ICMS_NFE_J1BTXIC3 TYPE ZIB_NFE_DIST_ITM,
        WA_ICMS_CTE_J1BTXIC3 TYPE ZIB_CTE_DIST_TER.

*  read table tg_impo_aux[] into wl_impo_aux with key taxtyp = 'ICM3'.
  SELECT SINGLE * INTO @WA_ICMS_NFE_J1BTXIC3 FROM ZIB_NFE_DIST_ITM WHERE CHAVE_NFE EQ @P_CHAVE_ACESSO AND PROD_ITEM EQ @WG_SELECTEDCELL-ROW_ID.
  IF SY-SUBRC IS INITIAL.
    WA_ICMS_J1BTXIC3-ICMS_BASE = ( WA_ICMS_NFE_J1BTXIC3-ICMS_BASE ) / ( WA_ICMS_NFE_J1BTXIC3-PROD_VLR_TOTAL_B + WA_ICMS_NFE_J1BTXIC3-IPI_VALOR + WA_ICMS_NFE_J1BTXIC3-ICMS_ST_VALOR + WA_ICMS_NFE_J1BTXIC3-PROD_VL_FRETE -
WA_ICMS_NFE_J1BTXIC3-ICMS_VL_DESONERADO - WA_ICMS_NFE_J1BTXIC3-PROD_VL_DESCONTO )  * 100.
    WA_ICMS_J1BTXIC3-ICMS_RATE = WA_ICMS_NFE_J1BTXIC3-ICMS_AQT.
  ELSE.
    SELECT SINGLE * INTO @WA_ICMS_CTE_J1BTXIC3 FROM ZIB_CTE_DIST_TER WHERE CD_CHAVE_CTE EQ @P_CHAVE_ACESSO.

    WA_ICMS_J1BTXIC3-ICMS_BASE = ( WA_ICMS_CTE_J1BTXIC3-VALOR_BASE_ICMS / WA_ICMS_CTE_J1BTXIC3-VALOR_PRESTACAO ) * 100.
    WA_ICMS_J1BTXIC3-ICMS_RATE = ( WA_ICMS_CTE_J1BTXIC3-VALOR_ICMS / WA_ICMS_CTE_J1BTXIC3-VALOR_BASE_ICMS ) * 100.
  ENDIF.

  IF ( WA_REGIO_BRANCH IS NOT INITIAL ) AND ( WA_REGIO_PARID IS NOT INITIAL ) AND ( WL_ITENS-MATNR IS NOT INITIAL ).

    CLEAR: J_1BTXIC3V_EXTRACT, J_1BTXIC3V_TOTAL.
*    move-corresponding wa_zmmt0168 to j_1btxic3v_extract.
    J_1BTXIC3V_EXTRACT-LAND1      = 'BR'.
    J_1BTXIC3V_EXTRACT-GRUOP      = '31'.
    J_1BTXIC3V_EXTRACT-SHIPFROM   = WA_REGIO_PARID.
    J_1BTXIC3V_EXTRACT-SHIPTO     = WA_REGIO_BRANCH.
    J_1BTXIC3V_EXTRACT-VALUE      = P_BRANCH.
    J_1BTXIC3V_EXTRACT-VALUE2     = WL_ITENS-MATNR.
    J_1BTXIC3V_EXTRACT-VALUE3     = P_PARID.
    J_1BTXIC3V_EXTRACT-RATE       = WA_ICMS_J1BTXIC3-ICMS_RATE.
    J_1BTXIC3V_EXTRACT-BASE       = WA_ICMS_J1BTXIC3-ICMS_BASE.
    IF WA_ICMS_J1BTXIC3-ICMS_BASE LT 100.
      J_1BTXIC3V_EXTRACT-TAXLAW     = 'IA2'.
    ENDIF.
    SELECT SINGLE *
       INTO WA_J_1BTXIC3
       FROM J_1BTXIC3
       WHERE LAND1     = 'BR'
       AND   SHIPFROM  = WA_REGIO_PARID
    AND   SHIPTO    = WA_REGIO_BRANCH
    AND   GRUOP     = '31'
    AND   VALUE     = P_BRANCH
    AND   VALUE2    = WL_ITENS-MATNR
    AND   VALUE3    = P_PARID.
    IF SY-SUBRC = 0.
      J_1BTXIC3V_EXTRACT-VALIDTO    = WA_J_1BTXIC3-VALIDTO.
      J_1BTXIC3V_EXTRACT-VALIDFROM  = WA_J_1BTXIC3-VALIDFROM.
      J_1BTXIC3V_EXTRACT-ACTION     = 'U'.
    ELSE.
*      MOVE sy-datum TO chdat.
      MOVE '20010101' TO CHDAT.
      HOUTPUT = '99999999' - CHDAT.
      J_1BTXIC3V_EXTRACT-VALIDFROM  = HOUTPUT.
      MOVE '99991231' TO CHDAT.
      HOUTPUT = '99999999' - CHDAT.
      J_1BTXIC3V_EXTRACT-VALIDTO    = HOUTPUT.
      J_1BTXIC3V_EXTRACT-ACTION     = 'N'.
    ENDIF.
    APPEND J_1BTXIC3V_EXTRACT.
    MOVE-CORRESPONDING J_1BTXIC3V_EXTRACT TO J_1BTXIC3V_TOTAL.
    APPEND J_1BTXIC3V_TOTAL.
*  endloop.

    IF J_1BTXIC3V_EXTRACT[] IS NOT INITIAL.
      CALL FUNCTION 'VIEWPROC_J_1BTXIC3V'
        EXPORTING
          FCODE                     = SAVE
          VIEW_ACTION               = 'U' "maint_mode
          VIEW_NAME                 = 'J_1BTXIC3V' "view_name
          CORR_NUMBER               = ' ' "corr_number
        IMPORTING
          UPDATE_REQUIRED           = STATUS_J_1BTXIC3V-UPD_FLAG
        TABLES
          EXCL_CUA_FUNCT            = EXCL_CUA_FUNCT
          EXTRACT                   = J_1BTXIC3V_EXTRACT
          TOTAL                     = J_1BTXIC3V_TOTAL
          X_HEADER                  = HEADER
          X_NAMTAB                  = NAMTAB
          DBA_SELLIST               = RANGETAB "dba_sellist
          DPL_SELLIST               = DPL_SELLIST
          CORR_KEYTAB               = E071K_TAB
        EXCEPTIONS
          MISSING_CORR_NUMBER       = 1
          NO_VALUE_FOR_SUBSET_IDENT = 2
          SAVING_CORRECTION_FAILED  = 3.
      CASE SY-SUBRC.
        WHEN 1.
          RAISE MISSING_CORR_NUMBER.
        WHEN 2.
          RAISE NO_VALUE_FOR_SUBSET_IDENT.
        WHEN 3.
      ENDCASE.
      "
      CLEAR  STATUS_J_1BTXIC3V-UPD_FLAG.
      FUNCTION = SAVE.
      EXCL_CUA_FUNCT-FUNCTION = 'ANZG'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'NEWL'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'KOPE'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'DELE'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'ORGI'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'MKAL'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'MKBL'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'MKLO'.
      APPEND EXCL_CUA_FUNCT.
      EXCL_CUA_FUNCT-FUNCTION = 'HELP'.
      APPEND EXCL_CUA_FUNCT.

*      call function 'VIEWPROC_J_1BTXIC3V'
*        exporting
*          fcode                     = edit
*          view_action               = 'U' "maint_mode
*          view_name                 = 'J_1BTXIC3V' "view_name
*          corr_number               = '' "corr_number
*        importing
*          ucomm                     = function
*          update_required           = status_j_1btxic3v-upd_flag
*        tables
*          excl_cua_funct            = excl_cua_funct
*          extract                   = j_1btxic3v_extract
*          total                     = j_1btxic3v_total
*          x_header                  = header
*          x_namtab                  = namtab
*          dba_sellist               = rangetab "dba_sellist
*          dpl_sellist               = dpl_sellist
*          corr_keytab               = e071k_tab
*        exceptions
*          missing_corr_number       = 1
*          no_value_for_subset_ident = 2.
*      case sy-subrc.
*        when 1.
*
*        when 2.
*
*        when others.
*          exit.
*      endcase.

      SELECT SINGLE *
       INTO WA_J_1BTXIC3
       FROM J_1BTXIC3
       WHERE LAND1     = 'BR'
       AND   SHIPFROM  = WA_REGIO_PARID
       AND   SHIPTO    = WA_REGIO_BRANCH
       AND   GRUOP     = '31'
       AND   VALUE     = P_BRANCH
       AND   VALUE2    = WL_ITENS-MATNR
       AND   VALUE3    = P_PARID.

*      field-symbols: <fs_atual_icms> type any.

      LOOP AT TG_IMPO_AUX ASSIGNING FIELD-SYMBOL(<FS_ATUAL_ICMS>).

        IF <FS_ATUAL_ICMS>-TAXTYP EQ 'ICM3'.
          DATA: WL_CALCULO_BASE   TYPE ZFIWRT0010-BASE,
                WL_CALCULO_TAXVAL TYPE ZFIWRT0010-TAXVAL.

          WL_CALCULO_BASE   = WL_ITENS-NETWR * ( WA_J_1BTXIC3-BASE / 100 ).
          WL_CALCULO_TAXVAL = WL_CALCULO_BASE * ( WA_J_1BTXIC3-RATE / 100 ).

          MOVE: WL_CALCULO_BASE   TO <FS_ATUAL_ICMS>-BASE,
                WA_J_1BTXIC3-RATE TO <FS_ATUAL_ICMS>-RATE,
                WL_CALCULO_TAXVAL TO <FS_ATUAL_ICMS>-TAXVAL.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.

  IF LT_RETURNS[] IS NOT INITIAL.
    MESSAGE S024(SD) WITH 'ICMS atualizado com sucesso.'.
  ENDIF.
ENDFORM.
* US #163043 - MMSILVA - 18.03.2024 - Fim
