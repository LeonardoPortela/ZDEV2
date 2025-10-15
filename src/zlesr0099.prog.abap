*&---------------------------------------------------------------------*
*& Report  ZLESR0099
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0099 MESSAGE-ID ZLES.

TABLES: J_1BBRANCH, LIKP, ZSDT0001, VTTK, MARA.

TYPES: BEGIN OF TY_REMESSA,
         ERDAT         TYPE LIKP-ERDAT, "= FILTRO (Data de Criação da Remessa)
         VKORG         TYPE LIKP-VKORG, "= FILTRO (Empresa)
         VSTEL         TYPE LIKP-VSTEL, "= FILTRO (Filial)
         VBELN         TYPE LIKP-VBELN, "= FILTRO (Remessa)
         ROUTE         TYPE LIKP-ROUTE, "– Itinerário;
         INCO1         TYPE LIKP-INCO1, "– Tipo do Frete;
         XBLNR         TYPE LIKP-XBLNR, "– Chave de Referência – Planilha do Romaneio;
         VBTYP         TYPE LIKP-VBTYP, "- Ctg. documento de vendas e distribuição
         CH_REFERENCIA TYPE ZSDT0001-CH_REFERENCIA,
       END OF TY_REMESSA,

       BEGIN OF TY_ITEM_REMESSA,
         VBELN TYPE LIPS-VBELN, "- Fornecimento;
         POSNR TYPE LIPS-POSNR, "- Item do Fornecimento;
         FKREL TYPE LIPS-FKREL, "- Relevância para o documento de faturamento;
         BWART TYPE LIPS-BWART, "- Tipo de movimento (administração de estoques);
         MATNR TYPE LIPS-MATNR, "- Nº do material;
       END OF TY_ITEM_REMESSA,

       BEGIN OF TY_REMESSA_PARC,
         VBELN TYPE VBPA-VBELN, "- Remessa;
         LIFNR TYPE VBPA-LIFNR, "- Agente de Frete da Remessa;
       END OF TY_REMESSA_PARC,

       BEGIN OF TY_DOC_TRANSP_ITM,
         TKNUM TYPE VTTP-TKNUM, "– Nr. Documento de Transporte;
         VBELN TYPE VTTP-VBELN, "- Fornecimento;
       END OF TY_DOC_TRANSP_ITM,

       BEGIN OF TY_DOC_TRANSP,
         TKNUM TYPE VTTK-TKNUM, "– Nr. Documento de Transporte;
         VSART TYPE VTTK-VSART, "- Tipo de expedição;
         TDLNR TYPE VTTK-TDLNR, "- Agente de Frete
       END OF TY_DOC_TRANSP,

       BEGIN OF TY_ROMANEIO,
         NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO  , "– Número do Romaneio;
         PLACA_CAV     TYPE ZSDT0001-PLACA_CAV    , "– Placa do Cavalo;
         NR_TICKET     TYPE ZSDT0001-NR_TICKET    , "– Número do Ticket;
         CH_REFERENCIA TYPE ZSDT0001-CH_REFERENCIA, "– Planilha do Romaneio SIGAM
         DOC_REM       TYPE ZSDT0001-DOC_REM      , "- Fornecimento
       END OF TY_ROMANEIO,

       BEGIN OF TY_DOC_CUSTO,
         REBEL TYPE VFKP-REBEL, "- Doc. referenciado;
         FKNUM TYPE VFKP-FKNUM, "- Nº custos de frete;
         EBELN TYPE VFKP-EBELN, "- Nº do documento de compras;
         EBELP TYPE VFKP-EBELP, "- Nº item do documento de compra;
         LBLNI TYPE VFKP-LBLNI, "- Nº folha registro de serviços;
         WAERS TYPE VFKP-WAERS, "- Moeda do item dos custos de transporte;
         NETWR TYPE VFKP-NETWR, "- Valor líquido em moeda do item custos de frete;
       END OF TY_DOC_CUSTO,

       BEGIN OF TY_FATURA_ITM,
         EBELN TYPE RSEG-EBELN, "- Nº do documento de compras;
         EBELP TYPE RSEG-EBELP, "- Nº item do documento de compra;
         LFBNR TYPE RSEG-LFBNR, "- Nº documento de um documento de referência;
         BELNR TYPE RSEG-BELNR, "- Nº documento de um documento contábil;
         GJAHR TYPE RSEG-GJAHR, "- Exercício;
       END OF TY_FATURA_ITM,

       BEGIN OF TY_FATURA,
         BELNR     TYPE RBKP-BELNR,       "- Nº documento de um documento contábil;
         GJAHR     TYPE RBKP-GJAHR,       "- Exercício;
         BUDAT     TYPE RBKP-BUDAT,       "- Data de lançamento no documento;
         BLDAT     TYPE RBKP-BLDAT,       "- Data no documento;
         XBLNR     TYPE RBKP-XBLNR,       "- Nº documento de referência;
         LIFNR     TYPE RBKP-LIFNR,       "- Emissor da fatura distinto;
         ZFBDT     TYPE RBKP-ZFBDT,       "- Data base para cálculo do vencimento;
         STBLG     TYPE RBKP-STBLG,
         STJAH     TYPE RBKP-STJAH,
         LC_REFTYP TYPE J_1BNFLIN-REFTYP, "= "BI"/"MD";
         LC_REFKEY TYPE J_1BNFLIN-REFKEY, "= VBELN;
       END OF TY_FATURA,

       BEGIN OF TY_VBFA,
         VBELN     TYPE VBFA-VBELN,       "- Fatura;
         POSNN     TYPE VBFA-POSNN,       "– Item da Fatura;
         ERDAT     TYPE VBFA-ERDAT,       "- Data da Criação;
         VBELV     TYPE VBFA-VBELV,       "– Fornecimento;
         LC_REFTYP TYPE J_1BNFLIN-REFTYP, "= "BI"/"MD";
         LC_REFKEY TYPE J_1BNFLIN-REFKEY, "= VBELN;
         LC_REFITM TYPE J_1BNFLIN-REFITM, "= POSNN.
       END OF TY_VBFA,

       BEGIN OF TY_NOTA_FISCAL_ITM,
         REFTYP TYPE J_1BNFLIN-REFTYP, "- Tipo referência;
         REFKEY TYPE J_1BNFLIN-REFKEY, "- Referência ao documento de origem;
         REFITM TYPE J_1BNFLIN-REFITM, "- Item de referência ao documento de origem;
         DOCNUM TYPE J_1BNFLIN-DOCNUM, "- Nº documento;
       END OF TY_NOTA_FISCAL_ITM,

       BEGIN OF TY_NOTA_FISCAL,
         DOCNUM TYPE J_1BNFDOC-DOCNUM, "- Nº documento;
         NFENUM TYPE J_1BNFDOC-NFENUM, "- Número de documento de nove posições;
         SERIES TYPE J_1BNFDOC-SERIES, "- Séries;
         DOCDAT TYPE J_1BNFDOC-DOCDAT, "- Data do documento;
         CANCEL TYPE J_1BNFDOC-CANCEL, "- Estornado;
       END OF TY_NOTA_FISCAL,

       BEGIN OF TY_FORNECEDOR,
         LIFNR TYPE LFA1-LIFNR, "- Código do Fornecedor;
         NAME1 TYPE LFA1-NAME1, "- Nome do Fornecedor;
       END OF TY_FORNECEDOR.

""""""""""""""""""""""""""""""""""""""""""

*       BEGIN OF TY_ALV,
*         ERDAT         TYPE LIKP-ERDAT,             "= FILTRO (Data de Criação da Remessa)
*         VKORG         TYPE LIKP-VKORG,             "= FILTRO (Empresa)
*         VSTEL         TYPE LIKP-VSTEL,             "= FILTRO (Filial)
*         VBELN         TYPE LIKP-VBELN,             "= FILTRO (Remessa)
*         ROUTE         TYPE LIKP-ROUTE,             "– Itinerário;
*         INCO1         TYPE LIKP-INCO1,             "– Tipo do Frete;
*         XBLNR         TYPE LIKP-XBLNR,             "– Chave de Referência – Planilha do Romaneio;
*         VBTYP         TYPE LIKP-VBTYP,             "- Ctg. documento de vendas e distribuição
*         CH_REFERENCIA TYPE ZSDT0001-CH_REFERENCIA, "– Planilha do Romaneio SIGAM
*         FKREL         TYPE LIPS-FKREL,             "- Relevância para o documento de faturamento;
*         BWART         TYPE LIPS-BWART,             "- Tipo de movimento (administração de estoques);
*         MATNR         TYPE LIPS-MATNR,             "- Nº do material;
*         TKNUM         TYPE VTTP-TKNUM,             "– Nr. Documento de Transporte;
*         VSART         TYPE VTTK-VSART,             "- Tipo de expedição;
*         NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO  , "– Número do Romaneio;
*         PLACA_CAV     TYPE ZSDT0001-PLACA_CAV    , "– Placa do Cavalo;
*         NR_TICKET     TYPE ZSDT0001-NR_TICKET    , "– Número do Ticket;
*         REBEL         TYPE VFKP-REBEL,             "- Doc. referenciado;
*         FKNUM         TYPE VFKP-FKNUM,             "- Nº custos de frete;
*         EBELN         TYPE VFKP-EBELN,             "- Nº do documento de compras;
*         EBELP         TYPE VFKP-EBELP,             "- Nº item do documento de compra;
*         LBLNI         TYPE VFKP-LBLNI,             "- Nº folha registro de serviços;
*         WAERS         TYPE VFKP-WAERS,             "- Moeda do item dos custos de transporte;
*         NETWR         TYPE VFKP-NETWR,             "- Valor líquido em moeda do item custos de frete;
*         LFBNR         TYPE RSEG-LFBNR,             "- Nº documento de um documento de referência;
*         BELNR         TYPE RSEG-BELNR,             "- Nº documento de um documento contábil;
*         GJAHR         TYPE RSEG-GJAHR,             "- Exercício;
*         BUDAT         TYPE RBKP-BUDAT,             "- Data de lançamento no documento;
*         BLDAT         TYPE RBKP-BLDAT,             "- Data no documento;
*         XBLNR_M       TYPE RBKP-XBLNR,             "- Nº documento de referência;
*         LIFNR         TYPE RBKP-LIFNR,             "- Emissor da fatura distinto;
*         ZFBDT         TYPE RBKP-ZFBDT,             "- Data base para cálculo do vencimento;
*         VBELN_F       TYPE VBRP-VBELN,             "- Fatura;
*         POSNN_F       TYPE VBRP-POSNR,             "– Item da Fatura;
*         ERDAT_F       TYPE VBFA-ERDAT,             "- Data da Criação;
*         VBELV         TYPE LIKP-VBELN,             "– Fornecimento;
*         REFTYP        TYPE J_1BNFLIN-REFTYP,       "= "BI"/"MD";
*         REFKEY        TYPE J_1BNFLIN-REFKEY,       "= VBELN;
*         REFITM        TYPE J_1BNFLIN-REFITM,       "= POSNN.
*         DOCNUM        TYPE J_1BNFLIN-DOCNUM,       "- Nº documento;
*         NFENUM        TYPE J_1BNFDOC-NFENUM,       "- Número de documento de nove posições;
*         SERIES        TYPE J_1BNFDOC-SERIES,       "- Séries;
*         DOCDAT        TYPE J_1BNFDOC-DOCDAT,       "- Data do documento;
*         CANCEL        TYPE J_1BNFDOC-CANCEL,       "- Estornado;
*       END OF TY_ALV.

DATA: IT_REMESSA          TYPE STANDARD TABLE OF TY_REMESSA,
      IT_REMESSA_AUX      TYPE STANDARD TABLE OF TY_REMESSA,
      IT_REMESSA_ITM      TYPE STANDARD TABLE OF TY_ITEM_REMESSA,
      IT_REMESSA_ITM_AUX  TYPE STANDARD TABLE OF TY_ITEM_REMESSA,
      IT_DOC_TRANSP_ITM   TYPE STANDARD TABLE OF TY_DOC_TRANSP_ITM,
      IT_DOC_TRANSP_ITMB  TYPE STANDARD TABLE OF TY_DOC_TRANSP_ITM,
      IT_DOC_TRANSP       TYPE STANDARD TABLE OF TY_DOC_TRANSP,
      IT_ROMANEIO         TYPE STANDARD TABLE OF TY_ROMANEIO,
      IT_DOC_CUSTO        TYPE STANDARD TABLE OF TY_DOC_CUSTO,
      IT_FATURA_ITM       TYPE STANDARD TABLE OF TY_FATURA_ITM,
      IT_FATURA           TYPE STANDARD TABLE OF TY_FATURA,
      IT_VBFA             TYPE STANDARD TABLE OF TY_VBFA,
      IT_NOTA_FISCAL_ITM  TYPE STANDARD TABLE OF TY_NOTA_FISCAL_ITM,
      IT_NOTA_FISCAL      TYPE STANDARD TABLE OF TY_NOTA_FISCAL,
      IT_NOTA_FISCALC_ITM TYPE STANDARD TABLE OF TY_NOTA_FISCAL_ITM,
      IT_NOTA_FISCALC     TYPE STANDARD TABLE OF TY_NOTA_FISCAL,
      IT_ALV              TYPE STANDARD TABLE OF ZDE_FLUXO_REMESSA_ALV,
      IT_MAKT             TYPE STANDARD TABLE OF MAKT,
      IT_ZLEST0032        TYPE STANDARD TABLE OF ZLEST0032,
      IT_FORNECEDOR       TYPE STANDARD TABLE OF TY_FORNECEDOR,
      IT_REMESSA_PARC     TYPE STANDARD TABLE OF TY_REMESSA_PARC,
      WA_REMESSA          TYPE TY_REMESSA,
      WA_REMESSA_ITM      TYPE TY_ITEM_REMESSA,
      WA_REMESSA_ITM_AUX  TYPE TY_ITEM_REMESSA,
      WA_DOC_TRANSP_ITM   TYPE TY_DOC_TRANSP_ITM,
      WA_DOC_TRANSP       TYPE TY_DOC_TRANSP,
      WA_ROMANEIO         TYPE TY_ROMANEIO,
      WA_DOC_CUSTO        TYPE TY_DOC_CUSTO,
      WA_FATURA_ITM       TYPE TY_FATURA_ITM,
      WA_FATURA           TYPE TY_FATURA,
      WA_VBFA             TYPE TY_VBFA,
      WA_NOTA_FISCAL_ITM  TYPE TY_NOTA_FISCAL_ITM,
      WA_NOTA_FISCAL      TYPE TY_NOTA_FISCAL,
      WA_ALV              TYPE ZDE_FLUXO_REMESSA_ALV,
      WA_MAKT             TYPE MAKT,
      WA_ZLEST0032        TYPE ZLEST0032,
      WA_FORNECEDOR       TYPE TY_FORNECEDOR,
      WA_REMESSA_PARC     TYPE TY_REMESSA_PARC.

*1.1  Filtros
*Data de Criação da Remessa (LIKP-ERDAT) – Obrigatório – De - Até;
*Empresa (LIKP-VKORG) - Obrigatório;
*Filial (LIKP-VSTEL) - Obrigatório; (Várias)
*Remessa (LIKP-VBELN);
*Nr. Romaneio (ZSDT0001-NR_ROMANEIO);
*Placa do Cavalo (ZSDT0001-PLACA_CAV);
*Nr. Ticket Pesagem (ZSDT0001-NR_TICKET);
*Documento de Transporte (VTTP-TKNUM);
*Material (LIPS-MATNR);
*Modal de Frete (VTTK-VSART);

SELECTION-SCREEN BEGIN OF BLOCK FILTROS WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR J_1BBRANCH-BUKRS  OBLIGATORY NO INTERVALS NO-EXTENSION,
                P_BRANCH FOR J_1BBRANCH-BRANCH OBLIGATORY NO-EXTENSION,
                P_DTCREA FOR LIKP-ERDAT        OBLIGATORY NO-EXTENSION,
                P_REMESS FOR LIKP-VBELN,
                P_ROMANE FOR ZSDT0001-NR_ROMANEIO, "DEFAULT SY-DATUM,
                P_PLACA  FOR ZSDT0001-PLACA_CAV,
                P_TICKET FOR ZSDT0001-NR_TICKET,
                P_TKNUM  FOR VTTK-TKNUM,
                P_MODAL  FOR VTTK-VSART,
                P_MATNR  FOR MARA-MATNR.
SELECTION-SCREEN END OF BLOCK FILTROS.

AT SELECTION-SCREEN.

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD P_BUKRS-LOW.

  IF SY-SUBRC IS NOT INITIAL.
    SET CURSOR FIELD 'P_BUKRS-LOW'.
    MESSAGE E091(8B) WITH P_BUKRS-LOW.
  ENDIF.

START-OF-SELECTION.

  PERFORM: LIMPAR_TABELAS,
           SELECIONAR_REGISTROS,
           ORGANIZAR_DADOS,
           LIMPAR_TABELAS.

  IF IT_ALV[] IS INITIAL.
    MESSAGE S104.
  ELSE.
    PERFORM: MOSTRAR_DADOS.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TABELAS .

  CLEAR: IT_REMESSA,
         IT_REMESSA_ITM,
         IT_DOC_TRANSP_ITM,
         IT_DOC_TRANSP,
         IT_ROMANEIO,
         IT_DOC_CUSTO,
         IT_FATURA_ITM,
         IT_FATURA,
         IT_REMESSA_ITM_AUX,
         IT_VBFA,
         IT_NOTA_FISCAL_ITM,
         IT_NOTA_FISCAL,
         IT_NOTA_FISCALC_ITM,
         IT_NOTA_FISCALC,
         IT_MAKT,
         IT_ZLEST0032,
         IT_FORNECEDOR,
         IT_REMESSA_PARC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_REGISTROS;
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_REGISTROS.

  FIELD-SYMBOLS: <LIKP> TYPE TY_REMESSA,
                 <VBFA> TYPE TY_VBFA,
                 <RBKP> TYPE TY_FATURA.

* Selecionar Remessa
* Tabela:
* •  LIKP
* Onde:
* •  ERDAT = FILTRO (Data de Criação da Remessa)
* •  VKORG = FILTRO (Empresa)
* •  VSTEL = FILTRO (Filial)
* •  VBELN = FILTRO (Remessa)
* •  ROUTE <> ‘FOB’
* •  INCO1 <> ‘FOB’
* •  VBTYP = ‘J’
* Pegar:
* •  VKORG – Código da Empresa;
* •  VSTEL – Código da Filial;
* •  ROUTE – Itinerário;
* •  INCO1 – Tipo do Frete;
* •  VBELN – Remessa;
* •  ERDAT – Data de Criação da Remessa;
* •  XBLNR – Chave de Referência – Planilha do Romaneio;
* •  VBTYP - Ctg. documento de vendas e distribuição

  SELECT ERDAT VKORG VSTEL VBELN ROUTE INCO1 XBLNR VBTYP
    INTO TABLE IT_REMESSA
    FROM LIKP
   WHERE ERDAT IN P_DTCREA
     AND VKORG IN P_BUKRS
     AND VSTEL IN P_BRANCH
     AND VBELN IN P_REMESS
     AND ROUTE NE 'FOB'
     AND INCO1 NE 'FOB'
     AND VBTYP EQ 'J'.

  CHECK SY-SUBRC IS INITIAL.

  LOOP AT IT_REMESSA ASSIGNING <LIKP>.
    <LIKP>-CH_REFERENCIA = <LIKP>-XBLNR.
  ENDLOOP.

* Selecionar Itens de Transporte
* Tabela:
* •	VTTP
* Onde:
* •	VBELN = <LIKP-VBELN>
* •	TKNUM = FILTRO (Documento de Transporte)
* Pegar:
* •	TKNUM – Nr. Documento de Transporte;
* •	VBELN - Fornecimento;
  IF IT_REMESSA[] IS NOT INITIAL.
    SELECT TKNUM VBELN
      INTO TABLE IT_DOC_TRANSP_ITM
      FROM VTTP
       FOR ALL ENTRIES IN IT_REMESSA
     WHERE TKNUM IN P_TKNUM
       AND VBELN EQ IT_REMESSA-VBELN.
  ENDIF.

  " Retirar Remessa que não são das VT's selecionadas
  IF P_TKNUM IS NOT INITIAL.
    CLEAR: IT_REMESSA_AUX.
    LOOP AT IT_DOC_TRANSP_ITM INTO WA_DOC_TRANSP_ITM.
      LOOP AT IT_REMESSA INTO WA_REMESSA WHERE VBELN EQ WA_DOC_TRANSP_ITM-VBELN.
        APPEND WA_REMESSA TO IT_REMESSA_AUX.
      ENDLOOP.
    ENDLOOP.
    SORT IT_REMESSA_AUX BY VBELN.
    DELETE ADJACENT DUPLICATES FROM IT_REMESSA_AUX COMPARING VBELN.
    CLEAR: IT_REMESSA.
    MOVE IT_REMESSA_AUX TO IT_REMESSA.
    CLEAR: IT_REMESSA_AUX.
  ENDIF.

* Selecionar Itens da Remessa
* Tabela:
* •  LIPS
* Onde:
* •  VBELN = <LIKP-VBELN>
* Pegar:
* •  VBELN - Fornecimento;
* •  FKREL - Relevância para o documento de faturamento;
* •  BWART - Tipo de movimento (administração de estoques);
* •  MATNR - Nº do material;

  SELECT VBELN POSNR FKREL BWART MATNR
    INTO TABLE IT_REMESSA_ITM
    FROM LIPS
     FOR ALL ENTRIES IN IT_REMESSA
   WHERE VBELN EQ IT_REMESSA-VBELN.

  SELECT VBELN LIFNR
    INTO TABLE IT_REMESSA_PARC
    FROM VBPA
     FOR ALL ENTRIES IN IT_REMESSA
   WHERE VBELN EQ IT_REMESSA-VBELN
     AND PARVW EQ 'SP'.

  SORT IT_REMESSA_PARC BY VBELN.

  SELECT *
    INTO TABLE IT_MAKT
    FROM MAKT
     FOR ALL ENTRIES IN IT_REMESSA_ITM
   WHERE SPRAS EQ SY-LANGU
     AND MATNR EQ IT_REMESSA_ITM-MATNR.

  SORT IT_MAKT BY MATNR.

* Selecionar Transporte
* Tabela:
* •	VTTK
* Onde:
* •	TKNUM = <VTTP-TKNUM>
* •	VSART = FILTRO (Modal de Frete)
* Pegar:
* •	TKNUM – Nr. Documento de Transporte;
* •	VSART - Tipo de expedição;

  IF IT_DOC_TRANSP_ITM[] IS NOT INITIAL.
    SELECT TKNUM VSART TDLNR
      INTO TABLE IT_DOC_TRANSP
      FROM VTTK
       FOR ALL ENTRIES IN IT_DOC_TRANSP_ITM
     WHERE TKNUM EQ IT_DOC_TRANSP_ITM-TKNUM
       AND VSART IN P_MODAL.
  ENDIF.

  "Se foi informado o Modal e não encontrou para as remessas não retorna
  CHECK ( P_MODAL IS NOT INITIAL AND SY-SUBRC IS INITIAL ) OR ( P_MODAL IS INITIAL ).

  SORT IT_DOC_TRANSP BY TKNUM.

  IF P_MODAL IS NOT INITIAL AND IT_DOC_TRANSP[] IS NOT INITIAL.
    "Limpar Itens da VT de outro modal
    LOOP AT IT_DOC_TRANSP INTO WA_DOC_TRANSP.
      LOOP AT IT_DOC_TRANSP_ITM INTO WA_DOC_TRANSP_ITM WHERE TKNUM EQ WA_DOC_TRANSP-TKNUM.
        APPEND WA_DOC_TRANSP_ITM TO IT_DOC_TRANSP_ITMB.
      ENDLOOP.
    ENDLOOP.
    CLEAR: IT_DOC_TRANSP_ITM.
    MOVE: IT_DOC_TRANSP_ITMB TO IT_DOC_TRANSP_ITM.
    CLEAR: IT_DOC_TRANSP_ITMB.
  ENDIF.

  "Retirar Remessa que não são do modal escolhido
  IF P_MODAL IS NOT INITIAL.
    CLEAR: IT_REMESSA_AUX, IT_REMESSA_ITM_AUX.
    LOOP AT IT_DOC_TRANSP INTO WA_DOC_TRANSP.
      LOOP AT IT_DOC_TRANSP_ITM INTO WA_DOC_TRANSP_ITM WHERE TKNUM EQ WA_DOC_TRANSP-TKNUM.
        LOOP AT IT_REMESSA INTO WA_REMESSA WHERE VBELN EQ WA_DOC_TRANSP_ITM-VBELN.
          APPEND WA_REMESSA TO IT_REMESSA_AUX.
        ENDLOOP.
        LOOP AT IT_REMESSA_ITM INTO WA_REMESSA_ITM WHERE VBELN EQ WA_DOC_TRANSP_ITM-VBELN.
          APPEND WA_REMESSA_ITM TO IT_REMESSA_ITM_AUX.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    CLEAR: IT_REMESSA, IT_REMESSA_ITM.

    MOVE: IT_REMESSA_AUX     TO IT_REMESSA,
          IT_REMESSA_ITM_AUX TO IT_REMESSA_ITM.

    CLEAR: IT_REMESSA_AUX,
           IT_REMESSA_ITM_AUX.

    SORT IT_REMESSA     BY VBELN.
    SORT IT_REMESSA_ITM BY VBELN POSNR.
    DELETE ADJACENT DUPLICATES FROM IT_REMESSA COMPARING VBELN.
    DELETE ADJACENT DUPLICATES FROM IT_REMESSA_ITM COMPARING VBELN POSNR.

  ENDIF.

* Selecionar Romaneio
* Tabela:
* •	ZSDT0001
* Onde:
* •	CH_REFERENCIA = <LIKP-XBLNR>;
* •	NR_ROMANEIO = FILTRO (Número do Romaneio);
* •	PLACA_CAV = FILTRO (Placa do Cavalo);
* •	NR_TICKET = FILTRO (Número do Ticket);
* Pegar:
* •	NR_ROMANEIO – Número do Romaneio;
* •	PLACA_CAV – Placa do Cavalo;
* •	NR_TICKET – Número do Ticket;
* •	CH_REFERENCIA – Planilha do Romaneio SIGAM

  IF IT_REMESSA[] IS NOT INITIAL.

    SELECT NR_ROMANEIO PLACA_CAV NR_TICKET CH_REFERENCIA DOC_REM
      INTO TABLE IT_ROMANEIO
      FROM ZSDT0001
       FOR ALL ENTRIES IN IT_REMESSA
     WHERE DOC_REM     EQ IT_REMESSA-VBELN
       AND NR_ROMANEIO IN P_ROMANE
       AND PLACA_CAV   IN P_PLACA
       AND NR_TICKET   IN P_TICKET.

    SORT IT_ROMANEIO BY DOC_REM.
  ENDIF.

  CHECK ( ( P_ROMANE IS NOT INITIAL OR P_PLACA IS NOT INITIAL OR P_TICKET IS NOT INITIAL ) AND SY-SUBRC IS INITIAL ) OR ( P_ROMANE IS INITIAL OR P_PLACA IS INITIAL OR P_TICKET IS INITIAL ).

  IF P_ROMANE IS NOT INITIAL OR P_PLACA IS NOT INITIAL OR P_TICKET IS NOT INITIAL.
    CLEAR: IT_REMESSA_AUX[], IT_REMESSA_ITM_AUX[].
    LOOP AT IT_ROMANEIO INTO WA_ROMANEIO.
      LOOP AT IT_REMESSA INTO WA_REMESSA WHERE VBELN EQ WA_ROMANEIO-DOC_REM.
        APPEND WA_REMESSA TO IT_REMESSA_AUX.
        LOOP AT IT_REMESSA_ITM INTO WA_REMESSA_ITM WHERE VBELN EQ WA_REMESSA-VBELN.
          APPEND WA_REMESSA_ITM TO IT_REMESSA_ITM_AUX.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    CLEAR: IT_REMESSA, IT_REMESSA_ITM.
    MOVE: IT_REMESSA_AUX     TO IT_REMESSA,
          IT_REMESSA_ITM_AUX TO IT_REMESSA_ITM.
    CLEAR: IT_REMESSA_AUX, IT_REMESSA_ITM_AUX.
    SORT IT_REMESSA     BY VBELN.
    SORT IT_REMESSA_ITM BY VBELN POSNR.
    DELETE ADJACENT DUPLICATES FROM IT_REMESSA COMPARING VBELN.
    DELETE ADJACENT DUPLICATES FROM IT_REMESSA_ITM COMPARING VBELN POSNR.
  ENDIF.

* Selecionar Documento de Custo
* Tabela:
* •	VFKP
* Onde:
* •	REBEL = <VTTK-TKNUM>
* •	NETWR <> 0
* Pegar:
* •	REBEL - Doc. referenciado;
* •	FKNUM- Nº custos de frete;
* •	EBELN - Nº do documento de compras;
* •	EBELP - Nº item do documento de compra;
* •	LBLNI - Nº folha registro de serviços;
* •	WAERS - Moeda do item dos custos de transporte;
* •	NETWR - Valor líquido em moeda do item custos de frete;

  IF IT_DOC_TRANSP[] IS NOT INITIAL.
    SELECT REBEL FKNUM EBELN EBELP LBLNI WAERS NETWR
      INTO TABLE IT_DOC_CUSTO
      FROM VFKP
       FOR ALL ENTRIES IN IT_DOC_TRANSP
     WHERE REBEL EQ IT_DOC_TRANSP-TKNUM
       AND NETWR NE 0.

    SORT IT_DOC_CUSTO BY REBEL.
  ENDIF.

* Selecionar Item da MIRO
* Tabela:
* •	RSEG
* Onde:
* •	EBELN = <VFKP-EBELN>
* •	EBELP = <VFKP-EBELP>;
* •	LFBNR = <VFKP-LBLNI>;
* Pegar:
* •	EBELN - Nº do documento de compras;
* •	EBELP - Nº item do documento de compra;
* •	LFBNR - Nº documento de um documento de referência;
* •	BELNR - Nº documento de um documento contábil;
* •	GJAHR - Exercício;

  IF IT_DOC_CUSTO[] IS NOT INITIAL.
    SELECT EBELN EBELP LFBNR BELNR GJAHR
      INTO TABLE IT_FATURA_ITM
      FROM RSEG
       FOR ALL ENTRIES IN IT_DOC_CUSTO
     WHERE EBELN EQ IT_DOC_CUSTO-EBELN
       AND EBELP EQ IT_DOC_CUSTO-EBELP
       AND LFBNR EQ IT_DOC_CUSTO-LBLNI.

    SORT IT_FATURA_ITM BY EBELN EBELP LFBNR.
  ENDIF.

* Selecionar Cabeçalho da MIRO
* Tabela:
* •	RBKP
* Onde:
* •	BELNR = <RSEG-BELNR>;
* •	GJAHR = <RSEG-GJAHR>;
* Pegar:
* •	BELNR - Nº documento de um documento contábil;
* •	GJAHR - Exercício;
* •	BUDAT - Data de lançamento no documento;
* •	BLDAT - Data no documento;
* •	XBLNR - Nº documento de referência;
* •	LIFNR - Emissor da fatura distinto;
* •	ZFBDT - Data base para cálculo do vencimento;

  IF IT_FATURA_ITM[] IS NOT INITIAL.

    SELECT BELNR GJAHR BUDAT BLDAT XBLNR LIFNR ZFBDT STBLG STJAH
      INTO TABLE IT_FATURA
      FROM RBKP
       FOR ALL ENTRIES IN IT_FATURA_ITM
     WHERE BELNR EQ IT_FATURA_ITM-BELNR
       AND GJAHR EQ IT_FATURA_ITM-GJAHR
       AND STBLG EQ SPACE.

    LOOP AT IT_FATURA INTO DATA(WA_FATURA) WHERE STBLG IS NOT INITIAL.
      DELETE IT_FATURA_ITM WHERE BELNR = WA_FATURA-BELNR AND GJAHR = WA_FATURA-GJAHR.
    ENDLOOP.

    DELETE IT_FATURA WHERE STBLG IS NOT INITIAL.

    LOOP AT IT_FATURA ASSIGNING <RBKP>.
      <RBKP>-LC_REFTYP = 'LI'.
      CONCATENATE <RBKP>-BELNR <RBKP>-GJAHR INTO <RBKP>-LC_REFKEY.
    ENDLOOP.

    SORT IT_FATURA BY BELNR GJAHR.

    SELECT * INTO TABLE IT_ZLEST0032
      FROM ZLEST0032
       FOR ALL ENTRIES IN IT_FATURA
     WHERE BELNR EQ IT_FATURA-BELNR.

    SORT IT_ZLEST0032 BY BELNR.
  ENDIF.

* Se <LIPS-FKREL> = “A” – Processo BI
*
* Selecionar Fluxo de Documento
* Tabela:
* •  VBFA
* Onde:
* •  VBELV = <LIKP-VBELN>
* •  VBTYP_N =’M’;
* •  VBTYP_V =’J’;
* Pegar:
* •  VBELN - Fatura;
* •  POSNN – Item da Fatura;
* •  VBELV – Fornecimento;
* Variável:
* •  LC_REFTYP = “BI”;
* •  LC_REFKEY = VBELN;
* •  LC_REFITM = POSNN.

  CLEAR: IT_REMESSA_ITM_AUX.
  MOVE IT_REMESSA_ITM TO IT_REMESSA_ITM_AUX.
  DELETE IT_REMESSA_ITM_AUX WHERE FKREL NE 'A'.

  IF IT_REMESSA_ITM_AUX[] IS NOT INITIAL.
    SELECT VBELN POSNN ERDAT VBELV
      INTO TABLE IT_VBFA
      FROM VBFA
       FOR ALL ENTRIES IN IT_REMESSA_ITM_AUX
     WHERE VBELV EQ IT_REMESSA_ITM_AUX-VBELN
       AND VBTYP_N EQ 'M'
       AND VBTYP_V EQ 'J'.

    IF SY-SUBRC IS INITIAL.
      LOOP AT IT_VBFA ASSIGNING <VBFA>.
        <VBFA>-LC_REFTYP = 'BI'.
        <VBFA>-LC_REFKEY = <VBFA>-VBELN.
        <VBFA>-LC_REFITM = <VBFA>-POSNN.
      ENDLOOP.
    ENDIF.
  ENDIF.

*Se <LIPS-FKREL> = “J” – Processo MD
*
*Selecionar Fluxo de Documento
*Tabela:
*•  VBFA
*Onde:
*•  VBELV = <LIKP-VBELN>
*•  VBTYP_N =’R’;
*•  VBTYP_V =’J’;
*Pegar:
*•  VBELN – Documento de Material;
*•  POSNN – Item do Documento de Material;
*•  ERDAT – Data de Criação;
*•  VBELV – Fornecimento;
*Variável:
*•  LC_REFTYP = “MD”
*•  LC_REFKEY = VBELN + ERDAT(4) ;
*•  LC_REFITM = POSNN.

  CLEAR: IT_REMESSA_ITM_AUX.
  MOVE IT_REMESSA_ITM TO IT_REMESSA_ITM_AUX.
  DELETE IT_REMESSA_ITM_AUX WHERE FKREL NE 'J'.

  IF IT_REMESSA_ITM_AUX[] IS NOT INITIAL.
    SELECT VBELN POSNN ERDAT VBELV
      APPENDING TABLE IT_VBFA
      FROM VBFA
       FOR ALL ENTRIES IN IT_REMESSA_ITM_AUX
     WHERE VBELV EQ IT_REMESSA_ITM_AUX-VBELN
       AND VBTYP_N EQ 'R'
       AND VBTYP_V EQ 'J'.

    IF SY-SUBRC IS INITIAL.
      LOOP AT IT_VBFA ASSIGNING <VBFA> WHERE LC_REFTYP IS INITIAL.
        <VBFA>-LC_REFTYP = 'MD'.
        CONCATENATE <VBFA>-VBELN <VBFA>-ERDAT(4) INTO <VBFA>-LC_REFKEY.
        <VBFA>-LC_REFITM = <VBFA>-POSNN.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Selecionar Item de Nota Fiscal
* Tabela:
* •  J_1BNFLIN
* Onde:
* •  REFTYP = <LC_REFTYP>;
* •  REFKEY = <LC_REFKEY>;
* •  REFITM = <LC_REFITM>
* Pegar:
* •  REFTYP - Tipo referência;
* •  REFKEY - Referência ao documento de origem;
* •  REFITM - Item de referência ao documento de origem;
* •  DOCNUM - Nº documento;

  IF IT_VBFA[] IS NOT INITIAL.
    SELECT REFTYP REFKEY REFITM DOCNUM
      INTO TABLE IT_NOTA_FISCAL_ITM
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN IT_VBFA
     WHERE REFTYP EQ IT_VBFA-LC_REFTYP
       AND REFKEY EQ IT_VBFA-LC_REFKEY
       AND REFITM EQ IT_VBFA-LC_REFITM.

    SORT IT_NOTA_FISCAL_ITM BY REFTYP REFKEY REFITM.
  ENDIF.

* Selecionar Nota Fiscal
* Tabela:
* •  J_1BNFLIN
* Onde:
* •  DOCNUM = <J_1BNFLIN-DOCNUM>;
* Pegar:
* •  DOCNUM - Nº documento;
* •  NFENUM - Número de documento de nove posições;
* •  SERIES - Séries;
* •  DOCDAT - Data do documento;
* •  CANCEL - Estornado;

  IF IT_NOTA_FISCAL_ITM[] IS NOT INITIAL.
    SELECT DOCNUM NFENUM SERIES DOCDAT CANCEL
      INTO TABLE IT_NOTA_FISCAL
      FROM J_1BNFDOC
       FOR ALL ENTRIES IN IT_NOTA_FISCAL_ITM
     WHERE DOCNUM EQ IT_NOTA_FISCAL_ITM-DOCNUM.

    SORT IT_NOTA_FISCAL BY DOCNUM.
  ENDIF.

* Selecionar Item do Cocnhecimento
* Tabela:
* •  J_1BNFLIN
* Onde:
* •  REFTYP = <LC_REFTYP - RBKP - LI>;
* •  REFKEY = <LC_REFKEY - RBKP - BELNR + GJAHR >;
* Pegar:
* •  REFTYP - Tipo referência;
* •  REFKEY - Referência ao documento de origem;
* •  REFITM - Item de referência ao documento de origem;
* •  DOCNUM - Nº documento;

  IF IT_FATURA[] IS NOT INITIAL.
    SELECT REFTYP REFKEY REFITM DOCNUM
      INTO TABLE IT_NOTA_FISCALC_ITM
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN IT_FATURA
     WHERE REFTYP EQ IT_FATURA-LC_REFTYP
       AND REFKEY EQ IT_FATURA-LC_REFKEY.

    SORT IT_NOTA_FISCALC_ITM BY REFTYP REFKEY.
  ENDIF.

* Selecionar Nota Fiscal
* Tabela:
* •  J_1BNFLIN
* Onde:
* •  DOCNUM = <J_1BNFLIN-DOCNUM>;
* Pegar:
* •  DOCNUM - Nº documento;
* •  NFENUM - Número de documento de nove posições;
* •  SERIES - Séries;
* •  DOCDAT - Data do documento;
* •  CANCEL - Estornado;

  IF IT_NOTA_FISCALC_ITM[] IS NOT INITIAL.
    SELECT DOCNUM NFENUM SERIES DOCDAT CANCEL
      INTO TABLE IT_NOTA_FISCALC
      FROM J_1BNFDOC
       FOR ALL ENTRIES IN IT_NOTA_FISCALC_ITM
     WHERE DOCNUM EQ IT_NOTA_FISCALC_ITM-DOCNUM.
  ENDIF.

  DELETE IT_ZLEST0032 WHERE DOCNUM EQ SPACE.
  IF IT_ZLEST0032[] IS NOT INITIAL.
    SELECT DOCNUM NFENUM SERIES DOCDAT CANCEL
      APPENDING TABLE IT_NOTA_FISCALC
      FROM J_1BNFDOC
       FOR ALL ENTRIES IN IT_ZLEST0032
     WHERE DOCNUM EQ IT_ZLEST0032-DOCNUM.
  ENDIF.
  SORT IT_NOTA_FISCALC BY DOCNUM.

  IF IT_FATURA[] IS NOT INITIAL.
    SELECT LIFNR NAME1
      INTO TABLE IT_FORNECEDOR
      FROM LFA1
       FOR ALL ENTRIES IN IT_FATURA
     WHERE LIFNR EQ IT_FATURA-LIFNR.
  ENDIF.

  IF IT_DOC_TRANSP[] IS NOT INITIAL.
    SELECT LIFNR NAME1
      APPENDING TABLE IT_FORNECEDOR
      FROM LFA1
       FOR ALL ENTRIES IN IT_DOC_TRANSP
     WHERE LIFNR EQ IT_DOC_TRANSP-TDLNR.
  ENDIF.

  IF IT_REMESSA_PARC[] IS  NOT INITIAL.
    SELECT LIFNR NAME1
      APPENDING TABLE IT_FORNECEDOR
      FROM LFA1
       FOR ALL ENTRIES IN IT_REMESSA_PARC
     WHERE LIFNR EQ IT_REMESSA_PARC-LIFNR.
  ENDIF.

  SORT IT_FORNECEDOR BY LIFNR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .

  DATA: CK_DOC_TRANSPORTE_ITM TYPE C LENGTH 1.

  CLEAR: IT_ALV[].

  LOOP AT IT_REMESSA INTO WA_REMESSA.
    CLEAR: WA_ALV.
    "" DADOS DA REMESSA
    """""""""""""""""""""""""""""""""""""""""""
    WA_ALV-ERDAT         = WA_REMESSA-ERDAT.
    WA_ALV-VKORG         = WA_REMESSA-VKORG.
    WA_ALV-VSTEL         = WA_REMESSA-VSTEL.
    WA_ALV-VBELN         = WA_REMESSA-VBELN.
    WA_ALV-ROUTE         = WA_REMESSA-ROUTE.
    WA_ALV-INCO1         = WA_REMESSA-INCO1.
    WA_ALV-XBLNR         = WA_REMESSA-XBLNR.
    WA_ALV-VBTYP         = WA_REMESSA-VBTYP.
    WA_ALV-CH_REFERENCIA = WA_REMESSA-CH_REFERENCIA.

    READ TABLE IT_REMESSA_PARC INTO WA_REMESSA_PARC WITH KEY VBELN = WA_REMESSA-VBELN BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_FORNECEDOR INTO WA_FORNECEDOR WITH KEY LIFNR = WA_REMESSA_PARC-LIFNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        WA_ALV-LIFNR = WA_FORNECEDOR-LIFNR.
        WA_ALV-NAME1 = WA_FORNECEDOR-NAME1.
      ENDIF.
    ENDIF.

    ""DADOS DO ROMANEIO
    """""""""""""""""""""""""""""""""""""""""""
    READ TABLE IT_ROMANEIO INTO WA_ROMANEIO WITH KEY DOC_REM = WA_REMESSA-VBELN BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      WA_ALV-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
      WA_ALV-NR_ROMANEIO   = WA_ROMANEIO-NR_ROMANEIO.
      WA_ALV-PLACA_CAV     = WA_ROMANEIO-PLACA_CAV.
      WA_ALV-NR_TICKET     = WA_ROMANEIO-NR_TICKET.
    ENDIF.

    """ FLUXO DA FATURAMENTO DA REMESSA """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_REMESSA-VBELN.
      "PEGAR O ULTIMO FATURAMENTO OU DOCUMENTO DE MATERIAL
      IF WA_ALV-VBELN_F IS INITIAL OR WA_VBFA-VBELN GT WA_ALV-VBELN_F.
        WA_ALV-VBELN_F = WA_VBFA-VBELN. "- Fatura;
        WA_ALV-POSNN_F = WA_VBFA-POSNN. "– Item da Fatura;
        WA_ALV-ERDAT_F = WA_VBFA-ERDAT. "- Data da Criação;
        WA_ALV-VBELV   = WA_VBFA-VBELV. "– Fornecimento;
        READ TABLE IT_NOTA_FISCAL_ITM INTO WA_NOTA_FISCAL_ITM WITH KEY REFTYP = WA_VBFA-LC_REFTYP REFKEY = WA_VBFA-LC_REFKEY REFITM = WA_VBFA-LC_REFITM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_ALV-DOCNUM = WA_NOTA_FISCAL_ITM-DOCNUM.
          WA_ALV-REFTYP = WA_NOTA_FISCAL_ITM-REFTYP.
          WA_ALV-REFKEY = WA_NOTA_FISCAL_ITM-REFKEY.
          WA_ALV-REFITM = WA_NOTA_FISCAL_ITM-REFITM.
          READ TABLE IT_NOTA_FISCAL INTO WA_NOTA_FISCAL WITH KEY DOCNUM = WA_NOTA_FISCAL_ITM-DOCNUM BINARY SEARCH.
          WA_ALV-NFENUM = WA_NOTA_FISCAL-NFENUM.
          WA_ALV-SERIES = WA_NOTA_FISCAL-SERIES.
          WA_ALV-DOCDAT = WA_NOTA_FISCAL-DOCDAT.
          WA_ALV-CANCEL = WA_NOTA_FISCAL-CANCEL.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_REMESSA_ITM INTO WA_REMESSA_ITM WHERE VBELN EQ WA_REMESSA-VBELN.

      CLEAR: WA_ALV-MAKTX.

      "" DADOS DO ITEM DA REMESSA
      """""""""""""""""""""""""""""""""""""""""
      WA_ALV-FKREL  = WA_REMESSA_ITM-FKREL.
      WA_ALV-BWART  = WA_REMESSA_ITM-BWART.
      WA_ALV-MATNR  = WA_REMESSA_ITM-MATNR.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_REMESSA_ITM-MATNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        WA_ALV-MAKTX = WA_MAKT-MAKTX.
      ENDIF.

      """ FLUXO DO TRANSPORTE """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CK_DOC_TRANSPORTE_ITM = ABAP_FALSE.
      LOOP AT IT_DOC_TRANSP_ITM INTO WA_DOC_TRANSP_ITM WHERE VBELN EQ WA_REMESSA_ITM-VBELN.
        CLEAR: WA_ALV-TKNUM,
               WA_ALV-VSART,
               WA_ALV-REBEL,
               WA_ALV-FKNUM,
               WA_ALV-EBELN,
               WA_ALV-EBELP,
               WA_ALV-LBLNI,
               WA_ALV-WAERS,
               WA_ALV-NETWR,
               WA_ALV-EBELN,
               WA_ALV-EBELP,
               WA_ALV-LFBNR,
               WA_ALV-BELNR,
               WA_ALV-GJAHR,
               WA_ALV-BUDAT,
               WA_ALV-BLDAT,
               WA_ALV-XBLNR,
               WA_ALV-ZFBDT,
               WA_ALV-DOCNUM_CTE,
               WA_ALV-NFENUM_CTE,
               WA_ALV-SERIES_CTE,
               WA_ALV-DOCDAT_CTE,
               WA_ALV-CANCEL_CTE.

        "" DADOS DO ITEM DO TRANSPORTE
        """""""""""""""""""""""""""""""""""""""
        WA_ALV-TKNUM = WA_DOC_TRANSP_ITM-TKNUM.

        READ TABLE IT_DOC_TRANSP INTO WA_DOC_TRANSP WITH KEY TKNUM = WA_DOC_TRANSP_ITM-TKNUM BINARY SEARCH.
        "" DADOS DO TRANSPORTE
        """""""""""""""""""""""""""""""""""""
        WA_ALV-VSART = WA_DOC_TRANSP-VSART.
        READ TABLE IT_FORNECEDOR INTO WA_FORNECEDOR WITH KEY LIFNR = WA_DOC_TRANSP-TDLNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WA_ALV-LIFNR = WA_FORNECEDOR-LIFNR.
          WA_ALV-NAME1 = WA_FORNECEDOR-NAME1.
        ENDIF.

        READ TABLE IT_DOC_CUSTO INTO WA_DOC_CUSTO WITH KEY REBEL = WA_DOC_TRANSP-TKNUM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          ""DADOS DO DOCUMENTO DE CUSTO -- TOTALIZADO ANTES POR REBEL
          WA_ALV-REBEL = WA_DOC_CUSTO-REBEL.
          WA_ALV-FKNUM = WA_DOC_CUSTO-FKNUM.
          WA_ALV-EBELN = WA_DOC_CUSTO-EBELN.
          WA_ALV-EBELP = WA_DOC_CUSTO-EBELP.
          WA_ALV-LBLNI = WA_DOC_CUSTO-LBLNI.
          WA_ALV-WAERS = WA_DOC_CUSTO-WAERS.
          WA_ALV-NETWR = WA_DOC_CUSTO-NETWR.

          READ TABLE IT_FATURA_ITM INTO WA_FATURA_ITM WITH KEY EBELN = WA_DOC_CUSTO-EBELN EBELP = WA_DOC_CUSTO-EBELP LFBNR = WA_DOC_CUSTO-LBLNI BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            "DADOS DO ITEM DA MIRO
            WA_ALV-EBELN = WA_FATURA_ITM-EBELN.
            WA_ALV-EBELP = WA_FATURA_ITM-EBELP.
            WA_ALV-LFBNR = WA_FATURA_ITM-LFBNR.
            WA_ALV-BELNR = WA_FATURA_ITM-BELNR.
            WA_ALV-GJAHR = WA_FATURA_ITM-GJAHR.

            READ TABLE IT_FATURA INTO WA_FATURA WITH KEY BELNR = WA_FATURA_ITM-BELNR GJAHR = WA_FATURA_ITM-GJAHR BINARY SEARCH.
            "DADOS DA MIRO
            WA_ALV-BUDAT = WA_FATURA-BUDAT.
            WA_ALV-BLDAT = WA_FATURA-BLDAT.
            WA_ALV-XBLNR = WA_FATURA-XBLNR.
            WA_ALV-LIFNR = WA_FATURA-LIFNR.
            WA_ALV-ZFBDT = WA_FATURA-ZFBDT.

            READ TABLE IT_FORNECEDOR INTO WA_FORNECEDOR WITH KEY LIFNR = WA_FATURA-LIFNR BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WA_ALV-NAME1 = WA_FORNECEDOR-NAME1.
            ENDIF.

            READ TABLE IT_NOTA_FISCALC_ITM INTO WA_NOTA_FISCAL_ITM WITH KEY REFTYP = WA_FATURA-LC_REFTYP REFKEY = WA_FATURA-LC_REFKEY BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              READ TABLE IT_NOTA_FISCALC INTO WA_NOTA_FISCAL WITH KEY DOCNUM = WA_NOTA_FISCAL_ITM-DOCNUM BINARY SEARCH.
              WA_ALV-DOCNUM_CTE = WA_NOTA_FISCAL-DOCNUM.
              WA_ALV-NFENUM_CTE = WA_NOTA_FISCAL-NFENUM.
              WA_ALV-SERIES_CTE = WA_NOTA_FISCAL-SERIES.
              WA_ALV-DOCDAT_CTE = WA_NOTA_FISCAL-DOCDAT.
              WA_ALV-CANCEL_CTE = WA_NOTA_FISCAL-CANCEL.
            ELSE.
              READ TABLE IT_ZLEST0032 INTO WA_ZLEST0032 WITH KEY BELNR = WA_ALV-BELNR BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                READ TABLE IT_NOTA_FISCALC INTO WA_NOTA_FISCAL WITH KEY DOCNUM = WA_ZLEST0032-DOCNUM BINARY SEARCH.
                WA_ALV-DOCNUM_CTE = WA_NOTA_FISCAL-DOCNUM.
                WA_ALV-NFENUM_CTE = WA_NOTA_FISCAL-NFENUM.
                WA_ALV-SERIES_CTE = WA_NOTA_FISCAL-SERIES.
                WA_ALV-DOCDAT_CTE = WA_NOTA_FISCAL-DOCDAT.
                WA_ALV-CANCEL_CTE = WA_NOTA_FISCAL-CANCEL.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CK_DOC_TRANSPORTE_ITM = ABAP_TRUE.
        APPEND WA_ALV TO IT_ALV.
      ENDLOOP.

      IF CK_DOC_TRANSPORTE_ITM EQ ABAP_FALSE.
        APPEND WA_ALV TO IT_ALV.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOSTRAR_DADOS .

  CALL SCREEN 0100.

ENDFORM.

INCLUDE ZLESR0099_0100.
