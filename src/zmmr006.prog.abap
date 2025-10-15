************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...:                                                     *
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 19.10.2007                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Relatório de ICMS Diferencial de Alíquota           *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 19.10.2007    Michely              Criação              DEVK902960   *
* 30.10.2007                                                           *
* 28.04.2014    Marcos Faneli                             DEVK937078   *
************************************************************************

REPORT ZMMR006 MESSAGE-ID Z01
               NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
               LINE-SIZE 076               "Comprimento da Linha
               LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
TABLES: J_1BNFDOC,    "Cabecalho da Nota Fiscal
        J_1BNFLIN,    "Partidas Individuais da Nota Fiscal
        J_1BAJ,       "Grupo de impostos
        LFA1,         "Mestre de Fornecedores
        T001,         "Empresas
        J_1BBRANCH,   "Filial
        BSEG,         "Documentos contabeis
        BKPF,         "Documentos contabeis
        T007S.        "Descricao de IVA
*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DEFINE FIELDCAT.
  CLEAR WA_FIELDCAT.
  IF ( &1 EQ 'MARK' ).
    WA_FIELDCAT-CHECKBOX      = C_MARK.
    WA_FIELDCAT-EDIT          = C_MARK.
  ENDIF.
  WA_FIELDCAT-FIELDNAME       = &1.  "Campo da tabela
  WA_FIELDCAT-REF_FIELDNAME   = &2.  "Campo referencia
  WA_FIELDCAT-REF_TABNAME     = &3.  "Tabela
  WA_FIELDCAT-KEY             = &4.  "Campo chave (X)sim ()nao
  WA_FIELDCAT-SELTEXT_L       = &5.  "Descrição
  WA_FIELDCAT-SELTEXT_M       = &6.  "Descrição
  WA_FIELDCAT-SELTEXT_S       = &7.  "Descrição
  WA_FIELDCAT-HOTSPOT         = &8.  "Campo link
*  wa_fieldcat-fix_colum       = &9.  "Congelar coluna
*  wa_fieldcat-just            = &10. "Alinhamento (R)ight (L)eft (C)ent
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
END-OF-DEFINITION.

* Tabelas temporárias utilizadas por Funções para determinação de
* valores de notas fiscais

* Nota Fiscal - Estrutura cabeçalho
DATA: BEGIN OF WK_HEADER.
        INCLUDE STRUCTURE J_1BNFDOC.
DATA: END OF WK_HEADER.

* Nota Fiscal - Estrutura cabeçalho (Segmento adicional)
DATA: BEGIN OF WK_HEADER_ADD.
        INCLUDE STRUCTURE J_1BINDOC.
DATA: END OF WK_HEADER_ADD.

* Nota Fiscal - Parceiro da Nota fiscal (Fornecedor/Cliente)
DATA: BEGIN OF WK_PARTNER OCCURS 0.
        INCLUDE STRUCTURE J_1BNFNAD.
DATA: END OF WK_PARTNER.

* Nota Fiscal - Estrutura de itens
DATA: BEGIN OF WK_ITEM OCCURS 0.
        INCLUDE STRUCTURE J_1BNFLIN.
DATA: END OF WK_ITEM.

* Nota Fiscal - Estrutura de itens (Segmento adicional)
DATA: BEGIN OF WK_ITEM_ADD OCCURS 0.
        INCLUDE STRUCTURE J_1BINLIN.
DATA: END OF WK_ITEM_ADD.

* Nota Fiscal - Estrutura de imposto dos itens
DATA: BEGIN OF WK_ITEM_TAX OCCURS 0.
        INCLUDE STRUCTURE J_1BNFSTX.
DATA: END OF WK_ITEM_TAX.

* Parceiro - Nome e endereço dos Clientes/Fornecedores
DATA: BEGIN OF WK_PARNAD.
        INCLUDE STRUCTURE J_1BINNAD.
DATA: END OF WK_PARNAD.

* Nota Fiscal - Estrutura de msg do cabeçalho da nota
DATA: BEGIN OF WK_HEADER_MSG OCCURS 0.
        INCLUDE STRUCTURE J_1BNFFTX.
DATA: END OF WK_HEADER_MSG.

* Nota Fiscal - Estrutura de referencia de msg
DATA: BEGIN OF WK_REFER_MSG OCCURS 0.
        INCLUDE STRUCTURE J_1BNFREF.
DATA: END OF WK_REFER_MSG.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
*Usado para criação de ALV
TYPE-POOLS: SLIS,
            KKBLO.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT      TYPE SLIS_T_FIELDCAT_ALV, "Estrutura de saida
      IT_HEADER        TYPE KKBLO_T_LISTHEADER WITH HEADER LINE,   "Cabeçalho
      IT_EVENT         TYPE SLIS_T_EVENT       WITH HEADER LINE,   "Eventos
      VG_LISTHEADER    TYPE SLIS_T_LISTHEADER,
      VG_LAYOUT        TYPE SLIS_LAYOUT_ALV,   "Layout do alv
      VG_VARIANT       TYPE DISVARIANT,        "Variantes de saida
      VG_GRID_SETTINGS TYPE LVC_S_GLAY,        "Config p/Grid
      VG_REPID         TYPE SY-REPID,
      ST_HEADER        TYPE KKBLO_LISTHEADER.

DATA: BEGIN OF WA_RELATORIO OCCURS 0,
        BRANCH         LIKE J_1BNFDOC-BRANCH,     "Filial
        PSTDAT         LIKE J_1BNFDOC-PSTDAT,     "Data de Operação
        DOCNUM         LIKE J_1BNFDOC-DOCNUM,     "Nr. Documento
        "NFNUM            LIKE J_1BNFDOC-NFNUM,      "Nr. Nota Fiscal
        NFNUM          LIKE J_1BNFDOC-NFENUM,      "Nr. Nota Fiscal
        BELNR          LIKE BKPF-BELNR,           "Nr. da fatura
        STCD1          TYPE C LENGTH 18,          "CNPJ
        NAME1          LIKE LFA1-NAME1,           "Nome do fornecedor
        REGIO          LIKE LFA1-REGIO,           "Uf
        TEXT1          LIKE T007S-TEXT1,          "Descrição do imposto
        BASE           LIKE J_1BNFSTX-BASE,       "Valor da base
        RATE           LIKE J_1BNFSTX-RATE,       "Taxa de alíquota
        TAXVAL         LIKE J_1BNFSTX-TAXVAL,     "Valor do imposto
        WRBTR          LIKE BSEG-WRBTR,           "Valor de diferencial
        GJAHR          LIKE BKPF-GJAHR,
        MWSKZ          LIKE BSEG-MWSKZ,           "Codigo IVA
        NFTOT          LIKE J_1BNFDOC-NFTOT,      "Valor total da NF
        NFITE          LIKE J_1BNFLIN-NFNET,      "Valor total do Item
        CRENAM         LIKE J_1BNFDOC-CRENAM,
        CFOP           LIKE J_1BNFLIN-CFOP,
        EBELN          LIKE EKKN-EBELN,
        EBELP          LIKE EKKN-EBELP,
        KNTTP          LIKE EKPO-KNTTP,
        SAKTO          LIKE EKKN-SAKTO,
        KOSTL          LIKE EKKN-KOSTL,
        AUFNR          LIKE EKKN-AUFNR,
        VORNR          LIKE AFVC-VORNR,
        MARK,
        CHAVE_NFE(200) TYPE C,
        AUTHCOD        LIKE J_1BNFE_ACTIVE-AUTHCOD,



      END OF WA_RELATORIO.

DATA: BEGIN OF WA_DOC OCCURS 0,
        BRANCH LIKE J_1BNFDOC-BRANCH,     "Filial
        PSTDAT LIKE J_1BNFDOC-PSTDAT,     "Data de Operação
        DOCNUM LIKE J_1BNFDOC-DOCNUM,     "Nr. Documento
        PARID  LIKE J_1BNFDOC-PARID,      "Fornecedor
        PARTYP LIKE J_1BNFDOC-PARTYP,     "Tipo de parceiro
        ITMNUM LIKE J_1BNFLIN-ITMNUM,     "Numero do doc. item
        REFKEY LIKE BKPF-AWKEY,           "Ref. doc de origem
        REFITM LIKE J_1BNFLIN-REFITM,      "Item Ref. doc de origem
        MWSKZ  LIKE J_1BNFLIN-MWSKZ,      "Codigo IVA
        NFNET  LIKE J_1BNFLIN-NFNETT,      "Total do item
        CRENAM LIKE J_1BNFDOC-CRENAM,
        NFE    LIKE J_1BNFDOC-NFE,
        NFENUM LIKE J_1BNFDOC-NFENUM,
        CFOP   LIKE J_1BNFLIN-CFOP,
        BELNR  LIKE RSEG-BELNR,
        GJAHR  LIKE RSEG-GJAHR,
        BUZEI  LIKE RSEG-BUZEI,
        EBELN  LIKE EKPO-EBELN,
        EBELP  LIKE EKPO-EBELP,
      END OF WA_DOC.

DATA: BEGIN OF WA_FOR OCCURS 0,
        LIFNR LIKE LFA1-LIFNR,           "Fornecedor ID
        STCD1 LIKE LFA1-STCD1,           "CNPJ
        STCD2 LIKE LFA1-STCD2,           "CPF
        STKZN LIKE LFA1-STKZN,           "Pessoa fisica (X)
        NAME1 LIKE LFA1-NAME1,           "Nome
        REGIO LIKE LFA1-REGIO,           "Uf
      END OF WA_FOR.

DATA: BEGIN OF WA_IMP OCCURS 0,
        DOCNUM LIKE J_1BNFSTX-DOCNUM,     "Numero do documento
        ITMNUM LIKE J_1BNFSTX-ITMNUM,     "Numedo do doc Item
        BASE   LIKE J_1BNFSTX-BASE,       "Valor da base
        RATE   LIKE J_1BNFSTX-RATE,       "Valor da aliquota
        TAXVAL LIKE J_1BNFSTX-TAXVAL,     "Valor do imposto
        TAXTYP LIKE J_1BNFSTX-TAXTYP,        "Tipo do imposto
        OTHBAS LIKE J_1BNFSTX-OTHBAS,     "Outras bases
        EXCBAS LIKE J_1BNFSTX-EXCBAS,
        TAXGRP LIKE J_1BAJ-TAXGRP,
      END OF WA_IMP.
*
*DATA: BEGIN OF WA_BKPF OCCURS 0,
*        BUKRS LIKE BKPF-BUKRS,           "Empresa
*        BELNR LIKE BKPF-BELNR,           "Nº do doc. contabil
*        GJAHR LIKE BKPF-GJAHR,           "Exercicio
*        AWKEY LIKE BKPF-AWKEY,           "Chave de referencia
*      END OF WA_BKPF.
*
*DATA: BEGIN OF WA_BSEG OCCURS 0,
*        BUKRS LIKE BSEG-BUKRS,           "Empresa
*        BELNR LIKE BSEG-BELNR,           "Nº do doc. contabil
*        GJAHR LIKE BSEG-GJAHR,           "Exercicio
*        WRBTR LIKE BSEG-WRBTR,           "Valor dif. aliquota
*        MWSKZ LIKE BSEG-MWSKZ,           "Cod. imposto
*        SHKZG LIKE BSEG-SHKZG,
*        HKONT LIKE BSEG-HKONT,
*        KOSTL LIKE BSEG-KOSTL,
*        EBELN LIKE BSEG-EBELN,
*        EBELP LIKE BSEG-EBELP,
*        AUFNR LIKE BSEG-AUFNR,
*        AUFPL LIKE BSEG-AUFPL,
*        APLZL LIKE BSEG-APLZL,
*      END OF WA_BSEG.

DATA: BEGIN OF WA_RSEG OCCURS 0,
        BELNR LIKE RSEG-BELNR,
        GJAHR LIKE RSEG-GJAHR,
        BUZEI LIKE RSEG-BUZEI,
        EBELN LIKE RSEG-EBELN,
        EBELP LIKE RSEG-EBELP,
      END OF WA_RSEG.

DATA: BEGIN OF WA_EKPO OCCURS 0,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        KNTTP LIKE EKPO-KNTTP,
        MENGE LIKE EKPO-MENGE,
      END OF WA_EKPO.


DATA: BEGIN OF WA_EKKN OCCURS 0,
        EBELN     LIKE EKKN-EBELN,
        EBELP     LIKE EKKN-EBELP,
        SAKTO     LIKE EKKN-SAKTO,
        KOSTL     LIKE EKKN-KOSTL,
        AUFNR     LIKE EKKN-AUFNR,
        AUFPL_ORD LIKE EKKN-AUFPL_ORD,
        APLZL_ORD LIKE EKKN-APLZL_ORD,
        MENGE     LIKE EKKN-MENGE,

      END OF WA_EKKN.

DATA: BEGIN OF WA_ACTIVE,
        DOCNUM  LIKE J_1BNFE_ACTIVE-DOCNUM,
        AUTHCOD LIKE J_1BNFE_ACTIVE-AUTHCOD,
        REGIO   LIKE J_1BNFE_ACTIVE-REGIO,
        NFYEAR  LIKE J_1BNFE_ACTIVE-NFYEAR,
        NFMONTH LIKE J_1BNFE_ACTIVE-NFMONTH,
        STCD1   LIKE J_1BNFE_ACTIVE-STCD1,
        MODEL   LIKE J_1BNFE_ACTIVE-MODEL,
        SERIE   LIKE J_1BNFE_ACTIVE-SERIE,
        NFNUM9  LIKE J_1BNFE_ACTIVE-NFNUM9,
        DOCNUM9 LIKE J_1BNFE_ACTIVE-DOCNUM9,
        CDV     LIKE J_1BNFE_ACTIVE-CDV,
      END OF WA_ACTIVE.

DATA: IT_RELATORIO LIKE STANDARD TABLE OF WA_RELATORIO,
      IT_DOC       LIKE STANDARD TABLE OF WA_DOC,
      IT_FOR       LIKE STANDARD TABLE OF WA_FOR,
      IT_IMP       LIKE STANDARD TABLE OF WA_IMP,
*      IT_BKPF      LIKE STANDARD TABLE OF WA_BKPF,
*      IT_BSEG      LIKE STANDARD TABLE OF WA_BSEG,
*      IT_BSEG_TOT  LIKE STANDARD TABLE OF WA_BSEG,
      IT_RSEG      LIKE STANDARD TABLE OF WA_RSEG,
      IT_EKPO      LIKE STANDARD TABLE OF WA_EKPO,
      IT_EKKN      LIKE STANDARD TABLE OF WA_EKKN,
      IT_ACTIVE    LIKE STANDARD TABLE OF WA_ACTIVE.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS: S_BUKRS    LIKE J_1BNFDOC-BUKRS OBLIGATORY. "Empresa
"s_gjahr    like bseg-gjahr obligatory.      "Exercicio
SELECT-OPTIONS:
            S_BRANCH   FOR J_1BNFDOC-BRANCH OBLIGATORY, "Filial
            S_PSTDAT   FOR SY-DATUM         OBLIGATORY, "Periodo
            S_MWSKZ    FOR BSEG-MWSKZ,
            S_MATNR    FOR J_1BNFLIN-MATNR,             "Produto
            S_PARID    FOR J_1BNFDOC-PARID,             "Fornecedor/Cliente
            S_DOCNUM   FOR J_1BNFDOC-DOCNUM.            "N° Nota
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Field Symbols                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Definição de Range                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Ponteiro de Objeto                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Classes Locais                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Containers                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'INI'.
*----------------------------------------------------------------------*
* Definição Macros                                                     *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Selecionar os dados
  PERFORM F_SELECIONA_DADOS.
* Montar dados selecionados para impressão
  PERFORM F_MONTA_DADOS.
* Montar o cabeçalho da alv
  PERFORM F_MONTA_CABECALHO.
* Montar estrutura de dados do alv
  PERFORM F_MONTA_ESTRUTURA.
* Executar o alv para Background e foreground
  PERFORM F_EXECUTA_ALV.

END-OF-SELECTION.

*----------------------------------------------------------------------*
* Top-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* End-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At User-command                                                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At Line-selection                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Início das Sub-Rotinas                                               *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Selecionar os dados para o relatorio conforme paramentros de
*       seleção.
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  DATA TABIX TYPE SY-TABIX.
  CLEAR: IT_DOC[], IT_FOR[], IT_IMP[].
* Montar tabela interna de documentos com os dados conforme paramentros
  SELECT NF~BRANCH NF~PSTDAT NF~DOCNUM NF~PARID NF~PARTYP
         IT~ITMNUM IT~REFKEY IT~REFITM IT~MWSKZ  IT~NETWR NF~CRENAM NF~NFE NF~NFENUM IT~CFOP
    FROM J_1BNFLIN     AS IT
  INNER JOIN J_1BNFDOC AS NF
     ON NF~DOCNUM EQ IT~DOCNUM
*    AND NF~MANDT  EQ IT~MANDT
  INTO TABLE IT_DOC
  WHERE ( NF~BUKRS  EQ S_BUKRS  )
    AND ( NF~BRANCH IN S_BRANCH )
    AND ( NF~PSTDAT IN S_PSTDAT )
    AND ( IT~MATNR  IN S_MATNR  )
    AND ( NF~PARID  IN S_PARID  )
    AND ( NF~DOCNUM IN S_DOCNUM )
    AND ( IT~MWSKZ  IN S_MWSKZ  )
    AND ( NF~CANCEL NE 'X' )
    AND ( NF~DOCREF EQ 0 ).

  CHECK IT_DOC[] IS NOT INITIAL.

  " Montar Tabela para pegar dados da NFE Electronic Nota Fiscal: Actual
  SELECT DOCNUM AUTHCOD REGIO NFYEAR NFMONTH STCD1 MODEL SERIE NFNUM9 DOCNUM9 CDV
    FROM J_1BNFE_ACTIVE
    INTO TABLE IT_ACTIVE
    FOR ALL ENTRIES IN IT_DOC
   WHERE DOCNUM EQ IT_DOC-DOCNUM.

* Montar tabela interna de fornecedores conforme dados do doc. fiscal
*  select lifnr stcd1 stcd2 stkzn name1 regio
*    from lfa1
*  into table it_for
*  for all entries in it_doc
*  where lifnr eq it_doc-parid.

* Monta tabela interna de impostos conforme dados do doc. fiscal
  SELECT IM~DOCNUM IM~ITMNUM IM~BASE IM~RATE IM~TAXVAL IM~TAXTYP IM~OTHBAS IM~EXCBAS GR~TAXGRP
    FROM J_1BNFSTX  AS IM
  INNER JOIN J_1BAJ AS GR
*     ON GR~MANDT  EQ IM~MANDT
    ON GR~TAXTYP EQ IM~TAXTYP
  INTO TABLE IT_IMP
  FOR ALL ENTRIES IN IT_DOC
  WHERE IM~DOCNUM EQ IT_DOC-DOCNUM
    AND IM~ITMNUM EQ IT_DOC-ITMNUM
    AND ( ( GR~TAXGRP EQ 'ICMS' ) OR ( GR~TAXGRP EQ 'ICOP' ) ).

** Monta tabela de documentos contabeis ref. parametros de seleção e
** documentos fiscais selecionados acima.
*  SELECT BUKRS BELNR GJAHR AWKEY
*    FROM BKPF
*  INTO TABLE IT_BKPF
*  FOR ALL ENTRIES IN IT_DOC
*  WHERE AWKEY EQ IT_DOC-REFKEY
*    AND BUKRS EQ S_BUKRS.
*
** Monta tabela de documentos contabeis ref. parametros de seleção e
** documentos fiscais selecionados acima.
*  SELECT BUKRS BELNR GJAHR WRBTR MWSKZ HKONT KOSTL EBELN EBELP AUFNR AUFPL APLZL
*    FROM BSEG
*  INTO TABLE IT_BSEG
*  FOR ALL ENTRIES IN IT_BKPF
*  WHERE BUKRS EQ IT_BKPF-BUKRS
*    AND BELNR EQ IT_BKPF-BELNR
*    AND GJAHR EQ IT_BKPF-GJAHR
*    AND WRBTR GT 0
*    AND KOSTL NE ''.
*
*  CHECK IT_BSEG[] IS NOT INITIAL.
*
*  LOOP AT IT_BSEG INTO WA_BSEG.
*    COLLECT WA_BSEG INTO IT_BSEG_TOT.
*  ENDLOOP.

  LOOP AT IT_DOC INTO WA_DOC.
    WA_DOC-BELNR = WA_DOC-REFKEY(10).
    WA_DOC-GJAHR = WA_DOC-REFKEY+10(4).
    WA_DOC-BUZEI = WA_DOC-REFITM.
    MODIFY IT_DOC FROM WA_DOC INDEX SY-TABIX TRANSPORTING BELNR GJAHR BUZEI.
  ENDLOOP.

  SELECT BELNR GJAHR BUZEI EBELN EBELP
    FROM RSEG
    INTO TABLE IT_RSEG
    FOR ALL ENTRIES IN IT_DOC
    WHERE BELNR = IT_DOC-BELNR
    AND   GJAHR = IT_DOC-GJAHR.

  CHECK IT_RSEG[] IS NOT INITIAL.

  SORT IT_RSEG BY BELNR GJAHR BUZEI.
  LOOP AT IT_DOC INTO WA_DOC.
    TABIX = SY-TABIX.
    READ TABLE IT_RSEG INTO WA_RSEG WITH KEY BELNR = WA_DOC-BELNR
                                             GJAHR = WA_DOC-GJAHR
                                             BUZEI = WA_DOC-BUZEI BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_DOC-EBELN = WA_RSEG-EBELN.
      WA_DOC-EBELP = WA_RSEG-EBELP.
      MODIFY IT_DOC FROM WA_DOC INDEX TABIX TRANSPORTING EBELN EBELP.
    ENDIF.
  ENDLOOP.

  SELECT EBELN EBELP KNTTP
    FROM  EKPO
    INTO TABLE IT_EKPO
    FOR ALL ENTRIES IN IT_DOC
    WHERE EBELN EQ IT_DOC-EBELN
    AND   EBELP EQ IT_DOC-EBELP.

  CHECK IT_EKPO[] IS NOT INITIAL.

  SELECT EBELN
         EBELP
         SAKTO
         KOSTL
         AUFNR
         AUFPL_ORD
         APLZL_ORD
         MENGE
    FROM EKKN
    INTO TABLE IT_EKKN
    FOR ALL ENTRIES IN IT_EKPO
    WHERE EBELN = IT_EKPO-EBELN
    AND   EBELP = IT_EKPO-EBELP.

ENDFORM.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  f_monta_dados
*&---------------------------------------------------------------------*
*       Montar dados selecionados para impressão
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS .
  CLEAR: IT_RELATORIO[].
  IF NOT IT_DOC[] IS INITIAL.
    SORT: IT_DOC    BY DOCNUM PARID REFKEY,
          IT_FOR    BY LIFNR,
          IT_IMP    BY DOCNUM ITMNUM TAXTYP,
          IT_ACTIVE BY DOCNUM,
          IT_EKPO   BY EBELN EBELP,
          IT_EKKN   BY EBELN EBELP.


    "it_bseg by bukrs belnr gjahr.

*    clear wa_doc .
    LOOP AT IT_DOC INTO WA_DOC.
      CLEAR: WA_FOR, WA_IMP.
      WA_RELATORIO-BRANCH = WA_DOC-BRANCH.
      WA_RELATORIO-PSTDAT = WA_DOC-PSTDAT.      "Data de Operação
      WA_RELATORIO-DOCNUM = WA_DOC-DOCNUM.      "Nr. Documento

      CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
        EXPORTING
          DOC_NUMBER         = WA_DOC-DOCNUM
        IMPORTING
          DOC_HEADER         = WK_HEADER
        TABLES
          DOC_PARTNER        = WK_PARTNER
          DOC_ITEM           = WK_ITEM
          DOC_ITEM_TAX       = WK_ITEM_TAX
          DOC_HEADER_MSG     = WK_HEADER_MSG
          DOC_REFER_MSG      = WK_REFER_MSG
        EXCEPTIONS
          DOCUMENT_NOT_FOUND = 1
          DOCUM_LOCK         = 2
          OTHERS             = 3.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
        EXPORTING
          NF_HEADER   = WK_HEADER
        IMPORTING
          EXT_HEADER  = WK_HEADER_ADD
        TABLES
          NF_ITEM     = WK_ITEM
          NF_ITEM_TAX = WK_ITEM_TAX
          EXT_ITEM    = WK_ITEM_ADD.



      IF ( WA_DOC-NFE EQ 'X' ).
        WA_RELATORIO-NFNUM = WA_DOC-NFENUM.
      ELSE.
        WA_RELATORIO-NFNUM = WK_HEADER-NFNUM.
      ENDIF.

      WA_RELATORIO-CFOP = WA_DOC-CFOP.

      READ TABLE IT_ACTIVE INTO WA_ACTIVE WITH KEY DOCNUM = WA_DOC-DOCNUM BINARY SEARCH.
      WA_RELATORIO-AUTHCOD = WA_ACTIVE-AUTHCOD.

      "CONCATENATE WA_ACTIVE-DOCNUM
      CONCATENATE
            WA_ACTIVE-REGIO
            WA_ACTIVE-NFYEAR
            WA_ACTIVE-NFMONTH
            WA_ACTIVE-STCD1
            WA_ACTIVE-MODEL
            WA_ACTIVE-SERIE
            WA_ACTIVE-NFNUM9
            WA_ACTIVE-DOCNUM9
            WA_ACTIVE-CDV
        INTO WA_RELATORIO-CHAVE_NFE.

      WA_RELATORIO-CRENAM = WA_DOC-CRENAM.
      CLEAR WA_FOR.
      IF WA_DOC-PARTYP EQ 'C'.
        SELECT SINGLE KUNNR STCD1 STCD2 STKZN NAME1
          FROM KNA1
          INTO WA_FOR
         WHERE KUNNR EQ WA_DOC-PARID.
      ENDIF.
      IF WA_DOC-PARTYP EQ 'V'.
        SELECT SINGLE LIFNR STCD1 STCD2 STKZN NAME1
          FROM LFA1
          INTO WA_FOR
         WHERE LIFNR EQ WA_DOC-PARID.
      ENDIF.

      IF WA_FOR-STKZN EQ SPACE.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            INPUT  = WA_FOR-STCD1
          IMPORTING
            OUTPUT = WA_RELATORIO-STCD1.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            INPUT  = WA_FOR-STCD2
          IMPORTING
            OUTPUT = WA_RELATORIO-STCD1.
        WA_RELATORIO-STCD1 = WA_RELATORIO-STCD1(14).
      ENDIF.
      WA_RELATORIO-NAME1 = WA_FOR-NAME1.        "Nome do fornecedor
      WA_RELATORIO-REGIO = WA_FOR-REGIO.        "Uf
      WA_RELATORIO-NFTOT  = WK_HEADER_ADD-NFTOT.        "Valor Nota fiscal
      CLEAR WA_IMP.
      READ TABLE IT_IMP INTO WA_IMP WITH KEY DOCNUM = WA_DOC-DOCNUM
                                             ITMNUM = WA_DOC-ITMNUM
                                             TAXGRP = 'ICMS'
*                                             TAXTYP = 'ICMS'
                                             BINARY SEARCH.
      WA_RELATORIO-MWSKZ  = WA_DOC-MWSKZ.               "Codigo IVA
      IF WA_IMP-BASE NE 0.
        WA_RELATORIO-BASE   = WA_IMP-BASE.           "Base de imposto
      ELSEIF WA_IMP-OTHBAS NE 0.
        WA_RELATORIO-BASE = WA_IMP-OTHBAS.
      ELSE.
        WA_RELATORIO-BASE = WA_IMP-EXCBAS.
      ENDIF.
      WA_RELATORIO-RATE   = WA_IMP-RATE.          "Aliquota de ICMS
      WA_RELATORIO-TAXVAL = WA_IMP-TAXVAL.        "Valor do imposto

      CLEAR WA_IMP.
      READ TABLE IT_IMP INTO WA_IMP WITH KEY DOCNUM = WA_DOC-DOCNUM
                                             ITMNUM = WA_DOC-ITMNUM
                                             TAXGRP = 'ICOP'
*                                             TAXTYP = 'ICOP'
                                             BINARY SEARCH.

      IF WA_IMP-TAXVAL < 0.
        WA_RELATORIO-WRBTR = WA_IMP-TAXVAL * -1.
      ELSE.
        WA_RELATORIO-WRBTR = WA_IMP-TAXVAL.
      ENDIF.
      IF WA_RELATORIO-BASE EQ 0.
        WA_RELATORIO-BASE = WA_IMP-BASE.
      ENDIF.
      WA_RELATORIO-NFITE = WA_DOC-NFNET.
*      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY AWKEY = WA_DOC-REFKEY
*                                               BINARY SEARCH.
*
*      read table it_bseg into wa_bseg with key bukrs = wa_bkpf-bukrs
*                                               belnr = wa_bkpf-belnr
*                                               gjahr = wa_bkpf-gjahr
**                                               buzei = wk_item-refitm+3(3)
*                                               binary search.
*      wa_relatorio-wrbtr = wa_bseg-wrbtr.
      WA_RELATORIO-BELNR = WA_DOC-REFKEY(10).
      WA_RELATORIO-GJAHR = WK_HEADER-GJAHR.
      SELECT SINGLE TEXT1
        FROM T007S
        INTO WA_RELATORIO-TEXT1
       WHERE SPRAS EQ 'P'
         AND KALSM EQ 'TAXBRA'
         AND MWSKZ EQ WK_ITEM-MWSKZ.

      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_DOC-EBELN
                                               EBELP = WA_DOC-EBELP BINARY SEARCH.

      IF SY-SUBRC = 0.
        WA_RELATORIO-EBELN = WA_EKPO-EBELN.
        WA_RELATORIO-EBELP = WA_EKPO-EBELP.
        WA_RELATORIO-KNTTP = WA_EKPO-KNTTP.
        LOOP AT IT_EKKN INTO WA_EKKN WHERE EBELN = WA_DOC-EBELN
                                     AND   EBELP = WA_DOC-EBELP.
          WA_RELATORIO-SAKTO = WA_EKKN-SAKTO.
          WA_RELATORIO-KOSTL = WA_EKKN-KOSTL.
          WA_RELATORIO-AUFNR = WA_EKKN-AUFNR.
          IF WA_EKKN-AUFPL_ORD IS NOT INITIAL.
            SELECT  SINGLE VORNR
              FROM AFVC
              INTO WA_RELATORIO-VORNR
              WHERE AUFPL = WA_EKKN-AUFPL_ORD
              AND   APLZL = WA_EKKN-APLZL_ORD.
          ENDIF.

        ENDLOOP.
      ENDIF.


      IF ( WA_RELATORIO-WRBTR NE 0 ).
        APPEND WA_RELATORIO TO IT_RELATORIO.
      ENDIF.
      CLEAR WA_RELATORIO.
    ENDLOOP.
  ELSE.
    MESSAGE I000 WITH 'Não existe informação para esta seleção'.
    STOP.
  ENDIF.
ENDFORM.                    " f_monta_dados
*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXECUTA_ALV .
* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = VL_REPID
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = VG_LAYOUT
*     i_background_id         = c_enjoy
      IT_FIELDCAT             = IT_FIELDCAT[]
      I_DEFAULT               = 'A'
      I_SAVE                  = 'X'
      IT_EVENTS               = IT_EVENT[]
    TABLES
      T_OUTTAB                = IT_RELATORIO
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " f_executa_alv
*&---------------------------------------------------------------------*
*&      Form  f_monta_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTA_CABECALHO .
* Colocando os dados para exibição do cabecalho
* Título do relatório
  DATA: VL_BUTXT       LIKE T001-BUTXT,       "Nome da empresa
        VL_FILIAL1     LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL2     LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL(100) TYPE C,
        VL_DATA1(10)   TYPE C,
        VL_DATA2(10)   TYPE C,
        VL_DATA(25)    TYPE C.

  CLEAR IT_HEADER.

  IT_HEADER-TYP  = 'H'.
  IT_HEADER-INFO = 'ICMS Diferencial de alíquota'.
  APPEND  IT_HEADER.


  SELECT SINGLE BUTXT
    FROM T001
    INTO VL_BUTXT
   WHERE BUKRS = S_BUKRS.
  CONCATENATE S_BUKRS
              VL_BUTXT INTO VL_BUTXT
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Empresa'.
  IT_HEADER-INFO = VL_BUTXT.
  APPEND  IT_HEADER.

  SELECT SINGLE NAME
    FROM J_1BBRANCH
    INTO VL_FILIAL1
   WHERE BUKRS  EQ S_BUKRS
     AND BRANCH EQ S_BRANCH-LOW.
  CONCATENATE S_BRANCH-LOW
              VL_FILIAL1 INTO VL_FILIAL1
              SEPARATED BY SPACE.
  IF NOT S_BRANCH-HIGH IS INITIAL.
    SELECT SINGLE NAME
      FROM J_1BBRANCH
      INTO VL_FILIAL2
     WHERE BUKRS  EQ S_BUKRS
       AND BRANCH EQ S_BRANCH-HIGH.
    CONCATENATE S_BRANCH-HIGH
                VL_FILIAL2 INTO VL_FILIAL2
                SEPARATED BY SPACE.
  ENDIF.
  CONCATENATE VL_FILIAL1
              VL_FILIAL2 INTO VL_FILIAL
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Filial'.
  IT_HEADER-INFO = VL_FILIAL.
  APPEND  IT_HEADER.

  CONCATENATE S_PSTDAT-LOW+6(2) '.'
              S_PSTDAT-LOW+4(2) '.'
              S_PSTDAT-LOW(4)
              INTO VL_DATA1.

  CONCATENATE S_PSTDAT-HIGH+6(2) '.'
              S_PSTDAT-HIGH+4(2) '.'
              S_PSTDAT-HIGH(4)
              INTO VL_DATA2.

  CONCATENATE VL_DATA1
              'a'
              VL_DATA2 INTO VL_DATA
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Periodo'.
  IT_HEADER-INFO = VL_DATA.
  APPEND  IT_HEADER.
ENDFORM.                    " f_monta_cabecalho
*&---------------------------------------------------------------------*
*&      Form  f_monta_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTA_ESTRUTURA .
  PERFORM F_FIELDCAT USING:
        '0' 'X' 'IT_RELATORIO' 'BRANCH' 'Filial'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '1' 'X' 'IT_RELATORIO' 'PSTDAT' 'Data Oper.'
        10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '2' 'X' 'IT_RELATORIO' 'BELNR'  'Nr Doc.Ctb.'
        10  'X'  ''             '' 'X'
  CHANGING IT_FIELDCAT,
        '3' 'X' 'IT_RELATORIO' 'DOCNUM' 'Nr Docum.'
        06  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '4' 'X' 'IT_RELATORIO' 'NFNUM'  'N.Fiscal'
        06  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '5' 'X' 'IT_RELATORIO' 'CFOP'  'CFOP'
        06  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '6' '' 'IT_RELATORIO'  'NFTOT'   'Valor N.Fiscal'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '7' '' 'IT_RELATORIO'  'NFITE'   'Valor Item'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '8' '' 'IT_RELATORIO'  'MWSKZ'  'IVA'
        03  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '9' '' 'IT_RELATORIO'  'CRENAM'  'Criado por.'
        12  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '10'  ''  'IT_RELATORIO' 'STCD1'  'CNPJ/CPF'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '11' ''  'IT_RELATORIO' 'NAME1'  'Nome do Fornecedor'
        30  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '12' ''  'IT_RELATORIO' 'BASE'  'Base de Calc.'
        14  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '13' ''  'IT_RELATORIO' 'RATE'  '%Icms'
        08  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '14' ''  'IT_RELATORIO' 'TAXVAL' 'Valor ICMS Nf.'
        14  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '15' ''  'IT_RELATORIO' 'WRBTR' 'Dif.Aliquota'
        14  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '16' ''  'IT_RELATORIO' 'TEXT1' 'Imposto'
       20  ''  ''             '' ''
 "ALRS
  CHANGING IT_FIELDCAT,
       '17' ''  'IT_RELATORIO' 'EBELN' 'Pedido'
       10  ''  ''             '' 'X'
  CHANGING IT_FIELDCAT,
       '18' ''  'IT_RELATORIO' 'EBELP' 'Item'
       07  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '19' ''  'IT_RELATORIO' 'KNTTP' 'Ctg.ctb'
       05  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '20' ''  'IT_RELATORIO' 'SAKTO' 'Conta'
       10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '21' ''  'IT_RELATORIO' 'KOSTL' 'C.Custo'
       10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '22' ''  'IT_RELATORIO' 'AUFNR' 'Ordem'
       10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '23' ''  'IT_RELATORIO' 'VORNR' 'Operação'
       07  ''  ''             '' ''
  "ALRS
  CHANGING IT_FIELDCAT,
       '24' ''  'IT_RELATORIO' 'CHAVE_NFE' 'Chave da NFE.'
        22  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '25' ''  'IT_RELATORIO' 'AUTHCOD' 'Protocolo'
        23  ''  ''             '' ''
  CHANGING IT_FIELDCAT.

ENDFORM.                    " f_monta_estrutura
**&---------------------------------------------------------------------
*                                                                      *
*&      Form  f_fieldcat                                               *
*&---------------------------------------------------------------------*
* Preenche a tabela fieldcat                                           *
*----------------------------------------------------------------------*
* p_cont   -> Posição do campo                                         *
* p_key    -> campo chave                                              *
* p_tab    -> tabela interna                                           *
* p_field  -> campo da tabela interna                                  *
* p_desc   -> Descrição do campo                                       *
* p_tam    -> Tamanho do campo de saída                                *
* p_qtde   -> É um campo de to tipo QUAN                               *
* p_fix    -> Congelar a coluna                                        *
* p_just-> -> Alinhamento (R)ight (L)eft (C)ent                        *
*----------------------------------------------------------------------*
FORM F_FIELDCAT USING P_CONT P_KEY  P_TAB  P_FIELD P_DESC
      P_TAM  P_QTDE P_FIX  P_JUST P_HOT
CHANGING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

* Tabela interna local
  DATA: TL_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  TL_FIELDCAT-COL_POS    = P_CONT.
  TL_FIELDCAT-KEY        = P_KEY.
  TL_FIELDCAT-TABNAME    = P_TAB.
  TL_FIELDCAT-FIELDNAME  = P_FIELD.
  TL_FIELDCAT-SELTEXT_L  = P_DESC.
  TL_FIELDCAT-SELTEXT_M  = P_DESC.
  TL_FIELDCAT-SELTEXT_S  = P_DESC.
  TL_FIELDCAT-OUTPUTLEN  = P_TAM.
  TL_FIELDCAT-QUANTITY   = P_QTDE.
  TL_FIELDCAT-FIX_COLUMN = P_FIX.
  TL_FIELDCAT-JUST       = P_JUST.
  TL_FIELDCAT-HOTSPOT    = P_HOT.
  APPEND TL_FIELDCAT TO P_FIELDCAT.

ENDFORM.                    " f_fieldcatJ1BNFDOC
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING UCOMM LIKE SY-UCOMM
      SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_RELATORIO INTO WA_RELATORIO INDEX SELFIELD-TABINDEX.
  IF SELFIELD-FIELDNAME = 'BELNR'.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'MIR4'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'MIR4'.
    ENDIF.
    SET PARAMETER ID:  'RBN' FIELD WA_RELATORIO-BELNR,
                       'GJR' FIELD WA_RELATORIO-GJAHR,
                       'NCH' FIELD 'X'.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ELSEIF SELFIELD-FIELDNAME = 'EBELN'.
    SET PARAMETER ID:  'BES' FIELD WA_RELATORIO-EBELN.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*     Form Para Fazer o cabeçalho   no ALV                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*     Form Para Fazer o cabeçalho   no ALV                             *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = c_logo
      IT_LIST_COMMENTARY = IT_HEADER[].

ENDFORM.                    "top_of_page
