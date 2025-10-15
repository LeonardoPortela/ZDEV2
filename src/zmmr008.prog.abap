************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 07.04.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Relatório fisico e fisico financeiro de pedidos     *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 07.04.2008    Michely              Criação              DEVK903849   *
* 02.10.2008    Marcus Barbara       Criação              DEVK904994   *
* 09.10.2008    Marcus Barbara       Criação              DEVK905032   *
* 09.10.2008    Marcus Barbara       Criação              DEVK905036   *
* 09.10.2008    Marcus Barbara       Criação              DEVK905038   *
* 09.10.2008    Marcus Barbara       Criação              DEVK905050   *
*                                                                      *
************************************************************************

REPORT ZMMR008 NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
               MESSAGE-ID Z01
               LINE-SIZE 076               "Comprimento da Linha
               LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
TABLES: EKKO, BKPF, LFA1, EKPO, EKBE.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS,
            KKBLO.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV,                   "Estrutura de saida
      IT_EVENT           TYPE SLIS_T_EVENT       WITH HEADER LINE,   "Eventos
      IT_HEADER          TYPE KKBLO_T_LISTHEADER WITH HEADER LINE,   "Cabeçalho
      VG_LAYOUT          TYPE SLIS_LAYOUT_ALV.   "Layout do alv
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF WA_LFA1,
        LIFNR            LIKE LFA1-LIFNR,
        NAME1            LIKE LFA1-NAME1,
      END   OF WA_LFA1,

      BEGIN OF WA_EKKO,
        EBELN            LIKE EKKO-EBELN,
        WAERS            LIKE EKKO-WAERS,
        BUKRS            LIKE EKKO-BUKRS,
        AEDAT            LIKE EKKO-AEDAT,
        UNSEZ            LIKE EKKO-UNSEZ,
        LIFNR            LIKE EKKO-LIFNR,
        ZTERM            LIKE EKKO-ZTERM,
      END   OF WA_EKKO,

      BEGIN OF WA_EKPO,
        EBELN            LIKE EKPO-EBELN,
        MATNR            LIKE EKPO-MATNR,
        TXZ01            LIKE EKPO-TXZ01,
        MEINS            LIKE EKPO-MEINS,
        MENGE            LIKE EKPO-MENGE,
        NETPR            LIKE EKPO-NETPR,
        NETWR            LIKE EKPO-NETWR,
        WERKS            LIKE EKPO-WERKS,
      END   OF WA_EKPO,

      BEGIN OF WA_EKBE,
        WAERS            LIKE EKBE-WAERS,
        EBELN            LIKE EKBE-EBELN,
        MENGE            LIKE EKBE-MENGE,
        DMBTR            LIKE EKBE-DMBTR,
        WRBTR            LIKE EKBE-WRBTR,
      END   OF WA_EKBE,

      BEGIN OF WA_T001W,
        WERKS            LIKE EKPO-WERKS,
        NAME1            LIKE T001W-NAME1,
      END   OF WA_T001W,

      BEGIN OF WA_MAKT,
        MATNR            LIKE MAKT-MATNR,
        MAKTX            LIKE MAKT-MAKTX,
      END   OF WA_MAKT,

      BEGIN OF WA_T052,
        ZTERM            LIKE V_T052-ZTERM,
        ZTAG1            LIKE V_T052-ZTAG1,
      END   OF WA_T052,

      BEGIN OF WA_BSAK,
        BUKRS            LIKE BSAK-BUKRS,
        LIFNR            LIKE BSAK-LIFNR,
        BLART            LIKE BSAK-BLART,
        BUDAT            LIKE BSAK-BUDAT,
        BELNR            LIKE BSAK-BELNR,
        SHKZG            LIKE BSAK-SHKZG,
        DMBTR            LIKE BSAK-DMBTR,
        DMBE2            LIKE BSAK-DMBE2,
        UMSKS            LIKE BSAK-UMSKS,
        EBELN            LIKE BSAK-EBELN,
        ZUONR            LIKE BSAK-ZUONR,
      END   OF WA_BSAK,

      BEGIN OF WA_BKPF,
        BELNR            LIKE BKPF-BELNR,
        AWKEY            LIKE BKPF-AWKEY,
        AWKEY10(10),
      END   OF WA_BKPF,

      BEGIN OF WA_RSEG,
        BELNR            LIKE RSEG-BELNR,
        EBELN            LIKE RSEG-EBELN,
      END   OF WA_RSEG,

      BEGIN OF WA_RELATORIO,
        BUKRS             LIKE EKKO-BUKRS,
        BUTXT             LIKE T001-BUTXT,
        WERKS             LIKE EKPO-WERKS,
        NAME1             LIKE T001W-NAME1,
        AEDAT             LIKE EKKO-AEDAT,
        EBELN             LIKE EKKO-EBELN,
        MATNR(6),
        MAKTX             LIKE MAKT-MAKTX,
        MEINS             LIKE EKPO-MEINS,
        WAERS             LIKE EKBE-WAERS,
        MENGE             LIKE EKPO-MENGE,
        CAL12             LIKE EKPO-NETPR,
        CAL13             LIKE EKPO-NETWR,
        CAL14             LIKE EKPO-NETPR,
        CAL15             LIKE EKPO-NETWR,
        CAL16             LIKE EKBE-MENGE,
        CAL17             LIKE EKBE-DMBTR,
        CAL18             LIKE EKBE-WRBTR,
        CAL19             LIKE EKBE-MENGE,
        CAL20             LIKE EKBE-DMBTR,
        CAL21             LIKE EKBE-WRBTR,
        CAL22             LIKE EKBE-MENGE,
        CAL23             LIKE EKBE-DMBTR,
        CAL24             LIKE EKBE-WRBTR,
        CAL25             LIKE EKKO-UNSEZ,
        CAL26             LIKE EKKO-LIFNR,
        CAL27             LIKE LFA1-NAME1,
        CAL28             LIKE EKKO-AEDAT,
        CAL29             LIKE EKPO-NETWR,
        CAL30             LIKE EKPO-NETWR,
        TOTAL_PAGO_BRL    LIKE BSAK-DMBTR,
        TOTAL_PAGO_USD    LIKE BSAK-DMBE2,
        SALDO_PAGAR_BRL   LIKE BSAK-DMBTR,
        SALDO_PAGAR_USD   LIKE BSAK-DMBE2,
        TOTAL_ADIANT_BRL  LIKE BSAK-DMBTR,
        TOTAL_ADIANT_USD  LIKE BSAK-DMBE2,
        TOTAL_FATURADOBRL LIKE EKBE-DMBTR,
        TOTAL_FATURADOUSD LIKE EKBE-WRBTR,
        SALDO_FATURA_BRL  LIKE EKBE-DMBTR,
        SALDO_FATURA_USD  LIKE EKBE-WRBTR,
      END OF WA_RELATORIO.

DATA: IT_LFA1            LIKE STANDARD TABLE OF WA_LFA1,
      IT_EKKO            LIKE STANDARD TABLE OF WA_EKKO,
      IT_EKPO            LIKE STANDARD TABLE OF WA_EKPO,
      IT_EKBE            LIKE STANDARD TABLE OF WA_EKBE,
      IT_EKBE2            LIKE STANDARD TABLE OF WA_EKBE,
      IT_T001W           LIKE STANDARD TABLE OF WA_T001W,
      IT_MAKT            LIKE STANDARD TABLE OF WA_MAKT,
      IT_RELATORIO       LIKE STANDARD TABLE OF WA_RELATORIO,
      IT_EKBE_T          LIKE STANDARD TABLE OF WA_EKBE,
      IT_EKBE_T2         LIKE STANDARD TABLE OF WA_EKBE,
      IT_T052            LIKE STANDARD TABLE OF WA_T052,
      IT_BSAK            LIKE STANDARD TABLE OF WA_BSAK,
      IT_BSAK_A          LIKE STANDARD TABLE OF WA_BSAK,
      IT_BSAK_P          LIKE STANDARD TABLE OF WA_BSAK,
      IT_BKPF            LIKE STANDARD TABLE OF WA_BKPF,
      IT_BKPF_P          LIKE STANDARD TABLE OF WA_BKPF,
      IT_RSEG            LIKE STANDARD TABLE OF WA_RSEG,
      IT_RSEG_P          LIKE STANDARD TABLE OF WA_RSEG.


*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
DATA:
      VG_BUTXT           LIKE T001-BUTXT,
      VG_UKURS           LIKE TCURR-UKURS.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.
PARAMETERS: R_FIS  RADIOBUTTON GROUP TP DEFAULT 'X' USER-COMMAND US_ARCHIVE,
            R_FIN  RADIOBUTTON GROUP TP.
SELECTION-SCREEN END   OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS: P_BUKRS      LIKE EKKO-BUKRS,
            S_UNSEZ(9)   TYPE C.
SELECT-OPTIONS:
            S_AEDAT      FOR  EKKO-AEDAT,
            S_BUDAT      FOR  BKPF-BUDAT,
            S_LIFNR      FOR  LFA1-LIFNR,
            S_EBELN      FOR  EKKO-EBELN,
*Período Entrada Mercadoria
            S_PEMER      FOR  EKBE-BUDAT,
            S_WERKS      FOR  EKPO-WERKS.
PARAMETERS: P_BRSCH      LIKE LFA1-BRSCH,
            P_GDATU      LIKE TCURR-GDATU.
SELECTION-SCREEN END   OF BLOCK B1.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
*PARAMETERS: P_LAYOUT    LIKE disvariant-variant.
*SELECTION-SCREEN END   OF BLOCK B2.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF SY-UCOMM EQ 'US_ARCHIVE'.
    PERFORM ATUALIZA_SELECAO.
  ELSE.
    IF P_BUKRS IS INITIAL.
      MESSAGE I000 WITH '"Empresa" campo obrigatório!'.
      LEAVE TO SCREEN 1000.
      EXIT.
    ENDIF.

    IF S_AEDAT IS INITIAL.
      MESSAGE I000 WITH '"Período pedido" campo obrigatório!'.
      LEAVE TO SCREEN 1000.
      EXIT.
    ENDIF.

    IF R_FIN IS NOT INITIAL.
      IF S_BUDAT IS INITIAL.
        MESSAGE I000 WITH '"Periodo Financeiro" campo obrigatório!'.
        LEAVE TO SCREEN 1000.
        EXIT.
      ENDIF.
    ENDIF.

    IF P_GDATU IS INITIAL.
      MESSAGE I000 WITH '"Data da cotação" campo obrigatório!'.
      LEAVE TO SCREEN 1000.
      EXIT.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.
  P_BRSCH = '0002'.
  PERFORM ATUALIZA_SELECAO.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  PERFORM ATUALIZA_SELECAO.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  SY-MANDT = '060'.
  SELECT SINGLE UKURS
    FROM TCURR
    INTO (VG_UKURS)
   WHERE KURST EQ 'B'
     AND GDATU EQ P_GDATU
     AND FCURR EQ 'USD'
     AND TCURR EQ 'BRL'.
  IF SY-SUBRC NE 0.
    MESSAGE I000 WITH 'Para está data não tem taxa de dólar cadastrada.'.
    EXIT.
  ENDIF.
  PERFORM F_EVITA_TIME_OUT USING 'Processando...'.
  IF R_FIN IS INITIAL.
    PERFORM F_SELECIONA_DADOS_FIS.
  ELSE.
    PERFORM F_SELECIONA_DADOS_FIN.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  f_sel_fornecedores
*&---------------------------------------------------------------------*
*       Seleciono na tabela LFA1 todos os fornecedores (LIFNR) que
*       estão no Setor selecionado (BRSCH)
*----------------------------------------------------------------------*
FORM F_SEL_FORNECEDORES .
  FREE IT_LFA1.
  IF P_BRSCH IS INITIAL.
    SELECT LIFNR NAME1
      FROM LFA1
      INTO TABLE IT_LFA1
     WHERE LIFNR IN S_LIFNR.
  ELSE.
    SELECT LIFNR NAME1
      FROM LFA1
      INTO TABLE IT_LFA1
     WHERE LIFNR IN S_LIFNR
       AND BRSCH EQ P_BRSCH.
  ENDIF.
ENDFORM.                    " f_sel_fornecedores

*&---------------------------------------------------------------------*
*&      Form  f_sel_ped_cab
*&---------------------------------------------------------------------*
*       Seleciono os numeros de pedido (EBELN) conforme os parametros
*       informados e com os fornecedores selecionados na it_lfa1
*----------------------------------------------------------------------*
FORM F_SEL_PED_CAB .
  FREE IT_EKKO.
  CHECK IT_LFA1[] IS NOT INITIAL.
  IF S_UNSEZ IS INITIAL.
    SELECT EBELN WAERS BUKRS AEDAT UNSEZ LIFNR ZTERM
      FROM EKKO
      INTO TABLE IT_EKKO
       FOR ALL ENTRIES IN IT_LFA1
     WHERE BUKRS EQ P_BUKRS
       AND BSTYP EQ 'F'
       AND AEDAT IN S_AEDAT
       AND LIFNR EQ IT_LFA1-LIFNR
       AND EBELN IN S_EBELN.
  ELSE.
    SELECT EBELN WAERS BUKRS AEDAT UNSEZ LIFNR ZTERM
      FROM EKKO
      INTO TABLE IT_EKKO
       FOR ALL ENTRIES IN IT_LFA1
     WHERE BUKRS EQ P_BUKRS
       AND BSTYP EQ 'F'
       AND AEDAT IN S_AEDAT
       AND LIFNR EQ IT_LFA1-LIFNR
       AND EBELN IN S_EBELN
       AND UNSEZ EQ S_UNSEZ.
  ENDIF.
ENDFORM.                    " f_sel_ped_cab

*&---------------------------------------------------------------------*
*&      Form  f_sel_ped_item
*&---------------------------------------------------------------------*
*       Seleciono os itens de pedido partindo da selecção na it_ekko
*----------------------------------------------------------------------*
FORM F_SEL_PED_ITEM .
  FREE IT_EKPO.
  CHECK IT_EKKO[] IS NOT INITIAL.
  SELECT EBELN MATNR TXZ01 MEINS MENGE NETPR NETWR WERKS
    FROM EKPO
    INTO TABLE IT_EKPO
     FOR ALL ENTRIES IN IT_EKKO
   WHERE EBELN EQ IT_EKKO-EBELN
     AND WERKS IN S_WERKS.
ENDFORM.                    " f_sel_ped_item

*&---------------------------------------------------------------------*
*&      Form  f_emp_centro
*&---------------------------------------------------------------------*
*       Seleciono o nome da empresa e do centro.
*----------------------------------------------------------------------*
FORM F_EMP_CENTRO_TX .
  SELECT SINGLE BUTXT
    FROM T001
    INTO VG_BUTXT
   WHERE BUKRS EQ P_BUKRS.

  CHECK IT_EKPO[] IS NOT INITIAL.
  SELECT WERKS NAME1
    FROM T001W
    INTO TABLE IT_T001W
     FOR ALL ENTRIES IN IT_EKPO
   WHERE WERKS EQ IT_EKPO-WERKS.

ENDFORM.                    " f_emp_centro

*&---------------------------------------------------------------------*
*&      Form  f_sel_mov_mercad
*&---------------------------------------------------------------------*
*       Seleciono a movimentação de mercadoria (EKBE) conforme dados
*       selecionados na EKKO.
*----------------------------------------------------------------------*
FORM F_SEL_MOV_MERCAD .
  FREE: IT_EKBE, IT_EKBE2.
  CHECK IT_EKKO[] IS NOT INITIAL.
  SELECT WAERS EBELN MENGE DMBTR WRBTR
    FROM EKBE
    INTO TABLE IT_EKBE
     FOR ALL ENTRIES IN IT_EKPO
   WHERE VGABE EQ 1
     AND EBELN EQ IT_EKPO-EBELN.

  SORT IT_EKBE BY EBELN.
  LOOP AT IT_EKBE INTO WA_EKBE.
    COLLECT WA_EKBE INTO IT_EKBE_T.
    CLEAR WA_EKBE.
  ENDLOOP.

  IF S_PEMER IS NOT INITIAL.
    SELECT WAERS EBELN MENGE DMBTR WRBTR
      FROM EKBE
      INTO TABLE IT_EKBE2
       FOR ALL ENTRIES IN IT_EKPO
     WHERE VGABE EQ 1
       AND EBELN EQ IT_EKPO-EBELN
       AND BUDAT IN S_PEMER.

    SORT IT_EKBE2 BY EBELN.
    LOOP AT IT_EKBE2 INTO WA_EKBE.
      COLLECT WA_EKBE INTO IT_EKBE_T2.
      CLEAR WA_EKBE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f_sel_mov_mercad

*&---------------------------------------------------------------------*
*&      Form  f_sel_material
*&---------------------------------------------------------------------*
*       Seleciona a descrição dos materiais.
*----------------------------------------------------------------------*
FORM F_SEL_MATERIAL.
  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
     FOR ALL ENTRIES IN IT_EKPO
  WHERE MATNR EQ IT_EKPO-MATNR.
ENDFORM.                    " f_sel_material

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
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Chamada de select de dados para a opção Fisico.
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS_FIS .
  PERFORM F_SEL_FORNECEDORES.
  PERFORM F_SEL_PED_CAB.
  PERFORM F_SEL_PED_ITEM.
  PERFORM F_EMP_CENTRO_TX.
  PERFORM F_SEL_MATERIAL.
  PERFORM F_SEL_MOV_MERCAD.

  PERFORM F_MONTA_DADOS_FIS.
  PERFORM F_MONTA_CAB.
  PERFORM F_MONTA_ESTRUTURA_FIS.
  PERFORM F_EXECUTA_ALV.

ENDFORM.                    " f_seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  f_montra_estrutura
*&---------------------------------------------------------------------*
*       Monta estrutura de ALV
*----------------------------------------------------------------------*
FORM F_MONTA_ESTRUTURA_FIS .
  PERFORM F_FIELDCAT USING:
        '0' '' 'IT_RELATORIO' 'CAL25' 'Safra'
        9  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '1' '' 'IT_RELATORIO' 'BUKRS' 'Empresa'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '2' '' 'IT_RELATORIO' 'BUTXT' 'Nome'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '3' '' 'IT_RELATORIO' 'WERKS' 'Centro'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '4' '' 'IT_RELATORIO' 'NAME1' 'Nome Centro'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '5' '' 'IT_RELATORIO' 'CAL26' 'Fornecedor'
        10   ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '6' '' 'IT_RELATORIO' 'CAL27' 'Nome'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '7' '' 'IT_RELATORIO' 'AEDAT' 'Dt. Ped.'
        10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '8' 'X' 'IT_RELATORIO' 'EBELN' 'Pedido'
        10  'X'  ''             '' 'X'
  CHANGING IT_FIELDCAT,
        '9' '' 'IT_RELATORIO' 'MATNR' 'Material'
         6  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '10' '' 'IT_RELATORIO' 'MAKTX' 'Descrição'
        25  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '11' '' 'IT_RELATORIO' 'MEINS' 'UNID.'
        07  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '12' '' 'IT_RELATORIO' 'WAERS' 'Moeda'
        05  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '13' '' 'IT_RELATORIO' 'MENGE' 'Qt. Pedido'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '14' '' 'IT_RELATORIO' 'CAL12' 'Preço R$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '15' '' 'IT_RELATORIO' 'CAL13' 'Faturamento R$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '16' '' 'IT_RELATORIO' 'CAL14' 'Preço US$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '17' '' 'IT_RELATORIO' 'CAL15' 'Faturamento US$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '18' '' 'IT_RELATORIO' 'CAL16' 'Qt. Recebida'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '19' '' 'IT_RELATORIO' 'CAL17' 'Recebido R$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '20' '' 'IT_RELATORIO' 'CAL18' 'Recebido US$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '21' '' 'IT_RELATORIO' 'CAL19' 'Sdo. Entregar'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '22' '' 'IT_RELATORIO' 'CAL20' 'Sdo. Receber R$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '23' '' 'IT_RELATORIO' 'CAL21' 'SDO. Receber US$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '24' '' 'IT_RELATORIO' 'CAL22' 'Qte Receb.Periodo'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '25' '' 'IT_RELATORIO' 'CAL23' 'Receb.Periodo R$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '26' '' 'IT_RELATORIO' 'CAL24' 'Receb.Periodo US$'
        17  ''  ''             '' ''
  CHANGING IT_FIELDCAT.

ENDFORM.                    " f_montra_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_monta_dados
*&---------------------------------------------------------------------*
*       Monta os dados para a impressão na ALV
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS_FIS .
  FREE IT_RELATORIO.
  SORT: IT_EKKO  BY EBELN,
        IT_EKPO  BY EBELN MATNR,
        IT_MAKT  BY MATNR,
        IT_T001W BY WERKS,
        IT_EKBE_T BY EBELN.
*---> 05/07/2023 - Migração S4 - DL
  SORT IT_LFA1 BY LIFNR.
  SORT IT_EKBE_T2 BY EBELN.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT IT_EKKO INTO WA_EKKO.
    WA_RELATORIO-BUKRS = WA_EKKO-BUKRS.
    WA_RELATORIO-BUTXT = VG_BUTXT."Empresa
    WA_RELATORIO-AEDAT = WA_EKKO-AEDAT."Data
    WA_RELATORIO-EBELN = WA_EKKO-EBELN."Pedido
    WA_RELATORIO-CAL25 = WA_EKKO-UNSEZ."Safra
    WA_RELATORIO-CAL26 = WA_EKKO-LIFNR."Fornecedor

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR
                                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_RELATORIO-CAL27 = WA_LFA1-NAME1."Nome do Fornecedor
    ENDIF.

    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_EKKO-EBELN
                                                     BINARY SEARCH.
    WA_RELATORIO-MATNR = WA_EKPO-MATNR+12."Material
    WA_RELATORIO-MEINS = WA_EKPO-MEINS."Unidade
    WA_RELATORIO-MENGE = WA_EKPO-MENGE."Qtde Pedido

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR
                                                     BINARY SEARCH.
    WA_RELATORIO-MAKTX = WA_MAKT-MAKTX."Descrição

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKPO-WERKS
                                                       BINARY SEARCH.
    WA_RELATORIO-WERKS = WA_T001W-WERKS."Centro
    WA_RELATORIO-NAME1 = WA_T001W-NAME1."Descrição

    READ TABLE IT_EKBE_T INTO WA_EKBE WITH KEY EBELN = WA_EKKO-EBELN
                                                       BINARY SEARCH.
    WA_RELATORIO-WAERS = WA_EKBE-WAERS."Moeda
    WA_RELATORIO-CAL16 = WA_EKBE-MENGE."Qtde. Recebida
    WA_RELATORIO-CAL17 = WA_EKBE-DMBTR."Total R$ Recebida
    WA_RELATORIO-CAL18 = WA_EKBE-WRBTR."Total US$ Recebida
    WA_RELATORIO-CAL19 = WA_RELATORIO-MENGE - WA_RELATORIO-CAL16."Saldo a entregar

    READ TABLE IT_EKBE_T2 INTO WA_EKBE WITH KEY EBELN = WA_EKKO-EBELN
                                                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_RELATORIO-CAL22 = WA_EKBE-MENGE."Qtde. Recebida
      WA_RELATORIO-CAL23 = WA_EKBE-DMBTR."Total R$ Recebida
      WA_RELATORIO-CAL24 = WA_EKBE-WRBTR."Total US$ Recebida
    ENDIF.

    IF WA_RELATORIO-WAERS EQ 'BRL'.
      WA_RELATORIO-CAL13 = WA_EKPO-NETPR."Faturamento R$
      WA_RELATORIO-CAL20 = WA_RELATORIO-CAL13 - WA_RELATORIO-CAL17."Saldo a receber R$
      IF WA_RELATORIO-CAL16 EQ 0.
        WA_RELATORIO-CAL15 = WA_EKBE-WRBTR."Faturamento US$
      ELSE.
        WA_RELATORIO-CAL15 = ( WA_RELATORIO-CAL20 /  VG_UKURS ) + WA_EKBE-WRBTR."Faturamento US$
      ENDIF.
      WA_RELATORIO-CAL21 = WA_RELATORIO-CAL15 - WA_RELATORIO-CAL18."Saldo a Receber US$
      WA_RELATORIO-CAL12 = WA_EKPO-NETPR.
      WA_RELATORIO-CAL14 = WA_RELATORIO-CAL15 / WA_RELATORIO-MENGE.
    ELSE.
      WA_RELATORIO-CAL15 = WA_EKPO-NETWR."Faturamento US$
      WA_RELATORIO-CAL21 = WA_RELATORIO-CAL15 - WA_RELATORIO-CAL18."Saldo a Receber US$
      IF WA_RELATORIO-CAL16 EQ 0.
        WA_RELATORIO-CAL13 = WA_EKBE-DMBTR."Faturamento R$
      ELSE.
        WA_RELATORIO-CAL13 = ( WA_RELATORIO-CAL21 *  VG_UKURS ) + WA_EKBE-DMBTR."Faturamento R$
      ENDIF.
      WA_RELATORIO-CAL20 = WA_RELATORIO-CAL13 - WA_RELATORIO-CAL17."Saldo a receber R$
      WA_RELATORIO-CAL12 = WA_RELATORIO-CAL13 / WA_RELATORIO-MENGE.
      WA_RELATORIO-CAL14 = WA_EKPO-NETPR.

    ENDIF.

    APPEND WA_RELATORIO TO IT_RELATORIO.
  ENDLOOP.
ENDFORM.                    " f_monta_dados
*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       Executa a ALV com as informações da it_relatório
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
    I_CALLBACK_PROGRAM       = VL_REPID
*    i_callback_pf_status_set = 'SET_PF_STATUS'
    I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
    IS_LAYOUT                = VG_LAYOUT
*    i_background_id          = c_enjoy
    IT_FIELDCAT              = IT_FIELDCAT[]
    I_DEFAULT                = 'A'
    I_SAVE                   = 'X'
    IT_EVENTS                = IT_EVENT[]
  TABLES
    T_OUTTAB                 = IT_RELATORIO
  EXCEPTIONS
    PROGRAM_ERROR            = 1
    OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "f_executa_alv

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar no nº de pedido chamo a transação ME23N
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING UCOMM LIKE SY-UCOMM
      SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_RELATORIO INTO WA_RELATORIO INDEX SELFIELD-TABINDEX.
  IF SELFIELD-FIELDNAME = 'EBELN'.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'ME23N'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'ME23N'.
    ENDIF.

    SET PARAMETER ID 'BES' FIELD WA_RELATORIO-EBELN.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  f_monta_cab
*&---------------------------------------------------------------------*
*       Gera o cabeçalho do relatório
*----------------------------------------------------------------------*
FORM F_MONTA_CAB .
* Colocando os dados para exibição do cabecalho
* Título do relatório
  DATA: VL_DATA1(10)     TYPE C,
        VL_DATA2(10)     TYPE C,
        VL_DATA(25)      TYPE C,
        VL_DATA_ATUAL    TYPE C LENGTH 14,
        VL_HORA          TYPE C LENGTH 8,
        VL_DATA_COTACAO(30).

  CLEAR IT_HEADER.

  IT_HEADER-TYP  = 'H'.

  IF R_FIN IS INITIAL.
    IT_HEADER-INFO = 'Relatório Fisíco'.
  ELSE.
    IT_HEADER-INFO = 'Relatório Fisíco Financeiro'.
  ENDIF.

  APPEND  IT_HEADER.

  CONCATENATE S_AEDAT-LOW+6(2) '.'
              S_AEDAT-LOW+4(2) '.'
              S_AEDAT-LOW(4)
              INTO VL_DATA1.

  CONCATENATE S_AEDAT-HIGH+6(2) '.'
              S_AEDAT-HIGH+4(2) '.'
              S_AEDAT-HIGH(4)
              INTO VL_DATA2.

  CONCATENATE VL_DATA1
              'a'
              VL_DATA2 INTO VL_DATA
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Período pedido'.
  IT_HEADER-INFO = VL_DATA.
  APPEND  IT_HEADER.

  IF R_FIN IS NOT INITIAL.
    IT_HEADER-TYP  = 'S'.
    IT_HEADER-KEY  = 'Período Financeiro'.

    CONCATENATE S_BUDAT-LOW+6(2) '.'
            S_BUDAT-LOW+4(2) '.'
            S_BUDAT-LOW(4)
            INTO VL_DATA1.

    CONCATENATE S_BUDAT-HIGH+6(2) '.'
            S_BUDAT-HIGH+4(2) '.'
            S_BUDAT-HIGH(4)
            INTO VL_DATA2.

    CONCATENATE VL_DATA1 'a' VL_DATA2 INTO IT_HEADER-INFO
                SEPARATED BY SPACE.
    APPEND  IT_HEADER.
  ELSE.
    IF S_PEMER IS NOT INITIAL.
      IT_HEADER-TYP  = 'S'.
      IT_HEADER-KEY  = 'Período Entrada Mercadoria'.

      CONCATENATE S_PEMER-LOW+6(2) '.'
              S_PEMER-LOW+4(2) '.'
              S_PEMER-LOW(4)
              INTO VL_DATA1.

      CONCATENATE S_PEMER-HIGH+6(2) '.'
              S_PEMER-HIGH+4(2) '.'
              S_PEMER-HIGH(4)
              INTO VL_DATA2.

      CONCATENATE VL_DATA1 'a' VL_DATA2 INTO IT_HEADER-INFO
                  SEPARATED BY SPACE.
      APPEND  IT_HEADER.
    ENDIF.
  ENDIF.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Cotação'.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      INPUT  = P_GDATU
    IMPORTING
      OUTPUT = VL_DATA_COTACAO.

  VL_DATA2 = VG_UKURS.
  CONCATENATE VL_DATA_COTACAO ' - ' VL_DATA2 INTO IT_HEADER-INFO.
  APPEND IT_HEADER.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Usuário'.
  IT_HEADER-INFO = SY-UNAME.
  APPEND  IT_HEADER.

  CONCATENATE SY-DATUM+6(2)
              '.' SY-DATUM+4(2)
              '.' SY-DATUM(4) INTO VL_DATA_ATUAL.

  DATA HORA_UZEIT TYPE UZEIT.
  HORA_UZEIT = SY-UZEIT.

  CONCATENATE HORA_UZEIT(2)
              ':' HORA_UZEIT+2(2)
              ':' HORA_UZEIT+4(2) INTO VL_HORA.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Data'.
  CONCATENATE VL_DATA_ATUAL ' - ' VL_HORA INTO IT_HEADER-INFO.
  APPEND IT_HEADER.

ENDFORM.                    " f_monta_cab

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*      Chama o cabeçalho da ALV                                        *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
*    i_logo             = c_logo
    IT_LIST_COMMENTARY = IT_HEADER[].

ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  f_evita_time_out
*&---------------------------------------------------------------------*
*       Mostra msg na tela.
*----------------------------------------------------------------------*
*      -->P_msg   Mensagem a exibir
*----------------------------------------------------------------------*
FORM F_EVITA_TIME_OUT  USING    VALUE(P_MSG).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = P_MSG.
ENDFORM.                    " f_evita_time_out

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados_fin
*&---------------------------------------------------------------------*
*       Seleciona dados para a opção Fisico Financeiro
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS_FIN.

  PERFORM F_SEL_FORNECEDORES.
  PERFORM F_SEL_PED_CAB.
  PERFORM F_DT_VENCIMENTO.
  PERFORM F_SEL_PED_ITEM.
  PERFORM F_EMP_CENTRO_TX.
  PERFORM F_SEL_MATERIAL.
  PERFORM F_SEL_MOV_MERCAD.
  PERFORM F_TOTAL_FATURA.
  PERFORM F_TOTAL_ADIANTAMENTO.
  PERFORM F_TOTAL_PAGO.

  PERFORM F_MONTA_DADOS_FIN.
  PERFORM F_MONTA_CAB.
  PERFORM F_MONTA_ESTRUTURA_FIN.
  PERFORM F_EXECUTA_ALV.

ENDFORM.                    " f_seleciona_dados_fin
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_FIN
*&---------------------------------------------------------------------*
*       Montar Tabela Interna com informações referente ao relatório
*       fisico financeiro.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS_FIN .
  FREE IT_RELATORIO.
*---> 05/07/2023 - Migração S4 - DL
  SORT IT_LFA1 BY LIFNR.
  SORT IT_T052 BY ZTERM.
  SORT IT_EKPO BY EBELN.
  SORT IT_T001W BY WERKS.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT IT_EKKO INTO WA_EKKO.
    CLEAR WA_RELATORIO.
    WA_RELATORIO-CAL25 = WA_EKKO-UNSEZ."Safra
    WA_RELATORIO-BUKRS = WA_EKKO-BUKRS."Empresa
    WA_RELATORIO-BUTXT = VG_BUTXT."Nome Empresa

    WA_RELATORIO-CAL26 = WA_EKKO-LIFNR."Fornecedor
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR
                                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_RELATORIO-CAL27 = WA_LFA1-NAME1."Nome do Fornecedor
    ENDIF.
    WA_RELATORIO-AEDAT = WA_EKKO-AEDAT."Data do Pedido
    WA_RELATORIO-EBELN = WA_EKKO-EBELN."Pedido
    WA_RELATORIO-WAERS = WA_EKKO-WAERS."Moeda

    READ TABLE IT_T052 INTO WA_T052 WITH KEY ZTERM = WA_EKKO-ZTERM
                                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_RELATORIO-CAL28 = WA_EKKO-AEDAT + WA_T052-ZTAG1."Data de Vencimento
    ENDIF.

    WA_RELATORIO-TOTAL_FATURADOBRL = 0.
    WA_RELATORIO-TOTAL_FATURADOUSD = 0.
*---> 05/07/2023 - Migração S4 - DL
    SORT IT_EKPO BY EBELN.
    SORT IT_T001W BY WERKS.
    SORT IT_BSAK BY LIFNR.
    SORT IT_BKPF BY BELNR.
    SORT IT_RSEG BY BELNR.
*<--- 05/07/2023 - Migração S4 - DL
    LOOP AT IT_BSAK INTO WA_BSAK WHERE LIFNR EQ WA_EKKO-LIFNR.
      LOOP AT IT_BKPF INTO WA_BKPF WHERE BELNR EQ WA_BSAK-BELNR.
        LOOP AT IT_RSEG INTO WA_RSEG WHERE BELNR = WA_BKPF-AWKEY10.
          IF WA_BSAK-SHKZG EQ 'H'.
            WA_RELATORIO-TOTAL_FATURADOBRL = WA_RELATORIO-TOTAL_FATURADOBRL - WA_BSAK-DMBTR.
            WA_RELATORIO-TOTAL_FATURADOUSD = WA_RELATORIO-TOTAL_FATURADOUSD - WA_BSAK-DMBE2.
          ELSE.
            WA_RELATORIO-TOTAL_FATURADOBRL = WA_RELATORIO-TOTAL_FATURADOBRL + WA_BSAK-DMBTR.
            WA_RELATORIO-TOTAL_FATURADOUSD = WA_RELATORIO-TOTAL_FATURADOUSD + WA_BSAK-DMBE2.
          ENDIF.
          WA_RELATORIO-TOTAL_FATURADOBRL = WA_RELATORIO-TOTAL_FATURADOBRL * -1.
          WA_RELATORIO-TOTAL_FATURADOUSD = WA_RELATORIO-TOTAL_FATURADOUSD * -1.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_EKKO-EBELN
                                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKPO-WERKS
                                                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        WA_RELATORIO-WERKS = WA_T001W-WERKS."Centro
        WA_RELATORIO-NAME1 = WA_T001W-NAME1."Descrição
      ENDIF.

      IF WA_EKKO-WAERS EQ 'BRL'.
        WA_RELATORIO-CAL29 = WA_EKPO-NETWR.
        WA_RELATORIO-CAL30 = ( ( WA_EKPO-NETWR - WA_RELATORIO-TOTAL_FATURADOBRL ) / VG_UKURS ) + WA_RELATORIO-TOTAL_FATURADOUSD.
      ELSE.
        WA_RELATORIO-CAL29 = ( ( WA_EKPO-NETWR - WA_RELATORIO-TOTAL_FATURADOUSD ) * VG_UKURS ) + WA_RELATORIO-TOTAL_FATURADOBRL.
        WA_RELATORIO-CAL30 = WA_EKPO-NETWR.
      ENDIF.
    ENDIF.

    WA_RELATORIO-TOTAL_ADIANT_BRL = 0.
    WA_RELATORIO-TOTAL_ADIANT_USD = 0.
    LOOP AT IT_BSAK_A INTO WA_BSAK WHERE LIFNR EQ WA_EKKO-LIFNR
                                     AND EBELN EQ WA_EKKO-EBELN.
      IF WA_BSAK-SHKZG EQ 'H'.
        WA_RELATORIO-TOTAL_ADIANT_BRL = WA_RELATORIO-TOTAL_ADIANT_BRL - WA_BSAK-DMBTR.
        WA_RELATORIO-TOTAL_ADIANT_USD = WA_RELATORIO-TOTAL_ADIANT_USD - WA_BSAK-DMBE2.
      ELSE.
        WA_RELATORIO-TOTAL_ADIANT_BRL = WA_RELATORIO-TOTAL_ADIANT_BRL + WA_BSAK-DMBTR.
        WA_RELATORIO-TOTAL_ADIANT_USD = WA_RELATORIO-TOTAL_ADIANT_USD + WA_BSAK-DMBE2.
      ENDIF.
    ENDLOOP.

    WA_RELATORIO-SALDO_FATURA_BRL = WA_RELATORIO-CAL29 - WA_RELATORIO-TOTAL_FATURADOBRL.
    WA_RELATORIO-SALDO_FATURA_USD = WA_RELATORIO-CAL30 - WA_RELATORIO-TOTAL_FATURADOUSD.

    LOOP AT IT_BSAK_P INTO WA_BSAK WHERE LIFNR EQ WA_EKKO-LIFNR.
      IF WA_BSAK-BLART NE 'RE'.
        IF ( WA_BSAK-ZUONR EQ WA_EKKO-EBELN ) OR
           ( WA_BSAK-EBELN EQ WA_EKKO-EBELN ).
          IF WA_BSAK-SHKZG EQ 'H'.
            WA_RELATORIO-TOTAL_PAGO_BRL = WA_RELATORIO-TOTAL_PAGO_BRL - WA_BSAK-DMBTR.
            WA_RELATORIO-TOTAL_PAGO_USD = WA_RELATORIO-TOTAL_PAGO_USD - WA_BSAK-DMBE2.
          ELSE.
            WA_RELATORIO-TOTAL_PAGO_BRL = WA_RELATORIO-TOTAL_PAGO_BRL + WA_BSAK-DMBTR.
            WA_RELATORIO-TOTAL_PAGO_USD = WA_RELATORIO-TOTAL_PAGO_USD + WA_BSAK-DMBE2.
          ENDIF.
        ENDIF.
      ELSE.
        LOOP AT IT_BKPF_P INTO WA_BKPF WHERE BELNR EQ WA_BSAK-BELNR.
          LOOP AT IT_RSEG_P INTO WA_RSEG WHERE BELNR EQ WA_BKPF-AWKEY10.
            IF WA_RSEG-EBELN EQ WA_EKKO-EBELN.
              IF WA_BSAK-SHKZG EQ 'H'.
                WA_RELATORIO-TOTAL_PAGO_BRL = WA_RELATORIO-TOTAL_PAGO_BRL - WA_BSAK-DMBTR.
                WA_RELATORIO-TOTAL_PAGO_USD = WA_RELATORIO-TOTAL_PAGO_USD - WA_BSAK-DMBE2.
              ELSE.
                WA_RELATORIO-TOTAL_PAGO_BRL = WA_RELATORIO-TOTAL_PAGO_BRL + WA_BSAK-DMBTR.
                WA_RELATORIO-TOTAL_PAGO_USD = WA_RELATORIO-TOTAL_PAGO_USD + WA_BSAK-DMBE2.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    WA_RELATORIO-TOTAL_PAGO_BRL = WA_RELATORIO-TOTAL_PAGO_BRL * -1.
    WA_RELATORIO-TOTAL_PAGO_USD = WA_RELATORIO-TOTAL_PAGO_USD * -1.
    WA_RELATORIO-SALDO_PAGAR_BRL  = WA_RELATORIO-CAL29 - WA_RELATORIO-TOTAL_PAGO_BRL.
    WA_RELATORIO-SALDO_PAGAR_USD  = WA_RELATORIO-CAL30 - WA_RELATORIO-TOTAL_PAGO_USD.

    IF ( WA_EKKO-WAERS EQ 'USD' ) AND ( WA_RELATORIO-SALDO_PAGAR_USD EQ 0 ).
*---> 14/06/2023 - Migração S4 - JS
*         WA_RELATORIO-CAL29 = WA_RELATORIO-TOTAL_PAGO_BRL.
      WA_RELATORIO-CAL29 = CONV #( WA_RELATORIO-TOTAL_PAGO_BRL ).
*<--- 14/06/2023 - Migração S4 - JS
      WA_RELATORIO-SALDO_PAGAR_BRL  = 0.
    ENDIF.
    IF ( WA_EKKO-WAERS EQ 'BRL' ) AND ( WA_RELATORIO-SALDO_PAGAR_USD EQ 0 ).
*---> 14/06/2023 - Migração S4 - JS
*        WA_RELATORIO-CAL30 = WA_RELATORIO-TOTAL_PAGO_USD.
    WA_RELATORIO-CAL30 = CONV #( WA_RELATORIO-TOTAL_PAGO_USD ).
*<--- 14/06/2023 - Migração S4 - JS

      WA_RELATORIO-SALDO_PAGAR_USD  = 0.
    ENDIF.

    WA_RELATORIO-TOTAL_ADIANT_BRL = WA_RELATORIO-TOTAL_ADIANT_BRL * -1.
    WA_RELATORIO-TOTAL_ADIANT_USD = WA_RELATORIO-TOTAL_ADIANT_USD * -1.
    APPEND WA_RELATORIO TO IT_RELATORIO.
  ENDLOOP.
ENDFORM.                    " F_MONTA_DADOS_FIN

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ESTRUTURA_FIN
*&---------------------------------------------------------------------*
*       Monta estrutura de apresentação das informações do relatório
*       fisico financeiro.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_MONTA_ESTRUTURA_FIN.

  PERFORM F_FIELDCAT USING:
        '0' '' 'IT_RELATORIO' 'CAL25' 'Safra'
        9  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '1' '' 'IT_RELATORIO' 'BUKRS' 'Empresa'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '2' '' 'IT_RELATORIO' 'BUTXT' 'Nome'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '3' '' 'IT_RELATORIO' 'WERKS' 'Centro'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '4' '' 'IT_RELATORIO' 'NAME1' 'Nome Centro'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '5' '' 'IT_RELATORIO' 'CAL26' 'Fornecedor'
        10   ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '6' '' 'IT_RELATORIO' 'CAL27' 'Nome'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '7' '' 'IT_RELATORIO' 'AEDAT' 'Dt. Ped.'
        10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '8' 'X' 'IT_RELATORIO' 'EBELN' 'Pedido'
        10  'X'  ''             '' 'X'
  CHANGING IT_FIELDCAT,
        '9' '' 'IT_RELATORIO' 'WAERS' 'Moeda'
        05  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '10' '' 'IT_RELATORIO' 'CAL28' 'Dt. Vencimento'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '11' '' 'IT_RELATORIO' 'CAL29' 'Total Pedido R$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '12' '' 'IT_RELATORIO' 'CAL30' 'Total Pedido US$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '13' '' 'IT_RELATORIO' 'TOTAL_PAGO_BRL' 'Pago R$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '14' '' 'IT_RELATORIO' 'TOTAL_PAGO_USD' 'Pago US$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '15' '' 'IT_RELATORIO' 'SALDO_PAGAR_BRL' 'Vlr a Pagar R$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '16' '' 'IT_RELATORIO' 'SALDO_PAGAR_USD' 'Vlr a Pagar US$'
        14   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '17' '' 'IT_RELATORIO' 'TOTAL_ADIANT_BRL' 'Adto R$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '18' '' 'IT_RELATORIO' 'TOTAL_ADIANT_USD' 'Adto US$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '19' '' 'IT_RELATORIO' 'TOTAL_FATURADOBRL' 'Total Faturado R$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '20' '' 'IT_RELATORIO' 'TOTAL_FATURADOUSD' 'Total Faturado US$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '21' '' 'IT_RELATORIO' 'SALDO_FATURA_BRL' 'Sdo Faturar R$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT,
        '22' '' 'IT_RELATORIO' 'SALDO_FATURA_USD' 'Sdo Faturar US$'
        15   '' ''             ''      ''
  CHANGING IT_FIELDCAT.

ENDFORM.                    " F_MONTA_ESTRUTURA_FIN
*&---------------------------------------------------------------------*
*&      Form  F_DT_VENCIMENTO
*&---------------------------------------------------------------------*
*       Busca data de vencimento
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_DT_VENCIMENTO .

  SELECT ZTERM ZTAG1
    FROM T052
    INTO TABLE IT_T052
    FOR ALL ENTRIES IN IT_EKKO
   WHERE ZTERM EQ IT_EKKO-ZTERM.

ENDFORM.                    " F_DT_VENCIMENTO

*&---------------------------------------------------------------------*
*&      Form  F_TOTAL_FATURA
*&---------------------------------------------------------------------*
*       Valor total da fatura.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_TOTAL_FATURA.

  SELECT BUKRS LIFNR BLART BUDAT BELNR
         SHKZG DMBTR DMBE2 UMSKS EBELN
         ZUONR
    FROM BSAK
    INTO TABLE IT_BSAK
     FOR ALL ENTRIES IN IT_EKKO
   WHERE BUKRS EQ P_BUKRS
     AND LIFNR EQ IT_EKKO-LIFNR
     AND BLART EQ 'RE'
     AND BUDAT IN S_BUDAT.

  DATA IT_BKPF2 LIKE STANDARD TABLE OF WA_BKPF.

  SELECT BELNR AWKEY
    FROM BKPF
    INTO TABLE IT_BKPF2
     FOR ALL ENTRIES IN IT_BSAK
   WHERE BELNR EQ IT_BSAK-BELNR.

  FREE IT_BKPF.
  LOOP AT IT_BKPF2 INTO WA_BKPF.
    WA_BKPF-AWKEY10 = WA_BKPF-AWKEY(10).
    APPEND WA_BKPF TO IT_BKPF.
  ENDLOOP.

  SELECT BELNR EBELN
    FROM RSEG
    INTO TABLE IT_RSEG
     FOR ALL ENTRIES IN IT_BKPF
   WHERE BELNR = IT_BKPF-AWKEY10.

ENDFORM.                    " F_TOTAL_FATURA
*&---------------------------------------------------------------------*
*&      Form  F_TOTAL_ADIANTAMENTO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_TOTAL_ADIANTAMENTO .

  SELECT BUKRS LIFNR BLART BUDAT BELNR
         SHKZG DMBTR DMBE2 UMSKS EBELN
         ZUONR
    FROM BSAK
    INTO TABLE IT_BSAK_A
     FOR ALL ENTRIES IN IT_EKKO
   WHERE BUKRS EQ P_BUKRS
     AND LIFNR EQ IT_EKKO-LIFNR
     AND UMSKS EQ 'A'
     AND EBELN EQ IT_EKKO-EBELN
     AND BUDAT IN S_BUDAT.

ENDFORM.                    " F_TOTAL_ADIANTAMENTO

*&---------------------------------------------------------------------*
*&      Form  F_TOTAL_PAGO
*&---------------------------------------------------------------------*
*       Obter total pago
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM F_TOTAL_PAGO .

  SELECT BUKRS LIFNR BLART BUDAT BELNR
         SHKZG DMBTR DMBE2 UMSKS EBELN
         ZUONR
    FROM BSAK
    INTO TABLE IT_BSAK_P
     FOR ALL ENTRIES IN IT_EKKO
   WHERE BUKRS EQ P_BUKRS
     AND LIFNR EQ IT_EKKO-LIFNR
     AND BUDAT IN S_BUDAT.

  DATA IT_BKPF3 LIKE STANDARD TABLE OF WA_BKPF.

  SELECT BELNR AWKEY
    FROM BKPF
    INTO TABLE IT_BKPF3
     FOR ALL ENTRIES IN IT_BSAK
   WHERE BELNR EQ IT_BSAK-BELNR.

  FREE IT_BKPF_P.
  LOOP AT IT_BKPF3 INTO WA_BKPF.
    WA_BKPF-AWKEY10 = WA_BKPF-AWKEY(10).
    APPEND WA_BKPF TO IT_BKPF_P.
  ENDLOOP.

  SELECT BELNR EBELN
    FROM RSEG
    INTO TABLE IT_RSEG_P
     FOR ALL ENTRIES IN IT_BKPF_P
   WHERE BELNR = IT_BKPF_P-AWKEY10.

ENDFORM.                    " F_TOTAL_PAGO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SELECAO
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM ATUALIZA_SELECAO.

  IF R_FIS IS INITIAL.
    CLEAR: S_BUDAT.
    LOOP AT SCREEN.
      IF SCREEN-NAME CS 'S_BUDAT'.
        SCREEN-INPUT = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF SCREEN-NAME CS 'S_PEMER'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR: S_PEMER.

    LOOP AT SCREEN.
      IF SCREEN-NAME CS 'S_BUDAT'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF SCREEN-NAME CS 'S_PEMER'.
        SCREEN-INPUT = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ATUALIZA_SELECAO
