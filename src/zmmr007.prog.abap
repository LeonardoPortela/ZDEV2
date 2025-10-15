************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 24.10.2007                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Relatorio para listar as notas de saidas ou entada  *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 24.10.2007   Michely               Criação              DEVK902975   *
* 19.11.2007   Marcus Barbara        Alteração            DEVK905220   *
* 19.11.2007   Marcus Barbara        Alteração            DEVK905222   *
*                                                                      *
************************************************************************

REPORT ZMMR007 MESSAGE-ID Z01
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
*        j_1baj,       "Grupo de impostos
*        lfa1,         "Mestre de Fornecedores
*        t001,         "Empresas
*        j_1bbranch,   "Filial
        BSEG.         "Documentos contabeis
*        bkpf,         "Documentos contabeis
*        t007s.        "Descricao de IVA
*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS,
            KKBLO.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV, "Estrutura de saida
      IT_HEADER          TYPE KKBLO_T_LISTHEADER WITH HEADER LINE,   "Cabeçalho
      IT_EVENT           TYPE SLIS_T_EVENT       WITH HEADER LINE,   "Eventos
      VG_LISTHEADER      TYPE SLIS_T_LISTHEADER,
      VG_LAYOUT          TYPE SLIS_LAYOUT_ALV,   "Layout do alv
      VG_VARIANT         TYPE DISVARIANT,        "Variantes de saida
      VG_GRID_SETTINGS   TYPE LVC_S_GLAY,        "Config p/Grid
      VG_REPID           TYPE SY-REPID,
      ST_HEADER          TYPE KKBLO_LISTHEADER.

DATA: BEGIN OF WA_RELATORIO OCCURS 0,
        BRANCH           LIKE J_1BNFDOC-BRANCH,
        CFOP             LIKE J_1BNFLIN-CFOP,       "CFOP
        DOCNUM           LIKE J_1BNFDOC-DOCNUM,     "Nr. Documento
        NFNUM            LIKE J_1BNFDOC-NFNUM,      "Nr. Nota fiscal
        SERIES           LIKE J_1BNFDOC-SERIES,     "Serie
        NAME1            LIKE LFA1-NAME1,           "Nome do fornecedor
        STCD1            TYPE C LENGTH 18,          "CNPJ
        STCD3            TYPE LFA1-STCD3,
        MAKTX            LIKE J_1BNFLIN-MAKTX,       "Produto
        PSTDAT           LIKE J_1BNFDOC-PSTDAT,     "Data de Operação
        DOCDAT           LIKE J_1BNFDOC-DOCDAT,     "Data de Documento
        MEINS            LIKE J_1BNFLIN-MEINS,
        MENGE            LIKE J_1BNFLIN-MENGE,      "Quantidade
        NETWR            LIKE J_1BNFLIN-NETWR,      "Valor unitario
        NFNETT           LIKE J_1BNFLIN-NFNETT,     "Valor total
        ICMBS            LIKE J_1BNFSTX-BASE,       "Base ICMS
        ICMAL            LIKE J_1BNFSTX-RATE,       "Aliquota ICMS %
        ICMVL            LIKE J_1BNFSTX-TAXVAL,     "Valor de ICMS
        OUTBS            LIKE J_1BNFSTX-OTHBAS,     "Outros
        ISEVL            LIKE J_1BNFSTX-BASE,       "Isentos
        FUNVL            LIKE J_1BNFSTX-BASE,       "Funrural
        PISVL            LIKE J_1BNFSTX-BASE,       "Pis
        COFVL            LIKE J_1BNFSTX-BASE,       "Cofins
        OBSER            LIKE J_1BNFDOC-OBSERVAT,   "Observações
      END OF WA_RELATORIO.

DATA: BEGIN OF WA_NOTA OCCURS 0,
        DOCNUM           LIKE J_1BNFDOC-DOCNUM,
        NFNUM            LIKE J_1BNFDOC-NFNUM,
        SERIES           LIKE J_1BNFDOC-SERIES,
        DOCDAT           LIKE J_1BNFDOC-DOCDAT,
        PSTDAT           LIKE J_1BNFDOC-PSTDAT,
        OBSER            LIKE J_1BNFDOC-OBSERVAT,
        BELNR            LIKE J_1BNFDOC-BELNR,
        PARTYP           LIKE J_1BNFDOC-PARTYP,
        PARID            LIKE J_1BNFDOC-PARID,
        CANCEL           LIKE J_1BNFDOC-CANCEL,
        BRANCH           LIKE J_1BNFDOC-PARID,
      END OF WA_NOTA.

DATA: BEGIN OF WA_ITEM OCCURS 0,
        DOCNUM           LIKE J_1BNFLIN-DOCNUM,
        ITMNUM           LIKE J_1BNFLIN-ITMNUM,
        CFOP             LIKE J_1BNFLIN-CFOP,
        MATNR            LIKE J_1BNFLIN-MATNR,
        MAKTX            LIKE J_1BNFLIN-MAKTX,
        MWSKZ            LIKE J_1BNFLIN-MWSKZ,
        MENGE            LIKE J_1BNFLIN-MENGE,
        NETWR            LIKE J_1BNFLIN-NETWR,
        NFNETT           LIKE J_1BNFLIN-NFNETT,
        NETPR            LIKE J_1BNFLIN-NETPR,
        MEINS            LIKE J_1BNFLIN-MEINS,
      END OF WA_ITEM.

DATA: BEGIN OF WA_IMP OCCURS 0,
        DOCNUM           LIKE J_1BNFSTX-DOCNUM,     "Numero do documento
        ITMNUM           LIKE J_1BNFSTX-ITMNUM,     "Numedo do doc Item
        BASE             LIKE J_1BNFSTX-BASE,       "Valor da base
        RATE             LIKE J_1BNFSTX-RATE,       "Valor da aliquota
        TAXVAL           LIKE J_1BNFSTX-TAXVAL,     "Valor do imposto
        OTHBAS           LIKE J_1BNFSTX-OTHBAS,     "Outras bases
        EXCBAS           LIKE J_1BNFSTX-EXCBAS,
        TAXTYP           LIKE J_1BNFSTX-TAXTYP,
      END OF WA_IMP.

DATA: BEGIN OF WA_FOR OCCURS 0,
        STCD1            LIKE LFA1-STCD1,           "CNPJ
        STCD2            LIKE LFA1-STCD2,
        NAME1            LIKE LFA1-NAME1,           "Nome
        STKZN            LIKE LFA1-STKZN,
        LIFNR            LIKE LFA1-LIFNR,
        STCD3            LIKE LFA1-STCD3,
      END OF WA_FOR.

DATA: BEGIN OF WA_BAA OCCURS 0,
        NFTYPE           LIKE J_1BAA-NFTYPE,
      END OF WA_BAA.

DATA: BEGIN OF WA_TAXICM OCCURS 0,
        TAXTYP           LIKE J_1BNFSTX-TAXTYP,
      END OF WA_TAXICM.

DATA: IT_RELATORIO       LIKE STANDARD TABLE OF WA_RELATORIO,
      IT_NOTA            LIKE STANDARD TABLE OF WA_NOTA,
      IT_NOTAPAR         LIKE STANDARD TABLE OF WA_NOTA,
      IT_ITEM            LIKE STANDARD TABLE OF WA_ITEM,
      IT_IMP             LIKE STANDARD TABLE OF WA_IMP,
      IT_FOR             LIKE STANDARD TABLE OF WA_FOR,
      IT_CLI             LIKE STANDARD TABLE OF WA_FOR,
      IT_BAA             LIKE STANDARD TABLE OF WA_BAA.

FIELD-SYMBOLS <TESTE> LIKE WA_TAXICM.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S04.
PARAMETERS: P_SAIDA      RADIOBUTTON GROUP TP DEFAULT 'X',
            P_ENTRA      RADIOBUTTON GROUP TP.
SELECTION-SCREEN END   OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS: P_BUKRS      LIKE J_1BNFDOC-BUKRS OBLIGATORY. "Empresa
SELECT-OPTIONS:
            S_BRANCH     FOR J_1BNFDOC-BRANCH OBLIGATORY, "Filial
            S_PSTDAT     FOR SY-DATUM         OBLIGATORY, "Periodo
            S_MWSKZ      FOR BSEG-MWSKZ,
            S_CFOP       FOR J_1BNFLIN-CFOP,
            S_MATNR      FOR J_1BNFLIN-MATNR,             "Produto
            S_CRENAM     FOR J_1BNFDOC-CRENAM,
            S_PARID      FOR J_1BNFDOC-PARID,             "Fornecedor/Cliente
            S_DOCNUM     FOR J_1BNFDOC-DOCNUM,            "N° Documento
            S_NFNUM      FOR J_1BNFDOC-NFNUM.             "Nº Nota fiscal
SELECTION-SCREEN END   OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETERS: P_ATIVA      RADIOBUTTON GROUP CAN DEFAULT 'X',
            P_CANCE      RADIOBUTTON GROUP CAN,
            P_ATCA       RADIOBUTTON GROUP CAN.
SELECTION-SCREEN END   OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
PARAMETERS: P_FISIC      RADIOBUTTON GROUP PES,
            P_JURID      RADIOBUTTON GROUP PES,
            P_PJPF       RADIOBUTTON GROUP PES DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B3.
*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
DATA: VL_CANCEL          TYPE J_1BNFDOC-CANCEL.
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
*&      Form  f_monta_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MONTA_ESTRUTURA .
  PERFORM F_FIELDCAT USING:
        '0' 'X' 'IT_RELATORIO' 'BRANCH' 'Filial'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '1' 'X' 'IT_RELATORIO' 'CFOP'   'CFOP'
        04  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '2' 'X' 'IT_RELATORIO' 'DOCNUM' 'Nr Docum.'
        06  ''  ''             '' 'X'
  CHANGING IT_FIELDCAT,
        '3' 'X' 'IT_RELATORIO' 'NFNUM'  'Nr Nota'
        06  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '4' 'X' 'IT_RELATORIO' 'SERIES' 'Serie'
        05  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '5' ''  'IT_RELATORIO' 'NAME1'  'Nome'
        30  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '6' ''  'IT_RELATORIO' 'STCD1'  'CNPJ/CPF'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '7' ''  'IT_RELATORIO' 'STCD3'  'Inscrição'
        20  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '8' ''  'IT_RELATORIO' 'MEINS'  'Unidade'
        25  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
        '9' ''  'IT_RELATORIO' 'MAKTX'  'Produto'
        25  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '10' ''  'IT_RELATORIO' 'DOCDAT' 'Dta.Doc.'
        10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '11' ''  'IT_RELATORIO' 'PSTDAT' 'Dta.Lanc.'
        10  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '12' ''  'IT_RELATORIO' 'MENGE' 'Quantidade'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '13' ''  'IT_RELATORIO' 'NETWR' 'Unitário'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '14' ''  'IT_RELATORIO' 'NFNETT' 'Valor Total'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '15' ''  'IT_RELATORIO' 'ICMBS'  'Base ICMS'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '16' ''  'IT_RELATORIO' 'ICMAL'  'Aliquota %'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '17' ''  'IT_RELATORIO' 'ICMVL'  'Valor ICMS'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '18' ''  'IT_RELATORIO' 'OUTBS'  'Outros'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '19' ''  'IT_RELATORIO' 'ISEVL'  'Isentos'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '20' ''  'IT_RELATORIO' 'PISVL'  'PIS'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '11' ''  'IT_RELATORIO' 'COFVL'  'COFINS'
        16  ''  ''             '' ''
  CHANGING IT_FIELDCAT,
       '22' ''  'IT_RELATORIO' 'OBSER'  'Observação'
        50  ''  ''             '' ''
  CHANGING IT_FIELDCAT.
ENDFORM.                    " f_monta_estrutura
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
ENDFORM.                    " f_executa_alv
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
  DATA: VL_BUTXT         LIKE T001-BUTXT,       "Nome da empresa
        VL_FILIAL1       LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL2       LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL(100)   TYPE C,
        VL_DATA1(10)     TYPE C,
        VL_DATA2(10)     TYPE C,
        VL_DATA(25)      TYPE C.

  CLEAR IT_HEADER.

  IF P_ENTRA EQ 'X'.
    IT_HEADER-TYP  = 'H'.
    IT_HEADER-INFO = 'Conferência de Entrada'.
    APPEND  IT_HEADER.
  ELSE.
    IT_HEADER-TYP  = 'H'.
    IT_HEADER-INFO = 'Conferência de Saídas'.
    APPEND  IT_HEADER.
  ENDIF.

  SELECT SINGLE BUTXT
    FROM T001
    INTO VL_BUTXT
   WHERE BUKRS = P_BUKRS.
  CONCATENATE P_BUKRS
              VL_BUTXT INTO VL_BUTXT
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Empresa'.
  IT_HEADER-INFO = VL_BUTXT.
  APPEND  IT_HEADER.

  SELECT SINGLE NAME
    FROM J_1BBRANCH
    INTO VL_FILIAL1
   WHERE BUKRS  EQ P_BUKRS
     AND BRANCH EQ S_BRANCH-LOW.
  CONCATENATE S_BRANCH-LOW
              VL_FILIAL1 INTO VL_FILIAL1
              SEPARATED BY SPACE.
  IF NOT S_BRANCH-HIGH IS INITIAL.
    SELECT SINGLE NAME
      FROM J_1BBRANCH
      INTO VL_FILIAL2
     WHERE BUKRS  EQ P_BUKRS
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
*&      Form  f_monta_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS .
  DATA: VL_TOTAL         TYPE N,
        VL_UNIT          TYPE N,
        VL_QTDE          TYPE N,
        VL_BRANCH        TYPE C LENGTH 10.

  REFRESH IT_RELATORIO.
  IF NOT IT_NOTA[] IS INITIAL.
    SORT: IT_NOTA BY DOCNUM,
          IT_ITEM BY DOCNUM ITMNUM,
          IT_IMP  BY DOCNUM ITMNUM TAXTYP,
          IT_FOR  BY LIFNR,
          IT_CLI  BY LIFNR.

    LOOP AT IT_NOTA INTO WA_NOTA.
      LOOP AT IT_ITEM INTO WA_ITEM WHERE DOCNUM EQ WA_NOTA-DOCNUM.
        IF S_MATNR IS NOT INITIAL.
          CHECK WA_ITEM-MATNR IN S_MATNR.
        ENDIF.
        IF S_MWSKZ IS NOT INITIAL.
          CHECK WA_ITEM-MWSKZ IN S_MWSKZ.
        ENDIF.
        IF S_CFOP IS NOT INITIAL.
          CHECK WA_ITEM-CFOP IN S_CFOP.
        ENDIF.

        CASE WA_NOTA-PARTYP.
          WHEN 'V'.
            READ TABLE IT_FOR INTO WA_FOR WITH KEY LIFNR = WA_NOTA-PARID
                                                   BINARY SEARCH.
          WHEN 'C'.
            READ TABLE IT_CLI INTO WA_FOR WITH KEY LIFNR = WA_NOTA-PARID
                                                   BINARY SEARCH.
          WHEN 'B'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_NOTA-BRANCH
              IMPORTING
                OUTPUT = VL_BRANCH.

            SELECT SINGLE STCD1 STCD2 NAME1 STKZN LIFNR STCD3
              FROM LFA1
              INTO WA_FOR
             WHERE LIFNR EQ VL_BRANCH.

        ENDCASE.

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
        WA_RELATORIO-STCD3  = WA_FOR-STCD3.
        WA_RELATORIO-NAME1  = WA_FOR-NAME1.        "Nome do fornecedor
        WA_RELATORIO-BRANCH = WA_NOTA-BRANCH.
        WA_RELATORIO-DOCNUM = WA_NOTA-DOCNUM.
        WA_RELATORIO-NFNUM  = WA_NOTA-NFNUM.
        WA_RELATORIO-SERIES = WA_NOTA-SERIES.
        WA_RELATORIO-PSTDAT = WA_NOTA-PSTDAT.
        WA_RELATORIO-DOCDAT = WA_NOTA-DOCDAT.
        WA_RELATORIO-CFOP   = WA_ITEM-CFOP(4).
        WA_RELATORIO-MAKTX  = WA_ITEM-MAKTX.
        IF WA_NOTA-CANCEL IS INITIAL.
          WA_RELATORIO-MEINS = WA_ITEM-MEINS.
          WA_RELATORIO-MENGE = WA_ITEM-MENGE.
          WA_RELATORIO-NETWR  = WA_ITEM-NETPR.
          WA_RELATORIO-NFNETT = WA_ITEM-NETWR.

          CLEAR WA_IMP.
          LOOP AT IT_IMP INTO WA_IMP WHERE DOCNUM EQ WA_ITEM-DOCNUM
                                       AND ITMNUM EQ WA_ITEM-ITMNUM.
            IF 'ICM0 ICM1 ICM2 ICM3 ICMF IFR1 IC1O ICM4 ICMN ICMO ICMX' CS WA_IMP-TAXTYP.
              WA_RELATORIO-ICMBS  = WA_IMP-BASE.
              WA_RELATORIO-ICMAL  = WA_IMP-RATE.
              WA_RELATORIO-ICMVL  = WA_IMP-TAXVAL.
              WA_RELATORIO-OUTBS  = WA_IMP-OTHBAS.
              WA_RELATORIO-ISEVL  = WA_IMP-EXCBAS.
              DELETE IT_IMP WHERE DOCNUM EQ WA_IMP-DOCNUM
                              AND ITMNUM EQ WA_IMP-ITMNUM
                              AND TAXTYP EQ WA_IMP-TAXTYP.
            ENDIF.
            IF 'IPIS IPS3 IPSA IPSN IPSO IPSS IPSU IPSW IPSV IPSZ IPW3' CS WA_IMP-TAXTYP.
              WA_RELATORIO-PISVL  = WA_IMP-TAXVAL.
            ENDIF.
            IF 'ICN3 ICOA ICOF ICON ICOS ICOU ICOV ICOW ICOZ ICW3 ICOO' CS WA_IMP-TAXTYP.
              WA_RELATORIO-COFVL  = WA_IMP-TAXVAL.
            ENDIF.
          ENDLOOP.
          WA_RELATORIO-OBSER = WA_NOTA-OBSER.
        ELSE.
          WA_RELATORIO-OBSER = 'Cancelada.'.
        ENDIF.

        IF P_FISIC EQ 'X'.
          IF WA_FOR-STKZN NE SPACE.
            APPEND WA_RELATORIO TO IT_RELATORIO.
          ENDIF.
        ENDIF.
        IF P_JURID EQ 'X'.
          IF WA_FOR-STKZN EQ SPACE.
            APPEND WA_RELATORIO TO IT_RELATORIO.
          ENDIF.
        ENDIF.
        IF P_FISIC EQ 'X'.
          IF WA_FOR-STKZN NE SPACE.
            APPEND WA_RELATORIO TO IT_RELATORIO.
          ENDIF.
        ENDIF.
        IF P_PJPF EQ 'X'.
          APPEND WA_RELATORIO TO IT_RELATORIO.
        ENDIF.
        CLEAR WA_RELATORIO.
      ENDLOOP.
    ENDLOOP.
  ELSE.
    MESSAGE I000 WITH 'Não existe informação para esta seleção'.
    STOP.
  ENDIF.
ENDFORM.                    " f_monta_dados
*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.
  DATA VL_TIPO             TYPE C LENGTH 1.

  IF P_ENTRA EQ 'X'.
    VL_TIPO = '1'.
  ELSE.
    VL_TIPO = '2'.
  ENDIF.

  IF P_ATIVA EQ 'X'. "Lista somente as notas ativas
    VL_CANCEL = 'X'.
  ENDIF.
  IF P_CANCE EQ 'X'. "Somente as notas canceladas
    VL_CANCEL = SPACE.
  ENDIF.
  IF P_ATCA EQ 'X'.  "Todas as notas
    VL_CANCEL = '9'.
  ENDIF.

* Busca tipos de nota (Entrada/Saida)
  SELECT NFTYPE
    FROM J_1BAA
    INTO TABLE IT_BAA
   WHERE DIRECT EQ VL_TIPO.

* Montar tabela interna de documentos com os dados conforme paramentros
  SELECT DOCNUM NFNUM  SERIES DOCDAT PSTDAT OBSERVAT
         BELNR  PARTYP PARID  CANCEL BRANCH
    FROM J_1BNFDOC
    INTO TABLE IT_NOTA
     FOR ALL ENTRIES IN IT_BAA
   WHERE BUKRS  EQ P_BUKRS
     AND BRANCH IN S_BRANCH
     AND PSTDAT IN S_PSTDAT
     AND PARID  IN S_PARID
     AND DOCNUM IN S_DOCNUM
     AND NFNUM  IN S_NFNUM
     AND CRENAM IN S_CRENAM
     AND CANCEL NE VL_CANCEL
     AND NFTYPE EQ IT_BAA-NFTYPE
     AND DOCREF EQ 0.

* Montar tabela interna com todos os dados dos itens das notas
  SELECT DOCNUM ITMNUM CFOP MATNR MAKTX MWSKZ MENGE NETWR NFNETT NETPR MEINS
    FROM J_1BNFLIN
    INTO TABLE IT_ITEM
     FOR ALL ENTRIES IN IT_NOTA
   WHERE DOCNUM = IT_NOTA-DOCNUM.

  REFRESH IT_FOR.
  SELECT STCD1 STCD2 NAME1 STKZN LIFNR STCD3
    FROM LFA1
    INTO TABLE IT_FOR
     FOR ALL ENTRIES IN IT_NOTA
   WHERE LIFNR EQ IT_NOTA-PARID.

  REFRESH IT_CLI.
  SELECT STCD1 STCD2 NAME1 STKZN KUNNR STCD3
    FROM KNA1
    INTO TABLE IT_CLI
     FOR ALL ENTRIES IN IT_NOTA
   WHERE KUNNR EQ IT_NOTA-PARID.

* Monta tabela interna de impostos conforme dados do doc. fiscal
  SELECT DOCNUM ITMNUM BASE RATE TAXVAL OTHBAS EXCBAS TAXTYP
    FROM J_1BNFSTX
    INTO TABLE IT_IMP
     FOR ALL ENTRIES IN IT_ITEM
   WHERE DOCNUM EQ IT_ITEM-DOCNUM
     AND ITMNUM EQ IT_ITEM-ITMNUM.

ENDFORM.                    " f_seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*     Form Para Fazer o cabeçalho   no ALV                             *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
*    i_logo             = c_logo
    IT_LIST_COMMENTARY = IT_HEADER[].
  SET TITLEBAR 'INI2'.
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING UCOMM LIKE SY-UCOMM
      SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_RELATORIO INTO WA_RELATORIO INDEX SELFIELD-TABINDEX.
  IF SELFIELD-FIELDNAME = 'DOCNUM'.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'J1B3N'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'J1B3N'.
    ENDIF.
    SET PARAMETER ID: 'JEF' FIELD WA_RELATORIO-DOCNUM.
    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    "user_command
