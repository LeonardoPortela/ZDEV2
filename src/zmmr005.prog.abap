************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...:                                                     *
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 03.10.2007                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Listagem dos impostos recuperáveis PIS e COFINS     *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 03.10.2007    Michely                                   DEVK902841   *
* 19.10.2007    Michely              Correçoes            DEVK902957   *
*    Motivo: Não estava correto o PIS, estaca errado o grupo           *
* 19.10.2007    Michely              Correções            DEVK902961   *
*    Motivo: Listando todos os documentos fiscais, deve listar somente *
*            documentos com valores em PIS ou COFINS                   *
* 25.10.2007   Michely               Correções            DEVK902981   *
*    Motivo: Valor total de algumas notas não esta mostrando, utilizar *
*            funções para buscar dados da nota.                        *
************************************************************************

report ZMMR005
               no standard page heading    "Não exibe cabeçalho standard
               message-id Z01
               line-size 076               "Comprimento da Linha
               line-count 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
tables: J_1BNFDOC,    "Cabecalho da Nota Fiscal
        J_1BNFLIN,    "Partidas Individuais da Nota Fiscal
        LFA1,         "Mestre de Fornecedores
        KNA1,         "Mestre de Cliente
        T001,         "Empresas
        J_1BBRANCH.   "Filial



*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
define FIELDCAT.
  clear WA_FIELDCAT.
  if ( &1 eq 'MARK' ).
    WA_FIELDCAT-CHECKBOX      = C_MARK.
    WA_FIELDCAT-EDIT          = C_MARK.
  endif.
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
  append WA_FIELDCAT to IT_FIELDCAT.
end-of-definition.

data: begin of WA_BKPF occurs 0.
        include structure BKPF.
data: end of WA_BKPF.

data: begin of WA_BSEG occurs 0.
        include structure BSEG.
data: end of WA_BSEG.

* Nota Fiscal - Estrutura de itens
data: begin of WA_ITEM_I occurs 0.
        include structure J_1BNFLIN.
data: end of WA_ITEM_I.

* Nota Fiscal - Estrutura de itens
data: begin of WA_ITEM_E occurs 0.
        include structure J_1BINLIN.
data: end of WA_ITEM_E.

* Nota Fiscal - Estrutura de itens
data: begin of WA_IMP_T occurs 0.
        include structure J_1BNFSTX.
data: end of WA_IMP_T.

data: IT_BKPF   like standard table of WA_BKPF,
      IT_BSEG   like standard table of WA_BSEG,
      IT_IMP_T  like standard table of WA_IMP_T,
      IT_ITEM_I like standard table of WA_ITEM_I.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
*Usado para criação de ALV
type-pools: SLIS,
            KKBLO.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: IT_FIELDCAT      type SLIS_T_FIELDCAT_ALV, "Estrutura de saida
      IT_HEADER        type KKBLO_T_LISTHEADER with header line,   "Cabeçalho
      IT_EVENT         type SLIS_T_EVENT       with header line,   "Eventos
      IT_SORT          type SLIS_SORTINFO_ALV,
      IT_ALV_SORT      type SLIS_T_SORTINFO_ALV,
      VG_LISTHEADER    type SLIS_T_LISTHEADER,
      VG_LAYOUT        type SLIS_LAYOUT_ALV,   "Layout do alv
      VG_VARIANT       type DISVARIANT,        "Variantes de saida
      VG_GRID_SETTINGS type LVC_S_GLAY,        "Config p/Grid
      VG_REPID         type SY-REPID,
      ST_HEADER        type KKBLO_LISTHEADER.


data: begin of WA_RELATORIO occurs 0,
        BRANCH   like J_1BNFDOC-BRANCH,     "Filial
        PSTDAT   like J_1BNFDOC-PSTDAT,     "Data de Operação
        DOCDAT   like J_1BNFDOC-DOCDAT,     "Data do Documento
        DOCNUM   like J_1BNFDOC-DOCNUM,     "Nr. Documento
        BELNR    like BKPF-BELNR,           "Nr. da fatura
        STCD1    type C length 18,           "CNPJ
        NAME1    like LFA1-NAME1,           "Nome do fornecedor
        MAKTX    like MAKT-MAKTX,           "Descrição do Produto
        NFTOT    like J_1BNFLIN-NFNETT,      "Valor da Nota
        VLPIS    like J_1BNFSTX-BASE,       "Valor de PIS
        VLCOFINS like J_1BNFSTX-BASE,       "Valor de COFINS
        GJAHR    like BKPF-GJAHR,
        NFNUM    like J_1BNFDOC-NFNUM,
        MARK,
      end of WA_RELATORIO.

data: begin of WA_DOC occurs 0,
        BRANCH like J_1BNFDOC-BRANCH,     "Filial
        PSTDAT like J_1BNFDOC-PSTDAT,     "Data de Operação
        DOCDAT like J_1BNFDOC-DOCDAT,     "Data do Documento
        DOCNUM like J_1BNFDOC-DOCNUM,     "Nr. Documento
        PARID  like J_1BNFDOC-PARID,      "Fornecedor
        MATNR  like J_1BNFLIN-MATNR,      "Produto
        ITMNUM like J_1BNFLIN-ITMNUM,     "Numero do doc. item
        NFNET  like J_1BNFLIN-NFNET,
        PARTYP like J_1BNFDOC-PARTYP,     "Tipo de parceiro
        REFKEY like BKPF-AWKEY,           "Referencia ao lncto ctb.
        BELNR  like J_1BNFDOC-BELNR,
        GJAHR  like J_1BNFDOC-GJAHR,
        BUKRS  like J_1BNFDOC-BUKRS,
        MAKTX  like J_1BNFLIN-MAKTX,
        CANCEL like J_1BNFDOC-CANCEL,
        NFNUM  type C length 16, "like j_1bnfdoc-nfnum,
        NFNETT like J_1BNFLIN-NFNETT,
      end of WA_DOC.

data: begin of WA_FOR occurs 0,
        LIFNR like LFA1-LIFNR,           "Fornecedor ID
        STCD1 like LFA1-STCD1,           "CNPJ
        STCD2 like LFA1-STCD2,           "CPF
        STKZN like LFA1-STKZN,           "Pessoa fisica (X)
        NAME1 like LFA1-NAME1,           "Nome
      end of WA_FOR.

data: begin of WA_IMP occurs 0,
        DOCNUM like J_1BNFSTX-DOCNUM,     "Numero do documento
        ITMNUM like J_1BNFSTX-ITMNUM,     "Numedo do doc Item
        BASE   like J_1BNFSTX-BASE,       "Valor da base
        RATE   like J_1BNFSTX-RATE,       "Valor da aliquota
        TAXVAL like J_1BNFSTX-TAXVAL,     "Valor do imposto
        TAXGRP like J_1BAJ-TAXGRP,        "Tipo do imposto
        OTHBAS like J_1BNFSTX-OTHBAS,     "Outras bases
      end of WA_IMP.

data: begin of WA_ITEM occurs 0,
        MATNR like MAKT-MATNR,           "N° Produto
        MAKTX like MAKT-MAKTX,           "Descrição
      end of WA_ITEM.

data: begin of WA_VLD_BKP occurs 0,
        BELNR like J_1BNFDOC-BELNR,
        GJAHR like J_1BNFDOC-GJAHR,
      end of WA_VLD_BKP.

data: IT_RELATORIO like standard table of WA_RELATORIO,
      IT_DOC       like standard table of WA_DOC,
      IT_FOR       like standard table of WA_FOR,
      IT_ITEM      like standard table of WA_ITEM,
      IT_IMP       like standard table of WA_IMP,
      IT_VLD_BKP   like standard table of WA_VLD_BKP,
      R_IVA        like range of J_1BNFLIN-MWSKZ with header line. " Exemplo zfib005
*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
data: VG_DATA   type C,                   "Data atual
      VG_TITULO type C,                   "Titulo
      VG_ANO    type C,                   "Ano
      VG_HORA   type C.                   "Hora

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
constants: C_MARK     type C length 1 value 'X',
           C_PONTO(1) type C          value '.',
           C_SPACE(3) type C          value '   '.
*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
selection-screen begin of block B1 with frame title text-S01.
  parameters: S_BUKRS      like J_1BNFDOC-BUKRS obligatory. "Empresa
  select-options:
         S_BRANCH          for J_1BNFDOC-BRANCH obligatory, "Filial
         S_PSTDAT          for SY-DATUM         obligatory, "Periodo
         S_MATNR           for J_1BNFLIN-MATNR,             "Produto
         S_PARID           for J_1BNFDOC-PARID,             "Fornecedor/Cliente
         S_DOCNUM          for J_1BNFDOC-DOCNUM,            "N° Nota
         S_CRENAM          for J_1BNFDOC-CRENAM.            "Criado por
selection-screen end   of block B1.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
at selection-screen.

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
initialization.
  set titlebar 'INI'.

  refresh R_IVA.
  clear R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'BT'.
  R_IVA-LOW = 'P1'.
  R_IVA-HIGH = 'P4'.
  append R_IVA.

*----------------------------------------------------------------------*
* Definição Macros                                                     *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
* Selecionar os dados
  perform F_SELECIONA_DADOS.
* Montar dados selecionados para impressão
  perform F_MONTA_DADOS.
* Montar o cabeçalho da alv
  perform F_MONTA_CABECALHO.
* Montar estrutura de dados do alv
  perform F_MONTA_ESTRUTURA.
* Monta os valores totais de quebra
  perform F_MONTA_SUB_TOTAIS.
* Executar o alv para Background e foreground
  perform F_EXECUTA_ALV.

end-of-selection.

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
form F_SELECIONA_DADOS.
  clear: IT_DOC[], IT_ITEM[].
* Montar tabela interna de documentos com os dados conforme paramentros
  select NF~BRANCH NF~PSTDAT NF~DOCDAT NF~DOCNUM NF~PARID
         IT~MATNR  IT~ITMNUM IT~NETWR  NF~PARTYP IT~REFKEY
         NF~BELNR  NF~GJAHR  NF~BUKRS  IT~MAKTX  NF~CANCEL
         NF~NFNUM
    from J_1BNFDOC       as NF
    inner join J_1BNFLIN as IT
       on IT~DOCNUM eq NF~DOCNUM
      and IT~MANDT  eq NF~MANDT
    into table IT_DOC
    where ( NF~BUKRS  eq S_BUKRS  )
      and ( NF~BRANCH in S_BRANCH )
      and ( NF~PSTDAT in S_PSTDAT )
      and ( IT~MATNR  in S_MATNR  )
      and ( NF~PARID  in S_PARID  )
      and ( NF~DOCNUM in S_DOCNUM )
      and ( NF~CRENAM in S_CRENAM )
      and ( NF~CANCEL ne 'X' )
      and ( IT~MWSKZ  not in R_IVA ).

  select NF~BELNR NF~GJAHR
    from J_1BNFDOC       as NF
    inner join J_1BNFSTX as IP
       on NF~DOCNUM eq IP~DOCNUM
    into table IT_VLD_BKP
    for all entries in IT_DOC
    where NF~DOCNUM eq IT_DOC-DOCNUM.

* Montar lançamentos que nao tem nota fiscal relacionada.
  select *
    from BKPF
    into table IT_BKPF
  for all entries in IT_VLD_BKP
   where GJAHR eq IT_VLD_BKP-GJAHR
     and BUKRS eq S_BUKRS
     and BRNCH eq S_BRANCH
     and BUDAT in S_PSTDAT
     and BELNR ne IT_VLD_BKP-BELNR
     and STBLG is null.

* Monta tabela interna de produtos conforme dados do doc. fiscal
  select MATNR MAKTX
    from MAKT
    into table IT_ITEM
  for all entries in IT_DOC
             where MATNR eq IT_DOC-MATNR.

* Monta tabela interna de impostos conforme dados do doc. fiscal
  select IM~DOCNUM IM~ITMNUM IM~BASE IM~RATE IM~TAXVAL GR~TAXGRP
    from J_1BNFSTX  as IM
  inner join J_1BAJ as GR
     on GR~MANDT  eq IM~MANDT
    and GR~TAXTYP eq IM~TAXTYP
    into table IT_IMP
  for all entries in IT_DOC
             where IM~DOCNUM eq IT_DOC-DOCNUM
               and IM~ITMNUM eq IT_DOC-ITMNUM
               and ( ( GR~TAXGRP eq 'PIS' ) or ( GR~TAXGRP eq 'COFI' ) ).

endform.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  f_monta_estrutura
*&---------------------------------------------------------------------*
*       Montar estrutura de dados do alv
*----------------------------------------------------------------------*
form F_MONTA_ESTRUTURA .
  perform F_FIELDCAT using:
       '0' 'X' 'IT_RELATORIO' 'BRANCH' 'Filial'
       04  ''  ''             '' '' ''
   changing IT_FIELDCAT,
       '1' 'X' 'IT_RELATORIO' 'PSTDAT' 'Data Oper.'
       10  ''  ''             '' '' ''
   changing IT_FIELDCAT,
       '2' 'X' 'IT_RELATORIO' 'DOCDAT' 'Data Doc.'
       10  ''  ''             '' '' ''
   changing IT_FIELDCAT,
       '3' 'X' 'IT_RELATORIO' 'DOCNUM' 'Nr Docum.'
       06  ''  ''             '' '' ''
  changing IT_FIELDCAT,
       '4' 'X' 'IT_RELATORIO' 'NFNUM'  'Nr Nota'
       06  ''  ''             '' '' ''
  changing IT_FIELDCAT,
       '5' 'X' 'IT_RELATORIO' 'BELNR'  'Nr Doc.Ctb.'
       10  ''  ''             '' 'X' ''
   changing IT_FIELDCAT,
       '6' ''  'IT_RELATORIO' 'STCD1'  'CNPJ/CPF'
       18  ''  ''             '' ''  ''
   changing IT_FIELDCAT,
       '7' ''  'IT_RELATORIO' 'NAME1'  'Nome do Fornecedor'
       30  ''  ''             '' ''  ''
   changing IT_FIELDCAT,
       '8' ''  'IT_RELATORIO' 'MAKTX'  'Produto'
       30  ''  ''             '' ''  ''
   changing IT_FIELDCAT,
       '9' ''  'IT_RELATORIO' 'NFTOT'  'Valor Nota'
       14  ''  ''             '' ''  'X'
   changing IT_FIELDCAT,
      '10' ''  'IT_RELATORIO' 'VLPIS'  'Valor PIS'
       14  ''  ''             '' ''  'X'
   changing IT_FIELDCAT,
      '11' ''  'IT_RELATORIO' 'VLCOFINS' 'Valor COFINS'
       14  ''  ''             '' ''  'X'
   changing IT_FIELDCAT.
endform.                    " f_monta_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       Executar o alv para Background e foreground
*----------------------------------------------------------------------*
form F_EXECUTA_ALV .
* Variavel Local
  data: VL_REPID like SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  append IT_EVENT.

*  it_sort-spos = 4.
*  it_sort-fieldname = slis_fieldname-fieldname.
*  it_sort-up = 'X'.
*  it_sort-subtot = 'X'.
*  append it_sort.

  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      I_CALLBACK_PROGRAM      = VL_REPID
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = VG_LAYOUT
*     i_background_id         = c_enjoy
      IT_FIELDCAT             = IT_FIELDCAT[]
      IT_SORT                 = IT_ALV_SORT[]
      I_DEFAULT               = 'A'
      I_SAVE                  = 'X'
      IT_EVENTS               = IT_EVENT[]
    tables
      T_OUTTAB                = IT_RELATORIO
    exceptions
      PROGRAM_ERROR           = 1
      others                  = 2.
  if SY-SUBRC <> 0.
    message id SY-MSGID type SY-MSGTY number SY-MSGNO
    with SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " f_executa_alv
*&---------------------------------------------------------------------*
*&      Form  f_monta_dados
*&---------------------------------------------------------------------*
*       Montar dados selecionados para impressão
*----------------------------------------------------------------------*
form F_MONTA_DADOS .
*  data: vl_bas           like rseg-RMWWR.

  clear: IT_RELATORIO[].
  if not IT_DOC[] is initial.
    sort: IT_DOC  by DOCNUM PARID ITMNUM MATNR,
          IT_ITEM by MATNR,
          IT_IMP  by DOCNUM ITMNUM TAXGRP.
    clear WA_DOC .
    loop at IT_DOC into WA_DOC.
      clear: WA_ITEM, WA_FOR.
      WA_RELATORIO-BRANCH = WA_DOC-BRANCH.
      WA_RELATORIO-PSTDAT = WA_DOC-PSTDAT.      "Data de Operação
      WA_RELATORIO-DOCDAT = WA_DOC-DOCDAT.      "Data do Documento
      WA_RELATORIO-DOCNUM = WA_DOC-DOCNUM.      "Nr. Documento
      WA_RELATORIO-BELNR  = WA_DOC-REFKEY.
      WA_RELATORIO-GJAHR  = WA_DOC-GJAHR.
      WA_RELATORIO-NFNUM  = WA_DOC-NFNUM.

      clear WA_FOR.
      if WA_DOC-PARTYP eq 'C'.
        select single KUNNR STCD1 STCD2 STKZN NAME1
          from KNA1
        into WA_FOR
        where KUNNR eq WA_DOC-PARID.
      endif.
      if WA_DOC-PARTYP eq 'V'.
        select single LIFNR STCD1 STCD2 STKZN NAME1
        from LFA1
        into WA_FOR
        where LIFNR eq WA_DOC-PARID.
      endif.

      if WA_FOR-STKZN eq SPACE.
        call function 'CONVERSION_EXIT_CGCBR_OUTPUT'
          exporting
            INPUT  = WA_FOR-STCD1
          importing
            OUTPUT = WA_RELATORIO-STCD1.
      else.
        call function 'CONVERSION_EXIT_CPFBR_OUTPUT'
          exporting
            INPUT  = WA_FOR-STCD2
          importing
            OUTPUT = WA_RELATORIO-STCD1.
        WA_RELATORIO-STCD1 = WA_RELATORIO-STCD1(14).
      endif.
      WA_RELATORIO-NAME1 = WA_FOR-NAME1.        "Nome do fornecedor

      select single *
        from J_1BNFLIN
        into WA_ITEM_I
       where DOCNUM = WA_DOC-DOCNUM
         and ITMNUM = WA_DOC-ITMNUM.

      call function 'J_1B_NF_VALUE_DETERMINATION_I'
        exporting
          NF_ITEM     = WA_ITEM_I
        importing
          EXT_ITEM    = WA_ITEM_E
        tables
          NF_ITEM_TAX = IT_IMP_T.

*
*      call function 'J_1B_NF_VALUE_DETERMINATION_I'
*        exporting
*          ext_item    = wa_item_e
*        importing
*          ext_item    = it_item_i
*        tables
*          nf_item_tax = it_imp_t.



      WA_RELATORIO-NFTOT = WA_ITEM_E-NFTOT.        "Valor da Nota

      read table IT_IMP into WA_IMP with key DOCNUM = WA_DOC-DOCNUM
                                             ITMNUM = WA_DOC-ITMNUM
                                             TAXGRP = 'PIS'
                                             binary search.

      WA_RELATORIO-VLPIS = WA_IMP-TAXVAL.
      clear WA_IMP.
      read table IT_IMP into WA_IMP with key DOCNUM = WA_DOC-DOCNUM
                                             ITMNUM = WA_DOC-ITMNUM
                                             TAXGRP = 'COFI'
                                             binary search.
      WA_RELATORIO-VLCOFINS = WA_IMP-TAXVAL.
      clear WA_IMP.
      if ( WA_RELATORIO-VLPIS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*        select single dmbtr
*          from bseg
*          into wa_relatorio-vlpis
*         where bukrs eq wa_doc-bukrs
*           and belnr eq wa_doc-belnr
*           and gjahr eq wa_doc-gjahr
*           and ( ( altkt eq '0000113215' ) or ( altkt eq '0000113214' ) ).

        data:   LT_BSEG type FAGL_T_BSEG.

        call function 'FAGL_GET_BSEG'
          exporting
            I_BUKRS   = WA_DOC-BUKRS
            I_BELNR   = WA_DOC-BELNR
            I_GJAHR   = WA_DOC-GJAHR
          importing
            ET_BSEG   = LT_BSEG
          exceptions
            NOT_FOUND = 1
            others    = 2.

        delete LT_BSEG where ALTKT ne '0000113215'  or  ALTKT ne '0000113214'.

        read table LT_BSEG into data(LS_BSEG) index 1.
        if SY-SUBRC = 0.
          WA_RELATORIO-VLPIS = conv #( LS_BSEG-DMBTR ).
        endif.
*<--- S4 Migration - 19/06/2023 - MA
      endif.
      if ( WA_RELATORIO-VLCOFINS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*        select single DMBTR
*          from BSEG
*          into WA_RELATORIO-VLCOFINS
*         where BUKRS eq WA_DOC-BUKRS
*           and BELNR eq WA_DOC-BELNR
*           and GJAHR eq WA_DOC-GJAHR
*           and ( ( ALTKT eq '0000113216' ) or ( ALTKT eq '0000113217' ) ).

        call function 'FAGL_GET_BSEG'
          exporting
            I_BUKRS   = WA_DOC-BUKRS
            I_BELNR   = WA_DOC-BELNR
            I_GJAHR   = WA_DOC-GJAHR
          importing
            ET_BSEG   = LT_BSEG
          exceptions
            NOT_FOUND = 1
            others    = 2.

        delete LT_BSEG where ALTKT ne '0000113216'  or  ALTKT ne '0000113217'.

        read table LT_BSEG into LS_BSEG index 1.
        if SY-SUBRC = 0.
          WA_RELATORIO-VLCOFINS = conv #( LS_BSEG-DMBTR ).
        endif.
*<--- S4 Migration - 16/06/2023 - MA
      endif.

      read table IT_ITEM into WA_ITEM with key MATNR  = WA_DOC-MATNR
                                               binary search.
      if WA_DOC-MATNR is initial.
        if WA_DOC-MAKTX is initial.
          WA_RELATORIO-MAKTX = '*SERVIÇO*'.
        else.
          WA_RELATORIO-MAKTX = WA_DOC-MAKTX.
        endif.
      else.
        WA_RELATORIO-MAKTX = WA_ITEM-MAKTX.       "Descrição do Produto
      endif.

      if ( WA_RELATORIO-VLPIS ne 0 ) or ( WA_RELATORIO-VLCOFINS ne 0 ).
        append WA_RELATORIO to IT_RELATORIO.
      endif.
      clear WA_RELATORIO.
    endloop.
  endif.
  if not IT_BKPF is initial.
    loop at IT_BKPF into WA_BKPF.
      clear: WA_ITEM, WA_FOR.
      WA_RELATORIO-BRANCH = WA_BKPF-BRNCH.
      WA_RELATORIO-PSTDAT = WA_BKPF-BUDAT.      "Data de Operação
      WA_RELATORIO-DOCDAT = WA_BKPF-BLDAT.      "Data do Documento
*      wa_relatorio-docnum = 'XXX'.      "Nr. Documento
      WA_RELATORIO-BELNR  = WA_BKPF-AWKEY(10).
      WA_RELATORIO-GJAHR  = WA_BKPF-GJAHR.
      WA_RELATORIO-NFNUM  = WA_BKPF-XBLNR.
* ---> S4 Migration - 19/06/2023 - MA
*      select single *
*        from BSEG
*        into WA_BSEG
*       where BUKRS eq WA_BKPF-BUKRS
*         and BELNR eq WA_BKPF-BELNR
*         and GJAHR eq WA_BKPF-GJAHR.

      call function 'FAGL_GET_BSEG'
        exporting
          I_BUKRS   = WA_BKPF-BUKRS
          I_BELNR   = WA_BKPF-BELNR
          I_GJAHR   = WA_BKPF-GJAHR
        importing
          ET_BSEG   = LT_BSEG
        exceptions
          NOT_FOUND = 1
          others    = 2.

      read table LT_BSEG into LS_BSEG index 1.
      if SY-SUBRC = 0.
        move-corresponding LS_BSEG to WA_BSEG.
      endif.
*<--- S4 Migration - 16/06/2023 - MA



      clear WA_FOR.
      if not WA_BSEG-KUNNR is initial.
        select single KUNNR STCD1 STCD2 STKZN NAME1
          from KNA1
          into WA_FOR
         where KUNNR eq WA_BSEG-KUNNR.
      endif.
      if not WA_BSEG-LIFNR is initial.
        select single LIFNR STCD1 STCD2 STKZN NAME1
          from LFA1
          into WA_FOR
         where LIFNR eq WA_BSEG-LIFNR.
      endif.

      if WA_FOR-STKZN eq SPACE.
        call function 'CONVERSION_EXIT_CGCBR_OUTPUT'
          exporting
            INPUT  = WA_FOR-STCD1
          importing
            OUTPUT = WA_RELATORIO-STCD1.
      else.
        call function 'CONVERSION_EXIT_CPFBR_OUTPUT'
          exporting
            INPUT  = WA_FOR-STCD2
          importing
            OUTPUT = WA_RELATORIO-STCD1.
        WA_RELATORIO-STCD1 = WA_RELATORIO-STCD1(14).
      endif.
      WA_RELATORIO-NAME1 = WA_FOR-NAME1.        "Nome do fornecedor

      select single RMWWR
        from RBKP
        into WA_RELATORIO-NFTOT
       where BELNR eq WA_BKPF-AWKEY(10)
         and GJAHR eq WA_BKPF-GJAHR.
*      wa_relatorio-nftot = wa_doc-nfnet.        "Valor da Nota

      clear WA_IMP.
      if WA_BKPF-BLART eq 'AF'.
        if ( WA_RELATORIO-VLPIS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*          select single DMBTR
*            from BSEG
*            into WA_RELATORIO-VLPIS
*           where BUKRS eq WA_BSEG-BUKRS
*             and BELNR eq WA_BSEG-BELNR
*             and GJAHR eq WA_BSEG-GJAHR
*             and ( ALTKT eq '0000121402' ).

*        data:   LT_BSEG type FAGL_T_BSEG.

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS   = WA_BSEG-BUKRS
              I_BELNR   = WA_BSEG-BELNR
              I_GJAHR   = WA_BSEG-GJAHR
            importing
              ET_BSEG   = LT_BSEG
            exceptions
              NOT_FOUND = 1
              others    = 2.

          delete LT_BSEG where ALTKT ne '0000121402'.

          read table LT_BSEG into LS_BSEG index 1.
          if SY-SUBRC = 0.
            WA_RELATORIO-VLPIS = conv #( LS_BSEG-DMBTR ).
          endif.
*<--- S4 Migration - 16/06/2023 - MA

        endif.
        if ( WA_RELATORIO-VLCOFINS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*          select single DMBTR
*            from BSEG
*           into WA_RELATORIO-VLCOFINS
*          where BUKRS eq WA_BSEG-BUKRS
*            and BELNR eq WA_BSEG-BELNR
*            and GJAHR eq WA_BSEG-GJAHR
*            and ( ALTKT eq '0000121403' ).

*        data:   LT_BSEG type FAGL_T_BSEG.

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS   = WA_BSEG-BUKRS
              I_BELNR   = WA_BSEG-BELNR
              I_GJAHR   = WA_BSEG-GJAHR
            importing
              ET_BSEG   = LT_BSEG
            exceptions
              NOT_FOUND = 1
              others    = 2.

          delete LT_BSEG where ALTKT ne '0000121403'.

          read table LT_BSEG into LS_BSEG index 1.
          if SY-SUBRC = 0.
            WA_RELATORIO-VLCOFINS = conv #( LS_BSEG-DMBTR ).
          endif.
*<--- S4 Migration - 16/06/2023 - MA
        endif.
      else.
        if ( WA_RELATORIO-VLPIS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*          select single DMBTR
*            from BSEG
*            into WA_RELATORIO-VLPIS
*           where BUKRS eq WA_BSEG-BUKRS
*             and BELNR eq WA_BSEG-BELNR
*             and GJAHR eq WA_BSEG-GJAHR
*             and ( ( ALTKT eq '0000113215' ) or ( ALTKT eq '0000113214' ) ).

*        data:   LT_BSEG type FAGL_T_BSEG.

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS   = WA_BSEG-BUKRS
              I_BELNR   = WA_BSEG-BELNR
              I_GJAHR   = WA_BSEG-GJAHR
            importing
              ET_BSEG   = LT_BSEG
            exceptions
              NOT_FOUND = 1
              others    = 2.

          delete LT_BSEG where ALTKT ne '0000113215' or ALTKT ne '0000113214'.

          read table LT_BSEG into LS_BSEG index 1.
          if SY-SUBRC = 0.
            WA_RELATORIO-VLPIS = conv #( LS_BSEG-DMBTR ).
          endif.
*<--- S4 Migration - 16/06/2023 - MA
        endif.
        if ( WA_RELATORIO-VLCOFINS eq 0 ).
* ---> S4 Migration - 19/06/2023 - MA
*          select single DMBTR
*            from BSEG
*            into WA_RELATORIO-VLCOFINS
*           where BUKRS eq WA_BSEG-BUKRS
*             and BELNR eq WA_BSEG-BELNR
*             and GJAHR eq WA_BSEG-GJAHR
*             and ( ( ALTKT eq '0000113216' ) or ( ALTKT eq '0000113217' ) ).

*        data:   LT_BSEG type FAGL_T_BSEG.

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS   = WA_BSEG-BUKRS
              I_BELNR   = WA_BSEG-BELNR
              I_GJAHR   = WA_BSEG-GJAHR
            importing
              ET_BSEG   = LT_BSEG
            exceptions
              NOT_FOUND = 1
              others    = 2.

          delete LT_BSEG where ALTKT ne '0000113216' or ALTKT ne '0000113217'.

          read table LT_BSEG into LS_BSEG index 1.
          if SY-SUBRC = 0.
            WA_RELATORIO-VLCOFINS = conv #( LS_BSEG-DMBTR ).
          endif.
*<--- S4 Migration - 16/06/2023 - MA
        endif.
      endif.

      read table IT_ITEM into WA_ITEM with key MATNR  = WA_BSEG-MATNR
                                               binary search.
      if WA_BSEG-MATNR is initial.
        WA_RELATORIO-MAKTX = '*SERVIÇO*'.
      else.
        WA_RELATORIO-MAKTX = WA_ITEM-MAKTX.       "Descrição do Produto
      endif.

      if ( WA_RELATORIO-VLPIS ne 0 ) or ( WA_RELATORIO-VLCOFINS ne 0 ).
        append WA_RELATORIO to IT_RELATORIO.
      endif.
      clear WA_RELATORIO.
    endloop.
  endif.
  if IT_RELATORIO[] is initial.
    message I000 with 'Não existe informação para esta seleção'.
    stop.
  endif.
  sort IT_RELATORIO by BRANCH DOCNUM PSTDAT MAKTX.
endform.                    " f_monta_dados

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
form F_FIELDCAT using P_CONT P_KEY  P_TAB  P_FIELD P_DESC
      P_TAM  P_QTDE P_FIX  P_JUST P_HOT P_SUM
changing P_FIELDCAT type SLIS_T_FIELDCAT_ALV.

* Tabela interna local
  data: TL_FIELDCAT type SLIS_T_FIELDCAT_ALV with header line.

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
  TL_FIELDCAT-DO_SUM     = P_SUM.
  append TL_FIELDCAT to P_FIELDCAT.

endform.                    " f_fieldcatJ1BNFDOC
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
form TOP_OF_PAGE.
* Cabeçalho
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
*     i_logo             = c_logo
      IT_LIST_COMMENTARY = IT_HEADER[].

endform.                    " top_of_page

*&---------------------------------------------------------------------*
*&      Form  f_monta_cabecalho
*&---------------------------------------------------------------------*
*       Monta cabeçalho
*----------------------------------------------------------------------*
form F_MONTA_CABECALHO .
* Colocando os dados para exibição do cabecalho
* Título do relatório
  data: VL_BUTXT       like T001-BUTXT,       "Nome da empresa
        VL_FILIAL1     like J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL2     like J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL(100) type C,
        VL_DATA1(10)   type C,
        VL_DATA2(10)   type C,
        VL_DATA(25)    type C.

  clear IT_HEADER.

  IT_HEADER-TYP  = 'H'.
  IT_HEADER-INFO = 'Impostos Recuperáveis (PIS/COFINS)'.
  append  IT_HEADER.


  select single BUTXT
    from T001
    into VL_BUTXT
   where BUKRS = S_BUKRS.
  concatenate S_BUKRS
              VL_BUTXT into VL_BUTXT
              separated by SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Empresa'.
  IT_HEADER-INFO = VL_BUTXT.
  append  IT_HEADER.

  select single NAME
    from J_1BBRANCH
    into VL_FILIAL1
   where BUKRS  eq S_BUKRS
     and BRANCH eq S_BRANCH-LOW.
  concatenate S_BRANCH-LOW
              VL_FILIAL1 into VL_FILIAL1
              separated by SPACE.
  if not S_BRANCH-HIGH is initial.
    select single NAME
      from J_1BBRANCH
      into VL_FILIAL2
     where BUKRS  eq S_BUKRS
       and BRANCH eq S_BRANCH-HIGH.
    concatenate S_BRANCH-HIGH
                VL_FILIAL2 into VL_FILIAL2
                separated by SPACE.
  endif.
  concatenate VL_FILIAL1
              VL_FILIAL2 into VL_FILIAL
              separated by SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Filial'.
  IT_HEADER-INFO = VL_FILIAL.
  append  IT_HEADER.

  concatenate S_PSTDAT-LOW+6(2) '.'
              S_PSTDAT-LOW+4(2) '.'
              S_PSTDAT-LOW(4)
              into VL_DATA1.

  concatenate S_PSTDAT-HIGH+6(2) '.'
              S_PSTDAT-HIGH+4(2) '.'
              S_PSTDAT-HIGH(4)
              into VL_DATA2.

  concatenate VL_DATA1
              'a'
              VL_DATA2 into VL_DATA
              separated by SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Periodo'.
  IT_HEADER-INFO = VL_DATA.
  append  IT_HEADER.

endform.                    " f_monta_cabecalho

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
form USER_COMMAND  using UCOMM like SY-UCOMM
      SELFIELD type SLIS_SELFIELD.

  read table IT_RELATORIO into WA_RELATORIO index SELFIELD-TABINDEX.
  if SELFIELD-FIELDNAME = 'BELNR'.
    call function 'AUTHORITY_CHECK_TCODE'
      exporting
        TCODE  = 'MIR4'
      exceptions
        OK     = 1
        NOT_OK = 2.
    if SY-SUBRC = 2.
      message E077(S#) with 'MIR4'.
    endif.
    set parameter id:  'RBN' field WA_RELATORIO-BELNR,
                       'GJR' field WA_RELATORIO-GJAHR,
                       'NCH' field 'X'.
    call transaction 'MIR4' and skip first screen.
  endif.
endform.                    "user_command
*----------------------------------------------------------------------*
* Form      : F_MONTA_SUB_TOTAIS.                                      *
* Descrição : Monta os SubTotais ( Verifica Colunas a Somar ).         *
* Entrada   : NÃO HÁ                                                   *
* Saída     : NÃO HÁ                                                   *
*----------------------------------------------------------------------*
form F_MONTA_SUB_TOTAIS.

* Limpa TI
  clear : IT_SORT, IT_ALV_SORT.
  refresh IT_ALV_SORT.

  IT_SORT-SPOS      = 1.
  IT_SORT-FIELDNAME = 'BRANCH'.
  IT_SORT-TABNAME   = 'IT_RELATORIO'.
  IT_SORT-SUBTOT    = 'X'.
  IT_SORT-UP        = 'X'.
  IT_SORT-GROUP     = '*'.
  append IT_SORT to IT_ALV_SORT.
  clear: IT_SORT.

  IT_SORT-SPOS      = 2.
  IT_SORT-FIELDNAME = 'MAKTX'.
  IT_SORT-TABNAME   = 'IT_RELATORIO'.
  IT_SORT-SUBTOT    = 'X'.
  IT_SORT-UP        = 'X'.
  IT_SORT-GROUP     = '*'.
  append IT_SORT to IT_ALV_SORT.
  clear: IT_SORT.

*  it_sort-spos      = 3.
*  it_sort-fieldname = 'DOCNUM'.
*  it_sort-tabname   = 'IT_RELATORIO'.
*  it_sort-subtot    = 'X'.
*  it_sort-up        = 'X'.
*  it_sort-group     = '*'.
*  append it_sort to it_alv_sort.
*  clear: it_sort.
endform.                    " f_monta_sub_totais
