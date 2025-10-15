*&---------------------------------------------------------------------*
*& Report  ZFIR0061                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 23/12/2015                                              &*
*& Descrição: Relatório Livro Caixa (Fundo Fixo)                      &*
*& Transação: FI                                                      &*
*&--------------------------------------------------------------------&*
report ZFIR0061.

*----------------------------------------------------------------------*
***Tabelas
*----------------------------------------------------------------------*
tables: TCJ_DOCUMENTS,ZFIT0172.
include <ICON>.
*----------------------------------------------------------------------*
***Estrutura
*----------------------------------------------------------------------*
types: begin of TY_SAIDA,
         COMP_CODE            type TCJ_DOCUMENTS-COMP_CODE,
         CAJO_NUMBER          type TCJ_DOCUMENTS-CAJO_NUMBER,
         FISC_YEAR            type TCJ_DOCUMENTS-FISC_YEAR,
         DT_DOCUMENTO         type TCJ_DOCUMENTS-DOCUMENT_DATE,
         GL_ACCOUNT           type TCJ_C_JOURNALS-GL_ACCOUNT,
         POSTING_NUMBER       type TCJ_DOCUMENTS-POSTING_NUMBER,
         CURRENCY             type TCJ_C_JOURNALS-CURRENCY,
         SALDO_INIC           type C,
         H_RECEIPTS           type TCJ_DOCUMENTS-H_RECEIPTS,
         H_PAYMENTS           type TCJ_DOCUMENTS-H_PAYMENTS,
         SALDO_FIM            type TCJ_DOCUMENTS-H_PAYMENTS,
         SALDO_FBCJ           type  ZDE_SALDO_FBJC,
         DESCRICAO            type TCJ_DOCUMENTS-BP_NAME,
*CAJO_NAME      TYPE TCJ_CJ_NAMES-CAJO_NAME,
         DT_LCTO              type TCJ_DOCUMENTS-POSTING_DATE,
         DOCUMENT_NUMBER      type TCJ_DOCUMENTS-DOCUMENT_NUMBER,
         DOCUMENT_STATUS(132),
         ACCOUNTANT           type TCJ_DOCUMENTS-ACCOUNTANT,
         STATUS(4),

       end of TY_SAIDA,

       begin of TY_SAIDA_PAI,
         COMP_CODE_  type CHAR4,
         COMP_CODE   type TCJ_DOCUMENTS-COMP_CODE,
         CAJO_NUMBER type TCJ_DOCUMENTS-CAJO_NUMBER,
         CURRENCY    type TCJ_DOCUMENTS-CURRENCY,
         SALDO_INIC  type ZSLDO_FUNFIXO-SALDO_INIC,
         H_RECEIPTS  type TCJ_DOCUMENTS-H_RECEIPTS,
         H_PAYMENTS  type TCJ_DOCUMENTS-H_PAYMENTS,
         SALDO_FIM   type ZSLDO_FUNFIXO-SALDO_FIM,
         DESCRICAO   type TCJ_DOCUMENTS-BP_NAME,
         TOTAL_FINAL type TCJ_DOCUMENTS-H_PAYMENTS,
* ---> S4 Migration - 20/06/2023 - JS
*         SALDO_FBCJ  type  ZDE_SALDO_FBJC,
         SALDO_FBCJ  type  dmbtr,
* <--- S4 Migration - 20/06/2023 - JS
         VL_FBCJ     type  ZDE_SALDO_FBJC,
         TOTAL_REC   type TCJ_DOCUMENTS-H_PAYMENTS,
         TOTAL_DES   type TCJ_DOCUMENTS-H_PAYMENTS,
         TOTAL_INI   type ZSLDO_FUNFIXO-SALDO_FIM,
         PERIODO     type ZPERI,
       end of TY_SAIDA_PAI.


*----------------------------------------------------------------------*
***Tabelas Interna
*----------------------------------------------------------------------*
data: IT_TCJ_C_JOURNALS        type table of TCJ_C_JOURNALS,
      IT_TCJ_DOCUMENTS         type table of TCJ_DOCUMENTS with header line,
      IT_TCJ_DOCUMENTS_AUX     type table of TCJ_DOCUMENTS with header line,
      IT_TCJ_DOCUMENTS_ANT     type table of TCJ_DOCUMENTS with header line,
      IT_TCJ_DOCUMENTS_ANT_AUX type table of TCJ_DOCUMENTS with header line,
      IT_ZSLDO_FUNFIXO         type table of ZSLDO_FUNFIXO,
      IT_SAIDA_TRE             type table of ZDE_FLUXO_CAIXA with header line,
      IT_SAIDA_PAI             type table of TY_SAIDA_PAI,
      IT_SAIDA                 type table of TY_SAIDA.
data:
  T_POSTINGS       type table of ISCJ_POSTINGS,
  T_WTAX_ITEMS     type table of TCJ_WTAX_ITEMS,
  T_SPLIT_POSTINGS type table of ISCJ_POSTINGS,
  T_CPD            type table of TCJ_CPD.

data: SOMA_REC   type CJAMOUNT,
      SOMA_DES   type CJAMOUNT,
      SOMA_FIM   type CJAMOUNT,
      SOMA_FINAL type CJAMOUNT,
      SOMA_FBCJ  type CJAMOUNT.

"ZDE_FLUXO_CAIXA
*----------------------------------------------------------------------*
***Work Area
*----------------------------------------------------------------------*
data: WA_TCJ_C_JOURNALS    type TCJ_C_JOURNALS,
      WA_TCJ_DOCUMENTS     type TCJ_DOCUMENTS,
      WA_TCJ_DOCUMENTS_AUX type TCJ_DOCUMENTS,
      WA_TCJ_DOCUMENTS_ANT type TCJ_DOCUMENTS,
      WA_ZSLDO_FUNFIXO     type ZSLDO_FUNFIXO,
      WA_SAIDA_TRE         type ZDE_FLUXO_CAIXA,
      WA_SAIDA_PAI         type TY_SAIDA_PAI,
      WA_SAIDA             type TY_SAIDA.

*----------------------------------------------------------------------*
***Estrutura
*----------------------------------------------------------------------*
data: IT_CATALOG type LVC_T_FCAT,
      H_HEADER   type TREEV_HHDR,
      WA_VARIANT type DISVARIANT,
      TREE       type ref to CL_GUI_ALV_TREE.

data: G_VARIANT       like DISVARIANT,
      G_TREE3         type ref to CL_GUI_ALV_TREE,
      DG_HTML_CNTRL   type ref to CL_GUI_HTML_VIEWER,
*      TREE              TYPE REF TO CL_GUI_ALV_TREE,
      DG_DYNDOC_ID    type ref to CL_DD_DOCUMENT,
      DG_SPLITTER     type ref to CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2   type ref to CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_TREE  type ref to CL_GUI_CONTAINER,
      DG_PARENT_HTML  type ref to CL_GUI_CONTAINER,
      DG_PARENT_HTML1 type ref to CL_GUI_CONTAINER,
      DG_PARENT_HTML2 type ref to CL_GUI_CONTAINER,
      PICTURE         type ref to CL_GUI_PICTURE,
      CONTAINER       type ref to CL_GUI_CUSTOM_CONTAINER,
      OK_CODE         type SY-UCOMM.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
selection-screen: begin of block B1 with frame title text-001.
  select-options: S_BUKRS for TCJ_DOCUMENTS-COMP_CODE  obligatory no intervals no-extension,
                  S_LIVRO for TCJ_DOCUMENTS-CAJO_NUMBER obligatory,
                  S_PERIO for TCJ_DOCUMENTS-POSTING_DATE obligatory.
selection-screen: end of block B1.

*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
start-of-selection.

  perform CHECK_VARIAVEL.
  perform: F_SELECIONA_DADOS.

end-of-selection.
  if S_PERIO-HIGH  is not initial.
    call screen 0100.
  endif.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
form F_SELECIONA_DADOS.

  ranges PERIO for TCJ_DOCUMENTS-POSTING_DATE.

  append value #(
                  SIGN   = 'I'
                  OPTION = 'BT'
                  LOW    = S_PERIO-LOW
                  HIGH   = S_PERIO-HIGH
                ) to PERIO.

  select *
    from TCJ_DOCUMENTS
    into table IT_TCJ_DOCUMENTS
    where COMP_CODE    in S_BUKRS
      and CAJO_NUMBER  in S_LIVRO
*      AND POSTING_DATE EQ S_PERIO-LOW
      and POSTING_DATE in PERIO
      and ( ( DOCUMENT_STATUS ne 'D' ) "AND ( document_status NE 'S' )
     ).

*check S_PERIO-HIGH.
  if S_PERIO-HIGH is initial.
    message 'Preencha o periodo completo' type 'I'.
  else.

    select *
       from TCJ_DOCUMENTS
       into table IT_TCJ_DOCUMENTS_ANT
       where COMP_CODE    in S_BUKRS
         and CAJO_NUMBER  in S_LIVRO
*      AND POSTING_DATE GE '01012000'
         and POSTING_DATE lt S_PERIO-LOW
         and ( ( DOCUMENT_STATUS ne 'D' ) "AND ( document_status NE 'S' )
      ).

    select *
      from TCJ_C_JOURNALS
      into table IT_TCJ_C_JOURNALS
       for all entries in IT_TCJ_DOCUMENTS
     where COMP_CODE    eq IT_TCJ_DOCUMENTS-COMP_CODE
       and CAJO_NUMBER  eq IT_TCJ_DOCUMENTS-CAJO_NUMBER.
  endif.

endform.                    "F_SELECIONA_DADOS

*=============================================================================*
*Form F_ORGANIZA_DADOS                                                        *
*=============================================================================*
form: F_ORGANIZA_DADOS.

  data: SALDO_ANT_REC      type TCJ_DOCUMENTS-H_PAYMENTS,
        SALDO_ANT_REC_BFCJ type TCJ_DOCUMENTS-H_PAYMENTS,
        SALDO_ANT_DES      type TCJ_DOCUMENTS-H_PAYMENTS,
        SALDO_ANT_DES_FBCJ type TCJ_DOCUMENTS-H_PAYMENTS,
        SALDO_ANT_TOT      type TCJ_DOCUMENTS-H_PAYMENTS,
        SALDO_ANT_TOT_FBCJ type TCJ_DOCUMENTS-H_PAYMENTS,
        SOMA_ANT           type TCJ_DOCUMENTS-H_PAYMENTS,
        SOMA_REC           type TCJ_DOCUMENTS-H_PAYMENTS,
        TOTAL_REC          type TCJ_DOCUMENTS-H_PAYMENTS,
        TOTAL_REC_FBCJ     type TCJ_DOCUMENTS-H_PAYMENTS,
        SOMA_DES           type TCJ_DOCUMENTS-H_PAYMENTS,
        TOTAL_DES          type TCJ_DOCUMENTS-H_PAYMENTS,
        TOTAL_DES_FBCJ     type TCJ_DOCUMENTS-H_PAYMENTS,
        SOMA_FBCJ          type TCJ_DOCUMENTS-H_PAYMENTS,
        VAR_TOTAL          type TCJ_DOCUMENTS-H_PAYMENTS.


  clear: IT_TCJ_DOCUMENTS_AUX[], WA_SAIDA_PAI.
  if IT_TCJ_DOCUMENTS is not initial.
    move IT_TCJ_DOCUMENTS_ANT[] to IT_TCJ_DOCUMENTS_AUX[].
  else.
    move IT_TCJ_DOCUMENTS[] to IT_TCJ_DOCUMENTS_AUX[].

    loop at IT_TCJ_DOCUMENTS_ANT[] into WA_TCJ_DOCUMENTS_ANT.
      append WA_TCJ_DOCUMENTS_ANT to IT_TCJ_DOCUMENTS_AUX[].
    endloop.

  endif.
  sort IT_TCJ_DOCUMENTS_AUX by COMP_CODE CAJO_NUMBER.
  delete adjacent duplicates from IT_TCJ_DOCUMENTS_AUX comparing COMP_CODE CAJO_NUMBER.

  loop at IT_TCJ_DOCUMENTS_AUX.

    clear: WA_SAIDA_PAI-SALDO_FIM, WA_SAIDA_PAI-COMP_CODE , WA_SAIDA_PAI-CAJO_NUMBER,WA_SAIDA_PAI-DESCRICAO,
      WA_SAIDA_PAI-CURRENCY ,  WA_SAIDA_PAI-SALDO_INIC, SALDO_ANT_REC, SALDO_ANT_DES, SALDO_ANT_TOT, TOTAL_REC, TOTAL_DES,
      WA_SAIDA-H_RECEIPTS, WA_SAIDA-H_PAYMENTS.

    loop at IT_TCJ_DOCUMENTS_ANT where COMP_CODE   eq IT_TCJ_DOCUMENTS_AUX-COMP_CODE
                                   and CAJO_NUMBER eq IT_TCJ_DOCUMENTS_AUX-CAJO_NUMBER.

*      ADD it_tcj_documents_ant-h_receipts TO saldo_ant_rec_bfcj.
*      ADD it_tcj_documents_ant-h_payments TO saldo_ant_des_fbcj.

      if IT_TCJ_DOCUMENTS_ANT-DOCUMENT_STATUS ne 'S'.
        add IT_TCJ_DOCUMENTS_ANT-H_RECEIPTS to SALDO_ANT_REC.
        add IT_TCJ_DOCUMENTS_ANT-H_PAYMENTS to SALDO_ANT_DES.
      endif.

    endloop.

    WA_SAIDA_PAI-COMP_CODE   = IT_TCJ_DOCUMENTS_AUX-COMP_CODE.
    WA_SAIDA_PAI-COMP_CODE_  = |{ IT_TCJ_DOCUMENTS_AUX-COMP_CODE }|.
    WA_SAIDA_PAI-CAJO_NUMBER = IT_TCJ_DOCUMENTS_AUX-CAJO_NUMBER.
    WA_SAIDA_PAI-DESCRICAO   = ''.
    WA_SAIDA_PAI-CURRENCY    = IT_TCJ_DOCUMENTS_AUX-CURRENCY.
    SALDO_ANT_TOT = SALDO_ANT_REC - SALDO_ANT_DES.
    SALDO_ANT_TOT_FBCJ = SALDO_ANT_REC_BFCJ - SALDO_ANT_DES_FBCJ.
    WA_SAIDA_PAI-SALDO_INIC  =   SALDO_ANT_TOT .
*---> 08/06/2023 - Migração S4 - JS
*           wa_saida_pai-saldo_fbcj  =   saldo_ant_tot_fbcj .
    WA_SAIDA_PAI-SALDO_FBCJ = conv #( SALDO_ANT_TOT_FBCJ ).
*<--- 08/06/2023 - Migração S4 - JS
    WA_SAIDA_PAI-SALDO_FIM   = 0.

    loop at IT_TCJ_DOCUMENTS into WA_TCJ_DOCUMENTS
                             where COMP_CODE   eq IT_TCJ_DOCUMENTS_AUX-COMP_CODE
                               and CAJO_NUMBER eq IT_TCJ_DOCUMENTS_AUX-CAJO_NUMBER.

      clear: WA_SAIDA.

      WA_SAIDA-COMP_CODE       = WA_TCJ_DOCUMENTS-COMP_CODE.
      WA_SAIDA-CURRENCY        = WA_TCJ_DOCUMENTS-CURRENCY.
      WA_SAIDA-CAJO_NUMBER     = WA_TCJ_DOCUMENTS-CAJO_NUMBER.
      WA_SAIDA-FISC_YEAR       = WA_TCJ_DOCUMENTS-FISC_YEAR.
      WA_SAIDA-DT_DOCUMENTO    = WA_TCJ_DOCUMENTS-DOCUMENT_DATE.

      read table IT_TCJ_C_JOURNALS into WA_TCJ_C_JOURNALS with key COMP_CODE    = WA_SAIDA-COMP_CODE
                                                                   CAJO_NUMBER  = WA_SAIDA-CAJO_NUMBER.

      if SY-SUBRC is initial.
        WA_SAIDA-GL_ACCOUNT      = WA_TCJ_C_JOURNALS-GL_ACCOUNT.
      endif.

      WA_SAIDA-POSTING_NUMBER  = WA_TCJ_DOCUMENTS-POSTING_NUMBER.
      WA_SAIDA-H_RECEIPTS      = WA_TCJ_DOCUMENTS-H_RECEIPTS.
      WA_SAIDA-H_PAYMENTS      = WA_TCJ_DOCUMENTS-H_PAYMENTS.
      WA_SAIDA-DESCRICAO       = WA_TCJ_DOCUMENTS-BP_NAME.
      WA_SAIDA-DT_LCTO         = WA_TCJ_DOCUMENTS-POSTING_DATE.
      WA_SAIDA-DOCUMENT_NUMBER = WA_TCJ_DOCUMENTS-DOCUMENT_NUMBER.
      WA_SAIDA-ACCOUNTANT      = WA_TCJ_DOCUMENTS-ACCOUNTANT.

      if WA_TCJ_DOCUMENTS-DOCUMENT_STATUS ne 'S'.
        add WA_SAIDA-H_RECEIPTS to TOTAL_REC.
        add WA_SAIDA-H_PAYMENTS to TOTAL_DES.
      endif.

*      ADD wa_saida-h_receipts TO total_rec_fbcj.
*      ADD wa_saida-h_payments TO total_des_fbcj.

      WA_SAIDA-DOCUMENT_STATUS = switch #( WA_TCJ_DOCUMENTS-DOCUMENT_STATUS
                                           when 'C' then text-003
                                           when 'R' then text-004
                                           when 'S' then text-005
                                           when 'P' then text-006
                                         ).
      append WA_SAIDA to IT_SAIDA.

    endloop.

    WA_SAIDA_PAI-H_RECEIPTS      = TOTAL_REC.
    WA_SAIDA_PAI-H_PAYMENTS      = TOTAL_DES.
    add WA_SAIDA_PAI-SALDO_INIC to WA_SAIDA_PAI-TOTAL_INI.
    add TOTAL_REC               to WA_SAIDA_PAI-TOTAL_REC.
    add TOTAL_DES               to WA_SAIDA_PAI-TOTAL_DES.
    WA_SAIDA_PAI-SALDO_FIM       = SALDO_ANT_TOT + TOTAL_REC - TOTAL_DES.
*    wa_saida_pai-saldo_fbcj      = saldo_ant_tot_fbcj + total_rec_fbcj - total_des_fbcj.
    add WA_SAIDA_PAI-SALDO_FIM  to WA_SAIDA_PAI-TOTAL_FINAL.

    add WA_SAIDA_PAI-SALDO_INIC to SOMA_REC.
    add WA_SAIDA_PAI-H_RECEIPTS to SOMA_DES.
    add WA_SAIDA_PAI-H_PAYMENTS to SOMA_FIM.
    add WA_SAIDA_PAI-SALDO_FIM  to SOMA_FINAL.

    call function 'FCJ_GET_DATA_FOR_SCREEN'
      exporting
        I_COMP_CODE            = IT_TCJ_DOCUMENTS_AUX-COMP_CODE
        I_CAJO_NUMBER          = IT_TCJ_DOCUMENTS_AUX-CAJO_NUMBER
        I_DISPLAY_PERIOD_LO    = S_PERIO-LOW
        I_DISPLAY_PERIOD_HI    = S_PERIO-HIGH
      importing
        E_RUNNING_CASH_BALANCE = WA_SAIDA_PAI-SALDO_FBCJ
      tables
        E_POSTINGS             = T_POSTINGS
        E_WTAX_ITEMS           = T_WTAX_ITEMS
        E_SPLIT_POSTINGS       = T_SPLIT_POSTINGS
        E_CPD                  = T_CPD.

    add WA_SAIDA_PAI-SALDO_FBCJ to SOMA_FBCJ.

    "Incluir o periodo.
    " CS2022000862 - Incluir cabeçalho ao gerar Relatório - ZFI0076 (Leidiane Santos) / Anderson Oenning
    WA_SAIDA_PAI-PERIODO = |{ S_PERIO-LOW+6(2) }/{ S_PERIO-LOW+4(2) }/{ S_PERIO-LOW(4) } - { S_PERIO-HIGH+6(2) }/{ S_PERIO-HIGH+4(2) }/{ S_PERIO-HIGH(4) }|.
    "Fim " CS2022000862

    append WA_SAIDA_PAI to IT_SAIDA_PAI.

  endloop.

  append value #(
                  COMP_CODE_  = 'SOMA'
                  SALDO_INIC  = SOMA_REC
                  H_RECEIPTS  = SOMA_DES
                  H_PAYMENTS  = SOMA_FIM
                  SALDO_FIM   = SOMA_FINAL
                  SALDO_FBCJ  = SOMA_FBCJ
                ) to IT_SAIDA_PAI.


endform.                    "F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
module PBO output.

  set pf-status 'PF0100_STATUS'.

  if CONTAINER is initial.
    perform CREATE_CONTAINER_ALV_TREE.
  endif.

  if ( TREE is initial ).
    perform: INICIAR_TREE.
  endif.

endmodule.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*

module PAI input.

  case SY-UCOMM.
    when: 'BACK' or 'CANCEL' or 'EXIT'.
      leave to screen 0.
  endcase.

endmodule.                 " PAI  INPUT

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_ALV_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CREATE_CONTAINER_ALV_TREE .

  data: URL(255)        type C,
        IT_FIELDCATALOG type LVC_T_FCAT.

* create a container for the tree control
  create object CONTAINER
    exporting
      CONTAINER_NAME              = 'C_TREE'
    exceptions
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  if SY-SUBRC <> 0.
    message A000(TREE_CONTROL_MSG).
  endif.

  create object DG_DYNDOC_ID
    exporting
      STYLE = 'ALV_GRID'.

  create object DG_SPLITTER
    exporting
      PARENT  = CONTAINER
      ROWS    = 2
      COLUMNS = 1.

  call method DG_SPLITTER->GET_CONTAINER
    exporting
      ROW       = 1
      COLUMN    = 1
    receiving
      CONTAINER = DG_PARENT_HTML.

  create object DG_SPLITTER_2
    exporting
      PARENT  = DG_PARENT_HTML
      ROWS    = 1
      COLUMNS = 2.

  call method DG_SPLITTER_2->GET_CONTAINER
    exporting
      ROW       = 1
      COLUMN    = 1
    receiving
      CONTAINER = DG_PARENT_HTML1.

  call method DG_SPLITTER_2->SET_COLUMN_WIDTH
    exporting
      ID    = 1
      WIDTH = 20.

  call method DG_SPLITTER_2->GET_CONTAINER
    exporting
      ROW       = 1
      COLUMN    = 2
    receiving
      CONTAINER = DG_PARENT_HTML2.

  create object PICTURE
    exporting
      PARENT = DG_PARENT_HTML2.

  perform F_PEGA_IMAGEM using 'LOGO_NOVO' changing URL.

  call method PICTURE->LOAD_PICTURE_FROM_URL
    exporting
      URL = URL.

  call method PICTURE->SET_DISPLAY_MODE
    exporting
      DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

  call method DG_SPLITTER->GET_CONTAINER
    exporting
      ROW       = 2
      COLUMN    = 1
    receiving
      CONTAINER = DG_PARENT_TREE.

  call method DG_SPLITTER->SET_ROW_HEIGHT
    exporting
      ID     = 1
      HEIGHT = 27.



*  PERFORM CRIA_HTML_CAB_TREE.


endform.                    " CREATE_CONTAINER_ALV_TREE

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
form F_PEGA_IMAGEM  using    NOME_LOGO
                    changing URL.

  data: begin of GRAPHIC_TABLE occurs 0,
          LINE(255) type X,
        end of GRAPHIC_TABLE.
  data: L_GRAPHIC_XSTR type XSTRING.
  data: GRAPHIC_SIZE   type I.
  data: L_GRAPHIC_CONV type I.
  data: L_GRAPHIC_OFFS type I.

  refresh GRAPHIC_TABLE.
  call method CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    exporting
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    receiving
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.
  while L_GRAPHIC_CONV > 255.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    append GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  endwhile.
  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  append GRAPHIC_TABLE.
  call function 'DP_CREATE_URL'
    exporting
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    tables
      DATA     = GRAPHIC_TABLE
    changing
      URL      = URL.
endform.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form CRIA_HTML_CAB_TREE.

  data: TEXTO(40), VG_MES(2), VG_ANO(4),
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            type SDYDO_TEXT_TABLE,
        P_TEXT                  type SDYDO_TEXT_ELEMENT,
        POSITION                type I.

  data: COLUMN         type ref to CL_DD_AREA,
        COLUMN_1       type ref to CL_DD_AREA,
        COLUMN_2       type ref to CL_DD_AREA,
        TABLE_ELEMENT  type ref to CL_DD_TABLE_ELEMENT,
        TABLE_ELEMENT2 type ref to CL_DD_TABLE_ELEMENT.

  call method DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

  call method DG_DYNDOC_ID->ADD_TABLE
    exporting
      NO_OF_COLUMNS = 1
      BORDER        = '0'
      WIDTH         = '100%'
    importing
      TABLE         = TABLE_ELEMENT.

  call method TABLE_ELEMENT->ADD_COLUMN
    importing
      COLUMN = COLUMN.

  call method TABLE_ELEMENT->SET_COLUMN_STYLE
    exporting
      COL_NO    = 1
      SAP_ALIGN = 'CENTER'
      SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

  P_TEXT = 'Livro Caixa Consolidado'.
  call method COLUMN->ADD_TEXT
    exporting
      TEXT      = P_TEXT
      SAP_STYLE = 'HEADING'.

  call method DG_DYNDOC_ID->ADD_TABLE
    exporting
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '100%'
    importing
      TABLE         = TABLE_ELEMENT2.

  call method TABLE_ELEMENT2->ADD_COLUMN
    exporting
      SAP_STYLE   = 'SAP_BOLD'
      STYLE_CLASS = 'SAP_BOLD'
    importing
      COLUMN      = COLUMN_1.

  call method TABLE_ELEMENT2->ADD_COLUMN
    importing
      COLUMN = COLUMN_2.

  call method TABLE_ELEMENT2->SET_COLUMN_STYLE
    exporting
      COL_NO       = 2
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.
*
*  IF S_BUKRS IS NOT INITIAL.
  SDYDO_TEXT_ELEMENT = 'Empresa: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*  ENDIF.
  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*  IF S_PERIO IS NOT INITIAL.
  SDYDO_TEXT_ELEMENT = 'Periodo: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*  ENDIF.
  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = 'Valor_Total_Inicial: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = 'Valor_Total_Receitas: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = 'Valor_Total_Despesas: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = 'Valor_Total_Final: '.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*
*
  call method COLUMN_1->ADD_TEXT
    exporting
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  clear: P_TEXT_TABLE.

*  IF S_BUKRS IS NOT INITIAL.
  SDYDO_TEXT_ELEMENT = S_BUKRS-LOW.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*  ENDIF.
  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

*  IF S_PERIO-LOW IS NOT INITIAL.
  concatenate S_PERIO-LOW+6(2) '/' S_PERIO-LOW+4(2) '/' S_PERIO-LOW(4) into SDYDO_TEXT_ELEMENT.
*    IF S_PERIO-HIGH IS NOT INITIAL.
  concatenate SDYDO_TEXT_ELEMENT '-' S_PERIO-HIGH+6(2) into SDYDO_TEXT_ELEMENT separated by SPACE.
  concatenate SDYDO_TEXT_ELEMENT '/' S_PERIO-HIGH+4(2) '/' S_PERIO-HIGH(4) into SDYDO_TEXT_ELEMENT.
*    ENDIF.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.
*  ENDIF.

  SDYDO_TEXT_ELEMENT = ''.

  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.


  SDYDO_TEXT_ELEMENT = WA_SAIDA_PAI-TOTAL_INI.
  call function 'STRING_REPLACE'
    exporting
      PATTERN    = '.'
      SUBSTITUTE = ','
    changing
      TEXT       = SDYDO_TEXT_ELEMENT.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = WA_SAIDA_PAI-TOTAL_REC.
  call function 'STRING_REPLACE'
    exporting
      PATTERN    = '.'
      SUBSTITUTE = ','
    changing
      TEXT       = SDYDO_TEXT_ELEMENT.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = WA_SAIDA_PAI-TOTAL_DES.
  call function 'STRING_REPLACE'
    exporting
      PATTERN    = '.'
      SUBSTITUTE = ','
    changing
      TEXT       = SDYDO_TEXT_ELEMENT.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = ''.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  SDYDO_TEXT_ELEMENT = WA_SAIDA_PAI-TOTAL_FINAL.
  call function 'STRING_REPLACE'
    exporting
      PATTERN    = '.'
      SUBSTITUTE = ','
    changing
      TEXT       = SDYDO_TEXT_ELEMENT.
  append SDYDO_TEXT_ELEMENT to P_TEXT_TABLE.

  call method COLUMN_2->ADD_TEXT
    exporting
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  perform CONTAINER_HTML.


endform.                    " CRIA_HTML_CAB_DRE

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CONTAINER_HTML .

  data : DL_LENGTH        type I,                           " Length
         DL_BACKGROUND_ID type SDYDO_KEY value SPACE. " Background_id

  if DG_HTML_CNTRL is initial.
    create object DG_HTML_CNTRL
      exporting
        PARENT = DG_PARENT_HTML1.
  endif.

  call function 'REUSE_ALV_GRID_COMMENTARY_SET'
    exporting
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    importing
      LENGTH   = DL_LENGTH.

  call method DG_DYNDOC_ID->MERGE_DOCUMENT.

  call method DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND
    exporting
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

  call method DG_DYNDOC_ID->DISPLAY_DOCUMENT
    exporting
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML1
    exceptions
      HTML_DISPLAY_ERROR = 1.

*  PERFORM INICIAR_TREE.

endform.                    " CONTAINER_HTML
*&---------------------------------------------------------------------*
*&      Form  CATALOGO_TREE_ALV_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCATALOG  text
*----------------------------------------------------------------------*
form CATALOGO_TREE_ALV_DRE   tables  IT_FIELDCATALOG structure LVC_S_FCAT.

  data: FIELD       type LVC_S_FCAT,
        VG_GJAHR    type GJAHR,
        VG_MONAT    type MONAT,
        VT_GJAHR(4),
        VT_MONAT(2).

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'COMP_CODE'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 9.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'PERIODO'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 23.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'FISC_YEAR'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 10.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'DT_LCTO'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 16.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'GL_ACCOUNT'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'CURRENCY'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 10.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'SALDO_INIC'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_FALSE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'H_RECEIPTS'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_FALSE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'H_PAYMENTS'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_FALSE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'SALDO_FIM'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_FALSE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'SALDO_FBCJ'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_FALSE.
  FIELD-DATATYPE  = 'NUMC'.
  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

*  CLEAR: field.
*  field-col_pos   = 1.
*  field-fieldname = 'VL_FBCJ'.
*  field-tabname   = 'ZDE_FLUXO_CAIXA'.
*  field-emphasize = 'K41'.
*  field-outputlen = 20.
*  field-scrtext_m = field-scrtext_l.
*  field-scrtext_s = field-scrtext_l.
**  FIELD-DO_SUM    = ABAP_FALSE.
*  field-datatype  = 'NUMC'.
*  field-just      = 'R'.
*  APPEND field TO it_fieldcatalog.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'DESCRICAO'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 40.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
*  FIELD-DO_SUM    = ABAP_TRUE.
*  FIELD-DATATYPE  = 'NUMC'.
*  FIELD-JUST      = 'R'.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'DOCUMENT_NUMBER'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 40.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'DT_DOCUMENTO'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'DOCUMENT_STATUS'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 45.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

  clear: FIELD.
  FIELD-COL_POS   = 1.
  FIELD-FIELDNAME = 'ACCOUNTANT'.
  FIELD-TABNAME   = 'ZDE_FLUXO_CAIXA'.
  FIELD-EMPHASIZE = 'K41'.
  FIELD-OUTPUTLEN = 20.
  FIELD-SCRTEXT_M = FIELD-SCRTEXT_L.
  FIELD-SCRTEXT_S = FIELD-SCRTEXT_L.
  FIELD-DO_SUM    = ABAP_FALSE.
  append FIELD to IT_FIELDCATALOG.

endform.                    "CATALOGO_TREE_ALV_DRE

*&---------------------------------------------------------------------*
*&      Form  CREATE_TREE_ALV_TREE3_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form INICIAR_TREE .

  data: L_HIERARCHY_HEADER type TREEV_HHDR,
        LT_LIST_COMMENTARY type SLIS_T_LISTHEADER,
        L_LOGO             type SDYDO_VALUE,
        LS_LINE            type SLIS_LISTHEADER,
        IT_FIELDCATALOG    type LVC_T_FCAT,
        VG_MES(2),
        VG_ANO(4),
        LT_EVENTS          type CNTL_SIMPLE_EVENTS,
        L_EVENT            type CNTL_SIMPLE_EVENT,
        I_DEFAULT          type CHAR01.

  create object G_TREE3
    exporting
      PARENT                      = DG_PARENT_TREE
      NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_MULTIPLE
      ITEM_SELECTION              = SPACE
      NO_HTML_HEADER              = 'X'
      NO_TOOLBAR                  = ''
    exceptions
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.

  if SY-SUBRC is not initial.
    message X208(00) with 'ERROR'.                          "#EC NOTEXT
  endif.

*  CLEAR L_EVENT.
*  L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  L_EVENT-APPL_EVENT = 'X'.
  append L_EVENT to LT_EVENTS.

  call method G_TREE3->SET_REGISTERED_EVENTS
    exporting
      EVENTS                    = LT_EVENTS
    exceptions
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

*  CREATE OBJECT ALV_TREE_VIEW_EVENT.
*  SET HANDLER ALV_TREE_VIEW_EVENT->ON_NODE_DOUBLE_CLICK FOR G_TREE3.

  L_HIERARCHY_HEADER-T_IMAGE = 'LOGO_NOVO'.
  L_HIERARCHY_HEADER-HEADING = 'Livro Caixa'.
  L_HIERARCHY_HEADER-WIDTH   = 30.


  clear LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = 'Livro Caixa Consolidado'.
  append LS_LINE to LT_LIST_COMMENTARY.

  if S_BUKRS is not initial.
    clear LS_LINE.
    LS_LINE-KEY  = 'Livro Caixa'.
    LS_LINE-TYP  = 'S'.
    LS_LINE-INFO = S_BUKRS .
    append LS_LINE to LT_LIST_COMMENTARY.
  endif.


  L_LOGO = 'LOGO_NOVO'.

  perform CATALOGO_TREE_ALV_DRE   tables  IT_FIELDCATALOG.

*  IF VARIANT IS NOT INITIAL.
*    MOVE VARIANT TO G_VARIANT-VARIANT.
*    I_DEFAULT = ABAP_FALSE.
*  ELSE.
*    I_DEFAULT = ABAP_TRUE.
*  ENDIF.


  G_VARIANT-REPORT = SY-REPID.

  call method G_TREE3->SET_TABLE_FOR_FIRST_DISPLAY
    exporting
      I_STRUCTURE_NAME    = 'ZDE_FLUXO_CAIXA'
      IS_HIERARCHY_HEADER = L_HIERARCHY_HEADER
      IT_LIST_COMMENTARY  = LT_LIST_COMMENTARY
      I_LOGO              = L_LOGO
      I_BACKGROUND_ID     = 'ALV_BACKGROUND'
      I_SAVE              = 'A'
      IS_VARIANT          = G_VARIANT
    changing
      IT_OUTTAB           = IT_SAIDA_TRE[]      "I_DEFAULT = I_DEFAULT
      IT_FIELDCATALOG     = IT_FIELDCATALOG.

  data: IT_NODE_KEY type LVC_T_NKEY.

  perform :           F_ORGANIZA_DADOS,
                      CRIA_HTML_CAB_TREE,
                      ESTRUTURA_SAIDA_ALV_TREE_VIEW tables IT_NODE_KEY.

** calculate totals
*  CALL METHOD G_TREE3->UPDATE_CALCULATIONS.
*
* this method must be called to send the data to the frontend

  call method G_TREE3->FRONTEND_UPDATE.

*  CALL METHOD G_TREE3->EXPAND_NODES
*    EXPORTING
*      IT_NODE_KEY = IT_NODE_KEY.

*  CALL METHOD G_TREE3->COLLAPSE_ALL_NODES.




*  PERFORM CHANGE_TOOLBAR.

*  PERFORM CRIA_HTML_CAB_TREE.

endform.                    " CREATE_TREE_ALV_TREE3_TREE

*&---------------------------------------------------------------------*
*&      Form  ESTRUTURA_SAIDA_ALV_TREE_VIEW
*&---------------------------------------------------------------------*

form ESTRUTURA_SAIDA_ALV_TREE_VIEW  tables IT_NODE_KEY type LVC_T_NKEY.

  data: CAJO_NUMBER_KEY type LVC_NKEY,
        GL_ACCOUNT_KEY  type LVC_NKEY,
        LAST_KEY        type LVC_NKEY.

  data: P_SALDO_FINAL_STATUS type CHAR1.
  data: WA_NODE_PAI like line of IT_SAIDA_TRE.

  "LOOP EMPRESA

  loop at IT_SAIDA_PAI into WA_SAIDA_PAI .
    " Inclui nodos Raises da ALV TREE VIEW """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*****************************************************************************************************************
    clear: IT_SAIDA_TRE,WA_SAIDA, WA_NODE_PAI.

    if WA_SAIDA_PAI-CAJO_NUMBER is not initial and IT_SAIDA_TRE-COMP_CODE  is initial.
      perform ADD_CAJO_NUMBER using WA_SAIDA_PAI '' changing CAJO_NUMBER_KEY WA_NODE_PAI.
    else.
      if WA_SAIDA_PAI-COMP_CODE_ eq 'SOMA'.
        perform ADD_CAJO_NUMBER using WA_SAIDA_PAI '' changing CAJO_NUMBER_KEY WA_NODE_PAI.
      endif.
    endif.

    if WA_SAIDA_PAI-COMP_CODE is not initial and WA_SAIDA_PAI-CAJO_NUMBER is not initial.
      perform ADD_GL_ACCOUNT using WA_SAIDA CAJO_NUMBER_KEY P_SALDO_FINAL_STATUS changing GL_ACCOUNT_KEY.
    endif.

    if P_SALDO_FINAL_STATUS = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL and  WA_SAIDA_PAI-COMP_CODE_ ne 'SOMA'.
      call method G_TREE3->CHANGE_NODE
        exporting
          I_NODE_KEY     = CAJO_NUMBER_KEY
          I_OUTTAB_LINE  = WA_NODE_PAI
          IT_ITEM_LAYOUT = value #( ( FIELDNAME = 'SALDO_FIM'
                                      STYLE     = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL
                                      U_STYLE    = 'X' ) ).
    endif.

  endloop.


endform.                    " ESTRUTURA_SAIDA_ALV_TREE_VIEW

*&---------------------------------------------------------------------*
*&      Form  ADD_WERKS
*&---------------------------------------------------------------------*
form ADD_CAJO_NUMBER  using    P_SAIDA_PAI type TY_SAIDA_PAI
                               P_RELAT_KEY type LVC_NKEY
                      changing P_NODE_KEY  type LVC_NKEY
                               P_NODE_PAI  type ZDE_FLUXO_CAIXA.


  data: L_NODE_TEXT    type LVC_VALUE,
        LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI,
        LS_NODE        type LVC_S_LAYN.

  append value #(
                  FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME
                  STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT
                ) to LT_ITEM_LAYOUT.


*  LS_ITEM_LAYOUT-FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME.
*  LS_ITEM_LAYOUT-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT.
*  APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.

  L_NODE_TEXT       =  P_SAIDA_PAI-CAJO_NUMBER.
  LS_NODE = value #(
                    ISFOLDER  = ABAP_TRUE
                    STYLE     = cond #( when WA_SAIDA_PAI-COMP_CODE_ eq 'SOMA' then '10' else '0' )
                    N_IMAGE   = cond #( when WA_SAIDA_PAI-COMP_CODE_ eq 'SOMA' then '@KV@' else ABAP_FALSE )
                  ).
*  LS_NODE-ISFOLDER  = ABAP_TRUE.
*  LS_NODE-STYLE     = COND #( WHEN WA_SAIDA_PAI-COMP_CODE_ EQ 'SOMA' THEN '10' ELSE '0' ).
*  LS_NODE-N_IMAGE   = COND #( WHEN WA_SAIDA_PAI-COMP_CODE_ EQ 'SOMA' THEN '@KV@' ELSE ABAP_FALSE ).


  move-corresponding WA_SAIDA_PAI to IT_SAIDA_TRE.   "#EC CI_FLDEXT_OK[2610650]
  if WA_SAIDA_PAI-COMP_CODE_ ne 'SOMA'.
    move-corresponding WA_SAIDA_PAI to   P_NODE_PAI. "#EC CI_FLDEXT_OK[2610650]
  endif.
  call method G_TREE3->ADD_NODE
    exporting
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = L_NODE_TEXT
      IS_OUTTAB_LINE   = IT_SAIDA_TRE
      IS_NODE_LAYOUT   = LS_NODE
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    importing
      E_NEW_NODE_KEY   = P_NODE_KEY.

endform.                    " ADD_WERKS

*&---------------------------------------------------------------------*
*&      Form  ADD_WERKS
*&---------------------------------------------------------------------*
form ADD_GL_ACCOUNT  using  P_SAIDA type TY_SAIDA
                            P_RELAT_KEY type LVC_NKEY
                            P_SALDO_FINAL_STATUS type CHAR1
                   changing P_NODE_KEY  type LVC_NKEY.


  data: L_NODE_TEXT    type LVC_VALUE,
        LT_ITEM_LAYOUT type LVC_T_LAYI,
        LS_ITEM_LAYOUT type LVC_S_LAYI,
        LS_NODE        type LVC_S_LAYN.

  LS_ITEM_LAYOUT-FIELDNAME = G_TREE3->C_HIERARCHY_COLUMN_NAME.
  LS_ITEM_LAYOUT-STYLE     = CL_GUI_COLUMN_TREE=>STYLE_DEFAULT.
  append LS_ITEM_LAYOUT to LT_ITEM_LAYOUT.

  clear: WA_SAIDA, IT_SAIDA_TRE, P_SALDO_FINAL_STATUS.

  loop at IT_SAIDA into WA_SAIDA.

    if WA_SAIDA-CAJO_NUMBER eq WA_SAIDA_PAI-CAJO_NUMBER.
      move-corresponding WA_SAIDA to IT_SAIDA_TRE. "#EC CI_FLDEXT_OK[2610650]
      L_NODE_TEXT       =  P_SAIDA-CAJO_NUMBER.
      LS_NODE-N_IMAGE   = SPACE.
      LS_NODE-EXP_IMAGE = SPACE.


      perform F_STATUS_DOC using P_SAIDA
                           changing LS_NODE-N_IMAGE.

      if LS_NODE-N_IMAGE = ICON_LED_YELLOW.
        P_SALDO_FINAL_STATUS = CL_GUI_COLUMN_TREE=>STYLE_INTENSIFD_CRITICAL.
      endif.


      call method G_TREE3->ADD_NODE
        exporting
          I_RELAT_NODE_KEY = P_RELAT_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = L_NODE_TEXT
          IS_OUTTAB_LINE   = IT_SAIDA_TRE
          IS_NODE_LAYOUT   = LS_NODE
          IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
        importing
          E_NEW_NODE_KEY   = P_NODE_KEY.
    endif.

  endloop.


endform.                    " ADD_WERKS
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CHECK_VARIAVEL .
  case '*'.
    when S_LIVRO-LOW or S_BUKRS-LOW or S_PERIO-LOW or S_PERIO-HIGH.
      message |O valor "*" não pode ser Utilizado como Parametro!| type 'S'.
      exit.
  endcase.

  perform: F_SELECIONA_DADOS.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_STATUS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SAIDA  text
*      <--P_LS_NODE_N_IMAGE  text
*----------------------------------------------------------------------*
form F_STATUS_DOC  using    P_SAIDA type TY_SAIDA
                   changing LS_NODE_N_IMAGE  type LVC_S_LAYN-N_IMAGE.

  data: IT_SET  type table of RGSB4,
        WA_SET  type RGSB4,
        WA_BKPF type BKPF.

  data: IT_RG_DT type range of ZFIT0172-DATA_ATUAL,
        WA_RG_DT like line of IT_RG_DT.


  call function 'G_SET_GET_ALL_VALUES'
    exporting
      CLIENT        = SY-MANDT
      SETNR         = 'MAGGI_ZFI0076_DATA'
      CLASS         = '0000'
    tables
      SET_VALUES    = IT_SET
    exceptions
      SET_NOT_FOUND = 1
      others        = 2.

  check IT_SET is not initial.

  clear WA_RG_DT.
  loop at IT_SET assigning field-symbol(<FS_SET>).
    WA_RG_DT-SIGN    = 'I'.
    WA_RG_DT-OPTION  = 'LE'.
    WA_RG_DT-LOW     = <FS_SET>-FROM.
    append  WA_RG_DT to IT_RG_DT.
    clear: WA_RG_DT.
  endloop.

  select single *
    from ZFIT0172
    where COMP_CODE      = P_SAIDA-COMP_CODE
      and CAJO_NUMBER    = P_SAIDA-CAJO_NUMBER
      and FISC_YEAR      = P_SAIDA-FISC_YEAR
      and POSTING_NUMBER = P_SAIDA-POSTING_NUMBER.

  if SY-SUBRC = 0.
    LS_NODE_N_IMAGE = ICON_CHECKED.
  else.
    LS_NODE_N_IMAGE = ICON_LED_YELLOW.
  endif.

*** BUG - 73731 - Inicio - CBRAND
  data(LVA_AWKEY) = |{ P_SAIDA-POSTING_NUMBER }{ P_SAIDA-CAJO_NUMBER }{ P_SAIDA-COMP_CODE }| .

  select single *
    from BKPF
    into WA_BKPF
      where AWKEY eq LVA_AWKEY.

  if WA_BKPF-STBLG <> ''.
    LS_NODE_N_IMAGE = ICON_DUMMY.
  endif.
*** BUG - 73731 - Fim - CBRAND

  if P_SAIDA-DT_LCTO in IT_RG_DT.
    LS_NODE_N_IMAGE = ICON_CHECKED.
  endif.

endform.
