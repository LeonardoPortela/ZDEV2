*&---------------------------------------------------------------------*
*& Report  ZIMPRECOLHER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZIMPRECOLHER.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

type-pools: slis.

tables : j_1bnfdoc ,
         vbfa,
         j_1bnflin,
         j_1bnfstx.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

types:
      begin of ty_j_1bnfdoc,
        belnr     type j_1bnfdoc-belnr,
        docnum    type j_1bnfdoc-docnum,
        pstdat    type j_1bnfdoc-pstdat,
        bukrs     type j_1bnfdoc-bukrs,
        series    type j_1bnfdoc-series,
        nftype    type j_1bnfdoc-nftype,
        docdat    type j_1bnfdoc-docdat,
        crenam    type j_1bnfdoc-crenam,
        model     type j_1bnfdoc-model,
        nfnum     type j_1bnfdoc-nfnum,
        branch    type j_1bnfdoc-branch,
        parid     type j_1bnfdoc-parid,
        nfe       type j_1bnfdoc-nfe,
        nfenum    type j_1bnfdoc-nfenum,
        partyp    type j_1bnfdoc-partyp,
        nftot     type j_1bnfdoc-nftot,
        direct    type j_1bnfdoc-direct,
        cancel    type j_1bnfdoc-cancel,
      end   of ty_j_1bnfdoc,

      begin of ty_j_1baj,
        taxtyp    type j_1baj-taxtyp,
        taxgrp    type j_1baj-taxgrp,
      end   of ty_j_1baj,

      begin of ty_j_1bnflin,
        docnum    type j_1bnflin-docnum,
        cfop      type j_1bnflin-cfop,
        menge     type j_1bnflin-menge,
        meins     type j_1bnflin-meins,
        netwrt     type j_1bnflin-netwrt,
        matnr     type j_1bnflin-matnr,
        refkey    type j_1bnflin-refkey,
      end   of ty_j_1bnflin,

      begin of ty_remessa,
        vbeln  like vbak-vbeln,
        fkart  like vbrk-fkart,
        aubel  like vbrp-aubel,
        knumv  like vbrk-knumv,
      end of ty_remessa,

      begin of ty_konv,
        knumv  like konv-knumv,
        kwert  like konv-kwert,
        kbetr  like konv-kbetr,
        kschl  like konv-kschl,
      end   of ty_konv,

      begin of ty_lfa1,
        lifnr     type lfa1-lifnr,
        name1     type lfa1-name1,
      end   of ty_lfa1,

      begin of ty_saida,
        nome_clifor type lfa1-name1,
        cpf         type lfa1-stcd1,
        municipio   type lfa1-ort01,
        uf          type lfa1-regio,
        NFENUM      type j_1bnfdoc-NFENUM,  "Nº Nota
        docnum      type j_1bnfdoc-docnum, "Nº documento
        docdat      type j_1bnfdoc-docdat, "DATA DOCUMENTO
        matnr       type makt-matnr,       "Cod Material
        produto     type makt-maktx,       "Desc Material
        Tributo     type konv-kwert,       "Valor Tributo
        netwrt      type j_1bnflin-netwrt, "VALOR TOTAL
        percentual  type konv-kbetr,       "Percentual
        tipo_ordem  type vbrk-fkart,
        doc_fatura  type lips-vbeln,
        ordem       type lips-vgbel,
        quantidade  type j_1bnflin-menge,
        data(25)    type c,
        user        type sy-uname,
      end   of ty_saida.


types: begin of ty_estrutura.
include type slis_fieldcat_main.
include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

data:
      "t_rbkp      TYPE TABLE OF ty_rbkp,
      t_j_1bnfdoc type table of j_1bnfdoc,
      t_j_1bnfstx type table of j_1bnfstx,
      t_j_1baj    type table of ty_j_1baj,
      t_j_1bnflin type table of j_1bnflin,
      t_lfa1      type table of ty_lfa1,
      t_saida     type table of ty_saida,
      t_konv      type table of ty_konv,
      t_remessa   type table of ty_remessa.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

data: wa_j_1bnfdoc type j_1bnfdoc,
      wa_j_cabe    type j_1bindoc,
      wa_j_1bnfstx type j_1bnfstx,
      wa_j_1baj    type ty_j_1baj,
      wa_j_1bnflin type j_1bnflin,
      wa_konv      type ty_konv,
      wa_remessa   type ty_remessa,
      wa_saida     type ty_saida.

*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

data: x_data type d,
      x_hora type sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

data: xs_events    type slis_alv_event,
      events       type slis_t_event,
      t_print      type slis_print_alv,
      estrutura    type table of ty_estrutura,
      wa_estrutura type ty_estrutura,
      v_report     like sy-repid,
      t_top        type slis_t_listheader,
      t_sort       type slis_t_sortinfo_alv with header line.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
  parameter: p_bukrs  type ty_j_1bnfdoc-bukrs obligatory.

  select-options: p_branch for j_1bnfdoc-branch obligatory,
                  p_docdat for j_1bnfdoc-docdat obligatory no-extension,
                  p_docnum for j_1bnfdoc-docnum no intervals no-extension .

selection-screen: end of block b1.

selection-screen: begin of block b2 with frame title text-002.
  parameter: p_zfao  type j_1bnfdoc-cancel,
             p_zfeo  type j_1bnfdoc-cancel,
             p_zfea  type j_1bnfdoc-cancel,
             p_zima  type j_1bnfdoc-cancel,
             p_zfab  type j_1bnfdoc-cancel,
             p_zfeg  type j_1bnfdoc-cancel.
selection-screen: end of block b2.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
perform f_iniciar_variaves.
perform f_seleciona_dados.
perform f_organiza_dados.
perform f_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

form f_seleciona_dados .

  if ( p_zfao eq 'X' and ( p_zfeo eq 'X' or p_zfea eq 'X' or p_zima eq 'X' or p_zfab eq 'X' or p_zfeg eq 'X') ) or
     ( p_zfeo eq 'X' and ( p_zfao eq 'X' or p_zfea eq 'X' or p_zima eq 'X' or p_zfab eq 'X' or p_zfeg eq 'X') ) or
     ( p_zfea eq 'X' and ( p_zfeo eq 'X' or p_zfao eq 'X' or p_zima eq 'X' or p_zfab eq 'X' or p_zfeg eq 'X') ) or
     ( p_zima eq 'X' and ( p_zfeo eq 'X' or p_zfea eq 'X' or p_zfao eq 'X' or p_zfab eq 'X' or p_zfeg eq 'X') ) or
     ( p_zfab eq 'X' and ( p_zfeo eq 'X' or p_zfea eq 'X' or p_zima eq 'X' or p_zfao eq 'X' or p_zfeg eq 'X') ) or
     ( p_zfeg eq 'X' and ( p_zfeo eq 'X' or p_zfea eq 'X' or p_zima eq 'X' or p_zfab eq 'X' or p_zfao eq 'X') ).
    message i000(z01) with 'Selecione apenas um Tipo de Imposto !' .
    stop.
  endif.
  if p_zfao eq '' and p_zfeo eq '' and p_zfea eq '' and p_zima eq '' and p_zfab eq '' and p_zfeg eq ''.
    message i000(z01) with 'Selecione pelo menos um Tipo de Imposto !' .
    stop.
  endif.

  select *
    from j_1bnfdoc
    into table t_j_1bnfdoc
    where cancel ne 'X'
      and bukrs  eq p_bukrs
      and branch in p_branch
      and docdat in p_docdat
      and docnum in p_docnum.

endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
form f_organiza_dados .


  data: produto     type c length 40,
        tipo_ordem  type vbrk-fkart,
        docnum      type j_1bnfdoc-docnum,
        wa_lfa1     type lfa1.

  sort: t_j_1bnfdoc by docdat docnum nfnum,
        t_j_1bnflin by docnum,
        t_remessa   by aubel.

  loop at t_j_1bnfdoc into wa_j_1bnfdoc.

    select *
      from j_1bnflin
      into table t_j_1bnflin
        where docnum eq wa_j_1bnfdoc-docnum.

    select *
      from j_1bnfstx
      into table t_j_1bnfstx
        where docnum eq  wa_j_1bnfdoc-docnum.

*---> 05/07/2023 - Migração S4 - DL
    SORT t_j_1bnfstx BY docnum.
    sORT t_j_1bnflin BY docnum.
*<--- 05/07/2023 - Migração S4 - DL

    select vk~vbeln vk~fkart vp~aubel vk~knumv
      from vbrk as vk
       inner join vbrp as vp on vp~vbeln eq vk~vbeln
      into table t_remessa
      for all entries in t_j_1bnflin
        where vk~vbeln   eq t_j_1bnflin-refkey(10).

    read table t_j_1bnfstx into wa_j_1bnfstx
      with key docnum = wa_j_1bnfdoc-docnum
        binary search.

    "Cliente / Fornecedor / Local
    call function 'Z_PARCEIRO_INFO'
      exporting
        p_parceiro         = wa_j_1bnfdoc-parid
        p_partype          = wa_j_1bnfdoc-partyp
      changing
        wa_info_part       = wa_lfa1.
    "Imposto
    read table t_j_1bnflin into wa_j_1bnflin
      with key docnum = wa_j_1bnfdoc-docnum
        binary search.

    call function 'J_1B_NF_VALUE_DETERMINATION'
      exporting
        nf_header   = wa_j_1bnfdoc
      importing
        ext_header  = wa_j_cabe
      tables
        nf_item     = t_j_1bnflin
        nf_item_tax = t_j_1bnfstx.

    if sy-subrc is initial.
      read table t_remessa into wa_remessa
        with key vbeln  = wa_j_1bnflin-refkey(10)
        binary search.

        wa_saida-ordem       = wa_remessa-aubel.
        wa_saida-doc_fatura  = wa_remessa-vbeln.
        wa_saida-tipo_ordem  = wa_remessa-fkart.

*SV  Estorn.venda à vista
*SHR  Estorno adm.curso
*S1  Estorno fatura (S1)
*S3  Estorno fatura (S3)
*IVS  Estorno faturam.int.
*LRS  Estorno lis.faturas
*LGS  Estorno Listas NCréd
*IGS  Estorno NC interna
*S2  Estorno nota crédito
*FAS  Estorno sol.adiant.

      if ( wa_saida-tipo_ordem eq 'SV' )  or ( wa_saida-tipo_ordem eq 'SHR' ) or
         ( wa_saida-tipo_ordem eq 'S1' )  or ( wa_saida-tipo_ordem eq 'S3' )  or
         ( wa_saida-tipo_ordem eq 'IVS' ) or ( wa_saida-tipo_ordem eq 'LRS' ) or
         ( wa_saida-tipo_ordem eq 'LGS' ) or ( wa_saida-tipo_ordem eq 'IGS' ) or
         ( wa_saida-tipo_ordem eq 'S2' )  or ( wa_saida-tipo_ordem eq 'FAS' ).
        clear: wa_saida, wa_lfa1, wa_j_1bnfdoc,wa_j_1bnfstx, wa_j_1baj, wa_j_1bnflin, x_data, x_hora, tipo_ordem, wa_konv, wa_j_1bnfdoc, wa_remessa,wa_j_cabe.
        continue.
      endif.


      TRY.

CL_PRC_RESULT_FACTORY=>GET_INSTANCE( )->GET_PRC_RESULT( )->GET_PRICE_ELEMENT_DB(
  EXPORTING IT_SELECTION_ATTRIBUTE = VALUE #(
 ( fieldname = 'KNUMV' value = WA_REMESSA-KNUMV )
 )
  IMPORTING ET_PRC_ELEMENT_CLASSIC_FORMAT = DATA(ETL306C6R1041) ).
  CLEAR T_KONV.
  TYPES: BEGIN OF TYL306C6R1548,
    KNUMV TYPE KONV-KNUMV,
    KWERT TYPE KONV-KWERT,
    KBETR TYPE KONV-KBETR,
    KSCHL TYPE KONV-KSCHL,
  END OF TYL306C6R1548.
  DATA: LML306C6R2474 TYPE TYL306C6R1548,
        LWL306C6R7944 LIKE LINE OF T_KONV.
  LOOP AT ETL306C6R1041 REFERENCE INTO DATA(LDRL306C6R7725).
    LML306C6R2474-KNUMV = LDRL306C6R7725->KNUMV.
    LML306C6R2474-KWERT = LDRL306C6R7725->KWERT.
    LML306C6R2474-KBETR = LDRL306C6R7725->KBETR.
    LML306C6R2474-KSCHL = LDRL306C6R7725->KSCHL.
    LWL306C6R7944 = LML306C6R2474.
    APPEND LWL306C6R7944 TO T_KONV.
  ENDLOOP.
CATCH CX_PRC_RESULT .
  SY-SUBRC = 4.
ENDTRY.

      if p_zfao eq 'X' .

        delete t_konv where kschl ne 'ZFAO'.

      elseif p_zfeo  eq 'X' .

        delete t_konv where kschl ne 'ZFEO'.

      elseif  p_zfea  eq 'X' .

        delete t_konv where kschl ne 'ZFEA'.

      elseif p_zima  eq 'X' .

        delete t_konv where kschl ne 'ZIMA'.

      elseif p_zfab eq 'X' .

        delete t_konv where kschl ne 'ZFAB'.

      elseif p_zfeg eq 'X' .

        delete t_konv where kschl ne 'ZFEG'.

      endif.

      read table t_konv into wa_konv
        with key knumv = wa_remessa-knumv
        binary search.

      select single maktx
        from makt
        into produto
        where matnr eq wa_j_1bnflin-matnr.

    endif.

    x_data = sy-datum.
    x_hora = sy-uzeit.

    concatenate x_data+6(2) '/'
                x_data+4(2) '/'
                x_data(4)   ' -  '
                x_hora(2)   ':'
                x_hora+2(2) ':'
                x_hora+4(2) into wa_saida-data.

    wa_saida-quantidade  = wa_j_1bnflin-menge.
    wa_saida-Tributo     = wa_konv-kwert.
    wa_saida-percentual  = wa_konv-kbetr.
    wa_saida-nome_clifor = wa_lfa1-name1.
    wa_saida-cpf         = wa_lfa1-stcd1.
    wa_saida-municipio   = wa_lfa1-ort01.
    wa_saida-uf          = wa_lfa1-regio.
    wa_saida-netwrt      = wa_j_cabe-nftot.
    wa_saida-matnr       = wa_j_1bnflin-matnr.
    wa_saida-docnum      = wa_j_1bnfdoc-docnum.
    wa_saida-NFENUM      = wa_j_1bnfdoc-NFENUM.
    wa_saida-docdat      = wa_j_1bnfdoc-docdat.

    wa_saida-produto     = produto.
    wa_saida-user        = sy-uname.

    if wa_saida-Tributo > 0 .
      append wa_saida to t_saida.
    else.
      clear: wa_saida, wa_lfa1, wa_j_1bnfdoc,wa_j_1bnfstx, wa_j_1baj, wa_j_1bnflin, x_data, x_hora, tipo_ordem, wa_konv, wa_j_1bnfdoc, wa_remessa,wa_j_cabe.
      continue.

    endif.

    clear: wa_saida, wa_lfa1, wa_j_1bnfdoc,wa_j_1bnfstx, wa_j_1baj, wa_j_1bnflin, x_data, x_hora, tipo_ordem, wa_konv, wa_j_1bnfdoc, wa_remessa, wa_j_cabe.

  endloop.

endform.                    " F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

form f_imprime_dados .
  if t_saida[] is initial.
    message i000(z01) with 'Não foram encontrados dados para os parametros'
                           'informados' .
    stop.
  endif.
  perform f_definir_eventos.
  perform f_alv_sort.
  perform f_montar_layout.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    tables
      t_outtab                = t_saida.

endform.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*

form f_definir_eventos .
  perform f_carregar_eventos using:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
endform.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*

form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.
endform.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

form f_montar_layout.
  perform f_montar_estrutura using:

   "TABELA "CAMPO    "TAB INTERNA  "VARIAVEL DA WA "CAPTION

   1 ''   ''      'T_SAIDA' 'NOME_CLIFOR'  'Nome do Fornecedor'     ' ',
   2 ''   ''      'T_SAIDA' 'CPF'          'CPF'                    ' ',
   3 ''   ''      'T_SAIDA' 'MUNICIPIO'    'Municipio'              ' ',
   4 ''   ''      'T_SAIDA' 'UF'           'UF'                     ' ',
   5 ''   ''      'T_SAIDA' 'NFENUM'        'Número da nota'         ' ',
   6 ''   ''      'T_SAIDA' 'DOCNUM'       'DOC_NUM'                ' ',
   7 ''   ''      'T_SAIDA' 'DOCDAT'       'Data do Documento'      ' ',
   8 ''   ''      'T_SAIDA' 'MATNR'        'Código material'        ' ',
   9 ''   ''      'T_SAIDA' 'PRODUTO'      'Descrição'              ' ',
  10 ''   ''      'T_SAIDA' 'QUANTIDADE'   'Quantidade'             ' ',
  10 ''   ''      'T_SAIDA' 'TRIBUTO'      'Valor Tributo'          ' ',
  11 ''   ''      'T_SAIDA' 'NETWRT'       'Valor da Nota'          ' ',
  13 ''   ''      'T_SAIDA' 'TIPO_ORDEM'   'Tipo de Ordem'          ' '.


endform.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
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
form f_montar_estrutura using value(p_col_pos)       type i
                              value(p_ref_tabname)   like dd02d-tabname
                              value(p_ref_fieldname) like dd03d-fieldname
                              value(p_tabname)       like dd02d-tabname
                              value(p_field)         like dd03d-fieldname
                              value(p_scrtext_l)     like dd03p-scrtext_l
                              value(p_outputlen).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.


  if p_field eq 'DOCNUM'.
    wa_estrutura-hotspot = 'X'.
  else.
    clear wa_estrutura-hotspot.
  endif.

  append wa_estrutura to estrutura.

endform.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xtop_of_page.                                          "#EC CALLED

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_top.
*            I_LOGO             = ''.

endform. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar_variaves.

  data: w_texto1(40).
  data: w_texto2(20).

  v_report = sy-repid.

*** Nome do Report
  perform f_construir_cabecalho using 'H' text-002.

  select single butxt from t001 into w_texto2
    where bukrs eq p_bukrs.

  concatenate 'Empresa:' p_bukrs '-' w_texto2 into w_texto1 separated by space.
*** Nome da empresa
  perform f_construir_cabecalho using 'H' w_texto1.

  if not p_branch is initial.

    select single name1 from t001w into w_texto2
      where werks = p_branch.

    concatenate 'Filial:' p_branch  '-' w_texto2 into  w_texto1 separated by space.
    perform f_construir_cabecalho using 'S' w_texto1.
  endif.

  write: sy-datum to w_texto2.
  concatenate 'Data:' w_texto2 into w_texto1 separated by space.
  perform f_construir_cabecalho using 'S' w_texto1.
  write: sy-uzeit to w_texto2.
  concatenate 'Hora:' w_texto2 into w_texto1 separated by space.
  perform f_construir_cabecalho using 'S' w_texto1.
  concatenate 'Usuário:' sy-uname into w_texto1 separated by space.
  perform f_construir_cabecalho using 'S' w_texto1.
endform.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
form f_construir_cabecalho using typ text.

  data: ls_line type slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  append ls_line to t_top.


*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.



endform.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_sort.

*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUKRS'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 1.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUTXT'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 2.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'GSBER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 3.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'NAME'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 4.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'DATA'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 5.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'USER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 6.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
endform.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
form f_user_command using l_ucomm
                          l_selfield type slis_selfield.

  if l_selfield-fieldname = 'DOCNUM'.
    read table t_saida index l_selfield-tabindex into wa_saida.

    set parameter id 'JEF' field l_selfield-value.

    call transaction 'J1B3N' and skip first screen.
    "CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  endif.

endform.                    "f_user_command
