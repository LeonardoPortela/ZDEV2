*&---------------- NINJA EVOLUION -  A M A G G I-----------------------*
* Programa   : ZFIS45                                                  *
* Descrição  : RELATORIO P/ LANÇAMENTOS SEM CATEGORIA                  *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Ronaldo Freitas                        Data: 08/02/2024 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
report  zfis45.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
* TIPOS PARA ALV
*----------------------------------------------------------------------*

type-pools: slis.
tables : t001, t001w, bkpf, ska1, bseg, rbkp, j_1bnfdoc,j_1bnflin.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

types:
  begin of ty_t001w      ,
    werks type t001w-werks,
    lifnr type t001w-lifnr,
    kunnr type t001w-kunnr,
  end   of ty_t001w      ,

  begin of ty_bnflin,
    docnum type j_1bnflin-docnum,
    refkey type j_1bnflin-refkey,
    netwr  type j_1bnflin-netwr,
  end of ty_bnflin,

  begin of ty_bnfdoc,
    docnum type j_1bnfdoc-docnum,
    nfenum type j_1bnfdoc-nfenum,
    series type j_1bnfdoc-series,
    pstdat type j_1bnfdoc-pstdat,
    nfe    type j_1bnfdoc-nfe,
    nfnum  type j_1bnfdoc-nfnum,
  end of ty_bnfdoc,

  begin of ty_j_1bnfe_active_nfe,
    docnum  type j_1bnfe_active-docnum,
    regio   type j_1bnfe_active-regio,
    nfyear  type j_1bnfe_active-nfyear,
    nfmonth type j_1bnfe_active-nfmonth,
    stcd1   type j_1bnfe_active-stcd1,
    model   type j_1bnfe_active-model,
    serie   type j_1bnfe_active-serie,
    nfnum9  type j_1bnfe_active-nfnum9,
    docnum9 type j_1bnfe_active-docnum9,
    cdv     type j_1bnfe_active-cdv,
  end of ty_j_1bnfe_active_nfe,


  begin of ty_ekbe,
    ebeln type ekbe-ebeln,
    vgabe type ekbe-vgabe,
    lfbnr type ekbe-lfbnr,
    belnr type ekbe-belnr,
    gjahr type ekbe-gjahr,
  end of ty_ekbe,

  begin of ty_rbkp,
    bukrs type rbkp-bukrs,
    belnr type rbkp-belnr,
    gjahr type rbkp-gjahr,
    rmwwr type rbkp-rmwwr,
    budat type rbkp-budat,
    xblnr type vttk-tknum,
    awkey type bkpf-awkey,
    stblg type rbkp-stblg,
  end of ty_rbkp,

  begin of ty_bkpf,
    bukrs type bkpf-bukrs,
    gjahr type bkpf-gjahr,
    awkey type bkpf-awkey,
    belnr type bkpf-belnr,
  end of ty_bkpf,

  begin of ty_rbco,
    belnr	  type rbco-belnr,
    gjahr	  type rbco-gjahr,
    buzei	  type rbco-buzei,
    cobl_nr	type rbco-cobl_nr,
    wrbtr   type rbco-wrbtr,
    saknr   type rbco-saknr,
  end of ty_rbco,

  begin of ty_bsis,
    bukrs	type bsis-bukrs,
    hkont	type bsis-hkont,
    augdt	type bsis-augdt,
    augbl	type bsis-augbl,
    zuonr	type bsis-zuonr,
    gjahr	type bsis-gjahr,
    belnr	type bsis-belnr,
    buzei	type bsis-buzei,
    dmbtr type bsis-dmbtr,
  end of ty_bsis,

  begin of ty_zlest0002,
    pc_veiculo type zlest0002-pc_veiculo,
    cd_cidade  type zlest0002-cd_cidade,
    cd_uf      type zlest0002-cd_uf,
  end of ty_zlest0002,

  begin of ty_saida,
    bschl   type bseg-bschl,  " Chave do lançamento
    gsber   type bseg-gsber,  " Filial
    bukrs   type vfkp-bukrs,  " Empresa
    belnr   type bkpf-belnr,  " Documento contábil
    hkont   type bseg-hkont,  "  bseg-ktonr,  " Conta Despesa Pedido
    sgtxt   type bseg-sgtxt,  " Texto das contas
    budat   type bkpf-budat,  " Data lançamento
    bldat   type bkpf-bldat,  " Data documento
    dmbtr   type bseg-dmbtr, " Valor Fatura
    kostl   type ekkn-kostl,  " Centro Custo
    ltext   type cskt-ltext ,  " Descrição Centro Custo
    lifnr   type bseg-lifnr ,  " Participante - Fornecedor
    awkey   type bkpf-awkey,  " Nº MIRO
    ebeln   type bseg-ebeln, " Nº Pedido
    nftype  type j_1bnfdoc-nftype, "Categoria da nota
    cod_cta type j_1bnflin-cod_cta, "Conta contabil
  end  of ty_saida,

  begin of ty_saidav,
    bukrs   type rbkp-bukrs,  " Empresa -
    gsber   type rbkp-gsber,  " Filial -
    belnr   type bkpf-belnr,  " Documento contábil -
    budat   type rbkp-budat,  " Data lançamento -
    bldat   type rbkp-bldat,  " Data documento -
    rmwwr   type rbkp-rmwwr,  " Valor Fatura -
    fwste   type bset-fwste,  " PIS -
    fwstec  type bset-fwste,  " COFINS -
    kostl   type ekkn-kostl,  " Centro Custo -
    ltext   type cskt-ltext , " Descrição Centro Custo -
    lifnr   type lfa1-name1 , " *Participante - Fornecedor  RBKP-LIFNR + LFA1-NAME1
    belnrm  type rbkp-belnr,  " Nº MIRO
    ebeln   type ekbe-ebeln,  " Nº Pedido
    matnr   type ekpo-matnr,  " Cod. Material
    txz01   type ekpo-txz01,  " Desc. Material
    nftype  type j_1bnfdoc-nftype, "Categoria da nota
    cod_cta type j_1bnflin-cod_cta, "Conta contabil
  end  of ty_saidav.




*Participante - Fornecedor  RBKP-LIFNR + LFA1-NAME1
*Nº MIRO  RBKP-BELNR
*Nº Pedido  EKBE-EBELN
*Cod. Material
*EKPO-MATNR
*Desc. Material
*EKPO-TXZ01




types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*&---------------------------------------------------------------------*
*& Constantes
*&---------------------------------------------------------------------*

*constants:
*c_pc(2)            type c value 'PC',
*c_lr(2)            type c value 'LR'.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

data:
  t_lfa1               type table of lfa1,
  t_t001w              type table of ty_t001w,
  t_saida              type table of ty_saida,
  t_saidav             type table of ty_saidav,
  wa_saidav            type ty_saidav,
  t_vfsi               type table of vfsi,
  t_tvtkt              type table of tvtkt,
  t_zlest0110          type table of zlest0110 with header line,
  t_bnfdoc             type table of ty_bnfdoc with header line,
  t_bnflin             type table of ty_bnflin with header line,
  t_ekbe               type table of ty_ekbe,
  t_rbkp               type table of ty_rbkp,
  t_rbkp_aux           type table of ty_rbkp,
  t_bkpf               type table of ty_bkpf,
  t_rbco               type table of ty_rbco,
  t_bsis               type table of ty_bsis,
  t_zlest0002          type table of ty_zlest0002,
  t_zlest0032          type table of zlest0032,
  t_j_1bnflin          type table of j_1bnflin,
  t_lips               type table of lips,
  t_j_1bbranch         type table of j_1bbranch,
  t_bseg               type table of bseg,
  t_j_1bnfe_active_nfe type table of ty_j_1bnfe_active_nfe,
  t_j_1btindtypt       type table of j_1btindtypt,
  t_dd07t              type table of dd07t.





*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

data:
  wa_t001w              type ty_t001w,
  wa_j_1bnfstx          type j_1bnfstx,
  wa_vfsi               type vfsi,
  wa_tvtkt              type tvtkt,
  wa_ekbe               type ty_ekbe,
  wa_saida              type ty_saida,
  wa_rbkp               type ty_rbkp,
  wa_bkpf               type ty_bkpf,
  wa_zlest0002          type ty_zlest0002,
  wa_zlest0032          type zlest0032,
  wa_j_1bnflin          type j_1bnflin,
  wa_lips               type lips,
  wa_j_1bbranch         type  j_1bbranch,
  wa_j_1bbranch_aux     type  j_1bbranch,
  wa_bseg               type bseg,
  wa_j_1bnfe_active_nfe type ty_j_1bnfe_active_nfe.

*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

data: x_data   type d,
      x_hora   type sy-uzeit,
      vg_tabix type sy-tabix.

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

data: w_variant type disvariant,
      w_layout  type slis_layout_alv.

ranges: r_mwskz1             for rbkp-mwskz1.
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
  select-options:
*                  s_bukrs  FOR  t001-bukrs OBLIGATORY,
*                  s_werks  FOR  t001w-werks OBLIGATORY, " NO INTERVALS NO-EXTENSION,
*                  s_belnr  FOR  bkpf-belnr,
**                  s_saknr  FOR  ska1-saknr,
**                  s_zuonr  FOR  bseg-zuonr,
*                  s_blart  FOR  bkpf-blart,
*                  s_budat  FOR  bkpf-budat OBLIGATORY,
*                  s_bldat  FOR  bkpf-bldat.

                  s_bukrs  for  rbkp-bukrs obligatory,
                  s_werks  for  rbkp-gsber obligatory, " NO INTERVALS NO-EXTENSION,
                  s_belnr  for  rbkp-belnr,
                  s_blart  for  rbkp-blart,
                  s_nftype for j_1bnfdoc-nftype,
                  s_budat  for  rbkp-budat obligatory,
                  s_bldat  for  rbkp-bldat,
                  p_conta  for j_1bnflin-cod_cta.

*  "p_add02  for  vttk-add02 no intervals no-extension.
*  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-009.
*    PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD'. "Layout
*  SELECTION-SCREEN END OF BLOCK b2.

selection-screen: end of block b1.

"&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
perform f_iniciar_variaves.

if s_budat[] is not initial.

*  PERFORM f_seleciona_dados.
  perform f_seleciona_dados_v2.
  perform f_imprime_dadosv.
*  PERFORM f_imprime_dados.

else.

  message s000(z01) with 'Campo data inválido.' 'Informar Data Registro'
        display like 'E'.
  return.
endif.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

form f_seleciona_dados .

*select
*B.BSCHL as Chave_lancamento,
*A.BUKRS as Empresa,
*B.GSBER as Filial,
*A.BUDAT as Data_lancamento,
*A.BLDAT as Data_documento,
*A.BELNR as Documento_contabil,
*B.LIFNR as Participante_Fornecedor,
*B.HKONT as Conta_Despesa_Pedido,
*B.SGTXT as Texto_contas,
*B.DMBTR as Valor_Fatura,
*B.EBELN as N_Pedido,
*A.AWKEY as N_MIRO,
*A.BUKRS as Centro_Custo,
*A.BUKRS as Descricao_CentroCusto
*from BSEG B
*Inner join BKPF A on (A.BELNR=B.BELNR and A.BUKRS=B.BUKRS )
*where A.belnr = 5101437270

*select * from RBKP
*  into table @data(it_rbkp)
*  WHERE bukrs   IN @s_bukrs
*    AND gsber   IN @s_werks
*    AND belnr   IN @s_belnr
*    and blart   IN @s_blart
*    AND budat   IN @s_budat.

* Ir na BSEG tabela principal.
******>>>>>Inicio melhoria USER STORY 158506 / AOENNING <<<<<<<<<<
*  if s_nftype is not initial.
  select b~bukrs, b~gsber, b~belnr, b~ebeln, b~ebelp, b~lifnr, b~awkey, b~bschl, b~dmbtr, b~hkont, b~sgtxt,
       a~gjahr, a~budat, a~bldat
  from bseg as b
  inner join bkpf as a on ( a~belnr = b~belnr and a~bukrs = b~bukrs )
  into table @data(lt_bseg)
  where b~bukrs   in @s_bukrs
    and b~gsber   in @s_werks
    and b~belnr   in @s_belnr
    and a~budat   in @s_budat.

******>>>>>Fim melhoria USER STORY 158506 / AOENNING <<<<<<<<<<
*  else.
*    select b~bukrs, b~gsber, b~belnr, b~ebeln, b~ebelp, b~lifnr, b~awkey, b~bschl, b~dmbtr, b~hkont, b~sgtxt,
*           a~gjahr, a~budat, a~bldat
*      from bseg as b
*      inner join bkpf as a on ( a~belnr = b~belnr and a~bukrs = b~bukrs )
*      into table @lt_bseg
*      where b~bukrs   in @s_bukrs
*        and b~gsber   in @s_werks
*        and b~belnr   in @s_belnr
*        and a~budat   in @s_budat.
*  endif.

  if sy-subrc is initial.

    select * from tbsl
    into table @data(lt_tbsl)
    for all entries in @lt_bseg
    where bschl eq @lt_bseg-bschl.

    sort lt_bseg by bukrs gsber belnr.

* Centro de custo -
    select ebeln, ebelp, kostl from ekkn
      into table @data(lt_ekkn)
      for all entries in @lt_bseg
      where ebeln eq @lt_bseg-ebeln
        and ebelp eq @lt_bseg-ebelp.
    if sy-subrc is initial.

      sort lt_ekkn by ebeln ebelp.

      select kostl, ltext
        into table @data(lt_cskt)
        from cskt
        for all entries in @lt_ekkn
        where kostl eq @lt_ekkn-kostl.
      if sy-subrc is initial.

        sort lt_cskt by kostl.

        select bukrs, gsber, belnr, ebeln, ebelp, lifnr, awkey from bseg
          into table @data(lt_bseg_aux)
          where bukrs in @s_bukrs
            and belnr in @s_belnr
            and bschl eq '31'.
        if sy-subrc is initial.

          sort lt_bseg_aux by bukrs belnr.

          select bukrs, belnr, gjahr, awkey, budat, bldat from bkpf
            into table @data(lt_bkpf)
            for all entries in @lt_bseg_aux
            where budat in @s_budat
              and bldat in @s_bldat
              and awkey eq @lt_bseg_aux-awkey.
          if sy-subrc is initial.
            sort lt_bkpf by awkey.
          endif.

        endif.

      endif.

      select bukrs, gsber, belnr, ebeln, ebelp, lifnr, awkey from bseg
        into table @lt_bseg_aux
        where bukrs in @s_bukrs
          and belnr in @s_belnr
          and bschl eq '31'.
      if sy-subrc is initial.
        sort lt_bseg_aux by bukrs belnr.
      endif.
    endif.

  endif.


  data: vg_tabix type sy-tabix.

  loop at lt_bseg into data(wa_bseg).

    vg_tabix = sy-tabix.

* bukrs, gsber, belnr, ebeln, ebelp, lifnr,
    wa_saida-bukrs = wa_bseg-bukrs.
    wa_saida-gsber = wa_bseg-gsber.
    wa_saida-belnr = wa_bseg-belnr.
    wa_saida-ebeln = wa_bseg-ebeln.
*    wa_saida-nftype = wa_bseg-nftype.
    wa_saida-bschl = wa_bseg-bschl.

    read table lt_tbsl into data(wa_tbsl) with key bschl = wa_bseg-bschl.
    if sy-subrc is initial.
      if wa_tbsl-shkzg eq 'H'.
        wa_saida-dmbtr = wa_bseg-dmbtr * ( - 1 ).
      else.
        wa_saida-dmbtr = wa_bseg-dmbtr.
      endif.
    endif.
*    wa_saida-ebelp = wa_bseg-ebelp.
    wa_saida-lifnr = wa_bseg-lifnr.
    wa_saida-hkont = wa_bseg-hkont.
    wa_saida-sgtxt = wa_bseg-sgtxt.
    wa_saida-awkey = wa_bseg-awkey.

    data(lt_bsegd) = lt_bseg.
    delete lt_bsegd where lifnr is initial.


    read table lt_bsegd into data(wa_bsegx) with key belnr = wa_bseg-belnr.
    if sy-subrc is initial.
      wa_saida-lifnr = wa_bsegx-lifnr.
    endif.

    read table lt_ekkn into data(wa_ekkn) with key ebeln = wa_bseg-ebeln
                                                    ebelp = wa_bseg-ebelp binary search.
    if sy-subrc is initial.
      wa_saida-kostl = wa_ekkn-kostl.
    endif.

    if wa_saida-kostl is not initial.
      read table lt_cskt into data(wa_cskt) with key kostl = wa_ekkn-kostl binary search.
      if sy-subrc is initial.
        wa_saida-ltext = wa_cskt-ltext.
      endif.

    else.
      clear wa_saida-ltext.
    endif.

    read table lt_bseg_aux into data(wa_bseg_aux) with key bukrs = wa_bseg-bukrs
                                                            belnr = wa_bseg-belnr binary search.

    if sy-subrc is initial.

      if wa_saida-awkey is initial.
        wa_saida-awkey = wa_bseg_aux-awkey.
      endif.

      read table lt_bkpf into data(wa_bkpf) with key awkey = wa_bseg_aux-awkey binary search.

      if sy-subrc is initial.
        wa_saida-budat = wa_bkpf-budat.
        wa_saida-bldat = wa_bkpf-bldat.

        append wa_saida to t_saida.
        clear wa_saida.

      endif.
    endif.

  endloop.

endform.                    " F_SELECIONA_DADOS
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
      is_layout               = w_layout
      is_variant              = w_variant
    tables
      t_outtab                = t_saida.

endform.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

form f_imprime_dadosv .
  if t_saidav[] is initial.
    message i000(z01) with 'Não foram encontrados dados para os parametros'
                           'informados' .
    stop.
  endif.

  perform f_definir_eventos.
  perform f_alv_sort.
*  PERFORM f_montar_layout.
  perform f_montar_layoutv.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
      is_layout               = w_layout
      is_variant              = w_variant
    tables
      t_outtab                = t_saidav.

endform.                    " F_IMPRIME_DADOSv

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

*Chave do lançamento  Empresa Filial  Data lançamento Data documento  Documento contábil
*Fornecedor Conta Despesa Pedido  Texto das contas  Valor Fatura  Nº Pedido Nº MIRO
*Centro Custo	Descrição

  perform f_montar_estrutura using:

  1 ''   ''      'T_SAIDA' 'BSCHL'        'Chave do lançamento'     ' ' ' ',
  2 ''   ''      'T_SAIDA' 'BUKRS'        'Empresa'                 ' ' ' ',
  3 ''   ''      'T_SAIDA' 'GSBER'        'Filial'                  ' ' ' ',
  4 ''   ''      'T_SAIDA' 'BUDAT'        'Data lançamento'         ' ' ' ',
  5 ''   ''      'T_SAIDA' 'BLDAT'        'Data documento'          ' ' ' ',
  6 ''   ''      'T_SAIDA' 'BELNR'        'Documento contábil'      ' ' ' ',
  7 ''   ''      'T_SAIDA' 'LIFNR'        'Fornecedor'              ' ' ' ',
  8 ''   ''      'T_SAIDA' 'HKONT'        'Conta Despesa Pedido'    ' ' ' ',
  9 ''   ''      'T_SAIDA' 'SGTXT'        'Texto das contas'        ' ' ' ',
 10 ''   ''      'T_SAIDA' 'DMBTR'        'Valor Fatura'            ' ' ' ',
 11 ''   ''      'T_SAIDA' 'EBELN'        'Nº Pedido'               ' ' ' ',
 12 ''   ''      'T_SAIDA' 'AWKEY'        'Nº MIRO'                 ' ' ' ',
 13 ''   ''      'T_SAIDA' 'KOSTL'        'Centro Custo'            ' ' ' ',
 14 ''   ''      'T_SAIDA' 'LTEXT'        'Descrição'               ' ' ' ',
 15 ''   ''      'T_SAIDA' 'NFTYPE'       'Categoria da nota'       ' ' ' '.

  clear: w_variant, w_layout.
  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra      = 'X'.
*  layout-box_fieldname = 'CHK'.
*  w_layout-box_fieldname = 'CHECK'.
  w_variant-report    = sy-repid.
*  w_variant-variant   = p_layout.

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
                              value(p_outputlen)
                              value(p_no_zero)        type c  .

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
  wa_estrutura-no_zero       = p_no_zero.

  if ( p_field eq 'TKNUM' ) or ( p_field eq 'BELNR' ).
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
    where bukrs eq s_bukrs-low.

  concatenate 'Empresa:' s_bukrs-low '-' w_texto2 into w_texto1 separated by space.
*** Nome da empresa
  perform f_construir_cabecalho using 'H' w_texto1.

  if not s_werks is initial.

    select single name1 from t001w into w_texto2
      where werks = s_werks-low.

    concatenate 'Filial:' s_werks-low  '-' w_texto2 into  w_texto1 separated by space.
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

  if l_selfield-fieldname = 'TKNUM'.

    read table t_saida index l_selfield-tabindex into wa_saida.

    set parameter id 'TNR' field l_selfield-value.

    call transaction 'VT03N' and skip first screen.

  endif.

  if l_selfield-fieldname = 'BELNR'.


    break rfreitas.

    read table t_saidav index l_selfield-tabindex into wa_saidav.

    set parameter id 'BLN' field wa_saidav-belnr.
    set parameter id 'BUK' field wa_saidav-bukrs.
    set parameter id 'GJR' field wa_saidav-budat(4).
    call transaction 'FB03' and skip first screen.

*    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

*    SET PARAMETER ID 'RBN' FIELD wa_saida-belnr.
**    SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
*    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.


  endif.

endform.                    "f_user_command
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados_v2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_seleciona_dados_v2 .

  types: begin of ty_rbkp,
           awkey type awkey.
           include type rbkp.
types end of ty_rbkp.

  data: it_rbkp type table of ty_rbkp.
* Buscar Parametrização
*stvarv
**I0 Industr: Somente PIS/COFINS
**Y4 Consumo: PIS/COFINS dedutível
**Y1 Consumo: PIS/COFINS dedutível
**S1 Serviço: PIS/COFINS dedutível

*  break rfreitas.

  select * from tvarvc
    into table @data(it_tvarv)
    where name eq 'IVA_ZFIS69'.
  if sy-subrc is initial.

    loop at it_tvarv into data(wa_tvarv).
      r_mwskz1-sign   = wa_tvarv-sign.
      r_mwskz1-option = wa_tvarv-opti.
      r_mwskz1-low    = wa_tvarv-low.
      append r_mwskz1 to r_mwskz1.
      clear r_mwskz1.
    endloop.
  else.
    free r_mwskz1.
  endif.




* Cabeçalho doc.da fatura recebida
  select * from rbkp as a
*  inner join rseg as b on a~belnr eq a~belnr and b~gjahr eq a~gjahr
  into corresponding fields of table @it_rbkp
    where a~bukrs   in @s_bukrs
      and a~gsber   in @s_werks
      and a~belnr   in @s_belnr
      and a~blart   in @s_blart
      and a~budat   in @s_budat
      and a~bldat   in @s_bldat
      and a~j_1bnftype in @s_nftype
      and not exists ( select * from rseg
                   where belnr eq a~belnr
                     and gjahr eq a~gjahr
                     and bukrs in @s_bukrs
                     and belnr   in @s_belnr
                     and mwskz  in @r_mwskz1 ). "( 'I0', 'Y4', 'Y1', 'S1' ). " Parametrização pend...

  if sy-subrc is initial.
    sort it_rbkp by bukrs belnr gjahr.

    loop at it_rbkp into data(wa_rbkp).
      wa_rbkp-awkey = wa_rbkp-belnr && wa_rbkp-gjahr.
      modify it_rbkp from wa_rbkp index sy-tabix.
    endloop.

    select a~bukrs, a~belnr, a~gjahr, a~awkey
    from bkpf as a
    into table @data(it_bkpf)
      for all entries in @it_rbkp
      where  a~bukrs eq @it_rbkp-bukrs
         and a~awkey eq @it_rbkp-awkey.
    if sy-subrc is initial.
      sort it_bkpf by bukrs awkey.

      select *
      from bseg
      into table @data(it_bseg)
      for all entries in @it_bkpf
      where awkey eq @it_bkpf-awkey
        and buzid eq 'S'. ">>>>>Melhoria USER STORY 158506 / AOENNING <<<<<<<<<<aa
    endif.





* Dados fiscais do segmento do documento
    select * from bset
      into table @data(it_bset)
      for all entries in @it_bkpf
      where bukrs eq @it_bkpf-bukrs
        and belnr eq @it_bkpf-belnr
        and gjahr eq @it_bkpf-gjahr
        and ktosl in ( 'PI3', 'CO3' ).
    if sy-subrc is initial.
      sort it_bset by bukrs belnr gjahr ktosl.
    endif.

    select * from lfa1
      into table t_lfa1
      for all entries in it_rbkp
    where lifnr eq it_rbkp-lifnr.
    if sy-subrc is initial.
      sort t_lfa1 by lifnr.
    endif.

* Histórico para o documento de compra
    select * from ekbe
      into table @data(it_ekbe)
    for all entries in @it_rbkp
      where
            belnr eq @it_rbkp-belnr
        and gjahr eq @it_rbkp-gjahr.
*      AND buzei EQ @it_rbkp-buzei.
    if sy-subrc is initial.
      sort it_ekbe by belnr gjahr.

* Centro de custo
      select ebeln, ebelp, kostl from ekkn
        into table @data(lt_ekkn)
        for all entries in @it_ekbe
        where ebeln eq @it_ekbe-ebeln
          and ebelp eq @it_ekbe-ebelp.
      if sy-subrc is initial.
        sort lt_ekkn by ebeln ebelp.

* Textos de centros de custo
        select kostl, ltext
          into table @data(lt_cskt)
          from cskt
          for all entries in @lt_ekkn
          where kostl eq @lt_ekkn-kostl.
        if sy-subrc is initial.
          sort lt_cskt by kostl.
        endif.

        select * from ekpo
          into table @data(it_ekpo)
          for all entries in @lt_ekkn
          where ebeln eq @lt_ekkn-ebeln
            and ebelp eq @lt_ekkn-ebelp.
        if sy-subrc is initial.
          sort it_ekpo by ebeln ebelp.
        endif.

      endif.
    endif.
  endif.

  if it_rbkp is not initial.

    loop at it_rbkp into wa_rbkp.

      wa_saidav-bukrs  = wa_rbkp-bukrs.
      wa_saidav-gsber  = wa_rbkp-gsber.
      wa_saidav-budat  = wa_rbkp-budat.
      wa_saidav-bldat  = wa_rbkp-bldat.
      wa_saidav-rmwwr  = wa_rbkp-rmwwr.
      wa_saidav-belnrm = wa_rbkp-belnr.
      wa_saidav-nftype = wa_rbkp-j_1bnftype. ">>>>>Melhoria USER STORY 158506 / AOENNING <<<<<<<<<<

      read table it_bkpf into data(wa_bkpf) with key bukrs = wa_rbkp-bukrs
                                                     awkey = wa_rbkp-awkey
*                                                     belnr = wa_rbkp-belnr
*                                                     gjahr = wa_rbkp-gjahr
                                                     binary search.
      if sy-subrc is initial.
        wa_saidav-belnr  = wa_bkpf-belnr.
        read table it_bseg into data(wa_bseg) with key awkey = wa_bkpf-awkey.
        if sy-subrc eq 0.
          wa_saidav-cod_cta = wa_bseg-altkt. ">>>>>Melhoria USER STORY 158506 / AOENNING <<<<<<<<<<
        endif.
      endif.

      read table it_bset into data(wa_bset) with key bukrs = wa_bkpf-bukrs
                                                     belnr = wa_bkpf-belnr
                                                     gjahr = wa_bkpf-gjahr
                                                     ktosl = 'PI3'
                                                     binary search.

      if sy-subrc is initial.
        wa_saidav-fwste  = wa_bset-fwste.
      endif.

      read table it_bset into wa_bset with key bukrs = wa_bkpf-bukrs
                                                     belnr = wa_bkpf-belnr
                                                     gjahr = wa_bkpf-gjahr
                                                     ktosl = 'CO3'
                                                     binary search.

      if sy-subrc is initial.
        wa_saidav-fwstec = wa_bset-fwste.
      endif.



      read table it_ekbe into data(wa_ekbe) with key belnr = wa_rbkp-belnr
                                                     gjahr = wa_rbkp-gjahr
*                                                     buzei = wa_rbkp-buzei
                                                     binary search.
      if sy-subrc is initial.
        wa_saidav-ebeln = wa_ekbe-ebeln.
      endif.

      read table lt_ekkn into data(wa_ekkn) with key ebeln = wa_ekbe-ebeln
                                                     ebelp = wa_ekbe-ebelp
                                                     binary search.
      if sy-subrc is initial.

        read table lt_cskt into data(wa_cskt) with key kostl = wa_ekkn-kostl
                                                       binary search.
        if sy-subrc is initial.
          wa_saidav-kostl = wa_cskt-kostl.
          wa_saidav-ltext = wa_cskt-ltext.
        endif.

        read table it_ekpo into data(wa_ekpo) with key ebeln = wa_ekkn-ebeln
                                                       ebelp = wa_ekkn-ebelp
                                                       binary search.
        if sy-subrc is initial.
          wa_saidav-matnr = wa_ekpo-matnr.
          wa_saidav-txz01 = wa_ekpo-txz01.
        endif.

      endif.

      read table t_lfa1 into data(wa_lfa1) with key lifnr = wa_rbkp-lifnr
                                                    binary search.
      if sy-subrc is initial.
        wa_saidav-lifnr = wa_rbkp-lifnr && ' - ' && wa_lfa1-name1.
        "wa_saidav-lifnr = |{ wa_rbkp-lifnr }-{ wa_lfa1-name1 }|."wa_rbkp-lifnr && ' - ' && wa_lfa1-name1.
        "CONCATENATE wa_rbkp-lifnr '-' wa_lfa1-name1 INTO wa_saidav-lifnr SEPARATED BY space.

      endif.

      append wa_saidav to t_saidav.
      clear: wa_saidav, wa_bseg, wa_bkpf.

    endloop.

  endif.

******>>>>>Inicio melhoria USER STORY 158506 / AOENNING <<<<<<<<<<
  if s_nftype is not initial.
    delete t_saidav where nftype not in s_nftype.
  endif.

  if p_conta is not initial.
    sort t_saidav by cod_cta.
    delete t_saidav where cod_cta not in  p_conta.
  endif.
******>>>>>Fim melhoria USER STORY 158506 / AOENNING <<<<<<<<<<


*Ao clicar no Documento contábil abrir a FB03, passar os parâmetros abaixo.
*BKPF-BUKRS
*BKPF-BELNR
*BKPF-GJAHR

endform.
*&---------------------------------------------------------------------*
*& Form f_montar_layoutv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_montar_layoutv .


*Chave do lançamento  Empresa Filial  Data lançamento Data documento  Documento contábil
*Fornecedor Conta Despesa Pedido  Texto das contas  Valor Fatura  Nº Pedido Nº MIRO
*Centro Custo	Descrição

  perform f_montar_estrutura using:

  1 ''   ''      'T_SAIDAV' 'BUKRS'        'Empresa'                    ' ' ' ',
  2 ''   ''      'T_SAIDAV' 'GSBER'        'Filial'                     ' ' ' ',
  3 ''   ''      'T_SAIDAV' 'BELNR'        'Documento contábil'         ' ' ' ',
  4 ''   ''      'T_SAIDAV' 'BUDAT'        'Data lançamento'            ' ' ' ',
  5 ''   ''      'T_SAIDAV' 'BLDAT'        'Data documento'             ' ' ' ',
  6 ''   ''      'T_SAIDAV' 'RMWWR'        'Valor Fatura'               ' ' ' ',
  7 ''   ''      'T_SAIDAV' 'FWSTE'        'PIS'                        ' ' ' ',
  8 ''   ''      'T_SAIDAV' 'FWSTEC'       'COFINS'                     ' ' ' ',
  9 ''   ''      'T_SAIDAV' 'KOSTL'        'Centro Custo'               ' ' ' ',
 10 ''   ''      'T_SAIDAV' 'LTEXT'        'Descrição Centro Custo'     ' ' ' ',
 11 ''   ''      'T_SAIDAV' 'LIFNR'        'Participante - Fornecedor'  ' ' ' ',
 12 ''   ''      'T_SAIDAV' 'BELNRM'       'Nº MIRO'                    ' ' ' ',
 13 ''   ''      'T_SAIDAV' 'EBELN'        'Nº Pedido'                  ' ' ' ',
 14 ''   ''      'T_SAIDAV' 'MATNR'        'Cod. Material'              ' ' ' ',
 15 ''   ''      'T_SAIDAV' 'TXZ01'        'Desc. Material'             ' ' ' ',
 15 ''   ''      'T_SAIDAV' 'NFTYPE'       'Categoria da nota'          ' ' ' ',
 16 ''   ''      'T_SAIDAV' 'COD_CTA'      'Conta contabíl   '          ' ' ' '.

  clear: w_variant, w_layout.
  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra      = 'X'.
  w_variant-report    = sy-repid.
*  w_variant-variant   = p_layout.

endform.
