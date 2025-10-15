************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exp. & Importação Ltda                       *
* Data desenv ...: 25.02.2009                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: APR – Relatório de Aprovação de Recebimento         *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 25.02.2009    Marcus Barbara       Criação              DEVK905550   *
* 09.03.2009    Marcus Barbara       Alteração            DEVK905611   *
* 11.03.2009    Marcus Barbara       Alteração            DEVK905639   *
*                                                                      *
************************************************************************

report zmmr010.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
type-pools: slis,
            kkblo.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
data: it_fieldcat type slis_t_fieldcat_alv,                   "Estrutura de saida
      it_event    type slis_t_event       with header line,   "Eventos
      it_header   type kkblo_t_listheader with header line,   "Cabeçalho
      vg_layout   type slis_layout_alv.   "Layout do alv

*----------------------------------------------------------------------*
* Variávei globais
*----------------------------------------------------------------------*
data: vg_fm_name  type rs38l_fnam. "Nome da função smart form

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_rbkp,
        bukrs like rbkp-bukrs,
        gsber like rbkp-gsber,
        budat like rbkp-budat,
        belnr like rbkp-belnr,
        lifnr like rbkp-lifnr,
        usnam like rbkp-usnam,
        cpudt like rbkp-cpudt,
        cputm like rbkp-cputm,
        gjahr like rbkp-gjahr,
        xblnr like rbkp-xblnr,
      end of wa_rbkp,

      begin of wa_ekko,
        bukrs like ekko-bukrs,
        lifnr like ekko-lifnr,
        ebeln like ekko-ebeln,
        ekgrp like ekko-ekgrp,
        aedat like ekko-aedat,
      end of wa_ekko,

      begin of wa_ekpo,
        ebeln like ekpo-ebeln,
        ebelp like ekpo-ebelp,
        werks like ekpo-werks,
        matnr like ekpo-matnr,
        lgort like ekpo-lgort,
        txz01 like ekpo-txz01,
        meins like ekpo-meins,
        menge like ekpo-menge,
        knttp like ekpo-knttp,

      end of wa_ekpo,

      begin of wa_mard,
        matnr like mard-matnr,
        werks like mard-werks,
        lgort like mard-lgort,
        lgpbe like mard-lgpbe,
      end of wa_mard,

      begin of wa_t024,
        ekgrp like t024-ekgrp,
        eknam like t024-eknam,
        ektel like t024-ektel,
      end of wa_t024,

      begin of wa_lfa1,
        lifnr like lfa1-lifnr,
        name1 like lfa1-name1,
      end of wa_lfa1,

      begin of wa_t001,
        bukrs like t001-bukrs,
        butxt like t001-butxt,
      end of wa_t001,

      begin of wa_t001w,
        werks like t001w-werks,
        name1 like t001w-name1,
      end of wa_t001w,

      begin of wa_ekes,
        ebeln like ekes-ebeln,
        ebelp like ekes-ebelp,
        vbeln like ekes-vbeln,
      end of wa_ekes,

      begin of wa_rela,
        belnr like rbkp-belnr,
        budat like rbkp-budat,
        cpudt like rbkp-cpudt,
        cputm like rbkp-cputm,
        usnam like rbkp-usnam,
        xblnr like rbkp-xblnr,
        ebeln like rseg-ebeln,
        buzei like rseg-buzei,
        lifnr like rbkp-lifnr,
        forne like lfa1-name1,
        bukrs like ekko-bukrs,
        aedat like ekko-aedat,
        ekgrp like ekko-ekgrp,
        knttp like ekpo-knttp,

      end of wa_rela,

      begin of wa_rela_form,
        belnr like rbkp-belnr,
        budat like rbkp-budat,
        cpudt like rbkp-cpudt,
        cputm like rbkp-cputm,
        usnam like rbkp-usnam,
        xblnr like rbkp-xblnr,
        ebeln like rseg-ebeln,
        buzei like rseg-buzei,
        lifnr like rbkp-lifnr,
        forne like lfa1-name1,
        bukrs like ekko-bukrs,
        aedat like ekko-aedat,
        ekgrp like ekko-ekgrp,
        knttp like ekpo-knttp,
      end of wa_rela_form,

      begin of wa_rela_det,
        ebelp like ekpo-ebelp,
        txz01 like ekpo-txz01,
        lgpbe like mard-lgpbe,
        vbeln like ekes-vbeln,
        meins like ekpo-meins,
        anln1 like ekkn-anln1,
        matnr like ekpo-matnr,
        knttp like ekpo-knttp,
        kostl like ekkn-kostl,
        menge like ekpo-menge,
        belnr like rbselbest-belnr,
        ebeln like rbselbest-ebeln,
      end of wa_rela_det,


      begin of wa_rbselbest,
        belnr like rbselbest-belnr,
        gjahr like rbselbest-gjahr,
        ebeln like rbselbest-ebeln,
      end of wa_rbselbest,

      begin of wa_ekkn,
        ebeln like ekkn-ebeln,
        ebelp like ekkn-ebelp,
        anln1 like ekkn-anln1,
        kostl like ekkn-kostl,
      end of wa_ekkn,

     begin of wa_rseg,
       belnr type rseg-belnr,
       ebelp type rseg-ebelp,
       ebeln type rseg-ebeln,
    end of wa_rseg,

     begin of wa_rseg_rel,
       belnr type rseg-belnr,
       ebelp type rseg-ebelp,
       ebeln type rseg-ebeln,
    end of wa_rseg_rel,

      begin of wa_texto,
       ebelp like ekpo-ebelp,
       line  like tline-tdline,
     end of wa_texto.

data: it_rbkp like standard table of wa_rbkp,
      it_ekko like standard table of wa_ekko,
      it_ekpo like standard table of wa_ekpo,
      it_ekkn like standard table of wa_ekkn,
      it_mard like standard table of wa_mard,
      it_mard_aux like standard table of wa_mard,
      it_lfa1 like standard table of wa_lfa1,
      it_ekes like standard table of wa_ekes,
      it_rela like standard table of wa_rela,
      it_rela_det     like standard table of wa_rela_det,
      it_rbselbest like standard table of wa_rbselbest,
      it_texto    like standard table of wa_texto,
      it_rseg     like standard table of wa_rseg,
      it_rseg_rel like standard table of wa_rseg_rel.

data: begin of stxl_id,
        tdobject like stxl-tdobject,
        tdname   like stxl-tdname,
        tdid     like stxl-tdid,
        tdspras  like stxl-tdspras,
      end of stxl_id,

      begin of wa_ekpo_text,
        ebeln    like ekpo-ebeln,
        ebelp    like ekpo-ebelp,
        tdline   like tline-tdline,
        belnr    like rbselbest-belnr,
      end of wa_ekpo_text,

      begin of wa_ekpo_text_aux,
        ebeln    like ekpo-ebeln,
        ebelp    like ekpo-ebelp,
        tdline   like tline-tdline,
        belnr    like rbselbest-belnr,
      end of wa_ekpo_text_aux,

      begin of wa_lines,
        tdformat like tline-tdformat,
        tdline   like tline-tdline,
      end of wa_lines.

data: it_lines like standard table of wa_lines,
      it_ekpo_text     like standard table of wa_ekpo_text,
      it_ekpo_text_aux like standard table of wa_ekpo_text_aux.


*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s01.
parameters:
           p_bukrs like t001-bukrs obligatory,
           p_gsber like t001w-werks obligatory.
select-options:
           p_budat for sy-datum obligatory.
selection-screen end   of block b0.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
at selection-screen.

  if p_budat-low is initial.
    message '"Data Inicial" campo obrigatório!' type 'I'.
    leave to screen 1000.
    exit.
  endif.

  if p_budat-high is initial.
    message '"Data Fim" campo obrigatório!' type 'I'.
    leave to screen 1000.
    exit.
  endif.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.

  clear: it_rbkp,
         it_ekko,
         it_ekpo,
         it_mard,
         it_lfa1,
         it_rela,
         it_rbselbest.

* Cabeçalho doc.da fatura recebida
  perform f_sel_cab_fatura_recebida.
* Cabeçalho do documento de compra
  perform f_sel_cab_documento_compra.

* Mestre de fornecedores (parte geral)
  perform f_sel_mestre_fornecedor.

* Montar relatorio.
  perform f_monta_dados_relatorio.

* ALV ***********************************************
* Monta cabeçalho ALV
  perform f_alv_monta_cabecalho.
* Monta estrutura de apresentação.
  perform f_alv_monta_estrutura.
* Executar ALV
  perform f_alv_executa.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  ALV_MENSAGEM
*&---------------------------------------------------------------------*
*       Posicionar usuario de processos internos ao relatorio
*----------------------------------------------------------------------*

form alv_mensagem  using  value(p_0481).
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = p_0481.
endform.                    " ALV_MENSAGEM

*&---------------------------------------------------------------------*
*&      Form  F_SEL_CAB_FATURA_RECEBIDA
*&---------------------------------------------------------------------*
*       Seleciona cabeçalhos dos documentos das faturas recebidas
*----------------------------------------------------------------------*
form f_sel_cab_fatura_recebida .

  perform alv_mensagem using 'Pesquisando cabeçalhos dos documentos das faturas recebidas'.

  select bukrs gsber budat belnr lifnr usnam cpudt cputm gjahr xblnr
    from rbkp
    into corresponding fields of table it_rbkp
   where bukrs eq p_bukrs
     and gsber eq p_gsber
     and budat in p_budat
     and rbstat in ('A','B','C','D').

  " Item de documento fatura recebida
  select belnr ebelp ebeln
    from rseg
  into table it_rseg
    for all entries in it_rbkp
  where belnr eq it_rbkp-belnr.


endform.                    " F_SEL_CAB_FATURA_RECEBIDA

*&---------------------------------------------------------------------*
*&      Form  F_SEL_CAB_DOCUMENTO_COMPRA
*&---------------------------------------------------------------------*
*       Seleciona cabeçalho dos documento de compra
*----------------------------------------------------------------------*
form f_sel_cab_documento_compra .

  perform alv_mensagem using 'Pesquisando cabeçalhos dos documentos de compra'.

  select belnr gjahr ebeln
    from rbselbest
    into corresponding fields of table it_rbselbest
    for all entries in it_rbkp
   where belnr eq it_rbkp-belnr
     and gjahr eq it_rbkp-gjahr.

  select bukrs lifnr ebeln ekgrp aedat
    from ekko
    into corresponding fields of table it_ekko
    for all entries in it_rbselbest
   where ebeln eq it_rbselbest-ebeln.

endform.                    " F_SEL_CAB_DOCUMENTO_COMPRA

*&---------------------------------------------------------------------*
*&      Form  F_SEL_ITENS_DOCUMENTO_COMPRA
*&---------------------------------------------------------------------*
*       Selecionar itens dos documentos de compra
*----------------------------------------------------------------------*
form f_sel_itens_documento_compra using p_ebeln.

  perform alv_mensagem using 'Pesquisando itens dos documentos de compra'.

  clear: it_ekpo, it_ekko.

  select ebeln ebelp werks matnr lgort txz01 meins menge knttp
    from ekpo
    into corresponding fields of table it_ekpo
   where ebeln eq p_ebeln and
         elikz ne 'X'.

  select ebeln ebelp anln1 kostl
    from ekkn
    into corresponding fields of table it_ekkn
   where ebeln eq p_ebeln.

  select belnr ebelp ebeln
    from rseg
    into table it_rseg_rel
    for all entries in it_ekpo
  where ebelp eq it_ekpo-ebelp
    and ebeln eq it_ekpo-ebeln.


endform.                    " F_SEL_ITENS_DOCUMENTO_COMPRA

*&---------------------------------------------------------------------*
*&      Form  F_SEL_DEPOSITO_MATERIAL
*&---------------------------------------------------------------------*
*       Selecionar Dados de depósito para material
*----------------------------------------------------------------------*
form f_sel_deposito_material .

  perform alv_mensagem using 'Pesquisando dados de depósito para material'.

  select matnr werks lgort lgpbe
    from mard
    into corresponding fields of table it_mard
     for all entries in it_ekpo
   where matnr eq it_ekpo-matnr
     and werks eq it_ekpo-werks
     and lgort eq it_ekpo-lgort.

endform.                    " F_SEL_DEPOSITO_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  F_SEL_GRUPO_COMPRA
*&---------------------------------------------------------------------*
*       Selecionar grupos de compra
*----------------------------------------------------------------------*
form f_sel_grupo_compra using p_ekgrp.

  perform alv_mensagem using 'Pesquisando grupos de compra'.

  select single ekgrp eknam ektel
    from t024
    into wa_t024
   where ekgrp eq p_ekgrp.

endform.                    " F_SEL_GRUPO_COMPRA

*&---------------------------------------------------------------------*
*&      Form  F_SEL_MESTRE_FORNECEDOR
*&---------------------------------------------------------------------*
*       Selecionar dados mestres de fornecedor
*----------------------------------------------------------------------*
form f_sel_mestre_fornecedor .

  perform alv_mensagem using 'Pesquisando dados mestres de fornecedor'.

  select lifnr name1
    from lfa1
    into corresponding fields of table it_lfa1
     for all entries in it_rbkp
   where lifnr eq it_rbkp-lifnr.

endform.                    " F_SEL_MESTRE_FORNECEDOR

*&---------------------------------------------------------------------*
*&      Form  F_SEL_EMPRESAS
*&---------------------------------------------------------------------*
*       Selecionar dados mestre de empresas
*----------------------------------------------------------------------*
form f_sel_empresas using p_bukrs.

  perform alv_mensagem using 'Pesquisando dados mestre de empresas'.

  select single bukrs butxt
    from t001
    into wa_t001
   where bukrs eq p_bukrs.

endform.                    " F_SEL_EMPRESAS

*&---------------------------------------------------------------------*
*&      Form  F_SEL_CENTROS_FILIAIS
*&---------------------------------------------------------------------*
*       Seleciona centros/filiais
*----------------------------------------------------------------------*
form f_sel_centros_filiais using p_werks.

  perform alv_mensagem using 'Pesquisando dados mestre centros/filiais'.

  select single werks name1
    from t001w
    into wa_t001w
   where werks eq p_werks.

endform.                    " F_SEL_CENTROS_FILIAIS

*&---------------------------------------------------------------------*
*&      Form  F_SEL_CONFIRMA_PEDIDO
*&---------------------------------------------------------------------*
*       Seleciona Confirmações pedido
*----------------------------------------------------------------------*
form f_sel_confirma_pedido.

  select ebeln ebelp vbeln
    into table it_ekes
    from ekes
     for all entries in it_ekpo
   where ebeln eq it_ekpo-ebeln
     and ebelp eq it_ekpo-ebelp.

endform.                    " F_SEL_CONFIRMA_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_RELATORIO
*&---------------------------------------------------------------------*
*       Montagem de dados do relatório.
*----------------------------------------------------------------------*

form f_monta_dados_relatorio .

  perform alv_mensagem using 'Montando relatorio de seleção de aprovações de recebimento'.

  sort: it_lfa1  by lifnr,
        it_rbselbest by belnr gjahr,
        it_ekko by ebeln,
        it_rseg by belnr.

  loop at it_rbkp into wa_rbkp.

    read table it_rseg into wa_rseg with key belnr = wa_rbkp-belnr binary search.

    if ( sy-subrc eq 0 ).
      clear wa_rela.
      wa_rela-belnr = wa_rbkp-belnr.
      wa_rela-budat = wa_rbkp-budat.
      wa_rela-cputm = wa_rbkp-cputm.
      wa_rela-cpudt = wa_rbkp-cpudt.
      wa_rela-usnam = wa_rbkp-usnam.
      wa_rela-lifnr = wa_rbkp-lifnr.
      wa_rela-xblnr = wa_rbkp-xblnr.

      read table it_rbselbest into wa_rbselbest with key belnr = wa_rbkp-belnr
                                                         gjahr = wa_rbkp-gjahr
                                                         binary search.
      if sy-subrc eq 0.
        read table it_ekko into wa_ekko with key ebeln = wa_rbselbest-ebeln binary search.
        if sy-subrc eq 0.
          wa_rela-ebeln = wa_ekko-ebeln.
          wa_rela-bukrs = wa_ekko-bukrs.
          wa_rela-ekgrp = wa_ekko-ekgrp.
          wa_rela-aedat = wa_ekko-aedat.
        endif.
      endif.

      read table it_lfa1 into wa_lfa1 with key lifnr = wa_rbkp-lifnr binary search.
      if sy-subrc eq 0.
        wa_rela-forne = wa_lfa1-name1.
      endif.

      append wa_rela to it_rela.

    endif.
  endloop.

endform.                    " F_MONTA_DADOS_RELATORIO


*&---------------------------------------------------------------------*
*&      Form  F_ALV_MONTA_CABECALHO
*&---------------------------------------------------------------------*
*       ALV - Monta cabeçalho ALV
*----------------------------------------------------------------------*

form f_alv_monta_cabecalho .

  perform alv_mensagem using 'Montando cabeçalho do relatorio de seleção de aprovações de recebimento'.

  data: vl_data1(10)     type c,
        vl_data2(10)     type c,
        vl_data(25)      type c,
        vl_butxt         like t001-butxt,   "Nome da empresa.
        vl_name1         like t001w-name1.

  clear it_header.

  it_header-typ  = 'H'.
  it_header-info = 'Relatório de Aprovação de Recebimento'.
  append  it_header.

  concatenate p_budat-low+6(2) '.'
              p_budat-low+4(2) '.'
              p_budat-low(4)
              into vl_data1.

  concatenate p_budat-high+6(2) '.'
              p_budat-high+4(2) '.'
              p_budat-high(4)
              into vl_data2.

  concatenate vl_data1
              'a'
              vl_data2 into vl_data
              separated by space.

  select single butxt from t001 into vl_butxt where bukrs = p_bukrs.
  concatenate p_bukrs vl_butxt into vl_butxt separated by space.
  it_header-typ  = 'S'.
  it_header-key  = 'Empresa'.
  it_header-info = vl_butxt.
  append  it_header.

  select single name1 from t001w into vl_name1 where werks = p_gsber.
  concatenate p_bukrs vl_butxt into vl_butxt separated by space.
  it_header-typ  = 'S'.
  it_header-key  = 'Filial'.
  it_header-info = vl_name1.
  append  it_header.

  it_header-typ  = 'S'.
  it_header-key  = 'Período de lançamento'.
  it_header-info = vl_data.
  append  it_header.

  it_header-typ  = 'S'.
  it_header-key  = 'Usuário'.
  it_header-info = sy-uname.
  append  it_header.

  vl_data2 = sy-datum.

  concatenate vl_data2+6(2) '.'
              vl_data2+4(2) '.'
              vl_data2(4)
              into vl_data1.

  it_header-typ  = 'S'.
  it_header-key  = 'Data'.
  it_header-info = vl_data1.
  append  it_header.

endform.                    " F_ALV_MONTA_CABECALHO

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
* p_just   -> Alinhamento (R)ight (L)eft (C)ent                        *
* p_hot    -> Link para chamada de evento                              *
*----------------------------------------------------------------------*
form f_fieldcat using p_cont p_key  p_tab  p_field p_desc
      p_tam  p_qtde p_fix  p_just p_hot
changing p_fieldcat type slis_t_fieldcat_alv.

* Tabela interna local
  data: tl_fieldcat type slis_t_fieldcat_alv with header line.

  tl_fieldcat-col_pos    = p_cont.
  tl_fieldcat-key        = p_key.
  tl_fieldcat-tabname    = p_tab.
  tl_fieldcat-fieldname  = p_field.
  tl_fieldcat-seltext_l  = p_desc.
  tl_fieldcat-seltext_m  = p_desc.
  tl_fieldcat-seltext_s  = p_desc.
  tl_fieldcat-outputlen  = p_tam.
  tl_fieldcat-quantity   = p_qtde.
  tl_fieldcat-fix_column = p_fix.
  tl_fieldcat-just       = p_just.
  tl_fieldcat-hotspot    = p_hot.
  append tl_fieldcat to p_fieldcat.

endform.                    " f_fieldcatJ1BNFDOC

*&---------------------------------------------------------------------*
*&      Form  F_ALV_MONTA_ESTRUTURA
*&---------------------------------------------------------------------*
*       ALV - Monta estrututa de apresenção de ALV
*----------------------------------------------------------------------*
form f_alv_monta_estrutura .

  perform alv_mensagem using 'Montando estrutura de apresentação do relatorio de seleção de aprovações de recebimento'.

  perform f_fieldcat using:
       '0' '' 'IT_RELA' 'BELNR' 'Doc.Fatura'
       10  ''  ''         '' 'X'
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'BUDAT' 'Dt.Lcto'
       10  ''  ''             '' ''
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'CPUTM' 'Hora'
       08  ''  ''             '' ''
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'USNAM' 'Usuário'
       12  ''  ''             '' ''
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'EBELN' 'Pedido'
       12  ''  ''             '' ''
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'LIFNR' 'Cód.'
       10  ''  ''             '' ''
 changing it_fieldcat,
       '1' '' 'IT_RELA' 'FORNE' 'Fornecedor'
       35  ''  ''             '' ''
 changing it_fieldcat.


endform.                    " F_ALV_MONTA_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*      Chama o cabeçalho da ALV                                        *
*----------------------------------------------------------------------*
form top_of_page.
* Cabeçalho
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = it_header[].

endform.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA
*&---------------------------------------------------------------------*
*       ALV - Chamada de relatorio ALV
*----------------------------------------------------------------------*
form f_alv_executa .

  data: vl_repid like sy-repid.

  vl_repid = sy-repid.

  it_event-name = slis_ev_top_of_page.
  it_event-form = slis_ev_top_of_page.
  append it_event.

  vg_layout-zebra = 'X'.

* Função para exibir o ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = vl_repid
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = vg_layout
      it_fieldcat             = it_fieldcat[]
      i_default               = 'A'
      i_save                  = 'X'
      it_events               = it_event[]
    tables
      t_outtab                = it_rela
    exceptions
      program_error           = 1
      others                  = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " F_ALV_EXECUTA

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
form user_command  using ucomm like sy-ucomm
      selfield type slis_selfield.

  data: tdline_text   like tline-tdline.


  data: var_belnr type rbkp-belnr.

  clear: var_belnr, wa_rela_form.

  read table it_rela into wa_rela      index selfield-tabindex.
  move wa_rela to wa_rela_form.

  var_belnr = wa_rela-belnr.


  if selfield-fieldname = 'BELNR'.
* Montar dados de recebimento
    clear: it_rela_det.
* Item do documento de compras
    perform f_sel_itens_documento_compra using wa_rela-ebeln.
* Dados de depósito para material
    perform f_sel_deposito_material.
* Confirmações pedido
    perform f_sel_confirma_pedido.
* Monta dados de recebimento
    perform f_monta_dados_recebimento.
* Grupos de compra
    perform f_sel_grupo_compra using wa_rela-ekgrp.
* Empresas
    perform f_sel_empresas using wa_rela-bukrs.
* Centros/filiais
    read table it_ekpo into wa_ekpo with key ebeln = wa_rela-ebeln.
    perform f_sel_centros_filiais using wa_ekpo-werks.


    delete it_rela_det where belnr ne var_belnr.

*    loop at it_rela_det into wa_rela_det where belnr eq var_belnr.
*      loop at it_ekpo_text into wa_ekpo_text where ebelp = wa_rela_det-ebelp
*                                               and ebeln = wa_rela_det-ebeln
*                                               and belnr = wa_rela_det-belnr.
*
*        tdline_text = wa_ekpo_text-tdline.
*        "wa_ekpo_text_aux-tdline = wa_ekpo_text-tdline.
*        wa_ekpo_text_aux-ebeln  = wa_ekpo_text-ebeln.
*        wa_ekpo_text_aux-ebelp  = wa_ekpo_text-ebelp.
*        wa_ekpo_text_aux-belnr  = wa_ekpo_text-belnr.
*
*        concatenate tdline_text wa_ekpo_text-tdline into  tdline_text separated by space.
*
*      endloop.
*    endloop.


* Chamada de relatório SMARTFORMS
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname = 'ZMMR010_SMART_FORM_4'
      importing
        fm_name  = vg_fm_name
      exceptions
        others   = 3.

    check sy-subrc is initial.


    call function vg_fm_name
      exporting
        bukrs        = wa_rela_form-bukrs
        empre        = wa_t001-butxt
        werks        = wa_ekpo-werks
        centr        = wa_t001w-name1
        cpudt        = wa_rela_form-cpudt
        cputm        = wa_rela_form-cputm
        belnr        = wa_rela_form-belnr
        xblnr        = wa_rela_form-xblnr
        usnam        = wa_rela_form-usnam
        ebeln        = wa_rela_form-ebeln
        ekgrp        = wa_rela_form-ekgrp
        eknam        = wa_t024-eknam
        ektel        = wa_t024-ektel
        lifnr        = wa_rela_form-lifnr
        forne        = wa_rela_form-forne
        aedat        = wa_rela_form-aedat
        knttp        = wa_ekpo-knttp
        vbeln        = wa_ekes-vbeln
        anln1        = wa_ekkn-anln1
        kostl        = wa_ekkn-kostl
        lgpbe        = wa_mard-lgpbe
        menge        = wa_ekpo-menge
        ebelp        = wa_ekpo-ebelp
      tables
        itens        = it_rela_det
        it_ekpo_text = it_ekpo_text[].





*    call function vg_fm_name
*      exporting
*        bukrs        = wa_rela-bukrs
*        empre        = wa_t001-butxt
*        werks        = wa_ekpo-werks
*        centr        = wa_t001w-name1
*        cpudt        = wa_rela-cpudt
*        cputm        = wa_rela-cputm
*        belnr        = wa_rela-belnr
*        xblnr        = wa_rela-xblnr
*        usnam        = wa_rela-usnam
*        ebeln        = wa_rela-ebeln
*        ekgrp        = wa_rela-ekgrp
*        eknam        = wa_t024-eknam
*        ektel        = wa_t024-ektel
*        lifnr        = wa_rela-lifnr
*        forne        = wa_rela-forne
*        aedat        = wa_rela-aedat
*        knttp        = wa_ekpo-knttp
*        vbeln        = wa_ekes-vbeln
*        anln1        = wa_ekkn-anln1
*        kostl        = wa_ekkn-kostl
*        lgpbe        = wa_mard-lgpbe
*        menge        = wa_ekpo-menge
*        ebelp        = wa_ekpo-ebelp
*      tables
*        itens        = it_rela_det
*        it_ekpo_text = it_ekpo_text[].

  endif.

endform.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_RECEBIMENTO
*&---------------------------------------------------------------------*
*       Monta dados recebimento
*----------------------------------------------------------------------*
form f_monta_dados_recebimento .

  sort: it_ekpo  by ebeln ebelp,
        it_mard  by matnr werks lgort,
        it_ekes  by ebeln ebelp,
        it_ekkn  by ebeln ebelp,
        it_rseg_rel by ebeln ebelp.

  clear it_ekpo_text.


  sort: it_ekpo by ebeln ebelp.

  loop at it_rela into wa_rela.
    loop at it_rseg_rel into wa_rseg_rel where ebeln eq wa_rela-ebeln
                                           and belnr eq wa_rela-belnr.


      wa_rela_det-belnr = wa_rela-belnr.
      wa_rela_det-ebeln = wa_rela-ebeln.

      read table it_ekpo into wa_ekpo with key ebeln = wa_rseg_rel-ebeln
                                               ebelp = wa_rseg_rel-ebelp.
      if ( sy-subrc eq 0 ).

        wa_rela_det-ebelp = wa_ekpo-ebelp.
        wa_rela_det-txz01 = wa_ekpo-txz01.
        wa_rela_det-meins = wa_ekpo-meins.
        wa_rela_det-matnr = wa_ekpo-matnr.
        wa_rela_det-knttp = wa_ekpo-knttp.
        wa_rela_det-menge = wa_ekpo-menge.

        read table it_mard into wa_mard with key matnr = wa_ekpo-matnr
                                                 werks = wa_ekpo-werks
                                                 lgort = wa_ekpo-lgort binary search.
        if sy-subrc eq 0.
          wa_rela_det-lgpbe = wa_mard-lgpbe.
        endif.

        read table it_ekes into wa_ekes with key ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp binary search.
        if sy-subrc eq 0.
          wa_rela_det-vbeln = wa_ekes-vbeln.
        endif.

        read table it_ekkn into wa_ekkn with key ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp binary search.
        if sy-subrc eq 0.
          wa_rela_det-anln1 = wa_ekkn-anln1.
          wa_rela_det-kostl = wa_ekkn-kostl.
        endif.

        perform: texto_material_desc using wa_ekpo-matnr wa_ekpo-ebelp wa_ekpo-ebeln wa_rela-belnr.


        append wa_rela_det to it_rela_det.
      endif.
      clear:  wa_rseg_rel, wa_ekpo, wa_mard, wa_ekes, wa_ekkn, wa_rela_det.
    endloop.
  endloop.


endform.                    " F_MONTA_DADOS_RECEBIMENTO
*&---------------------------------------------------------------------*
*&      Form  TEXTO_MATERIAL
*&---------------------------------------------------------------------*
form texto_material_desc using  p_matnr_desc
                                p_ebelp_desc
                                p_ebeln_desc
                                p_belnr_desc.

  types:
      begin of stxl_id,
       tdobject like stxl-tdobject,
       tdname   like stxl-tdname,
       tdid     like stxl-tdid,
       tdspras  like stxl-tdspras,
    end of stxl_id.

  data begin of textheader.
          include structure thead.
  data end of textheader.

  data begin of textlines occurs 0.
          include structure tline.
  data end of textlines.

  data: rt_lines  type table of tline with header line.

  data: wl_stxl_id type stxl_id,
        v_name type stxh-tdname.

  concatenate p_ebeln_desc p_ebelp_desc into v_name.

  if not ( p_matnr_desc  is initial ) and ( not p_ebelp_desc is initial ).

    clear: wl_stxl_id.

    call function 'READ_TEXT'
      exporting
        id       = 'F03'
        language = 'P'
        name     = v_name
        object   = 'EKPO'
      importing
        header   = textheader
      tables
        lines    = textlines
      exceptions
        others   = 1.


    loop  at textlines.

      wa_ekpo_text-ebeln = p_ebeln_desc.
      wa_ekpo_text-ebelp = p_ebelp_desc.
      wa_ekpo_text-belnr = p_belnr_desc.
      wa_ekpo_text-tdline = rt_lines-tdline.
      append wa_ekpo_text to it_ekpo_text.

      clear: wa_ekpo_text.

    endloop.

    if not ( it_ekpo_text[] is initial ).
      delete it_ekpo_text where tdline eq ''.
    endif.

    if ( textlines[] is initial ).

      stxl_id-tdobject = 'MATERIAL'.
      stxl_id-tdname   =  p_matnr_desc.
      stxl_id-tdid     = 'BEST'.
      stxl_id-tdspras  = 'PT'.

      import tline to rt_lines
       from database stxl(tx)
            client   sy-mandt
            id       stxl_id
            accepting truncation                     "important for Unicode->Nonunicode
            ignoring conversion errors.



      loop at rt_lines.

        wa_ekpo_text-ebeln = p_ebeln_desc.
        wa_ekpo_text-ebelp = p_ebelp_desc.
        wa_ekpo_text-belnr = p_belnr_desc.
        wa_ekpo_text-tdline = rt_lines-tdline.
        append wa_ekpo_text to it_ekpo_text.

        clear: wa_ekpo_text.
      endloop.

      if not ( it_ekpo_text[] is initial ).
        delete it_ekpo_text where tdline eq ''.
      endif.


    endif.

    clear: textlines[].

  endif.
endform.                    " TEXTO_MATERIAL
