*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDJ0001                                                *
* Descrição  : Job Estorno Processos SD                                *
* Módulo     : SD                                Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 10/06/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

report zsdj0001 no standard page heading message-id sd.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
types: begin of type_active,
         docnum type j_1bnfe_active-docnum,
         cancel type j_1bnfe_active-cancel,
         credat type j_1bnfe_active-credat,
       end   of type_active,

       begin of type_doc,
         docnum type j_1bnfdoc-docnum,
         manual type j_1bnfdoc-manual,
       end   of type_doc,

       begin of type_lin,
         docnum type j_1bnflin-docnum,
         itmnum type j_1bnflin-itmnum,
         reftyp type j_1bnflin-reftyp,
         refkey type j_1bnflin-refkey,
         vbeln  type vbfa-vbeln      ,
       end   of type_lin,

       begin of type_vbfa,
         vbelv   type vbfa-vbelv,
         posnv   type vbfa-posnv,
         vbeln   type vbfa-vbeln,
         posnn   type vbfa-posnn,
         vbtyp_n type vbfa-vbtyp_n,
         vbtyp_v type vbfa-vbtyp_v,
         erdat   type vbfa-erdat,
         mjahr   type mseg-mjahr,
       end   of type_vbfa,

       begin of type_vbak,
         vbeln type vbak-vbeln,
         auart type vbak-auart,
       end   of type_vbak,

       begin of type_likp,
         vbeln type likp-vbeln,
         vbtyp type likp-vbtyp,
       end   of type_likp,

       begin of type_mseg,
         mblnr type mseg-mblnr,
         mjahr type mseg-mjahr,
         zeile type mseg-zeile,
         smbln type mseg-smbln,
       end   of type_mseg,

       begin of type_0023,
         vbeln    type zsdt0023-vbeln,
         mblnr_s  type zsdt0023-mblnr_s,
         mblnr_e  type zsdt0023-mblnr_e,
         dt_saida type zsdt0023-dt_saida,
       end   of type_0023,

       begin of type_msg,
         tcode   type sytcode,
         docu    type zdocu,
         msg     type char100,
         doc_ret type zdocu,
       end   of type_msg.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
data: t_active     type table of type_active,
      t_doc        type table of type_doc   ,
      t_linm       type table of type_lin   ,
      t_lin        type table of type_lin   ,
      t_vbfamc     type table of type_vbfa  ,
      t_vbfarj     type table of type_vbfa  ,
      t_vbfamj     type table of type_vbfa  ,
      t_vbfahj     type table of type_vbfa  ,
      t_vbakv      type table of type_vbak  ,
      t_vbakt      type table of type_vbak  ,
      t_likp       type table of type_likp  ,
      t_likpfl     type table of type_likp  ,
      t_mseg       type table of type_mseg  ,
      t_zsdt0023   type table of type_0023  ,
      t_zsdt0023fl type table of type_0023  ,
      t_msg        type table of type_msg   .

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
constants: c_x     type char1 value 'X'    ,
*---> 05/06/2023 - Migração S4 - JS
*           c_m     type char1 value 'M'
*           c_c     type char1 value 'C'
*           c_r     type VBTYPL_N value 'R'    ,
*           c_j     type VBTYPL_N value 'J'    ,
*           c_h     type char1 value 'H'    ,
*           c_e     type char1 value 'E'    ,
           c_m     type VBTYPL_N value 'M'    ,
           c_c     type VBTYPL_N value 'C'    ,
           c_r     type VBTYPL_N value 'R'    ,
           c_j     type VBTYPL_N value 'J'    ,
           c_h     type VBTYPL_N value 'H'    ,
           c_e     type VBTYPL_N value 'E'    ,
*<--- 05/06/2023 - Migração S4 - JS
           c_vl11  type char4 value 'VL11' ,
           c_vl09  type char4 value 'VL08' ,
           c_vl02n type char5 value 'VL02N',
           c_zrfl  type char4 value 'ZRFL' ,
           c_mbst  type char4 value 'MBST' ,
           c_j1b3n type char5 value 'J1B3N'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
data: st_vbfa type type_vbfa,
      st_vbak type type_vbak,
      st_likp type type_likp.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
selection-screen begin of block a1 with frame title text-001.
parameters
  p_datum type sydatum default sy-datum.
selection-screen end   of block a1.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
start-of-selection.

* Seleciona Dados
  perform: z_seleciona_dados   ,

* Estorna Documentos
           z_estorna_documentos,

* Cancelamento NF Writer
           z_canc_nfwriter     ,

* Grava logs do processamento
           z_salva_log         .

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                           Seleciona Dados                            *
*----------------------------------------------------------------------*
form z_seleciona_dados.

  refresh t_msg.

* Seleciona J_1BNFE_ACTIVE
  perform z_seleciona_active.

  " CHECK NOT t_active[] IS INITIAL.
  if t_active[] is not initial.

* Seleciona J_1BNFDOC
    perform: z_seleciona_doc   ,

* Seleciona J_1BNFLIN
               z_seleciona_lin   ,

* Seleciona VBFA VBTYP_N = M VBTYP_V = C
               z_seleciona_vbfa tables t_vbfamc
                                 using c_m
                                       c_c,

* Seleciona VBAK - Vendas
               z_seleciona_vbak tables t_vbakv
                                       t_vbfamc,

* Seleciona VBFA VBTYP_R = M VBTYP_V = J
               z_seleciona_vbfa tables t_vbfamj
                                 using c_m
                                       c_j,

* Seleciona VBFA VBTYP_R = R VBTYP_V = J
               z_seleciona_vbfa tables t_vbfarj
                                 using c_r
                                       c_j,

* Seleciona VBFA VBTYP_R = H VBTYP_V = J
               z_seleciona_vbfa tables t_vbfahj
                                 using c_h
                                       c_j,

* Seleciona VBAK - Transferência
               z_seleciona_vbak tables t_vbakt
                                       t_vbfamj,

* Seleciona LIKP
               z_seleciona_likp tables t_vbfamj
                                       t_likp,

* Seleciona MSEG
               z_seleciona_mseg tables t_vbfahj,

* Seleciona ZSDT0023
               z_seleciona_zsdt0023,

* Seleciona ZSDT0023 Formação de Lote
               z_seleciona_zsdt0023fl,

* Seleciona LIKP Formação de Lote
               z_seleciona_likpfl.
  endif.
* Faz extorno dos registros que não tem likp
* Seleciona ZSDT0023 por data.
    perform: z_seleciona_zsdt0023_data.



endform.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ACTIVE                                       *
*&---------------------------------------------------------------------*
*                          Seleciona J_1BNFE_ACTIVE
*----------------------------------------------------------------------*
form z_seleciona_active.

  refresh t_active.

  select docnum cancel credat
    from j_1bnfe_active
    into table t_active
  where  cancel eq c_x
    and  credat eq p_datum.

  sort t_active by docnum ascending.
  check t_active[] is initial.

  write text-002.

endform.                    " Z_SELECIONA_ACTIVE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN                                          *
*&---------------------------------------------------------------------*
*                         Seleciona J_1BNFLIN                          *
*----------------------------------------------------------------------*
form z_seleciona_lin.

  refresh t_lin.
  check not t_active[] is initial.

  select docnum itmnum reftyp refkey
    from j_1bnflin
    into table t_lin
    for all entries in t_active
  where  docnum eq t_active-docnum.

  sort t_lin by docnum ascending
                itmnum ascending.

* Converte RFKEY para VBELN
  perform z_converte_refkey.

endform.                    " Z_SELECIONA_LIN

*&---------------------------------------------------------------------*
*&      Form  Z_CONVERTE_REFKEY                                        *
*&---------------------------------------------------------------------*
*                         Converte RFKEY para VBELN                    *
*----------------------------------------------------------------------*
form z_converte_refkey.

  data: sl_lin   type type_lin,
        vl_in    type char10  ,
        vl_index type i       .

  loop at t_lin into sl_lin.

    vl_index = sy-tabix.
    vl_in    = sl_lin-refkey.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vl_in
      importing
        output = sl_lin-vbeln.

    modify t_lin from sl_lin index vl_index
      transporting vbeln.

    clear sl_lin.

  endloop.

endform.                    " Z_CONVERTE_REFKEY

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                            Seleciona VBFA                            *
*----------------------------------------------------------------------*
form z_seleciona_vbfa tables p_vbfa    structure st_vbfa
                       using p_vbtyp_n type vbfa-vbtyp_n
                             p_vbtyp_v type vbfa-vbtyp_v.

  refresh p_vbfa.
  check not t_lin[] is initial.

  select vbelv posnv   vbeln
         posnn vbtyp_n vbtyp_v
         erdat
    from vbfa
    into table p_vbfa
    for all entries in t_lin
  where  vbeln   eq t_lin-vbeln
    and  vbtyp_n eq p_vbtyp_n
    and  vbtyp_v eq p_vbtyp_v.

* Converte ERDAT para MJAHR
  perform z_converte_mjahr tables p_vbfa.

endform.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAK                                         *
*&---------------------------------------------------------------------*
*                           Seleciona VBAK                             *
*----------------------------------------------------------------------*
form z_seleciona_vbak tables p_vbak structure st_vbak
                             p_vbfa structure st_vbfa.

  refresh p_vbak.
  check not p_vbfa[] is initial.

  select vbeln auart
    from vbak
    into table p_vbak
    for all entries in p_vbfa
  where  vbeln eq p_vbfa-vbelv.

  sort p_vbak by vbeln ascending.

endform.                    " Z_SELECIONA_VBAK

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_DOCUMENTOS                                     *
*&---------------------------------------------------------------------*
*                            Estorna Documentos                        *
*----------------------------------------------------------------------*
form z_estorna_documentos.

* Processo SD - Vendas
  perform: z_sd_vendas,

* Processo Formação de Lote Estorno Mov.Mercadoria
           z_form_lote.

endform.                    " Z_ESTORNA_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  Z_SD_VENDAS                                              *
*&---------------------------------------------------------------------*
*                         Processo SD - Vendas                         *
*----------------------------------------------------------------------*
form z_sd_vendas.

  data: sl_vbfa   type type_vbfa               ,
        tl_vbfa   type table of type_vbfa      .

  sort: t_vbfamj by vbeln ascending,
        t_vbfarj by vbelv ascending,
        t_vbfahj by vbelv ascending.

  check not t_vbfamc[] is initial.
  tl_vbfa[] = t_vbfamc[].
  sort tl_vbfa by vbeln ascending.
  delete adjacent duplicates from tl_vbfa comparing vbeln.

  loop at tl_vbfa into sl_vbfa.

*   Estorno Fatura
    perform z_estorno_fatura using sl_vbfa-vbeln
                                   sl_vbfa-vbelv.

    clear sl_vbfa.

  endloop.


endform.                    " Z_SD_VENDAS

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNO_FATURA                                         *
*&---------------------------------------------------------------------*
*                             Estorno Fatura                           *
*----------------------------------------------------------------------*
form z_estorno_fatura using p_vbeln type vbak-vbeln
                            p_vbelv type vbfa-vbelv.

  data: sl_return type bapireturn1             ,
        tl_return type table of bapireturn1    ,
        tl_sucess type table of bapivbrksuccess.

  call function 'ZBAPI_BILLINGDOC_CANCEL1'
    exporting
      billingdocument = p_vbeln
    tables
      return          = tl_return
      success         = tl_sucess.

  if sy-subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    wait up to 2 seconds.
*   Estorna Remessa
    perform z_estorna_remessa using p_vbeln
                                    p_vbelv.
  else.
    delete tl_return where type ne c_e.
    loop at tl_return into sl_return.
*     Salva Mensagens
      perform z_monta_msg using c_vl11
                                p_vbeln
                                sl_return-message
                                space.
      clear sl_return.
    endloop.
  endif.

endform.                    " Z_ESTORNO_FATURA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_MSG                                              *
*&---------------------------------------------------------------------*
*                              Salva Mensagens                         *
*----------------------------------------------------------------------*
form z_monta_msg using p_tcode   type c
                       p_docu    type c
                       p_message type c
                       p_doc_ret type c.

  data sl_msg type type_msg.

  sl_msg-tcode   = p_tcode.
  sl_msg-docu    = p_docu.
  sl_msg-msg     = p_message.
  sl_msg-doc_ret = p_doc_ret.

  append sl_msg to t_msg.

endform.                    " Z_MONTA_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_REMESSA                                        *
*&---------------------------------------------------------------------*
*                             Estorna Remessa                          *
*----------------------------------------------------------------------*
form z_estorna_remessa using p_vbeln type vbak-vbeln
                             p_vbelv type vbfa-vbelv.

  data: sl_vbfamj   type type_vbfa,
        sl_vbfarj   type type_vbfa,
        sl_vbfahj   type type_vbfa,
        sl_vbak     type type_vbak,
        sl_likp     type type_likp,
        sl_mseg     type type_mseg,
        sl_zsdt0023 type type_0023.

  read table t_vbfamj into sl_vbfamj
    with key vbeln = p_vbeln
    binary search.

  check sy-subrc is initial.

  read table t_likp into sl_likp
    with key vbeln = sl_vbfamj-vbelv
    binary search.

  check sy-subrc is initial.

  read table t_vbakv into sl_vbak
    with key vbeln = p_vbelv
    binary search.

*  CHECK sl_vbak-auart eq c_zrfl.
  check sl_vbak-auart ne c_zrfl.

  read table t_vbfarj into sl_vbfarj
    with key vbelv = sl_likp-vbeln
    binary search.

  read table t_vbfahj into sl_vbfahj
    with key vbelv = sl_likp-vbeln
    binary search.

  read table t_mseg into sl_mseg
    with key mblnr = sl_vbfahj-vbeln
             mjahr = sl_vbfahj-mjahr
    binary search.

*  IF sl_mseg-smbln NE sl_vbfahj-vbeln.
*   Executa VL09
  perform z_executa_vl09 using sl_likp.
*  ENDIF.

* Executa VL02N
  perform z_executa_vl02n using sl_likp.

  read table t_zsdt0023 into sl_zsdt0023
    with key vbeln = sl_likp-vbeln
    binary search.

  check sy-subrc is initial.
* Estorno MBST
  perform z_estorno_mbst using sl_zsdt0023-mblnr_s
                               sl_zsdt0023-dt_saida.
  perform z_estorno_mbst using sl_zsdt0023-mblnr_e
                               sl_zsdt0023-dt_saida.

endform.                    " Z_ESTORNA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKP                                         *
*&---------------------------------------------------------------------*
*                            Seleciona LIKP                            *
*----------------------------------------------------------------------*
form z_seleciona_likp tables p_vbfa structure st_vbfa
                             p_likp structure st_likp.

  data tl_vbfa type table of type_vbfa.

  refresh p_likp.
  check not p_vbfa[] is initial.
  tl_vbfa[] = p_vbfa[].
  sort tl_vbfa by vbelv ascending.
  delete adjacent duplicates from tl_vbfa comparing vbelv.

  select vbeln vbtyp
    from likp
    into table p_likp
    for all entries in tl_vbfa
  where  vbeln eq tl_vbfa-vbelv.

  sort p_likp by vbeln ascending.

endform.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MSEG                                         *
*&---------------------------------------------------------------------*
*                             Seleciona MSEG                           *
*----------------------------------------------------------------------*
form z_seleciona_mseg tables p_vbfa structure st_vbfa.

  data tl_vbfa type table of type_vbfa.

  refresh t_mseg.
  check not p_vbfa[] is initial.
  tl_vbfa[] = p_vbfa[].
  sort tl_vbfa by vbelv ascending.
  delete adjacent duplicates from tl_vbfa comparing vbelv.

  select mblnr mjahr
         zeile smbln
    from mseg
    into table t_mseg
    for all entries in tl_vbfa
  where  mblnr eq tl_vbfa-vbelv
    and  mjahr eq tl_vbfa-mjahr.

  sort t_mseg by mblnr ascending
                 mjahr ascending.

endform.                    " Z_SELECIONA_MSEG

*&---------------------------------------------------------------------*
*&      Form  Z_CONVERTE_MJAHR                                         *
*&---------------------------------------------------------------------*
*                      Converte ERDAT para MJAHR                       *
*----------------------------------------------------------------------*
form z_converte_mjahr tables p_vbfa structure st_vbfa.

  data: sl_vbfa  type type_vbfa,
        vl_index type i        .

  loop at p_vbfa into sl_vbfa.

    vl_index       = sy-tabix.
    sl_vbfa-mjahr  = sl_vbfa-erdat(04).
    modify p_vbfa from sl_vbfa index vl_index
      transporting mjahr.

    clear sl_vbfa.

  endloop.

endform.                    " Z_CONVERTE_MJAHR

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_VL09                                           *
*&---------------------------------------------------------------------*
*                               Executa VL09                           *
*----------------------------------------------------------------------*
form z_executa_vl09 using p_likp type type_likp.

  data: tl_mesg type table of mesg,
        sl_mesg type mesg         .

  call function 'WS_REVERSE_GOODS_ISSUE'
    exporting
      i_vbeln                   = p_likp-vbeln
      i_budat                   = sy-datlo
      i_tcode                   = 'VL09'
      i_vbtyp                   = p_likp-vbtyp
    tables
      t_mesg                    = tl_mesg
    exceptions
      error_reverse_goods_issue = 1
      others                    = 2.

  if sy-subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    wait up to 2 seconds.
  else.
    delete tl_mesg where msgty ne c_e.
    loop at tl_mesg into sl_mesg.
*     Salva Mensagens
      perform z_monta_msg using c_vl09
                                p_likp-vbeln
                                sl_mesg-text
                                space.
      clear sl_mesg.
    endloop.
  endif.

endform.                    " Z_EXECUTA_VL09

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_VL02N                                          *
*&---------------------------------------------------------------------*
*                             Executa VL02N                            *
*----------------------------------------------------------------------*
form z_executa_vl02n using p_likp type type_likp.

  data: sl_header_d type bapiobdlvhdrchg    ,
        sl_header_c type bapiobdlvhdrctrlchg,
        tl_return   type table of bapiret2  ,
        sl_return   type bapiret2           .

  sl_header_d-deliv_numb = p_likp-vbeln.
  sl_header_c-dlv_del    = 'X'.
  sl_header_c-deliv_numb = p_likp-vbeln.

  call function 'BAPI_OUTB_DELIVERY_CHANGE'
    exporting
      header_data    = sl_header_d
      header_control = sl_header_c
      delivery       = p_likp-vbeln
    tables
      return         = tl_return.

  if sy-subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    wait up to 2 seconds.
  else.
    delete tl_return where type ne c_e.
    loop at tl_return into sl_return.
*     Salva Mensagens
      perform z_monta_msg using c_vl02n
                                p_likp-vbeln
                                sl_return-message
                                space.
      clear sl_return.
    endloop.
  endif.

endform.                    " Z_EXECUTA_VL02N

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0023                                     *
*&---------------------------------------------------------------------*
*                           Seleciona ZSDT0023                         *
*----------------------------------------------------------------------*
form z_seleciona_zsdt0023.

  refresh t_zsdt0023.
  check not t_likp[] is initial.

  select vbeln mblnr_s mblnr_e dt_saida
    from zsdt0023
    into table t_zsdt0023
    for all entries in t_likp
  where  vbeln eq t_likp-vbeln.

  sort t_zsdt0023 by vbeln ascending.

endform.                    " Z_SELECIONA_ZSDT0023

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNO_MBST                                           *
*&---------------------------------------------------------------------*
*                               Estorno MBST                           *
*----------------------------------------------------------------------*
form z_estorno_mbst using p_mblnr    type zsdt0023-mblnr_s
                          p_dt_saida type zsdt0023-dt_saida.

  data: tl_return type table of bapiret2           ,
        sl_return type bapiret2                    ,
        vl_year   type bapi2017_gm_head_02-doc_year.

  check not p_mblnr is initial.

  vl_year = p_datum(04).

  call function 'BAPI_GOODSMVT_CANCEL'
    exporting
      materialdocument    = p_mblnr
      matdocumentyear     = vl_year
      goodsmvt_pstng_date = p_dt_saida
    tables
      return              = tl_return.

  if sy-subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    wait up to 2 seconds.
  else.
    delete tl_return where type ne c_e.
    loop at tl_return into sl_return.
*     Salva Mensagens
      perform z_monta_msg using c_mbst
                                p_mblnr
                                sl_return-message
                                space.
      clear sl_return.
    endloop.
  endif.

endform.                    " Z_ESTORNO_MBST

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0023FL                                   *
*&---------------------------------------------------------------------*
*                  Seleciona ZSDT0023 Formação de Lote                 *
*----------------------------------------------------------------------*
form z_seleciona_zsdt0023fl.

  refresh t_zsdt0023fl.

  CHECK t_vbfamj[] is NOT INITIAL.

  select vbeln mblnr_s mblnr_e dt_saida
    from zsdt0023
    into table t_zsdt0023fl
    for all entries in t_vbfamj
  where vbeln    eq t_vbfamj-vbelv
    and dt_saida eq p_datum.

  sort t_zsdt0023fl by vbeln ascending.

endform.                    " Z_SELECIONA_ZSDT0023FL

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKPFL                                       *
*&---------------------------------------------------------------------*
*                      Seleciona LIKP Formação de Lote                 *
*----------------------------------------------------------------------*
form z_seleciona_likpfl.

  refresh t_likpfl.
  check not t_zsdt0023fl[] is initial.

  select vbeln vbtyp
    from likp
    into table t_likpfl
    for all entries in t_zsdt0023fl
  where  vbeln eq t_zsdt0023fl-vbeln.

  sort t_likpfl by vbeln ascending.

endform.                    " Z_SELECIONA_LIKPFL

*&---------------------------------------------------------------------*
*&      Form  Z_FORM_LOTE                                              *
*&---------------------------------------------------------------------*
*        Processo Formação de Lote Estorno Mov.Mercadoria              *
*----------------------------------------------------------------------*
form z_form_lote.

  data sl_zsdt0023 type type_0023.

  loop at t_zsdt0023fl into sl_zsdt0023.

    read table t_likpfl
      with key vbeln = sl_zsdt0023-vbeln
      binary search
      transporting no fields.

    check sy-subrc is initial.

    read table t_vbfamj
      with key vbelv = sl_zsdt0023-vbeln
      binary search
      transporting no fields.

    check sy-subrc is initial.

    perform z_estorno_mbst using sl_zsdt0023-mblnr_e
                                 sl_zsdt0023-dt_saida.

    perform z_estorno_mbst using sl_zsdt0023-mblnr_s
                                 sl_zsdt0023-dt_saida.

    clear sl_zsdt0023.

  endloop.

endform.                    " Z_FORM_LOTE

*&---------------------------------------------------------------------*
*&      Form  Z_CANC_NFWRITER                                          *
*&---------------------------------------------------------------------*
*                          Cancelamento NF Writer                      *
*----------------------------------------------------------------------*
form z_canc_nfwriter.

  data sl_doc type type_doc.

  loop at t_doc into sl_doc.

*   Cancela NF Writer
    perform z_nfwriter using sl_doc-docnum.

    clear sl_doc.

  endloop.

endform.                    " Z_CANC_NFWRITER

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC                                          *
*&---------------------------------------------------------------------*
*                             Seleciona J_1BNFDOC                      *
*----------------------------------------------------------------------*
form z_seleciona_doc.

  refresh: t_doc ,
           t_linm.
  check not t_active[] is initial.

  select docnum manual
    from j_1bnfdoc
    into table t_doc
    for all entries in t_active
  where  docnum eq t_active-docnum
    and  candat eq '00000000'.
*    AND  manual EQ c_x.

  sort t_doc by docnum ascending.

  check not t_doc[] is initial.

  select docnum itmnum reftyp refkey
    from j_1bnflin
    into table t_linm
    for all entries in t_doc
  where  docnum eq t_doc-docnum.

  sort t_linm by docnum ascending
                 itmnum ascending.

endform.                    " Z_SELECIONA_DOC

*&---------------------------------------------------------------------*
*&      Form  Z_NFWRITER                                               *
*&---------------------------------------------------------------------*
*                            Cancela NF Writer                         *
*----------------------------------------------------------------------*
form z_nfwriter using p_docnum type j_1bnfdoc-docnum.

  data: sl_lin  type type_lin        ,
        vl_doc  type j_1bnfdoc-docnum,
        vl_aux  type char10          ,
        vl_aux2 type char10          ,
        vl_msg  type char100         .

  read table t_linm into sl_lin
    with key docnum = p_docnum
    binary search.
  if sl_lin-refkey is initial.
    call function 'J_1B_NF_DOCUMENT_CANCEL'
      exporting
        doc_number               = p_docnum
        ref_type                 = sl_lin-reftyp
        ref_key                  = sl_lin-refkey
      importing
        doc_number               = vl_doc
      exceptions
        document_not_found       = 1
        cancel_not_possible      = 2
        nf_cancel_type_not_found = 3
        database_problem         = 4
        docum_lock               = 5
        nfe_cancel_simulation    = 6
        others                   = 7.

    if not sy-subrc is initial.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into vl_msg.
    else.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = c_x.
      vl_msg = text-003.
    endif.
  elseif sl_lin-reftyp eq 'MD'.
    read table t_lin into sl_lin
    with key docnum = p_docnum
    binary search.

    perform z_estorno_mbst using sl_lin-vbeln
                                 p_datum.
  endif.
  vl_aux  = p_docnum.
  vl_aux2 = vl_doc.
* Salva Mensagens
  perform z_monta_msg using c_j1b3n
                            vl_aux
                            vl_msg
                            vl_aux2.

endform.                    " Z_NFWRITER

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_LOG                                              *
*&---------------------------------------------------------------------*
*                       Grava logs do processamento                    *
*----------------------------------------------------------------------*
form z_salva_log.

  data: sl_j0001 type zsdtj0001         ,
        sl_msg   type type_msg          ,
        tl_j0001 type table of zsdtj0001,
        vl_uzeit type syuzeit           ,
        vl_datum type sydatum           .

  check not t_msg[] is initial.

  vl_uzeit = sy-uzeit.
  vl_datum = sy-datum.

  loop at t_msg into sl_msg.

    sl_j0001-data      = vl_datum.
    sl_j0001-hora      = vl_uzeit.
    sl_j0001-documento = sl_msg-docu.
    sl_j0001-tcode     = sl_msg-tcode.
    sl_j0001-mensagem  = sl_msg-msg.
    sl_j0001-doc_ret   = sl_msg-doc_ret.

    append sl_j0001 to tl_j0001.

    clear: sl_msg  ,
           sl_j0001.

  endloop.

  check not tl_j0001[] is initial.

  modify zsdtj0001 from table tl_j0001.

endform.                    " Z_SALVA_LOG

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0023_DATA                                     *
*&---------------------------------------------------------------------*
*                           Seleciona ZSDT0023                         *
*----------------------------------------------------------------------*
form z_seleciona_zsdt0023_data.

  clear: t_zsdt0023,
         t_likp.

  select vbeln mblnr_s mblnr_e dt_saida
    from zsdt0023
    into table t_zsdt0023
  where  dt_saida eq p_datum.

  sort t_zsdt0023 by vbeln.

  if t_zsdt0023 is not initial.

    select vbeln vbtyp
       from likp
       into table t_likp
       for all entries in t_zsdt0023
     where  vbeln eq t_zsdt0023-vbeln.


      sort t_likp by vbeln.

      data: sl_zsdt0023 type type_0023,
            sl_likp     type type_likp.

      loop at t_zsdt0023 into sl_zsdt0023.
        read table t_likp into sl_likp with key vbeln = sl_zsdt0023-vbeln  binary search.

        if sy-subrc is not initial.

          perform z_estorno_mbst using sl_zsdt0023-mblnr_s
                                       sl_zsdt0023-dt_saida.

          perform z_estorno_mbst using sl_zsdt0023-mblnr_e
                                       sl_zsdt0023-dt_saida.
        endif.
      endloop.

  else.
    write text-005.
  endif.

endform.                    " Z_SELECIONA_ZSDT0023
