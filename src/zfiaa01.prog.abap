************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 23.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Projeto UNISIS – Imobilizados Grupo André Maggi     *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************

report  zfir0005.

*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
types: begin of ty_arq,
         filler(350),
       end of ty_arq.

data: begin of t_log occurs 0,
        code(6),
        tipo(1),
        msg(100),
      end of t_log.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
data: t_arq     type ty_arq occurs 0 with header line.

data: begin of t_as01 occurs 0,
        n(50),
        o(50),
        p(50),
        q(50),
        r(50),
        s(50),
        t(50),
        u(50),
        v(50),
        w(50),
        x(50),
        y(50),
        z(50),
        aa(50),
        bb(50),
        cc(50),
        dd(50),
        ee(50),
*        ff(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        gg(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        hh(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        ii(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        jj(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        kk(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
*        ll(50),   ##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50
        mm(50),   "*-CS2022000954-01.02.2023-#93721-JT
        nn(50),   "*-CS2022000954-01.02.2023-#93721-JT
        oo(50),   "*-CS2022000954-01.02.2023-#93721-JT
      end of t_as01.

data: begin of t_as02 occurs 0,
        a(50),
        b(50),
        c(50),
        d(50),
        e(50),
        f(50),
        g(50),
        h(50),
        i(50),
        j(50),
        k(50),
        l(50),
        m(50),
        nn(50),
        oo(50),
        pp(50),
        qq(50),
        rr(50),
        ss(50),
        tt(50),
        uu(50),
        vv(50),
        xx(50),
        zz(50),
        yy(50),
        mm2(50),   "*-CS2022000954-01.02.2023-#93721-JT
        nn2(50),   "*-CS2022000954-01.02.2023-#93721-JT
        oo2(50),   "*-CS2022000954-01.02.2023-#93721-JT
      end of t_as02.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
data:   bdcdata like bdcdata    occurs 0 with header line.
*       messages of call transaction
data:   messtab like bdcmsgcoll occurs 0 with header line.
*       error session opened (' ' or 'X')
data:   e_group_opened.
*       message texts
tables: t100.

selection-screen begin of block a1 with frame title text-h01.
  parameters: p_bukrs like bseg-bukrs obligatory,
              p_as01  radiobutton group g1 default 'X',
              p_as02  radiobutton group g1,
              p_file  like rlgrap-filename  obligatory.
selection-screen end of block a1.

at selection-screen on value-request for p_file.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
*     mask             = '*.TXT'
*     mode             = 'S'
      title            = 'Busca de Arquivo'
    importing
      filename         = p_file
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.


*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*

start-of-selection.

  perform carrega_arquivo.

  perform trata_arquivo.

  perform processa_arquivo.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
form carrega_arquivo .

  call function 'WS_UPLOAD'
    exporting
      filename                = p_file
      filetype                = 'ASC'
    tables
      data_tab                = t_arq
    exceptions
      conversion_error        = 1
      file_open_error         = 2
      file_read_error         = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      others                  = 11.

endform.                    " CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  TRATA_ARQUIVO
*&---------------------------------------------------------------------*

form trata_arquivo .

  loop at t_arq.
    if not p_as01 is initial.

      split t_arq-filler at ';' into t_as01-n
                                     t_as01-o
                                     t_as01-p
                                     t_as01-q
                                     t_as01-r
                                     t_as01-s
                                     t_as01-t
                                     t_as01-u
                                     t_as01-v
                                     t_as01-w
                                     t_as01-x
                                     t_as01-y
                                     t_as01-z
                                     t_as01-aa
                                     t_as01-bb
                                     t_as01-cc
                                     t_as01-dd
                                     t_as01-ee
*-##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50 Inicio
*                                     t_as01-ff
*                                     t_as01-gg
*                                     t_as01-hh
*                                     t_as01-ii
*                                     t_as01-jj
*                                     t_as01-kk
*                                     t_as01-ll
*-##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50 Fim
                                     t_as01-mm  "*-CS2022000954-01.02.2023-#93721-JT
                                     t_as01-nn  "*-CS2022000954-01.02.2023-#93721-JT
                                     t_as01-oo. "*-CS2022000954-01.02.2023-#93721-JT
      if not t_as01 is initial.
        append t_as01.
      endif.
    else.
      split t_arq-filler at ';' into t_as02-a
                                     t_as02-b
                                     t_as02-c
                                     t_as02-d
                                     t_as02-e
                                     t_as02-f
                                     t_as02-g
                                     t_as02-h
                                     t_as02-i
                                     t_as02-j
                                     t_as02-k
                                     t_as02-l
                                     t_as02-m
                                     t_as02-nn
                                     t_as02-oo
                                     t_as02-pp
                                     t_as02-qq
                                     t_as02-rr
                                     t_as02-ss
                                     t_as02-tt
                                     t_as02-uu
                                     t_as02-vv
                                     t_as02-xx
                                     t_as02-zz
                                     t_as02-yy
                                     t_as02-mm2  "*-CS2022000954-01.02.2023-#93721-JT
                                     t_as02-nn2  "*-CS2022000954-01.02.2023-#93721-JT
                                     t_as02-oo2. "*-CS2022000954-01.02.2023-#93721-JT

      if not t_as02 is initial.
        append t_as02.
      endif.
    endif.
  endloop.
endform.                    " TRATA_ARQUIVO

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  append bdcdata.
endform.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_ARQUIVO
*&---------------------------------------------------------------------*

form processa_arquivo .

  if not p_as01 is initial.
    perform bi_as01.
  else.
    perform bi_as02.
  endif.

  if t_log is not initial.
    write:/ 'Processados com sucesso'.
    loop at t_log where tipo = 'S'.

      write:/ t_log-msg.

    endloop.

    write:/ 'Processados com erro'.
    loop at t_log where tipo = 'E'.

      write:/ t_log-msg.

    endloop.
  endif.
endform.                    " PROCESSA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  BI_AS01
*&---------------------------------------------------------------------*
form bi_as01 .
  data: wl_erro(1),
        v_verifica(3).

  clear wl_erro.

  loop at t_as01.
    clear: bdcdata,
           v_verifica.

    refresh bdcdata.


    perform bdc_dynpro      using 'SAPLAIST' '0105'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-BUKRS'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANLA-ANLKL'
                                  t_as01-aa.
    perform bdc_field       using 'ANLA-BUKRS'
                                  t_as01-p.
* INICIO - Descomentado - 25.05.2009
    perform bdc_field       using 'RA02S-NASSETS'
*Alteração Gustavo Marques 29.05.2009 início.
                                                            "t_as01-n.
                                  '1'.
*Alteração Gustavo Marques 29.05.2009 Fim.
* FIM - Descomentado - 25.05.2009


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-ANLN1'.
    perform bdc_field       using 'ANLA-ANLN1'
                                  t_as01-n.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANLA-TXT50'
                                  t_as01-q.
    perform bdc_field       using 'ANLA-TXA50'
                                  t_as01-r.
    perform bdc_field       using 'ANLA-SERNR'
                                  t_as01-t.
* INICIO - Descomentado - 25.05.2009
    perform bdc_field       using 'ANLA-INVNR'
                                  t_as01-u.
* FIM - Descomentado - 25.05.2009

    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-IVDAT'.
    perform bdc_field       using 'ANLA-IVDAT'
                                  t_as01-v.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=TAB02'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-TXT50'.
    perform bdc_field       using 'ANLA-TXT50'
                                  t_as01-q.
    perform bdc_field       using 'ANLA-TXA50'
                                  t_as01-r.
*perform bdc_field       using 'ANLH-ANLHTXT'
*                              t_as01-.
    perform bdc_field       using 'ANLA-SERNR'
                                  t_as01-t.
* INICIO - Descomentado - 25.05.2009
    perform bdc_field       using 'ANLA-INVNR'
                                  t_as01-u.
* FIM - Descomentado - 25.05.2009

*-CS2022000954-01.02.2023-#93721-JT-inicio
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-INVZU'.
    perform bdc_field       using 'ANLA-INVZU'
                                  t_as01-mm.
*-CS2022000954-01.02.2023-#93721-JT-fim

    perform bdc_field       using 'ANLA-IVDAT'
                                  t_as01-v.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLZ-KFZKZ'.
    perform bdc_field       using 'ANLZ-GSBER'
                                  t_as01-bb.
    perform bdc_field       using 'ANLZ-KOSTL'
                                  t_as01-w.
    perform bdc_field       using 'ANLZ-WERKS'
                                  t_as01-bb.
    perform bdc_field       using 'ANLZ-STORT'
                                  t_as01-x.
* INICIO - Descomentado - 25.05.2009
    perform bdc_field       using 'ANLZ-RAUMN'
                                  t_as01-y.
    perform bdc_field       using 'ANLZ-KFZKZ'
                                  t_as01-z.
* FIM - Descomentado - 25.05.2009


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=TAB04'.  "'=BUCH'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLZ-GSBER'.
    perform bdc_field       using 'ANLZ-GSBER'
                                  t_as01-bb.
    perform bdc_field       using 'ANLZ-KOSTL'
                                  t_as01-w.
    perform bdc_field       using 'ANLZ-WERKS'
                                  t_as01-bb.
    perform bdc_field       using 'ANLZ-STORT'
                                  t_as01-x.
* INICIO - Descomentado - 25.05.2009
    perform bdc_field       using 'ANLZ-RAUMN'
                                  t_as01-y.
    perform bdc_field       using 'ANLZ-KFZKZ'
                                  t_as01-z.
* FIM - Descomentado - 25.05.2009
    " Verifica se precisa cadastrar os NDJAR

*-CS2022000954-01.02.2023-#93721-JT-inicio
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.  "'=BUCH'.
*    PERFORM bdc_field       USING 'ANLA-LIFNR'
*                                   t_as01-cc.
    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=TAB07'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'ANLA-LIFNR'.
*    PERFORM bdc_field       USING 'ANLA-LIFNR'
*                                   t_as01-cc.
    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.  "'=BUCH'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-LEANZ'.
    perform bdc_field       using 'ANLA-LKAUF'
                                  t_as01-nn.
    perform bdc_field       using 'ANLA-LEANZ'
                                  t_as01-oo.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.  "'=BUCH'.
*-CS2022000954-01.02.2023-#93721-JT-fim

    v_verifica = t_as01-n(3).

    if v_verifica ne 'OBR'.

* Adicionado conforme solicitação 24.01.2012
      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=TAB08'."'=BUCH'.
*      PERFORM bdc_field       USING 'ANLA-LIFNR'    "*-CS2022000954-01.02.2023-#93721-JT-inicio - comentado
*                                     t_as01-cc.     "*-CS2022000954-01.02.2023-#93721-JT-inicio - comentado
* Fim

* Conforme solicitação - 26.01.2012
      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=BUCH'.
      perform bdc_field       using 'ANLB-NDJAR(01)'
                                    t_as01-dd.
      perform bdc_field       using 'ANLB-NDJAR(03)'
                                    t_as01-ee.
*-##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50 Inicio
*      PERFORM bdc_field       USING 'ANLB-NDJAR(04)'
*                                    t_as01-ff.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(05)'
*                                    t_as01-gg.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(06)'
*                                    t_as01-hh.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(07)'
*                                    t_as01-ii.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(08)'
*                                    t_as01-jj.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(09)'
*                                    t_as01-kk.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(12)'
*                                    t_as01-ll.
*-##149510 AHSS - 27/08/2024 - Remover as areas 10, 11, 15, 16, 20, 30 e 50 Fim
* Fim
    else.
* Adicionado conforme solicitação 24.01.2012
      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=BUCH'.
      perform bdc_field       using 'ANLA-LIFNR'
                                     t_as01-cc.

* Fim

    endif.


    data: opt type ctu_params.

    opt-dismode = 'N'.
*    opt-dismode = 'A'.
    opt-defsize = 'X'.
    opt-defsize = 'X'.
    data p_upd type char01 value 'S'.

    call transaction 'AS01' using bdcdata
    options from opt
*    mode opt-dismode
    messages into messtab.
*    update p_upd.

    read table messtab with key msgtyp = 'A'.
    if sy-subrc = 0.
      wl_erro = 'X'.
      message s398(00) with 'Ocorreu um erro no processamento'
                            'do batch input para ativo'
                            t_as01-n '.'.
    else.
      read table messtab with key msgtyp = 'E'.
      if sy-subrc = 0.
        wl_erro = 'X'.
        message s398(00) with 'Ocorreu um erro no processamento'
                              'do batch input para ativo'
                              t_as01-n '.'.
      endif.
    endif.
  endloop.
  if wl_erro is initial.
    message s398(00) with 'Todos registros processados'
                           'com sucesso.'.
  endif.
endform.                                                    " BI_AS01
*&---------------------------------------------------------------------*
*&      Form  BI_AS02
*&---------------------------------------------------------------------*
form bi_as02 .

  data: w_bukrs       like anlz-bukrs,
        w_anln1       like anlz-anln1,
        w_anln2       like anlz-anln2,
        w_kostl       like anlz-kostl,
        v_verifica(3).

  loop at t_as02.

    clear: bdcdata,
           v_verifica.
    refresh bdcdata.

    perform bdc_dynpro      using 'SAPLAIST' '0100'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-BUKRS'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANLA-ANLN1'
                                  t_as02-a.
    perform bdc_field       using 'ANLA-ANLN2'
                                  t_as02-b.
    perform bdc_field       using 'ANLA-BUKRS'
                                  t_as02-c.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANLA-TXT50'
                                  t_as02-d.
    perform bdc_field       using 'ANLA-TXA50'
                                  t_as02-e.
*perform bdc_field       using 'ANLH-ANLHTXT'
*                              record-ANLHTXT_006.
    perform bdc_field       using 'ANLA-SERNR'
                                  t_as02-g.
    perform bdc_field       using 'ANLA-INVNR'
                                  t_as02-h.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-IVDAT'.
    perform bdc_field       using 'ANLA-IVDAT'
                                  t_as02-i.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=TAB02'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-TXT50'.
    perform bdc_field       using 'ANLA-TXT50'
                                  t_as02-d.
    perform bdc_field       using 'ANLA-TXA50'
                                  t_as02-e.
    perform bdc_field       using 'ANLH-ANLHTXT'
                                  t_as02-f.
    perform bdc_field       using 'ANLA-SERNR'
                                  t_as02-g.
    perform bdc_field       using 'ANLA-INVNR'
                                  t_as02-h.
    perform bdc_field       using 'ANLA-IVDAT'
                                  t_as02-i.


    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLZ-KOSTL'.
*perform bdc_field       using 'ANLZ-GSBER'
*                              T_AS02-.
    perform bdc_field       using 'ANLZ-KOSTL'
                                  t_as02-j.
*perform bdc_field       using 'ANLZ-WERKS'
*                              record-WERKS_018.

    w_bukrs = t_as02-c.

    w_anln1 = t_as02-a.
    shift w_anln1 right deleting trailing space.
    translate w_anln1 using ' 0'.

    w_anln2 = t_as02-b.
    shift w_anln2 right deleting trailing space.
    translate w_anln2 using ' 0'.
*
*    w_kostl = t_as02-j.
*    SHIFT w_kostl RIGHT DELETING TRAILING space.
*    TRANSLATE w_kostl USING ' 0'.
*
*    SELECT SINGLE kostl FROM anlz INTO w_kostl
*      WHERE bukrs = w_bukrs AND
*            anln1 = w_anln1 AND
*            anln2 = w_anln2 AND
*            kostl = w_kostl.
*
*    IF sy-subrc <> 0.
*      PERFORM bdc_dynpro      USING 'SAPLAIST' '3020'.
*      PERFORM bdc_field       USING 'BDC_CURSOR'
*                                    '%#AUTOTEXT001'.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '=YES'.
*    ENDIF.

    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLZ-KFZKZ'.
*perform bdc_field       using 'ANLZ-GSBER'
*                              record-GSBER_019.
    perform bdc_field       using 'ANLZ-KOSTL'
                                  t_as02-j.
*perform bdc_field       using 'ANLZ-WERKS'
*                              record-WERKS_021.
    perform bdc_field       using 'ANLZ-STORT'
                                  t_as02-k.
    perform bdc_field       using 'ANLZ-RAUMN'
                                  t_as02-l.
    perform bdc_field       using 'ANLZ-KFZKZ'
                                  t_as02-m(8).


    " Verifica se precisa cadastrar os NDJAR

    v_verifica = t_as02-a(3).

    if v_verifica ne 'OBR'.


      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=TAB08'."'=BUCH'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'ANLZ-GSBER'.
*perform bdc_field       using 'ANLZ-GSBER'
*                              record-GSBER_025.
      perform bdc_field       using 'ANLZ-KOSTL'
                                    t_as02-j.
*perform bdc_field       using 'ANLZ-WERKS'
*                              record-WERKS_027.
      perform bdc_field       using 'ANLZ-STORT'
                                    t_as02-k.
      perform bdc_field       using 'ANLZ-RAUMN'
                                    t_as02-l.
      perform bdc_field       using 'ANLZ-KFZKZ'
                                    t_as02-m(8).


* Conforme solicitação - 26.01.2012
      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=BUCH'.
*-CS2022000954-01.02.2023-#93721-JT-inicio - comentado
*      PERFORM bdc_field       USING 'ANLB-NDJAR(01)'
*                                    t_as02-qq.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(03)'
*                                    t_as02-rr.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(04)'
*                                    t_as02-ss.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(05)'
*                                    t_as02-tt.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(06)'
*                                    t_as02-uu.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(07)'
*                                    t_as02-vv.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(08)'
*                                    t_as02-xx.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(09)'
*                                    t_as02-zz.
*      PERFORM bdc_field       USING 'ANLB-NDJAR(12)'
*                                    t_as02-yy.
*-CS2022000954-01.02.2023-#93721-JT-fim - comentado
* Fim

    else.

      perform bdc_dynpro      using 'SAPLAIST' '1000'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=BUCH'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'ANLZ-GSBER'.
*perform bdc_field       using 'ANLZ-GSBER'
*                              record-GSBER_025.
      perform bdc_field       using 'ANLZ-KOSTL'
                                    t_as02-j.
*perform bdc_field       using 'ANLZ-WERKS'
*                              record-WERKS_027.
      perform bdc_field       using 'ANLZ-STORT'
                                    t_as02-k.
      perform bdc_field       using 'ANLZ-RAUMN'
                                    t_as02-l.
      perform bdc_field       using 'ANLZ-KFZKZ'
                                    t_as02-m(8).
    endif.


    data: opt type ctu_params.
    opt-dismode = 'N'.
*    opt-dismode = 'A'.
    opt-defsize = 'X'.
    opt-defsize = 'X'.

    call transaction 'AS02'
    using bdcdata
    options from opt
    messages into messtab.

    if sy-subrc = 0.
      t_log-code = 'AS02'.
      t_log-tipo = 'S'.
      concatenate 'Empresa' w_bukrs
                  'Imob.' w_anln1 '-' w_anln1
                  'Processado' into t_log-msg separated by space.
    else.
      t_log-code = 'AS02'.
      t_log-tipo = 'S'.
      concatenate 'Empresa' w_bukrs
                  'Imob.' w_anln1 '-' w_anln1
                  'erro' into t_log-msg separated by space.
    endif.
    append t_log.
    clear t_log.

  endloop.
endform.                                               "BI_AS02
