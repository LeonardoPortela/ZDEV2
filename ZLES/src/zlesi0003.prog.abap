*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : BBKO Consulting S.A.                                    *
* Data       : 17/07/2010                                              *
* Descrição  : Importação dos arquivos de Posto                        *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
* Cód Espec. :                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 17/07/2010 | BBKO         | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*
report  zlesi0003 message-id zles.

*----------------------------------------------------------------------*
* Type-pool                                                            *
*----------------------------------------------------------------------*

type-pools: zlesi.

*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
tables: zlest0013,
        zlest0007,
        zlest0008,
        zlest0009,
        adr6.
*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
types: begin of y_reg1,
         reg(1),
         id           type zchvid,
         cnpj         type stcd1,
         data         type d,
         valor        type kwert,
         conhec       type exti1,
         peso         type brgew_15,
         ctafrete     type exti2,
         cnpj_propri  type stcd1,
         rejeitado,
       end of y_reg1,

       begin of y_lfa1,
         stcd1        type stcd1,
         stcd2        type stcd2,
         lifnr        type lifnr,
         bloq         type char01,
       end of y_lfa1,

       begin of y_chkvttk,
         tdlnr        type tdlnr,
         exti1        type vttk-exti1,
         exti2        type vttk-exti2,
         id           type zchvid,
       end of y_chkvttk,

       begin of y_vttk,
         tdlnr        type tdlnr,
         exti1        type vttk-exti1,
         exti2        type vttk-exti2,
         tknum        type tknum,
       end of y_vttk,

       begin of y_vfkp,
         rebel        type rebel,
         knumv        type knumv,
       end  of y_vfkp,

       begin of y_vlrzadm,
         knumv       type knumv,
         kposn       type kposn,
         stunr       type stunr,
         zaehk       type dzaehk,
         kwert       type kwert,
         kbetr       type kbetr,
       end of y_vlrzadm,

       begin of y_file,
         linha(400),
       end of y_file,

       begin of y_chvid,
         id           type zchvid,
       end of y_chvid,

       begin of y_index,
         index        type i,
       end of y_index.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
data: t_file          type standard table of y_file
                           with header line initial size 0,
      t_idxerro       type standard table of y_index
                           with header line initial size 0,

      t_file_transf   type table of y_file,
      t_files_loc     type table of sdokpath,
      t_files_unix    type table of epsfili,
      t_zlest0007     type table of zlest0007,
      t_dir_unix      type table of epsfili,
      t_dir_local     type table of sdokpath,
      t_dir_loc_f     type table of sdokpath,
      t_zlest0013     type table of zlest0013,
      t_zlest0008     type table of zlest0008,
      t_zlesemail     type table of zlest0008,
      t_zlest0009     type table of zlest0009,
      t_zlest0025     type table of zlest0025,
      t_0009_aux      type table of zlest0009,
      t_vfkp          type table of y_vfkp,
      t_chkvttk       type table of y_chkvttk,
      t_lfa1          type table of y_lfa1,
      t_lfa2          type table of y_lfa1,
      t_vlrzadm       type table of y_vlrzadm,
      t_dest          type table of somlreci1,
      t_reg1          type table of y_reg1,
      t_reg1_aux      type table of y_reg1,
      it_log          type table of zlesi_log_processo initial size 0 with header line.

data: t_email_plist   type standard table of sopcklsti1 with header line initial size 10,
      t_email_rcvrs   type standard table of somlreci1  with header line initial size 50,
      t_email_textos  type standard table of solisti1   with header line initial size 100,
      t_anexo_bin     type standard table of solisti1   with header line initial size 0,
      t_obj_header    type standard table of solisti1   with header line initial size 1,

      t_vttk          type y_vttk  occurs 0 with header line,
      t_chvid         type y_chvid occurs 0 with header line,

      tabela          type table of zlest0013 initial size 0 with header line,
      lt_emais        type table of adr6 initial size 0 with header line,

      ti_cockpit_lote          type zles_cockpit_lote_t,
      ti_cockpit_lancto        type zles_cockpit_lancto_t,
      ti_msg                   type table of bapiret2.

data: begin of t_split occurs 0,
        valor         type char50,
      end of t_split.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
data: st_files_doc    type sdokpath,
      st_files_unix   type epsfili,
      st_zlest0007    type zlest0007,
      st_regd         type y_reg1,
      st_regh         type y_reg1,
      st_regh_aux     type y_reg1,
      st_zlest0013    type zlest0013,
      st_zlest0008    type zlest0008,
      st_zlest0009    type zlest0009,
      st_zlest0025    type zlest0025,
      st_mess         type zlest0008,
      st_lfa1h        type y_lfa1,
      st_lfa1d        type y_lfa1,
      st_vfkp         type y_vfkp,
      st_chkvttk      type y_chkvttk,
      st_vlrzadm      type y_vlrzadm,
      st_0009         type zlest0009,
      st_doc          like sodocchgi1,
      st_vttk         type y_vttk,
      st_vtpa         type vtpa,
      st_lfa1         type lfa1,
      wa_log          type zlesi_log_processo.

*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
data: v_caminho       type epsf-epsdirnam,
      v_mensagem      type bapi_msg,
      v_erro          type c,
      v_advertencia   type c,
      v_posto_file    type zcodposto,
      v_prefix_ent    type zlest0007-prefix,
      v_prefix_log    type zlest0007-prefix,
      v_prefix_proc   type zlest0007-prefix,
      v_index         type i,
      v_lote          type zlest0008-lote,
      v_parvw         type vtpa-parvw.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
constants: c_e            type c value 'E',
           c_i            type c value 'I',
           c_f            type c value 'F',
           c_l            type c value 'L',
           c_s            type c value 'S',
           c_u            type c value 'U',
           c_x            type c value 'X',
           c_w            type c value 'W',
           c_log(10)      type c value 'LOG',
           c_proc(10)     type c value 'PROC',
           c_ent(10)      type c value 'ENT',
           c_asc(10)      type c value 'ASC',
           c_mask_loc(6)  type c value '*.*',
           c_mask_unix(6) type c value '*.*',
           c_email1(14)   type c value 'ZLES_CONF_SUC',
           c_email2(14)   type c value 'ZLES_CONF_ER1',
           c_crlinefeed   type  abap_cr_lf
                          value cl_abap_char_utilities=>cr_lf.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
selection-screen begin of block b with frame title text-002.
parameters: p_input(60) type c modif id fil,
            p_proc(60)  type c modif id fil,
            p_log(60)   type c modif id fil,
            p_chkso(1)  type c no-display.
selection-screen end of block b.

selection-screen begin of block c with frame title text-003.
parameters: r_local radiobutton group 1
                    default 'X'
                    user-command scr,
            r_unix  radiobutton group 1.
selection-screen skip 1.
parameters: b_email as checkbox default 'X'.
selection-screen end of block c.

*----------------------------------------------------------------------*
* Evento de tela                                                       *
*----------------------------------------------------------------------*
at selection-screen output.

  loop at screen.
    if screen-group1 = 'FIL'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

  if p_input is initial and p_proc is initial and p_log is initial
    or t_zlest0007[] is initial.
    perform valida_tela_selecao.
  else.
    perform busca_file.
  endif.

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*

start-of-selection.

  perform le_diretorio.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILE_LOCAL
*&---------------------------------------------------------------------*
form busca_file .

  check: p_chkso is initial
     or  ( r_local = c_x  and p_chkso = c_u )
     or  ( r_unix  = c_x  and p_chkso = c_w ).

  clear: p_input,
         p_log,
         p_proc.

  perform preenche_caminho using: c_ent  changing p_input,
                                  c_log  changing p_log,
                                  c_proc changing p_proc.

  if p_input is initial or p_log  is initial or p_proc is initial.
    clear: p_chkso.
    if r_unix = c_x.
      message w003 into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
    else.
      message w004 into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
    endif.
    message v_mensagem type c_s display like c_e.
  endif.

endform.                    " BUSCA_FILE_LOCAL

*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
form valida_tela_selecao .

  clear v_erro.
  perform seleciona_interface.

  if v_erro is initial.
    perform busca_file.
  else.
    clear: p_input,
           p_proc,
           p_log,
           p_chkso.
  endif.

endform.                    " VALIDA_TELA_SELECAO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INTERFACE
*&---------------------------------------------------------------------*
form seleciona_interface .

  clear: p_chkso,
         t_zlest0007[].

  select single *
    from zlest0007
    into st_zlest0007
   where id_interface = sy-repid
     and id_ctg = c_ent
     and vlde <= sy-datum
     and vlate >= sy-datum.

  if sy-subrc is initial.
    append st_zlest0007 to t_zlest0007.
  else.
    message w026 with c_ent 'ZLES0009' into v_mensagem.
    perform envia_mensagem_procto  using sy-repid
                                         c_e
                                         '999'
                                         v_mensagem
                                         space.
    message v_mensagem type c_s display like c_e.
    v_erro = c_x.
    exit.
  endif.

  if v_erro is initial.

    select single *
      from zlest0007
      into  st_zlest0007
     where id_interface = sy-repid
       and id_ctg = c_log
       and vlde <= sy-datum
       and vlate >= sy-datum.

    if sy-subrc is initial.
      append st_zlest0007 to t_zlest0007.
    else.
      message w026 with c_log 'ZLES0009' into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
      message v_mensagem type c_s display like c_e.
      v_erro = c_x.
      exit.
    endif.

  endif.

  if v_erro is initial.

    select single *
      from zlest0007
      into  st_zlest0007
     where id_interface = sy-repid
       and id_ctg = c_proc
       and vlde <= sy-datum
       and vlate >= sy-datum.

    if sy-subrc is initial.
      append st_zlest0007 to t_zlest0007.
    else.
      message w026 with c_proc 'ZLES0009' into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
      message v_mensagem type c_s display like c_e.
      v_erro = c_x.
      exit.
    endif.

  endif.

endform.                    " SELECIONA_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
form preenche_caminho  using    v_categ
                       changing v_path.

  read table t_zlest0007 into st_zlest0007 with key id_ctg = v_categ.
  check sy-subrc is initial.

  if r_local = c_x.
    if not st_zlest0007-pathwin is initial.
      v_path = st_zlest0007-pathwin.
      p_chkso = c_w.
    endif.
  else.
    if not st_zlest0007-pathunix is initial.
      v_path = st_zlest0007-pathunix.
      p_chkso = c_u.
    endif.
  endif.

  case st_zlest0007-id_ctg.
    when c_log.
      v_prefix_log = st_zlest0007-prefix.
    when c_proc.
      v_prefix_proc = st_zlest0007-prefix.
    when c_ent.
      v_prefix_ent = st_zlest0007-prefix.
  endcase.

endform.                    " PREENCHE_CAMINHO

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO
*&---------------------------------------------------------------------*
form le_diretorio .

  data: v_index         type sy-tabix,
        v_mask_unix     type epsfilnam,
        v_mask_locl(60) type c,
        v_erro_log.

  check: not p_input is initial
     and not p_proc  is initial
     and not p_log   is initial.

  clear: v_erro_log.

  refresh: t_0009_aux,
           t_zlesemail.

* Processa arquivos de origem UNIX
  if r_unix = c_x.

    concatenate v_prefix_ent c_mask_unix into v_mask_unix.

    call function 'EPS_GET_DIRECTORY_LISTING'
      exporting
        dir_name               = p_input
        file_mask              = v_mask_unix
      tables
        dir_list               = t_dir_unix
      exceptions
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        others                 = 8.

    if sy-subrc <> 0 or t_dir_unix[] is initial.
      message w000
         with 'Diretório Unix: ' p_input
              ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
              v_mask_unix
          into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
      message v_mensagem type c_s display like c_e.
      leave list-processing.
    else.

      select *
        from zlest0008
        into table t_zlest0008
         for all entries in t_dir_unix
       where filename = t_dir_unix-name.

      sort t_zlest0008 by filename ascending
                            idctrl descending.
      delete adjacent duplicates from t_zlest0008 comparing filename.

      select *
        from zlest0009
        into table t_zlest0009
         for all entries in t_dir_unix
       where filename = t_dir_unix-name.

*     Bloqueia todos registro antes da consistência
      clear v_lote.
      loop at t_dir_unix into st_files_unix.
        v_index = sy-tabix.
        perform check_bloqueio_arquivo using st_files_unix-name
                                             v_lote
                                    changing v_erro.
        if v_erro = c_x.
          delete t_dir_unix index v_index.
          v_erro_log = c_x.
        endif.
      endloop.

*     Consiste arquivo bloqueados
      loop at t_dir_unix into st_files_unix.
        clear v_lote.
        perform carrega_arq using st_files_unix-name c_u.
        if not v_erro is initial.
          v_erro_log = c_x.
        endif.
      endloop.

    endif.

  elseif r_local = c_x.

    concatenate v_prefix_ent c_mask_loc into v_mask_locl.

    call function 'TMP_GUI_DIRECTORY_LIST_FILES'
      exporting
        directory  = p_input
        filter     = v_mask_locl
      tables
        file_table = t_dir_loc_f
        dir_table  = t_dir_local
      exceptions
        cntl_error = 1
        others     = 2.

    if sy-subrc <> 0 or t_dir_loc_f[] is initial.
      message w000
         with 'Diretório Local: ' p_input
              ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
              v_mask_locl
          into v_mensagem.
      perform envia_mensagem_procto  using sy-repid
                                           c_e
                                           '999'
                                           v_mensagem
                                           space.
      message v_mensagem type c_s display like c_e.
      leave list-processing.
    else.

      select *
        from zlest0008
        into table t_zlest0008
         for all entries in t_dir_loc_f
       where filename = t_dir_loc_f-pathname(40).

      sort t_zlest0008 by filename ascending
                            idctrl descending.
      delete adjacent duplicates from t_zlest0008 comparing filename.

      select *
        from zlest0009
        into table t_zlest0009
         for all entries in t_dir_loc_f
       where filename = t_dir_loc_f-pathname(40).

*     Bloqueia todos registro antes da consistência
      clear v_lote.
      loop at t_dir_loc_f into st_files_doc.
        v_index = sy-tabix.
        perform check_bloqueio_arquivo using st_files_doc-pathname
                                             v_lote
                                   changing  v_erro.
        if v_erro = c_x.
          delete t_dir_loc_f index v_index.
          v_erro_log = c_x.
        endif.
      endloop.

*     Consiste arquivo bloqueado
      loop at t_dir_loc_f into st_files_doc.
        clear v_lote.
        perform processa using st_files_doc-pathname c_l.
        if not v_erro is initial.
          v_erro_log = c_x.
        endif.
      endloop.
    endif.
  endif.

* Limpa controle de bloqueio que foi criado neste processo
  delete zlest0009 from table t_0009_aux.

* Verifica se houve erro em algum processamento...
  if v_erro_log is initial.
    if v_advertencia is not initial.
      message w000(zles) with 'Arquivos processado com advertência(s)!'
                         display like c_w.
    else.
      message s000(zles) with 'Arquivos processado!'
                         display like c_s.
    endif.
  else.
    message s000(zles)
       with 'Existem arquivos/registros que não foram'
            'processados,'
            'verificar transaçãö LOG ZLES0010'
       display like c_e.
    exit.
  endif.

endform.                    " LE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
form carrega_arq  using  v_file
                         v_tipo.

  data: v_caminho     type string,
        lidx_aux1     type i,
        lidx_aux2     type i,
        sl_msg        type bapiret2,
        r_lote        type lxhme_range_c10_t,
        wr_lote       type lxhme_range_c10.

  clear:  v_posto_file,
          v_erro,
          v_advertencia.

  refresh: t_file,
           t_idxerro,
           t_chkvttk,
           t_lfa1,
           t_vttk,
           t_vfkp,
           t_vlrzadm,
           t_chvid,
           t_zlest0025.

  concatenate p_input v_file into v_caminho.

  perform le_arquivo_unix_window using v_tipo
                                       v_caminho.

  perform transfer_arqlido_tabreg.
  check not t_reg1[] is initial.

  perform consiste_header_tabreg using v_file
                                       v_tipo
                              changing st_lfa1h.
  check v_erro is initial.

  perform consiste_detgeral_tabreg using v_file
                                         v_tipo
                                         st_lfa1h.
  check v_erro is initial.

  sort: t_vttk      by tdlnr exti1 exti2,
        t_vfkp      by rebel,
        t_vlrzadm   by knumv kposn,
        t_zlest0025 by chvid.

  perform consiste_detindiv_tabreg using v_file st_regh st_lfa1h.

  clear: r_lote.


  wr_lote-sign   = 'I'.
  wr_lote-option = 'EQ'.
  wr_lote-low  = st_zlest0013-lote.

  append wr_lote to r_lote.

  call function 'Z_LES_COCKPIT_AUTOMACAO_POSTO'
    exporting
      rt_lote   = r_lote[]
    tables
      t_msg     = ti_msg
      t_lotes   = ti_cockpit_lote
      t_lanctos = ti_cockpit_lancto.

  if not ti_msg is initial.

    refresh: it_log.

    v_advertencia = c_x.

    loop at ti_msg into sl_msg.
      wa_log = sl_msg-message.

      append wa_log to it_log.
    endloop.

  endif.

  if not t_idxerro[] is initial.
    perform transfere_file using p_input p_log v_tipo v_file space.
    v_erro = c_x.
  endif.

  if not tabela[] is initial.
    v_advertencia = c_x.
  endif.

  perform transfere_file using p_input p_proc v_tipo v_file c_x.

endform.                    " CARREGA_ARQ

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO_UNIX_WINDOW
*&---------------------------------------------------------------------*
form le_arquivo_unix_window  using  p_tipo
                                    p_path.

  data: l_path type string.
  l_path = p_path.

  refresh t_file.
  clear   t_file.

  if p_tipo = c_u.
*   Lê arquivo UNIX
    open dataset v_caminho for input in binary mode.
    do.
      read dataset p_path into t_file.
      if sy-subrc  is initial.
        append t_file.
      else.
        exit.
      endif.
    enddo.

  elseif p_tipo = c_l.
*   Lê arquivo WINDOWS
    call function 'GUI_UPLOAD'
      exporting
        filename                = l_path
        filetype                = 'ASC'
      tables
        data_tab                = t_file
      exceptions
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        others                  = 17.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

endform.                    " LE_ARQUIVO_UNIX_WINDOW

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_ARQLIDO_TABREG
*&---------------------------------------------------------------------*
form transfer_arqlido_tabreg.

  refresh t_reg1.

  loop at t_file.

    clear: st_regd.

    split t_file-linha at '|' into table t_split.
    case t_file-linha(1).

      when '1'.
        loop at t_split.
          case sy-tabix.
            when 1.
              st_regd-reg = t_split-valor.
            when 2.
              st_regd-cnpj = t_split-valor.
            when 3.
              "PERFORM converte_char_date USING t_split-valor
              "                        CHANGING st_regd-data.
              st_regd-data = sy-datum.
            when 4.
              perform converte_char_decimal using t_split-valor
                                         changing st_regd-valor.
            when others.
              exit.
          endcase.
        endloop.

      when others.

        loop at t_split.
          case sy-tabix.
            when 1.
              st_regd-reg = t_split-valor.
            when 2.
              st_regd-id = t_split-valor.
            when 3.
              perform converte_char_decimal using t_split-valor
                                         changing st_regd-valor.
            when 4.
              st_regd-cnpj = t_split-valor.
            when 5.
              write t_split-valor to st_regd-conhec
              using edit mask '==ALPHA'.
            when 6.
              perform converte_char_decimal using t_split-valor
                                         changing st_regd-peso.
            when 7.
              perform converte_char_date using t_split-valor
                                      changing st_regd-data.
            when 8.
              write t_split-valor to st_regd-ctafrete
              using edit mask '==ALPHA'.
            when 9.
              st_regd-cnpj_propri = t_split-valor.
            when others.
              exit.
          endcase.
        endloop.

    endcase.

    append st_regd to t_reg1.

  endloop.

endform.                    " TRANSFER_ARQLIDO_TABREG

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_HEADER_TABREG
*&---------------------------------------------------------------------*
form consiste_header_tabreg using ph_file
                                  ph_tipo
                         changing ph_lfa1h.

  data: l_nrmsg        type bdc_mnr,
        l_dscbloqueio type qsperrgr,
        vg_tabix      type sy-tabix,
        n_stcd        type i.

  clear: ph_lfa1h,
         st_regh,
         v_erro.

  read table t_reg1 index 1 into st_regh.
  if st_regh-reg <> '1'.
    v_erro = '1'.
  else.

    n_stcd = strlen( st_regh-cnpj ).

    if n_stcd gt 11.

      select stcd1 lifnr
        into corresponding fields of table t_lfa1
        from lfa1
        where stcd1 =  st_regh-cnpj.

    else.

      select stcd2 lifnr
        into corresponding fields of table t_lfa1
        from lfa1
        where stcd2 =  st_regh-cnpj.

    endif.

    if sy-subrc is initial.

      loop at t_lfa1 into st_lfa1h.

        vg_tabix = sy-tabix.

        call function 'Z_VERIFICA_CLI_FOR_CTA_MAT'
          exporting
            p_koart      = 'K'
            p_fornecedor = st_lfa1h-lifnr
          exceptions
            error        = 1
            others       = 2.

        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into v_mensagem.
          wa_log-texto  = v_mensagem.
          st_lfa1h-bloq   = 'X'.
        endif.

        modify t_lfa1 index vg_tabix from st_lfa1h transporting bloq.

      endloop.

      delete t_lfa1 where bloq eq 'X'.

      if t_lfa1 is not initial.

        read table t_lfa1 into st_lfa1h index 1.
        v_posto_file = st_lfa1h-lifnr.

*     Verifica se o Posto está liberado para processamento
        select single dscbloqueio
          into l_dscbloqueio
         from zlest0017
        where codposto = st_lfa1h-lifnr.

        if sy-subrc is initial.
          v_erro = '3'.
        endif.

      else.
        v_erro = '4'.
      endif.

    else.
      v_erro = '2'.
    endif.

  endif.

  if not v_erro is initial.
    l_nrmsg = '101'.
    if v_erro = '1'.
      v_mensagem = 'Registro de header do arquivo não foi encontrado'.
    elseif v_erro = '2'.
      concatenate 'CNPJ' st_regh-cnpj 'do reg. header para o posto está inválido' into v_mensagem separated by space.
    elseif v_erro = '3'.
      concatenate 'Posto' st_lfa1h-lifnr 'bloqueado pelo motivo' l_dscbloqueio into v_mensagem separated by space.
    elseif v_erro = '4'.
      " Mensagem retornada pelo módulo de função Z_VERIFICA_CLI_FOR_CTA_MAT
    endif.

    perform envia_mensagem_procto using ph_file
                                        c_e
                                        l_nrmsg
                                        v_mensagem
                                        space.

    perform transfere_file using p_input
                                  p_log
                                  ph_tipo
                                  ph_file
                                  c_x.
*    IF NOT v_erro = '3'.
*      PERFORM f_email USING c_e ph_file.
*    ENDIF.
    v_erro = c_x.
    exit.
  endif.

* Move informação de dados do Header
  if n_stcd gt 11.
    read table t_lfa1 into ph_lfa1h with key stcd1 = st_regh-cnpj binary search.
  else.
    read table t_lfa1 into ph_lfa1h with key stcd2 = st_regh-cnpj binary search.
  endif.

endform.                    " CONSISTE_HEADER_TABREG

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_DETGERAL_TABREG
*&---------------------------------------------------------------------*
form consiste_detgeral_tabreg  using  pd_file
                                      pd_tipo
                                      pd_lfa1h.
  data: lidx_aux1     type i,
        lidx_aux2     type i,
        w_lfa1        type y_lfa1,
        vg_tabix      type sy-tabix.

  clear: v_erro, st_regd.

  t_reg1_aux[] = t_reg1[].
  st_lfa1h = pd_lfa1h.

  delete t_reg1_aux where reg = '1'. "Elimina o Header

  describe table t_reg1     lines lidx_aux1.
  describe table t_reg1_aux lines lidx_aux2.

  subtract 1 from lidx_aux1.
  if lidx_aux1 <> lidx_aux2.
    v_mensagem ='Formato do arq. inválido, layout esperado Header(1) + Detalhe(n)'.
    wa_log-texto = v_mensagem.
    append wa_log to it_log.
    perform envia_mensagem_procto using pd_file c_e '002' v_mensagem v_lote.
    perform transfere_file using p_input p_log pd_tipo pd_file c_x.
    v_erro = c_x.
    exit.
  endif.

* CNPJS - Registros detalhes
  if not t_reg1_aux[] is initial.

    select stcd1 lifnr
      appending corresponding fields of table t_lfa1
      from lfa1
       for all entries in t_reg1_aux
     where stcd1 = t_reg1_aux-cnpj.

    loop at t_lfa1 into w_lfa1.

      vg_tabix = sy-tabix.

      call function 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        exporting
          p_koart      = 'K'
          p_fornecedor = w_lfa1-lifnr
        exceptions
          error        = 1
          others       = 2.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into v_mensagem.
        wa_log-texto = v_mensagem.
        append wa_log to it_log.
        w_lfa1-bloq = c_x.
        modify t_lfa1 index vg_tabix from w_lfa1 transporting bloq.
      endif.

    endloop.

    delete t_lfa1 where bloq eq c_x.

    sort t_lfa1 by stcd1.
  endif.

  describe table t_lfa1 lines lidx_aux2. "(Header+Detalhe)
  subtract 1 from lidx_aux2. "Menos 1 reg de header
  if lidx_aux2 <= 0.
    v_mensagem = 'Nenhum dos CNPJ no registro detalhe do arquivo estão cadastrados'.
    wa_log-texto = v_mensagem.
    append wa_log to it_log.
    perform envia_mensagem_procto using pd_file c_e '102' v_mensagem v_lote.
    perform transfere_file using p_input p_log pd_tipo pd_file c_x.
    v_erro = c_x.
    exit.
  endif.

  if not t_reg1_aux[] is initial.

    loop at  t_reg1_aux into st_regd.
      read table t_lfa1 into st_lfa1d with key stcd1 = st_regd-cnpj
                        binary search.
      if sy-subrc is initial.
        st_chkvttk-tdlnr = st_lfa1d-lifnr.
        st_chkvttk-exti1 = st_regd-conhec.
        st_chkvttk-exti2 = st_regd-ctafrete.
        st_chkvttk-id    = st_regd-id.
        append st_chkvttk to t_chkvttk.
      endif.
    endloop.

    sort t_chkvttk.
    delete adjacent duplicates from t_chkvttk.

    if not t_chkvttk[] is initial.
      select tdlnr exti1 exti2 tknum
        from vttk
        into table t_vttk
        for all entries in t_chkvttk
        where tdlnr = t_chkvttk-tdlnr
          and exti1 = t_chkvttk-exti1
          and exti2 = t_chkvttk-exti2.
    endif.

  endif.

  if t_vttk[] is initial.
    v_mensagem = 'Nenhuma DACTE e Carta Frete do arquivo estão cadastrados (Posto verificar númeração correta)'.
    wa_log-texto = v_mensagem.
    append wa_log to it_log.
    perform envia_mensagem_procto using pd_file c_e '103' v_mensagem v_lote.
    perform transfere_file using p_input p_log pd_tipo pd_file c_x.

    loop at t_chkvttk into st_chkvttk.

      clear: st_zlest0025.

      if not st_chkvttk-id is initial.
        select single *
          into st_zlest0025
          from zlest0025
         where chvid = st_chkvttk-id.
      endif.

      if st_zlest0025 is initial.
        concatenate 'Carta Frete' st_chkvttk-exti2 'e/ou CT-e' st_chkvttk-exti1 'não cadastrado' into v_mensagem separated by space.
      else.
        concatenate st_zlest0025-deschvid 'DACTE' st_chkvttk-exti1 'e/ou carta frete' st_chkvttk-exti2 'não cadastrado (Posto verificar numeração)' into v_mensagem separated by space.
      endif.
      wa_log-texto = v_mensagem.
      append wa_log to it_log.
      perform envia_mensagem_procto using pd_file c_e '006' v_mensagem v_lote.

    endloop.

    v_erro = c_x.
    exit.
  else.

    select rebel knumv
       into table t_vfkp
       from vfkp
        for all entries in t_vttk
      where rebel = t_vttk-tknum
        and refty = '8'
        and fkpty = 'Z001'.

    if not t_vfkp[] is initial.
      select knumv kposn stunr zaehk kwert kbetr
        into table t_vlrzadm
        from konv
         for all entries in t_vfkp
       where knumv = t_vfkp-knumv
         and kschl = 'ZADM'.
    endif.

  endif.

  loop at t_reg1_aux into st_regd.
    append st_regd-id to t_chvid.
  endloop.

  sort t_chvid.
  delete adjacent duplicates from t_chvid.

  if not t_chvid[] is initial.
    select *
      into table t_zlest0025
      from zlest0025
       for all entries in t_chvid
     where chvid = t_chvid-id.
  endif.

  if t_zlest0025[] is initial.
    v_mensagem = 'Código de histórico do lançamento não cadastrado (Posto verificar código de histórico correto)'.
    wa_log-texto = v_mensagem.
    append wa_log to it_log.
    perform envia_mensagem_procto using pd_file c_e '104' v_mensagem v_lote.
    perform transfere_file using p_input p_log pd_tipo pd_file c_x.
    v_erro = c_x.
    exit.
  endif.

endform.                    " CONSISTE_DETGERAL_TABREG

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_DETINDIV_TABREG
*&---------------------------------------------------------------------*
form consiste_detindiv_tabreg  using pi_file
                                     pi_regh
                                     pi_lfa1h.

  data: begin of lt_valor occurs 2,
          codposto    type zcodposto,
          valor       type kwert,
        end   of lt_valor.

  data: lidx_aux2     type i,
        lc_valor1(15) type c,
        lc_valor2(15) type c,
        lp_valor      type kwert.

* Cabecalho Header
  st_regh  = pi_regh.
  st_lfa1h = pi_lfa1h.

  refresh: t_zlest0013.
  clear:   t_zlest0013.

* Gera número do Lote
  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr = '01'
      object      = 'ZLOTE'
    importing
      number      = v_lote.

* Verifica razão social para carta frete e conhecimento.
  perform check_razao_social_docto using pi_file.

* Consiste registro detalhes
* Tabela T_IDXERRO contem o indexador do registro referente ao
* arquivo original, Nota a tabela T_REG1_AUX não contem o reg.header
  loop at t_reg1_aux into st_regd.

    lidx_aux2 = sy-tabix.
    check st_regd-rejeitado is initial.

*   Valida código Controle de Identificação do registro (ChvId)
    read table t_zlest0025 into st_zlest0025
       with key chvid = st_regd-id binary search.
    if sy-subrc <> 0.
      concatenate 'Controle Identificação (ChvId)' st_regd-id 'não cadastrado' into v_mensagem separated by space.
      wa_log-texto = v_mensagem.
      append wa_log to it_log.

      perform envia_mensagem_procto using pi_file c_e '003' v_mensagem v_lote.
      t_idxerro-index = lidx_aux2 + 1.
      append t_idxerro.
      continue.
    endif.

*   Verifica se controle de identificação está liberado
    if st_zlest0025-bl <> '2'.
      concatenate 'Controle Identificação (ChvId)' st_regd-id 'bloqueado' into v_mensagem separated by space.
      wa_log-texto = v_mensagem.
      append wa_log to it_log.

      perform envia_mensagem_procto using pi_file c_e '004' v_mensagem v_lote.
      t_idxerro-index = lidx_aux2 + 1.
      append t_idxerro.
      continue.
    endif.

*   Valida Código de Fornecedor
    read table t_lfa1 into st_lfa1d with key stcd1 = st_regd-cnpj
                                    binary search.
    if sy-subrc <> 0.
      concatenate 'CNPJ' st_regd-cnpj 'não cadastrado'
             into v_mensagem separated by space.

      wa_log-texto = v_mensagem.
      append wa_log to it_log.

      perform envia_mensagem_procto using pi_file c_e '005' v_mensagem v_lote.
      t_idxerro-index = lidx_aux2 + 1.
      append t_idxerro.
      continue.
    endif.

    if st_regd-id <> '5'.

*   Valida conhecimento / carta frete
      read table t_vttk into st_vttk with key tdlnr = st_lfa1d-lifnr
                                              exti1 = st_regd-conhec
                                              exti2 = st_regd-ctafrete
                                    binary search.
      if sy-subrc <> 0.
        concatenate st_zlest0025-deschvid 'DACTE' st_regd-conhec 'e/ou carta frete' st_regd-ctafrete 'não cadastrado (Posto verificar numeração)' into v_mensagem separated by space.
        ""CONCATENATE 'Carta Frete' st_regd-ctafrete 'e/ou CT-e' st_regd-conhec 'não cadastrado' INTO v_mensagem SEPARATED BY space.
        wa_log-texto = v_mensagem.
        append wa_log to it_log.

        perform envia_mensagem_procto using pi_file c_e '006' v_mensagem v_lote.
        t_idxerro-index = lidx_aux2 + 1.
        append t_idxerro.
        continue.
      endif.

    endif.


    if not st_regd-cnpj_propri is initial.

      " Módulo de funcção para parceiros "SD_PARTNER_READ" (VBPA,VTPA)
      call function 'CONVERSION_EXIT_PARVW_INPUT'
        exporting
          input  = 'PV'
        importing
          output = v_parvw.

      select single * into st_vtpa
        from vtpa
       where vbeln eq st_vttk-tknum
         and parvw eq v_parvw.

      if sy-subrc eq 0.

        clear: st_lfa1.

        select single * into st_lfa1
          from lfa1
          where lifnr eq st_vtpa-lifnr.

        if ( sy-subrc <> 0 ) or
           ( st_lfa1-stkzn is initial and st_regd-cnpj_propri ne st_lfa1-stcd1 ) or
           ( not st_lfa1-stkzn is initial and st_regd-cnpj_propri ne st_lfa1-stcd2 ) .
          concatenate st_zlest0025-deschvid 'DACTE' st_regd-conhec 'com CNPJ do proprietário do veículo CNPJ/CPF' st_regd-cnpj_propri 'inválido!'
                 into v_mensagem separated by space.

          wa_log-texto = v_mensagem.
          append wa_log to it_log.

          perform envia_mensagem_procto using pi_file c_e '010' v_mensagem v_lote.
          t_idxerro-index = lidx_aux2 + 1.
          append t_idxerro.
          continue.
        endif.
      else.
        concatenate 'Doc. de transporte' st_vttk-tknum 'não possui parceiro de proprietário do veículo!'
               into v_mensagem separated by space.

        wa_log-texto = v_mensagem.
        append wa_log to it_log.

        perform envia_mensagem_procto using pi_file c_e '010' v_mensagem v_lote.
        t_idxerro-index = lidx_aux2 + 1.
        append t_idxerro.
        continue.
      endif.
    endif.

*   Valida se Valor para carta frete / Conhecimento foi informado
    if st_regd-valor is initial.
      concatenate st_zlest0025-deschvid 'não informado na carta Frete' st_regd-ctafrete 'e CT-e' st_regd-conhec
             into v_mensagem separated by space.

      wa_log-texto = v_mensagem.
      append wa_log to it_log.

      perform envia_mensagem_procto using pi_file c_e '007' v_mensagem v_lote.
      t_idxerro-index = lidx_aux2 + 1.
      append t_idxerro.
      continue.
    endif.

*   Valida Adiantamento
    if st_zlest0025-ctlgchavid = 'AD'
      and st_zlest0025-vdarcohec = c_s.
      perform obtem_vlrdocto_historico_1 using st_vttk-tknum
                                      changing lp_valor.
      if st_regd-valor <> lp_valor.
        write: st_regd-valor to lc_valor1 no-sign no-zero,
               lp_valor      to lc_valor2 no-sign no-zero.
        condense: lc_valor1, lc_valor2.
        concatenate st_zlest0025-deschvid 'DACTE' st_regd-conhec 'de R$' lc_valor2 'com valor diferente do sistema (Posto verificar valor)'
               into v_mensagem separated by space.

        wa_log-texto = v_mensagem.
        append wa_log to it_log.

        perform envia_mensagem_procto using pi_file c_e '008' v_mensagem v_lote.
        t_idxerro-index = lidx_aux2 + 1.
        append t_idxerro.
        continue.
      endif.
    endif.

*   Valida Gastos de reembolso
    if st_zlest0025-ctlgchavid = 'AP'
      and st_zlest0025-vdarcohec = c_s.

      select codposto valor
        into table lt_valor
        from zlest0028
       where ( codposto = st_lfa1h-lifnr or
               codposto <= space )
         and codtrp   = st_lfa1d-lifnr
         and chvid    = st_regd-id
         and conhec   = st_regd-conhec
         and ctafrete = st_regd-ctafrete.

      clear lt_valor.
      if not lt_valor[] is initial.
        read table lt_valor with key codposto = st_lfa1h-lifnr.
        if not sy-subrc is initial.
          read table lt_valor with key codposto = space.
        endif.
      endif.

      if st_regd-valor <> lt_valor-valor.
        write: st_regd-valor  to lc_valor1 no-sign no-zero,
               lt_valor-valor to lc_valor2 no-sign no-zero.
        condense: lc_valor1, lc_valor2.
        concatenate st_zlest0025-deschvid 'DACTE' st_regd-conhec ' de R$' lc_valor1 'não registrado no sistema (Posto enviar cópia dacte)' into v_mensagem separated by space.
        wa_log-texto = v_mensagem.
        append wa_log to it_log.

        perform envia_mensagem_procto using pi_file c_e '009' v_mensagem v_lote.
        t_idxerro-index = lidx_aux2 + 1.
        append t_idxerro.
        continue.
      endif.

    endif.

*   Grava registros OK...
    move: st_regh-cnpj        to st_zlest0013-cnpj_posto,
          st_regh-data        to st_zlest0013-datalote,
          st_regh-valor       to st_zlest0013-vlrlote,
          v_lote              to st_zlest0013-lote,
          st_lfa1h-lifnr      to st_zlest0013-codposto,
          st_regd-cnpj        to st_zlest0013-cnpj_trp,
          st_lfa1d-lifnr      to st_zlest0013-codtrp,
          st_regd-ctafrete    to st_zlest0013-ctafrete,
          st_regd-conhec      to st_zlest0013-conhec,
          st_regd-id          to st_zlest0013-chvid,
          st_regd-valor       to st_zlest0013-vlrconhec,
          st_regd-peso        to st_zlest0013-qtde,
          st_regd-data        to st_zlest0013-dtacheg,
          c_i                 to st_zlest0013-status,
          sy-datum            to st_zlest0013-data,
          sy-uzeit            to st_zlest0013-hora,
          sy-uname            to st_zlest0013-usuario.

    append st_zlest0013 to t_zlest0013.

  endloop.

  check: not t_zlest0013[] is initial.

  clear: tabela[], tabela.

  perform elimina_registros_importados tables t_zlest0013 using pi_file v_lote.

  perform elimina_registros_duplicados tables t_zlest0013 using pi_file v_lote.

  if not t_zlest0013[] is initial.
    insert zlest0013 from table t_zlest0013.
  endif.

  if tabela[] is initial.
    v_mensagem = 'DACTE e Carta Frete importados com sucesso!'.
    perform envia_mensagem_procto using pi_file c_s '105' v_mensagem v_lote.

    concatenate 'Carta Frete' st_regd-ctafrete 'e/ou CT-e' st_regd-conhec 'importado com sucesso!'
           into v_mensagem separated by space.

    wa_log-texto = v_mensagem.
    append wa_log to it_log.
  else.
    v_mensagem = 'DACTE e Carta Frete importados com advertências!'.
    perform envia_mensagem_procto using pi_file c_w '105' v_mensagem v_lote.

    concatenate 'Carta Frete' st_regd-ctafrete 'e/ou CT-e' st_regd-conhec 'importado com advertências!'
           into v_mensagem separated by space.

    wa_log-texto = v_mensagem.
    append wa_log to it_log.
  endif.

endform.                    " CONSISTE_DETINDIV_TABREG

*&---------------------------------------------------------------------*
*&      Form  OBTEM_VLRDOCTO_HISTORICO_1
*&---------------------------------------------------------------------*
form obtem_vlrdocto_historico_1  using  p_knum
                               changing p_valor.

  clear p_valor.

  read table t_vfkp into st_vfkp with key rebel = p_knum binary search.
  check: sy-subrc is initial.

  read table t_vlrzadm into st_vlrzadm with key knumv = st_vfkp-knumv
       binary search.
  check: sy-subrc is initial.

  v_index = sy-tabix.

  do.
*   p_valor = p_valor + st_vlrzadm-kwert.
    p_valor = p_valor + st_vlrzadm-kbetr.
    add 1 to v_index.
    read table t_vlrzadm into st_vlrzadm index v_index.
    if sy-subrc <> 0 or st_vlrzadm-knumv <> st_vfkp-knumv.
      exit.
    endif.
  enddo.

endform.                    " OBTEM_VLRDOCTO_HISTORICO_1


*&---------------------------------------------------------------------*
*&      Form  CHECK_RAZAO_SOCIAL_DOCTO
*&---------------------------------------------------------------------*
form check_razao_social_docto using pi_file.

  data: lqbr_cnpj       type stcd1,
        ltabix          type i.

* Se existir mais de uma razão social(Raiz) para o arquivo aceitar o
* primeiro CNPJ e ignorar os demais.
  t_reg1[] = t_reg1_aux[].
  sort t_reg1 by cnpj.

  loop at t_reg1 into st_regd.

    if sy-tabix = 1.
      lqbr_cnpj = st_regd-cnpj.
    endif.

    check: st_regd-cnpj(8) <> lqbr_cnpj(8).

    loop at t_reg1_aux into st_regh_aux.
      ltabix = sy-tabix.
      if st_regh_aux-cnpj(8) <> lqbr_cnpj(8).
        t_idxerro-index = ltabix + 1.
        append t_idxerro.
        v_mensagem = 'Lote rejeitado por existir mais de uma razão social (Posto enviar arquivo por empresa)'.
        perform envia_mensagem_procto using pi_file c_e '106' v_mensagem v_lote.
        st_regh_aux-rejeitado = c_x.
        modify t_reg1_aux from st_regh_aux index ltabix
                  transporting rejeitado.
      endif.
    endloop.

*   Encerra a valiação
    exit.

  endloop.

endform.                    " CHECK_RAZAO_SOCIAL_DOCTO

*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
form transfere_file  using  v_de
                            v_para
                            v_tipo
                            v_file
                            v_dele.

  data: v_dest    type rlgrap-filename,
        v_sour    type rlgrap-filename,
        v_dest1   type string,
        v_sour1   type string,
        v_rc_bool type c,
        v_rc_num  type i,
        v_orig    type char4,
        li_nreg1  type i,
        li_nreg2  type i,
        l_path    type string.


* Identifica o destino da transferência
  if v_para = p_log.
    v_orig = 'ERRO'.
  else.
    v_orig = 'OK'.
  endif.

* Gera o Path para gravação
  concatenate v_para v_file into v_dest.
  concatenate v_de   v_file into v_sour.

* Gera o Path para check e transferência
  v_dest1 = v_para. "Somente o diretório
  v_sour1 = v_sour. "Path completo

* Limpa tabela de anexo de e-mail
  refresh t_file_transf.

* Analisa o ambiente para processamento
  if v_tipo = c_u.

    open dataset v_dest for output in text mode encoding default.
    if sy-subrc is initial.

      loop at t_file.

*       O header do arquivo é sempre salvo independente do erro
        if sy-tabix > 1.
          clear t_idxerro.
          read table t_idxerro with key index = sy-tabix.
          if v_orig = 'OK' and t_idxerro-index > 0.
            continue.
          elseif t_idxerro-index = 0.
            if v_orig = 'ERRO' and not t_idxerro[] is initial.
              continue.
            endif.
          endif.
        endif.

        transfer t_file-linha to v_dest.
        append  t_file to t_file_transf.

      endloop.
    endif.
    close dataset v_dest.

    if v_dele = c_x.
      delete dataset v_de.
    endif.

  elseif v_tipo = c_l.

    describe table t_file    lines li_nreg1.
    describe table t_idxerro lines li_nreg2.
    subtract 1 from li_nreg1. "subtrai o registro Header

*   Tudo certo ou Tudo errado
    if ( v_orig = 'OK'   and li_nreg2 is initial ) or
       ( v_orig = 'ERRO' and ( li_nreg1 = li_nreg2 or
         li_nreg2 is initial ) ).

*     Verifica a existência do diretório
      call method cl_gui_frontend_services=>directory_exist
        exporting
          directory = v_dest1
        receiving
          result    = v_rc_bool.

*     Cria o diretório para transferência
      if v_rc_bool is initial.
        call method cl_gui_frontend_services=>directory_create
          exporting
            directory = v_dest1
          changing
            rc        = v_rc_num.
      endif.

*     Transfere o arquivo para o diretório
      v_dest1 = v_dest. "Path completo
      call method cl_gui_frontend_services=>file_copy
        exporting
          source      = v_sour1
          destination = v_dest1
          overwrite   = 'X'.

      t_file_transf[] = t_file[].

    else.

      loop at t_file.
*       O header do arquivo é sempre salvo independente do erro
        if sy-tabix > 1.
          clear t_idxerro.
          read table t_idxerro with key index = sy-tabix.
          if v_orig = 'OK' and t_idxerro-index > 0.
            continue.
          elseif t_idxerro-index = 0.
            if v_orig = 'ERRO'  and not t_idxerro[] is initial.
              continue.
            endif.
          endif.
        endif.

        append  t_file to t_file_transf.
      endloop.

      l_path = v_dest.

      call function 'GUI_DOWNLOAD'
        exporting
          filename                = l_path
          filetype                = c_asc
        tables
          data_tab                = t_file_transf
        exceptions
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          others                  = 22.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

    endif.

*   Elimina o arquivo de origem
    if v_dele = c_x.
      call method cl_gui_frontend_services=>file_delete
        exporting
          filename             = v_sour1
        changing
          rc                   = v_rc_num
        exceptions
          file_delete_failed   = 1
          cntl_error           = 2
          error_no_gui         = 3
          file_not_found       = 4
          access_denied        = 5
          unknown_error        = 6
          not_supported_by_gui = 7
          wrong_parameter      = 8
          others               = 9.
    endif.

  endif.

endform.                    " TRANSFERE_FILE

*&---------------------------------------------------------------------*
*&      Form  f_email
*&---------------------------------------------------------------------*
*       Preenche dados do E-mail e Envia
*----------------------------------------------------------------------*
form f_email using p_type v_filename.

  field-symbols <linha>   type solisti1.

  data: v_nrlinha         type i,
        v_datum(10)       type c,
        v_uzeit(8)        type c,
        v_linha255        type char255,
        vl_stand_text     like  thead-tdname.

  data: tl_line      like tline occurs 0 with header line.

  perform busca_email.

  refresh: t_email_plist,
           t_email_textos,
           t_obj_header,
           t_email_rcvrs.

  clear: t_email_plist,
         t_email_textos,
         t_obj_header,
         t_email_rcvrs.

* Destinatário de E-mails
  loop at lt_emais.
    t_email_rcvrs-receiver  = lt_emais-smtp_addr.
    t_email_rcvrs-express   = c_x.
    t_email_rcvrs-rec_type  = 'U'.
    t_email_rcvrs-com_type = 'INT'.
    t_email_rcvrs-notif_del = c_x.
    t_email_rcvrs-notif_ndel = c_x.
    append t_email_rcvrs.
  endloop.

* Identifica a chave do texto que contem o corpo do E-mail
* Nota: 1) Os textos foram criadoa através da transação standard SO10
*       2) O transporte/criação de request através do programa RSTXTRAN
  if p_type = c_s.
    "  Texto não criado para sucesso!!!!!
    vl_stand_text = c_email1.
  elseif p_type = c_e.
    vl_stand_text = c_email2.
  endif.

* Busca texto para o E-mail
  call function 'READ_TEXT'
    exporting
      id                      = 'ST'
      language                = 'P'
      name                    = vl_stand_text
      object                  = 'TEXT'
    tables
      lines                   = tl_line
    exceptions
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      others                  = 8.

  loop at tl_line.

    t_email_textos-line = tl_line-tdline.

    if t_email_textos-line cs '&ARQ&'.
      replace '&ARQ&' with v_filename into t_email_textos-line.
      condense t_email_textos-line.
    elseif t_email_textos-line cs '&DATA&'.
      concatenate sy-datum+6(2) sy-datum+4(2) sy-datum(4)
             into v_datum separated by '/'.
      replace '&DATA&' with v_datum into t_email_textos-line.
      condense t_email_textos-line.
    elseif t_email_textos-line cs '&HORA&'.
      concatenate sy-uzeit(2) sy-uzeit+2(2) sy-uzeit+4(2)
             into v_uzeit separated by ':'.
      replace '&HORA&' with v_uzeit into t_email_textos-line.
      condense t_email_textos-line.
    elseif t_email_textos-line cs '&USER&'.
      replace '&USER&' with sy-uname into t_email_textos-line.
      condense t_email_textos-line.
    endif.

    append t_email_textos. clear t_email_textos.

  endloop.

* Condições de erro para posto/transportadora
  if not t_zlesemail[] is initial.
    clear t_email_textos.
    append t_email_textos.
    append t_email_textos.
    t_email_textos-line = 'Inconsistências:'.
    append t_email_textos.
    clear t_email_textos.
    assign t_email_textos to <linha>.
    loop at t_zlesemail into st_mess.
      <linha> = st_mess-msgv1.
      append t_email_textos.
    endloop.
  endif.

* Seta dados gerais do email
  describe table t_email_textos lines v_nrlinha.
  read table t_email_textos index v_nrlinha.

* Dados gerais do email
  st_doc-doc_size = ( v_nrlinha - 1 ) * 255 + strlen( t_email_textos ).
  st_doc-obj_name = 'SAPRPT'.
  st_doc-sensitivty = c_f.
  st_doc-obj_langu = sy-langu.
  st_doc-obj_descr = text-t00.

* Seta dados de transmissão para o corpo do email
  perform seta_formato_email  using space
                                    v_nrlinha
                                    'RAW'
                                    space.

* Envia o anexo dos registros errados
  if p_type = c_e.

    st_doc-obj_descr = text-t01.

    if 1 <> 1.  "Rotina Inibida...

*     Seta o nome do arquivo de anexo
      find first occurrence of '.' in v_filename.
      if sy-subrc <> 0.
        concatenate  v_filename '.TXT' into t_obj_header.
      else.
        t_obj_header = v_filename.
      endif.
      append t_obj_header.
*     Gera o arquivo de erros
      assign t_anexo_bin to <linha>.
      loop at t_file_transf into v_linha255.
        <linha> = v_linha255.
        <linha>+253(2) = c_crlinefeed.
        append t_anexo_bin.
      endloop.
*     Seta dados de transmissão para arquivo em anexo
      describe table t_anexo_bin lines v_nrlinha.
      perform seta_formato_email  using c_x
                                        v_nrlinha
                                        'TXT'
                                        t_obj_header.
    endif.
  endif.

* Envia o e-mail
  call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    exporting
      document_data              = st_doc
      put_in_outbox              = c_x
      commit_work                = c_x
    tables
      packing_list               = t_email_plist
      object_header              = t_obj_header
      contents_bin               = t_anexo_bin
      contents_txt               = t_email_textos
      receivers                  = t_email_rcvrs
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      others                     = 8.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform. " f_email

*&---------------------------------------------------------------------*
*&      Form  SETA_FORMATO_EMAIL
*&---------------------------------------------------------------------*
form seta_formato_email  using p_eh_anexo
                               p_numreg
                               p_tipodoc
                               p_nome_anexo.
  clear: t_email_plist.

  t_email_plist-transf_bin = p_eh_anexo. "X ou space
  t_email_plist-head_start = 1.
  if p_eh_anexo is initial.
    t_email_plist-head_num = 0.
  else.
    t_email_plist-head_num  = 1.
    t_email_plist-obj_name  = p_nome_anexo.
    t_email_plist-obj_descr = p_nome_anexo.
    t_email_plist-doc_size  = p_numreg * 255.
  endif.
  t_email_plist-body_start = 1.
  t_email_plist-body_num = p_numreg.   "Qde linhas corpo/anexo do email
  t_email_plist-doc_type = p_tipodoc.  "RAW ou tipo extensão do anexo
  append t_email_plist.

endform.                    " SETA_FORMATO_EMAIL

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DECIMAL
*&---------------------------------------------------------------------*
form converte_char_decimal  using  p_valor
                         changing p_valor_dec.

  data: lc_valor type char30.

  clear p_valor_dec.
  lc_valor = p_valor.

  replace all occurrences of regex '[^0-9,]' in lc_valor
                             with ''.

  call function 'OIU_ME_CHAR_TO_NUMBER'
    exporting
      i_char               = lc_valor
   importing
*  E_FLOAT              =
     e_packed             = p_valor_dec
   exceptions
     invalid_number       = 1
     others               = 2.

endform.                    " CONVERTE_CHAR_DECIMAL

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DATE
*&---------------------------------------------------------------------*
form converte_char_date  using  p_char_data
                      changing  p_dats_data.

  data:  l_aux_data(8) type c,
         l_aux(10)     type c,
         l_dia(2),
         l_mes(2),
         l_ano(4).

  clear: p_dats_data.
  l_aux = p_char_data.

  replace all occurrences of regex '[^0-9\s]' in l_aux
    with '/'.

  if sy-subrc is initial.
    split l_aux at '/' into l_dia l_mes l_ano.
    concatenate l_ano l_mes l_dia into l_aux_data.
  else.
    l_aux_data = l_aux.
  endif.

  call function 'RP_CHECK_DATE'
    exporting
      date         = l_aux_data
    exceptions
      date_invalid = 1
      others       = 2.

  if sy-subrc is initial.
    p_dats_data = l_aux_data.
  endif.

endform.                    " CONVERTE_CHAR_DATE

*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQUEIO_ARQUIVO
*&---------------------------------------------------------------------*
form check_bloqueio_arquivo  using  p_filename
                                    p_lote
                          changing  p_erro.

  data: lc_mensagem  type bdc_vtext1.
  clear: p_erro.

  read table t_zlest0009 into st_zlest0009 with key filename = p_filename.
  if not sy-subrc is initial.

    st_zlest0009-filename = p_filename.
    st_zlest0009-data     = sy-datum.
    st_zlest0009-hora     = sy-uzeit.
    st_zlest0009-usuario  = sy-uname.

    append: st_zlest0009 to t_zlest0009,
            st_zlest0009 to t_0009_aux.

    insert zlest0009 from st_zlest0009.
    clear st_zlest0009.

  else.

    p_erro = c_x.
    concatenate 'Arquivo' p_filename 'já esta sendo processado.'
           into lc_mensagem separated by space.

    perform envia_mensagem_procto  using  p_filename
                                        c_e
                                        '107'
                                        lc_mensagem
                                        p_lote.
  endif.

endform.                    " CHECK_BLOQUEIO_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  ENVIA_MENSAGEM_PROCTO
*&---------------------------------------------------------------------*
form envia_mensagem_procto  using p_filename
                                  p_msgtyp
                                  p_msgnr
                                  p_msgv1
                                  p_lote.

  statics: lv_filename  type epsfilnam value %_maxchar,
           lv_version   type zidctrl,
           lv_vcont     type numc10.

  if lv_filename <> p_filename.

    clear: st_zlest0008,
           lv_vcont.

    if p_filename = sy-repid.

      select max( idctrl ) max( cont )
        into (st_zlest0008-idctrl, lv_vcont)
        from zlest0008
       where filename = p_filename
        group by idctrl.
      endselect.

      if sy-subrc is initial.
        if lv_vcont >= '9999999998'.
          lv_version = st_zlest0008-idctrl + 1.
          clear lv_vcont.
        else.
          lv_version = st_zlest0008-idctrl.
        endif.
      else.
        lv_version = st_zlest0008-idctrl + 1.
      endif.

    else.
      read table t_zlest0008 into st_zlest0008 with key filename = p_filename.
      lv_version = st_zlest0008-idctrl + 1.
    endif.

    lv_filename = p_filename.
    refresh t_zlesemail.
  endif.

  add 1 to lv_vcont.
  st_mess-filename = p_filename.
  st_mess-idctrl   = lv_version.
  st_mess-tcode    = sy-tcode.
  st_mess-cont     = lv_vcont.
  st_mess-dyname   = 'LES'.
  st_mess-msgtyp   = p_msgtyp.
  st_mess-msgspra  = sy-langu.
  st_mess-msgid    = 'FR'.
  st_mess-msgnr    = p_msgnr.
  st_mess-msgv1    = p_msgv1.
  st_mess-data     = sy-datum.
  st_mess-hora     = sy-uzeit.
  st_mess-usuario  = sy-uname.
  st_mess-lote     = p_lote.

  insert zlest0008 from st_mess.

* Mensagens que deverão ser anexada no envio de email - Somente Erros
* Pode-se neste ponto filtrar a mensagem que se deseja
* enviar no corpo do e-amail
  if st_mess-msgtyp <> c_s.
    if st_mess-msgnr = '008' or st_mess-msgnr = '009'.
*     Omite o valor SAP
      replace first occurrence of regex 'SAP.*\W.*' in st_mess-msgv1
                                   with 'SAP'.
    endif.
    append st_mess to t_zlesemail.
  endif.

  write: p_msgv1.

  clear st_mess.

endform.                    " ENVIA_MENSAGEM_PROCTO

*&---------------------------------------------------------------------*
*&      Form  ELIMINA_REGISTROS_IMPORTADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ZLEST0013  text
*----------------------------------------------------------------------*
form elimina_registros_importados  tables p_t_zlest0013 structure zlest0013 using pi_file v_lote.

  data: wa_registro  type zlest0013,
        wa_datavenc  type zlest0015,
        wa_zlest0025 type zlest0025,
        vg_data      type c length 10.

  data: dia  type c length 2,
        mes  type c length 2,
        ano  type c length 4,
        data type c length 10.

  loop at p_t_zlest0013 into st_zlest0013.

    if st_zlest0013-chvid ne '5'.

      clear: wa_registro.

      select single *
        into wa_registro
        from zlest0013
       where cnpj_trp eq st_zlest0013-cnpj_trp
         and conhec   eq st_zlest0013-conhec
         and chvid    eq st_zlest0013-chvid.

      if sy-subrc is not initial.
        continue.
      endif.

      select single *
        into wa_datavenc
        from zlest0015
      where lote eq wa_registro-lote.

      if sy-subrc eq 0.
        append st_zlest0013 to tabela.
        read table t_zlest0025 into wa_zlest0025 with key chvid = st_zlest0013-chvid binary search.
        if sy-subrc is initial.
          write wa_datavenc-vencimento to vg_data.
          concatenate wa_zlest0025-deschvid 'DACTE' st_zlest0013-conhec 'da transportadora'
                      st_zlest0013-cnpj_trp 'está pago em' vg_data 'lote' wa_registro-lote
                      into v_mensagem separated by space.
        else.


          "dia = wa_registro-datalote+6(2).
          dia = wa_datavenc-vencimento+6(2).
          mes = wa_datavenc-vencimento+4(2).
          ano = wa_datavenc-vencimento(4).
          concatenate dia '.' mes '.' ano into data.


          case st_zlest0013-chvid.
            when '1'.
              concatenate 'Adiantamento DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em' data 'lote' wa_registro-lote
              into v_mensagem separated by space.
            when '2'.
              concatenate 'Saldo de frete DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago a outro posto em' data 'lote' wa_registro-lote
              into v_mensagem separated by space.
            when '6'.
              concatenate 'Estadia DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em' data 'lote' wa_registro-lote
              into v_mensagem separated by space.
            when '13'.
              concatenate 'Bonificação DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em ' data 'lote ' wa_registro-lote
              into v_mensagem separated by space.
            when '15'.
              concatenate 'Pedagio DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em' data 'lote' wa_registro-lote
              into v_mensagem separated by space.
            when '24'.
              concatenate 'Dif. de Frete DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em' data 'lote' wa_registro-lote
              into v_mensagem separated by space.

          endcase.
        endif.

        perform envia_mensagem_procto using pi_file c_e '109' v_mensagem v_lote.

*              CONCATENATE 'Conhecimento chave' st_zlest0013-chvid 'da transportadora' st_zlest0013-cnpj_trp 'com nr ' st_zlest0013-conhec
*              'já importado no lote ' wa_registro-lote INTO v_mensagem SEPARATED BY space.


      elseif ( sy-subrc is not initial ) and ( wa_registro is not initial ).
        append st_zlest0013 to tabela.
*        CONCATENATE 'Conhecimento chave ' st_zlest0013-chvid 'da transportadora' st_zlest0013-cnpj_trp 'com nr ' st_zlest0013-conhec
*                    ' já importado no lote ' wa_registro-lote
*                    INTO v_mensagem SEPARATED BY space.


        dia = wa_datavenc-vencimento+6(2).
        mes = wa_datavenc-vencimento+4(2).
        ano = wa_datavenc-vencimento(4).
        concatenate dia '.' mes '.' ano into data.



        case st_zlest0013-chvid.
          when '1'.
            concatenate 'Adiantamento DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em'  data 'lote' wa_registro-lote
            into v_mensagem separated by space.
          when '2'.
            concatenate 'Saldo de frete DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago a outro posto em'  data 'lote' wa_registro-lote
            into v_mensagem separated by space.
          when '6'.
            concatenate 'Estadia DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em'  data'lote' wa_registro-lote
            into v_mensagem separated by space.
          when '13'.
            concatenate 'Bonificação DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em '  data 'lote ' wa_registro-lote
            into v_mensagem separated by space.
          when '15'.
            concatenate 'Pedagio DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em'  data 'lote' wa_registro-lote
            into v_mensagem separated by space.
          when '24'.
            concatenate 'Dif. de Frete DACTE' st_zlest0013-conhec 'e/ou CF' wa_registro-ctafrete 'já pago para outro posto em' data 'lote' wa_registro-lote
            into v_mensagem separated by space.
        endcase.

        perform envia_mensagem_procto using pi_file c_e '109' v_mensagem v_lote.

      endif.
    endif.
  endloop.

  loop at tabela into st_zlest0013.
    delete t_zlest0013 where cnpj_trp eq st_zlest0013-cnpj_trp and conhec eq st_zlest0013-conhec and chvid  eq st_zlest0013-chvid.
  endloop.


endform.                    " ELIMINA_REGISTROS_IMPORTADOS

*&---------------------------------------------------------------------*
*&      Form  ELIMINA_REGISTROS_DUPLICADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form elimina_registros_duplicados  tables p_t_zlest0013 structure zlest0013 using pi_file v_lote.

  data: tempd type table of zlest0013 with header line initial size 0,
        temp  type table of zlest0013 with header line initial size 0,
        wa_zlest0025 type zlest0025,
        lin   type i.

  move p_t_zlest0013[] to tempd[].

  sort tempd by cnpj_trp conhec chvid.

  loop at p_t_zlest0013 into st_zlest0013.
    lin = 0.
    loop at tempd where cnpj_trp = st_zlest0013-cnpj_trp
                    and conhec   = st_zlest0013-conhec
                    and chvid    = st_zlest0013-chvid.
      lin = lin + 1.
    endloop.
    if lin ge 2.
      append st_zlest0013 to temp.
    endif.
  endloop.

  sort temp by cnpj_trp conhec chvid.

  loop at temp into st_zlest0013.
    read table t_zlest0025 into wa_zlest0025 with key chvid = st_zlest0013-chvid binary search.
    if sy-subrc is initial.
      concatenate wa_zlest0025-deschvid 'DACTE' st_zlest0013-conhec 'da transportadora'
                  st_zlest0013-cnpj_trp 'duplicado no lote' v_lote
                  into v_mensagem separated by space.
    else.
      concatenate 'Conhecimento chave ' st_zlest0013-chvid 'da transportadora' st_zlest0013-cnpj_trp 'com nr ' st_zlest0013-conhec
                  ' duplicado no lote ' v_lote into v_mensagem separated by space.
    endif.
    perform envia_mensagem_procto using pi_file
                                        c_e
                                        '110'
                                        v_mensagem
                                        v_lote.
    delete t_zlest0013 where cnpj_trp eq st_zlest0013-cnpj_trp and conhec eq st_zlest0013-conhec and chvid  eq st_zlest0013-chvid.
    append st_zlest0013 to tabela.
  endloop.

endform.                    " ELIMINA_REGISTROS_DUPLICADOS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0500 output.

*  DATA: html       TYPE TABLE OF w3html INITIAL SIZE 0,
*        listobject TYPE TABLE OF abaplist INITIAL SIZE 0,
*        vg_html    TYPE string.
*
*  "Variáveis de e-mail
*  DATA: document_data TYPE sodocchgi1,
*        packing_list  TYPE TABLE OF sopcklsti1 WITH HEADER LINE,
*        receivers     TYPE TABLE OF somlreci1 WITH HEADER LINE.
*
*  CLEAR: it_log[].
*
*  PERFORM carrega_arq USING st_files_doc-pathname c_l.
*
*  IF ( NOT it_log[] IS INITIAL ) AND ( NOT v_erro IS INITIAL ).
*
*    PERFORM add TABLES html USING '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>Log de Importação de Arquivo</STRONG></FONT></DIV><BR>'.
*    PERFORM add TABLES html USING '<FONT face=Verdana color=#0000ff size=2>'.
*
*    PERFORM add TABLES html USING '<table align="center" cellspacing="0" cellpadding="1" border="1">'.
*    CONCATENATE '<tr><td colspan="1" align="center" bgcolor="666666"><font color="#FFFFFF" size=1><strong>'
*                'Mensagens de Processamento de Importação de Arquivo'
*                '</strong></font></td></tr>'
*           INTO vg_html.
*    PERFORM add TABLES html USING vg_html.
*    CONCATENATE '<tr>'
*                '<td><font color="#000030" size=2><b>Sequência</b></font></td>'
*                '<td><font color="#000030" size=2><b>Mensagem</b></font></p></td>'
*                '</tr>' INTO vg_html.
*
*    LOOP AT it_log INTO wa_log.
*      CONCATENATE '<tr>'
*                  '<td><font color="#000000" size=2>' wa_log-texto '</font></td>'
*                  '</tr>' INTO vg_html.
*      PERFORM add TABLES html USING vg_html.
*    ENDLOOP.
*
*    PERFORM add TABLES html USING '</table>'.
*
*    document_data-obj_name  = 'ProcArqP'.
*    document_data-obj_descr = 'Arq. Processamento Posto - Grupo Maggi'.
*    document_data-no_change = 'X'.
*
*    packing_list-head_start = 1.
*    packing_list-head_num   = 60.
*    packing_list-body_start = 1.
*    packing_list-body_num   = 160.
*    packing_list-doc_type   = 'HTM'.
*    APPEND packing_list.
*
*    PERFORM busca_email.
*
*    LOOP AT lt_emais.
*      receivers-receiver = lt_emais-smtp_addr.
*      receivers-rec_type = 'U'.
*      receivers-express  = 'X'.
*      APPEND receivers.
*    ENDLOOP.
*
*    IF NOT receivers[] IS INITIAL.
*      CALL FUNCTION 'ZHTML_ENVIA_EMAIL_CP'
*        EXPORTING
*          i_titulo      = 'Importação de Arquivo de Postos'
*          document_data = document_data
*        TABLES
*          packing_list  = packing_list
*          html          = html
*          receivers     = receivers.
*    ENDIF.
*
*  ENDIF.


  set user-command 'BACK'.

endmodule.                 " STATUS_0500  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  BUSCA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_EMAIS  text
*----------------------------------------------------------------------*
form busca_email.

  check: not v_posto_file is initial and b_email = c_x.
  data:  lt_trans type table of lfa1 initial size 0 with header line,
         wa_trans type lfa1,
         wa_lfa1  type y_lfa1.

  clear: lt_emais[], lt_trans[].

* Elimina codigo do posto de gasolina
  delete t_lfa1 where lifnr = v_posto_file.

* Busca emails para envio de ocorrências posto x transportadora
  if t_lfa1[] is initial.
    call function 'Z_LES_EMAIL'
      exporting
        v_posto  = v_posto_file
        v_status = '2'
      tables
        lt_emais = lt_emais.
  else.
    loop at t_lfa1 into wa_lfa1.
      wa_trans-lifnr = wa_lfa1-lifnr.
      append  wa_trans to lt_trans.
    endloop.
    call function 'Z_LES_EMAIL'
      exporting
        v_posto  = v_posto_file
        v_status = '2'
      tables
        lt_trans = lt_trans
        lt_emais = lt_emais.
  endif.

* Se existem email válidos transmite ocorrências
  if lt_emais[] is initial.
    write: /'Foi solicitado o envio de e-mail',
           /'Nenhum endereço foi adquirido, ver transação ZLES0029'.
    exit.
  endif.

  sort lt_emais.
  delete adjacent duplicates from lt_emais.

endform.                    " BUSCA_EMAIL

*&---------------------------------------------------------------------*
*&      Form  ADD
*&---------------------------------------------------------------------*
*       Add código HTML - Truncar
*----------------------------------------------------------------------*
form add  tables   p_html  structure w3html
          using    p_texto type string.
  call function 'ZHTML_ADD'
    exporting
      i_texto = p_texto
    tables
      it_html = p_html.
endform.                    " ADD

*&---------------------------------------------------------------------*
*&      Form  PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form processa using  v_file v_tipo.

  data: html       type table of w3html initial size 0,
        listobject type table of abaplist initial size 0,
        vg_html    type string.

  "Variáveis de e-mail
  data: document_data type sodocchgi1,
        packing_list  type table of sopcklsti1 with header line,
        receivers     type table of somlreci1 with header line.

  clear: it_log[].

  perform carrega_arq using v_file v_tipo.

  if ( not it_log[] is initial ) and ( not v_erro is initial ) and ( t_zlest0013[] is initial ).

    perform add tables html using '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>Log de Importação de Arquivo</STRONG></FONT></DIV><BR>'.
    perform add tables html using '<FONT face=Verdana color=#0000ff size=2>'.

    perform add tables html using '<table align="center" cellspacing="0" cellpadding="1" border="1">'.
    concatenate '<tr><td colspan="1" align="center" bgcolor="666666"><font color="#FFFFFF" size=1><strong>'
                'Mensagens de Processamento de Importação de Arquivo'
                '</strong></font></td></tr>'
           into vg_html.
    perform add tables html using vg_html.
    concatenate '<tr>'
                '<td><font color="#000030" size=2><b>Sequência</b></font></td>'
                '<td><font color="#000030" size=2><b>Mensagem</b></font></p></td>'
                '</tr>' into vg_html.

    loop at it_log into wa_log.
      concatenate '<tr>'
                  '<td><font color="#000000" size=2>' wa_log-texto '</font></td>'
                  '</tr>' into vg_html.
      perform add tables html using vg_html.
    endloop.

    perform add tables html using '</table>'.

    document_data-obj_name  = 'ProcArqP'.
    document_data-obj_descr = 'Processamento Posto - Grupo Maggi'.
    document_data-no_change = 'X'.

    packing_list-head_start = 1.
    packing_list-head_num   = 60.
    packing_list-body_start = 1.
    packing_list-body_num   = 160.
    packing_list-doc_type   = 'HTM'.
    append packing_list.

    perform busca_email.

    loop at lt_emais.
      receivers-receiver = lt_emais-smtp_addr.
      receivers-rec_type = 'U'.
      receivers-express  = 'X'.
      append receivers.
    endloop.

    if not receivers[] is initial.
      call function 'ZHTML_ENVIA_EMAIL_CP'
        exporting
          i_titulo      = 'Importação de Arquivo de Postos'
          document_data = document_data
        tables
          packing_list  = packing_list
          html          = html
          receivers     = receivers.
    endif.

  endif.

endform.                    " PROCESSA
