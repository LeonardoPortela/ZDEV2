*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : BBKO Consulting S.A.                                    *
* Data       : 07/07/2010                                              *
* Descrição  : Importação dos arquivos da ALL                          *
*              Etapa 1 Rodo - Ferro - Confirmação de chegada           *
* Prefixo    : L1                                                      *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 07/07/2010 | BBKO         | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*

REPORT  zlesi0005 MESSAGE-ID zles.
*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES:
  zlest0019,
  zlest0007,
  zlest0008,
  zlest0009.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF y_reg10,
         tipo(2),
         movto(1),
         empresa(70),
         cnpj(14),
         dataenv(10),
         horaenv(8),
         observ(100),
         tabix       TYPE sy-tabix,
         erro        TYPE c,
         id_refkey   TYPE zlest0019-id_zlest0019,
       END OF y_reg10,

       BEGIN OF y_reg20,
         tipo(2),
         idvagao(11),
         dcl(10),
         pesovagao    TYPE brgew_15,
         datadesc(10),
         horadesc(8),
         seriedcl(3),
         tabix        TYPE sy-tabix,
         index        TYPE sy-index,
         erro         TYPE c,
       END OF y_reg20,

       BEGIN OF y_reg30,
         tipo(2),
         nf(11),
         pesonota       TYPE brgew_15,
         pesochgd       TYPE brgew_15,
         datachgd(10),
         cnpj(14),
         compl(54),
         prod(6),
         tabix          TYPE sy-tabix,
         index          TYPE sy-index,
         erro           TYPE c,
         docnum         TYPE j_1bnfdoc-docnum,
         bukrs          TYPE j_1bbranch-bukrs,
         branch         TYPE j_1bbranch-branch,
         nr_nf_terceiro TYPE zlest0019-nr_nf_terceiro,
         cod_fornecedor TYPE zlest0019-cod_fornecedor,
         tp_transgenia  TYPE zde_tp_transgenia,
       END OF y_reg30,

       BEGIN OF y_reg40,
         tipo(2),
         totreg30  TYPE numc3,
         totpeso30 TYPE brgew_15,
         tabix     TYPE sy-tabix,
         index     TYPE sy-index,
         erro      TYPE c,
       END OF y_reg40,

       BEGIN OF y_reg50,
         tipo(2),
         totreg30  TYPE numc3,
         totpeso30 TYPE brgew_15,
       END OF y_reg50,

       BEGIN OF y_branch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE j_1bbranch-branch,
         stcd1  TYPE j_1bbranch-stcd1,
       END OF y_branch,

       BEGIN OF y_nfdoc,
         docnum     TYPE j_1bnfdoc-docnum,
         nfenum     TYPE j_1bnfdoc-nfenum,
         nfnum      TYPE j_1bnfdoc-nfnum,
         series     TYPE j_1bnfdoc-series,
         bukrs      TYPE j_1bnfdoc-bukrs,
         branch     TYPE j_1bnfdoc-branch,
         cgc        TYPE j_1bnfdoc-cgc,
         cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
       END OF y_nfdoc,

       BEGIN OF y_index,
         index TYPE i,
       END OF y_index,

       BEGIN OF y_file,
         linha(400),
       END OF y_file,

       BEGIN OF y_msg,
         repid      TYPE sy-repid,
         direcao(1) TYPE c,
         msg_num(3) TYPE c,
         msg_seq(3) TYPE i,
         texto(255) TYPE c,
         tabix      TYPE sy-tabix,
       END OF y_msg,

       BEGIN OF y_epsfili.
         INCLUDE STRUCTURE epsfili.
TYPES: nome TYPE sdokpath-pathname,
       END OF y_epsfili.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA:  t_file        TYPE STANDARD TABLE OF y_file
                           WITH HEADER LINE INITIAL SIZE 0,

       t_idxerro     TYPE STANDARD TABLE OF y_index
                          WITH HEADER LINE INITIAL SIZE 0,

       t_file_transf TYPE TABLE OF y_file.

DATA: t_files_unix   TYPE TABLE OF epsfili,
      t_zlest0007    TYPE TABLE OF zlest0007,
      t_dir_unix     TYPE TABLE OF epsfili,
      t_dir_unix2    TYPE TABLE OF y_epsfili,
      t_dir_local    TYPE TABLE OF sdokpath,
      t_dir_loc_f    TYPE TABLE OF sdokpath,
      t_reg20        TYPE TABLE OF y_reg20,
      t_reg30        TYPE TABLE OF y_reg30,
      t_reg40        TYPE TABLE OF y_reg40,
      t_reg20a       TYPE TABLE OF y_reg20,
      t_reg30a       TYPE TABLE OF y_reg30,
      t_reg40a       TYPE TABLE OF y_reg40,
      t_zlest0019    TYPE TABLE OF zlest0019,
      t_zlest0008    TYPE TABLE OF zlest0008,
      t_zlest0009    TYPE TABLE OF zlest0009,
      t_0009_aux     TYPE TABLE OF zlest0009,
      t_kna1         TYPE TABLE OF kna1,
      t_msg          TYPE TABLE OF y_msg,
      t_branch       TYPE TABLE OF y_branch,
      t_zlest0041    TYPE TABLE OF zlest0041,
      t_nfedoc       TYPE TABLE OF y_nfdoc,
      t_nfdoc        TYPE TABLE OF y_nfdoc,
      t_dest         TYPE TABLE OF somlreci1,
      t_zlest0035    TYPE TABLE OF zlest0035,
      t_zib_nfe_forn TYPE TABLE OF zib_nfe_forn,
      t_zlest0087    TYPE TABLE OF zlest0087,
      t_log_ext      TYPE zlest0008_t,
      t_frete        TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: st_files_doc    TYPE sdokpath,
      st_files_unix   TYPE epsfili,
      st_files_unix2  TYPE y_epsfili,
      st_zlest0007    TYPE zlest0007,
      st_zlest0019    TYPE zlest0019,
      st_zlest0008    TYPE zlest0008,
      st_zlest0009    TYPE zlest0009,
      st_mess         TYPE zlest0008,
      st_msg          TYPE y_msg,
      st_reg10        TYPE y_reg10,
      st_reg20        TYPE y_reg20,
      st_reg30        TYPE y_reg30,
      st_reg30_2      TYPE y_reg30,
      st_reg50        TYPE y_reg50,
      st_reg40        TYPE y_reg40,
      st_reg40_check  TYPE y_reg40,
      st_reg50_check  TYPE y_reg50,
      st_branch       TYPE y_branch,
      st_zlest0041    TYPE zlest0041,
      st_nfdoc        TYPE y_nfdoc,
      st_0009         TYPE zlest0009,
      st_kna1         TYPE kna1,
      st_lfa1         TYPE lfa1,
      st_zib_nfe_forn TYPE zib_nfe_forn,
      st_zlest0087    TYPE zlest0087,
      st_zlest0089    TYPE zlest0089,
      vl_ctrlfile     TYPE c.
*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: v_caminho     TYPE sdokpath-pathname, "EPSF-EPSDIRNAM,
      v_mensagem    TYPE bapi_msg,
      v_msg(255)    TYPE c,
      v_erro        TYPE c,
      v_prefix_ent  TYPE zlest0007-prefix,
      v_prefix_log  TYPE zlest0007-prefix,
      v_prefix_proc TYPE zlest0007-prefix,
      v_version     TYPE zlest0008-idctrl,
      v_leave,
      v_call_ext    TYPE c.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_l1(2)        TYPE c VALUE 'L1',      " Confirmação de saída Ferroviário
           c_asc(10)      TYPE c VALUE 'ASC',
           c_mask_loc(6)  TYPE c VALUE '*.txt',
           c_mask_unix(6) TYPE c VALUE '*.*',
           c_u            TYPE c VALUE 'U',
           c_w            TYPE c VALUE 'W',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S'.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_input(60) TYPE c MODIF ID fil,
              p_proc(60)  TYPE c MODIF ID fil,
              p_log(60)   TYPE c MODIF ID fil,
              p_chkso(1)  TYPE c NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE TEXT-003.
  PARAMETERS: r_local RADIOBUTTON GROUP 1
                      DEFAULT 'X'
                      USER-COMMAND scr,
              r_unix  RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK c.

*----------------------------------------------------------------------*
* Evento de tela                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'FIL'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZLES0012_CNPJ'
    TABLES
      set_values    = t_frete
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_frete BY from.

  IF p_input IS INITIAL AND p_proc IS INITIAL AND p_log IS INITIAL
    OR t_zlest0007[] IS INITIAL.
    PERFORM valida_tela_selecao.
  ELSE.
    PERFORM busca_file.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM le_diretorio.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILE_LOCAL
*&---------------------------------------------------------------------*
FORM busca_file .

  CHECK: p_chkso IS INITIAL
     OR  ( r_local = c_x  AND p_chkso = c_u )
     OR  ( r_unix  = c_x  AND p_chkso = c_w ).

  CLEAR: p_input,
         p_log,
         p_proc.

  PERFORM preenche_caminho USING: c_ent  CHANGING p_input,
                                  c_log  CHANGING p_log,
                                  c_proc CHANGING p_proc.

  IF p_input IS INITIAL OR p_log  IS INITIAL OR p_proc IS INITIAL.
    CLEAR: p_chkso.
    IF r_unix = c_x.
      MESSAGE w003 INTO v_mensagem.
      PERFORM envia_mensagem_procto  USING sy-repid
                                           c_e
                                           '999'
                                           v_mensagem.
    ELSE.
      MESSAGE w004 INTO v_mensagem.
      PERFORM envia_mensagem_procto  USING sy-repid
                                           c_e
                                           '999'
                                           v_mensagem.
    ENDIF.
    MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.
  ENDIF.

ENDFORM.                    " BUSCA_FILE_LOCAL

*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
FORM valida_tela_selecao .

  CLEAR v_erro.
  PERFORM seleciona_interface.

  IF v_erro IS INITIAL.
    PERFORM busca_file.
  ELSE.
    CLEAR: p_input,
           p_proc,
           p_log,
           p_chkso.
  ENDIF.

ENDFORM.                    " VALIDA_TELA_SELECAO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INTERFACE
*&---------------------------------------------------------------------*
FORM seleciona_interface .

  DATA: v_lifnr TYPE lfa1-lifnr.

  CLEAR: p_chkso,
         t_zlest0007[].

  SELECT SINGLE *
    FROM zlest0007
    INTO st_zlest0007
   WHERE id_interface = sy-repid
     AND id_ctg = c_ent
     AND prefix = c_l1
     AND vlde <= sy-datum
     AND vlate >= sy-datum.

  IF sy-subrc IS INITIAL.
    APPEND st_zlest0007 TO t_zlest0007.
  ELSE.
    MESSAGE w026 WITH c_ent 'ZLES0009' INTO v_mensagem.
    PERFORM envia_mensagem_procto  USING sy-repid
                                         c_e
                                         '999'
                                         v_mensagem.
    MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.
    v_erro = c_x.
    EXIT.
  ENDIF.

  IF v_erro IS INITIAL.
    SELECT SINGLE *
     FROM zlest0007
     INTO  st_zlest0007
    WHERE id_interface = sy-repid
      AND id_ctg = c_log
      AND vlde <= sy-datum
      AND vlate >= sy-datum.

    IF sy-subrc IS INITIAL.
      APPEND st_zlest0007 TO t_zlest0007.
    ELSE.
      MESSAGE w026 WITH c_log 'ZLES0009' INTO v_mensagem.
      PERFORM envia_mensagem_procto  USING sy-repid
                                           c_e
                                           '999'
                                           v_mensagem.
      MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.
      v_erro = c_x.
      EXIT.
    ENDIF.
  ENDIF.

  IF v_erro IS INITIAL.
    SELECT SINGLE *
      FROM zlest0007
      INTO  st_zlest0007
     WHERE id_interface = sy-repid
       AND id_ctg = c_proc
       AND vlde <= sy-datum
       AND vlate >= sy-datum.

    IF sy-subrc IS INITIAL.
      APPEND st_zlest0007 TO t_zlest0007.
    ELSE.
      MESSAGE w026 WITH c_proc 'ZLES0009' INTO v_mensagem.
      PERFORM envia_mensagem_procto  USING sy-repid
                                           c_e
                                           '999'
                                           v_mensagem.
      MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.
      v_erro = c_x.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
FORM preenche_caminho  USING    v_categ
                       CHANGING v_path.

  READ TABLE t_zlest0007 INTO st_zlest0007 WITH KEY id_ctg = v_categ.
  IF r_local = c_x.
    IF NOT st_zlest0007-pathwin IS INITIAL.
      v_path = st_zlest0007-pathwin.
      p_chkso = c_w.
    ENDIF.
  ELSE.
    IF NOT st_zlest0007-pathunix IS INITIAL.
      v_path = st_zlest0007-pathunix.
      p_chkso = c_u.
    ENDIF.
  ENDIF.

  CASE st_zlest0007-id_ctg.
    WHEN c_log.
      v_prefix_log = st_zlest0007-prefix.
    WHEN c_proc.
      v_prefix_proc = st_zlest0007-prefix.
    WHEN c_ent.
      v_prefix_ent = st_zlest0007-prefix.
  ENDCASE.

  CLEAR st_zlest0007.

ENDFORM.                    " PREENCHE_CAMINHO

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO
*&---------------------------------------------------------------------*
FORM le_diretorio .

  DATA: v_index         TYPE sy-tabix,
        v_mask_unix     TYPE epsfilnam,
        v_mask_locl(60) TYPE c,
        v_erro_log,
        wl_tabix        TYPE sy-tabix,
        vl_extencao(4),
        vl_cont         TYPE sy-tabix.

  CHECK: NOT p_input IS INITIAL
     AND NOT p_proc  IS INITIAL
     AND NOT p_log   IS INITIAL.

  CLEAR: v_erro_log, v_leave.   REFRESH t_0009_aux.

* Processa arquivos de origem UNIX
  IF r_unix = c_x.

    CONCATENATE v_prefix_ent c_mask_unix INTO v_mask_unix.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = p_input
        file_mask              = v_mask_unix
      TABLES
        dir_list               = t_dir_unix
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    DELETE t_dir_unix WHERE size = 0 OR name = ''.

    LOOP AT t_dir_unix INTO st_files_unix.
      wl_tabix = sy-tabix.
      vl_cont = strlen( st_files_unix-name ) - 4.
      vl_extencao  = st_files_unix-name+vl_cont(4).
      TRANSLATE vl_extencao TO UPPER CASE.

      IF vl_extencao NE '.TXT'.
        DELETE t_dir_unix INDEX wl_tabix.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0 OR t_dir_unix[] IS INITIAL.

      MESSAGE w000
      WITH 'Diretório Unix: ' p_input
           ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
           v_mask_unix
      INTO v_mensagem.

      PERFORM envia_mensagem_procto  USING sy-repid
                                             c_e
                                             '999'
                                             v_mensagem.

      MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.

      IF sy-tcode EQ 'ZLES0103'
      OR sy-batch IS NOT INITIAL.
        v_leave = c_x.
      ELSE.
        LEAVE LIST-PROCESSING.
      ENDIF.

    ELSE.
      REFRESH t_dir_unix2.
      LOOP AT t_dir_unix INTO st_files_unix.
        MOVE-CORRESPONDING st_files_unix TO st_files_unix2.
        st_files_unix2-nome = st_files_unix-name.
        APPEND st_files_unix2 TO t_dir_unix2.
      ENDLOOP.
*     Obtem controle informações para controle de Log
      SELECT *
      FROM zlest0008
      INTO TABLE t_zlest0008
      FOR ALL ENTRIES IN t_dir_unix2
      WHERE filename = t_dir_unix2-nome.

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.

      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

* Obtem arquivos bloqueados

      SELECT *
      FROM zlest0009
      INTO TABLE t_zlest0009
      FOR ALL ENTRIES IN t_dir_unix2
      WHERE filename = t_dir_unix2-nome.

*     Bloqueia arquivos para processamento
      LOOP AT t_dir_unix INTO st_files_unix.

        v_index = sy-tabix.

        PERFORM check_bloqueio_arquivo USING st_files_unix-name
                                    CHANGING v_erro.

        IF v_erro = c_x.

          DELETE t_dir_unix INDEX v_index.

          PERFORM transfere_file USING p_input
                                       p_log
                                       p_proc
                                       c_u
                                       st_files_unix-name
                                       c_x.

          v_erro_log = c_x.

        ENDIF.

      ENDLOOP.

*     Consiste arquivo apto para processamento
      LOOP AT t_dir_unix INTO st_files_unix.
        PERFORM carrega_arq USING st_files_unix-name c_u.
        IF NOT v_erro IS INITIAL.
          v_erro_log = c_x.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ELSEIF r_local = c_x.

    CONCATENATE v_prefix_ent c_mask_loc INTO v_mask_locl.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        directory  = p_input
        filter     = v_mask_locl
*     IMPORTING
*       FILE_COUNT =
*       DIR_COUNT  =
      TABLES
        file_table = t_dir_loc_f
        dir_table  = t_dir_local
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

    IF sy-subrc <> 0 OR t_dir_loc_f[] IS INITIAL.

      MESSAGE w000
         WITH 'Diretório Local: ' p_input
              ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
              v_mask_locl
          INTO v_mensagem.
      PERFORM envia_mensagem_procto  USING sy-repid
                                           c_e
                                           '999'
                                           v_mensagem.
      MESSAGE v_mensagem TYPE c_s DISPLAY LIKE c_e.

      LEAVE LIST-PROCESSING.

    ELSE.

*     Obtem controle informações para controle de Log
      SELECT *
      FROM zlest0008
      INTO TABLE t_zlest0008
      FOR ALL ENTRIES IN t_dir_loc_f
      WHERE filename = t_dir_loc_f-pathname.

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

*     Obtem arquivos bloqueados
      SELECT *
      FROM zlest0009
      INTO TABLE t_zlest0009
      FOR ALL ENTRIES IN t_dir_loc_f
      WHERE filename = t_dir_loc_f-pathname.

*     Bloqueia todos registro antes da consistência
      LOOP AT t_dir_loc_f INTO st_files_doc.

        v_index = sy-tabix.

        PERFORM check_bloqueio_arquivo USING st_files_doc-pathname CHANGING v_erro.

        IF v_erro = c_x.

          DELETE t_dir_loc_f INDEX v_index.

          PERFORM transfere_file USING p_input p_log p_proc c_l st_files_doc-pathname c_x.

          v_erro_log = c_x.

        ENDIF.

      ENDLOOP.

*     Consiste arquivo apto para processamento
      LOOP AT t_dir_loc_f INTO st_files_doc.

        PERFORM carrega_arq USING st_files_doc-pathname c_l.

        IF NOT v_erro IS INITIAL.
          v_erro_log = c_x.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

  IF v_leave IS INITIAL.
* Limpa controle de bloqueio que foi criado neste processo
    DELETE zlest0009 FROM TABLE t_0009_aux.

* Verifica se houve erro em algum processamento...
    IF v_erro_log IS INITIAL.
      MESSAGE s000(zles) WITH 'Arquivos processado!'
                         DISPLAY LIKE c_s.
    ELSE.
      MESSAGE s000(zles)
         WITH 'Existem arquivos/registros que não foram'
              'processados,'
              'verificar transaçãö LOG ZLES0010'
         DISPLAY LIKE c_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " LE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
FORM carrega_arq  USING    v_file
                           v_tipo.

  DATA:
    "v_caminho     TYPE string,
    lidx_aux1 TYPE i,
    lidx_aux2 TYPE i.

  CLEAR v_erro.

  REFRESH: t_file,
           t_idxerro,
           t_branch,
           t_nfdoc,
           t_reg30,
           t_zlest0019.

  CONCATENATE p_input v_file INTO v_caminho.

  PERFORM le_arquivo_unix_window USING v_tipo
                                       v_caminho.

  PERFORM transfer_arqlido_tabreg.

  PERFORM consiste_header_arquivo USING v_file
                                        v_tipo.

* Erro no header do arquivo
  IF st_reg10-erro IS INITIAL.

    PERFORM consiste_detalhe_arquivo USING v_file
                                           v_tipo.

    IF NOT t_msg[] IS INITIAL.
      "PERFORM transfere_file USING p_input p_log p_proc v_tipo v_file c_x.
      PERFORM transfere_file USING p_input p_log p_proc v_tipo v_file v_erro.
    ELSE.
      PERFORM transfere_file USING p_input p_log p_proc v_tipo v_file space.
    ENDIF.

  ELSE.
    PERFORM transfere_file USING p_input p_log p_proc v_tipo v_file c_x.
  ENDIF.

ENDFORM.                    " CARREGA_ARQ

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO_UNIX_WINDOW
*&---------------------------------------------------------------------*
FORM le_arquivo_unix_window  USING  p_tipo
                                    p_path.

  DATA: l_path TYPE string.
  l_path = p_path.

  REFRESH t_file.
  CLEAR   t_file.

*   Lê arquivo UNIX
  IF p_tipo = c_u.

*    OPEN DATASET v_caminho FOR INPUT IN BINARY MODE.
    OPEN DATASET v_caminho FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
    DO.
      READ DATASET p_path INTO t_file.
      IF sy-subrc  IS INITIAL.
        APPEND t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLOSE DATASET v_caminho.

*   Lê arquivo WINDOWS
  ELSEIF p_tipo = c_l.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = l_path
        filetype                = c_asc
      TABLES
        data_tab                = t_file
      EXCEPTIONS
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
        OTHERS                  = 17.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " LE_ARQUIVO_UNIX_WINDOW

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_ARQLIDO_TABREG
*&---------------------------------------------------------------------*
FORM transfer_arqlido_tabreg.

  DATA: st_file      TYPE y_file,
        l_char06     TYPE char6,
        l_char11     TYPE char11,
        vl_index     TYPE sy-index,
        vl_tabix     TYPE sy-tabix,
        vl_id_refkey TYPE zlest0019-id_zlest0019,
        vl_caminho   TYPE string.

  CLEAR:    st_reg10,
            st_reg20,
            st_reg30,
            st_reg40,
            st_reg50,
            st_file,
            vl_index,
            vl_tabix,
            v_mensagem,
            vl_caminho.

  REFRESH : t_reg20,
            t_reg30,
            t_reg40,
            t_zlest0035.

  vl_caminho = v_caminho.

  LOOP AT t_file.

    vl_tabix = sy-tabix.

* Tipo de registro 10
    IF t_file-linha(2) = '10'.

      vl_id_refkey = t_file-linha+102(10).

      IF vl_id_refkey IS INITIAL.
        PERFORM zgeranumero CHANGING vl_id_refkey.
      ENDIF.
      "condense vl_id_refkey no-gaps.

      CONCATENATE t_file-linha(102) vl_id_refkey INTO t_file-linha .

      PERFORM converte_char_date USING t_file-linha+87(10)
                                 CHANGING st_reg10-dataenv.

      st_reg10-tipo         = t_file-linha(2).
      st_reg10-movto        = t_file-linha+2(1).
      st_reg10-empresa      = t_file-linha+3(70).
      st_reg10-cnpj         = t_file-linha+73(14).
      l_char06              = t_file-linha+97(5).
      st_reg10-id_refkey    = vl_id_refkey.

      REPLACE ALL OCCURRENCES OF ':' IN l_char06 WITH space.
      CONDENSE l_char06 NO-GAPS.
      CONCATENATE l_char06 '00' INTO st_reg10-horaenv.
      CONDENSE st_reg10-horaenv NO-GAPS.

      CONCATENATE 'Nr Identificação: ' t_file-linha+102 INTO st_reg10-observ.

      st_reg10-tabix        = vl_tabix.

      st_file-linha = t_file-linha.

      MODIFY t_file[] FROM st_file INDEX 1.

      IF r_local IS NOT INITIAL.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = vl_caminho
          TABLES
            data_tab                = t_file[]
          EXCEPTIONS
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
            OTHERS                  = 22.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        DELETE DATASET vl_caminho.

        OPEN DATASET  vl_caminho FOR OUTPUT IN TEXT MODE    "smart: 11/01/10 E111
           ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
        IF sy-subrc IS INITIAL.
          LOOP AT t_file.
            TRANSFER t_file-linha TO vl_caminho.
          ENDLOOP.

          CLOSE DATASET vl_caminho.
        ENDIF.
      ENDIF.

***** Tipo de registro 20
    ELSEIF t_file-linha(2) = '20'.
      "ALRS
      SORT t_reg20 BY index DESCENDING.

      READ TABLE t_reg20 INTO st_reg20 INDEX 1.

      IF sy-subrc IS INITIAL.
        vl_index = vl_index + 1.
      ELSE.
        vl_index =  1.
      ENDIF.

      CLEAR: st_reg20.

      PERFORM converte_char_decimal USING    t_file-linha+23(14)
                                    CHANGING st_reg20-pesovagao.

      PERFORM converte_char_date USING    t_file-linha+37(10)
                                 CHANGING st_reg20-datadesc.


      st_reg20-tipo         = t_file-linha(2).
      st_reg20-idvagao      = t_file-linha+2(11).
      st_reg20-dcl          = t_file-linha+13(10).
      l_char06              = t_file-linha+47(5).

      REPLACE ALL OCCURRENCES OF ':' IN l_char06 WITH space.
      CONDENSE l_char06 NO-GAPS.
      CONCATENATE l_char06 '00' INTO st_reg20-horadesc.
      CONDENSE st_reg20-horadesc NO-GAPS.

      st_reg20-seriedcl     = t_file-linha+52(3).
      st_reg20-index        = vl_index.
      st_reg20-tabix        = vl_tabix.
      APPEND st_reg20 TO t_reg20.

      CLEAR st_reg20.

* Tipo de registro 30
    ELSEIF t_file-linha(2) = '30'.

      CLEAR: st_reg30.
      READ TABLE t_reg20 INTO st_reg20 INDEX 1.

      IF sy-subrc IS NOT INITIAL.
        vl_index = vl_index + 1.
      ENDIF.

      PERFORM converte_char_decimal USING    t_file-linha+13(14)
                                    CHANGING st_reg30-pesonota.

      PERFORM converte_char_decimal USING    t_file-linha+27(14)
                                    CHANGING st_reg30-pesochgd.

      PERFORM converte_char_date USING    t_file-linha+41(10)
                                 CHANGING st_reg30-datachgd.

      st_reg30-tipo         = t_file-linha(2).
      l_char11              = t_file-linha+2(11).
      st_reg30-nf           = l_char11.
      st_reg30-cnpj         = t_file-linha+51(14).
      st_reg30-compl        = t_file-linha+65(54).
      st_reg30-prod         = t_file-linha+119(6).
      st_reg30-index        = vl_index.
      st_reg30-tabix        = vl_tabix.
      APPEND st_reg30 TO t_reg30.

      CLEAR st_reg30.

* Tipo de registro 40
    ELSEIF t_file-linha(2) = '40'.

      CLEAR st_reg40.

      PERFORM converte_char_decimal USING t_file-linha+5(14)
                                    CHANGING st_reg40-totpeso30.

      REPLACE ALL OCCURRENCES OF REGEX '[^0-9,]' IN t_file-linha+2(3) WITH ''.
      st_reg40-tipo         = t_file-linha(2).
      st_reg40-totreg30     = t_file-linha+2(3).
      st_reg40-index        = vl_index.
      st_reg40-tabix        = vl_tabix.
      APPEND st_reg40 TO t_reg40.

      CLEAR st_reg40.

* Tipo de registro 50
    ELSEIF t_file-linha(2) = '50'.

      PERFORM converte_char_decimal USING t_file-linha+5(14)
                                    CHANGING st_reg50-totpeso30.

      st_reg50-tipo = t_file-linha(2).

      REPLACE ALL OCCURRENCES OF REGEX '[^0-9,]' IN t_file-linha+2(3) WITH ''.

      st_reg50-totreg30 = t_file-linha+2(3).

    ENDIF.

  ENDLOOP.

ENDFORM.                    " TRANSFER_ARQLIDO_TABREG

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_HEADER_ARQUIVO
*&---------------------------------------------------------------------*
FORM consiste_header_arquivo  USING p1_file
                                    p1_tipo.

  DATA:
    BEGIN OF lt_j_1bnfdoc OCCURS 0,
      nfnum  TYPE j_1bnfnumb,
      nfenum TYPE j_1bnfnum9,
      bukrs  TYPE bukrs,
      branch TYPE j_1bbranc_,
    END OF   lt_j_1bnfdoc,

    BEGIN OF lt_filial OCCURS 0,
      cnpj TYPE stcd1,
    END OF lt_filial.

  DATA: lc_valor1(15)   TYPE c,
        lc_valor2(15)   TYPE c,
        vl_nf           TYPE sy-index,
        vl_cont         TYPE sy-index,
        vl_cont1        TYPE sy-index,
        vl_atu_index    TYPE sy-index,
        vl_nfe          TYPE j_1bnfdoc-nfenum,
        vl_nfe_terc(11),
        vl_nfnum        TYPE j_1bnfdoc-nfnum,
        vl_seq          TYPE i,
        vl_nf_aux(11),
        vl_lifnr        TYPE lfa1-lifnr,
        vl_nfenum       TYPE j_1bnfdoc-nfenum,
        vl_emp          TYPE c LENGTH 4.



  DATA: tl_lfa1  TYPE TABLE OF lfa1,
        wl_lfa1  TYPE lfa1,
        tl_t001w TYPE TABLE OF t001w,
        wl_t001w TYPE t001w.

  DATA: tabix TYPE sy-tabix.

  DATA: it_zlest0041 TYPE TABLE OF zlest0041.

  CLEAR: v_erro,
         vl_cont,
         vl_cont1,
         v_mensagem.

  DATA: vl_tabix      TYPE sy-tabix.
  REFRESH: lt_filial, t_branch, lt_j_1bnfdoc, t_nfedoc, t_nfdoc, t_reg20a, t_reg30a, t_reg40a.

  t_reg20a[] = t_reg20[].
  t_reg30a[] = t_reg30[].
  t_reg40a[] = t_reg40[].
  CLEAR : vl_lifnr.
* Segmento 10

* Validação do CNPJ
  SELECT COUNT( * )
  FROM lfa1
  WHERE stcd1 = st_reg10-cnpj
  AND sperq NE 99.

  IF NOT sy-subrc IS INITIAL.
    v_msg = st_reg10-tabix.
    CONDENSE v_msg NO-GAPS.
    CONCATENATE TEXT-010 '10' TEXT-006 v_msg TEXT-005 st_reg10-cnpj '-' st_reg10-empresa INTO v_mensagem SEPARATED BY space.
    PERFORM controle_msg   USING 'E' '001' v_mensagem v_msg.
    st_reg10-erro = c_x.
  ENDIF.

* Segmento 20

* Validação do número e série do DCL
  LOOP AT t_reg20a INTO st_reg20.
    IF st_reg20-dcl IS INITIAL.
      v_msg = st_reg20-tabix.
      CONDENSE v_msg NO-GAPS.
      v_mensagem = st_reg20-tabix.
      CONDENSE v_mensagem NO-GAPS.
      CONCATENATE TEXT-010 '20' TEXT-006 v_msg TEXT-007 INTO v_mensagem SEPARATED BY space.
      PERFORM controle_msg   USING 'E' '002' v_mensagem v_msg.
      st_reg20-erro = c_x.
      MODIFY t_reg20 FROM st_reg20 TRANSPORTING erro WHERE index = st_reg20-index.
    ENDIF.
  ENDLOOP.

* Segmento 30

* Validação do CNPJ
  LOOP AT t_reg30a INTO st_reg30.
    SELECT SINGLE *
      FROM lfa1 INTO wl_lfa1
       WHERE stcd1 EQ st_reg30-cnpj
       AND   ktokk EQ 'ZFIC'
       AND sperq NE 99.
    IF sy-subrc = 0.
      lt_filial-cnpj = st_reg30-cnpj.
      APPEND lt_filial.
    ENDIF.
  ENDLOOP.

  SORT lt_filial.
  DELETE ADJACENT DUPLICATES FROM lt_filial.
  DESCRIBE TABLE lt_filial LINES vl_cont.

  IF NOT lt_filial[] IS INITIAL.
    SELECT * FROM lfa1 INTO TABLE tl_lfa1
      FOR ALL ENTRIES IN lt_filial
    WHERE stcd1 EQ lt_filial-cnpj.

    SELECT * FROM t001w INTO TABLE tl_t001w
      FOR ALL ENTRIES IN tl_lfa1
    WHERE werks EQ tl_lfa1-lifnr+6(4).

    IF ( sy-subrc EQ 0 ).

      SELECT bukrs branch stcd1
        FROM j_1bbranch
        INTO TABLE t_branch
         FOR ALL ENTRIES IN lt_filial
       WHERE stcd1  EQ lt_filial-cnpj
         AND branch NE '0001'.

      LOOP AT t_branch INTO st_branch.
        tabix = sy-tabix.
        READ TABLE tl_t001w INTO wl_t001w WITH KEY werks = st_branch-branch.
        IF ( sy-subrc EQ 0 ).
          CONTINUE.
        ELSE.
          DELETE t_branch INDEX tabix.
        ENDIF.
      ENDLOOP.

    ELSE.

      SELECT bukrs branch stcd1
        FROM j_1bbranch
        INTO TABLE t_branch
         FOR ALL ENTRIES IN lt_filial
       WHERE stcd1  EQ lt_filial-cnpj
         AND branch NE '0001'.

    ENDIF.

    IF sy-subrc IS INITIAL.
      DESCRIBE TABLE t_branch LINES vl_cont1.
      IF vl_cont <> vl_cont1.

        LOOP AT t_reg30a INTO st_reg30.

          READ TABLE t_branch INTO st_branch WITH KEY stcd1 = st_reg30-cnpj.

          IF NOT sy-subrc IS INITIAL.

            v_msg = st_reg30-tabix.
            CONDENSE v_msg NO-GAPS.
            v_mensagem = st_reg30-cnpj.
            CONDENSE v_mensagem NO-GAPS.
            CONCATENATE TEXT-010 '30' TEXT-006 v_msg TEXT-008 v_mensagem INTO v_mensagem SEPARATED BY space.
            PERFORM controle_msg   USING 'E' '003' v_mensagem v_msg.
            st_reg30-erro = c_x.
            MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE tabix = st_reg30-tabix AND erro NE 'T'.

          ELSE.

            IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).
              vl_nf               = st_reg30-nf+2(9).
              lt_j_1bnfdoc-nfnum  = vl_nf.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_reg30-nf+2(9)
                IMPORTING
                  output = lt_j_1bnfdoc-nfenum.
            ELSE.
              vl_nf               = st_reg30-nf.
              lt_j_1bnfdoc-nfnum  = vl_nf.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_reg30-nf
                IMPORTING
                  output = lt_j_1bnfdoc-nfenum.
            ENDIF.

            lt_j_1bnfdoc-bukrs  = st_branch-bukrs.
            lt_j_1bnfdoc-branch = st_branch-branch.
            APPEND lt_j_1bnfdoc.

          ENDIF.

        ENDLOOP.

      ELSE.

* Dados de nota fiscal
        LOOP AT t_reg30a INTO st_reg30.
          READ TABLE t_branch INTO st_branch WITH KEY stcd1 = st_reg30-cnpj.
          IF sy-subrc IS INITIAL.
            IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).
              vl_nf               = st_reg30-nf+2(9).
              lt_j_1bnfdoc-nfnum  = vl_nf.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_reg30-nf+2(9)
                IMPORTING
                  output = lt_j_1bnfdoc-nfenum.
            ELSE.

              vl_nf               = st_reg30-nf.
              lt_j_1bnfdoc-nfnum  = vl_nf.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = st_reg30-nf
                IMPORTING
                  output = lt_j_1bnfdoc-nfenum.
            ENDIF.

            lt_j_1bnfdoc-bukrs  = st_branch-bukrs.
            lt_j_1bnfdoc-branch = st_branch-branch.

            APPEND lt_j_1bnfdoc.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


  IF NOT lt_j_1bnfdoc[] IS INITIAL.

* Documento de nota fiscal eletrônica
    SELECT docnum nfenum nfnum series bukrs branch cgc cnpj_bupla
      INTO TABLE t_nfedoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN lt_j_1bnfdoc
     WHERE nfenum = lt_j_1bnfdoc-nfenum
       AND bukrs  = lt_j_1bnfdoc-bukrs
       AND branch = lt_j_1bnfdoc-branch
       AND model  IN ('55')
       AND form   NE space
       AND cancel <> c_x.

* Documento de nota fiscal
    SELECT docnum nfenum nfnum series bukrs branch cgc cnpj_bupla
      INTO TABLE t_nfdoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN lt_j_1bnfdoc
     WHERE nfnum  = lt_j_1bnfdoc-nfnum
       AND bukrs  = lt_j_1bnfdoc-bukrs
       AND branch = lt_j_1bnfdoc-branch
       AND model  IN ('01','04')
       AND form   NE space
       AND cancel <> c_x.

* NF de remessa por conta e ordem de terceiros

  ENDIF.

* Validação de nota fiscal
  LOOP AT t_reg30a INTO st_reg30.

    vl_tabix  = sy-tabix.
    IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_reg30-nf+2(9)
        IMPORTING
          output = vl_nfe.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_reg30-nf
        IMPORTING
          output = vl_nfe.


    ENDIF.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = ST_REG30-NF
*      IMPORTING
*        OUTPUT = VL_NFE.

    CONDENSE vl_nfe NO-GAPS.

    READ TABLE t_nfedoc INTO st_nfdoc WITH KEY nfenum = vl_nfe.

    IF st_nfdoc-cnpj_bupla IS NOT INITIAL AND st_nfdoc-cnpj_bupla NE st_reg30-cnpj AND sy-subrc IS INITIAL.
      sy-subrc = 1.
    ENDIF.

    IF NOT sy-subrc IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = st_reg30-nf
        IMPORTING
          output = vl_nf_aux.

      IF strlen( vl_nf_aux ) <= 6.

        IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = st_reg30-nf+2(9)
            IMPORTING
              output = vl_nfnum.

        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = st_reg30-nf
            IMPORTING
              output = vl_nfnum.

        ENDIF.

        CONDENSE vl_nfnum NO-GAPS.

        READ TABLE t_nfdoc INTO st_nfdoc  WITH KEY nfenum = vl_nfnum.

        IF st_nfdoc-cnpj_bupla IS NOT INITIAL AND st_nfdoc-cnpj_bupla NE st_reg30-cnpj AND sy-subrc IS INITIAL.
          sy-subrc = 1.
        ENDIF.

      ELSE.
        sy-subrc = 1.
      ENDIF.

      IF NOT sy-subrc IS INITIAL .
        "Tabela de nf de remessa por conta e ordem de terceiros

        DATA(lc_cpf_cnpj) = st_reg30-cnpj.
        CONDENSE lc_cpf_cnpj.
        DATA(lc_qtd) = strlen( lc_cpf_cnpj ).

        IF lc_qtd EQ 11.
          SELECT lifnr INTO TABLE @DATA(it_lifnr)
            FROM lfa1
           WHERE stcd2 EQ @st_reg30-cnpj
             AND sperq NE 99.
        ELSE.
          SELECT SINGLE lifnr
            FROM lfa1
            INTO vl_lifnr
           WHERE stcd1 EQ st_reg30-cnpj
             AND sperq NE 99.
        ENDIF.

        "ALRS
        IF NOT sy-subrc IS INITIAL.
          v_msg = st_reg30-tabix.
          CONDENSE v_msg NO-GAPS.
          v_mensagem = st_reg30-nf.
          CONDENSE v_mensagem NO-GAPS.
          CONCATENATE TEXT-010 '30' TEXT-006 v_msg TEXT-016 v_mensagem  TEXT-017 INTO v_mensagem SEPARATED BY space.
          PERFORM controle_msg   USING 'E' '003' v_mensagem '999'.
          st_reg30-erro = c_x.
          MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE tabix = st_reg30-tabix AND erro NE 'T'.
          CONTINUE.
        ENDIF.

        CLEAR: it_zlest0041[].

        IF lc_qtd EQ 11.

          SELECT *"nr_nf serie centro_comprador
            INTO TABLE it_zlest0041
            FROM zlest0041
             FOR ALL ENTRIES IN it_lifnr
           WHERE nr_nf       EQ vl_nfe
             AND cod_cliente EQ it_lifnr-lifnr.

        ELSE.

          SELECT *"nr_nf serie centro_comprador
            INTO TABLE it_zlest0041
            FROM zlest0041
           WHERE nr_nf       = vl_nfe
             AND cod_cliente = vl_lifnr.

        ENDIF.

        "READ TABLE t_zlest0041 INTO st_zlest0041 WITH KEY nr_nf = vl_nfe.

        IF it_zlest0041[] IS INITIAL.
*          V_MSG = ST_REG30-TABIX.
*          CONDENSE V_MSG NO-GAPS.
*          V_MENSAGEM = ST_REG30-NF.
*          CONDENSE V_MENSAGEM NO-GAPS.
*          CONCATENATE TEXT-010 '30' TEXT-006 V_MSG TEXT-016 V_MENSAGEM  TEXT-015 INTO V_MENSAGEM SEPARATED BY SPACE.
*          PERFORM CONTROLE_MSG   USING 'E' '004' V_MENSAGEM V_MSG.
          st_reg30-erro = 'T'. "C_X.
*          MODIFY T_REG30 FROM ST_REG30 TRANSPORTING ERRO WHERE TABIX = ST_REG30-TABIX.
          MODIFY t_reg30 FROM st_reg30 INDEX vl_tabix TRANSPORTING erro.
        ELSE.
          SORT it_zlest0041 BY data DESCENDING.
          READ TABLE it_zlest0041 INDEX 1 INTO st_zlest0041.

          vl_nfe_terc             = st_reg30-nf.

          st_reg30-nf             = st_zlest0041-nr_nf_propria.
          st_reg30-docnum         = st_zlest0041-docnum.
          st_reg30-nr_nf_terceiro = st_zlest0041-nr_nf.
          st_reg30-cod_fornecedor = st_zlest0041-cod_cliente.

          CONCATENATE '00' st_zlest0041-centro_comprador(2) INTO vl_emp.

          st_reg30-bukrs  = vl_emp.
          st_reg30-branch = st_zlest0041-centro_comprador.

          MODIFY t_reg30 FROM st_reg30 TRANSPORTING nf docnum bukrs branch nr_nf_terceiro cod_fornecedor WHERE nf = vl_nfe_terc AND tipo = st_reg30-tipo AND cnpj = st_reg30-cnpj.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

  "Valida Todos os CNPJ
  LOOP AT t_reg30a INTO st_reg30_2.

    lc_cpf_cnpj = st_reg30_2-cnpj.
    CONDENSE lc_cpf_cnpj.
    lc_qtd = strlen( lc_cpf_cnpj ).

    IF lc_qtd EQ 11.
      SELECT * INTO TABLE @DATA(it_lfa1)
        FROM lfa1
       WHERE stcd2 EQ @st_reg30_2-cnpj
         AND sperq NE 99.

      IF sy-subrc IS INITIAL.
        READ TABLE it_lfa1 INDEX 1 INTO wl_lfa1.
      ENDIF.
    ELSE.
      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
       WHERE stcd1 EQ st_reg30_2-cnpj
       AND   sperq NE '99'.
    ENDIF.

    IF NOT sy-subrc IS INITIAL.
      CONCATENATE TEXT-010 '30' TEXT-011 TEXT-008 INTO v_mensagem SEPARATED BY space.
      PERFORM controle_msg   USING 'E' '003' v_mensagem '999'.

      "Erro somente na linha (ALRS - 24/07/2014)
      st_reg30_2-erro = c_x.
      MODIFY t_reg30 FROM st_reg30_2 TRANSPORTING erro WHERE tabix = st_reg30_2-tabix.

*      LOOP AT T_REG30A INTO ST_REG30.
*        ST_REG30-ERRO = C_X.
*        MODIFY T_REG30 FROM ST_REG30 TRANSPORTING ERRO WHERE TIPO = '30'. " AND ERRO NE 'T'.
*      ENDLOOP.
*      EXIT.
*
    ELSEIF wl_lfa1-scacd = '9999' AND wl_lfa1-ktokk NE 'ZFIC'. "fornecedor é emissor de NF eletrônica
      IF ( st_reg30_2-nf(1) EQ 0 ) AND ( st_reg30_2-nf+1(1) NE 0 ).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30_2-nf+2(9)
          IMPORTING
            output = vl_nfenum.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30_2-nf
          IMPORTING
            output = vl_nfenum.
      ENDIF.
      CONDENSE vl_nfenum NO-GAPS.

      CLEAR st_zib_nfe_forn.
      SELECT SINGLE *
          FROM zib_nfe_forn
          INTO st_zib_nfe_forn
          WHERE nu_chave_cnpj   = st_reg30_2-cnpj
          AND   nu_chave_numero  = vl_nfenum.

      IF sy-subrc NE 0.
        v_msg = st_reg30-tabix.
        CONDENSE v_msg NO-GAPS.
        v_mensagem = st_reg30_2-nf.
        CONDENSE v_mensagem NO-GAPS.
        CONCATENATE TEXT-010 '30' TEXT-006 v_msg TEXT-016 v_mensagem  TEXT-018 INTO v_mensagem SEPARATED BY space.
        PERFORM controle_msg   USING 'E' '004' v_mensagem '999'.
        st_reg30_2-erro = c_x.
        MODIFY t_reg30 FROM st_reg30_2 TRANSPORTING erro WHERE tabix = st_reg30_2-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Segmento 40 Totalizadores

* Validação numero de registros e peso vagão
  CLEAR:  st_reg40_check-totreg30, st_reg40_check-totpeso30, vl_cont.

  vl_cont1 = 1.

  SORT t_reg30a BY index.

  LOOP AT t_reg30a INTO st_reg30.

    vl_cont = vl_cont + 1.

    ON CHANGE OF st_reg30-index.

      IF sy-tabix <> 1.
        READ TABLE t_reg40a INTO st_reg40 WITH KEY index = vl_cont1.
        IF sy-subrc IS INITIAL.
          IF st_reg40-totreg30 <> st_reg40_check-totreg30.
            v_msg = st_reg40-tabix.
            CONDENSE v_msg NO-GAPS.
            v_mensagem = vl_cont1.
            CONDENSE v_mensagem NO-GAPS.
            CONCATENATE TEXT-010 '40' TEXT-006 v_msg TEXT-012 INTO v_mensagem SEPARATED BY space.
            PERFORM controle_msg   USING 'E' '005' v_mensagem v_msg.
            st_reg30-erro = c_x.
            vl_atu_index = st_reg30-index - 1.
            MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE index = vl_atu_index.
          ENDIF.
          IF st_reg40-totpeso30 <> st_reg40_check-totpeso30.
            v_msg = st_reg40-tabix.
            CONDENSE v_msg NO-GAPS.
            v_mensagem = vl_cont1.
            CONDENSE v_mensagem NO-GAPS.
            CONCATENATE TEXT-010 '40' TEXT-006 v_msg TEXT-013 INTO v_mensagem SEPARATED BY space.
            PERFORM controle_msg   USING 'E' '005' v_mensagem v_msg.
            st_reg30-erro = c_x.
            vl_atu_index = st_reg30-index - 1.
            MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE index = vl_atu_index.
          ENDIF.
        ENDIF.
        CLEAR:  st_reg40_check-totreg30, st_reg40_check-totpeso30.
        vl_cont = 1.
      ENDIF.
      vl_cont1 = st_reg30-index.
    ENDON.

    st_reg40_check-totreg30  = vl_cont.
    st_reg40_check-totpeso30 = st_reg40_check-totpeso30 + st_reg30-pesochgd.

    AT LAST.
      READ TABLE t_reg40a INTO st_reg40 WITH KEY index = vl_cont1.
      IF sy-subrc IS INITIAL.
        IF st_reg40-totreg30 <> st_reg40_check-totreg30.
          v_msg = st_reg40-tabix.
          CONDENSE v_msg NO-GAPS.
          v_mensagem = vl_cont1.
          CONDENSE v_mensagem NO-GAPS.
          CONCATENATE TEXT-010 '40' TEXT-006 v_msg TEXT-012 INTO v_mensagem SEPARATED BY space.
          PERFORM controle_msg   USING 'E' '005' v_mensagem v_msg.
          st_reg30-erro = c_x.
          MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE index = vl_cont1.
        ENDIF.
        IF st_reg40-totpeso30 <> st_reg40_check-totpeso30.
          v_msg = st_reg40-tabix.
          CONDENSE v_msg NO-GAPS.
          v_mensagem = vl_cont1.
          CONDENSE v_mensagem NO-GAPS.
          CONCATENATE TEXT-010 '40' TEXT-006 v_msg TEXT-013 INTO v_mensagem SEPARATED BY space.
          PERFORM controle_msg   USING 'E' '005' v_mensagem v_msg.
          st_reg30-erro = c_x.
          MODIFY t_reg30 FROM st_reg30 TRANSPORTING erro WHERE index = vl_cont1.
        ENDIF.
      ENDIF.
      CLEAR:  st_reg40_check-totreg30, st_reg40_check-totpeso30, vl_cont.
    ENDAT.

  ENDLOOP.


* Registra erro e transfere o arquivo para diretório de log
  CHECK: v_erro = c_x.

  LOOP AT t_msg INTO st_msg.
    PERFORM envia_mensagem_procto USING p1_file
                                        'E'
                                        st_msg-msg_num
                                        st_msg-texto.
  ENDLOOP.

ENDFORM.                    " CONSISTE_HEADER_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_DETALHE_ARQUIVO
*&---------------------------------------------------------------------*
FORM consiste_detalhe_arquivo  USING p1_file
                                     p1_tipo.

  DATA: vl_tabix     TYPE sy-tabix,
        vl_name1     TYPE lfa1-name1,
        vl_stcd1     TYPE stcd1,
        vl_stcd2     TYPE stcd2,
        vl_kunnr     TYPE kna1-kunnr,
        vl_bukrs     TYPE j_1bbranch-bukrs,
        vl_branch    TYPE j_1bbranch-branch,
        vl_nfnum     TYPE j_1bnfdoc-nfnum,
        vl_nfenum    TYPE j_1bnfdoc-nfenum,
        vl_nfnuma    TYPE j_1bnfdoc-nfnum,
        vl_nfenuma   TYPE j_1bnfdoc-nfenum,
        vl_ctrl      TYPE c,
        vl_ctrl87    TYPE c,
        sl_zlest0035 TYPE zlest0035,
        vl_docnum    TYPE j_1bnfdoc-docnum,
        vl_nfnum_a   TYPE char6,
        vl_saida     TYPE char9,
        vl_id_refkey TYPE zlest0019-id_zlest0019,
        vl_cnpjferro TYPE zlest0019-cnpjferro,
        vl_serie     TYPE zlest0035-serie_nf,
        vl_ocorr19   TYPE sy-tabix,
        vl_ocorr87   TYPE sy-tabix,
        ls_j_1bnfdoc TYPE j_1bnfdoc.

  DATA: lw_zlest0039 TYPE zlest0039.

  REFRESH: t_zlest0019,t_zlest0087, t_idxerro.
  CLEAR:   t_zlest0019, vl_tabix, vl_name1, vl_bukrs,vl_branch,vl_nfnum,vl_nfenum, vl_ctrl, vl_ctrl87, vl_id_refkey, vl_cnpjferro,vl_ocorr19,vl_ocorr87.

* Conferencia segmento 20
  LOOP AT t_reg20 INTO st_reg20 WHERE erro = c_x.

* Elimina os registros com log de erro
    DELETE t_reg30 WHERE index = st_reg20-index.
    DELETE t_reg40 WHERE index = st_reg20-index.

  ENDLOOP.

* Conferencia segmento 30
  DELETE t_reg30 WHERE erro = c_x.

* Conferencia segmento 40
  LOOP AT t_reg30 INTO st_reg30 WHERE erro = c_x.
* Elimina os registros com log de erro
    DELETE t_reg20 WHERE index = st_reg30-index.
    DELETE t_reg40 WHERE index = st_reg30-index.
  ENDLOOP.

  SORT: t_reg20 BY index,
        t_reg30 BY index.

  LOOP AT t_reg30 INTO st_reg30.

    vl_tabix = sy-tabix.
    " ON CHANGE OF ST_REG30-INDEX.
    IF st_reg30-erro = 'T'."Terceiros com erro
      IF vl_ocorr87 = 0.
        vl_ocorr87 = 1.
      ELSE.
        vl_ocorr87 = 2.
      ENDIF.
      IF vl_ocorr87 = 1.
        "REG10
        st_zlest0087-idinter          = c_l1.
        st_zlest0087-tp_movi          = c_e.
        st_zlest0087-tp_reg           = st_reg10-tipo.
        st_zlest0087-idinter          = 'L1'.
        st_zlest0087-cnpj_prest_serv  = st_reg10-cnpj.
        st_zlest0087-empresa          = st_reg10-empresa.
        st_zlest0087-dtaenvio         = st_reg10-dataenv.
        st_zlest0087-horaenvio        = st_reg10-horaenv.
        st_zlest0087-id_refkey        = st_reg10-id_refkey.
        MOVE:  sy-datum                    TO st_zlest0087-erdat,
               sy-uzeit                    TO st_zlest0087-erzet,
               sy-uname                    TO st_zlest0087-uname.

        vl_id_refkey = st_reg10-id_refkey.
        SHIFT vl_id_refkey LEFT DELETING LEADING '0'.
        CONCATENATE sy-repid vl_id_refkey st_reg10-empresa INTO st_zlest0087-chave SEPARATED BY space.
        CONDENSE    st_zlest0087-chave NO-GAPS.

        vl_id_refkey = st_reg10-id_refkey.
        APPEND st_zlest0087 TO t_zlest0087.
        CLEAR st_zlest0087.
      ENDIF.

      "REG 30
      CLEAR: vl_bukrs, vl_branch, vl_nfenum, vl_nfnum, vl_nfenuma, vl_nfnuma.
      st_zlest0087-idinter            = c_l1.
      st_zlest0087-tp_movi            = c_e.
      st_zlest0087-tp_reg             = st_reg30-tipo.

      IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30-nf+2(9)
          IMPORTING
            output = vl_nfenum.

      ELSE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30-nf
          IMPORTING
            output = vl_nfenum.


      ENDIF.

      CONDENSE vl_nfenum NO-GAPS.

      IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30-nf+2(9)
          IMPORTING
            output = vl_nfenuma.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = st_reg30-nf
          IMPORTING
            output = vl_nfenuma.
      ENDIF.

      CONDENSE vl_nfenuma NO-GAPS.

      CLEAR st_zib_nfe_forn.
      SELECT SINGLE *
          FROM zib_nfe_forn
          INTO st_zib_nfe_forn
          WHERE nu_chave_cnpj   = st_reg30-cnpj
          AND   nu_chave_numero  = vl_nfenum.

      IF sy-subrc = 0.
        st_zlest0087-bukrs            = st_zib_nfe_forn-bukrs.
        st_zlest0087-werks            = st_zib_nfe_forn-branch.
        st_zlest0087-nr_nf            = st_zib_nfe_forn-nu_chave_numero.
        st_zlest0087-serie            = st_zib_nfe_forn-nu_chave_serie.
        st_zlest0087-nfe              = 'X'.
      ELSE.
        st_zlest0087-nr_nf            = vl_nfenuma.
        st_zlest0087-serie            = ''.
        st_zlest0087-nfe              = ''.
      ENDIF.

      SELECT SINGLE *
          FROM lfa1
          INTO st_lfa1
          WHERE stcd1  = st_reg30-cnpj
          AND   sperq  NE 99.

      IF sy-subrc = 0.
        st_zlest0087-lifnr            = st_lfa1-lifnr.
      ENDIF.

      st_zlest0087-peso_nf            = st_reg30-pesonota.
      st_zlest0087-descarga_rodo      = st_reg30-pesochgd.
      st_zlest0087-dt_descarga        = st_reg30-datachgd.
      st_zlest0087-id_refkey          = st_reg10-id_refkey.

      READ TABLE t_reg20 INTO st_reg20 WITH KEY index = st_reg30-index.
      IF sy-subrc IS INITIAL.
        MOVE st_reg20-datadesc           TO st_zlest0087-dt_descarga.
      ENDIF.

      SELECT SINGLE *
             FROM zlest0089
             INTO st_zlest0089
             WHERE produto  = st_reg30-prod
             AND   bukrs   = st_zib_nfe_forn-bukrs.
      IF sy-subrc = 0.
        st_zlest0087-produto          = st_zlest0089-matnr.
      ENDIF.

      READ TABLE t_frete WITH KEY from = st_reg30-cnpj.
      IF sy-subrc = 0.
        st_zlest0087-id_frete = 'T'.
      ELSE.
        st_zlest0087-id_frete = 'D'.
      ENDIF.
      " 07/07/2014 ALRS
      st_zlest0087-cnpj_prest_serv  = st_reg10-cnpj.
      st_zlest0087-empresa          = st_reg10-empresa.
      MOVE: sy-datum                    TO st_zlest0087-erdat,
            sy-uzeit                    TO st_zlest0087-erzet,
            sy-uname                    TO st_zlest0087-uname.

      "CHAVE
      CONCATENATE st_zlest0087-bukrs '-' st_zlest0087-werks '-' vl_nfenum vl_nfnum INTO st_zlest0087-chave SEPARATED BY space.
      CONDENSE st_zlest0087-chave NO-GAPS.

      APPEND st_zlest0087 TO t_zlest0087.
      CLEAR st_zlest0087.
      vl_ctrl87 = c_x.
      CONTINUE.
    ENDIF.

    IF vl_ocorr19 = 0.
      vl_ocorr19 = 1.
    ELSE.
      vl_ocorr19 = 2.
    ENDIF.

    IF vl_ocorr19 = 1.

      SELECT SINGLE name1 stcd1 stcd2
        INTO (vl_name1, vl_stcd1, vl_stcd2)
        FROM lfa1
       WHERE stcd1 = st_reg10-cnpj
       AND sperq NE 99.

      CLEAR st_zlest0019.

      vl_id_refkey = st_reg10-id_refkey.

      MOVE:
      sy-mandt                    TO st_zlest0019-mandt,
      c_l1                        TO st_zlest0019-idinter,
      c_e                         TO st_zlest0019-tp_movi,
      st_reg10-tipo               TO st_zlest0019-tp_reg,
      vl_name1                    TO st_zlest0019-nomempferro,
      st_reg10-dataenv            TO st_zlest0019-dtaenvio,
      st_reg10-horaenv            TO st_zlest0019-horaenvio,
      st_reg10-observ             TO st_zlest0019-obs,
      sy-datum                    TO st_zlest0019-erdat,
      sy-uzeit                    TO st_zlest0019-erzet,
      sy-uname                    TO st_zlest0019-uname,
      vl_id_refkey                TO st_zlest0019-id_zlest0019.

      IF NOT vl_stcd1 IS INITIAL.
        MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
      ELSEIF NOT vl_stcd2 IS INITIAL.
        MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
      ELSE.
        MOVE st_reg10-empresa     TO st_zlest0019-cnpjferro.
      ENDIF.

      vl_cnpjferro = st_zlest0019-cnpjferro.

      SHIFT vl_id_refkey LEFT DELETING LEADING '0'.

      CONCATENATE sy-repid vl_id_refkey st_reg10-empresa  INTO st_zlest0019-chave SEPARATED BY space.
      CONDENSE    st_zlest0019-chave NO-GAPS.

      vl_id_refkey = st_zlest0019-id_zlest0019.

      APPEND st_zlest0019 TO t_zlest0019.

    ENDIF.

    " ENDON.

    CLEAR: vl_bukrs, vl_branch, vl_nfenum, vl_nfnum, vl_nfenuma, vl_nfnuma.

    CONDENSE st_reg30-cnpj NO-GAPS.

    SELECT SINGLE bukrs branch
      INTO (vl_bukrs, vl_branch)
      FROM j_1bbranch
    WHERE stcd1 = st_reg30-cnpj
      AND branch NE '0001' .

    IF vl_bukrs IS INITIAL.
      vl_bukrs  = st_reg30-bukrs .
      vl_branch = st_reg30-branch.
    ENDIF.

    IF ( st_reg30-nf(1) EQ 0 ) AND ( st_reg30-nf+1(1) NE 0 ).

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = ST_REG30-NF+2(9)
*        IMPORTING
*          OUTPUT = VL_NFNUMA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_reg30-nf+2(9)
        IMPORTING
          output = vl_nfenuma.

    ELSE.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = ST_REG30-NF
*        IMPORTING
*          OUTPUT = VL_NFNUMA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = st_reg30-nf
        IMPORTING
          output = vl_nfenuma.

    ENDIF.
    IF st_reg30-docnum IS INITIAL .
*   Documento de nota fiscal eletrônica
      SELECT SINGLE docnum nfenum
        INTO (vl_docnum, vl_nfenum)
        FROM j_1bnfdoc
      WHERE nfenum = vl_nfenuma
        AND bukrs  = vl_bukrs
        AND branch = vl_branch
        AND direct = 2
        AND  cancel <> c_x
        AND nftype IN ('ZA', 'ZT').
    ELSE.
      vl_docnum = st_reg30-docnum.
      vl_nfenum = vl_nfenuma.
    ENDIF.

*Buscar apenas nfe
**   Documento de nota fiscal
*    SELECT SINGLE docnum nfnum
*      INTO (vl_docnum, vl_nfnum)
*      FROM j_1bnfdoc
*    WHERE  nfnum  = vl_nfnuma
*      AND  bukrs  = vl_bukrs
*      AND  branch = vl_branch
*      AND  cancel <> c_x.

    CLEAR st_zlest0019.

*    READ TABLE T_REG20 INTO ST_REG20 WITH KEY INDEX = ST_REG30-INDEX.
*
*    IF SY-SUBRC IS INITIAL.
*      MOVE:
*        ST_REG20-DCL                TO ST_ZLEST0019-DCL,
*        ST_REG20-SERIEDCL           TO ST_ZLEST0019-SERIEDCL.
*    ENDIF.

    PERFORM zgeranumero CHANGING st_zlest0019-id_zlest0019.

    MOVE:
    sy-mandt                    TO st_zlest0019-mandt,
    c_l1                        TO st_zlest0019-idinter,
    c_e                         TO st_zlest0019-tp_movi,
    st_reg30-tipo               TO st_zlest0019-tp_reg,
    st_reg30-cnpj               TO st_zlest0019-cnpjcliente,
    vl_bukrs                    TO st_zlest0019-bukrs,
    vl_branch                   TO st_zlest0019-branch,
    vl_nfenum                   TO st_zlest0019-nfenum,
    vl_nfnum                    TO st_zlest0019-nfnum,
    st_reg30-pesonota           TO st_zlest0019-pesonf,
    st_reg30-pesochgd           TO st_zlest0019-pesodvagao,
    st_reg30-datachgd           TO st_zlest0019-dtachegada,
    st_reg30-prod               TO st_zlest0019-produto,
    sy-datum                    TO st_zlest0019-erdat,
    sy-uzeit                    TO st_zlest0019-erzet,
    sy-uname                    TO st_zlest0019-uname,
    st_reg30-nr_nf_terceiro     TO st_zlest0019-nr_nf_terceiro,
    st_reg30-cod_fornecedor     TO st_zlest0019-cod_fornecedor,
    vl_id_refkey                TO st_zlest0019-id_refkey.

    st_zlest0019-cnpjferro = vl_cnpjferro.

    "07/07/14 ALRS
    SELECT SINGLE name1 stcd1 stcd2
      INTO (vl_name1, vl_stcd1, vl_stcd2)
      FROM lfa1
     WHERE stcd1 = st_reg10-cnpj
     AND sperq NE 99.

    IF NOT vl_stcd1 IS INITIAL.
      MOVE vl_stcd1             TO st_zlest0019-cnpjferro.
    ELSEIF NOT vl_stcd2 IS INITIAL.
      MOVE vl_stcd2             TO st_zlest0019-cnpjferro.
    ELSE.
      MOVE st_reg10-empresa     TO st_zlest0019-cnpjferro.
    ENDIF.
    MOVE vl_name1                    TO st_zlest0019-nomempferro.

    READ TABLE t_reg20 INTO st_reg20 WITH KEY index = st_reg30-index.
    IF sy-subrc IS INITIAL.
      MOVE st_reg20-datadesc           TO st_zlest0019-dtachegada.
    ENDIF.

    CONCATENATE vl_bukrs '-' vl_branch '-' vl_nfenum vl_nfnum INTO st_zlest0019-chave SEPARATED BY space.
    CONDENSE st_zlest0019-chave NO-GAPS.


    APPEND st_zlest0019 TO t_zlest0019.

*    "L1
*    UPDATE zlest0039 SET datatransb      = ''
*                         pesotransb      = ''
*                         unidadetransb   = ''
*                         "L2
*                         dataterminal    = ''
*                         pesoterminal    = ''
*                         unidadeterminal = ''
*                         "L3
*                         datachegada     = ''
*                         pesochegada     = ''
*                         unidadechegada  = ''
*                         status          = 'ET'
*    WHERE nfenum EQ st_zlest0019-nfenum
*      AND cnpj EQ st_zlest0019-cnpjcliente.


    "Ajuste para ZLEST0039
    SELECT SINGLE * FROM zlest0039 INTO lw_zlest0039 WHERE docnum EQ vl_docnum.
    IF ( sy-subrc EQ 0 ).

      IF NOT ( lw_zlest0039-pontotransb IS INITIAL ).
        CLEAR vl_kunnr.
        SELECT SINGLE kunnr
         INTO (vl_kunnr)
         FROM kna1
        WHERE stcd1 = st_reg10-cnpj.

        DATA(lw_zlest0039_ajuste) = lw_zlest0039.
        lw_zlest0039_ajuste-tp_transgenia    = st_reg30-tp_transgenia.
        lw_zlest0039_ajuste-datatransb       = st_reg30-datachgd.
        lw_zlest0039_ajuste-pesotransb       = st_reg30-pesochgd.
        lw_zlest0039_ajuste-unidadetransb    = lw_zlest0039-unidadesaida.
        lw_zlest0039_ajuste-status           = 'L1'.
        lw_zlest0039_ajuste-transb_efetivo   = vl_kunnr.
        lw_zlest0039_ajuste-dataterminal     = ''.
        lw_zlest0039_ajuste-pesoterminal     = ''.
        lw_zlest0039_ajuste-unidadeterminal  = ''.
        lw_zlest0039_ajuste-datachegada      = ''.
        lw_zlest0039_ajuste-pesochegada      = ''.
        lw_zlest0039_ajuste-unidadechegada   = ''.
        lw_zlest0039_ajuste-tp_importacao_l1 = 'A'.
        PERFORM verificar_carguero USING lw_zlest0039 lw_zlest0039_ajuste.
        CLEAR: lw_zlest0039_ajuste.

        "L1
        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = vl_docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        UPDATE zlest0039 SET datatransb       = st_reg30-datachgd
                            pesotransb       = st_reg30-pesochgd
                            unidadetransb    = lw_zlest0039-unidadesaida
                            status           = 'L1'
                            transb_efetivo   = vl_kunnr
                             "L2
                            dataterminal     = ''
                            pesoterminal     = ''
                            unidadeterminal  = ''
                            "L3
                            datachegada      = ''
                            pesochegada      = ''
                            unidadechegada   = ''
                            tp_importacao_l1 = 'A'
                            tp_transgenia    = st_reg30-tp_transgenia


            WHERE docnum EQ vl_docnum.

        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = vl_docnum.


      ELSEIF ( lw_zlest0039-pontotransb IS INITIAL ).

        MOVE-CORRESPONDING lw_zlest0039 TO lw_zlest0039_ajuste.
        lw_zlest0039_ajuste-tp_transgenia    = st_reg30-tp_transgenia.
        lw_zlest0039_ajuste-datachegada      = st_reg30-datachgd.
        lw_zlest0039_ajuste-pesochegada      = st_reg30-pesochgd.
        lw_zlest0039_ajuste-status           = 'L1'.
        lw_zlest0039_ajuste-transb_efetivo   = vl_kunnr.
        lw_zlest0039_ajuste-tp_importacao_l1 = 'A'.
        PERFORM verificar_carguero USING lw_zlest0039 lw_zlest0039_ajuste.
        CLEAR: lw_zlest0039_ajuste.

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = vl_docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.


        UPDATE zlest0039 SET datachegada      = st_reg30-datachgd
                             pesochegada      = st_reg30-pesochgd
                             unidadechegada   = lw_zlest0039-unidadesaida
                             status          = 'L1'
                             transb_efetivo  = vl_kunnr
                             tp_importacao_l1 = 'A'
                             tp_transgenia    = st_reg30-tp_transgenia
             WHERE docnum EQ vl_docnum.

        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = vl_docnum.


      ENDIF.
    ENDIF.

    vl_ctrl = c_x.


*   ZLEST0035
    CLEAR: sl_zlest0035,
           vl_saida    ,
           vl_nfnum_a  .
    IF NOT st_zlest0019-nfenum IS INITIAL.
      vl_saida = st_zlest0019-nfenum.
    ELSEIF st_zlest0019-nfnum IS NOT INITIAL.
      vl_nfnum_a = st_zlest0019-nfnum.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_nfnum_a
          output = vl_saida.
    ENDIF.

    sl_zlest0035-nr_nf      = vl_saida.
*
*    IF ( ST_REG30-NF(1) EQ 0 ) AND ( ST_REG30-NF+1(1) NE 0 ).
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = ST_REG30-NF+1(1)
*        IMPORTING
*          OUTPUT = VL_SERIE.
*
*      SL_ZLEST0035-SERIE_NF   = VL_SERIE.
*
*    ELSE.
*      SL_ZLEST0035-SERIE_NF   = ST_REG30-NF.
*    ENDIF.
    CLEAR: ls_j_1bnfdoc.
    SELECT SINGLE * FROM j_1bnfdoc INTO ls_j_1bnfdoc WHERE docnum EQ vl_docnum.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_j_1bnfdoc-series
      IMPORTING
        output = sl_zlest0035-serie_nf.

*&---------------------------------------------------------------------------
*&  Inicio ajuste referente o IR178486 / 29/04/2024 / AOENNING.
*&---------------------------------------------------------------------------
    "Seleciona dados J_1BNFE_ACTIVE / seleciona o cnpj vinculado a nota.
    IF vl_docnum IS NOT INITIAL.
      SELECT SINGLE stcd1 FROM j_1bnfe_active INTO @DATA(vg_stcd1)
      WHERE docnum EQ @vl_docnum.
      IF sy-subrc EQ 0.
        CLEAR: st_reg30-cnpj.
        st_reg30-cnpj = vg_stcd1.
      ENDIF.
    ENDIF.
*&---------------------------------------------------------------------------
*&  Fim   ajuste referente o IR178486 / 29/04/2024 / AOENNING.
*&---------------------------------------------------------------------------

    sl_zlest0035-docnum     = vl_docnum.
    sl_zlest0035-cnpj       = st_reg30-cnpj.
    sl_zlest0035-werks      = st_zlest0019-branch.
    sl_zlest0035-qtd_nf     = st_reg30-pesonota.
    sl_zlest0035-qtd_cheg   = st_reg30-pesochgd.
    sl_zlest0035-saldo      = st_reg30-pesochgd.
    sl_zlest0035-dtachegada = st_reg30-datachgd.

    APPEND sl_zlest0035 TO t_zlest0035.

  ENDLOOP.

  IF NOT vl_ctrl IS INITIAL.

    MODIFY zlest0019 FROM TABLE t_zlest0019.
    MODIFY zlest0035 FROM TABLE t_zlest0035.
    v_mensagem = 'Controle de balança importados com sucesso!'.
    PERFORM envia_mensagem_procto USING p1_file
                                        c_s
                                        '006'
                                        v_mensagem.
  ENDIF.


  IF NOT vl_ctrl87 IS INITIAL.
    MODIFY zlest0087 FROM TABLE t_zlest0087.
    v_mensagem = 'Descarga Rod. NF Terceiros importados com sucesso!'.
    PERFORM envia_mensagem_procto USING p1_file
                                        c_s
                                        '006'
                                        v_mensagem.
  ENDIF.

ENDFORM.                    " CONSISTE_DETALHE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
FORM transfere_file  USING  v_orig
                            v_log
                            v_proc
                            v_tipo
                            v_file
                            v_erro.

  DATA: v_dest    TYPE string,
        v_sour    TYPE string,
        v_dest1   TYPE string,
        v_sour1   TYPE string,
        v_rc_bool TYPE c,
        v_rc_num  TYPE i,
        li_nreg1  TYPE i,
        li_nreg2  TYPE i,
        l_path    TYPE string.

  CLEAR: v_dest, vl_ctrlfile.

* Identifica o destino da transferência
  CASE v_erro.
    WHEN: 'X'.
      v_dest = v_log.
      v_sour = v_orig.
    WHEN OTHERS.
      v_dest = v_proc.
      v_sour = v_orig.
  ENDCASE.

*  IF v_erro = c_x.
*    v_dest = v_log.
*    v_sour = v_orig.
*  ELSE.
*    v_dest = v_proc.
*    v_sour = v_orig.
*  ENDIF.

* Gera o Path completo
  CONCATENATE v_dest   v_file INTO v_dest1.
  CONCATENATE v_orig   v_file INTO v_sour1.
  CONDENSE v_dest NO-GAPS.
  CONDENSE v_sour NO-GAPS.


* Analisa o ambiente para processamento
  IF v_tipo = c_u.

    OPEN DATASET  v_dest1 FOR OUTPUT IN TEXT MODE    "smart: 11/01/10 E111
         ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
    IF sy-subrc IS INITIAL.
      LOOP AT t_file.
        TRANSFER t_file-linha TO v_dest1.
      ENDLOOP.

      CLOSE DATASET v_dest1.

      "IF v_erro NE c_x.
      DELETE DATASET v_sour1.
      "ENDIF.

    ENDIF.


    IF v_erro = c_x.
      DELETE DATASET v_sour1.
    ENDIF.


*    OPEN DATASET v_dest1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*
*    IF sy-subrc IS INITIAL.
*
*      LOOP AT t_file.
*        TRANSFER t_file-linha TO v_dest.
*      ENDLOOP.
*
*    ENDIF.
*
*    CLOSE DATASET v_dest1.


* Diretório Local - Windows
  ELSEIF v_tipo = c_l.

*   Verifica a existência do diretório
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory = v_dest
      RECEIVING
        result    = v_rc_bool.

*     Cria o diretório para transferência
    IF v_rc_bool IS INITIAL.

      CALL METHOD cl_gui_frontend_services=>directory_create
        EXPORTING
          directory = v_dest
        CHANGING
          rc        = v_rc_num.

    ENDIF.

*   Transfere o arquivo para o diretório
    CALL METHOD cl_gui_frontend_services=>file_copy
      EXPORTING
        source      = v_sour1
        destination = v_dest1
        overwrite   = 'X'.

*   Elimina o arquivo de origem
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename = v_sour1
      CHANGING
        rc       = v_rc_num.

    vl_ctrlfile = c_x.


  ENDIF.

ENDFORM.                    " TRANSFERE_FILE

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DECIMAL
*&---------------------------------------------------------------------*
FORM converte_char_decimal  USING  p_valor
                         CHANGING p_valor_dec.

  DATA: lc_valor TYPE char30.

  CLEAR p_valor_dec.
  lc_valor = p_valor.

  REPLACE ALL OCCURRENCES OF REGEX '[^0-9,]' IN lc_valor
                             WITH ''.

  CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
    EXPORTING
      i_char         = lc_valor
    IMPORTING
*     E_FLOAT        =
      e_packed       = p_valor_dec
    EXCEPTIONS
      invalid_number = 1
      OTHERS         = 2.

ENDFORM.                    " CONVERTE_CHAR_DECIMAL

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DATE
*&---------------------------------------------------------------------*
FORM converte_char_date  USING  p_char_data
                      CHANGING  p_dats_data.

  DATA: l_aux_data(8) TYPE c,
        l_aux(10)     TYPE c,
        l_dia(2),
        l_mes(2),
        l_ano(4).

  CLEAR: p_dats_data.
  l_aux = p_char_data.

  REPLACE ALL OCCURRENCES OF REGEX '[^0-9\s]' IN l_aux
    WITH '/'.

  IF sy-subrc IS INITIAL.
    SPLIT l_aux AT '/' INTO l_dia l_mes l_ano.
    CONCATENATE l_ano l_mes l_dia INTO l_aux_data.
  ELSE.
    l_aux_data = l_aux.
  ENDIF.

  CALL FUNCTION 'RP_CHECK_DATE'
    EXPORTING
      date         = l_aux_data
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  IF sy-subrc IS INITIAL.
    p_dats_data = l_aux_data.
  ENDIF.

ENDFORM.                    " CONVERTE_CHAR_DATE

*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQUEIO_ARQUIVO
*&---------------------------------------------------------------------*
FORM check_bloqueio_arquivo  USING     p_filename
                             CHANGING  p_erro.

  DATA: lc_mensagem  TYPE bdc_vtext1.

  CLEAR: p_erro.

  READ TABLE t_zlest0009 INTO st_zlest0009 WITH KEY filename = p_filename.

  IF NOT sy-subrc IS INITIAL.

    st_zlest0009-filename = p_filename.
    st_zlest0009-data     = sy-datum.
    st_zlest0009-hora     = sy-uzeit.
    st_zlest0009-usuario  = sy-uname.

    APPEND: st_zlest0009 TO t_zlest0009,
            st_zlest0009 TO t_0009_aux.

    INSERT zlest0009 FROM st_zlest0009.
    COMMIT WORK.

    CLEAR st_zlest0009.

  ELSE.

    p_erro = c_x.

    CONCATENATE 'Arquivo' p_filename 'já esta sendo processado.'
    INTO lc_mensagem SEPARATED BY space.

    PERFORM envia_mensagem_procto USING p_filename
                                        c_e
                                        '11'
                                        lc_mensagem.
  ENDIF.

ENDFORM.                    " CHECK_BLOQUEIO_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  ENVIA_MENSAGEM_PROCTO
*&---------------------------------------------------------------------*
FORM envia_mensagem_procto  USING p_filename
                                  p_msgtyp
                                  p_msgnr
                                  p_msgv1.
  "STATICS: LV_FILENAME  TYPE EPSFILNAM VALUE %_MAXCHAR,
  STATICS: lv_filename TYPE sdokpath-pathname VALUE %_maxchar,
           lv_version  TYPE zidctrl,
           lv_vcont    TYPE numc10.

  IF ( lv_filename <> p_filename ) AND ( v_call_ext EQ abap_false ).

    CLEAR: st_zlest0008,
           lv_vcont.

    IF p_filename = sy-repid.

      SELECT MAX( idctrl ) MAX( cont )
        INTO (st_zlest0008-idctrl, lv_vcont)
        FROM zlest0008
       WHERE filename = p_filename
        GROUP BY idctrl.
      ENDSELECT.

      IF sy-subrc IS INITIAL.
        IF lv_vcont >= '9999999998'.
          lv_version = st_zlest0008-idctrl + 1.
          CLEAR lv_vcont.
        ELSE.
          lv_version = st_zlest0008-idctrl.
        ENDIF.
      ELSE.
        lv_version = st_zlest0008-idctrl + 1.
      ENDIF.

    ELSE.
      READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = p_filename.
      lv_version = st_zlest0008-idctrl + 1.
    ENDIF.

    lv_filename = p_filename.
  ENDIF.

  ADD 1 TO lv_vcont.
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
  st_mess-lote     = space.

  IF v_call_ext EQ abap_true.
    APPEND st_mess TO t_log_ext.
  ELSE.
    INSERT zlest0008 FROM st_mess.
    COMMIT WORK.
  ENDIF.

  CLEAR st_mess.


ENDFORM.                    " ENVIA_MENSAGEM_PROCTO

*&---------------------------------------------------------------------*
*&      Form  CONTROLE_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1998   text
*      -->P_MSG_NUM  text
*      -->P_V_MENSAGEM  text
*----------------------------------------------------------------------*
FORM controle_msg  USING    p_dir
                            p_msg_num
                            p_msg
                            p_tabix.

  DATA : vl_index TYPE sy-index.

  SORT t_msg BY repid direcao msg_num.

  READ TABLE t_msg INTO st_msg WITH KEY repid    = sy-repid
                                        direcao  = p_dir
                                        msg_num  = p_msg_num.

  IF sy-subrc IS INITIAL.
    vl_index = st_msg-msg_seq.
    vl_index = vl_index + 1.
  ELSE.
    st_msg-msg_seq = '001'.
  ENDIF.

  MOVE:
  sy-repid   TO st_msg-repid,
  p_dir      TO st_msg-direcao,
  p_msg_num  TO st_msg-msg_num,
  p_msg      TO st_msg-texto,
  p_tabix    TO st_msg-tabix.

  APPEND st_msg TO t_msg.

  v_erro = c_x.

ENDFORM.                    " CONTROLE_MSG

*&---------------------------------------------------------------------*
*&      Form  zgeranumero
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ZNUMERO    text
*----------------------------------------------------------------------*
FORM zgeranumero CHANGING znumero .

  DATA: lc_key(10) TYPE n.

  DATA: vn_parametro TYPE zsped002-nr_parametro.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZSEQ_LES19'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '1'
      object                  = 'ZSEQ_LES19'
      quantity                = '00000000000000000001'
      ignore_buffer           = 'X'
    IMPORTING
      number                  = lc_key
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

* Desbloqueia o objeto de numeração
  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      object           = 'ZSEQ_LES19'
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  znumero = lc_key.


ENDFORM.                 " ZGERANUMERO  INPUT


FORM f_proc_registros_externo USING p_l1_30 TYPE zde_zlest0019_l1_30_t
                           CHANGING p_logs  TYPE zlest0008_t.

  DATA: vl_index     TYPE sy-index,
        vl_tabix     TYPE sy-tabix,
        v_file       TYPE string,
        v_tipo       TYPE string,
        vl_id_refkey TYPE zlest0019-id_zlest0019.

  CLEAR: v_erro, t_log_ext[].

  v_call_ext = abap_true.

  REFRESH: t_file,
           t_branch,
           t_nfdoc,
           t_reg30,
           t_zlest0019.

  CHECK p_l1_30[] IS NOT INITIAL.

*----------------------------------------------------------------------*
* Carregar Registros L1/10.
*----------------------------------------------------------------------*
  READ TABLE p_l1_30 INTO DATA(_wl_l1_30) INDEX 1.

  CHECK sy-subrc EQ 0.

  CLEAR: st_reg10.

  PERFORM zgeranumero CHANGING st_reg10-id_refkey.

  st_reg10-tipo      = '10'.
  st_reg10-movto     = 'E'.
  st_reg10-cnpj      = _wl_l1_30-cnpjferro.
  st_reg10-dataenv   = _wl_l1_30-dtaenvio.
  st_reg10-horaenv   = _wl_l1_30-horaenvio.
  st_reg10-tabix     = 1.

*----------------------------------------------------------------------*
* Carregar Registros L1/30.
*----------------------------------------------------------------------*
  CLEAR: vl_index.
  LOOP AT p_l1_30 INTO _wl_l1_30.

    vl_tabix = sy-tabix.
    ADD 1 TO vl_index.

    CLEAR: st_reg30.

    st_reg30-tipo     = '30'.

    IF _wl_l1_30-nfenum IS NOT INITIAL.
      st_reg30-nf       = _wl_l1_30-nfenum.
    ELSE.
      st_reg30-nf       = _wl_l1_30-nfnum.
    ENDIF.

    st_reg30-pesonota   = _wl_l1_30-pesonf.
    st_reg30-pesochgd   = _wl_l1_30-pesodvagao.
    st_reg30-datachgd   = _wl_l1_30-dtachegada.
    st_reg30-cnpj       = _wl_l1_30-cnpjcliente.
    st_reg30-tp_transgenia = _wl_l1_30-tp_transgenia.
    st_reg30-index      = vl_index.
    st_reg30-tabix      = vl_tabix.
    APPEND st_reg30 TO t_reg30.

  ENDLOOP.

  PERFORM consiste_header_arquivo USING v_file c_l.

* Erro no header do arquivo
  IF st_reg10-erro IS INITIAL.
    PERFORM consiste_detalhe_arquivo USING v_file
                                           c_l.
  ENDIF.

  p_logs[] = t_log_ext[].

ENDFORM.                    " CARREGA_ARQ

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_CARGUERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_ZLEST0039  text
*      -->P_ST_REG30_PESOCHGD  text
*----------------------------------------------------------------------*
FORM verificar_carguero  USING    p_lw_zlest0039    TYPE zlest0039
                                  p_lw_zlest0039_a TYPE zlest0039.

  "Ñão é possível o reenvio da quantidade ainda
  CHECK 1 EQ 2.

  IF ( p_lw_zlest0039-datatransb NE p_lw_zlest0039_a-datatransb AND p_lw_zlest0039-datatransb IS NOT INITIAL ).
    DATA(alterou_data_transbordo) = abap_true.
  ELSE.
    alterou_data_transbordo = abap_false.
  ENDIF.

  IF ( p_lw_zlest0039-pesotransb  NE p_lw_zlest0039_a-pesotransb  AND p_lw_zlest0039-pesotransb  IS NOT INITIAL ).
    DATA(alterou_peso_transbordo) = abap_true.
  ELSE.
    alterou_peso_transbordo = abap_false.
  ENDIF.

  DATA(alterar) = abap_false.

  IF p_lw_zlest0039-pesotransb IS INITIAL AND p_lw_zlest0039_a-pesotransb IS NOT INITIAL.
    alterar = abap_true.
  ELSEIF alterou_peso_transbordo EQ abap_true AND p_lw_zlest0039_a-pesotransb IS NOT INITIAL.
    alterar = abap_true.
  ELSEIF alterou_data_transbordo EQ abap_true AND p_lw_zlest0039_a-pesotransb IS NOT INITIAL.
    alterar = abap_true.
  ELSEIF p_lw_zlest0039_a-pesotransb IS INITIAL.

    IF ( p_lw_zlest0039-datachegada NE p_lw_zlest0039_a-datachegada AND p_lw_zlest0039-datachegada IS NOT INITIAL ).
      DATA(alterou_data_chegada) = abap_true.
    ELSE.
      alterou_data_chegada = abap_false.
    ENDIF.

    IF ( p_lw_zlest0039-pesochegada  NE p_lw_zlest0039_a-pesochegada AND p_lw_zlest0039-pesochegada IS NOT INITIAL ).
      DATA(alterou_peso_chegada) = abap_true.
    ELSE.
      alterou_peso_chegada = abap_false.
    ENDIF.

    IF p_lw_zlest0039-pesochegada IS INITIAL AND p_lw_zlest0039_a-pesochegada IS NOT INITIAL.
      alterar = abap_true.
    ELSEIF alterou_peso_chegada EQ abap_true AND p_lw_zlest0039_a-pesochegada IS NOT INITIAL.
      alterar = abap_true.
    ELSEIF alterou_data_chegada EQ abap_true AND p_lw_zlest0039_a-pesochegada IS NOT INITIAL.
      alterar = abap_true.
    ENDIF.

  ENDIF.

  IF alterar EQ abap_true.

    SELECT SINGLE * INTO @DATA(wa_zsdt0001)
      FROM zsdt0001
     WHERE nro_nf_prod EQ @p_lw_zlest0039-docnum
       AND nro_nf_prod NE @space.

    CHECK sy-subrc IS INITIAL.
    CHECK wa_zsdt0001-id_ordem IS NOT INITIAL.

    UPDATE zlest0185
       SET ck_descarregado = abap_false
     WHERE id_ordem EQ wa_zsdt0001-id_ordem.

  ENDIF.

ENDFORM.
