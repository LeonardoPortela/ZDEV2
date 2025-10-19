*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : BBKO Consulting S.A.                                    *
* Data       : 07/07/2010                                              *
* Descrição  : Importação dos arquivos da ALL                          *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
* Cód Espec. :                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 07/07/2010 | BBKO         | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*
REPORT  zlesi0002 MESSAGE-ID zles.
*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES:
        zlest0010,
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
           cnpj       TYPE stcd1,
           dataenv    TYPE d,
           horaenv    TYPE sy-uzeit,
           observ(100),
       END OF y_reg10,

       BEGIN OF y_reg30,
           tipo(2),
           nfenum     TYPE j_1bnfnum9,
           pesonota   TYPE brgew_15,
           pesochgd   TYPE brgew_15,
           datachgd   TYPE d,
           cnpj       TYPE stcd1,
       END OF y_reg30,

       BEGIN OF y_reg50,
           tipo(2),
           totreg30   TYPE numc3,
           totpeso2   TYPE brgew_15,
       END OF y_reg50,

       BEGIN OF y_branch,
         bukrs        TYPE j_1bbranch-bukrs,
         branch       TYPE j_1bbranch-branch,
         stcd1        TYPE j_1bbranch-stcd1,
       END OF y_branch,

       BEGIN OF y_nfdoc,
         docnum       TYPE j_1bnfdoc-docnum,
         nfenum       TYPE j_1bnfdoc-nfenum,
         series       TYPE j_1bnfdoc-series,
         bukrs        TYPE j_1bnfdoc-bukrs,
         branch       TYPE j_1bnfdoc-branch,
       END OF y_nfdoc,

       BEGIN OF ty_zlest0041,
         nr_nf       TYPE zlest0041-nr_nf,
         serie       TYPE zlest0041-serie,
         centro_comprador TYPE zlest0041-centro_comprador,
       END   OF ty_zlest0041,

       BEGIN OF y_index,
        index         TYPE i,
      END OF y_index,

      BEGIN OF y_file,
        linha(400),
      END OF y_file.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA:  t_file         TYPE STANDARD TABLE OF y_file
                           WITH HEADER LINE INITIAL SIZE 0,

      t_idxerro       TYPE STANDARD TABLE OF y_index
                           WITH HEADER LINE INITIAL SIZE 0,

      t_file_transf   TYPE TABLE OF y_file.

DATA: t_files_unix    TYPE TABLE OF epsfili,
      t_zlest0007     TYPE TABLE OF zlest0007,
      t_dir_unix      TYPE TABLE OF epsfili,
      t_dir_local     TYPE TABLE OF sdokpath,
      t_dir_loc_f     TYPE TABLE OF sdokpath,
      t_reg30         TYPE TABLE OF y_reg30,
      t_zlest0041     TYPE TABLE OF ty_zlest0041,
      t_zlest0010     TYPE TABLE OF zlest0010,
      t_zlest0008     TYPE TABLE OF zlest0008,
      t_zlest0009     TYPE TABLE OF zlest0009,
      t_0009_aux      TYPE TABLE OF zlest0009,
      t_branch        TYPE TABLE OF y_branch,
      t_nfdoc         TYPE TABLE OF y_nfdoc,
      t_dest          TYPE TABLE OF somlreci1.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: st_files_doc    TYPE sdokpath,
      st_files_unix   TYPE epsfili,
      st_zlest0007    TYPE zlest0007,
      st_zlest0010    TYPE zlest0010,
      st_zlest0008    TYPE zlest0008,
      st_zlest0009    TYPE zlest0009,
      st_mess         TYPE zlest0008,
      st_reg10        TYPE y_reg10,
      st_reg30        TYPE y_reg30,
      st_reg50        TYPE y_reg50,
      st_reg50_check  TYPE y_reg50,
      st_branch       TYPE y_branch,
      st_nfdoc        TYPE y_nfdoc,
      st_zlest0041    TYPE ty_zlest0041,
      st_0009         TYPE zlest0009.

*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: v_caminho      TYPE epsf-epsdirnam,
      v_mensagem     TYPE bapi_msg,
      v_erro         TYPE c,
      v_prefix_ent   TYPE zlest0007-prefix,
      v_prefix_log   TYPE zlest0007-prefix,
      v_prefix_proc  TYPE zlest0007-prefix,
      v_version      TYPE zlest0008-idctrl.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_asc(10)      TYPE c VALUE 'ASC',
           c_mask_loc(6)  TYPE c VALUE '*.*',
           c_mask_unix(6) TYPE c VALUE '*.*',
           c_u            TYPE c VALUE 'U',
           c_w            TYPE c VALUE 'W',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S'.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE text-002.
PARAMETERS: p_input(60) TYPE c MODIF ID fil,
            p_proc(60)  TYPE c MODIF ID fil,
            p_log(60)   TYPE c MODIF ID fil,
            p_chkso(1)  TYPE c NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE text-003.
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
        v_erro_log.

  CHECK: NOT p_input IS INITIAL
     AND NOT p_proc  IS INITIAL
     AND NOT p_log   IS INITIAL.

  CLEAR: v_erro_log.
  REFRESH t_0009_aux.

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
      LEAVE LIST-PROCESSING.
    ELSE.

*     Obtem controle informações para controle de Log
      SELECT *
        FROM zlest0008
        INTO TABLE t_zlest0008
         FOR ALL ENTRIES IN t_dir_unix
       WHERE filename = t_dir_unix-name.

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

*     Obtem arquivos bloqueados
      SELECT *
        FROM zlest0009
        INTO TABLE t_zlest0009
         FOR ALL ENTRIES IN t_dir_unix
       WHERE filename = t_dir_unix-name.

*     Bloqueia arquivos para processamento
      LOOP AT t_dir_unix INTO st_files_unix.
        v_index = sy-tabix.
        PERFORM check_bloqueio_arquivo USING st_files_unix-name
                                    CHANGING v_erro.
        IF v_erro = c_x.
          DELETE t_dir_unix INDEX v_index.
          PERFORM transfere_file USING p_input p_log c_u st_files_unix-name c_x.
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
        directory        = p_input
        filter           = v_mask_locl
*     IMPORTING
*       FILE_COUNT       =
*       DIR_COUNT        =
      TABLES
        file_table       = t_dir_loc_f
        dir_table        = t_dir_local
   EXCEPTIONS
     cntl_error       = 1
     OTHERS           = 2.

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
       WHERE filename = t_dir_loc_f-pathname(40).

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

*     Obtem arquivos bloqueados
      SELECT *
        FROM zlest0009
        INTO TABLE t_zlest0009
         FOR ALL ENTRIES IN t_dir_loc_f
       WHERE filename = t_dir_loc_f-pathname(40).

*     Bloqueia todos registro antes da consistência
      LOOP AT t_dir_loc_f INTO st_files_doc.
        v_index = sy-tabix.
        PERFORM check_bloqueio_arquivo USING st_files_doc-pathname
                                   CHANGING  v_erro.
        IF v_erro = c_x.
          DELETE t_dir_loc_f INDEX v_index.
          PERFORM transfere_file USING p_input p_log c_l st_files_doc-pathname c_x.
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

ENDFORM.                    " LE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
FORM carrega_arq  USING    v_file
                           v_tipo.

  DATA: v_caminho     TYPE string,
        lidx_aux1     TYPE i,
        lidx_aux2     TYPE i.

  CLEAR v_erro.

  REFRESH: t_file,
           t_idxerro,
           t_branch,
           t_nfdoc,
           t_reg30,
           t_zlest0010.

  CONCATENATE p_input v_file INTO v_caminho.

  PERFORM le_arquivo_unix_window USING v_tipo
                                       v_caminho.

  PERFORM transfer_arqlido_tabreg.

  PERFORM consiste_header_arquivo USING v_file
                                        v_tipo.
  CHECK: v_erro IS INITIAL.

  PERFORM consiste_detalhe_arquivo USING v_file
                                         v_tipo.

  IF NOT t_idxerro[] IS INITIAL.
    PERFORM transfere_file USING p_input p_log v_tipo v_file space.
    v_erro = c_x.
  ENDIF.

  PERFORM transfere_file USING p_input p_proc v_tipo v_file c_x.

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

  IF p_tipo = c_u.
*   Lê arquivo UNIX
    OPEN DATASET v_caminho FOR INPUT IN BINARY MODE.
    DO.
      READ DATASET p_path INTO t_file.
      IF sy-subrc  IS INITIAL.
        APPEND t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ELSEIF p_tipo = c_l.
*   Lê arquivo WINDOWS
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

  DATA: l_char06      TYPE char6,
        l_char11      TYPE char11.

  CLEAR: st_reg10,
         st_reg30,
         st_reg50,
         st_reg50_check,
         v_mensagem.

  LOOP AT t_file.

    IF t_file-linha(2) = '10'.

      st_reg10-tipo    = t_file-linha(2).
      st_reg10-movto   = t_file-linha+2(1).
      st_reg10-empresa = t_file-linha+3(70).
      st_reg10-cnpj    = t_file-linha+73(14).
      PERFORM converte_char_date USING t_file-linha+87(10)
                              CHANGING st_reg10-dataenv.
      l_char06 = t_file-linha+97(5).
      REPLACE ALL OCCURRENCES OF REGEX '\D' IN l_char06 WITH ''.
      CONCATENATE l_char06 '00' INTO l_char06.
      st_reg10-horaenv = l_char06.
      st_reg10-observ  = t_file-linha+102.

    ELSEIF t_file-linha(2) = '30'.

      st_reg30-tipo     = t_file-linha(2).
      l_char11          = t_file-linha+2(11).
      IF l_char11(2) IS INITIAL OR l_char11(2) = '00'.
        st_reg30-nfenum = l_char11+2.
      ELSE.
        st_reg30-nfenum = l_char11.
      ENDIF.
      CONDENSE st_reg30-nfenum.
      PERFORM converte_char_decimal USING t_file-linha+13(14)
                                 CHANGING st_reg30-pesonota.
      PERFORM converte_char_decimal USING t_file-linha+27(14)
                                 CHANGING st_reg30-pesochgd.
      PERFORM converte_char_date USING t_file-linha+41(10)
                              CHANGING st_reg30-datachgd.
      st_reg30-cnpj     = t_file-linha+51(14).
      APPEND st_reg30 TO t_reg30.

      st_reg50_check-tipo = '50'.
      ADD: 1                  TO st_reg50_check-totreg30,
           st_reg30-pesonota  TO st_reg50_check-totpeso2.

    ELSEIF t_file-linha(2) = '50'.

      st_reg50-tipo = t_file-linha(2).
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9,]' IN t_file-linha+2(3)
                             WITH ''.
      st_reg50-totreg30 = t_file-linha+2(3).
      PERFORM converte_char_decimal USING t_file-linha+5(14)
                                 CHANGING st_reg50-totpeso2.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " TRANSFER_ARQLIDO_TABREG

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_HEADER_ARQUIVO
*&---------------------------------------------------------------------*
FORM consiste_header_arquivo  USING p1_file
                                    p1_tipo.

  DATA: BEGIN OF lt_j_1bnfdoc OCCURS 0,
          nfenum      TYPE j_1bnfnum9,
          bukrs       TYPE bukrs,
          branch      TYPE j_1bbranc_,
        END OF   lt_j_1bnfdoc,

        BEGIN OF lt_filial OCCURS 0,
          cnpj        TYPE stcd1,
        END OF lt_filial.

  DATA: lc_valor1(15) TYPE c,
        lc_valor2(15) TYPE c.

  CLEAR: v_erro,
         v_mensagem.

* Verifica Layout do arquivo e totalizadores
  IF NOT st_reg10-tipo IS INITIAL AND
     NOT st_reg50-tipo IS INITIAL AND
     NOT t_reg30[]     IS INITIAL.

    IF st_reg50_check-totreg30 <> st_reg50-totreg30.
      v_erro = c_x.
      CONCATENATE 'Total de'
                  st_reg50-totreg30
                  'reg. do tipo 30 informado difere de'
                  st_reg50_check-totreg30
                  'reg. calculados na conferência.'
            INTO v_mensagem SEPARATED BY space.

    ELSEIF st_reg50_check-totpeso2 <> st_reg50-totpeso2.
      v_erro = c_x.
      WRITE: st_reg50-totpeso2       TO lc_valor1 NO-SIGN NO-ZERO,
             st_reg50_check-totpeso2 TO lc_valor2 NO-SIGN NO-ZERO.
      CONCATENATE 'Total de peso chegada'
                  lc_valor1
                  'informado difere de'
                  lc_valor2
                  'calculado na conferência.'
             INTO v_mensagem SEPARATED BY space.

    ENDIF.
  ELSE.
    v_erro = c_x.
    v_mensagem = 'Arquivo esta fora do Layout.'.
  ENDIF.

* Valida registro do tipo 10 e 30
  IF v_erro IS INITIAL.

*   Dados gerais do Fornecedor
    SELECT COUNT( * )
      FROM lfa1
      WHERE stcd1 = st_reg10-cnpj.

    IF sy-subrc IS INITIAL.

*     Dados gerais de Filiais
      LOOP AT t_reg30 INTO st_reg30.
        lt_filial-cnpj = st_reg30-cnpj.
        APPEND lt_filial.
      ENDLOOP.

      SORT lt_filial.
      DELETE ADJACENT DUPLICATES FROM lt_filial.

      IF NOT lt_filial[] IS INITIAL.
        SELECT bukrs branch stcd1
          FROM j_1bbranch
          INTO TABLE t_branch
          FOR ALL ENTRIES IN lt_filial
          WHERE stcd1 = lt_filial-cnpj.
      ENDIF.

      IF t_branch[] IS INITIAL.
        CONCATENATE 'Nenhum dos CNPJ do reg. 30 do arquivo'
                    'pertencem ao grupo Andre Maggi'
              INTO v_mensagem SEPARATED BY space.
        v_erro = c_x.
      ELSE.

*       Notas fiscais das filias
        SORT t_branch BY stcd1.

        LOOP AT t_reg30 INTO st_reg30.
          READ TABLE t_branch INTO st_branch
                WITH KEY stcd1 = st_reg30-cnpj BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lt_j_1bnfdoc-nfenum = st_reg30-nfenum.
            lt_j_1bnfdoc-bukrs  = st_branch-bukrs.
            lt_j_1bnfdoc-branch = st_branch-branch.
            APPEND lt_j_1bnfdoc.
          ENDIF.
        ENDLOOP.

        IF NOT lt_j_1bnfdoc[] IS INITIAL.
          SELECT docnum nfenum series bukrs branch
            INTO TABLE t_nfdoc
            FROM j_1bnfdoc
             FOR ALL ENTRIES IN lt_j_1bnfdoc
           WHERE nfenum = lt_j_1bnfdoc-nfenum
             AND bukrs  = lt_j_1bnfdoc-bukrs
             AND branch = lt_j_1bnfdoc-branch.

* NF de remessa por conta e ordem de terceiros
          SELECT nr_nf serie centro_comprador
            INTO TABLE t_zlest0041
            FROM zlest0041
             FOR ALL ENTRIES IN lt_j_1bnfdoc
           WHERE nr_nf  = lt_j_1bnfdoc-nfenum
             AND centro_comprador = lt_j_1bnfdoc-branch.


        ENDIF.

        IF t_nfdoc[] IS INITIAL and t_zlest0041[] is initial.
          v_mensagem = 'Nenhuma NFe do reg. 30 foi encontrada'.
          v_erro = c_x.
        ENDIF.

      ENDIF.

    ELSE.
      v_erro = c_x.
      CONCATENATE 'CNPJ'
                  st_reg10-cnpj
                  'não cadastrado no cadastro de fornecedores'
            INTO v_mensagem SEPARATED BY space.
    ENDIF.
  ENDIF.

* Registra erro e transfere o arquivo para diretório de log
  CHECK: v_erro = c_x.

  PERFORM envia_mensagem_procto USING p1_file
                                      c_e
                                      '01'
                                      v_mensagem.

  PERFORM transfere_file USING p_input
                               p_log
                               p1_tipo
                               p1_file
                               c_x.

ENDFORM.                    " CONSISTE_HEADER_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CONSISTE_DETALHE_ARQUIVO
*&---------------------------------------------------------------------*
FORM consiste_detalhe_arquivo  USING p1_file
                                     p1_tipo.

  DATA: lidx_aux2     TYPE i.
  SORT: t_nfdoc BY nfenum bukrs branch.

  REFRESH t_zlest0010.
  CLEAR   t_zlest0010.

  LOOP AT t_reg30 INTO st_reg30.

    lidx_aux2 = sy-tabix.

    READ TABLE t_branch INTO st_branch WITH KEY stcd1 = st_reg30-cnpj
    BINARY SEARCH.

    IF sy-subrc <> 0.
      CONCATENATE 'CNPJ'
            st_reg30-cnpj
            'do reg. 10 não pertence ao grupo Andre Maggi'
            INTO v_mensagem SEPARATED BY space.
      PERFORM envia_mensagem_procto USING p1_file
                                          c_e
                                          '02'
                                          v_mensagem.
      t_idxerro-index = lidx_aux2 + 1. "Soma o Header
      APPEND t_idxerro.
      CONTINUE.
    ENDIF.

    READ TABLE t_nfdoc INTO st_nfdoc WITH KEY nfenum = st_reg30-nfenum
                                              bukrs  = st_branch-bukrs
                                              branch = st_branch-branch
    BINARY SEARCH.

    IF sy-subrc <> 0.
      CONCATENATE 'Nota' st_reg30-nfenum 'não existe para a empresa'
                  st_branch-bukrs
                  'e filial' st_branch-branch
            INTO v_mensagem SEPARATED BY space.
      PERFORM envia_mensagem_procto USING p1_file
                                          c_e
                                          '03'
                                          v_mensagem.
      t_idxerro-index = lidx_aux2 + 1. "Soma o Header
      APPEND t_idxerro.
      CONTINUE.
    ENDIF.

*   Alimenta tabela de registros OK!
    st_zlest0010-bukrs       = st_branch-bukrs.
    st_zlest0010-branch      = st_branch-branch.
    st_zlest0010-nfenum      = st_reg30-nfenum.
    st_zlest0010-cnpj        = st_reg10-cnpj.
    st_zlest0010-zdata_chega = st_reg30-datachgd.
    st_zlest0010-peso_nota   = st_reg30-pesonota.
    st_zlest0010-peso_chega  = st_reg30-pesochgd.
    st_zlest0010-tp_movi     = st_reg10-movto.
    st_zlest0010-cnpj2       = st_reg30-cnpj.
    st_zlest0010-data        = sy-datum.
    st_zlest0010-hora        = sy-uzeit.
    st_zlest0010-usuario     = sy-uname.
    APPEND st_zlest0010 TO t_zlest0010.

  ENDLOOP.

  CHECK: NOT t_zlest0010[] IS INITIAL.

  INSERT zlest0010 FROM TABLE t_zlest0010.

  v_mensagem = 'Controle de balança importados com sucesso!'.
  PERFORM envia_mensagem_procto USING p1_file
                                      c_s
                                      '04'
                                      v_mensagem.

ENDFORM.                    " CONSISTE_DETALHE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
FORM transfere_file  USING  v_de
                            v_para
                            v_tipo
                            v_file
                            v_dele.

  DATA: v_dest    TYPE rlgrap-filename,
        v_sour    TYPE rlgrap-filename,
        v_dest1   TYPE string,
        v_sour1   TYPE string,
        v_rc_bool TYPE c,
        v_rc_num  TYPE i,
        v_orig    TYPE char4,
        li_nreg1  TYPE i,
        li_nreg2  TYPE i,
        l_path    TYPE string.

* Identifica o destino da transferência
  IF v_para = p_log.
    v_orig = 'ERRO'.
  ELSE.
    v_orig = 'OK'.
  ENDIF.

* Gera o Path
  CONCATENATE v_para v_file INTO v_dest.
  CONCATENATE v_de   v_file INTO v_sour.

* Gera o Path para check e transferência
  v_dest1 = v_para. "Somente o diretório
  v_sour1 = v_sour. "Path completo

* Analisa o ambiente para processamento
  IF v_tipo = c_u.

    OPEN DATASET v_dest FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc IS INITIAL.

      LOOP AT t_file.

*       O header do arquivo é sempre salvo independente do erro
        IF sy-tabix > 1.
          CLEAR t_idxerro.
          READ TABLE t_idxerro WITH KEY INDEX = sy-tabix.
          IF v_orig = 'OK' AND t_idxerro-index > 0.
            CONTINUE.
          ELSEIF t_idxerro-index = 0.
            IF v_orig = 'ERRO' AND NOT t_idxerro[] IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        TRANSFER t_file-linha TO v_dest.
      ENDLOOP.
    ENDIF.
    CLOSE DATASET v_dest.

    IF v_dele = c_x.
      DELETE DATASET v_de.
    ENDIF.

  ELSEIF v_tipo = c_l.

    DESCRIBE TABLE t_file    LINES li_nreg1.
    DESCRIBE TABLE t_idxerro LINES li_nreg2.

*   Tudo certo ou Tudo errado
    IF ( v_orig = 'OK'   AND li_nreg2 IS INITIAL ) OR
       ( v_orig = 'ERRO' AND ( li_nreg1 = li_nreg2 OR
         li_nreg2 IS INITIAL ) ).

*     Verifica a existência do diretório
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory = v_dest1
        RECEIVING
          result    = v_rc_bool.

*     Cria o diretório para transferência
      IF v_rc_bool IS INITIAL.
        CALL METHOD cl_gui_frontend_services=>directory_create
          EXPORTING
            directory = v_dest1
          CHANGING
            rc        = v_rc_num.
      ENDIF.

*     Transfere o arquivo para o diretório
      v_dest1 = v_dest. "Path completo
      CALL METHOD cl_gui_frontend_services=>file_copy
        EXPORTING
          SOURCE      = v_sour1
          DESTINATION = v_dest1
          overwrite   = 'X'.

    ELSE.

      REFRESH t_file_transf.

      LOOP AT t_file.
*       O header do arquivo é sempre salvo independente do erro
        IF sy-tabix > 1.
          CLEAR t_idxerro.
          READ TABLE t_idxerro WITH KEY INDEX = sy-tabix.
          IF v_orig = 'OK' AND t_idxerro-index > 0.
            CONTINUE.
          ELSEIF t_idxerro-index = 0.
            IF v_orig = 'ERRO'  AND NOT t_idxerro[] IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND  t_file TO t_file_transf.
      ENDLOOP.

      l_path = v_dest.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = l_path
          filetype                = c_asc
        TABLES
          data_tab                = t_file_transf
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

    ENDIF.

*   Elimina o arquivo de origem
    IF v_dele = c_x.
      CALL METHOD cl_gui_frontend_services=>file_delete
        EXPORTING
          filename = v_sour1
        CHANGING
          rc       = v_rc_num.
    ENDIF.

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
      i_char               = lc_valor
   IMPORTING
*  E_FLOAT              =
     e_packed             = p_valor_dec
   EXCEPTIONS
     invalid_number       = 1
     OTHERS               = 2.

ENDFORM.                    " CONVERTE_CHAR_DECIMAL

*&---------------------------------------------------------------------*
*&      Form  CONVERTE_CHAR_DATE
*&---------------------------------------------------------------------*
FORM converte_char_date  USING  p_char_data
                      CHANGING  p_dats_data.

  DATA:  l_aux_data(8) TYPE c,
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
FORM check_bloqueio_arquivo  USING  p_filename
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

  STATICS: lv_filename  TYPE epsfilnam VALUE %_maxchar,
           lv_version   TYPE zidctrl,
           lv_vcont     TYPE numc10.

  IF lv_filename <> p_filename.

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

  INSERT zlest0008 FROM st_mess.
  CLEAR st_mess.

ENDFORM.                    " ENVIA_MENSAGEM_PROCTO
