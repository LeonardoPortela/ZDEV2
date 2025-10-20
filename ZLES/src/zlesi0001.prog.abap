*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : Thiago R.R.Escudeiro / BBKO Consulting S.A.             *
* Data       : 17/07/2010                                              *
* Descrição  : Importação dos arquivos Ferroviário                     *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
* Cód Espec. :                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 19/07/2010 | BBKO         | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*
REPORT  zlesi0001 MESSAGE-ID zles.
*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: zlest0003,
        zlest0004,
        zlest0005,
        zlest0006,
        zlest0007,
        zlest0008,
        zlest0009,
        adr6.

**----------------------------------------------------------------------*
** Tipos                                                                *
**----------------------------------------------------------------------*
* TYPES:
*---> 30/05/2023 - Migração S4 - JS
data: begin of ty_dir_unix,
        NAME  type  EPSFILNAM,
        SIZE  type  EPSFILSIZ,
        RC    type  EPSFTPRC,
      end of ty_dir_unix.
*<--- 30/05/2023 - Migração S4 - JS
*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: t_files_loc     TYPE TABLE OF sdokpath,
      t_files_unix    TYPE TABLE OF epsfili,
      t_zlest0003     TYPE TABLE OF zlest0003,
      t_zlest0004     TYPE TABLE OF zlest0004,
      t_zlest0005     TYPE TABLE OF zlest0005,
      t_zlest0006     TYPE TABLE OF zlest0006,
      t_zlest0007     TYPE TABLE OF zlest0007,
      t_zlest0008     TYPE TABLE OF zlest0008,
      t_email0008     TYPE TABLE OF  zlest0008,
      t_zlest0009     TYPE TABLE OF zlest0009,
      t_0009_aux      TYPE TABLE OF zlest0009,
      t_dir_unix      TYPE TABLE OF epsfili,
      t_dir_local     TYPE TABLE OF sdokpath,
      t_dir_loc_f     TYPE TABLE OF sdokpath.

DATA: t_email_plist   TYPE STANDARD TABLE OF sopcklsti1
                           WITH HEADER LINE INITIAL SIZE 10,
      t_email_rcvrs   TYPE STANDARD TABLE OF somlreci1
                           WITH HEADER LINE INITIAL SIZE 50,
      t_email_textos  TYPE STANDARD TABLE OF solisti1
                           WITH HEADER LINE INITIAL SIZE 100,
      t_obj_header    TYPE STANDARD TABLE OF solisti1
                           WITH HEADER LINE INITIAL SIZE 1.

DATA: BEGIN OF t_file OCCURS 0,
        linha(400),
      END OF t_file.
*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: st_files_doc    TYPE sdokpath,
      st_files_unix   TYPE epsfili,
      st_zlest0003    TYPE zlest0003,
      st_zlest0004    TYPE zlest0004,
      st_zlest0005    TYPE zlest0005,
      st_zlest0006    TYPE zlest0006,
      st_zlest0007    TYPE zlest0007,
      st_zlest0008    TYPE zlest0008,
      st_zlest0009    TYPE zlest0009,
      st_zlest0041    TYPE zlest0041,
      st_lfa1         TYPE lfa1     ,
      st_mess         TYPE zlest0008,
      st_0009         TYPE zlest0009,
      st_doc          LIKE sodocchgi1.

*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: v_caminho       TYPE epsf-epsdirnam,
      v_erro          TYPE c,
      v_lote          TYPE char10,
      v_version       TYPE zlest0008-idctrl,
      v_cont          TYPE zlest0008-cont,
      v_lifnr         TYPE lfa1-lifnr.
*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_mask_def(6)  TYPE c VALUE '*.*',
           c_u            TYPE c VALUE 'U',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S',
           c_w            TYPE c VALUE 'W',
           c_email1(14)   TYPE c VALUE 'ZLES_EMAIL_ARQ',
           c_email2(14)   TYPE c VALUE 'ZLES_EMAIL_ER1',
           c_nfcli(5)     TYPE c VALUE 'NFCLI',
           c_desp(4)      TYPE c VALUE 'DESP',
           c_vagao(5)     TYPE c VALUE 'VAGAO',
           c_fatu(4)      TYPE c VALUE 'FATU',
           c_nf(2)        TYPE c VALUE 'NF',
           c_dcl(3)       TYPE c VALUE 'DCL',
           c_vag(3)       TYPE c VALUE 'VAG',
           c_fat(3)       TYPE c VALUE 'FAT'.
*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK e WITH FRAME TITLE text-005.
PARAMETERS: r_nf   AS CHECKBOX DEFAULT 'X',
            r_desp AS CHECKBOX DEFAULT 'X',
            r_vag  AS CHECKBOX DEFAULT 'X',
            r_fat  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK e.
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE text-002.
PARAMETERS: p_input(60) TYPE c MODIF ID fil,
            p_proc(60)  TYPE c MODIF ID fil,
            p_log(60)   TYPE c MODIF ID fil,
            p_chkso(1)  TYPE c NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b.
SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE text-003.
PARAMETERS: r_local RADIOBUTTON GROUP 1 USER-COMMAND scr DEFAULT 'X',
            r_unix RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK c.
SELECTION-SCREEN BEGIN OF BLOCK d WITH FRAME TITLE text-004.
SELECT-OPTIONS: s_dest FOR adr6-smtp_addr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK d.

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

AT SELECTION-SCREEN ON BLOCK e.

  IF r_nf IS INITIAL  AND
    r_desp IS INITIAL AND
    r_vag IS INITIAL  AND
    r_fat IS INITIAL.
    MESSAGE e000(zles) WITH 'Selecionar opção de Controle de Arquivo'.
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

  PERFORM preenche_caminho USING c_ent CHANGING p_input.
  IF v_erro = c_x.
    CLEAR: v_erro.
  ENDIF.

  PERFORM preenche_caminho USING c_log CHANGING p_log.
  IF v_erro = c_x.
    CLEAR: v_erro.
  ENDIF.

  PERFORM preenche_caminho USING c_proc CHANGING p_proc.
  IF v_erro = c_x.
    CLEAR: v_erro.
  ENDIF.

  IF p_input IS INITIAL OR p_log  IS INITIAL OR p_proc IS INITIAL.
    CLEAR: p_chkso.
    IF r_unix = c_x.
      MESSAGE s003 DISPLAY LIKE c_e.
    ELSE.
      MESSAGE s004 DISPLAY LIKE c_e.
    ENDIF.
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

  CLEAR: p_chkso,
         t_zlest0007[].

* Obtem Localização dos arquivos a serem processado
  SELECT SINGLE * FROM zlest0007
    INTO  st_zlest0007
    WHERE id_interface = sy-repid
      AND id_ctg = c_ent
      AND vlde <= sy-datum
      AND vlate >= sy-datum.

  IF sy-subrc IS INITIAL.
    APPEND st_zlest0007 TO t_zlest0007.
  ELSE.
    MESSAGE s026 WITH c_ent 'ZLES0009' DISPLAY LIKE c_e.
    v_erro = c_x.
    EXIT.
  ENDIF.

  IF v_erro IS INITIAL.
    SELECT SINGLE * FROM zlest0007
       INTO  st_zlest0007
       WHERE id_interface = sy-repid
        AND id_ctg = c_log
         AND vlde <= sy-datum
         AND vlate >= sy-datum.
    IF sy-subrc IS INITIAL.
      APPEND st_zlest0007 TO t_zlest0007.
    ELSE.
      MESSAGE s026 WITH c_log 'ZLES0009' DISPLAY LIKE c_e.
      v_erro = c_x.
      EXIT.
    ENDIF.
  ENDIF.

  IF v_erro IS INITIAL.
    SELECT SINGLE * FROM zlest0007
       INTO  st_zlest0007
       WHERE id_interface = sy-repid
         AND id_ctg = c_proc
         AND vlde <= sy-datum
         AND vlate >= sy-datum.
    IF sy-subrc IS INITIAL.
      APPEND st_zlest0007 TO t_zlest0007.
    ELSE.
      MESSAGE s026 WITH c_proc 'ZLES0009' DISPLAY LIKE c_e.
      v_erro = c_x.
      EXIT.
    ENDIF.
  ENDIF.

* Obtem prefixo para nome de arquivos a serem processado
  SELECT *
    FROM zlest0007
    APPENDING TABLE  t_zlest0007
   WHERE id_interface = sy-repid
     AND id_ctg IN (c_nfcli, c_desp, c_vagao, c_fatu)
     AND vlde <= sy-datum
     AND vlate >= sy-datum.

ENDFORM.                    " SELECIONA_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
FORM preenche_caminho  USING    v_categ
                       CHANGING v_path.

  READ TABLE t_zlest0007 INTO st_zlest0007 WITH KEY id_ctg = v_categ.
  CHECK sy-subrc IS INITIAL.

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

  CLEAR st_zlest0007.

ENDFORM.                    " PREENCHE_CAMINHO

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO
*&---------------------------------------------------------------------*
FORM le_diretorio .

  DATA: v_index         TYPE sy-tabix,
        v_mask_unix     TYPE epsfilnam,
        v_mask_locl(60) TYPE c,
        v_mask          TYPE string,
        v_ctrl,
        v_erro_log.

  CHECK: NOT p_input IS INITIAL
     AND NOT p_proc  IS INITIAL
     AND NOT p_log   IS INITIAL.

  CLEAR: v_erro_log.
  REFRESH t_0009_aux.

* Gera o prefixo para seleção de arquivo no diretório
  PERFORM gera_mask_arquivo USING '1' CHANGING v_mask.
  PERFORM gera_mask_arquivo USING '2' CHANGING v_mask.
  PERFORM gera_mask_arquivo USING '3' CHANGING v_mask.
  PERFORM gera_mask_arquivo USING '4' CHANGING v_mask.

  IF v_mask IS INITIAL OR STRLEN( v_mask ) > 40.
    MESSAGE s000
       WITH 'Não foi possível determinar os prefixos para arquivos'
            'de controle assinalados.'
       DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF r_unix = c_x.

*   v_mask_unix = v_mask. "Mais que uma mascara não funciona na funcão
    v_mask_unix = '*.*'.

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

*   Elimina arquivo que não combina com a mascara
    v_mask_unix = v_mask.
    IF NOT t_dir_unix[] IS INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '\W{1,}\b' IN v_mask
                                  WITH '|' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '\W{1,}$' IN v_mask
                                  WITH '' IGNORING CASE.
      LOOP AT t_dir_unix INTO st_files_unix.
        v_index = sy-tabix.
        FIND FIRST OCCURRENCE OF REGEX v_mask IN st_files_unix-name
                   IGNORING CASE.
        IF sy-subrc <> 0.
          DELETE t_dir_unix INDEX v_index.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF t_dir_unix[] IS INITIAL.
      MESSAGE s000
         WITH 'Diretório Unix: ' p_input
              ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
              v_mask_unix
         DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ELSE.

      SELECT filename idctrl FROM zlest0008
        INTO TABLE t_zlest0008
         FOR ALL ENTRIES IN t_dir_unix
       WHERE filename = t_dir_unix-name.

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

      SELECT *
        FROM zlest0009
        INTO TABLE t_zlest0009
         FOR ALL ENTRIES IN t_dir_unix
       WHERE filename = t_dir_unix-name.

      LOOP AT t_dir_unix INTO st_files_unix.
        v_index = sy-tabix.
        CLEAR v_erro.
        PERFORM verifica_bloqueio USING st_files_unix-name.
        IF v_erro = c_x.
          DELETE t_dir_unix INDEX v_index.
          v_erro_log = c_x.
        ENDIF.
      ENDLOOP.

*     Consiste arquivo bloqueados
      LOOP AT t_dir_unix INTO st_files_unix.

        PERFORM obtem_prefixo_arquivo USING st_files_unix-name
                                   CHANGING v_ctrl.
        CHECK v_ctrl BETWEEN '1' AND '4'.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZLOTE'
          IMPORTING
            number      = v_lote.

        CLEAR v_erro.
        PERFORM carrega_arq USING st_files_unix-name c_u v_ctrl.
        IF NOT v_erro IS INITIAL.
          v_erro_log = c_x.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ELSEIF r_local = c_x.

*   v_mask_locl = v_mask. "Mais que uma mascara não funciona na funcão
    v_mask_locl = '*.*'.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        directory        = p_input
        filter           = v_mask_locl
*   IMPORTING
*     FILE_COUNT       =
*     DIR_COUNT        =
      TABLES
        file_table       = t_dir_loc_f
        dir_table        = t_dir_local
   EXCEPTIONS
     cntl_error       = 1
     OTHERS           = 2.

*   Elimina arquivo que não combina com a mascara
    v_mask_locl = v_mask.
    IF NOT t_dir_loc_f[] IS INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '\W{1,}\b' IN v_mask
                                  WITH '|' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '\W{1,}$' IN v_mask
                                  WITH '' IGNORING CASE.
      LOOP AT t_dir_loc_f INTO st_files_doc.
        v_index = sy-tabix.
        FIND FIRST OCCURRENCE OF REGEX v_mask IN st_files_doc-pathname
                              IGNORING CASE.
        IF sy-subrc <> 0.
          DELETE t_dir_loc_f INDEX v_index.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF t_dir_loc_f[] IS INITIAL.
      MESSAGE s000
        WITH 'Diretório Local: ' p_input
             ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
             v_mask_locl
        DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ELSE.

      SELECT *
        FROM zlest0008
        INTO TABLE t_zlest0008
         FOR ALL ENTRIES IN t_dir_loc_f
       WHERE filename = t_dir_loc_f-pathname(40).

      SORT t_zlest0008 BY filename ASCENDING
                            idctrl DESCENDING.
      DELETE ADJACENT DUPLICATES FROM t_zlest0008 COMPARING filename.

      SELECT *
        FROM zlest0009
        INTO TABLE t_zlest0009
         FOR ALL ENTRIES IN t_dir_loc_f
       WHERE filename = t_dir_loc_f-pathname(40).

*     Bloqueia todos registro antes da consistência
      LOOP AT t_dir_loc_f INTO st_files_doc.
        v_index = sy-tabix.
        CLEAR v_erro.
        PERFORM verifica_bloqueio USING st_files_doc-pathname.
        IF v_erro = c_x.
          DELETE t_dir_loc_f INDEX v_index.
          v_erro_log = c_x.
        ENDIF.
      ENDLOOP.

*     Consiste arquivos não bloqueados
      LOOP AT t_dir_loc_f INTO st_files_doc.

        PERFORM obtem_prefixo_arquivo USING st_files_doc-pathname
                                   CHANGING v_ctrl.
        CHECK v_ctrl BETWEEN '1' AND '4'.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZLOTE'
          IMPORTING
            number      = v_lote.

        CLEAR v_erro.
        PERFORM carrega_arq USING st_files_doc-pathname c_l v_ctrl.
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
       WITH 'Existem arquivos/registros que não foram processados,'
            'verificar transaçãö LOG ZLES0010'
       DISPLAY LIKE c_e.
    EXIT.
  ENDIF.

ENDFORM.                    " LE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  GERA_MASK_ARQUIVO
*&---------------------------------------------------------------------*
FORM gera_mask_arquivo  USING value(p_ctrl_arq)
                     CHANGING p_prefixo_arq.

  DATA: v_mask          TYPE string,
        v_ctg           TYPE zid_ctg,
        v_prefix        TYPE zprefix,
        v_index         TYPE i,
        v_tipo.

  CASE p_ctrl_arq.
    WHEN '1'.
      IF r_nf = c_x.
        v_ctg = c_nfcli.
        v_prefix = c_nf.
        v_tipo = '1'.
      ENDIF.
    WHEN '2'.
      IF r_desp = c_x.
        v_ctg = c_desp.
        v_prefix = c_dcl.
        v_tipo = '2'.
      ENDIF.
    WHEN '3'.
      IF r_vag = c_x.
        v_ctg = c_vagao.
        v_prefix = c_vag.
        v_tipo = '3'.
      ENDIF.
    WHEN '4'.
      IF r_fat = c_x.
        v_ctg = c_fatu.
        v_prefix = c_fat.
        v_tipo = '4'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  CHECK: NOT v_ctg IS INITIAL.

  READ TABLE t_zlest0007 INTO st_zlest0007 WITH KEY id_ctg = v_ctg.
  v_index = sy-tabix.

  IF sy-subrc IS INITIAL.

    IF p_prefixo_arq IS INITIAL.
      CONCATENATE st_zlest0007-prefix c_mask_def INTO  p_prefixo_arq.
    ELSE.
      CONCATENATE p_prefixo_arq ',' st_zlest0007-prefix c_mask_def
             INTO p_prefixo_arq.
    ENDIF.

    CONCATENATE v_tipo '-' sy-repid INTO st_zlest0007-id_interface.
    MODIFY t_zlest0007 FROM st_zlest0007 INDEX v_index
               TRANSPORTING id_interface.

  ELSE.

    IF p_prefixo_arq IS INITIAL.
      CONCATENATE v_prefix c_mask_def INTO  p_prefixo_arq.
    ELSE.
      CONCATENATE p_prefixo_arq ',' v_prefix c_mask_def
             INTO p_prefixo_arq.
    ENDIF.

    CONCATENATE v_tipo '-' sy-repid INTO st_zlest0007-id_interface.
    st_zlest0007-id_ctg       = v_ctg.
    st_zlest0007-prefix       = v_prefix.
    st_zlest0007-vlde         = sy-datum.
    st_zlest0007-vlate        = sy-datum.
    st_zlest0007-data         = sy-datum.
    st_zlest0007-hora         = sy-uzeit.
    st_zlest0007-usuario      = sy-uname.
    APPEND st_zlest0007      TO t_zlest0007.

  ENDIF.

ENDFORM.                    " GERA_MASK_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  OBTEM_PREFIXO_ARQUIVO
*&---------------------------------------------------------------------*
FORM obtem_prefixo_arquivo  USING p_filename
                         CHANGING tipo_ctrl.

  DATA: l_tam     TYPE i,
        l_upper   TYPE zprefix,
        l_lower   TYPE zprefix.

  CLEAR tipo_ctrl.

  LOOP AT t_zlest0007 INTO zlest0007.

    l_tam = STRLEN( zlest0007-prefix ).
    CHECK l_tam > 0.
    l_upper = zlest0007-prefix.
    l_lower = zlest0007-prefix..
    TRANSLATE l_upper TO UPPER CASE.
    TRANSLATE l_lower TO LOWER CASE.
    IF p_filename(l_tam) = l_upper OR p_filename(l_tam) = l_lower.
      tipo_ctrl = zlest0007-id_interface(1).
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " OBTEM_PREFIXO_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
FORM carrega_arq  USING    v_file
                           v_tipo
                           v_ctrl.

  DATA: v_caminho TYPE string,
        v_aux(10) TYPE c,
        v_dia(2),
        v_mes(2),
        v_ano(4).

  REFRESH t_file.
  CONCATENATE p_input v_file INTO v_caminho.

  IF v_tipo = c_u.

    OPEN DATASET v_caminho FOR INPUT IN BINARY MODE.
    DO.
      READ DATASET v_caminho INTO t_file.
      IF sy-subrc  IS INITIAL.
        APPEND t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ELSEIF v_tipo = c_l.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = v_caminho
        filetype                = 'ASC'
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

* Elimina controle de quebra
  DELETE t_file WHERE linha(3) <= space.

  REFRESH t_email0008.
  CLEAR t_email0008.

  IF v_ctrl = '1'.
    PERFORM trata_nf_file USING v_file.
  ENDIF.
  IF v_ctrl = '2'.
    PERFORM trata_des_file USING v_file.
  ENDIF.
  IF v_ctrl = '3'.
    PERFORM trata_vag_file USING v_file.
  ENDIF.
  IF v_ctrl = '4'.
    PERFORM trata_fat_file USING v_file.
  ENDIF.

  IF t_email0008[] IS INITIAL.
*   Processamento executado com sucesso
    PERFORM transfere_file USING p_input
                                 p_proc
                                 v_tipo
                                 v_file.

  ELSE.
*   Processamento executado com erro
    PERFORM f_email USING 'E' v_file.

    PERFORM transfere_file USING p_input
                                 p_log
                                 v_tipo
                                 v_file.
  ENDIF.

ENDFORM.                    " CARREGA_ARQ

*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
FORM transfere_file  USING    v_de
                              v_para
                              v_tipo
                              v_file.

  DATA: v_dest    TYPE rlgrap-filename,
        v_sour    TYPE rlgrap-filename,
        v_dest1   TYPE string,
        v_sour1   TYPE string,
        v_rc_bool TYPE c,
        v_rc_num  TYPE i.

  CONCATENATE v_para v_file INTO v_dest.
  CONCATENATE v_de v_file   INTO v_sour.

* Gera o Path para check e transferência
  v_dest1 = v_para.

  IF v_tipo = c_u.

    OPEN DATASET v_dest FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc IS INITIAL.
      LOOP AT t_file.
        TRANSFER t_file TO v_dest.
      ENDLOOP.
    ENDIF.
    CLOSE DATASET v_dest.
    DELETE DATASET v_de.

  ELSEIF v_tipo = c_l.

*   Verifica a existência do diretório
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory = v_dest1
      RECEIVING
        result    = v_rc_bool.

*   Cria o diretório para transferência
    IF v_rc_bool IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>directory_create
        EXPORTING
          directory = v_dest1
        CHANGING
          rc        = v_rc_num.
    ENDIF.

*   Transfere o arquivo para o diretório
    v_dest1 = v_dest.
    v_sour1 = v_sour.
    CALL METHOD cl_gui_frontend_services=>file_copy
      EXPORTING
        SOURCE      = v_sour1
        DESTINATION = v_dest1
        overwrite   = 'X'.

*   Elimina o arquivo de origem
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename = v_sour1
      CHANGING
        rc       = v_rc_num.

  ENDIF.

ENDFORM.                    " TRANSFERE_FILE

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_BLOQUEIO
*&---------------------------------------------------------------------*
FORM verifica_bloqueio USING v_name.

  DATA: v_version TYPE zlest0008-idctrl.

  READ TABLE t_zlest0009 INTO st_zlest0009 WITH KEY filename = v_name.
  IF NOT sy-subrc IS INITIAL.

    st_zlest0009-filename = v_name.
    st_zlest0009-data = sy-datum.
    st_zlest0009-hora = sy-uzeit.
    st_zlest0009-usuario = sy-uname.

    APPEND: st_zlest0009 TO t_zlest0009,
            st_zlest0009 TO t_0009_aux.

    INSERT zlest0009 FROM st_zlest0009.
    CLEAR st_zlest0009.

  ELSE.

    CLEAR st_zlest0008.
    READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = v_name.
    v_version = st_zlest0008-idctrl + 1.
    v_cont = v_cont + 1.
    st_mess-filename = v_name.
    st_mess-idctrl = v_version.
    st_mess-tcode = sy-tcode.
    st_mess-cont = v_cont.
    st_mess-dyname = 'LES'.
    st_mess-msgtyp = c_e.
    st_mess-msgspra = 'PT'.
    st_mess-msgid = 'FR'.
    st_mess-msgnr = '09'.
    st_mess-msgv1 = 'Arquivo ja esta sendo processado'.
    st_mess-data = sy-datum.
    st_mess-hora = sy-uzeit.
    st_mess-usuario = sy-uname.

    INSERT zlest0008 FROM st_mess.
    CLEAR st_mess.
    v_erro = c_x.

  ENDIF.

ENDFORM.                    " VERIFICA_BLOQUEIO

*&---------------------------------------------------------------------*
*&      Form  f_email
*&---------------------------------------------------------------------*
*       Preenche dados do E-mail e Envia
*----------------------------------------------------------------------*
FORM f_email USING p_type v_filename.

  DATA: v_nrlinha         TYPE i,
        v_datum(10)       TYPE c,
        v_uzeit(8)        TYPE c,
        v_linha255        TYPE char255,
        vl_stand_text     LIKE  thead-tdname.

  DATA: tl_line      LIKE tline OCCURS 0 WITH HEADER LINE.

  CHECK: NOT s_dest[] IS INITIAL.

  REFRESH: t_email_plist,
           t_email_textos,
           t_obj_header,
           t_email_rcvrs.

  CLEAR: t_email_plist,
         t_email_textos,
         t_obj_header,
         t_email_rcvrs.

* Destinatário de E-mails
  LOOP AT s_dest.
    t_email_rcvrs-receiver  = s_dest-low.
    t_email_rcvrs-express   = c_x.
    t_email_rcvrs-rec_type  = 'U'.
    t_email_rcvrs-com_type = 'INT'.
    t_email_rcvrs-notif_del = c_x.
    t_email_rcvrs-notif_ndel = c_x.
    APPEND t_email_rcvrs.
  ENDLOOP.

  SORT t_email_rcvrs BY receiver.
  DELETE ADJACENT DUPLICATES FROM t_email_rcvrs
                  COMPARING receiver.

* Identifica a chave do texto que contem o corpo do E-mail
* Nota: 1) Os textos foram criadoa através da transação standard SO10
*       2) O transporte/criação de request através do programa RSTXTRAN
  IF p_type = c_s.
    "  Texto não criado para sucesso!!!!!
    vl_stand_text = c_email1.
  ELSEIF p_type = c_e.
    vl_stand_text = c_email2.
  ENDIF.

* Busca texto para o E-mail
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'ST'
      language                = 'P'
      name                    = vl_stand_text
      object                  = 'TEXT'
    TABLES
      lines                   = tl_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  LOOP AT tl_line.

    t_email_textos = tl_line-tdline.

    IF t_email_textos CS '&ARQ&'.
      REPLACE '&ARQ&' WITH v_filename INTO t_email_textos.
      CONDENSE t_email_textos.
    ELSEIF t_email_textos CS '&DATA&'.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)
             INTO v_datum SEPARATED BY '/'.
      REPLACE '&DATA&' WITH v_datum INTO t_email_textos.
      CONDENSE t_email_textos.
    ELSEIF t_email_textos CS '&HORA&'.
      CONCATENATE sy-uzeit(2) sy-uzeit+2(2) sy-uzeit+4(2)
             INTO v_uzeit SEPARATED BY ':'.
      REPLACE '&HORA&' WITH v_uzeit INTO t_email_textos.
      CONDENSE t_email_textos.
    ELSEIF t_email_textos CS '&USER&'.
      REPLACE '&USER&' WITH sy-uname INTO t_email_textos.
      CONDENSE t_email_textos.
    ENDIF.

    APPEND t_email_textos. CLEAR t_email_textos.

  ENDLOOP.

  IF NOT t_email0008[] IS INITIAL.
    t_email_textos = 'Inconsistências:'.
    APPEND t_email_textos.
    CLEAR t_email_textos.
    APPEND t_email_textos.
    APPEND t_email_textos.
    LOOP AT t_email0008 INTO st_mess.
      t_email_textos = st_mess-msgv1.
      APPEND t_email_textos. CLEAR t_email_textos.
    ENDLOOP.
    CLEAR st_mess.
  ENDIF.

* Seta dados gerais do email
  DESCRIBE TABLE t_email_textos LINES v_nrlinha.
  READ TABLE t_email_textos INDEX v_nrlinha.

* Dados gerais do email
  st_doc-doc_size = ( v_nrlinha - 1 ) * 255 + STRLEN( t_email_textos ).
  st_doc-obj_name = 'SAPRPT'.
  st_doc-sensitivty = 'F'.
  st_doc-obj_langu = sy-langu.
  st_doc-obj_descr = text-t00.

  CLEAR: t_email_plist.
  t_email_plist-transf_bin = space.
  t_email_plist-head_start = 1.
  t_email_plist-head_num = 0.
  t_email_plist-body_start = 1.
  t_email_plist-body_num = v_nrlinha.
  t_email_plist-doc_type = 'RAW'.
  APPEND t_email_plist.

* Envia o e-mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = st_doc
      put_in_outbox              = c_x
      commit_work                = c_x
    TABLES
      packing_list               = t_email_plist
      object_header              = t_obj_header
      contents_txt               = t_email_textos
      receivers                  = t_email_rcvrs
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. " f_email

*&---------------------------------------------------------------------*
*&      Form  TRATA_NF_FILE
*&---------------------------------------------------------------------*
FORM trata_nf_file USING v_file.

  DATA: vl_ok             TYPE char1,
        qtde(10)          TYPE c,
        vl_branch         TYPE j_1bbranch-branch,
        vl_bukrs          TYPE j_1bbranch-bukrs,
        vl_stcd1          TYPE j_1bbranch-stcd1,
        vl_nf_ant         TYPE zlest0003-nr_nf,
        nf_depara         TYPE c LENGTH 1,
        st_zlest0003_aux  TYPE zlest0003.

  READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = v_file.
  IF sy-subrc IS INITIAL.
    v_version = st_zlest0008-idctrl + 1.
  ELSE.
    v_version = 1.
  ENDIF.

  LOOP AT t_file.

    MOVE: t_file-linha(3)    TO st_zlest0003-serie_despacho,
          t_file-linha+3(6)  TO st_zlest0003-nr_despacho,
          t_file-linha+9(9)  TO st_zlest0003-nr_nf,
          t_file-linha+24(8) TO st_zlest0003-qtde_rateio,
          v_file             TO st_zlest0003-arq_ori,
          v_lote             TO st_zlest0003-lote,
          sy-datum           TO st_zlest0003-data,
          sy-uzeit           TO st_zlest0003-hora,
          sy-uname           TO st_zlest0003-usuario.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = st_zlest0003-nr_nf
      IMPORTING
        output = st_zlest0003-nr_nf.

    nf_depara = 'N'.

    "NR NF Terceiro
    SELECT SINGLE *
      FROM zlest0004
      INTO st_zlest0004
     WHERE nr_despacho    = st_zlest0003-nr_despacho
       AND serie_despacho = st_zlest0003-serie_despacho.

    IF sy-subrc IS INITIAL.
      "Verifica se Possui Local de Negócios para Este CNPJ
      SELECT SINGLE bukrs branch stcd1
        INTO (vl_bukrs, vl_branch, vl_stcd1)
        FROM j_1bbranch
       WHERE stcd1 = st_zlest0004-cgc_remetente.

      IF NOT sy-subrc IS INITIAL .

        SELECT SINGLE *
          INTO st_lfa1
          FROM lfa1
         WHERE stcd1 EQ st_zlest0004-cgc_remetente .

        SELECT SINGLE *
          FROM zlest0041
          INTO st_zlest0041
         WHERE cod_cliente EQ st_lfa1-lifnr
           AND nr_nf       EQ st_zlest0003-nr_nf.


        IF sy-subrc  IS INITIAL.

          SELECT SINGLE *
            INTO st_zlest0003_aux
            FROM zlest0003
           WHERE serie_despacho = st_zlest0003-serie_despacho
             AND nr_despacho    = st_zlest0003-nr_despacho
             AND nr_nf          = st_zlest0003-nr_nf.
          "Caso a nota seja de terceiro sera  atualizada

          IF sy-subrc IS INITIAL.

            nf_depara =  'S'.

            vl_nf_ant = st_zlest0003-nr_nf.

            UPDATE zlest0003 SET nr_nf = st_zlest0041-nr_nf_propria WHERE serie_despacho = st_zlest0003-serie_despacho
                                                                      AND nr_despacho    = st_zlest0003-nr_despacho
                                                                      AND nr_nf          = vl_nf_ant.
          ELSE.

            st_zlest0003-nr_nf = st_zlest0041-nr_nf_propria .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    IF nf_depara NE 'S'.
      APPEND st_zlest0003 TO t_zlest0003.
    ELSE.
      CLEAR : st_zlest0003.
    ENDIF.

  ENDLOOP.

  MODIFY zlest0003 FROM TABLE t_zlest0003.

  v_cont = v_cont + 1.
  st_mess-filename = v_file.
  st_mess-idctrl = v_version.
  st_mess-tcode = sy-tcode.
  st_mess-cont = v_cont.
  st_mess-dyname = 'LES'.
  st_mess-msgtyp = c_s.
  st_mess-msgspra = 'PT'.
  st_mess-msgid = 'FR'.
  st_mess-msgnr = '05'.
  st_mess-msgv1 = 'Nota fiscal de Cliente processado com sucesso'.
  st_mess-data = sy-datum.
  st_mess-hora = sy-uzeit.
  st_mess-usuario = sy-uname.
  st_mess-lote = v_lote.

  INSERT zlest0008 FROM st_mess.

  CLEAR: st_mess,
         st_zlest0003.

ENDFORM.                    " TRATA_NF_FILE

*&---------------------------------------------------------------------*
*&      Form  TRATA_FAT_FILE
*&---------------------------------------------------------------------*
FORM trata_fat_file USING v_file.

  DATA: v_aux(18) TYPE c,
        v_index   TYPE sy-tabix,
        vl_branch TYPE j_1bbranch-branch,
        vl_bukrs  TYPE j_1bbranch-bukrs,
        vl_stcd1  TYPE j_1bbranch-stcd1,
        vl_nf_ant TYPE zlest0003-nr_nf.

  READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = v_file.
  IF sy-subrc IS INITIAL.
    v_version = st_zlest0008-idctrl + 1.
  ELSE.
    v_version = 1.
  ENDIF.

  LOOP AT t_file.

    MOVE: t_file-linha(3)    TO st_zlest0006-serie_nf_all,
          t_file-linha+3(6)  TO st_zlest0006-nr_nf_all,
          t_file-linha+9(10) TO st_zlest0006-nr_fatura.
    CONCATENATE t_file-linha+23(4)
                t_file-linha+21(2)
                t_file-linha+19(2) INTO st_zlest0006-emissao.
    CONCATENATE t_file-linha+31(4)
                t_file-linha+29(2)
                t_file-linha+27(2) INTO st_zlest0006-vencimento.
    CONCATENATE t_file-linha+35(15) '.'
                t_file-linha+50(2) INTO v_aux.
    st_zlest0006-vlr_fatura = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+53(14) '.'
                t_file-linha+67(2) INTO v_aux.
    st_zlest0006-base_calculo = v_aux.
    CLEAR v_aux.
    MOVE: t_file-linha+69(2) TO st_zlest0006-aliquota.
    CONCATENATE t_file-linha+72(10) '.' t_file-linha+82(2) INTO v_aux.
    st_zlest0006-vlr_icms = v_aux.
    CLEAR v_aux.
    MOVE: t_file-linha+84(14) TO st_zlest0006-cgc_all,
          t_file-linha+98(14) TO st_zlest0006-cgc_cliente,
          t_file-linha+112(3) TO st_zlest0006-cfop,
          t_file-linha+115(3) TO st_zlest0006-banco,
          v_file              TO st_zlest0006-arq_ori,
          v_lote              TO st_zlest0006-lote,
          sy-datum            TO st_zlest0006-data,
          sy-uzeit            TO st_zlest0006-hora,
          sy-uname            TO st_zlest0006-usuario,
          v_lote              TO st_zlest0006-lote,
          'L'                 TO st_zlest0006-status,
          sy-datum            TO st_zlest0006-data,
          sy-uzeit            TO st_zlest0006-hora,
          sy-uname            TO st_zlest0006-usuario.

    APPEND st_zlest0006 TO t_zlest0006.

    SELECT SINGLE nr_fatura
      FROM zlest0006
      INTO st_zlest0006-nr_fatura
    WHERE  nr_fatura EQ st_zlest0006-nr_fatura
      AND  status    EQ 'B'.
    IF sy-subrc IS INITIAL.
      MESSAGE e836(sd) WITH text-006.
      LEAVE LIST-PROCESSING.
    ENDIF.

*   Caso queira gravar erro para envio de e-mail
*   Alimentar a tabela abaixo.

*   t_email0008
*   v_erro = c_x.

    "NR NF Terceiro
    SELECT *
      FROM zlest0004
      INTO TABLE t_zlest0004
     WHERE nr_fatura EQ st_zlest0006-nr_fatura.

    LOOP AT t_zlest0004 INTO st_zlest0004.
      "Verifica se Possui Local de Negócios para Este CNPJ
      SELECT SINGLE bukrs branch stcd1
        INTO (vl_bukrs, vl_branch, vl_stcd1)
        FROM j_1bbranch
       WHERE stcd1 = st_zlest0004-cgc_remetente.

      IF NOT sy-subrc IS INITIAL .

        SELECT *
          INTO TABLE t_zlest0003
          FROM zlest0003
         WHERE serie_despacho EQ st_zlest0004-serie_despacho
           AND nr_despacho    EQ st_zlest0004-nr_despacho.

        LOOP AT t_zlest0003 INTO st_zlest0003.
          SELECT SINGLE *
            INTO st_lfa1
            FROM lfa1
           WHERE stcd1 EQ st_zlest0004-cgc_remetente .

          SELECT SINGLE *
            FROM zlest0041
            INTO st_zlest0041
           WHERE cod_cliente EQ st_lfa1-lifnr
             AND nr_nf       EQ st_zlest0003-nr_nf.


          vl_nf_ant = st_zlest0003-nr_nf.

          UPDATE zlest0003 SET nr_nf = st_zlest0041-nr_nf_propria WHERE serie_despacho = st_zlest0003-serie_despacho
                                                                    AND nr_despacho    = st_zlest0003-nr_despacho
                                                                    AND nr_nf          = vl_nf_ant.

          COMMIT WORK.
          "st_zlest0003-nr_nf = st_zlest0041-nr_nf_propria .

          "MODIFY t_zlest0003 FROM st_zlest0003.
        ENDLOOP.

      ENDIF.
    ENDLOOP.
    "MODIFY zlest0003 FROM TABLE t_zlest0003.

  ENDLOOP.


  MODIFY zlest0006 FROM TABLE t_zlest0006.

  v_cont = v_cont + 1.
  st_mess-filename = v_file.
  st_mess-idctrl = v_version.
  st_mess-tcode = sy-tcode.
  st_mess-cont = v_cont.
  st_mess-dyname = 'LES'.
  st_mess-msgtyp = c_s.
  st_mess-msgspra = 'PT'.
  st_mess-msgid = 'FR'.
  st_mess-msgnr = '05'.
  st_mess-msgv1 = 'Fatura processado com sucesso'.
  st_mess-data = sy-datum.
  st_mess-hora = sy-uzeit.
  st_mess-usuario = sy-uname.
  st_mess-lote = v_lote.

  INSERT zlest0008 FROM st_mess.

  CLEAR: st_mess,
         st_zlest0006.

ENDFORM.                    " TRATA_FAT_FILE

*&---------------------------------------------------------------------*
*&      Form  TRATA_DES_FILE
*&---------------------------------------------------------------------*
FORM trata_des_file USING v_file.

  DATA: v_aux(17) TYPE c    ,
        vl_ok     TYPE char1,
        vl_branch TYPE j_1bbranch-branch,
        vl_bukrs  TYPE j_1bbranch-bukrs,
        vl_stcd1  TYPE j_1bbranch-stcd1,
        vl_nf_ant TYPE zlest0003-nr_nf.

  READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = v_file.
  IF sy-subrc IS INITIAL.
    v_version = st_zlest0008-idctrl + 1.
  ELSE.
    v_version = 1.
  ENDIF.

  LOOP AT t_file.

    MOVE: t_file-linha(3)    TO st_zlest0004-serie_despacho,
          t_file-linha+3(6)  TO st_zlest0004-nr_despacho.
    CONCATENATE t_file-linha+13(4)
                t_file-linha+11(2)
                t_file-linha+9(2) INTO st_zlest0004-emissao.
    MOVE: t_file-linha+17(5) TO st_zlest0004-origem,
          t_file-linha+22(5) TO st_zlest0004-destino,
          t_file-linha+27(3) TO st_zlest0004-mercadoria.
    CONCATENATE t_file-linha+30(5) '.' t_file-linha+35(2) INTO v_aux.
    st_zlest0004-peso = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+37(11) '.' t_file-linha+48(2) INTO v_aux.
    st_zlest0004-tarifa = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+50(11) '.' t_file-linha+61(2) INTO v_aux.
    st_zlest0004-vlr_total_frete = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+63(11) '.' t_file-linha+74(2) INTO v_aux.
    st_zlest0004-vlr_total_taxas = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+76(15) '.' t_file-linha+91(2) INTO v_aux.
    st_zlest0004-vlr_total = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+93(15) '.' t_file-linha+108(2) INTO v_aux.
    st_zlest0004-base_calculo = v_aux.
    CLEAR v_aux.
    MOVE: t_file-linha+110(2) TO st_zlest0004-aliquota.
    CONCATENATE t_file-linha+112(11) '.' t_file-linha+123(2) INTO v_aux.
    st_zlest0004-vlr_icms = v_aux.
    CLEAR v_aux.
    MOVE: t_file-linha+125(10) TO st_zlest0004-nr_fatura,
          t_file-linha+135(14) TO st_zlest0004-cgc_remetente,
          t_file-linha+149(14) TO st_zlest0004-cgc_dest,
          v_file               TO st_zlest0004-arq_ori,
          v_lote               TO st_zlest0004-lote,
          sy-datum             TO st_zlest0004-data,
          sy-uzeit             TO st_zlest0004-hora,
          sy-uname             TO st_zlest0004-usuario.

*   Verifica Se Possui algum Faturamento para o Despacho
*    PERFORM z_verifica_fat USING st_zlest0004-serie_despacho
*                                 st_zlest0004-nr_despacho
*                        CHANGING vl_ok.
*
*    IF NOT vl_ok IS INITIAL.
*      MESSAGE e836(sd) WITH text-006.
*      LEAVE LIST-PROCESSING.
*    ENDIF.

    "Verifica se Possui Local de Negócios para Este CNPJ
    SELECT SINGLE bukrs branch stcd1
      INTO (vl_bukrs, vl_branch, vl_stcd1)
      FROM j_1bbranch
     WHERE stcd1 = st_zlest0004-cgc_remetente.

    IF NOT sy-subrc IS INITIAL .


      SELECT *
        INTO TABLE t_zlest0003
        FROM zlest0003
       WHERE serie_despacho EQ st_zlest0004-serie_despacho
         AND nr_despacho    EQ st_zlest0004-nr_despacho.

      LOOP AT t_zlest0003 INTO st_zlest0003.
        SELECT SINGLE *
          INTO st_lfa1
          FROM lfa1
         WHERE stcd1 EQ st_zlest0004-cgc_remetente .

        SELECT SINGLE *
          FROM zlest0041
          INTO st_zlest0041
         WHERE cod_cliente EQ st_lfa1-lifnr
           AND nr_nf       EQ st_zlest0003-nr_nf.

        "Caso a nota seja de terceiro sera  atualizada
        IF sy-subrc  IS INITIAL.


          UPDATE zlest0003 SET nr_nf = st_zlest0041-nr_nf_propria WHERE serie_despacho = st_zlest0003-serie_despacho
                                                                    AND nr_despacho    = st_zlest0003-nr_despacho
                                                                    AND nr_nf          = vl_nf_ant.

          COMMIT WORK.
          "st_zlest0003-nr_nf = st_zlest0041-nr_nf_propria .

          "MODIFY zlest0003 FROM st_zlest0003.

        ENDIF.

      ENDLOOP.



    ENDIF.



    APPEND st_zlest0004 TO t_zlest0004.
    "    Caso queira gravar erro para envio de e-mail
*   Alimentar a tabela abaixo.

*   t_email0008
*   v_erro = c_x.

  ENDLOOP.

  MODIFY zlest0004 FROM TABLE t_zlest0004.

  v_cont = v_cont + 1.
  st_mess-filename = v_file.
  st_mess-idctrl = v_version.
  st_mess-tcode = sy-tcode.
  st_mess-cont = v_cont.
  st_mess-dyname = 'LES'.
  st_mess-msgtyp = c_s.
  st_mess-msgspra = 'PT'.
  st_mess-msgid = 'FR'.
  st_mess-msgnr = '05'.
  st_mess-msgv1 = 'Despacho processado com sucesso'.
  st_mess-data = sy-datum.
  st_mess-hora = sy-uzeit.
  st_mess-usuario = sy-uname.
  st_mess-lote = v_lote.

  INSERT zlest0008 FROM st_mess.

  CLEAR: st_mess,
         st_zlest0004.

ENDFORM.                    " TRATA_DES_FILE

*&---------------------------------------------------------------------*
*&      Form  TRATA_VAG_FILE
*&---------------------------------------------------------------------*
FORM trata_vag_file USING v_file.

  DATA: v_aux(17) TYPE c    ,
        vl_ok     TYPE char1.

  READ TABLE t_zlest0008 INTO st_zlest0008 WITH KEY filename = v_file.
  IF sy-subrc IS INITIAL.
    v_version = st_zlest0008-idctrl + 1.
  ELSE.
    v_version = 1.
  ENDIF.

  LOOP AT t_file.
    MOVE: t_file-linha(3) TO st_zlest0005-serie_despacho,
          t_file-linha+3(6) TO st_zlest0005-nr_despacho,
          t_file-linha+9(7) TO st_zlest0005-nr_vagao.
    CONCATENATE t_file-linha+16(5) '.' t_file-linha+21(2) INTO v_aux.
    st_zlest0005-peso_calculo = v_aux.
    CLEAR v_aux.
    CONCATENATE t_file-linha+23(5) '.' t_file-linha+28(2) INTO v_aux.
    st_zlest0005-peso_real = v_aux.
    CLEAR v_aux.
    MOVE: t_file-linha+30(3) TO st_zlest0005-tipo_vagao,
          v_file             TO st_zlest0005-arq_ori,
          v_lote             TO st_zlest0005-lote,
          sy-datum           TO st_zlest0005-data,
          sy-uzeit           TO st_zlest0005-hora,
          sy-uname           TO st_zlest0005-usuario.

*   Verifica Se Possui algum Faturamento para o Despacho
*    PERFORM z_verifica_fat USING st_zlest0005-serie_despacho
*                                 st_zlest0005-nr_despacho
*                        CHANGING vl_ok.
*    IF NOT vl_ok IS INITIAL.
*      MESSAGE e836(sd) WITH text-006.
*      LEAVE LIST-PROCESSING.
*    ENDIF.

    APPEND st_zlest0005 TO t_zlest0005.

*   Caso queira gravar erro para envio de e-mail
*   Alimentar a tabela abaixo.

*   t_email0008
*   v_erro = c_x.

  ENDLOOP.

  MODIFY zlest0005 FROM TABLE t_zlest0005.

  v_cont = v_cont + 1.
  st_mess-filename = v_file.
  st_mess-idctrl = v_version.
  st_mess-tcode = sy-tcode.
  st_mess-cont = v_cont.
  st_mess-dyname = 'LES'.
  st_mess-msgtyp = c_s.
  st_mess-msgspra = 'PT'.
  st_mess-msgid = 'FR'.
  st_mess-msgnr = '05'.
  st_mess-msgv1 = 'Vagão processado com sucesso'.
  st_mess-data = sy-datum.
  st_mess-hora = sy-uzeit.
  st_mess-usuario = sy-uname.
  st_mess-lote = v_lote.

  INSERT zlest0008 FROM st_mess.

  CLEAR: st_mess,
         st_zlest0005.

ENDFORM.                    " TRATA_VAG_FILE

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_FAT                                           *
*&---------------------------------------------------------------------*
*        Verifica Se Possui algum Faturamento para o Despacho          *
*----------------------------------------------------------------------*
FORM z_verifica_fat USING p_serie_despacho TYPE zlest0003-serie_despacho
                          p_nr_despacho    TYPE zlest0003-nr_despacho
                 CHANGING p_ok             TYPE char1.

  DATA vl_fatura TYPE zlest0004-nr_fatura.

  CLEAR p_ok.

  SELECT SINGLE nr_fatura
    FROM zlest0004
    INTO vl_fatura
  WHERE  serie_despacho EQ p_serie_despacho
    AND  nr_despacho    EQ p_nr_despacho.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE nr_fatura
    FROM zlest0006
    INTO vl_fatura
  WHERE  nr_fatura EQ vl_fatura
    AND  status    EQ 'B'.

  CHECK sy-subrc IS INITIAL.

  p_ok = 'X'.

ENDFORM.                    " Z_VERIFICA_FAT
