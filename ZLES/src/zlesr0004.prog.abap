*$*$ -------------------------------------------------------------- *$*$
*$*$                    GRUPO ANDRÉ MAGGI                           *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Autor     : Robson Motta - BBKO Consulting                     *$*$
*$*$ Data      : 18/08/2009                                         *$*$
*$*$ Descrição : Lista Log de Processamento ZLEST0008               *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Data      :                                                    *$*$
*$*$ Autor     :                                                    *$*$
*$*$ Solicit.  :                                                    *$*$
*$*$ Chamado   :                                                    *$*$
*$*$ Descrição :                                                    *$*$
*$*$ Versão    :                                                    *$*$
*$*$ ---------------------------------------------------------------*$*$

REPORT  zlesr0004
        MESSAGE-ID zles
        NO STANDARD PAGE HEADING
        LINE-COUNT 65(1)
        LINE-SIZE  150.

***********************************************************************
* DECLARAÇÃO DE TABELAS                                               *
***********************************************************************
TABLES: zlest0008.

***********************************************************************
* DECLARAÇÃO DE RANGES                                                *
***********************************************************************
RANGES: rc_idctrl       FOR zlest0008-idctrl OCCURS 1.

***********************************************************************
* DECLARAÇÃO TIPOS                                                    *
***********************************************************************
TYPE-POOLS: slis.

TYPES: BEGIN OF type_alv,
         mark,
         filename	      TYPE epsfilnam,
         idctrl	        TYPE zidctrl,
         msgtyp	        TYPE bdc_mart,
         msgv1          TYPE bdc_vtext1,
         lote	          TYPE char10,
         data	          TYPE erdat,
         hora	          TYPE erzet,
         usuario        TYPE ernam,
       END OF type_alv,

        BEGIN OF y_file,
         linha(400),
       END OF y_file.

***********************************************************************
* DECLARAÇÃO DE TABELAS INTERNAS                                      *
***********************************************************************
DATA: yt_zlest0008      TYPE  TABLE OF zlest0008,
      yt_fieldcat       TYPE  slis_t_fieldcat_alv,
      yt_event          TYPE  slis_t_event,
      yt_listheader     TYPE  slis_t_listheader,
      t_zlest0007       TYPE TABLE OF zlest0007,
      st_zlest0007      TYPE zlest0007,
      v_prefix_ent      TYPE zlest0007-prefix,
      v_prefix_log      TYPE zlest0007-prefix,
      v_prefix_proc     TYPE zlest0007-prefix,
      wg_cont_r         TYPE sy-tabix.

DATA:  t_file         TYPE STANDARD TABLE OF y_file
                           WITH HEADER LINE INITIAL SIZE 0.

DATA: yt_saida          TYPE  STANDARD TABLE OF type_alv
                              WITH HEADER LINE.


***********************************************************************
* DECLARAÇÃO DE ESTRUTURAS                                            *
***********************************************************************
DATA: e_alv_layout      TYPE  slis_layout_alv,
      e_fieldcat        TYPE  slis_fieldcat_alv,
      e_event           TYPE  slis_alv_event,

      w_zlest0008       TYPE  zlest0008.

***********************************************************************
* DECLARAÇÃO DE CONSTANTES                                            *
***********************************************************************
CONSTANTS: cc_a(1)            TYPE c           VALUE 'A',
           cc_e(1)            TYPE c           VALUE 'E',
           cc_i(1)            TYPE c           VALUE 'I',
           cc_x(1)            TYPE c           VALUE 'X',
           c_l1(2)            TYPE c           VALUE 'L1',
           c_l2(2)            TYPE c           VALUE 'L2',
           c_l3(2)            TYPE c           VALUE 'L3',
           cc_eq(2)           TYPE c           VALUE 'EQ',
           c_log(10)          TYPE c           VALUE 'LOG',
           c_proc(10)         TYPE c           VALUE 'PROC',
           c_ent(10)          TYPE c           VALUE 'ENT'.

***********************************************************************
* DECLARAÇÃO DE VARIÁVEIS                                             *
***********************************************************************
DATA: vc_repid          TYPE  sy-repid        VALUE sy-repid,
      vc_msgerr         TYPE  bapi_msg,

      vc_formpfstatus   TYPE  slis_formname,
      vc_formusercomm   TYPE  slis_formname,
      vl_formtopofpag   TYPE  slis_formname,
      e_def_variant     TYPE  disvariant,
      e_variant         TYPE  disvariant.

***********************************************************************
* DEFINIÇÃO DA TELA DE SELEÇÃO                                        *
***********************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_data   FOR  zlest0008-data,
                s_file   FOR  zlest0008-filename NO-EXTENSION
                                                 NO INTERVALS.
PARAMETERS:     p_idctrl TYPE zlest0008-idctrl.
SELECT-OPTIONS                 p_user   FOR sy-uname NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2
               WITH FRAME TITLE text-002 NO INTERVALS.
PARAMETERS: p_vari  TYPE  disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN: END OF BLOCK b1.

***********************************************************************
* INITIALIZATION                                                      *
***********************************************************************
INITIALIZATION.

  vc_repid = sy-repid.
  PERFORM yf_alv_init_variant.


***********************************************************************
* AT SELECTION-SCREEN                                                 *
***********************************************************************
AT SELECTION-SCREEN.

* Display ALV variants.
  PERFORM yf_alv_display_variant.


***********************************************************************
* AT SELECTION-SCREEN ON                                              *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  PERFORM yf_alv_reuse_variant_f4.


***********************************************************************
* START-OF-SELECTION                                                  *
***********************************************************************
START-OF-SELECTION.

  PERFORM: yf_obtem_log_processamento,
           yf_chama_processo_alv.

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_LOG_PROCESSAMENTO
*&---------------------------------------------------------------------*
FORM yf_obtem_log_processamento.

  REFRESH rc_idctrl.

  IF NOT p_idctrl IS INITIAL.
    rc_idctrl-sign   = cc_i.
    rc_idctrl-option = cc_eq.
    rc_idctrl-low = p_idctrl.
    APPEND rc_idctrl.
  ENDIF.

  SELECT *
    INTO TABLE yt_zlest0008
    FROM zlest0008
   WHERE filename IN s_file
     AND idctrl   IN rc_idctrl
     AND data     IN s_data
     AND usuario  IN p_user.

*  Exibe mensagem de erro
  IF yt_zlest0008[] IS INITIAL.
    MESSAGE s000 DISPLAY LIKE cc_e WITH text-m01.
  ENDIF.

ENDFORM.                    " YF_OBTEM_LOG_PROCESSAMENTO

*&---------------------------------------------------------------------*
*&      Form  YF_CHAMA_PROCESSO_ALV
*&---------------------------------------------------------------------*
FORM yf_chama_processo_alv.

  CHECK: NOT yt_zlest0008[] IS INITIAL.

  PERFORM yf_monta_saida_alv.
  PERFORM yf_gera_laytgeral_alv.
  PERFORM yf_gera_cabecalho_alv.
  PERFORM yf_gera_linhasdet_alv.
  PERFORM yf_gera_rotevento_alv.

  PERFORM yf_exibe_relatorio_alv.

ENDFORM.                    " YF_CHAMA_PROCESSO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_SAIDA_ALV
*&---------------------------------------------------------------------*
FORM yf_monta_saida_alv.

  REFRESH: yt_saida.

* Transfere dados para saida ALV
  LOOP AT yt_zlest0008 INTO w_zlest0008.
    CLEAR: yt_saida.
    MOVE-CORRESPONDING w_zlest0008 TO yt_saida.
*   Anexa valores
    APPEND yt_saida.
  ENDLOOP.

ENDFORM.                    " YF_MONTA_SAIDA_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LAYTGERAL_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_laytgeral_alv.

  CLEAR: e_alv_layout.
  e_alv_layout-default_item      = cc_x.
  e_alv_layout-zebra             = cc_x.
  e_alv_layout-colwidth_optimize = cc_x.

ENDFORM.                    " YF_GERA_LAYTGERAL_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_CABECALHO_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_cabecalho_alv.

  DATA: lc_data(10)    TYPE c,
        lc_hora(10)    TYPE c,
        lc_dt(25)      TYPE c,
        lc_mandt(40)   TYPE c.

  DATA: w_listheader LIKE LINE OF yt_listheader.

  WRITE sy-datum DD/MM/YYYY TO lc_data.
  WRITE sy-uzeit            TO lc_hora.

  CONCATENATE lc_data lc_hora INTO lc_dt SEPARATED BY ' / '.
  CONCATENATE sy-mandt sy-host sy-uname
         INTO lc_mandt SEPARATED BY ' - '.

  REFRESH: yt_listheader.

  CLEAR w_listheader.
  w_listheader-typ     = 'S'.       "H=Header, S=Selection, A=Action
  w_listheader-key     = text-007.
  APPEND w_listheader TO yt_listheader.

  w_listheader-typ    = 'S'.        "H=Header, S=Selection, A=Action
  w_listheader-key    = text-008.
  w_listheader-info   = lc_dt.
  APPEND w_listheader TO yt_listheader.

  w_listheader-typ    = 'S'.
  w_listheader-key    = text-009.
  w_listheader-info   = lc_mandt.
  APPEND w_listheader TO yt_listheader.

ENDFORM.                    " YF_GERA_CABECALHO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LINHASDET_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_linhasdet_alv.

  DATA li_numcol   TYPE int4 VALUE 0.

  REFRESH: yt_fieldcat.

* File Name
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-key                   = cc_x.
  e_fieldcat-fix_column            = cc_x.
  e_fieldcat-fieldname             = 'FILENAME'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'FILENAME'.
  e_fieldcat-seltext_m             = 'Nome do Arquivo'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Arquivo processado'.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.

* Controle de Versão de Arquivo
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'IDCTRL'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'IDCTRL'.
  e_fieldcat-seltext_s             = 'Versão'.
  e_fieldcat-reptext_ddic          = 'Versão do Arquivo'.
  APPEND e_fieldcat TO yt_fieldcat.

* Tipo de mensagem
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'MSGTYP'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'MSGTYP'.
  e_fieldcat-seltext_s             = 'TpMsg'.
  e_fieldcat-reptext_ddic          = 'Tipo de Mensagem'.
  e_fieldcat-outputlen             = 5.
  APPEND e_fieldcat TO yt_fieldcat.

* Descrição do Processamento
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'MSGV1'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'MSGV1'.
  e_fieldcat-seltext_l             = 'Descrição do processamento'.
  e_fieldcat-reptext_ddic          = 'Descrição do processamento'.
  e_fieldcat-ddictxt               = 'L'.
  e_fieldcat-outputlen             = 100.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.

* Lote
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'LOTE'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'LOTE'.
  e_fieldcat-seltext_s             = 'Lote'.
  e_fieldcat-reptext_ddic          = 'Número do Lote'.
  APPEND e_fieldcat TO yt_fieldcat.

* Data
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'DATA'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'DATA'.
  e_fieldcat-seltext_m             = 'Dt Execução'.
  e_fieldcat-reptext_ddic          = 'Data do processamento'.
  APPEND e_fieldcat TO yt_fieldcat.

* Hora
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'HORA'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'HORA'.
  e_fieldcat-seltext_m             = 'Hr Execução'.
  e_fieldcat-ddictxt               = 'M'.
  e_fieldcat-outputlen             = 10.
  e_fieldcat-reptext_ddic          = 'Hora do processamento'.
  APPEND e_fieldcat TO yt_fieldcat.

* Usuário
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'USUARIO'.
  e_fieldcat-ref_tabname           = 'ZLEST0008'.
  e_fieldcat-ref_fieldname         = 'USUARIO'.
  e_fieldcat-seltext_m             = 'Usuário'.
  e_fieldcat-ddictxt               = 'M'.
  e_fieldcat-outputlen             = 10.
  e_fieldcat-reptext_ddic          = 'Usuário do processamento'.
  APPEND e_fieldcat TO yt_fieldcat.

ENDFORM.                    " YF_GERA_LINHASDET_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_ROTEVENTO_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_rotevento_alv.

  REFRESH: yt_event.

  CLEAR e_event.
  e_event-name = slis_ev_top_of_list.
  e_event-form = 'YF_ALV_TOP_OF_LIST'.
  APPEND e_event TO yt_event.

  CLEAR e_event.
  e_event-name = slis_ev_user_command.
  e_event-form = 'YF_ALV_USER_COMMAND'.
  APPEND e_event TO yt_event.

ENDFORM.                    " YF_GERA_ROTEVENTO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_EXIBE_RELATORIO_ALV
*&---------------------------------------------------------------------*
FORM yf_exibe_relatorio_alv.

  vc_formpfstatus = 'YF_SET_PF_STATUS'.
  vc_formusercomm = 'YF_ALV_USER_COMMAND'.
  vl_formtopofpag = 'YF_ALV_TOP_OF_LIST'.
  e_alv_layout-box_fieldname = 'MARK'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_interface_check        = ' '
      i_callback_program       = vc_repid
      i_callback_pf_status_set = vc_formpfstatus
      i_callback_user_command  = vc_formusercomm
      i_callback_top_of_page   = vl_formtopofpag
      is_layout                = e_alv_layout
      it_fieldcat              = yt_fieldcat
      i_default                = 'X'
      i_save                   = cc_a
      is_variant               = e_variant
      it_events                = yt_event
    TABLES
      t_outtab                 = yt_saida[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " YF_EXIBE_RELATORIO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_TOP_OF_LIST
*&---------------------------------------------------------------------*
FORM yf_alv_top_of_list.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = yt_listheader.

ENDFORM.                    " YF_ALV_TOP_OF_LIST

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_USER_COMMAND - Eventos
*&---------------------------------------------------------------------*
FORM yf_alv_user_command USING i_ucomm    LIKE sy-ucomm
                               i_selfield TYPE slis_selfield.


  DATA: wl_saida TYPE type_alv.
  DATA: wl_diretorio TYPE string.
  CLEAR: wg_cont_r, wl_diretorio.

  DATA: var_ctg    TYPE zid_ctg,
        var_prefix TYPE zprefix.

  READ TABLE yt_saida INTO wl_saida WITH KEY mark = 'X'.

  CASE wl_saida-msgtyp.
    WHEN: 'E'.
      CASE i_ucomm.
        WHEN 'DOWN'.

          READ TABLE yt_saida TRANSPORTING NO FIELDS WITH KEY mark = 'X'.

          IF sy-subrc IS INITIAL.
            CALL METHOD cl_gui_frontend_services=>directory_browse
              EXPORTING
                window_title         = 'Salvar o(s) arquivos'
              CHANGING
                selected_folder      = wl_diretorio
              EXCEPTIONS
                cntl_error           = 1
                error_no_gui         = 2
                not_supported_by_gui = 3
                OTHERS               = 4.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF .

          LOOP AT yt_saida WHERE mark IS NOT INITIAL.
            PERFORM download_arquivo USING yt_saida wl_diretorio.
          ENDLOOP.

          IF wg_cont_r GT 0.
            MESSAGE s000(zles) WITH 'Foram marcados para ser reprocessado(s)' wg_cont_r 'arquivo(s).'.
          ENDIF.

      ENDCASE.
    WHEN OTHERS.
      MESSAGE s000(zles) WITH 'É permitido somente download de arquivos com ERROS'.
  ENDCASE.
ENDFORM.                    " YF_ALV_USER_COMMAND

*---------------------------------------------------------------------*
*       FORM YF_SET_PF_STATUS
*---------------------------------------------------------------------*
FORM yf_set_pf_status USING extab TYPE slis_t_extab.

  DATA: lc_fcode   TYPE gui_code.

  "Copiar (SE41) o pf-status do programa "SAPLKKBL/STANDARD"
  "Elimnar comandos que não necessite

* Funções a serem implementadas - A serem definidas...
**  lc_fcode = ?????????.
**  APPEND lc_fcode TO extab.
**  lc_fcode = ????????.
**  APPEND lc_fcode TO extab.

  SET PF-STATUS 'PRINCIPAL_100' EXCLUDING extab.

ENDFORM.                    "YF_SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_INIT_VARIANT
*&---------------------------------------------------------------------*
FORM yf_alv_init_variant .

  CLEAR: e_variant,
         e_def_variant.

  e_variant-report = sy-repid.

* Get default variant at screen initialisation
  e_def_variant = e_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = cc_a
    CHANGING
      cs_variant = e_def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_vari = e_def_variant-variant.
  ENDIF.

ENDFORM.                    " YF_ALV_INIT_VARIANT

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_DISPLAY_VARIANT
*&---------------------------------------------------------------------*
FORM yf_alv_display_variant.

* Display variant list on screen.
  IF NOT p_vari IS INITIAL.

    MOVE: e_variant TO e_def_variant,
          p_vari     TO e_def_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = cc_a
      CHANGING
        cs_variant = e_def_variant.

    e_variant = e_def_variant.
    CLEAR p_vari.

  ELSE.
    CLEAR e_variant.
    e_variant-report = vc_repid.
  ENDIF.

ENDFORM.                    " YF_ALV_DISPLAY_VARIANT

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_REUSE_VARIANT_F4
*&---------------------------------------------------------------------*
FORM yf_alv_reuse_variant_f4 .

  DATA: lc_exit(1) TYPE c.

  CLEAR: lc_exit,
         e_def_variant.

* Call the ALV variant on F4 push button.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = e_variant
      i_save     = cc_a
    IMPORTING
      e_exit     = lc_exit
      es_variant = e_def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid
            TYPE 'S' NUMBER sy-msgno
            DISPLAY LIKE 'E'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF lc_exit = space.
      p_vari = e_def_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_ALV_REUSE_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_YT_SAIDA  text
*----------------------------------------------------------------------*
FORM download_arquivo  USING    wl_saida LIKE LINE OF yt_saida
                                wl_diretorio.

  DATA:     wl_input(60)     TYPE c,
            wl_caminho(100)  TYPE c,
            wl_log(60)       TYPE c,
            wl_subrc         TYPE sy-subrc,
            vl_cont          TYPE sy-tabix,
            vl_extensao(4)                ,
            wl_arq(60)                    ,
            vl_prefix        TYPE zprefix .

  DATA:  lw_zlest0007 TYPE zlest0007.

  CLEAR:  wl_input,
          wl_log,
          vl_cont,
          vl_extensao,
          wl_arq,
          wl_subrc.

  "Verifica qual é o tipo do arquivo L1, L2 ou L3.
  CASE wl_saida-filename(2).
    WHEN: 'L1'.
      vl_prefix =  wl_saida-filename(2).
    WHEN: 'L2'.
      vl_prefix = wl_saida-filename(2).
    WHEN: 'L3'.
      vl_prefix = wl_saida-filename(2).
  ENDCASE.

  SELECT SINGLE * FROM zlest0007 INTO lw_zlest0007 WHERE id_ctg = 'LOG'
                                                     AND prefix = vl_prefix.

  IF ( sy-subrc EQ 0 ).

    vl_cont      = STRLEN( wl_saida-filename ) - 4.
    vl_extensao  = wl_saida-filename+vl_cont(4).
    wl_arq       = wl_saida-filename(vl_cont).
    TRANSLATE vl_extensao TO UPPER CASE.


    IF vl_extensao EQ '.TXT'.

      CONCATENATE  lw_zlest0007-pathunix wl_saida-filename INTO wl_caminho.

      PERFORM le_arquivo_unix USING wl_caminho
                              CHANGING wl_subrc.
      IF wl_subrc IS INITIAL.

        PERFORM transfere_file USING wl_diretorio wl_arq wl_caminho
                               CHANGING wl_subrc.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " DOWNLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
FORM preenche_caminho  USING    v_categ
                                v_prefix
                       CHANGING v_path.

  READ TABLE t_zlest0007 INTO st_zlest0007
    WITH KEY id_ctg = v_categ
             prefix = v_prefix.

  IF NOT st_zlest0007-pathunix IS INITIAL.
    v_path = st_zlest0007-pathunix.

  ENDIF.

  CLEAR st_zlest0007.

ENDFORM.                    " PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO_UNIX_WINDOW
*&---------------------------------------------------------------------*
FORM le_arquivo_unix  USING  p_path
                     CHANGING subrc.

  DATA: l_path TYPE string.
  l_path = p_path.

  REFRESH t_file.
  CLEAR   t_file.

*    OPEN DATASET v_caminho FOR INPUT IN BINARY MODE.
  OPEN DATASET p_path FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
  IF sy-subrc IS INITIAL .
    ADD 1 TO wg_cont_r.
    subrc = sy-subrc.

    DO.
      READ DATASET p_path INTO t_file.
      IF sy-subrc  IS INITIAL.
        APPEND t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CLOSE DATASET p_path.
  ENDIF.
ENDFORM.                    "LE_ARQUIVO_UNIX_WINDOW
*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
FORM transfere_file  USING    p_path p_arq p_caminho
                     CHANGING subrc.

  DATA: l_path TYPE string,
        wl_separator.
*  L_PATH = P_PATH.
  CLEAR: l_path.
  cl_gui_frontend_services=>get_file_separator( CHANGING
    file_separator       = wl_separator ).

  CONCATENATE p_path wl_separator p_arq sy-datum '_' sy-uzeit '_R.TXT' INTO l_path.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = l_path
    CHANGING
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
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  OPEN DATASET  L_PATH FOR OUTPUT IN TEXT MODE    "smart: 11/01/10 E111
*         ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
*  IF SY-SUBRC IS INITIAL.
*    LOOP AT T_FILE.
*      TRANSFER T_FILE-LINHA TO L_PATH.
*    ENDLOOP.
*
*    CLOSE DATASET L_PATH.
*    DELETE DATASET P_CAMINHO.
*  ENDIF.
ENDFORM.                    " TRANSFERE_FILE
