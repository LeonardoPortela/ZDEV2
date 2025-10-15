************************************************************************
************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 25.01.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Programa de automatização de compensação de fornec. *
*                  do modulo Comercialização-SIGAM (F-44)              *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 25.01.2008    Michely              Inicio               DEVK903420   *
*                                                                      *
************************************************************************

REPORT zgl001_comp_f44   NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
                         MESSAGE-ID z01
                         LINE-SIZE  150              "Comprimento da Linha
                         LINE-COUNT 65.              "Número de Linhas
*----------------------------------------------------------------------*
* Declarações gerais                                                   *
*----------------------------------------------------------------------*
INCLUDE <icon>.

FIELD-SYMBOLS <icone> LIKE icon_checked.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_lotes,
        mandt            LIKE zgl001_comp_f44-mandt,
        bukrs            LIKE zgl001_comp_f44-bukrs,
        lote             LIKE zgl001_comp_f44-lote,
        belnr            LIKE zgl001_comp_f44-belnr,
      END OF wa_lotes.

DATA: BEGIN OF wa_documentos,
        mandt            LIKE zgl001_comp_f44-mandt, "Mandante
        bukrs            LIKE zgl001_comp_f44-bukrs, "Empresa
        belnr            LIKE zgl001_comp_f44-belnr, "Nº documento contabil
        lote             LIKE zgl001_comp_f44-lote,  "Lote Sigam
        lifnr            LIKE zgl001_comp_f44-lifnr, "Fornecedor
        waers            LIKE zgl001_comp_f44-waers, "Código moeda
        budat            LIKE zgl001_comp_f44-budat, "Dt lanc. Documento
        umskz            LIKE zgl001_comp_f44-umskz, "Codigo de Razão
        dmbtr            LIKE zgl001_comp_f44-dmbtr, "Montante em moeda interna
        processamento    LIKE zgl001_comp_f44-processamento, "P-Parcial T-Total
        acerto           LIKE zgl001_comp_f44-acerto, "Sim/Nao
     END OF wa_documentos.

DATA: BEGIN OF wa_retorno,
        mandt            LIKE zgl002_comp_f44-mandt,
        bukrs            LIKE zgl002_comp_f44-bukrs,
        lote             LIKE zgl002_comp_f44-lote,
        status           LIKE zgl002_comp_f44-status,
        sgtxt            LIKE zgl002_comp_f44-sgtxt,
      END OF wa_retorno.

DATA: it_documentos      LIKE STANDARD TABLE OF wa_documentos,
      it_lotes           LIKE STANDARD TABLE OF wa_lotes,
      wa_dochead         LIKE wa_documentos.

DATA: vg_exe             TYPE c LENGTH 4 VALUE 'FORE',
      vg_mode            TYPE c LENGTH 1,
      vg_doc             TYPE n LENGTH 10,
      vg_bukrs           LIKE zgl001_comp_f44-bukrs.

*----------------------------------------------------------------------*
* Declaração de dados para BATCH-INPUT
*----------------------------------------------------------------------*
DATA: wa_bdcdata         LIKE bdcdata,
      wa_message         LIKE bdcmsgcoll,
      it_bdcdata         LIKE STANDARD TABLE OF wa_bdcdata,
      it_message         LIKE STANDARD TABLE OF wa_message,
      it_retcomp         LIKE STANDARD TABLE OF wa_bdcdata.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s00.
PARAMETERS: p_bukrs      LIKE zgl001_comp_f44-bukrs,
            p_lote       LIKE zgl001_comp_f44-lote.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: rb_fore  RADIOBUTTON GROUP exe DEFAULT 'X' ,
            rb_back  RADIOBUTTON GROUP exe.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETERS: rb_modn  RADIOBUTTON GROUP mod DEFAULT 'X',
            rb_moda  RADIOBUTTON GROUP mod.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-s03.
PARAMETERS: rb_aut  RADIOBUTTON GROUP ret DEFAULT 'X',
            rb_job  RADIOBUTTON GROUP ret.
SELECTION-SCREEN END   OF BLOCK b3.


*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF ( p_bukrs IS INITIAL ) AND  ( NOT p_lote IS INITIAL ).
    MESSAGE i000 WITH 'O parâmetro de empresa deve ser informado.'.
    LEAVE TO SCREEN 0.
  ENDIF.
  IF ( p_lote IS INITIAL ) AND ( NOT p_bukrs IS INITIAL ).
    MESSAGE i000 WITH 'O parâmetro de lote deve ser informado.'.
    LEAVE TO SCREEN 0.
  ENDIF.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_seleciona_doc.

  IF rb_fore IS INITIAL.
    vg_exe = 'BACK'.
  ELSE.
    vg_exe = 'FORE'.
  ENDIF.

  PERFORM f_compensacao USING vg_exe.

END-OF-SELECTION.

*----------------------------------------------------------------------*
* Top-of-page                                                          *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  SET TITLEBAR 'TITULO'.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.
  WRITE: /01  sy-vline,
          08  'Empresa',
          23  'Lote',
          50  'Mensagem',
          150 sy-vline.
  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

*----------------------------------------------------------------------*
* End-of-page                                                          *
*----------------------------------------------------------------------*
END-OF-PAGE.

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_doc
*&---------------------------------------------------------------------*
*       Selecionar os dados enviado pelo XI para compensação
*----------------------------------------------------------------------*
FORM f_seleciona_doc .
  REFRESH it_documentos.

  IF ( p_bukrs IS INITIAL ) AND ( p_lote IS INITIAL ).
    SELECT z1~mandt z1~bukrs z1~lote
      FROM zgl001_comp_f44 AS z1
      INTO TABLE it_lotes
      GROUP BY z1~mandt z1~bukrs z1~lote.
  ELSE.
    SELECT z1~mandt z1~bukrs z1~lote
      FROM zgl001_comp_f44 AS z1
      INTO TABLE it_lotes
      WHERE z1~bukrs EQ p_bukrs
      AND z1~lote  EQ p_lote
      GROUP BY z1~mandt z1~bukrs z1~lote.
  ENDIF.

ENDFORM.                    " f_seleciona_doc
*&---------------------------------------------------------------------*
*&      Form  f_compensacao
*&---------------------------------------------------------------------*
*       Processo de compensacao dos documentos.
*----------------------------------------------------------------------*
FORM f_compensacao USING p_exe.
  DATA: vl_mess(256)     TYPE c,
        vl_lote          LIKE zgl002_comp_f44-lote,
        vl_bukrs         LIKE zgl002_comp_f44-bukrs.
  IF rb_modn IS INITIAL.
    vg_mode = 'A'.
  ELSE.
    vg_mode = 'N'.
  ENDIF.

* Função para montar SHPDB e executar a call transaction
  IF p_exe EQ 'BACK'.
    PERFORM f_executa_back.
  ELSE.
    PERFORM f_evita_time_out USING 'Processando...'.
    IF it_lotes[] IS NOT INITIAL.
*      sy-mandt = '060'.
      CALL FUNCTION 'Z_GL_COMP_F44_SHDB'
        EXPORTING
          i_modo     = vg_mode
          i_ret      = ''
        TABLES
          it_lotes   = it_lotes
          it_retorno = it_message.
    ENDIF.
    IF it_message IS INITIAL AND it_lotes IS NOT INITIAL.
      SELECT bukrs lote sgtxt
        FROM zgl002_comp_f44
        INTO (vl_bukrs, vl_lote, vl_mess)
         FOR ALL ENTRIES IN it_lotes
       WHERE bukrs EQ it_lotes-bukrs
         AND lote  EQ it_lotes-lote.
        ASSIGN icon_incomplete TO <icone>.
        WRITE: /01 sy-vline,
               03 <icone> AS ICON,
               08 vl_bukrs,
               23 vl_lote,
               50 vl_mess,
               150 sy-vline.
      ENDSELECT.
    ENDIF.

    LOOP AT it_message INTO wa_message.
      ASSIGN icon_check      TO <icone>.
      IF 'WAE' CS wa_message-msgtyp.
        ASSIGN icon_incomplete TO <icone>.
      ELSEIF wa_message-msgtyp EQ 'S'.
        ASSIGN icon_check      TO <icone>.
        IF rb_aut IS NOT INITIAL.
          IF wa_message-msgnr EQ 312.
            IF wa_message-msgv1 IS NOT INITIAL.
              vg_doc   = wa_message-msgv1.
              vg_bukrs = wa_message-msgv2.
              PERFORM f_retorno_comp USING vg_doc vg_bukrs.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      PERFORM f_busca_mess USING wa_message-msgid
            wa_message-msgnr
            wa_message-msgv1
            wa_message-msgv2
            wa_message-msgv3
            wa_message-msgv4
      CHANGING vl_mess.

      PERFORM f_imprime_erros  USING vl_mess.
    ENDLOOP.
    ULINE.
  ENDIF.

ENDFORM.                    " f_compensacao
*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESS  Mensagem de sucesso ou erro do shdb.
*----------------------------------------------------------------------*
FORM f_imprime_erros  USING    p_mess.
  WRITE: /01 sy-vline,
          03 <icone> AS ICON,
          08 wa_lotes-bukrs,
          23 wa_lotes-lote,
          50 p_mess,
          150 sy-vline.
ENDFORM.                    " f_imprime_erros
*&---------------------------------------------------------------------*
*&      Form  f_busca_mess
*&---------------------------------------------------------------------*
*       busca mensagem a mensagem.
*----------------------------------------------------------------------*
*      -->P_MSGID  text
*      -->P_MSGNR  text
*      -->P_MSGV1  text
*      -->P_MSGV2  text
*      -->P_MSGV3  text
*      -->P_MSGV4  text
*      <--R_MESS  text
*----------------------------------------------------------------------*
FORM f_busca_mess  USING    p_msgid
                            p_msgnr
                            p_msgv1
                            p_msgv2
                            p_msgv3
                            p_msgv4
                   CHANGING r_mess.
  sy-msgid = p_msgid.
  sy-msgno = p_msgnr.
  sy-msgv1 = p_msgv1.
  sy-msgv2 = p_msgv2.
  sy-msgv3 = p_msgv3.
  sy-msgv4 = p_msgv4.
  CALL FUNCTION 'CUTC_GET_MESSAGE'
    EXPORTING
      msg_id      = sy-msgid
      msg_no      = sy-msgno
      msg_arg1    = sy-msgv1
      msg_arg2    = sy-msgv2
      msg_arg3    = sy-msgv3
      msg_arg4    = sy-msgv4
      language    = sy-langu
    IMPORTING
      raw_message = r_mess.

ENDFORM.                    " f_busca_mess
*&---------------------------------------------------------------------*
*&      Form  f_executa_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_executa_back .
*  Variáveis para criar Job
  DATA: vl_report   LIKE sy-cprog,          "Programa
        vl_auth     LIKE tbtcjob-authcknam, "Usuário
        vl_jobgroup LIKE tbtcjob-jobgroup,  "Grupo
        vl_jobname  LIKE tbtcjob-jobname,   "Nome do JOB
        vl_jobcount LIKE tbtcjob-jobcount,  "Número do JOB
        print_parameters TYPE pri_params.

  vl_jobgroup = 'COMP_AUT_F44'.
  vl_auth = sy-uname.
  vl_report = 'ZGL001_COMP_F44'.
  CONCATENATE 'JOB_'
              sy-uname
              '_F44' INTO vl_jobname.

*Cria nova tarefa - JOB
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      delanfrep        = 'X'
      jobgroup         = vl_jobgroup
      jobname          = vl_jobname
    IMPORTING
      jobcount         = vl_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE s001 WITH text-001. "Impossível programar execução dos dados
    EXIT.
  ENDIF.

  vg_exe = 'FORE'.

  SUBMIT (vl_report) TO SAP-SPOOL
    SPOOL PARAMETERS print_parameters
    WITHOUT SPOOL DYNPRO
    VIA JOB vl_jobname NUMBER vl_jobcount
    AND RETURN.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = vl_jobcount
        jobname              = vl_jobname
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
*   Impossível programar execução dos dados
      MESSAGE i001 WITH text-001.
      EXIT.
    ENDIF.
    MESSAGE s000 WITH text-002.
  ENDIF.

ENDFORM.                    " f_executa_back
*&---------------------------------------------------------------------*
*&      Form  f_evita_time_out
*&---------------------------------------------------------------------*
*       Para exibir mensagem de processando
*----------------------------------------------------------------------*
FORM f_evita_time_out USING value(p_msg).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = p_msg.
ENDFORM.                    " f_evita_time_out
*&---------------------------------------------------------------------*
*&      Form  F_RETORNO_COMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MESSAGE_MSGV1  Numero de compensação
*----------------------------------------------------------------------*
FORM f_retorno_comp  USING p_doc p_bukrs.
  DATA: wa_docret          LIKE zfit0006,
        it_docret          LIKE STANDARD TABLE OF wa_docret.

  PERFORM f_evita_time_out USING 'Start de retorno de compensação para SIGAM...'.

  WAIT UP TO 1 SECONDS.

  SELECT *
    FROM zfit0006
    INTO TABLE it_docret
   WHERE belnr EQ p_doc
     AND bukrs EQ p_bukrs.

*---> 05/07/2023 - Migração S4 - DL
  SORT it_docret BY belnr.
*<--- 05/07/2023 - Migração S4 - DL

  READ TABLE it_docret INTO wa_docret WITH KEY belnr = p_doc
                                               BINARY SEARCH.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'Z_FI_RETURN_PAYMENT_AP_AR'
    EXPORTING
      i_bukrs = wa_docret-bukrs
      i_augbl = wa_docret-belnr
      i_gjahr = wa_docret-gjahr
      i_tcode = wa_docret-tcode.

  UPDATE zfit0006 SET status = 'E' WHERE bukrs   = wa_docret-bukrs
                                     AND belnr   = wa_docret-belnr
                                     AND gjahr   = wa_docret-gjahr
                                     AND bukrs_e = wa_docret-bukrs_e
                                     AND belnr_e = wa_docret-belnr_e
                                     AND gjahr_e = wa_docret-gjahr_e
                                     AND laufd   = wa_docret-laufd
                                     AND laufi   = wa_docret-laufi.

ENDFORM.                    " F_RETORNO_COMP
