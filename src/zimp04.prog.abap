************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 05.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Aprovação de Tributos a Pagar                       *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 05.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************

REPORT  zimp04.

*-----------------------------------------------------------------------
* Declaração para SELECT-OPTIONS
*-----------------------------------------------------------------------
TABLES:
  b120.

*-----------------------------------------------------------------------
* Pool de tipos
*-----------------------------------------------------------------------
TYPE-POOLS:
  slis.

*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:

  BEGIN OF ty_zimp_cabecalho,
    nro_doc_tr       TYPE zimp_cabecalho-nro_doc_tr,
    gjahr            TYPE zimp_cabecalho-gjahr,
    budat            TYPE zimp_cabecalho-budat,
    zfbdt            TYPE zimp_cabecalho-zfbdt,
    tp_arrec         TYPE zimp_cabecalho-tp_arrec,
    belnr            TYPE zimp_cabecalho-belnr,
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 INICIO
    estorno          TYPE zimp_cabecalho-estorno,
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 FIM
* Início Alteração Ricardo Furst 18.07.2009
    usuario          TYPE zimp_cabecalho-usnam,
    cta_at_mon       TYPE zimp_cabecalho-cta_at_mon,
    cta_tse          TYPE zimp_cabecalho-cta_tse,
* Fim Alteração Ricardo Furst 18.07.2009
  END OF ty_zimp_cabecalho,

  BEGIN OF ty_zimp_detalhe,
    nro_doc_tr       TYPE zimp_detalhe-nro_doc_tr,
    gjahr            TYPE zimp_detalhe-gjahr,
    buzei            TYPE zimp_detalhe-buzei,
    vlr_principal    TYPE zimp_detalhe-vlr_principal,
    vlr_multa        TYPE zimp_detalhe-vlr_multa,
    vlr_juros        TYPE zimp_detalhe-vlr_juros,
    gsber            TYPE zimp_detalhe-gsber,
    sgtxt            TYPE zimp_detalhe-sgtxt,
    aprovador        TYPE zimp_detalhe-aprovador,
* Início Alteração Ricardo Furst 20.07.2009
    tse              TYPE zimp_detalhe-tse,
    vlr_atual_mone   TYPE zimp_detalhe-vlr_atual_mone,
    usuario_apr      TYPE zimp_detalhe-usuario_apr,
* Fim Alteração Ricardo Furst 20.07.2009
    vlr_outras_ent TYPE zimp_detalhe-vlr_outras_ent,
  END OF ty_zimp_detalhe,

  BEGIN OF ty_zimp_tipos_impos,
    tp_arrec         TYPE zimp_tipos_impos-tp_arrec,
    arrecadacao      TYPE zimp_tipos_impos-arrecadacao,
  END OF ty_zimp_tipos_impos,

  BEGIN OF ty_relatorio,
    aprovador        TYPE zimp_detalhe-aprovador,
    gjahr            TYPE zimp_detalhe-gjahr,
    buzei            TYPE zimp_detalhe-buzei,
    tp_arrec         TYPE zimp_cabecalho-tp_arrec,
    arrecadacao      TYPE zimp_tipos_impos-arrecadacao,
    nro_doc_tr       TYPE zimp_detalhe-nro_doc_tr,
    budat            TYPE zimp_cabecalho-budat,
    zfbdt            TYPE zimp_cabecalho-zfbdt,
    vlr_principal    TYPE zimp_detalhe-vlr_principal,
    vlr_multa        TYPE zimp_detalhe-vlr_multa,
    vlr_juros        TYPE zimp_detalhe-vlr_juros,
* Início Alteração Ricardo Furst 20.07.2009
    tse              TYPE zimp_detalhe-tse,
    vlr_atual_mone   TYPE zimp_detalhe-vlr_atual_mone,
    usuario_apr      TYPE zimp_detalhe-usuario_apr,
* Fim Alteração Ricardo Furst 20.07.2009
    vlr_total        TYPE zimp_detalhe-vlr_principal,
    sgtxt            TYPE zimp_detalhe-sgtxt,
*    descricao        type zimp_tipos_impos-descricao,
vlr_outras_ent TYPE zimp_detalhe-vlr_outras_ent,
  END OF ty_relatorio.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_zimp_cabecalho   TYPE TABLE OF ty_zimp_cabecalho WITH HEADER LINE,
  t_zimp_tipos_impos TYPE TABLE OF ty_zimp_tipos_impos,
  t_zimp_detalhe     TYPE TABLE OF ty_zimp_detalhe,
  t_relatorio        TYPE TABLE OF ty_relatorio,
* Início alteração Ricardo Furst 03.07.2009
  t_relat_aux        TYPE TABLE OF ty_relatorio,
* Fim alteração Ricardo Furst 03.07.2009
  t_fieldcat         TYPE slis_t_fieldcat_alv,
  t_listheader       TYPE slis_t_listheader,
* Início Alteração Ricardo Furst.
  t_zimp_det_aux     TYPE STANDARD TABLE OF zimp_detalhe WITH HEADER LINE.
* Fim Alteração Ricardo Furst.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_relatorio        TYPE ty_relatorio,
* Início alteração Ricardo Furst 03.07.2009
  w_relat_aux        TYPE ty_relatorio,
* Fim alteração Ricardo Furst 03.07.2009
  w_fieldcat         TYPE slis_fieldcat_alv,
  w_layout           TYPE slis_layout_alv,
  w_listheader       TYPE slis_listheader.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_zimp_cabecalho>   TYPE ty_zimp_cabecalho,
  <f_zimp_tipos_impos> TYPE ty_zimp_tipos_impos,
  <f_zimp_detalhe>     TYPE ty_zimp_detalhe,
  <f_relatorio>        TYPE ty_relatorio.

*-----------------------------------------------------------------------
* Constantes
*-----------------------------------------------------------------------
CONSTANTS:

  c_s                TYPE c VALUE 'S',
  c_e                TYPE c VALUE 'E',
  c_x                TYPE c VALUE 'X'.

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_nr_doc           FOR <f_zimp_detalhe>-nro_doc_tr
                     NO-EXTENSION NO INTERVALS.

PARAMETERS:
  p_emp              TYPE b120-bukrs
                     OBLIGATORY.

SELECT-OPTIONS:
  s_filial           FOR b120-j_1bbranch
                     NO-EXTENSION NO INTERVALS.

* Início Alteração Ricardo Furst 18.07.2009
SELECT-OPTIONS:
  s_usua             FOR <f_zimp_cabecalho>-usuario
                     NO-EXTENSION NO INTERVALS.
* Fim Alteração Ricardo Furst 18.07.2009
*PARAMETERS: p_est  AS CHECKBOX.
*SELECT-OPTIONS: s_belnr FOR <f_zimp_cabecalho>-belnr
*                     NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b01.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Busca dados nas tabelas e verifica a consistência
  PERFORM zf_busca_dados.

* Monta relatório
  PERFORM zf_monta_relatorio.

* Configura e exibe relatório
  PERFORM zf_exibe_relatorio.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busca dados para processamento
*----------------------------------------------------------------------*
FORM zf_busca_dados .

* Seleciona cabeçalho dos documentos
  SELECT nro_doc_tr gjahr budat zfbdt tp_arrec belnr
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 INICIO
         estorno
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 FIM
* Início Alteração Ricardo Furst 18.07.2009
         usnam cta_at_mon cta_tse
* Fim Alteração Ricardo Furst 18.07.2009
    INTO TABLE t_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs      EQ p_emp    AND
          nro_doc_tr IN s_nr_doc
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 INICIO.
      AND ( estorno    EQ 'X' OR estorno = space )
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 FIM.
* Início Alteração Ricardo Furst 18.07.2009
      AND usnam IN s_usua.
* Fim Alteração Ricardo Furst 18.07.2009

  IF sy-subrc = 0.
    LOOP AT t_zimp_cabecalho.
      IF ( NOT t_zimp_cabecalho-belnr IS INITIAL ) AND t_zimp_cabecalho-estorno = space.
        DELETE t_zimp_cabecalho.
      ENDIF.
    ENDLOOP.
    IF t_zimp_cabecalho[] IS INITIAL.
      MESSAGE text-e01 TYPE c_s DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SELECT tp_arrec arrecadacao
      INTO TABLE t_zimp_tipos_impos
      FROM zimp_tipos_impos
      FOR ALL ENTRIES IN t_zimp_cabecalho
      WHERE tp_arrec = t_zimp_cabecalho-tp_arrec ORDER BY PRIMARY KEY .

*   Se foi apenas selecionado um documento, verifica se já não foi
*   criado seu respectivo documento FI-AP
    IF sy-dbcnt = 1.

      READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>
        INDEX 1.
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 INICIO.
      IF ( NOT <f_zimp_cabecalho>-belnr IS INITIAL )
        AND
         ( <f_zimp_cabecalho>-estorno = space ).
*Alteração Gustavo Marques - Rollout Consulting 29.05.09 FIM.

        MESSAGE text-e02 TYPE c_s DISPLAY LIKE c_e.
        LEAVE LIST-PROCESSING.
      ENDIF.

    ENDIF.

    SORT t_zimp_cabecalho BY nro_doc_tr gjahr.

*   Seleciona itens dos documentos
    SELECT nro_doc_tr gjahr buzei vlr_principal vlr_multa vlr_juros
           gsber sgtxt aprovador
* Início Alteração Ricardo Furst 20.07.2009
           tse vlr_atual_mone usuario_apr
* Fim Alteração Ricardo Furst 20.07.2009
* Eduardo
      vlr_outras_ent
* Eduardo
      INTO TABLE t_zimp_detalhe
      FROM zimp_detalhe
      FOR ALL ENTRIES IN t_zimp_cabecalho
      WHERE bukrs       = p_emp    AND
            nro_doc_tr = t_zimp_cabecalho-nro_doc_tr
        AND usuario_apr IN s_usua
* Início Alteração Ricardo Furst.
        AND job = space.

    IF sy-subrc = 0.

*     Efetua o filtro por filial
      DELETE t_zimp_detalhe WHERE NOT gsber IN s_filial.

    ENDIF.

  ENDIF.

* Verifica se existem registros válidos
  IF t_zimp_detalhe[] IS INITIAL.
    MESSAGE text-e01 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*       Monta estrutura do relatório
*----------------------------------------------------------------------*
FORM zf_monta_relatorio .

  LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>.

    READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>
      WITH KEY nro_doc_tr = <f_zimp_detalhe>-nro_doc_tr
               gjahr      = <f_zimp_detalhe>-gjahr
      BINARY SEARCH.

    READ TABLE t_zimp_tipos_impos ASSIGNING <f_zimp_tipos_impos>
      WITH KEY tp_arrec = <f_zimp_cabecalho>-tp_arrec
      BINARY SEARCH.

    w_relatorio-aprovador     = <f_zimp_detalhe>-aprovador.
    w_relatorio-gjahr         = <f_zimp_detalhe>-gjahr.
    w_relatorio-buzei         = <f_zimp_detalhe>-buzei.
    w_relatorio-tp_arrec      = <f_zimp_cabecalho>-tp_arrec.
    w_relatorio-arrecadacao   = <f_zimp_tipos_impos>-arrecadacao.
    w_relatorio-nro_doc_tr    = <f_zimp_detalhe>-nro_doc_tr.
    w_relatorio-budat         = <f_zimp_cabecalho>-budat.
    w_relatorio-zfbdt         = <f_zimp_cabecalho>-zfbdt.
    w_relatorio-vlr_principal = <f_zimp_detalhe>-vlr_principal.
    w_relatorio-vlr_multa     = <f_zimp_detalhe>-vlr_multa.
    w_relatorio-vlr_juros     = <f_zimp_detalhe>-vlr_juros.
    w_relatorio-vlr_outras_ent = <f_zimp_detalhe>-vlr_outras_ent.
* Início Alteração Ricardo Furst 20.07.2009
    w_relatorio-vlr_atual_mone = <f_zimp_detalhe>-vlr_atual_mone.
    w_relatorio-tse           = <f_zimp_detalhe>-tse.
* Fim Alteração Ricardo Furst 20.07.2009
    w_relatorio-vlr_total     = w_relatorio-vlr_principal  +
                                w_relatorio-vlr_multa      +
                                w_relatorio-vlr_juros      +
                                w_relatorio-vlr_outras_ent +
* Início Alteração Ricardo Furst 20.07.2009
                                w_relatorio-vlr_atual_mone +
                                w_relatorio-tse.
    w_relatorio-usuario_apr   = <f_zimp_detalhe>-usuario_apr.
* Fim Alteração Ricardo Furst 20.07.2009
    w_relatorio-sgtxt         = <f_zimp_detalhe>-sgtxt.

    APPEND w_relatorio TO t_relatorio.

  ENDLOOP.

ENDFORM.                    " ZF_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*       Configura e exibe relatório
*----------------------------------------------------------------------*
FORM zf_exibe_relatorio .

  w_layout-colwidth_optimize = c_x.
  w_layout-zebra             = c_x.

  w_fieldcat-checkbox = c_x.
  w_fieldcat-edit     = c_x.

  PERFORM zf_fieldcat USING
    'APROVADOR'     'Aprov.'(011).

  CLEAR: w_fieldcat-checkbox,
         w_fieldcat-edit.

  PERFORM zf_fieldcat USING:
    'TP_ARREC'      'Tipo'(001),
    'ARRECADACAO'   'Doc. Arrecadação'(012),
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 INICIO
    'DESCRICAO'     'Tipo'(013),
*Alteração Gustavo Marques - ROllout Consulting 29.05.2009 FIM
    'NRO_DOC_TR'    'Nro. Doc.'(002),
    'BUDAT'         'Dt. Lcto.'(003),
    'ZFBDT'         'Dt. Vcto.'(004),
    'VLR_PRINCIPAL' 'Vlr. Principal'(005),
    'VLR_MULTA'     'Vlr. Multa'(006),
    'VLR_JUROS'     'Vlr. Juros'(007),
* Início Alteração Ricardo Furst 20.07.2009
    'VLR_ATUAL_MONE' 'Vlr. At. Monet.'(013),
    'TSE'       'Vlr. TSE'(014),
* Fim Alteração Ricardo Furst 20.07.2009
* Eduardo
   'VLR_OUTRAS_ENT'  'Outras Entidades'(099),
* Eduardo

    'VLR_TOTAL'     'Vlr. Total'(010),
* Início Alteração Ricardo Furst 20.07.2009
    'USUARIO_APR'   'Usuario'(015),
* Fim Alteração Ricardo Furst 20.07.2009
    'SGTXT'         'Texto'(008).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZF_ALV_STATUS_SET'
      i_callback_user_command  = 'ZF_ALV_USER_COMMAND'
      i_callback_top_of_page   = 'ZF_ALV_TOP_OF_PAGE'
      is_layout                = w_layout
      it_fieldcat              = t_fieldcat
    TABLES
      t_outtab                 = t_relatorio
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ZF_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_FIELDCAT
*&---------------------------------------------------------------------*
*       Monta ALV Fieldcat
*----------------------------------------------------------------------*
FORM zf_fieldcat  USING    p_field TYPE slis_fieldcat_alv-fieldname
                           p_text  TYPE slis_fieldcat_alv-seltext_l.

  w_fieldcat-fieldname = p_field.
  w_fieldcat-seltext_l = p_text.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ZF_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  zf_alv_status_set
*&---------------------------------------------------------------------*
*       PF-STATUS ALV
*----------------------------------------------------------------------*
FORM zf_alv_status_set USING lt_extab TYPE slis_t_extab.    "#EC CALLED

  SET PF-STATUS 'ZIMP04' EXCLUDING lt_extab.

ENDFORM.                    "zf_alv_status_set

*&---------------------------------------------------------------------*
*&      Form  zf_alv_user_command
*&---------------------------------------------------------------------*
*       USER-COMMAND ALV
*----------------------------------------------------------------------*
FORM zf_alv_user_command USING r_ucomm     TYPE sy-ucomm
                               ls_selfield TYPE slis_selfield. "#EC CALLED

  FIELD-SYMBOLS:
     <f_grid> TYPE REF TO cl_gui_alv_grid.
  DATA: v_index(20) TYPE c.
  DATA: p_jobn(32),
              p_jobd     LIKE sy-datum,"sy-datum,
              p_jobt     LIKE sy-uzeit." VALUE sy-uzeit.

  p_jobd = sy-datum.
  p_jobt = sy-uzeit.
*  break abap.
  DATA:   v_job_id   LIKE tbtcjob-jobcount,
          v_jobd     LIKE sy-datum,
          v_jobt     LIKE sy-uzeit,
          v_back(1)  TYPE c,
          v_aux_1(1) TYPE c,
          v_aux_2(2) TYPE c.
*  BREAK-POINT.
* Processa a aprovação
  CASE r_ucomm.

    WHEN 'DATA_SAVE'.
* Início Alteração Ricardo Furst 03.07.2009
      LOOP AT t_relatorio INTO w_relatorio.

        MOVE-CORRESPONDING w_relatorio TO w_relat_aux.

        APPEND w_relat_aux TO t_relat_aux.

      ENDLOOP.

* Fim Alteração Ricardo Furst 03.07.2009
      ASSIGN ('(SAPLSLVC_FULLSCREEN)gt_grid-grid') TO <f_grid>.

      CALL METHOD <f_grid>->check_changed_data.
      DATA: w_tabix(3).
      LOOP AT t_relatorio ASSIGNING <f_relatorio>.
        w_tabix = sy-tabix.
*        UPDATE zimp_detalhe SET aprovador = <f_relatorio>-aprovador
*          WHERE bukrs      = p_emp                    AND
*                nro_doc_tr = <f_relatorio>-nro_doc_tr AND
*                gjahr      = <f_relatorio>-gjahr.
*
*        IF sy-subrc <> 0.
*          ROLLBACK WORK.
*          MESSAGE text-e03 TYPE 'A'.
*        ENDIF.

* Início Alteração Ricardo Furst 03.07.2009
        READ TABLE t_relat_aux INTO w_relat_aux INDEX sy-tabix.

        IF sy-subrc = 0.

          READ TABLE t_zimp_cabecalho WITH KEY nro_doc_tr = w_relat_aux-nro_doc_tr.
          IF sy-subrc <> 0.
            CLEAR t_zimp_cabecalho.
          ENDIF.

          IF ( w_relat_aux-aprovador <> <f_relatorio>-aprovador AND
               t_zimp_cabecalho-belnr = space                        )
            OR
             (  w_relat_aux-aprovador = <f_relatorio>-aprovador AND
              t_zimp_cabecalho-estorno = 'X'                          ).

            MOVE p_emp TO t_zimp_det_aux-bukrs.

            MOVE w_relat_aux-nro_doc_tr TO t_zimp_det_aux-nro_doc_tr.
            MOVE w_relat_aux-gjahr TO t_zimp_det_aux-gjahr.
            MOVE w_relat_aux-buzei TO t_zimp_det_aux-buzei.

*            MOVE w_relatorio-nro_doc_tr TO t_zimp_det_aux-nro_doc_tr.
*            MOVE w_relatorio-gjahr TO t_zimp_det_aux-gjahr.
*            MOVE w_relatorio-buzei TO t_zimp_det_aux-buzei.
            MOVE 'X' TO t_zimp_det_aux-job.

            UPDATE zimp_detalhe SET job = t_zimp_det_aux-job
              WHERE bukrs      = p_emp                    AND
                    nro_doc_tr = t_zimp_det_aux-nro_doc_tr AND
                    gjahr      = t_zimp_det_aux-gjahr AND
                    buzei      = t_zimp_det_aux-buzei.

            APPEND t_zimp_det_aux.

            COMMIT WORK AND WAIT.

            CONCATENATE 'ZIMP04' sy-datum sy-uzeit w_tabix INTO p_jobn SEPARATED BY '-'.
            CALL FUNCTION 'JOB_OPEN'
              EXPORTING
                jobname          = p_jobn
              IMPORTING
                jobcount         = v_job_id
              EXCEPTIONS
                cant_create_job  = 1
                invalid_job_data = 2
                jobname_missing  = 3
                OTHERS           = 4.

*BREAK-POINT.

            SUBMIT zimp07 WITH s_nr_doc-low  = t_zimp_cabecalho-nro_doc_tr
                          WITH p_emp         = p_emp
                          WITH s_filial-low  = s_filial-low
                          WITH s_dtlanc-low  = t_zimp_cabecalho-budat
                          WITH s_dtvenc-low  = t_zimp_cabecalho-zfbdt

*            SUBMIT zimp07 WITH s_nr_doc-low  = <f_relatorio>-nro_doc_tr
*                          WITH p_emp         = p_emp
*                          WITH s_filial-low  = s_filial-low
*                          WITH s_dtlanc-low  = <f_relatorio>-budat
*                          WITH s_dtvenc-low  = <f_relatorio>-zfbdt

*                          AND RETURN.
                   VIA JOB p_jobn
                    NUMBER v_job_id
                AND RETURN.





            IF p_jobd  = sy-datum AND p_jobt <= sy-uzeit.
              v_jobd  = ' '.
              v_jobt  = ' '.
              v_aux_1 = 'X'.
            ELSE.
              v_jobd  = p_jobd.
              v_jobt  = p_jobt.
              v_aux_1 = ' '.
            ENDIF.

            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount             = v_job_id
                jobname              = p_jobn
                sdlstrtdt            = v_jobd
                sdlstrttm            = v_jobt
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

          ENDIF.

        ENDIF.
* Fim Alteração Ricardo Furst 03.07.2009

      ENDLOOP.

      IF sy-subrc = 0.
        COMMIT WORK.
        ls_selfield-refresh = c_x.
        MESSAGE text-i02 TYPE c_s.
      ELSE.
        MESSAGE text-i01 TYPE 'I'.
      ENDIF.

    WHEN 'ALL'.

      LOOP AT t_relatorio ASSIGNING <f_relatorio>.
        <f_relatorio>-aprovador = c_x.
      ENDLOOP.

      ls_selfield-refresh = c_x.

    WHEN 'SAL'.

      LOOP AT t_relatorio ASSIGNING <f_relatorio>.
        CLEAR <f_relatorio>-aprovador.
      ENDLOOP.

      ls_selfield-refresh = c_x.

  ENDCASE.

ENDFORM.                    "zf_alv_user_command

*&---------------------------------------------------------------------*
*&      Form  zf_alv_top_of_page
*&---------------------------------------------------------------------*
*       TOP-OF-PAGE ALV
*----------------------------------------------------------------------*
FORM zf_alv_top_of_page.                                    "#EC CALLED

  IF t_listheader[] IS INITIAL.

    w_listheader-typ  = 'S'.
    w_listheader-key  = 'Empresa'(009).
    w_listheader-info = p_emp.

    APPEND w_listheader TO t_listheader.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_listheader.

ENDFORM.                    "zf_alv_top_of_page
