*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 15/05/2013                                              &*
*& Descrição: Extrator dados Classificação HVI                        &*
*& Transação: ZMM0047                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Report   ZMMR0025
*&---------------------------------------------------------------------*

REPORT  zmmr0025.

TYPE-POOLS abap.

** TABLES
**----------------------------------------------------------------------
TABLES: mseg, chvw, mara, mchb,   "STANDARD
        zmmt0025, zmmt0027, ztsafrafardos.

** CONSTANTS
**----------------------------------------------------------------------


** TYPES
**----------------------------------------------------------------------
TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_chvw,
         werks TYPE chvw-werks,
         matnr TYPE chvw-matnr,
         charg TYPE chvw-charg,
         budat TYPE chvw-budat,
         menge TYPE chvw-menge,
         bwart TYPE chvw-bwart,
         mblnr TYPE chvw-mblnr,
         mjahr TYPE chvw-mjahr,
       END OF ty_chvw,

       BEGIN OF ty_mseg,
         mblnr TYPE mseg-mblnr,
         mjahr TYPE mseg-mjahr,
         zeile TYPE mseg-zeile,
         bwart TYPE mseg-bwart,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         lgort TYPE mseg-lgort,
         charg TYPE mseg-charg,
         smbln TYPE mseg-smbln,
         menge TYPE mseg-menge,
         budat TYPE mkpf-budat,
         check TYPE c,
       END OF ty_mseg,

       BEGIN OF ty_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         cspem TYPE mchb-cspem,
       END OF ty_mchb.

TYPES: BEGIN OF ty_zmme_cl.
         INCLUDE TYPE zmme_cl.
TYPES:   check TYPE c,
       END OF ty_zmme_cl.

TYPES: ty_matnr TYPE RANGE OF mara-matnr.

** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: it_mara          TYPE STANDARD TABLE OF ty_mara,
      it_chvw          TYPE STANDARD TABLE OF ty_chvw,
      it_mseg          TYPE STANDARD TABLE OF ty_mseg,
      it_mseg_aux      TYPE STANDARD TABLE OF ty_mseg,
      it_mchb          TYPE STANDARD TABLE OF ty_mchb,
      it_matnr         TYPE STANDARD TABLE OF zmme_cl,
      it_return        TYPE STANDARD TABLE OF ty_zmme_cl,
      it_return_s      TYPE STANDARD TABLE OF ty_zmme_cl WITH UNIQUE SORTED KEY key COMPONENTS matnr charg atinn,
      it_return_aux    TYPE STANDARD TABLE OF ty_zmme_cl,
      it_zmmt0025      TYPE STANDARD TABLE OF zmmt0025,
      it_zmmt0027      TYPE STANDARD TABLE OF zmmt0027,
      it_zppt0002      TYPE STANDARD TABLE OF zppt0002,
      it_log           TYPE STANDARD TABLE OF zmmt0173,
      it_ztsafrafardos TYPE STANDARD TABLE OF ztsafrafardos.  "ADD - IS - 12.06.2013

DATA: rg_matnr TYPE RANGE OF matnr,
      vg_erro  TYPE char01,
      vg_msg   TYPE char40.

** WORK AREAS
**----------------------------------------------------------------------
DATA: wa_mara          TYPE ty_mara,
      wa_chvw          TYPE ty_chvw,
      wa_mseg          TYPE ty_mseg,
      wa_mchb          TYPE ty_mchb,
      wa_matnr         TYPE zmme_cl,
      wa_return        TYPE zmme_cl,
      wa_zmmt0025      TYPE zmmt0025,
      wa_zmmt0027      TYPE zmmt0027,
      wa_ztsafrafardos TYPE ztsafrafardos,
      wa_zppt0002      TYPE zppt0002.  "ADD - IS - 12.06.2013

** VARIABLES
**----------------------------------------------------------------------
DATA: vl_dataini TYPE ztsafrafardos-data_inicio,
      vl_datafim TYPE ztsafrafardos-data_fim.

DATA: s_werks TYPE RANGE OF werks_d.
*      s_safra TYPE RANGE OF charg_d.

DATA: vg_msg_indicador TYPE char40.

** RANGES
**----------------------------------------------------------------------


** SELECTION SCREEN
**----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK one WITH FRAME TITLE TEXT-t01.

  SELECT-OPTIONS:
*    s_werks   FOR chvw-werks      OBLIGATORY,
    s_matkl   FOR mara-matkl      OBLIGATORY NO-EXTENSION NO INTERVALS,
    s_safra   FOR ztsafrafardos-charg NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK one.



*----------------------------------------------------------------------
* Evento: Start-of-selection
*----------------------------------------------------------------------
START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

*&-----------------------------------------------------------------------------------------
*&  Inicio ajuste BUG SOLTO 119975 - dump ao executar com múltiplas filiais / AOENNING
*&-----------------------------------------------------------------------------------------

*&-------------------------------------------------------------------------
*&  Seleciona data e filial para processamento na tabela de parametro.
*&-------------------------------------------------------------------------
  FREE: it_ztsafrafardos.
  SELECT charg werks_from data_inicio data_fim
  FROM ztsafrafardos
  INTO CORRESPONDING FIELDS OF TABLE it_ztsafrafardos.
*  WHERE charg IN s_safra.
  IF sy-subrc EQ 0.

    "Buscar data.
    SORT it_ztsafrafardos DESCENDING BY data_fim.
    READ TABLE it_ztsafrafardos INTO DATA(ws_ztsafra) INDEX 1.
    IF sy-subrc EQ 0.
      vl_dataini = ws_ztsafra-data_inicio.
      vl_datafim = ws_ztsafra-data_fim.
    ENDIF.

    "Buscar safra.
    SORT it_ztsafrafardos DESCENDING BY  charg.
    READ TABLE it_ztsafrafardos INTO DATA(ws_param) INDEX 1.
    IF sy-subrc EQ 0.
      s_safra = VALUE #( sign = 'I' option = 'EQ' low = ws_param-charg ).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ws_param-charg ) TO s_safra.
    ENDIF.

    SORT it_ztsafrafardos BY charg.
    DELETE it_ztsafrafardos WHERE charg NOT IN s_safra.

    SORT it_ztsafrafardos BY werks_from.
    DELETE ADJACENT DUPLICATES FROM it_ztsafrafardos COMPARING werks_from.
  ENDIF.

  LOOP AT it_ztsafrafardos INTO wa_ztsafrafardos.
    CLEAR: vg_msg_indicador.
    vg_msg_indicador = |Processando dados filial { wa_ztsafrafardos-werks_from } aguarde ....|.
    PERFORM f_progress USING '' vg_msg_indicador.

    PERFORM f_lima_variaveis_globais.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_ztsafrafardos-werks_from ) TO s_werks.

    PERFORM:  f_seleciona_dados.

    IF vg_erro EQ abap_true.
      CONTINUE.
    ENDIF.

    PERFORM:  f_monta_tabela,
              f_inserir_tabela.

    CLEAR: ztsafrafardos.
  ENDLOOP.

*&-----------------------------------------------------------------------------------------
*&  Fim ajuste BUG SOLTO 119975 - dump ao executar com múltiplas filiais / AOENNING
*&-----------------------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  RANGES: lra_budat_mkpf FOR mkpf-budat.

  FIELD-SYMBOLS: <fs_return> TYPE ty_zmme_cl,
                 <fs_mseg>   TYPE ty_mseg.

** LIMPAR TABELA ZMMT0027 OS PARAMETROS REQUISITADOS JA EXISTIREM
  DELETE FROM zmmt0027 WHERE werks  IN s_werks
                         AND matkl  IN s_matkl
                         AND safra  IN s_safra.

  PERFORM f_progress USING 10 'Selecionando lotes.'.

** SELECIONAR MATERIAIS
  SELECT matnr matkl normt
    FROM mara
    INTO TABLE it_mara
   WHERE matkl IN s_matkl.

  IF it_mara[] IS NOT INITIAL.

    rg_matnr = VALUE ty_matnr( FOR lwa_mara IN it_mara[] (
        sign   = 'I'
        option = 'EQ'
        low    = lwa_mara-matnr ) ).
    SORT rg_matnr[] BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rg_matnr[] COMPARING low.

** SELECIONAR OS TIPOS DE ATINN
    SELECT *
      FROM zmmt0025
      INTO TABLE it_zmmt0025.

    IF it_zmmt0025[] IS NOT INITIAL.

*&------------------------------------------------------------------------
* Comentado BUG SOLTO 119975 / AOENNING
*&------------------------------------------------------------------------
** SELECIONAR LOTES DE ACORDO COM OS MATERIAIS
*      SELECT SINGLE data_inicio data_fim
*      FROM ztsafrafardos
*        INTO (vl_dataini, vl_datafim)
*      WHERE charg IN s_safra.
*&------------------------------------------------------------------------
      IF vl_dataini IS NOT INITIAL AND vl_datafim IS NOT INITIAL.

        CLEAR: lra_budat_mkpf[].
        APPEND VALUE #( sign = 'I' option = 'BT' low  = vl_dataini high = vl_datafim ) TO lra_budat_mkpf.

        SELECT mblnr mjahr zeile bwart matnr
               werks lgort charg smbln menge
               budat_mkpf AS budat
          FROM mseg AS m INTO CORRESPONDING FIELDS OF TABLE it_mseg
         WHERE matnr      IN rg_matnr
           AND werks      IN s_werks
           AND bwart      IN ('131','309','311')
           AND budat_mkpf IN lra_budat_mkpf
           AND shkzg      EQ 'S'
           AND NOT EXISTS ( SELECT mblnr
                              FROM mseg AS e
                             WHERE smbln EQ m~mblnr
                               AND sjahr EQ m~mjahr )
           %_HINTS ORACLE 'INDEX("Z08")'.

        IF it_mseg[] IS NOT INITIAL.
*          SORT it_mseg[] BY mblnr mjahr zeile ASCENDING.
*          DELETE it_mseg[] WHERE budat GE vl_datafim OR budat LE vl_dataini.
** DELETAR DOCUMENTOS ESTORNADOS
*          PERFORM f_progress USING 25 'Verificando estornados.'.

*          SELECT mblnr mjahr zeile bwart matnr
*                 werks lgort charg smbln menge
*                 budat_mkpf
*            FROM mseg
*            INTO TABLE it_mseg_aux
*             FOR ALL ENTRIES IN it_mseg
*           WHERE smbln EQ it_mseg-mblnr
*             AND mjahr EQ it_mseg-mjahr.
*
*          IF it_mseg_aux[] IS NOT INITIAL.
*            SORT it_mseg_aux BY mjahr smbln.
*
*            LOOP AT it_mseg_aux INTO wa_mseg.
*              DELETE it_mseg WHERE mjahr EQ wa_mseg-mjahr
*                               AND mblnr EQ wa_mseg-smbln.
*
*            ENDLOOP.
*
*          ENDIF.

          IF it_mseg[] IS NOT INITIAL.
** LOTES DUPLICADOS
            SORT it_mseg BY werks charg mblnr DESCENDING.
            DELETE ADJACENT DUPLICATES FROM it_mseg COMPARING  werks charg.

** SELECIONAR SAFRA, DADOS CLASSIFICAÇÃO
            PERFORM f_progress USING 40 'Buscando classificação.'.

*            LOOP AT it_mseg INTO wa_mseg.
*              MOVE: wa_mseg-matnr TO wa_matnr-matnr,
*                    wa_mseg-charg TO wa_matnr-charg.
*
*              APPEND wa_matnr TO it_matnr.
*
*            ENDLOOP.

            it_matnr[] = VALUE #( FOR wa_mseg IN it_mseg[] (
                                      matnr   = wa_mseg-matnr
                                      charg   = wa_mseg-charg ) ).

            CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
              TABLES
                t_matnr  = it_matnr
                t_return = it_return
              EXCEPTIONS
                erro4    = 1
                OTHERS   = 2.
            IF it_return[] IS NOT INITIAL.
              SORT: it_zmmt0025 BY atnam,
                    "it_return BY matnr charg atinn, -- Já ordenado dentro da função Z_DADOSCLASSIFICACAOLOTE
                    it_mseg BY matnr charg.

              READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.

              LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                                 AND atwrt NE s_safra-low.

                APPEND wa_return TO it_return_aux.

              ENDLOOP.

              IF it_return_aux[] IS NOT INITIAL.
                SORT it_return_aux BY matnr charg atinn.

                CLEAR: wa_return.

                LOOP AT it_return_aux INTO wa_return.
                  READ TABLE it_return ASSIGNING <fs_return> WITH KEY matnr = wa_return-matnr
                                                                      charg = wa_return-charg BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    <fs_return>-check = abap_true.
                  ENDIF.

                  READ TABLE it_mseg ASSIGNING <fs_mseg> WITH KEY matnr = wa_return-matnr
                                                                  charg = wa_return-charg BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    <fs_mseg>-check = abap_true.
                  ENDIF.

                ENDLOOP.

                UNASSIGN: <fs_mseg>, <fs_return>.

                DELETE it_return WHERE check EQ abap_true.
                DELETE it_mseg WHERE check EQ abap_true.

              ENDIF.

            ENDIF.

** SELEÇÕES FARDO DE ORIGEM
** ADD - IS - 12.06.2013 - Inicio
            SELECT *
              FROM zppt0002
              INTO TABLE it_zppt0002
               FOR ALL ENTRIES IN it_mseg
             WHERE acharg  EQ it_mseg-charg
               AND werks   EQ it_mseg-werks.

            SORT it_zppt0002 BY acharg werks .
** ADD - IS - 12.06.2013 - Fim

** SELEÇAO DA COLUNA “STATUS”
            PERFORM f_progress USING 55 'Verificando status.'.

            SELECT matnr werks lgort charg clabs cspem
              FROM mchb
              INTO TABLE it_mchb
               FOR ALL ENTRIES IN it_mseg
             WHERE matnr EQ it_mseg-matnr
               AND werks EQ it_mseg-werks
               AND lgort EQ it_mseg-lgort
               AND charg EQ it_mseg-charg.

** TABELA MSEG VAZIA APOS ESTORNADOS
*            MESSAGE 'Documentos estornados.' TYPE 'I'.
          ENDIF.
        ELSE.
** NAO FORAM ENCONTRADOS DADOS NA TABELA MSEG
*          MESSAGE 'Não foram encontrados dados para os parâmetros informados.' TYPE 'I'.
          CLEAR: vg_msg.
          vg_msg = 'Não foram encontrados dados para os parâmetros informados'.
          PERFORM f_add_log USING 'ZMM0047' vg_msg 'E'.
          vg_erro = abap_true.
        ENDIF.

      ELSE.
** NAO EXISTE DADOS NA TABELA ZTSAFRAFARDOS
*        MESSAGE 'Não foram encontrados dados na tabela ZTSAFRAFARDOS para a SAFRA informada.' TYPE 'I'.
        CLEAR: vg_msg.
        vg_msg = 'Não foram encontrados dados na tabela ZTSAFRAFARDOS para a SAFRA informada'.
        PERFORM f_add_log USING 'ZMM0047' vg_msg 'E'.
        vg_erro = abap_true.
      ENDIF.

    ELSE.
** NAO EXISTE DADOS NA TABELA ZZMMT0025
*      MESSAGE 'Não foram encontrados dados na tabela ZMMT0025.' TYPE 'I'.
      CLEAR: vg_msg.
      vg_msg = 'Não foram encontrados dados na tabela ZMMT0025.'.
      PERFORM f_add_log USING 'ZMM0047' vg_msg 'E'.
      vg_erro = abap_true.

    ENDIF.

  ELSE.
** NÃO EXISTE MATERIAIS PARA O GRUPO(S) INFORMADO(S)
*    MESSAGE 'Não foram encontrados materiais para o grupo(s) informado(s).' TYPE 'I'.
    CLEAR: vg_msg.
    vg_msg = 'Não foram encontrados materiais para o grupo(s) informado(s)'.
    PERFORM f_add_log USING 'ZMM0047' vg_msg 'E'.
    vg_erro = abap_true.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_TABELA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_monta_tabela.

  CHECK: it_mseg[] IS NOT INITIAL.

  PERFORM f_progress USING 70 'Montando tabela.'.

  SORT: it_mara          BY matnr normt matkl,
*        it_mseg          BY mblnr mjahr matnr werks lgort charg smbln,
        it_mseg          BY matnr werks lgort charg,
        it_mchb          BY matnr werks lgort charg clabs cspem,
        it_return        BY matnr charg atinn,
        it_zmmt0025      BY atinn.

  it_return_s[] = it_return[].

  FREE: it_return.

  LOOP AT it_mseg INTO wa_mseg.
    CLEAR: wa_mara, wa_mchb, wa_return, wa_zmmt0027, wa_zppt0002.

    wa_zmmt0027-werks   = wa_mseg-werks.
    wa_zmmt0027-matnr   = wa_mseg-matnr.
    wa_zmmt0027-charg   = wa_mseg-charg.
    wa_zmmt0027-menge   = wa_mseg-menge.
    wa_zmmt0027-budat   = wa_mseg-budat.
    wa_zmmt0027-lgort   = wa_mseg-lgort.
*    wa_zmmt0027-safra   = s_safra-low.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mseg-matnr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zmmt0027-matkl   = wa_mara-matkl.
      wa_zmmt0027-normt   = wa_mara-normt.
    ENDIF.

** ADD - IS - 12.06.2013 - Inicio
    READ TABLE it_zppt0002 INTO wa_zppt0002 WITH KEY acharg = wa_mseg-charg
                                                     werks  = wa_mseg-werks
*                                                     matnr  = wa_mseg-matnr
                                            BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zmmt0027-charg_orig  = wa_zppt0002-charg.
    ENDIF.
** ADD - IS - 12.06.2013 - Fim

    READ TABLE it_mchb INTO wa_mchb WITH KEY matnr = wa_mseg-matnr
                                             werks = wa_mseg-werks
                                             lgort = wa_mseg-lgort
                                             charg = wa_mseg-charg
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_mchb-clabs EQ 0 AND wa_mchb-cspem EQ 0.
        wa_zmmt0027-status = 'EMBARCADO'.
      ELSEIF wa_mchb-cspem NE 0.
        wa_zmmt0027-status = 'RESERVADO'.
      ELSEIF wa_mchb-clabs NE 0.
        wa_zmmt0027-status = 'DISPONIVEL'.
      ELSEIF wa_mchb-cspem NE 0 AND wa_mchb-clabs NE 0.
        wa_zmmt0027-status = 'VERIFICAR'.
      ENDIF.
    ENDIF.

    LOOP AT it_return_s INTO wa_return USING KEY key WHERE matnr = wa_mseg-matnr
                                                       AND charg = wa_mseg-charg.

      READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atinn = wa_return-atinn
                                              BINARY SEARCH.
      IF sy-subrc = 0.
        CASE wa_zmmt0025-atnam.
          WHEN: 'UHML'.
            wa_zmmt0027-far_uhml    = wa_return-atwrt.      " FARDINHOS_UHML
          WHEN: 'UI'.
            wa_zmmt0027-far_ui      = wa_return-atwrt.      " FARDINHOS_UI
          WHEN: 'STR'.
            wa_zmmt0027-far_str     = wa_return-atwrt.      " FARDINHOS_STR
          WHEN: 'ELG'.
            wa_zmmt0027-far_elg     = wa_return-atwrt.      " FARDINHOS_ELG
          WHEN: 'MIC'.
            wa_zmmt0027-far_mic     = wa_return-atwrt.      " FARDINHOS_MIC
          WHEN: 'RD'.
            wa_zmmt0027-far_rd      = wa_return-atwrt.      " FARDINHOS_RD
          WHEN: '+B'.
            wa_zmmt0027-far_b       = wa_return-atwrt.      " FARDINHOS_B
          WHEN: 'CG'.
            wa_zmmt0027-far_cg      = wa_return-atwrt.      " FARDINHOS_CG
          WHEN: 'T.CNT'.
            wa_zmmt0027-far_tcnt    = wa_return-atwrt.      " FARDINHOS_TCNT
          WHEN: 'T.AREA'.
            wa_zmmt0027-far_tarea   = wa_return-atwrt.      " FARDINHOS_TAREA
          WHEN: 'LEAF'.
            wa_zmmt0027-far_leaf    = wa_return-atwrt.      " FARDINHOS_LEAF
          WHEN: 'MR'.
            wa_zmmt0027-far_mr      = wa_return-atwrt.      " FARDINHOS_MR
          WHEN: 'SFI(W)'.
            wa_zmmt0027-far_sfiw    = wa_return-atwrt.      " FARDINHOS_SFIW
          WHEN: 'SCI'.
            wa_zmmt0027-far_sci     = wa_return-atwrt.      " FARDINHOS_SCI
          WHEN: 'CSP'.
            wa_zmmt0027-far_csp     = wa_return-atwrt.      " FARDINHOS_CSP
          WHEN: 'PERIODO'.
            wa_zmmt0027-far_periodo = wa_return-atwrt.      " FARDINHOS_PERIODO
          WHEN: 'SAFRA'.
            wa_zmmt0027-safra       = wa_return-atwrt.      " SAFRA
          WHEN: 'VARIEDADE'.
            wa_zmmt0027-variedade   = wa_return-atwrt.      " VARIEDADE
          WHEN: 'TALHAO'.
            wa_zmmt0027-talhao      = wa_return-atwrt.      " TALHAO
        ENDCASE.
      ENDIF.
    ENDLOOP.


    APPEND wa_zmmt0027 TO it_zmmt0027.
  ENDLOOP.

ENDFORM.                    " F_MONTA_TABELA

*&---------------------------------------------------------------------*
*&      Form  F_INSERIR_TABELA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_inserir_tabela .
  CHECK: it_zmmt0027[] IS NOT INITIAL.

  PERFORM f_progress USING 95 'Gravando dados.'.

  SORT it_zmmt0027 BY charg werks matkl matnr safra.
  MODIFY zmmt0027 FROM TABLE it_zmmt0027.

  IF sy-subrc IS INITIAL.
    COMMIT WORK.
*    MESSAGE 'Dados inseridos com sucesso na tabela ZMMT0027' TYPE 'S'.
    CLEAR: vg_msg.
    vg_msg = 'Dados inseridos com sucesso na tabela ZMMT0027'.
    PERFORM f_add_log USING 'ZMM0047' vg_msg 'S'.
  ELSE.
    ROLLBACK WORK.
*    MESSAGE 'Não foi possivel inserir os dados na tabela ZMMT0027' TYPE 'I'.
    CLEAR: vg_msg.
    vg_msg = 'Não foi possivel inserir os dados na tabela ZMMT0027'.
    PERFORM f_add_log USING 'ZMM0047' vg_msg 'S'.
  ENDIF.

ENDFORM.                    " F_INSERIR_TABELA

*&---------------------------------------------------------------------*
*&      Form  F_PROGRESS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_progress  USING vf_percen vf_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = vf_percen   " Veloc do relogio em %
      text       = vf_text.    " Texto que aparecerá.
ENDFORM.                    " F_PROGRESS
*&---------------------------------------------------------------------*
*& Form f_lima_variaveis_globais
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_lima_variaveis_globais .

  FREE: it_mara,
        it_chvw,
        it_mseg,
        it_mseg_aux,
        it_mchb,
        it_matnr,
        it_return,
        it_return_s,
        it_return_aux,
        it_zmmt0025,
        it_zmmt0027,
        it_zppt0002,
        s_werks,
        rg_matnr.



  CLEAR: wa_mara,
         wa_chvw,
         wa_mseg,
         wa_mchb,
         wa_matnr,
         wa_return,
         wa_zmmt0025,
         wa_zmmt0027,
         vg_erro,
         wa_zppt0002.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_add_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_add_log USING p_trans TYPE char10
                     p_msg TYPE char40
                     p_tipo TYPE bapi_mtype.


  it_log = VALUE #( (
  type        = p_tipo
  werks       = wa_ztsafrafardos-werks_from
  charg       = wa_ztsafrafardos-charg
  date_ini    = wa_ztsafrafardos-data_inicio
  date_fim    = wa_ztsafrafardos-data_fim
  dt_atual    = sy-datum
  hr_atual    = sy-uzeit
  user_atual  = sy-uname
  message     = p_msg
  message_v1  = ''
  message_v2  = ''
  message_v3  = ''
  message_v4  = ''

  ) ).

  IF it_log IS NOT INITIAL.
    MODIFY zmmt0173 FROM TABLE it_log.
    COMMIT WORK.
  ENDIF.



ENDFORM.
