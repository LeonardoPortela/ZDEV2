*&---------------------------------------------------------------------*
*& Report  ZFIS38
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfis38.

TYPES: BEGIN OF ty_proc,
         chave      TYPE zib_nfe_forn-nu_chave,
         dt_emissao TYPE zib_nfe_dist_ter-dt_emissao,
         dest_cnpj  TYPE stcd1,
         dest_ie    TYPE stcd3,
         bukrs      TYPE zib_nfe_forn-bukrs,
         branch     TYPE zib_nfe_forn-branch,
       END OF ty_proc.

DATA: tg_proc             TYPE TABLE OF ty_proc          WITH HEADER LINE,
      tg_zib_nfe_forn     TYPE TABLE OF zib_nfe_forn     WITH HEADER LINE,
      tg_zib_nfe_forn_env TYPE TABLE OF zib_nfe_forn     WITH HEADER LINE,
      tg_zib_nfe_dist_ter TYPE TABLE OF zib_nfe_dist_ter WITH HEADER LINE.

"BUG 67933
DATA: lv_data_man TYPE P0001-BEGDA.

DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_exec TYPE c.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  SELECT SINGLE COUNT( * ) INTO @DATA(wl_tbtco)
    FROM tbtco
   WHERE jobname EQ 'ZFIS38_JOB'
     AND status  EQ 'R'.

  IF ( wl_tbtco EQ 1 ) OR ( p_exec EQ abap_true ).

    "<<"BUG 67933 - Prazo de Manifesto 180 Nota Técnica 2020.001


    DATA(lv_date) =  sy-datum.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date
        days      = 0
        months    = 6
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_data_man.

*>>""<<"BUG 67933


    PERFORM: f_proc_manifesto_env, "Processar Retorno Manifesto já enviados
             f_selecionar_nfe,     "Selecionar Novas Entradas para realizar manifesto
             f_proc_manifesto.     "Processar Envio Manifestos
  ENDIF.

FORM f_selecionar_nfe.

  CLEAR: tg_proc[],
         tg_zib_nfe_dist_ter[],
         tg_zib_nfe_forn[].

*  "<<"BUG 67933
*  SELECT SINGLE low
*    FROM tvarvc
*      INTO @DATA(w_tvarvc)
*    WHERE name = @l_tvarv.

  SELECT *
    FROM zib_nfe_forn INTO TABLE tg_zib_nfe_forn
   WHERE manifesto_env   EQ abap_false
     AND nu_chave_modelo EQ '55'
     AND atualizado      EQ 'X'
     AND docnum          NE '0000000000'
     AND docnum          NE space
    AND dt_emissao >= lv_data_man.                             "BUG 67933

*  "Desconsiderar registros antigos....
*  LOOP AT TG_ZIB_NFE_FORN WHERE DT_EMISSAO <= '20181030'.
*    TG_ZIB_NFE_FORN-MANIFESTO_ENV = ABAP_TRUE.
*    TG_ZIB_NFE_FORN-STOP_ENV_MAN  = ABAP_TRUE.
*    MODIFY ZIB_NFE_FORN FROM TG_ZIB_NFE_FORN.
*    DELETE TG_ZIB_NFE_FORN.
*  ENDLOOP.
  ">>"BUG 67933
  CHECK tg_zib_nfe_forn[] IS NOT INITIAL.

  SORT tg_zib_nfe_forn BY dt_emissao.

  DATA(_count) = 0.
  LOOP AT tg_zib_nfe_forn.

    SELECT SINGLE *
      FROM zib_nfe_dist_ter INTO @DATA(_wl_ib_nfe_dist_ter)
     WHERE chave_nfe EQ @tg_zib_nfe_forn-nu_chave.

    IF sy-subrc EQ 0.
      ADD 1 TO _count.
      APPEND _wl_ib_nfe_dist_ter TO tg_zib_nfe_dist_ter.
    ENDIF.

    IF _count > 100.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT tg_zib_nfe_dist_ter.

    READ TABLE tg_zib_nfe_forn WITH KEY nu_chave = tg_zib_nfe_dist_ter-chave_nfe.

    CHECK sy-subrc EQ 0.

    tg_proc-chave       = tg_zib_nfe_dist_ter-chave_nfe.
    tg_proc-dt_emissao  = tg_zib_nfe_dist_ter-dt_emissao.
    tg_proc-dest_cnpj   = tg_zib_nfe_dist_ter-destino_cnpj.
    tg_proc-dest_ie     = tg_zib_nfe_dist_ter-destino_ie.
    tg_proc-bukrs       = tg_zib_nfe_forn-bukrs.
    tg_proc-branch      = tg_zib_nfe_forn-branch.

    APPEND tg_proc.
  ENDLOOP.


ENDFORM.

FORM f_proc_manifesto .

  LOOP AT tg_proc.

    SELECT SINGLE *
      FROM zib_nfe_forn INTO @DATA(_wl_zib_nfe_forn)
     WHERE nu_chave EQ @tg_proc-chave.

    CHECK ( sy-subrc EQ 0 ).

    FREE: zcl_manifesto_dest.
    CREATE OBJECT zcl_manifesto_dest.

    zcl_manifesto_dest->set_chave( tg_proc-chave ).
    zcl_manifesto_dest->set_cd_operacao( '210200' ).
    zcl_manifesto_dest->set_cnpj_dest( tg_proc-dest_cnpj ).
    zcl_manifesto_dest->set_ie_dest( tg_proc-dest_ie ).
    zcl_manifesto_dest->set_bukrs( tg_proc-bukrs ).
    zcl_manifesto_dest->set_branch( tg_proc-branch ).

    TRY.

        _wl_zib_nfe_forn-manifesto_env = abap_true.
        ADD 1 TO _wl_zib_nfe_forn-qtde_env_man.
        MODIFY zib_nfe_forn FROM _wl_zib_nfe_forn.
        COMMIT WORK.

        zcl_manifesto_dest->gravar_manifesto( RECEIVING e_doc_manifesto = DATA(vl_doc_manifesto) ).
        IF ( vl_doc_manifesto IS NOT INITIAL ).
          TRY.
              zcl_manifesto_dest->enviar_manifesto( i_sem_confirmacao =  'X' ).
            CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
              ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          ENDTRY.

          _wl_zib_nfe_forn-doc_manifesto = vl_doc_manifesto.
          MODIFY zib_nfe_forn FROM _wl_zib_nfe_forn.
          COMMIT WORK.
        ENDIF.

      CATCH zcx_manifesto_dest INTO ex_man_dest.
        ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM f_proc_manifesto_env.

  CLEAR: tg_zib_nfe_forn_env[].

  SELECT *
    FROM zib_nfe_forn INTO TABLE tg_zib_nfe_forn_env
   WHERE manifesto_env EQ abap_true
     AND stop_env_man  EQ abap_false
    AND dt_emissao >= lv_data_man.                             "BUG 67933.

  CHECK tg_zib_nfe_forn_env[] IS NOT INITIAL.

  LOOP AT tg_zib_nfe_forn_env.

    CHECK tg_zib_nfe_forn_env-doc_manifesto IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0127 INTO @DATA(_wl_zsdt0127)
     WHERE chave         EQ @tg_zib_nfe_forn_env-nu_chave
       AND doc_manifesto EQ @tg_zib_nfe_forn_env-doc_manifesto.

    CHECK sy-subrc EQ 0.

    IF _wl_zsdt0127-autorizado EQ abap_true.
      tg_zib_nfe_forn_env-stop_env_man = abap_true.
      MODIFY zib_nfe_forn FROM tg_zib_nfe_forn_env.
      CONTINUE.
    ELSE.

      TRY.
          FREE: zcl_manifesto_dest.

          CREATE OBJECT zcl_manifesto_dest
            EXPORTING
              i_chave         = tg_zib_nfe_forn_env-nu_chave
              i_doc_manifesto = tg_zib_nfe_forn_env-doc_manifesto.

          zcl_manifesto_dest->enviar_manifesto( i_sem_confirmacao =  'X'  ).

        CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
          ex_man_dest->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      ADD 1 TO tg_zib_nfe_forn_env-qtde_env_man.

      IF tg_zib_nfe_forn_env-qtde_env_man >= 3.
        tg_zib_nfe_forn_env-stop_env_man = abap_true.
      ENDIF.
      MODIFY zib_nfe_forn FROM tg_zib_nfe_forn_env.

    ENDIF.

  ENDLOOP.


ENDFORM.


FORM f_tratar_ie USING p_ie.

  DATA: vl_ie_num TYPE p.

  CHECK p_ie IS NOT INITIAL.

  CLEAR: vl_ie_num.

  REPLACE ALL OCCURRENCES OF  '.'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '/'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '\'  IN p_ie  WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF  '-'  IN p_ie  WITH '' IGNORING CASE.

  CONDENSE p_ie  NO-GAPS.

  TRY.
      vl_ie_num  = p_ie.
      p_ie       = vl_ie_num.
      CONDENSE p_ie NO-GAPS.
    CATCH cx_sy_conversion_no_number.
    CATCH cx_sy_conversion_overflow.
  ENDTRY.

ENDFORM.
