FUNCTION z_cl_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_USUARIO) LIKE  SY-UNAME DEFAULT SY-UNAME
*"     VALUE(IV_DOCSIM) TYPE  ZSDED003 OPTIONAL
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"  TABLES
*"      T_CHECKLISTS STRUCTURE  ZSD_SIM_CHECKLIST_EST OPTIONAL
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_CHECKLIST OPTIONAL
*"      T_ITENS STRUCTURE  ZSD_ITENS_CHECKLSIT_EST OPTIONAL
*"      T_ITENS_R STRUCTURE  ZSD_ITENS_CHECKLSIT_EST_R OPTIONAL
*"----------------------------------------------------------------------

  TYPE-POOLS: icon.

  TYPES:

    BEGIN OF ty_docs ,
      doc_simulacao  TYPE zsdt0382-doc_simulacao,
      checklistid    TYPE zsdt0382-checklistid,
      checkid        TYPE zsdt0382-checkid,
      pergunta       TYPE zsdt0382-pergunta,
      tpcheck        TYPE zsdt0382-tpcheck,
      tpcond         TYPE zsdt0382-tpcond,
      tpinconf       TYPE zsdt0382-tpinconf,
      inconformidade TYPE zsdt0382-inconformidade,
      texto          TYPE zsdt0382-texto,

    END OF ty_docs,

    BEGIN OF ty_zsdt0381,
      doc_simulacao TYPE zsdt0381-doc_simulacao,
      checklistid   TYPE zsdt0381-checklistid,
      bukrs         TYPE zsdt0381-bukrs,
      status        TYPE zsdt0381-status,
      vkbur         TYPE zsdt0381-vkbur,
      descr         TYPE zsdt0381-descr,
      date_create   TYPE zsdt0381-date_create,
      time_create   TYPE zsdt0381-time_create,
      user_create   TYPE zsdt0381-user_create,
    END OF ty_zsdt0381.

  DATA: vflag(1).
  "vvalor_ate TYPE ZSDT0387-valor_ate.

  DATA:
    wa_zsdt0381      TYPE zsd_sim_checklist_est,
    wa_zsdt0385      TYPE zsdt0385,
    wa_zsdt0387      TYPE zsdt0387,
    wa_estra         TYPE zsd_estrategia_checklist,
    wa_docs          TYPE zsd_itens_checklsit_est,

    v_ukurs          TYPE tcurr-ukurs,
    e_row_id         TYPE sy-tabix,
    v_ic_set_state   TYPE c,
    v_append_ordem   TYPE c,
    produto_desc(40) TYPE c,
    tg_docs          TYPE TABLE OF zsd_itens_checklsit_est,
    it_zsdt0381      TYPE TABLE OF zsd_sim_checklist_est,
    it_zsdt0385      TYPE TABLE OF zsdt0385,
    it_zsdt0387      TYPE TABLE OF zsdt0387,
    it_estra         TYPE TABLE OF zsd_estrategia_checklist,
    it_docs          TYPE TABLE OF ty_docs,

    BEGIN OF tg_checklist OCCURS 0,
      status_icon   TYPE char4,
      status        TYPE zsdt0381-status,
      bukrs         TYPE zsdt0381-bukrs,
      vkbur         TYPE zsdt0381-vkbur,
      doc_simulacao TYPE zsdt0381-doc_simulacao,
      checklistid   TYPE zsdt0381-checklistid,
      descr         TYPE zsdt0381-descr,
      user_create   TYPE zsdt0381-user_create,
    END OF tg_checklist.

  DATA vflg_ico(1).


  "SELEÇÃO
  IF iv_docsim IS INITIAL.
    SELECT *
         FROM zi_in_chklist_sim
         INTO  CORRESPONDING FIELDS OF TABLE @it_zsdt0381
         WHERE status EQ '01'.

  ELSE.
    SELECT doc_simulacao checklistid status bukrs AS vkorg
           vkbur descr
         FROM zsdt0381
         INTO  CORRESPONDING FIELDS OF TABLE it_zsdt0381
         WHERE  doc_simulacao = iv_docsim.
  ENDIF.

  CHECK it_zsdt0381[] IS NOT INITIAL.


  SELECT *
         FROM zsdt0385
         INTO CORRESPONDING FIELDS OF TABLE it_zsdt0385
         FOR ALL ENTRIES IN it_zsdt0381
             WHERE bukrs     LE it_zsdt0381-vkorg
               AND bukrs_ate GE it_zsdt0381-vkorg.


  SELECT * FROM zsdt0382
     INTO CORRESPONDING FIELDS OF TABLE tg_docs
    FOR ALL ENTRIES IN it_zsdt0381
    WHERE doc_simulacao = it_zsdt0381-doc_simulacao.

  IF sy-subrc EQ 0.

    SELECT * FROM zsdt0380_r
      INTO TABLE t_itens_r
        FOR ALL ENTRIES IN it_zsdt0381
          WHERE checklistid = it_zsdt0381-checklistid.

  ENDIF.

  SELECT * FROM zsdt0387
     INTO TABLE it_zsdt0387
    FOR ALL ENTRIES IN it_zsdt0381
    WHERE doc_simulacao = it_zsdt0381-doc_simulacao.

  SORT it_zsdt0385 BY bukrs bukrs_ate nivel.
  SORT it_zsdt0381 BY doc_simulacao.
  SORT it_zsdt0387 BY aprovador.

  FREE: it_estra.

  LOOP AT it_zsdt0381 INTO wa_zsdt0381.

    tg_checklist-doc_simulacao   =   wa_zsdt0381-doc_simulacao .
    tg_checklist-checklistid       =   wa_zsdt0381-checklistid.
    tg_checklist-status      =   wa_zsdt0381-status  .
    tg_checklist-bukrs  =   wa_zsdt0381-vkorg.
    tg_checklist-vkbur  =   wa_zsdt0381-vkbur.
    tg_checklist-descr      =   wa_zsdt0381-descr .
    tg_checklist-user_create     =   wa_zsdt0381-user_create .

    CASE wa_zsdt0381-status.
      WHEN '02'.
        tg_checklist-status_icon = icon_green_light.  "'Aprovado'.
      WHEN '03'.
        tg_checklist-status_icon = icon_defect.       "'Reprovado'.
      WHEN ''.
        tg_checklist-status_icon = icon_light_out.    "'Bloqueado'.
      WHEN '01'.
        tg_checklist-status_icon = icon_yellow_light. "'Aguardando Aprovação'.
    ENDCASE.

    vflg_ico = 'N'.

    LOOP AT it_zsdt0385 INTO wa_zsdt0385 WHERE bukrs     LE wa_zsdt0381-vkorg
                                               AND bukrs_ate GE wa_zsdt0381-vkorg
                                               AND vkbur     LE wa_zsdt0381-vkbur
                                               AND vkbur_ate GE wa_zsdt0381-vkbur.


      " verifica se tem estrategia especifica para um escritorio de vendas
      READ TABLE it_zsdt0385 ASSIGNING FIELD-SYMBOL(<fs_especifica>)
        WITH KEY bukrs = wa_zsdt0381-vkorg
                 bukrs_ate = wa_zsdt0381-vkorg
                 vkbur = wa_zsdt0381-vkbur
                 vkbur_ate = wa_zsdt0381-vkbur
                 nivel = wa_zsdt0385-nivel.

      IF sy-subrc EQ 0.

        " se tem estrategia especifica, então só deixa seguir, se estiver nela
        CHECK <fs_especifica>-vkbur = wa_zsdt0385-vkbur
          AND <fs_especifica>-vkbur_ate = wa_zsdt0385-vkbur_ate.

      ENDIF.

      IF  wa_zsdt0385-bukrs_ate IS INITIAL.
        IF  wa_zsdt0385-bukrs NE wa_zsdt0381-vkorg.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0385-bukrs     GT wa_zsdt0381-vkorg OR
             wa_zsdt0385-bukrs_ate LT wa_zsdt0381-vkorg.
        CONTINUE.
      ENDIF.

      IF  wa_zsdt0385-vkbur_ate IS INITIAL.
        IF  wa_zsdt0385-vkbur NE wa_zsdt0381-vkbur.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0385-vkbur     GT wa_zsdt0381-vkbur OR
             wa_zsdt0385-vkbur_ate LT wa_zsdt0381-vkbur.
        CONTINUE.
      ENDIF.

      IF
        ( wa_zsdt0385-dt_val_de  LT sy-datum AND
          wa_zsdt0385-dt_val_ate GT sy-datum )
        OR
        (
          wa_zsdt0385-dt_val_de  EQ sy-datum AND
          wa_zsdt0385-dt_val_ate EQ sy-datum AND
          wa_zsdt0385-hr_val_de  LE sy-uzeit AND
          wa_zsdt0385-hr_val_ate GE sy-uzeit )
        OR
        (
          wa_zsdt0385-dt_val_de  EQ sy-datum AND
          wa_zsdt0385-dt_val_ate GT sy-datum AND
          wa_zsdt0385-hr_val_de  LE sy-uzeit )
        OR
        (
          wa_zsdt0385-dt_val_de  LT sy-datum AND
          wa_zsdt0385-dt_val_ate EQ sy-datum AND
          wa_zsdt0385-hr_val_ate GE sy-uzeit ).

        wa_estra-bukrs        = wa_zsdt0381-vkorg.
        wa_estra-vkbur        = wa_zsdt0381-vkbur.
        wa_estra-doc_simulacao = wa_zsdt0381-doc_simulacao.
        wa_estra-nivel        = wa_zsdt0385-nivel.

        READ TABLE it_zsdt0387 INTO wa_zsdt0387
          WITH KEY doc_simulacao = wa_estra-doc_simulacao
                   nivel = wa_estra-nivel.

        IF sy-subrc = 0.
          wa_estra-estado     = icon_checked.
          wa_estra-opcoes     = icon_system_undo.
          vflg_ico            = 'N'.
          wa_estra-aprovador  = wa_zsdt0387-aprovador.
        ELSEIF vflg_ico       = 'S'.
          wa_estra-estado     = icon_led_yellow.
          wa_estra-opcoes     = ''.
          wa_estra-aprovador  = wa_zsdt0385-aprovador.
        ELSE.
          IF iv_usuario NE wa_zsdt0385-aprovador.
            wa_estra-estado   = ''.
            wa_estra-opcoes   = icon_led_yellow.
          ELSE.
            wa_estra-estado   = icon_led_yellow.
            wa_estra-opcoes   = icon_set_state.
          ENDIF.
          vflg_ico = 'X'.
          wa_estra-aprovador = wa_zsdt0385-aprovador.
        ENDIF.
        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.

        APPEND wa_estra TO it_estra.
*
      ENDIF.
    ENDLOOP.

    APPEND tg_checklist.
    CLEAR tg_checklist.
  ENDLOOP.

  IF tg_checklist[] IS NOT INITIAL.
    SORT it_estra BY doc_simulacao aprovador.
    LOOP AT tg_checklist.
      CLEAR vflag.
      LOOP AT it_estra INTO wa_estra WHERE doc_simulacao    = tg_checklist-doc_simulacao
                                       AND aprovador = iv_usuario.
        vflag = 'X'.
        EXIT.
      ENDLOOP.
      LOOP AT it_estra INTO wa_estra WHERE doc_simulacao = tg_checklist-doc_simulacao.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.
      SORT t_estra BY doc_simulacao nivel.
      IF vflag = 'X'.

        LOOP AT it_zsdt0381 INTO wa_zsdt0381 WHERE doc_simulacao = tg_checklist-doc_simulacao.

          LOOP AT tg_docs INTO wa_docs WHERE doc_simulacao = wa_zsdt0381-doc_simulacao.

            MOVE-CORRESPONDING wa_docs TO t_itens.

            t_itens-bukrs = wa_zsdt0381-vkorg.
            t_itens-vkbur = wa_zsdt0381-vkbur.

            CASE wa_docs-tpcheck .
              WHEN 'C'.
                t_itens-radio_sim = '@0O@'.
              WHEN 'T'.
                t_itens-radio_sim = '@0L@'.
              WHEN OTHERS.

                IF t_itens-flag_sim IS INITIAL.
                  t_itens-radio_sim = '@SR@'.
                ELSE.
                  t_itens-radio_sim = '@R6@'.
                ENDIF.

                IF t_itens-flag_nao IS INITIAL.
                  t_itens-radio_nao = '@SR@'.
                ELSE.
                  t_itens-radio_nao = '@R6@'.
                ENDIF.

            ENDCASE.

            APPEND t_itens.

          ENDLOOP.

        ENDLOOP.

        MOVE-CORRESPONDING tg_checklist TO t_checklists.
        MOVE-CORRESPONDING wa_zsdt0381 TO t_checklists.

        APPEND t_checklists.
      ENDIF.

    ENDLOOP.

    IF t_checklists[] IS NOT INITIAL.
      e_msg = 'Sucesso'.
    ELSE.
      e_msg = 'Não há solicitações à aprovar.'.
    ENDIF.

  ENDIF.


ENDFUNCTION.
