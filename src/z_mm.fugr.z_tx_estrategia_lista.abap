FUNCTION z_tx_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(V_CHAVE_NFE) LIKE  ZMMT0149-CHAVE_NFE OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZMMT0149
*"      T_ESTRA STRUCTURE  ZMM_ESTRATEGIA_TAXA OPTIONAL
*"      T_DOCS STRUCTURE  ZIB_NFE_DIST_TER OPTIONAL
*"----------------------------------------------------------------------
  TYPE-POOLS: icon.

  TYPES:

    BEGIN OF ty_estra ,
      chave_nfe  TYPE zmmt0151-chave_nfe,
      nivel      TYPE zmmt0151-nivel,
      aprovador  TYPE zmmt0151-aprovador,
      data_atual TYPE zmmt0151-data_atual,
      hora_atual TYPE zmmt0151-hora_atual,
      motivo     TYPE zmmt0151-motivo,
      valor_de   TYPE zmmt0151-valor_de,
      valor_ate  TYPE zmmt0151-valor_ate,
      usuario    TYPE zmmt0151-usuario,
      estado(4),
      opcoes(4),
    END OF ty_estra,

    BEGIN OF ty_docs ,
      chave_nfe       TYPE zib_nfe_dist_ter-chave_nfe,
      numero          TYPE zib_nfe_dist_ter-numero,
      ctr_valor_total TYPE zib_nfe_dist_ter-ctr_valor_total,
      vl_total        TYPE zib_nfe_dist_ter-vl_total,
      p_emissor       TYPE zib_nfe_dist_ter-p_emissor, "-LFA1-Name1
    END OF ty_docs,

    BEGIN OF ty_zmmt0149,
      chave_nfe  TYPE zmmt0149-chave_nfe,
      bukrs      TYPE zmmt0149-bukrs,
      status     TYPE zmmt0149-status,
      info_wkurs TYPE zmmt0149-info_wkurs,
      calc_wkurs TYPE zmmt0149-calc_wkurs,
      desvio     TYPE zmmt0149-desvio,
      data_atual TYPE zmmt0149-data_atual,
      hora_atual TYPE zmmt0149-hora_atual,
      usuario    TYPE zmmt0149-usuario,
    END OF ty_zmmt0149,

    BEGIN OF ty_zmmt0150,
      bukrs        TYPE  zmmt0150-bukrs,
      bukrs_ate    TYPE  zmmt0150-bukrs_ate,
      nivel        TYPE  zmmt0150-nivel,
      aprovador    TYPE  zmmt0150-aprovador,
      dt_val_de    TYPE  zmmt0150-dt_val_de,
      hr_val_de    TYPE  zmmt0150-hr_val_de,
      dt_val_ate   TYPE  zmmt0150-dt_val_ate,
      hr_val_ate   TYPE  zmmt0150-hr_val_ate,
      valor_de     TYPE  zmmt0150-valor_de,
      valor_ate    TYPE  zmmt0150-valor_ate,
      data_atual   TYPE  zmmt0150-data_atual,
      hora_atual   TYPE  zmmt0150-hora_atual,
      usuario      TYPE  zmmt0150-usuario,
      motivo       TYPE  zmmt0150-motivo,
      transf_aprov TYPE  zmmt0150-transf_aprov,
    END OF ty_zmmt0150,

    BEGIN OF ty_zmmt0151,
      chave_nfe  TYPE zmmt0151-chave_nfe,
      nivel      TYPE zmmt0151-nivel,
      aprovador  TYPE zmmt0151-aprovador,
      valor_de   TYPE zmmt0151-valor_de,
      valor_ate  TYPE zmmt0151-valor_ate,
      data_atual TYPE zmmt0151-data_atual,
      hora_atual TYPE zmmt0151-hora_atual,
      usuario    TYPE zmmt0151-usuario,
    END OF ty_zmmt0151.

  DATA: vflag(1),
        vvalor_ate TYPE zmmt0151-valor_ate.

  DATA:
    wa_zmmt0149      TYPE ty_zmmt0149,
    wa_zmmt0150      TYPE ty_zmmt0150,
    wa_zmmt0151      TYPE ty_zmmt0151,
    wa_estra         TYPE ty_estra,
    wa_docs          TYPE ty_docs,

    v_ukurs          TYPE tcurr-ukurs,
    e_row_id         TYPE sy-tabix,
    v_ic_set_state   TYPE c,
    v_append_ordem   TYPE c,
    produto_desc(40) TYPE c,
    tg_docs          TYPE TABLE OF ty_docs,
    it_zmmt0149      TYPE TABLE OF ty_zmmt0149,
    it_zmmt0150      TYPE TABLE OF ty_zmmt0150,
    it_zmmt0151      TYPE TABLE OF ty_zmmt0151,
    it_estra         TYPE TABLE OF ty_estra,
    it_docs          TYPE TABLE OF ty_docs,

    BEGIN OF tg_ordens OCCURS 0,
      status     TYPE sy-ucomm,
      bukrs      TYPE zmmt0149-bukrs,
      chave_nfe  TYPE zmmt0149-chave_nfe,
      info_wkurs TYPE zmmt0149-info_wkurs,
      calc_wkurs TYPE zmmt0149-calc_wkurs,
      desvio     TYPE zmmt0149-desvio,
      usuario    TYPE zmmt0149-usuario,

    END OF tg_ordens.

  DATA vflg_ico(1).


  "SELEÇÃO
  IF v_chave_nfe IS INITIAL.
    SELECT *
         FROM zmmt0149
         INTO  CORRESPONDING FIELDS OF TABLE it_zmmt0149
         WHERE status EQ 'L'.

  ELSE.
    SELECT *
         FROM zmmt0149
         INTO  CORRESPONDING FIELDS OF TABLE it_zmmt0149
         WHERE  chave_nfe = v_chave_nfe.
  ENDIF.

  CHECK it_zmmt0149[] IS NOT INITIAL.


  SELECT *
         FROM zmmt0150
         INTO CORRESPONDING FIELDS OF TABLE it_zmmt0150
         FOR ALL ENTRIES IN it_zmmt0149
             WHERE bukrs     LE it_zmmt0149-bukrs
               AND bukrs_ate GE it_zmmt0149-bukrs.


  SELECT chave_nfe
         numero
         ctr_valor_total
         vl_total
         p_emissor
    FROM zib_nfe_dist_ter
     INTO TABLE tg_docs
    FOR ALL ENTRIES IN it_zmmt0149
    WHERE chave_nfe = it_zmmt0149-chave_nfe.

  SORT it_zmmt0150 BY bukrs bukrs_ate nivel.
  SORT it_zmmt0149 BY chave_nfe.
  SORT it_zmmt0151 BY aprovador.

  FREE: it_estra.

  LOOP AT it_zmmt0149 INTO wa_zmmt0149.

    tg_ordens-chave_nfe   =   wa_zmmt0149-chave_nfe .
    tg_ordens-bukrs       =   wa_zmmt0149-bukrs.
    tg_ordens-status      =   wa_zmmt0149-status  .
    tg_ordens-info_wkurs  =   wa_zmmt0149-info_wkurs.
    tg_ordens-calc_wkurs  =   wa_zmmt0149-calc_wkurs.
    tg_ordens-desvio      =   wa_zmmt0149-desvio .
    tg_ordens-usuario     =   wa_zmmt0149-usuario .

    CASE wa_zmmt0149-status.
      WHEN ''.
        tg_ordens-status = icon_green_light.  "'Aprovado'.
      WHEN ''.
        tg_ordens-status = icon_defect.       "'Reprovado'.
      WHEN ''.
        tg_ordens-status = icon_light_out.    "'Bloqueado'.
      WHEN 'L'.
        tg_ordens-status = icon_yellow_light. "'Aguardando Aprovação'.
    ENDCASE.

    vflg_ico = 'N'.
    vvalor_ate = 0.

    LOOP AT it_zmmt0150 INTO wa_zmmt0150.
      IF wa_zmmt0150-bukrs_ate IS INITIAL.
        IF wa_zmmt0150-bukrs NE wa_zmmt0149-bukrs.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zmmt0150-bukrs     GT wa_zmmt0149-bukrs OR
             wa_zmmt0150-bukrs_ate LT wa_zmmt0149-bukrs.
        CONTINUE.
      ENDIF.
      IF  wa_zmmt0150-dt_val_de  LT sy-datum AND
          wa_zmmt0150-dt_val_ate GT sy-datum
          OR
          wa_zmmt0150-dt_val_de  EQ sy-datum AND
          wa_zmmt0150-dt_val_ate EQ sy-datum AND
          wa_zmmt0150-hr_val_de  LE sy-uzeit AND
          wa_zmmt0150-hr_val_ate GE sy-uzeit
         OR
          wa_zmmt0150-dt_val_de  EQ sy-datum AND
          wa_zmmt0150-dt_val_ate GT sy-datum AND
          wa_zmmt0150-hr_val_de  LE sy-uzeit
         OR
          wa_zmmt0150-dt_val_de  LT sy-datum AND
          wa_zmmt0150-dt_val_ate EQ sy-datum AND
          wa_zmmt0150-hr_val_ate GE sy-uzeit.
*        IF xtotal > vvalor_ate.
*          vvalor_ate = wa_zmmt0150-valor_ate.
*        ENDIF.
      ENDIF.
    ENDLOOP.

    wa_estra-chave_nfe  = wa_zmmt0149-chave_nfe.
    wa_estra-valor_de   = wa_zmmt0150-valor_de.
    wa_estra-valor_ate  = wa_zmmt0150-valor_ate.
    wa_estra-nivel      = wa_zmmt0150-nivel.

    READ TABLE it_zmmt0151 INTO wa_zmmt0151 WITH KEY chave_nfe = wa_zmmt0149-chave_nfe  BINARY SEARCH.

    IF sy-subrc = 0.
      wa_estra-estado     = icon_checked.
      wa_estra-opcoes     = icon_system_undo.
      vflg_ico            = 'N'.
      wa_estra-aprovador  = wa_zmmt0151-aprovador.
    ELSEIF vflg_ico       = 'S'.
      wa_estra-estado     = icon_led_yellow.
      wa_estra-opcoes     = ''.
      wa_estra-aprovador  = wa_zmmt0150-aprovador.
    ELSE.
      IF v_usuario NE wa_zmmt0150-aprovador.
        wa_estra-estado   = ''.
        wa_estra-opcoes   = icon_led_yellow.
      ELSE.
        wa_estra-estado   = icon_led_yellow.
        wa_estra-opcoes   = icon_set_state.
      ENDIF.
      vflg_ico = 'X'.
      wa_estra-aprovador = wa_zmmt0150-aprovador.
    ENDIF.

    IF vflg_ico = 'X'.
      vflg_ico  = 'S'.
    ENDIF.

    APPEND wa_estra TO it_estra.

    DATA(_count_nivel) = 0.
    IF v_chave_nfe IS INITIAL.
      SORT it_estra BY chave_nfe aprovador.
      CLEAR: e_row_id, v_ic_set_state, v_append_ordem.
      LOOP AT it_estra INTO wa_estra
         WHERE aprovador = v_usuario
           AND chave_nfe     = tg_ordens-chave_nfe.

        IF wa_estra-aprovador = v_usuario.
          ADD 1 TO _count_nivel.
        ENDIF.

        IF e_row_id IS NOT INITIAL AND sy-tabix NE e_row_id + 1.
          EXIT.
        ELSEIF e_row_id IS NOT INITIAL AND sy-tabix EQ e_row_id + 1.
          IF wa_estra-opcoes NE icon_system_undo.
            v_ic_set_state = 'X'.
          ENDIF.
          e_row_id = sy-tabix.
        ELSE.
          e_row_id = sy-tabix.
        ENDIF.

        LOOP AT it_estra INTO DATA(wl_estra2) WHERE chave_nfe = tg_ordens-chave_nfe.
          IF wl_estra2-estado NE icon_checked AND sy-tabix LT e_row_id.
            v_append_ordem = 'N'.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    IF _count_nivel > 1.
      CLEAR: v_append_ordem.
    ENDIF.

    IF v_append_ordem = 'N'.
      CONTINUE.
    ENDIF.

    APPEND tg_ordens.
    CLEAR tg_ordens.

  ENDLOOP.

  IF tg_ordens[] IS NOT INITIAL.
    SORT it_estra BY chave_nfe aprovador.
    LOOP AT tg_ordens.
      CLEAR vflag.
      LOOP AT it_estra INTO wa_estra WHERE chave_nfe    = tg_ordens-chave_nfe
                                       AND aprovador = v_usuario.
        vflag = 'X'.
        EXIT.
      ENDLOOP.
      LOOP AT it_estra INTO wa_estra WHERE chave_nfe = tg_ordens-chave_nfe.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.
      SORT t_estra BY chave_nfe nivel.
      IF vflag = 'X'.
        LOOP AT it_zmmt0149 INTO wa_zmmt0149 WHERE chave_nfe = tg_ordens-chave_nfe.
          LOOP AT tg_docs INTO wa_docs WHERE chave_nfe = wa_zmmt0149-chave_nfe.

            MOVE-CORRESPONDING wa_docs TO t_docs.
            APPEND t_docs.
          ENDLOOP.

        ENDLOOP.
        MOVE-CORRESPONDING tg_ordens TO t_lotes.
        APPEND t_lotes.
      ENDIF.

    ENDLOOP.

    IF t_lotes[] IS NOT INITIAL.
      msg = 'Sucesso'.
    ELSE.
      msg = 'Não há solicitações à aprovar.'.
    ENDIF.

  ENDIF.
ENDFUNCTION.
