FUNCTION z_ov_estrategia_lista_isen_jur.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USUARIO) LIKE  SY-UNAME
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ORD_VENDAS_EST_ISEN_JUROS
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_OV OPTIONAL
*"      T_ITENS STRUCTURE  ZSD_ITENS_OV_EST_ISEN_JUR OPTIONAL
*"      T_026 STRUCTURE  ZFIT0026 OPTIONAL
*"----------------------------------------------------------------------

**----------------------------------------------------------------------*
*  TYPE POOLS
**----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:  BEGIN OF ty_ordens.
            INCLUDE TYPE zsd_ord_vendas_est_isen_juros.
  TYPES:  END OF ty_ordens.

  TYPES:  BEGIN OF ty_estra.
            INCLUDE TYPE zsd_estrategia_ov.
  TYPES:  END OF ty_estra,

  BEGIN OF ty_itens.
    INCLUDE TYPE zsd_itens_ov_est_isen_jur.
  TYPES:  END OF ty_itens,

  BEGIN OF ty_zfit186.
    INCLUDE TYPE zfit186.
  TYPES:  END OF ty_zfit186,

  BEGIN OF ty_ov_filhos,
    ov_principal TYPE vbak-vbeln,
    filho        TYPE vbak-vbeln,
  END OF ty_ov_filhos,


  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
    vkorg TYPE vbak-vkorg,
    vkbur TYPE vbak-vkbur,
    waerk TYPE vbak-waerk,
    auart TYPE vbak-auart,
    audat TYPE vbak-audat,
    ernam TYPE vbak-ernam,
    kunnr TYPE vbak-kunnr,
  END OF ty_vbak,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    wrkst TYPE mara-wrkst,
  END OF ty_mara,

  BEGIN OF ty_zsdt0041,
    vbeln         TYPE zsdt0041-vbeln,
    doc_simulacao TYPE zsdt0041-doc_simulacao,
  END OF ty_zsdt0041,

  BEGIN OF ty_vbap,
    vbeln  TYPE vbap-vbeln,
    bukrs  TYPE vbak-vkorg,
    posnr  TYPE vbap-posnr,
    netwr  TYPE vbap-netwr,
    matnr  TYPE vbap-matnr,
    arktx  TYPE vbap-arktx,
    meins  TYPE vbap-meins,
    kwmeng TYPE vbap-kwmeng,
  END OF ty_vbap,

  BEGIN OF ty_zsdt0336,
    bukrs          TYPE zsdt0336-bukrs,
    bukrs_ate      TYPE zsdt0336-bukrs_ate,
    vkbur          TYPE zsdt0336-vkbur,
    vkbur_ate      TYPE zsdt0336-vkbur_ate,
*    waers          TYPE zsdt0336-waers,
    nivel          TYPE zsdt0336-nivel,
    aprovador      TYPE zsdt0336-aprovador,
    valor_de       TYPE zsdt0336-valor_de,
    valor_ate      TYPE zsdt0336-valor_ate,
    dt_val_de      TYPE zsdt0336-dt_val_de,
    dt_val_ate     TYPE zsdt0336-dt_val_ate,
    hr_val_de      TYPE zsdt0336-hr_val_de,
    hr_val_ate     TYPE zsdt0336-hr_val_ate,
    tp_negocio_de  TYPE zdefi_tp_neg,
    tp_negocio_ate TYPE zdefi_tp_neg,
  END OF ty_zsdt0336,

  BEGIN OF ty_zsdt0337,
    bukrs              TYPE zsdt0337-bukrs,
    vbeln              TYPE zsdt0337-vbeln,
    cod_solict_isencao TYPE zsdt0337-cod_solict_isencao, "SMC 23-06-2025
    nivel              TYPE zsdt0337-nivel,
    aprovador          TYPE zsdt0337-aprovador,
    valor_de           TYPE zsdt0337-valor_de,
    valor_ate          TYPE zsdt0337-valor_ate,
    status_apr         TYPE zsdt0337-status_apr,
    data_atual         TYPE zsdt0337-data_atual,
    hora_atual         TYPE zsdt0337-hora_atual,
    valor_Brl          TYPE zsdt0337-valor_brl,
  END OF ty_zsdt0337,

  BEGIN OF ty_tcurr,
    kurst TYPE tcurr-kurst,
    fcurr TYPE tcurr-fcurr,
    tcurr TYPE tcurr-tcurr,
    gdatu TYPE tcurr-gdatu,
    ukurs TYPE tcurr-ukurs,
  END OF ty_tcurr,

  BEGIN OF ty_t005,
    land1 TYPE t005-land1,
    waers TYPE t005-waers,
  END OF   ty_t005,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
    land1 TYPE t001-land1,
  END OF ty_t001,

  BEGIN OF ty_vbeln,
    vbeln TYPE vbak-vbeln,
  END OF ty_vbeln.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  DATA: xtotal     TYPE vbap-netwr,
        xtotal_1   TYPE vbap-netwr,
        vvalor_ate TYPE zsdt0337-valor_ate,
        tg_ordens  TYPE TABLE OF ty_ordens WITH HEADER LINE,
        vflag(1).

** Criação de tabela dinamica
  DATA:
    wa_t005     TYPE ty_t005,
    wa_tcurr    TYPE ty_tcurr,
    wa_zfit186  TYPE ty_zfit186,
    wa_vbak     TYPE ty_vbak,
    wa_kna1     TYPE ty_kna1,
    wa_vbap     TYPE ty_vbap,
    wa_mara     TYPE ty_mara,
    wa_zsdt0041 TYPE ty_zsdt0041,
    wa_zsdt0336 TYPE ty_zsdt0336,
    wa_zsdt0337 TYPE ty_zsdt0337,
    wa_t001     TYPE ty_t001,
    wa_estra    TYPE ty_estra,
    wa_itens    TYPE ty_itens,
    tg_itens    TYPE TABLE OF ty_itens,
    it_zfit186  TYPE TABLE OF ty_zfit186,
    it_vbak     TYPE TABLE OF ty_vbak,
    it_kna1     TYPE TABLE OF ty_kna1,
    it_vbap     TYPE TABLE OF ty_vbap,
    it_mara     TYPE TABLE OF ty_mara,
    it_zsdt0041 TYPE TABLE OF ty_zsdt0041,
    it_zsdt0336 TYPE TABLE OF ty_zsdt0336,
    it_zsdt0337 TYPE TABLE OF ty_zsdt0337,
    it_t001     TYPE TABLE OF ty_t001,
    it_t005     TYPE TABLE OF ty_t005,
    t_tcurr     TYPE TABLE OF ty_tcurr,
    it_estra    TYPE TABLE OF ty_estra,
    lt_dd07     TYPE TABLE OF dd07v,
    lt_filhos   TYPE TABLE OF ty_ov_filhos,
    lt_vbeln    TYPE TABLE OF ty_vbeln,
    lt_vbeln2   TYPE TABLE OF ty_vbeln.

  RANGES: r_vbeln FOR vbak-vbeln.

  IF i_vbeln IS NOT INITIAL.
    r_vbeln-sign   = 'I'.
    r_vbeln-option = 'EQ'.
    r_vbeln-low    = i_vbeln.
    APPEND r_vbeln.
  ENDIF.

  DATA vflg_ico(1).

  "Seleção
  IF i_vbeln IS INITIAL.
    IF sy-tcode = 'ZFIS66'.
      SELECT *
        FROM zfit186 INTO CORRESPONDING FIELDS OF TABLE it_zfit186
       WHERE status_solicit EQ '3'. "146630 - CS2024000604  - RGA

    ELSE.
      SELECT *
        FROM zfit186 INTO CORRESPONDING FIELDS OF TABLE it_zfit186
       WHERE status_solicit EQ '1'. "146630 - CS2024000604  - RGA
    ENDIF.

  ELSE.
    SELECT *
      FROM zfit186 INTO CORRESPONDING FIELDS OF TABLE it_zfit186
     WHERE ov_principal    EQ i_vbeln
       AND status_solicit  EQ '1'.  "146630 - CS2024000604  - RGA
  ENDIF.

  SORT it_zfit186 BY ov_principal.
  DELETE ADJACENT DUPLICATES FROM it_zfit186 COMPARING ov_principal.

*  SELECT *
*    FROM zsdt0337
*    INTO TABLE @DATA(lt_zsdt0337)
*    FOR ALL ENTRIES IN @it_zfit186
*    WHERE vbeln  EQ @it_zfit186-ov_principal
*      AND aprovador  = @sy-uname.
*  IF sy-subrc IS INITIAL.
*    SORT lt_zsdt0337 BY bukrs vbeln data_atual DESCENDING hora_atual DESCENDING.
*
*    LOOP AT it_zfit186 ASSIGNING FIELD-SYMBOL(<fs_zfit186>).
*      READ TABLE lt_zsdt0337 ASSIGNING FIELD-SYMBOL(<fs_0337>)
*      WITH KEY vbeln = <fs_zfit186>-ov_principal
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        IF <fs_0337>-status_apr EQ 'L' OR
*           <fs_0337>-status_apr EQ 'R'.
*          DELETE it_zfit186 WHERE ov_principal EQ <fs_zfit186>-ov_principal.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  CHECK it_zfit186[] IS NOT INITIAL.

  DATA: lv_para TYPE c,
        lr_ovs  TYPE RANGE OF vbak-vbeln.

  LOOP AT it_zfit186 ASSIGNING FIELD-SYMBOL(<fs_zfit186>).
    WHILE lv_para IS INITIAL .

      IF lr_ovs IS INITIAL  .

        APPEND INITIAL LINE TO lr_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs>).

        <fs_ovs>-sign = 'I'.
        <fs_ovs>-option = 'EQ'.
        <fs_ovs>-low = <fs_zfit186>-ov_principal.
        " APP FIORI - ZFIS66 - Itens nao estão carregando no APP PANF #144978 - inicio
        SELECT vbeln
        FROM zsdt0090
        INTO TABLE lt_vbeln
        WHERE vbelv IN lr_ovs
          AND vbeln <> space
          AND vbeln <> <fs_zfit186>-ov_principal
          AND estorno EQ space.
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.

      ELSE.

        SELECT vbeln, vbelv
         FROM zsdt0090
         INTO TABLE @DATA(lt_filhos_ov)
         WHERE vbelv IN @lr_ovs
           AND vbeln <> @space
**           AND vbeln <> <fs_zfit186>-ov_principal
           AND estorno EQ @space.

        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ELSE.

          "remove os itens do mesmo nivel
          SORT: lt_filhos_ov BY vbeln vbelv.
          DELETE ADJACENT DUPLICATES FROM lt_filhos_ov COMPARING vbeln vbelv.

          "remove os itens duplicados
          SORT: lt_filhos_ov BY vbeln.
          DELETE ADJACENT DUPLICATES FROM lt_filhos_ov COMPARING vbeln.

          LOOP AT lt_filhos_ov ASSIGNING FIELD-SYMBOL(<fs_filhos_ov>).

            IF <fs_filhos_ov>-vbeln <> <fs_filhos_ov>-vbelv.
              APPEND CORRESPONDING #( <fs_filhos_ov> ) TO lt_vbeln.
            ENDIF.

          ENDLOOP.

        ENDIF.
        " APP FIORI - ZFIS66 - Itens nao estão carregando no APP PANF #144978 - Fim
      ENDIF.

      APPEND LINES OF lt_vbeln TO lt_vbeln2.

      IF lt_vbeln IS NOT INITIAL.
        FREE lr_ovs.
        lr_ovs = VALUE #( FOR ls_ovs IN lt_vbeln
                            ( sign   = 'I'
                            option = 'EQ'
                            low    = ls_ovs-vbeln )
                             ).
        CLEAR: lt_vbeln.
      ENDIF.

    ENDWHILE.

    IF lt_vbeln2 IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_filhos ASSIGNING FIELD-SYMBOL(<fs_filhos>).
      <fs_filhos>-ov_principal =  <fs_zfit186>-ov_principal.
      <fs_filhos>-filho        = <fs_zfit186>-ov_principal.

      LOOP AT lt_vbeln2 ASSIGNING FIELD-SYMBOL(<fs_vbeln>).
        APPEND INITIAL LINE TO lt_filhos ASSIGNING <fs_filhos>.
        <fs_filhos>-ov_principal =  <fs_zfit186>-ov_principal.
        <fs_filhos>-filho        = <fs_vbeln>.
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO lt_filhos ASSIGNING <fs_filhos>.
      <fs_filhos>-ov_principal =  <fs_zfit186>-ov_principal.
      <fs_filhos>-filho        = <fs_zfit186>-ov_principal.
    ENDIF.

    FREE: lt_vbeln,
          lr_ovs,
          lt_vbeln2.
  ENDLOOP.

  SORT lt_filhos BY ov_principal.

  SELECT *
    FROM zfit0026
    INTO TABLE @DATA(lt_zfit0026)
    FOR ALL ENTRIES IN @lt_filhos
    WHERE ( vbeln = @lt_filhos-ov_principal
       OR   vbeln = @lt_filhos-filho )
      AND obj_key_v EQ @space
      AND ajuste    EQ @space.
  IF sy-subrc IS INITIAL.

    DELETE lt_zfit0026[] WHERE vlr_juros_calc <= 0.
    CHECK lt_zfit0026[] IS NOT INITIAL.

    t_026[] = lt_zfit0026[].
    SORT lt_zfit0026 BY vbeln.
    DATA(lt_zfit0026_aux) = lt_zfit0026.
    DELETE ADJACENT DUPLICATES FROM lt_zfit0026_aux COMPARING vbeln.

    SELECT *
      FROM zsdt0041
      INTO TABLE @DATA(lt_zsdt0041)
      FOR ALL ENTRIES IN @lt_zfit0026_aux
      WHERE vbeln = @lt_zfit0026_aux-vbeln.
    IF sy-subrc IS INITIAL.
      SORT lt_zsdt0041 BY vbeln.
    ENDIF.

    IF lt_filhos IS NOT INITIAL.

      DATA(lt_filhos_aux) = lt_filhos.
      DELETE ADJACENT DUPLICATES FROM lt_filhos_aux COMPARING ov_principal.

      SELECT *
        FROM zsdt0041
        APPENDING TABLE lt_zsdt0041
        FOR ALL ENTRIES IN lt_filhos_aux
        WHERE vbeln = lt_filhos_aux-ov_principal.
      IF sy-subrc IS INITIAL.
        SORT lt_zsdt0041 BY vbeln.
      ENDIF.

    ENDIF.

  ENDIF.

  SELECT *
    FROM vbak INTO CORRESPONDING FIELDS OF TABLE it_vbak
     FOR ALL ENTRIES IN it_zfit186
   WHERE vbeln EQ it_zfit186-ov_principal.

  IF lt_filhos IS NOT INITIAL.

    SELECT *
      FROM vbak APPENDING CORRESPONDING FIELDS OF TABLE it_vbak
       FOR ALL ENTRIES IN lt_filhos
     WHERE vbeln EQ lt_filhos-filho.

  ENDIF.

  CHECK it_vbak[] IS NOT INITIAL.


  SELECT vbap~*
    FROM vbap
    INNER JOIN vbep
      ON vbap~vbeln = vbep~vbeln
      AND vbap~posnr = vbep~posnr
    INTO CORRESPONDING FIELDS OF TABLE @it_vbap
     FOR ALL ENTRIES IN @it_zfit186
   WHERE vbap~vbeln EQ @it_zfit186-ov_principal.

  CHECK it_vbap[] IS NOT INITIAL.

  SELECT *
    FROM mara INTO CORRESPONDING FIELDS OF TABLE it_mara
     FOR ALL ENTRIES IN it_vbap
   WHERE matnr EQ it_vbap-matnr.

  CHECK it_mara[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0041 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0041
     FOR ALL ENTRIES IN it_zfit186
   WHERE vbeln EQ it_zfit186-ov_principal.

  SELECT bukrs butxt land1
    FROM t001 INTO CORRESPONDING FIELDS OF TABLE it_t001
     FOR ALL ENTRIES IN it_vbak
   WHERE bukrs EQ it_vbak-vkorg.

  CHECK it_t001[] IS NOT INITIAL.

  SELECT *
    FROM kna1 INTO CORRESPONDING FIELDS OF TABLE it_kna1
     FOR ALL ENTRIES IN it_vbak
   WHERE kunnr EQ it_vbak-kunnr.

  CHECK it_kna1[] IS NOT INITIAL.

  SELECT land1 waers
    FROM t005 INTO CORRESPONDING FIELDS OF TABLE it_t005
     FOR ALL ENTRIES IN it_t001
   WHERE land1 = it_t001-land1.

  CHECK it_t005[] IS NOT INITIAL.

  SELECT kurst fcurr tcurr gdatu ukurs
    FROM tcurr INTO CORRESPONDING FIELDS OF TABLE t_tcurr
     FOR ALL ENTRIES IN it_t005
   WHERE kurst EQ 'B'
     AND fcurr EQ 'USD'
     AND tcurr EQ it_t005-waers.

  CHECK t_tcurr[] IS NOT INITIAL.

  "Estrategia de Liberação - Salvas
  SELECT *
    FROM zsdt0337 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0337
    FOR ALL ENTRIES IN it_zfit186
    WHERE vbeln EQ it_zfit186-ov_principal
      AND status_apr = '3'
    AND cod_solict_isencao EQ it_zfit186-cd_sol_isen. "SMC 23-06-2023
  IF sy-subrc IS INITIAL.

    SORT it_zsdt0337 BY bukrs vbeln cod_solict_isencao data_atual DESCENDING hora_atual DESCENDING.

    LOOP AT it_zsdt0337  ASSIGNING FIELD-SYMBOL(<fs_zsdt0337>) .

      DATA(lv_tabix) = sy-tabix.

      SELECT COUNT(*)
        FROM zsdt0337
        INTO @DATA(lv_count)
       WHERE vbeln = @<fs_zsdt0337>-vbeln
        AND cod_solict_isencao = @<fs_zsdt0337>-cod_solict_isencao "SMC - 23-06-2025
         AND nivel = '1'
         AND status_apr = '3'
         AND data_atual >= @<fs_zsdt0337>-data_atual
         AND hora_atual > @<fs_zsdt0337>-hora_atual.
      IF sy-subrc IS INITIAL.
        DELETE it_zsdt0337 INDEX lv_tabix.                           .
      ENDIF.

    ENDLOOP.

    SORT it_zsdt0337 BY vbeln nivel cod_solict_isencao.

  ENDIF.


  "Estrategia de Liberação - Parâmetros
  SELECT  *
    FROM zsdt0336 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0336
     FOR ALL ENTRIES IN it_vbak
   WHERE bukrs     LE it_vbak-vkorg
     AND bukrs_ate GE it_vbak-vkorg
     AND vkbur     LE it_vbak-vkbur
     AND vkbur_ate GE it_vbak-vkbur.
*     AND waers     EQ it_vbak-waerk. SMC 16-06-25

*  IF I_VBELN IS INITIAL.
*    IT_ZSDT0336_DEP[] = IT_ZSDT0336[].
*    DELETE IT_ZSDT0336_DEP WHERE APROVADOR NE I_USUARIO.
*    SORT IT_ZSDT0336_DEP BY BUKRS BUKRS_ATE VKBUR VKBUR_ATE.
*    DELETE ADJACENT DUPLICATES FROM IT_ZSDT0336_DEP COMPARING BUKRS BUKRS_ATE VKBUR VKBUR_ATE.
*  ENDIF.

  SORT: it_t001             BY bukrs,
        it_vbak             BY vbeln,
        it_vbap             BY vbeln posnr,
        it_zsdt0336         BY bukrs bukrs_ate vkbur vkbur_ate nivel,
        it_zsdt0337         BY vbeln nivel aprovador cod_solict_isencao,
        it_zfit186          BY ov_principal,
        it_t005             BY land1,
        t_tcurr             BY tcurr gdatu,
        it_kna1             BY kunnr.

  CLEAR: tg_ordens[], tg_itens[], it_estra[].

  LOOP AT it_zfit186 INTO wa_zfit186.

    CLEAR: tg_ordens, wa_vbak, wa_t001, wa_t005 , wa_vbap, wa_kna1.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zfit186-ov_principal.


    CHECK sy-subrc = 0.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_vbak-vkorg BINARY SEARCH.
    CONCATENATE wa_vbak-vkorg '-' wa_t001-butxt INTO  tg_ordens-empresa SEPARATED BY space.

    tg_ordens-ov_principal = wa_zfit186-ov_principal.

    xtotal = 0.
    xtotal_1 = 0.
    READ TABLE it_t005 INTO wa_t005 WITH KEY land1 =  wa_t001-land1 BINARY SEARCH.
    LOOP AT t_tcurr INTO wa_tcurr WHERE tcurr = wa_t005-waers.
      EXIT.
    ENDLOOP.
    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_zfit186-ov_principal.
      xtotal_1 = xtotal_1 + wa_vbap-netwr.
      IF wa_vbak-waerk EQ 'USD'.
        xtotal = xtotal + wa_vbap-netwr.
      ELSE.
        IF wa_tcurr-ukurs > 0.
          xtotal = xtotal + ( wa_vbap-netwr / wa_tcurr-ukurs ).
        ELSE.
          xtotal = xtotal + ( wa_vbap-netwr * abs( wa_tcurr-ukurs ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE wa_kna1-kunnr '-' wa_kna1-name1
             INTO tg_ordens-ds_cliente SEPARATED BY space.
    ENDIF.

    tg_ordens-netwr         = wa_zfit186-vl_moeda_doc. "146630 - RGA
    xtotal = tg_ordens-netwr.
    tg_ordens-netwr_usd         = wa_zfit186-vl_moeda_doc. "146630 - SMC 16-06-2025
    "146630 - SMC 16-06-2025
    IF wa_zfit186-moeda <> 'BRL'.
      tg_ordens-netwr_usd         = wa_zfit186-vl_moeda_doc. "146630 - SMC 16-06-2025
      xtotal = tg_ordens-netwr * abs( wa_tcurr-ukurs ).
    ENDIF.
    "146630 - SMC 16-06-2025


    tg_ordens-cd_sol_isen = wa_zfit186-cd_sol_isen.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZDOFI_TP_NEG'   "<-- Your Domain Here
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_dd07
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF lt_dd07 IS NOT INITIAL.
      SORT lt_dd07 BY domvalue_l.

      READ TABLE lt_dd07 ASSIGNING FIELD-SYMBOL(<fs_dd07>)
      WITH KEY domvalue_l = wa_zfit186-tipo_negocio
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_zfit186-tipo_negocio(1) '-' <fs_dd07>-ddtext INTO tg_ordens-tipo_negocio SEPARATED BY space.
      ENDIF.
    ENDIF.
*    tg_ordens-tipo_negocio    = wa_zfit186-tipo_negocio.
*    tg_ordens-netwr_usd     = wa_zfit186-moeda.
    tg_ordens-ernam         = wa_zfit186-usuario_solicit.
    tg_ordens-just_workflow = wa_zfit186-justificativa.
    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN tg_ordens-just_workflow WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN tg_ordens-just_workflow WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN tg_ordens-just_workflow WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN tg_ordens-just_workflow WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN tg_ordens-just_workflow WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN tg_ordens-just_workflow WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN tg_ordens-just_workflow WITH 'e'.
    REPLACE ALL OCCURRENCES OF        'º'     IN tg_ordens-just_workflow WITH 'o' IGNORING CASE.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = tg_ordens-just_workflow
      IMPORTING
        outtext           = tg_ordens-just_workflow
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.


    tg_ordens-moeda         = wa_zfit186-moeda.


    IF xtotal = 0.
      CONTINUE.
    ENDIF.

    "Inicio
    vvalor_ate = 0.
    vflg_ico   = 'N'.

    LOOP AT it_zsdt0336 INTO wa_zsdt0336 WHERE bukrs     LE wa_vbak-vkorg
                                           AND bukrs_ate GE wa_vbak-vkorg
                                           AND vkbur     LE wa_vbak-vkbur
                                           AND vkbur_ate GE wa_vbak-vkbur
                                           AND tp_negocio_de LE wa_zfit186-tipo_negocio
                                           AND tp_negocio_ate GE wa_zfit186-tipo_negocio.
*                                           AND waers     EQ wa_vbak-waerk. SMC 16-06-25

      IF  wa_zsdt0336-bukrs_ate IS INITIAL.
        IF  wa_zsdt0336-bukrs NE wa_vbak-vkorg.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0336-bukrs     GT wa_vbak-vkorg OR
             wa_zsdt0336-bukrs_ate LT wa_vbak-vkorg.
        CONTINUE.
      ENDIF.

      IF  wa_zsdt0336-vkbur_ate IS INITIAL.
        IF  wa_zsdt0336-vkbur NE wa_vbak-vkbur.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0336-vkbur     GT wa_vbak-vkbur OR
             wa_zsdt0336-vkbur_ate LT wa_vbak-vkbur.
        CONTINUE.
      ENDIF.

      IF ( ( wa_zsdt0336-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
             wa_zsdt0336-dt_val_ate GT sy-datum )
           OR
          ( wa_zsdt0336-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0336-dt_val_ate EQ sy-datum AND
            wa_zsdt0336-hr_val_de  LE sy-uzeit AND
            wa_zsdt0336-hr_val_ate GE sy-uzeit )
          OR
          ( wa_zsdt0336-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0336-dt_val_ate GT sy-datum AND
            wa_zsdt0336-hr_val_de  LE sy-uzeit )
          OR
          ( wa_zsdt0336-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0336-dt_val_ate EQ sy-datum AND
            wa_zsdt0336-hr_val_ate GE sy-uzeit ) ).
        IF xtotal > vvalor_ate.
          vvalor_ate = wa_zsdt0336-valor_ate.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT it_zsdt0336 INTO wa_zsdt0336 WHERE bukrs     LE wa_vbak-vkorg
                                           AND bukrs_ate GE wa_vbak-vkorg
                                           AND vkbur     LE wa_vbak-vkbur
                                           AND vkbur_ate GE wa_vbak-vkbur
                                           AND tp_negocio_de LE wa_zfit186-tipo_negocio
                                           AND tp_negocio_ate GE wa_zfit186-tipo_negocio.
*                                           AND waers     EQ wa_vbak-waerk. SMC 16-06-25


      IF  wa_zsdt0336-bukrs_ate IS INITIAL.
        IF  wa_zsdt0336-bukrs NE wa_vbak-vkorg.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0336-bukrs     GT wa_vbak-vkorg OR
             wa_zsdt0336-bukrs_ate LT wa_vbak-vkorg.
        CONTINUE.
      ENDIF.

      IF  wa_zsdt0336-vkbur_ate IS INITIAL.
        IF  wa_zsdt0336-vkbur NE wa_vbak-vkbur.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0336-vkbur     GT wa_vbak-vkbur OR
             wa_zsdt0336-vkbur_ate LT wa_vbak-vkbur.
        CONTINUE.
      ENDIF.

      IF
        ( wa_zsdt0336-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0336-dt_val_de  LT sy-datum AND
          wa_zsdt0336-dt_val_ate GT sy-datum )
        OR
        ( wa_zsdt0336-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0336-dt_val_de  EQ sy-datum AND
          wa_zsdt0336-dt_val_ate EQ sy-datum AND
          wa_zsdt0336-hr_val_de  LE sy-uzeit AND
          wa_zsdt0336-hr_val_ate GE sy-uzeit )
        OR
        ( wa_zsdt0336-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0336-dt_val_de  EQ sy-datum AND
          wa_zsdt0336-dt_val_ate GT sy-datum AND
          wa_zsdt0336-hr_val_de  LE sy-uzeit )
        OR
        ( wa_zsdt0336-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0336-dt_val_de  LT sy-datum AND
          wa_zsdt0336-dt_val_ate EQ sy-datum AND
          wa_zsdt0336-hr_val_ate GE sy-uzeit ).

        wa_estra-bukrs        = wa_vbak-vkorg.
        wa_estra-vbeln        = wa_vbak-vbeln.
        wa_estra-valor_de     = wa_zsdt0336-valor_de.
        wa_estra-valor_ate    = wa_zsdt0336-valor_ate.
        wa_estra-nivel        = wa_zsdt0336-nivel.
        wa_estra-tp_negocio_de = wa_zsdt0336-tp_negocio_de.
        wa_estra-tp_negocio_ate = wa_zsdt0336-tp_negocio_ate.
*        wa_estra-waerk        = wa_zsdt0336-waers. SMC 16-05-2025

        READ TABLE it_zsdt0337 INTO wa_zsdt0337 WITH KEY vbeln     = wa_zfit186-ov_principal
                                                         nivel     = wa_zsdt0336-nivel
                                                         cod_solict_isencao = wa_zfit186-cd_sol_isen "SMC - 23-06-2025
                                                         BINARY SEARCH.

        IF sy-subrc = 0 AND wa_zsdt0337-status_apr <> '1'.
          wa_estra-estado       = icon_checked.
          wa_estra-opcoes       = '' .
          vflg_ico = 'N'.
          wa_estra-aprovador    = wa_zsdt0337-aprovador.
        ELSEIF vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
          wa_estra-aprovador    = wa_zsdt0336-aprovador.
        ELSE.
          IF i_usuario NE wa_zsdt0336-aprovador.
            wa_estra-estado     =  ' '.
            wa_estra-opcoes     = icon_led_yellow  .
          ELSE.
            wa_estra-estado     = icon_led_yellow .
            wa_estra-opcoes     = icon_set_state  .
          ENDIF.
          vflg_ico = 'X'.
          wa_estra-aprovador    = wa_zsdt0336-aprovador.
        ENDIF.

        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.

        APPEND wa_estra TO it_estra.
*
      ENDIF.
    ENDLOOP.

* SMC - 16-05-2025 / ""146630 - RGA - 24.06.2025
    IF tg_ordens-moeda <> 'BRL'.

      SELECT SINGLE ukurs
         FROM zi_est_tcurr
         INTO @DATA(lv_ptax)
         WHERE kurst EQ 'G'
         AND fcurr   EQ @wa_zfit186-moeda
         AND tcurr   EQ 'BRL'
         AND gdatu   EQ @wa_zfit186-data.
      IF sy-subrc EQ 0.
        tg_ordens-netwr = lv_ptax * wa_zfit186-vl_moeda_doc.
      ENDIF.

    ELSE.
      tg_ordens-netwr =  wa_zfit186-vl_moeda_doc. ""146630 - RGA
    ENDIF.
* SMC - 16-05-2025

    APPEND tg_ordens.
    CLEAR tg_ordens.

  ENDLOOP.
  IF tg_ordens[] IS NOT INITIAL.
    SORT it_estra BY vbeln aprovador.
    LOOP AT tg_ordens.
      CLEAR vflag.

      IF sy-tcode EQ 'ZFIS66'.
        LOOP AT it_estra INTO wa_estra WHERE vbeln     = tg_ordens-ov_principal
                                       AND   aprovador = i_usuario.
          vflag = 'X'.
          EXIT.
        ENDLOOP.

      ELSEIF sy-tcode EQ 'ZFI0174'.
        LOOP AT it_estra INTO wa_estra WHERE vbeln     = tg_ordens-ov_principal.

          vflag = 'X'.
          EXIT.
        ENDLOOP.

      ENDIF.


      LOOP AT it_estra INTO wa_estra WHERE vbeln = tg_ordens-ov_principal.
        CLEAR: t_estra.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.

      SORT t_estra BY vbeln nivel.
      IF vflag = 'X'.

        READ TABLE lt_filhos TRANSPORTING NO FIELDS
        WITH KEY ov_principal = tg_ordens-ov_principal
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_filhos ASSIGNING <fs_filhos> FROM sy-tabix.

            IF <fs_filhos>-ov_principal <>  tg_ordens-ov_principal.
              EXIT.
            ENDIF.

            READ TABLE lt_zfit0026 TRANSPORTING NO FIELDS
            WITH KEY vbeln = <fs_filhos>-filho
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              " APP FIORI - ZFIS66 - Itens nao estão carregando no APP PANF #144978 - Inicio
              CONTINUE.
**              READ TABLE lt_zfit0026 TRANSPORTING NO FIELDS
**              WITH KEY vbeln = tg_ordens-ov_principal
**              BINARY SEARCH.
**              IF sy-subrc IS INITIAL.
**                <fs_filhos>-filho = tg_ordens-ov_principal.
**              ENDIF.
              " APP FIORI - ZFIS66 - Itens nao estão carregando no APP PANF #144978 - Fim
            ENDIF.

*            IF sy-subrc IS INITIAL.

            LOOP AT lt_zfit0026 ASSIGNING FIELD-SYMBOL(<fs_zfit0026>) FROM sy-tabix.
              IF <fs_zfit0026>-vbeln <> <fs_filhos>-filho.
                EXIT.
              ENDIF.

              xtotal  = 0.

              ADD wa_vbap-netwr TO xtotal.

              wa_itens-org_vendas     = <fs_zfit0026>-bukrs.
              wa_itens-data_venc      = <fs_zfit0026>-data_venc.
              READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <fs_zfit0026>-vbeln.

              IF sy-subrc IS INITIAL.
                wa_itens-escr_vendas    = wa_vbak-vkbur.
              ENDIF.

              READ TABLE lt_zsdt0041 ASSIGNING FIELD-SYMBOL(<fs_zsdt0041>)
              WITH KEY vbeln = tg_ordens-ov_principal
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_itens-simul_venda    = <fs_zsdt0041>-doc_simulacao.
              ENDIF.
              wa_itens-ov_principal   = tg_ordens-ov_principal.
              wa_itens-zid_lanc       = <fs_zfit0026>-zid_lanc.
              wa_itens-vbeln          = <fs_zfit0026>-vbeln.
              wa_itens-waerk          = <fs_zfit0026>-moeda.
              wa_itens-valor_ov       = <fs_zfit0026>-mont_moeda.
              wa_itens-vlr_juros_calc = <fs_zfit0026>-vlr_juros_calc.
              wa_itens-vlr_juros_rbdo = <fs_zfit0026>-vlr_juros_rbdo.
              wa_itens-vlr_desc_jros  = <fs_zfit0026>-vlr_desc_jros .
              wa_itens-saldo_juros    =  wa_itens-vlr_juros_calc -  wa_itens-vlr_juros_rbdo - wa_itens-vlr_desc_jros.

              APPEND wa_itens TO t_itens.
              CLEAR: wa_itens . "140708-APP FIORI - Aprovação de isenção de juros ZFIS66 - SMC
            ENDLOOP.

*            ENDIF.
          ENDLOOP.
        ENDIF.
        MOVE-CORRESPONDING tg_ordens TO t_ordens.

        APPEND t_ordens.
      ENDIF.
    ENDLOOP.

    IF t_ordens[] IS NOT INITIAL.
      e_msg = 'Sucesso'.
    ELSE.
      e_msg = 'Não há Ordens de Venda à aprovar.'.
    ENDIF.

  ENDIF.


ENDFUNCTION.
