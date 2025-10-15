FUNCTION z_ov_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USUARIO) LIKE  SY-UNAME
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"     VALUE(I_VISUALIZAR) TYPE  FLAG DEFAULT SPACE
*"     VALUE(I_TESTE) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ORD_VENDAS_EST
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_OV OPTIONAL
*"      T_ITENS STRUCTURE  ZSD_ITENS_OV_EST OPTIONAL
*"----------------------------------------------------------------------

**----------------------------------------------------------------------*
*  TYPE POOLS
**----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:  BEGIN OF ty_ordens.
            INCLUDE TYPE zsd_ord_vendas_est.
  TYPES:  END OF ty_ordens.

  TYPES:  BEGIN OF ty_estra.
            INCLUDE TYPE zsd_estrategia_ov.
  TYPES:  END OF ty_estra,

  BEGIN OF ty_itens.
    INCLUDE TYPE zsd_itens_ov_est.
  TYPES:  END OF ty_itens,

  BEGIN OF ty_zsdt0116.
    INCLUDE TYPE zsdt0116.
  TYPES:  END OF ty_zsdt0116,

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

  BEGIN OF ty_zsdt0141,
    bukrs      TYPE zsdt0141-bukrs,
    bukrs_ate  TYPE zsdt0141-bukrs_ate,
    vkbur      TYPE zsdt0141-vkbur,
    vkbur_ate  TYPE zsdt0141-vkbur_ate,
    waers      TYPE zsdt0141-waers,
    nivel      TYPE zsdt0141-nivel,
    aprovador  TYPE zsdt0141-aprovador,
    valor_de   TYPE zsdt0141-valor_de,
    valor_ate  TYPE zsdt0141-valor_ate,
    dt_val_de  TYPE zsdt0141-dt_val_de,
    dt_val_ate TYPE zsdt0141-dt_val_ate,
    hr_val_de  TYPE zsdt0141-hr_val_de,
    hr_val_ate TYPE zsdt0141-hr_val_ate,
  END OF ty_zsdt0141,

  BEGIN OF ty_zsdt0142,
    bukrs         TYPE zsdt0142-bukrs,
    vbeln         TYPE zsdt0142-vbeln,
    "165578 - 15.07.2025 - RAMON -->
    seq           TYPE zsdt0142-seq,
    "165578 - 15.07.2025 - RAMON --<
    nivel         TYPE zsdt0142-nivel,
    aprovador     TYPE zsdt0142-aprovador,
    valor_de      TYPE zsdt0142-valor_de,
    valor_ate     TYPE zsdt0142-valor_ate,

    "165578 - 23.07.2025 - RAMON -->
    vlr_foto_acum TYPE zsdt0142-vlr_foto_acum,
    "165578 - 23.07.2025 - RAMON --<

  END OF ty_zsdt0142,

  " 12.06.2025 - RAMON - 174339 -->
*  BEGIN OF ty_tcurr,
*    kurst TYPE tcurr-kurst,
*    fcurr TYPE tcurr-fcurr,
*    tcurr TYPE tcurr-tcurr,
*    gdatu TYPE tcurr-gdatu,
*    ukurs TYPE tcurr-ukurs,
*  END OF ty_tcurr,

  " 12.06.2025 - RAMON - 174339 --<

  BEGIN OF ty_t005,
    land1 TYPE t005-land1,
    waers TYPE t005-waers,
  END OF   ty_t005,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
    land1 TYPE t001-land1,
  END OF ty_t001.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  DATA: xtotal     TYPE vbap-netwr,
        lv_exc,
        "xtotal_1   TYPE vbap-netwr, " 16.05.2025 - 174339 - RAMON
        vvalor_ate TYPE zsdt0142-valor_ate,
        tg_ordens  TYPE TABLE OF ty_ordens WITH HEADER LINE,
        vflag(1).

** Criação de tabela dinamica
  DATA:
    wa_t005     TYPE ty_t005,
*    wa_tcurr    TYPE ty_tcurr, " 12.06.2025 - RAMON - 174339 -->
    wa_zsdt0116 TYPE ty_zsdt0116,
    wa_vbak     TYPE ty_vbak,
    wa_kna1     TYPE ty_kna1,
    wa_vbap     TYPE ty_vbap,
    wa_mara     TYPE ty_mara,
    wa_zsdt0041 TYPE ty_zsdt0041,
    wa_zsdt0141 TYPE ty_zsdt0141,
    wa_zsdt0142 TYPE ty_zsdt0142,
    wa_t001     TYPE ty_t001,
    wa_estra    TYPE ty_estra,
    wa_itens    TYPE ty_itens,
    tg_itens    TYPE TABLE OF ty_itens,
    it_zsdt0116 TYPE TABLE OF ty_zsdt0116,
    it_vbak     TYPE TABLE OF ty_vbak,
    it_kna1     TYPE TABLE OF ty_kna1,
    it_vbap     TYPE TABLE OF ty_vbap,
    it_mara     TYPE TABLE OF ty_mara,
    it_zsdt0041 TYPE TABLE OF ty_zsdt0041,
    it_zsdt0141 TYPE TABLE OF ty_zsdt0141,
    it_zsdt0142 TYPE TABLE OF ty_zsdt0142,
    it_t001     TYPE TABLE OF ty_t001,
    it_t005     TYPE TABLE OF ty_t005,
    "t_tcurr     TYPE TABLE OF ty_tcurr, " 12.06.2025 - RAMON - 174339
    it_estra    TYPE TABLE OF ty_estra.

  DATA lv_usuario TYPE sy-uname.

  RANGES: r_vbeln FOR vbak-vbeln.
  RANGES r_stat_wf FOR zsdt0116-status_workflow.

  lv_usuario = i_usuario.

  IF i_vbeln IS NOT INITIAL.
    r_vbeln-sign   = 'I'.
    r_vbeln-option = 'EQ'.
    APPEND r_vbeln.
  ENDIF.

  DATA vflg_ico(1).

  IF i_teste = abap_true.
    APPEND 'IEQL' TO r_stat_wf.
  ENDIF.

  "Seleção
  IF i_vbeln IS INITIAL.
    SELECT *
      FROM zsdt0116 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0116
     WHERE status NE 'X'
       AND status_workflow = 'L'.
  ELSE.
    SELECT *
      FROM zsdt0116 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0116
     WHERE vbeln           EQ i_vbeln
       AND status          NE 'X'
       AND status_workflow IN r_stat_wf.
  ENDIF.

  SORT it_zsdt0116 BY vbeln seq.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0116 COMPARING vbeln seq.

  CHECK it_zsdt0116[] IS NOT INITIAL.

  SELECT *
    FROM vbak INTO CORRESPONDING FIELDS OF TABLE it_vbak
     FOR ALL ENTRIES IN it_zsdt0116
   WHERE vbeln EQ it_zsdt0116-vbeln.

  CHECK it_vbak[] IS NOT INITIAL.


  SELECT vbap~*
    FROM vbap
    INNER JOIN vbep
      ON vbap~vbeln = vbep~vbeln
      AND vbap~posnr = vbep~posnr
    INTO CORRESPONDING FIELDS OF TABLE @it_vbap
     FOR ALL ENTRIES IN @it_zsdt0116
   WHERE vbap~vbeln EQ @it_zsdt0116-vbeln
    AND vbep~lifsp NE '12' .

  CHECK it_vbap[] IS NOT INITIAL.

  SELECT *
    FROM mara INTO CORRESPONDING FIELDS OF TABLE it_mara
     FOR ALL ENTRIES IN it_vbap
   WHERE matnr EQ it_vbap-matnr.

  CHECK it_mara[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0041 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0041
     FOR ALL ENTRIES IN it_zsdt0116
   WHERE vbeln EQ it_zsdt0116-vbeln.

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

  " " 12.06.2025 - RAMON - 174339 -->
*  SELECT kurst fcurr tcurr gdatu ukurs
*    FROM tcurr INTO CORRESPONDING FIELDS OF TABLE t_tcurr
*     FOR ALL ENTRIES IN it_t005
*   WHERE kurst EQ 'B'
*     AND fcurr EQ 'USD'
*     AND tcurr EQ it_t005-waers.
*
*  CHECK t_tcurr[] IS NOT INITIAL.
  " " 12.06.2025 - RAMON - 174339 --<

  "Estrategia de Liberação - Salvas
  "165578 - 15.07.2025 - RAMON -->

  SELECT *
    FROM zi_est_zsdt0142 INTO CORRESPONDING FIELDS OF TABLE @it_zsdt0142
    FOR ALL ENTRIES IN @it_zsdt0116
    WHERE vbeln EQ @it_zsdt0116-vbeln
      AND seq EQ  @it_zsdt0116-seq.
*  SELECT *
*    FROM zsdt0142 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0142
*    FOR ALL ENTRIES IN it_zsdt0116
*    WHERE vbeln EQ it_zsdt0116-vbeln.

  "165578 - 15.07.2025 - RAMON --<

  "Estrategia de Liberação - Parâmetros
  SELECT  *
    FROM zsdt0141 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0141
     FOR ALL ENTRIES IN it_vbak
   WHERE bukrs     LE it_vbak-vkorg
     AND bukrs_ate GE it_vbak-vkorg
     AND vkbur     LE it_vbak-vkbur
     AND vkbur_ate GE it_vbak-vkbur
      AND waers     EQ 'USD'. " Rubenilson Pereira - 10.02.25 - US165578 / DESCOMENTADO: 19.05.2025 - 174339 - RAMON
  "AND waers     EQ it_vbak-waerk." Rubenilson Pereira - 10.02.25 - US165578

  " 19.05.2025 - 174339 - RAMON -->

  SELECT * FROM zsd_in_est_limite_ov_01
    INTO TABLE @DATA(lt_acumulado)
      FOR ALL ENTRIES IN @it_vbak
        WHERE vbeln EQ @it_vbak-vbeln.

**  SELECT * FROM zi_est_ov_acumu_05
**    INTO TABLE @DATA(lt_acumulado)
**      FOR ALL ENTRIES IN @it_vbak
**        WHERE vbeln EQ @it_vbak-vbeln.
  " 19.05.2025 - 174339 - RAMON --<

*  IF I_VBELN IS INITIAL.
*    IT_ZSDT0141_DEP[] = IT_ZSDT0141[].
*    DELETE IT_ZSDT0141_DEP WHERE APROVADOR NE LV_usuario.
*    SORT IT_ZSDT0141_DEP BY BUKRS BUKRS_ATE VKBUR VKBUR_ATE.
*    DELETE ADJACENT DUPLICATES FROM IT_ZSDT0141_DEP COMPARING BUKRS BUKRS_ATE VKBUR VKBUR_ATE.
*  ENDIF.

  SORT: it_t001             BY bukrs,
        it_vbak             BY vbeln,
        it_vbap             BY vbeln posnr,
        it_zsdt0141         BY bukrs bukrs_ate vkbur vkbur_ate nivel,
        it_zsdt0142         BY vbeln nivel aprovador,
        it_zsdt0116         BY vbeln,
        it_t005             BY land1,
        "t_tcurr             BY tcurr gdatu, " 12.06.2025 - RAMON - 174339
        it_kna1             BY kunnr.

  CLEAR: tg_ordens[], tg_itens[], it_estra[].

  LOOP AT it_zsdt0116 INTO wa_zsdt0116.

    CLEAR: tg_ordens, wa_vbak, wa_t001, wa_t005 , wa_vbap, wa_kna1.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0116-vbeln.

    CHECK sy-subrc = 0.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_vbak-vkorg BINARY SEARCH.
    CONCATENATE wa_vbak-vkorg '-' wa_t001-butxt INTO  tg_ordens-empresa SEPARATED BY space.

    tg_ordens-vbeln = wa_zsdt0116-vbeln.

    "165578 - 15.07.2025 - RAMON -->
    tg_ordens-seq = wa_zsdt0116-seq.
    "165578 - 15.07.2025 - RAMON -->

    xtotal = 0.

    "xtotal_1 = 0.

    READ TABLE it_t005 INTO wa_t005 WITH KEY land1 =  wa_t001-land1 BINARY SEARCH.

    " 12.06.2025 - RAMON - 174339 -->
*    LOOP AT t_tcurr INTO wa_tcurr WHERE tcurr = wa_t005-waers.
*      EXIT.
*    ENDLOOP.
    " 12.06.2025 - RAMON - 174339 --<

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_zsdt0116-vbeln.
      " Rubenilson Pereira - 10.02.25 - US165578
      xtotal = xtotal + wa_vbap-netwr.
      " Rubenilson Pereira - 10.02.25 - US165578
    ENDLOOP.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.

    IF sy-subrc = 0.
      CONCATENATE wa_kna1-kunnr '-' wa_kna1-name1
             INTO tg_ordens-ds_cliente SEPARATED BY space.
    ENDIF.

*    tg_ordens-netwr         = xtotal_1.
    tg_ordens-netwr         = xtotal. " Rubenilson Pereira - 10.02.25 - US165578
    tg_ordens-netwr_usd     = xtotal.
    tg_ordens-ernam         = wa_zsdt0116-user_apv.
    tg_ordens-just_workflow = wa_zsdt0116-just_workflow.

    IF tg_ordens-just_workflow IS NOT INITIAL.
      tg_ordens-just_icon = icon_display_more.
    ELSE.
      tg_ordens-just_icon = icon_enter_more.
    ENDIF.

    " 19.05.2025 - 174339 - RAMON  -->
    IF wa_zsdt0116-vlr_liberado IS NOT INITIAL.

      IF wa_vbak-waerk <> 'USD'.
        tg_ordens-netwr = wa_zsdt0116-vlr_liberado_moeda.
      ELSE.
        tg_ordens-netwr = wa_zsdt0116-vlr_liberado.
      ENDIF.

    ENDIF.

    READ TABLE lt_acumulado ASSIGNING FIELD-SYMBOL(<fs_acumulado>)
      WITH KEY vbeln = wa_zsdt0116-vbeln.

    IF sy-subrc EQ 0.
      tg_ordens-vlr_acumulado = <fs_acumulado>-valor_acumulado_apr.
      tg_ordens-vlr_ac_sap = <fs_acumulado>-valor_acumulado_sap.
      tg_ordens-vlr_ac_opus = <fs_acumulado>-valor_acumulado_opus.
    ENDIF.

    " 19.05.2025 - 174339 - RAMON  --<

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

    tg_ordens-waerk         = wa_vbak-waerk.

    IF xtotal = 0.
      CONTINUE.
    ENDIF.

    " 19.05.2025 - 174339 - RAMON  -->
    IF tg_ordens-vlr_acumulado > 0.
      xtotal = tg_ordens-vlr_acumulado.
    ENDIF.
    " 19.05.2025 - 174339 - RAMON  --<

    "Inicio
    vvalor_ate = 0.
    vflg_ico   = 'N'.

    LOOP AT it_zsdt0141 INTO wa_zsdt0141 WHERE bukrs     LE wa_vbak-vkorg
                                           AND bukrs_ate GE wa_vbak-vkorg
                                           AND vkbur     LE wa_vbak-vkbur
                                           AND vkbur_ate GE wa_vbak-vkbur
                                          AND waers = 'USD'.
      "AND waers     EQ wa_vbak-waerk. " Rubenilson Pereira - 10.02.25 - US165578

      IF  wa_zsdt0141-bukrs_ate IS INITIAL.
        IF  wa_zsdt0141-bukrs NE wa_vbak-vkorg.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0141-bukrs     GT wa_vbak-vkorg OR
             wa_zsdt0141-bukrs_ate LT wa_vbak-vkorg.
        CONTINUE.
      ENDIF.

      IF  wa_zsdt0141-vkbur_ate IS INITIAL.
        IF  wa_zsdt0141-vkbur NE wa_vbak-vkbur.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0141-vkbur     GT wa_vbak-vkbur OR
             wa_zsdt0141-vkbur_ate LT wa_vbak-vkbur.
        CONTINUE.
      ENDIF.

      IF ( ( wa_zsdt0141-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
             wa_zsdt0141-dt_val_ate GT sy-datum )
           OR
          ( wa_zsdt0141-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0141-dt_val_ate EQ sy-datum AND
            wa_zsdt0141-hr_val_de  LE sy-uzeit AND
            wa_zsdt0141-hr_val_ate GE sy-uzeit )
          OR
          ( wa_zsdt0141-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0141-dt_val_ate GT sy-datum AND
            wa_zsdt0141-hr_val_de  LE sy-uzeit )
          OR
          ( wa_zsdt0141-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0141-dt_val_ate EQ sy-datum AND
            wa_zsdt0141-hr_val_ate GE sy-uzeit ) ).
        IF xtotal > vvalor_ate.
          vvalor_ate = wa_zsdt0141-valor_ate.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT it_zsdt0141 INTO wa_zsdt0141 WHERE bukrs     LE wa_vbak-vkorg
                                           AND bukrs_ate GE wa_vbak-vkorg
                                           AND vkbur     LE wa_vbak-vkbur
                                           AND vkbur_ate GE wa_vbak-vkbur
                                           AND waers = 'USD'.
      "AND waers     EQ wa_vbak-waerk. " Rubenilson Pereira - 10.02.25 - US165578


      IF  wa_zsdt0141-bukrs_ate IS INITIAL.
        IF  wa_zsdt0141-bukrs NE wa_vbak-vkorg.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0141-bukrs     GT wa_vbak-vkorg OR
             wa_zsdt0141-bukrs_ate LT wa_vbak-vkorg.
        CONTINUE.
      ENDIF.

      IF  wa_zsdt0141-vkbur_ate IS INITIAL.
        IF  wa_zsdt0141-vkbur NE wa_vbak-vkbur.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0141-vkbur     GT wa_vbak-vkbur OR
             wa_zsdt0141-vkbur_ate LT wa_vbak-vkbur.
        CONTINUE.
      ENDIF.

      IF
        ( wa_zsdt0141-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0141-dt_val_de  LT sy-datum AND
          wa_zsdt0141-dt_val_ate GT sy-datum )
        OR
        ( wa_zsdt0141-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0141-dt_val_de  EQ sy-datum AND
          wa_zsdt0141-dt_val_ate EQ sy-datum AND
          wa_zsdt0141-hr_val_de  LE sy-uzeit AND
          wa_zsdt0141-hr_val_ate GE sy-uzeit )
        OR
        ( wa_zsdt0141-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0141-dt_val_de  EQ sy-datum AND
          wa_zsdt0141-dt_val_ate GT sy-datum AND
          wa_zsdt0141-hr_val_de  LE sy-uzeit )
        OR
        ( wa_zsdt0141-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0141-dt_val_de  LT sy-datum AND
          wa_zsdt0141-dt_val_ate EQ sy-datum AND
          wa_zsdt0141-hr_val_ate GE sy-uzeit ).

        wa_estra-bukrs        = wa_vbak-vkorg.
        wa_estra-vbeln        = wa_vbak-vbeln.
        wa_estra-seq          = wa_zsdt0116-seq. "165578 - 15.07.2025 - RAMON -->
        wa_estra-valor_de     = wa_zsdt0141-valor_de.
        wa_estra-valor_ate    = wa_zsdt0141-valor_ate.
        wa_estra-nivel        = wa_zsdt0141-nivel.
        wa_estra-waerk        = wa_zsdt0141-waers.

        CLEAR lv_exc. " 21.07.2025 - RAMON - 174339

        READ TABLE it_zsdt0142 INTO wa_zsdt0142 WITH KEY vbeln     = wa_zsdt0116-vbeln
                                                         seq       = wa_zsdt0116-seq "165578 - 15.07.2025 - RAMON -->
                                                         nivel     = wa_zsdt0141-nivel.
        "BINARY SEARCH.

        IF sy-subrc = 0.
          wa_estra-estado       = icon_checked.
          wa_estra-opcoes       = icon_system_undo .
          vflg_ico = 'N'.
          wa_estra-aprovador    = wa_zsdt0142-aprovador.
          wa_estra-vlr_foto_acum = wa_zsdt0142-vlr_foto_acum.
        ELSEIF vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
          wa_estra-aprovador    = wa_zsdt0141-aprovador.
        ELSE.
          IF lv_usuario NE wa_zsdt0141-aprovador.

            " 21.07.2025 - RAMON - 174339 -->
            lv_exc = 'X'.
            IF i_visualizar = abap_false.
              EXIT.
            ENDIF.
            " 21.07.2025 - RAMON - 174339 --<

            wa_estra-estado     =  ' '.
            wa_estra-opcoes     = icon_led_yellow  .

          ELSE.
            wa_estra-estado     = icon_led_yellow .
            wa_estra-opcoes     = icon_set_state  .
          ENDIF.
          vflg_ico = 'X'.
          wa_estra-aprovador    = wa_zsdt0141-aprovador.
        ENDIF.

        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.

        APPEND wa_estra TO it_estra.
*
      ENDIF.
    ENDLOOP.

    " 21.07.2025 - RAMON - 174339 -->
    IF lv_exc = abap_true AND i_visualizar = abap_false.
      lv_exc = abap_false.
      CONTINUE.
    ENDIF.
    " 21.07.2025 - RAMON - 174339 --<

    APPEND tg_ordens.
    CLEAR tg_ordens.
  ENDLOOP.


  IF tg_ordens[] IS NOT INITIAL.

    " 12.06.2025 - RAMON - 174339 -->
    SELECT * FROM zi_est_simu_ov
      INTO TABLE @DATA(lt_simu)
        FOR ALL ENTRIES IN @tg_ordens
          WHERE vbeln = @tg_ordens-vbeln.
    " 12.06.2025 - RAMON - 174339 --<

    SORT it_estra BY vbeln aprovador.

    LOOP AT tg_ordens.
      CLEAR vflag.
      LOOP AT it_estra INTO wa_estra WHERE vbeln     = tg_ordens-vbeln
                                       AND seq = tg_ordens-seq    " 17.07.2025 - RAMON - 174339
                                       AND aprovador = lv_usuario.
        vflag = 'X'.
        EXIT.
      ENDLOOP.

      LOOP AT it_estra INTO wa_estra WHERE vbeln = tg_ordens-vbeln
                                       AND seq = tg_ordens-seq.    " 17.07.2025 - RAMON - 174339.
        CLEAR: t_estra.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.

      SORT t_estra BY vbeln nivel.
      IF vflag = 'X' OR i_teste = abap_true." 16.06.2025 - RAMON - 174339
        LOOP AT it_vbap INTO wa_vbap WHERE vbeln = tg_ordens-vbeln.
          xtotal  = 0.

          READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbap-vbeln.

          CHECK sy-subrc = 0.

          ADD wa_vbap-netwr TO xtotal.

          wa_itens-bukrs             = wa_vbak-vkorg.
          wa_itens-vkbur             = wa_vbak-vkbur.
          wa_itens-vbeln             = wa_vbap-vbeln.
          wa_itens-seq               = tg_ordens-seq. " 17.07.2025 - RAMON - 174339
          wa_itens-posnr             = wa_vbap-posnr.
          wa_itens-waerk             = wa_vbak-waerk.
          wa_itens-netwr             = wa_vbap-netwr.
          wa_itens-arktx             = wa_vbap-arktx.
          wa_itens-meins             = wa_vbap-meins.
          wa_itens-zmeng             = wa_vbap-kwmeng.
          wa_itens-matnr             = wa_vbap-matnr.
          IF wa_vbak-waerk EQ 'USD'.
            wa_itens-netwr_usd = wa_vbap-netwr.
          ELSE.

            " 12.06.2025 - RAMON - 174339 -->

            READ TABLE lt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>)
              WITH KEY vbeln = wa_itens-vbeln.

            IF sy-subrc EQ 0.
              wa_itens-netwr_usd = ( wa_vbap-netwr / <fs_simu>-kurrf ).
            ENDIF.


*            IF wa_tcurr-ukurs > 0.
*              wa_itens-netwr_usd = ( wa_vbap-netwr / wa_tcurr-ukurs ).
*            ELSE.
*              wa_itens-netwr_usd = ( wa_vbap-netwr * abs( wa_tcurr-ukurs ) ).
*            ENDIF.
            " 12.06.2025 - RAMON - 174339 --<
          ENDIF.

          TRY .
              wa_itens-wrkst  = it_mara[ matnr = wa_vbap-matnr ]-wrkst.
            CATCH cx_sy_itab_line_not_found.
              wa_itens-wrkst = ''.
          ENDTRY.

          TRY .
              wa_itens-doc_simulacao = it_zsdt0041[ vbeln = wa_vbap-vbeln ]-doc_simulacao.
            CATCH cx_sy_itab_line_not_found.
              wa_itens-doc_simulacao = ''.
          ENDTRY.

          APPEND wa_itens TO t_itens.

        ENDLOOP.
        MOVE-CORRESPONDING tg_ordens TO t_ordens.

        APPEND t_ordens.
      ENDIF.
    ENDLOOP.

    " 12.06.2025 - RAMON - 174339 -->
    PERFORM f_check_valor_estrat TABLES t_ordens t_estra.
    " 12.06.2025 - RAMON - 174339 --<

    IF t_ordens[] IS NOT INITIAL.
      e_msg = 'Sucesso'.
    ELSE.
      e_msg = 'Não há Ordens de Venda à aprovar.'.
    ENDIF.

  ENDIF.


ENDFUNCTION.
