FUNCTION z_sd_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(V_LOTE) LIKE  ZSDT0151-LOTE OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ROMA_IMP
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_SD OPTIONAL
*"----------------------------------------------------------------------


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:
    BEGIN OF ty_estra.
      INCLUDE STRUCTURE zsd_estrategia_sd.
      TYPES:  mark TYPE c,
    END OF ty_estra.

  TYPES: BEGIN OF ty_tcurr,
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
         END OF ty_t001.
  DATA tg_ordens TYPE TABLE OF zsd_roma_imp WITH HEADER LINE.

*********************************************************************************************
* Variáveis
*********************************************************************************************
  DATA: xtotal      TYPE zsdt0151-total,
        vvalor_ate  TYPE zsdt0152-valor_ate,
        vwaerk      TYPE vbak-waerk,
        vflag(1),
        vflg_ico(1).


*********************************************************************************************
* Tabelas / Workarea
*********************************************************************************************
  DATA: it_zsdt0151 TYPE TABLE OF zsdt0151,
        it_zsdt0152 TYPE TABLE OF zsdt0152,
        it_zsdt0153 TYPE TABLE OF zsdt0153,
        it_t001w    TYPE TABLE OF t001w,
        it_estra    TYPE TABLE OF ty_estra,
        it_t001     TYPE TABLE OF ty_t001,
        it_t005     TYPE TABLE OF ty_t005,
        t_tcurr     TYPE TABLE OF ty_tcurr.

  DATA: wa_zsdt0151 TYPE zsdt0151,
        wa_zsdt0152 TYPE zsdt0152,
        wa_zsdt0153 TYPE zsdt0153,
        wa_t001w    TYPE t001w,
        wa_vbak     TYPE vbak,
        wa_zsdt0001 TYPE zsdt0001,
        wa_estra    TYPE ty_estra,
        wa_t001     TYPE ty_t001,
        wa_t005     TYPE ty_t005,
        wa_tcurr    TYPE ty_tcurr.
*********************************************************************************************
* Seleciona ordens/romaneio para aprovação
*********************************************************************************************
  IF v_lote IS INITIAL.
    SELECT *
      FROM zsdt0151
      INTO TABLE it_zsdt0151
      WHERE status = ''.
  ELSE.
    SELECT *
    FROM zsdt0151
    INTO TABLE it_zsdt0151
    WHERE lote = v_lote.
  ENDIF.

  CHECK it_zsdt0151[] IS NOT INITIAL.

  SELECT *
    FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_zsdt0151
   WHERE  werks         = it_zsdt0151-werks.

  SORT it_t001w BY werks.

  SELECT *
    FROM zsdt0153
    INTO TABLE it_zsdt0153
   FOR ALL ENTRIES IN it_zsdt0151
   WHERE werks         = it_zsdt0151-werks
   AND   vbeln         = it_zsdt0151-vbeln
   AND   ch_referencia = it_zsdt0151-ch_referencia.

  SORT it_zsdt0153 BY lote nivel.

  SELECT *
    FROM zsdt0152
    INTO TABLE it_zsdt0152
     FOR ALL ENTRIES IN it_zsdt0151
    WHERE werks     LE it_zsdt0151-werks
    AND   werks_ate GE it_zsdt0151-werks.

  SELECT bukrs butxt land1
      FROM t001
      INTO TABLE it_t001
      FOR ALL ENTRIES IN it_zsdt0151
      WHERE  bukrs EQ it_zsdt0151-vkorg.

  SELECT land1 waers
     FROM t005
     INTO TABLE it_t005
     FOR ALL ENTRIES IN it_t001
     WHERE land1 = it_t001-land1.

  SELECT kurst fcurr tcurr gdatu ukurs
    FROM tcurr
    INTO TABLE t_tcurr
    FOR ALL ENTRIES IN it_t005
    WHERE kurst = 'B'
    AND   fcurr EQ 'USD'
    AND   tcurr EQ it_t005-waers.


  SORT: it_zsdt0152 BY vkorg werks werks_ate nivel waers,
        it_t001             BY bukrs,
        it_t005             BY land1,
        t_tcurr             BY tcurr gdatu.

  LOOP AT it_zsdt0151 INTO wa_zsdt0151.
    "
    vvalor_ate = 0.
    vflg_ico = 'N'.
    CLEAR wa_vbak.
    SELECT SINGLE *
       FROM vbak
       INTO wa_vbak
       WHERE vbeln = wa_zsdt0151-vbeln.
    vwaerk = wa_vbak-waerk.
    IF vwaerk = 'BRL'.
      READ TABLE  it_zsdt0152 INTO wa_zsdt0152 WITH KEY vkorg = wa_zsdt0151-vkorg
                                                        waers = wa_vbak-waerk.
      IF sy-subrc NE 0.
        vwaerk  = 'USD'.
      ENDIF.
    ENDIF.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zsdt0151-vkorg BINARY SEARCH.
    READ TABLE it_t005 INTO wa_t005 WITH KEY land1 =  wa_t001-land1 BINARY SEARCH.
    LOOP AT t_tcurr INTO wa_tcurr WHERE tcurr = wa_t005-waers.
      EXIT.
    ENDLOOP.

    xtotal = abs( wa_zsdt0151-saldo ).
***    IF WA_VBAK-WAERK = 'BRL' AND VWAERK = 'USD' .
***      XTOTAL = ABS( WA_ZSDT0151-SALDO ) / WA_TCURR-UKURS.
***    ENDIF.

    IF wa_vbak-waerk = 'BRL' AND vwaerk = 'USD' .
      xtotal = abs( wa_zsdt0151-saldo ) / wa_tcurr-ukurs.
    ENDIF.

    IF xtotal < '0.01'.
      xtotal = 1.
    ENDIF.

    LOOP AT it_zsdt0152 INTO wa_zsdt0152 WHERE vkorg = wa_zsdt0151-vkorg
                                         AND   waers = vwaerk.

      IF  wa_zsdt0152-werks_ate IS INITIAL.
        IF  wa_zsdt0152-werks NE wa_zsdt0151-werks.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0152-werks     GT wa_zsdt0151-werks OR
             wa_zsdt0152-werks_ate LT wa_zsdt0151-werks.
        CONTINUE.
      ENDIF.

      IF ( ( wa_zsdt0152-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0152-dt_val_ate GT sy-datum )
           OR
          ( wa_zsdt0152-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0152-dt_val_ate EQ sy-datum AND
            wa_zsdt0152-hr_val_de  LE sy-uzeit AND
            wa_zsdt0152-hr_val_ate GE sy-uzeit )
          OR
          ( wa_zsdt0152-dt_val_de  EQ sy-datum AND              "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0152-dt_val_ate GT sy-datum AND
            wa_zsdt0152-hr_val_de  LE sy-uzeit )
          OR
          ( wa_zsdt0152-dt_val_de  LT sy-datum AND              "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zsdt0152-dt_val_ate EQ sy-datum AND
            wa_zsdt0152-hr_val_ate GE sy-uzeit ) ).
        IF xtotal > vvalor_ate.
          vvalor_ate = wa_zsdt0152-valor_ate.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR wa_zsdt0001.
    IF wa_zsdt0151-ch_referencia IS NOT INITIAL.
      SELECT SINGLE * FROM zsdt0001 INTO wa_zsdt0001 WHERE ch_referencia = wa_zsdt0151-ch_referencia.
    ENDIF.

    "retorne ordens
    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0151-werks BINARY SEARCH.
    CONCATENATE wa_zsdt0151-werks '-' wa_t001w-name1 INTO  tg_ordens-empresa.
    tg_ordens-lote           = wa_zsdt0151-lote.
    tg_ordens-vbeln          = wa_zsdt0151-vbeln.
    tg_ordens-ch_referencia  = wa_zsdt0151-ch_referencia.
    tg_ordens-nr_romaneio    = wa_zsdt0001-nr_romaneio.
    tg_ordens-saldo          = wa_zsdt0151-saldo.
    tg_ordens-limite         = conv #( wa_zsdt0151-limite ). "Ja esta em BRL
    IF  wa_vbak-waerk = 'USD'.
      tg_ordens-total          = 0.
    ELSE.
      tg_ordens-total          = wa_zsdt0151-total.
    ENDIF.

    tg_ordens-total_est      = xtotal.
    tg_ordens-lfimg          = wa_zsdt0151-lfimg.
    tg_ordens-matnr          = wa_zsdt0151-matnr.
    tg_ordens-kunnr          = wa_zsdt0151-kunnr.
    tg_ordens-waers          = wa_vbak-waerk. "moeda da Fatura

    "
    LOOP AT it_zsdt0152 INTO wa_zsdt0152   WHERE vkorg = wa_zsdt0151-vkorg
                                           AND   waers = vwaerk.
      IF  wa_zsdt0152-werks_ate IS INITIAL.
        IF  wa_zsdt0152-werks NE wa_zsdt0151-werks.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zsdt0152-werks     GT wa_zsdt0151-werks OR
             wa_zsdt0152-werks_ate LT wa_zsdt0151-werks.
        CONTINUE.
      ENDIF.

      IF
        ( wa_zsdt0152-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0152-dt_val_de  LT sy-datum AND
          wa_zsdt0152-dt_val_ate GT sy-datum )
        OR
        ( wa_zsdt0152-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0152-dt_val_de  EQ sy-datum AND
          wa_zsdt0152-dt_val_ate EQ sy-datum AND
          wa_zsdt0152-hr_val_de  LE sy-uzeit AND
          wa_zsdt0152-hr_val_ate GE sy-uzeit )
        OR
        ( wa_zsdt0152-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0152-dt_val_de  EQ sy-datum AND
          wa_zsdt0152-dt_val_ate GT sy-datum AND
          wa_zsdt0152-hr_val_de  LE sy-uzeit )
        OR
        ( wa_zsdt0152-valor_ate  <= vvalor_ate AND            "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zsdt0152-dt_val_de  LT sy-datum AND
          wa_zsdt0152-dt_val_ate EQ sy-datum AND
          wa_zsdt0152-hr_val_ate GE sy-uzeit ).

        wa_estra-werks        = wa_zsdt0151-werks.
        "
        IF wa_zsdt0151-ch_referencia IS NOT INITIAL.
          SELECT SINGLE * FROM zsdt0001 INTO wa_zsdt0001 WHERE ch_referencia = wa_zsdt0151-ch_referencia.
          wa_estra-nr_romaneio  = wa_zsdt0001-nr_romaneio.
        ENDIF.
        "
        wa_estra-lote             = wa_zsdt0151-lote.
        wa_estra-vbeln            = wa_zsdt0151-vbeln.
        wa_estra-ch_referencia    = wa_zsdt0151-ch_referencia.

*        WA_ESTRA-VALOR_DE     = WA_ZSDT0152-VALOR_DE.
        WRITE wa_zsdt0152-valor_de TO  wa_estra-valor_de CURRENCY 'BRL'.
        SHIFT  wa_estra-valor_de LEFT DELETING LEADING space.

*        WA_ESTRA-VALOR_ATE    = WA_ZSDT0152-VALOR_ATE.
        WRITE wa_zsdt0152-valor_ate TO  wa_estra-valor_ate CURRENCY 'BRL'.
        SHIFT  wa_estra-valor_ate LEFT DELETING LEADING space.


        wa_estra-aprovador    = wa_zsdt0152-aprovador.       "modificação 03.01.2017
        wa_estra-nivel        = wa_zsdt0152-nivel.
        wa_estra-waers        = wa_zsdt0152-waers.


        READ TABLE it_zsdt0153 INTO wa_zsdt0153 WITH KEY lote          = wa_zsdt0151-lote
                                                         nivel         = wa_zsdt0152-nivel BINARY SEARCH.

        IF sy-subrc = 0.
          wa_estra-estado       = icon_checked .
          wa_estra-opcoes       = icon_system_undo .
          vflg_ico = 'N'.
          wa_estra-aprovador    = wa_zsdt0153-aprovador.         "modificação 03.01.2017
        ELSEIF vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
          wa_estra-aprovador    = wa_zsdt0152-aprovador.         "modificação 03.01.2017
        ELSE.
          IF v_usuario NE wa_zsdt0152-aprovador OR xtotal >  vvalor_ate.
            wa_estra-estado       =  ' '.
            wa_estra-opcoes       = icon_led_yellow  .
            IF xtotal >  vvalor_ate.
              wa_estra-opcoes       = icon_led_red.
            ENDIF.
          ELSE.
            wa_estra-estado       = icon_led_yellow .
            wa_estra-opcoes       = icon_set_state  .
          ENDIF.
          vflg_ico = 'X'.
          wa_estra-aprovador    = wa_zsdt0152-aprovador.         "modificação 03.01.2017
        ENDIF.

        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.

        APPEND wa_estra TO it_estra.
*
      ENDIF.
    ENDLOOP.


    APPEND tg_ordens.
    CLEAR tg_ordens.

  ENDLOOP.

  IF tg_ordens[] IS NOT INITIAL.
    SORT it_estra BY lote aprovador.
    LOOP AT tg_ordens.
      CLEAR vflag.
      LOOP AT it_estra INTO wa_estra WHERE lote      = tg_ordens-lote
                                     AND   aprovador = v_usuario.
        vflag = 'X'.
        EXIT.
      ENDLOOP.
      LOOP AT it_estra INTO wa_estra WHERE lote           = tg_ordens-lote.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.
      SORT  t_estra BY lote nivel.
      IF vflag = 'X'.
        MOVE-CORRESPONDING tg_ordens TO t_ordens.
        APPEND t_ordens.
      ENDIF.
    ENDLOOP.


    IF t_ordens[] IS NOT INITIAL.
      msg = 'Sucesso'.
    ELSE.
      msg = 'Não há lotes à aprovar.'.
    ENDIF.

  ENDIF.


ENDFUNCTION.
