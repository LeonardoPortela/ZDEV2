FUNCTION z_solic_taxa_curva.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPCAO) TYPE  CHAR03 OPTIONAL
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_NR_OV STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_DATA_VENC STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_DATA_LIB STRUCTURE  ZSDT0094 OPTIONAL
*"      IT_VKORG STRUCTURE  ZSDT0051 OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_saida,
           data_registro           TYPE zsdt0094-data_registro,
           hora_registro           TYPE zsdt0094-hora_registro,
           matnr                   TYPE mara-matnr,
           maktx                   TYPE makt-maktx,
           tpsim                   TYPE zsdt0040-tpsim,
           tp_venda                TYPE zsdt0051-tp_venda,
           zterm                   TYPE zsdt0052-zterm,
           data_progr              TYPE zsdt0055-data_progr,
           dtde_logist             TYPE zsdt0051-dtde_logist,
           formula                 TYPE kurrf,
           dmbtr                   TYPE zsdt0053-dmbtr,
           pmein                   TYPE zsdt0053-pmein,
           programa                TYPE zsdt0094-programa,
           nro_sol_ov              TYPE zsdt0094-nro_sol_ov,
           data_venc               TYPE zsdt0094-data_venc,
           data_lib                TYPE zsdt0094-data_lib,
           inco1                   TYPE zsdt0051-inco1,
           cadencia_qte            TYPE zsdt0094-cadencia_qte,
           zieme                   TYPE zsdt0094-zieme,
           total_proporc           TYPE zsdt0094-total_proporc,
           total_proporc_usd       TYPE zsdt0094-total_proporc,
           total_proporc_usd_curva TYPE zsdt0094-total_proporc,
           taxa_curva              TYPE zsdt0094-taxa_curva,
           frete_cif               TYPE zsdt0094-total_proporc,
           frete_porto             TYPE zsdt0094-total_proporc,
           tipo                    TYPE zsdt0094-tipo,
           tipo_taxa               TYPE zsdt0094-tipo_taxa,
           fixacao                 TYPE zsdt0094-fixacao,
           bezei                   type zsdt0094-bezei,
           charg                   type zsdt0053-charg,
           waerk                   type zsdt0051-waerk,
           vbeln                   type zsdt0094-vbeln,
           vkorg                   type vkorg,
           kunnr                   type kunnr,
           cli_forn                type name1,
           ktokd                   type ktokd,
           intercompany            type zsdt0094-intercompany,
         END OF ty_saida.


  DATA: gt_zsdt0094 TYPE TABLE OF zsdt0094,
        gw_zsdt0094 TYPE zsdt0094.

  DATA: gt_zsdt0053 TYPE TABLE OF zsdt0053,
        gt_zsdt0051 TYPE TABLE OF zsdt0051,
        gt_zsdt0052 TYPE TABLE OF zsdt0052,
        gt_zsdt0055 TYPE TABLE OF zsdt0055,
        gt_zsdt0059 TYPE TABLE OF zsdt0059,
        gt_vkorg    TYPE TABLE OF zsdt0051,
        gt_zsdt0056 TYPE TABLE OF zsdt0056,
        gt_zsdt0073 TYPE TABLE OF zsdt0073.


  DATA: gw_zsdt0053 TYPE zsdt0053,
        gw_zsdt0051 TYPE zsdt0051,
        gw_zsdt0052 TYPE zsdt0052,
        gw_zsdt0055 TYPE zsdt0055,
        gw_zsdt0059 TYPE zsdt0059,
        gw_vkorg    TYPE zsdt0051,
        gw_zsdt0056 TYPE zsdt0056,
        gw_zsdt0073 TYPE zsdt0073.

  DATA: gt_mara TYPE TABLE OF mara,
        gw_mara TYPE mara,
        gt_makt TYPE TABLE OF makt,
        gw_makt TYPE makt.


  DATA: gt_saida TYPE TABLE OF ty_saida,
        gw_saida TYPE ty_saida.

  DATA: gw_nro_sol   TYPE zsdt0094,
        gw_data_venc TYPE zsdt0094,
        gw_data_lib  TYPE zsdt0094.

  DATA: var_taxa TYPE kurrf.
  DATA: var_len TYPE i.
  DATA: var_id      TYPE cind,
        var_true    TYPE c,
        var_feriado TYPE c.

  DATA: gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

  RANGES r_nro_sol    FOR zsdt0094-nro_sol_ov.
  RANGES r_data_venc  FOR zsdt0094-data_venc.
  RANGES r_data_lib   FOR zsdt0094-data_lib.
  RANGES r_vkorg      FOR zsdt0051-vkorg.
  RANGES r_bezei      FOR zsdt0059-bezei.

  var_linhas = lines( it_nr_ov ).
  IF var_linhas EQ 1.
    CLEAR: gw_nro_sol.
    READ TABLE it_nr_ov INTO gw_nro_sol INDEX 1.
    r_nro_sol-sign   = 'I'.
    r_nro_sol-option = 'EQ'.
    r_nro_sol-low    = gw_nro_sol-nro_sol_ov.
    r_nro_sol-high   = gw_nro_sol-nro_sol_ov.
    APPEND r_nro_sol.
  ELSEIF var_linhas > 1.
    LOOP AT it_nr_ov INTO gw_nro_sol.
      r_nro_sol-sign   = 'I'.
      r_nro_sol-option = 'EQ'.
      r_nro_sol-low    = gw_nro_sol-nro_sol_ov.
      APPEND r_nro_sol.
    ENDLOOP.
  ENDIF.

  CLEAR: var_linhas.
  DESCRIBE TABLE it_data_venc LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_data_venc.
      READ TABLE it_data_venc INTO gw_data_venc INDEX 1.
      r_data_venc-sign   = 'I'.
      r_data_venc-option = 'EQ'.
      r_data_venc-low    = gw_data_venc-data_venc.
      r_data_venc-high   = gw_data_venc-data_venc.
      APPEND r_data_venc.

    WHEN: '2'.
      CLEAR: gw_data_venc.
      READ TABLE it_data_venc INTO gw_data_venc INDEX 1.
      r_data_venc-sign   = 'I'.
      r_data_venc-option = 'BT'.
      r_data_venc-low    = gw_data_venc-data_venc.
      READ TABLE it_data_venc INTO gw_data_venc INDEX 2.
      r_data_venc-high    = gw_data_venc-data_venc.
      APPEND r_data_venc.
  ENDCASE.


  CLEAR: var_linhas.
  DESCRIBE TABLE it_data_lib LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_data_lib.
      READ TABLE it_data_lib INTO gw_data_lib INDEX 1.
      r_data_lib-sign   = 'I'.
      r_data_lib-option = 'EQ'.
      r_data_lib-low    = gw_data_lib-data_lib.
      r_data_lib-high   = gw_data_lib-data_lib.
      APPEND r_data_lib.

    WHEN: '2'.
      CLEAR: gw_data_lib.
      READ TABLE it_data_lib INTO gw_data_lib INDEX 1.
      r_data_lib-sign   = 'I'.
      r_data_lib-option = 'BT'.
      r_data_lib-low    = gw_data_lib-data_lib.
      READ TABLE it_data_lib INTO gw_data_lib INDEX 2.
      r_data_lib-high    = gw_data_lib-data_lib.
      APPEND r_data_lib.
  ENDCASE.


  CLEAR: var_linhas.
  var_linhas = lines( it_vkorg ).

  IF var_linhas EQ 1.
    CLEAR: gw_vkorg.
    READ TABLE it_vkorg INTO gw_vkorg INDEX 1.
    r_vkorg-sign   = 'I'.
    r_vkorg-option = 'EQ'.
    r_vkorg-low    = gw_vkorg-vkorg.
    r_vkorg-high   = gw_vkorg-vkorg.
    APPEND r_vkorg.
  ELSEIF var_linhas > 1.

    LOOP AT it_vkorg INTO gw_vkorg.
      r_vkorg-sign   = 'I'.
      r_vkorg-option = 'EQ'.
      r_vkorg-low    = gw_vkorg-vkorg.
      APPEND r_vkorg.
    ENDLOOP.

  ENDIF.

  "Atualizar as Taxas que foram feitas antes das 08:00 e que estão com a taxa curva com o valor 0,01. (INICIO)
  SELECT * FROM zsdt0094
    INTO TABLE gt_zsdt0094
   WHERE taxa_curva  EQ '0.01000'
      OR taxa_cambio EQ '0.01000'.

  SELECT * FROM zsdt0094
      APPENDING TABLE gt_zsdt0094
     WHERE taxa_curva  EQ space.

  IF ( sy-subrc EQ 0 ).

    LOOP AT gt_zsdt0094 INTO gw_zsdt0094.

      FREE: gobj_zcl_webservice_tx_curva.
      CREATE OBJECT gobj_zcl_webservice_tx_curva.

      IF ( gw_zsdt0094-taxa_curva EQ '0.01000' ) OR ( gw_zsdt0094-taxa_curva EQ '0.00000' ) OR ( gw_zsdt0094-taxa_curva IS INITIAL ).

        "Regara para validar data de finais de semana e feriados.
        "Recuperar a taxa curva com a data util.
        WHILE var_true IS INITIAL.

          CALL FUNCTION 'DATE_COMPUTE_DAY'
            EXPORTING
              date = gw_zsdt0094-data_lib
            IMPORTING
              day  = var_id.

          CASE var_id.
            WHEN: 6.

              ADD 2 TO gw_zsdt0094-data_lib.

              CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
                EXPORTING
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                IMPORTING
                  holiday_found                = var_feriado
                EXCEPTIONS
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  OTHERS                       = 7.

              IF NOT var_feriado IS INITIAL.
                CONTINUE.
              ELSE.
                var_true = 'X'.
              ENDIF.

            WHEN: 7.

              ADD 1 TO gw_zsdt0094-data_lib.

              CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
                EXPORTING
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                IMPORTING
                  holiday_found                = var_feriado
                EXCEPTIONS
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  OTHERS                       = 7.

              IF NOT var_feriado IS INITIAL.
                CONTINUE.
              ELSE.
                var_true = 'X'.
              ENDIF.

            WHEN OTHERS.

              CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
                EXPORTING
                  date                         = gw_zsdt0094-data_lib
                  holiday_calendar_id          = 'BR'
                IMPORTING
                  holiday_found                = var_feriado
                EXCEPTIONS
                  calendar_buffer_not_loadable = 1
                  date_after_range             = 2
                  date_before_range            = 3
                  date_invalid                 = 4
                  holiday_calendar_id_missing  = 5
                  holiday_calendar_not_found   = 6
                  OTHERS                       = 7.

              IF NOT var_feriado IS INITIAL.
                ADD 1 TO gw_zsdt0094-data_lib.
                CONTINUE.
              ELSE.
                var_true = 'X'.
              ENDIF.
          ENDCASE.
        ENDWHILE.

        var_taxa = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = gw_zsdt0094-data_venc
                                                              i_data_lib = gw_zsdt0094-data_lib  ).

        IF ( var_taxa NE '0.01000' ) OR ( var_taxa EQ '0.00000') OR ( var_taxa IS INITIAL ).

          UPDATE zsdt0094 SET taxa_curva = var_taxa
                     WHERE data_registro = gw_zsdt0094-data_registro
                       AND hora_registro = gw_zsdt0094-hora_registro
                       AND programa      = gw_zsdt0094-programa
                       AND nro_sol_ov    = gw_zsdt0094-nro_sol_ov.


          COMMIT WORK.

          IF ( gw_zsdt0094-taxa_cambio EQ '0.01000' ) OR ( var_taxa EQ '0.00000').

            UPDATE zsdt0094 SET taxa_cambio = var_taxa
                       WHERE data_registro = gw_zsdt0094-data_registro
                         AND hora_registro = gw_zsdt0094-hora_registro
                         AND programa      = gw_zsdt0094-programa
                         AND nro_sol_ov    = gw_zsdt0094-nro_sol_ov.

            COMMIT WORK.

          ENDIF.

        ENDIF.

      ELSE.

        IF ( gw_zsdt0094-taxa_cambio EQ '0.01000' ) OR ( var_taxa EQ '0.00000').

          UPDATE zsdt0094 SET taxa_cambio = gw_zsdt0094-taxa_curva
                     WHERE data_registro = gw_zsdt0094-data_registro
                       AND hora_registro = gw_zsdt0094-hora_registro
                       AND programa      = gw_zsdt0094-programa
                       AND nro_sol_ov    = gw_zsdt0094-nro_sol_ov.

          COMMIT WORK.

        ENDIF.

      ENDIF.

      CLEAR: gw_zsdt0094, var_taxa, var_true.
    ENDLOOP.
  ENDIF.

  REFRESH: gt_zsdt0094[].
  "Atualizar as Taxas que foram feitas antes das 08:00 e que estão com a taxa curva com o valor 0,01. (FIM)


  "Adicionar informações do BEZEI.
  r_bezei-sign   =  'I'.
  r_bezei-option = 'EQ'.
  r_bezei-low    = 'TAXA CAMBIO FRAME'.
  r_bezei-high   = 'TAXA CAMBIO FRAME'.
  APPEND r_bezei.

  "Adicionar informações do BEZEI.
  r_bezei-sign   =  'I'.
  r_bezei-option = 'EQ'.
  r_bezei-low    = 'TAXA CAMBIO'.
  r_bezei-high   = 'TAXA CAMBIO'.
  APPEND r_bezei.

  SELECT * FROM zsdt0056
    INTO TABLE gt_zsdt0056.
  IF ( sy-subrc EQ 0 ).

    LOOP AT gt_zsdt0056 INTO gw_zsdt0056.

      var_len  = strlen( gw_zsdt0056-bezei ).

      CASE var_len.
        WHEN: '2' OR '3'.
          IF ( gw_zsdt0056-bezei(1) EQ 'T' ).
            CLEAR: r_bezei.

            r_bezei-sign   =  'I'.
            r_bezei-option = 'EQ'.
            r_bezei-low    = gw_zsdt0056-bezei.
            r_bezei-high   = gw_zsdt0056-bezei.
            APPEND r_bezei.

          ENDIF.
        WHEN OTHERS.
          CLEAR: gw_zsdt0056, var_len.
          CONTINUE.
      ENDCASE.
      CLEAR: gw_zsdt0056, var_len.
    ENDLOOP.
  ENDIF.

  CASE i_opcao.

    WHEN: 'VDA'.
      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
        WHERE nro_sol_ov IN r_nro_sol
          AND data_venc  IN r_data_venc
          AND data_lib   IN r_data_lib
          AND tipo       EQ i_opcao.

    WHEN: 'FRE'.

      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
      WHERE nro_sol_ov IN r_nro_sol
        AND data_venc  IN r_data_venc
        AND data_lib   IN r_data_lib
        AND tipo       EQ i_opcao.

    WHEN OTHERS.

      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
      WHERE nro_sol_ov IN r_nro_sol
        AND data_venc  IN r_data_venc
        AND data_lib   IN r_data_lib.

  ENDCASE.

  CHECK NOT gt_zsdt0094[] IS INITIAL.

  SELECT * FROM zsdt0051
    INTO TABLE gt_zsdt0051
    FOR ALL ENTRIES IN gt_zsdt0094
  WHERE nro_sol_ov EQ gt_zsdt0094-nro_sol_ov
    AND vkorg      IN r_vkorg.

  SELECT * FROM zsdt0053
    INTO TABLE gt_zsdt0053
    FOR ALL ENTRIES IN gt_zsdt0094
  WHERE nro_sol_ov EQ gt_zsdt0094-nro_sol_ov
    AND fixacao    EQ gt_zsdt0094-fixacao.

  SELECT * FROM mara
    INTO TABLE gt_mara
    FOR ALL ENTRIES IN gt_zsdt0053
  WHERE matnr EQ gt_zsdt0053-matnr.

  SELECT * FROM makt
    INTO TABLE gt_makt
    FOR ALL ENTRIES IN gt_mara
  WHERE matnr EQ gt_mara-matnr
    AND spras EQ sy-langu.

  SELECT * FROM zsdt0052
    INTO TABLE gt_zsdt0052
    FOR ALL ENTRIES IN gt_zsdt0094
  WHERE nro_sol_ov EQ gt_zsdt0094-nro_sol_ov.

  SELECT * FROM zsdt0059
    INTO TABLE gt_zsdt0059
    FOR ALL ENTRIES IN gt_zsdt0053
 WHERE nro_sol_ov EQ gt_zsdt0053-nro_sol_ov
   AND bezei IN r_bezei
   AND posnr EQ gt_zsdt0053-fixacao.

  DELETE gt_zsdt0059 WHERE field EQ 'QTDFIXADA'.

  SELECT * FROM zsdt0073
    INTO TABLE gt_zsdt0073
    FOR ALL ENTRIES IN gt_zsdt0059
 WHERE nro_sol_ov   EQ gt_zsdt0059-nro_sol_ov
   AND fixacao      EQ gt_zsdt0059-posnr.

  SELECT * FROM zsdt0055
    INTO TABLE gt_zsdt0055
    FOR ALL ENTRIES IN gt_zsdt0094
  WHERE nro_sol_ov  EQ gt_zsdt0094-nro_sol_ov
    AND fixacao     EQ gt_zsdt0094-fixacao
    AND valdt_hedge EQ gt_zsdt0094-data_venc.

  SELECT * FROM zsdt0055
    APPENDING TABLE gt_zsdt0055
    FOR ALL ENTRIES IN gt_zsdt0094
  WHERE nro_sol_ov  EQ gt_zsdt0094-nro_sol_ov.

  SORT: gt_zsdt0094 BY data_registro hora_registro DESCENDING.

  LOOP AT gt_zsdt0094 INTO gw_zsdt0094.

    READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0094-nro_sol_ov.
    IF NOT ( r_vkorg IS INITIAL ) AND ( gw_zsdt0051-vkorg NOT IN r_vkorg  ).
      CONTINUE.
    ENDIF.
    gw_saida-tp_venda      = gw_zsdt0051-tp_venda.
    gw_saida-dtde_logist   = gw_zsdt0051-dtde_logist.
    gw_saida-waerk         = gw_zsdt0051-waerk.
    gw_saida-vkorg         = gw_zsdt0051-vkorg. "USER STORY 168869 / AOENNING


    IF gw_zsdt0094-inco1 IS INITIAL.
      gw_saida-inco1         = gw_zsdt0051-inco1.
    ELSE.
      gw_saida-inco1         = gw_zsdt0094-inco1.
    ENDIF.

    READ TABLE gt_zsdt0052 INTO gw_zsdt0052 WITH KEY nro_sol_ov = gw_zsdt0094-nro_sol_ov.
    gw_saida-zterm = gw_zsdt0052-zterm.


    READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0094-nro_sol_ov
                                                     fixacao    = gw_zsdt0094-fixacao.
    gw_saida-dmbtr  = gw_zsdt0053-dmbtr.
    gw_saida-pmein  = gw_zsdt0053-pmein.

    SELECT SINGLE charg
      FROM zsdt0053
      INTO gw_saida-charg
      WHERE nro_sol_ov EQ gw_zsdt0094-nro_sol_ov
        AND fixacao    EQ gw_zsdt0094-fixacao
        AND posnr EQ ( SELECT MIN( posnr )
                          FROM zsdt0053
                          WHERE nro_sol_ov EQ gw_zsdt0094-nro_sol_ov
                            AND fixacao    EQ gw_zsdt0094-fixacao
                            AND status NE 'C' ).

    gw_saida-formula = gw_zsdt0094-taxa_cambio.

    READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov  = gw_zsdt0094-nro_sol_ov
                                                     fixacao     = gw_zsdt0094-fixacao
                                                     valdt_hedge = gw_zsdt0094-data_venc.

    IF ( sy-subrc EQ 0 ).
      gw_saida-data_progr    = gw_zsdt0055-data_progr.
    ELSE.
      READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov  = gw_zsdt0094-nro_sol_ov
                                                       fixacao     = gw_zsdt0094-fixacao
                                                       data_progr  = gw_zsdt0094-data_venc.
      gw_saida-data_progr    = gw_zsdt0055-data_progr.
    ENDIF.

    gw_saida-data_registro           = gw_zsdt0094-data_registro.
    gw_saida-hora_registro           = gw_zsdt0094-hora_registro.
    gw_saida-programa                = gw_zsdt0094-programa     .
    gw_saida-nro_sol_ov              = gw_zsdt0094-nro_sol_ov   .
    gw_saida-data_venc               = gw_zsdt0094-data_venc    .
    gw_saida-data_lib                = gw_zsdt0094-data_lib     .
    gw_saida-cadencia_qte            = gw_zsdt0094-cadencia_qte .
    gw_saida-zieme                   = gw_zsdt0094-zieme        .
    gw_saida-total_proporc           = gw_zsdt0094-total_proporc.

    TRY.
        gw_saida-total_proporc_usd       = gw_zsdt0094-total_proporc / gw_zsdt0094-taxa_cambio.
        gw_saida-total_proporc_usd_curva = gw_zsdt0094-total_proporc / gw_zsdt0094-taxa_curva.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    gw_saida-taxa_curva              = gw_zsdt0094-taxa_curva   .
    gw_saida-frete_cif               = gw_zsdt0094-frete_cif    .
    gw_saida-frete_porto             = gw_zsdt0094-frete_porto  .
    gw_saida-tipo                    = gw_zsdt0094-tipo  .

    READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0053-matnr.
    gw_saida-matnr = gw_mara-matnr.
    READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
    gw_saida-maktx = gw_makt-maktx.


    "Verificar Quando é frame.
    IF ( gw_saida-zterm IS INITIAL ).
      READ TABLE gt_zsdt0073 INTO gw_zsdt0073 WITH KEY nro_sol_ov = gw_zsdt0059-nro_sol_ov
                                                       fixacao    = gw_zsdt0059-posnr.
      gw_saida-zterm  = gw_zsdt0073-zterm.
    ENDIF.

    gw_saida-tipo_taxa = gw_zsdt0094-tipo_taxa.
    gw_saida-fixacao   = gw_zsdt0094-fixacao.
    gw_saida-bezei     = gw_zsdt0094-bezei.

    APPEND gw_saida TO gt_saida .

    CLEAR: gw_saida, gw_zsdt0094, gw_zsdt0051, gw_zsdt0052, gw_zsdt0053, gw_zsdt0059, gw_zsdt0055, gw_mara, gw_makt.

  ENDLOOP.

  it_resultado[] = gt_saida[].


ENDFUNCTION.
