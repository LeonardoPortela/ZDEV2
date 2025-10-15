FUNCTION zpm_busca_ordens_orcam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  XUBNAME
*"     REFERENCE(I_AUFNR) TYPE  AUFK-AUFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LINES) TYPE  NUMC2
*"     REFERENCE(E_ORDENS_ORC) TYPE  ZTPM_T_ORC_ORDEM
*"  TABLES
*"      T_ORDENS STRUCTURE  ZPMR0003 OPTIONAL
*"----------------------------------------------------------------------

* Declaração dos tipos de dados globais
  TYPES:   BEGIN OF ty_ordens.
             INCLUDE  TYPE zpmr0003.
           TYPES:    END OF ty_ordens,

           BEGIN OF ty_resb,
             werks   TYPE resb-werks,
             aufnr   TYPE resb-aufnr,
             matnr   TYPE resb-matnr,
             bdmng   TYPE resb-bdmng,
             rsnum   TYPE resb-rsnum,
             preis   TYPE afvc-preis,
             vprsv   TYPE mbew-vprsv,
             p_valor TYPE p LENGTH 13 DECIMALS 2,
           END OF ty_resb,

           BEGIN OF ty_afvc,             "Selecão de informação operação da ordem
             aufpl TYPE afvc-aufpl,
             objnr TYPE afvc-objnr,
             vornr TYPE afvc-vornr,
             werks TYPE afvc-werks,
             banfn TYPE afvc-banfn,
             preis TYPE afvc-preis,
             aufnr TYPE afko-aufnr,
           END OF ty_afvc,

           BEGIN OF ty_ihsg,
             objnr      TYPE  ihsg-objnr,
             counter    TYPE  ihsg-counter,
             ernam      TYPE  ihsg-ernam,
             pmsog      TYPE  ihsg-pmsog,
             nivel      TYPE  zpmr0002-nivel,
             aprovador  TYPE  zpmr0002-aprovador,
             usua_subst TYPE  zpmr0002-usua_subst,
             data_lim   TYPE  zpmr0002-data_lim,
             check(1),
           END OF ty_ihsg.


  TYPES: BEGIN OF ty_ordens2.
           INCLUDE TYPE zpmr0003.
           TYPES: rowcolor(4) TYPE c,
           cell_color  TYPE lvc_t_scol,    " Cor da Célula
           sttxt       TYPE caufvd-sttxt,
           asttx       TYPE caufvd-asttx,
           icon        TYPE c LENGTH 4.
  TYPES : END OF ty_ordens2.

*** Tabelas
  DATA: gt_ordens     TYPE TABLE OF ty_ordens,
        gt_ordens2    TYPE TABLE OF ty_ordens2,
        gt_ordens_aux TYPE TABLE OF ty_ordens WITH HEADER LINE,
        it_ordens     TYPE TABLE OF ty_ordens,
        t_ordens_aux  TYPE TABLE OF zpmr0003,
        it_ihsg       TYPE TABLE OF ty_ihsg,
        lw_ordens     TYPE zpmr0003,
        objnr_        TYPE j_objnr,
        objnr_aux     TYPE j_objnr,
        nivel_        TYPE numc10,
        gt_zpmr0002   TYPE TABLE OF zpmr0002 WITH HEADER LINE,
        zt_zpmr0002   TYPE TABLE OF zpmr0002 WITH HEADER LINE,
        order_types   TYPE rsis_t_range,
        r_werks       TYPE rsis_t_range,
        ls_orc_order  TYPE ztpm_orc_ordem,
        it_resb       TYPE TABLE OF ty_resb,
        it_afvc       TYPE TABLE OF ty_afvc,
        esheader      TYPE bapi_alm_order_header_e,
        it_return     TYPE TABLE OF bapiret2,
        it_olist      TYPE TABLE OF bapi_alm_order_objectlist,
        isheader      TYPE TABLE OF bapi_alm_order_headers_i.


  SELECT *
   FROM zpmr0002
   INTO CORRESPONDING FIELDS OF TABLE gt_zpmr0002
   WHERE aprovador  EQ i_user
      OR usua_subst EQ i_user. "Anderson Oenning

  LOOP AT gt_zpmr0002 ASSIGNING FIELD-SYMBOL(<wa_estrategia>).
    IF <wa_estrategia>-aprovador IS NOT INITIAL
      AND <wa_estrategia>-usua_subst IS NOT INITIAL
      AND <wa_estrategia>-data_lim >= sy-datum.
      <wa_estrategia>-aprovador = <wa_estrategia>-usua_subst.
    ENDIF.
  ENDLOOP.

  DELETE gt_zpmr0002 WHERE aprovador NE i_user.
  CHECK gt_zpmr0002[] IS NOT INITIAL.

* Range de centro
  r_werks = VALUE #( FOR ls1 IN gt_zpmr0002 ( sign = 'I' option = 'EQ' low = ls1-centro_desp ) ).

  SELECT *
    FROM ztparam
    INTO TABLE @DATA(_parameters)
   WHERE param = 'TP_ORDEM'
     AND const IN @r_werks.

* Range de tipo de Ordens
  order_types = VALUE #( FOR ls2 IN _parameters ( sign = 'I' option = 'EQ' low = ls2-zval ) ).

  SORT order_types BY low.
  DELETE ADJACENT DUPLICATES FROM order_types COMPARING low.

  SELECT *
    FROM ihgns
    INTO TABLE @DATA(lt_ihgns)
   WHERE  genvname EQ @i_user.
  IF sy-subrc IS INITIAL.

    lt_ihgns = lt_ihgns.
    SORT lt_ihgns BY objnr.
    DELETE ADJACENT DUPLICATES FROM lt_ihgns COMPARING objnr.

    SELECT *
    FROM bpeg
    INTO TABLE @DATA(lt_bpeg)
    FOR ALL ENTRIES IN @lt_ihgns
    WHERE objnr = @lt_ihgns-objnr
      AND pldat = '00000000'
      AND vorga = 'KBUD'.
    IF sy-subrc IS INITIAL.
      SORT lt_bpeg BY objnr.

      DATA(lt_bpeg_aux) = lt_bpeg.
      SORT lt_bpeg_aux BY objnr.
      DELETE ADJACENT DUPLICATES FROM lt_bpeg_aux COMPARING objnr.

* Seleção das Ordens para aprovar
      SELECT DISTINCT a~mandt werks a~erdat h~equnr e~eqktx a~aufnr a~objnr a~ktext a~user4 a~erfzeit i~counter
                      h~iwerk i~pmsog
        INTO CORRESPONDING FIELDS OF TABLE t_ordens[]
        FROM aufk AS a
        INNER JOIN ihsg  AS i ON i~objnr EQ a~objnr
        INNER JOIN afih  AS h ON h~aufnr EQ a~aufnr
        LEFT  JOIN eqkt  AS e ON e~equnr EQ h~equnr
        FOR ALL ENTRIES IN lt_bpeg_aux
        WHERE a~objnr = lt_bpeg_aux-objnr
          AND a~auart IN order_types
          AND a~werks IN r_werks
          AND i~lvorm EQ abap_false.

    ENDIF.

  ENDIF.

* Seleção das Ordens para aprovar
  SELECT DISTINCT a~mandt werks a~erdat h~equnr e~eqktx a~aufnr a~objnr a~ktext a~user4 a~erfzeit i~counter
                  h~iwerk i~pmsog
    INTO CORRESPONDING FIELDS OF TABLE gt_ordens_aux
    FROM aufk AS a
    INNER JOIN ihsg  AS i ON i~objnr EQ a~objnr
    INNER JOIN afih  AS h ON h~aufnr EQ a~aufnr
    LEFT  JOIN eqkt  AS e ON e~equnr EQ h~equnr
    FOR ALL ENTRIES IN gt_zpmr0002
    WHERE ( a~phas0 EQ abap_true  AND
            a~phas1 EQ abap_false )
      AND a~auart IN order_types
      AND a~werks EQ gt_zpmr0002-centro_desp
      AND i~lvorm EQ abap_false.

  CHECK gt_ordens_aux[] IS NOT INITIAL.
  SORT gt_ordens_aux[] BY aufnr counter.

  LOOP AT gt_ordens_aux.
    IF objnr_aux NE gt_ordens_aux-objnr.
      objnr_aux = gt_ordens_aux-objnr.
    ELSE.
      CONTINUE.
    ENDIF.

    FREE gt_ordens.
    gt_ordens = VALUE #( FOR ls IN gt_ordens_aux WHERE ( objnr EQ gt_ordens_aux-objnr ) ( ls ) ).

* get nos dados que já estão aprovados
    SELECT *
      FROM ihgns
      INTO TABLE @DATA(gt_ihgns)
      FOR ALL ENTRIES IN @gt_ordens
      WHERE objnr   EQ @gt_ordens-objnr
       AND  counter EQ @gt_ordens-counter
       AND  geniakt EQ @abap_false.

    SORT gt_ihgns BY objnr counter ASCENDING gendatum DESCENDING gentime DESCENDING.

* get nos dados com o Nivel de cada Ordem
    FREE it_ihsg.
    SELECT DISTINCT a~objnr a~counter a~ernam a~pmsog b~nivel b~aprovador b~usua_subst b~data_lim
      FROM ihsg AS a
      INNER JOIN zpmr0002 AS b ON b~permit EQ a~pmsog
        INTO CORRESPONDING FIELDS OF TABLE it_ihsg
          FOR ALL ENTRIES IN gt_ordens
            WHERE objnr   EQ gt_ordens-objnr
             AND  counter EQ gt_ordens-counter
             AND  b~centro_desp EQ gt_ordens-werks.

    SORT it_ihsg BY objnr nivel.

* Desmarca o Check da Ordem a ser aprovada
    LOOP AT it_ihsg ASSIGNING FIELD-SYMBOL(<wa2>) WHERE check IS INITIAL.
      IF line_exists( gt_ihgns[ objnr = <wa2>-objnr counter = <wa2>-counter ] ) .
        <wa2>-check = abap_true.
      ENDIF.

      IF <wa2>-usua_subst IS NOT INITIAL AND <wa2>-data_lim >= sy-datum .
        <wa2>-aprovador = <wa2>-usua_subst. "Anderson Oenning
      ENDIF.
    ENDLOOP.

*    deleta todas as ordens com check e diferente do aprovador selecionado
    DELETE it_ihsg WHERE check IS NOT INITIAL.

    SORT it_ihsg BY objnr nivel.
    CLEAR: objnr_, nivel_.
    LOOP AT it_ihsg ASSIGNING <wa2>.
      IF objnr_ NE <wa2>-objnr AND nivel_ NE <wa2>-nivel.
        objnr_ = <wa2>-objnr.
        nivel_ = <wa2>-nivel.
        CONTINUE.
      ENDIF.

      IF objnr_ EQ <wa2>-objnr AND nivel_ EQ <wa2>-nivel.
        CONTINUE.
      ENDIF.
      <wa2>-check = abap_true.
    ENDLOOP.

* deleta todas as Ordens com Check
    DELETE it_ihsg WHERE check IS NOT INITIAL.
    DELETE it_ihsg WHERE aprovador NE i_user .

* adiciona na saida todas as Ordens que estão no IHSG
    it_ordens = VALUE #( FOR ls3 IN gt_ordens
                         FOR ls4 IN  it_ihsg WHERE ( objnr = ls3-objnr AND counter = ls3-counter )
                        ( CORRESPONDING #( ls3 ) )
                      ).

    LOOP AT it_ordens INTO DATA(ls_ordens).
      MOVE-CORRESPONDING ls_ordens TO lw_ordens.
      APPEND lw_ordens TO t_ordens[].
    ENDLOOP.
  ENDLOOP.

*  APPEND LINES OF T_ORDENS TO T_ORDENS_AUX.
*  FREE T_ORDENS.
*
*  LOOP AT GT_ZPMR0002 INTO DATA(W_0002).
*    LOOP AT T_ORDENS_AUX ASSIGNING FIELD-SYMBOL(<ORDEM>).
*      IF <ORDEM>-USER4 BETWEEN W_0002-VALOR_DE AND W_0002-VALOR_ATE.
*        CLEAR <ORDEM>-WERT2.
*        APPEND <ORDEM> TO T_ORDENS.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.

  "//Verifica se há Ordens Reprovadas
  SELECT  *
    FROM zpmr0006
    INTO TABLE @DATA(historico)
    FOR ALL ENTRIES IN @t_ordens
   WHERE aufnr  = @t_ordens-aufnr
     AND status = 'R'.

  IF sy-subrc IS INITIAL.
    LOOP AT historico  INTO DATA(wa).
      LOOP AT t_ordens ASSIGNING FIELD-SYMBOL(<wa1>) WHERE aufnr EQ wa-aufnr.
        IF ( wa-vlr_estimado EQ <wa1>-user4 ).
          <wa1>-status = 'R'.
        ELSE.
          <wa1>-status = 'P'.
*          DELETE FROM zpmr0006 WHERE aufnr = <wa1>-aufnr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF t_ordens[] IS NOT INITIAL.

      MOVE-CORRESPONDING t_ordens[] TO gt_ordens2.

      DATA(lt_ordens_aux) = t_ordens[].
      SORT lt_ordens_aux BY aufnr.
      DELETE ADJACENT DUPLICATES FROM lt_ordens_aux COMPARING aufnr.

      SELECT *
        FROM afih
        INTO TABLE @DATA(lt_afih)
        FOR ALL ENTRIES IN @lt_ordens_aux
        WHERE aufnr = @lt_ordens_aux-aufnr.
      IF sy-subrc IS INITIAL.
        SORT lt_afih BY aufnr.

        DATA(lt_afih_aux) = lt_afih.

        SORT lt_afih_aux BY iloan.
        DELETE ADJACENT DUPLICATES FROM lt_afih_aux COMPARING iloan.

        SELECT *
          FROM iloa
          INTO TABLE @DATA(lt_iloa)
          FOR ALL ENTRIES IN @lt_afih_aux
          WHERE iloan = @lt_afih_aux-iloan.
        IF sy-subrc IS INITIAL.
          SORT lt_iloa BY iloan.
        ENDIF.

      ENDIF.

      LOOP AT gt_ordens2 ASSIGNING FIELD-SYMBOL(<ls_ordens>).

        SELECT *
        FROM afko AS a
        INNER JOIN resb AS b ON b~rsnum = a~rsnum
        INTO CORRESPONDING FIELDS OF TABLE it_resb
        WHERE a~aufnr EQ <ls_ordens>-aufnr
          AND b~werks EQ <ls_ordens>-werks.


        SELECT SINGLE *
         FROM aufk
         INTO @DATA(w_aufk)
         WHERE aufnr EQ @<ls_ordens>-aufnr
           AND werks EQ @<ls_ordens>-werks.

*-> reread order status text in print language
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            flg_user_stat    = abap_true
            objnr            = w_aufk-objnr
            only_active      = abap_true
            spras            = sy-langu
          IMPORTING
            line             = <ls_ordens>-sttxt
            user_line        = <ls_ordens>-asttx
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.

*        IF <ls_ordens>-asttx IS NOT INITIAL.
*          SELECT SINGLE * FROM  tj30t INTO @DATA(w_tj30t)
*                 WHERE  stsma       = 'ZPM00010'
*                 AND    txt04       = @<ls_ordens>-asttx
*                 AND    spras       = @sy-langu.
*
*          IF w_tj30t IS NOT INITIAL.
*            <ls_ordens>-asttx =  w_tj30t-txt30 .
*          ENDIF.
*        ENDIF.


        IF it_resb IS NOT INITIAL.
          LOOP AT it_resb ASSIGNING FIELD-SYMBOL(<ls_resb>) WHERE aufnr EQ <ls_ordens>-aufnr.
            SELECT SINGLE *
            INTO @DATA(ls_mbew)
            FROM mbew
            WHERE matnr EQ @<ls_resb>-matnr
              AND bwkey EQ @<ls_resb>-werks.


            IF sy-subrc = 0.
              <ls_resb>-vprsv = ls_mbew-vprsv.

              IF <ls_resb>-vprsv = 'V'.
                <ls_resb>-preis = ls_mbew-stprs.
              ELSE.
                <ls_resb>-preis = ls_mbew-verpr.
              ENDIF.

              <ls_resb>-p_valor = ( <ls_resb>-bdmng * <ls_resb>-preis ).
            ENDIF.

            ADD <ls_resb>-p_valor TO <ls_ordens>-wert2.
          ENDLOOP.
        ENDIF.

        SELECT *
        FROM afvc AS a
        INNER JOIN afko AS b ON b~aufpl EQ a~aufpl AND b~aufnr EQ <ls_ordens>-aufnr
        INTO CORRESPONDING FIELDS OF TABLE it_afvc.

        LOOP AT it_afvc ASSIGNING FIELD-SYMBOL(<ls_afvc>) WHERE aufnr EQ <ls_ordens>-aufnr.
          IF sy-subrc = 0.
            ADD <ls_afvc>-preis TO <ls_ordens>-wert2.
          ENDIF.
        ENDLOOP.

        READ TABLE lt_afih ASSIGNING FIELD-SYMBOL(<fs_afih>)
        WITH KEY aufnr = <ls_ordens>-aufnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_iloa ASSIGNING FIELD-SYMBOL(<fs_iloa>)
          WITH KEY iloan = <fs_afih>-iloan
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <ls_ordens>-tplnr = <fs_iloa>-tplnr.
          ENDIF.

          <ls_ordens>-arbpl = <fs_afih>-gewrk.
        ENDIF.
*        CLEAR: esheader.
*        FREE: it_return, it_olist.
*        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*          EXPORTING
*            number    = <ls_ordens>-aufnr
*          IMPORTING
*            es_header = esheader
*          TABLES
*            return    = it_return
*            et_olist  = it_olist
*          EXCEPTIONS
*            OTHERS    = 01.
*
*        IF sy-subrc EQ 0.
*          <ls_ordens>-tplnr = esheader-funct_loc.
*          <ls_ordens>-arbpl = esheader-mn_wk_ctr.
*        ENDIF.

      ENDLOOP.

      DATA(lt_ordens) = gt_ordens2.
      SORT lt_ordens BY aufnr.
      DELETE ADJACENT DUPLICATES FROM lt_ordens COMPARING aufnr.

      SELECT aufnr,obs_reprov,belnr
        FROM zpmr0006
        INTO TABLE @DATA(lt_0006)
        FOR ALL ENTRIES IN @lt_ordens
        WHERE aufnr EQ @lt_ordens-aufnr.
      IF sy-subrc IS INITIAL.
        SORT lt_0006 BY aufnr.
      ENDIF.

      LOOP AT gt_ordens2 ASSIGNING <ls_ordens>.

        READ TABLE lt_bpeg TRANSPORTING NO FIELDS
        WITH KEY objnr = <ls_ordens>-objnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          LOOP AT lt_bpeg ASSIGNING FIELD-SYMBOL(<fs_bpeg>) FROM sy-tabix.
            IF <fs_bpeg>-objnr <> <ls_ordens>-objnr.
              EXIT.
            ENDIF.

            ls_orc_order-id           = <fs_bpeg>-belnr.
            ls_orc_order-aufnr        = <ls_ordens>-aufnr.
            ls_orc_order-iwerk        = <ls_ordens>-werks.
            ls_orc_order-arbpl        = <ls_ordens>-arbpl.
            ls_orc_order-ktext        = <ls_ordens>-ktext.

            READ TABLE lt_0006 ASSIGNING FIELD-SYMBOL(<fs_0006>)
            WITH KEY aufnr = <ls_ordens>-aufnr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_orc_order-short_text = <fs_0006>-obs_reprov.
            ENDIF.

            ls_orc_order-user4        = <ls_ordens>-user4.
            ls_orc_order-cost_plan    = <ls_ordens>-wert2.
            ls_orc_order-tplnr        = <ls_ordens>-tplnr.

*            CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*              EXPORTING
*                number    = <ls_ordens>-aufnr
*              IMPORTING
*                es_header = esheader
*              TABLES
*                return    = it_return
*                et_olist  = it_olist
*              EXCEPTIONS
*                OTHERS    = 01.
*            IF sy-subrc IS INITIAL.
            READ TABLE lt_afih ASSIGNING <fs_afih>
            WITH KEY aufnr = <ls_ordens>-aufnr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_orc_order-equnr = <fs_afih>-equnr.
            ENDIF.

            IF ls_orc_order-equnr IS NOT INITIAL.

              SELECT SINGLE eqktx
                FROM eqkt
                INTO ls_orc_order-eqktx
                WHERE equnr = ls_orc_order-equnr
                  AND spras = sy-langu.

            ENDIF.

*            ENDIF.
            IF ls_orc_order-id IS NOT INITIAL.

              ls_orc_order-istat = 'L'.

            ELSE.

              IF ls_orc_order-short_text IS NOT INITIAL.
                ls_orc_order-istat = 'R'.
              ELSE.
                ls_orc_order-istat = 'P'.
              ENDIF.

            ENDIF.

            ls_orc_order-erdat        = <ls_ordens>-erdat.

            APPEND ls_orc_order TO e_ordens_orc.
            CLEAR ls_orc_order.

          ENDLOOP.

        ELSE.

*          ls_orc_order-id           = <ls_ordens>-aufnr.
          ls_orc_order-aufnr        = <ls_ordens>-aufnr.
          ls_orc_order-iwerk        = <ls_ordens>-werks.
          ls_orc_order-arbpl        = <ls_ordens>-arbpl.
          ls_orc_order-ktext        = <ls_ordens>-ktext.

          READ TABLE lt_0006 ASSIGNING <fs_0006>
          WITH KEY aufnr = <ls_ordens>-aufnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_orc_order-short_text = <fs_0006>-obs_reprov.
          ENDIF.

          ls_orc_order-user4        = <ls_ordens>-user4.
          ls_orc_order-cost_plan    = <ls_ordens>-wert2.
          ls_orc_order-tplnr        = <ls_ordens>-tplnr.

*          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*            EXPORTING
*              number    = <ls_ordens>-aufnr
*            IMPORTING
*              es_header = esheader
*            TABLES
*              return    = it_return
*              et_olist  = it_olist
*            EXCEPTIONS
*              OTHERS    = 01.
*          IF sy-subrc IS INITIAL.

          READ TABLE lt_afih ASSIGNING <fs_afih>
          WITH KEY aufnr = <ls_ordens>-aufnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_orc_order-equnr = <fs_afih>-equnr.
          ENDIF.

          IF ls_orc_order-equnr IS NOT INITIAL.

            SELECT SINGLE eqktx
              FROM eqkt
              INTO ls_orc_order-eqktx
              WHERE equnr = ls_orc_order-equnr
                AND spras = sy-langu.

          ENDIF.

*          ENDIF.

          IF ls_orc_order-id IS NOT INITIAL.

            ls_orc_order-istat = 'L'.

          ELSE.

            IF ls_orc_order-short_text IS NOT INITIAL.
              ls_orc_order-istat = 'R'.
            ELSE.
              ls_orc_order-istat = 'P'.
            ENDIF.

          ENDIF.

          ls_orc_order-erdat        = <ls_ordens>-erdat.

          APPEND ls_orc_order TO e_ordens_orc.
          CLEAR ls_orc_order.

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.

  TYPES : BEGIN OF ty_suplementos.
            INCLUDE TYPE zpmr0006.
            TYPES :nivel      TYPE numc10,
            aprovador  TYPE usnam,
            permit     TYPE pmsog,
            usua_subst TYPE usnam,
            data_lim   TYPE sy-datum,
            check      TYPE char1,
          END OF ty_suplementos,

          BEGIN OF ty_saida_0120.
            INCLUDE TYPE zpmr0006.
            TYPES: rowcolor(4) TYPE c,
            sttxt       TYPE caufvd-sttxt,
            asttx       TYPE caufvd-asttx,
            icon1       TYPE c LENGTH 4,
            icon        TYPE c LENGTH 4,
            cellcolor   TYPE lvc_t_scol,
            style       TYPE lvc_t_styl.
  TYPES END OF ty_saida_0120.

  DATA: wa_zpmr0002 TYPE zpmr0002.

  DATA: suplementos   TYPE TABLE OF ty_suplementos,
        it_zpmr0006   TYPE TABLE OF zpmr0006 WITH DEFAULT KEY,
        estrategias   TYPE TABLE OF zpmr0002,
        vl_nivel      TYPE numc10,
        user_data     TYPE alm_me_user_data,
        order_header  TYPE alm_me_order_header,
        user_profile  TYPE alm_me_c010prf,
        t_suplementos TYPE TABLE OF zpmr0006,
        gw_saida_0120 TYPE ty_saida_0120.

*  DATA: gt_zpmr0002 TYPE TABLE OF zpmr0002 WITH HEADER LINE.
*  DATA: zt_zpmr0002 TYPE TABLE OF zpmr0002 WITH HEADER LINE.


  SELECT DISTINCT a~mandt a~aufnr a~belnr a~status a~solicitante a~dt_solicitacao a~vlr_estimado a~werks
        a~observacao a~responsavel a~dt_modificacao a~equipment a~equipment_desc a~short_text a~object
        a~currency a~nivel_aprovado a~obs_reprov c~nivel c~aprovador c~permit c~usua_subst c~data_lim  "Anderson Oenning
  FROM zpmr0006 AS a
  INNER JOIN     aufk AS b ON b~aufnr       EQ a~aufnr
  INNER JOIN zpmr0002 AS c ON c~centro_desp EQ a~werks
  INTO CORRESPONDING FIELDS OF TABLE suplementos
  WHERE b~phas1 EQ abap_true
   AND  a~status NE 'R'
   AND  a~werks IN r_werks.

  SELECT DISTINCT a~mandt a~aufnr a~belnr a~status a~solicitante a~dt_solicitacao a~vlr_estimado a~werks
        a~observacao a~responsavel a~dt_modificacao a~equipment a~equipment_desc a~short_text a~object
        a~currency a~nivel_aprovado a~obs_reprov c~nivel c~aprovador c~permit c~usua_subst c~data_lim  "Anderson Oenning
  FROM zpmr0006 AS a
  INNER JOIN     aufk AS b ON b~aufnr       EQ a~aufnr
  INNER JOIN zpmr0002 AS c ON c~centro_desp EQ a~werks
  APPENDING CORRESPONDING FIELDS OF TABLE suplementos
  WHERE b~phas1 EQ abap_true
   AND  a~status = 'R'
   AND  a~werks IN r_werks.

  SORT suplementos BY object nivel.

  LOOP AT suplementos ASSIGNING FIELD-SYMBOL(<supl>) WHERE check IS INITIAL.
    vl_nivel = <supl>-nivel_aprovado.

*    IF VL_NIVEL IS INITIAL.
*      <SUPL>-CHECK = ABAP_TRUE.
*
*      MODIFY SUPLEMENTOS
*        FROM <SUPL>
*          TRANSPORTING CHECK
*            WHERE OBJECT EQ <SUPL>-OBJECT.
*
*
*      <SUPL>-CHECK = ABAP_FALSE.
*
*      MODIFY SUPLEMENTOS
*        FROM <SUPL>
*          TRANSPORTING CHECK
*              WHERE OBJECT EQ <SUPL>-OBJECT
*                 AND NIVEL EQ <SUPL>-NIVEL
*                AND PERMIT EQ <SUPL>-PERMIT.

*      ADD 1 TO VL_NIVEL.
*      IF VL_NIVEL NE <SUPL>-NIVEL.
*        <SUPL>-CHECK = ABAP_TRUE.
*      ENDIF.

*    ELSE.
    ADD 1 TO vl_nivel.
    IF vl_nivel NE <supl>-nivel.
      <supl>-check = abap_true.
    ENDIF.
*    ENDIF.
  ENDLOOP.

  LOOP AT suplementos ASSIGNING <supl>.
    IF <supl>-aufnr IS NOT INITIAL
      AND <supl>-aprovador IS NOT INITIAL
      AND <supl>-usua_subst IS NOT INITIAL
      AND <supl>-data_lim >= sy-datum.
      <supl>-aprovador = <supl>-usua_subst.
    ENDIF.
  ENDLOOP.

  DELETE suplementos WHERE check IS NOT INITIAL.
  DELETE suplementos WHERE responsavel NE i_user AND belnr NE 0000000000.
  DELETE suplementos WHERE aprovador   NE i_user AND belnr EQ 0000000000.

  it_zpmr0006 = VALUE #( FOR ls9 IN suplementos ( CORRESPONDING #( ls9 ) ) ).

  APPEND LINES OF it_zpmr0006 TO t_suplementos.

  LOOP AT t_suplementos INTO DATA(_zpmr0006).

    APPEND INITIAL LINE TO e_ordens_orc ASSIGNING FIELD-SYMBOL(<fs_ordem_supl>).

    MOVE-CORRESPONDING _zpmr0006 TO gw_saida_0120.

    gw_saida_0120-icon1 = '@DH@'.

    IF gw_saida_0120-obs_reprov IS NOT INITIAL.
      gw_saida_0120-icon = '@DH@'.
    ELSE.
      gw_saida_0120-icon = abap_false.
    ENDIF.

    SELECT SINGLE *
       FROM aufk
       INTO w_aufk
       WHERE aufnr EQ _zpmr0006-aufnr
         AND werks EQ _zpmr0006-werks.

*-> reread order status text in print language
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        flg_user_stat    = abap_true
        objnr            = w_aufk-objnr
        only_active      = abap_true
        spras            = sy-langu
      IMPORTING
        line             = gw_saida_0120-sttxt
        user_line        = gw_saida_0120-asttx
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF gw_saida_0120-asttx IS NOT INITIAL.
      SELECT SINGLE * FROM  tj30t INTO @DATA(w_tj30t)
             WHERE  stsma       = 'ZPM00010'
             AND    txt04       = @gw_saida_0120-asttx
             AND    spras       = @sy-langu.

      IF w_tj30t IS NOT INITIAL.
        gw_saida_0120-asttx = |{ gw_saida_0120-asttx } - { w_tj30t-txt30 }| .
      ENDIF.
    ENDIF.

*      "Inicio USER STORY 75882 - Anderson Oenning - 20/05/2022
    "Selecionar dados local / Centro de trabalho.

    CLEAR: esheader.
    FREE: it_return, it_olist.
    CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
      EXPORTING
        number    = _zpmr0006-aufnr
      IMPORTING
        es_header = esheader
      TABLES
        return    = it_return
        et_olist  = it_olist
      EXCEPTIONS
        OTHERS    = 01.

    IF sy-subrc EQ 0.
      gw_saida_0120-tplnr = esheader-funct_loc.
      gw_saida_0120-arbpl = esheader-mn_wk_ctr.
    ENDIF.


*      "Fim USER STORY 75882

*    APPEND gw_saida_0120 TO gt_saida_0120.

    <fs_ordem_supl>-id            = _zpmr0006-belnr.
    <fs_ordem_supl>-aufnr         = gw_saida_0120-aufnr.
    <fs_ordem_supl>-iwerk         = gw_saida_0120-werks.
    <fs_ordem_supl>-arbpl         = gw_saida_0120-arbpl.
    <fs_ordem_supl>-short_text    = gw_saida_0120-observacao.
    <fs_ordem_supl>-tplnr         = gw_saida_0120-tplnr.
    <fs_ordem_supl>-equnr         = esheader-equipment.
    <fs_ordem_supl>-observacao    = gw_saida_0120-obs_reprov.
    IF <fs_ordem_supl>-equnr IS NOT INITIAL.

      SELECT SINGLE eqktx
        FROM eqkt
        INTO <fs_ordem_supl>-eqktx
        WHERE equnr = <fs_ordem_supl>-equnr
          AND spras = sy-langu.

    ENDIF.
    <fs_ordem_supl>-istat         = _zpmr0006-status.
    <fs_ordem_supl>-erdat         = gw_saida_0120-dt_solicitacao.
    <fs_ordem_supl>-vlr_estimado  = gw_saida_0120-vlr_estimado.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM e_ordens_orc COMPARING ALL FIELDS.

ENDFUNCTION.
