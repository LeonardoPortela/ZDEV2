*&---------------------------------------------------------------------*
*&  Include           ZGL013_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_verifica_erros.
  DATA: wl_linha(6),
        tl_tbsl           TYPE TABLE OF tbsl      WITH HEADER LINE,
* ---> S4 Migration - 17/07/2023 - CA
*        TL_CSKB           TYPE TABLE OF CSKB      WITH HEADER LINE,
* <--- S4 Migration - 17/07/2023 - CA
        wl_tka02          TYPE tka02,
        wl_tabwt          TYPE tabwt,
        wl_t856t          TYPE t856t,
        tl_zfit0030       TYPE TABLE OF zfit0030  WITH HEADER LINE,
        tl_setleaf        TYPE TABLE OF setleaf   WITH HEADER LINE,
        tl_zglt032_aux    TYPE TABLE OF ty_glt032,
        wl_zglt032_aux    LIKE LINE OF tg_zglt032,
        wl_t001           TYPE t001,
        wl_t005           TYPE t005,
        wl_t007a          TYPE t007a,
        wl_t030k          TYPE t030k,
        wl_tcurc          TYPE tcurc,
        wl_t003t          TYPE t003t,
        wl_ska1           TYPE ska1,
        wl_skb1           TYPE skb1,
        wl_lfa1           TYPE lfa1,
        wl_kna1           TYPE kna1,
        wl_zimp_cad_depto TYPE  zimp_cad_depto,
        v_qtde            TYPE i VALUE 0,
        vkokrs            TYPE tka02-kokrs,
        msge(50)          TYPE c,
        flagm(1)          TYPE c,
        vakont            TYPE lfb1-akont,
        t_hkont           TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

* ---> S4 Migration - 17/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA

  CLEAR:    tg_msg_ret.
  REFRESH:  tg_msg_ret.


  IF wg_zglt031-blart IS INITIAL.
    MOVE:  c_ts_100-tab1          TO tg_msg_ret-aba.
    MOVE: 'WG_ZGLT031-BLART'  TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 '' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM t003t
      INTO wl_t003t
      WHERE blart EQ wg_zglt031-blart
      AND   spras EQ sy-langu.
    IF sy-subrc NE 0.
      MOVE: 'WG_ZGLT031-BLART'  TO tg_msg_ret-field,
*            'Tipo documento não cadastrado'  TO TG_MSG_RET-MSG,
            TEXT-e02 TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_zglt031-dpto_resp IS INITIAL.
    MOVE:  c_ts_100-tab1          TO tg_msg_ret-aba.
    MOVE: 'WG_ZGLT031-DPTO_RESP'  TO tg_msg_ret-field.
    CONCATENATE TEXT-e03 '' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM zimp_cad_depto
      INTO wl_zimp_cad_depto
      WHERE dep_resp EQ wg_zglt031-dpto_resp.
    IF sy-subrc NE 0.
      MOVE: 'WG_ZGLT031-DPTO_RESP'  TO tg_msg_ret-field,
            TEXT-e04  TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_zglt031-descricao IS INITIAL.
    MOVE:  c_ts_100-tab1          TO tg_msg_ret-aba.
    MOVE: 'WG_ZGLT031-DESCRICAO'  TO tg_msg_ret-field.
    CONCATENATE TEXT-e05 '' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_zglt031-bukrs IS INITIAL.
*    MOVE:  c_ts_100-tab1          TO tg_msg_ret-aba.
*    MOVE: 'WG_ZGLT031-BUKRS'  TO tg_msg_ret-field.
*    CONCATENATE 'Empresa -' 'Obrigatório' INTO tg_msg_ret-msg SEPARATED BY space.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM t001
      INTO wl_t001
      WHERE bukrs EQ wg_zglt031-bukrs.
    IF sy-subrc NE 0.
      MOVE: 'WG_ZGLT031-BUKRS'  TO tg_msg_ret-field,
           TEXT-e06 TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.


  IF wg_zglt031-moeda_interna IS INITIAL .
*    MOVE: c_ts_100-tab1 TO tg_msg_ret-aba,
*          'Informe Moeda interna'       TO tg_msg_ret-msg,
*          'WG_ZGLT031-MOEDA_INTERNA'    TO tg_msg_ret-field.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM tcurc
       INTO wl_tcurc
        WHERE  waers EQ wg_zglt031-moeda_interna.
    IF sy-subrc NE 0.
      MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba,
             TEXT-e07 TO tg_msg_ret-msg,
             'WG_ZGLT031-MOEDA_INTERNA'    TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_zglt031-moeda_forte IS INITIAL .
*    MOVE: c_ts_100-tab1 TO tg_msg_ret-aba,
*          'Informe Moeda forte'       TO tg_msg_ret-msg,
*          'WG_ZGLT031-MOEDA_FORTE'    TO tg_msg_ret-field.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM tcurc
       INTO wl_tcurc
        WHERE  waers EQ wg_zglt031-moeda_forte.
    IF sy-subrc NE 0.
      MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba,
             TEXT-e08 TO tg_msg_ret-msg,
             'WG_ZGLT031-MOEDA_FORTE'    TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_zglt031-moeda_grupo IS INITIAL .
*    MOVE: c_ts_100-tab1 TO tg_msg_ret-aba,
*          'Informe Moeda grupo'       TO tg_msg_ret-msg,
*          'WG_ZGLT031-MOEDA_GRUPO'    TO tg_msg_ret-field.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM tcurc
       INTO wl_tcurc
        WHERE  waers EQ wg_zglt031-moeda_grupo.
    IF sy-subrc NE 0.
      MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba,
             TEXT-e09 TO tg_msg_ret-msg,
             'WG_ZGLT031-MOEDA_GRUPO'    TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_zglt031-dt_doc IS INITIAL AND wg_zglt031-dt_doc_ult_mes IS INITIAL.
    MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba,
           TEXT-e10  TO tg_msg_ret-msg,
            'WG_ZGLT031-DT_DOC'          TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_zglt031-dt_lcto IS INITIAL AND  wg_zglt031-dt_lcto_ult_mes IS INITIAL.
    MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba,
            TEXT-e11 TO tg_msg_ret-msg,
            'WG_ZGLT031-DT_LCTO'          TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF tg_zglt032[] IS NOT INITIAL.

    SELECT * FROM tbsl INTO TABLE tl_tbsl.

    SELECT SINGLE *
            FROM tka02
            INTO wl_tka02
            WHERE bukrs  = wg_zglt031-bukrs.
    MOVE wl_tka02-kokrs TO vkokrs.

* ---> S4 Migration - 17/07/2023 - CA
*    SELECT  *
*      FROM CSKB
*      INTO TABLE TL_CSKB
*      WHERE  KOKRS  = WL_TKA02-KOKRS
*      AND    DATAB  LE SY-DATUM
*      AND    DATBI  GE SY-DATUM.
* <--- S4 Migration - 17/07/2023 - CA

    "SELECT * FROM CSKB INTO TABLE TL_CSKB.


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_CTAS_TPMV_BPC'
      TABLES
        set_values    = t_hkont
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.

    ENDIF.

    SELECT * FROM setleaf INTO TABLE tl_setleaf WHERE setname EQ 'CONTAS_EC-CS'.

    SORT: tl_tbsl     BY bschl,
* ---> S4 Migration - 17/07/2023 - CA
*          tl_cskb     BY kstar,
* <--- S4 Migration - 17/07/2023 - CA
          tl_zfit0030 BY hkont,
          tl_setleaf  BY valfrom.

  ELSE.
    MOVE: TEXT-e12 TO tg_msg_ret-msg,
          c_ts_100-tab2 TO tg_msg_ret-aba.  """"""""""
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  tl_zglt032_aux[] = tg_zglt032[].
  SORT: tl_zglt032_aux BY bschl.

  CLEAR: wg_zglt032, wl_zglt032_aux.

  LOOP AT tg_zglt032 INTO wg_zglt032.
    wl_linha = sy-tabix.
    v_qtde = 0.
    LOOP AT tl_zglt032_aux INTO wl_zglt032_aux WHERE bschl EQ wg_zglt032-bschl
                                                 AND hkont EQ wg_zglt032-hkont.
      ADD 1 TO v_qtde.
      CLEAR: wl_zglt032_aux.
    ENDLOOP.

*    IF V_QTDE > 1.
*      MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
*      CONCATENATE 'Chave de lançamento e Conta duplicado' ' LINHA: ' WL_LINHA INTO TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
  ENDLOOP.

  CLEAR: wg_zglt032, wl_linha.

  LOOP AT tg_zglt032 INTO wg_zglt032.
    wl_linha = sy-tabix.
    IF wg_zglt032-bschl IS INITIAL
      AND ( wg_zglt032-hkont IS INITIAL OR wg_zglt032-umskz IS INITIAL OR wg_zglt032-anbwa IS INITIAL OR
            wg_zglt032-bewar IS INITIAL OR wg_zglt032-vbund IS INITIAL OR wg_zglt032-kostl IS INITIAL OR
            wg_zglt032-prctr IS INITIAL OR wg_zglt032-aufnr IS INITIAL OR wg_zglt032-matnr IS INITIAL OR
            wg_zglt032-zuonr IS INITIAL OR wg_zglt032-sgtxt IS INITIAL ).

      MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e13 '' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    " 28.02.2018 ALRS
*    IF WL_T001-LAND1 EQ 'BR' OR  WL_T001-LAND1 EQ ''.
*      IF WG_ZGLT031-ST_CONC_BANC = 'X' AND NOT '40_50' CS WG_ZGLT032-BSCHL.
*        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
*        CONCATENATE TEXT-E37 '' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
*    ENDIF.

    IF wg_zglt031-bukrs IS NOT INITIAL.
      IF wg_zglt032-tax_code IS NOT INITIAL.
        SELECT SINGLE *
          FROM t001
          INTO wl_t001
          WHERE bukrs EQ wg_zglt031-bukrs.
        SELECT SINGLE *
          FROM t005
          INTO wl_t005
          WHERE land1 = wl_t001-land1.

        SELECT SINGLE *
          FROM t007a
          INTO wl_t007a
          WHERE kalsm = wl_t005-kalsm
          AND   mwskz = wg_zglt032-tax_code.
        IF sy-subrc NE 0.
          MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba.
          CONCATENATE wg_zglt032-tax_code TEXT-e38 wl_t005-kalsm wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSE.
          SELECT SINGLE *
             FROM t030k
               INTO  wl_t030k
             WHERE ktopl = '0050'
             AND   mwskz = wg_zglt032-tax_code.
          IF sy-subrc NE 0.
            MOVE:  c_ts_100-tab1 TO tg_msg_ret-aba.
            CONCATENATE wg_zglt032-tax_code TEXT-e38 wl_t005-kalsm wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: tl_tbsl, vakont.
    READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl BINARY SEARCH.
    IF wg_zglt032-hkont IS INITIAL AND tl_tbsl-koart EQ 'S'.
      IF ( wl_t001-land1 EQ 'BR' OR  wl_t001-land1 EQ '' ).
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e14 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF wg_zglt031-st_conc_banc = ''.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e14 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSE.
      SELECT SINGLE *                  "#EC CI_DB_OPERATION_OK[2389136]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2431747]
        INTO wl_ska1
        WHERE ktopl = '0050'
        AND   saknr = wg_zglt032-hkont.
      READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl BINARY SEARCH.

      IF wl_ska1-ktoks = 'YB03' AND tl_tbsl-koart NE 'D' AND tl_tbsl-koart NE 'K'.
        IF wg_zglt031-bukrs IS INITIAL.
          MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e15'' wg_zglt032-hkont TEXT-e16 '' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSE.
          SELECT SINGLE *              "#EC CI_DB_OPERATION_OK[2431747]
              FROM skb1
              INTO wl_skb1
              WHERE bukrs = wg_zglt031-bukrs
              AND   saknr = wg_zglt032-hkont.
          IF sy-subrc = 0.
            IF wl_skb1-mitkz IS NOT INITIAL.
              MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e15'' wg_zglt032-hkont TEXT-e16 '' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF wg_zglt031-bukrs IS NOT INITIAL AND tl_tbsl-koart EQ 'S'.
        SELECT SINGLE *                "#EC CI_DB_OPERATION_OK[2431747]
         FROM skb1
         INTO wl_skb1
         WHERE bukrs = wg_zglt031-bukrs
         AND   saknr = wg_zglt032-hkont.

        IF sy-subrc NE 0.
          MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e36 '' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSEIF wl_skb1-xspeb = 'X'.
          MOVE:  c_ts_100-tab1               TO tg_msg_ret-aba.
          CONCATENATE TEXT-e27 ''  wg_zglt032-hkont TEXT-e28 '' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.


    ENDIF.



    IF wg_zglt032-anbwa IS INITIAL.
      CLEAR: tl_tbsl.
      READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl
                                               koart = 'A'
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e17 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSE.
      SELECT SINGLE *
        FROM tabwt
        INTO wl_tabwt
        WHERE bwasl = wg_zglt032-anbwa.
      IF sy-subrc NE 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e18 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_zglt032-umskz IS INITIAL.
      CLEAR: tl_tbsl.
      READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl
                                               xsonu = 'X'
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e19 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_zglt032-umskz IS NOT INITIAL.
      CLEAR: tl_tbsl.
      READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl
                                               xsonu = 'X'
                                      BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e20 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    CLEAR: tl_tbsl, vakont.
    READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = wg_zglt032-bschl BINARY SEARCH.

    IF sy-subrc EQ 0.
      CASE tl_tbsl-koart.
        WHEN 'K'.
          IF wg_zglt032-hkont IS NOT INITIAL.
            SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wg_zglt032-hkont.
            IF sy-subrc NE 0.
              MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e21'' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
            SELECT SINGLE akont INTO vakont FROM lfb1 WHERE lifnr = wg_zglt032-hkont AND bukrs = wg_zglt031-bukrs.
            CLEAR wl_lfa1.
            SELECT SINGLE *
              FROM lfa1
              INTO wl_lfa1
              WHERE lifnr = wg_zglt032-hkont.
            IF wl_lfa1-sperr = 'X'.
              MOVE:  c_ts_100-tab1               TO tg_msg_ret-aba.
              CONCATENATE TEXT-e22 ''  wg_zglt032-hkont TEXT-e23 '' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.

        WHEN 'D'.
          IF wg_zglt032-hkont IS NOT INITIAL.
            SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wg_zglt032-hkont.
            IF sy-subrc NE 0.
              MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e24 '' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
            SELECT SINGLE akont INTO vakont FROM knb1 WHERE kunnr = wg_zglt032-hkont AND bukrs = wg_zglt031-bukrs.
            CLEAR wl_kna1.
            SELECT SINGLE *
              FROM kna1
              INTO wl_kna1
              WHERE kunnr = wg_zglt032-hkont.
            IF wl_kna1-sperr = 'X'.
              MOVE:  c_ts_100-tab1               TO tg_msg_ret-aba.
              CONCATENATE TEXT-e25 ''  wg_zglt032-hkont TEXT-e26 '' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          CLEAR  wl_ska1.
          vakont = wg_zglt032-hkont.
          SELECT SINGLE *              "#EC CI_DB_OPERATION_OK[2431747]
            FROM ska1                  "#EC CI_DB_OPERATION_OK[2389136]
            INTO wl_ska1
            WHERE ktopl = '0050'
            AND   saknr = wg_zglt032-hkont.
          IF  wl_ska1-xspeb = 'X'.
            MOVE:  c_ts_100-tab1               TO tg_msg_ret-aba.
            CONCATENATE TEXT-e27 ''  wg_zglt032-hkont TEXT-e28 '' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
          ENDIF.


      ENDCASE.
    ENDIF.
    READ TABLE t_hkont WITH KEY from = vakont.
    IF sy-subrc EQ 0.
      IF wg_zglt032-bewar IS INITIAL.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e29 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        SELECT *
         INTO TABLE tl_zfit0030
         FROM zfit0030
         WHERE hkont  = vakont .
        IF tl_zfit0030[] IS NOT INITIAL.
          CLEAR: msge, flagm.
          flagm = 'N'.
          LOOP AT tl_zfit0030.
            IF tl_zfit0030-bewar EQ wg_zglt032-bewar.
              flagm = 'S'.
            ENDIF.
          ENDLOOP.
          IF flagm = 'N'.
            LOOP AT tl_zfit0030.
              CONCATENATE msge tl_zfit0030-bewar INTO msge SEPARATED BY space.
            ENDLOOP.
            CONCATENATE TEXT-e30 '' msge INTO msge SEPARATED BY space.
            MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
            CONCATENATE msge wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wg_zglt032-vbund IS INITIAL.
      CLEAR: tl_setleaf.
      READ TABLE tl_setleaf INTO tl_setleaf WITH KEY valfrom = wg_zglt032-hkont
                                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e31 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      READ TABLE tl_setleaf INTO tl_setleaf WITH KEY valfrom = wg_zglt032-akont
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e32 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF    wg_zglt032-kostl IS INITIAL
     AND  wg_zglt032-prctr IS INITIAL
     AND  wg_zglt032-aufnr IS INITIAL
     AND  '40_50' CS wg_zglt032-bschl.
* ---> S4 Migration - 17/07/2023 - CA
*      CLEAR: tl_cskb.
*
*      READ TABLE tl_cskb INTO tl_cskb WITH KEY kstar = wg_zglt032-hkont
*                                      BINARY SEARCH.
      lv_controllingarea  = wl_tka02-kokrs.
      lv_costelement      = wg_zglt032-hkont.
      lv_keydate          = sy-datum.

      CLEAR: lt_returns[], ls_coeldes.

      CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
        EXPORTING
          controllingarea   = lv_controllingarea
          costelement       = lv_costelement
          keydate           = lv_keydate
        IMPORTING
          costelementdetail = ls_coeldes
        TABLES
          return            = lt_returns.

      READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc <> 0.
*      IF sy-subrc EQ 0.
* <--- S4 Migration - 17/07/2023 - CA
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e33 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      CLEAR wl_skb1.
      IF '0200_0201' CS wg_zglt031-bukrs. "EUROPA
        SELECT SINGLE *                "#EC CI_DB_OPERATION_OK[2431747]
          FROM skb1
          INTO wl_skb1
          WHERE bukrs = wg_zglt031-bukrs
          AND   saknr = wg_zglt032-hkont
          AND fstag   = 'YB09'.
      ENDIF.

      IF wl_skb1-fstag   = 'YB09'  AND tl_tbsl-koart  = 'S'.
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e33 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF    wg_zglt032-kostl IS NOT INITIAL.
* ---> S4 Migration - 17/07/2023 - CA
*      CLEAR: tl_cskb.
*      READ TABLE tl_cskb INTO tl_cskb WITH KEY kstar = wg_zglt032-hkont
*                                      BINARY SEARCH.
      lv_controllingarea  = wl_tka02-kokrs.
      lv_costelement      = wg_zglt032-hkont.
      lv_keydate          = sy-datum.

      CLEAR: lt_returns[], ls_coeldes.

      CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
        EXPORTING
          controllingarea   = lv_controllingarea
          costelement       = lv_costelement
          keydate           = lv_keydate
        IMPORTING
          costelementdetail = ls_coeldes
        TABLES
          return            = lt_returns.

      READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc = 0.
*      IF sy-subrc NE 0.
* <--- S4 Migration - 17/07/2023 - CA
        MOVE:  c_ts_100-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e34 '' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
* ---> S4 Migration - 17/07/2023 - CA
    CLEAR: wg_zglt032, tl_tbsl, tl_zfit0030, tl_setleaf.
*    CLEAR: wg_zglt032, tl_tbsl, tl_zfit0030, tl_setleaf, tl_cskb.
* <--- S4 Migration - 17/07/2023 - CA
  ENDLOOP.
*  MOVE:
ENDFORM.                    " F_VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_montar_layout  USING p_edit.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
*        0  'ZGLT032'   'TP_LCTO'      'TG_ZGLT032'  'TP_LCTO'   'Tipo Lançamento'     '10' ' '   ' ' 'X',
        1   'TBSLT'     'BSCHL'       'TG_ZGLT032'  'BSCHL'     TEXT-a01              '08' p_edit ' ' 'X'.
  IF x_visao = 'X'.
    PERFORM f_montar_estrutura USING:
      2   ' '         ' '           'TG_ZGLT032'  'AKONT'       TEXT-a02              '12' p_edit ' ' 'X',
      3   ' '         ' '           'TG_ZGLT032'  'DESCR_A'     TEXT-a03              '35' ' '    ' ' ' '.
  ELSE.
    PERFORM f_montar_estrutura USING:
      2   ' '         ' '           'TG_ZGLT032'  'HKONT'       TEXT-a02              '12' p_edit ' ' ' ',
      3   ' '         ' '           'TG_ZGLT032'  'DESCR'       TEXT-a03              '35' ' '    ' ' 'X'.
  ENDIF.
  PERFORM f_montar_estrutura USING:
        4   ' '         ' '           'TG_ZGLT032'  'UMSKZ'     TEXT-a04              '10' p_edit ' ' ' ',
        5   ''          ''            'TG_ZGLT032'  'TAX_CODE'  TEXT-a12              '12' p_edit ' ' ' ',
        6   'TABWT'     'BWASL'       'TG_ZGLT032'  'ANBWA'     TEXT-a05              '10' p_edit ' ' ' ',
        7   'T856T'     'TRTYP'       'TG_ZGLT032'  'BEWAR'     TEXT-a06              '10' p_edit ' ' ' ',
        9   'TSTGC'     'RCOMP'       'TG_ZGLT032'  'VBUND'     TEXT-a07              '10' p_edit ' ' ' ',
        9   'CSKS'      'KOSTL'       'TG_ZGLT032'  'KOSTL'     TEXT-a08              '12' p_edit ' ' ' ',
        10  'CEPC'      'PRCTR'       'TG_ZGLT032'  'PRCTR'     TEXT-a09              '12' p_edit ' ' ' ',
        11  'AFIH'      'AUFNR'       'TG_ZGLT032'  'AUFNR'     TEXT-a10              '14' p_edit ' ' ' ',
        12  'ZGLT032'   'MATNR'       'TG_ZGLT032'  'MATNR'     TEXT-a15              '15' p_edit ' ' ' ',
        13  'ZGLT032'   'MATNR_FI'    'TG_ZGLT032'  'MATNR_FI'  TEXT-a11              '15' p_edit ' ' ' ',
        14  'ZGLT032'   'ZUONR'       'TG_ZGLT032'  'ZUONR'     TEXT-a13              '15' p_edit ' ' ' ',
        15  'ZGLT032'   'SGTXT'       'TG_ZGLT032'  'SGTXT'     TEXT-a14              '30' p_edit ' ' ' '.
ENDFORM.                    " F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_montar_estrutura  USING  p_col_pos   p_ref_tabname p_ref_fieldname p_tabname p_field
                                p_scrtext_l p_outputlen   p_edit          p_sum     p_emphasize.

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.

  w_fieldcatalog-key           = ' '.
  CASE p_field.
    WHEN 'BSCHL' OR 'HKONT' OR 'DESCR' .
      w_fieldcatalog-key           = 'X'.
    WHEN OTHERS.
  ENDCASE.

  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos       = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen   = p_outputlen.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  CASE p_field.
    WHEN 'HKONT' OR 'UMSKZ' OR 'TAX_CODE' .
      w_fieldcatalog-f4availabl = c_x.
    WHEN OTHERS.
  ENDCASE.

  APPEND w_fieldcatalog TO t_fieldcatalog.
ENDFORM.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_limpa_campos.
  CLEAR: wg_zglt031, tg_editor, x_field.

  REFRESH: tg_zglt032, tg_editor.

  wg_zglt031-blart = 'LM'.
  CLEAR x_visao.
*  BTN_VISAO = '@KU@ Visão de Razão'.
  btn_visao = TEXT-b01.
  "
  SELECT SINGLE *
    FROM usr05
    INTO @DATA(_usr05)
    WHERE bname = @sy-uname
    AND parid   = 'BUK'.
  IF sy-subrc = 0.
    wg_zglt031-bukrs = _usr05-parva+0(4).
  ENDIF.
ENDFORM.                    " F_LIMPA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_obtem_proximo.
**  SELECT * FROM zglt031 INTO TABLE it_zglt031 ORDER BY tp_lcto DESCENDING.
**
**  READ TABLE it_zglt031 INTO wa_zglt031 INDEX 1.
**  IF sy-subrc NE 0.
**    wg_zglt031-tp_lcto = 1.
**  ELSE.
**    wg_zglt031-tp_lcto = wa_zglt031-tp_lcto.
**    ADD 1 TO wg_zglt031-tp_lcto.
**  ENDIF.

  DATA: vl_number TYPE i.
  IF wg_zglt031-tp_lcto IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZGL_LCMAN'
      IMPORTING
        number                  = vl_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      wg_zglt031-tp_lcto = vl_number.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_OBTEM_PROXIMO

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_grava_dados.
  CHECK wg_zglt031-tp_lcto IS NOT INITIAL.

  DATA: wl_input_glt031 TYPE zglt031,
        tl_input_glt032 TYPE TABLE OF zglt032 WITH HEADER LINE,
        vbuzei          TYPE zglt032-buzei,
        wg_zglt032_chv  TYPE ty_glt032.

  MOVE: sy-mandt                    TO wl_input_glt031-mandt,
        wg_zglt031-tp_lcto          TO wl_input_glt031-tp_lcto,
        wg_zglt031-descricao        TO wl_input_glt031-descricao,
        wg_zglt031-dpto_resp        TO wl_input_glt031-dpto_resp,
        wg_zglt031-bukrs            TO wl_input_glt031-bukrs,
        wg_zglt031-moeda_doc        TO wl_input_glt031-moeda_doc,
        wg_zglt031-st_lc_moeda      TO wl_input_glt031-st_lc_moeda,
        wg_zglt031-moeda_interna    TO wl_input_glt031-moeda_interna,
        wg_zglt031-moeda_int_hist   TO wl_input_glt031-moeda_int_hist,
        wg_zglt031-moeda_forte      TO wl_input_glt031-moeda_forte,
        wg_zglt031-moeda_ft_hist    TO wl_input_glt031-moeda_ft_hist,
        wg_zglt031-moeda_grupo      TO wl_input_glt031-moeda_grupo,
        wg_zglt031-moeda_gp_hist    TO wl_input_glt031-moeda_gp_hist,
        wg_zglt031-blart            TO wl_input_glt031-blart,
        wg_zglt031-xblnr            TO wl_input_glt031-xblnr,
        wg_zglt031-bktxt            TO wl_input_glt031-bktxt,
        wg_zglt031-dt_doc           TO wl_input_glt031-dt_doc,
        wg_zglt031-dt_doc_ult_mes   TO wl_input_glt031-dt_doc_ult_mes,
        wg_zglt031-dt_lcto          TO wl_input_glt031-dt_lcto,
        wg_zglt031-dt_lcto_ult_mes  TO wl_input_glt031-dt_lcto_ult_mes,
        wg_zglt031-prov_est         TO wl_input_glt031-prov_est,
        wg_zglt031-st_ap_fiscal     TO wl_input_glt031-st_ap_fiscal,
        wg_zglt031-st_conc_banc     TO wl_input_glt031-st_conc_banc,
        wg_zglt031-st_trans_banc    TO wl_input_glt031-st_trans_banc,
        wg_zglt031-st_agrupa        TO wl_input_glt031-st_agrupa,
        wg_zglt031-st_aprova        TO wl_input_glt031-st_aprova,
*        wg_zglt031-ref_lcto         TO wl_input_glt031-ref_lcto,
        sy-uname                    TO wl_input_glt031-usnam,
        sy-datum                    TO wl_input_glt031-dt_entrada,
        sy-uzeit                    TO wl_input_glt031-hr_entrada.

  REFRESH: tg_editor.
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        wl_input_glt031-ref_lcto = wg_editor-line.
      ELSEIF sy-tabix GE 2.
        CONCATENATE wl_input_glt031-ref_lcto wg_editor-line INTO wl_input_glt031-ref_lcto. " SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "SORT TG_ZGLT032 BY TP_LCTO BSCHL HKONT BUZEI.
  vbuzei = 0.
  CLEAR wg_zglt032_chv.
  LOOP AT tg_zglt032 INTO wg_zglt032.
    ADD 1 TO vbuzei.
    MOVE: sy-mandt                TO tl_input_glt032-mandt,
          wl_input_glt031-tp_lcto TO tl_input_glt032-tp_lcto,
          wg_zglt032-bschl        TO tl_input_glt032-bschl,
          wg_zglt032-hkont        TO tl_input_glt032-hkont,
          vbuzei                  TO tl_input_glt032-buzei,
          wg_zglt032-umskz        TO tl_input_glt032-umskz,
          wg_zglt032-anbwa        TO tl_input_glt032-anbwa,
          wg_zglt032-bewar        TO tl_input_glt032-bewar,
          wg_zglt032-vbund        TO tl_input_glt032-vbund,
          wg_zglt032-kostl        TO tl_input_glt032-kostl,
          wg_zglt032-prctr        TO tl_input_glt032-prctr,
          wg_zglt032-aufnr        TO tl_input_glt032-aufnr,
          wg_zglt032-matnr        TO tl_input_glt032-matnr,
          wg_zglt032-matnr_fi     TO tl_input_glt032-matnr_fi,
          wg_zglt032-tax_code     TO tl_input_glt032-tax_code,
          wg_zglt032-zuonr        TO tl_input_glt032-zuonr,
          wg_zglt032-sgtxt        TO tl_input_glt032-sgtxt.

    APPEND tl_input_glt032.
    CLEAR: tl_input_glt032, wg_zglt032.
  ENDLOOP.

*  CLEAR: tl_input_glt032, wg_zglt031, wg_zglt032.
  DELETE FROM zglt032 WHERE tp_lcto = wg_zglt031-tp_lcto.
  MODIFY zglt031 FROM       wl_input_glt031.
*  MOVE-CORRESPONDING TL_INPUT_GLT032 to WA_ZGLT032_LOG.
  MODIFY zglt032 FROM TABLE tl_input_glt032.

  MESSAGE s836(sd) WITH TEXT-m01 wg_zglt031-tp_lcto TEXT-m02.

ENDFORM.                    " F_GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_busca_dados.
  DATA: wl_zglt031   TYPE zglt031,
        tl_zglt032   TYPE TABLE OF zglt032 WITH HEADER LINE,
        wl_tbsl      TYPE tbsl,
        wl_cont      TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix,
        wl_akont     TYPE lfb1-akont,
        wl_hkont     TYPE t074-hkont,
        wl_t003t     TYPE t003t.

  DATA: tl_parametros TYPE ustyp_t_parameters,
        wl_parametros TYPE ustyp_parameters.

  IF wg_zglt031-blart IS NOT INITIAL.
    SELECT SINGLE *
      FROM t003t
      INTO wl_t003t
      WHERE spras = sy-langu
      AND   blart = wg_zglt031-blart.

    MOVE wl_t003t-ltext TO  wg_zglt031-ltext.
  ENDIF.

  IF wg_acao = c_add. "Novo Lançamento
    wg_acao = c_modif.
  ELSEIF wg_acao = c_displa.
    CHECK wg_zglt031-tp_lcto IS NOT INITIAL.
    SELECT SINGLE * FROM zglt031
      INTO wl_zglt031
    WHERE tp_lcto EQ wg_zglt031-tp_lcto.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m03.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zglt031-loekz IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m04.
      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING wl_zglt031 TO wg_zglt031.

      IF wg_zglt031-blart IS NOT INITIAL.
        SELECT SINGLE *
          FROM t003t
          INTO wl_t003t
          WHERE spras = sy-langu
          AND   blart = wg_zglt031-blart.

        MOVE wl_t003t-ltext TO  wg_zglt031-ltext.
      ENDIF.

      REFRESH: tg_editor.
      CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
      wl_cont = strlen( wg_zglt031-ref_lcto ).
      wl_cont_aux = wl_cont / 72.

      DO.
        MOVE: wg_zglt031-ref_lcto+wl_cont_aux2 TO wg_editor-line.
        ADD 72 TO wl_cont_aux2.
        APPEND wg_editor TO tg_editor.

        IF wl_cont_aux2 GT wl_cont.
          EXIT.
        ENDIF.
      ENDDO.

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.

      SELECT * FROM zglt032
        INTO TABLE tl_zglt032
      WHERE tp_lcto EQ wg_zglt031-tp_lcto
      ORDER BY buzei.

      CLEAR: wg_zglt032.
      REFRESH: tg_zglt032.
      LOOP AT tl_zglt032.
        MOVE-CORRESPONDING tl_zglt032 TO wg_zglt032.
        MOVE tl_zglt032-hkont TO wg_zglt032-akont.

        IF tl_zglt032-bschl IS NOT INITIAL.
          SELECT SINGLE * FROM tbsl INTO wl_tbsl WHERE bschl EQ tl_zglt032-bschl.
          IF sy-subrc EQ 0.
            CASE wl_tbsl-koart.
              WHEN 'K'.
                SELECT SINGLE name1 FROM lfa1 INTO wg_zglt032-descr WHERE lifnr EQ wg_zglt032-hkont.
                SELECT SINGLE akont FROM lfb1 INTO wl_akont         WHERE lifnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
                wg_zglt032-akont = wl_akont.
                IF tl_zglt032-umskz NE ''.
                  SELECT SINGLE skont FROM t074 INTO wl_hkont
                    WHERE ktopl = '0050'
                    AND   koart = 'K'
                    AND   umskz = tl_zglt032-umskz
                    AND   hkont = wl_akont.
                  wg_zglt032-akont = wl_hkont.
                ENDIF.
                SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                                     AND spras EQ sy-langu
                                                                     AND ktopl EQ '0050'.
              WHEN 'D'.
                SELECT SINGLE name1 FROM kna1 INTO wg_zglt032-descr WHERE kunnr EQ wg_zglt032-hkont.
                SELECT SINGLE akont FROM knb1 INTO wl_akont         WHERE kunnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
                wg_zglt032-akont = wl_akont.
                IF tl_zglt032-umskz NE ''.
                  SELECT SINGLE skont FROM t074 INTO wl_hkont
                    WHERE ktopl = '0050'
                    AND   koart = 'D'
                    AND   umskz = tl_zglt032-umskz
                    AND   hkont = wl_akont.
                  wg_zglt032-akont = wl_hkont.
                ENDIF.
                SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                                     AND spras EQ sy-langu
                                                                     AND ktopl EQ '0050'.

              WHEN 'S'.
                SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr WHERE saknr EQ wg_zglt032-hkont
                                                                     AND spras EQ sy-langu
                                                                     AND ktopl EQ '0050'.
                wg_zglt032-descr_a = wg_zglt032-descr.
              WHEN 'I'.
                SELECT SINGLE mcoa1 FROM anla INTO wg_zglt032-descr WHERE anln1 EQ wg_zglt032-hkont.
                wg_zglt032-descr_a = wg_zglt032-descr.
            ENDCASE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING wg_zglt032 TO wa_zglt032_log.
        APPEND wg_zglt032 TO tg_zglt032.
        APPEND wa_zglt032_log TO it_zglt032_log.

*        INSERT ZGLT032_LOG FROM WG_ZGLT032.

        CLEAR: wg_zglt032, wl_tbsl.
      ENDLOOP.
    ENDIF.
    REFRESH: tg_fields.
    PERFORM f_trata_campos USING  space
                                  'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM f_trata_campos USING  space
                                  'GR1'
                                  c_0         "INPUT 1     NO INPUT 0
                                  c_0.        "INVISIBLE 1 VISIBLE 0

    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
    wg_acao = c_modif.
  ENDIF.

  "PERFORM F_VERIFICA_ERROS.
  REFRESH tg_msg_ret.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
      i_show        = ''
      i_repid       = sy-repid
      i_pressed_tab = 'TS_100_IMP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.


ENDFORM.                    " F_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_trata_campos  USING p_field p_group1 p_value p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.

ENDFORM.                    " F_TRATA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_HELP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_montar_help.

  lt_f4-fieldname = 'HKONT'.
  lt_f4-register  = 'X'.
  lt_f4-getbefore = 'X'.
  APPEND lt_f4.

  lt_f4-fieldname = 'TAX_CODE'.
  lt_f4-register = 'X'.
  lt_f4-getbefore = 'X'.
  lt_f4-chngeafter ='X'.
  APPEND lt_f4.


  lt_f4-fieldname = 'UMSKZ'.
  lt_f4-register  = 'X'.
  lt_f4-getbefore = 'X'.
  APPEND lt_f4.

ENDFORM.                    " F_MONTAR_HELP

*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_eliminar_lancamento.
  DATA: wl_zglt031 TYPE zglt031.

  SELECT SINGLE * FROM zglt031
    INTO wl_zglt031
  WHERE tp_lcto EQ wg_zglt031-tp_lcto.

  IF sy-subrc IS INITIAL.
    IF wl_zglt031-loekz IS INITIAL.
      MOVE: c_x TO wl_zglt031-loekz.
      MODIFY zglt031 FROM wl_zglt031.

      MESSAGE s836(sd) WITH TEXT-m05.

      CLEAR:    wg_zglt031, tg_editor.
      REFRESH:  tg_zglt032, tg_editor, tg_fields.

      PERFORM f_trata_campos USING  space
                                    'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                    'GR1'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E'
         WITH TEXT-m06.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_atualiza_alv2.
  DATA: wl_tbsl  TYPE tbsl,
        wl_lfa1  TYPE lfa1,
        wl_kna1  TYPE kna1,
        wl_skat  TYPE skat,
        wl_anla  TYPE anla,
        wl_anln1 TYPE anla-anln1,
        wl_akont TYPE lfb1-akont,
        wl_hkont TYPE t074-hkont,
        tabix    TYPE sy-tabix.


  LOOP AT tg_zglt032 INTO wg_zglt032.
    tabix = sy-tabix.
    CHECK wg_zglt032-bschl IS NOT INITIAL.
    CHECK wg_zglt032-hkont IS NOT INITIAL.

    SELECT SINGLE * FROM tbsl
      INTO wl_tbsl
    WHERE bschl EQ wg_zglt032-bschl.


    CLEAR wg_zglt032-descr.
    MOVE wg_zglt032-hkont TO wg_zglt032-akont.
    CASE wl_tbsl-koart.
      WHEN 'K'.
        SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wg_zglt032-hkont.
        MOVE: wl_lfa1-name1 TO wg_zglt032-descr.
        "
        SELECT SINGLE akont FROM lfb1 INTO wl_akont         WHERE lifnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
        wg_zglt032-akont = wl_akont.
        IF wg_zglt032-umskz NE ''.
          SELECT SINGLE skont FROM t074 INTO wl_hkont
            WHERE ktopl = '0050'
            AND   koart = 'K'
            AND   umskz = wg_zglt032-umskz
            AND   hkont = wl_akont.
          wg_zglt032-akont = wl_hkont.
        ENDIF.
        SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                             AND spras EQ sy-langu
                                                             AND ktopl EQ '0050'.

      WHEN 'D'.
        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wg_zglt032-hkont.
        SELECT SINGLE akont FROM knb1 INTO wl_akont         WHERE kunnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
        wg_zglt032-akont = wl_akont.
        IF wg_zglt032-umskz NE ''.
          SELECT SINGLE skont FROM t074 INTO wl_hkont
            WHERE ktopl = '0050'
            AND   koart = 'D'
            AND   umskz = wg_zglt032-umskz
            AND   hkont = wl_akont.
          wg_zglt032-akont = wl_hkont.
        ENDIF.
        SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                             AND spras EQ sy-langu
                                                             AND ktopl EQ '0050'.

        MOVE: wl_kna1-name1 TO wg_zglt032-descr.
      WHEN 'S'.
        SELECT SINGLE * FROM skat INTO wl_skat WHERE saknr EQ wg_zglt032-hkont
                                                 AND spras EQ sy-langu
                                                 AND ktopl EQ '0050'.
        MOVE: wl_skat-txt50 TO wg_zglt032-descr.
        wg_zglt032-descr_a = wg_zglt032-descr.
      WHEN 'I'.
        SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wg_zglt032-hkont.
        MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
        wg_zglt032-descr_a = wg_zglt032-descr.
      WHEN 'A'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_zglt032-hkont
          IMPORTING
            output = wl_anln1.

        SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wl_anln1.
        MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
    ENDCASE.

    IF wg_zglt032-descr IS INITIAL.
      CLEAR wg_zglt032-hkont.
    ENDIF.
    MODIFY tg_zglt032 FROM wg_zglt032 INDEX tabix.
  ENDLOOP.

ENDFORM.                    " F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_atualiza_alv.
  DATA: wl_tbsl  TYPE tbsl,
        wl_lfa1  TYPE lfa1,
        wl_kna1  TYPE kna1,
        wl_skat  TYPE skat,
        wl_anla  TYPE anla,
        wl_anln1 TYPE anla-anln1,
        wl_akont TYPE lfb1-akont,
        wl_hkont TYPE t074-hkont.

  CALL METHOD grid1->get_selected_cells
    IMPORTING
      et_cell = tg_selectedcell.

  LOOP AT tg_selectedcell INTO wg_selectedcell. "WHERE col_id-fieldname EQ 'HKONT'.
    CLEAR wg_zglt032.
    READ TABLE tg_zglt032 INTO wg_zglt032 INDEX wg_selectedcell-row_id-index.
    CHECK wg_zglt032-bschl IS NOT INITIAL.

    SELECT SINGLE * FROM tbsl
      INTO wl_tbsl
    WHERE bschl EQ wg_zglt032-bschl.


    CLEAR wg_zglt032-descr.
    MOVE wg_zglt032-hkont TO wg_zglt032-akont.
    CASE wl_tbsl-koart.
      WHEN 'K'.
        SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wg_zglt032-hkont.
        MOVE: wl_lfa1-name1 TO wg_zglt032-descr.
        "
        SELECT SINGLE akont FROM lfb1 INTO wl_akont         WHERE lifnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
        wg_zglt032-akont = wl_akont.
        IF wg_zglt032-umskz NE ''.
          SELECT SINGLE skont FROM t074 INTO wl_hkont
            WHERE ktopl = '0050'
            AND   koart = 'K'
            AND   umskz = wg_zglt032-umskz
            AND   hkont = wl_akont.
          wg_zglt032-akont = wl_hkont.
        ENDIF.
        SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                             AND spras EQ sy-langu
                                                             AND ktopl EQ '0050'.

      WHEN 'D'.
        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wg_zglt032-hkont.
        SELECT SINGLE akont FROM knb1 INTO wl_akont         WHERE kunnr = wg_zglt032-hkont  AND bukrs = wg_zglt031-bukrs.
        wg_zglt032-akont = wl_akont.
        IF wg_zglt032-umskz NE ''.
          SELECT SINGLE skont FROM t074 INTO wl_hkont
            WHERE ktopl = '0050'
            AND   koart = 'D'
            AND   umskz = wg_zglt032-umskz
            AND   hkont = wl_akont.
          wg_zglt032-akont = wl_hkont.
        ENDIF.
        SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                             AND spras EQ sy-langu
                                                             AND ktopl EQ '0050'.

        MOVE: wl_kna1-name1 TO wg_zglt032-descr.
      WHEN 'S'.
        SELECT SINGLE * FROM skat INTO wl_skat WHERE saknr EQ wg_zglt032-hkont
                                                 AND spras EQ sy-langu
                                                 AND ktopl EQ '0050'.
        MOVE: wl_skat-txt50 TO wg_zglt032-descr.
        wg_zglt032-descr_a = wg_zglt032-descr.
      WHEN 'I'.
        SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wg_zglt032-hkont.
        MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
        wg_zglt032-descr_a = wg_zglt032-descr.
      WHEN 'A'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_zglt032-hkont
          IMPORTING
            output = wl_anln1.

        SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wl_anln1.
        MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
    ENDCASE.

    IF wg_zglt032-descr IS INITIAL.
      CLEAR wg_zglt032-hkont.
    ENDIF.
    MODIFY tg_zglt032 FROM wg_zglt032 INDEX wg_selectedcell-row_id-index.
  ENDLOOP.

ENDFORM.                    " F_ATUALIZA_ALV


*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_doc INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_docs OCCURS 0,
          bukrs         TYPE zglt031-bukrs,
          tp_lcto       TYPE zglt031-tp_lcto,
          descricao     TYPE zglt031-descricao,
          usnam         TYPE zglt031-usnam,
          dep_resp      TYPE zglt031-dpto_resp,
          dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
        END OF tl_docs.


  SELECT zglt031~bukrs zglt031~tp_lcto zglt031~descricao zglt031~usnam zglt031~dpto_resp  zimp_cad_depto~dep_resp_desc
     FROM zglt031
     INNER JOIN zimp_cad_depto ON zimp_cad_depto~dep_resp = zglt031~dpto_resp
     INTO TABLE tl_docs.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TP_LCTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT031-TP_LCTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_docs
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BLART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_blart INPUT.
  DATA: tl_return_tab1 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc1      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_blart OCCURS 0,
          blart TYPE t003t-blart,
          ltext TYPE t003t-ltext,
        END OF tl_blart .

  SELECT blart ltext
     FROM t003t
     INTO TABLE tl_blart
    WHERE spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TP_LCTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT031-TP_LCTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_blart
      return_tab      = tl_return_tab1
      dynpfld_mapping = tl_dselc1.
ENDMODULE.                 " SEARCH_BLART  INPUT
*&---------------------------------------------------------------------*
*&      Form  LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM logs .

* SELECT *
*   FROM zglt032
*   INTO TABLE tg_zglt032
*   where TP_LCTO eq TP_LCTO.

* LOOP AT it_zglt032_log into wa_zglt032_log .
*   READ TABLE tg_zglT032 into  wg_zglt032 WITH KEY TP_LCTO = wa_zglt032_log-TP_LCTO.
*    READ TABLE IT_ZGLT076 INTO WA_ZGLT076 WITH KEY TP_LCTO = wa_zglt076-TP_LCTO.
*
*    IF wg_zglt032-TP_LCTO NE wa_zglt032_log-TP_LCTO.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-TP_LCTO.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-TP_LCTO.
*      APPEND  wa_zglt076 to it_zglt076.
**      insert ZGLT076 from  WA_ZGLT076.
*    ENDIF.
*
*    IF wg_zglt032-BSCHL NE wa_zglt032_log-BSCHL.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-BSCHL.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-BSCHL.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-HKONT NE wa_zglt032_log-HKONT.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-HKONT.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-HKONT.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-BUZEI NE wa_zglt032_log-BUZEI.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-BUZEI.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-BUZEI.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-UMSKZ NE wa_zglt032_log-UMSKZ.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-UMSKZ.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-UMSKZ.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-ANBWA NE wa_zglt032_log-ANBWA.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-ANBWA.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-ANBWA.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-BEWAR NE wa_zglt032_log-BEWAR.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-BEWAR.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-BEWAR.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-VBUND NE wa_zglt032_log-VBUND.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-VBUND.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-VBUND.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-KOSTL NE wa_zglt032_log-KOSTL.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-KOSTL.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-KOSTL.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-PRCTR NE wa_zglt032_log-PRCTR.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-PRCTR.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-PRCTR.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-AUFNR NE wa_zglt032_log-AUFNR.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-AUFNR.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-AUFNR.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-MATNR NE wa_zglt032_log-MATNR.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-MATNR.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-MATNR.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-MATNR_FI NE wa_zglt032_log-MATNR_FI.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-MATNR_FI.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-MATNR_FI.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-ZUONR NE wa_zglt032_log-ZUONR.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-ZUONR.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-ZUONR.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-SGTXT NE wa_zglt032_log-SGTXT.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-SGTXT.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-SGTXT.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*    IF wg_zglt032-TAX_CODE NE wa_zglt032_log-TAX_CODE.
*      WA_ZGLT076-VALUE_OLD = wA_zglt032_log-TAX_CODE.
*      WA_ZGLT076-VALUE_NEW = wg_zglt032-TAX_CODE.
*      APPEND  wa_zglt076 to it_zglt076.
*    ENDIF.
*
*
*clear: wa_zglt032_log.
* ENDLOOP.



ENDFORM.                    " LOGS
