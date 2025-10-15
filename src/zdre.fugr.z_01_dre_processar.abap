FUNCTION z_01_dre_processar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PBUKRS) TYPE  BUKRS DEFAULT '0001'
*"     REFERENCE(PGJAHR) TYPE  GJAHR DEFAULT '2015'
*"     REFERENCE(PPOPER) TYPE  POPER DEFAULT '001'
*"     REFERENCE(PVERSN) TYPE  VERSN_011 DEFAULT 'AM15'
*"     REFERENCE(PUKURS) TYPE  UKURS_CURR DEFAULT 0
*"  EXCEPTIONS
*"      ERRO_SQL
*"      ERRO_DRE
*"----------------------------------------------------------------------

  DATA: exc_ref                 TYPE REF TO cx_sy_native_sql_error,
        error_text              TYPE string,
        wa_registro             TYPE zdre_sql_proc,
        pmonat                  TYPE monat,
        it_registro             TYPE TABLE OF zdre_sql_proc    WITH HEADER LINE,
        it_zgl015_dre_est02     TYPE TABLE OF zgl015_dre_est02 WITH HEADER LINE,
        it_zgl015_dre_est03     TYPE TABLE OF zgl015_dre_est03 WITH HEADER LINE,
        it_zgl015_dre_est04     TYPE TABLE OF zgl015_dre_est04 WITH HEADER LINE,
        it_zgl015_dre_est05     TYPE TABLE OF zgl015_dre_est05 WITH HEADER LINE,
        it_zgl015_dre_est06     TYPE TABLE OF zgl015_dre_est06 WITH HEADER LINE,
        it_zgl021_dre_dados     TYPE SORTED TABLE OF zgl021_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr nivel saknr WITH HEADER LINE,
        it_zgl029_dre_dados     TYPE SORTED TABLE OF zgl029_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr saknr kostl prctr matkl vbund WITH HEADER LINE,
        it_zgl022_dre_dados     TYPE SORTED TABLE OF zgl022_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr nivel saknr kostl WITH HEADER LINE,
        it_zgl023_dre_dados     TYPE SORTED TABLE OF zgl023_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr nivel saknr prctr WITH HEADER LINE,
        it_zgl024_dre_dados     TYPE SORTED TABLE OF zgl024_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr nivel saknr matkl WITH HEADER LINE,

        it_zgl021_dre_dados_aux     TYPE TABLE OF zgl021_dre_dados,
        it_zgl029_dre_dados_aux     TYPE TABLE OF zgl029_dre_dados,
        it_zgl022_dre_dados_aux     TYPE TABLE OF zgl022_dre_dados,
        it_zgl023_dre_dados_aux     TYPE TABLE OF zgl023_dre_dados,
        it_zgl024_dre_dados_aux     TYPE TABLE OF zgl024_dre_dados,


        it_zgl024_dre_dados_exc TYPE SORTED TABLE OF zgl024_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr nivel saknr matkl WITH HEADER LINE,
        psql_bukrs              TYPE zgl015_dre_est03-bukrs,
        psql_gjahr              TYPE c LENGTH 4,
        psql_poper              TYPE c LENGTH 3,
        pbukrs_01               TYPE bukrs,
        pbukrs_0b               TYPE bukrs,
        wa_zgl015_dre_est08     TYPE TABLE OF zgl015_dre_est08 WITH HEADER LINE,
        lc_kokrs                TYPE kokrs.

  MOVE ppoper TO pmonat.

  MOVE: pbukrs TO psql_bukrs,
        pgjahr TO psql_gjahr,
        ppoper TO psql_poper.

  SELECT SINGLE kokrs INTO lc_kokrs FROM tka02 WHERE bukrs EQ pbukrs.

  "Ajuste de empresa do parâmetro"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Empresa Base Consulta
  MOVE pbukrs TO pbukrs_0b.

  "Empresa Base Estrutura
  SELECT SINGLE * INTO wa_zgl015_dre_est08 FROM zgl015_dre_est08
   WHERE versn   EQ pversn
     AND bukrs_b EQ pbukrs_0b.

  IF sy-subrc IS INITIAL.
    pbukrs_01 = wa_zgl015_dre_est08-bukrs.
  ELSE.
    pbukrs_01 = pbukrs.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Excluir DRE Atual. """""""""""""""""""""""""""""""""""""""""""""""""""""
  DELETE FROM zgl021_dre_dados WHERE bukrs EQ pbukrs
                                 AND versn EQ pversn
                                 AND monat EQ pmonat
                                 AND gjahr EQ pgjahr.

  DELETE FROM zgl022_dre_dados WHERE bukrs EQ pbukrs
                                 AND versn EQ pversn
                                 AND monat EQ pmonat
                                 AND gjahr EQ pgjahr.

  DELETE FROM zgl023_dre_dados WHERE bukrs EQ pbukrs
                                 AND versn EQ pversn
                                 AND monat EQ pmonat
                                 AND gjahr EQ pgjahr.

  DELETE FROM zgl024_dre_dados WHERE bukrs EQ pbukrs
                                 AND versn EQ pversn
                                 AND monat EQ pmonat
                                 AND gjahr EQ pgjahr.

  DELETE FROM zgl029_dre_dados WHERE bukrs EQ pbukrs
                                 AND versn EQ pversn
                                 AND monat EQ pmonat
                                 AND gjahr EQ pgjahr.

  CALL FUNCTION 'Z_01_DRE_SQL_PROC'
    EXPORTING
      pbukrs      = pbukrs
      pgjahr      = pgjahr
      ppoper      = ppoper
      pversn      = pversn
    TABLES
      it_registro = it_registro
    EXCEPTIONS
      erro_sql    = 1
      erro_dre    = 2
      OTHERS      = 3.

  IF sy-subrc IS NOT INITIAL.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_dre.
      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_dre.
    ENDCASE.
  ENDIF.

  CHECK it_registro[] IS NOT INITIAL.

  "Contas Configuradas dentro da Estrutura da DRE da Empresa
  SELECT * INTO TABLE it_zgl015_dre_est03
    FROM zgl015_dre_est03 AS a
   WHERE a~bukrs EQ pbukrs_01
     AND a~versn EQ pversn
     AND NOT EXISTS ( SELECT * FROM zgl015_dre_est04 AS b  "Estrutura de DRE – Tipo de centro de custo
                       WHERE b~bukrs EQ a~bukrs
                         AND b~versn EQ a~versn
                         AND b~ktopl EQ a~ktopl
                         AND b~saknr EQ a~saknr )
     AND NOT EXISTS ( SELECT * FROM zgl015_dre_est05 AS c  "Estrutura de DRE – Centro de lucro
                       WHERE c~versn EQ a~versn
                         AND c~ktopl EQ a~ktopl
                         AND c~saknr EQ a~saknr
                         AND c~kokrs EQ lc_kokrs )
     AND NOT EXISTS ( SELECT * FROM zgl015_dre_est06 AS d  "Estrutura de DRE – Grupo de mercadorias
                       WHERE d~bukrs EQ a~bukrs
                         AND d~versn EQ a~versn
                         AND d~ktopl EQ a~ktopl
                         AND d~saknr EQ a~saknr ).

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e008(zdre) WITH pbukrs pversn RAISING erro_dre.
  ENDIF.

  SELECT * INTO TABLE it_zgl015_dre_est04
    FROM zgl015_dre_est04
   WHERE bukrs EQ pbukrs_01
     AND versn EQ pversn.

  SELECT * INTO TABLE it_zgl015_dre_est05
    FROM zgl015_dre_est05
   WHERE bukrs EQ pbukrs_01
     AND versn EQ pversn
     AND kokrs EQ lc_kokrs.

  SELECT * INTO TABLE it_zgl015_dre_est06
    FROM zgl015_dre_est06
   WHERE bukrs EQ pbukrs_01
     AND versn EQ pversn.


  "Total de Conta do Razão  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  LOOP AT it_zgl015_dre_est03.
    LOOP AT it_registro WHERE saknr EQ it_zgl015_dre_est03-saknr.

      CLEAR: it_zgl029_dre_dados.
      MOVE it_registro-poper TO it_zgl029_dre_dados-monat.
      it_zgl029_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl029_dre_dados-versn          = it_registro-versn.
      it_zgl029_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl029_dre_dados-saknr          = it_registro-saknr.
      it_zgl029_dre_dados-vbund          = it_registro-vbund.
      it_zgl029_dre_dados-qtd_ton        = 0.
      it_zgl029_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl029_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl029_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl029_dre_dados.

      MOVE it_registro-poper TO it_zgl021_dre_dados-monat.
      it_zgl021_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl021_dre_dados-versn          = it_registro-versn.
      it_zgl021_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl021_dre_dados-nivel          = it_zgl015_dre_est03-nivel.
      it_zgl021_dre_dados-saknr          = it_zgl015_dre_est03-saknr.
      it_zgl021_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl021_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl021_dre_dados-vlr_grupo      = it_registro-vlosl.
      it_zgl021_dre_dados-vlr_dolar_conv = it_registro-vlksl * pukurs.
      COLLECT it_zgl021_dre_dados.
    ENDLOOP.
  ENDLOOP.
  "Total de Conta do Razão  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Total de Centro de Custo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*  IF it_registro[] IS NOT INITIAL.
*    DATA(lit_registro_aux) = it_registro[].
*    DELETE lit_registro_aux WHERE kostl IS INITIAL.
*    IF lit_registro_aux[] IS NOT INITIAL.
*      SELECT *
*        FROM zgl015_dre_cvend INTO TABLE @DATA(lit_zgl015_dre_cvend)
*        FOR ALL ENTRIES IN @lit_registro_aux
*       WHERE BUKRS EQ @lit_registro_aux-bukrs
*         AND HKONT EQ @lit_registro_aux-saknr
*         AND KOSTL EQ @lit_registro_aux-kostl.
*    ENDIF.
*  ENDIF.

  LOOP AT it_zgl015_dre_est04.
    LOOP AT it_registro WHERE saknr EQ it_zgl015_dre_est04-saknr
                          AND kosar EQ it_zgl015_dre_est04-kosar.

      CLEAR: it_zgl029_dre_dados.
      MOVE it_registro-poper TO it_zgl029_dre_dados-monat.
      it_zgl029_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl029_dre_dados-versn          = it_registro-versn.
      it_zgl029_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl029_dre_dados-saknr          = it_registro-saknr.
      it_zgl029_dre_dados-vbund          = it_registro-vbund.
      it_zgl029_dre_dados-kostl          = it_registro-kostl.
      it_zgl029_dre_dados-qtd_ton        = 0.
      it_zgl029_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl029_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl029_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl029_dre_dados.

      CLEAR: it_zgl022_dre_dados.
      MOVE it_registro-poper TO it_zgl022_dre_dados-monat.
      it_zgl022_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl022_dre_dados-nivel          = it_zgl015_dre_est04-nivel.
      it_zgl022_dre_dados-versn          = it_registro-versn.
      it_zgl022_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl022_dre_dados-saknr          = it_registro-saknr.
      it_zgl022_dre_dados-kostl          = it_registro-kostl.
      it_zgl022_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl022_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl022_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl022_dre_dados.

*      IF it_registro-kostl is NOT INITIAL.
*        READ TABLE lit_zgl015_dre_cvend INTO DATA(lwa_zgl015_dre_cvend) WITH KEY BUKRS = it_registro-bukrs
*                                                                                 HKONT = it_registro-saknr
*                                                                                 KOSTL = it_registro-kostl.
*        IF SY-SUBRC EQ 0.
*          READ TABLE it_zgl015_dre_est06 WITH KEY saknr = lwa_zgl015_dre_cvend-hkont
*                                                  matkl = lwa_zgl015_dre_cvend-matkl.
*          IF SY-SUBRC EQ 0.
*            CLEAR: it_zgl024_dre_dados_exc.
*            MOVE it_registro-poper TO it_zgl024_dre_dados_exc-monat.
*            it_zgl024_dre_dados_exc-bukrs          = it_registro-bukrs.
*            it_zgl024_dre_dados_exc-nivel          = it_zgl015_dre_est06-nivel.
*            it_zgl024_dre_dados_exc-versn          = it_registro-versn.
*            it_zgl024_dre_dados_exc-gjahr          = it_registro-gjahr.
*            it_zgl024_dre_dados_exc-saknr          = it_registro-saknr.
*            it_zgl024_dre_dados_exc-matkl          = it_registro-matkl.
*            it_zgl024_dre_dados_exc-qtd_ton        = it_registro-qtmsl.
*            it_zgl024_dre_dados_exc-vlr_rea        = it_registro-vlhsl.
*            it_zgl024_dre_dados_exc-vlr_dolar      = it_registro-vlksl.
*            it_zgl024_dre_dados_exc-vlr_grupo      = it_registro-vlosl.
*            COLLECT it_zgl024_dre_dados_exc.
*          ENDIF.
*        ENDIF.
*      ENDIF.


    ENDLOOP.
  ENDLOOP.
  "Total de Centro de Custo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Total de Centro de Lucro """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  LOOP AT it_zgl015_dre_est05.
    LOOP AT it_registro WHERE saknr EQ it_zgl015_dre_est05-saknr
                          AND prctr EQ it_zgl015_dre_est05-prctr.

      CLEAR: it_zgl029_dre_dados.
      MOVE it_registro-poper TO it_zgl029_dre_dados-monat.
      it_zgl029_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl029_dre_dados-versn          = it_registro-versn.
      it_zgl029_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl029_dre_dados-saknr          = it_registro-saknr.
      it_zgl029_dre_dados-vbund          = it_registro-vbund.
      it_zgl029_dre_dados-prctr          = it_registro-prctr.
      it_zgl029_dre_dados-qtd_ton        = it_registro-qtmsl.
      it_zgl029_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl029_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl029_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl029_dre_dados.

      CLEAR: it_zgl023_dre_dados.
      MOVE it_registro-poper TO it_zgl023_dre_dados-monat.
      it_zgl023_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl023_dre_dados-nivel          = it_zgl015_dre_est05-nivel.
      it_zgl023_dre_dados-versn          = it_registro-versn.
      it_zgl023_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl023_dre_dados-saknr          = it_registro-saknr.
      it_zgl023_dre_dados-prctr          = it_registro-prctr.
      it_zgl023_dre_dados-qtd_ton        = it_registro-qtmsl.
      it_zgl023_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl023_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl023_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl023_dre_dados.

    ENDLOOP.
  ENDLOOP.
  "Total de Centro de Lucro """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Total de Grupo de Mercadoria """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  LOOP AT it_zgl015_dre_est06.
    LOOP AT it_registro WHERE saknr EQ it_zgl015_dre_est06-saknr
                          AND matkl EQ it_zgl015_dre_est06-matkl.

      CLEAR: it_zgl029_dre_dados.
      MOVE it_registro-poper TO it_zgl029_dre_dados-monat.
      it_zgl029_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl029_dre_dados-versn          = it_registro-versn.
      it_zgl029_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl029_dre_dados-saknr          = it_registro-saknr.
      it_zgl029_dre_dados-vbund          = it_registro-vbund.
      it_zgl029_dre_dados-matkl          = it_registro-matkl.
      it_zgl029_dre_dados-qtd_ton        = it_registro-qtmsl.
      it_zgl029_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl029_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl029_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl029_dre_dados.

      CLEAR: it_zgl024_dre_dados.
      MOVE it_registro-poper TO it_zgl024_dre_dados-monat.
      it_zgl024_dre_dados-bukrs          = it_registro-bukrs.
      it_zgl024_dre_dados-nivel          = it_zgl015_dre_est06-nivel.
      it_zgl024_dre_dados-versn          = it_registro-versn.
      it_zgl024_dre_dados-gjahr          = it_registro-gjahr.
      it_zgl024_dre_dados-saknr          = it_registro-saknr.
      it_zgl024_dre_dados-matkl          = it_registro-matkl.
      it_zgl024_dre_dados-qtd_ton        = it_registro-qtmsl.
      it_zgl024_dre_dados-vlr_rea        = it_registro-vlhsl.
      it_zgl024_dre_dados-vlr_dolar      = it_registro-vlksl.
      it_zgl024_dre_dados-vlr_grupo      = it_registro-vlosl.
      COLLECT it_zgl024_dre_dados.

    ENDLOOP.
  ENDLOOP.

*alterado por guilherme rabelo inicio


  UPDATE zglt_dre_02
  SET nivel = ' '
      tp_conta = ' '
      WHERE bukrs = pbukrs
        AND gjahr = pgjahr
        AND poper = ppoper.

  COMMIT WORK AND WAIT.

  UPDATE zglt_dre_04
  SET nivel = ' '
      tp_conta = ' '
      WHERE bukrs = pbukrs
        AND gjahr = pgjahr
        AND poper = ppoper.

  COMMIT WORK AND WAIT.
*------------------------------------------------------------- Para contas do razão *

  DATA: it_zglt_dre_02 TYPE TABLE OF zglt_dre_02.
  DATA: r_monat TYPE RANGE OF zglt_dre_02-poper,
        s_monat LIKE LINE OF r_monat.

  DATA: xtabela_zglt_dre_02         TYPE TABLE OF zglt_dre_02,
        xtabela_zglt_dre_02_custeio TYPE TABLE OF zglt_dre_02.


  REFRESH r_monat.

  LOOP AT it_zgl021_dre_dados INTO DATA(ls_zgl021_dre_dados).

    IF  ls_zgl021_dre_dados-monat = '12'.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '012'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '013'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '014'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '015'.
      APPEND s_monat TO r_monat.

    ELSE.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = ls_zgl021_dre_dados-monat.
      APPEND s_monat TO r_monat.
    ENDIF.

    DELETE  ADJACENT DUPLICATES FROM r_monat COMPARING ALL FIELDS.
  ENDLOOP.

  IF NOT it_zgl021_dre_dados IS INITIAL.
    SELECT * FROM  zglt_dre_02
      INTO TABLE it_zglt_dre_02
      FOR ALL ENTRIES IN it_zgl021_dre_dados
      WHERE bukrs = it_zgl021_dre_dados-bukrs
        AND gjahr = it_zgl021_dre_dados-gjahr
        AND poper IN r_monat
        AND saknr = it_zgl021_dre_dados-saknr.


    IF sy-subrc = 0.

      it_zgl021_dre_dados_aux = it_zgl021_dre_dados[]. "WPP
      SORT it_zgl021_dre_dados_aux by bukrs gjahr monat saknr. "WPP


      LOOP AT it_zglt_dre_02 INTO DATA(wa_zglt_dre_02).

        READ TABLE it_zgl021_dre_dados_aux INTO DATA(zgl021_dre_dados) WITH KEY  bukrs = wa_zglt_dre_02-bukrs
                                                                                 gjahr = wa_zglt_dre_02-gjahr
                                                                                 monat = wa_zglt_dre_02-poper+1(2)
                                                                                 saknr = wa_zglt_dre_02-saknr
                                                                                 BINARY SEARCH.

        IF sy-subrc = 0.

          wa_zglt_dre_02-nivel = zgl021_dre_dados-nivel.
          wa_zglt_dre_02-tp_conta = 'RZ'.

          MODIFY zglt_dre_02 FROM wa_zglt_dre_02.

          APPEND wa_zglt_dre_02 TO xtabela_zglt_dre_02.

        ELSEIF wa_zglt_dre_02-poper = '012' OR wa_zglt_dre_02-poper = '013' OR wa_zglt_dre_02-poper = '014' OR wa_zglt_dre_02-poper = '015'.

          READ TABLE it_zgl021_dre_dados_aux INTO DATA(zgl021_dre_dados_12) WITH KEY bukrs = wa_zglt_dre_02-bukrs
                                                                                     gjahr = wa_zglt_dre_02-gjahr
                                                                                     monat = '12'
                                                                                     saknr = wa_zglt_dre_02-saknr
                                                                                     BINARY SEARCH.

          IF sy-subrc = 0.

            wa_zglt_dre_02-nivel = zgl021_dre_dados_12-nivel.
            wa_zglt_dre_02-tp_conta = 'RZ'.

            MODIFY zglt_dre_02 FROM wa_zglt_dre_02.

            APPEND wa_zglt_dre_02 TO xtabela_zglt_dre_02.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.
*------------------------------------- Para contas de Centro de custo *

  DATA:it_zglt_dre_02_cc TYPE TABLE OF zglt_dre_02.


  REFRESH r_monat.

  LOOP AT it_zgl022_dre_dados INTO DATA(ls_zglt_dre_02_cc).

    IF  ls_zglt_dre_02_cc-monat = '12'.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '012'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '013'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '014'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '015'.
      APPEND s_monat TO r_monat.

    ELSE.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = ls_zglt_dre_02_cc-monat.
      APPEND s_monat TO r_monat.
    ENDIF.

    DELETE  ADJACENT DUPLICATES FROM r_monat COMPARING ALL FIELDS.
  ENDLOOP.

  IF NOT it_zgl022_dre_dados IS INITIAL.

    SELECT * FROM zglt_dre_02
      INTO TABLE it_zglt_dre_02_cc
      FOR ALL ENTRIES IN it_zgl022_dre_dados
      WHERE bukrs = it_zgl022_dre_dados-bukrs
        AND gjahr = it_zgl022_dre_dados-gjahr
        AND poper IN r_monat
        AND saknr = it_zgl022_dre_dados-saknr
        AND kostl = it_zgl022_dre_dados-kostl.


    IF sy-subrc = 0.

      DATA:wa_aufk TYPE aufk.

      SELECT * FROM aufk
        INTO TABLE @DATA(it_aufk)
        FOR ALL ENTRIES IN @it_zglt_dre_02_cc
        WHERE aufnr = @it_zglt_dre_02_cc-aufnr.
*        AND auart <> 'ZSIN' AND auart <> 'ZSTA'.

      SORT it_aufk by aufnr.

      it_zgl022_dre_dados_aux = it_zgl022_dre_dados[].
      SORT it_zgl022_dre_dados_aux by bukrs gjahr saknr monat kostl.

      LOOP AT it_zglt_dre_02_cc INTO DATA(wa_zglt_dre_02_cc).

*        CLEAR: lwa_zgl015_dre_cvend.
*        READ TABLE lit_zgl015_dre_cvend INTO lwa_zgl015_dre_cvend WITH KEY BUKRS = wa_zglt_dre_02_cc-bukrs
*                                                                           HKONT = wa_zglt_dre_02_cc-saknr
*                                                                           KOSTL = wa_zglt_dre_02_cc-kostl.
*        IF SY-SUBRC EQ 0. "Exceção
*          READ TABLE it_zgl024_dre_dados_exc INTO DATA(wa_zgl024_dre_dados) WITH KEY bukrs = wa_zglt_dre_02_cc-bukrs
*                                                                                     gjahr = wa_zglt_dre_02_cc-gjahr
*                                                                                     monat = wa_zglt_dre_02_cc-poper+1(2)
*                                                                                     saknr = wa_zglt_dre_02_cc-saknr
*                                                                                     matkl = lwa_zgl015_dre_cvend-matkl.
*          IF sy-subrc = 0.
*
*            wa_zglt_dre_02_cc-nivel    = wa_zgl024_dre_dados-nivel.
*            wa_zglt_dre_02_cc-tp_conta = 'MT'.
*
*            MODIFY zglt_dre_02 FROM  wa_zglt_dre_02_cc.
*            APPEND wa_zglt_dre_02_cc TO xtabela_zglt_dre_02.
*
*          ELSEIF wa_zglt_dre_02_cc-poper = '012' OR wa_zglt_dre_02_cc-poper = '013' OR wa_zglt_dre_02_cc-poper = '014' OR wa_zglt_dre_02_cc-poper = '015'.
*
*            READ TABLE it_zgl024_dre_dados INTO DATA(wa_zgl024_dre_dados_12) WITH KEY bukrs = wa_zglt_dre_02_cc-bukrs
*                                                                                      gjahr = wa_zglt_dre_02_cc-gjahr
*                                                                                      monat = '12'
*                                                                                      saknr = wa_zglt_dre_02_cc-saknr
*                                                                                      matkl = lwa_zgl015_dre_cvend-matkl.
*
*            IF sy-subrc = 0.
*              wa_zglt_dre_02_cc-nivel = wa_zgl024_dre_dados_12-nivel.
*              wa_zglt_dre_02_cc-tp_conta = 'MT'.
*
*              MODIFY zglt_dre_02 FROM  wa_zglt_dre_02_cc.
*              APPEND wa_zglt_dre_02_cc TO xtabela_zglt_dre_02.
*            ENDIF.
*          ENDIF.
*
*          CONTINUE.
*        ENDIF.

        CLEAR:wa_aufk.
        READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_zglt_dre_02_cc-aufnr BINARY SEARCH.

        CHECK ( sy-subrc = 0 AND wa_aufk-auart = 'ZSIN' OR wa_aufk-auart = 'ZSTA' AND wa_zglt_dre_02_cc-aufnr IS NOT INITIAL ) OR ( wa_zglt_dre_02_cc-aufnr = ' ' ).

        READ TABLE it_zgl022_dre_dados_aux INTO DATA(wa_zgl022_dre_dados) WITH KEY bukrs = wa_zglt_dre_02_cc-bukrs
                                                                                   gjahr = wa_zglt_dre_02_cc-gjahr
                                                                                   saknr = wa_zglt_dre_02_cc-saknr
                                                                                   monat = wa_zglt_dre_02_cc-poper+1(2)
                                                                                   kostl = wa_zglt_dre_02_cc-kostl
                                                                                   BINARY SEARCH.

        IF sy-subrc = 0.

          wa_zglt_dre_02_cc-nivel = wa_zgl022_dre_dados-nivel.
          wa_zglt_dre_02_cc-tp_conta = 'CC'.
          MODIFY zglt_dre_02 FROM wa_zglt_dre_02_cc.
          APPEND wa_zglt_dre_02_cc TO xtabela_zglt_dre_02.

        ELSEIF wa_zglt_dre_02_cc-poper = '012' OR wa_zglt_dre_02_cc-poper = '013' OR wa_zglt_dre_02_cc-poper = '014' OR wa_zglt_dre_02_cc-poper = '015'.

          READ TABLE it_zgl022_dre_dados_aux INTO DATA(wa_zgl022_dre_dados_12) WITH KEY  bukrs = wa_zglt_dre_02_cc-bukrs
                                                                                         gjahr = wa_zglt_dre_02_cc-gjahr
                                                                                         saknr = wa_zglt_dre_02_cc-saknr
                                                                                         monat = '12'
                                                                                         kostl = wa_zglt_dre_02_cc-kostl
                                                                                         BINARY SEARCH.

          IF sy-subrc = 0.

            wa_zglt_dre_02_cc-nivel = wa_zgl022_dre_dados_12-nivel.
            wa_zglt_dre_02_cc-tp_conta = 'CC'.
            MODIFY zglt_dre_02 FROM wa_zglt_dre_02_cc.
            APPEND wa_zglt_dre_02_cc TO xtabela_zglt_dre_02.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.

*----------------------------------------------------- Para contas de Centro de Lucro *

  DATA: it_zglt_dre_02_cl TYPE TABLE OF zglt_dre_02.


  REFRESH r_monat.

  LOOP AT it_zgl023_dre_dados INTO DATA(ls_zgl023_dre_dados).

    IF  ls_zgl023_dre_dados-monat = '12'.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '012'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '013'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '014'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '015'.
      APPEND s_monat TO r_monat.

    ELSE.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = ls_zgl023_dre_dados-monat.
      APPEND s_monat TO r_monat.
    ENDIF.
    DELETE  ADJACENT DUPLICATES FROM r_monat COMPARING ALL FIELDS.
  ENDLOOP.

  IF NOT it_zgl023_dre_dados IS INITIAL.

    SELECT * FROM zglt_dre_02
      INTO TABLE it_zglt_dre_02_cl
      FOR ALL ENTRIES IN it_zgl023_dre_dados
          WHERE bukrs = it_zgl023_dre_dados-bukrs
            AND gjahr = it_zgl023_dre_dados-gjahr
            AND poper IN r_monat
            AND saknr = it_zgl023_dre_dados-saknr
            AND prctr = it_zgl023_dre_dados-prctr.


    IF sy-subrc = 0.

      it_zgl023_dre_dados_aux = it_zgl023_dre_dados[].
      SORT it_zgl023_dre_dados_aux by bukrs gjahr monat saknr prctr.

      LOOP AT it_zglt_dre_02_cl INTO DATA(wa_zglt_dre_02_cl).

        READ TABLE it_zgl023_dre_dados_aux INTO DATA(wa_zgl023_dre_dados) WITH KEY  bukrs = wa_zglt_dre_02_cl-bukrs
                                                                                    gjahr = wa_zglt_dre_02_cl-gjahr
                                                                                    monat = wa_zglt_dre_02_cl-poper+1(2)
                                                                                    saknr = wa_zglt_dre_02_cl-saknr
                                                                                    prctr = wa_zglt_dre_02_cl-prctr
                                                                                    BINARY SEARCH.

        IF sy-subrc = 0.

          wa_zglt_dre_02_cl-nivel = wa_zgl023_dre_dados-nivel.
          wa_zglt_dre_02_cl-tp_conta = 'CL'.

          MODIFY zglt_dre_02 FROM wa_zglt_dre_02_cl.
          APPEND wa_zglt_dre_02_cl TO xtabela_zglt_dre_02.

        ELSEIF wa_zglt_dre_02_cl-poper = '012' OR wa_zglt_dre_02_cl-poper = '013' OR wa_zglt_dre_02_cl-poper = '014' OR  wa_zglt_dre_02_cl-poper = '015'.

          READ TABLE it_zgl023_dre_dados_aux INTO DATA(wa_zgl023_dre_dados_12) WITH KEY  bukrs = wa_zglt_dre_02_cl-bukrs
                                                                                         gjahr = wa_zglt_dre_02_cl-gjahr
                                                                                         monat = '12'
                                                                                         saknr = wa_zglt_dre_02_cl-saknr
                                                                                         prctr = wa_zglt_dre_02_cl-prctr
                                                                                         BINARY SEARCH.

          IF sy-subrc = 0.

            wa_zglt_dre_02_cl-nivel = wa_zgl023_dre_dados_12-nivel.
            wa_zglt_dre_02_cl-tp_conta = 'CL'.

            MODIFY zglt_dre_02 FROM wa_zglt_dre_02_cl.
            APPEND wa_zglt_dre_02_cl TO xtabela_zglt_dre_02.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

*------------------- Para contas de Material *

  DATA:it_zglt_dre_02_mt TYPE TABLE OF zglt_dre_02.

  REFRESH r_monat.

  LOOP AT it_zgl024_dre_dados INTO DATA(ls_zgl024_dre_dados).

    IF  ls_zgl024_dre_dados-monat = '12'.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '012'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '013'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '014'.
      APPEND s_monat TO r_monat.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = '015'.
      APPEND s_monat TO r_monat.

    ELSE.

      s_monat-option = 'EQ'.
      s_monat-sign = 'I'.
      s_monat-low = ls_zgl024_dre_dados-monat.
      APPEND s_monat TO r_monat.
    ENDIF.

    DELETE  ADJACENT DUPLICATES FROM r_monat COMPARING ALL FIELDS.
  ENDLOOP.

  IF NOT it_zgl024_dre_dados IS INITIAL.

    SELECT * FROM zglt_dre_02
      INTO TABLE it_zglt_dre_02_mt
      FOR ALL ENTRIES IN it_zgl024_dre_dados
      WHERE bukrs = it_zgl024_dre_dados-bukrs
        AND gjahr = it_zgl024_dre_dados-gjahr
        AND poper IN r_monat
        AND saknr = it_zgl024_dre_dados-saknr
        AND matkl = it_zgl024_dre_dados-matkl.

    IF sy-subrc = 0.

      it_zgl024_dre_dados_aux = it_zgl024_dre_dados[].
      sort it_zgl024_dre_dados_aux by bukrs gjahr monat saknr matkl.

      LOOP AT it_zglt_dre_02_mt INTO DATA(wa_zglt_dre_02_mt).

        READ TABLE it_zgl024_dre_dados_aux INTO DATA(wa_zgl024_dre_dados) WITH KEY  bukrs = wa_zglt_dre_02_mt-bukrs
                                                                                    gjahr = wa_zglt_dre_02_mt-gjahr
                                                                                    monat = wa_zglt_dre_02_mt-poper+1(2)
                                                                                    saknr = wa_zglt_dre_02_mt-saknr
                                                                                    matkl = wa_zglt_dre_02_mt-matkl
                                                                                    BINARY SEARCH.

        IF sy-subrc = 0.

          wa_zglt_dre_02_mt-nivel = wa_zgl024_dre_dados-nivel.
          wa_zglt_dre_02_mt-tp_conta = 'MT'.

          MODIFY zglt_dre_02 FROM  wa_zglt_dre_02_mt.
          APPEND wa_zglt_dre_02_mt TO xtabela_zglt_dre_02.

        ELSEIF wa_zglt_dre_02_mt-poper = '012' OR wa_zglt_dre_02_mt-poper = '013' OR wa_zglt_dre_02_mt-poper = '014' OR wa_zglt_dre_02_mt-poper = '015'.

          READ TABLE it_zgl024_dre_dados_aux INTO DATA(wa_zgl024_dre_dados_12) WITH KEY bukrs = wa_zglt_dre_02_mt-bukrs
                                                                                        gjahr = wa_zglt_dre_02_mt-gjahr
                                                                                        monat = '12'
                                                                                        saknr = wa_zglt_dre_02_mt-saknr
                                                                                        matkl = wa_zglt_dre_02_mt-matkl
                                                                                        BINARY SEARCH.

          IF sy-subrc = 0.

            wa_zglt_dre_02_mt-nivel = wa_zgl024_dre_dados_12-nivel.
            wa_zglt_dre_02_mt-tp_conta = 'MT'.

            MODIFY zglt_dre_02 FROM  wa_zglt_dre_02_mt.
            APPEND wa_zglt_dre_02_mt TO xtabela_zglt_dre_02.

          ENDIF.

        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.


* ---------ZGLT_DRE_04 para contas razão, de centro de custo, de centro de lucro e de Material *

  IF NOT xtabela_zglt_dre_02 IS INITIAL.

    SELECT * FROM zglt_dre_04
      INTO TABLE @DATA(it_zglt_dre_04)
      FOR ALL ENTRIES IN @xtabela_zglt_dre_02
      WHERE bukrs  = @xtabela_zglt_dre_02-bukrs
        AND gjahr =  @xtabela_zglt_dre_02-gjahr
        AND poper =  @xtabela_zglt_dre_02-poper
        AND saknr =  @xtabela_zglt_dre_02-saknr
        AND kostl =  @xtabela_zglt_dre_02-kostl
        AND prctr =  @xtabela_zglt_dre_02-prctr
        AND matkl =  @xtabela_zglt_dre_02-matkl
        AND aufnr =  @xtabela_zglt_dre_02-aufnr.

    IF sy-subrc = 0.

      SORT xtabela_zglt_dre_02 by bukrs  gjahr poper saknr kostl prctr matkl aufnr.

      LOOP AT it_zglt_dre_04 ASSIGNING FIELD-SYMBOL(<fs_zglt_dre_04>).

        READ TABLE xtabela_zglt_dre_02 INTO DATA(wa_xtabela_zglt_dre_02) WITH KEY bukrs  = <fs_zglt_dre_04>-bukrs
                                                                                  gjahr =  <fs_zglt_dre_04>-gjahr
                                                                                  poper =  <fs_zglt_dre_04>-poper
                                                                                  saknr =  <fs_zglt_dre_04>-saknr
                                                                                  kostl =  <fs_zglt_dre_04>-kostl
                                                                                  prctr =  <fs_zglt_dre_04>-prctr
                                                                                  matkl =  <fs_zglt_dre_04>-matkl
                                                                                  aufnr =  <fs_zglt_dre_04>-aufnr
                                                                                  BINARY SEARCH.

        IF sy-subrc = 0.
          <fs_zglt_dre_04>-nivel    = wa_xtabela_zglt_dre_02-nivel.
          <fs_zglt_dre_04>-tp_conta = wa_xtabela_zglt_dre_02-tp_conta.
        ENDIF.

      ENDLOOP.

      MODIFY zglt_dre_04 FROM TABLE it_zglt_dre_04.
    ENDIF.

  ENDIF.


*-------------------------------------- Para contas de Custeio  - Nível igual a vazio *
*Na tabela ZGLT_DRE_02 existem registros que não foram tratados na seleções acima, portanto estarão com o nível em branco.

  IF ppoper EQ '012'.

    SELECT * FROM zglt_dre_02
    INTO TABLE @DATA(it_zglt_dre_02_vazio)
    WHERE  bukrs = @pbukrs
       AND gjahr = @pgjahr
       AND poper IN ('012','013','014','015')
       AND nivel  = ' '.
  ELSE.

    SELECT * FROM zglt_dre_02
    INTO TABLE it_zglt_dre_02_vazio
    WHERE  bukrs = pbukrs
       AND gjahr = pgjahr
       AND poper = ppoper
       AND nivel = ' '.

  ENDIF.

  IF it_zglt_dre_02_vazio[] IS NOT INITIAL.
    DATA(it_zglt_dre_02_vazio_aux) = it_zglt_dre_02_vazio[].
    DELETE it_zglt_dre_02_vazio_aux WHERE kostl IS INITIAL.
    IF it_zglt_dre_02_vazio_aux[] IS NOT INITIAL.
      SELECT *
        FROM zgl015_dre_cvend INTO TABLE @DATA(lit_zgl015_dre_cvend)
        FOR ALL ENTRIES IN @it_zglt_dre_02_vazio_aux
       WHERE bukrs EQ @it_zglt_dre_02_vazio_aux-bukrs
         AND hkont EQ @it_zglt_dre_02_vazio_aux-saknr
         AND kostl EQ @it_zglt_dre_02_vazio_aux-kostl.
    ENDIF.
  ENDIF.

  IF it_zglt_dre_02_vazio[] IS NOT INITIAL.

    SELECT * FROM zgl015_dre_est03
      INTO TABLE @DATA(it_zgl015_dre_est03_vazio)
      FOR ALL ENTRIES IN @it_zglt_dre_02_vazio
      WHERE bukrs  = @pbukrs_01
        AND versn  = @pversn
        AND saknr =  @it_zglt_dre_02_vazio-saknr.

    IF sy-subrc = 0.

      SORT lit_zgl015_dre_cvend by bukrs hkont kostl.
      SORT it_zgl015_dre_est06 by saknr matkl.
      SORT it_zgl015_dre_est03_vazio by saknr.

      LOOP AT it_zglt_dre_02_vazio INTO DATA(wa_zglt_dre_02_vazio).

        READ TABLE lit_zgl015_dre_cvend INTO DATA(lwa_zgl015_dre_cvend) WITH KEY bukrs = wa_zglt_dre_02_vazio-bukrs
                                                                                 hkont = wa_zglt_dre_02_vazio-saknr
                                                                                 kostl = wa_zglt_dre_02_vazio-kostl
                                                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE it_zgl015_dre_est06 WITH KEY saknr = lwa_zgl015_dre_cvend-hkont
                                                  matkl = lwa_zgl015_dre_cvend-matkl BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_zglt_dre_02_vazio-tp_conta = 'MT'.
            wa_zglt_dre_02_vazio-nivel    = it_zgl015_dre_est06-nivel.

            MODIFY zglt_dre_02 FROM wa_zglt_dre_02_vazio.
            APPEND wa_zglt_dre_02_vazio TO xtabela_zglt_dre_02_custeio.
            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE it_zgl015_dre_est03_vazio INTO DATA(wa_zgl015_dre_est03) WITH KEY saknr =  wa_zglt_dre_02_vazio-saknr BINARY SEARCH.


        IF wa_zgl015_dre_est03-nivel = ' ' OR wa_zglt_dre_02_vazio-nivel = ' '.

*            wa_zglt_dre_02_vazio-nivel = wa_zgl015_dre_est03-nivel.
          wa_zglt_dre_02_vazio-tp_conta = 'CT'.

          MODIFY zglt_dre_02 FROM wa_zglt_dre_02_vazio.
          APPEND wa_zglt_dre_02_vazio TO xtabela_zglt_dre_02_custeio.
*
*          ELSE.
*
*            wa_zglt_dre_02_vazio-tp_conta = 'CC'.
*            wa_zglt_dre_02_vazio-nivel = wa_zgl015_dre_est03-nivel.
*
*            MODIFY zglt_dre_02 FROM wa_zglt_dre_02_vazio.
*            APPEND wa_zglt_dre_02_vazio TO xtabela_zglt_dre_02_custeio.

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.


*--------------------------------Para tabela ZGLT_DRE_04

  IF NOT xtabela_zglt_dre_02_custeio IS INITIAL.

    SELECT * FROM zglt_dre_04
      INTO TABLE @DATA(it_zglt_dre_04_vazio)
      FOR ALL ENTRIES IN @xtabela_zglt_dre_02_custeio
       WHERE bukrs  = @xtabela_zglt_dre_02_custeio-bukrs
         AND gjahr =  @xtabela_zglt_dre_02_custeio-gjahr
         AND poper =  @xtabela_zglt_dre_02_custeio-poper
         AND saknr =  @xtabela_zglt_dre_02_custeio-saknr
         AND kostl =  @xtabela_zglt_dre_02_custeio-kostl
         AND prctr =  @xtabela_zglt_dre_02_custeio-prctr
         AND aufnr =  @xtabela_zglt_dre_02_custeio-aufnr
         AND matkl =  @xtabela_zglt_dre_02_custeio-matkl.

    IF sy-subrc = 0.

      SORT xtabela_zglt_dre_02_custeio by  bukrs gjahr poper saknr kostl aufnr matkl prctr.

      LOOP AT it_zglt_dre_04_vazio ASSIGNING FIELD-SYMBOL(<fs_zglt_dre_04_vazio>).

        READ TABLE xtabela_zglt_dre_02_custeio INTO DATA(wa_xtabela_zglt_dre_02_custeio) WITH KEY bukrs  = <fs_zglt_dre_04_vazio>-bukrs
                                                                                                  gjahr =  <fs_zglt_dre_04_vazio>-gjahr
                                                                                                  poper =  <fs_zglt_dre_04_vazio>-poper
                                                                                                  saknr =  <fs_zglt_dre_04_vazio>-saknr
                                                                                                  kostl =  <fs_zglt_dre_04_vazio>-kostl
                                                                                                  aufnr =  <fs_zglt_dre_04_vazio>-aufnr
                                                                                                  matkl =  <fs_zglt_dre_04_vazio>-matkl
                                                                                                  prctr =  <fs_zglt_dre_04_vazio>-prctr BINARY SEARCH.

        IF sy-subrc = 0.
          <fs_zglt_dre_04_vazio>-tp_conta =  wa_xtabela_zglt_dre_02_custeio-tp_conta.
          <fs_zglt_dre_04_vazio>-nivel    =  wa_xtabela_zglt_dre_02_custeio-nivel.
        ENDIF.
      ENDLOOP.

      MODIFY zglt_dre_04 from TABLE it_zglt_dre_04_vazio.
    ENDIF.
  ENDIF.

  "Total de Grupo de Mercadoria """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  DELETE it_zgl021_dre_dados WHERE vlr_rea EQ 0 AND vlr_dolar EQ 0 AND vlr_dolar_conv EQ 0 AND vlr_grupo EQ 0.
  DELETE it_zgl022_dre_dados WHERE vlr_rea EQ 0 AND vlr_dolar EQ 0 AND vlr_dolar_conv EQ 0 AND vlr_grupo EQ 0.
  DELETE it_zgl023_dre_dados WHERE vlr_rea EQ 0 AND vlr_dolar EQ 0 AND vlr_dolar_conv EQ 0 AND vlr_grupo EQ 0.
  DELETE it_zgl024_dre_dados WHERE vlr_rea EQ 0 AND vlr_dolar EQ 0 AND vlr_dolar_conv EQ 0 AND vlr_grupo EQ 0.

  IF NOT it_zgl021_dre_dados[] IS INITIAL.
    MODIFY zgl021_dre_dados FROM TABLE it_zgl021_dre_dados.
  ENDIF.

  IF NOT it_zgl022_dre_dados[] IS INITIAL.
    MODIFY zgl022_dre_dados FROM TABLE it_zgl022_dre_dados.
  ENDIF.

  IF NOT it_zgl023_dre_dados[] IS INITIAL.
    MODIFY zgl023_dre_dados FROM TABLE it_zgl023_dre_dados.
  ENDIF.

  IF NOT it_zgl024_dre_dados[] IS INITIAL.
    MODIFY zgl024_dre_dados FROM TABLE it_zgl024_dre_dados.
  ENDIF.

  DELETE it_zgl029_dre_dados WHERE vlr_rea    EQ 0
                               AND vlr_dolar  EQ 0
                               AND vlr_grupo  EQ 0.

  IF it_zgl029_dre_dados[] IS NOT INITIAL.
    MODIFY zgl029_dre_dados FROM TABLE it_zgl029_dre_dados.
  ENDIF.

  CALL FUNCTION 'Z_01_DRE_TOTALIZA_MES'
    EXPORTING
      pbukrs = pbukrs
      pgjahr = pgjahr
      ppoper = ppoper
      pversn = pversn.

ENDFUNCTION.
