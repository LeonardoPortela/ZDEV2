FUNCTION z_fi_gl_saldo_bsix.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_PARCEIRO) TYPE  LIFNR OPTIONAL
*"     REFERENCE(P_DT_INICIAL) TYPE  BUDAT DEFAULT '19990101'
*"     REFERENCE(P_DT_POSICAO) TYPE  BUDAT DEFAULT SY-DATUM
*"     REFERENCE(CONTAS) TYPE  ZCT_EMP_CONTAS OPTIONAL
*"     REFERENCE(WAERS) TYPE  WAERS OPTIONAL
*"     REFERENCE(P_GERAR_SOC_PARCEIRA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_DT_EQ) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_VERIFICA_RZ_ESP) TYPE  CHAR01 DEFAULT SPACE
*"  TABLES
*"      IT_BKPF STRUCTURE  BKPF OPTIONAL
*"      IT_PARTIDAS STRUCTURE  ZDE_FI_GL_PARTIDAS_CLI_FOR OPTIONAL
*"      IT_BSXS STRUCTURE  BSIS OPTIONAL
*"      IT_BSXK STRUCTURE  BSIK OPTIONAL
*"      IT_BSXD STRUCTURE  BSID OPTIONAL
*"      IT_SALDO_CONTAS STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"      IT_SALDO_PARCEIRO STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"      IT_SALDO_CTA_PARC STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT2
*"       OPTIONAL
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2BSH |13/12/2024 |Melhoria de performance de     &*
*&                                    |processamento de dados.        &*
*&                                    |Chamado: 158518 TCODE = ZGL042 &*
*&--------------------------------------------------------------------&*
  DATA: it_contas   TYPE zct_emp_contas,
        wa_conta    TYPE zlc_emp_contas,
        fl_moeda(1),
        vg_ktopl    TYPE ktopl,
        it_t074     TYPE TABLE OF t074 WITH HEADER LINE,
        it_t001     TYPE TABLE OF t001 WITH HEADER LINE,
        it_moedas   TYPE TABLE OF x001 WITH HEADER LINE,
        it_skb1     TYPE TABLE OF skb1 WITH HEADER LINE,
        it_bsis     TYPE TABLE OF bsis WITH HEADER LINE, "Documentos em Aberto
        it_bsas     TYPE TABLE OF bsas WITH HEADER LINE, "Documentos em Compensados
        it_bsik     TYPE TABLE OF bsik WITH HEADER LINE, "Partidas em Aberto de Forncedor
        it_bsak     TYPE TABLE OF bsak WITH HEADER LINE, "Partidas Fechadas de Forncedor
        it_bsid     TYPE TABLE OF bsid WITH HEADER LINE, "Partidas em Aberto de Cliente
        it_bsad     TYPE TABLE OF bsad WITH HEADER LINE, "Partidas em Aberto de Cliente
        it_bsik_aux TYPE TABLE OF bsik WITH HEADER LINE, "Partidas em Aberto de Forncedor
        it_bsak_aux TYPE TABLE OF bsak WITH HEADER LINE, "Partidas Fechadas de Forncedor
        it_bsid_aux TYPE TABLE OF bsid WITH HEADER LINE, "Partidas em Aberto de Cliente
        it_bsad_aux TYPE TABLE OF bsad WITH HEADER LINE. "Partidas Fechadas de Cliente.

*---> 28/08/2023 - Ticket MG-6425 - GT - Início
  DATA: lr_saknr TYPE RANGE OF saknr.
*<--- 28/08/2023 - Ticket MG-6425 - GT - Fim

*  RANGES: RBUDAT FOR RANGE_DATE.
*
*  IF P_DT_INICIAL EQ P_DT_POSICAO.
*    RBUDAT-SIGN   = 'I'.
*    RBUDAT-OPTION = 'EQ'.
*    RBUDAT-LOW    = P_DT_POSICAO.
*    RBUDAT-HIGH   = P_DT_POSICAO..
*    APPEND RBUDAT.
*  ENDIF..

  DATA: it_saldo_fagl TYPE HASHED TABLE OF zde_fi_gl_saldo_faglflext  WITH UNIQUE KEY rclnt ryear rldnr rbukrs rbusa racct rassc WITH HEADER LINE.
  DATA: it_saldo_cp   TYPE HASHED TABLE OF zde_fi_gl_saldo_faglflext2 WITH UNIQUE KEY rclnt ryear rldnr rbukrs racct parid rassc WITH HEADER LINE.

  RANGES: p_empresas FOR t001-bukrs,
          p_contas   FOR skb1-saknr,
          p_parceirr FOR bsik-lifnr.

  IF it_bkpf[] IS NOT INITIAL.

    CALL FUNCTION 'Z_FI_GL_PARTIDAS'
      EXPORTING
        i_contas    = contas
      TABLES
        it_bkpf     = it_bkpf
        it_partidas = it_partidas.

    CHECK 1 = 2.

  ENDIF.

  IF p_verifica_rz_esp EQ abap_true.

    LOOP AT contas INTO wa_conta.

      CLEAR: it_t074[].

      SELECT SINGLE ktopl INTO vg_ktopl
        FROM t001
       WHERE bukrs EQ wa_conta-bukrs.

      SELECT * INTO TABLE it_t074
        FROM t074
       WHERE ktopl EQ vg_ktopl
         AND skont EQ wa_conta-saknr.

      IF sy-subrc IS INITIAL.
        LOOP AT it_t074.
          wa_conta-umskz = it_t074-umskz.
          APPEND wa_conta TO it_contas.
        ENDLOOP.

        CLEAR: it_t074[].

        SELECT * INTO TABLE it_t074
          FROM t074
         WHERE ktopl EQ vg_ktopl
           AND hkont EQ wa_conta-saknr.

        IF sy-subrc IS INITIAL.
          CLEAR: wa_conta-umskz.
          APPEND wa_conta TO it_contas.
        ENDIF.

      ELSE.
        APPEND wa_conta TO it_contas.
      ENDIF.
    ENDLOOP.

  ELSE.
    MOVE contas TO it_contas.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM it_contas COMPARING bukrs saknr umskz.

  IF p_parceiro IS NOT INITIAL.
    p_parceirr-sign   = 'I'.
    p_parceirr-option = 'EQ'.
    p_parceirr-low    = p_parceiro.
    p_parceirr-high   = p_parceiro.
    APPEND p_parceirr.
  ENDIF.

  LOOP AT it_contas INTO wa_conta.
    p_empresas-sign   = 'I'.
    p_empresas-option = 'EQ'.
    p_empresas-low    = wa_conta-bukrs.
    p_empresas-high   = wa_conta-bukrs.
    APPEND p_empresas.
  ENDLOOP.

  LOOP AT it_contas INTO wa_conta.
    IF wa_conta-saknr NE '*'.
      p_contas-sign   = 'I'.
      p_contas-option = 'EQ'.
      p_contas-low    = wa_conta-saknr.
      p_contas-high   = wa_conta-saknr.
      APPEND p_contas.
    ELSEIF wa_conta-saknr CS '*'.
      p_contas-sign   = 'I'.
      p_contas-option = 'CP'.
      p_contas-low    = wa_conta-saknr.
      p_contas-high   = wa_conta-saknr.
      APPEND p_contas.
    ENDIF.
  ENDLOOP.

  "Deixar somentes contas solicitadas com razão especial.
  "DELETE IT_CONTAS WHERE UMSKZ EQ SPACE.

  SELECT *
    FROM t001
    INTO TABLE it_t001
   WHERE bukrs IN p_empresas.

  IF ( sy-subrc IS INITIAL ).
    LOOP AT it_t001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs = it_t001-bukrs
        IMPORTING
          e_x001  = it_moedas.
      APPEND it_moedas.
    ENDLOOP.
  ENDIF.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2431747]
    INTO TABLE it_skb1
    FROM skb1
   WHERE bukrs IN p_empresas
     AND saknr IN p_contas.

  IF it_skb1[] IS NOT INITIAL.
*---> 28/08/2023 - Ticket MG-6425 - GT - Início
*    SELECT *
*      FROM bsis_bck AS a
*      INNER JOIN faglflexa AS b ON ( b~rbukrs = a~bukrs
*                                 AND b~belnr  = a~belnr
*                                 AND b~gjahr  = a~gjahr
*                                 AND b~buzei  = a~buzei
*                                 AND b~rclnt  = a~mandt )
*      INTO CORRESPONDING FIELDS OF TABLE it_bsis
*       FOR ALL ENTRIES IN it_skb1
*     WHERE a~bukrs EQ it_skb1-bukrs
*       AND a~hkont EQ it_skb1-saknr
*       AND a~budat GE p_dt_inicial
*       AND a~budat LE p_dt_posicao
*       AND b~budat GE p_dt_inicial
*       AND b~budat LE p_dt_posicao
**       AND B~BUDAT IN RBUDAT
*       AND b~rldnr LE '0L'
*       AND b~racct EQ it_skb1-saknr
*       AND ( b~bstat EQ ' ' OR b~bstat EQ 'L' ).
*    "Documento normal                   (' ')
*    "Lançamento em ledger não principal ('L')

    " Foi necessário substituir a seleção da tabela BSIS_BCK pela view BSIS_VIEW, pois não estava retornando valores. Porém, ao trocar pela view
    " o SELECT apresentou DUMP de mémoria devido a perda de performance. Para resolver o problema, foi desmenbrado os SELECTs das tabelas
    lr_saknr = VALUE #( FOR ls_skb1 IN it_skb1 ( sign = 'I' option = 'EQ' low = ls_skb1-saknr ) ).

    SELECT *
      FROM bsis_view
      INTO CORRESPONDING FIELDS OF TABLE @it_bsis
       FOR ALL ENTRIES IN @it_skb1
     WHERE bukrs EQ @it_skb1-bukrs
       AND hkont EQ @it_skb1-saknr
       AND ( budat GE @p_dt_inicial AND budat LE @p_dt_posicao ).

    IF sy-subrc EQ 0.
      SELECT *
        FROM faglflexa
        INTO TABLE @DATA(lt_faglflexa)
         FOR ALL ENTRIES IN @it_bsis
       WHERE rbukrs EQ @it_bsis-bukrs
         AND belnr  EQ @it_bsis-belnr
         AND gjahr  EQ @it_bsis-gjahr
         AND buzei  EQ @it_bsis-buzei
         AND rldnr  LE '0L'
         AND racct  IN @lr_saknr
         AND ( bstat EQ ' ' OR bstat EQ 'L' )
         AND ( budat GE @p_dt_inicial AND budat LE @p_dt_posicao ).

      IF sy-subrc EQ 0.
**<<<------"158518 - NMS - INI------>>>
*        LOOP AT it_bsis INTO DATA(ls_bsis).
*          DATA(lv_index) = sy-tabix.
*
*          IF NOT line_exists( lt_faglflexa[ rbukrs = ls_bsis-bukrs belnr = ls_bsis-belnr gjahr = ls_bsis-gjahr buzei = ls_bsis-buzei ] ).
*            DELETE it_bsis INDEX lv_index.
*          ENDIF.
*        ENDLOOP.
* O procesamento do LOOP era para deixar as duas TIs com os mesmos documento conforme a chave de validação de existência. Porém,
* gerou uma lentidão nop processamento. Como a promeira seleção da BSIS_VIEW não foi muito demorada, foi refeita a consulta com
* os campos chaves da FAGLFLEXA na BSIS_VIEW onde a consulta é mais rápida ainda.
        SELECT *
          FROM bsis_view
          INTO CORRESPONDING FIELDS OF TABLE @it_bsis
           FOR ALL ENTRIES IN @lt_faglflexa
         WHERE bukrs EQ @lt_faglflexa-rbukrs
           AND belnr EQ @lt_faglflexa-belnr
           AND gjahr EQ @lt_faglflexa-gjahr
           AND buzei EQ @lt_faglflexa-buzei.
**<<<------"158518 - NMS - FIM------>>>
      ENDIF.
    ENDIF.
*<--- 28/08/2023 - Ticket MG-6425 - GT - Fim

    SORT it_bsis BY bukrs hkont augdt augbl zuonr gjahr belnr buzei.
    DELETE ADJACENT DUPLICATES FROM it_bsis COMPARING bukrs hkont augdt augbl zuonr gjahr belnr buzei.

    IF p_dt_eq IS NOT INITIAL.
      DELETE it_bsis WHERE budat NE p_dt_posicao.
    ENDIF.

    IF it_bsis[] IS NOT INITIAL.

      SELECT *
        INTO TABLE it_bsik
        FROM bsik
         FOR ALL ENTRIES IN it_bsis
       WHERE bukrs EQ it_bsis-bukrs
         AND hkont EQ it_bsis-hkont
         AND gjahr EQ it_bsis-gjahr
         AND belnr EQ it_bsis-belnr
         AND buzei EQ it_bsis-buzei
         "AND UMSKS EQ SPACE
         AND lifnr IN p_parceirr.

      IF it_contas IS NOT INITIAL.
        CLEAR: it_bsik_aux[].
        LOOP AT it_contas INTO wa_conta.
          IF wa_conta-saknr EQ '*'.
            LOOP AT it_bsik WHERE bukrs EQ wa_conta-bukrs
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsik TO it_bsik_aux.
            ENDLOOP.
          ELSE.
            LOOP AT it_bsik WHERE bukrs EQ wa_conta-bukrs
                              AND hkont EQ wa_conta-saknr
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsik TO it_bsik_aux.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
        CLEAR: it_bsik[].
        MOVE it_bsik_aux[] TO it_bsik[].
      ELSE.
        DELETE it_bsik WHERE umsks NE space.
      ENDIF.

      IF p_parceirr IS NOT INITIAL.
        LOOP AT it_bsis.
          READ TABLE it_bsik WITH KEY bukrs = it_bsis-bukrs
                                      hkont = it_bsis-hkont
                                      gjahr = it_bsis-gjahr
                                      belnr = it_bsis-belnr
                                      buzei = it_bsis-buzei.
          IF sy-subrc IS INITIAL.
            APPEND it_bsis TO it_bsxs.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SELECT *
        INTO TABLE it_bsid
        FROM bsid
         FOR ALL ENTRIES IN it_bsis
       WHERE bukrs EQ it_bsis-bukrs
         AND hkont EQ it_bsis-hkont
         AND gjahr EQ it_bsis-gjahr
         AND belnr EQ it_bsis-belnr
         AND buzei EQ it_bsis-buzei
         "AND UMSKS EQ SPACE
         AND ( bstat EQ '' OR bstat EQ 'L' )               "Stefanini FGM 31/07/2025 - IR251212
         AND kunnr IN p_parceirr.

      IF it_contas IS NOT INITIAL.
        CLEAR: it_bsid_aux[].
        LOOP AT it_contas INTO wa_conta.
          IF wa_conta-saknr EQ '*'.
            LOOP AT it_bsid WHERE bukrs EQ wa_conta-bukrs
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsid TO it_bsid_aux.
            ENDLOOP.
          ELSE.
            LOOP AT it_bsid WHERE bukrs EQ wa_conta-bukrs
                              AND hkont EQ wa_conta-saknr
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsid TO it_bsid_aux.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
        CLEAR: it_bsid[].
        MOVE it_bsid_aux[] TO it_bsid[].
      ELSE.
        DELETE it_bsid WHERE umsks NE space.
      ENDIF.

      IF p_parceirr IS NOT INITIAL.
        LOOP AT it_bsis.
          READ TABLE it_bsid WITH KEY bukrs = it_bsis-bukrs
                                      hkont = it_bsis-hkont
                                      gjahr = it_bsis-gjahr
                                      belnr = it_bsis-belnr
                                      buzei = it_bsis-buzei.
          IF sy-subrc IS INITIAL.
            APPEND it_bsis TO it_bsxs.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    SELECT *
      FROM bsas AS a
      INNER JOIN faglflexa AS b ON ( b~rbukrs = a~bukrs
                                 AND b~belnr  = a~belnr
                                 AND b~gjahr  = a~gjahr
                                 AND b~buzei  = a~buzei
                                 AND b~rclnt  = a~mandt )
      INTO CORRESPONDING FIELDS OF TABLE it_bsas
       FOR ALL ENTRIES IN it_skb1
     WHERE a~bukrs EQ it_skb1-bukrs
       AND a~hkont EQ it_skb1-saknr
       AND a~augdt GT p_dt_posicao
       AND b~budat GE p_dt_inicial
       AND b~budat LE p_dt_posicao
*       AND B~BUDAT IN RBUDAT
       AND b~rldnr LE '0L'
       AND b~racct EQ it_skb1-saknr
       AND ( b~bstat EQ ' ' OR b~bstat EQ 'L' ).

    IF p_dt_eq IS NOT INITIAL.
      DELETE it_bsas WHERE augdt NE p_dt_posicao.
    ENDIF.

    IF it_bsas[] IS NOT INITIAL.

      SELECT *
        INTO TABLE it_bsak
        FROM bsak
         FOR ALL ENTRIES IN it_bsas
       WHERE bukrs EQ it_bsas-bukrs
         AND hkont EQ it_bsas-hkont
         AND gjahr EQ it_bsas-gjahr
         AND belnr EQ it_bsas-belnr
         AND buzei EQ it_bsas-buzei
         AND lifnr IN p_parceirr.

      IF it_contas IS NOT INITIAL.
        CLEAR: it_bsak_aux[].
        LOOP AT it_contas INTO wa_conta.
          IF wa_conta-saknr EQ '*'.
            LOOP AT it_bsak WHERE bukrs EQ wa_conta-bukrs
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsak TO it_bsak_aux.
            ENDLOOP.
          ELSE.
            LOOP AT it_bsak WHERE bukrs EQ wa_conta-bukrs
                              AND hkont EQ wa_conta-saknr
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsak TO it_bsak_aux.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
        CLEAR: it_bsak[].
        MOVE it_bsak_aux[] TO it_bsak[].
      ELSE.
        DELETE it_bsak WHERE umsks NE space.
      ENDIF.

      IF p_parceirr IS NOT INITIAL.
        LOOP AT it_bsas.
          READ TABLE it_bsak WITH KEY bukrs = it_bsas-bukrs
                                      hkont = it_bsas-hkont
                                      gjahr = it_bsas-gjahr
                                      belnr = it_bsas-belnr
                                      buzei = it_bsas-buzei.
          IF sy-subrc IS INITIAL.
            APPEND it_bsas TO it_bsxs.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SELECT *
        INTO TABLE it_bsad
        FROM bsad
         FOR ALL ENTRIES IN it_bsas
       WHERE bukrs EQ it_bsas-bukrs
         AND hkont EQ it_bsas-hkont
         AND gjahr EQ it_bsas-gjahr
         AND belnr EQ it_bsas-belnr
         AND buzei EQ it_bsas-buzei
         AND kunnr IN p_parceirr.

      IF it_contas IS NOT INITIAL.
        CLEAR: it_bsad_aux[].
        LOOP AT it_contas INTO wa_conta.
          IF wa_conta-saknr EQ '*'.
            LOOP AT it_bsad WHERE bukrs EQ wa_conta-bukrs
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsad TO it_bsad_aux.
            ENDLOOP.
          ELSE.
            LOOP AT it_bsad WHERE bukrs EQ wa_conta-bukrs
                              AND hkont EQ wa_conta-saknr
                              AND umskz EQ wa_conta-umskz.
              APPEND it_bsad TO it_bsad_aux.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
        CLEAR: it_bsad[].
        MOVE it_bsad_aux[] TO it_bsad[].
      ELSE.
        DELETE it_bsad WHERE umsks NE space.
      ENDIF.

      IF p_parceirr IS NOT INITIAL.
        LOOP AT it_bsas.
          READ TABLE it_bsad WITH KEY bukrs = it_bsas-bukrs
                                      hkont = it_bsas-hkont
                                      gjahr = it_bsas-gjahr
                                      belnr = it_bsas-belnr
                                      buzei = it_bsas-buzei.
          IF sy-subrc IS INITIAL.
            APPEND it_bsas TO it_bsxs.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    MOVE it_bsik[] TO it_bsxk[].
    MOVE it_bsid[] TO it_bsxd[].

    IF p_parceirr IS INITIAL.
      MOVE it_bsis[] TO it_bsxs[].

      LOOP AT it_bsas.
        APPEND it_bsas TO it_bsxs.
      ENDLOOP.
    ENDIF.

    LOOP AT it_bsak.
      APPEND it_bsak TO it_bsxk.
    ENDLOOP.

    LOOP AT it_bsad.
      APPEND it_bsad TO it_bsxd.
    ENDLOOP.

    SORT it_bsxs BY gjahr belnr buzei.
    SORT it_bsxk BY gjahr belnr buzei.
    SORT it_bsxd BY gjahr belnr buzei.

    LOOP AT it_bsxs.
      CLEAR: it_saldo_fagl.
      it_saldo_fagl-rclnt  = it_bsxs-mandt.
      it_saldo_fagl-rbukrs = it_bsxs-bukrs.
      it_saldo_fagl-racct  = it_bsxs-hkont.
      it_saldo_fagl-rldnr  = it_bsxs-shkzg.

      IF p_gerar_soc_parceira IS NOT INITIAL.
        it_saldo_fagl-rassc = it_bsxs-vbund.
      ENDIF.

      IF waers IS INITIAL.
        it_saldo_fagl-slvt = it_bsxs-dmbtr.
      ELSE.
        READ TABLE it_t001 WITH KEY bukrs = it_saldo_fagl-rbukrs.
        IF it_t001-waers EQ waers.
          it_saldo_fagl-slvt = it_bsxs-dmbtr.
        ELSE.
          READ TABLE it_moedas WITH KEY bukrs = it_saldo_fagl-rbukrs.
          IF sy-subrc IS INITIAL.
            IF it_moedas-hwae2 EQ  waers.
              it_saldo_fagl-slvt = it_bsxs-dmbe2.
            ELSEIF it_moedas-hwae3 EQ waers.
              it_saldo_fagl-slvt = it_bsxs-dmbe3.
            ELSEIF waers EQ it_bsxs-waers.
              it_saldo_fagl-slvt = it_bsxs-wrbtr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      COLLECT it_saldo_fagl.

      """" Saldo Parceiro e Conta Razão """"""""""""""""""""""""""""""""""""""""""""
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_saldo_cp.
      it_saldo_cp-rclnt  = it_saldo_fagl-rclnt.
      it_saldo_cp-rbukrs = it_saldo_fagl-rbukrs.
      it_saldo_cp-racct  = it_saldo_fagl-racct.
      it_saldo_cp-rldnr  = it_saldo_fagl-rldnr.
      it_saldo_cp-rassc  = it_saldo_fagl-rassc.
      it_saldo_cp-slvt   = it_saldo_fagl-slvt.

      "FORNECEDOR
      READ TABLE it_bsxk WITH KEY gjahr = it_bsxs-gjahr
                                  belnr = it_bsxs-belnr
                                  buzei = it_bsxs-buzei.
      IF sy-subrc IS INITIAL.
        it_saldo_cp-parid = it_bsxk-lifnr.
        COLLECT it_saldo_cp.
      ENDIF.

      "CLIENTE
      READ TABLE it_bsxd WITH KEY gjahr = it_bsxs-gjahr
                                  belnr = it_bsxs-belnr
                                  buzei = it_bsxs-buzei.
      IF sy-subrc IS INITIAL.
        it_saldo_cp-parid = it_bsxd-kunnr.
        COLLECT it_saldo_cp.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDLOOP.

    MOVE it_saldo_fagl[] TO it_saldo_contas[].
    MOVE it_saldo_cp[]   TO it_saldo_cta_parc[].
    CLEAR it_saldo_fagl[].

    LOOP AT it_bsxk.
      CLEAR: it_saldo_fagl.
      it_saldo_fagl-rclnt  = it_bsxk-mandt.
      it_saldo_fagl-rbukrs = it_bsxk-bukrs.
      it_saldo_fagl-racct  = it_bsxk-lifnr.
      it_saldo_fagl-rldnr  = it_bsxk-shkzg.

      IF waers IS INITIAL.
        it_saldo_fagl-slvt = it_bsxk-dmbtr.
      ELSE.
        READ TABLE it_t001 WITH KEY bukrs = it_saldo_fagl-rbukrs.
        IF it_t001-waers EQ waers.
          it_saldo_fagl-slvt = it_bsxk-dmbtr.
        ELSE.
          READ TABLE it_moedas WITH KEY bukrs = it_saldo_fagl-rbukrs.
          IF sy-subrc IS INITIAL.
            IF it_moedas-hwae2 EQ waers.
              it_saldo_fagl-slvt = it_bsxk-dmbe2.
            ELSEIF it_moedas-hwae3 EQ waers.
              it_saldo_fagl-slvt = it_bsxk-dmbe3.
            ELSEIF waers EQ it_bsxk-waers.
              it_saldo_fagl-slvt = it_bsxk-wrbtr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF p_gerar_soc_parceira IS NOT INITIAL.
        READ TABLE it_bsxs WITH KEY gjahr = it_bsxk-gjahr
                                    belnr = it_bsxk-belnr
                                    buzei = it_bsxk-buzei.
        IF sy-subrc IS INITIAL.
          it_saldo_fagl-rassc = it_bsxs-vbund.
        ENDIF.
      ENDIF.

      COLLECT it_saldo_fagl.
    ENDLOOP.

    LOOP AT it_bsxd.
      CLEAR: it_saldo_fagl.
      it_saldo_fagl-rclnt  = it_bsxd-mandt.
      it_saldo_fagl-rbukrs = it_bsxd-bukrs.
      it_saldo_fagl-racct  = it_bsxd-kunnr.
      it_saldo_fagl-rldnr  = it_bsxd-shkzg.
      it_saldo_fagl-slvt   = it_bsxd-dmbtr.

      IF waers IS INITIAL.
        it_saldo_fagl-slvt = it_bsxd-dmbtr.
      ELSE.
        READ TABLE it_t001 WITH KEY bukrs = it_saldo_fagl-rbukrs.
        IF it_t001-waers EQ waers.
          it_saldo_fagl-slvt = it_bsxd-dmbtr.
        ELSE.
          READ TABLE it_moedas WITH KEY bukrs = it_saldo_fagl-rbukrs.
          IF sy-subrc IS INITIAL.
            IF it_moedas-hwae2 EQ waers.
              it_saldo_fagl-slvt = it_bsxd-dmbe2.
            ELSEIF it_moedas-hwae3 EQ waers.
              it_saldo_fagl-slvt = it_bsxd-dmbe3.
            ELSEIF waers EQ it_bsxd-waers.
              it_saldo_fagl-slvt = it_bsxd-wrbtr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF p_gerar_soc_parceira IS NOT INITIAL.
        READ TABLE it_bsxs WITH KEY gjahr = it_bsxd-gjahr
                                    belnr = it_bsxd-belnr
                                    buzei = it_bsxd-buzei.
        IF sy-subrc IS INITIAL.
          it_saldo_fagl-rassc = it_bsxs-vbund.
        ENDIF.
      ENDIF.

      COLLECT it_saldo_fagl.
    ENDLOOP.

    MOVE it_saldo_fagl[] TO it_saldo_parceiro[].

    IF p_parceiro IS NOT INITIAL.
      DELETE it_saldo_parceiro WHERE racct NE p_parceiro.
      DELETE it_saldo_cta_parc WHERE parid NE p_parceiro.
    ENDIF.

  ENDIF.

ENDFUNCTION.
