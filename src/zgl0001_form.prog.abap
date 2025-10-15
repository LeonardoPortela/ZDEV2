
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE user_command_0200 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE user_command_0300 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT lo_report1.
  lo_report1->get_data( ).
  lo_report1->generate_output( ).

ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'T0200'.
  CREATE OBJECT lo_report2.
  lo_report2->get_data( ).
  lo_report2->generate_output( ).
ENDMODULE.

MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
  SET TITLEBAR 'T0300'.
  CREATE OBJECT lo_report3.
  lo_report3->get_data( ).
  lo_report3->generate_output( ).
ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'REFRESHG3'.
      lo_report3->get_data( ).
      lo_report3->o_alv->refresh( ).
  ENDCASE.
ENDFORM.

FORM get_data_start.
  TYPES: BEGIN OF ty_vbfa_aux,
           vbeln   TYPE  vbfa-vbeln,
           posnn   TYPE  vbfa-posnn,
           vbtyp_n TYPE  vbfa-vbtyp_n,
           vbelv   TYPE  vbfa-vbelv,
           sonum   TYPE vbfa-sonum,
         END OF ty_vbfa_aux.

  DATA lt_vbfa_aux TYPE TABLE OF ty_vbfa_aux.

  DATA: git_saidan TYPE TABLE OF ty_saida1,
        gwa_saidan TYPE ty_saida1.

  DATA: it_1bnflinx TYPE TABLE OF j_1bnflin,
        wa_1bnflinx TYPE j_1bnflin.

*Buscar documentos da ZCONF:
  SELECT * FROM j_1bnfdoc
  INTO TABLE @DATA(it_1bnfdoc_me)
        WHERE bukrs  IN @p_bukrs
        AND pstdat IN @p_budat
        AND docnum IN @p_docnum
        AND cancel NE @abap_true.

  IF it_1bnfdoc_me[] IS NOT INITIAL.
    SELECT docnum, cfop, matkl, matnr, refkey, refitm, itmnum, werks, nfnet, menge FROM j_1bnflin
    INTO TABLE @DATA(it_1bnflin)
          FOR ALL ENTRIES IN @it_1bnfdoc_me
          WHERE docnum EQ @it_1bnfdoc_me-docnum
          AND cfop IN ( SELECT cfop FROM zglt0001 WHERE tp_m EQ 'ME' ).

    IF sy-subrc IS INITIAL.

      DATA(it_1bnflin_me) = it_1bnflin[].

      SORT it_1bnflin_me BY docnum.
      DELETE ADJACENT DUPLICATES FROM it_1bnflin_me COMPARING docnum.

      DATA: lr_docnum TYPE RANGE OF j_1bnflin-docnum.

      IF it_1bnflin_me IS NOT INITIAL.
        LOOP AT it_1bnflin_me ASSIGNING FIELD-SYMBOL(<fs_lin_me>).
          lr_docnum[] = VALUE #( FOR wa_doc IN it_1bnflin_me ( option = 'EQ' sign = 'I' low = wa_doc-docnum ) ).
        ENDLOOP.
      ENDIF.

      DELETE it_1bnfdoc_me WHERE docnum NOT IN lr_docnum[].

      IF it_1bnfdoc_me IS NOT INITIAL.
        DATA(it_1bnfdoc) = it_1bnfdoc_me[].
      ENDIF.

    ENDIF.

  ENDIF.
*Buscar documentos da ZCONF:
  SELECT * FROM j_1bnfdoc
  APPENDING TABLE @it_1bnfdoc
  WHERE bukrs  IN @p_bukrs
  AND pstdat IN @p_budat
  AND docnum IN @p_docnum
  AND doctyp NE '5'
  AND cancel NE @abap_true
  AND inco1  NE 'FOB'. "NOT IN ('FOB', 'CFR').

*  IF sy-subrc IS INITIAL.
  IF it_1bnfdoc IS NOT INITIAL.
    SORT it_1bnfdoc BY docnum.

    DATA: lr_parid TYPE RANGE OF j_1bnfdoc-parid.

    DATA(it_parid) = it_1bnfdoc.

    SORT it_parid BY parid ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_parid COMPARING parid.

    lr_parid[] = VALUE #( FOR wa_parid IN it_parid ( option = 'EQ' sign = 'I' low = wa_parid-parid ) ).

    IF lr_parid[] IS NOT INITIAL.
      SORT lr_parid BY low.
      DELETE ADJACENT DUPLICATES FROM lr_parid COMPARING low.
    ENDIF.
* Filtrar documentos por CFOP:
    IF it_1bnfdoc[] IS NOT INITIAL.
      SELECT docnum, cfop, matkl, matnr, refkey, refitm, itmnum, werks, nfnet, menge FROM j_1bnflin
      APPENDING TABLE @it_1bnflin
      FOR ALL ENTRIES IN @it_1bnfdoc
      WHERE docnum EQ @it_1bnfdoc-docnum
      AND cfop IN ( SELECT cfop FROM zglt0001 ).

      IF it_1bnflin IS NOT INITIAL.
        SORT it_1bnflin BY docnum cfop matkl matnr refkey refitm itmnum werks nfnet menge.
        DELETE ADJACENT DUPLICATES FROM it_1bnflin COMPARING ALL FIELDS.
        SORT it_1bnflin BY docnum.
      ENDIF.

    ENDIF.
*Buscar dados de cadastro na BKPF:
    IF it_1bnflin[] IS NOT INITIAL.
      SELECT * FROM bkpf
      INTO TABLE @DATA(it_bkpf)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE bukrs IN @p_bukrs
            AND awkey EQ @it_1bnflin-refkey(20).

      IF it_bkpf IS NOT INITIAL.
        SORT it_bkpf BY bukrs awkey.

        SELECT bukrs, belnr, kunnr, xref3, vbel2, vbeln FROM bseg
        INTO TABLE @DATA(it_bseg)
              FOR ALL ENTRIES IN @it_bkpf
              WHERE bukrs EQ @it_bkpf-bukrs
              AND belnr EQ @it_bkpf-belnr
              AND kunnr IN @lr_parid.

        IF sy-subrc IS INITIAL.
          SORT it_bseg BY bukrs belnr kunnr.
        ENDIF.
      ENDIF.

    ENDIF.
*Buscar dados de cadastro na ADOCA:
    IF it_bkpf[] IS NOT INITIAL.
* Busca dados na CDS da tabela Diário Universal contra Cliente
      SELECT
      bukrs, gjahr, belnr, rassc, hsl, tsl, racct
      FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf
      WHERE rldnr  EQ '0L'
      AND bukrs IN @p_bukrs
      AND ( rassc  IN ( SELECT tipo FROM zglt0002 ) OR ( rassc  EQ @space ) )
      AND gjahr  EQ @it_bkpf-gjahr
      AND belnr  EQ @it_bkpf-belnr
      AND koart  EQ 'D'                "Cliente
      INTO TABLE @DATA(it_acdoca).
**<<<------"188565 - NMS - INI------>>>
* Busca dados na CDS da tabela Diário Universal contra Conta Razão
      SELECT belnr, gjahr, vbeln, fkart
        FROM vbrk
        INTO TABLE @DATA(tl_bkpf_vbrk)
        FOR ALL ENTRIES IN @it_bkpf
      WHERE fkart EQ 'ZRFU'            "Fat.forn.futuro BR
        AND belnr EQ @it_bkpf-belnr
        AND gjahr EQ @it_bkpf-gjahr.

      IF NOT tl_bkpf_vbrk[] IS INITIAL.
        SELECT bukrs, gjahr, belnr, rassc, hsl, tsl, racct
        FROM zi_zgl0001_03
        FOR ALL ENTRIES IN @tl_bkpf_vbrk
        WHERE rldnr  EQ '0L'
          AND bukrs  IN @p_bukrs
          AND ( rassc  IN ( SELECT tipo FROM zglt0002 ) OR ( rassc  EQ @space ) )
          AND gjahr  EQ @tl_bkpf_vbrk-gjahr
          AND belnr  EQ @tl_bkpf_vbrk-belnr
          AND koart  EQ 'S'            "Contas do Razão
          AND racct  EQ '0000214900'   "Venda mercadoria para entrega futura
          APPENDING TABLE @it_acdoca.

      ENDIF.
**<<<------"188565 - NMS - FIM------>>>
      IF it_acdoca[] IS NOT INITIAL.
        SORT it_acdoca BY bukrs gjahr belnr.

        SELECT * FROM zglt0003
        INTO TABLE @it_zglt0003.

        IF sy-subrc IS INITIAL.
          SORT it_zglt0003 BY tipo conta_ori.
        ENDIF.
      ENDIF.

      SELECT
      bukrs, kunnr, gjahr, belnr, rassc, hsl, tsl, racct
      FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf
      WHERE rldnr  EQ '0L'
      AND bukrs IN @p_bukrs
      AND gjahr  EQ @it_bkpf-gjahr
      AND belnr  EQ @it_bkpf-belnr
      AND koart  EQ 'S'
      INTO TABLE @DATA(it_acdoca_c).


*      SELECT
*      bukrs, kunnr, gjahr, belnr, rassc, hsl, tsl, racct
*      FROM zi_zgl0001_03
**      INNER JOIN zglt0003  AS b
**          ON a~racct = b~conta_ori
*      FOR ALL ENTRIES IN @it_bkpf
**      WHERE rldnr  EQ '0L'
*      WHERE rldnr  EQ '0L'
**      AND  TIPO   EQ '2'
*      AND bukrs IN @p_bukrs
*      AND gjahr  EQ @it_bkpf-gjahr
*      AND belnr  EQ @it_bkpf-belnr
*      AND koart  EQ 'S'
**      AND b~tipo EQ '2'
**      AND rassc ( rassc  IN ( SELECT tipo EQ 2 FROM zglt0003 ) OR ( rassc  EQ @space ) )
**      AND TIPO   EQ '2'
*      INTO TABLE @DATA(it_acdoca_c).

      IF it_acdoca_c IS NOT INITIAL.
        SORT it_acdoca_c BY bukrs kunnr. "gjahr belnr.
      ENDIF.

      DATA(lt_zglt0003) = it_zglt0003.

      DELETE lt_zglt0003 WHERE tipo <> '2'.
      DELETE ADJACENT
            DUPLICATES FROM lt_zglt0003 COMPARING conta_ori.

      DATA(lt_acdoca_c_aux) = it_acdoca_c.
      FREE lt_acdoca_c_aux.

      LOOP AT it_acdoca_c INTO DATA(ls_ac_c).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_ac_c-racct
          IMPORTING
            output = ls_ac_c-racct.

        READ TABLE lt_zglt0003 TRANSPORTING NO FIELDS
             WITH KEY conta_ori = ls_ac_c-racct.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_ac_c-racct
            IMPORTING
              output = ls_ac_c-racct.

          APPEND ls_ac_c TO lt_acdoca_c_aux.

        ENDIF.
      ENDLOOP.

*      SELECT a~rbukrs, a~kunnr, a~gjahr, a~belnr, a~rassc, a~hsl, a~tsl, a~racct
**      FROM zi_zgl0001_03 AS z
*      FROM acdoca AS a
*      INNER JOIN zglt0003 AS z
*          ON a~racct = z~conta_ori
*      INNER JOIN j_1bnfdoc AS j
*          ON j~parid = a~kunnr
**          ON z~rldnr = f~rldnr
**          ON s~connid = f~connid
**          ON s~connid = f~connid
**          ON s~connid = f~connid
*      FOR ALL ENTRIES IN @it_bkpf
*      WHERE a~rldnr  EQ '0L'
**      AND a~rbukrs IN @p_bukrs
*      AND a~rbukrs EQ j~bukrs
*      AND a~gjahr  EQ @it_bkpf-gjahr
*      AND a~belnr  EQ @it_bkpf-belnr
*      AND a~kunnr   EQ @it_bkpf-belnr
*      AND a~racct  EQ z~conta_ori
*      AND a~koart  EQ 'S'
*      AND z~tipo   EQ '2'
*      INTO TABLE @DATA(it_acdoca_cc).
*
*      IF it_acdoca_c IS NOT INITIAL.
*        SORT it_acdoca_c BY bukrs kunnr. "gjahr belnr.
*      ENDIF.
    ENDIF.
*Buscar ponto de coleta:
    IF it_1bnfdoc[] IS NOT INITIAL.

      SELECT docnum, ort01, regio, parvw FROM j_1bnfnad
      INTO TABLE @DATA(it_1bnfnad)
            FOR ALL ENTRIES IN @it_1bnfdoc
            WHERE docnum EQ @it_1bnfdoc-docnum
            AND parvw = 'PC'.

      IF it_1bnfnad IS NOT INITIAL.
        SORT it_1bnfnad BY docnum parvw.
      ENDIF.
*Buscar local de entrega:
      SELECT docnum, ort01, regio, parvw FROM j_1bnfnad
      APPENDING TABLE @it_1bnfnad
      FOR ALL ENTRIES IN @it_1bnfdoc
      WHERE docnum EQ @it_1bnfdoc-docnum
      AND parvw = 'LR'.

      IF it_1bnfnad IS NOT INITIAL.
        SORT it_1bnfnad BY docnum parvw.
      ENDIF.

*Buscar nome do cliente:
      SELECT kunnr, name1 FROM kna1
      INTO TABLE @DATA(it_kna1)
            FOR ALL ENTRIES IN @it_1bnfdoc
            WHERE kunnr EQ @it_1bnfdoc-parid.

      IF it_kna1 IS NOT INITIAL.
        SORT it_kna1 BY kunnr.
      ENDIF.

    ENDIF.

*Buscar descrição do material:
    IF it_1bnflin[] IS NOT INITIAL.

      SELECT vbeln, posnn, vbtyp_n, vbelv, sonum FROM vbfa
      INTO TABLE @DATA(it_vbfa)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE vbeln EQ @it_1bnflin-refkey(10)
            AND posnn EQ @it_1bnflin-refitm
            AND vbtyp_n EQ 'M'
            AND vbtyp_v EQ 'J'.
    ENDIF.

    IF it_vbfa[] IS NOT INITIAL.

      SELECT vbeln, posnn, vbtyp_n, vbelv, sonum FROM vbfa
      INTO TABLE @DATA(lt_vbfa_rout)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE vbeln EQ @it_1bnflin-refkey(10)
*            AND posnn EQ @it_1bnflin-refitm
            AND vbtyp_n EQ 'M'
            AND vbtyp_v EQ 'J'.

* Inicio - FA - 18.09.2025

*      SELECT vbeln, auart
*        FROM vbak
*        INTO TABLE @DATA(lt_vbak)
*        FOR ALL ENTRIES IN @lt_vbfa_rout
*        WHERE vbeln EQ @lt_vbfa_rout-vbelv
*          AND auart EQ 'ZTRI'.

      SELECT *
        FROM vbfa
        INTO TABLE @DATA(lt_vbfa_inti)
        FOR ALL ENTRIES IN @lt_vbfa_rout
        WHERE vbeln = @lt_vbfa_rout-vbeln AND
              vbtyp_v = 'J'.
* Fim - FA - 18.09.2025

      IF sy-subrc IS INITIAL.
        SELECT vbeln, route
          FROM likp
          INTO TABLE @DATA(lt_likp)
          FOR ALL ENTRIES IN @it_vbfa
          WHERE vbeln = @it_vbfa-vbelv.

        IF sy-subrc IS INITIAL.
          SORT lt_likp BY vbeln.
*          LOOP AT lt_vbfa_rout ASSIGNING FIELD-SYMBOL(<fs_vbfa>).
*
*            READ TABLE lt_vbak TRANSPORTING NO FIELDS
*                 WITH KEY vbeln = <fs_vbfa>-vbelv.
*
*            IF sy-subrc IS INITIAL.
*              APPEND <fs_vbfa> TO lt_vbfa_aux.
*            ENDIF.
*          ENDLOOP.
        ENDIF.
      ENDIF.

      SORT it_vbfa BY vbeln posnn.

      DATA: it_vbfax TYPE TABLE OF vbfa,
            wa_vbfax TYPE vbfa.

      LOOP AT it_vbfa INTO DATA(wa_vbfa).
        wa_vbfax = CORRESPONDING #( wa_vbfa ).
        FREE wa_vbfax-sonum.
        wa_vbfax-sonum = wa_vbfa-vbelv.
        APPEND wa_vbfax TO it_vbfax.
        CLEAR wa_vbfax.
      ENDLOOP.

      IF it_vbfax[] IS NOT INITIAL.

        SORT it_vbfax BY vbeln posnn.

        SELECT mblnr, mjahr, xblnr FROM mkpf
        INTO TABLE @DATA(it_mkpf)
              FOR ALL ENTRIES IN @it_vbfax
              WHERE xblnr EQ @it_vbfax-sonum.
        IF it_mkpf IS NOT INITIAL.
          SORT it_mkpf BY xblnr.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA: it_mkpf_c TYPE TABLE OF mkpf,
          wa_mkpf_c TYPE mkpf.

    LOOP AT it_mkpf INTO DATA(wa_mkpf).
      wa_mkpf_c = CORRESPONDING #( wa_mkpf ).
      FREE wa_mkpf_c-zch_referencia.
      wa_mkpf_c-zch_referencia = wa_mkpf-mblnr && wa_mkpf-mjahr.
      APPEND wa_mkpf_c TO it_mkpf_c.
      CLEAR wa_mkpf_c.
    ENDLOOP.

    IF it_mkpf_c[] IS NOT INITIAL.
      SORT it_mkpf_c BY xblnr.

      SELECT bukrs, belnr, gjahr, awkey FROM bkpf
      INTO TABLE @DATA(it_bkpf_c)
            FOR ALL ENTRIES IN @it_mkpf_c
            WHERE awkey = @it_mkpf_c-zch_referencia.
    ENDIF.

    IF it_bkpf_c[] IS NOT INITIAL.
      SORT it_bkpf_c BY awkey.

      SELECT bukrs, gjahr, belnr, rassc, hsl, tsl, racct, ktosl FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf_c
      WHERE bukrs EQ @it_bkpf_c-bukrs
      AND gjahr  EQ @it_bkpf_c-gjahr
      AND belnr  EQ @it_bkpf_c-belnr
      INTO TABLE @DATA(it_acdoca_c2).

      IF it_acdoca_c2 IS NOT INITIAL.
        SORT it_acdoca_c2 BY bukrs gjahr belnr ktosl.
      ENDIF.
    ENDIF.

    IF it_1bnflin[] IS NOT INITIAL.
      SELECT matnr, maktx FROM makt
      INTO TABLE @DATA(it_makt)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE matnr EQ @it_1bnflin-matnr
            AND spras EQ @sy-langu.

      IF sy-subrc IS INITIAL.
        SORT it_makt BY matnr.
      ENDIF.

*Buscar Ordem de venda:
      LOOP AT it_1bnflin INTO DATA(wa_1bnflin).
        DATA(lv_tabix) = sy-tabix.
        wa_1bnflinx = CORRESPONDING #( wa_1bnflin ).
        FREE wa_1bnflinx-matnr.
        wa_1bnflinx-matnr = wa_1bnflin-refkey.
        APPEND wa_1bnflinx TO it_1bnflinx.
        CLEAR wa_1bnflinx.
      ENDLOOP.
    ENDIF.

    IF it_1bnflin[] IS NOT INITIAL.
      SORT it_1bnflin BY refkey.
      SELECT matnr, vbeln, aubel FROM vbrp
      INTO TABLE @DATA(it_vbrp)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE vbeln EQ @it_1bnflin-refkey(10).

      SORT it_vbrp BY vbeln.
    ENDIF.

    IF it_vbrp[] IS NOT INITIAL.
      SORT it_vbrp BY aubel.
      SELECT vbeln, route FROM vbap
      INTO TABLE @DATA(it_vbap)
            FOR ALL ENTRIES IN @it_vbrp
            WHERE vbeln EQ @it_vbrp-aubel.
      IF it_vbap IS NOT INITIAL.
        SORT it_vbap BY vbeln.
      ENDIF.
      SORT it_vbrp BY vbeln.
    ENDIF.

    IF it_vbap[] IS NOT INITIAL.
      SORT it_vbap BY route.
      SELECT * FROM tvro
      INTO TABLE @DATA(it_tvro)
            FOR ALL ENTRIES IN @it_vbap
            WHERE route EQ @it_vbap-route.
      IF it_tvro IS NOT INITIAL.
        SORT it_tvro BY route.
      ENDIF.
      SORT it_vbap BY vbeln.
    ENDIF.

    SELECT tp_m, cfop
    INTO TABLE @DATA(git_zglt0001x)
          FROM zglt0001.

    IF sy-subrc IS INITIAL.
      SORT git_zglt0001x BY tp_m cfop.
    ENDIF.

*Incluir na saída o campo Nº Conhec. BL
    SELECT tp_m, cfop
    INTO TABLE @DATA(git_zglt0001)
          FROM zglt0001
          WHERE tp_m EQ 'ME'.

    IF sy-subrc IS INITIAL.
      SORT git_zglt0001 BY tp_m cfop.
    ENDIF.
**<<<------"188565 - NMS - INI------>>>
*    IF it_1bnflin IS NOT INITIAL.
    IF it_bseg[] IS NOT INITIAL.
**<<<------"188565 - NMS - FIM------>>>
      SELECT vbeln, posnn, vbtyp_n, vbelv, sonum FROM vbfa
      INTO TABLE @DATA(it_vbfa_bl)
*        FOR ALL ENTRIES IN @it_1bnflin
            FOR ALL ENTRIES IN @it_bseg
            WHERE vbeln EQ @it_bseg-vbeln "@it_bseg-xref3(10)
*         WHERE vbelv EQ @it_bseg-vbel2
            AND vbtyp_v EQ 'J'
            AND vbtyp_n IN ( 'M', 'O', 'P' ).
*          AND vbtyp_n EQ 'J'.
      IF it_vbfa IS NOT INITIAL.
        SORT it_vbfa_bl BY vbeln.

        SELECT vbeln, id_due
        INTO TABLE @DATA(it_zdoc_exp)
              FROM zdoc_exp
              FOR ALL ENTRIES IN @it_vbfa
              WHERE vbeln EQ @it_vbfa-vbelv.
        IF sy-subrc IS INITIAL.
          SORT it_zdoc_exp BY vbeln.

          SELECT id_due, id_nomeacao_tran
          INTO TABLE @DATA(it_zsdt0170)
                FROM zsdt0170
                FOR ALL ENTRIES IN @it_zdoc_exp
                WHERE id_due EQ @it_zdoc_exp-id_due.
          IF sy-subrc IS INITIAL.
            SORT it_zsdt0170 BY id_due.
            SELECT id_nomeacao_tran, nr_conhec, dt_data
            INTO TABLE @DATA(it_znom_conhec)
                  FROM znom_conhec
                  FOR ALL ENTRIES IN @it_zsdt0170
                  WHERE id_nomeacao_tran = @it_zsdt0170-id_nomeacao_tran.
            IF sy-subrc IS INITIAL.
              SORT it_znom_conhec BY id_nomeacao_tran.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*  DATA(lt_acdoca_c_aux) = it_acdoca_c.

*  SORT lt_acdoca_c_aux BY 0000100460.

  DELETE lt_acdoca_c_aux WHERE kunnr IS INITIAL.

  SORT lt_acdoca_c_aux BY racct DESCENDING.

*  DELETE ADJACENT
*         DUPLICATES FROM lt_acdoca_c_aux
*         COMPARING kunnr.

  LOOP AT it_1bnflin INTO wa_1bnflin.

    IF it_1bnfdoc IS NOT INITIAL.
      READ TABLE it_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_1bnfdoc>) WITH KEY docnum = wa_1bnflin-docnum
      BINARY SEARCH.


      IF sy-subrc IS INITIAL.

        gwa_saidan-bukrs   	 = <fs_1bnfdoc>-bukrs.
        gwa_saidan-branch  	 = <fs_1bnfdoc>-branch.
        gwa_saidan-nfenum    = <fs_1bnfdoc>-nfenum.
        gwa_saidan-docnum    = <fs_1bnfdoc>-docnum.
        gwa_saidan-parid     = <fs_1bnfdoc>-parid.
        gwa_saidan-shpunt    = <fs_1bnfdoc>-shpunt.
        gwa_saidan-pstdat    = <fs_1bnfdoc>-pstdat.
        gwa_saidan-docdat    = <fs_1bnfdoc>-docdat.
*      gwa_saidan-anzpk     = <fs_1bnfdoc>-anzpk.
        gwa_saidan-anzpk     = wa_1bnflin-menge.
        gwa_saidan-inco1     = <fs_1bnfdoc>-inco1.
        gwa_saidan-poper     = <fs_1bnfdoc>-pstdat+4(2).


        READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = <fs_1bnfdoc>-parid
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-name1     = wa_kna1-name1.
        ENDIF.

        READ TABLE it_1bnfnad INTO DATA(wa_1bnfnad) WITH KEY docnum = <fs_1bnfdoc>-docnum
              parvw = 'PC'
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-ort01_p   = wa_1bnfnad-ort01.
          gwa_saidan-regio     = wa_1bnfnad-regio.
        ENDIF.

        READ TABLE it_1bnfnad INTO DATA(wa_1bnfnadl) WITH KEY docnum = <fs_1bnfdoc>-docnum
              parvw = 'LR'
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-ort01     = wa_1bnfnadl-ort01.
          gwa_saidan-regio_l   = wa_1bnfnadl-regio.
        ENDIF.

        READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              awkey = wa_1bnflin-refkey(20)
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF wa_bkpf-waers EQ 'USD'.
            gwa_saidan-kursf     = abs( wa_bkpf-kursf ).
          ELSE.
            gwa_saidan-kursf     = abs( wa_bkpf-kurs2 ).
          ENDIF.
          gwa_saidan-belnr     = wa_bkpf-belnr.
          gwa_saidan-waers     = wa_bkpf-waers.

        ENDIF.

        READ TABLE lt_acdoca_c_aux INTO DATA(wa_acdoca_c) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              kunnr  = <fs_1bnfdoc>-parid
              belnr  = wa_bkpf-belnr.
*              BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_acdoca_c-racct
            IMPORTING
              output = gwa_saidan-saknr_r.

          READ TABLE lt_zglt0003 INTO DATA(wa_zglt0003) WITH KEY tipo = '2'
                conta_ori = gwa_saidan-saknr_r
                BINARY SEARCH.

          IF sy-subrc IS INITIAL.
            gwa_saidan-saknr_rc  = wa_zglt0003-conta_cut.
          ENDIF.
        ENDIF.


*        READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
*              awkey = wa_1bnflin-refkey(20)
*              BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*
*          IF wa_bkpf-waers EQ 'USD'.
*            gwa_saidan-kursf     = abs( wa_bkpf-kursf ).
*          ELSE.
*            gwa_saidan-kursf     = abs( wa_bkpf-kurs2 ).
*          ENDIF.
*          gwa_saidan-belnr     = wa_bkpf-belnr.
*          gwa_saidan-waers     = wa_bkpf-waers.
*
*        ENDIF.

        READ TABLE it_acdoca INTO DATA(wa_acdoca) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              gjahr  = wa_bkpf-gjahr
              belnr  = wa_bkpf-belnr
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-gjahr     = wa_acdoca-gjahr.
          gwa_saidan-rassc     = wa_acdoca-rassc.
          gwa_saidan-hsl       = wa_1bnflin-nfnet.

          IF gwa_saidan-kursf NE 0.
            gwa_saidan-tsl       = wa_1bnflin-nfnet / abs( gwa_saidan-kursf ).
          ELSE.
            CLEAR gwa_saidan-tsl.
          ENDIF.


          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_acdoca-racct
            IMPORTING
              output = gwa_saidan-racct.

        ENDIF.


        READ TABLE it_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '1'
        conta_ori = gwa_saidan-racct
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-saknr_cc  = wa_zglt0003-conta_cut.
        ENDIF.

*   Incluir na saída o campo Nº Conhec. BL
        READ TABLE git_zglt0001 INTO DATA(wa_zglt0001) WITH KEY cfop = wa_1bnflin-cfop.

        IF sy-subrc IS INITIAL.

          READ TABLE it_bseg INTO DATA(wa_bseg) WITH KEY bukrs = wa_bkpf-bukrs
                belnr = wa_bkpf-belnr
                kunnr = <fs_1bnfdoc>-parid
                BINARY SEARCH.
          IF sy-subrc IS INITIAL.


            READ TABLE it_vbfa_bl INTO DATA(wa_vbfa_bl) WITH KEY vbeln = wa_bseg-vbeln "vbeln = wa_bseg-xref3(10) "vbelv = wa_bseg-vbel2 " vbeln = wa_bseg-xref3(10)
                  BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              READ TABLE it_zdoc_exp INTO DATA(wa_zdoc_exp) WITH KEY vbeln = wa_vbfa_bl-vbelv
                    BINARY SEARCH.

              IF sy-subrc IS INITIAL.

                READ TABLE it_zsdt0170 INTO DATA(wa_zsdt0170) WITH KEY id_due = wa_zdoc_exp-id_due
                      BINARY SEARCH.

                IF sy-subrc IS INITIAL.

                  READ TABLE it_znom_conhec INTO DATA(wa_znom_conhec) WITH KEY id_nomeacao_tran = wa_zsdt0170-id_nomeacao_tran
                        BINARY SEARCH.

                  IF sy-subrc IS INITIAL AND wa_znom_conhec-dt_data IS NOT INITIAL AND p_budat-high GT wa_znom_conhec-dt_data.
                    CONTINUE.
                  ENDIF.

                  IF sy-subrc IS INITIAL AND wa_znom_conhec-nr_conhec IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF..
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    gwa_saidan-cfop    	 = wa_1bnflin-cfop.
    gwa_saidan-matkl     = wa_1bnflin-matkl.
    gwa_saidan-matnr     = wa_1bnflin-matnr.
    gwa_saidan-refkey    = wa_1bnflin-refkey.

    READ TABLE git_zglt0001x INTO DATA(wa_zglt0001x) WITH KEY cfop = wa_1bnflin-cfop.
    IF sy-subrc IS INITIAL.
      gwa_saidan-tp_m      = wa_zglt0001x-tp_m.
    ENDIF.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_1bnflin-matnr
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-maktx     = wa_makt-maktx.
    ENDIF.

    READ TABLE it_vbrp INTO DATA(wa_vbrp) WITH KEY vbeln = wa_1bnflin-refkey
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-aubel     = wa_vbrp-aubel.
    ENDIF.

    READ TABLE it_vbfax INTO wa_vbfax WITH KEY vbeln = wa_1bnflin-refkey(10)
    posnn = wa_1bnflin-refitm
    BINARY SEARCH.

    READ TABLE it_mkpf_c INTO wa_mkpf_c WITH KEY xblnr = wa_vbfax-sonum
    BINARY SEARCH.

    READ TABLE it_bkpf_c INTO DATA(wa_bkpf_c) WITH KEY awkey = wa_mkpf_c-zch_referencia
          BINARY SEARCH.

    READ TABLE it_acdoca_c2 INTO DATA(wa_acdoca_c2) WITH KEY bukrs = wa_bkpf_c-bukrs
          gjahr  = wa_bkpf_c-gjahr
          belnr  = wa_bkpf_c-belnr
          ktosl  = 'GBB'.
*          BINARY SEARCH.

    IF sy-subrc IS INITIAL. " AND wa_acdoca_c2-ktosl = 'GBB'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_acdoca_c2-racct
        IMPORTING
          output = gwa_saidan-saknr_c.
    ENDIF.

    READ TABLE it_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '3'
    conta_ori = gwa_saidan-saknr_c
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_saidan-saknr_p   = wa_zglt0003-conta_cut.
    ENDIF.

    gwa_saidan-saknr_e = wa_1bnflin-matkl.
    READ TABLE it_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '4'
    conta_ori = wa_1bnflin-matkl
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-saknr_ec  = wa_zglt0003-conta_cut.
    ENDIF.

    IF  gwa_saidan-regio EQ gwa_saidan-regio_l.
      gwa_saidan-uf        = 'SIM'.
    ELSE.
      gwa_saidan-uf        = 'NAO'.
    ENDIF.


    READ TABLE it_vbap INTO DATA(wa_vbap) WITH KEY vbeln = wa_vbrp-aubel
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-route     = wa_vbap-route.
    ENDIF.

*    READ TABLE lt_vbfa_aux INTO DATA(wa_vbfa_aux) WITH KEY vbeln = wa_vbrp-vbeln.
**            BINARY SEARCH.

*    IF sy-subrc IS INITIAL.
*
*      READ TABLE lt_vbak INTO DATA(wa_vbak) WITH KEY vbeln = wa_vbfa_aux-vbelv.
**          BINARY SEARCH.
*
*      IF sy-subrc IS INITIAL.
*    READ TABLE it_vbfa INTO DATA(wa_vbfa_aux) WITH KEY vbeln = wa_vbak-vbeln.

    IF gwa_saidan-route IS INITIAL.

      READ TABLE it_vbfa INTO DATA(wa_vbfa_aux) WITH KEY vbeln = wa_vbrp-vbeln.
*    binary search.

      IF sy-subrc IS INITIAL.
        READ TABLE lt_likp INTO DATA(wa_likp) WITH KEY vbeln = wa_vbfa_aux-vbelv
        BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          gwa_saidan-route     = wa_likp-route.
        ENDIF.

      ENDIF.
*    ENDIF.

    ENDIF.

    READ TABLE it_tvro INTO DATA(wa_tvro) WITH KEY route = gwa_saidan-route"wa_vbap-route
*    READ TABLE it_tvro INTO DATA(wa_tvro) WITH KEY route = gwa_saidan-route
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      "RGA - 182960 - Bug impeditivo  - 30.06.2025
      "horas transporte
      DATA(lv_hrtransp) = wa_tvro-distz / 60.

      "dias transporte
      DATA(lv_diastransp) = round( val = ( lv_hrtransp / 8 ) dec = 0 ).


      IF <fs_1bnfdoc>-authtime >= '150000'.
        DATA(lv_dtauto) = <fs_1bnfdoc>-authdate + 1.
        gwa_saidan-docdat_e = lv_diastransp + lv_dtauto.
      ELSE.
        gwa_saidan-docdat_e = lv_diastransp + <fs_1bnfdoc>-authdate.
      ENDIF.
      "RGA - 182960 - Bug impeditivo  - 30.06.2025 - fim

      IF wa_tvro-traztd GT 0.
        DATA(lv_calc) = ( wa_tvro-traztd / 10000 ).
      ENDIF.

      gwa_saidan-traztd    = lv_calc.


    ELSE.

      IF gwa_saidan-route IS NOT INITIAL.

        SELECT SINGLE *
          FROM tvro
          INTO @DATA(ls_tvro)
          WHERE route EQ @gwa_saidan-route.
        IF sy-subrc IS INITIAL.
*          SORT it_tvro BY route.
*        ENDIF.

*          READ TABLE it_tvro INTO wa_tvro WITH KEY route = gwa_saidan-route
*
*          BINARY SEARCH.
*          IF sy-subrc IS INITIAL.

          "RGA - 182960 - Bug impeditivo  - 30.06.2025
          "horas transporte
          lv_hrtransp = ls_tvro-distz / 60.

          "dias transporte
          lv_diastransp = round( val = ( lv_hrtransp / 8 ) dec = 0 ).


          IF <fs_1bnfdoc>-authtime >= '150000'.
            lv_dtauto = <fs_1bnfdoc>-authdate + 1.
            gwa_saidan-docdat_e = lv_diastransp + lv_dtauto.
          ELSE.
            gwa_saidan-docdat_e = lv_diastransp + <fs_1bnfdoc>-authdate.
          ENDIF.
          "RGA - 182960 - Bug impeditivo  - 30.06.2025 - fim

          IF ls_tvro-traztd GT 0.
            lv_calc = ( ls_tvro-traztd / 10000 ).
          ENDIF.

          gwa_saidan-traztd    = lv_calc.

*          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

*    gwa_saidan-docdat_e  = <fs_1bnfdoc>-pstdat + lv_calc.

    IF gwa_saidan-docdat_e GE p_budat-low AND gwa_saidan-docdat_e LE p_budat-high.
      gwa_saidan-gera      = 'NAO'. "'SIM'.
    ELSEIF p_budat-high IS INITIAL.
      DATA: v_ultimo_dia TYPE sy-datum.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = p_budat-low
        IMPORTING
          last_day_of_month = v_ultimo_dia.

      IF gwa_saidan-docdat_e LE v_ultimo_dia.
        gwa_saidan-gera      = 'NAO'. "'SIM'.
      ENDIF.
    ELSE.
      gwa_saidan-gera      = 'SIM'. "'NAO'.
    ENDIF.

    IF gwa_saidan-tp_m EQ 'ME'.
      gwa_saidan-gera      = 'SIM'.
    ENDIF.

    APPEND gwa_saidan TO git_saidan.
    CLEAR gwa_saidan.

  ENDLOOP.


  IF git_saidan[] IS INITIAL.
    "MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros com os parâmetros informado!'(004).
    "MESSAGE 'Não foram encontrados registros com os parâmetros informado!' TYPE 'I' DISPLAY LIKE 'i'.
    "EXIT.
  ELSE.
    SORT git_saidan[] BY gera ASCENDING.

    DELETE git_saidan[] WHERE gjahr IS INITIAL AND rassc IS INITIAL AND hsl IS INITIAL.

    IF p_gera = abap_false.

    ELSEIF p_gera = 'SIM'.
      DELETE git_saidan[] WHERE gera <> 'SIM'.
    ELSE.
      DELETE git_saidan[] WHERE gera <> 'NAO'.
    ENDIF.

    IF p_matnr[] IS INITIAL.

    ELSE.
      DELETE git_saidan[] WHERE matnr NOT IN p_matnr[].
    ENDIF.

    IF p_waers = abap_false.

    ELSE.
      DELETE git_saidan[] WHERE waers <> p_waers-low.
    ENDIF.

*    IF p_saknr = abap_false.
    IF p_saknr[] IS INITIAL.

    ELSE.
*      DATA: _saknr_cc TYPE saknr.
*      CLEAR:_saknr_cc.
*      _saknr_cc = p_saknr-low.

      DATA: lr_saknr TYPE RANGE OF saknr.
      DATA: ls_saknr LIKE LINE OF lr_saknr.

      ls_saknr-sign   = 'I'.
      ls_saknr-option = 'EQ'.

      LOOP AT p_saknr[] INTO DATA(ls_saknr_tela).
        ls_saknr-low = ls_saknr_tela-low.
        APPEND ls_saknr TO lr_saknr.
      ENDLOOP.

      LOOP AT lr_saknr ASSIGNING FIELD-SYMBOL(<fs_saknr>).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_saknr>-low
          IMPORTING
            output = <fs_saknr>-low.
        CONDENSE <fs_saknr>-low  NO-GAPS.
      ENDLOOP.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = _saknr_cc
*        IMPORTING
*          output = _saknr_cc.
*      CONDENSE _saknr_cc NO-GAPS.

*      DELETE git_saidan[] WHERE saknr_cc <> _saknr_cc.
      DELETE git_saidan[] WHERE saknr_cc NOT IN lr_saknr.
    ENDIF.
  ENDIF.

  IF git_saidan[] IS NOT INITIAL.
    MOVE-CORRESPONDING git_saidan[] TO it_saida1.

    LOOP AT it_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida1>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-racct
        IMPORTING
          output = <fs_saida1>-racct.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_cc
        IMPORTING
          output = <fs_saida1>-saknr_cc.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_r
        IMPORTING
          output = <fs_saida1>-saknr_r.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_rc
        IMPORTING
          output = <fs_saida1>-saknr_rc.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_c
        IMPORTING
          output = <fs_saida1>-saknr_c.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_p
        IMPORTING
          output = <fs_saida1>-saknr_p.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_e
        IMPORTING
          output = <fs_saida1>-saknr_e.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_saida1>-saknr_ec
        IMPORTING
          output = <fs_saida1>-saknr_ec.

    ENDLOOP.

    FREE: git_saidan.
  ENDIF.

ENDFORM.

FORM get_dados_alv2.

  DATA: lr_racct  TYPE RANGE OF acdoca-racct,
        lt_saida2 TYPE STANDARD TABLE OF ty_saida2 INITIAL SIZE 0,
        lw_saida2 TYPE ty_saida2.

  FREE:it_saida2,lr_racct[].
  CLEAR: lr_racct.

  DATA(it_racct) = it_zglt0003.
  DELETE it_racct WHERE tipo <> '3'.
  SORT it_racct BY conta_ori ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_racct COMPARING conta_ori.

  IF it_racct IS NOT INITIAL.

    LOOP AT it_racct ASSIGNING FIELD-SYMBOL(<fs_racct>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_racct>-conta_ori
        IMPORTING
          output = <fs_racct>-conta_ori.
    ENDLOOP.

    lr_racct[] = VALUE #( FOR wa_racct IN it_racct ( option = 'EQ' sign = 'I' low = wa_racct-conta_ori ) ).

    SORT lr_racct.
    DELETE ADJACENT DUPLICATES FROM lr_racct.
    DELETE lr_racct WHERE low IS INITIAL.
  ENDIF.

  SELECT DISTINCT * FROM zi_zgl0001_02(
          p_bukrs = @p_bukrs-low
          , p_gjahr = @p_ano
          , p_poper = @p_poper
  ) WHERE racct IN @lr_racct[]
  INTO TABLE @DATA(getlist_acdoca).

  IF getlist_acdoca IS NOT INITIAL.

    SORT getlist_acdoca.
    CLEAR: lw_saida2.
    LOOP AT getlist_acdoca ASSIGNING FIELD-SYMBOL(<fs_acdocax>).
      lw_saida2-rbukrs = <fs_acdocax>-bukrs.
      lw_saida2-gjahr = <fs_acdocax>-gjahr.
      lw_saida2-matnr = <fs_acdocax>-matnr.
      lw_saida2-racct = <fs_acdocax>-racct.
      lw_saida2-poper = <fs_acdocax>-poper.

      SELECT SINGLE * FROM zi_zgl0001_01(
      p_racct = @<fs_acdocax>-racct
      , p_bukrs = @<fs_acdocax>-bukrs
      , p_gjahr = @<fs_acdocax>-gjahr
      , p_poper = @<fs_acdocax>-poper
      , p_matnr = @<fs_acdocax>-matnr
      ) INTO @DATA(get_valores).

      lw_saida2-ksl   =  get_valores-ksl.
      lw_saida2-msl   =  get_valores-msl.
      lw_saida2-tsl   =  get_valores-tsl.

      IF get_valores-msl NE 0 AND get_valores-tsl NE 0.
        lw_saida2-precob =  get_valores-tsl / get_valores-msl.
      ENDIF.
      IF get_valores-msl NE 0 AND get_valores-tsl EQ 0.
        lw_saida2-precob =  get_valores-tsl.
      ENDIF.
      IF get_valores-ksl NE 0 AND get_valores-msl NE 0.
        lw_saida2-preco =  get_valores-ksl / get_valores-msl.
      ENDIF.
      IF get_valores-ksl NE 0 AND get_valores-msl EQ 0.
        lw_saida2-preco =  get_valores-ksl.
      ENDIF.

      APPEND lw_saida2 TO lt_saida2.
      CLEAR:lw_saida2.
    ENDLOOP.

    FREE: getlist_acdoca.
    MOVE-CORRESPONDING lt_saida2 TO it_saida2.
  ENDIF.

ENDFORM.
