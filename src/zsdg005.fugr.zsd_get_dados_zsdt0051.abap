FUNCTION zsd_get_dados_zsdt0051.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(R_TPCONT) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_CONT) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_ORGVEN) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_CDIST) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_SATIV) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_ESCVEN) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_CLIEN) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_DATENT) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_FATUV) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_CENT) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_MATER) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_GRUPO) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_WAERKS) TYPE  ZRSDSSELOPTS OPTIONAL
*"     REFERENCE(R_POSNR) TYPE  ZRSDSSELOPTS
*"  TABLES
*"      T_OV TYPE  ZSDE009_T OPTIONAL
*"      T_REMESSA TYPE  ZSDE010_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: somar         TYPE j_1bnflin-menge,
        diminuir      TYPE j_1bnflin-menge,
        vbelnrfmg     TYPE vbfa-vbeln,
        vg_refkey_aux TYPE j_1bnflin-refkey,
        vg_refkey     TYPE j_1bnflin-refkey.

  IF  r_tpcont  IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Tipo de Contrato !'.
    EXIT.
  ENDIF.

  IF  r_orgven IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Organização de Vendas !'.
    EXIT.
  ENDIF.

  IF r_orgven IS NOT INITIAL .

    DATA(vl_orgven) = r_orgven[ 1 ]-low.

    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
      ID 'BUKRS' FIELD vl_orgven.

    IF sy-subrc <> 0.
      MESSAGE i000(z01) WITH 'Perfil do usuário sem acesso a esta Org. de Vendas'.
      EXIT.
    ENDIF.

  ENDIF.

  IF  r_cdist IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Canal Distribuição !'.
    EXIT.
  ENDIF.

  IF  r_sativ IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar o Setor de Atividade !'.
    EXIT.
  ENDIF.

  IF  r_datent IS INITIAL .
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de Entrada !'.
    EXIT.
  ENDIF.

  IF  r_fatuv[] IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data de Faturamento !'.
    EXIT.
  ENDIF.

  SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel erdat lifsk faksk
    FROM vbak
    INTO TABLE it_vbak
    WHERE auart IN  r_tpcont
    AND vbeln   IN  r_cont
    AND vkorg   IN  r_orgven
    AND vtweg   IN  r_cdist
    AND spart   IN  r_sativ
    AND vkbur   IN  r_escven
    AND kunnr   IN  r_clien
    AND audat   IN  r_datent.

  CHECK sy-subrc IS INITIAL.

  SELECT *
    APPENDING TABLE it_zsdt0041_safra
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT *
    FROM tvbur
       INTO TABLE it_tvbur
   FOR ALL ENTRIES IN it_vbak
  WHERE vkbur EQ it_vbak-vkbur.

  IF sy-subrc IS INITIAL.
    SELECT *
    FROM tvkbt
      INTO TABLE it_tvkbt
      FOR ALL ENTRIES IN it_tvbur
    WHERE vkbur = it_tvbur-vkbur
        AND spras EQ sy-langu.
  ENDIF.

  SELECT *
  FROM tvfs
    INTO TABLE it_tvfs
    FOR ALL ENTRIES IN it_vbak
  WHERE faksp EQ it_vbak-lifsk.

  IF sy-subrc IS INITIAL.
    SELECT *
    FROM tvfst
      INTO TABLE it_tvfst
      FOR ALL ENTRIES IN it_tvfs
    WHERE faksp = it_tvfs-faksp
      AND  spras EQ sy-langu.
  ENDIF.

  SELECT *
  FROM tvls
    INTO TABLE it_tvls
    FOR ALL ENTRIES IN it_vbak
  WHERE lifsp EQ it_vbak-lifsk.

  IF sy-subrc IS INITIAL.
    SELECT *
    FROM tvlst
      INTO TABLE it_tvlst
      FOR ALL ENTRIES IN it_tvls
    WHERE lifsp = it_tvls-lifsp
       AND  spras EQ sy-langu.
  ENDIF.

  SELECT *
  FROM zsdt0271
    INTO TABLE it_zsdt0271
   FOR ALL ENTRIES IN it_vbak
  WHERE filial EQ it_vbak-vkbur .

  IF it_zsdt0271[] IS NOT INITIAL.
    SELECT *
     FROM zsdt0270
   INTO TABLE it_zsdt0270
     FOR ALL ENTRIES IN it_zsdt0271
   WHERE cod_regional EQ it_zsdt0271-cod_regional .
  ENDIF.

  SELECT doc_simulacao vbeln
    INTO TABLE it_zsdt0041
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT *
    INTO TABLE it_zsdt0041_frete
    FROM zsdt0041
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT doc_simulacao vbeln
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbeln EQ it_vbak-vbeln.

  SELECT doc_simulacao vbelv
    APPENDING TABLE it_zsdt0041
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbelv EQ it_vbak-vbeln.

  SELECT *
    INTO TABLE it_zsdt0090_frete
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbak
   WHERE vbelv EQ it_vbak-vbeln
      OR vbeln EQ it_vbak-vbeln.

  SORT it_zsdt0041 BY vbeln doc_simulacao.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0041 COMPARING ALL FIELDS.

  IF it_zsdt0041[] IS NOT INITIAL.

    SELECT doc_simulacao cultura safra
      INTO TABLE it_zsdt0040
      FROM zsdt0040
      FOR ALL ENTRIES IN it_zsdt0041
     WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    SORT it_zsdt0040 BY doc_simulacao.

    SELECT * INTO TABLE it_zsdt0038
      FROM zsdt0038
       FOR ALL ENTRIES IN it_zsdt0040
     WHERE cultura EQ it_zsdt0040-cultura.

    SORT it_zsdt0038 BY cultura.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zfit0026
      INTO TABLE it_0026
      FOR ALL ENTRIES IN it_vbak
       WHERE vbeln EQ it_vbak-vbeln.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zib_contabil_chv
        INTO TABLE it_zib
         FOR ALL ENTRIES IN it_0026
          WHERE obj_key EQ it_0026-obj_key.

      IF sy-subrc IS INITIAL.
        LOOP AT it_zib.
          READ TABLE it_0026 WITH KEY obj_key = it_zib-obj_key.

          IF sy-subrc IS INITIAL.
            MOVE : it_zib-bukrs   TO it_zib_bsid-bukrs,
                   it_0026-docnum TO it_zib_bsid-belnr,
                   it_zib-gjahr   TO it_zib_bsid-gjahr.

            IF it_0026-docnum = '0000000000'.
              MOVE it_zib-belnr TO it_zib_bsid-belnr.
            ENDIF.

            APPEND it_zib_bsid.
          ENDIF.
          CLEAR: it_zib_bsid.
        ENDLOOP.

        IF sy-subrc IS INITIAL.
          SELECT *
           FROM bsid
           INTO TABLE it_bsid
           FOR ALL ENTRIES IN it_zib_bsid
           WHERE bukrs EQ it_zib_bsid-bukrs
             AND belnr EQ it_zib_bsid-belnr
             AND gjahr EQ it_zib_bsid-gjahr.

          IF sy-subrc IS INITIAL.
            SELECT *
              FROM bsad
              INTO TABLE it_bsad
              FOR ALL ENTRIES IN it_bsid
              WHERE bukrs EQ it_bsid-bukrs
                AND belnr EQ it_bsid-belnr
                AND gjahr EQ it_bsid-gjahr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT *
    FROM vbkd
    INTO TABLE it_vbkd_aux
     FOR ALL ENTRIES IN it_vbak
     WHERE vbeln EQ it_vbak-vbeln.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM t052u
      INTO TABLE it_t052u
      FOR ALL ENTRIES IN it_vbkd_aux
      WHERE spras EQ sy-langu
       AND  zterm EQ it_vbkd_aux-zterm.

    SELECT   spras   zterm   vtext
      FROM tvzbt
      INTO TABLE it_tvzbt
      FOR ALL ENTRIES IN it_vbkd_aux
      WHERE spras EQ sy-langu
       AND  zterm EQ it_vbkd_aux-zterm.

  ENDIF.

  SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , WAERS , KBETR , KMEIN , KWERT FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'PR00' AND WAERS IN @R_WAERKS INTO TABLE @IT_KONV .

  SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , WAERS , KBETR , KMEIN , KWERT FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'ICMI' AND WAERS IN @R_WAERKS INTO TABLE @IT_KONV_IMP .

  SELECT vbeln vbap~matnr arktx werks vbap~ntgew vbap~gewei posnr kwmeng vrkme j_1bcfop j_1btxsdc
    FROM vbap
    INNER JOIN mara
    ON mara~matnr  = vbap~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_vbap
    FOR ALL ENTRIES IN it_vbak
    WHERE vbap~vbeln EQ it_vbak-vbeln
    AND   vbap~matnr IN r_mater
    AND   mara~matkl IN r_grupo
    AND   vbap~werks IN r_cent.

  CHECK sy-subrc IS INITIAL.

  SELECT matnr wrkst
    FROM mara
    INTO TABLE it_mara
    FOR ALL ENTRIES IN it_vbap
    WHERE matnr EQ it_vbap-matnr.

  SELECT vbeln posnr etenr lifsp
    FROM vbep
    INTO TABLE it_vbep
     FOR ALL ENTRIES IN it_vbap
   WHERE vbeln EQ it_vbap-vbeln
     AND posnr EQ it_vbap-posnr
     AND etenr EQ 1.

  SELECT * INTO TABLE it_vbep2
    FROM vbep
    FOR ALL ENTRIES IN it_vbap
    WHERE vbeln EQ it_vbap-vbeln
      AND posnr EQ it_vbap-posnr.

  SELECT vbeln valdt
    FROM vbkd
    INTO TABLE it_vbkd
    FOR ALL ENTRIES IN it_vbak
  WHERE vbeln EQ it_vbak-vbeln.

  SELECT name1 kunnr stcd1 stcd2
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbak
    WHERE kunnr EQ it_vbak-kunnr .

  REFRESH rg_cpf_cnpj.
  rg_cpf_cnpj-sign = 'I'.
  rg_cpf_cnpj-option = 'EQ'.

  LOOP AT it_kna1 INTO wa_kna1.
    IF wa_kna1-stcd1 IS NOT INITIAL.
      rg_cpf_cnpj-low = wa_kna1-stcd1.
      APPEND rg_cpf_cnpj.
    ENDIF.
    IF wa_kna1-stcd2 IS NOT INITIAL.
      rg_cpf_cnpj-low = wa_kna1-stcd2.
      APPEND rg_cpf_cnpj.
    ENDIF.
  ENDLOOP.

  CHECK sy-subrc IS INITIAL.

  SELECT vbelv vbeln erdat rfmng rfwrt vbtyp_n vbtyp_v posnn posnv
   FROM vbfa
   INTO TABLE it_vbfa_aux2
   FOR ALL ENTRIES IN it_vbap
   WHERE vbelv EQ it_vbap-vbeln
    AND  posnv EQ it_vbap-posnr
    AND  vbtyp_n IN ('M','O','N','R','S')
    AND  vbtyp_v IN ('C','M','T').

  IF r_fatuv[] IS NOT INITIAL.
    DATA(vl_fatuv) = r_fatuv[ 1 ]-high.
  ENDIF.
  LOOP AT it_vbfa_aux2.
    IF it_vbfa_aux2-erdat IN r_fatuv.
      APPEND it_vbfa_aux2 TO it_vbfa.
      DELETE it_vbfa_aux2.
    ELSEIF it_vbfa_aux2-erdat GT vl_fatuv.
      DELETE it_vbfa_aux2.
    ENDIF.
  ENDLOOP.

  IF it_vbfa[] IS NOT INITIAL.
    SELECT vbeln fkart sfakn
      FROM vbrk
      INTO TABLE it_vbrk
      FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln = it_vbfa-vbeln.

    it_vbrk_aux[] = it_vbrk[].

    it_vbrk_est[] = it_vbrk[].
    DELETE it_vbrk_est WHERE fkart NE 'ZROB'.

    IF it_vbrk_est[] IS NOT INITIAL.
      SELECT vbelv vbeln erdat rfmng rfwrt vbtyp_n vbtyp_v posnn posnv
        INTO TABLE it_vbfa_est
        FROM vbfa
        FOR ALL ENTRIES IN it_vbrk_est
        WHERE vbeln EQ it_vbrk_est-vbeln
         AND  vbtyp_n EQ 'O'
         AND  vbtyp_v EQ 'H'.

      IF it_vbfa_est[] IS NOT INITIAL.
        SELECT vbeln vkorg vtweg spart auart vkbur kunnr audat knumv vgbel
          INTO TABLE it_vbak_est
          FROM vbak
          FOR ALL ENTRIES IN it_vbfa_est
          WHERE vbeln EQ it_vbfa_est-vbelv.

        IF it_vbak_est[] IS NOT INITIAL.
          LOOP AT it_vbak_est INTO wa_vbak.
            READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbak-vgbel
                                                     fkart = 'ZTRI'.
            IF sy-subrc IS INITIAL.

              DELETE it_vbrk WHERE vbeln EQ wa_vbak-vgbel.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

    DELETE it_vbrk_aux WHERE sfakn IS INITIAL.

    SORT it_vbrk BY vbeln.
    SORT it_vbfa BY vbeln.

    LOOP AT it_vbrk_aux INTO wa_vbrk_aux.

      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      DELETE it_vbfa WHERE vbeln EQ wa_vbrk_aux-sfakn.
      DELETE it_vbfa WHERE vbeln EQ wa_vbrk_aux-vbeln.

    ENDLOOP.

    LOOP AT it_vbfa INTO wa_vbfa.
      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
      APPEND wa_vbfa_aux TO it_vbfa_aux.

    ENDLOOP.

    SORT it_vbfa_aux BY vg_refkey.

    SELECT refkey docnum menge meins netwr menge itmnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_vbfa_aux
      WHERE refkey  EQ it_vbfa_aux-vg_refkey.

    CHECK sy-subrc IS INITIAL.

    SELECT docnum docdat nfnum nfe nfenum
      FROM j_1bnfdoc
      INTO TABLE it_j_1bnfdoc
      FOR ALL ENTRIES IN it_j_1bnflin
      WHERE docnum EQ it_j_1bnflin-docnum.

    CHECK sy-subrc IS INITIAL.

    SORT: it_vbfa      BY vbeln posnn,
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln.

    LOOP AT it_vbap INTO wa_vbap.
      CLEAR: somar, diminuir, wa_vbfa_tot.

      LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
        IF wa_vbrk-fkart = 'ZTRI' OR sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbfa-vbeln
          IMPORTING
            output = wa_vbfa-vbeln.

        vg_refkey = wa_vbfa-vbeln.

        READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = vg_refkey itmnum = wa_vbfa-posnn BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          "Somar/Diminuir
          IF wa_vbfa-vbtyp_n = 'M'.
            somar = somar + wa_j_1bnflin-menge.
          ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O' )
            OR   ( wa_vbfa-vbtyp_n = 'R'   AND  wa_vbfa-vbtyp_v = 'T' ) .
            diminuir = diminuir + wa_j_1bnflin-menge.
          ENDIF.

          wa_vbfa_tot-totalmenge = somar - diminuir.
          wa_vbfa_tot-posnn      = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.
          wa_vbfa_tot-totaldim   = diminuir.

        ENDIF.
      ENDLOOP.

      IF wa_vbfa_tot-totalmenge GE 0.
        APPEND wa_vbfa_tot TO it_vbfa_tot.

        CLEAR: wa_j_1bnflin,
               wa_vbfa,
               wa_vbap.
      ENDIF.

    ENDLOOP.
  ENDIF.

  IF it_vbfa_aux2[] IS NOT INITIAL.
    SELECT vbeln fkart sfakn
      FROM vbrk
      APPENDING TABLE it_vbrk
      FOR ALL ENTRIES IN it_vbfa_aux2
      WHERE vbeln = it_vbfa_aux2-vbeln.

    it_vbrk_aux[] = it_vbrk[].
    DELETE it_vbrk_aux WHERE sfakn IS INITIAL.

    SORT it_vbrk      BY vbeln.
    SORT it_vbfa_aux2 BY vbeln.

    LOOP AT it_vbrk_aux INTO wa_vbrk_aux.
      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbrk INDEX sy-tabix.
      ENDIF.

      "Elimina Estorno VBFA
      READ TABLE it_vbfa_aux2 INTO wa_vbfa WITH KEY vbeln = wa_vbrk_aux-sfakn BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbfa_aux2 INDEX sy-tabix.
      ENDIF.

      READ TABLE it_vbfa_aux2 INTO wa_vbfa WITH KEY vbeln = wa_vbrk_aux-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_vbfa_aux2 INDEX sy-tabix.
      ENDIF.

    ENDLOOP.

    REFRESH it_vbfa_aux.

    LOOP AT it_vbfa_aux2 INTO wa_vbfa.
      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
      APPEND wa_vbfa_aux TO it_vbfa_aux.
    ENDLOOP.

    SORT it_vbfa_aux BY vg_refkey.

    SELECT refkey docnum menge meins netwr menge itmnum
      FROM j_1bnflin
      APPENDING TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_vbfa_aux
      WHERE refkey  EQ it_vbfa_aux-vg_refkey.

    SORT: it_vbfa_aux2 BY vbeln posnn,
          it_j_1bnflin BY refkey itmnum,
          it_vbrk      BY vbeln.

    LOOP AT it_vbap INTO wa_vbap.

      CLEAR: somar, diminuir, wa_vbfa_tot.

      LOOP AT it_vbfa_aux2 INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnv EQ wa_vbap-posnr.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.
        IF wa_vbrk-fkart = 'ZTRI'.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_vbfa-vbeln
          IMPORTING
            output = wa_vbfa-vbeln.

        vg_refkey = wa_vbfa-vbeln.

        READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = vg_refkey itmnum = wa_vbfa-posnn BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          "Somar/Diminuir
          IF wa_vbfa-vbtyp_n = 'M'.
            somar = somar + wa_j_1bnflin-menge.
          ENDIF.

          wa_vbfa_tot-totalmenge = somar - diminuir.
          wa_vbfa_tot-posnn = wa_vbfa-posnv.
          wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.

        ENDIF.

      ENDLOOP.

      IF wa_vbfa_tot-totalmenge GE 0.
        APPEND wa_vbfa_tot TO it_vbfa_tot2.

        CLEAR: wa_j_1bnflin,
               wa_vbfa,
               wa_vbap.
      ENDIF.

    ENDLOOP.

  ENDIF.

  SELECT *
    APPENDING TABLE it_zsdt0090_safra
    FROM zsdt0090
     FOR ALL ENTRIES IN it_vbap
   WHERE vbeln EQ it_vbap-vbeln
      AND posnn EQ it_vbap-posnr.

  IF sy-subrc IS INITIAL.
    SELECT *
      APPENDING TABLE it_zsdt0041_safra
      FROM zsdt0041
        FOR ALL ENTRIES IN it_zsdt0090_safra
      WHERE vbeln EQ it_zsdt0090_safra-vbelv.
  ENDIF.

  SELECT *
    FROM zsdt0082
  INTO TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_vbap
  WHERE vbeln EQ it_vbap-vbeln
    AND posnr EQ it_vbap-posnr
    AND status = 1.

  SELECT *
    FROM zsdt0275
  INTO TABLE it_zsdt0275
    FOR ALL ENTRIES IN it_zsdt0041
  WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

  SELECT *
    FROM zsdt0274
  INTO TABLE it_zsdt0274
    FOR ALL ENTRIES IN it_zsdt0040
  WHERE safra       EQ it_zsdt0040-safra
   AND cod_cultura  EQ it_zsdt0040-cultura.


  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
    FOR ALL ENTRIES IN it_zsdt0274
  WHERE nr_proposta = it_zsdt0274-nr_proposta.

  SELECT *
    FROM zsdt0273
  APPENDING TABLE it_zsdt0273
  WHERE cpf_cnpj IN rg_cpf_cnpj.

  IF it_zsdt0273[] IS NOT INITIAL.

    SELECT *
       FROM zsdt0272
    APPENDING TABLE it_zsdt0272
       FOR ALL ENTRIES IN it_zsdt0273
     WHERE nr_proposta = it_zsdt0273-nr_proposta
     AND tp_proposta   = '2'.
  ENDIF.

  IF it_zsdt0274[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0272
   APPENDING TABLE it_zsdt0272
      FOR ALL ENTRIES IN it_zsdt0274
   WHERE nr_proposta = it_zsdt0274-nr_proposta
    AND tp_proposta   = '1'.
  ENDIF.

  DELETE it_zsdt0272 WHERE estagio EQ 'Cancelado'.

  LOOP AT it_zsdt0273 .
    READ TABLE it_zsdt0272 WITH KEY nr_proposta = it_zsdt0273-nr_proposta.
    IF sy-subrc NE 0.
      DELETE it_zsdt0273 WHERE nr_proposta EQ it_zsdt0273-nr_proposta.
    ENDIF.
  ENDLOOP.

  IF r_cont IS NOT INITIAL.
    vl_vbeln = r_cont[ 1 ]-low.
  ENDIF.

  IF  r_posnr IS NOT INITIAL.
    vl_posnr = r_posnr[ 1 ]-low.
  ENDIF.

  PERFORM saida USING r_waerks.
  PERFORM remessa.

  APPEND LINES OF it_saida_sd TO t_ov.

  LOOP AT it_saida_sd2 INTO DATA(wa_saida).

    APPEND VALUE #(
                    contrato            = wa_saida-vbeln2
                    docucmento_estorno  = wa_saida-vbeln3
                    documento_fatura    = wa_saida-vbelnk
                    documento_fiscal    = wa_saida-docnum2
                    numero_nota_fiscal  = wa_saida-nfenum
                    data_emissao        = wa_saida-erdat2
                    volume_faturado     = wa_saida-qtdfatur
                    unidade_medida      = wa_saida-vrkme
                    valor_nota          = wa_saida-rfwrt2
                   ) TO t_remessa.

  ENDLOOP.

ENDFUNCTION.
