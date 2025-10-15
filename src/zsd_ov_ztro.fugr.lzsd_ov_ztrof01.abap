*----------------------------------------------------------------------*
*                                                                      *
***INCLUDE LZSD_OV_ZTROF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                         Seleciona Dados                              *
*----------------------------------------------------------------------*
FORM z_seleciona_dados USING p_tknum TYPE vttk-tknum
                             p_tdlnr TYPE vttk-tdlnr
                             p_add03 TYPE vttk-add03.

  REFRESH t_bapiret2.

* Verifica se Existe Alguma Ordem para o Trasporte
  PERFORM z_verifica_transp USING p_tknum.

  CHECK v_vbeln_a IS INITIAL.

* Seleciona Itens Transporte
  PERFORM: z_seleciona_vttp USING p_tknum,

           z_seleciona_tvkwz USING p_tdlnr,
* Seleciona Dados Fornecimentos
           z_seleciona_lips,
* Seleciona Dados da OV
           z_seleciona_vbak,
* Seleciona Itens OV
           z_seleciona_vbap,
* Seleciona Custo de Frete
           z_seleciona_vfkp USING p_tknum,
* Seleciona Dados Comerciais
           z_seleciona_vbkd,
* Seleciona Dados Parceiros
           z_seleciona_vtpa USING p_tknum,
* Seleciona Mestre Material
           z_seleciona_mara,
* Seleciona Material
           z_seleciona_zsdt0022,
*Condições (dados de operação)
           z_seleciona_konv.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTTP                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Itens Transporte                      *
*----------------------------------------------------------------------*
FORM z_seleciona_vttp USING p_tknum TYPE vttk-tknum.

  DATA: lc_msg_vt TYPE c LENGTH 200.

  REFRESH t_vttp.
  CLEAR s_vttk.

  SELECT SINGLE *
    FROM vttk
    INTO s_vttk
  WHERE  tknum EQ p_tknum.

  IF sy-subrc IS INITIAL.

    IF s_vttk-stdis IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 016 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 016 WITH s_vttk-tknum.

    ELSEIF s_vttk-streg IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 017 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 017 WITH s_vttk-tknum.

    ELSEIF s_vttk-stlbg IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 018 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 018 WITH s_vttk-tknum.

    ELSEIF s_vttk-stlad IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 019 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 019 WITH s_vttk-tknum.

    ELSEIF s_vttk-stabf IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 020 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 020 WITH s_vttk-tknum.

    ELSEIF s_vttk-sttbg IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 021 WITH s_vttk-tknum INTO lc_msg_vt.
      PERFORM z_monta_erro USING lc_msg_vt.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER 021 WITH s_vttk-tknum.

    ENDIF.

  ENDIF.

  SELECT tknum
         tpnum
         vbeln
    FROM vttp
    INTO TABLE t_vttp
  WHERE  tknum EQ p_tknum.

  SORT t_vttp BY tknum ASCENDING
                 tpnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTTP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                   Seleciona Dados Fornecimentos                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  DATA: tl_lips            TYPE TABLE OF lips,
        sl_lips            LIKE LINE OF t_lips,
        wa_zsdt_depara_cen TYPE zsdt_depara_cen,
        vg_tabix           TYPE sy-tabix.

  REFRESH: t_lips,
           t_likp,
           t_ekbe,
           t_ekko.

  CHECK NOT t_vttp[] IS INITIAL.

  SELECT *
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_vttp
  WHERE  vbeln EQ t_vttp-vbeln.

  IF s_vttk-shtyp EQ 'Z021'.

    LOOP AT t_lips INTO sl_lips.

      vg_tabix = sy-tabix.

      SELECT SINGLE * INTO wa_zsdt_depara_cen
        FROM zsdt_depara_cen
       WHERE centrov_1 EQ sl_lips-werks.

      IF sy-subrc IS INITIAL.
        s_vttk-tplst  = wa_zsdt_depara_cen-centro_real.
        sl_lips-werks = wa_zsdt_depara_cen-centro_real.
        MODIFY t_lips INDEX vg_tabix FROM sl_lips TRANSPORTING werks.
      ENDIF.

    ENDLOOP.

  ENDIF.

  SORT t_lips BY vbeln ASCENDING.

  CHECK NOT t_lips[] IS INITIAL.
  tl_lips[] = t_lips[].
  SORT tl_lips BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lips COMPARING vbeln.

  SELECT *
    FROM likp
    INTO TABLE t_likp
    FOR ALL ENTRIES IN tl_lips
  WHERE  vbeln EQ tl_lips-vbeln.

  SORT t_likp BY vbeln ASCENDING.

  CHECK s_vttk-shtyp EQ 'Z020' AND
    NOT t_likp[]     IS INITIAL.

  SELECT *
    FROM ekbe
    INTO TABLE t_ekbe
    FOR ALL ENTRIES IN t_likp
  WHERE  belnr EQ t_likp-vbeln
    AND  bewtp EQ 'L'.

  SORT t_ekbe BY belnr ASCENDING.
  CHECK NOT t_ekbe[] IS INITIAL.

  SELECT *
    FROM ekko
    INTO TABLE t_ekko
    FOR ALL ENTRIES IN t_ekbe
  WHERE  ebeln EQ t_ekbe-ebeln.

  SORT t_ekko BY ebeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIPS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAK                                         *
*&---------------------------------------------------------------------*
*                          Seleciona Dados da OV                       *
*----------------------------------------------------------------------*
FORM z_seleciona_vbak.

  REFRESH t_vbak.

  CHECK s_vttk-shtyp NE 'Z020'.
  CHECK NOT t_lips[] IS INITIAL.

  SELECT *
    FROM vbak
    INTO TABLE t_vbak
    FOR ALL ENTRIES IN t_lips
  WHERE  vbeln EQ t_lips-vgbel.

  SORT t_vbak BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBAK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAP                                         *
*&---------------------------------------------------------------------*
*                              Seleciona Itens OV                      *
*----------------------------------------------------------------------*
FORM z_seleciona_vbap.

  REFRESH t_vbap.

  CHECK s_vttk-shtyp NE 'Z020'.
  CHECK NOT t_vbak[] IS INITIAL.

  SELECT *
    FROM vbap
    INTO TABLE t_vbap
    FOR ALL ENTRIES IN t_vbak
  WHERE  vbeln EQ t_vbak-vbeln.

  SORT t_vbap BY vbeln ASCENDING
                 posnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBAP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VFKP                                         *
*&---------------------------------------------------------------------*
*                        Seleciona Custo de Frete                      *
*----------------------------------------------------------------------*
FORM z_seleciona_vfkp USING p_tknum TYPE vttk-tknum.

  REFRESH t_vfkp.

  SELECT fknum
         fkpos
         bukrs
         refty
         rebel
         repos
         kzwi1
         knumv
    FROM vfkp
    INTO TABLE t_vfkp
  WHERE  refty EQ '8' "Conversao Hana - Amaggi - WPP
    AND  rebel EQ p_tknum.

  SORT t_vfkp BY fkpos ASCENDING.

  CHECK t_vfkp[] IS INITIAL.

* Monta Msn Erro
  PERFORM z_monta_erro USING TEXT-008.
  MESSAGE i836(sd) WITH TEXT-008.

ENDFORM.                    " Z_SELECIONA_VFKP

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_OV                                                *
*&---------------------------------------------------------------------*
*                                   Cria OV                            *
*----------------------------------------------------------------------*
FORM z_cria_ov USING p_tknum TYPE vttk-tknum.

  TYPES: BEGIN OF ty_konh,
           kappl TYPE konh-kappl,
           kschl TYPE konh-kschl,
*---> 10.07.2023 15:53:19 - Migração S4 - DL
*          vakey TYPE konh-vakey,
           vakey TYPE konh_kks-vakey,
*<--- 10.07.2023 15:53:19 - Migração S4 - DL
           datbi TYPE konh-datbi,
           erdat TYPE konh-erdat,
           knumh TYPE konh-knumh,
         END OF ty_konh.

  DATA: sl_header          TYPE bapisdhd1,
        sl_headerx         TYPE bapisdhd1x,
        tl_item            TYPE TABLE OF bapisditm,
        tl_partner         TYPE TABLE OF bapiparnr,
        tl_schedules       TYPE TABLE OF bapischdl,
        tl_conditions      TYPE TABLE OF bapicond,
        tl_bapiparex       TYPE TABLE OF bapiparex,
        tl_sadrvb          TYPE TABLE OF sadrvb,
        tl_vbpavb          TYPE TABLE OF vbpavb,
        sl_vbpavb          TYPE vbpavb,
        sl_vbak            TYPE vbak,
        sl_vbap            TYPE vbap,
        sl_mvke            TYPE mvke,
        sl_vfkp            TYPE type_vfkp,
        sl_lips            TYPE lips,
        sl_tvepz           TYPE tvepz,
        sl_tvep            TYPE tvep,
        sl_likp            TYPE likp,
        sl_vttp            TYPE type_vttp,
        sl_vbkd            TYPE vbkd,
        sl_vtpa            TYPE vtpa,
        sl_marc            TYPE marc,
        sl_ekko            TYPE ekko,
        sl_ekbe            TYPE ekbe,
        sl_lfa1_sp         TYPE lfa1,
        sl_lfa1_pc         TYPE lfa1,
        sl_zsdt0022        TYPE zsdt0022,
        sl_item            TYPE bapisditm,
        sl_schedules       TYPE bapischdl,
        sl_conditions      TYPE bapicond,
        sl_partner         TYPE bapiparnr,
        sl_bapiparex       TYPE bapiparex,
        sl_t184            TYPE t184,
        sl_1bsdica         TYPE j_1bsdica,
        vl_brgew           TYPE vbap-brgew,
        vl_ntgew           TYPE vbap-ntgew,
        vl_regio_rec       TYPE t001w-regio,
        vl_regio_emi       TYPE kna1-regio,
        vl_ktokd           TYPE kna1-ktokd,
        vl_industry        TYPE j_1bindus2,
        vl_direct          TYPE j_1bdirect,
        vl_dstcat          TYPE j_1bdstcat,
        vl_bukrs           TYPE ekko-bukrs,
        sl_lips_aux        TYPE lips,
        bape_vbak          TYPE bape_vbak,
        bape_vbakx         TYPE bape_vbakx,
        vg_tabix           TYPE sy-tabix,
        wa_zsdt_depara_cen TYPE zsdt_depara_cen,
        i_xvbadr           TYPE TABLE OF sadrvb WITH HEADER LINE,
        i_xvbpa            TYPE TABLE OF vbpavb WITH HEADER LINE,
        p_parid            TYPE  j_1bparid,
        p_parid_we         TYPE  j_1bparid,
        p_parid_lr         TYPE  j_1bparid,
        p_parid_rg         TYPE  j_1bparid,
        p_parid_pc         TYPE  j_1bparid,
        p_parid_rm         TYPE  j_1bparid,
        p_parid_ag         TYPE  j_1bparid,

        sl_lfa1            TYPE  lfa1,

        t_konh             TYPE TABLE OF ty_konh,
        s_konh             TYPE ty_konh,

        t_konh_aux         TYPE TABLE OF ty_konh,
        s_konh_aux         TYPE ty_konh,
        sl_zlest0030_sel   TYPE zlest0030,

        t_konp             TYPE TABLE OF konp,
        s_konp             TYPE konp.

*---> 10.07.2023 15:53:51 - Migração S4 - DL
  DATA: var_vakey TYPE konh_kks-vakey,
*<--- 10.07.2023 15:53:51 - Migração S4 - DL
        var_tplst TYPE kunnr.

  DATA: wa_setleaf TYPE setleaf,
        it_setleaf LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: v_vbeln,
         v_fat  .

  CHECK t_bapiret2[] IS INITIAL.

  READ TABLE: t_vbak INTO sl_vbak INDEX 1,
              t_vbap INTO sl_vbap INDEX 1,
              t_vfkp INTO sl_vfkp INDEX 1,
              t_vbkd INTO sl_vbkd INDEX 1,
              t_vttp INTO sl_vttp INDEX 1,
              t_vtpa INTO sl_vtpa INDEX 1,
              t_likp INTO sl_likp INDEX 1.

  LOOP AT t_lips INTO sl_lips.
    vg_tabix  = sy-tabix.

    IF s_vttk-shtyp NE 'Z020'.

      IF sl_lips-gewei NE sl_vbap-gewei.
*       Converte Unidades
        PERFORM z_converte_unit USING sl_lips-gewei
                                      sl_vbap-gewei
                             CHANGING sl_lips-brgew.
        PERFORM z_converte_unit USING sl_lips-gewei
                                      sl_vbap-gewei
                             CHANGING sl_lips-ntgew.
      ENDIF.

    ENDIF.

    ADD: sl_lips-brgew TO vl_brgew,
         sl_lips-ntgew TO vl_ntgew.

    CLEAR sl_lips.

  ENDLOOP.

  READ TABLE t_lips INTO sl_lips INDEX 1.

  IF s_vttk-shtyp EQ 'Z020'.
    READ TABLE t_ekbe INTO sl_ekbe
       WITH KEY belnr = sl_lips-vbeln
       BINARY SEARCH.
    READ TABLE t_ekko INTO sl_ekko
      WITH KEY ebeln = sl_ekbe-ebeln
      BINARY SEARCH.
    vl_bukrs = sl_ekko-bukrs.
  ELSEIF s_vttk-shtyp EQ 'Z021'.
    vl_bukrs      = sl_vfkp-bukrs.
    sl_likp-vkorg = sl_vfkp-bukrs.
    sl_lips-vtweg = '10'.
  ELSE.
    vl_bukrs = sl_vbak-bukrs_vf.
  ENDIF.

  READ TABLE t_marc INTO sl_marc
    WITH KEY matnr = sl_lips-matnr
             werks = sl_lips-werks
    BINARY SEARCH.

  READ TABLE t_mvke INTO sl_mvke
    WITH KEY matnr = sl_lips-matnr
             vkorg = sl_likp-vkorg
             vtweg = sl_lips-vtweg
    BINARY SEARCH.

  READ TABLE t_zsdt0022 INTO sl_zsdt0022 WITH KEY mfrgr = sl_marc-mfrgr
                                                  matkl = sl_lips-matkl.


  SELECT SINGLE *
    FROM t184
    INTO sl_t184
   WHERE auart EQ sl_zsdt0022-auart
     AND mtpos EQ sl_mvke-mtpos.

  SELECT SINGLE *
   FROM j_1bsdica
   INTO sl_1bsdica
  WHERE auart  EQ sl_zsdt0022-auart
    AND pstyv  EQ sl_t184-pstyv.

  "Valida ZFRE Duplicada
*  DATA(_COUNT_ZFRE) = 0.
*  LOOP AT t_konv INTO DATA(s_konv_zfre) WHERE kschl = 'ZFRE'.
*    ADD 1 TO _COUNT_ZFRE.
*  ENDLOOP.
*
*  IF _COUNT_ZFRE > 1.
*    APPEND VALUE #( type = 'E'  message = 'Existe mais de um valor de frete cadastrado.Solicite a regularização à transportadora da sua região.' ) TO t_bapiret2.
*    EXIT.
*  ENDIF.
  "Valida ZFRE Duplicada


  DATA(_cfop_uf_emit_dif_prest) = abap_false.               "US 91605
  CLEAR: sl_lfa1_sp, sl_lfa1_pc.                            "US 91605

  IF s_vttk-shtyp EQ 'Z026'.

    READ TABLE t_lips INTO sl_lips INDEX 1.

    CALL FUNCTION 'Z_LES_TIPO_REMESSA'
      EXPORTING
        p_vbeln    = sl_lips-vbeln
      CHANGING
        p_parid_rm = p_parid
        p_parid_we = p_parid_we
        p_parid_lr = p_parid_lr
        p_parid_rg = p_parid_rg
        cfop_frete = sl_item-cfop_long
        cond_paga  = sl_item-pmnttrms
        form_paga  = sl_item-pymt_meth.

    sl_partner-partn_numb = p_parid_rg.
    sl_partner-partn_role = 'AG'.
    APPEND sl_partner TO tl_partner.

    IF NOT s_vttk-tdlnr IS INITIAL.
      sl_partner-partn_role = 'SP'.
      sl_partner-partn_numb = s_vttk-tdlnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.

    READ TABLE t_vtpa INTO sl_vtpa
      WITH KEY parvw = 'PC'
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      sl_partner-partn_role = 'PC'.
      sl_partner-partn_numb = sl_vtpa-lifnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.

    CLEAR: sl_vtpa.
    READ TABLE t_vtpa INTO sl_vtpa
      WITH KEY parvw = 'RM'
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      sl_partner-partn_role = 'RM'.
      sl_partner-partn_numb = sl_vtpa-lifnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.


  ELSE.

    CASE s_vttk-shtyp.
      WHEN: 'Z021'.

        REFRESH: tl_vbpavb[], tl_sadrvb[].
        CLEAR: sl_vbpavb, vl_regio_rec, sl_lips-werks, sl_lfa1.

        CALL FUNCTION 'SD_PARTNER_READ'
          EXPORTING
            f_vbeln  = s_vttk-tknum
            object   = 'VTPA'
          TABLES
            i_xvbadr = tl_sadrvb
            i_xvbpa  = tl_vbpavb.

        READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'PC'.

        SELECT SINGLE * FROM lfa1 INTO sl_lfa1 WHERE lifnr EQ sl_vbpavb-lifnr.

        IF sy-subrc EQ 0.
          sl_lfa1_pc = sl_lfa1.
        ENDIF.

        IF ( sl_lfa1_pc IS NOT INITIAL ) AND ( sl_lfa1_pc-ktokk NE 'ZFIC').

          READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = sl_vbpavb-kunnr
            IMPORTING
              output = sl_lips-werks.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = sl_lips-werks
            IMPORTING
              output = sl_lips-werks.

        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = sl_vbpavb-lifnr
            IMPORTING
              output = sl_lips-werks.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = sl_lips-werks
            IMPORTING
              output = sl_lips-werks.
        ENDIF.

        SELECT SINGLE regio
          FROM t001w
          INTO vl_regio_rec
        WHERE  werks EQ sl_lips-werks.

        "US 91605 - Ini
        READ TABLE tl_vbpavb INTO DATA(sl_vbpavb_sp) WITH KEY parvw = 'SP'.
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM lfa1 INTO sl_lfa1_sp WHERE lifnr EQ sl_vbpavb_sp-lifnr.
        ENDIF.
        "US 91605 - Fim


      WHEN OTHERS.

        REFRESH: tl_vbpavb[], tl_sadrvb[].
        CLEAR: sl_vbpavb, vl_regio_rec, sl_lfa1.

        CALL FUNCTION 'SD_PARTNER_READ'
          EXPORTING
            f_vbeln  = s_vttk-tknum
            object   = 'VTPA'
          TABLES
            i_xvbadr = tl_sadrvb
            i_xvbpa  = tl_vbpavb.

        READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'PC'.

        SELECT SINGLE * FROM lfa1 INTO sl_lfa1 WHERE lifnr EQ sl_vbpavb-lifnr.
        IF sy-subrc EQ 0.
          vl_regio_rec = sl_lfa1-regio.
          sl_lfa1_pc   = sl_lfa1.                           "US 91605
        ENDIF.

        "US 91605 - Ini
        READ TABLE tl_vbpavb INTO sl_vbpavb_sp WITH KEY parvw = 'SP'.
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM lfa1 INTO sl_lfa1_sp WHERE lifnr EQ sl_vbpavb_sp-lifnr.
        ENDIF.
        "US 91605 - Fim

*          SELECT SINGLE REGIO
*            FROM T001W
*            INTO VL_REGIO_REC
*          WHERE  WERKS EQ SL_LIPS-WERKS.

        "      ENDIF.
    ENDCASE.
  ENDIF.

  IF ( s_vttk-shtyp NE 'Z020' AND
       s_vttk-shtyp NE 'Z021' ).

    CALL FUNCTION 'SD_PARTNER_READ'
      EXPORTING
        f_vbeln  = sl_vbak-vbeln
        object   = 'VBPA'
      TABLES
        i_xvbadr = tl_sadrvb
        i_xvbpa  = tl_vbpavb.

  ELSE.

    CALL FUNCTION 'SD_PARTNER_READ'
      EXPORTING
        f_vbeln  = s_vttk-tknum
        object   = 'VTPA'
      TABLES
        i_xvbadr = tl_sadrvb
        i_xvbpa  = tl_vbpavb.

  ENDIF.

  "US 91605- Ini
  IF ( sl_lfa1_sp-regio IS NOT INITIAL AND sl_lfa1_pc-regio IS NOT INITIAL ) AND
     ( sl_lfa1_sp-regio NE sl_lfa1_pc-regio ).
    _cfop_uf_emit_dif_prest = abap_true.
  ENDIF.
  "US 91605- Fim

  IF s_vttk-shtyp NE 'Z026'.
    IF s_vttk-shtyp NE 'Z021' OR sl_lfa1-ktokk EQ 'ZFIC'. "ALRS 210715
      DELETE tl_vbpavb WHERE parvw NE 'Z1' AND
                             parvw NE 'LR'.
      READ TABLE tl_vbpavb INTO sl_vbpavb INDEX 1.
    ELSE.
      READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'PC'.
    ENDIF.

    "SORT TL_VBPAVB BY PARVW DESCENDING.

    IF sl_vbpavb-parvw EQ 'LR'.
      SELECT SINGLE regio ktokd
        FROM kna1
        INTO (vl_regio_emi, vl_ktokd)
      WHERE  kunnr EQ sl_vbpavb-kunnr.
    ELSE.
      SELECT SINGLE regio
        FROM lfa1
        INTO vl_regio_emi
      WHERE  lifnr EQ sl_vbpavb-lifnr.
    ENDIF.

    SELECT SINGLE industry
      FROM j_1bbranch
      INTO vl_industry
     WHERE bukrs  EQ vl_bukrs
       AND branch EQ s_vttk-tplst.

    IF vl_regio_rec EQ vl_regio_emi.
      vl_dstcat = 0.
    ELSE.
      vl_dstcat = 1.
    ENDIF.

    CLEAR: sl_zlest0030_sel.                                "US 91605

    SELECT SINGLE *
      FROM zlest0030
      INTO sl_zlest0030_sel
     WHERE direct     EQ '2'
       AND dstcat     EQ vl_dstcat
       AND industry   EQ vl_industry
       AND tpparceiro EQ '0'
       AND tdlnr      EQ s_vttk-tdlnr
       AND bukrs      EQ vl_bukrs.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE *
        FROM zlest0030
        INTO sl_zlest0030_sel "sl_item-cfop_long
       WHERE direct     EQ '2'
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ '0'
         AND tdlnr      EQ s_vttk-tdlnr
         AND bukrs      EQ space.
      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE *
          FROM zlest0030
          INTO sl_zlest0030_sel
         WHERE direct     EQ '2'
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ '0'
           AND tdlnr      EQ space
           AND bukrs      EQ vl_bukrs.

        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE *
            FROM zlest0030
            INTO sl_zlest0030_sel
           WHERE direct     EQ '2'
             AND dstcat     EQ vl_dstcat
             AND industry   EQ vl_industry
             AND tpparceiro EQ '0'
             AND tdlnr      EQ space
             AND bukrs      EQ space.
        ENDIF.
      ENDIF.
    ENDIF.


    IF _cfop_uf_emit_dif_prest EQ abap_true.

      IF sl_zlest0030_sel-cfop_uf_emit_dif_prest IS INITIAL.
        APPEND VALUE #( type = 'E'  message = 'CFOP UF Emissor Dif.Prest. não cadastrado na ZLES0028!' ) TO t_bapiret2.
        EXIT.
      ENDIF.

      sl_item-cfop_long = sl_zlest0030_sel-cfop_uf_emit_dif_prest.
    ELSE.
      sl_item-cfop_long = sl_zlest0030_sel-cfop.
    ENDIF.

  ENDIF.

* Dados Cabeçalho

  READ TABLE t_tvkwz_f INTO s_tvkwz_f INDEX 1.
  IF ( sy-subrc EQ 0 ).
    CLEAR: sl_lips_aux.
    READ TABLE t_lips INTO sl_lips_aux INDEX 1.
    SELECT SINGLE * FROM tvkwz INTO s_tvkwz_r WHERE werks EQ sl_lips_aux-werks.
    IF ( sy-subrc EQ 0 ) AND ( s_tvkwz_f-vkorg NE s_tvkwz_r-vkorg ).
      sl_header-sales_org  = s_tvkwz_f-vkorg.
      sl_header-doc_type   = 'ZTRH'.
    ELSEIF ( s_tvkwz_f-vkorg EQ s_tvkwz_r-vkorg  ) AND ( s_vttk-shtyp NE 'Z026' ).
      sl_header-sales_org  = sl_likp-vkorg.
      sl_header-doc_type   = 'ZTRO'.
    ELSEIF ( s_tvkwz_f-vkorg EQ s_tvkwz_r-vkorg  ) AND ( s_vttk-shtyp EQ 'Z026' ).
      sl_header-sales_org  = sl_likp-vkorg.
*      IF vl_ktokd = 'ZCIC'.
*        sl_header-doc_type   = 'ZTRH'.
*      ELSE.
*        sl_header-doc_type   = 'ZTRT'.
*      ENDIF.
      sl_header-doc_type   = 'ZTRT'.
    ENDIF.

  ELSE.
    sl_header-sales_org  = sl_likp-vkorg.
    sl_header-doc_type   = sl_zsdt0022-auart.
  ENDIF.

  sl_header-distr_chan = sl_lips-vtweg.
  sl_header-division   = '08'.
  sl_header-curr_iso   = 'BRL'.
  sl_header-currency   = 'BRL'.
  sl_header-created_by = sy-uname.
  IF s_vttk-shtyp NE 'Z020' AND
     s_vttk-shtyp NE 'Z021'.
    sl_header-pmnttrms   = sl_vbkd-zterm.
    sl_header-purch_no_c = sl_vbkd-bstkd.
    sl_header-price_date = sl_vbkd-prsdt.
    sl_header-exchg_rate = sl_vbkd-kursk.
  ELSEIF s_vttk-shtyp EQ 'Z021'.
    sl_header-pmnttrms   = '0001'.
    sl_header-purch_no_s = sl_likp-vbeln.
    sl_header-price_date = sy-datum.
    sl_header-exchg_rate = space.
  ELSEIF s_vttk-shtyp NE 'Z026'.
    sl_header-pmnttrms   = '0001'.
    sl_header-purch_no_c = sl_ekbe-ebeln.
    sl_header-price_date = sy-datum.
    sl_header-exchg_rate = space.
  ELSEIF s_vttk-shtyp EQ 'Z026'.
    sl_header-purch_no_c = sl_ekbe-ebeln.
    sl_header-price_date = sy-datum.
    sl_header-exchg_rate = space.
  ENDIF.

* Itens
  sl_item-itm_number   = sl_lips-posnr.
  sl_item-target_qty   = 1.
  sl_item-plant        = s_vttk-tdlnr+06(04).

  "Opter Área de contabilidade de custos
  IF s_vttk-shtyp EQ 'Z026'.
    sl_header-doc_type   = 'ZTRT'.
  ELSE.
    SELECT * INTO TABLE it_setleaf
      FROM setleaf
     WHERE setname EQ 'CTE_AVULSO'.

    IF sy-subrc EQ 0.
      IF ( sl_item-plant EQ '0116' ) OR ( sl_item-plant EQ '1510' ).
        sl_header-doc_type   = 'ZTRA'.
      ENDIF.
    ENDIF.
  ENDIF.

  "Definir qual o material utilizar.
  CLEAR: sl_zsdt0022.
  READ TABLE t_zsdt0022 INTO sl_zsdt0022 WITH KEY mfrgr = sl_marc-mfrgr
                                                  matkl = sl_lips-matkl
                                                  auart = sl_header-doc_type.

*---> 20/07/2023 - Migração S4 - DG
  "  sl_item-material     = sl_zsdt0022-matnr.
  DATA(v_len) = strlen( sl_zsdt0022-matnr ).

  IF v_len > 18.
    sl_item-material_long = CONV #( sl_zsdt0022-matnr ).
  ELSE.
    sl_item-material      = CONV #( sl_zsdt0022-matnr ).
  ENDIF.
*<--- 20/07/2023 - Migração S4 - DG



  sl_item-gross_wght   = vl_brgew.
  sl_item-net_weight   = vl_ntgew.
  sl_item-untof_wght   = sl_lips-gewei.
  sl_item-route        = s_vttk-route.
  sl_item-sd_taxcode   = 'V0'.
  sl_item-taxlawiss    = space.
  sl_item-dlvschduse   = 'T'.
  APPEND sl_item TO tl_item.

* Schedule
  sl_schedules-itm_number = sl_lips-posnr.
  sl_schedules-req_qty    = 1.
  APPEND sl_schedules TO tl_schedules.

  IF s_vttk-shtyp NE 'Z026'.
* Partner
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_vttk-tplst
      IMPORTING
        output = sl_partner-partn_numb.
    sl_partner-partn_role = 'AG'.
    APPEND sl_partner TO tl_partner.

    IF s_vttk-shtyp EQ 'Z020'.

      READ TABLE t_lips INTO DATA(w_lips) INDEX 1.

      SELECT SINGLE *
        FROM vbpa
        INTO @DATA(sl_vbpa)
        WHERE vbeln EQ @w_lips-vbeln
          AND parvw EQ 'WE'.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_branch)
         WHERE bukrs  = @vl_bukrs
           AND branch = @sl_vbpa-kunnr+6(4).
        IF sy-subrc IS INITIAL.
          DELETE tl_partner WHERE partn_role = 'AG'.
          CLEAR: sl_partner.
          sl_partner-partn_role = 'AG'.
          sl_partner-partn_numb = sl_vbpa-kunnr.
          APPEND sl_partner TO tl_partner.
        ENDIF.
      ENDIF.

*      READ TABLE T_VTPA INTO SL_VTPA WITH KEY PARVW = 'LR'.
*      IF SY-SUBRC = 0.
*        SELECT SINGLE *
*          FROM J_1BBRANCH INTO @DATA(_WL_BRANCH)
*         WHERE BUKRS  = @VL_BUKRS
*           AND BRANCH = @SL_VTPA-KUNNR+6(4).
*        IF SY-SUBRC = 0.
*          DELETE TL_PARTNER WHERE PARTN_ROLE = 'AG'.
*          CLEAR: SL_PARTNER.
*          SL_PARTNER-PARTN_ROLE = 'AG'.
*          SL_PARTNER-PARTN_NUMB = SL_VTPA-KUNNR.
*          APPEND SL_PARTNER TO TL_PARTNER.
*        ENDIF.
*      ENDIF.
    ENDIF.

    READ TABLE t_vtpa INTO sl_vtpa
      WITH KEY parvw = 'CS'
      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      sl_partner-partn_role = 'CS'.
      sl_partner-partn_numb = sl_vtpa-kunnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.

    IF NOT s_vttk-tdlnr IS INITIAL.
      sl_partner-partn_role = 'SP'.
      sl_partner-partn_numb = s_vttk-tdlnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.
  ENDIF.

  CASE sl_header-doc_type.
    WHEN: 'ZTRH' OR 'ZTRT'.
* Conditions
      sl_conditions-itm_number = sl_lips-posnr.
      sl_conditions-cond_type  = 'PR00'.

      IF NOT t_konv[] IS INITIAL.

        CLEAR: sl_conditions-cond_value.
        LOOP AT t_konv INTO s_konv.
          CASE s_konv-kschl.
            WHEN: 'ZBH1'.
              sl_conditions-cond_value = sl_conditions-cond_value + s_konv-kwert.
            WHEN: 'ZHI1'.
              sl_conditions-cond_value = sl_conditions-cond_value + s_konv-kwert.
*            WHEN: 'ZVCT'.
*              SL_CONDITIONS-COND_VALUE = SL_CONDITIONS-COND_VALUE + S_KONV-KWERT.
          ENDCASE.

          CLEAR: s_konv.
        ENDLOOP.
        sl_conditions-currency   = 'BRL'.

        IF ( sl_conditions-cond_value IS INITIAL ).
          APPEND INITIAL LINE TO t_bapiret2[] ASSIGNING FIELD-SYMBOL(<lfs_bapiret2>).
          <lfs_bapiret2>-type = 'E'.
          <lfs_bapiret2>-message = 'Valores não encontrados para condições ZBH1 e ZHI1 na TK13. Acionar Dep. Fiscal'.
        ENDIF.

        APPEND sl_conditions TO tl_conditions.
      ELSE.
        "Não encontrado preço
        MESSAGE s038(zdoceftent) INTO DATA(lc_texto).
        APPEND VALUE #( type = 'E' id = 'ZDOCEFTENT' number = '038' message = lc_texto ) TO t_bapiret2.
        EXIT.
      ENDIF.

      "Colocar ZIH1 e ZIH2 INICIO.
      CLEAR: var_vakey, var_tplst.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = s_vttk-tplst
        IMPORTING
          output = var_tplst.


      CONCATENATE sl_header-sales_org sl_header-distr_chan var_tplst sl_item-material INTO var_vakey.
      IF NOT ( var_vakey IS INITIAL ).

        SELECT kappl kschl vakey datbi erdat knumh
           FROM konh_kks
          INTO TABLE t_konh_aux
         WHERE kappl EQ 'V'
           AND kschl EQ 'ZIH1'
           AND vakey EQ var_vakey
           AND datbi >= sy-datum.

        IF ( sy-subrc EQ  0 ).

          SORT t_konh_aux BY erdat DESCENDING.

          LOOP AT t_konh_aux INTO s_konh_aux.
            vg_tabix = sy-tabix.
            IF ( vg_tabix EQ 1 ).

              s_konh-kappl = s_konh_aux-kappl.
              s_konh-kschl = s_konh_aux-kschl.
              s_konh-vakey = s_konh_aux-vakey.
              s_konh-datbi = s_konh_aux-datbi.
              s_konh-erdat = s_konh_aux-erdat.
              s_konh-knumh = s_konh_aux-knumh.

              APPEND s_konh TO t_konh.
              CLEAR: s_konh_aux, s_konh.
              CONTINUE.
            ELSE.
              DELETE t_konh_aux INDEX vg_tabix.
            ENDIF.
            CLEAR: s_konh_aux, s_konh, vg_tabix.
          ENDLOOP.

          REFRESH: t_konh_aux[].

          SELECT kappl kschl vakey datbi erdat knumh
             FROM konh_kks
            INTO TABLE t_konh_aux
           WHERE kappl EQ 'V'
             AND kschl EQ 'ZIH2'
             AND vakey EQ var_vakey
             AND datbi >= sy-datum.

          SORT t_konh_aux BY erdat DESCENDING.

          LOOP AT t_konh_aux INTO s_konh_aux.
            vg_tabix = sy-tabix.
            IF ( vg_tabix EQ 1 ).

              s_konh-kappl = s_konh_aux-kappl.
              s_konh-kschl = s_konh_aux-kschl.
              s_konh-vakey = s_konh_aux-vakey.
              s_konh-datbi = s_konh_aux-datbi.
              s_konh-erdat = s_konh_aux-erdat.
              s_konh-knumh = s_konh_aux-knumh.


              APPEND s_konh TO t_konh.
              CLEAR: s_konh_aux, s_konh.
              CONTINUE.
            ELSE.
              DELETE t_konh_aux INDEX vg_tabix.
            ENDIF.
            CLEAR: s_konh_aux, s_konh, vg_tabix.
          ENDLOOP.

        ENDIF.

        SELECT * FROM konp
          INTO TABLE t_konp
          FOR ALL ENTRIES IN t_konh
        WHERE knumh    EQ t_konh-knumh
          AND loevm_ko NE 'X'.

        IF ( sy-subrc EQ 0 ).

          LOOP AT t_konh INTO s_konh.
            sl_conditions-itm_number = sl_lips-posnr.
            CASE s_konh-kschl.
              WHEN: 'ZIH1'.
                sl_conditions-cond_type  = s_konh-kschl.
                READ TABLE t_konp INTO s_konp WITH KEY knumh = s_konh-knumh.
                sl_conditions-cond_value = s_konp-kbetr.
              WHEN: 'ZIH2'.
                sl_conditions-cond_type  = s_konh-kschl.
                READ TABLE t_konp INTO s_konp WITH KEY knumh = s_konh-knumh.
                sl_conditions-cond_value = s_konp-kbetr.
            ENDCASE.

            sl_conditions-currency   = 'BRL'.
            APPEND sl_conditions TO tl_conditions.

            CLEAR: sl_conditions, s_konh, s_konp.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "Colocar ZIH1 e ZIH2 - FIM

      "Colocar Parceiros PC e RM
      CALL FUNCTION 'Z_LES_TIPO_REMESSA'
        EXPORTING
          p_vbeln    = sl_lips-vbeln
        CHANGING
          p_parid_rm = p_parid_rm
          p_parid_pc = p_parid_pc.

      CLEAR: sl_partner.
      sl_partner-partn_role = 'RM'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_parid_rm
        IMPORTING
          output = sl_partner-partn_numb.
      APPEND sl_partner TO tl_partner.

      CLEAR: sl_partner.
      sl_partner-partn_role = 'PC'.
      sl_partner-partn_numb = p_parid_pc.
      APPEND sl_partner TO tl_partner.


    WHEN: 'ZTRO'.

      sl_conditions-itm_number = sl_lips-posnr.
      sl_conditions-cond_type  = 'PR00'.
      sl_conditions-cond_value = sl_vfkp-kzwi1.

      LOOP AT t_konv INTO s_konv.
        CASE s_konv-kschl.
          WHEN: 'ZVCT'.
            sl_conditions-cond_value =  sl_conditions-cond_value + s_konv-kwert.
        ENDCASE.
        CLEAR: s_konv.
      ENDLOOP.

      IF ( sl_conditions-cond_value IS INITIAL ).
        APPEND INITIAL LINE TO t_bapiret2[] ASSIGNING <lfs_bapiret2>.
        <lfs_bapiret2>-type = 'E'.
        <lfs_bapiret2>-message = 'Valores não encontrados para condições ZVCT na TK13. Acionar Dep. Fiscal'.
      ENDIF.

      sl_conditions-currency   = 'BRL'.
      APPEND sl_conditions TO tl_conditions.

  ENDCASE.

* Extension - Campo TKNUM
  sl_bapiparex-structure     = 'BAPE_VBAK'.
  sl_bapiparex-valuepart1+30 = p_tknum.
  APPEND sl_bapiparex TO tl_bapiparex.
  CLEAR sl_bapiparex.
  sl_bapiparex-structure     = 'BAPE_VBAKX'.
  sl_bapiparex-valuepart1+30 = 'X'.
  APPEND sl_bapiparex TO tl_bapiparex.

  "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
  "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
  CLEAR: wa_setleaf.
  SELECT SINGLE *
    FROM setleaf
    INTO wa_setleaf
   WHERE setname = 'VF01_USUARIO'
     AND valfrom = sy-uname.

  IF sy-subrc IS INITIAL.
    PERFORM busca_memoria_data_retro CHANGING sl_header-doc_date sl_header-bill_date sl_header-req_date_h.
  ENDIF.

  REFRESH: tl_vbpavb[], tl_sadrvb[].

  CALL FUNCTION 'SD_PARTNER_READ'
    EXPORTING
      f_vbeln  = s_vttk-tknum
      object   = 'VTPA'
    TABLES
      i_xvbadr = tl_sadrvb
      i_xvbpa  = tl_vbpavb.

  "Colocar Parceiros PC e RM
  CALL FUNCTION 'Z_LES_TIPO_REMESSA'
    EXPORTING
      p_vbeln    = sl_lips-vbeln
    CHANGING
      p_parid_ag = p_parid_ag
      p_parid_rm = p_parid_rm
      p_parid_we = p_parid_we.

*-CS2021001045 - 15.02.2022 - JT - inicio
  IF s_vttk-shtyp EQ 'Z021'.

    SELECT SINGLE * INTO @DATA(wa_zlest0110)
      FROM zlest0110
     WHERE vbeln EQ @sl_lips-vbeln.

    IF sy-subrc IS INITIAL.

      "Remetente
      sl_partner-partn_role = 'T1'.
      sl_partner-partn_numb = wa_zlest0110-cliente.
      APPEND sl_partner TO tl_partner.

      "Expedidor
      READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'PC'.
      sl_partner-partn_role = 'T2'.
      sl_partner-partn_numb = sl_vbpavb-lifnr.
      APPEND sl_partner TO tl_partner.

      "Recebedor de Mercadoria
      READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.
      sl_partner-partn_role = 'T3'.
      sl_partner-partn_numb = sl_vbpavb-kunnr.
      APPEND sl_partner TO tl_partner.

      SELECT SINGLE * INTO @DATA(wa_lips)
        FROM lips
       WHERE vbeln EQ @sl_lips-vbeln.

      DATA: lv_centro_real TYPE  werks_d.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = wa_lips-werks
          tp_centro_out        = 'R'
        IMPORTING
          centro_real          = lv_centro_real
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.

      "Destinatário da Carga
      sl_partner-partn_role = 'T4'.
      sl_partner-partn_numb = lv_centro_real.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_partner-partn_numb
        IMPORTING
          output = sl_partner-partn_numb.

      APPEND sl_partner TO tl_partner.

    ELSE.

      DATA: i_xvbadr_a TYPE TABLE OF sadrvb WITH HEADER LINE,
            i_xvbpa_a	 TYPE TABLE OF vbpavb WITH HEADER LINE.

      CALL FUNCTION 'SD_PARTNER_READ' " Aviso de recebimento
        EXPORTING
          f_vbeln  = sl_lips-vbeln
          object   = 'VBPA'
        TABLES
          i_xvbadr = i_xvbadr_a
          i_xvbpa  = i_xvbpa_a.

      READ TABLE i_xvbpa_a INTO DATA(wa_xvbpa_a) WITH KEY parvw = 'WL'.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE i_xvbpa_a INTO wa_xvbpa_a WITH KEY parvw = 'LF'.
      ENDIF.

      "Remetente
      sl_partner-partn_role = 'T1'.
      sl_partner-partn_numb = wa_xvbpa_a-lifnr.
      APPEND sl_partner TO tl_partner.

      "Expedidor
      sl_partner-partn_role = 'T2'.
      sl_partner-partn_numb = wa_xvbpa_a-lifnr.
      APPEND sl_partner TO tl_partner.

      "Recebedor de Mercadoria
      READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.
      sl_partner-partn_role = 'T3'.
      sl_partner-partn_numb = sl_vbpavb-kunnr.
      APPEND sl_partner TO tl_partner.

      "Recebedor de Mercadoria
      sl_partner-partn_role = 'T4'.
      sl_partner-partn_numb = sl_vbpavb-kunnr.
      APPEND sl_partner TO tl_partner.

    ENDIF.

    "Recebedor de Mercadoria
    READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.
    sl_partner-partn_role = 'T3'.
    sl_partner-partn_numb = sl_vbpavb-kunnr.
    APPEND sl_partner TO tl_partner.

  ELSEIF s_vttk-shtyp EQ 'Z009'.

    CALL FUNCTION 'SD_PARTNER_READ' " Aviso de recebimento
      EXPORTING
        f_vbeln  = sl_lips-vbeln
        object   = 'VBPA'
      TABLES
        i_xvbadr = i_xvbadr_a
        i_xvbpa  = i_xvbpa_a.

    "Cliente
    READ TABLE i_xvbpa_a INTO wa_xvbpa_a WITH KEY parvw = 'AG'.
    IF sy-subrc IS INITIAL.
      TRY .
          DATA(at_lfa1) = CAST zcl_fornecedores(
          zcl_fornecedores=>zif_parceiros~get_instance(
            )->set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj = CONV #( wa_xvbpa_a-stcd1 ) i_cpf = CONV #( wa_xvbpa_a-stcd2 ) i_insc_estatual = CONV #( wa_xvbpa_a-stcd3 )
            ) )->at_lfa1.

          "Remetente
          sl_partner-partn_role = 'T1'.
          sl_partner-partn_numb = at_lfa1-lifnr.
          APPEND sl_partner TO tl_partner.

        CATCH zcx_parceiros.
      ENDTRY.
    ENDIF.

    READ TABLE i_xvbpa_a INTO wa_xvbpa_a WITH KEY parvw = 'PC'.
    IF sy-subrc IS INITIAL.
      "Expedidor
      sl_partner-partn_role = 'T2'.
      sl_partner-partn_numb = wa_xvbpa_a-lifnr.
      APPEND sl_partner TO tl_partner.
    ENDIF.

    "Recebedor de Mercadoria
    READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.
    sl_partner-partn_role = 'T3'.
    sl_partner-partn_numb = sl_vbpavb-kunnr.
    APPEND sl_partner TO tl_partner.

    "Recebedor de Mercadoria
    sl_partner-partn_role = 'T4'.
    sl_partner-partn_numb = sl_vbpavb-kunnr.
    APPEND sl_partner TO tl_partner.

  ELSE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_parid_rm
      IMPORTING
        output = p_parid_rm.

*-CS2021001045 - 06.04.2022 - JT - inicio
    IF s_vttk-shtyp EQ 'Z005'.
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF @wa_zlest0110
        FROM zlest0213
       WHERE vbeln EQ @sl_lips-vbeln.

      IF sy-subrc EQ 0.
        "Remetente
        sl_partner-partn_role = 'T1'.
        sl_partner-partn_numb = wa_zlest0110-cliente.
        APPEND sl_partner TO tl_partner.
      ELSE.
        "Remetente
        sl_partner-partn_role = 'T1'.
        sl_partner-partn_numb = p_parid_rm.
        APPEND sl_partner TO tl_partner.
      ENDIF.
    ELSE.
      "Remetente
      sl_partner-partn_role = 'T1'.
      sl_partner-partn_numb = p_parid_rm.
      APPEND sl_partner TO tl_partner.
    ENDIF.
*-CS2021001045 - 06.04.2022 - JT - fim

    "CS2021000278  - Parceiro PC
    CALL FUNCTION 'SD_PARTNER_READ' " Aviso de recebimento
      EXPORTING
        f_vbeln  = sl_lips-vbeln
        object   = 'VBPA'
      TABLES
        i_xvbadr = i_xvbadr_a
        i_xvbpa  = i_xvbpa_a.

    "Expedidor
    READ TABLE i_xvbpa_a INTO wa_xvbpa_a WITH KEY parvw = 'PC'.
    IF sy-subrc = 0.
      sl_partner-partn_role = 'T2'.
      sl_partner-partn_numb = wa_xvbpa_a-lifnr. "p_parid_rm.
      APPEND sl_partner TO tl_partner.
    ENDIF.
    "CS2021000278  - Parceiro PC


    "Recebedor de Mercadoria
    READ TABLE tl_vbpavb INTO sl_vbpavb WITH KEY parvw = 'LR'.
    sl_partner-partn_role = 'T3'.
    sl_partner-partn_numb = sl_vbpavb-kunnr.
    APPEND sl_partner TO tl_partner.

    "Destinatário da Carga
    sl_partner-partn_role = 'T4'.
    sl_partner-partn_numb = p_parid_we.
    APPEND sl_partner TO tl_partner.

  ENDIF.

  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      order_header_in     = sl_header
    IMPORTING
      salesdocument       = v_vbeln
    TABLES
      return              = t_bapiret2
      order_items_in      = tl_item
      order_partners      = tl_partner
      order_schedules_in  = tl_schedules
      order_conditions_in = tl_conditions
      extensionin         = tl_bapiparex.

  IF NOT v_vbeln IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CALL FUNCTION 'ZSD_FAT_ZTRO'
      EXPORTING
        p_sales  = v_vbeln
      IMPORTING
        p_fat    = v_fat
        t_return = t_bapiret2.
  ENDIF.

ENDFORM.                    " Z_CRIA_OV

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBKD                                         *
*&---------------------------------------------------------------------*
*                        Seleciona Dados Comerciais                    *
*----------------------------------------------------------------------*
FORM z_seleciona_vbkd.

  REFRESH t_vbkd.

  CHECK s_vttk-shtyp NE 'Z020'.
  CHECK NOT t_vbak[] IS INITIAL.

  SELECT *
    FROM vbkd
    INTO TABLE t_vbkd
    FOR ALL ENTRIES IN t_vbak
  WHERE  vbeln EQ t_vbak-vbeln.

  SORT t_vbkd BY vbeln ASCENDING
                 posnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBKD

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTPA                                         *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Parceiros                      *
*----------------------------------------------------------------------*
FORM z_seleciona_vtpa USING p_tknum TYPE vttk-tknum.

  REFRESH t_vtpa.

  SELECT *
    FROM vtpa
    INTO TABLE t_vtpa
  WHERE  vbeln EQ p_tknum.

  SORT t_vtpa BY parvw ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTPA

*&---------------------------------------------------------------------*
*&      Form  Z_INSERE_BDC                                             *
*&---------------------------------------------------------------------*
*                             Insere BDC                               *
*----------------------------------------------------------------------*
FORM z_insere_bdc USING p_dynbegin TYPE any
                        p_field    TYPE any
                        p_value    TYPE any.

  DATA sl_bdc TYPE bdcdata.

  CLEAR sl_bdc.

  IF p_dynbegin EQ 'X'.
    sl_bdc-dynbegin = 'X'.
    sl_bdc-program  = p_field.
    sl_bdc-dynpro   = p_value.
  ELSE.
    sl_bdc-fnam = p_field.
    sl_bdc-fval = p_value.
  ENDIF.

  APPEND sl_bdc TO t_bdc.

ENDFORM.                    " Z_INSERE_BDC

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_TRANSP                                        *
*&---------------------------------------------------------------------*
*       Verifica se Existe Alguma Ordem para o Trasporte               *
*----------------------------------------------------------------------*
FORM z_verifica_transp USING p_tknum TYPE vttk-tknum.

  CLEAR: v_vbeln_a,
         v_fat    .

  SELECT SINGLE vbeln
    FROM vbak
    INTO v_vbeln_a
  WHERE  tknum EQ p_tknum.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE vbeln
    FROM lips
    INTO v_fat
  WHERE  vgbel EQ v_vbeln_a.

ENDFORM.                    " Z_VERIFICA_TRANSP

*&---------------------------------------------------------------------*
*&      Form  Z_RET_MSN                                                *
*&---------------------------------------------------------------------*
*                          Retorna Menssagens                          *
*----------------------------------------------------------------------*
FORM z_ret_msn USING p_sem_mensagem TYPE char01.

  DATA vl_texto TYPE char50.

  IF NOT v_vbeln_a IS INITIAL.
    IF v_fat IS INITIAL.
      MESSAGE i836(sd) WITH TEXT-006 v_vbeln_a.
    ELSE.
      MESSAGE i836(sd) WITH TEXT-006 v_vbeln_a TEXT-004 v_fat.
    ENDIF.
    v_vbeln = v_vbeln_a.
    EXIT.
  ENDIF.

  IF NOT v_vbeln IS INITIAL.
    IF v_fat IS INITIAL AND vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
      MESSAGE i836(sd) WITH TEXT-002 v_vbeln TEXT-005 TEXT-003.
    ELSEIF ( sy-tcode NE 'ZLES0106' ) AND
           ( sy-tcode NE 'ZLES0113' ) AND
           ( sy-tcode NE 'ZLES0115' ) AND
           ( sy-tcode NE 'ZLES0136' ) AND
           ( sy-tcode NE 'ZMM0127'  ) AND
           ( vg_faturamento_autom = abap_false ) AND  "*-#133089-21.02.2024-JT
           ( p_sem_mensagem EQ abap_false ).
      CONCATENATE v_fat TEXT-005 INTO vl_texto SEPARATED BY space.
      MESSAGE i836(sd) WITH TEXT-002 v_vbeln TEXT-004 vl_texto.
    ENDIF.
  ELSEIF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    MESSAGE i836(sd) WITH TEXT-001.
  ENDIF.

ENDFORM.                    " Z_RET_MSN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MARA                                         *
*&---------------------------------------------------------------------*
*                       Seleciona Mestre Material                      *
*----------------------------------------------------------------------*
FORM z_seleciona_mara.

  REFRESH: t_marc,
           t_mvke.

  CHECK NOT t_lips[] IS INITIAL.

  SELECT *
    FROM marc
    INTO TABLE t_marc
    FOR ALL ENTRIES IN t_lips
  WHERE  matnr EQ t_lips-matnr
    AND  werks EQ t_lips-werks.

  SORT t_marc BY matnr ASCENDING
                 werks ASCENDING.

  SELECT *
    FROM mvke
    INTO TABLE t_mvke
    FOR ALL ENTRIES IN t_lips
  WHERE  matnr EQ t_lips-matnr.

  SORT t_mvke BY matnr ASCENDING
                 vkorg ASCENDING
                 vtweg ASCENDING.

ENDFORM.                    " Z_SELECIONA_MARA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0022                                     *
*&---------------------------------------------------------------------*
*                          Seleciona ZSDT0022                          *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0022.

  REFRESH t_zsdt0022.

  CHECK NOT t_marc[] IS INITIAL.

  SELECT *
    FROM zsdt0022
    INTO TABLE t_zsdt0022
    FOR ALL ENTRIES IN t_marc
  WHERE  mfrgr EQ t_marc-mfrgr.

  SORT t_zsdt0022 BY mfrgr ASCENDING
                     auart ASCENDING.

  IF t_zsdt0022[] IS INITIAL.
*   Monta Msn Erro
    PERFORM z_monta_erro USING TEXT-009.
    MESSAGE i836(sd) WITH TEXT-007.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_ZSDT0022

*&---------------------------------------------------------------------*
*&      Form  Z_CONVERTE_UNIT                                          *
*&---------------------------------------------------------------------*
*                            Converte Unidades                         *
*----------------------------------------------------------------------*
FORM z_converte_unit USING p_lips_gewei TYPE lips-gewei
                           p_vbap_gewei TYPE lips-gewei
                  CHANGING p_peso       TYPE lips-brgew.

  DATA: vl_mgvgw TYPE plfh-mgvgw,
        vl_in    TYPE plfh-mgvgw.

  vl_in = p_peso.

  CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
    EXPORTING
      unit_new_imp  = p_vbap_gewei
      unit_old_imp  = p_lips_gewei
      value_old_imp = vl_in
    IMPORTING
      value_new_exp = vl_mgvgw
    EXCEPTIONS
      overflow      = 1
      OTHERS        = 2.

  p_peso = vl_mgvgw.

ENDFORM.                    " Z_CONVERTE_UNIT

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                             Monta Msn Erro                           *
*----------------------------------------------------------------------*
FORM z_monta_erro USING p_text TYPE c.

  DATA sl_bapiret2 TYPE bapiret2.

  sl_bapiret2-type    = 'E'.
  sl_bapiret2-message = p_text.

  APPEND sl_bapiret2 TO t_bapiret2.

ENDFORM.                    " Z_MONTA_ERRO
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVKWZ
*&---------------------------------------------------------------------*
FORM z_seleciona_tvkwz USING p_tdlnr TYPE vttk-tdlnr.


  DATA: var_werks TYPE tvkwz-werks.
  REFRESH: t_tvkwz_f[].

  CLEAR: s_tvkwz_f, var_werks.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_tdlnr
    IMPORTING
      output = var_werks.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = var_werks
    IMPORTING
      output = var_werks.


  SELECT * FROM tvkwz
    INTO TABLE t_tvkwz_f
  WHERE werks EQ var_werks.

ENDFORM.                    " Z_SELECIONA_TVKWZ
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KONV
*&---------------------------------------------------------------------*
FORM z_seleciona_konv .

  DATA: vl_kinak LIKE konv-kinak.


*---> 19/07/2023 - Migração S4 - DG
*  SELECT * FROM konv
*    INTO TABLE t_konv
*    FOR ALL ENTRIES IN t_vfkp
*  WHERE knumv EQ t_vfkp-knumv
*    AND kschl IN ('ZFRE','ZADH','ZBH1','ZHI1','ZVCT')
*    AND kinak EQ vl_kinak.

  SELECT * FROM v_konv
    INTO CORRESPONDING FIELDS OF TABLE @t_konv
    FOR ALL ENTRIES IN @t_vfkp
  WHERE knumv EQ @t_vfkp-knumv
    AND kschl IN ('ZFRE','ZADH','ZBH1','ZHI1','ZVCT')
    AND kinak EQ @vl_kinak.
*<--- 19/07/2023 - Migração S4 - DG
ENDFORM.                    " Z_SELECIONA_KONV

*&---------------------------------------------------------------------*
*&      Form  BUSCA_MEMORIA_DATA_RETRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_memoria_data_retro  CHANGING date_1 TYPE audat
                                        date_2 TYPE audat
                                        date_3 TYPE audat.

  DATA: id TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  IMPORT p1 = date_1 FROM MEMORY ID id.
  IF date_1 IS NOT INITIAL.
    date_2 = date_1.
    date_3 = date_1.
  ENDIF.

ENDFORM.                    " BUSCA_MEMORIA_DATA_RETRO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_MEMORIA_DATA_RETRO_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_DATA  text
*----------------------------------------------------------------------*
FORM busca_memoria_data_retro_2  CHANGING p_vl_data.

  DATA: id     TYPE c LENGTH 10 VALUE 'ROMRETRO',
        date_1 TYPE audat.

  IMPORT p1 = date_1 FROM MEMORY ID id.
  IF date_1 IS NOT INITIAL.
    CONCATENATE date_1+6(2) '.' date_1+4(2) '.' date_1(4) INTO p_vl_data.
  ENDIF.

ENDFORM.                    " BUSCA_MEMORIA_DATA_RETRO_2
