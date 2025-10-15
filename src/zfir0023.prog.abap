**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + ?????                                                                |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|    + Welgem Barbosa  ( welgem.barbosa@amaggi.com.br )                     |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatorio de Movimento Contábeis Ativo Imobilizado                        |*
**/===========================================================================\*


REPORT  zfir0023.
TYPE-POOLS: slis.

**********************************************************************
*  Declaracao de Estrutura                                           *
**********************************************************************
TABLES: faglflexa, bseg.
TYPES: BEGIN OF ty_faglflexa,
         ryear  TYPE faglflexa-ryear,
         rbukrs TYPE faglflexa-rbukrs,
         belnr  TYPE faglflexa-belnr,
         buzei  TYPE faglflexa-buzei,
         tsl    TYPE faglflexa-tsl,
         ksl    TYPE faglflexa-ksl,
         osl    TYPE faglflexa-osl,
         activ  TYPE faglflexa-activ,
         racct  TYPE faglflexa-racct,
         budat  TYPE faglflexa-budat,
         awtyp  TYPE faglflexa-awtyp,
*        ryear  type faglflexa-ryear,
       END OF ty_faglflexa,

       BEGIN OF ty_bseg,
         belnr TYPE bseg-belnr,
         buzei TYPE bseg-buzei,
         bukrs TYPE bseg-bukrs,
         gjahr TYPE bseg-gjahr,
         gsber TYPE bseg-gsber,
         sgtxt TYPE bseg-sgtxt,
         anln1 TYPE bseg-anln1,
         anln2 TYPE bseg-anln2,
         matnr TYPE bseg-matnr,
         werks TYPE bseg-werks,
         menge TYPE bseg-menge,
         meins TYPE bseg-meins,
         ebeln TYPE bseg-ebeln,
         ebelp TYPE bseg-ebelp,
         bewar TYPE bseg-bewar,
         kokrs TYPE bseg-kokrs,
         zuonr TYPE rbkp-zuonr,
         altkt TYPE bseg-altkt,
       END OF ty_bseg,

       BEGIN OF ty_ttypt,
         awtyp TYPE ttypt-awtyp,
         otext TYPE ttypt-otext,
       END OF ty_ttypt,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_anlz,
         bukrs TYPE anlz-bukrs,
         anln1 TYPE anlz-anln1,
         anln2 TYPE anlz-anln2,
         kostl TYPE anlz-kostl,
       END OF ty_anlz,

       BEGIN OF ty_anla,
         bukrs   TYPE anla-bukrs,
         anln1   TYPE anla-anln1,
         anln2   TYPE anla-anln2,
         txt50_1 TYPE anla-txt50,
         txa50   TYPE anla-txa50,
       END OF ty_anla,

       BEGIN OF ty_anlh,
         bukrs   TYPE anlh-bukrs,
         anln1   TYPE anlh-anln1,
         anlhtxt TYPE anlh-anlhtxt,
       END OF ty_anlh,

       BEGIN OF ty_anlb,
         bukrs TYPE anlb-bukrs,
         anln1 TYPE anlb-anln1,
         anln2 TYPE anlb-anln2,
         afabg TYPE anlb-afabg,
       END OF ty_anlb,

       BEGIN OF ty_skat,
         saknr TYPE skat-saknr,
         txt50 TYPE skat-txt50,
         spras TYPE skat-spras,
         altkt TYPE bseg-altkt,
       END OF ty_skat,

*       BEGIN OF TY_RBKP,
*         GJAHR TYPE RBKP-GJAHR,
*         BUDAT TYPE RBKP-BUDAT,
**         ZUONR TYPE RBKP-ZUONR,
*         XBLNR TYPE RBKP-XBLNR,
*         BELNR TYPE RBKP-BELNR,
*         BLDAT TYPE RBKP-BLDAT,
*         LIFNR TYPE RBKP-LIFNR,
*       END OF TY_RBKP,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_t022t,
         activity TYPE t022t-activity,
         langu    TYPE t022t-langu,
         txt      TYPE t022t-txt,
       END OF ty_t022t,

       BEGIN OF ty_t856t,
         trtyp TYPE t856t-trtyp,
         txt   TYPE t856t-txt,
       END OF ty_t856t,

       BEGIN OF ty_cskt,
         kostl TYPE cskt-kostl,
         ltext TYPE cskt-ltext,
       END OF ty_cskt,

       BEGIN OF ty_saida,
         bukrs     TYPE bseg-bukrs,
         gsber     TYPE bseg-gsber,
         kostl     TYPE anlz-kostl,
         ltext     TYPE cskt-ltext,
         ebeln     TYPE bseg-ebeln,
         ebelp     TYPE bseg-ebelp,
         xblnr     TYPE rbkp-xblnr,
         lifnr     TYPE rbkp-lifnr,
         name1     TYPE lfa1-name1,
         racct     TYPE faglflexa-racct,
         txt50     TYPE skat-txt50,
         anln1     TYPE bseg-anln1,
         anln2     TYPE bseg-anln2,
         txt50_1   TYPE anla-txt50,
         txa50     TYPE anla-txa50,
         anlhtxt   TYPE anlh-anlhtxt,
         belnr     TYPE bseg-belnr,
         bldat     TYPE rbkp-bldat,
         afabg     TYPE anlb-afabg,
         budat     TYPE faglflexa-budat,
         tsl       TYPE faglflexa-tsl,
         ksl       TYPE faglflexa-ksl,
         osl       TYPE faglflexa-osl,
         sgtxt     TYPE bseg-sgtxt,
         matnr     TYPE bseg-matnr,
         maktx     TYPE makt-maktx,
         menge     TYPE bseg-menge,
         meins     TYPE bseg-meins,
         werks     TYPE bseg-werks,
         txt       TYPE t022t-txt,
         otext     TYPE ttypt-otext,
         bewar     TYPE bseg-bewar,
         txt_bewar TYPE t856t-txt,
       END OF ty_saida.

**********************************************************************
*  Declaracoes de Variaveis/Tabelas/Workareas                        *
**********************************************************************
DATA: tg_faglflexa    TYPE TABLE OF ty_faglflexa,
      tg_bseg         TYPE TABLE OF ty_bseg,
      tg_makt         TYPE TABLE OF ty_makt,
      tg_ttypt        TYPE TABLE OF ty_ttypt,
      tg_anlb         TYPE TABLE OF ty_anlb,
      tg_anlz         TYPE TABLE OF ty_anlz,
      tg_anla         TYPE TABLE OF ty_anla,
      tg_anlh         TYPE TABLE OF ty_anlh,
      tg_skat         TYPE TABLE OF ty_skat,
      tg_skat_aux     TYPE TABLE OF ty_skat,
*      TG_RBKP      TYPE TABLE OF TY_RBKP,
      tg_rbkp         TYPE TABLE OF rbkp,
      it_bkpf         TYPE TABLE OF bkpf,
      it_bkpf_aux     TYPE TABLE OF bkpf,
      tg_lfa1         TYPE TABLE OF ty_lfa1,
      tg_t022t        TYPE TABLE OF ty_t022t,
      tg_t856t        TYPE TABLE OF ty_t856t,
      tg_cskt         TYPE TABLE OF ty_cskt,
      tg_saida_aux    TYPE TABLE OF ty_saida,
      tg_saida        TYPE TABLE OF ty_saida,
      it_fagl_splinfo TYPE TABLE OF fagl_splinfo,
      it_mseg         TYPE TABLE OF mseg,
      tabix           TYPE sy-tabix,

      wa_fagl_splinfo TYPE fagl_splinfo,
      wa_mseg         TYPE mseg,
      wg_faglflexa    TYPE ty_faglflexa,
      wg_bseg         TYPE ty_bseg,
      wg_makt         TYPE ty_makt,
      wg_ttypt        TYPE ty_ttypt,
      wg_anlb         TYPE ty_anlb,
      wg_anlh         TYPE ty_anlh,
      wg_anlz         TYPE ty_anlz,
      wg_anla         TYPE ty_anla,
      wg_skat         TYPE ty_skat,
      wg_skat_aux     TYPE ty_skat,
*      WG_RBKP      TYPE TY_RBKP,
      wg_rbkp         TYPE rbkp,
      wa_lin          TYPE ty_bseg,
      wa_bkpf         TYPE bkpf,
      wa_bkpf_aux     TYPE bkpf,
      wg_lfa1         TYPE ty_lfa1,
      wg_t022t        TYPE ty_t022t,
      wg_t856t        TYPE ty_t856t,
      wg_cskt         TYPE ty_cskt,
      wg_saida_aux    TYPE ty_saida,
      wg_saida        TYPE ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

**********************************************************************
*  Tela de selecao                                                   *
**********************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  s_bukrs FOR faglflexa-rbukrs OBLIGATORY,
                   s_gsber FOR bseg-gsber,
                   s_anln1 FOR bseg-anln1,
                   s_anln2 FOR bseg-anln2,
                   s_kostl FOR bseg-kostl,
                   s_racct FOR faglflexa-racct OBLIGATORY,
                   s_budat FOR faglflexa-budat OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.
  PERFORM imprimir_dados.

FORM seleciona_dados.

  SELECT ryear rbukrs belnr buzei hsl ksl osl activ racct
         budat awtyp
    FROM faglflexa
    INTO TABLE tg_faglflexa
     WHERE rbukrs IN s_bukrs
       AND racct IN s_racct
       AND budat IN s_budat
       AND rldnr EQ '0L'.

  IF sy-subrc IS INITIAL.

    SELECT awtyp otext
      FROM ttypt
      INTO TABLE tg_ttypt
       FOR ALL ENTRIES IN tg_faglflexa
        WHERE awtyp EQ tg_faglflexa-awtyp
          AND spras EQ sy-langu.

* ---> S4 Migration - 15/06/2023 - MA
*    SELECT BELNR BUZEI BUKRS GJAHR GSBER SGTXT ANLN1
*           ANLN2 MATNR WERKS MENGE MEINS EBELN EBELP BEWAR KOKRS EBELN ALTKT
*      FROM BSEG
*      INTO TABLE TG_BSEG
*       FOR ALL ENTRIES IN TG_FAGLFLEXA
*        WHERE BELNR EQ TG_FAGLFLEXA-BELNR
*          AND BUZEI EQ TG_FAGLFLEXA-BUZEI
*          AND BUKRS EQ TG_FAGLFLEXA-RBUKRS
*          AND GSBER IN S_GSBER.

    DATA lt_fields TYPE fagl_t_field.
    DATA: lt_bseg TYPE TABLE OF bseg,
          t_bseg  TYPE TABLE OF bseg.

    lt_fields = VALUE #( ( line = 'BELNR' )
                         ( line = 'BUZEI' )
                         ( line = 'BUKRS' )
                         ( line = 'GJAHR' )
                         ( line = 'GSBER' )
                         ( line = 'SGTXT' )
                         ( line = 'ANLN1' )
                         ( line = 'ANLN2' )
                         ( line = 'MATNR' )
                         ( line = 'WERKS' )
                         ( line = 'MENGE' )
                         ( line = 'MEINS' )
                         ( line = 'EBELN' )
                         ( line = 'EBELP' )
                         ( line = 'BEWAR' )
                         ( line = 'KOKRS' )
                         ( line = 'EBELN' )
                         ( line = 'ALTKT' )
                         ).

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = tg_faglflexa
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-RBUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND BUZEI = IT_FOR_ALL_ENTRIES-BUZEI|
        it_fieldlist       = lt_fields
      IMPORTING
        et_bseg            = lt_bseg
      EXCEPTIONS
        not_found          = 1.

    DELETE lt_bseg WHERE gsber NOT IN s_gsber.

*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
*    if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
    IF lines( lt_bseg ) > 0.
      sy-subrc = 0.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024
      MOVE-CORRESPONDING lt_bseg TO tg_bseg.
      sy-dbcnt = lines( lt_bseg ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.

*<--- S4 Migration - 15/06/2023 - MA
    IF sy-subrc IS INITIAL.

      SELECT bukrs anln1 anln2 kostl
        FROM anlz
        INTO TABLE tg_anlz
        FOR ALL ENTRIES IN tg_bseg
         WHERE bukrs EQ tg_bseg-bukrs
           AND anln1 EQ tg_bseg-anln1
           AND anln2 EQ tg_bseg-anln2.

      IF tg_anlz[] IS NOT INITIAL.

        SELECT kostl ltext
          FROM cskt
          INTO TABLE tg_cskt
          FOR ALL ENTRIES IN tg_anlz
          WHERE spras = 'P'
          AND   kokrs = 'MAGI'
          AND   kostl = tg_anlz-kostl.

        SELECT bukrs anln1 anln2 afabg
        FROM anlb
        INTO TABLE tg_anlb
        FOR ALL ENTRIES IN tg_anlz
         WHERE bukrs EQ tg_anlz-bukrs
           AND anln1 EQ tg_anlz-anln1
           AND anln2 EQ tg_anlz-anln2.

        SELECT bukrs anln1 anln2 txt50 txa50
          FROM anla
          INTO TABLE tg_anla
          FOR ALL ENTRIES IN tg_anlz
          WHERE bukrs EQ tg_anlz-bukrs
            AND anln1 EQ tg_anlz-anln1
            AND anln2 EQ tg_anlz-anln2.

        SELECT bukrs anln1 anlhtxt
          FROM anlh
          INTO TABLE tg_anlh
          FOR ALL ENTRIES IN tg_anlz
          WHERE bukrs EQ tg_anlz-bukrs
            AND anln1 EQ tg_anlz-anln1.

      ENDIF.

*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
      IF tg_bseg[] IS NOT INITIAL.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024

        SELECT saknr txt50 spras
          FROM skat
          INTO TABLE tg_skat
          FOR ALL ENTRIES IN tg_bseg
          WHERE ktopl EQ '0050'
            AND saknr EQ tg_bseg-altkt
            AND spras EQ sy-langu.
        MOVE: tg_skat[] TO tg_skat_aux[].

        SELECT *
          FROM bkpf
          INTO TABLE it_bkpf
          FOR ALL ENTRIES IN tg_bseg
          WHERE bukrs EQ tg_bseg-bukrs
            AND belnr EQ tg_bseg-belnr
            AND gjahr EQ tg_bseg-gjahr.

*
*      SELECT GJAHR BUDAT ZUONR XBLNR BLDAT LIFNR
*        FROM RBKP
*        INTO TABLE TG_RBKP
*        FOR ALL ENTRIES IN TG_BSEG
*         WHERE GJAHR EQ TG_BSEG-GJAHR
*           AND BUDAT IN S_BUDAT
*           AND ZUONR EQ TG_BSEG-ZUONR.

*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024
      ENDIF.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024
      LOOP AT tg_bseg INTO wa_lin.

        READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_lin-bukrs
                                                 belnr = wa_lin-belnr
                                                 gjahr = wa_lin-gjahr.
        tabix = sy-tabix.
        IF wa_bkpf-awtyp EQ 'MKPF'.
          wa_bkpf-arcid = wa_bkpf-awkey(10).
          wa_bkpf-gjahr = wa_bkpf-gjahr.
          MODIFY it_bkpf FROM wa_bkpf INDEX tabix TRANSPORTING arcid gjahr.
        ELSE.
          wa_bkpf-arcid = wa_bkpf-awkey(10).
          MODIFY it_bkpf FROM wa_bkpf INDEX tabix TRANSPORTING arcid.
        ENDIF.

      ENDLOOP.
*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
      IF it_bkpf[] IS NOT INITIAL.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024

        MOVE it_bkpf[] TO it_bkpf_aux[].
        DELETE it_bkpf_aux WHERE awtyp NE 'MKPF'.

        SELECT *
        FROM rbkp
        INTO TABLE tg_rbkp
        FOR ALL ENTRIES IN it_bkpf
         WHERE gjahr EQ it_bkpf-gjahr
           AND belnr EQ it_bkpf-arcid.
*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
      ENDIF.
      IF it_bkpf_aux[] IS NOT INITIAL.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024
        SELECT *
          FROM mseg
          INTO TABLE it_mseg
          FOR ALL ENTRIES IN it_bkpf_aux
          WHERE mjahr EQ it_bkpf_aux-gjahr
            AND mblnr EQ it_bkpf_aux-arcid
            AND bukrs EQ it_bkpf_aux-bukrs.

        IF sy-subrc IS INITIAL.
          SELECT lifnr name1
            FROM lfa1
            INTO TABLE tg_lfa1
             FOR ALL ENTRIES IN tg_rbkp
              WHERE lifnr EQ tg_rbkp-lifnr.

          SELECT lifnr name1
           FROM lfa1
           APPENDING TABLE tg_lfa1
            FOR ALL ENTRIES IN it_mseg
             WHERE lifnr EQ it_mseg-lifnr.

        ENDIF.
*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
      ENDIF.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024

      SELECT matnr maktx
        FROM makt
        INTO TABLE tg_makt
         FOR ALL ENTRIES IN tg_bseg
          WHERE matnr EQ tg_bseg-matnr.

      SELECT trtyp txt
        FROM t856t
        INTO TABLE tg_t856t
         FOR ALL ENTRIES IN tg_bseg
          WHERE trtyp EQ tg_bseg-bewar
            AND langu EQ sy-langu.

    ENDIF.

*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
    IF tg_faglflexa[] IS NOT INITIAL.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024
      SELECT saknr txt50
          FROM skat
          INTO TABLE tg_skat
           FOR ALL ENTRIES IN tg_faglflexa
           WHERE saknr EQ tg_faglflexa-racct
             AND spras EQ sy-langu.

      SELECT activity langu txt
        FROM t022t
        INTO TABLE tg_t022t
         FOR ALL ENTRIES IN tg_faglflexa
         WHERE activity EQ tg_faglflexa-activ
           AND langu EQ sy-langu.
*---> Ini - Ajuste imobilizado - 2000002318/IR160075 - Stefanini - 12/03/2024
    ENDIF.
*<--- Fim - 2000002318/IR160075 - Stefanini - 12/03/2024

  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

FORM organiza_dados .

*  DELETE TG_RBKP WHERE ZUONR IS INITIAL.

  SORT: tg_bseg BY belnr buzei bukrs,
        tg_makt BY matnr,
        tg_ttypt BY awtyp,
        tg_anlz BY bukrs anln1 anln2,
        tg_anla BY bukrs anln1 anln2,
        tg_anlb BY bukrs anln1 anln2,
        tg_anlh BY bukrs anln1,
        tg_lfa1 BY lifnr,
        tg_skat BY saknr,
        tg_t022t BY activity,
        tg_t856t BY trtyp,
        tg_cskt  BY kostl.

  LOOP AT tg_faglflexa INTO wg_faglflexa.
    READ TABLE tg_bseg INTO wg_bseg
      WITH KEY belnr = wg_faglflexa-belnr
               buzei = wg_faglflexa-buzei
               bukrs = wg_faglflexa-rbukrs
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_anlz INTO wg_anlz
        WITH KEY bukrs = wg_bseg-bukrs
                 anln1 = wg_bseg-anln1
                 anln2 = wg_bseg-anln2
                 BINARY SEARCH.

      READ TABLE tg_anla INTO wg_anla
        WITH KEY bukrs = wg_bseg-bukrs
                 anln1 = wg_bseg-anln1
                 anln2 = wg_bseg-anln2
                 BINARY SEARCH.

      READ TABLE tg_anlb INTO wg_anlb
        WITH KEY bukrs = wg_bseg-bukrs
                 anln1 = wg_bseg-anln1
                 anln2 = wg_bseg-anln2
                 BINARY SEARCH.

      READ TABLE tg_anlh INTO wg_anlh
        WITH KEY bukrs = wg_bseg-bukrs
                 anln1 = wg_bseg-anln1
                 BINARY SEARCH.

      READ TABLE tg_makt INTO wg_makt
        WITH KEY matnr = wg_bseg-matnr
                 BINARY SEARCH.

      READ TABLE tg_ttypt INTO wg_ttypt
        WITH KEY awtyp = wg_faglflexa-awtyp
                 BINARY SEARCH.

      READ TABLE tg_skat INTO wg_skat
            WITH KEY saknr = wg_faglflexa-racct
                     BINARY SEARCH.

      READ TABLE tg_t022t INTO wg_t022t
        WITH KEY activity = wg_faglflexa-activ
                    BINARY SEARCH.

      READ TABLE tg_t856t INTO wg_t856t
         WITH KEY trtyp = wg_bseg-bewar
                     BINARY SEARCH.

      READ TABLE tg_cskt  INTO wg_cskt WITH KEY kostl = wg_anlz-kostl BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE wg_cskt-ltext  TO wg_saida-ltext.
      ENDIF.

      READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wg_bseg-bukrs
                                               belnr = wg_bseg-belnr
                                               gjahr = wg_bseg-gjahr.
      wa_bkpf-arcid = wa_bkpf-awkey(10).

      IF wa_bkpf-awtyp EQ 'MKPF'.

        READ TABLE it_bkpf_aux INTO wa_bkpf_aux
          WITH KEY  bukrs = wg_bseg-bukrs
                    belnr = wg_bseg-belnr
                    gjahr = wg_bseg-gjahr.

        wa_bkpf_aux-arcid = wa_bkpf_aux-awkey(10).
        wa_bkpf_aux-gjahr = wa_bkpf_aux-gjahr.

        READ TABLE it_mseg INTO wa_mseg
          WITH KEY mjahr = wa_bkpf_aux-gjahr
                   mblnr = wa_bkpf_aux-arcid
                   bukrs = wa_bkpf_aux-bukrs.



        IF sy-subrc IS INITIAL.
          READ TABLE tg_lfa1 INTO wg_lfa1
            WITH KEY lifnr = wa_mseg-lifnr
                     BINARY SEARCH.
        ENDIF.
        MOVE: wa_mseg-lifnr TO wg_saida-lifnr.
*              WG_LFA1-NAME1 TO WG_SAIDA-NAME1,
*              WA_BKPF-XBLNR TO WG_SAIDA-XBLNR.
      ELSE.

        CASE wa_bkpf-awtyp.
          WHEN 'BKPF'.
            IF sy-subrc IS INITIAL.
              READ TABLE tg_lfa1 INTO wg_lfa1
                WITH KEY lifnr = wg_rbkp-lifnr
                         BINARY SEARCH.
            ENDIF.
*            MOVE: WG_LFA1-NAME1 TO WG_SAIDA-NAME1,
*                WA_BKPF-XBLNR     TO WG_SAIDA-XBLNR.

          WHEN OTHERS.
            READ TABLE tg_rbkp INTO wg_rbkp
            WITH KEY gjahr = wa_bkpf-gjahr
              belnr = wa_bkpf-arcid.

            IF sy-subrc IS INITIAL.
              READ TABLE tg_lfa1 INTO wg_lfa1
                WITH KEY lifnr = wg_rbkp-lifnr
                         BINARY SEARCH.
            ENDIF.
*            MOVE: WG_LFA1-NAME1 TO WG_SAIDA-NAME1,
*                  WA_BKPF-XBLNR TO WG_SAIDA-XBLNR,
*                  WG_RBKP-LIFNR TO WG_SAIDA-LIFNR.
        ENDCASE.
      ENDIF.

      MOVE-CORRESPONDING: wg_anlz      TO wg_saida,
                          wg_anlb      TO wg_saida,
                          wg_rbkp      TO wg_saida,
                          wg_lfa1      TO wg_saida,
                          wg_skat      TO wg_saida,
                          wa_bkpf      TO wg_saida,
*                          WG_SKAT_AUX  TO WG_SAIDA,
*                          WA_MSEG      TO WG_SAIDA,
                          wg_t022t     TO wg_saida,
                          wg_makt      TO wg_saida,
                          wg_ttypt     TO wg_saida,
                          wg_faglflexa TO wg_saida,
                          wg_bseg      TO wg_saida.

      MOVE: wg_t856t-txt    TO wg_saida-txt_bewar,
            wg_anla-txt50_1 TO wg_saida-txt50_1,
            wg_anla-txa50   TO wg_saida-txa50,
            wg_anlh-anlhtxt TO wg_saida-anlhtxt.

      APPEND wg_saida TO tg_saida.

    ENDIF.
    CLEAR: wg_saida, wg_bseg, wg_rbkp, wg_anlz, wg_anla, wg_t022t, wg_skat, wg_faglflexa, wg_makt,
    wg_ttypt, wg_lfa1, wg_t856t, wa_bkpf, wg_skat_aux.
  ENDLOOP.

ENDFORM.

FORM imprimir_dados.

  PERFORM montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = estrutura[]
      i_save             = 'A'
      it_events          = events
      is_print           = t_print
    TABLES
      t_outtab           = tg_saida.

ENDFORM.

FORM montar_layout.
  PERFORM montar_estrutura USING:
       01 'BSEG'        'BUKRS'   'TG_SAIDA' 'BUKRS'      ' '                      ' ',
       02 'BSEG'        'GSBER'   'TG_SAIDA' 'GSBER'      ' '                      ' ',
       03 'ANLZ'        'KOSTL'   'TG_SAIDA' 'KOSTL'      ' '                      ' ',
       04 'CSKT'        'LTEXT'   'TG_SAIDA' 'LTEXT'      ' '                      ' ',
       05 'BSEG'        'EBELN'   'TG_SAIDA' 'EBELN'      ' '                      ' ',
       06 'BSEG'        'EBELP'   'TG_SAIDA' 'EBELP'      ' '                      ' ',
       07 'RBKP'        'XBLNR'   'TG_SAIDA' 'XBLNR'      ' '                      ' ',
       08 'RBKP'        'LIFNR'   'TG_SAIDA' 'LIFNR'      ' '                      ' ',
       09 'LFA1'        'NAME1'   'TG_SAIDA' 'NAME1'      'Desc. Fornecedor'       ' ',
       10 'FAGLFLEXA'   'RACCT'   'TG_SAIDA' 'RACCT'      ' '                      ' ',
       11 'SKAT'        'TXT50'   'TG_SAIDA' 'TXT50'      ' '                      ' ',
       12 'BSEG'        'ANLN1'   'TG_SAIDA' 'ANLN1'      ' '                      ' ',
       13 'BSEG'        'ANLN2'   'TG_SAIDA' 'ANLN2'      ' '                      ' ',
       14 'ANLA'        'TXT50'   'TG_SAIDA' 'TXT50_1'    ' '                      ' ',
       15 'ANLA'        'TXA50'   'TG_SAIDA' 'TXA50'      ' '                      ' ',
       16 'ANLH'        'ANLHTXT' 'TG_SAIDA' 'ANLHTXT'    ' '                      ' ',
       17 'BSEG'        'BELNR'   'TG_SAIDA' 'BELNR'      ' '                      ' ',
       18 'RBKP'        'BLDAT'   'TG_SAIDA' 'BLDAT'      ' '                      ' ',
       19 'ANLB'        'AFABG'   'TG_SAIDA' 'AFABG'      ' '                      ' ',
       20 'FAGLFLEXA'   'BUDAT'   'TG_SAIDA' 'BUDAT'      ' '                      ' ',
       21 'FAGLFLEXA'   'TSL'     'TG_SAIDA' 'TSL'        'Montante MI'            ' ',
       22 'FAGLFLEXA'   'KSL'     'TG_SAIDA' 'KSL'        'Montante MI2'           ' ',
       23 'FAGLFLEXA'   'OSL'     'TG_SAIDA' 'OSL'        'Montante MI3'           ' ',
       24 'BSEG'        'SGTXT'   'TG_SAIDA' 'SGTXT'      ' '                      ' ',
       25 'BSEG'        'MATNR'   'TG_SAIDA' 'MATNR'      ' '                      ' ',
       26 'MAKT'        'MAKTX'   'TG_SAIDA' 'MAKTX'      ' '                      ' ',
       27 'BSEG'        'MENGE'   'TG_SAIDA' 'MENGE'      ' '                      ' ',
       28 'BSEG'        'MEINS'   'TG_SAIDA' 'MEINS'      ' '                      ' ',
       29 'BSEG'        'WERKS'   'TG_SAIDA' 'WERKS'      ' '                      ' ',
       30 'T022T'       'TXT'     'TG_SAIDA' 'TXT'        'Operação'               ' ',
       31 'TTYPT'       'OTEXT'   'TG_SAIDA' 'OTEXT'      'Operação Referência'    ' ',
       32 'BSEG'        'BEWAR'   'TG_SAIDA' 'BEWAR'      'Tipo do Movimento'      ' ',
       33 'T856T'       'TXT'     'TG_SAIDA' 'TXT_BEWAR'  'Dec. do tipo Movimento' ' '.

ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  APPEND VALUE #(
                  fieldname     = p_field
                  tabname       = p_tabname
                  ref_tabname   = p_ref_tabname
                  ref_fieldname = p_ref_fieldname
                  key           = abap_false
                  key_sel       = abap_true
                  col_pos       = p_col_pos
                  no_out        = abap_false
                  seltext_s     = p_scrtext_l
                  seltext_m     = p_scrtext_l
                  seltext_l     = p_scrtext_l
                  ddictxt       = COND #( WHEN p_scrtext_l IS NOT INITIAL THEN 'L' ELSE abap_false )
                 ) TO estrutura.

ENDFORM.                    " montar_estrutura
