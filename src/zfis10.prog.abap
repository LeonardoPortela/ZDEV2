*&---------------------------------------------------------------------*
*& Report  ZFIS10
*& Consultoria: Rollout
*&---------------------------------------------------------------------*
REPORT  zfis10.

TYPE-POOLS: slis.
*&---------------------------------------------------------------------*
*& Declaração de Tipos
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_skb1,
          bukrs TYPE skb1-bukrs,
          saknr TYPE skb1-saknr,
          mitkz TYPE skb1-mitkz,
       END OF ty_skb1,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_bsik,
          bukrs TYPE bsik-bukrs,
          lifnr TYPE bsik-lifnr,
          budat TYPE bsik-budat,
          waers TYPE bsik-waers,
          bldat TYPE bsik-bldat,
          xblnr TYPE bsik-xblnr,
          blart TYPE bsik-blart,
          shkzg TYPE bsik-shkzg,
          dmbtr TYPE bsik-dmbtr,
          sgtxt TYPE bsik-sgtxt,
          hkont TYPE bsik-hkont,
          dmbe2 TYPE bsik-dmbe2,
          saknr TYPE bsik-saknr,
          belnr TYPE bsik-belnr,
          gjahr TYPE bsik-gjahr,
       END OF ty_bsik,

       BEGIN OF ty_bsak,
          bukrs TYPE bsak-bukrs,
          lifnr TYPE bsak-lifnr,
          budat TYPE bsak-budat,
          waers TYPE bsak-waers,
          bldat TYPE bsak-bldat,
          xblnr TYPE bsak-xblnr,
          gjahr TYPE bsak-gjahr,
          shkzg TYPE bsak-shkzg,
          dmbtr TYPE bsak-dmbtr,
          sgtxt TYPE bsak-sgtxt,
          augbl TYPE bsak-augbl,
          hkont TYPE bsak-hkont,
          dmbe2 TYPE bsak-dmbe2,
          belnr TYPE bsak-belnr,
          augdt TYPE bsak-augdt,
          saknr TYPE bsak-saknr,
       END OF ty_bsak,

      BEGIN OF ty_bsid,
          bukrs TYPE bsid-bukrs,
          kunnr TYPE bsid-kunnr,
          budat TYPE bsid-budat,
          waers TYPE bsid-waers,
          bldat TYPE bsid-bldat,
          xblnr TYPE bsid-xblnr,
          blart TYPE bsid-blart,
          shkzg TYPE bsid-shkzg,
          dmbtr TYPE bsid-dmbtr,
          sgtxt TYPE bsid-sgtxt,
          hkont TYPE bsid-hkont,
          dmbe2 TYPE bsid-dmbe2,
          belnr TYPE bsid-belnr,
          saknr TYPE bsid-saknr,
          gjahr TYPE bsid-gjahr,
      END OF ty_bsid,

      BEGIN OF ty_bsad,
          bukrs TYPE bsad-bukrs,
          kunnr TYPE bsad-kunnr,
          belnr TYPE bsad-belnr,
          budat TYPE bsad-budat,
          waers TYPE bsad-waers,
          bldat TYPE bsad-bldat,
          xblnr TYPE bsad-xblnr,
          blart TYPE bsad-blart,
          shkzg TYPE bsad-shkzg,
          dmbtr TYPE bsad-dmbtr,
          sgtxt TYPE bsad-sgtxt,
          hkont TYPE bsad-hkont,
          augbl TYPE bsad-augbl,
          augdt TYPE bsad-augdt,
          dmbe2 TYPE bsad-dmbe2,
          saknr TYPE bsid-saknr,
          gjahr TYPE bsad-gjahr,
          bschl TYPE bsad-bschl,
      END OF ty_bsad,

      BEGIN OF ty_kna1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
      END OF ty_kna1,

      BEGIN OF ty_bkpf,
        belnr TYPE bkpf-belnr,
        kursf TYPE bkpf-kursf,
      END OF ty_bkpf,

      BEGIN OF ty_saida,
          clifor     TYPE c LENGTH 10 ,
          bukrs      TYPE skb1-bukrs,
          lifnr      TYPE bsak-lifnr,
          name1      TYPE lfa1-name1,
          saknr      TYPE skb1-saknr,
          mitkz      TYPE skb1-mitkz,
          budat      TYPE bsik-budat,
          budat_item TYPE bsik-budat,
          waers      TYPE bsik-waers,
          waers_item TYPE bsik-waers,
          bldat      TYPE bsik-bldat,
          xblnr      TYPE bsik-xblnr,
          blart      TYPE bsik-blart,
          shkzg      TYPE bsik-shkzg,
          dmbtr      TYPE bsik-dmbtr,
          dmbtr_item TYPE bsik-dmbtr,
          sgtxt      TYPE bsik-sgtxt,
          hkont      TYPE bsik-hkont,
          dmbe2      TYPE bsik-dmbe2,
          dmbe2_item TYPE bsik-dmbe2,
          txcam      TYPE p DECIMALS 4, "TYPE bsik-dmbe2,
          augbl      TYPE bsak-augbl,
          augdt      TYPE bsak-augdt,
          belnr      TYPE bsak-belnr,
          txcam_item TYPE p DECIMALS 4, "TYPE bsik-dmbe2,
          saldo_r    TYPE bsik-dmbe2,
          saldo_u    TYPE bsik-dmbe2,
          txcam_s    TYPE p DECIMALS 4, "TYPE bsik-dmbe2,
          varcam     TYPE bsik-dmbe2,
      END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*&---------------------------------------------------------------------*
*& Declaração tabelas
*&---------------------------------------------------------------------*

DATA: t_skb1     TYPE TABLE OF ty_skb1,
      t_lfa1     TYPE TABLE OF ty_lfa1,
      t_bsik     TYPE TABLE OF ty_bsik,
      t_bsak     TYPE TABLE OF ty_bsak,
      t_bsak_a   TYPE TABLE OF ty_bsak,
      t_bsad_a   TYPE TABLE OF ty_bsad,
      t_bsad_b   TYPE TABLE OF ty_bsad,
      t_bsak_b   TYPE TABLE OF ty_bsak,
      t_bsid     TYPE TABLE OF ty_bsid,
      t_bsad     TYPE TABLE OF ty_bsad,
      t_saida2   TYPE TABLE OF ty_saida,
      t_bsad_aux TYPE TABLE OF ty_bsad,
      t_kna1     TYPE TABLE OF ty_kna1,
      t_bkpf     TYPE TABLE OF ty_bkpf,
      t_saida    TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& Declaração DE WORK AREAS
*&---------------------------------------------------------------------*
DATA: wa_skb1   TYPE ty_skb1,
      wa_lfa1   TYPE ty_lfa1,
      wa_bsik   TYPE ty_bsik,
      wa_bsak   TYPE ty_bsak,
      wa_bsid   TYPE ty_bsid,
      wa_bsad   TYPE ty_bsad,
      wa_saida2 TYPE ty_saida,
      wa_kna1   TYPE ty_kna1,
      wa_bkpf   TYPE ty_bkpf,
      wa_saida  TYPE ty_saida.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid.

*&---------------------------------------------------------------------*
*& Declaração DE Variaveis Globais
*&---------------------------------------------------------------------*
DATA: v_name(30),
      v_txcam    TYPE p DECIMALS 4, "TYPE ty_bsak-dmbtr,
      v_txcam_2  TYPE p DECIMALS 4, "TYPE ty_bsak-dmbtr,
      v_txcam_s  TYPE p DECIMALS 4, "TYPE ty_bsak-dmbtr,
      v_taxa     TYPE p DECIMALS 4, "TYPE ty_bsak-dmbtr,
      vl_saldo_r TYPE ty_bsak-dmbtr,
      vl_saldo_u TYPE ty_bsak-dmbtr,
      v_varcam     TYPE ty_bsak-dmbtr.
*&---------------------------------------------------------------------*
*& Declaração DE Contantes.
*&---------------------------------------------------------------------*
CONSTANTS: c_x(1) TYPE c VALUE 'X'.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR wa_skb1-bukrs OBLIGATORY,
                s_lifnr FOR wa_bsik-lifnr MODIF ID lif,
                s_kunnr FOR wa_bsid-kunnr MODIF ID cli,
                s_hkont FOR wa_bsak-hkont,
                s_augdt FOR wa_bsak-augdt OBLIGATORY,
                s_waers FOR wa_bsak-waers OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_forn RADIOBUTTON GROUP gr1 USER-COMMAND asnum DEFAULT 'X',
            p_clie RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CASE 'X'.
    WHEN p_forn.
      v_name = 'CLI'.
      CLEAR:   s_kunnr.
      REFRESH: s_kunnr.
      PERFORM f_modifica_tela.
    WHEN p_clie.
      v_name = 'LIF'.
      CLEAR:   s_lifnr.
      REFRESH: s_lifnr.
      PERFORM f_modifica_tela.
    WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_forn EQ c_x
  AND s_lifnr IS INITIAL
  AND s_hkont IS INITIAL.
    MESSAGE i000(z01) WITH 'Campo Fornecedor OU Conta Contábil é Obrigatorio.'.
    STOP.
  ELSEIF p_clie EQ c_x
     AND s_kunnr IS INITIAL
     AND s_hkont IS INITIAL.
    MESSAGE i000(z01) WITH 'Campo Cliente OU Conta Contábil é Obrigatorio.'.
    STOP.
  ENDIF.

  PERFORM iniciar_variaves.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.
  PERFORM imprimir_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .
  SELECT bukrs saknr mitkz "#EC CI_DB_OPERATION_OK[2431747]
    FROM skb1
    INTO TABLE t_skb1
     WHERE bukrs IN s_bukrs
       AND saknr IN s_hkont
       AND ( mitkz EQ 'K'
          OR mitkz EQ 'D' ).

  IF sy-subrc IS INITIAL.
    IF p_forn EQ c_x.
      SELECT bukrs lifnr budat waers bldat
             xblnr blart shkzg dmbtr sgtxt
             hkont dmbe2 saknr belnr gjahr
        FROM bsik
        INTO TABLE t_bsik
         FOR ALL ENTRIES IN t_skb1
         WHERE bukrs IN s_bukrs
           AND lifnr IN s_lifnr
           AND augdt IN s_augdt
           AND waers IN s_waers
           AND hkont EQ t_skb1-saknr.

      SELECT bukrs lifnr budat waers bldat xblnr gjahr
             shkzg dmbtr sgtxt augbl hkont dmbe2 belnr
             augdt saknr
        FROM bsak
        INTO TABLE t_bsak
        FOR ALL ENTRIES IN t_skb1
         WHERE bukrs IN s_bukrs
           AND lifnr IN s_lifnr
           AND augdt IN s_augdt
           AND waers IN s_waers
           AND hkont EQ t_skb1-saknr.

      IF sy-subrc = 0.
        t_bsak_b[] = t_bsak[].

        DO.
          SELECT bukrs lifnr budat waers bldat xblnr gjahr
                 shkzg dmbtr sgtxt augbl hkont dmbe2 belnr
                 augdt saknr
           FROM bsak INTO TABLE t_bsak_a
           FOR ALL ENTRIES IN t_bsak_b
           WHERE bukrs EQ t_bsak_b-bukrs AND
                 gjahr EQ t_bsak_b-gjahr AND
                 belnr EQ t_bsak_b-augbl AND
                ( augbl NE t_bsak_b-belnr ). "AND belnr NE t_bsak_b-augbl ).


          IF sy-subrc <> 0.
            EXIT.
          ELSE.

            LOOP AT t_bsak_a INTO wa_bsak.
              IF wa_bsak-augbl = wa_bsak-belnr.
                DELETE t_bsak_a INDEX sy-tabix.
              ENDIF.
            ENDLOOP.

            IF t_bsak_a[] IS INITIAL.
              EXIT.
            ENDIF.
            APPEND LINES OF t_bsak_a TO t_bsak.
            t_bsak_b[] = t_bsak_a[].
            REFRESH t_bsak_a.
          ENDIF.

        ENDDO.
      ENDIF.
      IF NOT t_bsak[] IS INITIAL.
        SELECT lifnr name1
          FROM lfa1
          INTO TABLE t_lfa1
           FOR ALL ENTRIES IN t_bsak
          WHERE lifnr EQ t_bsak-lifnr.

        SELECT belnr kursf
          FROM bkpf
          INTO TABLE t_bkpf
          FOR ALL ENTRIES IN t_bsak
          WHERE bukrs IN s_bukrs
            AND belnr EQ t_bsak-augbl
            AND gjahr EQ t_bsak-gjahr.

        SELECT bukrs lifnr budat waers bldat
               xblnr blart shkzg dmbtr sgtxt
               hkont dmbe2 saknr belnr gjahr
         FROM bsik APPENDING TABLE t_bsik
          FOR ALL ENTRIES IN t_bsak
          WHERE bukrs EQ t_bsak-bukrs
            AND gjahr EQ t_bsak-gjahr
            AND belnr EQ t_bsak-augbl.

      ENDIF.

    ELSE.
      SELECT bukrs kunnr budat waers bldat xblnr
             blart shkzg dmbtr sgtxt hkont dmbe2
             belnr saknr gjahr
        FROM bsid
        INTO TABLE t_bsid
         FOR ALL ENTRIES IN t_skb1
         WHERE bukrs IN s_bukrs
           AND kunnr IN s_kunnr
           AND augdt IN s_augdt
           AND waers IN s_waers
           AND hkont EQ t_skb1-saknr.

      SELECT bukrs kunnr belnr budat waers bldat xblnr blart
             shkzg dmbtr sgtxt hkont augbl augdt dmbe2
             saknr gjahr bschl
        FROM bsad
        INTO TABLE t_bsad
         FOR ALL ENTRIES IN t_skb1
         WHERE bukrs IN s_bukrs
           AND kunnr IN s_kunnr
           AND augdt IN s_augdt
           AND waers IN s_waers
           AND hkont EQ t_skb1-saknr.


*        AND belnr IN
*('0101811685',
*'0101802144',
*'0101802145',
*'0101802146',
*'0101802147',
*'0101802148')
*voltar

*('0101811686',
*'0101802143',
*'0101802149',
*'0101802150',
*'0101802151')
*"'0101811692',
      "'0101811692',
      "'0101811692',
      "'0101811693')

      IF sy-subrc = 0.
        t_bsad_b[] = t_bsad[].

        DO.
          SELECT bukrs kunnr belnr budat waers bldat xblnr blart
                 shkzg dmbtr sgtxt hkont augbl augdt dmbe2
                 saknr gjahr bschl
                       FROM bsad INTO TABLE t_bsad_a
               FOR ALL ENTRIES IN t_bsad_b
               WHERE bukrs EQ t_bsad_b-bukrs AND
                     gjahr EQ t_bsad_b-gjahr AND
                     belnr EQ t_bsad_b-augbl AND
                    ( augbl NE t_bsad_b-belnr ) AND
                     hkont IN s_hkont. "AND belnr NE t_bsak_b-augbl ).

          IF sy-subrc <> 0.
            EXIT.
          ELSE.

            IF t_bsad_a[] IS INITIAL.
              EXIT.
            ENDIF.
            APPEND LINES OF t_bsad_a TO t_bsad.
            t_bsad_b[] = t_bsad_a[].
            REFRESH t_bsad_a.
          ENDIF.

        ENDDO.
      ENDIF.
      IF NOT t_bsad[] IS INITIAL.
        SELECT kunnr name1
          FROM kna1
          INTO TABLE t_kna1
           FOR ALL ENTRIES IN t_bsad
           WHERE kunnr EQ t_bsad-kunnr.

        SELECT belnr kursf
          FROM bkpf
          INTO TABLE t_bkpf
          FOR ALL ENTRIES IN t_bsad
          WHERE bukrs IN s_bukrs
            AND belnr EQ t_bsad-augbl
            AND gjahr EQ t_bsad-gjahr.

        SELECT bukrs kunnr budat waers bldat xblnr
               blart shkzg dmbtr sgtxt hkont dmbe2
               belnr saknr gjahr
          FROM bsid APPENDING TABLE t_bsid
           FOR ALL ENTRIES IN t_bsad
           WHERE bukrs EQ t_bsad-bukrs
             AND gjahr EQ t_bsad-gjahr
             AND belnr EQ t_bsad-augbl.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_TELA
*&---------------------------------------------------------------------*

FORM f_modifica_tela .

  LOOP AT SCREEN.

    IF screen-group1 = v_name.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " F_MODIFICA_TELA
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM organiza_dados .

  SORT: t_skb1 BY bukrs saknr.

  IF p_forn EQ c_x.

    SORT: t_bsak BY bukrs belnr gjahr, "LIFNR
          t_lfa1 BY lifnr.


    LOOP AT t_bsak INTO wa_bsak.

      IF wa_bsak-belnr = wa_bsak-augbl.
        DELETE t_bsak.
      ENDIF.
    ENDLOOP.

    LOOP AT t_bsak INTO wa_bsak.

      READ TABLE t_skb1 INTO wa_skb1
        WITH KEY bukrs = wa_bsak-bukrs
                 saknr = wa_bsak-hkont
                 BINARY SEARCH.

      IF sy-subrc IS INITIAL.

** BUSCA NOME DO FORNECEDOR
        READ TABLE t_lfa1 INTO wa_lfa1
          WITH KEY lifnr = wa_bsak-lifnr
                   BINARY SEARCH.

**CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
        IF wa_bsak-dmbe2 = '0.01'.
          CLEAR wa_bsak-dmbe2.
        ENDIF.

        IF NOT wa_bsak-dmbe2 IS INITIAL.
          v_txcam = wa_bsak-dmbtr / wa_bsak-dmbe2. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
*          v_txcam = wa_bsak-dmbtr.
          v_txcam = 1.
        ENDIF.

** A NIVEL DE CABECALHO
        MOVE: wa_bsak-bukrs TO wa_saida-bukrs,
              wa_bsak-lifnr TO wa_saida-lifnr,
              wa_lfa1-name1 TO wa_saida-name1,
              wa_bsak-hkont TO wa_saida-hkont,
              wa_bsak-belnr TO wa_saida-belnr,
              wa_bsak-xblnr TO wa_saida-xblnr,
              wa_bsak-budat TO wa_saida-budat,
              wa_bsak-bldat TO wa_saida-bldat,
              wa_bsak-waers TO wa_saida-waers,
              wa_bsak-dmbtr TO wa_saida-dmbtr,
              wa_bsak-dmbe2 TO wa_saida-dmbe2,
              v_txcam       TO wa_saida-txcam,
              wa_bsak-augbl TO wa_saida-augbl,
              wa_bsak-augdt TO wa_saida-augdt.

        CLEAR: wa_bsak, wa_bsik.

* BUSCA ITEM
        READ TABLE t_bsak INTO wa_bsak
          WITH KEY belnr = wa_saida-augbl.

        IF sy-subrc <> 0.
          READ TABLE t_bsik INTO wa_bsik
                  WITH KEY belnr = wa_saida-augbl.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING wa_bsik TO wa_bsak.
          ENDIF.
        ENDIF.

        READ TABLE t_bkpf INTO wa_bkpf
          WITH KEY belnr = wa_saida-augbl.

        IF sy-subrc EQ 0.
          v_txcam_2 = wa_bkpf-kursf.
        ENDIF.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
        IF NOT wa_bsak-dmbe2 IS INITIAL.
          v_txcam = wa_bsak-dmbtr / wa_bsak-dmbe2. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
*          v_txcam = wa_bsak-dmbtr.
          v_txcam = 1.
        ENDIF.

** VALOR DO SALDO (CALCULO REAL)
        vl_saldo_r = wa_saida-dmbtr - wa_bsak-dmbtr - v_txcam_2.

** VALOR DO SALDO (CALCULO DOLAR)
        vl_saldo_u = wa_saida-dmbe2 - wa_bsak-dmbe2.

** TAXA DE CAMBIO DO SALDO (CALCULO DOLAR)
        IF NOT vl_saldo_u IS INITIAL.
          v_txcam_s = vl_saldo_r / vl_saldo_u. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
          v_txcam_s = vl_saldo_r.
        ENDIF.

** VARIACAO CAMBIAL (CALCULO)
*        V_VARCAM = ( ( WA_BSAK-DMBE2 * V_TXCAM ) - ( WA_SAIDA-TXCAM * WA_BSAK-DMBE2 ) ).
        v_varcam = ( ( wa_bsak-dmbe2 * v_txcam_2 ) - ( v_txcam_2 * wa_bsak-dmbe2 ) ).
*        if not wa_bsik-belnr is initial.
*          clear v_varcam.
*        endif.

** NIVEL DE ITEM
        MOVE: wa_bsak-budat TO wa_saida-budat_item,
              wa_bsak-waers TO wa_saida-waers_item,
*              wa_bsak-dmbtr TO wa_saida-dmbtr_item,    "IGOR VILELA - COMENTADO 19/08/11.
*              wa_bsak-dmbe2 TO wa_saida-dmbe2_item,
*              v_txcam       TO wa_saida-txcam_item,
**             IGOR vILELA -  INICIO
              v_txcam_2       TO wa_saida-txcam_item,

***            IGOR VILELA - FIM
              vl_saldo_r    TO wa_saida-saldo_r,
              vl_saldo_u    TO wa_saida-saldo_u,
              v_txcam_s     TO wa_saida-txcam_s,
              v_varcam      TO wa_saida-varcam.


        IF wa_bsak-dmbe2 LE wa_saida-dmbe2.
          MOVE wa_bsak-dmbe2 TO wa_saida-dmbe2_item.
        ELSEIF wa_bsak-dmbe2 GE wa_saida-dmbe2.
          MOVE wa_saida-dmbe2 TO wa_saida-dmbe2_item.
        ENDIF.

        IF wa_saida-dmbe2_item GT wa_saida-dmbe2.
          v_taxa = wa_saida-dmbtr_item / wa_saida-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
          wa_saida-dmbe2_item = wa_saida-dmbe2 * v_taxa.
        ENDIF.

*** iGOR vILELA - INCLUSAO - INICIO.
        " ADD CSB
        IF wa_saida-dmbe2_item IS INITIAL.
          wa_saida-dmbe2_item = wa_saida-dmbe2.
        ENDIF.
        " FIM CSB

        " Nova regra de validação. CSB
        IF ( ( wa_saida-augdt IS NOT INITIAL ) AND ( wa_saida-augdt IN s_augdt ) ) .
          wa_saida-dmbe2_item =  wa_saida-dmbe2.
        ELSE.
          IF ( ( wa_saida-augdt IS NOT INITIAL ) AND ( wa_saida-augdt > s_augdt-high ) ) .
            wa_saida-dmbe2_item =  0.
          ENDIF.
        ENDIF.
        " Fim Nova regra de validação. CSB

        wa_saida-dmbtr_item = v_txcam_2 * wa_saida-dmbe2_item.
*** iGOR vILELA - INCLUSAO - FIM.

        IF wa_bsak-shkzg EQ 'H'.
          wa_saida-dmbtr_item = wa_saida-dmbtr_item * ( - 1 ).
          wa_saida-dmbe2_item = wa_saida-dmbe2_item * ( - 1 ).
          wa_saida-saldo_r = wa_saida-saldo_r * ( - 1 ).
          wa_saida-saldo_u = wa_saida-saldo_u * ( - 1 ).
          wa_saida-varcam  = wa_saida-varcam  * ( - 1 ).
          wa_saida-dmbe2   = wa_saida-dmbe2   * ( - 1 ).
          wa_saida-dmbtr   = wa_saida-dmbtr   * ( - 1 ).
        ENDIF.




        " CSB conta solicitada saldo Real
        wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr.
        wa_saida-saldo_r = wa_saida-dmbtr_item - wa_saida-dmbtr - wa_saida-varcam.
        wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item.
        " Fim CSB

         wa_saida-clifor   = 'Fornecedor'.

        APPEND wa_saida TO t_saida.
        CLEAR wa_saida.

      ENDIF.

    ENDLOOP.

    LOOP AT t_bsik INTO wa_bsik.
      READ TABLE t_skb1 INTO wa_skb1
        WITH KEY bukrs = wa_bsik-bukrs
                 saknr = wa_bsik-hkont
                 BINARY SEARCH.

      IF sy-subrc IS INITIAL.
*
*        READ TABLE t_bsak INTO wa_bsak
*          WITH KEY bukrs = wa_bsik-bukrs
**                   LIFNR = WA_BSIK-LIFNR
*                   belnr = wa_bsik-belnr
*                   gjahr = wa_bsik-gjahr
*                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.

** BUSCA NOME DO FORNECEDOR
          READ TABLE t_lfa1 INTO wa_lfa1
            WITH KEY lifnr = wa_bsik-lifnr
                     BINARY SEARCH.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
          IF NOT wa_bsik-dmbe2 IS INITIAL.
            v_txcam = wa_bsik-dmbtr / wa_bsik-dmbe2. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
            v_txcam = wa_bsik-dmbtr.
          ENDIF.

** A NIVEL DE CABECALHO
          MOVE: wa_bsik-bukrs TO wa_saida-bukrs,
                wa_bsik-lifnr TO wa_saida-lifnr,
                wa_lfa1-name1 TO wa_saida-name1,
                wa_bsik-hkont TO wa_saida-hkont,
                wa_bsik-belnr TO wa_saida-belnr,
                wa_bsik-xblnr TO wa_saida-xblnr,
                wa_bsik-budat TO wa_saida-budat,
                wa_bsik-bldat TO wa_saida-bldat,
                wa_bsik-waers TO wa_saida-waers,
                wa_bsik-dmbtr TO wa_saida-dmbtr,
                wa_bsik-dmbe2 TO wa_saida-dmbe2,
                v_txcam       TO wa_saida-txcam,
                wa_bsak-augbl TO wa_saida-augbl,
                wa_bsak-augdt TO wa_saida-augdt.

          CLEAR: wa_bsak.

** BUSCA ITEM
          READ TABLE t_bsak INTO wa_bsak
            WITH KEY belnr = wa_saida-augbl.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
          IF NOT wa_bsak-dmbe2 IS INITIAL.
            v_txcam = wa_bsak-dmbtr / wa_bsak-dmbe2. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
            v_txcam = wa_bsak-dmbtr.
          ENDIF.

** VALOR DO SALDO (CALCULO REAL)
          vl_saldo_r = wa_saida-dmbtr - wa_bsak-dmbtr.

** VALOR DO SALDO (CALCULO DOLAR)
          vl_saldo_u = wa_saida-dmbe2 - wa_bsak-dmbe2.

** TAXA DE CAMBIO DO SALDO (CALCULO DOLAR)
          IF NOT vl_saldo_u IS INITIAL.
            v_txcam_s = vl_saldo_r / vl_saldo_u. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
            v_txcam_s = vl_saldo_r.
          ENDIF.

          READ TABLE t_bkpf INTO wa_bkpf
           WITH KEY belnr = wa_saida-augbl.

          IF sy-subrc <> 0.
            v_txcam_2 = wa_bkpf-kursf.
          ENDIF.
** VARIACAO CAMBIAL (CALCULO)
*          v_varcam = ( ( vl_saldo_u * v_txcam_s ) - ( v_txcam_s * wa_saida-txcam ) ).
          CLEAR v_varcam.
** NIVEL DE ITEM
          MOVE: wa_bsak-budat TO wa_saida-budat_item,
                wa_bsak-waers TO wa_saida-waers_item,
                wa_bsak-dmbtr TO wa_saida-dmbtr_item,
*                wa_bsak-dmbe2 TO wa_saida-dmbe2_item,
*                 Igor  Vilela - Inicio 19/08/2011
                v_txcam       TO wa_saida-txcam_item,
*                  v_txcam_2       TO wa_saida-txcam_item,
*                 Igor  Vilela - fim  19/08/2011
                vl_saldo_r    TO wa_saida-saldo_r,
                vl_saldo_u    TO wa_saida-saldo_u,
                v_txcam_s     TO wa_saida-txcam_s,
                v_varcam      TO wa_saida-varcam.

          IF wa_bsak-dmbe2 LE wa_saida-dmbe2.
            MOVE wa_bsak-dmbe2 TO wa_saida-dmbe2_item.
          ELSEIF wa_bsak-dmbe2 GE wa_saida-dmbe2.
            MOVE wa_saida-dmbe2 TO wa_saida-dmbe2_item.
          ENDIF.

          IF wa_saida-dmbe2_item GT wa_saida-dmbe2.
            v_taxa = wa_saida-dmbtr_item / wa_saida-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
            wa_saida-dmbe2_item = wa_saida-dmbe2 * v_taxa.
          ENDIF.
**** iGOR vILELA - INCLUSAO - INICIO.
*          wa_saida-dmbtr_item = v_txcam_2 * wa_saida-dmbe2_item.
**** iGOR vILELA - INCLUSAO - FIM.

          " ADD CSB
          IF wa_saida-dmbe2_item IS INITIAL.
            wa_saida-dmbe2_item = wa_saida-dmbe2.
          ENDIF.
          " FIM CSB

          " CSB conta solicitada saldo real
          wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr.
          wa_saida-saldo_r = wa_saida-dmbtr_item - wa_saida-dmbtr - wa_saida-varcam.
          wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item.
          " Fim CSB

           wa_saida-clifor   = 'Fornecedor'.

          APPEND wa_saida TO t_saida.
          CLEAR wa_saida.

        ENDIF.
      ENDIF.
    ENDLOOP.


  ELSE.


    SORT: t_bsid BY bukrs belnr gjahr, "KUNNR
          t_kna1 BY kunnr.

    DATA: v_sinal TYPE bsad-shkzg.

    SORT t_bsad BY budat belnr.
    LOOP AT t_bsad INTO wa_bsad.
      v_sinal = wa_bsad-shkzg.
      t_saida2[] = t_saida[].
      DELETE t_saida2 WHERE augbl NE wa_bsad-augbl.

      READ TABLE t_saida2 INTO wa_saida2 INDEX 1.
      IF sy-subrc <> 0.
        CLEAR wa_saida2.
      ENDIF.
      CLEAR: vl_saldo_r, vl_saldo_u.

      READ TABLE t_skb1 INTO wa_skb1
        WITH KEY bukrs = wa_bsad-bukrs
                 saknr = wa_bsad-hkont
                 BINARY SEARCH.

      IF sy-subrc IS INITIAL.

** BUSCA NOME DO FORNECEDOR
        READ TABLE t_kna1 INTO wa_kna1
          WITH KEY kunnr = wa_bsad-kunnr
                   BINARY SEARCH.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO

        IF wa_bsad-dmbe2 = '0.01'.
          CLEAR wa_bsad-dmbe2.
        ENDIF.
        IF NOT wa_bsad-dmbe2 IS INITIAL.
          v_txcam = wa_bsad-dmbtr / wa_bsad-dmbe2. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
*          v_txcam = wa_bsad-dmbtr.
          v_txcam = 1.
        ENDIF.

** A NIVEL DE CABECALHO
        MOVE: wa_bsad-bukrs TO wa_saida-bukrs,
              wa_bsad-kunnr TO wa_saida-lifnr,
              wa_kna1-name1 TO wa_saida-name1,
              wa_bsad-hkont TO wa_saida-hkont,
              wa_bsad-belnr TO wa_saida-belnr,
              wa_bsad-xblnr TO wa_saida-xblnr,
              wa_bsad-budat TO wa_saida-budat,
              wa_bsad-bldat TO wa_saida-bldat,
              wa_bsad-waers TO wa_saida-waers,
              wa_bsad-dmbtr TO wa_saida-dmbtr,
              wa_bsad-dmbe2 TO wa_saida-dmbe2,
              v_txcam       TO wa_saida-txcam,
              wa_bsad-augbl TO wa_saida-augbl,
              wa_bsad-augdt TO wa_saida-augdt.

        CLEAR: wa_bsad, wa_bsik.

* BUSCA ITEM
        READ TABLE t_bsad INTO wa_bsad
          WITH KEY belnr = wa_saida-augbl.

        IF sy-subrc <> 0.
          READ TABLE t_bsik INTO wa_bsik
                  WITH KEY belnr = wa_saida-augbl.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING wa_bsik TO wa_bsad.
          ENDIF.

        ENDIF.
        READ TABLE t_bkpf INTO wa_bkpf
          WITH KEY belnr = wa_saida-augbl.
*** igor vilela - 19/08/2011 - inicio
        IF sy-subrc = 0.
          v_txcam_2 = wa_bkpf-kursf.
        ENDIF.
*** igor vilela - 19/08/2011 - fim
** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
        IF wa_bsad-dmbe2 = '0.01' OR wa_bsad-dmbe2 EQ 1 OR
           wa_bsad-dmbe2 = '0.02'.

          CLEAR wa_bsad-dmbe2.
        ENDIF.
        IF NOT wa_bsad-dmbe2 IS INITIAL.
          v_txcam = wa_bsad-dmbtr / wa_bsad-dmbe2. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
*          v_txcam = wa_bsad-dmbtr.
          v_txcam = 1.
        ENDIF.

** VALOR DO SALDO (CALCULO REAL)
        vl_saldo_r = wa_saida-dmbtr - wa_bsad-dmbtr - v_txcam_2.

** VALOR DO SALDO (CALCULO DOLAR)
        vl_saldo_u = wa_saida-dmbe2 - wa_bsad-dmbe2.

** TAXA DE CAMBIO DO SALDO (CALCULO DOLAR)
        IF NOT vl_saldo_u IS INITIAL.
          IF vl_saldo_r < 0.
            vl_saldo_u = vl_saldo_u * ( - 1 ) .
          ENDIF.
          v_txcam_s = vl_saldo_r / vl_saldo_u. "#EC CI_FLDEXT_OK[2610650]
        ELSE.
*          v_txcam_s = vl_saldo_r.
          v_txcam_s = 1.
        ENDIF.

** VARIACAO CAMBIAL (CALCULO)
*        V_VARCAM = ( ( VL_SALDO_U * V_TXCAM ) - ( WA_SAIDA-TXCAM * VL_SALDO_U ) ).
        v_varcam = ( ( vl_saldo_u * v_txcam_2 ) - ( v_txcam_2 * vl_saldo_u ) ).
*        if not wa_bsik-belnr is initial.
*          clear v_varcam.
*        endif.

** NIVEL DE ITEM
        MOVE: wa_bsad-budat TO wa_saida-budat_item,
              wa_bsad-waers TO wa_saida-waers_item,
*              wa_bsad-dmbtr TO wa_saida-dmbtr_item, Comentado igor 18/08/2011
              wa_bsad-dmbe2 TO wa_saida-dmbe2_item,
**              igor vilela - inicio 18/08/2011
              v_txcam_2 TO wa_saida-txcam_item,
*              v_txcam       TO wa_saida-txcam_item,
**              igor vilela - fim 18/08/2011
              vl_saldo_r    TO wa_saida-saldo_r,
              vl_saldo_u    TO wa_saida-saldo_u,
              v_txcam_s     TO wa_saida-txcam_s,
              v_varcam      TO wa_saida-varcam.

        IF wa_bsad-dmbe2 LE wa_saida-dmbe2.
          MOVE wa_bsad-dmbe2 TO wa_saida-dmbe2_item.
        ELSEIF wa_bsad-dmbe2 GE wa_saida-dmbe2.
          MOVE wa_saida-dmbe2 TO wa_saida-dmbe2_item.
        ENDIF.

        IF wa_saida-dmbe2_item GT wa_saida-dmbe2.
          v_taxa = wa_saida-dmbtr_item / wa_saida-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
          wa_saida-dmbe2_item = wa_saida-dmbe2 * v_taxa.
        ENDIF.

*** iGOR vILELA - INCLUSAO - INICIO.
        " ADD CSB
        IF wa_saida-dmbe2_item IS INITIAL.
          wa_saida-dmbe2_item = wa_saida-dmbe2.
        ENDIF.
        " FIM CSB

        " Nova regra de validação. CSB
        IF ( ( wa_saida-augdt IS NOT INITIAL ) AND ( wa_saida-augdt IN s_augdt ) ) .
          wa_saida-dmbe2_item =  wa_saida-dmbe2.
        ELSE.
          IF ( ( wa_saida-augdt IS NOT INITIAL ) AND ( wa_saida-augdt > s_augdt-high ) ) .
            wa_saida-dmbe2_item =  0.
          ENDIF.
        ENDIF.
        " Fim Nova regra de validação. CSB

        wa_saida-dmbtr_item = v_txcam_2 * wa_saida-dmbe2_item.

*** iGOR vILELA - INCLUSAO - FIM.

*        IF wa_bsad-shkzg EQ 'H'.
        IF v_sinal EQ 'H'.
          wa_saida-dmbtr_item = wa_saida-dmbtr_item * ( - 1 ).
          wa_saida-dmbe2_item = wa_saida-dmbe2_item * ( - 1 ).
          wa_saida-saldo_r = wa_saida-saldo_r * ( - 1 ).
          wa_saida-saldo_u = wa_saida-saldo_u * ( - 1 ).
          wa_saida-varcam  = wa_saida-varcam  * ( - 1 ).
          wa_saida-dmbe2   = wa_saida-dmbe2   * ( - 1 ).
          wa_saida-dmbtr   = wa_saida-dmbtr   * ( - 1 ).
        ENDIF.


        IF wa_saida-dmbe2_item GT wa_saida-dmbe2.

*          wa_saida-txcam_s = wa_saida-txcam_item = wa_saida-txcam.
          wa_saida-txcam_s = wa_saida-txcam.
          wa_saida-varcam = 0.
          wa_saida-saldo_r = wa_saida-saldo_u * wa_saida-txcam.
        ELSE.
*                    BREAK-POINT.
          IF wa_saida2-budat IS INITIAL.
*            wa_saida-txcam_s = wa_saida-txcam_item = wa_saida-txcam.
            wa_saida-txcam_s = wa_saida-txcam.
            wa_saida-varcam = 0.
            wa_saida-dmbtr_item = wa_saida-dmbe2_item * wa_saida-txcam_item.
*            wa_saida-saldo_r = wa_saida-saldo_u * wa_saida-txcam.
            wa_saida-saldo_r = wa_saida-dmbtr - wa_saida-dmbtr_item.
            wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item.
            wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB
*              wa_saida-varcam = wa_saida-dmbe2_item
          ELSE.
            IF wa_saida2-dmbe2_item IS  NOT INITIAL.
              v_taxa = wa_saida2-dmbtr_item / wa_saida2-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
            ENDIF.
            IF v_taxa > 0.
              wa_saida-dmbtr_item = wa_saida-dmbe2_item * v_taxa.
              wa_saida-txcam_s = 0.
              wa_saida-saldo_r = wa_saida-dmbtr - wa_saida-dmbtr_item.
              wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item.
              v_taxa = ( wa_saida-txcam - wa_saida2-txcam ) .
              "wa_saida-varcam = wa_saida-dmbe2_item * v_taxa. comentado CSB
              wa_saida-txcam_item =  wa_saida-dmbtr_item / wa_saida-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
              wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB
            ENDIF.
          ENDIF.
        ENDIF.
*        BREAK-POINT.
*** iGOR vILELA - INCLUSAO - INICIO.
        " ADD CSB
        IF wa_saida-dmbe2_item IS INITIAL.
          wa_saida-dmbe2_item = wa_saida-dmbe2.
        ENDIF.
        " FIM CSB
        wa_saida-dmbtr_item = v_txcam_2 * wa_saida-dmbe2_item.
*** iGOR vILELA - INCLUSAO - FIM.
        IF wa_bsad-bschl = '09'.
          SELECT bukrs kunnr belnr budat waers bldat xblnr blart
                 shkzg dmbtr sgtxt hkont augbl augdt dmbe2
                 saknr gjahr bschl
            FROM bsad
            INTO TABLE t_bsad_a
             WHERE bukrs EQ wa_bsad-bukrs AND
                   kunnr EQ wa_bsad-kunnr AND
                   gjahr EQ wa_bsad-gjahr AND
                   belnr EQ wa_bsad-belnr.

          DELETE t_bsad_a WHERE augbl EQ wa_bsad-belnr.

          IF NOT t_bsad_a[] IS INITIAL.
            READ TABLE t_bsad_a INTO wa_bsad INDEX 1.
            v_taxa = wa_bsad-dmbtr / wa_bsad-dmbe2. "#EC CI_FLDEXT_OK[2610650]
            "wa_saida-varcam = ( wa_saida-txcam_item - v_taxa ) * wa_saida-dmbe2_item. comentado CSB
            wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB


          ENDIF.
        ENDIF.

        " CSB conta solicitada saldo real
        wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB
        wa_saida-saldo_r = wa_saida-dmbtr_item - wa_saida-dmbtr - wa_saida-varcam.
        wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item. " CSB Calculo novo
        " Fim CSB

         wa_saida-clifor   = 'Cliente'.


        APPEND wa_saida TO t_saida.
        CLEAR wa_saida.

      ENDIF.

    ENDLOOP.

    LOOP AT t_bsid INTO wa_bsid.
      READ TABLE t_skb1 INTO wa_skb1
        WITH KEY bukrs = wa_bsid-bukrs
                 saknr = wa_bsid-hkont
                 BINARY SEARCH.

      IF sy-subrc IS INITIAL.
*
*        READ TABLE t_bsad INTO wa_bsad
*          WITH KEY bukrs = wa_bsid-bukrs
**                   LIFNR = WA_bsid-LIFNR
*                   belnr = wa_bsid-belnr
*                   gjahr = wa_bsid-gjahr
*                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.

** BUSCA NOME DO FORNECEDOR
          READ TABLE t_kna1 INTO wa_kna1
            WITH KEY kunnr = wa_bsid-kunnr
                     BINARY SEARCH.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
          IF NOT wa_bsid-dmbe2 IS INITIAL.
            v_txcam = wa_bsid-dmbtr / wa_bsid-dmbe2. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
*            v_txcam = wa_bsid-dmbtr.
            v_txcam = 1.
          ENDIF.

** A NIVEL DE CABECALHO
          MOVE: wa_bsid-bukrs TO wa_saida-bukrs,
                wa_bsid-kunnr TO wa_saida-lifnr,
                wa_kna1-name1 TO wa_saida-name1,
                wa_bsid-hkont TO wa_saida-hkont,
                wa_bsid-belnr TO wa_saida-belnr,
                wa_bsid-xblnr TO wa_saida-xblnr,
                wa_bsid-budat TO wa_saida-budat,
                wa_bsid-bldat TO wa_saida-bldat,
                wa_bsid-waers TO wa_saida-waers,
                wa_bsid-dmbtr TO wa_saida-dmbtr,
                wa_bsid-dmbe2 TO wa_saida-dmbe2,
                v_txcam       TO wa_saida-txcam,
                wa_bsad-augbl TO wa_saida-augbl,
                wa_bsad-augdt TO wa_saida-augdt.

          CLEAR: wa_bsad.

** BUSCA ITEM
          READ TABLE t_bsad INTO wa_bsad
            WITH KEY belnr = wa_saida-augbl.

** CALCULO DA TAXA CAMBIAL A NIVEL DE CABECALHO
          IF NOT wa_bsad-dmbe2 IS INITIAL.
            v_txcam = wa_bsad-dmbtr / wa_bsad-dmbe2. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
*            v_txcam = wa_bsad-dmbtr.
            v_txcam = 1.
          ENDIF.

** VALOR DO SALDO (CALCULO REAL)
          vl_saldo_r = wa_saida-dmbtr - wa_bsad-dmbtr.

** VALOR DO SALDO (CALCULO DOLAR)
          vl_saldo_u = wa_saida-dmbe2 - wa_bsad-dmbe2.

** TAXA DE CAMBIO DO SALDO (CALCULO DOLAR)
          IF NOT vl_saldo_u IS INITIAL.
            IF vl_saldo_r < 0.
              vl_saldo_u = vl_saldo_u * ( - 1 ) .
            ENDIF.
            v_txcam_s = vl_saldo_r / vl_saldo_u. "#EC CI_FLDEXT_OK[2610650]
          ELSE.
*            v_txcam_s = vl_saldo_r.
            v_txcam_s = 1.
          ENDIF.

          READ TABLE t_bkpf INTO wa_bkpf
            WITH KEY belnr = wa_saida-augbl.
*** igor vilela - 19/08/2011 - inicio
          IF sy-subrc <> 0.
            v_txcam_2 = wa_bkpf-kursf.
          ENDIF.
*** igor vilela - 19/08/2011 - fim

** VARIACAO CAMBIAL (CALCULO)
*          v_varcam = ( ( vl_saldo_u * v_txcam_s ) - ( v_txcam_s * wa_saida-txcam ) ).
          CLEAR v_varcam.
** NIVEL DE ITEM
          MOVE: wa_bsad-budat TO wa_saida-budat_item,
                wa_bsad-waers TO wa_saida-waers_item,
                wa_bsad-dmbtr TO wa_saida-dmbtr_item,
                wa_bsad-dmbe2 TO wa_saida-dmbe2_item,
                v_txcam       TO wa_saida-txcam_item,
                vl_saldo_r    TO wa_saida-saldo_r,
                vl_saldo_u    TO wa_saida-saldo_u,
                v_txcam_s     TO wa_saida-txcam_s,
                v_varcam      TO wa_saida-varcam.

*BREAK-POINT.
          IF wa_bsad-dmbe2 LE wa_saida-dmbe2.
            MOVE wa_bsad-dmbe2 TO wa_saida-dmbe2_item.
          ELSEIF wa_bsad-dmbe2 GE wa_saida-dmbe2.
            MOVE wa_saida-dmbe2 TO wa_saida-dmbe2_item.
          ENDIF.

          IF wa_saida-dmbe2_item GE wa_saida-dmbe2.
*            v_taxa = wa_saida-dmbtr_item / wa_saida-dmbe2_item.
*            wa_saida-dmbe2_item = wa_saida-dmbe2 * v_taxa.
            v_taxa = wa_saida2-dmbtr_item / wa_saida2-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
            IF v_taxa > 0.
              wa_saida-dmbtr_item = wa_saida-dmbe2_item * v_taxa.
              wa_saida-txcam_s = 0.
              wa_saida-saldo_r = wa_saida-dmbtr - wa_saida-dmbtr_item.
              wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item.
*              v_taxa = ( wa_saida-txcam - wa_saida2-txcam ) .
              wa_saida-txcam_item =  wa_saida-dmbtr_item / wa_saida-dmbe2_item. "#EC CI_FLDEXT_OK[2610650]
*              wa_saida-varcam = wa_saida-dmbe2_item * wa_saida2-txcam. "v_taxa.
              "wa_saida-varcam = ( wa_saida-txcam - v_taxa ) * wa_saida-dmbe2_item. comentado CSB
              wa_saida-varcam =   wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB

            ENDIF.
          ENDIF.

          IF wa_bsad-bschl = '09'.
            SELECT bukrs kunnr belnr budat waers bldat xblnr blart
                   shkzg dmbtr sgtxt hkont augbl augdt dmbe2
                   saknr gjahr bschl
              FROM bsad
              INTO TABLE t_bsad_a
               WHERE bukrs EQ wa_bsad-bukrs AND
                     kunnr EQ wa_bsad-kunnr AND
                     gjahr EQ wa_bsad-gjahr AND
                     belnr EQ wa_bsad-belnr.

            DELETE t_bsad_a WHERE augbl EQ wa_bsad-belnr.

            IF NOT t_bsad_a[] IS INITIAL.
              READ TABLE t_bsad_a INTO wa_bsad INDEX 1.
              v_taxa = wa_bsad-dmbtr / wa_bsad-dmbe2. "#EC CI_FLDEXT_OK[2610650]
              "wa_saida-varcam = ( wa_saida-txcam_item - v_taxa ) * wa_saida-dmbe2_item. comentado CSB
              wa_saida-varcam =  wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB
            ENDIF.
          ENDIF.

          " CSB conta solicitada Saldo Real
          wa_saida-varcam = wa_saida-dmbtr_item - wa_saida-dmbtr. " ADD CSB
          wa_saida-saldo_r = wa_saida-dmbtr_item - wa_saida-dmbtr - wa_saida-varcam.
          wa_saida-saldo_u = wa_saida-dmbe2 - wa_saida-dmbe2_item. " CSB Calculo novo
          " Fim CSB

          wa_saida-clifor   = 'Cliente'.

          APPEND wa_saida TO t_saida.
          CLEAR wa_saida.

        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.

  SORT t_saida.
  DELETE ADJACENT DUPLICATES FROM t_saida.

  LOOP AT t_saida INTO wa_saida.
    IF wa_saida-belnr EQ wa_saida-augbl.
      DELETE t_saida INDEX sy-tabix.
    ENDIF.

    IF wa_saida-augdt IN s_augdt.
      CONTINUE.
    ELSE.
      DELETE t_saida INDEX sy-tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .
*  PERFORM DEFINIR_EVENTOS.
  PERFORM montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = v_report
            i_callback_user_command = 'F_USER_COMMAND'
            it_fieldcat             = estrutura[]
            i_save                  = 'A'
*            IT_EVENTS               = EVENTS
            is_print                = t_print
       TABLES
            t_outtab                = t_saida.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.

  DATA: v_name TYPE dd03p-scrtext_l.

  IF p_forn EQ 'X'.
    v_name = 'Fornecedor'.
  ELSE.
    v_name = 'Cliente'.
  ENDIF.

  PERFORM montar_estrutura USING:
        1 ' '     ' '     'T_SAIDA' 'CLIFOR' 'Tipo'  ' ' ,
        2 'BSAK'  'BUKRS' 'T_SAIDA' 'BUKRS'  ' '  ' ' ,
        3 'BSAK'  'LIFNR' 'T_SAIDA' 'LIFNR'  v_name  ' ' ,
        4 'LFA1'  'NAME1' 'T_SAIDA' 'NAME1'  ' '  ' ' ,
        5 'BSAK'  'HKONT' 'T_SAIDA' 'HKONT'  'Conta'  ' ' ,
        6 'BSAK'  'BELNR' 'T_SAIDA' 'BELNR'  'NRO.Doc.'  ' ' ,
        7 'BSAK'  'XBLNR' 'T_SAIDA' 'XBLNR'  ' '  ' ' ,
        8 'BSAK'  'BUDAT' 'T_SAIDA' 'BUDAT'  'Dt. Lcto'  ' ' ,
        9 'BSAK'  'BLDAT' 'T_SAIDA' 'BLDAT'  'Dt. Docto'  ' ' ,
       10 'BSAK'  'WAERS' 'T_SAIDA' 'WAERS'  ''  ' ' ,
       11 'BSAK'  'DMBTR' 'T_SAIDA' 'DMBTR'  'Valor Original (R$)'  ' ' ,
       12 'BSAK'  'DMBE2' 'T_SAIDA' 'DMBE2'  'Valor Original (US$)'  ' ' ,
       13 ' '     ' '     'T_SAIDA' 'TXCAM'  'Tx.Cambio'  ' ' ,
       14 'BSAK'  'AUGBL' 'T_SAIDA' 'AUGBL'  'Doc.Comp.'  ' ' ,
       15 'BSAK'  'AUGDT' 'T_SAIDA' 'AUGDT'  'Dt.Comp.'  ' ' ,
       16 'BSAK'  'BUDAT' 'T_SAIDA' 'BUDAT_ITEM'  'Dt.lcto'  ' ' ,
       17 'BSAK'  'WAERS' 'T_SAIDA' 'WAERS_ITEM'  ''  ' ' ,
       18 'BSAK'  'DMBTR' 'T_SAIDA' 'DMBTR_ITEM'  'Valor Atualizado (R$)'  ' ' ,
       19 'BSAK'  'DMBE2' 'T_SAIDA' 'DMBE2_ITEM'  'Valor Original (US$)'  ' ' ,
       20 ' '     ' '     'T_SAIDA' 'TXCAM_ITEM'  'Tx.Cambio'  ' ' ,
       21 ' '     ' '     'T_SAIDA' 'SALDO_R'  'Saldo (R$)'  ' ' ,
       22 ' '     ' '     'T_SAIDA' 'SALDO_U'  'Saldo (US$) '  ' ' ,
       "23 ' '     ' '     'T_SAIDA' 'TXCAM_S'  'Tx.Cambio'  ' ' , Comentado CSB
       24 ' '     ' '     'T_SAIDA' 'VARCAM'   'Var. Cambial'  ' ' .



ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen).

  CLEAR wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.

  IF p_field EQ 'LIFNR'
  AND p_forn EQ c_x.
    p_scrtext_l = 'N. Fornecedor'.

  ELSEIF p_field EQ 'LIFNR'
     AND p_clie  EQ c_x.
    p_scrtext_l = 'N. Cliente'.

  ENDIF.


  IF p_field EQ 'BELNR' OR
     p_field EQ 'AUGBL'.
    wa_estrutura-hotspot = 'X'.
  ELSE.
    CLEAR wa_estrutura-hotspot.
  ENDIF.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-ddictxt       = 'L'.
  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
FORM iniciar_variaves.

  v_report = sy-repid.

*  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  IF l_selfield-fieldname = 'BELNR' OR
     l_selfield-fieldname = 'AUGBL'.
    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'BLN' FIELD l_selfield-value.
    SET PARAMETER ID 'GJR' FIELD wa_saida-bldat(4).
    SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDIF.

ENDFORM.                    "F_USER_COMMAND
